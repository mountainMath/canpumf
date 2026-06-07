# R/pipeline.R — Three-stage data pipeline for standard (non-LFS) surveys.
#
# Stage 1: pumf_locate_or_download() — find or fetch the zip, extract alongside it.
# Stage 2: pumf_parse_metadata()     — in metadata_parsers.R
# Stage 3: pumf_build_duckdb()       — reads data, applies labels, writes DuckDB.


# ---- internal helpers -------------------------------------------------------

# Compute the expected DuckDB file path for a survey version.
.pumf_db_path <- function(series, version, cache_path) {
  if (series == "LFS")
    return(file.path(cache_path, "LFS", "LFS.duckdb"))
  db_file <- paste0(series, "_", gsub("[^A-Za-z0-9._-]", "_", version), ".duckdb")
  file.path(cache_path, series, version, db_file)
}

# Compute the DuckDB table name for a given series / version / lang.
.pumf_table_name <- function(series, version, lang) {
  if (series == "LFS") return(paste0("lfs_", lang))
  lm <- pumf_registry_lookup(series, version)$layout_mask
  if (is.null(lm)) lang else paste0(lang, "_", lm)
}

# Probe whether a DuckDB file can be opened for writing.
#
# Two failure modes require detection:
#   (a) External lock (OS-level): dbConnect itself raises an error containing
#       "lock", "conflict", etc.
#   (b) In-process read-only sharing: DuckDB silently hands back a connection
#       that shares the read-only in-process instance already open (e.g. from
#       a get_pumf() tbl the user is holding).  dbConnect succeeds, but the
#       first write raises "attached in read-only mode".
#
# We probe for (b) with a rolled-back DDL statement.  DuckDB DDL is fully
# transactional, so BEGIN + CREATE TABLE + ROLLBACK leaves no trace.
# shutdown=FALSE on disconnect: if the connection shares an existing in-process
# instance (the user's open tbl), shutdown=TRUE would invalidate that instance.
.assert_duckdb_writable <- function(db_path) {
  if (!file.exists(db_path)) return(invisible(NULL))
  con <- tryCatch(
    DBI::dbConnect(duckdb::duckdb(), dbdir = db_path),
    error = function(e) e
  )
  if (inherits(con, "error")) {
    msg <- conditionMessage(con)
    if (grepl("lock|conflict|in use|block", msg, ignore.case = TRUE))
      stop("'", basename(db_path), "' is locked by an open connection.\n",
           "Close it first with close_pumf(tbl) and then retry.",
           call. = FALSE)
    stop(con)
  }

  write_err <- tryCatch({
    DBI::dbExecute(con, "BEGIN")
    DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS _canpumf_rw_probe (x INTEGER)")
    DBI::dbExecute(con, "ROLLBACK")
    NULL
  }, error = function(e) {
    tryCatch(DBI::dbExecute(con, "ROLLBACK"), error = function(e2) NULL)
    e
  })

  if (!is.null(write_err)) {
    # Probe failed: this connection shares the existing in-process instance
    # (the user's open tbl).  Use shutdown=FALSE to leave that instance intact.
    DBI::dbDisconnect(con, shutdown = FALSE)
    msg <- conditionMessage(write_err)
    if (grepl("read[_-]?only|attached in read", msg, ignore.case = TRUE))
      stop("'", basename(db_path), "' is held open by a read-only connection ",
           "(e.g. a tbl from get_pumf()).\n",
           "Close it first with close_pumf(tbl) and then retry.",
           call. = FALSE)
    stop(write_err)
  }

  # Probe succeeded: no other connection is sharing this instance, so a clean
  # shutdown leaves no lingering driver reference that would interfere with
  # subsequent connections.
  DBI::dbDisconnect(con, shutdown = TRUE)
  invisible(NULL)
}

# Returns the path to the single zip in dir, or NULL if none exists.
.find_version_zip <- function(dir) {
  if (!dir.exists(dir)) return(NULL)
  zips <- list.files(dir, pattern = "\\.zip$", ignore.case = TRUE, full.names = TRUE)
  if (length(zips) == 0L) NULL else zips[[1L]]
}

# TRUE when version_dir contains something other than the zip and metadata/.
# Presence of any such item means the zip was already extracted.
#
# StatCan occasionally ships zips whose top-level entry has the same name as
# the zip file (e.g. 2025-CSV.zip/ inside 2025-CSV.zip).  After extraction
# this creates a subdirectory named "2025-CSV.zip" — a directory, not a file,
# even though its name ends in .zip.  We use file.info() to distinguish real
# zip files from extracted directories with zip-like names.
.version_is_extracted <- function(dir) {
  if (!dir.exists(dir)) return(FALSE)
  items    <- list.files(dir)
  if (length(items) == 0L) return(FALSE)
  are_dirs <- file.info(file.path(dir, items))$isdir
  # Any subdirectory other than metadata/ is extracted content.
  if (any(are_dirs & items != "metadata", na.rm = TRUE)) return(TRUE)
  # Among regular files, exclude zip archives and duckdb files.
  other_files <- items[!are_dirs &
                        !grepl("\\.zip$|\\.duckdb$", items, ignore.case = TRUE) &
                        items != "metadata"]
  length(other_files) > 0L
}

# Strip any query string from a URL and return the bare filename.
.zip_filename_from_url <- function(url) {
  basename(strsplit(url, "?", fixed = TRUE)[[1L]][[1L]])
}


# ---- Stage 1 ----------------------------------------------------------------

#' Locate or download a PUMF version directory
#'
#' Ensures the version directory exists and its zip has been extracted.
#' With `refresh = TRUE`, clears the DuckDB file(s) and `metadata/` subdirectory
#' so that Stages 2 and 3 re-run, but does **not** re-download or re-extract
#' raw data.
#'
#' For EFT-only surveys (older Census years), stops with an informative error
#' asking the user to deposit the zip manually.
#'
#' @param series Survey series acronym, e.g. `"SFS"`.
#' @param version Version string, e.g. `"2019"` or `"2021 (individuals)"`.
#' @param cache_path Root cache directory. Defaults to
#'   `getOption("canpumf.cache_path", tempdir())`.
#' @param refresh If `TRUE`, delete the `.duckdb` file and `metadata/` dir so
#'   the downstream stages re-run. Raw zip and extracted files are untouched.
#'
#' @return The version directory path, invisibly.
#' @keywords internal
pumf_locate_or_download <- function(series,
                                    version,
                                    cache_path = getOption("canpumf.cache_path",
                                                           tempdir()),
                                    refresh    = FALSE,
                                    redownload = FALSE) {
  version_dir <- file.path(cache_path, series, version)

  # Step 2a: redownload — wipe the entire version directory (zip, extracted
  # content, DuckDB, metadata) so the download/extract cycle starts fresh.
  # DuckDB lock check must have already passed before this point.
  if (redownload && dir.exists(version_dir)) {
    message("Removing cached data for ", series, " ", version, " ...")
    unlink(version_dir, recursive = TRUE)
  }

  # Step 2b: refresh — wipe DuckDB(s) and metadata/, leave raw files alone.
  # Use "\\.duckdb" (no $ anchor) to also remove .duckdb.wal and any other
  # DuckDB sidecar files left from a previous run or interrupted write.
  # No-op when redownload already wiped the directory.
  if ((refresh || redownload) && dir.exists(version_dir)) {
    duckdb_paths <- list.files(version_dir, pattern = "\\.duckdb",
                               ignore.case = TRUE, full.names = TRUE)
    for (p in duckdb_paths) {
      if (!unlink(p)) next
      warning("Could not delete '", basename(p), "' during refresh. ",
              "Close any open connections with close_pumf() first.",
              call. = FALSE)
    }
    meta_dir <- file.path(version_dir, "metadata")
    if (dir.exists(meta_dir)) unlink(meta_dir, recursive = TRUE)
  }

  # Step 3: download zip when neither zip nor extracted content is present.
  # Skips download when content already exists (manual deposit, previous run
  # where zip was removed, or test fixtures with pre-built extracted dirs).
  zip_path     <- .find_version_zip(version_dir)
  is_extracted <- .version_is_extracted(version_dir)

  if (is.null(zip_path) && !is_extracted) {
    collection <- list_canpumf_collection()
    row <- dplyr::filter(collection,
                         .data$Acronym == series,
                         .data$Version == version)
    if (nrow(row) == 0L) {
      stop(series, " version '", version, "' was not found in the canpumf ",
           "collection. Check available versions with list_canpumf_collection().")
    }
    url <- row$url[[1L]]
    if (identical(url, "(EFT)")) {
      stop(series, " ", version, " is distributed via Statistics Canada's ",
           "Electronic File Transfer (EFT) and cannot be downloaded automatically.\n",
           "Download the zip file manually from the Statistics Canada EFT portal ",
           "and place it in:\n  ", version_dir)
    }
    dir.create(version_dir, recursive = TRUE, showWarnings = FALSE)
    zip_name <- .zip_filename_from_url(url)
    zip_path  <- file.path(version_dir, zip_name)
    message("Downloading ", series, " ", version, " ...")
    old_timeout <- getOption("timeout")
    options(timeout = max(600L, old_timeout))
    on.exit(options(timeout = old_timeout), add = TRUE)
    utils::download.file(url, zip_path, mode = "wb", quiet = FALSE)
    is_extracted <- FALSE  # need extraction after download
  }

  # Step 4: extract zip when not yet done.
  if (!is_extracted) {
    message("Extracting ", basename(zip_path), " ...")
    robust_unzip(zip_path, exdir = version_dir)
  }

  invisible(version_dir)
}


# ============================================================================
# Stage 3 helpers
# ============================================================================

# Recursive search for the main data file inside version_dir.
# Excludes metadata/, SPSS command file directories, and documentation files.
# Applies file_mask regex when multiple candidates remain.
.find_pumf_data_file <- function(version_dir, file_mask, prefer_fwf = FALSE) {
  # Derive the extension pattern from the file_mask when it contains an
  # explicit extension; otherwise use prefer_fwf (TRUE → TXT/DAT, FALSE → CSV).
  # This avoids the chicken-and-egg problem of needing is_fwf before finding
  # the file, while keeping backward-compatible behaviour for surveys that have
  # only one type of data file.
  ext_pat <- if (!is.null(file_mask) && grepl("\\.csv$", file_mask, ignore.case = TRUE))
    "\\.csv$"
  else if (!is.null(file_mask) && grepl("\\.(txt|dat)$", file_mask, ignore.case = TRUE))
    "\\.(txt|dat)$"
  else if (prefer_fwf)
    "\\.(txt|dat)$"
  else
    "\\.csv$"
  # Subdirectory names that hold metadata/layout but not data
  excl_pat <- paste0(
    "/(metadata|SPSS|Command|Syntax|Layout|SpssCard|",
    "Reading[_ ]cards|Documents|canpumf)/")

  all_files  <- list.files(version_dir, recursive = TRUE, full.names = TRUE)
  candidates <- all_files[grepl(ext_pat, all_files, ignore.case = TRUE)]
  candidates <- candidates[!grepl(excl_pat, candidates, ignore.case = TRUE)]
  # BSW files are always handled separately; exclude them regardless of format
  candidates <- candidates[!grepl("_BSW\\.", basename(candidates), ignore.case = TRUE)]

  if (grepl("\\.csv$", ext_pat, fixed = TRUE)) {
    candidates <- candidates[!grepl(
      "codebook|variables|layout|readme|lisezmoi|dictionary",
      basename(candidates), ignore.case = TRUE)]
  }

  if (!is.null(file_mask) && length(candidates) > 1L) {
    filtered <- candidates[grepl(file_mask, basename(candidates),
                                  ignore.case = TRUE)]
    if (length(filtered) > 0L) candidates <- filtered
  }

  if (length(candidates) == 0L)
    stop("Could not find data file in ", version_dir,
         if (!is.null(file_mask)) paste0(" matching '", file_mask, "'"), ".")
  if (length(candidates) > 1L)
    stop("Found multiple candidate data files:\n",
         paste(" ", basename(candidates), collapse = "\n"),
         "\nSet file_mask in the registry entry to select one.")

  candidates[[1L]]
}


# Read bootstrap weight data for a survey version.
# CSV BSW files are read directly.
# FWF BSW files: parse the BSW-specific SPSS command files to get column
# positions, then read FWF.  Returns NULL with a warning if the file is not
# found or the layout cannot be determined.
.read_bsw_data <- function(version_dir, reg, data_encoding) {
  bsw_file_mask <- reg$bsw_file_mask
  if (is.null(bsw_file_mask)) return(NULL)

  all_files <- list.files(version_dir, recursive = TRUE, full.names = TRUE)
  all_files <- all_files[!grepl("/metadata/", all_files, fixed = TRUE)]

  bsw_files <- all_files[grepl(bsw_file_mask, basename(all_files),
                                 ignore.case = TRUE)]
  if (length(bsw_files) == 0L) {
    warning("BSW file matching '", bsw_file_mask, "' not found; ",
            "bootstrap weights will not be joined.")
    return(NULL)
  }
  bsw_path <- bsw_files[[1L]]

  is_csv <- grepl("\\.csv$", bsw_path, ignore.case = TRUE)

  # Parse BSW-specific SPSS metadata (layout + variable types).
  # For FWF BSW this is required; for CSV BSW it provides type information so
  # weight columns are stored as DOUBLE rather than VARCHAR in DuckDB.
  formats    <- detect_formats(version_dir)
  bsw_parsed <- NULL
  if (!is.null(reg$bsw_mask) && !is.null(formats$spss_split)) {
    bsw_parsed <- tryCatch(
      parse_spss_split(formats$spss_split, layout_mask = reg$bsw_mask),
      error = function(e) NULL
    )
  }

  if (is_csv) {
    bsw <- readr::read_csv(bsw_path,
                            col_types = readr::cols(.default = "c"),
                            locale = readr::locale(encoding = data_encoding),
                            show_col_types = FALSE)
    names(bsw) <- toupper(names(bsw))

    # The join key (e.g. PUMFID) must stay character at join time: the main
    # data frame is also all-character until Step 7's .apply_numeric_conversion.
    # Exclude it from all BSW numeric conversions here.
    join_cols <- if (!is.null(reg$bsw_join_key)) toupper(reg$bsw_join_key)
                 else character(0L)
    if (!is.null(bsw_parsed)) {
      bsw_vars <- bsw_parsed$variables[
        !bsw_parsed$variables$name %in% join_cols, , drop = FALSE]
      bsw <- .apply_numeric_conversion(bsw, bsw_vars)
    }
    # BSW weight columns are continuous by definition.  The _vare.sps often
    # labels only the join key, leaving BSWxx columns absent from bsw_vars and
    # still character after the above.  Sweep any remaining character columns.
    for (col in setdiff(names(bsw)[vapply(bsw, is.character, logical(1L))], join_cols))
      bsw[[col]] <- suppressWarnings(as.numeric(bsw[[col]]))
    return(bsw)
  }

  # FWF BSW: need column positions from the BSW-specific SPSS command files.
  if (is.null(bsw_parsed) || is.null(bsw_parsed$layout)) {
    warning("Could not determine BSW column layout for mask '", reg$bsw_mask,
            "'; bootstrap weights will not be joined.")
    return(NULL)
  }

  bsw_layout <- bsw_parsed$layout
  bsw <- readr::read_fwf(
    bsw_path,
    col_positions = readr::fwf_positions(bsw_layout$start, bsw_layout$end,
                                          col_names = bsw_layout$name),
    col_types = readr::cols(.default = "c"),
    trim_ws   = TRUE,
    locale    = readr::locale(encoding = data_encoding),
    show_col_types = FALSE
  )
  join_cols <- if (!is.null(reg$bsw_join_key)) toupper(reg$bsw_join_key)
               else character(0L)
  bsw_vars  <- bsw_parsed$variables[
    !bsw_parsed$variables$name %in% join_cols, , drop = FALSE]
  bsw <- .apply_numeric_conversion(bsw, bsw_vars)
  for (col in setdiff(names(bsw)[vapply(bsw, is.character, logical(1L))], join_cols))
    bsw[[col]] <- suppressWarnings(as.numeric(bsw[[col]]))
  bsw
}


# Apply pre-label data fixups from the registry entry:
#   str_pad — left/right-pad specified columns to a target width
#   rename  — rename columns (only when the old name is present)
.apply_data_fixups <- function(data, fixups) {
  for (spec in fixups$str_pad) {
    for (col in spec$cols) {
      if (col %in% names(data))
        data[[col]] <- stringr::str_pad(data[[col]], width = spec$width,
                                         side = spec$side, pad = spec$pad)
    }
  }
  if (length(fixups$rename) > 0L) {
    old <- names(fixups$rename)
    new <- unname(fixups$rename)
    for (i in seq_along(old))
      if (old[[i]] %in% names(data))
        names(data)[names(data) == old[[i]]] <- new[[i]]
  }
  data
}


# Convert character columns to numeric per the variables metadata.
# Applies missing-value range and registry na_values; selects INTEGER vs DOUBLE
# from decimals field.
# na_values: character vector of raw values that become NA (e.g. c("99999999", "88888888")).
.apply_numeric_conversion <- function(data, variables, na_values = character(0L)) {
  num_vars <- variables[variables$type == "numeric", ]
  for (i in seq_len(nrow(num_vars))) {
    v   <- num_vars[i, ]
    col <- v$name
    if (!col %in% names(data) || !is.character(data[[col]])) next

    raw  <- data[[col]]
    vals <- suppressWarnings(as.numeric(raw))
    if (!is.na(v$missing_low) && !is.na(v$missing_high))
      vals[!is.na(vals) & vals >= v$missing_low & vals <= v$missing_high] <-
        NA_real_
    if (length(na_values) > 0L)
      vals[trimws(raw) %in% na_values] <- NA_real_

    data[[col]] <- if (!is.na(v$decimals) && v$decimals == 0L)
      as.integer(vals) else vals
  }
  data
}


# Map raw character values → factor labels using codes metadata.
# label_col is "label_en" or "label_fr".
# Unmatched raw values become NA (warned).
# Factor levels are the complete ordered set from codes, not just those seen in
# the data; this is the contract from test-factor-enum.R.
.apply_code_labels <- function(data, codes, label_col) {
  char_cols  <- names(data)[vapply(data, is.character, logical(1L))]
  coded_cols <- intersect(char_cols, unique(codes$name))

  for (col in coded_cols) {
    col_codes <- codes[codes$name == col, ]
    labels    <- col_codes[[label_col]]

    # Fall back per-row to label_en when label_fr is NA
    if (label_col == "label_fr") {
      na_lbl  <- is.na(labels)
      if (any(na_lbl)) labels[na_lbl] <- col_codes$label_en[na_lbl]
    }

    valid     <- !is.na(labels)
    lookup    <- stats::setNames(labels[valid], col_codes$val[valid])
    lvls      <- unique(labels[valid])

    raw_vals  <- data[[col]]
    unmatched <- unique(raw_vals[!raw_vals %in% c(names(lookup), NA_character_)])
    if (length(unmatched) > 0L)
      warning("Variable ", col, ": ", length(unmatched),
              " unmatched raw value(s) become NA: ",
              paste(sort(unmatched)[seq_len(min(5L, length(unmatched)))],
                    collapse = ", "),
              if (length(unmatched) > 5L) " ...")

    data[[col]] <- factor(lookup[raw_vals], levels = lvls)
  }
  data
}


# Verify that factor columns were written as ENUM (not VARCHAR) in DuckDB.
# duckdb >= 1.5.2 does this automatically; for older versions this function
# performs ALTER TABLE to enforce ENUM types.
# factor_levels: named list  col_name -> character vector of levels
.ensure_enum_columns <- function(con, table_name, factor_levels) {
  if (length(factor_levels) == 0L) return(invisible(NULL))

  info <- DBI::dbGetQuery(
    con, sprintf("PRAGMA table_info('%s')", table_name))

  for (col in names(factor_levels)) {
    row <- info[info$name == col, ]
    if (nrow(row) == 0L) next
    if (grepl("^ENUM", row$type)) next   # already ENUM — duckdb >= 1.5.2

    lvls     <- factor_levels[[col]]
    lvls_sql <- paste0("'", gsub("'", "''", lvls), "'", collapse = ", ")
    type_nm  <- paste0(gsub("[^A-Za-z0-9]", "_", table_name), "_",
                       gsub("[^A-Za-z0-9]", "_", col), "_enum")

    tryCatch({
      DBI::dbExecute(con, sprintf('DROP TYPE IF EXISTS "%s"', type_nm))
      DBI::dbExecute(con,
        sprintf('CREATE TYPE "%s" AS ENUM (%s)', type_nm, lvls_sql))
      DBI::dbExecute(con,
        sprintf('ALTER TABLE "%s" ALTER COLUMN "%s" TYPE "%s"',
                table_name, col, type_nm))
    }, error = function(e)
      warning("Could not convert '", col, "' to ENUM: ", conditionMessage(e)))
  }
  invisible(NULL)
}


# ============================================================================
# Stage 3 — public function
# ============================================================================

#' Build a labeled DuckDB table for a PUMF version
#'
#' Reads the canonical metadata from `metadata/`, reads the raw data file,
#' optionally joins bootstrap weights, applies code labels as factors, converts
#' numeric columns, and writes to a `.duckdb` file.
#'
#' Skips re-building if the named table already exists in the DuckDB file and
#' `refresh = FALSE`.  Passing `refresh = TRUE` drops and rewrites the table
#' without re-downloading or re-extracting raw data.
#'
#' Returns the db path and table name invisibly.  Call [pumf_open_duckdb()] to
#' open a read-only connection and get a lazy `dplyr::tbl()`.  Keeping Stage 3
#' and connection-opening separate prevents DuckDB file-lock conflicts when
#' building multiple language tables for the same survey in one session.
#'
#' @param version_dir Path returned by [pumf_locate_or_download()].
#' @param series Survey series acronym, e.g. `"SFS"`.
#' @param version Version string, e.g. `"2019"`.
#' @param lang `"eng"` (default) or `"fra"`.
#' @param layout_mask Optional layout mask; used in the DuckDB table name.
#' @param file_mask Optional regex to select the data file.  Overrides registry.
#' @param refresh If `TRUE`, drop and rewrite the DuckDB table.
#'
#' @return Invisibly, a named list with `db_path` and `table_name`.
#' @keywords internal
pumf_build_duckdb <- function(version_dir,
                               series,
                               version,
                               lang        = "eng",
                               layout_mask = NULL,
                               file_mask   = NULL,
                               refresh     = FALSE) {
  stopifnot(lang %in% c("eng", "fra"))

  # Step 1: DuckDB path and table name
  db_file    <- paste0(series, "_",
                       gsub("[^A-Za-z0-9._-]", "_", version), ".duckdb")
  db_path    <- file.path(version_dir, db_file)
  table_name <- if (is.null(layout_mask)) lang
                else paste0(lang, "_", layout_mask)
  result     <- list(db_path = db_path, table_name = table_name)

  # Step 3: skip if the table is already built.
  # Open a temporary connection just for the existence check, then close it so
  # no lock is held when we return.
  if (!refresh && file.exists(db_path)) {
    con_chk <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path,
                               read_only = TRUE)
    exists  <- DBI::dbExistsTable(con_chk, table_name)
    DBI::dbDisconnect(con_chk, shutdown = TRUE)
    if (exists) return(invisible(result))
  }

  # Step 4: read canonical metadata
  meta_dir <- file.path(version_dir, "metadata")
  if (!dir.exists(meta_dir))
    stop("metadata/ not found in ", version_dir,
         ". Run pumf_parse_metadata() first.")

  meta      <- read_metadata(meta_dir)
  variables <- meta$variables
  codes     <- meta$codes
  layout    <- meta$layout  # NULL for CSV-format data
  label_col <- if (lang == "eng") "label_en" else "label_fr"

  # Warn about missing French labels; fall back per-row to English
  if (lang == "fra") {
    na_var <- variables$name[is.na(variables[[label_col]])]
    if (length(na_var) > 0L)
      warning("lang='fra': ", length(na_var),
              " variable(s) have no French label; using label_en for: ",
              paste(head(na_var, 5L), collapse = ", "),
              if (length(na_var) > 5L)
                paste0(" ... and ", length(na_var) - 5L, " more."))

    na_code <- is.na(codes[[label_col]])
    if (any(na_code)) codes[[label_col]][na_code] <- codes$label_en[na_code]
  }

  # Step 5: read data file
  reg      <- pumf_registry_lookup(series, version)
  data_enc <- if (!is.null(reg$data_encoding)) reg$data_encoding else "CP1252"
  eff_mask <- if (!is.null(file_mask)) file_mask else reg$file_mask

  data_path <- .find_pumf_data_file(version_dir, eff_mask, prefer_fwf = !is.null(layout))
  # FWF only when layout exists AND the actual data file is not CSV.
  # Some surveys (e.g. CHS) ship both a CSV and a TXT file; the SPSS DATA LIST
  # section creates a layout.csv, but data must be read from the CSV.
  is_fwf    <- !is.null(layout) && !grepl("\\.csv$", data_path, ignore.case = TRUE)
  message("Reading ", if (is_fwf) "fixed-width" else "CSV",
          " data from ", basename(data_path), " ...")

  if (is_fwf) {
    data <- readr::read_fwf(
      data_path,
      col_positions  = readr::fwf_positions(layout$start, layout$end,
                                             col_names = layout$name),
      col_types      = readr::cols(.default = "c"),
      trim_ws        = TRUE,
      locale         = readr::locale(encoding = data_enc),
      show_col_types = FALSE
    )
  } else {
    data <- readr::read_csv(
      data_path,
      col_types      = readr::cols(.default = "c"),
      locale         = readr::locale(encoding = data_enc),
      show_col_types = FALSE
    )
    names(data) <- toupper(names(data))
  }

  # Apply pre-label data fixups (str_pad, column renames) from registry
  if (!is.null(reg) && length(reg$data_fixups) > 0L)
    data <- .apply_data_fixups(data, reg$data_fixups)

  # Step 6: BSW join
  if (!is.null(reg) && !is.null(reg$bsw_mask) && !is.null(reg$bsw_join_key)) {
    bsw <- .read_bsw_data(version_dir, reg, data_enc)
    if (!is.null(bsw)) {
      drop_cols <- intersect(reg$bsw_drop_cols, names(bsw))
      if (length(drop_cols) > 0L)
        bsw <- bsw[, !names(bsw) %in% drop_cols, drop = FALSE]
      data <- dplyr::left_join(data, bsw, by = reg$bsw_join_key)
    }
  }

  # Step 7: numeric types, then code labels → factors
  na_vals <- if (!is.null(reg)) reg$data_fixups$na_values %||% character(0L)
             else character(0L)
  data <- .apply_numeric_conversion(data, variables, na_values = na_vals)
  data <- .apply_code_labels(data, codes, label_col)

  # Step 9: write to DuckDB
  .assert_duckdb_writable(db_path)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  if (DBI::dbExistsTable(con, table_name))
    DBI::dbRemoveTable(con, table_name)
  message("Writing DuckDB table '", table_name, "' ...")
  DBI::dbWriteTable(con, table_name, data)

  # Step 8: verify / enforce ENUM on factor columns
  factor_cols <- names(data)[vapply(data, is.factor, logical(1L))]
  if (length(factor_cols) > 0L)
    .ensure_enum_columns(
      con, table_name,
      stats::setNames(lapply(factor_cols, function(c) levels(data[[c]])),
                      factor_cols))

  # Step 10: disconnect writer — no lingering connections so the next
  # pumf_build_duckdb call (e.g. for lang="fra") can open the file read-write.
  DBI::dbDisconnect(con, shutdown = TRUE)
  invisible(result)
}


#' Open a DuckDB table as a lazy dplyr tbl
#'
#' Opens a connection to the `.duckdb` file produced by [pumf_build_duckdb()]
#' and returns a lazy `dplyr::tbl()`.  Use [close_pumf()] to release the
#' connection when done.
#'
#' @param db_path Path to the `.duckdb` file.
#' @param table_name Name of the table to open.
#' @param read_only Open in read-only mode (default `TRUE`).  Pass `FALSE` to
#'   allow write operations on the DuckDB file (e.g. to add custom views).
#'
#' @return A lazy `dplyr::tbl()`.
#' @keywords internal
pumf_open_duckdb <- function(db_path, table_name, read_only = TRUE) {
  if (!file.exists(db_path))
    stop("DuckDB file not found: ", db_path,
         ". Run pumf_build_duckdb() first.")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path,
                         read_only = read_only)
  if (!DBI::dbExistsTable(con, table_name)) {
    DBI::dbDisconnect(con, shutdown = TRUE)
    stop("Table '", table_name, "' not found in ", db_path, ".")
  }
  dplyr::tbl(con, table_name)
}


# ============================================================================
# Pipeline orchestrator
# ============================================================================

#' Run the full three-stage PUMF pipeline for one survey version
#'
#' Convenience wrapper that:
#' 1. Looks up the survey registry entry for `(series, version)`.
#' 2. Calls [pumf_locate_or_download()] (Stage 1).
#' 3. Calls [pumf_parse_metadata()] (Stage 2) with the registry's
#'    `layout_mask` and `metadata_encoding`.
#' 4. Calls [pumf_build_duckdb()] (Stage 3).
#' 5. Returns a lazy `dplyr::tbl()` via [pumf_open_duckdb()].
#'
#' Each stage is idempotent: subsequent calls reuse cached results unless
#' `refresh = TRUE`.
#'
#' @param series Survey series acronym, e.g. `"SFS"` or `"CHS"`.
#' @param version Version string, e.g. `"2019"` or `"2021 (individuals)"`.
#' @param lang `"eng"` (default) or `"fra"`.
#' @param cache_path Root cache directory.  Defaults to
#'   `getOption("canpumf.cache_path", tempdir())`.
#' @param refresh If `TRUE`, clear DuckDB and metadata and rebuild from
#'   already-extracted raw data.  Does **not** re-download.
#' @param redownload If `TRUE`, delete the zip and all extracted content and
#'   re-download from StatCan before rebuilding.  Implies `refresh = TRUE`.
#' @param read_only Open the DuckDB in read-only mode (default `TRUE`).
#'
#' @return A lazy `dplyr::tbl()` backed by a DuckDB connection.
#' @keywords internal
pumf_run_pipeline <- function(series,
                               version,
                               lang       = "eng",
                               cache_path = getOption("canpumf.cache_path",
                                                      tempdir()),
                               refresh    = FALSE,
                               redownload = FALSE,
                               read_only  = TRUE) {
  stopifnot(lang %in% c("eng", "fra"))

  reg <- pumf_registry_lookup(series, version)

  # redownload implies a full rebuild
  eff_refresh <- refresh || redownload

  # When a rebuild is requested and the DB already exists, verify it is not
  # locked before doing any download / parse work.  For cache hits (no refresh,
  # table already built) no write will occur, so multiple read-only connections
  # are fine and the check must not run.  For first-time builds the file does
  # not exist yet, so .assert_duckdb_writable is a no-op; the check inside
  # pumf_build_duckdb() covers that write.
  if (eff_refresh)
    .assert_duckdb_writable(.pumf_db_path(series, version, cache_path))

  # Stage 1 — locate or download
  version_dir <- pumf_locate_or_download(series, version,
                                          cache_path = cache_path,
                                          refresh    = eff_refresh,
                                          redownload = redownload)

  # Stage 2 — parse metadata
  pumf_parse_metadata(version_dir,
                       layout_mask       = reg$layout_mask,
                       metadata_encoding = reg$metadata_encoding,
                       refresh           = eff_refresh)

  # Stage 3 — build DuckDB
  result <- pumf_build_duckdb(version_dir, series, version,
                               lang        = lang,
                               layout_mask = reg$layout_mask,
                               refresh     = eff_refresh)

  pumf_open_duckdb(result$db_path, result$table_name, read_only = read_only)
}
