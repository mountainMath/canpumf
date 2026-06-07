# R/lfs_pipeline.R — LFS longitudinal database pipeline.
#
# All LFS versions share a single DuckDB at <cache_path>/LFS/LFS.duckdb.
# Per-version zip and metadata live at <cache_path>/LFS/<version>/.
# The DuckDB holds up to three tables:
#   lfs_eng      — English-labeled rows from all loaded versions
#   lfs_fra      — French-labeled rows from all loaded versions
#   lfs_versions — tracking table (version, type, survyear, survmnth, ...)
#
# SURVYEAR and SURVMNTH are stored as INTEGER in both data tables (not labeled)
# so that filtering `WHERE SURVYEAR = 2023 AND SURVMNTH = 6` works directly.
#
# Connection discipline: every write block opens a fresh RW connection and
# closes it before returning.  The returned tbl holds an open RO connection;
# callers who need to load additional versions must first collect() or
# disconnect the previous tbl (DuckDB allows at most one RW connection).


# ---- Internal helpers -------------------------------------------------------

# Parse version string → "annual" | "monthly"
.lfs_version_type <- function(v) {
  if (grepl("^[0-9]{4}$", v))        "annual"
  else if (grepl("^[0-9]{4}-[0-9]{2}$", v)) "monthly"
  else stop("Invalid LFS version string '", v,
            "'. Expected YYYY (annual) or YYYY-MM (monthly).")
}

.lfs_survyear <- function(v) as.integer(substr(v, 1L, 4L))
.lfs_survmnth <- function(v) {
  if (grepl("-", v, fixed = TRUE)) as.integer(substr(v, 6L, 7L))
  else NA_integer_
}

# Create lfs_versions tracking table if it does not exist.
.lfs_ensure_versions_table <- function(con) {
  if (!DBI::dbExistsTable(con, "lfs_versions"))
    DBI::dbExecute(con, "
      CREATE TABLE lfs_versions (
        version        VARCHAR,
        type           VARCHAR,
        survyear       INTEGER,
        survmnth       INTEGER,
        downloaded_at  TIMESTAMP DEFAULT NOW(),
        n_records      INTEGER
      )")
}

# TRUE when lfs_versions contains an entry for this version string.
.lfs_version_exists <- function(con, version) {
  .lfs_ensure_versions_table(con)
  n <- DBI::dbGetQuery(
    con,
    sprintf("SELECT COUNT(*) AS n FROM lfs_versions WHERE version = '%s'", version))$n
  n > 0L
}

# TRUE when lfs_versions contains an annual entry for this calendar year.
.lfs_has_annual <- function(con, survyear) {
  .lfs_ensure_versions_table(con)
  n <- DBI::dbGetQuery(
    con,
    sprintf("SELECT COUNT(*) AS n FROM lfs_versions
             WHERE survyear = %d AND type = 'annual'", survyear))$n
  n > 0L
}

# Monthly versions recorded in lfs_versions for a given year.
.lfs_monthly_versions <- function(con, survyear) {
  .lfs_ensure_versions_table(con)
  DBI::dbGetQuery(
    con,
    sprintf("SELECT version FROM lfs_versions
             WHERE survyear = %d AND type = 'monthly'", survyear))$version
}

# TRUE when data_tbl has any rows for (survyear [, survmnth]).
.lfs_data_exists <- function(con, data_tbl, survyear, survmnth = NA_integer_) {
  if (!DBI::dbExistsTable(con, data_tbl)) return(FALSE)
  sql <- sprintf("SELECT COUNT(*) AS n FROM \"%s\" WHERE SURVYEAR = %d",
                 data_tbl, survyear)
  if (!is.na(survmnth))
    sql <- paste0(sql, sprintf(" AND SURVMNTH = %d", survmnth))
  DBI::dbGetQuery(con, sql)$n > 0L
}

# Append new_data to table_name, extending the schema when columns differ.
#   New columns in new_data → ALTER TABLE ADD COLUMN (NULL for old rows).
#   Columns in table missing from new_data → NA column before append.
#
# Factor columns are converted to character (VARCHAR) before writing.
# The LFS shared table spans multiple years; label strings can legitimately
# differ across years (e.g. a province name update), so ENUM types would
# conflict on append.  VARCHAR is the right type for longitudinal LFS data.
.lfs_append <- function(con, table_name, new_data) {
  for (col in names(new_data))
    if (is.factor(new_data[[col]]))
      new_data[[col]] <- as.character(new_data[[col]])

  if (!DBI::dbExistsTable(con, table_name)) {
    DBI::dbWriteTable(con, table_name, new_data)
    return(invisible(NULL))
  }

  existing <- DBI::dbListFields(con, table_name)
  incoming <- names(new_data)

  for (col in setdiff(incoming, existing)) {
    sql_type <- if (is.integer(new_data[[col]])) "INTEGER"
    else if (is.numeric(new_data[[col]]))  "DOUBLE"
    else if (is.logical(new_data[[col]]))  "BOOLEAN"
    else "VARCHAR"
    DBI::dbExecute(
      con, sprintf('ALTER TABLE "%s" ADD COLUMN "%s" %s',
                   table_name, col, sql_type))
    message("  Added new column '", col, "' (", sql_type, ") to ", table_name)
  }

  all_cols <- DBI::dbListFields(con, table_name)
  for (col in setdiff(all_cols, incoming))
    new_data[[col]] <- NA

  DBI::dbAppendTable(con, table_name, new_data[, all_cols, drop = FALSE])
  invisible(NULL)
}

# Locate LFS CSV data files in a version directory.
#
# StatCan has shipped LFS data in several formats over the years:
#   (a) One annual file:          pub<YYYY>.csv        (all 12 months)
#   (b) 12 bundled monthly files: pub<MM><YY>.csv × 12 (one per month)
#   (c) Single monthly release:   pub<MM><YY>.csv × 1  (current-year update)
#
# All cases are handled identically — every matching file is returned and the
# caller binds them.  We try the known "pub*.csv" prefix first; if that yields
# nothing we fall back to any non-metadata CSV so the function keeps working if
# StatCan ever renames their files.
#
# Returns a sorted character vector of full paths, never length-0 (the caller
# stops on empty).  Files inside metadata/ are always excluded.
.lfs_find_data_files <- function(version_dir) {
  all_files <- list.files(version_dir, recursive = TRUE, full.names = TRUE)
  all_files <- all_files[!startsWith(
    normalizePath(all_files, mustWork = FALSE),
    normalizePath(file.path(version_dir, "metadata"), mustWork = FALSE)
  )]

  # Primary pattern — every known StatCan LFS release uses this prefix
  primary <- all_files[grepl("^pub[^/]*\\.csv$", basename(all_files),
                              ignore.case = TRUE, perl = TRUE)]
  if (length(primary) > 0L) return(sort(primary))

  # Fallback — any CSV that doesn't look like a codebook / layout file
  fallback <- all_files[
    grepl("\\.csv$", all_files, ignore.case = TRUE) &
    !grepl("codebook|layout|readme|lisezmoi|variables|codes",
            basename(all_files), ignore.case = TRUE)]
  sort(fallback)
}


# Classify one LFS data filename as "annual" or "monthly" based on the digit
# string that follows the "pub" prefix.  The heuristic works for all known
# StatCan naming conventions:
#
#   pub2023.csv   → first two digits "20" > 12  → annual  (year = 2023)
#   pub0120.csv   → first two digits "01" ≤ 12  → monthly (Jan 2020)
#   pub1223.csv   → first two digits "12" ≤ 12  → monthly (Dec 2023)
#   pub012024.csv → first two digits "01" ≤ 12  → monthly (Jan 2024, long year)
#
# Non-pub filenames (from the fallback path) are classified as "unknown".
.lfs_file_format <- function(path) {
  nm <- tolower(basename(path))
  if (!startsWith(nm, "pub")) return("unknown")
  # Capture the full digit string without stripping leading zeros:
  #   pub0223.csv → "0223" (not "223"), first two chars "02" → month 2 → monthly
  #   pub2023.csv → "2023", first two chars "20" → 20 > 12 → annual
  digits <- gsub("(?i)^pub([0-9]+)\\.csv$", "\\1", nm, perl = TRUE)
  if (!grepl("^[0-9]+$", digits)) return("unknown")
  if (as.integer(substr(digits, 1L, 2L)) > 12L) "annual" else "monthly"
}


# Build labeled data frame for one LFS version.
# SURVYEAR and SURVMNTH are kept as INTEGER regardless of their type in
# variables.csv so that SQL filtering works without knowing the label strings.
.lfs_build_version <- function(version_dir, label_col) {
  meta      <- read_metadata(file.path(version_dir, "metadata"))
  variables <- meta$variables
  codes     <- meta$codes

  # Force SURVYEAR and SURVMNTH to integer before label mapping
  lfs_int_cols <- c("SURVYEAR", "SURVMNTH")
  variables$type[variables$name %in% lfs_int_cols]    <- "numeric"
  variables$decimals[variables$name %in% lfs_int_cols] <- 0L

  # Exclude SURVYEAR and SURVMNTH from code labeling (kept as integers)
  codes_lbl <- codes[!codes$name %in% lfs_int_cols, ]

  # Locate data files — supports all three StatCan shipping formats:
  #   (a) single annual file, (b) 12 bundled monthly files, (c) one monthly
  data_files <- .lfs_find_data_files(version_dir)

  if (length(data_files) == 0L)
    stop("No LFS data files found in ", version_dir,
         ".\nExpected pub*.csv files (or any non-metadata CSV as fallback).")

  # Classify and log what was found
  formats   <- vapply(data_files, .lfs_file_format, character(1L))
  n_annual  <- sum(formats == "annual")
  n_monthly <- sum(formats == "monthly")

  if (n_annual == 1L && n_monthly == 0L) {
    message("  annual file: ", basename(data_files[[1L]]))
  } else if (n_monthly > 0L && n_annual == 0L) {
    message("  ", n_monthly, " monthly file(s): ",
            paste(basename(data_files), collapse = ", "))
  } else {
    # Mixed or unknown format — read everything, let the caller sort it out
    message("  ", length(data_files), " data file(s): ",
            paste(basename(data_files), collapse = ", "))
    if (n_annual > 0L && n_monthly > 0L)
      warning("LFS version directory contains both annual- and monthly-format ",
              "files; all will be combined. Remove duplicates if rows are ",
              "double-counted.", call. = FALSE)
  }

  data <- dplyr::bind_rows(lapply(data_files, function(p) {
    df <- readr::read_csv(p,
                           col_types = readr::cols(.default = "c"),
                           locale    = readr::locale(encoding = "CP1252"),
                           show_col_types = FALSE)
    names(df) <- toupper(names(df))
    df
  }))

  # Guard: ensure the key survey dimension columns are present
  missing_cols <- setdiff(lfs_int_cols, names(data))
  if (length(missing_cols) > 0L)
    stop("LFS data is missing required columns: ",
         paste(missing_cols, collapse = ", "),
         "\nCheck that the correct data file(s) are in ", version_dir)

  data <- .apply_numeric_conversion(data, variables)
  data <- .apply_code_labels(data, codes_lbl, label_col)
  data
}

# Open a filtered lazy tbl from the LFS DuckDB.
# Returns the full table when survyear is NULL.
.lfs_open_tbl <- function(db_path, data_tbl, survyear = NULL,
                            survmnth = NA_integer_, read_only = TRUE) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path,
                         read_only = read_only)
  if (!DBI::dbExistsTable(con, data_tbl)) {
    DBI::dbDisconnect(con, shutdown = TRUE)
    stop("Table '", data_tbl, "' does not exist in ", db_path,
         ". No LFS data has been loaded yet.")
  }
  tbl <- dplyr::tbl(con, data_tbl)
  if (!is.null(survyear)) {
    tbl <- dplyr::filter(tbl, .data$SURVYEAR == survyear)
    if (!is.na(survmnth))
      tbl <- dplyr::filter(tbl, .data$SURVMNTH == survmnth)
  }
  tbl
}

# Delete data for a year and remove matching lfs_versions rows.
.lfs_delete_year <- function(con, data_tbl, survyear, type_filter = NULL) {
  if (DBI::dbExistsTable(con, data_tbl))
    DBI::dbExecute(
      con,
      sprintf('DELETE FROM "%s" WHERE SURVYEAR = %d', data_tbl, survyear))
  sql <- sprintf("DELETE FROM lfs_versions WHERE survyear = %d", survyear)
  if (!is.null(type_filter))
    sql <- paste0(sql, sprintf(" AND type = '%s'", type_filter))
  DBI::dbExecute(con, sql)
}

# Record a version in lfs_versions.
.lfs_record_version <- function(con, version, vtype, survyear, survmnth, n) {
  survmnth_sql <- if (is.na(survmnth)) "NULL" else as.character(survmnth)
  DBI::dbExecute(
    con,
    sprintf(
      "INSERT INTO lfs_versions (version,type,survyear,survmnth,downloaded_at,n_records)
       VALUES ('%s','%s',%d,%s,NOW(),%d)",
      version, vtype, survyear, survmnth_sql, n))
}


# ---- Status helper (version = NULL) ----------------------------------------

.lfs_status <- function(db_path, data_tbl, lang, read_only = TRUE) {
  if (!file.exists(db_path)) {
    message("LFS database does not exist yet. ",
            "Call lfs_get_pumf(version = \"YYYY\") to load a version, ",
            "or lfs_get_pumf(refresh = \"auto\") to load all available versions.")
    return(invisible(NULL))
  }

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  .lfs_ensure_versions_table(con)

  versions_df <- DBI::dbGetQuery(
    con,
    "SELECT version, type, survyear, survmnth FROM lfs_versions ORDER BY survyear, survmnth")

  if (nrow(versions_df) == 0L) {
    message("LFS database exists but no versions have been loaded yet.")
    return(invisible(NULL))
  }

  # Summarise what's in the database
  annual  <- versions_df$version[versions_df$type == "annual"]
  monthly <- versions_df$version[versions_df$type == "monthly"]
  parts   <- character(0L)
  if (length(annual)  > 0L)
    parts <- c(parts, paste0(paste(annual, collapse = ", "), " (annual)"))
  if (length(monthly) > 0L)
    parts <- c(parts, paste0(paste(monthly, collapse = ", "), " (monthly)"))

  has_lang <- DBI::dbExistsTable(con, data_tbl)
  lang_tag  <- if (has_lang) paste0(" [", lang, " table present]")
              else paste0(" [no ", lang, " table yet]")

  message("LFS database contains: ", paste(parts, collapse = ", "), lang_tag)

  if (!has_lang) return(invisible(NULL))
  .lfs_open_tbl(db_path, data_tbl, read_only = read_only)
}


# ---- Auto-refresh helper ----------------------------------------------------

.lfs_auto_refresh <- function(db_path, data_tbl, label_col, lang, cache_path,
                                read_only = TRUE) {
  available <- tryCatch(
    list_available_lfs_pumf_versions(),
    error = function(e) {
      warning("Could not fetch LFS versions from StatCan: ", conditionMessage(e))
      tibble::tibble(version = character(0L))
    }
  )

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  .lfs_ensure_versions_table(con)
  loaded <- DBI::dbGetQuery(con, "SELECT version FROM lfs_versions")$version
  DBI::dbDisconnect(con, shutdown = TRUE)

  to_add <- setdiff(available$version, loaded)
  if (length(to_add) == 0L) {
    message("LFS database is up to date.")
  } else {
    message("Loading ", length(to_add), " LFS version(s): ",
            paste(to_add, collapse = ", "))
    for (v in to_add) {
      tryCatch(
        lfs_get_pumf(v, lang = lang, cache_path = cache_path,
                     read_only = FALSE),
        error = function(e)
          warning("Failed to load LFS ", v, ": ", conditionMessage(e))
      )
    }
  }

  .lfs_open_tbl(db_path, data_tbl, read_only = read_only)
}


# ---- Public function --------------------------------------------------------

#' Get Labour Force Survey PUMF data from a shared longitudinal DuckDB
#'
#' Manages a single `LFS.duckdb` file that accumulates all downloaded LFS
#' versions.  Each call either retrieves already-loaded data or downloads,
#' parses, labels, and appends a new version.
#'
#' **Version types**:
#' - `"YYYY"` (e.g. `"2023"`) — annual file released by StatCan after year-end.
#' - `"YYYY-MM"` (e.g. `"2024-06"`) — monthly file for the current year.
#'
#' When an annual file for year Y is loaded and monthly files for that year are
#' already in the database, the monthly rows are replaced (supersession).
#' Conversely, if an annual for year Y is already loaded, requesting a monthly
#' for that year returns the annual data filtered to that month without
#' re-downloading.
#'
#' **Connection note**: the returned `tbl` holds an open DuckDB connection.
#' Loading a second version (i.e. calling `lfs_get_pumf` again while holding
#' the first result) requires the first tbl's connection to be closed first.
#' Use [close_pumf()] or `dplyr::collect()` the result before the next call.
#'
#' @param version LFS version string (`"YYYY"` or `"YYYY-MM"`), or `NULL` to
#'   report database state and return the full table.
#' @param lang `"eng"` (default) or `"fra"`.
#' @param cache_path Root cache directory.
#' @param refresh `FALSE` (default), `TRUE` (re-download and re-label the
#'   specified version), or `"auto"` (download all versions not yet in the
#'   database).  `refresh = TRUE` requires a non-NULL `version`.
#' @param read_only Open the DuckDB connection in read-only mode (default
#'   `TRUE`).  Pass `FALSE` to allow write access to the LFS DuckDB.
#'
#' @return A lazy `dplyr::tbl()`, or `invisible(NULL)` when `version = NULL`
#'   and no data has been loaded.
#' @keywords internal
lfs_get_pumf <- function(version    = NULL,
                          lang       = "eng",
                          cache_path = getOption("canpumf.cache_path",
                                                  tempdir()),
                          refresh    = FALSE,
                          read_only  = TRUE) {
  stopifnot(lang %in% c("eng", "fra"))
  if (!identical(refresh, FALSE) &&
      !identical(refresh, TRUE)  &&
      !identical(refresh, "auto"))
    stop("'refresh' must be FALSE, TRUE, or \"auto\".")
  if (identical(refresh, TRUE) && is.null(version))
    stop("Specify a version when refresh = TRUE.  ",
         "Use refresh = \"auto\" to reload all versions.")

  lfs_dir   <- file.path(cache_path, "LFS")
  db_path   <- file.path(lfs_dir, "LFS.duckdb")
  data_tbl  <- paste0("lfs_", lang)
  label_col <- if (lang == "eng") "label_en" else "label_fr"

  dir.create(lfs_dir, showWarnings = FALSE, recursive = TRUE)

  # version = NULL: just report status
  if (is.null(version))
    return(.lfs_status(db_path, data_tbl, lang, read_only = read_only))

  # refresh = "auto": download everything missing
  if (identical(refresh, "auto"))
    return(.lfs_auto_refresh(db_path, data_tbl, label_col, lang, cache_path,
                              read_only = read_only))

  vtype    <- .lfs_version_type(version)
  survyear <- .lfs_survyear(version)
  survmnth <- .lfs_survmnth(version)

  # --- check existing data via a read-only connection ---
  if (file.exists(db_path)) {
    con_chk <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path,
                               read_only = TRUE)
    .lfs_ensure_versions_table(con_chk)

    if (!identical(refresh, TRUE)) {
      # Monthly requested but annual already covers this year
      if (vtype == "monthly" && .lfs_has_annual(con_chk, survyear)) {
        message("Annual LFS data for ", survyear, " already loaded; ",
                version, " is covered.")
        DBI::dbDisconnect(con_chk, shutdown = TRUE)
        return(.lfs_open_tbl(db_path, data_tbl, survyear, survmnth,
                              read_only = read_only))
      }
      # Already loaded for this version AND in the lang table.
      # Use lfs_versions (not just data presence) to distinguish "annual
      # already loaded" from "only monthly data happens to exist for this year".
      already_downloaded <- .lfs_version_exists(con_chk, version)
      already_in_lang    <- .lfs_data_exists(con_chk, data_tbl, survyear, survmnth)
      if (already_downloaded && already_in_lang) {
        DBI::dbDisconnect(con_chk, shutdown = TRUE)
        return(.lfs_open_tbl(db_path, data_tbl, survyear, survmnth,
                              read_only = read_only))
      }
    }
    DBI::dbDisconnect(con_chk, shutdown = TRUE)
  }

  # --- Stage 1: locate / download ---
  version_dir <- pumf_locate_or_download("LFS", version,
                                          cache_path = cache_path,
                                          refresh    = identical(refresh, TRUE))

  # --- Stage 2: parse metadata ---
  pumf_parse_metadata(version_dir, refresh = identical(refresh, TRUE))

  # --- Stage 3 (LFS variant): build labeled data and append ---
  message("Labeling LFS ", version, " [", lang, "] ...")
  data <- .lfs_build_version(version_dir, label_col)
  n    <- nrow(data)

  # Write phase: open RW, append, close
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  .lfs_ensure_versions_table(con)

  if (identical(refresh, TRUE)) {
    # Wipe existing data for this year before re-appending
    .lfs_delete_year(con, data_tbl, survyear)
  } else if (vtype == "annual") {
    # Supersede monthly data for this year
    monthlies <- .lfs_monthly_versions(con, survyear)
    if (length(monthlies) > 0L) {
      message("Annual LFS ", version, " supersedes monthly: ",
              paste(monthlies, collapse = ", "))
      .lfs_delete_year(con, data_tbl, survyear, type_filter = "monthly")
    }
  }

  .lfs_append(con, data_tbl, data)

  # Record in lfs_versions (once per version, not per lang)
  if (!.lfs_version_exists(con, version))
    .lfs_record_version(con, version, vtype, survyear, survmnth, n)

  DBI::dbDisconnect(con, shutdown = TRUE)
  message("LFS ", version, ": ", n, " rows appended to ", data_tbl, ".")

  .lfs_open_tbl(db_path, data_tbl, survyear, survmnth, read_only = read_only)
}
