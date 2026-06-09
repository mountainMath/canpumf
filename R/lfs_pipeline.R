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

# Parse level names from a DuckDB inline ENUM type string.
# e.g. "ENUM('a', 'b''c')" → c("a", "b'c")
.parse_inline_enum_levels <- function(type_str) {
  if (!startsWith(type_str, "ENUM(")) return(character(0L))
  inner <- substr(type_str, 6L, nchar(type_str) - 1L)
  vals  <- character(0L)
  i     <- 1L
  n     <- nchar(inner)
  while (i <= n) {
    if (substr(inner, i, i) == "'") {
      j <- i + 1L
      while (j <= n) {
        if (substr(inner, j, j) == "'") {
          if (j < n && substr(inner, j + 1L, j + 1L) == "'") {
            j <- j + 2L   # escaped ''
          } else {
            break          # closing quote
          }
        } else {
          j <- j + 1L
        }
      }
      vals <- c(vals, gsub("''", "'", substr(inner, i + 1L, j - 1L)))
      i    <- j + 2L       # skip closing quote + ", " separator
    } else {
      i <- i + 1L
    }
  }
  vals
}

# Append new_data to table_name, extending the schema when columns differ.
#   New columns in new_data → ALTER TABLE ADD COLUMN (NULL for old rows).
#   Columns in table missing from new_data → NA column before append.
#   Factor columns → stored as ENUM; types evolved on each append as needed.
.lfs_append <- function(con, table_name, new_data) {
  factor_cols   <- names(new_data)[vapply(new_data, is.factor, logical(1L))]
  factor_levels <- stats::setNames(
    lapply(factor_cols, function(c) levels(new_data[[c]])),
    factor_cols)

  # ---- First write: create table, enforce ENUM for factor columns ----
  if (!DBI::dbExistsTable(con, table_name)) {
    DBI::dbWriteTable(con, table_name, new_data)
    # DuckDB >= 1.5.2 auto-creates inline ENUMs; .ensure_enum_columns is a
    # no-op for columns that are already ENUM, so this is safe for all versions.
    if (length(factor_cols) > 0L)
      .ensure_enum_columns(con, table_name, factor_levels)
    return(invisible(NULL))
  }

  # ---- Schema evolution ----
  existing <- DBI::dbListFields(con, table_name)
  incoming <- names(new_data)

  for (col in setdiff(incoming, existing)) {
    r_val    <- new_data[[col]]
    sql_type <- if (is.integer(r_val)) "INTEGER"
                else if (is.numeric(r_val)) "DOUBLE"
                else if (is.logical(r_val)) "BOOLEAN"
                else "VARCHAR"
    DBI::dbExecute(con, sprintf('ALTER TABLE "%s" ADD COLUMN "%s" %s',
                                 table_name, col, sql_type))
    message("  Added new column '", col, "' (", sql_type, ") to ", table_name)
  }

  schema_df   <- DBI::dbGetQuery(con, paste0(
    "SELECT column_name, data_type FROM information_schema.columns ",
    "WHERE table_name = '", table_name, "' AND table_schema = 'main'"))
  pragma_info <- DBI::dbGetQuery(
    con, sprintf("PRAGMA table_info('%s')", table_name))

  for (col in intersect(incoming, schema_df$column_name)) {
    db_type     <- schema_df$data_type[schema_df$column_name == col]
    pragma_type <- pragma_info$type[pragma_info$name == col]
    r_val       <- new_data[[col]]

    # VARCHAR → numeric (e.g. FINALWT corrected after a metadata fix)
    if ((is.integer(r_val) || is.numeric(r_val)) &&
        db_type %in% c("VARCHAR", "TEXT", "CHAR", "CHARACTER VARYING")) {
      new_sql <- if (is.integer(r_val)) "INTEGER" else "DOUBLE"
      tryCatch({
        DBI::dbExecute(con, sprintf(
          'ALTER TABLE "%s" ALTER COLUMN "%s" SET DATA TYPE %s',
          table_name, col, new_sql))
        message("  Upgraded column '", col, "' from VARCHAR to ", new_sql,
                " in ", table_name)
      }, error = function(e)
        warning("Could not upgrade type of '", col, "' from VARCHAR to ", new_sql,
                ": ", conditionMessage(e),
                "\nUse redownload = TRUE for a full clean rebuild.", call. = FALSE))
    }

    # DOUBLE → INTEGER (e.g. REC_NUM now forced to integer after a metadata fix)
    if (is.integer(r_val) && db_type == "DOUBLE") {
      tryCatch({
        DBI::dbExecute(con, sprintf(
          'ALTER TABLE "%s" ALTER COLUMN "%s" SET DATA TYPE INTEGER',
          table_name, col))
        message("  Upgraded column '", col, "' from DOUBLE to INTEGER in ", table_name)
      }, error = function(e)
        warning("Could not upgrade type of '", col, "' from DOUBLE to INTEGER: ",
                conditionMessage(e),
                "\nUse redownload = TRUE for a full clean rebuild.", call. = FALSE))
    }

    # ENUM evolution: extend inline ENUM with any new factor levels
    if (col %in% factor_cols && grepl("^ENUM", db_type)) {
      current_levels <- .parse_inline_enum_levels(pragma_type)
      new_lvls       <- setdiff(factor_levels[[col]], current_levels)
      if (length(new_lvls) > 0L) {
        all_levels <- union(current_levels, factor_levels[[col]])
        lvls_sql   <- paste0("'", gsub("'", "''", all_levels), "'", collapse = ", ")
        tryCatch({
          DBI::dbExecute(con, sprintf(
            'ALTER TABLE "%s" ALTER COLUMN "%s" TYPE ENUM(%s)',
            table_name, col, lvls_sql))
          message("  Extended ENUM for '", col, "' with: ",
                  paste(new_lvls, collapse = ", "))
        }, error = function(e)
          warning("Could not extend ENUM for '", col, "': ", conditionMessage(e),
                  call. = FALSE))
      }
    }

    # VARCHAR → ENUM: column predates ENUM enforcement; upgrade now
    if (col %in% factor_cols &&
        db_type %in% c("VARCHAR", "TEXT", "CHAR", "CHARACTER VARYING")) {
      # Include any existing values in the data so the cast doesn't fail
      existing_vals <- tryCatch(
        na.omit(DBI::dbGetQuery(con, sprintf(
          'SELECT DISTINCT CAST("%s" AS VARCHAR) AS val FROM "%s"',
          col, table_name))$val),
        error = function(e) character(0L))
      all_levels <- union(existing_vals, factor_levels[[col]])
      lvls_sql   <- paste0("'", gsub("'", "''", all_levels), "'", collapse = ", ")
      tryCatch({
        DBI::dbExecute(con, sprintf(
          'ALTER TABLE "%s" ALTER COLUMN "%s" TYPE ENUM(%s)',
          table_name, col, lvls_sql))
        message("  Upgraded column '", col, "' from VARCHAR to ENUM in ", table_name)
      }, error = function(e)
        warning("Could not upgrade '", col, "' to ENUM: ", conditionMessage(e),
                call. = FALSE))
    }
  }

  # ---- Append ----
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

  # Force these to integer regardless of what the codebook says.
  # SURVYEAR/SURVMNTH: needed so SQL year/month filters work on integers.
  # REC_NUM: a record counter — always whole numbers, never needs double precision.
  lfs_int_cols <- c("SURVYEAR", "SURVMNTH", "REC_NUM")
  variables$type[variables$name %in% lfs_int_cols]    <- "numeric"
  variables$decimals[variables$name %in% lfs_int_cols] <- 0L

  # Exclude these from code labeling (kept as raw integers)
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

  data <- bind_rows(lapply(data_files, function(p) {
    df <- readr::read_csv(p,
                           col_types = readr::cols(.default = "c"),
                           locale    = readr::locale(encoding = "CP1252"),
                           show_col_types = FALSE)
    names(df) <- toupper(names(df))
    df
  }))

  # Guard: ensure the key survey dimension columns are present
  missing_cols <- setdiff(c("SURVYEAR", "SURVMNTH"), names(data))
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
  tbl <- tbl(con, data_tbl)
  if (!is.null(survyear)) {
    tbl <- filter(tbl, .data$SURVYEAR == survyear)
    if (!is.na(survmnth))
      tbl <- filter(tbl, .data$SURVMNTH == survmnth)
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

  # Read what's already loaded via a read-only connection — no write needed yet.
  loaded <- character(0L)
  if (file.exists(db_path)) {
    con_ro <- tryCatch(
      DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE),
      error = function(e) NULL
    )
    if (!is.null(con_ro)) {
      if (DBI::dbExistsTable(con_ro, "lfs_versions"))
        loaded <- DBI::dbGetQuery(con_ro, "SELECT version FROM lfs_versions")$version
      DBI::dbDisconnect(con_ro, shutdown = TRUE)
    }
  }

  to_add <- setdiff(available$version, loaded)
  if (length(to_add) == 0L) {
    message("LFS database is up to date.")
  } else {
    # Write access is needed — check for a locked connection before starting.
    .assert_duckdb_writable(db_path)
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
#' @param refresh `FALSE` (default), `TRUE` (re-parse and re-label the
#'   specified version from the cached raw files), or `"auto"` (download all
#'   versions not yet in the database).  `refresh = TRUE` requires a non-NULL
#'   `version`.
#' @param redownload If `TRUE`, delete the cached zip and extracted content for
#'   the specified version and re-download from StatCan before rebuilding.
#'   Implies `refresh = TRUE`.  Requires a non-NULL `version`.
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
                          redownload = FALSE,
                          read_only  = TRUE) {
  stopifnot(lang %in% c("eng", "fra"))
  if (!identical(refresh, FALSE) &&
      !identical(refresh, TRUE)  &&
      !identical(refresh, "auto"))
    stop("'refresh' must be FALSE, TRUE, or \"auto\".")
  # No version specified + rebuild requested → rebuild all loaded versions.
  if ((identical(refresh, TRUE) || isTRUE(redownload)) && is.null(version)) {
    lfs_dir_e  <- file.path(cache_path, "LFS")
    db_path_e  <- file.path(lfs_dir_e, "LFS.duckdb")
    data_tbl_e <- paste0("lfs_", lang)

    if (!file.exists(db_path_e)) {
      message("No LFS data in cache. Download a version first: ",
              "get_pumf(\"LFS\", \"2024\")")
      return(invisible(NULL))
    }
    con_e <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path_e, read_only = TRUE)
    loaded <- if (DBI::dbExistsTable(con_e, "lfs_versions"))
      DBI::dbGetQuery(con_e,
        "SELECT version FROM lfs_versions ORDER BY survyear, survmnth NULLS LAST")$version
    else character(0L)
    DBI::dbDisconnect(con_e, shutdown = TRUE)

    if (length(loaded) == 0L) {
      message("No LFS versions loaded yet. Download first: ",
              "get_pumf(\"LFS\", \"2024\")")
      return(invisible(NULL))
    }
    message("Rebuilding ", length(loaded), " LFS version(s): ",
            paste(loaded, collapse = ", "))
    for (v in loaded) {
      tryCatch({
        tbl_v <- lfs_get_pumf(v, lang = lang, cache_path = cache_path,
                               refresh    = refresh,
                               redownload = redownload,
                               read_only  = FALSE)
        if (!is.null(tbl_v) && DBI::dbIsValid(tbl_v$src$con))
          DBI::dbDisconnect(tbl_v$src$con, shutdown = TRUE)
      }, error = function(e)
        warning("Failed to rebuild LFS ", v, ": ", conditionMessage(e), call. = FALSE))
    }
    return(.lfs_open_tbl(db_path_e, data_tbl_e, read_only = read_only))
  }

  # redownload implies a full rebuild
  eff_refresh <- identical(refresh, TRUE) || isTRUE(redownload)

  lfs_dir   <- file.path(cache_path, "LFS")
  db_path   <- file.path(lfs_dir, "LFS.duckdb")
  data_tbl  <- paste0("lfs_", lang)
  label_col <- if (lang == "eng") "label_en" else "label_fr"

  dir.create(lfs_dir, showWarnings = FALSE, recursive = TRUE)

  # refresh = "auto": download everything missing (takes priority over version=NULL)
  if (identical(refresh, "auto"))
    return(.lfs_auto_refresh(db_path, data_tbl, label_col, lang, cache_path,
                              read_only = read_only))

  # version = NULL: just report status
  if (is.null(version))
    return(.lfs_status(db_path, data_tbl, lang, read_only = read_only))

  vtype    <- .lfs_version_type(version)
  survyear <- .lfs_survyear(version)
  survmnth <- .lfs_survmnth(version)

  # --- check existing data via a read-only connection ---
  if (file.exists(db_path)) {
    con_chk <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path,
                               read_only = TRUE)
    .lfs_ensure_versions_table(con_chk)

    if (!eff_refresh) {
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

  # Verify the DuckDB is not locked before doing any download / parse work.
  .assert_duckdb_writable(db_path)

  # --- Stage 1: locate / download ---
  version_dir <- pumf_locate_or_download("LFS", version,
                                          cache_path = cache_path,
                                          refresh    = eff_refresh,
                                          redownload = isTRUE(redownload))

  # --- Stage 2: parse metadata ---
  pumf_parse_metadata(version_dir, refresh = eff_refresh)

  # --- Stage 3 (LFS variant): build labeled data and append ---
  message("Labeling LFS ", version, " [", lang, "] ...")
  data <- .lfs_build_version(version_dir, label_col)
  n    <- nrow(data)

  # Write phase: open RW, append, close
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  .lfs_ensure_versions_table(con)

  if (eff_refresh) {
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
