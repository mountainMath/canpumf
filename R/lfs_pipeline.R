# R/lfs_pipeline.R — LFS longitudinal database pipeline.
#
# The engine shared with other longitudinal series lives in R/longitudinal.R;
# this file holds the LFS spec, the LFS data-file handling, and the
# versions-table / append helpers the engine uses (the tracking table name is
# their `vt` argument, "lfs_versions" for LFS).
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
.lfs_ensure_versions_table <- function(con, vt = "lfs_versions") {
  if (!DBI::dbExistsTable(con, vt))
    DBI::dbExecute(con, paste0("
      CREATE TABLE ", vt, " (
        version        VARCHAR,
        type           VARCHAR,
        survyear       INTEGER,
        survmnth       INTEGER,
        downloaded_at  TIMESTAMP DEFAULT NOW(),
        n_records      INTEGER
      )"))
}

# TRUE when lfs_versions contains an entry for this version string.
.lfs_version_exists <- function(con, version, vt = "lfs_versions") {
  .lfs_ensure_versions_table(con, vt)
  n <- DBI::dbGetQuery(
    con,
    sprintf("SELECT COUNT(*) AS n FROM %s WHERE version = '%s'", vt, version))$n
  n > 0L
}

# TRUE when lfs_versions contains an annual entry for this calendar year.
.lfs_has_annual <- function(con, survyear, vt = "lfs_versions") {
  .lfs_ensure_versions_table(con, vt)
  n <- DBI::dbGetQuery(
    con,
    sprintf("SELECT COUNT(*) AS n FROM %s
             WHERE survyear = %d AND type = 'annual'", vt, survyear))$n
  n > 0L
}

# Monthly versions recorded in lfs_versions for a given year.
.lfs_monthly_versions <- function(con, survyear, vt = "lfs_versions") {
  .lfs_ensure_versions_table(con, vt)
  DBI::dbGetQuery(
    con,
    sprintf("SELECT version FROM %s
             WHERE survyear = %d AND type = 'monthly'", vt, survyear))$version
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

  # SURVYEAR/SURVMNTH/REC_NUM are kept as integer (cast explicitly after numeric
  # conversion below): SURVYEAR/SURVMNTH so make_date() and SQL year/month
  # filters operate on integers; REC_NUM is a whole-number record counter.  The
  # column list comes from the shared LFS registry entry's force_integer fixup.
  # Mark them numeric so .apply_numeric_conversion parses them — it now always
  # yields double, so the integer typing is restored by the cast further down.
  lfs_int_cols <- pumf_registry_lookup("LFS", NA_character_)$data_fixups$force_integer
  variables$type[variables$name %in% lfs_int_cols] <- "numeric"

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
  # .apply_numeric_conversion yields double; restore integer typing for the
  # survey-dimension/record columns so make_date() and integer filters work.
  for (col in intersect(lfs_int_cols, names(data)))
    data[[col]] <- as.integer(data[[col]])
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
.lfs_delete_year <- function(con, data_tbl, survyear, type_filter = NULL,
                             vt = "lfs_versions") {
  if (DBI::dbExistsTable(con, data_tbl))
    DBI::dbExecute(
      con,
      sprintf('DELETE FROM "%s" WHERE SURVYEAR = %d', data_tbl, survyear))
  sql <- sprintf("DELETE FROM %s WHERE survyear = %d", vt, survyear)
  if (!is.null(type_filter))
    sql <- paste0(sql, sprintf(" AND type = '%s'", type_filter))
  DBI::dbExecute(con, sql)
}

# Delete data for a single month and remove only that version's lfs_versions
# row.  Used when refreshing one monthly version so sibling months (and any
# annual) loaded for the same year are left untouched.
.lfs_delete_month <- function(con, data_tbl, version, survyear, survmnth,
                              vt = "lfs_versions") {
  if (DBI::dbExistsTable(con, data_tbl))
    DBI::dbExecute(
      con,
      sprintf('DELETE FROM "%s" WHERE SURVYEAR = %d AND SURVMNTH = %d',
              data_tbl, survyear, survmnth))
  DBI::dbExecute(
    con,
    sprintf("DELETE FROM %s WHERE version = '%s'", vt, version))
}

# Record a version in lfs_versions.
.lfs_record_version <- function(con, version, vtype, survyear, survmnth, n,
                                vt = "lfs_versions") {
  survmnth_sql <- if (is.na(survmnth)) "NULL" else as.character(survmnth)
  DBI::dbExecute(
    con,
    sprintf(
      "INSERT INTO %s (version,type,survyear,survmnth,downloaded_at,n_records)
       VALUES ('%s','%s',%d,%s,NOW(),%d)",
      vt, version, vtype, survyear, survmnth_sql, n))
}




# ---- Longitudinal spec ------------------------------------------------------

# LFS as a longitudinal series (R/longitudinal.R): 2006 onward in the current
# PUMF layout, annual files superseding the monthly ones.
.lfs_spec <- function() {
  list(
    series         = "LFS",
    db_file        = "LFS.duckdb",
    table_prefix   = "lfs",
    versions_table = "lfs_versions",
    annual_files   = TRUE,
    example        = "2024",
    validate       = .lfs_version_type,
    available      = function() list_available_lfs_pumf_versions()$version,
    prepare        = function(version, cache_path, refresh, redownload) {
      version_dir <- pumf_locate_or_download("LFS", version,
                                             cache_path = cache_path,
                                             refresh    = refresh,
                                             redownload = redownload)
      pumf_parse_metadata(version_dir, refresh = refresh)
      version_dir
    },
    build          = function(version_dir, label_col, version)
      .lfs_build_version(version_dir, label_col),
    variables      = .lfs_merged_variables)
}

# Variable labels across every loaded LFS version, most recent winning: the
# shared table is the union of all versions' columns, and variables such as
# GENDER (~2020) are absent from the older versions' variables.csv.
# `versions` are the loaded versions, oldest first.
.lfs_merged_variables <- function(cache_path, versions) {
  all_vars <- lapply(versions, function(v) {
    md <- file.path(cache_path, "LFS", v, "metadata")
    if (!dir.exists(md)) return(NULL)
    tryCatch(read_metadata(md)$variables, error = function(e) NULL)
  })
  all_vars <- do.call(rbind, all_vars[!vapply(all_vars, is.null, logical(1L))])
  if (is.null(all_vars) || nrow(all_vars) == 0L)
    stop("No LFS metadata found in any version directory.", call. = FALSE)
  all_vars[!duplicated(all_vars$name, fromLast = TRUE), , drop = FALSE]
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
  .long_get_pumf(.lfs_spec(), version = version, lang = lang,
                 cache_path = cache_path, refresh = refresh,
                 redownload = redownload, read_only = read_only)
}
