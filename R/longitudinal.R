# R/longitudinal.R — Shared engine for longitudinal PUMF series.
#
# A longitudinal series is released as a sequence of time slices (months or
# years) with the same, or nearly the same, variables and codes.  All loaded
# slices are appended to one DuckDB per series, so they can be queried as a
# single table:
#
#   <cache_path>/<series>/<db_file>
#     <table_prefix>_eng, <table_prefix>_fra   labelled rows of every slice
#     <versions_table>                          what has been loaded
#
# Slices are addressed as "YYYY" (a year) or "YYYY-MM" (a month) and the data
# tables carry integer SURVYEAR / SURVMNTH columns.  Schema drift between slices
# is absorbed by .long_append() (new columns are added, ENUM levels extended).
#
# Each series is described by a spec (a list) returned by
# .pumf_longitudinal_spec(series):
#
#   series          series acronym, also the cache subdirectory
#   entry           the shared registry entry (.pumf_longitudinal_entry());
#                   its data_fixups$force_integer are the integer columns
#   db_file         DuckDB file name inside <cache_path>/<series>/
#                   (default <series>.duckdb)
#   table_prefix    data tables are <table_prefix>_<lang>
#                   (default tolower(series))
#   versions_table  name of the tracking table (default <table_prefix>_versions)
#   annual_files    TRUE when a "YYYY" version is one annual release that
#                   supersedes that year's monthly loads (LFS); FALSE when a
#                   year is simply its twelve months, loaded one by one
#   example         version used in user-facing hints
#   validate(v)     stop() unless v is a valid version; returns "annual" or
#                   "monthly"
#   available()     character vector of versions refresh = "auto" loads
#   prepare(version, cache_path, refresh, redownload)
#                   Stages 1 + 2: fetch the raw files and write metadata/;
#                   returns the version directory
#   build(version_dir, label_col, version)
#                   the labelled data frame for one version
#   finalize(tbl)   optional: the lazy tbl handed back to the user, e.g. to
#                   reorder columns (the stored table is not touched)
#   variables(cache_path, versions)
#                   variables table used by label_pumf_columns(), given the
#                   loaded versions (oldest first)
#   codes(cache_path, versions)
#                   code labels of every loaded version (pumf_dictionary())
#   timeline        optional: how get_lfs_timeline() maps the series (see
#                   R/lfs_timeline.R)
#
# LFS (R/lfs_pipeline.R) and LFS_HIST (R/lfs_hist.R) are the two instances.


# ---- Spec registry ----------------------------------------------------------

# One spec constructor per series; the set of longitudinal series is the names
# of this vector.  Constructors are named, not referenced, so this file does
# not depend on collation order.
.pumf_longitudinal_spec_fns <- c(LFS = ".lfs_spec", LFS_HIST = ".lfs_hist_spec")
.pumf_longitudinal_series   <- names(.pumf_longitudinal_spec_fns)

# TRUE when `series` is handled by the longitudinal engine.
.is_longitudinal <- function(series) {
  length(series) == 1L && !is.na(series) && series %in% .pumf_longitudinal_series
}

.pumf_longitudinal_spec <- function(series) {
  if (!.is_longitudinal(series))
    stop("'", series, "' is not a longitudinal series.", call. = FALSE)
  match.fun(.pumf_longitudinal_spec_fns[[series]])()
}

# Shipped reference CSVs (inst/extdata/<dir>/<which>.csv: the LFS_HIST
# canonical dictionary, the LFS timeline harmonisation tables), read once per
# session as all-character tibbles.
.pumf_extdata_cache <- new.env(parent = emptyenv())

.pumf_extdata_csv <- function(dir, which) {
  key <- paste(dir, which, sep = "/")
  if (is.null(.pumf_extdata_cache[[key]])) {
    path <- system.file("extdata", dir, paste0(which, ".csv"), package = "canpumf")
    if (!nzchar(path))
      stop("Reference file '", key, ".csv' is missing from the installed ",
           "package.", call. = FALSE)
    .pumf_extdata_cache[[key]] <- readr::read_csv(
      path, col_types = readr::cols(.default = "c"), na = "",
      locale = readr::locale(encoding = "UTF-8"), progress = FALSE)
  }
  .pumf_extdata_cache[[key]]
}

# Names a spec leaves to the defaults.
.long_db_path <- function(spec, cache_path)
  file.path(cache_path, spec$series,
            spec$db_file %||% paste0(spec$series, ".duckdb"))

.long_table_prefix <- function(spec) spec$table_prefix %||% tolower(spec$series)

.long_table_name <- function(spec, lang) paste0(.long_table_prefix(spec), "_", lang)

.long_versions_table <- function(spec)
  spec$versions_table %||% paste0(.long_table_prefix(spec), "_versions")

# The columns kept as INTEGER (SURVYEAR, SURVMNTH, REC_NUM), from the shared
# registry entry.
.long_int_cols <- function(spec) spec$entry$data_fixups$force_integer

# The tbl handed back to the user, after the spec's optional finalize hook.
.long_finalize <- function(spec, t)
  if (is.function(spec$finalize)) spec$finalize(t) else t


# ---- Connections ------------------------------------------------------------

# Run fn(con) on a read-only connection to the series' database and release
# it.  A probe, so the instance is not shut down (see .duckdb_connect()): a tbl
# the user holds on the same file is unaffected, and a later read-write open
# still succeeds once the last connection is gone.  Returns `default` when the
# file does not exist or (unless `strict`) cannot be opened.
.long_with_readonly_con <- function(spec, cache_path, fn, default = NULL,
                                    strict = FALSE) {
  db_path <- .long_db_path(spec, cache_path)
  if (!file.exists(db_path)) return(default)
  con <- if (strict) .duckdb_connect_quiet(db_path, read_only = TRUE)
         else tryCatch(.duckdb_connect_quiet(db_path, read_only = TRUE),
                       error = function(e) NULL)
  if (is.null(con)) return(default)
  on.exit(DBI::dbDisconnect(con, shutdown = FALSE), add = TRUE)
  fn(con)
}

# Open the data table of a series as a lazy tbl, filtered to a year (and
# month) when `survyear` is given.  This is the connection handed back to the
# caller, so it goes through .duckdb_connect() (Connections pane).
.long_open_tbl <- function(spec, db_path, data_tbl, survyear = NULL,
                           survmnth = NA_integer_, read_only = TRUE) {
  con <- .duckdb_connect(db_path, read_only = read_only)
  if (!DBI::dbExistsTable(con, data_tbl)) {
    DBI::dbDisconnect(con, shutdown = TRUE)
    stop("Table '", data_tbl, "' does not exist in ", db_path,
         ". No ", spec$series, " data has been loaded yet.")
  }
  t <- tbl(con, data_tbl)
  if (!is.null(survyear)) {
    t <- filter(t, .data$SURVYEAR == survyear)
    if (!is.na(survmnth))
      t <- filter(t, .data$SURVMNTH == survmnth)
  }
  .long_finalize(spec, t)
}

# Close the connection of a tbl returned by a nested load.
.long_close_tbl <- function(t) {
  if (!is.null(t) && DBI::dbIsValid(t$src$con))
    DBI::dbDisconnect(t$src$con, shutdown = TRUE)
  invisible(NULL)
}


# ---- Tracking table ---------------------------------------------------------
#
# <versions_table>(version, type, survyear, survmnth, downloaded_at, n_records)
# records every loaded slice once (not per language).  The readers below
# return "nothing" when the table does not exist, so they are safe on a
# read-only connection; the write phase creates it.

.long_ensure_versions_table <- function(con, spec) {
  vt <- .long_versions_table(spec)
  if (!DBI::dbExistsTable(con, vt))
    DBI::dbExecute(con, paste0(
      "CREATE TABLE ", .qid(con, vt), " (
        version        VARCHAR,
        type           VARCHAR,
        survyear       INTEGER,
        survmnth       INTEGER,
        downloaded_at  TIMESTAMP DEFAULT NOW(),
        n_records      INTEGER
      )"))
}

# The tracking table, oldest slice first (every column; an empty frame with
# `version` and `type` when it does not exist).  `db` qualifies the table in
# an ATTACHed database.
.long_read_versions <- function(con, spec, db = NULL) {
  vt <- .long_versions_table(spec)
  if (!.duckdb_table_exists_in(con, vt, db))
    return(data.frame(version = character(0L), type = character(0L)))
  qt <- if (is.null(db)) .qid(con, vt) else paste0(.qid(con, db), ".", .qid(con, vt))
  DBI::dbGetQuery(con, paste0(
    "SELECT * FROM ", qt, " ORDER BY survyear, survmnth NULLS LAST"))
}

# TRUE when `table` exists in the connection's current database or, with
# `db`, in the database attached under that alias (get_lfs_timeline()).
.duckdb_table_exists_in <- function(con, table, db = NULL) {
  if (is.null(db)) return(DBI::dbExistsTable(con, table))
  DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM duckdb_tables()
                        WHERE database_name = ? AND table_name = ?",
                  params = list(db, table))$n > 0L
}

# Versions recorded in a series' tracking table, oldest first (none when the
# database is missing or locked).
.long_loaded_versions <- function(spec, cache_path) {
  .long_with_readonly_con(spec, cache_path, default = character(0L),
                          function(con) .long_read_versions(con, spec)$version)
}

.long_version_exists <- function(con, spec, version) {
  vt <- .long_versions_table(spec)
  if (!DBI::dbExistsTable(con, vt)) return(FALSE)
  DBI::dbGetQuery(con, paste0(
    "SELECT COUNT(*) AS n FROM ", .qid(con, vt), " WHERE version = ?"),
    params = list(version))$n > 0L
}

# TRUE when an annual slice is recorded for the calendar year.
.long_has_annual <- function(con, spec, survyear) {
  vt <- .long_versions_table(spec)
  if (!DBI::dbExistsTable(con, vt)) return(FALSE)
  DBI::dbGetQuery(con, paste0(
    "SELECT COUNT(*) AS n FROM ", .qid(con, vt),
    " WHERE survyear = ? AND type = 'annual'"),
    params = list(survyear))$n > 0L
}

# Monthly versions recorded for a year.
.long_monthly_versions <- function(con, spec, survyear) {
  vt <- .long_versions_table(spec)
  if (!DBI::dbExistsTable(con, vt)) return(character(0L))
  DBI::dbGetQuery(con, paste0(
    "SELECT version FROM ", .qid(con, vt),
    " WHERE survyear = ? AND type = 'monthly'"),
    params = list(survyear))$version
}

# TRUE when data_tbl has any rows for (survyear [, survmnth]).
.long_data_exists <- function(con, data_tbl, survyear, survmnth = NA_integer_) {
  if (!DBI::dbExistsTable(con, data_tbl)) return(FALSE)
  DBI::dbGetQuery(con, paste0(
    "SELECT COUNT(*) AS n FROM ", .qid(con, data_tbl),
    " WHERE SURVYEAR = ?", if (!is.na(survmnth)) " AND SURVMNTH = ?"),
    params = c(list(survyear), if (!is.na(survmnth)) list(survmnth)))$n > 0L
}

# TRUE when `version` is recorded and its rows are in `data_tbl`: the tracking
# table distinguishes "annual loaded" from "some months of the year exist", the
# data table tells whether this language has it.
.long_is_loaded <- function(con, spec, version, data_tbl) {
  .long_version_exists(con, spec, version) &&
    .long_data_exists(con, data_tbl, .lfs_survyear(version), .lfs_survmnth(version))
}

.long_record_version <- function(con, spec, version, vtype, survyear, survmnth, n) {
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", .qid(con, .long_versions_table(spec)),
    " (version, type, survyear, survmnth, downloaded_at, n_records)
     VALUES (?, ?, ?, ?, NOW(), ?)"),
    params = list(version, vtype, survyear, survmnth, n))
}

# Remove one slice: the rows of a year ("YYYY") or a month ("YYYY-MM") from
# each of `data_tbls` that exists and, with `record`, its tracking rows (a
# year removes every record of that year, or only those of `type_filter`).
.long_delete_slice <- function(con, spec, version, data_tbls, record = TRUE,
                               type_filter = NULL) {
  survyear <- .lfs_survyear(version)
  survmnth <- .lfs_survmnth(version)
  for (t in data_tbls) {
    if (!DBI::dbExistsTable(con, t)) next
    DBI::dbExecute(con, paste0(
      "DELETE FROM ", .qid(con, t), " WHERE SURVYEAR = ?",
      if (!is.na(survmnth)) " AND SURVMNTH = ?"),
      params = c(list(survyear), if (!is.na(survmnth)) list(survmnth)))
  }
  vt <- .long_versions_table(spec)
  if (!record || !DBI::dbExistsTable(con, vt)) return(invisible(NULL))
  if (is.na(survmnth))
    DBI::dbExecute(con, paste0(
      "DELETE FROM ", .qid(con, vt), " WHERE survyear = ?",
      if (!is.null(type_filter)) " AND type = ?"),
      params = c(list(survyear), if (!is.null(type_filter)) list(type_filter)))
  else
    DBI::dbExecute(con, paste0(
      "DELETE FROM ", .qid(con, vt), " WHERE version = ?"),
      params = list(version))
  invisible(NULL)
}


# ---- Append with schema evolution -------------------------------------------

# Column types of a table from the catalogue (no data scanned), as DuckDB
# prints them ("ENUM('a', 'b')" for an inline ENUM).  `db` is an ATTACHed
# database; NULL is the connection's own.
.duckdb_column_types <- function(con, table, db = NULL) {
  DBI::dbGetQuery(con,
    "SELECT column_name, data_type FROM duckdb_columns()
     WHERE table_name = ? AND database_name = COALESCE(?, current_database())
     ORDER BY column_index",
    params = list(table, db %||% NA_character_))
}

# Levels of a DuckDB ENUM column, or NULL for other types.
.duckdb_enum_levels <- function(con, table, col, db = NULL) {
  tps <- .duckdb_column_types(con, table, db)
  tp  <- tps$data_type[tps$column_name == col]
  if (length(tp) != 1L || !startsWith(tp, "ENUM(")) return(NULL)
  DBI::dbGetQuery(con, sprintf("SELECT unnest(enum_range(NULL::%s)) AS l", tp))$l
}

.duckdb_enum_sql <- function(con, levels)
  paste0("ENUM(", paste(.qstr(con, levels), collapse = ", "), ")")

# Append new_data to table_name, extending the schema when columns differ.
#   New columns in new_data → ALTER TABLE ADD COLUMN (NULL for old rows).
#   Columns in table missing from new_data → NA column before append.
#   Factor columns → stored as ENUM; types evolved on each append as needed.
.long_append <- function(con, table_name, new_data) {
  factor_cols   <- names(new_data)[vapply(new_data, is.factor, logical(1L))]
  factor_levels <- stats::setNames(
    lapply(factor_cols, function(c) levels(new_data[[c]])),
    factor_cols)
  qt <- .qid(con, table_name)
  varchar_types <- c("VARCHAR", "TEXT", "CHAR", "CHARACTER VARYING")
  rebuild_hint <- "\nUse redownload = TRUE for a full clean rebuild."
  alter <- function(col, type, ok_msg, fail_msg, hint = "") {
    tryCatch({
      DBI::dbExecute(con, paste0("ALTER TABLE ", qt, " ALTER COLUMN ",
                                 .qid(con, col), " SET DATA TYPE ", type))
      message("  ", ok_msg)
    }, error = function(e)
      warning(fail_msg, ": ", conditionMessage(e), hint, call. = FALSE))
  }

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
    sql_type <- if (is.factor(r_val)) .duckdb_enum_sql(con, factor_levels[[col]])
                else if (is.integer(r_val)) "INTEGER"
                else if (is.numeric(r_val)) "DOUBLE"
                else if (is.logical(r_val)) "BOOLEAN"
                else "VARCHAR"
    DBI::dbExecute(con, paste0("ALTER TABLE ", qt, " ADD COLUMN ",
                               .qid(con, col), " ", sql_type))
    message("  Added new column '", col, "' (",
            if (is.factor(r_val)) "ENUM" else sql_type, ") to ", table_name)
  }

  types <- .duckdb_column_types(con, table_name)
  for (col in intersect(incoming, types$column_name)) {
    db_type <- types$data_type[types$column_name == col]
    r_val   <- new_data[[col]]

    # VARCHAR → numeric (e.g. FINALWT corrected after a metadata fix)
    if ((is.integer(r_val) || is.numeric(r_val)) && db_type %in% varchar_types) {
      new_sql <- if (is.integer(r_val)) "INTEGER" else "DOUBLE"
      alter(col, new_sql,
            paste0("Upgraded column '", col, "' from VARCHAR to ", new_sql,
                   " in ", table_name),
            paste0("Could not upgrade type of '", col, "' from VARCHAR to ",
                   new_sql), hint = rebuild_hint)
    }

    # DOUBLE → INTEGER (e.g. REC_NUM now forced to integer after a metadata fix)
    if (is.integer(r_val) && db_type == "DOUBLE")
      alter(col, "INTEGER",
            paste0("Upgraded column '", col, "' from DOUBLE to INTEGER in ",
                   table_name),
            paste0("Could not upgrade type of '", col, "' from DOUBLE to INTEGER"),
            hint = rebuild_hint)

    # ENUM evolution: extend inline ENUM with any new factor levels
    if (col %in% factor_cols && startsWith(db_type, "ENUM(")) {
      current_levels <- .duckdb_enum_levels(con, table_name, col)
      new_lvls       <- setdiff(factor_levels[[col]], current_levels)
      if (length(new_lvls) > 0L)
        alter(col, .duckdb_enum_sql(con, union(current_levels, factor_levels[[col]])),
              paste0("Extended ENUM for '", col, "' with: ",
                     paste(new_lvls, collapse = ", ")),
              paste0("Could not extend ENUM for '", col, "'"))
    }

    # VARCHAR → ENUM: column predates ENUM enforcement; upgrade now
    if (col %in% factor_cols && db_type %in% varchar_types) {
      # Include any existing values in the data so the cast doesn't fail
      existing_vals <- tryCatch(
        na.omit(DBI::dbGetQuery(con, paste0(
          "SELECT DISTINCT CAST(", .qid(con, col), " AS VARCHAR) AS val FROM ",
          qt))$val),
        error = function(e) character(0L))
      alter(col, .duckdb_enum_sql(con, union(existing_vals, factor_levels[[col]])),
            paste0("Upgraded column '", col, "' from VARCHAR to ENUM in ",
                   table_name),
            paste0("Could not upgrade '", col, "' to ENUM"))
    }
  }

  # ---- Append ----
  all_cols <- DBI::dbListFields(con, table_name)
  for (col in setdiff(all_cols, incoming))
    new_data[[col]] <- NA

  DBI::dbAppendTable(con, table_name, new_data[, all_cols, drop = FALSE])
  invisible(NULL)
}


# ---- Status (version = NULL) ------------------------------------------------

.long_status <- function(spec, db_path, data_tbl, lang, read_only = TRUE) {
  s <- spec$series
  if (!file.exists(db_path)) {
    message(s, " database does not exist yet. ",
            "Call get_pumf(\"", s, "\", \"", spec$example, "\") to load a version, ",
            "or get_pumf(\"", s, "\", refresh = \"auto\") to load all available versions.")
    return(invisible(NULL))
  }

  # A single connection serves both the status summary and the returned tbl.
  # It is opened with .duckdb_connect (not .duckdb_connect_quiet) because it
  # is the connection handed back to the caller, so it should appear in the
  # RStudio Connections pane.  On the early-exit paths that return no tbl,
  # `keep` stays FALSE and the connection is closed before returning.
  con  <- .duckdb_connect(db_path, read_only = read_only)
  keep <- FALSE
  on.exit(if (!keep) DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  versions_df <- .long_read_versions(con, spec)
  if (nrow(versions_df) == 0L) {
    message(s, " database exists but no versions have been loaded yet.")
    return(invisible(NULL))
  }

  annual  <- versions_df$version[versions_df$type == "annual"]
  monthly <- versions_df$version[versions_df$type == "monthly"]
  parts   <- character(0L)
  if (length(annual)  > 0L)
    parts <- c(parts, paste0(paste(annual, collapse = ", "), " (annual)"))
  if (length(monthly) > 0L)
    parts <- c(parts, paste0(.long_compress_months(monthly), " (monthly)"))

  has_lang <- DBI::dbExistsTable(con, data_tbl)
  lang_tag <- if (has_lang) paste0(" [", lang, " table present]")
              else paste0(" [no ", lang, " table yet]")

  message(s, " database contains: ", paste(parts, collapse = ", "), lang_tag)

  if (!has_lang) return(invisible(NULL))

  keep <- TRUE
  .long_finalize(spec, tbl(con, data_tbl))
}

# Monthly versions for display: a year whose twelve months are all present is
# shown as "YYYY-01..YYYY-12"; other months are listed individually.
.long_compress_months <- function(v) {
  yr  <- substr(v, 1L, 4L)
  out <- character(0L)
  for (y in unique(yr)) {
    m <- v[yr == y]
    out <- c(out, if (length(m) == 12L) paste0(y, "-01..", y, "-12") else m)
  }
  paste(out, collapse = ", ")
}


# ---- Auto refresh -----------------------------------------------------------

.long_auto_refresh <- function(spec, db_path, data_tbl, lang, cache_path,
                               read_only = TRUE) {
  available <- tryCatch(
    spec$available(),
    error = function(e) {
      warning("Could not list available ", spec$series, " versions: ",
              conditionMessage(e), call. = FALSE)
      character(0L)
    })

  to_add <- setdiff(available, .long_loaded_versions(spec, cache_path))
  if (length(to_add) == 0L) {
    message(spec$series, " database is up to date.")
  } else {
    .assert_duckdb_writable(db_path)
    message("Loading ", length(to_add), " ", spec$series, " version(s): ",
            if (length(to_add) > 12L)
              paste0(to_add[[1L]], " .. ", to_add[[length(to_add)]])
            else paste(to_add, collapse = ", "))
    .long_load_each(spec, to_add, lang, cache_path, on_error = "load")
  }

  .long_open_tbl(spec, db_path, data_tbl, read_only = read_only)
}

# Load `versions` one after the other through nested .long_get_pumf() calls,
# closing each write-phase tbl.  With `on_error` (a verb for the warning:
# "load", "rebuild") a failed version is reported and the others continue;
# without it the error propagates.
.long_load_each <- function(spec, versions, lang, cache_path, refresh = FALSE,
                            redownload = FALSE, on_error = NULL) {
  load_one <- function(v)
    .long_close_tbl(.long_get_pumf(spec, v, lang = lang, cache_path = cache_path,
                                   refresh = refresh, redownload = redownload,
                                   read_only = FALSE))
  for (v in versions) {
    if (is.null(on_error)) load_one(v)
    else tryCatch(load_one(v), error = function(e)
      warning("Failed to ", on_error, " ", spec$series, " ", v, ": ",
              conditionMessage(e), call. = FALSE))
  }
  invisible(NULL)
}


# ---- Engine -----------------------------------------------------------------

# Load (if needed) and open one version of a longitudinal series.  See
# lfs_get_pumf() for the argument semantics; `spec` selects the series.
.long_get_pumf <- function(spec,
                           version    = NULL,
                           lang       = "eng",
                           cache_path = getOption("canpumf.cache_path", tempdir()),
                           refresh    = FALSE,
                           redownload = FALSE,
                           read_only  = TRUE) {
  stopifnot(lang %in% c("eng", "fra"))
  if (!identical(refresh, FALSE) &&
      !identical(refresh, TRUE)  &&
      !identical(refresh, "auto"))
    stop("'refresh' must be FALSE, TRUE, or \"auto\".")

  s           <- spec$series
  db_path     <- .long_db_path(spec, cache_path)
  data_tbl    <- .long_table_name(spec, lang)
  label_col   <- .pumf_label_col(lang)
  eff_refresh <- identical(refresh, TRUE) || isTRUE(redownload)

  # No version + rebuild requested: rebuild every loaded version.
  if (eff_refresh && is.null(version)) {
    if (!file.exists(db_path)) {
      message("No ", s, " data in cache. Download a version first: ",
              "get_pumf(\"", s, "\", \"", spec$example, "\")")
      return(invisible(NULL))
    }
    loaded <- .long_loaded_versions(spec, cache_path)
    if (length(loaded) == 0L) {
      message("No ", s, " versions loaded yet. Download first: ",
              "get_pumf(\"", s, "\", \"", spec$example, "\")")
      return(invisible(NULL))
    }
    message("Rebuilding ", length(loaded), " ", s, " version(s): ",
            paste(loaded, collapse = ", "))
    .long_load_each(spec, loaded, lang, cache_path, refresh = refresh,
                    redownload = redownload, on_error = "rebuild")
    return(.long_open_tbl(spec, db_path, data_tbl, read_only = read_only))
  }

  dir.create(dirname(db_path), showWarnings = FALSE, recursive = TRUE)

  if (identical(refresh, "auto"))
    return(.long_auto_refresh(spec, db_path, data_tbl, lang, cache_path,
                              read_only = read_only))

  if (is.null(version))
    return(.long_status(spec, db_path, data_tbl, lang, read_only = read_only))

  vtype    <- spec$validate(version)
  survyear <- .lfs_survyear(version)
  survmnth <- .lfs_survmnth(version)

  # A year of a series without annual files is its twelve monthly loads.
  if (vtype == "annual" && !isTRUE(spec$annual_files))
    return(.long_get_year_of_months(spec, version, lang, cache_path,
                                    refresh, redownload, eff_refresh, read_only))

  # --- check existing data via a read-only probe ---
  # "annual": a monthly was requested but an annual already covers this year.
  # The monthly is not stored separately (the annual superseded it), so the
  # annual filtered to the requested month is returned.  This applies even
  # under refresh -- StatCan withdraws the monthly raw file once the annual is
  # released, and re-deriving the month would corrupt the annual's rows for
  # that month.  "loaded": the version is recorded and in this lang table.
  state <- .long_with_readonly_con(spec, cache_path, strict = TRUE, function(con) {
    if (vtype == "monthly" && .long_has_annual(con, spec, survyear)) "annual"
    else if (!eff_refresh && .long_is_loaded(con, spec, version, data_tbl)) "loaded"
    else NULL
  })
  if (identical(state, "annual"))
    message("Annual ", s, " data for ", survyear, " already loaded; ",
            version, " is covered by the annual file.")
  if (!is.null(state))
    return(.long_open_tbl(spec, db_path, data_tbl, survyear, survmnth,
                          read_only = read_only))

  # Verify the DuckDB is not locked before doing any download / parse work.
  .assert_duckdb_writable(db_path)

  # --- Stages 1 + 2 ---
  version_dir <- spec$prepare(version, cache_path = cache_path,
                              refresh = eff_refresh,
                              redownload = isTRUE(redownload))

  # --- Stage 3: build labelled data and append ---
  message("Labeling ", s, " ", version, " [", lang, "] ...")
  data <- spec$build(version_dir, label_col, version)
  n    <- nrow(data)

  # Write phase: open RW, append, close
  con <- .duckdb_connect_quiet(db_path)
  .long_ensure_versions_table(con, spec)

  if (eff_refresh) {
    # An annual covers the whole year: drop every row and version record for
    # the year (any monthlies plus a prior annual) before re-appending.  A
    # monthly replaces only its own month's data and version record.
    .long_delete_slice(con, spec, version, data_tbl)
  } else if (vtype == "annual") {
    monthlies <- .long_monthly_versions(con, spec, survyear)
    if (length(monthlies) > 0L) {
      message("Annual ", s, " ", version, " supersedes monthly: ",
              paste(monthlies, collapse = ", "))
      .long_delete_slice(con, spec, version, data_tbl, type_filter = "monthly")
    }
  } else if (.long_data_exists(con, data_tbl, survyear, survmnth)) {
    # A month re-loaded into a language table that already has it (the
    # tracking row was lost): replace rather than duplicate.
    .long_delete_slice(con, spec, version, data_tbl, record = FALSE)
  }

  .long_append(con, data_tbl, data)

  # Record once per version, not per lang
  if (!.long_version_exists(con, spec, version))
    .long_record_version(con, spec, version, vtype, survyear, survmnth, n)

  DBI::dbDisconnect(con, shutdown = TRUE)
  message(s, " ", version, ": ", n, " rows appended to ", data_tbl, ".")

  .long_open_tbl(spec, db_path, data_tbl, survyear, survmnth,
                 read_only = read_only)
}

# "YYYY" for a series without annual files: load the missing months of the
# year one by one, then return the year.
.long_get_year_of_months <- function(spec, version, lang, cache_path, refresh,
                                     redownload, eff_refresh, read_only) {
  db_path  <- .long_db_path(spec, cache_path)
  data_tbl <- .long_table_name(spec, lang)
  survyear <- .lfs_survyear(version)
  months   <- intersect(sprintf("%s-%02d", version, 1:12), spec$available())

  todo <- months
  if (!eff_refresh) {
    have <- .long_with_readonly_con(spec, cache_path, strict = TRUE,
                                    default = rep(FALSE, length(months)),
                                    function(con)
      vapply(months, function(m) .long_is_loaded(con, spec, m, data_tbl),
             logical(1L)))
    todo <- months[!have]
  }

  if (length(todo) > 0L) {
    .assert_duckdb_writable(db_path)
    message("Loading ", spec$series, " ", version, ": ", length(todo),
            " month(s)")
    .long_load_each(spec, todo, lang, cache_path, refresh = refresh,
                    redownload = redownload)
  }
  .long_open_tbl(spec, db_path, data_tbl, survyear, read_only = read_only)
}


# ---- Stage 3: shared labelling ----------------------------------------------

# Turn the all-character frame `data` of one slice into the labelled frame
# that is appended to the series table, using the slice's `meta`
# (`read_metadata()` list).  `int_cols` (the registry's force_integer fixup:
# SURVYEAR, SURVMNTH, REC_NUM) are parsed as numbers but kept INTEGER and
# never labelled, so make_date() and SQL year/month filters work on integers.
# `missing_codes` is passed through to .apply_numeric_conversion().  `where`
# names the data location in the error for a slice lacking the survey
# dimensions (the version directory for LFS, the data file for LFS_HIST).
.long_label_frame <- function(data, meta, label_col, int_cols, series, where,
                              missing_codes = list()) {
  variables <- meta$variables
  variables$type[variables$name %in% int_cols] <- "numeric"
  codes <- meta$codes[!meta$codes$name %in% int_cols, ]

  missing_cols <- setdiff(c("SURVYEAR", "SURVMNTH"), names(data))
  if (length(missing_cols) > 0L)
    stop(series, " data is missing required columns: ",
         paste(missing_cols, collapse = ", "), where, call. = FALSE)

  data <- .apply_numeric_conversion(data, variables, missing_codes = missing_codes)
  # .apply_numeric_conversion yields double; restore the integer typing.
  for (col in intersect(int_cols, names(data)))
    data[[col]] <- as.integer(data[[col]])
  .apply_code_labels(data, codes, label_col)
}
