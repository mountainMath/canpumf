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
# is absorbed by .lfs_append() (new columns are added, ENUM levels extended).
#
# Each series is described by a spec (a list) returned by
# .pumf_longitudinal_spec(series):
#
#   series          series acronym, also the cache subdirectory
#   db_file         DuckDB file name inside <cache_path>/<series>/
#   table_prefix    data tables are <table_prefix>_<lang>
#   versions_table  name of the tracking table
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
#   variables(cache_path, versions)
#                   variables table used by label_pumf_columns(), given the
#                   loaded versions (oldest first)
#
# LFS (R/lfs_pipeline.R) and LFS_HIST (R/lfs_hist.R) are the two instances.


# ---- Spec registry ----------------------------------------------------------

.pumf_longitudinal_specs <- function() {
  list(LFS = .lfs_spec(), LFS_HIST = .lfs_hist_spec())
}

.pumf_longitudinal_series <- c("LFS", "LFS_HIST")

# TRUE when `series` is handled by the longitudinal engine.
.is_longitudinal <- function(series) {
  length(series) == 1L && !is.na(series) && series %in% .pumf_longitudinal_series
}

.pumf_longitudinal_spec <- function(series) {
  if (!.is_longitudinal(series))
    stop("'", series, "' is not a longitudinal series.", call. = FALSE)
  .pumf_longitudinal_specs()[[series]]
}

.long_db_path <- function(spec, cache_path)
  file.path(cache_path, spec$series, spec$db_file)

.long_table_name <- function(spec, lang) paste0(spec$table_prefix, "_", lang)

# Versions recorded in a series' tracking table, oldest first ("" when none).
# Read-only and lock-friendly.
.long_loaded_versions <- function(spec, cache_path) {
  db_path <- .long_db_path(spec, cache_path)
  if (!file.exists(db_path)) return(character(0L))
  con <- tryCatch(.duckdb_connect_quiet(db_path, read_only = TRUE),
                  error = function(e) NULL)
  if (is.null(con)) return(character(0L))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  if (!DBI::dbExistsTable(con, spec$versions_table)) return(character(0L))
  DBI::dbGetQuery(con, sprintf(
    "SELECT version FROM %s ORDER BY survyear, survmnth NULLS LAST",
    spec$versions_table))$version
}

# Close the connection of a tbl returned by a nested load.
.long_close_tbl <- function(t) {
  if (!is.null(t) && DBI::dbIsValid(t$src$con))
    DBI::dbDisconnect(t$src$con, shutdown = TRUE)
  invisible(NULL)
}


# ---- Status (version = NULL) ------------------------------------------------

.long_status <- function(spec, db_path, data_tbl, lang, read_only = TRUE) {
  s  <- spec$series
  vt <- spec$versions_table
  if (!file.exists(db_path)) {
    message(s, " database does not exist yet. ",
            "Call get_pumf(\"", s, "\", \"", spec$example, "\") to load a version, ",
            "or get_pumf(\"", s, "\", refresh = \"auto\") to load all available versions.")
    return(invisible(NULL))
  }

  # A single connection serves both the status summary and the returned tbl.
  # It is opened with a plain dbConnect (not .duckdb_connect_quiet) because it
  # is the connection handed back to the caller, so it should appear in the
  # RStudio Connections pane.  On the early-exit paths that return no tbl,
  # `keep` stays FALSE and the connection is closed before returning.
  con  <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = read_only)
  keep <- FALSE
  on.exit(if (!keep) DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  versions_df <- if (DBI::dbExistsTable(con, vt))
    DBI::dbGetQuery(con, sprintf(
      "SELECT version, type, survyear, survmnth FROM %s ORDER BY survyear, survmnth", vt))
  else data.frame(version = character(0L), type = character(0L))

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
  tbl(con, data_tbl)
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
    for (v in to_add) {
      tryCatch(
        .long_close_tbl(.long_get_pumf(spec, v, lang = lang,
                                       cache_path = cache_path,
                                       read_only = FALSE)),
        error = function(e)
          warning("Failed to load ", spec$series, " ", v, ": ",
                  conditionMessage(e), call. = FALSE))
    }
  }

  .lfs_open_tbl(db_path, data_tbl, read_only = read_only)
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

  s         <- spec$series
  vt        <- spec$versions_table
  db_path   <- .long_db_path(spec, cache_path)
  data_tbl  <- .long_table_name(spec, lang)
  label_col <- if (lang == "eng") "label_en" else "label_fr"

  # No version + rebuild requested: rebuild every loaded version.
  if ((identical(refresh, TRUE) || isTRUE(redownload)) && is.null(version)) {
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
    for (v in loaded) {
      tryCatch(
        .long_close_tbl(.long_get_pumf(spec, v, lang = lang,
                                       cache_path = cache_path,
                                       refresh    = refresh,
                                       redownload = redownload,
                                       read_only  = FALSE)),
        error = function(e)
          warning("Failed to rebuild ", s, " ", v, ": ", conditionMessage(e),
                  call. = FALSE))
    }
    return(.lfs_open_tbl(db_path, data_tbl, read_only = read_only))
  }

  eff_refresh <- identical(refresh, TRUE) || isTRUE(redownload)

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
                                    refresh, redownload, read_only))

  # --- check existing data via a read-only connection ---
  if (file.exists(db_path)) {
    con_chk <- .duckdb_connect_quiet(db_path, read_only = TRUE)
    has_vt  <- DBI::dbExistsTable(con_chk, vt)

    # Monthly requested but an annual already covers this year: the monthly is
    # not stored separately (the annual superseded it), so return the annual
    # filtered to the requested month.  This applies even under refresh --
    # StatCan withdraws the monthly raw file once the annual is released, and
    # re-deriving the month would corrupt the annual's rows for that month.
    if (has_vt && vtype == "monthly" && .lfs_has_annual(con_chk, survyear, vt)) {
      message("Annual ", s, " data for ", survyear, " already loaded; ",
              version, " is covered by the annual file.")
      DBI::dbDisconnect(con_chk, shutdown = TRUE)
      return(.lfs_open_tbl(db_path, data_tbl, survyear, survmnth,
                           read_only = read_only))
    }

    if (!eff_refresh && has_vt) {
      # Loaded for this version (per the tracking table, which distinguishes
      # "annual loaded" from "some months of the year exist") and in the lang table.
      already_downloaded <- .lfs_version_exists(con_chk, version, vt)
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
  .lfs_ensure_versions_table(con, vt)

  if (eff_refresh) {
    if (vtype == "annual") {
      # An annual covers the whole year: drop every row and version record for
      # the year (any monthlies plus a prior annual) before re-appending.
      .lfs_delete_year(con, data_tbl, survyear, vt = vt)
    } else {
      # Monthly: replace only this month's data and version record.
      .lfs_delete_month(con, data_tbl, version, survyear, survmnth, vt = vt)
    }
  } else if (vtype == "annual") {
    monthlies <- .lfs_monthly_versions(con, survyear, vt)
    if (length(monthlies) > 0L) {
      message("Annual ", s, " ", version, " supersedes monthly: ",
              paste(monthlies, collapse = ", "))
      .lfs_delete_year(con, data_tbl, survyear, type_filter = "monthly", vt = vt)
    }
  } else if (.lfs_data_exists(con, data_tbl, survyear, survmnth)) {
    # A month re-loaded into a language table that already has it (the
    # tracking row was lost): replace rather than duplicate.
    DBI::dbExecute(con, sprintf(
      'DELETE FROM "%s" WHERE SURVYEAR = %d AND SURVMNTH = %d',
      data_tbl, survyear, survmnth))
  }

  .lfs_append(con, data_tbl, data)

  # Record once per version, not per lang
  if (!.lfs_version_exists(con, version, vt))
    .lfs_record_version(con, version, vtype, survyear, survmnth, n, vt = vt)

  DBI::dbDisconnect(con, shutdown = TRUE)
  message(s, " ", version, ": ", n, " rows appended to ", data_tbl, ".")

  .lfs_open_tbl(db_path, data_tbl, survyear, survmnth, read_only = read_only)
}

# "YYYY" for a series without annual files: load the missing months of the
# year one by one, then return the year.
.long_get_year_of_months <- function(spec, version, lang, cache_path, refresh,
                                     redownload, read_only) {
  db_path  <- .long_db_path(spec, cache_path)
  data_tbl <- .long_table_name(spec, lang)
  survyear <- .lfs_survyear(version)
  months   <- intersect(sprintf("%s-%02d", version, 1:12), spec$available())
  eff_refresh <- identical(refresh, TRUE) || isTRUE(redownload)

  todo <- months
  if (!eff_refresh && file.exists(db_path)) {
    con <- .duckdb_connect_quiet(db_path, read_only = TRUE)
    have <- if (DBI::dbExistsTable(con, spec$versions_table)) {
      loaded <- vapply(months, function(m) .lfs_version_exists(con, m, spec$versions_table),
                       logical(1L))
      in_lang <- vapply(months, function(m)
        .lfs_data_exists(con, data_tbl, survyear, .lfs_survmnth(m)), logical(1L))
      loaded & in_lang
    } else rep(FALSE, length(months))
    DBI::dbDisconnect(con, shutdown = TRUE)
    todo <- months[!have]
  }

  if (length(todo) > 0L) {
    .assert_duckdb_writable(db_path)
    message("Loading ", spec$series, " ", version, ": ", length(todo),
            " month(s)")
    for (m in todo)
      .long_close_tbl(.long_get_pumf(spec, m, lang = lang,
                                     cache_path = cache_path,
                                     refresh = refresh, redownload = redownload,
                                     read_only = FALSE))
  }
  .lfs_open_tbl(db_path, data_tbl, survyear, read_only = read_only)
}
