# R/cache_mgmt.R — Inspect and manage the local PUMF cache.


# ---- Internal helpers -------------------------------------------------------

.empty_cache_tibble <- function() {
  tibble::tibble(
    series       = character(),
    version      = character(),
    has_raw      = logical(),
    has_metadata = logical(),
    has_duckdb   = logical(),
    raw_mb       = double(),
    duckdb_mb    = double(),
    built_with   = character()
  )
}

# The canpumf version(s) that built the tables of one DuckDB file, read from
# its `pumf_build_info` stamp: one string ("0.6.1", or "0.6.1, 0.6.2" when
# tables were built by different versions), or NA when the file has no stamp
# (built before 0.6.1) or cannot be opened (e.g. locked by a writer).  The
# connection is read-only and released without shutting the instance down, so
# a tbl the user holds open on the same file is unaffected.
.duckdb_built_with <- function(db_path) {
  if (!file.exists(db_path)) return(NA_character_)
  con <- tryCatch(.duckdb_connect_quiet(db_path, read_only = TRUE),
                  error = function(e) NULL)
  if (is.null(con)) return(NA_character_)
  on.exit(DBI::dbDisconnect(con, shutdown = FALSE))
  info <- tryCatch(.read_build_info(con), error = function(e) NULL)
  if (is.null(info)) return(NA_character_)
  v <- unique(info$canpumf_version)
  paste(v[order(package_version(v))], collapse = ", ")
}

# Size in MB of all files under path matching the (optional) include pattern,
# excluding paths that match exclude_pattern.
.path_size_mb <- function(paths) {
  if (length(paths) == 0L) return(0)
  sum(file.info(paths)$size, na.rm = TRUE) / 1e6
}

# Describe one non-LFS version directory as a single-row tibble.
.describe_version_dir <- function(series, version, version_dir) {
  has_zip <- !is.null(.find_version_zip(version_dir))
  has_ext <- .version_is_extracted(version_dir)

  has_metadata <- file.exists(
    file.path(version_dir, "metadata", "variables.csv"))

  db_file <- file.path(
    version_dir,
    paste0(series, "_", gsub("[^A-Za-z0-9._-]", "_", version), ".duckdb"))
  has_duckdb <- file.exists(db_file)

  all_files <- list.files(version_dir, recursive = TRUE, full.names = TRUE)
  raw_files <- all_files[!grepl(
    "/metadata(/|$)|\\.duckdb", all_files, ignore.case = TRUE)]

  tibble::tibble(
    series       = series,
    version      = version,
    has_raw      = has_zip || has_ext,
    has_metadata = has_metadata,
    has_duckdb   = has_duckdb,
    raw_mb       = .path_size_mb(raw_files),
    duckdb_mb    = if (has_duckdb) file.info(db_file)$size / 1e6 else NA_real_,
    built_with   = if (has_duckdb) .duckdb_built_with(db_file) else NA_character_
  )
}

# Describe all versions of a longitudinal series (LFS, LFS_HIST) — combining
# disk state with the shared DuckDB's tracking table.
.describe_lfs_cache <- function(lfs_dir, series = "LFS") {
  spec    <- .pumf_longitudinal_spec(series)
  vt      <- spec$versions_table
  db_path <- file.path(lfs_dir, spec$db_file)
  db_mb   <- if (file.exists(db_path)) file.info(db_path)$size / 1e6 else NA_real_

  # Loaded versions from the shared DuckDB tracking table
  loaded <- character(0L)
  if (file.exists(db_path)) {
    con <- tryCatch(
      .duckdb_connect_quiet(db_path, read_only = TRUE),
      error = function(e) NULL
    )
    if (!is.null(con)) {
      if (DBI::dbExistsTable(con, vt))
        loaded <- DBI::dbGetQuery(con, paste("SELECT version FROM", vt))$version
      DBI::dbDisconnect(con, shutdown = TRUE)
    }
  }

  # Version directories present on disk (pattern: YYYY or YYYY-MM)
  on_disk <- list.dirs(lfs_dir, recursive = FALSE, full.names = FALSE)
  on_disk <- on_disk[grepl("^[0-9]{4}(-[0-9]{2})?$", on_disk)]

  all_versions <- sort(union(on_disk, loaded))
  if (length(all_versions) == 0L) return(.empty_cache_tibble())

  rows <- lapply(all_versions, function(v) {
    vdir <- file.path(lfs_dir, v)
    has_zip <- dir.exists(vdir) && !is.null(.find_version_zip(vdir))
    has_ext <- dir.exists(vdir) && .version_is_extracted(vdir)

    has_metadata <- dir.exists(vdir) &&
      file.exists(file.path(vdir, "metadata", "variables.csv"))

    all_files <- if (dir.exists(vdir))
      list.files(vdir, recursive = TRUE, full.names = TRUE)
    else character(0L)
    raw_files <- all_files[!grepl(
      "/metadata(/|$)|\\.duckdb", all_files, ignore.case = TRUE)]

    tibble::tibble(
      series       = series,
      version      = v,
      has_raw      = has_zip || has_ext,
      has_metadata = has_metadata,
      has_duckdb   = v %in% loaded,
      raw_mb       = .path_size_mb(raw_files),
      # Shared DuckDB: same file backs all versions; show total size in every row.
      duckdb_mb    = db_mb,
      # The longitudinal databases track their versions in their own table
      # and carry no per-table build stamp.
      built_with   = NA_character_
    )
  })

  do.call(rbind, rows)
}


# ---- list_pumf_cache --------------------------------------------------------

#' List the contents of the local canpumf cache
#'
#' Scans the cache directory and returns a tibble describing every downloaded
#' PUMF version — which raw files, parsed metadata, and DuckDB tables are
#' present — along with their disk sizes.
#'
#' For LFS surveys the DuckDB is a single shared file (`LFS.duckdb`) that
#' accumulates all versions; its total size is reported in `duckdb_mb` for
#' every LFS row.  Use [remove_pumf_cache()] to free disk space.
#'
#' @param cache_path Root cache directory.  Defaults to
#'   `getOption("canpumf.cache_path", tempdir())`.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{`series`}{Survey series acronym.}
#'     \item{`version`}{Version string.}
#'     \item{`has_raw`}{`TRUE` if a zip or extracted data files are present.}
#'     \item{`has_metadata`}{`TRUE` if a parsed `metadata/` directory exists.}
#'     \item{`has_duckdb`}{`TRUE` if a DuckDB table is built for this version.}
#'     \item{`raw_mb`}{Disk size of raw files in MB (excluding metadata and DuckDB).}
#'     \item{`duckdb_mb`}{Disk size of the DuckDB file in MB.  For LFS this is
#'       the total shared `LFS.duckdb` size, repeated for each version row.}
#'     \item{`built_with`}{The canpumf version that built the DuckDB tables,
#'       from the build stamp Stage 3 writes since 0.6.1.  `NA` when there is
#'       no DuckDB, when it was built before 0.6.1 (no stamp: no `pumf_row_id`
#'       key and no sentinel companion, so `pumf_sidecar()` needs a rebuild
#'       with `get_pumf(..., refresh = TRUE)`), for the longitudinal series,
#'       and when the file is locked by a writer.  Tables built by different
#'       versions are listed together, oldest first.}
#'   }
#'   Returns a zero-row tibble with the same column structure if the cache
#'   directory does not exist or is empty.
#'
#' @seealso [remove_pumf_cache()], [get_pumf()]
#'
#' @examples
#' \donttest{
#' list_pumf_cache()
#' # With an explicit cache path:
#' list_pumf_cache(cache_path = file.path(tempdir(), "pumf_cache"))
#' }
#' @export
list_pumf_cache <- function(cache_path = getOption("canpumf.cache_path",
                                                    tempdir())) {
  if (!dir.exists(cache_path)) return(.empty_cache_tibble())

  series_dirs <- list.dirs(cache_path, recursive = FALSE, full.names = FALSE)
  series_dirs <- series_dirs[nchar(series_dirs) > 0L]

  rows <- list()

  for (series in series_dirs) {
    series_dir <- file.path(cache_path, series)

    if (.is_longitudinal(series)) {
      lfs_rows <- .describe_lfs_cache(series_dir, series)
      if (nrow(lfs_rows) > 0L) rows[[length(rows) + 1L]] <- lfs_rows
      next
    }

    version_dirs <- list.dirs(series_dir, recursive = FALSE, full.names = FALSE)
    version_dirs <- version_dirs[nchar(version_dirs) > 0L]
    for (version in version_dirs)
      rows[[length(rows) + 1L]] <- .describe_version_dir(
        series, version, file.path(series_dir, version))
  }

  if (length(rows) == 0L) return(.empty_cache_tibble())
  do.call(rbind, rows)
}


# ---- remove_pumf_cache ------------------------------------------------------

#' Remove a PUMF version, or one of its languages, from the local cache
#'
#' Deletes the DuckDB table (and optionally the raw zip and extracted files)
#' for one cached PUMF version, or with `lang` only the tables of one
#' language.
#'
#' With the default `keep_raw = TRUE`, only the DuckDB and parsed `metadata/`
#' are removed; the raw zip and extracted data are left intact so that
#' [get_pumf()] can rebuild without re-downloading.  Set `keep_raw = FALSE`
#' to delete everything, freeing the full disk space.
#'
#' A survey built in both languages holds an `eng` and a `fra` table (each
#' with its sentinel companion) in one DuckDB
#' file.  `lang = "fra"` drops the French tables and keeps everything else:
#' the English tables, the shared bootstrap-weight tables, the metadata and
#' the raw files.  DuckDB does not return the space of a dropped table to the
#' file system on its own, so the database is then compacted by copying it to
#' a fresh file, which takes about as long as reading the remaining tables
#' once.  When no table remains, the file is deleted instead.  The database
#' must not be open: close tbls with [close_pumf()] first.  `keep_raw` is
#' ignored with `lang`, and the longitudinal series (`"LFS"`, `"LFS_HIST"`)
#' do not support it.
#'
#' For LFS surveys the DuckDB is shared across all versions.  Removing one
#' version deletes only that version's rows from the shared `LFS.duckdb`; if
#' it was the last loaded version the shared database file is also deleted.
#'
#' @param series Survey series acronym, e.g. `"SFS"` or `"LFS"`.
#' @param version Version string, e.g. `"2019"` or `"2023-06"`.
#' @param keep_raw If `TRUE` (default), keep the raw zip and extracted data so
#'   [get_pumf()] can rebuild without re-downloading.  If `FALSE`, delete
#'   everything including raw files.
#' @param cache_path Root cache directory.  Defaults to
#'   `getOption("canpumf.cache_path", tempdir())`.
#' @param lang `NULL` (default) removes the version.  `"eng"` or `"fra"`
#'   removes only that language's tables from the DuckDB and compacts the
#'   file.
#'
#' @return Invisibly `NULL`.
#'
#' @seealso [list_pumf_cache()], [get_pumf()]
#'
#' @examples
#' \donttest{
#' # Drop the French tables of a bilingual build and shrink the file:
#' fra <- get_pumf("SFS", "2019", lang = "fra")   # NULL if StatCan is unreachable
#' if (!is.null(fra)) {
#'   close_pumf(fra)
#'   remove_pumf_cache("SFS", "2019", lang = "fra")
#' }
#'
#' # Remove only DuckDB and metadata, keep raw files for quick rebuild:
#' remove_pumf_cache("SFS", "2019")
#'
#' # Remove everything including raw files:
#' remove_pumf_cache("SFS", "2019", keep_raw = FALSE)
#' }
#' @export
remove_pumf_cache <- function(series,
                               version,
                               keep_raw   = TRUE,
                               cache_path = getOption("canpumf.cache_path",
                                                      tempdir()),
                               lang       = NULL) {
  if (!is.null(lang)) {
    if (!is.character(lang) || length(lang) != 1L || !lang %in% c("eng", "fra"))
      stop("'lang' must be \"eng\" or \"fra\".", call. = FALSE)
    if (.is_longitudinal(series))
      stop("'lang' is not supported for the longitudinal series (", series,
           "): their shared database holds every version in one table per ",
           "language.", call. = FALSE)
  }
  if (.is_longitudinal(series)) {
    .remove_lfs_cache(version, cache_path, keep_raw, series)
  } else {
    version_dir <- file.path(cache_path, series, version)
    if (!dir.exists(version_dir))
      stop("'", series, " ", version, "' not found in cache at ", cache_path)
    if (is.null(lang))
      .remove_non_lfs_cache(series, version, version_dir, keep_raw)
    else
      .remove_pumf_lang(series, version, lang, cache_path)
  }
  invisible(NULL)
}

# Drop the tables of one language from a survey's DuckDB and compact the
# file.  The language's objects are its main table(s) (one per module),
# their sidecar tables (pumf_sentinels_, pumf_removed_), the
# <table>_bsw_* views that 0.6.0 made for bootstrap weights and their
# pumf_build_info rows; the pumf_bsw_* weight tables are shared between the
# languages (they join on pumf_row_id) and stay while the other language
# does.  A dropped table's blocks are marked free inside the file but the
# file is not truncated, so the remaining content is copied into a fresh
# file (COPY FROM DATABASE keeps the ENUM types, views and stamp) which then
# replaces the old one.  Needs the write lock.
.remove_pumf_lang <- function(series, version, lang, cache_path) {
  db_path <- .pumf_db_path(series, version, cache_path)
  if (!file.exists(db_path))
    stop("'", series, " ", version, "' has no DuckDB in the cache at ",
         cache_path, "; nothing to remove.", call. = FALSE)
  .assert_duckdb_writable(db_path)
  size_before <- file.info(db_path)$size

  con  <- .duckdb_connect_quiet(db_path)
  done <- FALSE
  on.exit(if (!done) DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  objs <- DBI::dbListTables(con)

  reg    <- pumf_registry_lookup(series, version)
  mods   <- .pumf_entry_modules(reg)
  mains  <- if (is.null(mods)) .pumf_table_name(series, version, lang)
            else vapply(names(mods), function(m)
              .pumf_table_name(series, version, lang, m), character(1L))
  mains  <- intersect(mains, objs)
  if (length(mains) == 0L) {
    DBI::dbDisconnect(con, shutdown = TRUE); done <- TRUE
    stop("'", series, " ", version, "' has no '", lang, "' table in ",
         basename(db_path), ".", call. = FALSE)
  }
  others <- if (is.null(mods)) .pumf_table_name(series, version,
                                                setdiff(c("eng", "fra"), lang))
            else vapply(names(mods), function(m)
              .pumf_table_name(series, version, setdiff(c("eng", "fra"), lang), m),
              character(1L))
  other_remains <- length(intersect(others, objs)) > 0L

  if (!other_remains) {
    # The last language: the whole file goes, like remove_pumf_cache()
    # without lang but leaving the metadata (it is bilingual and cheap).
    DBI::dbDisconnect(con, shutdown = TRUE); done <- TRUE
    unlink(c(db_path, paste0(db_path, ".wal")))
    message("Removed ", basename(db_path), ": the '", lang, "' table",
            if (length(mains) > 1L) "s" else "",
            " of ", series, " ", version, " were the last. ",
            sprintf("%.0f MB freed.", size_before / 1e6))
    return(invisible(NULL))
  }

  views <- objs[grepl(paste0("^(", paste(mains, collapse = "|"), ")_bsw"), objs)]
  for (v in views)
    DBI::dbExecute(con, sprintf('DROP VIEW IF EXISTS "%s"', v))
  for (t in c(mains, intersect(.pumf_sidecar_tables(mains), objs)))
    DBI::dbExecute(con, sprintf('DROP TABLE IF EXISTS "%s"', t))
  if (DBI::dbExistsTable(con, .build_info_table))
    for (t in mains)
      DBI::dbExecute(con, sprintf(
        'DELETE FROM "%s" WHERE "table" = ?', .build_info_table),
        params = list(t))

  # Compact: copy what is left into a new file beside the old one, then swap.
  tmp_path <- paste0(db_path, ".compact")
  unlink(c(tmp_path, paste0(tmp_path, ".wal")))
  dbname <- DBI::dbGetQuery(con, "SELECT current_database() AS d")$d
  DBI::dbExecute(con, sprintf("ATTACH '%s' AS canpumf_compact",
                              gsub("'", "''", tmp_path)))
  DBI::dbExecute(con, sprintf('COPY FROM DATABASE "%s" TO canpumf_compact',
                              dbname))
  DBI::dbExecute(con, "DETACH canpumf_compact")
  DBI::dbDisconnect(con, shutdown = TRUE); done <- TRUE
  unlink(paste0(db_path, ".wal"))
  # Swap in two renames (Windows cannot rename onto an existing file); the
  # old file is only deleted once the compacted copy is in place.
  old_path <- paste0(db_path, ".old")
  unlink(old_path)
  if (!file.rename(db_path, old_path) || !file.rename(tmp_path, db_path))
    stop("Could not replace '", basename(db_path), "' with its compacted ",
         "copy '", basename(tmp_path), "'.", call. = FALSE)
  unlink(old_path)
  size_after <- file.info(db_path)$size
  message("Removed the '", lang, "' table", if (length(mains) > 1L) "s" else "",
          " of ", series, " ", version, " and compacted ", basename(db_path),
          sprintf(": %.0f MB to %.0f MB.", size_before / 1e6, size_after / 1e6))
  invisible(NULL)
}

.remove_non_lfs_cache <- function(series, version, version_dir, keep_raw) {
  if (keep_raw) {
    db_paths <- list.files(version_dir, pattern = "\\.duckdb",
                            ignore.case = TRUE, full.names = TRUE)
    for (p in db_paths) unlink(p)
    meta_dir <- file.path(version_dir, "metadata")
    if (dir.exists(meta_dir)) unlink(meta_dir, recursive = TRUE)
    message("Removed DuckDB and metadata for ", series, " ", version,
            ". Raw files kept; use get_pumf() to rebuild.")
  } else {
    unlink(version_dir, recursive = TRUE)
    message("Removed all cached data for ", series, " ", version, ".")
  }
}

.remove_lfs_cache <- function(version, cache_path, keep_raw, series = "LFS") {
  spec     <- .pumf_longitudinal_spec(series)
  vt       <- spec$versions_table
  data_tbl <- paste0(spec$table_prefix, c("_eng", "_fra"))
  lfs_dir  <- file.path(cache_path, series)
  db_path  <- file.path(lfs_dir, spec$db_file)
  vdir     <- file.path(lfs_dir, version)
  survyear <- .lfs_survyear(version)
  survmnth <- .lfs_survmnth(version)

  if (!dir.exists(lfs_dir))
    stop("No ", series, " data found in cache at ", cache_path)

  if (file.exists(db_path)) {
    .assert_duckdb_writable(db_path)
    con <- .duckdb_connect_quiet(db_path)

    for (tbl in data_tbl) {
      if (!DBI::dbExistsTable(con, tbl)) next
      if (is.na(survmnth)) {
        DBI::dbExecute(con, sprintf(
          'DELETE FROM "%s" WHERE SURVYEAR = %d', tbl, survyear))
      } else {
        DBI::dbExecute(con, sprintf(
          'DELETE FROM "%s" WHERE SURVYEAR = %d AND SURVMNTH = %d',
          tbl, survyear, survmnth))
      }
    }

    # A year also removes its monthly versions (a series without annual
    # files records only months).
    if (DBI::dbExistsTable(con, vt))
      DBI::dbExecute(con, if (is.na(survmnth))
        sprintf("DELETE FROM %s WHERE survyear = %d", vt, survyear)
      else sprintf("DELETE FROM %s WHERE version = '%s'", vt, version))

    # Delete the shared DuckDB when ALL data tables are empty, not just when
    # lfs_versions is empty — the two can diverge if data was manipulated
    # outside the normal pipeline.
    data_tbls <- intersect(data_tbl, DBI::dbListTables(con))
    total_rows <- if (length(data_tbls) == 0L) 0L else {
      sum(vapply(data_tbls, function(t)
        DBI::dbGetQuery(con, sprintf('SELECT COUNT(*) AS n FROM "%s"', t))$n,
        numeric(1L)))
    }
    DBI::dbDisconnect(con, shutdown = TRUE)

    if (total_rows == 0L) {
      unlink(list.files(lfs_dir, pattern = "\\.duckdb",
                         full.names = TRUE, ignore.case = TRUE))
      message(series, " database removed (no data remaining).")
    }
  }

  # Version directories: the version itself, or the months of a year.
  vdirs <- if (is.na(survmnth))
    c(vdir, file.path(lfs_dir, sprintf("%s-%02d", version, 1:12)))
  else vdir
  vdirs <- vdirs[dir.exists(vdirs)]
  if (length(vdirs) > 0L) {
    if (keep_raw) {
      for (d in vdirs) {
        meta_dir <- file.path(d, "metadata")
        if (dir.exists(meta_dir)) unlink(meta_dir, recursive = TRUE)
      }
      message("Removed ", series, " ", version, " from database. ",
              "Raw files kept; use get_pumf(\"", series, "\", \"", version,
              "\") to rebuild.")
    } else {
      unlink(vdirs, recursive = TRUE)
      message("Removed all cached data for ", series, " ", version, ".")
    }
  } else {
    message("Removed ", series, " ", version, " from database.")
  }
}
