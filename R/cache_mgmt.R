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
    duckdb_mb    = double()
  )
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
    duckdb_mb    = if (has_duckdb) file.info(db_file)$size / 1e6 else NA_real_
  )
}

# Describe all LFS versions — combining disk state with lfs_versions table.
.describe_lfs_cache <- function(lfs_dir) {
  db_path <- file.path(lfs_dir, "LFS.duckdb")
  db_mb   <- if (file.exists(db_path)) file.info(db_path)$size / 1e6 else NA_real_

  # Loaded versions from the shared DuckDB tracking table
  loaded <- character(0L)
  if (file.exists(db_path)) {
    con <- tryCatch(
      DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE),
      error = function(e) NULL
    )
    if (!is.null(con)) {
      if (DBI::dbExistsTable(con, "lfs_versions"))
        loaded <- DBI::dbGetQuery(con, "SELECT version FROM lfs_versions")$version
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
      series       = "LFS",
      version      = v,
      has_raw      = has_zip || has_ext,
      has_metadata = has_metadata,
      has_duckdb   = v %in% loaded,
      raw_mb       = .path_size_mb(raw_files),
      # Shared DuckDB: same file backs all versions; show total size in every row.
      duckdb_mb    = db_mb
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
#' every LFS row.
#'
#' @param cache_path Root cache directory.  Defaults to
#'   `getOption("canpumf.cache_path", tempdir())`.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{series}{Survey series acronym.}
#'     \item{version}{Version string.}
#'     \item{has_raw}{`TRUE` if a zip or extracted data files are present.}
#'     \item{has_metadata}{`TRUE` if a parsed `metadata/` directory exists.}
#'     \item{has_duckdb}{`TRUE` if a DuckDB table is built for this version.}
#'     \item{raw_mb}{Disk size of raw files in MB (excluding metadata and DuckDB).}
#'     \item{duckdb_mb}{Disk size of the DuckDB file in MB.  For LFS this is
#'       the total shared `LFS.duckdb` size, repeated for each version.}
#'   }
#' @export
list_pumf_cache <- function(cache_path = getOption("canpumf.cache_path",
                                                    tempdir())) {
  if (!dir.exists(cache_path)) return(.empty_cache_tibble())

  series_dirs <- list.dirs(cache_path, recursive = FALSE, full.names = FALSE)
  series_dirs <- series_dirs[nchar(series_dirs) > 0L]

  rows <- list()

  for (series in series_dirs) {
    series_dir <- file.path(cache_path, series)

    if (series == "LFS") {
      lfs_rows <- .describe_lfs_cache(series_dir)
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

#' Remove a PUMF version from the local cache
#'
#' Deletes the DuckDB table (and optionally the raw zip and extracted files)
#' for one cached PUMF version.
#'
#' With the default `keep_raw = TRUE`, only the DuckDB and parsed `metadata/`
#' are removed; the raw zip and extracted data are left intact so that
#' [get_pumf()] can rebuild without re-downloading.  Set `keep_raw = FALSE`
#' to delete everything, freeing the full disk space.
#'
#' For LFS surveys the DuckDB is shared across all versions.  Removing one
#' version deletes only that version's rows; if it was the last version the
#' shared `LFS.duckdb` is also deleted.
#'
#' @param series Survey series acronym, e.g. `"SFS"` or `"LFS"`.
#' @param version Version string, e.g. `"2019"` or `"2023-06"`.
#' @param keep_raw If `TRUE` (default), keep the raw zip and extracted data so
#'   [get_pumf()] can rebuild without re-downloading.  If `FALSE`, delete
#'   everything including raw files.
#' @param cache_path Root cache directory.  Defaults to
#'   `getOption("canpumf.cache_path", tempdir())`.
#'
#' @return Invisibly `NULL`.
#' @export
remove_pumf_cache <- function(series,
                               version,
                               keep_raw   = TRUE,
                               cache_path = getOption("canpumf.cache_path",
                                                      tempdir())) {
  if (series == "LFS") {
    .remove_lfs_cache(version, cache_path, keep_raw)
  } else {
    version_dir <- file.path(cache_path, series, version)
    if (!dir.exists(version_dir))
      stop("'", series, " ", version, "' not found in cache at ", cache_path)
    .remove_non_lfs_cache(series, version, version_dir, keep_raw)
  }
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

.remove_lfs_cache <- function(version, cache_path, keep_raw) {
  lfs_dir  <- file.path(cache_path, "LFS")
  db_path  <- file.path(lfs_dir, "LFS.duckdb")
  vdir     <- file.path(lfs_dir, version)
  survyear <- .lfs_survyear(version)
  survmnth <- .lfs_survmnth(version)

  if (!dir.exists(lfs_dir))
    stop("No LFS data found in cache at ", cache_path)

  if (file.exists(db_path)) {
    .assert_duckdb_writable(db_path)
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)

    for (tbl in c("lfs_eng", "lfs_fra")) {
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

    if (DBI::dbExistsTable(con, "lfs_versions"))
      DBI::dbExecute(con, sprintf(
        "DELETE FROM lfs_versions WHERE version = '%s'", version))

    # Delete the shared DuckDB when ALL data tables are empty, not just when
    # lfs_versions is empty — the two can diverge if data was manipulated
    # outside the normal pipeline.
    data_tbls <- intersect(c("lfs_eng", "lfs_fra"), DBI::dbListTables(con))
    total_rows <- if (length(data_tbls) == 0L) 0L else {
      sum(vapply(data_tbls, function(t)
        DBI::dbGetQuery(con, sprintf('SELECT COUNT(*) AS n FROM "%s"', t))$n,
        numeric(1L)))
    }
    DBI::dbDisconnect(con, shutdown = TRUE)

    if (total_rows == 0L) {
      unlink(list.files(lfs_dir, pattern = "\\.duckdb",
                         full.names = TRUE, ignore.case = TRUE))
      message("LFS database removed (no data remaining).")
    }
  }

  if (dir.exists(vdir)) {
    if (keep_raw) {
      meta_dir <- file.path(vdir, "metadata")
      if (dir.exists(meta_dir)) unlink(meta_dir, recursive = TRUE)
      message("Removed LFS ", version, " from database. ",
              "Raw files kept; use get_pumf(\"LFS\", \"", version,
              "\") to rebuild.")
    } else {
      unlink(vdir, recursive = TRUE)
      message("Removed all cached data for LFS ", version, ".")
    }
  } else {
    message("Removed LFS ", version, " from database.")
  }
}
