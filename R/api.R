# R/api.R — Public entry points for the canpumf package.
#
# get_pumf()      — download, parse, label, and return a lazy DuckDB tbl
# pumf_metadata() — download and parse metadata only; return canonical list


# ---- Internal helper: resolve deprecated parameter names --------------------

.api_resolve_deprecated <- function(series, version, cache_path,
                                     dots, fn_name) {
  if (!is.null(dots$pumf_series)) {
    warning(fn_name, ": argument 'pumf_series' is deprecated; use 'series'.",
            call. = FALSE)
    if (is.null(series)) series <- dots$pumf_series
  }
  if (!is.null(dots$pumf_version)) {
    warning(fn_name, ": argument 'pumf_version' is deprecated; use 'version'.",
            call. = FALSE)
    if (is.null(version)) version <- dots$pumf_version
  }
  if (!is.null(dots$pumf_cache_path)) {
    warning(fn_name, ": argument 'pumf_cache_path' is deprecated; use 'cache_path'.",
            call. = FALSE)
    cache_path <- dots$pumf_cache_path
  }
  # Silently drop args that no longer apply (guess_numeric, timeout, etc.)
  list(series = series, version = version, cache_path = cache_path)
}


# ---- get_pumf ---------------------------------------------------------------

#' Get a Statistics Canada PUMF dataset as a lazy DuckDB table
#'
#' Main entry point for the canpumf package.  Downloads (if needed), parses
#' metadata, applies bilingual labels, and returns a lazy `dplyr::tbl()` backed
#' by a DuckDB file in the cache directory.  Subsequent calls reuse the cached
#' DuckDB without re-downloading.
#'
#' **Breaking change from the pre-DuckDB API**: this function now returns a
#' lazy `dplyr::tbl()` instead of a tibble.  Call `dplyr::collect()` to
#' materialise a local tibble.
#'
#' The LFS is treated specially: all versions share a single `LFS.duckdb`
#' database.  Pass `version = "YYYY"` (annual) or `"YYYY-MM"` (monthly).
#' `refresh = "auto"` downloads every available LFS version that is not yet in
#' the database; this is only valid for LFS.
#'
#' @param series Survey series acronym, e.g. `"SFS"`, `"CHS"`, `"LFS"`,
#'   `"Census"`, `"CPSS"`.
#' @param version Version string (e.g. `"2019"`, `"2021 (individuals)"`,
#'   `"2023-06"`).  For series with a single version omit or pass `NULL`.
#' @param lang `"eng"` (default) or `"fra"`.  Selects which set of labels to
#'   apply.  Each language creates a separate DuckDB table (created lazily on
#'   first request).
#' @param cache_path Root cache directory.  Defaults to
#'   `getOption("canpumf.cache_path", tempdir())`.  Set persistently in
#'   `.Rprofile` with `options(canpumf.cache_path = "<path>")`.
#' @param refresh `FALSE` (default) reuses cached data.  `TRUE` clears the
#'   DuckDB table and metadata and rebuilds from the already-extracted raw
#'   files (does not re-download).  `"auto"` is accepted for LFS only and
#'   downloads all available versions not yet in the database.
#' @param redownload If `TRUE`, delete the cached zip and extracted files and
#'   re-download from StatCan before rebuilding.  Implies `refresh = TRUE`.
#'   Not valid with `refresh = "auto"`.
#' @param read_only Open the DuckDB connection in read-only mode (default
#'   `TRUE`).  Pass `FALSE` to allow write access, e.g. to persist custom
#'   views or derived tables in the DuckDB file.  Use [close_pumf()] to
#'   release the connection when done.
#' @param ... Accepts deprecated parameter names (`pumf_series`,
#'   `pumf_version`, `pumf_cache_path`, `layout_mask`, `file_mask`,
#'   `guess_numeric`, `timeout`, `refresh_layout`) with a warning.
#'
#' @return A lazy `dplyr::tbl()` backed by a DuckDB connection.
#'   Call `dplyr::collect()` to obtain a local tibble.
#'   Call [close_pumf()] to release the connection.
#' @export
get_pumf <- function(series     = NULL,
                     version    = NULL,
                     lang       = "eng",
                     cache_path = getOption("canpumf.cache_path", tempdir()),
                     refresh    = FALSE,
                     redownload = FALSE,
                     read_only  = TRUE,
                     ...) {
  dots <- list(...)
  resolved <- .api_resolve_deprecated(series, version, cache_path, dots, "get_pumf")
  series     <- resolved$series
  version    <- resolved$version
  cache_path <- resolved$cache_path

  if (is.null(series))
    stop("'series' must be specified (e.g. get_pumf(\"SFS\", \"2019\")).")
  version <- pumf_resolve_version(series, version)
  stopifnot(lang %in% c("eng", "fra"))

  if (!identical(refresh, FALSE) && !identical(refresh, TRUE) &&
      !identical(refresh, "auto"))
    stop("'refresh' must be FALSE, TRUE, or \"auto\".")
  if (identical(refresh, "auto") && series != "LFS")
    stop("refresh = \"auto\" is only valid for LFS. ",
         "Use refresh = TRUE to rebuild a specific non-LFS survey version.")
  if (isTRUE(redownload) && identical(refresh, "auto"))
    stop("redownload = TRUE is not compatible with refresh = \"auto\". ",
         "Call lfs_get_pumf() per version instead.")

  # Resolve single-version non-LFS series so table_name and db_path are known.
  if (series != "LFS" && is.null(version)) {
    collection <- list_canpumf_collection()
    rows <- dplyr::filter(collection, .data$Acronym == series)
    if (nrow(rows) == 0L)
      stop("Unknown series '", series,
           "'. Check list_canpumf_collection() for available series.")
    if (nrow(rows) > 1L)
      stop("Series '", series, "' has multiple versions: ",
           paste(rows$Version, collapse = ", "),
           ".\nSpecify 'version' (e.g. get_pumf(\"", series, "\", \"",
           rows$Version[[1L]], "\")).")
    version <- rows$Version[[1L]]
  }

  # Ensure the DB is built and get a temporary read-write connection.
  con <- suppressMessages(
    get_pumf_connection(series     = series,
                        version    = version,
                        lang       = lang,
                        cache_path = cache_path,
                        refresh    = refresh,
                        redownload = redownload)
  )
  if (is.null(con)) return(invisible(NULL))

  # Select the language table from the connection.
  table_name <- .pumf_table_name(series, version, lang)
  db_path    <- .pumf_db_path(series, version, cache_path)

  if (read_only) {
    # Re-open as read-only so callers cannot accidentally mutate the DB.
    DBI::dbDisconnect(con, shutdown = TRUE)
    tbl <- pumf_open_duckdb(db_path, table_name, read_only = TRUE)
  } else {
    if (!DBI::dbExistsTable(con, table_name)) {
      DBI::dbDisconnect(con, shutdown = TRUE)
      stop("Table '", table_name, "' not found in ", db_path, ".")
    }
    tbl <- dplyr::tbl(con, table_name)
  }

  # For LFS, pumf_open_duckdb returns the whole shared lfs_eng/lfs_fra table.
  # When a specific version was requested, filter to that year (and month).
  if (series == "LFS" && !is.null(version)) {
    survyear <- .lfs_survyear(version)
    survmnth <- .lfs_survmnth(version)
    tbl <- dplyr::filter(tbl, .data$SURVYEAR == survyear)
    if (!is.na(survmnth))
      tbl <- dplyr::filter(tbl, .data$SURVMNTH == survmnth)
  }

  tbl
}


# ---- close_pumf -------------------------------------------------------------

#' Close the DuckDB connection backing a PUMF lazy table
#'
#' Disconnects the DuckDB connection embedded in a lazy `dplyr::tbl()` returned
#' by [get_pumf()].  After calling this function the table can no longer be
#' queried.
#'
#' Closing is only necessary when you need to release the file lock — for
#' example, before calling `get_pumf(..., refresh = TRUE)` on the same survey,
#' or before writing to the DuckDB from another process.  Read-only connections
#' (the default) do not block other reads.
#'
#' @param tbl A lazy `dplyr::tbl()` returned by [get_pumf()].
#' @return Invisibly `NULL`.
#' @export
close_pumf <- function(tbl) {
  con <- tbl$src$con
  if (!is.null(con) && DBI::dbIsValid(con))
    DBI::dbDisconnect(con, shutdown = TRUE)
  invisible(NULL)
}


# ---- pumf_metadata ----------------------------------------------------------

#' Download and parse PUMF metadata without building a DuckDB table
#'
#' Runs Stage 1 (locate or download) and Stage 2 (parse metadata) and returns
#' the full bilingual canonical metadata.  Both `label_en` and `label_fr`
#' columns are always returned regardless of language.  This is useful for
#' inspecting variable definitions and code labels before loading data.
#'
#' @param series Survey series acronym.
#' @param version Version string.
#' @param cache_path Root cache directory.
#' @param refresh If `TRUE`, re-parse metadata from the already-extracted raw
#'   command files (does not re-download).
#' @param redownload If `TRUE`, delete the cached zip and extracted files and
#'   re-download from StatCan before re-parsing.  Implies `refresh = TRUE`.
#'
#' @return Named list with elements:
#'   - `variables`: tibble (name, label_en, label_fr, type, decimals,
#'     missing_low, missing_high)
#'   - `codes`: tibble (name, val, label_en, label_fr)
#'   - `layout`: tibble (name, start, end) or `NULL` for CSV-format data
#' @export
pumf_metadata <- function(series,
                           version,
                           cache_path = getOption("canpumf.cache_path",
                                                   tempdir()),
                           refresh    = FALSE,
                           redownload = FALSE) {
  version     <- pumf_resolve_version(series, version)
  reg         <- pumf_registry_lookup(series, version)
  eff_refresh <- refresh || redownload
  version_dir <- pumf_locate_or_download(series, version,
                                          cache_path = cache_path,
                                          refresh    = eff_refresh,
                                          redownload = redownload)
  pumf_parse_metadata(version_dir,
                       layout_mask       = reg$layout_mask,
                       metadata_encoding = reg$metadata_encoding,
                       refresh           = eff_refresh)
  read_metadata(file.path(version_dir, "metadata"))
}
