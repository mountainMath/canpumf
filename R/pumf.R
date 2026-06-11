#' Read raw PUMF data as an in-memory tibble
#'
#' Reads a manually-deposited PUMF directory that lives outside the standard
#' cache structure and returns a fully in-memory tibble.  For surveys supported
#' by the canpumf registry, use [get_pumf()] instead, which returns a lazy
#' DuckDB table with bilingual labels pre-applied and no need for a local
#' directory.
#'
#' @param pumf_base_path Path to the extracted PUMF directory containing the
#'   data file and SPSS/SAS command files.
#' @param layout_mask Optional regex string to select a specific layout file
#'   when multiple SPSS command files are present (e.g. `"PUMF_i"` for the
#'   individuals file of a hierarchical survey).
#' @param file_mask Optional regex string to select a specific data file when
#'   multiple data files are present.  Defaults to `layout_mask`.
#' @param guess_numeric Logical; if `TRUE` (default), apply numeric type
#'   conversion and missing-value range handling using the parsed metadata.
#'
#' @return A tibble containing all rows and columns from the data file, with
#'   numeric conversion applied when `guess_numeric = TRUE`.  The tibble has
#'   attributes `pumf_base_path` and `layout_mask` recording the inputs.
#'
#' @seealso [get_pumf()], [pumf_metadata()]
#'
#' @examples
#' \dontrun{
#' df <- read_pumf_data("/path/to/extracted/SFS2019")
#' dim(df)
#' }
#' @export
read_pumf_data <- function(pumf_base_path,
                           layout_mask   = NULL,
                           file_mask     = layout_mask,
                           guess_numeric = TRUE) {
  if (!metadata_exists(pumf_base_path)) {
    tryCatch(
      pumf_parse_metadata(pumf_base_path, layout_mask = layout_mask),
      error = function(e)
        stop("Could not parse metadata in ", pumf_base_path, ": ", e$message)
    )
  }

  meta      <- read_metadata(file.path(pumf_base_path, "metadata"))
  is_fwf    <- !is.null(meta$layout)
  enc       <- "CP1252"
  data_path <- tryCatch(
    .find_pumf_data_file(pumf_base_path, file_mask, is_fwf),
    error = function(e)
      stop("Could not find data file in ", pumf_base_path, ": ", e$message)
  )

  if (is_fwf) {
    pumf_data <- readr::read_fwf(
      data_path,
      col_positions  = readr::fwf_positions(meta$layout$start, meta$layout$end,
                                             col_names = meta$layout$name),
      col_types      = readr::cols(.default = "c"),
      trim_ws        = TRUE,
      locale         = readr::locale(encoding = enc),
      show_col_types = FALSE)
  } else {
    pumf_data <- readr::read_csv(
      data_path,
      col_types      = readr::cols(.default = "c"),
      locale         = readr::locale(encoding = enc),
      show_col_types = FALSE)
    names(pumf_data) <- toupper(names(pumf_data))
  }

  if (guess_numeric)
    pumf_data <- .apply_numeric_conversion(pumf_data, meta$variables)

  attr(pumf_data, "pumf_base_path") <- pumf_base_path
  attr(pumf_data, "layout_mask")    <- layout_mask
  pumf_data
}


#' Get a read-write DuckDB connection to a PUMF database
#'
#' Runs the full pipeline and returns a raw read-write
#' [DBI::DBIConnection-class].  Use this when you need direct SQL access —
#' to persist custom views, join derived tables, or inspect DuckDB internals.
#' For everyday analysis use [get_pumf()], which returns a safer read-only
#' lazy `dplyr::tbl()`.
#'
#' @param series Survey series acronym, e.g. `"SFS"`, `"Census"`.
#' @param version Version string, e.g. `"2019"`.  `NULL` for single-version
#'   series.
#' @param lang `"eng"` (default) or `"fra"`.
#' @param cache_path Root cache directory.  Defaults to
#'   `getOption("canpumf.cache_path", tempdir())`.
#' @param refresh If `TRUE`, rebuild from already-extracted files (no
#'   re-download).
#' @param redownload If `TRUE`, re-download and rebuild from scratch.
#' @param ... Accepts deprecated parameter names (`pumf_series`,
#'   `pumf_version`, `pumf_cache_path`) with a warning.
#'
#' @return A [DBI::DBIConnection-class] in read-write mode.  Disconnect with
#'   `DBI::dbDisconnect(con, shutdown = TRUE)` when done.  For a safer
#'   read-only lazy table use [get_pumf()] instead.
#'
#' @seealso [get_pumf()]
#'
#' @examples
#' \dontrun{
#' con <- get_pumf_connection("SFS", "2019")
#' DBI::dbListTables(con)
#' DBI::dbGetQuery(con, 'SELECT COUNT(*) FROM "eng_SFS_2019"')
#' DBI::dbDisconnect(con, shutdown = TRUE)
#' }
#' @export
get_pumf_connection <- function(series     = NULL,
                                version    = NULL,
                                lang       = "eng",
                                cache_path = getOption("canpumf.cache_path",
                                                       tempdir()),
                                refresh    = FALSE,
                                redownload = FALSE,
                                ...) {
  dots     <- list(...)
  resolved <- .api_resolve_deprecated(series, version, cache_path, dots,
                                      "get_pumf_connection")
  series     <- resolved$series
  version    <- resolved$version
  cache_path <- resolved$cache_path

  if (is.null(series))
    stop("'series' must be specified.")
  version <- pumf_resolve_version(series, version)
  stopifnot(lang %in% c("eng", "fra"))

  if (!identical(refresh, FALSE) && !identical(refresh, TRUE) &&
      !identical(refresh, "auto"))
    stop("'refresh' must be FALSE, TRUE, or \"auto\".")
  if (identical(refresh, "auto") && series != "LFS")
    stop("refresh = \"auto\" is only valid for LFS.")
  if (isTRUE(redownload) && identical(refresh, "auto"))
    stop("redownload = TRUE is not compatible with refresh = \"auto\".")

  if (is.null(version)) {
    collection <- list_canpumf_collection()
    rows <- filter(collection, .data$Acronym == series)
    if (nrow(rows) == 0L)
      stop("Unknown series '", series,
           "'. Check list_canpumf_collection() for available series.")
    if (nrow(rows) > 1L)
      stop("Series '", series, "' has multiple versions: ",
           paste(rows$Version, collapse = ", "), ".\nSpecify 'version'.")
    version <- rows$Version[[1L]]
  }

  tbl <- pumf_run_pipeline(series, version,
                           lang       = lang,
                           cache_path = cache_path,
                           refresh    = refresh,
                           redownload = redownload,
                           read_only  = FALSE)
  if (is.null(tbl)) return(invisible(NULL))
  con    <- tbl$src$con
  tables <- sort(DBI::dbListTables(con))
  message("Connected to DuckDB (read-write). Available tables: ",
          paste(tables, collapse = ", "),
          ".\nDisconnect with DBI::dbDisconnect(con, shutdown = TRUE) when done.")
  con
}
