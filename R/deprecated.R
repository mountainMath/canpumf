# ---- Deprecated functions ----------------------------------------------------
#
# The exports that 0.7.0 folded into other functions.  Each is a thin wrapper
# around its replacement that warns once per call through .Deprecated(); they
# are to be removed in a later release.

#' Deprecated functions in canpumf
#'
#' These functions were folded into others in canpumf 0.7.0.  They still work,
#' with a deprecation warning, and will be removed in a later release.
#'
#' | Deprecated | Use instead |
#' |---|---|
#' | `pumf_var_labels(tbl)` | `pumf_dictionary(tbl, what = "variables")` |
#' | `list_pumf_registry()` | `pumf_registry()` |
#' | `pumf_label_repairs(tbl, action)` | `pumf_pdf_crosscheck(tbl, "repairs", action)` |
#' | `pumf_freq_validation(tbl)` | `pumf_pdf_crosscheck(tbl, "validation")` |
#' | `add_lfs_SURVDATE(tbl)` | `add_lfs_columns(tbl, "SURVDATE")` |
#' | `add_lfs_GENDER_SEX(tbl)` | `add_lfs_columns(tbl, "GENDER_SEX")` |
#' | `get_lfs_timeline(lang, sources, refresh)` | `get_pumf("LFS_TIMELINE", lang = lang, sources = sources, refresh = refresh)` |
#' | `list_canpumf_collection()` | `list_pumf_catalogue()` |
#' | `list_statcan_pumf_catalogue(...)` | `list_pumf_catalogue("statcan", ...)` |
#' | `list_borealis_pumf_catalogue(...)` | `list_pumf_catalogue("borealis", ...)` |
#' | `list_available_lfs_pumf_versions()` | `list_pumf_catalogue("lfs")` |
#' | `get_pumf_connection(series, version)` | `get_pumf(series, version, read_only = FALSE)`, and `dbplyr::remote_con()` on the result for the connection |
#'
#' @param tbl A lazy `dplyr::tbl()` returned by [get_pumf()].
#' @param action As in [pumf_pdf_crosscheck()].
#' @param sources As in [lfs_timeline].
#' @param series,version,lang,cache_path,refresh,redownload,... As in
#'   [get_pumf()].
#'
#' @return What the replacement returns.
#'
#' @name canpumf-deprecated
#' @keywords internal
NULL

#' @rdname canpumf-deprecated
#' @export
pumf_var_labels <- function(tbl) {
  .Deprecated("pumf_dictionary(tbl, what = \"variables\")", package = "canpumf")
  d <- pumf_dictionary(tbl, what = "variables")
  d[, c("name", "label_en", "label_fr", .metadata_description_cols)]
}

#' @rdname canpumf-deprecated
#' @export
get_pumf_connection <- function(series     = NULL,
                                version    = NULL,
                                lang       = "eng",
                                cache_path = getOption("canpumf.cache_path",
                                                       tempdir()),
                                refresh    = FALSE,
                                redownload = FALSE,
                                ...) {
  .Deprecated("get_pumf(..., read_only = FALSE)", package = "canpumf")
  dots     <- list(...)
  resolved <- .api_resolve_deprecated(series, version, cache_path, dots,
                                      "get_pumf_connection")
  series     <- resolved$series
  version    <- resolved$version
  cache_path <- resolved$cache_path

  if (is.null(series))
    stop("'series' must be specified.")
  version <- pumf_resolve_version(series, version, cache_path)
  .pumf_check_call_args(series, lang, refresh, redownload)

  if (is.null(version)) version <- .pumf_single_version(series)

  # Degrade gracefully when Statistics Canada is unreachable: an informative
  # message + NULL rather than a hard error (so get_pumf() examples and callers
  # survive an outage; CRAN policy for packages using Internet resources).
  tbl <- .pumf_offline_null(
    pumf_run_pipeline(series, version,
                      lang       = lang,
                      cache_path = cache_path,
                      refresh    = refresh,
                      redownload = redownload,
                      read_only  = FALSE))
  if (is.null(tbl)) return(invisible(NULL))
  con    <- tbl$src$con
  tables <- sort(DBI::dbListTables(con))
  message("Connected to DuckDB (read-write). Available tables: ",
          paste(tables, collapse = ", "),
          ".\nDisconnect with DBI::dbDisconnect(con, shutdown = TRUE) when done.")
  con
}

#' @rdname canpumf-deprecated
#' @export
list_pumf_registry <- function() {
  .Deprecated("pumf_registry()", package = "canpumf")
  pumf_registry()
}

#' @rdname canpumf-deprecated
#' @export
pumf_label_repairs <- function(tbl, action = NULL) {
  .Deprecated("pumf_pdf_crosscheck(tbl, \"repairs\")", package = "canpumf")
  pumf_pdf_crosscheck(tbl, "repairs", action = action)
}

#' @rdname canpumf-deprecated
#' @export
pumf_freq_validation <- function(tbl) {
  .Deprecated("pumf_pdf_crosscheck(tbl, \"validation\")", package = "canpumf")
  pumf_pdf_crosscheck(tbl, "validation")
}

#' @rdname canpumf-deprecated
#' @export
add_lfs_SURVDATE <- function(tbl) {
  .Deprecated("add_lfs_columns(tbl, \"SURVDATE\")", package = "canpumf")
  add_lfs_columns(tbl, "SURVDATE")
}

#' @rdname canpumf-deprecated
#' @export
add_lfs_GENDER_SEX <- function(tbl) {
  .Deprecated("add_lfs_columns(tbl, \"GENDER_SEX\")", package = "canpumf")
  add_lfs_columns(tbl, "GENDER_SEX")
}

#' @rdname canpumf-deprecated
#' @export
get_lfs_timeline <- function(lang = c("eng", "fra"),
                             sources = c("LFS_HIST", "LFS"),
                             refresh = FALSE,
                             cache_path = getOption("canpumf.cache_path",
                                                    tempdir())) {
  .Deprecated("get_pumf(\"LFS_TIMELINE\")", package = "canpumf")
  .lfs_timeline_open(lang = lang, sources = sources, refresh = refresh,
                     cache_path = cache_path)
}

#' @rdname canpumf-deprecated
#' @export
list_canpumf_collection <- function() {
  .Deprecated("list_pumf_catalogue()", package = "canpumf")
  list_pumf_catalogue("canpumf")
}

#' @rdname canpumf-deprecated
#' @export
list_statcan_pumf_catalogue <- function(...) {
  .Deprecated("list_pumf_catalogue(\"statcan\")", package = "canpumf")
  list_pumf_catalogue("statcan", ...)
}

#' @rdname canpumf-deprecated
#' @export
list_borealis_pumf_catalogue <- function(...) {
  .Deprecated("list_pumf_catalogue(\"borealis\")", package = "canpumf")
  list_pumf_catalogue("borealis", ...)
}

#' @rdname canpumf-deprecated
#' @export
list_available_lfs_pumf_versions <- function() {
  .Deprecated("list_pumf_catalogue(\"lfs\")", package = "canpumf")
  list_pumf_catalogue("lfs")
}
