
#' Get select pumf database connections
#'
#' @description This is a convenience function that downloads and accesses pumf data for
#' a curated set of pumf datasets.
#'
#' @param pumf_series sereis for the pumf data, like LSF, or CHS
#' @param pumf_version In case there are several versions of a given series, like for LFS, the version
#' @param layout_mask optional layout mask in case there are several files.
#' identifiers. For LFS this is the month/year.
#' @param file_mask optional additional mask to filter down to specific PUMF file if there are several
#' @param guess_numeric logical, will guess numeric columns and covert to numeric and set missing values
#' to \code{NA} if set to \code{TRUE} (default)
#' @param pumf_cache_path A path to a permanent cache. If none is fould the data is stored in the temporary
#' directory for the duration of the session.
#' @param refresh optionall re-downlad pumf data, only for series that can be downloaded directly from StatCan
#' @param refresh_layout (optional) regenerate the layout and metadata
#' @param timeout Optional parameter to specify connection timeout for download
#'
#' @return A database connection
#' @export
get_pumf_connection <- function(pumf_series,pumf_version = NULL,
                     layout_mask=NULL,
                     file_mask=layout_mask,
                     guess_numeric = TRUE,
                     pumf_cache_path = getOption("canpumf.cache_path"),
                     refresh=FALSE,
                     refresh_layout=FALSE,
                     timeout=3000){
  pumf_data <- NULL
  old_timeout <- getOption("timeout")
  options(timeout=timeout)
  if (is.null(pumf_cache_path)) {
    warning("No cache path specified, storing pumf data in temporary directory.")
    pumf_cache_path <- file.path(tempdir(),"pumf")
    if (!dir.exists(pumf_cache_path)) dir.create(pumf_cache_path)
  }
  if (!dir.exists(pumf_cache_path)) stop("Invalid cache path: ",pumf_cache_path)

  d<- list_canpumf_collection() %>%
    filter(.data$Acronym==pumf_series)
  pumf_dir <- file.path(pumf_cache_path,pumf_series)
  if (!is.null(pumf_version)) {
    dd <- d %>% filter(.data$Version==pumf_version)
    if (nrow(dd)==0) {
      dd<-d |> filter(grepl(pumf_version,.data$Version))
    }
    if (nrow(dd) > 1) {
      if (pumf_series=="Census") {
        dd <- dd |>
          filter(grepl("individuals",.data$Version, ignore.case = TRUE))
      }
    }
    d <- dd
  } else if (is.null(pumf_version)) {
    if (nrow(d)!=1) {
      stop("Could not find PUMF version.")
    }
    pumf_version <- d$Version
  }

  if (nrow(d)!=1) {
    stop("Could not find PUMF version.")
  }

  destination_dir <- get_data_cache_path(pumf_cache_path,pumf_series,pumf_version)

  if (!is.null(destination_dir) && length(destination_dir) == 1) {
    destination_dir <- file.path(pumf_cache_path,destination_dir)
  } else {
    destination_dir <- file.path(pumf_cache_path,pumf_series)
    if (!is.null(pumf_version)) {
      destination_dir <- file.path(destination_dir,pumf_version)
    }
  }


  duckdb_table_name <- paste0(c(pumf_version,layout_mask,file_mask), collapse =  "_")
  if (is.null(duckdb_table_name)) {
    duckdb_table_name="pumf_version"
  }

  duckdb_path <- file.path(destination_dir,paste0(pumf_series,".duckdb"))

  if (!file.exists(duckdb_path)) {
    pumf_data <- get_pumf(pumf_series = pumf_series,
                          pumf_version = pumf_version,
                          layout_mask=layout_mask,
                          file_mask=file_mask,
                          guess_numeric = guess_numeric,
                          pumf_cache_path = pumf_cache_path,
                          refresh=refresh,
                          refresh_layout=refresh_layout,
                          timeout=timeout)

    pumf_data <- pumf_data |>
      label_pumf_data()

    con <- DBI::dbConnect(duckdb::duckdb(), dbdir=duckdb_path)

    duckdb::dbWriteTable(con, duckdb_table_name, pumf_data)

    DBI::dbDisconnect(con)
  }

  pumf_tbl <- DBI::dbConnect(duckdb::duckdb(), dbdir=duckdb_path) |>
    tbl(duckdb_table_name)

  return (pumf_tbl)
}
