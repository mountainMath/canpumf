#' Open PUMF documentation in browser
#'
#' @description When available this will open the PUF user guide in the default browser.
#'
#' @param pumf_series sereis for the pumf data, like LSF, or CHS
#' @param pumf_version In case there are several versions of a given series, like for LFS, the version
#' @param documentation_type which documentation to open, either "user_guide", "reference_guide" or "quality" or "errata". Not
#' all types are available for all PUMFs.
#' identifiers. For LFS this is the month/year.
#' @param pumf_cache_path A path to a permanent cache. If none is fould the data is stored in the temporary
#' directory for the duration of the session.
#'
#' @return nothing, opens document in browser
#' @export
open_pumf_documentation <- function(pumf_series, pumf_version = NULL,
                      documentation_type="user_guide",
                      pumf_cache_path = getOption("canpumf.cache_path")){
  cached_pumf <- dir(pumf_cache_path,full.names=TRUE)
  url <- NULL
  if (pumf_version=="1996 (individuals)"|pumf_version=="1996") {
    path <- cached_pumf[grepl("95M0010X",cached_pumf)&grepl("1996",cached_pumf)&!grepl("\\.zip$",cached_pumf)]
    if (length(path)==1) {
      path <- dir(path,full.names=TRUE,"english",ignore.case=TRUE)
      path <- dir(path,full.names=TRUE,"doc",ignore.case=TRUE)
      url <- dir(path,full.names=TRUE,"\\.pdf")
    }
  } else if (pumf_version=="2001 (individuals)"|pumf_version=="2001"){
    path <- cached_pumf[grepl("95M0016X",cached_pumf)&grepl("2001",cached_pumf)&!grepl("\\.zip$",cached_pumf)]
    if (length(path)==1) {
      path <- dir(path,full.names=TRUE,"english",ignore.case=TRUE)
      path <- dir(path,full.names=TRUE,"doc",ignore.case=TRUE)
      path <- dir(path,full.names=TRUE,"pdf",ignore.case=TRUE)
      if (documentation_type=="user_guide") {
        url <- dir(path,full.names=TRUE,"Documentation",ignore.case=TRUE)
      } else  if (documentation_type=="quality") {
        url <- dir(path,full.names=TRUE,"quality",ignore.case=TRUE)
      } else  if (documentation_type=="errata") {
        url <- dir(path,full.names=TRUE,"errata",ignore.case=TRUE)
      }
      url <- dir(path,full.names=TRUE,"\\.pdf")
    }
  } else if (pumf_version=="2006 (individuals)"|pumf_version=="2006"){
    path <- cached_pumf[grepl("95M0028X",cached_pumf)&grepl("2006",cached_pumf)&!grepl("\\.zip$",cached_pumf)]
    if (length(path)==1) {
      path <- dir(path,full.names=TRUE,"english",ignore.case=TRUE)
      path <- dir(path,full.names=TRUE,"doc",ignore.case=TRUE)
      if (documentation_type=="user_guide") {
        url <- dir(path,full.names=TRUE,"Documentation",ignore.case=TRUE)
      } else  if (documentation_type=="questionnaire") {
        url <- dir(path,full.names=TRUE,"questionnaire",ignore.case=TRUE)
      }
    }
  } else if (pumf_version=="2011 (individuals)"|pumf_version=="2011"){
    path <- cached_pumf[grepl("99M0001X",cached_pumf)&grepl("2011",cached_pumf)&!grepl("\\.zip$",cached_pumf)]
    if (length(path)==1) {
      path <- dir(path,full.names=TRUE,"Individual file",ignore.case=TRUE)
      path <- dir(path,full.names=TRUE,"english",ignore.case=TRUE)
      path <- dir(path,full.names=TRUE,"doc",ignore.case=TRUE)
      if (documentation_type=="user_guide") {
        url <- dir(path,full.names=TRUE,"User Guide",ignore.case=TRUE)
      } else if (documentation_type=="questionnaire") {
        path <- dir(path,full.names=TRUE,"questionnaire",ignore.case=TRUE)
        url <- dir(path,full.names=TRUE,"\\.pdf",ignore.case=TRUE)
      }
    }
  } else if (pumf_version=="2016 (individuals)"|pumf_version=="2016 (hierarchical)"|pumf_version=="2016"){
    if (pumf_version=="2016 (hierarchical)") {
      path <- cached_pumf[grepl("98M0001X",cached_pumf)&grepl("2016",cached_pumf)&!grepl("\\.zip$",cached_pumf)]
    } else {
      path <- cached_pumf[grepl("98M0001X",cached_pumf)&grepl("2016",cached_pumf)&!grepl("\\.zip$",cached_pumf)]
    }
    if (length(path)==1) {
      path <- dir(path,full.names=TRUE,"english",ignore.case=TRUE)
      path <- dir(path,full.names=TRUE,"Documentation$",ignore.case=TRUE)
      if (documentation_type=="user_guide") {
        url <- dir(path,full.names=TRUE,"User Guide",ignore.case=TRUE)
      } else if (documentation_type=="questionnaire") {
        path <- dir(path,full.names=TRUE,"questionnaire",ignore.case=TRUE)
        url <- dir(path,full.names=TRUE,"\\.pdf",ignore.case=TRUE)
      } else if (documentation_type=="reference_guide") {
        path <- dir(path,full.names=TRUE,"reference",ignore.case=TRUE)
        url <- dir(path,full.names=TRUE,"\\.pdf",ignore.case=TRUE)
      }
    }
  } else if (pumf_version=="2021 (individuals)"|pumf_version=="2021") {
    path <- cached_pumf[grepl("98M0001X",cached_pumf)&grepl("2021",cached_pumf)&!grepl("\\.zip$",cached_pumf)]
    if (length(path)==1) {
      path <- dir(path,full.names=TRUE,"english",ignore.case=TRUE)
      path <- dir(path,full.names=TRUE,"Documentation$",ignore.case=TRUE)
      if (documentation_type=="user_guide") {
        url <- dir(path,full.names=TRUE,"User Guide",ignore.case=TRUE)
      } else if (documentation_type=="questionnaire") {
        path <- dir(path,full.names=TRUE,"questionnaire",ignore.case=TRUE)
        url <- dir(path,full.names=TRUE,"\\.pdf",ignore.case=TRUE)
      } else if (documentation_type=="reference_guide") {
        path <- dir(path,full.names=TRUE,"reference",ignore.case=TRUE)
        url <- dir(path,full.names=TRUE,"\\.pdf",ignore.case=TRUE)
      }
    }  }

  if (!is.null(url)) {
    lapply(url,browseURL)
  } else {
    stop("Could not find documentation for ",pumf_series," ",pumf_version)
  }
 return()
}
