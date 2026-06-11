# canpumf collection





list_census_collection <- function() {
  base_url <- "https://www150.statcan.gc.ca/n1/pub/98m0001x/index-eng.htm"
  pumf_data <- rvest::read_html(base_url) |>
    rvest::html_nodes("main div ul li a")


  tibble::tibble(Title = "Census of population",
         Acronym = "Census",
         Version=pumf_data |> rvest::html_text(),
         `Survey Number`="3901",
         url=paste0("https://www150.statcan.gc.ca/n1/pub/98m0001x/",pumf_data |> rvest::html_attr("href"))) |>
    mutate(Year=stringr::str_extract(.data$Version,"\\d{4}")) |>
    mutate(type=gsub(" .+","",.data$Version)) |>
    mutate(Version=paste0(.data$Year," (",tolower(.data$type),")")) |>
    select(-"Year",-"type")
}

list_pumf_collection <- function(){
  tibble::tibble(Title=NA,Acronym=NA, `Survey Number`=NA) |>
    stats::na.omit()
}



#' List Statistics Canada PUMF datasets supported by canpumf
#'
#' Returns a tibble of all survey series and versions for which canpumf has
#' download wrappers.  Scrapes the StatCan website to discover Census versions;
#' other series are hard-coded.  Requires an internet connection.
#'
#' @return A tibble with columns `Title`, `Acronym`, `Version`,
#'   `Survey Number`, and `url`.  The `url` column contains the download URL or
#'   `"(EFT)"` for versions distributed via the Research Data Centre (EFT only).
#'   Pass `Acronym` and `Version` to [get_pumf()] to download a dataset.
#'
#' @seealso [get_pumf()], [list_available_lfs_pumf_versions()]
#'
#' @examples
#' \dontrun{
#' collection <- list_canpumf_collection()
#' # Show all SFS versions
#' collection[collection$Acronym == "SFS", c("Acronym", "Version")]
#' }
#' @export
list_canpumf_collection <- function(){
  canpumf_conveninence_series <- c("LFS","ITS","CPSS","SFS","SHS","GSS")
  pumf_surveys<-list_pumf_collection() %>%
    filter(.data$Acronym %in% canpumf_conveninence_series)

  ccahs <- tibble(Title = "Canadian COVID-19 Antibody and Health Survey",
                  Acronym = "CCAHS",
                  Version=c("1"),
                  `Survey Number`="5339",
                  url="https://www150.statcan.gc.ca/n1/en/pub/13-25-0007/2022001/CCAHS_ECSAC.zip?st=BPMowORM")

  cpss <- tibble(Title="Canadian Perspectives Survey Series",
                 Acronym="CPSS",
                 Version=c("1","2","3","4","5","6"),
                 `Survey Number`="5311",
                 url=c("https://www150.statcan.gc.ca/n1/en/pub/45-25-0002/2020001/CSV.zip",
                       "https://www150.statcan.gc.ca/n1/en/pub/45-25-0004/2020001/CSV.zip",
                       "https://www150.statcan.gc.ca/n1/en/pub/45-25-0007/2020001/CSV.zip",
                       "https://www150.statcan.gc.ca/n1/en/pub/45-25-0009/2020001/CSV.zip",
                       "https://www150.statcan.gc.ca/n1/pub/45-25-0010/2021001/CSV-eng.zip",
                       "https://www150.statcan.gc.ca/n1/en/pub/45-25-0012/2021001/CSV.zip"))
  chs <- tibble(Title="Canadian Housing Survey",
                Acronym="CHS",
                Version=c("2018","2021","2022"),
                `Survey Number`="5269",
                url=c("https://www150.statcan.gc.ca/n1/en/pub/46-25-0001/2021001/2018.zip",
                      "https://www150.statcan.gc.ca/n1/en/pub/46-25-0001/2021001/2021.zip",
                      "https://www150.statcan.gc.ca/n1/en/pub/46-25-0001/2021001/2022.zip"))

  shs <- tibble(Title="Survey of Household Spending",
                Acronym="SHS",
                Version=c("2017","2019","2021"),
                `Survey Number`="3508",
                url=c("https://www150.statcan.gc.ca/n1/en/pub/62m0004x/2017001/SHS_EDM_2017-eng.zip",
                      "https://www150.statcan.gc.ca/n1/en/pub/62m0004x/2017001/SHS_EDM_2019.zip",
                      "https://www150.statcan.gc.ca/n1/pub/62m0004x/2017001/SHS_EDM_2021.zip"))

  its_versions <- tibble("Acronym"="ITS",
                         Version=c("2019","2018"),
                         url=c("https://www150.statcan.gc.ca/n1/pub/24-25-0002/2021001/2019/SPSS.zip",
                               "https://www150.statcan.gc.ca/n1/pub/24-25-0002/2021001/2018/SPSS.zip"))

  lfs_version_url<- "https://www150.statcan.gc.ca/n1/pub/71m0001x/71m0001x2021001-eng.htm"
  d<-rvest::read_html(lfs_version_url) %>%
    rvest::html_nodes(xpath="//a")
  d<-d[rvest::html_text(d)=="CSV"]
  lfs_versions <- tibble(Acronym="LFS",url=rvest::html_attr(d,"href")) %>%
    mutate(url=ifelse(substr(.data$url,1,4)=="http",url,paste0("https://www150.statcan.gc.ca/n1/pub/71m0001x/",.data$url))) %>%
    mutate(Version=stringr::str_match(.data$url,"\\d{4}-\\d{2}")%>% lapply(first) %>% unlist) %>%
    mutate(Version=coalesce(.data$Version,stringr::str_match(.data$url,"(\\d{4})-CSV")[,2]))

  # Pre-2006 LFS PUMF monthly releases are EFT-only.  Scrape the catalogue
  # index to discover which year/month combinations StatCan has published.
  # Catalogue entries follow the pattern 71M0001X{YYYY}{MMM} (3-digit month).
  lfs_eft_versions <- tryCatch({
    cat_page <- rvest::read_html("https://www150.statcan.gc.ca/n1/en/catalogue/71M0001X")
    hrefs    <- rvest::html_attr(rvest::html_elements(cat_page, "a"), "href")
    m        <- stringr::str_match(hrefs, "71M0001X(\\d{4})(\\d{3})$")
    m        <- m[!is.na(m[, 1L]), , drop = FALSE]
    year     <- as.integer(m[, 2L])
    month    <- as.integer(m[, 3L])
    eft      <- year < 2006L & month >= 1L & month <= 12L
    tibble::tibble(
      Acronym = "LFS",
      Version = paste0(m[eft, 2L], "-",
                        formatC(month[eft], width = 2L, flag = "0")),
      url     = "(EFT)"
    ) |> distinct()
  }, error = function(e) {
    tibble::tibble(Acronym = character(0L), Version = character(0L),
                   url     = character(0L))
  })
  lfs_versions <- bind_rows(lfs_versions, lfs_eft_versions)

  sfs_versions <- tibble(Acronym="SFS",
                         Version=c("1999","2005","2012","2016","2019","2023"),
                         url=c("https://www150.statcan.gc.ca/n1/pub/13m0006x/2021001/SFS1999-eng.zip",
                               "https://www150.statcan.gc.ca/n1/pub/13m0006x/2021001/SFS2005-eng.zip",
                               "https://www150.statcan.gc.ca/n1/pub/13m0006x/2021001/SFS2012-eng.zip",
                               "https://www150.statcan.gc.ca/n1/pub/13m0006x/2021001/SFS2016-eng.zip",
                               "https://www150.statcan.gc.ca/n1/pub/13m0006x/2021001/SFS2019__PUMF_E.zip",
                               "https://www150.statcan.gc.ca/n1/pub/13m0006x/2021001/SFS2023-eng.zip"))
  cis_versions <- tibble(Acronym="CIS",
                         Version=c("2022", "2021", "2020", "2019", "2018", "2017"),
                         url=c("https://www150.statcan.gc.ca/n1/pub/72m0003x/2024001/2022.zip",
                               "https://www150.statcan.gc.ca/n1/en/pub/72m0003x/2024001/2021.zip",
                               "https://www150.statcan.gc.ca/n1/en/pub/72m0003x/2023002/2020.zip",
                               "https://www150.statcan.gc.ca/n1/en/pub/72m0003x/2021001/2019.zip",
                               "https://www150.statcan.gc.ca/n1/en/pub/72m0003x/2021001/2018-eng.zip",
                               "https://www150.statcan.gc.ca/n1/en/pub/72m0003x/2019001/2017-eng.zip"))

  gss_versions <- tibble(Acronym="GSS",
                             Version=c("1996", "2007", "2012", "2018"),
                             url=c("https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat3/c11_1996.zip",
                                   "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat3/c21_2007.zip",
                                   "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat3/c26_2012.zip",
                                   "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat3/c32_2018.zip"),
                             Title="General Social Survey - Caregiving and Care Receiving",
                             `Survey Number`='4502'
  )
  sgvp_versions <- tibble(Acronym="SGVP",
                          Version=c("1997", "2000", "2004", "2007", "2010", "2013", "2018", "2023"),
                          url=c("https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat5/NSGVP-ENDBP_1997.zip",
                                "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat5/NSGVP-ENDBP_2000.zip",
                                "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat5/CSGVP-ECDBP_2004.zip",
                                "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat5/CSGVP-ECDBP_2007.zip",
                                "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat5/CSGVP-ECDBP_2010.zip",
                                "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat5/c27_2013.zip",
                                "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat5/c33_2018.zip",
                                "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat5/GVP_DBP_2023.zip"),
                          Title="General Social Survey - Giving, Volunteering and Participating",
                          `Survey Number`='4430'
  )

  census_download <- list_census_collection()

  first_year <- census_download$Version |> str_extract("\\d{4}") |> as.integer() |> min()
  last_eft_year <- first_year - 5

  if (nrow(pumf_surveys)>0) {
    result <- pumf_surveys %>%
      left_join(bind_rows(lfs_versions,its_versions,sfs_versions,gss_versions,sgvp_versions),
                by="Acronym") %>%
      bind_rows(chs,cpss,shs)
  } else {
    result <- bind_rows(chs,cpss,shs,ccahs) |>
      bind_rows(lfs_versions |> mutate(Title="Labour Force Survey",`Survey Number`="3701"),
                its_versions |> mutate(Title="International Travel Survey",`Survey Number`='3152'),
                sfs_versions |> mutate(Title="Survey of Financial Securities",`Survey Number`='2620'),
                cis_versions |> mutate(Title="Canadian Income Survey",`Survey Number`='5200'),
                gss_versions,
                sgvp_versions,
                tibble(Title="Census of population",Acronym="Census",`Survey Number`="3901",
                       Version=paste0(seq(1971,last_eft_year,5)," (individuals)"),
                       url="(EFT)"),
                # tibble(Title="Census of population",Acronym="Census",`Survey Number`="3901",
                #        Version=paste0(seq(2006,last_eft_year,5)," (hierarchical)"),
                #        url="(EFT)"),
                tibble(Title="Census of population",Acronym="Census",`Survey Number`="3901",
                       Version=paste0(seq(1971,last_eft_year,5)," (households)"),
                       url="(EFT)"),
                tibble(Title="Census of population",Acronym="Census",`Survey Number`="3901",
                       Version=paste0(c(1971L, 1976L, seq(1986L, pmin(1996L, last_eft_year), 5L)), " (families)"),
                       url="(EFT)"))
  }
  result |>
    bind_rows(census_download)
}


#' List available LFS PUMF versions
#'
#' Scrapes the Statistics Canada LFS PUMF publication page and returns a
#' tibble of all available annual and monthly versions with their download
#' URLs.  Requires an internet connection.  For the broader collection of all
#' supported surveys see [list_canpumf_collection()].
#'
#' @return A tibble with columns `Date` (human-readable label from the StatCan
#'   page), `version` (a string of the form `"YYYY"` for annual versions or
#'   `"YYYY-MM"` for monthly versions), and `url` (direct download link).
#'
#' @seealso [get_pumf()], [list_canpumf_collection()]
#'
#' @examples
#' \dontrun{
#' lfs_versions <- list_available_lfs_pumf_versions()
#' tail(lfs_versions)
#' }
#' @export
list_available_lfs_pumf_versions <- function(){
  base_url <- "https://www150.statcan.gc.ca/n1/pub/71m0001x/"
  url <- paste0(base_url,"71m0001x2021001-eng.htm")
  ts <- rvest::read_html(url) %>%
    rvest::html_elements("a")
  ts <- ts[rvest::html_text(ts)=="CSV"]

  lct <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")

  d <- tibble::tibble(
    url  = paste0(base_url, rvest::html_attr(ts,"href")),
    Date = gsub(" \\| PUMF: CSV","", rvest::html_attr(ts,"title"))) |>
    mutate(version = case_when(
      grepl("^\\d{4}$",.data$Date) ~ .data$Date,
      TRUE ~ strftime(as.Date(paste0("01 ",.data$Date),format="%d %B %Y"),"%Y-%m"))) |>
    select("Date", "version", "url")

  Sys.setlocale("LC_TIME", lct)
  d
}
