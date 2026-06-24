# canpumf collection





list_census_collection <- function() {
  base_url  <- "https://www150.statcan.gc.ca/n1/pub/98m0001x/index-eng.htm"
  pumf_data <- tryCatch(
    rvest::read_html(base_url) |> rvest::html_nodes("main div ul li a"),
    error = function(e) NULL
  )
  if (!is.null(pumf_data)) {
    tibble::tibble(Title = "Census of population",
           Acronym = "Census",
           Version = pumf_data |> rvest::html_text(),
           `Survey Number` = "3901",
           url = paste0("https://www150.statcan.gc.ca/n1/pub/98m0001x/",
                        pumf_data |> rvest::html_attr("href"))) |>
      mutate(Year    = stringr::str_extract(.data$Version, "\\d{4}")) |>
      mutate(type    = gsub(" .+", "", .data$Version)) |>
      mutate(Version = paste0(.data$Year, " (", tolower(.data$type), ")")) |>
      select(-"Year", -"type")
  } else {
    .census_collection_fallback()
  }
}

# Hardcoded GSS/SGVP fallback — only the registry-supported surveys so that
# get_pumf() still works for already-cached data when StatCan is unreachable.
.gss_collection_fallback <- function() {
  tibble::tibble(
    Title          = c(rep("General Social Survey - Caregiving", 5L),
                       rep("General Social Survey - Giving",     8L)),
    Acronym        = c(rep("GSS",  5L), rep("SGVP", 8L)),
    `Survey Number` = c(rep("4502", 5L), rep("4430", 8L)),
    Version        = c("Cycle 11 (1996)", "Cycle 16 (2002)", "Cycle 21 (2007)",
                       "Cycle 26 (2012)", "Cycle 32 (2018)",
                       "1997", "2000", "2004", "2007", "2010", "2013", "2018", "2023"),
    url            = c(
      "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat3/c11_1996.zip",
      # Cycle 16 ("Aging and Social Support", 2002); StatCan files it under the
      # Education category (cat9) and mislabels it "Education 2002".
      "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat9/c16_2002.zip",
      "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat3/c21_2007.zip",
      "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat3/c26_2012.zip",
      "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat3/c32_2018.zip",
      "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat5/NSGVP-ENDBP_1997.zip",
      "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat5/NSGVP-ENDBP_2000.zip",
      "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat5/CSGVP-ECDBP_2004.zip",
      "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat5/CSGVP-ECDBP_2007.zip",
      "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat5/CSGVP-ECDBP_2010.zip",
      "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat5/c27_2013.zip",
      "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat5/c33_2018.zip",
      "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat5/GVP_DBP_2023.zip"
    )
  )
}

# Hardcoded Census versions used when StatCan is unreachable.
# URLs are best-effort; they work for 2016/2021 (both use catalog 98m0001x)
# but may be stale for older years.  If StatCan is unreachable the download
# would fail regardless, so accuracy of the URL is moot in that scenario.
.census_collection_fallback <- function() {
  tibble::tibble(
    Title          = "Census of population",
    Acronym        = "Census",
    `Survey Number` = "3901",
    Version = c(
      "2021 (individuals)", "2021 (hierarchical)",
      "2016 (individuals)", "2016 (hierarchical)",
      "2011 (individuals)", "2011 (hierarchical)",
      "2006 (individuals)", "2006 (hierarchical)",
      "2001 (individuals)", "2001 (households)", "2001 (families)",
      "1996 (individuals)", "1996 (households)", "1996 (families)",
      "1991 (individuals)", "1991 (households)", "1991 (families)"
    ),
    url = c(
      "https://www150.statcan.gc.ca/n1/pub/98m0001x/2022001/cen21_ind_98m0001x_part_rec21.zip",
      "https://www150.statcan.gc.ca/n1/pub/98m0001x/2022001/cen21_hier_98M0001X_rec21_hier.zip",
      "https://www150.statcan.gc.ca/n1/pub/98m0001x/2017001/cen16_ind_98m0001x_part_rec16.zip",
      "https://www150.statcan.gc.ca/n1/pub/98m0001x/2017001/cen16_hier_98m0002x_rec16_hier.zip",
      "https://www150.statcan.gc.ca/n1/pub/99m0001x/2013001/nhs11_ind_99m0001x_part_enm11.zip",
      "https://www150.statcan.gc.ca/n1/pub/99m0001x/2013001/nhs11_hier_99m0002x_enm11_hier.zip",
      "https://www150.statcan.gc.ca/n1/pub/95m0028x/2009001/cen06_ind_95m0028x_part_rec06.zip",
      "https://www150.statcan.gc.ca/n1/pub/95m0028x/2009001/cen06_hier_95m0029x_part_rec06.zip",
      "https://www150.statcan.gc.ca/n1/pub/95m0016x/2003001/cen01_ind_95m0016x_part_rec01.zip",
      "https://www150.statcan.gc.ca/n1/pub/95m0016x/2003001/cen01_hous_95m0020x_mena_rec01.zip",
      "https://www150.statcan.gc.ca/n1/pub/95m0016x/2003001/cen01_fam_95m0018x_fam_rec01.zip",
      "https://www150.statcan.gc.ca/n1/pub/95m0010x/1999001/cen96_ind_95m0010X_part_rec96_v2.zip",
      "https://www150.statcan.gc.ca/n1/pub/95m0010x/1999001/cen96_hous_95m0011x_mena_rec96_v2.zip",
      "https://www150.statcan.gc.ca/n1/pub/95m0010x/1999001/cen96_fam_95m0012x_fam_rec96_v2.zip",
      "https://www150.statcan.gc.ca/n1/pub/95m0007x/1996001/cen91_ind_95m0007x_ind_rec91.zip",
      "https://www150.statcan.gc.ca/n1/pub/95m0007x/1996001/cen91_hous_95m0008X_mena_rec91.zip",
      "https://www150.statcan.gc.ca/n1/pub/95m0007x/1996001/cen91_fam_95m0009x_fam_rec91.zip"
    )
  )
}

list_pumf_collection <- function(){
  tibble::tibble(Title=NA,Acronym=NA, `Survey Number`=NA) |>
    stats::na.omit()
}

# Scrape all GSS PUMF downloads from the shared catalogue index.
# Returns a tibble with Title, Acronym, Survey Number, Version, url.
#
# Version conventions (to match registry keys):
#   GSS cycles               → Acronym="GSS",  Version="Cycle N (YYYY)" derived
#     from the cNN_ cycle prefix in the zip filename (e.g. c34_2019.zip ->
#     "Cycle 34 (2019)"); TU_ET_2022.zip is the cycle-36 exception.
#   cat5 (Giving/SGVP)       → Acronym="SGVP", Version=plain year e.g. "2023"
list_gss_collection <- function() {
  base_url <- "https://www150.statcan.gc.ca/n1/pub/45-25-0001/"
  page     <- rvest::read_html(paste0(base_url, "index-eng.htm"))

  # Static mapping: cat directory -> survey metadata
  cat_meta <- tibble::tribble(
    ~cat,    ~Title,                                       ~Acronym, ~Survey.Number, ~theme_prefix,
    "cat1",  "General Social Survey - Canadian Safety",   "GSS",    "4504",         "Safety",
    "cat2",  "General Social Survey - Work and Home",     "GSS",    "5221",         "Work and Home",
    "cat3",  "General Social Survey - Caregiving",        "GSS",    "4502",         NA_character_,
    "cat4",  "General Social Survey - Family",            "GSS",    "4501",         "Family",
    "cat5",  "General Social Survey - Giving",            "SGVP",   "4430",         NA_character_,
    "cat6",  "General Social Survey - Social Identity",   "GSS",    "5024",         "Social Identity",
    "cat7",  "General Social Survey - Time Use",          "GSS",    "4503",         "Time Use",
    "cat8",  "General Social Survey - ICT",               "GSS",    "4505",         "ICT",
    "cat9",  "General Social Survey - Education",         "GSS",    "4500",         "Education",
    "cat10", "General Social Survey - Health",            "GSS",    "3894",         "Health"
  )

  zip_nodes <- rvest::html_elements(page, 'a[href$=".zip"]')
  tibble::tibble(
    href    = rvest::html_attr(zip_nodes, "href"),
    year    = trimws(rvest::html_text(zip_nodes))
  ) |>
    dplyr::filter(!is.na(.data$href)) |>
    dplyr::mutate(
      url = paste0(base_url, .data$href),
      cat = stringr::str_extract(.data$href, "^cat\\d+")
    ) |>
    dplyr::left_join(cat_meta, by = "cat") |>
    dplyr::mutate(
      # GSS rows take the canonical "Cycle N (YYYY)" key derived from the zip
      # filename (this also resolves StatCan's mis-filing of the cycle-16
      # "Aging and Social Support" PUMF under the Education category -- its
      # c16_2002.zip filename yields "Cycle 16 (2002)" regardless).  The legacy
      # Giving/Volunteering surveys keep their plain-year SGVP keys.
      Version = dplyr::if_else(
        .data$Acronym == "SGVP",
        .data$year,
        vapply(.data$href, .statcan_gss_version, character(1L),
               edition = NA_character_, USE.NAMES = FALSE)
      )
    ) |>
    dplyr::rename(`Survey Number` = "Survey.Number") |>
    dplyr::select("Title", "Acronym", "Survey Number", "Version", "url")
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
                Version=c("2017","2019","2021","2023"),
                `Survey Number`="3508",
                url=c("https://www150.statcan.gc.ca/n1/en/pub/62m0004x/2017001/SHS_EDM_2017-eng.zip",
                      "https://www150.statcan.gc.ca/n1/en/pub/62m0004x/2017001/SHS_EDM_2019.zip",
                      "https://www150.statcan.gc.ca/n1/pub/62m0004x/2017001/SHS_EDM_2021.zip",
                      "https://www150.statcan.gc.ca/n1/pub/62m0004x/2017001/SHS_EDM_2023.zip"))

  its_versions <- tibble("Acronym"="ITS",
                         Version=c("2019","2018"),
                         url=c("https://www150.statcan.gc.ca/n1/pub/24-25-0002/2021001/2019/SPSS.zip",
                               "https://www150.statcan.gc.ca/n1/pub/24-25-0002/2021001/2018/SPSS.zip"))

  lfs_version_url <- "https://www150.statcan.gc.ca/n1/pub/71m0001x/71m0001x2021001-eng.htm"
  lfs_versions <- tryCatch({
    d <- rvest::read_html(lfs_version_url) %>% rvest::html_nodes(xpath="//a")
    d <- d[rvest::html_text(d)=="CSV"]
    tibble(Acronym="LFS", url=rvest::html_attr(d,"href")) %>%
      mutate(url=ifelse(substr(.data$url,1,4)=="http", url,
                        paste0("https://www150.statcan.gc.ca/n1/pub/71m0001x/", .data$url))) %>%
      mutate(Version=stringr::str_match(.data$url,"\\d{4}-\\d{2}") %>% lapply(first) %>% unlist) %>%
      mutate(Version=coalesce(.data$Version, stringr::str_match(.data$url,"(\\d{4})-CSV")[,2]))
  }, error = function(e) {
    tibble(Acronym=character(), url=character(), Version=character())
  })

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

  gss_all <- tryCatch(
    list_gss_collection(),
    error = function(e) .gss_collection_fallback()
  )

  census_download   <- list_census_collection()
  scrape_failed_lfs <- nrow(lfs_versions) == 0L
  scrape_failed_cen <- identical(census_download, .census_collection_fallback())
  scrape_failed_gss <- identical(gss_all, .gss_collection_fallback())

  if (scrape_failed_lfs || scrape_failed_cen || scrape_failed_gss) {
    what <- c(if (scrape_failed_cen) "Census",
              if (scrape_failed_lfs) "LFS",
              if (scrape_failed_gss) "GSS/SGVP")
    warning("Statistics Canada website unreachable; ",
            paste(what, collapse = " and "),
            " version list(s) are hard-coded and may be incomplete.",
            call. = FALSE)
  }

  first_year <- census_download$Version |> str_extract("\\d{4}") |> as.integer() |> min()
  last_eft_year <- first_year - 5

  if (nrow(pumf_surveys)>0) {
    result <- pumf_surveys %>%
      left_join(bind_rows(lfs_versions,its_versions,sfs_versions,gss_all),
                by="Acronym") %>%
      bind_rows(chs,cpss,shs)
  } else {
    result <- bind_rows(chs,cpss,shs,ccahs) |>
      bind_rows(lfs_versions |> mutate(Title="Labour Force Survey",`Survey Number`="3701"),
                its_versions |> mutate(Title="International Travel Survey",`Survey Number`='3152'),
                sfs_versions |> mutate(Title="Survey of Financial Securities",`Survey Number`='2620'),
                cis_versions |> mutate(Title="Canadian Income Survey",`Survey Number`='5200'),
                gss_all,
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
