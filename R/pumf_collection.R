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
    dplyr::mutate(Year=stringr::str_extract(.data$Version,"\\d{4}")) |>
    dplyr::mutate(type=gsub(" .+","",.data$Version)) |>
    dplyr::mutate(Version=paste0(.data$Year," (",tolower(.data$type),")")) |>
    dplyr::select(-"Year",-"type")
}

#' List StatCan PUMF collection with canpumf wrappers
#'
#' @description A list of pumf collections and versions with convenience wrappers in canpumf.
#'
#' @return a tibble with a list of the PUMF files with canpumf convenience wrappers
#' @export
list_canpumf_collection <- function(){
  canpumf_conveninence_series <- c("LFS","ITS","CPSS","SFS","SHS")
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
                url=c("https://www150.statcan.gc.ca/n1/en/pub/46-25-0001/2021001/2018-eng.zip",
                      "https://www150.statcan.gc.ca/n1/en/pub/46-25-0001/2021001/2021.zip",
                      "https://www150.statcan.gc.ca/n1/en/pub/46-25-0001/2021001/2022.zip"))

  shs <- tibble(Title="Survey of Household Spending",
                Acronym="SHS",
                Version=c("2017","2019"),
                `Survey Number`="3508",
                url=c("https://www150.statcan.gc.ca/n1/en/pub/62m0004x/2017001/SHS_EDM_2017-eng.zip?st=LVOzS5ri",
                      "https://www150.statcan.gc.ca/n1/en/pub/62m0004x/2017001/SHS_EDM_2019.zip?st=KxKAqlFL"))

  its_versions <- tibble("Acronym"="ITS",
                         Version=c("2019","2018"),
                         url=c("https://www150.statcan.gc.ca/n1/pub/24-25-0002/2021001/2019/SPSS.zip",
                               "https://www150.statcan.gc.ca/n1/pub/24-25-0002/2021001/2018/SPSS.zip"))

  lfs_version_url<- "https://www150.statcan.gc.ca/n1/pub/71m0001x/71m0001x2021001-eng.htm"
  d<-rvest::read_html(lfs_version_url) %>%
    rvest::html_nodes(xpath="//a")
  d<-d[rvest::html_text(d)=="CSV"]
  lfs_versions <- tibble(Acronym="LFS",url=rvest::html_attr(d,"href")) %>%
    dplyr::mutate(url=ifelse(substr(.data$url,1,4)=="http",url,paste0("https://www150.statcan.gc.ca/n1/pub/71m0001x/",.data$url))) %>%
    dplyr::mutate(Version=stringr::str_match(.data$url,"\\d{4}-\\d{2}")%>% lapply(first) %>% unlist) %>%
    dplyr::mutate(Version=coalesce(.data$Version,stringr::str_match(.data$url,"(\\d{4})-CSV")[,2]))

  sfs_versions <- tibble(Acronym="SFS",
                         Version=c("1999","2005","2012","2016","2019","2023"),
                         url=c("https://www150.statcan.gc.ca/n1/pub/13m0006x/2021001/SFS1999-eng.zip",
                               "https://www150.statcan.gc.ca/n1/pub/13m0006x/2021001/SFS2005-eng.zip",
                               "https://www150.statcan.gc.ca/n1/pub/13m0006x/2021001/SFS2012-eng.zip",
                               "https://www150.statcan.gc.ca/n1/pub/13m0006x/2021001/SFS2016-eng.zip",
                               "https://www150.statcan.gc.ca/n1/pub/13m0006x/2021001/SFS2019__PUMF_E.zip",
                               "https://www150.statcan.gc.ca/n1/pub/13m0006x/2021001/SFS2023-eng.zip"))

  census_download <- list_census_collection()

  first_year <- census_download$Version |> str_extract("\\d{4}") |> as.integer() |> min()
  last_eft_year <- first_year - 5

  if (nrow(pumf_surveys)>0) {
    result <- pumf_surveys %>%
      left_join(bind_rows(lfs_versions,its_versions,sfs_versions),
                by="Acronym") %>%
      bind_rows(chs,cpss,shs)
  } else {
    result <- bind_rows(chs,cpss,shs,ccahs) |>
      bind_rows(lfs_versions |> mutate(Title="Labour Force Survey",`Survey Number`="3701"),
                its_versions |> mutate(Title="International Travel Survey",`Survey Number`='3152'),
                sfs_versions |> mutate(Title="Survey of Financial Securities",`Survey Number`='2620'),
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
                       Version=paste0(seq(1971,pmin(1996,last_eft_year),5)," (families)"),
                       url="(EFT)"))
  }
  result |>
    bind_rows(census_download)
}
