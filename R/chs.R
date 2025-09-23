# parsing CHS pumf

get_chs_pumf <- function(pumf_version,pumf_cache_path) {
  chs_base_path <- file.path(pumf_cache_path,"CHS")
  if (!dir.exists(chs_base_path)) dir.create(chs_base_path)
  pumf_base_path <- file.path(chs_base_path,pumf_version)
  if (!dir.exists(pumf_base_path) || length(dir(pumf_base_path))==0) {
    available_chs_versions <- list_canpumf_collection() |>
      dplyr::filter(.data$Acronym=="CHS",.data$Version==pumf_version)

    if (nrow(available_chs_versions)==0) {
      stop("CHS version ",pumf_version," is not available, check available CHS PUMF versions via `list_canpumf_collection()`")
    }

    if (!dir.exists(pumf_base_path)) dir.create(pumf_base_path)
    download_pumf(available_chs_versions$url,pumf_base_path)
  }


  if (length(dir(pumf_base_path))==1) {
    pumf_base_path <- dir(pumf_base_path,full.names=TRUE)
  }

  if (pumf_version=="2021") {
    layout_mask <- "CHS2021ECL_PUMF"
    bsw_layout_mask <- "chs2021ecl_PUMF_bsw"
    pumf_data_path <- dir(file.path(pumf_base_path,"Data"),"CHS2021ECL_PUMF\\.csv",full.names = TRUE)
    bsw_data_path <- dir(file.path(pumf_base_path,"Data"),"chs2021ecl_PUMF_bsw\\.csv",
                         ignore.case = TRUE,full.names = TRUE)
  } else if (pumf_version=="2022") {
    layout_mask <- "chs2022ecl_pumf"
    bsw_layout_mask <- "chs2022ecl_pumf_bsw"
    pumf_data_path <- file.path(pumf_base_path,"Chs2022ecl_pumf.csv")
    bsw_data_path <- file.path(pumf_base_path,"Chs2022ecl_pumf_bsw.csv")
  } else {
    warning("Untested pumf version")
    layout_mask <- "CHS2018ECL_PUMF"
    bsw_layout_mask <- "chs2018ecl_PUMF_bsw"
    pumf_data_path <- dir(file.path(pumf_base_path,"Data"),"CHS2018ECL_PUMF\\.csv",full.names = TRUE)
    bsw_data_path <- dir(file.path(pumf_base_path,"Data"),"chs2018ecl_PUMF_bsw\\.csv",
                         ignore.case = TRUE,full.names = TRUE)
  }

  parse_pumf_metadata_spss(pumf_base_path,layout_mask)
  parse_pumf_metadata_spss(pumf_base_path,bsw_layout_mask)

  pumf_data <- readr::read_csv(pumf_data_path,locale = readr::locale(encoding = "CP1252"),
                               col_types=readr::cols(PFWEIGHT="n",.default = "c"))


  bsw_data <- readr::read_csv(bsw_data_path,locale = readr::locale(encoding = "CP1252"),
                              col_types=readr::cols(PUMFID="c",.default = "n"))

  pumf_data <- pumf_data |>
    left_join(bsw_data,by="PUMFID")


  attr(pumf_data,"pumf_base_path") <- pumf_base_path
  attr(pumf_data,"layout_mask") <- layout_mask

  pumf_data
}
