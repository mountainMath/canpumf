# parsing CHS pumf

get_chs_pumf <- function(pumf_version,pumf_cache_path) {
  chs_base_path <- file.path(pumf_cache_path,"CHS")
  pumf_base_path <- file.path(chs_base_path,pumf_version)
  if (!dir.exists(pumf_base_path)) {
    available_chs_versions <- list_canpumf_collection() |>
      dplyr::filter(Acronym=="CHS",Version==pumf_version)

    if (nrow(available_chs_versions)==0) {
      stop("CHS version ",pumf_version," is not available, check available CHS PUMF versions via `list_canpumf_collection()`")
    }

    dir.create(pumf_base_path)
  }

  if (length(dir(pumf_base_path))==0) {
    download_pumf(available_chs_versions$url,pumf_base_path)
  }

  if (length(dir(pumf_base_path))==1) {
    pumf_base_path <- dir(pumf_base_path,full.names=TRUE)
  }

  canpumf:::parse_pumf_metadata_spss(pumf_base_path,"CHS2021ECL_PUMF")
  canpumf:::parse_pumf_metadata_spss(pumf_base_path,"chs2021ecl_PUMF_bsw")

  pumf_data_path <- dir(file.path(pumf_base_path,"Data"),"CHS2021ECL_PUMF\\.csv",full.names = TRUE)

  pumf_data <- readr::read_csv(pumf_data_path,locale = readr::locale(encoding = "CP1252"),
                               col_types=readr::cols(PFWEIGHT="n",.default = "c"))

  bsw_data_path <- dir(file.path(pumf_base_path,"Data"),"chs2021ecl_PUMF_bsw\\.csv",
                       ignore.case = TRUE,full.names = TRUE)

  bsw_data <- readr::read_csv(bsw_data_path,locale = readr::locale(encoding = "CP1252"),
                              col_types=readr::cols(PUMFID="c",.default = "n"))

  pumf_data <- pumf_data |>
    left_join(bsw_data,by="PUMFID")


  attr(pumf_data,"pumf_base_path") <- pumf_base_path
  attr(pumf_data,"layout_mask") <- "CHS2021ECL_PUMF"

  pumf_data
}
