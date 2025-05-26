# supporting the SFS pumf

get_sfs_pumf <- function(pumf_version,pumf_cache_path) {
  sfs_base_path <- file.path(pumf_cache_path,"SFS")
  pumf_base_path <- file.path(sfs_base_path,pumf_version)
  if (!dir.exists(pumf_base_path)) {
    available_sfs_versions <- list_canpumf_collection() |>
      dplyr::filter(.data$Acronym=="SFS",.data$Version==pumf_version)

    if (nrow(available_sfs_versions)==0) {
      stop("SFS version ",pumf_version," is not available, check available CHS PUMF versions via `list_canpumf_collection()`")
    }

    dir.create(pumf_base_path)
  }

  if (length(dir(pumf_base_path))==0) {
    download_pumf(available_sfs_versions$url,pumf_base_path)
  }

  if (length(dir(pumf_base_path))==1) {
    pumf_base_path <- dir(pumf_base_path,full.names=TRUE)
  }

  parse_pumf_metadata_spss(pumf_base_path,"bsweights")
  parse_pumf_metadata_spss(pumf_base_path,"EFAM_PUMF_R")
  parse_pumf_metadata_spss(pumf_base_path,"EFAM_PUMF_[^R]")


  pumf_data <- read_pumf_data(pumf_base_path=pumf_base_path,layout_mask = "EFAM_PUMF_[^R]",
                              file_mask = "EFAM_PUMF\\.txt") |>
    mutate(across(c("PASRBUYG","PASRDWNG","PASRMPFG"),~str_pad(.x,width=2,side="left",pad="0")))

  bsw_data <- read_pumf_data(pumf_base_path=pumf_base_path,layout_mask = "bsweights",
                             file_mask = "SFS2023_BSWEIGHTS_PUMF\\.txt")
  if ("PWEIGHT" %in% names(bsw_data)) {
    bsw_data <- bsw_data |>
      select(-"PWEIGHT")
  }
  pumf_data <- pumf_data |>
    left_join(bsw_data,by="PEFAMID")


  attr(pumf_data,"pumf_base_path") <- pumf_base_path
  attr(pumf_data,"layout_mask") <- "EFAM_PUMF_[^R]"

  pumf_data
}
