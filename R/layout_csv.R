#' Parse PUMF medatadata
#'
#' @param pumf_base_path path for pumf data
#'
#' @return NULL
#' @export
parse_pumf_metadata_csv <- function(pumf_base_path){
  vars <- dir(pumf_base_path,pattern="variables\\.csv")
  if (length(vars)==1) {
    data <- suppressWarnings(readr::read_csv(file.path(pumf_base_path,vars),
                                             locale=readr::locale(encoding = "Latin1"),
                                             col_types = readr::cols(.default="c"))) %>%
      setNames(gsub(" /.+$","",names(.)))
    var_labels <- data %>%
      select(name=.data$Variable,
             label=.data$`Variable Name - English`) %>%
      filter(!is.na(.data$name)) %>%
      mutate_at("name",toupper) %>%
      mutate(dup = .data$label %in% .data$label[duplicated(.data$label)]) %>%
      group_by(.data$label,.data$dup) %>%
      mutate(label=ifelse(.data$dup,paste0(.data$label," (",.data$name,")"),.data$label)) %>%
      ungroup() %>%
      select(-.data$dup)
  } else {
    stop("Could not find medatata")
  }

  saveRDS(var_labels,file.path(pumf_clean_layout_dir(pumf_base_path),"var.Rds"))

  miss <- tibble::tibble(name=NA_character_,missing=NA_character_,
                         missing_low=NA_real_,missing_high=NA_real_)  %>%
    slice(-1)
  saveRDS(miss,file.path(pumf_clean_layout_dir(pumf_base_path),"miss.Rds"))


  val_labels <- data %>%
    dplyr::select(name=.data$Variable,
                  val=.data$Code,
                  label=.data$`Label - English`) %>%
    dplyr::filter(!is.na(.data$val)|!is.na(.data$name)) %>%
    tidyr::fill(.data$name,.direction="down") %>%
    tidyr::fill(.data$label,.direction="down") %>%
    filter(!is.na(.data$val)) %>%
    mutate_at("name",toupper) %>%
    mutate(val=ifelse(.data$name=="SURVMNTH",stringr::str_pad(.data$val,width=2,side="left",pad="0"),.data$val)) # manual fix for bad LFS coding

  saveRDS(val_labels,file.path(pumf_clean_layout_dir(pumf_base_path),"val.Rds"))

  NULL
}


#' Parse PUMF data
#'
#' @param data_base_path path for raw pumf data
#' @param pumf_path path for ourput pumf data
#'
#' @return tibble with variable labels
#' @export
parse_pumf_data_csv <- function(data_base_path,pumf_path){
  data_path <- dir(data_base_path,pattern="\\.csv")
  data_path <- file.path(data_base_path,data_path[data_path!=vars])
  pumf_data <- readr::read_csv(data_path,
                               locale=readr::locale(encoding = "Latin1"),
                               col_types = readr::cols(.default = "c"))
  write_csv(pumf_data,file.path(pumf_path,"data.csv"))
  pumf_data
}

