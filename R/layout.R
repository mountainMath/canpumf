ensure_layout_data_parsed <- function(pumf_base_path,layout_mask=NULL){
  path <- file.path(pumf_clean_layout_dir(pumf_base_path,layout_mask),"var.Rds")
  if (!file.exists(path)) {
    vars <- dir(pumf_base_path,pattern="variables\\.csv")
    if (length(vars)==1) {
      parse_pumf_metadata_csv(pumf_base_path)
    } else {
      parse_pumf_metadata_spss(pumf_base_path,layout_mask)
    }
  }
}


#' Read PUMF variable labels
#'
#' @param pumf_base_path pumf base path
#' @param layout_mask optional path or mask for the layout file in case there are several,
#'
#' @return tibble with variable labels
#' @export
read_pumf_var_labels <- function(pumf_base_path,layout_mask=NULL){
  if (is.data.frame(pumf_base_path)) {
    layout_mask <- attr(pumf_base_path,"layout_mask")
    pumf_base_path <- attr(pumf_base_path,"pumf_base_path")
  }
  if (is.data.frame(pumf_base_path)) {
    pumf_base_path <- attr(pumf_base_path,"pumf_base_path")
    if (is.null(layout_mask)) layout_mask <- attr(pumf_base_path,"layout_mask")
  }
  #ensure_layout_data_parsed(pumf_base_path,layout_mask)
  path <- file.path(pumf_clean_layout_dir(pumf_base_path,layout_mask),"var.Rds")
  readRDS(path)
}

#' Read PUMF missing data information
#'
#' @param pumf_base_path pumf base path
#' @param layout_mask optional path or mask for the layout file in case there are several,
#'
#' @return tibble with missing data information
#' @export
read_pumf_miss_labels <- function(pumf_base_path,layout_mask=NULL){
  if (is.data.frame(pumf_base_path)) {
    layout_mask <- attr(pumf_base_path,"layout_mask")
    pumf_base_path <- attr(pumf_base_path,"pumf_base_path")
  }
  if (is.data.frame(pumf_base_path)) {
    pumf_base_path <- attr(pumf_base_path,"pumf_base_path")
    if (is.null(layout_mask)) layout_mask <- attr(pumf_base_path,"layout_mask")
  }
  #ensure_layout_data_parsed(pumf_base_path,layout_mask)
  path <- file.path(pumf_clean_layout_dir(pumf_base_path,layout_mask),"miss.Rds")
  readRDS(path)
}


#' Get PUMF value labels
#'
#' @param pumf_base_path pumf base path
#' @param layout_mask optional path or mask for the layout file in case there are several,
#'
#' @return tibble with value labels
#' @export
read_pumf_val_labels <- function(pumf_base_path,layout_mask=NULL){
  if (is.data.frame(pumf_base_path)) {
    layout_mask <- attr(pumf_base_path,"layout_mask")
    pumf_base_path <- attr(pumf_base_path,"pumf_base_path")
  }
  if (is.data.frame(pumf_base_path)) {
    pumf_base_path <- attr(pumf_base_path,"pumf_base_path")
    if (is.null(layout_mask)) layout_mask <- attr(pumf_base_path,"layout_mask")
  }
  #ensure_layout_data_parsed(pumf_base_path,layout_mask)
  path <- file.path(pumf_clean_layout_dir(pumf_base_path,layout_mask),"val.Rds")
  read_rds(path)
}

