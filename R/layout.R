# ensure_layout_data_parsed() retired: called the old SPSS/CSV parsers
# (layout_spss.R, layout_csv.R) which have been deleted. The new pipeline
# uses pumf_parse_metadata() → canonical metadata/variables.csv instead.


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
#' @param numeric_only options, only returns missing data for numeric columns, defaul is `TRUE`.
#'
#' @return tibble with missing data information
#' @export
read_pumf_miss_labels <- function(pumf_base_path,layout_mask=NULL, numeric_only=TRUE){
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
  if (file.exists(path)) {
    miss <- readRDS(path)
    val <- read_pumf_val_labels(pumf_base_path)
    miss <- miss |> filter(!(.data$name %in% unique(val$name)))
  } else {
    miss <- tibble::tibble()
  }
  miss
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

