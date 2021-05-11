pumf_data_dir <- function(pumf_base_path){
  data_dir <- dir(pumf_base_path,"*Data*")
  if (length(data_dir)==0) stop("Could not find data directory in PUMF base path, aborting.")
  if (length(data_dir)>1) {
    data_dir <- data_dir[!grepl("dictionary",data_dir)]
    if (length(data_dir)>1) stop("Found multiple data directories in PUMF base path, aborting.")
  }
  file.path(pumf_base_path,data_dir)
}

pumf_layout_dir <- function(pumf_base_path){
  layout_dir <- dir(pumf_base_path,"Layout|Syntax|Command")
  if (length(layout_dir)==0||!dir.exists(file.path(pumf_base_path,layout_dir))){
    data_dir <- dir(pumf_base_path,"*Data*")
    if (length(data_dir)==0) stop("Could not find layout or data in PUMF base path, aborting.")
    data_path <- file.path(pumf_base_path,data_dir)
    layout_dir <- dir(data_path,"Layout|Syntax")
    if (length(layout_dir)==0) stop("Could not find layout in PUMF base path, aborting.")
    layout_dir <- file.path(data_path,layout_dir)
  } else {
    layout_dir <- file.path(pumf_base_path,layout_dir)
  }
  spss_path <- dir(layout_dir,"SPSS")
  if (length(spss_path)==0) stop("Could not find layout in PUMF base path, aborting.")
  if (!grepl(spss_path,"\\.sps$")) layout_dir<-file.path(layout_dir,spss_path)
  layout_dir
}


slice_raw_layout_data <- function(r){
  start_line <- which(grepl(" LABELS| VALUES",r$value))+1
  end_line <- which(grepl(" +\\.",r$value))-1
  r %>%
    slice(seq(start_line,end_line))
}

tmp_layout_path <- function(pumf_base_path,...) {
  file.path(tempdir(),paste0("canpumf_",digest::digest(pumf_base_path),"_",paste0(...,collapse="_"),".Rds"))
}


find_unique_layout_file <- function(layout_path,pattern,path_or_pattern=NULL){
  validate_path <- function(path){
    if (length(path)==0) stop("Could not find layout file.")
    if (length(path)>1) stop(paste0("Found several layout files: ",paste0(path,collapse=", "),".\n",
                                    "Please further specify which layout file to use."))
    NULL
  }

  path <- path_or_pattern
  if (is.null(path)) {
    path <- dir(layout_path,pattern=pattern)
    validate_path(path)
    path<-file.path(layout_path,path)
  } else {
    if (file.exists(file.path(layout_path,path))) path <- file.path(layout_path,path)
    if (!file.exists(path)) {
      paths <- dir(layout_path)
      paths <- paths[grepl(path,paths)]
      if (length(paths)>1) paths <- paths[grepl(pattern,paths)]
      validate_path(paths)
      path<-file.path(layout_path,paths)
    }
  }
  path
}


#' @import dplyr
#' @importFrom stats setNames
#' @import stringr
#' @import readr
#' @importFrom rlang .data
#' @importFrom rlang :=
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
