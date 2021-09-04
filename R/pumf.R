

#' Guess which columns in pumf data are numeric
#'
#' @param pumf_base_path pumf base path
#' @param layout_mask optional layout mask in case there are several layout files,
#' @param numeric_pattern optional pattern to guess numeric columns from their \code{NA} value range
#'
#' @return vector of column names
#' @export
guess_numeric_pumf_columns <- function(pumf_base_path,
                                       layout_mask=NULL,
                                       numeric_pattern = "\\d+-\\d+|THRU 99"){
  miss_data <- read_pumf_miss_labels(pumf_base_path,layout_mask)
  if (nrow(miss_data)==0) {
    val_labels <- read_pumf_val_labels(pumf_base_path)
    val_labels %>% filter(grepl(numeric_pattern,.data$val)) %>% pull(.data$name)
  }
  else  miss_data %>%
    filter(grepl(numeric_pattern,.data$missing)) %>%
    pull(.data$name)
}


#' Rename to human readable column names
#'
#' @param pumf_data pumf data file
#' @param pumf_base_path optional base path, guessed from attributes on \code{pumf_data}
#' @param layout_mask optional layout mask in case there are several layout files,
#' guessed from attributes on \code{layout_mask}
#'
#' @return data frame with renamed columns
#' @export
label_pumf_columns <- function(pumf_data,
                            pumf_base_path=attr(pumf_data,"pumf_base_path"),
                            layout_mask=attr(pumf_data,"layout_mask")){
  var_labels <- read_pumf_var_labels(pumf_base_path,layout_mask)
  vars <- pumf_data %>% names() %>% intersect(var_labels$name)
  vr <- var_labels %>% filter(.data$name %in% vars)
  pumf_data %>% rename(!!!setNames(vr$name,vr$label))

}

#' Add variable labels and rename to human readable column names
#'
#' @param pumf_data pumf data file
#' @param pumf_base_path optional base path, guessed from attributes on \code{pumf_data}
#' @param layout_mask optional layout mask in case there are several layout files,
#' guessed from attributes on \code{layout_mask}
#'
#' @return relabeled data frame
#' @export
label_pumf_data <- function(pumf_data,
                            pumf_base_path=attr(pumf_data,"pumf_base_path"),
                            layout_mask=attr(pumf_data,"layout_mask")){
  val_labels <- read_pumf_val_labels(pumf_base_path,layout_mask)
  var_labels <- read_pumf_var_labels(pumf_base_path,layout_mask)
  vars <- pumf_data %>% select_if(function(d)!is.numeric(d)) %>% names() %>% intersect(var_labels$name)
  for (var in vars) {
    vl <- val_labels %>%
      filter(.data$name==var) %>%
      filter(!grepl("^\\d+-\\d+$|^\\d+-$",.data$val))
    if (nrow(vl)==1 && vl$val[1]=="blank") vl <- vl %>% filter(.data$val!="blank")
    lookup <- setNames(vl$label,vl$val)
    if (length(lookup)>0) {
      missed <- pumf_data %>% pull(var) %>% unique %>% setdiff(vl$val)
      if (any(is.na(missed)) && 'blank' %in% names(lookup)) {
        pumf_data <- pumf_data %>%
          mutate_at(var,function(d)coalesce(d,"blank"))
        missed <- setdiff(missed,NA)
      }
      if (length(missed)==0) {
       pumf_data <- pumf_data %>%
          mutate_at(var,function(d)factor(recode(d,!!!lookup),levels=unique(vl$label)))
      } else {
        pumf_data <- pumf_data %>%
          mutate_at(var,function(d)recode(d,!!!lookup))
      }
    }
  }

  label_pumf_columns(pumf_data=pumf_data,
                     pumf_base_path=pumf_base_path,
                     layout_mask=layout_mask)
}

#' Convert columns to numeric and convert all missing values to \code{NA}
#'
#' @param pumf_data pumf data file
#' @param pumf_base_path optional base path, guessed from attributes on \code{pumf_data}
#' @param layout_mask optional layout mask in case there are several layout files,
#' guessed from attributes on \code{layout_mask}
#' @param numeric_columns optional list of columns to conver, guessed from data if none provided
#' @param set_missing logical, will set missing values to \code{NA} if set to \code{TRUE} (default)
#' @param na.values optional vectors of values to convert to \code{NA}
#'
#' @return data frame with converted values
#' @export
convert_pumf_numeric_columns <- function(pumf_data,
                                         pumf_base_path=attr(pumf_data,"pumf_base_path"),
                                         layout_mask=attr(pumf_data,"layout_mask"),
                                         numeric_columns=NULL,
                                         set_missing=TRUE,
                                         na.values = c(".")){
  if (is.null(pumf_base_path)) stop("Could not find PUMF base path to access metadata.")
  if (is.null(numeric_columns))  numeric_columns <- guess_numeric_pumf_columns(pumf_base_path,layout_mask)

  miss_data <- read_pumf_miss_labels(pumf_base_path,layout_mask)
  missing <- setdiff(numeric_columns,miss_data$name)

  if (length(missing)>0 & nrow(miss_data)>0) {
    warning(paste0("Don't have missing value information for column(s) ",paste0(missing,collapse = ", "),"."))
  }

  convert_numeric_missing <- function(l,c) {
      miss <- miss_data %>% filter(.data$name==c)
      integer_miss <- !grepl(miss$missing,"\\.") & max(nchar(l)) < 10

      if (integer_miss) {
        nv <- NA_integer_
        nf <- as.integer
      } else {
        nv <- NA_real_
        nf <- as.numeric
      }

      for (n in na.values) l[l==n] <- nv
      l <- nf(l)
      if (nrow(miss)==1) l[l>=miss$missing_low & l<=miss$missing_high] <- nv
    l
  }

  for (c in numeric_columns) {
    if (nrow(miss_data)==0) {
      pumf_data <- pumf_data %>%
        mutate_at(c,as.numeric)
    } else {
      pumf_data <- pumf_data %>%
        mutate_at(c,~convert_numeric_missing(!!as.name(c),c))
    }
  }
  pumf_data
}




#' Parse PUMF data
#'
#' @param pumf_base_path optional base path, guessed from attributes on \code{pumf_data}
#' @param layout_mask optional layout mask in case there are several layout files,
#' guessed from attributes on \code{layout_mask}
#' @param file_mask optional additional mask to filter down to specific PUMF file if there are several
#' @param guess_numeric logical, will guess numeric columns and covert to numeric and set missing values
#' to \code{NA} if set to \code{TRUE} (default)
#'
#' @return data frame with one row for each case in the PUMF data
#' @export
read_pumf_data <- function(pumf_base_path,
                          layout_mask=NULL,
                          file_mask=layout_mask,
                          guess_numeric=TRUE){
  ## cgeck if data is csv
  vars <- dir(pumf_base_path,pattern="variables\\.csv")
  if (length(vars)==1) {
    data_path <- dir(pumf_base_path,pattern="\\.csv")
    data_path <- file.path(pumf_base_path,data_path[data_path!=vars])
    pumf_data <- readr::read_csv(data_path,
                                 locale=readr::locale(encoding = "Latin1"),
                                 col_types = readr::cols(.default = "c"))
  } else {
    layout<- read_pumf_layout(pumf_base_path,layout_mask)
    data_dir <- pumf_data_dir(pumf_base_path)
    data_path <- dir(data_dir,"\\.txt$")
    if (length(file_mask)>0) data_path <- data_path[grepl(file_mask,data_path)]
    if (length(data_path)==0) {
      data_path <- dir(data_dir,"\\.dat$")
      if (length(file_mask)>0) data_path <- data_path[grepl(file_mask,data_path)]
    }
    if (length(data_path)==0) stop("Could not find PUMF data file")
    if (length(data_path)>1) {
      message("Found multiple PUMF data files, reading all.")
      pumf_data <- data_path %>%
        lapply(function(path){
          read_fwf(file.path(data_dir,path),
                   col_positions = fwf_positions(layout$start,layout$end,col_names = layout$name),
                   col_types=cols(.default = "c"),
                   locale=locale(encoding = "Latin1")) %>%
            mutate(path=!!path)
        }) %>%
        bind_rows()
    } else {
      pumf_data <- readr::read_fwf(file.path(data_dir,data_path),
                            col_positions = readr::fwf_positions(layout$start,layout$end,col_names = layout$name),
                            col_types=cols(.default = "c"),
                            locale=locale(encoding = "Latin1"))
    }
  }

  attr(pumf_data,"pumf_base_path") <- pumf_base_path
  attr(pumf_data,"layout_mask") <- layout_mask

  if (guess_numeric) pumf_data <- pumf_data %>% convert_pumf_numeric_columns()

  pumf_data
}

#' Download PUMF data
#'
#' @param path Download path for PUMF SPSS data
#' @param destination_dir Optional path where to store the extracted PUMF data, default is `file.path(tempdir(),"pumf")`
#' @param timeout Optional parameter to specify connection timout for download
#' @return pumf_base_dir that can be used in the other package functions
#' @export
download_pumf <- function(path,destination_dir=file.path(tempdir(),"pumf"),timeout=3000){
  if (!dir.exists(destination_dir)||length(dir(destination_dir))==0) {
    message("Downloading PUMF data.")
    if (!dir.exists(destination_dir)) dir.create(destination_dir)
    tmp <- tempfile(fileext = '.zip')
    old_tmp <- getOption("timeout")
    options("timeout"=timeout)
    unzip_option=getOption("unzip")
    if (is.null(unzip_option)) unzip_option <- "internal"
    utils::download.file(path,tmp)
    options("timeout"=old_tmp)
    ls <- utils::unzip(tmp,exdir = destination_dir, unzip=unzip_option)
  } else {
    message("Path already exists, using cached data.")
  }
  new_dir <- file.path(destination_dir,"/SPSS")
  if (dir.exists(new_dir)) destination_dir<-new_dir
  destination_dir
}

#' Download PUMF LFS data
#'
#' @param version A version of the pumf data of fornat <Year>-<Month>, e.g. "2021-01"
#' @param destination_dir Optional path where to store the extracted PUMF data, default is `file.path(tempdir(),"pumf")`
#' @param timeout Optional parameter to specify connection timout for download
#' @return pumf_base_dir that can be used in the other package functions
#' @export
download_lfs_pumf <- function(version="2021-01",destination_dir=file.path(tempdir(),"pumf"),timeout=3000){
  if (!dir.exists(destination_dir)) dir.create(destination_dir)
  destination_dir <- file.path(destination_dir,paste0("lfs_",version,"-CSV"))
  url <- paste0("https://www150.statcan.gc.ca/n1/pub/71m0001x/2021001/",version,"-CSV.zip")
  download_pumf(url,destination_dir = destination_dir,timeout = timeout)
}


#' Add bootstrap weights to PUMF data
#'
#' @param pumf_data A dataframe with PUMF data
#' @param weight_column Name of the column with the standard weights
#' @param bootstrap_weight_count Number of boostrap weights to generate
#' @param bootstrap_weight_prefix Name prefix for the bootstrap weight columns
#' @param seed Random see to be used for bootstrap sample for reproducibility
#' @return pumf_base_dir that can be used in the other package functions
#' @export
add_bootstrap_weights <- function(pumf_data,
                                  weight_column,
                                  bootstrap_weight_count = 16,
                                  bootstrap_weight_prefix="WT",
                                  seed=NULL){
  n <- nrow(pumf_data)
  set.seed(seed)
  for (i in seq(1,bootstrap_weight_count)) {
    bootstrap_weight_column <- paste0(bootstrap_weight_prefix,i)
    wt <- dplyr::tibble(!!bootstrap_weight_column:=pull(pumf_data,weight_column)) %>%
      dplyr::mutate(rn=row_number()) %>%
      dplyr::left_join(tibble(rn=sample(seq(1,n),n,replace=TRUE)) %>%
                  dplyr::count(.data$rn), by="rn") %>%
      dplyr::mutate(n=coalesce(.data$n,0)) %>%
      dplyr::mutate(!!bootstrap_weight_column:=!!as.name(bootstrap_weight_column)*.data$n) %>%
      dplyr::select(bootstrap_weight_column)
    pumf_data <- pumf_data %>%
      dplyr::bind_cols(wt)
  }
  pumf_data
}
