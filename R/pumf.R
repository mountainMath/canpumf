

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
                                       numeric_pattern = "THRU 99"){
  miss_data <- read_pumf_miss_labels(pumf_base_path,layout_mask)

  miss_data %>%
    filter(grepl(numeric_pattern,missing)) %>%
    pull(.data$name)
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
  val_labels <- read_pumf_val_labels(pumf_base_path)
  var_labels <- read_pumf_var_labels(pumf_base_path)
  vars <- pumf_data %>% names() %>% intersect(var_labels$name)
  for (var in vars) {
    vl <- val_labels %>% filter(.data$name==var)
    lookup <- setNames(vl$label,vl$val)
    missed <- pumf_data %>% pull(var) %>% unique %>% setdiff(vl$val)
    if (length(missed)==0) {
      pumf_data <- pumf_data %>%
        mutate(!!as.name(var):=factor(as.character(lookup[!!as.name(var)]),
                                      levels=vl$label))
    } else {
      pumf_data <- pumf_data %>%
        mutate(!!as.name(var):=ifelse(!!as.name(var) %in% names(lookup),
                                      as.character(lookup[!!as.name(var)]),
                                      !!as.name(var)))
    }
  }

  vr <- var_labels %>% filter(.data$name %in% vars)
  pumf_data %>% rename(!!!setNames(vr$name,vr$label))
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

  if (length(missing)>0) {
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
    pumf_data <- pumf_data %>%
      mutate_at(c,~convert_numeric_missing(!!as.name(c),c))
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
  layout<- read_pumf_layout(pumf_base_path,layout_mask)
  data_dir <- pumf_data_dir(pumf_base_path)
  data_path <- dir(data_dir,"\\.txt$")
  if (length(file_mask)>0) data_path <- data_path[grepl(file_mask,data_path)]
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
    pumf_data <- read_fwf(file.path(data_dir,data_path),
                          col_positions = fwf_positions(layout$start,layout$end,col_names = layout$name),
                          col_types=cols(.default = "c"),
                          locale=locale(encoding = "Latin1"))
  }
  attr(pumf_data,"pumf_base_path") <- pumf_base_path
  attr(pumf_data,"layout_mask") <- layout_mask

  if (guess_numeric) pumf_data <- pumf_data %>% convert_pumf_numeric_columns()

  pumf_data
}
