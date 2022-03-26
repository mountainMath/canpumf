#' List StatCan PUMF collection
#'
#' @description While all of these PUMF files are in principle available,
#' not all are available through this package, mostly because there is no public download link.
#'
#' @return a tibble with a list of the StatCan PUMF collection
#' @export
list_pumf_collection <- function(){
  url <- "https://www150.statcan.gc.ca/n1/pub/11-625-x/11-625-x2010000-eng.htm"
  d<-rvest::read_html(url) %>%
    rvest::html_table() %>%
    lapply(function(e){
      if (length(names(e))!=3 || length(setdiff(names(e),c("Title","Acronym", "Survey Number")))>0) e<-NULL
      e
    }) %>%
    dplyr::bind_rows()
}

#' List StatCan PUMF collection with canpumf wrappers
#'
#' @description A list of pumf collections and versions with convenience wrappers in canpumf.
#'
#' @return a tibble with a list of the PUMF files with canpumf convenience wrappers
#' @export
list_canpumf_collection <- function(){
  canpumf_conveninence_series <- c("LFS","ITS","CPSS","SFS")
  pumf_surveys<-list_pumf_collection() %>%
    filter(.data$Acronym %in% canpumf_conveninence_series)

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
                Version=c("2018"),
                `Survey Number`="5269",
                url=c("https://www150.statcan.gc.ca/n1/en/pub/46-25-0001/2021001/2018-eng.zip"))


  its_versions <- tibble("Acronym"="ITS",
                         Version=c("2019","2018"),
                         url=c("https://www150.statcan.gc.ca/n1/pub/24-25-0002/2021001/2019/SPSS.zip",
                               "https://www150.statcan.gc.ca/n1/pub/24-25-0002/2021001/2018/SPSS.zip"))

  lfs_version_url<- "https://www150.statcan.gc.ca/n1/pub/71m0001x/71m0001x2021001-eng.htm"
  d<-rvest::read_html(lfs_version_url) %>%
    rvest::html_nodes(xpath="//a")
  d<-d[rvest::html_text(d)=="CSV"]
  lfs_versions <- tibble(Acronym="LFS",url=rvest::html_attr(d,"href")) %>%
    dplyr::mutate(Version=stringr::str_match(.data$url,"\\d{4}-\\d{2}")%>% lapply(first) %>% unlist)

  sfs_versions <- tibble(Acronym="SFS",
                         Version=c("2019"),
                         url=c("https://www150.statcan.gc.ca/n1/pub/13m0006x/2021001/SFS2019__PUMF_E.zip"))

  pumf_surveys %>%
    left_join(bind_rows(lfs_versions,its_versions,sfs_versions),
              by="Acronym") %>%
    bind_rows(chs,cpss)
}


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
    layout<- read_pumf_layout_spss(pumf_base_path,layout_mask)
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
                            col_types=readr::cols(.default = "c"),
                            locale=readr::locale(encoding = "Latin1"))
    }
  }

  attr(pumf_data,"pumf_base_path") <- pumf_base_path
  attr(pumf_data,"layout_mask") <- layout_mask

  if (guess_numeric) pumf_data <- pumf_data %>% convert_pumf_numeric_columns()

  pumf_data
}



#' Get select pumf data files
#'
#' @description This is a convenience function that downloads and accesses pumf data for
#' a curated set of pumf datasets.
#'
#' @param pumf_series sereis for the pumf data, like LSF, or CHS
#' @param pumf_version In case there are several versions of a given series, like for LFS, the version
#' @param layout_mask optional layout mask in case there are several files.
#' identifiers. For LFS this is the month/year.
#' @param file_mask optional additional mask to filter down to specific PUMF file if there are several
#' @param pumf_cache_path A path to a permanent cache. If none is fould the data is stored in the temporary
#' directory for the duration of the session.
#'
#' @return A tibble with the pumf data.
#' @export
get_pumf <- function(pumf_series,pumf_version = NULL,
                     layout_mask=NULL,
                     file_mask=layout_mask,
                     pumf_cache_path = getOption("canpumf.cache_path")){
  if (is.null(pumf_version)) {
    d<- list_canpumf_collection() %>%
      filter(.data$Acronym==pumf_series)
    if (!is.null(pumf_version)) d <- d %>% filter(.data$Version==pumf_version)
    if (nrow(d)!=1) {
      stop("Could not find PUMF version.")
    }
    pumf_version <- d$Version
  }

  if (is.null(pumf_cache_path)||nchar(pumf_cache_path)==0){
    pumf_cache_path<- file.path(tempdir(),"pumf")
    message(paste0("PUMF cache path is not set, consider setting the PUMF cache path via","\n",
            'options(canpumf.cache_path="<path to permanently cache PUMF data>")',"\n",
            "Data will be cached for the duration of the current session only."))
  }
  if (!dir.exists(pumf_cache_path)) dir.create(pumf_cache_path)
  destination_dir <- file.path(pumf_cache_path,pumf_series)
  if (!dir.exists(destination_dir)) dir.create(destination_dir)
  if (!is.null(pumf_version)) {
    destination_dir <- file.path(destination_dir,pumf_version)
    if (!dir.exists(destination_dir)) dir.create(destination_dir)
  }
  if (length(dir(destination_dir))==0) {
    dd<-download_pumf(d$url,destination_dir = destination_dir)
  }

  read_pumf_data(destination_dir,layout_mask=layout_mask, file_mask=file_mask)
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
    if (!dir.exists(dirname(destination_dir))) dir.create(dirname(destination_dir))
    message("Downloading PUMF data.")
    if (!dir.exists(destination_dir)) dir.create(destination_dir)
    tmp <- tempfile(fileext = '.zip')
    old_timeout <- getOption("timeout")
    options("timeout"=timeout)
    unzip_option=getOption("unzip")
    if (is.null(unzip_option)) unzip_option <- "internal"
    #httr::GET(path, httr::write_disk(tmp),timeout=timeout)
    utils::download.file(path,tmp,mode="wb")
    options("timeout"=old_timeout)
    ls <- utils::unzip(tmp,exdir = destination_dir, unzip=unzip_option)
  } else {
    message("Path already exists, using cached data.")
  }
  new_dir <- file.path(destination_dir,"/SPSS")
  if (dir.exists(new_dir)) destination_dir<-new_dir
  destination_dir
}


#' List available PUMF LFS versions
#'
#' @return A tibble with versions and urls to available LFS PUMF data
#' @export
list_available_lfs_pumf_versions <- function(){
  #base_url <- "https://www150.statcan.gc.ca/n1/en/catalogue/71M0001X/"
  base_url <- "https://www150.statcan.gc.ca/n1/pub/71m0001x/"
  url <- paste0(base_url,"71m0001x2021001-eng.htm")
  ts<-rvest::read_html(url) %>%
    rvest::html_elements("a")
  ts <- ts[rvest::html_text(ts)=="CSV"]

  tibble(url=paste0(base_url,rvest::html_attr(ts,"href")),
         Date=gsub(" \\| PUMF: CSV","",rvest::html_attr(ts,"title"))) %>%
    mutate(version=strftime(as.Date(paste0("01 ",.data$Date),format="%d %B %Y"),"%Y-%m")) %>%
    select(.data$Date,.data$version,.data$url)
}

#' Download PUMF LFS data
#'
#' @param version A version of the pumf data of fornat <Year>-<Month>, e.g. "2021-01"
#' @param destination_dir Optional path where to store the extracted PUMF data, default is `file.path(tempdir(),"pumf")`
#' @param timeout Optional parameter to specify connection timout for download
#' @return pumf_base_dir that can be used in the other package functions
#' @export
download_lfs_pumf <- function(version="2021-01",destination_dir=file.path(tempdir(),"pumf"),timeout=3000){
  pumf_url <- list_available_lfs_pumf_versions() %>% filter(version==!!version) %>% pull(url)
  if (length(pumf_url)==0) stop(paste0("LFS version ",version," is not available, check available LFS PUMF versions via `list_available_lfs_pumf_versions()`"))

  # url <- rvest::read_html(pumf_url) %>%
  #   rvest::html_elements("a") %>%
  #   lapply(function(t){
  #     tibble(text=rvest::html_text(t),url=rvest::html_attr(t,"href"))
  #   }) %>%
  #   bind_rows() %>%
  #   filter(.data$text=="CSV") %>%
  #   pull(.data$url) %>%
  #   first() %>%
  #   paste0(dirname(pumf_url),"/",.)
  if (!dir.exists(destination_dir)) {
    if (!dir.exists(dirname(destination_dir))) dir.create(dirname(destination_dir))
    dir.create(destination_dir)
  }
  destination_dir <- file.path(destination_dir,paste0("lfs_",version,"-CSV"))
  # if (as.integer(substr(version,6,7))<=4)
  #   url <- paste0("https://www150.statcan.gc.ca/n1/pub/71m0001x/2021001/",version,"-CSV-eng.zip")
  # else
  #   url <- paste0("https://www150.statcan.gc.ca/n1/pub/71m0001x/2021001/",version,"-CSV.zip")
  download_pumf(pumf_url,destination_dir = destination_dir,timeout = timeout)
  destination_dir
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
