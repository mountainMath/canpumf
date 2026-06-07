

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
  .Deprecated("pumf_metadata",
    msg = paste0("guess_numeric_pumf_columns() is deprecated. ",
                 "Use pumf_metadata() to access variable type information."))

  miss_labels <- c()
  miss_data <- read_pumf_miss_labels(pumf_base_path,layout_mask, numeric_only=TRUE)
  if (nrow(miss_data)==0) {
    val_labels <- read_pumf_val_labels(pumf_base_path)
    miss_labels <- val_labels %>%
      filter(grepl(numeric_pattern,.data$val)) %>%
      pull(.data$name)
  }
  else  {
    miss_labels <- miss_data %>%
      filter(grepl(numeric_pattern,.data$missing)) %>%
      pull(.data$name)
  }

  miss_labels
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
  .Deprecated("get_pumf",
    msg = paste0("label_pumf_columns() is deprecated. ",
                 "Use get_pumf() which applies labels automatically."))
  names(pumf_data) <- toupper(names(pumf_data))
  var_labels <- read_pumf_var_labels(pumf_base_path,layout_mask)  |> mutate(name=toupper(.data$name))
  if (sum(duplicated(var_labels$label))>0) {
    var_labels <- var_labels |>
      mutate(n=n(),.by=.data$label) |>
      mutate(label=case_when(.data$n>1~paste0(.data$label," (",.data$name,")"),TRUE~.data$label)) |>
      select(-"n")
  }

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
#' @param rename_columns rename PUMF columns to human readable names, default is `FALSE`
#' @param infer_missing_numeric optional character, infer variables that aren't labelled to be numeric
#'
#' @return relabeled data frame
#' @export
label_pumf_data <- function(pumf_data,
                            pumf_base_path=attr(pumf_data,"pumf_base_path"),
                            layout_mask=attr(pumf_data,"layout_mask"),
                            rename_columns=FALSE,
                            infer_missing_numeric=FALSE){
  .Deprecated("get_pumf",
    msg = paste0("label_pumf_data() is deprecated. ",
                 "Use get_pumf() which applies labels automatically via the new pipeline."))
  if (grepl("98M0001X",pumf_base_path)&&grepl("2021",pumf_base_path)) {
    infer_missing_numeric <- TRUE
  }
  val_labels <- read_pumf_val_labels(pumf_base_path,layout_mask) |> mutate(name=toupper(.data$name))
  var_labels <- read_pumf_var_labels(pumf_base_path,layout_mask) |> mutate(name=toupper(.data$name))

  if (sum(duplicated(var_labels$label))>0) {
    var_labels <- var_labels |>
      mutate(n=n(),.by=.data$label) |>
      mutate(label=case_when(.data$n>1~paste0(.data$label," (",.data$name,")"),TRUE~.data$label)) |>
      select(-"n")
  }

  n1 <- val_labels$name |> unique()
  n2 <- var_labels$name |> unique()
  if (length(setdiff(n1,n2))>0) {
    if(length(setdiff(toupper(n1),toupper(n2)))==0) {
      val_labels <- val_labels |> mutate(name=toupper(.data$name))
      var_labels <- var_labels |> mutate(name=toupper(.data$name))
    }
  }
  names(pumf_data) <- toupper(names(pumf_data))
  vars <- pumf_data %>% select_if(function(d)!is.numeric(d)) %>% names() %>% intersect(var_labels$name)
  missing_vars <- setdiff(n2,n1)
  for (var in vars) {
    vl <- val_labels %>%
      filter(.data$name==var) %>%
      filter(!grepl("^\\d+-\\d+$|^\\d+-$",.data$val))
    if (nrow(vl)==1 && vl$val[1]=="blank") vl <- vl %>% filter(.data$val!="blank")
    lookup <- setNames(vl$label,vl$val)
    if (length(lookup)>0) {
      missed <- pumf_data %>% pull(var) %>% unique %>% setdiff(vl$val)
      if (length(missed)>0) {
        missed_i <- as.integer(missed) |> as.character()
        for (m in intersect(missed_i,names(lookup))) {
          mi <- which(m==missed_i)
          mo <- missed[mi]
          if (length(lookup[m])==length(mo)) {
            lookup <- c(lookup,setNames(lookup[m],mo))
          }
        }
      }
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

  if (infer_missing_numeric) {
    pumf_data <- pumf_data |>
      mutate(across(setdiff(missing_vars,"PPSORT"),\(d)ifelse(d==88888888|d==99999999,NA_real_,as.numeric(d))))
  }

  if (rename_columns) {
    pumf_data <- label_pumf_columns(pumf_data=pumf_data,
                                    pumf_base_path=pumf_base_path,
                                    layout_mask=layout_mask)
  }
  pumf_data
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
  .Deprecated("get_pumf",
    msg = paste0("convert_pumf_numeric_columns() is deprecated. ",
                 "Use get_pumf() which handles numeric conversion automatically."))
  if (is.null(pumf_base_path)) stop("Could not find PUMF base path to access metadata.")
  if (is.null(numeric_columns))  numeric_columns <- guess_numeric_pumf_columns(pumf_base_path,layout_mask)

  miss_data <- read_pumf_miss_labels(pumf_base_path,layout_mask, numeric_only=TRUE)
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




#' Read raw PUMF data as a tibble (deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is kept for users who work with manually-deposited directories
#' outside the standard cache.  For all standard surveys use [get_pumf()],
#' which returns a lazy DuckDB table and handles labeling automatically.
#'
#' @param pumf_base_path Path to the extracted PUMF directory.
#' @param layout_mask Optional mask to select a specific layout file.
#' @param file_mask Optional mask to select a specific data file.
#' @param guess_numeric Logical; convert numeric columns and apply missing
#'   values.  Default `TRUE`.
#'
#' @return A tibble with attributes `pumf_base_path` and `layout_mask`.
#' @export
read_pumf_data <- function(pumf_base_path,
                           layout_mask  = NULL,
                           file_mask    = layout_mask,
                           guess_numeric = TRUE) {
  warning(
    "read_pumf_data() is deprecated. ",
    "Use get_pumf(series, version) for the new DuckDB-backed pipeline, ",
    "or pumf_metadata(series, version) to access metadata only.",
    call. = FALSE)

  # Parse canonical metadata if not already present
  if (!metadata_exists(pumf_base_path)) {
    tryCatch(
      pumf_parse_metadata(pumf_base_path, layout_mask = layout_mask),
      error = function(e)
        stop("Could not parse metadata in ", pumf_base_path, ": ", e$message)
    )
  }

  meta   <- read_metadata(file.path(pumf_base_path, "metadata"))
  is_fwf <- !is.null(meta$layout)
  enc    <- "CP1252"

  data_path <- tryCatch(
    .find_pumf_data_file(pumf_base_path, file_mask, is_fwf),
    error = function(e)
      stop("Could not find data file in ", pumf_base_path, ": ", e$message)
  )

  if (is_fwf) {
    pumf_data <- readr::read_fwf(
      data_path,
      col_positions  = readr::fwf_positions(meta$layout$start, meta$layout$end,
                                             col_names = meta$layout$name),
      col_types      = readr::cols(.default = "c"),
      trim_ws        = TRUE,
      locale         = readr::locale(encoding = enc),
      show_col_types = FALSE)
  } else {
    pumf_data <- readr::read_csv(
      data_path,
      col_types      = readr::cols(.default = "c"),
      locale         = readr::locale(encoding = enc),
      show_col_types = FALSE)
    names(pumf_data) <- toupper(names(pumf_data))
  }

  if (guess_numeric)
    pumf_data <- .apply_numeric_conversion(pumf_data, meta$variables)

  attr(pumf_data, "pumf_base_path") <- pumf_base_path
  attr(pumf_data, "layout_mask")    <- layout_mask

  if (guess_numeric) pumf_data <- pumf_data %>% convert_pumf_numeric_columns()

  pumf_data
}



#' Download PUMF data (deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use [pumf_locate_or_download()] internally or [get_pumf()] as the public
#' entry point.
#'
#' @param path Download URL for the PUMF zip file.
#' @param destination_dir Directory to extract into.
#' @param refresh Force re-download even if directory exists.
#' @param timeout Connection timeout in seconds.
#' @keywords internal
#' @return pumf_base_dir that can be used in the other package functions
#' @keywords internal
download_pumf <- function(path,destination_dir=file.path(tempdir(),"pumf"),refresh=FALSE,timeout=3000){
  if (refresh || !dir.exists(destination_dir) || length(dir(destination_dir))==0) {
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
    if (Sys.info()['sysname']=="Darwin") {
      system(paste0("ditto -V -x -k --sequesterRsrc --rsrc ",tmp," ",destination_dir))
    } else {
      utils::unzip(tmp,exdir = destination_dir, unzip=unzip_option)
    }
  } else {
    message("Path already exists, using cached data.")
  }
  new_dir <- file.path(destination_dir,"/SPSS")
  if (dir.exists(new_dir)) destination_dir<-new_dir
  destination_dir
}



#' Add bootstrap weights to PUMF data
#'
#' @param pumf_data A dataframe with PUMF data
#' @param weight_column Name of the column with the standard weights
#' @param bootstrap_weight_count Number of boostrap weights to generate
#' @param bootstrap_weight_prefix Name prefix for the bootstrap weight columns
#' @param algorithm Algorithm to calculate bootstrap weights, either of "iterative" or "experimental"
#' @param seed Random see to be used for bootstrap sample for reproducibility
#' @return pumf_base_dir that can be used in the other package functions
#' @export
add_bootstrap_weights <- function(pumf_data,
                                  weight_column,
                                  bootstrap_weight_count = 16,
                                  bootstrap_weight_prefix="BSW",
                                  algorithm="iterative",
                                  seed=NULL){
  n <- nrow(pumf_data)

  if (algorithm=="experimental") {
    rows <- 1:n
    wts <- 1:bootstrap_weight_count
    wt_names <- paste0(bootstrap_weight_prefix,wts)
    set.seed(seed)
    chunk_length <- 10
    split_weights <- split(wt_names, ceiling(seq_along(wt_names) / chunk_length))

    bsw <- lapply(split_weights,\(wt_batch){
      bwc <- length(wt_batch)
      perm <-  as.data.frame(replicate(bwc, sample(rows,replace = TRUE))) |>
        dplyr::as_tibble() |>
        setNames(wt_batch)
      draw <- perm |>
        tidyr::pivot_longer(everything()) |>
        count(.data$name,.data$value)
      bsw_counts <- draw |>
        tidyr::complete(name=.data$wt_batch,value=.data$rows,fill=list(n=0)) |>
        tidyr::pivot_wider(names_from=.data$name,values_from = .data$n) |>
        arrange(.data$value)
      bsw <- bsw_counts |>
        select(-.data$value)
    }) |>
      bind_cols()

    weights <- pumf_data |> pull(!!weight_column)
    bsw_final <-  as.data.frame(weights * bsw) |>
      setNames(wt_names) |>
      as_tibble()

    pumf_data <- pumf_data |> bind_cols(bsw_final)
  } else {

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
  }
  pumf_data
}


#' Get select pumf database connections (deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use [get_pumf()] instead, which returns a lazy duckplyr table directly.
#'
#' @param pumf_series Survey series acronym.
#' @param pumf_version Version string.
#' @param layout_mask Optional layout mask.
#' @param file_mask Optional file mask.
#' @param guess_numeric Logical.
#' @param pumf_cache_path Cache path.
#' @param refresh Logical.
#' @param refresh_layout Logical.
#' @param timeout Timeout in seconds.
#'
#' @return A lazy dplyr tbl backed by DuckDB.
#' @export
get_pumf_connection <- function(pumf_series, pumf_version = NULL,
                                 layout_mask = NULL,
                                 file_mask = layout_mask,
                                 guess_numeric = TRUE,
                                 pumf_cache_path = getOption("canpumf.cache_path"),
                                 refresh = FALSE,
                                 refresh_layout = FALSE,
                                 timeout = 3000) {
  .Deprecated("get_pumf",
    msg = paste0("get_pumf_connection() is deprecated. ",
                 "Use get_pumf(series, version) which returns a lazy duckplyr table directly."))
  get_pumf(series     = pumf_series,
           version    = pumf_version,
           cache_path = pumf_cache_path %||% getOption("canpumf.cache_path", tempdir()),
           refresh    = refresh)
}
