
#' Read PUMF variable labels
#'
#' @param pumf_base_path pumf base path
#' @param var_path optional path or mask for the layout file in case there are several,
#'
#' @return tibble with variable labels
#' @export
read_pumf_var_labels <- function(pumf_base_path,var_path=NULL){
  tmp <- tmp_layout_path(pumf_base_path,var_path,"var")
  if (!file.exists(tmp)) {
    var_path <- find_unique_layout_file(pumf_layout_dir(pumf_base_path),"vare\\.sps",var_path)

    var_labels <- read_lines(var_path, locale=locale(encoding = "Latin1")) %>%
      as_tibble() %>%
      slice_raw_layout_data() %>%
      mutate(name=str_match(.data$value,"^ +([A-Z,0-9,_]+) +")[,2]) %>%
      mutate(label=str_match(.data$value,'"(.+)"$')[,2]) %>%
      select(-.data$value)
    saveRDS(var_labels,tmp)
  } else {
    var_labels <- readRDS(tmp)
  }

  var_labels
}

#' Read PUMF missing data information
#'
#' @param pumf_base_path pumf base path
#' @param miss_path optional path or mask for the layout file in case there are several,
#'
#' @return tibble with missing data information
#' @export
read_pumf_miss_labels <- function(pumf_base_path,miss_path=NULL){
  tmp <- tmp_layout_path(pumf_base_path,miss_path,"miss")
  if (!file.exists(tmp)) {
    miss_path <- find_unique_layout_file(pumf_layout_dir(pumf_base_path),"miss\\.sps",miss_path)
    miss <- read_lines(miss_path, locale=locale(encoding = "Latin1")) %>%
      as_tibble() %>%
      slice_raw_layout_data() %>%
      mutate(name=str_match(.data$value,"^ +([A-Z,0-9,_]+) +")[,2]) %>%
      mutate(missing=str_match(.data$value,'\\((.+)\\)')[,2]) %>%
      select(-.data$value) %>%
      mutate(missing_low=strsplit(.data$missing," THRU ") %>% lapply(first) %>% unlist %>% as.numeric) %>%
      mutate(missing_high=strsplit(.data$missing," THRU ") %>% lapply(last) %>% unlist %>% as.numeric)
    saveRDS(miss,tmp)
  } else {
    miss <- readRDS(tmp)
  }

  miss
}


#' Get PUMF value labels
#'
#' @param pumf_base_path pumf base path
#' @param val_path optional path or mask for the layout file in case there are several,
#'
#' @return tibble with value labels
#' @export
read_pumf_val_labels <- function(pumf_base_path,val_path=NULL){
  tmp <- tmp_layout_path(pumf_base_path,val_path,"val")
  if (!file.exists(tmp)) {
    val_path <- find_unique_layout_file(pumf_layout_dir(pumf_base_path),"vale\\.sps",val_path)

    r <- read_lines(val_path, locale=locale(encoding = "Latin1")) %>%
      as_tibble() %>%
      slice_raw_layout_data()

    starts <- which(grepl("^ +/",r$value))

    val_labels <- seq(1,length(starts)) %>%
      lapply(function(i){
        s=starts[i]
        if (i==length(starts)) e=nrow(r) else e=starts[i+1]-1
        rr <- r %>% slice(seq(s,e))
        h <- str_match(r[1,],"(^ +/)")[,2]
        match_string <- rep_len(" ",nchar(h)+1) %>% paste0(collapse = "")
        values <- rr %>% filter(grepl(match_string,rr$value)) %>%
          mutate(value=gsub("^ +","",.data$value)) %>%
          mutate(val=strsplit(.data$value,"   +") %>% lapply(first) %>% unlist) %>%
          mutate(label=strsplit(.data$value,"   +") %>% lapply(last) %>% unlist) %>%
          mutate_all(~gsub('"',"",.))  %>%
          select(-.data$value)
        vars <- rr %>% filter(!grepl(match_string,rr$value)) %>%
          mutate(value=gsub("^ +/","",.data$value) %>% gsub("^ +","",.)) %>%
          pull(.data$value) %>%
          paste0(collapse = " ") %>%
          str_split(" ") %>%
          unlist
        vars %>%
          lapply(function(var){
            values %>% mutate(name=var)
          }) %>% bind_rows()
      }) %>%
      bind_rows()
    saveRDS(val_labels,tmp)
  } else {
    val_labels <- readRDS(tmp)
  }

  val_labels
}

read_pumf_layout <- function(pumf_base_path,i_path=NULL){
  i_path <- find_unique_layout_file(pumf_layout_dir(pumf_base_path),"_i\\.sps",i_path)

  rl <- read_lines(i_path, locale=locale(encoding = "Latin1")) %>%
    as_tibble()

  start_index <- which(grepl("^DATA ",rl$value))
  end_index <- which(grepl(" *\\.$",rl$value))

  layout <- rl %>%
    slice(seq(start_index+1,end_index-1)) %>%
    mutate(note=str_match(.data$value," +\\((.+)\\)$")[,2] %>% unlist) %>%
    cbind(str_match(.$value," +(\\d+) - +(\\d+)")[,c(2,3)] %>%
            as.data.frame() %>%
            as_tibble(.name_repair = "unique") %>%
            setNames(c("start","end"))) %>%
    mutate(name=str_match(.data$value,"^ +([A-Z,0-9,_]+) +")[,2]) %>%
    select(-.data$value) %>%
    mutate_at(c("start","end"),as.integer)
}
