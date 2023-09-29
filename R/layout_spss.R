#' Parse PUMF medatadata
#'
#' @param pumf_base_path path for pumf data
#' @param layout_mask optional parameter to identify layour files if there are many
#' @return NULL
#' @export
parse_pumf_metadata_spss <- function(pumf_base_path,layout_mask=NULL){
  cpld <- pumf_clean_layout_dir(pumf_base_path,layout_mask)
  var_path <- find_unique_layout_file(pumf_layout_dir(pumf_base_path),"vare\\.sps",layout_mask)

  var_labels <- read_lines(var_path, locale=locale(encoding = "Latin1")) %>%
    as_tibble() %>%
    slice_raw_layout_data() %>%
    mutate(name=str_match(.data$value,"^ +([A-Z,0-9,_]+) +")[,2]) %>%
    mutate(label=str_match(.data$value,'"(.+)"$')[,2]) %>%
    select(-.data$value)

  saveRDS(var_labels,file.path(cpld,"var.Rds"))


  miss_path <- find_unique_layout_file(pumf_layout_dir(pumf_base_path),"miss\\.sps",layout_mask)
  miss <- read_lines(miss_path, locale=locale(encoding = "Latin1")) %>%
    as_tibble() %>%
    slice_raw_layout_data() %>%
    mutate(name=str_match(.data$value,"^ +([A-Z,0-9,_]+) +")[,2]) %>%
    mutate(missing=str_match(.data$value,'\\((.+)\\)')[,2]) %>%
    select(-.data$value) %>%
    mutate(missing_low=strsplit(.data$missing," THRU ") %>% lapply(first) %>% unlist %>% as.numeric) %>%
    mutate(missing_high=strsplit(.data$missing," THRU ") %>% lapply(last) %>% unlist %>% as.numeric)

  saveRDS(miss,file.path(cpld,"miss.Rds"))


  val_path <- find_unique_layout_file(pumf_layout_dir(pumf_base_path),"vale\\.sps",layout_mask)

  r <- readr::read_lines(val_path, locale=readr::locale(encoding = "Latin1")) %>%
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

  saveRDS(val_labels,file.path(cpld,"val.Rds"))

  NULL
}


read_pumf_layout_spss <- function(pumf_base_path,layout_mask=NULL){
  i_path <- find_unique_layout_file(pumf_layout_dir(pumf_base_path),"_i\\.sps",layout_mask)

  rl <- readr::read_lines(i_path, locale=readr::locale(encoding = "Latin1")) %>%
    dplyr::as_tibble()

  start_index <- which(grepl("^DATA ",rl$value))
  end_index <- which(grepl(" *\\.$|^ *$",rl$value))
  end_index <- end_index[end_index>start_index] %>% first

  if (grepl("Individuals|Hierarchical",i_path)) {
    rows <- rl %>%
      slice(seq(start_index+1,end_index-1)) %>%
      summarise(value=paste0(.data$value,collapse="")) %>%
      pull(.data$value) %>%
      gsub(" +\\. *$","",.) %>%
      strsplit("  +") %>%
      unlist()
    layout <- tibble(value=rows) %>%
      filter(.data$value!="") %>%
      mutate(name=stringr::str_extract(.data$value,"^[A-Za-z0-9_]+")) %>%
      mutate(start=stringr::str_extract(.data$value," \\d+")) %>%
      mutate(end=stringr::str_extract(.data$value,"\\d+$")) %>%
      mutate(note="") %>%
      select(-.data$value) %>%
      mutate_at(c("start","end"),as.integer)
  } else {
    layout <- rl %>%
      slice(seq(start_index+1,end_index-1)) %>%
      mutate(note=stringr::str_match(.data$value," +\\((.+)\\)$")[,2] %>% unlist) %>%
      cbind(stringr::str_match(.$value," +(\\d+) - +(\\d+)")[,c(2,3)] %>%
              as.data.frame() %>%
              dplyr::as_tibble(.name_repair = "unique") %>%
              setNames(c("start","end"))) %>%
      mutate(name=stringr::str_match(.data$value,"^ +([A-Z,0-9,_]+) +")[,2]) %>%
      select(-.data$value) %>%
      mutate_at(c("start","end"),as.integer)
  }
  layout
}


ensure_2021_pumfi_metadata <- function(pumf_base_path){
  canpumf_dir <- file.path(pumf_base_path,"canpumf")

  if (!dir.exists(canpumf_dir)||length(dir(canpumf_dir))<2) {
    if (!dir.exists(canpumf_dir)) dir.create(canpumf_dir)
    layout_path <- dir(pumf_base_path,"English",full.names = TRUE)
    layout_path <- dir(layout_path,"SPSS",full.names = TRUE)

    spss <- readr::read_lines(file.path(layout_path,dir(layout_path,"\\.sps$")),
                              locale = readr::locale(encoding = "UTF-8")) |>
      as_tibble() |>
      mutate(value=gsub("^ +| *$","",.data$value))


    mr <- which(grepl("\\+$",spss$value))
    spss$value[mr] <- lapply(mr,\(i)paste0(gsub("' *\\+$","",spss$value[i]),gsub("^ *'","",spss$value[i+1]))) |> unlist()
    spss <- spss |>
      slice(-(mr+1))

    formats <- which(grepl("FORMATS$",spss$value))
    blanks <- which(""==spss$value)
    starts <- which(grepl("LABELS$",spss$value))
    ends <- which(grepl("^ *\\.$",spss$value))

    format_data <- spss |>
      slice(seq(formats+1,blanks[blanks>formats][1]-1)) |>
      pull(value) |>
      paste0(collapse = " ") |>
      gsub(" *\\. *$","",x=_) |>
      gsub("/$","",x=_) |>
      strsplit("/") |>
      unlist() |>
      gsub("^ +| +$","",x=_) |>
      as_tibble() |>
      mutate(p=strsplit(value," +")) |>
      mutate(name=lapply(.data$p,first) |> unlist(),
             value=lapply(.data$p,last) |> unlist()) |>
      select(.data$name,.data$value)


    name_labels <- spss |>
      slice(seq(starts[1]+1,starts[2]-1)) |>
      mutate(value=gsub("^ +| *\\.$","",.data$value)) |>
      mutate(name=gsub(" .+$","",.data$value)) |>
      mutate(label=str_extract(.data$value,"'.+'") |> gsub("'","",x=_) |> gsub("â€“","-",x=_)) |>
      select(.data$name,.data$label) |>
      filter(!grepl("^WEIGHT$|^WT\\d+$",.data$name))

    saveRDS(name_labels,file.path(canpumf_dir,"var.Rds"))

    var_labels_raw <- spss |>
      slice(seq(starts[2]+1,last(ends)-1)) |>
      mutate(value=gsub("^ +| *\\.$| *$","",.data$value))

    single_quotes <- which(grepl("'$|' *\\+ *$",var_labels_raw$value))
    var_labels_raw$value[single_quotes] <- gsub("'",'"',var_labels_raw$value[single_quotes])

    var_starts <- which(grepl("^\\/",var_labels_raw$value))

    val_labels <- 1:length(var_starts) |>
      purrr::map_df(\(r){
        s=var_starts[r]
        n<-var_labels_raw$value[s] |> gsub("^\\/","",x=_)
        s=s+1
        if (r==length(var_starts)) {
          e=nrow(var_labels_raw)
        } else {
          e=var_starts[r+1]-1
        }
        var_labels_raw |>
          slice(s:e) |>
          mutate(val=gsub(' *".+',"",.data$value),
                 label=str_extract(.data$value,'".+"') |> gsub('"',"",x=_)) |>
          mutate(name=n) |>
          select(.data$name,.data$val,.data$label)

      })

    saveRDS(val_labels,file.path(canpumf_dir,"val.Rds"))
  }
}
