#' Parse PUMF medatadata
#'
#' @param pumf_base_path path for pumf data
#' @param layout_mask optional parameter to identify layour files if there are many
#' @return NULL
#' @export
parse_pumf_metadata_cards <- function(pumf_base_path,layout_mask=NULL){
  cpld <- pumf_clean_layout_dir(pumf_base_path,layout_mask)
  cards_base_path <- dir(pumf_base_path,pattern="Reading cards",full.names = TRUE)
  if (length(cards_base_path)==0) {
    d<- dir(pumf_base_path,pattern="Data",full.names = TRUE)
    if (length(d)==1) cards_base_path <- dir(d,pattern="Reading cards",full.names = TRUE)
  }
  if (length(dir(cards_base_path,"SPSS"))==1) cards_base_path <- file.path(cards_base_path,"SPSS")
  layout_files <- dir(cards_base_path,full.names = TRUE)
  layout_types <- layout_files[grepl("\\.lay$",layout_files)] |> basename()
  if (!is.null(layout_mask) && length(layout_types)>1) {
    layout_files <- layout_files[grepl(layout_mask,layout_files)]
  }

  layout_types <- layout_files[grepl("\\.lay$",layout_files)] |> basename()
  if (length(layout_types)>1) {
    stop(paste0("Found more than one layout file:\n",paste0(layout_types,collapse="\n"),"\nSpecify a layout mask to continue."))
  }

  if (length(layout_types)==0) {
    stop(paste0("Could not find layout file."))
  }

  lay_path <- layout_files[grepl("\\.lay$",layout_files)]
  lay_pos <- readr::read_lines(lay_path, locale=readr::locale(encoding = "Latin1")) %>%
    as_tibble() |>
    filter(grepl("^ +",.data$value)) |>
    mutate(value=gsub("^ +","",.data$value)) |>
    mutate(name=stringr::str_match(.data$value,"^([A-Z,0-9,_]+)")[,2]) |>
    mutate(start=stringr::str_match(.data$value,'(\\d+) - ')[,2]) |>
    mutate(end=stringr::str_match(.data$value,'- (\\d+)')[,2]) |>
    select(-.data$value)

  saveRDS(lay_pos,file.path(cpld,"lay.Rds"))

  var_path <- layout_files[grepl("\\.lbe$",layout_files)]

  var_labels <- readr::read_lines(var_path, locale=readr::locale(encoding = "Latin1")) %>%
    as_tibble() %>%
    filter(grepl("^ +",.data$value)) |>
    mutate(value=gsub("^ +","",.data$value)) |>
    mutate(name=stringr::str_match(.data$value,"^([A-Z,0-9,_]+)")[,2]) |>
    mutate(label=stringr::str_match(.data$value,'"(.+)"$')[,2]) |>
    select(-.data$value)

  saveRDS(var_labels,file.path(cpld,"var.Rds"))

  miss_path <- layout_files[grepl("\\.mvs$",layout_files)]
  miss <- readr::read_lines(miss_path, locale=readr::locale(encoding = "Latin1")) %>%
    as_tibble() %>%
    filter(!(.data$value %in% c("MISSING VALUES","."))) |>
    mutate(name=stringr::str_match(.data$value,"^([A-Z,0-9,_]+) +")[,2]) %>%
    mutate(missing=stringr::str_match(.data$value,'\\((.+)\\)')[,2]) %>%
    select(-.data$value) %>%
    mutate(missing_low=strsplit(.data$missing," THRU ") %>% lapply(first) %>% unlist %>% as.numeric) %>%
    mutate(missing_high=strsplit(.data$missing," THRU ") %>% lapply(last) %>% unlist %>% as.numeric)

  saveRDS(miss,file.path(cpld,"miss.Rds"))


  val_path <- layout_files[grepl("\\.cde$",layout_files)]

  r <- readr::read_lines(val_path, locale=readr::locale(encoding = "Latin1")) %>%
    as_tibble() |>
    filter(!(.data$value %in% c("VALUE LABELS","",".")))

  starts <- c(1,which(grepl("^/",r$value)) +1)
  starts <- starts[-length(starts)]

  val_labels <- seq(1,length(starts)) %>%
    lapply(function(i){
      s=starts[i]
      if (i==length(starts)) e=nrow(r) else e=starts[i+1]-1
      name <- r$value[s] |> trimws()
      rr <- r %>% slice(seq(s+1,e-1))
      values <- rr |>
        mutate(name=name) |>
        mutate(val=stringr::str_match(.data$value,"^ *([A-Z,0-9,_]+) +")[,2]) %>%
        mutate(label=stringr::str_match(.data$value,' +\\"(.+)\\"')[,2]) |>
        select(-.data$value)
    }) %>%
    bind_rows()

  saveRDS(val_labels,file.path(cpld,"val.Rds"))

  NULL
}




#' Parse PUMF data
#'
#' @param pumf_base_path path for raw pumf data
#' @param layout_mask layout mask
#'
#' @return tibble with variable labels
#' @export
parse_pumf_data_cards <- function(pumf_base_path,layout_mask=NULL){
  cpld <- pumf_clean_layout_dir(pumf_base_path,layout_mask)
  if (!dir.exists(cpld) || length(dir(cpld))<4) {
    parse_pumf_metadata_cards(pumf_base_path,layout_mask)
  }
  data_base_path <- dir(pumf_base_path,"Data",full.names = TRUE)
  d<-dir(data_base_path,"TXT",full.names = TRUE)
  if (length(d)==1) data_base_path <- d
  weight_file_layout <- dir(data_base_path,"bsw_layout",full.names = TRUE)
  weight_file_data <- dir(data_base_path,"bsw_flatfile",full.names = TRUE)
  data_file_layout <- dir(data_base_path,"layout",full.names = TRUE)
  data_file_data <- dir(data_base_path,"flatfile",full.names = TRUE)
  data_file_layout<-data_file_layout[!grepl("bsw",data_file_layout)]
  data_file_data<-data_file_data[!grepl("bsw",data_file_data)]

  layout <- readr::read_lines(data_file_layout,locale=readr::locale(encoding = "Latin1")) |>
    as_tibble() |>
    filter(grepl("^@\\d+ ([A-Z,a-z,0-9,_]+) +\\$\\d+\\.$",.data$value)) |>
    mutate(name=stringr::str_match(.data$value,"^@\\d+ ([A-Z,a-z,0-9,_]+) +\\$\\d+\\.$")[,2],
           start=stringr::str_match(.data$value,"^@(\\d+) [A-Z,a-z,0-9,_]+ +\\$\\d+\\.$")[,2],
           size=stringr::str_match(.data$value,"^@\\d+ [A-Z,a-z,0-9,_]+ +\\$(\\d+)\\.$")[,2]) |>
    select(-.data$value) |>
    mutate(across(c(.data$start,.data$size),as.integer)) |>
    mutate(end=.data$start+.data$size-1)

  data <- readr::read_fwf(data_file_data,
                   col_positions = readr::fwf_positions(layout$start,layout$end,col_names = layout$name),
                   col_types=readr::cols(.default = "c"),
                   locale=readr::locale(encoding = "Latin1"))

  if (length(weight_file_layout)==1) {
    layoutw <- readr::read_lines(weight_file_layout,locale=readr::locale(encoding = "Latin1")) |>
      as_tibble() |>
      filter(grepl("^@",.data$value)) |>
      mutate(value=trimws(.data$value)) |>
      mutate(name=stringr::str_match(.data$value,"^@\\d+ ([A-Z,a-z,0-9,_]+) +\\$*\\d+\\.[\\d+]*$")[,2],
             start=stringr::str_match(.data$value,"^@(\\d+) [A-Z,a-z,0-9,_]+ +\\$*\\d+\\.[\\d+]*$")[,2],
             size=stringr::str_match(.data$value,"^@\\d+ [A-Z,a-z,0-9,_]+ +\\$*(\\d+)\\.[\\d+]*$")[,2]) |>
      select(-.data$value) |>
      mutate(across(c(.data$start,.data$size),as.numeric)) |>
      mutate(end=.data$start+.data$size-1)

    match_string <- layoutw$name[1]
    if (layout$name[1]!=match_string) {
      if (layout$name[1]==toupper(match_string)) {
        match_string=toupper(match_string)
        layoutw$name[1] <- match_string
      }
    }

    weights <- readr::read_fwf(weight_file_data,
                            col_positions = readr::fwf_positions(layoutw$start,layoutw$end,col_names = layoutw$name),
                            col_types=readr::cols(.default = "c"),
                            locale=readr::locale(encoding = "Latin1")) |>
      mutate(across(-1,as.numeric))

    data <- data |>
      left_join(weights,by=match_string)
  }


  data
}

