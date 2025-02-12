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

  lct <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")

  d<-tibble(url=paste0(base_url,rvest::html_attr(ts,"href")),
            Date=gsub(" \\| PUMF: CSV","",rvest::html_attr(ts,"title"))) |>
    mutate(version=case_when(grepl("^\\d{4}$",.data$Date) ~ .data$Date,
                             TRUE ~ strftime(as.Date(paste0("01 ",.data$Date),format="%d %B %Y"),"%Y-%m"))) |>
    select(.data$Date,.data$version,.data$url) |>
    select(.data$Date,.data$version,.data$url)

  Sys.setlocale("LC_TIME", lct)

  d
}


ensure_lfs_metadata <- function(lfs_path){
  canpumf_dir <- file.path(lfs_path,"canpumf")
  if (!dir.exists(canpumf_dir)) dir.create(canpumf_dir)
  metadata_path <- lfs_path
  if (dir.exists(file.path(lfs_path,"Documents"))) {
    metadata_path <- file.path(lfs_path,"Documents")
  }
  codebook_path <- dir(metadata_path,"codebook\\.csv",full.names = TRUE)
  codebook <- readr::read_csv(codebook_path,locale = readr::locale(encoding = "CP1252"),
                              col_types=readr::cols(.default = "c"))

  name_labels <- codebook |>
    filter(!is.na(.data$Field_Champ)) |>
    select(.data$Field_Champ,name=.data$Variable_Variable,label=.data$EnglishLabel_EtiquetteAnglais) |>
    mutate(n=n(),.by=.data$label) |>
    mutate(label=case_when(.data$n==1 ~ .data$label,
                           TRUE ~ paste0(.data$label," (",.data$name,")"))) |>
    select(-.data$n)

  val_labels <- codebook |>
    mutate(skip=!is.na(.data$Field_Champ)) |>
    tidyr::fill(.data$Field_Champ) |>
    filter(!.data$skip) |>
    inner_join(name_labels,by=c("Field_Champ")) |>
    select(.data$name,val=.data$Variable_Variable,label=.data$EnglishLabel_EtiquetteAnglais) |>
    mutate(n=n(),.by=c(.data$label,.data$name)) |>
    mutate(label=case_when(.data$n==1 ~ .data$label,
                           TRUE ~ paste0(.data$label," (",.data$val,")"))) |>
    select(-.data$n)

  saveRDS(val_labels,file.path(canpumf_dir,"val.rds"))
  saveRDS(name_labels |> select(.data$name,.data$label),file.path(canpumf_dir,"var.rds"))
}



get_lfs_pumf <- function(pumf_version,pumf_cache_path, timeout=3000){
  pumf_path <- file.path(pumf_cache_path,paste0("lfs_",pumf_version,"-CSV"))
  if (!dir.exists(pumf_path)||length(dir(pumf_path))==0) {
    pumf_url <- list_available_lfs_pumf_versions() %>% filter(version==!!pumf_version) %>% pull(url)
    if (length(pumf_url)==0) stop("LFS version ",pumf_version," is not available, check available LFS PUMF versions via `list_available_lfs_pumf_versions()`")

    download_pumf(pumf_url,destination_dir = pumf_path,timeout = timeout)
  }
  ensure_lfs_metadata(pumf_path)

  numerics <- c("wksaway",  "uhrsmain", "ahrsmain", "utothrs",  "atothrs",  "hrsaway",  "paidot",   "unpaidot",
                "xtrahrs", "tenure",   "prevten",  "hrlyearn", "durunemp", "durjless", "finalwt") |>
    toupper()

  pumf_file_path <- dir(pumf_path,"pub\\d+\\.csv",full.names = TRUE)
  pumf_data <- readr::read_csv(pumf_file_path,locale = readr::locale(encoding = "CP1252"),
                               col_types=readr::cols(.default = "c")) %>%
    setNames(toupper(names(.))) |>
    mutate(across(any_of(numerics),as.numeric))

  attr(pumf_data,"pumf_base_path") <- pumf_path
  pumf_data
}

