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
      pull(.data$value) |>
      paste0(collapse = " ") |>
      gsub(" *\\. *$","",x=_) |>
      gsub("/$","",x=_) |>
      strsplit("/") |>
      unlist() |>
      gsub("^ +| +$","",x=_) |>
      as_tibble() |>
      mutate(p=strsplit(.data$value," +")) |>
      mutate(name=lapply(.data$p,first) |> unlist(),
             value=lapply(.data$p,last) |> unlist()) |>
      select(.data$name,.data$value)


    name_labels <- spss |>
      slice(seq(starts[1]+1,starts[2]-1)) |>
      mutate(value=gsub("^ +| *\\.$","",.data$value)) |>
      mutate(name=gsub(" .+$","",.data$value)) |>
      mutate(label=str_extract(.data$value,"'.+'") |> gsub("'","",x=_) |> gsub("\u00E2\u20AC\u201C","-",x=_)) |>
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
        vv<-var_labels_raw |>
          slice(s:e) |>
          mutate(val=gsub(' *".+',"",.data$value),
                 label=str_extract(.data$value,'".+"') |> gsub('"',"",x=_)) |>
          mutate(name=n) |>
          select(.data$name,.data$val,.data$label)
        if (n=="LFACT" && !("1" %in% vv$val)) { # fix missing level in some version of the SPSS command files
          vv <- rbind(vv,tibble(name=n,val="1",label="Employed - Worked in reference week")) |>
            arrange(as.integer(.data$val))
        }
        if (n=="CMA") { # fix extra spaces and inconsistent dashes in SPSS command files
          vv <- vv |> mutate(label=gsub(" +"," ",.data$label) |>
                               gsub(" - "," \u2013 ",x=_))
        }
        vv
      }) |>
      mutate(label=gsub(" +$","",.data$label))

    saveRDS(val_labels,file.path(canpumf_dir,"val.Rds"))
  }
}


ensure_2016_pumfi_metadata <- function(pumf_base_path){
  canpumf_dir <- file.path(pumf_base_path,"canpumf")

  if (!dir.exists(canpumf_dir)||length(dir(canpumf_dir))<2) {
    if (!dir.exists(canpumf_dir)) dir.create(canpumf_dir)
    layout_path <- dir(pumf_base_path,"English",full.names = TRUE)
    layout_path <- dir(layout_path,"SPSS",full.names = TRUE)

    spss <- readr::read_lines(file.path(layout_path,dir(layout_path,"\\.sps$")),
                              locale = readr::locale(encoding = "CP1252")) |>
      as_tibble() |>
      mutate(value=gsub("^ +| *$","",.data$value))

    campumf_layout_path <- file.path(canpumf_dir,"layout.Rds")
    if (!file.exists(campumf_layout_path)) {
      layout <- get_census_dat_layout(spss)

      saveRDS(layout,campumf_layout_path)
    }



    formats <- which(grepl("FORMATS$",spss$value))
    blanks <- which(""==spss$value)
    starts <- which(grepl("LABELS$",spss$value))
    ends <- which(grepl("^ *\\.$",spss$value))

    format_data <- spss |>
      slice(seq(formats+1,blanks[blanks>formats][1]-1)) |>
      pull(.data$value) |>
      paste0(collapse = " ") |>
      gsub(" *\\. *$","",x=_) |>
      gsub("/$","",x=_) |>
      strsplit("/") |>
      unlist() |>
      gsub("^ +| +$","",x=_) |>
      as_tibble() |>
      mutate(p=strsplit(.data$value," +")) |>
      mutate(name=lapply(.data$p,first) |> unlist(),
             value=lapply(.data$p,last) |> unlist()) |>
      select(.data$name,.data$value)


    name_labels <- spss |>
      slice(seq(starts[1]+1,starts[2]-1)) |>
      mutate(value=gsub("^ +| *\\.$","",.data$value)) |>
      mutate(name=gsub(" .+$","",.data$value)) |>
      mutate(label=str_extract(.data$value,'".+"') |> gsub('"',"",x=_)) |>
      select(.data$name,.data$label) |>
      filter(!grepl("^WEIGHT$|^WT\\d+$",.data$name)) |>
      filter(.data$name!="")

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
        if (n=="") {
          s=s+1
          n<-var_labels_raw$value[s] |> gsub("^\\/","",x=_)
        }
        s=s+1
        if (r==length(var_starts)) {
          e=nrow(var_labels_raw)
        } else {
          e=var_starts[r+1]-1
        }
        vr <- var_labels_raw |>
          slice(s:e) |>
          mutate(val=gsub(' *".+',"",.data$value),
                 label=str_extract(.data$value,'".+"') |> gsub('"',"",x=_)) |>
          mutate(name=n)
      }) |>
      filter(.data$value!="/")

    saveRDS(val_labels,file.path(canpumf_dir,"val.Rds"))
  }
}

ensure_2011_pumfi_metadata <- function(pumf_base_path){
  canpumf_dir <- file.path(pumf_base_path,"canpumf")

  if (!dir.exists(canpumf_dir)||length(dir(canpumf_dir))<2) {
    if (!dir.exists(canpumf_dir)) dir.create(canpumf_dir)
    layout_path <- dir(pumf_base_path,"Individual file",full.names = TRUE)
    layout_path <- dir(layout_path,"English",full.names = TRUE)
    layout_path <- dir(layout_path,"SPSS",full.names = TRUE)

    spss <- readr::read_lines(file.path(layout_path,dir(layout_path,"\\.sps|\\.SPS$")),
                              locale = readr::locale(encoding = "CP1252")) |>
      as_tibble() |>
      mutate(value=gsub("^ +| *$","",.data$value))

    campumf_layout_path <- file.path(canpumf_dir,"layout.Rds")
    if (!file.exists(campumf_layout_path)) {
      layout <- get_census_dat_layout(spss)

      saveRDS(layout,campumf_layout_path)
    }



    formats <- which(grepl("FORMATS$",spss$value))
    blanks <- which(""==spss$value)
    starts <- which(grepl("LABELS$",spss$value))
    ends <- which(grepl("^ *\\.$",spss$value))

    format_data <- spss |>
      slice(seq(formats+1,blanks[blanks>formats][1]-1)) |>
      pull(.data$value) |>
      paste0(collapse = " ") |>
      gsub(" *\\. *$","",x=_) |>
      gsub("/$","",x=_) |>
      strsplit("/") |>
      unlist() |>
      gsub("^ +| +$","",x=_) |>
      as_tibble() |>
      mutate(p=strsplit(.data$value," +")) |>
      mutate(name=lapply(.data$p,first) |> unlist(),
             value=lapply(.data$p,last) |> unlist()) |>
      select(.data$name,.data$value)

      var_labels_raw <- spss |>
      slice(seq(starts[1]+1,last(ends)-1)) |>
      mutate(value=gsub("^ +| *\\.$| *$","",.data$value))

    single_quotes <- which(grepl("'$|' *\\+ *$",var_labels_raw$value))
    var_labels_raw$value[single_quotes] <- gsub("'",'"',var_labels_raw$value[single_quotes])

    var_starts <- which(grepl("^\\/",var_labels_raw$value))

    val_labels <- 1:length(var_starts) |>
      purrr::map_df(\(r){
        s=var_starts[r]
        n<-var_labels_raw$value[s] |> gsub("^\\/","",x=_)
        if (n=="") {
          s=s+1
          n<-var_labels_raw$value[s] |> gsub("^\\/","",x=_)
        }
        s=s+1
        if (r==length(var_starts)) {
          e=nrow(var_labels_raw)
        } else {
          e=var_starts[r+1]-1
        }
        vr <- var_labels_raw |>
          slice(s:e) |>
          mutate(val=gsub(' *".+',"",.data$value),
                 label=str_extract(.data$value,'".+"') |> gsub('"',"",x=_)) |>
          mutate(name=gsub("_$","",n))
      }) |>
      filter(.data$value!="/")

    saveRDS(val_labels,file.path(canpumf_dir,"val.Rds"))

    name_labels <- val_labels |>
      select(.data$name) |>
      distinct() |>
      mutate(label=.data$name)
    saveRDS(name_labels,file.path(canpumf_dir,"var.Rds"))
  }
}



ensure_2006_pumfi_metadata <- function(pumf_base_path){
  canpumf_dir <- file.path(pumf_base_path,"canpumf")

  if (!dir.exists(canpumf_dir)||length(dir(canpumf_dir))<2) {
    if (!dir.exists(canpumf_dir)) dir.create(canpumf_dir)
    layout_path <- dir(pumf_base_path,"English",full.names = TRUE)
    layout_path <- dir(layout_path,"SPSS",full.names = TRUE)

    spss <- readr::read_lines(file.path(layout_path,dir(layout_path,"\\.sps|\\.SPS$")),
                              locale = readr::locale(encoding = "CP1252")) |>
      as_tibble() |>
      mutate(value=gsub("^ +| *$","",.data$value))

    campumf_layout_path <- file.path(canpumf_dir,"layout.Rds")
    if (!file.exists(campumf_layout_path)) {
      layout <- get_census_dat_layout(spss)

      saveRDS(layout,campumf_layout_path)
    }



    formats <- which(grepl("FORMATS$",spss$value))
    blanks <- which(""==spss$value)
    starts <- which(grepl("LABELS$",spss$value))
    ends <- which(grepl("^ *\\.$",spss$value))

    format_data <- spss |>
      slice(seq(formats+1,blanks[blanks>formats][1]-1)) |>
      pull(.data$value) |>
      paste0(collapse = " ") |>
      gsub(" *\\. *$","",x=_) |>
      gsub("/$","",x=_) |>
      strsplit("/") |>
      unlist() |>
      gsub("^ +| +$","",x=_) |>
      as_tibble() |>
      mutate(p=strsplit(.data$value," +")) |>
      mutate(name=lapply(.data$p,first) |> unlist(),
             value=lapply(.data$p,last) |> unlist()) |>
      select(.data$name,.data$value)


    name_labels <- spss |>
      slice(seq(starts[1]+1,ends[1]-1)) |>
      mutate(value=gsub("\\t"," ",.data$value)) |>
      mutate(value=gsub("^ +| *\\.$","",.data$value)) |>
      mutate(name=gsub(" .+$","",.data$value)) |>
      mutate(label=str_extract(.data$value,'".+"') |> gsub('"',"",x=_)) |>
      select(.data$name,.data$label) |>
      filter(!grepl("^WEIGHT$|^WT\\d+$",.data$name)) |>
      filter(.data$name!="")

    saveRDS(name_labels,file.path(canpumf_dir,"var.Rds"))

    var_labels_raw <- spss |>
      slice(seq(starts[1]+1,last(ends)-1)) |>
      mutate(value=gsub("^ +| *\\.$| *$","",.data$value)) |>
      mutate(value=gsub("\\t"," ",.data$value))

    single_quotes <- which(grepl("'$|' *\\+ *$",var_labels_raw$value))
    var_labels_raw$value[single_quotes] <- gsub("'",'"',var_labels_raw$value[single_quotes])

    var_starts <- which(grepl("^\\/",var_labels_raw$value))

    val_labels <- 1:length(var_starts) |>
      purrr::map_df(\(r){
        s=var_starts[r]
        n<-var_labels_raw$value[s] |> gsub("^\\/","",x=_)
        if (n=="") {
          s=s+1
          n<-var_labels_raw$value[s] |> gsub("^\\/","",x=_)
        }
        s=s+1
        if (r==length(var_starts)) {
          e=nrow(var_labels_raw)
        } else {
          e=var_starts[r+1]-1
        }
        vr <- var_labels_raw |>
          slice(s:e) |>
          mutate(val=gsub(' *".+',"",.data$value),
                 label=str_extract(.data$value,'".+"') |> gsub('"',"",x=_)) |>
          mutate(name=gsub("_$","",n))
      }) |>
      filter(.data$value!="/")

    saveRDS(val_labels,file.path(canpumf_dir,"val.Rds"))
  }
}


ensure_2001_pumfi_metadata <- function(pumf_base_path){
  canpumf_dir <- file.path(pumf_base_path,"canpumf")

  if (!dir.exists(canpumf_dir)||length(dir(canpumf_dir))<2) {
    if (!dir.exists(canpumf_dir)) dir.create(canpumf_dir)
    layout_path <- dir(pumf_base_path,"English",full.names = TRUE,ignore.case = TRUE)
    layout_path <- dir(layout_path,"SPSS",full.names = TRUE)

    spss <- readr::read_lines(file.path(layout_path,dir(layout_path,"2001ESPSS")),
                              locale = readr::locale(encoding = "CP1252")) |>
      as_tibble() |>
      mutate(value=gsub("^ +| *$","",.data$value)) |>
      mutate(value=gsub("\\t"," ",.data$value))

    campumf_layout_path <- file.path(canpumf_dir,"layout.Rds")
    if (!file.exists(campumf_layout_path)) {
      layout <- get_census_dat_layout_alt(spss)

      saveRDS(layout,campumf_layout_path)
    }



    blanks <- which(""==spss$value)
    starts <- which(grepl("LABELS$",spss$value))
    ends <- which(grepl("\\. *$",spss$value))


    name_labels <- spss |>
      slice(seq(starts[1]+1,ends[ends>starts[1]][1]-1)) |>
      mutate(value=gsub("\\t"," ",.data$value)) |>
      mutate(value=gsub("^ +| *\\.$","",.data$value)) |>
      mutate(name=gsub(" .+$","",.data$value)) |>
      mutate(label=str_extract(.data$value," '.+' *") |> gsub("^ '|' *$","",x=_)) |>
      mutate(label=coalesce(.data$label,str_extract(.data$value,'".+"') |> gsub('"',"",x=_))) |>
      select(.data$name,.data$label) |>
      filter(!grepl("^WEIGHT$|WEIGHTP$|^WT\\d+$",.data$name)) |>
      filter(.data$name!="") |>
      mutate(n=n(),.by=.data$label) |>
      mutate(label=ifelse(.data$n==1,.data$label,paste0(.data$label," (",.data$name,")"))) |>
      select(-.data$n)

    saveRDS(name_labels,file.path(canpumf_dir,"var.Rds"))

    var_labels_raw <- spss |>
      slice(seq(starts[2]+1,ends[ends>starts[2]][1]-1)) |>
      mutate(value=gsub("^ +| *\\.$| *$","",.data$value)) |>
      mutate(value=gsub("\\t"," ",.data$value))

    var_ends <- which(grepl("\\/$",var_labels_raw$value))
    var_starts <- c(1,var_ends+1)[1:length(var_ends)]

    val_labels <- 1:length(var_starts) |>
      purrr::map_df(\(r){
        s=var_starts[r]
        e=var_ends[r]
        n<-var_labels_raw$value[s] |> gsub("\\/$","",x=_)
        if (n=="") {
          s=s+1
          n<-var_labels_raw$value[s] |> gsub("^\\/","",x=_)
        }
        s=s+1
        vr <- var_labels_raw |>
          slice(s:e) |>
          mutate(val=gsub(" *'.+","",.data$value),
                 label=str_extract(.data$value,"'.+'") |> gsub("'","",x=_)) |>
          mutate(name=gsub("_$","",n))
      }) |>
      filter(.data$value!="/")

    saveRDS(val_labels,file.path(canpumf_dir,"val.Rds"))
  }
}

ensure_1996_pumfi_metadata <- function(pumf_base_path){
  canpumf_dir <- file.path(pumf_base_path,"canpumf")

  if (!dir.exists(canpumf_dir)||length(dir(canpumf_dir))<2) {
    if (!dir.exists(canpumf_dir)) dir.create(canpumf_dir)
    layout_path <- dir(pumf_base_path,"English",full.names = TRUE,ignore.case = TRUE)
    layout_path <- dir(layout_path,"SPSS",full.names = TRUE,ignore.case = TRUE)

    spss <- readr::read_lines(file.path(layout_path,dir(layout_path,"\\.sps$")),
                              locale = readr::locale(encoding = "CP1252")) |>
      as_tibble() |>
      mutate(value=gsub("^ +| *$","",.data$value)) |>
      mutate(value=gsub("\\t"," ",.data$value))

    campumf_layout_path <- file.path(canpumf_dir,"layout.Rds")
    if (!file.exists(campumf_layout_path)) {
      layout <- get_census_dat_layout_alt(spss)

      saveRDS(layout,campumf_layout_path)
    }



    blanks <- which(""==spss$value)
    starts <- which(grepl("LABELS$",spss$value))
    ends <- which(grepl("\\. *$",spss$value))


    name_labels <- spss |>
      slice(seq(starts[1]+1,ends[ends>starts[1]][1]-1)) |>
      mutate(value=gsub("\\t"," ",.data$value)) |>
      mutate(value=gsub("^ +| *\\.$","",.data$value)) |>
      mutate(name=gsub(" .+$","",.data$value)) |>
      mutate(label=str_extract(.data$value," '.+' *") |> gsub("^ '|' *$","",x=_)) |>
      mutate(label=coalesce(.data$label,str_extract(.data$value,'".+"') |> gsub('"',"",x=_))) |>
      select(.data$name,.data$label) |>
      filter(!grepl("^WEIGHT$|WEIGHTP$|^WT\\d+$",.data$name)) |>
      filter(.data$name!="") |>
      mutate(n=n(),.by=.data$label) |>
      mutate(label=ifelse(.data$n==1,.data$label,paste0(.data$label," (",.data$name,")"))) |>
      select(-.data$n)

    saveRDS(name_labels,file.path(canpumf_dir,"var.Rds"))

    var_labels_raw <- spss |>
      slice(seq(starts[2]+1,ends[ends>starts[2]][1]-1)) |>
      mutate(value=gsub("^ +| *\\.$| *$","",.data$value)) |>
      mutate(value=gsub("\\t"," ",.data$value))

    var_ends <- which(grepl("\\/$",var_labels_raw$value))
    var_starts <- c(1,var_ends+1)[1:length(var_ends)]

    val_labels <- 1:length(var_starts) |>
      purrr::map_df(\(r){
        s=var_starts[r]
        e=var_ends[r]
        n<-var_labels_raw$value[s] |> gsub("\\/$","",x=_)
        if (n=="") {
          s=s+1
          n<-var_labels_raw$value[s] |> gsub("^\\/","",x=_)
        }
        s=s+1
        vr <- var_labels_raw |>
          slice(s:e) |>
          mutate(val=gsub(" *'.+","",.data$value),
                 label=str_extract(.data$value,"'.+'") |> gsub("'","",x=_)) |>
          mutate(name=gsub("_$","",n))
      }) |>
      filter(.data$value!="/")

    saveRDS(val_labels,file.path(canpumf_dir,"val.Rds"))
  }
}

get_census_dat_layout <- function(spss) {
  start_index <- which(grepl("^DATA ",spss$value))
  end_index <- which(grepl(" *\\.$|^ *$",spss$value))
  end_index <- end_index[end_index>start_index] %>% first

  rows <- spss %>%
    slice(seq(start_index+1,end_index)) %>%
    summarise(value=paste0(.data$value,collapse=".   ")) %>%
    pull(.data$value) %>%
    gsub(" +\\. *$","",.) %>%
    strsplit("  +") %>%
    unlist() |>
    gsub("\\.$","",x=_)
  layout <- tibble(value=rows) %>%
    filter(.data$value!="") %>%
    mutate(name=stringr::str_extract(.data$value,"^[A-Za-z0-9_]+")) %>%
    mutate(start=stringr::str_extract(.data$value," \\d+")) %>%
    mutate(end=stringr::str_extract(.data$value,"\\d+$")) %>%
    mutate(note="") %>%
    select(-.data$value) %>%
    mutate_at(c("start","end"),as.integer)
  layout
}

get_census_dat_layout_alt <- function(spss) {
  start_index <- which(grepl("^DATA ",spss$value))
  end_index <- which(grepl(" *\\.$|^ *$",spss$value))
  end_index <- end_index[end_index>start_index] %>% first

  rows <- spss %>%
    slice(seq(start_index+1,end_index)) %>%
    mutate(value=gsub("^/|\\.$","",.data$value))
  layout <- rows %>%
    filter(.data$value!="") %>%
    mutate(name=stringr::str_extract(.data$value,"^[A-Za-z0-9_]+")) %>%
    mutate(start=stringr::str_extract(.data$value," \\d+")) %>%
    mutate(end=stringr::str_extract(.data$value,"\\d+$")) %>%
    mutate(note="") %>%
    select(-.data$value) %>%
    mutate_at(c("start","end"),as.integer)
  layout
}



get_census_pumf <- function(pumf_version,pumf_cache_path){
  cached_pumf <- dir(pumf_cache_path)
  if (pumf_version=="2021 (individuals)"|pumf_version=="2021") {
    path <- cached_pumf[grepl("98M0001X",cached_pumf)&grepl("2021",cached_pumf)&!grepl("\\.zip$",cached_pumf)]
    if (length(path)==1) {
      pumf_base_path <- file.path(pumf_cache_path,path)
      pumf_data_file <- dir(pumf_base_path,"\\.csv",full.names = TRUE)
      pumf_data <- readr::read_csv(pumf_data_file,
                                   col_types = readr::cols(.default="c"),
                                   locale = readr::locale(encoding = "CP1252")) |>
        mutate(across(matches("^WEIGHT$|^WT\\d+$"),as.numeric))

      if ("RELIGION_DER" %in% names(pumf_data)) { # coding error in older version of pumf data
        pumf_data <- pumf_data |>
          rename(RELIG=.data$RELIGION_DER)
      }

      attr(pumf_data,"pumf_base_path") <- pumf_base_path

      ensure_2021_pumfi_metadata(pumf_base_path)

    } else {
      stop("2021 PUMF data is not avaialble")
    }
  } else if (pumf_version=="2016 (individuals)"|pumf_version=="2016"){
    path <- cached_pumf[grepl("98M0001X",cached_pumf)&grepl("2016",cached_pumf)&!grepl("\\.zip$",cached_pumf)]
    if (length(path)==1) {
      pumf_base_path <- file.path(pumf_cache_path,path)
      pumf_data_file <- dir(pumf_base_path,"\\.dat",full.names = TRUE)

      ensure_2016_pumfi_metadata(pumf_base_path)

      canpumf_dir <- file.path(pumf_base_path,"canpumf")
      layout_path <- file.path(canpumf_dir,"layout.Rds")
      layout <- readRDS(layout_path)

      pumf_data <- readr::read_fwf(pumf_data_file,
                                   col_types = readr::cols(.default="c"),
                                   trim_ws=TRUE,
                                   col_positions = readr::fwf_positions(layout$start,
                                                                        layout$end,
                                                                        col_names = layout$name),
                                   locale = readr::locale(encoding = "CP1252")) |>
        #mutate(across(everything(),\(x)gsub(" *","",x))) |> # remove spaces
        mutate(across(matches("^WEIGHT$|^WT\\d+$"),\(x)gsub(" *","",x) |> as.numeric()))

      attr(pumf_data,"pumf_base_path") <- pumf_base_path


    } else {
      stop("2016 PUMF data is not avaialble")
    }
  } else if (pumf_version=="2011 (individuals)"|pumf_version=="2011"){
    path <- cached_pumf[grepl("99M0001X",cached_pumf)&grepl("2011",cached_pumf)&!grepl("\\.zip$",cached_pumf)]
    if (length(path)==1) {
      pumf_base_path <- file.path(pumf_cache_path,path)
      individuals_path <- dir(pumf_base_path,"Individual",full.names = TRUE)
      pumf_data_file <- dir(individuals_path,"\\.dat",full.names = TRUE)

      ensure_2011_pumfi_metadata(pumf_base_path)

      canpumf_dir <- file.path(pumf_base_path,"canpumf")
      layout_path <- file.path(canpumf_dir,"layout.Rds")
      layout <- readRDS(layout_path)

      pumf_data <- readr::read_fwf(pumf_data_file,
                                   col_types = readr::cols(.default="c"),
                                   trim_ws=TRUE,
                                   col_positions = readr::fwf_positions(layout$start,
                                                                        layout$end,
                                                                        col_names = layout$name),
                                   locale = readr::locale(encoding = "CP1252")) |>
        #mutate(across(everything(),\(x)gsub(" *","",x))) |> # remove spaces
        mutate(across(matches("^WEIGHT$|^WEIGHTP$|^WT\\d+$"),\(x)gsub(" *","",x) |> as.numeric()))

      attr(pumf_data,"pumf_base_path") <- pumf_base_path

    } else {
      stop("2011 PUMF data is not avaialble")
    }

  } else if (pumf_version=="2006 (individuals)"|pumf_version=="2006"){
    path <- cached_pumf[grepl("95M0028X",cached_pumf)&grepl("2006",cached_pumf)&!grepl("\\.zip$",cached_pumf)]
    if (length(path)==1) {
      pumf_base_path <- file.path(pumf_cache_path,path)
      individuals_path <- pumf_base_path
      pumf_data_file <- dir(individuals_path,"\\.dat",full.names = TRUE)

      ensure_2006_pumfi_metadata(pumf_base_path)

      canpumf_dir <- file.path(pumf_base_path,"canpumf")
      layout_path <- file.path(canpumf_dir,"layout.Rds")
      layout <- readRDS(layout_path)

      pumf_data <- readr::read_fwf(pumf_data_file,
                                   col_types = readr::cols(.default="c"),
                                   trim_ws=TRUE,
                                   col_positions = readr::fwf_positions(layout$start,
                                                                        layout$end,
                                                                        col_names = layout$name),
                                   locale = readr::locale(encoding = "CP1252")) |>
        #mutate(across(everything(),\(x)gsub(" *","",x))) |> # remove spaces
        mutate(across(matches("^WEIGHT$|^WT\\d+$"),\(x)gsub(" *","",x) |> as.numeric()))

      attr(pumf_data,"pumf_base_path") <- pumf_base_path
    } else {
      stop("2001 PUMF data is not avaialble")
    }
  } else if (pumf_version=="2001 (individuals)"|pumf_version=="2001"){
    path <- cached_pumf[grepl("95M0016X",cached_pumf)&grepl("2001",cached_pumf)&!grepl("\\.zip$",cached_pumf)]
    if (length(path)==1) {
      pumf_base_path <- file.path(pumf_cache_path,path)
      individuals_path <- pumf_base_path
      pumf_data_file <- dir(individuals_path,"\\.dat",full.names = TRUE)

      ensure_2001_pumfi_metadata(pumf_base_path)

      canpumf_dir <- file.path(pumf_base_path,"canpumf")
      layout_path <- file.path(canpumf_dir,"layout.Rds")
      layout <- readRDS(layout_path)

      pumf_data <- readr::read_fwf(pumf_data_file,
                                   col_types = readr::cols(.default="c"),
                                   trim_ws=TRUE,
                                   col_positions = readr::fwf_positions(layout$start,
                                                                        layout$end,
                                                                        col_names = layout$name),
                                   locale = readr::locale(encoding = "CP1252")) |>
        #mutate(across(everything(),\(x)gsub(" *","",x))) |> # remove spaces
        mutate(across(matches("^WEIGHT$|WEIGHTP$|^WT\\d+$"),\(x)gsub(" *","",x) |> as.numeric()))

      attr(pumf_data,"pumf_base_path") <- pumf_base_path
    } else {
      stop("1996 PUMF data is not avaialble")
    }
  } else if (pumf_version=="1996 (individuals)"|pumf_version=="1996"){
    path <- cached_pumf[grepl("95M0010X",cached_pumf)&grepl("1996",cached_pumf)&!grepl("\\.zip$",cached_pumf)]
    if (length(path)==1) {
      pumf_base_path <- file.path(pumf_cache_path,path)
      individuals_path <- pumf_base_path
      pumf_data_file <- dir(individuals_path,"\\.dat",full.names = TRUE)

      ensure_1996_pumfi_metadata(pumf_base_path)

      canpumf_dir <- file.path(pumf_base_path,"canpumf")
      layout_path <- file.path(canpumf_dir,"layout.Rds")
      layout <- readRDS(layout_path)

      pumf_data <- readr::read_fwf(pumf_data_file,
                                   col_types = readr::cols(.default="c"),
                                   trim_ws=TRUE,
                                   col_positions = readr::fwf_positions(layout$start,
                                                                        layout$end,
                                                                        col_names = layout$name),
                                   locale = readr::locale(encoding = "CP1252")) |>
        #mutate(across(everything(),\(x)gsub(" *","",x))) |> # remove spaces
        mutate(across(matches("^WEIGHT$|WEIGHTP$|^WT\\d+$"),\(x)gsub(" *","",x) |> as.numeric()))

      attr(pumf_data,"pumf_base_path") <- pumf_base_path

    } else {
      stop("1996 PUMF data is not avaialble")
    }
  } else {
    stop("PUMF version not supported")
  }
  pumf_data
}

