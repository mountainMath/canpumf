ensure_2021_pumfi_metadata <- function(pumf_base_path,refresh_layout=FALSE){
  canpumf_dir <- file.path(pumf_base_path,"canpumf")

  if (!dir.exists(canpumf_dir)||length(dir(canpumf_dir))<2|refresh_layout) {
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


ensure_2016_pumf_metadata <- function(pumf_base_path,refresh_layout=FALSE){
  canpumf_dir <- file.path(pumf_base_path,"canpumf")

  if (!dir.exists(canpumf_dir)||length(dir(canpumf_dir))<2|refresh_layout) {
    if (!dir.exists(canpumf_dir)) dir.create(canpumf_dir)
    layout_path <- dir(pumf_base_path,"English",full.names = TRUE)
    layout_path <- dir(layout_path,"SPSS",full.names = TRUE)

    spss <- readr::read_lines(file.path(layout_path,dir(layout_path,"\\.sps$")),
                              locale = readr::locale(encoding = "CP1252")) |>
      as_tibble() |>
      mutate(value=gsub("^ +| *$","",.data$value))

    campumf_layout_path <- file.path(canpumf_dir,"layout.Rds")
    if (!file.exists(campumf_layout_path)|refresh_layout) {
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


ensure_2011_pumf_metadata <- function(pumf_base_path,refresh_layout=FALSE){
  canpumf_dir <- file.path(pumf_base_path,"canpumf")

  if (!dir.exists(canpumf_dir)||length(dir(canpumf_dir))<2|refresh_layout) {
    if (!dir.exists(canpumf_dir)) dir.create(canpumf_dir)
    layout_path <- dir(pumf_base_path,"Individual file",full.names = TRUE)
    if (length(layout_path)==0) layout_path <- pumf_base_path
    layout_path <- dir(layout_path,"English",full.names = TRUE)
    layout_path <- dir(layout_path,"SPSS",full.names = TRUE)

    spss <- readr::read_lines(file.path(layout_path,dir(layout_path,"\\.sps|\\.SPS$")),
                              locale = readr::locale(encoding = "CP1252")) |>
      as_tibble() |>
      mutate(value=gsub("^ +| *$","",.data$value))

    campumf_layout_path <- file.path(canpumf_dir,"layout.Rds")
    if (!file.exists(campumf_layout_path)|refresh_layout) {
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



ensure_2006_pumf_metadata <- function(pumf_base_path,refresh_layout=FALSE){
  canpumf_dir <- file.path(pumf_base_path,"canpumf")

  if (!dir.exists(canpumf_dir)||length(dir(canpumf_dir))<2|refresh_layout) {
    if (!dir.exists(canpumf_dir)) dir.create(canpumf_dir)
    layout_path <- dir(pumf_base_path,"English",full.names = TRUE)
    layout_path <- dir(layout_path,"SPSS",full.names = TRUE)

    spss <- readr::read_lines(file.path(layout_path,dir(layout_path,"\\.sps|\\.SPS$")),
                              locale = readr::locale(encoding = "CP1252")) |>
      as_tibble() |>
      mutate(value=gsub("^ +| *$","",.data$value))

    campumf_layout_path <- file.path(canpumf_dir,"layout.Rds")
    if (!file.exists(campumf_layout_path)|refresh_layout) {
      layout <- get_census_dat_layout(spss)

      saveRDS(layout,campumf_layout_path)
    }



    formats <- which(grepl("FORMATS$",spss$value))
    blanks <- which(""==spss$value)
    starts <- which(grepl("LABELS$",spss$value))
    # ends <- which(grepl("^ *\\.$",spss$value))
    ends_raw <- which(grepl("\\.$",spss$value))
    ends=c(0,0)
    ends[1]=ends_raw[ends_raw>starts[1]][1]
    ends[2]=ends_raw[ends_raw>starts[2]][1]

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
      slice(seq(starts[1]+1,ends[1]-1))

    if (sum(grepl("\\t",name_labels$value))==0) {
      name_labels <- name_labels |>
        mutate(name=strsplit(.data$value," ") |> lapply(first) |> unlist()) |>
        mutate(label=strsplit(.data$value," ") |> lapply(\(x)paste0(x[2:length(x)],collapse = " ")) |> unlist())
    } else {
      name_labels <- name_labels |>
        mutate(name=strsplit(.data$value,"\\t") |> lapply(first) |> unlist(),
               label=strsplit(.data$value,"\\t") |> lapply(last) |> unlist())
    }

    name_labels <- name_labels |>
      mutate(label=gsub("^'|'$","",.data$label)) |>
      mutate(label=gsub('^"|"$',"",.data$label)) |>
      select(.data$name,.data$label) |>
      filter(!grepl("^WEIGHT$|^WT\\d+$",.data$name)) |>
      filter(.data$name!="")


    saveRDS(name_labels,file.path(canpumf_dir,"var.Rds"))

    var_labels_raw <- spss |>
      slice(seq(starts[2]+1,last(ends)-1)) |>
      filter(.data$value!="/") |>
      mutate(value=gsub(" */$","",.data$value))
    var_starts <- which(!grepl("\\t",var_labels_raw$value))

    if (length(var_starts)==nrow(var_labels_raw)) {
      var_starts <- which(strsplit(var_labels_raw$value," ") |> lapply(length) |> unlist()==1)
    }


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
          slice(s:e)
        if (sum(grepl("\\t",vr$value))==0) {
          vr <- vr |>
            mutate(val=strsplit(.data$value," ") |> lapply(first) |> unlist()) |>
            mutate(label=strsplit(.data$value," ") |> lapply(\(x)paste0(x[2:length(x)],collapse = " ")) |> unlist())
        } else {
          vr <- vr |>
            mutate(val=strsplit(.data$value,"\t") |> lapply(first) |> unlist(),
                   label=strsplit(.data$value,"\t") |> lapply(last) |> unlist())
        }
        vr <- vr |>
          mutate(label=gsub("^'|'$","",.data$label)) |>
          mutate(label=gsub('^"|"$',"",.data$label)) |>
          mutate(name=gsub("_$","",n))
        vr
      }) |>
      filter(.data$value!="/")

    saveRDS(val_labels,file.path(canpumf_dir,"val.Rds"))
  }
}


ensure_2001_pumf_metadata <- function(pumf_base_path,refresh_layout=FALSE){
  canpumf_dir <- file.path(pumf_base_path,"canpumf")

  if (!dir.exists(canpumf_dir)||length(dir(canpumf_dir))<2|refresh_layout) {
    if (!dir.exists(canpumf_dir)) dir.create(canpumf_dir)
    layout_path <- dir(pumf_base_path,"English",full.names = TRUE,ignore.case = TRUE)
    layout_path <- dir(layout_path,"SPSS",full.names = TRUE)

    spss <- readr::read_lines(file.path(layout_path,dir(layout_path,"2001ESPSS")),
                              locale = readr::locale(encoding = "CP1252")) |>
      as_tibble() |>
      mutate(value=gsub("^ +| *$","",.data$value)) |>
      mutate(value=gsub("\\t"," ",.data$value))

    campumf_layout_path <- file.path(canpumf_dir,"layout.Rds")
    if (!file.exists(campumf_layout_path)|refresh_layout) {
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

ensure_1996_pumf_metadata <- function(pumf_base_path,refresh_layout=FALSE){
  canpumf_dir <- file.path(pumf_base_path,"canpumf")

  if (!dir.exists(canpumf_dir)||length(dir(canpumf_dir))<2|refresh_layout) {
    if (!dir.exists(canpumf_dir)) dir.create(canpumf_dir)
    layout_path <- dir(pumf_base_path,"English",full.names = TRUE,ignore.case = TRUE)
    layout_path <- dir(layout_path,"SPSS",full.names = TRUE,ignore.case = TRUE)

    spss <- readr::read_lines(file.path(layout_path,dir(layout_path,"\\.sps$")),
                              locale = readr::locale(encoding = "CP1252")) |>
      as_tibble() |>
      mutate(value=gsub("^ +| *$","",.data$value)) |>
      mutate(value=gsub("\\t"," ",.data$value))

    campumf_layout_path <- file.path(canpumf_dir,"layout.Rds")
    if (!file.exists(campumf_layout_path)|refresh_layout) {
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
        vr <- var_labels_raw |>
          slice(s:e)
        s=1
        e=nrow(vr)
        n<-vr$value[s] |> gsub("\\/$","",x=_)
        if (n=="") {
          s=s+1
          n<-vr$value[s] |> gsub("^\\/","",x=_)
        }
        if (length(unlist(str_split(n," ")))>1) {
          nn <- n
          n <- str_split(nn," +") |> unlist() |> first()
          x<-str_split(vr$value[1]," +") |> unlist()
          vr$value[s] <- paste0(x[2:length(x)],collapse = " ")
          s=s-1
        }
        s=s+1
        vr <- vr |>
          mutate(val=gsub(" *'.+","",.data$value),
                 label=str_extract(.data$value,"'.+'") |> gsub("'","",x=_)) |>
          mutate(name=gsub("_$","",n))
      }) |>
      filter(.data$value!="/")

    saveRDS(val_labels,file.path(canpumf_dir,"val.Rds"))
  }
}

get_year_level_version<-function(pumf_version){
  year <- substr(pumf_version,1,4)
  level <- NULL
  version <- NULL
  if (grepl("^\\d{4}$",pumf_version)||grepl("individuals",pumf_version)) {
    version<-"individuals"
  } else if (grepl("households",pumf_version)) {
    version <- "households"
  } else if (grepl("families",pumf_version)) {
    version <- "families"
  }

  if (!grepl("PR",pumf_version)) {
    level<-"CMA"
  } else {
    level="PR"
  }
  list(year=year,version=version,level=level)

}

ensure_1971_1991_pumf_metadata <- function(pumf_base_path,pumf_version,refresh_layout=FALSE){
  ylv <- get_year_level_version(pumf_version)
  canpumf_dir_base <- file.path(pumf_base_path,paste0(ylv,collapse = "_"))
  canpumf_dir <- file.path(canpumf_dir_base,"canpumf")

  if (!dir.exists(canpumf_dir)||length(dir(canpumf_dir))<2|refresh_layout) {
    if (!dir.exists(canpumf_dir_base)) dir.create(canpumf_dir_base)
    if (!dir.exists(canpumf_dir)) dir.create(canpumf_dir)
    layout_path <- dir(pumf_base_path,"eng\\.sps",full.names = TRUE,ignore.case = TRUE)
    if (ylv$version=="individuals") {
      layout_path <- layout_path[grepl("ind",layout_path)]
    }
    if (length(layout_path)>1) {
      if (ylv$level=="CMA") {
        layout_path <- layout_path[grepl("cma",layout_path)]
      } else {
        layout_path <- layout_path[grepl("pr",layout_path)]
      }
    }

    spss <- readr::read_lines(layout_path,
                              locale = readr::locale(encoding = "CP1252")) |>
      as_tibble() |>
      mutate(value=gsub("^ +| *$","",.data$value)) |>
      mutate(value=gsub("\\t"," ",.data$value)) |>
      mutate(value=gsub(" *\\&amp; *"," & ",.data$value)) |>
      mutate(value=gsub("\\&lt;","<",.data$value)) |>
      mutate(value=gsub("\\&gt;",">",.data$value))

    campumf_layout_path <- file.path(canpumf_dir,"layout.Rds")
    if (!file.exists(campumf_layout_path)|refresh_layout) {
      layout <- get_census_dat_layout_alt(spss)

      saveRDS(layout,campumf_layout_path)
    }



    blanks <- which(""==spss$value)
    starts <- which(grepl("LABELS$",spss$value))
    ends_raw <- which(grepl("\\.$",spss$value))
    ends=c(0,0)
    ends[1]=ends_raw[ends_raw>starts[1]][1]
    ends[2]=ends_raw[ends_raw>starts[2]][1]


    name_labels <- spss |>
      slice(seq(starts[1]+1,ends[1]-1))

    if (sum(grepl("\\t",name_labels$value))==0) {
      name_labels <- name_labels |>
        mutate(name=strsplit(.data$value," +") |> lapply(first) |> unlist()) |>
        mutate(label=strsplit(.data$value," +") |> lapply(\(x)paste0(x[2:length(x)],collapse = " ")) |> unlist())
    } else {
      name_labels <- name_labels |>
        mutate(name=strsplit(.data$value,"\\t") |> lapply(first) |> unlist(),
               label=strsplit(.data$value,"\\t") |> lapply(last) |> unlist())
    }

    name_labels <- name_labels |>
      mutate(label=gsub("^'|'$","",.data$label)) |>
      mutate(label=gsub('^"|"$',"",.data$label)) |>
      select(.data$name,.data$label) |>
      filter(!grepl("^WEIGHT$|^WT\\d+$",.data$name)) |>
      filter(.data$name!="")


    saveRDS(name_labels,file.path(canpumf_dir,"var.Rds"))

    var_labels_raw <- spss |>
      slice(seq(starts[2]+1,last(ends))) |>
      mutate(value=gsub("^/\\.$","/",.data$value))
    var_ends <- which(grepl("/$",var_labels_raw$value))
    var_starts <- c(1,var_ends[1:length(var_ends)-1]+1)


    val_labels <- 1:length(var_starts) |>
      purrr::map_df(\(r){
        s=var_starts[r]
        e=var_ends[r]
        vr<-var_labels_raw |> slice(s:(e-1))
        vr1 <- vr$value[1] |> strsplit(" +") |> unlist()
        n<-vr1[1]
        if (length(vr1)>1) {
          vr$value[1] <- vr$value[1] |> gsub(paste0(n," +"),"",x=_)
        } else {
          vr <- vr |> slice(-1)
        }
        vr <- vr |>
          mutate(val=strsplit(.data$value," +") |> lapply(first) |> unlist()) |>
          mutate(label=strsplit(.data$value," +") |> lapply(\(x)paste0(x[2:length(x)],collapse = " ")) |> unlist()) |>
          mutate(label=gsub("^'|'$","",.data$label)) |>
          mutate(label=gsub('^"|"$',"",.data$label)) |>
          mutate(name=gsub("_$","",.data$n))
        vr
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



get_census_pumf <- function(pumf_version,pumf_cache_path,refresh_layout=FALSE){
  cached_pumf <- dir(pumf_cache_path)
  ylv <- get_year_level_version(pumf_version)
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

      ensure_2021_pumfi_metadata(pumf_base_path,refresh_layout=refresh_layout)

    } else {
      stop("2021 PUMF data is not avaialble")
    }
  } else if (pumf_version=="2016 (individuals)"|pumf_version=="2016 (hierarchical)"|pumf_version=="2016"){
    if (pumf_version=="2016 (hierarchical)") {
      path <- cached_pumf[grepl("98M0001X",cached_pumf)&grepl("2016",cached_pumf)&!grepl("\\.zip$",cached_pumf)]
    } else {
      path <- cached_pumf[grepl("98M0001X",cached_pumf)&grepl("2016",cached_pumf)&!grepl("\\.zip$",cached_pumf)]
    }
    if (length(path)==1) {
      pumf_base_path <- file.path(pumf_cache_path,path)
      pumf_data_file <- dir(pumf_base_path,"\\.dat",full.names = TRUE)

      ensure_2016_pumf_metadata(pumf_base_path,refresh_layout=refresh_layout)

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
  } else if (pumf_version=="2011 (individuals)"|pumf_version=="2011 (hierarchical)"|pumf_version=="2011"){
    if (pumf_version=="2011 (hierarchical)") {
      path <- cached_pumf[grepl("99M0002X",cached_pumf)&grepl("2011",cached_pumf)&!grepl("\\.zip$",cached_pumf)]
    } else {
      path <- cached_pumf[grepl("99M0001X",cached_pumf)&grepl("2011",cached_pumf)&!grepl("\\.zip$",cached_pumf)]
    }
    if (length(path)==1) {
      pumf_base_path <- file.path(pumf_cache_path,path)
      individuals_path <- dir(pumf_base_path,"Individual",full.names = TRUE)
      pumf_data_file <- dir(individuals_path,"\\.dat",full.names = TRUE)
      if (length(pumf_data_file)==0) {
        pumf_data_file <- dir(pumf_base_path,"\\.dat",full.names = TRUE)
      }

      ensure_2011_pumf_metadata(pumf_base_path,refresh_layout=refresh_layout)

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

  } else if (pumf_version=="2006 (individuals)"|pumf_version=="2006 (hierarchical)"|pumf_version=="2006"){
    if (pumf_version=="2006 (hierarchical)") {
      path <- cached_pumf[grepl("95M0029X",cached_pumf)&grepl("2006",cached_pumf)&!grepl("\\.zip$",cached_pumf)]
    } else {
      path <- cached_pumf[grepl("95M0028X",cached_pumf)&grepl("2006",cached_pumf)&!grepl("\\.zip$",cached_pumf)]
    }
    if (length(path)==1) {
      pumf_base_path <- file.path(pumf_cache_path,path)
      individuals_path <- pumf_base_path
      pumf_data_file <- dir(individuals_path,"\\.dat",full.names = TRUE)

      ensure_2006_pumf_metadata(pumf_base_path,refresh_layout=refresh_layout)

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
      stop("2006 PUMF data is not avaialble")
    }
  } else if (pumf_version=="2001 (individuals)"|pumf_version=="2001 (households)"|pumf_version=="2001 (families)"|pumf_version=="2001"){
    if (pumf_version=="2001 (households)") {
      path <- cached_pumf[grepl("95M0020X",cached_pumf)&grepl("2001",cached_pumf)&!grepl("\\.zip$",cached_pumf)]
    } else if (pumf_version=="2001 (families)") {
      path <- cached_pumf[grepl("95M0018X",cached_pumf)&grepl("2001",cached_pumf)&!grepl("\\.zip$",cached_pumf)]
    } else {
      path <- cached_pumf[grepl("95M0016X",cached_pumf)&grepl("2001",cached_pumf)&!grepl("\\.zip$",cached_pumf)]
    }
    if (length(path)==1) {
      pumf_base_path <- file.path(pumf_cache_path,path)
      individuals_path <- pumf_base_path
      pumf_data_file <- dir(individuals_path,"\\.dat",full.names = TRUE)

      ensure_2001_pumf_metadata(pumf_base_path,refresh_layout=refresh_layout)

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
      stop("2001 PUMF data is not avaialble")
    }
  } else if (pumf_version=="1996 (individuals)"|pumf_version=="1996 (families)"|pumf_version=="1996 (households)"|pumf_version=="1996"){
    if (pumf_version=="1996 (households)") {
      path <- cached_pumf[grepl("95M0011X",cached_pumf)&grepl("1996",cached_pumf)&!grepl("\\.zip$",cached_pumf)]
    } else if (pumf_version=="1996 (families)") {
      path <- cached_pumf[grepl("95M0012X",cached_pumf)&grepl("1996",cached_pumf)&!grepl("\\.zip$",cached_pumf)]
    } else {
      path <- cached_pumf[grepl("95M0010X",cached_pumf)&grepl("1996",cached_pumf)&!grepl("\\.zip$",cached_pumf)]
    }
    if (length(path)==1) {
      pumf_base_path <- file.path(pumf_cache_path,path)
      individuals_path <- pumf_base_path
      pumf_data_file <- dir(individuals_path,"\\.dat",full.names = TRUE)
      pumf_data_file <- pumf_data_file[!grepl("testind",pumf_data_file)]
      if (length(pumf_data_file)!=1) {
        zip_file <- dir(individuals_path,"\\.zip",full.names = TRUE)
        if (length(zip_file)==1) {
            utils::unzip(zip_file,exdir = individuals_path)
        }
      }

      pumf_data_file <- dir(individuals_path,"\\.dat",full.names = TRUE)
      pumf_data_file <- pumf_data_file[!grepl("testind",pumf_data_file)]

      if (length(pumf_data_file)!=1) {
        stop("1996 PUMF data is not avaialble")
      }

      ensure_1996_pumf_metadata(pumf_base_path,refresh_layout=refresh_layout)

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
  } else if (ylv$year %in% seq(1971,1991,5)){
    path <- cached_pumf[grepl(ylv$year,cached_pumf)]
    path <- path[grepl("_FMGD",path)]
    if (length(path)==1) {
      pumf_base_path <- file.path(pumf_cache_path,path)

      pumf_version_path <- file.path(pumf_base_path,paste0(ylv,collapse = "_"))
      pumf_data_file <- dir(pumf_version_path,"\\.txt",full.names = TRUE)

      if (length(pumf_data_file)==0) {
        candidates <- dir(pumf_base_path,"\\.zip")
        if (ylv$version=="households") {
          candidates <- candidates[grepl("^hhld",candidates)]
        } else if (ylv$version=="families") {
          candidates <- candidates[grepl("^fam",candidates)]
        } else {
          candidates <- candidates[grepl("^indiv",candidates)]
        }
        if (ylv$year=="1971") {
          if (ylv$version=="PR") {
            candidates <- candidates[grepl("_prov",candidates)]
          } else {
            candidates <- candidates[grepl("_cma",candidates)]
          }
        }
        if (length(candidates)>1 && sum(grepl("dat",candidates))==1) {
          candidates <- candidates[grepl("dat",candidates)]
        }
        if (length(candidates)!=1) {
          stop("Could not locate data file")
        }
        utils::unzip(file.path(pumf_base_path,candidates),exdir = pumf_version_path)
        pumf_data_file <- dir(pumf_version_path,"\\.txt|\\.DAT",full.names = TRUE)
      }



      ensure_1971_1991_pumf_metadata(pumf_base_path,pumf_version,refresh_layout=refresh_layout)

      canpumf_dir <- file.path(pumf_version_path,"canpumf")
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

      attr(pumf_data,"pumf_base_path") <- pumf_version_path

    } else {
      stop(ylv$year," PUMF data is not avaialble")
    }
  } else {
    stop("PUMF version not supported")
  }
  pumf_data
}

