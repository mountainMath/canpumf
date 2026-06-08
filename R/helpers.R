
robust_pumf_base_dir <- function(pumf_base_path){
  d<-dir(pumf_base_path)
  d<-d[d!="canpumf"]
  while (length(d)==1) {
    pumf_base_path <- file.path(pumf_base_path,d)
    d<-dir(pumf_base_path)
  }
  pumf_base_path
}

pumf_clean_layout_dir <- function(pumf_base_path,var_path=NULL){
  p <- file.path(pumf_base_path,"canpumf")
  if (!dir.exists(p)) dir.create(p)
  if (!is.null(var_path)) {
    p <- file.path(p,var_path)
    if (!dir.exists(p)) dir.create(p)
  }
  p
}


pumf_data_dir <- function(pumf_base_path){
  pumf_base_path <- robust_pumf_base_dir(pumf_base_path)
  data_dir <- dir(pumf_base_path,"*Data*")
  if (length(data_dir)==0) {
    dd<-dir(pumf_base_path,".txt")
    if (length(dd)>0) return(pumf_base_path)
  }
  if (length(data_dir)==0) stop("Could not find data directory in PUMF base path, aborting.")
  if (length(data_dir)>1) {
    data_dir <- data_dir[!grepl("dictionary",data_dir)]
    if (length(data_dir)>1) stop("Found multiple data directories in PUMF base path, aborting.")
  }
  file.path(pumf_base_path,data_dir)
}

pumf_layout_dir <- function(pumf_base_path){
  pumf_base_path <- robust_pumf_base_dir(pumf_base_path)
  layout_dir<-pumf_base_path
  spss_path <- dir(layout_dir,"SPSS",full.names = TRUE)
  if (length(spss_path)==0){
    layout_dir <- dir(pumf_base_path,"Layout|Syntax|Command|SpssCard|Reading cards",full.names = TRUE)
    if (length(layout_dir)==0||!dir.exists(file.path(layout_dir))){
      data_path <- dir(pumf_base_path,"*Data*",full.names = TRUE)
      if (length(data_path)==0) stop("Could not find layout or data in PUMF base path, aborting.")
      layout_dir <- dir(data_path,"Layout|Syntax|Reading cards",full.names = TRUE)
      if (length(layout_dir)==0) stop("Could not find layout in PUMF base path, aborting.")
    } else {
    }
    spss_path <- dir(layout_dir,"SPSS",full.names = TRUE)
  }
  if (length(spss_path)==0) {
    if (basename(layout_dir)=="SpssCard") spss_path=layout_dir
    else stop("Could not find layout in PUMF base path, aborting.")
  }
  if (!grepl("\\.sps$",spss_path)) layout_dir<-spss_path
  layout_dir
}


slice_raw_layout_data <- function(r){
  start_line <- which(grepl(" LABELS| VALUES|DATA LIST FILE",r$value))+1
  end_line <- setdiff(which(grepl(" +\\.",r$value)),which(grepl(" +\\.\\.\\.",r$value)))-1
  r %>%
    slice(seq(start_line,end_line))
}

tmp_layout_path <- function(pumf_base_path,...) {
  file.path(tempdir(),paste0("canpumf_",digest::digest(pumf_base_path),"_",paste0(...,collapse="_"),".Rds"))
}


find_unique_layout_file <- function(layout_path,pattern,path_or_pattern=NULL){
  validate_path <- function(path){
    if (length(path)==0) stop("Could not find layout file.")
    if (length(path)>1) stop(paste0("Found several layout files: ",paste0(path,collapse=", "),".\n",
                                    "Please further specify which layout file to use."))
    NULL
  }



  path <- path_or_pattern
  if (is.null(path)) {
    path <- dir(layout_path,pattern="\\.sps$|\\.lay$")
    if (length(path)>1) {
      path <- dir(layout_path,pattern=pattern)
    }
    validate_path(path)
    path<-file.path(layout_path,path)
  } else {
    if (file.exists(file.path(layout_path,path))) path <- file.path(layout_path,path)
    if (!file.exists(path)) {
      paths <- dir(layout_path,pattern="\\.sps$|\\.lay$")
      paths <- paths[grepl(path,paths)]
      if (length(paths)>1) paths <- paths[grepl(pattern,paths)]
      if (length(paths)>1) {
        pp <- paste0(path_or_pattern,"_",pattern)
        if (substr(pattern,1,1)=="_") pp <- paste0(path_or_pattern,pattern)
        paths <- paths[grepl(pp,paths)]
      }
      validate_path(paths)
      if (grepl("\\.sps$",paths)) path<-file.path(layout_path,paths) else path <- layout_path
    }
  }
  path
}


# Low-level extractor: ditto on macOS with unzip fallbacks.
.unzip_impl <- function(path, exdir) {
  if (Sys.info()[['sysname']] == "Darwin") {
    # ditto does not support all ZIP compression variants (e.g. newer deflate
    # flavours used by StatCan since 2025).  Fall back to system unzip, then
    # to utils::unzip, if ditto exits non-zero.
    exit <- system(paste0("ditto -x -k --sequesterRsrc --rsrc '",
                           path, "' '", exdir, "'"))
    if (exit != 0L) {
      message("ditto failed (exit ", exit, "); falling back to unzip.")
      exit2 <- system(paste0("unzip -o '", path, "' -d '", exdir, "'"),
                      ignore.stdout = TRUE)
      if (exit2 != 0L) utils::unzip(path, exdir = exdir)
    }
  } else {
    utils::unzip(path, exdir = exdir)
  }
}

robust_unzip <- function(path, exdir) {
  zip_name <- basename(path)

  # Detect naming collision: some ZIPs have a single top-level directory with
  # the same name as the archive (e.g. 2025-CSV.zip contains 2025-CSV.zip/*).
  # When the archive lives inside exdir, extracting would require creating a
  # directory at the same path as the zip file — which fails.
  #
  # Fix: extract to a temp sibling directory (same filesystem → atomic rename),
  # strip .zip from the colliding directory name, then move into exdir.
  top_entries   <- tryCatch(utils::unzip(path, list = TRUE)$Name,
                             error = function(e) character(0L))
  # Some StatCan zips store filenames in CP1252 without the UTF-8 flag,
  # so top_entries may contain bytes invalid in the UTF-8 locale.
  # useBytes=TRUE matches the ASCII "/" without attempting encoding
  # translation, silencing spurious "input string is invalid" warnings.
  top_dirs      <- unique(sub("/.*", "/", grep("/", top_entries,
                                               value    = TRUE,
                                               fixed    = TRUE,
                                               useBytes = TRUE),
                              useBytes = TRUE))
  has_collision <- paste0(zip_name, "/") %in% top_dirs

  if (has_collision) {
    tmp_dir <- paste0(exdir, "_unzip_tmp")
    dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    .unzip_impl(path, tmp_dir)

    # Rename the colliding directory: strip the .zip extension so it no longer
    # shadows the archive file (2025-CSV.zip/ → 2025-CSV/).
    safe_name <- sub("\\.zip$", "", zip_name, ignore.case = TRUE)
    from_col  <- file.path(tmp_dir, zip_name)
    if (file.exists(from_col))
      file.rename(from_col, file.path(tmp_dir, safe_name))

    # Move everything from the temp dir into exdir.
    for (item in list.files(tmp_dir, all.files = FALSE)) {
      dest <- file.path(exdir, item)
      if (!file.exists(dest))
        file.rename(file.path(tmp_dir, item), dest)
    }
  } else {
    .unzip_impl(path, exdir)
  }
  invisible(NULL)
}

get_pumf_from_url <- function(url,pumf_cache_path,key=NULL) {
    tmp <- tempfile()
    if (is.null(key)) {
      key <- basename(url) |> gsub("\\.zip$","",x=_)
    }
    utils::download.file(url,tmp)
    robust_unzip(tmp,exdir=file.path(pumf_cache_path,key))
    unlink(tmp)
    key
}


#' @import dplyr
#' @importFrom stats setNames
#' @import stringr
#' @import readr
#' @importFrom rlang .data
#' @importFrom rlang :=
#' @import duckplyr
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "4.1")  utils::globalVariables(c("."))
