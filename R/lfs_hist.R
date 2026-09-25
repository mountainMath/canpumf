# R/lfs_hist.R — Historical monthly Labour Force Survey PUMF, 1976-2005.
#
# LFS_HIST is the second longitudinal series (see R/longitudinal.R).  Its data
# and SAS setup files come from Borealis, where ODESI deposited one dataset per
# month and language (catalogue 71M0001XCB).  The months use the legacy
# (pre-2017) PUMF layout, so they live in their own DuckDB
# (<cache_path>/LFS_HIST/LFS_HIST.duckdb) and are never mixed into LFS.duckdb.
#
# Labels come from a canonical bilingual dictionary shipped in
# inst/extdata/lfs_hist/ (built by tools/build_lfs_hist_reference.R), not from
# each month's own SAS program: ODESI reworded the same codes in every rebasing
# era, which would otherwise give one ENUM level per spelling.


# ---- Reference data ---------------------------------------------------------

.lfs_hist_first <- "1976-01"
.lfs_hist_last  <- "2005-12"

.lfs_hist_ref_cache <- new.env(parent = emptyenv())

# Read one of the shipped reference CSVs (datasets / variables / codes).
.lfs_hist_ref <- function(which) {
  if (is.null(.lfs_hist_ref_cache[[which]])) {
    path <- system.file("extdata", "lfs_hist", paste0(which, ".csv"),
                        package = "canpumf")
    if (!nzchar(path))
      stop("LFS_HIST reference file '", which, ".csv' is missing from the ",
           "installed package.", call. = FALSE)
    .lfs_hist_ref_cache[[which]] <- readr::read_csv(
      path, col_types = readr::cols(.default = "c"), na = "",
      locale = readr::locale(encoding = "UTF-8"), progress = FALSE)
  }
  .lfs_hist_ref_cache[[which]]
}

# Canonical variables table in the metadata schema.
.lfs_hist_variables <- function() {
  v <- .lfs_hist_ref("variables")
  v$decimals     <- as.integer(v$decimals)
  v$missing_low  <- as.numeric(v$missing_low)
  v$missing_high <- as.numeric(v$missing_high)
  v
}

# Census the weights of a month are rebased to, by ODESI deposit era:
# 1976-1986 are the original files, 1987-1995 were rebased to the 2001 Census,
# 1996-2000 to 2006, and 2001-2005 to 2011.  Weight levels can step at these
# boundaries.
.lfs_hist_rebased <- function(version) {
  y <- as.integer(substr(version, 1L, 4L))
  ifelse(y <= 1986L, "", ifelse(y <= 1995L, "2001", ifelse(y <= 2000L, "2006", "2011")))
}

# All YYYY-MM versions LFS_HIST covers.
.lfs_hist_all_versions <- function()
  sprintf("%d-%02d", rep(1976:2005, each = 12L), 1:12)


# ---- Catalogue matching (used by tools/build_lfs_hist_reference.R) ----------

# Map the monthly LFS datasets in a Borealis catalogue to YYYY-MM versions.
# Returns version, lang, doi, title, rebased (the "[Rebased ...]" tag).
.lfs_hist_index_from_catalogue <- function(cat_b) {
  en <- "^Labour Force Survey,.*\\[Canada\\]"
  fr <- "^Enqu\u00eate sur la population active,.*\\[Canada\\]"
  d  <- cat_b[grepl(en, cat_b$title) | grepl(fr, cat_b$title), ]
  mo_en <- tolower(month.name)
  mo_fr <- c("janvier", "f\u00e9vrier", "mars", "avril", "mai", "juin",
             "juillet", "ao\u00fbt", "septembre", "octobre", "novembre",
             "d\u00e9cembre")
  t <- tolower(d$title)
  month <- rep(NA_integer_, nrow(d))
  for (i in 1:12)
    month[is.na(month) & grepl(paste0("\\b(", mo_en[i], "|", mo_fr[i], ")\\b"),
                               t, perl = TRUE)] <- i
  year <- as.integer(stringr::str_match(d$title, ",\\s*\\S+\\s+([0-9]{4})")[, 2L])
  out <- tibble::tibble(
    version = sprintf("%d-%02d", year, month),
    lang    = ifelse(grepl(fr, d$title), "fra", "eng"),
    doi     = d$doi,
    title   = d$title,
    rebased = stringr::str_match(d$title, "(?i)(?:Rebased|Remani\u00e9)[^0-9]*([0-9]{4})?")[, 1L])
  out$rebased <- ifelse(is.na(out$rebased), "",
    ifelse(grepl("[0-9]{4}", out$rebased),
           sub(".*([0-9]{4}).*", "\\1", out$rebased), "yes"))
  out <- out[!is.na(year) & !is.na(month) & year >= 1976L & year <= 2005L, ]
  dup <- duplicated(out[c("version", "lang")])
  if (any(dup))
    warning("Several Borealis datasets for LFS_HIST ",
            paste(unique(out$version[dup]), collapse = ", "),
            "; keeping the first.", call. = FALSE)
  out[!dup, ]
}


# ---- Harmonisation ----------------------------------------------------------

# ODESI's 2001-2005 (rebased to the 2011 Census) files spell the NOC-S 2001
# occupation variables NOCS_01_25/NOCS_01_47/SP_NOCS01; every other era uses
# NOC01_25/NOC01_47/SP_NOC01.
.lfs_hist_renames <- c(NOCS_01_25 = "NOC01_25", NOCS_01_47 = "NOC01_47",
                       SP_NOCS01  = "SP_NOC01")

.lfs_hist_canonical_names <- function(x) {
  x   <- toupper(x)
  hit <- x %in% names(.lfs_hist_renames)
  x[hit] <- unname(.lfs_hist_renames[x[hit]])
  x
}

# Curated label corrections applied on top of the latest-month labels when the
# reference dictionary is built.  One row per (name, val).
#   EFAMTYPE 15: the 2001-2005 files repeat code 13's English label; every
#     earlier file (and the French) has the single-parent category.
#   ED76TO89/EDUC90 4: "certificate of diploma" typo of the 2003-2005 files.
.lfs_hist_label_fixes <- function() {
  tibble::tribble(
    ~name,      ~val, ~label_en,                                                          ~label_fr,
    "EFAMTYPE", "15", "Single-parent family, parent employed, youngest child 18 to 24",   "Familles monoparentales, parent occup\u00e9, plus jeune enfant 18 \u00e0 24",
    "ED76TO89", "4",  "Post-secondary certificate or diploma",                            "Certificat ou dipl\u00f4me d'\u00e9tudes postsecondaires",
    "EDUC90",   "4",  "Post-secondary certificate or diploma",                            "Certificat ou dipl\u00f4me d'\u00e9tudes postsecondaires")
}

# Codes whose meaning differs by period, which one label per code cannot
# express.  Rows replace the canonical codes of `name` for months from..to.
#   MARSTAT: until 1999-10 the data carry four categories, although the SAS
#     programs from 1989-09 on (and the French ones throughout) label them
#     with the six-category scheme introduced in 1999-11.  The new question
#     was phased in by rotation group, so in 1999-11..2000-03 codes 1 and 4
#     still include common-law and divorced respondents of the older groups.
.lfs_hist_code_eras <- function() {
  tibble::tribble(
    ~name,     ~val, ~label_en,                 ~label_fr,                                            ~from,     ~to,
    "MARSTAT", "1",  "Married or common-law",   "Mari\u00e9s ou en union libre",                      "1976-01", "1999-10",
    "MARSTAT", "2",  "Single, never married",   "C\u00e9libataires, n'ont jamais \u00e9t\u00e9 mari\u00e9s", "1976-01", "1999-10",
    "MARSTAT", "3",  "Widowed",                 "Veufs ou veuves",                                    "1976-01", "1999-10",
    "MARSTAT", "4",  "Separated or divorced",   "S\u00e9par\u00e9s ou divorc\u00e9s",                "1976-01", "1999-10")
}

# The canonical codes with the era rows for `version` swapped in.
.lfs_hist_codes_for <- function(version, codes = .lfs_hist_ref("codes")) {
  eras <- .lfs_hist_code_eras()
  eras <- eras[version >= eras$from & version <= eras$to, ]
  if (nrow(eras) == 0L) return(codes)
  codes <- codes[!codes$name %in% eras$name, , drop = FALSE]
  eras  <- eras[, c("name", "val", "label_en", "label_fr")]
  out   <- rbind(codes, tibble::as_tibble(eras)[names(codes)])
  out[order(match(out$name, unique(c(codes$name, eras$name)))), , drop = FALSE]
}


# ---- Versions ---------------------------------------------------------------

# "annual" or "monthly"; stops for malformed or out-of-range versions.
.lfs_hist_validate <- function(v) {
  if (!is.character(v) || length(v) != 1L || !grepl("^[0-9]{4}(-[0-9]{2})?$", v))
    stop("Invalid LFS_HIST version '", v, "'. Expected YYYY (a year of ",
         "monthly files) or YYYY-MM (one month).", call. = FALSE)
  y <- as.integer(substr(v, 1L, 4L))
  if (y >= 2006L)
    stop("LFS_HIST covers ", .lfs_hist_first, " to ", .lfs_hist_last, ". ",
         "Use get_pumf(\"LFS\", \"", v, "\") for 2006 onward.", call. = FALSE)
  if (y < 1976L || (nchar(v) == 7L && !v %in% .lfs_hist_all_versions()))
    stop("LFS_HIST covers ", .lfs_hist_first, " to ", .lfs_hist_last,
         "; '", v, "' is outside that range.", call. = FALSE)
  .lfs_version_type(v)
}

# Borealis DOIs of one month: list(eng, fra); fra is NULL when ODESI
# deposited no French dataset (1990-02, 1996-02).
.lfs_hist_dois <- function(version) {
  d <- .lfs_hist_ref("datasets")
  i <- match(version, d$version)
  if (is.na(i)) stop("No LFS_HIST dataset for ", version, ".", call. = FALSE)
  fra <- d$doi_fra[[i]]
  list(eng = d$doi_eng[[i]], fra = if (is.na(fra) || !nzchar(fra)) NULL else fra)
}


# ---- Stages 1 + 2 -----------------------------------------------------------

# The CSV data file of a month's directory, or NULL.
.lfs_hist_data_file <- function(version_dir) {
  f <- list.files(version_dir, pattern = "\\.csv$", ignore.case = TRUE,
                  full.names = TRUE)
  if (length(f) == 0L) NULL else f[which.max(file.size(f))]
}

# SAS setup program of a month, in `version_dir` (English) or its fra/ subdir.
.lfs_hist_sas_file <- function(version_dir, lang = "eng") {
  d <- if (lang == "eng") version_dir else file.path(version_dir, "fra")
  f <- list.files(d, pattern = "\\.sas$", ignore.case = TRUE, full.names = TRUE)
  if (length(f) == 0L) NULL else f[[1L]]
}

# Download one month from Borealis: from the English dataset the CSV data,
# the SAS setup program and the documentation; from the French dataset only
# its SAS program (for the French labels of the month check).
.lfs_hist_download <- function(version, version_dir) {
  dois <- .lfs_hist_dois(version)
  eng  <- .borealis_select_files(.borealis_dataset_files(dois$eng))
  ext  <- tolower(tools::file_ext(eng$filename))
  eng$role[ext == "sas"] <- "metadata"
  sel  <- eng[(eng$selected & eng$role %in% c("data", "doc")) | ext == "sas", ]
  sel$dest <- sel$filename
  if (!is.null(dois$fra)) {
    fra <- .borealis_dataset_files(dois$fra)
    fra <- fra[tolower(tools::file_ext(fra$filename)) == "sas", ]
    if (nrow(fra) == 1L) {
      fra$role <- "metadata"
      fra$dest <- file.path("fra", fra$filename)
      sel <- rbind(sel[names(fra)], fra)
    }
  }
  if (!any(sel$role == "data"))
    stop("No CSV data file in the Borealis dataset for LFS_HIST ", version,
         " (", dois$eng, ").", call. = FALSE)

  dir.create(file.path(version_dir, "fra"), recursive = TRUE, showWarnings = FALSE)
  .pumf_warn_cache_path_on_download()
  message("Downloading LFS_HIST ", version, " from Borealis (",
          format(sum(sel$size, na.rm = TRUE) / 1e6, digits = 3), " MB) ...")
  old_timeout <- getOption("timeout")
  options(timeout = max(1800L, old_timeout))
  on.exit(options(timeout = old_timeout), add = TRUE)
  for (i in seq_len(nrow(sel)))
    .borealis_download_file(sel$file_id[[i]], file.path(version_dir, sel$dest[[i]]))

  readr::write_csv(tibble::tibble(
    doi       = ifelse(startsWith(sel$dest, "fra/"), dois$fra %||% NA, dois$eng),
    title     = NA_character_,
    file_id   = sel$file_id,
    filename  = sel$dest,
    role      = sel$role,
    md5       = sel$md5,
    data_file = ifelse(sel$role == "data", sel$dest, NA_character_),
    fetched   = format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    file.path(version_dir, .borealis_manifest_file), na = "")
  invisible(version_dir)
}

# Stage 2: the month's metadata is the canonical dictionary restricted to the
# month's columns.  The month's own SAS program is only checked against it:
# a variable the dictionary does not know is reported (its values stay raw).
.lfs_hist_write_metadata <- function(version_dir) {
  data_file <- .lfs_hist_data_file(version_dir)
  cols <- .lfs_hist_canonical_names(names(readr::read_csv(
    data_file, n_max = 0L, col_types = readr::cols(.default = "c"),
    progress = FALSE)))
  vars  <- .lfs_hist_variables()
  codes <- .lfs_hist_codes_for(basename(version_dir))
  unknown <- setdiff(cols, vars$name)
  if (length(unknown) > 0L)
    warning("LFS_HIST ", basename(version_dir), ": variable(s) not in the ",
            "canonical dictionary, kept unlabelled: ",
            paste(unknown, collapse = ", "), call. = FALSE)
  sas <- .lfs_hist_sas_file(version_dir)
  if (!is.null(sas)) {
    own <- tryCatch(parse_sas_odesi(sas), error = function(e) NULL)
    if (!is.null(own)) {
      own$codes$name <- .lfs_hist_canonical_names(own$codes$name)
      # Compared with the era-free dictionary: most 4-category-era MARSTAT
      # programs still list the six later labels (docs/longitudinal.md).
      ref  <- .lfs_hist_ref("codes")
      miss <- setdiff(paste(own$codes$name, own$codes$val),
                      paste(ref$name, ref$val))
      if (length(miss) > 0L)
        warning("LFS_HIST ", basename(version_dir), ": ", length(miss),
                " code(s) of the month's SAS program are not in the canonical ",
                "dictionary: ", paste(utils::head(miss, 5L), collapse = ", "),
                if (length(miss) > 5L) " ...", call. = FALSE)
    }
  }
  meta <- list(
    variables = vars[match(intersect(cols, vars$name), vars$name), , drop = FALSE],
    codes     = codes[codes$name %in% cols, , drop = FALSE],
    layout    = NULL)
  write_metadata(meta, file.path(version_dir, "metadata"))
  invisible(version_dir)
}

.lfs_hist_prepare <- function(version, cache_path, refresh = FALSE,
                              redownload = FALSE) {
  if (.lfs_hist_validate(version) != "monthly")
    stop("LFS_HIST metadata is per month; pass a YYYY-MM version.",
         call. = FALSE)
  version_dir <- file.path(cache_path, "LFS_HIST", version)
  if (isTRUE(redownload) && dir.exists(version_dir))
    unlink(version_dir, recursive = TRUE)
  if (is.null(.lfs_hist_data_file(version_dir)))
    .lfs_hist_download(version, version_dir)
  if (is.null(.lfs_hist_data_file(version_dir)))
    stop("No CSV data file for LFS_HIST ", version, " in ", version_dir, ".",
         call. = FALSE)
  if (refresh || !metadata_exists(version_dir))
    .lfs_hist_write_metadata(version_dir)
  version_dir
}


# ---- Stage 3 ----------------------------------------------------------------

.lfs_hist_build <- function(version_dir, label_col, version = NULL) {
  meta      <- read_metadata(file.path(version_dir, "metadata"))
  variables <- meta$variables
  int_cols  <- .pumf_lfs_hist_entry$data_fixups$force_integer
  variables$type[variables$name %in% int_cols] <- "numeric"
  codes     <- meta$codes[!meta$codes$name %in% int_cols, ]

  data_file <- .lfs_hist_data_file(version_dir)
  message("  data file: ", basename(data_file))
  data <- readr::read_csv(data_file, col_types = readr::cols(.default = "c"),
                          locale = readr::locale(encoding = "CP1252"),
                          na = "", progress = FALSE)
  names(data) <- .lfs_hist_canonical_names(names(data))
  missing_cols <- setdiff(c("SURVYEAR", "SURVMNTH"), names(data))
  if (length(missing_cols) > 0L)
    stop("LFS_HIST data is missing required columns: ",
         paste(missing_cols, collapse = ", "), " (", data_file, ").",
         call. = FALSE)
  data <- .apply_numeric_conversion(data, variables,
                                    missing_codes = .label_missing_codes(meta$codes))
  for (col in intersect(int_cols, names(data)))
    data[[col]] <- as.integer(data[[col]])
  .apply_code_labels(data, codes, label_col)
}


# ---- Longitudinal spec ------------------------------------------------------

.lfs_hist_spec <- function() {
  list(
    series         = "LFS_HIST",
    db_file        = "LFS_HIST.duckdb",
    table_prefix   = "lfs_hist",
    versions_table = "lfs_hist_versions",
    annual_files   = FALSE,
    example        = "1995",
    validate       = .lfs_hist_validate,
    available      = .lfs_hist_all_versions,
    prepare        = .lfs_hist_prepare,
    build          = .lfs_hist_build,
    variables      = function(cache_path, versions) .lfs_hist_variables())
}
