# Build the LFS_HIST reference data shipped with the package.
#
# LFS_HIST is the monthly Labour Force Survey PUMF for January 1976 - December
# 2005 in the legacy (pre-2017) layout, as deposited on Borealis (ODESI series
# 71M0001XCB, one dataset per month and language).  This script writes
#
#   inst/extdata/lfs_hist/datasets.csv  version -> English/French Borealis DOIs
#   inst/extdata/lfs_hist/variables.csv canonical bilingual variable labels
#   inst/extdata/lfs_hist/codes.csv     canonical bilingual value labels
#
# The canonical dictionary exists because ODESI relabelled the same codes in
# every rebasing era ("Unemployed, temporary layoff" / "Unemploy,temp layoff"),
# which would otherwise give one ENUM level per spelling in the shared DuckDB.
# It is built from the SAS setup file of every month in both languages, taking
# each (variable, code) label from the most recent month that carries it, then
# applying the curated corrections in `fixes` below.  Every (variable, code)
# whose label *meaning* differs between months is written to
# label_conflicts.csv in the work directory for manual review.
#
#   Rscript tools/build_lfs_hist_reference.R [work_dir]
#
# work_dir (default: <canpumf.cache_path>/LFS_HIST/_reference) caches the
# downloaded SAS programs (~40 MB), so reruns only fetch what is missing.
# This file is .Rbuildignore'd (tools/).

suppressMessages(devtools::load_all(quiet = TRUE))

args     <- commandArgs(trailingOnly = TRUE)
work_dir <- if (length(args)) args[[1L]] else
  file.path(getOption("canpumf.cache_path", tempdir()), "LFS_HIST", "_reference")
dir.create(file.path(work_dir, "sas"), recursive = TRUE, showWarnings = FALSE)
out_dir <- file.path("inst", "extdata", "lfs_hist")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- 1. Dataset index -------------------------------------------------------

cat_b <- list_borealis_pumf_catalogue()
idx   <- canpumf:::.lfs_hist_index_from_catalogue(cat_b)
# The rebasing era follows from the year; titles only confirm it (the English
# 1987-2000 titles say "[Rebased]" without a Census year).
bad <- idx$rebased != "" & idx$rebased != "yes" &
  idx$rebased != canpumf:::.lfs_hist_rebased(idx$version)
if (any(bad))
  message("Titles disagreeing with the rebasing era of their year:\n  ",
          paste(idx$title[bad], collapse = "\n  "))
wide  <- merge(idx[idx$lang == "eng", c("version", "doi")],
               idx[idx$lang == "fra", c("version", "doi")],
               by = "version", all.x = TRUE, suffixes = c("_eng", "_fra"))
wide$rebased <- canpumf:::.lfs_hist_rebased(wide$version)
wide  <- wide[order(wide$version), c("version", "doi_eng", "doi_fra", "rebased")]
all_v <- sprintf("%d-%02d", rep(1976:2005, each = 12L), 1:12)
stopifnot(identical(wide$version, all_v))
utils::write.csv(wide, file.path(out_dir, "datasets.csv"), row.names = FALSE,
                 na = "")
message("datasets.csv: ", nrow(wide), " months; no French dataset for ",
        paste(wide$version[is.na(wide$doi_fra) | wide$doi_fra == ""],
              collapse = ", "))

# ---- 2. SAS programs --------------------------------------------------------

sas_path <- function(version, lang) file.path(work_dir, "sas",
                                              paste0(version, "_", lang, ".sas"))
todo <- rbind(data.frame(version = wide$version, lang = "eng", doi = wide$doi_eng),
              data.frame(version = wide$version, lang = "fra", doi = wide$doi_fra))
todo <- todo[!is.na(todo$doi) & todo$doi != "" &
               !file.exists(sas_path(todo$version, todo$lang)), ]
message("Downloading ", nrow(todo), " SAS programs ...")
for (i in seq_len(nrow(todo))) {
  f <- tryCatch(canpumf:::.borealis_dataset_files(todo$doi[i]),
                error = function(e) NULL)
  s <- f[grepl("\\.sas$", f$filename, ignore.case = TRUE), ]
  if (is.null(f) || nrow(s) != 1L) {
    warning("No unique SAS program for ", todo$version[i], " ", todo$lang[i])
    next
  }
  dest <- sas_path(todo$version[i], todo$lang[i])
  part <- paste0(dest, ".part")   # an interrupted download never looks complete
  canpumf:::.borealis_download_file(s$file_id, part)
  file.rename(part, dest)
  if (i %% 50L == 0L) message("  ", i, " / ", nrow(todo))
}

# ---- 3. Parse every month ---------------------------------------------------

parsed <- lapply(wide$version, function(v) {
  fr <- sas_path(v, "fra")
  m  <- parse_sas_odesi(sas_path(v, "eng"), if (file.exists(fr)) fr else NULL)
  m$variables$version <- v
  m$codes$version     <- v
  m
})
vars_long  <- do.call(rbind, lapply(parsed, `[[`, "variables"))
codes_long <- do.call(rbind, lapply(parsed, `[[`, "codes"))
# ODESI renamed the occupation variables in the 2001-2005 rebased files
vars_long$name  <- canpumf:::.lfs_hist_canonical_names(vars_long$name)
codes_long$name <- canpumf:::.lfs_hist_canonical_names(codes_long$name)
saveRDS(list(variables = vars_long, codes = codes_long),
        file.path(work_dir, "parsed_long.rds"))

# ---- 4. Canonical dictionary ------------------------------------------------

latest_first <- function(d) d[order(d$version, decreasing = TRUE), ]
pick <- function(d, key) {
  d <- latest_first(d)
  # French label: latest non-missing (two months have no French dataset).
  # Some deposits replaced accented characters (and their neighbours) with a
  # literal "?" ("Ann?d'enqu?"), so an intact label from an older month wins.
  fr <- d[!is.na(d$label_fr), ]
  fr <- fr[order(grepl("?", fr$label_fr, fixed = TRUE)), ]
  out <- d[!duplicated(d[key]), ]
  out$label_fr <- fr$label_fr[match(do.call(paste, out[key]), do.call(paste, fr[key]))]
  out
}

codes <- pick(codes_long, c("name", "val"))
vars  <- pick(vars_long, "name")

# The French SAS programs replaced accented characters (and often a neighbour)
# in every variable label with a literal "?" ("Ann?d'enqu?"); the value labels
# are intact.  The French SPSS files of the same deposits carry the variable
# labels intact, so they are taken from the .sav of two reference months (the
# latest first).  The .sav files (~25 MB each) are cached in work_dir.
sav_months <- c("2005-06", "1995-06")
sav_labels <- do.call(rbind, lapply(sav_months, function(v) {
  dest <- file.path(work_dir, paste0(v, "_fra.sav"))
  if (!file.exists(dest)) {
    f <- canpumf:::.borealis_dataset_files(wide$doi_fra[wide$version == v])
    f <- f[grepl("\\.sav$", f$filename, ignore.case = TRUE), ]
    stopifnot(nrow(f) == 1L)
    canpumf:::.borealis_download_file(f$file_id, paste0(dest, ".part"))
    file.rename(paste0(dest, ".part"), dest)
  }
  x <- haven::read_sav(dest, n_max = 1L)
  data.frame(name = canpumf:::.lfs_hist_canonical_names(names(x)),
             label_fr = vapply(x, function(col) attr(col, "label") %||% NA_character_, ""))
}))
sav_labels <- sav_labels[!is.na(sav_labels$label_fr) & !duplicated(sav_labels$name), ]
bad_fr <- is.na(vars$label_fr) | grepl("?", vars$label_fr, fixed = TRUE)
hit <- bad_fr & vars$name %in% sav_labels$name
vars$label_fr[hit] <- sav_labels$label_fr[match(vars$name[hit], sav_labels$name)]
if (any(bad_fr & !hit))
  warning("French variable labels still damaged: ",
          paste(vars$name[bad_fr & !hit], collapse = ", "))

# Curated corrections (see .claude/docs/longitudinal.md).
fixes <- canpumf:::.lfs_hist_label_fixes()
for (i in seq_len(nrow(fixes))) {
  hit <- codes$name == fixes$name[i] & codes$val == fixes$val[i]
  codes$label_en[hit] <- fixes$label_en[i]
  codes$label_fr[hit] <- fixes$label_fr[i]
}
vars$label_en[vars$name == "SURVMNTH"] <- "Survey month"
vars$label_fr[vars$name == "SURVMNTH"] <- "Mois de l'enquête"

codes <- codes[order(codes$name, as.numeric(codes$val)),
               c("name", "val", "label_en", "label_fr")]
vars  <- vars[order(match(vars$name, unique(vars_long$name))),
              c("name", "label_en", "label_fr", "type", "decimals",
                "missing_low", "missing_high")]
utils::write.csv(codes, file.path(out_dir, "codes.csv"), row.names = FALSE, na = "")
utils::write.csv(vars,  file.path(out_dir, "variables.csv"), row.names = FALSE, na = "")

# ---- 5. Conflict report -----------------------------------------------------

# Labels that differ between months after normalising case/punctuation are
# listed with the months that use each wording; most are abbreviations, a few
# are real changes of meaning that need a curated fix above.
norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))
cl <- codes_long
cl$n <- norm(cl$label_en)
spl <- split(cl, paste(cl$name, cl$val))
conf <- do.call(rbind, lapply(spl, function(d) {
  if (length(unique(d$n)) < 2L) return(NULL)
  agg <- stats::aggregate(version ~ label_en, d, function(v)
    paste0(min(v), "..", max(v), " (", length(v), ")"))
  data.frame(name = d$name[1L], val = d$val[1L], agg)
}))
utils::write.csv(conf, file.path(work_dir, "label_conflicts.csv"), row.names = FALSE)
message("Wrote ", out_dir, ": ", nrow(vars), " variables, ", nrow(codes),
        " codes; ", length(unique(paste(conf$name, conf$val))),
        " (variable, code) pairs with differing wording -> ",
        file.path(work_dir, "label_conflicts.csv"))
