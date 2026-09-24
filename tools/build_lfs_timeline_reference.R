# Build the harmonisation tables behind get_lfs_timeline().
#
# get_lfs_timeline() stacks the historical LFS (LFS_HIST, 1976-2005) and the
# current LFS (2006 onward) into one lazy table with a curated common schema.
# This script writes
#
#   inst/extdata/lfs_timeline/variables.csv  harmonised variables, their
#                                            sources in each series, scaling
#   inst/extdata/lfs_timeline/codes.csv      harmonised bilingual value labels
#   inst/extdata/lfs_timeline/recodes.csv    (source, source variable, source
#                                            code) -> harmonised code
#
# The code lists of the sources are taken from the shipped LFS_HIST dictionary
# (inst/extdata/lfs_hist/) and from the metadata of every current-LFS version in
# the cache (<canpumf.cache_path>/LFS/<version>/metadata/codes.csv), so load the
# current LFS versions first.  The script stops if any source code of a
# harmonised variable is left without a mapping (map it, or to NA explicitly).
#
#   Rscript tools/build_lfs_timeline_reference.R
#
# This file is .Rbuildignore'd (tools/).

suppressMessages(devtools::load_all(quiet = TRUE))

cache   <- getOption("canpumf.cache_path")
out_dir <- file.path("inst", "extdata", "lfs_timeline")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
num <- function(x) as.character(as.integer(x))   # "01" -> "1"

# ---- Source code lists ------------------------------------------------------

hist_codes <- canpumf:::.lfs_hist_ref("codes")
hist_vars  <- canpumf:::.lfs_hist_ref("variables")
hist_era   <- canpumf:::.lfs_hist_code_eras()

lfs_dirs <- list.files(file.path(cache, "LFS"), pattern = "^[0-9]{4}(-[0-9]{2})?$",
                       full.names = TRUE)
lfs_dirs <- lfs_dirs[file.exists(file.path(lfs_dirs, "metadata", "codes.csv"))]
stopifnot(length(lfs_dirs) > 0L)
rd <- function(f) readr::read_csv(f, col_types = readr::cols(.default = "c"),
                                  na = "", progress = FALSE)
lfs_codes <- do.call(rbind, lapply(rev(lfs_dirs), function(d)   # newest first
  cbind(rd(file.path(d, "metadata", "codes.csv"))[, c("name", "val", "label_en", "label_fr")],
        version = basename(d))))
lfs_vars <- do.call(rbind, lapply(rev(lfs_dirs), function(d)
  rd(file.path(d, "metadata", "variables.csv"))[, c("name", "label_en", "label_fr")]))
lfs_vars <- lfs_vars[!duplicated(lfs_vars$name), ]
lfs_codes$val <- num(lfs_codes$val)
latest_lfs <- lfs_codes[!duplicated(lfs_codes[c("name", "val")]), ]   # newest label
message("Current LFS metadata: ", basename(lfs_dirs[1]), " .. ",
        basename(lfs_dirs[length(lfs_dirs)]), " (", length(lfs_dirs), " versions)")

# ---- Curated variable list --------------------------------------------------

# Categorical variables with the same codes in both series (possibly under
# another name in LFS_HIST): the harmonised labels are the current LFS labels.
same_codes <- c(PROV = "PROV", AGE_12 = "AGE_12", AGE_6 = "AGE_6",
                EDUC = "EDUC90", MJH = "MJH", EVERWORK = "EVERWORK",
                FTPTLAST = "FTPTLAST", COWMAIN = "COWMAIN",
                YABSENT = "YABSENT", PAYAWAY = "PAYAWAY",
                FTPTMAIN = "FTPTMAIN", YAWAY = "YAWAY", WHYPT = "WHYPTNEW",
                UNION = "UNION", PERMTEMP = "PERMTEMP", ESTSIZE = "ESTSIZE",
                FIRMSIZE = "FIRMSIZE", FLOWUNEM = "FLOWUNEM",
                UNEMFTPT = "UNEMFTPT", WHYLEFTO = "WHYLEFTO",
                WHYLEFTN = "WHYLEFTN", AVAILABL = "AVAILABL",
                LKPUBAG = "LKPUBAG", LKEMPLOY = "LKEMPLOY", LKRELS = "LKRELS",
                LKATADS = "LKATADS", LKANSADS = "LKANSADS",
                LKOTHERN = "LKOTHER", PRIORACT = "PRIORACT",
                YNOLOOK = "YNOLOOK", TLOLOOK = "TLOLOOK",
                EFAMTYPE = "EFAMTYPE")

# Recoded variables: harmonised codes, labels, and the code map of each source
# as "source code = harmonised code" (unlisted source codes are an error).
m <- function(...) { x <- c(...); stats::setNames(unname(x), names(x)) }
recoded <- list(
  LFSSTAT = list(
    hist = "LFSSTAT", lfs = "LFSSTAT",
    map_hist = m(`1` = "1", `2` = "2", `3` = "3", `4` = "3", `5` = "3", `6` = "4"),
    map_lfs  = m(`1` = "1", `2` = "2", `3` = "3", `4` = "4")),
  GENDER_SEX = list(
    hist = "SEX", lfs = c("GENDER", "SEX"),
    label_en = "Gender/sex of respondent", label_fr = "Genre/sexe du répondant",
    codes = "GENDER",
    map_hist = m(`1` = "1", `2` = "2"),
    map_lfs  = m(`1` = "1", `2` = "2")),
  MARSTAT = list(
    hist = "MARSTAT", lfs = "MARSTAT",
    label_en = "Marital status of respondent (4 categories)",
    label_fr = "État matrimonial du répondant (4 catégories)",
    codes = hist_era[, c("val", "label_en", "label_fr")],
    # six categories: married, common-law, widowed, separated, divorced, single
    map_hist = m(`1` = "1", `2` = "1", `3` = "3", `4` = "4", `5` = "4", `6` = "2"),
    map_era  = m(`1` = "1", `2` = "2", `3` = "3", `4` = "4"),
    map_lfs  = m(`1` = "1", `2` = "1", `3` = "3", `4` = "4", `5` = "4", `6` = "2")),
  CMA = list(
    hist = "CMA", lfs = "CMA", hist_from = "1987-01",
    label_en = "Three largest Census Metropolitan Areas",
    label_fr = "Trois plus grandes régions métropolitaines de recensement",
    codes = data.frame(val = c("1", "2", "3", "4"),
      label_en = c("Montréal", "Toronto", "Vancouver", "Other CMA or non-CMA"),
      label_fr = c("Montréal", "Toronto", "Vancouver", NA)),
    map_hist = m(`1` = "1", `2` = "2", `3` = "3", `4` = "4"),
    map_lfs  = m(`0` = "4", `1` = "4", `2` = "1", `3` = "4", `4` = "2", `5` = "4",
                 `6` = "4", `7` = "4", `8` = "4", `9` = "3")),
  SCHOOLN = list(
    hist = "SCHOOLN", lfs = "SCHOOLN",
    map_hist = m(`1` = "1", `2` = "2", `3` = "3", `4` = "2", `5` = "3", `6` = "2",
                 `7` = "3", `8` = "2", `9` = "3"),
    map_lfs  = m(`1` = "1", `2` = "2", `3` = "3")),
  AGYOWNK = list(
    hist = "AGYOWNKN", lfs = "AGYOWNK",
    map_hist = m(`1` = "1", `2` = "1", `3` = "2", `4` = "3", `5` = "3", `6` = "4"),
    map_lfs  = m(`1` = "1", `2` = "2", `3` = "3", `4` = "4")),
  NAICS_18 = list(
    hist = "NAICS_18", lfs = "NAICS_21",
    label_en = "Industry of main job - NAICS 2007 (18 categories)",
    label_fr = "Branche d'activité de l'emploi principal - SCIAN 2007 (18 catégories)",
    codes = hist_codes[hist_codes$name == "NAICS_18", c("val", "label_en", "label_fr")],
    map_hist = stats::setNames(as.character(1:18), 1:18),
    map_lfs  = stats::setNames(as.character(c(1, 2, 2, 2, 3:9, 10, 10, 11:18)), 1:21))
)

# Numeric variables; `lfs_scale` undoes the implied decimals of the current
# LFS files (hours in tenths, wages in cents).  LFS_HIST is in plain units.
numeric_vars <- tibble::tribble(
  ~name,      ~hist,      ~lfs_scale,
  "WKSAWAY",  "WKSAWAY",  1,
  "UHRSMAIN", "UHRSMAIN", 0.1,
  "AHRSMAIN", "AHRSMAIN", 0.1,
  "UTOTHRS",  "UTOTHRS",  0.1,
  "ATOTHRS",  "ATOTHRS",  0.1,
  "HRSAWAY",  "HRSAWAY",  0.1,
  "PAIDOT",   "PAIDOT",   0.1,
  "UNPAIDOT", "UNPAIDOT", 0.1,
  "XTRAHRS",  "XTRAHRS",  0.1,
  "TENURE",   "TENURE",   1,
  "PREVTEN",  "PREVTEN",  1,
  "HRLYEARN", "HRLYEARN", 0.01,
  "DURUNEMP", "DURUNEMP", 1,
  "DURJLESS", "DURJLESS", 1,
  "FINALWT",  "FWEIGHT",  1)

# ---- Assemble ---------------------------------------------------------------

vars <- list(); codes <- list(); recodes <- list()
lfs_label <- function(v, col) lfs_vars[[col]][match(v, lfs_vars$name)]

add_recodes <- function(name, source, source_var, map, source_codes) {
  sc <- unique(num(source_codes$val[source_codes$name == source_var]))
  miss <- setdiff(sc, names(map))
  if (length(miss))
    stop(name, ": ", source, " ", source_var, " codes without a mapping: ",
         paste(miss, collapse = ", "), call. = FALSE)
  extra <- setdiff(names(map), sc)
  if (length(extra))
    message(name, ": ", source, " ", source_var, " mapping for codes the ",
            "source lacks (kept): ", paste(extra, collapse = ", "))
  data.frame(name = name, source = source, source_var = source_var,
             source_val = names(map), val = unname(map))
}

for (nm in names(same_codes)) {
  h   <- same_codes[[nm]]
  tgt <- latest_lfs[latest_lfs$name == nm, c("val", "label_en", "label_fr")]
  stopifnot(nrow(tgt) > 0L)
  hv  <- unique(num(hist_codes$val[hist_codes$name == h]))
  if (!setequal(hv, tgt$val))
    message(nm, ": code sets differ (LFS_HIST ", h, " only: ",
            paste(setdiff(hv, tgt$val), collapse = " "), "; LFS only: ",
            paste(setdiff(tgt$val, hv), collapse = " "), ")")
  idm <- function(v) stats::setNames(ifelse(v %in% tgt$val, v, NA), v)
  vars[[nm]] <- data.frame(name = nm, label_en = lfs_label(nm, "label_en"),
                           label_fr = lfs_label(nm, "label_fr"), type = "factor",
                           lfs_hist = h, lfs = nm, lfs_scale = NA, hist_from = NA)
  codes[[nm]] <- cbind(name = nm, tgt)
  recodes[[nm]] <- rbind(
    add_recodes(nm, "LFS_HIST", h, idm(hv), hist_codes),
    add_recodes(nm, "LFS", nm, idm(unique(num(lfs_codes$val[lfs_codes$name == nm]))),
                lfs_codes))
}

for (nm in names(recoded)) {
  r   <- recoded[[nm]]
  tgt <- if (is.data.frame(r$codes)) r$codes else
    latest_lfs[latest_lfs$name == (r$codes %||% nm), c("val", "label_en", "label_fr")]
  tgt <- as.data.frame(tgt)
  # French label of a curated code missing: fall back to the LFS label
  if (anyNA(tgt$label_fr) && nm == "CMA")
    tgt$label_fr[is.na(tgt$label_fr)] <-
      latest_lfs$label_fr[latest_lfs$name == "CMA" & latest_lfs$val == "0"]
  vars[[nm]] <- data.frame(
    name = nm, label_en = r$label_en %||% lfs_label(nm, "label_en"),
    label_fr = r$label_fr %||% lfs_label(nm, "label_fr"), type = "factor",
    lfs_hist = r$hist, lfs = paste(r$lfs, collapse = "|"), lfs_scale = NA,
    hist_from = r$hist_from %||% NA)
  codes[[nm]] <- cbind(name = nm, tgt)
  rc <- list(add_recodes(nm, "LFS_HIST", r$hist, r$map_hist, hist_codes))
  if (!is.null(r$map_era))
    rc <- c(rc, list(add_recodes(nm, "LFS_HIST_ERA", r$hist, r$map_era, hist_era)))
  for (lv in r$lfs)
    rc <- c(rc, list(add_recodes(nm, "LFS", lv, r$map_lfs, lfs_codes)))
  recodes[[nm]] <- do.call(rbind, rc)
  stopifnot(all(recodes[[nm]]$val %in% c(tgt$val, NA)))
}

for (i in seq_len(nrow(numeric_vars))) {
  nv <- numeric_vars[i, ]
  vars[[nv$name]] <- data.frame(
    name = nv$name, label_en = lfs_label(nv$name, "label_en"),
    label_fr = lfs_label(nv$name, "label_fr"), type = "numeric",
    lfs_hist = nv$hist, lfs = nv$name, lfs_scale = nv$lfs_scale, hist_from = NA)
}

vars <- rbind(
  data.frame(name = c("SOURCE", "SURVYEAR", "SURVMNTH"),
             label_en = c("Source series", "Survey year", "Survey month"),
             label_fr = c("Série source", "Année d'enquête", "Mois de l'enquête"),
             type = c("character", "integer", "integer"),
             lfs_hist = c(NA, "SURVYEAR", "SURVMNTH"), lfs = c(NA, "SURVYEAR", "SURVMNTH"),
             lfs_scale = NA, hist_from = NA),
  do.call(rbind, vars))
stopifnot(!anyNA(vars$label_en), !anyNA(vars$label_fr),
          all(vars$lfs_hist[!is.na(vars$lfs_hist)] %in% hist_vars$name))
codes   <- do.call(rbind, codes)
recodes <- do.call(rbind, recodes)
stopifnot(!anyNA(codes$label_en), !anyNA(codes$label_fr))

utils::write.csv(vars,    file.path(out_dir, "variables.csv"), row.names = FALSE, na = "")
utils::write.csv(codes,   file.path(out_dir, "codes.csv"),     row.names = FALSE, na = "")
utils::write.csv(recodes, file.path(out_dir, "recodes.csv"),   row.names = FALSE, na = "")
message("Wrote ", out_dir, ": ", nrow(vars), " variables, ", nrow(codes),
        " codes, ", nrow(recodes), " recodes")
