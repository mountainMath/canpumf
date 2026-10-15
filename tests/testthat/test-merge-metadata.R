# Helpers to build small synthetic parsed outputs
make_vars <- function(names, label_en = NULL, label_fr = NULL,
                      miss_lo = NA_real_, miss_hi = NA_real_,
                      decimals = NA_integer_) {
  n <- length(names)
  tibble::tibble(
    name         = names,
    label_en     = if (is.null(label_en)) paste0(names, "_en") else label_en,
    label_fr     = if (is.null(label_fr)) rep(NA_character_, n) else label_fr,
    type         = "character",
    decimals     = rep(decimals, n),
    missing_low  = rep(miss_lo, n),
    missing_high = rep(miss_hi, n)
  )
}

make_codes <- function(name, vals, label_en = NULL, label_fr = NULL) {
  nv <- length(vals)
  tibble::tibble(
    name     = rep(name, nv),
    val      = vals,
    label_en = if (is.null(label_en)) paste0(vals, "_en") else label_en,
    label_fr = if (is.null(label_fr)) rep(NA_character_, nv) else label_fr
  )
}

make_layout <- function(names) {
  tibble::tibble(name = names, start = seq_along(names), end = seq_along(names))
}

make_parsed <- function(var_names, code_name = NULL, code_vals = NULL,
                        label_en = NULL, label_fr = NULL,
                        code_en = NULL, code_fr = NULL,
                        miss_lo = NA_real_, miss_hi = NA_real_,
                        with_layout = FALSE) {
  vars  <- make_vars(var_names, label_en = label_en, label_fr = label_fr,
                     miss_lo = miss_lo, miss_hi = miss_hi)
  codes <- if (!is.null(code_name))
    make_codes(code_name, code_vals, label_en = code_en, label_fr = code_fr)
  else
    canpumf:::empty_codes()
  layout <- if (with_layout) make_layout(var_names) else NULL
  list(variables = vars, codes = codes, layout = layout)
}


# ---- Single-source passthrough ----------------------------------------

test_that("merge_metadata: single source returned unchanged", {
  p <- make_parsed(c("A","B"), "A", c("1","2"))
  m <- canpumf:::merge_metadata(list(only = p))
  expect_equal(m$variables$name, p$variables$name)
  expect_equal(nrow(m$codes), 2L)
})

test_that("merge_metadata: empty list errors", {
  expect_error(canpumf:::merge_metadata(list()), "No parsed metadata")
})


# ---- Priority ordering ------------------------------------------------

test_that("merge_metadata: higher-priority label_en wins", {
  high <- make_parsed("PROV", label_en = "Province (high)")
  low  <- make_parsed("PROV", label_en = "Province (low)")

  m <- canpumf:::merge_metadata(list(spss_mono = high, cpss_csv = low))
  expect_equal(m$variables$label_en[m$variables$name == "PROV"], "Province (high)")
})

test_that("merge_metadata: lower-priority label_fr fills missing", {
  high <- make_parsed("PROV", label_en = "Province", label_fr = NA_character_)
  low  <- make_parsed("PROV", label_en = "Province low", label_fr = "Province fr")

  m <- canpumf:::merge_metadata(list(spss_mono = high, cpss_csv = low))
  expect_equal(m$variables$label_fr[m$variables$name == "PROV"], "Province fr")
})

test_that("merge_metadata: label_fr NOT overwritten when high-priority has it", {
  high <- make_parsed("PROV", label_fr = "Province haute priorité")
  low  <- make_parsed("PROV", label_fr = "Province basse priorité")

  m <- canpumf:::merge_metadata(list(spss_mono = high, cpss_csv = low))
  expect_equal(m$variables$label_fr[m$variables$name == "PROV"], "Province haute priorité")
})


# ---- Variable union ---------------------------------------------------

test_that("merge_metadata: variables from both sources combined", {
  a <- make_parsed(c("AGE","SEX"))
  b <- make_parsed(c("PROV","INC"))

  m <- canpumf:::merge_metadata(list(spss_split = a, cpss_csv = b))
  expect_setequal(m$variables$name, c("AGE","SEX","PROV","INC"))
})


# ---- Code union and priority ------------------------------------------

test_that("merge_metadata: codes from both sources combined by (name, val) union", {
  a <- make_parsed("PROV", "PROV", c("10","24"), code_en = c("NL","QC"))
  b <- make_parsed("PROV", "PROV", c("35"),      code_en = c("ON"))

  m <- canpumf:::merge_metadata(list(spss_mono = a, cpss_csv = b))
  expect_setequal(m$codes$val[m$codes$name == "PROV"], c("10","24","35"))
})

test_that("merge_metadata: conflicting code label_en triggers warning, high-priority wins", {
  a <- make_parsed("PROV", "PROV", "10", code_en = "Newfoundland")
  b <- make_parsed("PROV", "PROV", "10", code_en = "NL")

  expect_warning(
    m <- canpumf:::merge_metadata(list(spss_split = a, cpss_csv = b)),
    "Conflicting English labels"
  )
  expect_equal(m$codes$label_en[m$codes$name == "PROV" & m$codes$val == "10"],
               "Newfoundland")
})

test_that("merge_metadata: code label_fr fills from lower-priority when missing", {
  a <- make_parsed("PROV", "PROV", "10", code_en = "Newfoundland", code_fr = NA_character_)
  b <- make_parsed("PROV", "PROV", "10", code_en = "NL",           code_fr = "Terre-Neuve")

  m <- suppressWarnings(
    canpumf:::merge_metadata(list(spss_split = a, cpss_csv = b))
  )
  expect_equal(m$codes$label_fr[m$codes$name == "PROV" & m$codes$val == "10"],
               "Terre-Neuve")
})


# ---- Missing ranges ---------------------------------------------------

test_that("merge_metadata: missing ranges merged from secondary source", {
  a <- make_parsed("INC", miss_lo = NA_real_, miss_hi = NA_real_)
  b <- make_parsed("INC", miss_lo = 9999996,  miss_hi = 9999999)

  m <- canpumf:::merge_metadata(list(spss_split = a, cpss_csv = b))
  expect_equal(m$variables$missing_low[m$variables$name == "INC"],  9999996)
  expect_equal(m$variables$missing_high[m$variables$name == "INC"], 9999999)
})

test_that("merge_metadata: conflicting missing ranges emit warning", {
  a <- make_parsed("INC", miss_lo = 9999996, miss_hi = 9999999)
  b <- make_parsed("INC", miss_lo = 9999990, miss_hi = 9999999)

  expect_warning(
    canpumf:::merge_metadata(list(spss_split = a, cpss_csv = b)),
    "Conflicting missing ranges"
  )
})


# ---- Layout -----------------------------------------------------------

test_that("merge_metadata: layout taken from first non-NULL source", {
  a <- make_parsed("A", with_layout = TRUE)
  b <- make_parsed("A", with_layout = FALSE)

  m <- canpumf:::merge_metadata(list(spss_mono = a, cpss_csv = b))
  expect_false(is.null(m$layout))
  expect_equal(m$layout$name, "A")
})

test_that("merge_metadata: layout NULL when no source provides one", {
  a <- make_parsed("A")
  b <- make_parsed("B")

  m <- canpumf:::merge_metadata(list(lfs_csv = a, cpss_csv = b))
  expect_null(m$layout)
})

test_that("merge_metadata: warns when layout names not in variable table", {
  a <- make_parsed("A")   # no layout
  b <- make_parsed("B", with_layout = TRUE)  # layout has "B", but merged vars has "A","B"
  # Manufacture a case where layout has an extra name
  b$layout <- tibble::tibble(name = c("B","GHOST"), start = 1:2, end = 1:2)

  expect_warning(
    canpumf:::merge_metadata(list(spss_split = a, cpss_csv = b)),
    "Variables in layout but not in variable labels"
  )
})

# ---- Layout-only numeric columns --------------------------------------------

test_that("merge_metadata: layout-only columns read with decimals become numeric variables", {
  a <- make_parsed("A")
  b <- make_parsed("B", with_layout = TRUE)
  # WTPG and WRPG1 are declared by the DATA LIST alone, with implied decimals
  # (CIUS 2022); PUMFID is declared without decimals
  b$layout <- tibble::tibble(name = c("B", "PUMFID", "WTPG", "WRPG1"),
                             start = 1:4, end = 1:4,
                             decimals = c(NA, NA, 9L, 9L))

  m <- expect_warning(
    canpumf:::merge_metadata(list(spss_split = a, cpss_csv = b)),
    "Variables in layout but not in variable labels: PUMFID$")
  v <- m$variables
  expect_setequal(v$name, c("A", "B", "WTPG", "WRPG1"))
  w <- v[v$name %in% c("WTPG", "WRPG1"), ]
  expect_true(all(w$type == "numeric"))
  expect_true(all(is.na(w$label_en) & is.na(w$label_fr)))
  expect_equal(w$decimals, c(9L, 9L))
  expect_false("PUMFID" %in% v$name)
  expect_equal(names(v), c("name", "label_en", "label_fr", "type", "decimals",
                           "missing_low", "missing_high"))
})

test_that("merge_metadata: a single source also has its layout-only columns promoted", {
  a <- make_parsed("A", with_layout = TRUE)
  a$layout <- tibble::tibble(name = c("A", "ID", "WT"), start = 1:3, end = 1:3,
                             decimals = c(NA, 0L, 2L))
  m <- expect_no_warning(canpumf:::merge_metadata(list(spss_split = a)))
  expect_equal(m$variables$name, c("A", "WT"))
  expect_equal(m$variables$type[m$variables$name == "WT"], "numeric")
  # a layout without a decimals column promotes nothing
  a$layout$decimals <- NULL
  m <- canpumf:::merge_metadata(list(spss_split = a))
  expect_equal(m$variables$name, "A")
})

test_that(".layout_promoted_vars: names the promoted rows only", {
  v <- make_vars(c("A", "WT", "ID"), label_en = c("a", NA, NA))
  v$type[v$name == "WT"] <- "numeric"
  lay <- tibble::tibble(name = c("A", "WT", "ID"), start = 1:3, end = 1:3,
                        decimals = c(NA, 2L, NA))
  expect_equal(canpumf:::.layout_promoted_vars(v, lay), "WT")
  expect_equal(canpumf:::.layout_promoted_vars(v, NULL), character(0L))
})

test_that("check_bilingual_coverage ignores unlabelled promoted columns", {
  v <- make_vars(c("A", "B", "C", "D"), label_fr = c("a", "b", "c", "d"))
  wt <- make_vars(paste0("W", 1:8), label_en = rep(NA_character_, 8L))
  wt$type <- "numeric"
  meta <- list(variables = rbind(v, wt), codes = canpumf:::empty_codes(), layout = NULL)
  expect_no_warning(canpumf:::check_bilingual_coverage(meta))
})


# ---- Canonical output schema ------------------------------------------

test_that("merge_metadata: output has canonical column names", {
  a <- make_parsed("X", "X", "1")
  b <- make_parsed("Y", "Y", "2")

  m <- canpumf:::merge_metadata(list(lfs_csv = a, cpss_csv = b))
  expect_named(m$variables,
               c("name","label_en","label_fr","type","decimals","missing_low","missing_high"))
  expect_named(m$codes, c("name","val","label_en","label_fr"))
})


# ---- detect_formats ----------------------------------------------------

test_that("detect_formats: returns lfs_csv for directory with codebook.csv", {
  tmp <- withr::local_tempdir()
  writeLines("Field_Champ,Variable_Variable", file.path(tmp, "codebook.csv"))

  f <- canpumf:::detect_formats(tmp)
  expect_true("lfs_csv" %in% names(f))
  expect_equal(basename(f$lfs_csv), "codebook.csv")
})

test_that("detect_formats: returns cpss_csv for directory with variables.csv", {
  tmp <- withr::local_tempdir()
  writeLines("Variable,Code", file.path(tmp, "variables.csv"))

  f <- canpumf:::detect_formats(tmp)
  expect_true("cpss_csv" %in% names(f))
  expect_equal(basename(f$cpss_csv), "variables.csv")
})

test_that("detect_formats: returns sas_cards for directory with .lay + .lbe", {
  tmp <- withr::local_tempdir()
  writeLines("dummy", file.path(tmp, "test.lay"))
  writeLines("dummy", file.path(tmp, "test.lbe"))

  f <- canpumf:::detect_formats(tmp)
  expect_true("sas_cards" %in% names(f))
})

test_that("detect_formats: returns spss_split for directory with vare/vale .sps", {
  tmp <- withr::local_tempdir()
  spss_dir <- file.path(tmp, "SPSS")
  dir.create(spss_dir)
  writeLines("VARIABLE LABELS", file.path(spss_dir, "survey_vare.sps"))
  writeLines("VALUE LABELS", file.path(spss_dir, "survey_vale.sps"))

  f <- canpumf:::detect_formats(tmp)
  expect_true("spss_split" %in% names(f))
  expect_false("spss_mono" %in% names(f))
})

test_that("detect_formats: returns spss_mono for single .sps with both block markers", {
  tmp <- withr::local_tempdir()
  spss_dir <- file.path(tmp, "SPSS")
  dir.create(spss_dir)
  writeLines(c("VARIABLE LABELS", "  A 'Label A'", "VALUE LABELS",
               "A", "  1 'One'", "."),
             file.path(spss_dir, "survey.sps"))

  f <- canpumf:::detect_formats(tmp)
  expect_true("spss_mono" %in% names(f))
  expect_false("spss_split" %in% names(f))
  expect_null(f$spss_mono$fra)  # no French sibling
})

test_that("detect_formats: returns empty list for empty directory", {
  tmp <- withr::local_tempdir()
  f <- canpumf:::detect_formats(tmp)
  expect_equal(length(f), 0L)
})


# ---- pumf_parse_metadata -----------------------------------------------

test_that("pumf_parse_metadata: writes metadata/variables.csv", {
  tmp <- withr::local_tempdir()
  fx  <- testthat::test_path("..", "fixtures", "cpss_csv")
  file.copy(file.path(fx, "variables.csv"), file.path(tmp, "variables.csv"))

  canpumf:::pumf_parse_metadata(tmp)
  expect_true(file.exists(file.path(tmp, "metadata", "variables.csv")))
  expect_true(file.exists(file.path(tmp, "metadata", "codes.csv")))
})

test_that("pumf_parse_metadata: skips when metadata already present", {
  tmp <- withr::local_tempdir()
  meta_dir <- file.path(tmp, "metadata")
  dir.create(meta_dir)
  # Write a sentinel variables.csv
  writeLines("name,label_en,label_fr,type,missing_low,missing_high",
             file.path(meta_dir, "variables.csv"))

  # Even with no source files, should not error (just returns early)
  expect_invisible(canpumf:::pumf_parse_metadata(tmp, refresh = FALSE))
})

test_that("pumf_parse_metadata: refresh = TRUE re-parses", {
  tmp <- withr::local_tempdir()
  fx  <- testthat::test_path("..", "fixtures", "lfs_codebook")
  file.copy(file.path(fx, "codebook.csv"), file.path(tmp, "codebook.csv"))

  canpumf:::pumf_parse_metadata(tmp)
  mtime1 <- file.mtime(file.path(tmp, "metadata", "variables.csv"))
  Sys.sleep(1.1)

  canpumf:::pumf_parse_metadata(tmp, refresh = TRUE)
  mtime2 <- file.mtime(file.path(tmp, "metadata", "variables.csv"))
  expect_gt(as.numeric(mtime2), as.numeric(mtime1))
})

test_that("pumf_parse_metadata: errors when no formats detected", {
  tmp <- withr::local_tempdir()
  expect_error(canpumf:::pumf_parse_metadata(tmp), "No parseable metadata files")
})


# ---- Double-encoded UTF-8 repair ---------------------------------------------

test_that(".fix_mojibake undoes UTF-8 read as CP1252 and re-encoded", {
  fix <- canpumf:::.fix_mojibake
  # Census 2021 individuals English .sps: en-dash shipped as "â€“"
  expect_equal(fix("School attendance â€“ Detailed"),
               "School attendance – Detailed")
  expect_equal(fix("cafÃ©"), "café")
  expect_equal(fix("lâ€™âge"), "l’âge")
})

test_that(".fix_mojibake leaves genuine accents and mixed text alone", {
  fix <- canpumf:::.fix_mojibake
  genuine <- c("Âge", "École à café", "Ã", "plain", NA)
  expect_identical(fix(genuine), genuine)
  # A damaged run next to a genuine accent: only the run is repaired.
  expect_equal(fix("cafÃ© été"), "café été")
})

test_that(".fix_metadata_mojibake repairs variable and code labels", {
  meta <- list(
    variables = make_vars("A", label_en = "x â€“ y",
                          label_fr = "Âge"),
    codes     = make_codes("A", "1", label_en = "cafÃ©"),
    layout    = NULL)
  out <- canpumf:::.fix_metadata_mojibake(meta)
  expect_equal(out$variables$label_en, "x – y")
  expect_equal(out$variables$label_fr, "Âge")
  expect_equal(out$codes$label_en, "café")
})

test_that(".fix_label_escapes decodes HTML entities and doubled apostrophes", {
  fix <- canpumf:::.fix_label_escapes
  expect_equal(fix(c("Yukon &amp; Northwest Territories", "No chldrn &lt;15, some &gt;14",
                     "Person 1''s spouse", "Plain", NA)),
               c("Yukon & Northwest Territories", "No chldrn <15, some >14",
                 "Person 1's spouse", "Plain", NA))
  # "&amp;lt;" is decoded once, not twice
  expect_equal(fix("&amp;lt;"), "&lt;")
})


# ---- layout_override / .pumf_layout_from_file() ------------------------

# A registry `layout_file` replaces the merged layout with the named card's
# (GSS Cycle 36 Episode, whose SPSS DATA LIST disagrees with the SAS card, #29).

test_that("merge_metadata: layout_override replaces the layout on both paths", {
  ovr <- tibble::tibble(name = c("B", "A"), start = c(1L, 3L), end = c(2L, 4L),
                        decimals = c(NA_integer_, 2L))
  one <- make_parsed(c("A", "B"), with_layout = TRUE)
  m1  <- canpumf:::merge_metadata(list(spss_split = one), layout_override = ovr)
  expect_identical(m1$layout, ovr)

  two <- make_parsed(c("A", "B"), with_layout = TRUE)
  m2  <- canpumf:::merge_metadata(list(spss_split = one, sas_cards = two),
                                  layout_override = ovr)
  expect_identical(m2$layout, ovr)
  # without the override the first source's layout is kept
  m3  <- canpumf:::merge_metadata(list(spss_split = one, sas_cards = two))
  expect_identical(m3$layout$start, c(1L, 2L))
})

test_that(".pumf_layout_from_file: reads one card, keeps the first of a duplicated name", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "Layout", "SAS"), recursive = TRUE)
  dir.create(file.path(dir, "metadata"))
  card <- c(
    "DATA EPISODE;",
    "   INFILE IN LRECL=23;",
    "   INPUT",
    "    @         1     PUMFID          6.  /*      1 -      6 */",
    "    @         7     INSTANCE        3.  /*      7 -      9 */",
    "    @        10     WGHT_EPI       10.4 /*     10 -     19 */",
    "    @        20     INSTANCE        3.  /*     20 -     22 */",
    "    @        23     FLAG           $1.  /*     23 -     23 */",
    "   ;",
    "RUN;")
  writeLines(card, file.path(dir, "Layout", "SAS", "Episode_i.SAS"))
  # a copy under metadata/ is a side-car, never a candidate
  writeLines(card, file.path(dir, "metadata", "Episode_i.SAS"))

  expect_message(
    lay <- canpumf:::.pumf_layout_from_file(dir, "^episode_i\\.sas$"),
    "declares INSTANCE more than once")
  expect_named(lay, c("name", "start", "end", "decimals"))
  expect_equal(lay$name,     c("PUMFID", "INSTANCE", "WGHT_EPI", "FLAG"))
  expect_equal(lay$start,    c(1L, 7L, 10L, 23L))
  expect_equal(lay$end,      c(6L, 9L, 19L, 23L))
  expect_equal(lay$decimals, c(NA_integer_, NA_integer_, 4L, NA_integer_))

  expect_error(canpumf:::.pumf_layout_from_file(dir, "^nothing$"),
               "matches 0 files")
  writeLines(card, file.path(dir, "Layout", "Episode_i.SAS"))
  expect_error(canpumf:::.pumf_layout_from_file(dir, "^Episode_i\\.SAS$"),
               "matches 2 files")
  writeLines(c("DATA X;", "RUN;"), file.path(dir, "Layout", "SAS", "empty_i.SAS"))
  expect_error(canpumf:::.pumf_layout_from_file(dir, "^empty_i\\.SAS$"),
               "declares no record layout")
})
