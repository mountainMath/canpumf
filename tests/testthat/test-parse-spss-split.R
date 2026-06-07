fx_dir <- function() testthat::test_path("..", "fixtures", "spss_split")

# ---- Synthetic fixture (no layout_mask needed) --------------------------

test_that("parse_spss_split: VARIABLE LABELS extracted", {
  m <- canpumf:::parse_spss_split(fx_dir())

  expect_s3_class(m$variables, "data.frame")
  expect_true(all(c("name","label_en","label_fr","type","decimals","missing_low","missing_high")
                  %in% names(m$variables)))
  expect_setequal(m$variables$name, c("FAMID","AGEGRP","PROV","INCOME","SEX"))
  expect_equal(m$variables$label_en[m$variables$name == "AGEGRP"], "Age group")
  expect_equal(m$variables$label_en[m$variables$name == "INCOME"], "Total income")
})

test_that("parse_spss_split: VALUE LABELS with quoted codes extracted", {
  m <- canpumf:::parse_spss_split(fx_dir())

  agegrp <- m$codes[m$codes$name == "AGEGRP", ]
  expect_equal(nrow(agegrp), 4L)
  # Codes should be the string "01", "02", etc. — not stripped of leading zero
  expect_equal(agegrp$val[agegrp$label_en == "Under 25 years"], "01")
  expect_equal(agegrp$label_en[agegrp$val == "01"], "Under 25 years")
})

test_that("parse_spss_split: VALUE LABELS with unquoted codes extracted", {
  m <- canpumf:::parse_spss_split(fx_dir())

  prov <- m$codes[m$codes$name == "PROV", ]
  expect_equal(nrow(prov), 5L)
  # Unquoted integer codes
  expect_equal(prov$label_en[prov$val == "10"], "Newfoundland and Labrador")
  expect_equal(prov$label_en[prov$val == "35"], "Ontario")
})

test_that("parse_spss_split: mixed quoted and unquoted codes in same file", {
  m <- canpumf:::parse_spss_split(fx_dir())
  # AGEGRP uses quoted codes, PROV uses unquoted — both must parse
  expect_gt(nrow(m$codes[m$codes$name == "AGEGRP", ]), 0L)
  expect_gt(nrow(m$codes[m$codes$name == "PROV",   ]), 0L)
})

test_that("parse_spss_split: MISSING VALUES extracted", {
  m <- canpumf:::parse_spss_split(fx_dir())

  income_row <- m$variables[m$variables$name == "INCOME", ]
  expect_equal(income_row$missing_low,  99999996)
  expect_equal(income_row$missing_high, 99999999)

  agegrp_row <- m$variables[m$variables$name == "AGEGRP", ]
  expect_equal(agegrp_row$missing_low,  99)
  expect_equal(agegrp_row$missing_high, 99)
})

test_that("parse_spss_split: layout extracted from _i.sps", {
  m <- canpumf:::parse_spss_split(fx_dir())

  expect_false(is.null(m$layout))
  expect_setequal(m$layout$name, c("FAMID","AGEGRP","PROV","INCOME","SEX"))

  income_lay <- m$layout[m$layout$name == "INCOME", ]
  expect_equal(income_lay$start, 11L)
  expect_equal(income_lay$end,   18L)

  # Single-column variable (SEX occupies column 19 only)
  sex_lay <- m$layout[m$layout$name == "SEX", ]
  expect_equal(sex_lay$start, 19L)
  expect_equal(sex_lay$end,   19L)
})

test_that("parse_spss_split: type inferred from codes, missing ranges, and (A) annotations", {
  m <- canpumf:::parse_spss_split(fx_dir())

  vars <- m$variables
  # Has value labels → character
  expect_equal(vars$type[vars$name == "AGEGRP"], "character")
  expect_equal(vars$type[vars$name == "PROV"],   "character")
  expect_equal(vars$type[vars$name == "SEX"],    "character")
  # Has missing range, no codes → numeric
  expect_equal(vars$type[vars$name == "INCOME"], "numeric")
  # No codes, no missing, but (A) format → character
  expect_equal(vars$type[vars$name == "FAMID"],  "character")
})

test_that("parse_spss_split: label_fr = NA when no French files", {
  # Point at a temp directory with only English files
  tmp <- withr::local_tempdir()
  file.copy(file.path(fx_dir(), "simple_i.sps"),    file.path(tmp, "test_i.sps"))
  file.copy(file.path(fx_dir(), "simple_vare.sps"), file.path(tmp, "test_vare.sps"))
  file.copy(file.path(fx_dir(), "simple_vale.sps"), file.path(tmp, "test_vale.sps"))
  file.copy(file.path(fx_dir(), "simple_miss.sps"), file.path(tmp, "test_miss.sps"))

  m <- canpumf:::parse_spss_split(tmp)
  expect_true(all(is.na(m$variables$label_fr)))
  expect_true(all(is.na(m$codes$label_fr)))
})

test_that("parse_spss_split: French labels joined when varf/valf present", {
  m <- canpumf:::parse_spss_split(fx_dir())

  expect_false(any(is.na(m$variables$label_fr)))
  expect_equal(m$variables$label_fr[m$variables$name == "AGEGRP"], "Groupe d age")
  expect_equal(m$variables$label_fr[m$variables$name == "INCOME"], "Revenu total")

  agegrp_fr <- m$codes[m$codes$name == "AGEGRP" & m$codes$val == "01", ]$label_fr
  expect_equal(agegrp_fr, "Moins de 25 ans")

  prov_fr <- m$codes[m$codes$name == "PROV" & m$codes$val == "24", ]$label_fr
  expect_equal(prov_fr, "Quebec")
})

test_that("parse_spss_split: canonical schema returned", {
  m <- canpumf:::parse_spss_split(fx_dir())
  expect_named(m$variables, c("name","label_en","label_fr","type","decimals","missing_low","missing_high"))
  expect_named(m$codes,     c("name","val","label_en","label_fr"))
  expect_named(m$layout,    c("name","start","end"))
})

# ---- Real SFS 2019 data (skip if not in cache) -------------------------

sfs2019_dir <- "/Users/jens/data/pumf.data/SFS/SFS2019/SPSS"

test_that("parse_spss_split: SFS 2019 variable count and known labels", {
  skip_if_not(dir.exists(sfs2019_dir), "SFS 2019 not in cache")

  m <- canpumf:::parse_spss_split(sfs2019_dir, layout_mask = "EFAM_PUMF_[^R]",
                                   encoding = "Latin1")

  expect_gt(nrow(m$variables), 50L)
  expect_equal(m$variables$label_en[m$variables$name == "PWEIGHT"], "Survey weights - PUMF")

  # Some AGEGRP codes with zero-padded quoted values
  agegrp <- m$codes[m$codes$name == "PAGEMIEG", ]
  expect_true("01" %in% agegrp$val)
  expect_equal(agegrp$label_en[agegrp$val == "01"], "Under 20 years")

  # Layout present
  expect_false(is.null(m$layout))
  expect_gt(nrow(m$layout), 50L)
})

test_that("parse_spss_split: SFS 2019 bilingual", {
  skip_if_not(dir.exists(sfs2019_dir), "SFS 2019 not in cache")

  m <- canpumf:::parse_spss_split(sfs2019_dir, layout_mask = "EFAM_PUMF_[^R]",
                                   encoding = "Latin1")

  n_fr <- sum(!is.na(m$variables$label_fr))
  expect_gt(n_fr, 30L)
})

test_that("parse_spss_split: SFS 2012 (SpssCard directory, different mask)", {
  sfs2012_dir <- "/Users/jens/data/pumf.data/SFS/2012/SpssCard"
  skip_if_not(dir.exists(sfs2012_dir), "SFS 2012 not in cache")

  m <- canpumf:::parse_spss_split(sfs2012_dir, encoding = "Latin1")
  expect_gt(nrow(m$variables), 20L)
})

test_that("parse_spss_split: CHS 2021 split files", {
  chs2021_dir <- "/Users/jens/data/pumf.data/CHS/2021/2021_CHS_PUMF/Layouts/SPSS"
  skip_if_not(dir.exists(chs2021_dir), "CHS 2021 not in cache")

  m <- canpumf:::parse_spss_split(chs2021_dir, layout_mask = "CHS2021ECL_PUMF",
                                   encoding = "CP1252")

  expect_gt(nrow(m$variables), 30L)
  n_fr <- sum(!is.na(m$variables$label_fr))
  expect_gt(n_fr, 20L)
})

# ---- Generate golden fixtures ---------------------------------------------
if (identical(Sys.getenv("CANPUMF_REGEN_FIXTURES"), "1") && dir.exists(sfs2019_dir)) {
  m <- canpumf:::parse_spss_split(sfs2019_dir, layout_mask = "EFAM_PUMF_[^R]",
                                   encoding = "Latin1")
  canpumf:::write_metadata(m, testthat::test_path("..", "fixtures", "sfs2019"))
  message("sfs2019 golden fixtures written")
}
