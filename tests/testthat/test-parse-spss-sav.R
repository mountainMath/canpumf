cis2017_path <- "/Users/jens/data/pumf.data/CIS2017.sav"

# ---- Real CIS 2017 .sav (skip if not in cache) --------------------------

test_that("parse_spss_sav: variable and code counts for CIS 2017", {
  skip_if_not(file.exists(cis2017_path), "CIS 2017 .sav not in cache")

  m <- canpumf:::parse_spss_sav(cis2017_path)

  expect_gt(nrow(m$variables), 100L)
  expect_gt(nrow(m$codes), 100L)
  expect_null(m$layout)
})

test_that("parse_spss_sav: canonical schema", {
  skip_if_not(file.exists(cis2017_path), "CIS 2017 .sav not in cache")

  m <- canpumf:::parse_spss_sav(cis2017_path)

  expect_named(m$variables,
               c("name","label_en","label_fr","type","decimals","missing_low","missing_high"))
  expect_named(m$codes, c("name","val","label_en","label_fr"))
})

test_that("parse_spss_sav: variable names are uppercase", {
  skip_if_not(file.exists(cis2017_path), "CIS 2017 .sav not in cache")

  m <- canpumf:::parse_spss_sav(cis2017_path)
  expect_true(all(m$variables$name == toupper(m$variables$name)))
})

test_that("parse_spss_sav: type correctly inferred from format.spss", {
  skip_if_not(file.exists(cis2017_path), "CIS 2017 .sav not in cache")

  m <- canpumf:::parse_spss_sav(cis2017_path)

  # YEAR: F4.0 → numeric, 0 decimals
  yr <- m$variables[m$variables$name == "YEAR", ]
  expect_equal(yr$type, "numeric")
  expect_equal(yr$decimals, 0L)

  # FWEIGHT: F11.4 → numeric, 4 decimals
  fw <- m$variables[m$variables$name == "FWEIGHT", ]
  expect_equal(fw$type, "numeric")
  expect_equal(fw$decimals, 4L)

  # PROV: A2 / has value labels → character, NA decimals
  pv <- m$variables[m$variables$name == "PROV", ]
  expect_equal(pv$type, "character")
  expect_true(is.na(pv$decimals))
})

test_that("parse_spss_sav: value labels extracted for PROV", {
  skip_if_not(file.exists(cis2017_path), "CIS 2017 .sav not in cache")

  m <- canpumf:::parse_spss_sav(cis2017_path)

  prov <- m$codes[m$codes$name == "PROV", ]
  expect_gt(nrow(prov), 5L)
  on_row <- prov[prov$val == "35", ]
  expect_equal(on_row$label_en, "Ontario")
})

test_that("parse_spss_sav: label_fr is always NA (single-language .sav)", {
  skip_if_not(file.exists(cis2017_path), "CIS 2017 .sav not in cache")

  m <- canpumf:::parse_spss_sav(cis2017_path)
  expect_true(all(is.na(m$variables$label_fr)))
  expect_true(all(is.na(m$codes$label_fr)))
})

# ---- detect_formats picks up .sav when no command files present ----------

test_that("detect_formats: detects .sav when no SPSS command files exist", {
  tmp <- withr::local_tempdir()
  file.copy(cis2017_path, file.path(tmp, "test.sav"))

  skip_if_not(file.exists(cis2017_path), "CIS 2017 .sav not in cache")
  f <- canpumf:::detect_formats(tmp)
  expect_true("spss_sav" %in% names(f))
  expect_equal(basename(f$spss_sav), "test.sav")
})

test_that("detect_formats: does NOT detect .sav when SPSS split files also present", {
  tmp     <- withr::local_tempdir()
  spss_d  <- file.path(tmp, "SPSS")
  dir.create(spss_d)
  writeLines("VARIABLE LABELS", file.path(spss_d, "survey_vare.sps"))
  writeLines("VALUE LABELS",    file.path(spss_d, "survey_vale.sps"))
  writeLines("dummy",           file.path(tmp, "data.sav"))

  f <- canpumf:::detect_formats(tmp)
  expect_false("spss_sav" %in% names(f))
  expect_true("spss_split" %in% names(f))
})
