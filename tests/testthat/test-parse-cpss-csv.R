fx <- function(...) testthat::test_path("..", "fixtures", "cpss_csv", ...)

# ---- Synthetic fixture (bilingual) ---------------------------------------

test_that("parse_cpss_csv: variable labels extracted", {
  m <- canpumf:::parse_cpss_csv(fx("variables.csv"))

  expect_s3_class(m$variables, "data.frame")
  expect_setequal(m$variables$name, c("PUMFID", "PROV", "GENDER"))
  expect_equal(m$variables$label_en[m$variables$name == "PROV"],  "Province")
  expect_equal(m$variables$label_fr[m$variables$name == "PROV"],  "Province")
  expect_equal(m$variables$label_en[m$variables$name == "PUMFID"],
               "Respondent identification")

  expect_equal(m$variables$label_fr[m$variables$name == "GENDER"], "Genre")

})

test_that("parse_cpss_csv: code labels extracted with correct variable names", {
  m <- canpumf:::parse_cpss_csv(fx("variables.csv"))

  prov <- m$codes[m$codes$name == "PROV", ]
  expect_equal(nrow(prov), 3L)
  expect_equal(prov$label_en[prov$val == "10"], "Newfoundland and Labrador")
  expect_equal(prov$label_fr[prov$val == "24"], "Quebec")

  gender <- m$codes[m$codes$name == "GENDER", ]
  expect_equal(nrow(gender), 2L)
  expect_equal(gender$label_en[gender$val == "1"], "Male")
  expect_equal(gender$label_fr[gender$val == "2"], "Feminin")
})

test_that("parse_cpss_csv: PUMFID has no codes", {
  m <- canpumf:::parse_cpss_csv(fx("variables.csv"))

  pumfid_codes <- m$codes[m$codes$name == "PUMFID", ]
  expect_equal(nrow(pumfid_codes), 0L)
})

test_that("parse_cpss_csv: canonical schema", {
  m <- canpumf:::parse_cpss_csv(fx("variables.csv"))

  expect_named(m$variables,
               c("name","label_en","label_fr","type","decimals","missing_low","missing_high"))
  expect_named(m$codes, c("name","val","label_en","label_fr"))
  expect_null(m$layout)
})

test_that("parse_cpss_csv: label_fr = NA when French columns absent", {
  tmp <- withr::local_tempdir()
  cb <- data.frame(
    Variable              = c("PROV", NA, NA),
    `Variable Name - English` = c("Province", NA, NA),
    Code                  = c(NA, "10", "35"),
    `Label - English`     = c(NA, "Newfoundland", "Ontario"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  readr::write_csv(cb, file.path(tmp, "variables.csv"))

  m <- canpumf:::parse_cpss_csv(file.path(tmp, "variables.csv"))

  expect_true(all(is.na(m$variables$label_fr)))
  expect_true(all(is.na(m$codes$label_fr)))
})

test_that("parse_cpss_csv: variable names are uppercase", {
  m <- canpumf:::parse_cpss_csv(fx("variables.csv"))
  expect_true(all(m$variables$name == toupper(m$variables$name)))
  expect_true(all(m$codes$name == toupper(m$codes$name)))
})

test_that("parse_cpss_csv: column name bilingual suffix is stripped", {
  tmp <- withr::local_tempdir()
  cb <- data.frame(
    `Variable / Variable`              = c("X", NA),
    `Variable Name - English / Nom en` = c("Ex variable", NA),
    `Variable Name - French / Nom fr`  = c("Variable Ex", NA),
    `Code / Code`                      = c(NA, "1"),
    `Label - English / Étiquette en`   = c(NA, "Yes"),
    `Label - French / Étiquette fr`    = c(NA, "Oui"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  readr::write_csv(cb, file.path(tmp, "variables.csv"))

  m <- canpumf:::parse_cpss_csv(file.path(tmp, "variables.csv"))
  expect_equal(m$variables$name, "X")
  expect_equal(m$variables$label_en, "Ex variable")
  expect_equal(m$variables$label_fr, "Variable Ex")
  expect_equal(m$codes$label_en, "Yes")
  expect_equal(m$codes$label_fr, "Oui")
})
