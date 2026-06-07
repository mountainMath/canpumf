fx <- function(...) testthat::test_path("..", "fixtures", "lfs_codebook", ...)

# ---- Synthetic fixture (bilingual) ---------------------------------------

test_that("parse_lfs_codebook: variable labels extracted", {
  m <- canpumf:::parse_lfs_codebook(fx("codebook.csv"))

  expect_s3_class(m$variables, "data.frame")
  expect_setequal(m$variables$name, c("PROV", "SEX", "SURVMNTH", "WAGE"))
  expect_equal(m$variables$label_en[m$variables$name == "PROV"],
               "Province of residence")
  expect_equal(m$variables$label_fr[m$variables$name == "PROV"],
               "Province de residence")
  expect_equal(m$variables$label_en[m$variables$name == "WAGE"],
               "Weekly wages and salary")
})

test_that("parse_lfs_codebook: code labels extracted with correct variable names", {
  m <- canpumf:::parse_lfs_codebook(fx("codebook.csv"))

  prov <- m$codes[m$codes$name == "PROV", ]
  expect_equal(nrow(prov), 3L)
  expect_equal(prov$label_en[prov$val == "35"], "Ontario")
  expect_equal(prov$label_fr[prov$val == "24"], "Quebec")

  sex <- m$codes[m$codes$name == "SEX", ]
  expect_equal(nrow(sex), 2L)
  expect_equal(sex$label_en[sex$val == "1"], "Male")
  expect_equal(sex$label_fr[sex$val == "2"], "Feminin")
})

test_that("parse_lfs_codebook: SURVMNTH codes zero-padded to two digits", {
  m <- canpumf:::parse_lfs_codebook(fx("codebook.csv"))

  mnth <- m$codes[m$codes$name == "SURVMNTH", ]
  expect_equal(nrow(mnth), 3L)
  expect_equal(sort(mnth$val), c("01", "02", "12"))
  expect_equal(mnth$label_en[mnth$val == "01"], "January")
  expect_equal(mnth$label_en[mnth$val == "12"], "December")
})

test_that("parse_lfs_codebook: WAGE has no codes", {
  m <- canpumf:::parse_lfs_codebook(fx("codebook.csv"))

  wage_codes <- m$codes[m$codes$name == "WAGE", ]
  expect_equal(nrow(wage_codes), 0L)
})

test_that("parse_lfs_codebook: variables with codes are character, without are numeric", {
  m <- canpumf:::parse_lfs_codebook(fx("codebook.csv"))

  type_of <- function(nm) m$variables$type[m$variables$name == nm]
  expect_equal(type_of("PROV"),    "character")
  expect_equal(type_of("SEX"),     "character")
  expect_equal(type_of("SURVMNTH"),"character")
  expect_equal(type_of("WAGE"),    "numeric")
})

test_that("parse_lfs_codebook: canonical schema", {
  m <- canpumf:::parse_lfs_codebook(fx("codebook.csv"))

  expect_named(m$variables,
               c("name","label_en","label_fr","type","decimals","missing_low","missing_high"))
  expect_named(m$codes, c("name","val","label_en","label_fr"))
  expect_null(m$layout)
})

test_that("parse_lfs_codebook: label_fr = NA when no French column present", {
  tmp <- withr::local_tempdir()
  # Write a codebook without the French column
  cb <- data.frame(
    Field_Champ               = c("PROV", NA, NA),
    Variable_Variable         = c("PROV", "10", "35"),
    EnglishLabel_EtiquetteAnglais = c("Province", "Ontario", "Newfoundland"),
    stringsAsFactors = FALSE
  )
  readr::write_csv(cb, file.path(tmp, "codebook.csv"))

  m <- canpumf:::parse_lfs_codebook(file.path(tmp, "codebook.csv"))

  expect_true(all(is.na(m$variables$label_fr)))
  expect_true(all(is.na(m$codes$label_fr)))
})

test_that("parse_lfs_codebook: variable names are uppercase", {
  m <- canpumf:::parse_lfs_codebook(fx("codebook.csv"))
  expect_true(all(m$variables$name == toupper(m$variables$name)))
  expect_true(all(m$codes$name == toupper(m$codes$name)))
})

# ---- Real LFS data (skip if not in cache) --------------------------------

test_that("parse_lfs_codebook: real LFS codebook variable count", {
  lfs_path <- Sys.glob(file.path(getOption("canpumf.cache_path", ""),
                                 "LFS", "2023", "*/Documents/codebook.csv"))
  if (length(lfs_path) == 0L)
    lfs_path <- Sys.glob(file.path(getOption("canpumf.cache_path", ""),
                                   "LFS", "2022", "*/Documents/codebook.csv"))
  skip_if(length(lfs_path) == 0L, "LFS codebook not in cache")

  m <- canpumf:::parse_lfs_codebook(lfs_path[[1L]])
  expect_gt(nrow(m$variables), 50L)
  expect_true("PROV" %in% m$variables$name)
  expect_true("SURVMNTH" %in% m$codes$name)

  # SURVMNTH codes should be zero-padded
  mnth_vals <- m$codes$val[m$codes$name == "SURVMNTH"]
  expect_true(all(nchar(mnth_vals) == 2L))
})
