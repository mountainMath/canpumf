mk_meta <- function(with_french = TRUE, with_layout = TRUE) {
  fr_var   <- if (with_french) c("Statut d'activité", "Âge du répondant", "Province") else c(NA_character_, NA_character_, NA_character_)
  fr_codes <- if (with_french) c("Employé, a travaillé", "Employé, absent", "Terre-Neuve", "Î.-P.-É.") else rep(NA_character_, 4)

  list(
    variables = tibble::tibble(
      name         = c("LFSSTAT", "AGE", "PROV"),
      label_en     = c("Labour force status", "Age of respondent", "Province"),
      label_fr     = fr_var,
      type         = c("character", "numeric", "character"),
      missing_low  = c(NA_real_, 98, NA_real_),
      missing_high = c(NA_real_, 99, NA_real_)
    ),
    codes = tibble::tibble(
      name     = c("LFSSTAT", "LFSSTAT", "PROV", "PROV"),
      val      = c("1", "2", "10", "11"),
      label_en = c("Employed, worked", "Employed, absent", "Newfoundland", "PEI"),
      label_fr = fr_codes
    ),
    layout = if (with_layout)
      tibble::tibble(name = c("LFSSTAT", "AGE", "PROV"), start = c(1L, 2L, 4L), end = c(1L, 3L, 5L))
    else
      NULL
  )
}

test_that("full bilingual round-trip (variables, codes, layout)", {
  meta    <- mk_meta()
  vdir    <- withr::local_tempdir()
  mdir    <- file.path(vdir, "metadata")
  canpumf:::write_metadata(meta, mdir)

  expect_setequal(sort(dir(mdir)), c("codes.csv", "layout.csv", "variables.csv"))

  back <- canpumf:::read_metadata(mdir)
  expect_identical(back$variables, meta$variables)
  expect_identical(back$codes,     meta$codes)
  expect_identical(back$layout,    meta$layout)
})

test_that("round-trip without layout returns NULL layout", {
  meta <- mk_meta(with_layout = FALSE)
  mdir <- withr::local_tempdir()
  canpumf:::write_metadata(meta, mdir)

  back <- canpumf:::read_metadata(mdir)
  expect_null(back$layout)
  expect_identical(back$variables, meta$variables)
  expect_identical(back$codes,     meta$codes)
})

test_that("round-trip with all-NA label_fr (French absent)", {
  meta <- mk_meta(with_french = FALSE)
  mdir <- withr::local_tempdir()
  canpumf:::write_metadata(meta, mdir)      # no coverage warning: ALL NA is expected

  back <- canpumf:::read_metadata(mdir)
  expect_true(all(is.na(back$variables$label_fr)))
  expect_true(all(is.na(back$codes$label_fr)))
})

test_that("round-trip with empty codes table", {
  meta <- mk_meta()
  meta$codes <- tibble::tibble(name = character(), val = character(),
                               label_en = character(), label_fr = character())
  mdir <- withr::local_tempdir()
  canpumf:::write_metadata(meta, mdir)

  back <- canpumf:::read_metadata(mdir)
  expect_identical(back$codes, meta$codes)
})

test_that("metadata_exists: TRUE after write, FALSE for empty dir", {
  meta <- mk_meta()
  vdir <- withr::local_tempdir()
  mdir <- file.path(vdir, "metadata")
  canpumf:::write_metadata(meta, mdir)

  expect_true(canpumf:::metadata_exists(vdir))
  expect_false(canpumf:::metadata_exists(withr::local_tempdir()))
})

test_that("validate_metadata errors on missing required element", {
  meta <- mk_meta()
  expect_error(canpumf:::write_metadata(list(variables = meta$variables), tempfile()),
               "missing elements")
})

test_that("validate_metadata errors on missing required columns", {
  meta <- mk_meta()
  meta$variables$label_en <- NULL           # drop required column
  expect_error(canpumf:::write_metadata(meta, tempfile()), "missing columns")
})

test_that("validate_metadata warns on unexpected type value", {
  meta <- mk_meta()
  meta$variables$type[1] <- "factor"
  expect_warning(canpumf:::write_metadata(meta, tempfile()), "unexpected values")
})
