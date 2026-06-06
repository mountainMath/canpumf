mk_meta_bilingual <- function() {
  list(
    variables = tibble::tibble(
      name         = c("LFSSTAT", "AGE", "PROV"),
      label_en     = c("Labour force status", "Age of respondent", "Province"),
      label_fr     = c("Statut d'activité", "Âge du répondant", "Province"),
      type         = c("character", "numeric", "character"),
      missing_low  = c(NA_real_, 98, NA_real_),
      missing_high = c(NA_real_, 99, NA_real_)
    ),
    codes = tibble::tibble(
      name     = c("LFSSTAT", "LFSSTAT", "PROV"),
      val      = c("1", "2", "10"),
      label_en = c("Employed, worked", "Employed, absent", "Newfoundland"),
      label_fr = c("Employé, a travaillé", "Employé, absent", "Terre-Neuve")
    ),
    layout = NULL
  )
}

mk_meta_no_french <- function() {
  m <- mk_meta_bilingual()
  m$variables$label_fr <- NA_character_
  m$codes$label_fr     <- NA_character_
  m
}


# --- Schema: both label columns present ----------------------------------

test_that("variables.csv has label_en and label_fr columns", {
  meta <- mk_meta_bilingual()
  mdir <- withr::local_tempdir()
  canpumf:::write_metadata(meta, mdir)

  back <- canpumf:::read_metadata(mdir)
  expect_true("label_en" %in% names(back$variables))
  expect_true("label_fr" %in% names(back$variables))
})

test_that("codes.csv has label_en and label_fr columns", {
  meta <- mk_meta_bilingual()
  mdir <- withr::local_tempdir()
  canpumf:::write_metadata(meta, mdir)

  back <- canpumf:::read_metadata(mdir)
  expect_true("label_en" %in% names(back$codes))
  expect_true("label_fr" %in% names(back$codes))
})


# --- French-absent behaviour ---------------------------------------------

test_that("all-NA label_fr writes and reads without error or warning", {
  meta <- mk_meta_no_french()
  mdir <- withr::local_tempdir()
  # complete absence of French is expected — no warning
  expect_no_warning(canpumf:::write_metadata(meta, mdir))

  back <- canpumf:::read_metadata(mdir)
  expect_true(all(is.na(back$variables$label_fr)))
  expect_true(all(is.na(back$codes$label_fr)))
})


# --- Bilingual coverage warning ------------------------------------------

test_that("check_bilingual_coverage warns when >20% of label_fr are NA (partial French)", {
  meta <- mk_meta_bilingual()
  # Make 2 of 3 variables missing French (67%) — above the 20% default threshold
  meta$variables$label_fr[c(2, 3)] <- NA_character_

  expect_warning(
    canpumf:::check_bilingual_coverage(meta),
    regexp = "no French translation"
  )
})

test_that("check_bilingual_coverage is silent when all label_fr are NA (no French files)", {
  meta <- mk_meta_no_french()
  expect_no_warning(canpumf:::check_bilingual_coverage(meta))
})

test_that("check_bilingual_coverage is silent when coverage is <= threshold", {
  meta <- mk_meta_bilingual()
  meta$variables$label_fr[3] <- NA_character_   # 1 of 3 = 33%; above 20%... use 4-var example
  # 4 variables, 1 NA = 25% > 20% threshold → warn
  meta$variables <- rbind(
    meta$variables,
    tibble::tibble(name = "X", label_en = "Extra", label_fr = "Extra_fr",
                   type = "character", missing_low = NA_real_, missing_high = NA_real_)
  )
  meta$variables$label_fr[3] <- NA_character_   # 1 of 4 = 25%
  expect_warning(canpumf:::check_bilingual_coverage(meta, threshold = 0.20))

  # with higher threshold, no warning
  expect_no_warning(canpumf:::check_bilingual_coverage(meta, threshold = 0.30))
})


# --- select_labels: language selection -----------------------------------

test_that("select_labels(lang='eng') adds label column from label_en", {
  meta   <- mk_meta_bilingual()
  result <- canpumf:::select_labels(meta, "eng")

  expect_equal(result$variables$label, meta$variables$label_en)
  expect_equal(result$codes$label,     meta$codes$label_en)
})

test_that("select_labels(lang='fra') adds label column from label_fr", {
  meta   <- mk_meta_bilingual()
  result <- canpumf:::select_labels(meta, "fra")

  expect_equal(result$variables$label, meta$variables$label_fr)
  expect_equal(result$codes$label,     meta$codes$label_fr)
})

test_that("select_labels falls back to label_en when label_fr is NA, with warning", {
  meta <- mk_meta_bilingual()
  meta$variables$label_fr[2] <- NA_character_   # AGE has no French label

  expect_warning(
    result <- canpumf:::select_labels(meta, "fra"),
    regexp = "AGE"
  )
  expect_equal(result$variables$label[2], meta$variables$label_en[2])  # English used
  expect_equal(result$variables$label[1], meta$variables$label_fr[1])  # French used where available
})

test_that("select_labels(lang='eng') never warns even when label_fr is all NA", {
  meta <- mk_meta_no_french()
  expect_no_warning(canpumf:::select_labels(meta, "eng"))
})


# --- Separate DuckDB tables per language ---------------------------------

duck_con <- function() DBI::dbConnect(duckdb::duckdb(), ":memory:")

test_that("eng and fra tables coexist in the same DuckDB with different factor levels", {
  con <- duck_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  df_eng <- data.frame(PROV = factor(c("Newfoundland", "Quebec"),
                                     levels = c("Newfoundland", "Quebec", "Ontario")))
  df_fra <- data.frame(PROV = factor(c("Terre-Neuve", "Québec"),
                                     levels = c("Terre-Neuve", "Québec", "Ontario")))

  DBI::dbWriteTable(con, "eng", df_eng)
  DBI::dbWriteTable(con, "fra", df_fra)

  eng_back <- DBI::dbGetQuery(con, "SELECT * FROM eng")
  fra_back <- DBI::dbGetQuery(con, "SELECT * FROM fra")

  # DuckDB returns ENUM columns as R factors; compare via as.character()
  expect_false(identical(as.character(eng_back$PROV[1]), as.character(fra_back$PROV[1])))
  expect_equal(as.character(eng_back$PROV[1]), "Newfoundland")
  expect_equal(as.character(fra_back$PROV[1]), "Terre-Neuve")
})

test_that("requesting only lang='eng' does not create the 'fra' table", {
  con <- duck_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  df <- data.frame(x = factor("A", levels = c("A", "B")))
  DBI::dbWriteTable(con, "eng", df)

  expect_true(DBI::dbExistsTable(con, "eng"))
  expect_false(DBI::dbExistsTable(con, "fra"))
})

test_that("both tables created after requesting both languages", {
  con <- duck_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  DBI::dbWriteTable(con, "eng", data.frame(x = factor("A", levels = c("A", "B"))))
  DBI::dbWriteTable(con, "fra", data.frame(x = factor("A_fr", levels = c("A_fr", "B_fr"))))

  expect_true(DBI::dbExistsTable(con, "eng"))
  expect_true(DBI::dbExistsTable(con, "fra"))
})
