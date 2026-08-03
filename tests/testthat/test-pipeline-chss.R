# Integration tests for the Canadian Health Survey on Seniors (CHSS) pipeline.
# Tests run against data already in the user's canpumf cache.
#
# CHSS has a single version ("2019-2020").  Two things make it distinctive:
#   * StatCan ships three format bundles and only the TXT/SAS ones carry the
#     command files, so the registry pins download_format = "TXT" (the CSV zip
#     holds the data alone).
#   * The 1000 bootstrap weights live in a separate fixed-width flat file whose
#     layout is declared only by a SAS INPUT card using the array shorthand
#     "@28 (BSW1-BSW1000) (1000* 7.2)", with the decimal point implied.

.chss_vdir <- function() {
  file.path(getOption("canpumf.cache_path", ""), "CHSS", "2019-2020")
}

.chss_build <- function(lang, db_path) {
  canpumf:::pumf_build_duckdb(.chss_vdir(), "CHSS", "2019-2020",
                               lang        = lang,
                               layout_mask = "PUMF_MASTER_chss",
                               db_path     = db_path,
                               refresh     = TRUE)
}

test_that("CHSS 2019-2020: full pipeline emits no warnings", {
  skip_if_not(canpumf:::.version_is_extracted(.chss_vdir()),
              "CHSS 2019-2020 not extracted in cache")

  reg   <- canpumf:::pumf_registry_lookup("CHSS", "2019-2020")
  tmp   <- tempfile(fileext = ".duckdb")
  con   <- NULL
  warns <- character(0L)

  withCallingHandlers(
    {
      canpumf:::pumf_parse_metadata(.chss_vdir(),
                                     layout_mask       = reg$layout_mask,
                                     metadata_encoding = reg$metadata_encoding,
                                     refresh           = TRUE)
      r   <- .chss_build("eng", tmp)
      tbl <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
      con <<- tbl$src$con
      dplyr::collect(dplyr::count(tbl))
    },
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  if (!is.null(con)) DBI::dbDisconnect(con, shutdown = TRUE)
  unlink(tmp)

  expect_identical(warns, character(0L),
    label = "CHSS 2019-2020: should have no warnings")
})

test_that("CHSS 2019-2020: registry pins the TXT bundle", {
  reg <- canpumf:::pumf_registry_lookup("CHSS", "2019-2020")
  expect_identical(reg$download_format, "TXT")

  # The catalogue offers CSV/SAS/TXT for this edition; the override must leave
  # exactly the TXT row so the CSV bundle (data only, no command files) can
  # never win the format de-dup.
  cat <- data.frame(
    Acronym = "CHSS", Version = "2019-2020",
    format  = c("CSV", "SAS", "TXT"),
    stringsAsFactors = FALSE)
  expect_identical(canpumf:::.statcan_apply_format_override(cat)$format, "TXT")
})

test_that("CHSS 2019-2020: bootstrap weights are joined at the right scale", {
  skip_if_not(canpumf:::.version_is_extracted(.chss_vdir()),
              "CHSS 2019-2020 not extracted in cache")
  skip_if_not(file.exists(file.path(.chss_vdir(), "metadata", "variables.csv")),
              "CHSS 2019-2020 metadata not parsed")

  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)
  r   <- suppressWarnings(.chss_build("eng", tmp))

  tbl <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE), add = TRUE)

  cols <- colnames(tbl)
  expect_length(grep("^BSW\\d+$", cols), 1000L)
  expect_true("WTS_CM" %in% cols,
    label = "CHSS: main weight column present")
  expect_false("FWGT" %in% cols,
    label = "CHSS: BSW-side duplicate weight dropped")

  d <- dplyr::collect(dplyr::summarise(
    tbl,
    n      = dplyr::n(),
    na_bsw = sum(as.integer(is.na(BSW1))),
    wts    = sum(WTS_CM,  na.rm = TRUE),
    bsw1   = sum(BSW1,    na.rm = TRUE),
    bsw999 = sum(BSW1000, na.rm = TRUE)))

  expect_equal(d$n, 41550)
  expect_equal(d$na_bsw, 0,
    label = "CHSS: BSW join should match every record")
  # 6,435,765 is the weighted total the PUMF data dictionary prints on every
  # frequency table.  The BSW flat file stores weights with the decimal point
  # implied by its "7.2" informat, so a missing implied-decimal correction would
  # inflate each replicate total 100-fold.
  expect_equal(d$wts, 6435765, tolerance = 1e-4)
  expect_equal(d$bsw1,   d$wts, tolerance = 1e-3)
  expect_equal(d$bsw999, d$wts, tolerance = 1e-3)
})

test_that("CHSS 2019-2020: ALWDVWKY is continuous, not categorical", {
  skip_if_not(canpumf:::.version_is_extracted(.chss_vdir()),
              "CHSS 2019-2020 not extracted in cache")
  skip_if_not(file.exists(file.path(.chss_vdir(), "metadata", "variables.csv")),
              "CHSS 2019-2020 metadata not parsed")

  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)
  r   <- suppressWarnings(.chss_build("eng", tmp))

  tbl <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE), add = TRUE)

  d <- dplyr::collect(dplyr::summarise(
    tbl,
    lo = min(ALWDVWKY, na.rm = TRUE),
    hi = max(ALWDVWKY, na.rm = TRUE)))

  # Data dictionary: 000 = "Has not had a drink in past week", 001-084 =
  # "Number of drinks consumed in past week", 996/999 reserved (-> NA).
  expect_equal(d$lo, 0)
  expect_equal(d$hi, 84)
})

test_that("CHSS 2019-2020: metadata has English and French labels", {
  vdir <- .chss_vdir()
  skip_if_not(file.exists(file.path(vdir, "metadata", "variables.csv")),
              "CHSS 2019-2020 metadata not parsed")

  meta <- canpumf:::read_metadata(file.path(vdir, "metadata"))
  expect_gt(sum(!is.na(meta$variables$label_en)), 0L,
    label = "CHSS: should have English variable labels")
  expect_gt(sum(!is.na(meta$variables$label_fr)), 0L,
    label = "CHSS: should have French variable labels")
})

test_that("CHSS 2019-2020: eng/fra bilingual parity", {
  skip_if_not(canpumf:::.version_is_extracted(.chss_vdir()),
              "CHSS 2019-2020 not extracted in cache")
  skip_if_not(file.exists(file.path(.chss_vdir(), "metadata", "variables.csv")),
              "CHSS 2019-2020 metadata not parsed")

  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  r_eng <- suppressWarnings(.chss_build("eng", tmp))
  r_fra <- suppressWarnings(.chss_build("fra", tmp))

  # The 1000 replicate weights are numeric by construction and carry no labels;
  # dropping them keeps two collected copies of this 1727-column survey from
  # costing a gigabyte of memory.
  drop_bsw <- function(d) d[, !grepl("^BSW\\d+$", names(d)), drop = FALSE]
  eng <- drop_bsw(.collect_pumf_table(tmp, r_eng$table_name))
  fra <- drop_bsw(.collect_pumf_table(tmp, r_fra$table_name))

  expect_pumf_bilingual_parity(eng, fra, label = "CHSS 2019-2020")
})
