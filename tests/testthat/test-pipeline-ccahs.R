# Integration tests for the Canadian COVID-19 Antibody and Health Survey
# (CCAHS) pipeline.  Tests run against data already in the user's canpumf cache.
#
# CCAHS has a single version ("1"): split-SPSS metadata, CSV data file, and a
# bootstrap-weight CSV joined on PUMFID (WGT_PUMF dropped from the BSW side).

.ccahs_vdir <- function() {
  file.path(getOption("canpumf.cache_path", ""), "CCAHS", "1")
}

test_that("CCAHS 1: full pipeline emits no warnings", {
  skip_if_not(canpumf:::.version_is_extracted(.ccahs_vdir()),
              "CCAHS 1 not extracted in cache")

  reg  <- canpumf:::pumf_registry_lookup("CCAHS", "1")
  tmp  <- tempfile(fileext = ".duckdb")
  con  <- NULL
  warns <- character(0L)

  withCallingHandlers(
    {
      canpumf:::pumf_parse_metadata(.ccahs_vdir(),
                                     metadata_encoding = reg$metadata_encoding,
                                     refresh           = TRUE)
      r   <- canpumf:::pumf_build_duckdb(.ccahs_vdir(), "CCAHS", "1",
                                          lang    = "eng",
                                          db_path = tmp,
                                          refresh = TRUE)
      tbl <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
      con <<- tbl$src$con
      dplyr::collect(tbl)
    },
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  if (!is.null(con)) DBI::dbDisconnect(con, shutdown = TRUE)
  unlink(tmp)

  expect_identical(warns, character(0L),
    label = "CCAHS 1: should have no warnings")
})

test_that("CCAHS 1: bootstrap weights are joined", {
  skip_if_not(canpumf:::.version_is_extracted(.ccahs_vdir()),
              "CCAHS 1 not extracted in cache")
  skip_if_not(file.exists(file.path(.ccahs_vdir(), "metadata", "variables.csv")),
              "CCAHS 1 metadata not parsed")

  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)
  r <- suppressWarnings(
    canpumf:::pumf_build_duckdb(.ccahs_vdir(), "CCAHS", "1",
                                 lang = "eng", db_path = tmp, refresh = TRUE))
  d <- .collect_pumf_table(tmp, r$table_name)

  bsw_cols <- grep("^BSW", names(d), value = TRUE)
  expect_gt(length(bsw_cols), 0L,
    label = "CCAHS 1: bootstrap weight columns should be joined")
  expect_true("WGT_PUMF" %in% names(d),
    label = "CCAHS 1: main weight column present")
  expect_false(anyNA(d[[bsw_cols[1L]]]),
    label = "CCAHS 1: BSW join should match every record")
})

test_that("CCAHS 1: metadata has English and French labels", {
  vdir <- .ccahs_vdir()
  skip_if_not(file.exists(file.path(vdir, "metadata", "variables.csv")),
              "CCAHS 1 metadata not parsed")

  meta <- canpumf:::read_metadata(file.path(vdir, "metadata"))
  expect_gt(sum(!is.na(meta$variables$label_en)), 0L,
    label = "CCAHS 1: should have English variable labels")
  expect_gt(sum(!is.na(meta$variables$label_fr)), 0L,
    label = "CCAHS 1: should have French variable labels")
})

test_that("CCAHS 1: eng/fra bilingual parity", {
  skip_if_not(canpumf:::.version_is_extracted(.ccahs_vdir()),
              "CCAHS 1 not extracted in cache")
  skip_if_not(file.exists(file.path(.ccahs_vdir(), "metadata", "variables.csv")),
              "CCAHS 1 metadata not parsed")

  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  r_eng <- suppressWarnings(
    canpumf:::pumf_build_duckdb(.ccahs_vdir(), "CCAHS", "1",
                                 lang = "eng", db_path = tmp, refresh = TRUE))
  r_fra <- suppressWarnings(
    canpumf:::pumf_build_duckdb(.ccahs_vdir(), "CCAHS", "1",
                                 lang = "fra", db_path = tmp, refresh = TRUE))

  eng <- .collect_pumf_table(tmp, r_eng$table_name)
  fra <- .collect_pumf_table(tmp, r_fra$table_name)

  expect_pumf_bilingual_parity(eng, fra, label = "CCAHS 1")
})
