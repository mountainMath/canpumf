# Tests for the public registry API: pumf_registry_entry(), pumf_registry(),
# list_pumf_registry(), the override merge in pumf_registry_lookup(), and the
# get_pumf(registry=) custom-import path.

# ---- pumf_registry_entry() --------------------------------------------------

test_that("pumf_registry_entry: records only supplied fields", {
  e <- pumf_registry_entry(file_mask = "X\\.csv")
  expect_s3_class(e, "pumf_registry_entry")
  expect_equal(names(e), "file_mask")
  expect_equal(e$file_mask, "X\\.csv")
})

test_that("pumf_registry_entry: errors on unknown field", {
  expect_error(pumf_registry_entry(not_a_field = 1), regexp = "Unknown registry field")
})

test_that("pumf_registry_entry: errors on wrong field type", {
  expect_error(pumf_registry_entry(file_mask = 42), regexp = "single string")
  expect_error(pumf_registry_entry(data_fixups = "nope"), regexp = "must be a list")
})

test_that("pumf_registry_entry: warns on unrecognised data_fixups field", {
  expect_warning(
    pumf_registry_entry(data_fixups = list(force_numeric = "A", bogus = 1)),
    regexp = "Unrecognised data_fixups"
  )
})

# ---- pumf_registry() / list_pumf_registry() ---------------------------------

test_that("pumf_registry: returns full entry for a known survey", {
  e <- pumf_registry("SFS", "2019")
  expect_s3_class(e, "pumf_registry_entry")
  expect_equal(e$series, "SFS")
  expect_equal(e$data_encoding, "CP1252")
  expect_true(!is.null(e$file_mask))
})

test_that("pumf_registry: returns default entry for an unknown survey", {
  e <- pumf_registry("NOPE", "1999")
  expect_s3_class(e, "pumf_registry_entry")
  expect_equal(e$series, "NOPE")
  expect_null(e$file_mask)
  expect_equal(e$data_encoding, "CP1252")  # pipeline default
})

test_that("list_pumf_registry: tibble overview of registered surveys", {
  tb <- list_pumf_registry()
  expect_s3_class(tb, "tbl_df")
  expect_true(all(c("series", "version", "file_mask", "layout_mask",
                    "bsw_join_key", "data_fixups") %in% names(tb)))
  expect_gt(nrow(tb), 0L)
  expect_true("SFS" %in% tb$series)
})

# ---- override merge in pumf_registry_lookup() -------------------------------

test_that("pumf_registry_lookup: active override patches built-in entry", {
  base <- canpumf:::pumf_registry_lookup("SFS", "2019")

  canpumf:::.pumf_registry_override_set(
    "SFS", "2019", pumf_registry_entry(file_mask = "OVERRIDE\\.txt"))
  on.exit(canpumf:::.pumf_registry_override_clear("SFS", "2019"), add = TRUE)

  merged <- canpumf:::pumf_registry_lookup("SFS", "2019")
  expect_equal(merged$file_mask, "OVERRIDE\\.txt")   # patched
  expect_equal(merged$bsw_join_key, base$bsw_join_key) # untouched field kept
  expect_equal(merged$series, "SFS")

  canpumf:::.pumf_registry_override_clear("SFS", "2019")
  expect_equal(canpumf:::pumf_registry_lookup("SFS", "2019")$file_mask,
               base$file_mask)
})

test_that("pumf_registry_lookup: override supplies entry for unregistered survey", {
  expect_null(canpumf:::pumf_registry_lookup("ZZZ", "2025"))
  canpumf:::.pumf_registry_override_set(
    "ZZZ", "2025", pumf_registry_entry(file_mask = "Z\\.csv"))
  on.exit(canpumf:::.pumf_registry_override_clear("ZZZ", "2025"), add = TRUE)

  e <- canpumf:::pumf_registry_lookup("ZZZ", "2025")
  expect_equal(e$series, "ZZZ")
  expect_equal(e$version, "2025")
  expect_equal(e$file_mask, "Z\\.csv")
  expect_equal(e$data_encoding, "CP1252")   # default filled in
})

# ---- get_pumf(registry=) validation ----------------------------------------

test_that("get_pumf: rejects a non-entry registry and LFS registry", {
  expect_error(get_pumf("SFS", "2019", registry = list(file_mask = "x")),
               regexp = "pumf_registry_entry")
  expect_error(get_pumf("LFS", "2024",
                        registry = pumf_registry_entry(file_mask = "x")),
               regexp = "not supported for LFS")
})

# ---- end-to-end custom import ----------------------------------------------

# Build a not-in-registry survey directory with TWO ambiguous data files so a
# custom file_mask is required to select one.
.make_custom_dir <- function(tmp, series = "CUSTOM", version = "2025") {
  vdir     <- file.path(tmp, series, version)
  meta_dir <- file.path(vdir, "metadata")
  dir.create(meta_dir, recursive = TRUE)
  readr::write_csv(
    tibble::tibble(name = c("ID", "WEIGHT"),
                   label_en = c("Record ID", "Survey weight"),
                   label_fr = c("Identifiant", "Poids"),
                   type = c("numeric", "numeric"),
                   decimals = c(0L, 0L),
                   missing_low = NA_real_, missing_high = NA_real_),
    file.path(meta_dir, "variables.csv"))
  readr::write_csv(
    tibble::tibble(name = character(), val = character(),
                   label_en = character(), label_fr = character()),
    file.path(meta_dir, "codes.csv"))
  # Two candidate data files with different row counts and non-excluded names.
  readr::write_csv(tibble::tibble(ID = as.character(1:2),
                                  WEIGHT = as.character(rep(100L, 2))),
                   file.path(vdir, "aaa.csv"))
  readr::write_csv(tibble::tibble(ID = as.character(1:5),
                                  WEIGHT = as.character(rep(100L, 5))),
                   file.path(vdir, "bbb.csv"))
  vdir
}

test_that("get_pumf(registry=): custom file_mask drives import of new survey", {
  tmp <- withr::local_tempdir()
  .make_custom_dir(tmp)

  # Without a registry the two data files are ambiguous -> error.
  expect_error(
    suppressMessages(get_pumf("CUSTOM", "2025", cache_path = tmp)),
    regexp = "multiple candidate data files"
  )

  # With a custom file_mask the right file is selected and built.
  tbl <- suppressMessages(get_pumf(
    "CUSTOM", "2025", cache_path = tmp,
    registry = pumf_registry_entry(file_mask = "bbb\\.csv")))
  on.exit(try(close_pumf(tbl), silent = TRUE), add = TRUE)

  expect_equal(dplyr::collect(dplyr::count(tbl))$n, 5L)  # bbb.csv has 5 rows
})

test_that("get_pumf(registry=): cached build reused unless refresh=TRUE", {
  tmp <- withr::local_tempdir()
  .make_custom_dir(tmp)

  tbl1 <- suppressMessages(get_pumf(
    "CUSTOM", "2025", cache_path = tmp,
    registry = pumf_registry_entry(file_mask = "bbb\\.csv")))
  close_pumf(tbl1)

  # Re-running with a different registry but no refresh: cached build reused,
  # informative message emitted, row count unchanged (5).
  expect_message(
    tbl2 <- get_pumf("CUSTOM", "2025", cache_path = tmp,
                     registry = pumf_registry_entry(file_mask = "aaa\\.csv")),
    regexp = "already exists"
  )
  expect_equal(dplyr::collect(dplyr::count(tbl2))$n, 5L)
  close_pumf(tbl2)

  # When a build actually happens, the override is applied: drop the DuckDB so
  # the next call rebuilds (metadata kept), and supply a different file_mask.
  # (refresh = TRUE would also rebuild but additionally re-parses metadata,
  # which this metadata-only fixture cannot regenerate.)
  unlink(list.files(file.path(tmp, "CUSTOM", "2025"),
                    pattern = "\\.duckdb", full.names = TRUE))
  tbl3 <- suppressMessages(get_pumf(
    "CUSTOM", "2025", cache_path = tmp,
    registry = pumf_registry_entry(file_mask = "aaa\\.csv")))
  on.exit(try(close_pumf(tbl3), silent = TRUE), add = TRUE)
  expect_equal(dplyr::collect(dplyr::count(tbl3))$n, 2L)  # aaa.csv has 2 rows
})

test_that("pumf_metadata(registry=): 'already parsed' message; never from get_pumf", {
  tmp <- withr::local_tempdir()
  .make_custom_dir(tmp)  # writes metadata/variables.csv + two data files

  # registry supplied + metadata present + no refresh -> message
  expect_message(
    m <- pumf_metadata("CUSTOM", "2025", cache_path = tmp,
                       registry = pumf_registry_entry(metadata_encoding = "UTF-8")),
    regexp = "already parsed"
  )
  expect_true(all(c("variables", "codes") %in% names(m)))

  # without a registry the message is not emitted
  msgs <- testthat::capture_messages(
    pumf_metadata("CUSTOM", "2025", cache_path = tmp))
  expect_false(any(grepl("already parsed", msgs)))

  # get_pumf() parses via a different path and never emits the metadata message
  msgs2 <- testthat::capture_messages(
    tbl <- get_pumf("CUSTOM", "2025", cache_path = tmp,
                    registry = pumf_registry_entry(file_mask = "bbb\\.csv")))
  on.exit(try(close_pumf(tbl), silent = TRUE), add = TRUE)
  expect_false(any(grepl("already parsed", msgs2)))
})
