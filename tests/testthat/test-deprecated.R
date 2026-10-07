# The 0.6.0 exports folded into other functions in 0.7.0 (R/deprecated.R):
# each warns and returns what its replacement returns.  Offline; the shims
# that need a network source are checked against a mocked replacement.

test_that("deprecated shims warn and forward to their replacement", {
  expect_warning(reg <- list_pumf_registry(), "pumf_registry")
  expect_equal(reg, pumf_registry())

  df <- tibble::tibble(SEX = c("Male", "Female"))
  expect_warning(out <- add_lfs_GENDER_SEX(df), "add_lfs_columns")
  expect_equal(out, add_lfs_columns(df, "GENDER_SEX"))
  expect_warning(add_lfs_SURVDATE(tibble::tibble(X = 1L)), "add_lfs_columns") |>
    expect_error("SURVYEAR")
})

test_that("deprecated catalogue functions call list_pumf_catalogue()", {
  seen <- character()
  local_mocked_bindings(list_pumf_catalogue = function(source = "canpumf", ...) {
    seen <<- c(seen, source)
    tibble::tibble(source = source)
  })
  expect_warning(list_canpumf_collection(), "deprecated")
  expect_warning(list_statcan_pumf_catalogue(max_surveys = 1), "deprecated")
  expect_warning(list_borealis_pumf_catalogue(verbose = FALSE), "deprecated")
  expect_warning(list_available_lfs_pumf_versions(), "deprecated")
  expect_equal(seen, c("canpumf", "statcan", "borealis", "lfs"))
})

test_that("pumf_var_labels and the PDF reports forward to their replacement", {
  tmp <- withr::local_tempdir()
  make_e2e_version_dir(tmp)
  tbl <- suppressMessages(get_pumf("FAKE", "2099", cache_path = tmp))
  on.exit(close_pumf(tbl))

  expect_warning(vl <- pumf_var_labels(tbl), "what = \"variables\"")
  expect_named(vl, c("name", "label_en", "label_fr", "description_en",
                     "description_fr"))
  expect_equal(vl$name, pumf_dictionary(tbl, what = "variables")$name)

  expect_warning(r <- pumf_label_repairs(tbl), "pumf_pdf_crosscheck")
  expect_equal(r, pumf_pdf_crosscheck(tbl))
  expect_warning(v <- pumf_freq_validation(tbl), "pumf_pdf_crosscheck")
  expect_equal(v, pumf_pdf_crosscheck(tbl, "validation"))
})

test_that("list_pumf_catalogue: extra arguments only for the statcan source", {
  expect_error(list_pumf_catalogue("lfs", max_surveys = 1), "statcan")
  expect_error(list_pumf_catalogue("nope"), "should be one of")
})
