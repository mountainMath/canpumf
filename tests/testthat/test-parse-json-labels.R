# parse_json_value_labels(): the JSON value-label dictionary of The Canadian
# Peoples census files (TCP 1881), and its place in detect_formats() and
# pumf_parse_metadata().

.write_json_fixture <- function(dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  json <- file.path(dir, "1881_value_labels.json")
  writeLines(c(
    '{',
    '  "sex_TCP": {"1": "Male", "2": "Female", "9": "Unknown"},',
    '  "dprov": {"35": "Ontario", "24": "Quebec", " 13 ": "New Brunswick"},',
    '  "empty_label": {"1": "", "2": "Two"},',
    '  "note": "not a dictionary"',
    '}'), json)
  csv <- file.path(dir, "1881_v20251217.csv")
  writeLines(c("serial,sex_TCP,dprov,age,namlast",
               "1,1,35,34,Smith", "2,2,24,7,Tremblay"), csv)
  list(json = json, csv = csv)
}

test_that("parse_json_value_labels: codes from the JSON, variables from the CSV header", {
  f   <- .write_json_fixture(withr::local_tempdir())
  out <- canpumf:::parse_json_value_labels(f$json, data_path = f$csv)
  expect_named(out, c("variables", "codes", "layout"))
  expect_null(out$layout)

  # the header first, then coded variables the header does not have
  expect_equal(out$variables$name,
               c("SERIAL", "SEX_TCP", "DPROV", "AGE", "NAMLAST", "EMPTY_LABEL"))
  expect_true(all(out$variables$type == "character"))
  expect_true(all(is.na(out$variables$label_en)))
  expect_true(all(is.na(out$variables$label_fr)))

  codes <- out$codes
  expect_named(codes, c("name", "val", "label_en", "label_fr"))
  expect_setequal(unique(codes$name), c("SEX_TCP", "DPROV", "EMPTY_LABEL"))
  expect_equal(codes$label_en[codes$name == "SEX_TCP"], c("Male", "Female", "Unknown"))
  # code strings are trimmed; an empty label is NA; there is no French
  expect_equal(codes$val[codes$name == "DPROV"], c("35", "24", "13"))
  expect_equal(codes$label_en[codes$name == "EMPTY_LABEL"], c(NA, "Two"))
  expect_true(all(is.na(codes$label_fr)))
})

test_that("parse_json_value_labels: without a data file only the coded variables", {
  f   <- .write_json_fixture(withr::local_tempdir())
  out <- canpumf:::parse_json_value_labels(f$json)
  expect_equal(out$variables$name, c("SEX_TCP", "DPROV", "EMPTY_LABEL"))
})

test_that("parse_json_value_labels: a JSON without dictionaries gives empty codes", {
  tmp  <- withr::local_tempdir()
  json <- file.path(tmp, "x_value_labels.json")
  writeLines('{"a": "b"}', json)
  out <- canpumf:::parse_json_value_labels(json)
  expect_equal(nrow(out$codes), 0L)
  expect_named(out$codes, c("name", "val", "label_en", "label_fr"))
})

test_that("detect_formats: finds the JSON dictionary", {
  tmp <- withr::local_tempdir()
  f   <- .write_json_fixture(tmp)
  fmt <- canpumf:::detect_formats(tmp)
  expect_equal(normalizePath(fmt$json_labels), normalizePath(f$json))
})

test_that("pumf_parse_metadata: a JSON-only release yields the canonical CSVs", {
  tmp <- withr::local_tempdir()
  .write_json_fixture(tmp)
  suppressMessages(canpumf:::pumf_parse_metadata(tmp))
  meta <- canpumf:::read_metadata(file.path(tmp, "metadata"))
  expect_equal(meta$variables$name,
               c("SERIAL", "SEX_TCP", "DPROV", "AGE", "NAMLAST", "EMPTY_LABEL"))
  expect_equal(sum(meta$codes$name == "SEX_TCP"), 3L)
  expect_null(meta$layout)
})
