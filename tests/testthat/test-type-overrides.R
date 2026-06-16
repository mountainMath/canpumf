# Tests for the force_character / force_integer / force_bigint storage-type
# overrides, and the LFS integer-forcing now sourced from the registry.

# Build a not-in-registry survey with columns that exercise each override.
# All are declared numeric in variables.csv so the overrides are what change
# their storage type.
.make_types_dir <- function(tmp, smallint_vals = c("5", "10", "15")) {
  vdir <- file.path(tmp, "CUST", "2025"); meta <- file.path(vdir, "metadata")
  dir.create(meta, recursive = TRUE)
  readr::write_csv(tibble::tibble(
    name = c("GEO", "BIGID", "SMALLINT", "WEIGHT"),
    label_en = NA_character_, label_fr = NA_character_,
    type = "numeric", decimals = 0L,
    missing_low = NA_real_, missing_high = NA_real_),
    file.path(meta, "variables.csv"))
  readr::write_csv(tibble::tibble(name = character(), val = character(),
    label_en = character(), label_fr = character()),
    file.path(meta, "codes.csv"))
  readr::write_csv(tibble::tibble(
    GEO      = c("001", "002", "001"),
    BIGID    = c("3000000000", "3000000001", "3000000002"),  # > 2^31
    SMALLINT = smallint_vals,
    WEIGHT   = c("100", "100", "100")),
    file.path(vdir, "data.csv"))
  vdir
}

.col_types <- function(con, table = "eng") {
  info <- DBI::dbGetQuery(con, sprintf("PRAGMA table_info('%s')", table))
  stats::setNames(info$type, info$name)
}

test_that("force_* overrides set DuckDB column types and preserve values", {
  tmp <- withr::local_tempdir()
  .make_types_dir(tmp)

  tbl <- suppressMessages(get_pumf("CUST", "2025", cache_path = tmp,
    registry = pumf_registry_entry(data_fixups = list(
      force_character = "GEO",
      force_bigint    = "BIGID",
      force_integer   = "SMALLINT"))))
  on.exit(try(close_pumf(tbl), silent = TRUE), add = TRUE)

  types <- .col_types(tbl$src$con)
  expect_equal(unname(types[["GEO"]]),      "VARCHAR")
  expect_equal(unname(types[["BIGID"]]),    "BIGINT")
  expect_equal(unname(types[["SMALLINT"]]), "INTEGER")
  expect_equal(unname(types[["WEIGHT"]]),   "DOUBLE")   # numeric default

  df <- dplyr::collect(tbl)
  expect_setequal(df$GEO, c("001", "002"))              # leading zeros kept
  expect_true(3000000002 %in% df$BIGID)                 # > 2^31 preserved
})

test_that("force_integer overflow surfaces an informative error", {
  tmp <- withr::local_tempdir()
  # A value beyond the 32-bit range in a force_integer column.
  .make_types_dir(tmp, smallint_vals = c("5", "3000000000", "15"))

  expect_error(
    suppressMessages(get_pumf("CUST", "2025", cache_path = tmp,
      registry = pumf_registry_entry(data_fixups = list(
        force_integer = "SMALLINT")))),
    regexp = "force_integer|INTEGER"
  )
})

test_that("get_pumf rejects a variable in two force_* sets", {
  tmp <- withr::local_tempdir()
  .make_types_dir(tmp)
  expect_error(
    suppressMessages(get_pumf("CUST", "2025", cache_path = tmp,
      registry = pumf_registry_entry(data_fixups = list(
        force_integer = "SMALLINT", force_character = "SMALLINT")))),
    regexp = "more than one force"
  )
})

# ---- LFS integer-forcing sourced from the registry --------------------------

test_that("LFS registry entry carries the integer-forced columns", {
  e <- canpumf:::pumf_registry_lookup("LFS", NA_character_)
  expect_equal(e$data_fixups$force_integer,
               c("SURVYEAR", "SURVMNTH", "REC_NUM"))
  # discoverable through the public accessor for any LFS version
  expect_equal(pumf_registry("LFS", "2024")$data_fixups$force_integer,
               c("SURVYEAR", "SURVMNTH", "REC_NUM"))
})
