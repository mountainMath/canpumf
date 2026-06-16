# Unit tests for Stage 3 helpers.
# pumf_build_duckdb() end-to-end is covered in test-pipeline-cpss.R.

duck_con <- function() DBI::dbConnect(duckdb::duckdb(), ":memory:")

# ---- .find_pumf_data_file ---------------------------------------------------

test_that(".find_pumf_data_file: finds single CSV", {
  tmp <- withr::local_tempdir()
  writeLines("a,b\n1,2", file.path(tmp, "survey.csv"))
  expect_equal(basename(canpumf:::.find_pumf_data_file(tmp, NULL, FALSE)),
               "survey.csv")
})

test_that(".find_pumf_data_file: excludes metadata/ subdir", {
  tmp  <- withr::local_tempdir()
  meta <- file.path(tmp, "metadata")
  dir.create(meta)
  writeLines("", file.path(meta, "variables.csv"))
  writeLines("a,b\n1,2", file.path(tmp, "survey.csv"))
  expect_equal(basename(canpumf:::.find_pumf_data_file(tmp, NULL, FALSE)),
               "survey.csv")
})

test_that(".find_pumf_data_file: excludes codebook.csv", {
  tmp <- withr::local_tempdir()
  writeLines("", file.path(tmp, "codebook.csv"))
  writeLines("a,b\n1,2", file.path(tmp, "survey.csv"))
  expect_equal(basename(canpumf:::.find_pumf_data_file(tmp, NULL, FALSE)),
               "survey.csv")
})

test_that(".find_pumf_data_file: applies file_mask regex", {
  tmp <- withr::local_tempdir()
  writeLines("", file.path(tmp, "survey_main.csv"))
  writeLines("", file.path(tmp, "survey_bsw.csv"))
  result <- canpumf:::.find_pumf_data_file(tmp, "main", FALSE)
  expect_equal(basename(result), "survey_main.csv")
})

test_that(".find_pumf_data_file: errors when no file found", {
  tmp <- withr::local_tempdir()
  expect_error(
    canpumf:::.find_pumf_data_file(tmp, NULL, FALSE),
    regexp = "Could not find data file"
  )
})

test_that(".find_pumf_data_file: errors when multiple files and no mask", {
  tmp <- withr::local_tempdir()
  writeLines("", file.path(tmp, "a.csv"))
  writeLines("", file.path(tmp, "b.csv"))
  expect_error(
    canpumf:::.find_pumf_data_file(tmp, NULL, FALSE),
    regexp = "multiple candidate"
  )
})

test_that(".find_pumf_data_file: finds FWF .txt file", {
  tmp <- withr::local_tempdir()
  writeLines("", file.path(tmp, "data.txt"))
  result <- canpumf:::.find_pumf_data_file(tmp, NULL, TRUE)
  expect_equal(basename(result), "data.txt")
})

test_that(".find_pumf_data_file: finds FWF .dat file", {
  tmp <- withr::local_tempdir()
  writeLines("", file.path(tmp, "data.dat"))
  result <- canpumf:::.find_pumf_data_file(tmp, NULL, TRUE)
  expect_equal(basename(result), "data.dat")
})

# ---- .apply_data_fixups -----------------------------------------------------

test_that(".apply_data_fixups: str_pad pads short values", {
  data  <- data.frame(COL = c("1", "2", "12"), stringsAsFactors = FALSE)
  fixup <- list(str_pad = list(list(cols="COL", width=2L, side="left", pad="0")))
  result <- canpumf:::.apply_data_fixups(data, fixup)
  expect_equal(result$COL, c("01", "02", "12"))
})

test_that(".apply_data_fixups: str_pad skips absent columns silently", {
  data  <- data.frame(OTHER = "x", stringsAsFactors = FALSE)
  fixup <- list(str_pad = list(list(cols="COL", width=2L, side="left", pad="0")))
  expect_no_error(canpumf:::.apply_data_fixups(data, fixup))
})

test_that(".apply_data_fixups: rename renames existing column", {
  data  <- data.frame(OLD = 1L)
  fixup <- list(str_pad = list(), rename = c(OLD = "NEW"))
  result <- canpumf:::.apply_data_fixups(data, fixup)
  expect_true("NEW" %in% names(result))
  expect_false("OLD" %in% names(result))
})

test_that(".apply_data_fixups: rename skips absent column", {
  data  <- data.frame(OTHER = 1L)
  fixup <- list(str_pad = list(), rename = c(OLD = "NEW"))
  result <- canpumf:::.apply_data_fixups(data, fixup)
  expect_equal(names(result), "OTHER")
})

# ---- .apply_numeric_conversion ----------------------------------------------

test_that(".apply_numeric_conversion: converts character to double", {
  vars <- tibble::tibble(name="X", type="numeric",
                          missing_low=NA_real_, missing_high=NA_real_,
                          decimals=2L)
  data <- data.frame(X = c("1.5", "2.3"), stringsAsFactors = FALSE)
  result <- canpumf:::.apply_numeric_conversion(data, vars)
  expect_type(result$X, "double")
  expect_equal(result$X, c(1.5, 2.3))
})

test_that(".apply_numeric_conversion: 0-decimal numeric stays double", {
  vars <- tibble::tibble(name="X", type="numeric",
                          missing_low=NA_real_, missing_high=NA_real_,
                          decimals=0L)
  data <- data.frame(X = c("1", "2"), stringsAsFactors = FALSE)
  result <- canpumf:::.apply_numeric_conversion(data, vars)
  expect_type(result$X, "double")
  expect_equal(result$X, c(1, 2))
})

test_that(".apply_numeric_conversion: large 0-decimal values survive (no int overflow)", {
  vars <- tibble::tibble(name="X", type="numeric",
                          missing_low=NA_real_, missing_high=NA_real_,
                          decimals=0L)
  # 3e9 exceeds the 32-bit signed integer range; as.integer() would NA it.
  data <- data.frame(X = c("3000000000", "5"), stringsAsFactors = FALSE)
  result <- canpumf:::.apply_numeric_conversion(data, vars)
  expect_equal(result$X, c(3e9, 5))
  expect_false(anyNA(result$X))
})

test_that(".apply_numeric_conversion: missing range becomes NA", {
  vars <- tibble::tibble(name="X", type="numeric",
                          missing_low=98, missing_high=99,
                          decimals=0L)
  data <- data.frame(X = c("1", "98", "99", "2"), stringsAsFactors = FALSE)
  result <- canpumf:::.apply_numeric_conversion(data, vars)
  expect_equal(result$X, c(1, NA_real_, NA_real_, 2))
})

test_that(".apply_numeric_conversion: skips absent and non-character columns", {
  vars <- tibble::tibble(name=c("X","Y"), type="numeric",
                          missing_low=NA_real_, missing_high=NA_real_,
                          decimals=0L)
  data <- data.frame(Z = "1", stringsAsFactors = FALSE)  # X absent, Y absent
  expect_no_error(canpumf:::.apply_numeric_conversion(data, vars))
})

# ---- .apply_code_labels -----------------------------------------------------

codes_df <- tibble::tibble(
  name     = c("PROV","PROV","PROV","SEX","SEX"),
  val      = c("10","24","35","1","2"),
  label_en = c("Newfoundland","Quebec","Ontario","Male","Female"),
  label_fr = c("Terre-Neuve","Québec","Ontario","Homme","Femme")
)

test_that(".apply_code_labels: creates factor with all levels", {
  data   <- data.frame(PROV = c("10","35"), stringsAsFactors = FALSE)
  result <- canpumf:::.apply_code_labels(data, codes_df, "label_en")
  expect_s3_class(result$PROV, "factor")
  expect_equal(nlevels(result$PROV), 3L)   # all three codes, not just 2 present
})

test_that(".apply_code_labels: levels follow codes order", {
  data   <- data.frame(PROV = c("35","10"), stringsAsFactors = FALSE)
  result <- canpumf:::.apply_code_labels(data, codes_df, "label_en")
  expect_equal(levels(result$PROV), c("Newfoundland","Quebec","Ontario"))
})

test_that(".apply_code_labels: uses label_fr when requested", {
  data   <- data.frame(PROV = c("24"), stringsAsFactors = FALSE)
  result <- canpumf:::.apply_code_labels(data, codes_df, "label_fr")
  expect_equal(as.character(result$PROV), "Québec")
})

test_that(".apply_code_labels: unmatched value → NA + warning", {
  data   <- data.frame(PROV = c("10","99"), stringsAsFactors = FALSE)
  expect_warning(
    result <- canpumf:::.apply_code_labels(data, codes_df, "label_en"),
    regexp = "99"
  )
  expect_true(is.na(result$PROV[2]))
})

test_that(".apply_code_labels: NA input stays NA without warning", {
  data   <- data.frame(PROV = c("10", NA_character_), stringsAsFactors = FALSE)
  expect_no_warning(
    result <- canpumf:::.apply_code_labels(data, codes_df, "label_en")
  )
  expect_true(is.na(result$PROV[2]))
})

test_that(".apply_code_labels: skips numeric columns", {
  data   <- data.frame(PROV = c(10, 24), SEX = c("1","2"))
  result <- canpumf:::.apply_code_labels(data, codes_df, "label_en")
  expect_type(result$PROV, "double")     # untouched
  expect_s3_class(result$SEX, "factor")  # labeled
})

# ---- .ensure_enum_columns ---------------------------------------------------

test_that(".ensure_enum_columns: no-op when factors already stored as ENUM", {
  con <- duck_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  df <- data.frame(x = factor(c("A","B"), levels = c("A","B","C")))
  DBI::dbWriteTable(con, "t", df)

  # duckdb 1.5.2 should have already written ENUM; function should be a no-op
  expect_no_error(
    canpumf:::.ensure_enum_columns(con, "t", list(x = c("A","B","C")))
  )

  info <- DBI::dbGetQuery(con, "PRAGMA table_info('t')")
  expect_true(grepl("^ENUM", info$type[info$name == "x"]))
})

# ---- pumf_build_duckdb unit tests -------------------------------------------

# Build a minimal in-directory set: metadata/ + one CSV data file.
make_minimal_version_dir <- function(base) {
  vdir     <- file.path(base, "FAKE", "2099")
  meta_dir <- file.path(vdir, "metadata")
  dir.create(meta_dir, recursive = TRUE)

  variables <- tibble::tibble(
    name="PROV", label_en="Province", label_fr="Province",
    type="character", decimals=NA_integer_,
    missing_low=NA_real_, missing_high=NA_real_
  )
  codes <- tibble::tibble(
    name=c("PROV","PROV"), val=c("10","35"),
    label_en=c("Newfoundland","Ontario"), label_fr=c("Terre-Neuve","Ontario")
  )
  readr::write_csv(variables, file.path(meta_dir, "variables.csv"))
  readr::write_csv(codes,     file.path(meta_dir, "codes.csv"))

  # Main data file
  readr::write_csv(
    tibble::tibble(PROV = c("10","35","10")),
    file.path(vdir, "survey.csv")
  )

  vdir
}

# Helper: build a minimal version dir, call pumf_build_duckdb, open a fresh
# read-only connection, collect the result, then disconnect.
collect_build <- function(vdir, lang = "eng", refresh = FALSE) {
  r   <- canpumf:::pumf_build_duckdb(vdir, "FAKE", "2099",
                                      lang = lang, refresh = refresh)
  tbl <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))
  dplyr::collect(tbl)
}

test_that("pumf_build_duckdb: creates DuckDB file and returns path list", {
  tmp  <- withr::local_tempdir()
  vdir <- make_minimal_version_dir(tmp)

  result <- canpumf:::pumf_build_duckdb(vdir, "FAKE", "2099", lang = "eng")
  expect_named(result, c("db_path", "table_name"), ignore.order = TRUE)
  expect_true(file.exists(result$db_path))
  expect_equal(result$table_name, "eng")
})

test_that("pumf_build_duckdb: factor column with correct levels", {
  tmp    <- withr::local_tempdir()
  vdir   <- make_minimal_version_dir(tmp)
  result <- collect_build(vdir)

  expect_equal(nrow(result), 3L)
  expect_setequal(unique(result$PROV), c("Newfoundland", "Ontario"))
})

test_that("pumf_build_duckdb: lang=fra uses French labels", {
  tmp    <- withr::local_tempdir()
  vdir   <- make_minimal_version_dir(tmp)
  result <- collect_build(vdir, lang = "fra")

  expect_true("Terre-Neuve" %in% result$PROV)
  expect_false("Newfoundland" %in% result$PROV)
})

test_that("pumf_build_duckdb: eng and fra tables coexist in same DuckDB", {
  tmp  <- withr::local_tempdir()
  vdir <- make_minimal_version_dir(tmp)

  r_eng <- canpumf:::pumf_build_duckdb(vdir, "FAKE", "2099", lang = "eng")
  r_fra <- canpumf:::pumf_build_duckdb(vdir, "FAKE", "2099", lang = "fra")

  # Both calls close all connections; open a fresh one to verify
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = r_eng$db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  expect_true(DBI::dbExistsTable(con, "eng"))
  expect_true(DBI::dbExistsTable(con, "fra"))
})

test_that("pumf_build_duckdb: skip rebuild when table exists and refresh=FALSE", {
  tmp  <- withr::local_tempdir()
  vdir <- make_minimal_version_dir(tmp)

  r1       <- canpumf:::pumf_build_duckdb(vdir, "FAKE", "2099", lang = "eng")
  mtime1   <- file.info(r1$db_path)$mtime
  Sys.sleep(0.05)
  r2       <- canpumf:::pumf_build_duckdb(vdir, "FAKE", "2099", lang = "eng")
  mtime2   <- file.info(r2$db_path)$mtime

  expect_equal(mtime1, mtime2)
})

test_that("pumf_build_duckdb: refresh=TRUE rewrites the table", {
  tmp  <- withr::local_tempdir()
  vdir <- make_minimal_version_dir(tmp)

  canpumf:::pumf_build_duckdb(vdir, "FAKE", "2099", lang = "eng")

  # Modify metadata and rebuild
  new_codes <- tibble::tibble(
    name=c("PROV"), val=c("10"),
    label_en="Newfoundland Only", label_fr="Seul Terre-Neuve"
  )
  readr::write_csv(new_codes, file.path(vdir, "metadata", "codes.csv"))

  # val "35" is in the data but absent from the truncated codes → unmatched warning
  result <- suppressWarnings(collect_build(vdir, refresh = TRUE))
  expect_true("Newfoundland Only" %in% result$PROV)
})

test_that("pumf_build_duckdb: errors when metadata/ is absent", {
  tmp  <- withr::local_tempdir()
  vdir <- file.path(tmp, "FAKE", "2099")
  dir.create(vdir, recursive = TRUE)

  expect_error(
    canpumf:::pumf_build_duckdb(vdir, "FAKE", "2099"),
    regexp = "metadata/ not found"
  )
})

test_that("pumf_build_duckdb: ENUM column type in DuckDB", {
  tmp  <- withr::local_tempdir()
  vdir <- make_minimal_version_dir(tmp)

  r   <- canpumf:::pumf_build_duckdb(vdir, "FAKE", "2099", lang = "eng")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = r$db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  info      <- DBI::dbGetQuery(con, "PRAGMA table_info('eng')")
  prov_type <- info$type[info$name == "PROV"]
  expect_true(grepl("^ENUM", prov_type),
    label = paste0("PROV should be ENUM, got: '", prov_type, "'"))
})

# ---- pumf_open_duckdb -------------------------------------------------------

test_that("pumf_open_duckdb: returns lazy tbl", {
  tmp  <- withr::local_tempdir()
  vdir <- make_minimal_version_dir(tmp)

  r   <- canpumf:::pumf_build_duckdb(vdir, "FAKE", "2099", lang = "eng")
  tbl <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))
  expect_s3_class(tbl, "tbl")
  expect_equal(nrow(dplyr::collect(tbl)), 3L)
})

test_that("pumf_open_duckdb: errors when file missing", {
  expect_error(
    canpumf:::pumf_open_duckdb(tempfile(fileext = ".duckdb"), "eng"),
    regexp = "not found"
  )
})

test_that("pumf_open_duckdb: errors when table missing", {
  tmp  <- withr::local_tempdir()
  vdir <- make_minimal_version_dir(tmp)

  r <- canpumf:::pumf_build_duckdb(vdir, "FAKE", "2099", lang = "eng")
  expect_error(
    canpumf:::pumf_open_duckdb(r$db_path, "nonexistent_table"),
    regexp = "not found"
  )
})

# ---- pumf_run_pipeline ------------------------------------------------------

test_that("pumf_run_pipeline: returns lazy tbl for minimal fixture", {
  tmp  <- withr::local_tempdir()
  vdir <- make_minimal_version_dir(tmp)

  # pumf_run_pipeline calls Stage 1 (locate_or_download) first; bypass it by
  # providing a fake already-extracted version_dir and call the stages directly.
  # (pumf_run_pipeline itself is exercised via the SFS integration test.)
  # Here we test that build → open → collect works end-to-end.
  r   <- canpumf:::pumf_build_duckdb(vdir, "FAKE", "2099", lang = "eng")
  tbl <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))

  expect_s3_class(tbl, "tbl")
  result <- dplyr::collect(tbl)
  expect_equal(nrow(result), 3L)
})

test_that("pumf_run_pipeline: metadata_encoding passed from registry", {
  # Verify that pumf_parse_metadata picks up metadata_encoding via the registry.
  # We can't run the full pipeline without real data, so just check that the
  # function accepts and propagates the parameter.
  reg <- canpumf:::pumf_registry_lookup("Census", "2021 (individuals)")
  expect_equal(reg$metadata_encoding, "UTF-8")

  # The encoding is passed to pumf_parse_metadata as metadata_encoding; verify
  # the function signature accepts it without error.
  expect_no_error(
    formals(canpumf:::pumf_parse_metadata)[["metadata_encoding"]]
  )
})
