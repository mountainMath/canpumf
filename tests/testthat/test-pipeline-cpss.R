# End-to-end integration test for CPSS v1.
# Exercises all three pipeline stages for a real downloadable survey.
# Skipped whenever CPSS v1 data is not already in the local cache.
# (The download itself is tested in test-pipeline-stage1.R.)

.cpss_cache <- function() getOption("canpumf.cache_path", "")

.cpss_vdir <- function() {
  file.path(.cpss_cache(), "CPSS", "1")
}

.cpss_extracted <- function() {
  canpumf:::.version_is_extracted(.cpss_vdir())
}

# ---- Stage 1 ----------------------------------------------------------------

test_that("CPSS v1: locate_or_download creates version_dir with zip + content", {
  skip_if_not(.cpss_extracted(), "CPSS v1 not in cache")

  vdir <- canpumf:::pumf_locate_or_download("CPSS", "1", cache_path = .cpss_cache())

  expect_true(dir.exists(vdir))
  expect_false(is.null(canpumf:::.find_version_zip(vdir)))
  expect_true(canpumf:::.version_is_extracted(vdir))
})

# ---- Stage 2 ----------------------------------------------------------------
# CPSS v1 ships only PDF codebooks — no machine-readable variables.csv.
# Stage 2 and 3 tests are skipped automatically when detect_formats finds
# nothing to parse.  CPSS v2–v6 include a variables.csv and will run fully.

.cpss_has_metadata <- function() {
  vdir <- .cpss_vdir()
  canpumf:::.version_is_extracted(vdir) &&
    length(canpumf:::detect_formats(vdir)) > 0L
}

test_that("CPSS v1: pumf_parse_metadata produces canonical CSV files", {
  skip_if_not(.cpss_extracted(), "CPSS v1 not in cache")
  skip_if_not(.cpss_has_metadata(), "CPSS v1 has no machine-readable codebook")

  canpumf:::pumf_parse_metadata(.cpss_vdir())

  meta_dir <- file.path(.cpss_vdir(), "metadata")
  expect_true(file.exists(file.path(meta_dir, "variables.csv")))
  expect_true(file.exists(file.path(meta_dir, "codes.csv")))

  meta <- canpumf:::read_metadata(meta_dir)
  expect_gt(nrow(meta$variables), 5L)
  expect_gt(nrow(meta$codes),     5L)
  expect_named(meta$variables,
               c("name","label_en","label_fr","type","decimals",
                 "missing_low","missing_high"),
               ignore.order = TRUE)
})

# ---- Stage 3 — eng ----------------------------------------------------------

test_that("CPSS v1: pumf_build_duckdb creates DuckDB for lang=eng", {
  skip_if_not(.cpss_extracted(), "CPSS v1 not in cache")
  skip_if_not(file.exists(file.path(.cpss_vdir(), "metadata", "variables.csv")),
              "Stage 2 not run")

  r <- canpumf:::pumf_build_duckdb(.cpss_vdir(), "CPSS", "1", lang = "eng")

  expect_named(r, c("db_path", "table_name"), ignore.order = TRUE)
  expect_true(file.exists(r$db_path))
  expect_equal(r$table_name, "eng")
})

test_that("CPSS v1 eng table: row and column counts are plausible", {
  skip_if_not(.cpss_extracted(), "CPSS v1 not in cache")
  skip_if_not(file.exists(file.path(.cpss_vdir(), "CPSS_1.duckdb")),
              "Stage 3 not run")

  r   <- canpumf:::pumf_build_duckdb(.cpss_vdir(), "CPSS", "1", lang = "eng")
  tbl <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))

  result <- dplyr::collect(tbl)
  expect_gt(nrow(result), 1000L)
  expect_gt(ncol(result), 10L)
})

test_that("CPSS v1 eng table: PROV column is ENUM in DuckDB", {
  skip_if_not(.cpss_extracted(), "CPSS v1 not in cache")
  skip_if_not(file.exists(file.path(.cpss_vdir(), "CPSS_1.duckdb")),
              "Stage 3 not run")

  r   <- canpumf:::pumf_build_duckdb(.cpss_vdir(), "CPSS", "1", lang = "eng")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = r$db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  info <- DBI::dbGetQuery(con, "PRAGMA table_info('eng')")
  enum_cols <- info$name[grepl("^ENUM", info$type)]
  expect_gt(length(enum_cols), 0L,
    label = "At least one ENUM column expected in CPSS eng table")
})

test_that("CPSS v1 eng table: second call is a no-op (no rebuild)", {
  skip_if_not(.cpss_extracted(), "CPSS v1 not in cache")
  skip_if_not(file.exists(file.path(.cpss_vdir(), "CPSS_1.duckdb")),
              "Stage 3 not run")

  db_path <- file.path(.cpss_vdir(), "CPSS_1.duckdb")
  mtime1  <- file.info(db_path)$mtime
  Sys.sleep(0.05)

  canpumf:::pumf_build_duckdb(.cpss_vdir(), "CPSS", "1", lang = "eng")
  mtime2  <- file.info(db_path)$mtime

  expect_equal(mtime1, mtime2)
})

# ---- Stage 3 — fra ----------------------------------------------------------

test_that("CPSS v1: pumf_build_duckdb creates DuckDB for lang=fra", {
  skip_if_not(.cpss_extracted(), "CPSS v1 not in cache")
  skip_if_not(file.exists(file.path(.cpss_vdir(), "metadata", "variables.csv")),
              "Stage 2 not run")

  r <- canpumf:::pumf_build_duckdb(.cpss_vdir(), "CPSS", "1", lang = "fra")
  expect_equal(r$table_name, "fra")
  expect_true(file.exists(r$db_path))
})

test_that("CPSS v1: eng and fra tables coexist in same DuckDB", {
  skip_if_not(.cpss_extracted(), "CPSS v1 not in cache")
  skip_if_not(file.exists(file.path(.cpss_vdir(), "CPSS_1.duckdb")),
              "Stage 3 not run")

  canpumf:::pumf_build_duckdb(.cpss_vdir(), "CPSS", "1", lang = "eng")
  canpumf:::pumf_build_duckdb(.cpss_vdir(), "CPSS", "1", lang = "fra")

  con <- DBI::dbConnect(duckdb::duckdb(),
                         dbdir = file.path(.cpss_vdir(), "CPSS_1.duckdb"),
                         read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  expect_true(DBI::dbExistsTable(con, "eng"))
  expect_true(DBI::dbExistsTable(con, "fra"))
})

test_that("CPSS v1: eng/fra bilingual parity", {
  skip_if_not(.cpss_extracted(), "CPSS v1 not in cache")
  skip_if_not(file.exists(file.path(.cpss_vdir(), "metadata", "variables.csv")),
              "Stage 2 not run")

  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  r_eng <- canpumf:::pumf_build_duckdb(.cpss_vdir(), "CPSS", "1",
                                        lang = "eng", db_path = tmp, refresh = TRUE)
  r_fra <- canpumf:::pumf_build_duckdb(.cpss_vdir(), "CPSS", "1",
                                        lang = "fra", db_path = tmp, refresh = TRUE)

  eng <- .collect_pumf_table(tmp, r_eng$table_name)
  fra <- .collect_pumf_table(tmp, r_fra$table_name)

  expect_pumf_bilingual_parity(eng, fra, label = "CPSS v1")
})

# ---- pumf_open_duckdb -------------------------------------------------------

test_that("pumf_open_duckdb returns lazy tbl for CPSS", {
  skip_if_not(.cpss_extracted(), "CPSS v1 not in cache")
  skip_if_not(file.exists(file.path(.cpss_vdir(), "CPSS_1.duckdb")),
              "Stage 3 not run")

  r   <- canpumf:::pumf_build_duckdb(.cpss_vdir(), "CPSS", "1", lang = "eng")
  tbl <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))

  expect_s3_class(tbl, "tbl")
  expect_false(inherits(tbl, "data.frame"))
  result <- dplyr::collect(tbl)
  expect_s3_class(result, "data.frame")
})
