# Integration tests for the Canadian Income Survey (CIS) pipeline.
# CIS uses SPSS split-file format; no bootstrap weights.
# These tests run against data already in the user's canpumf cache.

.cis_cache <- function() getOption("canpumf.cache_path", "")

.cis_vdir <- function(version) {
  file.path(.cis_cache(), "CIS", version)
}

.cis_metadata_exists <- function(version) {
  file.exists(file.path(.cis_vdir(version), "metadata", "variables.csv"))
}

.cis_duckdb_exists <- function(version) {
  db_file <- paste0("CIS_", version, ".duckdb")
  file.exists(file.path(.cis_vdir(version), db_file))
}

.cis_any_version <- function() {
  for (v in c("2022", "2021", "2020", "2019", "2018")) {
    if (canpumf:::.version_is_extracted(.cis_vdir(v))) return(v)
  }
  NULL
}


# ---- Stage 2: metadata parsing ----------------------------------------------

test_that("CIS: pumf_parse_metadata produces canonical CSVs", {
  v <- .cis_any_version()
  skip_if(is.null(v), "No CIS version in cache")

  canpumf:::pumf_parse_metadata(.cis_vdir(v))

  meta_dir <- file.path(.cis_vdir(v), "metadata")
  expect_true(file.exists(file.path(meta_dir, "variables.csv")))
  expect_true(file.exists(file.path(meta_dir, "codes.csv")))
  expect_true(file.exists(file.path(meta_dir, "layout.csv")))

  meta <- canpumf:::read_metadata(meta_dir)
  expect_gt(nrow(meta$variables), 20L)
  expect_gt(nrow(meta$codes),     20L)
  expect_gt(nrow(meta$layout),    20L)
  expect_true("PROV" %in% meta$variables$name,
              label = "PROV variable expected in CIS metadata")
})

test_that("CIS: PROV codes present in metadata", {
  v <- .cis_any_version()
  skip_if(is.null(v), "No CIS version in cache")
  skip_if_not(.cis_metadata_exists(v), "CIS metadata not parsed")

  meta <- canpumf:::read_metadata(file.path(.cis_vdir(v), "metadata"))
  prov_codes <- meta$codes[meta$codes$name == "PROV", ]
  expect_gt(nrow(prov_codes), 0L,
            label = "PROV must have value labels in codes.csv")
})


# ---- Stage 3: DuckDB build --------------------------------------------------

test_that("CIS: pumf_build_duckdb creates eng table", {
  v <- .cis_any_version()
  skip_if(is.null(v), "No CIS version in cache")
  skip_if_not(.cis_metadata_exists(v), "CIS metadata not parsed")

  r <- canpumf:::pumf_build_duckdb(.cis_vdir(v), "CIS", v, lang = "eng")
  expect_true(file.exists(r$db_path))
})

test_that("CIS eng table: row count and no BSW columns", {
  v <- .cis_any_version()
  skip_if(is.null(v), "No CIS version in cache")
  skip_if_not(.cis_duckdb_exists(v), "CIS DuckDB not built")

  r      <- canpumf:::pumf_build_duckdb(.cis_vdir(v), "CIS", v, lang = "eng")
  tbl    <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))
  result <- dplyr::collect(tbl)

  # CIS has ~10,000–100,000 respondents depending on year
  expect_gt(nrow(result), 5000L)
})

test_that("CIS eng table: PROV is a labelled factor", {
  v <- .cis_any_version()
  skip_if(is.null(v), "No CIS version in cache")
  skip_if_not(.cis_duckdb_exists(v), "CIS DuckDB not built")

  r      <- canpumf:::pumf_build_duckdb(.cis_vdir(v), "CIS", v, lang = "eng")
  tbl    <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))
  result <- dplyr::collect(tbl)

  expect_true("PROV" %in% names(result), label = "PROV column present")
  expect_true(is.factor(result[["PROV"]]),
              label = "PROV should be a factor")
  vals <- unique(stats::na.omit(result[["PROV"]]))
  expect_false(all(grepl("^\\d+$", as.character(vals))),
    label = "PROV should contain labels, not raw numeric codes")
})

test_that("CIS eng table: ENUM columns in DuckDB", {
  v <- .cis_any_version()
  skip_if(is.null(v), "No CIS version in cache")
  skip_if_not(.cis_duckdb_exists(v), "CIS DuckDB not built")

  r   <- canpumf:::pumf_build_duckdb(.cis_vdir(v), "CIS", v, lang = "eng")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = r$db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  info      <- DBI::dbGetQuery(con, sprintf("PRAGMA table_info('%s')", r$table_name))
  enum_cols <- info$name[grepl("^ENUM", info$type)]
  expect_gt(length(enum_cols), 0L,
    label = "At least one ENUM column expected in CIS eng table")
})

test_that("CIS: pumf_run_pipeline returns lazy tbl without warnings", {
  v <- .cis_any_version()
  skip_if(is.null(v), "No CIS version in cache")
  skip_if_not(.cis_metadata_exists(v), "CIS metadata not parsed")

  expect_no_warning({
    tbl <- canpumf:::pumf_run_pipeline("CIS", v,
                                        lang       = "eng",
                                        cache_path = .cis_cache())
    on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))
    dplyr::collect(tbl)
  })
})

test_that("CIS: eng/fra bilingual parity", {
  v <- .cis_any_version()
  skip_if(is.null(v), "No CIS version in cache")
  skip_if_not(.cis_metadata_exists(v), "CIS metadata not parsed")

  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  r_eng <- canpumf:::pumf_build_duckdb(.cis_vdir(v), "CIS", v,
                                        lang = "eng", db_path = tmp, refresh = TRUE)
  r_fra <- canpumf:::pumf_build_duckdb(.cis_vdir(v), "CIS", v,
                                        lang = "fra", db_path = tmp, refresh = TRUE)

  eng <- .collect_pumf_table(tmp, r_eng$table_name)
  fra <- .collect_pumf_table(tmp, r_fra$table_name)

  expect_pumf_bilingual_parity(eng, fra, label = paste0("CIS ", v))
})
