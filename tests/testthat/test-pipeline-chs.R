# Integration tests for the Canadian Housing Survey (CHS) pipeline.
# CHS uses SPSS split-file format with a separate bootstrap weights (BSW) CSV.
# These tests run against data already in the user's canpumf cache; they skip
# silently when the relevant version has not yet been downloaded.

.chs_cache <- function() getOption("canpumf.cache_path", "")

.chs_vdir <- function(version) {
  file.path(.chs_cache(), "CHS", version)
}

.chs_metadata_exists <- function(version) {
  file.exists(file.path(.chs_vdir(version), "metadata", "variables.csv"))
}

.chs_duckdb_exists <- function(version) {
  db_file <- paste0("CHS_", version, ".duckdb")
  file.exists(file.path(.chs_vdir(version), db_file))
}


# ---- Helper: pick any cached CHS version ------------------------------------

.chs_any_version <- function() {
  for (v in c("2022", "2021", "2018")) {
    if (canpumf:::.version_is_extracted(.chs_vdir(v))) return(v)
  }
  NULL
}


# ---- Stage 2: metadata parsing ----------------------------------------------

test_that("CHS: pumf_parse_metadata produces canonical CSVs", {
  v <- .chs_any_version()
  skip_if(is.null(v), "No CHS version in cache")

  reg <- canpumf:::pumf_registry_lookup("CHS", v)
  canpumf:::pumf_parse_metadata(.chs_vdir(v),
                                 layout_mask = reg$layout_mask)

  meta_dir <- file.path(.chs_vdir(v), "metadata")
  expect_true(file.exists(file.path(meta_dir, "variables.csv")))
  expect_true(file.exists(file.path(meta_dir, "codes.csv")))

  meta <- canpumf:::read_metadata(meta_dir)
  expect_gt(nrow(meta$variables), 20L)
  expect_gt(nrow(meta$codes),    20L)
  expect_true("PROV" %in% meta$variables$name ||
              "PPROV" %in% meta$variables$name,
              label = "Province variable expected in CHS metadata")
})


# ---- Stage 3: DuckDB build --------------------------------------------------

test_that("CHS: pumf_build_duckdb creates eng table", {
  v <- .chs_any_version()
  skip_if(is.null(v), "No CHS version in cache")
  skip_if_not(.chs_metadata_exists(v), "CHS metadata not parsed")

  reg <- canpumf:::pumf_registry_lookup("CHS", v)
  r   <- canpumf:::pumf_build_duckdb(.chs_vdir(v), "CHS", v,
                                      lang        = "eng",
                                      layout_mask = reg$layout_mask)

  expect_true(file.exists(r$db_path))
  expect_equal(r$table_name, if (is.null(reg$layout_mask)) "eng"
                             else paste0("eng_", reg$layout_mask))
})

test_that("CHS eng table: row count and BSW columns are plausible", {
  v <- .chs_any_version()
  skip_if(is.null(v), "No CHS version in cache")
  skip_if_not(.chs_duckdb_exists(v), "CHS DuckDB not built")

  reg  <- canpumf:::pumf_registry_lookup("CHS", v)
  r    <- canpumf:::pumf_build_duckdb(.chs_vdir(v), "CHS", v,
                                       lang        = "eng",
                                       layout_mask = reg$layout_mask)
  tbl  <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))

  result <- dplyr::collect(tbl)

  # Row count: all three CHS versions have 10,000+ respondents
  expect_gt(nrow(result), 5000L)

  # BSW join: weight columns should be present
  weight_cols <- names(result)[grepl("^(BSW|bsw|BSWEIGHT|PFWEIGHT)",
                                      names(result), ignore.case = TRUE)]
  expect_gt(length(weight_cols), 0L,
    label = "Expected bootstrap weight columns from BSW join")
})

test_that("CHS eng table: PROV column exists and is labeled", {
  v <- .chs_any_version()
  skip_if(is.null(v), "No CHS version in cache")
  skip_if_not(.chs_duckdb_exists(v), "CHS DuckDB not built")

  reg    <- canpumf:::pumf_registry_lookup("CHS", v)
  r      <- canpumf:::pumf_build_duckdb(.chs_vdir(v), "CHS", v,
                                         lang        = "eng",
                                         layout_mask = reg$layout_mask)
  tbl    <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))
  result <- dplyr::collect(tbl)

  prov_col <- names(result)[grepl("^(PROV|PPROV)$", names(result),
                                   ignore.case = TRUE)]
  skip_if(length(prov_col) == 0L, "No province column found")

  prov_vals <- unique(stats::na.omit(result[[prov_col[[1L]]]]))
  # Should contain human-readable province names, not numeric codes
  expect_true(any(grepl("Ontario|British Columbia|Quebec|Québec|Alberta",
                          prov_vals, ignore.case = TRUE)),
    label = paste0("Province column '", prov_col[[1L]],
                   "' should contain labels, got: ",
                   paste(head(prov_vals, 5), collapse = ", ")))
})

test_that("CHS eng table: categorical columns stored as ENUM in DuckDB", {
  v <- .chs_any_version()
  skip_if(is.null(v), "No CHS version in cache")
  skip_if_not(.chs_duckdb_exists(v), "CHS DuckDB not built")

  reg <- canpumf:::pumf_registry_lookup("CHS", v)
  r   <- canpumf:::pumf_build_duckdb(.chs_vdir(v), "CHS", v,
                                      lang        = "eng",
                                      layout_mask = reg$layout_mask)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = r$db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  info      <- DBI::dbGetQuery(con, sprintf("PRAGMA table_info('%s')", r$table_name))
  enum_cols <- info$name[grepl("^ENUM", info$type)]
  expect_gt(length(enum_cols), 0L,
    label = "At least one ENUM column expected in CHS eng table")
})

test_that("CHS: fra table has French province labels", {
  v <- .chs_any_version()
  skip_if(is.null(v), "No CHS version in cache")
  skip_if_not(.chs_metadata_exists(v), "CHS metadata not parsed")

  reg <- canpumf:::pumf_registry_lookup("CHS", v)
  r   <- canpumf:::pumf_build_duckdb(.chs_vdir(v), "CHS", v,
                                      lang        = "fra",
                                      layout_mask = reg$layout_mask)
  tbl    <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))
  result <- dplyr::collect(tbl)

  prov_col <- names(result)[grepl("^(PROV|PPROV)$", names(result),
                                   ignore.case = TRUE)]
  skip_if(length(prov_col) == 0L, "No province column found")

  prov_vals <- unique(stats::na.omit(result[[prov_col[[1L]]]]))
  # French labels: "Québec", "Ontario" (shared), "Colombie-Britannique"
  expect_true(any(grepl("Québec|Colombie|Ontario|Alberta|Québec",
                          prov_vals)),
    label = "French province labels expected in CHS fra table")
})

test_that("CHS: eng and fra tables coexist in same DuckDB", {
  v <- .chs_any_version()
  skip_if(is.null(v), "No CHS version in cache")
  skip_if_not(.chs_duckdb_exists(v), "CHS DuckDB not built")

  reg    <- canpumf:::pumf_registry_lookup("CHS", v)
  r_eng  <- canpumf:::pumf_build_duckdb(.chs_vdir(v), "CHS", v,
                                         lang = "eng", layout_mask = reg$layout_mask)
  r_fra  <- canpumf:::pumf_build_duckdb(.chs_vdir(v), "CHS", v,
                                         lang = "fra", layout_mask = reg$layout_mask)
  con    <- DBI::dbConnect(duckdb::duckdb(), dbdir = r_eng$db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  expect_true(DBI::dbExistsTable(con, r_eng$table_name))
  expect_true(DBI::dbExistsTable(con, r_fra$table_name))
})

test_that("CHS: eng/fra bilingual parity", {
  v <- .chs_any_version()
  skip_if(is.null(v), "No CHS version in cache")
  skip_if_not(.chs_metadata_exists(v), "CHS metadata not parsed")

  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  reg   <- canpumf:::pumf_registry_lookup("CHS", v)
  r_eng <- canpumf:::pumf_build_duckdb(.chs_vdir(v), "CHS", v,
                                        lang = "eng", layout_mask = reg$layout_mask,
                                        db_path = tmp, refresh = TRUE)
  r_fra <- canpumf:::pumf_build_duckdb(.chs_vdir(v), "CHS", v,
                                        lang = "fra", layout_mask = reg$layout_mask,
                                        db_path = tmp, refresh = TRUE)

  eng <- .collect_pumf_table(tmp, r_eng$table_name)
  fra <- .collect_pumf_table(tmp, r_fra$table_name)

  expect_pumf_bilingual_parity(eng, fra, label = paste0("CHS ", v))
})
