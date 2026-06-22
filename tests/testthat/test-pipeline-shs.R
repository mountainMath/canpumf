# Integration tests for the Survey of Household Spending (SHS) pipeline.
# SHS uses SAS reading cards format with a BSW FWF file co-located with data.
# These tests run against data already in the user's canpumf cache.

.shs_cache <- function() getOption("canpumf.cache_path", "")

.shs_vdir <- function(version) {
  file.path(.shs_cache(), "SHS", version)
}

.shs_metadata_exists <- function(version) {
  file.exists(file.path(.shs_vdir(version), "metadata", "variables.csv"))
}

.shs_duckdb_exists <- function(version) {
  db_file <- paste0("SHS_", version, ".duckdb")
  file.exists(file.path(.shs_vdir(version), db_file))
}

.shs_any_version <- function() {
  for (v in c("2023", "2021", "2019", "2017")) {
    if (canpumf:::.version_is_extracted(.shs_vdir(v))) return(v)
  }
  NULL
}


# ---- Stage 2: metadata parsing ----------------------------------------------

test_that("SHS: pumf_parse_metadata produces canonical CSVs", {
  v <- .shs_any_version()
  skip_if(is.null(v), "No SHS version in cache")

  reg <- canpumf:::pumf_registry_lookup("SHS", v)
  canpumf:::pumf_parse_metadata(.shs_vdir(v), layout_mask = reg$layout_mask)

  meta_dir <- file.path(.shs_vdir(v), "metadata")
  expect_true(file.exists(file.path(meta_dir, "variables.csv")))
  expect_true(file.exists(file.path(meta_dir, "codes.csv")))
  expect_true(file.exists(file.path(meta_dir, "layout.csv")))

  meta <- canpumf:::read_metadata(meta_dir)
  expect_gt(nrow(meta$variables), 50L)
  expect_gt(nrow(meta$codes),     50L)
  expect_gt(nrow(meta$layout),    50L)
  expect_true("HHTYPE6" %in% meta$variables$name,
              label = "HHTYPE6 variable expected in SHS metadata")
})

test_that("SHS: HHTYPE6 codes present in metadata", {
  v <- .shs_any_version()
  skip_if(is.null(v), "No SHS version in cache")
  skip_if_not(.shs_metadata_exists(v), "SHS metadata not parsed")

  meta <- canpumf:::read_metadata(file.path(.shs_vdir(v), "metadata"))
  hhtype_codes <- meta$codes[meta$codes$name == "HHTYPE6", ]
  expect_gt(nrow(hhtype_codes), 0L,
            label = "HHTYPE6 must have value labels in codes.csv")
})


# ---- Stage 3: DuckDB build --------------------------------------------------

test_that("SHS: pumf_build_duckdb creates eng table", {
  v <- .shs_any_version()
  skip_if(is.null(v), "No SHS version in cache")
  skip_if_not(.shs_metadata_exists(v), "SHS metadata not parsed")

  reg <- canpumf:::pumf_registry_lookup("SHS", v)
  r   <- canpumf:::pumf_build_duckdb(.shs_vdir(v), "SHS", v, lang = "eng")
  expect_true(file.exists(r$db_path))
})

test_that("SHS eng table: row count and BSW columns present", {
  v <- .shs_any_version()
  skip_if(is.null(v), "No SHS version in cache")
  skip_if_not(.shs_duckdb_exists(v), "SHS DuckDB not built")

  reg    <- canpumf:::pumf_registry_lookup("SHS", v)
  r      <- canpumf:::pumf_build_duckdb(.shs_vdir(v), "SHS", v, lang = "eng")
  tbl    <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))
  result <- dplyr::collect(tbl)

  expect_gt(nrow(result), 5000L)

  bsw_cols <- names(result)[grepl("^BSW", names(result), ignore.case = TRUE)]
  expect_gt(length(bsw_cols), 50L,
    label = paste0("Expected BSW weight columns; got ", length(bsw_cols)))
})

test_that("SHS eng table: HHTYPE6 is a labelled factor", {
  v <- .shs_any_version()
  skip_if(is.null(v), "No SHS version in cache")
  skip_if_not(.shs_duckdb_exists(v), "SHS DuckDB not built")

  reg    <- canpumf:::pumf_registry_lookup("SHS", v)
  r      <- canpumf:::pumf_build_duckdb(.shs_vdir(v), "SHS", v, lang = "eng")
  tbl    <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))
  result <- dplyr::collect(tbl)

  expect_true("HHTYPE6" %in% names(result), label = "HHTYPE6 column present")
  expect_true(is.factor(result[["HHTYPE6"]]),
              label = "HHTYPE6 should be a factor")
  vals <- unique(stats::na.omit(result[["HHTYPE6"]]))
  expect_false(all(grepl("^\\d+$", as.character(vals))),
    label = "HHTYPE6 should contain labels, not raw numeric codes")
})

test_that("SHS eng table: ENUM columns in DuckDB", {
  v <- .shs_any_version()
  skip_if(is.null(v), "No SHS version in cache")
  skip_if_not(.shs_duckdb_exists(v), "SHS DuckDB not built")

  reg <- canpumf:::pumf_registry_lookup("SHS", v)
  r   <- canpumf:::pumf_build_duckdb(.shs_vdir(v), "SHS", v, lang = "eng")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = r$db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  info      <- DBI::dbGetQuery(con, sprintf("PRAGMA table_info('%s')", r$table_name))
  enum_cols <- info$name[grepl("^ENUM", info$type)]
  expect_gt(length(enum_cols), 0L,
    label = "At least one ENUM column expected in SHS eng table")
})

test_that("SHS: pumf_run_pipeline returns lazy tbl without warnings", {
  v <- .shs_any_version()
  skip_if(is.null(v), "No SHS version in cache")
  skip_if_not(.shs_metadata_exists(v), "SHS metadata not parsed")

  expect_no_warning({
    tbl <- canpumf:::pumf_run_pipeline("SHS", v,
                                        lang       = "eng",
                                        cache_path = .shs_cache())
    on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))
    dplyr::collect(tbl)
  })
})

# ---- Multi-module (2017: Interview + Diary) ---------------------------------

test_that("SHS 2017 registry exposes Interview + Diary modules with own BSW", {
  reg  <- canpumf:::pumf_registry_lookup("SHS", "2017")
  mods <- canpumf:::.pumf_entry_modules(reg)
  expect_identical(sort(names(mods)), c("Diary", "Interview"))
  expect_true(mods$Interview$is_primary)
  expect_null(mods$Interview$meta_subdir)
  expect_identical(mods$Diary$meta_subdir, "Diary")
  # each module carries its own BSW flatfile so the Interview replicate weights
  # are not mis-joined onto the Diary table
  expect_identical(mods$Interview$bsw_file_mask, "interview_bsw_flatfile\\.txt")
  expect_identical(mods$Diary$bsw_file_mask,     "diary_bsw_flatfile\\.txt")
  expect_identical(canpumf:::.pumf_module_key(reg), "CASEID")
})

test_that("SHS 2017 builds joinable Interview + Diary, each with BSW", {
  skip_if_not(canpumf:::.version_is_extracted(.shs_vdir("2017")),
              "SHS 2017 not extracted in cache")

  main <- suppressMessages(get_pumf("SHS", "2017"))
  on.exit(close_pumf(main), add = TRUE)
  expect_true("CASEID" %in% colnames(main))

  diary <- suppressMessages(pumf_module(main, "Diary"))
  expect_true("CASEID" %in% colnames(diary))
  expect_identical(main$src$con, diary$src$con)

  # every diary row belongs to an interview respondent
  ik      <- dplyr::distinct(dplyr::select(main, CASEID))
  matched <- dplyr::inner_join(dplyr::select(diary, CASEID), ik, by = "CASEID")
  expect_identical(dplyr::pull(dplyr::tally(diary)),
                   dplyr::pull(dplyr::tally(matched)))

  # both modules carry their own replicate weights
  expect_gt(sum(grepl("^BSW", colnames(main),  ignore.case = TRUE)), 50L)
  expect_gt(sum(grepl("^BSW", colnames(diary), ignore.case = TRUE)), 50L)

  expect_silent(suppressWarnings(label_pumf_columns(diary)))
})

test_that("SHS: eng/fra bilingual parity", {
  v <- .shs_any_version()
  skip_if(is.null(v), "No SHS version in cache")
  skip_if_not(.shs_metadata_exists(v), "SHS metadata not parsed")

  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)

  r_eng <- canpumf:::pumf_build_duckdb(.shs_vdir(v), "SHS", v,
                                        lang = "eng", db_path = tmp, refresh = TRUE)
  r_fra <- canpumf:::pumf_build_duckdb(.shs_vdir(v), "SHS", v,
                                        lang = "fra", db_path = tmp, refresh = TRUE)

  eng <- .collect_pumf_table(tmp, r_eng$table_name)
  fra <- .collect_pumf_table(tmp, r_fra$table_name)

  expect_pumf_bilingual_parity(eng, fra, label = paste0("SHS ", v))
})
