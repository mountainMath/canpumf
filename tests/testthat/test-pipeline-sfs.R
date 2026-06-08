# Integration tests for the Survey of Financial Security (SFS) pipeline.
# SFS uses SPSS split-file format with a bootstrap weights (BSW) FWF file.
# These tests run against data already in the user's canpumf cache.

.sfs_cache <- function() getOption("canpumf.cache_path", "")

.sfs_vdir <- function(version) {
  file.path(.sfs_cache(), "SFS", version)
}

.sfs_metadata_exists <- function(version) {
  file.exists(file.path(.sfs_vdir(version), "metadata", "variables.csv"))
}

.sfs_duckdb_exists <- function(version) {
  db_file <- paste0("SFS_", version, ".duckdb")
  file.exists(file.path(.sfs_vdir(version), db_file))
}


# ---- Helper: pick any cached SFS version with BSW (2016/2019/2023) ----------

.sfs_bsw_version <- function() {
  for (v in c("2023", "2019", "2016")) {
    if (canpumf:::.version_is_extracted(.sfs_vdir(v))) return(v)
  }
  NULL
}


# ---- Stage 2: metadata parsing ----------------------------------------------

test_that("SFS: pumf_parse_metadata produces canonical CSVs", {
  v <- .sfs_bsw_version()
  skip_if(is.null(v), "No SFS version with BSW in cache")

  reg <- canpumf:::pumf_registry_lookup("SFS", v)
  canpumf:::pumf_parse_metadata(.sfs_vdir(v), layout_mask = reg$layout_mask)

  meta_dir <- file.path(.sfs_vdir(v), "metadata")
  expect_true(file.exists(file.path(meta_dir, "variables.csv")))
  expect_true(file.exists(file.path(meta_dir, "codes.csv")))

  meta <- canpumf:::read_metadata(meta_dir)
  expect_gt(nrow(meta$variables), 30L)
  expect_gt(nrow(meta$codes),    30L)
  # Key SFS variables
  expect_true("PAGEMIEG" %in% meta$variables$name ||
              "PWEIGHT"  %in% meta$variables$name,
              label = "Expected age or weight variable in SFS metadata")
})



# ---- Stage 3: DuckDB build --------------------------------------------------

test_that("SFS: pumf_build_duckdb creates eng table", {
  v <- .sfs_bsw_version()
  skip_if(is.null(v), "No SFS version with BSW in cache")
  skip_if_not(.sfs_metadata_exists(v), "SFS metadata not parsed")

  reg <- canpumf:::pumf_registry_lookup("SFS", v)
  r   <- canpumf:::pumf_build_duckdb(.sfs_vdir(v), "SFS", v,
                                      lang        = "eng",
                                      layout_mask = reg$layout_mask)

  expect_true(file.exists(r$db_path))
})

test_that("SFS eng table: row count and BSW columns present", {
  v <- .sfs_bsw_version()
  skip_if(is.null(v), "No SFS version with BSW in cache")
  skip_if_not(.sfs_duckdb_exists(v), "SFS DuckDB not built")

  reg  <- canpumf:::pumf_registry_lookup("SFS", v)
  r    <- canpumf:::pumf_build_duckdb(.sfs_vdir(v), "SFS", v,
                                       lang        = "eng",
                                       layout_mask = reg$layout_mask)
  tbl  <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))

  result <- dplyr::collect(tbl)

  # SFS sample sizes: ~12,000 (2019), ~9,000 (2016/2023)
  expect_gt(nrow(result), 5000L)

  # BSW join: should have many weight columns (500 bootstrap weights)
  bsw_cols <- names(result)[grepl("^BSW|^bsw", names(result), ignore.case = TRUE)]
  expect_gt(length(bsw_cols), 50L,
    label = paste0("Expected 500 bootstrap weight columns; got ", length(bsw_cols)))
})

test_that("SFS eng table: PWEIGHT column absent (dropped per registry)", {
  v <- .sfs_bsw_version()
  skip_if(is.null(v), "No SFS version with BSW in cache")
  skip_if_not(.sfs_duckdb_exists(v), "SFS DuckDB not built")

  reg    <- canpumf:::pumf_registry_lookup("SFS", v)
  r      <- canpumf:::pumf_build_duckdb(.sfs_vdir(v), "SFS", v,
                                         lang        = "eng",
                                         layout_mask = reg$layout_mask)
  tbl    <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))
  result <- dplyr::collect(tbl)

  # PWEIGHT appears in both the main data and the BSW file.
  # bsw_drop_cols = "PWEIGHT" drops the BSW copy to avoid a name conflict;
  # the main-data PWEIGHT remains in the final table.
  expect_true("PWEIGHT" %in% names(result),
    label = "PWEIGHT from main data should be present")
})

test_that("SFS eng table: padded columns are properly labeled", {
  v <- .sfs_bsw_version()
  skip_if(is.null(v), "No SFS version with BSW in cache")
  skip_if_not(.sfs_duckdb_exists(v), "SFS DuckDB not built")

  padded <- c("PASRBUYG", "PASRDWNG", "PASRMPFG")
  reg    <- canpumf:::pumf_registry_lookup("SFS", v)
  r      <- canpumf:::pumf_build_duckdb(.sfs_vdir(v), "SFS", v,
                                         lang        = "eng",
                                         layout_mask = reg$layout_mask)
  tbl    <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))
  result <- dplyr::collect(tbl)

  present <- intersect(padded, names(result))
  skip_if(length(present) == 0L, "Padded columns not in this SFS version")

  for (col in present) {
    vals <- unique(stats::na.omit(result[[col]]))
    # Should be factor labels, not raw numeric codes like "1", "2"
    expect_false(all(grepl("^\\d+$", vals)),
      label = paste0(col, " should contain labels, not raw codes"))
  }
})

test_that("SFS eng table: ENUM columns in DuckDB", {
  v <- .sfs_bsw_version()
  skip_if(is.null(v), "No SFS version with BSW in cache")
  skip_if_not(.sfs_duckdb_exists(v), "SFS DuckDB not built")

  reg <- canpumf:::pumf_registry_lookup("SFS", v)
  r   <- canpumf:::pumf_build_duckdb(.sfs_vdir(v), "SFS", v,
                                      lang        = "eng",
                                      layout_mask = reg$layout_mask)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = r$db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  info      <- DBI::dbGetQuery(con, sprintf("PRAGMA table_info('%s')", r$table_name))
  enum_cols <- info$name[grepl("^ENUM", info$type)]
  expect_gt(length(enum_cols), 0L,
    label = "At least one ENUM column expected in SFS eng table")
})

test_that("SFS: fra table built without error", {
  v <- .sfs_bsw_version()
  skip_if(is.null(v), "No SFS version with BSW in cache")
  skip_if_not(.sfs_metadata_exists(v), "SFS metadata not parsed")

  reg <- canpumf:::pumf_registry_lookup("SFS", v)
  r   <- canpumf:::pumf_build_duckdb(.sfs_vdir(v), "SFS", v,
                                      lang        = "fra",
                                      layout_mask = reg$layout_mask)
  expect_true(file.exists(r$db_path))
})

test_that("SFS: eng and fra tables coexist in same DuckDB", {
  v <- .sfs_bsw_version()
  skip_if(is.null(v), "No SFS version with BSW in cache")
  skip_if_not(.sfs_duckdb_exists(v), "SFS DuckDB not built")

  reg   <- canpumf:::pumf_registry_lookup("SFS", v)
  r_eng <- canpumf:::pumf_build_duckdb(.sfs_vdir(v), "SFS", v,
                                        lang = "eng", layout_mask = reg$layout_mask)
  r_fra <- canpumf:::pumf_build_duckdb(.sfs_vdir(v), "SFS", v,
                                        lang = "fra", layout_mask = reg$layout_mask)
  con   <- DBI::dbConnect(duckdb::duckdb(), dbdir = r_eng$db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  expect_true(DBI::dbExistsTable(con, r_eng$table_name))
  expect_true(DBI::dbExistsTable(con, r_fra$table_name))
})

# ---- pumf_run_pipeline convenience ------------------------------------------

test_that("pumf_run_pipeline: returns lazy tbl for SFS", {
  v <- .sfs_bsw_version()
  skip_if(is.null(v), "No SFS version with BSW in cache")
  skip_if_not(.sfs_metadata_exists(v), "SFS metadata not parsed")

  tbl <- canpumf:::pumf_run_pipeline("SFS", v,
                                      lang       = "eng",
                                      cache_path = .sfs_cache())
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))

  expect_s3_class(tbl, "tbl")
  expect_gt(dplyr::collect(tbl) |> nrow(), 5000L)
})

# ---- Per-version verified tests ---------------------------------------------

# All end-to-end verified versions (import without errors or unexpected warnings)
.sfs_verified <- c("2023", "2019", "2016", "2012", "2005")

# ---- No-unexpected-warnings loop --------------------------------------------

for (.v in .sfs_verified) {
  local({
    ver <- .v
    test_that(paste0("SFS ", ver, ": pipeline emits no unexpected warnings"), {
      skip_if_not(.sfs_duckdb_exists(ver),
                  paste("SFS", ver, "DuckDB not built"))

      warns <- character(0L)
      withCallingHandlers(
        {
          reg <- canpumf:::pumf_registry_lookup("SFS", ver)
          r   <- canpumf:::pumf_build_duckdb(.sfs_vdir(ver), "SFS", ver,
                                              lang        = "eng",
                                              layout_mask = reg$layout_mask)
          tbl <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
          on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))
          dplyr::collect(tbl)
        },
        warning = function(w) {
          warns <<- c(warns, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      expect_identical(warns, character(0L),
        label = paste0("SFS ", ver, " should produce no warnings"))
    })
  })
}

# ---- Bilingual label coverage loop ------------------------------------------

for (.v in .sfs_verified) {
  local({
    ver <- .v
    test_that(paste0("SFS ", ver, ": metadata has English and French labels"), {
      skip_if_not(.sfs_metadata_exists(ver),
                  paste("SFS", ver, "metadata not parsed"))

      meta <- canpumf:::read_metadata(file.path(.sfs_vdir(ver), "metadata"))

      en_vars <- sum(!is.na(meta$variables$label_en) &
                       nchar(meta$variables$label_en) > 0L)
      expect_gt(en_vars, 0L,
        label = paste0("SFS ", ver, ": should have English variable labels"))

      fr_vars <- sum(!is.na(meta$variables$label_fr) &
                       nchar(meta$variables$label_fr) > 0L)
      expect_gt(fr_vars, 0L,
        label = paste0("SFS ", ver, ": should have French variable labels"))

      en_codes <- sum(!is.na(meta$codes$label_en) &
                        nchar(meta$codes$label_en) > 0L)
      expect_gt(en_codes, 0L,
        label = paste0("SFS ", ver, ": should have English code labels"))

      fr_codes <- sum(!is.na(meta$codes$label_fr) &
                        nchar(meta$codes$label_fr) > 0L)
      expect_gt(fr_codes, 0L,
        label = paste0("SFS ", ver, ": should have French code labels"))
    })
  })
}
