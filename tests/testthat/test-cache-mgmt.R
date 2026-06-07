# Tests for list_pumf_cache() and remove_pumf_cache().

# ---- list_pumf_cache --------------------------------------------------------

test_that("list_pumf_cache: empty cache returns empty tibble", {
  tmp <- withr::local_tempdir()
  result <- list_pumf_cache(cache_path = tmp)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
  expect_named(result, c("series","version","has_raw","has_metadata",
                          "has_duckdb","raw_mb","duckdb_mb"), ignore.order = TRUE)
})

test_that("list_pumf_cache: nonexistent cache_path returns empty tibble", {
  result <- list_pumf_cache(cache_path = "/nonexistent/path/xyz")
  expect_equal(nrow(result), 0L)
})

test_that("list_pumf_cache: detects raw + metadata + duckdb for built survey", {
  tmp <- withr::local_tempdir()
  # Use the e2e fixture builder from test-api.R via get_pumf
  vdir     <- file.path(tmp, "SFS", "2019")
  meta_dir <- file.path(vdir, "metadata")
  dir.create(meta_dir, recursive = TRUE)

  vars <- tibble::tibble(name="X", label_en="V", label_fr="V",
                          type="character", decimals=NA_integer_,
                          missing_low=NA_real_, missing_high=NA_real_)
  readr::write_csv(vars,  file.path(meta_dir, "variables.csv"))
  readr::write_csv(tibble::tibble(name=character(), val=character(),
                                   label_en=character(), label_fr=character()),
                   file.path(meta_dir, "codes.csv"))
  writeLines("raw content", file.path(vdir, "data.csv"))

  result <- list_pumf_cache(cache_path = tmp)
  expect_equal(nrow(result), 1L)
  expect_equal(result$series,       "SFS")
  expect_equal(result$version,      "2019")
  expect_true(result$has_raw)
  expect_true(result$has_metadata)
  expect_false(result$has_duckdb)
  expect_true(result$raw_mb >= 0)
  expect_true(is.na(result$duckdb_mb))
})

test_that("list_pumf_cache: has_duckdb TRUE after get_pumf builds it", {
  tmp  <- withr::local_tempdir()
  vdir <- file.path(tmp, "FAKE", "2099")
  meta_dir <- file.path(vdir, "metadata")
  dir.create(meta_dir, recursive = TRUE)

  vars <- tibble::tibble(name="X", label_en="V", label_fr="V",
                          type="character", decimals=NA_integer_,
                          missing_low=NA_real_, missing_high=NA_real_)
  readr::write_csv(vars, file.path(meta_dir, "variables.csv"))
  readr::write_csv(tibble::tibble(name=character(), val=character(),
                                   label_en=character(), label_fr=character()),
                   file.path(meta_dir, "codes.csv"))
  readr::write_csv(tibble::tibble(X = "a"), file.path(vdir, "data.csv"))
  readr::write_csv(tibble::tibble(
    Field_Champ=c("X", NA_character_),
    Variable_Variable=c("X", "a"),
    EnglishLabel_EtiquetteAnglais=c("Var","Label a"),
    FrenchLabel_EtiquetteFrancais=c("Var","Etiq a")
  ), file.path(vdir, "codebook.csv"))
  writeLines("", file.path(vdir, "sentinel.txt"))

  tbl <- get_pumf("FAKE", "2099", cache_path = tmp)
  close_pumf(tbl)

  result <- list_pumf_cache(cache_path = tmp)
  row <- result[result$series == "FAKE" & result$version == "2099", ]
  expect_true(row$has_duckdb)
  expect_true(row$duckdb_mb > 0)
})

# ---- remove_pumf_cache: non-LFS ---------------------------------------------

test_that("remove_pumf_cache: keep_raw=TRUE removes DuckDB and metadata only", {
  tmp <- withr::local_tempdir()
  vdir <- file.path(tmp, "SFS", "2019")
  meta_dir <- file.path(vdir, "metadata")
  dir.create(meta_dir, recursive = TRUE)
  writeLines("raw", file.path(vdir, "data.csv"))
  writeLines("var", file.path(meta_dir, "variables.csv"))
  # Fake a duckdb file
  writeLines("", file.path(vdir, "SFS_2019.duckdb"))

  expect_message(
    remove_pumf_cache("SFS", "2019", keep_raw = TRUE, cache_path = tmp),
    regexp = "Raw files kept"
  )

  expect_false(file.exists(file.path(vdir, "SFS_2019.duckdb")))
  expect_false(dir.exists(meta_dir))
  expect_true(file.exists(file.path(vdir, "data.csv")))   # raw kept
  expect_true(dir.exists(vdir))
})

test_that("remove_pumf_cache: keep_raw=FALSE removes entire version dir", {
  tmp <- withr::local_tempdir()
  vdir <- file.path(tmp, "SFS", "2019")
  dir.create(vdir, recursive = TRUE)
  writeLines("raw", file.path(vdir, "data.csv"))

  expect_message(
    remove_pumf_cache("SFS", "2019", keep_raw = FALSE, cache_path = tmp),
    regexp = "Removed all"
  )
  expect_false(dir.exists(vdir))
})

test_that("remove_pumf_cache: errors when version not in cache", {
  tmp <- withr::local_tempdir()
  expect_error(
    remove_pumf_cache("SFS", "2099", cache_path = tmp),
    regexp = "not found in cache"
  )
})

# ---- remove_pumf_cache: LFS -------------------------------------------------

test_that("remove_pumf_cache: LFS keep_raw=TRUE removes rows and metadata", {
  tmp      <- withr::local_tempdir()
  lfs_dir  <- file.path(tmp, "LFS")
  db_path  <- file.path(lfs_dir, "LFS.duckdb")
  vdir     <- file.path(lfs_dir, "2022")
  meta_dir <- file.path(vdir, "metadata")
  dir.create(meta_dir, recursive = TRUE)
  writeLines("raw", file.path(vdir, "pub2022.csv"))
  writeLines("var", file.path(meta_dir, "variables.csv"))

  # Seed the DuckDB with a row and an lfs_versions entry
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  DBI::dbWriteTable(con, "lfs_eng",
                    data.frame(SURVYEAR = 2022L, PROV = "ON"))
  DBI::dbWriteTable(con, "lfs_versions",
                    data.frame(version = "2022", type = "annual",
                               survyear = 2022L, survmnth = NA_integer_,
                               loaded_at = Sys.time(), n_records = 1L))
  DBI::dbDisconnect(con, shutdown = TRUE)

  expect_message(
    remove_pumf_cache("LFS", "2022", keep_raw = TRUE, cache_path = tmp),
    regexp = "Raw files kept"
  )

  # DuckDB still exists (had 1 version, now 0) — file deleted
  expect_false(file.exists(db_path))
  expect_false(dir.exists(meta_dir))
  expect_true(file.exists(file.path(vdir, "pub2022.csv")))  # raw kept
})

test_that("remove_pumf_cache: LFS with two versions keeps DB after removing one", {
  tmp      <- withr::local_tempdir()
  lfs_dir  <- file.path(tmp, "LFS")
  db_path  <- file.path(lfs_dir, "LFS.duckdb")
  dir.create(lfs_dir, recursive = TRUE)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  DBI::dbWriteTable(con, "lfs_eng",
                    data.frame(SURVYEAR = c(2021L, 2022L), PROV = c("ON","QC")))
  DBI::dbWriteTable(con, "lfs_versions",
                    data.frame(version   = c("2021", "2022"),
                               type      = c("annual", "annual"),
                               survyear  = c(2021L, 2022L),
                               survmnth  = c(NA_integer_, NA_integer_),
                               loaded_at = c(Sys.time(), Sys.time()),
                               n_records = c(1L, 1L)))
  DBI::dbDisconnect(con, shutdown = TRUE)

  remove_pumf_cache("LFS", "2022", keep_raw = TRUE, cache_path = tmp)

  # DuckDB still exists; 2021 data intact
  expect_true(file.exists(db_path))
  con2 <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
  n <- DBI::dbGetQuery(con2, "SELECT COUNT(*) AS n FROM lfs_eng WHERE SURVYEAR=2021")$n
  DBI::dbDisconnect(con2, shutdown = TRUE)
  expect_equal(n, 1L)
})
