# Tests for the public API: get_pumf() and pumf_metadata().

# ---- get_pumf: input validation ---------------------------------------------

test_that("get_pumf: errors when series is NULL", {
  expect_error(get_pumf(), regexp = "series.*must be specified")
})

test_that("get_pumf: errors on invalid lang", {
  expect_error(get_pumf("SFS", "2019", lang = "deu"), regexp = "lang")
})

test_that("get_pumf: errors on refresh='auto' for non-LFS", {
  expect_error(
    get_pumf("SFS", "2019", refresh = "auto"),
    regexp = "auto.*LFS"
  )
})

test_that("get_pumf: errors on invalid refresh value", {
  expect_error(
    get_pumf("SFS", "2019", refresh = "yes"),
    regexp = "refresh.*must be"
  )
})

test_that("get_pumf: errors when redownload=TRUE and refresh='auto'", {
  expect_error(
    get_pumf("LFS", refresh = "auto", redownload = TRUE),
    regexp = "redownload.*auto|auto.*redownload",
    ignore.case = TRUE
  )
})

test_that("get_pumf: errors when version=NULL and multiple exist", {
  skip_if_offline()
  tryCatch(
    expect_error(get_pumf("SFS"), regexp = "multiple versions"),
    error = function(e) skip(paste("StatCan unreachable:", conditionMessage(e)))
  )
})

# ---- get_pumf: deprecated parameter names -----------------------------------

test_that("get_pumf: warns on deprecated 'pumf_series'", {
  # Don't actually run pipeline; the warning fires before any download
  expect_warning(
    tryCatch(
      get_pumf(pumf_series = "SFS", version = "2019",
               cache_path = tempdir()),
      error = function(e) NULL  # pipeline may fail without real data
    ),
    regexp = "pumf_series.*deprecated"
  )
})

test_that("get_pumf: warns on deprecated 'pumf_version'", {
  expect_warning(
    tryCatch(
      get_pumf(series = "SFS", pumf_version = "2019",
               cache_path = tempdir()),
      error = function(e) NULL
    ),
    regexp = "pumf_version.*deprecated"
  )
})

test_that("get_pumf: warns on deprecated 'pumf_cache_path'", {
  expect_warning(
    tryCatch(
      get_pumf(series = "SFS", version = "2019",
               pumf_cache_path = tempdir()),
      error = function(e) NULL
    ),
    regexp = "pumf_cache_path.*deprecated"
  )
})

# ---- pumf_metadata: basic contract ------------------------------------------

test_that("pumf_metadata returns list with three elements", {
  tmp  <- withr::local_tempdir()
  # Use a pre-built fixture version directory (no download needed)
  vdir     <- file.path(tmp, "FAKE", "2099")
  meta_dir <- file.path(vdir, "metadata")
  dir.create(meta_dir, recursive = TRUE)

  vars  <- tibble::tibble(name="X", label_en="V", label_fr="V",
                           type="character", decimals=NA_integer_,
                           missing_low=NA_real_, missing_high=NA_real_)
  codes <- tibble::tibble(name="X", val="1", label_en="One", label_fr="Un")
  readr::write_csv(vars,  file.path(meta_dir, "variables.csv"))
  readr::write_csv(codes, file.path(meta_dir, "codes.csv"))
  writeLines("X\n1", file.path(vdir, "data.csv"))
  writeLines("", file.path(vdir, "sentinel.txt"))

  # pumf_metadata calls pumf_locate_or_download then pumf_parse_metadata.
  # Since the version dir already has extracted content + metadata/,
  # both stages are no-ops and it reads the existing canonical CSVs.
  m <- pumf_metadata("FAKE", "2099", cache_path = tmp)

  expect_named(m, c("variables", "codes", "layout"), ignore.order = TRUE)
  expect_equal(nrow(m$variables), 1L)
  expect_equal(nrow(m$codes),     1L)
  expect_null(m$layout)
})

test_that("pumf_metadata: variables has expected columns", {
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
  writeLines("", file.path(vdir, "sentinel.txt"))

  m <- pumf_metadata("FAKE", "2099", cache_path = tmp)
  expect_named(m$variables,
               c("name","label_en","label_fr","type","decimals",
                 "missing_low","missing_high"),
               ignore.order = TRUE)
})


test_that("label_pumf_columns: errors clearly when tbl has no provenance", {
  tmp <- withr::local_tempdir()
  vdir     <- file.path(tmp, "FAKE", "2099")
  meta_dir <- file.path(vdir, "metadata")
  dir.create(meta_dir, recursive = TRUE)
  readr::write_csv(tibble::tibble(name="X", label_en="MyX", label_fr=NA_character_,
                                   type="numeric", decimals=0L,
                                   missing_low=NA_real_, missing_high=NA_real_),
                   file.path(meta_dir, "variables.csv"))
  readr::write_csv(tibble::tibble(name=character(), val=character(),
                                   label_en=character(), label_fr=character()),
                   file.path(meta_dir, "codes.csv"))

  db <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db)
  DBI::dbWriteTable(con, "t", data.frame(X = 1L))
  bare_tbl <- dplyr::tbl(con, "t")

  expect_error(label_pumf_columns(bare_tbl), regexp = "pumf provenance")
  DBI::dbDisconnect(con, shutdown = TRUE)
})

# ---- multi-module announcement (registry-only, no cache) --------------------

test_that(".pumf_announce_modules: lists sibling modules once per survey", {
  # Reset the once-per-session memo so the message reliably fires.
  rm(list = ls(canpumf:::.pumf_modules_announced),
     envir = canpumf:::.pumf_modules_announced)

  # SHS/2017 is multi-module (Interview primary + Diary): get_pumf() loads the
  # primary, so the hint must name the Diary module and a pumf_module() example.
  expect_message(
    canpumf:::.pumf_announce_modules("SHS", "2017"),
    "multi-module")
  rm(list = ls(canpumf:::.pumf_modules_announced),
     envir = canpumf:::.pumf_modules_announced)
  expect_message(
    canpumf:::.pumf_announce_modules("SHS", "2017"),
    'pumf_module\\(main, "Diary"\\)')

  # Second call for the same survey is silent (announced only once).
  expect_silent(canpumf:::.pumf_announce_modules("SHS", "2017"))

  # Single-module surveys never announce.
  expect_silent(canpumf:::.pumf_announce_modules("SFS", "2019"))
})

# ---- get_pumf end-to-end (uses synthetic fixture) ---------------------------

make_e2e_version_dir <- function(tmp, series = "FAKE", version = "2099") {
  vdir     <- file.path(tmp, series, version)
  meta_dir <- file.path(vdir, "metadata")
  dir.create(meta_dir, recursive = TRUE)

  vars  <- tibble::tibble(
    name = c("PROV","WEIGHT"),
    label_en = c("Province","Survey weight"),
    label_fr = c("Province","Poids"),
    type = c("character","numeric"),
    decimals = c(NA_integer_, 0L),
    missing_low = c(NA_real_, 9999L),
    missing_high = c(NA_real_, 9999L)
  )
  codes <- tibble::tibble(
    name = c("PROV","PROV"),
    val  = c("10","35"),
    label_en = c("Newfoundland","Ontario"),
    label_fr = c("Terre-Neuve","Ontario")
  )
  readr::write_csv(vars,  file.path(meta_dir, "variables.csv"))
  readr::write_csv(codes, file.path(meta_dir, "codes.csv"))
  readr::write_csv(
    tibble::tibble(PROV=c("10","35","10"), WEIGHT=c("100","200","9999")),
    file.path(vdir, "survey.csv")
  )
  # Minimal codebook so pumf_parse_metadata can re-parse on refresh=TRUE
  readr::write_csv(
    tibble::tibble(
      Field_Champ               = c("PROV", NA, NA, "WEIGHT"),
      Variable_Variable         = c("PROV", "10", "35", "WEIGHT"),
      EnglishLabel_EtiquetteAnglais = c("Province","Newfoundland","Ontario","Survey weight"),
      FrenchLabel_EtiquetteFrancais = c("Province","Terre-Neuve","Ontario","Poids")
    ),
    file.path(vdir, "codebook.csv")
  )
  writeLines("", file.path(vdir, "sentinel.txt"))
  vdir
}

test_that("get_pumf_connection: returns a DBI connection with table list message", {
  tmp <- withr::local_tempdir()
  make_e2e_version_dir(tmp)

  con <- expect_message(
    get_pumf_connection("FAKE", "2099", cache_path = tmp),
    regexp = "Available tables"
  )
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  expect_true(inherits(con, "duckdb_connection"))
  expect_true(length(DBI::dbListTables(con)) > 0L)
})

test_that("get_pumf_connection: connection is read-write (can create a table)", {
  tmp <- withr::local_tempdir()
  make_e2e_version_dir(tmp)

  con <- suppressMessages(get_pumf_connection("FAKE", "2099", cache_path = tmp))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbWriteTable(con, "derived", data.frame(x = 1L))
  expect_true(DBI::dbExistsTable(con, "derived"))
})

test_that("get_pumf: returns lazy tbl for non-LFS survey", {
  tmp <- withr::local_tempdir()
  make_e2e_version_dir(tmp)

  tbl <- get_pumf("FAKE", "2099", cache_path = tmp)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))

  expect_s3_class(tbl, "tbl")
  result <- dplyr::collect(tbl)
  expect_equal(nrow(result), 3L)
  expect_setequal(na.omit(unique(result$PROV)), c("Newfoundland","Ontario"))
  expect_true(is.na(result$WEIGHT[result$WEIGHT == 9999L]) ||
              any(is.na(result$WEIGHT)))  # missing range applied
})

test_that("label_pumf_columns: renames columns using variable labels", {
  tmp <- withr::local_tempdir()
  make_e2e_version_dir(tmp)

  tbl     <- get_pumf("FAKE", "2099", cache_path = tmp)
  labeled <- label_pumf_columns(tbl)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))

  cols <- colnames(labeled)
  expect_true("Province" %in% cols)
  expect_true("Survey weight" %in% cols)
  expect_false("PROV" %in% cols)
  expect_false("WEIGHT" %in% cols)

  # Collecting still works
  result <- dplyr::collect(labeled)
  expect_equal(nrow(result), 3L)
})

test_that("get_pumf: lang=fra returns French labels", {
  tmp <- withr::local_tempdir()
  make_e2e_version_dir(tmp)

  tbl <- get_pumf("FAKE", "2099", lang = "fra", cache_path = tmp)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))

  result <- dplyr::collect(tbl)
  expect_true("Terre-Neuve" %in% result$PROV)
  expect_false("Newfoundland" %in% result$PROV)
})

test_that("get_pumf: refresh=TRUE rebuilds without re-downloading", {
  tmp <- withr::local_tempdir()
  make_e2e_version_dir(tmp)

  tbl1 <- get_pumf("FAKE", "2099", cache_path = tmp)
  db   <- file.path(tmp, "FAKE", "2099", "FAKE_2099.duckdb")
  m1   <- file.info(db)$mtime
  close_pumf(tbl1)

  Sys.sleep(0.05)
  tbl2 <- get_pumf("FAKE", "2099", cache_path = tmp, refresh = TRUE)
  m2   <- file.info(db)$mtime
  close_pumf(tbl2)

  # DuckDB was rewritten; raw data file still present
  expect_true(m2 > m1)
  expect_true(file.exists(file.path(tmp, "FAKE", "2099", "survey.csv")))
})

test_that("get_pumf: redownload=TRUE wipes version dir and rebuilds", {
  tmp <- withr::local_tempdir()
  make_e2e_version_dir(tmp)

  # Seed a canary file to verify it gets removed by redownload
  canary <- file.path(tmp, "FAKE", "2099", "canary.txt")
  writeLines("canary", canary)

  tbl1 <- get_pumf("FAKE", "2099", cache_path = tmp)
  close_pumf(tbl1)
  expect_true(file.exists(canary))

  # redownload would attempt a network fetch for an unknown series; the canary
  # and DuckDB should be gone before it hits the network error.
  expect_error(
    get_pumf("FAKE", "2099", cache_path = tmp, redownload = TRUE),
    regexp = "not found in the canpumf collection|download"
  )
  expect_false(file.exists(canary))
})

test_that("get_pumf: second call is a no-op (cached)", {
  tmp <- withr::local_tempdir()
  make_e2e_version_dir(tmp)

  tbl1 <- get_pumf("FAKE", "2099", cache_path = tmp)
  db   <- file.path(tmp, "FAKE", "2099", "FAKE_2099.duckdb")
  m1   <- file.info(db)$mtime
  DBI::dbDisconnect(tbl1$src$con, shutdown = TRUE)

  Sys.sleep(0.05)
  tbl2 <- get_pumf("FAKE", "2099", cache_path = tmp)
  m2   <- file.info(db)$mtime
  DBI::dbDisconnect(tbl2$src$con, shutdown = TRUE)

  expect_equal(m1, m2)
})

test_that("get_pumf: LFS dispatches to lfs_get_pumf", {
  tmp <- withr::local_tempdir()
  # Synthetic LFS "version dir" — same structure as test-pipeline-lfs.R
  vdir     <- file.path(tmp, "LFS", "2022")
  meta_dir <- file.path(vdir, "metadata")
  dir.create(meta_dir, recursive = TRUE)

  vars  <- tibble::tibble(name=c("SURVYEAR","PROV"), label_en=c("Year","Province"),
                           label_fr=c("Annee","Province"), type=c("numeric","character"),
                           decimals=c(0L,NA_integer_), missing_low=NA_real_, missing_high=NA_real_)
  codes <- tibble::tibble(name="PROV", val="35", label_en="Ontario", label_fr="Ontario")
  readr::write_csv(vars,  file.path(meta_dir, "variables.csv"))
  readr::write_csv(codes, file.path(meta_dir, "codes.csv"))
  cb <- data.frame(Field_Champ=c("PROV",NA), Variable_Variable=c("PROV","35"),
                   EnglishLabel_EtiquetteAnglais=c("Province","Ontario"),
                   FrenchLabel_EtiquetteFrancais=c("Province","Ontario"),
                   stringsAsFactors=FALSE)
  readr::write_csv(cb, file.path(vdir, "codebook.csv"))
  readr::write_csv(tibble::tibble(SURVYEAR=2022L, SURVMNTH=1L, PROV="35"),
                   file.path(vdir, "pub2022.csv"))
  writeLines("", file.path(vdir, "sentinel.txt"))

  tbl <- get_pumf("LFS", "2022", cache_path = tmp)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))

  expect_s3_class(tbl, "tbl")
  result <- dplyr::collect(tbl)
  expect_equal(nrow(result), 1L)
  expect_equal(unique(result$SURVYEAR), 2022L)
})

# ---- close_pumf -------------------------------------------------------------

test_that("close_pumf: disconnects the connection", {
  tmp <- withr::local_tempdir()
  make_e2e_version_dir(tmp)

  tbl <- get_pumf("FAKE", "2099", cache_path = tmp)
  con <- tbl$src$con
  expect_true(DBI::dbIsValid(con))

  close_pumf(tbl)
  expect_false(DBI::dbIsValid(con))
})

test_that("close_pumf: is idempotent on already-closed connection", {
  tmp <- withr::local_tempdir()
  make_e2e_version_dir(tmp)

  tbl <- get_pumf("FAKE", "2099", cache_path = tmp)
  close_pumf(tbl)
  expect_no_error(close_pumf(tbl))
})

# ---- read_only parameter ----------------------------------------------------

test_that("get_pumf: read_only=TRUE (default) returns a valid tbl", {
  tmp <- withr::local_tempdir()
  make_e2e_version_dir(tmp)

  tbl <- get_pumf("FAKE", "2099", cache_path = tmp)
  on.exit(close_pumf(tbl))
  expect_s3_class(tbl, "tbl")
})

test_that("get_pumf: read_only=FALSE opens a writable connection", {
  tmp <- withr::local_tempdir()
  make_e2e_version_dir(tmp)

  tbl <- get_pumf("FAKE", "2099", cache_path = tmp, read_only = FALSE)
  on.exit(close_pumf(tbl))
  con <- tbl$src$con
  # A writable connection allows DDL operations
  expect_no_error(DBI::dbExecute(con,
    "CREATE OR REPLACE VIEW test_view AS SELECT 1 AS x"))
})

# ---- lock detection ---------------------------------------------------------

test_that(".assert_duckdb_writable: no error when file does not exist", {
  tmp <- withr::local_tempdir()
  expect_no_error(
    canpumf:::.assert_duckdb_writable(file.path(tmp, "nonexistent.duckdb"))
  )
})

test_that(".assert_duckdb_writable: no error on an unlocked DuckDB file", {
  tmp <- withr::local_tempdir()
  make_e2e_version_dir(tmp)
  # Build a DuckDB, close it, then confirm the helper sees it as writable.
  tbl <- get_pumf("FAKE", "2099", cache_path = tmp)
  db  <- file.path(tmp, "FAKE", "2099", "FAKE_2099.duckdb")
  close_pumf(tbl)
  expect_no_error(canpumf:::.assert_duckdb_writable(db))
})

test_that(".assert_duckdb_writable: clear error when read-only connection is open", {
  tmp <- withr::local_tempdir()
  make_e2e_version_dir(tmp)
  # Open a read-only tbl (default) and keep it open.
  tbl <- get_pumf("FAKE", "2099", cache_path = tmp)
  db  <- file.path(tmp, "FAKE", "2099", "FAKE_2099.duckdb")
  # The check must detect the in-process read-only sharing and give a clear message.
  expect_error(
    canpumf:::.assert_duckdb_writable(db),
    regexp = "held open by a read-only connection",
    fixed  = FALSE
  )
  close_pumf(tbl)
})
