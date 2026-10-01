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

test_that("close_pumf: closes a raw DuckDB connection (get_pumf_connection)", {
  tmp <- withr::local_tempdir()
  make_e2e_version_dir(tmp)

  # get_pumf_connection() hands back a DBIConnection, not a tbl; close_pumf()
  # must accept it directly even though it was never registered by get_pumf().
  con <- suppressMessages(get_pumf_connection("FAKE", "2099", cache_path = tmp))
  expect_true(inherits(con, "DBIConnection"))
  expect_true(DBI::dbIsValid(con))

  expect_no_error(close_pumf(con))
  expect_false(DBI::dbIsValid(con))
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

# ---- read path never takes a write lock (issue #18) -------------------------

test_that("get_pumf: cache hit succeeds when the DuckDB file is not writable", {
  skip_on_os("windows")
  tmp <- withr::local_tempdir()
  make_e2e_version_dir(tmp)

  tbl1 <- get_pumf("FAKE", "2099", cache_path = tmp)
  close_pumf(tbl1)

  # A write-protected file stands in for "someone else holds the write lock"
  # (the notebook-render scenario: the render process must be able to read
  # while the interactive session's connections are open).  Any attempt to
  # open read-write fails on this file, so a cache hit only passes if the
  # whole read path stays read-only.
  db <- file.path(tmp, "FAKE", "2099", "FAKE_2099.duckdb")
  Sys.chmod(db, "0444")
  on.exit(Sys.chmod(db, "0644"), add = TRUE)

  tbl2 <- get_pumf("FAKE", "2099", cache_path = tmp)
  on.exit(close_pumf(tbl2), add = TRUE)
  expect_equal(nrow(dplyr::collect(tbl2)), 3L)
})

test_that("get_pumf: default connection refuses writes", {
  tmp <- withr::local_tempdir()
  make_e2e_version_dir(tmp)

  tbl <- get_pumf("FAKE", "2099", cache_path = tmp)
  on.exit(close_pumf(tbl))
  expect_error(
    DBI::dbExecute(tbl$src$con, "CREATE TABLE should_fail (x INTEGER)"),
    regexp = "read[ _-]only"
  )
})

test_that("get_pumf: cache hit leaves an already-open tbl on the same file valid", {
  tmp <- withr::local_tempdir()
  make_e2e_version_dir(tmp)

  tbl1 <- get_pumf("FAKE", "2099", cache_path = tmp)
  tbl2 <- get_pumf("FAKE", "2099", cache_path = tmp)
  on.exit({ close_pumf(tbl2); close_pumf(tbl1) }, add = TRUE)

  expect_equal(nrow(dplyr::collect(tbl1)), 3L)
  expect_equal(nrow(dplyr::collect(tbl2)), 3L)
})

test_that("get_pumf: reads share a read-write connection the session holds", {
  tmp <- withr::local_tempdir()
  make_e2e_version_dir(tmp)

  # duckdb >= 1.5.6 refuses read_only = TRUE on a file whose in-process
  # instance is read-write; the read path then shares that instance.
  rw <- get_pumf("FAKE", "2099", cache_path = tmp, read_only = FALSE)
  on.exit(close_pumf(rw), add = TRUE)
  db <- file.path(tmp, "FAKE", "2099", "FAKE_2099.duckdb")

  expect_true(canpumf:::.duckdb_table_exists(db, "eng"))
  expect_false(is.na(list_pumf_cache(cache_path = tmp)$built_with))
  tbl <- get_pumf("FAKE", "2099", cache_path = tmp)
  expect_equal(nrow(dplyr::collect(tbl)), 3L)
  expect_equal(nrow(dplyr::collect(rw)), 3L)
  expect_no_error(DBI::dbExecute(rw$src$con,
    "CREATE OR REPLACE VIEW test_view AS SELECT 1 AS x"))
})

test_that(".is_duckdb_read_only_mismatch: recognises duckdb's wording only", {
  mismatch <- simpleError(paste0(
    "`read_only` can't be applied to the database instance for ",
    "`/tmp/FAKE_2099.duckdb`, which already exists."))
  expect_true(canpumf:::.is_duckdb_read_only_mismatch(mismatch))
  expect_false(canpumf:::.is_duckdb_read_only_mismatch(
    simpleError("IO Error: Could not set lock on file")))
  expect_false(canpumf:::.is_duckdb_read_only_mismatch(simpleError(paste0(
    "`config$threads` can't be applied to the database instance for ",
    "`/tmp/FAKE_2099.duckdb`, which already exists."))))
})

test_that("get_pumf: read_only = FALSE cannot write while a tbl holds the file", {
  tmp <- withr::local_tempdir()
  make_e2e_version_dir(tmp)

  ro <- get_pumf("FAKE", "2099", cache_path = tmp)
  on.exit(close_pumf(ro), add = TRUE)
  # duckdb >= 1.5.6 fails in dbConnect(), reported with the close_pumf()
  # message; earlier versions hand back the read-only instance.
  rw <- tryCatch(get_pumf("FAKE", "2099", cache_path = tmp, read_only = FALSE),
                 canpumf_read_only_held = function(e) e)
  if (inherits(rw, "error")) {
    expect_match(conditionMessage(rw), "close_pumf")
  } else {
    expect_error(DBI::dbExecute(rw$src$con, "CREATE TABLE should_fail (x INTEGER)"),
                 regexp = "read[ _-]only")
  }
  expect_equal(nrow(dplyr::collect(ro)), 3L)
})

test_that("add_bootstrap_weights: clear error when another tbl holds the file", {
  tmp <- withr::local_tempdir()
  make_e2e_version_dir(tmp)

  tbl   <- get_pumf("FAKE", "2099", cache_path = tmp)
  other <- get_pumf("FAKE", "2099", cache_path = tmp)
  on.exit(close_pumf(other), add = TRUE)

  expect_error(
    suppressWarnings(suppressMessages(
      add_bootstrap_weights(tbl, weight_col = "WEIGHT", n_replicates = 4L))),
    regexp = "held open by a read-only connection|locked by an open connection"
  )
})


# ---- pumf_sentinels ----------------------------------------------------------

# A DuckDB with a labelled main table and its sentinel companion, registered
# with provenance as get_pumf() would.
.sentinel_db <- function(with_companion = TRUE, env = parent.frame()) {
  cache <- withr::local_tempdir(.local_envir = env)
  s <- list(cache = cache, series = "SENT", version = "2099", lang = "eng")
  s$db_path <- .pumf_db_path(s$series, s$version, cache)
  dir.create(dirname(s$db_path), recursive = TRUE, showWarnings = FALSE)
  s$tname <- .pumf_table_name(s$series, s$version, s$lang)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = s$db_path)
  DBI::dbWriteTable(con, s$tname,
    data.frame(pumf_row_id = 1:4, INC = c(10, NA, NA, 40), HRS = c(NA, 2, 3, 4)))
  if (with_companion)
    DBI::dbWriteTable(con, .sentinel_table_name(s$tname),
      data.frame(pumf_row_id = c(1L, 2L, 3L),
                 INC = factor(c(NA, "Not available", "Not applicable")),
                 HRS = factor(c("Not applicable", NA, NA))))
  DBI::dbDisconnect(con, shutdown = TRUE)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = s$db_path, read_only = TRUE)
  .pumf_register_con(con, s$series, s$version, cache, s$lang)
  dplyr::tbl(con, s$tname)
}

test_that("pumf_sentinels: returns the companion table on the same connection", {
  t <- .sentinel_db()
  on.exit(close_pumf(t))
  sent <- pumf_sentinels(t)
  expect_s3_class(sent, "tbl_sql")
  expect_identical(sent$src$con, t$src$con)
  d <- dplyr::collect(dplyr::arrange(sent, pumf_row_id))
  expect_equal(d$pumf_row_id, c(1, 2, 3))
  expect_equal(as.character(d$INC), c(NA, "Not available", "Not applicable"))
})

test_that("pumf_sentinels: join = TRUE suffixes the sentinel columns", {
  t <- .sentinel_db()
  on.exit(close_pumf(t))
  j <- dplyr::collect(dplyr::arrange(pumf_sentinels(t, join = TRUE), pumf_row_id))
  expect_true(all(c("INC", "INC_sentinel", "HRS", "HRS_sentinel") %in% names(j)))
  expect_equal(nrow(j), 4L)
  expect_equal(j$INC, c(10, NA, NA, 40))
  expect_equal(as.character(j$INC_sentinel),
               c(NA, "Not available", "Not applicable", NA))
  # composes with dplyr verbs applied first
  f <- t |> dplyr::filter(is.na(INC)) |> pumf_sentinels(join = TRUE) |>
    dplyr::count(INC_sentinel) |> dplyr::collect()
  expect_equal(nrow(f), 2L)
})

test_that("pumf_sentinels: join = TRUE needs pumf_row_id in the tbl", {
  t <- .sentinel_db()
  on.exit(close_pumf(t))
  expect_error(pumf_sentinels(dplyr::select(t, INC), join = TRUE), "pumf_row_id")
})

test_that("pumf_sentinels: errors on a table built without a companion", {
  t <- .sentinel_db(with_companion = FALSE)
  on.exit(close_pumf(t))
  expect_error(pumf_sentinels(t), "refresh = TRUE")
})

test_that("pumf_sentinels: errors on a data.frame or a tbl without provenance", {
  expect_error(pumf_sentinels(data.frame(x = 1)), "lazy tbl")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "t", data.frame(X = 1L))
  expect_error(pumf_sentinels(dplyr::tbl(con, "t")), "provenance")
})

test_that("pumf_sentinels: refuses the longitudinal series", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "lfs_eng", data.frame(X = 1L))
  .pumf_register_con(con, "LFS", "2024-01", tempdir(), "eng")
  expect_error(pumf_sentinels(dplyr::tbl(con, "lfs_eng")), "longitudinal")
})


# ---- build stamp: tables built before 0.6.1 ---------------------------------

# A table with no pumf_build_info row is one built before 0.6.1.  get_pumf()
# says so once per session and table, through .pumf_check_build_stamp().
test_that(".pumf_check_build_stamp: speaks once per table, never for a stamped one", {
  t <- .sentinel_db(with_companion = FALSE)   # written without a stamp
  on.exit(close_pumf(t), add = TRUE)
  con <- t$src$con
  args <- list(con, "SENT", "2099", "eng", "eng", "/tmp/stamp-a.duckdb")
  msg <- NULL
  withCallingHandlers(
    spoke <- do.call(.pumf_check_build_stamp, args),
    message = function(m) { msg <<- conditionMessage(m); invokeRestart("muffleMessage") })
  expect_true(spoke)
  expect_match(msg, "SENT 2099 \\[eng\\] was built by canpumf before 0.6.1")
  expect_match(msg, "codes that share a label are merged")
  expect_match(msg, "refresh = TRUE.*list_pumf_cache\\(\\).*canpumf.stale_cache_message")
  # the second time the same table is opened it is silent
  expect_silent(again <- do.call(.pumf_check_build_stamp, args))
  expect_false(again)
  # another table of another database speaks again
  expect_message(.pumf_check_build_stamp(con, "SENT", "2099", "fra", "fra",
                                         "/tmp/stamp-b.duckdb"),
                 "SENT 2099 \\[fra\\]")
  # a stamped table without metadata/codes_applied.csv was built by a 0.6.1
  # development version before value labels were made unique: it speaks once
  vdir <- withr::local_tempdir()
  db3  <- file.path(vdir, "stamped.duckdb")
  wcon <- DBI::dbConnect(duckdb::duckdb(), dbdir = db3)
  for (tab in c("eng", "fra")) {
    DBI::dbWriteTable(wcon, tab, data.frame(pumf_row_id = 1:2, X = 1:2))
    .write_build_info(wcon, tab)
  }
  DBI::dbDisconnect(wcon, shutdown = TRUE)
  rcon <- DBI::dbConnect(duckdb::duckdb(), dbdir = db3, read_only = TRUE)
  on.exit(DBI::dbDisconnect(rcon, shutdown = TRUE), add = TRUE)
  expect_message(res <- .pumf_check_build_stamp(rcon, "S", "v", "eng", "eng", db3),
                 "S v \\[eng\\] was built by a canpumf 0.6.1 development version.*refresh = TRUE")
  expect_true(res)
  # a stamped table with the side-car is silent (a fresh key: another table)
  dir.create(file.path(vdir, "metadata"))
  .write_codes_applied(data.frame(name = "X", val = "1", label_en = "a", label_fr = "a"),
                       file.path(vdir, "metadata"))
  expect_silent(res <- .pumf_check_build_stamp(rcon, "S", "v", "fra", "fra", db3))
  expect_false(res)
})

test_that(".pumf_check_build_stamp: options(canpumf.stale_cache_message = FALSE) silences it", {
  withr::local_options(canpumf.stale_cache_message = FALSE)
  t <- .sentinel_db(with_companion = FALSE)
  on.exit(close_pumf(t), add = TRUE)   # add = TRUE keeps withr's option restore
  expect_silent(res <- .pumf_check_build_stamp(t$src$con, "SENT", "2099", "eng",
                                               "eng", "/tmp/stamp-c.duckdb"))
  expect_false(res)
})

# A minimal FAKE/2099 version directory get_pumf() can build without network.
.stamp_version_dir <- function(tmp) {
  vdir <- file.path(tmp, "FAKE", "2099")
  meta_dir <- file.path(vdir, "metadata")
  dir.create(meta_dir, recursive = TRUE)
  readr::write_csv(tibble::tibble(
    name = "X", label_en = "V", label_fr = "V", type = "character",
    decimals = NA_integer_, missing_low = NA_real_, missing_high = NA_real_),
    file.path(meta_dir, "variables.csv"))
  readr::write_csv(tibble::tibble(name = character(), val = character(),
                                  label_en = character(), label_fr = character()),
                   file.path(meta_dir, "codes.csv"))
  readr::write_csv(tibble::tibble(X = c("a", "b")), file.path(vdir, "data.csv"))
  readr::write_csv(tibble::tibble(
    Field_Champ = c("X", NA_character_), Variable_Variable = c("X", "a"),
    EnglishLabel_EtiquetteAnglais = c("Var", "Label a"),
    FrenchLabel_EtiquetteFrancais = c("Var", "Etiq a")),
    file.path(vdir, "codebook.csv"))
  writeLines("", file.path(vdir, "sentinel.txt"))
  vdir
}

test_that("get_pumf: a freshly built table is silent, one without a stamp is announced once", {
  tmp <- withr::local_tempdir()
  .stamp_version_dir(tmp)
  expect_no_message(t <- get_pumf("FAKE", "2099", cache_path = tmp),
                    message = "before 0.6.1")
  db_path <- DBI::dbGetInfo(t$src$con)$dbname
  close_pumf(t)

  # strip the stamp: the table now looks like a pre-0.6.1 build
  wcon <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  DBI::dbRemoveTable(wcon, "pumf_build_info")
  DBI::dbDisconnect(wcon, shutdown = TRUE)
  expect_message(t <- get_pumf("FAKE", "2099", cache_path = tmp),
                 "FAKE 2099 \\[eng\\] was built by canpumf before 0.6.1")
  close_pumf(t)
  expect_no_message(t <- get_pumf("FAKE", "2099", cache_path = tmp),
                    message = "before 0.6.1")
  close_pumf(t)
})
