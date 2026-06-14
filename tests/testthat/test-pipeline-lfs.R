# Tests for the LFS longitudinal pipeline.
#
# Unit tests use synthetic version directories (no network, no real LFS data).
# Integration tests are cache-gated: skip unless the user has LFS data at
# getOption("canpumf.cache_path").

duck_mem <- function() DBI::dbConnect(duckdb::duckdb(), ":memory:")

# ---- Version parsing helpers ------------------------------------------------

test_that(".lfs_version_type: annual", {
  expect_equal(canpumf:::.lfs_version_type("2023"), "annual")
  expect_equal(canpumf:::.lfs_version_type("2006"), "annual")
})

test_that(".lfs_version_type: monthly", {
  expect_equal(canpumf:::.lfs_version_type("2024-06"), "monthly")
  expect_equal(canpumf:::.lfs_version_type("2026-01"), "monthly")
})

test_that(".lfs_version_type: invalid", {
  expect_error(canpumf:::.lfs_version_type("24-6"),   "Invalid")
  expect_error(canpumf:::.lfs_version_type("2024-6"),  "Invalid")
  expect_error(canpumf:::.lfs_version_type("2024"),    NA)  # should NOT error
})

test_that(".lfs_survyear / .lfs_survmnth", {
  expect_equal(canpumf:::.lfs_survyear("2023"),    2023L)
  expect_equal(canpumf:::.lfs_survyear("2024-06"), 2024L)
  expect_true(is.na(canpumf:::.lfs_survmnth("2023")))
  expect_equal(canpumf:::.lfs_survmnth("2024-06"), 6L)
})

# ---- lfs_versions table helpers ---------------------------------------------

test_that(".lfs_ensure_versions_table: creates table", {
  con <- duck_mem()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  canpumf:::.lfs_ensure_versions_table(con)
  expect_true(DBI::dbExistsTable(con, "lfs_versions"))
  fields <- DBI::dbListFields(con, "lfs_versions")
  expect_true(all(c("version","type","survyear","survmnth","n_records") %in% fields))
})

test_that(".lfs_ensure_versions_table: idempotent", {
  con <- duck_mem()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  canpumf:::.lfs_ensure_versions_table(con)
  expect_no_error(canpumf:::.lfs_ensure_versions_table(con))
})

test_that(".lfs_version_exists: FALSE when absent", {
  con <- duck_mem()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  expect_false(canpumf:::.lfs_version_exists(con, "2022"))
})

test_that(".lfs_version_exists: TRUE after insert", {
  con <- duck_mem()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  canpumf:::.lfs_ensure_versions_table(con)
  DBI::dbExecute(con,
    "INSERT INTO lfs_versions VALUES ('2022','annual',2022,NULL,NOW(),100)")
  expect_true(canpumf:::.lfs_version_exists(con, "2022"))
  expect_false(canpumf:::.lfs_version_exists(con, "2023"))
})

test_that(".lfs_has_annual: detects annual entry", {
  con <- duck_mem()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  canpumf:::.lfs_ensure_versions_table(con)
  DBI::dbExecute(con,
    "INSERT INTO lfs_versions VALUES ('2022','annual',2022,NULL,NOW(),100)")
  expect_true(canpumf:::.lfs_has_annual(con, 2022L))
  expect_false(canpumf:::.lfs_has_annual(con, 2021L))
})

test_that(".lfs_monthly_versions: returns monthly entries for year", {
  con <- duck_mem()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  canpumf:::.lfs_ensure_versions_table(con)
  DBI::dbExecute(con,
    "INSERT INTO lfs_versions VALUES ('2024-01','monthly',2024,1,NOW(),50)")
  DBI::dbExecute(con,
    "INSERT INTO lfs_versions VALUES ('2024-02','monthly',2024,2,NOW(),50)")
  DBI::dbExecute(con,
    "INSERT INTO lfs_versions VALUES ('2023','annual',2023,NULL,NOW(),100)")

  result <- canpumf:::.lfs_monthly_versions(con, 2024L)
  expect_setequal(result, c("2024-01", "2024-02"))
  expect_equal(canpumf:::.lfs_monthly_versions(con, 2023L), character(0L))
})

# ---- .lfs_append (schema evolution) ----------------------------------------

test_that(".lfs_append: creates table on first call", {
  con <- duck_mem()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  df <- data.frame(SURVYEAR = 2022L, PROV = "Ontario")
  canpumf:::.lfs_append(con, "lfs_eng", df)
  expect_true(DBI::dbExistsTable(con, "lfs_eng"))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM lfs_eng")$n, 1L)
})

test_that(".lfs_append: new column added with NULLs for old rows", {
  con <- duck_mem()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  df1 <- data.frame(SURVYEAR = 2022L, PROV = "Ontario")
  df2 <- data.frame(SURVYEAR = 2023L, PROV = "Quebec", NEWVAR = 1L)

  canpumf:::.lfs_append(con, "lfs_eng", df1)
  canpumf:::.lfs_append(con, "lfs_eng", df2)

  result <- DBI::dbGetQuery(con, "SELECT * FROM lfs_eng ORDER BY SURVYEAR")
  expect_equal(nrow(result), 2L)
  expect_true("NEWVAR" %in% names(result))
  expect_true(is.na(result$NEWVAR[result$SURVYEAR == 2022]))
  expect_equal(result$NEWVAR[result$SURVYEAR == 2023], 1L)
})

test_that(".lfs_append: VARCHAR column upgraded to DOUBLE when numeric data appended", {
  con <- duck_mem()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # Simulate old state: FINALWT was incorrectly stored as VARCHAR
  df_old <- data.frame(SURVYEAR = 2022L, FINALWT = "1000", stringsAsFactors = FALSE)
  canpumf:::.lfs_append(con, "lfs_eng", df_old)

  schema_before <- DBI::dbGetQuery(con,
    "SELECT data_type FROM information_schema.columns WHERE table_name='lfs_eng' AND column_name='FINALWT'")
  expect_true(grepl("VARCHAR|CHAR", schema_before$data_type, ignore.case = TRUE))

  # Now append corrected data where FINALWT is numeric
  df_new <- data.frame(SURVYEAR = 2023L, FINALWT = 2000.0)
  expect_message(
    canpumf:::.lfs_append(con, "lfs_eng", df_new),
    regexp = "Upgraded.*FINALWT.*VARCHAR.*DOUBLE", ignore.case = TRUE
  )

  schema_after <- DBI::dbGetQuery(con,
    "SELECT data_type FROM information_schema.columns WHERE table_name='lfs_eng' AND column_name='FINALWT'")
  expect_false(grepl("VARCHAR|CHAR", schema_after$data_type, ignore.case = TRUE))

  result <- DBI::dbGetQuery(con, "SELECT FINALWT FROM lfs_eng ORDER BY SURVYEAR")
  expect_true(is.numeric(result$FINALWT))
})

test_that(".lfs_append: DOUBLE column upgraded to INTEGER when integer data appended", {
  con <- duck_mem()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # Simulate old state: REC_NUM was incorrectly stored as DOUBLE
  df_old <- data.frame(SURVYEAR = 2022L, REC_NUM = 1.0)
  canpumf:::.lfs_append(con, "lfs_eng", df_old)

  schema_before <- DBI::dbGetQuery(con,
    "SELECT data_type FROM information_schema.columns WHERE table_name='lfs_eng' AND column_name='REC_NUM'")
  expect_equal(schema_before$data_type, "DOUBLE")

  # Append with REC_NUM as integer — should trigger DOUBLE → INTEGER upgrade
  df_new <- data.frame(SURVYEAR = 2023L, REC_NUM = 2L)
  expect_message(
    canpumf:::.lfs_append(con, "lfs_eng", df_new),
    regexp = "Upgraded.*REC_NUM.*DOUBLE.*INTEGER", ignore.case = TRUE
  )

  schema_after <- DBI::dbGetQuery(con,
    "SELECT data_type FROM information_schema.columns WHERE table_name='lfs_eng' AND column_name='REC_NUM'")
  expect_equal(schema_after$data_type, "INTEGER")

  result <- DBI::dbGetQuery(con, "SELECT REC_NUM FROM lfs_eng ORDER BY SURVYEAR")
  expect_true(is.integer(result$REC_NUM))
})

test_that(".lfs_append: missing column in new data filled with NA", {
  con <- duck_mem()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  df1 <- data.frame(SURVYEAR = 2022L, PROV = "ON", OLDVAR = 5L)
  df2 <- data.frame(SURVYEAR = 2023L, PROV = "QC")

  canpumf:::.lfs_append(con, "lfs_eng", df1)
  canpumf:::.lfs_append(con, "lfs_eng", df2)

  result <- DBI::dbGetQuery(con, "SELECT * FROM lfs_eng ORDER BY SURVYEAR")
  expect_true(is.na(result$OLDVAR[result$SURVYEAR == 2023]))
})

# ---- .lfs_delete_year -------------------------------------------------------

test_that(".lfs_delete_year: removes rows and lfs_versions entry", {
  con <- duck_mem()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  canpumf:::.lfs_ensure_versions_table(con)
  df <- data.frame(SURVYEAR = c(2022L, 2022L, 2023L), PROV = c("ON","QC","AB"))
  canpumf:::.lfs_append(con, "lfs_eng", df)
  DBI::dbExecute(con,
    "INSERT INTO lfs_versions VALUES ('2022','annual',2022,NULL,NOW(),2)")

  canpumf:::.lfs_delete_year(con, "lfs_eng", 2022L)

  n_data <- DBI::dbGetQuery(con,
    "SELECT COUNT(*) AS n FROM lfs_eng WHERE SURVYEAR=2022")$n
  n_ver  <- DBI::dbGetQuery(con,
    "SELECT COUNT(*) AS n FROM lfs_versions WHERE survyear=2022")$n
  expect_equal(n_data, 0L)
  expect_equal(n_ver,  0L)
  # 2023 row untouched
  expect_equal(DBI::dbGetQuery(con,
    "SELECT COUNT(*) AS n FROM lfs_eng WHERE SURVYEAR=2023")$n, 1L)
})

# ---- .lfs_delete_month ------------------------------------------------------

test_that(".lfs_delete_month: removes only the target month and version", {
  con <- duck_mem()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  canpumf:::.lfs_ensure_versions_table(con)
  df <- data.frame(
    SURVYEAR = c(2025L, 2025L, 2025L),
    SURVMNTH = c(1L,    2L,    3L),
    PROV     = c("ON",  "QC",  "AB"))
  canpumf:::.lfs_append(con, "lfs_eng", df)
  DBI::dbExecute(con,
    "INSERT INTO lfs_versions VALUES ('2025-01','monthly',2025,1,NOW(),1)")
  DBI::dbExecute(con,
    "INSERT INTO lfs_versions VALUES ('2025-02','monthly',2025,2,NOW(),1)")
  DBI::dbExecute(con,
    "INSERT INTO lfs_versions VALUES ('2025-03','monthly',2025,3,NOW(),1)")

  canpumf:::.lfs_delete_month(con, "lfs_eng", "2025-02", 2025L, 2L)

  # Only February data removed
  months <- DBI::dbGetQuery(con,
    "SELECT SURVMNTH FROM lfs_eng WHERE SURVYEAR=2025 ORDER BY SURVMNTH")$SURVMNTH
  expect_equal(months, c(1L, 3L))

  # Only the 2025-02 version record removed
  vers <- DBI::dbGetQuery(con,
    "SELECT version FROM lfs_versions WHERE survyear=2025 ORDER BY version")$version
  expect_equal(vers, c("2025-01", "2025-03"))
})

# ---- Synthetic end-to-end: lfs_get_pumf -------------------------------------

# Build a minimal fake LFS version directory
make_lfs_vdir <- function(cache_dir, version,
                           extra_col = FALSE, n_rows = 3L) {
  vdir     <- file.path(cache_dir, "LFS", version)
  meta_dir <- file.path(vdir, "metadata")
  dir.create(meta_dir, recursive = TRUE)

  survyear <- canpumf:::.lfs_survyear(version)
  survmnth <- canpumf:::.lfs_survmnth(version)
  if (is.na(survmnth)) survmnth <- 1L

  # --- Canonical metadata (already-parsed) ---
  variables <- tibble::tibble(
    name = c("SURVYEAR","SURVMNTH","PROV"),
    label_en = c("Year","Month","Province"),
    label_fr = c("Annee","Mois","Province"),
    type = c("numeric","numeric","character"),
    decimals = c(0L, 0L, NA_integer_),
    missing_low  = NA_real_, missing_high = NA_real_
  )
  codes <- tibble::tibble(
    name = c("PROV","PROV"),
    val  = c("10","35"),
    label_en = c("Newfoundland","Ontario"),
    label_fr = c("Terre-Neuve","Ontario")
  )
  if (extra_col) {
    variables <- tibble::add_row(variables,
      name="EXTRA", label_en="Extra var", label_fr="Extra",
      type="numeric", decimals=0L,
      missing_low=NA_real_, missing_high=NA_real_)
  }
  readr::write_csv(variables, file.path(meta_dir, "variables.csv"))
  readr::write_csv(codes,     file.path(meta_dir, "codes.csv"))

  # --- Raw codebook.csv so pumf_parse_metadata(refresh=TRUE) can re-parse ---
  # Minimal LFS codebook format: Field_Champ / Variable_Variable / labels
  prov_codes <- if (!extra_col)
    c("PROV","10","35") else c("PROV","10","35")
  cb_rows <- data.frame(
    Field_Champ               = c("PROV", NA, NA),
    Variable_Variable         = c("PROV", "10", "35"),
    EnglishLabel_EtiquetteAnglais = c("Province", "Newfoundland", "Ontario"),
    FrenchLabel_EtiquetteFrancais = c("Province", "Terre-Neuve", "Ontario"),
    stringsAsFactors = FALSE
  )
  readr::write_csv(cb_rows, file.path(vdir, "codebook.csv"))

  # --- Synthetic data file (LFS pattern: pub####.csv) ---
  data <- tibble::tibble(
    SURVYEAR = rep(survyear, n_rows),
    SURVMNTH = rep(survmnth, n_rows),
    PROV     = rep(c("10","35","10"), length.out = n_rows)
  )
  if (extra_col) data$EXTRA <- 42L
  readr::write_csv(data, file.path(vdir, "pub2022.csv"))

  # Sentinel "extracted" file so Stage 1 skips re-extraction
  writeLines("", file.path(vdir, "sentinel.txt"))

  vdir
}

# ---- .lfs_file_format -------------------------------------------------------

test_that("lfs_file_format: annual files (first 2 digits > 12)", {
  expect_equal(canpumf:::.lfs_file_format("pub2023.csv"),  "annual")
  expect_equal(canpumf:::.lfs_file_format("pub2016.csv"),  "annual")
  expect_equal(canpumf:::.lfs_file_format("PUB2021.CSV"),  "annual")   # case
  expect_equal(canpumf:::.lfs_file_format("/path/to/pub2023.csv"), "annual")
})

test_that("lfs_file_format: monthly files (first 2 digits <= 12)", {
  expect_equal(canpumf:::.lfs_file_format("pub0120.csv"),  "monthly")  # Jan 2020
  expect_equal(canpumf:::.lfs_file_format("pub1223.csv"),  "monthly")  # Dec 2023
  expect_equal(canpumf:::.lfs_file_format("pub0124.csv"),  "monthly")  # Jan 2024
  expect_equal(canpumf:::.lfs_file_format("pub012024.csv"), "monthly") # Jan 2024, long year
})

test_that("lfs_file_format: non-pub files return 'unknown'", {
  expect_equal(canpumf:::.lfs_file_format("codebook.csv"),     "unknown")
  expect_equal(canpumf:::.lfs_file_format("lfs2023.csv"),      "unknown")
  expect_equal(canpumf:::.lfs_file_format("pub_readme.txt"),   "unknown")
})

# ---- .lfs_find_data_files ---------------------------------------------------

.make_lfs_raw_dir <- function(dir, files) {
  # Helper: create a bare version directory containing the given file names
  for (f in files) writeLines("", file.path(dir, f))
  dir.create(file.path(dir, "metadata"), showWarnings = FALSE)
  invisible(dir)
}

test_that("lfs_find_data_files: single annual file", {
  d <- withr::local_tempdir()
  .make_lfs_raw_dir(d, c("pub2023.csv", "codebook.csv", "sentinel.txt"))
  found <- canpumf:::.lfs_find_data_files(d)
  expect_equal(basename(found), "pub2023.csv")
})

test_that("lfs_find_data_files: 12 monthly files", {
  d <- withr::local_tempdir()
  monthly <- sprintf("pub%02d20.csv", 1:12)
  .make_lfs_raw_dir(d, c(monthly, "codebook.csv"))
  found <- canpumf:::.lfs_find_data_files(d)
  expect_equal(length(found), 12L)
  expect_true(all(basename(found) %in% monthly))
})

test_that("lfs_find_data_files: single monthly release", {
  d <- withr::local_tempdir()
  .make_lfs_raw_dir(d, c("pub0124.csv", "LFS_codebook.csv"))
  found <- canpumf:::.lfs_find_data_files(d)
  expect_equal(basename(found), "pub0124.csv")
})

test_that("lfs_find_data_files: fallback when no pub*.csv found", {
  d <- withr::local_tempdir()
  .make_lfs_raw_dir(d, c("lfs2025data.csv", "lfs_codebook.csv", "readme.csv"))
  found <- canpumf:::.lfs_find_data_files(d)
  # Excludes codebook and readme; finds lfs2025data.csv
  expect_equal(basename(found), "lfs2025data.csv")
})

test_that("lfs_find_data_files: excludes metadata/ subdirectory", {
  d <- withr::local_tempdir()
  writeLines("", file.path(d, "pub2023.csv"))
  meta_dir <- file.path(d, "metadata")
  dir.create(meta_dir)
  writeLines("", file.path(meta_dir, "pub_fake.csv"))
  found <- canpumf:::.lfs_find_data_files(d)
  expect_equal(basename(found), "pub2023.csv")
  expect_false("pub_fake.csv" %in% basename(found))
})

# ---- .lfs_build_version scenarios -------------------------------------------

.make_lfs_build_dir <- function(d, version, data_files) {
  # Create a version directory with canonical metadata + specified data files
  meta_dir <- file.path(d, "metadata")
  dir.create(meta_dir, recursive = TRUE)
  survyear <- canpumf:::.lfs_survyear(version)
  survmnth <- canpumf:::.lfs_survmnth(version)
  if (is.na(survmnth)) survmnth <- 1L

  vars <- tibble::tibble(
    name = c("SURVYEAR","SURVMNTH","PROV"),
    label_en = c("Year","Month","Province"),
    label_fr = c("Year","Month","Province"),
    type = c("numeric","numeric","character"),
    decimals = c(0L, 0L, NA_integer_),
    missing_low = NA_real_, missing_high = NA_real_
  )
  codes <- tibble::tibble(
    name = "PROV", val = "35",
    label_en = "Ontario", label_fr = "Ontario"
  )
  readr::write_csv(vars,  file.path(meta_dir, "variables.csv"))
  readr::write_csv(codes, file.path(meta_dir, "codes.csv"))

  for (spec in data_files) {
    row <- tibble::tibble(SURVYEAR = survyear, SURVMNTH = spec$mnth, PROV = "35")
    readr::write_csv(row, file.path(d, spec$name))
  }
  invisible(d)
}

test_that("lfs_build_version: single annual file", {
  d <- withr::local_tempdir()
  .make_lfs_build_dir(d, "2023",
    list(list(name = "pub2023.csv", mnth = 1L)))
  data <- canpumf:::.lfs_build_version(d, "label_en")
  expect_equal(nrow(data), 1L)
  expect_equal(data$SURVYEAR, 2023L)
})

test_that("lfs_build_version: 12 monthly files are combined", {
  d <- withr::local_tempdir()
  monthly_files <- lapply(1:12, function(m)
    list(name = sprintf("pub%02d23.csv", m), mnth = m))
  .make_lfs_build_dir(d, "2023", monthly_files)
  data <- canpumf:::.lfs_build_version(d, "label_en")
  expect_equal(nrow(data), 12L)
  expect_equal(sort(unique(data$SURVMNTH)), 1L:12L)
})

test_that("lfs_build_version: single monthly file", {
  d <- withr::local_tempdir()
  .make_lfs_build_dir(d, "2024-06",
    list(list(name = "pub0624.csv", mnth = 6L)))
  data <- canpumf:::.lfs_build_version(d, "label_en")
  expect_equal(nrow(data), 1L)
  expect_equal(data$SURVMNTH, 6L)
})

test_that("lfs_build_version: fallback non-pub CSV is read", {
  d <- withr::local_tempdir()
  meta_dir <- file.path(d, "metadata")
  dir.create(meta_dir, recursive = TRUE)
  vars <- tibble::tibble(
    name = c("SURVYEAR","SURVMNTH"), label_en = c("Y","M"), label_fr = c("Y","M"),
    type = c("numeric","numeric"), decimals = c(0L,0L),
    missing_low = NA_real_, missing_high = NA_real_)
  readr::write_csv(vars, file.path(meta_dir, "variables.csv"))
  readr::write_csv(tibble::tibble(name=character(),val=character(),
    label_en=character(),label_fr=character()), file.path(meta_dir, "codes.csv"))
  readr::write_csv(tibble::tibble(SURVYEAR=2025L, SURVMNTH=1L),
                   file.path(d, "lfs2025data.csv"))
  data <- canpumf:::.lfs_build_version(d, "label_en")
  expect_equal(nrow(data), 1L)
  expect_equal(data$SURVYEAR, 2025L)
})

test_that("lfs_build_version: errors when SURVYEAR/SURVMNTH missing", {
  d <- withr::local_tempdir()
  meta_dir <- file.path(d, "metadata")
  dir.create(meta_dir, recursive = TRUE)
  vars <- tibble::tibble(
    name = "X", label_en = "X", label_fr = "X",
    type = "numeric", decimals = 0L,
    missing_low = NA_real_, missing_high = NA_real_)
  readr::write_csv(vars, file.path(meta_dir, "variables.csv"))
  readr::write_csv(tibble::tibble(name=character(),val=character(),
    label_en=character(),label_fr=character()), file.path(meta_dir, "codes.csv"))
  readr::write_csv(tibble::tibble(X = 1L), file.path(d, "pub2025.csv"))
  expect_error(canpumf:::.lfs_build_version(d, "label_en"), "SURVYEAR")
})

test_that("lfs_build_version: warns on mixed annual+monthly files", {
  d <- withr::local_tempdir()
  .make_lfs_build_dir(d, "2023",
    list(list(name = "pub2023.csv",  mnth = 1L),
         list(name = "pub0123.csv",  mnth = 1L)))
  expect_warning(canpumf:::.lfs_build_version(d, "label_en"),
                 "annual.*monthly|monthly.*annual")
})

# ---- lfs_get_pumf end-to-end ------------------------------------------------

test_that("lfs_get_pumf: loads a version and returns filtered tbl", {
  tmp  <- withr::local_tempdir()
  make_lfs_vdir(tmp, "2022", n_rows = 5L)

  tbl <- canpumf:::lfs_get_pumf("2022", cache_path = tmp)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))

  result <- dplyr::collect(tbl)
  expect_equal(nrow(result), 5L)
  expect_equal(unique(result$SURVYEAR), 2022L)
  expect_true("Ontario" %in% result$PROV)
})

test_that("lfs_get_pumf: skip reload when version already in lang table", {
  tmp <- withr::local_tempdir()
  make_lfs_vdir(tmp, "2022", n_rows = 5L)

  tbl1 <- canpumf:::lfs_get_pumf("2022", cache_path = tmp)
  DBI::dbDisconnect(tbl1$src$con, shutdown = TRUE)

  # Second call: should skip build and return quickly
  db_mtime1 <- file.info(file.path(tmp, "LFS", "LFS.duckdb"))$mtime
  Sys.sleep(0.05)
  tbl2 <- canpumf:::lfs_get_pumf("2022", cache_path = tmp)
  db_mtime2 <- file.info(file.path(tmp, "LFS", "LFS.duckdb"))$mtime
  DBI::dbDisconnect(tbl2$src$con, shutdown = TRUE)

  expect_equal(db_mtime1, db_mtime2)
})

test_that("lfs_get_pumf: schema evolution when second version has new column", {
  tmp <- withr::local_tempdir()
  make_lfs_vdir(tmp, "2022", extra_col = FALSE, n_rows = 4L)
  make_lfs_vdir(tmp, "2023", extra_col = TRUE,  n_rows = 3L)

  tbl1 <- canpumf:::lfs_get_pumf("2022", cache_path = tmp)
  DBI::dbDisconnect(tbl1$src$con, shutdown = TRUE)

  tbl2 <- canpumf:::lfs_get_pumf("2023", cache_path = tmp)
  DBI::dbDisconnect(tbl2$src$con, shutdown = TRUE)

  # Full table should have both years and the new EXTRA column
  con <- DBI::dbConnect(duckdb::duckdb(),
                         dbdir = file.path(tmp, "LFS", "LFS.duckdb"),
                         read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  result <- DBI::dbGetQuery(con, "SELECT * FROM lfs_eng ORDER BY SURVYEAR")
  expect_equal(nrow(result), 7L)
  expect_true("EXTRA" %in% names(result))
  # 2022 rows have NA for EXTRA
  expect_true(all(is.na(result$EXTRA[result$SURVYEAR == 2022])))
  # 2023 rows have value 42
  expect_true(all(result$EXTRA[result$SURVYEAR == 2023] == 42L))
})

test_that("lfs_get_pumf: annual supersedes monthly versions for same year", {
  tmp <- withr::local_tempdir()
  make_lfs_vdir(tmp, "2023-01", n_rows = 2L)
  make_lfs_vdir(tmp, "2023-02", n_rows = 2L)
  make_lfs_vdir(tmp, "2023",    n_rows = 10L)  # annual

  # Load two monthly versions
  tbl_m1 <- canpumf:::lfs_get_pumf("2023-01", cache_path = tmp)
  DBI::dbDisconnect(tbl_m1$src$con, shutdown = TRUE)
  tbl_m2 <- canpumf:::lfs_get_pumf("2023-02", cache_path = tmp)
  DBI::dbDisconnect(tbl_m2$src$con, shutdown = TRUE)

  # Now load annual — should supersede the two monthly entries
  tbl_a <- canpumf:::lfs_get_pumf("2023", cache_path = tmp)
  DBI::dbDisconnect(tbl_a$src$con, shutdown = TRUE)

  con <- DBI::dbConnect(duckdb::duckdb(),
                         dbdir = file.path(tmp, "LFS", "LFS.duckdb"),
                         read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # lfs_versions: only the annual entry remains for 2023
  ver <- DBI::dbGetQuery(con,
    "SELECT version, type FROM lfs_versions WHERE survyear = 2023")
  expect_equal(nrow(ver), 1L)
  expect_equal(ver$type, "annual")
  expect_equal(ver$version, "2023")

  # Data table: 10 rows (annual), not 2+2=4 (monthly) nor 14 (both)
  n <- DBI::dbGetQuery(con,
    "SELECT COUNT(*) AS n FROM lfs_eng WHERE SURVYEAR = 2023")$n
  expect_equal(n, 10L)
})

test_that("lfs_get_pumf: monthly request covered by existing annual returns subset", {
  tmp <- withr::local_tempdir()
  make_lfs_vdir(tmp, "2022", n_rows = 6L)  # annual, SURVMNTH=1 in synthetic data

  tbl_a <- canpumf:::lfs_get_pumf("2022", cache_path = tmp)
  DBI::dbDisconnect(tbl_a$src$con, shutdown = TRUE)

  # Request monthly "2022-01" — should return filtered annual data (no download)
  tbl_m <- canpumf:::lfs_get_pumf("2022-01", cache_path = tmp)
  on.exit(DBI::dbDisconnect(tbl_m$src$con, shutdown = TRUE))

  result <- dplyr::collect(tbl_m)
  expect_equal(unique(result$SURVYEAR), 2022L)
  expect_equal(unique(result$SURVMNTH), 1L)
  expect_equal(nrow(result), 6L)  # all 6 rows have SURVMNTH=1 in synthetic data
})

test_that("lfs_get_pumf: refresh=TRUE re-labels existing version", {
  tmp <- withr::local_tempdir()
  make_lfs_vdir(tmp, "2022", n_rows = 5L)

  tbl1 <- canpumf:::lfs_get_pumf("2022", cache_path = tmp)
  DBI::dbDisconnect(tbl1$src$con, shutdown = TRUE)

  # Modify the RAW codebook.csv — pumf_parse_metadata(refresh=TRUE) re-parses
  # from this source and overwrites metadata/codes.csv, so changes must be
  # made here, not in metadata/.
  new_codebook <- data.frame(
    Field_Champ               = c("PROV", NA),
    Variable_Variable         = c("PROV", "10"),
    EnglishLabel_EtiquetteAnglais = c("Province", "Newfoundland Only"),
    FrenchLabel_EtiquetteFrancais = c("Province", "Seul Terre-Neuve"),
    stringsAsFactors = FALSE
  )
  readr::write_csv(new_codebook,
                   file.path(tmp, "LFS", "2022", "codebook.csv"))

  # val "35" has no label in the new codebook → becomes NA + warning
  tbl2 <- suppressWarnings(
    canpumf:::lfs_get_pumf("2022", cache_path = tmp, refresh = TRUE)
  )
  on.exit(DBI::dbDisconnect(tbl2$src$con, shutdown = TRUE))

  result <- dplyr::collect(tbl2)
  expect_true("Newfoundland Only" %in% result$PROV)
})

test_that("lfs_get_pumf: refresh of one monthly preserves sibling months", {
  tmp <- withr::local_tempdir()
  make_lfs_vdir(tmp, "2025-01", n_rows = 2L)
  make_lfs_vdir(tmp, "2025-02", n_rows = 3L)
  make_lfs_vdir(tmp, "2025-03", n_rows = 4L)

  for (v in c("2025-01", "2025-02", "2025-03")) {
    tbl <- canpumf:::lfs_get_pumf(v, cache_path = tmp)
    DBI::dbDisconnect(tbl$src$con, shutdown = TRUE)
  }

  # Refresh only the middle month
  tbl_r <- suppressMessages(
    canpumf:::lfs_get_pumf("2025-02", cache_path = tmp, refresh = TRUE))
  DBI::dbDisconnect(tbl_r$src$con, shutdown = TRUE)

  con <- DBI::dbConnect(duckdb::duckdb(),
                         dbdir = file.path(tmp, "LFS", "LFS.duckdb"),
                         read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # All three months' data must still be present, with unchanged row counts.
  counts <- DBI::dbGetQuery(con,
    "SELECT SURVMNTH, COUNT(*) AS n FROM lfs_eng WHERE SURVYEAR=2025
     GROUP BY SURVMNTH ORDER BY SURVMNTH")
  expect_equal(counts$SURVMNTH, c(1L, 2L, 3L))
  expect_equal(counts$n,        c(2L, 3L, 4L))

  # All three version records survive (only 2025-02 was deleted then re-added).
  vers <- DBI::dbGetQuery(con,
    "SELECT version FROM lfs_versions WHERE survyear=2025 ORDER BY version")$version
  expect_equal(vers, c("2025-01", "2025-02", "2025-03"))
})

test_that("lfs_get_pumf: refresh of a monthly covered by annual returns annual subset", {
  tmp <- withr::local_tempdir()
  make_lfs_vdir(tmp, "2024-01", n_rows = 2L)
  make_lfs_vdir(tmp, "2024",    n_rows = 10L)  # annual (synthetic SURVMNTH=1)

  # Load a monthly, then the annual supersedes it.
  tbl_m <- canpumf:::lfs_get_pumf("2024-01", cache_path = tmp)
  DBI::dbDisconnect(tbl_m$src$con, shutdown = TRUE)
  tbl_a <- suppressMessages(canpumf:::lfs_get_pumf("2024", cache_path = tmp))
  DBI::dbDisconnect(tbl_a$src$con, shutdown = TRUE)

  # Refreshing the (now superseded) monthly must NOT re-insert monthly rows or
  # add a monthly version record — it returns the annual filtered to that month.
  tbl_r <- suppressMessages(
    canpumf:::lfs_get_pumf("2024-01", cache_path = tmp, refresh = TRUE))
  DBI::dbDisconnect(tbl_r$src$con, shutdown = TRUE)

  con <- DBI::dbConnect(duckdb::duckdb(),
                         dbdir = file.path(tmp, "LFS", "LFS.duckdb"),
                         read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # lfs_versions: only the annual remains for 2024.
  ver <- DBI::dbGetQuery(con,
    "SELECT version, type FROM lfs_versions WHERE survyear=2024")
  expect_equal(nrow(ver), 1L)
  expect_equal(ver$type, "annual")

  # Data: still exactly the 10 annual rows (no duplicated month rows).
  n <- DBI::dbGetQuery(con,
    "SELECT COUNT(*) AS n FROM lfs_eng WHERE SURVYEAR=2024")$n
  expect_equal(n, 10L)
})

test_that("lfs_get_pumf: version=NULL returns status message", {
  tmp <- withr::local_tempdir()

  # No database yet
  expect_message(
    canpumf:::lfs_get_pumf(version = NULL, cache_path = tmp),
    regexp = "does not exist"
  )

  # After loading a version
  make_lfs_vdir(tmp, "2022", n_rows = 3L)
  tbl <- canpumf:::lfs_get_pumf("2022", cache_path = tmp)
  DBI::dbDisconnect(tbl$src$con, shutdown = TRUE)

  expect_message(
    tbl2 <- canpumf:::lfs_get_pumf(version = NULL, cache_path = tmp),
    regexp = "2022"
  )
  on.exit(DBI::dbDisconnect(tbl2$src$con, shutdown = TRUE))
})

test_that("lfs_get_pumf: fra table uses French labels", {
  tmp <- withr::local_tempdir()
  make_lfs_vdir(tmp, "2022", n_rows = 4L)

  tbl_eng <- canpumf:::lfs_get_pumf("2022", lang = "eng", cache_path = tmp)
  DBI::dbDisconnect(tbl_eng$src$con, shutdown = TRUE)
  tbl_fra <- canpumf:::lfs_get_pumf("2022", lang = "fra", cache_path = tmp)
  on.exit(DBI::dbDisconnect(tbl_fra$src$con, shutdown = TRUE))

  fra_result <- dplyr::collect(tbl_fra)
  expect_true("Terre-Neuve" %in% fra_result$PROV)
  expect_false("Newfoundland" %in% fra_result$PROV)
})

test_that("lfs_get_pumf: SURVYEAR/SURVMNTH stored as integer, PROV stored as ENUM", {
  tmp <- withr::local_tempdir()
  make_lfs_vdir(tmp, "2022", n_rows = 3L)

  tbl <- canpumf:::lfs_get_pumf("2022", cache_path = tmp)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))

  con  <- tbl$src$con
  info <- DBI::dbGetQuery(con, "PRAGMA table_info('lfs_eng')")

  yr_type   <- info$type[info$name == "SURVYEAR"]
  mn_type   <- info$type[info$name == "SURVMNTH"]
  prov_type <- info$type[info$name == "PROV"]

  expect_true(grepl("INT", yr_type, ignore.case = TRUE),
    label = paste0("SURVYEAR type: '", yr_type, "'"))
  expect_true(grepl("INT", mn_type, ignore.case = TRUE),
    label = paste0("SURVMNTH type: '", mn_type, "'"))
  expect_true(grepl("ENUM", prov_type, ignore.case = TRUE),
    label = paste0("PROV should be ENUM in LFS table, got: '", prov_type, "'"))
})

test_that(".lfs_append: factor column stored as ENUM on first write", {
  con <- duck_mem()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  df <- data.frame(
    SURVYEAR = 2022L,
    PROV     = factor("Newfoundland", levels = c("Newfoundland", "Ontario"))
  )
  canpumf:::.lfs_append(con, "lfs_eng", df)

  info <- DBI::dbGetQuery(con, "PRAGMA table_info('lfs_eng')")
  prov_type <- info$type[info$name == "PROV"]
  expect_true(grepl("ENUM", prov_type, ignore.case = TRUE),
    label = paste0("PROV type after first write: '", prov_type, "'"))
})

test_that(".lfs_append: ENUM gains new levels from subsequent factor data", {
  con <- duck_mem()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  df1 <- data.frame(SURVYEAR = 2022L,
                    PROV = factor("Newfoundland", levels = c("Newfoundland", "Ontario")))
  df2 <- data.frame(SURVYEAR = 2023L,
                    PROV = factor("Alberta", levels = c("Newfoundland", "Ontario", "Alberta")))

  canpumf:::.lfs_append(con, "lfs_eng", df1)
  canpumf:::.lfs_append(con, "lfs_eng", df2)

  result <- DBI::dbGetQuery(con, "SELECT PROV FROM lfs_eng ORDER BY SURVYEAR")
  expect_equal(as.character(result$PROV), c("Newfoundland", "Alberta"))
})

test_that(".lfs_append: VARCHAR factor column upgraded to ENUM on subsequent write", {
  con <- duck_mem()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # Old data written as plain character (VARCHAR)
  df_old <- data.frame(SURVYEAR = 2021L, PROV = "Ontario", stringsAsFactors = FALSE)
  canpumf:::.lfs_append(con, "lfs_eng", df_old)
  info_before <- DBI::dbGetQuery(con, "PRAGMA table_info('lfs_eng')")
  expect_true(grepl("VARCHAR|CHAR", info_before$type[info_before$name == "PROV"],
                    ignore.case = TRUE))

  # New data comes in as factor → should trigger VARCHAR → ENUM upgrade
  df_new <- data.frame(SURVYEAR = 2022L,
                       PROV = factor("Newfoundland", levels = c("Newfoundland", "Ontario")))
  expect_message(
    canpumf:::.lfs_append(con, "lfs_eng", df_new),
    regexp = "Upgraded.*PROV.*ENUM", ignore.case = TRUE
  )

  info_after <- DBI::dbGetQuery(con, "PRAGMA table_info('lfs_eng')")
  expect_true(grepl("ENUM", info_after$type[info_after$name == "PROV"],
                    ignore.case = TRUE))
})

test_that("lfs_get_pumf: refresh=TRUE with NULL version and no cache returns NULL invisibly", {
  tmp <- withr::local_tempdir()
  result <- expect_message(
    canpumf:::lfs_get_pumf(version = NULL, cache_path = tmp, refresh = TRUE),
    regexp = "No LFS data"
  )
  expect_null(result)
})

test_that("lfs_get_pumf: refresh=TRUE with NULL version rebuilds all loaded versions", {
  tmp <- withr::local_tempdir()
  # Seed two versions
  make_lfs_vdir(tmp, "2022", n_rows = 2L)
  make_lfs_vdir(tmp, "2023", n_rows = 2L)
  tbl1 <- canpumf:::lfs_get_pumf("2022", cache_path = tmp)
  DBI::dbDisconnect(tbl1$src$con, shutdown = TRUE)
  tbl2 <- canpumf:::lfs_get_pumf("2023", cache_path = tmp)
  DBI::dbDisconnect(tbl2$src$con, shutdown = TRUE)

  # refresh=TRUE with no version should rebuild both
  tbl_all <- expect_message(
    canpumf:::lfs_get_pumf(version = NULL, cache_path = tmp, refresh = TRUE),
    regexp = "Rebuilding 2 LFS"
  )
  on.exit(DBI::dbDisconnect(tbl_all$src$con, shutdown = TRUE))

  result <- dplyr::collect(tbl_all)
  expect_true(all(c(2022L, 2023L) %in% result$SURVYEAR))
})

test_that("lfs_get_pumf: refresh='auto' with NULL version runs auto-refresh, not status", {
  # Regression test: refresh='auto' must take priority over version=NULL so that
  # lfs_get_pumf(refresh="auto") actually triggers .lfs_auto_refresh, not .lfs_status.
  tmp <- withr::local_tempdir()
  # .lfs_auto_refresh calls list_available_lfs_pumf_versions() which requires
  # network; intercept by checking the error is network-related, not a status message.
  result <- tryCatch(
    canpumf:::lfs_get_pumf(version = NULL, cache_path = tmp, refresh = "auto"),
    error = function(e) e,
    message = function(m) m
  )
  # If the bug were present we'd get a .lfs_status message ("No LFS data loaded")
  # instead of a network or auto-refresh attempt. Confirm it's not a status message.
  if (inherits(result, "message"))
    expect_false(grepl("No LFS data loaded", conditionMessage(result)))
})

test_that("lfs_get_pumf: invalid refresh value errors", {
  tmp <- withr::local_tempdir()
  expect_error(
    canpumf:::lfs_get_pumf("2022", cache_path = tmp, refresh = "all"),
    regexp = "must be FALSE, TRUE"
  )
})

# ---- Cache-gated integration test -------------------------------------------

test_that("lfs_get_pumf: real LFS version loads and returns plausible data", {
  cache <- getOption("canpumf.cache_path", "")
  lfs_versions <- list.files(file.path(cache, "LFS"),
                               pattern = "^[0-9]{4}(-[0-9]{2})?$",
                               full.names = FALSE)
  skip_if(length(lfs_versions) == 0L, "No LFS version in cache")

  v   <- lfs_versions[[1L]]
  tbl <- canpumf:::lfs_get_pumf(v, cache_path = cache)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))

  result <- dplyr::collect(tbl)
  expect_gt(nrow(result), 1000L)
  expect_true("SURVYEAR" %in% names(result))
  expect_true("SURVMNTH" %in% names(result))
  expect_true(is.integer(result$SURVYEAR))
  expect_true(is.integer(result$SURVMNTH))

  # PROV should be labeled (not raw codes)
  if ("PROV" %in% names(result)) {
    prov_vals <- unique(stats::na.omit(result$PROV))
    expect_false(all(grepl("^[0-9]+$", prov_vals)),
      label = "PROV should be labeled, not raw codes")
  }
})
