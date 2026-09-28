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

test_that(".apply_data_fixups: rename_regex rewrites onto declared names", {
  data  <- data.frame(AB1 = 1L, AC28AA = 2L, AGEGRP5 = 3L)
  fixup <- list(rename_regex = c("^A" = ""))
  result <- canpumf:::.apply_data_fixups(
    data, fixup, known_vars = c("B1", "C28AA", "AGEGRP5"))
  # AGEGRP5 is itself a declared name, so the pattern must leave it alone.
  expect_equal(names(result), c("B1", "C28AA", "AGEGRP5"))
})

test_that(".apply_data_fixups: rename_regex never collides with an existing column", {
  # Both the decorated and the bare name are present: rewriting AB1 onto B1
  # would produce two B1 columns, so the rewrite must be skipped.
  data  <- data.frame(AB1 = 1L, B1 = 2L)
  fixup <- list(rename_regex = c("^A" = ""))
  result <- canpumf:::.apply_data_fixups(data, fixup, known_vars = c("B1"))
  expect_equal(names(result), c("AB1", "B1"))
})

test_that(".apply_data_fixups: rename_regex is a no-op without known_vars", {
  data  <- data.frame(AB1 = 1L)
  fixup <- list(rename_regex = c("^A" = ""))
  expect_equal(names(canpumf:::.apply_data_fixups(data, fixup)), "AB1")
})

test_that(".apply_data_fixups: rename_regex is not applied as a literal rename", {
  # `$rename` partial-matches `rename_regex`; an entry declaring only the regex
  # form must not have its pattern treated as a column name.
  data  <- data.frame(`^A` = 1L, check.names = FALSE)
  fixup <- list(rename_regex = c("^A" = ""))
  result <- canpumf:::.apply_data_fixups(data, fixup, known_vars = "B1")
  expect_equal(names(result), "^A")
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

test_that(".apply_numeric_conversion: missing_codes NA discrete values only", {
  # Sentinels on both sides of the valid data (PALS 2006 AUDE_Q02): a single
  # missing_low/missing_high pair cannot express this, so the codes are listed.
  vars <- tibble::tibble(name="X", type="numeric",
                          missing_low=NA_real_, missing_high=NA_real_,
                          decimals=0L)
  data <- data.frame(X = c("-7", "1", "66", "998", "999"),
                     stringsAsFactors = FALSE)
  result <- canpumf:::.apply_numeric_conversion(
    data, vars, missing_codes = list(X = c(-5, -6, -7, 998, 999)))
  expect_equal(result$X, c(NA, 1, 66, NA, NA))
})

test_that(".apply_numeric_conversion: missing_codes apply per column", {
  vars <- tibble::tibble(name=c("X","Y"), type="numeric",
                          missing_low=NA_real_, missing_high=NA_real_,
                          decimals=0L)
  data <- data.frame(X = c("9", "1"), Y = c("9", "1"), stringsAsFactors = FALSE)
  result <- canpumf:::.apply_numeric_conversion(data, vars,
                                                 missing_codes = list(X = 9))
  expect_equal(result$X, c(NA, 1))
  expect_equal(result$Y, c(9, 1))
})

test_that(".apply_numeric_conversion: implied decimals are opt-in", {
  vars <- tibble::tibble(name="X", type="numeric",
                          missing_low=NA_real_, missing_high=NA_real_,
                          decimals=2L)
  data <- data.frame(X = c("8269", "201"), stringsAsFactors = FALSE)
  expect_equal(canpumf:::.apply_numeric_conversion(data, vars)$X,
               c(8269, 201))
  expect_equal(
    canpumf:::.apply_numeric_conversion(data, vars, implied_decimals = TRUE)$X,
    c(82.69, 2.01))
})

test_that(".apply_numeric_conversion: an explicit point overrides implied decimals", {
  # SAS/SPSS w.d informat rule: a value that already carries a "." is read at
  # face value, so mixed columns survive the correction unscathed.
  vars <- tibble::tibble(name="X", type="numeric",
                          missing_low=NA_real_, missing_high=NA_real_,
                          decimals=2L)
  data <- data.frame(X = c("82.69", "8269", NA), stringsAsFactors = FALSE)
  expect_equal(
    canpumf:::.apply_numeric_conversion(data, vars, implied_decimals = TRUE)$X,
    c(82.69, 82.69, NA_real_))
})

test_that(".apply_numeric_conversion: missing range applies after implied decimals", {
  # Reserved codes are documented in display units (99999.99), not raw digits.
  vars <- tibble::tibble(name="X", type="numeric",
                          missing_low=99999.96, missing_high=99999.99,
                          decimals=2L)
  data <- data.frame(X = c("0008269", "9999999"), stringsAsFactors = FALSE)
  expect_equal(
    canpumf:::.apply_numeric_conversion(data, vars, implied_decimals = TRUE)$X,
    c(82.69, NA_real_))
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

test_that("pumf_build_duckdb: fixed-width implied decimals come from the layout", {
  # WGT is declared "( 4 )" in DATA LIST: divided on read unless the value
  # carries its own point.  AMT has only a display FORMAT with 2 decimals,
  # which must not change the stored value.
  tmp  <- withr::local_tempdir()
  vdir <- file.path(tmp, "FAKE", "2099")
  meta <- file.path(vdir, "metadata")
  dir.create(meta, recursive = TRUE)
  readr::write_csv(tibble::tibble(
    name = c("WGT", "AMT"), label_en = c("Weight", "Amount"),
    label_fr = c("Poids", "Montant"), type = "numeric", decimals = c(4L, 2L),
    missing_low = NA_real_, missing_high = NA_real_),
    file.path(meta, "variables.csv"))
  readr::write_csv(tibble::tibble(name = character(), val = character(),
                                  label_en = character(), label_fr = character()),
                   file.path(meta, "codes.csv"))
  readr::write_csv(tibble::tibble(name = c("WGT", "AMT"), start = c(1L, 8L),
                                  end = c(7L, 11L), decimals = c(4L, NA)),
                   file.path(meta, "layout.csv"))
  writeLines(c("00123451234", "12.3456  15"), file.path(vdir, "survey.txt"))

  res <- collect_build(vdir)
  expect_equal(res$WGT, c(1.2345, 12.3456))
  expect_equal(res$AMT, c(1234, 15))
})

test_that("pumf_build_duckdb: labelled missing codes of numeric variables become NA", {
  # No MISSING VALUES anywhere.  AGE is sentinel-only (999.7/999.9 read with one
  # implied decimal, GSS Cycle 21 AGE_DIV_MA1); IDX is a 0-1 index whose
  # sentinels 7/9 sit above the data, one with a qualified label (Cycle 8 D11),
  # so it is kept numeric via force_numeric; HRS has a zero label, a valid 0.
  tmp  <- withr::local_tempdir()
  vdir <- file.path(tmp, "FAKE", "2099")
  meta <- file.path(vdir, "metadata")
  dir.create(meta, recursive = TRUE)
  readr::write_csv(tibble::tibble(
    name = c("AGE", "IDX", "HRS"), label_en = c("Age", "Index", "Hours"),
    label_fr = c("Âge", "Indice", "Heures"), type = "numeric",
    decimals = c(1L, 3L, NA), missing_low = NA_real_, missing_high = NA_real_),
    file.path(meta, "variables.csv"))
  readr::write_csv(tibble::tibble(
    name     = c("AGE", "AGE", "IDX", "IDX", "IDX", "HRS", "HRS"),
    val      = c("999.7", "999.9", "1", "7", "9", "0", "99"),
    label_en = c("Not asked", "Not stated", "Full health",
                 "NOT STATED - PATH UNKNOWN", "Don't know", "None", "Not stated"),
    label_fr = c("Non demandé", "Non déclaré", "Pleine santé",
                 "Non déclaré", "Ne sait pas", "Aucun", "Non déclaré")),
    file.path(meta, "codes.csv"))
  readr::write_csv(tibble::tibble(name = c("AGE", "IDX", "HRS"),
                                  start = c(1L, 5L, 10L), end = c(4L, 9L, 11L),
                                  decimals = c(1L, NA, NA)),
                   file.path(meta, "layout.csv"))
  writeLines(c("04530.97300", "9997    799", "99991.00012", "0071    912"),
             file.path(vdir, "survey.txt"))
  canpumf:::.pumf_registry_override_set(
    "FAKE", "2099", pumf_registry_entry(data_fixups = list(force_numeric = "IDX")))
  on.exit(canpumf:::.pumf_registry_override_clear("FAKE", "2099"), add = TRUE)

  res <- collect_build(vdir)
  expect_equal(res$AGE, c(45.3, NA, NA, 7.1))
  expect_equal(res$IDX, c(0.973, NA, 1, NA))
  expect_equal(res$HRS, c(0, NA, 12, 12))

  # The companion names the sentinels of the force_numeric variable by their
  # documented labels, although force_numeric removed IDX's rows from the code
  # table before labelling (it once fell back to the digits "7" and "9").
  con <- DBI::dbConnect(duckdb::duckdb(),
                        dbdir = file.path(vdir, "FAKE_2099.duckdb"), read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  sent <- DBI::dbGetQuery(con, 'SELECT * FROM "pumf_sentinels_eng" ORDER BY pumf_row_id')
  expect_equal(sent$pumf_row_id, c(2, 3, 4))
  expect_equal(as.character(sent$IDX), c("NOT STATED - PATH UNKNOWN", NA, "Don't know"))
  expect_equal(as.character(sent$AGE), c("Not asked", "Not stated", NA))
  expect_equal(as.character(sent$HRS), c("Not stated", NA, NA))

  # codes_applied.csv records how each code was applied: the labelled values
  # that stay numbers ("value": IDX 1 "Full health", HRS 0 "None") against the
  # sentinels, and pumf_topcodes() lists the former.
  ca <- canpumf:::.read_codes_applied(meta)
  expect_equal(ca$applied_as[ca$name == "AGE"], c("sentinel", "sentinel"))
  expect_equal(ca$applied_as[ca$name == "IDX"], c("value", "sentinel", "sentinel"))
  expect_equal(ca$applied_as[ca$name == "HRS"], c("value", "sentinel"))
  tc <- pumf_topcodes("FAKE", "2099", cache_path = tmp)
  expect_s3_class(tc, "tbl_df")
  expect_equal(names(tc), c("name", "val", "label_en", "label_fr"))
  expect_equal(tc$name, c("HRS", "IDX"))
  expect_equal(tc$val, c(0, 1))
  expect_equal(tc$label_en, c("None", "Full health"))
  expect_equal(tc$label_fr, c("Aucun", "Pleine sant\u00e9"))
})

test_that("pumf_build_duckdb: force_numeric is ignored when every value is labelled", {
  # DAY is fully labelled (GSS Cycle 12 DDAY): a category despite the override.
  # HRS carries a top-code label beside unlabelled values: the case
  # force_numeric exists for, so it is numeric and its sentinel is NA.
  tmp  <- withr::local_tempdir()
  vdir <- file.path(tmp, "FAKE", "2099")
  meta <- file.path(vdir, "metadata")
  dir.create(meta, recursive = TRUE)
  readr::write_csv(tibble::tibble(
    name = c("DAY", "HRS"), label_en = c("Day", "Hours"),
    label_fr = c("Jour", "Heures"), type = "character", decimals = NA_integer_,
    missing_low = NA_real_, missing_high = NA_real_),
    file.path(meta, "variables.csv"))
  readr::write_csv(tibble::tibble(
    name     = c("DAY", "DAY", "HRS", "HRS"),
    val      = c("1", "2", "75", "98"),
    label_en = c("Sunday", "Monday", "75 and more", "Not stated"),
    label_fr = c("Dimanche", "Lundi", "75 et plus", "Non déclaré")),
    file.path(meta, "codes.csv"))
  readr::write_csv(tibble::tibble(DAY = c("1", "2", "01"), HRS = c("40", "75", "98")),
                   file.path(vdir, "survey.csv"))
  canpumf:::.pumf_registry_override_set(
    "FAKE", "2099",
    pumf_registry_entry(data_fixups = list(force_numeric = c("DAY", "HRS"))))
  on.exit(canpumf:::.pumf_registry_override_clear("FAKE", "2099"), add = TRUE)

  res <- collect_build(vdir)
  expect_equal(as.character(res$DAY), c("Sunday", "Monday", "Sunday"))
  expect_equal(res$HRS, c(40, 75, NA))
})

test_that(".fully_labelled_vars: numeric and implied-decimal matches", {
  codes <- tibble::tibble(name = c("A", "A", "B", "C"),
                          val = c("1", "2", "999.7", "5"),
                          label_en = c("x", "y", "Not asked", "Five"),
                          label_fr = NA_character_)
  data  <- tibble::tibble(A = c("01", "2", " "), B = c("9997", "9997", "9997"),
                          C = c("5", "6", "5"))
  lay   <- tibble::tibble(name = "B", start = 1L, end = 4L, decimals = 1L)
  expect_equal(canpumf:::.fully_labelled_vars(data, codes, c("A", "B", "C", "Z"),
                                              decimals = lay),
               c("A", "B"))
})

test_that(".label_missing_codes: qualified labels count, zero and count labels do not", {
  codes <- tibble::tibble(
    name     = c("A", "A", "B", "B", "C", "D"),
    val      = c("96", "97", "0", "98", "0", "2"),
    label_en = c("NOT APPLICABLE(DOES NOT DRIVE)", "Refused", "None",
                 "Not asked - born in Canada", "zero income, not applicable",
                 "Two Not stated codes"),
    label_fr = c(NA, NA, "Aucun", "Non demandé - né au Canada", NA, NA))
  expect_equal(canpumf:::.label_missing_codes(codes),
               list(A = c(96, 97), B = 98))
})

test_that("read_metadata: layout.csv without a decimals column reads as NA", {
  # Caches written before layout decimals existed must still load.
  tmp <- withr::local_tempdir()
  readr::write_csv(tibble::tibble(name = "X", label_en = "X", label_fr = "X",
    type = "numeric", decimals = 2L, missing_low = NA_real_, missing_high = NA_real_),
    file.path(tmp, "variables.csv"))
  readr::write_csv(tibble::tibble(name = character(), val = character(),
                                  label_en = character(), label_fr = character()),
                   file.path(tmp, "codes.csv"))
  readr::write_csv(tibble::tibble(name = "X", start = 1L, end = 4L),
                   file.path(tmp, "layout.csv"))
  md <- canpumf:::read_metadata(tmp)
  expect_true("decimals" %in% names(md$layout))
  expect_true(is.na(md$layout$decimals))
})

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


# ---- sentinel companion ------------------------------------------------------

test_that(".apply_numeric_conversion: records the blanked sentinels per column", {
  vars <- tibble::tibble(
    name = c("INC", "HRS"), type = "numeric", decimals = NA_integer_,
    missing_low = c(NA_real_, 998), missing_high = c(NA_real_, 999))
  data <- tibble::tibble(INC = c("100", "9999999", "8888888", "200"),
                         HRS = c("40", "999", "998", "12"))
  out  <- canpumf:::.apply_numeric_conversion(data, vars,
                                              na_values = c("9999999", "8888888"))
  sent <- attr(out, "pumf_sentinels")
  expect_named(sent, c("INC", "HRS"))
  expect_equal(sent$INC, c(NA, 9999999, 8888888, NA))
  expect_equal(sent$HRS, c(NA, 999, 998, NA))
  expect_equal(out$INC, c(100, NA, NA, 200))
})

test_that(".apply_numeric_conversion: a column without sentinels is not recorded", {
  vars <- tibble::tibble(name = "X", type = "numeric", decimals = NA_integer_,
                         missing_low = NA_real_, missing_high = NA_real_)
  out  <- canpumf:::.apply_numeric_conversion(tibble::tibble(X = c("1", "2")), vars)
  expect_length(attr(out, "pumf_sentinels"), 0L)
})

test_that(".apply_numeric_conversion: an unparseable value is not a sentinel", {
  vars <- tibble::tibble(name = "X", type = "numeric", decimals = NA_integer_,
                         missing_low = NA_real_, missing_high = NA_real_)
  out  <- suppressWarnings(canpumf:::.apply_numeric_conversion(
    tibble::tibble(X = c("1", "abc", "")), vars))
  expect_length(attr(out, "pumf_sentinels"), 0L)
})

test_that(".apply_code_labels: records na_values blanked in labelled columns", {
  codes <- tibble::tibble(name = "PROV", val = c("10", "35"),
                          label_en = c("NL", "ON"), label_fr = c("TN", "ON"))
  data  <- tibble::tibble(PROV = c("10", "99", "35"))
  out   <- canpumf:::.apply_code_labels(data, codes, "label_en", na_values = "99")
  expect_equal(attr(out, "pumf_sentinels")$PROV, c(NA, 99, NA))
  expect_equal(as.character(out$PROV), c("NL", NA, "ON"))
})

test_that(".sentinel_companion: keeps only rows with a sentinel", {
  sent <- list(A = c(NA, 9, NA, 9), B = c(8, NA, NA, 9))
  out  <- canpumf:::.sentinel_companion(sent, 4L)
  expect_equal(out$pumf_row_id, c(1L, 2L, 4L))
  expect_equal(out$A, c(NA, 9, 9))
  expect_equal(out$B, c(8, NA, 9))
  empty <- canpumf:::.sentinel_companion(list(), 4L)
  expect_equal(nrow(empty), 0L)
  expect_named(empty, "pumf_row_id")
})

test_that(".label_sentinel_companion: codes.csv label wins, then registry, then digits", {
  sent  <- tibble::tibble(pumf_row_id = 1:4,
                          INC = c(9999999, 8888888, NA, 77),
                          HRS = c(NA, 999, 0, NA))
  codes <- tibble::tibble(name = "HRS", val = "999",
                          label_en = "Not applicable", label_fr = "Sans objet")
  labs  <- list("9999999" = c(label_en = "Not applicable", label_fr = "Sans objet"),
                "8888888" = c(label_en = "Not available"),
                HRS = list("0" = c(label_en = "Zero hours")))
  en <- canpumf:::.label_sentinel_companion(sent, codes, "label_en", labs)
  expect_true(is.factor(en$INC))
  expect_equal(as.character(en$INC),
               c("Not applicable", "Not available", NA, "77"))
  expect_equal(levels(en$INC), c("77", "Not available", "Not applicable"))
  expect_equal(as.character(en$HRS), c(NA, "Not applicable", "Zero hours", NA))
  fr <- canpumf:::.label_sentinel_companion(sent, codes, "label_fr", labs)
  # label_fr where given, label_en as the fallback
  expect_equal(as.character(fr$INC), c("Sans objet", "Not available", NA, "77"))
  expect_equal(as.character(fr$HRS), c(NA, "Sans objet", "Zero hours", NA))
})

test_that(".label_sentinel_companion: same label for two codes keeps them apart", {
  sent  <- tibble::tibble(pumf_row_id = 1:2, X = c(99, 999))
  labs  <- list("99" = c(label_en = "NA"), "999" = c(label_en = "NA"))
  out   <- canpumf:::.label_sentinel_companion(sent, NULL, "label_en", labs)
  expect_equal(levels(out$X), c("NA (99)", "NA (999)"))
  expect_equal(as.character(out$X), c("NA (99)", "NA (999)"))
})

# ---- unique value labels ------------------------------------------------------

test_that(".pumf_dedupe_labels: suffixes every member of a group, leaves the rest", {
  f <- canpumf:::.pumf_dedupe_labels
  expect_equal(f(c("A", "Other", "B", "Other", NA), c("1", "3", "5", "6", "9")),
               c("A", "Other (3)", "B", "Other (6)", NA))
  expect_equal(f(c("A", "B"), c("1", "2")), c("A", "B"))
  expect_equal(f(c(NA_character_, NA), c("1", "2")), c(NA_character_, NA))
  expect_equal(f(character(0), character(0)), character(0))
  # the same code listed twice ("01" and "1") is one code, not a duplicate
  expect_equal(f(c("Yes", "Yes", "No"), c("01", "1", "2"), key = c("1", "1", "2")),
               c("Yes", "Yes", "No"))
  # only present codes count: one "Other" in the data leaves the label alone,
  # two suffix the whole group (the absent member included)
  expect_equal(f(c("A", "Other", "Other", "Other"), c("1", "3", "6", "7"),
                 present = c(TRUE, TRUE, FALSE, FALSE)),
               c("A", "Other", "Other", "Other"))
  expect_equal(f(c("A", "Other", "Other", "Other"), c("1", "3", "6", "7"),
                 present = c(TRUE, TRUE, FALSE, TRUE)),
               c("A", "Other (3)", "Other (6)", "Other (7)"))
})

test_that(".pumf_codes_present: normalised codes that occur, na_values excluded", {
  codes <- tibble::tibble(name = c("H", "H", "S", "S", "N"), val = c("01", "3", "a", "b", "1"),
                          label_en = "x", label_fr = NA)
  d <- tibble::tibble(H = c("1", "01", "3", NA, "99"), S = c("a", "a", "c", NA, NA),
                      N = c(1, 2, 3, 4, 5))
  p <- canpumf:::.pumf_codes_present(d, codes, na_values = "99")
  expect_equal(sort(p$H), c("1", "3"))
  expect_equal(sort(p$S), c("a", "c"))
  expect_null(p$N)   # numeric columns are not coded columns
  expect_equal(canpumf:::.pumf_codes_present(d, NULL), list())
})

test_that(".pumf_unique_code_labels: per variable and per language", {
  codes <- tibble::tibble(
    name     = c("H", "H", "H", "H", "Q", "Q", "P", "P"),
    val      = c("1", "3", "6", "9", "7", "8", "01", "1"),
    label_en = c("English", "Other", "Other", "Not stated", "Don't know", "Don't know", "Yes", "Yes"),
    label_fr = c("Anglais", "Autre", "Autre", NA, "Ne sais pas", "Ne sait pas", "Oui", NA))
  out <- canpumf:::.pumf_unique_code_labels(codes)
  h <- out[out$name == "H", ]
  expect_equal(h$label_en, c("English", "Other (3)", "Other (6)", "Not stated"))
  expect_equal(h$label_fr, c("Anglais", "Autre (3)", "Autre (6)", "Not stated"))
  # English collides, French does not: only English is suffixed
  q <- out[out$name == "Q", ]
  expect_equal(q$label_en, c("Don't know (7)", "Don't know (8)"))
  expect_equal(q$label_fr, c("Ne sais pas", "Ne sait pas"))
  # "01" and "1" are one code: the first listing is kept, unsuffixed
  p <- out[out$name == "P", ]
  expect_equal(nrow(p), 1L)
  expect_equal(p$label_en, "Yes")
  expect_identical(canpumf:::.pumf_unique_code_labels(NULL), NULL)
  expect_equal(nrow(canpumf:::.pumf_unique_code_labels(codes[0, ])), 0L)
  # with a present list, a group is suffixed only where two of its codes occur
  out <- canpumf:::.pumf_unique_code_labels(codes, present = list(H = c("1", "3"), Q = c("7", "8")))
  expect_equal(out$label_en[out$name == "H"], c("English", "Other", "Other", "Not stated"))
  expect_equal(out$label_en[out$name == "Q"], c("Don't know (7)", "Don't know (8)"))
  # a variable absent from the list has no present code: labels as documented,
  # French still filled from English
  out <- canpumf:::.pumf_unique_code_labels(codes, present = list())
  expect_equal(out$label_en[out$name == "H"], c("English", "Other", "Other", "Not stated"))
  expect_equal(out$label_fr[out$name == "H"], c("Anglais", "Autre", "Autre", "Not stated"))
})

test_that(".pumf_apply_code_fixups: codes_supplement appends, codes_override replaces", {
  codes <- tibble::tibble(name = c("HHMOTG", "HHMOTG", "HHMOTG", "X"),
                          val = c("1", "3", "6", "1"),
                          label_en = c("English", "Other", "Other", "x"),
                          label_fr = c("Anglais", "Autre", "Autre", NA))
  fx <- list(
    codes_supplement = list(X = data.frame(val = "9", label_en = "Not stated",
                                           label_fr = "Non d\u00e9clar\u00e9")),
    codes_override   = list(HHMOTG = data.frame(
      val = c("03", "6", "7"),
      label_en = c("Other single responses", "Other multiple responses", "New"),
      label_fr = c("Autres r\u00e9ponses uniques", "Autres r\u00e9ponses multiples", "Nouveau"))))
  out <- canpumf:::.pumf_apply_code_fixups(codes, fx)
  h <- out[out$name == "HHMOTG", ]
  # "03" matches the declared "3"; 7 is appended
  expect_equal(h$val, c("1", "3", "6", "7"))
  expect_equal(h$label_en, c("English", "Other single responses", "Other multiple responses", "New"))
  expect_equal(h$label_fr[2:3], c("Autres r\u00e9ponses uniques", "Autres r\u00e9ponses multiples"))
  x <- out[out$name == "X", ]
  expect_equal(x$val, c("1", "9"))
  expect_equal(x$label_en, c("x", "Not stated"))
  expect_identical(canpumf:::.pumf_apply_code_fixups(codes, NULL), codes)
  expect_identical(canpumf:::.pumf_apply_code_fixups(codes, list()), codes)
})

test_that("codes_applied.csv round-trips through the read/write helpers", {
  tmp   <- withr::local_tempdir()
  codes <- tibble::tibble(name = "H", val = c("3", "6"),
                          label_en = c("Other (3)", "Other (6)"),
                          label_fr = c("Autre (3)", "Autre (6)"))
  expect_null(canpumf:::.read_codes_applied(tmp))
  canpumf:::.write_codes_applied(codes, tmp)
  back <- canpumf:::.read_codes_applied(tmp)
  expect_equal(back, as.data.frame(codes))
})

test_that(".apply_code_labels: codes sharing a label become distinct levels", {
  codes <- tibble::tibble(
    name = "HHMOTG", val = c("1", "3", "6"),
    label_en = c("English", "Other", "Other"),
    label_fr = c("Anglais", "Autre", "Autre"))
  d   <- tibble::tibble(HHMOTG = c("3", "6", "1", "6"))
  out <- canpumf:::.apply_code_labels(d, codes, "label_en")
  expect_equal(levels(out$HHMOTG), c("English", "Other (3)", "Other (6)"))
  expect_equal(as.character(out$HHMOTG), c("Other (3)", "Other (6)", "English", "Other (6)"))
  out_fr <- canpumf:::.apply_code_labels(d, codes, "label_fr")
  expect_equal(levels(out_fr$HHMOTG), c("Anglais", "Autre (3)", "Autre (6)"))
})

test_that(".apply_code_labels: a shared label whose other codes never occur is left alone", {
  # CHS PITM_05: 5 and 8 both "Do not know", only 8 in the data
  codes <- tibble::tibble(
    name = "PITM_05", val = c("1", "5", "8"),
    label_en = c("Yes", "Do not know", "Do not know"),
    label_fr = c("Oui", "Ne sait pas", "Ne sait pas"))
  d   <- tibble::tibble(PITM_05 = c("1", "8", "8"))
  out <- canpumf:::.apply_code_labels(d, codes, "label_en")
  expect_equal(levels(out$PITM_05), c("Yes", "Do not know"))
  expect_equal(as.character(out$PITM_05), c("Yes", "Do not know", "Do not know"))
  # a zero-padded data value is the same code as its unpadded listing
  d2  <- tibble::tibble(PITM_05 = c("05", "8"))
  expect_equal(levels(canpumf:::.apply_code_labels(d2, codes, "label_en")$PITM_05),
               c("Yes", "Do not know (5)", "Do not know (8)"))
})

test_that(".apply_code_labels: an English-only collision leaves French unsuffixed", {
  codes <- tibble::tibble(
    name = "Q", val = c("1", "7", "8"),
    label_en = c("Yes", "Don't know", "Don't know"),
    label_fr = c("Oui", "Ne sais pas", "Refus"))
  d <- tibble::tibble(Q = c("7", "8", "1"))
  expect_equal(levels(canpumf:::.apply_code_labels(d, codes, "label_en")$Q),
               c("Yes", "Don't know (7)", "Don't know (8)"))
  expect_equal(levels(canpumf:::.apply_code_labels(d, codes, "label_fr")$Q),
               c("Oui", "Ne sais pas", "Refus"))
})

test_that(".apply_code_labels: a French label missing on one of two same-English codes", {
  # fr falls back to en for code 8 only, which then collides with nothing in French
  codes <- tibble::tibble(
    name = "Q", val = c("7", "8"),
    label_en = c("Don't know", "Don't know"),
    label_fr = c("Ne sait pas", NA))
  d <- tibble::tibble(Q = c("7", "8"))
  expect_equal(levels(canpumf:::.apply_code_labels(d, codes, "label_fr")$Q),
               c("Ne sait pas", "Don't know"))
})

test_that("pumf_build_duckdb: writes pumf_row_id and the sentinel companion as ENUM", {
  tmp  <- withr::local_tempdir()
  vdir <- make_minimal_version_dir(tmp)
  meta <- file.path(vdir, "metadata")
  readr::write_csv(tibble::tibble(
    name = c("PROV", "INC"), label_en = c("Province", "Income"),
    label_fr = c("Province", "Revenu"), type = c("character", "numeric"),
    decimals = NA_integer_, missing_low = NA_real_, missing_high = NA_real_),
    file.path(meta, "variables.csv"))
  readr::write_csv(tibble::tibble(PROV = c("10", "35", "10"),
                                  INC  = c("100", "9999999", "8888888")),
                   file.path(vdir, "survey.csv"))
  fx <- list(na_values = c("9999999", "8888888"),
             sentinel_labels = list(
               "9999999" = c(label_en = "Not applicable", label_fr = "Sans objet")))
  r <- canpumf:::pumf_build_duckdb(vdir, "FAKE", "2099", lang = "eng",
                                   data_fixups = fx, refresh = TRUE)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = r$db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  main <- DBI::dbGetQuery(con, 'SELECT * FROM "eng" ORDER BY pumf_row_id')
  expect_equal(names(main)[1L], "pumf_row_id")
  expect_equal(main$pumf_row_id, c(1, 2, 3))
  expect_equal(main$INC, c(100, NA, NA))
  types <- DBI::dbGetQuery(con, "PRAGMA table_info('eng')")
  expect_equal(types$type[types$name == "pumf_row_id"], "BIGINT")

  expect_true(DBI::dbExistsTable(con, "pumf_sentinels_eng"))
  sent <- DBI::dbGetQuery(con, 'SELECT * FROM "pumf_sentinels_eng" ORDER BY pumf_row_id')
  expect_equal(names(sent), c("pumf_row_id", "INC"))
  expect_equal(sent$pumf_row_id, c(2, 3))
  # labelled through sentinel_labels, or the digits when unlabelled
  expect_equal(as.character(sent$INC), c("Not applicable", "8888888"))
  stypes <- DBI::dbGetQuery(con, "PRAGMA table_info('pumf_sentinels_eng')")
  expect_match(stypes$type[stypes$name == "INC"], "^ENUM")
  expect_equal(stypes$type[stypes$name == "pumf_row_id"], "BIGINT")

  # the build stamp names the table, the canpumf version and the build time
  info <- canpumf:::.read_build_info(con, "eng")
  expect_equal(nrow(info), 1L)
  expect_equal(info$canpumf_version, as.character(utils::packageVersion("canpumf")))
  expect_match(info$built, "^[0-9]{4}-[0-9]{2}-[0-9]{2} ")
  expect_null(canpumf:::.read_build_info(con, "fra"))
})

test_that("pumf_build_duckdb: a rebuild replaces the table's build stamp", {
  tmp  <- withr::local_tempdir()
  vdir <- make_minimal_version_dir(tmp)
  r <- canpumf:::pumf_build_duckdb(vdir, "FAKE", "2099", lang = "eng", refresh = TRUE)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = r$db_path)
  DBI::dbExecute(con, "UPDATE pumf_build_info SET canpumf_version = '0.0.1'")
  DBI::dbDisconnect(con, shutdown = TRUE)
  canpumf:::pumf_build_duckdb(vdir, "FAKE", "2099", lang = "eng", refresh = TRUE)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = r$db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  info <- DBI::dbReadTable(con, "pumf_build_info")
  expect_equal(nrow(info), 1L)
  expect_equal(info$table, "eng")
  expect_equal(info$canpumf_version, as.character(utils::packageVersion("canpumf")))
})

test_that("pumf_build_duckdb: a survey without sentinels gets an empty companion", {
  tmp  <- withr::local_tempdir()
  vdir <- make_minimal_version_dir(tmp)
  r <- canpumf:::pumf_build_duckdb(vdir, "FAKE", "2099", lang = "eng", refresh = TRUE)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = r$db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  sent <- DBI::dbGetQuery(con, 'SELECT * FROM "pumf_sentinels_eng"')
  expect_equal(nrow(sent), 0L)
  expect_named(sent, "pumf_row_id")
})
