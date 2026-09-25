# Tests for the longitudinal engine (R/longitudinal.R) and the LFS_HIST series
# (R/lfs_hist.R).  Everything here is offline: the engine runs on a synthetic
# spec, and the ODESI SAS parser on an inline fixture.

# ---- Synthetic series -------------------------------------------------------

# A spec whose slices are months without annual files (like LFS_HIST); each
# prepared month has 3 rows, and PROV is labelled through a fixed code list.
.fake_long_spec <- function(first_year = 1990L, n_years = 2L) {
  versions <- sprintf("%d-%02d", rep(first_year + seq_len(n_years) - 1L, each = 12L), 1:12)
  codes <- tibble::tibble(name = "PROV", val = c("10", "35"),
                          label_en = c("Newfoundland", "Ontario"),
                          label_fr = c("Terre-Neuve", "Ontario"))
  list(
    series         = "FAKE",
    db_file        = "FAKE.duckdb",
    table_prefix   = "fake",
    versions_table = "fake_versions",
    annual_files   = FALSE,
    example        = as.character(first_year),
    validate       = function(v) {
      if (nchar(v) == 7L && !v %in% versions) stop("out of range: ", v)
      canpumf:::.lfs_version_type(v)
    },
    available      = function() versions,
    prepare        = function(version, cache_path, refresh, redownload) {
      d <- file.path(cache_path, "FAKE", version)
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
      d
    },
    build          = function(version_dir, label_col, version) {
      data <- data.frame(
        SURVYEAR = rep(as.integer(substr(version, 1L, 4L)), 3L),
        SURVMNTH = rep(as.integer(substr(version, 6L, 7L)), 3L),
        PROV     = c("10", "35", "35"),
        FWEIGHT  = c(100, 200, 300),
        stringsAsFactors = FALSE)
      canpumf:::.apply_code_labels(data, codes, label_col)
    },
    variables      = function(cache_path, versions)
      tibble::tibble(name = c("PROV", "FWEIGHT"),
                     label_en = c("Province", "Weight"),
                     label_fr = c("Province", "Poids")))
}

fake_get <- function(spec, ...) suppressMessages(canpumf:::.long_get_pumf(spec, ...))
fake_close <- function(t) canpumf:::.long_close_tbl(t)

test_that(".long_get_pumf loads a month and records it", {
  tmp  <- withr::local_tempdir()
  spec <- .fake_long_spec()
  t <- fake_get(spec, "1990-03", cache_path = tmp)
  on.exit(fake_close(t))
  d <- dplyr::collect(t)
  expect_equal(nrow(d), 3L)
  expect_true(all(d$SURVMNTH == 3L))
  expect_s3_class(d$PROV, "factor")
  expect_equal(canpumf:::.long_loaded_versions(spec, tmp), "1990-03")
  expect_true(file.exists(file.path(tmp, "FAKE", "FAKE.duckdb")))
})

test_that("a year of a series without annual files loads its twelve months", {
  tmp  <- withr::local_tempdir()
  spec <- .fake_long_spec()
  t <- fake_get(spec, "1990-02", cache_path = tmp)
  fake_close(t)
  t <- fake_get(spec, "1990", cache_path = tmp)
  on.exit(fake_close(t))
  d <- dplyr::collect(t)
  expect_equal(nrow(d), 36L)
  expect_setequal(unique(d$SURVMNTH), 1:12)
  loaded <- canpumf:::.long_loaded_versions(spec, tmp)
  expect_equal(loaded, sprintf("1990-%02d", 1:12))
})

test_that("a loaded year is served without writing", {
  tmp  <- withr::local_tempdir()
  spec <- .fake_long_spec()
  fake_close(fake_get(spec, "1990", cache_path = tmp))
  spec$prepare <- function(...) stop("prepare must not run on a cache hit")
  t <- fake_get(spec, "1990", cache_path = tmp)
  on.exit(fake_close(t))
  expect_equal(nrow(dplyr::collect(t)), 36L)
  # Two read-only opens of the same file can coexist (issue #18).
  t2 <- fake_get(spec, "1990-05", cache_path = tmp)
  expect_equal(nrow(dplyr::collect(t2)), 3L)
  fake_close(t2)
})

test_that("refreshing a month replaces only that month", {
  tmp  <- withr::local_tempdir()
  spec <- .fake_long_spec()
  fake_close(fake_get(spec, "1990-01", cache_path = tmp))
  fake_close(fake_get(spec, "1990-02", cache_path = tmp))
  t <- fake_get(spec, "1990-01", cache_path = tmp, refresh = TRUE)
  fake_close(t)
  con <- DBI::dbConnect(duckdb::duckdb(), file.path(tmp, "FAKE", "FAKE.duckdb"),
                        read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  n <- DBI::dbGetQuery(con, "SELECT SURVMNTH, COUNT(*) AS n FROM fake_eng GROUP BY 1 ORDER BY 1")
  expect_equal(n$n, c(3, 3))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM fake_versions")$n, 2)
})

test_that("French table uses French labels; status reports compressed months", {
  tmp  <- withr::local_tempdir()
  spec <- .fake_long_spec()
  fake_close(fake_get(spec, "1990", cache_path = tmp))
  t <- fake_get(spec, "1990-01", lang = "fra", cache_path = tmp)
  expect_true("Terre-Neuve" %in% dplyr::collect(t)$PROV)
  fake_close(t)
  expect_message(t <- canpumf:::.long_get_pumf(spec, NULL, cache_path = tmp),
                 "1990-01..1990-12")
  fake_close(t)
})

test_that("refresh = 'auto' loads every available version", {
  tmp  <- withr::local_tempdir()
  spec <- .fake_long_spec(n_years = 1L)
  t <- fake_get(spec, refresh = "auto", cache_path = tmp)
  on.exit(fake_close(t))
  expect_equal(length(canpumf:::.long_loaded_versions(spec, tmp)), 12L)
  expect_equal(nrow(dplyr::collect(t)), 36L)
})

test_that("versions outside the series are rejected before any download", {
  tmp  <- withr::local_tempdir()
  spec <- .fake_long_spec()
  expect_error(fake_get(spec, "1995-01", cache_path = tmp), "out of range")
  expect_error(fake_get(spec, "95", cache_path = tmp), "Invalid")
})


# ---- Spec registry ----------------------------------------------------------

test_that("LFS and LFS_HIST are longitudinal; others are not", {
  expect_true(canpumf:::.is_longitudinal("LFS"))
  expect_true(canpumf:::.is_longitudinal("LFS_HIST"))
  expect_false(canpumf:::.is_longitudinal("SFS"))
  expect_false(canpumf:::.is_longitudinal(NA_character_))
  expect_equal(canpumf:::.pumf_db_path("LFS_HIST", "1990", "/c"),
               file.path("/c", "LFS_HIST", "LFS_HIST.duckdb"))
  expect_equal(canpumf:::.pumf_table_name("LFS_HIST", "1990", "fra"), "lfs_hist_fra")
  expect_equal(canpumf:::.pumf_table_name("LFS", "2020", "eng"), "lfs_eng")
  expect_equal(pumf_registry_lookup("LFS_HIST", NA_character_)$data_fixups$force_integer,
               c("SURVYEAR", "SURVMNTH", "REC_NUM"))
})

test_that("get_pumf rejects options the longitudinal pipeline does not support", {
  expect_error(get_pumf("LFS_HIST", "1990", module = "x"), "module")
  expect_error(get_pumf("LFS_HIST", "1990", borealis = "doi:10.5683/SP3/XXXX"),
               "borealis")
})


# ---- LFS_HIST versions and reference data -----------------------------------

test_that("LFS_HIST versions: 1976-01 .. 2005-12, 2006+ points to LFS", {
  v <- canpumf:::.lfs_hist_all_versions()
  expect_length(v, 360L)
  expect_equal(range(v), c("1976-01", "2005-12"))
  expect_equal(canpumf:::.lfs_hist_validate("1990"), "annual")
  expect_equal(canpumf:::.lfs_hist_validate("2005-12"), "monthly")
  expect_error(canpumf:::.lfs_hist_validate("2006-01"), "get_pumf\\(\"LFS\"")
  expect_error(canpumf:::.lfs_hist_validate("1975"), "outside")
  expect_error(canpumf:::.lfs_hist_validate("1990-13"), "outside")
  expect_error(canpumf:::.lfs_hist_validate("1990-1"), "Invalid")
})

test_that("LFS_HIST rebasing eras follow the deposit year", {
  expect_equal(canpumf:::.lfs_hist_rebased(c("1986-12", "1987-01", "1995-12",
                                              "1996-01", "2000-12", "2001-01")),
               c("", "2001", "2001", "2006", "2006", "2011"))
})

test_that("LFS_HIST 2001-2005 NOC-S names map to the canonical names", {
  expect_equal(canpumf:::.lfs_hist_canonical_names(
                 c("nocs_01_25", "NOCS_01_47", "SP_NOCS01", "Prov")),
               c("NOC01_25", "NOC01_47", "SP_NOC01", "PROV"))
})

test_that("LFS_HIST shipped reference data covers every month", {
  ds <- canpumf:::.lfs_hist_ref("datasets")
  expect_equal(ds$version, canpumf:::.lfs_hist_all_versions())
  expect_true(all(grepl("^doi:10\\.5683/", ds$doi_eng)))
  expect_equal(ds$version[is.na(ds$doi_fra)], c("1990-02", "1996-02"))

  vars  <- canpumf:::.lfs_hist_variables()
  codes <- canpumf:::.lfs_hist_ref("codes")
  expect_true(all(c("SURVYEAR", "SURVMNTH", "LFSSTAT", "PROV", "FWEIGHT") %in% vars$name))
  expect_false(anyDuplicated(vars$name) > 0L)
  expect_false(anyDuplicated(paste(codes$name, codes$val)) > 0L)
  expect_true(all(codes$name %in% vars$name))
  expect_false(any(is.na(codes$label_en)))
  # One label per code: no two codes of a variable share a label, which would
  # merge them in the ENUM.
  dup <- stats::aggregate(label_en ~ name, codes, function(x) anyDuplicated(x) > 0L)
  expect_equal(dup$name[dup$label_en], character(0L))
  expect_equal(codes$label_en[codes$name == "PROV" & codes$val == "35"], "Ontario")
})


# ---- ODESI SAS parser -------------------------------------------------------

.odesi_sas <- function(labels) {
  c("LIBNAME LIBRARY '';",
    "PROC FORMAT LIBRARY=LIBRARY ;",
    "  Value  V4_F",
    paste0("    1='", labels[1], "'"),
    paste0("    2='", labels[2], "'"),
    "  ;",
    "  Value  V5_F",
    "    10='Newfoundland'",
    "    35='Ontario'",
    "  ;",
    "RUN;",
    "DATA  OUT.lfs;",
    " INFILE 'lfs.txt' LRECL = 20;",
    " INPUT",
    "  REC_NUM 1-5  SURVYEAR 6-9  LFSSTAT 10-10  PROV 11-12  FWEIGHT 13-17",
    ";",
    "  FORMAT  LFSSTAT V4_F. ;",
    "  FORMAT  PROV V5_F. ;",
    "  LABEL",
    paste0("    REC_NUM='", labels[3], "'"),
    "    SURVYEAR='Survey year'",
    "    LFSSTAT='Labour force status'",
    "    PROV='Province'",
    "    FWEIGHT='Final weight'",
    "  ;",
    "RUN;")
}

test_that("parse_sas_odesi reads formats, layout and labels in both languages", {
  d  <- withr::local_tempdir()
  en <- file.path(d, "e.sas"); fr <- file.path(d, "f.sas")
  writeLines(.odesi_sas(c("Employed", "Unemployed, can''t work", "Record")), en)
  writeLines(.odesi_sas(c("Occupé", "Chômeur", "Enregistrement")), fr,
             useBytes = FALSE)
  m <- parse_sas_odesi(en, fr, encoding = "UTF-8")

  expect_equal(m$variables$name, c("REC_NUM", "SURVYEAR", "LFSSTAT", "PROV", "FWEIGHT"))
  expect_equal(m$variables$type[m$variables$name %in% c("LFSSTAT", "PROV")],
               c("character", "character"))
  expect_equal(m$variables$type[m$variables$name == "FWEIGHT"], "numeric")
  expect_equal(m$variables$label_fr[m$variables$name == "REC_NUM"], "Enregistrement")

  expect_equal(nrow(m$codes), 4L)
  expect_equal(m$codes$label_en[m$codes$name == "LFSSTAT" & m$codes$val == "2"],
               "Unemployed, can't work")
  expect_equal(m$codes$label_fr[m$codes$name == "LFSSTAT" & m$codes$val == "1"],
               "Occupé")

  expect_equal(m$layout$start[m$layout$name == "PROV"], 11)
  expect_equal(m$layout$end[m$layout$name == "FWEIGHT"], 17)
})

test_that("LFS_HIST MARSTAT uses the four-category labels until 1999-10", {
  old <- .lfs_hist_codes_for("1988-01")
  new <- .lfs_hist_codes_for("1999-11")
  m_old <- old[old$name == "MARSTAT", ]
  m_new <- new[new$name == "MARSTAT", ]
  expect_equal(m_old$val, c("1", "2", "3", "4"))
  expect_equal(m_old$label_en[m_old$val == "2"], "Single, never married")
  expect_equal(nrow(m_new), 6L)
  expect_equal(m_new$label_en[m_new$val == "2"], "Living in common-law")
  # other variables are untouched
  expect_identical(old[old$name != "MARSTAT", ], new[new$name != "MARSTAT", ])
})

test_that("LFS_HIST reference labels carry the curated fixes and intact French", {
  codes <- .lfs_hist_ref("codes")
  vars  <- .lfs_hist_variables()
  expect_match(codes$label_en[codes$name == "EFAMTYPE" & codes$val == "15"],
               "^Single-parent family, parent employed")
  expect_false(any(grepl("?", vars$label_fr, fixed = TRUE)))
  expect_equal(vars$label_fr[vars$name == "SURVYEAR"], "Année d'enquête")
})
