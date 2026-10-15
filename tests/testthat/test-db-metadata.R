# Tests for R/db_metadata.R: the metadata a canpumf DuckDB file carries about
# itself (issue #34).  Build stamp with provenance, stored dictionary, DuckDB
# comments, and the readers that make the file usable without metadata/.

# A built FAKE 2099 database (both languages when `fra = TRUE`), closed.
.described_db <- function(tmp, fra = FALSE) {
  make_e2e_version_dir(tmp)
  t <- suppressMessages(get_pumf("FAKE", "2099", cache_path = tmp))
  close_pumf(t)
  if (fra) {
    t <- suppressMessages(get_pumf("FAKE", "2099", lang = "fra", cache_path = tmp))
    close_pumf(t)
  }
  file.path(tmp, "FAKE", "2099", "FAKE_2099.duckdb")
}

.ro_con <- function(db, env = parent.frame()) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db, read_only = TRUE)
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE), envir = env)
  con
}


# ---- Stage 1: download manifests --------------------------------------------

test_that(".write_statcan_manifest / .read_statcan_manifest round-trip", {
  dir <- withr::local_tempdir()
  expect_null(canpumf:::.read_statcan_manifest(dir))
  canpumf:::.write_statcan_manifest(
    dir, "https://www150.statcan.gc.ca/n1/pub/x.zip", "x.zip", "SFS", "2019")
  m <- canpumf:::.read_statcan_manifest(dir)
  expect_named(m, c("url", "filename", "fetched", "series", "version"))
  expect_equal(m$url, "https://www150.statcan.gc.ca/n1/pub/x.zip")
  expect_equal(m$series, "SFS")
  expect_match(m$fetched, "^\\d{4}-\\d{2}-\\d{2} ")
})

test_that(".version_is_extracted: a manifest alone is not extracted content", {
  dir <- withr::local_tempdir()
  canpumf:::.write_statcan_manifest(dir, "u", "x.zip", "S", "v")
  writeLines("", file.path(dir, "borealis_manifest.csv"))
  expect_false(canpumf:::.version_is_extracted(dir))
  writeLines("", file.path(dir, "survey.csv"))
  expect_true(canpumf:::.version_is_extracted(dir))
})

test_that("pumf_locate_or_download: a StatCan download leaves statcan_manifest.csv", {
  tmp  <- withr::local_tempdir()
  vdir <- file.path(tmp, "FAKE", "2099")
  inner <- tempfile(fileext = ".txt")
  writeLines("survey data", inner)
  url <- "https://www150.statcan.gc.ca/n1/pub/fake-2099.zip?st=abc"
  local_mocked_bindings(
    .pumf_resolve_collection_row = function(series, version, ...)
      tibble::tibble(url = url),
    .pumf_download = function(url, destfile, ...)
      utils::zip(destfile, files = inner, flags = "-jq"),
    .pumf_warn_cache_path_on_download = function(...) invisible(NULL))
  suppressMessages(canpumf:::pumf_locate_or_download("FAKE", "2099", cache_path = tmp))
  m <- canpumf:::.read_statcan_manifest(vdir)
  expect_equal(m$url, url)
  expect_equal(m$filename, "fake-2099.zip")
  expect_equal(c(m$series, m$version), c("FAKE", "2099"))
  expect_true(file.exists(file.path(vdir, "fake-2099.zip")))
  # the manifest survives a refresh (raw files are kept)
  suppressMessages(canpumf:::pumf_locate_or_download("FAKE", "2099", cache_path = tmp,
                                                     refresh = TRUE))
  expect_equal(canpumf:::.read_statcan_manifest(vdir)$url, url)
})


# ---- .pumf_source_info --------------------------------------------------------

test_that(".pumf_source_info: deposit, statcan, borealis, and the bundle directory", {
  vdir <- withr::local_tempdir()
  src  <- canpumf:::.pumf_source_info(vdir, file.path(vdir, "data.csv"))
  expect_equal(src$source, "deposit")
  expect_equal(src$data_file, "data.csv")
  expect_equal(src$licence, "Statistics Canada Open Licence")
  expect_match(src$licence_url, "statcan.gc.ca")
  expect_true(is.na(src$source_url))

  canpumf:::.write_statcan_manifest(vdir, "https://x/y.zip", "y.zip", "S", "v")
  src <- canpumf:::.pumf_source_info(vdir)
  expect_equal(src$source, "statcan")
  expect_equal(src$source_url, "https://x/y.zip")
  expect_false(is.na(src$downloaded))
  expect_true(is.na(src$data_file))

  # a Census vintage whose versions share one archive: the manifest sits in
  # the bundle directory, the parent of the version directory
  bundle <- withr::local_tempdir()
  sub    <- file.path(bundle, "individuals")
  dir.create(sub)
  canpumf:::.write_statcan_manifest(bundle, "https://x/1986.zip", "1986.zip", "Census", "1986")
  expect_equal(canpumf:::.pumf_source_info(sub, source_dir = bundle)$source_url,
               "https://x/1986.zip")
  expect_equal(canpumf:::.pumf_source_info(sub)$source, "deposit")

  # a Borealis manifest wins and carries the deposit's licence
  readr::write_csv(tibble::tibble(
    file_id = 1L, filename = "survey.csv.gz", role = "data",
    doi = "doi:10.5683/SP3/FAKE01", title = "Fake survey",
    fetched = "2026-10-15 10:00:00",
    licence = "CC0 1.0", licence_url = "http://creativecommons.org/publicdomain/zero/1.0"),
    file.path(vdir, "borealis_manifest.csv"), na = "")
  src <- canpumf:::.pumf_source_info(vdir)
  expect_equal(src$source, "borealis")
  expect_equal(src$source_doi, "doi:10.5683/SP3/FAKE01")
  expect_equal(src$source_url, "https://doi.org/10.5683/SP3/FAKE01")
  expect_equal(src$source_title, "Fake survey")
  expect_equal(src$licence, "CC0 1.0")
  expect_equal(src$downloaded, "2026-10-15 10:00:00")
})

test_that(".pumf_source_info: a Borealis manifest without licence columns gives NA", {
  vdir <- withr::local_tempdir()
  readr::write_csv(tibble::tibble(
    file_id = 1L, filename = "survey.csv.gz", role = "data",
    doi = "doi:10.5683/SP3/FAKE01", title = "Fake survey",
    fetched = "2026-10-15 10:00:00"),
    file.path(vdir, "borealis_manifest.csv"), na = "")
  src <- canpumf:::.pumf_source_info(vdir)
  expect_equal(src$source, "borealis")
  expect_true(is.na(src$licence))
  expect_true(is.na(src$licence_url))
})


# ---- User-supplied provenance ----------------------------------------------------

.write_user_prov <- function(dir, ...) {
  v <- c(...)
  utils::write.csv(data.frame(field = names(v), value = unname(v),
                              stringsAsFactors = FALSE),
                   file.path(dir, "pumf_provenance.csv"), row.names = FALSE)
}

test_that(".read_user_provenance: file round-trip, blanks, unknown fields, shape errors", {
  dir <- withr::local_tempdir()
  expect_null(canpumf:::.read_user_provenance(dir))
  .write_user_prov(dir, licence = "CC BY 4.0", citation = " Doe (2020) ",
                   note = "")
  u <- canpumf:::.read_user_provenance(dir)
  expect_equal(u, list(licence = "CC BY 4.0", citation = "Doe (2020)"))
  .write_user_prov(dir, licence = "CC0", bogus = "x")
  expect_warning(u <- canpumf:::.read_user_provenance(dir),
                 "unknown field\\(s\\) ignored: bogus")
  expect_equal(u, list(licence = "CC0"))
  .write_user_prov(dir, licence = "a", licence = "b")
  expect_error(canpumf:::.read_user_provenance(dir), "more than once: licence")
  writeLines(c("a,b", "1,2"), file.path(dir, "pumf_provenance.csv"))
  expect_warning(expect_null(canpumf:::.read_user_provenance(dir)),
                 "needs the columns 'field' and 'value'")
})

test_that(".pumf_source_info: the file wins over the registry, both over the manifest", {
  vdir <- withr::local_tempdir()
  canpumf:::.write_statcan_manifest(
    vdir, "https://www150.statcan.gc.ca/n1/pub/x.zip", "x.zip", "FAKE", "2099")
  src <- canpumf:::.pumf_source_info(vdir, "d.csv")
  expect_equal(src$source, "statcan")
  expect_true(is.na(src$citation))
  expect_true(is.na(src$note))
  expect_true(is.na(src$user_fields))
  src <- canpumf:::.pumf_source_info(
    vdir, "d.csv", provenance = list(licence = "CC0", note = "from registry"))
  expect_equal(src$licence, "CC0")
  expect_equal(src$note, "from registry")
  expect_equal(src$source_url, "https://www150.statcan.gc.ca/n1/pub/x.zip")
  expect_equal(src$user_fields, "licence, note")
  .write_user_prov(vdir, licence = "CC BY 4.0", citation = "Doe (2020)",
                   source = "deposit")
  src <- canpumf:::.pumf_source_info(
    vdir, "d.csv", provenance = list(licence = "CC0", note = "from registry"))
  expect_equal(src$licence, "CC BY 4.0")
  expect_equal(src$citation, "Doe (2020)")
  expect_equal(src$source, "deposit")
  expect_equal(src$note, "from registry")
  expect_equal(src$user_fields, "licence, citation, source, note")
  # the bundle directory is searched when the version directory has no file
  bundle <- withr::local_tempdir()
  sub    <- file.path(bundle, "individuals"); dir.create(sub)
  .write_user_prov(bundle, citation = "bundle cite")
  expect_equal(canpumf:::.pumf_source_info(sub, source_dir = bundle)$citation,
               "bundle cite")
  .write_user_prov(sub, citation = "version cite")
  expect_equal(canpumf:::.pumf_source_info(sub, source_dir = bundle)$citation,
               "version cite")
})

test_that("pumf_provenance.csv is neither extracted content nor a data file", {
  dir <- withr::local_tempdir()
  .write_user_prov(dir, licence = "CC0")
  expect_false(canpumf:::.version_is_extracted(dir))
  writeLines("PROV,WEIGHT", file.path(dir, "survey.csv"))
  expect_true(canpumf:::.version_is_extracted(dir))
  expect_equal(basename(canpumf:::.find_pumf_data_file(dir, NULL)), "survey.csv")
})

test_that("pumf_registry_entry(provenance =): validation, print, lookup merge", {
  e <- pumf_registry_entry(provenance = list(licence = "CC0", citation = "X"))
  expect_equal(e$provenance, list(licence = "CC0", citation = "X"))
  expect_output(print(e), "licence = CC0, citation = X")
  expect_error(pumf_registry_entry(provenance = "CC0"), "named list")
  expect_error(pumf_registry_entry(provenance = list(licence = c("a", "b"))),
               "single string")
  expect_error(pumf_registry_entry(provenance = list("CC0")), "named list")
  expect_warning(pumf_registry_entry(provenance = list(licence = "CC0", foo = "1")),
                 "unknown field\\(s\\) ignored: foo")
  expect_true("provenance" %in% canpumf:::.pumf_registry_fields)
  expect_null(pumf_registry("FAKE", "2099")$provenance)
  canpumf:::.pumf_registry_override_set("FAKE", "2099", e)
  withr::defer(canpumf:::.pumf_registry_override_clear("FAKE", "2099"))
  expect_equal(pumf_registry_lookup("FAKE", "2099")$provenance$licence, "CC0")
})

test_that("Stage 3 stamps the user's provenance; a change needs refresh = TRUE", {
  tmp  <- withr::local_tempdir()
  make_e2e_version_dir(tmp)
  vdir <- file.path(tmp, "FAKE", "2099")
  .write_user_prov(vdir, source = "deposit", licence = "CC BY 4.0",
                   licence_url = "https://creativecommons.org/licenses/by/4.0/",
                   source_title = "A survey deposited by hand",
                   citation = "Doe, J. (2020). A survey.")
  reg <- pumf_registry_entry(provenance = list(licence = "ignored, file wins",
                                               note = "registry note"))
  t <- suppressMessages(get_pumf("FAKE", "2099", cache_path = tmp, registry = reg))
  prov <- dplyr::collect(pumf_sidecar(t, "provenance"))
  expect_equal(prov$source, "deposit")
  expect_equal(prov$licence, "CC BY 4.0")
  expect_equal(prov$source_title, "A survey deposited by hand")
  expect_equal(prov$citation, "Doe, J. (2020). A survey.")
  expect_equal(prov$note, "registry note")
  expect_equal(prov$user_fields,
               "source, licence, licence_url, source_title, citation, note")
  expect_match(canpumf:::.pumf_table_comment_of(dbplyr::remote_con(t), "eng"),
               "deposited by hand")
  close_pumf(t)
  lc <- list_pumf_cache(tmp)
  expect_equal(lc$source, "deposit")
  expect_equal(lc$licence, "CC BY 4.0")
  # the file is read at build time only
  .write_user_prov(vdir, citation = "Changed (2021)")
  t <- suppressMessages(get_pumf("FAKE", "2099", cache_path = tmp))
  expect_equal(dplyr::collect(pumf_sidecar(t, "provenance"))$citation,
               "Doe, J. (2020). A survey.")
  close_pumf(t)
  t <- suppressMessages(get_pumf("FAKE", "2099", cache_path = tmp, refresh = TRUE))
  prov <- dplyr::collect(pumf_sidecar(t, "provenance"))
  expect_equal(prov$citation, "Changed (2021)")
  expect_equal(prov$licence, "Statistics Canada Open Licence")
  expect_equal(prov$user_fields, "citation")
  close_pumf(t)
  expect_true(file.exists(file.path(vdir, "pumf_provenance.csv")))
})

test_that(".long_source_info: the entry's provenance is laid over the default", {
  spec <- list(entry = list(provenance = list(citation = "LFS cite",
                                              licence = "Custom")))
  src <- canpumf:::.long_source_info(spec)
  expect_equal(src$source, "statcan")
  expect_equal(src$licence, "Custom")
  expect_equal(src$citation, "LFS cite")
  expect_equal(src$user_fields, "citation, licence")
  expect_true(is.na(canpumf:::.long_source_info(list(entry = list()))$user_fields))
})


# ---- Build stamp -------------------------------------------------------------

test_that("Stage 3 stamps the table with its provenance", {
  tmp <- withr::local_tempdir()
  db  <- .described_db(tmp)
  con <- .ro_con(db)
  info <- DBI::dbReadTable(con, "pumf_build_info")
  expect_equal(nrow(info), 1L)
  expect_true(all(canpumf:::.build_info_cols %in% names(info)))
  expect_equal(info$table, "eng")
  expect_equal(info$canpumf_version, as.character(utils::packageVersion("canpumf")))
  expect_equal(info$duckdb_version, as.character(utils::packageVersion("duckdb")))
  expect_equal(c(info$series, info$version, info$lang), c("FAKE", "2099", "eng"))
  expect_true(is.na(info$module))
  expect_equal(info$source, "deposit")
  expect_equal(info$data_file, "survey.csv")
  expect_equal(info$licence, "Statistics Canada Open Licence")
  expect_true(is.na(info$fixups))
  # the same row through the sidecar accessor
  t <- get_pumf("FAKE", "2099", cache_path = tmp)
  on.exit(close_pumf(t), add = TRUE)
  prov <- dplyr::collect(pumf_sidecar(t, "provenance"))
  expect_equal(nrow(prov), 1L)
  expect_equal(prov$source, "deposit")
  expect_error(pumf_sidecar(t, "provenance", join = TRUE), "no records to join")
  l <- pumf_sidecar(t)
  expect_true("provenance" %in% l$sidecar)
  expect_equal(l$kind[l$sidecar == "provenance"], "info")
  expect_equal(l$n_rows[l$sidecar == "provenance"], 1L)
})

test_that(".write_build_info: a second language adds a row; an old stamp table gains columns", {
  tmp <- withr::local_tempdir()
  db  <- .described_db(tmp, fra = TRUE)
  con <- .ro_con(db)
  info <- DBI::dbReadTable(con, "pumf_build_info")
  expect_setequal(info$table, c("eng", "fra"))
  expect_equal(info$lang[info$table == "fra"], "fra")

  # a stamp table of the first 0.7.0 shape (four columns) is widened in place
  db2  <- file.path(withr::local_tempdir(), "old.duckdb")
  wcon <- DBI::dbConnect(duckdb::duckdb(), dbdir = db2)
  on.exit(DBI::dbDisconnect(wcon, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(wcon, "pumf_build_info", data.frame(
    table = "eng", canpumf_version = "0.7.0", duckdb_version = "1.0.0",
    built = "2026-09-26 00:00:00"))
  canpumf:::.write_build_info(wcon, "fra", list(series = "S", lang = "fra",
                                                source = "statcan"))
  info <- DBI::dbReadTable(wcon, "pumf_build_info")
  expect_true(all(canpumf:::.build_info_cols %in% names(info)))
  expect_equal(info$source[info$table == "fra"], "statcan")
  expect_true(is.na(info$source[info$table == "eng"]))
  # rewriting a table replaces its row
  canpumf:::.write_build_info(wcon, "fra", list(series = "S", lang = "fra",
                                                source = "borealis"))
  info <- canpumf:::.read_build_info(wcon, "fra")
  expect_equal(nrow(info), 1L)
  expect_equal(info$source, "borealis")
})

test_that(".pumf_stamp_info / .pumf_table_comment: fixups and the one-line description", {
  src <- canpumf:::.pumf_source_info(withr::local_tempdir(), "d.csv")
  s <- canpumf:::.pumf_stamp_info("GSS", "Cycle 16 (2002)", "eng", "cg4", src,
                                  list(rename_columns = list(), labels_as_description = TRUE))
  expect_equal(s$module, "cg4")
  expect_equal(s$fixups, "rename_columns, labels_as_description")
  expect_equal(s$data_file, "d.csv")
  expect_equal(canpumf:::.pumf_table_comment("FAKE", "2099", "eng", NULL, src),
               "FAKE 2099 [eng]")
  src$source_title <- "Fake survey"
  expect_equal(canpumf:::.pumf_table_comment("FAKE", "2099", "fra", "cg4", src),
               "FAKE 2099 cg4 [fra] - Fake survey")
})


# ---- Dictionary table ---------------------------------------------------------

test_that("Stage 3 stores the dictionary; pumf_dictionary() reads it back", {
  tmp <- withr::local_tempdir()
  db  <- .described_db(tmp, fra = TRUE)
  con <- .ro_con(db)
  stored <- DBI::dbReadTable(con, "pumf_dictionary")
  expect_true(all(c("table", canpumf:::.dictionary_cols) %in% names(stored)))
  expect_setequal(unique(stored$table), c("eng", "fra"))
  eng <- stored[stored$table == "eng", ]
  expect_true(all(c("PROV", "WEIGHT") %in% eng$name))
  expect_equal(eng$applied_as[eng$name == "PROV" & !is.na(eng$val)], c("level", "level"))

  t <- get_pumf("FAKE", "2099", cache_path = tmp)
  on.exit(close_pumf(t), add = TRUE)
  d <- pumf_dictionary(t)
  expect_true(isTRUE(attr(d, "applied_recorded")))
  attr(d, "applied_recorded") <- NULL
  expect_equal(d, canpumf:::.read_dictionary(con, "eng"))
  expect_equal(nrow(pumf_dictionary(t, what = "topcodes")), 0L)
})

test_that("the database stays usable after metadata/ is deleted", {
  tmp <- withr::local_tempdir()
  db  <- .described_db(tmp, fra = TRUE)
  t   <- get_pumf("FAKE", "2099", cache_path = tmp)
  d0  <- pumf_dictionary(t)
  close_pumf(t)
  unlink(file.path(tmp, "FAKE", "2099", "metadata"), recursive = TRUE)

  expect_no_message(t <- get_pumf("FAKE", "2099", cache_path = tmp),
                    message = "development version")
  on.exit(close_pumf(t), add = TRUE)
  expect_equal(pumf_dictionary(t), d0)
  expect_equal(pumf_dictionary("FAKE", "2099", cache_path = tmp), d0)
  expect_equal(nrow(pumf_dictionary(t, what = "topcodes")), 0L)
  expect_equal(colnames(label_pumf_columns(t)),
               c("pumf_row_id", "Province", "Survey weight"))
  df <- collect_pumf(t)
  expect_equal(attr(df$PROV, "label"), "Province")
  fr <- pumf_translate(df, "fra")
  expect_equal(levels(fr$PROV), c("Terre-Neuve", "Ontario"))
  expect_equal(names(label_pumf_columns(fr)), c("pumf_row_id", "Province", "Poids"))
})

test_that("a stamped table without a stored dictionary falls back to metadata/", {
  tmp <- withr::local_tempdir()
  db  <- .described_db(tmp)
  t   <- get_pumf("FAKE", "2099", cache_path = tmp)
  d0  <- pumf_dictionary(t)
  close_pumf(t)
  wcon <- DBI::dbConnect(duckdb::duckdb(), dbdir = db)
  DBI::dbRemoveTable(wcon, "pumf_dictionary")
  DBI::dbDisconnect(wcon, shutdown = TRUE)
  t <- get_pumf("FAKE", "2099", cache_path = tmp)
  on.exit(close_pumf(t), add = TRUE)
  expect_equal(pumf_dictionary(t), d0)
  expect_equal(colnames(label_pumf_columns(t)),
               c("pumf_row_id", "Province", "Survey weight"))
})


# ---- Comments ------------------------------------------------------------------

test_that("Stage 3 comments every labelled column and the table", {
  tmp <- withr::local_tempdir()
  db  <- .described_db(tmp, fra = TRUE)
  con <- .ro_con(db)
  expect_equal(canpumf:::.pumf_column_comments(con, "eng"),
               c(pumf_row_id = "Record position in the data file (1-based)",
                 PROV = "Province", WEIGHT = "Survey weight"))
  expect_equal(canpumf:::.pumf_column_comments(con, "fra")[["WEIGHT"]], "Poids")
  expect_equal(canpumf:::.pumf_table_comment_of(con, "eng"), "FAKE 2099 [eng]")
  expect_equal(canpumf:::.pumf_table_comment_of(con, "fra"), "FAKE 2099 [fra]")
  # the sentinel sidecar is not commented
  expect_true(is.na(canpumf:::.pumf_table_comment_of(con, "pumf_sentinels_eng")))
})


# ---- Readers: provenance without the registry ----------------------------------

test_that("a tbl opened with DBI::dbConnect() gets its provenance from the stamp", {
  tmp <- withr::local_tempdir()
  db  <- .described_db(tmp, fra = TRUE)
  con <- .ro_con(db)
  x <- dplyr::tbl(con, "fra")
  prov <- canpumf:::.pumf_tbl_prov(x)
  expect_equal(c(prov$series, prov$version, prov$lang), c("FAKE", "2099", "fra"))
  expect_equal(normalizePath(prov$cache_path), normalizePath(tmp))
  expect_null(prov$module)
  # and is registered from then on
  expect_false(is.null(canpumf:::.pumf_lookup_con(con)))
  expect_equal(colnames(label_pumf_columns(x)), c("pumf_row_id", "Province", "Poids"))
  expect_equal(pumf_sidecar(x)$sidecar, c("sentinels", "provenance"))
  expect_equal(nrow(dplyr::collect(pumf_sidecar(x, "sentinels"))), 1L)
  expect_equal(pumf_dictionary(x)$label_fr[pumf_dictionary(x)$name == "WEIGHT"], "Poids")
})

test_that(".pumf_prov_from_db: a file outside a cache layout has no cache_path", {
  db <- file.path(withr::local_tempdir(), "loose.duckdb")
  wcon <- DBI::dbConnect(duckdb::duckdb(), dbdir = db)
  DBI::dbWriteTable(wcon, "eng", data.frame(pumf_row_id = 1:2, X = 1:2))
  canpumf:::.write_build_info(wcon, "eng", list(series = "S", version = "v", lang = NA))
  DBI::dbDisconnect(wcon, shutdown = TRUE)
  con <- .ro_con(db)
  prov <- canpumf:::.pumf_prov_from_db(con, dplyr::tbl(con, "eng"))
  expect_equal(prov$series, "S")
  expect_equal(prov$version, "v")
  expect_equal(prov$lang, "eng")   # NA lang defaults to English
  expect_true(is.na(prov$cache_path))
  # a table without a stamp row is still an error
  expect_null(canpumf:::.pumf_prov_from_db(con, dplyr::tbl(con, "pumf_build_info")))
  expect_error(canpumf:::.pumf_tbl_prov(dplyr::tbl(con, "pumf_build_info")), "provenance")
})

test_that("the readers never open a write connection", {
  tmp <- withr::local_tempdir()
  db  <- .described_db(tmp)
  ro  <- .ro_con(db)                                  # a read-only hold
  # by series/version there is no tbl: a transient read-only connection
  expect_s3_class(pumf_dictionary("FAKE", "2099", cache_path = tmp), "data.frame")
  expect_true(DBI::dbIsValid(ro))
  expect_equal(nrow(DBI::dbReadTable(ro, "eng")), 3L)
})


# ---- list_pumf_cache: source and licence ----------------------------------------

test_that("list_pumf_cache: reports source and licence from the stamp", {
  tmp <- withr::local_tempdir()
  .described_db(tmp)
  r <- list_pumf_cache(cache_path = tmp)
  expect_true(all(c("source", "licence") %in% names(r)))
  expect_equal(r$source, "deposit")
  expect_equal(r$licence, "Statistics Canada Open Licence")
})
