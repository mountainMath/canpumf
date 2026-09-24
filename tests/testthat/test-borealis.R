# Borealis Dataverse as a second PUMF source (R/borealis.R).
# Offline tests cover DOI handling, file selection, the download manifest, the
# StatCan-first Census resolution and registry integration.  The network tests
# at the end talk to borealisdata.ca and are skipped on CRAN / offline.

# ---- DOI handling -----------------------------------------------------------

test_that(".borealis_normalize_doi accepts the common DOI spellings", {
  norm <- canpumf:::.borealis_normalize_doi
  want <- "doi:10.5683/SP3/LG7WKC"
  expect_equal(norm("doi:10.5683/SP3/LG7WKC"), want)
  expect_equal(norm("DOI:10.5683/SP3/LG7WKC"), want)
  expect_equal(norm("10.5683/SP3/LG7WKC"), want)
  expect_equal(norm(" https://doi.org/10.5683/SP3/LG7WKC "), want)
  expect_equal(norm("https://dx.doi.org/10.5683/SP3/LG7WKC"), want)
  expect_equal(norm(paste0("https://borealisdata.ca/dataset.xhtml?persistentId=",
                           "doi%3A10.5683%2FSP3%2FLG7WKC")), want)
})

test_that(".borealis_normalize_doi rejects non-DOIs", {
  norm <- canpumf:::.borealis_normalize_doi
  expect_error(norm("LG7WKC"), "does not look like a DOI")
  expect_error(norm(""), "single string")
  expect_error(norm(c("doi:10.5683/SP3/A", "doi:10.5683/SP3/B")), "single string")
  expect_error(norm(NA_character_), "single string")
})

test_that(".borealis_doi_arg takes a DOI or one catalogue row", {
  arg <- canpumf:::.borealis_doi_arg
  row <- tibble::tibble(title = "x", doi = "doi:10.5683/SP3/LG7WKC")
  expect_equal(arg(row), "doi:10.5683/SP3/LG7WKC")
  expect_equal(arg("10.5683/SP3/LG7WKC"), "doi:10.5683/SP3/LG7WKC")
  expect_error(arg(rbind(row, row)), "single row")
  expect_error(arg(tibble::tibble(title = "x")), "single row")
})

test_that("the API key is sent only when BOREALIS_DATAVERSE_KEY is set", {
  withr::with_envvar(c(BOREALIS_DATAVERSE_KEY = ""), {
    expect_null(canpumf:::.borealis_token())
    expect_length(canpumf:::.borealis_headers()$headers, 0L)
  })
  withr::with_envvar(c(BOREALIS_DATAVERSE_KEY = "abc"), {
    expect_equal(canpumf:::.borealis_token(), "abc")
    expect_equal(unname(canpumf:::.borealis_headers()$headers), "abc")
  })
})

# ---- File selection ---------------------------------------------------------

.fake_listing <- function(filename, size = 1e7, directory = NA_character_,
                          original = NA_character_, restricted = FALSE) {
  n <- length(filename)
  tibble::tibble(
    file_id      = seq_len(n),
    filename     = filename,
    directory    = rep_len(directory, n),
    size         = rep_len(size, n),
    md5          = NA_character_,
    content_type = NA_character_,
    original     = rep_len(original, n),
    restricted   = rep_len(restricted, n))
}

test_that(".borealis_select_files: ODESI Census layout picks CSV + .sps", {
  # The shape of a 1971 Census dataset: CSV and FWF copies of the data, SPSS
  # and SAS command files, Stata, a Dataverse .tab ingest, and documentation.
  files <- .fake_listing(
    c("pumf-95M00-E-1971-individual-cma_F1.csv", "indiv71_cma.dat",
      "indiv1971_cma_eng.sps", "indiv1971_cma_eng.sas",
      "indiv1971_cma.dta", "indiv1971_cma.tab",
      "pumf1971rcl_e.pdf", "pumf71i_codebook.txt", "indiv71.missRecode"),
    size     = c(8e6, 9e6, 5e4, 5e4, 8e6, 8e6, 2e6, 3e5, 1e3),
    original = c(NA, NA, NA, NA, NA, "indiv1971_cma.dta", NA, NA, NA))
  sel  <- canpumf:::.borealis_select_files(files)
  role <- stats::setNames(sel$role, sel$filename)

  expect_equal(unname(role["pumf-95M00-E-1971-individual-cma_F1.csv"]), "data")
  expect_equal(sum(sel$role == "data"), 1L)
  expect_equal(unname(role["indiv1971_cma_eng.sps"]), "metadata")
  # SPSS wins; the SAS command file is not needed alongside it
  expect_equal(unname(role["indiv1971_cma_eng.sas"]), "skip")
  expect_equal(unname(role[c("pumf1971rcl_e.pdf", "pumf71i_codebook.txt")]),
               c("doc", "doc"))
  expect_true(all(role[c("indiv71_cma.dat", "indiv1971_cma.dta",
                         "indiv1971_cma.tab", "indiv71.missRecode")] == "skip"))
  expect_identical(sel$selected, sel$role != "skip")
})

test_that(".borealis_select_files: FWF fallback and .sav metadata", {
  files <- .fake_listing(c("survey.txt", "survey.sav", "guide.pdf"),
                         size = c(5e7, 1e6, 1e6))
  sel <- canpumf:::.borealis_select_files(files)
  expect_equal(sel$role, c("data", "metadata", "doc"))
})

test_that(".borealis_select_files: oversized docs and restricted files skipped", {
  files <- .fake_listing(c("data.csv", "cmd.sps", "huge.pdf", "secret.pdf"),
                         size = c(1e7, 1e4, 2e8, 1e5),
                         restricted = c(FALSE, FALSE, FALSE, TRUE))
  withr::with_envvar(c(BOREALIS_DATAVERSE_KEY = ""), {
    sel <- canpumf:::.borealis_select_files(files, max_doc_mb = 50)
  })
  expect_equal(sel$role, c("data", "metadata", "skip", "skip"))
  withr::with_envvar(c(BOREALIS_DATAVERSE_KEY = "abc"), {
    sel <- canpumf:::.borealis_select_files(files, max_doc_mb = 50)
  })
  expect_equal(sel$role[[4L]], "doc")
})

# ---- StatCan-availability flag ---------------------------------------------

test_that(".borealis_norm_series strips years, suffixes and articles", {
  norm <- canpumf:::.borealis_norm_series
  expect_equal(norm("General Social Survey, Cycle 18, 2004 [Canada]: Main File"),
               "general social survey cycle 18 2004 canada main file")
  expect_equal(norm("1971 Census of Canada (Individuals)"),
               "census of population")
  expect_equal(norm("The Labour Force Survey Public Use Microdata File"),
               "labour force survey")
})

test_that(".borealis_edition_years expands ranges and ignores brackets", {
  yrs <- function(x) sort(canpumf:::.borealis_edition_years("", x))
  expect_equal(yrs("Canadian Community Health Survey, 2011-2012"), c(2011, 2012))
  expect_equal(yrs("Labour Force Survey, 1976–1978"), 1976:1978)
  expect_equal(yrs("GSS, Cycle 18, 2004 [Canada 2006 revision]"), 2004)
  expect_length(yrs("No year here"), 0L)
})

test_that(".borealis_match_statcan flags datasets StatCan also posts", {
  sc <- tibble::tibble(
    Acronym      = c("GSS", "GSS", "CCHS", "CCHS", "LFS", "EFT"),
    SeriesTitle  = c("General Social Survey", "General Social Survey",
                     "Canadian Community Health Survey",
                     "Canadian Community Health Survey",
                     "Labour Force Survey", "Census of Population"),
    Title        = c("General Social Survey — Cycle 18 (2004)",
                     "General Social Survey — CSGVP (2004)",
                     "Canadian Community Health Survey — 2011-2012",
                     "Canadian Community Health Survey — 2012",
                     "Labour Force Survey — 2019",
                     "Census of Population — 1981"),
    edition      = c("2004", "2004", "2011-2012", "2012", "2019", "1981"),
    catalogue_id = c("45250001", "45250001", "82M0013X", "82M0013X",
                     "71M0001X", ""),
    url          = c(rep("https://www150.statcan.gc.ca/x", 5), "EFT"))
  bor <- tibble::tibble(
    title = c("General Social Survey, Cycle 18, 2004 [Canada]: Main File",
              "Enquête sociale générale, Cycle 18, 2004 [Canada]",
              "Canadian Community Health Survey, 2012: Mental Health Component",
              "Canadian Community Health Survey, 2011-2012: Annual Component",
              "Some LFS release 2019",
              "Labour Force Survey, 1976",
              "1981 Census of Canada: Individuals",
              "General Social Survey, Cycle 19, 2005 [Canada]"),
    alt_title = c(NA, "General Social Survey, Cycle 18, 2004", NA, NA, NA, NA,
                  NA, NA),
    other_id  = c(NA, NA, NA, NA, "71M0001XCB", "71M0001XCB", NA, NA))
  out <- canpumf:::.borealis_match_statcan(bor, sc)
  expect_equal(out$statcan, c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
  # Same cycle beats the CSGVP row that shares series and year.
  expect_equal(out$statcan_title[1:2],
               rep("General Social Survey — Cycle 18 (2004)", 2))
  # Year sets must agree exactly: 2012 is not 2011-2012.
  expect_equal(out$statcan_title[3:4],
               c("Canadian Community Health Survey — 2012",
                 "Canadian Community Health Survey — 2011-2012"))
  # Catalogue number in otherId matches without a title match.
  expect_equal(out$statcan_series[[5]], "LFS")
  # EFT-only rows are not direct downloads.
  expect_true(is.na(out$statcan_series[[7]]))
  expect_false(any(canpumf:::.borealis_match_statcan(bor, NULL)$statcan))
})

test_that("an explicit Borealis load warns when StatCan has the data", {
  env <- canpumf:::.borealis_catalogue_cache
  old <- env$data
  on.exit(env$data <- old, add = TRUE)
  env$data <- tibble::tibble(
    doi = c("doi:10.5683/SP/AAAAAA", "doi:10.5683/SP/BBBBBB"),
    title = c("GSS Cycle 18", "Census 1971"),
    statcan = c(TRUE, FALSE), statcan_series = c("GSS", NA),
    statcan_title = c("General Social Survey — Cycle 18 (2004)", NA))
  warn <- canpumf:::.borealis_warn_statcan_available
  expect_warning(warn("doi:10.5683/SP/AAAAAA"), "also available directly")
  expect_no_warning(warn("doi:10.5683/SP/BBBBBB"))
  expect_no_warning(warn("doi:10.5683/SP/CCCCCC"))
})

# ---- Manifest ---------------------------------------------------------------

test_that("the manifest pins the data file as an anchored, escaped mask", {
  vdir <- withr::local_tempdir()
  expect_null(canpumf:::.borealis_manifest_doi(vdir))
  expect_null(canpumf:::.borealis_manifest_file_mask(vdir))

  readr::write_csv(tibble::tibble(
    doi       = "doi:10.5683/SP3/LG7WKC",
    title     = "Census 1971",
    file_id   = c("1", "2"),
    filename  = c("pumf (1971).csv", "cmd.sps"),
    role      = c("data", "metadata"),
    md5       = NA_character_,
    data_file = c("pumf (1971).csv", NA),
    fetched   = "2026-09-23 10:00:00"),
    file.path(vdir, "borealis_manifest.csv"), na = "")

  expect_equal(canpumf:::.borealis_manifest_doi(vdir), "doi:10.5683/SP3/LG7WKC")
  mask <- canpumf:::.borealis_manifest_file_mask(vdir)
  expect_true(grepl(mask, "pumf (1971).csv"))
  expect_false(grepl(mask, "pumf (1971)Xcsv"))
  expect_false(grepl(mask, "old pumf (1971).csv"))
})

test_that("a version loaded with borealis = keeps that source when reopened", {
  cache <- withr::local_tempdir()
  write_manifest <- function(series, version, doi) {
    vdir <- file.path(cache, series, version)
    dir.create(vdir, recursive = TRUE)
    readr::write_csv(tibble::tibble(doi = doi, file_id = "1", filename = "d.csv",
                                    role = "data", data_file = "d.csv"),
                     file.path(vdir, "borealis_manifest.csv"), na = "")
  }
  # No manifest: the configured source applies.
  expect_null(canpumf:::.borealis_cached_doi("SHS", "1997", cache))

  # A StatCan survey loaded from Borealis reopens from Borealis.
  write_manifest("SHS", "1997", "doi:10.5683/SP3/EZXFNL")
  expect_equal(canpumf:::.borealis_cached_doi("SHS", "1997", cache),
               "doi:10.5683/SP3/EZXFNL")

  # A registry entry that already points at the cached DOI needs no override,
  # so its data_fixups stay in force.
  key <- "1971 (individuals, CMA)"
  reg_doi <- canpumf:::.borealis_entry_doi(
    canpumf:::.pumf_registry[[paste0("Census/", key)]])
  write_manifest("Census", key, reg_doi)
  expect_null(canpumf:::.borealis_cached_doi("Census", key, cache))

  expect_null(canpumf:::.borealis_cached_doi("LFS", "2020", cache))
  expect_null(canpumf:::.borealis_cached_doi("SHS", "1997", NULL))
})

# ---- Census resolution: StatCan/EFT first, Borealis otherwise ----------------

test_that("pumf_resolve_version: pre-1991 Census falls back to Borealis", {
  rv    <- canpumf:::pumf_resolve_version
  empty <- withr::local_tempdir()
  expect_equal(rv("Census", "1971", empty), "1971 (individuals, provincial)")
  expect_equal(rv("Census", "1971/cma", empty), "1971 (individuals, CMA)")
  expect_equal(rv("Census", "1971 households CMA", empty),
               "1971 (households, CMA)")
  expect_equal(rv("Census", "1986 families", empty), "1986 (families)")
  expect_equal(rv("Census", "1981", empty), "1981 (individuals)")
  # full keys pass through unchanged
  expect_equal(rv("Census", "1976 (households)", empty), "1976 (households)")
  expect_equal(rv("Census", "1976/households", empty), "1976/households")
  # explicit source keywords
  expect_equal(rv("Census", "1986 families eft", empty), "1986/families")
  # StatCan-downloadable vintages are unaffected
  expect_equal(rv("Census", "1991", empty), "1991 (individuals)")
})

test_that("pumf_resolve_version: a deposited EFT bundle takes precedence", {
  rv    <- canpumf:::pumf_resolve_version
  cache <- withr::local_tempdir()
  dir.create(file.path(cache, "Census", "1971"), recursive = TRUE)
  file.create(file.path(cache, "Census", "1971", "1971PUMF_FMGD.zip"))
  expect_equal(rv("Census", "1971", cache), "1971/individuals_prov")
  expect_equal(rv("Census", "1971 households CMA", cache), "1971/households_cma")
  expect_equal(rv("Census", "1971 borealis", cache),
               "1971 (individuals, provincial)")
  # other years still resolve to Borealis
  expect_equal(rv("Census", "1986 families", cache), "1986 (families)")
})

test_that("EFT entries point at their Borealis twin", {
  alt <- canpumf:::.pumf_borealis_alternative
  expect_equal(alt("Census", "1971/individuals_cma"), "1971 (individuals, CMA)")
  expect_equal(alt("Census", "1986/families"), "1986 (families)")
  expect_null(alt("Census", "2021 (individuals)"))
  expect_null(alt("SFS", "2019"))
})

# ---- Registry integration -----------------------------------------------------

.borealis_keys <- function() {
  reg <- canpumf:::.pumf_registry
  names(reg)[vapply(reg, function(e) !is.null(e$borealis), logical(1L))]
}

test_that("every Borealis registry entry has a valid, unique DOI", {
  keys <- .borealis_keys()
  expect_length(keys, 14L)
  dois <- vapply(canpumf:::.pumf_registry[keys], canpumf:::.borealis_entry_doi,
                 character(1L))
  expect_true(all(grepl("^doi:10\\.5683/SP3/[A-Z0-9]{6}$", dois)))
  expect_false(anyDuplicated(dois) > 0L)
})

test_that("list_canpumf_collection rows for Borealis entries", {
  rows <- canpumf:::.borealis_registry_collection()
  expect_equal(nrow(rows), 14L)
  expect_true(all(rows$Acronym == "Census"))
  expect_true(all(startsWith(rows$url, "https://borealisdata.ca/dataset.xhtml")))
  expect_true("1971 (individuals, CMA)" %in% rows$Version)
})

test_that("pumf_registry_entry validates the borealis field", {
  e <- pumf_registry_entry(borealis = "10.5683/SP3/LG7WKC")
  expect_equal(e$borealis, list(doi = "10.5683/SP3/LG7WKC"))
  expect_error(pumf_registry_entry(borealis = "nonsense"), "DOI")
  expect_error(pumf_registry_entry(borealis = list(files = 1)), "DOI string")
  expect_error(pumf_registry_entry(
    borealis = list(doi = "doi:10.5683/SP3/LG7WKC", bogus = 1)),
    "Unrecognised borealis")
})

test_that("an explicit Borealis DOI drops a registered entry's own config", {
  lookup <- canpumf:::pumf_registry_lookup
  ver    <- "1986 (families)"
  own    <- canpumf:::.borealis_entry_doi(lookup("Census", ver))
  withr::defer(canpumf:::.pumf_registry_override_clear("Census", ver))

  # same DOI: the built-in fixups stay
  canpumf:::.pumf_registry_override_set("Census", ver, structure(
    list(borealis = list(doi = own, explicit = TRUE)),
    class = "pumf_registry_entry"))
  expect_true("AGEM" %in% lookup("Census", ver)$data_fixups$force_numeric)

  # another dataset: calibrated fixups no longer apply
  canpumf:::.pumf_registry_override_set("Census", ver, structure(
    list(borealis = list(doi = "doi:10.5683/SP3/XXXXXX", explicit = TRUE)),
    class = "pumf_registry_entry"))
  e <- lookup("Census", ver)
  expect_length(e$data_fixups, 0L)
  expect_equal(e$borealis$doi, "doi:10.5683/SP3/XXXXXX")
})

test_that("get_pumf(borealis =) argument checks", {
  expect_error(get_pumf("LFS", "2023", borealis = "doi:10.5683/SP3/LG7WKC"),
               "not supported for LFS")
  expect_error(get_pumf("Census", borealis = "doi:10.5683/SP3/LG7WKC"),
               "'version' must be specified")
})

# ---- Network ----------------------------------------------------------------

test_that("list_borealis_pumf_files classifies a live Census dataset", {
  skip_on_cran()
  skip_if_offline("borealisdata.ca")
  files <- tryCatch(list_borealis_pumf_files("doi:10.5683/SP3/LG7WKC"),
                    canpumf_network_error = function(e) skip(conditionMessage(e)))
  expect_equal(attr(files, "doi"), "doi:10.5683/SP3/LG7WKC")
  data <- files$filename[files$role == "data"]
  expect_length(data, 1L)
  expect_match(data, "\\.csv$")
  expect_true(any(grepl("\\.sps$", files$filename[files$role == "metadata"])))
})

test_that("list_borealis_pumf_catalogue lists the Census PUMFs", {
  skip_on_cran()
  skip_if_offline("borealisdata.ca")
  cat <- tryCatch(list_borealis_pumf_catalogue(verbose = FALSE, cache_path = NULL),
                  canpumf_network_error = function(e) skip(conditionMessage(e)))
  expect_true(all(c("title", "year", "language", "doi", "url") %in% names(cat)))
  expect_gt(nrow(cat), 1000L)
  dois <- vapply(canpumf:::.pumf_registry[.borealis_keys()],
                 canpumf:::.borealis_entry_doi, character(1L))
  expect_true(all(dois %in% cat$doi))
})
