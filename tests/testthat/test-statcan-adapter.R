# Offline tests for the catalogue adapter that maps the StatCan crawl frame
# (list_statcan_pumf_catalogue()) onto the curated-collection contract.
# These use synthetic input -- no network.

# A small synthetic crawl frame covering the cases the adapter must handle:
# GSS cycle/year collisions, the legacy Giving/Volunteering prefixes, a phantom
# editionless GSS row, plain year-keyed surveys, a format collision, and an
# unsupported series that must be filtered out.
.fake_cat <- function() {
  tibble::tibble(
    catalogue_id = c("45250001", "45250001", "45250001", "45250001",
                     "45250001", "45250001", "12M0016X",
                     "62m0004x", "13m0006x", "CCHS",
                     "45250002", "45250009", "45250012",
                     "13250007", "13250009", "13250011"),
    Acronym      = c("GSS", "GSS", "GSS", "GSS", "GSS", "GSS", "GSS",
                     "SHS", "SFS", "CCHS",
                     "CPSS", "CPSS", "CPSS",
                     "CCAHS", "CCAHS", "CCAHS"),
    Title        = c(
                     # umbrella GSS rows arrive already edition-specific from the
                     # crawl (series name + structural descriptor)
                     "General Social Survey — Cycle 21 (2007)",
                     "General Social Survey — Cycle 21 (2007)",
                     "General Social Survey — Cycle 14 (2000)",
                     "General Social Survey — NSGVP (2000)",
                     "General Social Survey — Cycle 32 (2018)",
                     "General Social Survey — Cycle 33 (2018)",
                     "General Social Survey",
                     "Survey of Household Spending",
                     "Survey of Financial Security",
                     "Canadian Community Health Survey",
                     "Canadian Perspectives Survey Series 1: Impacts of COVID-19",
                     "Canadian Perspectives Survey Series 4: Information Sources",
                     "Canadian Perspectives Survey Series 6: Substance Use",
                     "Canadian COVID-19 Antibody and Health Survey, Cycle 1: PUMF",
                     "Canadian COVID-19 Antibody and Health Survey Cycle 2: PUMF",
                     "Canadian COVID-19 Antibody and Health Survey Cycle 2: PUMF"),
    SeriesTitle  = c(rep("General Social Survey", 7),
                     "Survey of Household Spending",
                     "Survey of Financial Security",
                     "Canadian Community Health Survey",
                     rep("Canadian Perspectives Survey Series", 3),
                     rep("Canadian COVID-19 Antibody and Health Survey", 3)),
    survey_url   = "https://www150.statcan.gc.ca/n1/en/catalogue/x",
    edition      = c("2007", "2007", "2000", "2000", "2018", "2018", NA,
                     "2017", "1999", "2017",
                     "2020", "2020", "2021",
                     "2022", "2022", "2022"),
    format       = c("CSV", "SAS", NA, NA, NA, NA, NA, "CSV", "CSV", "CSV",
                     "CSV", "CSV", "CSV", "CSV", "CSV", "CSV"),
    url = c(
      "https://x/n1/pub/45-25-0001/cat9/c21_2007.zip",       # Cycle 21 (2007)
      "https://x/n1/pub/45-25-0001/cat9/c21_2007_SAS.zip",   # same cycle, SAS
      "https://x/n1/pub/45-25-0001/cat8/c14_2000.zip",       # Cycle 14 (2000)
      "https://x/n1/pub/45-25-0001/cat5/NSGVP-ENDBP_2000.zip", # NSGVP (2000)
      "https://x/n1/pub/45-25-0001/cat1/c32_2018.zip",       # Cycle 32 (2018)
      "https://x/n1/pub/45-25-0001/cat2/c33_2018.zip",       # Cycle 33 (2018)
      "(EFT)",                                               # phantom, no edition
      "https://x/n1/pub/62m0004x/2017001/SHS_2017.zip",
      "https://x/n1/pub/13m0006x/2021001/SFS1999-eng.zip",
      "https://x/n1/pub/cchs/cchs_2017.zip",                 # unsupported series
      "https://x/n1/pub/45-25-0002/2020001/CSV.zip",         # CPSS 1 (2020)
      "https://x/n1/pub/45-25-0009/2020001/CSV.zip",         # CPSS 4 (2020 collide)
      "https://x/n1/pub/45-25-0012/2021001/CSV.zip",         # CPSS 6 (2021)
      "https://x/n1/pub/13-25-0007/2022001/CCAHS.zip",       # CCAHS 1
      "https://x/n1/pub/13-25-0009/2022001/CCAHS.zip",       # CCAHS 2 (a)
      "https://x/n1/pub/13-25-0011/2022001/CCAHS.zip"),      # CCAHS 2 (b, collide)
    product_url  = "https://x/prod-eng.htm")
}

test_that(".statcan_gss_version derives cycle/GVP keys from the filename", {
  expect_equal(canpumf:::.statcan_gss_version("c21_2007.zip", "2007"),
               "Cycle 21 (2007)")
  expect_equal(canpumf:::.statcan_gss_version("c16_2002.zip", "2002"),
               "Cycle 16 (2002)")
  expect_equal(canpumf:::.statcan_gss_version("CSGVP-ECDBP_2007.zip", "2007"),
               "CSGVP (2007)")
  expect_equal(canpumf:::.statcan_gss_version("NSGVP-ENDBP_2000.zip", "2000"),
               "NSGVP (2000)")
  expect_equal(canpumf:::.statcan_gss_version("https://x/y/c33_2018.zip", "2018"),
               "Cycle 33 (2018)")
  # no cycle/GVP signal -> falls back to the bare edition
  expect_equal(canpumf:::.statcan_gss_version("opaque.zip", "2011"), "2011")
})

test_that(".statcan_series_title strips edition tail and boilerplate", {
  st <- canpumf:::.statcan_series_title
  # umbrella: only "– Public Use Microdata Files" boilerplate to strip
  expect_equal(st("General Social Survey – Public Use Microdata Files"),
               "General Social Survey")
  # per-edition CPSS: keep the "Series" word, drop the cycle number + descriptor
  expect_equal(st("Canadian Perspectives Survey Series 6: Substance Use"),
               "Canadian Perspectives Survey Series")
  # per-edition CCAHS: a trailing "Cycle" marker is dropped
  expect_equal(st("Canadian COVID-19 Antibody and Health Survey, Cycle 1: PUMF"),
               "Canadian COVID-19 Antibody and Health Survey")
  # a title with no edition tail is unchanged
  expect_equal(st("Survey of Financial Security"), "Survey of Financial Security")
  expect_true(is.na(st(NA_character_)))
})

test_that("adapter returns the curated contract plus provenance columns", {
  out <- canpumf:::.statcan_catalogue_to_collection(.fake_cat())
  expect_s3_class(out, "tbl_df")
  expect_identical(
    names(out),
    c("Acronym", "Version", "SeriesTitle", "Title", "url",
      "catalogue_id", "survey_url", "product_url"))
})

test_that("adapter carries SeriesTitle and edition-specific Title through", {
  out <- canpumf:::.statcan_catalogue_to_collection(.fake_cat())
  # SeriesTitle is the plain series name; Title is edition-specific
  expect_equal(
    out$SeriesTitle[out$Acronym == "GSS" & out$Version == "Cycle 21 (2007)"],
    "General Social Survey")
  expect_equal(
    out$Title[out$Acronym == "GSS" & out$Version == "Cycle 21 (2007)"],
    "General Social Survey — Cycle 21 (2007)")
  # CPSS keeps StatCan's own edition-specific title; series name is stripped
  expect_equal(out$SeriesTitle[out$Acronym == "CPSS" & out$Version == "6"],
               "Canadian Perspectives Survey Series")
  expect_match(out$Title[out$Acronym == "CPSS" & out$Version == "6"],
               "Series 6")
})

test_that("adapter derives SeriesTitle when absent (legacy crawl frame)", {
  cat <- .fake_cat()
  cat$SeriesTitle <- NULL                       # simulate a pre-SeriesTitle snapshot
  out <- canpumf:::.statcan_catalogue_to_collection(cat)
  expect_true("SeriesTitle" %in% names(out))
  expect_equal(out$SeriesTitle[out$Acronym == "CPSS" & out$Version == "6"],
               "Canadian Perspectives Survey Series")
})

test_that("adapter filters to supported series", {
  out <- canpumf:::.statcan_catalogue_to_collection(.fake_cat())
  expect_false("CCHS" %in% out$Acronym)
  expect_setequal(unique(out$Acronym),
                  c("GSS", "SHS", "SFS", "CPSS", "CCAHS"))
})

test_that("adapter disambiguates colliding GSS years and drops phantom rows", {
  out <- canpumf:::.statcan_catalogue_to_collection(.fake_cat())
  gss <- out[out$Acronym == "GSS", ]
  expect_setequal(
    gss$Version,
    c("Cycle 21 (2007)", "Cycle 14 (2000)", "NSGVP (2000)",
      "Cycle 32 (2018)", "Cycle 33 (2018)"))
  # the editionless (EFT) GSS row produced no version key and is gone
  expect_false(any(is.na(gss$Version)))
  expect_false("(EFT)" %in% gss$url)
})

test_that("adapter yields one highest-preference URL per (Acronym, Version)", {
  out <- canpumf:::.statcan_catalogue_to_collection(.fake_cat())
  expect_false(any(duplicated(out[, c("Acronym", "Version")])))
  # CSV beats SAS for Cycle 21 (2007)
  u <- out$url[out$Acronym == "GSS" & out$Version == "Cycle 21 (2007)"]
  expect_match(u, "c21_2007\\.zip$")
})

test_that("adapter keeps plain-year keys for non-GSS supported series", {
  out <- canpumf:::.statcan_catalogue_to_collection(.fake_cat())
  expect_equal(out$Version[out$Acronym == "SHS"], "2017")
  expect_equal(out$Version[out$Acronym == "SFS"], "1999")
})

test_that(".statcan_cycle_version reads the cycle number from the title", {
  cv <- canpumf:::.statcan_cycle_version
  expect_equal(cv("Canadian Perspectives Survey Series 6: X", "2021"), "6")
  expect_equal(cv("... Antibody and Health Survey, Cycle 1: Y", "2022"), "1")
  expect_equal(cv("... Antibody and Health Survey Cycle 2: Y", "2022"), "2")
  # no cycle/series signal -> falls back to the bare edition
  expect_equal(cv("Some untitled survey", "2020"), "2020")
})

test_that("adapter keys CPSS/CCAHS by cycle number, not the colliding year", {
  out <- canpumf:::.statcan_catalogue_to_collection(.fake_cat())

  cpss <- out[out$Acronym == "CPSS", ]
  # cycles 1 and 4 both edition '2020' but stay distinct via the title number
  expect_setequal(cpss$Version, c("1", "4", "6"))
  expect_match(cpss$url[cpss$Version == "1"], "45-25-0002")
  expect_match(cpss$url[cpss$Version == "4"], "45-25-0009")
  expect_match(cpss$url[cpss$Version == "6"], "45-25-0012")

  ccahs <- out[out$Acronym == "CCAHS", ]
  # the two Cycle-2 catalogue entries collapse to one Version per the dedup
  expect_setequal(ccahs$Version, c("1", "2"))
  expect_match(ccahs$url[ccahs$Version == "1"], "13-25-0007")
  expect_false(any(duplicated(ccahs$Version)))
})

test_that("adapter handles empty / all-unsupported input", {
  empty <- canpumf:::.statcan_catalogue_to_collection(NULL)
  expect_s3_class(empty, "tbl_df")
  expect_equal(nrow(empty), 0L)

  only_unsupported <- .fake_cat()[.fake_cat()$Acronym == "CCHS", ]
  expect_equal(nrow(canpumf:::.statcan_catalogue_to_collection(only_unsupported)),
               0L)
})
