# Offline tests for the persistent (cross-session) catalogue cache layered on
# list_statcan_pumf_catalogue(): the cache-file path resolver, the
# write/read round-trip, the staleness warning, and the offline-fallback path
# when a live crawl fails. No network: the crawl is mocked.

test_that(".statcan_catalogue_cache_file resolves only when a cache_path is set", {
  expect_null(canpumf:::.statcan_catalogue_cache_file(NULL))
  expect_null(canpumf:::.statcan_catalogue_cache_file(""))
  expect_equal(
    canpumf:::.statcan_catalogue_cache_file("/tmp/somewhere"),
    file.path("/tmp/somewhere", "pumf_catalogue.rds"))
})

test_that("persistent write/read round-trips the catalogue and metadata", {
  dir  <- withr::local_tempdir()
  file <- file.path(dir, "pumf_catalogue.rds")
  data <- tibble::tibble(Acronym = "GSS", Version = "Cycle 21 (2007)",
                         url = "https://x/c21_2007.zip")

  canpumf:::.statcan_write_persistent(file, data, prefer = c("CSV", "SAS"))
  expect_true(file.exists(file))

  got <- canpumf:::.statcan_read_persistent(file)
  expect_identical(got$data, data)
  expect_identical(got$prefer, c("CSV", "SAS"))
  expect_s3_class(got$fetched, "POSIXct")
})

test_that("persistent read returns NULL for missing / malformed files", {
  expect_null(canpumf:::.statcan_read_persistent(NULL))
  expect_null(canpumf:::.statcan_read_persistent(
    file.path(withr::local_tempdir(), "nope.rds")))

  bad <- file.path(withr::local_tempdir(), "bad.rds")
  saveRDS(list(foo = 1), bad)               # wrong shape -> rejected
  expect_null(canpumf:::.statcan_read_persistent(bad))
})

test_that("write is a no-op when there is no durable cache file", {
  expect_silent(canpumf:::.statcan_write_persistent(NULL, data.frame(), "CSV"))
})

test_that(".statcan_warn_if_stale warns past the threshold, silent when fresh", {
  withr::local_options(canpumf.catalogue_max_age_days = 30)

  expect_silent(canpumf:::.statcan_warn_if_stale(Sys.time() - as.difftime(1, units = "days")))
  expect_warning(
    canpumf:::.statcan_warn_if_stale(Sys.time() - as.difftime(45, units = "days")),
    "out of date")
})

test_that("a full crawl is persisted and reused across a cleared session cache", {
  dir <- withr::local_tempdir()
  withr::local_options(canpumf.cache_path = dir)

  fake <- tibble::tibble(catalogue_id = "45250001", Acronym = "GSS",
                         Title = "General Social Survey",
                         survey_url = "u", edition = "2007", format = "CSV",
                         url = "https://x/c21_2007.zip", product_url = "p")
  calls <- 0L

  testthat::with_mocked_bindings(
    .statcan_crawl_catalogue = function(prefer, max_surveys, surveys, verbose) {
      calls <<- calls + 1L
      fake
    },
    {
      # clear any session cache so the disk path is exercised
      rm(list = ls(canpumf:::.statcan_catalogue_cache),
         envir = canpumf:::.statcan_catalogue_cache)

      one <- list_statcan_pumf_catalogue(verbose = FALSE)
      expect_identical(one, fake)
      expect_equal(calls, 1L)
      expect_true(file.exists(file.path(dir, "pumf_catalogue.rds")))

      # clear the session cache again; a fresh call must read from disk, not crawl
      rm(list = ls(canpumf:::.statcan_catalogue_cache),
         envir = canpumf:::.statcan_catalogue_cache)
      two <- list_statcan_pumf_catalogue(verbose = FALSE)
      expect_identical(two, fake)
      expect_equal(calls, 1L)           # crawl not called a second time
    })
})

test_that("an unreachable crawl falls back to the last persisted catalogue", {
  dir <- withr::local_tempdir()
  withr::local_options(canpumf.cache_path = dir)

  fake <- tibble::tibble(catalogue_id = "45250001", Acronym = "GSS",
                         Title = "General Social Survey",
                         survey_url = "u", edition = "2007", format = "CSV",
                         url = "https://x/c21_2007.zip", product_url = "p")
  # seed a good persisted copy
  canpumf:::.statcan_write_persistent(
    file.path(dir, "pumf_catalogue.rds"), fake,
    prefer = names(canpumf:::.statcan_format_tokens))

  testthat::with_mocked_bindings(
    .statcan_crawl_catalogue = function(prefer, max_surveys, surveys, verbose)
      stop("StatCan unreachable"),
    {
      rm(list = ls(canpumf:::.statcan_catalogue_cache),
         envir = canpumf:::.statcan_catalogue_cache)
      expect_warning(
        out <- list_statcan_pumf_catalogue(refresh = TRUE, verbose = FALSE),
        "unreachable")
      expect_identical(out, fake)
    })
})

test_that("an unreachable crawl with no cached copy re-raises the error", {
  dir <- withr::local_tempdir()                 # empty: no persisted catalogue
  withr::local_options(canpumf.cache_path = dir)

  testthat::with_mocked_bindings(
    .statcan_crawl_catalogue = function(prefer, max_surveys, surveys, verbose)
      stop("StatCan unreachable"),
    # with no persisted *and* no shipped snapshot, the error must surface
    .statcan_shipped_snapshot = function() NULL,
    {
      rm(list = ls(canpumf:::.statcan_catalogue_cache),
         envir = canpumf:::.statcan_catalogue_cache)
      expect_error(
        list_statcan_pumf_catalogue(refresh = TRUE, verbose = FALSE),
        "unreachable")
    })
})

test_that("an unreachable crawl falls back to the shipped snapshot when no user cache", {
  dir <- withr::local_tempdir()                 # empty: no persisted catalogue
  withr::local_options(canpumf.cache_path = dir)

  shipped <- tibble::tibble(catalogue_id = "62m0004x", Acronym = "SHS",
                            SeriesTitle = "Survey of Household Spending",
                            Title = "Survey of Household Spending — 2017",
                            survey_url = "u", edition = "2017", format = "CSV",
                            url = "https://x/SHS_2017.zip", product_url = "p")

  testthat::with_mocked_bindings(
    .statcan_crawl_catalogue = function(prefer, max_surveys, surveys, verbose)
      stop("StatCan unreachable"),
    .statcan_shipped_snapshot = function()
      list(fetched = Sys.time(), prefer = names(canpumf:::.statcan_format_tokens),
           data = shipped),
    {
      rm(list = ls(canpumf:::.statcan_catalogue_cache),
         envir = canpumf:::.statcan_catalogue_cache)
      expect_warning(
        out <- list_statcan_pumf_catalogue(refresh = TRUE, verbose = FALSE),
        "unreachable")
      expect_identical(out, shipped)
    })
})

# ---- non-crawling cached accessor + download-URL resolver -------------------

test_that(".statcan_catalogue_cached reads cache/snapshot without crawling", {
  dir <- withr::local_tempdir()
  withr::local_options(canpumf.cache_path = dir)
  rm(list = ls(canpumf:::.statcan_catalogue_cache),
     envir = canpumf:::.statcan_catalogue_cache)

  fake <- tibble::tibble(catalogue_id = "13m0006x", Acronym = "SFS",
                         SeriesTitle = "Survey of Financial Security",
                         Title = "Survey of Financial Security — 2019",
                         survey_url = "u", edition = "2019", format = "CSV",
                         url = "https://x/SFS2019-eng.zip", product_url = "p")

  # A live crawl must never be triggered by the cached accessor.
  testthat::with_mocked_bindings(
    .statcan_crawl_catalogue = function(...) stop("must not crawl"),
    {
      # (1) nothing on disk, no shipped copy -> NULL
      expect_null(testthat::with_mocked_bindings(
        .statcan_shipped_snapshot = function() NULL,
        canpumf:::.statcan_catalogue_cached(dir)))

      # (2) shipped snapshot used when no user cache
      got <- testthat::with_mocked_bindings(
        .statcan_shipped_snapshot = function()
          list(fetched = Sys.time(), prefer = "CSV", data = fake),
        canpumf:::.statcan_catalogue_cached(dir))
      expect_identical(got, fake)

      # (3) user persisted cache wins over the shipped snapshot
      canpumf:::.statcan_write_persistent(
        file.path(dir, "pumf_catalogue.rds"), fake,
        prefer = names(canpumf:::.statcan_format_tokens))
      rm(list = ls(canpumf:::.statcan_catalogue_cache),
         envir = canpumf:::.statcan_catalogue_cache)
      got2 <- testthat::with_mocked_bindings(
        .statcan_shipped_snapshot = function() stop("should not be reached"),
        canpumf:::.statcan_catalogue_cached(dir))
      expect_identical(got2, fake)
    })
})

test_that(".pumf_resolve_collection_row: adapter hit for a supported series", {
  dir <- withr::local_tempdir()
  rm(list = ls(canpumf:::.statcan_catalogue_cache),
     envir = canpumf:::.statcan_catalogue_cache)

  fake <- tibble::tibble(catalogue_id = "13m0006x", Acronym = "SFS",
                         SeriesTitle = "Survey of Financial Security",
                         Title = "Survey of Financial Security — 2019",
                         survey_url = "u", edition = "2019", format = "CSV",
                         url = "https://x/SFS2019-eng.zip", product_url = "p")

  testthat::with_mocked_bindings(
    .statcan_catalogue_cached = function(cache_path = NULL) fake,
    # curated fallback must NOT be consulted on an adapter hit
    list_canpumf_collection = function(...) stop("curated path must not run"),
    {
      row <- canpumf:::.pumf_resolve_collection_row("SFS", "2019", dir)
      expect_equal(nrow(row), 1L)
      expect_equal(row$url, "https://x/SFS2019-eng.zip")
    })
})

test_that(".pumf_resolve_collection_row: falls back to curated for SGVP and on miss", {
  dir <- withr::local_tempdir()
  rm(list = ls(canpumf:::.statcan_catalogue_cache),
     envir = canpumf:::.statcan_catalogue_cache)

  sfs <- tibble::tibble(catalogue_id = "13m0006x", Acronym = "SFS",
                        SeriesTitle = "Survey of Financial Security",
                        Title = "Survey of Financial Security — 2019",
                        survey_url = "u", edition = "2019", format = "CSV",
                        url = "https://x/SFS2019-eng.zip", product_url = "p")
  curated <- tibble::tibble(
    Acronym = c("SGVP", "SFS"),
    Version = c("2013", "2099"),
    url     = c("https://curated/GVP_PUMF_MAIN.zip", "https://curated/SFS2099.zip"))

  testthat::with_mocked_bindings(
    .statcan_catalogue_cached = function(cache_path = NULL) sfs,
    list_canpumf_collection = function(...) curated,
    {
      # SGVP is not a scraper-supported series -> curated path
      sgvp <- canpumf:::.pumf_resolve_collection_row("SGVP", "2013", dir)
      expect_equal(sgvp$url, "https://curated/GVP_PUMF_MAIN.zip")

      # supported series but version absent from the adapter -> curated fallback
      miss <- canpumf:::.pumf_resolve_collection_row("SFS", "2099", dir)
      expect_equal(miss$url, "https://curated/SFS2099.zip")
    })
})
