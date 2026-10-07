# Tests for pumf_locate_or_download() (Stage 1)
# Network-dependent tests use skip_if_offline() and wrap download calls in
# tryCatch(error = skip(...)) so StatCan downtime produces a skip, not a failure.

# ---- helpers ----------------------------------------------------------------

# Create a minimal fake version directory that looks "already downloaded + extracted"
make_fake_version_dir <- function(base, series = "FAKE", version = "2099") {
  vdir <- file.path(base, series, version)
  dir.create(vdir, recursive = TRUE)
  writeLines("fake data", file.path(vdir, "data.txt"))          # extracted file
  writeLines("fake zip",  file.path(vdir, "fake.zip"))          # zip sentinel
  vdir
}

# ---- .find_version_zip ------------------------------------------------------

test_that(".find_version_zip: returns NULL for missing dir", {
  expect_null(canpumf:::.find_version_zip(tempfile()))
})

test_that(".find_version_zip: returns NULL when no zip present", {
  tmp <- withr::local_tempdir()
  writeLines("x", file.path(tmp, "data.txt"))
  expect_null(canpumf:::.find_version_zip(tmp))
})

test_that(".find_version_zip: returns zip path when zip present", {
  tmp <- withr::local_tempdir()
  zp <- file.path(tmp, "survey.zip")
  writeLines("z", zp)
  result <- canpumf:::.find_version_zip(tmp)
  expect_equal(result, zp)
})

# ---- .version_is_extracted --------------------------------------------------

test_that(".version_is_extracted: FALSE for missing dir", {
  expect_false(canpumf:::.version_is_extracted(tempfile()))
})

test_that(".version_is_extracted: FALSE when only zip present", {
  tmp <- withr::local_tempdir()
  writeLines("z", file.path(tmp, "survey.zip"))
  expect_false(canpumf:::.version_is_extracted(tmp))
})

test_that(".version_is_extracted: FALSE when only zip + metadata present", {
  tmp <- withr::local_tempdir()
  writeLines("z", file.path(tmp, "survey.zip"))
  dir.create(file.path(tmp, "metadata"))
  expect_false(canpumf:::.version_is_extracted(tmp))
})

test_that(".version_is_extracted: FALSE when zip + duckdb only (no raw data)", {
  tmp <- withr::local_tempdir()
  writeLines("z", file.path(tmp, "survey.zip"))
  writeLines("d", file.path(tmp, "survey.duckdb"))
  expect_false(canpumf:::.version_is_extracted(tmp))
})

test_that(".version_is_extracted: TRUE when extracted files present", {
  tmp <- withr::local_tempdir()
  writeLines("z", file.path(tmp, "survey.zip"))
  writeLines("x", file.path(tmp, "data.txt"))
  expect_true(canpumf:::.version_is_extracted(tmp))
})

test_that(".version_is_extracted: TRUE when extracted dir present", {
  tmp <- withr::local_tempdir()
  writeLines("z", file.path(tmp, "survey.zip"))
  dir.create(file.path(tmp, "SPSS"))
  expect_true(canpumf:::.version_is_extracted(tmp))
})

# ---- .zip_filename_from_url -------------------------------------------------

test_that(".zip_filename_from_url: strips query string", {
  url <- "https://example.com/path/survey.zip?st=ABC123"
  expect_equal(canpumf:::.zip_filename_from_url(url), "survey.zip")
})

test_that(".zip_filename_from_url: handles URL without query string", {
  url <- "https://example.com/path/survey.zip"
  expect_equal(canpumf:::.zip_filename_from_url(url), "survey.zip")
})

# ---- pumf_locate_or_download: error cases -----------------------------------

test_that("pumf_locate_or_download: errors for unknown series/version", {
  tmp <- withr::local_tempdir()
  skip_if_offline()
  tryCatch(
    expect_error(
      canpumf:::pumf_locate_or_download("NOSUCHSERIES", "9999", cache_path = tmp),
      regexp = "not found in the canpumf collection"
    ),
    # If list_pumf_catalogue() fails (StatCan unreachable), skip gracefully
    error = function(e) skip(paste("StatCan unreachable:", conditionMessage(e)))
  )
})

test_that("pumf_locate_or_download: errors with EFT message for EFT-only surveys", {
  tmp <- withr::local_tempdir()
  skip_if_offline()
  # Older Census versions are EFT-only (e.g. 1971 individuals)
  tryCatch(
    expect_error(
      canpumf:::pumf_locate_or_download("Census", "1971 (individuals)",
                                         cache_path = tmp),
      regexp = "Electronic File Transfer"
    ),
    error = function(e) skip(paste("StatCan unreachable:", conditionMessage(e)))
  )
})

# ---- pumf_locate_or_download: refresh logic ---------------------------------

test_that("pumf_locate_or_download: refresh deletes .duckdb and metadata/", {
  tmp     <- withr::local_tempdir()
  vdir    <- make_fake_version_dir(tmp)
  db_path <- file.path(vdir, "FAKE_2099.duckdb")
  meta_dir <- file.path(vdir, "metadata")

  writeLines("db",  db_path)
  dir.create(meta_dir)
  writeLines("v",  file.path(meta_dir, "variables.csv"))

  canpumf:::pumf_locate_or_download("FAKE", "2099",
                                     cache_path = tmp, refresh = TRUE)

  expect_false(file.exists(db_path))
  expect_false(dir.exists(meta_dir))
  expect_true(file.exists(file.path(vdir, "data.txt")))  # raw data untouched
  expect_true(file.exists(file.path(vdir, "fake.zip")))  # zip untouched
})

test_that("pumf_locate_or_download: refresh without duckdb/metadata is a no-op", {
  tmp  <- withr::local_tempdir()
  vdir <- make_fake_version_dir(tmp)

  expect_no_error(
    canpumf:::pumf_locate_or_download("FAKE", "2099",
                                       cache_path = tmp, refresh = TRUE)
  )
  expect_true(file.exists(file.path(vdir, "data.txt")))
})

# ---- .pumf_parse_stage2: Stage 2 as the pipeline runs it --------------------

# Records the arguments of every pumf_parse_metadata() call instead of parsing.
local_stage2_recorder <- function(env = parent.frame()) {
  calls <- list()
  rec   <- function(version_dir, layout_mask = NULL, metadata_encoding = NULL,
                    refresh = FALSE, meta_subdir = NULL, file_mask = NULL,
                    layout_file = NULL) {
    calls[[length(calls) + 1L]] <<- list(
      version_dir = version_dir, layout_mask = layout_mask,
      metadata_encoding = metadata_encoding, refresh = refresh,
      meta_subdir = meta_subdir, file_mask = file_mask, layout_file = layout_file)
    invisible(version_dir)
  }
  testthat::local_mocked_bindings(pumf_parse_metadata = rec, .env = env)
  function() calls
}

test_that(".pumf_parse_stage2: a multi-module entry parses every module with its own masks", {
  calls <- local_stage2_recorder()
  reg   <- canpumf:::pumf_registry_lookup("GSS", "Cycle 36 (2022)")
  canpumf:::.pumf_parse_stage2("vdir", reg, refresh = TRUE)
  got <- calls()
  expect_length(got, 2L)
  # Main: primary module, metadata/ itself, no reading-card override.
  expect_equal(got[[1L]]$layout_mask, "_Main_")
  expect_null(got[[1L]]$meta_subdir)
  expect_equal(got[[1L]]$file_mask, "Main-Principal_PUMF\\.txt")
  expect_null(got[[1L]]$layout_file)
  expect_true(got[[1L]]$refresh)
  # Episode: its subdir, its file mask and the SAS card the registry names
  # (issue #29); pumf_metadata() used to parse without any of these.
  expect_equal(got[[2L]]$layout_mask, "_Episode_")
  expect_equal(got[[2L]]$meta_subdir, "Episode")
  expect_equal(got[[2L]]$file_mask,   "Episode_PUMF\\.txt")
  expect_equal(got[[2L]]$layout_file, "^TU_ET_2022_Episode_i\\.SAS$")
  expect_equal(got[[2L]]$metadata_encoding, reg$metadata_encoding)
})

test_that(".pumf_parse_stage2: a single-table entry is one call with the entry's masks", {
  calls <- local_stage2_recorder()
  reg   <- canpumf:::pumf_registry_lookup("SFS", "2019")
  canpumf:::.pumf_parse_stage2("vdir", reg)
  got <- calls()
  expect_length(got, 1L)
  expect_equal(got[[1L]]$layout_mask, reg$layout_mask)
  expect_equal(got[[1L]]$file_mask,   reg$file_mask)
  expect_null(got[[1L]]$meta_subdir)
  expect_false(got[[1L]]$refresh)
  # pumf_run_pipeline() hands the same module list to Stage 3.
  mods <- canpumf:::.pumf_stage_modules(reg)
  expect_length(mods, 1L)
  expect_true(mods[[1L]]$is_primary)
  expect_null(mods[[1L]]$data_fixups)
  expect_null(mods[[1L]]$bsw_override)
})

test_that("pumf_metadata(): runs Stage 2 for every module, returns the primary metadata", {
  tmp   <- withr::local_tempdir()
  vdir  <- file.path(tmp, "GSS", "Cycle 36 (2022)")
  mdir  <- file.path(vdir, "metadata")
  dir.create(mdir, recursive = TRUE)
  readr::write_csv(tibble::tibble(name = "PUMFID", label_en = "Id", label_fr = "Id",
                                  type = "character", decimals = NA_integer_,
                                  missing_low = NA_real_, missing_high = NA_real_),
                   file.path(mdir, "variables.csv"))
  readr::write_csv(tibble::tibble(name = character(), val = character(),
                                  label_en = character(), label_fr = character()),
                   file.path(mdir, "codes.csv"))
  calls <- local_stage2_recorder()
  testthat::local_mocked_bindings(
    pumf_locate_or_download = function(series, version, cache_path, refresh = FALSE,
                                       redownload = FALSE, ...) vdir)
  m <- pumf_metadata("GSS", "Cycle 36 (2022)", cache_path = tmp)
  expect_named(m, c("variables", "codes", "layout"), ignore.order = TRUE)
  expect_equal(m$variables$name, "PUMFID")
  got <- calls()
  expect_length(got, 2L)
  expect_true(all(vapply(got, function(x) identical(x$version_dir, vdir), logical(1L))))
  expect_equal(got[[2L]]$meta_subdir, "Episode")
  expect_equal(got[[2L]]$layout_file, "^TU_ET_2022_Episode_i\\.SAS$")
  expect_false(any(vapply(got, `[[`, logical(1L), "refresh")))
})

# ---- pumf_locate_or_download: already extracted -----------------------------

test_that("pumf_locate_or_download: skips download+extract when already done", {
  tmp  <- withr::local_tempdir()
  vdir <- make_fake_version_dir(tmp)

  # Should not attempt any network access (no mocking needed — zip exists and
  # is extracted, so list_pumf_catalogue() is never called)
  result <- canpumf:::pumf_locate_or_download("FAKE", "2099", cache_path = tmp)

  expect_equal(result, vdir)
  expect_true(file.exists(file.path(vdir, "data.txt")))
})

test_that("pumf_locate_or_download: returns version_dir invisibly", {
  tmp  <- withr::local_tempdir()
  vdir <- make_fake_version_dir(tmp)

  result <- withVisible(
    canpumf:::pumf_locate_or_download("FAKE", "2099", cache_path = tmp)
  )
  expect_false(result$visible)
  expect_equal(result$value, vdir)
})

# ---- .extract_inner_zips ----------------------------------------------------

test_that(".extract_inner_zips: excludes top-level zips regardless of separator", {
  tmp  <- withr::local_tempdir()
  vdir <- make_fake_version_dir(tmp)   # writes a placeholder (invalid) fake.zip

  # A trailing separator on `dir` reproduces, on any platform, the mixed-
  # separator situation seen on Windows (backslash dir vs forward-slash
  # list.files output): the top-level fake.zip must still be excluded, so no
  # extraction of the invalid placeholder is attempted.
  expect_no_warning(canpumf:::.extract_inner_zips(paste0(vdir, "/")))
})

test_that(".extract_inner_zips: unpacks zips nested inside zips at any depth", {
  # CIUS 2018/2020 ship Data.zip, which holds RAW.zip, which holds the data
  # file.  One pass only exposes RAW.zip; the loop must go on until nothing
  # new turns up.
  tmp  <- withr::local_tempdir()
  vdir <- file.path(tmp, "FAKE", "2099")
  dir.create(vdir, recursive = TRUE)
  work <- withr::local_tempdir()
  writeLines("survey data", file.path(work, "PUMF.txt"))
  withr::with_dir(work, {
    utils::zip("RAW.zip", files = "PUMF.txt")
    utils::zip("Data.zip", files = "RAW.zip")
  })
  bundle <- file.path(vdir, "bundle")
  dir.create(bundle)
  file.copy(file.path(work, "Data.zip"), bundle)

  expect_message(canpumf:::.extract_inner_zips(vdir), "Extracting inner zip RAW.zip")
  expect_true(file.exists(file.path(bundle, "PUMF.txt")))
  # A second call finds everything in place and extracts nothing.
  expect_no_message(canpumf:::.extract_inner_zips(vdir))
})

# ---- pumf_locate_or_download: extraction from zip ---------------------------

test_that("pumf_locate_or_download: extracts zip when only zip is present", {
  tmp  <- withr::local_tempdir()
  vdir <- file.path(tmp, "FAKE", "2099")
  dir.create(vdir, recursive = TRUE)

  # Create a real zip with one file inside
  inner_file <- tempfile(fileext = ".txt")
  writeLines("survey data", inner_file)
  zip_path <- file.path(vdir, "survey.zip")
  utils::zip(zip_path, files = inner_file, flags = "-j")  # -j: junk paths

  # Should extract without downloading
  canpumf:::pumf_locate_or_download("FAKE", "2099", cache_path = tmp)

  # Something other than the zip should now exist
  expect_true(canpumf:::.version_is_extracted(vdir))
})

# ---- integration test (requires network) ------------------------------------

test_that("pumf_locate_or_download: downloads and extracts CPSS v1", {
  skip_if_offline()
  skip_on_cran()

  tmp    <- withr::local_tempdir()
  result <- tryCatch(
    canpumf:::pumf_locate_or_download("CPSS", "1", cache_path = tmp),
    error = function(e) skip(paste("Download failed:", conditionMessage(e)))
  )

  expect_true(dir.exists(result))
  expect_true(canpumf:::.version_is_extracted(result))
  # zip is retained alongside extracted content
  expect_false(is.null(canpumf:::.find_version_zip(result)))
})
