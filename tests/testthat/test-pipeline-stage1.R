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
    # If list_canpumf_collection() fails (StatCan unreachable), skip gracefully
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

# ---- pumf_locate_or_download: already extracted -----------------------------

test_that("pumf_locate_or_download: skips download+extract when already done", {
  tmp  <- withr::local_tempdir()
  vdir <- make_fake_version_dir(tmp)

  # Should not attempt any network access (no mocking needed — zip exists and
  # is extracted, so list_canpumf_collection() is never called)
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
