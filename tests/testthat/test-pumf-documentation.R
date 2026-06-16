# Tests for open_pumf_documentation() and its helpers.

.doc_cache <- function() getOption("canpumf.cache_path", "")

.doc_vdir <- function(series, version = NULL) {
  if (is.null(version)) file.path(.doc_cache(), series)
  else                  file.path(.doc_cache(), series, version)
}


# ---- Input validation --------------------------------------------------------

test_that("open_pumf_documentation: returns NULL with message when dir missing", {
  dir <- file.path(tempdir(), "no_such_series", "9999")
  expect_message(
    result <- open_pumf_documentation(
      series     = "FAKE",
      version    = "9999",
      cache_path = tempdir()
    ),
    regexp = "No data found"
  )
  expect_null(result)
})

test_that("open_pumf_documentation: errors on invalid lang", {
  expect_error(
    open_pumf_documentation("SFS", "2019", lang = "deu"),
    regexp = "lang"
  )
})

test_that("open_pumf_documentation: errors when series is NULL", {
  expect_error(open_pumf_documentation(), regexp = "series.*must be specified")
})

test_that("open_pumf_documentation: deprecated params warn", {
  expect_warning(
    tryCatch(
      open_pumf_documentation(pumf_series = "SFS", pumf_version = "2019",
                               cache_path = tempdir()),
      error = function(e) NULL,
      message = function(m) NULL
    ),
    regexp = "deprecated"
  )
})


# ---- LFS with no version: most recently cached ------------------------------

test_that(".pumf_lfs_latest_cached: returns NULL when LFS dir absent", {
  expect_null(canpumf:::.pumf_lfs_latest_cached(tempdir()))
})

test_that(".pumf_lfs_latest_cached: picks most recent version", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  # .pumf_lfs_latest_cached looks for <cache_path>/LFS/<version>/
  lfs_dir <- file.path(tmp, "LFS")
  dir.create(file.path(lfs_dir, "2021"),    recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(lfs_dir, "2023"),    recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(lfs_dir, "2022-06"), recursive = TRUE, showWarnings = FALSE)
  # Create a dummy file so each version dir looks "extracted"
  writeLines("x", file.path(lfs_dir, "2023",    "dummy.txt"))
  writeLines("x", file.path(lfs_dir, "2022-06", "dummy.txt"))
  writeLines("x", file.path(lfs_dir, "2021",    "dummy.txt"))
  # Should prefer annual 2023 > monthly 2022-06 > annual 2021
  expect_equal(canpumf:::.pumf_lfs_latest_cached(tmp), "2023")
})

test_that("open_pumf_documentation: LFS no version emits message when no LFS cache", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  expect_message(
    result <- open_pumf_documentation("LFS", cache_path = tmp),
    regexp = "No LFS data"
  )
  expect_null(result)
})


# ---- Language scoring -------------------------------------------------------

test_that(".pumf_lang_score: scores English files correctly", {
  paths <- c("guide_ENG.pdf", "guide_FRA.pdf", "userguide.pdf")
  scores <- canpumf:::.pumf_lang_score(paths, "eng")
  expect_equal(scores, c(2L, 0L, 1L))
})

test_that(".pumf_lang_score: scores French files correctly", {
  paths <- c("guide_eng.pdf", "guide_fra.pdf", "manual.pdf")
  scores <- canpumf:::.pumf_lang_score(paths, "fra")
  expect_equal(scores, c(0L, 2L, 1L))
})

test_that(".pumf_lang_score: _e. suffix scores as English", {
  scores <- canpumf:::.pumf_lang_score(c("pumf1976rcl_e.pdf", "pumf1976rclv2_f.pdf"), "eng")
  expect_equal(scores, c(2L, 0L))
})

test_that(".pumf_lang_score: _f. suffix scores as French", {
  scores <- canpumf:::.pumf_lang_score(c("pumf1976rcl_e.pdf", "pumf1976rclv2_f.pdf"), "fra")
  expect_equal(scores, c(0L, 2L))
})

test_that(".pumf_lang_score: 'recensement' scores as French", {
  scores <- canpumf:::.pumf_lang_score(
    c("Recensement 1986 FMGD - Familles.pdf", "1986 PUMF - Family.pdf"), "fra")
  expect_equal(scores, c(2L, 1L))
})

test_that(".pumf_sort_by_lang: puts preferred language first", {
  paths <- c("guide_fra.pdf", "guide_eng.pdf", "generic.pdf")
  sorted <- canpumf:::.pumf_sort_by_lang(paths, "eng")
  expect_equal(sorted[[1L]], "guide_eng.pdf")
})


# ---- Parent-dir walk-up (EFT Census pattern) --------------------------------

test_that(".pumf_drop_version_sibling_docs: excludes sibling version dirs", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  # Simulate: parent/shared_docs/guide.pdf  (kept)
  #           parent/sibling/metadata/      (sibling version dir, docs inside excluded)
  #           parent/version/               (current version dir, already excluded by caller)
  shared  <- file.path(tmp, "shared_docs"); dir.create(shared, recursive = TRUE)
  sibling <- file.path(tmp, "sibling");     dir.create(file.path(sibling, "metadata"), recursive = TRUE)
  current <- file.path(tmp, "current");     dir.create(current, showWarnings = FALSE)
  writeLines("x", file.path(shared,  "guide.pdf"))
  writeLines("x", file.path(sibling, "other.pdf"))
  paths <- c(file.path(shared, "guide.pdf"), file.path(sibling, "other.pdf"))
  result <- canpumf:::.pumf_drop_version_sibling_docs(paths, tmp, current)
  expect_equal(basename(result), "guide.pdf")
})

test_that("parent-dir walk-up finds PDFs in shared FMGD subdir", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  # open_pumf_documentation builds: cache_path/Census/1971/individuals_prov
  # Parent (year dir):               cache_path/Census/1971/
  year_dir    <- file.path(tmp, "Census", "1971")
  version_dir <- file.path(year_dir, "individuals_prov")
  shared_dir  <- file.path(year_dir, "1971PUMF_FMGD")
  dir.create(file.path(version_dir, "metadata"), recursive = TRUE)
  dir.create(shared_dir, recursive = TRUE)
  writeLines("x", file.path(shared_dir, "pumf1971rcl_e.pdf"))
  writeLines("x", file.path(shared_dir, "recensement-1971-fmgd-combine-francais.pdf"))
  r_eng <- with_mocked_bindings(
    browseURL = function(url, ...) invisible(url), .package = "utils",
    open_pumf_documentation("Census", "1971/individuals_prov", lang = "eng", cache_path = tmp)
  )
  r_fra <- with_mocked_bindings(
    browseURL = function(url, ...) invisible(url), .package = "utils",
    open_pumf_documentation("Census", "1971/individuals_prov", lang = "fra", cache_path = tmp)
  )
  expect_equal(basename(r_eng), "pumf1971rcl_e.pdf")
  expect_equal(basename(r_fra), "recensement-1971-fmgd-combine-francais.pdf")
})

test_that("doc_mask filters to correct file-type in shared dir", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  # open_pumf_documentation builds: cache_path/Census/1986/families
  # Parent (year dir):               cache_path/Census/1986/
  year_dir   <- file.path(tmp, "Census", "1986")
  shared_dir <- file.path(year_dir, "1986PUMF_FMGD")
  dir.create(file.path(year_dir, "families"), recursive = TRUE)
  dir.create(shared_dir, recursive = TRUE)
  for (f in c("1986 PUMF - Family.pdf", "1986 PUMF - Individuals.pdf",
               "Recensement 1986 FMGD - Familles.pdf",
               "Recensement 1986 FMGD - Particuliers.pdf")) {
    writeLines("x", file.path(shared_dir, f))
  }
  r_eng <- with_mocked_bindings(
    browseURL = function(url, ...) invisible(url), .package = "utils",
    open_pumf_documentation("Census", "1986/families", lang = "eng", cache_path = tmp)
  )
  r_fra <- with_mocked_bindings(
    browseURL = function(url, ...) invisible(url), .package = "utils",
    open_pumf_documentation("Census", "1986/families", lang = "fra", cache_path = tmp)
  )
  expect_equal(basename(r_eng), "1986 PUMF - Family.pdf")
  expect_equal(basename(r_fra), "Recensement 1986 FMGD - Familles.pdf")
})


# ---- Integration: documentation found in cache ------------------------------
# Loops over every registered series/version; skips entries not in cache.
# Checks both languages and (for 1986) that the correct file-type PDF is chosen.

.doc_in_cache <- function(series, version) {
  vdir <- .doc_vdir(series, version)
  dir.exists(vdir) && (
    canpumf:::.version_is_extracted(vdir) ||
    !is.null(canpumf:::.find_version_zip(vdir)) ||
    # EFT vintages: docs live in a shared subdir of the parent (year) directory
    length(list.files(dirname(vdir), pattern = "\\.pdf$",
                      recursive = TRUE, ignore.case = TRUE)) > 0L
  )
}

.open_doc_quietly <- function(series, version, lang) {
  with_mocked_bindings(
    browseURL = function(url, ...) invisible(url),
    .package  = "utils",
    withCallingHandlers(
      open_pumf_documentation(series, version, lang = lang,
                               cache_path = .doc_cache()),
      message = function(m) invokeRestart("muffleMessage")
    )
  )
}

# Generate one test per registered entry per language.
for (.key in canpumf:::pumf_registry_keys()) {
  local({
    key     <- .key
    parts   <- strsplit(key, "/")[[1L]]
    series  <- parts[1L]
    version <- paste(parts[-1L], collapse = "/")

    for (.lang in c("eng", "fra")) {
      local({
        lang <- .lang
        test_that(paste0(key, " [", lang, "]: documentation found"), {
          skip_if(.doc_cache() == "", "Cache not configured")
          skip_if_not(.doc_in_cache(series, version),
                      paste(key, "not in cache"))
          result <- .open_doc_quietly(series, version, lang)
          expect_true(!is.null(result) && length(result) >= 1L,
                      label = paste("docs found for", key, lang))
        })
      })
    }

    # For 1986 Census: verify the doc_mask selects the right file type.
    if (series == "Census" && grepl("^1986/", version)) {
      test_that(paste0(key, ": doc_mask returns correct file-type PDF"), {
        skip_if(.doc_cache() == "", "Cache not configured")
        skip_if_not(.doc_in_cache(series, version),
                    paste(key, "not in cache"))
        file_type <- sub("^1986/", "", version)   # "families", "households", "individuals"
        for (lang in c("eng", "fra")) {
          result <- .open_doc_quietly(series, version, lang)
          expect_false(
            any(grepl(
              switch(file_type,
                families    = "Household|Individu|Particulier",
                households  = "Family|Familles|Individu|Particulier",
                individuals = "Family|Familles|Household|[Mm].nages"),
              basename(result), ignore.case = TRUE)),
            label = paste(key, lang, "should not return wrong file type;",
                          "got:", paste(basename(result), collapse = ", "))
          )
        }
      })
    }
  })
}

# LFS: auto-selects most recently downloaded version.
test_that("LFS: documentation found for cached version", {
  skip_if(.doc_cache() == "", "Cache not configured")
  skip_if_not(dir.exists(file.path(.doc_cache(), "LFS")), "LFS not in cache")
  latest <- canpumf:::.pumf_lfs_latest_cached(.doc_cache())
  skip_if(is.null(latest), "No LFS versions downloaded")
  result <- .open_doc_quietly("LFS", NULL, "eng")
  expect_true(!is.null(result))
})
