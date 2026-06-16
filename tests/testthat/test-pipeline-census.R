# Integration tests for the Census of Population pipeline.
# All releases from 1991 onward are available via direct download.
# Tests run against data already in the user's canpumf cache.

.census_cache <- function() getOption("canpumf.cache_path", "")

.census_vdir <- function(version) {
  file.path(.census_cache(), "Census", version)
}

.census_extracted <- function(version) {
  vdir <- .census_vdir(version)
  if (canpumf:::.version_is_extracted(vdir)) return(TRUE)
  # bundled-archive versions (e.g. "1986/individuals"): raw data lives in the
  # parent year directory, mirroring the logic in pumf_build_duckdb().
  grepl("/", version, fixed = TRUE) &&
    canpumf:::.version_is_extracted(dirname(vdir))
}

.census_metadata_exists <- function(version) {
  file.exists(file.path(.census_vdir(version), "metadata", "variables.csv"))
}

.census_duckdb_exists <- function(version) {
  db_file <- paste0("Census_", gsub("[^A-Za-z0-9._-]", "_", version), ".duckdb")
  file.exists(file.path(.census_vdir(version), db_file))
}

# All end-to-end verified versions (import without errors or unexpected warnings)
.census_verified <- c(
  "2021 (individuals)", "2021 (hierarchical)",
  "2016 (individuals)", "2016 (hierarchical)",
  "2011 (individuals)", "2011 (hierarchical)",
  "2006 (individuals)", "2006 (hierarchical)",
  "2001 (individuals)", "2001 (households)", "2001 (families)",
  "1996 (individuals)", "1996 (households)", "1996 (families)",
  "1991 (individuals)", "1991 (households)", "1991 (families)",
  # Bundled-archive years (single zip per year, all types share source dir)
  "1986/individuals", "1986/households", "1986/families",
  "1981/individuals", "1981/households",
  "1976/individuals", "1976/households", "1976/families",
  "1971/individuals_prov", "1971/individuals_cma",
  "1971/households_prov", "1971/households_cma",
  "1971/families_prov",   "1971/families_cma"
)

# Census versions where codes_supplement injects manually: map version ->
# expected warning regex.  Any warning NOT matching this pattern is unexpected.
.census_supplement_warnings <- list(
  "2006 (hierarchical)"  = "absent from command files",
  "2001 (families)"      = "absent from command files",
  "1991 (individuals)"   = "absent from command files",
  # 2021 individuals: StatCan's UTF-8 command file omits French variable labels
  # for 74 of 144 variables — a known upstream gap, not a parser bug.
  "2021 (individuals)"   = "no French translation",
  # 1981 individuals: three pairs of DATA LIST names transposed (WKACTMA/WKACTFA,
  # FAOCC81/MAOCC81, FALFACT/MALFACT) and 1 blank PROV record (data artifact).
  "1981/individuals"     = "Variable PROV: \\d+ unmatched|names swapped",
  # 1971: codes_supplement injects missing value-0 labels for TYPE66/TYPE71 (CMA
  # individuals) and CMACODE (provincial families).
  "1971/individuals_cma" = "absent from command files",
  "1971/families_prov"   = "absent from command files"
)

.census_any_version <- function() {
  for (v in .census_verified) {
    if (.census_extracted(v)) return(v)
  }
  NULL
}


# ---- Stage 2: metadata parsing ----------------------------------------------

test_that("Census: pumf_parse_metadata produces canonical CSVs", {
  v <- .census_any_version()
  skip_if(is.null(v), "No Census version in cache")

  reg <- canpumf:::pumf_registry_lookup("Census", v)
  canpumf:::pumf_parse_metadata(.census_vdir(v),
                                 metadata_encoding = reg$metadata_encoding)

  meta_dir <- file.path(.census_vdir(v), "metadata")
  expect_true(file.exists(file.path(meta_dir, "variables.csv")))
  expect_true(file.exists(file.path(meta_dir, "codes.csv")))

  meta <- canpumf:::read_metadata(meta_dir)
  expect_gt(nrow(meta$variables), 50L)
  expect_gt(nrow(meta$codes),    100L)
  expect_true("PR" %in% meta$variables$name,
              label = "PR variable expected in Census metadata")
  if (grepl("2021", v)) {
    expect_true(any(!is.na(meta$variables$label_fr)),
                label = "Census 2021 should have French labels")
  }
})

test_that("Census 2021: metadata uses UTF-8 encoding correctly", {
  skip_if_not(.census_extracted("2021 (individuals)"),
              "Census 2021 (individuals) not in cache")

  canpumf:::pumf_parse_metadata(.census_vdir("2021 (individuals)"),
                                 metadata_encoding = "UTF-8")

  meta <- canpumf:::read_metadata(
    file.path(.census_vdir("2021 (individuals)"), "metadata"))

  fr_labels <- stats::na.omit(meta$variables$label_fr)
  if (length(fr_labels) > 0L) {
    expect_true(any(grepl("[éèêëàâîïôùûüç]", fr_labels)),
      label = "French labels should contain accented characters")
    expect_false(any(grepl("Ã", fr_labels)),
      label = "French labels must not contain mojibake 'Ã'")
  }
})

test_that("Census 2011 (individuals): variable labels present from SAS parser", {
  skip_if_not(.census_extracted("2011 (individuals)"),
              "Census 2011 (individuals) not in cache")

  canpumf:::pumf_parse_metadata(.census_vdir("2011 (individuals)"))

  meta   <- canpumf:::read_metadata(
    file.path(.census_vdir("2011 (individuals)"), "metadata"))
  labeled <- sum(!is.na(meta$variables$label_en) &
                   nchar(meta$variables$label_en) > 0L)
  expect_gt(labeled, 50L,
    label = "2011 individuals should have variable labels from the SAS DATA step parser")
})

test_that("Census 2006 (hierarchical): AGEGRP codes present", {
  skip_if_not(.census_extracted("2006 (hierarchical)"),
              "Census 2006 (hierarchical) not in cache")

  canpumf:::pumf_parse_metadata(.census_vdir("2006 (hierarchical)"))

  meta   <- canpumf:::read_metadata(
    file.path(.census_vdir("2006 (hierarchical)"), "metadata"))
  agegrp <- meta$codes[meta$codes$name == "AGEGRP", ]
  expect_gt(nrow(agegrp), 5L,
    label = "AGEGRP must have value labels (tests single-quote SPSS parser)")
  expect_true(any(grepl("years", agegrp$label_en)),
    label = "AGEGRP labels should contain 'years'")
})

test_that("Census 2006 (hierarchical): registry codes_supplement for MORGH", {
  reg   <- canpumf:::pumf_registry_lookup("Census", "2006 (hierarchical)")
  suppl <- reg$data_fixups$codes_supplement
  expect_false(is.null(suppl),
    label = "codes_supplement should be defined for Census 2006 (hierarchical)")
  expect_true("MORGH" %in% names(suppl),
    label = "MORGH should have a codes_supplement entry")
  morgh <- suppl[["MORGH"]]
  expect_true("8" %in% morgh$val,
    label = "MORGH codes_supplement should contain code 8")
  # PDF user guide: 8 = "Not available" / "Non disponible" (see
  # override_verification.csv)
  expect_true("Not available" %in% morgh$label_en,
    label = "MORGH code 8 should be labeled 'Not available'")
})

test_that("Census 2001 (families): registry codes_supplement for MODEF", {
  reg   <- canpumf:::pumf_registry_lookup("Census", "2001 (families)")
  suppl <- reg$data_fixups$codes_supplement
  expect_false(is.null(suppl),
    label = "codes_supplement should be defined for Census 2001 (families)")
  expect_true("MODEF" %in% names(suppl),
    label = "MODEF should have a codes_supplement entry")
  modef <- suppl[["MODEF"]]
  expect_true("7" %in% modef$val,
    label = "MODEF codes_supplement should contain code 7")
  expect_true("Other method" %in% modef$label_en,
    label = "MODEF code 7 should be labeled 'Other method'")
})


# ---- Bilingual label coverage tests -----------------------------------------

# For every verified version: metadata must have English and French labels for
# both variables and value codes.  Province code 59 (British Columbia /
# Colombie-Britannique) is present in all Census PUMF files and provides a
# reliable bilingual anchor.
#
# Notes on older files:
# - 1986/families, 1976/households, 1976/families: no French labels at all
#   (English-only SPSS); both fr_vars and French codes checks are skipped.
# - Pre-1986 files use ALL CAPS labels, so province checks use ignore.case=TRUE.
# - Some 1971/1981 households files have "COLUMBIE" (typo) rather than
#   "COLOMBIE", so the pattern "Col[ou]mbie" covers both spellings.
.census_no_fr_labels <- c(
  "1986/families",
  "1976/households", "1976/families"
)

for (.v in .census_verified) {
  local({
    ver <- .v
    test_that(paste0("Census ", ver, ": metadata has English and French labels"), {
      skip_if_not(.census_metadata_exists(ver),
                  paste("Census", ver, "metadata not parsed"))

      meta <- canpumf:::read_metadata(file.path(.census_vdir(ver), "metadata"))

      en_vars <- sum(!is.na(meta$variables$label_en) &
                       nchar(meta$variables$label_en) > 0L)
      expect_gt(en_vars, 0L,
        label = paste0(ver, ": should have English variable labels"))

      if (!ver %in% .census_no_fr_labels) {
        fr_vars <- sum(!is.na(meta$variables$label_fr) &
                         nchar(meta$variables$label_fr) > 0L)
        expect_gt(fr_vars, 0L,
          label = paste0(ver, ": should have French variable labels"))
      }

      expect_true(any(grepl("British Columbia", meta$codes$label_en,
                            ignore.case = TRUE)),
        label = paste0(ver, ": 'British Columbia' expected in English codes"))

      if (!ver %in% .census_no_fr_labels)
        expect_true(any(grepl("Col[ou]mbie", meta$codes$label_fr,
                              ignore.case = TRUE)),
          label = paste0(ver, ": 'Colombie/Columbie' expected in French codes"))
    })
  })
}

# 1991 specific: bundled English XMF must produce genuinely English labels —
# not the French-only labels that come with the StatCan download.
test_that("Census 1991 (individuals): English labels from bundled XMF are genuinely English", {
  skip_if_not(.census_metadata_exists("1991 (individuals)"),
              "Census 1991 (individuals) metadata not parsed")

  meta <- canpumf:::read_metadata(
    file.path(.census_vdir("1991 (individuals)"), "metadata"))

  prov_en <- meta$codes$label_en[meta$codes$name == "PROVP" &
                                    meta$codes$val == "10"]
  expect_true(any(grepl("Newfoundland", prov_en, fixed = TRUE)),
    label = "PROVP code 10 should be 'Newfoundland' in English (not 'Terre-Neuve')")

  sex_en <- meta$codes$label_en[meta$codes$name == "SEXP"]
  expect_true(any(grepl("(?i)female|male", sex_en, perl = TRUE)),
    label = "SEXP codes should have English sex labels (Female/Male)")
  expect_false(any(grepl("(?i)femme|homme", sex_en, perl = TRUE)),
    label = "SEXP label_en should not contain French 'Femme'/'Homme'")
})


# ---- Full pipeline tests (Stage 2 + Stage 3) --------------------------------

# One test per verified version: re-parse metadata (Stage 2) and rebuild the
# DuckDB into a temp file (Stage 3).  Using refresh=TRUE + a temp db_path means
# every run exercises the full import path — encoding bugs, sentinel detection,
# force_numeric, codes_supplement — regardless of what is cached on disk.

for (.v in .census_verified) {
  local({
    ver <- .v
    pat <- .census_supplement_warnings[[ver]]   # NULL for most versions

    test_that(paste0("Census ", ver, ": full pipeline emits no unexpected warnings"), {
      skip_if_not(.census_extracted(ver),
                  paste("Census", ver, "not extracted in cache"))

      reg  <- canpumf:::pumf_registry_lookup("Census", ver)
      tmp  <- tempfile(fileext = ".duckdb")
      con  <- NULL
      warns <- character(0L)

      withCallingHandlers(
        {
          canpumf:::pumf_parse_metadata(.census_vdir(ver),
                                         metadata_encoding = reg$metadata_encoding,
                                         refresh           = TRUE)
          r   <- canpumf:::pumf_build_duckdb(.census_vdir(ver), "Census", ver,
                                              lang    = "eng",
                                              db_path = tmp,
                                              refresh = TRUE)
          tbl <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
          con <<- tbl$src$con
          dplyr::collect(tbl)
        },
        warning = function(w) {
          warns <<- c(warns, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )

      if (!is.null(con)) DBI::dbDisconnect(con, shutdown = TRUE)
      unlink(tmp)

      if (!is.null(pat)) {
        unexpected <- warns[!grepl(pat, warns)]
        expect_identical(unexpected, character(0L),
          label = paste0("Census ", ver, ": unexpected warnings"))
      } else {
        expect_identical(warns, character(0L),
          label = paste0("Census ", ver, ": should have no warnings"))
      }
    })
  })
}


# ---- Stage 3: generic DuckDB build ------------------------------------------

test_that("Census: pumf_build_duckdb creates eng table", {
  v <- .census_any_version()
  skip_if(is.null(v), "No Census version in cache")
  skip_if_not(.census_metadata_exists(v), "Census metadata not parsed")

  r <- canpumf:::pumf_build_duckdb(.census_vdir(v), "Census", v, lang = "eng")
  expect_true(file.exists(r$db_path))
})

test_that("Census eng table: row count and PR labeling", {
  v <- .census_any_version()
  skip_if(is.null(v), "No Census version in cache")
  skip_if_not(.census_duckdb_exists(v), "Census DuckDB not built")

  r      <- canpumf:::pumf_build_duckdb(.census_vdir(v), "Census", v, lang = "eng")
  tbl    <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))
  result <- dplyr::collect(tbl)

  expect_gt(nrow(result), 50000L)
  expect_true("PR" %in% names(result))
  prov_vals <- unique(stats::na.omit(result$PR))
  expect_true(any(grepl("Ontario|British Columbia|Quebec|Alberta",
                          prov_vals, ignore.case = TRUE)),
    label = paste0("PR should have English labels; got: ",
                   paste(head(prov_vals, 5L), collapse = ", ")))
})

test_that("Census eng table: ENUM columns present", {
  v <- .census_any_version()
  skip_if(is.null(v), "No Census version in cache")
  skip_if_not(.census_duckdb_exists(v), "Census DuckDB not built")

  r   <- canpumf:::pumf_build_duckdb(.census_vdir(v), "Census", v, lang = "eng")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = r$db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  info      <- DBI::dbGetQuery(con, sprintf("PRAGMA table_info('%s')", r$table_name))
  enum_cols <- info$name[grepl("^ENUM", info$type)]
  expect_gt(length(enum_cols), 0L,
    label = "At least one ENUM column expected in Census eng table")
})

test_that("Census fra table: uses French labels", {
  v <- .census_any_version()
  skip_if(is.null(v), "No Census version in cache")
  skip_if_not(.census_metadata_exists(v), "Census metadata not parsed")

  meta <- canpumf:::read_metadata(file.path(.census_vdir(v), "metadata"))
  skip_if(all(is.na(meta$variables$label_fr)), "No French labels in metadata")

  r      <- canpumf:::pumf_build_duckdb(.census_vdir(v), "Census", v, lang = "fra")
  tbl    <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))
  result <- dplyr::collect(tbl)

  prov_vals <- unique(stats::na.omit(result$PR))
  expect_true(any(grepl("Ontario|Québec|Colombie|Alberta", prov_vals)),
    label = paste0("French PR labels expected; got: ",
                   paste(head(prov_vals, 5L), collapse = ", ")))
})


# ---- Bilingual parity (per verified version) --------------------------------

for (.v in .census_verified) {
  local({
    ver <- .v
    test_that(paste0("Census ", ver, ": eng/fra bilingual parity"), {
      skip_if_not(.census_extracted(ver),
                  paste("Census", ver, "not extracted in cache"))
      skip_if_not(.census_metadata_exists(ver),
                  paste("Census", ver, "metadata not parsed"))

      tmp <- tempfile(fileext = ".duckdb")
      on.exit(unlink(tmp), add = TRUE)

      r_eng <- suppressWarnings(
        canpumf:::pumf_build_duckdb(.census_vdir(ver), "Census", ver,
                                     lang = "eng", db_path = tmp, refresh = TRUE))
      r_fra <- suppressWarnings(
        canpumf:::pumf_build_duckdb(.census_vdir(ver), "Census", ver,
                                     lang = "fra", db_path = tmp, refresh = TRUE))

      eng <- .collect_pumf_table(tmp, r_eng$table_name)
      fra <- .collect_pumf_table(tmp, r_fra$table_name)

      expect_pumf_bilingual_parity(eng, fra, label = paste0("Census ", ver))
    })
  })
}


# ---- Version-specific data fixup tests --------------------------------------

test_that("Census 2021: RELIGION_DER renamed to RELIG", {
  skip_if_not(.census_duckdb_exists("2021 (individuals)"),
              "Census 2021 (individuals) DuckDB not built")

  r      <- canpumf:::pumf_build_duckdb(.census_vdir("2021 (individuals)"),
                                         "Census", "2021 (individuals)", lang = "eng")
  tbl    <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))
  result <- dplyr::collect(tbl)

  expect_false("RELIGION_DER" %in% names(result),
    label = "RELIGION_DER should be renamed to RELIG by the data fixup")
})


# ---- Public API -------------------------------------------------------------

test_that("get_pumf: Census returns lazy tbl", {
  v <- .census_any_version()
  skip_if(is.null(v), "No Census version in cache")
  skip_if_not(.census_metadata_exists(v), "Census metadata not parsed")

  tbl <- get_pumf("Census", v, lang = "eng", cache_path = .census_cache())
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))

  expect_s3_class(tbl, "tbl")
  count <- dplyr::count(tbl) |> dplyr::collect()
  expect_gt(count$n, 50000L)
})

test_that("pumf_metadata: Census returns bilingual metadata list", {
  v <- .census_any_version()
  skip_if(is.null(v), "No Census version in cache")

  m <- pumf_metadata("Census", v, cache_path = .census_cache())

  expect_named(m, c("variables", "codes", "layout"), ignore.order = TRUE)
  expect_gt(nrow(m$variables), 50L)
  expect_true("PR" %in% m$variables$name)
  expect_true(all(c("label_en", "label_fr") %in% names(m$variables)))
})
