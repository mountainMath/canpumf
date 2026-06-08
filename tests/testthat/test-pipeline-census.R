# Integration tests for the Census of Population pipeline.
# All releases from 1991 onward are available via direct download.
# Tests run against data already in the user's canpumf cache.

.census_cache <- function() getOption("canpumf.cache_path", "")

.census_vdir <- function(version) {
  file.path(.census_cache(), "Census", version)
}

.census_extracted <- function(version) {
  canpumf:::.version_is_extracted(.census_vdir(version))
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
  "2001 (individuals)", "2001 (households)", "2001 (families)"
)

# Census versions where codes_supplement injects manually: map version ->
# expected warning regex.  Any warning NOT matching this pattern is unexpected.
.census_supplement_warnings <- list(
  "2006 (hierarchical)" = "absent from command files",
  "2001 (families)"     = "absent from command files"
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
  expect_true("Not stated" %in% morgh$label_en,
    label = "MORGH code 8 should be labeled 'Not stated'")
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


# ---- Stage 3: per-version pipeline tests ------------------------------------

# One test per verified version: collect from existing DuckDB and check for
# unexpected warnings during build.  pumf_build_duckdb skips re-building when
# the table already exists (refresh=FALSE), so on already-built caches these
# tests verify that collect() itself is warning-free.  On fresh caches they
# additionally cover the build step.

for (.v in .census_verified) {
  local({
    ver <- .v
    pat <- .census_supplement_warnings[[ver]]   # NULL for most versions

    test_that(paste0("Census ", ver, ": pipeline emits no unexpected warnings"), {
      skip_if_not(.census_duckdb_exists(ver),
                  paste("Census", ver, "DuckDB not built"))

      warns <- character(0L)
      withCallingHandlers(
        {
          r   <- canpumf:::pumf_build_duckdb(.census_vdir(ver), "Census", ver,
                                              lang = "eng")
          tbl <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
          on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))
          dplyr::collect(tbl)
        },
        warning = function(w) {
          warns <<- c(warns, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )

      if (!is.null(pat)) {
        unexpected <- warns[!grepl(pat, warns)]
        expect_identical(unexpected, character(0L))
        # If DuckDB was just built (not a skip), check the supplement fired
        # (we can't reliably test this on pre-built caches)
      } else {
        expect_identical(warns, character(0L))
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
