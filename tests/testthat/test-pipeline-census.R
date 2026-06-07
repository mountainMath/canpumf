# Integration tests for the Census pipeline (SPSS monolithic format).
# Census 2021 and 2016 are downloadable from StatCan; older years are EFT-only.
# These tests run against data already in the user's canpumf cache.

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

# Pick any downloadable Census version that's in cache
.census_any_version <- function() {
  for (v in c("2021 (individuals)", "2016 (individuals)",
               "2021 (hierarchical)", "2016 (hierarchical)")) {
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
  # All Census files have a PR variable (province)
  expect_true("PR" %in% meta$variables$name,
              label = "PR variable expected in Census metadata")
  # 2021 and 2016 have bilingual command files
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

  # UTF-8 parsing should produce proper Unicode, not mojibake.
  # Mojibake occurs when UTF-8 bytes are read as Latin-1: "é" (U+00E9) would
  # appear as two characters "Ã©" (0xC3 0xA9 misread as two Latin-1 chars).
  fr_labels <- stats::na.omit(meta$variables$label_fr)
  if (length(fr_labels) > 0L) {
    # Should contain proper accented characters (é, è, â, etc.)
    expect_true(any(grepl("[éèêëàâîïôùûüç]", fr_labels)),
      label = "French labels should contain accented characters (é, è, etc.)")
    # Must NOT contain the mojibake signature "Ã" (UTF-8 0xC3 read as Latin-1)
    expect_false(any(grepl("Ã", fr_labels)),
      label = "French labels must not contain mojibake 'Ã' (UTF-8 read as Latin-1)")
  }
})


# ---- Stage 3: DuckDB build --------------------------------------------------

test_that("Census: pumf_build_duckdb creates eng table", {
  v <- .census_any_version()
  skip_if(is.null(v), "No Census version in cache")
  skip_if_not(.census_metadata_exists(v), "Census metadata not parsed")

  reg <- canpumf:::pumf_registry_lookup("Census", v)
  r   <- canpumf:::pumf_build_duckdb(.census_vdir(v), "Census", v,
                                      lang = "eng",
                                      layout_mask = reg$layout_mask)

  expect_true(file.exists(r$db_path))
})

test_that("Census eng table: row count and PR labeling", {
  v <- .census_any_version()
  skip_if(is.null(v), "No Census version in cache")
  skip_if_not(.census_duckdb_exists(v), "Census DuckDB not built")

  reg  <- canpumf:::pumf_registry_lookup("Census", v)
  r    <- canpumf:::pumf_build_duckdb(.census_vdir(v), "Census", v,
                                       lang = "eng", layout_mask = reg$layout_mask)
  tbl  <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))

  result <- dplyr::collect(tbl)
  # All Census PUMF files have 1M+ records
  expect_gt(nrow(result), 500000L)
  expect_true("PR" %in% names(result))

  prov_vals <- unique(stats::na.omit(result$PR))
  # Labels should be province names, not codes
  expect_true(any(grepl("Ontario|British Columbia|Quebec|Alberta",
                          prov_vals, ignore.case = TRUE)),
    label = paste0("PROV should have English labels; got: ",
                   paste(head(prov_vals, 5), collapse = ", ")))
})

test_that("Census eng table: ENUM columns present", {
  v <- .census_any_version()
  skip_if(is.null(v), "No Census version in cache")
  skip_if_not(.census_duckdb_exists(v), "Census DuckDB not built")

  reg <- canpumf:::pumf_registry_lookup("Census", v)
  r   <- canpumf:::pumf_build_duckdb(.census_vdir(v), "Census", v,
                                      lang = "eng", layout_mask = reg$layout_mask)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = r$db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  info      <- DBI::dbGetQuery(con, sprintf("PRAGMA table_info('%s')", r$table_name))
  enum_cols <- info$name[grepl("^ENUM", info$type)]
  expect_gt(length(enum_cols), 0L,
    label = "At least one ENUM column expected in Census eng table")
})

test_that("Census: fra table uses French labels", {
  v <- .census_any_version()
  skip_if(is.null(v), "No Census version in cache")
  skip_if_not(.census_metadata_exists(v), "Census metadata not parsed")

  # Only test French if there are French labels in the metadata
  meta <- canpumf:::read_metadata(file.path(.census_vdir(v), "metadata"))
  skip_if(all(is.na(meta$variables$label_fr)), "No French labels in metadata")

  reg  <- canpumf:::pumf_registry_lookup("Census", v)
  r    <- canpumf:::pumf_build_duckdb(.census_vdir(v), "Census", v,
                                       lang = "fra", layout_mask = reg$layout_mask)
  tbl  <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))

  result <- dplyr::collect(tbl)
  prov_vals <- unique(stats::na.omit(result$PR))
  expect_true(any(grepl("Ontario|Québec|Colombie|Alberta", prov_vals)),
    label = paste0("French PROV labels expected; got: ",
                   paste(head(prov_vals, 5), collapse = ", ")))
})

test_that("Census 2021: RELIGION_DER renamed to RELIG (data fixup)", {
  skip_if_not(.census_metadata_exists("2021 (individuals)"),
              "Census 2021 metadata not parsed")
  skip_if_not(.census_duckdb_exists("2021 (individuals)"),
              "Census 2021 DuckDB not built")

  reg <- canpumf:::pumf_registry_lookup("Census", "2021 (individuals)")
  r   <- canpumf:::pumf_build_duckdb(.census_vdir("2021 (individuals)"),
                                      "Census", "2021 (individuals)",
                                      lang = "eng")
  tbl <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))

  result <- dplyr::collect(tbl)
  # Either RELIG exists (renamed from RELIGION_DER) or neither does
  expect_false("RELIGION_DER" %in% names(result),
    label = "RELIGION_DER should be renamed to RELIG by the data fixup")
})

# ---- Public API via get_pumf ------------------------------------------------

test_that("get_pumf: Census returns lazy tbl", {
  v <- .census_any_version()
  skip_if(is.null(v), "No Census version in cache")
  skip_if_not(.census_metadata_exists(v), "Census metadata not parsed")

  tbl <- get_pumf("Census", v, lang = "eng",
                   cache_path = .census_cache())
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))

  expect_s3_class(tbl, "tbl")
  # Lazy: don't collect — just check it's queryable
  count <- dplyr::count(tbl) |> dplyr::collect()
  expect_gt(count$n, 500000L)
})

# ---- pumf_metadata for Census -----------------------------------------------

test_that("pumf_metadata: Census returns bilingual metadata list", {
  v <- .census_any_version()
  skip_if(is.null(v), "No Census version in cache")

  m <- pumf_metadata("Census", v, cache_path = .census_cache())

  expect_named(m, c("variables","codes","layout"), ignore.order = TRUE)
  expect_gt(nrow(m$variables), 50L)
  expect_true("PR" %in% m$variables$name)
  expect_true(all(c("label_en","label_fr") %in% names(m$variables)))
})
