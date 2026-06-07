# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

`canpumf` is an R package that facilitates ingesting Statistics Canada Public Use Microdata Files (PUMF) into R. It downloads, parses metadata, applies bilingual labels, and returns lazy DuckDB-backed tables for efficient out-of-memory access.

## Common Commands

```r
# Install dependencies / load package during development
devtools::load_all()

# Document (rebuild Rd files and NAMESPACE from roxygen2 comments)
devtools::document()

# Run tests
devtools::test()

# Run R CMD check
devtools::check()

# Build pkgdown site
pkgdown::build_site()
```

Tests live in `tests/testthat/`. There is no `R/local_test.R`-style ad-hoc testing — all tests run via `devtools::test()`.

## Architecture

### Data access flow (new DuckDB pipeline — `duckdb` branch)

1. **`get_pumf(series, version, lang="eng", ...)`** — main entry point. Runs all three pipeline stages and returns a lazy `dplyr::tbl()` backed by DuckDB. Call `dplyr::collect()` for a local tibble. For LFS, delegates to `lfs_get_pumf()`.
2. **`pumf_metadata(series, version, ...)`** — runs Stage 1+2 only; returns the bilingual canonical metadata list (variables, codes, layout).
3. **`open_pumf_documentation(series, version, ...)`** — scans the cache directory for PDF/TXT docs and opens them in the browser.

### Three-stage pipeline (non-LFS)

All standard surveys use an idempotent three-stage pipeline in `R/pipeline.R`:

1. **Stage 1 — `pumf_locate_or_download(series, version, cache_path, refresh)`**: ensures the version directory at `<cache_path>/<series>/<version>/` exists with extracted content.
2. **Stage 2 — `pumf_parse_metadata(version_dir, layout_mask, metadata_encoding, refresh)`**: detects and parses all metadata formats, merges into canonical CSVs in `<version_dir>/metadata/`.
3. **Stage 3 — `pumf_build_duckdb(version_dir, series, version, lang, ...)`**: reads data, joins BSW weights, applies labels, writes to `<version_dir>/<series>_<version>.duckdb`. Returns path list; use `pumf_open_duckdb()` to get a lazy tbl.

`pumf_run_pipeline()` chains all three stages using registry config.

### LFS longitudinal pipeline (`R/lfs_pipeline.R`)

LFS uses a single shared DuckDB at `<cache_path>/LFS/LFS.duckdb` accumulating all versions:
- `lfs_eng` and `lfs_fra` tables store labeled rows (VARCHAR categoricals, not ENUM)
- `lfs_versions` tracking table records what has been downloaded and parsed
- Annual versions supersede monthly versions for the same year
- Schema evolution: `ALTER TABLE ADD COLUMN` when new versions add variables

### Metadata parsers (`R/metadata_parsers.R`)

Six parsers converge on three canonical CSV files in `<version_dir>/metadata/`:
- `variables.csv` — one row per variable (name, label_en, label_fr, type, decimals, missing_low, missing_high)
- `codes.csv` — one row per code value (name, val, label_en, label_fr)
- `layout.csv` — one row per fixed-width column (name, start, end); absent for CSV-format data

Parsers: `parse_spss_mono()`, `parse_spss_split()`, `parse_sas_cards()`, `parse_lfs_codebook()`, `parse_cpss_csv()`, `parse_spss_sav()`.

### Survey registry (`R/registry.R`)

`pumf_registry_lookup(series, version)` returns per-survey configuration:
- `layout_mask` — SPSS file disambiguation for split-file surveys
- `bsw_mask`, `bsw_file_mask`, `bsw_join_key`, `bsw_drop_cols` — bootstrap weight join config
- `file_mask` — data file selector
- `data_encoding`, `metadata_encoding` — encoding overrides
- `data_fixups` — `str_pad` and `rename` transformations applied before label mapping

Surveys without a registry entry use auto-detection.

### Cache and storage

Users must set `options(canpumf.cache_path="<path>")` (typically in `.Rprofile`) to persist data across sessions. Without this, data is stored in `tempdir()` for the session only.

Cache layout:
```
<cache_path>/
  <series>/
    <version>/
      <original>.zip          # retained
      <series>_<version>.duckdb
      metadata/
        variables.csv
        codes.csv
        layout.csv            # only for fixed-width data
  LFS/
    LFS.duckdb                # single shared database for all LFS versions
    <version>/
      <original>.zip
      metadata/
        variables.csv
        codes.csv
```

### Key files

- `R/api.R` — `get_pumf()`, `pumf_metadata()` (public entry points)
- `R/pipeline.R` — Stage 1 (`pumf_locate_or_download`), Stage 3 (`pumf_build_duckdb`, `pumf_open_duckdb`), `pumf_run_pipeline`
- `R/lfs_pipeline.R` — `lfs_get_pumf()` and LFS-specific helpers
- `R/registry.R` — `pumf_registry_lookup()`, `pumf_registry_keys()`
- `R/metadata_parsers.R` — all six parsers, `detect_formats()`, `merge_metadata()`, `pumf_parse_metadata()`, `read_metadata()`, `write_metadata()`
- `R/helpers.R` — `robust_unzip()`, `pumf_layout_dir()`, `find_unique_layout_file()`, import declarations
- `R/pumf_collection.R` — `list_canpumf_collection()`, `list_available_lfs_pumf_versions()`
- `R/pumf_documentation.R` — `open_pumf_documentation()`
- `R/pumf.R` — deprecated legacy functions (`label_pumf_data`, `convert_pumf_numeric_columns`, `read_pumf_data`, etc.) kept for backward compatibility

### Deprecated functions (keep with warning, remove in next major version)

- `get_pumf_connection()` → use `get_pumf()`
- `label_pumf_data()`, `label_pumf_columns()`, `convert_pumf_numeric_columns()`, `guess_numeric_pumf_columns()` → handled automatically by new pipeline
- `read_pumf_data()` → use `get_pumf()` or `pumf_metadata()`; kept for manual-deposit use cases
- Old parameter names `pumf_series`, `pumf_version`, `pumf_cache_path` → use `series`, `version`, `cache_path`
