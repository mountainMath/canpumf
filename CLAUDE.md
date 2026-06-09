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

Tests live in `tests/testthat/`. All tests run via `devtools::test()`. `R/local_test.R` is an ad-hoc scratch file and is not part of the test suite.

**When adding or changing tests**, keep `tests/TEST_COVERAGE.md` in sync: update the coverage matrix row for the affected survey, and update the **Verified datasets** table in `README.md` to match what the registry and test suite actually cover. The registry (`R/registry.R`), the test suite, and the README verified-datasets table are the three sources of truth — they must agree.

## Public API

### Primary entry points (`R/api.R`)

1. **`get_pumf(series, version, lang="eng", ...)`** — main entry point. Runs all three pipeline stages and returns a lazy `dplyr::tbl()` backed by DuckDB. Values come pre-labeled (factors with human-readable levels). Call `dplyr::collect()` for a local tibble. For LFS, delegates to `lfs_get_pumf()`. Registers connection provenance so `label_pumf_columns()` works downstream.
2. **`label_pumf_columns(tbl)`** — renames tbl columns from short coded names (e.g. `PHHSIZE`) to human-readable variable labels (e.g. `"Household size"`). Works on any tbl returned by `get_pumf()`, including after dplyr filters. Looks up survey provenance from the connection registry.
3. **`close_pumf(tbl)`** — disconnects the DuckDB connection backing a lazy tbl. Only needed before writing (e.g. `refresh = TRUE`) to the same file from another tbl.
4. **`pumf_metadata(series, version, ...)`** — runs Stage 1+2 only; returns the canonical metadata list (`variables`, `codes`, `layout`).
5. **`open_pumf_documentation(series, version, ...)`** — scans the cache directory for PDF/TXT docs and opens them in the browser.

### Connection provenance registry (`R/api.R`)

`get_pumf()` registers `(series, version, cache_path, lang)` in a package-level environment keyed by the DuckDB connection's C++ external-pointer address. This key is stable across R copies of the S4 connection wrapper (R's copy-on-modify would silently lose attrs set directly on the wrapper). `label_pumf_columns()` uses `.pumf_lookup_con()` to retrieve this provenance; `close_pumf()` removes it.

## Architecture

### Three-stage pipeline (non-LFS)

All standard surveys use an idempotent three-stage pipeline in `R/pipeline.R`:

1. **Stage 1 — `pumf_locate_or_download(series, version, cache_path, refresh)`**: ensures the version directory at `<cache_path>/<series>/<version>/` exists with extracted content.
2. **Stage 2 — `pumf_parse_metadata(version_dir, layout_mask, metadata_encoding, refresh)`**: detects and parses all metadata formats, merges into canonical CSVs in `<version_dir>/metadata/`.
3. **Stage 3 — `pumf_build_duckdb(version_dir, series, version, lang, layout_mask, file_mask, refresh)`**: reads data file, joins BSW weights, applies numeric conversion and code labels, writes to `<version_dir>/<series>_<version>.duckdb`. Returns path list; use `pumf_open_duckdb()` to get a lazy tbl.

`pumf_run_pipeline()` chains all three stages using registry config.

#### Data file detection

`.find_pumf_data_file(version_dir, file_mask, prefer_fwf)` selects the data file. The extension pattern is derived from the `file_mask` first (`.csv` → CSV, `.txt`/`.dat` → FWF). When `file_mask` uses an unrecognised extension (e.g. `.INDIV` for the 1991 Census), `ext_pat` is set to `NULL` and all files are searched, with `file_mask` alone selecting the result. The final `is_fwf` decision is made from the actual found file extension and the presence of `layout.csv` — this handles surveys like CHS that ship both a CSV and a TXT file but whose SPSS DATA LIST section also creates a `layout.csv`.

### LFS longitudinal pipeline (`R/lfs_pipeline.R`)

LFS uses a single shared DuckDB at `<cache_path>/LFS/LFS.duckdb` accumulating all versions:
- `lfs_eng` and `lfs_fra` tables store labeled rows (VARCHAR/ENUM categoricals)
- `lfs_versions` tracking table records what has been downloaded and parsed
- Annual versions supersede monthly versions for the same year
- Schema evolution: `ALTER TABLE ADD COLUMN` / `ALTER COLUMN SET DATA TYPE` when new versions add or change variables
- `get_pumf("LFS", version)` returns the full shared table **filtered** to the requested year (and month for monthly versions); `get_pumf("LFS")` returns the full unfiltered table

#### `label_pumf_columns()` for LFS

Because the shared `lfs_eng/lfs_fra` schema is the union of all loaded versions, variables introduced in later years (e.g. `GENDER` added ~2020) are absent from older versions' `variables.csv`. `label_pumf_columns()` therefore reads and merges `variables.csv` from **every** loaded version directory in chronological order, with the most-recent label winning on conflicts.

### Metadata parsers (`R/metadata_parsers.R`)

Six parsers converge on three canonical CSV files in `<version_dir>/metadata/`:
- `variables.csv` — one row per variable (name, label_en, label_fr, type, decimals, missing_low, missing_high)
- `codes.csv` — one row per code value (name, val, label_en, label_fr)
- `layout.csv` — one row per fixed-width column (name, start, end); absent for CSV-format data

Parsers (in detection priority order):
1. `parse_lfs_codebook()` — LFS `*codebook.csv`; always read as CP1252
2. `parse_cpss_csv()` — CPSS `variables.csv`
3. `parse_sas_cards()` — directory with `.lay` + `.lbe` files
4. `parse_spss_split()` — directory with `vare`/`vale`/`_i` named `.sps` files
5. `parse_spss_mono()` — single `.sps`, `*SPSS.txt`, or `.xmf` file whose content contains `VALUE LABELS` or `DATA LIST`. `VARIABLE LABELS` is optional (e.g. Census 2011 individuals). DATA LIST-only files (e.g. SFS 1999) produce a table with correct column types but no human-readable labels.
6. `parse_spss_sav()` — binary SPSS `.sav` file (read via haven)

Multiple parsers can fire for the same survey (e.g. split-SPSS for layout/codes and SAS cards for BSW weights). `merge_metadata()` consolidates all results.

Key parsing details:
- **Sentinel detection**: variables whose only value labels are sentinel phrases (Not applicable, Not stated, Not in universe, Not available, Valid skip, Refusal, Refused, Don't know, Missing, N/A, Does not apply, Not in scope) are classified as `numeric` with a `missing_low/missing_high` range rather than `character`, to avoid spurious NA warnings when numeric values have no matching label.
- **Zero-padded codes**: unquoted SPSS numeric codes like `01`, `02` are normalized via `as.numeric()` → `as.character()` so they match bare integer values in CSV data.
- **Multi-variable VALUE LABELS blocks**: `/VAR1 VAR2 VAR3` headers (possibly spanning continuation lines) are fully parsed so all listed variables receive the code/label pairs.
- **SPSS DATA LIST column ranges**: spaces around the dash are tolerated in all forms — `129-135`, `129 - 135`, `129-  135` — via `(\\d+)\\s*-\\s*(\\d+)` normalisation before tokenisation. A leading `/` record-group marker on the first variable line is stripped (not discarded) so the variable is retained.
- **SPSS DATA LIST section terminator**: the section ends at the first blank line, `.` line, or occurrence of `VARIABLE LABELS`, `VALUE LABELS`, `MISSING VALUES`, `FORMATS`, or `EXECUTE` at the start of a line. The keyword check is the reliable terminator for older files (e.g. 1991 XMF) that have no blank line between `DATA LIST` and `VARIABLE LABELS`.
- **SPSS DATA LIST decimals**: no annotation → `fmt_type="F"`, `decimals=0` (integer); `(A)` or `(An)` → character format; `(n)` → `decimals=n`; `(Fn.d)` → `decimals=d`.
- **DATA LIST-only SPSS files**: `_spss_parse_data_list` returns an `is_char` flag per column (derived from `(A)` annotations in the raw section lines). When a SPSS file has `DATA LIST` but no `VARIABLE LABELS` or `VALUE LABELS` (e.g. SFS 1999), `.spss_mono_single` falls back to populating `variables.csv` from the layout type info: `(A)` columns → `type="character"`, others → `type="numeric"`. Labels are all `NA`. This produces a fully importable DuckDB table with raw codes but no human-readable factor levels.
- **Metadata encoding**: default is `"CP1252"` (superset of Latin-1, handles Windows-era en-dashes and curly quotes). Exceptions: Census 2021 uses `"UTF-8"` (command files shipped as UTF-8); Census 1991 (individuals) uses `"CP850"` (DOS-era IBM Code Page 850). The `detect_formats()` SPSS keyword scan uses `useBytes = TRUE` to tolerate non-UTF-8 bytes without warnings regardless of encoding.

### Survey registry (`R/registry.R`)

`pumf_registry_lookup(series, version)` returns per-survey configuration:
- `layout_mask` — SPSS file disambiguation for split-file surveys
- `bsw_mask`, `bsw_file_mask`, `bsw_join_key`, `bsw_drop_cols` — bootstrap weight join config
- `file_mask` — data file selector (extension determines CSV vs FWF)
- `data_encoding`, `metadata_encoding` — encoding overrides
- `data_fixups` — `str_pad` and `rename` transformations applied before label mapping; `na_values` character vector of raw string sentinels that become `NA` for all numeric columns (used for undeclared Census income sentinels)

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

- `R/api.R` — `get_pumf()`, `label_pumf_columns()`, `close_pumf()`, `pumf_metadata()`, connection provenance registry
- `R/pipeline.R` — Stage 1 (`pumf_locate_or_download`), Stage 3 (`pumf_build_duckdb`, `pumf_open_duckdb`), `pumf_run_pipeline`, `.find_pumf_data_file`, `.read_bsw_data`
- `R/lfs_pipeline.R` — `lfs_get_pumf()` and LFS-specific helpers
- `R/registry.R` — `pumf_registry_lookup()`, `pumf_registry_keys()`
- `R/metadata_parsers.R` — all six parsers, `detect_formats()`, `merge_metadata()`, `pumf_parse_metadata()`, `read_metadata()`, `write_metadata()`
- `R/helpers.R` — `robust_unzip()`, import declarations
- `R/pumf_collection.R` — `list_canpumf_collection()`, `list_available_lfs_pumf_versions()`
- `R/pumf_documentation.R` — `open_pumf_documentation()`
- `R/pumf.R` — deprecated legacy functions kept for backward compatibility

### Deprecated functions (keep with warning, remove in next major version)

- `get_pumf_connection()` → use `get_pumf()`
- `label_pumf_data()` → data is already labeled by `get_pumf()`; use `label_pumf_columns()` if column renaming is needed
- `convert_pumf_numeric_columns()`, `guess_numeric_pumf_columns()` → handled automatically by the pipeline
- `read_pumf_data()` → use `get_pumf()` or `pumf_metadata()`; kept for manual-deposit use cases
- Old parameter names `pumf_series`, `pumf_version`, `pumf_cache_path` → use `series`, `version`, `cache_path`
