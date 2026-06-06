# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

`canpumf` is an R package that facilitates ingesting Statistics Canada Public Use Microdata Files (PUMF) into R. It parses SPSS Command Files and other metadata formats to infer layout structure, variable labels, value labels, and missing data values.

## Common Commands

```r
# Install dependencies / load package during development
devtools::load_all()

# Document (rebuild Rd files and NAMESPACE from roxygen2 comments)
devtools::document()

# Run R CMD check
devtools::check()

# Build pkgdown site
pkgdown::build_site()
```

There are no automated tests (no `testthat` suite). The file `R/local_test.R` contains manual test code wrapped in `if (FALSE)` — it is excluded from the build and used for ad-hoc testing during development.

## Architecture

### Data access flow

1. **`get_pumf(pumf_series, pumf_version, ...)`** — main entry point. Downloads (if needed) and reads PUMF data into a tibble.
2. **`get_pumf_connection(...)`** — variant that persists data to a DuckDB database and returns a `duckplyr` table reference for lazy/out-of-memory access.
3. **`label_pumf_data(pumf_data)`** — parses metadata and labels categorical columns as R factors with human-readable labels. Optionally renames columns via `rename_columns=TRUE`.

### Metadata parsing

Parsed metadata is cached in a `canpumf/` subdirectory inside the PUMF data directory as `.Rds` files:
- `var.Rds` — variable labels (name → label)
- `val.Rds` — value labels (name, val → label)  
- `miss.Rds` — missing value ranges for numeric columns

Three parsers handle different StatCan formats:
- **`R/layout_spss.R`** — parses SPSS `.sps` command files (most common; used for LFS, Census, CHS, etc.)
- **`R/layout_cards.R`** — parses "Reading cards" fixed-width format
- **`R/layout_csv.R`** — parses CSV-format metadata (CPSS and similar)

### Survey-specific modules

Each module in `R/` handles the quirks of a specific survey family:
- `R/lfs.R` — Labour Force Survey (uses CSV codebook, organized by year or year-month)
- `R/census.R` — Census PUMF (1971–present; recent years downloadable, older via EFT)
- `R/chs.R` — Canadian Housing Survey
- `R/sfs.R` — Survey of Financial Security
- `R/shs.R` — Survey of Household Spending

### Key helpers

- `R/helpers.R` — path resolution utilities (`pumf_layout_dir`, `pumf_data_dir`, `pumf_clean_layout_dir`, `find_unique_layout_file`), unzip helpers, and the import declarations for the package.
- `R/pumf_cache_path.R` — maps `(pumf_series, pumf_version)` to the on-disk cache directory path (handles Census product-code-based directory naming).
- `R/pumf_collection.R` — `list_canpumf_collection()` scrapes StatCan and returns download URLs for all supported series/versions.
- `R/pumf_documentation.R` — `open_pumf_documentation()` helper.

### Cache and storage

Users must set `options(canpumf.cache_path="<path>")` (typically in `.Rprofile`) to persist data across sessions. Without this, data is stored in `tempdir()` for the session only. The cache stores raw PUMF zip contents in `<cache_path>/<series>/<version>/`, and metadata in a `canpumf/` subdirectory alongside the raw files.

For DuckDB-backed access, a `.duckdb` file is written to the version directory the first time `get_pumf_connection()` is called.

### Layout file discovery

`pumf_layout_dir()` walks the PUMF directory tree looking for directories named `SPSS`, `Layout`, `Syntax`, `Command`, `SpssCard`, or `Reading cards`. The `layout_mask` parameter is passed through the call chain and used to disambiguate when multiple `.sps` or `.lay` files exist (e.g., for surveys with separate person/family/household files).

### Data attribute pattern

The `pumf_base_path` and `layout_mask` are stored as R attributes on the returned tibble, allowing subsequent calls like `label_pumf_data()`, `label_pumf_columns()`, and `convert_pumf_numeric_columns()` to omit these arguments — they are inferred automatically from the data object.
