# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

`canpumf` is an R package for loading Statistics Canada Public Use Microdata Files (PUMF) into R. It downloads the files (from StatCan, or from the Borealis Dataverse for vintages StatCan does not post), parses the metadata, applies bilingual labels, and returns lazy DuckDB-backed tables so the data can be used without loading it into memory.

## Topic docs (read before changing that area)

| File | Covers |
|---|---|
| [docs/metadata-parsers.md](docs/metadata-parsers.md) | The nine parsers, sentinel/missing detection, SPSS/SAS/PDF parsing quirks, encodings, mojibake repair |
| [docs/pdf-crosscheck.md](docs/pdf-crosscheck.md) | User-guide PDF parser, frequency validation, truncation fingerprints, label repair (`R/pdf_repair.R`) |
| [docs/registry.md](docs/registry.md) | Registry fields and `data_fixups`, sibling inheritance, version aliases (incl. EFT-vs-Borealis Census resolution), download-URL resolution, the Borealis source, **override verification workflow** |
| [docs/multi-module.md](docs/multi-module.md) | Linked-module surveys (GSS 16, GSS Time Use, SHS 2017, SGVP): registry, pipeline, `pumf_module()` |
| `tests/TEST_COVERAGE.md` | What each test file covers; per-survey coverage matrix |

## Common Commands

```r
devtools::load_all()     # load package during development
devtools::document()     # rebuild Rd + NAMESPACE from roxygen2
devtools::test()         # run tests (tests/testthat/)
devtools::check()        # R CMD check
pkgdown::build_site()
```

`R/local_test.R` is an ad-hoc scratch file and is not part of the test suite.

## Sources of truth that must agree

The registry (`R/registry.R`), the test suite, and the **Verified datasets** table in `README.md` must describe the same set of datasets. **When adding or changing tests**, update the matching row of the coverage matrix in `tests/TEST_COVERAGE.md` and the README table. **Every manual registry override** also needs a verified row in `tests/testthat/override_verification.csv` (see [docs/registry.md](docs/registry.md#override-verification-workflow)).

## Public API

- **`get_pumf(series, version, lang="eng", ...)`**: the main entry point. It runs the pipeline and returns a lazy `dplyr::tbl()` whose categorical values are pre-labelled factors. Options include `module =` and `registry =`. LFS is delegated to `lfs_get_pumf()`. The function registers connection provenance (below).
- **`label_pumf_columns(tbl)`**: renames coded column names (`PHHSIZE`) to variable labels ("Household size"). It works after dplyr verbs and is module-aware. `pumf_var_labels()` returns the label lookup itself.
- **`close_pumf(x)`**: `x` is a lazy tbl (closes `x$src$con`) or a DuckDB connection (detected via `inherits(x, "DBIConnection")`). Registry cleanup is guarded with `exists()`, because `get_pumf_connection()` never registers. Only needed before writing to the same file from another tbl.
- **`pumf_metadata()`**: runs Stages 1+2 and returns `list(variables, codes, layout)`. **`open_pumf_documentation()`**: opens cached PDF/TXT docs.
- **`get_pumf_connection()`** (exported, in `R/pumf.R`): returns a **read-write** DuckDB connection and is not registered. **`read_pumf_data()`**: covers the case where the user deposits files manually.
- **Bootstrap weights** (`R/api.R`): `add_bootstrap_weights(tbl, weight_col, ...)` works on DuckDB-backed or in-memory tbls. `remove_bootstrap_weights()` drops the BSW table and its companion view. `bsw_info()` summarises the BSW tables present.
- **Label repair**: `pumf_label_repairs()`, `pumf_freq_validation()` (see [docs/pdf-crosscheck.md](docs/pdf-crosscheck.md)).
- **Registry and catalogue**: `pumf_registry()`, `list_pumf_registry()`, `pumf_registry_entry()`, `list_canpumf_collection()`, `list_statcan_pumf_catalogue()`, `list_available_lfs_pumf_versions()`.
- **Borealis**: `get_pumf(..., borealis = <doi or catalogue row>)`, `list_borealis_pumf_catalogue()`, `list_borealis_pumf_files()` (`R/borealis.R`; see [docs/registry.md](docs/registry.md#borealis-dataverse-source)).
- **Cache**: `list_pumf_cache()`, `remove_pumf_cache()`. **LFS helpers**: `add_lfs_SURVDATE()`, `add_lfs_GENDER_SEX()`.

### Connection provenance registry (`R/api.R`)

`get_pumf()` registers `(series, version, cache_path, lang, module)` in a package-level environment. The key is the DuckDB connection's C++ external-pointer address, which stays stable across R copies of the S4 wrapper. Attributes set directly on the wrapper would be silently lost to copy-on-modify. `label_pumf_columns()` reads the provenance via `.pumf_lookup_con()`, and `close_pumf()` removes it.

### Connection locking invariant (issue #18)

**The read path must never open a write connection.** DuckDB allows many concurrent read-only connections across processes but only one exclusive writer. A spurious `read_only = FALSE` open therefore breaks things like rendering a notebook while the interactive session holds tbls open. Write connections exist only in two places:
- (a) inside `pumf_build_duckdb()` and the LFS write phase, while a build or refresh actually writes. They are opened after `.assert_duckdb_writable()` and closed before returning.
- (b) in `add_bootstrap_weights()` / `remove_bootstrap_weights()`. These close the input tbl, assert writability (for the actionable lock message), write, and return a fresh read-only tbl.

`get_pumf()` therefore calls `pumf_run_pipeline(read_only = read_only)` directly, **not** via `get_pumf_connection()`, which hardcodes read-write. On a cache hit, every connection is read-only from start to finish. Transient existence probes use `.duckdb_table_exists()` (read-only, `shutdown = FALSE`), so a user's open in-process instance is never shut down. Regression tests: the "read path never takes a write lock" section of `tests/testthat/test-api.R`.

## Architecture

### Three-stage pipeline (non-LFS, `R/pipeline.R`)

`pumf_run_pipeline()` chains three idempotent stages using the registry config:

1. **`pumf_locate_or_download(series, version, cache_path, refresh)`**: ensures `<cache_path>/<series>/<version>/` exists with its extracted content. It resolves the download URL via `.pumf_resolve_collection_row()` (see [docs/registry.md](docs/registry.md#download-url-resolution-stage-1)).
2. **`pumf_parse_metadata(version_dir, layout_mask, metadata_encoding, refresh)`**: detects and parses every metadata format, merges the results into the canonical CSVs in `<version_dir>/metadata/`, then runs the PDF cross-check.
3. **`pumf_build_duckdb(version_dir, series, version, lang, layout_mask, file_mask, refresh)`**: reads the data file, joins the BSW weights, applies the fixups, numeric conversion and code labels, and writes `<version_dir>/<series>_<version>.duckdb`. It returns paths; `pumf_open_duckdb()` gives a lazy tbl.

For multi-module surveys, Stages 2 and 3 run once per module into the same DuckDB file ([docs/multi-module.md](docs/multi-module.md)).

#### Data file detection

`.find_pumf_data_file(version_dir, file_mask, prefer_fwf)` picks the data file:
- **Extension pattern**: taken from `file_mask` first (`.csv` → CSV, `.txt`/`.dat` → FWF). For an unrecognised extension (the 1991 Census `.INDIV`), `ext_pat` is `NULL`, and `file_mask` alone selects the file.
- **FWF decision**: based on the extension of the file actually found and on whether `layout.csv` is present. This handles CHS, which ships both CSV and TXT but whose SPSS DATA LIST also creates a `layout.csv`.
- **`.sas7bdat` files** (PALS 2001): read with `haven::read_sas()` (`.read_sas_data()`). Character columns keep their raw codes. Numeric columns are rendered back to code strings by `.coerce_coded_to_character()` **after** the data fixups, so a renamed column is matched under its declared name.

### LFS longitudinal pipeline (`R/lfs_pipeline.R`)

LFS uses a single shared DuckDB at `<cache_path>/LFS/LFS.duckdb` that accumulates every version:
- The `lfs_eng`/`lfs_fra` tables hold labelled rows, and `lfs_versions` records what has been loaded.
- An annual version supersedes the monthly versions for the same year.
- The schema evolves with `ALTER TABLE ADD COLUMN` / `ALTER COLUMN SET DATA TYPE`.
- `get_pumf("LFS", version)` returns the shared table filtered to that year (and month); `get_pumf("LFS")` returns it unfiltered.
- For LFS, `label_pumf_columns()` merges `variables.csv` from **every** loaded version in chronological order, with the most recent label winning. The shared schema is the union of all versions, and variables like `GENDER` (~2020) are missing from older versions.

### Cache and storage

Users set `options(canpumf.cache_path = "<path>")` (typically in `.Rprofile`). Without it, data goes to `tempdir()` and lasts only for the session.

```
<cache_path>/
  pumf_catalogue.rds        # persisted StatCan catalogue scrape
  borealis_catalogue.rds    # persisted Borealis catalogue
  <series>/<version>/
    <original>.zip          # retained (Borealis: loose files + borealis_manifest.csv)
    <series>_<version>.duckdb
    metadata/
      variables.csv, codes.csv
      layout.csv            # fixed-width data only
      pdf_validation.csv, label_repairs.csv   # PDF cross-check side-cars
      <module>/             # secondary modules of multi-module surveys
  LFS/
    LFS.duckdb              # one shared database for all LFS versions
    <version>/<original>.zip, metadata/
```

### Key files

- `R/api.R`: `get_pumf()`, `label_pumf_columns()`, `close_pumf()`, `pumf_metadata()`, `pumf_module()`, bootstrap-weight functions, provenance registry
- `R/pipeline.R`: Stages 1 and 3, `pumf_run_pipeline()`, `.find_pumf_data_file()`, `.read_bsw_data()`
- `R/metadata_parsers.R`: all parsers, `detect_formats()`, `merge_metadata()`, `pumf_parse_metadata()`, `read_metadata()`/`write_metadata()`
- `R/pdf_repair.R`: the PDF cross-check and label repair
- `R/registry.R`: registry entries, lookup, aliases, `pumf_registry*()`
- `R/borealis.R`: Borealis Dataverse catalogue, file selection, download, manifest
- `R/statcan_catalogue.R`: StatCan catalogue scraper and adapter, `.pumf_resolve_collection_row()`
- `R/pumf_collection.R`: curated `list_canpumf_collection()`, `list_gss_collection()`, `list_available_lfs_pumf_versions()`
- `R/lfs_pipeline.R`, `R/lfs_helpers.R`: the LFS pipeline and the `add_lfs_*()` helpers
- `R/cache_mgmt.R`: `list_pumf_cache()`, `remove_pumf_cache()`
- `R/pumf.R`: `read_pumf_data()`, `get_pumf_connection()`
- `R/pumf_documentation.R`: `open_pumf_documentation()`
- `R/helpers.R`: `robust_unzip()`, import declarations
- `tools/verify_overrides.R`, `tools/refresh_catalogue_snapshot.R`: dev-only scripts (`.Rbuildignore`d)
