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
| [docs/longitudinal.md](docs/longitudinal.md) | Longitudinal series engine and spec (LFS, LFS_HIST): shared DuckDB per series, LFS_HIST Borealis source and canonical dictionary, the harmonised `get_lfs_timeline()` |
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
- **`pumf_dictionary(x, version, module, cache_path)`** and **`pumf_translate(x, to, dict, custom, warn)`** (`R/translate.R`): the dictionary is one tibble (`name`, `val`, `label_en`, `label_fr`; `val` `NA` = variable label, `name` `NA` = applies to every column, used for registry `sentinel_labels`) from a tbl's provenance or a series/version; longitudinal series go through the spec's `codes()` accessor, LFS_TIMELINE through the shipped reference. `pumf_translate()` works on collected data frames only: it resolves each column to a variable (coded name, source-language variable label, or `<VAR>_sentinel`), remaps factor levels via `levels<-` (duplicated targets merge), character values of labelled variables, and labelled column names; `custom` (named vector in the direction of the translation, or a data frame) wins over the dictionary and also renames matching columns; untranslated levels are kept and warned about once; the attribute `pumf_translation` is the ledger. `.pumf_var_label_map()` is the `" (NAME)"` disambiguation shared with `label_pumf_columns()`, and `.lfs_derived_var_labels` holds the bilingual labels of `SURVDATE`/`GENDER_SEX`.
- **`pumf_topcodes(x, version, module, cache_path)`** (`R/translate.R`): the labelled values a table keeps as numbers (top codes such as GSS 17 `WKWEHR` 75 "75 and more", bottom codes, labelled zeros), one row per value with `name`, `val`, `label_en`, `label_fr`. It reads the `applied_as` column of `metadata/codes_applied.csv` (`"level"` of a factor column, `"value"` kept as a number, `"sentinel"` blanked to NA; computed by `.pumf_codes_applied_as()` in Stage 3, which runs the documented codes of a numeric column through the same `.apply_numeric_conversion()` as the data) and errors for the longitudinal series and for a side-car without the column (advises `refresh = TRUE`). `.pumf_prov_from_arg()` and `.pumf_prov_meta()` are the argument and metadata-directory resolution it shares with `pumf_dictionary()`.
- **`close_pumf(x)`**: `x` is a lazy tbl (closes `x$src$con`) or a DuckDB connection (detected via `inherits(x, "DBIConnection")`). Registry cleanup is guarded with `exists()`, because `get_pumf_connection()` never registers. Only needed before writing to the same file from another tbl.
- **`pumf_metadata()`**: runs Stages 1+2 and returns `list(variables, codes, layout)`. **`open_pumf_documentation()`**: opens cached PDF/TXT docs.
- **`get_pumf_connection()`** (exported, in `R/pumf.R`): returns a **read-write** DuckDB connection and is not registered. **`read_pumf_data()`**: covers the case where the user deposits files manually.
- **`pumf_sentinels(tbl, join = FALSE)`** (`R/api.R`): returns the sentinel companion table `pumf_sentinels_<table>` (one row per record with a sentinel, one ENUM column per affected variable holding the sentinel's label, `NA` where the main table has a value), or with `join = TRUE` the tbl left-joined on `pumf_row_id` with `_sentinel` suffixes. Errors for longitudinal series and for caches built before 0.6.1. Labels resolve in `.label_sentinel_companion()`: codes.csv label, then registry `sentinel_labels` (per variable, then per code), then the digits.
- **`pumf_row_id`**: every Stage 3 table starts with this permanent BIGINT key (1-based record order). It is the join key for the sentinel companion and for bootstrap weights when the registry has no `bsw_join_key`.
- **Bootstrap weights** (`R/api.R`): `add_bootstrap_weights(tbl, weight_col, ...)` works on DuckDB-backed or in-memory tbls. `remove_bootstrap_weights()` drops the BSW table and its companion view. `bsw_info()` summarises the BSW tables present.
- **`get_lfs_timeline(lang, sources)`** (`R/lfs_timeline.R`): one lazy tbl over LFS_HIST + LFS with a curated common schema. It opens an in-memory DuckDB, ATTACHes both files `READ_ONLY` and builds a `UNION ALL BY NAME` view. Provenance series `"LFS_TIMELINE"` makes `label_pumf_columns()` work.
- **Label repair**: `pumf_label_repairs()`, `pumf_freq_validation()` (see [docs/pdf-crosscheck.md](docs/pdf-crosscheck.md)).
- **Registry and catalogue**: `pumf_registry()`, `list_pumf_registry()`, `pumf_registry_entry()`, `list_canpumf_collection()`, `list_statcan_pumf_catalogue()`, `list_available_lfs_pumf_versions()`.
- **Borealis**: `get_pumf(..., borealis = <doi or catalogue row>)`, `list_borealis_pumf_catalogue()`, `list_borealis_pumf_files()` (`R/borealis.R`; see [docs/registry.md](docs/registry.md#borealis-dataverse-source)).
- **Cache**: `list_pumf_cache()` (column `built_with` from the build stamp), `remove_pumf_cache()`. **LFS helpers**: `add_lfs_SURVDATE()`, `add_lfs_GENDER_SEX()`.
- **Build stamp** (`R/pipeline.R`): Stage 3 writes `pumf_build_info` (one row per table: `canpumf_version`, `duckdb_version`, `built`) via `.write_build_info()`; `.read_build_info(con, table)` reads it. A table without a row predates 0.6.1 (no `pumf_row_id`, no companion). `get_pumf()` reports that once per session and table through `.pumf_check_build_stamp()` (a `message()`, silenced by `options(canpumf.stale_cache_message = FALSE)`); the same check messages once for a stamped table whose `metadata/codes_applied.csv` is missing (a 0.6.1 development build from before the data-based value-label rule), and `remove_bootstrap_weights()` drops `pumf_row_id` only from an unstamped table. Longitudinal series are not stamped.

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
3. **`pumf_build_duckdb(version_dir, series, version, lang, layout_mask, file_mask, refresh)`**: reads the data file, joins the BSW weights, applies the fixups, numeric conversion and code labels, prepends `pumf_row_id`, and writes `<version_dir>/<series>_<version>.duckdb` plus the `pumf_sentinels_<table>` companion. `.apply_numeric_conversion()` and `.apply_code_labels()` return the values they blanked in a `pumf_sentinels` attribute; `.sentinel_companion()` and `.label_sentinel_companion()` turn them into the companion. It returns paths; `pumf_open_duckdb()` gives a lazy tbl. **Value labels are unique per variable and language, decided on the data**: `.pumf_unique_code_labels(codes, present)` (in `R/pipeline.R`) appends the code to every member of a group of distinct codes sharing a label (`"Other (3)"`, `"Other (6)"`) when at least two of the group's codes occur in the data (`present`, from `.pumf_codes_present(data, codes, na_values)`; `present = NULL` is the document-based rule, `list()` suffixes nothing), after the fr→en fallback and ignoring NA/empty labels, with `"01"`/`"1"` counted as one code. `.apply_code_labels()` computes `present` from its data; `.label_sentinel_companion()` dedupes only the sentinel values that occur (via `.pumf_dedupe_labels()`); Stage 3 writes the labels it applied for every coded variable to `metadata/codes_applied.csv` (`.write_codes_applied()`), and `.pumf_dictionary_from_prov()` reads that file (`.read_codes_applied()`, falling back to the documented labels for a pre-0.6.1 cache) so the dictionary matches the ENUM levels. The longitudinal `codes()` accessors and `.lfs_timeline_label_map()` apply the document-based rule, which is equivalent there (no LFS/LFS_HIST variable has a shared label). **Registry code rows** (`codes_supplement` appends, `codes_override` replaces a declared code's labels) are applied by `.pumf_apply_code_fixups(codes, fx)` before all of this, in Stage 3 and in the dictionary fallback. Caches built before 0.6.1 keep merged levels until `refresh = TRUE`.

For multi-module surveys, Stages 2 and 3 run once per module into the same DuckDB file ([docs/multi-module.md](docs/multi-module.md)).

#### Data file detection

`.find_pumf_data_file(version_dir, file_mask, prefer_fwf)` picks the data file:
- **Extension pattern**: taken from `file_mask` first (`.csv` → CSV, `.txt`/`.dat` → FWF). For an unrecognised extension (the 1991 Census `.INDIV`), `ext_pat` is `NULL`, and `file_mask` alone selects the file.
- **FWF decision**: based on the extension of the file actually found and on whether `layout.csv` is present. This handles CHS, which ships both CSV and TXT but whose SPSS DATA LIST also creates a `layout.csv`.
- **`.sas7bdat` files** (PALS 2001): read with `haven::read_sas()` (`.read_sas_data()`). Character columns keep their raw codes. Numeric columns are rendered back to code strings by `.coerce_coded_to_character()` **after** the data fixups, so a renamed column is matched under its declared name.

### Longitudinal series (`R/longitudinal.R`)

Series whose time slices share (nearly) the same variables are appended to one DuckDB per series by the generic engine `.long_get_pumf(spec, ...)`. Each series supplies a spec (`.pumf_longitudinal_spec()`) with its DB/table names, version validation, `prepare`/`build` steps and label lookup. Code that must treat these series specially tests `.is_longitudinal(series)`, never `series == "LFS"`. The instances are LFS (below) and **LFS_HIST** (1976–2005 monthly LFS from Borealis, `R/lfs_hist.R`, with a canonical bilingual dictionary shipped in `inst/extdata/lfs_hist/`). See [docs/longitudinal.md](docs/longitudinal.md).

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
    <series>_<version>.duckdb   # tables eng/fra (+ pumf_sentinels_eng/fra companions, pumf_bsw_* weights, pumf_build_info stamp)
    metadata/
      variables.csv, codes.csv
      codes_applied.csv     # value labels as applied by Stage 3 (fixups, fr fallback, code suffix) + applied_as
      layout.csv            # fixed-width data only
      pdf_validation.csv, label_repairs.csv   # PDF cross-check side-cars
      <module>/             # secondary modules of multi-module surveys
  LFS/
    LFS.duckdb              # one shared database for all LFS versions
    <version>/<original>.zip, metadata/
  LFS_HIST/
    LFS_HIST.duckdb         # one shared database for 1976-2005 months
    YYYY-MM/                # Borealis files, fra/*.sas, borealis_manifest.csv, metadata/
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
- `R/longitudinal.R`: the longitudinal engine and spec registry
- `R/lfs_pipeline.R`, `R/lfs_helpers.R`: the LFS spec, append helpers and the `add_lfs_*()` helpers
- `R/lfs_hist.R`: the LFS_HIST spec (Borealis download, canonical dictionary)
- `R/lfs_timeline.R`: `get_lfs_timeline()` and its harmonisation tables (`inst/extdata/lfs_timeline/`)
- `R/cache_mgmt.R`: `list_pumf_cache()`, `remove_pumf_cache()`
- `R/pumf.R`: `read_pumf_data()`, `get_pumf_connection()`
- `R/pumf_documentation.R`: `open_pumf_documentation()`
- `R/helpers.R`: `robust_unzip()`, import declarations
- `tools/verify_overrides.R`, `tools/refresh_catalogue_snapshot.R`, `tools/build_lfs_hist_reference.R`, `tools/build_lfs_timeline_reference.R`: dev-only scripts (`.Rbuildignore`d)
