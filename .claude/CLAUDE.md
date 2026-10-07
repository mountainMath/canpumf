# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

`canpumf` is an R package for loading Statistics Canada Public Use Microdata Files (PUMF) into R. It downloads the files (from StatCan, or from the Borealis Dataverse for vintages StatCan does not post), parses the metadata, applies bilingual labels, and returns lazy DuckDB-backed tables so the data can be used without loading it into memory.

## Topic docs (read before changing that area)

| File | Covers |
|---|---|
| [docs/metadata-parsers.md](docs/metadata-parsers.md) | The ten parsers, sentinel/missing detection, SPSS/SAS/PDF parsing quirks, encodings, mojibake repair |
| [docs/pdf-crosscheck.md](docs/pdf-crosscheck.md) | User-guide PDF parser, frequency validation, truncation fingerprints, label repair (`R/pdf_repair.R`) |
| [docs/registry.md](docs/registry.md) | Registry fields and `data_fixups`, the DuckDB-native CSV build (`csv_reader`), sibling inheritance, version aliases (incl. EFT-vs-Borealis Census resolution), download-URL resolution, the Borealis source, **override verification workflow** |
| [docs/longitudinal.md](docs/longitudinal.md) | Longitudinal series engine and spec (LFS, LFS_HIST): shared DuckDB per series, LFS_HIST Borealis source and canonical dictionary, the harmonised `get_pumf("LFS_TIMELINE")` |
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

Twenty functions, consolidated in 0.7.0 (keep it that way: a variant of an existing function becomes an argument, not a new export). The 0.6.0 names that were folded in are `.Deprecated()` shims in `R/deprecated.R`, documented together in `?"canpumf-deprecated"` (`@keywords internal`); `get_pumf_connection()` lives there too.

- **`get_pumf(series, version, lang="eng", ...)`**: the main entry point. It runs the pipeline and returns a lazy `dplyr::tbl()` whose categorical values are pre-labelled factors. Options include `module =` and `registry =`. LFS is delegated to `lfs_get_pumf()`; `series = "LFS_TIMELINE"` goes to `.get_pumf_timeline()` (below). The function registers connection provenance (below).
- **`label_pumf_columns(tbl)`**: renames coded column names (`PHHSIZE`) to variable labels ("Household size"). It works after dplyr verbs and is module-aware. `pumf_dictionary(tbl, what = "variables")` returns the label lookup itself (the description pair is the optional longer text of `variables.csv`, filled for CCRI by the `labels_as_description` fixup and `NA` elsewhere, see [docs/metadata-parsers.md](docs/metadata-parsers.md)).
- **`pumf_dictionary(x, version, module, cache_path, what)`** and **`pumf_translate(x, to, dict, custom, warn)`** (`R/translate.R`): the dictionary is one tibble (`name`, `val`, `label_en`, `label_fr`, `description_en`, `description_fr`, `applied_as`; `val` `NA` = variable label, `name` `NA` = applies to every column, used for registry `sentinel_labels`; `what = "variables"`/`"values"` keep the variable or the value rows) from a tbl's provenance or a series/version; longitudinal series go through the spec's `codes()` accessor, LFS_TIMELINE through the shipped reference. `pumf_translate()` works on collected data frames only: it resolves each column to a variable (coded name, source-language variable label, or `<VAR>_sentinel`), remaps factor levels via `levels<-` (duplicated targets merge), character values of labelled variables, and labelled column names; `custom` (named vector in the direction of the translation, or a data frame) wins over the dictionary and also renames matching columns; untranslated levels are kept and warned about once; the attribute `pumf_translation` is the ledger. `.pumf_var_label_map()` is the `" (NAME)"` disambiguation shared with `label_pumf_columns()`, and `.lfs_derived_var_labels` holds the bilingual labels of `SURVDATE`/`GENDER_SEX`.
- **`pumf_dictionary(x, what = "topcodes")`** (`.pumf_dictionary_topcodes()`): the labelled values a table keeps as numbers (top codes such as GSS 17 `WKWEHR` 75 "75 and more", bottom codes, labelled zeros), the dictionary rows with `applied_as == "value"` (`val` stays character). It reads the `applied_as` column of `metadata/codes_applied.csv` (`"level"` of a factor column, `"value"` kept as a number, `"sentinel"` blanked to NA; computed by `.pumf_codes_applied_as()` in Stage 3, which runs the documented codes of a numeric column through the same `.apply_numeric_conversion()` as the data) and errors for the longitudinal series and for a side-car without the column (advises `refresh = TRUE`). `.pumf_prov_from_arg()` and `.pumf_prov_meta()` are the argument and metadata-directory resolution of `pumf_dictionary()`; a version given with a series name goes through `pumf_resolve_version()`, as it does in `open_pumf_documentation()` (`remove_pumf_cache()` deliberately takes the exact key only).
- **`close_pumf(x)`**: `x` is a lazy tbl (closes `x$src$con`) or a DuckDB connection (detected via `inherits(x, "DBIConnection")`). Registry cleanup is guarded with `exists()`, because the deprecated `get_pumf_connection()` never registers. Only needed before writing to the same file from another tbl.
- **`pumf_metadata()`**: runs Stages 1+2 and returns `list(variables, codes, layout)`. **`open_pumf_documentation()`**: opens cached PDF/TXT docs.
- **`get_pumf_connection()`** (deprecated, in `R/deprecated.R`): returns a **read-write** DuckDB connection and is not registered; `get_pumf(read_only = FALSE)` + `dbplyr::remote_con()` replaces it. **`read_pumf_data()`** (internal): covers the case where the user deposits files manually.
- **Sidecar tables: `pumf_sidecar(tbl, sidecar = NULL, join = FALSE)`** (`R/api.R`): the one accessor for the tables Stage 3 writes beside a survey table. The spec list `.pumf_sidecars` drives both: each entry has `table(table_name)`, a `kind`, a `description` and the `absent` reason for the error. `"sentinels"` (kind `"values"`, table `pumf_sentinels_<table>`): one row per record with a sentinel, one ENUM column per affected variable holding the sentinel's label, `NA` where the main table has a value; `join = TRUE` left-joins it on `pumf_row_id` with `_sentinel` suffixes. Labels resolve in `.label_sentinel_companion()`: codes.csv label, then registry `sentinel_labels` (per variable, then per code), then the digits. `"removed"` (kind `"records"`, table `pumf_removed_<table>`): the records a registry `removed_records` fixup sets aside (TCP 1881), with the columns of the main table; `join = TRUE` appends them with `union_all` and needs the tbl to still have the survey table's columns. Without `sidecar`, `pumf_sidecar(tbl)` lists them (`.pumf_sidecar_list()`): `sidecar`, `table`, `kind`, `n_rows`, `description` for those present. Both error or return nothing for the longitudinal series; the sentinel sidecar is missing from caches built before 0.7.0. A new kind of side table becomes a `.pumf_sidecars` entry, not a new exported function (`.pumf_sidecar_tables()` is what `remove_pumf_cache(lang = )` drops). Bootstrap weights are deliberately their own family (below), and `pumf_pdf_crosscheck()` is a metadata report, not a record table.
- **`pumf_row_id`**: every Stage 3 table starts with this permanent BIGINT key (1-based record order). It is the join key for the sentinel sidecar and for bootstrap weights when the registry has no `bsw_join_key`.
- **Bootstrap weights** (`R/api.R`): `add_bootstrap_weights(tbl, weight_col, ...)` works on DuckDB-backed or in-memory tbls. On a lazy tbl everything happens on the tbl's own connection, which is never closed or reopened: a write connection (`get_pumf(..., read_only = FALSE)`, detected by `.bsw_con_writable()`) stores the weights in `pumf_bsw_<weight>[_<module>]`; a read-only connection reuses stored weights when they cover the request and otherwise generates into the TEMP table `tmp_pumf_bsw_<weight>` (message; warning above about 2 GB), copying stored replicates into it when extending. The result is the input tbl `inner_join`ed with the weights table, so earlier dplyr verbs and labels are kept; the key column(s) must still be in the tbl (coded name or label, `.bsw_key_in_tbl()`). Key: explicit `id_col` (may be several columns) > longitudinal `SURVYEAR`/`SURVMNTH`/`REC_NUM` > registry or module `bsw_join_key` > `pumf_row_id`; the survey table is never altered, and a pre-0.7.0 table without a key needs `refresh = TRUE` or an `id_col`. A weights table holds the replicates of one `prefix`. `.bsw_locate()` resolves connection, provenance, module table and registry key/strata (errors for a closed connection and for `LFS_TIMELINE`); `.bsw_state()`, `.bsw_n_missing()` and `.bsw_generate()` do the incremental logic. `remove_bootstrap_weights()` drops the temporary tables on any connection and the stored ones (plus the `<table>_bsw_<weight>` views of 0.6.0) on a write connection only. `bsw_info()` returns one row per set of replicates (`source`, `weight_col`, `prefix`, `bsw_table`, `temporary`, `n_replicates`, `size_mb`): `source = "generated"` for the stored and temporary weights tables, `source = "survey"` for the replicate weights that came with the survey. Those are columns of the survey table (joined from the registry's `bsw_file_mask` file in Stage 3, or shipped in the data file) and nothing records which; `.bsw_survey_families()` recognises a family of numeric columns `<prefix>1..n` without gaps that is absent from `variables.csv`, or labelled bootstrap/replicate, or entirely unlabelled (layout-promoted). It returns nothing for the longitudinal series.
- **`get_pumf("LFS_TIMELINE", lang, sources =, refresh =)`** (`R/lfs_timeline.R`, `.lfs_timeline_open()`, documented as the topic `?lfs_timeline`; `.get_pumf_timeline()` rejects `version`, `module`, `registry`, `borealis`, `redownload` and `read_only = FALSE`): one lazy tbl over LFS_HIST + LFS with a curated common schema. It opens an in-memory DuckDB, ATTACHes both files `READ_ONLY` and builds a `UNION ALL BY NAME` view. Provenance series `"LFS_TIMELINE"` makes `label_pumf_columns()` work.
- **Label repair**: `pumf_pdf_crosscheck(tbl, report = c("repairs", "validation"), action)` (see [docs/pdf-crosscheck.md](docs/pdf-crosscheck.md)).
- **Registry and catalogue**: `pumf_registry(series, version)` (one entry, or without `version` the overview, optionally of one series), `pumf_registry_entry()`, `list_pumf_catalogue(source = c("canpumf", "statcan", "borealis", "lfs"))` (`R/pumf_collection.R`; dispatches to the internal `.canpumf_collection()`, `.statcan_pumf_catalogue()`, `.borealis_pumf_catalogue()`, `.lfs_pumf_versions()`; `...` goes to the statcan crawler only).
- **Borealis**: `get_pumf(..., borealis = <doi or catalogue row>)`, `list_pumf_catalogue("borealis")`, `list_borealis_pumf_files()` (`R/borealis.R`; see [docs/registry.md](docs/registry.md#borealis-dataverse-source)).
- **Cache**: `list_pumf_cache()` (column `built_with` from the build stamp), `remove_pumf_cache()`; its `lang = "eng"|"fra"` (non-longitudinal only, `.remove_pumf_lang()`) drops that language's main table(s), sidecar tables, `_bsw_` views and stamp rows, keeps the shared `pumf_bsw_*` tables, and compacts the file with `COPY FROM DATABASE` into a `.compact` copy swapped into place (DuckDB never truncates on DROP); the last language deletes the file. It takes the write lock. **LFS helpers**: `add_lfs_columns(tbl, columns = c("SURVDATE", "GENDER_SEX"))`.
- **Build stamp** (`R/pipeline.R`): Stage 3 writes `pumf_build_info` (one row per table: `canpumf_version`, `duckdb_version`, `built`) via `.write_build_info()`; `.read_build_info(con, table)` reads it. A table without a row predates 0.7.0 (no `pumf_row_id`, no sentinel sidecar). `get_pumf()` reports that once per session and table through `.pumf_check_build_stamp()` (a `message()`, silenced by `options(canpumf.stale_cache_message = FALSE)`); the same check messages once for a stamped table whose `metadata/codes_applied.csv` is missing (a 0.7.0 development build from before the data-based value-label rule). Longitudinal series are not stamped.

### Connection provenance registry (`R/api.R`)

`get_pumf()` registers `(series, version, cache_path, lang, module)` in a package-level environment. The key is the DuckDB connection's C++ external-pointer address, which stays stable across R copies of the S4 wrapper. Attributes set directly on the wrapper would be silently lost to copy-on-modify. `label_pumf_columns()` reads the provenance via `.pumf_lookup_con()`, and `close_pumf()` removes it.

### Connection locking invariant (issue #18)

**The read path must never open a write connection.** DuckDB allows many concurrent read-only connections across processes but only one exclusive writer. A spurious `read_only = FALSE` open therefore breaks things like rendering a notebook while the interactive session holds tbls open. The package opens a write connection only in two places:
- (a) inside `pumf_build_duckdb()` and the LFS write phase, while a build or refresh actually writes. They are opened after `.assert_duckdb_writable()` and closed before returning.
- (b) in `remove_pumf_cache(lang = )`, which drops one language's tables and compacts the file.

A write connection the user asks for (`get_pumf(read_only = FALSE)`, the deprecated `get_pumf_connection()`) is theirs to hold. The bootstrap-weight functions never open a connection: they write through the tbl's own connection, to the database when it is a write connection and to a temporary table when it is read-only.

**Every file connection goes through `.duckdb_connect()`** (`R/helpers.R`; `.duckdb_connect_quiet()` is the variant kept out of the RStudio Connections pane), never a bare `DBI::dbConnect(duckdb::duckdb(), dbdir = ...)`. duckdb keeps one instance per file and process, and from 1.5.6 `dbConnect()` fails when `read_only` differs from that instance instead of ignoring the argument. The helper restores the behaviour the rest of the package assumes: a read-only open shares a read-write instance the session already holds (no lock is taken), and a read-write open against a held read-only instance raises the classed `canpumf_read_only_held` error with the `close_pumf()` message (`.stop_duckdb_read_only_held()`), which `.assert_duckdb_writable()` passes through.

`get_pumf()` therefore calls `pumf_run_pipeline(read_only = read_only)` directly, **not** via the deprecated `get_pumf_connection()`, which hardcodes read-write. On a cache hit, every connection is read-only from start to finish. Transient existence probes use `.duckdb_table_exists()` (read-only, `shutdown = FALSE`), so a user's open in-process instance is never shut down. Regression tests: the "read path never takes a write lock" section of `tests/testthat/test-api.R`.

## Architecture

### Three-stage pipeline (non-LFS, `R/pipeline.R`)

`pumf_run_pipeline()` chains three idempotent stages using the registry config:

1. **`pumf_locate_or_download(series, version, cache_path, refresh)`**: ensures `<cache_path>/<series>/<version>/` exists with its extracted content. It resolves the download URL via `.pumf_resolve_collection_row()` (see [docs/registry.md](docs/registry.md#download-url-resolution-stage-1)).
2. **`pumf_parse_metadata(version_dir, layout_mask, metadata_encoding, refresh, meta_subdir, file_mask, layout_file)`**: detects and parses every metadata format, merges the results into the canonical CSVs in `<version_dir>/metadata/` (layout-only columns read with implied decimals, such as replicate weights, become unlabelled numeric variables via `.promote_layout_numeric()`; identifiers without decimals stay out and character; a registry `layout_file` replaces the merged layout with the named reading card's, for releases whose cards disagree), then runs the PDF cross-check.
3. **`pumf_build_duckdb(version_dir, series, version, lang, layout_mask, file_mask, refresh)`**: reads the data file, joins the BSW weights, applies the fixups, numeric conversion and code labels, prepends `pumf_row_id`, and writes `<version_dir>/<series>_<version>.duckdb` plus the `pumf_sentinels_<table>` sidecar (and `pumf_removed_<table>` when the registry sets records aside). `.apply_numeric_conversion()` and `.apply_code_labels()` return the values they blanked in a `pumf_sentinels` attribute; `.sentinel_companion()` and `.label_sentinel_companion()` turn them into the companion. It returns paths; `pumf_open_duckdb()` gives a lazy tbl. **Value labels are unique per variable and language, decided on the data**: `.pumf_unique_code_labels(codes, present)` (in `R/pipeline.R`) appends the code to every member of a group of distinct codes sharing a label (`"Other (3)"`, `"Other (6)"`) when at least two of the group's codes occur in the data (`present`, from `.pumf_codes_present(data, codes, na_values)`; `present = NULL` is the document-based rule, `list()` suffixes nothing), after the fr→en fallback and ignoring NA/empty labels, with `"01"`/`"1"` counted as one code. `.apply_code_labels()` computes `present` from its data; `.label_sentinel_companion()` dedupes only the sentinel values that occur (via `.pumf_dedupe_labels()`); Stage 3 writes the labels it applied for every coded variable to `metadata/codes_applied.csv` (`.write_codes_applied()`), and `.pumf_dictionary_from_prov()` reads that file (`.read_codes_applied()`, falling back to the documented labels for a pre-0.7.0 cache) so the dictionary matches the ENUM levels. The longitudinal `codes()` accessors and `.lfs_timeline_label_map()` apply the document-based rule, which is equivalent there (no LFS/LFS_HIST variable has a shared label). **Registry code rows** (`codes_supplement` appends, `codes_override` replaces a declared code's labels) are applied by `.pumf_apply_code_fixups(codes, fx)` before all of this, in Stage 3 and in the dictionary fallback. Caches built before 0.7.0 keep merged levels until `refresh = TRUE`.

For multi-module surveys, Stages 2 and 3 run once per module into the same DuckDB file ([docs/multi-module.md](docs/multi-module.md)).

#### Data file detection

`.find_pumf_data_file(version_dir, file_mask, prefer_fwf)` picks the data file:
- **Extension pattern**: taken from `file_mask` first (`.csv` → CSV, `.txt`/`.dat` → FWF). For an unrecognised extension (the 1991 Census `.INDIV`), `ext_pat` is `NULL`, and `file_mask` alone selects the file.
- **FWF decision**: based on the extension of the file actually found and on whether `layout.csv` is present. This handles CHS, which ships both CSV and TXT but whose SPSS DATA LIST also creates a `layout.csv`.
- **`.sas7bdat` files** (PALS 2001): read with `haven::read_sas()` (`.read_sas_data()`). Character columns keep their raw codes. Numeric columns are rendered back to code strings by `.coerce_coded_to_character()` **after** the data fixups, so a renamed column is matched under its declared name.

#### DuckDB-native CSV build (`csv_reader = "duckdb"`)

For a CSV too large to hold in R (TCP 1881, 4.3 million records), Stage 3 keeps the records in DuckDB: `.pumf_native_scan()` stages the file in the TEMP table `pumf_csv_stage` and returns the **distinct-values frame** of the numeric and coded columns, the unchanged Step 7 helpers run on that frame, and `.pumf_native_write()` joins the resulting per-column maps back onto the records. **A Stage 3 rule must therefore stay a function of a column's set of values**; a rule that looks across columns or at row order needs its own SQL in `.pumf_native_write()`. The record-level CSV repairs (`rejoin_split_records`, `column_encoding`) and `text_missing_codes` exist only on the readr path (`.pumf_read_csv_repaired()`, CCRI 1911); the native reader refuses them. The build runs under `options(canpumf.native_memory_limit = "1GB")` with capped threads (`.pumf_native_limits()`) and falls back to readr when the frame would be large (`canpumf.native_max_cells`). See [docs/registry.md](docs/registry.md#duckdb-native-csv-build).

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
    <original>.zip          # retained (Borealis: loose files + borealis_manifest.csv; a CSV data file is kept as <name>.csv.gz)
    <series>_<version>.duckdb   # tables eng/fra (+ sidecars pumf_sentinels_eng/fra and pumf_removed_eng/fra, pumf_bsw_* weights, pumf_build_info stamp)
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
- `R/borealis.R`: Borealis Dataverse catalogue, file selection, download, manifest (`.borealis_fetch_selected()` downloads a selection, extracts zips and writes the manifest; shared with LFS_HIST). **Downloaded raw data stays compressed**: a CSV data file is fetched as a deflated one-file bundle, `.borealis_download_csv_gz()`, stored and read as `.csv.gz`; never leave or require an uncompressed copy of a large download
- `R/statcan_catalogue.R`: StatCan catalogue scraper and adapter, `.pumf_resolve_collection_row()`, the persisted-rds helpers both catalogues use (`.pumf_rds_cache_file()`, `.pumf_rds_read()`, `.pumf_rds_write()`, `.pumf_warn_if_stale()`), `.statcan_abs_url()`
- `R/pumf_collection.R`: `list_pumf_catalogue()`, the curated `.canpumf_collection()`, `list_gss_collection()`, `.lfs_pumf_versions()` (both LFS lists scrape through `.lfs_scrape_csv_links()`)
- `R/longitudinal.R`: the longitudinal engine, spec registry, the `(con, spec)` versions-table helpers, `.long_append()` and `.pumf_extdata_csv()`
- `R/lfs_pipeline.R`, `R/lfs_helpers.R`: the LFS spec (`.lfs_build_version()`, `.lfs_merged_metadata()`) and `add_lfs_columns()`
- `R/lfs_hist.R`: the LFS_HIST spec (Borealis download, canonical dictionary)
- `R/lfs_timeline.R`: `.lfs_timeline_open()` (behind `get_pumf("LFS_TIMELINE")`) and its harmonisation tables (`inst/extdata/lfs_timeline/`)
- `R/cache_mgmt.R`: `list_pumf_cache()`, `remove_pumf_cache()`
- `R/pumf.R`: `read_pumf_data()`
- `R/deprecated.R`: the `.Deprecated()` shims of the 0.6.0 names folded in by 0.7.0, and `get_pumf_connection()`
- `R/pumf_documentation.R`: `open_pumf_documentation()`
- `R/helpers.R`: `robust_unzip()`, `.duckdb_connect()`, the compressed-data helpers (`.gzip_file()`, `.zip_entry_to_gzip()`, `.is_csv_path()`, `.pumf_data_file_size()`), import declarations
- `tools/verify_overrides.R`, `tools/refresh_catalogue_snapshot.R`, `tools/build_lfs_hist_reference.R`, `tools/build_lfs_timeline_reference.R`: dev-only scripts (`.Rbuildignore`d)
