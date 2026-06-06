# canpumf DuckDB refactoring — work plan

## Decisions

| # | Decision | Choice |
|---|---|---|
| 1 | Multi-file surveys (BSW, etc.) | Join bootstrap weight files into main table before writing DuckDB |
| 2 | Labeled vs raw storage | Always apply labels; only labeled table written to DuckDB |
| 3 | Return type of `get_pumf()` | duckplyr lazy table (breaking change; users call `collect()` if they want a tibble) |
| 4 | Zip filename retention | Retain original filename; locate by "any `.zip` in the version dir" |
| 5 | Multi-format metadata | Parse ALL command files found (SPSS, SAS, CSV); merge results to maximise coverage |
| 6 | LFS database strategy | Single shared DuckDB + table across all LFS versions; monthly data superseded by annual when available |
| 7 | Categorical storage | R factors with complete levels → DuckDB ENUM columns; NA values stored as NULL |
| 8 | Test suite | `testthat` with golden-file fixtures per parser; integration tests for all downloadable surveys; property tests for factor/ENUM conversion |

---

## Overview

Three clean stages run in order whenever a connection to PUMF data is requested:

1. **Locate / download** — find or fetch the zip, extract alongside it.
2. **Parse metadata** — parse every command file format found; merge into canonical CSV.
3. **Build DuckDB** — read data, join BSW, apply labels, write DuckDB; return lazy table.

Each stage is idempotent. `refresh=TRUE` forces a stage to re-run. LFS is the primary exception to this pattern and is covered in Phase 7.

---

## Phase 1 — Canonical metadata format

All parsers converge on three CSV files written to `metadata/` inside the version directory.

### `metadata/variables.csv`

One row per variable.

| column | type | description |
|---|---|---|
| `name` | character | variable name (always uppercase) |
| `label` | character | English human-readable variable label |
| `type` | character | `"character"` or `"numeric"` |
| `missing_low` | numeric or NA | low end of missing-value range |
| `missing_high` | numeric or NA | high end of missing-value range |

`type` is `"numeric"` when the variable has a missing range and no code labels, `"character"` when it has code labels. Ambiguous cases (missing range AND code labels) resolve to `"character"`: if ANY code label exists, treat as character.

### `metadata/codes.csv`

One row per code value.

| column | type | description |
|---|---|---|
| `name` | character | variable name (uppercase) |
| `val` | character | raw code value as it appears in the data file |
| `label` | character | English label for this code |

Missing-value codes that also appear in the value labels block are included here (consistent with the current `miss_data %>% filter(!name %in% unique(val$name))` logic).

### `metadata/layout.csv`

One row per variable; **only written for fixed-width format data**. Not present for CSV-format PUMF files.

| column | type | description |
|---|---|---|
| `name` | character | variable name |
| `start` | integer | 1-based start column |
| `end` | integer | 1-based end column (inclusive) |

### Mapping from current `.Rds` files

| Old file | New file(s) |
|---|---|
| `canpumf/var.Rds` | `metadata/variables.csv` (name, label columns) |
| `canpumf/val.Rds` | `metadata/codes.csv` |
| `canpumf/miss.Rds` | `metadata/variables.csv` (missing_low, missing_high, type columns) |
| `canpumf/lay.Rds` / `canpumf/layout.Rds` | `metadata/layout.csv` |

### Factor conversion and NA handling

This subsection specifies how `codes.csv` drives factor creation in Stage 3. It is part of the canonical format definition because the level set is authoritative metadata, not a runtime detail.

**Level completeness**: factor levels are always the complete set from `codes.csv` for that variable, in the order they appear in `codes.csv` (which matches the order in the original SPSS/SAS command files — typically ascending code value). Levels are the *label* strings, not the raw code values. A code that does not appear in the actual data is still a level.

**Unmatched raw values**: data values not found in `codes.csv` (e.g. a code documented only in the user guide PDF, or a new code added after the command files were written) cannot be mapped to a label. These become `NA` after conversion. A warning is emitted listing the variable name and each unmatched value so the user can investigate.

**NA handling**: `NA` values in a converted factor — whether from unmatched raw values or explicit missing-value range substitution — are stored as `NULL` in DuckDB. They are **not** promoted to an explicit `"(NA)"` level. This keeps the ENUM definition clean and makes `IS NULL` queries in SQL work naturally.

**DuckDB ENUM storage**: R factors written via `duckdb::dbWriteTable()` with `duckdb >= 1.5.2` are stored as DuckDB `ENUM` types rather than `VARCHAR`. ENUMs are more compact (stored as integers internally) and preserve level order. The pipeline must verify this behaviour for the installed duckdb version; if the driver falls back to VARCHAR, an explicit `ALTER COLUMN ... SET DATA TYPE ENUM(...)` step is added.

**Reading ENUMs back**: when a duckplyr/DBI query collects data from a DuckDB ENUM column, it returns a character vector, not a factor. This is expected: users who need R factors call `mutate(across(where(is.character), as.factor))` or similar on the collected tibble. The lazy duckplyr table preserves ENUM semantics at the DuckDB level for efficient filtering and grouping.

---

## Phase 2 — Cache directory structure

### Standard surveys (all except LFS)

```
<cache_path>/
  <series>/
    <version>/
      <original_name>.zip          # retained; original download filename kept
      <series>_<version>.duckdb    # labeled, BSW-joined DuckDB table
      metadata/
        variables.csv
        codes.csv
        layout.csv                 # only for fixed-width data
```

Notes:
- The zip is identified as "the single `.zip` file in the version directory". No renaming.
- For EFT-only surveys (Census pre-download era), the user deposits the zip manually in `<cache_path>/Census/<version>/`. The pipeline detects this and skips download.
- For Census, version strings like `"2021 (individuals)"` map to directory `Census/2021 (individuals)/`. The product-code discovery logic in `pumf_cache_path.R` is retired.
- Multi-file surveys where a single version directory holds conceptually separate files (ITS CDN vs VIS) use `layout_mask` as a DuckDB table name within one `.duckdb` file. The default table is named `"main"`.

### LFS (exception)

The LFS uses a single shared DuckDB at the series level rather than per-version databases. Per-version subdirectories still exist to hold the zip and per-version metadata, but the DuckDB lives one level up.

```
<cache_path>/
  LFS/
    LFS.duckdb                     # single database, all versions, two tables
    2022/
      <original_2022>.zip
      metadata/
        variables.csv
        codes.csv
    2023/
      <original_2023>.zip
      metadata/
        variables.csv
        codes.csv
    2024-01/
      <original_2024-01>.zip
      metadata/
        variables.csv
        codes.csv
    ...
```

`LFS.duckdb` contains two tables:
- `lfs` — all labeled data rows across all loaded versions
- `lfs_versions` — version tracking (see Phase 7)

---

## Phase 3 — Metadata parsers

### Design principle: parse everything, merge the results

Different command file formats provided with a PUMF release may contain complementary information:
- The SPSS `*vare.sps` file may have variable labels absent from the SAS `.lbe` file.
- The SAS `.cde` file may have more complete code labels than the SPSS `*vale.sps`.
- The CSV codebook may have plain-text descriptions not encoded anywhere in the command files.

The dispatcher parses **all** formats found and merges using a priority order (most structured / most complete wins per variable). Merge strategy:
1. Variable label: use first non-NA across parsers, priority SPSS monolithic > SPSS split > SAS cards > CSV.
2. Code labels: union across parsers; where the same `(name, val)` appears in multiple sources, use the label from the highest-priority parser.
3. Missing ranges: union; flag conflicts as a warning.
4. Layout: use the source that actually defines column positions (only one format will have this per survey).

### New file: `R/metadata_parsers.R`

#### `pumf_parse_metadata(version_dir, layout_mask=NULL, refresh=FALSE)`

Public entry point. Checks for `metadata/variables.csv`; skips if present and `refresh=FALSE`. Calls the dispatcher, writes canonical CSV files, returns the metadata dir path invisibly.

#### `detect_formats(pumf_dir)` → named list of format → path(s)

Walks the extracted PUMF directory tree and returns all parseable command files:

```
list(
  spss_mono  = c("path/to/file.sps", ...),
  spss_split = c("path/to/dir/", ...),
  sas_cards  = c("path/to/dir/", ...),
  lfs_csv    = "path/to/codebook.csv",
  cpss_csv   = "path/to/variables.csv"
)
```

Detection rules (applied to the full recursive file tree):
1. `codebook.csv` → `lfs_csv`
2. `variables.csv` → `cpss_csv`
3. Directory containing `.lay` + `.lbe` files → `sas_cards`
4. Directory where multiple `.sps` files exist with names matching `vare|vale|miss|_i` → `spss_split`
5. Single `.sps` file containing both `VARIABLE LABELS` and `VALUE LABELS` keywords → `spss_mono`

#### `parse_spss_mono(sps_path, encoding="Latin1")` → list(variables, codes, layout)

Subsumes and generalises `ensure_2021_pumfi_metadata()` and `ensure_2016_pumf_metadata()`.

Section landmarks:
- `DATA LIST FILE` → layout (start/end positions)
- First `VARIABLE LABELS` block → variable name → label
- `VALUE LABELS` blocks (slash-delimited variable groups) → code labels
- `MISSING VALUES` block → missing ranges
- `FORMATS` block → type hints (A = character, F/N = numeric)

Quirks to handle robustly:
- String continuation: `'first part' + 'second part'` — join before parsing.
- Quote alternation: some files use single quotes, others double. Normalise to double.
- Slash-separated variable groups in `VALUE LABELS`.
- Leading/trailing whitespace and inline comments (`/*...*/`).
- Per-survey fixups stored in the registry, not in the parser (Census `LFACT` missing code 1, `CMA` spacing normalisation).

#### `parse_spss_split(layout_dir, layout_mask=NULL, encoding="Latin1")` → list(variables, codes, layout)

Consolidates `parse_pumf_metadata_spss()` + `read_pumf_layout_spss()` into a single pass.

File discovery via `find_unique_layout_file()` (kept from `helpers.R`):
- Layout: `*_i.sps`
- Variable labels: `*vare.sps`
- Value labels: `*vale.sps`
- Missing values: `*miss.sps`

`layout_mask` disambiguates when multiple `.sps` sets exist (e.g. SFS main vs BSW).

#### `parse_sas_cards(cards_dir, layout_mask=NULL, encoding="Latin1")` → list(variables, codes, layout)

Consolidates `parse_pumf_metadata_cards()` + layout reading from `parse_pumf_data_cards()`.

File types:
- `.lay` → column positions (`@pos NAME $width.` SAS input format or `NAME start - end` reading card format; detected by first non-blank character)
- `.lbe` → variable labels
- `.mvs` → missing value ranges
- `.cde` → code labels

#### `parse_lfs_codebook(codebook_path, encoding="CP1252")` → list(variables, codes, layout=NULL)

Consolidates `ensure_lfs_metadata()`.

The LFS `codebook.csv` structure:
- Rows where `Field_Champ` is filled → variable definitions (`Variable_Variable` = name, `EnglishLabel_EtiquetteAnglais` = label)
- Rows where `Field_Champ` is NA → code values for the preceding variable

Numeric column list (currently hardcoded in `get_lfs_pumf()`) moves here as a `type` override. The `SURVMNTH` left-pad fixup also moves here.

#### `parse_cpss_csv(variables_path, encoding="Latin1")` → list(variables, codes, layout=NULL)

Consolidates `parse_pumf_metadata_csv()`.

Columns used:
- `Variable` → variable name
- `Variable Name - English` → variable label
- `Code` → code value
- `Label - English` → code label

#### `merge_metadata(parsed_list)` → list(variables, codes, layout)

Warnings are emitted for:
- Same `(name, val)` pair with conflicting labels across sources.
- Conflicting missing ranges for the same variable.
- Variables in layout not present in variables table (and vice versa).

#### `write_metadata(metadata, metadata_dir)` → invisible(metadata_dir)

Writes the three canonical CSVs in UTF-8 regardless of source encoding.

---

## Phase 4 — Three-stage data pipeline

### New file: `R/pipeline.R`

#### Stage 1: `pumf_locate_or_download(series, version, cache_path, refresh=FALSE)`

Returns the version directory path.

Steps:
1. Build `version_dir <- file.path(cache_path, series, version)`.
2. If `refresh=TRUE`, delete the `.duckdb` file and `metadata/` (not the zip or extracted data).
3. If no `.zip` file in `version_dir`: look up URL from `list_canpumf_collection()`.
   - If URL is `"(EFT)"`: stop with a clear message — user must manually deposit the zip.
   - Otherwise: `dir.create(version_dir, recursive=TRUE)`; download to original filename; extract alongside.
4. If `.zip` exists but no extracted content: extract.
5. Return `version_dir`.

Extraction uses `ditto` on macOS, `utils::unzip()` elsewhere (`robust_unzip()` from `helpers.R`).

Replaces: `download_pumf()`, and the download boilerplate in `get_lfs_pumf()`, `get_chs_pumf()`, `get_sfs_pumf()`, `get_shs_pumf()`.

Note: for LFS, this function is called per version but the DuckDB target is at the series level. The LFS pipeline in Phase 7 calls this function for each version it needs to add.

#### Stage 2: `pumf_parse_metadata(version_dir, layout_mask=NULL, refresh=FALSE)`

Calls `detect_formats()`, runs all applicable parsers, calls `merge_metadata()`, calls `write_metadata()`. Returns `metadata_dir` invisibly.

#### Stage 3: `pumf_build_duckdb(version_dir, series, version, layout_mask=NULL, file_mask=NULL, refresh=FALSE)`

Steps:
1. Determine DuckDB path: `file.path(version_dir, paste0(series, "_", version, ".duckdb"))`.
2. If DuckDB exists and `refresh=FALSE`: skip to step 8.
3. Read canonical metadata from `metadata/`.
4. Read data file:
   - If `metadata/layout.csv` exists: `readr::read_fwf()` with positions.
   - Otherwise: `readr::read_csv()`.
   - `file_mask` selects the right data file when multiple exist.
   - All columns read as character; convert numeric columns per `variables.csv` `type` field.
5. Look up survey registry for BSW config:
   - If BSW mask present: read BSW file, join onto main data by join key, drop duplicate weight column.
6. Apply code labels and convert to factors (see Phase 1 "Factor conversion and NA handling"):
   - Apply per-survey data fixups from registry first (e.g. SFS `str_pad`), so the raw values match codes exactly before any mapping.
   - For each character column present in `codes.csv`: map raw values → label strings using the `val → label` lookup; apply `factor(..., levels = codes$label[codes$name == col])` with the complete ordered level set; unmatched values become `NA` (warn if any).
   - Character columns not in `codes.csv`: remain as character.
   - Numeric columns: coerce to numeric, then set values within `[missing_low, missing_high]` to `NA`.
7. Verify ENUM output: after writing to DuckDB, query `PRAGMA table_info(table_name)` and check that categorical columns have type `ENUM` not `VARCHAR`. If not, execute `ALTER TABLE ... ALTER COLUMN ... TYPE ENUM(...)` for each affected column.
8. Write labeled table to DuckDB. Table name = `layout_mask` if provided, else `"main"`.
9. Disconnect writer; open reader; return `duckplyr::tbl()`.

**LFS does not use this function** — it uses the LFS-specific pipeline in Phase 7 which handles appending to a shared table with schema reconciliation.

### Survey registry (`R/registry.R`)

A named list mapping `(series, version)` → configuration.

| field | description |
|---|---|
| `series` | e.g. `"SFS"` |
| `version` | e.g. `"2019"` |
| `layout_mask` | passed to parser to select right command file set |
| `bsw_mask` | layout mask for the bootstrap weight file (NULL if none) |
| `bsw_join_key` | column name to join BSW onto main data |
| `data_encoding` | encoding for raw data file (default `"CP1252"`) |
| `metadata_encoding` | encoding for command files (default `"Latin1"`) |
| `data_fixups` | named list of post-label transformations |
| `file_mask` | regex to select the right data file when multiple exist |

Surveys without a registry entry fall back to auto-detection. This covers the generic `read_pumf_data()` path for user-supplied directories not in the standard cache.

---

## Phase 5 — `open_pumf_documentation()` rewrite

Replace the 200-line if/else with a generic scanner in `R/pumf_documentation.R`.

Logic:
1. Find the version directory via `pumf_locate_or_download(..., refresh=FALSE)` (no download triggered).
2. `list.files()` recursively for `.pdf` and `.txt`.
3. If none found in extracted content, inspect the zip with `unzip(list=TRUE)` and extract only the doc files on demand into a `docs_extracted/` subdirectory.
4. Filter by `type` keyword pattern:

```r
type_patterns <- c(
  user_guide      = "User.?Guide|Guide.utilisateur|UserGuide",
  reference_guide = "Reference|référence|Ref.?Guide",
  questionnaire   = "Questionnaire",
  quality         = "Quality|qualité|Quality.?Guide",
  errata          = "errata|corrigenda"
)
```

5. `lapply(docs, browseURL)`; `invisible(docs)`.

---

## Phase 6 — Public API

### `get_pumf(series, version=NULL, layout_mask=NULL, file_mask=NULL, cache_path=..., refresh=FALSE, ...)`

For all series except LFS: runs all three stages in order; returns a lazy duckplyr table.

For LFS: delegates entirely to `lfs_get_pumf()` (Phase 7). The `refresh` parameter accepts `FALSE`, `TRUE`, or `"auto"` for LFS only; `"auto"` is an error for other series.

**Breaking change**: was a tibble, now a duckplyr table. Users call `collect()` for a local tibble.

### `pumf_metadata(series, version, cache_path=..., refresh=FALSE)`

Runs stages 1 and 2 only. Returns:
```r
list(
  variables = <tibble: name, label, type, missing_low, missing_high>,
  codes     = <tibble: name, val, label>,
  layout    = <tibble: name, start, end>  # NULL for CSV-format data
)
```

### Functions retained as-is

- `list_canpumf_collection()` — no change
- `list_available_lfs_pumf_versions()` — no change
- `add_bootstrap_weights()` — no change; works on any tibble
- `read_pumf_var_labels()`, `read_pumf_val_labels()`, `read_pumf_miss_labels()` — updated to read from canonical CSV

### Functions deprecated (keep with warning, remove in next major version)

- `get_pumf_connection()` — superseded by `get_pumf()`
- `label_pumf_data()` — now internal; exported for backward compat
- `convert_pumf_numeric_columns()`, `guess_numeric_pumf_columns()`, `label_pumf_columns()` — now internal
- `download_pumf()` — now internal
- `read_pumf_data()` — kept exported for manually-deposited directories outside standard cache; returns a tibble without DuckDB

---

## Phase 7 — LFS longitudinal database

The LFS is the only survey where it makes analytical sense to have all versions in one table: the schema is nearly uniform across years, and users routinely query multiple years together. The tradeoff is that the database state is less transparent than a per-version file.

### LFS version taxonomy

| Pattern | Type | Example | Coverage |
|---|---|---|---|
| `"YYYY"` | annual | `"2022"` | All 12 months of that year in one download |
| `"YYYY-MM"` | monthly | `"2024-06"` | One month of the current (or recent) year |

Annual files are released by StatCan several months after year-end and replace the monthly files for that year. Monthly files are available for the current year on an ongoing basis. A given calendar year can therefore exist in the database as 1–12 monthly entries or as one annual entry, never a mix.

### `lfs_versions` tracking table

Stored inside `LFS.duckdb` alongside the `lfs` data table.

| column | type | description |
|---|---|---|
| `version` | character | e.g. `"2022"` or `"2024-06"` |
| `type` | character | `"annual"` or `"monthly"` |
| `survyear` | integer | calendar year covered |
| `survmnth` | integer or NA | month (1–12) for monthly; NA for annual |
| `downloaded_at` | timestamp | when this version was added |
| `n_records` | integer | row count for this version |

### Monthly → annual supersession

When StatCan releases an annual file for year Y, any monthly versions of Y already in the database must be replaced:

1. DELETE all rows from `lfs` where `SURVYEAR == Y` (using DuckDB `DELETE FROM`).
2. DELETE all rows from `lfs_versions` where `survyear == Y`.
3. Download and parse the annual file for Y (Stage 1 + 2 for version `"Y"`).
4. Append the annual labeled data to `lfs`.
5. Insert one row into `lfs_versions` with `type = "annual"`.

The supersession check runs automatically whenever any LFS version is added or `refresh="auto"` is used: call `list_available_lfs_pumf_versions()`, find annual versions available on StatCan, compare against monthly entries in `lfs_versions`.

### Schema evolution

The LFS schema changes slightly across years (variables added, rarely removed). Strategy when appending a new version whose columns differ from the existing table:

- **New column in incoming data, not in `lfs` table**: `ALTER TABLE lfs ADD COLUMN <name> <type>` (DuckDB supports this); existing rows get `NULL`.
- **Column in `lfs` table missing from incoming data**: add the column as `NULL` to the incoming data frame before appending.
- In both cases, emit a message listing the differing columns so the user is aware.

This means `lfs` is always a superset of all versions' columns.

### `lfs_get_pumf(version=NULL, cache_path, refresh=FALSE)`

The internal implementation called by `get_pumf("LFS", ...)`.

#### Behaviour when `version=NULL`

1. Open (or create) `LFS.duckdb`.
2. If `lfs_versions` table exists and has rows:
   - Retrieve loaded versions.
   - Call `list_available_lfs_pumf_versions()` to get all available versions from StatCan.
   - Compute unavailable = available − loaded.
   - Emit an informative message:
     ```
     LFS database contains: 2022 (annual), 2023 (annual), 2024-01 to 2024-05 (monthly).
     Not yet downloaded: 2024-06, 2024-07.
     Use refresh="auto" to download all available versions.
     ```
3. If `lfs_versions` is empty or doesn't exist: emit a message that no data has been loaded and suggest specifying a version or using `refresh="auto"`.
4. Return a lazy `duckplyr::tbl()` reference to the full `lfs` table (possibly empty).

#### Behaviour when `version` is specified

1. Determine version type (annual if matches `"^\\d{4}$"`, monthly if matches `"^\\d{4}-\\d{2}$"`).
2. Check `lfs_versions`:
   - **Monthly version requested, annual for that year already loaded**: the annual data covers this month. No download needed. Return filtered table.
   - **Version already in `lfs_versions`**: no download needed. Return filtered table.
   - **Version not present**: proceed to download.
3. Download: call Stage 1 (`pumf_locate_or_download`) for the version subdirectory; call Stage 2 (`pumf_parse_metadata`).
4. Before appending, check if adding an annual version triggers supersession of existing monthly data for that year (see above).
5. Read and label data (same logic as Stage 3 but targeting the shared table):
   - Use version-specific `metadata/codes.csv` for labeling.
   - Reconcile schema against existing `lfs` table.
   - `DBI::dbAppendTable(con, "lfs", labeled_data)`.
   - Insert tracking row into `lfs_versions`.
6. Return filtered lazy table:
   - Annual `"YYYY"`: `filter(SURVYEAR == as.integer(year))`
   - Monthly `"YYYY-MM"`: `filter(SURVYEAR == year, SURVMNTH == month_label)` where `month_label` is the labeled value for that month number (e.g. `"June"` or `"06"` depending on how codes are applied — use the code label from `codes.csv`).

#### Behaviour when `refresh="auto"`

1. Call `list_available_lfs_pumf_versions()`.
2. Compare against `lfs_versions`.
3. For each available version not in the database, in chronological order: download and append (steps 3–5 above).
4. After all additions, run the supersession check: for each year where monthly entries exist and an annual version is now available, perform supersession.
5. Return full lazy `lfs` table with an informative summary message of what was added.

#### Behaviour when `refresh=TRUE`

Re-download and re-add a specific version. If `version=NULL` with `refresh=TRUE`, error with a message to use `refresh="auto"` for full refresh or specify a version.

Steps:
1. DELETE rows from `lfs` where `SURVYEAR` (and `SURVMNTH` if monthly) matches version.
2. DELETE matching row from `lfs_versions`.
3. Proceed with download and append as above.

### LFS metadata reconciliation

Since code labels may differ slightly across LFS codebook years (e.g. a province name changes, a new industry code is added), per-version labeling is used:

- Each version is labeled using its own `metadata/codes.csv` before appending.
- This means the `lfs` table stores human-readable factor labels, not raw codes. Longitudinal queries work correctly as long as label text is consistent.
- Where a label for the same concept changes between years (rare), both label strings will appear in the table. The `lfs_versions` table lets users identify which years use which labels if needed.

If a future version of a label conflicts with an existing one for the same code value, a warning is emitted at append time listing the conflicting variables so the user can investigate.

---

## Phase 8 — Comprehensive test suite

### Framework and location

`testthat` (standard for R packages). All tests live in `tests/testthat/`. Golden-file fixtures (reference CSVs) live in `tests/fixtures/<survey>/`. Actual PUMF data used as fixtures lives in `test_data/` (already gitignored where large).

### Test categories

#### A — Metadata parser unit tests

One test file per parser. Each test:
1. Points at a fixture directory containing a real PUMF command file set.
2. Calls the parser and compares output against reference CSVs stored in `tests/fixtures/<survey>/`.
3. Checks row counts, column names, specific known variable labels, and specific known code labels.
4. Checks that no expected variable is missing from the output and no wholly spurious variable appears.

Reference fixtures are generated once by running the new parsers against known-good data, spot-checked manually, then committed. They function as regression guards.

| Test file | Parser | Fixture source |
|---|---|---|
| `test-parse-spss-mono.R` | `parse_spss_mono()` | Census 2021 or 2016 SPSS file (EFT, in `test_data/` if available; otherwise `skip_if_not(file.exists(...))`) |
| `test-parse-spss-split.R` | `parse_spss_split()` | SFS 2019 or CHS 2018 SPSS split files |
| `test-parse-sas-cards.R` | `parse_sas_cards()` | Census 1991 reading cards — **`test_data/cen91_ind_95m0007x_ind_rec91/` already present in repo** |
| `test-parse-lfs-codebook.R` | `parse_lfs_codebook()` | LFS downloaded fixture (skip if offline) |
| `test-parse-cpss-csv.R` | `parse_cpss_csv()` | CPSS `variables.csv` from downloaded fixture (skip if offline) |
| `test-merge-metadata.R` | `merge_metadata()` | Synthetic in-memory lists; tests priority order and conflict warnings |
| `test-metadata-io.R` | `write_metadata()` / `read_metadata()` | Pure in-memory; already partly tested in Step 1 |

#### B — Integration tests (full pipeline per survey)

Each integration test runs the complete three-stage pipeline for one survey version and asserts:
- The DuckDB file is created.
- The `main` table has the expected column names.
- Row count is within a known range (not exact, since StatCan may revise files).
- All categorical columns have DuckDB type ENUM (queried via `PRAGMA table_info`).
- A sample of known factor levels is present (e.g. province names for PROV).
- Numeric columns have no values in the documented missing range remaining as data.

All integration tests use `testthat::skip_if_offline()` and `testthat::skip_on_cran()`. They also use a shared temp cache path and clean up after themselves.

| Test file | Survey | Notes |
|---|---|---|
| `test-pipeline-cpss.R` | CPSS v1 | Small CSV; good smoke test |
| `test-pipeline-lfs.R` | LFS (one recent annual) | Tests LFS pipeline including `lfs_versions` table |
| `test-pipeline-chs.R` | CHS 2021 | Tests SPSS split + BSW join |
| `test-pipeline-sfs.R` | SFS 2019 | Tests SPSS split + BSW join + `str_pad` fixup |
| `test-pipeline-census.R` | Census 2021 (individuals) | Tests SPSS mono; skip if EFT data absent |

#### C — Factor / ENUM property tests

`test-factor-enum.R` — no network, no large data.

- **Complete levels**: given a codes table with N distinct labels, the resulting factor has exactly N levels even when only a subset of codes appear in the synthetic data column.
- **Level order**: factor levels match the order of rows in `codes.csv`, not the order of first appearance in data.
- **Unmatched values → NA + warning**: a raw value not in `codes.csv` produces `NA` in the factor and triggers a warning naming the variable and the unmatched value(s).
- **Missing-range substitution**: numeric values within `[missing_low, missing_high]` become `NA_real_` after conversion.
- **ENUM type in DuckDB**: write a small data frame with a factor column to an in-memory DuckDB (`:memory:`); query `PRAGMA table_info`; assert column type is `ENUM`.
- **NULL round-trip**: write a factor with some `NA` elements; read back from DuckDB; assert those positions are `NA` in R (not a string `"NA"`).
- **Schema evolution (LFS)**: write two small data frames with differing column sets to the same DuckDB table using the append + ALTER logic; assert the merged table has all columns, NULLs in the right places, and ENUM types preserved after ALTER.

### Golden-file workflow

When a parser is first written:
1. Run it against the fixture directory.
2. Visually inspect a sample of variables and codes.
3. `write_metadata(parsed, "tests/fixtures/<survey>")` to commit the reference CSVs.
4. The test then reads these back with `read_metadata("tests/fixtures/<survey>")` and asserts `identical()`.

When a parser is intentionally changed:
1. Run tests — they will fail (good).
2. Inspect the diff between old and new fixture output.
3. If the change is correct, update the fixture: `write_metadata(new_parsed, "tests/fixtures/<survey>")` and commit.

This makes the intent of every parser change explicit in the git diff.

### `test_data/` fixture for SAS cards parser

`test_data/cen91_ind_95m0007x_ind_rec91/` is the only PUMF data already in the repo. It will be the primary always-available fixture for `test-parse-sas-cards.R` (and acts as a canary that the reading-cards parser works end-to-end). The golden files for this fixture are generated in Step 4 (SAS cards parser) and committed to `tests/fixtures/cen91/`.

---

## Implementation sequence

### Step 1 — Canonical metadata format + writer ✓ done

`write_metadata()`, `read_metadata()`, `metadata_exists()`, `validate_metadata()` in `R/metadata_parsers.R`. Testthat skeleton + `test-metadata-io.R` written alongside.

### Step 2 — testthat skeleton + factor/ENUM property tests (Phase 8C)

Set up `tests/testthat/` with `testthat::use_testthat()` if not present. Write `test-factor-enum.R` covering complete levels, level order, unmatched→NA+warning, missing-range substitution, ENUM type verification in `:memory:` DuckDB, NULL round-trip, and schema evolution for LFS append. These tests define the exact contract that Stage 3 must satisfy before any pipeline code is written.

### Step 3 — SPSS monolithic parser (Phase 3, hardest piece)

Write `parse_spss_mono()` against a known Census SPSS file. Compare output to current `ensure_2016_pumf_metadata()` / `ensure_2021_pumfi_metadata()` results to confirm no regression. Generate golden files → `tests/fixtures/census2016/` and `tests/fixtures/census2021/`. Write `test-parse-spss-mono.R`.

### Step 4 — SPSS split parser (Phase 3)

Refactor `parse_pumf_metadata_spss()` + `read_pumf_layout_spss()` into `parse_spss_split()`. Compare to current `.Rds` output for SFS 2019. Generate golden files → `tests/fixtures/sfs2019/`. Write `test-parse-spss-split.R`.

### Step 5 — SAS cards parser (Phase 3)

Refactor `parse_pumf_metadata_cards()` into `parse_sas_cards()`. Use `test_data/cen91_ind_95m0007x_ind_rec91/` as the always-present fixture. Generate golden files → `tests/fixtures/cen91/`. Write `test-parse-sas-cards.R`.

### Step 6 — CSV parsers (Phase 3)

Refactor `ensure_lfs_metadata()` → `parse_lfs_codebook()` and `parse_pumf_metadata_csv()` → `parse_cpss_csv()`. Write `test-parse-lfs-codebook.R` and `test-parse-cpss-csv.R` (both skip if offline). Generate golden fixtures from a downloaded LFS and CPSS version.

### Step 7 — Format detector + merger (Phase 3)

Write `detect_formats()` and `merge_metadata()`. Write `test-merge-metadata.R` with synthetic in-memory inputs covering priority ordering and conflict-warning cases.

### Step 8 — Survey registry (Phase 4)

Write `R/registry.R` with entries for all known surveys with special handling (SFS 2012/2016/2019/2023, CHS 2018/2021/2022, SHS 2017/2019, ITS 2018/2019, Census years).

### Step 9 — Stage 1: locate/download (Phase 4)

Write `pumf_locate_or_download()`. Test with CPSS v1 (small, downloadable; skip if offline).

### Step 10 — Stage 3: build DuckDB + factor/ENUM (Phase 4)

Write `pumf_build_duckdb()` including the factor conversion and ENUM verification step. Run `test-factor-enum.R` to validate the implementation against the contracts defined in Step 2. Write `test-pipeline-cpss.R` as the first integration test (skip if offline).

### Step 11 — Stage 2 wired in (Phase 4)

Wire `pumf_parse_metadata()` as the dispatcher between stages 1 and 3. Add `test-pipeline-chs.R` and `test-pipeline-sfs.R`.

### Step 12 — LFS pipeline (Phase 7)

Write `lfs_get_pumf()` with the `lfs_versions` table, version detection, append logic, schema reconciliation, supersession, and `refresh="auto"`. Write `test-pipeline-lfs.R`: load two annual versions, verify counts and column union, simulate supersession.

### Step 13 — Public API (Phase 6)

Update `get_pumf()` to dispatch to `lfs_get_pumf()` for LFS and to the standard pipeline for everything else. Add `pumf_metadata()`. Add deprecation warnings to old functions. Write `test-pipeline-census.R` (skip if EFT data absent).

### Step 14 — Documentation rewrite (Phase 5)

Replace `open_pumf_documentation()`.

### Step 15 — Cleanup

Remove `R/pumf_cache_path.R`, `R/layout_spss.R`, `R/layout_cards.R`, `R/layout_csv.R`, `R/lfs.R`, `R/chs.R`, `R/sfs.R`, `R/shs.R`, `R/census.R`, `R/canpumf_duckdb.R` once all logic is absorbed.
Update `NAMESPACE`, `DESCRIPTION`, `CLAUDE.md`. Run full test suite; fix any regressions.

---

## Files affected

| File | Action |
|---|---|
| `R/metadata_parsers.R` | **new** — all parsers + dispatcher + merger |
| `R/pipeline.R` | **new** — three-stage pipeline for non-LFS surveys |
| `R/lfs_pipeline.R` | **new** — LFS longitudinal database logic |
| `R/registry.R` | **new** — survey configuration registry |
| `R/pumf.R` | heavy rewrite — `get_pumf()` becomes thin dispatcher; internal functions deprecated |
| `R/layout.R` | retire — absorbed into `metadata_parsers.R` |
| `R/layout_spss.R` | retire — absorbed into `metadata_parsers.R` |
| `R/layout_cards.R` | retire — absorbed into `metadata_parsers.R` |
| `R/layout_csv.R` | retire — absorbed into `metadata_parsers.R` |
| `R/lfs.R` | retire — absorbed into `lfs_pipeline.R` + `parse_lfs_codebook()` |
| `R/chs.R` | retire — absorbed into registry |
| `R/sfs.R` | retire — absorbed into registry |
| `R/shs.R` | retire — absorbed into registry |
| `R/census.R` | retire — absorbed into registry + `parse_spss_mono()` |
| `R/canpumf_duckdb.R` | retire — `get_pumf_connection()` deprecated; logic in `pipeline.R` |
| `R/pumf_cache_path.R` | retire — logic absorbed into Stage 1 + registry |
| `R/helpers.R` | keep — `pumf_layout_dir()`, `find_unique_layout_file()`, `robust_unzip()` |
| `R/pumf_collection.R` | keep — `list_canpumf_collection()` unchanged |
| `R/pumf_documentation.R` | rewrite — generic scanner replaces if/else tree |
| `DESCRIPTION` | update imports; add `testthat` to Suggests |
| `NAMESPACE` | regenerate via `devtools::document()` |
| `CLAUDE.md` | update architecture section after completion |
| `tests/testthat/test-metadata-io.R` | **new** — write/read round-trip tests (Step 1) |
| `tests/testthat/test-factor-enum.R` | **new** — factor/ENUM property tests (Step 2, before pipeline) |
| `tests/testthat/test-parse-spss-mono.R` | **new** — SPSS monolithic parser (Step 3) |
| `tests/testthat/test-parse-spss-split.R` | **new** — SPSS split parser (Step 4) |
| `tests/testthat/test-parse-sas-cards.R` | **new** — SAS cards parser, uses `test_data/cen91_*` (Step 5) |
| `tests/testthat/test-parse-lfs-codebook.R` | **new** — LFS codebook parser, skip if offline (Step 6) |
| `tests/testthat/test-parse-cpss-csv.R` | **new** — CPSS CSV parser, skip if offline (Step 6) |
| `tests/testthat/test-merge-metadata.R` | **new** — merger + detector (Step 7) |
| `tests/testthat/test-pipeline-cpss.R` | **new** — CPSS end-to-end integration (Step 10) |
| `tests/testthat/test-pipeline-chs.R` | **new** — CHS integration (Step 11) |
| `tests/testthat/test-pipeline-sfs.R` | **new** — SFS integration (Step 11) |
| `tests/testthat/test-pipeline-lfs.R` | **new** — LFS longitudinal pipeline (Step 12) |
| `tests/testthat/test-pipeline-census.R` | **new** — Census integration, skip if EFT data absent (Step 13) |
| `tests/fixtures/cen91/` | **new** — golden metadata CSVs for Census 1991 SAS cards fixture |
| `tests/fixtures/census2016/` | **new** — golden metadata CSVs for Census 2016 SPSS mono fixture |
| `tests/fixtures/census2021/` | **new** — golden metadata CSVs for Census 2021 SPSS mono fixture |
| `tests/fixtures/sfs2019/` | **new** — golden metadata CSVs for SFS 2019 SPSS split fixture |
