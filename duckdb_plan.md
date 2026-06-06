# canpumf DuckDB refactoring — work plan

## Decisions

| # | Decision | Choice |
|---|---|---|
| 1 | Multi-file surveys (BSW, etc.) | Join bootstrap weight files into main table before writing DuckDB (Option A) |
| 2 | Labeled vs raw storage | Always apply labels; only labeled table written to DuckDB |
| 3 | Return type of `get_pumf()` | duckplyr lazy table (breaking change; users call `collect()` if they want a tibble) |
| 4 | Zip filename retention | Retain original filename; locate by "any `.zip` in the version dir" |
| 5 | Multi-format metadata | Parse ALL command files found (SPSS, SAS, CSV); merge results to maximise coverage |

---

## Overview

Three clean stages run in order whenever a connection to PUMF data is requested:

1. **Locate / download** — find or fetch the zip, extract alongside it.
2. **Parse metadata** — parse every command file format found; merge into canonical CSV.
3. **Build DuckDB** — read data, join BSW, apply labels, write DuckDB; return lazy table.

Each stage is idempotent. `refresh=TRUE` forces a stage to re-run even when its outputs exist.

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

`type` is `"numeric"` when the variable has a missing range and no code labels, `"character"` when it has code labels. Ambiguous cases (missing range AND code labels — which do exist, e.g. "not stated" coded numerically) are resolved by: if ANY code label exists, treat as `"character"`.

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

---

## Phase 2 — Cache directory structure

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
- For Census, version strings like `"2021 (individuals)"` map to directory `Census/2021 (individuals)/`. The product-code discovery logic in the current `pumf_cache_path.R` is retired; the user-deposited zip lives in the standardised path.
- Multi-file surveys where a single version directory holds conceptually separate files (ITS CDN vs VIS) use `layout_mask` as a DuckDB table name within one `.duckdb` file. The default table is named `main`.

---

## Phase 3 — Metadata parsers

### Design principle: parse everything, merge the results

Different command file formats provided with a PUMF release may contain complementary information. For example:
- The SPSS `*vare.sps` file may have variable labels absent from the SAS `.lbe` file.
- The SAS `.cde` file may have more complete code labels than the SPSS `*vale.sps`.
- The CSV codebook may have plain-text descriptions not encoded anywhere in the command files.

The dispatcher parses **all** formats found and merges using a priority order (most structured / most complete wins per variable). Merge strategy:
1. Variable label: use first non-NA across parsers, in priority order SPSS monolithic > SPSS split > SAS cards > CSV.
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
  spss_mono  = c("path/to/file.sps", ...),   # single-block SPSS
  spss_split = c("path/to/dir/", ...),        # directory with vare/vale/miss/i files
  sas_cards  = c("path/to/dir/", ...),        # directory with .lay/.lbe/.mvs/.cde
  lfs_csv    = "path/to/codebook.csv",        # LFS codebook format
  cpss_csv   = "path/to/variables.csv"        # CPSS variables.csv format
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

Quirks to handle:
- String continuation: `'first part' + 'second part'` — join before parsing.
- Quote alternation: some files use single quotes, others double. Normalise to double.
- Slash-separated variable groups in `VALUE LABELS`.
- Leading/trailing whitespace and inline comments (`/*...*/`).
- Documented per-survey fixups (stored in the survey registry, not hardcoded in the parser):
  - Census: `LFACT` missing code 1, `CMA` spacing normalisation.

#### `parse_spss_split(layout_dir, layout_mask=NULL, encoding="Latin1")` → list(variables, codes, layout)

Consolidates `parse_pumf_metadata_spss()` + `read_pumf_layout_spss()` into a single pass.

File discovery:
- Layout (`_i.sps`): already handled by `find_unique_layout_file()` — keep that helper.
- Variable labels (`vare.sps`), value labels (`vale.sps`), missing values (`miss.sps`): same pattern.
- `layout_mask` disambiguates when multiple `.sps` sets exist (e.g. SFS main vs BSW).

#### `parse_sas_cards(cards_dir, layout_mask=NULL, encoding="Latin1")` → list(variables, codes, layout)

Consolidates `parse_pumf_metadata_cards()` + the layout reading in `parse_pumf_data_cards()`.

File types:
- `.lay` → column positions (`@pos NAME $width.` or `NAME start - end` format)
- `.lbe` → variable labels
- `.mvs` → missing value ranges (`MISSING VALUES` block)
- `.cde` → code labels (`VALUE LABELS` block)

The `@pos NAME $width.` SAS input statement format and the `NAME start - end` reading card format are handled as two sub-variants detected by the first non-blank character of each layout line.

#### `parse_lfs_codebook(codebook_path, encoding="CP1252")` → list(variables, codes, layout=NULL)

Consolidates `ensure_lfs_metadata()`. The LFS `codebook.csv` uses:
- Rows where `Field_Champ` is filled → variable definitions (`Variable_Variable` = name, `EnglishLabel_EtiquetteAnglais` = label)
- Rows where `Field_Champ` is NA → code values for the preceding variable

Numeric column list (currently hardcoded in `get_lfs_pumf()`) moves here as a `type` override: any variable name in that list gets `type="numeric"` in `variables.csv` regardless of whether it has code labels. The `SURVMNTH` left-pad fixup also moves here.

#### `parse_cpss_csv(variables_path, encoding="Latin1")` → list(variables, codes, layout=NULL)

Consolidates `parse_pumf_metadata_csv()`. The CPSS `variables.csv` uses:
- `Variable` column → variable name
- `Variable Name - English` column → variable label
- `Code` column → code value
- `Label - English` column → code label

#### `merge_metadata(parsed_list)` → list(variables, codes, layout)

Takes the named list output from `detect_formats()` + individual parsers and merges:
1. Variables: full outer join on `name`; coalesce label, type, missing across sources by priority.
2. Codes: bind_rows then `distinct(name, val, .keep_all=TRUE)` in priority order.
3. Layout: use whichever source provided it (only one should).

Warnings are emitted for:
- Same `(name, val)` pair with conflicting labels across sources.
- Conflicting missing ranges for the same variable.
- Variables in layout not present in variables table (and vice versa).

#### `write_metadata(metadata, metadata_dir)` → invisible(metadata_dir)

Writes the three canonical CSVs. Always uses UTF-8 for output regardless of source encoding.

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

Extraction: use `ditto` on macOS (as now), `utils::unzip()` elsewhere.

Replaces: `download_pumf()`, the download boilerplate in `get_lfs_pumf()`, `get_chs_pumf()`, `get_sfs_pumf()`, `get_shs_pumf()`.

#### Stage 2: `pumf_parse_metadata(version_dir, layout_mask=NULL, refresh=FALSE)`

Calls `detect_formats()`, runs all applicable parsers, calls `merge_metadata()`, calls `write_metadata()`. Returns `metadata_dir` invisibly.

#### Stage 3: `pumf_build_duckdb(version_dir, series, version, layout_mask=NULL, file_mask=NULL, refresh=FALSE)`

Steps:
1. Determine DuckDB path: `file.path(version_dir, paste0(series, "_", version, ".duckdb"))`.
2. If DuckDB exists and `refresh=FALSE`: skip to step 7.
3. Read canonical metadata from `metadata/`.
4. Read data file:
   - If `metadata/layout.csv` exists: use `readr::read_fwf()` with positions.
   - Otherwise: use `readr::read_csv()`.
   - Apply `file_mask` to select the right data file when multiple exist.
   - Columns read as character by default; convert numeric columns per `variables.csv` `type` field.
5. Look up the survey registry entry for `(series, version)` to find any BSW file mask and join key.
   - If BSW mask present: repeat steps 2b–4 for the BSW file; join onto main data by the join key; drop the duplicate weight column from BSW.
6. Apply code labels:
   - For each column in `codes.csv`, apply the val → label mapping.
   - Create R factor with levels in val order from `codes.csv`.
   - Columns not in `codes.csv` and with `type="character"` remain as character.
   - Columns with `type="numeric"` set missing range values to `NA` then coerce to numeric.
   - Apply any data fixups registered for this `(series, version)` (e.g. SFS `str_pad`).
7. Write labeled table to DuckDB using `duckdb::dbWriteTable()`. Table name = `layout_mask` if provided, else `"main"`.
8. Disconnect writer connection; open reader connection; return `duckplyr::tbl()` reference.

Replaces: `read_pumf_data()`, `label_pumf_data()`, `convert_pumf_numeric_columns()`, `get_pumf_connection()`, BSW join logic in `sfs.R`, `chs.R`, `shs.R`.

### Survey registry

A named list (or internal data frame) mapping `(series, version)` → configuration. Lives in `R/registry.R`.

Each entry has:

| field | description |
|---|---|
| `series` | e.g. `"SFS"` |
| `version` | e.g. `"2019"` |
| `layout_mask` | passed to SPSS/SAS parser to select right command file set |
| `bsw_mask` | layout mask for the bootstrap weight file (NULL if none) |
| `bsw_join_key` | column name to join BSW onto main data |
| `data_encoding` | encoding for raw data file (default: `"CP1252"`) |
| `metadata_encoding` | encoding for command files (default: `"Latin1"`) |
| `data_fixups` | named list of transformations to apply post-label (e.g. `list(pad_left=c("PASRBUYG"))`) |
| `file_mask` | regex to select the right data file when multiple `.txt`/`.csv` exist |

Surveys without a registry entry fall back to auto-detection (no BSW, no fixups, default encodings). This covers the generic `read_pumf_data()` path for user-supplied directories.

---

## Phase 5 — `open_pumf_documentation()` rewrite

Replace the 200-line if/else with a generic scanner in `R/pumf_documentation.R`.

```r
open_pumf_documentation <- function(series, version=NULL, type="user_guide",
                                    cache_path=getOption("canpumf.cache_path")) {
  version_dir <- pumf_locate_or_download(series, version, cache_path, refresh=FALSE)
  
  # scan extracted content and inside zip for docs
  docs <- list.files(version_dir, pattern="\\.pdf$|\\.txt$",
                     recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
  
  if (length(docs)==0) {
    # look inside the zip without full extraction
    zips <- list.files(version_dir, pattern="\\.zip$", full.names=TRUE)
    if (length(zips)==1) {
      zip_contents <- unzip(zips, list=TRUE)
      doc_entries <- zip_contents$Name[grepl("\\.pdf$|\\.txt$", zip_contents$Name, ignore.case=TRUE)]
      if (length(doc_entries)>0) {
        exdir <- file.path(version_dir, "docs_extracted")
        unzip(zips, files=doc_entries, exdir=exdir)
        docs <- list.files(exdir, full.names=TRUE, recursive=TRUE)
      }
    }
  }
  
  type_patterns <- c(
    user_guide       = "User.?Guide|Guide.utilisateur|UserGuide",
    reference_guide  = "Reference|référence|Ref.?Guide",
    questionnaire    = "Questionnaire",
    quality          = "Quality|qualité|Quality.?Guide",
    errata           = "errata|corrigenda"
  )
  
  if (!is.null(type) && type %in% names(type_patterns)) {
    docs <- docs[grepl(type_patterns[[type]], docs, ignore.case=TRUE)]
  }
  
  if (length(docs)==0) stop("No documentation found for ", series, " ", version)
  lapply(docs, browseURL)
  invisible(docs)
}
```

---

## Phase 6 — Public API

### `get_pumf(series, version, layout_mask=NULL, file_mask=NULL, cache_path=..., refresh=FALSE, ...)`

Runs all three stages in order; returns a lazy duckplyr table. **Breaking change**: was a tibble, now a duckplyr table. Users who need a local tibble call `collect()`.

### `pumf_metadata(series, version, cache_path=..., refresh=FALSE)`

Runs stages 1 and 2 only. Returns a list:
```r
list(
  variables = <tibble: name, label, type, missing_low, missing_high>,
  codes     = <tibble: name, val, label>,
  layout    = <tibble: name, start, end>  # NULL for CSV-format data
)
```
Useful for inspecting the variable dictionary before loading data.

### Functions retained as-is

- `list_canpumf_collection()` — no change
- `list_available_lfs_pumf_versions()` — no change
- `add_bootstrap_weights()` — no change; works on any tibble/data frame
- `read_pumf_var_labels()`, `read_pumf_val_labels()`, `read_pumf_miss_labels()` — updated to read from canonical CSV instead of `.Rds`; keep exported for users with local PUMF directories

### Functions deprecated (keep with warning, remove in next major version)

- `get_pumf_connection()` — superseded by `get_pumf()`
- `label_pumf_data()` — now internal; kept exported for backward compat with `read_pumf_data()` workflow
- `convert_pumf_numeric_columns()` — now internal
- `guess_numeric_pumf_columns()` — now internal
- `label_pumf_columns()` — now internal
- `download_pumf()` — now internal (superseded by Stage 1)
- `read_pumf_data()` — kept exported for users with manually-deposited directories not in the standard cache layout; reads data and returns a tibble without DuckDB

---

## Implementation sequence

### Step 1 — Canonical metadata format + writer (Phase 1)

Write `write_metadata()` and the CSV schema. No parsers yet.
Test: manually construct a `list(variables, codes, layout)` and verify the CSVs look right.

### Step 2 — SPSS monolithic parser (Phase 3, hardest piece)

Write `parse_spss_mono()` against a known Census 2016 or 2021 SPSS file from `test_data/`.
Test: compare output to current `ensure_2016_pumf_metadata()` / `ensure_2021_pumfi_metadata()` results.

### Step 3 — SPSS split parser (Phase 3)

Refactor `parse_pumf_metadata_spss()` + `read_pumf_layout_spss()` into `parse_spss_split()` writing canonical CSV.
Test: compare to current `.Rds` output for a known SFS or CHS dataset.

### Step 4 — SAS cards parser (Phase 3)

Refactor `parse_pumf_metadata_cards()` into `parse_sas_cards()`.
Test: compare to current `.Rds` output for a known SHS dataset.

### Step 5 — CSV parsers (Phase 3)

Refactor `ensure_lfs_metadata()` into `parse_lfs_codebook()` and `parse_pumf_metadata_csv()` into `parse_cpss_csv()`.

### Step 6 — Format detector + merger (Phase 3)

Write `detect_formats()` and `merge_metadata()`. Test against directories with multiple format families.

### Step 7 — Survey registry (Phase 4)

Write `R/registry.R` with entries for all known surveys with special handling (SFS 2012/2016/2019/2023, CHS 2018/2021/2022, SHS 2017/2019, ITS 2018/2019, LFS, Census years).

### Step 8 — Stage 1: locate/download (Phase 4)

Write `pumf_locate_or_download()`. Test with a small downloadable survey (CPSS or CCAHS).

### Step 9 — Stage 3: build DuckDB (Phase 4)

Write `pumf_build_duckdb()`. Test end-to-end with a small survey.

### Step 10 — Stage 2 wired in (Phase 4)

Wire `pumf_parse_metadata()` as the dispatcher between stages 1 and 3.

### Step 11 — Public API (Phase 6)

Update `get_pumf()`, add `pumf_metadata()`, add deprecation warnings to old functions.

### Step 12 — Documentation rewrite (Phase 5)

Replace `open_pumf_documentation()`.

### Step 13 — Cleanup

Remove `R/pumf_cache_path.R` (logic absorbed into Stage 1 + registry).
Remove survey-specific modules (`lfs.R`, `chs.R`, `sfs.R`, `shs.R`) once all logic is in registry + pipeline.
Update `NAMESPACE` and `DESCRIPTION` (drop explicit `duckdb` import if handled via `DBI`; ensure `duckplyr` re-export).
Update `CLAUDE.md`.

---

## Files affected

| File | Action |
|---|---|
| `R/metadata_parsers.R` | **new** — all parsers + dispatcher + merger |
| `R/pipeline.R` | **new** — three-stage pipeline |
| `R/registry.R` | **new** — survey configuration registry |
| `R/pumf.R` | heavy rewrite — `get_pumf()` becomes thin wrapper; internal functions deprecated |
| `R/layout.R` | retire — content absorbed into `metadata_parsers.R` |
| `R/layout_spss.R` | retire — absorbed into `metadata_parsers.R` |
| `R/layout_cards.R` | retire — absorbed into `metadata_parsers.R` |
| `R/layout_csv.R` | retire — absorbed into `metadata_parsers.R` |
| `R/lfs.R` | retire — absorbed into registry + `parse_lfs_codebook()` |
| `R/chs.R` | retire — absorbed into registry |
| `R/sfs.R` | retire — absorbed into registry |
| `R/shs.R` | retire — absorbed into registry |
| `R/census.R` | retire — absorbed into registry + `parse_spss_mono()` |
| `R/canpumf_duckdb.R` | retire — `get_pumf_connection()` deprecated; logic in `pipeline.R` |
| `R/pumf_cache_path.R` | retire — logic absorbed into Stage 1 + registry |
| `R/helpers.R` | keep — `pumf_layout_dir()`, `find_unique_layout_file()`, `robust_unzip()` remain useful |
| `R/pumf_collection.R` | keep — `list_canpumf_collection()` unchanged |
| `R/pumf_documentation.R` | rewrite — generic scanner replaces if/else tree |
| `DESCRIPTION` | update imports |
| `NAMESPACE` | regenerate via `devtools::document()` |
| `CLAUDE.md` | update architecture section after completion |
