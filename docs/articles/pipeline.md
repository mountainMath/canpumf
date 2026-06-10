# canpumf Pipeline Architecture

This document describes how
[`get_pumf()`](https://mountainmath.github.io/canpumf/reference/get_pumf.md)
turns a Statistics Canada PUMF zip file into a lazy DuckDB-backed
[`dplyr::tbl()`](https://dplyr.tidyverse.org/reference/tbl.html),
covering every choice and fallback along the way. The LFS has its own
accumulating pipeline, described separately at the end.

------------------------------------------------------------------------

## High-level flow

------------------------------------------------------------------------

## Stage 1 — Locate or download

[`pumf_locate_or_download()`](https://mountainmath.github.io/canpumf/reference/pumf_locate_or_download.md)
ensures the version directory exists with extracted content before any
parsing begins.

**Cache layout:**

    <cache_path>/
      <series>/
        <version>/
          <original>.zip          # retained after extraction
          <extracted dirs>/
          metadata/               # written by Stage 2
          <series>_<version>.duckdb

**Decision sequence:**

1.  **`refresh = TRUE`** — delete the `.duckdb` file(s) and `metadata/`
    subdirectory, leaving the zip and extracted content untouched.
    Stages 2 and 3 then re-run without re-downloading.

2.  **`redownload = TRUE`** — wipe the *entire* version directory first,
    then proceed as a first-time run. Implies `refresh`.

3.  **Already extracted?** — `version_is_extracted()` returns `TRUE` if
    any subdirectory (other than `metadata/`) or non-zip non-duckdb file
    is present. If `TRUE`, the zip step is skipped even when the zip is
    still on disk.

4.  **Download** — the URL is looked up in
    [`list_canpumf_collection()`](https://mountainmath.github.io/canpumf/reference/list_canpumf_collection.md).
    Surveys distributed only via Statistics Canada’s EFT portal have the
    marker `"(EFT)"` instead of a URL; the function stops with
    instructions to deposit the zip manually.

5.  **Extract** — `robust_unzip()` handles two edge cases:

    - *Naming collision*: some zips contain a single top-level directory
      with the same name as the archive (e.g. `2025-CSV.zip/`). The
      colliding directory is renamed to strip `.zip` before being moved
      into the version directory.
    - *Encoding*: older StatCan zips store filenames without the UTF-8
      flag (General Purpose Bit Flag bit 11). `grep`/`sub` calls on zip
      entry names use `useBytes = TRUE` to avoid “invalid in this
      locale” warnings.

------------------------------------------------------------------------

## Stage 2 — Parse metadata

[`pumf_parse_metadata()`](https://mountainmath.github.io/canpumf/reference/pumf_parse_metadata.md)
converts raw SPSS/SAS command files into three canonical CSVs. The
function is idempotent: it does nothing if `metadata/variables.csv`
already exists and `refresh = FALSE`.

### Format detection

[`detect_formats()`](https://mountainmath.github.io/canpumf/reference/detect_formats.md)
scans the entire version directory recursively and identifies which
parser(s) apply. **Multiple parsers can fire for the same survey**
(e.g. SPSS split for layout/codes and SAS cards for BSW weights).

| Priority | Format | Detection rule |
|----|----|----|
| 1 | **LFS codebook CSV** | filename matches `codebook\.csv` (case-insensitive) |
| 2 | **CPSS variables CSV** | filename is exactly `variables.csv` |
| 3 | **SAS reading cards** | directory contains both a `.lay` and a `.lbe` file |
| 4 | **SPSS split-file** | any `.sps` file whose name ends in `vare`, `vale`, or `_i` |
| 5 | **SPSS monolithic** | `.sps` file, `*SPSS.txt` file, or `.xmf` file whose content contains `VALUE LABELS` **or `DATA LIST`** (checked with `useBytes = TRUE` to tolerate CP850/Latin-1 data); `VARIABLE LABELS` is optional |
| 6 | **SPSS `.sav`** | a `.sav` binary file readable by haven |

Detection for case 5 also searches for a parallel French file — any
candidate in the same set whose path includes `/fran` or `/french`
(case-insensitive).

### Parsers

#### SPSS monolithic (`parse_spss_mono`)

Handles the single-file SPSS format used by Census (2001–2021), SFS
1999, SHS, and others. The file typically contains `DATA LIST`,
`VARIABLE LABELS`, `VALUE LABELS`, and sometimes `MISSING VALUES` and
`FORMATS` sections. `VARIABLE LABELS` is optional (e.g. Census 2011
individuals omits it). Older releases like SFS 1999 have only
`DATA LIST` with no label sections at all — these produce a fully
importable table with raw codes but no human-readable factor levels.

Key parsing details:

- **Column ranges** — `DATA LIST` ranges may have spaces on either side
  of the dash (`129-135`, `129 - 135`, or `129- 135`). All three are
  normalised by the regex `(\\d+)\\s*-\\s*(\\d+)` before tokenisation.

- **Record-group marker** — a leading `/` on the first variable line
  (e.g. `/PROVP 1-2`) is stripped, not discarded, so the variable is
  retained.

- **Section terminator** — the `DATA LIST` section ends at the first
  blank line, `.` line, or occurrence of `VARIABLE LABELS`,
  `VALUE LABELS`, `MISSING VALUES`, `FORMATS`, or `EXECUTE` at the start
  of a line. The keyword check is the reliable terminator for older
  files (e.g. 1991 XMF) that have no blank line between `DATA LIST` and
  `VARIABLE LABELS`.

- **DATA LIST type annotations** — the `(A)` suffix after a column range
  marks a character-type variable. The parser records an `is_char` flag
  per column and uses it to populate `variables.csv` types when no
  `VARIABLE LABELS` section is present.

- **Sentinel detection** — variables whose only VALUE LABELS are
  sentinel phrases (“Not applicable”, “Valid skip”, “Don’t know”, etc.)
  are classified as `numeric` with a `missing_low/missing_high` range,
  not as `character`. This prevents spurious NA warnings when numeric
  values fall outside the label set.

- **Zero-padded codes** — unquoted SPSS numeric codes like `01`, `02`
  are normalised via
  [`as.numeric()`](https://rdrr.io/r/base/numeric.html) →
  [`as.character()`](https://rdrr.io/r/base/character.html) so they
  match bare integer values in CSV data.

- **Multi-variable VALUE LABELS blocks** — `/VAR1 VAR2 VAR3` headers
  (possibly spanning continuation lines) are fully parsed so all listed
  variables receive the code/label pairs.

#### SPSS split-file (`parse_spss_split`)

Used by SFS, CPSS, and similar surveys that ship separate files for
variable labels (`*vare.sps`), value labels (`*vale.sps`), missing
values (`*miss.sps`), and layout (`*_i.sps`). The `layout_mask` from the
registry disambiguates when a single directory holds multiple sets
(e.g. individual vs. household files).

#### SAS reading cards (`parse_sas_cards`)

`.lay` files supply the fixed-width column positions; `.lbe` files
supply the value labels in `PROC FORMAT` syntax. Variable labels come
from a companion `.sas` file if present. This parser reuses
`parse_spss_split`’s layout parser since the `.lay` format is identical.

#### LFS codebook CSV (`parse_lfs_codebook`)

The LFS ships a single `*codebook.csv` with one row per code value.
Columns are always read as CP1252 regardless of the `metadata_encoding`
registry field.

#### CPSS variables CSV (`parse_cpss_csv`)

The Canadian Perspectives Survey Series ships a `variables.csv` with
variable metadata only; no layout or codes. The encoding defaults to
Latin-1 (CP1252 if the registry overrides).

#### SPSS `.sav` (`parse_spss_sav`)

Haven is used for binary `.sav` files when no text-format command file
is available. This is a fallback for surveys that do not ship SPSS
syntax.

### Metadata encoding

The registry `metadata_encoding` field sets the encoding for all
text-format parsers. Default is `"CP1252"` (a superset of Latin-1 that
correctly decodes Windows-era en-dashes and curly quotes). Exceptions:

| Surveys                        | Encoding  | Reason                         |
|--------------------------------|-----------|--------------------------------|
| Census 2021, 2021 hierarchical | `"UTF-8"` | Command files shipped as UTF-8 |
| Census 1991 (individuals)      | `"CP850"` | DOS-era IBM Code Page 850      |

### Merge

[`merge_metadata()`](https://mountainmath.github.io/canpumf/reference/merge_metadata.md)
takes the list of parser outputs and produces a single
`list(variables, codes, layout)`. Conflicts are resolved: later parsers
win on duplicate variable names. If a layout is present in only some
parsers, the function checks that every variable with a layout entry
also appears in `variables`, stopping with a diagnostic otherwise.

The final result is written to:

- `metadata/variables.csv` — one row per variable (name, label_en,
  label_fr, type, decimals, missing_low, missing_high)
- `metadata/codes.csv` — one row per code value (name, val, label_en,
  label_fr)
- `metadata/layout.csv` — one row per fixed-width column (name, start,
  end); absent for CSV-format surveys

------------------------------------------------------------------------

## Stage 3 — Build DuckDB

[`pumf_build_duckdb()`](https://mountainmath.github.io/canpumf/reference/pumf_build_duckdb.md)
reads the canonical CSVs from `metadata/`, reads the raw data file,
applies transformations, and writes a `.duckdb` file. The function skips
the build if the target table already exists and `refresh = FALSE`.

### Data file selection

`find_pumf_data_file()` searches the version directory recursively.

**Extension pre-filter** — derived from the registry `file_mask`:

| `file_mask` ends in | Pre-filter |
|----|----|
| `.csv` | only files matching `\.csv$` |
| `.txt` or `.dat` | only files matching `\.(txt\|dat)$` |
| other / unusual (e.g. `.INDIV`) | all files (relies on `file_mask` alone) |
| absent + layout exists | `\.(txt\|dat)$` (FWF inferred from layout) |
| absent + no layout | `\.csv$` |

Several subdirectories are always excluded from the search: `metadata/`,
`SPSS/`, `Command/`, `Syntax/`, `Layout/`, `SpssCard/`,
`Reading_cards/`, `Documents/`. Bootstrap weight (`_BSW.`) files are
also excluded; they are handled separately.

When multiple candidates survive, the `file_mask` regex narrows the
list. If more than one still remains, the function stops with a message
listing the ambiguous files and asks to set `file_mask` in the registry.

### FWF vs. CSV

After the data file is identified:

- **FWF** when `metadata/layout.csv` exists *and* the data file does not
  end in `.csv`. This handles the edge case (e.g. CHS) where the SPSS
  DATA LIST produces a layout but the actual data ships as CSV.
- **CSV** otherwise.

Both paths read all columns as character
(`col_types = cols(.default = "c")`) to preserve leading zeros and avoid
premature type coercion. Numeric conversion happens explicitly in the
next step.

### Data fixups (pre-label)

Registry `data_fixups` entries are applied to the raw character data
before label mapping:

- **`str_pad`** — left- or right-pad specified columns to a target
  width. Used to zero-pad codes that arrive without leading zeros in
  some CSV formats (e.g. SFS).
- **`rename`** — rename a column; applied only when the old name is
  present (safe for surveys that ship in multiple release variants,
  e.g. Census 2021 RELIG/RELIGION_DER).

### Bootstrap weight join (BSW)

When the registry has `bsw_mask` + `bsw_join_key` + `bsw_file_mask`, the
BSW file is found, read (CSV or FWF), and left-joined onto the main data
by the join key before numeric conversion.

### Numeric conversion

`apply_numeric_conversion()` converts character columns typed
`"numeric"` in `variables.csv`:

1.  [`as.numeric()`](https://rdrr.io/r/base/numeric.html) on the raw
    character values.
2.  **Missing range** — values in `[missing_low, missing_high]` become
    `NA`. This handles SPSS-declared `MISSING VALUES` blocks.
3.  **`na_values` fixup** — additional raw string sentinels from the
    registry (`data_fixups$na_values`) are set to `NA` via
    `trimws(raw) %in% na_values`. Used for undeclared income sentinels
    in older Census files.

The two mechanisms complement each other: the SPSS `MISSING VALUES`
block catches sentinels declared in the command file; `na_values`
catches those that StatCan omitted from the command file but documents
in the user guide.

**Census income sentinel widths** (confirmed from SPSS DATA LIST
sections):

| Census years     | Income field width | Sentinels (`na_values`)    |
|------------------|--------------------|----------------------------|
| 2016, 2021       | 8 chars            | `"99999999"`, `"88888888"` |
| 1991–2011        | 7 chars            | `"9999999"`, `"8888888"`   |
| 1986 and earlier | unverified         | none applied               |

The two sets are kept separate: applying the 7-digit sentinel to an
8-char field would incorrectly NA out a valid \$9,999,999 income value
(stored as `" 9999999"` which trims to `"9999999"`).

### Code labels → factors

`apply_code_labels()` maps raw character values to R factors using
`codes.csv`. The factor levels are the complete ordered set from the
codes table, not just the values present in the data. Unmatched raw
values become `NA` with a warning that shows the first five offending
values.

When `lang = "fra"`, any missing French label falls back per-row to the
English label.

### DuckDB write and ENUM enforcement

The labelled data frame is written to DuckDB with `dbWriteTable()`.
Factor columns are stored as DuckDB `ENUM` types. DuckDB ≥ 1.5.2 does
this automatically; for older versions `ensure_enum_columns()` runs
`ALTER TABLE ... ALTER COLUMN ... TYPE ENUM(...)` for each factor
column.

A separate DuckDB table is created per language (table names `eng` and
`fra`, or `eng_<layout_mask>` for surveys with multiple file types). The
write connection is shut down before
[`pumf_open_duckdb()`](https://mountainmath.github.io/canpumf/reference/pumf_open_duckdb.md)
re-opens the file in read-only mode, preventing in-process lock
conflicts when building both language tables in the same session.

------------------------------------------------------------------------

## LFS pipeline

The LFS is handled by
[`lfs_get_pumf()`](https://mountainmath.github.io/canpumf/reference/lfs_get_pumf.md)
(delegated from
[`get_pumf()`](https://mountainmath.github.io/canpumf/reference/get_pumf.md)).
Instead of one DuckDB per version, all LFS versions share a single
`<cache_path>/LFS/LFS.duckdb` with accumulating tables `lfs_eng` and
`lfs_fra`.

Key differences from the standard pipeline:

- **Schema evolution** — when a new LFS version adds a variable absent
  from earlier versions, the column is added via
  `ALTER TABLE ADD COLUMN`. When a variable changes type (e.g. VARCHAR →
  ENUM), `ALTER COLUMN SET DATA TYPE` is used.

- **Annual supersedes monthly** — if annual and monthly versions for the
  same year are both loaded, the annual version supersedes the monthly
  rows for that year.

- **Version tracking** — a `lfs_versions` table in the shared DuckDB
  records which versions have been downloaded and parsed, so
  `refresh = "auto"` downloads only new versions.

- **[`get_pumf()`](https://mountainmath.github.io/canpumf/reference/get_pumf.md)
  return** — when a specific version is requested, the function applies
  a
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  on `SURVYEAR` (and `SURVMNTH` for monthly requests) over the full
  shared table. Calling `get_pumf("LFS")` without a version returns the
  unfiltered table.

- **[`label_pumf_columns()`](https://mountainmath.github.io/canpumf/reference/label_pumf_columns.md)
  for LFS** — because the shared schema is the union of all loaded
  versions, variables introduced in later years (e.g. `GENDER` added
  ~2020) are absent from older versions’ `variables.csv`.
  [`label_pumf_columns()`](https://mountainmath.github.io/canpumf/reference/label_pumf_columns.md)
  therefore reads and merges metadata from *every* loaded version
  directory in chronological order, with the most-recent label winning
  on conflicts.

------------------------------------------------------------------------

## Connection provenance registry

[`get_pumf()`](https://mountainmath.github.io/canpumf/reference/get_pumf.md)
registers `(series, version, cache_path, lang)` in a package-level
environment keyed by the DuckDB connection’s C++ external-pointer
address:

    .pumf_con_registry  <- new.env(hash = TRUE, parent = emptyenv())
    key = format(con@conn_ref)   # stable across R-level S4 copies

This key survives `dplyr` tbl transformations and
[`select()`](https://dplyr.tidyverse.org/reference/select.html)/[`filter()`](https://dplyr.tidyverse.org/reference/filter.html)
calls because those operations do not create new connections.
[`label_pumf_columns()`](https://mountainmath.github.io/canpumf/reference/label_pumf_columns.md)
uses `.pumf_lookup_con()` to retrieve the provenance;
[`close_pumf()`](https://mountainmath.github.io/canpumf/reference/close_pumf.md)
removes the entry and disconnects.

------------------------------------------------------------------------

## Registry configuration

`pumf_registry_lookup(series, version)` returns a named list that
controls every per-survey choice in the pipeline. Surveys without an
entry use auto-detection with defaults.

| Field | Purpose | Default |
|----|----|----|
| `file_mask` | regex to select the data file | `NULL` (auto) |
| `layout_mask` | SPSS file disambiguator for split-file surveys | `NULL` |
| `data_encoding` | encoding of the raw data file | `"CP1252"` |
| `metadata_encoding` | encoding of SPSS/SAS command files | `"CP1252"` |
| `bsw_mask` | `layout_mask` for BSW-specific SPSS files | `NULL` |
| `bsw_file_mask` | filename pattern for the BSW data file | `NULL` |
| `bsw_join_key` | column(s) to join BSW onto the main data | `NULL` |
| `bsw_drop_cols` | BSW columns to drop before joining | `character(0)` |
| `data_fixups` | list of `str_pad`, `rename`, `na_values` transforms | [`list()`](https://rdrr.io/r/base/list.html) |
