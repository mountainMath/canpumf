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
3. **`close_pumf(x)`** — disconnects the DuckDB connection. `x` may be a lazy tbl (the embedded `x$src$con` is closed) or a DuckDB connection from `get_pumf_connection()` (closed directly; detected via `inherits(x, "DBIConnection")`). The provenance-registry cleanup is guarded with `exists()` so unregistered connections (which `get_pumf_connection()` never registers) don't error. Only needed before writing (e.g. `refresh = TRUE`) to the same file from another tbl.
4. **`pumf_metadata(series, version, ...)`** — runs Stage 1+2 only; returns the canonical metadata list (`variables`, `codes`, `layout`).
5. **`open_pumf_documentation(series, version, ...)`** — scans the cache directory for PDF/TXT docs and opens them in the browser.

### Connection provenance registry (`R/api.R`)

`get_pumf()` registers `(series, version, cache_path, lang)` in a package-level environment keyed by the DuckDB connection's C++ external-pointer address. This key is stable across R copies of the S4 connection wrapper (R's copy-on-modify would silently lose attrs set directly on the wrapper). `label_pumf_columns()` uses `.pumf_lookup_con()` to retrieve this provenance; `close_pumf()` removes it.

### Connection locking invariant (issue #18)

**The read path must never open a write connection.** DuckDB allows many concurrent read-only connections across processes but only one exclusive writer, so a spurious `read_only = FALSE` open breaks e.g. rendering a notebook while the interactive session holds tbls open. Write connections exist only (a) inside `pumf_build_duckdb()` / the LFS write phase while a build or refresh actually writes — opened after `.assert_duckdb_writable()` and closed before returning — and (b) in `add_bootstrap_weights()` / `remove_bootstrap_weights()`, which close the input tbl first, assert writability (for the actionable lock message), write, and hand back a fresh read-only tbl. `get_pumf()` therefore calls `pumf_run_pipeline(read_only = read_only)` directly (NOT via `get_pumf_connection()`, which hardcodes read-write); on a cache hit every connection is read-only end to end. Transient existence probes use `.duckdb_table_exists()` (read-only, `shutdown = FALSE`) so an in-process instance shared with a user's open tbl is never shut down. Regression tests: "read path never takes a write lock" section of `tests/testthat/test-api.R`.

## Architecture

### Three-stage pipeline (non-LFS)

All standard surveys use an idempotent three-stage pipeline in `R/pipeline.R`:

1. **Stage 1 — `pumf_locate_or_download(series, version, cache_path, refresh)`**: ensures the version directory at `<cache_path>/<series>/<version>/` exists with extracted content.
2. **Stage 2 — `pumf_parse_metadata(version_dir, layout_mask, metadata_encoding, refresh)`**: detects and parses all metadata formats, merges into canonical CSVs in `<version_dir>/metadata/`.
3. **Stage 3 — `pumf_build_duckdb(version_dir, series, version, lang, layout_mask, file_mask, refresh)`**: reads data file, joins BSW weights, applies numeric conversion and code labels, writes to `<version_dir>/<series>_<version>.duckdb`. Returns path list; use `pumf_open_duckdb()` to get a lazy tbl.

`pumf_run_pipeline()` chains all three stages using registry config.

#### Multi-module surveys (linked files in one DuckDB)

Some surveys ship several linked files that share a respondent key and must be joined for analysis. Converted surveys: GSS cycle 16 / "Aging and Social Support" 2002 (`MAIN` + `CG4` + `CG6` + `CR`, `RECID`, person weight `WGHT_PER` only in MAIN); GSS Time Use 1998/2010/2015/2022 (`Main` + `Episode`, `RECID` or `PUMFID`); SHS 2017 (`Interview` + `Diary`, `CASEID`); SGVP 1997–2010 (`MAIN` + `GS`/`VD`/`GIVE`/`VOLNTR`, `PUMFID`/`MICRO_ID`/`IDNUM`). These are modelled as **several tables in one DuckDB file**, joinable on the shared key — *not* as separate databases (which could not be joined on one connection).

A registry entry declares `modules = list(MAIN = ..., CG4 = ...)` via `.make_entry()`; each module carries its own `layout_mask`, `file_mask`, `data_fixups`, and BSW config (`bsw_mask`, `bsw_file_mask`, `bsw_join_key`, `bsw_drop_cols`, `bsw_strata`). One module is `primary` (the default table; its `layout_mask`/`file_mask`/`data_fixups`/BSW config are auto-derived to the entry's top level so single-table code paths, `.read_bsw_data(reg)`, and the override ledger see it). `.pumf_entry_modules(reg)` returns the per-module config (each `list(id, layout_mask, file_mask, data_fixups, bsw_*, is_primary, meta_subdir)`); the primary module's `meta_subdir` is `NULL` (uses `metadata/`), secondary modules use `metadata/<id>/`. The entry also carries `module_key` — the shared respondent key the modules join on — recorded in one place (`.pumf_module_key(reg)`) rather than only in tests; it varies by survey (`RECID`/`PUMFID`/`MICRO_ID`/`CASEID`/`IDNUM`).

`pumf_run_pipeline()` loops the modules, running Stage 2 (`pumf_parse_metadata(..., layout_mask, meta_subdir)`) and Stage 3 (`pumf_build_duckdb(..., layout_mask, file_mask, meta_subdir, data_fixups, bsw_override)`) per module so all tables land in the one DuckDB file; the primary module's tbl is returned. Each module joins its **own** bootstrap weights: `bsw_override` passes that module's BSW config so e.g. the Interview replicate weights are not mis-joined onto Diary (an override whose fields are all `NULL` means "this module has no BSW"). Table names come from `.pumf_table_name(series, version, lang, module)` (`<lang>_<layout_mask>`), so modules are distinct tables.

User-facing: `get_pumf("GSS", "2002")` returns the primary (MAIN) table and emits a one-time message (`.pumf_announce_modules()`) listing the sibling modules and a `pumf_module()` example; `get_pumf("GSS", "2002", module = "CG4")` opens a module standalone (own connection); `pumf_module(tbl, "CG4")` opens a sibling module **on the same connection** so the two are joinable, and announces the `module_key` join key once per survey. `label_pumf_columns()`/`pumf_var_labels()` are module-aware: `.pumf_tbl_module()` recovers the module from the tbl's remote table name (falling back to the connection-registered module after joins), so each module's own `metadata/<id>/variables.csv` is read even though all modules share one connection.

#### Data file detection

`.find_pumf_data_file(version_dir, file_mask, prefer_fwf)` selects the data file. A `.sas7bdat` selection is read with `haven::read_sas()` (`.read_sas_data()`), for releases that ship the SAS dataset instead of a flat file (PALS 2001). Character columns keep their raw codes verbatim; numeric columns arrive native and are rendered back to code strings by `.coerce_coded_to_character()` **after** the data fixups run, so a renamed column is matched under the name the metadata declares. The extension pattern is derived from the `file_mask` first (`.csv` → CSV, `.txt`/`.dat` → FWF). When `file_mask` uses an unrecognised extension (e.g. `.INDIV` for the 1991 Census), `ext_pat` is set to `NULL` and all files are searched, with `file_mask` alone selecting the result. The final `is_fwf` decision is made from the actual found file extension and the presence of `layout.csv` — this handles surveys like CHS that ship both a CSV and a TXT file but whose SPSS DATA LIST section also creates a `layout.csv`.

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

Nine parsers converge on three canonical CSV files in `<version_dir>/metadata/`:
- `variables.csv` — one row per variable (name, label_en, label_fr, type, decimals, missing_low, missing_high)
- `codes.csv` — one row per code value (name, val, label_en, label_fr)
- `layout.csv` — one row per fixed-width column (name, start, end); absent for CSV-format data

Parsers (in detection priority order — highest first):
1. `parse_lfs_codebook()` — LFS `*codebook.csv`; always read as CP1252
2. `parse_cpss_csv()` — CPSS `variables.csv`
3. `parse_sas_cards()` — directory with `.lay` + `.lbe` files
4. `parse_spss_split()` — directory with `vare`/`vale`/`_i` named `.sps` files
5. `parse_spss_mono()` — single `.sps`, `*SPSS.txt`, or `.xmf` file whose content contains `VALUE LABELS` or `DATA LIST`. `VARIABLE LABELS` is optional (e.g. Census 2011 individuals). DATA LIST-only files (e.g. SFS 1999) produce layout+type info but no human-readable labels; labels are supplemented by `parse_pdf_dictionary()` (parser 7) when available.
6. `parse_spss_sav()` — binary SPSS `.sav` file (read via haven)
7. `parse_pdf_dictionary()` — StatCan PDF Data Dictionary (`*Dictionary.pdf`); extracts variable long-names and code-value labels. Requires `pdftools`. Positions in the PDF differ from the PUMF flat file so this parser produces only `variables` and `codes` (no `layout`). Used as a label-only fallback for surveys like SFS 1999 where the SPSS command file is DATA LIST-only. Only fires when `pdftools` is installed and a matching PDF is found under the version directory.
8. `parse_pdf_codebook()` — StatCan bilingual PDF **frequency codebook** (e.g. CPSS 1, the only CPSS cycle with no machine-readable `variables.csv`). A second, distinct PDF layout: per-variable `Variable Name:`/`Concept:` blocks followed by an `Answer Categories` frequency table whose `Code` column supplies value labels. Requires `pdftools`. Produces only `variables` and `codes` (no `layout`). Detection is a **fallback of last resort** — only consulted when no command file or codebook CSV was found, to avoid scanning PDFs for the common surveys; candidates are PDFs under a `Codebook`/`LivreDesCodes` path (`zerofreq` variants excluded), content-verified for the `Variable Name:` + `Answer Categories` signature before use.
9. `parse_pdf_freq_codebook()` — StatCan **user-guide data dictionary** (`12M00nn-GPE.pdf` and friends), a *third* PDF layout: `Variable Name: X  Position: N  Length: L`, a free-text label, then a right-aligned `FREQ`/`WTD` (French `FREQ`/`POND`) frequency table of `<code> <label> <freq> <wtd>` rows, closed by a `======` rule. Continuous variables print a single `lo : hi` range row instead of per-value rows. Requires `pdftools`; produces `variables`, `codes`, plus the extra `freqs`/`ranges` tables that drive the cross-check below. **It is deliberately excluded from `merge_metadata()`** unless nothing else parsed — see "PDF cross-check and label repair".

Multiple parsers can fire for the same survey (e.g. split-SPSS for layout/codes and SAS cards for BSW weights). `merge_metadata()` consolidates all results.

Key parsing details:
- **Sentinel detection**: two anchored patterns built from shared alternatives. `.missing_pat` matches **true-missing** labels (Not applicable, Not stated, Not asked, Valid skip, Refusal, Don't know, … plus French equivalents); `.sentinel_pat` additionally matches **zero-value** labels ("ZERO HOURS", "None", "Aucun don") that indicate a continuous variable but are valid zeros, not missing data. Variables whose value labels are ALL sentinel labels are classified `numeric`; their `missing_low/missing_high` range is derived from the `.missing_pat`-matching codes **only** (e.g. GSS 2012 `ITL_Q10` with 0="None", 97-99 missing must get range [97,99], not [0,99]).
- **SPSS string continuations**: `.spss_read_preprocess()` joins `'text' + 'more'` into one literal. StatCan uses this to keep long labels inside the file's line width and it appears in **all four** combinations of quote character and line break — Census 2021 alone ships single-quoted-continued (individuals EN), double-quoted-continued (individuals FR, 70 labels), inline double-quoted (hierarchical FR), and mixed delimiters (`"…d'eq" + 'uivalence'`). A trailing `+` pulls the next line up first (walking backwards so not-yet-visited indices stay valid), then the four quote-pair patterns are collapsed in place until a fixed point, so a chain of fragments folds one join at a time. Each content class must match the literal's *own* delimiter — a double-quoted label routinely contains apostrophes — and the opening quote must follow whitespace or start the line so a stray apostrophe cannot be read as an opening delimiter. Dropping the tail leaves a label cut mid-word (`SSGRAD` as `"Scolarité : … attestation d'éq"`), indistinguishable from the upstream truncation the PDF cross-check exists to repair.
- **Zero-padded codes**: unquoted SPSS numeric codes like `01`, `02` are normalized via `as.numeric()` → `.code_chr()` so they match bare integer values in CSV data. `.code_chr()` formats elementwise and never uses scientific notation (`as.character(200000)` would yield `"2e+05"` and break joins).
- **MISSING VALUES parsing**: `.spss_parse_missing()` tolerates padding inside the parens (`VALUEH  ( 999999 )/`) and negative values; single values become `missing_low == missing_high`, `lo THRU hi` becomes a range; multi-value discrete sets record only the first value (conservative).
- **SAS PROC FORMAT codes**: `parse_sas_data_labels()` (the `sas_labels` format, e.g. GSS 2007) parses `VALUE VnnnF` blocks and associates them to variables via the StatCan-style `/* VnnnF format applies to: VAR1 VAR2 */` comments (possibly spanning lines). Files without such comments (Census 2011) yield labels only, no codes.
- **Name-case normalisation**: Stage 3 uppercases `variables$name`, `codes$name`, and `layout$name` after `read_metadata()` (and `.pumf_read_variables()` does the same for `label_pumf_columns()`), because CSV data columns are uppercased on read while some command files declare mixed-case names (Census 2021 `TotInc`). Without this, mixed-case variables silently skip numeric conversion and labeling.
- **Multi-variable VALUE LABELS blocks**: `/VAR1 VAR2 VAR3` headers (possibly spanning continuation lines) are fully parsed so all listed variables receive the code/label pairs.
- **SPSS DATA LIST column ranges**: spaces around the dash are tolerated in all forms — `129-135`, `129 - 135`, `129-  135` — via `(\\d+)\\s*-\\s*(\\d+)` normalisation before tokenisation. A leading `/` record-group marker on the first variable line is stripped (not discarded) so the variable is retained.
- **SPSS DATA LIST section terminator**: the section ends at the first blank line, `.` line, or occurrence of `VARIABLE LABELS`, `VALUE LABELS`, `MISSING VALUES`, `FORMATS`, or `EXECUTE` at the start of a line. The keyword check is the reliable terminator for older files (e.g. 1991 XMF) that have no blank line between `DATA LIST` and `VARIABLE LABELS`.
- **SPSS DATA LIST decimals**: no annotation → `fmt_type="F"`, `decimals=0` (integer); `(A)` or `(An)` → character format; `(n)` → `decimals=n`; `(Fn.d)` → `decimals=d`.
- **DATA LIST-only SPSS files**: When a SPSS file has `DATA LIST` but no `VARIABLE LABELS` or `VALUE LABELS` (e.g. SFS 1999), `.spss_mono_single` populates `variables.csv` from layout type info: `(A)` columns → `type="character"`, others → `type="numeric"`. Labels are all `NA` but are filled in from `parse_pdf_dictionary()` when a `*Dictionary.pdf` is present.
- **PDF dictionary parser**: `parse_pdf_dictionary()` / `.parse_pdf_dict_single()` parses the standard StatCan bilingual PDF dictionary format. Variable blocks start with `<name>  Position: N  Character/Numeric(w)`. Sections: `Long name:` / `Long nom:` (variable label), `Codes:` / `Domaine:` (French equivalent) (code-value labels), `Reserved Codes:` / `Codes Réservés:` (sentinel codes → `missing_low/missing_high`), `Range:` (numeric range or code-like entries). Requires `pdftools` in Suggests.
- **PDF frequency-codebook parser**: `parse_pdf_codebook()` / `.parse_pdf_codebook_single()` parses the *other* StatCan PDF layout (CPSS 1). Variable blocks start with `Variable Name:` / `Nom de la variable :` and carry the label on the `Concept:` / `Concept :` line (wrapped continuation lines are appended until a blank line or next field key). The `Answer Categories` / `Catégories de réponse` table is parsed **from a right-anchored code-row regex** (`<label>  <code>  <numeric tail to EOL>`): the trailing block is asserted all-numeric (digits, commas, spaces, periods) so labels containing interior double-spaces can't be mis-split, and both comma-grouped (English `2,320`) and space-grouped (French `4 627`) numbers are tolerated. Continuation lines (no code+numbers) append to the previous category's label, so wrapped answer text rejoins (`"News outlets including local, national and" + "internat sources"`). Single-character labels that equal their code (`HHLDSIZC` `1`–`4`) are kept (label group allows one char). `Total` rows and the degenerate no-code rows (e.g. `VERDATE`'s lone date) are dropped. Page headers/footers are stripped with **whole-line-anchored** patterns so a header substring inside a `Concept:` value (`"Public use microdata file identifier"` vs the `Public Use Microdata File` page header) survives. All variables are typed `character` (parity with `parse_cpss_csv`); variables with no answer table (weight `COVID_WT`, id `PUMFID`, date `VERDATE`) get no codes and stay non-categorical. Requires `pdftools` in Suggests.
- **PDF user-guide frequency-dictionary parser**: `parse_pdf_freq_codebook()` / `.parse_pdf_freq_single()` parses the *third* StatCan PDF layout — the data-dictionary appendix of a PUMF user guide (GSS cycles, SGVP, PALS, SFS, Time Use). Block header: `Variable Name: X  Position: N  Length: L` (`Position` is **required** in `.pdf_freq_var_rx`, which is what separates this layout from the CPSS codebook above). The variable label is the free text between the header and whatever closes it: the table's `FREQ` header, **or** — for a variable printed without a table — the block's own trailing rule or `Coverage:`/`Source:`/`Format:` lines. The second bound is what stops the *last* block in a guide running to the end of the document (GSS Cycle 24's `WTSBS_001` documents bootstrap weight #1, prints no table, and without it produced a 78,014-character "label" that the repair pass then wrote over a sound command-file label). Table rows are `<code> <label> <FREQ> <WTD>` (French `FREQ`/`POND`); the table ends at a `======` rule or a `Coverage:`/`Source:`/`Format:`/`Weight variable:` line. **Numbers are right-aligned to the END of the `FREQ` header word**, but `pdftools`' column reconstruction drifts a few characters row to row, so the row is split by picking the number token whose **end column is nearest that anchor** (within `.pdf_freq_slack`, 8). A whitespace-run split would take the *weighted* count as the frequency whenever a label overruns into the number column (`"...support/wheelchair 1,980"`); a fixed cut plus a rightward digit-walk fails the other way — it stops at a thousands comma (GSS Cycle 26 `WLY_Q150` `9,520` → `9`) and never reaches a value starting past the anchor (`MAR_Q110` `97 Not Asked  0` → `NA`). Nearest-end selection survives both, because the weighted count sits far right (+17 against +2…+5 for the frequency) and digits inside a label sit far left. When **no** number lands near the anchor, the label has reached into the number column and pushed the weighted count onto the next line (GSS Cycle 24 Episode `SACT1` code 15, `"Domestic work (meal prep and cleanup, cleaning, laundry)         4,255"` with `"6,759,111"` alone below); the stranded number is taken only when it is unambiguous — exactly one candidate, starting at or after the anchor and **preceded by whitespace**, which is what excludes a label printed flush against both its counts (`"…cassette tapes or records3,4417,790,477"`, where neither count can be told from the other or from the label's text). Even in that unrecoverable case the digits are cut off the label, since they are certainly the number column. Symmetrically, a continuation line that is **nothing but a number** is that orphaned weighted count, not label text, and is not appended (`"… laundry) 6,759,111"`). **The code row's label is optional**: a scale labels only its endpoints (`LSR_Q110`, 0–10 satisfaction) and leaves codes 01–09 bare, and dropping those rows would leave most of the column undocumented. Continuous variables print a single `lo : hi   <freq>` **range row** in place of per-value rows. `Format:` (I2, F5.3, 4.1, A8, $CHAR2.) drives `.pdf_freq_type()` / `.pdf_freq_decimals()`. Beyond the usual `variables`/`codes` this parser also returns `freqs` (name/val/freq) and `ranges` (name/lo/hi/freq), which drive the cross-check below. Requires `pdftools` in Suggests.
- **Multi-module guides**: one user guide documents each linked module in turn, so a shared key (`RECID`, `PERSONID`) gets **one block per module**, each with that module's frequencies. Every returned table therefore carries a `block` id, and `variables` also carries `position`/`length` from the header. `.pumf_pdf_select_blocks(pdf, layout)` (in `R/pdf_repair.R`) resolves duplicated names by matching the header's position/length against the module's `layout.csv`, falling back to the first block. The eng/fra merge in `parse_pdf_freq_codebook()` joins on `block` (not `name`) when the two guides' name sequences are identical, for the same reason.
- **Metadata encoding**: default is `"CP1252"` (superset of Latin-1, handles Windows-era en-dashes and curly quotes). Exceptions: Census 2021 uses `"UTF-8"` (command files shipped as UTF-8); Census 1991 (individuals) uses `"CP850"` (DOS-era IBM Code Page 850); SHS 2017 and 2019 use `"UTF-8"` (their reading cards are UTF-8 where SHS 2021/2023 are not, so the default turned every accented French label into mojibake). The `detect_formats()` SPSS keyword scan uses `useBytes = TRUE` to tolerate non-UTF-8 bytes without warnings regardless of encoding.

### Survey registry (`R/registry.R`)

`pumf_registry_lookup(series, version)` returns per-survey configuration:
- `layout_mask` — SPSS file disambiguation for split-file surveys
- `bsw_mask`, `bsw_file_mask`, `bsw_join_key`, `bsw_drop_cols` — bootstrap weight join config
- `file_mask` — data file selector (extension determines CSV vs FWF)
- `data_encoding`, `metadata_encoding` — encoding overrides
- `data_fixups` — transformations applied before label mapping:
  - `str_pad` / `rename` / `cols_swap` — raw column transformations (`cols_swap` swaps two column names when the command file's labels are transposed relative to the data, e.g. Census 1981)
  - `rename_regex` — named character vector `c(pattern = "replacement")` rewriting many column names at once (`sub()` semantics), for releases that decorate the documented names wholesale (the PALS 2001 SAS dataset ships StatCan's *collection* names, prefixing 632 of 758 columns with "A"). A rewrite is applied only when it lands on a name the metadata declares **and** the current name is not itself declared, so it cannot collide with a correctly-named column and a stale pattern is a silent no-op. Read the field with `fixups[["rename"]]` / `fixups[["rename_regex"]]` — `$rename` partial-matches `rename_regex`.
  - `na_values` — character vector of raw string values that become `NA` in **all** columns: exact-match against numeric columns in `.apply_numeric_conversion()` and silently blanked in labeled columns by `.apply_code_labels()` (used for undeclared Census income sentinels and SAS-style `"."` missing markers)
  - `force_numeric` — variables with boundary/top-code labels alongside unlabeled continuous values; the type is forced to numeric and the codes are dropped, but first any **true-missing** sentinel codes (matching `.missing_pat`: Not stated, Don't know, Valid skip, … — not zero-value labels like "None") are converted into a per-variable `missing_low/missing_high` range so sentinel values become `NA`. An existing missing range (from `MISSING VALUES` or a split-SPSS miss file) takes precedence.
  - `force_character` / `force_integer` / `force_bigint` — character vectors of variable names whose **DuckDB storage type** is overridden. The raw string values are kept (no numeric conversion or code labeling), so geographic codes keep leading zeros and out-of-int-range IDs survive. `force_character` stays VARCHAR; `force_integer`/`force_bigint` cast the column to INTEGER/BIGINT via `ALTER COLUMN` after the table is written (an INTEGER cast that overflows 2^31 raises an error — use `force_bigint`). A variable may appear in at most one `force_*` set (incl. `force_numeric`); validated at build time and in `pumf_registry_entry()`. LFS sources its `SURVYEAR`/`SURVMNTH`/`REC_NUM` integer-forcing from the shared LFS registry entry (`.pumf_lfs_entry`) via this mechanism.
  - `codes_supplement` — per-variable extra code rows injected before label mapping (values present in data but absent from command files)
  - `missing_supplement` — explicit per-variable `c(lo, hi)` missing-range overrides for special codes no generic pattern can classify (e.g. GSS 2007 `999.5` "Child deceased")
  - `missing_codes` — named list `VAR = c(codes)` of **discrete** missing values, for variables whose sentinels do not form a single contiguous range and which the canonical `missing_low`/`missing_high` pair therefore cannot express (PALS 2006 `AUDE_Q02`: −5/−6/−7 and 998/999 straddle hours worked 1–97, so the derived `[-7, 999]` range would blank the whole column). Stage 3 clears the variable's derived/parsed range and passes the set to `.apply_numeric_conversion()`, which NAs those exact values in that column only.
  - `labels_supplement` — named list `c(VAR = c(label_en=, label_fr=))` supplying variable labels the source metadata leaves blank (e.g. CPSS 1 ships only a PDF codebook whose weight variable `COVID_WT` has an empty `Concept:` line in **both** the English and French dictionaries). Applied via `.pumf_apply_labels_supplement()` in **both** Stage 3 (`pumf_build_duckdb`) and `label_pumf_columns()`/`pumf_var_labels()` (`.pumf_read_variables_from_prov`), so a supplied label is visible everywhere; fills only `NA` labels, so genuine source labels always win. The `lang='fra'` build warning distinguishes variables that fall back to `label_en` from those with **no label in either language**.

Surveys without a registry entry use auto-detection, with one exception: when the requested version is a bare four-digit year and the same series has at least one other year-keyed entry, `pumf_registry_lookup()` inherits the config of the newest sibling whose year is ≤ the requested year (or the oldest sibling if the year predates them all). This lets a freshly released year deposited in the cache reuse the prior year's config — which now works cleanly because recent `file_mask`s use a generic `\d{4}` year. A `message()` fires once per session so the implicit reuse is discoverable (`.pumf_registry_newest_sibling()` / `.pumf_registry_inherit_announced`); a genuinely changed release still needs its own explicit entry. Inheritance is skipped for multi-part versions (Census `2021 (individuals)`) and for LFS (which has its own shared entry).

#### Version aliases (`pumf_resolve_version`)

`pumf_resolve_version(series, version)` canonicalises user-supplied version strings before lookup. Two series-specific maps:
- **Census**: any string starting with a four-digit year is parsed flexibly (file type by grepping `hierarchical`/`household`/`famil`; CMA vs provincial by `cma`) into the canonical registry key.
- **GSS** (`.pumf_gss_alias()` / `.pumf_gss_aliases`): GSS cycles are referenced by cycle number or theme name, but the caregiving series is keyed by plain year. Cycle 16 ("Aging and Social Support", 2002) resolves from `"Cycle 16"`/`"16"`/`"cycle16"`/`"Aging and Social Support (2002)"` to canonical `"2002"`. Matching is case-insensitive after stripping punctuation, inserting a space between trailing letters and digits, and collapsing whitespace. StatCan's catalogue files cycle 16's PUMF (zip `cat9/c16_2002.zip`) under the Education category and mislabels it "Education 2002"; `list_gss_collection()` relabels that one entry to canonical `"2002"` (matched by the `c16_2002.zip` filename) so it stays auto-downloadable via `get_pumf("GSS", "2002")` / `"Cycle 16"`. The mislabel string itself is dropped, not aliased.

### Override verification workflow

**Every manual registry override must be verified against the survey's official documentation** (PDF codebook/user guide, or the SPS command file when it is the authoritative source) **and recorded in the ledger** `tests/testthat/override_verification.csv`. The test `test-override-verification.R` enumerates all registry overrides (via `tests/testthat/helper-overrides.R`) and fails when an override is missing from the ledger, has `pending`/`mismatch` status, or when the ledger has stale rows for removed overrides.

Workflow (driven from `tools/verify_overrides.R`, which is `.Rbuildignore`d):

1. `devtools::load_all(); source("tools/verify_overrides.R")`
2. `vo_overrides(pending = TRUE)` — list overrides that still need checking.
3. `vo_find_pdfs(series, version)` — candidate PDFs in the cache (codebooks/dictionaries sort first).
4. `txt <- vo_source_lines(pdf)` — extract text via `pdftools::pdf_text()`, one element per line; line numbers are stable across sessions. For scanned PDFs without a text layer (1971/1981/1986 Census), render with `pdftoppm -r 300 -gray -png` and OCR with the `tesseract` CLI, then record page numbers with an "(OCR)" suffix.
5. `vo_locate(txt, "VARNAME")` then `vo_context(txt, line)` — find and *read* the variable's section; confirm the imputed value/label/type against the documentation text.
6. `vo_record(series, version, override_type, variable, value, source_file, source_lines, status, note)` — append/update the ledger row. Statuses: `confirmed` (documentation supports the override), `unverifiable` (no usable documentation; explain the supporting evidence in `note`, e.g. field-aligned data scans), `mismatch` (documentation contradicts the registry — fix the registry, then re-record).

Notes should be self-contained: cite what the documentation says (e.g. `"PDF: 8 = Not available / Non disponible"`), and when the evidence is a data observation, say so explicitly. When a French label is asserted but only the English PDF is machine-readable, note that the French label is the standard StatCan equivalent.

### PDF cross-check and label repair (`R/pdf_repair.R`)

StatCan's command files routinely ship **truncated** value and variable labels — hard cuts at 60 characters, dropped leading text, dropped interior text. The damage is upstream of the flavour-specific renderers (SAS/SPSS/Stata carry byte-identical text), so it cannot be dodged by parsing a different flavour. The same survey's user guide carries the full text, typeset from the metadata before the command files were generated.

Trusting a PDF scrape over a machine-readable command file would normally be a bad trade. What makes it a good one here is that this PDF layout prints the **frequency of every code**, so the parse can be reconciled against the actual data file before any of it is believed. `.pumf_pdf_crosscheck()` runs at the end of `pumf_parse_metadata()`, **after** `merge_metadata()`:

0. `.pumf_pdf_choose_candidates(pdf_paths, layout)` picks *which* guide to read. Detection ranks candidates by block count, but a release that ships both its original and a revised user guide (PALS 2006) has two guides with near-identical block counts describing **different** field positions — only the layout tells them apart. Each candidate's `Position:`/`Length:` headers are scored against `layout.csv` and the best-scoring one wins per language, but only when it clears `.pumf_pdf_min_pos_rate` (0.9), so a guide printing no positions at all cannot displace one that parses more blocks. PALS 2006: the pre-revision guide scores 3/746, the Dec 2011 revision 746/746.
1. `parse_pdf_freq_codebook()` → `.pumf_pdf_select_blocks()` (module disambiguation).
2. `.pumf_validate_pdf_freqs(pdf, layout, data_path)` tabulates each documented variable in the microdata and compares counts. Statuses: `validated` (every code's count matches and the data holds no undocumented values), `continuous` (the sentinel codes match and the remaining values are accounted for by the `lo : hi` range row), `mismatch` (counts disagree), `unchecked` (no data file / variable not in this module's data / column empty / file > 500 MB / **any of the block's printed counts unreadable**). The last of those is missing evidence, not contrary evidence: comparing against an `NA` count made `all()` return `NA` and the `if` that followed abort the whole Stage 2 run, and reporting `unchecked` withholds nothing, since repairs are barred only by an outright `mismatch`. Written to `metadata/pdf_validation.csv`.
3. **Document-level verdict.** Whether the guide describes this file at all is a question about the *document*, not about any one variable, and there are two independent corroboration channels: do the printed counts reproduce a tabulation of the data (`.pumf_pdf_min_freq_rate`, 0.5), and does `.pumf_pdf_position_agreement()` reproduce the command file's layout (`.pumf_pdf_min_pos_rate`, 0.9)?
   - **Both fail** → wrong document. The validation table is still written (it is the evidence for the decision) but the repair ledger is written empty and the function returns, so nothing downstream reads divergences off a rejected guide.
   - **Frequencies fail, positions pass** → right document, different tabulation base. PALS 2006's frequencies are computed over the disability sub-population (guide `SEX` 9422+8001 = 17423 = the `DISAB=1` count, against 72167 rows in the file), so no count matches even though all 746 positions do. The counts carry no per-variable information, so `mismatch` is downgraded to `unchecked` ("guide frequencies use a different population") **for the blocks the layout confirms only** — blocks the layout does not confirm keep their `mismatch` and stay barred from repair.
4. `.pumf_apply_pdf_repairs()` compares every variable and value label. `.pumf_repair_action()` returns `fill` (command file had no label), `repair` (the guide's text **demonstrably extends** the command file's — either a strict superstring, or a ≥8-char subsequence sharing a ≥4-char prefix or suffix, which is what catches the interior-drop pattern `"Single-ded house" ⊂ "Single-detached house"`), `flag` (they differ but the guide does not extend), or `ok`. A repair is withheld **only** on `status == "mismatch"` — a variable the frequency check could not reach is not evidence against the parse, and the repair rule is self-corroborating. Every ledger row carries the variable's `validation` status so uncorroborated repairs stay visible. Written to `metadata/label_repairs.csv`.

**The truncation fingerprint** (`.pumf_truncation_width()` / `.pumf_at_truncation()`) gates every `repair`. The string shapes alone cannot separate the two reasons a guide's text can be longer than the command file's:

- GSS Cycle 16 truncates at 60 characters — 1,665 of its 1,860 variable labels sit at 59–60, against 42 in the six lengths below. The guide holds the missing tail; replacing the label is a genuine repair.
- SGVP 2007 and PALS 2006 print the **question wording** where the command file gives a hand-written short label (`"How many hours do you work per week"` vs `"How many hours do you (does ....) usually work per week?"`). These are different fields, not a damaged one, and the abbreviation is often a subsequence of the question — so the shape test alone would "repair" a perfectly good label into a question.

A hard cut leaves a spike at the ceiling; a hand-abbreviated label set thins out towards its longest entry. Comparing the top two lengths against the six below them separates them cleanly: GSS Cycle 16 40–62× (variables) and 1.4–6.0× (codes), GSS Cycle 26 10.1×/3.5×, PALS codes 2.9× — against SGVP 0.45×, CHS 0.39×, CIS 0.41×, CPSS 0.33×, SFS 0.04×. The width is computed **per label field from the command file itself** (variables and codes separately, both languages pooled), needs ≥20 non-empty labels, and returns `NA` where there is no ceiling.

`.pumf_truncation_width()` only sees labels cut at a **fixed ceiling**. The other damage pattern — **dropped leading text** — leaves a short label instead, well below any ceiling, so `.pumf_left_truncated()` supplies a second signature: the command-file text is a strict suffix (≥8 chars) of the guide's (`"relative in a family farm or business?"`, `"foot or bus)"`, `"les réserves indiennes)"`).

The suffix shape alone over-fires, because two things that are *not* damage produce it as well — and both are rejected by `.pumf_annotation_prefix()`, which looks at the **dropped text** rather than at what survived:

- **Editorial notes.** SGVP 2007 prints `"Grouped variable: Age group"` where the command file has `"Age group"`, `"Variable groupée : Groupe d'âge"` for `"Groupe d'âge"`.
- **Scraped field furniture.** GSS Cycle 16's guide leaks its own header into the text: `"Longueur : 2 Age du répondant la dernière fois qu'il a pris sa retraite."`.

The test is whether the dropped text contains a colon: StatCan's guides write these as `Key: value`, while text lost to truncation is running prose. Judging the dropped text is what makes the mid-list case work — `"Co-worker of respondent and Other relatives)"` (of `"Other (Do not include organizations here) (Includes Ex-spouse/Ex-partner/Same sex partner/ …"`) starts with a capital exactly as an intact label would, so **nothing about the survivor separates it from an editorial prefix**. This is the value label that motivated the whole cross-check: what survives reads like a category about co-workers rather than the "Other" bucket it is.

The veto **outranks both fingerprints** in `damaged()`, not just the suffix one: the furniture case sits at the ceiling too, so the width test alone would append the guide's header to a complete label.

Net effect of the fingerprints and the veto together: SGVP went from 35 repairs to 1 (that one a real dropped prefix, `"3 or 4 times a year"` of `"At least 3 or 4 times a year"`) and PALS from 245 to 100, while GSS Cycle 16 kept 2,860 of 2,865.

Codes the guide documents but the command file never declared are **reported, not injected** — that is a registry `codes_supplement` decision. Sentinels already covered by the variable's `missing_low`/`missing_high` range are not reported at all, and the report is restricted to variables the command file treats as **categorical** (i.e. that appear in `codes.csv`): where it declares no codes the variable is continuous, and the guide's zero-value rows ("No hours", "None", "Aucun don") label a valid numeric zero rather than a code the command file forgot. This removed 120 spurious flags on SGVP alone.

Detection (`.pumf_detect_freq_pdfs()`, `detect_formats()` section 10) is gated on `getOption("canpumf.pdf_crosscheck", TRUE)` and on `pdftools` being installed. It shortlists PDFs by path pattern, prefers `layout_mask`-matching paths for multi-module surveys, caps at 8 files > 50 KB sorted by size, and content-verifies (≥10 block headers **and** a FREQ column) before use. eng/fra are told apart by whether `Nom de la variable` outnumbers `Variable Name`.

**The frequency dictionary is deliberately kept out of `merge_metadata()`** so the command file stays authoritative; it becomes a primary source only when no other parser fired at all (in which case the `block`/`position`/`length` bookkeeping columns are stripped first).

User-facing: `pumf_label_repairs(tbl, action = NULL)` and `pumf_freq_validation(tbl)` read the two side-car CSVs (module-aware via `.pumf_meta_dir_from_tbl()`; both return zero rows for LFS and for surveys with no such guide).

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
- `R/metadata_parsers.R` — all nine parsers, `detect_formats()`, `merge_metadata()`, `pumf_parse_metadata()`, `read_metadata()`, `write_metadata()`
- `R/pdf_repair.R` — user-guide PDF cross-check: `.pumf_pdf_select_blocks()`, `.pumf_validate_pdf_freqs()`, `.pumf_apply_pdf_repairs()`, `.pumf_pdf_crosscheck()`, and the exported `pumf_label_repairs()` / `pumf_freq_validation()`
- `R/helpers.R` — `robust_unzip()`, import declarations
- `R/pumf_collection.R` — `list_canpumf_collection()`, `list_available_lfs_pumf_versions()`
- `R/pumf_documentation.R` — `open_pumf_documentation()`
- `R/pumf.R` — `read_pumf_data()` (manual-deposit use case); `get_pumf_connection()` (internal, unexported)

### Bootstrap weight functions

- `add_bootstrap_weights(tbl, weight_col, ...)` — DuckDB-backed or in-memory; replaces old `add_pumf_bootstrap_weights`
- `remove_bootstrap_weights(tbl, weight_col)` — removes BSW table and companion view
- `bsw_info(tbl)` — summarises BSW tables present in the DuckDB file
