# Survey registry, version aliases, download resolution, override verification

Back to [CLAUDE.md](../CLAUDE.md). Code: `R/registry.R` (entries, lookup, aliases), `R/statcan_catalogue.R` + `R/pumf_collection.R` (download URLs).

## Registry entries

`pumf_registry_lookup(series, version)` returns the per-survey configuration. Entries are built with `.make_entry()`. The user-facing counterparts are `pumf_registry()` (inspect an entry), `list_pumf_registry()` (overview) and `pumf_registry_entry()` (build a custom entry and pass it as `get_pumf(..., registry =)`).

- `layout_mask`: disambiguates SPSS/SAS files for split-file surveys. It also becomes part of the DuckDB table name.
- `bsw_mask`, `bsw_file_mask`, `bsw_join_key`, `bsw_drop_cols`, `bsw_strata`: how the bootstrap weights are joined.
- `file_mask`: selects the data file. Its extension decides CSV vs FWF (see "Data file detection" in CLAUDE.md).
- `data_encoding`, `metadata_encoding`: encoding overrides. The known exceptions are listed in [metadata-parsers.md](metadata-parsers.md#encoding).
- `modules`, `module_key`: multi-module surveys. See [multi-module.md](multi-module.md).
- `data_fixups`: transformations applied before labels are mapped (below).
- `borealis`: `list(doi =, files =)`, a Borealis Dataverse source for the version (see [Borealis](#borealis-dataverse-source) below). `files` optionally pins file names; otherwise `.borealis_select_files()` picks them.

### `data_fixups`

- `str_pad`, `rename`, `cols_swap`: raw column transformations. `cols_swap` swaps two column names when the command file's labels are transposed relative to the data (Census 1981).
- `rename_regex`: a named vector `c(pattern = "replacement")` with `sub()` semantics. It handles releases that decorate names wholesale; the PALS 2001 SAS dataset prefixes 632 of 758 columns with "A". A rewrite is applied only when the new name is declared in the metadata **and** the current name is not. It therefore cannot collide with a correctly named column, and a stale pattern silently does nothing. **Read it with `fixups[["rename"]]` / `fixups[["rename_regex"]]`**, because `$rename` partial-matches `rename_regex`.
- `na_values`: raw string values that become `NA` in **all** columns. Numeric columns use an exact match in `.apply_numeric_conversion()`, and labelled columns blank the value in `.apply_code_labels()`. Used for undeclared Census income sentinels and SAS `"."`.
- `force_numeric`: for variables whose boundary or top-code labels sit alongside unlabelled continuous values. The variable is typed numeric and its codes are dropped. Before that, any **true-missing** sentinel codes (`.missing_pat`; not zero-value labels) become its `missing_low`/`missing_high` range. An existing range from `MISSING VALUES` or a split-SPSS miss file takes precedence. **Labels win**: `.fully_labelled_vars()` drops the override for any variable whose every non-empty data value is a labelled code (compared numerically, and with the layout's implied decimals for fixed-width data). Such a variable stays categorical, because only unlabelled values justify numeric. So a stale or over-broad list cannot strip real categories (GSS Cycle 17 once forced 236 of them), and a shared fixup can be numeric in one file and a factor in another (1971 Census SUBSAMPL: an unlabelled 0 in the household files, labelled ONE-FIVE elsewhere). After promotion, any variable that is still numeric also has its labelled missing codes set to NA as discrete codes (see `.label_missing_codes()` in metadata-parsers.md).
- `force_character` / `force_integer` / `force_bigint`: override the DuckDB **storage type** and keep the raw strings, with no numeric conversion or labelling. Geographic codes therefore keep their leading zeros, and IDs outside the integer range survive. `force_integer`/`force_bigint` cast via `ALTER COLUMN` after the write, and an INTEGER overflow past 2^31 raises an error (use `force_bigint`). A variable may be in at most one `force_*` set, including `force_numeric`. This is validated at build time and in `pumf_registry_entry()`. LFS gets its `SURVYEAR`/`SURVMNTH`/`REC_NUM` integer forcing from the shared `.pumf_lfs_entry` this way.
- `codes_supplement`: extra code rows per variable, for values present in the data but missing from the command files.
- `missing_supplement`: an explicit `c(lo, hi)` missing range per variable, for special codes no generic pattern can classify (GSS 2007 `999.5` "Child deceased").
- `missing_codes`: `VAR = c(codes)`, for **discrete** missing values that do not form a single contiguous range. Example: PALS 2006 `AUDE_Q02` has −5/−6/−7 and 998/999 around valid hours 1–97, so the derived range `[-7, 999]` would blank the whole column. Stage 3 clears the variable's range and NAs those exact values only.
- `labels_supplement`: `c(VAR = c(label_en=, label_fr=))`, for labels the source leaves blank. Example: CPSS 1 `COVID_WT` has an empty `Concept:` line in both languages. It is applied via `.pumf_apply_labels_supplement()` both in Stage 3 and in `label_pumf_columns()`/`pumf_var_labels()` (`.pumf_read_variables_from_prov`). It fills only `NA` labels, so a genuine source label always wins. The `lang='fra'` build warning distinguishes an English fallback from a variable with no label in either language.

### Sibling inheritance for unregistered years

A survey without an entry falls back to auto-detection, with one exception. If the requested version is a bare four-digit year and the series has other year-keyed entries, `pumf_registry_lookup()` inherits the config of the newest sibling whose year is ≤ the requested one (or the oldest sibling if the year is earlier than all of them). This lets a newly released year reuse the previous year's config, which works because recent `file_mask`s use a generic `\d{4}`. A `message()` fires once per session (`.pumf_registry_newest_sibling()`, `.pumf_registry_inherit_announced`). A release that genuinely changed still needs its own entry. Inheritance is skipped for multi-part versions (`2021 (individuals)`) and for LFS.

## Version aliases: `pumf_resolve_version(series, version)`

Called before every lookup to turn the user's version string into the canonical key.

- **GSS**: canonical keys are `"Cycle N (YYYY)"`. `.pumf_gss_alias()` generates `Cycle N`, bare `N`, bare `YYYY` and `Cycle N YYYY` for each canonical key. It layers on `.pumf_gss_theme_aliases`, which holds theme names and the historical registry keys (`"Family 2017"`, `"Aging and Social Support 2002"`, `"Education 2007"`). Matching is case-insensitive after stripping punctuation, splitting `cycle16` → `cycle 16`, and collapsing whitespace. SGVP is a separate series with plain-year keys.
- **CPSS / CCAHS**: `.pumf_cycle_alias()` maps `Series N`/`Cycle N`/`CPSS N` → bare `N`. The keys are cycle numbers, because reference years collide. A bare year is deliberately **not** a CPSS alias.
- **Census**: any string starting with a four-digit year is parsed flexibly. The file type comes from grepping for `hierarchical`/`household`/`famil`, and CMA vs provincial from `cma`. For 1971–1986 two keys exist per file: the EFT key (`"1971/individuals_cma"`, `"1986/families"`) and the Borealis key (`"1971 (individuals, CMA)"`, `"1986 (families)"`). The keyword `eft` or `borealis` in the version string forces one. Otherwise the EFT key wins only when `.census_eft_bundle_present()` finds the bundle (zip, extracted raw files, or an existing EFT build) under `<cache_path>/Census/<year>/`; with no bundle the Borealis key is returned. So `pumf_resolve_version()` takes `cache_path`, and tests of bare "1971" resolution must use a temp cache.

## Download URL resolution (Stage 1)

`pumf_locate_or_download()` calls `.pumf_resolve_collection_row(series, version)`, which works as follows:
- For series in `.statcan_supported_series` (GSS, SHS, SFS, CPSS, CIS, CHS, ITS, CCAHS), it tries the scraped StatCan catalogue first (`.pumf_adapter_collection()` → `.statcan_catalogue_cached()`). The lookup order is session cache → user-persisted `<cache_path>/pumf_catalogue.rds` → shipped `inst/extdata/pumf_catalogue.rds`. It **never** crawls live.
- On a miss, and for other series (LFS, Census, SGVP), it falls back to the curated `list_canpumf_collection()`.

StatCan files GSS cycle 16's `c16_2002.zip` under Education. The zip filename still yields `Cycle 16 (2002)`.

The snapshot is regenerated with `tools/refresh_catalogue_snapshot.R`, and `list_statcan_pumf_catalogue()` is the exported crawler.

## Borealis Dataverse source

Code: `R/borealis.R`. [Borealis](https://borealisdata.ca) hosts the ODESI PUMF collection (the `pumfs` dataverse, plus PUMF titles in `census`). StatCan stays primary; Borealis is used when:
- a registry entry carries `borealis` and StatCan has no download for it (the 14 Census 1971–1986 keys; these have no `list_canpumf_collection()` row, and `.borealis_registry_collection()` lists them), or
- the user passes `get_pumf(series, version, borealis = <doi or catalogue row>)`. This becomes a registry override: when the version's built-in entry points at the same DOI its fixups are kept, otherwise the entry is replaced by auto-detection (combine with `registry =` to supply fixups). An already-cached version from another source is only replaced with `redownload = TRUE`.

Stage 1 (`pumf_locate_or_download()`) downloads via `/api/access/datafile/<id>` into a flat version directory and writes `borealis_manifest.csv` (doi, file id, name, role). `.borealis_select_files()` picks one data file (CSV preferred; else FWF with a `.sav`/`.sps`), the `.sps`/`.sas` command files, and documentation under `canpumf.borealis_max_doc_mb` (default 50); SAS/Stata/tab copies and ODESI `missRecode` files are skipped. Stage 3 takes its `file_mask` from the manifest (`.borealis_manifest_file_mask()`), since ODESI datasets also ship FWF copies and text codebooks that would be data-file candidates; the PDF cross-check uses the same fallback.

Browsing: `list_borealis_pumf_catalogue()` (Dataverse search API, session-cached and persisted to `<cache_path>/borealis_catalogue.rds`, staleness warning like the StatCan catalogue) and `list_borealis_pumf_files(doi)`. `BOREALIS_DATAVERSE_KEY`, when set, is sent as `X-Dataverse-key` to `BOREALIS_SERVER` only.

The Borealis Census copies are English-only (ODESI `.sps`). Their overrides differ from the EFT twins: no `cols_swap` for 1981 (ODESI fixed the names), no ETHNICOR supplement for 1986, and 1986 families gets `force_numeric` (the ODESI `.sps` declares labels and MISSING VALUES the EFT family file lacks). The 1971 Borealis CSV carries correct negative incomes, whereas the EFT text files use sign overpunch that the FWF reader does not decode.

## Override verification workflow

**Every manual registry override must be checked against the survey's official documentation and recorded in the ledger `tests/testthat/override_verification.csv`.** The documentation is the PDF codebook or user guide, or the SPS command file when that is authoritative. `test-override-verification.R` enumerates all overrides via `tests/testthat/helper-overrides.R`. It fails when:
- an override has no ledger row,
- a row has status `pending`/`mismatch`, or
- a row is stale (its override was removed).

The workflow is driven from `tools/verify_overrides.R` (`.Rbuildignore`d):

1. `devtools::load_all(); source("tools/verify_overrides.R")`
2. `vo_overrides(pending = TRUE)`: lists the overrides that still need checking.
3. `vo_find_pdfs(series, version)`: lists candidate PDFs, with codebooks and dictionaries first.
4. `txt <- vo_source_lines(pdf)`: extracts the text via `pdftools::pdf_text()`, one line per element, with stable line numbers. Scanned PDFs (1971/1981/1986 Census) have no text layer. For those, run `pdftoppm -r 300 -gray -png` + the `tesseract` CLI and record page numbers with an "(OCR)" suffix.
5. `vo_locate(txt, "VARNAME")` then `vo_context(txt, line)`: *read* the variable's section and confirm the value, label and type.
6. `vo_record(series, version, override_type, variable, value, source_file, source_lines, status, note)`. Statuses:
   - `confirmed`: the documentation supports the override.
   - `unverifiable`: there is no usable documentation. Explain the evidence in `note`.
   - `mismatch`: the documentation contradicts the registry. Fix the registry, then re-record.

Notes must be self-contained. Quote the documentation (e.g. `"PDF: 8 = Not available / Non disponible"`), and say explicitly when the evidence is a data observation. When the French label is asserted but only the English PDF is machine-readable, say that the French label is the standard StatCan equivalent.
