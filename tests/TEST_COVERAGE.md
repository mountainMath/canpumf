# Test Suite Coverage

This document describes what the `canpumf` test suite covers.  Keep it in
sync whenever a new survey version is added to the registry, a new test file
is added, or the scope of an existing test changes.

---

## Unit tests (always run, no cache required)

| File | What it covers |
|---|---|
| `test-parse-lfs-codebook.R` | LFS `*codebook.csv` parser |
| `test-parse-cpss-csv.R` | CPSS `variables.csv` parser |
| `test-parse-sas-cards.R` | SAS reading cards parser (`.lay` + `.lbe`) |
| `test-parse-spss-mono.R` | SPSS monolithic parser (`parse_spss_mono`) |
| `test-parse-spss-split.R` | SPSS split-file parser (`parse_spss_split`) |
| `test-parse-spss-sav.R` | SPSS binary `.sav` parser (via haven) |
| `test-merge-metadata.R` | `merge_metadata()` conflict resolution |
| `test-metadata-io.R` | `read_metadata()` / `write_metadata()` round-trips |
| `test-bilingual.R` | Label selection, fallback, `check_labels()` |
| `test-factor-enum.R` | Factor → DuckDB ENUM encoding |
| `test-cache-mgmt.R` | Cache path resolution, version extraction checks |
| `test-registry.R` | `pumf_registry_lookup()` for all registered surveys |
| `test-override-verification.R` | Every manual registry override (`force_numeric`, `na_values`, `cols_swap`, `rename`, `codes_supplement`, `missing_supplement`) has a `confirmed`/`unverifiable` row in `override_verification.csv`; no stale ledger rows; confirmed rows carry a source file and date. See "Override verification workflow" in CLAUDE.md |
| `test-pipeline-stage1.R` | `pumf_locate_or_download()` (download, unzip, collision handling) — uses `skip_if_offline()` for download tests; download attempts wrapped in `tryCatch` → `skip()` so StatCan downtime produces a skip, not a failure |
| `test-pipeline-stage3.R` | Stage 3 helpers: `.find_pumf_data_file()`, `pumf_build_duckdb()` end-to-end with synthetic data |
| `test-api.R` | `get_pumf()`, `label_pumf_columns()`, `close_pumf()`, `pumf_metadata()` — mostly synthetic; some use `skip_if_offline()`. Includes the multi-module announcement (`.pumf_announce_modules()` lists sibling modules once per survey; silent for single-module surveys) — registry-only, no cache |
| `test-connection-pane.R` | `.duckdb_connect_quiet()` keeps transient internal connections out of the RStudio Connections pane (only the final returned connection registers); guards the `dbSendQuery` pane-popup regression |
| `test-bsw-and-helpers.R` | `add_bootstrap_weights()` (in-memory + DuckDB), `remove_bootstrap_weights()`, `bsw_info()`, `pumf_var_labels()`, collection listers. Bootstrap-weight coverage spans every incremental re-run branch: reuse (no recompute), extend columns (unstratified + within-strata), added rows → full regen (unstratified) / affected-strata-only regen (stratified), combined rows+columns, and `overwrite=TRUE` full regen. Stratified resampling is verified via the constant-weight within-stratum total invariant |

---

## Integration tests (cache-gated, real data)

These tests skip silently when the relevant survey data is not in the local
cache.  They are the primary regression guard against encoding bugs, parser
regressions, and sentinel-detection changes.

### Test categories

| Symbol | Category | Description |
|:---:|---|---|
| **W** | Warnings | Full Stage 2+3 pipeline in a fresh temp DuckDB, `refresh=TRUE`; fails on any unexpected warning |
| **L** | Labels | Metadata has non-empty English and French variable/code labels |
| **P** | Parity | eng and fra tables built fresh (`refresh=TRUE`); same columns, row counts, numeric values, NA patterns; at least one categorical column has different labels |

### Coverage matrix

Each row is one "verified set" — the loop variable or helper that controls
which cached versions are exercised.

| Survey | File | Versions tested | W | L | P | Notes |
|---|---|---|:---:|:---:|:---:|---|
| Census | `test-pipeline-census.R` | 2021 (ind.), 2021 (hier.), 2016 (ind.), 2016 (hier.), 2011 (ind.), 2011 (hier.), 2006 (ind.), 2006 (hier.), 2001 (ind./hh/fam), 1996 (ind./hh/fam), 1991 (ind./hh/fam) | ✓ | ✓ | ✓ | 2021 (ind.) warns "no French translation" for 74/144 variables — allowed |
| Census (EFT) | `test-pipeline-census.R` | 1986 (ind./hh/fam), 1981 (ind./hh), 1976 (ind./hh/fam), 1971 (ind./hh/fam — prov and cma variants) | ✓ | ✓ | ✓ | 1986/families, 1976/households, 1976/families: English-only (no French labels); 1981/individuals warns swapped column names; 1971 (cma-individuals, prov-families): warns "absent from command files" for codes_supplement injections |
| GSS (Caregiving) | `test-pipeline-gss.R` | Cycle 32 (2018), Cycle 26 (2012), Cycle 21 (2007), Cycle 16 (2002), Cycle 11 (1996) | ✓ | ✓ | ✓ | Canonical GSS keys are `Cycle N (YYYY)`; cycle number, bare year, and theme name all resolve as aliases (e.g. `2002`, `16`, `Aging and Social Support`). Cycle 21 (2007) warns "no French translation" for ~925/951 variables — allowed; it also subsumes the former "Education 2007" entry (same c21_2007 data). Cycle 16 (2002) = "Aging and Social Support", a multi-module survey (MAIN + CG4 + CG6 + CR linked on RECID); primary module MAIN carries WGHT_PER; English-only code labels (bilingual parity skipped) |
| GSS (Safety) | `test-pipeline-gss.R` | Cycle 34 (2019), Cycle 28 (2014), Cycle 13 (1999), Cycle 8 (1993) | ✓ | ✓ | ✓ | All have force_numeric boundary-label variables; theme aliases `Safety <year>` resolve to these cycles |
| GSS (Family) | `test-pipeline-gss.R` | Cycle 31 (2017), Cycle 25 (2011), Cycle 15 (2001), Cycle 10 (1995) | ✓ | ✓ | ✓ | All have age/family-history force_numeric; Cycle 25 (2011) also warns WTBS unlabeled; theme aliases `Family <year>` resolve to these cycles |
| GSS (Social Identity) | `test-pipeline-gss.R` | Cycle 35 (2020), Cycle 27 (2013), Cycle 17 (2003) | ✓ | ✓ | ✓ | Cycle 17 (2003)/Cycle 27 (2013) have many force_numeric; Cycle 17 (2003) warns WTBS unlabeled; theme aliases `Social Identity <year>` resolve to these cycles |
| GSS (Education) | `test-pipeline-gss.R` | Cycle 9 (1994) | ✓ | ✓ | ✓ | English-only command files (no French labels); the former "Education 2007" merged into Cycle 21 (2007) — same data. Alias `Education 1994` resolves here |
| GSS (Time Use) | `test-pipeline-gss.R` | Cycle 36 (2022), Cycle 29 (2015), Cycle 24 (2010), Cycle 12 (1998) | ✓ | ✓ | ✓ | Multi-module: each cycle ships a respondent-level **Main** file plus an **Episode** file (one row per activity episode), built as two linked tables joining on the cycle key (PUMFID for Cycle 36 (2022)/Cycle 29 (2015), RECID for Cycle 24 (2010)/Cycle 12 (1998)) via `pumf_module(tbl,"Episode")`; Cycle 36 (2022) Episode uses force_numeric (ACTIVITY/LOCATION/TUI_01/TUI_03); Cycle 24 (2010) warns WTBS_EPI unlabeled. The 2022 file (TU_ET_2022.zip, no cycle prefix) is mapped to cycle 36 explicitly; theme alias `Time Use <year>` resolves to these cycles |
| SFS | `test-pipeline-sfs.R` | 2023, 2019, 2016, 2012, 2005 | ✓ | ✓ | ✓ | BSW join tested for 2016/2019/2023 |
| SFS 1999 | `test-pipeline-sfs.R` | 1999 | ✓ | ✓ | — | DATA LIST-only SPSS + PDF dictionary (`parse_pdf_dictionary`) for labels; English-only (no French PDF bundled in the download); row count (15 933) and column count (80) asserted; PDF-derived labels verified |
| CHS | `test-pipeline-chs.R` | First of: 2022, 2021, 2018 | ✓\* | — | ✓ | Stage 2 smoke-test; BSW join tested; bilingual tests use whichever version is in cache |
| SGVP | `test-pipeline-sgvp.R` | 2023, 2018, 2013, 2010, 2007, 2004, 2000, 1997 | ✓ | ✓ | ✓ | 2018 warns codes_supplement injection for BRTHMACR — allowed; 2013 asserts the force_numeric top-code ranges (HSDSIZEC 1–6, CHH0014C 0–3) verified in `override_verification.csv`. Multi-module (older cycles): MAIN + detail tables joining on the cycle key — 2010/2007/2004 add **GS** (giving, PUMFID), 2000 adds **GS + VD** (volunteer detail, MICRO_ID), 1997 adds **GIVE + VOLNTR** (IDNUM); `pumf_module(tbl,"GS")` etc. |
| CCAHS | `test-pipeline-ccahs.R` | 1 | ✓ | ✓ | ✓ | BSW join tested (PUMFID key, WGT_PUMF dropped from BSW side) |
| ITS | `test-pipeline-its.R` | 2018, 2019 | ✓ | ✓ | ✓ | |
| SHS | `test-pipeline-shs.R` | First of: 2023, 2021, 2019, 2017 | ✓\* | — | ✓ | Stage 2 smoke-test; BSW join tested. 2017 is multi-module: **Interview** (primary) + **Diary** (one row per purchase) joining on CASEID, each with its own BSW flatfile (per-module BSW); `pumf_module(tbl,"Diary")` |
| CIS | `test-pipeline-cis.R` | First of: 2022, 2021, 2020, 2019, 2018, 2017 | ✓\* | — | ✓ | Stage 2 smoke-test |
| CPSS | `test-pipeline-cpss.R` | v1 (full, via PDF codebook†); v2–6 (full) | ✓ | — | ✓ | Cache-gated (`skip_if_not(.cpss_extracted(), ...)`); †v1 has no machine-readable codebook — `parse_pdf_codebook()` recovers bilingual labels from the codebook PDF, so Stage 2/3 run when `pdftools` is installed (`skip_if_not_installed("pdftools")`); Stage 1 download tested in `test-pipeline-stage1.R` |
| LFS | `test-pipeline-lfs.R` | Synthetic (all); real cache if present | ✓\* | — | — | 47 tests; synthetic fixtures cover schema evolution, ENUM types, version filters, fra labels |

\* For CHS, SHS, CIS, and LFS the **W** category is "emits no warnings during
`pumf_run_pipeline()`" rather than the full `refresh=TRUE` loop used for
Census/GSS/SFS.

---

## The bilingual parity helper

`helper-bilingual-parity.R` defines two shared helpers loaded automatically
by testthat:

- **`expect_pumf_bilingual_parity(eng, fra, label="")`** — asserts that two
  collected data frames (one per language) have the same column names, row
  count, column types, numeric values, and NA patterns for every categorical
  column, and that at least one categorical column has different label strings.
  Called by every survey that has full label coverage (see **P** column above).

- **`.collect_pumf_table(db_path, table_name)`** — opens a DuckDB table,
  collects it, and disconnects.  Convenience wrapper used by all parity tests.

---

## Keeping this file in sync

When you add a new survey or version to the test suite:

1. Add a row to the coverage matrix above.
2. Update the **Verified datasets** table in `README.md`.
3. Confirm there is a matching registry entry in `R/registry.R`.

When you add a new test category (e.g. a new assertion in the per-version
loop), add a column to the matrix and update the symbol legend.
