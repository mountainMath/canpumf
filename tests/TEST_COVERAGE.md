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
| `test-api.R` | `get_pumf()`, `label_pumf_columns()`, `close_pumf()`, `pumf_metadata()` — mostly synthetic; some use `skip_if_offline()` |

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
| GSS | `test-pipeline-gss.R` | 2018, 2012, 2007, 1996 | ✓ | ✓ | ✓ | 2007 warns "no French translation" for ~925/951 variables — allowed |
| SFS | `test-pipeline-sfs.R` | 2023, 2019, 2016, 2012, 2005 | ✓ | ✓ | ✓ | BSW join tested for 2016/2019/2023 |
| SFS 1999 | `test-pipeline-sfs.R` | 1999 | ✓ | ✓ | — | DATA LIST-only SPSS + PDF dictionary (`parse_pdf_dictionary`) for labels; English-only (no French PDF bundled in the download); row count (15 933) and column count (80) asserted; PDF-derived labels verified |
| CHS | `test-pipeline-chs.R` | First of: 2022, 2021, 2018 | ✓\* | — | ✓ | Stage 2 smoke-test; BSW join tested; bilingual tests use whichever version is in cache |
| SGVP | `test-pipeline-sgvp.R` | 2023, 2018, 2013, 2010, 2007, 2004, 2000, 1997 | ✓ | ✓ | ✓ | 2018 warns codes_supplement injection for BRTHMACR — allowed; 2013 asserts the force_numeric top-code ranges (HSDSIZEC 1–6, CHH0014C 0–3) verified in `override_verification.csv` |
| CCAHS | `test-pipeline-ccahs.R` | 1 | ✓ | ✓ | ✓ | BSW join tested (PUMFID key, WGT_PUMF dropped from BSW side) |
| ITS | `test-pipeline-its.R` | 2018, 2019 | ✓ | ✓ | ✓ | |
| SHS | `test-pipeline-shs.R` | First of: 2021, 2019, 2017 | ✓\* | — | ✓ | Stage 2 smoke-test; BSW join tested |
| CIS | `test-pipeline-cis.R` | First of: 2022, 2021, 2020, 2019, 2018, 2017 | ✓\* | — | ✓ | Stage 2 smoke-test |
| CPSS | `test-pipeline-cpss.R` | v1 (Stage 1 only†); v2–6 (full) | ✓ | — | ✓ | Cache-gated (`skip_if_not(.cpss_extracted(), ...)`); †v1 has no machine-readable codebook so Stage 2/3 are skipped; Stage 1 download tested in `test-pipeline-stage1.R` |
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
