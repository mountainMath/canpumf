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
| `test-parse-sas-cards.R` | SAS reading cards parser (`.lay` + `.lbe`); `@pos` INPUT layouts including the indexed-array shorthand `@28 (BSW1-BSW1000) (1000* 7.2)` used by bootstrap-weight cards, and its rejection of malformed ranges. Also `parse_sas_data_labels()`: quoted `PROC FORMAT` character codes keep their zero padding (`"01"`), `/* $FMT format applies to: … */` comments spanning lines associate every listed variable, the French phrasing `s'applique à:` associates too, and `detect_formats()` pairs an `ENG`/`FR` directory split via `.is_fra_sas()` |
| `test-parse-spss-mono.R` | SPSS monolithic parser (`parse_spss_mono`), including string continuations (`'text' + 'more'`) in all four combinations of quote character and line break, mixed delimiters (`"…d'eq" + 'uivalence'`), and chains of three fragments — a dropped tail leaves a label cut mid-word, which is exactly the shape of the upstream truncation the PDF cross-check repairs |
| `test-parse-spss-split.R` | SPSS split-file parser (`parse_spss_split`) |
| `test-parse-spss-sav.R` | SPSS binary `.sav` parser (via haven) |
| `test-merge-metadata.R` | `merge_metadata()` conflict resolution |
| `test-pdf-repair.R` | The user-guide PDF cross-check layer, everything downstream of the scrape (so no `pdftools` and no cache needed — the parser output is synthesised): `.pumf_norm_code()` reconciling zero-padded against bare codes, `.pumf_is_subsequence()` on the interior-drop pattern, and `.pumf_repair_action()`'s four verdicts — fill / repair (strict superstring **or** anchored subsequence) / flag / ok. `.pumf_validate_pdf_freqs()` against a synthetic fixed-width file for all four statuses (exact match, continuous with a `lo : hi` range row, range-only, wrong count → `mismatch`, absent column → `unchecked`). Multi-module disambiguation: `.pumf_pdf_select_blocks()` dropping a block at the wrong field offset, and `.pumf_pdf_resolve_duplicate_blocks()` picking on frequencies where offsets tie. `.pumf_apply_pdf_repairs()`: repairs and fills applied, ledger schema incl. the `validation` column, repairs withheld on `mismatch` **but not** on `unchecked`, sentinels already covered by a `MISSING VALUES` range not reported as undeclared, and genuinely undeclared codes reported but never injected. The fixture's labels carry a realistic 60-character ceiling so the truncation fingerprint fires; a `truncated = FALSE` variant keeps the same string shapes but removes the ceiling. Also covered: `.pumf_truncation_width()` finding a ceiling only where one was imposed, repairs downgraded to `flagged` when the command-file label is not at the ceiling, `.pumf_left_truncated()` accepting a strict suffix (including a mid-list truncation that starts with a capital), `.pumf_annotation_prefix()` rejecting both shapes of prepended text (the editorial "Grouped variable: Age group" and the scraped "Longueur : 2 …" field header) while passing running prose, the veto outranking the width fingerprint on a label that sits at the ceiling, a repair carried by the suffix signature alone where no ceiling exists, undeclared codes not reported for variables the command file declares continuous, `.pumf_pdf_position_agreement()` scoring the guide against `layout.csv`, and `.pumf_pdf_choose_candidates()` preferring the candidate whose field positions match the layout |
| `test-sentinel-labels.R` | `.missing_pat` / `.sentinel_pat` / `.detect_sentinel_only()`. Asserts English and French label pairs classify **identically** — the failure mode both known bugs took, since the check runs once per build language and a split silently changes a column's storage type. Covers accented labels and accented capitals (`(*UCP)`, without which PCRE's `\w` is ASCII-only), elided articles (`l'annulation`), a trailing sentence period present in one language only, that genuine categorical labels beginning with "No"/"Aucun" are *not* swallowed, that the NA range is derived from true-missing codes only (a zero label is a valid zero), and that the `zero` alternative stays ASCII-only on purpose (see the comment in `R/metadata_parsers.R`) |
| `test-metadata-io.R` | `read_metadata()` / `write_metadata()` round-trips |
| `test-bilingual.R` | Label selection, fallback, `check_labels()` |
| `test-factor-enum.R` | Factor → DuckDB ENUM encoding |
| `test-cache-mgmt.R` | Cache path resolution, version extraction checks |
| `test-registry.R` | `pumf_registry_lookup()` for all registered surveys |
| `test-override-verification.R` | Every manual registry override (`force_numeric`, `na_values`, `cols_swap`, `rename`, `rename_regex`, `codes_supplement`, `missing_supplement`, `missing_codes`) has a `confirmed`/`unverifiable` row in `override_verification.csv`; no stale ledger rows; confirmed rows carry a source file and date. See "Override verification workflow" in CLAUDE.md |
| `test-pipeline-stage1.R` | `pumf_locate_or_download()` (download, unzip, collision handling) — uses `skip_if_offline()` for download tests; download attempts wrapped in `tryCatch` → `skip()` so StatCan downtime produces a skip, not a failure |
| `test-pipeline-stage3.R` | Stage 3 helpers: `.find_pumf_data_file()`, `pumf_build_duckdb()` end-to-end with synthetic data; `.apply_numeric_conversion()`'s opt-in `implied_decimals` (SAS `w.d` scaling for fixed-width BSW files — off by default, deferring to an explicit `.`, and applied before the display-unit missing range) and its `missing_codes` discrete-sentinel blanking; the `rename_regex` fixup (rewrites only onto declared variable names, never collides with an already-correct column, no-op without `known_vars`, and is not applied as a literal rename) |
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
| GSS (Caregiving) | `test-pipeline-gss.R` | Cycle 32 (2018), Cycle 26 (2012), Cycle 21 (2007), Cycle 16 (2002), Cycle 11 (1996) | ✓ | ✓ | ✓ | Canonical GSS keys are `Cycle N (YYYY)`; cycle number, bare year, and theme name all resolve as aliases (e.g. `2002`, `16`, `Aging and Social Support`). Cycle 21 (2007) warns "no French translation" for ~925/951 variables — allowed; it also subsumes the former "Education 2007" entry (same c21_2007 data). Cycle 16 (2002) = "Aging and Social Support", a multi-module survey (MAIN + CG4 + CG6 + CR linked on RECID); primary module MAIN carries WGHT_PER; English-only code labels (bilingual parity skipped). Cycle 16 (2002) is also the reference case for the **user-guide PDF cross-check**: `12M0016-GPE.pdf` Appendix G is parsed, its per-code frequencies reconciled against all four `.DAT` files, and the command files' truncated labels repaired from it — asserted via `pumf_freq_validation()` (no `mismatch`, one block per name) and `pumf_label_repairs()` (repairs only lengthen labels; `CG4_FR_Q100_C` delivers the full label; the substantive receiver-vs-provider divergence on `CG4_FR_Q220`'s *variable* label is flagged, not applied — pinned to `kind == "variable"`, since the same variable's value labels are left-truncated and are repaired; `CG4_FR_Q100_C` code 85, the mid-list truncation that motivated the cross-check, is repaired; and `AGE_LAST_RETIRED_C`, where the guide leaks its "Longueur : 2" field header into the label, stays flagged). Cycle 26 (2012) is the reference case for **guide selection and the frequency-table reader**: it ships two candidate dictionaries and `.pumf_pdf_choose_candidates()` is asserted to pick `C26_PUMF_Users_Guide.pdf` over the analytical-file one; all 610 documented variables then reconcile with no `mismatch` and no `unchecked`, which pins the three table-layout quirks — `WLY_Q150`'s `9,520` (wider than the `FREQ` header it aligns to), `MAR_Q110`'s `97 Not Asked  0` (printed further right than the counts above it), and `LSR_Q110`'s 0–10 scale (codes 01–09 are rows with no label) |
| GSS (Safety) | `test-pipeline-gss.R` | Cycle 34 (2019), Cycle 28 (2014), Cycle 13 (1999), Cycle 8 (1993) | ✓ | ✓ | ✓ | All have force_numeric boundary-label variables; theme aliases `Safety <year>` resolve to these cycles |
| GSS (Family) | `test-pipeline-gss.R` | Cycle 31 (2017), Cycle 25 (2011), Cycle 15 (2001), Cycle 10 (1995) | ✓ | ✓ | ✓ | All have age/family-history force_numeric; Cycle 25 (2011) also warns WTBS unlabeled; theme aliases `Family <year>` resolve to these cycles |
| GSS (Social Identity) | `test-pipeline-gss.R` | Cycle 35 (2020), Cycle 27 (2013), Cycle 17 (2003) | ✓ | ✓ | ✓ | Cycle 17 (2003)/Cycle 27 (2013) have many force_numeric; Cycle 17 (2003) warns WTBS unlabeled; theme aliases `Social Identity <year>` resolve to these cycles |
| GSS (Education) | `test-pipeline-gss.R` | Cycle 9 (1994) | ✓ | ✓ | ✓ | English-only command files (no French labels); the former "Education 2007" merged into Cycle 21 (2007) — same data. Alias `Education 1994` resolves here |
| GSS (Time Use) | `test-pipeline-gss.R` | Cycle 36 (2022), Cycle 29 (2015), Cycle 24 (2010), Cycle 12 (1998) | ✓ | ✓ | ✓ | Multi-module: each cycle ships a respondent-level **Main** file plus an **Episode** file (one row per activity episode), built as two linked tables joining on the cycle key (PUMFID for Cycle 36 (2022)/Cycle 29 (2015), RECID for Cycle 24 (2010)/Cycle 12 (1998)) via `pumf_module(tbl,"Episode")`; Cycle 36 (2022) Episode uses force_numeric (ACTIVITY/LOCATION/TUI_01/TUI_03); Cycle 24 (2010) warns WTBS_EPI unlabeled, and is the reference case for the user-guide **block-label bound**: its last documented variable `WTSBS_001` prints no frequency table, so the label extraction used to run to the end of the document (78,014 characters of appendix and table of contents); the parse is asserted to return just the one-sentence label, with no block in the guide exceeding 500 characters. Its **Episode** guide is the reference case for the **wrapped frequency row**: `SACT1` code 15's label reaches into the number column and pushes the weighted count onto the next line, code 18's label is printed flush against both counts, and codes 15/18/20 are asserted to give frequencies 4255/NA/1680 with all three labels free of digits; the same block's unreadable count is asserted to validate as `unchecked` (it used to abort Stage 2 with `missing value where TRUE/FALSE needed`) while the rest of the guide still validates. The 2022 file (TU_ET_2022.zip, no cycle prefix) is mapped to cycle 36 explicitly; theme alias `Time Use <year>` resolves to these cycles |
| SFS | `test-pipeline-sfs.R` | 2023, 2019, 2016, 2012, 2005 | ✓ | ✓ | ✓ | BSW join tested for 2016/2019/2023 |
| SFS 1999 | `test-pipeline-sfs.R` | 1999 | ✓ | ✓ | — | DATA LIST-only SPSS + PDF dictionary (`parse_pdf_dictionary`) for labels; English-only (no French PDF bundled in the download); row count (15 933) and column count (80) asserted; PDF-derived labels verified |
| CHS | `test-pipeline-chs.R` | First of: 2022, 2021, 2018 | ✓\* | — | ✓ | Stage 2 smoke-test; BSW join tested; bilingual tests use whichever version is in cache |
| SGVP | `test-pipeline-sgvp.R` | 2023, 2018, 2013, 2010, 2007, 2004, 2000, 1997 | ✓ | ✓ | ✓ | 2018 warns codes_supplement injection for BRTHMACR — allowed; 2013 asserts the force_numeric top-code ranges (HSDSIZEC 1–6, CHH0014C 0–3) verified in `override_verification.csv`. Multi-module (older cycles): MAIN + detail tables joining on the cycle key — 2010/2007/2004 add **GS** (giving, PUMFID), 2000 adds **GS + VD** (volunteer detail, MICRO_ID), 1997 adds **GIVE + VOLNTR** (IDNUM); `pumf_module(tbl,"GS")` etc. 2007 is the reference case for a guide that **reconciles but supplies almost no repairs**: `CSGVP2007_MAIN_CdBk.pdf` validates 311/311 variables against the data (129 continuous), yet its labels are the full question wording against the command file's hand-written abbreviations rather than truncations of them — asserted as zero `mismatch`, ≤5 repaired/filled out of >500 divergences, and at least one flag reasoned "not truncated". It also pins the mirror of the left-truncation signature: the guide's "Grouped variable: " / "Variable groupée : " prefixes leave the command file's label a strict suffix of the guide's, exactly as a dropped prefix would, and `DH1GAGE` is asserted to stay `flagged` |
| CCAHS | `test-pipeline-ccahs.R` | 1 | ✓ | ✓ | ✓ | BSW join tested (PUMFID key, WGT_PUMF dropped from BSW side) |
| CHSS | `test-pipeline-chss.R` | 2019-2020 | ✓ | ✓ | ✓ | Registry pins `download_format = "TXT"` (the CSV bundle ships the data alone, without command files) and the override is asserted against a synthetic CSV/SAS/TXT catalogue frame. BSW join tested: the 1000 replicate weights live in a separate fixed-width file whose layout comes only from the SAS array card `@28 (BSW1-BSW1000) (1000* 7.2)`, with the decimal point implied — `sum(WTS_CM) == 6,435,765` (the weighted total printed in the data dictionary) and both BSW1 and BSW1000 match it. ALWDVWKY asserted continuous (0–84) via `force_numeric` |
| PALS | `test-pipeline-pals.R` | 2001, 2006 | ✓ | ✓ | ✓ | Both editions ship one archive holding a complete `PUMF/ENG/` and `PUMF/FR/` copy; the French command file is paired by the `/FR/` path marker alone. 2001 has **no flat file** — the SAS dataset (`pals_pumf_sas.sas7bdat`) is read with haven, labelled from `SAS code.sas` / `Code SAS.sas` (quoted character codes, French association comments reading `s'applique à:`), and its *collection* column names are stripped of the "A" prefix by `rename_regex` (632 of 758 columns; AGEGRP5/AGILIM/ATTENDRP correctly untouched). Weighted `DISAB` totals asserted against the User Guides (3,420,338 for 2001, 4,162,696 for 2006); `force_numeric` ranges asserted for HOURS/E1HRS/C28AA. 2006 asserts `missing_codes` on AUDE_Q02, whose sentinels (−5/−6/−7 and 998/999) straddle the valid hours so no single missing range can express them — 635 real answers survive, matching the guide. Note: 72 of PALS 2001's variables have `label_en == name` because the SAS command file itself writes `LABEL IDNUM = "IDNUM";` — the source is authoritative and no labels are invented. 2006 is the reference case for **guide selection and the position channel**: the release ships both its pre-revision and its Dec 2011 revised user guide, which describe different field positions (3/746 against 746/746 agreement with `layout.csv`), and `.pumf_pdf_choose_candidates()` is asserted to pick the revision in both languages. Its frequency tables are tabulated over the disability sub-population, so no count reproduces — the positions carry the corroboration instead, `mismatch` is downgraded to `unchecked`, and the 100 resulting repairs are asserted to be value labels apart from the single variable label that is a genuine dropped prefix (`AALR2_ACTIVITIES_1`, "visit family or friends?" of "How often do you (does ....) visit family or friends?") |
| ITS | `test-pipeline-its.R` | 2018, 2019 | ✓ | ✓ | ✓ | |
| SHS | `test-pipeline-shs.R` | First of: 2023, 2021, 2019, 2017 | ✓\* | — | ✓ | Stage 2 smoke-test; BSW join tested. 2017 and 2019 pin `metadata_encoding = "UTF-8"` (their reading cards are UTF-8 where 2021/2023 are not) and are asserted to carry no mojibake in their French labels. 2017 is multi-module: **Interview** (primary) + **Diary** (one row per purchase) joining on CASEID, each with its own BSW flatfile (per-module BSW); `pumf_module(tbl,"Diary")` |
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
