# Longitudinal series (LFS, LFS_HIST)

A *longitudinal* series is released as a sequence of time slices (months or
years) that have the same, or nearly the same, variables and codes. Every
loaded slice is appended to **one DuckDB per series**, so the whole history
can be queried as a single table. `LFS` (2006 onwards, StatCan) and
`LFS_HIST` (1976–2005, Borealis) are the two instances. Code:
`R/longitudinal.R` (engine), `R/lfs_pipeline.R` (LFS spec + append helpers),
`R/lfs_hist.R` (LFS_HIST spec).

## The spec

`.pumf_longitudinal_spec(series)` returns a list. The engine
`.long_get_pumf(spec, ...)` holds no series-specific logic.

| Field | LFS | LFS_HIST |
|---|---|---|
| `db_file` | `LFS.duckdb` | `LFS_HIST.duckdb` |
| `table_prefix` → tables | `lfs_eng`, `lfs_fra` | `lfs_hist_eng`, `lfs_hist_fra` |
| `versions_table` | `lfs_versions` | `lfs_hist_versions` |
| `annual_files` | `TRUE`: "YYYY" is one annual release that supersedes the year's monthly loads | `FALSE`: "YYYY" means the twelve months, loaded one by one |
| `validate(v)` | "annual"/"monthly", or `stop()` | 1976-01..2005-12 only; 2006+ points to `"LFS"` |
| `available()` | `list_available_lfs_pumf_versions()` | all 360 months |
| `prepare()` | Stages 1+2 of the standard pipeline | Borealis download + canonical metadata |
| `build()` | `.lfs_build_version()` | `.lfs_hist_build()` |
| `variables()` | merge of every loaded version's `variables.csv`, newest wins | the shipped canonical `variables.csv` |

To add a series: write a spec, add it to `.pumf_longitudinal_specs()` and
`.pumf_longitudinal_series`, and give it a shared registry entry in
`.pumf_longitudinal_entry()`. Everything that used to test `series == "LFS"`
(`get_pumf()`, `pumf_metadata()`, `label_pumf_columns()`, the cache
functions, `open_pumf_documentation()`, `.pumf_db_path()`,
`.pumf_table_name()`) now dispatches on `.is_longitudinal(series)`.

## Engine invariants

- Data tables carry integer `SURVYEAR` / `SURVMNTH` (registry
  `force_integer`). A version filter is a filter on these columns.
- Schema drift is absorbed by `.lfs_append()` (`ADD COLUMN`, widening types,
  extending ENUM levels). Any series whose ENUM levels come from a *fixed*
  code list avoids level churn entirely (LFS_HIST does).
- The versions-table helpers in `R/lfs_pipeline.R` take the table name as a
  trailing `vt` argument.
- Issue #18 holds: a cache hit opens read-only only. A build opens read-write
  after `.assert_duckdb_writable()`. `.long_get_year_of_months()` loads each
  missing month with its own nested write connection, closes it, then returns
  a read-only year filter.
- A month that is present in the data table but has no tracking row (an
  interrupted earlier build) is deleted before being appended again.
- The status message shows complete years as `YYYY-01..YYYY-12`
  (`.long_compress_months()`).

## LFS_HIST specifics

**Source.** Borealis, ODESI series 71M0001XCB: one dataset per month *and*
language, 718 datasets in all (no French dataset for 1990-02 and 1996-02).
`inst/extdata/lfs_hist/datasets.csv` maps each version to its English and
French DOIs and its rebasing era (`.lfs_hist_rebased()`: 1976–86 original,
1987–95 rebased to the 2001 Census, 1996–2000 to 2006, 2001–05 to 2011).
Titles are not reliable for the era (the French January 2001 title says
"Remanié Recensement 2016"), so it is derived from the year.

**Per-month cache.** `<cache>/LFS_HIST/YYYY-MM/` holds the English CSV,
`.sas`, codebook PDF, `lfs-epa-eng.htm` and record-layout text; `fra/` holds
the French `.sas`; `borealis_manifest.csv` records DOI, file id, md5 and role.
The download loop is explicit (`.lfs_hist_download()`) because the `files`
override of `.borealis_download_dataset()` would mark the `.sas` as data.

**Canonical dictionary.** ODESI relabelled the same codes in every era
("Unemployed, temporary layoff" / "Unemploy,temp layoff"). Taking labels
per month would give one ENUM level per spelling in the shared table. So the
labels come from a canonical dictionary shipped in
`inst/extdata/lfs_hist/{variables,codes}.csv` and built by
`tools/build_lfs_hist_reference.R`:

1. Download every month's English and French `.sas` (~40 MB, cached in
   `<cache>/LFS_HIST/_reference/sas/`, written via `.part` files).
2. Parse each month with `parse_sas_odesi()` and map the 2001–05 occupation
   names to canonical names (`.lfs_hist_canonical_names()`).
3. For each (variable, code), take the label of the most recent month that has
   it. French labels come from the latest month that has a French label.
4. Apply `.lfs_hist_label_fixes()`, the curated corrections for codes whose
   *meaning* changed between eras.
5. Write `label_conflicts.csv` to the work directory: every (variable, code)
   whose normalised wording differs between months, with the months using
   each wording. Review it after every rebuild.

Findings from the conflict review (September 2026):

- **French variable labels** are damaged in every French SAS program:
  accented characters, often together with a neighbour, became a literal
  `?` ("Ann?d'enqu?"), silently lost characters ("ge du conjoint"), or were
  truncated. The value labels are intact. The build takes every French
  variable label it can find in the French `.sav` of 2005-06 and 1995-06
  instead.
- **MARSTAT** is a real recode, not a relabel. Until 1999-10 the *data* carry
  four categories (married or common-law, single, widowed, separated or
  divorced). Only the 1987-01..1989-08 SAS programs label them that way; all
  others, and every French one, use the six-category labels. The six
  categories are phased in by rotation group over 1999-11..2000-03, so in
  those months codes 1/4 still include common-law/divorced respondents of the
  older groups. Period-specific codes live in `.lfs_hist_code_eras()` and are
  swapped in per month by `.lfs_hist_codes_for(version)`. The shared ENUM
  therefore holds both label sets.
- The 2003 files carry SOC80 labels on NOC01_47 codes 26+, and the 2001–05
  files repeat EFAMTYPE 13's label on 15. The canonical labels and
  `.lfs_hist_label_fixes()` correct both.
- A scan of observed code sets over 20 sampled months found no other
  recoding.

Full import check (September 2026, all 360 English months): 39.1M rows,
66k-126k per month, no gaps or outlier months. The June FWEIGHT total rises
smoothly from 17.05M (1976) to 25.74M (2005), with no jumps at the rebasing
boundaries. CMA is identified from 1987-01 exactly. MARSTAT switches from 4
to 6 categories in 1999-11. Columns present only in some years:

| Years | Columns |
|---|---|
| 1976-89 | ED76TO89, SPED7689 (EDUC90 from 1990, SPED1990 from 1989) |
| 1976-86 | SP_SOC80 (SP_NOC01 from 1987); SOC80_49 only 1984-86 |
| 1976-98 | SOC80_21 (NOC01_25/NOC01_47 from 1987, so 1987-98 have both) |
| 1976-96 | WHYPTOLD, YNOLKOLD (WHYPTNEW from 1996, YNOLOOK from 1997) |
| 1996- | UNION, PERMTEMP, ESTSIZE, WHYLEFTN; FIRMSIZE from 1998 |
| 1997- | HRLYEARN, PAIDOT, UNPAIDOT |

At load time `.lfs_hist_write_metadata()` restricts the canonical dictionary
to the month's CSV columns. It warns about columns the dictionary lacks, and
about codes in the month's own `.sas` that the canonical codes lack.

**Build.** The CSV is read all-character (CP1252, `na = ""`), columns are
renamed to canonical names, missing codes become `NA` through
`.label_missing_codes()`, the `force_integer` columns are cast, and
`.apply_code_labels()` labels the rest.

**Validation against table 14-10-0287** (September 2026, unadjusted
estimates, 528 series: Canada and provinces x gender x 15+/15-24/25-54/55+
x population/employment/full-time/unemployment, 190,080 month cells). The
microdata (FWEIGHT) reproduce the table to rounding in every cell for
1976-01..1984-12 and 1986-12..2005-12, so the PUMF weights are the current
published ones for all eras. The exception is **1985-01..1986-11**:
population still matches exactly, but microdata unemployment is about 1.2%
higher (Canada 15+, up to 2.5% in Saskatchewan) and employment 0.1-0.4%
lower. The gap shrinks from 1986-06 and disappears in 1986-12. The raw codes
show no coding change, and the Borealis documentation says nothing about
it. The most likely explanation is that the published series for these months
carry a later revision or adjustment that the PUMF files do not. It cannot
be fixed from the microdata.

## Harmonised timeline (`get_lfs_timeline()`)

`R/lfs_timeline.R` stacks LFS_HIST and LFS into one lazy tbl with a curated
common schema. It never writes to either series database: an in-memory DuckDB
`ATTACH`es each file `(READ_ONLY)`, builds one `SELECT` per series and joins
them in the view `lfs_timeline` with `UNION ALL BY NAME`. Other readers of the
files are not blocked. A file locked by a writer (an import in progress) gives
an actionable error. Provenance is registered as series `"LFS_TIMELINE"`, which
`.pumf_read_variables_from_prov()` and `.pumf_tbl_module()` special-case, so
`label_pumf_columns()` and `pumf_var_labels()` work.

**Reference tables** live in `inst/extdata/lfs_timeline/` and are built by
`tools/build_lfs_timeline_reference.R` from the LFS_HIST dictionary and every
cached current-LFS `metadata/codes.csv`:

- `variables.csv` has one row per harmonised variable. It records the source
  column in each series (`lfs_hist`, `lfs`; `"GENDER|SEX"` means COALESCE), the
  `type` (factor/numeric/integer/character), `lfs_scale` and `hist_from`.
- `codes.csv` holds the harmonised codes with English and French labels.
- `recodes.csv` maps source codes to harmonised codes. `source` is
  `LFS_HIST`, `LFS_HIST_ERA` (the MARSTAT 4-category era labels) or `LFS`.

**How the SQL is built.** At run time the source codes are joined to the
series' own labels: the LFS_HIST dictionary and eras, or the loaded LFS
versions' `codes.csv`. Every column becomes `CASE CAST(col AS VARCHAR) WHEN
'<source label>' THEN '<harmonised label>' ... END`, cast to an in-memory ENUM
`lfs_tl_<NAME>` in harmonised code order. Source ENUM levels that are neither
mapped nor a missing label (`.lfs_timeline_na_labels`: not applicable, valid
skip, not stated) raise a warning naming each column and level, and become NA.

**Harmonisation choices:**

- Identical code sets (PROV, AGE_12, EDUC←EDUC90, COWMAIN, the job-search
  variables, etc.) take the current LFS labels.
- LFSSTAT merges HIST's three unemployed subtypes (codes 3-5) into the
  current "Unemployed"; HIST 6 becomes "Not in labour force". GENDER_SEX uses HIST
  SEX and current GENDER, falling back to SEX before 2011.
- MARSTAT uses 4 categories (married or common-law, single, widowed,
  separated or divorced), because that is all the data carry until 1999-10.
- CMA uses Montréal, Toronto, Vancouver and Other. It is NULL before
  `hist_from` = 1987-01, because earlier files do not identify CMAs.
- SCHOOLN is non-student / full-time / part-time. AGYOWNK and NAICS_18
  collapse the finer current codes.
- The current LFS stores hours in tenths and HRLYEARN in cents (`lfs_scale`
  0.1 / 0.01). LFS_HIST stores plain units. FWEIGHT becomes FINALWT.

## Tests

`tests/testthat/test-longitudinal.R`. A synthetic spec (`series = "FAKE"`,
no network) exercises the engine: single month, a year made of 12 months,
cache hits that never call `prepare`, month refresh, French labels, status
message, `refresh = "auto"`. Further tests cover the LFS_HIST validation and
eras, the shipped reference data, and `parse_sas_odesi()` on an inline ODESI
fixture. LFS behaviour stays covered by `test-pipeline-lfs.R`.

`tests/testthat/test-lfs-timeline.R` builds tiny LFS_HIST and LFS databases
in a temp cache and checks the recodes, scaling, read-only attach, the
unmapped-level warning and the consistency of the reference tables.
