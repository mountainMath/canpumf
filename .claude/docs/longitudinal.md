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
  `?` ("Ann?d'enqu?"). The value labels are intact. The build takes the
  French variable labels from the French `.sav` of 2005-06 and 1995-06
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
  recoding. Variables that are simply blank in some eras (ED76TO89/EDUC90,
  the NOC/SOC occupation sets, FIRMSIZE/ESTSIZE before 1997) are expected.

At load time `.lfs_hist_write_metadata()` restricts the canonical dictionary
to the month's CSV columns. It warns about columns the dictionary lacks, and
about codes in the month's own `.sas` that the canonical codes lack.

**Build.** The CSV is read all-character (CP1252, `na = ""`), columns are
renamed to canonical names, missing codes become `NA` through
`.label_missing_codes()`, the `force_integer` columns are cast, and
`.apply_code_labels()` labels the rest.

## Tests

`tests/testthat/test-longitudinal.R`. A synthetic spec (`series = "FAKE"`,
no network) exercises the engine: single month, a year made of 12 months,
cache hits that never call `prepare`, month refresh, French labels, status
message, `refresh = "auto"`. Further tests cover the LFS_HIST validation and
eras, the shipped reference data, and `parse_sas_odesi()` on an inline ODESI
fixture. LFS behaviour stays covered by `test-pipeline-lfs.R`.
