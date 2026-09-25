# Harmonised Labour Force Survey timeline, 1976 onward

Stacks the historical monthly LFS files (\`"LFS_HIST"\`, 1976 to 2005)
and the current LFS files (\`"LFS"\`, 2006 onward) into one lazy table
with a curated common set of variables, so that long time series can be
pulled with a single query.

## Usage

``` r
get_lfs_timeline(
  lang = c("eng", "fra"),
  sources = .lfs_timeline_series,
  refresh = FALSE,
  cache_path = getOption("canpumf.cache_path", tempdir())
)
```

## Arguments

- lang:

  \`"eng"\` (default) or \`"fra"\` for the labels.

- sources:

  The series to include, by default both.

- refresh:

  \`FALSE\` (default) opens what is already loaded. \`"auto"\` first
  calls \`get_pumf(\<source\>, refresh = "auto")\` for each series in
  \`sources\`, which loads every available version not yet in its
  database (for example a newly released LFS month), then opens the
  timeline. When everything is loaded this only checks the list of
  available versions. The first call loads all of LFS_HIST (360 monthly
  files from Borealis) and all LFS years from StatCan, which takes
  hours. If a version fails to load, the warning says so and the
  timeline opens with what is there.

- cache_path:

  Root cache directory. Defaults to \`getOption("canpumf.cache_path",
  tempdir())\`.

## Value

A lazy \`dplyr::tbl()\` over the view \`lfs_timeline\`. Categorical
columns are factors. \[label_pumf_columns()\] and \[pumf_var_labels()\]
work on it. Release it with \[close_pumf()\].

## Details

The two series keep their own DuckDB files. This function attaches both
\*\*read-only\*\* to an in-memory DuckDB and returns a view over them,
so it never blocks (and is never blocked by) other readers. By default
it reads only what is already loaded. Load data first with, for example,
\`get_pumf("LFS_HIST", "1995")\` or \`get_pumf("LFS", "2015")\`, or pass
\`refresh = "auto"\` to bring both series up to date before opening the
timeline.

The harmonised table has these columns: \* \`SOURCE\` (\`"LFS_HIST"\` or
\`"LFS"\`), \`SURVYEAR\` and \`SURVMNTH\`. \* Numeric variables in plain
units. The hours and wage variables of the current LFS files carry
implied decimals (tenths of hours, cents), which are removed here.
\`FINALWT\` is \`FWEIGHT\` in LFS_HIST. \* Categorical variables whose
codes are the same in both series (e.g. \`PROV\`, \`AGE_12\`,
\`COWMAIN\`, \`EFAMTYPE\`). These carry the current LFS labels. \*
Recoded variables: - \`LFSSTAT\`: the three unemployed categories of
LFS_HIST are collapsed. - \`GENDER_SEX\`: LFS_HIST \`SEX\` and the
current \`SEX\`/\`GENDER\`, on the \`GENDER\` scale, as in
\[add_lfs_GENDER_SEX()\]. - \`MARSTAT\`: four categories (married or
common-law, single, widowed, separated or divorced). The files before
November 1999 only have these four. - \`CMA\`: Montreal, Toronto,
Vancouver or other. It is \`NA\` before 1987, when LFS_HIST does not
identify CMAs. - \`SCHOOLN\`: non-student, full-time or part-time
student. - \`AGYOWNK\`: the four age groups of the youngest child in the
current files. - \`NAICS_18\`: industry in the 18 groups of LFS_HIST.
The 21 current groups are merged. - \`EDUC\`: the 1990 onward
classification (LFS_HIST \`EDUC90\`). It is \`NA\` before 1990, whose
categories do not map onto it. - \`WHYPT\`: reasons for part-time work
from 1997 (\`WHYPTNEW\`).

Occupation, immigration and the LFS_HIST-only family and spouse
variables are not part of the harmonised table. Use \[get_pumf()\] on
each series for those.

Rebasing: LFS_HIST weights are rebased to different Censuses by period
(1987-1995 to 2001, 1996-2000 to 2006, 2001-2005 to 2011; 1976-1986 are
not rebased). Levels can therefore jump at the seams between periods and
at 2006.

## See also

\[get_pumf()\], \[add_lfs_SURVDATE()\]

## Examples

``` r
# \donttest{
tl <- get_lfs_timeline()
#> LFS timeline: LFS_HIST 1976-01..2005-12 (360 versions); LFS 2006..2026-08 (28 versions).
if (!is.null(tl)) {
  tl |>
    dplyr::filter(SURVMNTH == 6L) |>
    dplyr::group_by(SURVYEAR, LFSSTAT) |>
    dplyr::summarise(persons = sum(FINALWT, na.rm = TRUE), .groups = "drop") |>
    dplyr::collect()
  close_pumf(tl)
}
# }
```
