# Get Labour Force Survey PUMF data from a shared longitudinal DuckDB

Manages a single \`LFS.duckdb\` file that accumulates all downloaded LFS
versions. Each call either retrieves already-loaded data or downloads,
parses, labels, and appends a new version.

## Usage

``` r
lfs_get_pumf(
  version = NULL,
  lang = "eng",
  cache_path = getOption("canpumf.cache_path", tempdir()),
  refresh = FALSE,
  redownload = FALSE,
  read_only = TRUE
)
```

## Arguments

- version:

  LFS version string (\`"YYYY"\` or \`"YYYY-MM"\`), or \`NULL\` to
  report database state and return the full table.

- lang:

  \`"eng"\` (default) or \`"fra"\`.

- cache_path:

  Root cache directory.

- refresh:

  \`FALSE\` (default), \`TRUE\` (re-parse and re-label the specified
  version from the cached raw files), or \`"auto"\` (download all
  versions not yet in the database). \`refresh = TRUE\` requires a
  non-NULL \`version\`.

- redownload:

  If \`TRUE\`, delete the cached zip and extracted content for the
  specified version and re-download from StatCan before rebuilding.
  Implies \`refresh = TRUE\`. Requires a non-NULL \`version\`.

- read_only:

  Open the DuckDB connection in read-only mode (default \`TRUE\`). Pass
  \`FALSE\` to allow write access to the LFS DuckDB.

## Value

A lazy \`dplyr::tbl()\`, or \`invisible(NULL)\` when \`version = NULL\`
and no data has been loaded.

## Details

\*\*Version types\*\*: - \`"YYYY"\` (e.g. \`"2023"\`) — annual file
released by StatCan after year-end. - \`"YYYY-MM"\` (e.g. \`"2024-06"\`)
— monthly file for the current year.

When an annual file for year Y is loaded and monthly files for that year
are already in the database, the monthly rows are replaced
(supersession). Conversely, if an annual for year Y is already loaded,
requesting a monthly for that year returns the annual data filtered to
that month without re-downloading.

\*\*Connection note\*\*: the returned \`tbl\` holds an open DuckDB
connection. Loading a second version (i.e. calling \`lfs_get_pumf\`
again while holding the first result) requires the first tbl's
connection to be closed first. Use \[close_pumf()\] or
\`dplyr::collect()\` the result before the next call.
