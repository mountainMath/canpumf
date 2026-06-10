# Get a write-access DuckDB connection to a PUMF database

Downloads (if needed), parses metadata, builds the DuckDB, and returns a
raw read-write \[DBI::DBIConnection-class\] to the database file. The
connection gives full SQL access: create derived tables, persist custom
views, and join them against the original PUMF data in the same file.
\[get_pumf()\] is built on top of this function and adds language-table
selection and read-only semantics for everyday analysis.

## Usage

``` r
get_pumf_connection(
  series = NULL,
  version = NULL,
  lang = "eng",
  cache_path = getOption("canpumf.cache_path", tempdir()),
  refresh = FALSE,
  redownload = FALSE,
  ...
)
```

## Arguments

- series:

  Survey series acronym, e.g. \`"SFS"\` or \`"LFS"\`.

- version:

  Version string. \`NULL\` for single-version series.

- lang:

  \`"eng"\` (default) or \`"fra"\`. Passed to the pipeline to ensure the
  labelled table for this language is built before returning.

- cache_path:

  Root cache directory. Defaults to \`getOption("canpumf.cache_path",
  tempdir())\`.

- refresh:

  If \`TRUE\`, rebuild the DuckDB from already-extracted raw files
  before opening. Does \*\*not\*\* re-download.

- redownload:

  If \`TRUE\`, re-download from StatCan and rebuild. Implies \`refresh =
  TRUE\`.

- ...:

  Accepts deprecated parameter names (\`pumf_series\`, \`pumf_version\`,
  \`pumf_cache_path\`) with a warning.

## Value

A \[DBI::DBIConnection-class\] opened in read-write mode. Disconnect
with \`DBI::dbDisconnect(con, shutdown = TRUE)\` when done.
