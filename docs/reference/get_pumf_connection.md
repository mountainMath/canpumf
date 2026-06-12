# Get a read-write DuckDB connection to a PUMF database

Runs the full pipeline and returns a raw read-write
\[DBI::DBIConnection-class\]. Use this when you need direct SQL access —
to persist custom views, join derived tables, or inspect DuckDB
internals. For everyday analysis use \[get_pumf()\], which returns a
safer read-only lazy \`dplyr::tbl()\`.

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

  Survey series acronym, e.g. \`"SFS"\`, \`"Census"\`.

- version:

  Version string, e.g. \`"2019"\`. \`NULL\` for single-version series.

- lang:

  \`"eng"\` (default) or \`"fra"\`.

- cache_path:

  Root cache directory. Defaults to \`getOption("canpumf.cache_path",
  tempdir())\`.

- refresh:

  If \`TRUE\`, rebuild from already-extracted files (no re-download).

- redownload:

  If \`TRUE\`, re-download and rebuild from scratch.

- ...:

  Accepts deprecated parameter names (\`pumf_series\`, \`pumf_version\`,
  \`pumf_cache_path\`) with a warning.

## Value

A \[DBI::DBIConnection-class\] in read-write mode. Disconnect with
\`DBI::dbDisconnect(con, shutdown = TRUE)\` when done. For a safer
read-only lazy table use \[get_pumf()\] instead.

## See also

\[get_pumf()\]

## Examples

``` r
if (FALSE) { # \dontrun{
con <- get_pumf_connection("SFS", "2019")
DBI::dbListTables(con)
DBI::dbGetQuery(con, 'SELECT COUNT(*) FROM "eng_SFS_2019"')
DBI::dbDisconnect(con, shutdown = TRUE)
} # }
```
