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
read-only lazy table use \[get_pumf()\] instead. Returns
\`invisible(NULL)\` with an informative message if the data must be
downloaded but Statistics Canada is unreachable.

## See also

\[get_pumf()\]

## Examples

``` r
# \donttest{
con <- get_pumf_connection("SFS", "2019")  # NULL if StatCan is unreachable
#> Connected to DuckDB (read-write). Available tables: eng_EFAM_PUMF, eng_EFAM_PUMF_bsw_pweight, fra_EFAM_PUMF, pumf_bsw_pweight.
#> Disconnect with DBI::dbDisconnect(con, shutdown = TRUE) when done.
if (!is.null(con)) {
  tables <- DBI::dbListTables(con)
  DBI::dbGetQuery(con, sprintf('SELECT COUNT(*) AS n FROM "%s"', tables[1]))
  DBI::dbDisconnect(con, shutdown = TRUE)
}
# }
```
