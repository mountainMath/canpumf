# Remove a PUMF version from the local cache

Deletes the DuckDB table (and optionally the raw zip and extracted
files) for one cached PUMF version.

## Usage

``` r
remove_pumf_cache(
  series,
  version,
  keep_raw = TRUE,
  cache_path = getOption("canpumf.cache_path", tempdir())
)
```

## Arguments

- series:

  Survey series acronym, e.g. \`"SFS"\` or \`"LFS"\`.

- version:

  Version string, e.g. \`"2019"\` or \`"2023-06"\`.

- keep_raw:

  If \`TRUE\` (default), keep the raw zip and extracted data so
  \[get_pumf()\] can rebuild without re-downloading. If \`FALSE\`,
  delete everything including raw files.

- cache_path:

  Root cache directory. Defaults to \`getOption("canpumf.cache_path",
  tempdir())\`.

## Value

Invisibly \`NULL\`.

## Details

With the default \`keep_raw = TRUE\`, only the DuckDB and parsed
\`metadata/\` are removed; the raw zip and extracted data are left
intact so that \[get_pumf()\] can rebuild without re-downloading. Set
\`keep_raw = FALSE\` to delete everything, freeing the full disk space.

For LFS surveys the DuckDB is shared across all versions. Removing one
version deletes only that version's rows; if it was the last version the
shared \`LFS.duckdb\` is also deleted.
