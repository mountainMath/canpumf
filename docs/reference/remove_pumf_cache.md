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
version deletes only that version's rows from the shared \`LFS.duckdb\`;
if it was the last loaded version the shared database file is also
deleted.

## See also

\[list_pumf_cache()\], \[get_pumf()\]

## Examples

``` r
if (FALSE) { # \dontrun{
# Remove only DuckDB and metadata, keep raw files for quick rebuild:
remove_pumf_cache("SFS", "2019")

# Remove everything including raw files:
remove_pumf_cache("SFS", "2019", keep_raw = FALSE)
} # }
```
