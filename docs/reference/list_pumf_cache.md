# List the contents of the local canpumf cache

Scans the cache directory and returns a tibble describing every
downloaded PUMF version — which raw files, parsed metadata, and DuckDB
tables are present — along with their disk sizes.

## Usage

``` r
list_pumf_cache(cache_path = getOption("canpumf.cache_path", tempdir()))
```

## Arguments

- cache_path:

  Root cache directory. Defaults to \`getOption("canpumf.cache_path",
  tempdir())\`.

## Value

A tibble with columns:

- \`series\`:

  Survey series acronym.

- \`version\`:

  Version string.

- \`has_raw\`:

  \`TRUE\` if a zip or extracted data files are present.

- \`has_metadata\`:

  \`TRUE\` if a parsed \`metadata/\` directory exists.

- \`has_duckdb\`:

  \`TRUE\` if a DuckDB table is built for this version.

- \`raw_mb\`:

  Disk size of raw files in MB (excluding metadata and DuckDB).

- \`duckdb_mb\`:

  Disk size of the DuckDB file in MB. For LFS this is the total shared
  \`LFS.duckdb\` size, repeated for each version row.

Returns a zero-row tibble with the same column structure if the cache
directory does not exist or is empty.

## Details

For LFS surveys the DuckDB is a single shared file (\`LFS.duckdb\`) that
accumulates all versions; its total size is reported in \`duckdb_mb\`
for every LFS row. Use \[remove_pumf_cache()\] to free disk space.

## See also

\[remove_pumf_cache()\], \[get_pumf()\]

## Examples

``` r
# \donttest{
list_pumf_cache()
#> # A tibble: 255 × 7
#>    series        version       has_raw has_metadata has_duckdb  raw_mb duckdb_mb
#>    <chr>         <chr>         <lgl>   <lgl>        <lgl>        <dbl>     <dbl>
#>  1 1971PUMF_FMGD 1971_familie… TRUE    FALSE        FALSE       1.83          NA
#>  2 1971PUMF_FMGD 1971_familie… TRUE    FALSE        FALSE       7.09          NA
#>  3 1971PUMF_FMGD 1971_househo… TRUE    FALSE        FALSE       1.48          NA
#>  4 1971PUMF_FMGD 1971_individ… TRUE    FALSE        FALSE       5.43          NA
#>  5 1971PUMF_FMGD 1971_individ… TRUE    FALSE        FALSE      21.6           NA
#>  6 1976PUMF_FMGD 1976_househo… TRUE    FALSE        FALSE       2.35          NA
#>  7 1976PUMF_FMGD 1976_individ… TRUE    FALSE        FALSE      10.5           NA
#>  8 1981PUMF_FMGD 1981_familie… TRUE    FALSE        FALSE       0             NA
#>  9 1981PUMF_FMGD 1981_househo… TRUE    FALSE        FALSE       0.0119        NA
#> 10 1981PUMF_FMGD 1981_individ… TRUE    FALSE        FALSE       0.0123        NA
#> # ℹ 245 more rows
# With an explicit cache path:
list_pumf_cache(cache_path = file.path(tempdir(), "pumf_cache"))
#> # A tibble: 0 × 7
#> # ℹ 7 variables: series <chr>, version <chr>, has_raw <lgl>,
#> #   has_metadata <lgl>, has_duckdb <lgl>, raw_mb <dbl>, duckdb_mb <dbl>
# }
```
