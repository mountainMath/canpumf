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

- series:

  Survey series acronym.

- version:

  Version string.

- has_raw:

  \`TRUE\` if a zip or extracted data files are present.

- has_metadata:

  \`TRUE\` if a parsed \`metadata/\` directory exists.

- has_duckdb:

  \`TRUE\` if a DuckDB table is built for this version.

- raw_mb:

  Disk size of raw files in MB (excluding metadata and DuckDB).

- duckdb_mb:

  Disk size of the DuckDB file in MB. For LFS this is the total shared
  \`LFS.duckdb\` size, repeated for each version.

## Details

For LFS surveys the DuckDB is a single shared file (\`LFS.duckdb\`) that
accumulates all versions; its total size is reported in \`duckdb_mb\`
for every LFS row.
