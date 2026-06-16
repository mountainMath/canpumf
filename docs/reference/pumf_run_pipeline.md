# Run the full three-stage PUMF pipeline for one survey version

Convenience wrapper that: 1. Looks up the survey registry entry for
\`(series, version)\`. 2. Calls \[pumf_locate_or_download()\] (Stage 1).
3. Calls \[pumf_parse_metadata()\] (Stage 2) with the registry's
\`layout_mask\` and \`metadata_encoding\`. 4. Calls
\[pumf_build_duckdb()\] (Stage 3). 5. Returns a lazy \`dplyr::tbl()\`
via \[pumf_open_duckdb()\].

## Usage

``` r
pumf_run_pipeline(
  series,
  version,
  lang = "eng",
  cache_path = getOption("canpumf.cache_path", tempdir()),
  refresh = FALSE,
  redownload = FALSE,
  read_only = TRUE
)
```

## Arguments

- series:

  Survey series acronym, e.g. \`"SFS"\` or \`"CHS"\`.

- version:

  Version string, e.g. \`"2019"\` or \`"2021 (individuals)"\`.

- lang:

  \`"eng"\` (default) or \`"fra"\`.

- cache_path:

  Root cache directory. Defaults to \`getOption("canpumf.cache_path",
  tempdir())\`.

- refresh:

  If \`TRUE\`, clear DuckDB and metadata and rebuild from
  already-extracted raw data. Does \*\*not\*\* re-download.

- redownload:

  If \`TRUE\`, delete the zip and all extracted content and re-download
  from StatCan before rebuilding. Implies \`refresh = TRUE\`.

- read_only:

  Open the DuckDB in read-only mode (default \`TRUE\`).

## Value

A lazy \`dplyr::tbl()\` backed by a DuckDB connection.

## Details

Each stage is idempotent: subsequent calls reuse cached results unless
\`refresh = TRUE\`.
