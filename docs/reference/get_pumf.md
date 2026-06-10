# Get a Statistics Canada PUMF dataset as a lazy DuckDB table

Main entry point for the canpumf package. Downloads (if needed), parses
metadata, applies bilingual labels, and returns a lazy \`dplyr::tbl()\`
backed by a DuckDB file in the cache directory. Subsequent calls reuse
the cached DuckDB without re-downloading.

## Usage

``` r
get_pumf(
  series = NULL,
  version = NULL,
  lang = "eng",
  cache_path = getOption("canpumf.cache_path", tempdir()),
  refresh = FALSE,
  redownload = FALSE,
  read_only = TRUE,
  ...
)
```

## Arguments

- series:

  Survey series acronym, e.g. \`"SFS"\`, \`"CHS"\`, \`"LFS"\`,
  \`"Census"\`, \`"CPSS"\`.

- version:

  Version string (e.g. \`"2019"\`, \`"2021 (individuals)"\`,
  \`"2023-06"\`). For series with a single version omit or pass
  \`NULL\`.

- lang:

  \`"eng"\` (default) or \`"fra"\`. Selects which set of labels to
  apply. Each language creates a separate DuckDB table (created lazily
  on first request).

- cache_path:

  Root cache directory. Defaults to \`getOption("canpumf.cache_path",
  tempdir())\`. Set persistently in \`.Rprofile\` with
  \`options(canpumf.cache_path = "\<path\>")\`.

- refresh:

  \`FALSE\` (default) reuses cached data. \`TRUE\` clears the DuckDB
  table and metadata and rebuilds from the already-extracted raw files
  (does not re-download). \`"auto"\` is accepted for LFS only and
  downloads all available versions not yet in the database.

- redownload:

  If \`TRUE\`, delete the cached zip and extracted files and re-download
  from StatCan before rebuilding. Implies \`refresh = TRUE\`. Not valid
  with \`refresh = "auto"\`.

- read_only:

  Open the DuckDB connection in read-only mode (default \`TRUE\`). Pass
  \`FALSE\` to allow write access, e.g. to persist custom views or
  derived tables in the DuckDB file. Use \[close_pumf()\] to release the
  connection when done.

- ...:

  Accepts deprecated parameter names (\`pumf_series\`, \`pumf_version\`,
  \`pumf_cache_path\`, \`layout_mask\`, \`file_mask\`,
  \`guess_numeric\`, \`timeout\`, \`refresh_layout\`) with a warning.

## Value

A lazy \`dplyr::tbl()\` backed by a DuckDB connection. Call
\`dplyr::collect()\` to obtain a local tibble. Call \[close_pumf()\] to
release the connection.

## Details

\*\*Breaking change from the pre-DuckDB API\*\*: this function now
returns a lazy \`dplyr::tbl()\` instead of a tibble. Call
\`dplyr::collect()\` to materialise a local tibble.

The LFS is treated specially: all versions share a single \`LFS.duckdb\`
database. Pass \`version = "YYYY"\` (annual) or \`"YYYY-MM"\` (monthly).
\`refresh = "auto"\` downloads every available LFS version that is not
yet in the database; this is only valid for LFS.
