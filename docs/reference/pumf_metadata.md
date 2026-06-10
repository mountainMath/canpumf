# Download and parse PUMF metadata without building a DuckDB table

Runs Stage 1 (locate or download) and Stage 2 (parse metadata) and
returns the full bilingual canonical metadata. Both \`label_en\` and
\`label_fr\` columns are always returned regardless of language. This is
useful for inspecting variable definitions and code labels before
loading data.

## Usage

``` r
pumf_metadata(
  series,
  version,
  cache_path = getOption("canpumf.cache_path", tempdir()),
  refresh = FALSE,
  redownload = FALSE
)
```

## Arguments

- series:

  Survey series acronym.

- version:

  Version string.

- cache_path:

  Root cache directory.

- refresh:

  If \`TRUE\`, re-parse metadata from the already-extracted raw command
  files (does not re-download).

- redownload:

  If \`TRUE\`, delete the cached zip and extracted files and re-download
  from StatCan before re-parsing. Implies \`refresh = TRUE\`.

## Value

Named list with elements: - \`variables\`: tibble (name, label_en,
label_fr, type, decimals, missing_low, missing_high) - \`codes\`: tibble
(name, val, label_en, label_fr) - \`layout\`: tibble (name, start, end)
or \`NULL\` for CSV-format data
