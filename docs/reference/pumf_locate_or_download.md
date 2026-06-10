# Locate or download a PUMF version directory

Ensures the version directory exists and its zip has been extracted.
With \`refresh = TRUE\`, clears the DuckDB file(s) and \`metadata/\`
subdirectory so that Stages 2 and 3 re-run, but does \*\*not\*\*
re-download or re-extract raw data.

## Usage

``` r
pumf_locate_or_download(
  series,
  version,
  cache_path = getOption("canpumf.cache_path", tempdir()),
  refresh = FALSE,
  redownload = FALSE
)
```

## Arguments

- series:

  Survey series acronym, e.g. \`"SFS"\`.

- version:

  Version string, e.g. \`"2019"\` or \`"2021 (individuals)"\`.

- cache_path:

  Root cache directory. Defaults to \`getOption("canpumf.cache_path",
  tempdir())\`.

- refresh:

  If \`TRUE\`, delete the \`.duckdb\` file and \`metadata/\` dir so the
  downstream stages re-run. Raw zip and extracted files are untouched.

## Value

The version directory path, invisibly.

## Details

For EFT-only surveys (older Census years), stops with an informative
error asking the user to deposit the zip manually.
