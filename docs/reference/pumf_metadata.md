# Download and parse PUMF metadata without building a DuckDB table

Runs Stage 1 (locate or download) and Stage 2 (parse metadata) and
returns the full bilingual canonical metadata. Both \`label_en\` and
\`label_fr\` columns are always returned regardless of language. This is
useful for inspecting variable definitions and code labels before
loading data with \[get_pumf()\].

## Usage

``` r
pumf_metadata(
  series,
  version,
  cache_path = getOption("canpumf.cache_path", tempdir()),
  refresh = FALSE,
  redownload = FALSE,
  registry = NULL
)
```

## Arguments

- series:

  Survey series acronym, e.g. \`"SFS"\`, \`"LFS"\`, \`"Census"\`.

- version:

  Version string, e.g. \`"2019"\`, \`"2021 (individuals)"\`.

- cache_path:

  Root cache directory. Defaults to \`getOption("canpumf.cache_path",
  tempdir())\`.

- refresh:

  If \`TRUE\`, re-parse metadata from the already-extracted raw command
  files (does not re-download).

- redownload:

  If \`TRUE\`, delete the cached zip and extracted files and re-download
  from StatCan before re-parsing. Implies \`refresh = TRUE\`.

- registry:

  Optional custom configuration created by \[pumf_registry_entry()\] (or
  \[pumf_registry()\]) to drive metadata parsing for a survey not in the
  built-in registry, or to override fields of one that is. Not supported
  for LFS.

## Value

A named list with three elements:

- \`variables\`:

  Tibble with columns \`name\`, \`label_en\`, \`label_fr\`, \`type\`,
  \`decimals\`, \`missing_low\`, \`missing_high\`.

- \`codes\`:

  Tibble with columns \`name\`, \`val\`, \`label_en\`, \`label_fr\`,
  mapping numeric codes to their labels.

- \`layout\`:

  Tibble with columns \`name\`, \`start\`, \`end\` for fixed-width data
  files; \`NULL\` for CSV-format surveys.

## See also

\[get_pumf()\], \[pumf_var_labels()\]

## Examples

``` r
if (FALSE) { # \dontrun{
meta <- pumf_metadata("SFS", "2019")
meta$variables
meta$codes[meta$codes$name == "PEFAMID", ]
} # }
```
