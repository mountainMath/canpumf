# Open PUMF documentation in browser

Scans the cached version directory for PDF and TXT documentation files
and opens matching files in the default browser. If no files are found
in the extracted content, the zip is inspected and documentation files
are extracted on demand into a \`docs_extracted/\` subdirectory.

## Usage

``` r
open_pumf_documentation(
  series = NULL,
  version = NULL,
  documentation_type = "user_guide",
  cache_path = getOption("canpumf.cache_path", tempdir()),
  pumf_series = NULL,
  pumf_version = NULL,
  pumf_cache_path = NULL
)
```

## Arguments

- series:

  Survey series acronym, e.g. \`"SFS"\`, \`"Census"\`.

- version:

  Version string, e.g. \`"2019"\`, \`"2021 (individuals)"\`. If
  \`NULL\`, searches the series directory directly (for series with a
  single version).

- documentation_type:

  Which type of documentation to open. One of \`"user_guide"\`
  (default), \`"reference_guide"\`, \`"questionnaire"\`, \`"quality"\`,
  or \`"errata"\`. When multiple files match the type pattern those all
  opened. When only one file exists the filter is skipped.

- cache_path:

  Root cache directory. Defaults to \`getOption("canpumf.cache_path",
  tempdir())\`.

- pumf_series:

  Deprecated; use \`series\`.

- pumf_version:

  Deprecated; use \`version\`.

- pumf_cache_path:

  Deprecated; use \`cache_path\`.

## Value

Invisibly, the paths of the opened documentation files.
