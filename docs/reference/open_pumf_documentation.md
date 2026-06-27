# Open PUMF documentation in the browser

Scans the cached version directory for PDF documentation files and opens
them interactively. If no PDFs are found, falls back to small text files
(filtering out large FWF data files by size). When multiple candidate
files exist, an interactive menu lets you choose which to open, with
"Open all" as the last option. In non-interactive mode the first
preferred-language file is opened automatically.

## Usage

``` r
open_pumf_documentation(
  series = NULL,
  version = NULL,
  lang = NULL,
  cache_path = getOption("canpumf.cache_path", tempdir()),
  pumf_series = NULL,
  pumf_version = NULL,
  pumf_cache_path = NULL
)
```

## Arguments

- series:

  Survey series acronym (e.g. \`"SFS"\`, \`"Census"\`), \*\*or\*\* a
  lazy \`dplyr::tbl()\` / DuckDB connection returned by \[get_pumf()\].
  When a tbl or connection is supplied, \`version\`, \`cache_path\`, and
  \`lang\` are read from the connection provenance; explicit arguments
  take precedence.

- version:

  Version string (e.g. \`"2019"\`, \`"2021 (individuals)"\`). For LFS,
  omit to open documentation for the most recently downloaded version.
  Ignored when \`series\` is a tbl or connection.

- lang:

  \`"eng"\` (default) or \`"fra"\`. Documentation files whose names
  match the requested language are sorted first. When \`series\` is a
  connection and \`lang\` is not supplied, the connection's language is
  used.

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

Invisibly, the file path(s) of the opened documentation, or
\`invisible(NULL)\` when no documentation is found or data has not been
downloaded yet.

## Details

After opening documentation, emits a message listing any manual registry
overrides (sentinel values, forced-numeric columns, column swaps, etc.)
that were applied at import so values can be interpreted correctly.

## See also

\[get_pumf()\], \[pumf_metadata()\]

## Examples

``` r
if (interactive()) {
# Open by series and version
open_pumf_documentation("SFS", "2019")

# Open from an existing tbl (reads provenance automatically)
sfs <- get_pumf("SFS", "2019")
open_pumf_documentation(sfs)
close_pumf(sfs)

# French documentation
open_pumf_documentation("SFS", "2019", lang = "fra")
}
#> Documentation for SFS 2019: 
#> 
#> 1: PUMF User Guide_2019_English.pdf
#> 2: SFS2019_PUMF_E.pdf
#> 3: How to cite SFS2019_Comment citer ESF2019.pdf
#> 4: Open all
#> 
#> Enter an item from the menu, or 0 to exit
#> Documentation for SFS 2019: 
#> 
#> 1: PUMF User Guide_2019_English.pdf
#> 2: SFS2019_PUMF_E.pdf
#> 3: How to cite SFS2019_Comment citer ESF2019.pdf
#> 4: Open all
#> 
#> Documentation for SFS 2019: 
#> 
#> 1: How to cite SFS2019_Comment citer ESF2019.pdf
#> 2: PUMF User Guide_2019_English.pdf
#> 3: SFS2019_PUMF_E.pdf
#> 4: Open all
#> 
```
