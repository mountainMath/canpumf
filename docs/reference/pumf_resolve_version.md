# Resolve version aliases

Canonicalises user-supplied version strings for Census of Population.
Any string starting with a four-digit year is parsed flexibly: the file
type is detected by grepping for "hierarchical", "household", or "famil"
(defaulting to "individuals"), and CMA vs provincial by grepping for
"cma". The registry is then probed to find the correct canonical format
for that year (e.g. \`"1971/households_cma"\`, \`"1986/households"\`, or
\`"2001 (households)"\`).

## Usage

``` r
pumf_resolve_version(series, version)
```

## Arguments

- series:

  survey series acronym

- version:

  raw version string supplied by the caller, or \`NULL\`

## Value

canonical version string (or \`NULL\` if \`version\` was \`NULL\`)

## Details

Examples of accepted inputs (case-insensitive keywords): - \`"2021"\` →
\`"2021 (individuals)"\` - \`"1971"\` → \`"1971/individuals_prov"\` -
\`"1971 CMA"\` → \`"1971/individuals_cma"\` - \`"1971 households CMA"\`
→ \`"1971/households_cma"\` - \`"1986 families"\` → \`"1986/families"\`
