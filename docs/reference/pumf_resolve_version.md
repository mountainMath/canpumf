# Resolve version aliases

Canonicalises user-supplied version strings. Currently the only alias is
for the Census of Population: a bare four-digit year (e.g. \`"2021"\`)
resolves to \`"\<year\> (individuals)"\`.

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
