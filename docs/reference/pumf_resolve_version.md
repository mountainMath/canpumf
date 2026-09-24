# Resolve version aliases

Canonicalises user-supplied version strings for Census of Population.
Any string starting with a four-digit year is parsed flexibly: the file
type is detected by grepping for "hierarchical", "household", or "famil"
(defaulting to "individuals"), and CMA vs provincial by grepping for
"cma". The registry is then probed to find the correct canonical format
for that year (e.g. \`"1971 (households, CMA)"\`, \`"1986 (families)"\`,
or \`"2001 (households)"\`).

## Usage

``` r
pumf_resolve_version(
  series,
  version,
  cache_path = getOption("canpumf.cache_path", tempdir())
)
```

## Arguments

- series:

  survey series acronym

- version:

  raw version string supplied by the caller, or \`NULL\`

- cache_path:

  cache root, used to detect a deposited EFT bundle

## Value

canonical version string (or \`NULL\` if \`version\` was \`NULL\`)

## Details

The 1971–1986 vintages exist twice: as StatCan EFT bundles that must be
deposited by hand (\`"1971/individuals_cma"\`, \`"1986/families"\`, ...)
and as Borealis downloads (\`"1971 (individuals, CMA)"\`, \`"1986
(families)"\`, ...). A loose request resolves to the EFT key when that
year's bundle is present in \`cache_path\`, and to the Borealis key
otherwise. Adding \`"EFT"\` or \`"Borealis"\` to the version string
picks one explicitly, and an exact registry key is always returned
unchanged.

Examples of accepted inputs (case-insensitive keywords): - \`"2021"\`
-\> \`"2021 (individuals)"\` - \`"1971"\` -\> \`"1971 (individuals,
provincial)"\` (or \`"1971/individuals_prov"\` with the EFT bundle
deposited) - \`"1971 households CMA"\` -\> \`"1971 (households,
CMA)"\` - \`"1986 families EFT"\` -\> \`"1986/families"\`
