# Find the registered sibling whose config best fits an unregistered year

Considers only plain four-digit-year keys (\`series/2023\`) so
multi-part versions (Census \`1971/individuals_prov\`) never inherit
across types. Prefers the newest sibling not later than \`version\`; if
the requested year predates every entry, falls back to the oldest
registered sibling.

## Usage

``` r
.pumf_registry_newest_sibling(series, version)
```

## Value

the chosen sibling version string, or \`NULL\` if no year-keyed sibling
