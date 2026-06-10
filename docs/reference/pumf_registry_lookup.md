# Look up survey registry configuration

Returns the configuration entry for a given survey series and version,
or \`NULL\` if the survey falls back to auto-detection.

## Usage

``` r
pumf_registry_lookup(series, version)
```

## Arguments

- series:

  survey series acronym (e.g. \`"SFS"\`)

- version:

  survey version string (e.g. \`"2019"\`)

## Value

named list of configuration fields, or \`NULL\` if not in registry
