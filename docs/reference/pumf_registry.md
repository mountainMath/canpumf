# Inspect a survey's registry configuration

Returns the resolved configuration entry for a \`(series, version)\`
pair: the built-in registry entry when one exists, otherwise an
all-default entry. Useful for understanding the parsing strategy and
overrides applied to a survey, and as a template for
\[pumf_registry_entry()\].

## Usage

``` r
pumf_registry(series, version)
```

## Arguments

- series:

  Survey series acronym, e.g. \`"SFS"\`.

- version:

  Version string, e.g. \`"2019"\`.

## Value

A classed \`"pumf_registry_entry"\` list of all configuration fields.

## See also

\[pumf_registry_entry()\], \[list_pumf_registry()\], \[get_pumf()\]

## Examples

``` r
pumf_registry("SFS", "2019")
#> <pumf_registry_entry> SFS 2019
#>   file_mask:         EFAM_PUMF\.txt
#>   layout_mask:       EFAM_PUMF
#>   data_encoding:     CP1252
#>   metadata_encoding: CP1252
#>   bsw_mask:          bsweights
#>   bsw_file_mask:     BSWEIGHTS_PUMF\.txt
#>   bsw_join_key:      PEFAMID
#>   bsw_drop_cols:     PWEIGHT
```
