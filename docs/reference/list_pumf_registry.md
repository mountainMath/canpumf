# Overview of all built-in registry entries

Overview of all built-in registry entries

## Usage

``` r
list_pumf_registry()
```

## Value

A tibble with one row per registered \`(series, version)\` and columns
summarising the key configuration: \`file_mask\`, \`layout_mask\`,
\`bsw_join_key\`, and \`data_fixups\` (comma-separated fixup types
present).

## See also

\[pumf_registry()\], \[pumf_registry_entry()\]

## Examples

``` r
list_pumf_registry()
#> # A tibble: 82 × 6
#>    series version file_mask         layout_mask    bsw_join_key data_fixups  
#>    <chr>  <chr>   <chr>             <chr>          <chr>        <chr>        
#>  1 SFS    2023    "EFAM_PUMF\\.txt" EFAM_PUMF_[^R] PEFAMID      NA           
#>  2 SFS    2019    "EFAM_PUMF\\.txt" EFAM_PUMF      PEFAMID      NA           
#>  3 SFS    2016    "EFAM_PUMF\\.txt" EFAM_PUMF      PEFAMID      NA           
#>  4 SFS    2012     NA               NA             NA           NA           
#>  5 SFS    2005    "ec2005ef\\.txt"  NA             NA           force_numeric
#>  6 SFS    1999    "ec1999ef\\.sdf"  NA             NA           NA           
#>  7 CIS    2022    "PUMF\\.txt"      NA             NA           NA           
#>  8 CIS    2021    "PUMF\\.txt"      NA             NA           NA           
#>  9 CIS    2020    "PUMF\\.txt"      NA             NA           NA           
#> 10 CIS    2019    "PUMF\\.txt"      NA             NA           NA           
#> # ℹ 72 more rows
```
