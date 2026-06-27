# List Statistics Canada PUMF datasets supported by canpumf

Returns a tibble of all survey series and versions for which canpumf has
download wrappers. Scrapes the StatCan website to discover Census
versions; other series are hard-coded. Requires an internet connection.

## Usage

``` r
list_canpumf_collection()
```

## Value

A tibble with columns \`Title\`, \`Acronym\`, \`Version\`, \`Survey
Number\`, and \`url\`. The \`url\` column contains the download URL or
\`"(EFT)"\` for versions distributed via the Research Data Centre (EFT
only). Pass \`Acronym\` and \`Version\` to \[get_pumf()\] to download a
dataset.

## See also

\[get_pumf()\], \[list_available_lfs_pumf_versions()\]

## Examples

``` r
# \donttest{
collection <- list_canpumf_collection()
# Show all SFS versions
collection[collection$Acronym == "SFS", c("Acronym", "Version")]
#> # A tibble: 6 × 2
#>   Acronym Version
#>   <chr>   <chr>  
#> 1 SFS     1999   
#> 2 SFS     2005   
#> 3 SFS     2012   
#> 4 SFS     2016   
#> 5 SFS     2019   
#> 6 SFS     2023   
# }
```
