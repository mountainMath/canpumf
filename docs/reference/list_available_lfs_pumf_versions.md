# List available LFS PUMF versions

Scrapes the Statistics Canada LFS PUMF publication page and returns a
tibble of all available annual and monthly versions with their download
URLs. Requires an internet connection. For the broader collection of all
supported surveys see \[list_canpumf_collection()\].

## Usage

``` r
list_available_lfs_pumf_versions()
```

## Value

A tibble with columns \`Date\` (human-readable label from the StatCan
page), \`version\` (a string of the form \`"YYYY"\` for annual versions
or \`"YYYY-MM"\` for monthly versions), and \`url\` (direct download
link). If the StatCan website is unreachable the function returns an
empty tibble (with those columns) and a warning rather than erroring.

## See also

\[get_pumf()\], \[list_canpumf_collection()\]

## Examples

``` r
# \donttest{
lfs_versions <- list_available_lfs_pumf_versions()
tail(lfs_versions)
#> # A tibble: 6 × 3
#>   Date  version url                                                             
#>   <chr> <chr>   <chr>                                                           
#> 1 2011  2011    https://www150.statcan.gc.ca/n1/pub/71m0001x/2021001/hist/2011-…
#> 2 2010  2010    https://www150.statcan.gc.ca/n1/pub/71m0001x/2021001/hist/2010-…
#> 3 2009  2009    https://www150.statcan.gc.ca/n1/pub/71m0001x/2021001/hist/2009-…
#> 4 2008  2008    https://www150.statcan.gc.ca/n1/pub/71m0001x/2021001/hist/2008-…
#> 5 2007  2007    https://www150.statcan.gc.ca/n1/pub/71m0001x/2021001/hist/2007-…
#> 6 2006  2006    https://www150.statcan.gc.ca/n1/pub/71m0001x/2021001/hist/2006-…
# }
```
