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
link).

## See also

\[get_pumf()\], \[list_canpumf_collection()\]

## Examples

``` r
if (FALSE) { # \dontrun{
lfs_versions <- list_available_lfs_pumf_versions()
tail(lfs_versions)
} # }
```
