# List available PUMF LFS versions

Scrapes the StatCan LFS publication page and returns a tibble of all
available LFS PUMF versions with their download URLs.

## Usage

``` r
list_available_lfs_pumf_versions()
```

## Value

A tibble with columns \`Date\`, \`version\`, and \`url\`.
