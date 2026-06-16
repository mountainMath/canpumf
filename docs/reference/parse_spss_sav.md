# Parse an SPSS `.sav` data file for embedded metadata

Uses
[`haven::read_sav()`](https://haven.tidyverse.org/reference/read_spss.html)
to extract variable labels, value labels, and SPSS format codes (which
give type and decimal precision) without loading any data rows. This is
the primary metadata source for surveys such as CIS 2016/2017 that ship
only a `.sav` file and no separate SPSS command files.

## Usage

``` r
parse_spss_sav(sav_path)
```

## Arguments

- sav_path:

  Path to the `.sav` file.

## Value

Named list with elements `variables`, `codes`, and `layout` (always
`NULL` – `.sav` files embed metadata but use a binary record format that
does not translate to start/end positions).
