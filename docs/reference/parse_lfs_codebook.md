# Parse an LFS codebook CSV into canonical metadata

The LFS codebook is a single bilingual CSV with rows alternating between
variable definitions (`Field_Champ` filled) and code values
(`Field_Champ` NA).

## Usage

``` r
parse_lfs_codebook(codebook_path, encoding = "CP1252")
```

## Arguments

- codebook_path:

  Path to the LFS `codebook.csv` file.

- encoding:

  File encoding (default `"CP1252"`).

## Value

Named list with elements `variables`, `codes`, and `layout` (always
`NULL` for LFS CSV data).
