# Read canonical PUMF metadata from CSV files

Read canonical PUMF metadata from CSV files

## Usage

``` r
read_metadata(metadata_dir)
```

## Arguments

- metadata_dir:

  Path to the `metadata/` directory.

## Value

A list with elements `variables`, `codes`, and `layout` (NULL when no
`layout.csv` is present).
