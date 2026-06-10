# Parse a CPSS variables.csv into canonical metadata

The CPSS `variables.csv` is a single bilingual CSV. Variable rows have
the `Variable` column filled; code rows have `Variable` empty and the
`Code` column filled.

## Usage

``` r
parse_cpss_csv(variables_path, encoding = "Latin1")
```

## Arguments

- variables_path:

  Path to the CPSS `variables.csv` file.

- encoding:

  File encoding (default `"Latin1"`).

## Value

Named list with elements `variables`, `codes`, and `layout` (always
`NULL` for CPSS CSV data).
