# Guess which columns in pumf data are numeric

Guess which columns in pumf data are numeric

## Usage

``` r
guess_numeric_pumf_columns(
  pumf_base_path,
  layout_mask = NULL,
  numeric_pattern = "\\d+-\\d+|THRU 99"
)
```

## Arguments

- pumf_base_path:

  pumf base path

- layout_mask:

  optional layout mask in case there are several layout files,

- numeric_pattern:

  optional pattern to guess numeric columns from their `NA` value range

## Value

vector of column names
