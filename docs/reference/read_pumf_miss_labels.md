# Read PUMF missing data information

Read PUMF missing data information

## Usage

``` r
read_pumf_miss_labels(pumf_base_path, layout_mask = NULL, numeric_only = TRUE)
```

## Arguments

- pumf_base_path:

  pumf base path

- layout_mask:

  optional path or mask for the layout file in case there are several,

- numeric_only:

  options, only returns missing data for numeric columns, defaul is
  \`TRUE\`.

## Value

tibble with missing data information
