# Rename to human readable column names

Rename to human readable column names

## Usage

``` r
label_pumf_columns(
  pumf_data,
  pumf_base_path = attr(pumf_data, "pumf_base_path"),
  layout_mask = attr(pumf_data, "layout_mask")
)
```

## Arguments

- pumf_data:

  pumf data file

- pumf_base_path:

  optional base path, guessed from attributes on `pumf_data`

- layout_mask:

  optional layout mask in case there are several layout files, guessed
  from attributes on `layout_mask`

## Value

data frame with renamed columns
