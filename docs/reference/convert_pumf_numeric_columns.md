# Convert columns to numeric and convert all missing values to `NA`

Convert columns to numeric and convert all missing values to `NA`

## Usage

``` r
convert_pumf_numeric_columns(
  pumf_data,
  pumf_base_path = attr(pumf_data, "pumf_base_path"),
  layout_mask = attr(pumf_data, "layout_mask"),
  numeric_columns = NULL,
  set_missing = TRUE,
  na.values = c(".")
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

- numeric_columns:

  optional list of columns to conver, guessed from data if none provided

- set_missing:

  logical, will set missing values to `NA` if set to `TRUE` (default)

- na.values:

  optional vectors of values to convert to `NA`

## Value

data frame with converted values
