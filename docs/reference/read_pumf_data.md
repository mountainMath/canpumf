# Parse PUMF data

Parse PUMF data

## Usage

``` r
read_pumf_data(
  pumf_base_path,
  layout_mask = NULL,
  file_mask = layout_mask,
  guess_numeric = TRUE
)
```

## Arguments

- pumf_base_path:

  optional base path, guessed from attributes on `pumf_data`

- layout_mask:

  optional layout mask in case there are several layout files, guessed
  from attributes on `layout_mask`

- file_mask:

  optional additional mask to filter down to specific PUMF file if there
  are several

- guess_numeric:

  logical, will guess numeric columns and covert to numeric and set
  missing values to `NA` if set to `TRUE` (default)

## Value

data frame with one row for each case in the PUMF data
