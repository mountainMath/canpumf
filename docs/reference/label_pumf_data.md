# Add variable labels and rename to human readable column names

Add variable labels and rename to human readable column names

## Usage

``` r
label_pumf_data(
  pumf_data,
  pumf_base_path = attr(pumf_data, "pumf_base_path"),
  layout_mask = attr(pumf_data, "layout_mask"),
  rename_columns = FALSE,
  infer_missing_numeric = FALSE
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

- rename_columns:

  rename PUMF columns to human readable names, default is \`FALSE\`

- infer_missing_numeric:

  optional character, infer variables that aren't labelled to be numeric

## Value

relabeled data frame
