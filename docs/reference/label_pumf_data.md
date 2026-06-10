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

  A lazy \`dplyr::tbl()\` returned by \[get_pumf()\].

- pumf_base_path:

  Deprecated, ignored.

- layout_mask:

  Deprecated, ignored.

- rename_columns:

  If \`TRUE\`, rename columns to human-readable labels via
  \[label_pumf_columns()\]. Default \`FALSE\`.

- infer_missing_numeric:

  Deprecated, ignored.

## Value

\`pumf_data\` unchanged, or with columns renamed if \`rename_columns =
TRUE\`.
