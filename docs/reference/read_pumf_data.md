# Read raw PUMF data as a tibble (deprecated)

\`r lifecycle::badge("deprecated")\`

This function is kept for users who work with manually-deposited
directories outside the standard cache. For all standard surveys use
\[get_pumf()\], which returns a lazy DuckDB table and handles labeling
automatically.

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

  Path to the extracted PUMF directory.

- layout_mask:

  Optional mask to select a specific layout file.

- file_mask:

  Optional mask to select a specific data file.

- guess_numeric:

  Logical; convert numeric columns and apply missing values. Default
  \`TRUE\`.

## Value

A tibble with attributes \`pumf_base_path\` and \`layout_mask\`.
