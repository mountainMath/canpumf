# Retrieve variable labels as a tibble

Returns a tibble mapping short coded column names to their
human-readable variable labels, as a handy reference without renaming
the table.

## Usage

``` r
pumf_var_labels(tbl)
```

## Arguments

- tbl:

  A lazy \`dplyr::tbl()\` returned by \[get_pumf()\].

## Value

A tibble with columns \`name\` (coded column name), \`label_en\`, and
\`label_fr\`. Rows are in survey-metadata order.
