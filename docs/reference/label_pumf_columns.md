# Rename PUMF table columns to human-readable variable labels

Takes a lazy \`dplyr::tbl()\` returned by \[get_pumf()\] and returns the
same lazy table with column names replaced by the variable labels from
the survey metadata (e.g. \`PHHSIZE\` → \`"Household size"\`).

## Usage

``` r
label_pumf_columns(tbl)
```

## Arguments

- tbl:

  A lazy \`dplyr::tbl()\` returned by \[get_pumf()\].

## Value

A lazy \`dplyr::tbl()\` with renamed columns.

## Details

Duplicate labels are disambiguated by appending \` (VAR_NAME)\`. Columns
with no label (e.g. internal DuckDB columns) are left unchanged.

The tbl must have been produced by \[get_pumf()\]; the function reads
survey provenance (series, version, cache path, language) from the
underlying DuckDB connection.
