# Rename PUMF table columns to human-readable variable labels

Takes a lazy \`dplyr::tbl()\` returned by \[get_pumf()\] and returns the
same lazy table with column names replaced by the variable labels from
the survey metadata (e.g. \`PHHSIZE\` becomes \`"Household size"\`).
Duplicate labels are disambiguated by appending \` (VAR_NAME)\`.

## Usage

``` r
label_pumf_columns(tbl)
```

## Arguments

- tbl:

  A lazy \`dplyr::tbl()\` returned by \[get_pumf()\].

## Value

A lazy \`dplyr::tbl()\` with column names replaced by human-readable
variable labels. Columns with no metadata label are left unchanged.

## Details

The \`tbl\` must have been produced by \[get_pumf()\]; the function
reads survey provenance (series, version, cache path, language) from the
underlying DuckDB connection. Use \[pumf_var_labels()\] to inspect the
name-to-label mapping without renaming.

## See also

\[pumf_var_labels()\], \[get_pumf()\]

## Examples

``` r
# \donttest{
sfs <- get_pumf("SFS", "2019")
if (!is.null(sfs)) {
  sfs_labeled <- label_pumf_columns(sfs)
  colnames(sfs_labeled)
  close_pumf(sfs_labeled)
}
# }
```
