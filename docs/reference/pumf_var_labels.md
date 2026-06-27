# Retrieve variable labels as a tibble

Returns a tibble mapping short coded column names to their bilingual
human-readable variable labels. Use this as a quick reference without
renaming the table itself; to rename, use \[label_pumf_columns()\].

## Usage

``` r
pumf_var_labels(tbl)
```

## Arguments

- tbl:

  A lazy \`dplyr::tbl()\` returned by \[get_pumf()\].

## Value

A tibble with columns \`name\` (coded column name), \`label_en\`
(English label), and \`label_fr\` (French label). Rows follow
survey-metadata order.

## See also

\[label_pumf_columns()\], \[get_pumf()\]

## Examples

``` r
# \donttest{
sfs <- get_pumf("SFS", "2019")
if (!is.null(sfs)) {
  pumf_var_labels(sfs)
  close_pumf(sfs)
}
# }
```
