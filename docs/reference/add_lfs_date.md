# Add a DATE column to an LFS table

Creates a \`DATE\` column set to the first day of the survey month (from
\`SURVYEAR\` and \`SURVMNTH\`) and inserts it immediately after
\`SURVMNTH\`.

## Usage

``` r
add_lfs_date(tbl)
```

## Arguments

- tbl:

  A lazy \`dplyr::tbl()\` returned by \[get_pumf()\] for an LFS survey.
  Must contain integer columns \`SURVYEAR\` and \`SURVMNTH\`.

## Value

The same lazy table with a new \`DATE\` column (type \`Date\`)
positioned after \`SURVMNTH\`.

## See also

\[get_pumf()\], \[add_gender_sex()\]

## Examples

``` r
if (FALSE) { # \dontrun{
lfs <- get_pumf("LFS", "2023")
lfs |> add_lfs_date() |> dplyr::select(SURVYEAR, SURVMNTH, DATE) |>
  dplyr::distinct() |> dplyr::collect()
close_pumf(lfs)
} # }
```
