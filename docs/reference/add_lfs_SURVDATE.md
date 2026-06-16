# Add a date column to an LFS table

Creates a date column set to the first day of the survey month and
inserts it immediately after the survey month column. Works on both
unlabelled tables (columns \`SURVYEAR\` / \`SURVMNTH\`, date column
named \`SURVDATE\`) and labelled tables produced by
\[label_pumf_columns()\] (columns \`"Survey year"\` / \`"Survey
month"\`, date column named \`"Survey date"\`).

## Usage

``` r
add_lfs_SURVDATE(tbl)
```

## Arguments

- tbl:

  A lazy \`dplyr::tbl()\` returned by \[get_pumf()\] for an LFS survey,
  optionally passed through \[label_pumf_columns()\].

## Value

The same lazy table with a new date column positioned after the survey
month column.

## See also

\[get_pumf()\], \[label_pumf_columns()\], \[add_lfs_GENDER_SEX()\]

## Examples

``` r
if (FALSE) { # \dontrun{
# Unlabelled
lfs <- get_pumf("LFS", "2023")
lfs |> add_lfs_SURVDATE() |> dplyr::select(SURVYEAR, SURVMNTH, SURVDATE) |>
  dplyr::distinct() |> dplyr::collect()

# Labelled
lfs |> label_pumf_columns() |> add_lfs_SURVDATE() |>
  dplyr::select(`Survey year`, `Survey month`, `Survey date`) |>
  dplyr::distinct() |> dplyr::collect()

close_pumf(lfs)
} # }
```
