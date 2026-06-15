# Add a harmonised gender/sex column to an LFS table

LFS introduced \`GENDER\` (with values \`"Men+"\` / \`"Women+"\` /
\`"Non-binary persons"\`) to replace the binary \`SEX\` variable
(\`"Male"\` / \`"Female"\`) starting in 2020. In any given row exactly
one of the two columns is non-\`NA\`. \`add_lfs_GENDER_SEX()\` coalesces
them into a single harmonised column, recoding \`SEX\` values to the
\`GENDER\` scale so the result is consistent across all LFS vintages.

## Usage

``` r
add_lfs_GENDER_SEX(tbl)
```

## Arguments

- tbl:

  A lazy \`dplyr::tbl()\` returned by \[get_pumf()\] for an LFS survey,
  optionally passed through \[label_pumf_columns()\].

## Value

The same lazy table with a new harmonised gender/sex column.

## Details

Works on both unlabelled tables (columns \`SEX\` / \`GENDER\`, output
column named \`GENDER_SEX\`) and labelled tables produced by
\[label_pumf_columns()\] (columns \`"Sex of respondent"\` / \`"Gender of
respondent"\`, output column named \`"Gender/sex of respondent"\`).

The mapping applied to \`SEX\` / \`"Sex of respondent"\` when the gender
column is \`NA\`:

- \`"Male"\` \\\rightarrow\\ \`"Men+"\`

- \`"Female"\` \\\rightarrow\\ \`"Women+"\`

The output column is inserted after \`GENDER\` / \`"Gender of
respondent"\` when present, or after \`SEX\` / \`"Sex of respondent"\`
otherwise.

## See also

\[get_pumf()\], \[label_pumf_columns()\], \[add_lfs_SURVDATE()\]

## Examples

``` r
if (FALSE) { # \dontrun{
lfs <- get_pumf("LFS")

# Unlabelled
lfs |> add_lfs_GENDER_SEX() |>
  dplyr::count(SEX, GENDER, GENDER_SEX) |> dplyr::collect()

# Labelled
lfs |> label_pumf_columns() |> add_lfs_GENDER_SEX() |>
  dplyr::count(`Sex of respondent`, `Gender of respondent`,
               `Gender/sex of respondent`) |> dplyr::collect()

close_pumf(lfs)
} # }
```
