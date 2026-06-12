# Add a harmonised GENDER_SEX column to an LFS table

LFS introduced \`GENDER\` (with values \`"Men+"\` / \`"Women+"\` /
\`"Non-binary persons"\`) to replace the binary \`SEX\` variable
(\`"Male"\` / \`"Female"\`) starting in 2020. In any given row exactly
one of the two columns is non-\`NA\`. \`add_gender_sex()\` coalesces
them into a single \`GENDER_SEX\` column, recoding \`SEX\` values to the
\`GENDER\` scale so the result is consistent across all LFS vintages.

## Usage

``` r
add_gender_sex(tbl)
```

## Arguments

- tbl:

  A lazy \`dplyr::tbl()\` returned by \[get_pumf()\] for an LFS survey.
  Must contain at least one of \`SEX\` or \`GENDER\`.

## Value

The same lazy table with a new \`GENDER_SEX\` column positioned after
\`GENDER\` (if present) or after \`SEX\`.

## Details

The mapping applied to \`SEX\` when \`GENDER\` is \`NA\`:

- \`"Male"\` → \`"Men+"\`

- \`"Female"\` → \`"Women+"\`

\`GENDER_SEX\` is inserted after \`GENDER\` when that column is present,
or after \`SEX\` otherwise.

## See also

\[get_pumf()\], \[add_lfs_date()\]

## Examples

``` r
if (FALSE) { # \dontrun{
lfs <- get_pumf("LFS")
lfs |> add_gender_sex() |>
  dplyr::count(SEX, GENDER, GENDER_SEX) |> dplyr::collect()
close_pumf(lfs)
} # }
```
