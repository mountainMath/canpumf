# Close the DuckDB connection backing a PUMF lazy table

Disconnects the DuckDB connection embedded in a lazy \`dplyr::tbl()\`
returned by \[get_pumf()\]. After calling this function the table can no
longer be queried.

## Usage

``` r
close_pumf(tbl)
```

## Arguments

- tbl:

  A lazy \`dplyr::tbl()\` returned by \[get_pumf()\].

## Value

Invisibly \`NULL\`.

## Details

Closing is only necessary when you need to release the file lock – for
example, before calling \`get_pumf(..., refresh = TRUE)\` on the same
survey, or before writing to the DuckDB from another process. Read-only
connections (the default) do not block other readers.

## See also

\[get_pumf()\]

## Examples

``` r
if (FALSE) { # \dontrun{
sfs <- get_pumf("SFS", "2019")
# ... analysis ...
close_pumf(sfs)
} # }
```
