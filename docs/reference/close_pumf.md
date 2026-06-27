# Close the DuckDB connection backing a PUMF lazy table

Disconnects the DuckDB connection associated with \`x\`. \`x\` may be
either a lazy \`dplyr::tbl()\` returned by \[get_pumf()\] (the
connection embedded in the tbl is closed) or a DuckDB connection object
returned by \[get_pumf_connection()\] (closed directly). After calling
this function the table or connection can no longer be queried.

## Usage

``` r
close_pumf(x)
```

## Arguments

- x:

  A lazy \`dplyr::tbl()\` returned by \[get_pumf()\], or a DuckDB
  connection returned by \[get_pumf_connection()\]. \`NULL\` is accepted
  and is a no-op, so \`close_pumf()\` can be called unconditionally on a
  \[get_pumf()\] result that may be \`NULL\` (e.g. when Statistics
  Canada was unreachable).

## Value

Invisibly \`NULL\`.

## Details

All lazy tables and sibling modules opened from one \[get_pumf()\] call
share a single connection, so a single \`close_pumf()\` on any of them
releases it.

Closing is only necessary when you need to release the file lock – for
example, before calling \`get_pumf(..., refresh = TRUE)\` on the same
survey, or before writing to the DuckDB from another process. Read-only
connections (the default) do not block other readers.

## See also

\[get_pumf()\], \[get_pumf_connection()\]

## Examples

``` r
# \donttest{
sfs <- get_pumf("SFS", "2019")
if (!is.null(sfs)) {
  # ... analysis ...
  close_pumf(sfs)
}

# Also accepts a raw connection from get_pumf_connection()
con <- get_pumf_connection("SHS", "2017")
#> Connected to DuckDB (read-write). Available tables: eng_Diary, eng_Interview.
#> Disconnect with DBI::dbDisconnect(con, shutdown = TRUE) when done.
if (!is.null(con)) {
  DBI::dbListTables(con)
  close_pumf(con)
}
# }
```
