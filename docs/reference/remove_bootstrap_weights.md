# Remove bootstrap weight tables and views from a PUMF DuckDB database

Drops the bootstrap weight table(s) created by
\[add_bootstrap_weights()\] and their companion VIEWs from the DuckDB
file. When all BSW tables have been removed and the main survey table
has a \`pumf_row_id\` column (added automatically by
\[add_bootstrap_weights()\] when no natural key was available), that
column is also dropped.

## Usage

``` r
remove_bootstrap_weights(tbl, weight_col = NULL)
```

## Arguments

- tbl:

  A lazy \`dplyr::tbl()\` returned by \[get_pumf()\] or by
  \[add_bootstrap_weights()\].

- weight_col:

  Name of the weight column whose BSW table should be removed (e.g.
  \`"PWEIGHT"\`). If \`NULL\` (default), \*\*all\*\* bootstrap weight
  tables (and their companion VIEWs) are removed.

## Value

A lazy \`dplyr::tbl()\` backed by the original physical survey table
(without BSW columns), with a fresh read-only DuckDB connection.

## Details

Like \[add_bootstrap_weights()\], this function requires brief exclusive
write access: the read-only connection backing \`tbl\` is shut down, the
tables are dropped, and a fresh read-only connection is returned.

## See also

\[add_bootstrap_weights()\], \[bsw_info()\], \[get_pumf()\]

## Examples

``` r
# \donttest{
sfs <- get_pumf("SFS", "2019")
if (!is.null(sfs)) {
  sfs_bsw <- add_bootstrap_weights(sfs, weight_col = "PWEIGHT", seed = 1L)
  # Remove only the PWEIGHT BSW table
  sfs_clean <- remove_bootstrap_weights(sfs_bsw, weight_col = "PWEIGHT")
  close_pumf(sfs_clean)
}
#> Dropping view 'eng_EFAM_PUMF_bsw_pweight'...
#> Dropping bootstrap weight table 'pumf_bsw_pweight'...
# }
```
