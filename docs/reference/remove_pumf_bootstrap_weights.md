# Remove bootstrap weight tables and views from a PUMF DuckDB database

Drops the bootstrap weight table(s) created by
\[add_pumf_bootstrap_weights()\] and their companion VIEWs from the
DuckDB file. When all BSW tables have been removed and the main survey
table has a \`pumf_row_id\` column (added automatically by
\[add_pumf_bootstrap_weights()\] when no natural key was available),
that column is also dropped.

## Usage

``` r
remove_pumf_bootstrap_weights(tbl, weight_col = NULL)
```

## Arguments

- tbl:

  A lazy \`dplyr::tbl()\` returned by \[get_pumf()\] or by
  \[add_pumf_bootstrap_weights()\].

- weight_col:

  Name of the weight column whose BSW table should be removed (e.g.
  \`"WSTPWGT"\`). If \`NULL\` (default), \*\*all\*\* bootstrap weight
  tables (and their companion VIEWs) are removed.

## Value

A lazy \`dplyr::tbl()\` backed by the original physical survey table
(without BSW columns).

## Details

Like \[add_pumf_bootstrap_weights()\], this function requires brief
exclusive write access: the read-only connection backing \`tbl\` is shut
down, the tables are dropped, and a fresh read-only connection is
returned.
