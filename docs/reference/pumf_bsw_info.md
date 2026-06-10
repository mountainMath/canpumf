# Summarise bootstrap weight tables present in a PUMF DuckDB database

Queries the DuckDB file backing a PUMF lazy table for bootstrap weight
tables created by \[add_pumf_bootstrap_weights()\] and returns a summary
tibble with one row per BSW table found.

## Usage

``` r
pumf_bsw_info(tbl)
```

## Arguments

- tbl:

  A lazy \`dplyr::tbl()\` returned by \[get_pumf()\] or by
  \[add_pumf_bootstrap_weights()\].

## Value

A tibble (invisibly when empty) with columns:

- \`weight_col\`:

  The weight column the BSW table was built from (matched back to the
  case used in the main survey table).

- \`bsw_table\`:

  Name of the DuckDB table storing the weights.

- \`view_name\`:

  Name of the DuckDB VIEW joining survey + BSW.

- \`view_exists\`:

  Whether the companion VIEW is present.

- \`n_replicates\`:

  Number of bootstrap replicate columns.

- \`size_mb\`:

  Estimated table size in megabytes (from DuckDB metadata; \`NA\` when
  unavailable).
