# Summarise bootstrap weight tables present in a PUMF DuckDB database

Queries the DuckDB file backing a PUMF lazy table for bootstrap weight
tables created by \[add_bootstrap_weights()\] and returns a
one-row-per-table summary tibble. Returns an empty tibble (invisibly)
when no BSW tables are found.

## Usage

``` r
bsw_info(tbl)
```

## Arguments

- tbl:

  A lazy \`dplyr::tbl()\` returned by \[get_pumf()\] or by
  \[add_bootstrap_weights()\].

## Value

A tibble with columns:

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

## See also

\[add_bootstrap_weights()\], \[remove_bootstrap_weights()\]

## Examples

``` r
# \donttest{
sfs <- get_pumf("SFS", "2019")
if (!is.null(sfs)) {
  sfs_bsw <- add_bootstrap_weights(sfs, weight_col = "PWEIGHT", seed = 1L)
  bsw_info(sfs_bsw)
  close_pumf(sfs_bsw)
}
#> Adding replicates 201-500 to BSW table...
#>   Replicate 230 / 500 ...
#>   Replicate 260 / 500 ...
#>   Replicate 290 / 500 ...
#>   Replicate 320 / 500 ...
#>   Replicate 350 / 500 ...
#>   Replicate 380 / 500 ...
#>   Replicate 410 / 500 ...
#>   Replicate 440 / 500 ...
#>   Replicate 470 / 500 ...
#>   Replicate 500 / 500 ...
#> Writing bootstrap weight table 'pumf_bsw_pweight' to DuckDB...
# }
```
