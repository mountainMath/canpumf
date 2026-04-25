# Add bootstrap weights to PUMF data

Add bootstrap weights to PUMF data

## Usage

``` r
add_bootstrap_weights(
  pumf_data,
  weight_column,
  bootstrap_weight_count = 16,
  bootstrap_weight_prefix = "BSW",
  algorithm = "iterative",
  seed = NULL
)
```

## Arguments

- pumf_data:

  A dataframe with PUMF data

- weight_column:

  Name of the column with the standard weights

- bootstrap_weight_count:

  Number of boostrap weights to generate

- bootstrap_weight_prefix:

  Name prefix for the bootstrap weight columns

- algorithm:

  Algorithm to calculate bootstrap weights, either of "iterative" or
  "experimental"

- seed:

  Random see to be used for bootstrap sample for reproducibility

## Value

pumf_base_dir that can be used in the other package functions
