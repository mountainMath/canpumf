# Write canonical PUMF metadata to CSV files

Write canonical PUMF metadata to CSV files

## Usage

``` r
write_metadata(metadata, metadata_dir)
```

## Arguments

- metadata:

  A list with elements `variables`, `codes`, and optionally `layout`.
  Each is a tibble matching the canonical schema.

- metadata_dir:

  Path to the `metadata/` directory; created if absent.

## Value

`metadata_dir` invisibly.
