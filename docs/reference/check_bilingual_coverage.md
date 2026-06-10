# Warn when French label coverage is partial

Emits a warning when French files were clearly present (some `label_fr`
are non-NA) but more than `threshold` of variable labels are missing.
Surveys with no French files at all (all `label_fr` are `NA`) do not
trigger the warning.

## Usage

``` r
check_bilingual_coverage(metadata, threshold = 0.2)
```

## Arguments

- metadata:

  List from
  [`read_metadata()`](https://mountainmath.github.io/canpumf/reference/read_metadata.md).

- threshold:

  Proportion of missing `label_fr` above which a warning is emitted
  (default 0.2).

## Value

`NULL` invisibly.
