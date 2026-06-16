# Detect parseable metadata formats in a PUMF directory tree

Walks the directory tree under `pumf_dir` and returns every recognised
command-file format. Multiple formats may be present in one release; all
are returned so that the caller can run all applicable parsers and merge
results.

## Usage

``` r
detect_formats(pumf_dir, sps_mask = NULL)
```

## Arguments

- pumf_dir:

  Top-level version directory (i.e. the directory that contains the
  extracted PUMF zip contents).

## Value

A named list (possibly empty) whose elements are a subset of
`"lfs_csv"`, `"cpss_csv"`, `"sas_cards"`, `"spss_split"`, and
`"spss_mono"`. Each element is either a single path string or (for
`spss_mono`) a `list(eng = ..., fra = ...)` where `fra` is `NULL` when
no French file is found.
