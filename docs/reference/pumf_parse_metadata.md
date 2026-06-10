# Parse all metadata from a PUMF version directory

Detects every parseable command-file format in `version_dir`, runs all
applicable parsers, merges the results into the canonical schema, and
writes `metadata/variables.csv`, `metadata/codes.csv` (and optionally
`metadata/layout.csv`) under `version_dir`.

## Usage

``` r
pumf_parse_metadata(
  version_dir,
  layout_mask = NULL,
  metadata_encoding = NULL,
  refresh = FALSE
)
```

## Arguments

- version_dir:

  Path to the extracted version directory.

- layout_mask:

  Optional string to disambiguate when multiple command-file sets
  coexist in one directory (e.g. `"CDN"` for Census); passed through to
  [`parse_spss_split`](https://mountainmath.github.io/canpumf/reference/parse_spss_split.md)
  and
  [`parse_sas_cards`](https://mountainmath.github.io/canpumf/reference/parse_sas_cards.md).

- refresh:

  If `TRUE`, re-parse even if cached metadata exists.

## Value

`metadata_dir` path invisibly.

## Details

Idempotent: skips parsing if `metadata/variables.csv` already exists and
`refresh = FALSE`.
