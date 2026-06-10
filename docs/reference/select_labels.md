# Select language-specific labels from a metadata list

Picks `label_en` or `label_fr` based on `lang` and adds a `label` column
to `variables` and `codes`. When `lang = "fra"` and individual
`label_fr` values are `NA`, those entries fall back to `label_en` and a
warning is emitted listing the affected variable names.

## Usage

``` r
select_labels(metadata, lang = "eng")
```

## Arguments

- metadata:

  List from
  [`read_metadata()`](https://mountainmath.github.io/canpumf/reference/read_metadata.md).

- lang:

  `"eng"` (default) or `"fra"`.

## Value

Modified metadata list with an additional `label` column in `variables`
and `codes`.
