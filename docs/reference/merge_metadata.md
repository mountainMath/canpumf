# Merge metadata from multiple parser outputs

Sources are applied in priority order: `spss_mono` \> `spss_split` \>
`sas_cards` \> `lfs_csv` \> `cpss_csv`. For each variable / code, the
highest-priority source provides the English label; French labels fill
in from lower-priority sources when missing. Conflicting missing ranges
or English code labels emit warnings.

## Usage

``` r
merge_metadata(parsed_list)
```

## Arguments

- parsed_list:

  Named list of parser outputs (each a list with elements `variables`,
  `codes`, and `layout`).

## Value

Single merged canonical metadata list.
