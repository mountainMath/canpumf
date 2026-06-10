# Parse a set of SAS/SPSS reading-card files for PUMF metadata

Handles surveys where metadata is stored in reading-card files: `.lay`
(column positions), `.lbe` (English variable labels), `.cde` (English
value labels), `.mvs` (missing values), with optional `.lbf` / `.cdf`
for French.

## Usage

``` r
parse_sas_cards(cards_dir, layout_mask = NULL, encoding = "Latin1")
```

## Arguments

- cards_dir:

  Path to the directory containing the reading-card files.

- layout_mask:

  Optional string; used to filter when multiple files share the same
  extension (e.g. SHS 2017 has separate interview and diary files).

- encoding:

  Character encoding (e.g. `"Latin1"`, `"CP1252"`).

## Value

A list with elements `variables`
(`name, label_en, label_fr, type, missing_low, missing_high`), `codes`
(`name, val, label_en, label_fr`), and `layout` (`name, start, end`;
`NULL` when no `.lay` file is found).

## Details

The `.lay` file supports two sub-formats detected automatically:

- Reading-card:

  Lines like `NAME start - end (A)`

- SAS input:

  Lines like `@pos NAME \$CHAR6.`
