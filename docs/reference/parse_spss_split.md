# Parse a set of SPSS split command files for PUMF metadata

Handles surveys where metadata is spread across separate files:
`*_i.sps` (layout), `*vare.sps` (English variable labels), `*vale.sps`
(English value labels), `*miss.sps` (missing values), with optional
`*varf.sps` / `*valf.sps` for French.

## Usage

``` r
parse_spss_split(layout_dir, layout_mask = NULL, encoding = "Latin1")
```

## Arguments

- layout_dir:

  Path to the directory containing the SPSS split files.

- layout_mask:

  Optional string or regex; passed to `find_unique_layout_file()` to
  disambiguate when multiple sets of split files exist (e.g. SFS has
  both `EFAM_PUMF_*` and `bsweights_pumf_*` files in the same
  directory).

- encoding:

  Character encoding, e.g. `"Latin1"` or `"CP1252"`.

## Value

A list with elements `variables`
(`name, label_en, label_fr, type, missing_low, missing_high`), `codes`
(`name, val, label_en, label_fr`), and `layout` (`name, start, end`;
`NULL` if no layout file is found).
