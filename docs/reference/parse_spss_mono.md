# Parse a monolithic SPSS command file for PUMF metadata

Handles Census-style monolithic SPSS files where a single `.sps` file
contains `DATA LIST`, `FORMATS`, `VARIABLE LABELS`, `VALUE LABELS`, and
optionally `MISSING VALUES` sections. Supports both the 2021 style
(single-quoted labels, `+` continuation, `/VAR` headers in
`VALUE LABELS`) and the 2016 style (double-quoted labels, `/` on its own
line separating variable groups).

## Usage

``` r
parse_spss_mono(eng_sps_path, fra_sps_path = NULL, encoding = "Latin1")
```

## Arguments

- eng_sps_path:

  Path to the English `.sps` file.

- fra_sps_path:

  Optional path to the French `.sps` file. When provided, French labels
  are joined onto the canonical metadata. When `NULL` (default), all
  `label_fr` values are `NA`.

- encoding:

  Character encoding of the file(s), e.g. `"Latin1"`, `"CP1252"`, or
  `"UTF-8"`.

## Value

A list with elements `variables`
(`name, label_en, label_fr, type, missing_low, missing_high`), `codes`
(`name, val, label_en, label_fr`), and `layout` (`name, start, end`;
`NULL` when no `DATA LIST` section is present).
