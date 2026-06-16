# Parse a StatCan PDF Data Dictionary for variable and code labels

Extracts variable long-names and code-value labels from the bilingual
PDF data dictionaries shipped with some older StatCan PUMF releases
(e.g. SFS 1999). Positions in the PDF do not match the PUMF flat file;
this parser produces only `variables` and `codes` (no `layout`).

## Usage

``` r
parse_pdf_dictionary(eng_pdf, fra_pdf = NULL)
```

## Arguments

- eng_pdf:

  Path to the English *Dictionary.pdf*.

- fra_pdf:

  Optional path to the French *Dictionnaire.pdf*.

## Value

Named list with elements `variables`, `codes`, `layout` (always `NULL`).
