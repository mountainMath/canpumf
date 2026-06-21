# Parse a StatCan bilingual PDF frequency codebook

Extracts variable names/labels and code-value labels from the bilingual
PDF *codebook* shipped with surveys such as CPSS 1, which (unlike CPSS
2-6) ships no machine-readable `variables.csv`. The codebook lists each
variable as a “Variable Name:/Concept:” block followed by an “Answer
Categories” frequency table whose `Code` column supplies the value
labels. Positions in the codebook are not used; this parser produces
only `variables` and `codes` (no `layout`).

## Usage

``` r
parse_pdf_codebook(eng_pdf, fra_pdf = NULL)
```

## Arguments

- eng_pdf:

  Path to the English codebook PDF.

- fra_pdf:

  Optional path to the French codebook PDF.

## Value

Named list with elements `variables`, `codes`, `layout` (always `NULL`),
or `NULL` if no variable blocks were found.
