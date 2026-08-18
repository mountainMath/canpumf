# Validate a PDF frequency parse against the microdata

For every variable the PDF dictionary describes, compares the per-code
frequencies printed in the guide against a tabulation of the actual data
file. This is what turns the PDF from an unverifiable transcription into
a checkable source: a variable whose counts do not reconcile is never
used to repair a label.

## Usage

``` r
.pumf_validate_pdf_freqs(pdf, layout, data_path, data_encoding = "CP1252")
```

## Arguments

- pdf:

  Parser output from \[parse_pdf_freq_codebook()\].

- layout:

  Canonical layout tibble (name/start/end), or \`NULL\` for CSV data.

- data_path:

  Path to the microdata file, or \`NULL\` when none was found.

- data_encoding:

  Encoding of the data file.

## Value

Tibble with columns \`block\`, \`name\`, \`status\`, \`n_codes\`,
\`n_matched\`, \`note\` – one row per guide block, so a name documented
once per module of a multi-module guide gets one row per module.
