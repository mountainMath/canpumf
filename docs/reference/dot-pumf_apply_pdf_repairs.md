# Repair command-file labels from a validated PDF dictionary

Compares every variable and value label in the merged command-file
metadata against the PDF dictionary, fills in labels the command file
left blank, replaces labels the PDF demonstrably extends (the
upstream-truncation fingerprint), and records every divergence – acted
on or not – in a ledger.

## Usage

``` r
.pumf_apply_pdf_repairs(metadata, pdf, validation)
```

## Arguments

- metadata:

  Merged canonical metadata (\`variables\`/\`codes\`/\`layout\`).

- pdf:

  Parser output from \[parse_pdf_freq_codebook()\].

- validation:

  Output of \[.pumf_validate_pdf_freqs()\].

## Value

List with elements \`metadata\` (possibly modified) and \`repairs\` (the
ledger tibble).

## Details

A repair is withheld only where the frequency check actively contradicts
the PDF parse (status \`"mismatch"\`); a variable the check could not
reach is not evidence against it. Each ledger row carries the variable's
validation status so corroborated repairs are distinguishable from
merely uncontradicted ones.
