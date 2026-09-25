# Resolve remaining duplicate blocks by which one reconciles with the data

After \[.pumf_pdf_select_blocks()\] a name can still map to several
blocks – typically a respondent key that sits at the same offset in
every module of a multi-module survey. The frequencies settle it: only
the block describing the file actually being parsed reconciles against
it. Where none does, the first block is kept and its (mismatching)
status stands, so nothing is repaired from a block we could not confirm.

## Usage

``` r
.pumf_pdf_resolve_duplicate_blocks(pdf, validation)
```

## Arguments

- pdf:

  Parser output, already position-filtered.

- validation:

  Block-keyed output of \[.pumf_validate_pdf_freqs()\].

## Value

List with the filtered \`pdf\` and \`validation\` (one row per name).
