# Filter guide blocks by the layout's field position

A user guide covering several linked files documents each of them in
turn, so a shared respondent key (\`RECID\`, \`PERSONID\`) gets one
block per module – each with that module's own frequencies. Keyed by
name alone the first block wins, which then contradicts every other
module's data file.

## Usage

``` r
.pumf_pdf_select_blocks(pdf, layout)
```

## Arguments

- pdf:

  Parser output from \[parse_pdf_freq_codebook()\].

- layout:

  Canonical layout tibble (name/start/end), or \`NULL\`.

## Value

\`pdf\`, with \`variables\`/\`codes\`/\`freqs\`/\`ranges\` filtered to
the surviving blocks.

## Details

This is the cheap first pass: the block header prints \`Position:\` and
\`Length:\`, and the command file's layout gives the same two numbers
for this module, so a name documented at different offsets in different
modules is resolved here. It cannot separate blocks that share an offset
(every module's file starts \`RECID\` at position 1) – those go to
\[.pumf_pdf_resolve_duplicate_blocks()\], which decides on the
frequencies.
