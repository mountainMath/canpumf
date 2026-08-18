# Compare the guide's field positions against the command file's layout

The structural corroboration channel, independent of the frequencies:
each guide block prints the variable's \`Position:\` (and usually
\`Length:\`), and the command file's layout gives the same two numbers.
Agreement across the whole document is what tells "the right guide whose
counts were tabulated on a different population" apart from "the wrong
guide".

## Usage

``` r
.pumf_pdf_position_agreement(pdf, layout)
```

## Arguments

- pdf:

  Parser output from \[parse_pdf_freq_codebook()\].

- layout:

  Canonical layout tibble (name/start/end), or \`NULL\`.

## Value

List with \`n\` (blocks that could be compared), \`agree\`, \`rate\`
(\`NA\` when nothing was comparable) and \`ok\`, a logical vector
aligned to \`pdf\$variables\`.
