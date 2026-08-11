# Pick the guide that matches this module's layout

Detection shortlists candidates on filename and block count, which
cannot separate a release that ships both its original and its revised
user guide: PALS 2006's two English guides document the same 746
variables, but the original's fields all sit eight columns to the left
of the revised file's. Scoring each candidate's
\`Position:\`/\`Length:\` headers against the command file's layout
picks the right one – and does so per language, so the English and
French guides of the same edition are chosen together.

## Usage

``` r
.pumf_pdf_choose_candidates(pdf_paths, layout)
```

## Arguments

- pdf_paths:

  The \`pdf_freq\` element of \[detect_formats()\].

- layout:

  Canonical layout tibble (name/start/end), or \`NULL\`.

## Value

\`pdf_paths\` with \`eng\`/\`fra\` possibly replaced.
