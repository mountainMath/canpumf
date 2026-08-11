# Inspect the PDF-versus-microdata frequency validation

Companion to \[pumf_label_repairs()\]. Reports, per variable, whether
the frequencies printed in the survey's PDF data dictionary reconcile
against a tabulation of the actual data file. This is the evidence
\`canpumf\` uses to decide whether a label from the guide may be
trusted.

## Usage

``` r
pumf_freq_validation(tbl)
```

## Arguments

- tbl:

  A lazy \`dplyr::tbl()\` returned by \[get_pumf()\].

## Value

A tibble with columns \`block\` (the guide block the check ran on),
\`name\`, \`status\`, \`n_codes\`, \`n_matched\` and \`note\`; zero rows
when the survey ships no parseable PDF dictionary.

## Details

\`status\` is one of:

- \`validated\`:

  Every documented code's count matches the data exactly, and the data
  holds no undocumented values.

- \`continuous\`:

  The documented sentinel codes match and the remaining values are
  accounted for by the guide's \`lo : hi\` range row – a continuous
  variable, correctly parsed.

- \`mismatch\`:

  Counts disagree; nothing from the guide is used for this variable.

- \`unchecked\`:

  The check could not be run: no data file, no overlapping codes, or –
  common for a multi-module survey – the variable belongs to a sibling
  module and is absent from this one's data file. Also used when the
  guide's field positions reproduce the command file's layout but none
  of its counts reproduce the data (note \`"guide frequencies use a
  different population"\`): the guide is the right document, its tables
  were simply tabulated on another base, so the counts are treated as
  absent rather than as evidence against the parse.

A guide corroborated by \*neither\* channel – neither its counts nor its
field positions – is rejected as the wrong document. Its validation
table is still returned (it is the evidence for that decision), but the
repair ledger is empty and no label from it is used.

## See also

\[pumf_label_repairs()\]

## Examples

``` r
# \donttest{
gss <- get_pumf("GSS", "Cycle 16 (2002)")
if (!is.null(gss)) {
  table(pumf_freq_validation(gss)$status)
  close_pumf(gss)
}
# }
```
