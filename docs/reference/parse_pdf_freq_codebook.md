# Parse a StatCan PDF frequency data dictionary

Extracts variable names, variable labels, value labels and *per-code
frequencies* from the data-dictionary appendix of a StatCan PUMF user
guide (GSS cycles, SGVP, PALS, SFS). Each variable appears as a
“Variable Name:/Position:” block followed by a frequency table whose
rows are `<code> <label> <FREQ> <WTD>`.

## Usage

``` r
parse_pdf_freq_codebook(eng_pdf, fra_pdf = NULL)
```

## Arguments

- eng_pdf:

  Path to the English user guide / codebook PDF.

- fra_pdf:

  Optional path to the French counterpart.

## Value

Named list with elements `variables` (plus `block`, `position`,
`length`), `codes`, `layout` (always `NULL`), `freqs`
(name/val/freq/block) and `ranges` (name/lo/hi/freq/block, for
continuous variables), or `NULL` if no variable blocks were found.

## Details

Unlike the other metadata parsers this one also returns the frequencies,
which
[`pumf_parse_metadata`](https://mountainmath.github.io/canpumf/reference/pumf_parse_metadata.md)
uses to validate the parse against the actual microdata before allowing
any label to be repaired from it.

Every returned table carries a `block` id – the index of the guide block
it came from. A guide covering several linked modules documents a shared
key once per module, so `name` alone is not unique; the block id plus
the `position`/`length` columns on `variables` are what
[`.pumf_pdf_select_blocks`](https://mountainmath.github.io/canpumf/reference/dot-pumf_pdf_select_blocks.md)
uses to pick the block belonging to the module being parsed. Positions
are used only for that disambiguation – layout always comes from the
command file.
