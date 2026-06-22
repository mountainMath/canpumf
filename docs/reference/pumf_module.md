# Open a sibling module of a multi-module survey

Some surveys ship several linked fixed-width files that share a
respondent key (e.g. GSS cycle 16, "Aging and Social Support", 2002,
whose MAIN, CG4, CG6 and CR files all join on \`RECID\`, with the person
weight \`WGHT_PER\` living only in MAIN). \`get_pumf()\` returns the
survey's primary module; \`pumf_module()\` returns one of its sibling
modules \*\*on the same DuckDB connection\*\*, so the two tbls are
joinable on the shared key without opening a second connection.

## Usage

``` r
pumf_module(tbl, module)
```

## Arguments

- tbl:

  A lazy tbl returned by \[get_pumf()\] for a multi-module survey.

- module:

  Name of the module to open (e.g. \`"CG4"\`). See the survey's registry
  entry for available module ids.

## Value

A lazy \`dplyr::tbl()\` for the requested module, backed by the same
connection as \`tbl\`.

## Examples

``` r
if (FALSE) { # \dontrun{
main <- get_pumf("GSS", "2002")          # primary module (MAIN), has WGHT_PER
cg4  <- pumf_module(main, "CG4")         # caregiving module, same connection
dplyr::left_join(main, cg4, by = "RECID")
} # }
```
