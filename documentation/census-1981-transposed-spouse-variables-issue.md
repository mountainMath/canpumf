# Census 1981: get an authoritative ruling from StatCan on the spouse-variable naming collision

**Type:** data quality / needs upstream confirmation
**Version:** canpumf 0.5.3
**Affects:** `get_pumf("Census", "1981 (individuals)")`

## Summary

For the 1981 Census PUMF (individuals) the metadata shipped by StatCan is self-contradictory. The SPSS command files disagree with the PDF documentation on at least three variables.

`canpumf` applies a `cols_swap` override to these three pairs of columns, and warns loudly on every build. The override exists because **Statistics Canada's SPSS command file is internally inconsistent**: its `DATA LIST` and its `VARIABLE LABELS` / `VALUE LABELS` read the `FA*` / `MA*` mnemonics under *opposite* conventions.

* The record layout PDF reads them as **father / mother**: `FALFACT` is the husband's.
* The SPSS labels read them as **male / female**: `MALFACT` is the husband's.

The `DATA LIST` follows the PDF's convention; the label sections follow the other. Same
identifiers, two meanings, one file.

**Which spouse each column holds is not in doubt** — the PDF, the value-label code
schemes, and the data itself all agree. What is unresolved is **what the variables should
be called**, and that is a question only Statistics Canada can answer authoritatively.

## What canpumf does today

`R/registry.R`:

```r
"Census/1981/individuals" = .make_entry("Census", "1981/individuals",
  bundle_sps_mask = "ind81",
  file_mask       = "^INDMDF81\\.DAT$",
  data_fixups     = list(
    cols_swap = c(WKACTMA = "WKACTFA", FAOCC81 = "MAOCC81", FALFACT = "MALFACT")
  )),
```

We swap the column names, then apply the SPSS labels. **Functionally this is identical to
following the PDF** — reading the PDF's positions and attaching the PDF's prose
descriptions. Every column ends up carrying the right spouse's data either way. The only
difference is the delivered *name*: we emit `MALFACT` for position 156, where the PDF
calls it `FALFACT`.

Each swap emits a warning at build time:

```
Columns WKACTMA and WKACTFA: names swapped relative to command file —
DATA LIST variable names appear transposed.
```

## The collision, in detail

### Statistics Canada's record layout PDF (`pumf1981rcl_ev2.pdf`, p. 64)

```
68  1  156      N  FALFACT   Labour Force Activity of H/MLP*
69  1  157      N  MALFACT   Labour Force Activity of W/FLP**
70  2  158-159  N  FAOCC81   Occupation, 1981 of H/MLP*
71  2  160-161  N  MAOCC81   Occupation, 1981 of W/FLP**
72  2  162-163  N  WKACTFA   Work Activity in 1980 for H/MLP*
73  2  164-165  N  WKACTMA   Work Activity in 1980 for W/FLP**
```

and, spelled out in prose in the variable detail pages (pp. 146, 154):

```
FALFACT  -  Labour Force Activity of Husband or Male Lone Parent
WKACTFA  -  Work Activity in 1980 for Husband or Male Lone Parent
```

The PDF is self-consistent throughout: `FA*` = father = husband/male lone parent,
`MA*` = mother = wife/female lone parent.

### `ind81_eng.sps`, `DATA LIST` (lines 69–74)

```
FALFACT   156 - 156
MALFACT   157 - 157
FAOCC81   158 - 159
MAOCC81   160 - 161
WKACTFA   162 - 163
WKACTMA   164 - 165
```

**Identical to the PDF.** So far, no disagreement at all.

### `ind81_eng.sps`, `VARIABLE LABELS` (lines 172–177)

```
MALFACT  'HUSBAND/MALE LONE PARENT: LABOUR FORCE ACTIVITY'
FALFACT  'WIFE/FEMALE LONE PARENT: LABOUR FORCE ACTIVITY'
MAOCC81  'HUSBAND/MALE LONE PARENT: OCCUPATION - 1981 BASIS'
FAOCC81  'WIFE/FEMALE LONE PARENT: OCCUPATION - 1981 BASIS'
WKACTMA  'HUSBAND/MALE LONE PARENT: WORK ACTIVITY - 1980'
WKACTFA  'WIFE/FEMALE LONE PARENT: WORK ACTIVITY - 1980'
```

**This is where it breaks.** The `DATA LIST` put `FALFACT` at 156, which the PDF documents
as the *husband's* — but this section labels `FALFACT` as the *wife's*. The `VALUE LABELS`
follow the same male/female reading: the label set attached to `WKACTMA` is the 10-code
scheme, and the PDF documents the 10-code scheme at 162–163, which the `DATA LIST` names
`WKACTFA`.

Used as shipped, without intervention, the file therefore attributes each spouse's labour
force activity, occupation and work activity to the other spouse.

The French `ind81_fre.sps` carries the identical `DATA LIST` and the same label
assignment, so this is systematic to the release, not a slip in one file.

## Which column holds which spouse — not in dispute

Three independent lines of evidence, all agreeing with the PDF:

### 1. Value-label code schemes

The paired variables use *different* code schemes. From `ind81_eng.sps`, `WKACTMA` (the
husband's, per its label) has 10 codes `0`–`10`; `WKACTFA` has 12 codes `0`–`12`.
`MISSING VALUES` declares `MAOCC81 (0,17)` and `FAOCC81 (0,16)`. Reading
`INDMDF81.DAT` (486,876 records) at the raw positions:

| Positions | Values found | Scheme belongs to | PDF says |
|---|---|---|---|
| 162–163 | 0–10 | husband's work activity | husband ✓ |
| 164–165 | 0–12 | wife's work activity | wife ✓ |
| 158–159 | max 17 | husband's occupation | husband ✓ |
| 160–161 | max 16 | wife's occupation | wife ✓ |

The PDF's own code list for `WKACTFA` (162–163) runs `00`–`10`, confirming the match.

### 2. Not-applicable agreement (settles the labour-force pair)

`MALFACT` / `FALFACT` share one code scheme (`0`–`4`), so the code lists cannot
discriminate. A person with no husband has no husband's work activity, no husband's
occupation and no husband's labour-force activity, so the zeros must coincide:

| | vs husband's work activity (162–163) | vs wife's work activity (164–165) |
|---|---|---|
| column 156 | **100.00%** | 92.05% |
| column 157 | 92.05% | **100.00%** |

Column 156 is the husband's — as the PDF says. Agreement against the occupation columns
gives the same answer (99.66% / 99.91%).

### 3. Third-party corroboration (Borealis deposit)

The same PUMF is deposited on Borealis (`doi:10.5683/SP3/XHTFC8`). Its `pumf81i.sps`
`DATA LIST` reads:

```
    MALFACT    156 -  156        MAOCC81    158 -  159        WKACTMA    162 -  163
    FALFACT    157 -  157        FAOCC81    160 -  161        WKACTFA    164 -  165
```

— i.e. **they made the same choice we did**: rename the columns so the SPSS label
sections become correct, rather than rename the labels to match the PDF. Their
`pumf81i_codebook.txt` documents the variables at those positions with sample frequencies
that match `INDMDF81.DAT` exactly (`MALFACT` code 0 = 106,989 at col 156; `FALFACT` =
81,563 at col 157; `MAOCC81` = 105,313; `FAOCC81` = 81,109).

**This is corroboration of an interpretation, not confirmation of a fact.** The Borealis
files are not a Statistics Canada reissue. The codebook header reads:

```
  Canada. Statistics Canada./
     Census of Canada, 1981 : public use sample tapes user documentation :
     individual file (2 % sample) codebook. Ottawa, Ont. Statistics Canada, September 1982

  Machine-readable DDMS edition codebook compiled by: Canada. Health and Welfare Canada
  Version 1.1, Edited by: Data Library Service, University of Toronto
```

and `pust81i.sas` is titled `Health & Welfare Canada ed. SPSS control commands`. So an
independent data-library team hit the same defect and resolved it the same way — useful,
but it carries no more authority than our own reading.

Notably, **the Borealis deposit ships the identical StatCan record layout PDF** — the file
is byte-for-byte the same (`md5 202e617a4fd0c1172027b422ed8cf303`) as the one in the
StatCan bundle. So their modified `.sps` contradicts the PDF sitting next to it in their
own deposit, unremarked.

## What we need from Statistics Canada

1. **Confirm the defect.** `ind81_eng.sps` / `ind81_fre.sps` attach `VARIABLE LABELS` and
   `VALUE LABELS` under a male/female reading of `FA*`/`MA*` while their `DATA LIST` uses
   the record layout's father/mother reading. Is this acknowledged?
2. **Rule on the naming — the actual open question.** For position 156, is the intended
   variable name `FALFACT` (record layout PDF, `FA*` = father) or `MALFACT` (SPSS labels,
   `MA*` = male)? Both documents are Statistics Canada's own and they disagree. We
   currently emit `MALFACT`, which means **anyone coding from the official record layout
   and asking for `FALFACT` expecting the husband gets the wife from `canpumf`.** If the
   PDF is authoritative we should invert the override — keep the PDF's names and move the
   labels instead. Same data either way; only the names change.
3. **Reissue a self-consistent command file** under whichever convention is ruled correct.
4. **Confirm scope.** We have only examined the individuals file; the households and
   families files ship from the same bundle.

## Notes for canpumf

* Only the 1981 individuals file is affected. The 1986 individuals PUMF carries no
  husband/wife spouse-characteristic variables at all, and no other cached census year
  uses `cols_swap`.
* Until StatCan rules, the naming divergence from the official record layout should be
  called out in the documentation — it silently returns the other spouse to anyone coding
  from the PDF.
* The build-time warning is worth keeping, but its wording ("names swapped relative to
  command file") describes the mechanism rather than the reason, and could point here.

## Environment

```
canpumf 0.5.3
R 4.6.0 (2026-04-24)
macOS 26.6 (darwin 25.6.0)
```
