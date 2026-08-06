# Surface (or repair) truncated value and variable labels in upstream StatCan metadata

**Type:** enhancement / data quality
**Version:** canpumf 0.5.3

## Summary

A non-trivial share of the value and variable labels that `canpumf` exposes are silently
truncated or corrupted. **The corruption is upstream** — it is present verbatim in the
command files Statistics Canada ships with the PUMFs, in every flavour — so this is not a
parser bug. But `canpumf` passes it through without any signal, and the failure is quiet:
the codes are correct, the frequencies are correct, only the human-readable label is
wrong. Anyone building a table or a chart off `levels()` or `pumf_var_labels()` gets a
plausible-looking but wrong label and no reason to suspect it.

Filing this to track whether `canpumf` should detect, warn about, or override these.

## Reproducer

```r
library(canpumf)
library(dplyr)

c16 <- get_pumf("GSS", "Cycle 16 (2002)")
cg4 <- pumf_module(c16, "CG4") |> collect()

levels(cg4$CG4_FR_Q100_C)[c(6, 17, 19)]
#> [1] "Son/Daughter/Grandson/Granddaughter/Son or Daughter in law/N"
#> [2] "of self-employed professionals)"
#> [3] "Co-worker of respondent and Other relatives)"

pumf_var_labels(c16) |> filter(name == "CG4_FR_Q100_C") |> pull(label_en)
#> [1] "Relationship of the Long Term Care Receiver to respondent -"

# a second survey, different corruption
g13 <- get_pumf("GSS", "Cycle 13 (1999)")
levels(collect(select(g13, DWELC))$DWELC)
#> [1] "Single-ded house" "Low-risestories)" "High-risstories)" "Other" "Not stated"
```

## Ground truth vs. delivered

From Appendix G of the Cycle 16 user guide (`12M0016-GPE.pdf`), variable
`CG4_FR_Q100_C`. Frequencies match the data file exactly in every case, so the codes are
sound:

| Code | Freq | User guide | Delivered by canpumf |
|---|---|---|---|
| 19 | 17 | `Son/Daughter/Grandson/Granddaughter/Son or Daughter in law/Nephew or Niece of the respondent` | `Son/Daughter/Grandson/Granddaughter/Son or Daughter in law/N` |
| 83 | 443 | `Non-Governmental Organization (Includes clients and patients of self-employed professionals)` | `of self-employed professionals)` |
| 85 | 457 | `Other (Do not include organizations here) (Includes Ex-spouse/Ex-partner/Same sex partner/ Co-worker of respondent and Other relatives)` | `Co-worker of respondent and Other relatives)` |

Variable label: `Relationship of the Long Term Care Receiver to respondent - collapsed.`
delivered as `Relationship of the Long Term Care Receiver to respondent -`.

Note code 83 is materially misleading rather than merely short — the true category is
*non-governmental organization*, and what survives reads like a category about
professionals' clients.

## Confirmed upstream

`C16MDFSasAndCode-EngFr/C16PUMF_CG4_SAS_CARDS_E.SAS`:

```sas
proc format;
/* $CG100_C format applies to: CG4_FR_Q100_C  */
VALUE $CG100_C
                ...
                "19" = "Son/Daughter/Grandson/Granddaughter/Son or Daughter in law/N"
                ...
                "83" = "of self-employed professionals)"
                "84" = "Paid employee/worker of respondent"
                "85" = "Co-worker of respondent and Other relatives)"
```

and line 1091 of the same file:

```sas
LABEL CG4_FR_Q100_C         = "Relationship of the Long Term Care Receiver to respondent - ";
```

Same for Cycle 13, in `C13MDFSasAndCode-EngFr/c13micme.sas`:

```sas
                1 = "Single-ded house"
                2 = "Low-risestories)"
```

So no change to the parsing logic is called for. `canpumf` is faithfully reproducing
damaged source metadata.

## Not fixable by picking a different command-file flavour

The damage sits upstream of the flavour-specific renderers, so it is not a SAS artifact
and cannot be dodged by parsing the SPSS or Stata file instead. Across the 68 cached
versions that ship more than one flavour, SAS, SPSS and Stata carry byte-identical label
text — GSS Cycle 16, GSS Cycle 31, SGVP 2018 and CHSS 2019-2020 all compare at Jaccard
1.000 on the full label multiset. The Cycle 16 example above appears verbatim in
`C16PUMF_CG4_SPSS_CARDS_E.sps:287`.

A scan asking *"does any other flavour hold a strictly longer label with this 60-char
prefix or suffix?"* returned no genuine recoveries; every hit was a coincidental prefix
collision between two distinct labels.

The shipped binaries are not an escape hatch either. GSS 16's `C16PUMF_FR_CG4.sav` and
`c16pumf_fr_cg4.sas7bdat` carry **zero** variable and value labels (data only), and
`GSS31PUMF_en.dta` has exactly the same truncation profile as its `.do` file. The
heaviest-hit surveys ship one flavour anyway: LFS only `*codebook.csv`, CPSS only PDFs,
SHS 2017/2019 only `.lay`/`.lbe`.

**One exception in the whole cache: PALS 2006**, where the SAS cards are clean and the
SPSS cards carry both corruption patterns:

```
SAS_Cards(E).sas:1913     2 = "I make the majority of decisions about my everyday activities"
SPSS_Cards(E).sps:3252    2   "I make the majority of decisions about my everyday activitie"

SAS_Cards(E).sas:2106     2 = "Business schools, trade or vocational schools "
SPSS_Cards(E).sps:3426    2   "or trade or vocational schools"
```

`merge_metadata()` ranks `spss_mono` above `sas_labels` (`R/metadata_parsers.R:1995`), so
`canpumf` currently serves the corrupt text — see `PALS/2006/metadata/codes.csv:82` and
`:1456`. Two labels, and a clean copy is sitting in the same zip.

## Scope

Measured across my local cache — 134 `codes.csv` and 75 `variables.csv` files, so a
subset of what `canpumf` supports, and the true totals are larger:

| | Count |
|---|---|
| Value labels scanned | 156,172 |
| Value labels exactly 60 chars | 708 |
| Value labels with unbalanced `)` | 62 |
| Variable labels scanned | 30,557 |
| Variable labels exactly 60 chars | **4,051** |
| Files with at least one suspect label | 72 of 134 |

The 60-character figure is the strongest signal. It is not a global cap — 8.3% of
variable labels are longer than 60 characters, up to 388 — but the length distribution
has a ~16× spike at exactly 60 against its neighbours:

```
length  55   56   57   58   59    60   61   62   63   64   65
count  291  259  244  252 1017  4051  177  149  159  135  113
```

Per-family hit counts from the same heuristic: GSS (570), LFS (78), SGVP (49),
PALS (48), Census (9), CPSS (5), SHS (4), CHS (3), CCAHS (2), CHSS (1), SFS (1).

**These are heuristic hits, not confirmed truncations.** Auditing LFS against its source
`LFS_PUMF_EPA_FGMD_codebook.csv` found all 78 to be false positives: they dedupe to 10
distinct labels, every one complete and grammatical (`EFAMTYPE` 07 `"Single-earner
couple, man+ employed, youngest child 18 to 24"`, `NOC_43` 35 `"Technical trades and
transportation officers and controllers"`, …), sitting in blocks whose neighbouring
labels run past 60 freely — up to 223 characters. LFS's own length histogram is flat
through the region:

```
length  55  56  57  58  59  60  61  62  63  64  65
count    3   4   7   2   4   9   5   5   5   4   8
```

i.e. no cap at all. The aggregate 4,051 spike is real; individual hits need
corroboration before being called damage.

## Suggested options

Roughly in increasing order of effort:

1. **Document it.** A note in the vignette or in `?pumf_var_labels` saying labels come
   from StatCan's shipped command files, are known to be truncated in places, and
   should be checked against the user guide data dictionary when they look wrong. This
   alone would have saved me some time.
2. **Detect and warn.** A cheap heuristic catches most of it: label length exactly 60,
   unbalanced brackets, or a label starting with a lowercase word or `and`/`of`/`etc.`.
   Could surface as a one-line message on `get_pumf()`, or better as an exported
   `pumf_suspect_labels(pumf)` that users can call deliberately without noisy warnings.
3. **Flag in the returned table.** An extra logical column on `pumf_var_labels()` output
   — e.g. `label_suspect` — so the information is available programmatically without
   changing existing behaviour.
4. **Override file.** A small curated CSV of corrected labels for the worst offenders,
   applied on read. Highest value for users, but ongoing maintenance and someone has to
   transcribe from the PDFs, so probably only worth it for high-traffic variables.
5. **Fix PALS 2006 from its own SAS cards.** A `codes_supplement` for the two labels
   above. Not a heuristic and not a transcription — the correct text ships in the PUMF.

Option 2 or 3 seems like the right trade — it is a few lines, it changes no existing
output, and it converts a silent wrong answer into a visible one.

Whichever of 2/3 is taken, the LFS audit above says a bare `nchar() == 60` test will fire
on clean surveys. Worth pairing it with a corroborating signal: flag a length-60 label
only when the variable's sibling labels also cluster at 60, or when the bilingual
counterpart is materially longer. Both cleanly separate GSS 16 from LFS.

## Environment

```
canpumf 0.5.3
R 4.6.0 (2026-04-24)
macOS 26.6 (darwin 25.6.0)
```
