# User-guide PDF parser, cross-check and label repair

Back to [CLAUDE.md](../CLAUDE.md). Code: `parse_pdf_freq_codebook()` in `R/metadata_parsers.R`; everything else in `R/pdf_repair.R`.

StatCan's command files routinely ship **truncated** value and variable labels: hard cuts at 60 characters, dropped leading text, dropped interior text. The damage happens upstream of the SAS/SPSS/Stata renderers, which carry byte-identical text, so parsing a different flavour does not avoid it. The survey's user guide has the full text, typeset from the metadata before the command files were generated.

Normally, trusting a PDF scrape over a machine-readable command file would be a bad trade. It is a good trade here because this PDF layout prints the **frequency of every code**. The parse can therefore be reconciled against the actual data file before any of it is believed.

## The parser: `parse_pdf_freq_codebook()` / `.parse_pdf_freq_single()`

This is the *third* StatCan PDF layout: the data-dictionary appendix of a PUMF user guide (GSS cycles, SGVP, PALS, SFS, Time Use). It needs `pdftools`. It returns `variables` and `codes`, plus `freqs` (name/val/freq) and `ranges` (name/lo/hi/freq), which drive the cross-check.

- **Block header**: `Variable Name: X  Position: N  Length: L`. `Position` is **required** in `.pdf_freq_var_rx`. That requirement is what separates this layout from the CPSS codebook.
- **Variable label**: the free text between the header and whatever closes it. That is either the table's `FREQ` header or, for a variable printed without a table, the block's trailing rule or its `Coverage:`/`Source:`/`Format:` lines. The second rule stops the *last* block from running to the end of the document. GSS Cycle 24 `WTSBS_001` has no table, and without the rule it produced a 78,014-character "label" that the repair pass then wrote over a sound label.
- **Table rows**: `<code> <label> <FREQ> <WTD>` (French: `FREQ`/`POND`). The table ends at a `======` rule or at a `Coverage:`/`Source:`/`Format:`/`Weight variable:` line.
- **Column split**: numbers are right-aligned to the **end of the `FREQ` header word**. `pdftools` drifts a few characters from row to row, so the parser picks the number token whose **end column is nearest the anchor**, within `.pdf_freq_slack` (8). The alternatives fail:
  - A whitespace-run split takes the *weighted* count whenever a label overruns into the number column (`"...support/wheelchair 1,980"`).
  - A fixed cut plus a rightward digit walk stops at a thousands comma (GSS 26 `WLY_Q150` `9,520` → `9`). It also never reaches a value that starts past the anchor (`MAR_Q110` `97 Not Asked  0` → `NA`).

  Nearest-end works because the weighted count sits far right (+17, against +2…+5 for the frequency) and any digits in the label sit far left.
- **Stranded counts**: when **no** number lands near the anchor, the label has pushed the weighted count onto the next line (GSS 24 Episode `SACT1` code 15). The parser uses the stranded number only when it is unambiguous: exactly one candidate, starting at or after the anchor and **preceded by whitespace**. That rule excludes flush-printed `"…records3,4417,790,477"`. Even when the count cannot be recovered, the digits are removed from the label. A continuation line that is **only a number** is the orphaned weighted count and is not appended to the label.
- **Optional code-row label**: a scale such as `LSR_Q110` (0–10) labels only its endpoints and leaves codes 01–09 bare. Those rows are kept.
- **Range rows**: continuous variables print a single `lo : hi   <freq>` row instead of per-value rows.
- **`Format:`** (I2, F5.3, 4.1, A8, $CHAR2.) drives `.pdf_freq_type()` and `.pdf_freq_decimals()`.
- **Multi-module guides**: a single guide documents each linked module in turn, so a shared key (`RECID`, `PERSONID`) gets one block per module. Every table therefore carries a `block` id, and `variables` also carries `position`/`length`. `.pumf_pdf_select_blocks(pdf, layout)` resolves duplicate names by matching position and length against the module's `layout.csv`, falling back to the first block. For the same reason, the eng/fra merge joins on `block` rather than on `name` when both guides have identical name sequences.
- **Not merged**: the parser's output is kept out of `merge_metadata()`, so the command file stays authoritative. It becomes the primary source only when no other parser fired. In that case the `block`/`position`/`length` bookkeeping columns are stripped first.

## Detection

`.pumf_detect_freq_pdfs()` (section 10 of `detect_formats()`) runs only when `getOption("canpumf.pdf_crosscheck", TRUE)` is set and `pdftools` is installed. It works as follows:

- Shortlists PDFs by path pattern.
- Prefers paths matching `layout_mask` for multi-module surveys.
- Keeps at most 8 files, each larger than 50 KB, sorted by size.
- Checks each file's content: it needs ≥10 block headers **and** a FREQ column.
- Tells eng from fra by whether `Nom de la variable` outnumbers `Variable Name`.

## The cross-check: `.pumf_pdf_crosscheck()` (end of `pumf_parse_metadata()`)

0. **Pick the guide**: `.pumf_pdf_choose_candidates(pdf_paths, layout)`. Some releases ship both the original and a revised guide (PALS 2006). Their block counts are nearly identical, but their field positions differ. Each candidate's `Position:`/`Length:` headers are scored against `layout.csv`, and the best one per language wins, provided it clears `.pumf_pdf_min_pos_rate` (0.9). PALS 2006: the original guide scores 3/746, the Dec 2011 revision 746/746.
1. **Parse**: `parse_pdf_freq_codebook()` → `.pumf_pdf_select_blocks()`.
2. **Validate**: `.pumf_validate_pdf_freqs(pdf, layout, data_path)` tabulates each documented variable in the microdata and compares the counts. The results are written to `metadata/pdf_validation.csv`. Statuses:
   - `validated`: every count matches, and the data holds no undocumented values.
   - `continuous`: the sentinel counts match, and the remaining values fall within the `lo : hi` range row.
   - `mismatch`: the counts disagree.
   - `unchecked`: the data could not be compared. Causes: no data file, variable not in this module, empty column, file > 500 MB, or **any printed count unreadable**. This is missing evidence, not contrary evidence (and an `NA` count used to abort Stage 2).
3. **Document-level verdict.** There are two independent checks that this guide describes this file:
   - Frequencies: do the printed counts reproduce the data (`.pumf_pdf_min_freq_rate`, 0.5)?
   - Positions: does `.pumf_pdf_position_agreement()` reproduce the layout (`.pumf_pdf_min_pos_rate`, 0.9)?

   Outcomes:
   - **Both fail** → wrong document. The validation table is still written as evidence. The repair ledger is written empty, and the function returns.
   - **Frequencies fail but positions pass** → right document, different tabulation base. PALS 2006 tabulates the disability sub-population: guide `SEX` 9422 + 8001 = 17423, the `DISAB=1` count, against 72167 rows in the file. For blocks the layout confirms, `mismatch` is downgraded to `unchecked` ("guide frequencies use a different population"). Blocks the layout does not confirm stay `mismatch`.
4. **Repair**: `.pumf_apply_pdf_repairs()` compares every variable and value label, and writes `metadata/label_repairs.csv`. `.pumf_repair_action()` returns one of:
   - `fill`: the command file has no label.
   - `repair`: the guide's text **demonstrably extends** the command file's. It is either a strict superstring, or a ≥8-char subsequence sharing a ≥4-char prefix or suffix. The subsequence case catches interior drops, e.g. `"Single-ded house"` ⊂ `"Single-detached house"`.
   - `flag`: the texts differ, but the guide's does not extend the command file's.
   - `ok`: nothing to do.

   A repair is withheld **only** on `mismatch`. Every ledger row carries the variable's `validation` status, so uncorroborated repairs stay visible.

## Truncation fingerprints (gate every `repair`)

The shape of the strings alone cannot separate the two reasons a guide's text may be longer than the command file's:
- **Real truncation.** GSS Cycle 16 cuts at 60 characters: 1,665 of its 1,860 variable labels are 59–60 characters long, against 42 across the six lengths below.
- **Different fields.** SGVP 2007 and PALS 2006 print the **question wording** where the command file has a hand-written short label. The short label is often a subsequence of the question, so it would be "repaired" into a question.

**Fixed ceiling**: `.pumf_truncation_width()` / `.pumf_at_truncation()`. A hard cut leaves a spike at the ceiling, while hand-abbreviated labels thin out towards the longest one. The test compares the counts at the top two lengths against the six lengths below them:

| Survey | Ratio |
|---|---|
| GSS 16 | 40–62× (variables), 1.4–6.0× (codes) |
| GSS 26 | 10.1× / 3.5× |
| PALS | 2.9× (codes) |
| SGVP, CHS, CIS, CPSS, SFS | 0.45×, 0.39×, 0.41×, 0.33×, 0.04× |

The width is computed from the command file itself, separately for variables and codes, pooling both languages. It needs ≥20 non-empty labels, and is `NA` when there is no ceiling.

**Dropped leading text**: `.pumf_left_truncated()`. This damage leaves a short label rather than one at the ceiling. The signature is that the command-file text is a strict suffix (≥8 chars) of the guide's, e.g. `"relative in a family farm or business?"`, `"foot or bus)"`, `"les réserves indiennes)"`.

**Annotation veto**: `.pumf_annotation_prefix()` judges the **dropped text**. A colon marks a `Key: value` annotation rather than lost prose. This rejects two things that also produce the suffix shape:
- editorial notes (SGVP: `"Grouped variable: Age group"` vs `"Age group"`)
- scraped field furniture (GSS 16: `"Longueur : 2 Age du répondant …"`)

Judging the dropped text is what lets a mid-list truncation starting with a capital through: `"Co-worker of respondent and Other relatives)"` of `"Other (Do not include organizations here) (Includes …"`. That value label motivated the whole cross-check. The veto **outranks both fingerprints**, because the furniture case also sits at the ceiling.

Net effect: SGVP went from 35 repairs to 1 (a real dropped prefix, `"3 or 4 times a year"`). PALS went from 245 to 100. GSS Cycle 16 kept 2,860 of 2,865.

## Undeclared codes

Codes the guide documents but the command file never declared are **reported, not injected**. Adding them is a registry `codes_supplement` decision (see [registry.md](registry.md)). Two kinds are not reported at all:
- Sentinels already covered by the variable's missing range.
- Codes on variables the command file treats as continuous (not in `codes.csv`). There, the guide's zero-value rows ("No hours", "None") label a valid zero. This removed 120 spurious flags on SGVP alone.

## User-facing

- `pumf_label_repairs(tbl, action = NULL)` and `pumf_freq_validation(tbl)` read the two side-car CSVs. They are module-aware via `.pumf_meta_dir_from_tbl()`, and return zero rows for LFS and for surveys without such a guide.
- `options(canpumf.pdf_crosscheck = FALSE)` disables the whole step.
