# Metadata parsers (`R/metadata_parsers.R`)

Back to [CLAUDE.md](../CLAUDE.md). The PDF user-guide parser (#9) and everything downstream of it are covered in [pdf-crosscheck.md](pdf-crosscheck.md).

Nine parsers converge on three canonical CSV files in `<version_dir>/metadata/`:
- `variables.csv`: one row per variable (name, label_en, label_fr, type, decimals, missing_low, missing_high)
- `codes.csv`: one row per code value (name, val, label_en, label_fr)
- `layout.csv`: one row per fixed-width column (name, start, end). Absent for CSV-format data.

Several parsers can fire for the same survey (e.g. split-SPSS for layout/codes and SAS cards for BSW weights). `merge_metadata()` consolidates the results. `pumf_parse_metadata()` then runs `.fix_metadata_mojibake()` and finally the PDF cross-check.

## Parsers, in detection priority order (highest first)

1. `parse_lfs_codebook()`: LFS `*codebook.csv`. Always read as CP1252.
2. `parse_cpss_csv()`: CPSS `variables.csv`.
3. `parse_sas_cards()`: a directory with `.lay` + `.lbe` files.
4. `parse_spss_split()`: a directory with `vare`/`vale`/`_i` named `.sps` files.
5. `parse_spss_mono()`: a single `.sps`, `*SPSS.txt` or `.xmf` file whose content contains `VALUE LABELS` or `DATA LIST`. `VARIABLE LABELS` is optional (e.g. Census 2011 individuals). DATA LIST-only files (e.g. SFS 1999) give layout and type info but no labels. Parser 7 fills in the labels where a PDF dictionary exists.
6. `parse_spss_sav()`: a binary SPSS `.sav` file, read via haven.
7. `parse_pdf_dictionary()`: the StatCan bilingual PDF **Data Dictionary** (`*Dictionary.pdf`). Produces `variables` and `codes` only. PDF positions differ from the flat file, so it produces no `layout`. It is a label-only fallback (SFS 1999).
8. `parse_pdf_codebook()`: the StatCan bilingual PDF **frequency codebook** (CPSS 1, the only CPSS cycle with no `variables.csv`). Produces `variables` and `codes` only. Detection is a last resort: it is consulted only when no command file or codebook CSV was found.
9. `parse_pdf_freq_codebook()`: the **user-guide data dictionary**. It is excluded from `merge_metadata()` unless nothing else parsed. See [pdf-crosscheck.md](pdf-crosscheck.md).

Parsers 7–9 need `pdftools`, which is in Suggests, and are skipped when it is not installed.

## Sentinels and missing values

- **Sentinel detection** uses two anchored patterns built from shared alternatives. `.missing_pat` matches **true-missing** labels (Not applicable, Not stated, Not asked, Valid skip, Refusal, Don't know, … and the French equivalents). `.sentinel_pat` also matches **zero-value** labels ("ZERO HOURS", "None", "Aucun don"): these mark a continuous variable, but the value is a valid zero, not missing data. A variable whose value labels are ALL sentinels is classified `numeric`. Its `missing_low`/`missing_high` range comes from the `.missing_pat` codes **only**. For example, GSS 2012 `ITL_Q10` has 0 = "None" and 97–99 missing, so the range must be [97, 99], not [0, 99].
- **MISSING VALUES**: `.spss_parse_missing()` tolerates padding inside the parens (`VALUEH  ( 999999 )/`) and negative values. A single value gives `missing_low == missing_high`, and `lo THRU hi` gives a range. Every `NAMES (values)` group on a (joined) MISSING VALUES statement is parsed, so ODESI files declaring several variables per line (`CMACODE (0)/ FAMSIZE (99)/ …`) and the shared form `A B (99)` are all captured. A comma list of contiguous integers (in any order: `998,999`, `8, 7`) becomes a range; any other set of discrete values records only its first value (conservative). Use the registry's `missing_codes` fixup when that matters.

## SPSS details

- **String continuations**: `.spss_read_preprocess()` joins `'text' + 'more'` into a single literal. StatCan uses this in **all four** combinations of quote character and line break. Census 2021 alone ships single-quoted continuations (individuals EN), double-quoted continuations (individuals FR, 70 labels), inline double-quoted ones (hierarchical FR) and mixed delimiters (`"…d'eq" + 'uivalence'`). The algorithm:
  - A trailing `+` first pulls the next line up, walking backwards so indices not yet visited stay valid.
  - The four quote-pair patterns are then collapsed in place until nothing changes.
  - Each content class must match the literal's *own* delimiter, because a double-quoted label often contains apostrophes.
  - The opening quote must follow whitespace or start the line.

  A dropped tail leaves a label cut mid-word (`SSGRAD` as `"Scolarité : … attestation d'éq"`). That looks exactly like the upstream truncation the PDF cross-check repairs.
- **Line endings**: command files are read with `.read_cmd_lines()`, which treats any run of CRs before an LF (and a lone CR) as one break. readr splits the `\r\r\n` of the Borealis/ODESI 1976 Census household `.sps` into a blank line plus a line starting with `\n`, which breaks every section.
- **Doubled apostrophes**: `'Person 1''s son'` (Census 1986) is parked before the quote swap in `.spss_read_preprocess()` so the label is not split there. `.fix_label_escapes()` (run with `.fix_mojibake()` in `.fix_metadata_mojibake()`) also turns a `''` left inside a double-quoted label, and HTML entities such as `Yukon &amp; NWT` (1986 EFT), into plain text.
- **Inline VALUE LABELS headers**: only text before the first quote can hold variable names, so `HHTYPE 1 "1 FMLY 2 PARENTS NO OTHERS"` (Census 1976) does not create variables `FMLY`/`PARENTS`/`NO`.
- **Zero-padded codes**: unquoted numeric codes like `01` are normalised via `as.numeric()` → `.code_chr()` so they match bare integers in CSV data. `.code_chr()` formats each element separately and never uses scientific notation (`as.character(200000)` gives `"2e+05"`, which breaks joins).
- **Multi-variable VALUE LABELS blocks**: `/VAR1 VAR2 VAR3` headers are fully parsed, including headers that span continuation lines.
- **DATA LIST column ranges**: spaces around the dash are tolerated (`129-135`, `129 - 135`, `129-  135`). A leading `/` record-group marker on the first variable line is stripped, and the variable is kept.
- **DATA LIST terminator**: the section ends at the first of: a blank line, a `.` line, or a line starting with `VARIABLE LABELS`, `VALUE LABELS`, `MISSING VALUES`, `FORMATS` or `EXECUTE`. The keyword check is needed for old files (1991 XMF) that have no blank line.
- **DATA LIST decimals**: no annotation → `fmt_type="F"`, `decimals=0`. `(A)`/`(An)` → character. `(n)` → `decimals=n`. `(Fn.d)` → `decimals=d`.
- **DATA LIST-only files** (SFS 1999): `.spss_mono_single` fills `variables.csv` from the layout. `(A)` columns become `character` and all others `numeric`. Labels stay `NA` until parser 7 supplies them.

## SAS details

- **PROC FORMAT codes**: `parse_sas_data_labels()` (the `sas_labels` format, e.g. GSS 2007) parses `VALUE VnnnF` blocks. It links them to variables through the StatCan-style `/* VnnnF format applies to: VAR1 VAR2 */` comments, which may span lines. Files without such comments (Census 2011) yield labels only, no codes.

## PDF dictionary / codebook details

- **`parse_pdf_dictionary()`** (`.parse_pdf_dict_single()`): each variable block starts with `<name>  Position: N  Character/Numeric(w)`. Sections:
  - `Long name:` / `Long nom:` → the variable label
  - `Codes:` / `Domaine:` → code-value labels
  - `Reserved Codes:` / `Codes Réservés:` → sentinels, giving `missing_low`/`missing_high`
  - `Range:`
- **`parse_pdf_codebook()`** (`.parse_pdf_codebook_single()`): each variable block starts with `Variable Name:` / `Nom de la variable :`, and the label is on the `Concept:` line. Wrapped continuation lines are appended until a blank line or the next field key.
  - The `Answer Categories` / `Catégories de réponse` table is parsed with a **right-anchored** code-row regex, `<label>  <code>  <numeric tail>`. The tail must be all-numeric, so a label with interior double spaces cannot be mis-split. It accepts both `2,320` and `4 627` number grouping.
  - A line with no code or numbers is a continuation and is appended to the previous category's label.
  - A single-character label that equals its code (`HHLDSIZC` `1`–`4`) is kept.
  - `Total` rows and rows with no code are dropped.
  - Page headers and footers are stripped with **whole-line-anchored** patterns, so the header text inside a `Concept:` value survives.
  - Every variable is typed `character`, as in `parse_cpss_csv`. Variables with no answer table (`COVID_WT`, `PUMFID`, `VERDATE`) get no codes.
  - Detection candidates are PDFs under a `Codebook`/`LivreDesCodes` path, excluding `zerofreq` variants. Each is checked for the `Variable Name:` + `Answer Categories` signature before use.

## Encoding

- **`metadata_encoding`** defaults to `"CP1252"`, a superset of Latin-1 that handles en-dashes and curly quotes. Exceptions:
  - Census 2021 → `"UTF-8"`
  - Census 1991 (all three file types) and GSS Cycles 8/10/11 (1993/1995/1996), which are DOS-era files → `"CP850"`
  - SHS 2017 and 2019 → `"UTF-8"`. Their reading cards are UTF-8, while those for 2021 and 2023 are not.

  The `detect_formats()` SPSS keyword scan uses `useBytes = TRUE`, so non-UTF-8 bytes never produce warnings.
- **Double-encoded UTF-8**: some command files are *valid* UTF-8 but contain text that was already mis-decoded upstream. The Census 2021 individuals English `.sps`/`.dct` spell `–` as `â€“` in 10 labels (issue #22; the `.do` file is clean). No encoding setting can fix this. Instead, `.fix_metadata_mojibake()` runs on the `merge_metadata()` output. `.fix_mojibake()` works one character at a time: it replaces a CP1252-rendered UTF-8 lead byte plus the exact continuation bytes it requires, and only when those bytes decode to valid UTF-8. Genuine `Âge`/`é`, and strings that mix genuine and damaged accents, come out correctly.

## Name case

Stage 3 uppercases `name` in `variables`, `codes` and `layout` after `read_metadata()`, and `.pumf_read_variables()` does the same for `label_pumf_columns()`. CSV data columns are uppercased on read, but some command files declare mixed-case names (Census 2021 `TotInc`). Without the uppercasing, those variables would silently skip numeric conversion and labelling.
