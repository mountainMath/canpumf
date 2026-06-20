# Bundled extdata resources

Files under `inst/extdata/` are internal package resources used by the metadata
parser. They are **not** user-facing data objects and are loaded programmatically
via `system.file("extdata", ..., package = "canpumf")`.

## `census_1991/`

English SPSS/XMF command files for the 1991 Census of Population Public Use
Microdata Files (PUMF):

| File         | PUMF type    |
|--------------|--------------|
| `IND91.XMF`  | Individuals  |
| `HHOLD91.XMF`| Households   |
| `CNCF91.XMF` | Families     |

**Why they are bundled:** the 1991 Census PUMF is distributed by Statistics
Canada as French-only EFT archives, and the downloaded `.XMF` command files
(encoded CP850) therefore carry only French variable and value labels. These
bundled English command files supply the corresponding English labels so that
`get_pumf("Census", "1991 (...)")` populates both `label_en` and `label_fr`.

They are referenced from the survey registry via the `bundled_eng_sps` field
(see `R/registry.R`, the `Census/1991 (...)` entries) and read by the SPSS
command-file parser (`R/metadata_parsers.R`).

**Source:** Statistics Canada, 1991 Census Public Use Microdata Files.
