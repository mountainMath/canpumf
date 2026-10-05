# Future enhancements

Internal list of improvements that are known but not scheduled. Each entry says what is wrong or missing, the evidence, and the intended fix.

## Census 2021 (individuals): French label of `ETHDER` code 26

**Type:** data quality / upstream documentation error
**Noted:** 2026-10-05, canpumf 0.6.1 (while checking #27)
**Affects:** `get_pumf("Census", "2021 (individuals)", lang = "fra")`

### Problem

Statistics Canada gives `ETHDER` codes 21 and 26 the same French label, "Autres origines d'Europe du Sud-Est". Code 26 is the Eastern European group, so its label should read "Autres origines d'Europe de l'Est".

Since 0.6.1 the two codes no longer merge into one level (#27): both occur in the data, so they come out as "Autres origines d'Europe du Sud-Est (21)" and "Autres origines d'Europe du Sud-Est (26)". The codes are distinguishable, but the label of 26 is still wrong.

### Evidence

| Source | Code 21 | Code 26 |
|---|---|---|
| English SPSS / `codes.csv` | Other Southeast European origins | Other Eastern European origins |
| English user guide (`2021 Census Individuals PUMF User Guide_v2.pdf`, p. 23) | Other Southeast European origins, 5,538 records | Other Eastern European origins, 2,460 records |
| French SPSS (`FMGD 2021 particuliers SPSS FR_v2.sps`, lines 681 and 686) | Autres origines d'Europe du Sud-Est | Autres origines d'Europe du Sud-Est |
| French user guide (`Guide de l'utilisateur du FMGD particuliers du Recensement_v2.pdf`, p. 26) | Autres origines d'Europe du Sud-Est, 5 538 | Autres origines d'Europe du Sud-Est, 2 460 |
| Data (`fra` table, 0.6.1 build) | 5,538 records | 2,460 records |

The French guide repeats the wrong label in its table, but the note beside code 26 reads "Comprend les réponses uniques pour les origines d'Europe de l'Est", and the record counts match the English code 26. The error is therefore in StatCan's French label, in both the command file and the guide table.

### Intended fix

* Add a `codes_override` to the `"Census/2021 (individuals)"` registry entry that sets the French label of `ETHDER` 26 to "Autres origines d'Europe de l'Est" (as done for Census 1986 households `HHMOTG`). With distinct labels the code suffix disappears from both levels.
* Add the ledger row to `tests/testthat/override_verification.csv` with the sources above, and a post-build check in `test-pipeline-census.R`.
* Existing French caches need a rebuild to pick it up.
* The exact wording "Autres origines d'Europe de l'Est" is inferred from the guide's note and the pattern of the neighbouring labels; StatCan publishes no corrected French label for this code.
