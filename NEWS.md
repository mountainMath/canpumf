# canpumf 0.5.1

## New features
* Multi-module survey support. Surveys that ship several linked files sharing a respondent key are now modelled as several joinable tables in one DuckDB file. `get_pumf()` returns the survey's primary (respondent-level) module and emits a one-time message listing the available sibling modules; `pumf_module(tbl, "<module>")` opens a sibling on the **same** connection so the two are joinable, and announces the shared join key. Each module's join key is recorded in the registry (`module_key`) so it never has to be guessed (it varies: `RECID`, `PUMFID`, `MICRO_ID`, `CASEID`, `IDNUM`). Converted surveys include GSS cycle 16 / "Aging and Social Support" 2002 (MAIN + CG4 + CG6 + CR), GSS Time Use 1998/2010/2015/2022 (Main + Episode), the Survey of Household Spending 2017 (Interview + Diary, each with its own bootstrap weights), and the Giving/Volunteering/Participating cycles 1997–2010 (MAIN + GS/VD/GIVE/VOLNTR).
* `close_pumf()` now also accepts a DuckDB connection returned by `get_pumf_connection()`, closing it directly, in addition to a lazy `dplyr::tbl()` returned by `get_pumf()`.
* New `parse_pdf_codebook()` metadata parser for StatCan bilingual PDF *frequency codebooks*. This recovers variable and value labels for surveys whose only machine-readable companion is the data file — notably CPSS cycle 1, which (unlike CPSS 2–6) ships no `variables.csv`. CPSS 1 now imports with full bilingual labels (parity with the other cycles) when `pdftools` is installed. Like the existing PDF data-dictionary parser, it is a label fallback that only fires when no command file or codebook CSV is found, and requires `pdftools` (Suggests).

## Documentation
* New "Working with multi-module PUMF surveys" vignette showing how to load the primary module, open sibling modules with `pumf_module()`, join them inside DuckDB, and use `get_pumf_connection()` / `close_pumf()` directly.
* New "Bootstrap weights" vignette documenting the resampling method, how the weights are stored, stratification, estimating uncertainty, and the incremental re-run behaviour (reuse, adding replicates, and regeneration when rows are added).

## Bug fixes
* `get_pumf("LFS")` (and other calls) no longer trigger spurious RStudio "Error in dbSendQuery(...)" Connections-pane popups. Transient internal DuckDB connections (status checks, write phases, BSW edits) are no longer registered in the RStudio Connections pane; only the final connection returned to the user is registered.
* `add_bootstrap_weights()` on an in-memory `data.frame`/`tibble` that already has replicate columns now extends the existing set (generating only the additional replicates) instead of regenerating a full set and producing duplicate column names. This matches the DuckDB-backed behaviour.
* `add_bootstrap_weights()` now handles rows added to a survey table that already has bootstrap weights correctly. Previously it generated replicates for the new rows in isolation (resampling only among the new rows), which is statistically wrong. It now deletes and regenerates the affected weights: every row when unstratified, or only the strata that gained rows when `strata_cols` are in effect (complete strata keep their existing weights).

# canpumf 0.5.0

## Major changes
* Data is now imported into DuckDB (breaking change, but only requiring slight modification of code)
* Adaptable metadata parsing registry
* Multiple more robust strategies to parse metadata
* Better data download and import mechanics
* Extensive test suite to prevent regressions and catch if StatCan re-releases data with changed metadata
