# canpumf 0.5.1

## New features
* New `parse_pdf_codebook()` metadata parser for StatCan bilingual PDF *frequency codebooks*. This recovers variable and value labels for surveys whose only machine-readable companion is the data file — notably CPSS cycle 1, which (unlike CPSS 2–6) ships no `variables.csv`. CPSS 1 now imports with full bilingual labels (parity with the other cycles) when `pdftools` is installed. Like the existing PDF data-dictionary parser, it is a label fallback that only fires when no command file or codebook CSV is found, and requires `pdftools` (Suggests).

## Documentation
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
