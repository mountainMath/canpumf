# canpumf 0.5.1

## Bug fixes
* `get_pumf("LFS")` (and other calls) no longer trigger spurious RStudio "Error in dbSendQuery(...)" Connections-pane popups. Transient internal DuckDB connections (status checks, write phases, BSW edits) are no longer registered in the RStudio Connections pane; only the final connection returned to the user is registered.

# canpumf 0.5.0

## Major changes
* Data is now imported into DuckDB (breaking change, but only requiring slight modification of code)
* Adaptable metadata parsing registry
* Multiple more robust strategies to parse metadata
* Better data download and import mechanics
* Extensive test suite to prevent regressions and catch if StatCan re-releases data with changed metadata
