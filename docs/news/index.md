# Changelog

## canpumf 0.5.0

### Major changes

- Data is now imported into DuckDB (breaking change, but only requiring
  slight modification of code)
- Adaptable metadata parsing registry
- Multiple more robust strategies to parse metadata
- Better data download and import mechanics
- Extensive test suite to prevent regressions and catch if StatCan
  re-releases data with changed metadata
