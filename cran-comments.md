# canpumf 0.5.0

* This is a new release.

## Major changes
* Data is now imported into DuckDB (breaking change, but only requiring slight modification of code)
* Adaptable metadata parsing registry
* Multiple more robust strategies to parse metadata
* Better data download and import mechanics
* Extensive test suite to prevent regressions and catch if StatCan re-releases data with changed metadata

## R CMD check results

0 errors | 0 warnings | 0 note

Checked using

```
R version 4.5.2 (2025-10-31)
Platform: aarch64-apple-darwin20
Running under: macOS Tahoe 26.5
```


## Test results

[ FAIL 0 | WARN 0 | SKIP 27 | PASS 14534 ]
