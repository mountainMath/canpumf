# canpumf 0.5.3

This is an update to the version currently on CRAN (0.5.2).

The main changes:

* PUMF data can now also be loaded from the Borealis Dataverse
  (<https://borealisdata.ca>), which hosts Statistics Canada PUMFs that
  Statistics Canada itself does not post online. Statistics Canada stays the
  primary source.
* Several metadata-parsing fixes (implied decimals in fixed-width data,
  labelled missing codes in numeric columns, repair of double-encoded UTF-8
  labels). See NEWS.md.

`curl` and `jsonlite` are new in Imports. They are used for concurrent paging
of the Borealis search API.

The package policy is unchanged. All file output goes to
`getOption("canpumf.cache_path", tempdir())`. The network-facing examples are
in `\donttest{}` and fail gracefully when the remote servers are unreachable.
The tests that need the network are skipped on CRAN.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test results

[ FAIL 0 | WARN 0 | SKIP 124 | PASS 14100 ]

The tests download data from Statistics Canada and Borealis, so they run
only when `NOT_CRAN` is `"true"`. The skips cover survey vintages that are
not in the local cache and bilingual checks for the English-only Borealis
copies.

## Test environments

* local macOS 27 (aarch64-apple-darwin23), R 4.6.0
* GitHub Actions: macOS (release), Windows (release), Ubuntu (devel, release,
  oldrel-1)
