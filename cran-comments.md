# canpumf 0.6.1

This is an update to the version currently on CRAN (0.6.0, published
2026-09-25).

It follows 0.6.0 closely because of duckdb 1.5.6 (on CRAN since 2026-09-29).
That release makes `dbConnect()` fail when its `read_only` differs from the
database instance the R process already holds for the file, where earlier
versions handed the existing instance back. With canpumf 0.6.0 and duckdb
1.5.6, `get_pumf()` therefore fails for a file the session holds read-write
(`get_pumf_connection()`, `get_pumf(read_only = FALSE)`), and a write to a file
held by a read-only table reports duckdb's error instead of the advice to
close the table. 0.6.1 works with duckdb before and after 1.5.6. The CRAN
checks of 0.6.0 are not affected, because the tests that exercise this need
downloaded data and do not run on CRAN.

The other changes:

* Special values (not stated, valid skip, ...) that are blanked to `NA` are
  kept in a companion table, returned by the new `pumf_sentinels()`.
* New `pumf_dictionary()`, `pumf_translate()` and `pumf_topcodes()` for
  bilingual reporting.
* Value labels are unique per variable.
* `add_bootstrap_weights()` no longer closes and reopens the connection of the
  table it is given. It stores the weights only through a connection opened
  for writing (`get_pumf(read_only = FALSE)`) and otherwise keeps them in a
  temporary table of the session.
* `remove_pumf_cache()` can remove a single language.
* Further Canadian Internet Use Survey releases, and fixes for the GSS Cycle 36
  Time Use episode file and several Census files. See NEWS.md.

There are no changes to the dependencies.

The package policy is unchanged. All file output goes to
`getOption("canpumf.cache_path", tempdir())`. The network-facing examples are
in `\donttest{}` and fail gracefully when the remote servers are unreachable.
The tests that need the network are skipped on CRAN.

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Days since last update: 5

  The reason for the short interval is the duckdb 1.5.6 release described
  above.

## Test results

[ FAIL 0 | WARN 0 | SKIP 151 | PASS 24962 ]

The tests download data from Statistics Canada and Borealis, so they run
only when `NOT_CRAN` is `"true"`. The skips cover survey vintages that are
not in the local cache and bilingual checks for the English-only Borealis
copies.

## Test environments

* local macOS 27 (aarch64-apple-darwin23), R 4.6.0, duckdb 1.5.6
* GitHub Actions: macOS (release), Windows (release), Ubuntu (devel, release,
  oldrel-1)
