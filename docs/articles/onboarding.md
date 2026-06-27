# Onboarding a new PUMF

This vignette explains how to bring a Statistics Canada PUMF that
`canpumf` does not yet know about into the package: a brand-new release
year of a survey already covered (e.g. the next cycle of the Canadian
Housing Survey the day it ships), or an entirely new survey series.

The happy path is “drop the files in the right place and call
[`get_pumf()`](https://mountainmath.github.io/canpumf/reference/get_pumf.md)”.
When that is not enough, the second half of this vignette walks through
the deliberate, low-risk workflow for figuring out the configuration a
survey needs — parse the metadata first, borrow a registry entry from a
related survey, tweak until the parse is clean, and only then build the
full table.

``` r

library(canpumf)
#> canpumf.cache_path is not set.
#> Downloaded data is stored in tempdir() and discarded when this R session ends, so it will be re-downloaded next time.
#> To persist data across sessions, set a cache directory:
#>   options(canpumf.cache_path = "~/canpumf_cache")
#> Add that line to your .Rprofile to make it permanent.
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

------------------------------------------------------------------------

## Naming conventions and where to put the files

Everything lives under a single cache directory. Set it once, ideally in
your `.Rprofile`, so data persists across sessions:

``` r

options(canpumf.cache_path = "~/data/pumf.data")
```

Without this option the cache falls back to
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) and is discarded at
the end of the session.

The cache is organised strictly by **series** and **version**:

    <cache_path>/
      <series>/
        <version>/
          <original>.zip              # the StatCan download, retained
          <series>_<version>.duckdb   # built by canpumf
          metadata/                   # canonical CSVs, written by canpumf
            variables.csv
            codes.csv
            layout.csv                # only for fixed-width data

The two names you choose — `<series>` and `<version>` — are the
identifiers you will pass to `get_pumf(series, version)`. A few
conventions matter:

- **`series`** is the survey acronym used throughout the package:
  `"SHS"`, `"SFS"`, `"CHS"`, `"GSS"`, `"ITS"`, `"SGVP"`, `"Census"`,
  `"LFS"`, … Use the existing acronym for a new year of an existing
  survey so it joins its siblings. For a genuinely new survey pick a
  short, stable acronym.
- **`version`** is normally the four-digit reference year (`"2023"`).
  Sticking to a bare year is what unlocks the smart fallback described
  below. Census uses multi-part versions (`"2021 (individuals)"`) and
  LFS accumulates all versions into one shared database — those are
  special-cased and documented in their own vignettes.

To deposit a PUMF manually, create the version directory and drop the
StatCan zip into it (extracted files are fine too — `canpumf` will unzip
nested archives on first use):

``` r

dir.create("~/data/pumf.data/CHS/2025", recursive = TRUE)
file.copy("~/Downloads/2025.zip", "~/data/pumf.data/CHS/2025/")
```

For surveys that `canpumf` knows how to download, you can skip the
manual copy entirely and let
[`get_pumf()`](https://mountainmath.github.io/canpumf/reference/get_pumf.md)
fetch the zip (see
[`list_canpumf_collection()`](https://mountainmath.github.io/canpumf/reference/list_canpumf_collection.md)
/
[`list_pumf_registry()`](https://mountainmath.github.io/canpumf/reference/list_pumf_registry.md)).
Manual deposit is for the case where the file is brand-new, EFT-only, or
otherwise not yet wired into the collection table.

------------------------------------------------------------------------

## Smart defaults and the newest-sibling fallback

A survey’s configuration lives in the **registry** (`R/registry.R`):
which file in the directory is the data file (`file_mask`), how the
bootstrap-weight file joins (`bsw_*`), encodings, and per-variable
fixups. Surveys with no registry entry fall back to **auto-detection**,
which inspects the directory and picks the data file and metadata parser
heuristically.

Two design choices make new years onboard with little or no work:

1.  **Generic year masks.** Recent entries use a `\d{4}` placeholder
    instead of a hard-coded year, e.g. the CHS data file mask is
    `chs\d{4}ecl_pumf\.csv` rather than `chs2022ecl_pumf\.csv`. Because
    each version directory contains exactly one year’s files, the
    generic pattern resolves unambiguously and a new year needs no edit
    to the mask.

2.  **Newest-sibling config inheritance.** When you request a bare-year
    version that has *no* registry entry, and the same series has at
    least one other year-keyed entry,
    [`pumf_registry_lookup()`](https://mountainmath.github.io/canpumf/reference/pumf_registry_lookup.md)
    automatically reuses the config of the newest sibling whose year is
    at or before the one you asked for (or the oldest sibling if your
    year predates them all). It prints a one-time message so the reuse
    is visible:

``` r

# Suppose CHS/2025 has just been released and is not in the registry yet.
tbl <- get_pumf("CHS", "2025")
#> No CHS/2025 registry entry; inheriting config from CHS/2022. Verify the new
#> release matches (file layout, codes, BSW join) and add an explicit entry if
#> it differs.
```

If the 2025 release follows the same naming and layout as 2022 — which
is the common case — this just works: the generic mask finds
`chs2025ecl_pumf.csv`, the BSW join is wired the same way, and the
metadata is parsed from the command files shipped *inside the 2025
download itself* (metadata is always per-version; it is never borrowed
from a sibling, because variable positions and codes drift between
cycles).

Inheritance is deliberately **not** silent and **not** a substitute for
an explicit entry. Treat the message as a prompt to confirm the new
release really matches, and to add a proper `CHS/2025` entry once you
have. Inheritance is skipped for multi-part Census versions and for LFS.

So the first thing to try with any new bare-year release is simply:

``` r

tbl <- get_pumf("CHS", "2025")
tbl |> label_pumf_columns() |> head() |> collect()
```

If the columns look right and the labels resolve, you are done — go to
[section 4](#promote) to make it permanent.

------------------------------------------------------------------------

## When the automatic import fails

If
[`get_pumf()`](https://mountainmath.github.io/canpumf/reference/get_pumf.md)
errors, picks the wrong file, leaves columns unlabeled, or the weights
do not join, stop and work the problem in stages. The golden rule is to
**get the metadata parsing right before building the full table**.
Parsing is cheap and idempotent; building scans the whole data file.

### See what is actually in the directory

``` r

vdir <- file.path(getOption("canpumf.cache_path"), "NEWSURVEY", "2025")
list.files(vdir, recursive = TRUE)
```

Look for: the data file (`.txt`/`.dat` for fixed-width, `.csv` for
delimited), the command/codebook files that describe it (`.sps`, `.sas`,
`.lay`/`.lbe`, `*codebook.csv`, `*Dictionary.pdf`, `.sav`), and any
bootstrap-weight file (often `*bsw*`). The names tell you which parser
will fire and what `file_mask` needs to select.

### Parse the metadata in isolation

[`pumf_metadata()`](https://mountainmath.github.io/canpumf/reference/pumf_metadata.md)
runs only the locate + parse stages and returns the canonical metadata
(`variables`, `codes`, `layout`) without building the DuckDB table. This
is your fast feedback loop:

``` r

meta <- pumf_metadata("NEWSURVEY", "2025")
str(meta$variables)   # one row per variable: name, label_en, label_fr, type, ...
str(meta$codes)       # one row per code value: name, val, label_en, label_fr
str(meta$layout)      # fixed-width column ranges (absent for CSV data)
```

Good signs: every data column appears in `variables`; categorical
variables have rows in `codes`; for fixed-width data, `layout` has
sensible `start`/`end` positions. Common problems and the registry field
that fixes each:

- **Wrong file chosen / “no data file found”** → set `file_mask`.
- **Several command files, parser picks the wrong one** → set
  `layout_mask` to disambiguate the SPSS/SAS files.
- **Garbled accents in French labels / read errors** → set
  `metadata_encoding` (default `"CP1252"`; older DOS-era files sometimes
  need `"CP850"`, some recent ones `"UTF-8"`).
- **Labels missing entirely** (DATA LIST-only SPSS) → supply a
  `*Dictionary.pdf` in the directory so the PDF parser can fill them in.

### Start from an existing entry as a template

You rarely start from a blank slate. Inspect a related entry — an
earlier year of the same survey, or a different survey with the same
file format — and use it as the starting point:

``` r

pumf_registry("CHS", "2022")   # inspect a known entry
list_pumf_registry()           # see everything that is registered
```

Build a candidate configuration with
[`pumf_registry_entry()`](https://mountainmath.github.io/canpumf/reference/pumf_registry_entry.md).
Only the fields you supply are recorded; the rest fall back to pipeline
defaults. You pass it to
[`pumf_metadata()`](https://mountainmath.github.io/canpumf/reference/pumf_metadata.md)
/
[`get_pumf()`](https://mountainmath.github.io/canpumf/reference/get_pumf.md)
via the `registry =` argument **without touching `R/registry.R`** —
perfect for iterating:

``` r

entry <- pumf_registry_entry(
  file_mask     = "PUMF_NEWSURVEY_\\d{4}\\.txt",  # generic year from the start
  bsw_file_mask = "bsw_flatfile\\.txt",
  bsw_join_key  = "CASEID"
)

# Re-parse with the candidate config until the metadata looks right:
meta <- pumf_metadata("NEWSURVEY", "2025", registry = entry, refresh = TRUE)
```

Note `refresh = TRUE`: parsing is idempotent, so once `metadata/` exists
a new `registry` has no effect until you force a re-parse.

If you are adapting a survey whose format matches an existing one,
literally copy that survey’s entry fields into
[`pumf_registry_entry()`](https://mountainmath.github.io/canpumf/reference/pumf_registry_entry.md)
and adjust the masks. This is exactly how a new CHS year reuses the
CHS/2022 shape, and how a new survey might start from the configuration
of an existing one that shares its file format.

### Tweak fixups for data-level issues

Once the structure parses, the build stage may still need per-variable
adjustments. These go in `data_fixups` (see
[`?pumf_registry_entry`](https://mountainmath.github.io/canpumf/reference/pumf_registry_entry.md)
for the full list):

- `force_numeric` — a continuous variable carrying top-code/boundary
  labels; drops the spurious codes but first converts true-missing
  sentinels to an `NA` range.
- `force_character` / `force_integer` / `force_bigint` — override the
  DuckDB storage type so geographic codes keep leading zeros, or
  out-of-range IDs are not truncated.
- `na_values` — raw string values that should become `NA` across all
  columns (undeclared sentinels, SAS-style `"."`).
- `codes_supplement` / `missing_supplement` — inject code rows or
  missing ranges that the command files omit.

``` r

entry <- pumf_registry_entry(
  file_mask   = "PUMF_NEWSURVEY_\\d{4}\\.txt",
  data_fixups = list(
    force_numeric  = "INCOME",
    force_character = "GEOCODE"
  )
)
```

### Build the full table

When the metadata is clean and the fixups are in place, do the full
build:

``` r

tbl <- get_pumf("NEWSURVEY", "2025", registry = entry, refresh = TRUE)

tbl |> label_pumf_columns() |> head() |> collect()   # spot-check labels
tbl |> count() |> collect()                           # row count sanity check
bsw_info(tbl)                                         # confirm weights joined
```

Verify a few known values against the official documentation: a
categorical variable’s levels, a continuous variable’s range, the total
row count, and that the bootstrap weights are present and join 1:1.

------------------------------------------------------------------------

## Promote the configuration into the registry

A `registry =` patch only lasts for the session. Once it works, make it
permanent by cloning the **canpumf** repo, adding an entry to
`R/registry.R` so plain `get_pumf("NEWSURVEY", "2025")` works for
everyone and making a pull request to merge this into the official
package:

``` r

# In R/registry.R, inside the .pumf_registry list:
newsurvey.2025  = .make_entry("NEWSURVEY", "2025",
  file_mask     = "PUMF_NEWSURVEY_\\d{4}\\.txt",   # keep the generic year
  bsw_file_mask = "bsw_flatfile\\.txt",
  bsw_join_key  = "CASEID")
```

Alternatively [open an
issue](https://github.com/mountainMath/canpumf/issues) and document your
successful registry modification and the package maintainers will add it
to the package.

Use the generic `\d{4}` year mask so the next release year inherits
cleanly via the newest-sibling fallback. When you add the entry, keep
the three sources of truth in sync: the registry, the test suite
(`tests/testthat/`, `tests/TEST_COVERAGE.md`), and the README
verified-datasets table.

Finally, **every manual override must be verified against the survey’s
official documentation and recorded** in
`tests/testthat/override_verification.csv`. The
`test-override-verification.R` test fails if an override is missing from
the ledger or marked `pending`/`mismatch`. The workflow for confirming
overrides against the PDF codebook is described in the package’s
`CLAUDE.md` and driven by `tools/verify_overrides.R`.

------------------------------------------------------------------------

## Summary

- Put the PUMF at `<cache_path>/<series>/<version>/`, using the survey
  acronym and a bare four-digit year.
- Try `get_pumf(series, version)` first — generic year masks plus
  newest-sibling inheritance often handle a new year with no code change
  (watch for the inheritance message).
- If it fails, iterate with
  `pumf_metadata(..., registry = pumf_registry_entry(...))` starting
  from a related entry, fixing the parse before building.
- Build with `get_pumf(..., registry = ...)`, spot-check
  labels/rows/weights.
- Promote the working config into `R/registry.R`, sync tests + README,
  and record any overrides in the verification ledger.
