# Open PUMF documentation in browser

When available this will open the PUF user guide in the default browser.

## Usage

``` r
open_pumf_documentation(
  pumf_series,
  pumf_version = NULL,
  documentation_type = "user_guide",
  pumf_cache_path = getOption("canpumf.cache_path")
)
```

## Arguments

- pumf_series:

  sereis for the pumf data, like LSF, or CHS

- pumf_version:

  In case there are several versions of a given series, like for LFS,
  the version

- documentation_type:

  which documentation to open, either "user_guide", "reference_guide" or
  "quality" or "errata". Not all types are available for all PUMFs.
  identifiers. For LFS this is the month/year.

- pumf_cache_path:

  A path to a permanent cache. If none is fould the data is stored in
  the temporary directory for the duration of the session.

## Value

nothing, opens document in browser
