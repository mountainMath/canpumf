# Get select pumf data files

This is a convenience function that downloads and accesses pumf data for
a curated set of pumf datasets.

## Usage

``` r
get_pumf(
  pumf_series,
  pumf_version = NULL,
  layout_mask = NULL,
  file_mask = layout_mask,
  pumf_cache_path = getOption("canpumf.cache_path"),
  refresh = FALSE,
  refresh_layout = FALSE,
  timeout = 3000
)
```

## Arguments

- pumf_series:

  sereis for the pumf data, like LSF, or CHS

- pumf_version:

  In case there are several versions of a given series, like for LFS,
  the version

- layout_mask:

  optional layout mask in case there are several files. identifiers. For
  LFS this is the month/year.

- file_mask:

  optional additional mask to filter down to specific PUMF file if there
  are several

- pumf_cache_path:

  A path to a permanent cache. If none is fould the data is stored in
  the temporary directory for the duration of the session.

- refresh:

  optionall re-downlad pumf data, only for series that can be downloaded
  directly from StatCan

- refresh_layout:

  (optional) regenerate the layout and metadata

- timeout:

  Optional parameter to specify connection timeout for download

## Value

A tibble with the pumf data.
