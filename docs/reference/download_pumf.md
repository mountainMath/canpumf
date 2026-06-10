# Download PUMF data (deprecated)

\`r lifecycle::badge("deprecated")\`

Use \[pumf_locate_or_download()\] internally or \[get_pumf()\] as the
public entry point.

## Usage

``` r
download_pumf(
  path,
  destination_dir = file.path(tempdir(), "pumf"),
  refresh = FALSE,
  timeout = 3000
)
```

## Arguments

- path:

  Download URL for the PUMF zip file.

- destination_dir:

  Directory to extract into.

- refresh:

  Force re-download even if directory exists.

- timeout:

  Connection timeout in seconds.

## Value

pumf_base_dir that can be used in the other package functions
