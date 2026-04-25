# Download PUMF data

Download PUMF data

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

  Download path for PUMF SPSS data

- destination_dir:

  Optional path where to store the extracted PUMF data, default is
  \`file.path(tempdir(),"pumf")\`

- refresh:

  Optional parameter to force re-download of PUMF data

- timeout:

  Optional parameter to specify connection timeout for download

## Value

pumf_base_dir that can be used in the other package functions
