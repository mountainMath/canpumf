# canpumf

<!-- badges: start -->
<!-- badges: end -->

The goal of {canpumf} is to facilitate ingesting StatCan PUMF data in R.

## Installation

You can install the current development version of canpumf from [GitHub](https://github.com/mountainMath/canpumf) with:

``` r
remotes::install_github("mountainmath/canpumf")
```

## Documentation
Please consult the [documentation and example articles](https://mountainmath.github.io/canpumf/) for further information.

StatCan publishes an [official guide to the Labour Force Survey](https://www150.statcan.gc.ca/n1/en/catalogue/71-543-G) for different vintages of the [LFS](https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&SDDS=3701).

## Cache path

PUMF data can be large and should be cached locally. Set the `canpumf.cache_path` option to a local directory via `options(canpumf.cache_path="<your local path>")` in your `.Rprofile`. Without this, data is stored in `tempdir()` for the session only.

## Basic usage

Some PUMF data is available from StatCan via direct download and can be accessed directly via `get_pumf()`. In other cases, PUMF data must be ordered via EFT and deposited in the cache directory so `get_pumf()` can find it.

`get_pumf()` downloads (if needed), parses metadata, applies value labels automatically, and returns a lazy `dplyr::tbl()` backed by a local DuckDB database. Call `dplyr::collect()` to load into memory.

Column values are labeled automatically (e.g. province codes become factor levels like `"British Columbia"`). Column *names* remain as short coded names by default (e.g. `PROV`, `LFSSTAT`). To rename columns to human-readable variable labels, pipe through `label_pumf_columns()`:

```r
tbl <- get_pumf("LFS", "2022") |>
  label_pumf_columns()
```

When done querying, release the DuckDB connection with `close_pumf(tbl)`.

## LFS data

LFS data is organized by year, except for the current year where it is organized by month. To access data for a specific year:

```r
lfs_2022 <- get_pumf("LFS", "2022")
```

This downloads the 2022 LFS PUMF data if needed, parses it, loads labeled data into a shared DuckDB database, and returns a lazy tbl filtered to 2022. To access all LFS data currently in the local database:

```r
lfs_all_local <- get_pumf("LFS")
```

To ensure the local database contains all available LFS versions, use `refresh = "auto"`. This checks StatCan for versions not yet in the database and imports them:

```r
lfs_all <- get_pumf("LFS", refresh = "auto")
```

## Census data

The canpumf package supports Census PUMF from 1971 through 2021. The 2021 and 2016 files are available via direct download; older years must be ordered through Statistics Canada's EFT portal and placed in the cache directory.

```r
pumf_2021 <- get_pumf("Census", "2021")
```

By default the package loads the *individuals* file. Available variants by year:

| Years | Variants |
|---|---|
| 2021 | individuals, hierarchical |
| 2016 | individuals, hierarchical |
| 2011 | individuals (NHS), hierarchical (NHS) |
| 2006 | individuals, hierarchical |
| 2001 | individuals, households, families |
| 1996 | individuals, households, families |
| 1991 | individuals, households, families |
| 1986 | individuals, households |
| 1981 | individuals, households |
| 1976 | individuals |
| 1971 | individuals, individuals PR |

```r
pumf_h_2016 <- get_pumf("Census", "2016 (hierarchical)")
```

## Other PUMF

The package makes an attempt to parse other PUMF files. It has been successfully used with:

* CHS 2018, 2021, 2022
* ITS 2017
* VTS 2018
* TSRC 2017
* Covid Series 3
* SFS 2019
* SHS 2019

### Cite **canpumf**

If you wish to cite the `canpumf` package in your work:

  von Bergmann, J. (2026), canpumf: Import StatCan PUMF data into R. v0.5.0.

A BibTeX entry for LaTeX users is
```
  @Manual{,
    author = {Jens {von Bergmann}},
    title = {canpumf: Import StatCan PUMF data into R},
    year = {2026},
    note = {R package version 0.5.0},
    url = {https://mountainmath.github.io/canpumf/},
  }
```
