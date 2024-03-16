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

StatCan publishes an [official guide to the Labour Force Surve](https://www150.statcan.gc.ca/n1/en/catalogue/71-543-G) for different vintages of the [LFS](https://www23.statcan.gc.ca/imdb/p2SV.pl?Function=getSurvey&SDDS=3701).

## Cache path

PUMF data can be large and should be cached locally. For this purpose users should set the options variable canpumf.cache_path to a directory where the data can be stored via `options(canpumf.cache_path="<your local path>")` in your `.Rprofile`, alternatively the path can be specified with every call.

## Basic usage
Some PUMF data is available from StatCan via direct download, this data can be directly accessed via the `get_pumf()` function. In other cases, PUMF data has to be ordered via EFT and shuold be deposited in the cache directory so `get_pumf()` function can find it. The `label_pumf_data()` function parses metadata and labels the data with human-readable labels.

## LFS data
For LFS data, the data is orgnaized by year except for the current year where it’s organized by month. To access the data for 2022 you would use:

```
lfs_2022 <- get_pumf("LFS","2022")
```
This will download the 2022 LFS pumf data if needed and load it.

StatCan unfortunately does not provide standardized metadata for PUMF files, but the canpumf package also parses the SPSS Command Files in order to allow for automated labelling of the data. To label the data with human-readable labels use

```
lfs_2022_labelled <- lfs_2022 |>
  label_pumf_data()
```

Sometimes for people familiar with the data the standard column names the PUMF data comes with are more convenient to work with than the long human-readable names, in this case you can use `label_pumf_data(rename_columns=FALSE)` to only label the column categories.

Care should be taken with identifying codes for unavailable or unapplicable data in numeric columns, these aren’t provided in machine readable format for most PUMF data and have to be handled manually. Always refer to the PUMF data dictionaries.

## Census data
The canpumf package can currently handle the Census individuals PUMF for the 1996 through 2021 censuses. These have to be ordered via EFT and places into the canpumf cache path, with a directory name equal to the original filename that data came with via EFT that contains the product identifier and census year so the package can find the data. It can then be accessed via:

```
pumf_2021 <- get_pumf("Census","2021") |>
  label_pumf_data()
```

By default the package will load the *individuals* file, 
the *hierarchical* is availabel for the 2006, 2011, 2016 censuses, the *families* or *households* versions are available for the 1971, 1976, 1986, 1991, 1996, and 2001 censuses.

```
pumf_h_2016 <- get_pumf("Census","2016 (hierarchical)") |>
  label_pumf_data()
```

## Other PUMF
The package makes an attempt to parse other PUMF files.

The code may need further refinements to work with all StatCan PUMF files. It has been successfully tried with the following PUMF files:

* CHS 2018
* ITS 2017
* VTS 2018
* TSRC 2017
* Covid Series 3
* SFS 2019

### Cite **canpumf**

If you wish to cite the `canpumf` package in your work:

  von Bergmann, J. (2024), canpumf: Import StatCan PUMF data into R. v0.2.0.

A BibTeX entry for LaTeX users is
```
  @Manual{,
    author = {Jens {von Bergmann}},
    title = {canpumf: Import StatCan PUMF data into R},
    year = {2024},
    note = {R package version 0.2.0},
    url = {https://mountainmath.github.io/canpumf/},
  }
```


