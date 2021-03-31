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


## Example

This is a basic example how to read a StatCan PUMF file in R. It assume that the variable `pumf_base_path` points to the base directory of the PUMF data. The base directory hold the data as well as the syntax or layour files are subdirectories. Sometimes the layout or syntax file can also be subdirectories of the data directory.

``` r
library(canpumf)

pumf_base_path <- "<path to base directory of pumf data>"

pumf_data <- canpumf::read_pumf_data(pumf_base_path)
```

The code may need further refinements to work with all StatCan PUMF files. It has been successfully tried with the following PUMF files:

* CHS 2018
* ITS 2017
* VTS 2018
* TSRC 2017
* Covid Series 3


