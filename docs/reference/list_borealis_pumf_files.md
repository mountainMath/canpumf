# List the files of a Borealis PUMF dataset

Shows every file in a Borealis dataset together with the role canpumf
assigns it (\`data\`, \`metadata\`, \`doc\` or \`skip\`) and whether
\`get_pumf(..., borealis =)\` would download it. Use it to check which
data file and command files a DOI provides before loading it.

## Usage

``` r
list_borealis_pumf_files(doi)
```

## Arguments

- doi:

  The dataset DOI, e.g. \`"doi:10.5683/SP3/LG7WKC"\` (the \`doi:\`
  prefix, a bare \`10.5683/...\` or a doi.org URL all work), or a
  one-row tibble from \[list_borealis_pumf_catalogue()\].

## Value

A tibble with one row per file: \`file_id\`, \`filename\`,
\`directory\`, \`size\` (bytes), \`md5\`, \`content_type\`, \`original\`
(the uploaded file behind a Dataverse \`.tab\` ingest), \`restricted\`,
\`role\` and \`selected\`. The dataset DOI and title are attached as
attributes.

## See also

\[list_borealis_pumf_catalogue()\], \[get_pumf()\]

## Examples

``` r
# \donttest{
tryCatch(list_borealis_pumf_files("doi:10.5683/SP3/LG7WKC"),
         error = function(e) message(conditionMessage(e)))
#> # A tibble: 13 × 10
#>    file_id filename      directory   size md5   content_type original restricted
#>      <int> <chr>         <chr>      <dbl> <chr> <chr>        <chr>    <lgl>     
#>  1  562715 pumf-95M00-E… CSV       6.95e6 4c87… text/csv     NA       FALSE     
#>  2  562589 pumf-95M00-E… STATA     3.73e6 04c0… application… NA       FALSE     
#>  3  562623 pumf-95M00-E… STATA7    3.67e6 ebd8… application… NA       FALSE     
#>  4  562588 pumf-95M00-E… STATA     5.52e3 8fae… application… NA       FALSE     
#>  5  562535 pumf-95M00-E… SPSS      3.63e6 8038… application… NA       FALSE     
#>  6  563148 pumf-95M00-E… NA        6.90e6 8038… text/tab-se… pumf-95… FALSE     
#>  7  562399 pumf1971rcl_… Survey d… 2.54e7 0676… application… NA       FALSE     
#>  8  562467 pumf71i.sps   Survey d… 1.96e4 3053… application… NA       FALSE     
#>  9  562435 pumf71i_code… Survey d… 5.20e4 dcc8… text/plain   NA       FALSE     
#> 10  562680 pumf_95M00_E… SAS       1.82e4 e4c3… application… NA       FALSE     
#> 11  562679 pumf_95M00_E… SAS       5.43e6 ca6d… text/plain   NA       FALSE     
#> 12  562505 pust71i.sas   Survey d… 2.36e4 d36c… application… NA       FALSE     
#> 13  562352 readstep.html Survey d… 2.47e5 16a9… text/html    NA       FALSE     
#> # ℹ 2 more variables: role <chr>, selected <lgl>
# }
```
