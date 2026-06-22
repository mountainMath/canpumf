# Working with multi-module PUMF surveys

``` r

library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(canpumf)
options(canpumf.cache_path = Sys.getenv("COMPILE_VIG_CANPUMF"))
```

Some Statistics Canada PUMF surveys ship **several linked data files**
rather than one. Each file is a different unit of analysis that shares a
common respondent key, and the files are meant to be joined for
analysis. Examples include:

| Survey | Modules | Join key |
|----|----|----|
| GSS cycle 16 — Aging and Social Support (2002) | `MAIN` + `CG4` + `CG6` + `CR` | `RECID` |
| GSS — Time Use (1998, 2010, 2015, 2022) | `Main` + `Episode` | `RECID` / `PUMFID` |
| Survey of Household Spending (2017) | `Interview` + `Diary` | `CASEID` |
| Giving, Volunteering and Participating (1997–2010) | `MAIN` + `GS` / `VD` / `GIVE` / `VOLNTR` | `PUMFID` / `MICRO_ID` / `IDNUM` |

`canpumf` models these as **several tables inside one DuckDB file**, so
the modules can be joined on a single connection.
[`get_pumf()`](https://mountainmath.github.io/canpumf/reference/get_pumf.md)
always returns the survey’s **primary module** (the respondent-level
file that carries the survey weight), and tells you which sibling
modules are available.

## Loading the primary module

[`get_pumf()`](https://mountainmath.github.io/canpumf/reference/get_pumf.md)
returns the main file as usual. For a multi-module survey it also emits
a one-time message listing the other modules and how to open one:

``` r

main <- get_pumf("GSS", "2002")   # primary module (MAIN), carries WGHT_PER
#> GSS/2002 is a multi-module survey; you loaded the primary module. Other linked modules: CG4, CG6, CR.
#> Open one on the same connection with pumf_module(), e.g.:
#>   cg4 <- pumf_module(main, "CG4")
#> GSS/2002 is a multi-module survey; you loaded the primary module. Other linked modules: CG4, CG6, CR.
#> Open one on the same connection with pumf_module(), e.g.:
#>   cg4 <- pumf_module(main, "CG4")

main |> select(1:5) |> head()
#> # A query:  ?? x 5
#> # Database: DuckDB 1.5.4 [root@Darwin 25.5.0:R 4.6.0//Users/jens/data/pumf.data/GSS/2002/GSS_2002.duckdb]
#>   RECID WGHT_PER AGE_2001_GR5      AGE_2001_GR10     SEX   
#>   <dbl>    <dbl> <fct>             <fct>             <fct> 
#> 1     1     367. 45 to 49          45 to 54          Female
#> 2     2     234. 65 to 69          65 to 74          Male  
#> 3     3     142. 80 years and over 75 years and over Male  
#> 4     4     500. 45 to 49          45 to 54          Male  
#> 5     5     828. 55 to 59          55 to 64          Male  
#> 6     6     762. 45 to 49          45 to 54          Female
```

Everything you already know about
[`get_pumf()`](https://mountainmath.github.io/canpumf/reference/get_pumf.md)
output applies to the primary module: values come pre-labelled,
[`label_pumf_columns()`](https://mountainmath.github.io/canpumf/reference/label_pumf_columns.md)
renames columns to their human-readable labels, and
[`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html)
pulls a local tibble.

## Opening a sibling module

Use
[`pumf_module()`](https://mountainmath.github.io/canpumf/reference/pumf_module.md)
to open another module. Crucially, it opens on the **same DuckDB
connection** as `main`, so the two tbls are joinable without a second
connection. The first time you open a module for a survey, `canpumf`
reminds you of the key the modules join on:

``` r

cg4 <- pumf_module(main, "CG4")   # the caregiving module
#> GSS/2002 modules join on 'RECID' (e.g. dplyr::inner_join(main, CG4, by = "RECID")).
#> GSS/2002 modules join on 'RECID' (e.g. dplyr::inner_join(main, CG4, by = "RECID")).

cg4 |> select(1:5) |> head()
#> # A query:  ?? x 5
#> # Database: DuckDB 1.5.4 [root@Darwin 25.5.0:R 4.6.0//Users/jens/data/pumf.data/GSS/2002/GSS_2002.duckdb]
#>   RECID PERSONID CG4_FR_Q100_C                           CG4_FR_Q104 CG4_FR_Q105
#>   <dbl>    <dbl> <fct>                                   <fct>       <fct>      
#> 1     6        1 Neighbour of respondent                 Not asked   Not asked  
#> 2     7        1 Father of respondent                    No          Not asked  
#> 3    10        1 Aunt of respondent                      Not asked   Not asked  
#> 4    14        1 Close friend of respondent              Not asked   Not asked  
#> 5    14        2 Co-worker of respondent and Other rela… Not asked   Not asked  
#> 6    14        3 Co-worker of respondent and Other rela… Not asked   Not asked
```

## Joining modules for analysis

Because both tbls share one connection, the join runs entirely inside
DuckDB — nothing is pulled into R until you
[`collect()`](https://dplyr.tidyverse.org/reference/compute.html). The
respondent-level survey weight lives only on the primary module, so a
typical pattern is to join the detail module to the columns you need
from `main`:

``` r

joined <- cg4 |>
  inner_join(
    main |> select(RECID, WGHT_PER),
    by = "RECID"
  )

joined |>
  summarise(weighted_n = sum(WGHT_PER, na.rm = TRUE)) |>
  collect()
#> # A tibble: 1 × 1
#>   weighted_n
#>        <dbl>
#> 1   9045983.
```

The detail modules typically have a different row count than the primary
module — for example a caregiving or time-use episode module has one row
per episode rather than one row per respondent — so use the join
direction that fits your unit of analysis. But specific requirements may
vary by use case, and an inner join might not always be the right
choice. This decision is thus left to the user.

## A second example: the Survey of Household Spending

The 2017 SHS pairs an `Interview` file (one row per household) with a
`Diary` file (one row per recorded purchase), joined on `CASEID`. Each
module ships its own bootstrap-weight set, so replicate weights stay
attached to the correct unit of analysis:

``` r

shs   <- get_pumf("SHS", "2017")          # Interview (primary)
#> SHS/2017 is a multi-module survey; you loaded the primary module. Other linked modules: Diary.
#> Open one on the same connection with pumf_module(), e.g.:
#>   diary <- pumf_module(main, "Diary")
diary <- pumf_module(shs, "Diary")        # one row per purchase, same connection
#> SHS/2017 modules join on 'CASEID' (e.g. dplyr::inner_join(main, Diary, by = "CASEID")).

diary |>
  inner_join(shs |> select(CASEID), by = "CASEID") |>
  tally() |>
  collect()
#> # A tibble: 1 × 1
#>       n
#>   <dbl>
#> 1  4012
```

## Cleaning up

All modules opened from one
[`get_pumf()`](https://mountainmath.github.io/canpumf/reference/get_pumf.md)
call share a single connection, so a single
[`close_pumf()`](https://mountainmath.github.io/canpumf/reference/close_pumf.md)
on any of the tbls releases it:

``` r

close_pumf(main)
close_pumf(shs)
```

## Database connections

Alternatively the same functionality can be achieved by opening a
general database connection that does not immediately select tables, and
then manually select appropriate subtables:

``` r

con<-get_pumf_connection("SHS", "2017") 
#> Connected to DuckDB (read-write). Available tables: eng_Diary, eng_Interview.
#> Disconnect with DBI::dbDisconnect(con, shutdown = TRUE) when done.

DBI::dbListTables(con)
#> [1] "eng_Diary"     "eng_Interview"
```

``` r

close_pumf(con)
```

## Notes

- `get_pumf("GSS", "2002", module = "CG4")` opens a module
  **standalone** (its own connection). Prefer
  [`pumf_module()`](https://mountainmath.github.io/canpumf/reference/pumf_module.md)
  when you intend to join, so both tbls share one connection.
- The join key is recorded in the survey registry and surfaced in the
  messages above, so you never have to guess it — it varies across
  surveys (`RECID`, `PUMFID`, `MICRO_ID`, `CASEID`, `IDNUM`).
- [`label_pumf_columns()`](https://mountainmath.github.io/canpumf/reference/label_pumf_columns.md)
  and
  [`pumf_var_labels()`](https://mountainmath.github.io/canpumf/reference/pumf_var_labels.md)
  are module-aware: each module is labelled from its own metadata even
  though all modules share one connection.
