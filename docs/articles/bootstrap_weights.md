# Bootstrap weights

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
```

PUMF data are a synthetic sample, altered from the original survey
responses for privacy reasons. As with any survey sample, point
estimates carry sampling uncertainty, and it is good practice to
quantify it. Some PUMF ship with bootstrap weights for exactly this
purpose; when they do not, **canpumf** can generate them with
[`add_bootstrap_weights()`](https://mountainmath.github.io/canpumf/reference/add_bootstrap_weights.md).

This vignette explains how
[`add_bootstrap_weights()`](https://mountainmath.github.io/canpumf/reference/add_bootstrap_weights.md)
works: the method it uses, where the weights live, how stratification
works, and — importantly — what happens when you call it again on a
dataset that already has weights.

## The method: a resampling bootstrap

For each of `n_replicates` replicates, a sample of `n` rows is drawn
**with replacement** from the `n` rows of the (stratum of the) survey.
The bootstrap weight for row $`i`$ in replicate $`b`$ is

``` math
 w^{*}_{i,b} = w_i \cdot c_{i,b}, 
```

where $`w_i`$ is the original survey weight and $`c_{i,b}`$ is the
number of times row $`i`$ was selected in replicate $`b`$. Because each
row is selected once on average, $`\mathbb{E}[c_{i,b}] = 1`$ and the
replicate weights are unbiased for the original weights. The spread of
an estimate across the replicates is what gives you its sampling
variance (see [Estimating uncertainty](#estimating-uncertainty)).

The default is `n_replicates = 500`. Pass `seed` for reproducibility.

## Two modes

[`add_bootstrap_weights()`](https://mountainmath.github.io/canpumf/reference/add_bootstrap_weights.md)
works on two kinds of input and behaves differently for each:

- **A DuckDB-backed lazy table** returned by
  [`get_pumf()`](https://mountainmath.github.io/canpumf/reference/get_pumf.md)
  (the typical case). The replicate weights are written into the DuckDB
  file as a persistent, separate table and exposed through a VIEW that
  joins the survey table to the weights. The returned `tbl` points at
  that view. Because the weights are persisted, **subsequent calls reuse
  them** instead of recomputing.
- **An in-memory `data.frame` / `tibble`** (e.g. after
  [`collect()`](https://dplyr.tidyverse.org/reference/compute.html)).
  The weights are generated in memory and the augmented data frame is
  returned. Nothing is persisted.

``` r

cis <- get_pumf("CIS", "2019")

cis_bsw <- cis |>
  add_bootstrap_weights(weight_col = "FWEIGHT", n_replicates = 200, seed = 42)
#> Generating 200 CPBSW replicates for 72643 observations...
#>   Replicate 20 / 200 ...
#>   Replicate 40 / 200 ...
#>   Replicate 60 / 200 ...
#>   Replicate 80 / 200 ...
#>   Replicate 100 / 200 ...
#>   Replicate 120 / 200 ...
#>   Replicate 140 / 200 ...
#>   Replicate 160 / 200 ...
#>   Replicate 180 / 200 ...
#>   Replicate 200 / 200 ...
#> Adding 'pumf_row_id' column to 'eng'...
#> Writing bootstrap weight table 'pumf_bsw_fweight' to DuckDB...

# 200 replicate columns CPBSW1 … CPBSW200 are now available
grep("^CPBSW", colnames(cis_bsw), value = TRUE) |> head()
#> [1] "CPBSW1" "CPBSW2" "CPBSW3" "CPBSW4" "CPBSW5" "CPBSW6"
```

## Where the weights are stored (DuckDB path)

The replicate weights are **not** copied into the main survey table.
Instead:

- They are written to a separate DuckDB table, auto-named
  `paste0("pumf_bsw_", tolower(weight_col))` (e.g. `pumf_bsw_pweight`).
  The auto-naming means generating weights for two different weight
  columns (e.g. a household weight and a person weight) produces two
  independent BSW tables that never collide.
- A companion VIEW (e.g. `eng_bsw_pweight`) joins the survey table to
  the BSW table on a row identifier, so the returned `tbl` exposes every
  survey column plus the replicate columns.

[`bsw_info()`](https://mountainmath.github.io/canpumf/reference/bsw_info.md)
summarises what is stored:

``` r

bsw_info(cis_bsw)
#> # A tibble: 1 × 6
#>   weight_col bsw_table        view_name       view_exists n_replicates size_mb
#>   <chr>      <chr>            <chr>           <lgl>              <int>   <dbl>
#> 1 FWEIGHT    pumf_bsw_fweight eng_bsw_fweight TRUE                 200    0.07
```

This separation keeps the main table untouched and lets you add,
inspect, or remove replicate weights independently of the survey data.

## Identifying rows

To join replicate weights back to the survey rows a stable row
identifier is needed. With `id_col = NULL` (the default):

- the survey registry’s `bsw_join_key` is used when one is defined (e.g.
  `PEFAMID` for the SFS), requiring no change to the table; otherwise
- a `pumf_row_id` column (DuckDB `rowid`) is added to the survey table
  once.

You can always pass `id_col` explicitly to use a natural key.

## Stratified bootstrap weights

Many survey designs are stratified, and the resampling should respect
that: each replicate should resample **within** each stratum, preserving
the stratum sample sizes. Pass `strata_cols`:

``` r

cis_bsw <- cis |>
  add_bootstrap_weights(weight_col = "FWEIGHT", strata_cols = "PROV",
                        n_replicates = 200, seed = 42)
```

Resolution order for the strata:

1.  `strata_cols` if you pass it,
2.  otherwise the registry `bsw_strata` for the survey,
3.  otherwise, for the LFS, the default `c("SURVYEAR", "SURVMNTH")` so
    each month is resampled separately,
4.  otherwise unstratified.

Pass `strata_cols = character(0)` to force unstratified weights even
when a default exists (e.g. for the LFS).

## Estimating uncertainty

To get a bootstrap standard error for an estimate, compute the estimate
once with the original weights and once with each replicate weight; the
spread of the replicate estimates is the sampling variability. The
example below estimates the total population represented by the survey
(the sum of the weights), which depends only on the weight columns, but
the same pattern applies to any weighted statistic — replace `sum(.x)`
with your estimator (a weighted mean, share, total, …) evaluated with
each weight column.

``` r

est <- cis_bsw |>
  summarise(across(c(FWEIGHT, matches("^CPBSW[0-9]+$")), ~ sum(.x))) |>
  collect()
#> Warning: Missing values are always removed in SQL aggregation functions.
#> Use `na.rm = TRUE` to silence this warning
#> This warning is displayed once every 8 hours.

point_estimate      <- est$FWEIGHT
replicate_estimates <- est |> select(matches("^CPBSW[0-9]+$")) |> unlist()

# Bootstrap variance: mean squared deviation of the replicate estimates from
# the full-sample estimate; the standard error is its square root.
# Confidence intervals can be obtained by taking the appropriate quantiles.
confidence_interval <- quantile(replicate_estimates,c(0.025,0.975))
std_error <- sqrt(mean((replicate_estimates - point_estimate)^2))

c(estimate = point_estimate, se = std_error, conf=confidence_interval)
#>   estimate         se  conf.2.5% conf.97.5% 
#>   36831173     170660   36493061   37126328
```

The same pattern works after
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html) /
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html):
carry the `CPBSW*` columns through the summary, then collapse them into
a standard error per group.

## Incremental re-runs

Calling
[`add_bootstrap_weights()`](https://mountainmath.github.io/canpumf/reference/add_bootstrap_weights.md)
again on a survey that already has weights does only the work needed to
satisfy the request. The decision is driven by two questions: are there
enough replicate columns, and do all rows have weights?

### Reuse — nothing to do

If the stored weights already cover every row and there are at least
`n_replicates` of them, they are reused without recomputation
(requesting fewer than are stored simply returns a subset). This is why
repeatedly opening a weighted survey is cheap.

### More replicates requested

If you ask for more replicates than are stored (and no rows were added),
the additional replicate columns are generated and appended; the
existing columns are kept unchanged. When stratified, the new columns
are resampled within strata, consistent with the existing ones.

``` r

# First 200 replicates …
cis_bsw <- add_bootstrap_weights(cis, "FWEIGHT", n_replicates = 200, seed = 42)
# … later, extend to 500: only CPBSW201 … CPBSW500 are generated.
cis_bsw <- add_bootstrap_weights(cis, "FWEIGHT", n_replicates = 500, seed = 42)
```

### Rows added to the survey table

This is the subtle case. A bootstrap replicate resamples the **full
population** (or full stratum), so adding rows invalidates the replicate
weights of the affected resampling universe — you cannot simply generate
weights for the new rows in isolation.
[`add_bootstrap_weights()`](https://mountainmath.github.io/canpumf/reference/add_bootstrap_weights.md)
detects that not all rows have weights and **deletes and regenerates**
the affected weights:

- **Unstratified:** the whole population changed, so every row’s
  replicate weights are regenerated.
- **Stratified:** only the strata that gained rows are regenerated in
  full; strata that are already complete keep their existing weights.
  This makes re-weighting an updated multi-year file (for example,
  appending a new LFS month or year) inexpensive — only the new strata
  are recomputed.

Extending the number of replicates and adding rows can happen in the
same call; each is handled on the rows it applies to.

> Extending a weighted table after appending rows relies on the new rows
> having a valid identifier. Use a natural `id_col` (or the registry
> `bsw_join_key`) when you expect to grow the table, so the new rows can
> be matched.

### Forcing a full regeneration

Pass `overwrite = TRUE` to discard the stored weights and regenerate
from scratch regardless of what is already there.

``` r

cis_bsw <- add_bootstrap_weights(cis, "PWEIGHT", n_replicates = 200,
                                 seed = 42, overwrite = TRUE)
```

## Multiple weight columns

Because each BSW table is named after its weight column, hierarchical
surveys with several weights are handled by simply calling the function
once per weight:

By default all replicate columns share the `CPBSW` prefix, so when
storing weights for more than one weight column give each a distinct
`prefix` to tell the two sets of replicates apart (the column names are
survey-specific):

``` r

tbl <- get_pumf("<series>", "<version>") |>
  add_bootstrap_weights(weight_col = "<household_weight>", prefix = "HHBSW") |>
  add_bootstrap_weights(weight_col = "<person_weight>",    prefix = "PPBSW")
```

## Filtered input tables

Bootstrap weights always cover the complete physical survey table, so
variance estimates use the full sample. If the input `tbl` carries
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html)
operations, they are captured and re-applied to the returned view, so
the visible rows still match your subset. Other operations
([`select()`](https://dplyr.tidyverse.org/reference/select.html),
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html), …) are
not replayed — apply them to the returned tbl instead.

## Connection lifecycle

Generating weights on a DuckDB-backed table requires exclusive write
access, so
[`add_bootstrap_weights()`](https://mountainmath.github.io/canpumf/reference/add_bootstrap_weights.md)
shuts down the connection held by the input `tbl`. **Use the returned
tbl** afterwards; the input `tbl` (and any other lazy tables backed by
the same DuckDB file) are invalid after the call.

## Inspecting and removing weights

``` r

bsw_info(cis_bsw)                      # list stored BSW tables and replicate counts
cis_clean <- remove_bootstrap_weights(cis_bsw, weight_col = "PWEIGHT")
```

[`remove_bootstrap_weights()`](https://mountainmath.github.io/canpumf/reference/remove_bootstrap_weights.md)
drops the BSW table and its companion view and returns a clean tbl on
the original survey table.
