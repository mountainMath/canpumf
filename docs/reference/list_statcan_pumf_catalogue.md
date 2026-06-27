# Crawl the full Statistics Canada PUMF catalogue (experimental)

Scrapes the live StatCan "Public use microdata" listing and follows each
survey to its product page to discover every PUMF series, its editions,
and direct-download URLs. This is an exploratory counterpart to
\[list_canpumf_collection()\], which returns only the curated set of
surveys canpumf has tested download wrappers for.

## Usage

``` r
list_statcan_pumf_catalogue(
  prefer = names(.statcan_format_tokens),
  max_surveys = NULL,
  surveys = NULL,
  verbose = TRUE,
  refresh = FALSE,
  cache_path = getOption("canpumf.cache_path")
)
```

## Arguments

- prefer:

  Character vector of format tokens in order of preference; the default
  puts CSV / flat text ahead of statistical-package formats.

- max_surveys:

  Optional integer: only crawl the first N surveys (useful for a quick
  look — a full crawl issues a few hundred requests).

- surveys:

  Optional character vector of catalogue ids to restrict to.

- verbose:

  If \`TRUE\`, print progress as each survey is crawled.

- refresh:

  If \`FALSE\` (the default), the crawl result is cached and reused — a
  full crawl is expensive (hundreds of requests). Within a session it is
  held in memory; a \*full\* crawl (no \`max_surveys\`/\`surveys\`) is
  also persisted to disk under \`cache_path\` so it survives across
  sessions. Set \`TRUE\` to re-scrape the live catalogue and replace
  both caches, e.g. to pick up a newly released survey.

- cache_path:

  Directory for the cross-session catalogue cache
  (\`pumf_catalogue.rds\`). Defaults to
  \`getOption("canpumf.cache_path")\`; when unset there is no durable
  cache and only the in-session cache is used. A persisted catalogue
  older than \`getOption("canpumf.catalogue_max_age_days", 30)\`
  triggers a staleness warning suggesting \`refresh = TRUE\`. If a live
  crawl fails (StatCan unreachable) the last persisted copy is returned
  with a warning.

## Value

A tibble with one row per discovered edition: \`catalogue_id\`,
\`Acronym\`, \`SeriesTitle\`, \`Title\`, \`survey_url\`, \`edition\`,
\`format\`, \`url\`, and \`product_url\`. \`SeriesTitle\` is the
plain-language series name matching the acronym (the catalogue title
with the edition-specific tail and "Public Use Microdata File"
boilerplate stripped). \`Title\` is edition-specific: StatCan's own
per-edition catalogue title where it carries one, otherwise — for
\*umbrella\* products whose catalogue title is only the series name
(e.g. the consolidated General Social Survey, or a census year's
individuals/hierarchical pair) — a synthesised \`"\<series\> —
\<edition\>"\`, where the structural edition descriptor disambiguates
colliding years (GSS \`"Cycle 16 (2002)"\`, census \`"2021
(individuals)"\`). \`edition\` remains the reference period/variant.
\`survey_url\` is the survey's catalogue overview page (the L2 page the
crawler followed); \`url\`/\`product_url\` point at the individual
edition's download and product page. \`Acronym\` and \`SeriesTitle\` are
derived from the title since StatCan exposes no such field; they match
the curated \[list_canpumf_collection()\] values for most surveys but
are best-effort (Census \`Acronym\` is hard-coded to \`"Census"\`).
Surveys with no downloadable file get a single row with \`url =
"(EFT)"\`.

## Details

The StatCan markup is irregular and this crawler is best-effort: surveys
distributed only by Electronic File Transfer (EFT) report \`url =
"(EFT)"\`, and some products may not be parsed. When an edition is
offered in several formats the one highest in \`prefer\` is kept
(CSV/flat-text first).

## See also

\[list_canpumf_collection()\], \[get_pumf()\]

## Examples

``` r
# \donttest{
# Quick look at the first 5 surveys
head(list_statcan_pumf_catalogue(max_surveys = 5))
#> [1/5] Survey of Household Spending: Public Use Microdata File (62M0004X)
#> [2/5] Canadian Survey on Working Conditions: Public Use Microdata File (14250001)
#> [3/5] Labour Force Survey: Public Use Microdata File (71M0001X)
#> [4/5] General Social Survey – Public Use Microdata Files (45250001)
#> [5/5] Survey on Early Learning and Child Care Arrangements: Public Use Microdata File (42250001)
#> # A tibble: 6 × 9
#>   catalogue_id Acronym SeriesTitle         Title survey_url edition format url  
#>   <chr>        <chr>   <chr>               <chr> <chr>      <chr>   <chr>  <chr>
#> 1 62M0004X     SHS     Survey of Househol… Surv… https://w… 2017    NA     http…
#> 2 62M0004X     SHS     Survey of Househol… Surv… https://w… 2019    NA     http…
#> 3 62M0004X     SHS     Survey of Househol… Surv… https://w… 2021    NA     http…
#> 4 62M0004X     SHS     Survey of Househol… Surv… https://w… 2023    NA     http…
#> 5 14250001     CSWC    Canadian Survey on… Cana… https://w… 2024-2… NA     http…
#> 6 71M0001X     LFS     Labour Force Survey Labo… https://w… 2006    CSV    http…
#> # ℹ 1 more variable: product_url <chr>
# }
```
