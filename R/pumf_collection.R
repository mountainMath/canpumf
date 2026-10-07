# canpumf collection

# Census download rows.  The 98m0001x index page this once scraped is gone
# (404 since 2026); the StatCan catalogue crawl (session cache, persisted copy
# or shipped snapshot) lists every Census download, with the hard-coded list
# as the last resort.
list_census_collection <- function() {
  .census_collection_from_catalogue() %||% .census_collection_fallback()
}

# Census rows from the StatCan catalogue (98M0001X), or NULL if unavailable.
.census_collection_from_catalogue <- function(
    cache_path = getOption("canpumf.cache_path")) {
  cat <- tryCatch(.statcan_catalogue_cached(cache_path), error = function(e) NULL)
  if (is.null(cat) || !nrow(cat)) return(NULL)
  cen <- cat[toupper(cat$catalogue_id) == "98M0001X" &
               grepl("^\\d{4} \\(", cat$edition) & !is.na(cat$url), , drop = FALSE]
  if (!nrow(cen)) return(NULL)
  tibble::tibble(Title = "Census of population",
                 Acronym = "Census",
                 Version = cen$edition,
                 `Survey Number` = "3901",
                 url = cen$url)
}

# Hardcoded GSS/SGVP fallback — only the registry-supported surveys so that
# get_pumf() still works for already-cached data when StatCan is unreachable.
.gss_collection_fallback <- function() {
  tibble::tibble(
    Title          = c(rep("General Social Survey - Caregiving", 5L),
                       rep("General Social Survey - Giving",     8L)),
    Acronym        = c(rep("GSS",  5L), rep("SGVP", 8L)),
    `Survey Number` = c(rep("4502", 5L), rep("4430", 8L)),
    Version        = c("Cycle 11 (1996)", "Cycle 16 (2002)", "Cycle 21 (2007)",
                       "Cycle 26 (2012)", "Cycle 32 (2018)",
                       "1997", "2000", "2004", "2007", "2010", "2013", "2018", "2023"),
    url            = c(
      "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat3/c11_1996.zip",
      # Cycle 16 ("Aging and Social Support", 2002); StatCan files it under the
      # Education category (cat9) and mislabels it "Education 2002".
      "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat9/c16_2002.zip",
      "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat3/c21_2007.zip",
      "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat3/c26_2012.zip",
      "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat3/c32_2018.zip",
      "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat5/NSGVP-ENDBP_1997.zip",
      "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat5/NSGVP-ENDBP_2000.zip",
      "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat5/CSGVP-ECDBP_2004.zip",
      "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat5/CSGVP-ECDBP_2007.zip",
      "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat5/CSGVP-ECDBP_2010.zip",
      "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat5/c27_2013.zip",
      "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat5/c33_2018.zip",
      "https://www150.statcan.gc.ca/n1/pub/45-25-0001/cat5/GVP_DBP_2023.zip"
    )
  )
}

# Hardcoded Census versions used when neither the StatCan index page nor the
# catalogue is available.  Since 2023 StatCan serves every Census PUMF zip from
# 98m0001x/2023001/.
.census_collection_fallback <- function() {
  tibble::tibble(
    Title          = "Census of population",
    Acronym        = "Census",
    `Survey Number` = "3901",
    Version = c(
      "2021 (individuals)", "2021 (hierarchical)",
      "2016 (individuals)", "2016 (hierarchical)",
      "2011 (individuals)", "2011 (hierarchical)",
      "2006 (individuals)", "2006 (hierarchical)",
      "2001 (individuals)", "2001 (households)", "2001 (families)",
      "1996 (individuals)", "1996 (households)", "1996 (families)",
      "1991 (individuals)", "1991 (households)", "1991 (families)"
    ),
    url = c(
      "https://www150.statcan.gc.ca/n1/pub/98m0001x/2023001/cen21_ind_98m0001x_part_rec21.zip",
      "https://www150.statcan.gc.ca/n1/pub/98m0001x/2023001/cen21_hier_98M0001X_rec21_hier.zip",
      "https://www150.statcan.gc.ca/n1/pub/98m0001x/2023001/cen16_ind_98m0001x_part_rec16.zip",
      "https://www150.statcan.gc.ca/n1/pub/98m0001x/2023001/cen16_hier_98m0002x_rec16_hier.zip",
      "https://www150.statcan.gc.ca/n1/pub/98m0001x/2023001/nhs11_ind_99m0001x_part_enm11.zip",
      "https://www150.statcan.gc.ca/n1/pub/98m0001x/2023001/nhs11_hier_99m0002x_enm11_hier.zip",
      "https://www150.statcan.gc.ca/n1/pub/98m0001x/2023001/cen06_ind_95m0028x_part_rec06.zip",
      "https://www150.statcan.gc.ca/n1/pub/98m0001x/2023001/cen06_hier_95m0029x_part_rec06.zip",
      "https://www150.statcan.gc.ca/n1/pub/98m0001x/2023001/cen01_ind_95m0016x_part_rec01.zip",
      "https://www150.statcan.gc.ca/n1/pub/98m0001x/2023001/cen01_hous_95m0020x_mena_rec01.zip",
      "https://www150.statcan.gc.ca/n1/pub/98m0001x/2023001/cen01_fam_95m0018x_fam_rec01.zip",
      "https://www150.statcan.gc.ca/n1/pub/98m0001x/2023001/cen96_ind_95m0010X_part_rec96_v2.zip",
      "https://www150.statcan.gc.ca/n1/pub/98m0001x/2023001/cen96_hous_95m0011x_mena_rec96_v2.zip",
      "https://www150.statcan.gc.ca/n1/pub/98m0001x/2023001/cen96_fam_95m0012x_fam_rec96_v2.zip",
      "https://www150.statcan.gc.ca/n1/pub/98m0001x/2023001/cen91_ind_95m0007x_ind_rec91.zip",
      "https://www150.statcan.gc.ca/n1/pub/98m0001x/2023001/cen91_hous_95m0008X_mena_rec91.zip",
      "https://www150.statcan.gc.ca/n1/pub/98m0001x/2023001/cen91_fam_95m0009x_fam_rec91.zip"
    )
  )
}

# Scrape all GSS PUMF downloads from the shared catalogue index.
# Returns a tibble with Title, Acronym, Survey Number, Version, url.
#
# Version conventions (to match registry keys):
#   GSS cycles               → Acronym="GSS",  Version="Cycle N (YYYY)" derived
#     from the cNN_ cycle prefix in the zip filename (e.g. c34_2019.zip ->
#     "Cycle 34 (2019)"); TU_ET_2022.zip is the cycle-36 exception.
#   cat5 (Giving/SGVP)       → Acronym="SGVP", Version=plain year e.g. "2023"
list_gss_collection <- function() {
  base_url <- "https://www150.statcan.gc.ca/n1/pub/45-25-0001/"
  page     <- rvest::read_html(paste0(base_url, "index-eng.htm"))

  # Static mapping: cat directory -> survey metadata
  cat_meta <- tibble::tribble(
    ~cat,    ~Title,                                       ~Acronym, ~Survey.Number, ~theme_prefix,
    "cat1",  "General Social Survey - Canadian Safety",   "GSS",    "4504",         "Safety",
    "cat2",  "General Social Survey - Work and Home",     "GSS",    "5221",         "Work and Home",
    "cat3",  "General Social Survey - Caregiving",        "GSS",    "4502",         NA_character_,
    "cat4",  "General Social Survey - Family",            "GSS",    "4501",         "Family",
    "cat5",  "General Social Survey - Giving",            "SGVP",   "4430",         NA_character_,
    "cat6",  "General Social Survey - Social Identity",   "GSS",    "5024",         "Social Identity",
    "cat7",  "General Social Survey - Time Use",          "GSS",    "4503",         "Time Use",
    "cat8",  "General Social Survey - ICT",               "GSS",    "4505",         "ICT",
    "cat9",  "General Social Survey - Education",         "GSS",    "4500",         "Education",
    "cat10", "General Social Survey - Health",            "GSS",    "3894",         "Health"
  )

  zip_nodes <- rvest::html_elements(page, 'a[href$=".zip"]')
  tibble::tibble(
    href    = rvest::html_attr(zip_nodes, "href"),
    year    = trimws(rvest::html_text(zip_nodes))
  ) |>
    dplyr::filter(!is.na(.data$href)) |>
    dplyr::mutate(
      url = paste0(base_url, .data$href),
      cat = stringr::str_extract(.data$href, "^cat\\d+")
    ) |>
    dplyr::left_join(cat_meta, by = "cat") |>
    dplyr::mutate(
      # GSS rows take the canonical "Cycle N (YYYY)" key derived from the zip
      # filename (this also resolves StatCan's mis-filing of the cycle-16
      # "Aging and Social Support" PUMF under the Education category -- its
      # c16_2002.zip filename yields "Cycle 16 (2002)" regardless).  The legacy
      # Giving/Volunteering surveys keep their plain-year SGVP keys.
      Version = dplyr::if_else(
        .data$Acronym == "SGVP",
        .data$year,
        vapply(.data$href, .statcan_gss_version, character(1L),
               edition = NA_character_, USE.NAMES = FALSE)
      )
    ) |>
    dplyr::rename(`Survey Number` = "Survey.Number") |>
    dplyr::select("Title", "Acronym", "Survey Number", "Version", "url")
}



#' List the PUMF datasets available to canpumf
#'
#' One entry point for the catalogues canpumf knows: the curated collection it
#' has tested download wrappers for, the live Statistics Canada PUMF listing,
#' the Borealis Dataverse collection, and the Labour Force Survey releases.
#' Each source returns its own columns (below); pass the series and version a
#' row names to [get_pumf()].  All of them need an internet connection, and
#' each degrades gracefully when its site is unreachable.
#'
#' @section `source = "canpumf"`:
#' The series and versions canpumf has download wrappers for.  Census versions
#' are scraped from the StatCan website; the other series are hard-coded.
#' Columns: `Title`, `Acronym`, `Version`, `Survey Number` and `url`.  The
#' `url` column holds the download URL, `"(EFT)"` for versions distributed via
#' the Research Data Centre (EFT only), or the Borealis dataset page for
#' versions canpumf loads from Borealis.  Pass `Acronym` and `Version` to
#' [get_pumf()].
#'
#' @section `source = "statcan"` (experimental):
#' Crawls the live StatCan "Public use microdata" listing and follows each
#' survey to its product page to discover every PUMF series, its editions, and
#' direct-download URLs, including surveys canpumf has not tested.  The StatCan
#' markup is irregular and the crawler is best-effort: surveys distributed only
#' by Electronic File Transfer (EFT) report `url = "(EFT)"`, and some products
#' may not be parsed.  When an edition is offered in several formats the one
#' highest in `prefer` is kept (CSV/flat-text first).
#'
#' A full crawl issues a few hundred requests, so its result is cached: in
#' memory for the session, and (for a full crawl, no `max_surveys`/`surveys`)
#' persisted to `<cache_path>/pumf_catalogue.rds`.  A persisted catalogue older
#' than `getOption("canpumf.catalogue_max_age_days", 30)` days triggers a
#' staleness warning.  If StatCan is unreachable the last persisted copy (or
#' the snapshot shipped with the package) is returned with a warning.
#'
#' Columns, one row per discovered edition: `catalogue_id`, `Acronym`,
#' `SeriesTitle`, `Title`, `survey_url`, `edition`, `format`, `url`, and
#' `product_url`. `SeriesTitle` is the plain-language series name matching the
#' acronym (the catalogue title with the edition-specific tail and "Public Use
#' Microdata File" boilerplate stripped). `Title` is edition-specific:
#' StatCan's own per-edition catalogue title where it carries one, otherwise --
#' for *umbrella* products whose catalogue title is only the series name (e.g.
#' the consolidated General Social Survey, or a census year's
#' individuals/hierarchical pair) -- a synthesised `"<series> -- <edition>"`,
#' where the structural edition descriptor disambiguates colliding years (GSS
#' `"Cycle 16 (2002)"`, census `"2021 (individuals)"`). `edition` remains the
#' reference period/variant. `survey_url` is the survey's catalogue overview
#' page; `url`/`product_url` point at the individual edition's download and
#' product page. `Acronym` and `SeriesTitle` are derived from the title since
#' StatCan exposes no such field; they match the `"canpumf"` values for most
#' surveys but are best-effort (Census `Acronym` is hard-coded to `"Census"`).
#' Surveys with no downloadable file get a single row with `url = "(EFT)"`.
#'
#' @section `source = "borealis"`:
#' The Statistics Canada PUMF datasets held in the
#' [Borealis](https://borealisdata.ca) Dataverse (the ODESI PUMF collection
#' and the Census PUMFs), together with the census microdata Statistics Canada
#' has never published: the historical census samples deposited by ODESI
#' (1871, 1881, 1891, 1901 and the CCRI 1911 sample) and the open
#' complete-count censuses of The Canadian Peoples project (dataverse
#' `TCPCensusData`; 1881 at the time of writing, the other years are
#' restricted and left out). Borealis also carries vintages that Statistics
#' Canada no longer posts, such as the 1971--1986 Census PUMFs. Any dataset
#' listed here can be loaded with
#' `get_pumf(series, version, borealis = <doi or row>)`; see
#' [list_borealis_pumf_files()] to inspect a dataset's files first.
#'
#' Where Statistics Canada also posts a dataset for direct download, the
#' `statcan` column is `TRUE`. Prefer StatCan's copy in that case (via
#' `get_pumf(series, version)` without `borealis =`): the Borealis files are
#' re-deposits and can carry transcription errors. `get_pumf()` warns when an
#' explicitly requested Borealis dataset is flagged this way. The flag is a
#' heuristic match on catalogue number, series title, years and cycle number
#' against the `"statcan"` catalogue, so check `statcan_title` before relying
#' on it.
#'
#' The catalogue is fetched from the public Dataverse search API. There are
#' several thousand datasets and Borealis renders them slowly, so pages are
#' requested concurrently (`getOption("canpumf.borealis_parallel", 8)`), and a
#' full fetch takes about a minute. The result is cached for the session and
#' persisted to `<cache_path>/borealis_catalogue.rds`, with the same staleness
#' warning and offline fallback as the `"statcan"` catalogue.
#'
#' Columns, one row per dataset: `title`, `year` (the first year in the
#' title), `language` (`"eng"`/`"fra"`, guessed from the title), `statcan`
#' (logical, the dataset is also available from Statistics Canada),
#' `statcan_series` and `statcan_title` (the matching StatCan catalogue entry,
#' `NA` when none), `series` (the Borealis series name), `doi`, `dataverse`,
#' `file_count`, `published_at` and `url`. English and French versions of a
#' PUMF are separate datasets.
#'
#' @section `source = "lfs"`:
#' The annual and monthly Labour Force Survey PUMF releases on the StatCan LFS
#' publication page.  Columns: `Date` (the label on the StatCan page),
#' `version` (`"YYYY"` for annual versions, `"YYYY-MM"` for monthly ones) and
#' `url` (direct download link).  If the StatCan website is unreachable the
#' result is an empty tibble with a warning.
#'
#' @param source Which catalogue: `"canpumf"` (default), `"statcan"`,
#'   `"borealis"` or `"lfs"`.
#' @param refresh For `"statcan"` and `"borealis"`: re-fetch the catalogue
#'   instead of using the session or persisted copy.
#' @param verbose For `"statcan"` and `"borealis"`: report crawl progress.
#' @param cache_path For `"statcan"` and `"borealis"`: directory for the
#'   persisted catalogue.  Defaults to `getOption("canpumf.cache_path")`; when
#'   unset only the in-session cache is used.
#' @param ... For `"statcan"` only: `prefer` (format tokens in order of
#'   preference; the default puts CSV / flat text ahead of statistical-package
#'   formats), `max_surveys` (crawl only the first N surveys, for a quick look)
#'   and `surveys` (catalogue ids to restrict the crawl to).
#'
#' @return A tibble; its columns depend on `source` (see the sections above).
#'
#' @seealso [get_pumf()], [list_borealis_pumf_files()], [pumf_registry()]
#'
#' @examples
#' \donttest{
#' collection <- list_pumf_catalogue()
#' # Show all SFS versions
#' collection[collection$Acronym == "SFS", c("Acronym", "Version")]
#'
#' tail(list_pumf_catalogue("lfs"))
#'
#' # Quick look at the first 5 surveys of the live StatCan listing
#' tryCatch(head(list_pumf_catalogue("statcan", max_surveys = 5)),
#'          error = function(e) message(conditionMessage(e)))
#'
#' # needs internet access; fails gracefully when Borealis is unreachable
#' bor <- tryCatch(list_pumf_catalogue("borealis"), error = function(e) NULL)
#' if (!is.null(bor)) dplyr::filter(bor, grepl("1971 Census", title))
#' }
#' @export
list_pumf_catalogue <- function(source = c("canpumf", "statcan", "borealis",
                                           "lfs"),
                                refresh    = FALSE,
                                verbose    = TRUE,
                                cache_path = getOption("canpumf.cache_path"),
                                ...) {
  source <- match.arg(source)
  dots   <- list(...)
  if (length(dots) && source != "statcan")
    stop("Argument(s) ", paste0("'", names(dots), "'", collapse = ", "),
         " apply to source = \"statcan\" only.", call. = FALSE)
  switch(source,
    canpumf  = .canpumf_collection(),
    lfs      = .lfs_pumf_versions(),
    borealis = .borealis_pumf_catalogue(refresh = refresh, verbose = verbose,
                                        cache_path = cache_path),
    statcan  = do.call(.statcan_pumf_catalogue,
                       c(dots, list(verbose = verbose, refresh = refresh,
                                    cache_path = cache_path))))
}


# The curated collection behind list_pumf_catalogue("canpumf"): every series
# and version canpumf has download wrappers for (Census versions scraped,
# the rest hard-coded).
.canpumf_collection <- function(){
  ccahs <- tibble::tibble(Title = "Canadian COVID-19 Antibody and Health Survey",
                  Acronym = "CCAHS",
                  Version=c("1"),
                  `Survey Number`="5339",
                  url="https://www150.statcan.gc.ca/n1/en/pub/13-25-0007/2022001/CCAHS_ECSAC.zip?st=BPMowORM")

  chss <- tibble::tibble(Title = "Canadian Health Survey on Seniors",
                 Acronym = "CHSS",
                 Version = c("2019-2020"),
                 `Survey Number` = "5267",
                 # TXT, not CSV: the CSV bundle ships the data alone, while the
                 # TXT bundle also carries the SPSS layout cards (see the
                 # CHSS/2019-2020 registry entry's download_format).
                 url = "https://www150.statcan.gc.ca/n1/pub/13-25-0010/2024001/2019-2020_TXT.zip")

  # Both PALS editions hang off the single 2009001 publication page of catalogue
  # 82M0023X (the 2004001 edition page is gone), so the pair is curated here
  # rather than left to the crawl, which has no edition token to tell them apart.
  pals <- tibble::tibble(Title = "Participation and Activity Limitation Survey",
                 Acronym = "PALS",
                 Version = c("2001", "2006"),
                 `Survey Number` = "3251",
                 url = paste0("https://www150.statcan.gc.ca/n1/pub/82m0023x/",
                              "2009001/PALS_EPLA_", c("2001", "2006"), ".zip"))

  cpss <- tibble::tibble(Title="Canadian Perspectives Survey Series",
                 Acronym="CPSS",
                 Version=c("1","2","3","4","5","6"),
                 `Survey Number`="5311",
                 url=c("https://www150.statcan.gc.ca/n1/en/pub/45-25-0002/2020001/CSV.zip",
                       "https://www150.statcan.gc.ca/n1/en/pub/45-25-0004/2020001/CSV.zip",
                       "https://www150.statcan.gc.ca/n1/en/pub/45-25-0007/2020001/CSV.zip",
                       "https://www150.statcan.gc.ca/n1/en/pub/45-25-0009/2020001/CSV.zip",
                       "https://www150.statcan.gc.ca/n1/pub/45-25-0010/2021001/CSV-eng.zip",
                       "https://www150.statcan.gc.ca/n1/en/pub/45-25-0012/2021001/CSV.zip"))
  chs <- tibble::tibble(Title="Canadian Housing Survey",
                Acronym="CHS",
                Version=c("2018","2021","2022"),
                `Survey Number`="5269",
                url=c("https://www150.statcan.gc.ca/n1/en/pub/46-25-0001/2021001/2018.zip",
                      "https://www150.statcan.gc.ca/n1/en/pub/46-25-0001/2021001/2021.zip",
                      "https://www150.statcan.gc.ca/n1/en/pub/46-25-0001/2021001/2022.zip"))

  shs <- tibble::tibble(Title="Survey of Household Spending",
                Acronym="SHS",
                Version=c("2017","2019","2021","2023"),
                `Survey Number`="3508",
                url=c("https://www150.statcan.gc.ca/n1/en/pub/62m0004x/2017001/SHS_EDM_2017-eng.zip",
                      "https://www150.statcan.gc.ca/n1/en/pub/62m0004x/2017001/SHS_EDM_2019.zip",
                      "https://www150.statcan.gc.ca/n1/pub/62m0004x/2017001/SHS_EDM_2021.zip",
                      "https://www150.statcan.gc.ca/n1/pub/62m0004x/2017001/SHS_EDM_2023.zip"))

  its_versions <- tibble::tibble(Acronym="ITS",
                         Version=c("2019","2018"),
                         url=c("https://www150.statcan.gc.ca/n1/pub/24-25-0002/2021001/2019/SPSS.zip",
                               "https://www150.statcan.gc.ca/n1/pub/24-25-0002/2021001/2018/SPSS.zip"))

  # Version from the download URL ("..._2024-01-CSV.zip" -> "2024-01",
  # "..._2024-CSV.zip" -> "2024").
  lfs_links <- .lfs_scrape_csv_links()
  lfs_versions <- if (is.null(lfs_links)) {
    tibble::tibble(Acronym = character(), url = character(), Version = character())
  } else {
    tibble::tibble(Acronym = "LFS", url = lfs_links$url) |>
      mutate(Version = stringr::str_match(.data$url, "\\d{4}-\\d{2}")[, 1L]) |>
      mutate(Version = coalesce(.data$Version,
                                stringr::str_match(.data$url, "(\\d{4})-CSV")[, 2L]))
  }

  # Pre-2006 LFS PUMF monthly releases are EFT-only.  Scrape the catalogue
  # index to discover which year/month combinations StatCan has published.
  # Catalogue entries follow the pattern 71M0001X{YYYY}{MMM} (3-digit month).
  lfs_eft_versions <- tryCatch({
    cat_page <- rvest::read_html("https://www150.statcan.gc.ca/n1/en/catalogue/71M0001X")
    hrefs    <- rvest::html_attr(rvest::html_elements(cat_page, "a"), "href")
    m        <- stringr::str_match(hrefs, "71M0001X(\\d{4})(\\d{3})$")
    m        <- m[!is.na(m[, 1L]), , drop = FALSE]
    year     <- as.integer(m[, 2L])
    month    <- as.integer(m[, 3L])
    eft      <- year < 2006L & month >= 1L & month <= 12L
    tibble::tibble(
      Acronym = "LFS",
      Version = paste0(m[eft, 2L], "-",
                        formatC(month[eft], width = 2L, flag = "0")),
      url     = "(EFT)"
    ) |> distinct()
  }, error = function(e) {
    tibble::tibble(Acronym = character(0L), Version = character(0L),
                   url     = character(0L))
  })
  lfs_versions <- bind_rows(lfs_versions, lfs_eft_versions)

  sfs_versions <- tibble::tibble(Acronym="SFS",
                         Version=c("1999","2005","2012","2016","2019","2023"),
                         url=c("https://www150.statcan.gc.ca/n1/pub/13m0006x/2021001/SFS1999-eng.zip",
                               "https://www150.statcan.gc.ca/n1/pub/13m0006x/2021001/SFS2005-eng.zip",
                               "https://www150.statcan.gc.ca/n1/pub/13m0006x/2021001/SFS2012-eng.zip",
                               "https://www150.statcan.gc.ca/n1/pub/13m0006x/2021001/SFS2016-eng.zip",
                               "https://www150.statcan.gc.ca/n1/pub/13m0006x/2021001/SFS2019__PUMF_E.zip",
                               "https://www150.statcan.gc.ca/n1/pub/13m0006x/2021001/SFS2023-eng.zip"))
  cis_versions <- tibble::tibble(Acronym="CIS",
                         Version=c("2022", "2021", "2020", "2019", "2018", "2017"),
                         url=c("https://www150.statcan.gc.ca/n1/pub/72m0003x/2024001/2022.zip",
                               "https://www150.statcan.gc.ca/n1/en/pub/72m0003x/2024001/2021.zip",
                               "https://www150.statcan.gc.ca/n1/en/pub/72m0003x/2023002/2020.zip",
                               "https://www150.statcan.gc.ca/n1/en/pub/72m0003x/2021001/2019.zip",
                               "https://www150.statcan.gc.ca/n1/en/pub/72m0003x/2021001/2018-eng.zip",
                               "https://www150.statcan.gc.ca/n1/en/pub/72m0003x/2019001/2017-eng.zip"))

  gss_all <- tryCatch(
    list_gss_collection(),
    error = function(e) .gss_collection_fallback()
  )

  census_download   <- list_census_collection()
  scrape_failed_lfs <- nrow(lfs_versions) == 0L
  scrape_failed_cen <- identical(census_download, .census_collection_fallback())
  scrape_failed_gss <- identical(gss_all, .gss_collection_fallback())

  if (scrape_failed_lfs || scrape_failed_cen || scrape_failed_gss) {
    what <- c(if (scrape_failed_cen) "Census",
              if (scrape_failed_lfs) "LFS",
              if (scrape_failed_gss) "GSS/SGVP")
    warning("Statistics Canada website unreachable; ",
            paste(what, collapse = " and "),
            " version list(s) are hard-coded and may be incomplete.",
            call. = FALSE)
  }

  first_year <- census_download$Version |> str_extract("\\d{4}") |> as.integer()
  first_year <- if (any(!is.na(first_year))) min(first_year, na.rm = TRUE) else 1991L
  last_eft_year <- first_year - 5

  census_eft <- function(versions)
    tibble::tibble(Title = "Census of population", Acronym = "Census",
                   `Survey Number` = "3901", Version = versions, url = "(EFT)")

  bind_rows(chs, cpss, shs, ccahs, chss, pals) |>
    bind_rows(lfs_versions |> mutate(Title = "Labour Force Survey", `Survey Number` = "3701"),
              its_versions |> mutate(Title = "International Travel Survey", `Survey Number` = "3152"),
              sfs_versions |> mutate(Title = "Survey of Financial Securities", `Survey Number` = "2620"),
              cis_versions |> mutate(Title = "Canadian Income Survey", `Survey Number` = "5200"),
              gss_all,
              census_eft(paste0(seq(1971, last_eft_year, 5), " (individuals)")),
              census_eft(paste0(seq(1971, last_eft_year, 5), " (households)")),
              census_eft(paste0(c(1971L, 1976L, seq(1986L, pmin(1996L, last_eft_year), 5L)),
                                " (families)"))) |>
    bind_rows(census_download, .borealis_registry_collection())
}

# Collection rows for registry entries sourced from Borealis (the 1971-1986
# Census PUMFs, the TCP complete-count census of 1881 and the CCRI census
# samples); `url` is the Borealis dataset page.
.borealis_registry_collection <- function() {
  keys <- .pumf_registry_borealis_keys()
  if (length(keys) == 0L) return(NULL)
  ents <- .pumf_registry[keys]
  series <- vapply(ents, function(e) e$series, character(1L))
  tibble::tibble(
    Title           = dplyr::case_when(
      series == "Census" ~ "Census of population",
      series == "TCP"    ~ "The Canadian Peoples complete-count census",
      series == "CCRI"   ~ "Canadian Century Research Infrastructure census sample",
      TRUE               ~ series),
    Acronym         = series,
    Version         = vapply(ents, function(e) e$version, character(1L)),
    `Survey Number` = ifelse(series == "Census", "3901", NA_character_),
    url             = vapply(ents, function(e)
      .borealis_dataset_url(.borealis_entry_doi(e)), character(1L)))
}


# The LFS versions StatCan posts, behind list_pumf_catalogue("lfs"): Date,
# version ("YYYY" or "YYYY-MM") and url; an empty tibble with a warning
# when StatCan is unreachable.
.lfs_pumf_versions <- function(){
  empty <- tibble::tibble(Date = character(0L), version = character(0L),
                          url = character(0L))

  # Fail gracefully when StatCan is unreachable: return whatever was scraped
  # (an empty tibble if nothing) with a warning, rather than erroring -- mirrors
  # the other list_pumf_catalogue() sources.
  links <- .lfs_scrape_csv_links()
  if (is.null(links)) {
    warning("Statistics Canada website unreachable; no LFS PUMF versions ",
            "could be retrieved.", call. = FALSE)
    return(empty)
  }
  if (nrow(links) == 0L) return(empty)

  lct <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  on.exit(Sys.setlocale("LC_TIME", lct), add = TRUE)

  # Version from the link's title ("January 2024 | PUMF: CSV" -> "2024-01",
  # "2024 | PUMF: CSV" -> "2024").
  tibble::tibble(
    url  = links$url,
    Date = gsub(" \\| PUMF: CSV","", links$title)) |>
    mutate(version = case_when(
      grepl("^\\d{4}$",.data$Date) ~ .data$Date,
      TRUE ~ strftime(as.Date(paste0("01 ",.data$Date),format="%d %B %Y"),"%Y-%m"))) |>
    select("Date", "version", "url")
}

# The "CSV" download links of the LFS PUMF publication page, as a tibble with
# `url` (absolute) and `title` (the anchor's title attribute), or NULL when
# StatCan is unreachable.  .canpumf_collection() derives the version from
# the URL and .lfs_pumf_versions() from the title; the two
# agree for the links StatCan posts, and each keeps its own derivation.
.lfs_pumf_page_base <- "https://www150.statcan.gc.ca/n1/pub/71m0001x/"
.lfs_scrape_csv_links <- function() {
  a <- tryCatch(
    rvest::read_html(paste0(.lfs_pumf_page_base, "71m0001x2021001-eng.htm")) |>
      rvest::html_elements("a"),
    error = function(e) NULL)
  if (is.null(a)) return(NULL)
  a <- a[rvest::html_text(a) == "CSV"]
  tibble::tibble(
    url   = .statcan_abs_url(rvest::html_attr(a, "href"), base = .lfs_pumf_page_base),
    title = rvest::html_attr(a, "title"))
}
