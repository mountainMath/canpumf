# Borealis Dataverse as a second PUMF source.
#
# Borealis (https://borealisdata.ca), the Canadian research-data Dataverse,
# hosts the ODESI collection of Statistics Canada PUMFs: the "pumfs" dataverse
# (~3400 datasets, English and French) plus the Census PUMFs in the "census"
# dataverse.  It carries vintages StatCan no longer posts publicly -- notably
# the 1971-1986 Census PUMFs, which StatCan only distributes via EFT.
#
# StatCan stays the primary source.  Borealis is used when
#   * a registry entry carries a `borealis` DOI and StatCan has no download
#     for that version (the old Census vintages), or
#   * the user asks for it explicitly: get_pumf(..., borealis = "doi:...").
#
# Everything here uses the public Dataverse API and needs no account.  When the
# BOREALIS_DATAVERSE_KEY environment variable is set (as in canivt), it is sent
# as the X-Dataverse-key header, which also gives access to restricted files
# the key's owner is entitled to.
#
# A Borealis dataset is a loose collection of files (data in several formats,
# command files, documentation), not a single zip.  .borealis_select_files()
# picks one readable data file plus the command files canpumf's parsers
# understand, and .borealis_download_dataset() lays them out flat in the
# version directory with a borealis_manifest.csv recording the provenance.  The
# manifest also tells Stage 3 which file is the data file, since ODESI
# datasets ship FWF copies and plain-text codebooks that would otherwise be
# candidates.

BOREALIS_SERVER <- "https://borealisdata.ca"
.borealis_user_agent <- "canpumf (https://github.com/mountainMath/canpumf)"

# Dataverse subtrees holding census and survey microdata.  Everything under
# "pumfs" is a PUMF.  The "census" dataverse also holds aggregate products
# (profiles, GIS layers, tables), so a dataset is kept from it only when its
# title names a PUMF/FMGD or it ships an SPSS data file, which the historical
# samples StatCan never published do (the ODESI 1871-1901 census samples and
# the CCRI 1911 sample, titled "Census of Population, 1911 [Canada]").
# "TCPCensusData" is The Canadian Peoples project's complete-count censuses
# 1871-1921; only the datasets whose data file is open are listed (1881, the
# others are restricted and cannot be tested).
.borealis_subtrees <- c("pumfs", "census", "TCPCensusData")
.borealis_census_pumf_rx <- "(?i)(PUMF|FMGD|microdata|microdonn)"
.borealis_microdata_file_fq <- "fileType:\"SPSS Binary\""

.borealis_manifest_file <- "borealis_manifest.csv"

# Session cache for the catalogue.
.borealis_catalogue_cache <- new.env(parent = emptyenv())

# The API key, or NULL.  Only ever sent to BOREALIS_SERVER.
.borealis_token <- function() {
  key <- Sys.getenv("BOREALIS_DATAVERSE_KEY", "")
  if (nzchar(key)) key else NULL
}

.borealis_headers <- function() {
  key <- .borealis_token()
  if (is.null(key)) httr::add_headers() else httr::add_headers(`X-Dataverse-key` = key)
}

# Normalise a DOI given as "doi:10.5683/SP3/XXXX", "10.5683/SP3/XXXX", or a
# doi.org / Borealis dataset URL to the "doi:10.5683/..." form the API expects.
.borealis_normalize_doi <- function(doi) {
  if (!is.character(doi) || length(doi) != 1L || is.na(doi) || !nzchar(doi))
    stop("A Borealis DOI must be a single string such as ",
         "\"doi:10.5683/SP3/LG7WKC\".", call. = FALSE)
  d <- trimws(doi)
  d <- sub("^.*persistentId=", "", d)
  d <- utils::URLdecode(d)
  d <- sub("^https?://(dx\\.)?doi\\.org/", "", d, ignore.case = TRUE)
  d <- sub("^doi:", "", d, ignore.case = TRUE)
  if (!grepl("^10\\.[0-9]+/", d))
    stop("'", doi, "' does not look like a DOI (expected e.g. ",
         "\"doi:10.5683/SP3/LG7WKC\").", call. = FALSE)
  paste0("doi:", d)
}

.borealis_dataset_url <- function(doi)
  paste0(BOREALIS_SERVER, "/dataset.xhtml?persistentId=", doi)

# GET a Borealis API path and return the parsed JSON `data` element.  Any
# failure (unreachable host, HTTP error, API error status) is raised as a
# canpumf_network_error so get_pumf() degrades gracefully.
.borealis_api <- function(path, query = list()) {
  url  <- paste0(BOREALIS_SERVER, path)
  resp <- tryCatch(
    httr::GET(url, query = query, .borealis_headers(),
              httr::user_agent(.borealis_user_agent),
              httr::timeout(120)),
    error = function(e) e)
  fail <- function(detail)
    stop(.pumf_network_error(paste0(
      "Borealis is unreachable or returned an error for '", url, "': ", detail,
      ". Try again later.")))
  if (inherits(resp, "error")) fail(conditionMessage(resp))
  if (httr::status_code(resp) == 404L)
    stop("Borealis has no dataset or file at '", url, "' (HTTP 404). ",
         "Check the DOI.", call. = FALSE)
  if (httr::http_error(resp)) fail(paste0("HTTP ", httr::status_code(resp)))
  body <- tryCatch(httr::content(resp, as = "parsed", type = "application/json",
                                 encoding = "UTF-8"),
                   error = function(e) NULL)
  if (!is.list(body) || !identical(body$status, "OK"))
    fail(if (is.list(body) && !is.null(body$message)) body$message
         else "unexpected response")
  body$data
}

# ---- Catalogue --------------------------------------------------------------

# Year and language heuristics from a dataset title.
.borealis_title_year <- function(title) {
  y <- stringr::str_extract(title, "(?<![0-9])(1[89]|20)[0-9]{2}(?![0-9])")
  as.integer(y)
}

.borealis_title_lang <- function(title) {
  fra <- grepl(paste0("(?i)(Enqu[e\u00ea]te|Recensement|FMGD|microdonn|",
                      "Fichier|Programme|[E\u00c9]tude|Sondage|\u00e0 grande diffusion)"),
               title, perl = TRUE)
  ifelse(fra, "fra", "eng")
}

# Search-API query for one page of a subtree.  Sorting by name makes the paging
# stable, so pages can be fetched concurrently.  `metadata_fields` adds the
# citation fields used to match a dataset to the StatCan catalogue.
.borealis_search_query <- function(subtree, start, per_page) {
  list(q = "*", type = "dataset", subtree = subtree, sort = "name",
       order = "asc", per_page = per_page, start = start,
       metadata_fields = "citation:otherId",
       metadata_fields = "citation:series",
       metadata_fields = "citation:alternativeTitle")
}

# Fetch several search pages concurrently.  Borealis renders each result
# server-side at roughly 0.07 s per dataset whatever the page size, so the
# ~3700 PUMF datasets take four minutes sequentially.  Fetching pages of 100
# with a few requests in flight brings that down to well under a minute.
# HTTP/2 multiplexing is off: Borealis answers multiplexed streams on one
# connection one after another, so only separate connections run in parallel.
# Returns the list of `data` elements, one per start offset; a page that fails
# is retried sequentially through .borealis_api(), which raises a
# canpumf_network_error.
.borealis_search_pages <- function(subtree, starts, per_page,
                                   max_parallel = getOption("canpumf.borealis_parallel", 8L)) {
  urls <- vapply(starts, function(st)
    httr::modify_url(paste0(BOREALIS_SERVER, "/api/search"),
                     query = .borealis_search_query(subtree, st, per_page)),
    character(1L))
  bodies <- vector("list", length(urls))
  pool <- curl::new_pool(total_con = max_parallel, host_con = max_parallel,
                         multiplex = FALSE)
  key  <- .borealis_token()
  # Requests are added as slots free up rather than all at once: curl starts a
  # request's connect timeout when it is queued, so a page waiting behind
  # slow ones would otherwise time out.
  nxt <- 0L
  submit <- function() {
    if (nxt >= length(urls)) return(invisible())
    nxt <<- nxt + 1L
    j <- nxt
    h <- curl::new_handle(timeout = 120, useragent = .borealis_user_agent)
    if (!is.null(key)) curl::handle_setheaders(h, `X-Dataverse-key` = key)
    curl::curl_fetch_multi(urls[[j]], handle = h, pool = pool,
      done = function(res) {
        if (res$status_code == 200L) {
          body <- tryCatch(jsonlite::fromJSON(rawToChar(res$content),
                                              simplifyVector = FALSE),
                           error = function(e) NULL)
          if (is.list(body) && identical(body$status, "OK"))
            bodies[[j]] <<- body$data
        }
        submit()
      },
      fail = function(msg) submit())
  }
  for (k in seq_len(min(max_parallel, length(urls)))) submit()
  curl::multi_run(pool = pool)
  for (i in which(vapply(bodies, is.null, logical(1L))))
    bodies[[i]] <- .borealis_api("/api/search",
                                 .borealis_search_query(subtree, starts[[i]], per_page))
  bodies
}

# Value of a citation field in the `metadataBlocks` of a search item or a
# dataset version, or NULL.
.borealis_citation_field <- function(x, field) {
  for (f in x$metadataBlocks$citation$fields)
    if (identical(f$typeName, field)) return(f$value)
  NULL
}

.borealis_search_subtree <- function(subtree, verbose = TRUE, per_page = 100L) {
  first <- .borealis_api("/api/search", .borealis_search_query(subtree, 0L, per_page))
  total <- as.integer(first$total_count)
  if (verbose)
    message("Borealis '", subtree, "': fetching ", total, " datasets")
  starts <- if (total > per_page) seq(per_page, total - 1L, by = per_page) else integer()
  rest   <- if (length(starts)) .borealis_search_pages(subtree, starts, per_page) else list()
  items  <- unlist(lapply(c(list(first), rest), `[[`, "items"), recursive = FALSE)

  other_id <- function(i) {
    v <- .borealis_citation_field(i, "otherId")
    if (is.null(v)) return(NA_character_)
    paste(vapply(v, function(o) .chr1(o$otherIdValue$value), character(1L)),
          collapse = " ")
  }
  series_name <- function(i) {
    v <- .borealis_citation_field(i, "series")
    if (is.null(v)) NA_character_ else .chr1(v[[1L]]$seriesName$value)
  }
  alt_titles <- function(i) {
    v <- .borealis_citation_field(i, "alternativeTitle")
    if (is.null(v)) NA_character_ else paste(unlist(v), collapse = " | ")
  }
  tibble::tibble(
    doi            = vapply(items, function(i) .chr1(i$global_id), character(1L)),
    title          = vapply(items, function(i) .chr1(i$name), character(1L)),
    series         = vapply(items, series_name, character(1L)),
    other_id       = vapply(items, other_id, character(1L)),
    alt_title      = vapply(items, alt_titles, character(1L)),
    dataverse      = vapply(items, function(i) .chr1(i$identifier_of_dataverse), character(1L)),
    published_at   = vapply(items, function(i) .chr1(i$published_at), character(1L)),
    file_count     = vapply(items, function(i)
                       if (is.null(i$fileCount)) NA_integer_ else as.integer(i$fileCount),
                       integer(1L)))
}

# The files of a subtree, one row per file: `doi` of the dataset, `name`,
# `file_type` (Dataverse's friendly type), `restricted` and `size`.  `fq`
# is a Solr filter on the file fields; the SPSS filter above narrows the
# ~8000 files of "census" to the 60-odd microdata deposits, so the listing
# fits in one page.  Pages of 1000 are fetched sequentially (a few at most).
.borealis_subtree_files <- function(subtree, fq = NULL, per_page = 1000L) {
  query <- function(start) {
    q <- list(q = "*", type = "file", subtree = subtree, sort = "name",
              order = "asc", per_page = per_page, start = start)
    if (!is.null(fq)) q$fq <- fq
    q
  }
  first <- .borealis_api("/api/search", query(0L))
  total <- as.integer(first$total_count)
  pages <- list(first)
  for (st in if (total > per_page) seq(per_page, total - 1L, by = per_page) else integer())
    pages <- c(pages, list(.borealis_api("/api/search", query(st))))
  items <- unlist(lapply(pages, `[[`, "items"), recursive = FALSE)
  tibble::tibble(
    doi        = vapply(items, function(i) .chr1(i$dataset_persistent_id), character(1L)),
    name       = vapply(items, function(i) .chr1(i$name), character(1L)),
    file_type  = vapply(items, function(i) .chr1(i$file_type), character(1L)),
    restricted = vapply(items, function(i) isTRUE(i$restricted), logical(1L)),
    size       = vapply(items, function(i)
                   if (is.null(i$size_in_bytes)) NA_real_ else as.numeric(i$size_in_bytes),
                   numeric(1L)))
}

# The datasets of the "census" subtree that are microdata: PUMF titles, plus
# any dataset with an SPSS data file (the historical census samples).
.borealis_census_microdata <- function(census) {
  by_title <- grepl(.borealis_census_pumf_rx, census$title, perl = TRUE)
  spss     <- .borealis_subtree_files("census", fq = .borealis_microdata_file_fq)
  census[by_title | census$doi %in% spss$doi, , drop = FALSE]
}

# The TCP datasets with an open data file.  The others need a Borealis key
# entitled to them (the ODESI deposits are all open).
.borealis_tcp_open <- function(tcp) {
  files <- .borealis_subtree_files("TCPCensusData")
  data  <- files[grepl("\\.(csv|zip|txt|dat|tab)$", files$name, ignore.case = TRUE), ]
  tcp[tcp$doi %in% data$doi[!data$restricted], , drop = FALSE]
}

# ---- Matching against the StatCan catalogue ------------------------------------

# Normalise a survey title for series matching: lower case, ASCII letters and
# digits only, with StatCan's product-type suffixes removed.
.borealis_norm_series <- function(x) {
  x <- tolower(x)
  x <- gsub("\\([^)]*\\)?", " ", x)
  x <- sub(paste0("(,? *(public use )?microdata( file)?| public use microdata file",
                  "|, documentation and data files| pumf).*$"), "", x)
  x <- gsub("[^a-z0-9]+", " ", x)
  x <- sub("^((19|20)[0-9]{2} )?(the )?", "", trimws(x))
  x <- sub("^census of canada", "census of population", x)
  trimws(x)
}

# Years a StatCan catalogue row covers, from its edition and title: every
# four-digit year, with "2024-2025" style ranges filled in.
.borealis_edition_years <- function(edition, title) {
  txt <- gsub("\\[[^]]*\\]", " ", paste(edition, title))
  rng <- stringr::str_match_all(txt, "((?:19|20)[0-9]{2})\\s*[-\u2013/]\\s*((?:19|20)[0-9]{2})")[[1L]]
  yrs <- as.integer(stringr::str_extract_all(txt, "(?<![0-9])(19|20)[0-9]{2}(?![0-9])")[[1L]])
  if (nrow(rng))
    for (k in seq_len(nrow(rng)))
      yrs <- c(yrs, seq(as.integer(rng[k, 2L]), as.integer(rng[k, 3L])))
  unique(yrs)
}

# Mark the Borealis datasets Statistics Canada also offers as a direct
# download.  A dataset matches a StatCan catalogue row when the row's
# catalogue number appears in the dataset's ODESI identifier, or its series
# title starts the dataset's title or one of its alternative titles (French
# datasets carry their English title there), the dataset covers the same
# years as the row, and any cycle or series number the two titles both give
# agrees.  Requiring the same years keeps a component file apart from the
# annual release: CCHS 2015 Nutrition is not the 2015-2016 annual file.  EFT-only rows are not direct downloads.  Adds `statcan` (TRUE
# when matched) and `statcan_series`/`statcan_title`, the matched row's
# acronym and title.
.borealis_match_statcan <- function(bor, statcan) {
  bor$statcan        <- FALSE
  bor$statcan_series <- NA_character_
  bor$statcan_title  <- NA_character_
  if (is.null(statcan) || !nrow(statcan)) return(bor)
  sc <- statcan[grepl("^https?://", statcan$url), , drop = FALSE]
  if (!nrow(sc)) return(bor)
  sc$norm  <- .borealis_norm_series(sc$SeriesTitle)
  sc$years <- Map(.borealis_edition_years, sc$edition, sc$Title)
  sc$catno <- toupper(gsub("[^A-Za-z0-9]", "", sc$catalogue_id))
  # Longest series names first, so "canadian internet use survey household ..."
  # wins over a shorter prefix.
  sc <- sc[order(-nchar(sc$norm)), , drop = FALSE]

  titles <- lapply(seq_len(nrow(bor)), function(i) {
    alt <- if (is.na(bor$alt_title[[i]])) character()
           else strsplit(bor$alt_title[[i]], " | ", fixed = TRUE)[[1L]]
    .borealis_norm_series(c(bor$title[[i]], alt))
  })
  ids  <- toupper(gsub("[^A-Za-z0-9]", "", ifelse(is.na(bor$other_id), "", bor$other_id)))
  cycle_no <- function(x)
    stringr::str_match(x, "\\b(?:cycle|series) ([0-9]+(?: [0-9])?)\\b")[, 2L]
  sc$cycle <- cycle_no(.borealis_norm_series(sc$Title))
  has_catno <- nzchar(sc$catno) & grepl("[0-9]", sc$catno)
  for (i in seq_len(nrow(bor))) {
    y <- sort(.borealis_edition_years("", bor$title[[i]]))
    if (!length(y)) next
    cyc <- cycle_no(titles[[i]][[1L]])
    by_cat    <- has_catno &
                   vapply(sc$catno, function(k) grepl(k, ids[[i]], fixed = TRUE), logical(1L))
    by_series <- nzchar(sc$norm) &
                   vapply(sc$norm, function(n)
                     any(titles[[i]] == n | startsWith(titles[[i]], paste0(n, " "))),
                     logical(1L))
    same_cycle <- !is.na(sc$cycle) & !is.na(cyc) & sc$cycle == cyc
    same_years <- vapply(sc$years, function(yy)
      if (length(yy)) identical(sort(yy), y) else NA, logical(1L))
    ok  <- (by_cat | by_series) &
             ifelse(is.na(same_years), same_cycle, same_years) &
             (is.na(sc$cycle) | is.na(cyc) | same_cycle)
    # A row naming the same cycle beats one that names none (GSS Cycle 18
    # over the 2004 CSGVP, both "General Social Survey" in 2004).
    hit <- which(ok)[order(!same_cycle[ok])]
    if (!length(hit)) next
    bor$statcan[[i]]        <- TRUE
    bor$statcan_series[[i]] <- sc$Acronym[[hit[[1L]]]]
    bor$statcan_title[[i]]  <- sc$Title[[hit[[1L]]]]
  }
  bor
}

.borealis_crawl_catalogue <- function(verbose = TRUE) {
  parts <- lapply(.borealis_subtrees, .borealis_search_subtree, verbose = verbose)
  names(parts) <- .borealis_subtrees
  parts$census        <- .borealis_census_microdata(parts$census)
  parts$TCPCensusData <- .borealis_tcp_open(parts$TCPCensusData)
  out <- dplyr::bind_rows(parts)
  out <- out[!duplicated(out$doi), , drop = FALSE]
  out$year         <- .borealis_title_year(out$title)
  out$language     <- .borealis_title_lang(out$title)
  out$published_at <- as.Date(substr(out$published_at, 1L, 10L))
  out$url          <- .borealis_dataset_url(out$doi)
  out <- .borealis_match_statcan(out, .statcan_catalogue_cached())
  out <- out[order(out$title), c("title", "year", "language", "statcan",
                                 "statcan_series", "statcan_title", "series",
                                 "doi", "dataverse", "file_count",
                                 "published_at", "url")]
  tibble::as_tibble(out)
}

# Persisted catalogue: the same rds format and helpers as the StatCan one
# (.pumf_rds_read() etc. in statcan_catalogue.R).
.borealis_catalogue_cache_file <- function(
    cache_path = getOption("canpumf.cache_path")) {
  .pumf_rds_cache_file(cache_path, "borealis_catalogue.rds")
}

.borealis_warn_if_stale <- function(fetched)
  .pumf_warn_if_stale(fetched, "Borealis PUMF", "borealis")

# The Borealis catalogue behind list_pumf_catalogue("borealis"), cached for
# the session and persisted to <cache_path>/borealis_catalogue.rds.
.borealis_pumf_catalogue <- function(refresh    = FALSE,
                                     verbose    = TRUE,
                                     cache_path = getOption("canpumf.cache_path")) {
  if (!refresh && !is.null(.borealis_catalogue_cache$data))
    return(.borealis_catalogue_cache$data)

  cache_file <- .borealis_catalogue_cache_file(cache_path)
  if (!refresh) {
    cached <- .pumf_rds_read(cache_file)
    if (!is.null(cached)) {
      .borealis_warn_if_stale(cached$fetched)
      .borealis_catalogue_cache$data <- cached$data
      return(cached$data)
    }
  }

  out <- tryCatch(
    .borealis_crawl_catalogue(verbose = verbose),
    error = function(e) {
      cached <- .pumf_rds_read(cache_file)
      if (is.null(cached)) stop(e)
      warning("Borealis unreachable; returning the last cached catalogue ",
              "(fetched ", format(cached$fetched, "%Y-%m-%d"), "). ",
              conditionMessage(e), call. = FALSE)
      cached$data
    })

  .borealis_catalogue_cache$data <- out
  .pumf_rds_write(cache_file, out, prefer = NULL)
  out
}

# Warn when an explicitly requested Borealis dataset is one Statistics Canada
# also offers as a direct download.  StatCan's own copy is preferred: the
# Borealis copies are re-deposits that can carry transcription errors (the
# 2006 LFS labels arrive as mojibake, for example).  Only an already-fetched
# catalogue is consulted -- the session cache or the persisted copy -- so the
# check never triggers a catalogue download.
.borealis_warn_statcan_available <- function(doi,
                                             cache_path = getOption("canpumf.cache_path")) {
  cat <- .borealis_catalogue_cache$data
  if (is.null(cat))
    cat <- .pumf_rds_read(.borealis_catalogue_cache_file(cache_path))$data
  if (is.null(cat) || !"statcan" %in% names(cat)) return(invisible(FALSE))
  row <- cat[cat$doi == doi & cat$statcan %in% TRUE, , drop = FALSE]
  if (!nrow(row)) return(invisible(FALSE))
  warning(doi, " (", row$title[[1L]], ") is also available directly from ",
          "Statistics Canada as \"", row$statcan_title[[1L]], "\". StatCan's ",
          "copy is preferred; see list_pumf_catalogue() for the matching ",
          row$statcan_series[[1L]], " version.", call. = FALSE)
  invisible(TRUE)
}

# ---- Files ------------------------------------------------------------------

# Raw file listing of a dataset's latest version.
.borealis_dataset_files <- function(doi) {
  doi <- .borealis_normalize_doi(doi)
  d   <- .borealis_api("/api/datasets/:persistentId/", list(persistentId = doi))
  v   <- d$latestVersion
  title <- .chr1(.borealis_citation_field(v, "title"))
  files <- v$files
  out <- tibble::tibble(
    file_id      = vapply(files, function(f) as.integer(f$dataFile$id), integer(1L)),
    filename     = vapply(files, function(f) .chr1(f$dataFile$filename), character(1L)),
    directory    = vapply(files, function(f) .chr1(f$directoryLabel), character(1L)),
    size         = vapply(files, function(f)
                     as.numeric(f$dataFile$filesize %||% NA_real_), numeric(1L)),
    md5          = vapply(files, function(f) .chr1(f$dataFile$md5), character(1L)),
    content_type = vapply(files, function(f) .chr1(f$dataFile$contentType), character(1L)),
    original     = vapply(files, function(f) .chr1(f$dataFile$originalFileName), character(1L)),
    restricted   = vapply(files, function(f) isTRUE(f$restricted), logical(1L)))
  attr(out, "doi")   <- doi
  attr(out, "title") <- title
  out
}

# Classify each file and pick the subset canpumf downloads.
#
#   data     -- one data file: CSV (or a zip whose name says csv) first, then a
#               fixed-width .txt/.dat (or an ascii/raw/txt zip) with a matching
#               command file, then .sas7bdat.
#   metadata -- SPSS .sps command files; else a documentation .sas; else the
#               .sav (embedded labels); else a converter .sas from the SAS
#               data folder.  Also a "*value_labels.json" dictionary.
#   doc      -- PDFs / HTML / codebook text up to `max_doc_mb`.
#   skip     -- Stata files, Dataverse .tab ingests, .missRecode, the
#               non-selected data formats, and SAS/Stata/SPSS data zips.
.borealis_select_files <- function(files, max_doc_mb = getOption("canpumf.borealis_max_doc_mb", 50)) {
  n    <- nrow(files)
  name <- files$filename
  dir  <- ifelse(is.na(files$directory), "", files$directory)
  ext  <- tolower(tools::file_ext(name))
  role <- rep("skip", n)

  is_ingest   <- ext == "tab" & !is.na(files$original)
  in_data_dir <- grepl("(?i)(^|/)(SAS|SPSS|STATA[0-9]*)$", dir, perl = TRUE)
  is_zip      <- ext == "zip"
  cmd_zip     <- is_zip & grepl("(?i)(_sas|_do|_dct|_sps|spss|syntax|command)", name, perl = TRUE) &
                 !grepl("(?i)(csv|ascii|raw)", name, perl = TRUE)

  # -- data
  csv     <- which(ext == "csv" & !in_data_dir)
  csv_zip <- which(is_zip & !cmd_zip & grepl("(?i)csv", name, perl = TRUE))
  fwf_zip <- which(is_zip & !cmd_zip & grepl("(?i)(ascii|txt|dat|raw)", paste(dir, name), perl = TRUE) &
                   !grepl("(?i)csv", name, perl = TRUE))
  fwf     <- which(ext %in% c("txt", "dat") & !grepl("(?i)(codebook|readme|lisezmoi)", name, perl = TRUE) &
                   files$size > 1e6)
  sas7    <- which(ext == "sas7bdat")
  pick_largest <- function(idx) idx[which.max(files$size[idx])]
  data_idx <- if (length(csv))      pick_largest(csv)
              else if (length(csv_zip)) pick_largest(csv_zip)
              else if (length(fwf_zip)) pick_largest(fwf_zip)
              else if (length(fwf))     pick_largest(fwf)
              else if (length(sas7))    pick_largest(sas7)
              else integer(0L)
  role[data_idx] <- "data"

  # -- metadata (command files)
  sps <- which(ext == "sps")
  sas <- which(ext == "sas")
  sav <- which(ext == "sav" | (is_ingest & grepl("\\.sav$", files$original, ignore.case = TRUE)))
  sas_loose <- sas[!in_data_dir[sas]]
  if (length(sps)) {
    role[sps] <- "metadata"
  } else if (length(sas_loose)) {
    role[sas_loose] <- "metadata"
  } else if (length(sav)) {
    # The .sav carries full variable and value labels; prefer it over a
    # converter-generated .sas (generic V1_F format names) in the SAS folder.
    real <- sav[ext[sav] == "sav"]
    role[if (length(real)) real[[1L]] else sav[[1L]]] <- "metadata"
  } else if (length(sas)) {
    role[sas] <- "metadata"
  }
  # Command-code zips (e.g. Census 2021 "Command Code/*_sas.zip") are only
  # needed when no loose command file exists.
  if (!any(role == "metadata") && any(cmd_zip)) role[cmd_zip] <- "metadata"
  # A JSON value-label dictionary (The Canadian Peoples census files) is the
  # only machine-readable metadata of its dataset; parse_json_value_labels().
  role[grepl("(?i)value_labels\\.json$", name, perl = TRUE)] <- "metadata"

  # -- documentation
  doc <- which(role == "skip" &
               (ext %in% c("pdf", "html", "htm") |
                (ext == "txt" & (files$size < 5e6 |
                 grepl("(?i)(codebook|readme|lisezmoi)", name, perl = TRUE)))) &
               !in_data_dir &
               (is.na(files$size) | files$size <= max_doc_mb * 1e6))
  role[doc] <- "doc"

  role[files$restricted & is.null(.borealis_token())] <- "skip"

  files$role     <- role
  files$selected <- role != "skip"
  files
}

#' List the files of a Borealis PUMF dataset
#'
#' Shows every file in a Borealis dataset together with the role canpumf
#' assigns it (`data`, `metadata`, `doc` or `skip`) and whether
#' `get_pumf(..., borealis =)` would download it. Use it to check which data
#' file and command files a DOI provides before loading it.
#'
#' @param doi The dataset DOI, e.g. `"doi:10.5683/SP3/LG7WKC"` (the `doi:`
#'   prefix, a bare `10.5683/...` or a doi.org URL all work), or a one-row
#'   tibble from `list_pumf_catalogue("borealis")`.
#'
#' @return A tibble with one row per file: `file_id`, `filename`, `directory`,
#'   `size` (bytes), `md5`, `content_type`, `original` (the uploaded file
#'   behind a Dataverse `.tab` ingest), `restricted`, `role` and `selected`.
#'   The dataset DOI and title are attached as attributes.
#' @seealso [list_pumf_catalogue()], [get_pumf()]
#' @examples
#' \donttest{
#' tryCatch(list_borealis_pumf_files("doi:10.5683/SP3/LG7WKC"),
#'          error = function(e) message(conditionMessage(e)))
#' }
#' @export
list_borealis_pumf_files <- function(doi) {
  .borealis_select_files(.borealis_dataset_files(.borealis_doi_arg(doi)))
}

# Accept a DOI string or a one-row catalogue tibble.
.borealis_doi_arg <- function(x) {
  if (is.data.frame(x)) {
    if (nrow(x) != 1L || !"doi" %in% names(x))
      stop("Pass a single row of list_pumf_catalogue(\"borealis\") (with a `doi` ",
           "column) or a DOI string.", call. = FALSE)
    x <- x$doi[[1L]]
  }
  .borealis_normalize_doi(x)
}

# ---- Download ---------------------------------------------------------------

# Normalised DOI of a registry entry's `borealis` source, or NULL.
.borealis_entry_doi <- function(reg) {
  b <- reg$borealis
  if (is.character(b)) b <- list(doi = b)
  if (is.null(b$doi)) return(NULL)
  .borealis_normalize_doi(b$doi)
}

.borealis_read_manifest <- function(version_dir) {
  f <- file.path(version_dir, .borealis_manifest_file)
  if (!file.exists(f)) return(NULL)
  tryCatch(readr::read_csv(f, col_types = readr::cols(.default = "c"),
                           progress = FALSE),
           error = function(e) NULL)
}

# DOI recorded in a version directory's manifest, or NULL.
.borealis_manifest_doi <- function(version_dir) {
  m <- .borealis_read_manifest(version_dir)
  if (is.null(m) || nrow(m) == 0L) return(NULL)
  m$doi[[1L]]
}

# DOI of a version cached from Borealis whose source is not the configured one
# (the built-in entry, or `registry` when given), else NULL.  get_pumf() uses
# it so a version loaded with `borealis =` reopens without the argument.
.borealis_cached_doi <- function(series, version, cache_path, registry = NULL) {
  if (.is_longitudinal(series) || is.null(version) || is.null(cache_path)) return(NULL)
  mdoi <- .borealis_manifest_doi(file.path(cache_path, series, version))
  if (is.null(mdoi)) return(NULL)
  reg <- registry %||% .pumf_registry[[paste0(series, "/", version)]]
  if (identical(.borealis_normalize_doi(mdoi), .borealis_entry_doi(reg)))
    return(NULL)
  mdoi
}

# Anchored file_mask for the data file named in the manifest, or NULL.  For a
# zipped data file the manifest records the extracted file in `data_file`.
.borealis_manifest_file_mask <- function(version_dir) {
  m <- .borealis_read_manifest(version_dir)
  if (is.null(m) || !"data_file" %in% names(m)) return(NULL)
  df <- m$data_file[m$role == "data" & !is.na(m$data_file)]
  if (length(df) == 0L) return(NULL)
  paste0("^", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", basename(df[[1L]])), "$")
}

# Download Borealis files to `dest`: one file through the single-file
# endpoint, several as a zip bundle (Borealis stores files uncompressed and
# the single-file endpoint serves them that way; the bundle endpoint deflates
# on the fly, which makes a large text file a much smaller transfer, so a
# one-file bundle is also how a CSV data file is fetched, see
# .borealis_download_csv_gz()).  Dataverse .tab ingests are fetched in their
# original upload format.
.borealis_download_file <- function(file_ids, dest, original = FALSE,
                                    bundle = length(file_ids) > 1L) {
  url <- paste0(BOREALIS_SERVER,
                if (bundle) "/api/access/datafiles/" else "/api/access/datafile/",
                paste(file_ids, collapse = ","),
                if (original) "?format=original" else "")
  key <- .borealis_token()
  headers <- if (is.null(key)) NULL else c(`X-Dataverse-key` = key)
  .pumf_download(url, dest, mode = "wb", quiet = TRUE, headers = headers,
                 source = "Borealis", timeout = 1800L)
}
.borealis_download_bundle <- function(file_ids, dest, original = FALSE) {
  .borealis_download_file(file_ids, dest, original = original, bundle = TRUE)
}

# Download one Borealis CSV into the cache as `<dest>.gz`; returns that path.
# The file comes as a one-file zip bundle (the 1.1 GB TCP 1881 file is an
# 85 MB transfer) whose entry is recompressed straight to gzip, so the
# uncompressed file is neither transferred nor written.  Dataverse leaves a
# file out of a bundle above the installation's zip size limit (it only says
# so in the bundle's MANIFEST.TXT); when the bundle does not hold the complete
# file (`size` bytes, when known), the file is downloaded as is and compressed.
.borealis_download_csv_gz <- function(file_id, dest, size = NA_real_,
                                      original = FALSE) {
  gz  <- paste0(dest, ".gz")
  zip <- paste0(dest, ".bundle.zip")
  on.exit(unlink(zip), add = TRUE)
  ok <- tryCatch({
    .borealis_download_bundle(file_id, zip, original = original)
    # A file in a dataset folder comes as "<folder>/<name>" after an entry for
    # the folder itself.
    entry <- setdiff(utils::unzip(zip, list = TRUE)$Name, "MANIFEST.TXT")
    entry <- entry[!endsWith(entry, "/")]
    length(entry) == 1L && {
      n <- .zip_entry_to_gzip(zip, entry, gz)
      n > 0 && (is.na(size) || n == size)
    }
  }, warning = function(w) FALSE, error = function(e) FALSE)
  if (!isTRUE(ok)) {
    unlink(c(zip, gz))
    message("No compressed download of ", basename(dest),
            "; downloading the uncompressed file ...")
    .borealis_download_file(file_id, dest, original = original)
    gz <- .gzip_file(dest)
  }
  gz
}

# Role of a file the automatic selection skips but the caller pins by name or
# id: a command file is metadata (a SAS card inside the deposit's "SAS" data
# folder, which the selection passes over for the .sav), a document or image
# is documentation, anything else is the data file.
.borealis_pinned_role <- function(filename) {
  ext <- tolower(tools::file_ext(filename))
  ifelse(ext %in% c("sas", "sps", "json"), "metadata",
         ifelse(ext %in% c("pdf", "html", "htm", "jpg", "jpeg", "png", "xls",
                           "xlsx", "doc", "docx"), "doc", "data"))
}

#' @keywords internal
#' @noRd
# Download the selected files of a Borealis dataset into `version_dir` (flat),
# extract zips, and write borealis_manifest.csv.  `files` optionally restricts
# the download to the given file ids or file names (overriding the automatic
# selection, e.g. to pick a different data file).
.borealis_download_dataset <- function(doi, version_dir, files = NULL) {
  listing <- .borealis_select_files(.borealis_dataset_files(doi))
  doi     <- attr(listing, "doi") %||% .borealis_normalize_doi(doi)
  if (!is.null(files)) {
    hit <- listing$file_id %in% suppressWarnings(as.integer(files)) |
           listing$filename %in% files
    if (!any(hit))
      stop("None of the requested Borealis files (", paste(files, collapse = ", "),
           ") are in ", doi, ". See list_borealis_pumf_files().", call. = FALSE)
    listing$selected <- hit
    pinned <- hit & listing$role == "skip"
    listing$role[pinned] <- .borealis_pinned_role(listing$filename[pinned])
    # One data file: a pin that names one outranks the automatic choice.
    if (any(pinned & listing$role == "data"))
      listing$selected[!pinned & listing$role == "data"] <- FALSE
  }
  sel <- listing[listing$selected, , drop = FALSE]
  if (!any(sel$role == "data"))
    stop("Could not identify a data file canpumf can read (CSV, fixed-width ",
         "text or SAS) in Borealis dataset ", doi, ". Inspect it with ",
         "list_borealis_pumf_files(\"", doi, "\") and pass the file ids to ",
         "use via pumf_registry_entry(borealis = list(doi = ..., files = ...)).",
         call. = FALSE)

  dir.create(version_dir, recursive = TRUE, showWarnings = FALSE)
  .pumf_warn_cache_path_on_download()
  title <- attr(listing, "title")
  message("Downloading from Borealis: ",
          if (!is.na(title %||% NA)) title else doi,
          " (", format(sum(sel$size, na.rm = TRUE) / 1e6, digits = 3), " MB, ",
          nrow(sel), " files) ...")
  .borealis_fetch_selected(sel, version_dir, doi, title %||% NA_character_)
}

# Download the selected files `sel` (rows of .borealis_dataset_files() with a
# `role`) into `version_dir`, extract zips, and write borealis_manifest.csv.
# Files are laid out flat under the name Borealis gives them (a Dataverse .tab
# ingest under its original upload's), prefixed with their directory when a
# name repeats; a `dest` column in `sel` instead gives each file's path
# relative to `version_dir` (LFS_HIST keeps the French SAS program in fra/).
# A CSV data file is kept compressed in the cache (every reader takes
# `.csv.gz`); the manifest's md5 stays the one Borealis publishes, i.e. that
# of the uncompressed file.  `doi` is one DOI or one per row of `sel`.
.borealis_fetch_selected <- function(sel, version_dir, doi,
                                     title = NA_character_) {
  dir.create(version_dir, recursive = TRUE, showWarnings = FALSE)
  if (!"dest" %in% names(sel)) sel$dest <- NA_character_
  sel$local     <- NA_character_
  sel$data_file <- NA_character_
  for (i in seq_len(nrow(sel))) {
    use_orig <- tolower(tools::file_ext(sel$filename[[i]])) == "tab" &&
                !is.na(sel$original[[i]])
    rel <- sel$dest[[i]]
    if (is.na(rel)) {
      rel <- if (use_orig) sel$original[[i]] else sel$filename[[i]]
      # Files are flattened; disambiguate a name that repeats across directories.
      if (rel %in% sel$local && !is.na(sel$directory[[i]]))
        rel <- paste0(gsub("[/ ]+", "_", sel$directory[[i]]), "_", rel)
    }
    dest <- file.path(version_dir, rel)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    if (sel$role[[i]] == "data" && grepl("\\.csv$", dest, ignore.case = TRUE)) {
      dest <- .borealis_download_csv_gz(
        sel$file_id[[i]], dest, original = use_orig,
        size = if (use_orig) NA_real_ else sel$size[[i]])
    } else {
      .borealis_download_file(sel$file_id[[i]], dest, original = use_orig)
    }
    sel$local[[i]] <- if (dirname(rel) == ".") basename(dest)
                      else file.path(dirname(rel), basename(dest))
    if (tolower(tools::file_ext(dest)) == "zip") {
      before <- list.files(version_dir, recursive = TRUE)
      robust_unzip(dest, exdir = version_dir)
      if (sel$role[[i]] == "data") {
        new <- setdiff(list.files(version_dir, recursive = TRUE), before)
        cand <- new[grepl("\\.(csv|txt|dat|sas7bdat)$", new, ignore.case = TRUE)]
        csv  <- cand[grepl("\\.csv$", cand, ignore.case = TRUE)]
        if (length(csv)) cand <- csv
        if (length(cand)) {
          sizes <- file.size(file.path(version_dir, cand))
          sel$data_file[[i]] <- cand[[which.max(sizes)]]
        }
      }
    } else if (sel$role[[i]] == "data") {
      sel$data_file[[i]] <- sel$local[[i]]
    }
  }
  .extract_inner_zips(version_dir)

  manifest <- tibble::tibble(
    doi       = doi,
    title     = title,
    file_id   = sel$file_id,
    filename  = sel$local,
    role      = sel$role,
    md5       = sel$md5,
    data_file = sel$data_file,
    fetched   = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  readr::write_csv(manifest, file.path(version_dir, .borealis_manifest_file),
                   na = "")
  invisible(version_dir)
}
