# Experimental full-catalogue scraper for StatCan Public Use Microdata Files.
#
# This is a *separate, exploratory* counterpart to list_canpumf_collection().
# Where list_canpumf_collection() returns the curated set of surveys canpumf
# has download wrappers for, list_statcan_pumf_catalogue() crawls the live
# StatCan "Public use microdata" listing and each survey's product page to
# discover *every* PUMF series, its versions, and direct-download URLs.
#
# The crawl has three levels:
#   L1  the microdata listing       /n1/en/type/data?count=200  (cookie-gated)
#         -> 1 row per survey series (catalogue id + title)
#   L2  the survey catalogue page   /n1/en/catalogue/<ID>
#         -> the product page link(s) /n1/pub/<code>/<code><issue>-eng.htm
#   L3  the product page            (the -eng.htm above)
#         -> the actual .zip download links, one per edition/format
#
# StatCan markup is irregular, so this is best-effort: surveys distributed only
# via Electronic File Transfer (EFT) have no .zip and are reported with
# url = "(EFT)".

.statcan_base <- "https://www150.statcan.gc.ca"
.statcan_microdata_url <-
  "https://www150.statcan.gc.ca/n1/en/type/data?count=200"

# Format tokens that may appear in a download filename, in canpumf's order of
# preference: a plain-text/CSV flat file is easiest to ingest, Beyond 20/20
# (.ivt) the least.  Used both to detect a file's format and to rank choices.
# Session-level cache for list_statcan_pumf_catalogue() results, keyed by the
# arguments that affect output. Persists for the running R session; cleared by
# refresh = TRUE (per key) or by restarting R.
.statcan_catalogue_cache <- new.env(parent = emptyenv())

.statcan_format_tokens <- c(
  CSV   = "CSV",
  TXT   = "TXT",         # StatCan's flat ASCII export; CSV-equivalent for us
  ASCII = "ASCII",
  SPSS  = "SPSS",
  SAS   = "SAS",
  STATA = "STATA",
  IVT   = "IVT"          # Beyond 20/20
)

# Open a session on the microdata landing page so the count/pagination cookie is
# set, then jump to the count=200 listing.  Returns a parsed xml_document.
.statcan_microdata_page <- function() {
  s <- rvest::session(paste0(.statcan_base, "/n1/en/type/data"),
                      httr::user_agent("Mozilla/5.0 (canpumf)"))
  s <- rvest::session_jump_to(s, .statcan_microdata_url)
  rvest::read_html(s)
}

# L1: parse the "Public use microdata" accordion into one row per survey.
# Returns tibble(catalogue_id, Title, catalogue_url).
#
# Most items link to a /n1/en/catalogue/<ID> page, but a few link straight to a
# /n1/pub/<code>/index-eng.htm product page (e.g. specific Aboriginal Peoples
# Survey / GSS cycles surfaced separately).  We recover an id from whichever
# form is present so none get a NA id, then drop the direct-product rows that
# duplicate a real catalogue entry — the catalogue row drives full edition
# discovery, so it is the one to keep.
.statcan_parse_microdata_list <- function(page) {
  det   <- rvest::html_element(page, "details#publicusemicrodata")
  items <- rvest::html_elements(det, "li.ndm-item")
  a     <- rvest::html_element(items, "a")
  href  <- rvest::html_attr(a, "href")
  title <- trimws(rvest::html_text(a))

  cat_id   <- stringr::str_match(href, "catalogue/([0-9A-Za-z-]+)")[, 2]
  is_cat   <- !is.na(cat_id)
  pub_code <- ifelse(grepl("/n1/pub/", href),
                     sub(".*/n1/pub/([^/]+)/.*", "\\1", href), NA_character_)

  out <- tibble::tibble(
    catalogue_id  = ifelse(is_cat, cat_id, pub_code),
    Title         = title,
    catalogue_url = ifelse(grepl("^http", href), href,
                           paste0(.statcan_base, href)),
    .key          = .statcan_norm_id(ifelse(is_cat, cat_id, pub_code)),
    .is_cat       = is_cat
  )
  out <- out[order(!out$.is_cat), , drop = FALSE]                 # catalogue first
  out <- out[!is.na(out$.key) & !duplicated(out$.key), , drop = FALSE]
  out$.key <- out$.is_cat <- NULL
  out
}

# Normalise a catalogue id or product code to alphanumeric-lowercase so the two
# can be compared regardless of StatCan's inconsistent hyphenation across
# vintages (82M0013X -> "82m0013x", 14250001 -> "14-25-0001" -> "14250001").
.statcan_norm_id <- function(x) gsub("[^a-z0-9]", "", tolower(x))

# L2: from a survey catalogue page, find the product page(s) that carry the
# actual downloads.  StatCan links the newest issue as a /n1/pub/.../-eng.htm
# product page (which itself usually lists every edition's zip), plus a set of
# per-issue "More information" catalogue ids.  We return the product-page URLs,
# preferring the directly-linked -eng.htm products.
#
# A catalogue page also cross-links *related but distinct* products (e.g. the
# CCHS page links the "Health Reports" article 82-003-s), which must not be
# mistaken for editions of this survey.  When `catalogue_id` is supplied we keep
# only product pages whose /n1/pub/<code>/ directory belongs to this survey,
# i.e. its normalised code starts with the normalised catalogue id.
#
# StatCan also keeps *archived* catalogue entries for products whose PUMF has
# been migrated into a consolidated umbrella product -- e.g. each legacy
# per-cycle General Social Survey (12M00xxX / 89M00xxX) is marked "- ARCHIVED"
# and its only product link points to the 45-25-0001 GSSP umbrella.  These are
# duplicates of editions the umbrella already carries.  When *every* product
# link belongs to a *different* product (none matches this entry's own
# catalogue id), the returned vector carries `attr(., "replacement_codes")` =
# the normalised codes those links point to; the caller drops the entry when
# all of those replacement products are themselves present in the listing (so
# nothing is lost -- the umbrella that carries the editions is retained).  The
# archived-text signal is unreliable (many migrated pages no longer say
# "ARCHIVED" in the title), so the structural redirect is the trigger.  A
# genuine EFT-only survey has no product link at all and is never flagged.
.statcan_product_urls <- function(catalogue_url, catalogue_id = NULL) {
  pg   <- tryCatch(rvest::read_html(catalogue_url), error = function(e) NULL)
  if (is.null(pg)) return(character(0))
  href <- rvest::html_attr(rvest::html_elements(pg, "a"), "href")
  prod <- href[grepl("/n1/pub/.+-eng\\.htm$", href)]
  prod <- unique(prod[!is.na(prod)])
  replacement <- character(0)
  if (!is.null(catalogue_id) && !is.na(catalogue_id) && length(prod)) {
    cid  <- .statcan_norm_id(catalogue_id)
    code <- sub(".*/n1/pub/([^/]+)/.*", "\\1", prod)
    keep <- startsWith(.statcan_norm_id(code), cid)
    if (!any(keep)) replacement <- unique(.statcan_norm_id(code))
    prod <- prod[keep]
  }
  out <- ifelse(grepl("^http", prod), prod, paste0(.statcan_base, prod))
  attr(out, "replacement_codes") <- replacement
  out
}

# Classify a download into a format token (NA if none recognised).  The
# filename usually carries the token (`2022_CSV.zip`), but on some product pages
# the filename is opaque while the *anchor text* is exactly the format label
# (`<a>CSV</a>`), so we fall back to that.  Legacy bundled downloads (e.g. old
# GSS cycle zips) carry no format signal anywhere and stay NA.
.statcan_detect_format <- function(file, text = NULL) {
  for (src in list(file, text)) {
    if (is.null(src) || is.na(src)) next
    up  <- toupper(src)
    hit <- vapply(.statcan_format_tokens,
                  function(tok) grepl(tok, up, fixed = TRUE),
                  logical(1))
    if (any(hit)) return(names(.statcan_format_tokens)[which(hit)[1]])
  }
  NA_character_
}

# Census PUMF filenames encode the reference year as a `cenNN` / `nhsNN` prefix
# (the 2011 cycle shipped as the National Household Survey) and the file type as
# `ind` / `fam` / `hous` / `hier`, e.g. `cen21_ind_98m0001x_part_rec21.zip` ->
# "2021 (individuals)".  The generic period detector can't see this — the only
# four-digit run in the name is the StatCan product code (`98m0001x` -> "0001")
# — so census downloads get a dedicated decoder, matching the canonical Version
# strings list_canpumf_collection() uses.  Forward-compatible: when the 2026
# census PUMF lands as `cen26_...` it resolves to "2026 (...)" with no change
# here.  Two-digit years map at a 2030 cut-off (00-30 -> 2000s, else 1900s).
# Returns NA for non-census filenames so the generic path handles everything else.
.statcan_census_edition <- function(file) {
  if (is.null(file) || is.na(file)) return(NA_character_)
  m <- stringr::str_match(tolower(basename(file)),
                          "^(?:cen|nhs)(\\d{2})_(ind|fam|hous|hier)_")
  if (is.na(m[1L])) return(NA_character_)
  yy   <- as.integer(m[2L])
  year <- if (yy <= 30L) 2000L + yy else 1900L + yy
  type <- c(ind = "individuals", fam = "families",
            hous = "households",  hier = "hierarchical")[[m[3L]]]
  sprintf("%d (%s)", year, type)
}

# Guess the edition/reference period for a download from its anchor text or
# filename: a year ("2023-CSV.zip" -> "2023"), a year-month for monthly series
# ("2026-05-CSV.zip" -> "2026-05"), or a year range ("1997-1998"). We take the
# most specific match found in either string so monthly editions are not
# collapsed to their year (which would let the per-edition format de-dup drop
# eleven months).  When neither carries a period (e.g. CPSS / ITS, whose files
# are named CSV.zip / SAS.zip), fall back to the directory segment immediately
# before the filename in the download path: a reference year/range ("/2019/",
# "/2019-2020/") or a StatCan issue ("/2020001/" -> the leading year).
.statcan_detect_edition <- function(text, file, url = NULL) {
  census <- .statcan_census_edition(file)
  if (!is.na(census)) return(census)
  pat  <- "\\d{4}(?:-\\d{2,4})?"
  cand <- c(stringr::str_extract(file, pat), stringr::str_extract(text, pat))
  cand <- cand[!is.na(cand)]
  if (length(cand)) return(cand[which.max(nchar(cand))])
  if (!is.null(url)) {
    seg <- sub(".*/", "", sub("/[^/]+$", "", url))          # dir before the file
    if (grepl("^\\d{4}(?:-\\d{2,4})?$", seg)) return(seg)    # /2019/ or /2019-2020/
    iss <- stringr::str_match(seg, "^(\\d{4})\\d{3}$")[, 2]  # /2020001/ issue
    if (!is.na(iss)) return(iss)
  }
  NA_character_
}

# L3: parse a product page into one row per downloadable zip.
# Returns tibble(edition, format, file, url).
.statcan_parse_product_page <- function(product_url) {
  pg <- tryCatch(rvest::read_html(product_url), error = function(e) NULL)
  if (is.null(pg)) return(NULL)
  a    <- rvest::html_elements(pg, "a")
  href <- rvest::html_attr(a, "href")
  text <- trimws(rvest::html_text(a))
  keep <- which(grepl("\\.zip", href, ignore.case = TRUE))
  if (length(keep) == 0L) return(NULL)
  base <- sub("[^/]*$", "", product_url)            # product page directory
  purrr::map_dfr(keep, function(i) {
    h    <- href[i]
    url  <- if (grepl("^http", h)) h
            else if (startsWith(h, "/")) paste0(.statcan_base, h)  # site-absolute
            else paste0(base, h)                                   # page-relative
    file <- basename(sub("\\?.*$", "", h))
    tibble::tibble(
      edition = .statcan_detect_edition(text[i], file, url),
      format  = .statcan_detect_format(file, text[i]),
      file    = file,
      url     = url
    )
  })
}

# Among several format choices for one edition, keep the most-preferred.
.statcan_pick_format <- function(df, prefer) {
  if (nrow(df) <= 1L) return(df)
  rank <- match(df$format, prefer)
  rank[is.na(rank)] <- length(prefer) + 1L
  df[order(rank), , drop = FALSE][1L, , drop = FALSE]
}

# Identity of a download *modulo its format*: the basename with extension and any
# format token (and adjacent separators) stripped.  StatCan offers the same file
# as `..._CSV.zip`/`..._SAS.zip` (or bare `CSV.zip`/`SAS.zip`), which collapse to
# the same stem and are genuine format-variants; files with no format token keep
# their whole name, so distinct surveys or file-types sharing one edition string
# (GSS giving-survey vs cycle in 2007; census ind/fam/hous) stay distinct.
.statcan_file_stem <- function(file, format = NULL) {
  stem <- sub("\\.[^.]+$", "", basename(file %||% ""))
  if (!is.null(format) && !is.na(format)) {
    tok  <- .statcan_format_tokens[[format]] %||% format
    stem <- gsub(paste0("[ _.-]*", tok, "[ _.-]*"), "", stem, ignore.case = TRUE)
  }
  tolower(gsub("[^A-Za-z0-9]", "", stem))
}

# StatCan exposes no short survey-acronym field, so we derive one from the title.
# Census is hard-coded to "Census" to stay consistent with the curated
# list_canpumf_collection() (its initialism "CP" would obscure an important
# survey).  Otherwise two sources, in priority: (1) a genuine parenthetical
# acronym carried in the title -- "... Survey (MHACS): ..." or "... Model
# (SPSD/M)" -- ignoring the generic "(PUMF)"; (2) failing that, an initialism of
# the survey name with the trailing "Public Use Microdata File" boilerplate,
# connective stopwords, and edition numbers removed.  This reproduces the curated
# acronym for most surveys (CHS, SFS, CIS, LFS, SHS, GSS, ITS, CPSS, CCAHS, ...);
# one editorial short form still differs by design (SGVP -> "CSGVP").  Best-
# effort: a few surveys yield an unconventional initialism.
.statcan_acronym_skip <- c("of", "on", "and", "the", "to", "in", "for", "a", "an",
                           "with", "at", "or", "microdata", "file", "files",
                           "collection", "pumf", "pumfs", "cycle")
.statcan_acronym <- function(title) {
  if (is.null(title) || is.na(title)) return(NA_character_)
  if (grepl("\\bcensus\\b", title, ignore.case = TRUE)) return("Census")
  paren <- stringr::str_match(
    title, "\\(([A-Z][A-Za-z0-9/&-]*[A-Z][A-Za-z0-9/&-]*)\\)")[, 2]
  if (!is.na(paren) && !grepl("^PUMFs?$", paren, ignore.case = TRUE))
    return(paren)
  name <- sub("\\s*[:–—-]\\s+.*$", "", title)  # drop boilerplate tail
  name <- gsub("[^A-Za-z0-9 ]", " ", name)               # punctuation -> space
  toks <- strsplit(trimws(name), "\\s+")[[1]]
  toks <- toks[!tolower(toks) %in% .statcan_acronym_skip]
  toks <- toks[!grepl("^[0-9]+$", toks) & nzchar(toks)]  # drop year/edition tokens
  if (!length(toks)) return(NA_character_)
  paste(toupper(substr(toks, 1L, 1L)), collapse = "")
}

# Plain-language *series* title matching the acronym/catalogue, derived from the
# survey's catalogue (L1) title by stripping the edition-specific tail and the
# "Public Use Microdata File(s)" boilerplate.  StatCan's catalogue titles come in
# two shapes: an umbrella product carries only the series name ("General Social
# Survey – Public Use Microdata Files" -> "General Social Survey"); a per-edition
# product folds the edition into the title ("Canadian Perspectives Survey Series
# 6: Substance Use ..." -> "Canadian Perspectives Survey Series", "Canadian
# COVID-19 Antibody and Health Survey, Cycle 1: PUMF" -> "Canadian COVID-19
# Antibody and Health Survey").  A trailing "Cycle" is an edition marker and is
# dropped; a trailing "Series" is kept (it is part of the CPSS series name).
# Best-effort, like .statcan_acronym(): a few surveys yield an imperfect strip.
.statcan_series_title <- function(title) {
  if (is.null(title) || is.na(title)) return(NA_character_)
  s <- sub("\\s*[:–—-]\\s+.*$", "", title)              # edition/boilerplate tail
  s <- sub("\\s+\\(?\\d{4}\\)?\\s*$", "", s)             # trailing (YYYY) or YYYY
  s <- sub("\\s+\\d+\\s*$", "", s)                       # trailing cycle/series number
  s <- sub("[,;]?\\s+Cycle\\s*$", "", s, ignore.case = TRUE)  # trailing edition marker
  s <- sub("[,;]\\s*$", "", s)                           # dangling separator
  trimws(s)
}

# GSS editions collide on year, so a bare year is not a unique Version key:
# 2000/2004/2007/2010 each carry a GSS cycle *and* a Giving/Volunteering survey,
# and 2018 carries cycles 32 and 33.  The disambiguator is in the zip filename --
# the GSS cycle number (`cNN_`) or the legacy Giving/Volunteering product prefix
# (NSGVP 1997/2000, CSGVP 2004-2010; the post-2013 GVP rounds became regular GSS
# cycles and are already `cNN_`).  Canonical key is "Cycle N (YYYY)" or
# "<PREFIX> (YYYY)"; falls back to the bare edition when neither pattern matches.
#
# A few GSS PUMFs ship without the `cNN_` cycle prefix in the filename; map those
# file stems to their cycle number explicitly (e.g. the 2022 Time Use file
# TU_ET_2022.zip is cycle 36).
.statcan_gss_cycle_exceptions <- c("TU_ET" = "36")

.statcan_gss_version <- function(file, edition) {
  f  <- basename(sub("\\?.*$", "", file %||% ""))
  yr <- stringr::str_extract(f, "\\d{4}")
  if (is.na(yr)) yr <- edition
  cyc <- stringr::str_match(f, "(?i)^c(\\d+)_")[, 2]
  if (!is.na(cyc)) return(sprintf("Cycle %s (%s)", cyc, yr))
  for (stem in names(.statcan_gss_cycle_exceptions))
    if (grepl(stem, f, fixed = TRUE))
      return(sprintf("Cycle %s (%s)", .statcan_gss_cycle_exceptions[[stem]], yr))
  gvp <- stringr::str_match(f, "(?i)^(NSGVP|CSGVP|GVP)")[, 2]
  if (!is.na(gvp)) return(sprintf("%s (%s)", toupper(gvp), yr))
  edition
}

# CPSS and CCAHS are keyed by bare cycle number ("1".."6" / "1") in the registry,
# but their download URLs carry only a colliding reference year (CPSS cycles 1-4
# all live under /2020001/, 5-6 under /2021001/), so the year-derived edition is
# not a unique Version key.  The cycle number is, however, embedded in the survey
# title ("... Survey Series 6: ..." / "... Health Survey, Cycle 2: ..."); extract
# it so the adapter Version matches the registry key.  Falls back to `edition`.
.statcan_cycle_version <- function(title, edition) {
  m <- stringr::str_match(title %||% "", "(?i)(?:Series|Cycle)\\s+(\\d+)")[, 2]
  if (!is.na(m)) return(m)
  edition
}

# Structural, edition-specific descriptor used to synthesise a per-edition Title
# for umbrella products whose catalogue (L1) title is only the series name.  GSS
# editions collide on year, so the disambiguator is the cycle/GVP key carried in
# the zip filename (.statcan_gss_version -> "Cycle 16 (2002)"); every other
# series uses the (already disambiguated, e.g. census "2021 (individuals)")
# edition string as-is.  No survey theme is invented here -- the descriptor is
# purely structural (cycle/variant/period), per the catalogue-swap design.
.statcan_edition_descriptor <- function(acronym, url, edition) {
  if (identical(acronym, "GSS")) return(.statcan_gss_version(url, edition))
  edition
}

# Series the adapter exposes to the curated-collection contract.  LFS and Census
# are deliberately excluded -- they keep their dedicated, semi-manual paths (LFS
# has monthly granularity and a shared-DuckDB pipeline the crawl does not
# reproduce; Census needs the EFT 1971-1986 editions injected).  The legacy
# Giving/Volunteering surveys now live under the GSS (GSSP) umbrella, so no
# separate SGVP entry is needed.
.statcan_supported_series <- c("GSS", "SHS", "SFS", "CPSS", "CIS", "CHS",
                               "ITS", "CCAHS")

# Adapter: turn the raw crawl frame from list_statcan_pumf_catalogue() into the
# curated-collection contract get_pumf()/pipeline.R consume -- columns
# `Acronym`, `Version`, `url` (one download per series+version) plus `Title` and
# the crawl-provenance columns `catalogue_id`, `survey_url`, `product_url`.
# Filters to canpumf-supported series, derives the canonical `Version` key
# (GSS cycle-aware; bare edition otherwise), drops editionless phantom rows
# (e.g. archived per-cycle GSS catalogue entries the umbrella already carries),
# and guarantees a single highest-preference URL per (Acronym, Version).
.statcan_catalogue_to_collection <- function(cat,
                                             supported = .statcan_supported_series,
                                             prefer    = names(.statcan_format_tokens)) {
  empty <- tibble::tibble(
    Acronym = character(), Version = character(), SeriesTitle = character(),
    Title = character(), url = character(), catalogue_id = character(),
    survey_url = character(), product_url = character())
  if (is.null(cat) || nrow(cat) == 0L) return(empty)
  if (!is.null(supported))
    cat <- cat[cat$Acronym %in% supported, , drop = FALSE]
  if (nrow(cat) == 0L) return(empty)
  # Tolerate a crawl frame persisted before SeriesTitle existed by deriving it.
  if (!("SeriesTitle" %in% names(cat)))
    cat$SeriesTitle <- vapply(cat$Title, .statcan_series_title, character(1L),
                              USE.NAMES = FALSE)

  # Canonical Version key. GSS year collides, so derive Cycle N (YYYY) etc.;
  # CPSS/CCAHS years also collide (several cycles per year), so key them by the
  # cycle number carried in the title to match their bare-number registry keys.
  ver <- cat$edition
  is_gss <- cat$Acronym == "GSS"
  if (any(is_gss))
    ver[is_gss] <- mapply(.statcan_gss_version, cat$url[is_gss],
                          cat$edition[is_gss], USE.NAMES = FALSE)
  is_cyc <- cat$Acronym %in% c("CPSS", "CCAHS")
  if (any(is_cyc))
    ver[is_cyc] <- mapply(.statcan_cycle_version, cat$Title[is_cyc],
                          cat$edition[is_cyc], USE.NAMES = FALSE)
  cat$Version <- ver
  # Drop rows with no usable version (e.g. the no-download archived GSS cycle
  # catalogue entries whose editions the umbrella already supplies).
  cat <- cat[!is.na(cat$Version), , drop = FALSE]
  if (nrow(cat) == 0L) return(empty)

  # One URL per (Acronym, Version): the crawl already collapsed format variants
  # per file-stem, but distinct stems can still map to one derived Version, so
  # keep the highest-preference format.
  rank <- match(cat$format, prefer)
  rank[is.na(rank)] <- length(prefer) + 1L
  cat <- cat[order(cat$Acronym, cat$Version, rank), , drop = FALSE]
  cat <- cat[!duplicated(cat[, c("Acronym", "Version")]), , drop = FALSE]

  tibble::tibble(
    Acronym      = cat$Acronym,
    Version      = cat$Version,
    SeriesTitle  = cat$SeriesTitle,
    Title        = cat$Title,
    url          = cat$url,
    catalogue_id = cat$catalogue_id,
    survey_url   = cat$survey_url,
    product_url  = cat$product_url)
}

# Adapter output (curated-collection contract) built from the *cached* catalogue
# only -- never triggers a live crawl.  Returns NULL when no catalogue snapshot
# is available at all.
.pumf_adapter_collection <- function(cache_path = getOption("canpumf.cache_path")) {
  cat <- .statcan_catalogue_cached(cache_path)
  if (is.null(cat)) return(NULL)
  .statcan_catalogue_to_collection(cat)
}

# Download-URL resolver used by the pipeline (Stage 1).  For series the scraper
# covers (.statcan_supported_series) the scraped catalogue is consulted first,
# so freshly released editions resolve without a registry/curated-list edit; on
# a miss -- or for series the scraper deliberately does not cover (LFS and Census
# keep their dedicated paths; SGVP ships under reused zip names the umbrella
# crawl can't disambiguate) -- it falls back to the curated list_canpumf_collection().
# Returns a 0- or 1-row tibble carrying at least Acronym/Version/url.
.pumf_resolve_collection_row <- function(series, version,
                                         cache_path = getOption("canpumf.cache_path")) {
  if (series %in% .statcan_supported_series) {
    adon <- tryCatch(.pumf_adapter_collection(cache_path), error = function(e) NULL)
    if (!is.null(adon) && nrow(adon)) {
      hit <- adon[adon$Acronym == series & adon$Version == version, , drop = FALSE]
      if (nrow(hit)) return(hit)
    }
  }
  coll <- tryCatch(list_canpumf_collection(), error = function(e) NULL)
  if (is.null(coll))
    return(tibble::tibble(Acronym = character(), Version = character(),
                          url = character()))
  coll[coll$Acronym == series & coll$Version == version, , drop = FALSE]
}

# --- Persistent catalogue cache --------------------------------------------
# A full crawl issues hundreds of requests, so besides the per-session env cache
# (.statcan_catalogue_cache) the *full default* crawl is also persisted to disk
# under the user's configured cache_path, surviving across R sessions.  When no
# durable cache_path is set (data lives in tempdir() for the session only) there
# is nothing to persist and only the session cache is used.  The stored object
# is list(fetched, prefer, data); a read older than the staleness threshold
# warns the user to regenerate, and a live crawl that fails (StatCan
# unreachable) falls back to the last good persisted copy.

# Path to the persisted catalogue, or NULL when no durable cache_path is set.
.statcan_catalogue_cache_file <- function(
    cache_path = getOption("canpumf.cache_path")) {
  if (is.null(cache_path) || !nzchar(cache_path)) return(NULL)
  file.path(cache_path, "pumf_catalogue.rds")
}

# Staleness threshold in days (default 1 month), overridable via option.
.statcan_catalogue_max_age <- function()
  as.numeric(getOption("canpumf.catalogue_max_age_days", 30))

.statcan_read_persistent <- function(file) {
  if (is.null(file) || !file.exists(file)) return(NULL)
  obj <- tryCatch(readRDS(file), error = function(e) NULL)
  if (is.list(obj) && all(c("fetched", "data") %in% names(obj))) obj else NULL
}

# The catalogue snapshot shipped with the package (inst/extdata/pumf_catalogue.rds),
# refreshed at each release by tools/refresh_catalogue_snapshot.R.  Acts as the
# terminal, always-available fallback: a freshly installed package with no user
# cache and no network still resolves download URLs from this ground-truth copy,
# so a StatCan markup change cannot silently break get_pumf() between releases.
# Same list(fetched, prefer, data) shape as the persisted cache; NULL when the
# package ships without one (e.g. before the first snapshot is generated).
.statcan_shipped_snapshot <- function() {
  f <- system.file("extdata", "pumf_catalogue.rds", package = "canpumf")
  if (!nzchar(f)) return(NULL)
  .statcan_read_persistent(f)
}

# Non-crawling catalogue accessor used by the download-URL resolver: returns the
# crawl *data frame* from the cheapest available source -- the in-session cache,
# then the user's persisted cache, then the shipped snapshot -- WITHOUT ever
# launching a live crawl (which issues hundreds of requests).  Returns NULL only
# when none of the three exist.  Callers wanting a guaranteed-fresh crawl use
# list_statcan_pumf_catalogue(refresh = TRUE) instead.
.statcan_catalogue_cached <- function(cache_path = getOption("canpumf.cache_path")) {
  key <- paste0(
    "prefer=",  paste(names(.statcan_format_tokens), collapse = ","), ";",
    "max=all;surveys=all")
  if (!is.null(.statcan_catalogue_cache[[key]]))
    return(.statcan_catalogue_cache[[key]])
  cached <- .statcan_read_persistent(.statcan_catalogue_cache_file(cache_path))
  if (is.null(cached)) cached <- .statcan_shipped_snapshot()
  if (is.null(cached)) return(NULL)
  cached$data
}

.statcan_write_persistent <- function(file, data, prefer) {
  if (is.null(file)) return(invisible(NULL))
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  tryCatch(saveRDS(list(fetched = Sys.time(), prefer = prefer, data = data),
                   file),
           error = function(e) NULL)
  invisible(NULL)
}

.statcan_warn_if_stale <- function(fetched) {
  age <- suppressWarnings(as.numeric(difftime(Sys.time(), fetched,
                                              units = "days")))
  if (is.finite(age) && age > .statcan_catalogue_max_age())
    warning(sprintf(
      paste0("Cached StatCan PUMF catalogue is %.0f days old (fetched %s) and ",
             "may be out of date. Regenerate with ",
             "list_statcan_pumf_catalogue(refresh = TRUE)."),
      age, format(fetched, "%Y-%m-%d")), call. = FALSE)
  invisible(NULL)
}

# The live crawl proper (L1 -> L2 -> L3), factored out of the cached wrapper.
.statcan_crawl_catalogue <- function(prefer, max_surveys, surveys, verbose) {
  page    <- .statcan_microdata_page()
  listing <- .statcan_parse_microdata_list(page)

  if (!is.null(surveys))
    listing <- listing[listing$catalogue_id %in% surveys, , drop = FALSE]
  if (!is.null(max_surveys))
    listing <- utils::head(listing, max_surveys)

  # normalised ids of every survey we are keeping, so a migrated entry is only
  # dropped when the umbrella that absorbed it is itself in the result.
  listing_ids <- .statcan_norm_id(listing$catalogue_id)

  out <- purrr::map_dfr(seq_len(nrow(listing)), function(i) {
    row <- listing[i, ]
    if (isTRUE(verbose))
      message(sprintf("[%d/%d] %s (%s)", i, nrow(listing),
                      row$Title, row$catalogue_id %||% "?"))

    prods <- tryCatch(.statcan_product_urls(row$catalogue_url, row$catalogue_id),
                      error = function(e) character(0))
    # Catalogue entry whose PUMF was migrated into a consolidated umbrella
    # product (e.g. legacy GSS cycles -> 45-25-0001): every product link points
    # at a different product. Drop it so the survey is not listed twice -- but
    # only when *all* of those replacement products are themselves in the
    # listing, so the editions are never lost (the umbrella carries them).
    repl <- attr(prods, "replacement_codes")
    if (length(repl) && all(repl %in% listing_ids)) return(NULL)
    dl <- purrr::map_dfr(prods, .statcan_parse_product_page)

    if (is.null(dl) || nrow(dl) == 0L) {
      return(tibble::tibble(catalogue_id = row$catalogue_id, Title = row$Title,
                            survey_url = row$catalogue_url,
                            edition = NA_character_, format = NA_character_,
                            url = "(EFT)", product_url = prods[1] %||% NA))
    }

    # one product page; remember which it came from for traceability
    dl$product_url <- if (length(prods) == 1L) prods else NA_character_

    # Collapse only rows that are the *same file* in different formats: group by
    # (edition, file-stem-modulo-format), not edition alone.  Grouping on edition
    # alone silently drops genuinely distinct surveys/file-types that share a year
    # (GSS giving-survey vs cycle in 2007) or a mis-parsed edition string (census
    # cen21/cen16 both -> "0001").  A sentinel replaces NA edition so an undated
    # download still yields its own row instead of being dropped by split().
    stem <- mapply(.statcan_file_stem, dl$file, dl$format, USE.NAMES = FALSE)
    ed   <- ifelse(is.na(dl$edition), "NA", dl$edition)
    parts <- split(dl, paste(ed, stem, sep = "\r"))
    dl <- purrr::map_dfr(parts, .statcan_pick_format, prefer = prefer)

    tibble::tibble(catalogue_id = row$catalogue_id, Title = row$Title,
                   survey_url = row$catalogue_url,
                   edition = dl$edition, format = dl$format,
                   url = dl$url, product_url = dl$product_url)
  })

  cols <- c("catalogue_id", "Acronym", "SeriesTitle", "Title", "survey_url",
            "edition", "format", "url", "product_url")
  if (is.null(out) || nrow(out) == 0L) {
    empty <- tibble::tibble(!!!stats::setNames(
      rep(list(character(0)), length(cols)), cols))
    return(empty)
  }
  out$Acronym <- vapply(out$Title, .statcan_acronym, character(1L),
                        USE.NAMES = FALSE)

  # SeriesTitle: the plain-language series name (always from the L1 title).
  # Title: edition-specific.  StatCan's per-edition catalogue entries already
  # carry an edition-specific title, so those are kept; an *umbrella* catalogue
  # entry (one catalogue_id carrying several editions under a single series-level
  # title, e.g. the GSS 45-25-0001 umbrella or a census year's ind+hier) has a
  # generic L1 title, so we synthesise "<series> — <structural edition descriptor>".
  out$SeriesTitle <- vapply(out$Title, .statcan_series_title, character(1L),
                            USE.NAMES = FALSE)
  n_ed     <- tapply(out$edition, out$catalogue_id,
                     function(e) length(unique(e[!is.na(e)])))
  umbrella <- !is.na(out$edition) & as.integer(n_ed[out$catalogue_id]) > 1L
  desc     <- mapply(.statcan_edition_descriptor, out$Acronym, out$url,
                     out$edition, USE.NAMES = FALSE)
  out$Title <- ifelse(umbrella & !is.na(out$SeriesTitle),
                      paste0(out$SeriesTitle, " — ", desc),
                      out$Title)

  out[, cols]
}

#' Crawl the full Statistics Canada PUMF catalogue (experimental)
#'
#' Scrapes the live StatCan "Public use microdata" listing and follows each
#' survey to its product page to discover every PUMF series, its editions, and
#' direct-download URLs.  This is an exploratory counterpart to
#' [list_canpumf_collection()], which returns only the curated set of surveys
#' canpumf has tested download wrappers for.
#'
#' The StatCan markup is irregular and this crawler is best-effort: surveys
#' distributed only by Electronic File Transfer (EFT) report `url = "(EFT)"`,
#' and some products may not be parsed.  When an edition is offered in several
#' formats the one highest in `prefer` is kept (CSV/flat-text first).
#'
#' @param prefer Character vector of format tokens in order of preference; the
#'   default puts CSV / flat text ahead of statistical-package formats.
#' @param max_surveys Optional integer: only crawl the first N surveys (useful
#'   for a quick look — a full crawl issues a few hundred requests).
#' @param surveys Optional character vector of catalogue ids to restrict to.
#' @param verbose If `TRUE`, print progress as each survey is crawled.
#' @param refresh If `FALSE` (the default), the crawl result is cached and
#'   reused — a full crawl is expensive (hundreds of requests). Within a session
#'   it is held in memory; a *full* crawl (no `max_surveys`/`surveys`) is also
#'   persisted to disk under `cache_path` so it survives across sessions. Set
#'   `TRUE` to re-scrape the live catalogue and replace both caches, e.g. to
#'   pick up a newly released survey.
#' @param cache_path Directory for the cross-session catalogue cache
#'   (`pumf_catalogue.rds`). Defaults to `getOption("canpumf.cache_path")`; when
#'   unset there is no durable cache and only the in-session cache is used. A
#'   persisted catalogue older than
#'   `getOption("canpumf.catalogue_max_age_days", 30)` triggers a staleness
#'   warning suggesting `refresh = TRUE`. If a live crawl fails (StatCan
#'   unreachable) the last persisted copy is returned with a warning.
#'
#' @return A tibble with one row per discovered edition: `catalogue_id`,
#'   `Acronym`, `SeriesTitle`, `Title`, `survey_url`, `edition`, `format`,
#'   `url`, and `product_url`. `SeriesTitle` is the plain-language series name
#'   matching the acronym (the catalogue title with the edition-specific tail and
#'   "Public Use Microdata File" boilerplate stripped). `Title` is
#'   edition-specific: StatCan's own per-edition catalogue title where it carries
#'   one, otherwise — for *umbrella* products whose catalogue title is only the
#'   series name (e.g. the consolidated General Social Survey, or a census year's
#'   individuals/hierarchical pair) — a synthesised `"<series> — <edition>"`,
#'   where the structural edition descriptor disambiguates colliding years (GSS
#'   `"Cycle 16 (2002)"`, census `"2021 (individuals)"`). `edition` remains the
#'   reference period/variant. `survey_url` is the survey's catalogue overview
#'   page (the L2 page the crawler followed); `url`/`product_url` point at the
#'   individual edition's download and product page. `Acronym` and `SeriesTitle`
#'   are derived from the title since StatCan exposes no such field; they match
#'   the curated [list_canpumf_collection()] values for most surveys but are
#'   best-effort (Census `Acronym` is hard-coded to `"Census"`). Surveys with no
#'   downloadable file get a single row with `url = "(EFT)"`.
#'
#' @seealso [list_canpumf_collection()], [get_pumf()]
#'
#' @examples
#' \dontrun{
#' # Quick look at the first 5 surveys
#' head(list_statcan_pumf_catalogue(max_surveys = 5))
#' }
#' @export
list_statcan_pumf_catalogue <- function(prefer      = names(.statcan_format_tokens),
                                        max_surveys = NULL,
                                        surveys     = NULL,
                                        verbose     = TRUE,
                                        refresh     = FALSE,
                                        cache_path  = getOption("canpumf.cache_path")) {
  key <- paste0(
    "prefer=",  paste(prefer, collapse = ","), ";",
    "max=",     if (is.null(max_surveys)) "all" else max_surveys, ";",
    "surveys=", if (is.null(surveys)) "all" else paste(sort(surveys), collapse = ","))
  if (!refresh && !is.null(.statcan_catalogue_cache[[key]]))
    return(.statcan_catalogue_cache[[key]])

  # Only a full crawl (every survey, default selection) is persisted across
  # sessions; partial crawls stay in the session cache only.
  is_full    <- is.null(surveys) && is.null(max_surveys)
  cache_file <- .statcan_catalogue_cache_file(cache_path)

  if (!refresh && is_full) {
    cached <- .statcan_read_persistent(cache_file)
    if (!is.null(cached) && identical(cached$prefer, prefer)) {
      .statcan_warn_if_stale(cached$fetched)
      .statcan_catalogue_cache[[key]] <- cached$data
      return(cached$data)
    }
  }

  # Live crawl, wrapped so an unreachable StatCan falls back to the last good
  # copy instead of erroring: first the user's persisted cache, then the
  # snapshot shipped with the package (always available, even on a fresh install
  # with no network).
  out <- tryCatch(
    .statcan_crawl_catalogue(prefer, max_surveys, surveys, verbose),
    error = function(e) {
      cached <- if (is_full) .statcan_read_persistent(cache_file) else NULL
      if (is.null(cached)) cached <- .statcan_shipped_snapshot()
      if (is.null(cached)) stop(e)
      warning("Statistics Canada catalogue unreachable; returning the last ",
              "cached PUMF catalogue (fetched ",
              format(cached$fetched, "%Y-%m-%d"), "). ", conditionMessage(e),
              call. = FALSE)
      cached$data
    })

  .statcan_catalogue_cache[[key]] <- out
  if (is_full) .statcan_write_persistent(cache_file, out, prefer)
  out
}
