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
.statcan_product_urls <- function(catalogue_url, catalogue_id = NULL) {
  pg   <- tryCatch(rvest::read_html(catalogue_url), error = function(e) NULL)
  if (is.null(pg)) return(character(0))
  href <- rvest::html_attr(rvest::html_elements(pg, "a"), "href")
  prod <- href[grepl("/n1/pub/.+-eng\\.htm$", href)]
  prod <- unique(prod[!is.na(prod)])
  if (!is.null(catalogue_id) && !is.na(catalogue_id) && length(prod)) {
    cid  <- .statcan_norm_id(catalogue_id)
    code <- .statcan_norm_id(sub(".*/n1/pub/([^/]+)/.*", "\\1", prod))
    prod <- prod[startsWith(code, cid)]
  }
  ifelse(grepl("^http", prod), prod, paste0(.statcan_base, prod))
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

# Guess the edition/reference period for a download from its anchor text or
# filename: a year ("2023-CSV.zip" -> "2023"), a year-month for monthly series
# ("2026-05-CSV.zip" -> "2026-05"), or a year range ("1997-1998"). We take the
# most specific match found in either string so monthly editions are not
# collapsed to their year (which would let the per-edition format de-dup drop
# eleven months).
.statcan_detect_edition <- function(text, file) {
  pat  <- "\\d{4}(?:-\\d{2,4})?"
  cand <- c(stringr::str_extract(file, pat), stringr::str_extract(text, pat))
  cand <- cand[!is.na(cand)]
  if (!length(cand)) return(NA_character_)
  cand[which.max(nchar(cand))]
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
    url  <- if (grepl("^http", h)) h else paste0(base, h)
    file <- basename(sub("\\?.*$", "", h))
    tibble::tibble(
      edition = .statcan_detect_edition(text[i], file),
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
#'
#' @return A tibble with one row per discovered edition: `catalogue_id`,
#'   `Title`, `edition`, `format`, `url`, and `product_url`.  Surveys with no
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
                                        verbose     = TRUE) {
  page    <- .statcan_microdata_page()
  listing <- .statcan_parse_microdata_list(page)

  if (!is.null(surveys))
    listing <- listing[listing$catalogue_id %in% surveys, , drop = FALSE]
  if (!is.null(max_surveys))
    listing <- utils::head(listing, max_surveys)

  out <- purrr::map_dfr(seq_len(nrow(listing)), function(i) {
    row <- listing[i, ]
    if (isTRUE(verbose))
      message(sprintf("[%d/%d] %s (%s)", i, nrow(listing),
                      row$Title, row$catalogue_id %||% "?"))

    prods <- tryCatch(.statcan_product_urls(row$catalogue_url, row$catalogue_id),
                      error = function(e) character(0))
    dl <- purrr::map_dfr(prods, .statcan_parse_product_page)

    if (is.null(dl) || nrow(dl) == 0L) {
      return(tibble::tibble(catalogue_id = row$catalogue_id, Title = row$Title,
                            edition = NA_character_, format = NA_character_,
                            url = "(EFT)", product_url = prods[1] %||% NA))
    }

    # one product page; remember which it came from for traceability
    dl$product_url <- if (length(prods) == 1L) prods else NA_character_

    # collapse multiple formats per edition to the preferred one
    dl <- dl[!is.na(dl$edition) | is.na(dl$edition), , drop = FALSE]
    parts <- split(dl, dl$edition)
    dl <- purrr::map_dfr(parts, .statcan_pick_format, prefer = prefer)

    tibble::tibble(catalogue_id = row$catalogue_id, Title = row$Title,
                   edition = dl$edition, format = dl$format,
                   url = dl$url, product_url = dl$product_url)
  })

  out
}
