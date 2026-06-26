#' Open PUMF documentation in the browser
#'
#' Scans the cached version directory for PDF documentation files and opens
#' them interactively.  If no PDFs are found, falls back to small text files
#' (filtering out large FWF data files by size).  When multiple candidate
#' files exist, an interactive menu lets you choose which to open, with
#' "Open all" as the last option.  In non-interactive mode the first
#' preferred-language file is opened automatically.
#'
#' After opening documentation, emits a message listing any manual registry
#' overrides (sentinel values, forced-numeric columns, column swaps, etc.)
#' that were applied at import so values can be interpreted correctly.
#'
#' @param series Survey series acronym (e.g. `"SFS"`, `"Census"`), **or** a
#'   lazy `dplyr::tbl()` / DuckDB connection returned by [get_pumf()].  When a
#'   tbl or connection is supplied, `version`, `cache_path`, and `lang` are
#'   read from the connection provenance; explicit arguments take precedence.
#' @param version Version string (e.g. `"2019"`, `"2021 (individuals)"`).
#'   For LFS, omit to open documentation for the most recently downloaded
#'   version.  Ignored when `series` is a tbl or connection.
#' @param lang `"eng"` (default) or `"fra"`.  Documentation files whose names
#'   match the requested language are sorted first.  When `series` is a
#'   connection and `lang` is not supplied, the connection's language is used.
#' @param cache_path Root cache directory.  Defaults to
#'   `getOption("canpumf.cache_path", tempdir())`.
#' @param pumf_series Deprecated; use `series`.
#' @param pumf_version Deprecated; use `version`.
#' @param pumf_cache_path Deprecated; use `cache_path`.
#'
#' @return Invisibly, the file path(s) of the opened documentation, or
#'   `invisible(NULL)` when no documentation is found or data has not been
#'   downloaded yet.
#'
#' @seealso [get_pumf()], [pumf_metadata()]
#'
#' @examples
#' if (interactive()) {
#' # Open by series and version
#' open_pumf_documentation("SFS", "2019")
#'
#' # Open from an existing tbl (reads provenance automatically)
#' sfs <- get_pumf("SFS", "2019")
#' open_pumf_documentation(sfs)
#' close_pumf(sfs)
#'
#' # French documentation
#' open_pumf_documentation("SFS", "2019", lang = "fra")
#' }
#' @export
open_pumf_documentation <- function(series          = NULL,
                                     version         = NULL,
                                     lang            = NULL,
                                     cache_path      = getOption("canpumf.cache_path",
                                                                   tempdir()),
                                     pumf_series     = NULL,
                                     pumf_version    = NULL,
                                     pumf_cache_path = NULL) {

  explicit_lang <- !is.null(lang)

  if (!is.null(pumf_series)) {
    warning("'pumf_series' is deprecated; use 'series'.", call. = FALSE)
    if (is.null(series)) series <- pumf_series
  }
  if (!is.null(pumf_version)) {
    warning("'pumf_version' is deprecated; use 'version'.", call. = FALSE)
    if (is.null(version)) version <- pumf_version
  }
  if (!is.null(pumf_cache_path)) {
    warning("'pumf_cache_path' is deprecated; use 'cache_path'.", call. = FALSE)
    cache_path <- pumf_cache_path
  }

  # --- Resolve connection/tbl passed as series --------------------------------
  if (!is.null(series) && !is.character(series)) {
    con  <- if (inherits(series, "tbl_lazy")) series$src$con else series
    prov <- .pumf_lookup_con(con)
    if (is.null(prov))
      stop("Could not retrieve PUMF provenance from connection. ",
           "Was it created by get_pumf()?", call. = FALSE)
    if (is.null(version))  version    <- prov$version
    if (!explicit_lang)    lang       <- prov$lang %||% "eng"
    cache_path <- prov$cache_path
    series     <- prov$series
  }

  if (is.null(lang)) lang <- "eng"
  stopifnot(lang %in% c("eng", "fra"))
  if (is.null(series)) stop("'series' must be specified.")

  # --- LFS with no version: find most recently downloaded ----------------------
  if (series == "LFS" && is.null(version)) {
    version <- .pumf_lfs_latest_cached(cache_path)
    if (is.null(version)) {
      message("No LFS data has been downloaded yet. ",
              "Use get_pumf(\"LFS\", \"<version>\") to download first.")
      return(invisible(NULL))
    }
  }

  version_dir <- if (is.null(version)) file.path(cache_path, series)
                 else                  file.path(cache_path, series, version)

  if (!dir.exists(version_dir)) {
    message("No data found for ", series,
            if (!is.null(version)) paste0(" ", version), ". ",
            "Use get_pumf() or pumf_metadata() to download first.")
    return(invisible(NULL))
  }

  title    <- paste0(series, if (!is.null(version)) paste0(" ", version))
  reg      <- tryCatch(pumf_registry_lookup(series, version), error = function(e) NULL)
  doc_mask <- reg$doc_mask

  # --- Collect PDFs -----------------------------------------------------------
  docs <- .pumf_find_docs(version_dir, "\\.pdf$")

  if (length(docs) == 0L) {
    zip_path <- .find_version_zip(version_dir)
    if (!is.null(zip_path)) {
      zip_list <- tryCatch(utils::unzip(zip_path, list = TRUE), error = function(e) NULL)
      if (!is.null(zip_list)) {
        pdf_names <- zip_list$Name[grepl("\\.pdf$", zip_list$Name, ignore.case = TRUE)]
        if (length(pdf_names) > 0L) {
          docs_dir <- file.path(version_dir, "docs_extracted")
          dir.create(docs_dir, showWarnings = FALSE)
          utils::unzip(zip_path, files = pdf_names, exdir = docs_dir)
          docs <- .pumf_find_docs(docs_dir, "\\.pdf$")
        }
      }
    }
  }

  # Walk up to the year-level parent when the version dir has no PDFs.
  # Used by EFT Census vintages whose documentation sits in a shared
  # FMGD/ subdirectory one level above the version directories.
  if (length(docs) == 0L && !is.null(version)) {
    parent_dir <- dirname(version_dir)
    if (dir.exists(parent_dir) && parent_dir != cache_path) {
      parent_docs <- .pumf_find_docs(parent_dir, "\\.pdf$")
      # Drop files inside sibling version dirs (identified by having metadata/).
      if (length(parent_docs) > 0L)
        docs <- .pumf_drop_version_sibling_docs(parent_docs, parent_dir, version_dir)
    }
  }

  # Apply registry doc_mask to narrow to the relevant file-type docs
  # (e.g., families vs households vs individuals for 1986 Census).
  if (!is.null(doc_mask) && length(docs) > 0L) {
    filtered <- docs[grepl(doc_mask, basename(docs), ignore.case = TRUE)]
    if (length(filtered) > 0L) docs <- filtered
  }

  if (length(docs) > 0L) {
    docs <- .pumf_sort_by_lang(docs, lang)
    result <- .pumf_open_with_menu(docs, title)
    .pumf_emit_override_message(series, version)
    return(invisible(result))
  }

  # --- No PDFs: fall back to small text files ---------------------------------
  docs <- .pumf_find_docs(version_dir, "\\.(txt|rtf)$", max_size = 5e6)

  if (length(docs) == 0L) {
    zip_path <- .find_version_zip(version_dir)
    if (!is.null(zip_path)) {
      zip_list <- tryCatch(utils::unzip(zip_path, list = TRUE), error = function(e) NULL)
      if (!is.null(zip_list)) {
        txt_names <- zip_list$Name[
          grepl("\\.(txt|rtf)$", zip_list$Name, ignore.case = TRUE) &
          zip_list$Length < 5e6
        ]
        if (length(txt_names) > 0L) {
          docs_dir <- file.path(version_dir, "docs_extracted")
          dir.create(docs_dir, showWarnings = FALSE)
          utils::unzip(zip_path, files = txt_names, exdir = docs_dir)
          docs <- .pumf_find_docs(docs_dir, "\\.(txt|rtf)$", max_size = 5e6)
        }
      }
    }
  }

  if (length(docs) > 0L) {
    docs <- .pumf_sort_by_lang(docs, lang)
    result <- .pumf_open_with_menu(docs, title)
    .pumf_emit_override_message(series, version)
    return(invisible(result))
  }

  message("No documentation files found for ", title, ".")
  invisible(NULL)
}


# Find the most recently downloaded LFS version in the cache.
.pumf_lfs_latest_cached <- function(cache_path) {
  lfs_dir <- file.path(cache_path, "LFS")
  if (!dir.exists(lfs_dir)) return(NULL)
  subdirs <- list.dirs(lfs_dir, recursive = FALSE, full.names = FALSE)
  # Keep only version-like names: "YYYY" or "YYYY-MM"
  versions <- subdirs[grepl("^\\d{4}(-\\d{2})?$", subdirs)]
  if (length(versions) == 0L) return(NULL)
  # Only those that actually have extracted content
  versions <- versions[sapply(versions, function(v) {
    vd <- file.path(lfs_dir, v)
    length(list.files(vd)) > 0L
  })]
  if (length(versions) == 0L) return(NULL)
  # Sort: annual before monthly for same year; descending
  sort(versions, decreasing = TRUE)[[1L]]
}


# Scan a directory for documentation files matching ext_pat, excluding
# metadata/ and docs_extracted/ subdirs.  When max_size is given, files
# larger than that byte count are dropped (to exclude FWF data files).
.pumf_find_docs <- function(dir, ext_pat, max_size = NULL) {
  paths <- list.files(dir, pattern = ext_pat, recursive = TRUE,
                      full.names = TRUE, ignore.case = TRUE)
  paths <- paths[!grepl("/(metadata|docs_extracted)/", paths, ignore.case = TRUE)]
  if (!is.null(max_size)) {
    sizes <- file.size(paths)
    paths <- paths[!is.na(sizes) & sizes <= max_size]
  }
  paths
}


# Exclude docs that live inside sibling version directories (any direct child of
# parent_dir, other than current_version_dir, that has a metadata/ subdirectory).
.pumf_drop_version_sibling_docs <- function(paths, parent_dir, current_version_dir) {
  # normalizePath() returns backslash paths on Windows while .Platform$file.sep
  # is "/", so force winslash = "/" everywhere and use "/" as the separator —
  # otherwise the startsWith() prefix match below never fires on Windows.
  np <- function(x) normalizePath(x, winslash = "/", mustWork = FALSE)
  siblings <- list.dirs(parent_dir, recursive = FALSE, full.names = TRUE)
  current  <- np(current_version_dir)
  siblings <- siblings[np(siblings) != current]
  ver_sibs <- np(siblings[vapply(siblings,
    function(d) dir.exists(file.path(d, "metadata")), logical(1L))])
  if (length(ver_sibs) == 0L) return(paths)
  norm_paths <- np(paths)
  in_sib <- vapply(norm_paths, function(p)
    any(startsWith(p, paste0(ver_sibs, "/"))), logical(1L))
  paths[!in_sib]
}


# Score files by language preference: 2=explicit match, 1=neutral, 0=other lang.
.pumf_lang_score <- function(paths, lang) {
  bn <- tolower(basename(paths))
  # "_e." suffix (e.g. pumf1976rcl_e.pdf) and standard _eng/_en/english/anglais markers
  has_eng <- grepl(
    "(^|[_\\-.])(eng|en|english|anglais)([_\\-.]|$)|_e\\.",
    bn, perl = TRUE
  )
  # "_f." suffix (e.g. pumf1976rclv2_f.pdf), standard _fra/_fr/french/francais markers,
  # and "recensement" (French word for census, appears in StatCan French guide names)
  has_fra <- grepl(
    "(^|[_\\-.])(fra|fr|french|francais|fran.ais)([_\\-.]|$)|_f\\.|recensement",
    bn, perl = TRUE
  )
  if (lang == "eng") {
    ifelse(has_eng, 2L, ifelse(!has_fra, 1L, 0L))
  } else {
    ifelse(has_fra, 2L, ifelse(!has_eng, 1L, 0L))
  }
}


# Sort paths so preferred-language files come first (stable sort).
.pumf_sort_by_lang <- function(paths, lang) {
  scores <- .pumf_lang_score(paths, lang)
  paths[order(-scores, seq_along(paths))]
}


# Present an interactive selection menu (or open silently in batch mode).
# Last menu choice is always "Open all".
.pumf_open_with_menu <- function(paths, title) {
  if (length(paths) == 1L) {
    utils::browseURL(paths[[1L]])
    return(paths)
  }

  if (!interactive()) {
    utils::browseURL(paths[[1L]])
    return(paths[[1L]])
  }

  labels <- c(basename(paths), "Open all")
  choice <- utils::menu(labels, title = paste0("Documentation for ", title, ":"))

  if (choice == 0L) return(character(0L))

  if (choice == length(labels)) {
    lapply(paths, utils::browseURL)
    paths
  } else {
    utils::browseURL(paths[[choice]])
    paths[[choice]]
  }
}


# Emit a human-readable message describing registry overrides for the survey.
.pumf_emit_override_message <- function(series, version) {
  if (is.null(version)) return(invisible(NULL))
  reg <- tryCatch(pumf_registry_lookup(series, version), error = function(e) NULL)
  if (is.null(reg)) return(invisible(NULL))

  lines <- character(0L)
  fx    <- reg$data_fixups

  if (length(fx$na_values) > 0L)
    lines <- c(lines, paste0(
      "  NA values: raw values ",
      paste(fx$na_values, collapse = ", "),
      " are treated as missing in all numeric columns."
    ))

  if (length(fx$force_numeric) > 0L)
    lines <- c(lines, paste0(
      "  Forced numeric: ",
      paste(fx$force_numeric, collapse = ", "),
      " \u2014 boundary/top-code labels dropped; sentinel codes become NA ranges."
    ))

  if (length(fx$cols_swap) > 0L) {
    pairs <- paste0(names(fx$cols_swap), "\u2194", fx$cols_swap)
    lines <- c(lines, paste0(
      "  Column name swap: ", paste(pairs, collapse = ", "),
      " (command-file labels were transposed relative to data)."
    ))
  }

  if (length(fx$rename) > 0L) {
    pairs <- paste0(names(fx$rename), "\u2192", fx$rename)
    lines <- c(lines, paste0("  Renamed columns: ", paste(pairs, collapse = ", "), "."))
  }

  if (length(fx$codes_supplement) > 0L)
    lines <- c(lines, paste0(
      "  Extra codes injected for: ",
      paste(names(fx$codes_supplement), collapse = ", "), "."
    ))

  if (length(reg$missing_supplement) > 0L)
    lines <- c(lines, paste0(
      "  Missing-range overrides applied to: ",
      paste(names(reg$missing_supplement), collapse = ", "), "."
    ))

  if (length(lines) == 0L) return(invisible(NULL))

  message("Data import notes for ", series, " ", version, ":\n",
          paste(lines, collapse = "\n"))
  invisible(NULL)
}
