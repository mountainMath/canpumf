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
#'   The aliases [get_pumf()] accepts work here too (`"2021"` for the Census
#'   individuals file, `"Cycle 31"` or `"2017"` for a GSS cycle).
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
  # Same aliases as get_pumf() ("2021" -> "2021 (individuals)", GSS cycles).
  version <- pumf_resolve_version(series, version, cache_path)

  # --- Longitudinal series: most recently downloaded slice --------------------
  # With no version, the latest slice in the cache; LFS_HIST keeps months
  # only, so a year resolves to its latest cached month.
  if (.is_longitudinal(series) &&
      (is.null(version) ||
       !dir.exists(file.path(cache_path, series, version)))) {
    requested <- version
    version <- .pumf_lfs_latest_cached(cache_path, series, prefix = requested)
    if (is.null(version)) {
      message("No ", series, " data has been downloaded yet",
              if (!is.null(requested)) paste0(" for ", requested), ". ",
              "Use get_pumf(\"", series, "\", \"<version>\") to download first.")
      return(invisible(NULL))
    }
  }

  version_dir <- if (is.null(version)) file.path(cache_path, series)
                 else                  file.path(cache_path, series, version)

  if (!dir.exists(version_dir)) {
    cached <- .pumf_cached_versions(cache_path, series)
    message("No data found for ", series,
            if (!is.null(version)) paste0(" ", version), ". ",
            "Use get_pumf() or pumf_metadata() to download first.",
            if (!is.null(version) && length(cached) > 0L)
              paste0("\nCached ", series, " versions: ",
                     paste0('"', cached, '"', collapse = ", "), "."))
    return(invisible(NULL))
  }

  title <- paste0(series, if (!is.null(version)) paste0(" ", version))
  reg   <- tryCatch(pumf_registry_lookup(series, version), error = function(e) NULL)

  # PDFs first; with none, the small text files (the size cap excludes FWF
  # data files).  Each kind is looked for in the version directory, then in
  # the retained zip.
  kinds <- list(pdf = list(ext = "\\.pdf$",       max_size = NULL),
                txt = list(ext = "\\.(txt|rtf)$", max_size = 5e6))
  for (kind in names(kinds)) {
    k    <- kinds[[kind]]
    docs <- .pumf_find_docs(version_dir, k$ext, max_size = k$max_size)
    if (length(docs) == 0L)
      docs <- .pumf_extract_zip_docs(version_dir, k$ext, max_size = k$max_size)

    if (kind == "pdf") {
      # Walk up to the year-level parent when the version dir has no PDFs.
      # Used by EFT Census vintages whose documentation sits in a shared
      # FMGD/ subdirectory one level above the version directories.
      if (length(docs) == 0L && !is.null(version)) {
        parent_dir <- dirname(version_dir)
        if (dir.exists(parent_dir) && parent_dir != cache_path) {
          parent_docs <- .pumf_find_docs(parent_dir, k$ext)
          # Drop files inside sibling version dirs (identified by having metadata/).
          if (length(parent_docs) > 0L)
            docs <- .pumf_drop_version_sibling_docs(parent_docs, parent_dir, version_dir)
        }
      }
      # Apply registry doc_mask to narrow to the relevant file-type docs
      # (e.g., families vs households vs individuals for 1986 Census).
      if (!is.null(reg$doc_mask) && length(docs) > 0L) {
        filtered <- docs[grepl(reg$doc_mask, basename(docs), ignore.case = TRUE)]
        if (length(filtered) > 0L) docs <- filtered
      }
    }

    if (length(docs) > 0L) {
      docs <- .pumf_sort_by_lang(docs, lang)
      result <- .pumf_open_with_menu(docs, title)
      .pumf_emit_override_message(series, version, reg)
      return(invisible(result))
    }
  }

  message("No documentation files found for ", title, ".")
  invisible(NULL)
}


# Extract the documentation files matching `ext_pat` (and, with `max_size`,
# smaller than that many bytes) from the version's retained zip into
# docs_extracted/, and return their paths; character(0) when there is no zip
# or no such file.
.pumf_extract_zip_docs <- function(version_dir, ext_pat, max_size = NULL) {
  zip_path <- .find_version_zip(version_dir)
  if (is.null(zip_path)) return(character(0L))
  zip_list <- tryCatch(utils::unzip(zip_path, list = TRUE), error = function(e) NULL)
  if (is.null(zip_list)) return(character(0L))
  keep <- grepl(ext_pat, zip_list$Name, ignore.case = TRUE)
  if (!is.null(max_size)) keep <- keep & zip_list$Length < max_size
  names <- zip_list$Name[keep]
  if (length(names) == 0L) return(character(0L))
  docs_dir <- file.path(version_dir, "docs_extracted")
  dir.create(docs_dir, showWarnings = FALSE)
  utils::unzip(zip_path, files = names, exdir = docs_dir)
  .pumf_find_docs(docs_dir, ext_pat, max_size = max_size)
}


# Versions of a series with content in the cache, for the hint given when the
# requested version is not there.
.pumf_cached_versions <- function(cache_path, series) {
  sort(.pumf_version_dirs(cache_path, series))
}


# Find the most recently downloaded version of a longitudinal series in the
# cache ("YYYY" or "YYYY-MM" directories with content), optionally among those
# starting with `prefix` (a year).  Sorting descending puts an annual version
# before the months of the same year.
.pumf_lfs_latest_cached <- function(cache_path, series = "LFS", prefix = NULL) {
  versions <- .pumf_version_dirs(cache_path, series, pattern = "^\\d{4}(-\\d{2})?$")
  if (!is.null(prefix)) versions <- versions[startsWith(versions, prefix)]
  if (length(versions) == 0L) return(NULL)
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


# One human-readable note per data_fixups field, in the order the import notes
# list them.  Each function receives the fixup's value (never NULL) and returns
# the line, or character(0) when the value says nothing (FALSE, empty).  Every
# field of .pumf_fixup_fields has a note (test-pumf-documentation.R).
.pumf_fixup_notes <- list(
  na_values = function(v) if (length(v) > 0L) paste0(
    "  NA values: raw values ", paste(v, collapse = ", "),
    " are treated as missing in all numeric columns."),
  force_numeric = function(v) if (length(v) > 0L) paste0(
    "  Forced numeric: ", paste(v, collapse = ", "),
    " \u2014 boundary/top-code labels dropped; sentinel codes become NA ranges."),
  force_character = function(v) if (length(v) > 0L) paste0(
    "  Kept as text (leading zeros preserved, no labels): ",
    paste(v, collapse = ", "), "."),
  force_integer = function(v) if (length(v) > 0L) paste0(
    "  Stored as INTEGER: ", paste(v, collapse = ", "), "."),
  force_bigint = function(v) if (length(v) > 0L) paste0(
    "  Stored as BIGINT: ", paste(v, collapse = ", "), "."),
  cols_swap = function(v) if (length(v) > 0L) paste0(
    "  Column name swap: ", paste(paste0(names(v), "\u2194", v), collapse = ", "),
    " (command-file labels were transposed relative to data)."),
  rename = function(v) if (length(v) > 0L) paste0(
    "  Renamed columns: ", paste(paste0(names(v), "\u2192", v), collapse = ", "), "."),
  rename_regex = function(v) if (length(v) > 0L) paste0(
    "  Column names rewritten by pattern: ",
    paste(paste0(names(v), "\u2192", v), collapse = ", "),
    " (applied only where the result is a documented variable name)."),
  str_pad = function(v) if (length(v) > 0L) paste0(
    "  Raw values padded to a fixed width: ",
    paste(vapply(v, function(s)
      paste0(paste(s$cols, collapse = ", "), " (", s$width, ")"), ""),
      collapse = "; "), "."),
  codes_supplement = function(v) if (length(v) > 0L) paste0(
    "  Extra codes injected for: ", paste(names(v), collapse = ", "), "."),
  codes_override = function(v) if (length(v) > 0L) paste0(
    "  Code labels replaced from the user guide for: ",
    paste(names(v), collapse = ", "), "."),
  missing_supplement = function(v) if (length(v) > 0L) paste0(
    "  Missing-range overrides applied to: ", paste(names(v), collapse = ", "), "."),
  missing_codes = function(v) if (length(v) > 0L) paste0(
    "  Missing values declared as discrete codes (not a range) for: ",
    paste(names(v), collapse = ", "), "."),
  sentinel_labels = function(v) if (length(v) > 0L) paste0(
    "  Labels supplied for unlabelled sentinel codes (",
    paste(names(v), collapse = ", "), "); see pumf_sidecar(tbl, \"sentinels\")."),
  fix_mojibake = function(v) if (isTRUE(v))
    "  Mis-encoded accented text (\"Qu\u00c3\u00a9bec\") repaired in the character columns.",
  removed_records = function(v) paste0(
    "  Records with ", v$var, " = ", paste(v$values, collapse = ", "),
    " are set aside; see pumf_sidecar(tbl, \"removed\")."),
  keep_unlabelled_codes = function(v) if (isTRUE(v) || length(v) > 0L) paste0(
    "  Codes without a documented label are kept under the code itself",
    if (is.character(v)) paste0(" for: ", paste(v, collapse = ", ")), "."),
  rejoin_split_records = function(v) if (isTRUE(v))
    "  Records split over two lines by a line break inside a field are rejoined.",
  column_encoding = function(v) if (length(v) > 0L) paste0(
    "  Columns decoded with their own code page: ",
    paste(vapply(names(v), function(e)
      paste0(paste(v[[e]], collapse = ", "), " (", e, ")"), ""), collapse = "; "), "."),
  text_missing_codes = function(v) if (length(v) > 0L) paste0(
    "  Missing codes in text columns (", paste(v, collapse = ", "),
    ") become NA; see pumf_sidecar(tbl, \"sentinels\")."),
  labels_as_description = function(v) if (isTRUE(v)) paste0(
    "  The source's variable labels are sentences; they are kept as the ",
    "variable descriptions (pumf_dictionary(tbl, what = \"variables\"))."),
  labels_supplement = function(v) if (length(v) > 0L) paste0(
    "  Variable labels supplied by canpumf where the source metadata has none: ",
    length(v), " variable", if (length(v) != 1L) "s", ".")
)

# Emit a human-readable message describing registry overrides for the survey.
# `reg` is the entry open_pumf_documentation() already looked up; by default
# it is looked up here.
.pumf_emit_override_message <- function(series, version,
                                        reg = tryCatch(pumf_registry_lookup(series, version),
                                                       error = function(e) NULL)) {
  if (is.null(version)) return(invisible(NULL))
  if (is.null(reg)) return(invisible(NULL))

  fx    <- reg$data_fixups
  lines <- unlist(lapply(names(.pumf_fixup_notes), function(f) {
    # [[f]]: `$` would partial-match rename_regex on entries that declare
    # only the regex form.
    v <- fx[[f]]
    if (is.null(v)) character(0L) else .pumf_fixup_notes[[f]](v)
  }))

  if (length(lines) == 0L) return(invisible(NULL))

  message("Data import notes for ", series, " ", version, ":\n",
          paste(lines, collapse = "\n"))
  invisible(NULL)
}
