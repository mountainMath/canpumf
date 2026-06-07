#' Open PUMF documentation in browser
#'
#' Scans the cached version directory for PDF and TXT documentation files
#' and opens matching files in the default browser.  If no files are found in
#' the extracted content, the zip is inspected and documentation files are
#' extracted on demand into a `docs_extracted/` subdirectory.
#'
#' @param series Survey series acronym, e.g. `"SFS"`, `"Census"`.
#' @param version Version string, e.g. `"2019"`, `"2021 (individuals)"`.
#'   If `NULL`, searches the series directory directly (for series with a
#'   single version).
#' @param documentation_type Which type of documentation to open.  One of
#'   `"user_guide"` (default), `"reference_guide"`, `"questionnaire"`,
#'   `"quality"`, or `"errata"`.  When multiple files match the type pattern
#'   those all opened.  When only one file exists the filter is skipped.
#' @param cache_path Root cache directory.  Defaults to
#'   `getOption("canpumf.cache_path", tempdir())`.
#' @param pumf_series Deprecated; use `series`.
#' @param pumf_version Deprecated; use `version`.
#' @param pumf_cache_path Deprecated; use `cache_path`.
#'
#' @return Invisibly, the paths of the opened documentation files.
#' @export
open_pumf_documentation <- function(series           = NULL,
                                     version          = NULL,
                                     documentation_type = "user_guide",
                                     cache_path       = getOption("canpumf.cache_path",
                                                                   tempdir()),
                                     pumf_series      = NULL,
                                     pumf_version     = NULL,
                                     pumf_cache_path  = NULL) {
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
  if (is.null(series))
    stop("'series' must be specified.")

  version_dir <- if (is.null(version)) file.path(cache_path, series)
                 else                  file.path(cache_path, series, version)

  if (!dir.exists(version_dir))
    stop("Version directory does not exist: ", version_dir,
         ".\nDownload data first with get_pumf() or pumf_metadata().")

  # --- Keyword patterns for each documentation type --------------------------
  type_patterns <- c(
    user_guide      = "User.?Guide|Guide.utilisateur|UserGuide",
    reference_guide = "Reference|r\u00e9f\u00e9rence|Ref.?Guide",
    questionnaire   = "Questionnaire",
    quality         = "Quality|qualit\u00e9|Quality.?Guide",
    errata          = "errata|corrigenda"
  )

  # --- Scan extracted content recursively ------------------------------------
  docs <- list.files(version_dir, pattern = "\\.(pdf|txt)$",
                     recursive = TRUE, full.names = TRUE,
                     ignore.case = TRUE)
  # Exclude metadata/ and docs_extracted/ from the search
  docs <- docs[!grepl("/(metadata|docs_extracted)/", docs)]

  # --- Fall back to zip contents when nothing is found -----------------------
  if (length(docs) == 0L) {
    zip_path <- .find_version_zip(version_dir)
    if (!is.null(zip_path)) {
      zip_list <- tryCatch(utils::unzip(zip_path, list = TRUE),
                           error = function(e) NULL)
      if (!is.null(zip_list)) {
        doc_names <- zip_list$Name[grepl("\\.(pdf|txt)$", zip_list$Name,
                                          ignore.case = TRUE)]
        if (length(doc_names) > 0L) {
          docs_dir <- file.path(version_dir, "docs_extracted")
          dir.create(docs_dir, showWarnings = FALSE)
          utils::unzip(zip_path, files = doc_names, exdir = docs_dir)
          docs <- list.files(docs_dir, pattern = "\\.(pdf|txt)$",
                             recursive = TRUE, full.names = TRUE,
                             ignore.case = TRUE)
        }
      }
    }
  }

  if (length(docs) == 0L)
    stop("No documentation files found for ", series,
         if (!is.null(version)) paste0(" ", version), ".")

  # --- Filter by documentation_type ------------------------------------------
  pat <- type_patterns[documentation_type]
  if (!is.na(pat) && length(docs) > 1L) {
    filtered <- docs[grepl(pat, basename(docs), ignore.case = TRUE)]
    if (length(filtered) > 0L) docs <- filtered
  }

  lapply(docs, utils::browseURL)
  invisible(docs)
}
