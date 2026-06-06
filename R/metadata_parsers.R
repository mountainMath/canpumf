
# Canonical metadata schema
# variables.csv: name (chr), label_en (chr), label_fr (chr|NA),
#                type (chr: "character"|"numeric"),
#                missing_low (dbl|NA), missing_high (dbl|NA)
# codes.csv:     name (chr), val (chr), label_en (chr), label_fr (chr|NA)
# layout.csv:    name (chr), start (int), end (int)   [only for fixed-width data]

.metadata_variables_cols <- readr::cols(
  name         = readr::col_character(),
  label_en     = readr::col_character(),
  label_fr     = readr::col_character(),
  type         = readr::col_character(),
  missing_low  = readr::col_double(),
  missing_high = readr::col_double()
)

.metadata_codes_cols <- readr::cols(
  name     = readr::col_character(),
  val      = readr::col_character(),
  label_en = readr::col_character(),
  label_fr = readr::col_character()
)

.metadata_layout_cols <- readr::cols(
  name  = readr::col_character(),
  start = readr::col_integer(),
  end   = readr::col_integer()
)

empty_variables <- function() {
  tibble::tibble(name = character(), label_en = character(), label_fr = character(),
                 type = character(), missing_low = double(), missing_high = double())
}

empty_codes <- function() {
  tibble::tibble(name = character(), val = character(),
                 label_en = character(), label_fr = character())
}

empty_layout <- function() {
  tibble::tibble(name = character(), start = integer(), end = integer())
}


#' Write canonical PUMF metadata to CSV files
#'
#' @param metadata A list with elements \code{variables}, \code{codes}, and
#'   optionally \code{layout}. Each is a tibble matching the canonical schema.
#' @param metadata_dir Path to the \code{metadata/} directory; created if absent.
#' @return \code{metadata_dir} invisibly.
#' @keywords internal
write_metadata <- function(metadata, metadata_dir) {
  if (!dir.exists(metadata_dir)) dir.create(metadata_dir, recursive = TRUE)

  validate_metadata(metadata)
  check_bilingual_coverage(metadata)

  readr::write_csv(metadata$variables, file.path(metadata_dir, "variables.csv"), na = "")
  readr::write_csv(metadata$codes,     file.path(metadata_dir, "codes.csv"),     na = "")
  if (!is.null(metadata$layout) && nrow(metadata$layout) > 0)
    readr::write_csv(metadata$layout,  file.path(metadata_dir, "layout.csv"),    na = "")

  invisible(metadata_dir)
}


#' Read canonical PUMF metadata from CSV files
#'
#' @param metadata_dir Path to the \code{metadata/} directory.
#' @return A list with elements \code{variables}, \code{codes}, and \code{layout}
#'   (NULL when no \code{layout.csv} is present).
#' @keywords internal
read_metadata <- function(metadata_dir) {
  vars_path   <- file.path(metadata_dir, "variables.csv")
  codes_path  <- file.path(metadata_dir, "codes.csv")
  layout_path <- file.path(metadata_dir, "layout.csv")

  if (!file.exists(vars_path))  stop("metadata/variables.csv not found in ", metadata_dir)
  if (!file.exists(codes_path)) stop("metadata/codes.csv not found in ", metadata_dir)

  strip_spec <- function(df) {
    attr(df, "spec")     <- NULL
    attr(df, "problems") <- NULL
    class(df) <- c("tbl_df", "tbl", "data.frame")
    df
  }

  variables <- strip_spec(readr::read_csv(vars_path,  col_types = .metadata_variables_cols,
                                          show_col_types = FALSE))
  codes     <- strip_spec(readr::read_csv(codes_path, col_types = .metadata_codes_cols,
                                          show_col_types = FALSE))
  layout    <- if (file.exists(layout_path))
    strip_spec(readr::read_csv(layout_path, col_types = .metadata_layout_cols,
                               show_col_types = FALSE))
  else
    NULL

  list(variables = variables, codes = codes, layout = layout)
}


#' Check whether canonical metadata exists for a version directory
#'
#' @param version_dir Path to the version directory.
#' @return Logical.
#' @keywords internal
metadata_exists <- function(version_dir) {
  file.exists(file.path(version_dir, "metadata", "variables.csv"))
}


#' Select language-specific labels from a metadata list
#'
#' Picks \code{label_en} or \code{label_fr} based on \code{lang} and adds a
#' \code{label} column to \code{variables} and \code{codes}. When
#' \code{lang = "fra"} and individual \code{label_fr} values are \code{NA},
#' those entries fall back to \code{label_en} and a warning is emitted listing
#' the affected variable names.
#'
#' @param metadata List from \code{read_metadata()}.
#' @param lang \code{"eng"} (default) or \code{"fra"}.
#' @return Modified metadata list with an additional \code{label} column in
#'   \code{variables} and \code{codes}.
#' @keywords internal
select_labels <- function(metadata, lang = "eng") {
  label_col <- if (lang == "eng") "label_en" else "label_fr"

  vars  <- metadata$variables
  codes <- metadata$codes

  if (lang == "fra") {
    na_var_names <- vars$name[is.na(vars$label_fr)]
    if (length(na_var_names) > 0) {
      warning("French variable labels unavailable for: ",
              paste(na_var_names, collapse = ", "), "; falling back to English.")
      vars$label_fr <- ifelse(is.na(vars$label_fr), vars$label_en, vars$label_fr)
    }

    na_code_vars <- unique(codes$name[is.na(codes$label_fr)])
    if (length(na_code_vars) > 0) {
      warning("French code labels unavailable for: ",
              paste(na_code_vars, collapse = ", "), "; falling back to English.")
      codes$label_fr <- ifelse(is.na(codes$label_fr), codes$label_en, codes$label_fr)
    }
  }

  vars$label  <- vars[[label_col]]
  codes$label <- codes[[label_col]]

  list(variables = vars, codes = codes, layout = metadata$layout)
}


#' Warn when French label coverage is partial
#'
#' Emits a warning when French files were clearly present (some \code{label_fr}
#' are non-NA) but more than \code{threshold} of variable labels are missing.
#' Surveys with no French files at all (all \code{label_fr} are \code{NA})
#' do not trigger the warning.
#'
#' @param metadata List from \code{read_metadata()}.
#' @param threshold Proportion of missing \code{label_fr} above which a warning
#'   is emitted (default 0.2).
#' @return \code{NULL} invisibly.
#' @keywords internal
check_bilingual_coverage <- function(metadata, threshold = 0.2) {
  n <- nrow(metadata$variables)
  if (n == 0) return(invisible(NULL))

  n_missing <- sum(is.na(metadata$variables$label_fr))
  frac      <- n_missing / n

  # Only warn when French is partially available — complete absence is expected
  if (frac > 0 && frac < 1 && frac > threshold) {
    warning(sprintf(
      "%.0f%% of variable labels have no French translation (%d of %d variables).",
      frac * 100, n_missing, n
    ))
  }
  invisible(NULL)
}


# Internal: validate a metadata list against the canonical schema.
validate_metadata <- function(metadata) {
  required <- c("variables", "codes")
  missing_elements <- setdiff(required, names(metadata))
  if (length(missing_elements) > 0)
    stop("metadata list is missing elements: ", paste(missing_elements, collapse = ", "))

  check_cols <- function(df, expected_names, label) {
    missing_cols <- setdiff(expected_names, names(df))
    if (length(missing_cols) > 0)
      stop(label, " is missing columns: ", paste(missing_cols, collapse = ", "))
  }

  check_cols(metadata$variables,
             c("name", "label_en", "label_fr", "type", "missing_low", "missing_high"),
             "variables")
  check_cols(metadata$codes, c("name", "val", "label_en", "label_fr"), "codes")

  bad_types <- setdiff(unique(metadata$variables$type), c("character", "numeric", NA_character_))
  if (length(bad_types) > 0)
    warning("variables$type contains unexpected values: ", paste(bad_types, collapse = ", "))

  if (!is.null(metadata$layout))
    check_cols(metadata$layout, c("name", "start", "end"), "layout")

  invisible(NULL)
}
