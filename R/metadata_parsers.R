
# Canonical metadata schema
# variables.csv: name (chr), label (chr), type (chr: "character"|"numeric"),
#                missing_low (dbl|NA), missing_high (dbl|NA)
# codes.csv:     name (chr), val (chr), label (chr)
# layout.csv:    name (chr), start (int), end (int)   [only for fixed-width data]

.metadata_variables_cols <- readr::cols(
  name        = readr::col_character(),
  label       = readr::col_character(),
  type        = readr::col_character(),
  missing_low = readr::col_double(),
  missing_high= readr::col_double()
)

.metadata_codes_cols <- readr::cols(
  name  = readr::col_character(),
  val   = readr::col_character(),
  label = readr::col_character()
)

.metadata_layout_cols <- readr::cols(
  name  = readr::col_character(),
  start = readr::col_integer(),
  end   = readr::col_integer()
)

empty_variables <- function() {
  tibble::tibble(name=character(), label=character(), type=character(),
                 missing_low=double(), missing_high=double())
}

empty_codes <- function() {
  tibble::tibble(name=character(), val=character(), label=character())
}

empty_layout <- function() {
  tibble::tibble(name=character(), start=integer(), end=integer())
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

  readr::write_csv(metadata$variables, file.path(metadata_dir, "variables.csv"),
                   na = "")
  readr::write_csv(metadata$codes, file.path(metadata_dir, "codes.csv"),
                   na = "")
  if (!is.null(metadata$layout) && nrow(metadata$layout) > 0)
    readr::write_csv(metadata$layout, file.path(metadata_dir, "layout.csv"), na = "")

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
    attr(df, "spec") <- NULL
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

  check_cols(metadata$variables, c("name","label","type","missing_low","missing_high"),
             "variables")
  check_cols(metadata$codes, c("name","val","label"), "codes")

  bad_types <- setdiff(unique(metadata$variables$type), c("character","numeric",NA_character_))
  if (length(bad_types) > 0)
    warning("variables$type contains unexpected values: ", paste(bad_types, collapse = ", "))

  if (!is.null(metadata$layout))
    check_cols(metadata$layout, c("name","start","end"), "layout")

  invisible(NULL)
}
