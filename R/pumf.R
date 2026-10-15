# Read raw PUMF data as an in-memory tibble (internal).
# For manually-deposited PUMF directories outside the standard cache structure.
# Use get_pumf() for all registry-supported surveys.
#' @keywords internal
read_pumf_data <- function(pumf_base_path,
                           layout_mask   = NULL,
                           file_mask     = layout_mask,
                           guess_numeric = TRUE) {
  if (!metadata_exists(pumf_base_path)) {
    tryCatch(
      pumf_parse_metadata(pumf_base_path, layout_mask = layout_mask),
      error = function(e)
        stop("Could not parse metadata in ", pumf_base_path, ": ", e$message)
    )
  }

  meta      <- read_metadata(file.path(pumf_base_path, "metadata"))
  is_fwf    <- !is.null(meta$layout)
  enc       <- "CP1252"
  data_path <- tryCatch(
    .find_pumf_data_file(pumf_base_path, file_mask, is_fwf),
    error = function(e)
      stop("Could not find data file in ", pumf_base_path, ": ", e$message)
  )

  pumf_data <- .pumf_read_chr(data_path, enc,
                              layout = if (is_fwf) meta$layout else NULL)

  if (guess_numeric)
    pumf_data <- .apply_numeric_conversion(pumf_data, meta$variables)

  attr(pumf_data, "pumf_base_path") <- pumf_base_path
  attr(pumf_data, "layout_mask")    <- layout_mask
  pumf_data
}


