
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


# ============================================================
# SPSS monolithic parser
# ============================================================

#' Parse a monolithic SPSS command file for PUMF metadata
#'
#' Handles Census-style monolithic SPSS files where a single \code{.sps} file
#' contains \code{DATA LIST}, \code{FORMATS}, \code{VARIABLE LABELS},
#' \code{VALUE LABELS}, and optionally \code{MISSING VALUES} sections. Supports
#' both the 2021 style (single-quoted labels, \code{+} continuation, \code{/VAR}
#' headers in \code{VALUE LABELS}) and the 2016 style (double-quoted labels,
#' \code{/} on its own line separating variable groups).
#'
#' @param eng_sps_path Path to the English \code{.sps} file.
#' @param fra_sps_path Optional path to the French \code{.sps} file. When
#'   provided, French labels are joined onto the canonical metadata. When
#'   \code{NULL} (default), all \code{label_fr} values are \code{NA}.
#' @param encoding Character encoding of the file(s), e.g. \code{"Latin1"},
#'   \code{"CP1252"}, or \code{"UTF-8"}.
#' @return A list with elements \code{variables}
#'   (\code{name, label_en, label_fr, type, missing_low, missing_high}),
#'   \code{codes} (\code{name, val, label_en, label_fr}), and \code{layout}
#'   (\code{name, start, end}; \code{NULL} when no \code{DATA LIST} section is
#'   present).
#' @keywords internal
parse_spss_mono <- function(eng_sps_path, fra_sps_path = NULL, encoding = "Latin1") {
  eng <- .spss_mono_single(eng_sps_path, encoding)

  if (!is.null(fra_sps_path)) {
    fra <- .spss_mono_single(fra_sps_path, encoding)
    variables <- dplyr::left_join(
      dplyr::rename(eng$variables, label_en = "label"),
      dplyr::select(fra$variables, "name", label_fr = "label"),
      by = "name"
    )
    codes <- dplyr::left_join(
      dplyr::rename(eng$codes, label_en = "label"),
      dplyr::select(fra$codes, "name", "val", label_fr = "label"),
      by = c("name", "val")
    )
  } else {
    variables <- dplyr::mutate(
      dplyr::rename(eng$variables, label_en = "label"),
      label_fr = NA_character_
    )
    codes <- dplyr::mutate(
      dplyr::rename(eng$codes, label_en = "label"),
      label_fr = NA_character_
    )
  }

  variables <- variables[, c("name", "label_en", "label_fr", "type",
                              "missing_low", "missing_high")]
  codes     <- codes[, c("name", "val", "label_en", "label_fr")]

  list(variables = variables, codes = codes, layout = eng$layout)
}


# Internal: parse a single-language SPSS monolithic file.
# Returns list(variables=tibble(name,label,type,missing_low,missing_high),
#              codes=tibble(name,val,label), layout=tibble|NULL).
.spss_mono_single <- function(sps_path, encoding) {
  spss <- .spss_read_preprocess(sps_path, encoding)

  # Section keyword positions (trimmed lines, trimmed to start of line)
  is_kw <- function(pattern) which(grepl(pattern, spss$clean, perl = TRUE))

  var_labels_pos  <- is_kw("^VARIABLE LABELS\\s*$")
  val_labels_pos  <- is_kw("^VALUE LABELS\\s*$")
  formats_pos     <- is_kw("^FORMATS\\s*$|^VARIABLE FORMATS\\s*$")
  missing_pos     <- is_kw("^MISSING VALUES\\s*$")
  data_list_pos   <- is_kw("^DATA LIST")
  period_pos      <- is_kw("^\\.\\s*$")
  # Any of these marks the end of the previous section
  all_landmarks   <- sort(c(var_labels_pos, val_labels_pos, formats_pos,
                             missing_pos, data_list_pos, period_pos,
                             nrow(spss) + 1L))

  # Get content lines for a section that starts at `start`
  section_lines <- function(start) {
    end <- min(all_landmarks[all_landmarks > start]) - 1L
    if (end < start + 1L) return(character(0))
    spss$clean[seq(start + 1L, end)]
  }

  # ---- VARIABLE LABELS ----
  variable_labels <- tibble::tibble(name = character(), label = character())
  if (length(var_labels_pos) > 0)
    variable_labels <- .spss_parse_var_labels(section_lines(var_labels_pos[1]))

  # ---- VALUE LABELS ----
  codes <- tibble::tibble(name = character(), val = character(), label = character())
  if (length(val_labels_pos) > 0)
    codes <- .spss_parse_val_labels(section_lines(val_labels_pos[1]))

  # ---- FORMATS ----
  formats <- tibble::tibble(name = character(), fmt_type = character())
  if (length(formats_pos) > 0)
    formats <- .spss_parse_formats(section_lines(formats_pos[1]))

  # ---- MISSING VALUES ----
  missing_vals <- tibble::tibble(name = character(),
                                 missing_low = double(), missing_high = double())
  if (length(missing_pos) > 0)
    missing_vals <- .spss_parse_missing(section_lines(missing_pos[1]))

  # ---- DATA LIST (layout) ----
  layout <- NULL
  if (length(data_list_pos) > 0)
    layout <- .spss_parse_data_list(spss$clean, data_list_pos[1])

  # ---- Combine into variables tibble ----
  vars_with_codes <- unique(codes$name)

  variables <- variable_labels |>
    dplyr::left_join(formats,      by = "name") |>
    dplyr::left_join(missing_vals, by = "name") |>
    dplyr::mutate(
      type = dplyr::case_when(
        .data$name %in% vars_with_codes              ~ "character",
        !is.na(.data$missing_low)                    ~ "numeric",
        toupper(substr(.data$fmt_type, 1L, 1L)) == "A" ~ "character",
        !is.na(.data$fmt_type)                       ~ "numeric",
        TRUE                                         ~ "character"
      )
    ) |>
    dplyr::select("name", "label", "type", "missing_low", "missing_high")

  list(variables = variables, codes = codes, layout = layout)
}


# Read and pre-process an SPSS file: tab→space, strip trailing whitespace,
# join single-quote + continuation lines, normalize single→double quotes.
# Returns a tibble with 'value' (original) and 'clean' (processed) columns.
.spss_read_preprocess <- function(path, encoding) {
  raw <- readr::read_lines(path, locale = readr::locale(encoding = encoding))

  lines <- gsub("\\t", " ", raw)
  lines <- gsub("\\s+$", "", lines)

  # Join string continuations: 'text' + 'more' → 'textmore'
  cont <- which(grepl("'\\s*\\+\\s*$", lines))
  if (length(cont) > 0) {
    for (i in rev(cont)) {
      if (i + 1L <= length(lines)) {
        lines[i] <- paste0(sub("'\\s*\\+\\s*$", "", lines[i]),
                           sub("^\\s*'", "", lines[i + 1L]))
        lines <- lines[-(i + 1L)]
      }
    }
  }

  # Normalize single-quoted label strings → double quotes.
  # A line uses single-quote labels if it ends with '...' and has no double quotes.
  sq <- grepl("'[^']*'\\s*\\.?\\s*$", lines) &
        !grepl('"', lines, fixed = TRUE) &
        !grepl("^\\s*/\\*", lines)            # skip comments
  lines[sq] <- gsub("'", '"', lines[sq])

  # Strip trailing ' .' that some 2021-style lines use as section terminators
  lines <- gsub('"\\s*\\.$', '"', lines)

  tibble::tibble(clean = trimws(lines))
}


# Parse VARIABLE LABELS section content lines → tibble(name, label).
.spss_parse_var_labels <- function(lines) {
  lines <- lines[nchar(trimws(lines)) > 0]
  if (length(lines) == 0)
    return(tibble::tibble(name = character(), label = character()))

  tibble::tibble(value = trimws(lines)) |>
    dplyr::mutate(
      name  = sub("\\s.*", "", .data$value),
      label = {
        m <- stringr::str_match(.data$value, '"([^"]+)"')
        dplyr::if_else(!is.na(m[, 1L]), m[, 2L], sub("^[A-Za-z0-9_]+\\s+", "", .data$value))
      }
    ) |>
    dplyr::filter(grepl("^[A-Za-z][A-Za-z0-9_]*$", .data$name),
                  nchar(trimws(.data$label)) > 0) |>
    dplyr::select("name", "label")
}


# Parse VALUE LABELS section content lines → tibble(name, val, label).
# Handles both 2021 style (/VARNAME on header line) and 2016 style
# (/ on own line, variable name on next line, or bare name at section start).
.spss_parse_val_labels <- function(lines) {
  lines <- trimws(lines[nchar(trimws(lines)) > 0])
  if (length(lines) == 0)
    return(tibble::tibble(name = character(), val = character(), label = character()))

  # Group boundaries: position 1 (first group starts immediately) plus
  # every line that starts with /
  slash_pos   <- which(grepl("^/", lines))
  group_starts <- sort(unique(c(1L, slash_pos)))
  group_ends   <- c(group_starts[-1L] - 1L, length(lines))

  purrr::map2_df(group_starts, group_ends, function(gs, ge) {
    s <- gs

    # Extract variable name by stripping leading /
    n <- trimws(sub("^/", "", lines[s]))

    # If header was bare "/" (2016 style), next line is the variable name
    if (nchar(n) == 0L) {
      s <- s + 1L
      if (s > ge)
        return(tibble::tibble(name = character(), val = character(), label = character()))
      n <- trimws(lines[s])
    }

    # Take only the first word of the name (guard against "VAR1 VAR2" headers)
    n <- sub("\\s.*", "", n)

    code_start <- s + 1L
    if (code_start > ge)
      return(tibble::tibble(name = character(), val = character(), label = character()))

    code_lines <- lines[seq(code_start, ge)]
    code_lines <- code_lines[!grepl("^/", code_lines)]  # drop embedded / lines
    code_lines <- code_lines[nchar(code_lines) > 0L]

    if (length(code_lines) == 0L)
      return(tibble::tibble(name = character(), val = character(), label = character()))

    tibble::tibble(line = code_lines) |>
      dplyr::mutate(
        # Find all double-quoted strings on each line
        all_q   = stringr::str_extract_all(.data$line, '"[^"]*"'),
        n_q     = vapply(.data$all_q, length, integer(1L)),
        # Safe accessors: NA when the list element is empty
        first_q = vapply(.data$all_q,
                         function(x) if (length(x) > 0L) x[[1L]] else NA_character_,
                         character(1L)),
        last_q  = vapply(.data$all_q,
                         function(x) if (length(x) > 0L) x[[length(x)]] else NA_character_,
                         character(1L)),
        # Code: first quoted string content when line starts with ", else unquoted prefix
        val   = dplyr::if_else(
          grepl('^"', trimws(.data$line)),
          gsub('^"|"$', "", .data$first_q),
          trimws(sub('".*', "", .data$line))
        ),
        # Label: always the last quoted string
        label = gsub('^"|"$', "", .data$last_q)
      ) |>
      dplyr::filter(!is.na(.data$label), nchar(.data$val) > 0L) |>
      dplyr::mutate(name = n) |>
      dplyr::select("name", "val", "label")
  })
}


# Parse FORMATS section → tibble(name, fmt_type).
# fmt_type is the leading letter of the format code: A=character, F/N=numeric.
.spss_parse_formats <- function(lines) {
  if (length(lines) == 0L)
    return(tibble::tibble(name = character(), fmt_type = character()))

  combined <- paste(trimws(lines), collapse = " ")
  combined <- gsub("\\.\\s*$", "", combined)        # strip terminal period
  parts <- stringr::str_split(combined, "/")[[1L]]
  parts <- trimws(parts[nchar(trimws(parts)) > 0L])

  tibble::tibble(part = parts) |>
    dplyr::mutate(
      name     = stringr::str_match(.data$part, "^([A-Za-z][A-Za-z0-9_]*)")[, 2L],
      fmt_raw  = stringr::str_match(.data$part, "\\(([A-Za-z][^)]*)\\)")[, 2L],
      fmt_type = substr(.data$fmt_raw, 1L, 1L)
    ) |>
    dplyr::filter(!is.na(.data$name)) |>
    dplyr::select("name", "fmt_type")
}


# Parse MISSING VALUES section → tibble(name, missing_low, missing_high).
.spss_parse_missing <- function(lines) {
  lines <- lines[nchar(trimws(lines)) > 0L]
  if (length(lines) == 0L)
    return(tibble::tibble(name = character(),
                          missing_low = double(), missing_high = double()))

  tibble::tibble(value = trimws(lines)) |>
    dplyr::mutate(
      name        = stringr::str_match(.data$value, "^([A-Za-z][A-Za-z0-9_]*)")[, 2L],
      miss_str    = stringr::str_match(.data$value, "\\(([^)]+)\\)")[, 2L]
    ) |>
    dplyr::filter(!is.na(.data$name), !is.na(.data$miss_str)) |>
    dplyr::mutate(
      missing_low  = as.numeric(
        stringr::str_match(.data$miss_str, "^([\\d.]+)")[, 2L]),
      missing_high = dplyr::if_else(
        grepl("THRU", .data$miss_str, ignore.case = TRUE),
        as.numeric(stringr::str_match(.data$miss_str, "THRU\\s+([\\d.]+)")[, 2L]),
        .data$missing_low
      )
    ) |>
    dplyr::select("name", "missing_low", "missing_high")
}


# Parse DATA LIST section → tibble(name, start, end) or NULL.
# Handles single variable per line (AGEGRP 1-2), multiple per line
# (PPSORT 1-6  WEIGHT 7-22), and single-column variables without range (SEX 11).
.spss_parse_data_list <- function(all_clean, data_list_row) {
  remaining <- all_clean[seq(data_list_row + 1L, length(all_clean))]
  end_idx <- which(grepl("^\\.$|^$", remaining))[1L]
  # If no terminator found, use ALL remaining lines (not length-1 which misses last line)
  section <- if (is.na(end_idx)) remaining else remaining[seq_len(end_idx - 1L)]
  section <- section[grepl("[A-Za-z]", section)]
  section <- section[!grepl("^DATA LIST|^FILE HANDLE|^GET DATA|^FILE=|^/",
                             section, ignore.case = TRUE)]
  if (length(section) == 0L) return(NULL)

  # Tokenise the whole section; process pairs (name, range-or-col).
  # Ranges may have spaces around the dash (e.g. "11 - 18"), so we normalise
  # them to "11-18" before extracting tokens.
  all_text <- paste(gsub("\\.\\s*$", "", section), collapse = " ")
  all_text <- gsub("(\\d+)\\s+-\\s+(\\d+)", "\\1-\\2", all_text)  # "11 - 18" → "11-18"
  tokens <- stringr::str_extract_all(
    all_text, "[A-Za-z][A-Za-z0-9_]*|\\d+-\\d+|\\d+"
  )[[1L]]

  rows <- list()
  i <- 1L
  while (i <= length(tokens)) {
    tok <- tokens[i]
    if (grepl("^[A-Za-z]", tok) && i + 1L <= length(tokens)) {
      nxt <- tokens[i + 1L]
      if (grepl("^\\d+-\\d+$", nxt)) {
        parts <- as.integer(strsplit(nxt, "-")[[1L]])
        rows[[length(rows) + 1L]] <- list(name = tok, start = parts[1L], end = parts[2L])
        i <- i + 2L
      } else if (grepl("^\\d+$", nxt)) {
        col <- as.integer(nxt)
        rows[[length(rows) + 1L]] <- list(name = tok, start = col, end = col)
        i <- i + 2L
      } else {
        i <- i + 1L
      }
    } else {
      i <- i + 1L
    }
  }

  if (length(rows) == 0L) return(NULL)
  tibble::as_tibble(do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE)))
}


# ============================================================
# SPSS split-file parser
# ============================================================

#' Parse a set of SPSS split command files for PUMF metadata
#'
#' Handles surveys where metadata is spread across separate files:
#' \code{*_i.sps} (layout), \code{*vare.sps} (English variable labels),
#' \code{*vale.sps} (English value labels), \code{*miss.sps} (missing values),
#' with optional \code{*varf.sps} / \code{*valf.sps} for French.
#'
#' @param layout_dir Path to the directory containing the SPSS split files.
#' @param layout_mask Optional string or regex; passed to
#'   \code{find_unique_layout_file()} to disambiguate when multiple sets of
#'   split files exist (e.g. SFS has both \code{EFAM_PUMF_*} and
#'   \code{bsweights_pumf_*} files in the same directory).
#' @param encoding Character encoding, e.g. \code{"Latin1"} or \code{"CP1252"}.
#' @return A list with elements \code{variables}
#'   (\code{name, label_en, label_fr, type, missing_low, missing_high}),
#'   \code{codes} (\code{name, val, label_en, label_fr}), and \code{layout}
#'   (\code{name, start, end}; \code{NULL} if no layout file is found).
#' @keywords internal
parse_spss_split <- function(layout_dir, layout_mask = NULL, encoding = "Latin1") {

  # Helper: find a split file by suffix pattern; return NULL if absent or ambiguous
  find_opt <- function(suffix_pattern) {
    tryCatch(
      find_unique_layout_file(layout_dir, suffix_pattern, layout_mask),
      error = function(e) NULL
    )
  }

  layout_path  <- find_opt("_i\\.sps")
  eng_var_path <- find_opt("vare\\.sps")
  fra_var_path <- find_opt("varf\\.sps")
  eng_val_path <- find_opt("vale\\.sps")
  fra_val_path <- find_opt("valf\\.sps")
  miss_path    <- find_opt("miss\\.sps")

  # ---- Parse layout (_i.sps) ----
  layout_result <- if (!is.null(layout_path))
    .spss_split_parse_layout(layout_path, encoding)
  else
    list(layout = NULL, formats = tibble::tibble(name = character(), fmt_type = character()))

  # ---- Parse variable labels ----
  eng_var <- if (!is.null(eng_var_path))
    .spss_split_read_section(eng_var_path, encoding) |> .spss_parse_var_labels()
  else
    tibble::tibble(name = character(), label = character())

  fra_var <- if (!is.null(fra_var_path))
    .spss_split_read_section(fra_var_path, encoding) |> .spss_parse_var_labels()
  else
    NULL

  # ---- Parse value labels ----
  eng_val <- if (!is.null(eng_val_path))
    .spss_split_read_section(eng_val_path, encoding) |> .spss_parse_val_labels()
  else
    tibble::tibble(name = character(), val = character(), label = character())

  fra_val <- if (!is.null(fra_val_path))
    .spss_split_read_section(fra_val_path, encoding) |> .spss_parse_val_labels()
  else
    NULL

  # ---- Parse missing values ----
  missing_vals <- if (!is.null(miss_path))
    .spss_split_read_section(miss_path, encoding) |> .spss_parse_missing()
  else
    tibble::tibble(name = character(), missing_low = double(), missing_high = double())

  # ---- Combine type information ----
  vars_with_codes <- unique(eng_val$name)

  variables <- eng_var |>
    dplyr::left_join(layout_result$formats, by = "name") |>
    dplyr::left_join(missing_vals,           by = "name") |>
    dplyr::mutate(
      type = dplyr::case_when(
        .data$name %in% vars_with_codes              ~ "character",
        !is.na(.data$missing_low)                    ~ "numeric",
        toupper(substr(.data$fmt_type, 1L, 1L)) == "A" ~ "character",
        !is.na(.data$fmt_type)                       ~ "numeric",
        TRUE                                         ~ "character"
      )
    )

  # ---- Merge bilingual labels ----
  if (!is.null(fra_var)) {
    variables <- dplyr::left_join(
      dplyr::rename(variables, label_en = "label"),
      dplyr::select(fra_var, "name", label_fr = "label"),
      by = "name"
    )
  } else {
    variables <- dplyr::rename(variables, label_en = "label") |>
      dplyr::mutate(label_fr = NA_character_)
  }

  if (!is.null(fra_val)) {
    codes <- dplyr::left_join(
      dplyr::rename(eng_val, label_en = "label"),
      dplyr::select(fra_val, "name", "val", label_fr = "label"),
      by = c("name", "val")
    )
  } else {
    codes <- dplyr::rename(eng_val, label_en = "label") |>
      dplyr::mutate(label_fr = NA_character_)
  }

  variables <- variables[, c("name", "label_en", "label_fr", "type",
                              "missing_low", "missing_high")]
  codes     <- codes[, c("name", "val", "label_en", "label_fr")]

  list(variables = variables, codes = codes, layout = layout_result$layout)
}


# Read a split SPSS file and return its content lines with the leading keyword
# line(s) and comment lines stripped.  Does NOT do quote normalisation — the
# section parsers handle that via the monolithic preprocessor when needed.
.spss_split_read_section <- function(path, encoding) {
  raw   <- readr::read_lines(path, locale = readr::locale(encoding = encoding))
  lines <- trimws(gsub("\\t", " ", raw))
  lines <- lines[nchar(lines) > 0L]

  # Strip leading keyword / comment lines
  kw <- "^VARIABLE LABELS|^VALUE LABELS|^MISSING VALUES|^DATA LIST|^Comment"
  while (length(lines) > 0L && grepl(kw, lines[1L], ignore.case = TRUE))
    lines <- lines[-1L]

  # Normalise single-quoted label lines to double quotes (same logic as monolithic)
  sq <- grepl("'[^']*'\\s*\\.?\\s*$", lines) &
        !grepl('"', lines, fixed = TRUE) &
        !grepl("^\\s*/\\*", lines)
  lines[sq] <- gsub("'", '"', lines[sq])
  lines <- gsub('"\\s*\\.$', '"', lines)

  lines
}


# Parse an _i.sps layout file.
# Returns list(layout = tibble(name, start, end), formats = tibble(name, fmt_type)).
# fmt_type: "A" = character, "F" = numeric (decimal-place annotation), NA = unknown.
.spss_split_parse_layout <- function(path, encoding) {
  raw   <- readr::read_lines(path, locale = readr::locale(encoding = encoding))
  lines <- trimws(gsub("\\t", " ", raw))
  lines <- lines[nchar(lines) > 0L]

  # Find DATA LIST keyword and section content
  dl_row <- which(grepl("^DATA LIST", lines, ignore.case = TRUE))[1L]
  if (is.na(dl_row))
    return(list(layout  = NULL,
                formats = tibble::tibble(name = character(), fmt_type = character())))

  section <- lines[seq(dl_row + 1L, length(lines))]
  end_idx <- which(grepl("^\\.$|^$", section))[1L]
  section <- if (is.na(end_idx)) section else section[seq_len(end_idx - 1L)]
  section <- section[grepl("[A-Za-z]", section)]

  # Extract type annotations from each line before tokenising
  annot_df <- tibble::tibble(value = section) |>
    dplyr::mutate(
      name     = stringr::str_match(.data$value, "^([A-Za-z][A-Za-z0-9_]*)")[, 2L],
      annot    = stringr::str_match(.data$value, "\\(([^)]+)\\)")[, 2L],
      fmt_type = dplyr::case_when(
        trimws(.data$annot) == "A"            ~ "A",
        grepl("^\\d+$", trimws(.data$annot))  ~ "F",
        TRUE                                  ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(.data$name)) |>
    dplyr::select("name", "fmt_type")

  # Reuse the tokeniser-based data_list parser: prepend a dummy DATA LIST header
  layout <- .spss_parse_data_list(c("DATA LIST", section), 1L)

  list(layout = layout, formats = annot_df)
}
