
# Canonical metadata schema
# variables.csv: name (chr), label_en (chr), label_fr (chr|NA),
#                type (chr: "character"|"numeric"),
#                decimals (int|NA)  -- decimal places from format code; 0=integer, NA=unknown
#                missing_low (dbl|NA), missing_high (dbl|NA)
# codes.csv:     name (chr), val (chr), label_en (chr), label_fr (chr|NA)
# layout.csv:    name (chr), start (int), end (int)   [only for fixed-width data]

# Sentinel label pattern: labels that indicate a missing/non-response code rather
# than a genuine category.  Used by all parsers to distinguish sentinel-only
# variables (→ numeric with a missing range) from truly categorical ones.
.sentinel_pat <- paste0(
  "(?i)^(not (applicable|stated|asked|in (the )?universe|available|in sample)( [(][^)]*[)])?|data not available|",
  "valid skip|refusal|refused|don.?t know( [(][^)]*[)])?|do not know( [(][^)]*[)])?|",
  "missing|n/a|does not apply|not in scope|",
  # Zero-value labels: older Census files label code 0 as "ZERO", "ZERO HOURS",
  # "ZERO WEEKS", etc.  These describe the numeric zero and indicate the variable
  # is a continuous quantity, not a category.
  "zero(\\s+\\w+)*|",
  # French equivalents (1991 Census XMF is French-only; GSS uses "Non demandé")
  "sans objet|non (disponible|déclaré|applicable)|",
  "ne s.?applique pas|inconnu|manquant|non demandé|hors .chantillon)$"
)

# Identify variables whose value labels are ALL sentinel labels.
# Returns a named list: name → c(missing_lo, missing_hi) derived from the
# sentinel code values (NA_real_ pair when values are non-numeric).
.detect_sentinel_only <- function(codes_tbl, label_col = "label_en") {
  out <- list()
  for (v in unique(codes_tbl$name)) {
    vc     <- codes_tbl[codes_tbl$name == v, ]
    labels <- trimws(vc[[label_col]])
    if (length(labels) > 0L && all(grepl(.sentinel_pat, labels, perl = TRUE))) {
      s_nums <- suppressWarnings(as.numeric(vc$val))
      s_nums <- s_nums[!is.na(s_nums)]
      out[[v]] <- if (length(s_nums) > 0L) c(min(s_nums), max(s_nums))
                  else c(NA_real_, NA_real_)
    }
  }
  out
}

.metadata_variables_cols <- readr::cols(
  name         = readr::col_character(),
  label_en     = readr::col_character(),
  label_fr     = readr::col_character(),
  type         = readr::col_character(),
  decimals     = readr::col_integer(),
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
                 type = character(), decimals = integer(),
                 missing_low = double(), missing_high = double())
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
  # Backward-compat: old cached files may not have a decimals column
  if (!"decimals" %in% names(variables))
    variables$decimals <- NA_integer_
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

  # Only warn when French is partially available -- complete absence is expected
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
             c("name", "label_en", "label_fr", "type", "decimals",
               "missing_low", "missing_high"),
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
    # Deduplicate on the key columns before joining: some surveys (e.g. GSS 1996)
    # define the same variable in multiple VALUE LABELS sub-blocks with
    # inconsistent codes.  Keep only the first occurrence per (name[,val]).
    variables <- left_join(
      dplyr::distinct(rename(eng$variables, label_en = "label"), name, .keep_all = TRUE),
      dplyr::distinct(select(fra$variables, "name", label_fr = "label"), name, .keep_all = TRUE),
      by = "name"
    )
    codes <- left_join(
      dplyr::distinct(rename(eng$codes, label_en = "label"), name, val, .keep_all = TRUE),
      dplyr::distinct(select(fra$codes, "name", "val", label_fr = "label"), name, val, .keep_all = TRUE),
      by = c("name", "val")
    )
  } else {
    variables <- mutate(
      rename(eng$variables, label_en = "label"),
      label_fr = NA_character_
    )
    codes <- mutate(
      rename(eng$codes, label_en = "label"),
      label_fr = NA_character_
    )
  }

  variables <- variables[, c("name", "label_en", "label_fr", "type", "decimals",
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
  # "^MISSING VALUES($|\\s)" catches both "MISSING VALUES\n" (modern files where the
  # keyword is alone on its own line) and "MISSING VALUES  VAR  (0)/\n"
  # (older files like 1976/1981 where the first variable follows on the same line).
  missing_pos     <- is_kw("^MISSING VALUES($|\\s)")
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
  formats <- tibble::tibble(name = character(), fmt_type = character(),
                            decimals = integer())
  if (length(formats_pos) > 0)
    formats <- .spss_parse_formats(section_lines(formats_pos[1]))

  # ---- MISSING VALUES ----
  missing_vals <- tibble::tibble(name = character(),
                                 missing_low = double(), missing_high = double())
  if (length(missing_pos) > 0) {
    # Older SPS files (e.g. 1976/1981 Census) put the first variable on the same
    # line as the keyword: "MISSING VALUES  CMA  ( 0 )/".  Capture any inline
    # content after the keyword and prepend it to the section lines.
    miss_inline <- trimws(sub("^MISSING VALUES\\s*", "", spss$clean[missing_pos[1]]))
    miss_lines  <- c(if (nchar(miss_inline) > 0L) miss_inline else character(0L),
                     section_lines(missing_pos[1]))
    missing_vals <- .spss_parse_missing(miss_lines)
  }

  # ---- DATA LIST (layout) ----
  layout <- NULL
  if (length(data_list_pos) > 0)
    layout <- .spss_parse_data_list(spss$clean, data_list_pos[1])

  # ---- Combine into variables tibble ----
  # Variables whose value labels are ALL sentinel labels (Not applicable, Valid
  # skip, etc.) are continuous numerics with a missing-value range, not
  # categoricals.  Exclude them from truly_categorical so they fall through to
  # the missing_low or fmt_type rules.  Drop their codes entries (the missing
  # range captures the sentinel info; keeping them would cause spurious
  # "unmatched value" warnings in .apply_code_labels).
  sentinel_info    <- .detect_sentinel_only(codes, label_col = "label")
  sentinel_names   <- names(sentinel_info)
  truly_categorical <- setdiff(unique(codes$name), sentinel_names)
  codes            <- codes[!codes$name %in% sentinel_names, ]

  # Build a missing-range tibble from sentinel detection so that variables with
  # only sentinel VALUE LABELS (but no MISSING VALUES entry) still get a correct
  # missing range.  Ranges from the MISSING VALUES section take precedence.
  # names(list()) returns NULL, not character(0), so guard explicitly.
  snt_nms <- names(sentinel_info) %||% character(0L)
  sentinel_ranges <- tibble::tibble(
    name                  = snt_nms,
    missing_low_sentinel  = vapply(sentinel_info, function(x) x[[1L]], numeric(1L)),
    missing_high_sentinel = vapply(sentinel_info, function(x) x[[2L]], numeric(1L))
  )

  # Variables in DATA LIST without an explicit 'A' format type are numeric in SPSS.
  in_data_list <- if (!is.null(layout)) layout$name else character(0L)

  variables <- variable_labels |>
    left_join(formats,         by = "name") |>
    left_join(missing_vals,    by = "name") |>
    left_join(sentinel_ranges, by = "name") |>
    mutate(
      missing_low  = coalesce(.data$missing_low,  .data$missing_low_sentinel),
      missing_high = coalesce(.data$missing_high, .data$missing_high_sentinel),
      type = case_when(
        .data$name %in% truly_categorical                ~ "character",
        toupper(substr(.data$fmt_type, 1L, 1L)) == "A"  ~ "character",
        !is.na(.data$missing_low)                        ~ "numeric",
        !is.na(.data$fmt_type)                           ~ "numeric",
        .data$name %in% in_data_list                     ~ "numeric",
        TRUE                                             ~ "character"
      ),
      decimals = if_else(.data$type == "character", NA_integer_, .data$decimals)
    ) |>
    select("name", "label", "type", "decimals", "missing_low", "missing_high")

  # For DATA LIST-only files (no VARIABLE LABELS at all), populate variables
  # from the layout so that column types are preserved.  Only activate when
  # VARIABLE LABELS is entirely absent — for well-labeled surveys (Census, GSS,
  # etc.) some DATA LIST variables may legitimately lack labels, and adding them
  # here would trigger spurious "no French translation" warnings.
  if (!is.null(layout) && length(var_labels_pos) == 0L) {
    missing_names <- setdiff(layout$name, variables$name)
    if (length(missing_names) > 0L) {
      # Detect character-type variables from (A) annotations in the raw DATA
      # LIST section lines (done here rather than in _spss_parse_data_list so
      # that function's return value stays a plain (name, start, end) tibble).
      dl_char_vars <- character(0L)
      if (length(data_list_pos) > 0L) {
        dl_sec <- spss$clean[seq(data_list_pos[1L] + 1L, length(spss$clean))]
        end_dl <- which(grepl(
          "^\\.$|^$|^VARIABLE LABELS|^VALUE LABELS|^MISSING VALUES|^FORMATS|^EXECUTE",
          dl_sec, ignore.case = TRUE))[1L]
        if (!is.na(end_dl)) dl_sec <- dl_sec[seq_len(end_dl - 1L)]
        dl_sec <- dl_sec[grepl("[A-Za-z]", dl_sec)]
        dl_sec <- dl_sec[!grepl("^DATA LIST|^FILE HANDLE|^GET DATA|^FILE=",
                                 dl_sec, ignore.case = TRUE)]
        for (ln in dl_sec) {
          m <- regmatches(ln, regexpr("^([A-Za-z][A-Za-z0-9_]*)", trimws(ln)))
          if (length(m) == 1L && grepl("\\(A", ln, ignore.case = TRUE))
            dl_char_vars <- c(dl_char_vars, m)
        }
      }
      extra <- tibble::tibble(
        name         = missing_names,
        label        = NA_character_,
        type         = if_else(missing_names %in% dl_char_vars, "character", "numeric"),
        decimals     = NA_integer_,
        missing_low  = NA_real_,
        missing_high = NA_real_
      )
      variables <- bind_rows(variables, extra)
    }
  }

  list(variables = variables, codes = codes, layout = layout)
}


# Read and pre-process an SPSS file: tab->space, strip trailing whitespace,
# join single-quote + continuation lines, normalize single->double quotes.
# Returns a tibble with 'value' (original) and 'clean' (processed) columns.
.spss_read_preprocess <- function(path, encoding) {
  raw <- readr::read_lines(path, locale = readr::locale(encoding = encoding))

  lines <- gsub("\\t", " ", raw)
  lines <- gsub("\\s+$", "", lines)

  # Join string continuations: 'text' + 'more' -> 'textmore'
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

  # Normalize single-quoted label strings -> double quotes.
  # Allow any mix of trailing '.' and '/' after the closing quote, e.g.:
  #   CODE 'label' /          (2006 Census: block separator on same line)
  #   CODE 'label' / .        (2006 Census: last block + section terminator)
  #   CODE 'label' .          (2021 Census: section terminator)
  sq <- grepl("'[^']*'(\\s*[./])*\\s*$", lines) &
        !grepl('"', lines, fixed = TRUE) &
        !grepl("^\\s*/\\*", lines)            # skip comments
  lines[sq] <- gsub("'", '"', lines[sq])

  # Strip trailing '.' that some 2021-style lines use as section terminators
  lines <- gsub('"\\s*\\.$', '"', lines)

  # Expand trailing-'/' code lines into (code-line, bare-'/') pairs.
  # Census 2006 SPSS files end the last code of each variable block with
  # CODE "label" / or CODE "label" / . on the same line; split into the
  # clean code line plus a separator so group detection works correctly.
  trailing_sep <- grepl('".*\\s*/[./\\s]*$', lines) & !grepl("^/", lines)
  if (any(trailing_sep)) {
    expanded <- vector("list", length(lines))
    for (i in seq_along(lines)) {
      if (trailing_sep[[i]]) {
        expanded[[i]] <- c(sub("\\s*/[./\\s]*$", "", lines[[i]]), "/")
      } else {
        expanded[[i]] <- lines[[i]]
      }
    }
    lines <- unlist(expanded, use.names = FALSE)
  }

  tibble::tibble(clean = trimws(lines))
}


# Parse VARIABLE LABELS section content lines -> tibble(name, label).
.spss_parse_var_labels <- function(lines) {
  lines <- lines[nchar(trimws(lines)) > 0]
  if (length(lines) == 0)
    return(tibble::tibble(name = character(), label = character()))

  tibble::tibble(value = trimws(lines)) |>
    mutate(
      name  = sub("\\s.*", "", .data$value),
      label = {
        m <- stringr::str_match(.data$value, '"([^"]+)"')
        if_else(!is.na(m[, 1L]), m[, 2L], sub("^[A-Za-z0-9_]+\\s+", "", .data$value))
      }
    ) |>
    filter(grepl("^[A-Za-z][A-Za-z0-9_]*$", .data$name),
                  nchar(trimws(.data$label)) > 0) |>
    select("name", "label")
}


# Parse VALUE LABELS section content lines -> tibble(name, val, label).
# Handles both 2021 style (/VARNAME on header line) and 2016 style
# (/ on own line, variable name on next line, or bare name at section start).
.spss_parse_val_labels <- function(lines) {
  lines <- trimws(lines[nchar(trimws(lines)) > 0])
  if (length(lines) == 0)
    return(tibble::tibble(name = character(), val = character(), label = character()))

  # Group boundaries: position 1 (first group starts immediately) plus
  # every line that starts with /
  slash_pos    <- which(grepl("^/", lines))
  group_starts <- sort(unique(c(1L, slash_pos)))
  group_ends   <- c(group_starts[-1L] - 1L, length(lines))

  purrr::map2_df(group_starts, group_ends, function(gs, ge) {
    s <- gs

    # Extract content after the leading /
    first_line <- trimws(sub("^/", "", lines[s]))

    # Bare "/" (2016 style): next line holds the variable name(s)
    if (nchar(first_line) == 0L) {
      s <- s + 1L
      if (s > ge)
        return(tibble::tibble(name = character(), val = character(), label = character()))
      first_line <- trimws(lines[s])
    }

    # Collect variable names: the header may span several continuation lines.
    # A code line always contains at least one double-quoted label; header/name
    # lines do not.  Read lines while no double-quotes are present.
    name_words <- strsplit(first_line, "\\s+")[[1L]]
    code_start <- s + 1L
    while (code_start <= ge && !grepl('"', lines[code_start], fixed = TRUE)) {
      name_words <- c(name_words,
                      strsplit(trimws(lines[code_start]), "\\s+")[[1L]])
      code_start <- code_start + 1L
    }
    var_names <- name_words[grepl("^[A-Za-z][A-Za-z0-9_]*$", name_words)]

    if (length(var_names) == 0L || code_start > ge)
      return(tibble::tibble(name = character(), val = character(), label = character()))

    # 1991-style XMF: variable name and first code appear on the same header
    # line, e.g. "    SEXP   1  \"Femme\"".  The while loop above never fires
    # because first_line already contains a double-quote, so code_start = s+1
    # and the first code is left stranded on the header line.  Detect this by
    # checking whether first_line contains a quote, then strip the leading
    # identifier(s) and prepend the remainder as the first code line.
    inline_code <- if (grepl('"', first_line, fixed = TRUE)) {
      bare <- trimws(sub("^([A-Za-z][A-Za-z0-9_]*\\s+)+", "", first_line))
      if (nchar(bare) > 0L && grepl('"', bare, fixed = TRUE)) bare else character(0L)
    } else character(0L)

    code_lines <- lines[seq(code_start, ge)]
    code_lines <- code_lines[!grepl("^/", code_lines)]
    code_lines <- code_lines[nchar(code_lines) > 0L]
    code_lines <- c(inline_code, code_lines)

    if (length(code_lines) == 0L)
      return(tibble::tibble(name = character(), val = character(), label = character()))

    codes_df <- tibble::tibble(line = code_lines) |>
      mutate(
        all_q   = stringr::str_extract_all(.data$line, '"[^"]*"'),
        first_q = vapply(.data$all_q,
                         function(x) if (length(x) > 0L) x[[1L]] else NA_character_,
                         character(1L)),
        last_q  = vapply(.data$all_q,
                         function(x) if (length(x) > 0L) x[[length(x)]] else NA_character_,
                         character(1L)),
        is_q    = grepl('^"', trimws(.data$line)),
        raw_val = if_else(
          .data$is_q,
          gsub('^"|"$', "", .data$first_q),
          # Strip from first double-quote to end, then strip any trailing
          # non-identifier characters left by labels like '"No" single...'
          # where the label starts with a double-quoted word inside single quotes.
          trimws(stringr::str_replace(sub('".*', "", .data$line),
                                      "[^A-Za-z0-9._-]+$", ""))
        ),
        # Normalize unquoted (numeric) code values: parse as number then
        # convert back to string.  This strips leading zeros ("01" → "1")
        # and avoids integer overflow for large sentinel values (e.g. 99999999996).
        # R's as.character() renders whole-number doubles without ".0" ("1.0"→"1").
        # Quoted codes (character variables) are kept verbatim.
        val = if_else(
          .data$is_q,
          .data$raw_val,
          {
            num <- suppressWarnings(as.numeric(.data$raw_val))
            if_else(!is.na(num), as.character(num), .data$raw_val)
          }
        ),
        label = gsub('^"|"$', "", .data$last_q)
      ) |>
      filter(!is.na(.data$label), nchar(.data$val) > 0L) |>
      select("val", "label")

    # Replicate the code/label pairs for every variable in this header block
    purrr::map_df(var_names, function(vn)
      mutate(codes_df, name = vn)) |>
      select("name", "val", "label")
  })
}


# Parse FORMATS section -> tibble(name, fmt_type, decimals).
# fmt_type is the leading letter of the format code: A=character, F/N=numeric.
# decimals is the number of decimal places (e.g. F8.2 -> 2, F4.0 -> 0, A6 -> NA).
.spss_parse_formats <- function(lines) {
  if (length(lines) == 0L)
    return(tibble::tibble(name = character(), fmt_type = character(),
                          decimals = integer()))

  combined <- paste(trimws(lines), collapse = " ")
  combined <- gsub("\\.\\s*$", "", combined)        # strip terminal period
  parts <- stringr::str_split(combined, "/")[[1L]]
  parts <- trimws(parts[nchar(trimws(parts)) > 0L])

  tibble::tibble(part = parts) |>
    mutate(
      name     = stringr::str_match(.data$part, "^([A-Za-z][A-Za-z0-9_]*)")[, 2L],
      fmt_raw  = stringr::str_match(.data$part, "\\(([A-Za-z][^)]*)\\)")[, 2L],
      fmt_type = toupper(substr(.data$fmt_raw, 1L, 1L)),
      decimals = as.integer(
        stringr::str_match(.data$fmt_raw, "\\d+\\.(\\d+)")[, 2L])
    ) |>
    filter(!is.na(.data$name)) |>
    select("name", "fmt_type", "decimals")
}


# Parse MISSING VALUES section -> tibble(name, missing_low, missing_high).
.spss_parse_missing <- function(lines) {
  lines <- lines[nchar(trimws(lines)) > 0L]
  if (length(lines) == 0L)
    return(tibble::tibble(name = character(),
                          missing_low = double(), missing_high = double()))

  tibble::tibble(value = trimws(lines)) |>
    mutate(
      name        = stringr::str_match(.data$value, "^([A-Za-z][A-Za-z0-9_]*)")[, 2L],
      miss_str    = stringr::str_match(.data$value, "\\(([^)]+)\\)")[, 2L]
    ) |>
    filter(!is.na(.data$name), !is.na(.data$miss_str)) |>
    mutate(
      missing_low  = as.numeric(
        stringr::str_match(.data$miss_str, "^([\\d.]+)")[, 2L]),
      missing_high = if_else(
        grepl("THRU", .data$miss_str, ignore.case = TRUE),
        as.numeric(stringr::str_match(.data$miss_str, "THRU\\s+([\\d.]+)")[, 2L]),
        .data$missing_low
      )
    ) |>
    select("name", "missing_low", "missing_high")
}


# Parse DATA LIST section -> tibble(name, start, end) or NULL.
# Handles single variable per line (AGEGRP 1-2), multiple per line
# (PPSORT 1-6  WEIGHT 7-22), and single-column variables without range (SEX 11).
.spss_parse_data_list <- function(all_clean, data_list_row) {
  remaining <- all_clean[seq(data_list_row + 1L, length(all_clean))]
  # Stop at a blank/period line OR at the next SPSS command keyword.
  # Some older files (e.g. 1991 XMF) have no blank line between DATA LIST
  # and VARIABLE LABELS, so the keyword check is the only reliable terminator.
  end_idx <- which(grepl(
    "^\\.$|^$|^VARIABLE LABELS|^VALUE LABELS|^MISSING VALUES|^FORMATS|^EXECUTE",
    remaining, ignore.case = TRUE))[1L]
  section <- if (is.na(end_idx)) remaining else remaining[seq_len(end_idx - 1L)]
  section <- section[grepl("[A-Za-z]", section)]
  # Strip leading record-group marker (/) so variables on the same line as the
  # marker (e.g. "/PROVP  1-2") are retained.
  section <- sub("^/", "", section)
  section <- section[!grepl("^DATA LIST|^FILE HANDLE|^GET DATA|^FILE=",
                             section, ignore.case = TRUE)]
  if (length(section) == 0L) return(NULL)

  # Tokenise the whole section; process pairs (name, range-or-col).
  # Ranges may have spaces around the dash (e.g. "11 - 18"), so we normalise
  # them to "11-18" before extracting tokens.
  all_text <- paste(gsub("\\.\\s*$", "", section), collapse = " ")
  all_text <- gsub("(\\d+)\\s*-\\s*(\\d+)", "\\1-\\2", all_text)  # "11-  18" or "11 - 18" -> "11-18"
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
  tibble::as_tibble(do.call(rbind, lapply(rows, as.data.frame,
                                           stringsAsFactors = FALSE)))
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
    list(layout = NULL, formats = tibble::tibble(name = character(), fmt_type = character(),
                                                  decimals = integer()))

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
  sentinel_info     <- .detect_sentinel_only(eng_val, label_col = "label")
  sentinel_names    <- names(sentinel_info)
  truly_categorical <- setdiff(unique(eng_val$name), sentinel_names)
  eng_val           <- eng_val[!eng_val$name %in% sentinel_names, ]
  if (!is.null(fra_val))
    fra_val         <- fra_val[!fra_val$name %in% sentinel_names, ]

  # Ensure formats tibble has decimals column (defensively)
  if (!"decimals" %in% names(layout_result$formats))
    layout_result$formats$decimals <- NA_integer_

  # Variables in DATA LIST without an explicit 'A' format type are numeric in SPSS.
  in_data_list <- if (!is.null(layout_result$layout)) layout_result$layout$name
                  else character(0L)

  variables <- eng_var |>
    left_join(layout_result$formats, by = "name") |>
    left_join(missing_vals,           by = "name") |>
    mutate(
      type = case_when(
        .data$name %in% truly_categorical                ~ "character",
        toupper(substr(.data$fmt_type, 1L, 1L)) == "A"  ~ "character",
        !is.na(.data$missing_low)                        ~ "numeric",
        !is.na(.data$fmt_type)                           ~ "numeric",
        .data$name %in% in_data_list                     ~ "numeric",
        TRUE                                             ~ "character"
      ),
      decimals = if_else(.data$type == "character", NA_integer_, .data$decimals)
    )

  # ---- Merge bilingual labels ----
  if (!is.null(fra_var)) {
    variables <- left_join(
      rename(variables, label_en = "label"),
      select(fra_var, "name", label_fr = "label"),
      by = "name"
    )
  } else {
    variables <- rename(variables, label_en = "label") |>
      mutate(label_fr = NA_character_)
  }

  if (!is.null(fra_val)) {
    codes <- left_join(
      rename(eng_val, label_en = "label"),
      select(fra_val, "name", "val", label_fr = "label"),
      by = c("name", "val")
    )
  } else {
    codes <- rename(eng_val, label_en = "label") |>
      mutate(label_fr = NA_character_)
  }

  variables <- variables[, c("name", "label_en", "label_fr", "type", "decimals",
                              "missing_low", "missing_high")]
  codes     <- codes[, c("name", "val", "label_en", "label_fr")]

  list(variables = variables, codes = codes, layout = layout_result$layout)
}


# Read a split SPSS file and return its content lines with the leading keyword
# line(s) and comment lines stripped.  Does NOT do quote normalisation -- the
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
# Returns list(layout = tibble(name, start, end), formats = tibble(name, fmt_type, decimals)).
# fmt_type: "A" = character, "F" = numeric, NA = unknown.
# decimals: decimal places from format code (0 = integer, NA = unknown or character).
.spss_split_parse_layout <- function(path, encoding) {
  raw   <- readr::read_lines(path, locale = readr::locale(encoding = encoding))
  lines <- trimws(gsub("\\t", " ", raw))
  lines <- lines[nchar(lines) > 0L]

  # SAS @pos format: if any line starts with @, route to dedicated parser
  if (any(grepl("^@", lines))) {
    layout <- .sas_parse_at_layout(lines)
    # Derive fmt_type and decimals from the format spec after the variable name
    if (!is.null(layout)) {
      at_lines <- lines[grepl("^@\\s*\\d+", lines)]
      fmt_df <- tibble::tibble(value = at_lines) |>
        mutate(
          name     = stringr::str_match(.data$value,
                       "^@\\s*\\d+\\s+([A-Za-z][A-Za-z0-9_]*)")[, 2L],
          fmt_type = if_else(grepl("\\$", .data$value), "A", "F"),
          # Decimal count from "n.d" numeric format (e.g. "10.4" -> 4); NA for character
          decimals = if_else(
            grepl("\\$", .data$value),
            NA_integer_,
            as.integer(stringr::str_match(.data$value, "\\s+\\d+\\.(\\d+)\\s*$")[, 2L])
          )
        ) |>
        filter(!is.na(.data$name)) |>
        select("name", "fmt_type", "decimals")
    } else {
      fmt_df <- tibble::tibble(name = character(), fmt_type = character(),
                               decimals = integer())
    }
    return(list(layout = layout, formats = fmt_df))
  }

  # Find DATA LIST keyword and section content
  dl_row <- which(grepl("^DATA LIST|^INPUT\\s*$", lines, ignore.case = TRUE))[1L]
  if (is.na(dl_row))
    return(list(layout  = NULL,
                formats = tibble::tibble(name = character(), fmt_type = character(),
                                         decimals = integer())))

  section <- lines[seq(dl_row + 1L, length(lines))]
  end_idx <- which(grepl("^\\.$|^$", section))[1L]
  section <- if (is.na(end_idx)) section else section[seq_len(end_idx - 1L)]
  section <- section[grepl("[A-Za-z]", section)]

  # Extract type/decimal annotations from each line before tokenising.
  # SPSS DATA LIST annotation forms:
  #   none        → numeric integer (0 decimal places)
  #   (A) / (An)  → character
  #   (n)         → numeric, n decimal places  (e.g. "(4)" → 4 decimals)
  #   (Fn.d)      → numeric, d decimal places  (e.g. "(F8.2)" → 2 decimals)
  annot_df <- tibble::tibble(value = section) |>
    mutate(
      name     = stringr::str_match(.data$value, "^([A-Za-z][A-Za-z0-9_]*)")[, 2L],
      annot    = stringr::str_match(.data$value, "\\(([^)]+)\\)")[, 2L],
      fmt_type = case_when(
        trimws(.data$annot) == "A"            ~ "A",
        grepl("^A\\d*$", trimws(.data$annot)) ~ "A",
        grepl("^\\d+$", trimws(.data$annot))  ~ "F",
        is.na(.data$annot)                    ~ "F",
        TRUE                                  ~ NA_character_
      ),
      decimals = suppressWarnings(case_when(
        is.na(.data$annot)                           ~ 0L,
        grepl("^\\d+$", trimws(.data$annot))         ~
          as.integer(trimws(.data$annot)),
        !is.na(stringr::str_match(.data$annot,
                                   "[Ff]\\d+\\.(\\d+)")[, 2L]) ~
          as.integer(stringr::str_match(.data$annot, "[Ff]\\d+\\.(\\d+)")[, 2L]),
        TRUE                                         ~ NA_integer_
      ))
    ) |>
    filter(!is.na(.data$name)) |>
    select("name", "fmt_type", "decimals")

  # Detect sub-format: @pos (SAS input) vs NAME start-end (reading card)
  has_at <- any(grepl("^@", section))
  layout <- if (has_at)
    .sas_parse_at_layout(section)
  else
    .spss_parse_data_list(c("DATA LIST", section), 1L)

  list(layout = layout, formats = annot_df)
}


# Parse SAS @pos format layout lines.
# Input:  character vector of lines like "@1 CASEID $CHAR6." or "@6 WEIGHT 10.4"
# Output: tibble(name, start, end)
.sas_parse_at_layout <- function(lines) {
  at_lines <- lines[grepl("^@\\s*\\d+", lines)]
  if (length(at_lines) == 0L) return(NULL)

  tibble::tibble(value = at_lines) |>
    mutate(
      start   = as.integer(stringr::str_match(.data$value, "^@\\s*(\\d+)")[, 2L]),
      name    = stringr::str_match(.data$value,
                                   "^@\\s*\\d+\\s+([A-Za-z][A-Za-z0-9_]*)")[, 2L],
      # Size: from $CHARn. (character) or n.d (numeric)
      size    = as.integer(stringr::str_match(
        .data$value, "\\$?(?:CHAR)?(\\d+)\\.")[, 2L]),
      fmt_raw = stringr::str_match(.data$value,
                                   "\\s+(\\$[^.]+\\.|[0-9]+\\.[0-9]*)\\s*$")[, 2L],
      end     = .data$start + .data$size - 1L
    ) |>
    filter(!is.na(.data$name), !is.na(.data$start)) |>
    select("name", "start", "end")
}


# ============================================================
# SAS DATA step LABEL parser
# ============================================================

# Parse variable labels from SAS DATA step LABEL statements.
# Handles the pattern:  LABEL VARNAME = "label text" ;
# Used by Census 2011 individuals whose SPSS command file lacks VARIABLE LABELS.
parse_sas_data_labels <- function(eng_path, fra_path = NULL,
                                  encoding = "CP1252") {
  parse_one <- function(path) {
    if (is.null(path)) return(NULL)
    lines <- tryCatch(
      readr::read_lines(path, locale = readr::locale(encoding = encoding)),
      error = function(e) character(0L)
    )
    m <- stringr::str_match(lines,
           "^\\s*LABEL\\s+([A-Za-z][A-Za-z0-9_]*)\\s*=\\s*\"([^\"]+)\"")
    df <- tibble::tibble(name = toupper(m[, 2L]), label = m[, 3L])
    df[!is.na(df$name), ]
  }

  eng <- parse_one(eng_path)
  if (is.null(eng) || nrow(eng) == 0L) return(NULL)
  fra <- parse_one(fra_path)

  variables <- if (!is.null(fra) && nrow(fra) > 0L) {
    left_join(rename(eng, label_en = "label"),
              select(fra, "name", label_fr = "label"),
              by = "name")
  } else {
    mutate(rename(eng, label_en = "label"), label_fr = NA_character_)
  }
  variables$type         <- NA_character_
  variables$decimals     <- NA_integer_
  variables$missing_low  <- NA_real_
  variables$missing_high <- NA_real_

  list(
    variables = variables,
    codes     = tibble::tibble(name = character(), val = character(),
                               label_en = character(), label_fr = character()),
    layout    = NULL
  )
}


# ============================================================
# SAS reading cards parser
# ============================================================

#' Parse a set of SAS/SPSS reading-card files for PUMF metadata
#'
#' Handles surveys where metadata is stored in reading-card files:
#' \code{.lay} (column positions), \code{.lbe} (English variable labels),
#' \code{.cde} (English value labels), \code{.mvs} (missing values), with
#' optional \code{.lbf} / \code{.cdf} for French.
#'
#' The \code{.lay} file supports two sub-formats detected automatically:
#' \describe{
#'   \item{Reading-card}{Lines like \code{NAME start - end (A)}}
#'   \item{SAS input}{Lines like \code{@pos NAME \$CHAR6.}}
#' }
#'
#' @param cards_dir Path to the directory containing the reading-card files.
#' @param layout_mask Optional string; used to filter when multiple files share
#'   the same extension (e.g. SHS 2017 has separate interview and diary files).
#' @param encoding Character encoding (e.g. \code{"Latin1"}, \code{"CP1252"}).
#' @return A list with elements \code{variables}
#'   (\code{name, label_en, label_fr, type, missing_low, missing_high}),
#'   \code{codes} (\code{name, val, label_en, label_fr}), and \code{layout}
#'   (\code{name, start, end}; \code{NULL} when no \code{.lay} file is found).
#' @keywords internal
parse_sas_cards <- function(cards_dir, layout_mask = NULL, encoding = "Latin1") {

  # Helper: find a file by extension, optionally filtered by layout_mask
  find_ext <- function(ext) {
    pat   <- paste0("\\.", ext, "$")
    files <- dir(cards_dir, pattern = pat, full.names = TRUE, ignore.case = TRUE)
    if (!is.null(layout_mask) && length(files) > 1)
      files <- files[grepl(layout_mask, basename(files), ignore.case = TRUE)]
    if (length(files) == 0L) return(NULL)
    if (length(files) > 1L) {
      warning("Multiple .", ext, " files in ", cards_dir,
              "; using: ", basename(files[1L]))
      files <- files[1L]
    }
    files
  }

  lay_path <- find_ext("lay")
  lbe_path <- find_ext("lbe")
  lbf_path <- find_ext("lbf")
  cde_path <- find_ext("cde")
  cdf_path <- find_ext("cdf")
  mvs_path <- find_ext("mvs")

  # ---- Layout (.lay) ----
  layout_result <- if (!is.null(lay_path))
    .spss_split_parse_layout(lay_path, encoding)   # handles both NAME and @pos formats
  else
    list(layout  = NULL,
         formats = tibble::tibble(name = character(), fmt_type = character()))

  # ---- Variable labels (.lbe / .lbf) ----
  # SAS label files use NAME="label" (no space); SPSS label files use
  # NAME "label". Detect by presence of the = pattern and route accordingly.
  read_var_labels <- function(path) {
    if (is.null(path)) return(NULL)
    lines <- .spss_split_read_section(path, encoding)
    is_sas_fmt <- any(grepl('^[A-Za-z][A-Za-z0-9_]*\\s*=\\s*"', lines))
    df <- if (is_sas_fmt) {
      m <- stringr::str_match(lines,
             '^([A-Za-z][A-Za-z0-9_]*)\\s*=\\s*"([^"]*)"')
      tibble::tibble(name = m[, 2L], label = m[, 3L]) |>
        filter(!is.na(.data$name))
    } else {
      .spss_parse_var_labels(lines)
    }
    mutate(df, name = toupper(.data$name))
  }

  eng_var <- read_var_labels(lbe_path) %||%
    tibble::tibble(name = character(), label = character())
  fra_var <- read_var_labels(lbf_path)   # NULL when absent

  # ---- Value labels (.cde / .cdf) ----
  read_val_labels <- function(path) {
    if (is.null(path)) return(NULL)
    lines <- .spss_split_read_section(path, encoding)
    # SAS PROC FORMAT files share the .cde extension but use a different syntax
    # that _spss_parse_val_labels cannot handle — skip them silently.
    if (any(grepl("^PROC\\s+FORMAT", lines, ignore.case = TRUE)))
      return(tibble::tibble(name = character(), val = character(),
                            label = character()))
    df <- .spss_parse_val_labels(lines)
    mutate(df, name = toupper(.data$name))
  }

  eng_val <- read_val_labels(cde_path) %||%
    tibble::tibble(name = character(), val = character(), label = character())
  fra_val <- read_val_labels(cdf_path)

  # Drop range-notation codes (e.g. "00:06", "045:095") and empty-label codes.
  # SPSS .cde files use X:Y syntax to label a range of valid values as a group
  # (e.g. "045:095 'Valid age range'"), but individual data values within that
  # range cannot be mapped to a single factor level.  Keeping range codes also
  # suppresses sentinel-only detection, misclassifying numeric variables as
  # categorical.
  .drop_range_codes <- function(df) {
    if (is.null(df) || nrow(df) == 0L) return(df)
    df[!grepl("^\\d+:\\d+$", df$val) &
       !is.na(df$label) & nchar(trimws(df$label)) > 0L, ]
  }
  eng_val <- .drop_range_codes(eng_val)
  fra_val <- .drop_range_codes(fra_val)

  # ---- Missing values (.mvs) ----
  missing_vals <- if (!is.null(mvs_path)) {
    lines <- .spss_split_read_section(mvs_path, encoding)
    df    <- .spss_parse_missing(lines)
    mutate(df, name = toupper(.data$name))
  } else {
    tibble::tibble(name = character(), missing_low = double(), missing_high = double())
  }

  # ---- Combine type information ----
  sentinel_info     <- .detect_sentinel_only(eng_val, label_col = "label")
  sentinel_names    <- names(sentinel_info)
  truly_categorical <- setdiff(unique(eng_val$name), sentinel_names)
  eng_val           <- eng_val[!eng_val$name %in% sentinel_names, ]
  if (!is.null(fra_val))
    fra_val         <- fra_val[!fra_val$name %in% sentinel_names, ]

  # Ensure formats tibble has decimals column (defensively)
  if (!"decimals" %in% names(layout_result$formats))
    layout_result$formats$decimals <- NA_integer_
  fmt_df <- mutate(layout_result$formats, name = toupper(.data$name))

  # Variables with a layout entry but no explicit 'A' format type are numeric.
  in_data_list <- if (!is.null(layout_result$layout))
    toupper(layout_result$layout$name) else character(0L)

  variables <- eng_var |>
    left_join(fmt_df,       by = "name") |>
    left_join(missing_vals, by = "name") |>
    mutate(
      type = case_when(
        .data$name %in% truly_categorical                ~ "character",
        toupper(substr(.data$fmt_type, 1L, 1L)) == "A"  ~ "character",
        !is.na(.data$missing_low)                        ~ "numeric",
        !is.na(.data$fmt_type)                           ~ "numeric",
        .data$name %in% in_data_list                     ~ "numeric",
        TRUE                                             ~ "character"
      ),
      decimals = if_else(.data$type == "character", NA_integer_, .data$decimals)
    )

  # ---- Merge bilingual labels ----
  if (!is.null(fra_var)) {
    variables <- left_join(
      rename(variables, label_en = "label"),
      select(fra_var, "name", label_fr = "label"),
      by = "name"
    )
  } else {
    variables <- rename(variables, label_en = "label") |>
      mutate(label_fr = NA_character_)
  }

  if (!is.null(fra_val)) {
    codes <- left_join(
      distinct(rename(eng_val, label_en = "label")),
      distinct(select(fra_val, "name", "val", label_fr = "label")),
      by = c("name", "val")
    )
  } else {
    codes <- distinct(rename(eng_val, label_en = "label")) |>
      mutate(label_fr = NA_character_)
  }

  layout <- if (!is.null(layout_result$layout))
    mutate(layout_result$layout, name = toupper(.data$name))
  else
    NULL

  variables <- variables[, c("name", "label_en", "label_fr", "type", "decimals",
                              "missing_low", "missing_high")]
  codes     <- codes[, c("name", "val", "label_en", "label_fr")]

  list(variables = variables, codes = codes, layout = layout)
}


# ============================================================
# Step 6 -- CSV parsers
# ============================================================

#' Parse an LFS codebook CSV into canonical metadata
#'
#' The LFS codebook is a single bilingual CSV with rows alternating between
#' variable definitions (\code{Field_Champ} filled) and code values
#' (\code{Field_Champ} NA).
#'
#' @param codebook_path Path to the LFS \code{codebook.csv} file.
#' @param encoding File encoding (default \code{"CP1252"}).
#' @return Named list with elements \code{variables}, \code{codes},
#'   and \code{layout} (always \code{NULL} for LFS CSV data).
#' @keywords internal
parse_lfs_codebook <- function(codebook_path, encoding = "CP1252") {
  raw <- suppressWarnings(
    readr::read_csv(codebook_path,
                    locale         = readr::locale(encoding = encoding),
                    col_types      = readr::cols(.default = "c"),
                    show_col_types = FALSE)
  )

  # French label column name may vary by year; detect by pattern
  fra_col <- grep("(?i)french|fran", names(raw), value = TRUE, perl = TRUE)
  fra_col <- if (length(fra_col) > 0L) fra_col[[1L]] else NULL

  # Variable rows: Field_Champ is not NA
  is_var <- !is.na(raw$Field_Champ)

  # Fill Field_Champ down into code rows (base-R, avoids tidyr dependency)
  fc <- raw$Field_Champ
  for (i in seq_along(fc)) {
    if (is.na(fc[i]) && i > 1L) fc[i] <- fc[i - 1L]
  }

  # Lookup table: Field_Champ value -> uppercase variable name.
  # trimws() handles codebook versions where names carry leading/trailing spaces
  # (observed in 2025 LFS release: " lfsstat    ", "age_6    ", etc.).
  var_fc   <- raw$Field_Champ[is_var]
  var_name <- trimws(toupper(raw$Variable_Variable[is_var]))

  # Code rows: originally NA Field_Champ and Variable_Variable not NA.
  # Filter out range-description tokens that newer codebook formats use to
  # document numeric ranges rather than list individual code values:
  #   - Numeric ranges:       "1-99999", "001-240", "000001-999999"
  #   - Open-ended ranges:    "1976-"
  #   - Excel-mangled ranges: "Jan-99" (Excel converted "1-99" to a date)
  #   - Blank token:          "blank"  (means missing/not-applicable in data)
  vv <- raw$Variable_Variable
  is_range_desc <- grepl("^[0-9].*-[0-9]|^[0-9]+-$|^[A-Za-z]{2,3}-[0-9]",
                          vv, perl = TRUE)
  is_blank_tok  <- tolower(trimws(vv)) == "blank"
  is_code <- !is_var & !is.na(vv) & !is_range_desc & !is_blank_tok
  code_name <- var_name[match(fc[is_code], var_fc)]

  codes <- tibble::tibble(
    name     = code_name,
    val      = trimws(raw$Variable_Variable[is_code]),
    label_en = raw$EnglishLabel_EtiquetteAnglais[is_code],
    label_fr = if (!is.null(fra_col)) raw[[fra_col]][is_code] else NA_character_
  )
  codes <- codes[!is.na(codes$name), ]

  # Classify each variable:
  #  - No codes → numeric
  #  - All codes are sentinel labels (Not applicable, Not stated, Valid skip,
  #    etc.) → numeric; derive missing_low / missing_high from the sentinel
  #    code values so .apply_numeric_conversion can NA them out
  #  - At least one non-sentinel code → character (categorical)
  #
  # Sentinel labels are recognised by an exact anchored match so that genuine
  # category labels that merely CONTAIN the words ("Employee, not applicable")
  # are never misclassified.  .sentinel_pat and .detect_sentinel_only are
  # defined at file scope and shared with the SPSS parsers.
  sentinel_missing   <- .detect_sentinel_only(codes, label_col = "label_en")
  sentinel_only      <- names(sentinel_missing)
  truly_categorical  <- setdiff(unique(codes$name), sentinel_only)

  variables <- tibble::tibble(
    name         = var_name,
    label_en     = raw$EnglishLabel_EtiquetteAnglais[is_var],
    label_fr     = if (!is.null(fra_col)) raw[[fra_col]][is_var] else NA_character_,
    type         = ifelse(var_name %in% truly_categorical, "character", "numeric"),
    decimals     = NA_integer_,
    missing_low  = unname(vapply(var_name, function(v) {
      if (!is.null(sentinel_missing[[v]])) sentinel_missing[[v]][1L] else NA_real_
    }, numeric(1L))),
    missing_high = unname(vapply(var_name, function(v) {
      if (!is.null(sentinel_missing[[v]])) sentinel_missing[[v]][2L] else NA_real_
    }, numeric(1L)))
  )

  # SURVMNTH fixup: raw data has 2-digit month codes but codebook often has 1-digit
  if ("SURVMNTH" %in% codes$name) {
    is_mnth        <- codes$name == "SURVMNTH"
    codes$val[is_mnth] <- formatC(as.integer(codes$val[is_mnth]), width = 2L, flag = "0")
  }

  list(variables = variables, codes = codes, layout = NULL)
}


#' Parse a CPSS variables.csv into canonical metadata
#'
#' The CPSS \code{variables.csv} is a single bilingual CSV. Variable rows have
#' the \code{Variable} column filled; code rows have \code{Variable} empty and
#' the \code{Code} column filled.
#'
#' @param variables_path Path to the CPSS \code{variables.csv} file.
#' @param encoding File encoding (default \code{"Latin1"}).
#' @return Named list with elements \code{variables}, \code{codes},
#'   and \code{layout} (always \code{NULL} for CPSS CSV data).
#' @keywords internal
parse_cpss_csv <- function(variables_path, encoding = "Latin1") {
  raw <- suppressWarnings(
    readr::read_csv(variables_path,
                    locale         = readr::locale(encoding = encoding),
                    col_types      = readr::cols(.default = "c"),
                    show_col_types = FALSE)
  )
  # Strip bilingual suffix from column names (e.g. "Variable Name - English / Nom...")
  names(raw) <- gsub(" /.+$", "", names(raw))

  has_var_fr  <- "Variable Name - French" %in% names(raw)
  has_code_fr <- "Label - French"         %in% names(raw)

  # Variable rows: Variable column is not NA
  is_var <- !is.na(raw$Variable)

  # Fill Variable down for code rows
  var_col <- raw$Variable
  for (i in seq_along(var_col)) {
    if (is.na(var_col[i]) && i > 1L) var_col[i] <- var_col[i - 1L]
  }

  variables <- tibble::tibble(
    name         = toupper(raw$Variable[is_var]),
    label_en     = raw[["Variable Name - English"]][is_var],
    label_fr     = if (has_var_fr) raw[["Variable Name - French"]][is_var] else NA_character_,
    type         = "character",
    decimals     = NA_integer_,
    missing_low  = NA_real_,
    missing_high = NA_real_
  )

  # Code rows: Code column is not NA
  is_code <- !is.na(raw$Code)
  codes <- tibble::tibble(
    name     = toupper(var_col[is_code]),
    val      = raw$Code[is_code],
    label_en = raw[["Label - English"]][is_code],
    label_fr = if (has_code_fr) raw[["Label - French"]][is_code] else NA_character_
  )

  list(variables = variables, codes = codes, layout = NULL)
}


# ============================================================
# Step 7 -- Format detector, merger, dispatcher
# ============================================================

#' Detect parseable metadata formats in a PUMF directory tree
#'
#' Walks the directory tree under \code{pumf_dir} and returns every recognised
#' command-file format. Multiple formats may be present in one release; all are
#' returned so that the caller can run all applicable parsers and merge results.
#'
#' @param pumf_dir Top-level version directory (i.e. the directory that
#'   contains the extracted PUMF zip contents).
#' @return A named list (possibly empty) whose elements are a subset of
#'   \code{"lfs_csv"}, \code{"cpss_csv"}, \code{"sas_cards"},
#'   \code{"spss_split"}, and \code{"spss_mono"}. Each element is either a
#'   single path string or (for \code{spss_mono}) a
#'   \code{list(eng = ..., fra = ...)} where \code{fra} is \code{NULL} when no
#'   French file is found.
#' @keywords internal
detect_formats <- function(pumf_dir, sps_mask = NULL) {
  result <- list()
  all_files <- list.files(pumf_dir, recursive = TRUE, full.names = TRUE)
  # Exclude metadata/ output directories at any depth (covers both
  # <pumf_dir>/metadata/ for standalone deposits and <pumf_dir>/<type>/metadata/
  # for bundled-archive years where sibling types may already be built).
  all_files <- all_files[!grepl("/metadata/", all_files, fixed = TRUE)]
  # For bundled-archive versions with multiple per-type command files in the
  # same directory, sps_mask filters which SPSS/XMF files are visible so the
  # right type's command file is selected.
  if (!is.null(sps_mask)) {
    is_spss  <- grepl("\\.(sps|xmf)$", all_files, ignore.case = TRUE)
    all_files <- all_files[!is_spss | grepl(sps_mask, all_files, ignore.case = TRUE)]
  }

  # 1. LFS codebook.csv (may be prefixed, e.g. LFS_PUMF_EPA_FGMD_codebook.csv)
  cb <- all_files[grepl("(?i)codebook\\.csv$", basename(all_files), perl = TRUE)]
  if (length(cb) > 0L) result$lfs_csv <- cb[[1L]]

  # 2. CPSS variables.csv
  vf <- all_files[grepl("(?i)^variables\\.csv$", basename(all_files), perl = TRUE)]
  if (length(vf) > 0L) result$cpss_csv <- vf[[1L]]

  # 3. SAS reading cards: directory containing both .lay and .lbe files.
  # When multiple candidates exist (e.g. parallel SAS and SPSS card directories),
  # prefer .lbe files in SPSS syntax (contain "VARIABLE LABELS") over SAS syntax
  # (start with "LABEL") because our parser handles SPSS syntax natively.
  lay_files <- all_files[grepl("\\.lay$", all_files, ignore.case = TRUE)]
  lbe_files <- all_files[grepl("\\.lbe$", all_files, ignore.case = TRUE)]
  if (length(lay_files) > 0L && length(lbe_files) > 0L) {
    spss_lbe <- lbe_files[vapply(lbe_files, function(f) {
      hdr <- tryCatch(readLines(f, n = 5L, warn = FALSE, encoding = "latin1"),
                      error = function(e) character(0L))
      any(grepl("VARIABLE LABELS", hdr, ignore.case = TRUE))
    }, logical(1L))]
    chosen_lbe <- if (length(spss_lbe) > 0L) spss_lbe[[1L]] else lbe_files[[1L]]
    result$sas_cards <- dirname(chosen_lbe)
  }

  # 4. SPSS split: directory containing vare/vale/_i named .sps files
  sps_files  <- all_files[grepl("\\.sps$", all_files, ignore.case = TRUE)]
  split_sps  <- sps_files[grepl("(vare|vale|_i)\\.sps$", sps_files, ignore.case = TRUE)]
  if (length(split_sps) > 0L) {
    result$spss_split <- dirname(split_sps[[1L]])
  }

  # 5. SPSS monolithic: .sps file (not split-named), a .txt file with "SPSS"
  #    in its name (e.g. 2001 Census *SPSS.txt), or a .xmf file (e.g. 1991
  #    Census INDF91.XMF), verified by VALUE LABELS content. VARIABLE LABELS
  #    is optional — some releases (e.g. 2011 Census individuals) omit it.
  if (is.null(result$spss_split)) {
    spss_txt   <- all_files[grepl("spss\\.txt$", all_files, ignore.case = TRUE)]
    xmf_files  <- all_files[grepl("\\.xmf$",     all_files, ignore.case = TRUE)]
    mono_candidates <- c(
      sps_files[!grepl("(vare|vale|varf|valf|miss|_i)\\.sps$",
                        sps_files, ignore.case = TRUE)],
      spss_txt,
      xmf_files
    )
    # French indicators: /français/, /french/, /francais/, /-Fr/ directories,
    # or file suffixes _F.sps / _Fre.sps / _Fref.sps (case-insensitive).
    .is_fra <- function(paths) {
      grepl("(?i)(/(fran|french)|[/-][Ff]r[^a-z])", paths, perl = TRUE) |
      grepl("(?i)_[Ff](re?f?)?\\.sps$", basename(paths), perl = TRUE)
    }
    all_spss     <- c(sps_files, spss_txt, xmf_files)
    # Exclude likely-French files from English candidates
    mono_eng     <- mono_candidates[!.is_fra(mono_candidates)]
    if (length(mono_eng) == 0L) mono_eng <- mono_candidates   # fallback
    for (sps in head(mono_eng, 8L)) {
      hdr <- tryCatch(readLines(sps, warn = FALSE, encoding = "latin1"),
                      error = function(e) character(0L))
      has_val_labels <- any(grepl("VALUE LABELS", hdr, ignore.case = TRUE, useBytes = TRUE))
      has_data_list  <- any(grepl("DATA LIST",    hdr, ignore.case = TRUE, useBytes = TRUE))
      # Accept VALUE LABELS files always (original behaviour).
      # Accept DATA LIST-only files only when no richer source (SAS cards,
      # SPSS split) was already detected — avoids picking up layout-only SPSS
      # files that co-exist with SAS cards for the same survey (e.g. Census 2011).
      data_list_only_ok <- has_data_list && is.null(result$sas_cards) &&
                           is.null(result$spss_split)
      if (has_val_labels || data_list_only_ok) {
        fra_cands <- all_spss[.is_fra(all_spss) &
                               !grepl("(vare|vale|varf|valf|miss|_i)\\.sps$",
                                       all_spss, ignore.case = TRUE)]
        result$spss_mono <- list(eng = sps,
                                  fra = if (length(fra_cands) > 0L) fra_cands[[1L]] else NULL)
        break
      }
    }
  }

  # NOT YET IMPLEMENTED -- formats known to exist in StatCan PUMF releases:
  #
  # Stata do-files (*_lbe.do = "label variable NAME ...", *_vale.do = "label define FMT ...")
  # + Stata dictionary (*.dct = "_column(n) type NAME %fmt") for layout.
  # SFS 2005/2012/2016/2019 and Census 2016 all ship a StataCard/ directory alongside SPSS.
  # Since SPSS split/mono is always present in parallel, Stata adds no survey coverage.
  # Implement if a future release drops SPSS but retains Stata command files.
  #
  # DBF format (*.dbf): SFS 1984 and 1999 data files. The dBase III+ format can be
  # read with foreign::read.dbf(). These releases have no machine-readable codebook;
  # labels are in PDF only. Implement as a data-reading path (not a metadata parser)
  # if SFS 1984/1999 support is added.
  #
  # SAS program syntax (*_lbe.SAS LABEL blocks + *_pfe.SAS PROC FORMAT blocks):
  # SFS 2012/2016/2019 SasCard/ directories use this alongside SPSS split. No new
  # coverage: SPSS is always present. Implement if a future release drops SPSS.

  # 6. SPSS .sav data file with embedded labels (e.g. CIS surveys)
  #    Rank below command files; parse_spss_sav() handles it via haven.
  #    Only detect if no SPSS command files are present (avoid redundant parsing).
  if (is.null(result$spss_mono) && is.null(result$spss_split)) {
    sav_files <- all_files[grepl("\\.sav$", all_files, ignore.case = TRUE)]
    if (length(sav_files) > 0L) result$spss_sav <- sav_files[[1L]]
  }

  # 8. StatCan PDF data dictionary (e.g. SFS 1999).  Only used as a label
  #    fallback — positions in the PDF differ from the PUMF file; layout always
  #    comes from the SAS/SPSS command file.  Fires when a *Dictionary.pdf is
  #    found anywhere under version_dir and pdftools is available.
  eng_dict_pdf <- all_files[grepl("Dictionary\\.pdf$", all_files, ignore.case = TRUE)]
  if (length(eng_dict_pdf) > 0L &&
      requireNamespace("pdftools", quietly = TRUE)) {
    fra_dict_pdf <- all_files[grepl("Dictionnaire\\.pdf$", all_files, ignore.case = TRUE)]
    result$pdf_dict <- list(
      eng = eng_dict_pdf[[1L]],
      fra = if (length(fra_dict_pdf) > 0L) fra_dict_pdf[[1L]] else NULL
    )
  }

  # 7. SAS DATA step LABEL statements (e.g. 2011 Census individuals).
  #    Supplement when the primary parser lacks variable labels — scan for .SAS
  #    files containing "LABEL varname =" lines.
  if (is.null(result$sas_cards)) {
    sas_files <- all_files[grepl("\\.sas$", all_files, ignore.case = TRUE)]
    fra_sas   <- sas_files[grepl("/(fran|french)", sas_files, ignore.case = TRUE)]
    eng_sas   <- sas_files[!grepl("/(fran|french)", sas_files, ignore.case = TRUE)]
    for (sf in head(eng_sas, 4L)) {
      snippet <- tryCatch(readLines(sf, warn = FALSE, encoding = "latin1"),
                          error = function(e) character(0L))
      if (any(grepl("^\\s*LABEL\\s+[A-Za-z]", snippet, useBytes = TRUE))) {
        result$sas_labels <- list(
          eng = sf,
          fra = if (length(fra_sas) > 0L) fra_sas[[1L]] else NULL
        )
        break
      }
    }
  }

  result
}


#' Merge metadata from multiple parser outputs
#'
#' Sources are applied in priority order: \code{spss_mono} > \code{spss_split}
#' > \code{sas_cards} > \code{lfs_csv} > \code{cpss_csv}. For each variable /
#' code, the highest-priority source provides the English label; French labels
#' fill in from lower-priority sources when missing. Conflicting missing ranges
#' or English code labels emit warnings.
#'
#' @param parsed_list Named list of parser outputs (each a list with elements
#'   \code{variables}, \code{codes}, and \code{layout}).
#' @return Single merged canonical metadata list.
#' @keywords internal
merge_metadata <- function(parsed_list) {
  if (length(parsed_list) == 0L) stop("No parsed metadata to merge.")
  if (length(parsed_list) == 1L) return(parsed_list[[1L]])

  priority_order <- c("spss_mono", "spss_split", "sas_cards", "spss_sav",
                      "lfs_csv", "cpss_csv", "sas_labels", "pdf_dict")
  ordered  <- c(intersect(priority_order, names(parsed_list)),
                setdiff(names(parsed_list), priority_order))
  parsed_list <- parsed_list[ordered]

  # ---- Merge variables ----
  var_frames <- Filter(Negate(is.null), lapply(seq_along(parsed_list), function(i) {
    df <- parsed_list[[i]]$variables
    if (!is.null(df) && nrow(df) > 0L) { df$.src <- i; df } else NULL
  }))

  if (length(var_frames) > 0L) {
    all_v <- do.call(rbind, var_frames)
    if (!"decimals" %in% names(all_v)) all_v$decimals <- NA_integer_
    all_v <- all_v[order(all_v$name, all_v$.src), ]
    rows  <- lapply(split(all_v, all_v$name), function(grp) {
      row <- grp[1L, , drop = FALSE]
      # Fill label_en from lower-priority source when highest-priority has NA.
      # Allows a DATA LIST-only spss_mono (no variable labels) to coexist with
      # a sas_labels source that supplies the actual labels (e.g. Census 2011).
      if (is.na(row$label_en)) {
        en <- grp$label_en[!is.na(grp$label_en)]
        if (length(en) > 0L) row$label_en <- en[[1L]]
      }
      if (is.na(row$label_fr)) {
        fr <- grp$label_fr[!is.na(grp$label_fr)]
        if (length(fr) > 0L) row$label_fr <- fr[[1L]]
      }
      lo <- unique(grp$missing_low[!is.na(grp$missing_low)])
      hi <- unique(grp$missing_high[!is.na(grp$missing_high)])
      if (length(lo) > 1L || length(hi) > 1L)
        warning("Conflicting missing ranges for variable ", row$name, call. = FALSE)
      if (is.na(row$missing_low)  && length(lo) > 0L) row$missing_low  <- lo[[1L]]
      if (is.na(row$missing_high) && length(hi) > 0L) row$missing_high <- hi[[1L]]
      # Fill in decimals from lower-priority source when highest-priority has NA
      if (is.na(row$decimals)) {
        dec <- grp$decimals[!is.na(grp$decimals)]
        if (length(dec) > 0L) row$decimals <- dec[[1L]]
      }
      row
    })
    vars_merged <- do.call(rbind, rows)
    vars_merged$.src <- NULL
    rownames(vars_merged) <- NULL
    vars_merged <- vars_merged[, c("name","label_en","label_fr","type","decimals",
                                   "missing_low","missing_high")]
  } else {
    vars_merged <- empty_variables()
  }

  # ---- Merge codes ----
  code_frames <- Filter(Negate(is.null), lapply(seq_along(parsed_list), function(i) {
    df <- parsed_list[[i]]$codes
    if (!is.null(df) && nrow(df) > 0L) { df$.src <- i; df } else NULL
  }))

  if (length(code_frames) > 0L) {
    all_c <- do.call(rbind, code_frames)
    all_c <- all_c[order(all_c$name, all_c$val, all_c$.src), ]
    key   <- paste0(all_c$name, "|||", all_c$val)
    rows  <- lapply(split(all_c, key), function(grp) {
      row <- grp[1L, , drop = FALSE]
      if (is.na(row$label_fr)) {
        fr <- grp$label_fr[!is.na(grp$label_fr)]
        if (length(fr) > 0L) row$label_fr <- fr[[1L]]
      }
      en <- unique(grp$label_en[!is.na(grp$label_en)])
      if (length(en) > 1L)
        warning("Conflicting English labels for ", row$name, " val=", row$val,
                ": using '", en[[1L]], "'", call. = FALSE)
      row
    })
    codes_merged <- do.call(rbind, rows)
    codes_merged$.src <- NULL
    rownames(codes_merged) <- NULL
    codes_merged <- codes_merged[, c("name","val","label_en","label_fr")]
  } else {
    codes_merged <- empty_codes()
  }

  # ---- Layout: first non-NULL source ----
  layout <- NULL
  for (p in parsed_list) {
    if (!is.null(p$layout)) { layout <- p$layout; break }
  }

  # Warn about layout/variable-table mismatches
  if (!is.null(layout) && nrow(vars_merged) > 0L) {
    only_lay <- setdiff(toupper(layout$name), toupper(vars_merged$name))
    if (length(only_lay) > 0L)
      warning("Variables in layout but not in variable labels: ",
              paste(only_lay, collapse = ", "), call. = FALSE)
  }

  list(variables = vars_merged, codes = codes_merged, layout = layout)
}


#' Parse all metadata from a PUMF version directory
#'
#' Detects every parseable command-file format in \code{version_dir}, runs all
#' applicable parsers, merges the results into the canonical schema, and writes
#' \code{metadata/variables.csv}, \code{metadata/codes.csv} (and optionally
#' \code{metadata/layout.csv}) under \code{version_dir}.
#'
#' Idempotent: skips parsing if \code{metadata/variables.csv} already exists
#' and \code{refresh = FALSE}.
#'
#' @param version_dir Path to the extracted version directory.
#' @param layout_mask Optional string to disambiguate when multiple command-file
#'   sets coexist in one directory (e.g. \code{"CDN"} for Census); passed
#'   through to \code{\link{parse_spss_split}} and
#'   \code{\link{parse_sas_cards}}.
#' @param refresh If \code{TRUE}, re-parse even if cached metadata exists.
#' @return \code{metadata_dir} path invisibly.
#' @keywords internal
pumf_parse_metadata <- function(version_dir,
                                layout_mask       = NULL,
                                metadata_encoding = NULL,
                                refresh           = FALSE) {
  metadata_dir <- file.path(version_dir, "metadata")

  if (!refresh && metadata_exists(version_dir)) {
    return(invisible(metadata_dir))
  }

  # Derive series and version from the directory path for registry lookup.
  # Standard layout:  <cache>/<series>/<version>/
  #   → series = basename(dirname(version_dir)), version = basename(version_dir)
  # Bundled layout:   <cache>/<series>/<year>/<type>/
  #   → series = basename(dirname(dirname(version_dir)))
  #   → version = "<year>/<type>"  (the parent is a year, not a series name)
  path_series  <- basename(dirname(version_dir))
  path_version <- basename(version_dir)
  if (grepl("^\\d{4}", path_series)) {
    path_version <- paste0(path_series, "/", path_version)
    path_series  <- basename(dirname(dirname(version_dir)))
  }
  reg <- pumf_registry_lookup(path_series, path_version)

  # For bundled versions the shared raw files are in dirname(version_dir);
  # metadata CSV output always goes to this version's own metadata/ subdir.
  source_dir <- if (grepl("/", path_version, fixed = TRUE) &&
                    .version_is_extracted(dirname(version_dir)))
    dirname(version_dir)
  else
    version_dir

  # Apply bundle_sps_mask only when reading from the shared bundle dir so it
  # cannot accidentally filter out the right file for standalone deposits.
  effective_sps_mask <- if (!identical(source_dir, version_dir)) reg$bundle_sps_mask else NULL
  formats <- detect_formats(source_dir, sps_mask = effective_sps_mask)
  if (length(formats) == 0L) {
    stop("No parseable metadata files found in: ", source_dir)
  }

  # If the registry supplies a bundled English command file (for surveys like
  # 1991 Census where the downloaded XMF is French-only), promote the bundled
  # file to eng and demote the downloaded file to fra.
  if (!is.null(reg$bundled_eng_sps) && !is.null(formats$spss_mono)) {
    bundled <- system.file("extdata", reg$bundled_eng_sps, package = "canpumf")
    if (nchar(bundled) > 0L && file.exists(bundled))
      formats$spss_mono <- list(eng = bundled, fra = formats$spss_mono$eng)
  }

  # Encoding helpers: NULL means "use parser default".
  enc_spss <- metadata_encoding  %||%  "Latin1"
  # LFS codebook is always CP1252 regardless of metadata_encoding.
  # CPSS variables.csv uses Latin1 unless overridden.
  enc_csv  <- metadata_encoding  %||%  "Latin1"

  parsed <- list()

  if (!is.null(formats$lfs_csv))
    parsed$lfs_csv    <- parse_lfs_codebook(formats$lfs_csv)   # always CP1252

  if (!is.null(formats$cpss_csv))
    parsed$cpss_csv   <- parse_cpss_csv(formats$cpss_csv, encoding = enc_csv)

  if (!is.null(formats$sas_cards))
    parsed$sas_cards  <- parse_sas_cards(formats$sas_cards,
                                          layout_mask = layout_mask,
                                          encoding    = enc_spss)

  if (!is.null(formats$spss_split))
    parsed$spss_split <- parse_spss_split(formats$spss_split,
                                           layout_mask = layout_mask,
                                           encoding    = enc_spss)

  if (!is.null(formats$spss_mono))
    parsed$spss_mono  <- parse_spss_mono(formats$spss_mono$eng,
                                          fra_sps_path = formats$spss_mono$fra,
                                          encoding     = enc_spss)

  if (!is.null(formats$spss_sav))
    parsed$spss_sav   <- parse_spss_sav(formats$spss_sav)

  if (!is.null(formats$sas_labels))
    parsed$sas_labels <- parse_sas_data_labels(formats$sas_labels$eng,
                                                fra_path = formats$sas_labels$fra,
                                                encoding = metadata_encoding %||% "CP1252")

  if (!is.null(formats$pdf_dict))
    parsed$pdf_dict <- parse_pdf_dictionary(formats$pdf_dict$eng,
                                             fra_pdf = formats$pdf_dict$fra)

  metadata <- merge_metadata(parsed)

  dir.create(metadata_dir, showWarnings = FALSE, recursive = TRUE)
  write_metadata(metadata, metadata_dir)

  invisible(metadata_dir)
}


# ============================================================
# SPSS .sav parser (haven)
# ============================================================

#' Parse an SPSS \code{.sav} data file for embedded metadata
#'
#' Uses \code{haven::read_sav()} to extract variable labels, value labels, and
#' SPSS format codes (which give type and decimal precision) without loading any
#' data rows. This is the primary metadata source for surveys such as CIS
#' 2016/2017 that ship only a \code{.sav} file and no separate SPSS command
#' files.
#'
#' @param sav_path Path to the \code{.sav} file.
#' @return Named list with elements \code{variables}, \code{codes},
#'   and \code{layout} (always \code{NULL} -- \code{.sav} files embed metadata
#'   but use a binary record format that does not translate to start/end
#'   positions).
#' @keywords internal
parse_spss_sav <- function(sav_path) {
  df <- haven::read_sav(sav_path, n_max = 0L)

  # Per-column attribute extraction helpers
  get_attr <- function(col, nm) {
    v <- attr(col, nm)
    if (is.null(v)) NA_character_ else as.character(v)
  }

  var_label  <- vapply(df, get_attr, character(1L), nm = "label")
  fmt_spss   <- vapply(df, get_attr, character(1L), nm = "format.spss")

  # Type from format leading letter: A = character, F/N/E/G = numeric
  fmt_letter <- toupper(substr(fmt_spss, 1L, 1L))
  type <- case_when(
    fmt_letter == "A"                      ~ "character",
    fmt_letter %in% c("F", "N", "E", "G") ~ "numeric",
    TRUE                                   ~ "character"
  )

  # Decimals from "Fn.d" format (e.g. "F8.2" -> 2, "F4.0" -> 0, "A6" -> NA)
  decimals <- as.integer(
    stringr::str_match(fmt_spss, "[Ff][0-9]+\\.([0-9]+)")[, 2L])

  # Variables with value labels are categorical; clear their decimals
  has_val_labels <- vapply(df, function(col) {
    lbls <- attr(col, "labels")
    !is.null(lbls) && length(lbls) > 0L
  }, logical(1L))
  type[has_val_labels]          <- "character"
  decimals[type == "character"] <- NA_integer_

  variables <- tibble::tibble(
    name         = toupper(names(df)),
    label_en     = var_label,
    label_fr     = NA_character_,
    type         = type,
    decimals     = decimals,
    missing_low  = NA_real_,
    missing_high = NA_real_
  )

  # Value labels -> codes table
  code_rows <- lapply(seq_along(df), function(i) {
    lbls <- attr(df[[i]], "labels")
    if (is.null(lbls) || length(lbls) == 0L) return(NULL)
    tibble::tibble(
      name     = toupper(names(df)[i]),
      val      = as.character(lbls),   # numeric or character code -> string
      label_en = names(lbls),
      label_fr = NA_character_
    )
  })
  codes <- do.call(rbind, Filter(Negate(is.null), code_rows))
  if (is.null(codes)) codes <- empty_codes()

  list(variables = variables, codes = codes, layout = NULL)
}


# ============================================================
# StatCan PDF data dictionary parser (e.g. SFS 1999)
# ============================================================

# Internal: parse one language from a StatCan PDF Data Dictionary.
# Returns list(variables, codes, layout=NULL).
# "Codes:" / "Domaine:" sections produce regular codes.
# "Reserved Codes:" / "Codes Réservés:" sections produce reserved codes whose
# numeric values set missing_low/missing_high on the variable.
# "Range:" with a "lo:hi" value is a numeric range (no codes); "Range:" with
# value-label pairs is treated like "Codes:".
.parse_pdf_dict_single <- function(pdf_path, lang) {
  pages <- pdftools::pdf_text(pdf_path)
  lines <- unlist(strsplit(paste(pages, collapse = "\n"), "\n"))

  # Strip page headers, footers, and theme lines
  skip_rx <- paste0(
    "(?i)(^\\s*\\d{4}/\\d{2}/\\d{2}\\s+TH[E\\xc8]M",
    "|SFS DATA DICTIONARY",
    "|DICTIONNAIRE DE DONN",
    "|EXTERNAL CROSS-SECTIONAL",
    "|FICHIER EXTERNE TRANS",
    "|^\\s*Theme.Sub-theme:",
    "|^\\s*Th.+me.Sous-th.+me:)"
  )
  lines <- lines[!grepl(skip_rx, lines, perl = TRUE)]

  # Variable header: 1 leading space + NAME + spaces + "Position:"
  var_rx <- "^ ([A-Z][A-Z0-9_.]+)\\s+Position:"
  starts <- which(grepl(var_rx, lines))
  if (length(starts) == 0L) return(NULL)

  label_key <- if (lang == "eng") "Long name:" else "Long nom:"

  vars_list  <- vector("list", length(starts))
  codes_list <- vector("list", length(starts))

  for (i in seq_along(starts)) {
    s   <- starts[i]
    e   <- if (i < length(starts)) starts[i + 1L] - 1L else length(lines)
    blk <- lines[s:e]

    name <- regmatches(blk[1L], regexpr("[A-Z][A-Z0-9_.]+", blk[1L]))
    type <- if (grepl("Character", blk[1L], ignore.case = TRUE)) "character" else "numeric"

    ln_i  <- grep(label_key, blk, fixed = TRUE)
    label <- if (length(ln_i) > 0L)
      trimws(sub(paste0(".*", label_key, "\\s*"), "", blk[ln_i[1L]],
                 ignore.case = TRUE, perl = TRUE))
    else NA_character_

    state       <- "other"
    is_reserved <- FALSE
    cur_val     <- NA_character_
    cur_lbl     <- NA_character_
    code_rows   <- list()

    flush_code <- function() {
      if (!is.na(cur_val))
        code_rows[[length(code_rows) + 1L]] <<-
          list(val = cur_val, label = cur_lbl, reserved = is_reserved)
      cur_val <<- NA_character_
      cur_lbl <<- NA_character_
    }

    try_parse_code <- function(txt) {
      m <- regmatches(txt,
                      regexec("^([A-Za-z0-9:.]+)\\s{2,}(.+)$",
                              trimws(txt), perl = TRUE))[[1L]]
      if (length(m) == 3L) {
        flush_code()
        cur_val <<- m[2L]
        cur_lbl <<- trimws(m[3L])
      } else if (!is.na(cur_val) && nchar(trimws(txt)) > 0L) {
        cur_lbl <<- paste(cur_lbl, trimws(txt))
      }
    }

    for (j in seq_along(blk)[-1L]) {
      ln <- blk[j]

      # "Reserved Codes:" / "Codes Réservés:" — must be checked before plain "Codes:"
      if (grepl("reserv.*codes?|codes?.*r.serv|Codes.*R.serv",
                ln, ignore.case = TRUE, perl = TRUE)) {
        flush_code(); state <- "reserved"; is_reserved <- TRUE
        rest <- trimws(sub(
          "^.*(?:Reserved Codes?:|Codes? R.serv.s?:)\\s*", "",
          ln, ignore.case = TRUE, perl = TRUE))
        if (nchar(rest) > 0L) try_parse_code(rest)
        next
      }

      # "Codes:" or "Domaine:" (French equivalent for code-like value lists)
      if (grepl("^\\s+(Codes:|Domaine:)\\s*", ln, ignore.case = TRUE)) {
        flush_code(); state <- "codes"; is_reserved <- FALSE
        rest <- trimws(sub("^.*(?:Codes:|Domaine:)\\s*", "", ln,
                           ignore.case = TRUE, perl = TRUE))
        if (nchar(rest) > 0L) try_parse_code(rest)
        next
      }

      # "Range:" — numeric lo:hi skipped; value-label pairs treated as codes
      if (grepl("^\\s+Range:\\s*", ln, ignore.case = TRUE)) {
        flush_code()
        rest <- trimws(sub("^.*Range:\\s*", "", ln, ignore.case = TRUE, perl = TRUE))
        if (grepl("^\\d+:\\d+$", rest)) {
          state <- "other"
        } else {
          state <- "range"; is_reserved <- FALSE
          if (nchar(rest) > 0L) try_parse_code(rest)
        }
        next
      }

      # Known non-code section headers — exit code-collecting state
      if (grepl(
        "^\\s+(Long name:|Long nom:|Description:|Population:|Format:|Plage:|Etendue:)",
        ln, ignore.case = TRUE)) {
        flush_code(); state <- "other"; next
      }

      if (state %in% c("codes", "reserved", "range")) {
        rest <- trimws(ln)
        if (nchar(rest) == 0L) { flush_code(); state <- "other"; next }
        try_parse_code(rest)
      }
    }
    flush_code()

    # Compute missing range ONLY from reserved codes whose labels match
    # sentinel phrases (e.g. "Not Stated", "Don't Know", "Not Applicable").
    # Some StatCan PDFs put real category labels (e.g. "Yes", "No") under
    # "Reserved Codes:" alongside sentinels; using all reserved codes as the
    # missing range would incorrectly mask those real values.
    resv_rows     <- Filter(function(r) r$reserved, code_rows)
    resv_sentinel <- Filter(function(r) {
      grepl(.sentinel_pat, trimws(r$label), perl = TRUE)
    }, resv_rows)
    resv_num <- suppressWarnings(as.numeric(
      vapply(resv_sentinel, `[[`, character(1L), "val")))
    resv_num <- resv_num[!is.na(resv_num)]

    vars_list[[i]] <- tibble::tibble(
      name         = name,
      label_en     = if (lang == "eng") label else NA_character_,
      label_fr     = if (lang == "fra") label else NA_character_,
      type         = type,
      decimals     = NA_integer_,
      missing_low  = if (length(resv_num) > 0L) min(resv_num) else NA_real_,
      missing_high = if (length(resv_num) > 0L) max(resv_num) else NA_real_
    )

    if (length(code_rows) > 0L) {
      codes_list[[i]] <- tibble::tibble(
        name     = name,
        val      = vapply(code_rows, `[[`, character(1L), "val"),
        label_en = if (lang == "eng")
          vapply(code_rows, `[[`, character(1L), "label")
        else rep(NA_character_, length(code_rows)),
        label_fr = if (lang == "fra")
          vapply(code_rows, `[[`, character(1L), "label")
        else rep(NA_character_, length(code_rows))
      )
    }
  }

  list(
    variables = dplyr::bind_rows(vars_list),
    codes     = dplyr::bind_rows(codes_list),
    layout    = NULL
  )
}

#' Parse a StatCan PDF Data Dictionary for variable and code labels
#'
#' Extracts variable long-names and code-value labels from the bilingual PDF
#' data dictionaries shipped with some older StatCan PUMF releases (e.g. SFS
#' 1999).  Positions in the PDF do not match the PUMF flat file; this parser
#' produces only \code{variables} and \code{codes} (no \code{layout}).
#'
#' @param eng_pdf Path to the English \emph{Dictionary.pdf}.
#' @param fra_pdf Optional path to the French \emph{Dictionnaire.pdf}.
#' @return Named list with elements \code{variables}, \code{codes},
#'   \code{layout} (always \code{NULL}).
#' @keywords internal
parse_pdf_dictionary <- function(eng_pdf, fra_pdf = NULL) {
  eng <- .parse_pdf_dict_single(eng_pdf, "eng")
  if (is.null(eng)) return(NULL)

  if (!is.null(fra_pdf)) {
    fra <- .parse_pdf_dict_single(fra_pdf, "fra")
    if (!is.null(fra)) {
      # Merge French variable labels
      merged_vars <- merge(
        eng$variables,
        fra$variables[c("name", "label_fr")],
        by = "name", all.x = TRUE, suffixes = c("", ".fra")
      )
      merged_vars$label_fr     <- merged_vars$label_fr.fra
      merged_vars$label_fr.fra <- NULL
      eng$variables <- tibble::as_tibble(merged_vars)

      # Merge French code labels
      merged_codes <- merge(
        eng$codes,
        fra$codes[c("name", "val", "label_fr")],
        by = c("name", "val"), all.x = TRUE, suffixes = c("", ".fra")
      )
      merged_codes$label_fr     <- merged_codes$label_fr.fra
      merged_codes$label_fr.fra <- NULL
      eng$codes <- tibble::as_tibble(merged_codes)
    }
  }

  eng
}
