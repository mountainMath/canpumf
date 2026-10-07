# ---- Bilingual dictionary and translation of finished results ----------------
#
# An analysis is normally carried out in one language, but results are
# sometimes reported in both.  Every metadata/codes.csv and variables.csv
# carries both label_en and label_fr, so a result data frame labelled in one
# language can be relabelled in the other from the dictionary alone, without a
# second Stage 3 build.  The translation is keyed on (variable, label): the
# built tables carry labels, not codes, so two codes that share a label in the
# source language are already one level and stay one level (where both occur
# in the data the build has told them apart with a code suffix, and the
# dictionary carries the suffixed labels).

# Variable labels of derived LFS helper columns that are not in any
# variables.csv (add_lfs_columns()).
.lfs_derived_var_labels <- data.frame(
  name     = c("SURVDATE", "GENDER_SEX"),
  label_en = c("Survey date", "Gender/sex of respondent"),
  label_fr = c("Date d'enqu\u00eate", "Genre/sexe du r\u00e9pondant"),
  stringsAsFactors = FALSE)

# name -> label mapping for one language, duplicates disambiguated by
# appending " (NAME)".  Shared by label_pumf_columns() and pumf_translate(),
# which must agree on the disambiguated names.
.pumf_var_label_map <- function(variables, label_col) {
  m <- variables[!is.na(variables[[label_col]]), c("name", label_col),
                 drop = FALSE]
  names(m)[2L] <- "label"
  dups <- m$label[duplicated(m$label)]
  if (length(dups) > 0L) {
    is_dup <- m$label %in% dups
    m$label[is_dup] <- paste0(m$label[is_dup], " (", m$name[is_dup], ")")
  }
  as.data.frame(m, stringsAsFactors = FALSE)
}

# Registry sentinel_labels (data_fixups field) as dictionary rows: an entry
# keyed by a code applies to every variable (name NA), an entry keyed by a
# variable name holds per-code labels for that variable.
.pumf_sentinel_label_rows <- function(sentinel_labels) {
  empty <- data.frame(name = character(), val = character(),
                      label_en = character(), label_fr = character(),
                      stringsAsFactors = FALSE)
  if (length(sentinel_labels) == 0L) return(empty)
  one <- function(name, val, lab) {
    if (is.null(names(lab))) lab <- c(label_en = lab[[1L]])
    data.frame(name = name, val = val,
               label_en = if ("label_en" %in% names(lab)) lab[["label_en"]] else NA_character_,
               label_fr = if ("label_fr" %in% names(lab)) lab[["label_fr"]] else NA_character_,
               stringsAsFactors = FALSE)
  }
  rows <- lapply(names(sentinel_labels), function(key) {
    entry <- sentinel_labels[[key]]
    if (is.list(entry) && !is.null(names(entry)) &&
        !any(c("label_en", "label_fr") %in% names(entry))) {
      # per-variable: list(code = c(label_en=, label_fr=))
      do.call(rbind, lapply(names(entry), function(code)
        one(toupper(key), code, entry[[code]])))
    } else {
      one(NA_character_, key, entry)
    }
  })
  out <- do.call(rbind, c(list(empty), rows))
  out[!is.na(out$label_en) | !is.na(out$label_fr), , drop = FALSE]
}

# The versions loaded into a longitudinal database, oldest first, read through
# the registered connection when there is one (opening a second connection
# and shutting it down would invalidate the user's tbl).
.long_versions_from_prov <- function(prov) {
  spec    <- .pumf_longitudinal_spec(prov$series)
  db_path <- .long_db_path(spec, prov$cache_path)
  if (!file.exists(db_path))
    stop(prov$series, " database not found at '", db_path, "'.", call. = FALSE)
  existing_con <- prov$con
  versions <- if (!is.null(existing_con) && DBI::dbIsValid(existing_con))
    .long_read_versions(existing_con, spec)$version
  else
    .long_with_readonly_con(spec, prov$cache_path, strict = TRUE,
                            function(con) .long_read_versions(con, spec)$version)
  if (length(versions) == 0L)
    stop("No ", prov$series, " versions found in the database.", call. = FALSE)
  versions
}

# The registry entry, module and metadata directory of a non-longitudinal
# provenance record (series, version, cache_path, module).
.pumf_prov_meta <- function(prov) {
  series   <- prov$series
  reg      <- pumf_registry_lookup(series, prov$version)
  mods     <- .pumf_entry_modules(reg)
  mod      <- if (!is.null(prov$module) && !is.null(mods)) mods[[prov$module]]
  subdir   <- mod$meta_subdir
  meta_dir <- if (is.null(subdir))
    file.path(prov$cache_path, series, prov$version, "metadata")
  else
    file.path(prov$cache_path, series, prov$version, "metadata", subdir)
  if (!dir.exists(meta_dir))
    stop("Metadata directory not found: '", meta_dir, "'. ",
         "Run get_pumf(\"", series, "\", \"", prov$version, "\") first.",
         call. = FALSE)
  list(reg = reg, mod = mod, meta_dir = meta_dir,
       fix = if (!is.null(mod)) mod$data_fixups else reg$data_fixups)
}

# The provenance record behind the (x, version, module, cache_path) arguments
# of pumf_dictionary(): a get_pumf() tbl carries
# its own; a series name needs the version (except the longitudinal series).
.pumf_prov_from_arg <- function(x, version, module, cache_path) {
  if (is.character(x)) {
    if (length(x) != 1L)
      stop("'x' must be a single series name or a get_pumf() tbl.",
           call. = FALSE)
    series <- x
    if (identical(series, "LFS_TIMELINE"))
      return(list(series = series, version = NA_character_,
                  cache_path = cache_path, module = NULL))
    if (!.is_longitudinal(series) && is.null(version))
      stop("'version' is required when 'x' is a series name.", call. = FALSE)
    # Same aliases as get_pumf() ("2021" -> "2021 (individuals)", GSS cycles).
    version <- pumf_resolve_version(series, version, cache_path)
    return(list(series = series, version = version, cache_path = cache_path,
                module = module))
  }
  if (!inherits(x, "tbl_sql"))
    stop("'x' must be a lazy tbl from get_pumf() or a series name.",
         call. = FALSE)
  .pumf_tbl_prov(x, what = "'x'")
}

# variables + codes (both languages) for a provenance record, module-aware.
.pumf_dictionary_from_prov <- function(prov) {
  series   <- prov$series
  meta     <- NULL
  versions <- NULL
  if (identical(series, "LFS_TIMELINE")) {
    codes <- as.data.frame(.lfs_timeline_ref("codes"))
    sent  <- NULL
  } else if (.is_longitudinal(series)) {
    spec     <- .pumf_longitudinal_spec(series)
    versions <- .long_versions_from_prov(prov)
    if (is.null(spec$codes))
      stop("The ", series, " longitudinal spec has no 'codes' accessor.",
           call. = FALSE)
    codes <- as.data.frame(spec$codes(prov$cache_path, versions))
    sent  <- NULL
  } else {
    pm       <- .pumf_prov_meta(prov)
    meta_dir <- pm$meta_dir
    fix      <- pm$fix
    meta     <- read_metadata(meta_dir)
    # The value labels as Stage 3 applied them (registry code rows, French
    # fallback, the code suffix where the data hold several codes of one
    # label).  A cache built before 0.7.0 has no codes_applied.csv; its
    # table shows the documented labels, so those are returned.
    codes <- .read_codes_applied(meta_dir) %||%
      as.data.frame(.pumf_unique_code_labels(
        .pumf_apply_code_fixups(meta$codes, fix), present = list()))
    sent  <- .pumf_sentinel_label_rows(fix$sentinel_labels)
  }
  # The variable labels as label_pumf_columns() reads them (the same source,
  # so the two agree), plus the derived LFS helper columns for the shared
  # series.
  variables  <- .pumf_label_source(prov, meta = meta, versions = versions)
  for (d in setdiff(.metadata_description_cols, names(variables)))
    variables[[d]] <- rep(NA_character_, nrow(variables))
  codes$name <- toupper(codes$name)
  if (.is_shared_series(series)) {
    derived <- .pumf_derived_var_rows(variables$name)
    for (d in .metadata_description_cols)
      derived[[d]] <- rep(NA_character_, nrow(derived))
    variables <- rbind(variables[, c("name", "label_en", "label_fr",
                                     .metadata_description_cols)], derived)
  }
  vars <- data.frame(name = variables$name, val = NA_character_,
                     label_en = variables$label_en, label_fr = variables$label_fr,
                     description_en = variables$description_en,
                     description_fr = variables$description_fr,
                     applied_as = NA_character_, stringsAsFactors = FALSE)
  cds  <- data.frame(name = codes$name, val = as.character(codes$val),
                     label_en = codes$label_en, label_fr = codes$label_fr,
                     description_en = rep(NA_character_, nrow(codes)),
                     description_fr = rep(NA_character_, nrow(codes)),
                     applied_as = if ("applied_as" %in% names(codes))
                       as.character(codes$applied_as)
                     else rep(NA_character_, nrow(codes)),
                     stringsAsFactors = FALSE)
  if (!is.null(sent)) {
    sent$description_en <- rep(NA_character_, nrow(sent))
    sent$description_fr <- rep(NA_character_, nrow(sent))
    sent$applied_as     <- rep(NA_character_, nrow(sent))
  }
  out <- rbind(vars, cds, sent)
  # The build falls back to label_en where label_fr is missing (and to the
  # digits where neither exists), so the dictionary says what the table shows.
  na_fr <- is.na(out$label_fr)
  out$label_fr[na_fr] <- out$label_en[na_fr]
  na_en <- is.na(out$label_en)
  out$label_en[na_en] <- out$label_fr[na_en]
  out <- out[!is.na(out$label_en), , drop = FALSE]
  rownames(out) <- NULL
  tibble::as_tibble(out)
}

# The labelled values a non-longitudinal table keeps as numbers (top codes,
# bottom codes, labelled zeros): the dictionary's value rows applied as
# "value", which Stage 3 records in metadata/codes_applied.csv.
.pumf_dictionary_topcodes <- function(prov, dict) {
  codes <- .read_codes_applied(.pumf_prov_meta(prov)$meta_dir)
  if (is.null(codes) || !"applied_as" %in% names(codes))
    stop(prov$series, " ", prov$version, " was built by a canpumf version ",
         "that did not record how it applied the value labels. Rebuild it ",
         "with get_pumf(\"", prov$series, "\", \"", prov$version,
         "\", refresh = TRUE).", call. = FALSE)
  out <- dict[!is.na(dict$applied_as) & dict$applied_as == "value", ,
              drop = FALSE]
  out[order(out$name, suppressWarnings(as.numeric(out$val))), , drop = FALSE]
}


# ---- pumf_dictionary --------------------------------------------------------

#' Bilingual label dictionary of a PUMF
#'
#' Returns every variable label and value label of a survey in both English
#' and French, as one tibble.  It is the lookup [pumf_translate()] uses to
#' relabel a finished result in the other language, and it is a plain data
#' frame: it stays usable after [close_pumf()] and can be extended with
#' custom rows.
#'
#' The dictionary comes from the survey's cached `metadata/` files
#' (`variables.csv`, `codes.csv`, plus labels the registry supplies for
#' sentinel codes), so it is available as soon as the survey has been built in
#' either language.  For the longitudinal series (`"LFS"`, `"LFS_HIST"`) it
#' merges the metadata of every loaded version, keeping every distinct wording,
#' since each version was labelled from its own metadata when it was appended.
#'
#' Where the source documents a label in one language only, that label is
#' repeated in the other column, which is what the built table shows.
#'
#' @section Top codes:
#' Some count, age and amount variables carry a label on one or two of their
#' values only: the top code ("75 and more" on hours worked, "85 years and
#' over" on age), sometimes a bottom code or a labelled zero ("None").
#' canpumf keeps such a variable numeric, so that its unlabelled values are
#' not lost, and drops the label from the table: a 75 is a plain 75 although
#' it stands for 75 or more.  These are the rows with `applied_as` `"value"`,
#' and `what = "topcodes"` returns just them, so that a mean or a range can be
#' read with the ceiling in mind.  Sentinel codes of the same variables ("Not
#' stated", "Don't know") have `applied_as` `"sentinel"`: they become `NA` in
#' the table and are reported, with their labels, by [pumf_sidecar()].
#' `applied_as` is read from `metadata/codes_applied.csv`, which the build
#' writes; for a database built by an earlier canpumf version it is `NA`, and
#' `what = "topcodes"` asks for a rebuild with `get_pumf(..., refresh = TRUE)`.
#' The longitudinal series have no such variables, and `what = "topcodes"` is
#' an error for them.
#'
#' @param x A lazy `dplyr::tbl()` returned by [get_pumf()] or
#'   `get_pumf("LFS_TIMELINE")` ([lfs_timeline]), or a series name (`"SFS"`).
#' @param version The version, when `x` is a series name; the aliases
#'   [get_pumf()] accepts work here too (`"2021"` for the Census individuals
#'   file, `"Cycle 31"` or `"2017"` for a GSS cycle).  Ignored for a tbl.
#' @param module For a multi-module survey given by name, the module whose
#'   dictionary to return (default: the primary module).  A tbl carries its
#'   module.
#' @param cache_path Root cache directory, when `x` is a series name.
#' @param what Which rows to return: `"all"` (default), `"variables"` (the
#'   variable labels, one row per variable), `"values"` (the value labels) or
#'   `"topcodes"` (the labelled values the table keeps as numbers, sorted by
#'   variable and value; see the section below).
#'
#' @return A tibble with columns `name`, `val`, `label_en`, `label_fr`,
#'   `description_en`, `description_fr` and `applied_as`.  Rows with `val` `NA`
#'   are variable labels; the other rows are value labels.  A row with `name`
#'   `NA` is a value label that applies to every variable (the registry's
#'   labels for Census sentinel codes).  `description_en` and `description_fr`
#'   hold, on variable rows, a longer explanation of the variable where the
#'   source documents one beside the short label (the CCRI census samples),
#'   `NA` otherwise.  `applied_as` says, on value rows, how the build applied
#'   the label: `"level"` of a factor column, `"value"` kept as a number (a
#'   top code), `"sentinel"` blanked to `NA`; `NA` where that was not
#'   recorded.
#'
#' @seealso [pumf_translate()], [label_pumf_columns()], [pumf_sidecar()]
#' @examples
#' \donttest{
#' sfs <- get_pumf("SFS", "2019")
#' if (!is.null(sfs)) {
#'   d <- pumf_dictionary(sfs)
#'   d[d$name == "PREGION", ]
#'   pumf_dictionary(sfs, what = "variables")
#'   close_pumf(sfs)
#' }
#' gss <- get_pumf("GSS", "Cycle 17 (2003)")
#' if (!is.null(gss)) {
#'   pumf_dictionary(gss, what = "topcodes")   # WKWEHR 75 "75 and more", ...
#'   close_pumf(gss)
#' }
#' }
#' @export
pumf_dictionary <- function(x, version = NULL, module = NULL,
                            cache_path = getOption("canpumf.cache_path",
                                                   tempdir()),
                            what = c("all", "variables", "values", "topcodes")) {
  what <- match.arg(what)
  prov <- .pumf_prov_from_arg(x, version, module, cache_path)
  if (what == "topcodes" && .is_shared_series(prov$series))
    stop("what = \"topcodes\" is not available for the longitudinal series (",
         prov$series, ").", call. = FALSE)
  dict <- .pumf_dictionary_from_prov(prov)
  out  <- switch(what,
    all       = dict,
    variables = dict[is.na(dict$val) & !is.na(dict$name), , drop = FALSE],
    values    = dict[!is.na(dict$val), , drop = FALSE],
    topcodes  = .pumf_dictionary_topcodes(prov, dict))
  out
}


# ---- pumf_translate ---------------------------------------------------------

# Normalise the `custom` argument into dictionary rows (name, val = NA,
# label_en, label_fr).  A named character vector reads source -> target in the
# direction of the translation.
.pumf_custom_rows <- function(custom, from_col, to_col) {
  empty <- tibble::tibble(name = character(), val = character(),
                          label_en = character(), label_fr = character())
  if (is.null(custom) || length(custom) == 0L) return(empty)
  if (is.character(custom)) {
    if (is.null(names(custom)) || any(!nzchar(names(custom))))
      stop("A character 'custom' must be a named vector: ",
           "c(\"source label\" = \"translation\").", call. = FALSE)
    out <- tibble::tibble(name = NA_character_, val = NA_character_,
                          label_en = NA_character_, label_fr = NA_character_,
                          .rows = length(custom))
    out[[from_col]] <- unname(names(custom))
    out[[to_col]]   <- unname(custom)
    return(out)
  }
  if (!is.data.frame(custom) ||
      !all(c("label_en", "label_fr") %in% names(custom)))
    stop("'custom' must be a named character vector or a data frame with ",
         "columns 'label_en' and 'label_fr' (and optionally 'name').",
         call. = FALSE)
  tibble::tibble(
    name     = if ("name" %in% names(custom)) toupper(as.character(custom$name))
               else NA_character_,
    val      = if ("val" %in% names(custom)) as.character(custom$val)
               else NA_character_,
    label_en = as.character(custom$label_en),
    label_fr = as.character(custom$label_fr))
}

# Resolve `dict` (a dictionary tibble or a get_pumf() tbl) to a tibble.
.pumf_translate_dict <- function(dict) {
  if (is.null(dict)) return(NULL)
  if (inherits(dict, "tbl_lazy")) return(pumf_dictionary(dict))
  if (!is.data.frame(dict) ||
      !all(c("name", "val", "label_en", "label_fr") %in% names(dict)))
    stop("'dict' must be a dictionary from pumf_dictionary() or a lazy tbl ",
         "from get_pumf().", call. = FALSE)
  tibble::as_tibble(dict)
}

#' Translate the labels of a result between English and French
#'
#' Relabels a data frame produced from a [get_pumf()] table (after
#' `dplyr::collect()`, typically a summary table) from one language to the
#' other, using the survey's bilingual dictionary.  Factor levels and the
#' values of character columns are translated, and column names that are
#' variable labels (from [label_pumf_columns()]) are translated too.  Columns
#' that keep their coded names (`PROV`) are left alone.
#'
#' The translation is keyed on the column and the label.  A column is matched
#' to a survey variable by its coded name, by its variable label in the source
#' language, or, for the `<VAR>_sentinel` columns of
#' `pumf_sidecar("sentinels", join = TRUE)`, by the variable it annotates.
#' Its labels are then looked up among that variable's codes.  Custom entries and the
#' dictionary's variable-independent rows apply to every column.
#'
#' Labels the analysis introduced (`forcats::fct_collapse()`, a `case_when()`
#' recode) are not in the dictionary.  They are kept unchanged and reported
#' once as a warning; supply their translations through `custom`.  Two source
#' levels that translate to the same label are merged into one level.
#'
#' @param x A data frame.  Grouped tibbles keep their grouping.  A lazy tbl is
#'   rejected: `collect()` it first.
#' @param to Target language, `"fra"` (default) or `"eng"`.  The source
#'   language is the other one.
#' @param dict The survey dictionary from [pumf_dictionary()], or the lazy tbl
#'   from [get_pumf()] the result was computed from (its dictionary is looked
#'   up).  May be `NULL` when `custom` covers every label.
#' @param custom Translations for labels the dictionary does not know.  Either
#'   a named character vector read in the direction of the translation
#'   (`c("Young" = "Jeune")`), or a data frame with columns `label_en` and
#'   `label_fr` and an optional `name` (a coded variable name to restrict the
#'   entry to, `NA` for any column).  Custom entries take precedence over the
#'   dictionary.  They also rename a column whose name matches.
#' @param warn Warn about labels that were left untranslated (default `TRUE`).
#'
#' @return `x` with translated levels, values and column names.  The attribute
#'   `"pumf_translation"` holds a tibble of what happened to every label:
#'   `column`, `from`, `to` and `status` (`"translated"`, `"custom"`,
#'   `"ambiguous"` when the source label has several translations and the
#'   first was used, or `"untranslated"`).
#'
#' @seealso [pumf_dictionary()], [label_pumf_columns()]
#' @examples
#' \donttest{
#' sfs <- get_pumf("SFS", "2019")
#' if (!is.null(sfs)) {
#'   res <- sfs |>
#'     dplyr::count(PREGION, wt = PWEIGHT) |>
#'     dplyr::collect()
#'   pumf_translate(res, "fra", dict = sfs)
#'
#'   # A recoded level the survey does not know
#'   levels(res$PREGION)[levels(res$PREGION) %in%
#'                         c("Prairie provinces", "British Columbia")] <- "West"
#'   pumf_translate(res, "fra", dict = sfs, custom = c(West = "Ouest"))
#'   close_pumf(sfs)
#' }
#' }
#' @export
pumf_translate <- function(x, to = c("fra", "eng"), dict = NULL,
                           custom = NULL, warn = TRUE) {
  to <- match.arg(to)
  if (inherits(x, "tbl_lazy"))
    stop("pumf_translate() works on collected data frames; ",
         "dplyr::collect() the tbl first.", call. = FALSE)
  if (!is.data.frame(x))
    stop("'x' must be a data frame.", call. = FALSE)
  from_col <- if (to == "fra") "label_en" else "label_fr"
  to_col   <- if (to == "fra") "label_fr" else "label_en"

  dict   <- .pumf_translate_dict(dict)
  custom <- .pumf_custom_rows(custom, from_col, to_col)
  if (is.null(dict) && nrow(custom) == 0L)
    stop("Supply 'dict' (from pumf_dictionary() or the get_pumf() tbl) ",
         "and/or 'custom'.", call. = FALSE)
  if (is.null(dict))
    dict <- custom[0L, ]

  # Entries: name (NA = any column), from, to, kind.  Custom first so that it
  # wins on duplicates; within each source, variable-specific before global.
  mk <- function(d, kind) {
    d <- d[!is.na(d[[from_col]]) & !is.na(d[[to_col]]), , drop = FALSE]
    tibble::tibble(name = d$name, val = d$val, from = d[[from_col]],
                   to = d[[to_col]], kind = kind)
  }
  code_rows <- dict[!is.na(dict$val) | is.na(dict$name), , drop = FALSE]
  var_rows  <- dict[is.na(dict$val) & !is.na(dict$name), , drop = FALSE]
  entries   <- rbind(mk(custom, "custom"), mk(code_rows, "code"))

  # Column-name resolution
  variables <- data.frame(name = var_rows$name, label_en = var_rows$label_en,
                          label_fr = var_rows$label_fr, stringsAsFactors = FALSE)
  variables <- variables[!duplicated(variables$name), , drop = FALSE]
  from_map  <- .pumf_var_label_map(variables, from_col)   # name, label
  to_map    <- .pumf_var_label_map(variables, to_col)
  # A column belongs to a survey variable when the dictionary has a variable
  # row or code rows under that name.
  var_names <- unique(c(variables$name, code_rows$name[!is.na(code_rows$name)]))

  grp <- if (inherits(x, "grouped_df")) dplyr::group_vars(x) else NULL
  out <- if (!is.null(grp)) dplyr::ungroup(x) else x

  cols      <- names(out)
  new_names <- cols
  report    <- list()

  for (i in seq_along(cols)) {
    cn       <- cols[i]
    var_name <- NA_character_
    if (toupper(cn) %in% var_names) {
      var_name <- toupper(cn)
    } else if (grepl("_sentinel$", cn) &&
               toupper(sub("_sentinel$", "", cn)) %in% var_names) {
      var_name <- toupper(sub("_sentinel$", "", cn))
    } else if (cn %in% from_map$label) {
      var_name     <- from_map$name[match(cn, from_map$label)]
      tl           <- to_map$label[match(var_name, to_map$name)]
      if (!is.na(tl)) new_names[i] <- tl
    }
    # A custom entry naming the column renames it (custom wins over the map).
    cust_col <- entries[entries$kind == "custom" & is.na(entries$name) &
                          entries$from == cn, , drop = FALSE]
    if (nrow(cust_col) > 0L) new_names[i] <- cust_col$to[1L]

    col <- out[[i]]
    is_fct <- is.factor(col)
    if (!is_fct && !is.character(col)) next
    # Character columns are only translated when they belong to a survey
    # variable or when a custom entry matches; factors always are.
    src_levels <- if (is_fct) levels(col) else unique(col[!is.na(col)])
    if (length(src_levels) == 0L) next

    specific <- entries[!is.na(entries$name) & !is.na(var_name) &
                          entries$name == var_name, , drop = FALSE]
    global   <- entries[is.na(entries$name), , drop = FALSE]
    # Order: custom specific, custom global, dict specific, dict global.
    cand <- rbind(specific[specific$kind == "custom", ],
                  global[global$kind == "custom", ],
                  specific[specific$kind != "custom", ],
                  global[global$kind != "custom", ])
    cand <- cand[cand$from %in% src_levels, , drop = FALSE]
    if (!is_fct && is.na(var_name) && !any(cand$kind == "custom")) next

    # Ambiguity: a source label with several distinct translations among the
    # dictionary's rows for this variable.  The first (code order) is used.
    amb <- unique(specific$from[specific$kind == "code"][
      duplicated(unique(specific[specific$kind == "code", c("from", "to")])$from)])
    cand   <- cand[!duplicated(cand$from), , drop = FALSE]
    lookup <- stats::setNames(cand$to, cand$from)

    hit    <- src_levels %in% names(lookup)
    tgt    <- ifelse(hit, unname(lookup[src_levels]), src_levels)
    status <- ifelse(!hit, "untranslated",
              ifelse(src_levels %in% cand$from[cand$kind == "custom"], "custom",
              ifelse(src_levels %in% amb, "ambiguous", "translated")))
    # Unmatched values are only worth reporting for factors (categorical by
    # construction) and for character columns of a variable that has value
    # labels; a character variable without codes (the timeline's SOURCE) or a
    # column that is no survey variable at all is left alone quietly.
    if (!is_fct && (is.na(var_name) || !any(specific$kind == "code")))
      status[!hit] <- "kept"
    report[[length(report) + 1L]] <- tibble::tibble(
      column = cn, from = src_levels, to = tgt, status = status)

    if (is_fct) {
      levels(col) <- tgt   # duplicated targets merge levels
    } else {
      m <- match(col, names(lookup))
      col[!is.na(m)] <- unname(lookup[m[!is.na(m)]])
    }
    out[[i]] <- col
  }

  names(out) <- make.unique(new_names)
  if (!is.null(grp)) {
    grp_new <- new_names[match(grp, cols)]
    out <- dplyr::group_by(out, dplyr::across(dplyr::all_of(grp_new)))
  }

  report <- if (length(report) > 0L) do.call(rbind, report)
            else tibble::tibble(column = character(), from = character(),
                                to = character(), status = character())
  report <- report[report$status != "kept", , drop = FALSE]
  attr(out, "pumf_translation") <- report

  if (isTRUE(warn)) {
    miss <- report[report$status == "untranslated", , drop = FALSE]
    if (nrow(miss) > 0L) {
      by_col <- split(miss$from, miss$column)
      detail <- vapply(names(by_col), function(cn) {
        v <- by_col[[cn]]
        paste0(cn, ": ", paste(encodeString(utils::head(v, 4L), quote = "'"), collapse = ", "),
               if (length(v) > 4L) paste0(" ... (", length(v), ")"))
      }, character(1L))
      warning("pumf_translate(): ", nrow(miss), " label(s) in ",
              length(by_col), " column(s) have no ",
              if (to == "fra") "French" else "English",
              " translation and were kept. Supply them via 'custom': ",
              paste(detail, collapse = "; "), call. = FALSE)
    }
  }
  out
}
