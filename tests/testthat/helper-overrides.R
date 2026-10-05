# Enumerate all manual overrides declared in the survey registry.
#
# Shared between the test suite (test-override-verification.R) and the dev
# verification workflow (tools/verify_overrides.R).  One row per individual
# override claim that should be checked against the PDF documentation:
#
#   force_numeric      — one row per variable (value = "")
#   na_values          — one row per sentinel value (variable = "")
#   cols_swap          — one row per swapped pair (variable = lhs, value = rhs)
#   rename             — one row per renamed pair (variable = old, value = new)
#   rename_regex       — one row per pattern (variable = pattern, value = replacement)
#   codes_supplement   — one row per supplemented (variable, val) pair
#   codes_override     — one row per overridden (variable, val) pair
#   missing_supplement — one row per variable (value = "lo-hi" range)
#   missing_codes      — one row per (variable, code) pair; an empty vector
#                        (the variable has no missing code, the parsed range is
#                        cleared) gives one row with value = ""
#   labels_supplement  — one row per variable (value = supplied label_en)
#   force_character / force_integer / force_bigint
#                      — one row per variable (storage type kept or overridden)
#   fix_mojibake       — one row per entry that repairs double-encoded text in
#                        the data (variable = "", value = "TRUE")
#   removed_records    — one row per raw value that sends a record to the
#                        "removed" sidecar (variable = the flag variable)
#   keep_unlabelled_codes — one row per entry (variable = "", value = "TRUE")
#                        or per named variable (value = "")
#   layout_file        — one row per entry or module that names its record
#                        layout's command file (variable = module id, "" for a
#                        single-table survey; value = the pattern)
enumerate_registry_overrides <- function(registry = canpumf:::.pumf_registry) {
  rows <- list()
  add <- function(series, version, type, variable = "", value = "") {
    rows[[length(rows) + 1L]] <<- data.frame(
      series        = series,
      version       = version,
      override_type = type,
      variable      = variable,
      value         = value,
      stringsAsFactors = FALSE
    )
  }
  # Enumerate every override claim in one data_fixups list.
  add_fixups <- function(series, version, fx) {
    if (length(fx) == 0L) return(invisible())
    for (v in fx$force_numeric)
      add(series, version, "force_numeric", v)
    for (v in fx$force_character)
      add(series, version, "force_character", v)
    for (v in fx$force_integer)
      add(series, version, "force_integer", v)
    for (v in fx$force_bigint)
      add(series, version, "force_bigint", v)
    for (val in fx$na_values)
      add(series, version, "na_values", "", val)
    if (!is.null(fx$cols_swap))
      for (i in seq_along(fx$cols_swap))
        add(series, version, "cols_swap",
            names(fx$cols_swap)[i], unname(fx$cols_swap[i]))
    # [["rename"]], not $rename: `$` partial-matches, so an entry declaring only
    # rename_regex would be enumerated under both types.
    if (!is.null(fx[["rename"]]))
      for (i in seq_along(fx[["rename"]]))
        add(series, version, "rename",
            names(fx[["rename"]])[i], unname(fx[["rename"]][i]))
    if (!is.null(fx$rename_regex))
      for (i in seq_along(fx$rename_regex))
        add(series, version, "rename_regex",
            names(fx$rename_regex)[i], unname(fx$rename_regex[i]))
    if (!is.null(fx$missing_supplement))
      for (nm in names(fx$missing_supplement))
        add(series, version, "missing_supplement", nm,
            paste(fx$missing_supplement[[nm]], collapse = "-"))
    if (!is.null(fx$missing_codes))
      for (nm in names(fx$missing_codes)) {
        vals <- fx$missing_codes[[nm]]
        # numeric(0) is itself a claim: the variable has no missing code and
        # its parsed range is cleared (Census 1981 HHINC, whose declared 0 is
        # the codebook's "ZERO", a value).
        if (length(vals) == 0L) add(series, version, "missing_codes", nm, "")
        for (val in vals)
          add(series, version, "missing_codes", nm, as.character(val))
      }
    if (!is.null(fx$codes_supplement))
      for (nm in names(fx$codes_supplement)) {
        df <- fx$codes_supplement[[nm]]
        for (j in seq_len(nrow(df)))
          add(series, version, "codes_supplement", nm, df$val[j])
      }
    if (!is.null(fx$codes_override))
      for (nm in names(fx$codes_override)) {
        df <- fx$codes_override[[nm]]
        for (j in seq_len(nrow(df)))
          add(series, version, "codes_override", nm, df$val[j])
      }
    # sentinel_labels: only the per-variable form is a claim about one
    # variable's codes.  The code-keyed form ("9999999" = ...) is the survey-wide
    # convention already recorded under the na_values rows for the same codes.
    if (!is.null(fx$sentinel_labels))
      for (nm in names(fx$sentinel_labels))
        if (!grepl("^[0-9.-]+$", nm))
          for (code in names(fx$sentinel_labels[[nm]]))
            add(series, version, "sentinel_labels", nm, code)
    if (!is.null(fx$labels_supplement))
      for (nm in names(fx$labels_supplement))
        add(series, version, "labels_supplement", nm,
            unname(fx$labels_supplement[[nm]]["label_en"]))
    if (isTRUE(fx$fix_mojibake))
      add(series, version, "fix_mojibake", "", "TRUE")
    if (!is.null(fx$removed_records))
      for (val in fx$removed_records$values)
        add(series, version, "removed_records", fx$removed_records$var,
            as.character(val))
    if (isTRUE(fx$keep_unlabelled_codes))
      add(series, version, "keep_unlabelled_codes", "", "TRUE")
    else for (v in fx$keep_unlabelled_codes)
      add(series, version, "keep_unlabelled_codes", v)
  }
  for (entry in registry) {
    # Top-level data_fixups (for multi-module surveys this is the primary
    # module's, auto-derived by .make_entry()).
    add_fixups(entry$series, entry$version, entry$data_fixups)
    # Secondary modules carry their own data_fixups (e.g. an Episode module's
    # force_numeric); enumerate them so their overrides are ledger-checked too.
    if (!is.null(entry$modules)) {
      pm <- if (is.null(entry$primary_module)) names(entry$modules)[[1L]]
            else entry$primary_module
      for (id in setdiff(names(entry$modules), pm))
        add_fixups(entry$series, entry$version, entry$modules[[id]]$data_fixups)
      # layout_file is per module; the entry level only mirrors the primary's.
      for (id in names(entry$modules))
        if (!is.null(entry$modules[[id]]$layout_file))
          add(entry$series, entry$version, "layout_file", id,
              entry$modules[[id]]$layout_file)
    } else if (!is.null(entry$layout_file)) {
      add(entry$series, entry$version, "layout_file", "", entry$layout_file)
    }
  }
  do.call(rbind, rows)
}

override_key <- function(d) {
  paste(d$series, d$version, d$override_type, d$variable, d$value, sep = " | ")
}
