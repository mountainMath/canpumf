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
#   codes_supplement   — one row per supplemented (variable, val) pair
#   missing_supplement — one row per variable (value = "lo-hi" range)
#   labels_supplement  — one row per variable (value = supplied label_en)
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
    for (val in fx$na_values)
      add(series, version, "na_values", "", val)
    if (!is.null(fx$cols_swap))
      for (i in seq_along(fx$cols_swap))
        add(series, version, "cols_swap",
            names(fx$cols_swap)[i], unname(fx$cols_swap[i]))
    if (!is.null(fx$rename))
      for (i in seq_along(fx$rename))
        add(series, version, "rename",
            names(fx$rename)[i], unname(fx$rename[i]))
    if (!is.null(fx$missing_supplement))
      for (nm in names(fx$missing_supplement))
        add(series, version, "missing_supplement", nm,
            paste(fx$missing_supplement[[nm]], collapse = "-"))
    if (!is.null(fx$codes_supplement))
      for (nm in names(fx$codes_supplement)) {
        df <- fx$codes_supplement[[nm]]
        for (j in seq_len(nrow(df)))
          add(series, version, "codes_supplement", nm, df$val[j])
      }
    if (!is.null(fx$labels_supplement))
      for (nm in names(fx$labels_supplement))
        add(series, version, "labels_supplement", nm,
            unname(fx$labels_supplement[[nm]]["label_en"]))
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
    }
  }
  do.call(rbind, rows)
}

override_key <- function(d) {
  paste(d$series, d$version, d$override_type, d$variable, d$value, sep = " | ")
}
