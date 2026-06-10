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
  for (entry in registry) {
    fx <- entry$data_fixups
    if (length(fx) == 0L) next
    for (v in fx$force_numeric)
      add(entry$series, entry$version, "force_numeric", v)
    for (val in fx$na_values)
      add(entry$series, entry$version, "na_values", "", val)
    if (!is.null(fx$cols_swap))
      for (i in seq_along(fx$cols_swap))
        add(entry$series, entry$version, "cols_swap",
            names(fx$cols_swap)[i], unname(fx$cols_swap[i]))
    if (!is.null(fx$rename))
      for (i in seq_along(fx$rename))
        add(entry$series, entry$version, "rename",
            names(fx$rename)[i], unname(fx$rename[i]))
    if (!is.null(fx$missing_supplement))
      for (nm in names(fx$missing_supplement))
        add(entry$series, entry$version, "missing_supplement", nm,
            paste(fx$missing_supplement[[nm]], collapse = "-"))
    if (!is.null(fx$codes_supplement))
      for (nm in names(fx$codes_supplement)) {
        df <- fx$codes_supplement[[nm]]
        for (j in seq_len(nrow(df)))
          add(entry$series, entry$version, "codes_supplement", nm, df$val[j])
      }
  }
  do.call(rbind, rows)
}

override_key <- function(d) {
  paste(d$series, d$version, d$override_type, d$variable, d$value, sep = " | ")
}
