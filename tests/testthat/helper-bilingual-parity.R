# Shared bilingual parity helper.
# Asserts that eng and fra DuckDB tables are structurally identical:
#   - same column names
#   - same row count
#   - same column types (numeric vs categorical)
#   - same values for every numeric column
#   - same NA pattern for every categorical column
#   - at least one categorical column has different eng/fra labels
#
# eng, fra : collected data.frames from pumf_open_duckdb()
# label    : prefix string for testthat failure messages

expect_pumf_bilingual_parity <- function(eng, fra, label = "") {
  pfx <- if (nchar(label) > 0L) paste0(label, ": ") else ""

  # 1. Same column names (variable codes, not labels)
  expect_equal(sort(names(eng)), sort(names(fra)),
    label = paste0(pfx, "column names must match"))
  if (!setequal(names(eng), names(fra))) return(invisible(NULL))

  # 2. Same row count
  expect_equal(nrow(eng), nrow(fra),
    label = paste0(pfx, "row counts must match"))
  if (nrow(eng) != nrow(fra)) return(invisible(NULL))

  fra <- fra[, names(eng), drop = FALSE]   # align column order

  is_num_eng <- vapply(names(eng), function(col) is.numeric(eng[[col]]), logical(1L))
  is_num_fra <- vapply(names(fra), function(col) is.numeric(fra[[col]]), logical(1L))

  # 3. Column types (numeric vs categorical) must agree
  type_mismatch <- names(eng)[is_num_eng != is_num_fra]
  expect_equal(
    length(type_mismatch), 0L,
    label = paste0(pfx, "column type mismatch (numeric vs categorical): ",
                   paste(type_mismatch, collapse = ", "))
  )

  # 4. Numeric columns: identical values (labels play no role)
  for (col in names(eng)[is_num_eng & is_num_fra]) {
    expect_equal(eng[[col]], fra[[col]],
      label = paste0(pfx, "numeric column '", col, "'"))
  }

  # 5. Categorical columns: same NA pattern
  cat_cols <- names(eng)[!is_num_eng & !is_num_fra]
  for (col in cat_cols) {
    e_na <- is.na(eng[[col]])
    f_na <- is.na(fra[[col]])
    if (!identical(e_na, f_na)) {
      expect_equal(
        sum(f_na), sum(e_na),
        label = paste0(pfx, "NA count for categorical column '", col,
                       "' (eng=", sum(e_na), ", fra=", sum(f_na), ")")
      )
    }
  }

  # 6. At least one categorical column must have different labels
  if (length(cat_cols) > 0L) {
    any_differ <- any(vapply(cat_cols, function(col) {
      e <- sort(unique(stats::na.omit(as.character(eng[[col]]))))
      f <- sort(unique(stats::na.omit(as.character(fra[[col]]))))
      !identical(e, f)
    }, logical(1L)))
    expect_true(any_differ,
      label = paste0(pfx,
        "at least one categorical column must have different eng/fra labels"))
  }

  invisible(NULL)
}


# Convenience: open a DuckDB table, collect, and disconnect.
.collect_pumf_table <- function(db_path, table_name) {
  tbl <- canpumf:::pumf_open_duckdb(db_path, table_name)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE))
  dplyr::collect(tbl)
}
