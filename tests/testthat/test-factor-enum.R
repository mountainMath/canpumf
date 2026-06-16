# Spec implementations of the label-application contracts.
# These define what pumf_build_duckdb() must satisfy; the production versions
# in pipeline.R must produce identical results for any input.

apply_pumf_labels <- function(x, codes_for_var, lang = "eng") {
  label_col <- if (lang == "eng") "label_en" else "label_fr"
  vals      <- codes_for_var$val
  labels    <- codes_for_var[[label_col]]

  unmatched <- unique(x[!x %in% vals & !is.na(x)])
  if (length(unmatched) > 0)
    warning("Unmatched code values become NA: ", paste(sort(unmatched), collapse = ", "))

  lookup <- stats::setNames(labels, vals)
  factor(lookup[x], levels = unique(labels[!is.na(labels)]))
}

apply_numeric_missing <- function(x, missing_low, missing_high) {
  x <- as.numeric(x)
  if (!is.na(missing_low) && !is.na(missing_high))
    x[!is.na(x) & x >= missing_low & x <= missing_high] <- NA_real_
  x
}

# Helper: append df to a DuckDB table, extending the schema when columns differ.
# New columns in df → ALTER TABLE ADD COLUMN (NULL for existing rows).
# Columns in table missing from df → NA in the incoming data.
append_with_schema_evolution <- function(con, table_name, new_data) {
  if (!DBI::dbExistsTable(con, table_name)) {
    DBI::dbWriteTable(con, table_name, new_data)
    return(invisible(NULL))
  }

  existing_cols <- DBI::dbListFields(con, table_name)
  new_cols      <- names(new_data)

  for (col in setdiff(new_cols, existing_cols)) {
    sql_type <- if (is.integer(new_data[[col]])) "INTEGER"
    else if (is.numeric(new_data[[col]]))  "DOUBLE"
    else if (is.logical(new_data[[col]]))  "BOOLEAN"
    else "VARCHAR"
    DBI::dbExecute(con,
      sprintf('ALTER TABLE "%s" ADD COLUMN "%s" %s', table_name, col, sql_type))
  }

  # Re-read field list after ALTER so newly added columns are included
  all_cols <- DBI::dbListFields(con, table_name)

  for (col in setdiff(all_cols, new_cols))
    new_data[[col]] <- NA

  DBI::dbAppendTable(con, table_name, new_data[, all_cols, drop = FALSE])
  invisible(NULL)
}


# --- Factor level completeness -------------------------------------------

codes_prov <- tibble::tibble(
  name     = rep("PROV", 5),
  val      = c("10", "11", "12", "13", "24"),
  label_en = c("Newfoundland", "PEI", "Nova Scotia", "New Brunswick", "Quebec"),
  label_fr = c("Terre-Neuve", "Î.-P.-É.", "Nouvelle-Écosse", "Nouveau-Brunswick", "Québec")
)

test_that("factor has ALL levels from codes, even those absent from data", {
  x      <- c("10", "11", "10", NA)          # codes 12, 13, 24 not in data
  result <- apply_pumf_labels(x, codes_prov)

  expect_s3_class(result, "factor")
  expect_equal(nlevels(result), 5L)
  expect_setequal(levels(result), codes_prov$label_en)
})

test_that("factor levels follow codes.csv order, not data appearance order", {
  x      <- c("24", "10", "11")              # appear in reverse order
  result <- apply_pumf_labels(x, codes_prov)

  expect_equal(levels(result), codes_prov$label_en)   # not c("Quebec","Newfoundland","PEI")
})

test_that("unmatched raw values become NA and trigger a warning", {
  x <- c("10", "99", "ZZ")                  # 99 and ZZ not in codes
  expect_warning(
    result <- apply_pumf_labels(x, codes_prov),
    regexp = "99.*ZZ|ZZ.*99"
  )
  expect_true(is.na(result[2]))
  expect_true(is.na(result[3]))
  expect_false(is.na(result[1]))
})

test_that("NA in input remains NA in output without triggering unmatched warning", {
  x <- c("10", NA_character_)
  expect_no_warning(result <- apply_pumf_labels(x, codes_prov))
  expect_true(is.na(result[2]))
})


# --- Numeric missing-value substitution ----------------------------------

test_that("values in missing range become NA", {
  x      <- c("1", "97", "98", "99", "2")
  result <- apply_numeric_missing(x, missing_low = 98, missing_high = 99)

  expect_equal(result, c(1, 97, NA_real_, NA_real_, 2))
})

test_that("values outside missing range are unchanged", {
  result <- apply_numeric_missing(c("1", "97"), missing_low = 98, missing_high = 99)
  expect_equal(result, c(1, 97))
})

test_that("NA missing range leaves column numeric but untouched", {
  result <- apply_numeric_missing(c("5", "10"), missing_low = NA, missing_high = NA)
  expect_equal(result, c(5, 10))
  expect_type(result, "double")
})


# --- DuckDB ENUM storage -------------------------------------------------

duck_con <- function() DBI::dbConnect(duckdb::duckdb(), ":memory:")

test_that("R factor column is stored as ENUM in DuckDB", {
  con <- duck_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  df <- data.frame(
    province = factor(c("Ontario", "Quebec", NA),
                      levels = c("Ontario", "Quebec", "Alberta")),
    value    = c(1L, 2L, NA_integer_)
  )
  DBI::dbWriteTable(con, "t", df)

  info      <- DBI::dbGetQuery(con, "DESCRIBE t")
  prov_type <- info$column_type[info$column_name == "province"]

  expect_true(grepl("^ENUM", prov_type),
    label = paste0("Expected ENUM, got: '", prov_type, "'. ",
                   "Stage 3 must ALTER the column to ENUM if duckdb does not do this automatically."))
})

test_that("NA in factor round-trips as NULL/NA through DuckDB, not as string 'NA'", {
  con <- duck_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  df <- data.frame(x = factor(c("A", NA, "B"), levels = c("A", "B", "C")))
  DBI::dbWriteTable(con, "t", df)

  back <- DBI::dbGetQuery(con, "SELECT * FROM t")
  expect_true(is.na(back$x[2]))
  expect_false(identical(back$x[2], "NA"))
})

test_that("ENUM levels include values absent from the data", {
  con <- duck_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # Factor with level "C" not present in data
  df <- data.frame(x = factor(c("A", "B"), levels = c("A", "B", "C")))
  DBI::dbWriteTable(con, "t", df)

  info      <- DBI::dbGetQuery(con, "DESCRIBE t")
  col_type  <- info$column_type[info$column_name == "x"]

  # The ENUM type string should mention all three levels
  expect_true(grepl("C", col_type),
    label = paste0("ENUM type should contain all factor levels; got: '", col_type, "'"))
})


# --- Schema evolution (LFS append) ---------------------------------------

test_that("append_with_schema_evolution: new column added with NULLs for old rows", {
  con <- duck_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  df1 <- data.frame(SURVYEAR = 2022L, PROV = "Ontario",  WAGE = 30.0)
  df2 <- data.frame(SURVYEAR = 2023L, PROV = "Quebec",   WAGE = 35.0, NEWVAR = 1L)

  append_with_schema_evolution(con, "lfs", df1)
  append_with_schema_evolution(con, "lfs", df2)

  result <- DBI::dbGetQuery(con, "SELECT * FROM lfs ORDER BY SURVYEAR")

  expect_equal(nrow(result), 2L)
  expect_true("NEWVAR" %in% names(result))
  expect_true(is.na(result$NEWVAR[result$SURVYEAR == 2022]))
  expect_equal(result$NEWVAR[result$SURVYEAR == 2023], 1L)
})

test_that("append_with_schema_evolution: missing column in new data filled with NA", {
  con <- duck_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  df1 <- data.frame(SURVYEAR = 2022L, PROV = "ON", OLDVAR = 5L)
  df2 <- data.frame(SURVYEAR = 2023L, PROV = "QC")          # OLDVAR absent

  append_with_schema_evolution(con, "lfs", df1)
  append_with_schema_evolution(con, "lfs", df2)

  result <- DBI::dbGetQuery(con, "SELECT * FROM lfs ORDER BY SURVYEAR")

  expect_true(is.na(result$OLDVAR[result$SURVYEAR == 2023]))
  expect_equal(result$OLDVAR[result$SURVYEAR == 2022], 5L)
})

test_that("append_with_schema_evolution: identical schemas append cleanly", {
  con <- duck_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  df <- data.frame(SURVYEAR = 2022L, VAL = 1.5)
  append_with_schema_evolution(con, "lfs", df)
  append_with_schema_evolution(con, "lfs", data.frame(SURVYEAR = 2023L, VAL = 2.5))

  result <- DBI::dbGetQuery(con, "SELECT * FROM lfs")
  expect_equal(nrow(result), 2L)
})
