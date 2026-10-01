# Tests for add_bootstrap_weights(), remove_bootstrap_weights(), bsw_info(),
# pumf_var_labels(), list_canpumf_collection(), list_available_lfs_pumf_versions().

.bsw_cache <- function() getOption("canpumf.cache_path", "")

# Reuse the minimal e2e fixture from test-api.R so we can build a real DuckDB.
.make_bsw_dir <- function(tmp) {
  vdir     <- file.path(tmp, "FAKE", "2099")
  meta_dir <- file.path(vdir, "metadata")
  dir.create(meta_dir, recursive = TRUE)
  vars <- tibble::tibble(
    name = c("ID", "WEIGHT"),
    label_en = c("Record ID", "Survey weight"),
    label_fr = c("Identifiant", "Poids"),
    type = c("numeric", "numeric"),
    decimals = c(0L, 0L),
    missing_low = c(NA_real_, NA_real_),
    missing_high = c(NA_real_, NA_real_)
  )
  readr::write_csv(vars, file.path(meta_dir, "variables.csv"))
  readr::write_csv(
    tibble::tibble(name = character(), val = character(),
                   label_en = character(), label_fr = character()),
    file.path(meta_dir, "codes.csv")
  )
  readr::write_csv(
    tibble::tibble(ID = as.character(1:20), WEIGHT = as.character(rep(100L, 20))),
    file.path(vdir, "survey.csv")
  )
  writeLines("", file.path(vdir, "sentinel.txt"))
  vdir
}

# Build a DuckDB-backed survey table directly (with an optional strata column)
# and return a handle; used by the row-addition regeneration tests, which need
# to append rows to the physical table between add_bootstrap_weights() calls.
.bsw_db <- function(df, env = parent.frame(), series = "ROWT", version = "2020") {
  cache   <- withr::local_tempdir(.local_envir = env)
  s <- list(cache = cache, series = series, version = version, lang = "eng")
  s$db_path <- .pumf_db_path(s$series, s$version, cache)
  dir.create(dirname(s$db_path), recursive = TRUE, showWarnings = FALSE)
  s$tname <- .pumf_table_name(s$series, s$version, s$lang)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = s$db_path)
  DBI::dbWriteTable(con, s$tname, df)
  DBI::dbDisconnect(con, shutdown = TRUE)
  s
}
# A registered tbl on the handle's table.  The default is a write connection,
# where add_bootstrap_weights() stores the weights in the file; read_only = TRUE
# is the get_pumf() default, where they go to a temporary table.
.bsw_open <- function(s, read_only = FALSE, tname = s$tname) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = s$db_path, read_only = read_only)
  t   <- dplyr::tbl(con, tname)
  .pumf_register_con(con, s$series, s$version, s$cache, s$lang)
  t
}
.bsw_append <- function(s, df) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = s$db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, s$tname, df, append = TRUE)
}
# The stored weights, read through a connection of its own (every tbl on the
# file must be closed first).
.bsw_read <- function(s, table = "pumf_bsw_wt", order = '"ID"') {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = s$db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbGetQuery(con, sprintf('SELECT * FROM "%s" ORDER BY %s', table, order))
}
.bsw_stored_tables <- function(db_path) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbListTables(con)
}
.bsw_reps <- function(x, prefix = "CPBSW")
  grep(paste0("^", prefix, "[0-9]+$"), colnames(x), value = TRUE)


# ============================================================
# add_bootstrap_weights() — in-memory path
# ============================================================

test_that("add_bootstrap_weights (in-memory): appends BSW columns", {
  df <- tibble::tibble(ID = 1:10, WEIGHT = rep(100, 10), X = letters[1:10])
  result <- add_bootstrap_weights(df, weight_col = "WEIGHT",
                                  n_replicates = 8L, seed = 42L)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 10L)
  bsw_cols <- grep("^CPBSW",names(result), value = TRUE)
  expect_length(bsw_cols, 8L)
  expect_true(all(sapply(result[bsw_cols], is.numeric)))
})

test_that("add_bootstrap_weights (in-memory): custom prefix", {
  df     <- tibble::tibble(W = c(1, 2, 3))
  result <- add_bootstrap_weights(df, weight_col = "W",
                                  n_replicates = 4L, prefix = "REP", seed = 1L)
  expect_true(all(paste0("REP", 1:4) %in% names(result)))
})

test_that("add_bootstrap_weights (in-memory): NA weights replaced with 0", {
  df <- tibble::tibble(W = c(1, NA, 3))
  expect_warning(
    result <- add_bootstrap_weights(df, weight_col = "W",
                                    n_replicates = 4L, seed = 1L),
    regexp = "NA weight"
  )
  bsw_cols <- grep("^CPBSW",names(result), value = TRUE)
  expect_true(all(!is.na(result[bsw_cols])))
})

test_that("add_bootstrap_weights (in-memory): seed gives reproducible results", {
  df <- tibble::tibble(W = 1:5)
  r1 <- add_bootstrap_weights(df, "W", n_replicates = 4L, seed = 99L)
  r2 <- add_bootstrap_weights(df, "W", n_replicates = 4L, seed = 99L)
  expect_equal(r1, r2)
})

test_that("add_bootstrap_weights (in-memory): re-run extends without duplicating columns", {
  df <- tibble::tibble(ID = 1:10, WEIGHT = rep(100, 10))
  r1 <- add_bootstrap_weights(df, "WEIGHT", n_replicates = 4L, seed = 1L)
  expect_length(grep("^CPBSW", names(r1), value = TRUE), 4L)

  # Requesting more replicates must extend the existing set (CPBSW5..CPBSW8),
  # not regenerate CPBSW1..CPBSW8 and duplicate the column names.
  r2 <- suppressMessages(
    add_bootstrap_weights(r1, "WEIGHT", n_replicates = 8L, seed = 1L))
  bsw_cols <- grep("^CPBSW", names(r2), value = TRUE)
  expect_length(bsw_cols, 8L)
  expect_false(anyDuplicated(names(r2)) > 0L)
  expect_setequal(bsw_cols, paste0("CPBSW", 1:8))
  # The original replicate columns are preserved unchanged.
  expect_equal(r2[paste0("CPBSW", 1:4)], r1[paste0("CPBSW", 1:4)])
})

test_that("add_bootstrap_weights (in-memory): re-run reuses when enough replicates exist", {
  df <- tibble::tibble(ID = 1:10, WEIGHT = rep(100, 10))
  r1 <- add_bootstrap_weights(df, "WEIGHT", n_replicates = 8L, seed = 1L)
  r2 <- suppressMessages(
    add_bootstrap_weights(r1, "WEIGHT", n_replicates = 5L, seed = 1L))
  # No regeneration, no new columns, no duplicates.
  expect_identical(names(r2), names(r1))
  expect_equal(r2, r1)
})

test_that("add_bootstrap_weights (in-memory): stratified resamples within strata", {
  # Constant weights => within-stratum total of each replicate column equals
  # (weight * stratum size) iff resampling stayed inside the stratum.
  df <- tibble::tibble(ID = 1:10, WT = rep(100, 10), G = rep(c("A", "B"), each = 5))
  out <- as.data.frame(
    add_bootstrap_weights(df, "WT", strata_cols = "G", n_replicates = 4L, seed = 1L))
  repcols <- paste0("CPBSW", 1:4)
  sumA <- vapply(repcols, function(c) sum(out[out$ID <= 5L, c]), numeric(1))
  sumB <- vapply(repcols, function(c) sum(out[out$ID >= 6L, c]), numeric(1))
  expect_true(all(sumA == 500))   # 100 * 5
  expect_true(all(sumB == 500))
})


# ============================================================
# add_bootstrap_weights() + remove_bootstrap_weights() + bsw_info()
# — DuckDB path (uses the minimal fixture)
# ============================================================

test_that("add_bootstrap_weights (DuckDB): a read-only connection gets temporary weights", {
  tmp <- withr::local_tempdir()
  .make_bsw_dir(tmp)
  db  <- .pumf_db_path("FAKE", "2099", tmp)
  tbl <- suppressMessages(get_pumf("FAKE", "2099", cache_path = tmp))
  md5 <- tools::md5sum(db)

  expect_message(
    result <- add_bootstrap_weights(tbl, weight_col = "WEIGHT",
                                    n_replicates = 16L, seed = 7L),
    regexp = "temporary table")

  # Returns the input tbl joined with the replicate columns, on its connection.
  expect_length(.bsw_reps(result), 16L)
  expect_true(all(c("pumf_row_id", "ID", "WEIGHT") %in% colnames(result)))
  expect_equal(nrow(dplyr::collect(result)), 20L)
  expect_identical(result$src$con, tbl$src$con)
  # The input tbl was not closed.
  expect_equal(nrow(dplyr::collect(tbl)), 20L)

  info <- bsw_info(tbl)
  expect_equal(info$bsw_table, "tmp_pumf_bsw_weight")
  expect_true(info$temporary)

  # A second call on the same connection reuses the temporary weights.
  msgs <- testthat::capture_messages(
    again <- add_bootstrap_weights(tbl, weight_col = "WEIGHT",
                                   n_replicates = 16L, seed = 99L))
  expect_length(msgs, 0L)
  expect_equal(dplyr::collect(dplyr::arrange(again, ID)),
               dplyr::collect(dplyr::arrange(result, ID)))

  # Nothing reached the file.
  close_pumf(tbl)
  expect_identical(tools::md5sum(db), md5)
  expect_false(any(grepl("bsw", .bsw_stored_tables(db))))
})

test_that("add_bootstrap_weights (DuckDB): a write connection stores the weights", {
  tmp <- withr::local_tempdir()
  .make_bsw_dir(tmp)
  db <- .pumf_db_path("FAKE", "2099", tmp)
  rw <- suppressMessages(get_pumf("FAKE", "2099", cache_path = tmp,
                                  read_only = FALSE))
  msgs <- testthat::capture_messages(
    r1 <- add_bootstrap_weights(rw, weight_col = "WEIGHT",
                                n_replicates = 4L, seed = 1L))
  expect_false(any(grepl("temporary", msgs)))
  expect_false(bsw_info(rw)$temporary)
  stored <- dplyr::collect(dplyr::arrange(r1, ID))
  # The input tbl stays valid on a write connection too.
  expect_equal(nrow(dplyr::collect(rw)), 20L)
  close_pumf(rw)
  expect_true("pumf_bsw_weight" %in% .bsw_stored_tables(db))

  # A later read-only session reuses the stored weights: nothing is generated
  # (the seed differs) and nothing is temporary.
  ro <- suppressMessages(get_pumf("FAKE", "2099", cache_path = tmp))
  on.exit(try(close_pumf(ro), silent = TRUE))
  msgs <- testthat::capture_messages(
    r2 <- add_bootstrap_weights(ro, weight_col = "WEIGHT",
                                n_replicates = 4L, seed = 999L))
  expect_length(msgs, 0L)
  expect_equal(dplyr::collect(dplyr::arrange(r2, ID)), stored)
  expect_false(bsw_info(ro)$temporary)

  # Fewer replicates than stored: a subset of the stored columns.
  r3 <- add_bootstrap_weights(ro, weight_col = "WEIGHT", n_replicates = 2L)
  expect_identical(.bsw_reps(r3), c("CPBSW1", "CPBSW2"))
})

test_that("add_bootstrap_weights (DuckDB): a read-only connection extends stored weights in a temporary table", {
  tmp <- withr::local_tempdir()
  .make_bsw_dir(tmp)
  db <- .pumf_db_path("FAKE", "2099", tmp)
  rw <- suppressMessages(get_pumf("FAKE", "2099", cache_path = tmp,
                                  read_only = FALSE))
  stored <- suppressMessages(add_bootstrap_weights(
    rw, weight_col = "WEIGHT", n_replicates = 3L, seed = 1L)) |>
    dplyr::arrange(ID) |> dplyr::collect()
  close_pumf(rw)

  ro <- suppressMessages(get_pumf("FAKE", "2099", cache_path = tmp))
  expect_message(
    more <- add_bootstrap_weights(ro, weight_col = "WEIGHT",
                                  n_replicates = 6L, seed = 2L),
    regexp = "temporary table")
  more_df <- dplyr::collect(dplyr::arrange(more, ID))
  expect_identical(.bsw_reps(more), paste0("CPBSW", 1:6))
  expect_false(anyNA(more_df))
  # The stored replicates keep their values; the stored table is not touched.
  expect_equal(more_df[paste0("CPBSW", 1:3)], stored[paste0("CPBSW", 1:3)])
  info <- bsw_info(ro)
  expect_equal(info$n_replicates[!info$temporary], 3L)
  expect_equal(info$n_replicates[info$temporary], 6L)

  # A request the stored weights cover again reads the temporary table, which
  # holds the same values.
  few <- add_bootstrap_weights(ro, weight_col = "WEIGHT", n_replicates = 3L)
  expect_equal(dplyr::collect(dplyr::arrange(few, ID)), stored)

  # overwrite = TRUE regenerates the temporary weights and leaves the stored.
  over <- suppressMessages(add_bootstrap_weights(
    ro, weight_col = "WEIGHT", n_replicates = 3L, seed = 5L, overwrite = TRUE))
  over_df <- dplyr::collect(dplyr::arrange(over, ID))
  expect_false(isTRUE(all.equal(over_df[paste0("CPBSW", 1:3)],
                                stored[paste0("CPBSW", 1:3)])))
  close_pumf(ro)
  expect_equal(ncol(.bsw_read(list(db_path = db), "pumf_bsw_weight", "1")), 4L)
})

test_that("add_bootstrap_weights (DuckDB): re-run with custom prefix replaces its replicate columns", {
  tmp <- withr::local_tempdir()
  .make_bsw_dir(tmp)
  tbl <- suppressMessages(get_pumf("FAKE", "2099", cache_path = tmp))
  on.exit(try(close_pumf(tbl), silent = TRUE))

  # First pass: coded input → coded output plus REP1..REP4 replicate columns.
  t1 <- suppressMessages(add_bootstrap_weights(
    tbl, weight_col = "WEIGHT", n_replicates = 4L, prefix = "REP", seed = 1L))

  # Re-running on the augmented tbl replaces the replicate columns of that
  # prefix instead of joining them a second time.
  t2 <- suppressMessages(add_bootstrap_weights(
    t1, weight_col = "WEIGHT", n_replicates = 3L, prefix = "REP", seed = 1L))

  cn <- colnames(t2)
  expect_true(all(c("ID", "WEIGHT") %in% cn))   # coded names preserved
  expect_identical(.bsw_reps(t2, "REP"), paste0("REP", 1:3))
  expect_false(anyDuplicated(cn) > 0L)
  expect_equal(nrow(dplyr::collect(t2)), 20L)

  # Another prefix in a table of its own is added next to the first.
  t3 <- suppressMessages(add_bootstrap_weights(
    t2, weight_col = "WEIGHT", n_replicates = 2L, prefix = "ALT",
    bsw_table = "pumf_bsw_alt", seed = 1L))
  expect_identical(.bsw_reps(t3, "REP"), paste0("REP", 1:3))
  expect_identical(.bsw_reps(t3, "ALT"), paste0("ALT", 1:2))
})

test_that("add_bootstrap_weights (DuckDB): labelled and filtered input keeps filter and labels", {
  tmp <- withr::local_tempdir()
  .make_bsw_dir(tmp)
  input <- suppressMessages(get_pumf("FAKE", "2099", cache_path = tmp)) |>
    label_pumf_columns() |>
    dplyr::filter(`Record ID` > 10)
  on.exit(try(close_pumf(input), silent = TRUE))
  expected <- c("pumf_row_id", "Record ID", "Survey weight", paste0("CPBSW", 1:4))

  # The weight column may be named by its label.
  expect_no_warning(t1 <- suppressMessages(add_bootstrap_weights(
    input, weight_col = "Survey weight", n_replicates = 4L, seed = 1L)))
  expect_identical(colnames(t1), expected)
  expect_equal(nrow(dplyr::collect(t1)), 10L)
  expect_equal(nrow(dplyr::collect(input)), 10L)

  # The result is an ordinary lazy tbl: further verbs apply to it.
  out <- t1 |>
    dplyr::filter(`Record ID` > 15) |>
    dplyr::summarise(dplyr::across(dplyr::matches("^CPBSW"), sum)) |>
    dplyr::collect()
  expect_equal(ncol(out), 4L)

  # The weights cover the whole table, not the filtered rows.
  expect_equal(DBI::dbGetQuery(input$src$con,
    'SELECT COUNT(*) AS n FROM "tmp_pumf_bsw_weight"')$n, 20L)
})

test_that("add_bootstrap_weights (DuckDB): select() must keep the row key", {
  tmp <- withr::local_tempdir()
  .make_bsw_dir(tmp)
  tbl <- suppressMessages(get_pumf("FAKE", "2099", cache_path = tmp)) |>
    label_pumf_columns()
  on.exit(try(close_pumf(tbl), silent = TRUE))

  kept <- suppressMessages(add_bootstrap_weights(
    dplyr::select(tbl, pumf_row_id, `Survey weight`),
    weight_col = "Survey weight", n_replicates = 4L, seed = 1L))
  expect_identical(colnames(kept),
                   c("pumf_row_id", "Survey weight", paste0("CPBSW", 1:4)))

  # The weight column itself is read from the survey table, so it may be gone.
  expect_identical(
    colnames(add_bootstrap_weights(dplyr::select(tbl, pumf_row_id),
                                   weight_col = "Survey weight",
                                   n_replicates = 2L)),
    c("pumf_row_id", "CPBSW1", "CPBSW2"))

  expect_error(
    add_bootstrap_weights(dplyr::select(tbl, `Survey weight`),
                          weight_col = "Survey weight", n_replicates = 4L),
    regexp = "no longer has the column pumf_row_id")
})

test_that("add_bootstrap_weights (DuckDB): the key column may carry its variable label", {
  tmp <- withr::local_tempdir()
  .make_bsw_dir(tmp)
  tbl <- suppressMessages(get_pumf("FAKE", "2099", cache_path = tmp)) |>
    label_pumf_columns()
  on.exit(try(close_pumf(tbl), silent = TRUE))

  # id_col given as a label resolves to the coded ID, which the labelled tbl
  # carries as "Record ID".
  out <- suppressMessages(add_bootstrap_weights(
    dplyr::select(tbl, `Record ID`, `Survey weight`),
    weight_col = "Survey weight", id_col = "Record ID",
    n_replicates = 4L, seed = 1L))
  expect_identical(colnames(out),
                   c("Record ID", "Survey weight", paste0("CPBSW", 1:4)))
  expect_equal(nrow(dplyr::collect(out)), 20L)
  expect_identical(
    DBI::dbListFields(tbl$src$con, "tmp_pumf_bsw_weight")[1L], "ID")
  expect_error(add_bootstrap_weights(tbl, "Survey weight", id_col = "NOPE"),
               regexp = "neither a column")
})

test_that("add_bootstrap_weights (DuckDB): a label containing 'where' is not read as a WHERE clause", {
  tmp  <- withr::local_tempdir()
  vdir <- .make_bsw_dir(tmp)
  vars_path <- file.path(vdir, "metadata", "variables.csv")
  vars <- readr::read_csv(vars_path, show_col_types = FALSE)
  vars$label_en[vars$name == "ID"] <- "Place where the record is kept"
  readr::write_csv(vars, vars_path)

  tbl <- suppressMessages(get_pumf("FAKE", "2099", cache_path = tmp)) |>
    label_pumf_columns()
  on.exit(try(close_pumf(tbl), silent = TRUE))
  expect_no_warning(out <- suppressMessages(add_bootstrap_weights(
    tbl, weight_col = "Survey weight", n_replicates = 4L, seed = 1L)))
  expect_true("Place where the record is kept" %in% colnames(out))
  expect_equal(nrow(dplyr::collect(out)), 20L)
})

test_that("bootstrap-weight functions: a closed tbl asks for get_pumf() again", {
  tmp <- withr::local_tempdir()
  .make_bsw_dir(tmp)
  tbl <- suppressMessages(get_pumf("FAKE", "2099", cache_path = tmp))
  close_pumf(tbl)

  expect_error(add_bootstrap_weights(tbl, weight_col = "WEIGHT"),
               regexp = "Call get_pumf\\(\\) again")
  expect_error(bsw_info(tbl), regexp = "Call get_pumf\\(\\) again")
  expect_error(remove_bootstrap_weights(tbl), regexp = "Call get_pumf\\(\\) again")
})

test_that("add_bootstrap_weights (DuckDB): added rows regenerate all weights (unstratified)", {
  set.seed(0)
  s  <- .bsw_db(data.frame(ID = 1:8, WT = runif(8, 100, 200)))
  r1 <- suppressMessages(add_bootstrap_weights(
    .bsw_open(s), "WT", id_col = "ID", n_replicates = 3L, seed = 1L))
  close_pumf(r1)
  before <- .bsw_read(s)
  expect_equal(nrow(before), 8L)

  # Append 4 rows, then re-add: every replicate weight must be regenerated
  # because the whole resampling population changed.
  .bsw_append(s, data.frame(ID = 9:12, WT = runif(4, 100, 200)))
  r2 <- suppressMessages(add_bootstrap_weights(
    .bsw_open(s), "WT", id_col = "ID", n_replicates = 3L, seed = 1L))
  expect_equal(nrow(dplyr::collect(r2)), 12L)
  close_pumf(r2)
  after <- .bsw_read(s)

  expect_equal(nrow(after), 12L)
  expect_setequal(after$ID, 1:12)
  expect_false("CPBSW4" %in% names(after))      # still 3 replicates, no new cols
  # Pre-existing rows were regenerated, not carried over unchanged.
  expect_false(isTRUE(all.equal(before[before$ID <= 8L, -1L],
                                after[after$ID <= 8L, -1L])))
})

test_that("add_bootstrap_weights (DuckDB): added rows regenerate only affected strata", {
  set.seed(0)
  s  <- .bsw_db(data.frame(ID = 1:10, WT = runif(10, 100, 200),
                           G = rep(c("A", "B"), each = 5)))
  r1 <- suppressMessages(add_bootstrap_weights(
    .bsw_open(s), "WT", id_col = "ID", strata_cols = "G",
    n_replicates = 3L, seed = 1L))
  close_pumf(r1)
  before <- .bsw_read(s)

  # New rows only in stratum B: stratum A must be left untouched.
  .bsw_append(s, data.frame(ID = 11:12, WT = runif(2, 100, 200), G = c("B", "B")))
  r2 <- suppressMessages(add_bootstrap_weights(
    .bsw_open(s), "WT", id_col = "ID", strata_cols = "G",
    n_replicates = 3L, seed = 1L))
  close_pumf(r2)
  after <- .bsw_read(s)

  expect_equal(nrow(after), 12L)
  expect_true(all(11:12 %in% after$ID))
  # Stratum A (ID 1-5): weights preserved bit-for-bit.
  expect_equal(after[after$ID <= 5L, ], before[before$ID <= 5L, ],
               ignore_attr = TRUE)
  # Stratum B (ID 6-10): pre-existing rows regenerated.
  expect_false(isTRUE(all.equal(before[before$ID %in% 6:10, -1L],
                                after[after$ID %in% 6:10, -1L])))
})

test_that("add_bootstrap_weights (DuckDB): added rows on a read-only connection regenerate into the temporary table", {
  set.seed(0)
  s  <- .bsw_db(data.frame(ID = 1:10, WT = runif(10, 100, 200),
                           G = rep(c("A", "B"), each = 5)))
  r1 <- suppressMessages(add_bootstrap_weights(
    .bsw_open(s), "WT", id_col = "ID", strata_cols = "G",
    n_replicates = 3L, seed = 1L))
  close_pumf(r1)
  before <- .bsw_read(s)
  .bsw_append(s, data.frame(ID = 11:12, WT = runif(2, 100, 200), G = c("B", "B")))

  # The stored weights lack the two new rows, so they do not cover the table.
  ro <- .bsw_open(s, read_only = TRUE)
  expect_message(
    r2 <- add_bootstrap_weights(ro, "WT", id_col = "ID", strata_cols = "G",
                                n_replicates = 3L, seed = 1L),
    regexp = "temporary table")
  after <- as.data.frame(dplyr::collect(dplyr::arrange(r2, ID)))
  expect_equal(nrow(after), 12L)
  expect_false(anyNA(after))
  # Stratum A is copied from the stored weights, stratum B regenerated.
  reps <- paste0("CPBSW", 1:3)
  expect_equal(after[after$ID <= 5L, reps], before[before$ID <= 5L, reps],
               ignore_attr = TRUE)
  close_pumf(ro)
  expect_equal(.bsw_read(s), before)             # stored weights untouched
})

test_that("add_bootstrap_weights (DuckDB): reuses stored weights when nothing changed", {
  set.seed(0)
  s  <- .bsw_db(data.frame(ID = 1:8, WT = runif(8, 100, 200)))
  r1 <- suppressMessages(add_bootstrap_weights(
    .bsw_open(s), "WT", id_col = "ID", n_replicates = 4L, seed = 1L))
  close_pumf(r1)
  before <- .bsw_read(s)

  # Same request, deliberately different seed: if anything were recomputed the
  # stored values would change.  The weights are reused, so they must be
  # identical.
  r2 <- suppressMessages(add_bootstrap_weights(
    .bsw_open(s), "WT", id_col = "ID", n_replicates = 4L, seed = 999L))
  expect_length(.bsw_reps(r2), 4L)
  close_pumf(r2)
  expect_equal(.bsw_read(s), before)

  # Requesting fewer replicates returns a subset but does not shrink the
  # stored table.
  r3 <- suppressMessages(add_bootstrap_weights(
    .bsw_open(s), "WT", id_col = "ID", n_replicates = 2L, seed = 1L))
  expect_length(.bsw_reps(r3), 2L)
  close_pumf(r3)
  expect_equal(ncol(.bsw_read(s)), 5L)   # ID + CPBSW1..4 still stored
})

test_that("add_bootstrap_weights (DuckDB): adding replicates keeps existing columns unchanged", {
  set.seed(0)
  s  <- .bsw_db(data.frame(ID = 1:8, WT = runif(8, 100, 200)))
  r1 <- suppressMessages(add_bootstrap_weights(
    .bsw_open(s), "WT", id_col = "ID", n_replicates = 3L, seed = 1L))
  close_pumf(r1)
  before <- .bsw_read(s)

  r2 <- suppressMessages(add_bootstrap_weights(
    .bsw_open(s), "WT", id_col = "ID", n_replicates = 6L, seed = 1L))
  close_pumf(r2)
  after <- .bsw_read(s)

  expect_equal(nrow(after), 8L)
  expect_true(all(paste0("CPBSW", 1:6) %in% names(after)))
  expect_false("CPBSW7" %in% names(after))
  # The original three replicate columns are preserved bit-for-bit.
  expect_equal(after[, paste0("CPBSW", 1:3)], before[, paste0("CPBSW", 1:3)],
               ignore_attr = TRUE)
})

test_that("add_bootstrap_weights (DuckDB): added replicate columns resample within strata", {
  # Constant weights: each replicate column's within-stratum total equals
  # (weight * stratum size) only if resampling stayed inside the stratum.  This
  # guards the regression where adding columns resampled the whole population.
  s  <- .bsw_db(data.frame(ID = 1:10, WT = rep(100, 10), G = rep(c("A", "B"), each = 5)))
  r1 <- suppressMessages(add_bootstrap_weights(
    .bsw_open(s), "WT", id_col = "ID", strata_cols = "G", n_replicates = 2L, seed = 1L))
  close_pumf(r1)
  r2 <- suppressMessages(add_bootstrap_weights(
    .bsw_open(s), "WT", id_col = "ID", strata_cols = "G", n_replicates = 5L, seed = 1L))
  close_pumf(r2)
  after <- .bsw_read(s)

  repcols <- paste0("CPBSW", 1:5)
  sumA <- vapply(repcols, function(c) sum(after[after$ID <= 5L, c]), numeric(1))
  sumB <- vapply(repcols, function(c) sum(after[after$ID >= 6L, c]), numeric(1))
  expect_true(all(sumA == 500))   # 100 * 5, every column including the added 3-5
  expect_true(all(sumB == 500))
})

test_that("add_bootstrap_weights (DuckDB): added rows + more columns, stratified", {
  set.seed(0)
  s  <- .bsw_db(data.frame(ID = 1:10, WT = runif(10, 100, 200),
                           G = rep(c("A", "B"), each = 5)))
  r1 <- suppressMessages(add_bootstrap_weights(
    .bsw_open(s), "WT", id_col = "ID", strata_cols = "G", n_replicates = 3L, seed = 1L))
  close_pumf(r1)
  before <- .bsw_read(s)

  .bsw_append(s, data.frame(ID = 11:12, WT = runif(2, 100, 200), G = c("B", "B")))
  r2 <- suppressMessages(add_bootstrap_weights(
    .bsw_open(s), "WT", id_col = "ID", strata_cols = "G", n_replicates = 5L, seed = 1L))
  close_pumf(r2)
  after <- .bsw_read(s)

  expect_equal(nrow(after), 12L)
  expect_true(all(paste0("CPBSW", 1:5) %in% names(after)))
  expect_false(anyNA(after[, -1L]))
  # Unaffected stratum A keeps its first three replicates; columns 4-5 added.
  expect_equal(after[after$ID <= 5L, paste0("CPBSW", 1:3)],
               before[before$ID <= 5L, paste0("CPBSW", 1:3)], ignore_attr = TRUE)
})

test_that("add_bootstrap_weights (DuckDB): overwrite=TRUE regenerates from scratch", {
  set.seed(0)
  s  <- .bsw_db(data.frame(ID = 1:8, WT = runif(8, 100, 200)))
  r1 <- suppressMessages(add_bootstrap_weights(
    .bsw_open(s), "WT", id_col = "ID", n_replicates = 4L, seed = 1L))
  close_pumf(r1)
  before <- .bsw_read(s)

  # Same seed + same data => identical regeneration.
  r2 <- suppressMessages(add_bootstrap_weights(
    .bsw_open(s), "WT", id_col = "ID", n_replicates = 4L, seed = 1L, overwrite = TRUE))
  close_pumf(r2)
  expect_equal(.bsw_read(s), before)

  # Different seed => different weights (proves it recomputed, not reused).
  r3 <- suppressMessages(add_bootstrap_weights(
    .bsw_open(s), "WT", id_col = "ID", n_replicates = 4L, seed = 2L, overwrite = TRUE))
  close_pumf(r3)
  expect_false(isTRUE(all.equal(.bsw_read(s)[, -1L], before[, -1L])))
})

test_that("add_bootstrap_weights (DuckDB): pumf_row_id is the default key", {
  df <- data.frame(pumf_row_id = 1:6, ID = 101:106, wt = c(1, 2, 3, 4, 5, 6))
  s  <- .bsw_db(df)
  t  <- .bsw_open(s)
  out <- suppressMessages(
    add_bootstrap_weights(t, weight_col = "wt", n_replicates = 3L, seed = 1L))
  expect_length(.bsw_reps(out), 3L)
  close_pumf(out)
  bsw <- .bsw_read(s, order = "1")
  # keyed by the table's own column, and the main table is left as written
  expect_equal(names(bsw)[1L], "pumf_row_id")
  expect_equal(bsw$pumf_row_id, 1:6)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = s$db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  expect_identical(DBI::dbListFields(con, s$tname), names(df))
})

test_that("add_bootstrap_weights (DuckDB): a table without a row key needs id_col", {
  s <- .bsw_db(data.frame(ID = 101:106, wt = c(1, 2, 3, 4, 5, 6)))  # pre-0.6.1
  t <- .bsw_open(s, read_only = TRUE)
  on.exit(try(close_pumf(t), silent = TRUE))
  expect_error(add_bootstrap_weights(t, weight_col = "wt", n_replicates = 3L),
               regexp = "no column that identifies its rows")
  # The survey table is not altered to make one.
  expect_identical(colnames(dplyr::tbl(t$src$con, s$tname)), c("ID", "wt"))

  out <- suppressMessages(add_bootstrap_weights(
    t, weight_col = "wt", id_col = "ID", n_replicates = 3L, seed = 1L))
  expect_identical(colnames(out), c("ID", "wt", paste0("CPBSW", 1:3)))

  # Without id_col, the existing weights supply the key.
  again <- add_bootstrap_weights(t, weight_col = "wt", n_replicates = 3L)
  expect_equal(dplyr::collect(dplyr::arrange(again, ID)),
               dplyr::collect(dplyr::arrange(out, ID)))
})

test_that("add_bootstrap_weights (DuckDB): id_col must identify the rows", {
  s <- .bsw_db(data.frame(ID = c(1, 1, 2, NA), K = 1:4, wt = c(1, 2, 3, 4)))
  t <- .bsw_open(s, read_only = TRUE)
  on.exit(try(close_pumf(t), silent = TRUE))
  expect_error(
    suppressMessages(add_bootstrap_weights(t, "wt", id_col = "ID", n_replicates = 2L)),
    regexp = "does not identify the rows")

  # Existing weights keyed differently are not silently re-keyed.
  ok <- suppressMessages(add_bootstrap_weights(t, "wt", id_col = "K",
                                               n_replicates = 2L, seed = 1L))
  expect_error(add_bootstrap_weights(t, "wt", id_col = "ID", n_replicates = 2L),
               regexp = "keyed by K")
})

test_that("add_bootstrap_weights (DuckDB): several columns can form the key", {
  set.seed(0)
  df <- data.frame(Y = rep(2001:2002, each = 3), R = rep(1:3, 2),
                   wt = runif(6, 100, 200))
  s  <- .bsw_db(df)
  key <- c("Y", "R")
  r1 <- suppressMessages(add_bootstrap_weights(
    .bsw_open(s), "wt", id_col = key, n_replicates = 3L, seed = 1L))
  expect_identical(colnames(r1), c("Y", "R", "wt", paste0("CPBSW", 1:3)))
  expect_equal(nrow(dplyr::collect(r1)), 6L)
  close_pumf(r1)
  before <- .bsw_read(s, order = '"Y", "R"')
  expect_identical(names(before), c(key, paste0("CPBSW", 1:3)))

  # More replicates: the stored ones are kept, matched on both key columns.
  r2 <- suppressMessages(add_bootstrap_weights(
    .bsw_open(s), "wt", id_col = key, n_replicates = 5L, seed = 1L))
  close_pumf(r2)
  wider <- .bsw_read(s, order = '"Y", "R"')
  expect_equal(wider[names(before)], before)
  expect_false(anyNA(wider))

  # New rows, stratified by year: only the year that gained rows is redone.
  .bsw_append(s, data.frame(Y = 2002L, R = 4:5, wt = c(150, 160)))
  r3 <- suppressMessages(add_bootstrap_weights(
    .bsw_open(s), "wt", id_col = key, strata_cols = "Y",
    n_replicates = 5L, seed = 1L))
  expect_equal(nrow(dplyr::collect(r3)), 8L)
  close_pumf(r3)
  after <- .bsw_read(s, order = '"Y", "R"')
  expect_equal(nrow(after), 8L)
  expect_equal(after[after$Y == 2001L, ], wider[wider$Y == 2001L, ],
               ignore_attr = TRUE)
})

test_that("add_bootstrap_weights (DuckDB): longitudinal series key on month and record number", {
  df <- data.frame(SURVYEAR = rep(2020L, 8), SURVMNTH = rep(1:2, each = 4),
                   REC_NUM = rep(1:4, 2), FINALWT = rep(100, 8))
  s <- .bsw_db(df, series = "LFS", version = "2020")
  t <- .bsw_open(s, read_only = TRUE)
  on.exit(try(close_pumf(t), silent = TRUE))

  # One month of the shared table, as get_pumf("LFS", "2020-02") returns it.
  feb <- dplyr::filter(t, SURVMNTH == 2L)
  out <- suppressMessages(add_bootstrap_weights(feb, "FINALWT",
                                                n_replicates = 4L, seed = 1L))
  expect_equal(nrow(dplyr::collect(out)), 4L)
  expect_identical(
    setdiff(DBI::dbListFields(t$src$con, "tmp_pumf_bsw_finalwt"),
            paste0("CPBSW", 1:4)),
    c("SURVYEAR", "SURVMNTH", "REC_NUM"))

  # Resampled within each month by default: with constant weights every
  # replicate sums to the month's total.
  all_months <- add_bootstrap_weights(t, "FINALWT", n_replicates = 4L) |>
    dplyr::summarise(dplyr::across(dplyr::matches("^CPBSW"), sum),
                     .by = "SURVMNTH") |>
    dplyr::collect()
  expect_true(all(as.matrix(all_months[paste0("CPBSW", 1:4)]) == 400))
})

test_that("add_bootstrap_weights (DuckDB): a secondary module gets a weights table of its own", {
  reg  <- pumf_registry_lookup("SGVP", "2010")
  main <- .pumf_table_name("SGVP", "2010", "eng")
  gs   <- .pumf_table_name("SGVP", "2010", "eng", "GS")
  s <- .bsw_db(data.frame(pumf_row_id = 1:4, WT = rep(10, 4)),
               series = "SGVP", version = "2010")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = s$db_path)
  DBI::dbWriteTable(con, gs, data.frame(pumf_row_id = 1:6, WT = rep(5, 6)))
  DBI::dbDisconnect(con, shutdown = TRUE)

  t_main <- .bsw_open(s, read_only = TRUE)
  on.exit(try(close_pumf(t_main), silent = TRUE))
  t_gs <- dplyr::tbl(t_main$src$con, gs)

  b_main <- suppressMessages(add_bootstrap_weights(t_main, "WT", n_replicates = 2L, seed = 1L))
  b_gs   <- suppressMessages(add_bootstrap_weights(t_gs,   "WT", n_replicates = 2L, seed = 1L))
  expect_equal(nrow(dplyr::collect(b_main)), 4L)
  expect_equal(nrow(dplyr::collect(b_gs)), 6L)
  info <- bsw_info(t_main)
  expect_setequal(info$bsw_table, c("tmp_pumf_bsw_wt", "tmp_pumf_bsw_wt_gs"))
  expect_identical(unique(info$weight_col), "WT")

  # Removing by weight column addresses the table of the module passed in.
  remove_bootstrap_weights(t_gs, "WT") |> suppressMessages()
  expect_identical(bsw_info(t_main)$bsw_table, "tmp_pumf_bsw_wt")
})

test_that("bsw_info: reports BSW tables after add_bootstrap_weights", {
  tmp <- withr::local_tempdir()
  .make_bsw_dir(tmp)
  tbl    <- suppressMessages(get_pumf("FAKE", "2099", cache_path = tmp))
  on.exit(try(close_pumf(tbl), silent = TRUE))
  result <- suppressMessages(add_bootstrap_weights(
    tbl, weight_col = "WEIGHT", n_replicates = 8L, seed = 3L))

  info <- bsw_info(result)
  expect_s3_class(info, "tbl_df")
  expect_identical(names(info), c("source", "weight_col", "prefix", "bsw_table",
                                  "temporary", "n_replicates", "size_mb"))
  expect_equal(nrow(info), 1L)
  expect_equal(info$source, "generated")
  expect_equal(info$weight_col, "WEIGHT")
  expect_equal(info$prefix, "CPBSW")
  expect_equal(info$n_replicates, 8L)
  expect_true(info$temporary)
  expect_true(is.numeric(info$size_mb))
})

test_that("bsw_info: returns empty tibble (invisibly) when no BSW present", {
  tmp <- withr::local_tempdir()
  .make_bsw_dir(tmp)
  tbl <- suppressMessages(get_pumf("FAKE", "2099", cache_path = tmp))
  on.exit(close_pumf(tbl))

  expect_message(info <- bsw_info(tbl), regexp = "No bootstrap")
  expect_equal(nrow(info), 0L)
  expect_identical(names(info), c("source", "weight_col", "prefix", "bsw_table",
                                  "temporary", "n_replicates", "size_mb"))
})

test_that("bsw_info: reports the replicate weights that came with the survey", {
  n  <- 5L
  df <- data.frame(
    pumf_row_id = seq_len(n), ID = as.character(seq_len(n)), WEIGHT = 10,
    # from a bootstrap weights file: not in variables.csv
    BSW1 = 1, BSW2 = 1, BSW3 = 1, BSW4 = 1,
    # in the data file, labelled as replicate weights
    WT1 = 1, WT2 = 1, WT3 = 1,
    # layout-only columns: in variables.csv without a label, zero-padded
    WRPG_01 = 1, WRPG_02 = 1,
    # not replicate weights: labelled survey variables, a family with a gap,
    # coded columns, and a single numbered column
    Q1 = 1, Q2 = 1, Q3 = 1,
    GAP1 = 1, GAP3 = 1,
    CH1 = "a", CH2 = "b",
    AGE5 = 1)
  s <- .bsw_db(df)
  meta_dir <- file.path(dirname(s$db_path), "metadata")
  dir.create(meta_dir)
  vars <- tibble::tibble(
    name     = c("ID", "WEIGHT", paste0("WT", 1:3), "WRPG_01", "WRPG_02",
                 paste0("Q", 1:3)),
    label_en = c("Record ID", "Survey weight", rep("Replicate PUMF weight", 3L),
                 NA, NA, paste("Question", 1:3)),
    label_fr = c("Identifiant", "Poids",
                 rep("Copie du facteur de pond\u00e9ration FMGD", 3L),
                 NA, NA, paste("Question", 1:3)),
    type = "numeric", decimals = 0L,
    missing_low = NA_real_, missing_high = NA_real_)
  readr::write_csv(vars, file.path(meta_dir, "variables.csv"), na = "")
  readr::write_csv(
    tibble::tibble(name = character(), val = character(),
                   label_en = character(), label_fr = character()),
    file.path(meta_dir, "codes.csv"))

  t <- .bsw_open(s, read_only = TRUE)
  on.exit(try(close_pumf(t), silent = TRUE))

  info <- expect_silent(bsw_info(t))
  expect_identical(info$source, rep("survey", 3L))
  expect_setequal(info$prefix, c("BSW", "WT", "WRPG_"))
  expect_identical(info$n_replicates[match(c("BSW", "WT", "WRPG_"), info$prefix)],
                   c(4L, 3L, 2L))
  expect_true(all(is.na(info$weight_col)))
  expect_identical(unique(info$bsw_table), s$tname)
  expect_false(any(info$temporary))
  expect_equal(info$size_mb[info$prefix == "BSW"], round(n * 4 * 8 / 1e6, 2))

  # Generated weights are listed after the survey's own, as a type of their own.
  b <- suppressMessages(add_bootstrap_weights(t, "WEIGHT", n_replicates = 6L,
                                              seed = 1L))
  info <- bsw_info(b)
  expect_identical(info$source, c(rep("survey", 3L), "generated"))
  gen <- info[info$source == "generated", ]
  expect_identical(gen$weight_col, "WEIGHT")
  expect_identical(gen$prefix, "CPBSW")
  expect_identical(gen$bsw_table, "tmp_pumf_bsw_weight")
  expect_true(gen$temporary)
  expect_identical(gen$n_replicates, 6L)

  # Removing the generated weights leaves the survey's own.
  cleaned <- suppressMessages(remove_bootstrap_weights(b))
  expect_identical(bsw_info(cleaned)$source, rep("survey", 3L))
})

test_that("bsw_info: no survey replicates without metadata or for a longitudinal series", {
  df <- data.frame(pumf_row_id = 1:3, WEIGHT = 10, BSW1 = 1, BSW2 = 1)
  # No metadata directory: nothing says what BSW1/BSW2 are.
  t <- .bsw_open(.bsw_db(df), read_only = TRUE)
  expect_message(info <- bsw_info(t), regexp = "No bootstrap weights found")
  expect_equal(nrow(info), 0L)
  close_pumf(t)

  loc <- list(prov = list(series = "LFS"), table_name = "lfs_eng")
  expect_equal(nrow(.bsw_survey_families(NULL, loc)), 0L)
})

test_that("bsw_info: errors on data.frame input", {
  expect_error(bsw_info(data.frame(x = 1)), regexp = "DuckDB-backed")
})

test_that("remove_bootstrap_weights: drops temporary weights on the same connection", {
  tmp <- withr::local_tempdir()
  .make_bsw_dir(tmp)
  tbl    <- suppressMessages(get_pumf("FAKE", "2099", cache_path = tmp))
  on.exit(close_pumf(tbl))
  result <- suppressMessages(add_bootstrap_weights(
    tbl, weight_col = "WEIGHT", n_replicates = 8L, seed = 5L))
  expect_message(cleaned <- remove_bootstrap_weights(result),
                 regexp = "Dropping temporary")

  expect_length(.bsw_reps(cleaned), 0L)
  expect_identical(cleaned$src$con, tbl$src$con)
  expect_equal(nrow(dplyr::collect(tbl)), 20L)
  expect_message(bsw_info(cleaned), regexp = "No bootstrap")
  # nothing left to remove: the tbl comes back as it is
  expect_message(remove_bootstrap_weights(cleaned), regexp = "No bootstrap")
  expect_error(remove_bootstrap_weights(cleaned, "WEIGHT"),
               regexp = "No bootstrap weight table found")
})

test_that("remove_bootstrap_weights: stored weights need a write connection", {
  tmp <- withr::local_tempdir()
  .make_bsw_dir(tmp)
  db <- .pumf_db_path("FAKE", "2099", tmp)
  rw <- suppressMessages(get_pumf("FAKE", "2099", cache_path = tmp,
                                  read_only = FALSE))
  suppressMessages(add_bootstrap_weights(rw, "WEIGHT", n_replicates = 3L, seed = 1L))
  close_pumf(rw)

  ro <- suppressMessages(get_pumf("FAKE", "2099", cache_path = tmp))
  expect_error(remove_bootstrap_weights(ro), regexp = "read-only connection")
  # With temporary weights next to the stored ones, the temporary go and the
  # stored are reported as kept.
  suppressMessages(add_bootstrap_weights(ro, "WEIGHT", n_replicates = 5L, seed = 1L))
  expect_message(cleaned <- remove_bootstrap_weights(ro), regexp = "are kept")
  info <- bsw_info(cleaned)
  expect_identical(info$bsw_table, "pumf_bsw_weight")
  expect_false(info$temporary)
  close_pumf(ro)

  # A write connection drops them; the weight column may be given by its label.
  rw <- suppressMessages(get_pumf("FAKE", "2099", cache_path = tmp,
                                  read_only = FALSE))
  cleaned <- suppressMessages(
    remove_bootstrap_weights(label_pumf_columns(rw), "Survey weight"))
  expect_identical(cleaned$src$con, rw$src$con)
  expect_true("pumf_row_id" %in% colnames(cleaned))   # the permanent key stays
  close_pumf(rw)
  expect_false("pumf_bsw_weight" %in% .bsw_stored_tables(db))
})

test_that("remove_bootstrap_weights: drops the view of a 0.6.0 cache, keeps its pumf_row_id", {
  # A table as 0.6.0 left it: unstamped, with the pumf_row_id that
  # add_bootstrap_weights() added then, the weights and the view joining them.
  s <- .bsw_db(data.frame(pumf_row_id = 0:5, ID = 101:106, wt = c(1, 2, 3, 4, 5, 6)))
  t <- .bsw_open(s)
  suppressMessages(add_bootstrap_weights(t, "wt", n_replicates = 3L, seed = 1L))
  DBI::dbExecute(t$src$con, sprintf(paste(
    'CREATE VIEW "%s_bsw_wt" AS SELECT m.*, b.CPBSW1 FROM "%s" m',
    'JOIN pumf_bsw_wt b USING (pumf_row_id)'), s$tname, s$tname))

  # Adding replicates rewrites the weights table: the view must not block it.
  wider <- suppressMessages(add_bootstrap_weights(t, "wt", n_replicates = 4L, seed = 1L))
  expect_length(.bsw_reps(wider), 4L)
  DBI::dbExecute(t$src$con, sprintf(
    'CREATE VIEW "%s_bsw_wt" AS SELECT * FROM "%s"', s$tname, s$tname))

  cleaned <- suppressMessages(remove_bootstrap_weights(t))
  expect_identical(colnames(cleaned), c("pumf_row_id", "ID", "wt"))
  close_pumf(cleaned)
  expect_identical(.bsw_stored_tables(s$db_path), s$tname)
})

test_that("remove_bootstrap_weights: errors on data.frame input", {
  expect_error(remove_bootstrap_weights(data.frame(x = 1)),
               regexp = "DuckDB-backed")
})


# ============================================================
# pumf_var_labels()
# ============================================================

test_that("pumf_var_labels: errors when tbl has no provenance", {
  tmp  <- withr::local_tempdir()
  con  <- DBI::dbConnect(duckdb::duckdb(), dbdir = file.path(tmp, "x.duckdb"))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "t", data.frame(x = 1L))
  tbl <- dplyr::tbl(con, "t")
  expect_error(pumf_var_labels(tbl), regexp = "provenance")
})

test_that("pumf_var_labels: returns tibble with name/label_en/label_fr columns", {
  tmp <- withr::local_tempdir()
  .make_bsw_dir(tmp)
  tbl <- suppressMessages(get_pumf("FAKE", "2099", cache_path = tmp))
  on.exit(close_pumf(tbl))

  vl <- pumf_var_labels(tbl)
  expect_s3_class(vl, "tbl_df")
  expect_true(all(c("name", "label_en", "label_fr") %in% names(vl)))
  expect_true("WEIGHT" %in% vl$name)
  expect_equal(vl$label_en[vl$name == "WEIGHT"], "Survey weight")
})


# ============================================================
# list_canpumf_collection() and list_available_lfs_pumf_versions()
# — require network; skip offline
# ============================================================

test_that("list_canpumf_collection: returns tibble with expected columns", {
  # Works offline via hardcoded fallback (emits a warning when scraping fails)
  result <- suppressWarnings(list_canpumf_collection())
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("Title", "Acronym", "Version") %in% names(result)))
  expect_gt(nrow(result), 0L)
  expect_true("SFS" %in% result$Acronym)
  expect_true("Census" %in% result$Acronym)
})

test_that("list_canpumf_collection: warns and returns fallback when StatCan unreachable", {
  with_mocked_bindings(
    read_html = function(...) stop("simulated network error"),
    .package  = "rvest",
    {
      expect_warning(
        result <- list_canpumf_collection(),
        regexp = "unreachable"
      )
      expect_true("Census" %in% result$Acronym)
      expect_gt(nrow(result), 0L)
    }
  )
})

test_that("list_available_lfs_pumf_versions: returns tibble with date/version/url", {
  skip_if_offline()
  tryCatch({
    result <- list_available_lfs_pumf_versions()
    expect_s3_class(result, "tbl_df")
    expect_true(all(c("Date", "version", "url") %in% names(result)))
    expect_gt(nrow(result), 0L)
    expect_true(any(grepl("^\\d{4}$", result$version)))
  }, error = function(e) skip(paste("StatCan unreachable:", conditionMessage(e))))
})
