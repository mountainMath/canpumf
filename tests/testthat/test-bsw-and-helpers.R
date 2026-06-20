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


# ============================================================
# add_bootstrap_weights() + remove_bootstrap_weights() + bsw_info()
# — DuckDB path (uses the minimal fixture)
# ============================================================

test_that("add_bootstrap_weights (DuckDB): creates BSW table and view", {
  tmp <- withr::local_tempdir()
  .make_bsw_dir(tmp)
  tbl <- suppressMessages(get_pumf("FAKE", "2099", cache_path = tmp))
  on.exit(try(close_pumf(tbl), silent = TRUE))

  result <- add_bootstrap_weights(tbl, weight_col = "WEIGHT",
                                  n_replicates = 16L, seed = 7L)
  on.exit(try(close_pumf(result), silent = TRUE), add = TRUE)

  # Returns a lazy tbl with BSW columns
  cn <- colnames(result)
  bsw_cols <- grep("^CPBSW",cn, value = TRUE)
  expect_length(bsw_cols, 16L)
})

test_that("add_bootstrap_weights (DuckDB): re-run with custom prefix keeps coded columns", {
  tmp <- withr::local_tempdir()
  .make_bsw_dir(tmp)
  tbl <- suppressMessages(get_pumf("FAKE", "2099", cache_path = tmp))

  # First pass: coded input → coded output plus REP1..REP4 replicate columns.
  t1 <- suppressMessages(add_bootstrap_weights(
    tbl, weight_col = "WEIGHT", n_replicates = 4L, prefix = "REP", seed = 1L))

  # Re-running on the augmented (still coded) tbl must not mistake the
  # custom-prefix replicate columns for label aliases and relabel the output.
  t2 <- suppressMessages(add_bootstrap_weights(
    t1, weight_col = "WEIGHT", n_replicates = 4L, prefix = "REP", seed = 1L))
  on.exit(try(close_pumf(t2), silent = TRUE))

  cn <- colnames(t2)
  expect_true(all(c("ID", "WEIGHT") %in% cn))   # coded names preserved
  expect_false("Survey weight" %in% cn)          # not spuriously relabeled
  expect_false("Record ID" %in% cn)
})

test_that("bsw_info: reports BSW tables after add_bootstrap_weights", {
  tmp <- withr::local_tempdir()
  .make_bsw_dir(tmp)
  tbl    <- suppressMessages(get_pumf("FAKE", "2099", cache_path = tmp))
  result <- add_bootstrap_weights(tbl, weight_col = "WEIGHT",
                                  n_replicates = 8L, seed = 3L)
  on.exit(try(close_pumf(result), silent = TRUE))

  info <- bsw_info(result)
  expect_s3_class(info, "tbl_df")
  expect_equal(nrow(info), 1L)
  expect_equal(info$n_replicates, 8L)
  expect_true(info$view_exists)
})

test_that("bsw_info: returns empty tibble (invisibly) when no BSW present", {
  tmp <- withr::local_tempdir()
  .make_bsw_dir(tmp)
  tbl <- suppressMessages(get_pumf("FAKE", "2099", cache_path = tmp))
  on.exit(close_pumf(tbl))

  expect_message(info <- bsw_info(tbl), regexp = "No bootstrap")
  expect_equal(nrow(info), 0L)
})

test_that("bsw_info: errors on data.frame input", {
  expect_error(bsw_info(data.frame(x = 1)), regexp = "DuckDB-backed")
})

test_that("remove_bootstrap_weights: removes BSW and returns clean tbl", {
  tmp <- withr::local_tempdir()
  .make_bsw_dir(tmp)
  tbl    <- suppressMessages(get_pumf("FAKE", "2099", cache_path = tmp))
  result <- add_bootstrap_weights(tbl, weight_col = "WEIGHT",
                                  n_replicates = 8L, seed = 5L)
  cleaned <- remove_bootstrap_weights(result)
  on.exit(close_pumf(cleaned))

  bsw_cols <- grep("^CPBSW",colnames(cleaned), value = TRUE)
  expect_length(bsw_cols, 0L)
  expect_message(bsw_info(cleaned), regexp = "No bootstrap")
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
