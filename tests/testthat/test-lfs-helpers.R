# Tests for add_lfs_SURVDATE() and add_lfs_GENDER_SEX().

.lfs_cache <- function() getOption("canpumf.cache_path", "")

# ---- add_lfs_SURVDATE: input validation -----------------------------------------

test_that("add_lfs_SURVDATE: errors when neither coded nor labelled columns present", {
  expect_error(add_lfs_SURVDATE(tibble::tibble(X = 1L)),
               "SURVYEAR.*SURVMNTH|Survey year.*Survey month")
})

test_that("add_lfs_SURVDATE: works on labelled tbl (Survey year / Survey month)", {
  df <- tibble::tibble(`Survey year` = 2023L, `Survey month` = 6L, Z = 1L)
  result <- df |>
    dplyr::mutate(`Survey date` =
                    as.Date(paste0(`Survey year`, "-", sprintf("%02d", `Survey month`), "-01")),
                  .after = `Survey month`)
  expect_equal(result$`Survey date`, as.Date("2023-06-01"))
  expect_equal(which(names(result) == "Survey date"),
               which(names(result) == "Survey month") + 1L)
})


# ---- add_lfs_SURVDATE: unit (in-memory) -----------------------------------------

test_that("add_lfs_SURVDATE: SURVDATE is first-of-month, positioned after SURVMNTH", {
  df <- tibble::tibble(SURVYEAR = 2023L, SURVMNTH = 3L, X = 1L)
  result <- df |>
    dplyr::mutate(SURVDATE = as.Date(paste0(SURVYEAR, "-", sprintf("%02d", SURVMNTH), "-01")))
  expect_equal(result$SURVDATE, as.Date("2023-03-01"))
})


# ---- add_lfs_GENDER_SEX: input validation ----------------------------------------

test_that("add_lfs_GENDER_SEX: errors when neither coded nor labelled columns present", {
  expect_error(add_lfs_GENDER_SEX(tibble::tibble(X = 1L)),
               "SEX.*GENDER|GENDER.*SEX|labelled")
})

test_that("add_lfs_GENDER_SEX: works on labelled tbl (Sex/Gender of respondent)", {
  df <- tibble::tibble(
    `Sex of respondent`    = factor(c("Male", "Female", NA,       NA)),
    `Gender of respondent` = factor(c(NA,     NA,       "Men+",   "Women+"))
  )
  result <- add_lfs_GENDER_SEX(df)
  expect_true("Gender/sex of respondent" %in% names(result))
  expect_equal(which(names(result) == "Gender/sex of respondent"),
               which(names(result) == "Gender of respondent") + 1L)
  expect_equal(result$`Gender/sex of respondent`,
               c("Men+", "Women+", "Men+", "Women+"))
})


# ---- add_lfs_GENDER_SEX: unit (in-memory tibbles) --------------------------------

test_that("add_lfs_GENDER_SEX: recodes SEX-only data to Men+/Women+", {
  df <- tibble::tibble(
    SEX = factor(c("Male", "Female", "Male", NA)),
    Z   = 1:4
  )
  result <- add_lfs_GENDER_SEX(df)
  expect_true("GENDER_SEX" %in% names(result))
  # Position: after SEX
  expect_equal(which(names(result) == "GENDER_SEX"),
               which(names(result) == "SEX") + 1L)
  expect_equal(result$GENDER_SEX[!is.na(result$SEX)], c("Men+", "Women+", "Men+"))
  expect_true(is.na(result$GENDER_SEX[is.na(result$SEX)]))
})

test_that("add_lfs_GENDER_SEX: passes GENDER through when SEX absent", {
  df <- tibble::tibble(
    GENDER = factor(c("Men+", "Women+", "Non-binary persons")),
    Z      = 1:3
  )
  result <- add_lfs_GENDER_SEX(df)
  expect_equal(as.character(result$GENDER_SEX), as.character(result$GENDER))
  expect_equal(which(names(result) == "GENDER_SEX"),
               which(names(result) == "GENDER") + 1L)
})

test_that("add_lfs_GENDER_SEX: coalesces when both columns present", {
  df <- tibble::tibble(
    SEX    = factor(c("Male", "Female", NA,      NA)),
    GENDER = factor(c(NA,     NA,       "Men+",  "Women+"))
  )
  result <- add_lfs_GENDER_SEX(df)
  expect_equal(result$GENDER_SEX, c("Men+", "Women+", "Men+", "Women+"))
  # Positioned after GENDER
  expect_equal(which(names(result) == "GENDER_SEX"),
               which(names(result) == "GENDER") + 1L)
})


# ---- Integration: DuckDB-backed tbl -----------------------------------------

test_that("add_lfs_SURVDATE: SURVDATE column correct on cached LFS data (unlabelled)", {
  skip_if(.lfs_cache() == "", "Cache not configured")
  lfs_dir <- file.path(.lfs_cache(), "LFS")
  skip_if_not(dir.exists(lfs_dir), "LFS not in cache")
  latest <- canpumf:::.pumf_lfs_latest_cached(.lfs_cache())
  skip_if(is.null(latest), "No LFS versions downloaded")

  tbl  <- suppressMessages(get_pumf("LFS", latest, cache_path = .lfs_cache()))
  on.exit(close_pumf(tbl))

  result <- add_lfs_SURVDATE(tbl)
  cols   <- colnames(result)
  expect_true("SURVDATE" %in% cols)
  expect_equal(which(cols == "SURVDATE"), which(cols == "SURVMNTH") + 1L)
  sample <- dplyr::distinct(result, SURVYEAR, SURVMNTH, SURVDATE) |>
    dplyr::collect() |> dplyr::slice(1L)
  expect_s3_class(sample$SURVDATE, "Date")
  expect_equal(sample$SURVDATE,
               as.Date(paste0(sample$SURVYEAR, "-",
                              sprintf("%02d", sample$SURVMNTH), "-01")))
})

test_that("add_lfs_SURVDATE: 'Survey date' column correct on labelled LFS data", {
  skip_if(.lfs_cache() == "", "Cache not configured")
  lfs_dir <- file.path(.lfs_cache(), "LFS")
  skip_if_not(dir.exists(lfs_dir), "LFS not in cache")
  latest <- canpumf:::.pumf_lfs_latest_cached(.lfs_cache())
  skip_if(is.null(latest), "No LFS versions downloaded")

  tbl  <- suppressMessages(get_pumf("LFS", latest, cache_path = .lfs_cache()))
  on.exit(close_pumf(tbl))

  result <- tbl |> label_pumf_columns() |> add_lfs_SURVDATE()
  cols   <- colnames(result)
  expect_true("Survey date" %in% cols)
  expect_equal(which(cols == "Survey date"), which(cols == "Survey month") + 1L)
  sample <- dplyr::distinct(result, `Survey year`, `Survey month`, `Survey date`) |>
    dplyr::collect() |> dplyr::slice(1L)
  expect_s3_class(sample$`Survey date`, "Date")
  expect_equal(sample$`Survey date`,
               as.Date(paste0(sample$`Survey year`, "-",
                              sprintf("%02d", sample$`Survey month`), "-01")))
})

test_that("add_lfs_GENDER_SEX: 'Gender/sex of respondent' correct on labelled LFS table", {
  skip_if(.lfs_cache() == "", "Cache not configured")
  skip_if_not(dir.exists(file.path(.lfs_cache(), "LFS")), "LFS not in cache")

  tbl <- suppressMessages(get_pumf("LFS", cache_path = .lfs_cache()))
  on.exit(close_pumf(tbl))

  result <- tbl |> label_pumf_columns() |> add_lfs_GENDER_SEX()
  cols   <- colnames(result)
  expect_true("Gender/sex of respondent" %in% cols)
  expect_equal(which(cols == "Gender/sex of respondent"),
               which(cols == "Gender of respondent") + 1L)

  counts <- dplyr::count(result, `Sex of respondent`,
                          `Gender of respondent`,
                          `Gender/sex of respondent`) |> dplyr::collect()
  old_rows <- counts[!is.na(counts$`Sex of respondent`), ]
  expect_equal(old_rows$`Gender/sex of respondent`[old_rows$`Sex of respondent` == "Male"],   "Men+")
  expect_equal(old_rows$`Gender/sex of respondent`[old_rows$`Sex of respondent` == "Female"], "Women+")
})

test_that("add_lfs_SURVDATE then label_pumf_columns: produces 'Survey date'", {
  skip_if(.lfs_cache() == "", "Cache not configured")
  lfs_dir <- file.path(.lfs_cache(), "LFS")
  skip_if_not(dir.exists(lfs_dir), "LFS not in cache")
  latest <- canpumf:::.pumf_lfs_latest_cached(.lfs_cache())
  skip_if(is.null(latest), "No LFS versions downloaded")

  tbl <- suppressMessages(get_pumf("LFS", latest, cache_path = .lfs_cache()))
  on.exit(close_pumf(tbl))

  result <- tbl |> add_lfs_SURVDATE() |> label_pumf_columns()
  cols   <- colnames(result)
  expect_false("SURVDATE" %in% cols,        label = "SURVDATE should be renamed")
  expect_true("Survey date" %in% cols,  label = "Survey date should be present")
  expect_equal(which(cols == "Survey date"), which(cols == "Survey month") + 1L)
})

test_that("add_lfs_GENDER_SEX then label_pumf_columns: produces 'Gender/sex of respondent'", {
  skip_if(.lfs_cache() == "", "Cache not configured")
  skip_if_not(dir.exists(file.path(.lfs_cache(), "LFS")), "LFS not in cache")

  tbl <- suppressMessages(get_pumf("LFS", cache_path = .lfs_cache()))
  on.exit(close_pumf(tbl))

  result <- tbl |> add_lfs_GENDER_SEX() |> label_pumf_columns()
  cols   <- colnames(result)
  expect_false("GENDER_SEX" %in% cols,
               label = "GENDER_SEX should be renamed")
  expect_true("Gender/sex of respondent" %in% cols,
              label = "Gender/sex of respondent should be present")
  expect_equal(which(cols == "Gender/sex of respondent"),
               which(cols == "Gender of respondent") + 1L)
})

test_that("add_lfs_GENDER_SEX: GENDER_SEX correct on full LFS table", {
  skip_if(.lfs_cache() == "", "Cache not configured")
  skip_if_not(dir.exists(file.path(.lfs_cache(), "LFS")), "LFS not in cache")

  tbl <- suppressMessages(get_pumf("LFS", cache_path = .lfs_cache()))
  on.exit(close_pumf(tbl))

  result <- add_lfs_GENDER_SEX(tbl)
  cols   <- colnames(result)
  expect_true("GENDER_SEX" %in% cols)
  # Should sit right after GENDER (which is present in LFS)
  expect_equal(which(cols == "GENDER_SEX"), which(cols == "GENDER") + 1L)

  counts <- dplyr::count(result, SEX, GENDER, GENDER_SEX) |> dplyr::collect()
  # Older rows: SEX present, GENDER NA — GENDER_SEX must be Men+/Women+
  old_rows <- counts[!is.na(counts$SEX), ]
  expect_true(all(old_rows$GENDER_SEX %in% c("Men+", "Women+")))
  expect_equal(old_rows$GENDER_SEX[old_rows$SEX == "Male"],   "Men+")
  expect_equal(old_rows$GENDER_SEX[old_rows$SEX == "Female"], "Women+")
  # Newer rows: GENDER present, SEX NA — GENDER_SEX must equal GENDER
  new_rows <- counts[is.na(counts$SEX), ]
  expect_true(all(new_rows$GENDER_SEX == as.character(new_rows$GENDER)))
})
