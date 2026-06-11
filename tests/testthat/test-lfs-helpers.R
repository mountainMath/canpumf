# Tests for add_lfs_date() and add_gender_sex().

.lfs_cache <- function() getOption("canpumf.cache_path", "")

# ---- add_lfs_date: input validation -----------------------------------------

test_that("add_lfs_date: errors when SURVYEAR/SURVMNTH absent", {
  expect_error(add_lfs_date(tibble::tibble(X = 1L)),
               "SURVYEAR.*SURVMNTH|SURVMNTH.*SURVYEAR")
})


# ---- add_lfs_date: unit (in-memory) -----------------------------------------

test_that("add_lfs_date: DATE is first-of-month, positioned after SURVMNTH", {
  df <- tibble::tibble(SURVYEAR = 2023L, SURVMNTH = 3L, X = 1L)
  result <- df |>
    dplyr::mutate(DATE = as.Date(paste0(SURVYEAR, "-", sprintf("%02d", SURVMNTH), "-01")))
  expect_equal(result$DATE, as.Date("2023-03-01"))
})


# ---- add_gender_sex: input validation ----------------------------------------

test_that("add_gender_sex: errors when neither SEX nor GENDER present", {
  expect_error(add_gender_sex(tibble::tibble(X = 1L)),
               "SEX.*GENDER|GENDER.*SEX")
})


# ---- add_gender_sex: unit (in-memory tibbles) --------------------------------

test_that("add_gender_sex: recodes SEX-only data to Men+/Women+", {
  df <- tibble::tibble(
    SEX = factor(c("Male", "Female", "Male", NA)),
    Z   = 1:4
  )
  result <- add_gender_sex(df)
  expect_true("GENDER_SEX" %in% names(result))
  # Position: after SEX
  expect_equal(which(names(result) == "GENDER_SEX"),
               which(names(result) == "SEX") + 1L)
  expect_equal(result$GENDER_SEX[!is.na(result$SEX)], c("Men+", "Women+", "Men+"))
  expect_true(is.na(result$GENDER_SEX[is.na(result$SEX)]))
})

test_that("add_gender_sex: passes GENDER through when SEX absent", {
  df <- tibble::tibble(
    GENDER = factor(c("Men+", "Women+", "Non-binary persons")),
    Z      = 1:3
  )
  result <- add_gender_sex(df)
  expect_equal(as.character(result$GENDER_SEX), as.character(result$GENDER))
  expect_equal(which(names(result) == "GENDER_SEX"),
               which(names(result) == "GENDER") + 1L)
})

test_that("add_gender_sex: coalesces when both columns present", {
  df <- tibble::tibble(
    SEX    = factor(c("Male", "Female", NA,      NA)),
    GENDER = factor(c(NA,     NA,       "Men+",  "Women+"))
  )
  result <- add_gender_sex(df)
  expect_equal(result$GENDER_SEX, c("Men+", "Women+", "Men+", "Women+"))
  # Positioned after GENDER
  expect_equal(which(names(result) == "GENDER_SEX"),
               which(names(result) == "GENDER") + 1L)
})


# ---- Integration: DuckDB-backed tbl -----------------------------------------

test_that("add_lfs_date: DATE column correct on cached LFS data", {
  skip_if(.lfs_cache() == "", "Cache not configured")
  lfs_dir <- file.path(.lfs_cache(), "LFS")
  skip_if_not(dir.exists(lfs_dir), "LFS not in cache")
  latest <- canpumf:::.pumf_lfs_latest_cached(.lfs_cache())
  skip_if(is.null(latest), "No LFS versions downloaded")

  tbl  <- suppressMessages(get_pumf("LFS", latest, cache_path = .lfs_cache()))
  on.exit(close_pumf(tbl))

  result <- add_lfs_date(tbl)
  cols   <- colnames(result)
  expect_true("DATE" %in% cols)
  expect_equal(which(cols == "DATE"), which(cols == "SURVMNTH") + 1L)

  sample <- dplyr::distinct(result, SURVYEAR, SURVMNTH, DATE) |>
    dplyr::collect() |>
    dplyr::slice(1L)
  expect_s3_class(sample$DATE, "Date")
  expect_equal(sample$DATE,
               as.Date(paste0(sample$SURVYEAR, "-",
                              sprintf("%02d", sample$SURVMNTH), "-01")))
})

test_that("add_gender_sex: GENDER_SEX correct on full LFS table", {
  skip_if(.lfs_cache() == "", "Cache not configured")
  skip_if_not(dir.exists(file.path(.lfs_cache(), "LFS")), "LFS not in cache")

  tbl <- suppressMessages(get_pumf("LFS", cache_path = .lfs_cache()))
  on.exit(close_pumf(tbl))

  result <- add_gender_sex(tbl)
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
