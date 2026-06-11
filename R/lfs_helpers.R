#' Add a DATE column to an LFS table
#'
#' Creates a `DATE` column set to the first day of the survey month
#' (from `SURVYEAR` and `SURVMNTH`) and inserts it immediately after
#' `SURVMNTH`.
#'
#' @param tbl A lazy `dplyr::tbl()` returned by [get_pumf()] for an LFS
#'   survey.  Must contain integer columns `SURVYEAR` and `SURVMNTH`.
#'
#' @return The same lazy table with a new `DATE` column (type `Date`)
#'   positioned after `SURVMNTH`.
#'
#' @seealso [get_pumf()], [add_gender_sex()]
#'
#' @examples
#' \dontrun{
#' lfs <- get_pumf("LFS", "2023")
#' lfs |> add_lfs_date() |> dplyr::select(SURVYEAR, SURVMNTH, DATE) |>
#'   dplyr::distinct() |> dplyr::collect()
#' close_pumf(lfs)
#' }
#' @export
add_lfs_date <- function(tbl) {
  cols <- colnames(tbl)
  if (!all(c("SURVYEAR", "SURVMNTH") %in% cols))
    stop("'tbl' must contain SURVYEAR and SURVMNTH columns.", call. = FALSE)
  tbl |>
    dplyr::mutate(DATE = as.Date(dplyr::sql("MAKE_DATE(SURVYEAR, SURVMNTH, 1)"))) |>
    dplyr::relocate(DATE, .after = SURVMNTH)
}


#' Add a harmonised GENDER_SEX column to an LFS table
#'
#' LFS introduced `GENDER` (with values `"Men+"` / `"Women+"` / `"Non-binary
#' persons"`) to replace the binary `SEX` variable (`"Male"` / `"Female"`)
#' starting in 2020.  In any given row exactly one of the two columns is
#' non-`NA`.  `add_gender_sex()` coalesces them into a single `GENDER_SEX`
#' column, recoding `SEX` values to the `GENDER` scale so the result is
#' consistent across all LFS vintages.
#'
#' The mapping applied to `SEX` when `GENDER` is `NA`:
#' \itemize{
#'   \item `"Male"`   → `"Men+"`
#'   \item `"Female"` → `"Women+"`
#' }
#'
#' `GENDER_SEX` is inserted after `GENDER` when that column is present, or
#' after `SEX` otherwise.
#'
#' @param tbl A lazy `dplyr::tbl()` returned by [get_pumf()] for an LFS
#'   survey.  Must contain at least one of `SEX` or `GENDER`.
#'
#' @return The same lazy table with a new `GENDER_SEX` column positioned
#'   after `GENDER` (if present) or after `SEX`.
#'
#' @seealso [get_pumf()], [add_lfs_date()]
#'
#' @examples
#' \dontrun{
#' lfs <- get_pumf("LFS")
#' lfs |> add_gender_sex() |>
#'   dplyr::count(SEX, GENDER, GENDER_SEX) |> dplyr::collect()
#' close_pumf(lfs)
#' }
#' @export
add_gender_sex <- function(tbl) {
  cols       <- colnames(tbl)
  has_gender <- "GENDER" %in% cols
  has_sex    <- "SEX"    %in% cols

  if (!has_gender && !has_sex)
    stop("'tbl' must contain a SEX and/or GENDER column.", call. = FALSE)

  if (has_gender && has_sex) {
    tbl <- dplyr::mutate(tbl,
      GENDER_SEX = dplyr::coalesce(
        GENDER,
        dplyr::case_when(
          SEX == "Male"   ~ "Men+",
          SEX == "Female" ~ "Women+",
          TRUE            ~ NA_character_
        )
      )
    )
    tbl <- dplyr::relocate(tbl, GENDER_SEX, .after = GENDER)
  } else if (has_gender) {
    tbl <- dplyr::mutate(tbl, GENDER_SEX = GENDER) |>
      dplyr::relocate(GENDER_SEX, .after = GENDER)
  } else {
    tbl <- dplyr::mutate(tbl,
      GENDER_SEX = dplyr::case_when(
        SEX == "Male"   ~ "Men+",
        SEX == "Female" ~ "Women+",
        TRUE            ~ NA_character_
      )
    ) |>
      dplyr::relocate(GENDER_SEX, .after = SEX)
  }
  tbl
}
