#' Add a date column to an LFS table
#'
#' Creates a date column set to the first day of the survey month and inserts
#' it immediately after the survey month column.  Works on both unlabelled
#' tables (columns `SURVYEAR` / `SURVMNTH`, date column named `SURVDATE`) and
#' labelled tables produced by [label_pumf_columns()] (columns `"Survey year"`
#' / `"Survey month"`, date column named `"Survey date"`).
#'
#' @param tbl A lazy `dplyr::tbl()` returned by [get_pumf()] for an LFS
#'   survey, optionally passed through [label_pumf_columns()].
#'
#' @return The same lazy table with a new date column positioned after the
#'   survey month column.
#'
#' @seealso [get_pumf()], [label_pumf_columns()], [add_lfs_GENDER_SEX()]
#'
#' @examples
#' \dontrun{
#' # Unlabelled
#' lfs <- get_pumf("LFS", "2023")
#' lfs |> add_lfs_SURVDATE() |> dplyr::select(SURVYEAR, SURVMNTH, SURVDATE) |>
#'   dplyr::distinct() |> dplyr::collect()
#'
#' # Labelled
#' lfs |> label_pumf_columns() |> add_lfs_SURVDATE() |>
#'   dplyr::select(`Survey year`, `Survey month`, `Survey date`) |>
#'   dplyr::distinct() |> dplyr::collect()
#'
#' close_pumf(lfs)
#' }
#' @export
add_lfs_SURVDATE <- function(tbl) {
  cols <- colnames(tbl)
  if (all(c("SURVYEAR", "SURVMNTH") %in% cols)) {
    yr_col   <- "SURVYEAR"
    mnth_col <- "SURVMNTH"
    date_col <- "SURVDATE"
  } else if (all(c("Survey year", "Survey month") %in% cols)) {
    yr_col   <- "Survey year"
    mnth_col <- "Survey month"
    date_col <- "Survey date"
  } else {
    stop("'tbl' must contain SURVYEAR/SURVMNTH or 'Survey year'/'Survey month'.",
         call. = FALSE)
  }
  sql_expr <- dplyr::sql(sprintf('MAKE_DATE("%s", "%s", 1)', yr_col, mnth_col))
  dplyr::mutate(tbl,
                !!date_col := as.Date(sql_expr),
                .after = dplyr::all_of(mnth_col))
}


#' Add a harmonised gender/sex column to an LFS table
#'
#' LFS introduced `GENDER` (with values `"Men+"` / `"Women+"` / `"Non-binary
#' persons"`) to replace the binary `SEX` variable (`"Male"` / `"Female"`)
#' starting in 2020.  In any given row exactly one of the two columns is
#' non-`NA`.  `add_lfs_GENDER_SEX()` coalesces them into a single harmonised
#' column, recoding `SEX` values to the `GENDER` scale so the result is
#' consistent across all LFS vintages.
#'
#' Works on both unlabelled tables (columns `SEX` / `GENDER`, output column
#' named `GENDER_SEX`) and labelled tables produced by [label_pumf_columns()]
#' (columns `"Sex of respondent"` / `"Gender of respondent"`, output column
#' named `"Gender/sex of respondent"`).
#'
#' The mapping applied to `SEX` / `"Sex of respondent"` when the gender
#' column is `NA`:
#' \itemize{
#'   \item `"Male"`   \eqn{\rightarrow} `"Men+"`
#'   \item `"Female"` \eqn{\rightarrow} `"Women+"`
#' }
#'
#' The output column is inserted after `GENDER` / `"Gender of respondent"`
#' when present, or after `SEX` / `"Sex of respondent"` otherwise.
#'
#' @param tbl A lazy `dplyr::tbl()` returned by [get_pumf()] for an LFS
#'   survey, optionally passed through [label_pumf_columns()].
#'
#' @return The same lazy table with a new harmonised gender/sex column.
#'
#' @seealso [get_pumf()], [label_pumf_columns()], [add_lfs_SURVDATE()]
#'
#' @examples
#' \dontrun{
#' lfs <- get_pumf("LFS")
#'
#' # Unlabelled
#' lfs |> add_lfs_GENDER_SEX() |>
#'   dplyr::count(SEX, GENDER, GENDER_SEX) |> dplyr::collect()
#'
#' # Labelled
#' lfs |> label_pumf_columns() |> add_lfs_GENDER_SEX() |>
#'   dplyr::count(`Sex of respondent`, `Gender of respondent`,
#'                `Gender/sex of respondent`) |> dplyr::collect()
#'
#' close_pumf(lfs)
#' }
#' @export
add_lfs_GENDER_SEX <- function(tbl) {
  cols <- colnames(tbl)

  if (any(c("GENDER", "SEX") %in% cols)) {
    gender_col <- "GENDER"
    sex_col    <- "SEX"
    out_col    <- "GENDER_SEX"
  } else if (any(c("Gender of respondent", "Sex of respondent") %in% cols)) {
    gender_col <- "Gender of respondent"
    sex_col    <- "Sex of respondent"
    out_col    <- "Gender/sex of respondent"
  } else {
    stop("'tbl' must contain SEX/GENDER or their labelled equivalents.",
         call. = FALSE)
  }

  has_gender <- gender_col %in% cols
  has_sex    <- sex_col    %in% cols
  after_col  <- if (has_gender) gender_col else sex_col

  if (has_gender && has_sex) {
    dplyr::mutate(tbl,
      !!out_col := dplyr::coalesce(
        !!dplyr::sym(gender_col),
        dplyr::case_when(
          !!dplyr::sym(sex_col) == "Male"   ~ "Men+",
          !!dplyr::sym(sex_col) == "Female" ~ "Women+",
          TRUE                               ~ NA_character_
        )
      ),
      .after = dplyr::all_of(after_col))
  } else if (has_gender) {
    dplyr::mutate(tbl,
      !!out_col := !!dplyr::sym(gender_col),
      .after = dplyr::all_of(after_col))
  } else {
    dplyr::mutate(tbl,
      !!out_col := dplyr::case_when(
        !!dplyr::sym(sex_col) == "Male"   ~ "Men+",
        !!dplyr::sym(sex_col) == "Female" ~ "Women+",
        TRUE                               ~ NA_character_
      ),
      .after = dplyr::all_of(after_col))
  }
}
