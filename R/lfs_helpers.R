#' Add derived columns to an LFS table
#'
#' Adds columns the Labour Force Survey PUMF does not ship but most analyses
#' of it need.  Works on both unlabelled tables and labelled tables produced
#' by [label_pumf_columns()], where the new columns get their labels as
#' names.
#'
#' \describe{
#'   \item{`"SURVDATE"`}{A date column set to the first day of the survey
#'     month, inserted after the survey month column.  Built from `SURVYEAR` /
#'     `SURVMNTH` (labelled: `"Survey year"` / `"Survey month"`, output named
#'     `"Survey date"`).}
#'   \item{`"GENDER_SEX"`}{LFS introduced `GENDER` (`"Men+"` / `"Women+"` /
#'     `"Non-binary persons"`) to replace the binary `SEX` variable (`"Male"` /
#'     `"Female"`) starting in 2020; in any given row exactly one of the two
#'     columns is non-`NA`.  `GENDER_SEX` coalesces them into one harmonised
#'     column, recoding `SEX` to the `GENDER` scale (`"Male"` \eqn{\rightarrow}
#'     `"Men+"`, `"Female"` \eqn{\rightarrow} `"Women+"`), so the result is
#'     consistent across all LFS vintages.  It is inserted after `GENDER` when
#'     present, after `SEX` otherwise (labelled: `"Gender of respondent"` /
#'     `"Sex of respondent"`, output named `"Gender/sex of respondent"`).}
#' }
#'
#' @param tbl A lazy `dplyr::tbl()` returned by [get_pumf()] for an LFS
#'   survey, optionally passed through [label_pumf_columns()].
#' @param columns The columns to add: one or both of `"SURVDATE"` and
#'   `"GENDER_SEX"` (default both).
#'
#' @return The same lazy table with the new columns.
#'
#' @seealso [get_pumf()], [label_pumf_columns()]
#'
#' @examples
#' \donttest{
#' lfs <- get_pumf("LFS", "2023")   # NULL if StatCan is unreachable
#' if (!is.null(lfs)) {
#'   # Unlabelled
#'   lfs |> add_lfs_columns("SURVDATE") |>
#'     dplyr::select(SURVYEAR, SURVMNTH, SURVDATE) |>
#'     dplyr::distinct() |> dplyr::collect()
#'   lfs |> add_lfs_columns("GENDER_SEX") |>
#'     dplyr::count(GENDER, GENDER_SEX) |> dplyr::collect()
#'
#'   # Labelled
#'   lfs |> label_pumf_columns() |> add_lfs_columns() |>
#'     dplyr::select(`Survey year`, `Survey month`, `Survey date`,
#'                   `Gender/sex of respondent`) |>
#'     dplyr::distinct() |> dplyr::collect()
#'
#'   close_pumf(lfs)
#' }
#' }
#' @export
add_lfs_columns <- function(tbl, columns = c("SURVDATE", "GENDER_SEX")) {
  columns <- match.arg(columns, several.ok = TRUE)
  if ("SURVDATE" %in% columns)   tbl <- .add_lfs_SURVDATE(tbl)
  if ("GENDER_SEX" %in% columns) tbl <- .add_lfs_GENDER_SEX(tbl)
  tbl
}

# SURVDATE: the first day of the survey month, after the month column.
.add_lfs_SURVDATE <- function(tbl) {
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


# GENDER_SEX: GENDER, or SEX on the GENDER scale where GENDER is NA.
.add_lfs_GENDER_SEX <- function(tbl) {
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

  # SEX on the GENDER scale (quoted; evaluated lazily inside mutate()).
  sex_as_gender <- rlang::expr(dplyr::case_when(
    !!dplyr::sym(sex_col) == "Male"   ~ "Men+",
    !!dplyr::sym(sex_col) == "Female" ~ "Women+",
    TRUE                               ~ NA_character_
  ))
  value <- if (has_gender && has_sex)
    rlang::expr(dplyr::coalesce(!!dplyr::sym(gender_col), !!sex_as_gender))
  else if (has_gender) dplyr::sym(gender_col)
  else sex_as_gender

  dplyr::mutate(tbl, !!out_col := !!value, .after = dplyr::all_of(after_col))
}
