# R/lfs_timeline.R -- one harmonised table over LFS_HIST (1976-2005) and LFS
# (2006 onward).
#
# The two series stay in their own DuckDB files (separate write locks, see
# issue #18).  get_lfs_timeline() opens an in-memory DuckDB, ATTACHes both
# files READ_ONLY, and defines a view that maps each series onto a curated
# common schema (inst/extdata/lfs_timeline/, built by
# tools/build_lfs_timeline_reference.R):
#
#   variables.csv  harmonised variables: source column(s) in each series,
#                  type, scale of the current LFS values, first LFS_HIST month
#   codes.csv      harmonised bilingual value labels
#   recodes.csv    (source, source variable, source code) -> harmonised code
#
# The data tables store labels, not codes, so a recode is applied as
# source label -> harmonised label: the source labels come from the code lists
# of each series (LFS_HIST: the canonical dictionary and its code eras; LFS:
# the metadata of every loaded version).

.lfs_timeline_series <- c("LFS_HIST", "LFS")
.lfs_timeline_view   <- "lfs_timeline"

# Missing-value labels that some LFS builds left as ENUM levels; they map to NA
# like the missing codes themselves, so they are not reported as unmapped.
.lfs_timeline_na_labels <- c("Not applicable", "Non applicable", "Valid skip",
                             "Encha\u00eenement valide", "Not stated", "Non d\u00e9clar\u00e9")

.lfs_timeline_ref_cache <- new.env(parent = emptyenv())

.lfs_timeline_ref <- function(which) {
  if (is.null(.lfs_timeline_ref_cache[[which]])) {
    path <- system.file("extdata", "lfs_timeline", paste0(which, ".csv"),
                        package = "canpumf")
    if (!nzchar(path))
      stop("LFS timeline reference file '", which, ".csv' is missing from ",
           "the installed package.", call. = FALSE)
    .lfs_timeline_ref_cache[[which]] <- readr::read_csv(
      path, col_types = readr::cols(.default = "c"), na = "",
      locale = readr::locale(encoding = "UTF-8"), progress = FALSE)
  }
  .lfs_timeline_ref_cache[[which]]
}

# Code lists of one source series: name, val (no zero padding), label_en,
# label_fr, source ("LFS_HIST", "LFS_HIST_ERA" or "LFS").
.lfs_timeline_source_codes <- function(series, cache_path, versions) {
  strip0 <- function(x) as.character(as.integer(x))
  if (series == "LFS_HIST") {
    h <- .lfs_hist_ref("codes")[, c("name", "val", "label_en", "label_fr")]
    e <- .lfs_hist_code_eras()[, c("name", "val", "label_en", "label_fr")]
    out <- rbind(cbind(h, source = "LFS_HIST"), cbind(e, source = "LFS_HIST_ERA"))
  } else {
    out <- lapply(versions, function(v) {
      f <- file.path(cache_path, series, v, "metadata", "codes.csv")
      if (!file.exists(f)) return(NULL)
      readr::read_csv(f, col_types = readr::cols(.default = "c"), na = "",
                      progress = FALSE)[, c("name", "val", "label_en", "label_fr")]
    })
    out <- do.call(rbind, out)
    if (is.null(out))
      stop("No ", series, " metadata found under '",
           file.path(cache_path, series), "'.", call. = FALSE)
    out$source <- series
  }
  out$val <- strip0(out$val)
  unique(as.data.frame(out))
}

# Source label -> harmonised label for one harmonised variable and one source
# column.  Returns a named character vector (names: source labels); a source
# code deliberately without a harmonised equivalent maps to NA.
.lfs_timeline_label_map <- function(name, series, source_var, src_codes,
                                    recodes, codes, label_col) {
  rc <- recodes[recodes$name == name & recodes$source_var == source_var &
                  startsWith(recodes$source, series), ]
  rc <- data.frame(source = rc$source, val = rc$source_val, target_val = rc$val)
  sc <- src_codes[src_codes$name == source_var, ]
  j  <- merge(sc, rc, by = c("source", "val"))
  tgt <- codes[codes$name == name, ]
  j$target <- tgt[[label_col]][match(j$target_val, tgt$val)]
  j <- j[!is.na(j[[label_col]]) & !duplicated(j[[label_col]]), ]
  stats::setNames(j$target, j[[label_col]])
}

.lfs_timeline_sql_case <- function(con, col, map) {
  map <- map[!is.na(map)]
  if (length(map) == 0L) return("NULL")
  q <- function(x) as.character(DBI::dbQuoteString(con, x))
  paste0("CASE CAST(", col, " AS VARCHAR) ",
         paste0("WHEN ", q(names(map)), " THEN ", q(unname(map)), collapse = " "),
         " END")
}

# Levels of a DuckDB ENUM column, or NULL for other types.  Read from the
# catalogue, so no data is scanned.
.lfs_timeline_enum_levels <- function(con, db, table, col) {
  tp <- DBI::dbGetQuery(con, sprintf(
    "SELECT data_type FROM duckdb_columns() WHERE database_name = '%s' AND
       table_name = '%s' AND column_name = '%s'", db, table, col))$data_type
  if (length(tp) != 1L || !startsWith(tp, "ENUM(")) return(NULL)
  DBI::dbGetQuery(con, sprintf("SELECT unnest(enum_range(NULL::%s)) AS l", tp))$l
}

# SELECT list for one source series.
.lfs_timeline_select <- function(con, series, db, table, lang, src_codes,
                                 vars, codes, recodes) {
  label_col <- if (lang == "eng") "label_en" else "label_fr"
  have <- DBI::dbGetQuery(con, sprintf(
    "SELECT column_name FROM duckdb_columns() WHERE database_name = '%s'
       AND table_name = '%s'", db, table))$column_name
  qi  <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
  src_col <- if (series == "LFS_HIST") "lfs_hist" else "lfs"
  unmapped <- character(0L)
  exprs <- vapply(seq_len(nrow(vars)), function(i) {
    v    <- vars[i, ]
    cols <- strsplit(v[[src_col]] %||% "", "|", fixed = TRUE)[[1L]]
    cols <- cols[!is.na(cols) & cols %in% have]
    e <- switch(v$type,
      character = DBI::dbQuoteString(con, series),
      integer   = if (length(cols)) paste0("CAST(", qi(cols[1L]), " AS INTEGER)")
                  else "CAST(NULL AS INTEGER)",
      numeric   = {
        sc <- if (series == "LFS") suppressWarnings(as.numeric(v$lfs_scale)) else NA
        if (!length(cols)) "CAST(NULL AS DOUBLE)"
        else if (is.na(sc) || sc == 1) paste0("CAST(", qi(cols[1L]), " AS DOUBLE)")
        else paste0("CAST(", qi(cols[1L]), " AS DOUBLE) * ", sc)
      },
      factor    = {
        cases <- vapply(cols, function(cl) {
          map <- .lfs_timeline_label_map(v$name, series, cl, src_codes,
                                         recodes, codes, label_col)
          lv  <- .lfs_timeline_enum_levels(con, db, table, cl)
          miss <- setdiff(lv, c(names(map), .lfs_timeline_na_labels))
          if (length(miss)) unmapped <<- c(unmapped, paste0(cl, ": ", miss))
          .lfs_timeline_sql_case(con, qi(cl), map)
        }, "")
        e <- if (length(cases) == 0L) "NULL"
             else if (length(cases) == 1L) cases
             else paste0("COALESCE(", paste(cases, collapse = ", "), ")")
        if (series == "LFS_HIST" && !is.na(v$hist_from))
          e <- sprintf("CASE WHEN SURVYEAR * 100 + SURVMNTH < %d THEN NULL ELSE %s END",
                       as.integer(sub("-", "", v$hist_from)), e)
        paste0("CAST(", e, " AS ", qi(paste0("lfs_tl_", v$name)), ")")
      })
    paste0(e, " AS ", qi(v$name))
  }, "")
  if (length(unmapped))
    warning(series, " values without a harmonised equivalent (NA in the ",
            "timeline): ", paste(utils::head(unmapped, 10L), collapse = "; "),
            if (length(unmapped) > 10L) paste0("; ... (", length(unmapped), ")"),
            call. = FALSE)
  paste0("SELECT ", paste(exprs, collapse = ",\n  "), "\nFROM ", db, ".", table)
}

#' Harmonised Labour Force Survey timeline, 1976 onward
#'
#' Stacks the historical monthly LFS files (`"LFS_HIST"`, 1976 to 2005) and the
#' current LFS files (`"LFS"`, 2006 onward) into one lazy table with a curated
#' common set of variables, so that long time series can be pulled with a
#' single query.
#'
#' The two series keep their own DuckDB files. This function attaches both
#' **read-only** to an in-memory DuckDB and returns a view over them, so it
#' never blocks (and is never blocked by) other readers. It reads only what
#' is already loaded. Load data first with, for example,
#' `get_pumf("LFS_HIST", "1995")` or `get_pumf("LFS", "2015")`.
#'
#' The harmonised table has these columns:
#' * `SOURCE` (`"LFS_HIST"` or `"LFS"`), `SURVYEAR` and `SURVMNTH`.
#' * Numeric variables in plain units. The hours and wage variables of the
#'   current LFS files carry implied decimals (tenths of hours, cents), which
#'   are removed here. `FINALWT` is `FWEIGHT` in LFS_HIST.
#' * Categorical variables whose codes are the same in both series
#'   (e.g. `PROV`, `AGE_12`, `COWMAIN`, `EFAMTYPE`). These carry the current
#'   LFS labels.
#' * Recoded variables:
#'   - `LFSSTAT`: the three unemployed categories of LFS_HIST are collapsed.
#'   - `GENDER_SEX`: LFS_HIST `SEX` and the current `SEX`/`GENDER`, on the
#'     `GENDER` scale, as in [add_lfs_GENDER_SEX()].
#'   - `MARSTAT`: four categories (married or common-law, single, widowed,
#'     separated or divorced). The files before November 1999 only have these
#'     four.
#'   - `CMA`: Montreal, Toronto, Vancouver or other. It is `NA` before 1987,
#'     when LFS_HIST does not identify CMAs.
#'   - `SCHOOLN`: non-student, full-time or part-time student.
#'   - `AGYOWNK`: the four age groups of the youngest child in the current
#'     files.
#'   - `NAICS_18`: industry in the 18 groups of LFS_HIST. The 21 current
#'     groups are merged.
#'   - `EDUC`: the 1990 onward classification (LFS_HIST `EDUC90`). It is `NA`
#'     before 1990, whose categories do not map onto it.
#'   - `WHYPT`: reasons for part-time work from 1997 (`WHYPTNEW`).
#'
#' Occupation, immigration and the LFS_HIST-only family and spouse variables
#' are not part of the harmonised table. Use [get_pumf()] on each series for
#' those.
#'
#' Rebasing: LFS_HIST weights are rebased to different Censuses by period
#' (1987-1995 to 2001, 1996-2000 to 2006, 2001-2005 to 2011; 1976-1986 are
#' not rebased). Levels can therefore jump at the seams between periods and at
#' 2006.
#'
#' @param lang `"eng"` (default) or `"fra"` for the labels.
#' @param sources The series to include, by default both.
#' @param cache_path Root cache directory. Defaults to
#'   `getOption("canpumf.cache_path", tempdir())`.
#'
#' @return A lazy `dplyr::tbl()` over the view `lfs_timeline`. Categorical
#'   columns are factors. [label_pumf_columns()] and [pumf_var_labels()] work on
#'   it. Release it with [close_pumf()].
#'
#' @seealso [get_pumf()], [add_lfs_SURVDATE()]
#'
#' @examples
#' \donttest{
#' tl <- get_lfs_timeline()
#' if (!is.null(tl)) {
#'   tl |>
#'     dplyr::filter(SURVMNTH == 6L) |>
#'     dplyr::group_by(SURVYEAR, LFSSTAT) |>
#'     dplyr::summarise(persons = sum(FINALWT, na.rm = TRUE), .groups = "drop") |>
#'     dplyr::collect()
#'   close_pumf(tl)
#' }
#' }
#' @export
get_lfs_timeline <- function(lang = c("eng", "fra"),
                             sources = .lfs_timeline_series,
                             cache_path = getOption("canpumf.cache_path",
                                                    tempdir())) {
  lang    <- match.arg(lang)
  sources <- match.arg(sources, .lfs_timeline_series, several.ok = TRUE)
  vars    <- as.data.frame(.lfs_timeline_ref("variables"))
  codes   <- as.data.frame(.lfs_timeline_ref("codes"))
  recodes <- as.data.frame(.lfs_timeline_ref("recodes"))
  label_col <- if (lang == "eng") "label_en" else "label_fr"

  old <- options(duckdb.enable_rstudio_connection_pane = FALSE,
                 duckdb.force_rstudio_connection_pane  = FALSE)
  on.exit(options(old), add = TRUE)
  con <- DBI::dbConnect(duckdb::duckdb())
  ok  <- FALSE
  on.exit(if (!ok) DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  selects  <- character(0L)
  coverage <- character(0L)
  for (s in sources) {
    spec    <- .pumf_longitudinal_spec(s)
    db_path <- .long_db_path(spec, cache_path)
    if (!file.exists(db_path)) {
      message(s, ": nothing loaded (call get_pumf(\"", s, "\", \"",
              spec$example, "\") to load data).")
      next
    }
    db <- tolower(s)
    tryCatch(
      DBI::dbExecute(con, sprintf("ATTACH %s AS %s (READ_ONLY)",
                                  DBI::dbQuoteString(con, normalizePath(db_path)), db)),
      error = function(e)
        stop("Could not open ", basename(db_path), " read-only: ",
             conditionMessage(e), "\nIt is probably being written to (a ",
             "get_pumf(\"", s, "\", ...) load in another session). Retry ",
             "when that load has finished.", call. = FALSE))
    table <- .long_table_name(spec, lang)
    tabs  <- DBI::dbGetQuery(con, sprintf(
      "SELECT table_name FROM duckdb_tables() WHERE database_name = '%s'", db))$table_name
    if (!table %in% tabs) {
      message(s, ": no ", lang, " data loaded.")
      next
    }
    versions <- if (spec$versions_table %in% tabs) DBI::dbGetQuery(con, sprintf(
      "SELECT version FROM %s.%s ORDER BY survyear, survmnth", db,
      spec$versions_table))$version else character(0L)
    src_codes <- .lfs_timeline_source_codes(s, cache_path, versions)
    selects <- c(selects, .lfs_timeline_select(con, s, db, table, lang,
                                               src_codes, vars, codes, recodes))
    coverage <- c(coverage, paste0(s, " ", if (length(versions))
      paste0(versions[1L], "..", versions[length(versions)], " (",
             length(versions), " versions)") else "(no versions recorded)"))
  }
  if (length(selects) == 0L) {
    message("No LFS data loaded in '", cache_path, "'.")
    return(invisible(NULL))
  }

  for (nm in vars$name[vars$type == "factor"]) {
    lv <- codes[codes$name == nm, ]
    lv <- unique(lv[[label_col]][order(as.integer(lv$val))])
    DBI::dbExecute(con, sprintf("CREATE TYPE %s AS ENUM (%s)",
      DBI::dbQuoteIdentifier(con, paste0("lfs_tl_", nm)),
      paste(DBI::dbQuoteString(con, lv), collapse = ", ")))
  }
  DBI::dbExecute(con, paste0("CREATE VIEW ", .lfs_timeline_view, " AS\n",
                             paste(selects, collapse = "\nUNION ALL BY NAME\n")))
  .pumf_register_con(con, "LFS_TIMELINE", NA_character_, cache_path, lang)
  message("LFS timeline: ", paste(coverage, collapse = "; "), ".")
  ok <- TRUE
  tbl(con, .lfs_timeline_view)
}
