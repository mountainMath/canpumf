# R/api.R — Public entry points for the canpumf package.
#
# get_pumf()            — download, parse, label, and return a lazy DuckDB tbl
# label_pumf_columns()  — rename tbl columns to human-readable variable labels
# pumf_metadata()       — download and parse metadata only; return canonical list

# ---- Connection provenance registry -----------------------------------------
#
# DBI connections are S4 objects wrapping a C++ external pointer (Xptr).
# Assigning one to a new variable (e.g. con <- tbl$src$con) gives an R-level
# copy of the S4 wrapper, but the Xptr inside always points to the same C++
# object.  format(Xptr) returns the C++ address — a stable, unique key that
# survives S4 copies and dplyr tbl transformations.
#
# We use this as a key into a package-level environment so that provenance
# (series, version, cache_path, lang) set in get_pumf() can be retrieved in
# label_pumf_columns() even after the user has applied additional dplyr
# operations to the tbl.

.pumf_con_registry <- new.env(hash = TRUE, parent = emptyenv())

.pumf_register_con <- function(con, series, version, cache_path, lang) {
  key <- format(con@conn_ref)
  .pumf_con_registry[[key]] <- list(
    con        = con,
    series     = series,
    version    = version,
    cache_path = cache_path,
    lang       = lang
  )
  invisible(NULL)
}

# Close all registered connections whose DuckDB file matches db_path.
# Called before refresh deletes the file so the user doesn't need to
# manually close_pumf() before every get_pumf(..., refresh=TRUE).
.pumf_close_for_db <- function(db_path) {
  db_path  <- normalizePath(db_path, mustWork = FALSE)
  keys     <- ls(envir = .pumf_con_registry)
  n_closed <- 0L
  for (key in keys) {
    entry <- .pumf_con_registry[[key]]
    con   <- entry$con
    if (!DBI::dbIsValid(con)) {
      rm(list = key, envir = .pumf_con_registry)
      next
    }
    if (identical(normalizePath(con@driver@dbdir, mustWork = FALSE), db_path)) {
      message("Closing open connection to '", basename(db_path),
              "' for refresh.")
      rm(list = key, envir = .pumf_con_registry)
      DBI::dbDisconnect(con, shutdown = TRUE)
      n_closed <- n_closed + 1L
    }
  }
  # Run gc() to ensure C++ destructors fire and the OS file lock is released
  # before the caller attempts unlink().
  if (n_closed > 0L) gc(verbose = FALSE)
  invisible(n_closed)
}

.pumf_lookup_con <- function(con) {
  .pumf_con_registry[[format(con@conn_ref)]]
}


# ---- Internal helper: resolve deprecated parameter names --------------------

.api_resolve_deprecated <- function(series, version, cache_path,
                                     dots, fn_name) {
  if (!is.null(dots$pumf_series)) {
    warning(fn_name, ": argument 'pumf_series' is deprecated; use 'series'.",
            call. = FALSE)
    if (is.null(series)) series <- dots$pumf_series
  }
  if (!is.null(dots$pumf_version)) {
    warning(fn_name, ": argument 'pumf_version' is deprecated; use 'version'.",
            call. = FALSE)
    if (is.null(version)) version <- dots$pumf_version
  }
  if (!is.null(dots$pumf_cache_path)) {
    warning(fn_name, ": argument 'pumf_cache_path' is deprecated; use 'cache_path'.",
            call. = FALSE)
    cache_path <- dots$pumf_cache_path
  }
  # Silently drop args that no longer apply (guess_numeric, timeout, etc.)
  list(series = series, version = version, cache_path = cache_path)
}


# ---- get_pumf ---------------------------------------------------------------

#' Get a Statistics Canada PUMF dataset as a lazy DuckDB table
#'
#' Main entry point for the canpumf package.  Downloads (if needed), parses
#' metadata, applies bilingual labels, and returns a lazy `dplyr::tbl()` backed
#' by a DuckDB file in the cache directory.  Subsequent calls reuse the cached
#' DuckDB without re-downloading.
#'
#' The LFS is treated specially: all versions share a single `LFS.duckdb`
#' database.  Pass `version = "YYYY"` (annual) or `"YYYY-MM"` (monthly).
#' `refresh = "auto"` downloads every available LFS version that is not yet in
#' the database; this is only valid for LFS.
#'
#' @param series Survey series acronym, e.g. `"SFS"`, `"CHS"`, `"LFS"`,
#'   `"Census"`, `"CPSS"`.  See [list_canpumf_collection()] for all supported
#'   series and versions.
#' @param version Version string (e.g. `"2019"`, `"2021 (individuals)"`,
#'   `"2023-06"`).  For series with a single version omit or pass `NULL`.
#' @param lang `"eng"` (default) or `"fra"`.  Selects which set of labels to
#'   apply.  Each language creates a separate DuckDB table (created lazily on
#'   first request).
#' @param cache_path Root cache directory.  Defaults to
#'   `getOption("canpumf.cache_path", tempdir())`.  Set persistently in
#'   `.Rprofile` with `options(canpumf.cache_path = "<path>")`.
#' @param refresh `FALSE` (default) reuses cached data.  `TRUE` clears the
#'   DuckDB table and metadata and rebuilds from the already-extracted raw
#'   files (does not re-download).  `"auto"` is accepted for LFS only and
#'   downloads all available versions not yet in the database.
#' @param redownload If `TRUE`, delete the cached zip and extracted files and
#'   re-download from StatCan before rebuilding.  Implies `refresh = TRUE`.
#'   Not valid with `refresh = "auto"`.
#' @param read_only Open the DuckDB connection in read-only mode (default
#'   `TRUE`).  Pass `FALSE` to allow write access, e.g. to persist custom
#'   views or derived tables in the DuckDB file.  Use [close_pumf()] to
#'   release the connection when done.
#' @param ... Accepts deprecated parameter names (`pumf_series`,
#'   `pumf_version`, `pumf_cache_path`, `layout_mask`, `file_mask`,
#'   `guess_numeric`, `timeout`, `refresh_layout`) with a warning.
#'
#' @return A lazy `dplyr::tbl()` backed by a DuckDB connection.  Data values
#'   are pre-labeled as factors.  Call `dplyr::collect()` to materialise a
#'   local tibble, [label_pumf_columns()] to rename columns to their
#'   human-readable labels, or [close_pumf()] to release the connection.
#'
#' @seealso [label_pumf_columns()], [pumf_var_labels()], [pumf_metadata()],
#'   [close_pumf()], [list_canpumf_collection()]
#'
#' @examples
#' \dontrun{
#' # Download and open the SFS 2019 as a lazy DuckDB table
#' sfs <- get_pumf("SFS", "2019")
#' dplyr::glimpse(sfs)
#'
#' # Collect a local tibble after filtering
#' high_wealth <- sfs |>
#'   dplyr::filter(PEFAMID == 1) |>
#'   dplyr::collect()
#'
#' # French labels
#' sfs_fr <- get_pumf("SFS", "2019", lang = "fra")
#'
#' # LFS: annual version
#' lfs <- get_pumf("LFS", "2022")
#'
#' # Release the connection when done
#' close_pumf(sfs)
#' }
#' @export
get_pumf <- function(series     = NULL,
                     version    = NULL,
                     lang       = "eng",
                     cache_path = getOption("canpumf.cache_path", tempdir()),
                     refresh    = FALSE,
                     redownload = FALSE,
                     read_only  = TRUE,
                     ...) {
  dots <- list(...)
  resolved <- .api_resolve_deprecated(series, version, cache_path, dots, "get_pumf")
  series     <- resolved$series
  version    <- resolved$version
  cache_path <- resolved$cache_path

  if (is.null(series))
    stop("'series' must be specified (e.g. get_pumf(\"SFS\", \"2019\")).")
  version <- pumf_resolve_version(series, version)
  stopifnot(lang %in% c("eng", "fra"))

  if (!identical(refresh, FALSE) && !identical(refresh, TRUE) &&
      !identical(refresh, "auto"))
    stop("'refresh' must be FALSE, TRUE, or \"auto\".")
  if (identical(refresh, "auto") && series != "LFS")
    stop("refresh = \"auto\" is only valid for LFS. ",
         "Use refresh = TRUE to rebuild a specific non-LFS survey version.")
  if (isTRUE(redownload) && identical(refresh, "auto"))
    stop("redownload = TRUE is not compatible with refresh = \"auto\". ",
         "Call lfs_get_pumf() per version instead.")

  # Resolve single-version non-LFS series so table_name and db_path are known.
  if (series != "LFS" && is.null(version)) {
    collection <- list_canpumf_collection()
    rows <- filter(collection, .data$Acronym == series)
    if (nrow(rows) == 0L)
      stop("Unknown series '", series,
           "'. Check list_canpumf_collection() for available series.")
    if (nrow(rows) > 1L)
      stop("Series '", series, "' has multiple versions: ",
           paste(rows$Version, collapse = ", "),
           ".\nSpecify 'version' (e.g. get_pumf(\"", series, "\", \"",
           rows$Version[[1L]], "\")).")
    version <- rows$Version[[1L]]
  }

  # For LFS: route directly through lfs_get_pumf so that when data is already
  # loaded the fast path opens only a read-only connection.  Routing through
  # get_pumf_connection (the deprecated shim) always requests read_only=FALSE,
  # which tries to acquire a write lock even when no write is needed — that
  # fails when a read-only connection from a previous get_pumf("LFS", ...) call
  # is still open.
  if (series == "LFS") {
    tbl <- suppressMessages(
      lfs_get_pumf(version    = version,
                   lang       = lang,
                   cache_path = cache_path,
                   refresh    = refresh,
                   redownload = redownload,
                   read_only  = read_only)
    )
    if (is.null(tbl)) return(invisible(NULL))
    # ensure nicer column order
    cn <- colnames(tbl)
    if ("SEX" %in% cn && "GENDER" %in% cn) {
      isex    <- which(cn == "SEX"); igender <- which(cn == "GENDER")
      tbl <- if (isex > igender) relocate(tbl, "SEX",    .before = "GENDER")
             else                relocate(tbl, "GENDER", .after  = "SEX")
    }
    .pumf_register_con(tbl$src$con, series, version, cache_path, lang)
    return(tbl)
  }

  # Ensure the DB is built and get a temporary read-write connection.
  con <- suppressMessages(
    get_pumf_connection(series     = series,
                        version    = version,
                        lang       = lang,
                        cache_path = cache_path,
                        refresh    = refresh,
                        redownload = redownload)
  )
  if (is.null(con)) return(invisible(NULL))

  # Select the language table from the connection.
  table_name <- .pumf_table_name(series, version, lang)
  db_path    <- .pumf_db_path(series, version, cache_path)

  if (read_only) {
    # Re-open as read-only so callers cannot accidentally mutate the DB.
    DBI::dbDisconnect(con, shutdown = TRUE)
    tbl <- pumf_open_duckdb(db_path, table_name, read_only = TRUE)
  } else {
    if (!DBI::dbExistsTable(con, table_name)) {
      DBI::dbDisconnect(con, shutdown = TRUE)
      stop("Table '", table_name, "' not found in ", db_path, ".")
    }
    tbl <- tbl(con, table_name)
  }

  # Register provenance so label_pumf_columns() can find it later via the
  # connection's stable C++ pointer address.
  .pumf_register_con(tbl$src$con, series, version, cache_path, lang)


  tbl
}


# ---- shared metadata helper -------------------------------------------------

# Read the variables tibble for a tbl returned by get_pumf().
# Returns a data.frame(name, label_en, label_fr, type, ...) from metadata/.
.pumf_read_variables <- function(tbl) {
  prov <- .pumf_lookup_con(tbl$src$con)
  if (is.null(prov))
    stop("'tbl' has no pumf provenance. Was it created by get_pumf()?",
         call. = FALSE)
  series     <- prov$series
  version    <- prov$version
  cache_path <- prov$cache_path

  if (series == "LFS") {
    db_path <- file.path(cache_path, "LFS", "LFS.duckdb")
    if (!file.exists(db_path))
      stop("LFS database not found at '", db_path, "'.", call. = FALSE)
    con_tmp <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
    all_versions <- if (DBI::dbExistsTable(con_tmp, "lfs_versions"))
      DBI::dbGetQuery(
        con_tmp,
        "SELECT version FROM lfs_versions ORDER BY survyear, survmnth")$version
    else character(0L)
    DBI::dbDisconnect(con_tmp, shutdown = TRUE)
    if (length(all_versions) == 0L)
      stop("No LFS versions found in the database.", call. = FALSE)
    all_vars <- purrr::map(all_versions, function(v) {
      md <- file.path(cache_path, "LFS", v, "metadata")
      if (!dir.exists(md)) return(NULL)
      tryCatch(read_metadata(md)$variables, error = function(e) NULL)
    })
    all_vars  <- do.call(rbind, all_vars[!vapply(all_vars, is.null, logical(1L))])
    if (is.null(all_vars) || nrow(all_vars) == 0L)
      stop("No LFS metadata found in any version directory.", call. = FALSE)
    all_vars[!duplicated(all_vars$name, fromLast = TRUE), , drop = FALSE]
  } else {
    meta_dir <- file.path(cache_path, series, version, "metadata")
    if (!dir.exists(meta_dir))
      stop("Metadata directory not found: '", meta_dir, "'. ",
           "Run get_pumf(\"", series, "\", \"", version, "\") first.",
           call. = FALSE)
    vars <- read_metadata(meta_dir)$variables
    # DuckDB column names are uppercased at build time; normalise metadata
    # names so mixed-case command-file declarations (e.g. "TotInc") match.
    vars$name <- toupper(vars$name)
    vars
  }
}


# ---- label_pumf_columns -----------------------------------------------------

#' Rename PUMF table columns to human-readable variable labels
#'
#' Takes a lazy `dplyr::tbl()` returned by [get_pumf()] and returns the same
#' lazy table with column names replaced by the variable labels from the survey
#' metadata (e.g. `PHHSIZE` becomes `"Household size"`).  Duplicate labels are
#' disambiguated by appending ` (VAR_NAME)`.
#'
#' The `tbl` must have been produced by [get_pumf()]; the function reads survey
#' provenance (series, version, cache path, language) from the underlying
#' DuckDB connection.  Use [pumf_var_labels()] to inspect the name-to-label
#' mapping without renaming.
#'
#' @param tbl A lazy `dplyr::tbl()` returned by [get_pumf()].
#'
#' @return A lazy `dplyr::tbl()` with column names replaced by human-readable
#'   variable labels.  Columns with no metadata label are left unchanged.
#'
#' @seealso [pumf_var_labels()], [get_pumf()]
#'
#' @examples
#' \dontrun{
#' sfs <- get_pumf("SFS", "2019")
#' sfs_labeled <- label_pumf_columns(sfs)
#' colnames(sfs_labeled)
#' close_pumf(sfs_labeled)
#' }
#' @export
label_pumf_columns <- function(tbl) {
  prov      <- .pumf_lookup_con(tbl$src$con)
  if (is.null(prov))
    stop("'tbl' has no pumf provenance. Was it created by get_pumf()?",
         call. = FALSE)
  lang      <- prov$lang %||% "eng"
  label_col <- if (lang == "eng") "label_en" else "label_fr"
  variables <- .pumf_read_variables(tbl)

  var_labels <- variables[!is.na(variables[[label_col]]),
                           c("name", label_col), drop = FALSE]
  names(var_labels)[2L] <- "label"

  # Disambiguate duplicate labels by appending (NAME)
  dups <- var_labels$label[duplicated(var_labels$label)]
  if (length(dups) > 0L) {
    is_dup <- var_labels$label %in% dups
    var_labels$label[is_dup] <-
      paste0(var_labels$label[is_dup], " (", var_labels$name[is_dup], ")")
  }

  # Only rename columns present in the tbl
  tbl_cols   <- colnames(tbl)
  var_labels <- var_labels[var_labels$name %in% tbl_cols, , drop = FALSE]

  # Inject labels for derived LFS helper columns that are not in the metadata.
  derived <- c(SURVDATE   = "Survey date",
               GENDER_SEX = "Gender/sex of respondent")
  present_derived <- derived[names(derived) %in% tbl_cols]
  if (length(present_derived) > 0L) {
    extra <- data.frame(name  = names(present_derived),
                        label = unname(present_derived),
                        stringsAsFactors = FALSE)
    var_labels <- rbind(var_labels, extra)
  }

  if (nrow(var_labels) == 0L) return(tbl)

  rename_map <- stats::setNames(var_labels$name, var_labels$label)
  rename(tbl, !!!rename_map)
}


# ---- pumf_var_labels --------------------------------------------------------

#' Retrieve variable labels as a tibble
#'
#' Returns a tibble mapping short coded column names to their bilingual
#' human-readable variable labels.  Use this as a quick reference without
#' renaming the table itself; to rename, use [label_pumf_columns()].
#'
#' @param tbl A lazy `dplyr::tbl()` returned by [get_pumf()].
#'
#' @return A tibble with columns `name` (coded column name), `label_en`
#'   (English label), and `label_fr` (French label).  Rows follow
#'   survey-metadata order.
#'
#' @seealso [label_pumf_columns()], [get_pumf()]
#'
#' @examples
#' \dontrun{
#' sfs <- get_pumf("SFS", "2019")
#' pumf_var_labels(sfs)
#' close_pumf(sfs)
#' }
#' @export
pumf_var_labels <- function(tbl) {
  variables <- .pumf_read_variables(tbl)
  tibble::as_tibble(variables[, c("name", "label_en", "label_fr"), drop = FALSE])
}


# ---- close_pumf -------------------------------------------------------------

#' Close the DuckDB connection backing a PUMF lazy table
#'
#' Disconnects the DuckDB connection embedded in a lazy `dplyr::tbl()` returned
#' by [get_pumf()].  After calling this function the table can no longer be
#' queried.
#'
#' Closing is only necessary when you need to release the file lock — for
#' example, before calling `get_pumf(..., refresh = TRUE)` on the same survey,
#' or before writing to the DuckDB from another process.  Read-only connections
#' (the default) do not block other readers.
#'
#' @param tbl A lazy `dplyr::tbl()` returned by [get_pumf()].
#'
#' @return Invisibly `NULL`.
#'
#' @seealso [get_pumf()]
#'
#' @examples
#' \dontrun{
#' sfs <- get_pumf("SFS", "2019")
#' # ... analysis ...
#' close_pumf(sfs)
#' }
#' @export
close_pumf <- function(tbl) {
  con <- tbl$src$con
  if (!is.null(con) && DBI::dbIsValid(con)) {
    rm(list = format(con@conn_ref), envir = .pumf_con_registry, inherits = FALSE)
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
  invisible(NULL)
}


# ---- add_bootstrap_weights ---------------------------------------------

#' Generate bootstrap weights for a PUMF dataset
#'
#' For a **DuckDB-backed lazy table** (the typical case), bootstrap replicate
#' weights are written directly into the DuckDB file as a separate table and
#' exposed through a persistent VIEW that joins the main survey table with the
#' BSW columns.  The returned `tbl` references this view, so all downstream
#' dplyr operations have access to every replicate.
#'
#' For an **in-memory `data.frame` or `tibble`**, bootstrap weights are
#' generated entirely in memory and the augmented data frame is returned.
#'
#' Bootstrap weights are generated by the rescaled bootstrap: for each replicate
#' a sample of \eqn{n} rows is drawn with replacement; the bootstrap weight for
#' row \eqn{i} in replicate \eqn{b} is `original_weight[i] * count[i,b]`, where
#' `count[i,b]` is the number of times row \eqn{i} appeared in draw \eqn{b}.
#'
#' **Multiple weight columns (hierarchical data):** by default `bsw_table` is
#' named after `weight_col` (e.g. `"pumf_bsw_wstpwgt"`), so calling the
#' function twice with different weight columns (e.g. household weight and
#' person weight) produces two independent BSW tables and two separate views
#' without any conflict.
#'
#' **Connection note (DuckDB path):** calling this function fully shuts down the
#' DuckDB in-process instance held by `tbl` (because a write connection requires
#' exclusive access).  The input `tbl` and any other lazy tables backed by the
#' same DuckDB file become invalid after the call.  Use the returned tbl instead.
#'
#' **Filtered input tbls (DuckDB path):** bootstrap weights always cover the
#' complete physical survey table.  If `tbl` has dplyr `filter()` operations
#' applied, they are captured and automatically re-applied to the returned VIEW
#' tbl so the visible rows match the original subset.  Other operations
#' (`select()`, `mutate()`, etc.) are not replayed — they would interfere with
#' the BSW columns — so apply them manually to the returned tbl if needed.
#'
#' **ID column (DuckDB path):** a stable row identifier is needed to link the
#' main table to the BSW table.  If `id_col` is `NULL` (the default):
#'   * The survey registry `bsw_join_key` is used when available (e.g.
#'     `"PEFAMID"` for SFS 2016–2023) — no table modification needed.
#'   * Otherwise a `pumf_row_id` column (DuckDB `rowid`) is added to the main
#'     survey table.  The `ALTER TABLE ADD COLUMN` is O(1); the `UPDATE` that
#'     fills the values is O(n).
#'
#' @param tbl A lazy `dplyr::tbl()` returned by [get_pumf()], **or** an
#'   in-memory `data.frame` / `tibble`.
#' @param weight_col Name of the column holding the survey weights (string,
#'   e.g. `"WSTPWGT"`).
#' @param id_col Optional name of a column that uniquely identifies each row
#'   (DuckDB path only).  If `NULL` (default), the registry `bsw_join_key` is
#'   used when available; otherwise `pumf_row_id` is added to the main table.
#' @param n_replicates Number of bootstrap replicates to generate (default
#'   `500L`).
#' @param prefix Column-name prefix for replicate columns (default `"BSW"`).
#'   Columns are named `prefix1`, `prefix2`, …
#' @param bsw_table Name of the DuckDB table that stores the replicate weights
#'   (DuckDB path only).  Defaults to `NULL`, which auto-names it
#'   `paste0("pumf_bsw_", tolower(weight_col))` so separate calls with
#'   different weight columns do not overwrite each other.
#' @param seed Optional integer seed for reproducibility.
#' @param overwrite If the `bsw_table` already exists in the DuckDB file,
#'   overwrite it when `TRUE`; stop with an error when `FALSE` (default).
#'
#' @return
#'   * **DuckDB path:** a lazy `dplyr::tbl()` backed by a persistent DuckDB
#'     VIEW that contains all original survey columns plus the `n_replicates`
#'     bootstrap weight columns, with any input `filter()` operations re-applied.
#'   * **In-memory path:** the input `data.frame` / `tibble` with
#'     `n_replicates` new BSW columns appended.
#'
#' @seealso [bsw_info()], [remove_bootstrap_weights()], [get_pumf()]
#'
#' @examples
#' \dontrun{
#' sfs <- get_pumf("SFS", "2019")
#' sfs_bsw <- add_bootstrap_weights(sfs, weight_col = "WSTPWGT",
#'                                  n_replicates = 200L, seed = 42L)
#' bsw_info(sfs_bsw)
#' close_pumf(sfs_bsw)
#' }
#' @export
add_bootstrap_weights <- function(tbl,
                                       weight_col,
                                       id_col       = NULL,
                                       n_replicates = 500L,
                                       prefix       = "BSW",
                                       bsw_table    = NULL,
                                       seed         = NULL,
                                       overwrite    = FALSE) {

  stopifnot(is.character(weight_col), length(weight_col) == 1L)
  stopifnot(is.numeric(n_replicates), n_replicates >= 1L)
  n_replicates <- as.integer(n_replicates)

  # Auto-name BSW table after weight_col so separate calls for household vs
  # person weights land in independent tables.
  if (is.null(bsw_table))
    bsw_table <- paste0("pumf_bsw_", tolower(weight_col))

  # ---- Dispatch: in-memory (data.frame / tibble) ----------------------------
  if (is.data.frame(tbl))
    return(.add_bsw_inmemory(tbl, weight_col, n_replicates, prefix, seed))

  # ---- DuckDB-backed lazy tbl path ------------------------------------------

  con <- tbl$src$con
  if (is.null(con) || !DBI::dbIsValid(con))
    stop("The connection backing 'tbl' is no longer valid.", call. = FALSE)

  prov <- .pumf_lookup_con(con)
  if (is.null(prov))
    stop("'tbl' has no pumf provenance. Was it created by get_pumf()?",
         call. = FALSE)

  series     <- prov$series
  version    <- prov$version
  cache_path <- prov$cache_path
  lang       <- prov$lang %||% "eng"

  if (series == "LFS")
    stop("add_bootstrap_weights() is not supported for LFS. ",
         "LFS includes official bootstrap weights from Statistics Canada.",
         call. = FALSE)

  table_name <- .pumf_table_name(series, version, lang)
  db_path    <- .pumf_db_path(series, version, cache_path)

  # Capture the rendered SQL *before* closing the connection.  This is used to:
  #   (a) detect non-replayable ops (GROUP BY, HAVING, DISTINCT, column-select),
  #   (b) extract any WHERE clause to re-apply to the returned view tbl.
  # tbl$ops is restricted in newer dbplyr; sql_render() is the public interface.
  input_sql <- as.character(dbplyr::sql_render(tbl))

  # Warn when the SQL has ops we cannot cleanly replay (they would either drop
  # BSW columns or change aggregation semantics).
  # "Base" SELECT forms: "SELECT *" and "SELECT table.*" (filter() uses the
  # latter). Any other column list (from select()) would drop BSW columns.
  has_complex_ops <-
    grepl("\\bGROUP\\s+BY\\b|\\bHAVING\\b|\\bDISTINCT\\b",
          input_sql, ignore.case = TRUE) ||
    grepl("^SELECT\\s+(?!\\*|\\w+\\.\\*)", trimws(input_sql), perl = TRUE)

  if (has_complex_ops)
    warning(
      "The input tbl has dplyr operations (select, group_by, etc.) that ",
      "cannot be replayed on the BSW view \u2014 they would drop BSW columns or ",
      "change aggregation semantics. Apply them manually to the returned tbl.",
      call. = FALSE
    )

  # Extract the WHERE portion (+ ORDER BY if any) for re-application.
  where_start  <- regexpr("(?i)WHERE\\s", input_sql, perl = TRUE)
  where_clause <- if (where_start > 0L) substring(input_sql, where_start) else NULL
  # VIEW name: "pumf_bsw_wstpwgt" → "eng_bsw_wstpwgt"; preserves weight_col scope.
  view_name  <- paste0(table_name, "_", sub("^pumf_", "", bsw_table))

  # --- Determine row-identifier column ---------------------------------------
  reg <- pumf_registry_lookup(series, version)
  if (is.null(id_col)) {
    if (!is.null(reg$bsw_join_key) && length(reg$bsw_join_key) == 1L)
      id_col <- reg$bsw_join_key
  }

  # --- Pull id + weight from read-only connection ----------------------------
  id_sql <- if (!is.null(id_col))
    sprintf('"%s" AS row_id', id_col)
  else
    "rowid AS row_id"   # DuckDB virtual column; stable for unmodified heap tables

  wt_data <- DBI::dbGetQuery(
    con,
    sprintf('SELECT %s, CAST("%s" AS DOUBLE) AS w FROM "%s"',
            id_sql, weight_col, table_name)
  )
  n       <- nrow(wt_data)
  w       <- wt_data$w
  row_ids <- wt_data$row_id

  if (anyNA(w)) {
    warning(sum(is.na(w)), " NA weight(s) in '", weight_col,
            "' replaced with 0.", call. = FALSE)
    w[is.na(w)] <- 0
  }

  mem_gb <- n_replicates * n * 8 / 1e9
  if (mem_gb > 2)
    warning(sprintf(
      "Generating %d replicates for %d rows requires ~%.1f GB of memory.",
      n_replicates, n, mem_gb
    ), call. = FALSE)

  # --- Generate bootstrap counts ---------------------------------------------
  message(sprintf(
    "Generating %d bootstrap weight replicates for %d observations...",
    n_replicates, n
  ))
  if (!is.null(seed)) set.seed(seed)

  # vapply + tabulate: O(n × B) entirely at the C level, no per-column R loop.
  counts <- vapply(
    seq_len(n_replicates),
    function(i) tabulate(sample.int(n, n, replace = TRUE), nbins = n),
    integer(n)
  )
  bsw_matrix    <- w * counts   # double × integer → double matrix
  bsw_col_names <- paste0(prefix, seq_len(n_replicates))
  colnames(bsw_matrix) <- bsw_col_names

  bsw_df <- cbind(
    stats::setNames(
      data.frame(row_ids),
      if (!is.null(id_col)) id_col else "pumf_row_id"
    ),
    as.data.frame(bsw_matrix)
  )

  # --- Acquire exclusive write access ----------------------------------------
  # DuckDB in-process sharing: while a read-only instance is alive, dbConnect()
  # to the same file returns read-only even with read_only=FALSE.  Full shutdown
  # is required before a true write connection can be opened.
  rm(list = intersect(format(con@conn_ref), ls(envir = .pumf_con_registry)),
     envir = .pumf_con_registry, inherits = FALSE)
  if (DBI::dbIsValid(con))
    DBI::dbDisconnect(con, shutdown = TRUE)

  # --- Write BSW table and VIEW to DuckDB ------------------------------------
  rw_con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)
  on.exit(
    if (DBI::dbIsValid(rw_con)) DBI::dbDisconnect(rw_con, shutdown = TRUE),
    add = TRUE
  )

  # Add pumf_row_id when no natural key exists (ALTER TABLE ADD COLUMN is O(1)).
  if (is.null(id_col)) {
    if (!"pumf_row_id" %in% DBI::dbListFields(rw_con, table_name)) {
      message("Adding 'pumf_row_id' column to '", table_name, "'...")
      DBI::dbExecute(rw_con,
        sprintf('ALTER TABLE "%s" ADD COLUMN pumf_row_id BIGINT', table_name))
      DBI::dbExecute(rw_con,
        sprintf('UPDATE "%s" SET pumf_row_id = rowid', table_name))
    }
    id_col <- "pumf_row_id"
  }

  if (!overwrite && DBI::dbExistsTable(rw_con, bsw_table))
    stop("Table '", bsw_table, "' already exists in '", basename(db_path),
         "'. Use overwrite = TRUE to replace it.", call. = FALSE)

  message("Writing bootstrap weight table '", bsw_table, "' to DuckDB...")
  DBI::dbWriteTable(rw_con, bsw_table, bsw_df, overwrite = TRUE)

  DBI::dbExecute(rw_con, sprintf(
    'CREATE INDEX IF NOT EXISTS "idx_%s" ON "%s" ("%s")',
    bsw_table, bsw_table, id_col
  ))

  bsw_col_sql <- paste0('"b"."', bsw_col_names, '"', collapse = ", ")
  DBI::dbExecute(rw_con, sprintf(
    'CREATE OR REPLACE VIEW "%s" AS SELECT "m".*, %s FROM "%s" "m" JOIN "%s" "b" ON "m"."%s" = "b"."%s"',
    view_name, bsw_col_sql, table_name, bsw_table, id_col, id_col
  ))

  DBI::dbDisconnect(rw_con, shutdown = TRUE)

  # --- Reopen read-only, register provenance, re-apply WHERE if needed -------
  if (!file.exists(db_path))
    stop("DuckDB file not found after write: ", db_path, call. = FALSE)
  ro_con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)

  if (!is.null(where_clause)) {
    # Re-apply the captured WHERE (and ORDER BY) by building a new SQL query
    # that targets the view instead of the physical table.
    new_sql <- sprintf('SELECT *\nFROM "%s"\n%s', view_name, where_clause)
    new_tbl <- dplyr::tbl(ro_con, dplyr::sql(new_sql))
  } else {
    if (!DBI::dbExistsTable(ro_con, view_name)) {
      DBI::dbDisconnect(ro_con, shutdown = TRUE)
      stop("View '", view_name, "' not found in ", db_path, ".", call. = FALSE)
    }
    new_tbl <- dplyr::tbl(ro_con, view_name)
  }

  .pumf_register_con(ro_con, series, version, cache_path, lang)

  new_tbl
}


# Fast in-memory bootstrap weight generation for data.frame / tibble input.
.add_bsw_inmemory <- function(df, weight_col, n_replicates, prefix, seed) {
  n <- nrow(df)
  w <- df[[weight_col]]
  if (!is.numeric(w)) w <- suppressWarnings(as.numeric(w))
  if (anyNA(w)) {
    warning(sum(is.na(w)), " NA weight(s) in '", weight_col,
            "' replaced with 0.", call. = FALSE)
    w[is.na(w)] <- 0
  }
  if (!is.null(seed)) set.seed(seed)
  counts <- vapply(
    seq_len(n_replicates),
    function(i) tabulate(sample.int(n, n, replace = TRUE), nbins = n),
    integer(n)
  )
  bsw_matrix <- w * counts
  colnames(bsw_matrix) <- paste0(prefix, seq_len(n_replicates))
  cbind(df, as.data.frame(bsw_matrix))
}


# ---- bsw_info ----------------------------------------------------------

#' Summarise bootstrap weight tables present in a PUMF DuckDB database
#'
#' Queries the DuckDB file backing a PUMF lazy table for bootstrap weight
#' tables created by [add_bootstrap_weights()] and returns a one-row-per-table
#' summary tibble.  Returns an empty tibble (invisibly) when no BSW tables are
#' found.
#'
#' @param tbl A lazy `dplyr::tbl()` returned by [get_pumf()] or by
#'   [add_bootstrap_weights()].
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{`weight_col`}{The weight column the BSW table was built from
#'       (matched back to the case used in the main survey table).}
#'     \item{`bsw_table`}{Name of the DuckDB table storing the weights.}
#'     \item{`view_name`}{Name of the DuckDB VIEW joining survey + BSW.}
#'     \item{`view_exists`}{Whether the companion VIEW is present.}
#'     \item{`n_replicates`}{Number of bootstrap replicate columns.}
#'     \item{`size_mb`}{Estimated table size in megabytes (from DuckDB
#'       metadata; `NA` when unavailable).}
#'   }
#'
#' @seealso [add_bootstrap_weights()], [remove_bootstrap_weights()]
#'
#' @examples
#' \dontrun{
#' sfs <- get_pumf("SFS", "2019")
#' sfs_bsw <- add_bootstrap_weights(sfs, weight_col = "WSTPWGT", seed = 1L)
#' bsw_info(sfs_bsw)
#' close_pumf(sfs_bsw)
#' }
#' @export
bsw_info <- function(tbl) {
  if (is.data.frame(tbl))
    stop("bsw_info() requires a DuckDB-backed lazy tbl from get_pumf(). ",
         "For in-memory data frames, inspect column names directly.",
         call. = FALSE)

  con <- tbl$src$con
  if (is.null(con) || !DBI::dbIsValid(con))
    stop("The connection backing 'tbl' is no longer valid.", call. = FALSE)

  prov <- .pumf_lookup_con(con)
  if (is.null(prov))
    stop("'tbl' has no pumf provenance. Was it created by get_pumf()?",
         call. = FALSE)

  series     <- prov$series
  version    <- prov$version
  cache_path <- prov$cache_path
  lang       <- prov$lang %||% "eng"
  table_name <- .pumf_table_name(series, version, lang)
  db_path    <- .pumf_db_path(series, version, cache_path)

  all_objs   <- DBI::dbListTables(con)
  bsw_tables <- sort(all_objs[grepl("^pumf_bsw", all_objs)])

  empty <- tibble::tibble(
    weight_col   = character(0L),
    bsw_table    = character(0L),
    view_name    = character(0L),
    view_exists  = logical(0L),
    n_replicates = integer(0L),
    size_mb      = numeric(0L)
  )

  if (length(bsw_tables) == 0L) {
    message("No bootstrap weight tables found in '", basename(db_path), "'.")
    return(invisible(empty))
  }

  # Estimated table sizes from DuckDB catalogue (bytes → MB).
  size_df <- tryCatch(
    DBI::dbGetQuery(con,
      "SELECT table_name, estimated_size FROM duckdb_tables()"),
    error = function(e)
      data.frame(table_name = character(0L), estimated_size = numeric(0L))
  )

  # Column names of the main survey table, for case-preserving weight_col lookup.
  main_cols <- tryCatch(DBI::dbListFields(con, table_name), error = function(e) character(0L))

  rows <- lapply(bsw_tables, function(bt) {
    # Derive weight_col and view name from table name convention.
    # "pumf_bsw_wstpwgt" → weight suffix "wstpwgt"; view "eng_bsw_wstpwgt".
    wc_lower <- sub("^pumf_bsw_?", "", bt)   # "" for legacy "pumf_bsw"
    vn       <- paste0(table_name, "_", sub("^pumf_", "", bt))

    # Restore proper case by matching against main table columns.
    wc_match <- main_cols[tolower(main_cols) == wc_lower]
    wc       <- if (length(wc_match) == 1L) wc_match else wc_lower

    cols  <- tryCatch(DBI::dbListFields(con, bt), error = function(e) character(0L))
    n_rep <- max(0L, as.integer(length(cols) - 1L))   # minus the ID column

    sz_row  <- size_df[size_df$table_name == bt, , drop = FALSE]
    size_mb <- if (nrow(sz_row) == 1L)
      round(sz_row$estimated_size[[1L]] / 1e6, 2)
    else
      NA_real_

    tibble::tibble(
      weight_col   = wc,
      bsw_table    = bt,
      view_name    = vn,
      view_exists  = vn %in% all_objs,
      n_replicates = n_rep,
      size_mb      = size_mb
    )
  })

  do.call(rbind, rows)
}


# ---- remove_bootstrap_weights ------------------------------------------

#' Remove bootstrap weight tables and views from a PUMF DuckDB database
#'
#' Drops the bootstrap weight table(s) created by [add_bootstrap_weights()]
#' and their companion VIEWs from the DuckDB file.  When all BSW tables have
#' been removed and the main survey table has a `pumf_row_id` column (added
#' automatically by [add_bootstrap_weights()] when no natural key was
#' available), that column is also dropped.
#'
#' Like [add_bootstrap_weights()], this function requires brief exclusive
#' write access: the read-only connection backing `tbl` is shut down, the
#' tables are dropped, and a fresh read-only connection is returned.
#'
#' @param tbl A lazy `dplyr::tbl()` returned by [get_pumf()] or by
#'   [add_bootstrap_weights()].
#' @param weight_col Name of the weight column whose BSW table should be
#'   removed (e.g. `"WSTPWGT"`).  If `NULL` (default), **all** bootstrap
#'   weight tables (and their companion VIEWs) are removed.
#'
#' @return A lazy `dplyr::tbl()` backed by the original physical survey table
#'   (without BSW columns), with a fresh read-only DuckDB connection.
#'
#' @seealso [add_bootstrap_weights()], [bsw_info()], [get_pumf()]
#'
#' @examples
#' \dontrun{
#' sfs <- get_pumf("SFS", "2019")
#' sfs_bsw <- add_bootstrap_weights(sfs, weight_col = "WSTPWGT", seed = 1L)
#' # Remove only the WSTPWGT BSW table
#' sfs_clean <- remove_bootstrap_weights(sfs_bsw, weight_col = "WSTPWGT")
#' close_pumf(sfs_clean)
#' }
#' @export
remove_bootstrap_weights <- function(tbl, weight_col = NULL) {
  if (is.data.frame(tbl))
    stop("remove_bootstrap_weights() requires a DuckDB-backed lazy tbl. ",
         "For in-memory data frames, drop BSW columns directly, e.g.: ",
         "df[, !grepl(\"^BSW[0-9]+$\", names(df))]",
         call. = FALSE)

  con <- tbl$src$con
  if (is.null(con) || !DBI::dbIsValid(con))
    stop("The connection backing 'tbl' is no longer valid.", call. = FALSE)

  prov <- .pumf_lookup_con(con)
  if (is.null(prov))
    stop("'tbl' has no pumf provenance. Was it created by get_pumf()?",
         call. = FALSE)

  series     <- prov$series
  version    <- prov$version
  cache_path <- prov$cache_path
  lang       <- prov$lang %||% "eng"
  table_name <- .pumf_table_name(series, version, lang)
  db_path    <- .pumf_db_path(series, version, cache_path)

  # Identify which BSW tables to remove.
  all_objs   <- DBI::dbListTables(con)
  bsw_tables <- all_objs[grepl("^pumf_bsw", all_objs)]

  if (!is.null(weight_col)) {
    target <- paste0("pumf_bsw_", tolower(weight_col))
    if (!target %in% bsw_tables)
      stop("No bootstrap weight table found for weight_col '", weight_col,
           "'. Use bsw_info() to see what is present.", call. = FALSE)
    bsw_tables <- target
  }

  if (length(bsw_tables) == 0L) {
    message("No bootstrap weight tables to remove from '", basename(db_path), "'.")
    return(tbl)
  }

  # Acquire exclusive write access (same pattern as add_bootstrap_weights).
  rm(list = intersect(format(con@conn_ref), ls(envir = .pumf_con_registry)),
     envir = .pumf_con_registry, inherits = FALSE)
  if (DBI::dbIsValid(con)) DBI::dbDisconnect(con, shutdown = TRUE)

  rw_con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)
  on.exit(
    if (DBI::dbIsValid(rw_con)) DBI::dbDisconnect(rw_con, shutdown = TRUE),
    add = TRUE
  )

  for (bt in bsw_tables) {
    vn <- paste0(table_name, "_", sub("^pumf_", "", bt))
    if (vn %in% DBI::dbListTables(rw_con)) {
      message("Dropping view '", vn, "'...")
      DBI::dbExecute(rw_con, sprintf('DROP VIEW IF EXISTS "%s"', vn))
    }
    message("Dropping bootstrap weight table '", bt, "'...")
    DBI::dbExecute(rw_con, sprintf('DROP TABLE IF EXISTS "%s"', bt))
  }

  # When no BSW tables remain, also remove pumf_row_id from the main table —
  # it was added only to serve as a BSW join key.
  remaining_bsw <- DBI::dbListTables(rw_con)
  remaining_bsw <- remaining_bsw[grepl("^pumf_bsw", remaining_bsw)]
  if (length(remaining_bsw) == 0L &&
      "pumf_row_id" %in% DBI::dbListFields(rw_con, table_name)) {
    message("Removing 'pumf_row_id' column from '", table_name, "'...")
    DBI::dbExecute(rw_con,
      sprintf('ALTER TABLE "%s" DROP COLUMN pumf_row_id', table_name))
  }

  DBI::dbDisconnect(rw_con, shutdown = TRUE)

  # Reopen read-only on the physical table (no BSW view).
  new_tbl <- pumf_open_duckdb(db_path, table_name, read_only = TRUE)
  .pumf_register_con(new_tbl$src$con, series, version, cache_path, lang)

  new_tbl
}


# ---- pumf_metadata ----------------------------------------------------------

#' Download and parse PUMF metadata without building a DuckDB table
#'
#' Runs Stage 1 (locate or download) and Stage 2 (parse metadata) and returns
#' the full bilingual canonical metadata.  Both `label_en` and `label_fr`
#' columns are always returned regardless of language.  This is useful for
#' inspecting variable definitions and code labels before loading data with
#' [get_pumf()].
#'
#' @param series Survey series acronym, e.g. `"SFS"`, `"LFS"`, `"Census"`.
#' @param version Version string, e.g. `"2019"`, `"2021 (individuals)"`.
#' @param cache_path Root cache directory.  Defaults to
#'   `getOption("canpumf.cache_path", tempdir())`.
#' @param refresh If `TRUE`, re-parse metadata from the already-extracted raw
#'   command files (does not re-download).
#' @param redownload If `TRUE`, delete the cached zip and extracted files and
#'   re-download from StatCan before re-parsing.  Implies `refresh = TRUE`.
#'
#' @return A named list with three elements:
#'   \describe{
#'     \item{`variables`}{Tibble with columns `name`, `label_en`, `label_fr`,
#'       `type`, `decimals`, `missing_low`, `missing_high`.}
#'     \item{`codes`}{Tibble with columns `name`, `val`, `label_en`,
#'       `label_fr`, mapping numeric codes to their labels.}
#'     \item{`layout`}{Tibble with columns `name`, `start`, `end` for
#'       fixed-width data files; `NULL` for CSV-format surveys.}
#'   }
#'
#' @seealso [get_pumf()], [pumf_var_labels()]
#'
#' @examples
#' \dontrun{
#' meta <- pumf_metadata("SFS", "2019")
#' meta$variables
#' meta$codes[meta$codes$name == "PEFAMID", ]
#' }
#' @export
pumf_metadata <- function(series,
                           version,
                           cache_path = getOption("canpumf.cache_path",
                                                   tempdir()),
                           refresh    = FALSE,
                           redownload = FALSE) {
  version     <- pumf_resolve_version(series, version)
  reg         <- pumf_registry_lookup(series, version)
  eff_refresh <- refresh || redownload
  version_dir <- pumf_locate_or_download(series, version,
                                          cache_path = cache_path,
                                          refresh    = eff_refresh,
                                          redownload = redownload)
  pumf_parse_metadata(version_dir,
                       layout_mask       = reg$layout_mask,
                       metadata_encoding = reg$metadata_encoding,
                       refresh           = eff_refresh)
  read_metadata(file.path(version_dir, "metadata"))
}
