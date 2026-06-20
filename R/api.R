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
#' @param registry Optional custom configuration created by
#'   [pumf_registry_entry()] (or [pumf_registry()]), used to parse and build a
#'   survey that is not in the built-in registry, or to override fields of one
#'   that is.  Applied only when a build actually happens — on an
#'   already-imported survey it has no effect unless `refresh = TRUE` is also
#'   passed (a message is emitted in that case).  Not supported for LFS.  For a
#'   survey not in [list_canpumf_collection()], deposit the raw files under
#'   `<cache_path>/<series>/<version>/` first (there is no download URL).
#' @param register_connection If `TRUE` (default), the DuckDB connection backing
#'   the returned tbl may appear in the RStudio Connections pane (subject to
#'   RStudio/duckdb settings).  Pass `FALSE` to suppress that registration —
#'   useful when opening and closing many connections programmatically (e.g.
#'   iterating over surveys in a notebook), where the pane would otherwise be
#'   spammed.  Defaults to `getOption("canpumf.register_connection", TRUE)`, so
#'   you can disable it globally with
#'   `options(canpumf.register_connection = FALSE)`.
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
                     registry   = NULL,
                     register_connection =
                       getOption("canpumf.register_connection", TRUE),
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

  if (!is.null(registry)) {
    if (!inherits(registry, "pumf_registry_entry"))
      stop("'registry' must be created by pumf_registry_entry() or ",
           "pumf_registry().", call. = FALSE)
    if (series == "LFS")
      stop("'registry' overrides are not supported for LFS, which uses a ",
           "dedicated pipeline.", call. = FALSE)
  }

  # Optionally keep the DuckDB connection out of the RStudio Connections pane.
  # duckdb registers the connection during dbConnect(), gated by these two
  # options; forcing them off for the duration of this call suppresses
  # registration on every connection opened here (non-LFS, LFS, and the
  # read-only re-open).  Useful when iterating over many surveys in a notebook,
  # where rapidly opening/closing connections would otherwise spam the pane.
  if (!isTRUE(register_connection)) {
    old_pane_opts <- options(
      duckdb.enable_rstudio_connection_pane = FALSE,
      duckdb.force_rstudio_connection_pane  = FALSE)
    on.exit(options(old_pane_opts), add = TRUE)
  }

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

  # Install the custom registry override for the duration of this call so every
  # internal pumf_registry_lookup() sees the merged configuration.  The build is
  # idempotent, so on an already-imported survey the override only takes effect
  # under refresh/redownload; otherwise warn that it is not applied.
  if (!is.null(registry)) {
    .pumf_registry_override_set(series, version, registry)
    on.exit(.pumf_registry_override_clear(series, version), add = TRUE)
    eff_rebuild <- isTRUE(refresh) || identical(refresh, "auto") ||
      isTRUE(redownload)
    if (!eff_rebuild &&
        .duckdb_table_exists(.pumf_db_path(series, version, cache_path),
                             .pumf_table_name(series, version, lang)))
      message("A built table for ", series, " ", version, " [", lang,
              "] already exists; the supplied 'registry' is not applied. ",
              "Pass refresh = TRUE to rebuild with it.")
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
.pumf_read_variables_from_prov <- function(prov) {
  series     <- prov$series
  version    <- prov$version
  cache_path <- prov$cache_path
  if (series == "LFS") {
    db_path <- file.path(cache_path, "LFS", "LFS.duckdb")
    if (!file.exists(db_path))
      stop("LFS database not found at '", db_path, "'.", call. = FALSE)
    # Reuse the registered connection to avoid opening a second DuckDB instance.
    # Opening a new connection and disconnecting with shutdown=TRUE would
    # invalidate the existing tbl connection for the same file.
    existing_con <- prov$con
    if (!is.null(existing_con) && DBI::dbIsValid(existing_con)) {
      all_versions <- if (DBI::dbExistsTable(existing_con, "lfs_versions"))
        DBI::dbGetQuery(
          existing_con,
          "SELECT version FROM lfs_versions ORDER BY survyear, survmnth")$version
      else character(0L)
    } else {
      con_tmp <- .duckdb_connect_quiet(db_path, read_only = TRUE)
      all_versions <- if (DBI::dbExistsTable(con_tmp, "lfs_versions"))
        DBI::dbGetQuery(
          con_tmp,
          "SELECT version FROM lfs_versions ORDER BY survyear, survmnth")$version
      else character(0L)
      DBI::dbDisconnect(con_tmp, shutdown = TRUE)
    }
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
    vars$name <- toupper(vars$name)
    vars
  }
}

.pumf_read_variables <- function(tbl) {
  prov <- .pumf_lookup_con(tbl$src$con)
  if (is.null(prov))
    stop("'tbl' has no pumf provenance. Was it created by get_pumf()?",
         call. = FALSE)
  .pumf_read_variables_from_prov(prov)
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
#' **Incremental re-runs (DuckDB path):** when a BSW table already exists the
#' call only does the work needed to satisfy the request:
#'   * **More replicates** than stored (and no new rows): the additional
#'     replicate columns are appended; existing columns are kept.
#'   * **New rows** in the main table (some rows have no weights yet): because a
#'     bootstrap replicate resamples the full population, added rows invalidate
#'     the existing weights of their resampling universe, so those weights are
#'     deleted and regenerated.  Unstratified, this regenerates every row; when
#'     `strata_cols` are in effect, only the strata that gained rows are
#'     regenerated and complete strata keep their existing weights.
#'   * **Neither:** the stored weights are reused without recomputation.
#' Pass `overwrite = TRUE` to force a full fresh regeneration regardless.
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
#'   e.g. `"PWEIGHT"`).
#' @param id_col Optional name of a column that uniquely identifies each row
#'   (DuckDB path only).  If `NULL` (default), the registry `bsw_join_key` is
#'   used when available; otherwise `pumf_row_id` is added to the main table.
#' @param strata_cols Optional character vector of column names to stratify on.
#'   Resampling is performed independently within each unique combination of
#'   stratum values, preserving stratum sample sizes across replicates.  For
#'   LFS, defaults to `c("SURVYEAR", "SURVMNTH")` so each month is resampled
#'   separately.  For other surveys, use the registry `bsw_strata` field or
#'   pass explicitly (e.g. province, age group).  Pass `character(0)` to
#'   suppress the LFS default and generate unstratified weights.
#' @param n_replicates Number of bootstrap replicates to generate (default
#'   `500L`).
#' @param prefix Column-name prefix for replicate columns (default `"CPBSW"`).
#'   Columns are named `prefix1`, `prefix2`, …
#' @param bsw_table Name of the DuckDB table that stores the replicate weights
#'   (DuckDB path only).  Defaults to `NULL`, which auto-names it
#'   `paste0("pumf_bsw_", tolower(weight_col))` so separate calls with
#'   different weight columns do not overwrite each other.
#' @param seed Optional integer seed for reproducibility.
#' @param overwrite If the `bsw_table` already exists in the DuckDB file,
#'   regenerate and overwrite it when `TRUE`.  When `FALSE` (default) the
#'   existing table is reused silently — no computation is performed.
#'
#' @return
#'   * **DuckDB path:** a lazy `dplyr::tbl()` backed by a persistent DuckDB
#'     VIEW that contains all original survey columns plus the `n_replicates`
#'     bootstrap weight columns, with any input `filter()` operations re-applied.
#'   * **In-memory path:** the input `data.frame` / `tibble` with bootstrap
#'     weight columns appended so that `n_replicates` replicates are present.
#'     If the input already carries replicate columns for `prefix`, only the
#'     additional ones are generated (existing columns are preserved); when it
#'     already has at least `n_replicates`, the data frame is returned unchanged.
#'
#' @seealso [bsw_info()], [remove_bootstrap_weights()], [get_pumf()]
#'
#' @examples
#' \dontrun{
#' sfs <- get_pumf("SFS", "2019")
#' sfs_bsw <- add_bootstrap_weights(sfs, weight_col = "PWEIGHT",
#'                                  n_replicates = 200L, seed = 42L)
#' bsw_info(sfs_bsw)
#' close_pumf(sfs_bsw)
#' }
#' @export
add_bootstrap_weights <- function(tbl,
                                       weight_col,
                                       id_col       = NULL,
                                       strata_cols  = NULL,
                                       n_replicates = 500L,
                                       prefix       = "CPBSW",
                                       bsw_table    = NULL,
                                       seed         = NULL,
                                       overwrite    = FALSE) {

  stopifnot(is.character(weight_col), length(weight_col) == 1L)
  stopifnot(is.numeric(n_replicates), n_replicates >= 1L)
  n_replicates <- as.integer(n_replicates)

  # Auto-name BSW table after weight_col. If weight_col is a human-readable
  # label (resolved later), store the raw value for naming; the resolution
  # happens below after we know if this is DuckDB-backed or in-memory.
  bsw_table_auto <- is.null(bsw_table)

  # ---- Dispatch: in-memory (data.frame / tibble) ----------------------------
  if (is.data.frame(tbl)) {
    weight_col <- .bsw_resolve_col_df(tbl, weight_col, "weight_col")
    eff_strata_df <- if (identical(strata_cols, character(0L))) NULL else strata_cols
    if (!is.null(eff_strata_df)) {
      bad <- setdiff(eff_strata_df, names(tbl))
      if (length(bad) > 0L)
        stop("strata_cols not found in data frame: ", paste(bad, collapse = ", "),
             call. = FALSE)
    }
    return(.add_bsw_inmemory(tbl, weight_col, n_replicates, prefix, seed,
                              eff_strata_df))
  }

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

  table_name <- .pumf_table_name(series, version, lang)
  db_path    <- .pumf_db_path(series, version, cache_path)

  # Resolve weight_col / id_col: if label_pumf_columns() was called, the user
  # may pass a human-readable label (e.g. "Person weight") rather than the
  # coded column name (e.g. "PWEIGHT"). Translate back to the coded name so
  # SQL queries against the raw DuckDB table work correctly.
  weight_col <- .bsw_resolve_col_prov(con, table_name, weight_col, "weight_col", prov)
  if (!is.null(id_col))
    id_col <- .bsw_resolve_col_prov(con, table_name, id_col, "id_col", prov)

  # Detect whether the input tbl has had label_pumf_columns() applied so we
  # can re-apply it to the returned tbl.  The check: any column in the input
  # that is not a generated BSW replicate column and is not in the physical
  # table's coded column list must be a label alias.
  # Replicate columns are identified from the `prefix` used for this call
  # (default "CPBSW") rather than a hardcoded "*BSW" pattern, so detection stays
  # correct for custom prefixes.  startsWith + digit-suffix avoids having to
  # regex-escape a user-supplied prefix.
  physical_cols <- DBI::dbListFields(con, table_name)
  in_cols       <- colnames(tbl)
  is_bsw_rep    <- startsWith(in_cols, prefix) &
    grepl("^[0-9]+$", substring(in_cols, nchar(prefix) + 1L))
  survey_input_cols <- in_cols[!is_bsw_rep]
  input_was_labeled <- !all(survey_input_cols %in% physical_cols)

  if (bsw_table_auto)
    bsw_table <- paste0("pumf_bsw_", tolower(weight_col))

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
  # VIEW name: "pumf_bsw_pweight" → "eng_bsw_pweight"; preserves weight_col scope.
  view_name  <- paste0(table_name, "_", sub("^pumf_", "", bsw_table))

  # --- Determine row-identifier column (needed for all paths) ---------------
  reg <- pumf_registry_lookup(series, version)
  if (is.null(id_col)) {
    if (!is.null(reg$bsw_join_key) && length(reg$bsw_join_key) == 1L)
      id_col <- reg$bsw_join_key
  }

  # --- Resolve effective strata: explicit > registry > LFS default > none ---
  # character(0) explicitly suppresses the LFS default.
  eff_strata <- if (identical(strata_cols, character(0L))) {
    NULL
  } else {
    strata_cols %||% reg$bsw_strata %||%
      if (series == "LFS") c("SURVYEAR", "SURVMNTH") else NULL
  }
  if (!is.null(eff_strata)) {
    avail_cols <- DBI::dbListFields(con, table_name)
    bad_sc <- setdiff(eff_strata, avail_cols)
    if (length(bad_sc) > 0L)
      stop("strata_cols not found in table '", table_name, "': ",
           paste(bad_sc, collapse = ", "), call. = FALSE)
  }

  # --- Inspect existing BSW table (if present and not overwriting) ----------
  bsw_exists <- !overwrite && DBI::dbExistsTable(con, bsw_table)

  if (bsw_exists) {
    bsw_all_cols  <- DBI::dbListFields(con, bsw_table)
    rep_pat       <- paste0("^", prefix, "[0-9]+$")
    rep_cols_all  <- bsw_all_cols[grepl(rep_pat, bsw_all_cols)]
    # Sort numerically (BSW10 > BSW9, not lexicographically)
    rep_cols_all  <- rep_cols_all[order(
      as.integer(sub(paste0("^", prefix), "", rep_cols_all)))]
    n_existing    <- length(rep_cols_all)
    # The non-replicate column is the id join key used when BSW was created.
    id_col_bsw    <- bsw_all_cols[!grepl(rep_pat, bsw_all_cols)]
    id_col_bsw    <- if (length(id_col_bsw) == 1L) id_col_bsw[1L] else
                     (id_col %||% "pumf_row_id")
    # If the caller did not specify id_col, inherit it from the stored BSW table.
    if (is.null(id_col)) id_col <- id_col_bsw

    n_bsw_rows  <- DBI::dbGetQuery(
      con, sprintf('SELECT COUNT(*) AS n FROM "%s"', bsw_table))$n
    n_main_rows <- DBI::dbGetQuery(
      con, sprintf('SELECT COUNT(*) AS n FROM "%s"', table_name))$n

    need_more_rows <- n_bsw_rows  <  n_main_rows
    need_more_cols <- n_replicates > n_existing

    # ---- Case A: enough replicates, all rows present — no write needed ------
    # Return a SQL JOIN on the existing read-only connection; no disconnection.
    if (!need_more_rows && !need_more_cols) {
      cols_to_use <- rep_cols_all[seq_len(n_replicates)]
      bsw_sel     <- paste0('"b"."', cols_to_use, '"', collapse = ", ")
      join_sql    <- sprintf(
        'SELECT "m".*, %s FROM "%s" "m" JOIN "%s" "b" ON "m"."%s" = "b"."%s"',
        bsw_sel, table_name, bsw_table, id_col_bsw, id_col_bsw
      )
      full_sql <- if (!is.null(where_clause))
        sprintf('SELECT * FROM (%s) "_t"\n%s', join_sql, where_clause)
      else
        join_sql
      # con is already registered in .pumf_con_registry; no re-registration.
      result_tbl <- dplyr::tbl(con, dplyr::sql(full_sql))
      if (input_was_labeled) result_tbl <- label_pumf_columns(result_tbl)
      return(result_tbl)
    }
  } else {
    n_existing    <- 0L
    rep_cols_all  <- character(0L)
    id_col_bsw    <- id_col %||% "pumf_row_id"
    need_more_rows <- FALSE
    need_more_cols <- FALSE
  }

  # ---- Cases B/C/D: write connection needed --------------------------------
  # B = new rows only; C = more columns only; D = full fresh generation.
  # Pull the data we need from the read-only connection before closing it.

  id_sql <- if (!is.null(id_col))
    sprintf('"%s" AS row_id', id_col)
  else
    "rowid AS row_id"

  strata_sql <- if (!is.null(eff_strata))
    paste0(", ", paste0('"', eff_strata, '"', collapse = ", "))
  else
    ""

  # Pull ALL rows' weights whenever we (re)generate: fresh build, added columns,
  # or added rows.  The former "pull only the new rows" shortcut is gone:
  # bootstrap replicates resample the full (stratum) population, so added rows
  # invalidate — and require regenerating — every row in the affected resampling
  # universe, not just the new rows themselves.
  if (!bsw_exists || need_more_cols || need_more_rows) {
    wt_data_all <- DBI::dbGetQuery(con, sprintf(
      'SELECT %s, CAST("%s" AS DOUBLE) AS w%s FROM "%s"',
      id_sql, weight_col, strata_sql, table_name
    ))
  }
  if (bsw_exists && (need_more_rows || need_more_cols)) {
    # Read the current BSW table to preserve the still-valid replicate weights.
    old_bsw <- DBI::dbGetQuery(con, sprintf('SELECT * FROM "%s"', bsw_table))
  }

  # ---- Generate bootstrap weights ------------------------------------------
  # .gen_bsw generates replicates for a single (possibly filtered) wt_df.
  # Stratification is handled by the caller splitting wt_df by stratum.
  # seed_val: when NULL the caller is responsible for set.seed() before calling.
  .gen_bsw <- function(wt_df, n_cols_start, n_cols_end, seed_val,
                        show_progress = TRUE) {
    n <- nrow(wt_df)
    w <- wt_df$w
    if (anyNA(w)) { w[is.na(w)] <- 0 }
    n_new <- n_cols_end - n_cols_start
    if (n_new <= 0L) return(NULL)
    mem_gb <- n_new * (n / 1e9) * 8
    if (mem_gb > 2)
      warning(sprintf(
        "Generating %d replicates for %d rows requires ~%.1f GB of memory.",
        n_new, n, mem_gb), call. = FALSE)
    if (!is.null(seed_val)) set.seed(seed_val)
    # Progress: report at ~10 evenly-spaced checkpoints.
    report_at <- if (show_progress && n_new >= 10L)
      unique(round(seq(n_new / 10, n_new, length.out = 10L)))
    else
      integer(0L)
    counts <- matrix(0L, nrow = n, ncol = n_new)
    for (i in seq_len(n_new)) {
      counts[, i] <- tabulate(sample.int(n, n, replace = TRUE), nbins = n)
      if (i %in% report_at)
        message(sprintf("  Replicate %d / %d ...",
                        i + n_cols_start, n_cols_end))
    }
    mat <- w * counts
    colnames(mat) <- paste0(prefix, seq(n_cols_start + 1L, n_cols_end))
    cbind(stats::setNames(data.frame(wt_df[[1L]]), id_col %||% "pumf_row_id"),
          as.data.frame(mat))
  }

  # ---- Determine what to generate and build (or stream) the BSW data -------
  #
  # Case D + strata: write strata one at a time directly to DuckDB after
  # opening the write connection, so the full n × n_replicates matrix is never
  # materialised in memory.  The wt_data_all pulled above stays in memory but
  # each per-stratum chunk_df is freed after writing.
  # All other cases (Cases B/C or unstratified Case D) build bsw_df in memory
  # as before.

  use_strata_stream <- !bsw_exists && !is.null(eff_strata)

  if (!use_strata_stream) {
    if (!bsw_exists) {
      # Case D (unstratified): fresh generation for all rows.
      if (anyNA(wt_data_all$w)) {
        warning(sum(is.na(wt_data_all$w)), " NA weight(s) in '", weight_col,
                "' replaced with 0.", call. = FALSE)
        wt_data_all$w[is.na(wt_data_all$w)] <- 0
      }
      message(sprintf(
        "Generating %d %s replicates for %d observations...",
        n_replicates, prefix, nrow(wt_data_all)))
      bsw_df <- .gen_bsw(wt_data_all, 0L, n_replicates, seed)
    } else {
      # bsw_exists, with added rows and/or added replicate columns.
      #
      # Bootstrap replicate weights are produced by resampling the full
      # population (or, when stratified, the full stratum).  Therefore:
      #   * Added ROWS invalidate the replicate weights of their resampling
      #     universe and force regeneration there — the whole table when
      #     unstratified, or just the strata that gained rows when stratified
      #     (complete strata keep their existing weights).
      #   * Added COLUMNS are independent extra replicates appended to rows
      #     whose resampling universe is unchanged.
      n_target <- max(n_existing, n_replicates)
      id_name  <- id_col %||% "pumf_row_id"
      canon    <- c(id_name, paste0(prefix, seq_len(n_target)))

      if (anyNA(wt_data_all$w)) {
        warning(sum(is.na(wt_data_all$w)), " NA weight(s) in '", weight_col,
                "' replaced with 0.", call. = FALSE)
        wt_data_all$w[is.na(wt_data_all$w)] <- 0
      }
      has_bsw <- wt_data_all$row_id %in% old_bsw[[id_name]]
      n_new   <- sum(!has_bsw)

      if (is.null(eff_strata)) {
        if (need_more_rows) {
          # Whole population changed: every replicate weight is stale.
          message(sprintf(
            "%d new row(s) detected; deleting and regenerating all %d %s replicates for %d observations...",
            n_new, n_target, prefix, nrow(wt_data_all)))
          bsw_df <- .gen_bsw(wt_data_all, 0L, n_target, seed)[, canon, drop = FALSE]
        } else {
          # Case C: population unchanged, only add independent replicates.
          message(sprintf("Adding replicates %d-%d to BSW table...",
                          n_existing + 1L, n_target))
          add_df <- .gen_bsw(wt_data_all, n_existing, n_target, seed)
          bsw_df <- merge(old_bsw, add_df,
                          by = intersect(names(old_bsw), names(add_df)),
                          all = TRUE)[, canon, drop = FALSE]
        }
      } else {
        # Stratified: regenerate only the strata that have missing weights.
        strata_key  <- interaction(wt_data_all[eff_strata], drop = TRUE)
        affected    <- if (need_more_rows)
          unique(as.character(strata_key[!has_bsw])) else character(0L)
        is_affected <- as.character(strata_key) %in% affected

        if (!is.null(seed)) set.seed(seed)
        parts <- list()

        # 1) Affected strata: full fresh resample within each (n_target cols).
        if (any(is_affected)) {
          message(sprintf(
            "%d new row(s) in %d of %d strata; deleting and regenerating those strata in full (%d %s replicates)...",
            n_new, length(affected), nlevels(strata_key), n_target, prefix))
          aff_dat <- wt_data_all[is_affected, , drop = FALSE]
          aff_key <- droplevels(strata_key[is_affected])
          for (lv in levels(aff_key)) {
            s_data <- aff_dat[aff_key == lv, , drop = FALSE]
            parts[[length(parts) + 1L]] <-
              .gen_bsw(s_data, 0L, n_target, NULL,
                       show_progress = FALSE)[, canon, drop = FALSE]
          }
        }

        # 2) Unaffected strata: keep existing weights; add columns if requested,
        #    resampling within each stratum (independent extra replicates).
        if (any(!is_affected)) {
          unaff_dat <- wt_data_all[!is_affected, , drop = FALSE]
          keep_bsw  <- old_bsw[old_bsw[[id_name]] %in% unaff_dat$row_id, ,
                               drop = FALSE]
          if (n_target > n_existing) {
            unaff_key <- droplevels(strata_key[!is_affected])
            message(sprintf(
              "Adding replicates %d-%d to %d unaffected stratum/strata...",
              n_existing + 1L, n_target, nlevels(unaff_key)))
            add_parts <- list()
            for (lv in levels(unaff_key)) {
              s_data <- unaff_dat[unaff_key == lv, , drop = FALSE]
              add_parts[[length(add_parts) + 1L]] <-
                .gen_bsw(s_data, n_existing, n_target, NULL, show_progress = FALSE)
            }
            add_df   <- do.call(rbind, add_parts)
            keep_bsw <- merge(keep_bsw, add_df,
                              by = intersect(names(keep_bsw), names(add_df)),
                              all = TRUE)
          }
          parts[[length(parts) + 1L]] <- keep_bsw[, canon, drop = FALSE]
        }

        bsw_df <- do.call(rbind, parts)
      }
    }
  }

  id_col_final <- if (use_strata_stream) (id_col %||% "pumf_row_id")
                  else names(bsw_df)[1L]

  # --- Acquire exclusive write access ----------------------------------------
  rm(list = intersect(format(con@conn_ref), ls(envir = .pumf_con_registry)),
     envir = .pumf_con_registry, inherits = FALSE)
  if (DBI::dbIsValid(con))
    DBI::dbDisconnect(con, shutdown = TRUE)

  # --- Write BSW table and VIEW to DuckDB ------------------------------------
  rw_con <- .duckdb_connect_quiet(db_path, read_only = FALSE)
  on.exit(
    if (DBI::dbIsValid(rw_con)) DBI::dbDisconnect(rw_con, shutdown = TRUE),
    add = TRUE
  )

  # Add pumf_row_id to the main table when no natural key was available.
  if (is.null(id_col)) {
    if (!"pumf_row_id" %in% DBI::dbListFields(rw_con, table_name)) {
      message("Adding 'pumf_row_id' column to '", table_name, "'...")
      DBI::dbExecute(rw_con,
        sprintf('ALTER TABLE "%s" ADD COLUMN pumf_row_id BIGINT', table_name))
      DBI::dbExecute(rw_con,
        sprintf('UPDATE "%s" SET pumf_row_id = rowid', table_name))
    }
    id_col       <- "pumf_row_id"
    id_col_final <- "pumf_row_id"
  }

  if (use_strata_stream) {
    # Case D + strata: split wt_data_all by stratum, generate and write chunk
    # by chunk so peak memory = one stratum's BSW matrix (not the full table).
    # Warn once on NA weights before splitting (mirrors the non-stratified path)
    # so the per-stratum zeroing in .gen_bsw is not silent.
    if (anyNA(wt_data_all$w)) {
      warning(sum(is.na(wt_data_all$w)), " NA weight(s) in '", weight_col,
              "' replaced with 0.", call. = FALSE)
      wt_data_all$w[is.na(wt_data_all$w)] <- 0
    }
    strata_key  <- interaction(wt_data_all[eff_strata], drop = TRUE)
    strata_lvls <- levels(strata_key)
    n_st        <- length(strata_lvls)
    message(sprintf(
      "Generating %d %s replicates across %d %s strata (%d total obs)...",
      n_replicates, prefix, n_st,
      paste(eff_strata, collapse = "/"), nrow(wt_data_all)))
    if (!is.null(seed)) set.seed(seed)
    for (si in seq_along(strata_lvls)) {
      idx    <- which(strata_key == strata_lvls[si])
      s_data <- wt_data_all[idx, , drop = FALSE]
      sv_str <- paste(eff_strata,
                      as.character(unlist(s_data[1L, eff_strata, drop = FALSE])),
                      sep = "=", collapse = ", ")
      message(sprintf("  Stratum [%d/%d] %s (%d obs)",
                      si, n_st, sv_str, nrow(s_data)))
      chunk_df <- .gen_bsw(s_data, 0L, n_replicates,
                            seed_val = NULL, show_progress = FALSE)
      DBI::dbWriteTable(rw_con, bsw_table, chunk_df,
                        overwrite = (si == 1L), append = (si > 1L))
      rm(chunk_df)
    }
    rm(wt_data_all)
  } else {
    message("Writing bootstrap weight table '", bsw_table, "' to DuckDB...")
    DBI::dbWriteTable(rw_con, bsw_table, bsw_df, overwrite = TRUE)
  }

  DBI::dbExecute(rw_con, sprintf(
    'CREATE INDEX IF NOT EXISTS "idx_%s" ON "%s" ("%s")',
    bsw_table, bsw_table, id_col_final
  ))

  # Expose the full set of replicate columns that ended up in the BSW table.
  final_rep_cols <- setdiff(DBI::dbListFields(rw_con, bsw_table), id_col_final)
  bsw_col_sql    <- paste0('"b"."', final_rep_cols, '"', collapse = ", ")
  DBI::dbExecute(rw_con, sprintf(
    'CREATE OR REPLACE VIEW "%s" AS SELECT "m".*, %s FROM "%s" "m" JOIN "%s" "b" ON "m"."%s" = "b"."%s"',
    view_name, bsw_col_sql, table_name, bsw_table, id_col_final, id_col_final
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

  if (input_was_labeled) new_tbl <- label_pumf_columns(new_tbl)
  new_tbl
}


# Resolve a column name that may be a human-readable label back to the coded
# column name. For data.frame input: checks if the name is in colnames(df);
# if not, it must already be the correct name (no provenance available).
.bsw_resolve_col_df <- function(df, col, arg_name) {
  if (is.null(col) || col %in% names(df)) return(col)
  stop("'", arg_name, "' column '", col, "' not found in the data frame.",
       call. = FALSE)
}

# Resolve a column name that may be a human-readable label back to the coded
# column name for DuckDB-backed tables.  Looks up the label in variables.csv
# using the survey provenance stored in the connection registry.
.bsw_resolve_col_prov <- function(con, table_name, col, arg_name, prov) {
  if (is.null(col)) return(col)
  actual_cols <- DBI::dbListFields(con, table_name)
  if (col %in% actual_cols) return(col)
  # col is not a raw column name — try to find it as a human-readable label.
  variables  <- .pumf_read_variables_from_prov(prov)
  lang       <- prov$lang %||% "eng"
  label_col  <- if (lang == "eng") "label_en" else "label_fr"
  match_rows <- variables[!is.na(variables[[label_col]]) &
                            variables[[label_col]] == col, , drop = FALSE]
  if (nrow(match_rows) == 0L)
    stop("'", arg_name, "' value '", col,
         "' is neither a column in the DuckDB table nor a known variable label.",
         call. = FALSE)
  match_rows$name[1L]
}

# Fast in-memory bootstrap weight generation for data.frame / tibble input.
.add_bsw_inmemory <- function(df, weight_col, n_replicates, prefix, seed,
                               strata_cols = NULL) {
  n <- nrow(df)

  # Detect replicate columns already present for THIS prefix so a second call
  # extends the set instead of regenerating and duplicating column names
  # (mirrors the DuckDB-backed Cases A/C in add_bootstrap_weights()).
  rep_pat      <- paste0("^", prefix, "[0-9]+$")
  existing_rep <- grep(rep_pat, names(df), value = TRUE)
  existing_rep <- existing_rep[order(
    as.integer(sub(paste0("^", prefix), "", existing_rep)))]
  n_existing   <- length(existing_rep)

  # Case A: enough replicates already present — reuse silently, no regeneration.
  if (n_existing >= n_replicates) {
    message(sprintf(
      "Data frame already has %d '%s' replicate(s) (≥ %d requested); reusing.",
      n_existing, prefix, n_replicates))
    return(df)
  }

  # Case C: generate only the missing replicates (n_existing+1 .. n_replicates).
  n_new <- n_replicates - n_existing

  w <- df[[weight_col]]
  if (!is.numeric(w)) w <- suppressWarnings(as.numeric(w))
  if (anyNA(w)) {
    warning(sum(is.na(w)), " NA weight(s) in '", weight_col,
            "' replaced with 0.", call. = FALSE)
    w[is.na(w)] <- 0
  }
  if (n_existing > 0L)
    message(sprintf("Adding replicates %d-%d (data frame already has %d)...",
                    n_existing + 1L, n_replicates, n_existing))

  if (!is.null(seed)) set.seed(seed)
  report_at <- if (n_new >= 10L)
    unique(round(seq(n_new / 10, n_new, length.out = 10L)))
  else
    integer(0L)
  counts <- matrix(0L, nrow = n, ncol = n_new)
  if (!is.null(strata_cols)) {
    strata_key  <- interaction(df[strata_cols], drop = TRUE)
    strata_lvls <- levels(strata_key)
    for (i in seq_len(n_new)) {
      ct <- integer(n)
      for (lv in strata_lvls) {
        idx <- which(strata_key == lv)
        ct <- ct + tabulate(sample(idx, length(idx), replace = TRUE), nbins = n)
      }
      counts[, i] <- ct
      if (i %in% report_at)
        message(sprintf("  Replicate %d / %d ...", i + n_existing, n_replicates))
    }
  } else {
    for (i in seq_len(n_new)) {
      counts[, i] <- tabulate(sample.int(n, n, replace = TRUE), nbins = n)
      if (i %in% report_at)
        message(sprintf("  Replicate %d / %d ...", i + n_existing, n_replicates))
    }
  }
  bsw_matrix <- w * counts
  colnames(bsw_matrix) <- paste0(prefix, seq(n_existing + 1L, n_replicates))
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
#' sfs_bsw <- add_bootstrap_weights(sfs, weight_col = "PWEIGHT", seed = 1L)
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
#'   removed (e.g. `"PWEIGHT"`).  If `NULL` (default), **all** bootstrap
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
#' sfs_bsw <- add_bootstrap_weights(sfs, weight_col = "PWEIGHT", seed = 1L)
#' # Remove only the PWEIGHT BSW table
#' sfs_clean <- remove_bootstrap_weights(sfs_bsw, weight_col = "PWEIGHT")
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

  rw_con <- .duckdb_connect_quiet(db_path, read_only = FALSE)
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
#' @param registry Optional custom configuration created by
#'   [pumf_registry_entry()] (or [pumf_registry()]) to drive metadata parsing
#'   for a survey not in the built-in registry, or to override fields of one
#'   that is.  Not supported for LFS.
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
                           redownload = FALSE,
                           registry   = NULL) {
  version     <- pumf_resolve_version(series, version)
  if (!is.null(registry)) {
    if (!inherits(registry, "pumf_registry_entry"))
      stop("'registry' must be created by pumf_registry_entry() or ",
           "pumf_registry().", call. = FALSE)
    if (series == "LFS")
      stop("'registry' overrides are not supported for LFS.", call. = FALSE)
    .pumf_registry_override_set(series, version, registry)
    on.exit(.pumf_registry_override_clear(series, version), add = TRUE)
  }
  reg         <- pumf_registry_lookup(series, version)
  eff_refresh <- refresh || redownload
  version_dir <- pumf_locate_or_download(series, version,
                                          cache_path = cache_path,
                                          refresh    = eff_refresh,
                                          redownload = redownload)
  # Parsing is idempotent: with metadata already present and no refresh, a
  # supplied registry has no effect.  This message lives only here (get_pumf()
  # parses via pumf_parse_metadata() directly, not pumf_metadata()), so it is
  # never emitted twice for a get_pumf() call.
  if (!is.null(registry) && !eff_refresh && metadata_exists(version_dir))
    message("Metadata for ", series, " ", version, " is already parsed; ",
            "the supplied 'registry' is not applied. ",
            "Pass refresh = TRUE to re-parse with it.")
  pumf_parse_metadata(version_dir,
                       layout_mask       = reg$layout_mask,
                       metadata_encoding = reg$metadata_encoding,
                       refresh           = eff_refresh)
  read_metadata(file.path(version_dir, "metadata"))
}
