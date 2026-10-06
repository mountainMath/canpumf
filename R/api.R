# R/api.R -- Public entry points for the canpumf package.
#
# get_pumf()            -- download, parse, label, and return a lazy DuckDB tbl
# label_pumf_columns()  -- rename tbl columns to human-readable variable labels
# pumf_metadata()       -- download and parse metadata only; return canonical list

# ---- Connection provenance registry -----------------------------------------
#
# DBI connections are S4 objects wrapping a C++ external pointer (Xptr).
# Assigning one to a new variable (e.g. con <- tbl$src$con) gives an R-level
# copy of the S4 wrapper, but the Xptr inside always points to the same C++
# object.  format(Xptr) returns the C++ address -- a stable, unique key that
# survives S4 copies and dplyr tbl transformations.
#
# We use this as a key into a package-level environment so that provenance
# (series, version, cache_path, lang) set in get_pumf() can be retrieved in
# label_pumf_columns() even after the user has applied additional dplyr
# operations to the tbl.

.pumf_con_registry <- new.env(hash = TRUE, parent = emptyenv())

.pumf_register_con <- function(con, series, version, cache_path, lang,
                               module = NULL) {
  key <- format(con@conn_ref)
  .pumf_con_registry[[key]] <- list(
    con        = con,
    series     = series,
    version    = version,
    cache_path = cache_path,
    lang       = lang,
    module     = module
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
#' The Labour Force Survey is treated specially: all versions share a single
#' database, so the full history can be queried as one table.  Pass
#' `version = "YYYY"` (annual) or `"YYYY-MM"` (monthly).  `"LFS"` covers
#' 2006 onwards, from Statistics Canada.  `"LFS_HIST"` covers January 1976 to
#' December 2005 in the legacy (pre-2017) layout, from the Borealis Dataverse.
#' It is released monthly only, so a `"YYYY"` version loads that year's twelve
#' months, and its labels are harmonised across months.
#' `refresh = "auto"` loads every available version that is not yet in the
#' database; this is only valid for `"LFS"` and `"LFS_HIST"`.
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
#'   files (does not re-download).  `"auto"` is accepted for `"LFS"` and
#'   `"LFS_HIST"` only and loads all available versions not yet in the
#'   database.
#' @param redownload If `TRUE`, delete the cached zip and extracted files and
#'   re-download from StatCan before rebuilding.  Implies `refresh = TRUE`.
#'   Not valid with `refresh = "auto"`.
#' @param read_only Open the DuckDB connection in read-only mode (default
#'   `TRUE`).  Pass `FALSE` to allow write access, e.g. to store the weights
#'   of [add_bootstrap_weights()] or custom views and derived tables in the
#'   DuckDB file.  A write connection needs the file to itself, so use
#'   [close_pumf()] to release it when done.
#' @param registry Optional custom configuration created by
#'   [pumf_registry_entry()] (or [pumf_registry()]), used to parse and build a
#'   survey that is not in the built-in registry, or to override fields of one
#'   that is.  Applied only when a build actually happens -- on an
#'   already-imported survey it has no effect unless `refresh = TRUE` is also
#'   passed (a message is emitted in that case).  Not supported for LFS.  For a
#'   survey not in [list_canpumf_collection()], deposit the raw files under
#'   `<cache_path>/<series>/<version>/` first (there is no download URL).
#' @param borealis Load the data from the [Borealis](https://borealisdata.ca)
#'   Dataverse instead of Statistics Canada: a dataset DOI (e.g.
#'   `"doi:10.5683/SP3/LG7WKC"`) or a one-row tibble from
#'   [list_borealis_pumf_catalogue()]. canpumf downloads the dataset's CSV data
#'   file, its command files and documentation (see
#'   [list_borealis_pumf_files()]). Without this argument Borealis is used
#'   automatically only for versions StatCan does not post, such as the
#'   1971--1986 Census PUMFs. When `version` names a registered survey, its
#'   built-in configuration is replaced by auto-detection unless the registry
#'   entry points at the same DOI; combine with `registry` to supply fixups.
#'   A version already cached from another source is not replaced unless
#'   `redownload = TRUE`. Not supported for LFS.
#' @param module For multi-module surveys (several linked files in one DuckDB,
#'   e.g. GSS cycle 16 / "Aging and Social Support" 2002, whose `MAIN`, `CG4`,
#'   `CG6` and `CR` files join on `RECID`), selects which module table to
#'   return.  `NULL` (default) returns the survey's primary module; for a
#'   multi-module survey a one-time message then lists the sibling modules and
#'   shows how to open one.  Use [pumf_module()] to open a sibling module on the
#'   *same* connection so the two tbls are joinable.  Not supported for LFS.
#' @param register_connection If `TRUE` (default), the DuckDB connection backing
#'   the returned tbl may appear in the RStudio Connections pane (subject to
#'   RStudio/duckdb settings).  Pass `FALSE` to suppress that registration --
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
#'   Returns `invisible(NULL)` with an informative message if the data must be
#'   downloaded but Statistics Canada is unreachable.
#'
#'   A database built by canpumf before 0.6.1 carries no build stamp, no
#'   `pumf_row_id` key and no sentinel companion.  `get_pumf()` says so once
#'   per session when it opens one; rebuild with `refresh = TRUE`, or silence
#'   the message with `options(canpumf.stale_cache_message = FALSE)`.
#'   [list_pumf_cache()] reports the building version of every database.
#'
#' @seealso [label_pumf_columns()], [pumf_var_labels()], [pumf_metadata()],
#'   [close_pumf()], [list_canpumf_collection()], [list_pumf_cache()]
#'
#' @examples
#' \donttest{
#' # Download and open the SFS 2019 as a lazy DuckDB table.
#' # get_pumf() returns NULL if Statistics Canada is unreachable.
#' sfs <- get_pumf("SFS", "2019")
#' if (!is.null(sfs)) {
#'   dplyr::glimpse(sfs)
#'
#'   # Collect a local tibble (here, the first 100 records)
#'   sfs_local <- sfs |>
#'     head(100) |>
#'     dplyr::collect()
#'
#'   # Release the connection when done
#'   close_pumf(sfs)
#' }
#'
#' # French labels (opened and released on its own connection)
#' sfs_fr <- get_pumf("SFS", "2019", lang = "fra")
#' if (!is.null(sfs_fr)) close_pumf(sfs_fr)
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
                     module     = NULL,
                     borealis   = NULL,
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
  version <- pumf_resolve_version(series, version, cache_path)
  stopifnot(lang %in% c("eng", "fra"))

  if (!is.null(module) && .is_longitudinal(series))
    stop("'module' is not supported for ", series,
         ", which has a single shared table.",
         call. = FALSE)

  if (!identical(refresh, FALSE) && !identical(refresh, TRUE) &&
      !identical(refresh, "auto"))
    stop("'refresh' must be FALSE, TRUE, or \"auto\".")
  if (identical(refresh, "auto") && !.is_longitudinal(series))
    stop("refresh = \"auto\" is only valid for longitudinal series (",
         paste(.pumf_longitudinal_series, collapse = ", "), "). ",
         "Use refresh = TRUE to rebuild a specific survey version.")
  if (isTRUE(redownload) && identical(refresh, "auto"))
    stop("redownload = TRUE is not compatible with refresh = \"auto\". ",
         "Call get_pumf() per version instead.")

  if (!is.null(registry)) {
    if (!inherits(registry, "pumf_registry_entry"))
      stop("'registry' must be created by pumf_registry_entry() or ",
           "pumf_registry().", call. = FALSE)
    if (.is_longitudinal(series))
      stop("'registry' overrides are not supported for ", series,
           ", which uses the longitudinal pipeline.", call. = FALSE)
  }

  # get_pumf(borealis = ) becomes a registry override carrying an explicit
  # Borealis source (see pumf_locate_or_download()).
  user_registry <- registry
  # A version previously loaded with `borealis =` keeps its Borealis source:
  # its files and build do not match the built-in (StatCan) configuration, so
  # reopening it without the argument re-applies the recorded DOI.
  # redownload = TRUE discards the Borealis files and returns to the default.
  if (is.null(borealis) && !isTRUE(redownload))
    borealis <- .borealis_cached_doi(series, version, cache_path, registry)
  if (!is.null(borealis)) {
    if (.is_longitudinal(series))
      stop("'borealis' is not supported for ", series, ".", call. = FALSE)
    if (is.null(version))
      stop("'version' must be specified with 'borealis'; it names the cache ",
           "directory for the dataset.", call. = FALSE)
    doi <- .borealis_doi_arg(borealis)
    if (is.null(registry))
      registry <- structure(list(), class = "pumf_registry_entry")
    registry$borealis <- list(doi = doi, files = registry$borealis$files,
                              explicit = TRUE)
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
  if (!.is_longitudinal(series) && is.null(version)) {
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
    if (!eff_rebuild && !is.null(user_registry) &&
        .duckdb_table_exists(.pumf_db_path(series, version, cache_path),
                             .pumf_table_name(series, version, lang)))
      message("A built table for ", series, " ", version, " [", lang,
              "] already exists; the supplied 'registry' is not applied. ",
              "Pass refresh = TRUE to rebuild with it.")
  }

  # For LFS: route directly through lfs_get_pumf so that when data is already
  # loaded the fast path opens only a read-only connection.  Routing through
  # get_pumf_connection (the deprecated shim) always requests read_only=FALSE,
  # which tries to acquire a write lock even when no write is needed -- that
  # fails when a read-only connection from a previous get_pumf("LFS", ...) call
  # is still open.
  if (.is_longitudinal(series)) {
    # Degrade gracefully when Statistics Canada is unreachable (see the non-LFS
    # branch / get_pumf_connection): informative message + NULL, not an error.
    tbl <- tryCatch(
      suppressMessages(
        .long_get_pumf(.pumf_longitudinal_spec(series),
                       version    = version,
                       lang       = lang,
                       cache_path = cache_path,
                       refresh    = refresh,
                       redownload = redownload,
                       read_only  = read_only)
      ),
      canpumf_network_error = function(e) {
        message(conditionMessage(e)); NULL
      })
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

  # Ensure the DB is built and open it in the requested mode directly.  A write
  # connection exists only inside pumf_build_duckdb() while a build/refresh
  # actually writes, and is closed there; on a cache hit every connection is
  # read-only end to end.  Routing through get_pumf_connection() instead would
  # request read_only = FALSE even for pure reads, taking a write lock that
  # conflicts with concurrent read-only sessions (e.g. rendering a notebook
  # while the interactive session still holds tbls open) — the same reason the
  # LFS branch above calls lfs_get_pumf() directly.
  pipe_tbl <- tryCatch(
    suppressMessages(
      pumf_run_pipeline(series, version,
                        lang       = lang,
                        cache_path = cache_path,
                        refresh    = refresh,
                        redownload = redownload,
                        read_only  = read_only)
    ),
    canpumf_network_error = function(e) {
      message(conditionMessage(e)); NULL
    })
  if (is.null(pipe_tbl)) return(invisible(NULL))

  # Select the language table.  For multi-module surveys (e.g. GSS cycle 16)
  # `module` selects which linked table to return; all modules share one DuckDB
  # file and are joinable on the module key.  The pipeline returns the primary
  # module's tbl; a sibling module is opened on the same connection.
  # `.pumf_table_name` validates the module name against the registry.
  table_name <- .pumf_table_name(series, version, lang, module)
  db_path    <- .pumf_db_path(series, version, cache_path)

  if (is.null(module)) {
    tbl <- pipe_tbl
  } else {
    con <- pipe_tbl$src$con
    if (!DBI::dbExistsTable(con, table_name)) {
      DBI::dbDisconnect(con, shutdown = TRUE)
      stop("Table '", table_name, "' not found in ", db_path, ".")
    }
    tbl <- tbl(con, table_name)
  }

  # Register provenance so label_pumf_columns() can find it later via the
  # connection's stable C++ pointer address.
  .pumf_register_con(tbl$src$con, series, version, cache_path, lang, module)

  # Say once per session when the table predates the build stamp (canpumf
  # < 0.6.1) and so lacks pumf_row_id and the sentinel companion.
  .pumf_check_build_stamp(tbl$src$con, series, version, lang, table_name, db_path)

  # When the user loaded the survey's primary module (module = NULL) and the
  # survey is multi-module, list the sibling modules and show how to open one.
  if (is.null(module)) .pumf_announce_modules(series, version)

  tbl
}

# Tracks which database tables have already been reported as built by an
# earlier canpumf, so get_pumf() says it once per session.
.pumf_stale_announced <- new.env(parent = emptyenv())

# On a cache hit, a table built by canpumf < 0.6.1 has no row in
# `pumf_build_info` (and no pumf_row_id key, no sentinel companion).  Its
# values are still those the building version produced, so this is a message
# and not a warning, shown once per session and table, and silenced with
# options(canpumf.stale_cache_message = FALSE).  Returns TRUE when it spoke.
.pumf_check_build_stamp <- function(con, series, version, lang, table_name,
                                    db_path) {
  if (!isTRUE(getOption("canpumf.stale_cache_message", TRUE)))
    return(invisible(FALSE))
  key <- paste(db_path, table_name, sep = "::")
  if (!is.null(.pumf_stale_announced[[key]])) return(invisible(FALSE))
  info <- tryCatch(.read_build_info(con, table_name), error = function(e) NULL)
  rebuild <- sprintf(paste0(
    "Rebuild with get_pumf(\"%s\", \"%s\", refresh = TRUE); list_pumf_cache() ",
    "shows the canpumf version behind every database (column 'built_with'). ",
    "options(canpumf.stale_cache_message = FALSE) silences this message."),
    series, version)
  if (is.null(info)) {
    .pumf_stale_announced[[key]] <- TRUE
    message(sprintf(paste0(
      "%s %s [%s] was built by canpumf before 0.6.1: it has no pumf_row_id key ",
      "and no sentinel companion, so pumf_sidecar() is not available; codes ",
      "that share a label are merged into one level; and its values are those ",
      "of the version that built it (see NEWS for fixes since). "),
      series, version, lang), rebuild)
    return(invisible(TRUE))
  }
  # A stamped table whose metadata has no codes_applied.csv was built by a
  # 0.6.1 development version before value labels were made unique on the
  # data: pumf_dictionary() then describes labels the table may not show.
  applied <- .pumf_codes_applied_path(file.path(dirname(db_path), "metadata"))
  if (!file.exists(applied)) {
    .pumf_stale_announced[[key]] <- TRUE
    message(sprintf(paste0(
      "%s %s [%s] was built by a canpumf 0.6.1 development version before ",
      "value labels were made unique: codes sharing a label may be merged, ",
      "and pumf_dictionary() may not match the table's levels. "),
      series, version, lang), rebuild)
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

# Tracks which (series/version) multi-module hints have been announced this
# session so get_pumf() lists the available modules only once per survey.
.pumf_modules_announced <- new.env(parent = emptyenv())

# Emit a one-time hint, when get_pumf() returns the primary module of a
# multi-module survey, listing the sibling modules and how to open one on the
# same connection with pumf_module().  pumf_module() then announces the join
# key (the two messages are complementary).
.pumf_announce_modules <- function(series, version) {
  reg  <- tryCatch(pumf_registry_lookup(series, version),
                   error = function(e) NULL)
  mods <- .pumf_entry_modules(reg)
  if (is.null(mods) || length(mods) < 2L) return(invisible(NULL))

  akey <- paste(series, version, sep = "/")
  if (!is.null(.pumf_modules_announced[[akey]])) return(invisible(NULL))
  .pumf_modules_announced[[akey]] <- TRUE

  secondary <- names(mods)[!vapply(mods, function(m) isTRUE(m$is_primary),
                                   logical(1L))]
  ex <- secondary[[1L]]
  message(sprintf(
    paste0("%s is a multi-module survey; you loaded the primary module. ",
           "Other linked modules: %s.\n",
           "Open one on the same connection with pumf_module(), e.g.:\n",
           "  %s <- pumf_module(main, \"%s\")"),
    akey, paste(secondary, collapse = ", "), tolower(ex), ex))
  invisible(NULL)
}


#' Open a sibling module of a multi-module survey
#'
#' Some surveys ship several linked fixed-width files that share a respondent
#' key (e.g. GSS cycle 16, "Aging and Social Support", 2002, whose MAIN, CG4,
#' CG6 and CR files all join on `RECID`, with the person weight `WGHT_PER`
#' living only in MAIN).  `get_pumf()` returns the survey's primary module;
#' `pumf_module()` returns one of its sibling modules **on the same DuckDB
#' connection**, so the two tbls are joinable on the shared key without opening
#' a second connection.
#'
#' @param tbl A lazy tbl returned by [get_pumf()] for a multi-module survey.
#' @param module Name of the module to open (e.g. `"CG4"`).  See the survey's
#'   registry entry for available module ids.
#' @return A lazy `dplyr::tbl()` for the requested module, backed by the same
#'   connection as `tbl`.
#' @examples
#' \donttest{
#' main <- get_pumf("GSS", "Cycle 16 (2002)") # primary module (MAIN), has WGHT_PER
#' if (!is.null(main)) {
#'   cg4 <- pumf_module(main, "CG4")          # caregiving module, same connection
#'   dplyr::left_join(main, cg4, by = "RECID")
#'   close_pumf(main)
#' }
#' }
#' @export
pumf_module <- function(tbl, module) {
  if (is.null(module) || !is.character(module) || length(module) != 1L)
    stop("'module' must be a single module name (e.g. \"CG4\").", call. = FALSE)
  con <- tbl$src$con
  prov <- .pumf_lookup_con(con)
  if (is.null(prov))
    stop("Could not find survey provenance for this tbl. ",
         "pumf_module() only works on tbls returned by get_pumf().",
         call. = FALSE)
  table_name <- .pumf_table_name(prov$series, prov$version, prov$lang, module)
  if (!DBI::dbExistsTable(con, table_name))
    stop("Module table '", table_name, "' not found. The survey may not have ",
         "been built with this module.", call. = FALSE)
  out <- dplyr::tbl(con, table_name)
  .pumf_register_con(con, prov$series, prov$version, prov$cache_path,
                     prov$lang, module)
  # Surface the shared respondent key so callers know how to join the modules.
  # The key varies across surveys (PUMFID / RECID / MICRO_ID / CASEID / IDNUM);
  # announce it once per survey per session to keep repeated calls quiet.
  key <- .pumf_module_key(pumf_registry_lookup(prov$series, prov$version))
  if (!is.null(key)) {
    akey <- paste(prov$series, prov$version, sep = "/")
    if (is.null(.pumf_module_key_announced[[akey]])) {
      .pumf_module_key_announced[[akey]] <- TRUE
      message(sprintf("%s modules join on '%s' (e.g. dplyr::inner_join(main, %s, by = \"%s\")).",
                      akey, key, module, key))
    }
  }
  out
}

# Tracks which (series/version) module-key hints have been announced this
# session so pumf_module() messages the join key only once per survey.
.pumf_module_key_announced <- new.env(parent = emptyenv())


# ---- shared metadata helper -------------------------------------------------

# Read the variables tibble for a tbl returned by get_pumf().
# Returns a data.frame(name, label_en, label_fr, type, ...) from metadata/.
.pumf_read_variables_from_prov <- function(prov) {
  series     <- prov$series
  version    <- prov$version
  cache_path <- prov$cache_path
  if (identical(series, "LFS_TIMELINE"))   # get_lfs_timeline()
    return(as.data.frame(.lfs_timeline_ref("variables")))
  if (.is_longitudinal(series)) {
    spec <- .pumf_longitudinal_spec(series)
    spec$variables(cache_path, .long_versions_from_prov(prov))
  } else {
    # Multi-module surveys keep each secondary module's metadata in a
    # metadata/<module>/ subdir; the primary module uses metadata/.
    reg      <- pumf_registry_lookup(series, version)
    mods     <- .pumf_entry_modules(reg)
    subdir   <- if (!is.null(prov$module) && !is.null(mods) &&
                    !is.null(mods[[prov$module]]))
      mods[[prov$module]]$meta_subdir else NULL
    meta_dir <- if (is.null(subdir))
      file.path(cache_path, series, version, "metadata")
    else
      file.path(cache_path, series, version, "metadata", subdir)
    if (!dir.exists(meta_dir))
      stop("Metadata directory not found: '", meta_dir, "'. ",
           "Run get_pumf(\"", series, "\", \"", version, "\") first.",
           call. = FALSE)
    vars <- read_metadata(meta_dir)$variables
    vars$name <- toupper(vars$name)
    # Apply the same labels_supplement the Stage 3 build uses, so a label
    # supplied for a variable the source leaves blank (e.g. CPSS COVID_WT) is
    # visible to label_pumf_columns() and pumf_var_labels().
    .pumf_apply_labels_supplement(vars, pumf_registry_lookup(series, version))
  }
}

# Derive which module a tbl points at from its remote DuckDB table name.
# All modules of a multi-module survey share one connection, so the
# connection-keyed provenance registry can only remember one module at a time.
# When the tbl is a plain base-table reference (or a simple lazy query over
# one) `dbplyr::remote_name()` still resolves the underlying table name, which
# encodes the module's layout_mask; we reverse-map it to the module id so the
# correct metadata/<module>/ subdir is read regardless of which module was most
# recently registered.  Falls back to the stored prov$module for tbls whose
# base table can no longer be recovered (e.g. after a join).
.pumf_tbl_module <- function(tbl, prov) {
  if (.is_longitudinal(prov$series) || identical(prov$series, "LFS_TIMELINE"))
    return(prov$module)
  reg  <- pumf_registry_lookup(prov$series, prov$version)
  mods <- .pumf_entry_modules(reg)
  if (is.null(mods)) return(prov$module)
  tname <- tryCatch(as.character(dbplyr::remote_name(tbl)),
                    error = function(e) NULL)
  if (is.null(tname) || length(tname) != 1L || is.na(tname))
    return(prov$module)
  for (id in names(mods)) {
    if (identical(.pumf_table_name(prov$series, prov$version, prov$lang, id),
                  tname))
      return(id)
  }
  prov$module
}

.pumf_read_variables <- function(tbl) {
  prov <- .pumf_lookup_con(tbl$src$con)
  if (is.null(prov))
    stop("'tbl' has no pumf provenance. Was it created by get_pumf()?",
         call. = FALSE)
  prov$module <- .pumf_tbl_module(tbl, prov)
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
#' \donttest{
#' sfs <- get_pumf("SFS", "2019")
#' if (!is.null(sfs)) {
#'   sfs_labeled <- label_pumf_columns(sfs)
#'   colnames(sfs_labeled)
#'   close_pumf(sfs_labeled)
#' }
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

  var_labels <- .pumf_var_label_map(variables, label_col)

  # Only rename columns present in the tbl
  tbl_cols   <- colnames(tbl)
  var_labels <- var_labels[var_labels$name %in% tbl_cols, , drop = FALSE]

  # Inject labels for derived LFS helper columns that are not in the metadata.
  derived <- .lfs_derived_var_labels
  derived <- derived[derived$name %in% tbl_cols &
                       !derived$name %in% var_labels$name, , drop = FALSE]
  if (nrow(derived) > 0L)
    var_labels <- rbind(var_labels,
                        data.frame(name  = derived$name,
                                   label = derived[[label_col]],
                                   stringsAsFactors = FALSE))

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
#'   (English label), `label_fr` (French label), `description_en` and
#'   `description_fr` (a longer explanation of the variable where the source
#'   documents one beside the short label, as the CCRI census samples do;
#'   `NA` otherwise).  Rows follow survey-metadata order.
#'
#' @seealso [label_pumf_columns()], [get_pumf()]
#'
#' @examples
#' \donttest{
#' sfs <- get_pumf("SFS", "2019")
#' if (!is.null(sfs)) {
#'   pumf_var_labels(sfs)
#'   close_pumf(sfs)
#' }
#' }
#' @export
pumf_var_labels <- function(tbl) {
  variables <- .pumf_read_variables(tbl)
  for (d in setdiff(.metadata_description_cols, names(variables)))
    variables[[d]] <- rep(NA_character_, nrow(variables))
  tibble::as_tibble(variables[, c("name", "label_en", "label_fr",
                                  .metadata_description_cols), drop = FALSE])
}


# ---- close_pumf -------------------------------------------------------------

#' Close the DuckDB connection backing a PUMF lazy table
#'
#' Disconnects the DuckDB connection associated with `x`.  `x` may be either a
#' lazy `dplyr::tbl()` returned by [get_pumf()] (the connection embedded in the
#' tbl is closed) or a DuckDB connection object returned by
#' [get_pumf_connection()] (closed directly).  After calling this function the
#' table or connection can no longer be queried.
#'
#' All lazy tables and sibling modules opened from one [get_pumf()] call share a
#' single connection, so a single `close_pumf()` on any of them releases it.
#'
#' Closing is only necessary when you need to release the file lock -- for
#' example, before calling `get_pumf(..., refresh = TRUE)` on the same survey,
#' or before writing to the DuckDB from another process.  Read-only connections
#' (the default) do not block other readers.
#'
#' @param x A lazy `dplyr::tbl()` returned by [get_pumf()], or a DuckDB
#'   connection returned by [get_pumf_connection()].  `NULL` is accepted and is
#'   a no-op, so `close_pumf()` can be called unconditionally on a [get_pumf()]
#'   result that may be `NULL` (e.g. when Statistics Canada was unreachable).
#'
#' @return Invisibly `NULL`.
#'
#' @seealso [get_pumf()], [get_pumf_connection()]
#'
#' @examples
#' \donttest{
#' sfs <- get_pumf("SFS", "2019")
#' if (!is.null(sfs)) {
#'   # ... analysis ...
#'   close_pumf(sfs)
#' }
#'
#' # Also accepts a raw connection from get_pumf_connection()
#' con <- get_pumf_connection("SHS", "2017")
#' if (!is.null(con)) {
#'   DBI::dbListTables(con)
#'   close_pumf(con)
#' }
#' }
#' @export
close_pumf <- function(x) {
  # NULL is a no-op so close_pumf() can always be called -- e.g. on the result
  # of a get_pumf() that returned NULL because Statistics Canada was unreachable.
  if (is.null(x)) return(invisible(NULL))
  # Accept either a lazy tbl (connection lives in x$src$con) or a DuckDB/DBI
  # connection object handed in directly (e.g. from get_pumf_connection()).
  con <- if (inherits(x, "DBIConnection")) x else x$src$con
  if (!is.null(con) && DBI::dbIsValid(con)) {
    # Drop the provenance entry if this connection was registered by get_pumf().
    # Connections from get_pumf_connection() are not registered, so guard the rm.
    key <- format(con@conn_ref)
    if (exists(key, envir = .pumf_con_registry, inherits = FALSE))
      rm(list = key, envir = .pumf_con_registry, inherits = FALSE)
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
  invisible(NULL)
}


# ---- add_bootstrap_weights ---------------------------------------------

#' Generate bootstrap weights for a PUMF dataset
#'
#' For a **DuckDB-backed lazy table** (the typical case), the bootstrap
#' replicate weights go into a table of their own, keyed like the survey table,
#' and the function returns `tbl` joined with it.  What was applied to `tbl`
#' before (`filter()`, `select()`, [label_pumf_columns()], ...) is kept; only
#' the column(s) that identify the rows (see *ID column*) must still be
#' present.  Where the weights live depends on the connection of `tbl`:
#'   * **Write connection** (`get_pumf(..., read_only = FALSE)`): the weights
#'     are stored in the DuckDB file, and every later call reuses them, in this
#'     session or another.
#'   * **Read-only connection** (the [get_pumf()] default): stored weights are
#'     used when they cover the request.  Otherwise the weights are generated
#'     into a temporary table that lasts until the connection is closed.
#'     Nothing is written to the file, and a message says so.  Pass `seed` to
#'     get the same weights in every session.
#'
#' The function never closes or reopens a connection: `tbl`, and every other
#' table on the same connection, stays valid.
#'
#' For an **in-memory `data.frame` or `tibble`**, bootstrap weights are
#' generated entirely in memory and the augmented data frame is returned.
#'
#' Bootstrap weights are generated by the rescaled bootstrap: for each replicate
#' a sample of \eqn{n} rows is drawn with replacement; the bootstrap weight for
#' row \eqn{i} in replicate \eqn{b} is `original_weight[i] * count[i,b]`, where
#' `count[i,b]` is the number of times row \eqn{i} appeared in draw \eqn{b}.
#'
#' **Incremental re-runs (DuckDB path):** when weights exist already (stored
#' ones, or the temporary ones of this connection) the call only does the work
#' needed to satisfy the request:
#'   * **More replicates** than exist (and no new rows): the additional
#'     replicate columns are appended; existing columns are kept.
#'   * **New rows** in the main table (some rows have no weights yet): because a
#'     bootstrap replicate resamples the full population, added rows invalidate
#'     the existing weights of their resampling universe, so those weights are
#'     deleted and regenerated.  Unstratified, this regenerates every row; when
#'     `strata_cols` are in effect, only the strata that gained rows are
#'     regenerated and complete strata keep their existing weights.
#'   * **Neither:** the existing weights are reused without recomputation.
#' On a read-only connection the result of the first two cases goes to the
#' temporary table (stored replicates are copied into it, so they keep their
#' values), and the stored weights stay as they are.  Pass `overwrite = TRUE`
#' to force a full fresh regeneration regardless.
#'
#' **Multiple weight columns (hierarchical data):** by default `bsw_table` is
#' named after `weight_col` (e.g. `"pumf_bsw_wstpwgt"`), so calling the
#' function twice with different weight columns (e.g. household weight and
#' person weight) produces two independent BSW tables without any conflict.
#' Give the second call its own `prefix` to have both sets of replicate
#' columns in one result: replicate columns of the same `prefix` already in
#' `tbl` are replaced.  A weights table holds the replicates of one `prefix`.
#'
#' **The weights cover the whole survey table**, whatever rows `tbl` is
#' filtered to, so that every subset is weighted from the same replicates.  For
#' a small subset of a very large table (one month of the LFS) on a read-only
#' connection it is cheaper to `collect()` the subset and add the weights to
#' the data frame.
#'
#' **ID column (DuckDB path):** the weights are linked to the survey rows by a
#' key that identifies each row.  If `id_col` is `NULL` (the default):
#'   * The survey registry `bsw_join_key` is used when available (e.g.
#'     `"PEFAMID"` for SFS 2016-2023).
#'   * Otherwise the `pumf_row_id` column that every table built by canpumf
#'     0.6.1 or later carries (see [pumf_sidecar()]) is used.
#'   * The longitudinal series (`"LFS"`, `"LFS_HIST"`) use `SURVYEAR`,
#'     `SURVMNTH` and `REC_NUM` together.
#' A table built by an earlier version that has none of these needs a rebuild
#' (`get_pumf(..., refresh = TRUE)`) or an explicit `id_col`.
#'
#' @param tbl A lazy `dplyr::tbl()` returned by [get_pumf()], **or** an
#'   in-memory `data.frame` / `tibble`.
#' @param weight_col Name of the column holding the survey weights (string,
#'   e.g. `"PWEIGHT"`).
#' @param id_col Optional name(s) of the column(s) that uniquely identify each
#'   row (DuckDB path only).  If `NULL` (default), the registry `bsw_join_key`,
#'   `pumf_row_id` or the longitudinal record key is used (see *ID column*).
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
#'   Columns are named `prefix1`, `prefix2`, ...
#' @param bsw_table Name of the DuckDB table that stores the replicate weights
#'   (DuckDB path only).  Defaults to `NULL`, which auto-names it
#'   `paste0("pumf_bsw_", tolower(weight_col))` so separate calls with
#'   different weight columns do not overwrite each other.  The temporary
#'   table of a read-only connection has the same name prefixed with `tmp_`.
#' @param seed Optional integer seed for reproducibility.
#' @param overwrite If weights for `weight_col` exist already, regenerate them
#'   from scratch when `TRUE`: the stored table on a write connection, the
#'   temporary one on a read-only connection.  When `FALSE` (default) existing
#'   weights are reused.
#'
#' @return
#'   * **DuckDB path:** `tbl` inner-joined with the `n_replicates` bootstrap
#'     weight columns, a lazy `dplyr::tbl()` on the same connection.
#'   * **In-memory path:** the input `data.frame` / `tibble` with bootstrap
#'     weight columns appended so that `n_replicates` replicates are present.
#'     If the input already carries replicate columns for `prefix`, only the
#'     additional ones are generated (existing columns are preserved); when it
#'     already has at least `n_replicates`, the data frame is returned unchanged.
#'
#' @seealso [bsw_info()], [remove_bootstrap_weights()], [get_pumf()]
#'
#' @examples
#' \donttest{
#' # read-only connection (the default): the weights are temporary
#' sfs <- get_pumf("SFS", "2019")
#' if (!is.null(sfs)) {
#'   sfs_bsw <- add_bootstrap_weights(sfs, weight_col = "PWEIGHT",
#'                                    n_replicates = 200L, seed = 42L)
#'   bsw_info(sfs_bsw)
#'   close_pumf(sfs)
#' }
#'
#' # write connection: the weights are stored in the database and reused
#' sfs <- get_pumf("SFS", "2019", read_only = FALSE)
#' if (!is.null(sfs)) {
#'   sfs_bsw <- add_bootstrap_weights(sfs, weight_col = "PWEIGHT",
#'                                    n_replicates = 200L, seed = 42L)
#'   sfs <- remove_bootstrap_weights(sfs_bsw)
#'   close_pumf(sfs)
#' }
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
  # Everything happens on the connection of `tbl`: the weights are written to
  # the database when that connection can write and to a temporary table when
  # it cannot, and the result is `tbl` joined with them.  No connection is
  # closed or opened, and no SQL of the input is taken apart and replayed.

  loc        <- .bsw_locate(tbl)
  con        <- loc$con
  prov       <- loc$prov
  series     <- prov$series
  table_name <- loc$table_name
  physical_cols <- DBI::dbListFields(con, table_name)

  # Resolve weight_col / id_col: if label_pumf_columns() was called, the user
  # may pass a human-readable label (e.g. "Person weight") rather than the
  # coded column name (e.g. "PWEIGHT"). Translate back to the coded name so
  # SQL queries against the raw DuckDB table work correctly.
  weight_col  <- .bsw_resolve_col_prov(con, table_name, weight_col, "weight_col", prov)
  id_explicit <- !is.null(id_col)
  if (id_explicit)
    id_col <- unname(vapply(id_col, function(x)
      .bsw_resolve_col_prov(con, table_name, x, "id_col", prov), character(1L)))

  if (is.null(bsw_table))
    bsw_table <- paste0("pumf_bsw_", tolower(weight_col), loc$suffix)
  tmp_table <- paste0("tmp_", bsw_table)
  writable  <- .bsw_con_writable(con)

  # --- Default row key --------------------------------------------------------
  # Registry key first, then the permanent pumf_row_id that Stage 3 has written
  # since 0.6.1.  The longitudinal tables have neither: their records are
  # identified by month and record number.
  if (!id_explicit) {
    long_key <- c("SURVYEAR", "SURVMNTH", "REC_NUM")
    id_col <-
      if (.is_longitudinal(series) && all(long_key %in% physical_cols))
        long_key
      else if (length(loc$key) == 1L && loc$key %in% physical_cols)
        loc$key
      else if ("pumf_row_id" %in% physical_cols)
        "pumf_row_id"
  }

  # --- Resolve effective strata: explicit > registry > LFS default > none ---
  # character(0) explicitly suppresses the LFS default.
  eff_strata <- if (identical(strata_cols, character(0L))) {
    NULL
  } else if (!is.null(strata_cols)) {
    bad_sc <- setdiff(strata_cols, physical_cols)
    if (length(bad_sc) > 0L)
      stop("strata_cols not found in table '", table_name, "': ",
           paste(bad_sc, collapse = ", "), call. = FALSE)
    strata_cols
  } else if (!is.null(loc$strata) && all(loc$strata %in% physical_cols)) {
    loc$strata
  } else if (.is_longitudinal(series)) {
    intersect(c("SURVYEAR", "SURVMNTH"), physical_cols)
  }
  if (length(eff_strata) == 0L) eff_strata <- NULL

  # --- Look for weights that cover the request --------------------------------
  # A read-only connection prefers its own temporary table (it holds whatever
  # an earlier call on this connection generated) over the stored one.  `use`
  # is the first table with every row and enough replicates; `base` the first
  # one that can be extended.
  cand <- if (overwrite) list()
          else if (writable) list(.bsw_state(con, bsw_table, prefix))
          else list(.bsw_state(con, tmp_table, prefix),
                    .bsw_state(con, bsw_table, prefix))
  use  <- NULL
  base <- NULL
  for (st in cand) {
    if (is.null(st) || !all(st$key %in% physical_cols)) next
    same_key <- is.null(id_col) || setequal(st$key, id_col)
    if (id_explicit && !same_key)
      stop("The bootstrap weights in '", st$table, "' are keyed by ",
           paste(st$key, collapse = ", "), ", not ",
           paste(id_col, collapse = ", "), ".\n",
           "Pass that as 'id_col', or overwrite = TRUE to regenerate them.",
           call. = FALSE)
    st$n_missing <- .bsw_n_missing(con, table_name, st)
    if (st$n_missing == 0L && length(st$reps) >= n_replicates) {
      use <- st
      break
    }
    if (is.null(base) && same_key) base <- st
  }

  # --- Generate what is missing ----------------------------------------------
  if (is.null(use)) {
    if (!is.null(base)) id_col <- base$key
    if (is.null(id_col))
      stop("Table '", table_name, "' has no column that identifies its rows: ",
           "it was built by canpumf before 0.6.1.\n",
           "Rebuild it with get_pumf(..., refresh = TRUE), or name a column ",
           "with unique values in 'id_col'.", call. = FALSE)
    .bsw_generate(con, table_name, weight_col, id_col, eff_strata,
                  n_replicates, prefix, seed, base,
                  target = if (writable) bsw_table else tmp_table,
                  temporary = !writable)
    if (writable) {
      # 0.6.0 and earlier exposed the weights through a view, which would now
      # describe the table as it was before this write.
      DBI::dbExecute(con, sprintf('DROP VIEW IF EXISTS "%s"',
                                  .bsw_legacy_view(table_name, bsw_table)))
    } else {
      message("The bootstrap weights are in a temporary table: 'tbl' is on a ",
              "read-only connection, so they last until it is closed.\n",
              "Open the table with get_pumf(..., read_only = FALSE) to store ",
              "them in the database.")
    }
    use <- .bsw_state(con, if (writable) bsw_table else tmp_table, prefix)
  }

  # --- Join the weights to the input tbl --------------------------------------
  in_cols  <- colnames(tbl)
  key_in   <- .bsw_key_in_tbl(in_cols, use$key, prov)
  rep_in   <- in_cols[.bsw_is_rep(in_cols, prefix)]
  if (length(rep_in) > 0L)
    tbl <- dplyr::select(tbl, -dplyr::all_of(rep_in))
  weights <- dplyr::select(dplyr::tbl(con, use$table),
                           dplyr::all_of(c(use$key, use$reps[seq_len(n_replicates)])))
  dplyr::inner_join(tbl, weights, by = stats::setNames(use$key, key_in))
}


# ---- bootstrap-weight internals ------------------------------------------

# The bootstrap-weight functions were handed a tbl whose connection is closed.
.stop_tbl_con_closed <- function() {
  stop("The connection backing 'tbl' is no longer valid: it was closed by ",
       "close_pumf(), or by a refresh or removal of its database.\n",
       "Call get_pumf() again.", call. = FALSE)
}

# Connection, provenance and physical table behind a tbl from get_pumf().
# `suffix` separates the weights of a secondary module from those of the
# primary one, whose weight column may have the same name; `key` and `strata`
# are the registry's bsw_join_key and bsw_strata for that table.
.bsw_locate <- function(tbl) {
  con <- tbl$src$con
  if (is.null(con) || !DBI::dbIsValid(con)) .stop_tbl_con_closed()

  prov <- .pumf_lookup_con(con)
  if (is.null(prov))
    stop("'tbl' has no pumf provenance. Was it created by get_pumf()?",
         call. = FALSE)
  if (identical(prov$series, "LFS_TIMELINE"))
    stop("Bootstrap weights are not available for get_lfs_timeline(), which ",
         "has no table of its own.\ncollect() the rows of interest and pass ",
         "the data frame to add_bootstrap_weights().", call. = FALSE)

  prov$lang   <- prov$lang %||% "eng"
  prov$module <- .pumf_tbl_module(tbl, prov)
  reg <- pumf_registry_lookup(prov$series, prov$version)
  mod <- if (!is.null(prov$module)) .pumf_entry_modules(reg)[[prov$module]]
  list(con        = con,
       prov       = prov,
       table_name = .pumf_table_name(prov$series, prov$version, prov$lang,
                                     prov$module),
       db_path    = .pumf_db_path(prov$series, prov$version, prov$cache_path),
       suffix     = if (is.null(mod) || isTRUE(mod$is_primary)) ""
                    else paste0("_", tolower(mod$id)),
       key        = mod$bsw_join_key %||% reg$bsw_join_key,
       strata     = mod$bsw_strata %||% reg$bsw_strata)
}

# TRUE when writes through `con` reach the database file.  Asked of the
# database rather than of how the connection was opened: a read-only open
# shares a read-write instance the session already holds (.duckdb_connect()).
.bsw_con_writable <- function(con) {
  ro <- tryCatch(
    DBI::dbGetQuery(con, paste(
      "SELECT readonly FROM duckdb_databases()",
      "WHERE database_name = current_database()"))$readonly,
    error = function(e) NULL)
  if (length(ro) == 1L && !is.na(ro)) return(!ro)
  !isTRUE(tryCatch(con@driver@read_only, error = function(e) FALSE))
}

# Which of `cols` are replicate columns of `prefix` (prefix + digits).
# startsWith + digit suffix avoids regex-escaping a user-supplied prefix.
.bsw_is_rep <- function(cols, prefix) {
  startsWith(cols, prefix) &
    grepl("^[0-9]+$", substring(cols, nchar(prefix) + 1L))
}

# Describe a weights table: its key column(s) and its replicate columns of
# `prefix` in numeric order (CPBSW10 after CPBSW9).  NULL when the table does
# not exist or holds no replicate of that prefix.
.bsw_state <- function(con, name, prefix) {
  if (!DBI::dbExistsTable(con, name)) return(NULL)
  cols   <- DBI::dbListFields(con, name)
  is_rep <- .bsw_is_rep(cols, prefix)
  reps   <- cols[is_rep]
  if (length(reps) == 0L) return(NULL)
  reps <- reps[order(as.integer(substring(reps, nchar(prefix) + 1L)))]
  list(table = name, key = cols[!is_rep], reps = reps)
}

# Number of rows of the survey table that have no weights in `st`.
.bsw_n_missing <- function(con, table_name, st) {
  on <- paste(sprintf('"m"."%s" = "b"."%s"', st$key, st$key), collapse = " AND ")
  DBI::dbGetQuery(con, sprintf(
    'SELECT COUNT(*) AS n FROM "%s" "m" WHERE NOT EXISTS (SELECT 1 FROM "%s" "b" WHERE %s)',
    table_name, st$table, on))$n
}

# One value per row for the key column(s) of a data frame.
.bsw_key <- function(df, cols) {
  if (length(cols) == 1L) df[[cols]]
  else do.call(paste, c(unname(as.list(df[cols])), sep = "\r"))
}

# The key column(s) as the input tbl names them: coded, or under the variable
# label that label_pumf_columns() gave them.
.bsw_key_in_tbl <- function(in_cols, key, prov) {
  out  <- key
  gone <- setdiff(key, in_cols)
  if (length(gone) > 0L) {
    label_col <- if (prov$lang == "eng") "label_en" else "label_fr"
    map <- tryCatch(
      .pumf_var_label_map(.pumf_read_variables_from_prov(prov), label_col),
      error = function(e) NULL)
    for (k in gone) {
      lab <- map$label[map$name == k]
      if (length(lab) == 1L && lab %in% in_cols) out[out == k] <- lab
    }
    gone <- setdiff(gone, key[out != key])
  }
  if (length(gone) > 0L)
    stop("'tbl' no longer has the column", if (length(gone) > 1L) "s", " ",
         paste(gone, collapse = ", "), " that link",
         if (length(gone) == 1L) "s", " its rows to the bootstrap weights.\n",
         "Add the weights before select() or summarise() drops ",
         if (length(gone) > 1L) "them" else "it", ".", call. = FALSE)
  out
}

# Name of the view through which 0.6.0 and earlier exposed a weights table:
# "pumf_bsw_pweight" on table "eng" -> "eng_bsw_pweight".
.bsw_legacy_view <- function(table_name, bsw_table)
  paste0(table_name, "_", sub("^pumf_", "", bsw_table))

# Generate replicate weights for the whole survey table and write them to
# `target` on `con`, as a temporary table when `temporary`.
#   base = NULL: fresh generation.
#   base = a .bsw_state() with n_missing: keep what is still valid in that
#   table and add the replicate columns and/or regenerate the rows it lacks.
#   `base$table` is `target` itself, or the stored table a read-only
#   connection extends into its temporary one.
.bsw_generate <- function(con, table_name, weight_col, id_col, strata,
                          n_replicates, prefix, seed, base, target, temporary) {

  # Pull ALL rows' weights: bootstrap replicates resample the full (stratum)
  # population, so added rows invalidate -- and require regenerating -- every
  # row in the affected resampling universe, not just the new rows themselves.
  wt <- DBI::dbGetQuery(con, sprintf(
    'SELECT %s, CAST("%s" AS DOUBLE) AS ".w" FROM "%s"',
    paste0('"', unique(c(id_col, strata)), '"', collapse = ", "),
    weight_col, table_name))
  names(wt)[names(wt) == ".w"] <- "w"

  key <- .bsw_key(wt, id_col)
  if (anyNA(wt[id_col]) || anyDuplicated(key) > 0L)
    stop("'", paste(id_col, collapse = "', '"), "' ",
         if (length(id_col) > 1L) "do" else "does",
         " not identify the rows of '", table_name, "': the values are ",
         "missing or repeated for some rows.\n",
         "Name the column(s) that do in 'id_col'.", call. = FALSE)

  if (anyNA(wt$w)) {
    warning(sum(is.na(wt$w)), " NA weight(s) in '", weight_col,
            "' replaced with 0.", call. = FALSE)
    wt$w[is.na(wt$w)] <- 0
  }

  n_existing <- length(base$reps)
  n_target   <- max(n_existing, n_replicates)
  canon      <- c(id_col, paste0(prefix, seq_len(n_target)))

  if (temporary) {
    tmp_gb <- nrow(wt) * n_target * 8 / 1e9
    if (tmp_gb > 2)
      warning(sprintf(paste0(
        "The temporary bootstrap weights take about %.0f GB of memory and ",
        "temporary disk space, and are gone when the connection is closed.\n",
        "Open the table with get_pumf(..., read_only = FALSE) to store them ",
        "once, or collect() the rows of interest and add the weights to the ",
        "data frame."), tmp_gb), call. = FALSE)
  }

  # Replicates n_cols_start+1 .. n_cols_end for one resampling universe (the
  # table, or one stratum).  seed_val NULL: the caller has set the seed.
  gen <- function(wt_df, n_cols_start, n_cols_end, seed_val,
                  show_progress = TRUE) {
    n <- nrow(wt_df)
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
    mat <- wt_df$w * counts
    colnames(mat) <- paste0(prefix, seq(n_cols_start + 1L, n_cols_end))
    cbind(wt_df[id_col], as.data.frame(mat))
  }

  # ---- Fresh generation, stratified ------------------------------------------
  # Write the strata one at a time, so the full n x n_replicates matrix is
  # never materialised in memory: peak memory is one stratum's matrix.
  if (is.null(base) && !is.null(strata)) {
    strata_key  <- interaction(wt[strata], drop = TRUE)
    strata_lvls <- levels(strata_key)
    n_st        <- length(strata_lvls)
    message(sprintf(
      "Generating %d %s replicates across %d %s strata (%d total obs)...",
      n_replicates, prefix, n_st, paste(strata, collapse = "/"), nrow(wt)))
    if (!is.null(seed)) set.seed(seed)
    for (si in seq_along(strata_lvls)) {
      s_data <- wt[which(strata_key == strata_lvls[si]), , drop = FALSE]
      sv_str <- paste(strata,
                      as.character(unlist(s_data[1L, strata, drop = FALSE])),
                      sep = "=", collapse = ", ")
      message(sprintf("  Stratum [%d/%d] %s (%d obs)",
                      si, n_st, sv_str, nrow(s_data)))
      chunk_df <- gen(s_data, 0L, n_replicates,
                      seed_val = NULL, show_progress = FALSE)
      DBI::dbWriteTable(con, target, chunk_df, temporary = temporary,
                        overwrite = (si == 1L), append = (si > 1L))
      rm(chunk_df)
    }
    return(.bsw_index(con, target, id_col, temporary))
  }

  if (is.null(base)) {
    # ---- Fresh generation, unstratified --------------------------------------
    message(sprintf("Generating %d %s replicates for %d observations...",
                    n_replicates, prefix, nrow(wt)))
    bsw_df <- gen(wt, 0L, n_replicates, seed)
  } else {
    # ---- Existing weights, with added rows and/or added replicate columns ----
    #
    # Bootstrap replicate weights are produced by resampling the full
    # population (or, when stratified, the full stratum).  Therefore:
    #   * Added ROWS invalidate the replicate weights of their resampling
    #     universe and force regeneration there -- the whole table when
    #     unstratified, or just the strata that gained rows when stratified
    #     (complete strata keep their existing weights).
    #   * Added COLUMNS are independent extra replicates appended to rows
    #     whose resampling universe is unchanged.
    need_more_rows <- base$n_missing > 0L
    old_bsw <- DBI::dbGetQuery(con, sprintf(
      'SELECT %s FROM "%s"',
      paste0('"', c(id_col, base$reps), '"', collapse = ", "), base$table))
    old_key <- .bsw_key(old_bsw, id_col)
    has_bsw <- key %in% old_key
    n_new   <- sum(!has_bsw)

    if (is.null(strata)) {
      if (need_more_rows) {
        # Whole population changed: every replicate weight is stale.
        message(sprintf(
          "%d new row(s) detected; deleting and regenerating all %d %s replicates for %d observations...",
          n_new, n_target, prefix, nrow(wt)))
        bsw_df <- gen(wt, 0L, n_target, seed)[, canon, drop = FALSE]
      } else {
        # Population unchanged, only add independent replicates.
        message(sprintf("Adding replicates %d-%d to the bootstrap weights...",
                        n_existing + 1L, n_target))
        add_df <- gen(wt, n_existing, n_target, seed)
        bsw_df <- merge(old_bsw, add_df, by = id_col,
                        all = TRUE)[, canon, drop = FALSE]
      }
    } else {
      # Stratified: regenerate only the strata that have missing weights.
      strata_key  <- interaction(wt[strata], drop = TRUE)
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
        aff_dat <- wt[is_affected, , drop = FALSE]
        aff_key <- droplevels(strata_key[is_affected])
        for (lv in levels(aff_key)) {
          s_data <- aff_dat[aff_key == lv, , drop = FALSE]
          parts[[length(parts) + 1L]] <-
            gen(s_data, 0L, n_target, NULL,
                show_progress = FALSE)[, canon, drop = FALSE]
        }
      }

      # 2) Unaffected strata: keep existing weights; add columns if requested,
      #    resampling within each stratum (independent extra replicates).
      if (any(!is_affected)) {
        unaff_dat <- wt[!is_affected, , drop = FALSE]
        keep_bsw  <- old_bsw[old_key %in% key[!is_affected], , drop = FALSE]
        if (n_target > n_existing) {
          unaff_key <- droplevels(strata_key[!is_affected])
          message(sprintf(
            "Adding replicates %d-%d to %d unaffected stratum/strata...",
            n_existing + 1L, n_target, nlevels(unaff_key)))
          add_parts <- list()
          for (lv in levels(unaff_key)) {
            s_data <- unaff_dat[unaff_key == lv, , drop = FALSE]
            add_parts[[length(add_parts) + 1L]] <-
              gen(s_data, n_existing, n_target, NULL, show_progress = FALSE)
          }
          keep_bsw <- merge(keep_bsw, do.call(rbind, add_parts), by = id_col,
                            all = TRUE)
        }
        parts[[length(parts) + 1L]] <- keep_bsw[, canon, drop = FALSE]
      }

      bsw_df <- do.call(rbind, parts)
    }
  }

  if (!temporary)
    message("Writing bootstrap weight table '", target, "' to DuckDB...")
  DBI::dbWriteTable(con, target, bsw_df, overwrite = TRUE,
                    temporary = temporary)
  .bsw_index(con, target, id_col, temporary)
}

# Index a stored weights table on its key.  A temporary table goes without.
.bsw_index <- function(con, target, id_col, temporary) {
  if (!temporary)
    DBI::dbExecute(con, sprintf(
      'CREATE INDEX IF NOT EXISTS "idx_%s" ON "%s" (%s)',
      target, target, paste0('"', id_col, '"', collapse = ", ")))
  invisible(NULL)
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
  # col is not a raw column name -- try to find it as a human-readable label.
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

  # Case A: enough replicates already present -- reuse silently, no regeneration.
  if (n_existing >= n_replicates) {
    message(sprintf(
      "Data frame already has %d '%s' replicate(s) (>= %d requested); reusing.",
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

# The bootstrap weight tables a connection can see: those stored in its
# database ("pumf_bsw*") and its own temporary ones ("tmp_pumf_bsw*").
# `rows` is DuckDB's estimated row count.
.bsw_tables <- function(con) {
  tabs <- DBI::dbGetQuery(con, paste(
    "SELECT table_name, temporary, estimated_size AS rows,",
    "column_count AS n_cols FROM duckdb_tables()",
    "WHERE temporary OR database_name = current_database()"))
  keep <- ifelse(tabs$temporary, grepl("^tmp_pumf_bsw", tabs$table_name),
                 grepl("^pumf_bsw", tabs$table_name))
  tabs <- tabs[keep, , drop = FALSE]
  tabs[order(tabs$temporary, tabs$table_name), , drop = FALSE]
}

# The replicate weights that came with the survey.  They are columns of the
# survey table: joined in Stage 3 from the release's bootstrap weights file
# (registry `bsw_file_mask`), or part of the data file itself (Census WT1-WT16,
# GSS WTBS_001-WTBS_500).  Nothing records which columns they are, so they are
# recognised: a family of numeric columns named <prefix><number> and numbered
# 1..n without gaps is a set of replicate weights when
#   - none of its columns is in variables.csv (the columns of a bootstrap
#     weights file, which the survey's metadata does not describe), or
#   - a variable label calls it a bootstrap or replicate weight, or
#   - none of its columns has a label (layout-only columns promoted by
#     .promote_layout_numeric(), e.g. CIUS WRPG1-WRPG1000).
# Numbered families of survey variables (SGVP GS1DNX01-GS1DNX15) are documented
# with labels of their own and fail all three.  Returns a data frame with
# `prefix` and `n_replicates`; zero rows for the longitudinal series, whose
# shared table has no single variables.csv, and for a table without metadata.
.bsw_survey_families <- function(con, loc) {
  none <- data.frame(prefix = character(0L), n_replicates = integer(0L))
  if (.is_longitudinal(loc$prov$series)) return(none)
  vars <- tryCatch(read_metadata(.pumf_prov_meta(loc$prov)$meta_dir)$variables,
                   error = function(e) NULL)
  if (is.null(vars)) return(none)

  cols <- DBI::dbGetQuery(con, paste(
    "SELECT column_name, data_type FROM duckdb_columns()",
    "WHERE database_name = current_database() AND table_name = ?",
    "ORDER BY column_index"), params = list(loc$table_name))
  cols <- cols[grepl("[0-9]$", cols$column_name) &
                 grepl("^(DOUBLE|FLOAT|DECIMAL|U?(TINY|SMALL|BIG|HUGE)?INT)",
                       cols$data_type), , drop = FALSE]
  if (nrow(cols) == 0L) return(none)
  cols$prefix <- sub("[0-9]+$", "", cols$column_name)
  cols$num    <- as.numeric(substring(cols$column_name, nchar(cols$prefix) + 1L))

  labelled <- function(x) !is.na(x) & nzchar(x)
  is_bsw <- vapply(split(cols, cols$prefix), function(k) {
    if (!nzchar(k$prefix[[1L]]) || nrow(k) < 2L ||
        !setequal(k$num, seq_len(nrow(k)))) return(FALSE)
    v <- vars[match(k$column_name, vars$name), , drop = FALSE]
    if (all(is.na(v$name))) return(TRUE)
    if (any(grepl("bootstrap|replicate|r[e\u00e9]pliqu",
                  paste(v$label_en, v$label_fr), ignore.case = TRUE)))
      return(TRUE)
    !any(labelled(v$label_en) | labelled(v$label_fr))
  }, logical(1L))

  n <- table(cols$prefix)[names(is_bsw)[is_bsw]]
  data.frame(prefix = names(n), n_replicates = as.integer(n))
}

#' Summarise the bootstrap weights of a PUMF table
#'
#' Lists the bootstrap weights available for a PUMF lazy table, one row per set
#' of replicates, and says for each where it comes from:
#' \itemize{
#'   \item `source = "survey"`: replicate weights that came with the survey.
#'     They are columns of the survey table itself and are always part of the
#'     table [get_pumf()] returns.
#'   \item `source = "generated"`: weights made by [add_bootstrap_weights()],
#'     which are kept in a table of their own: stored in the DuckDB file, or
#'     temporary on this connection.
#' }
#' Returns an empty tibble (invisibly) when there are none.
#'
#' The survey's own replicate weights are recognised by their column names and
#' variable labels: a family of numeric columns numbered from 1 without gaps
#' (`BSW1`, ..., `BSW1000`) that the survey's metadata labels as bootstrap or
#' replicate weights, or does not describe at all.  The survey's documentation
#' has the last word on what they are and which weight they belong to.  They
#' are not reported for the longitudinal series (LFS), which ship none.
#'
#' @param tbl A lazy `dplyr::tbl()` returned by [get_pumf()] or by
#'   [add_bootstrap_weights()].
#'
#' @return A tibble with one row per set of replicate weights and the columns:
#'   \describe{
#'     \item{`source`}{`"survey"` for replicate weights that came with the
#'       survey, `"generated"` for weights made by [add_bootstrap_weights()].}
#'     \item{`weight_col`}{The weight column the generated weights were built
#'       from (matched back to the case used in the survey table).  `NA` for
#'       the survey's own replicates: the survey's documentation says which
#'       weight they belong to.}
#'     \item{`prefix`}{Common prefix of the replicate columns, which are named
#'       prefix plus number (`"BSW"` for `BSW1`, ..., `BSW1000`; `"WTBS_"` for
#'       `WTBS_001`, ..., `WTBS_500`).}
#'     \item{`bsw_table`}{Name of the DuckDB table holding the replicate
#'       columns: the survey table for `source = "survey"`, the weights table
#'       for `source = "generated"`.}
#'     \item{`temporary`}{`TRUE` for a temporary table, which belongs to this
#'       connection and is gone when it is closed; `FALSE` for weights stored
#'       in the database.}
#'     \item{`n_replicates`}{Number of bootstrap replicate columns.}
#'     \item{`size_mb`}{Size of the weights in megabytes before compression
#'       (rows times replicates times 8 bytes).  Stored weights take less on
#'       disk.}
#'   }
#'
#' @seealso [add_bootstrap_weights()], [remove_bootstrap_weights()]
#'
#' @examples
#' \donttest{
#' sfs <- get_pumf("SFS", "2019")
#' if (!is.null(sfs)) {
#'   bsw_info(sfs)   # the replicate weights SFS 2019 ships with
#'   sfs_bsw <- add_bootstrap_weights(sfs, weight_col = "PWEIGHT",
#'                                    n_replicates = 50L, seed = 1L)
#'   bsw_info(sfs_bsw)
#'   close_pumf(sfs)
#' }
#' }
#' @export
bsw_info <- function(tbl) {
  if (is.data.frame(tbl))
    stop("bsw_info() requires a DuckDB-backed lazy tbl from get_pumf(). ",
         "For in-memory data frames, inspect column names directly.",
         call. = FALSE)

  loc    <- .bsw_locate(tbl)
  con    <- loc$con
  tabs   <- .bsw_tables(con)
  survey <- .bsw_survey_families(con, loc)

  if (nrow(tabs) == 0L && nrow(survey) == 0L) {
    message("No bootstrap weights found for '", loc$table_name, "' in '",
            basename(loc$db_path), "': the survey table has no replicate ",
            "weights and add_bootstrap_weights() has made none.")
    return(invisible(tibble::tibble(
      source       = character(0L),
      weight_col   = character(0L),
      prefix       = character(0L),
      bsw_table    = character(0L),
      temporary    = logical(0L),
      n_replicates = integer(0L),
      size_mb      = numeric(0L)
    )))
  }
  size_mb <- function(n_row, n_rep) round(as.numeric(n_row) * n_rep * 8 / 1e6, 2)

  # The survey's own replicates, columns of the survey table.
  survey_rows <- if (nrow(survey) > 0L) {
    n_row <- DBI::dbGetQuery(con, paste(
      "SELECT estimated_size AS n FROM duckdb_tables()",
      "WHERE database_name = current_database() AND table_name = ?"),
      params = list(loc$table_name))$n
    list(tibble::tibble(
      source       = "survey",
      weight_col   = NA_character_,
      prefix       = survey$prefix,
      bsw_table    = loc$table_name,
      temporary    = FALSE,
      n_replicates = survey$n_replicates,
      size_mb      = size_mb(n_row, survey$n_replicates)
    ))
  }

  # Columns of the survey tables: they tell the key column(s) of a weights
  # table from its replicates, and restore the case of the weight column.
  # Views are left out: 0.6.0 and earlier exposed the weights through one.
  cols <- DBI::dbGetQuery(con, paste(
    "SELECT table_name, column_name FROM duckdb_columns()",
    "WHERE (database_name = current_database() OR database_name = 'temp')",
    "AND table_name IN (SELECT table_name FROM duckdb_tables())"))
  survey_cols <- unique(cols$column_name[
    !grepl("^(tmp_)?pumf_bsw", cols$table_name)])

  rows <- lapply(seq_len(nrow(tabs)), function(i) {
    bt <- tabs$table_name[[i]]
    # "pumf_bsw_wstpwgt" -> weight column "wstpwgt" ("" for a legacy
    # "pumf_bsw"); a secondary module's table adds "_<module>".
    wc <- sub("^(tmp_)?pumf_bsw_?", "", bt)
    for (cand in unique(c(wc, sub("_[^_]+$", "", wc)))) {
      hit <- survey_cols[tolower(survey_cols) == cand]
      if (length(hit) == 1L) {
        wc <- hit
        break
      }
    }
    bt_cols <- cols$column_name[cols$table_name == bt]
    reps    <- bt_cols[!bt_cols %in% survey_cols]

    tibble::tibble(
      source       = "generated",
      weight_col   = wc,
      prefix       = if (length(reps)) sub("[0-9]+$", "", reps[[1L]])
                     else NA_character_,
      bsw_table    = bt,
      temporary    = tabs$temporary[[i]],
      n_replicates = length(reps),
      size_mb      = size_mb(tabs$rows[[i]], length(reps))
    )
  })

  do.call(rbind, c(survey_rows, rows))
}


# ---- remove_bootstrap_weights ------------------------------------------

#' Remove bootstrap weight tables from a PUMF DuckDB database
#'
#' Drops the bootstrap weight table(s) created by [add_bootstrap_weights()].
#' The temporary tables of a read-only connection are always dropped.  Weights
#' stored in the DuckDB file are dropped only through a write connection
#' (`get_pumf(..., read_only = FALSE)`); on a read-only connection they are
#' left in place and the function says so.
#'
#' Like [add_bootstrap_weights()], the function works on the connection of
#' `tbl` and never closes it.  Tables that [add_bootstrap_weights()] returned
#' for the removed weights can no longer be queried.
#'
#' @param tbl A lazy `dplyr::tbl()` returned by [get_pumf()] or by
#'   [add_bootstrap_weights()].
#' @param weight_col Name of the weight column whose BSW table should be
#'   removed (e.g. `"PWEIGHT"`).  If `NULL` (default), **all** bootstrap
#'   weight tables are removed.
#'
#' @return A lazy `dplyr::tbl()` of the survey table (without BSW columns) on
#'   the connection of `tbl`.
#'
#' @seealso [add_bootstrap_weights()], [bsw_info()], [get_pumf()]
#'
#' @examples
#' \donttest{
#' sfs <- get_pumf("SFS", "2019")
#' if (!is.null(sfs)) {
#'   sfs_bsw <- add_bootstrap_weights(sfs, weight_col = "PWEIGHT", seed = 1L)
#'   # Remove only the PWEIGHT BSW table
#'   sfs <- remove_bootstrap_weights(sfs_bsw, weight_col = "PWEIGHT")
#'   close_pumf(sfs)
#' }
#' }
#' @export
remove_bootstrap_weights <- function(tbl, weight_col = NULL) {
  if (is.data.frame(tbl))
    stop("remove_bootstrap_weights() requires a DuckDB-backed lazy tbl. ",
         "For in-memory data frames, drop BSW columns directly, e.g.: ",
         "df[, !grepl(\"^BSW[0-9]+$\", names(df))]",
         call. = FALSE)

  loc        <- .bsw_locate(tbl)
  con        <- loc$con
  table_name <- loc$table_name
  tabs       <- .bsw_tables(con)

  if (!is.null(weight_col)) {
    # A variable label is accepted, as in add_bootstrap_weights().
    wc <- tryCatch(
      .bsw_resolve_col_prov(con, table_name, weight_col, "weight_col", loc$prov),
      error = function(e) weight_col)
    target <- paste0("pumf_bsw_", tolower(wc), loc$suffix)
    tabs   <- tabs[tabs$table_name %in% c(target, paste0("tmp_", target)), ,
                   drop = FALSE]
    if (nrow(tabs) == 0L)
      stop("No bootstrap weight table found for weight_col '", weight_col,
           "'. Use bsw_info() to see what is present.", call. = FALSE)
  }

  if (nrow(tabs) == 0L) {
    message("No bootstrap weight tables to remove from '",
            basename(loc$db_path), "'.")
    return(tbl)
  }

  stored <- tabs$table_name[!tabs$temporary]
  temp   <- tabs$table_name[tabs$temporary]

  # Stored weights go only through a connection that can write.  Nothing is
  # closed and reopened to get there: that is the caller's decision.
  if (length(stored) > 0L && !.bsw_con_writable(con)) {
    how <- paste0("close_pumf() the table and open it with ",
                  "get_pumf(..., read_only = FALSE) to remove them.")
    if (length(temp) == 0L)
      stop("The bootstrap weights in '", paste(stored, collapse = "', '"),
           "' are stored in the database, and 'tbl' is on a read-only ",
           "connection.\n", how, call. = FALSE)
    message("The bootstrap weights stored in '",
            paste(stored, collapse = "', '"), "' are kept: 'tbl' is on a ",
            "read-only connection.\n", how)
    stored <- character(0L)
  }

  for (bt in temp) {
    message("Dropping temporary bootstrap weight table '", bt, "'...")
    DBI::dbExecute(con, sprintf('DROP TABLE IF EXISTS "%s"', bt))
  }
  for (bt in stored) {
    # The view through which 0.6.0 and earlier exposed the weights.
    DBI::dbExecute(con, sprintf('DROP VIEW IF EXISTS "%s"',
                                .bsw_legacy_view(table_name, bt)))
    message("Dropping bootstrap weight table '", bt, "'...")
    DBI::dbExecute(con, sprintf('DROP TABLE IF EXISTS "%s"', bt))
  }

  dplyr::tbl(con, table_name)
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
#'       `type`, `decimals`, `missing_low`, `missing_high`, `description_en`,
#'       `description_fr`.  The descriptions are the longer text some
#'       documentation carries beside the short label (the CCRI census
#'       samples); `NA` for the surveys that document a label only.}
#'     \item{`codes`}{Tibble with columns `name`, `val`, `label_en`,
#'       `label_fr`, mapping numeric codes to their labels.}
#'     \item{`layout`}{Tibble with columns `name`, `start`, `end` for
#'       fixed-width data files; `NULL` for CSV-format surveys.}
#'   }
#'   Returns `invisible(NULL)` with an informative message if the data must be
#'   downloaded but Statistics Canada is unreachable.
#'
#' @seealso [get_pumf()], [pumf_var_labels()]
#'
#' @examples
#' \donttest{
#' meta <- pumf_metadata("SFS", "2019")
#' if (!is.null(meta)) {
#'   meta$variables
#'   meta$codes[meta$codes$name == "PEFAMID", ]
#' }
#' }
#' @export
pumf_metadata <- function(series,
                           version,
                           cache_path = getOption("canpumf.cache_path",
                                                   tempdir()),
                           refresh    = FALSE,
                           redownload = FALSE,
                           registry   = NULL) {
  version     <- pumf_resolve_version(series, version, cache_path)
  if (!is.null(registry)) {
    if (!inherits(registry, "pumf_registry_entry"))
      stop("'registry' must be created by pumf_registry_entry() or ",
           "pumf_registry().", call. = FALSE)
    if (.is_longitudinal(series))
      stop("'registry' overrides are not supported for ", series, ".",
           call. = FALSE)
    .pumf_registry_override_set(series, version, registry)
    on.exit(.pumf_registry_override_clear(series, version), add = TRUE)
  }
  reg         <- pumf_registry_lookup(series, version)
  eff_refresh <- refresh || redownload
  if (.is_longitudinal(series)) {
    version_dir <- tryCatch(
      .pumf_longitudinal_spec(series)$prepare(version, cache_path = cache_path,
                                              refresh = eff_refresh,
                                              redownload = redownload),
      canpumf_network_error = function(e) {
        message(conditionMessage(e)); NULL
      })
    if (is.null(version_dir)) return(invisible(NULL))
    return(read_metadata(file.path(version_dir, "metadata")))
  }
  # Degrade gracefully when Statistics Canada is unreachable: message + NULL
  # rather than a hard error (consistent with get_pumf()).
  version_dir <- tryCatch(
    pumf_locate_or_download(series, version,
                            cache_path = cache_path,
                            refresh    = eff_refresh,
                            redownload = redownload),
    canpumf_network_error = function(e) {
      message(conditionMessage(e)); NULL
    })
  if (is.null(version_dir)) return(invisible(NULL))
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


# ---- sidecar tables: pumf_sidecar(), list_pumf_sidecars() ---------------

#' Sidecar tables of a PUMF: sentinel codes, removed records
#'
#' Besides the survey table that [get_pumf()] returns, a database holds
#' record-level tables that belong to it, each linked by the permanent
#' `pumf_row_id` column (the record's 1-based position in the data file).
#' `list_pumf_sidecars()` says which ones a table has, and `pumf_sidecar()`
#' returns one of them as a lazy tbl, or the survey table combined with it.
#' (Bootstrap weights are kept apart: see [add_bootstrap_weights()] and
#' [bsw_info()].)
#'
#' @section Sidecars:
#' \describe{
#'   \item{`"sentinels"`}{Statistics Canada codes "not applicable", "not
#'     available", "not stated" and similar non-responses in numeric
#'     variables as sentinel values (Census income `9999999` / `8888888`, GSS
#'     `996`-`999`, ...).  [get_pumf()] converts them to `NA` so that sums and
#'     means are right, which loses the distinction between the reasons.  The
#'     sidecar keeps it: one row per record in which at least one value was a
#'     sentinel, and one column per variable in which a sentinel occurred,
#'     named as in the survey table.  A cell holds the sentinel's label
#'     (`"Not applicable"`; the code's digits where nothing labels it) where
#'     the survey table has `NA` for that reason, and `NA` where it has a
#'     value.  With `join = TRUE` the columns are left-joined onto `tbl` as
#'     `<VAR>_sentinel`.  It covers the sentinel rules Stage 3 applies:
#'     declared `MISSING VALUES` ranges, labelled missing codes, and the
#'     registry's `na_values` / `missing_codes` fixups (see
#'     `vignette("pipeline")`).  Values the data file could not parse as
#'     numbers, and unlabelled values of a categorical variable, are not
#'     sentinels and are not recorded.  Every table built by canpumf 0.6.1 or
#'     later has this sidecar.}
#'   \item{`"removed"`}{Records the producer of the file flags as not
#'     belonging to the data.  The 1881 census of The Canadian Peoples project
#'     (`"TCP"`, `"1881"`) marks 1,137 rows that were crossed out on the
#'     census page, are duplicates or blank, or do not refer to a person
#'     (`remove_TCP = 1`, the reason in `remove_why_TCP`).  [get_pumf()]
#'     leaves such records out of the survey table, so that counts are right
#'     without a filter; the sidecar holds them with the survey table's
#'     columns, labels and types.  Their `pumf_row_id` values are the gaps in
#'     the survey table's.  With `join = TRUE` they are appended to `tbl`
#'     (`UNION ALL`), which gives the file as published.  Only datasets whose
#'     registry entry declares `removed_records` have this sidecar (see
#'     [pumf_registry_entry()]).}
#' }
#'
#' Sidecars are not available for the longitudinal series (`"LFS"`,
#' `"LFS_HIST"`), whose shared databases are appended month by month:
#' `list_pumf_sidecars()` returns no rows for them.
#'
#' @param tbl A lazy `dplyr::tbl()` returned by [get_pumf()].
#' @param sidecar The sidecar's name: `"sentinels"` or `"removed"`.
#' @param join If `TRUE`, return `tbl` combined with the sidecar instead of
#'   the sidecar: for `"sentinels"` a left join on `pumf_row_id` with the
#'   sentinel columns suffixed `_sentinel` (`INCTAX_sentinel`), for
#'   `"removed"` the union of the two.  Apply it before
#'   [label_pumf_columns()], and for `"removed"` before any verb that changes
#'   the columns.
#'
#' @return `pumf_sidecar()`: a lazy `dplyr::tbl()` on the same connection as
#'   `tbl`.  An error is raised when the table has no such sidecar; for
#'   `"sentinels"` that means it was built by canpumf before 0.6.1 and needs
#'   `refresh = TRUE`.
#'
#'   `list_pumf_sidecars()`: a tibble with one row per sidecar the table has
#'   and the columns `sidecar` (the name to pass to `pumf_sidecar()`),
#'   `table` (the DuckDB table), `kind` (`"values"`: columns annotating the
#'   survey table's records; `"records"`: records left out of it), `n_rows`
#'   and `description`.
#'
#' @examples
#' \dontrun{
#' census <- get_pumf("Census", "2011 (individuals)")
#' list_pumf_sidecars(census)
#'
#' # how many NA incomes are "Not available" vs "Not applicable"?
#' pumf_sidecar(census, "sentinels") |> dplyr::count(TOTINC) |> dplyr::collect()
#'
#' # keep the reason next to the value
#' census |>
#'   pumf_sidecar("sentinels", join = TRUE) |>
#'   dplyr::filter(is.na(TOTINC)) |>
#'   dplyr::count(TOTINC_sentinel)
#'
#' # the records the 1881 census file flags for removal, by reason
#' tcp <- get_pumf("TCP", "1881")
#' pumf_sidecar(tcp, "removed") |> dplyr::count(REMOVE_WHY_TCP)
#' }
#' @export
pumf_sidecar <- function(tbl, sidecar, join = FALSE) {
  loc <- .pumf_sidecar_locate(tbl, "pumf_sidecar()")
  if (missing(sidecar) || !is.character(sidecar) || length(sidecar) != 1L ||
      !sidecar %in% names(.pumf_sidecars))
    stop("'sidecar' must be one of ",
         paste0('"', names(.pumf_sidecars), '"', collapse = ", "),
         "; see list_pumf_sidecars().", call. = FALSE)
  if (.is_longitudinal(loc$prov$series))
    stop("pumf_sidecar() is not available for the longitudinal series (",
         loc$prov$series, ").", call. = FALSE)
  spec       <- .pumf_sidecars[[sidecar]]
  side_table <- spec$table(loc$table_name)
  if (!DBI::dbExistsTable(loc$con, side_table))
    stop("No \"", sidecar, "\" sidecar for ", loc$prov$series, " ",
         loc$prov$version, ": ", spec$absent, call. = FALSE)
  side <- dplyr::tbl(loc$con, side_table)
  if (!join) return(side)
  if (identical(spec$kind, "values")) {
    if (!"pumf_row_id" %in% colnames(tbl))
      stop("'tbl' has no pumf_row_id column to join on; pass the tbl before ",
           "select() drops it.", call. = FALSE)
    return(dplyr::left_join(tbl, side, by = "pumf_row_id",
                            suffix = c("", spec$suffix)))
  }
  if (!setequal(colnames(tbl), colnames(side)))
    stop("'tbl' no longer has the columns of the survey table, so the \"",
         sidecar, "\" records cannot be appended; pass the tbl as returned ",
         "by get_pumf().", call. = FALSE)
  dplyr::union_all(tbl, dplyr::select(side, dplyr::all_of(colnames(tbl))))
}

#' @rdname pumf_sidecar
#' @export
list_pumf_sidecars <- function(tbl) {
  loc <- .pumf_sidecar_locate(tbl, "list_pumf_sidecars()")
  out <- tibble::tibble(sidecar = character(0L), table = character(0L),
                        kind = character(0L), n_rows = numeric(0L),
                        description = character(0L))
  if (.is_longitudinal(loc$prov$series)) return(out)
  for (nm in names(.pumf_sidecars)) {
    spec       <- .pumf_sidecars[[nm]]
    side_table <- spec$table(loc$table_name)
    if (!DBI::dbExistsTable(loc$con, side_table)) next
    n <- DBI::dbGetQuery(loc$con, sprintf(
      "SELECT count(*) AS n FROM %s",
      as.character(DBI::dbQuoteIdentifier(loc$con, side_table))))$n
    out <- tibble::add_row(out, sidecar = nm, table = side_table,
                           kind = spec$kind, n_rows = as.numeric(n),
                           description = spec$description)
  }
  out
}

# The sidecar tables: record-level tables Stage 3 writes beside a survey
# table, keyed by pumf_row_id.  `table` maps the survey table's name to the
# sidecar's; `kind` is "values" (columns annotating the survey table's
# records, joined with `suffix`) or "records" (records left out of it, with
# its columns); `absent` completes the error for a table without it.  A new
# sidecar is one entry here plus the Stage 3 code that writes its table (and
# its name in .remove_pumf_lang(), via .pumf_sidecar_tables()).
.pumf_sidecars <- list(
  sentinels = list(
    table       = function(t) .sentinel_table_name(t),
    kind        = "values",
    suffix      = "_sentinel",
    description = paste("Labels of the sentinel codes (not applicable, not",
                        "stated, ...) that are NA in the survey table"),
    absent      = paste("the database was built by an earlier canpumf",
                        "version. Rebuild it with get_pumf(..., refresh = TRUE).")),
  removed = list(
    table       = function(t) .removed_table_name(t),
    kind        = "records",
    description = paste("Records the producer flags for removal, left out",
                        "of the survey table"),
    absent      = "its registry entry sets no records aside.")
)

# The names of every sidecar table of the survey table(s) `table_name`.
.pumf_sidecar_tables <- function(table_name)
  unlist(lapply(.pumf_sidecars, function(s) s$table(table_name)),
         use.names = FALSE)

# Connection, provenance and survey-table name of a get_pumf() tbl.
.pumf_sidecar_locate <- function(tbl, fn) {
  if (!inherits(tbl, "tbl_sql"))
    stop("'tbl' must be a lazy tbl returned by get_pumf().", call. = FALSE)
  prov <- .pumf_lookup_con(tbl$src$con)
  if (is.null(prov))
    stop("'tbl' has no pumf provenance. Was it created by get_pumf()?",
         call. = FALSE)
  list(con = tbl$src$con, prov = prov,
       table_name = if (.is_longitudinal(prov$series)) NA_character_
                    else .pumf_table_name(prov$series, prov$version,
                                          prov$lang %||% "eng", prov$module))
}
