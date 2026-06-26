
# Used by parse_spss_split() to select one file from a split-SPSS directory.
find_unique_layout_file <- function(layout_path, pattern, path_or_pattern = NULL) {
  validate_path <- function(path) {
    if (length(path) == 0L) stop("Could not find layout file.")
    if (length(path) >  1L) stop("Found several layout files: ",
                                  paste(path, collapse = ", "),
                                  ".\nPlease further specify which layout file to use.")
    NULL
  }
  path <- path_or_pattern
  if (is.null(path)) {
    path <- dir(layout_path, pattern = "\\.sps$|\\.lay$")
    if (length(path) > 1L) path <- dir(layout_path, pattern = pattern)
    validate_path(path)
    path <- file.path(layout_path, path)
  } else {
    if (file.exists(file.path(layout_path, path))) path <- file.path(layout_path, path)
    if (!file.exists(path)) {
      paths <- dir(layout_path, pattern = "\\.sps$|\\.lay$")
      paths <- paths[grepl(path, paths)]
      if (length(paths) > 1L) paths <- paths[grepl(pattern, paths)]
      if (length(paths) > 1L) {
        pp <- paste0(path_or_pattern, "_", pattern)
        if (substr(pattern, 1L, 1L) == "_") pp <- paste0(path_or_pattern, pattern)
        paths <- paths[grepl(pp, paths)]
      }
      validate_path(paths)
      path <- if (grepl("\\.sps$", paths)) file.path(layout_path, paths) else layout_path
    }
  }
  path
}


# Low-level extractor: ditto on macOS with unzip fallbacks.
.unzip_impl <- function(path, exdir) {
  if (Sys.info()[['sysname']] == "Darwin") {
    # ditto does not support all ZIP compression variants (e.g. newer deflate
    # flavours used by StatCan since 2025).  Fall back to system unzip, then
    # to utils::unzip, if ditto exits non-zero.
    # system2() still runs via the shell and does not quote its args, so
    # shQuote() the path/exdir to handle spaces and quote characters safely.
    exit <- system2("ditto",
                    c("-x", "-k", "--sequesterRsrc", "--rsrc",
                      shQuote(path), shQuote(exdir)))
    if (exit != 0L) {
      message("ditto failed (exit ", exit, "); falling back to unzip.")
      exit2 <- system2("unzip", c("-o", shQuote(path), "-d", shQuote(exdir)),
                       stdout = FALSE)
      if (exit2 != 0L) utils::unzip(path, exdir = exdir)
    }
  } else {
    utils::unzip(path, exdir = exdir)
  }
}

robust_unzip <- function(path, exdir) {
  zip_name <- basename(path)

  # Detect naming collision: some ZIPs have a single top-level directory with
  # the same name as the archive (e.g. 2025-CSV.zip contains 2025-CSV.zip/*).
  # When the archive lives inside exdir, extracting would require creating a
  # directory at the same path as the zip file — which fails.
  #
  # Fix: extract to a temp sibling directory (same filesystem → atomic rename),
  # strip .zip from the colliding directory name, then move into exdir.
  top_entries   <- tryCatch(utils::unzip(path, list = TRUE)$Name,
                             error = function(e) character(0L))
  # Some StatCan zips store filenames in CP1252 without the UTF-8 flag,
  # so top_entries may contain bytes invalid in the UTF-8 locale.
  # useBytes=TRUE matches the ASCII "/" without attempting encoding
  # translation, silencing spurious "input string is invalid" warnings.
  top_dirs      <- unique(sub("/.*", "/", grep("/", top_entries,
                                               value    = TRUE,
                                               fixed    = TRUE,
                                               useBytes = TRUE),
                              useBytes = TRUE))
  has_collision <- paste0(zip_name, "/") %in% top_dirs

  if (has_collision) {
    tmp_dir <- paste0(exdir, "_unzip_tmp")
    dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    .unzip_impl(path, tmp_dir)

    # Rename the colliding directory: strip the .zip extension so it no longer
    # shadows the archive file (2025-CSV.zip/ → 2025-CSV/).
    safe_name <- sub("\\.zip$", "", zip_name, ignore.case = TRUE)
    from_col  <- file.path(tmp_dir, zip_name)
    if (file.exists(from_col))
      file.rename(from_col, file.path(tmp_dir, safe_name))

    # Move everything from the temp dir into exdir.
    for (item in list.files(tmp_dir, all.files = FALSE)) {
      dest <- file.path(exdir, item)
      if (!file.exists(dest))
        file.rename(file.path(tmp_dir, item), dest)
    }
  } else {
    .unzip_impl(path, exdir)
  }
  invisible(NULL)
}


# Signal a graceful, classed network error.  Callers that front a download
# (get_pumf_connection(), lfs_get_pumf()) catch `canpumf_network_error` and
# degrade to an informative message + NULL instead of erroring -- Statistics
# Canada being unreachable should not produce a hard error (CRAN policy for
# packages that use Internet resources).
.pumf_network_error <- function(message) {
  structure(
    class = c("canpumf_network_error", "error", "condition"),
    list(message = message, call = NULL))
}

# download.file() wrapper that converts any failure -- unreachable host, HTTP
# error, or a truncated/empty result -- into a canpumf_network_error condition.
.pumf_download <- function(url, destfile, ...) {
  status <- tryCatch(utils::download.file(url, destfile, ...),
                     error = function(e) 1L)
  ok <- identical(as.integer(status), 0L) &&
        file.exists(destfile) && file.info(destfile)$size > 0
  if (!ok) {
    if (file.exists(destfile)) unlink(destfile)   # drop a truncated/empty file
    stop(.pumf_network_error(paste0(
      "Statistics Canada is unreachable; could not download '", url, "'. ",
      "The server may be down or the file may have moved -- try again later.")))
  }
  invisible(0L)
}


# Open a DuckDB connection that never registers in the RStudio Connections pane.
#
# Used for the many short-lived internal connections (status checks, write
# phases, BSW edits, lock probes) that are opened and disconnected within a
# single call.  Registering these in the pane — and tearing them down moments
# later, often while another connection to the *same* database file is still
# open — is what triggers RStudio's "Error in dbSendQuery(conn, statement, ...)"
# pane popups: the pane observer enumerates objects on a handle that has already
# been shut down, or on a duplicate entry for the same database file.  Only the
# final connection returned to the user (via pumf_open_duckdb() / .lfs_open_tbl(),
# and the BSW read-only reopen) should ever appear in the pane; those honour
# getOption("canpumf.register_connection") via the option block in get_pumf().
#
# duckdb registers the connection synchronously inside dbConnect() when these
# options are enabled, so forcing them FALSE for the duration of the call
# suppresses registration entirely.  All other dbConnect arguments are passed
# through; the caller keeps its own disconnect / shutdown handling.
.duckdb_connect_quiet <- function(dbdir, read_only = FALSE, ...) {
  old <- options(duckdb.enable_rstudio_connection_pane = FALSE,
                 duckdb.force_rstudio_connection_pane  = FALSE)
  on.exit(options(old), add = TRUE)
  DBI::dbConnect(duckdb::duckdb(), dbdir = dbdir, read_only = read_only, ...)
}


#' @import dplyr
#' @importFrom stats setNames na.omit
#' @importFrom utils head
#' @import stringr
#' @import readr
#' @importFrom rlang .data
#' @importFrom rlang :=
#' @import duckplyr
#' @importFrom dbplyr sql_render
NULL

## quiets concerns of R CMD check re: NSE column names
if (getRversion() >= "4.1")
  utils::globalVariables(c(".", "SURVMNTH", "SURVYEAR", "SEX", "GENDER"))
