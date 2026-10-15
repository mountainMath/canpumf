
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
  # Primary extractor on every platform: zip::unzip() (uniform treatment).
  # utils::unzip() cannot extract StatCan zips whose entries carry accented
  # names stored in CP437/Latin-1 without the UTF-8 flag (e.g. SHS 2017's
  # "Data - Donnees/" folder): under a non-UTF-8 locale it errors with
  # "invalid multibyte string" (Windows) or silently fails to translate the
  # name to a wide string and drops the file (Linux).  zip::unzip() extracts
  # the stored bytes verbatim and is locale-agnostic on all platforms.
  if (tryCatch({ zip::unzip(path, exdir = exdir); TRUE },
               error = function(e) FALSE))
    return(invisible(NULL))

  # Fallback only for ZIP compression variants zip's bundled extractor cannot
  # handle (e.g. the newer deflate flavours StatCan has shipped since 2025).
  if (Sys.info()[['sysname']] == "Darwin") {
    # system2() runs via the shell without quoting its args, so shQuote() the
    # path/exdir to handle spaces and quote characters safely.  ditto first
    # (handles resource forks), then system unzip, then utils::unzip.
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
  invisible(NULL)
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
# The download runs with getOption("timeout") raised to at least `timeout`
# seconds (StatCan zips and Borealis bundles are large); the option is restored
# on exit.
.pumf_download <- function(url, destfile, ..., source = "Statistics Canada",
                           timeout = 600L) {
  old_timeout <- getOption("timeout")
  options(timeout = max(timeout, old_timeout))
  on.exit(options(timeout = old_timeout), add = TRUE)
  status <- tryCatch(utils::download.file(url, destfile, ...),
                     error = function(e) 1L)
  ok <- identical(as.integer(status), 0L) &&
        file.exists(destfile) && file.info(destfile)$size > 0
  if (!ok) {
    if (file.exists(destfile)) unlink(destfile)   # drop a truncated/empty file
    stop(.pumf_network_error(paste0(
      source, " is unreachable; could not download '", url, "'. ",
      "The server may be down or the file may have moved -- try again later.")))
  }
  invisible(0L)
}


# Open a DuckDB connection that never registers in the RStudio Connections pane.
#
# Used for the many short-lived internal connections (status checks, write
# phases, lock probes) that are opened and disconnected within a
# single call.  Registering these in the pane — and tearing them down moments
# later, often while another connection to the *same* database file is still
# open — is what triggers RStudio's "Error in dbSendQuery(conn, statement, ...)"
# pane popups: the pane observer enumerates objects on a handle that has already
# been shut down, or on a duplicate entry for the same database file.  Only the
# final connection returned to the user (via pumf_open_duckdb() / .long_open_tbl())
# should ever appear in the pane; those honour
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
  .duckdb_connect(dbdir, read_only = read_only, ...)
}

# Open a DuckDB file, whatever in-process instance of it already exists.
#
# duckdb keeps one instance per file and process, read-only or read-write as
# first opened.  Up to 1.5.5 a dbConnect() asking for the other mode was handed
# that instance and its read_only argument was ignored; from 1.5.6 it fails.
#   - read_only = TRUE while the process holds the file read-write (a
#     get_pumf_connection(), a get_pumf(read_only = FALSE) tbl): share that
#     instance, as before.  No lock is taken, the instance already exists.
#   - read_only = FALSE while it holds the file read-only (a get_pumf() tbl):
#     no write is possible until that tbl is closed, so say so.  Earlier duckdb
#     versions return a connection here that fails on its first write, which
#     .assert_duckdb_writable() detects.
# Registers in the RStudio Connections pane as a plain dbConnect() does; the
# internal short-lived connections use .duckdb_connect_quiet().
#
# Disconnecting: a transient read-only probe (a versions-table read, an
# existence check; .long_with_readonly_con(), .duckdb_table_exists()) ends
# with dbDisconnect(shutdown = FALSE), so that it never shuts down an
# instance the session shares with a user's open tbl; its own read-only
# instance is released with the connection and does not block a later
# read-write open.  A write phase and the connection handed back to the user
# (.long_close_tbl(), close_pumf()) shut down with shutdown = TRUE, which
# releases the file lock.
.duckdb_connect <- function(dbdir, read_only = FALSE, ...) {
  tryCatch(
    DBI::dbConnect(duckdb::duckdb(), dbdir = dbdir, read_only = read_only, ...),
    error = function(e) {
      if (!.is_duckdb_read_only_mismatch(e)) stop(e)
      if (!isTRUE(read_only)) .stop_duckdb_read_only_held(dbdir)
      DBI::dbConnect(duckdb::duckdb(), dbdir = dbdir, read_only = FALSE, ...)
    })
}

# TRUE when `e` is duckdb's (>= 1.5.6) refusal to open a file whose in-process
# instance was created with the other read_only setting.
.is_duckdb_read_only_mismatch <- function(e) {
  grepl("`read_only`.*can.t be applied to the database instance",
        conditionMessage(e))
}

# The error for a write attempted on a file the process holds open read-only.
.stop_duckdb_read_only_held <- function(db_path) {
  stop(structure(
    class = c("canpumf_read_only_held", "error", "condition"),
    list(message = paste0(
           "'", basename(db_path), "' is held open by a read-only connection ",
           "(e.g. a tbl from get_pumf()).\n",
           "Close it first with close_pumf(tbl) and then retry."),
         call = NULL)))
}


# ---- Small shared helpers ----------------------------------------------------

# SQL identifier / string literal quoting, vectorised, as plain character so
# the result can go through sprintf()/paste().  Every hand-built '"%s"' in a
# statement should be one of these.
.qid  <- function(con, x) as.character(DBI::dbQuoteIdentifier(con, x))
.qstr <- function(con, x) as.character(DBI::dbQuoteString(con, x))

# The label column of variables.csv / codes.csv for a language.
.pumf_label_col <- function(lang) if (lang == "eng") "label_en" else "label_fr"

# First element of `x` as a string, NA for NULL (JSON field extraction).
.chr1 <- function(x) if (is.null(x)) NA_character_ else as.character(x)[[1L]]

# Evaluate `expr`; when it raises the classed `canpumf_network_error` (offline,
# StatCan unreachable), print the message and return NULL so the caller can
# return invisibly instead of erroring.
.pumf_offline_null <- function(expr) {
  tryCatch(expr, canpumf_network_error = function(e) {
    message(conditionMessage(e)); NULL
  })
}

# TRUE for the series whose tables live in a shared, multi-version database:
# the longitudinal series and the LFS_TIMELINE view over them.  These have no
# per-version metadata directory, sidecars or build stamp.
.is_shared_series <- function(series) {
  .is_longitudinal(series) || identical(series, "LFS_TIMELINE")
}

# Read a data file with every column as character: fixed-width when `layout`
# (a data frame with name/start/end) is given, CSV otherwise.  CSV column
# names are upper-cased so they match the metadata; a fixed-width file takes
# its names from the layout.
.pumf_read_chr <- function(path, encoding, layout = NULL) {
  loc <- readr::locale(encoding = encoding)
  if (!is.null(layout)) {
    return(readr::read_fwf(
      path,
      col_positions  = readr::fwf_positions(layout$start, layout$end,
                                             col_names = layout$name),
      col_types      = readr::cols(.default = "c"),
      trim_ws        = TRUE,
      locale         = loc,
      show_col_types = FALSE))
  }
  data <- readr::read_csv(path, col_types = readr::cols(.default = "c"),
                          locale = loc, show_col_types = FALSE)
  names(data) <- toupper(names(data))
  data
}

# Non-empty version directories of a series in the cache (the directory names),
# optionally restricted to `pattern`.
.pumf_version_dirs <- function(cache_path, series, pattern = NULL,
                               non_empty = TRUE) {
  dir <- file.path(cache_path, series)
  if (!dir.exists(dir)) return(character(0L))
  dirs <- list.dirs(dir, full.names = FALSE, recursive = FALSE)
  if (!is.null(pattern)) dirs <- dirs[grepl(pattern, dirs)]
  if (non_empty)
    dirs <- dirs[vapply(dirs, function(d)
      length(list.files(file.path(dir, d))) > 0L, logical(1L))]
  dirs[nchar(dirs) > 0L]
}

#' @import dplyr
#' @importFrom stats setNames na.omit
#' @importFrom utils head
#' @import stringr
#' @import readr
#' @importFrom rlang .data
#' @importFrom rlang :=
#' @importFrom dbplyr sql_render
NULL

## quiets concerns of R CMD check re: NSE column names
if (getRversion() >= "4.1")
  utils::globalVariables(c(".", "SURVMNTH", "SURVYEAR", "SEX", "GENDER"))

# ---- Compressed data files ---------------------------------------------------
# A downloaded data file stays compressed in the cache: readr, readLines() and
# DuckDB's read_csv() all read a gzip file directly, so nothing ever needs the
# uncompressed copy on disk.  .borealis_download_dataset() therefore stores a
# CSV data file as `<name>.csv.gz` (see .borealis_download_csv_gz()).

# TRUE for a CSV path, plain or gzip-compressed.
.is_csv_path <- function(path) {
  grepl("\\.csv(\\.gz)?$", path, ignore.case = TRUE)
}

# Copy the binary connection `inp` into the gzip file `dest` and close it.
# Streams in chunks, so the memory use does not depend on the size.  Returns
# the number of (uncompressed) bytes copied; a failed copy leaves no `dest`.
.stream_to_gzip <- function(inp, dest, chunk = 16e6) {
  done <- FALSE
  out  <- gzfile(dest, "wb", compression = 6L)
  on.exit({
    close(inp)
    close(out)
    if (!done) unlink(dest)
  }, add = TRUE)
  n <- 0
  repeat {
    buf <- readBin(inp, "raw", n = chunk)
    if (length(buf) == 0L) break
    writeBin(buf, out)
    n <- n + length(buf)
  }
  done <- TRUE
  n
}

# Compress `path` to `<path>.gz` and remove the original.  Returns the new path.
.gzip_file <- function(path, chunk = 16e6) {
  dest <- paste0(path, ".gz")
  .stream_to_gzip(file(path, "rb"), dest, chunk = chunk)
  unlink(path)
  dest
}

# Recompress the entry `entry` of the archive `zip` as the gzip file `dest`,
# without the uncompressed file ever being on disk.  Returns the number of
# uncompressed bytes, which the caller compares with the expected size (R's
# unz() truncates an entry of 4 GB or more).
.zip_entry_to_gzip <- function(zip, entry, dest, chunk = 16e6) {
  .stream_to_gzip(unz(zip, entry, "rb"), dest, chunk = chunk)
}

# Size in bytes of the data a file holds: for a gzip file the uncompressed
# size recorded in its last four bytes (modulo 4 GB, which is what gzip
# stores), otherwise the file size.
.pumf_data_file_size <- function(path) {
  size <- file.size(path)
  if (!grepl("\\.gz$", path, ignore.case = TRUE) || is.na(size) || size < 18)
    return(size)
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  seek(con, size - 4)
  b <- as.numeric(readBin(con, "raw", n = 4L))
  sum(b * 256^(0:3))
}
