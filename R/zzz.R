# Package-level session state and cache-path advisories.
#
# canpumf persists downloads, parsed metadata, and built DuckDB files under
# getOption("canpumf.cache_path"). When that option is unset everything falls
# back to tempdir() and is silently discarded at the end of the session. These
# helpers make the unset state discoverable: a one-time startup message, a
# once-per-session warning on the first actual download, and a once-per-session
# warning from the catalogue scraper (which also loses its cross-session cache).

# Tracks once-per-session advisories so each fires at most once per R session.
.pumf_session_state <- new.env(parent = emptyenv())

# Whether a durable cache path has been configured.
.pumf_cache_path_set <- function() {
  !is.null(getOption("canpumf.cache_path"))
}

# Shared, copy-pasteable instructions for setting the cache path. `prefix`
# leads the block (e.g. "No persistent cache is configured"); the body explains
# how to set the option for this session and permanently via .Rprofile.
.pumf_cache_path_hint <- function(prefix) {
  paste0(
    prefix, ".\n",
    "Downloaded data is stored in tempdir() and discarded when this R session ",
    "ends, so it will be re-downloaded next time.\n",
    "To persist data across sessions, set a cache directory:\n",
    '  options(canpumf.cache_path = "~/canpumf_cache")\n',
    "Add that line to your .Rprofile to make it permanent.")
}

# Emit `expr` (a message()/warning() call) at most once per session, keyed by
# `id`. Subsequent calls with the same id are no-ops.
.pumf_once_per_session <- function(id, expr) {
  if (isTRUE(.pumf_session_state[[id]])) return(invisible(FALSE))
  .pumf_session_state[[id]] <- TRUE
  force(expr)
  invisible(TRUE)
}

# Once-per-session warning fired the first time a download is about to happen
# with no durable cache path configured.
.pumf_warn_cache_path_on_download <- function() {
  if (.pumf_cache_path_set()) return(invisible(FALSE))
  .pumf_once_per_session(
    "download_cache_warned",
    warning(.pumf_cache_path_hint(
      "canpumf.cache_path is not set; downloading to a temporary directory"),
      call. = FALSE))
}

.onAttach <- function(libname, pkgname) {
  if (.pumf_cache_path_set()) return(invisible())
  packageStartupMessage(.pumf_cache_path_hint(
    "canpumf.cache_path is not set"))
}
