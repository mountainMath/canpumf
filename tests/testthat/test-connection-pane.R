# Tests for .duckdb_connect_quiet(): transient internal DuckDB connections must
# never register in the RStudio Connections pane.  Registering short-lived
# connections (status checks, write phases) and tearing them down moments later
# — while another connection to the same database file is still open — is what
# triggered RStudio's "Error in dbSendQuery(conn, statement, ...)" pane popups
# for get_pumf("LFS") (.lfs_status() opened a transient connection sharing the
# pane identity of the returned one).

# duckdb notifies getOption("connectionObserver") when the pane options are on;
# we install a counting observer to assert whether registration happened.
with_observer <- function(code) {
  calls <- new.env()
  calls$opened <- 0L
  old <- options(
    connectionObserver = list(
      connectionOpened  = function(...) calls$opened <- calls$opened + 1L,
      connectionClosed  = function(...) invisible(NULL),
      connectionUpdated = function(...) invisible(NULL)
    ),
    # Force pane registration ON so a non-quiet connection *would* register.
    duckdb.enable_rstudio_connection_pane = TRUE,
    duckdb.force_rstudio_connection_pane  = TRUE
  )
  on.exit(options(old), add = TRUE)
  force(code)
  calls$opened
}

test_that(".duckdb_connect_quiet does not register in the Connections pane", {
  db <- tempfile(fileext = ".duckdb")
  n <- with_observer({
    con <- .duckdb_connect_quiet(db, read_only = FALSE)
    DBI::dbExecute(con, "CREATE TABLE t AS SELECT 1 AS x")
    DBI::dbDisconnect(con, shutdown = TRUE)
  })
  expect_identical(n, 0L)
})

test_that("a plain dbConnect (final returned connection) still registers", {
  db <- tempfile(fileext = ".duckdb")
  # Seed the file with the quiet helper so the plain open has a table to see.
  con0 <- .duckdb_connect_quiet(db, read_only = FALSE)
  DBI::dbExecute(con0, "CREATE TABLE t AS SELECT 1 AS x")
  DBI::dbDisconnect(con0, shutdown = TRUE)

  n <- with_observer({
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db, read_only = TRUE)
    DBI::dbDisconnect(con, shutdown = TRUE)
  })
  expect_gte(n, 1L)
})

test_that(".duckdb_connect_quiet restores the pane options afterwards", {
  db <- tempfile(fileext = ".duckdb")
  # A no-op observer must be present whenever the pane options are on, otherwise
  # duckdb's own open/close callbacks have nothing to notify.
  old <- options(
    connectionObserver = list(
      connectionOpened  = function(...) invisible(NULL),
      connectionClosed  = function(...) invisible(NULL),
      connectionUpdated = function(...) invisible(NULL)
    ),
    duckdb.enable_rstudio_connection_pane = TRUE,
    duckdb.force_rstudio_connection_pane  = TRUE)
  on.exit(options(old), add = TRUE)
  con <- .duckdb_connect_quiet(db, read_only = FALSE)
  DBI::dbDisconnect(con, shutdown = TRUE)
  expect_true(getOption("duckdb.enable_rstudio_connection_pane"))
  expect_true(getOption("duckdb.force_rstudio_connection_pane"))
})
