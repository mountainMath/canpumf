# devtools::test() runs in a callr subprocess that does not source ~/.Rprofile,
# so options() set there (e.g. canpumf.cache_path) are invisible to the test
# runner.  Read the path from the CANPUMF_CACHE_PATH environment variable as a
# fallback; set that in ~/.Renviron, which callr does source.
if (identical(getOption("canpumf.cache_path", ""), "")) {
  env_path <- Sys.getenv("CANPUMF_CACHE_PATH", unset = "")
  if (nzchar(env_path))
    options(canpumf.cache_path = env_path)
}

# Keep test DuckDB connections out of the RStudio/Positron Connections pane.
# When tests run in-process in an interactive session that has the pane enabled
# (e.g. via ~/.Rprofile), the pane holds opened connections alive, so their file
# locks are never released -- a later test opening the same .duckdb read-write
# then fails with a "Conflicting lock is held" IO error.  Forcing the pane off
# for the whole suite ensures connections are released as soon as the tests
# disconnect them.
options(duckdb.enable_rstudio_connection_pane = FALSE,
        duckdb.force_rstudio_connection_pane  = FALSE)
