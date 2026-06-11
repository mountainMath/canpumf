# devtools::test() runs in a callr subprocess that does not source ~/.Rprofile,
# so options() set there (e.g. canpumf.cache_path) are invisible to the test
# runner.  Read the path from the CANPUMF_CACHE_PATH environment variable as a
# fallback; set that in ~/.Renviron, which callr does source.
if (identical(getOption("canpumf.cache_path", ""), "")) {
  env_path <- Sys.getenv("CANPUMF_CACHE_PATH", unset = "")
  if (nzchar(env_path))
    options(canpumf.cache_path = env_path)
}
