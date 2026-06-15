library(testthat)
library(canpumf)

# The test suite depends on Statistics Canada servers (downloads) and a
# configured local data cache.  StatCan servers are frequently unreachable, so
# running the tests on CRAN would fail intermittently.  Run them only when
# NOT_CRAN is "true" -- set by devtools/testthat locally and by the r-lib
# GitHub Actions check, and unset on CRAN.  (devtools::test() runs the tests
# directly and is unaffected by this guard.)
if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  test_check("canpumf")
}
