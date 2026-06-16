# Every manual override in the registry must be explicitly verified against
# the PDF documentation and recorded in override_verification.csv.  The
# workflow for verifying (and recording) overrides lives in
# tools/verify_overrides.R; see "Override verification workflow" in CLAUDE.md.

test_that("all registry manual overrides are recorded in the verification ledger", {
  ledger <- utils::read.csv(test_path("override_verification.csv"),
                            colClasses = "character")
  overrides <- enumerate_registry_overrides()

  unrecorded <- setdiff(override_key(overrides), override_key(ledger))
  expect_equal(unrecorded, character(0),
               label = "registry overrides missing from override_verification.csv")

  stale <- setdiff(override_key(ledger), override_key(overrides))
  expect_equal(stale, character(0),
               label = "ledger rows without a matching registry override")
})

test_that("no override has an unresolved verification status", {
  ledger <- utils::read.csv(test_path("override_verification.csv"),
                            colClasses = "character")

  bad <- ledger[!ledger$status %in% c("confirmed", "unverifiable"), ]
  expect_equal(override_key(bad), character(0),
               label = "overrides with pending/mismatch status")

  confirmed <- ledger[ledger$status == "confirmed", ]
  incomplete <- confirmed[confirmed$source_file == "" | confirmed$checked_date == "", ]
  expect_equal(override_key(incomplete), character(0),
               label = "confirmed rows missing source_file or checked_date")
})
