# Integration tests for The Canadian Peoples (TCP) complete-count census, 1881.
# The data (a 1.1 GB CSV from Borealis) is never downloaded here: the tests run
# against a copy already in the user's canpumf cache and skip otherwise.
#
# The table has 4.3 million rows, so nothing is collected: every check is a
# query on the freshly built DuckDB, which also keeps the test's memory at the
# level of the DuckDB-native build it exercises.

.tcp_vdir <- function() {
  file.path(getOption("canpumf.cache_path", ""), "TCP", "1881")
}

.tcp_extracted <- function() canpumf:::.version_is_extracted(.tcp_vdir())

# Stage 2 + Stage 3 (eng, then fra into the same file) in a temp DuckDB.
# Returns the database path and the warnings raised on the way.
.tcp_build <- function() {
  reg   <- canpumf:::pumf_registry_lookup("TCP", "1881")
  tmp   <- tempfile(fileext = ".duckdb")
  warns <- character(0L)
  withCallingHandlers(
    {
      canpumf:::pumf_parse_metadata(.tcp_vdir(),
                                     layout_mask       = reg$layout_mask,
                                     metadata_encoding = reg$metadata_encoding,
                                     refresh           = TRUE,
                                     file_mask         = reg$file_mask)
      canpumf:::pumf_build_duckdb(.tcp_vdir(), "TCP", "1881", lang = "eng",
                                   db_path = tmp, refresh = TRUE)
      canpumf:::pumf_build_duckdb(.tcp_vdir(), "TCP", "1881", lang = "fra",
                                   db_path = tmp)
    },
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(db_path = tmp, warns = warns)
}

test_that("TCP 1881: registry entry resolves to the Borealis dataset", {
  reg <- canpumf:::pumf_registry_lookup("TCP", "1881")
  expect_equal(reg$borealis$doi, "doi:10.5683/SP3/FXZEVO")
  expect_equal(reg$csv_reader, "duckdb")
  expect_true(isTRUE(reg$data_fixups$fix_mojibake))
  expect_equal(reg$data_fixups$removed_records,
               list(var = "REMOVE_TCP", values = "1"))
})

test_that("TCP 1881: full pipeline, both languages, no warnings", {
  skip_if_not(.tcp_extracted(), "TCP 1881 not in cache")

  b <- .tcp_build()
  on.exit(unlink(b$db_path), add = TRUE)
  expect_identical(b$warns, character(0L),
                   label = "TCP 1881: should have no warnings")

  con <- canpumf:::.duckdb_connect(b$db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE, after = FALSE)
  q <- function(sql) DBI::dbGetQuery(con, sql)

  expect_true(all(c("eng", "fra", "pumf_removed_eng", "pumf_removed_fra",
                    "pumf_sentinels_eng", "pumf_sentinels_fra") %in%
                    DBI::dbListTables(con)))
  # The build's temporary stage and map tables are gone.
  expect_false(any(grepl("^pumf_(csv_stage|map_|smap_|mj_|mojibake)",
                         DBI::dbListTables(con))))

  # ---- Metadata: 52 variables, labelled in both languages by the registry ---
  meta <- canpumf:::read_metadata(file.path(.tcp_vdir(), "metadata"))
  vars <- canpumf:::.pumf_apply_labels_supplement(
    meta$variables, canpumf:::pumf_registry_lookup("TCP", "1881"))
  expect_equal(nrow(vars), 52L)
  expect_false(any(is.na(vars$label_en) | !nzchar(vars$label_en)))
  expect_false(any(is.na(vars$label_fr) | !nzchar(vars$label_fr)))
  expect_gt(nrow(meta$codes), 1000L)

  for (lang in c("eng", "fra")) {
    rem <- paste0("pumf_removed_", lang)

    # ---- Records: 4,277,810 in the file, 1,137 of them flagged for removal --
    expect_equal(q(sprintf("SELECT count(*) n FROM %s", lang))$n, 4276673)
    expect_equal(q(sprintf("SELECT count(*) n FROM %s", rem))$n, 1137)
    # One key over both tables: every record of the file is in exactly one.
    ids <- q(sprintf(
      "SELECT min(pumf_row_id) lo, max(pumf_row_id) hi, count(*) n,
              count(DISTINCT pumf_row_id) nd
       FROM (SELECT pumf_row_id FROM %s UNION ALL
             SELECT pumf_row_id FROM %s)", lang, rem))
    expect_equal(c(ids$lo, ids$hi, ids$n, ids$nd),
                 c(1, 4277810, 4277810, 4277810))
    expect_equal(q(sprintf(
      "SELECT count(*) n FROM %s WHERE CAST(REMOVE_TCP AS VARCHAR) <>
         'Record to keep'", lang))$n, 0)
    why <- q(sprintf(
      "SELECT CAST(REMOVE_WHY_TCP AS VARCHAR) w, count(*) n FROM %s
       GROUP BY 1 ORDER BY 1", rem))
    expect_equal(why$w, c("Crossed row", "Other reason"))
    expect_equal(why$n, c(1099, 38))

    # ---- Schema: same columns in both tables; ages numeric, ids character ---
    cols <- q(sprintf(
      "SELECT column_name, data_type FROM information_schema.columns
       WHERE table_name = '%s' ORDER BY ordinal_position", lang))
    rcols <- q(sprintf(
      "SELECT column_name, data_type FROM information_schema.columns
       WHERE table_name = '%s' ORDER BY ordinal_position", rem))
    expect_equal(cols$column_name, c("pumf_row_id", vars$name))
    expect_equal(rcols$column_name, cols$column_name)
    type <- stats::setNames(cols$data_type, cols$column_name)
    expect_equal(unname(type[c("pumf_row_id", "AGE", "AGEMONTH")]),
                 c("BIGINT", "DOUBLE", "DOUBLE"))
    expect_equal(unname(type[c("SERIAL", "UNIQUE_IDENTIFIER", "NAMLAST")]),
                 rep("VARCHAR", 3L))
    expect_equal(sum(grepl("^ENUM", cols$data_type)), 15L)

    age <- q(sprintf(
      "SELECT min(AGE) a0, max(AGE) a1, max(AGEMONTH) m1,
              count(DISTINCT SERIAL) hh FROM %s", lang))
    expect_equal(c(age$a0, age$a1, age$m1, age$hh), c(0, 116, 11, 802833))

    # ---- fix_mojibake: no double-encoded text is left -----------------------
    expect_equal(q(sprintf(
      "SELECT count(*) n FROM %s
       WHERE NAMLAST LIKE '%%Ã%%' OR NAMFRST LIKE '%%Ã%%'
          OR SDISTNAM LIKE '%%Ã%%' OR DISTNAM LIKE '%%Ã%%'
          OR DOCCUP LIKE '%%Ã%%'", lang))$n, 0)
    expect_gt(q(sprintf(
      "SELECT count(*) n FROM %s WHERE NAMLAST LIKE '%%é%%'", lang))$n, 0)

    # ---- keep_unlabelled_codes: an undocumented code stays, as itself -------
    expect_equal(q(sprintf(
      "SELECT count(*) n FROM %s WHERE CAST(DOCCUP_TCP AS VARCHAR) = '12110'",
      lang))$n, 2902)
    expect_equal(q(sprintf(
      "SELECT count(*) n FROM %s WHERE DOCCUP_TCP IS NULL
         AND DOCCUP IS NOT NULL", lang))$n, 0)

    # No value is blanked, so the sentinel table is empty.
    expect_equal(q(sprintf("SELECT count(*) n FROM pumf_sentinels_%s",
                           lang))$n, 0)
  }

  # ---- Language parity: the value labels are English-only, so the two tables
  # hold the same values; only the variable labels differ. -------------------
  expect_equal(q("SELECT count(*) n FROM (SELECT * FROM eng EXCEPT
                                           SELECT * FROM fra)")$n, 0)

  sex <- q("SELECT CAST(SEX AS VARCHAR) s, count(*) n FROM eng
            GROUP BY 1 ORDER BY 1")
  expect_equal(sex$s, c("Female", "Male", "Unknown"))
  expect_equal(sex$n, c(2110714, 2165754, 205))
  prov <- q("SELECT CAST(PROVINCE AS VARCHAR) p, count(*) n FROM eng
             GROUP BY 1 ORDER BY n DESC LIMIT 2")
  expect_equal(prov$p, c("Ontario", "Quebec"))
  expect_equal(prov$n, c(1923247, 1358126))

  # The build stamp covers both tables.
  expect_setequal(q('SELECT "table" FROM pumf_build_info')$table,
                  c("eng", "fra"))
})
