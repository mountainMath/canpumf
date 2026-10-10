# Integration tests for the Canadian Century Research Infrastructure (CCRI)
# 1911 census sample from Borealis (ODESI).  The data (a 60 MB csv.gz) is never
# downloaded here: the tests run against a copy already in the user's canpumf
# cache and skip otherwise.
#
# The file exercises most of the CCRI-specific repairs at once: fifteen records
# broken by a line break, two CP850 columns in a CP1252 file, missing codes
# written into the text columns, measures mixing amounts and nine-digit codes,
# and an English-only ODESI SAS program whose variable labels are sentences.

.ccri_vdir <- function() {
  file.path(getOption("canpumf.cache_path", ""), "CCRI", "1911")
}

.ccri_extracted <- function() canpumf:::.version_is_extracted(.ccri_vdir())

# Stage 2 + Stage 3 (eng, then fra into the same file) in a temp DuckDB.
# Returns the database path and the conditions raised on the way.
.ccri_build <- function() {
  reg   <- canpumf:::pumf_registry_lookup("CCRI", "1911")
  tmp   <- tempfile(fileext = ".duckdb")
  warns <- character(0L)
  msgs  <- character(0L)
  withCallingHandlers(
    {
      canpumf:::pumf_parse_metadata(.ccri_vdir(),
                                     layout_mask       = reg$layout_mask,
                                     metadata_encoding = reg$metadata_encoding,
                                     refresh           = TRUE,
                                     file_mask         = reg$file_mask)
      canpumf:::pumf_build_duckdb(.ccri_vdir(), "CCRI", "1911", lang = "eng",
                                   db_path = tmp, refresh = TRUE)
      canpumf:::pumf_build_duckdb(.ccri_vdir(), "CCRI", "1911", lang = "fra",
                                   db_path = tmp)
    },
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  list(db_path = tmp, warns = warns, msgs = msgs)
}

test_that("CCRI 1911: registry entry resolves to the Borealis dataset", {
  reg <- canpumf:::pumf_registry_lookup("CCRI", "1911")
  expect_equal(reg$borealis$doi, "doi:10.5683/SP3/MDTWGJ")
  expect_equal(reg$data_encoding, "CP1252")
  fx <- reg$data_fixups
  expect_true(isTRUE(fx$rejoin_split_records))
  expect_true(isTRUE(fx$labels_as_description))
  expect_true(isTRUE(fx$keep_unlabelled_codes))
  expect_equal(names(fx$column_encoding), "CP850")
  expect_length(fx$text_missing_codes, 17L)
  expect_setequal(names(fx$sentinel_labels), fx$text_missing_codes)
  expect_equal(length(fx$labels_supplement), 101L)
  # the measures take both the force_numeric and the missing_supplement;
  # AGE_AMOUNT and MONTH_OF_BIRTH have no card format, so only the range
  expect_setequal(setdiff(names(fx$missing_supplement), fx$force_numeric),
                  c("AGE_AMOUNT", "MONTH_OF_BIRTH"))
})

test_that("CCRI 1911: full pipeline, both languages", {
  skip_if_not(.ccri_extracted(), "CCRI 1911 not in cache")

  b <- .ccri_build()
  on.exit(unlink(b$db_path), add = TRUE)
  expect_identical(b$warns, character(0L),
                   label = "CCRI 1911: should have no warnings")
  # the split records are rejoined in each language's read
  expect_equal(sum(grepl("^Rejoined 15 record", b$msgs)), 2L)

  con <- canpumf:::.duckdb_connect(b$db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE, after = FALSE)
  q <- function(sql) DBI::dbGetQuery(con, sql)

  expect_setequal(DBI::dbListTables(con),
                  c("eng", "fra", "pumf_sentinels_eng", "pumf_sentinels_fra",
                    "pumf_build_info"))

  # ---- Metadata: 101 variables; short labels from the registry, the ODESI
  # sentences as descriptions (English only) ---------------------------------
  meta <- canpumf:::read_metadata(file.path(.ccri_vdir(), "metadata"))
  expect_equal(nrow(meta$variables), 101L)
  expect_true(all(is.na(meta$variables$label_en)))
  expect_false(any(is.na(meta$variables$description_en)))
  expect_true(all(is.na(meta$variables$description_fr)))
  expect_false(any(grepl("\\w\\?s\\b", meta$variables$description_en)))  # individual?s
  vars <- canpumf:::.pumf_apply_labels_supplement(
    meta$variables, canpumf:::pumf_registry_lookup("CCRI", "1911"))
  expect_false(any(is.na(vars$label_en) | !nzchar(vars$label_en)))
  expect_false(any(is.na(vars$label_fr) | !nzchar(vars$label_fr)))
  expect_gt(nrow(meta$codes), 6000L)

  for (lang in c("eng", "fra")) {
    sc <- paste0("pumf_sentinels_", lang)
    expect_equal(q(sprintf("SELECT count(*) n FROM %s", lang))$n, 371373)

    # ---- Schema: ids character, measures numeric, coded variables ENUM ------
    cols <- q(sprintf(
      "SELECT column_name, data_type FROM information_schema.columns
       WHERE table_name = '%s' ORDER BY ordinal_position", lang))
    expect_equal(cols$column_name, c("pumf_row_id", vars$name))
    type <- stats::setNames(cols$data_type, cols$column_name)
    expect_equal(unname(type[c("DWELLING_ID", "DERIVED_PERSON_NUM_IN_HOUSEHOLD",
                               "LAST_NAME")]), rep("VARCHAR", 3L))
    expect_equal(unname(type[c("AGE_AMOUNT", "HOURS_WORKED_CHIEF_OCC",
                               "YEAR_OF_NATURALIZATION", "YEAR_OF_BIRTH")]),
                 rep("DOUBLE", 4L))
    expect_equal(sum(cols$data_type == "DOUBLE"), 19L)
    expect_equal(sum(grepl("^ENUM", cols$data_type)), 28L)

    # ---- str_pad: sequence numbers padded to one width per column ----------
    hh <- q(sprintf(
      "SELECT DERIVED_HOUSEHOLD_ID_IN_DWELLING h, count(*) n FROM %s
       GROUP BY 1 ORDER BY 1", lang))
    expect_equal(hh$h[1:3], c("01", "02", "03"))
    expect_equal(hh$n[1:2], c(354287, 12344))
    expect_equal(nrow(hh), 23L)
    expect_equal(q(sprintf(
      "SELECT count(DISTINCT length(DERIVED_PERSON_NUM_IN_HOUSEHOLD)) n,
              count(DISTINCT length(DERIVED_SURNAME_NUMBER)) m
       FROM %s", lang)), data.frame(n = 1, m = 1))

    # ---- column_encoding: the CP850 columns decode their accents (0x82 is
    # "é" there and an undefined byte in CP1252); the names are CP1252 ------
    expect_gt(q(sprintf(
      "SELECT count(*) n FROM %s
       WHERE OCCUPATION_CHIEF_OCC_IND_CL LIKE '%%é%%'", lang))$n, 400)
    # the one value written in CP1252 instead ('caf\xe9 prop') is left as the
    # CP850 reading of its byte
    expect_equal(q(sprintf(
      "SELECT count(*) n FROM %s
       WHERE OCCUPATION_CHIEF_OCC_IND_CL = 'cafÚ prop'", lang))$n, 1)
    expect_gt(q(sprintf(
      "SELECT count(*) n FROM %s WHERE PLACE_OF_EMPLOYMENT_CL LIKE '%%é%%'",
      lang))$n, 200)
    expect_gt(q(sprintf(
      "SELECT count(*) n FROM %s WHERE LAST_NAME LIKE '%%é%%'", lang))$n, 5000)
    expect_equal(q(sprintf(
      "SELECT count(*) n FROM %s WHERE LAST_NAME LIKE '%%Ã%%'
          OR OCCUPATION_CHIEF_OCC_IND_CL LIKE '%%Ã%%'", lang))$n, 0)

    # ---- Mixed measures: amounts stay, the nine-digit codes go to the sidecar
    expect_equal(q(sprintf(
      "SELECT max(HOURS_WORKED_CHIEF_OCC) m FROM %s", lang))$m < 90000001, TRUE)
    expect_equal(q(sprintf(
      "SELECT count(*) n FROM %s WHERE IN_SCHOOL_MONTHS_AMOUNT = 10", lang))$n,
      27602)
    # ---- text_missing_codes: no code is left in a text column ---------------
    expect_equal(q(sprintf(
      "SELECT count(*) n FROM %s WHERE LAST_NAME LIKE '9999900%%'
          OR DERIVED_SURNAME_NUMBER LIKE '9999900%%'", lang))$n, 0)

    # ---- Sidecar: one column per variable with a blanked value --------------
    scols <- q(sprintf(
      "SELECT column_name FROM information_schema.columns
       WHERE table_name = '%s'", sc))$column_name
    expect_length(scols, 42L)
    expect_true(all(c("DERIVED_SURNAME_NUMBER", "TITLE", "HOURS_WORKED_CHIEF_OCC",
                      "YEAR_OF_NATURALIZATION", "AGE_AMOUNT") %in% scols))
    expect_equal(q(sprintf(
      "SELECT count(*) n FROM %s WHERE TITLE IS NOT NULL", sc))$n, 371373)
  }

  # ---- Labels: English levels, French where canpumf translates the missing
  # codes; the card's English value labels otherwise ---------------------------
  sex <- q("SELECT CAST(SEX AS VARCHAR) s, count(*) n FROM eng
            GROUP BY 1 ORDER BY n DESC")
  expect_equal(sex$s[1:2], c("Male", "Female"))
  expect_equal(sex$n[1:2], c(198476, 172188))
  expect_equal(sex$n[sex$s == "Missing -- Mandatory Field"], 300)
  sex_fr <- q("SELECT CAST(SEX AS VARCHAR) s, count(*) n FROM fra
               GROUP BY 1 ORDER BY n DESC")
  expect_equal(sex_fr$s[1:2], c("Male", "Female"))
  expect_equal(sex_fr$n[sex_fr$s == "Manquant -- champ obligatoire"], 300)

  hrs <- q("SELECT CAST(HOURS_WORKED_CHIEF_OCC AS VARCHAR) s, count(*) n
            FROM pumf_sentinels_eng WHERE HOURS_WORKED_CHIEF_OCC IS NOT NULL
            GROUP BY 1")
  expect_equal(hrs$n[hrs$s == "Blank"], 290899)
  expect_equal(hrs$n[hrs$s == "Full Time"], 10)
  # the card's 900000016 (a typo of 90000016) is still "Retired"
  wk <- q("SELECT CAST(WEEKS_WORKING_CHIEF_OCC AS VARCHAR) s, count(*) n
           FROM pumf_sentinels_eng WHERE WEEKS_WORKING_CHIEF_OCC IS NOT NULL
           GROUP BY 1")
  expect_equal(wk$n[wk$s == "Retired"], 2)
  # missing_codes combined with the measure range
  nat <- q("SELECT CAST(YEAR_OF_NATURALIZATION AS VARCHAR) s, count(*) n
            FROM pumf_sentinels_eng WHERE YEAR_OF_NATURALIZATION IS NOT NULL
            GROUP BY 1")
  expect_equal(nat$n[nat$s == "Naturalized"], 11816)
  expect_equal(nat$n[nat$s == "Blank"], 345736)
  # the values left are years, plus the source's own stray small numbers
  expect_equal(q("SELECT count(*) n FROM eng WHERE YEAR_OF_NATURALIZATION
                  BETWEEN 1800 AND 1911")$n, 4923)
  nat_fr <- q("SELECT CAST(YEAR_OF_NATURALIZATION AS VARCHAR) s, count(*) n
               FROM pumf_sentinels_fra WHERE YEAR_OF_NATURALIZATION IS NOT NULL
               GROUP BY 1")
  expect_equal(nat_fr$n[nat_fr$s == "En blanc"], 345736)

  # ---- keep_unlabelled_codes: an undocumented code stays, as itself --------
  occ <- q("SELECT CAST(OCC3B AS VARCHAR) s, count(*) n FROM eng
            WHERE OCC3B IS NOT NULL GROUP BY 1 ORDER BY n DESC")
  expect_equal(occ$n[occ$s == "Farmer, N. S."], 40340)
  expect_equal(occ$n[occ$s == "100"], 4552)
  expect_equal(occ$n[occ$s == "Blank"], 7368)
  expect_equal(occ$n[occ$s == "Blank"], 7368)

  # The build stamp covers both tables.
  expect_setequal(q('SELECT "table" FROM pumf_build_info')$table,
                  c("eng", "fra"))
})

test_that("CCRI 1911: dictionary, descriptions and top codes from the cache", {
  skip_if_not(.ccri_extracted(), "CCRI 1911 not in cache")
  skip_if_not(file.exists(file.path(.ccri_vdir(), "CCRI_1911.duckdb")),
              "CCRI 1911 not built")

  tbl <- get_pumf("CCRI", "1911")
  on.exit(close_pumf(tbl), add = TRUE)
  labs <- pumf_dictionary(tbl, what = "variables")
  expect_named(labs, c("name", "val", "label_en", "label_fr", "description_en",
                       "description_fr", "applied_as"))
  expect_equal(nrow(labs), 101L)
  expect_false(any(is.na(labs$label_en) | is.na(labs$label_fr) |
                     is.na(labs$description_en)))

  top <- pumf_dictionary(tbl, what = "topcodes")
  expect_setequal(unique(top$name), "IN_SCHOOL_MONTHS_AMOUNT")

  d <- pumf_dictionary(tbl)
  expect_equal(d$label_fr[which(d$name == "SEX" & d$val == "99999001")],
               "En blanc")
  expect_true(any(is.na(d$name) & d$val == "99999001", na.rm = TRUE))
})
