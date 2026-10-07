# Tests for pumf_dictionary() and pumf_translate() (R/translate.R) on the
# synthetic FAKE survey (helper-e2e.R).  No network, no cache.

.fake_tbl <- function(tmp, lang = "eng") {
  make_e2e_version_dir(tmp)
  suppressMessages(get_pumf("FAKE", "2099", lang = lang, cache_path = tmp))
}

# ============================================================
# pumf_dictionary()
# ============================================================

test_that("pumf_dictionary: variable and code rows in both languages", {
  tmp <- withr::local_tempdir()
  tbl <- .fake_tbl(tmp)
  on.exit(close_pumf(tbl))

  d <- pumf_dictionary(tbl)
  expect_s3_class(d, "tbl_df")
  expect_named(d, c("name", "val", "label_en", "label_fr"))
  vars  <- d[is.na(d$val), ]
  codes <- d[!is.na(d$val), ]
  expect_setequal(vars$name, c("PROV", "WEIGHT"))
  expect_equal(vars$label_fr[vars$name == "WEIGHT"], "Poids")
  expect_equal(codes$label_fr[codes$name == "PROV" & codes$val == "10"],
               "Terre-Neuve")
  expect_equal(nrow(codes), 2L)
})

test_that("pumf_dictionary: by series/version, same as from the tbl", {
  tmp <- withr::local_tempdir()
  tbl <- .fake_tbl(tmp)
  on.exit(close_pumf(tbl))
  expect_equal(pumf_dictionary("FAKE", "2099", cache_path = tmp),
               pumf_dictionary(tbl))
  expect_error(pumf_dictionary("FAKE", cache_path = tmp), "version")
  expect_error(pumf_dictionary(data.frame(x = 1)), "series name")
})

test_that("pumf_dictionary: errors on a tbl without provenance", {
  tmp <- withr::local_tempdir()
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = file.path(tmp, "x.duckdb"))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "t", data.frame(x = 1L))
  expect_error(pumf_dictionary(dplyr::tbl(con, "t")), "provenance")
})

test_that("pumf_dictionary: a missing French label repeats the English one", {
  tmp <- withr::local_tempdir()
  vdir <- make_e2e_version_dir(tmp)
  codes <- readr::read_csv(file.path(vdir, "metadata", "codes.csv"),
                           show_col_types = FALSE)
  codes$label_fr[codes$val == "35"] <- NA
  readr::write_csv(codes, file.path(vdir, "metadata", "codes.csv"))
  d <- pumf_dictionary("FAKE", "2099", cache_path = tmp)
  expect_equal(d$label_fr[!is.na(d$val) & d$val == "35"], "Ontario")
})

test_that(".pumf_sentinel_label_rows: per-code and per-variable entries", {
  rows <- canpumf:::.pumf_sentinel_label_rows(list(
    "9999999" = c(label_en = "Not applicable", label_fr = "Sans objet"),
    HRSWK     = list("999" = c(label_en = "NOT APPLICABLE"))))
  expect_equal(nrow(rows), 2L)
  glob <- rows[is.na(rows$name), ]
  expect_equal(glob$val, "9999999")
  expect_equal(glob$label_fr, "Sans objet")
  spec <- rows[!is.na(rows$name), ]
  expect_equal(spec$name, "HRSWK")
  expect_equal(spec$val, "999")
  expect_true(is.na(spec$label_fr))
  expect_equal(nrow(canpumf:::.pumf_sentinel_label_rows(NULL)), 0L)
})

# ============================================================
# pumf_translate()
# ============================================================

test_that("pumf_translate: factor levels, English to French and back", {
  tmp <- withr::local_tempdir()
  tbl <- .fake_tbl(tmp)
  on.exit(close_pumf(tbl))
  res <- dplyr::count(tbl, PROV) |> dplyr::collect() |> dplyr::arrange(PROV)
  expect_equal(levels(res$PROV), c("Newfoundland", "Ontario"))

  fr <- pumf_translate(res, "fra", dict = tbl)
  expect_equal(levels(fr$PROV), c("Terre-Neuve", "Ontario"))
  expect_equal(as.character(fr$PROV), c("Terre-Neuve", "Ontario"))
  expect_equal(fr$n, res$n)
  expect_named(fr, c("PROV", "n"))

  back <- pumf_translate(fr, "eng", dict = pumf_dictionary(tbl))
  expect_equal(levels(back$PROV), levels(res$PROV))

  rep <- attr(fr, "pumf_translation")
  expect_s3_class(rep, "tbl_df")
  expect_equal(rep$status, c("translated", "translated"))
  expect_equal(rep$column, c("PROV", "PROV"))
})

test_that("pumf_translate: labelled column names are translated too", {
  tmp <- withr::local_tempdir()
  tbl <- .fake_tbl(tmp)
  on.exit(close_pumf(tbl))
  res <- tbl |> label_pumf_columns() |> dplyr::collect()
  expect_named(res, c("pumf_row_id", "Province", "Survey weight"))

  fr <- pumf_translate(res, "fra", dict = tbl, warn = FALSE)
  expect_named(fr, c("pumf_row_id", "Province", "Poids"))
  expect_equal(levels(fr$Province), c("Terre-Neuve", "Ontario"))
})

test_that("pumf_translate: an unknown level is kept and warned about once", {
  tmp <- withr::local_tempdir()
  tbl <- .fake_tbl(tmp)
  on.exit(close_pumf(tbl))
  res <- dplyr::count(tbl, PROV) |> dplyr::collect()
  levels(res$PROV)[levels(res$PROV) == "Newfoundland"] <- "Atlantic"

  expect_warning(fr <- pumf_translate(res, "fra", dict = tbl),
                 "PROV: 'Atlantic'")
  expect_true("Atlantic" %in% levels(fr$PROV))
  expect_true("Ontario"  %in% levels(fr$PROV))
  rep <- attr(fr, "pumf_translation")
  expect_equal(rep$status[rep$from == "Atlantic"], "untranslated")
  expect_no_warning(pumf_translate(res, "fra", dict = tbl, warn = FALSE))
})

test_that("pumf_translate: custom translations, as a vector and a data frame", {
  tmp <- withr::local_tempdir()
  tbl <- .fake_tbl(tmp)
  on.exit(close_pumf(tbl))
  res <- dplyr::count(tbl, PROV) |> dplyr::collect()
  levels(res$PROV)[levels(res$PROV) == "Newfoundland"] <- "Atlantic"

  fr <- expect_no_warning(
    pumf_translate(res, "fra", dict = tbl, custom = c(Atlantic = "Atlantique")))
  expect_setequal(levels(fr$PROV), c("Atlantique", "Ontario"))
  rep <- attr(fr, "pumf_translation")
  expect_equal(rep$status[rep$from == "Atlantic"], "custom")

  # Data-frame form, restricted to one variable, and the reverse direction
  cust <- data.frame(name = "PROV", label_en = "Atlantic", label_fr = "Atlantique")
  fr2  <- pumf_translate(res, "fra", dict = tbl, custom = cust)
  expect_equal(levels(fr2$PROV), levels(fr$PROV))
  en   <- pumf_translate(fr2, "eng", dict = tbl, custom = cust)
  expect_setequal(levels(en$PROV), c("Atlantic", "Ontario"))

  # A custom entry restricted to another variable does not apply
  cust_other <- data.frame(name = "WEIGHT", label_en = "Atlantic",
                           label_fr = "Atlantique")
  expect_warning(pumf_translate(res, "fra", dict = tbl, custom = cust_other),
                 "Atlantic")

  # Custom entries override the dictionary
  fr3 <- pumf_translate(res, "fra", dict = tbl,
                        custom = c(Atlantic = "Atlantique", Ontario = "ONT"))
  expect_setequal(levels(fr3$PROV), c("Atlantique", "ONT"))
})

test_that("pumf_translate: custom entries rename matching columns", {
  tmp <- withr::local_tempdir()
  tbl <- .fake_tbl(tmp)
  on.exit(close_pumf(tbl))
  res <- dplyr::count(tbl, PROV, name = "households") |> dplyr::collect()
  fr  <- pumf_translate(res, "fra", dict = tbl,
                        custom = c(households = "ménages"))
  expect_named(fr, c("PROV", "ménages"))
})

test_that("pumf_translate: works with custom alone, no dictionary", {
  df <- data.frame(g = factor(c("Young", "Old")), n = 1:2)
  fr <- pumf_translate(df, "fra", custom = c(Young = "Jeune", Old = "Âgé"))
  expect_equal(levels(fr$g), c("Âgé", "Jeune"))
  expect_error(pumf_translate(df, "fra"), "custom")
})

test_that("pumf_translate: character columns of a survey variable translate", {
  tmp <- withr::local_tempdir()
  tbl <- .fake_tbl(tmp)
  on.exit(close_pumf(tbl))
  res <- dplyr::count(tbl, PROV) |> dplyr::collect()
  res$PROV <- as.character(res$PROV)
  res$note <- c("Newfoundland", "free text")   # not a survey variable
  fr <- pumf_translate(res, "fra", dict = tbl)
  expect_setequal(fr$PROV, c("Terre-Neuve", "Ontario"))
  expect_equal(fr$note, res$note)               # untouched, not reported
  expect_false("note" %in% attr(fr, "pumf_translation")$column)
})

test_that("pumf_translate: two source levels with one translation merge", {
  d <- tibble::tibble(name = "X", val = c("1", "2", "3"),
                      label_en = c("A", "B", "C"),
                      label_fr = c("AB", "AB", "C"))
  df <- data.frame(X = factor(c("A", "B", "C", "A")))
  fr <- pumf_translate(df, "fra", dict = d)
  expect_equal(levels(fr$X), c("AB", "C"))
  expect_equal(as.character(fr$X), c("AB", "AB", "C", "AB"))
})

test_that("pumf_translate: an ambiguous source label uses the first and says so", {
  d <- tibble::tibble(name = "X", val = c("1", "2"),
                      label_en = c("Don't know", "Don't know"),
                      label_fr = c("Ne sais pas", "Ne sait pas"))
  df <- data.frame(X = factor("Don't know"))
  fr <- pumf_translate(df, "fra", dict = d)
  expect_equal(levels(fr$X), "Ne sais pas")
  expect_equal(attr(fr, "pumf_translation")$status, "ambiguous")
})

test_that("pumf_translate: sentinel companion columns and global rows", {
  d <- tibble::tibble(
    name     = c("TOTINC", NA, NA),
    val      = c(NA, "9999999", "8888888"),
    label_en = c("Total income", "Not applicable", "Not available"),
    label_fr = c("Revenu total", "Sans objet", "Non disponible"))
  df <- data.frame(TOTINC = c(1, NA),
                   TOTINC_sentinel = factor(c(NA, "Not applicable")))
  fr <- pumf_translate(df, "fra", dict = d)
  expect_equal(levels(fr$TOTINC_sentinel), "Sans objet")
  expect_named(fr, c("TOTINC", "TOTINC_sentinel"))
})

test_that("pumf_translate: grouped tibbles keep their groups under new names", {
  tmp <- withr::local_tempdir()
  tbl <- .fake_tbl(tmp)
  on.exit(close_pumf(tbl))
  res <- tbl |> label_pumf_columns() |> dplyr::collect() |>
    dplyr::group_by(Province)
  fr <- pumf_translate(res, "fra", dict = tbl, warn = FALSE)
  expect_s3_class(fr, "grouped_df")
  expect_equal(dplyr::group_vars(fr), "Province")
  expect_equal(levels(fr$Province), c("Terre-Neuve", "Ontario"))
})

test_that("pumf_translate: rejects lazy tbls and bad arguments", {
  tmp <- withr::local_tempdir()
  tbl <- .fake_tbl(tmp)
  on.exit(close_pumf(tbl))
  expect_error(pumf_translate(tbl, "fra", dict = tbl), "collect")
  expect_error(pumf_translate(1:3, "fra", dict = tbl), "data frame")
  expect_error(pumf_translate(data.frame(x = 1), "fra", dict = data.frame(a = 1)),
               "dict")
  expect_error(pumf_translate(data.frame(x = 1), "fra", custom = c("a", "b")),
               "named")
  expect_error(pumf_translate(data.frame(x = 1), "fra",
                              custom = data.frame(label_en = "a")), "label_fr")
})

test_that("pumf_translate: French-built table translates to English", {
  tmp <- withr::local_tempdir()
  tbl <- .fake_tbl(tmp, lang = "fra")
  on.exit(close_pumf(tbl))
  res <- dplyr::count(tbl, PROV) |> dplyr::collect() |> dplyr::arrange(PROV)
  expect_equal(levels(res$PROV), c("Terre-Neuve", "Ontario"))
  en <- pumf_translate(res, "eng", dict = tbl)
  expect_equal(levels(en$PROV), c("Newfoundland", "Ontario"))
})

# ============================================================
# Duplicate labels: table and dictionary agree
# ============================================================

test_that("pumf_translate: codes sharing a label are suffixed alike in table and dictionary", {
  tmp  <- withr::local_tempdir()
  vdir <- make_e2e_version_dir(tmp)
  # Two "Territories" codes in English; French tells them apart.
  readr::write_csv(tibble::tibble(
    name = c("PROV", "PROV", "PROV", "PROV"), val = c("10", "35", "60", "61"),
    label_en = c("Newfoundland", "Ontario", "Territories", "Territories"),
    label_fr = c("Terre-Neuve", "Ontario", "Yukon", "Nunavut")),
    file.path(vdir, "metadata", "codes.csv"))
  readr::write_csv(
    tibble::tibble(PROV = c("10", "35", "60", "61"), WEIGHT = c("1", "2", "3", "4")),
    file.path(vdir, "survey.csv"))
  tbl <- suppressMessages(get_pumf("FAKE", "2099", cache_path = tmp))
  on.exit(close_pumf(tbl))

  df <- dplyr::collect(tbl)
  expect_equal(levels(df$PROV),
               c("Newfoundland", "Ontario", "Territories (60)", "Territories (61)"))
  d <- pumf_dictionary(tbl)
  expect_equal(d$label_en[d$name == "PROV" & d$val %in% c("60", "61")],
               c("Territories (60)", "Territories (61)"))

  fr <- pumf_translate(df, "fra", dict = d)
  expect_equal(levels(fr$PROV), c("Terre-Neuve", "Ontario", "Yukon", "Nunavut"))
  expect_equal(as.character(fr$PROV), c("Terre-Neuve", "Ontario", "Yukon", "Nunavut"))
  back <- pumf_translate(fr, "eng", dict = d)
  expect_equal(levels(back$PROV), levels(df$PROV))
  # the applied labels are recorded next to codes.csv
  expect_true(file.exists(file.path(vdir, "metadata", "codes_applied.csv")))
})

test_that("pumf_translate: a shared label is left alone when only one of its codes occurs", {
  tmp  <- withr::local_tempdir()
  vdir <- make_e2e_version_dir(tmp)
  readr::write_csv(tibble::tibble(
    name = c("PROV", "PROV", "PROV", "PROV"), val = c("10", "35", "60", "61"),
    label_en = c("Newfoundland", "Ontario", "Territories", "Territories"),
    label_fr = c("Terre-Neuve", "Ontario", "Yukon", "Nunavut")),
    file.path(vdir, "metadata", "codes.csv"))
  readr::write_csv(
    tibble::tibble(PROV = c("10", "35", "60", "60"), WEIGHT = c("1", "2", "3", "4")),
    file.path(vdir, "survey.csv"))
  tbl <- suppressMessages(get_pumf("FAKE", "2099", cache_path = tmp))
  on.exit(close_pumf(tbl))

  df <- dplyr::collect(tbl)
  expect_equal(levels(df$PROV), c("Newfoundland", "Ontario", "Territories"))
  d <- pumf_dictionary(tbl)
  expect_equal(d$label_en[d$name == "PROV" & d$val %in% c("60", "61")],
               c("Territories", "Territories"))
  fr <- expect_silent(pumf_translate(df, "fra", dict = d))
  expect_equal(as.character(fr$PROV), c("Terre-Neuve", "Ontario", "Yukon", "Yukon"))
  # by series/version, without the connection, the same dictionary
  d2 <- pumf_dictionary("FAKE", "2099", cache_path = tmp)
  expect_equal(d2, d)
})


# ---- pumf_dictionary(what = "topcodes") -------------------------------------

test_that("pumf_dictionary topcodes: a factor variable's codes are levels, not top codes", {
  tmp <- withr::local_tempdir()
  tbl <- .fake_tbl(tmp)
  on.exit(close_pumf(tbl))
  ca <- canpumf:::.read_codes_applied(file.path(tmp, "FAKE", "2099", "metadata"))
  expect_equal(ca$applied_as[ca$name == "PROV"], c("level", "level"))
  tc <- pumf_dictionary(tbl, what = "topcodes")
  expect_equal(nrow(tc), 0L)
  expect_equal(names(tc), names(pumf_dictionary(tbl)))
  expect_equal(tc, pumf_dictionary("FAKE", "2099", cache_path = tmp,
                                   what = "topcodes"))
  expect_error(pumf_dictionary("FAKE", cache_path = tmp, what = "topcodes"),
               "version")
  expect_error(pumf_dictionary("LFS", cache_path = tmp, what = "topcodes"),
               "longitudinal")
})

test_that("pumf_dictionary: resolves the version aliases of get_pumf()", {
  tmp  <- withr::local_tempdir()
  fake <- file.path(make_e2e_version_dir(tmp), "metadata")
  # The FAKE metadata under the canonical keys an alias resolves to, with a
  # top code recorded the way Stage 3 would.
  for (key in c("Census/2021 (individuals)", "GSS/Cycle 31 (2017)")) {
    meta <- file.path(tmp, key, "metadata")
    dir.create(meta, recursive = TRUE)
    file.copy(list.files(fake, full.names = TRUE), meta)
    readr::write_csv(data.frame(
      name = "WEIGHT", val = "75", label_en = "75 and more",
      label_fr = "75 et plus", applied_as = "value"),
      file.path(meta, "codes_applied.csv"))
  }
  expect_equal(pumf_dictionary("Census", "2021", cache_path = tmp),
               pumf_dictionary("Census", "2021 (individuals)", cache_path = tmp))
  expect_equal(pumf_dictionary("GSS", "2017", cache_path = tmp),
               pumf_dictionary("GSS", "Cycle 31 (2017)", cache_path = tmp))
  for (tc in list(pumf_dictionary("Census", "2021", cache_path = tmp,
                                  what = "topcodes"),
                  pumf_dictionary("GSS", "Cycle 31", cache_path = tmp,
                                  what = "topcodes"))) {
    expect_equal(tc$name, "WEIGHT")
    expect_equal(tc$val, "75")
  }
})

test_that("pumf_dictionary topcodes: asks for a rebuild when the side-car predates it", {
  tmp <- withr::local_tempdir()
  tbl <- .fake_tbl(tmp)
  on.exit(close_pumf(tbl))
  f  <- file.path(tmp, "FAKE", "2099", "metadata", "codes_applied.csv")
  ca <- readr::read_csv(f, col_types = readr::cols(.default = "c"))
  readr::write_csv(ca[, c("name", "val", "label_en", "label_fr")], f)
  expect_error(pumf_dictionary(tbl, what = "topcodes"), "refresh = TRUE")
  file.remove(f)
  expect_error(pumf_dictionary(tbl, what = "topcodes"), "refresh = TRUE")
})
