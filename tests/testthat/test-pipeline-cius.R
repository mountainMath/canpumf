# Integration tests for the Canadian Internet Use Survey (CIUS) pipeline.
# Tests run against data already in the user's canpumf cache.
#
# CIUS 2022 (#28) is distinctive for what its command files leave out: the
# person weight WTPG, the 1000 bootstrap replicate weights WRPG1-WRPG1000 and
# the record key PUMFID are declared by the DATA LIST alone, with no VARIABLE
# LABELS line in either language.  The weights are read with implied decimals
# and become unlabelled numeric variables through .promote_layout_numeric();
# PUMFID has no decimals and stays character.  The registry supplies WTPG's
# label from the user guide (labels_supplement).  The bundle also ships a GTAB
# "CIUS_PUMF_label.txt" beside the data file, so the entry needs a file_mask.

.cius_vdir <- function() {
  file.path(getOption("canpumf.cache_path", ""), "CIUS", "2022")
}

.cius_build <- function(lang, db_path) {
  reg <- canpumf:::pumf_registry_lookup("CIUS", "2022")
  canpumf:::pumf_build_duckdb(.cius_vdir(), "CIUS", "2022",
                               lang      = lang,
                               file_mask = reg$file_mask,
                               db_path   = db_path,
                               refresh   = TRUE)
}

test_that("CIUS 2022: registry entry selects the data file and labels WTPG", {
  reg <- canpumf:::pumf_registry_lookup("CIUS", "2022")
  expect_match("CIUS_PUMF.txt", reg$file_mask)
  expect_no_match("CIUS_PUMF_label.txt", reg$file_mask)
  sup <- reg$data_fixups$labels_supplement
  expect_named(sup, "WTPG")
  expect_false(is.na(sup$WTPG[["label_en"]]))
  expect_false(is.na(sup$WTPG[["label_fr"]]))
  # download URL comes from the StatCan catalogue snapshot
  expect_true("CIUS" %in% canpumf:::.statcan_supported_series)
})

test_that("CIUS 2022: full pipeline emits no warnings", {
  skip_if_not(canpumf:::.version_is_extracted(.cius_vdir()),
              "CIUS 2022 not extracted in cache")

  reg   <- canpumf:::pumf_registry_lookup("CIUS", "2022")
  tmp   <- tempfile(fileext = ".duckdb")
  con   <- NULL
  warns <- character(0L)

  withCallingHandlers(
    {
      canpumf:::pumf_parse_metadata(.cius_vdir(),
                                     metadata_encoding = reg$metadata_encoding,
                                     refresh           = TRUE)
      r   <- .cius_build("eng", tmp)
      tbl <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
      con <<- tbl$src$con
      dplyr::collect(dplyr::count(tbl))
    },
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  if (!is.null(con)) DBI::dbDisconnect(con, shutdown = TRUE)
  unlink(tmp)

  expect_identical(warns, character(0L),
    label = "CIUS 2022: should have no warnings")
})

test_that("CIUS 2022: layout-only weights are numeric, PUMFID stays character", {
  skip_if_not(canpumf:::.version_is_extracted(.cius_vdir()),
              "CIUS 2022 not extracted in cache")
  skip_if_not(file.exists(file.path(.cius_vdir(), "metadata", "variables.csv")),
              "CIUS 2022 metadata not parsed")

  meta <- canpumf:::read_metadata(file.path(.cius_vdir(), "metadata"))
  lay  <- meta$layout
  expect_true(all(c("PUMFID", "WTPG", "WRPG1", "WRPG1000") %in% lay$name))
  # promoted: numeric, unlabelled, decimals carried over from the DATA LIST
  v <- meta$variables
  w <- v[v$name %in% c("WTPG", "WRPG1", "WRPG1000"), ]
  expect_equal(nrow(w), 3L)
  expect_true(all(w$type == "numeric"))
  expect_true(all(is.na(w$label_en) & is.na(w$label_fr)))
  expect_true(all(w$decimals > 0L))
  # not promoted: an identifier without decimals
  expect_false("PUMFID" %in% v$name)
  expect_length(grep("^WRPG\\d+$", v$name), 1000L)

  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)
  r   <- suppressWarnings(.cius_build("eng", tmp))

  tbl <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE), add = TRUE)

  types <- DBI::dbGetQuery(tbl$src$con, sprintf(
    "SELECT column_name, data_type FROM information_schema.columns
      WHERE table_name = '%s' AND column_name IN ('PUMFID','WTPG','WRPG1','WRPG1000')",
    r$table_name))
  expect_equal(types$data_type[types$column_name == "PUMFID"], "VARCHAR")
  expect_true(all(types$data_type[types$column_name != "PUMFID"] == "DOUBLE"))

  d <- dplyr::collect(dplyr::summarise(
    tbl,
    n     = dplyr::n(),
    wt    = sum(WTPG,     na.rm = TRUE),
    w1    = sum(WRPG1,    na.rm = TRUE),
    w1000 = sum(WRPG1000, na.rm = TRUE)))
  expect_equal(d$n, 25118)
  # 32,612,697 is the weighted population printed on every frequency table of
  # the PUMF codebook.  The DATA LIST reads the weights with implied decimals,
  # but the file stores an explicit decimal point, which Stage 3 honours.
  expect_equal(d$wt,    32612697, tolerance = 1e-6)
  expect_equal(d$w1,    d$wt, tolerance = 1e-2)
  expect_equal(d$w1000, d$wt, tolerance = 1e-2)

  # the registry label reaches label_pumf_columns(); the unlabelled replicate
  # weights and the identifier keep their names
  reg <- canpumf:::pumf_registry_lookup("CIUS", "2022")
  for (lc in c("label_en", "label_fr")) {
    m <- canpumf:::.pumf_var_label_map(
      canpumf:::.pumf_apply_labels_supplement(v, reg), lc)
    expect_equal(m$label[m$name == "WTPG"], reg$data_fixups$labels_supplement$WTPG[[lc]])
    expect_false(any(c("PUMFID", "WRPG1") %in% m$name))
  }
})

test_that("CIUS 2022: French build has no unlabelled-variable warning for the weights", {
  skip_if_not(canpumf:::.version_is_extracted(.cius_vdir()),
              "CIUS 2022 not extracted in cache")
  skip_if_not(file.exists(file.path(.cius_vdir(), "metadata", "variables.csv")),
              "CIUS 2022 metadata not parsed")

  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)
  warns <- character(0L)
  r <- withCallingHandlers(.cius_build("fra", tmp),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  expect_identical(warns, character(0L),
    label = "CIUS 2022 fra: should have no warnings")

  tbl <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE), add = TRUE)
  d <- dplyr::collect(dplyr::summarise(tbl, wt = sum(WTPG, na.rm = TRUE)))
  expect_equal(d$wt, 32612697, tolerance = 1e-6)
})

test_that("CIUS 2022: English and French builds have the same structure", {
  skip_if_not(canpumf:::.version_is_extracted(.cius_vdir()),
              "CIUS 2022 not extracted in cache")
  skip_if_not(file.exists(file.path(.cius_vdir(), "metadata", "variables.csv")),
              "CIUS 2022 metadata not parsed")

  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)
  r_en <- suppressWarnings(.cius_build("eng", tmp))
  r_fr <- suppressWarnings(.cius_build("fra", tmp))
  eng  <- .collect_pumf_table(r_en$db_path, r_en$table_name)
  fra  <- .collect_pumf_table(r_fr$db_path, r_fr$table_name)
  expect_pumf_bilingual_parity(eng, fra, label = "CIUS 2022")
})
