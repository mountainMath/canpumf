# Tests for get_lfs_timeline() (R/lfs_timeline.R).  Offline: small LFS_HIST
# and LFS databases are written to a temporary cache with the same table
# layout the longitudinal engine produces (labelled ENUM columns, a versions
# table), plus the per-version LFS metadata the timeline reads its codes from.

hist_label <- function(name, val, col = "label_en") {
  if (name == "MARSTAT" && val == "era4") {   # 4-category era label
    e <- canpumf:::.lfs_hist_code_eras()
    return(e[[col]][e$name == "MARSTAT" & e$val == "4"])
  }
  h <- canpumf:::.lfs_hist_ref("codes")
  h[[col]][h$name == name & h$val == val]
}

# ENUM-typed table from a data.frame of character columns.
write_enum_table <- function(con, table, df, enum_cols) {
  DBI::dbWriteTable(con, table, df)
  for (cl in enum_cols) {
    lv <- unique(stats::na.omit(df[[cl]]))
    DBI::dbExecute(con, sprintf("ALTER TABLE %s ALTER %s TYPE ENUM(%s)", table, cl,
                                paste(DBI::dbQuoteString(con, lv), collapse = ", ")))
  }
}

make_timeline_cache <- function(dir) {
  # LFS_HIST: 1980-06 (4-category MARSTAT era, CMA not identified), 1990-06,
  # 2002-06 (six MARSTAT categories)
  dir.create(file.path(dir, "LFS_HIST"), recursive = TRUE)
  con <- DBI::dbConnect(duckdb::duckdb(), file.path(dir, "LFS_HIST", "LFS_HIST.duckdb"))
  h <- data.frame(
    SURVYEAR = c(1980L, 1990L, 2002L),
    SURVMNTH = 6L,
    LFSSTAT  = c(hist_label("LFSSTAT", "1"), hist_label("LFSSTAT", "4"),
                 hist_label("LFSSTAT", "6")),
    SEX      = c(hist_label("SEX", "1"), hist_label("SEX", "2"), hist_label("SEX", "2")),
    MARSTAT  = c(hist_label("MARSTAT", "era4"), hist_label("MARSTAT", "era4"),
                 hist_label("MARSTAT", "5")),
    CMA      = c(hist_label("CMA", "4"), hist_label("CMA", "1"), hist_label("CMA", "3")),
    SCHOOLN  = c(NA, hist_label("SCHOOLN", "7"), hist_label("SCHOOLN", "1")),
    EDUC90   = c(NA, hist_label("EDUC90", "5"), hist_label("EDUC90", "6")),
    UHRSMAIN = c(40, 37.5, NA),
    FWEIGHT  = c(100, 200, 300),
    stringsAsFactors = FALSE)
  write_enum_table(con, "lfs_hist_eng", h,
                   c("LFSSTAT", "SEX", "MARSTAT", "CMA", "SCHOOLN", "EDUC90"))
  DBI::dbWriteTable(con, "lfs_hist_versions", data.frame(
    version = c("1980-06", "1990-06", "2002-06"), survyear = h$SURVYEAR,
    survmnth = 6L))
  DBI::dbDisconnect(con, shutdown = TRUE)

  # LFS: 2010 (SEX) and 2025 (GENDER)
  codes <- data.frame(
    name = c("LFSSTAT", "LFSSTAT", "SEX", "SEX", "GENDER", "GENDER",
             "MARSTAT", "MARSTAT", "CMA", "CMA", "SCHOOLN", "EDUC"),
    val  = c("3", "4", "1", "2", "1", "2", "2", "6", "7", "9", "3", "5"),
    label_en = c("Unemployed", "Not in labour force", "Male", "Female",
                 "Men+", "Women+", "Living in common-law", "Single, never married",
                 "Calgary", "Vancouver", "Part-time student", "Bachelor's degree"),
    label_fr = c("En chômage", "Inactifs", "Masculin", "Féminin",
                 "Hommes+", "Femmes+", "Union libre", "Célibataire",
                 "Calgary", "Vancouver", "Étudiant à temps partiel",
                 "Baccalauréat"))
  for (v in c("2010", "2025")) {
    md <- file.path(dir, "LFS", v, "metadata")
    dir.create(md, recursive = TRUE)
    utils::write.csv(codes, file.path(md, "codes.csv"), row.names = FALSE)
  }
  con <- DBI::dbConnect(duckdb::duckdb(), file.path(dir, "LFS", "LFS.duckdb"))
  l <- data.frame(
    SURVYEAR = c(2010L, 2025L), SURVMNTH = 6L,
    LFSSTAT  = c("Unemployed", "Not in labour force"),
    SEX      = c("Male", NA), GENDER = c(NA, "Women+"),
    MARSTAT  = c("Living in common-law", "Single, never married"),
    CMA      = c("Calgary", "Vancouver"),
    SCHOOLN  = c("Part-time student", NA), EDUC = c("Bachelor's degree", NA),
    UHRSMAIN = c(375, NA), HRLYEARN = c(2550, NA), FINALWT = c(400, 500),
    stringsAsFactors = FALSE)
  write_enum_table(con, "lfs_eng", l,
                   c("LFSSTAT", "SEX", "GENDER", "MARSTAT", "CMA", "SCHOOLN", "EDUC"))
  DBI::dbWriteTable(con, "lfs_versions", data.frame(
    version = c("2010", "2025"), survyear = c(2010L, 2025L), survmnth = NA_integer_))
  DBI::dbDisconnect(con, shutdown = TRUE)
  dir
}

collect_timeline <- function(...) {
  tl <- suppressMessages(get_lfs_timeline(...))
  on.exit(close_pumf(tl))
  dplyr::arrange(dplyr::collect(tl), SURVYEAR)
}

test_that("get_lfs_timeline stacks and harmonises both series", {
  tmp <- make_timeline_cache(withr::local_tempdir())
  d <- collect_timeline(cache_path = tmp)
  expect_equal(d$SOURCE, c(rep("LFS_HIST", 3L), "LFS", "LFS"))
  expect_equal(d$SURVYEAR, c(1980L, 1990L, 2002L, 2010L, 2025L))
  expect_equal(as.character(d$LFSSTAT),
               c("Employed, at work", "Unemployed", "Not in labour force",
                 "Unemployed", "Not in labour force"))
  expect_equal(as.character(d$GENDER_SEX),
               c("Men+", "Women+", "Women+", "Men+", "Women+"))
  expect_equal(as.character(d$MARSTAT),
               c("Separated or divorced", "Separated or divorced",
                 "Separated or divorced", "Married or common-law",
                 "Single, never married"))
  # CMA: not identified before 1987; the current LFS's other CMAs merge
  expect_equal(as.character(d$CMA),
               c(NA, "Montréal", "Vancouver", "Other CMA or non-CMA", "Vancouver"))
  expect_equal(as.character(d$SCHOOLN),
               c(NA, "Part-time student", "Non-student", "Part-time student", NA))
  expect_equal(as.character(d$EDUC),
               c(NA, "Bachelor's degree", "Above bachelor's degree",
                 "Bachelor's degree", NA))
  # implied decimals of the current LFS removed; FWEIGHT becomes FINALWT
  expect_equal(d$UHRSMAIN, c(40, 37.5, NA, 37.5, NA))
  expect_equal(d$HRLYEARN, c(NA, NA, NA, 25.5, NA))
  expect_equal(d$FINALWT, c(100, 200, 300, 400, 500))
  # factor levels in harmonised code order; absent source columns are NA
  expect_equal(levels(d$LFSSTAT)[3:4], c("Unemployed", "Not in labour force"))
  expect_true(all(is.na(d$NAICS_18)))
  expect_setequal(colnames(d), canpumf:::.lfs_timeline_ref("variables")$name)
})

test_that("get_lfs_timeline: sources, French labels, column labels", {
  tmp <- make_timeline_cache(withr::local_tempdir())
  d <- collect_timeline(sources = "LFS", cache_path = tmp)
  expect_equal(unique(d$SOURCE), "LFS")

  tl <- suppressMessages(get_lfs_timeline(cache_path = tmp))
  on.exit(close_pumf(tl))
  expect_true("Labour force status" %in% colnames(label_pumf_columns(tl)))
  expect_equal(pumf_var_labels(tl)$name[1:3], c("SOURCE", "SURVYEAR", "SURVMNTH"))

  expect_message(r <- get_lfs_timeline(lang = "fra", cache_path = tmp),
                 "no fra data")
  expect_null(r)
  expect_message(get_lfs_timeline(cache_path = withr::local_tempdir()),
                 "nothing loaded")
})

test_that("get_lfs_timeline attaches read-only and reports unmapped values", {
  tmp <- make_timeline_cache(withr::local_tempdir())
  hist_db <- file.path(tmp, "LFS_HIST", "LFS_HIST.duckdb")
  tl <- suppressMessages(get_lfs_timeline(cache_path = tmp))
  # another process-level reader can still open the file
  con2 <- DBI::dbConnect(duckdb::duckdb(), hist_db, read_only = TRUE)
  expect_equal(DBI::dbGetQuery(con2, "SELECT count(*) AS n FROM lfs_hist_eng")$n, 3)
  DBI::dbDisconnect(con2, shutdown = TRUE)
  close_pumf(tl)

  con <- DBI::dbConnect(duckdb::duckdb(), file.path(tmp, "LFS", "LFS.duckdb"))
  DBI::dbExecute(con, "ALTER TABLE lfs_eng ALTER CMA TYPE VARCHAR")
  DBI::dbExecute(con, "ALTER TABLE lfs_eng ALTER CMA TYPE ENUM('Calgary', 'Vancouver', 'Atlantis')")
  DBI::dbDisconnect(con, shutdown = TRUE)
  expect_warning(tl <- suppressMessages(get_lfs_timeline(cache_path = tmp)),
                 "CMA: Atlantis")
  close_pumf(tl)
})

test_that("timeline reference tables are consistent", {
  v  <- canpumf:::.lfs_timeline_ref("variables")
  cd <- canpumf:::.lfs_timeline_ref("codes")
  rc <- canpumf:::.lfs_timeline_ref("recodes")
  expect_false(anyDuplicated(v$name) > 0L)
  expect_setequal(unique(cd$name), v$name[v$type == "factor"])
  expect_true(all(rc$name %in% cd$name))
  # every harmonised target exists, and each code has one label per language
  expect_true(all(paste(rc$name, rc$val)[!is.na(rc$val)] %in% paste(cd$name, cd$val)))
  expect_false(anyDuplicated(cd[c("name", "label_en")]) > 0L)
  expect_false(anyDuplicated(cd[c("name", "label_fr")]) > 0L)
  # LFS_HIST source columns exist in the LFS_HIST dictionary, and every
  # LFS_HIST code of a harmonised source column has a recode
  hv <- canpumf:::.lfs_hist_ref("variables")$name
  expect_true(all(stats::na.omit(v$lfs_hist) %in% hv))
  hc <- canpumf:::.lfs_hist_ref("codes")
  for (i in which(v$type == "factor")) {
    sc <- hc$val[hc$name == v$lfs_hist[i]]
    got <- rc$source_val[rc$name == v$name[i] & rc$source == "LFS_HIST"]
    expect_true(all(as.character(as.integer(sc)) %in% got), info = v$name[i])
  }
})
