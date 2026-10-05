# Stage 3 for large CSV files and the record-level registry fixups that came
# with TCP 1881: csv_reader = "duckdb" (the DuckDB-native build, which must
# write the tables the readr path writes), fix_mojibake, removed_records and
# keep_unlabelled_codes.  Synthetic data only: nothing is downloaded.

# A double-encoded "Québec" / "Trois-Rivières", as in the TCP 1881 text fields.
.mj_quebec    <- "QuÃ©bec"
.mj_rivieres  <- "Trois-RiviÃ¨res"

# A version dir whose CSV exercises every rule of Step 7: a lower-case header,
# padded and blank fields, "NA" strings, a quoted field with a comma, a coded
# column with an unlabelled value, a zero-padded code, a renamed column, a
# numeric column with a missing range and a registry sentinel, double-encoded
# text and a removal flag.
.make_native_dir <- function(base) {
  vdir <- file.path(base, "FAKE", "2099")
  meta <- file.path(vdir, "metadata")
  dir.create(meta, recursive = TRUE)
  readr::write_csv(tibble::tibble(
    name     = c("ID", "PROV", "SEX", "AGE", "INC", "PLACE", "FLAG", "NEWVAR"),
    label_en = c("Identifier", "Province", "Sex", "Age", "Income", "Place",
                 "Removal flag", "Renamed"),
    label_fr = c("Identifiant", "Province", "Sexe", "Âge", "Revenu", "Lieu",
                 "Indicateur de retrait", "Renommé"),
    type     = c("character", "character", "character", "numeric", "numeric",
                 "character", "character", "character"),
    decimals = NA_integer_,
    missing_low  = c(NA, NA, NA, 999, NA, NA, NA, NA),
    missing_high = c(NA, NA, NA, 999, NA, NA, NA, NA)),
    file.path(meta, "variables.csv"))
  readr::write_csv(tibble::tibble(
    name     = c("PROV", "PROV", "SEX", "SEX", "FLAG", "FLAG", "NEWVAR", "NEWVAR"),
    val      = c("10", "35", "01", "02", "0", "1", "A", "B"),
    label_en = c("Newfoundland", "Ontario", "Male", "Female", "Kept",
                 "Crossed out", "Alpha", "Beta"),
    label_fr = c("Terre-Neuve", "Ontario", "Homme", "Femme", "Gardé",
                 "Rayé", NA, NA)),
    file.path(meta, "codes.csv"))
  lines <- c(
    "id,prov,sex,age,inc,place,flag,oldvar",
    paste0("001,10,1,34,1000,", .mj_quebec, ",0,A"),
    "002,35,2,999,9999999,\"Smith, John\",0,B",
    paste0("003,48,1,7,250,", .mj_rivieres, ",1,A"),
    "004, 10 ,2,,NA,Ottawa,0,",
    paste0("005,35,1,61,9999999,", .mj_quebec, ",1,B"),
    "006,48,2,12,0,,0,A",
    "007,62,1,45,80,  Hull  ,0,B")
  con <- file(file.path(vdir, "survey.csv"), open = "wb", encoding = "UTF-8")
  writeLines(enc2utf8(lines), con, useBytes = TRUE)
  close(con)
  vdir
}

.native_fixups <- list(
  str_pad               = list(list(cols = "SEX", width = 2L, side = "left", pad = "0")),
  rename                = c(OLDVAR = "NEWVAR"),
  na_values             = "9999999",
  sentinel_labels       = list("9999999" = c(label_en = "Not applicable",
                                             label_fr = "Sans objet")),
  fix_mojibake          = TRUE,
  removed_records       = list(var = "FLAG", values = "1"),
  keep_unlabelled_codes = TRUE)

# Build the fixture with one reader and return every table of the database
# (plus the column types) and the applied codes.
.build_native <- function(reader, lang = "eng", fixups = .native_fixups,
                          entry_args = list(), gzip = FALSE,
                          env = parent.frame()) {
  tmp  <- withr::local_tempdir(.local_envir = env)
  vdir <- .make_native_dir(tmp)
  if (gzip) canpumf:::.gzip_file(file.path(vdir, "survey.csv"))
  entry <- do.call(pumf_registry_entry, utils::modifyList(list(
    csv_reader = reader, data_encoding = "UTF-8", data_fixups = fixups),
    entry_args))
  canpumf:::.pumf_registry_override_set("FAKE", "2099", entry)
  on.exit(canpumf:::.pumf_registry_override_clear("FAKE", "2099"), add = TRUE)
  r <- canpumf:::pumf_build_duckdb(vdir, "FAKE", "2099", lang = lang, refresh = TRUE)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = r$db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  tabs <- setdiff(sort(DBI::dbListTables(con)), "pumf_build_info")
  out  <- lapply(stats::setNames(tabs, tabs), function(t) list(
    data  = DBI::dbGetQuery(con, sprintf('SELECT * FROM "%s" ORDER BY pumf_row_id', t)),
    types = DBI::dbGetQuery(con, sprintf("PRAGMA table_info('%s')", t))[, c("name", "type")]))
  out$codes_applied <- readr::read_csv(
    file.path(vdir, "metadata", "codes_applied.csv"),
    col_types = readr::cols(.default = "c"))
  out
}

# ---- the record-level fixups on the readr path --------------------------------

test_that("removed_records: flagged records go to pumf_removed_<table>", {
  b <- suppressMessages(.build_native("readr"))
  main <- b$eng$data
  rem  <- b$pumf_removed_eng$data
  # records 3 and 5 are flagged: the main table keeps the others, with their
  # position in the file as the key
  expect_equal(main$pumf_row_id, c(1, 2, 4, 6, 7))
  expect_equal(rem$pumf_row_id, c(3, 5))
  expect_equal(names(rem), names(main))
  expect_equal(b$pumf_removed_eng$types, b$eng$types)
  expect_equal(as.character(rem$FLAG), c("Crossed out", "Crossed out"))
  expect_equal(as.character(unique(main$FLAG)), "Kept")
  # the removed records are labelled and converted like the others
  expect_equal(rem$AGE, c(7, 61))
  expect_equal(rem$INC, c(250, NA))
  expect_equal(as.character(rem$SEX), c("Male", "Male"))
})

test_that("removed_records: the sentinel table covers kept and removed records", {
  b    <- suppressMessages(.build_native("readr"))
  sent <- b$pumf_sentinels_eng$data
  # record 2: AGE 999 and INC 9999999; record 5 (removed): INC 9999999
  expect_equal(sent$pumf_row_id, c(2, 5))
  expect_equal(as.character(sent$INC), c("Not applicable", "Not applicable"))
  expect_equal(as.character(sent$AGE), c("999", NA))
})

test_that("removed_records: a flag variable missing from the data warns", {
  fx <- .native_fixups
  fx$removed_records <- list(var = "NOSUCH", values = "1")
  expect_warning(b <- suppressMessages(.build_native("readr", fixups = fx)),
                 "NOSUCH is not in the data")
  expect_equal(nrow(b$eng$data), 7L)
  expect_null(b$pumf_removed_eng)
})

test_that("fix_mojibake: double-encoded text in the data is repaired", {
  b <- suppressMessages(.build_native("readr"))
  expect_equal(b$eng$data$PLACE,
               c("Québec", "Smith, John", "Ottawa", NA, "Hull"))
  expect_equal(b$pumf_removed_eng$data$PLACE,
               c("Trois-Rivières", "Québec"))
  fx <- .native_fixups
  fx$fix_mojibake <- NULL
  raw <- suppressMessages(.build_native("readr", fixups = fx))
  expect_equal(raw$eng$data$PLACE[1L], .mj_quebec)
})

test_that(".fix_data_mojibake: only character columns, NA kept", {
  d <- tibble::tibble(a = c(.mj_quebec, NA, "plain"), b = c(1, 2, 3),
                      c = factor(c("x", "y", "x")))
  out <- canpumf:::.fix_data_mojibake(d)
  expect_equal(out$a, c("Québec", NA, "plain"))
  expect_identical(out$b, d$b)
  expect_identical(out$c, d$c)
})

test_that("keep_unlabelled_codes: an unlabelled value stays a level named by its code", {
  b <- suppressMessages(.build_native("readr"))
  expect_equal(as.character(b$eng$data$PROV),
               c("Newfoundland", "Ontario", "Newfoundland", "48", "62"))
  # documented codes first, then the kept ones in numeric order
  expect_match(b$eng$types$type[b$eng$types$name == "PROV"],
               "Newfoundland.*Ontario.*48.*62")
  ca <- b$codes_applied
  expect_equal(ca$label_en[ca$name == "PROV" & ca$val == "48"], "48")
  expect_equal(ca$label_fr[ca$name == "PROV" & ca$val == "48"], "48")

  fx <- .native_fixups
  fx$keep_unlabelled_codes <- NULL
  expect_warning(drop <- suppressMessages(.build_native("readr", fixups = fx)))
  expect_true(all(is.na(drop$eng$data$PROV[drop$eng$data$pumf_row_id %in% c(6, 7)])))
})

test_that(".pumf_keep_unlabelled_codes: named variables, integer matching, na_values", {
  codes <- tibble::tibble(name = c("A", "A", "B"), val = c("01", "02", "x"),
                          label_en = c("One", "Two", "Ex"), label_fr = NA_character_)
  d <- tibble::tibble(A = c("1", "2", "3", "99", NA), B = c("x", "y", "", "z", "x"))
  all <- canpumf:::.pumf_keep_unlabelled_codes(codes, d)
  # "1" is the documented "01"; 3 and 99 are new
  expect_equal(all$val[all$name == "A"], c("01", "02", "3", "99"))
  expect_equal(all$label_en[all$name == "A"], c("One", "Two", "3", "99"))
  expect_equal(all$val[all$name == "B"], c("x", "y", "z"))
  only <- canpumf:::.pumf_keep_unlabelled_codes(codes, d, vars = "B")
  expect_equal(only$val[only$name == "A"], c("01", "02"))
  nav <- canpumf:::.pumf_keep_unlabelled_codes(codes, d, na_values = "99")
  expect_equal(nav$val[nav$name == "A"], c("01", "02", "3"))
})

# ---- csv_reader = "duckdb" ------------------------------------------------------

test_that("csv_reader = 'duckdb' writes the tables the readr path writes", {
  for (lang in c("eng", "fra")) {
    a <- suppressMessages(.build_native("readr",  lang = lang))
    b <- suppressMessages(.build_native("duckdb", lang = lang))
    expect_setequal(names(b), names(a))
    expect_true(all(c(lang, paste0("pumf_removed_", lang),
                      paste0("pumf_sentinels_", lang)) %in% names(b)))
    for (t in names(a)) expect_equal(b[[t]], a[[t]], info = paste(lang, t))
  }
})

test_that("csv_reader = 'duckdb': without the record-level fixups", {
  fx <- .native_fixups[c("str_pad", "rename", "na_values")]
  a <- suppressWarnings(suppressMessages(.build_native("readr",  fixups = fx)))
  b <- suppressWarnings(suppressMessages(.build_native("duckdb", fixups = fx)))
  expect_equal(nrow(b$eng$data), 7L)
  expect_null(b$pumf_removed_eng)
  for (t in names(a)) expect_equal(b[[t]], a[[t]], info = t)
})

test_that("a gzip-compressed CSV builds the same tables with either reader", {
  plain <- suppressMessages(.build_native("readr"))
  for (reader in c("readr", "duckdb")) {
    gz <- suppressMessages(.build_native(reader, gzip = TRUE))
    expect_setequal(names(gz), names(plain))
    for (t in names(plain)) expect_equal(gz[[t]], plain[[t]], info = paste(reader, t))
  }
})

test_that(".gzip_file: compresses in place; .pumf_data_file_size reads the original size", {
  tmp  <- withr::local_tempdir()
  path <- file.path(tmp, "data.csv")
  writeLines(c("a,b", rep("1,2", 5000L)), path)
  size  <- file.size(path)
  lines <- readLines(path)
  expect_equal(canpumf:::.pumf_data_file_size(path), size)
  gz <- canpumf:::.gzip_file(path, chunk = 1000)
  expect_equal(basename(gz), "data.csv.gz")
  expect_false(file.exists(path))
  expect_lt(file.size(gz), size)
  expect_equal(readLines(gz), lines)
  expect_equal(canpumf:::.pumf_data_file_size(gz), size)
  expect_true(all(canpumf:::.is_csv_path(c("a.csv", "A.CSV", "a.csv.gz"))))
  expect_false(any(canpumf:::.is_csv_path(c("a.txt", "a.csv.zip", "csv.gz"))))
})

test_that(".find_pumf_data_file: a compressed CSV is the data file", {
  tmp <- withr::local_tempdir()
  writeLines("a,b", file.path(tmp, "survey.csv"))
  gz <- canpumf:::.gzip_file(file.path(tmp, "survey.csv"))
  writeLines("x", file.path(tmp, "notes.txt"))
  expect_equal(basename(canpumf:::.find_pumf_data_file(tmp, NULL)), "survey.csv.gz")
  # the anchored mask a Borealis manifest gives
  expect_equal(basename(canpumf:::.find_pumf_data_file(tmp, "^survey\\.csv\\.gz$")),
               "survey.csv.gz")
  expect_equal(normalizePath(canpumf:::.find_pumf_data_file(tmp, "survey\\.csv")),
               normalizePath(gz))
})

test_that("csv_reader = 'duckdb': announces itself and leaves no temporary table", {
  expect_message(b <- .build_native("duckdb"), "with DuckDB")
  expect_false(any(grepl("^pumf_(csv_stage|map_|smap_|mj_|mojibake)", names(b))))
})

test_that("csv_reader = 'duckdb': falls back to readr when the frame would be large", {
  withr::local_options(canpumf.native_max_cells = 1)
  a <- suppressMessages(.build_native("readr"))
  expect_message(b <- .build_native("duckdb"), "reading with readr instead")
  for (t in names(a)) expect_equal(b[[t]], a[[t]], info = t)
})

test_that("csv_reader = 'duckdb': refuses an encoding DuckDB cannot read and a BSW join", {
  expect_error(
    suppressMessages(.build_native("duckdb",
                                   entry_args = list(data_encoding = "CP850"))),
    "UTF-8 or Latin-1")
  expect_error(
    suppressMessages(.build_native("duckdb", entry_args = list(
      bsw_file_mask = "bsw\\.csv", bsw_join_key = "ID"))),
    "does not join a bootstrap-weight file")
})

test_that(".pumf_native_encoding: DuckDB's names for the supported encodings", {
  expect_equal(canpumf:::.pumf_native_encoding("UTF-8"), "utf-8")
  expect_equal(canpumf:::.pumf_native_encoding("utf8"), "utf-8")
  expect_equal(canpumf:::.pumf_native_encoding("latin1"), "latin-1")
  expect_equal(canpumf:::.pumf_native_encoding("ISO-8859-1"), "latin-1")
  expect_error(canpumf:::.pumf_native_encoding(NULL), "CP1252|UTF-8 or Latin-1")
})

test_that(".pumf_native_limits: sets the limit, caps the threads, restores both", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "SET threads = 8")
  cur <- function() DBI::dbGetQuery(con, paste(
    "SELECT current_setting('memory_limit') AS m,",
    "current_setting('threads') AS t"))
  before <- cur()
  old <- canpumf:::.pumf_native_limits(con, limit = "1GB")
  expect_equal(as.integer(cur()$t), 2L)
  expect_false(identical(cur()$m, before$m))
  canpumf:::.pumf_native_limits(con, restore = old)
  expect_equal(cur(), before)

  # a larger limit never raises the thread count; NULL leaves DuckDB alone
  canpumf:::.pumf_native_limits(con, limit = "64GB")
  expect_equal(as.integer(cur()$t), 8L)
  canpumf:::.pumf_native_limits(con, restore = old)
  canpumf:::.pumf_native_limits(con, limit = NULL)
  expect_equal(cur(), before)
})

test_that(".pumf_native_oom: only an out-of-memory error gets the hint", {
  expect_error(canpumf:::.pumf_native_oom(stop("Out of Memory Error: no block")),
               "canpumf.native_memory_limit")
  expect_error(canpumf:::.pumf_native_oom(stop("Binder Error: no column")),
               "^Binder Error: no column$")
  expect_equal(canpumf:::.pumf_native_oom(42), 42)
})

# ---- registry fields -------------------------------------------------------------

test_that("pumf_registry_entry: csv_reader and the record-level fixups are validated", {
  e <- pumf_registry_entry(csv_reader = "duckdb", data_fixups = list(
    fix_mojibake = TRUE, keep_unlabelled_codes = c("A", "B"),
    removed_records = list(var = "FLAG", values = c("1", "2"))))
  expect_equal(e$csv_reader, "duckdb")
  expect_error(pumf_registry_entry(csv_reader = "arrow"), "readr.*duckdb")
  expect_error(pumf_registry_entry(data_fixups = list(removed_records = "FLAG")),
               "removed_records")
  expect_error(pumf_registry_entry(data_fixups = list(
    removed_records = list(var = c("A", "B"), values = "1"))), "removed_records")
})

test_that("registry: TCP 1881 is read by DuckDB with its record-level fixups", {
  e <- canpumf:::pumf_registry_lookup("TCP", "1881")
  expect_equal(e$csv_reader, "duckdb")
  expect_equal(e$data_encoding, "UTF-8")
  expect_equal(e$borealis$doi, "doi:10.5683/SP3/FXZEVO")
  fx <- e$data_fixups
  expect_true(isTRUE(fx$fix_mojibake))
  expect_true(isTRUE(fx$keep_unlabelled_codes))
  expect_equal(fx$removed_records, list(var = "REMOVE_TCP", values = "1"))
  expect_setequal(fx$force_numeric, c("AGE", "AGEMONTH"))
  # every column of the release has a label in both languages
  labs <- fx$labels_supplement
  expect_length(labs, 52L)
  expect_true(all(vapply(labs, function(l)
    all(c("label_en", "label_fr") %in% names(l)) && all(nzchar(l)), logical(1L))))
  expect_equal(anyDuplicated(vapply(labs, `[[`, "", "label_en")), 0L)
  expect_equal(anyDuplicated(vapply(labs, `[[`, "", "label_fr")), 0L)
})

# ---- cache management ------------------------------------------------------------

test_that("remove_pumf_cache(lang = ): drops the language's removed-records sidecar", {
  tmp  <- withr::local_tempdir()
  vdir <- .make_native_dir(tmp)
  canpumf:::.pumf_registry_override_set("FAKE", "2099", pumf_registry_entry(
    csv_reader = "duckdb", data_encoding = "UTF-8", data_fixups = .native_fixups))
  on.exit(canpumf:::.pumf_registry_override_clear("FAKE", "2099"), add = TRUE)
  r <- suppressMessages(
    canpumf:::pumf_build_duckdb(vdir, "FAKE", "2099", lang = "eng", refresh = TRUE))
  suppressMessages(canpumf:::pumf_build_duckdb(vdir, "FAKE", "2099", lang = "fra"))
  tabs <- function() {
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = r$db_path, read_only = TRUE)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
    DBI::dbListTables(con)
  }
  expect_true(all(c("pumf_removed_eng", "pumf_removed_fra") %in% tabs()))
  suppressMessages(remove_pumf_cache("FAKE", "2099", lang = "fra", cache_path = tmp))
  left <- tabs()
  expect_true(all(c("eng", "pumf_removed_eng", "pumf_sentinels_eng") %in% left))
  expect_false(any(c("fra", "pumf_removed_fra", "pumf_sentinels_fra") %in% left))
})
