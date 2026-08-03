# Integration tests for the Participation and Activity Limitation Survey (PALS).
# Tests run against data already in the user's canpumf cache.
#
# Both editions ship one archive laid out as PUMF/ENG/ and PUMF/FR/, each a
# complete copy of the release in that language.  What makes them distinctive:
#   * 2001 ships no flat file at all -- only the SAS dataset the flat file would
#     have been built from -- so Stage 3 reads it with haven.  Its columns carry
#     the *collection* names, which prefix 632 of the 758 documented names with
#     an "A" that the registry strips via the rename_regex fixup.
#   * 2001's labels come from a SAS PROC FORMAT catalogue whose codes are quoted
#     character strings ("01", "R"), and whose French half states the
#     variable/format association as "s'applique a:" rather than "applies to".
#   * 2006 is a conventional fixed-width release, but its one sentinel-only
#     continuous variable (AUDE_Q02) has missing codes on *both* sides of the
#     valid data, which the missing_codes fixup handles.

.pals_vdir <- function(version) {
  file.path(getOption("canpumf.cache_path", ""), "PALS", version)
}

.pals_build <- function(version, lang, db_path, refresh = TRUE) {
  reg <- canpumf:::pumf_registry_lookup("PALS", version)
  canpumf:::pumf_build_duckdb(.pals_vdir(version), "PALS", version,
                              lang      = lang,
                              file_mask = reg$file_mask,
                              db_path   = db_path,
                              refresh   = refresh)
}

# A PALS build labels 750-odd columns over 70k+ records, so each edition is
# built once per session (both languages into the one file) and reused:
# pumf_build_duckdb(refresh = FALSE) returns early when the table is there.
.pals_cache <- new.env(parent = emptyenv())

.pals_db <- function(version) {
  key <- paste0("db:", version)
  if (is.null(.pals_cache[[key]]))
    .pals_cache[[key]] <- tempfile(fileext = ".duckdb")
  .pals_cache[[key]]
}

# Returns a lazy tbl; the caller disconnects tbl$src$con so the file is free for
# the next build.
.pals_tbl <- function(version, lang = "eng") {
  r <- suppressWarnings(
    .pals_build(version, lang, .pals_db(version), refresh = FALSE))
  canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
}

.pals_skip <- function(version) {
  skip_if_not(canpumf:::.version_is_extracted(.pals_vdir(version)),
              paste("PALS", version, "not extracted in cache"))
  skip_if_not(file.exists(file.path(.pals_vdir(version), "metadata",
                                    "variables.csv")),
              paste("PALS", version, "metadata not parsed"))
}

for (v in c("2001", "2006")) local({
  version <- v

  test_that(paste("PALS", version, ": full pipeline emits no warnings"), {
    .pals_skip(version)

    con   <- NULL
    warns <- character(0L)

    reg <- canpumf:::pumf_registry_lookup("PALS", version)
    withCallingHandlers(
      {
        canpumf:::pumf_parse_metadata(.pals_vdir(version),
                                       layout_mask       = reg$layout_mask,
                                       metadata_encoding = reg$metadata_encoding,
                                       refresh           = TRUE)
        # Seeds the shared per-edition database the tests below read from.
        # Both languages are built here, before any read-only connection has
        # been opened on the file: DuckDB keeps a per-path instance alive until
        # the last handle is collected, so a later write would be refused.
        r   <- .pals_build(version, "eng", .pals_db(version))
        .pals_build(version, "fra", .pals_db(version))
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

    expect_identical(warns, character(0L),
      label = paste("PALS", version, ": should have no warnings"))
  })

  test_that(paste("PALS", version, ": metadata is fully bilingual"), {
    .pals_skip(version)

    meta <- canpumf:::read_metadata(file.path(.pals_vdir(version), "metadata"))
    expect_identical(sum(is.na(meta$variables$label_en)), 0L)
    # The French command file is paired by the /FR/ path marker alone -- the
    # filenames carry no language suffix -- and for 2001 its PROC FORMAT
    # association comments read "s'applique a:".  A regression in either would
    # leave the French labels empty.
    expect_identical(sum(is.na(meta$variables$label_fr)), 0L)
    expect_gt(nrow(meta$codes), 4000L)
  })
})

test_that("PALS 2001: SAS dataset is read and the collection prefix stripped", {
  .pals_skip("2001")

  tbl <- .pals_tbl("2001")
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE), add = TRUE)

  cols <- colnames(tbl)
  expect_equal(dplyr::pull(dplyr::collect(dplyr::count(tbl))), 76260)
  expect_length(cols, 758L)

  # Documented names, not the AB../AC.. collection names the dataset ships.
  expect_true(all(c("B1", "C28AA", "D8A") %in% cols))
  expect_false(any(c("AB1", "AC28AA", "AD8A") %in% cols))
  # The three genuinely A-initial variables must survive the rewrite.
  expect_true(all(c("AGEGRP5", "AGILIM", "ATTENDRP") %in% cols))
})

test_that("PALS 2001: undeclared reserved codes are labelled, not dropped", {
  .pals_skip("2001")

  tbl <- .pals_tbl("2001")
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE), add = TRUE)

  # 55,550 of the 76,260 records are DISAB=0; the command file's PROC FORMAT
  # blocks cover only the disabled sub-sample, so without codes_supplement the
  # whole out-of-universe group would go NA in every disability variable.
  d <- dplyr::collect(dplyr::summarise(
    tbl,
    n_degree = sum(as.integer(DEGREE == "Not applicable"), na.rm = TRUE),
    n_icd9   = sum(as.integer(ICD9_1 == "Not applicable"), na.rm = TRUE),
    n_b12    = sum(as.integer(B12    == "Not applicable"), na.rm = TRUE),
    na_rooms = sum(as.integer(is.na(ROOMSP)))))

  expect_equal(d$n_degree, 55550)
  expect_equal(d$n_icd9,   55550)  # 1-char field: code 9, not 93
  expect_equal(d$n_b12,    19021 + 55550)
  expect_equal(d$na_rooms, 0)      # 99 = "Invalid data" is a level, not NA
})

test_that("PALS 2001: hours variables are continuous with sentinels removed", {
  .pals_skip("2001")

  tbl <- .pals_tbl("2001")
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE), add = TRUE)

  d <- dplyr::collect(dplyr::summarise(
    tbl,
    hours_lo = min(HOURS,  na.rm = TRUE), hours_hi = max(HOURS,  na.rm = TRUE),
    e1_lo    = min(E1HRS,  na.rm = TRUE), e1_hi    = max(E1HRS,  na.rm = TRUE),
    c28_lo   = min(C28AA,  na.rm = TRUE), c28_hi   = max(C28AA,  na.rm = TRUE),
    # -3 "Not applicable" (27,011) + -1 "Invalid data" (7)
    hours_na = sum(as.integer(is.na(HOURS)))))

  # User Guide "Allowed values": HOURS/E1HRS 001:065 plus 0 and the 66 top code,
  # C28AA 001:007.
  expect_equal(c(d$hours_lo, d$hours_hi), c(0, 66))
  expect_equal(c(d$e1_lo,    d$e1_hi),    c(0, 66))
  expect_equal(c(d$c28_lo,   d$c28_hi),   c(1, 7))
  expect_equal(d$hours_na, 27018)
})

test_that("PALS 2006: AUDE_Q02 keeps the data its sentinels straddle", {
  .pals_skip("2006")

  tbl <- .pals_tbl("2006")
  on.exit(DBI::dbDisconnect(tbl$src$con, shutdown = TRUE), add = TRUE)

  d <- dplyr::collect(dplyr::summarise(
    tbl,
    n     = dplyr::n(),
    na    = sum(as.integer(is.na(AUDE_Q02))),
    lo    = min(AUDE_Q02, na.rm = TRUE),
    hi    = max(AUDE_Q02, na.rm = TRUE)))

  # Sentinels -5/-6/-7 and 998/999 sit on either side of hours worked, so the
  # single [-7, 999] range min/max would yield NAs the entire column.  The
  # User Guide's "Response:" row counts 635 real answers.
  expect_equal(d$n - d$na, 635)
  expect_equal(d$lo, 1)
  expect_equal(d$hi, 66)
})

test_that("PALS: weighted totals match the published estimates", {
  for (version in c("2001", "2006")) {
    .pals_skip(version)

    tbl <- .pals_tbl(version)
    wt  <- if (version == "2001") "WEIGHT_P" else "WEIGHT_PUMF"
    d   <- dplyr::collect(dplyr::summarise(
      dplyr::group_by(tbl, DISAB),
      n = dplyr::n(), wtd = sum(.data[[wt]], na.rm = TRUE)))

    DBI::dbDisconnect(tbl$src$con, shutdown = TRUE)

    # The disabled sub-total each edition's User Guide prints on every
    # frequency table: 3,420,338 (2001) and 4,162,696 (2006).
    dis <- d$wtd[grepl("^(Disabled|Yes)$", as.character(d$DISAB))]
    expect_equal(dis, if (version == "2001") 3420338 else 4162696,
                 tolerance = 1e-6,
                 label = paste("PALS", version, "disabled weighted total"))
  }
})

test_that("PALS: eng/fra bilingual parity", {
  for (version in c("2001", "2006")) {
    .pals_skip(version)

    tmp <- .pals_db(version)
    skip_if_not(file.exists(tmp), "shared PALS database was not seeded")

    eng <- .collect_pumf_table(tmp, "eng")
    fra <- .collect_pumf_table(tmp, "fra")

    expect_pumf_bilingual_parity(eng, fra, label = paste("PALS", version))
  }
})

test_that("PALS: both editions resolve to their catalogue download", {
  # Curated in list_canpumf_collection() rather than crawled: both editions hang
  # off the single 2009001 publication page, which carries no edition token the
  # crawl could use to tell them apart.  Present on the scraped and the offline
  # fallback path alike, so this needs no network.
  coll <- suppressWarnings(list_canpumf_collection())

  hits <- coll[coll$Acronym == "PALS", , drop = FALSE]
  expect_setequal(hits$Version, c("2001", "2006"))
  expect_true(all(grepl("82m0023x/2009001/PALS_EPLA_\\d{4}\\.zip$", hits$url)))
})

# The shared per-edition databases live in tempdir() for the duration of the
# file's tests; drop them once it is done.
withr::defer(unlink(unlist(as.list(.pals_cache))), testthat::teardown_env())
