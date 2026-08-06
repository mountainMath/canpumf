# Integration tests for the General Social Survey (GSS) pipeline.
# Tests run against data already in the user's canpumf cache.

.gss_cache <- function() getOption("canpumf.cache_path", "")

.gss_vdir <- function(version) {
  file.path(.gss_cache(), "GSS", version)
}

# All end-to-end verified versions (Caregiving series, registered as plain years).
# 2002 is GSS cycle 16 ("Aging and Social Support"), a multi-module survey
# (MAIN + CG4 + CG6 + CR linked on RECID); the primary module is MAIN.
.gss_verified <- c("Cycle 32 (2018)", "Cycle 26 (2012)", "Cycle 21 (2007)", "Cycle 16 (2002)", "Cycle 11 (1996)")

# Non-caregiving GSS themes with their version strings
.gss_theme_verified <- c(
  "Cycle 34 (2019)", "Cycle 35 (2020)", "Cycle 36 (2022)",
  "Cycle 31 (2017)", "Cycle 28 (2014)", "Cycle 27 (2013)", "Cycle 29 (2015)",
  "Cycle 25 (2011)", "Cycle 17 (2003)",
  "Cycle 24 (2010)", "Cycle 9 (1994)", "Cycle 8 (1993)", "Cycle 13 (1999)",
  "Cycle 10 (1995)", "Cycle 15 (2001)", "Cycle 12 (1998)"
)

# Versions whose StatCan distribution contains only English command files;
# no French variable labels are available in the metadata.
.gss_no_french_vars <- c("Cycle 9 (1994)")

# Versions where StatCan's SPSS/SAS command files carry no French code labels;
# bilingual parity tests are skipped for these.
# Education 1994: English-only command files.
# 2002 (cycle 16): variables.csv has French variable names, but codes.csv has
#   none — the SPSS value-label blocks are English-only.
.gss_no_french_codes <- c("Cycle 9 (1994)", "Cycle 16 (2002)")

# GSS versions with known expected warnings (pattern matched against each warning).
# Any warning NOT matching this pattern is unexpected and fails the test.
.gss_supplement_warnings <- list(
  # 2018: force_numeric injects boundary-label variables
  "Cycle 32 (2018)" = "absent from command files",
  # 2012: force_numeric injects boundary-label variables; WTBS bootstrap weights
  #        are in the layout but lack individual variable labels in the SPSS file
  "Cycle 26 (2012)" = "absent from command files|Variables in layout but not in variable labels",
  # 2007: WTBS_002–WTBS_500 unlabeled in VARIABLE LABELS (StatCan only labels #1);
  #        the French SPSS file for Cycle 21 covers only ~26 of 951 variables
  "Cycle 21 (2007)" = "Variables in layout but not in variable labels|no French translation",
  # 2002 (cycle 16, MAIN module): force_numeric injects boundary-label variables
  "Cycle 16 (2002)" = "absent from command files",
  # Non-caregiving themes with force_numeric (boundary labels → continuous data)
  "Cycle 34 (2019)"         = "absent from command files",
  "Cycle 28 (2014)"         = "absent from command files",
  "Cycle 13 (1999)"         = "absent from command files|Variables in layout but not in variable labels|no French translation",
  "Cycle 8 (1993)"         = "absent from command files",
  "Cycle 31 (2017)"         = "absent from command files",
  "Cycle 25 (2011)"         = "absent from command files|Variables in layout but not in variable labels",
  "Cycle 15 (2001)"         = "absent from command files",
  "Cycle 10 (1995)"         = "absent from command files",
  "Cycle 27 (2013)"= "absent from command files",
  "Cycle 17 (2003)"= "absent from command files|Variables in layout but not in variable labels",
  "Cycle 9 (1994)"      = "absent from command files",
  "Cycle 29 (2015)"       = "absent from command files",
  "Cycle 24 (2010)"       = "Variables in layout but not in variable labels"
  # Time Use 1998: force_numeric for its continuous clock-time/duration/year
  # variables + lossy-source conflict suppression in merge_metadata() now leave
  # zero warnings under both C and UTF-8 list.files() ordering, so no allowance.
)


# ---- Full pipeline loop (Stage 2 + Stage 3) — Caregiving series -------------

for (.v in .gss_verified) {
  local({
    ver <- .v
    pat <- .gss_supplement_warnings[[ver]]

    test_that(paste0("GSS ", ver, ": full pipeline emits no unexpected warnings"), {
      skip_if_not(canpumf:::.version_is_extracted(.gss_vdir(ver)),
                  paste("GSS", ver, "not extracted in cache"))

      reg  <- canpumf:::pumf_registry_lookup("GSS", ver)
      tmp  <- tempfile(fileext = ".duckdb")
      con  <- NULL
      warns <- character(0L)

      withCallingHandlers(
        {
          canpumf:::pumf_parse_metadata(.gss_vdir(ver),
                                         metadata_encoding = reg$metadata_encoding,
                                         refresh           = TRUE)
          r   <- canpumf:::pumf_build_duckdb(.gss_vdir(ver), "GSS", ver,
                                              lang    = "eng",
                                              db_path = tmp,
                                              refresh = TRUE)
          tbl <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
          con <<- tbl$src$con
          dplyr::collect(tbl)
        },
        warning = function(w) {
          warns <<- c(warns, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )

      if (!is.null(con)) DBI::dbDisconnect(con, shutdown = TRUE)
      unlink(tmp)

      if (!is.null(pat)) {
        unexpected <- warns[!grepl(pat, warns)]
        expect_identical(unexpected, character(0L),
          label = paste0("GSS ", ver, ": unexpected warnings"))
      } else {
        expect_identical(warns, character(0L),
          label = paste0("GSS ", ver, ": should have no warnings"))
      }
    })
  })
}


# ---- Full pipeline loop — non-caregiving themes -----------------------------

for (.v in .gss_theme_verified) {
  local({
    ver <- .v
    pat <- .gss_supplement_warnings[[ver]]

    test_that(paste0("GSS '", ver, "': full pipeline emits no unexpected warnings"), {
      skip_if_not(canpumf:::.version_is_extracted(.gss_vdir(ver)),
                  paste("GSS", ver, "not extracted in cache"))

      reg  <- canpumf:::pumf_registry_lookup("GSS", ver)
      tmp  <- tempfile(fileext = ".duckdb")
      con  <- NULL
      warns <- character(0L)

      withCallingHandlers(
        {
          canpumf:::pumf_parse_metadata(.gss_vdir(ver),
                                         layout_mask       = reg$layout_mask,
                                         metadata_encoding = reg$metadata_encoding,
                                         refresh           = TRUE)
          r   <- canpumf:::pumf_build_duckdb(.gss_vdir(ver), "GSS", ver,
                                              lang        = "eng",
                                              layout_mask = reg$layout_mask,
                                              file_mask   = reg$file_mask,
                                              db_path     = tmp,
                                              refresh     = TRUE)
          tbl <- canpumf:::pumf_open_duckdb(r$db_path, r$table_name)
          con <<- tbl$src$con
          dplyr::collect(tbl)
        },
        warning = function(w) {
          warns <<- c(warns, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )

      if (!is.null(con)) DBI::dbDisconnect(con, shutdown = TRUE)
      unlink(tmp)

      if (!is.null(pat)) {
        unexpected <- warns[!grepl(pat, warns)]
        expect_identical(unexpected, character(0L),
          label = paste0("GSS '", ver, "': unexpected warnings"))
      } else {
        expect_identical(warns, character(0L),
          label = paste0("GSS '", ver, "': should have no warnings"))
      }
    })
  })
}


# ---- Bilingual label coverage -----------------------------------------------

for (.v in c(.gss_verified, .gss_theme_verified)) {
  local({
    ver <- .v
    test_that(paste0("GSS '", ver, "': metadata has English and French labels"), {
      vdir <- .gss_vdir(ver)
      skip_if_not(file.exists(file.path(vdir, "metadata", "variables.csv")),
                  paste("GSS", ver, "metadata not parsed"))

      meta <- canpumf:::read_metadata(file.path(vdir, "metadata"))

      en_vars <- sum(!is.na(meta$variables$label_en) &
                       nchar(meta$variables$label_en) > 0L)
      expect_gt(en_vars, 0L,
        label = paste0("GSS '", ver, "': should have English variable labels"))

      if (ver %in% .gss_no_french_vars) {
        skip(paste("GSS", ver, "has English-only command files; no French variable labels"))
      }
      fr_vars <- sum(!is.na(meta$variables$label_fr) &
                       nchar(meta$variables$label_fr) > 0L)
      expect_gt(fr_vars, 0L,
        label = paste0("GSS '", ver, "': should have French variable labels"))
    })
  })
}


# ---- Bilingual parity (per verified version) --------------------------------

for (.v in .gss_verified) {
  local({
    ver <- .v
    test_that(paste0("GSS ", ver, ": eng/fra bilingual parity"), {
      skip_if_not(canpumf:::.version_is_extracted(.gss_vdir(ver)),
                  paste("GSS", ver, "not extracted in cache"))
      skip_if_not(file.exists(file.path(.gss_vdir(ver), "metadata", "variables.csv")),
                  paste("GSS", ver, "metadata not parsed"))
      if (ver %in% .gss_no_french_codes) {
        skip(paste("GSS", ver, "has no French code labels; bilingual parity not testable"))
      }

      tmp <- tempfile(fileext = ".duckdb")
      on.exit(unlink(tmp), add = TRUE)

      r_eng <- suppressWarnings(
        canpumf:::pumf_build_duckdb(.gss_vdir(ver), "GSS", ver,
                                     lang = "eng", db_path = tmp, refresh = TRUE))
      r_fra <- suppressWarnings(
        canpumf:::pumf_build_duckdb(.gss_vdir(ver), "GSS", ver,
                                     lang = "fra", db_path = tmp, refresh = TRUE))

      eng <- .collect_pumf_table(tmp, r_eng$table_name)
      fra <- .collect_pumf_table(tmp, r_fra$table_name)

      expect_pumf_bilingual_parity(eng, fra, label = paste0("GSS ", ver))
    })
  })
}

for (.v in .gss_theme_verified) {
  local({
    ver <- .v
    test_that(paste0("GSS '", ver, "': eng/fra bilingual parity"), {
      skip_if_not(canpumf:::.version_is_extracted(.gss_vdir(ver)),
                  paste("GSS", ver, "not extracted in cache"))
      skip_if_not(file.exists(file.path(.gss_vdir(ver), "metadata", "variables.csv")),
                  paste("GSS", ver, "metadata not parsed"))
      if (ver %in% .gss_no_french_codes) {
        skip(paste("GSS", ver, "has no French code labels; bilingual parity not testable"))
      }

      reg <- canpumf:::pumf_registry_lookup("GSS", ver)
      tmp <- tempfile(fileext = ".duckdb")
      on.exit(unlink(tmp), add = TRUE)

      r_eng <- suppressWarnings(
        canpumf:::pumf_build_duckdb(.gss_vdir(ver), "GSS", ver,
          lang = "eng", layout_mask = reg$layout_mask,
          file_mask = reg$file_mask, db_path = tmp, refresh = TRUE))
      r_fra <- suppressWarnings(
        canpumf:::pumf_build_duckdb(.gss_vdir(ver), "GSS", ver,
          lang = "fra", layout_mask = reg$layout_mask,
          file_mask = reg$file_mask, db_path = tmp, refresh = TRUE))

      eng <- .collect_pumf_table(tmp, r_eng$table_name)
      fra <- .collect_pumf_table(tmp, r_fra$table_name)

      expect_pumf_bilingual_parity(eng, fra, label = paste0("GSS '", ver, "'"))
    })
  })
}


# ---- GSS cycle 16 (2002) multi-module support -------------------------------

test_that("GSS cycle/theme/year aliases resolve to canonical Cycle N (YYYY)", {
  rv <- function(v) canpumf:::pumf_resolve_version("GSS", v)
  # cycle number, bare year, and theme name all reach the canonical key
  expect_identical(rv("Cycle 16"), "Cycle 16 (2002)")
  expect_identical(rv("cycle16"),  "Cycle 16 (2002)")
  expect_identical(rv("16"),       "Cycle 16 (2002)")
  expect_identical(rv("2002"),     "Cycle 16 (2002)")
  expect_identical(rv("Aging and Social Support (2002)"), "Cycle 16 (2002)")
  # theme-named historical keys still resolve
  expect_identical(rv("Safety 1999"),  "Cycle 13 (1999)")
  expect_identical(rv("Family 2017"),  "Cycle 31 (2017)")
  expect_identical(rv("Education 2007"), "Cycle 21 (2007)")
  expect_identical(rv("Time Use 2022"), "Cycle 36 (2022)")
  # the canonical key is idempotent; unrelated versions pass through unchanged
  expect_identical(rv("Cycle 16 (2002)"), "Cycle 16 (2002)")
  expect_identical(rv("Health 1991"), "Health 1991")
})

test_that("GSS 2002 registry exposes four linked modules with MAIN primary", {
  reg  <- canpumf:::pumf_registry_lookup("GSS", "Cycle 16 (2002)")
  mods <- canpumf:::.pumf_entry_modules(reg)
  expect_identical(sort(names(mods)), c("CG4", "CG6", "CR", "MAIN"))
  expect_true(mods$MAIN$is_primary)
  # primary module drives the top-level layout_mask / file_mask
  expect_identical(reg$layout_mask, "C16PUMF_MAIN")
  # secondary modules keep metadata in metadata/<module>/, primary in metadata/
  expect_null(mods$MAIN$meta_subdir)
  expect_identical(mods$CG4$meta_subdir, "CG4")
})

test_that("GSS 2002 builds joinable modules sharing one connection", {
  skip_if_not(canpumf:::.version_is_extracted(.gss_vdir("Cycle 16 (2002)")),
              "GSS 2002 not extracted in cache")

  main <- get_pumf("GSS", "Cycle 16")
  on.exit(close_pumf(main), add = TRUE)

  # MAIN carries the respondent key and the person weight
  expect_true(all(c("RECID", "WGHT_PER") %in% colnames(main)))

  cg4 <- pumf_module(main, "CG4")
  expect_true("RECID" %in% colnames(cg4))
  # sibling module shares MAIN's connection, so the two are joinable
  expect_identical(main$src$con, cg4$src$con)
  joined <- dplyr::inner_join(main, cg4, by = "RECID")
  expect_gt(dplyr::pull(dplyr::tally(joined)), 0L)

  # module-aware labeling reads each module's own metadata
  expect_silent(suppressWarnings(label_pumf_columns(cg4)))
  expect_error(pumf_module(main, "NOPE"), "Unknown module")
})

# ---- user-guide PDF cross-check (cycle 16) ----------------------------------
# Cycle 16's SAS/SPSS cards carry the upstream truncation (hard cuts at 60
# characters, dropped leading text); the user guide's Appendix G dictionary
# carries the full text plus per-code frequencies, so the repair can be
# reconciled against the data file before it is believed.

.gss16_crosscheck_ready <- function() {
  vdir <- .gss_vdir("Cycle 16 (2002)")
  canpumf:::.version_is_extracted(vdir) &&
    file.exists(file.path(vdir, "metadata", "label_repairs.csv"))
}

test_that("GSS 2002: the guide's frequencies reconcile against the data file", {
  skip_if_not_installed("pdftools")
  skip_if_not(.gss16_crosscheck_ready(), "GSS 2002 PDF cross-check not in cache")

  main <- get_pumf("GSS", "Cycle 16")
  on.exit(close_pumf(main), add = TRUE)

  v <- pumf_freq_validation(main)
  expect_gt(nrow(v), 100L)
  expect_named(v, c("block", "name", "status", "n_codes", "n_matched", "note"))
  # Every documented variable resolves to exactly one guide block.
  expect_false(any(duplicated(v$name)))
  # Hundreds of variables reconcile exactly, and nothing contradicts the data.
  expect_gt(sum(v$status %in% c("validated", "continuous")), 500L)
  expect_equal(sum(v$status == "mismatch"), 0L)
})

test_that("GSS 2002: truncated labels are repaired from the guide", {
  skip_if_not_installed("pdftools")
  skip_if_not(.gss16_crosscheck_ready(), "GSS 2002 PDF cross-check not in cache")

  main <- get_pumf("GSS", "Cycle 16")
  on.exit(close_pumf(main), add = TRUE)

  r <- pumf_label_repairs(main)
  expect_gt(nrow(r), 100L)
  expect_named(r, c("kind", "name", "val", "lang", "label_command_file",
                    "label_pdf", "action", "reason", "validation"))
  # Nothing is repaired from a block the frequency check contradicted.
  expect_false(any(r$action %in% c("repaired", "filled") &
                     r$validation == "mismatch"))
  # A repair only ever lengthens a label.
  rep <- r[r$action == "repaired", ]
  expect_true(all(nchar(rep$label_pdf) > nchar(rep$label_command_file)))

  # The delivered variable label is the guide's, not the 60-char command-file cut.
  lab <- pumf_var_labels(main)
  expect_equal(lab$label_en[lab$name == "CG4_FR_Q100_C"],
               "Relationship of the Long Term Care Receiver to respondent - collapsed.")

  # CG4_FR_Q100_C code 85 is the case that motivated the whole cross-check: the
  # command file kept only the tail, and what survived ("Co-worker of respondent
  # and Other relatives)") reads like a category about co-workers rather than
  # the "Other" bucket it actually is.  It is a mid-list truncation, so the
  # survivor starts with a capital exactly as an intact label would -- only the
  # dropped text tells the two apart.
  # `which()` because `val` is empty on variable-kind rows: a bare `==` filter
  # evaluates to NA there and pulls all-NA rows into the subset.
  c85 <- r[which(r$name == "CG4_FR_Q100_C" & r$val == "85" & r$lang == "en"), ]
  expect_equal(unique(c85$action), "repaired")
  expect_match(c85$label_pdf[1L], "^Other \\(Do not include organizations here\\)")

  # The mirror shape: the guide's own field header scraped into a label the
  # command file has in full.  Repairing it would deliver "Longueur : 2 Age du
  # ...", so it must stay flagged even though the label sits at the ceiling.
  furn <- r[r$name == "AGE_LAST_RETIRED_C" & r$lang == "fr", ]
  expect_equal(furn$action, "flagged")
  expect_false(grepl("Longueur", lab$label_fr[lab$name == "AGE_LAST_RETIRED_C"]))
})

test_that("GSS 2002: substantive divergences are flagged, not silently applied", {
  skip_if_not_installed("pdftools")
  skip_if_not(.gss16_crosscheck_ready(), "GSS 2002 PDF cross-check not in cache")

  main <- get_pumf("GSS", "Cycle 16")
  on.exit(close_pumf(main), add = TRUE)

  flagged <- pumf_label_repairs(main, action = "flagged")
  expect_gt(nrow(flagged), 0L)

  # The guide says "long term provider", the command file "long term receiver".
  # Same length, so the guide does not extend it: record, do not act.  This is
  # about the variable label; the same variable's value labels are separately
  # left-truncated and are repaired, so the kind has to be pinned.
  q220 <- flagged[flagged$name == "CG4_FR_Q220" & flagged$lang == "en" &
                    flagged$kind == "variable", ]
  expect_equal(nrow(q220), 1L)
  expect_match(q220$label_command_file, "receiver")
  expect_match(q220$label_pdf, "provider")

  lab <- pumf_var_labels(main)
  expect_match(lab$label_en[lab$name == "CG4_FR_Q220"], "receiver")
})

# ---- user-guide PDF cross-check (cycle 26) ----------------------------------
# Cycle 26 ships two candidate dictionaries, and its guide exercises three
# layout quirks the anchor-based table reader has to survive: a frequency value
# wider than the "FREQ" header word (`9,520`), a zero frequency printed further
# right than its neighbours (`97 Not Asked  0`), and a 0-10 scale that labels
# only its endpoints and leaves codes 01-09 bare.

.gss26_crosscheck_ready <- function() {
  vdir <- .gss_vdir("Cycle 26 (2012)")
  canpumf:::.version_is_extracted(vdir) &&
    file.exists(file.path(vdir, "metadata", "pdf_validation.csv"))
}

test_that("GSS 2012: the user guide is chosen over the analytical-file dictionary", {
  skip_if_not_installed("pdftools")
  skip_if_not(.gss26_crosscheck_ready(), "GSS 2012 PDF cross-check not in cache")

  vdir <- .gss_vdir("Cycle 26 (2012)")
  meta <- canpumf:::read_metadata(file.path(vdir, "metadata"))
  reg  <- canpumf:::pumf_registry_lookup("GSS", "Cycle 26 (2012)")
  fmt  <- canpumf:::detect_formats(vdir, sps_mask = reg$layout_mask)
  skip_if(is.null(fmt$pdf_freq), "no frequency dictionary detected")

  chosen <- canpumf:::.pumf_pdf_choose_candidates(fmt$pdf_freq, meta$layout)
  expect_match(basename(chosen$eng), "Users_Guide")
})

test_that("GSS 2012: every documented variable reconciles with the data", {
  skip_if_not_installed("pdftools")
  skip_if_not(.gss26_crosscheck_ready(), "GSS 2012 PDF cross-check not in cache")

  tbl <- get_pumf("GSS", "Cycle 26 (2012)")
  on.exit(close_pumf(tbl), add = TRUE)

  v <- pumf_freq_validation(tbl)
  expect_gt(nrow(v), 500L)
  expect_equal(sum(v$status == "mismatch"), 0L)
  expect_equal(sum(v$status == "unchecked"), 0L)

  # WLY_Q150's code 1 prints "9,520", wider than the "FREQ" header it is
  # right-aligned to; MAR_Q110's code 97 prints a bare "0" three columns
  # further right than the counts above it.  Both are read off the number
  # column nearest the anchor rather than by walking out from it.
  expect_equal(v$status[v$name == "WLY_Q150"], "validated")
  expect_equal(v$status[v$name == "MAR_Q110"], "validated")

  # LSR_Q110 is a 0-10 satisfaction scale: the guide labels only 00 and 10, so
  # codes 01-09 are code rows with no label at all.  Dropping them would leave
  # nine of the column's values undocumented.
  expect_equal(v$status[v$name == "LSR_Q110"], "validated")
  expect_gte(v$n_codes[v$name == "LSR_Q110"], 14L)
})

test_that("GSS 2010: a block with no frequency table stops at its own rule", {
  skip_if_not_installed("pdftools")
  vdir <- .gss_vdir("Cycle 24 (2010)")
  skip_if_not(canpumf:::.version_is_extracted(vdir), "GSS 2010 not in cache")

  pdfs <- list.files(vdir, pattern = "[.]pdf$", recursive = TRUE, full.names = TRUE)
  cand <- canpumf:::.pumf_detect_freq_pdfs(pdfs, NULL)
  skip_if(is.null(cand$eng), "no English frequency dictionary detected")

  v <- canpumf:::.parse_pdf_freq_single(cand$eng, "eng")$variables

  # The label is the free text between the block header and the FREQ table, so
  # the guide's *last* block -- WTSBS_001, a bootstrap weight printed with no
  # table -- used to run to the end of the document and take the appendix and
  # table of contents with it (78,014 characters, which the repair pass then
  # wrote over a sound command-file label).
  w <- v$label_en[v$name == "WTSBS_001"]
  expect_equal(w, "Bootstrap weight # 1 for sport participation sample weight.")

  # No other block should be anywhere near that size either: the longest
  # legitimate label in this guide is a wrapped survey question.
  expect_lt(max(nchar(v$label_en), na.rm = TRUE), 500L)
})

test_that("GSS 2010 Episode: a label reaching into the number column", {
  skip_if_not_installed("pdftools")
  vdir <- .gss_vdir("Cycle 24 (2010)")
  skip_if_not(canpumf:::.version_is_extracted(vdir), "GSS 2010 not in cache")
  mdir <- file.path(vdir, "metadata", "Episode")
  skip_if_not(file.exists(file.path(mdir, "layout.csv")), "Episode metadata not built")

  reg  <- canpumf:::pumf_registry_lookup("GSS", "Cycle 24 (2010)")
  m    <- Filter(function(z) z$id == "Episode", canpumf:::.pumf_entry_modules(reg))[[1L]]
  lay  <- canpumf:::read_metadata(mdir)$layout
  fmt  <- canpumf:::detect_formats(vdir, sps_mask = m$layout_mask)
  cand <- canpumf:::.pumf_pdf_choose_candidates(fmt$pdf_freq, lay)
  skip_if(is.null(cand$eng), "no English Episode frequency dictionary detected")

  p  <- canpumf:::.parse_pdf_freq_single(cand$eng, "eng")
  b  <- p$variables$block[p$variables$name == "SACT1"][[1L]]
  cd <- p$codes[p$codes$block == b, ]
  fq <- p$freqs[p$freqs$block == b, ]
  lab <- function(v) cd$label_en[cd$val == v]
  frq <- function(v) fq$freq[fq$val == v]

  # Code 15's label reaches into the number column, which pushes the frequency
  # past the FREQ anchor and sends the weighted count to the following line:
  #   15  Domestic work (meal prep and cleanup, cleaning, laundry)     4,255
  #                            6,759,111
  # The stranded number is unambiguous -- one candidate, past the anchor, with
  # whitespace before it -- so it is read, and the orphaned weighted count is
  # not appended to the label.
  expect_equal(lab("15"), "Domestic work (meal prep and cleanup, cleaning, laundry)")
  expect_equal(frq("15"), 4255)

  # Code 18 prints its label flush against both counts
  # ("...cassette tapes or records3,4417,790,477"), so the two cannot be told
  # apart and the frequency stays unknown -- but the digits are certainly the
  # number column, so they are cut off the label rather than left in it.
  expect_equal(lab("18"), "Listening to MP3 players, CD's, cassette tapes or records")
  expect_true(is.na(frq("18")))

  # An ordinary row in the same table, for contrast.
  expect_equal(lab("20"), "Computer use (excluding email, chat groups, social networking)")
  expect_equal(frq("20"), 1680)

  # An unreadable count is missing evidence, not contrary evidence: it must not
  # abort the run (it used to make all() return NA and error the `if` after it,
  # taking the whole Stage 2 parse of this module down).
  dp <- canpumf:::.find_pumf_data_file(vdir, m$file_mask)
  skip_if(is.null(dp) || !file.exists(dp), "Episode data file not in cache")
  val <- canpumf:::.pumf_validate_pdf_freqs(p, lay, dp)
  expect_equal(val$status[val$block == b], "unchecked")
  expect_match(val$note[val$block == b], "unreadable")
  # ... and the rest of the guide still validates against the data.
  expect_true(sum(val$status %in% c("validated", "continuous")) > 20L)
})

test_that("get_pumf rejects module for non-modular surveys", {
  expect_error(
    canpumf:::.pumf_table_name("GSS", "Cycle 32 (2018)", "eng", module = "MAIN"),
    "no modules")
})

# Time Use cycles ship a Main respondent file and an Episode file (one row per
# diary episode) sharing the respondent key; both land in one DuckDB as linked
# tables. Each cycle keys on its own respondent id (PUMFID for 2015/2022, RECID
# for 1998/2010).
.gss_timeuse_modules <- list(
  "Cycle 36 (2022)" = "PUMFID",
  "Cycle 29 (2015)" = "PUMFID",
  "Cycle 24 (2010)" = "RECID",
  "Cycle 12 (1998)" = "RECID"
)

for (.v in names(.gss_timeuse_modules)) {
  local({
    ver <- .v
    key <- .gss_timeuse_modules[[ver]]

    test_that(paste0("GSS '", ver, "' registry exposes Main + Episode modules"), {
      reg  <- canpumf:::pumf_registry_lookup("GSS", ver)
      mods <- canpumf:::.pumf_entry_modules(reg)
      expect_identical(sort(names(mods)), c("Episode", "Main"))
      expect_true(mods$Main$is_primary)
      # primary (Main) drives the top-level layout_mask / file_mask
      expect_identical(reg$layout_mask, mods$Main$layout_mask)
      expect_null(mods$Main$meta_subdir)
      expect_identical(mods$Episode$meta_subdir, "Episode")
    })

    test_that(paste0("GSS '", ver, "' builds joinable Main + Episode"), {
      skip_if_not(canpumf:::.version_is_extracted(.gss_vdir(ver)),
                  paste("GSS", ver, "not extracted in cache"))

      main <- suppressWarnings(get_pumf("GSS", ver))
      on.exit(close_pumf(main), add = TRUE)
      expect_true(key %in% colnames(main))

      epi <- pumf_module(main, "Episode")
      expect_true(key %in% colnames(epi))
      # sibling module shares the Main connection, so the two are joinable
      expect_identical(main$src$con, epi$src$con)
      # every episode belongs to a respondent in Main
      main_keys <- dplyr::distinct(dplyr::select(main, dplyr::all_of(key)))
      matched   <- dplyr::inner_join(
        dplyr::select(epi, dplyr::all_of(key)), main_keys, by = key)
      expect_identical(dplyr::pull(dplyr::tally(epi)),
                       dplyr::pull(dplyr::tally(matched)))

      # module-aware labeling reads the Episode module's own metadata
      expect_silent(suppressWarnings(label_pumf_columns(epi)))
    })
  })
}
