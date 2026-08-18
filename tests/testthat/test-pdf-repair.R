# Unit tests for the user-guide PDF cross-check layer (R/pdf_repair.R).
#
# These exercise everything downstream of the PDF scrape, so they need neither
# pdftools nor a cached survey: the parser's output shape is synthesised
# directly.  The end-to-end run against a real guide lives in
# test-pipeline-gss.R.

# ---- code / label normalisation --------------------------------------------

test_that(".pumf_norm_code reconciles zero-padded and bare codes", {
  expect_equal(canpumf:::.pumf_norm_code(c("07", "7", "00000", "0")),
               c("7", "7", "0", "0"))
  # Large numbers must not go through scientific notation.
  expect_equal(canpumf:::.pumf_norm_code("200000"), "200000")
  # Non-numeric codes are compared as trimmed text.
  expect_equal(canpumf:::.pumf_norm_code(c(" A ", "N/A")), c("A", "N/A"))
})

test_that(".pumf_norm_label collapses whitespace but nothing else", {
  expect_equal(canpumf:::.pumf_norm_label("  a   b\n c "), "a b c")
  # Case and punctuation differences are real divergences, not noise.
  expect_equal(canpumf:::.pumf_norm_label("Not Asked."), "Not Asked.")
  expect_true(is.na(canpumf:::.pumf_norm_label("   ")))
})

test_that(".pumf_is_subsequence recognises the interior-drop pattern", {
  expect_true(canpumf:::.pumf_is_subsequence("Single-ded house",
                                             "Single-detached house"))
  expect_false(canpumf:::.pumf_is_subsequence("house detached", "detached house"))
})

# ---- the repair decision ----------------------------------------------------

test_that(".pumf_repair_action fills only where the command file is blank", {
  expect_equal(canpumf:::.pumf_repair_action(NA_character_, "Household size"), "fill")
  expect_equal(canpumf:::.pumf_repair_action("", "Household size"), "fill")
  # Nothing to say: the guide has no label either.
  expect_equal(canpumf:::.pumf_repair_action("Household size", NA_character_), "ok")
})

test_that(".pumf_repair_action repairs only where the guide extends the text", {
  # Hard truncation: strict superstring.
  expect_equal(
    canpumf:::.pumf_repair_action(
      "Relationship of the Long Term Care Receiver to respondent -",
      "Relationship of the Long Term Care Receiver to respondent - collapsed."),
    "repair")
  # Interior drop: subsequence sharing a long prefix.
  expect_equal(canpumf:::.pumf_repair_action("Single-ded house",
                                             "Single-detached house"),
               "repair")
  expect_equal(canpumf:::.pumf_repair_action("Household size", "Household size"), "ok")
})

test_that(".pumf_repair_action flags substantive divergences rather than repairing", {
  # A real upstream contradiction: receiver vs provider.  Same length, so the
  # guide does not extend the command file -- record it, do not act on it.
  expect_equal(
    canpumf:::.pumf_repair_action("Does / Did your long term receiver live ...",
                                  "Does / Did your long term provider live ..."),
    "flag")
  # Guide shorter than the command file: never overwrite.
  expect_equal(canpumf:::.pumf_repair_action("Household size of respondent",
                                             "Household size"),
               "flag")
  # Long enough to be a subsequence by coincidence, but sharing no anchor.
  expect_equal(canpumf:::.pumf_repair_action("abcdefgh", "xaxbxcxdxexfxgxhx"),
               "flag")
})

# ---- validation against the microdata --------------------------------------

# A three-variable fixed-width file:
#   SEX      col 1     1/2
#   AGE      cols 2-3  continuous, 97/98/99 sentinels
#   RECID    cols 4-6  identifier
.pdf_fixture_data <- function() {
  rows <- c(
    paste0("1", "25", "001"),
    paste0("1", "40", "002"),
    paste0("2", "40", "003"),
    paste0("2", "97", "004"),
    paste0("2", "99", "005")
  )
  p <- withr::local_tempfile(fileext = ".DAT", .local_envir = parent.frame())
  writeLines(rows, p)
  p
}

.pdf_fixture_layout <- function() {
  tibble::tibble(name  = c("SEX", "AGE", "RECID"),
                 start = c(1L, 2L, 4L),
                 end   = c(1L, 3L, 6L))
}

# Realistic label pairs: the command file's copy is the guide's text cut at 60
# characters, which is the corruption these repairs exist to undo.
.pdf_age_en <- "Age of the respondent at the time of the interview, in completed years"
.pdf_age_fr <- "Âge du répondant au moment de l'interview, en années révolues et entières"
.pdf_fem_en <- "Female respondent, including those reporting another gender identity"
.pdf_fem_fr <- "Répondante, y compris celles déclarant une autre identité de genre"
.pdf_cut <- function(s) substr(s, 1L, 60L)

.pdf_filler_labels <- function(n, widths)
  substr(paste0(sprintf("Filler label %03d ", seq_len(n)), strrep("abcdefghij ", 8L)),
         1L, widths)

# block 1 = SEX, block 2 = AGE (continuous), block 3 = RECID (range only)
.pdf_fixture_parse <- function() {
  list(
    variables = tibble::tibble(
      name     = c("SEX", "AGE", "RECID"),
      label_en = c("Sex of respondent", .pdf_age_en, "Record identifier"),
      label_fr = c("Sexe", .pdf_age_fr, "Identificateur"),
      type     = c("character", "numeric", "numeric"),
      decimals = c(NA_integer_, 0L, 0L),
      missing_low = NA_real_, missing_high = NA_real_,
      block    = 1:3,
      position = c(1L, 2L, 4L),
      length   = c(1L, 2L, 3L)),
    codes = tibble::tibble(
      name     = c("SEX", "SEX", "AGE", "AGE"),
      val      = c("1", "2", "97", "99"),
      label_en = c("Male", .pdf_fem_en, "Not asked", "Not stated"),
      label_fr = c("Masculin", .pdf_fem_fr, "Non demandé", "Non déclaré"),
      block    = c(1L, 1L, 2L, 2L)),
    layout = NULL,
    freqs = tibble::tibble(
      name  = c("SEX", "SEX", "AGE", "AGE"),
      val   = c("1", "2", "97", "99"),
      freq  = c(2, 3, 1, 1),
      block = c(1L, 1L, 2L, 2L)),
    ranges = tibble::tibble(
      name  = c("AGE", "RECID"),
      lo    = c("25", "001"),
      hi    = c("40", "005"),
      freq  = c(3, 5),
      block = c(2L, 3L))
  )
}

test_that(".pumf_validate_pdf_freqs reconciles exact, continuous and range-only", {
  v <- canpumf:::.pumf_validate_pdf_freqs(
    .pdf_fixture_parse(), .pdf_fixture_layout(), .pdf_fixture_data())

  expect_named(v, c("block", "name", "status", "n_codes", "n_matched", "note"))
  # Every code accounted for exactly.
  expect_equal(v$status[v$name == "SEX"], "validated")
  # Sentinels match; the remaining values are covered by the `lo : hi` row.
  expect_equal(v$status[v$name == "AGE"], "continuous")
  # Range row alone must equal the row count.
  expect_equal(v$status[v$name == "RECID"], "continuous")
})

test_that(".pumf_validate_pdf_freqs marks a wrong count as mismatch", {
  pdf <- .pdf_fixture_parse()
  pdf$freqs$freq[pdf$freqs$name == "SEX" & pdf$freqs$val == "1"] <- 99
  v <- canpumf:::.pumf_validate_pdf_freqs(pdf, .pdf_fixture_layout(),
                                          .pdf_fixture_data())
  expect_equal(v$status[v$name == "SEX"], "mismatch")
})

test_that(".pumf_validate_pdf_freqs is unchecked when the data is unavailable", {
  v <- canpumf:::.pumf_validate_pdf_freqs(.pdf_fixture_parse(),
                                          .pdf_fixture_layout(), NULL)
  expect_true(all(v$status == "unchecked"))
  expect_true(all(v$note == "no data file"))

  # A variable belonging to a sibling module is absent from this data file.
  lay <- .pdf_fixture_layout()[1:2, ]
  v2 <- canpumf:::.pumf_validate_pdf_freqs(.pdf_fixture_parse(), lay,
                                           .pdf_fixture_data())
  expect_equal(v2$status[v2$name == "RECID"], "unchecked")
  expect_equal(v2$note[v2$name == "RECID"], "variable not in data")
})

# ---- multi-module block disambiguation -------------------------------------

test_that(".pumf_pdf_select_blocks drops blocks at the wrong field position", {
  pdf <- .pdf_fixture_parse()
  # A sibling module documents RECID too, at a different offset.
  pdf$variables <- rbind(pdf$variables, tibble::tibble(
    name = "RECID", label_en = "Record identifier", label_fr = "Identificateur",
    type = "numeric", decimals = 0L, missing_low = NA_real_,
    missing_high = NA_real_, block = 4L, position = 90L, length = 3L))
  pdf$ranges <- rbind(pdf$ranges, tibble::tibble(
    name = "RECID", lo = "001", hi = "999", freq = 42, block = 4L))

  out <- canpumf:::.pumf_pdf_select_blocks(pdf, .pdf_fixture_layout())
  expect_equal(out$variables$block[out$variables$name == "RECID"], 3L)
  expect_equal(nrow(out$ranges[out$ranges$name == "RECID", ]), 1L)
})

test_that(".pumf_pdf_resolve_duplicate_blocks picks the block that reconciles", {
  pdf <- .pdf_fixture_parse()
  # Same name, same offset (every module's file starts RECID at position 4),
  # different frequencies -- only the data can tell these apart.
  pdf$variables <- rbind(pdf$variables, tibble::tibble(
    name = "RECID", label_en = "Record identifier", label_fr = "Identificateur",
    type = "numeric", decimals = 0L, missing_low = NA_real_,
    missing_high = NA_real_, block = 4L, position = 4L, length = 3L))
  pdf$ranges <- rbind(pdf$ranges, tibble::tibble(
    name = "RECID", lo = "001", hi = "999", freq = 42, block = 4L))

  # The position filter cannot separate them.
  kept <- canpumf:::.pumf_pdf_select_blocks(pdf, .pdf_fixture_layout())
  expect_setequal(kept$variables$block[kept$variables$name == "RECID"], c(3L, 4L))

  v   <- canpumf:::.pumf_validate_pdf_freqs(kept, .pdf_fixture_layout(),
                                            .pdf_fixture_data())
  res <- canpumf:::.pumf_pdf_resolve_duplicate_blocks(kept, v)

  expect_equal(res$pdf$variables$block[res$pdf$variables$name == "RECID"], 3L)
  expect_equal(nrow(res$validation), 3L)
  expect_false(any(duplicated(res$validation$name)))
})

# ---- repair application and the ledger -------------------------------------

# `truncated = FALSE` keeps exactly the same string shapes but removes the
# 60-character ceiling, so the command file reads as one whose labels were
# hand-abbreviated rather than damaged (SGVP 2007, PALS 2006).
.pdf_fixture_metadata <- function(truncated = TRUE) {
  n    <- 30L
  fill <- .pdf_filler_labels(n, if (truncated) rep(60L, n) else seq(31L, 60L))
  fnm  <- sprintf("FILL%02d", seq_len(n))
  vars <- tibble::tibble(
    name     = c("SEX", "AGE", "RECID", fnm),
    label_en = c("Sex of respondent", .pdf_cut(.pdf_age_en), NA_character_, fill),
    label_fr = c("Sexe", .pdf_cut(.pdf_age_fr), NA_character_, fill),
    type     = c("character", "numeric", "numeric", rep("character", n)),
    decimals = c(NA_integer_, 0L, 0L, rep(NA_integer_, n)),
    missing_low  = c(NA_real_, 97, NA_real_, rep(NA_real_, n)),
    missing_high = c(NA_real_, 99, NA_real_, rep(NA_real_, n)))
  codes <- tibble::tibble(
    name     = c("SEX", "SEX", fnm),
    val      = c("1", "2", rep("1", n)),
    label_en = c("Male", .pdf_cut(.pdf_fem_en), fill),
    label_fr = c("Masculin", .pdf_cut(.pdf_fem_fr), fill))
  list(variables = vars, codes = codes, layout = .pdf_fixture_layout())
}

test_that(".pumf_apply_pdf_repairs repairs, fills and records provenance", {
  pdf <- .pdf_fixture_parse()
  v   <- canpumf:::.pumf_validate_pdf_freqs(pdf, .pdf_fixture_layout(),
                                            .pdf_fixture_data())
  res <- canpumf:::.pumf_apply_pdf_repairs(.pdf_fixture_metadata(), pdf, v)

  # Truncated variable label extended by the guide.
  expect_equal(res$metadata$variables$label_en[res$metadata$variables$name == "AGE"],
               .pdf_age_en)
  # Missing variable label supplied.
  expect_equal(res$metadata$variables$label_en[res$metadata$variables$name == "RECID"],
               "Record identifier")
  # Truncated value label extended.
  expect_equal(res$metadata$codes$label_en[res$metadata$codes$name == "SEX" &
                                             res$metadata$codes$val == "2"],
               .pdf_fem_en)
  # Unchanged labels produce no ledger row.
  expect_false(any(res$repairs$name == "SEX" & res$repairs$val == "1" &
                     res$repairs$lang == "en"))

  expect_named(res$repairs,
               c("kind", "name", "val", "lang", "label_command_file",
                 "label_pdf", "action", "reason", "validation"))
  age <- res$repairs[res$repairs$name == "AGE" & res$repairs$lang == "en", ]
  expect_equal(age$action, "repaired")
  expect_equal(age$validation, "continuous")
})

test_that(".pumf_apply_pdf_repairs withholds repairs only on a mismatch", {
  pdf <- .pdf_fixture_parse()
  v   <- canpumf:::.pumf_validate_pdf_freqs(pdf, .pdf_fixture_layout(),
                                            .pdf_fixture_data())
  v$status[v$name == "AGE"] <- "mismatch"
  res <- canpumf:::.pumf_apply_pdf_repairs(.pdf_fixture_metadata(), pdf, v)

  expect_equal(res$metadata$variables$label_en[res$metadata$variables$name == "AGE"],
               .pdf_cut(.pdf_age_en))
  age <- res$repairs[res$repairs$name == "AGE" & res$repairs$lang == "en", ]
  expect_equal(age$action, "flagged")
  expect_equal(age$validation, "mismatch")

  # A variable the check could not reach is not evidence against the parse.
  v2 <- v
  v2$status[v2$name == "AGE"] <- "unchecked"
  res2 <- canpumf:::.pumf_apply_pdf_repairs(.pdf_fixture_metadata(), pdf, v2)
  expect_equal(res2$metadata$variables$label_en[res2$metadata$variables$name == "AGE"],
               .pdf_age_en)
  expect_equal(res2$repairs$validation[res2$repairs$name == "AGE" &
                                         res2$repairs$lang == "en"], "unchecked")
})

test_that(".pumf_apply_pdf_repairs does not report sentinels the missing range covers", {
  pdf <- .pdf_fixture_parse()
  v   <- canpumf:::.pumf_validate_pdf_freqs(pdf, .pdf_fixture_layout(),
                                            .pdf_fixture_data())
  res <- canpumf:::.pumf_apply_pdf_repairs(.pdf_fixture_metadata(), pdf, v)

  # AGE 97/99 are documented as codes in the guide but declared by the command
  # file as a MISSING VALUES range -- not a divergence.
  undeclared <- res$repairs[grepl("absent from the command file", res$repairs$reason), ]
  expect_equal(nrow(undeclared[undeclared$name == "AGE", ]), 0L)
})

test_that(".pumf_apply_pdf_repairs reports genuinely undeclared codes", {
  pdf <- .pdf_fixture_parse()
  pdf$codes <- rbind(pdf$codes, tibble::tibble(
    name = "SEX", val = "3", label_en = "Other", label_fr = "Autre", block = 1L))
  v <- canpumf:::.pumf_validate_pdf_freqs(pdf, .pdf_fixture_layout(),
                                          .pdf_fixture_data())
  res <- canpumf:::.pumf_apply_pdf_repairs(.pdf_fixture_metadata(), pdf, v)

  undeclared <- res$repairs[grepl("absent from the command file", res$repairs$reason), ]
  expect_equal(undeclared$name, "SEX")
  expect_equal(undeclared$val, "3")
  expect_equal(undeclared$action, "flagged")
  # Reported, never injected -- that is a registry codes_supplement decision.
  expect_equal(nrow(res$metadata$codes),
               nrow(.pdf_fixture_metadata()$codes))
})

test_that(".pumf_apply_pdf_repairs ignores codes on a variable declared continuous", {
  pdf <- .pdf_fixture_parse()
  # RECID carries no codes in the command file, so the guide's zero-value row is
  # a label on a valid numeric zero, not a code the command file forgot.
  pdf$codes <- rbind(pdf$codes, tibble::tibble(
    name = "RECID", val = "0", label_en = "None", label_fr = "Aucun", block = 3L))
  v <- canpumf:::.pumf_validate_pdf_freqs(pdf, .pdf_fixture_layout(),
                                          .pdf_fixture_data())
  res <- canpumf:::.pumf_apply_pdf_repairs(.pdf_fixture_metadata(), pdf, v)

  undeclared <- res$repairs[grepl("absent from the command file", res$repairs$reason), ]
  expect_equal(nrow(undeclared), 0L)
})

# ---- the truncation fingerprint --------------------------------------------

test_that(".pumf_truncation_width finds a ceiling only where one was imposed", {
  # A hard cut piles labels up at the ceiling.
  expect_equal(canpumf:::.pumf_truncation_width(
    .pdf_filler_labels(40L, rep(60L, 40L))), 60L)
  # Hand-written labels thin out towards the longest one.
  expect_true(is.na(canpumf:::.pumf_truncation_width(
    .pdf_filler_labels(40L, seq(21L, 60L)))))
  # Too little evidence to say either way.
  expect_true(is.na(canpumf:::.pumf_truncation_width(c("a", "bb", "ccc"))))
})

test_that(".pumf_left_truncated recognises what a dropped prefix leaves behind", {
  expect_true(canpumf:::.pumf_left_truncated(
    "relative in a family farm or business?",
    "Working without pay for your (his/her) spouse or another relative in a family farm or business?"))
  expect_true(canpumf:::.pumf_left_truncated(
    "les réserves indiennes)", "Régions rurales (incluant les réserves indiennes)"))
  # A truncation landing mid-list leaves a capitalised word first, exactly as an
  # intact label would -- so what survived cannot be used to judge it.
  expect_true(canpumf:::.pumf_left_truncated(
    "Co-worker of respondent and Other relatives)",
    "Other (Do not include organizations here) (Includes Ex-spouse/Ex-partner/Same sex partner/ Co-worker of respondent and Other relatives)"))
  # Not a suffix at all, and too short to judge.
  expect_false(canpumf:::.pumf_left_truncated("foot or bus", "by foot or bus)"))
  expect_false(canpumf:::.pumf_left_truncated("or bus)", "by foot or bus)"))
})

test_that(".pumf_annotation_prefix rejects text the guide prepends", {
  # An editorial note on a label the command file has in full.
  expect_true(canpumf:::.pumf_annotation_prefix(
    "Age group", "Grouped variable: Age group"))
  expect_true(canpumf:::.pumf_annotation_prefix(
    "Groupe d'âge", "Variable groupée : Groupe d'âge"))
  # Scraped field furniture bleeding into the label text.
  expect_true(canpumf:::.pumf_annotation_prefix(
    "Age du répondant la dernière fois qu'il a pris sa retraite.",
    "Longueur : 2 Age du répondant la dernière fois qu'il a pris sa retraite."))
  # Running prose lost to a truncation carries no "Key:" marker.
  expect_false(canpumf:::.pumf_annotation_prefix(
    "Co-worker of respondent and Other relatives)",
    "Other (Do not include organizations here) (Includes Ex-spouse/Ex-partner/Same sex partner/ Co-worker of respondent and Other relatives)"))
  # Extension on the right is a plain truncation, not an annotation.
  expect_false(canpumf:::.pumf_annotation_prefix(
    "Household size", "Household size: persons"))
})

test_that("repairs require the truncation fingerprint, not just a longer PDF text", {
  pdf <- .pdf_fixture_parse()
  v   <- canpumf:::.pumf_validate_pdf_freqs(pdf, .pdf_fixture_layout(),
                                            .pdf_fixture_data())
  res <- canpumf:::.pumf_apply_pdf_repairs(
    .pdf_fixture_metadata(truncated = FALSE), pdf, v)

  # Same strings as the repaired case -- but this command file has no ceiling,
  # so its short labels are abbreviations, not damage.
  expect_equal(res$metadata$variables$label_en[res$metadata$variables$name == "AGE"],
               .pdf_cut(.pdf_age_en))
  age <- res$repairs[res$repairs$name == "AGE" & res$repairs$lang == "en", ]
  expect_equal(age$action, "flagged")
  expect_match(age$reason, "not truncated")
  # A blank label is still filled: there is nothing there to protect.
  expect_equal(res$metadata$variables$label_en[res$metadata$variables$name == "RECID"],
               "Record identifier")
})

test_that("the annotation veto outranks the width fingerprint", {
  # The command file's label is at the ceiling, so the width fingerprint fires;
  # but the guide's extra text arrived on the *left* and is its own field
  # header, which the width test cannot see.  Repairing here would append the
  # guide's furniture to a label that is already complete.
  meta <- .pdf_fixture_metadata()
  cmd  <- meta$variables$label_en[meta$variables$name == "AGE"]
  pdf  <- .pdf_fixture_parse()
  pdf$variables$label_en[pdf$variables$name == "AGE"] <- paste0("Length: 2 ", cmd)

  v   <- canpumf:::.pumf_validate_pdf_freqs(pdf, .pdf_fixture_layout(),
                                            .pdf_fixture_data())
  res <- canpumf:::.pumf_apply_pdf_repairs(meta, pdf, v)

  expect_equal(res$metadata$variables$label_en[res$metadata$variables$name == "AGE"],
               cmd)
  age <- res$repairs[res$repairs$name == "AGE" & res$repairs$lang == "en" &
                       res$repairs$kind == "variable", ]
  expect_equal(age$action, "flagged")
  expect_match(age$reason, "not truncated")
})

test_that("a dropped prefix is repaired even where no width ceiling exists", {
  # A left-truncated label sits well below any ceiling, so the width
  # fingerprint cannot see it; the suffix signature has to carry it alone.
  meta <- .pdf_fixture_metadata(truncated = FALSE)
  sex2 <- meta$codes$name == "SEX" & meta$codes$val == "2"
  meta$codes$label_en[sex2] <- substr(.pdf_fem_en, 20L, nchar(.pdf_fem_en))
  meta$codes$label_fr[sex2] <- substr(.pdf_fem_fr, 16L, nchar(.pdf_fem_fr))

  pdf <- .pdf_fixture_parse()
  v   <- canpumf:::.pumf_validate_pdf_freqs(pdf, .pdf_fixture_layout(),
                                            .pdf_fixture_data())
  res <- canpumf:::.pumf_apply_pdf_repairs(meta, pdf, v)

  out <- res$metadata$codes[res$metadata$codes$name == "SEX" &
                              res$metadata$codes$val == "2", ]
  expect_equal(out$label_en, .pdf_fem_en)
  expect_equal(out$label_fr, .pdf_fem_fr)
  expect_true(all(res$repairs$action[res$repairs$name == "SEX" &
                                       res$repairs$val == "2"] == "repaired"))
})

# ---- document-level corroboration ------------------------------------------

test_that(".pumf_pdf_position_agreement scores the guide against the layout", {
  pdf <- .pdf_fixture_parse()
  pa  <- canpumf:::.pumf_pdf_position_agreement(pdf, .pdf_fixture_layout())
  expect_equal(pa$n, 3L)
  expect_equal(pa$agree, 3L)
  expect_equal(pa$rate, 1)

  # A guide for a different edition: every field shifted.
  pdf$variables$position <- pdf$variables$position + 8L
  pa2 <- canpumf:::.pumf_pdf_position_agreement(pdf, .pdf_fixture_layout())
  expect_equal(pa2$agree, 0L)
  expect_false(any(pa2$ok))
})

test_that(".pumf_pdf_choose_candidates prefers the guide matching the layout", {
  hdr <- function(shift) tibble::tibble(
    name     = c("SEX", "AGE", "RECID"),
    position = c(1L, 2L, 4L) + shift,
    length   = c(1L, 2L, 3L))
  paths <- list(
    eng = "original.pdf", fra = "original_fr.pdf",
    candidates = list(
      list(path = "original.pdf",  lang = "eng", n = 3L, header = hdr(8L)),
      list(path = "revised.pdf",   lang = "eng", n = 3L, header = hdr(0L)),
      list(path = "revised_fr.pdf", lang = "fra", n = 3L, header = hdr(0L))))

  out <- canpumf:::.pumf_pdf_choose_candidates(paths, .pdf_fixture_layout())
  expect_equal(out$eng, "revised.pdf")
  expect_equal(out$fra, "revised_fr.pdf")

  # Without a layout there is nothing to choose on; the detection pick stands.
  expect_equal(canpumf:::.pumf_pdf_choose_candidates(paths, NULL)$eng,
               "original.pdf")
})
