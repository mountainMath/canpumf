# Sentinel-label classification.
#
# `.sentinel_pat` / `.missing_pat` decide whether a variable with value labels is
# genuinely categorical or is a continuous variable whose only labels are
# sentinels (missing codes, and zero-value labels like "None").  Getting this
# wrong silently changes a column's storage type, and — because the check runs
# once per build language — an alternative that matches an English label but not
# its French translation splits the two builds apart.  Both real bugs this file
# guards were of exactly that shape.

test_that(".sentinel_pat matches true-missing labels in both languages", {
  en <- c("Not applicable", "Not stated", "Not asked", "Valid skip",
          "Refusal", "Don't know", "Missing", "Not available")
  fr <- c("Sans objet", "Non déclaré", "Non demandé", "Enchaînement valide",
          "Refus", "Ne sait pas", "Manquant", "Non disponible")

  expect_true(all(grepl(canpumf:::.missing_pat, en, perl = TRUE)))
  expect_true(all(grepl(canpumf:::.missing_pat, fr, perl = TRUE)))
})

test_that(".missing_pat matches accented labels irrespective of case", {
  # (*UCP) makes (?i) fold non-ASCII too; older command files shout their labels.
  expect_true(grepl(canpumf:::.missing_pat, "NON DÉCLARÉ", perl = TRUE))
  expect_true(grepl(canpumf:::.missing_pat, "Non déclaré", perl = TRUE))
})

test_that(".sentinel_pat classifies eng/fra label pairs identically", {
  # Real pairs from the cache that previously classified differently by
  # language: accented words (\w is ASCII-only without (*UCP)), an elided
  # article, and a trailing period present in one language only.
  pairs <- list(
    c("No separation prior to divorce or annulment",
      "Aucune séparation avant le divorce ou l'annulation"),
    c("No time spent doing these activities.",
      "Aucun temps alloué à cette activité"),
    c("None", "Aucun don")
  )
  for (p in pairs) {
    m <- grepl(canpumf:::.sentinel_pat, p, perl = TRUE)
    expect_equal(m[[1]], m[[2]],
                 info = paste0("eng/fra split on: ", p[[1]], " / ", p[[2]]))
    expect_true(m[[1]], info = p[[1]])
  }
})

test_that(".sentinel_pat does not swallow genuine categorical labels", {
  # The zero-label alternatives start with "no"/"none"/"aucun"; a real category
  # beginning with those words must still read as categorical.
  cat_labels <- c("No, did not work", "Nova Scotia", "Northern Ontario",
                  "None of the above, but has a disability",
                  "Aucun diplôme, certificat ou grade obtenu par le répondant",
                  "Nonante", "Married", "Refused to answer the whole module")
  hits <- cat_labels[grepl(canpumf:::.sentinel_pat, cat_labels, perl = TRUE)]
  expect_equal(hits, character(0))
})

test_that("the zero-label alternative stays ASCII-only on purpose", {
  # Widening it to z[eé]ro would match Census 1986 WKSWK's French
  # "Zéro semaines travaillées" while the English "Worked zero weeks" still
  # would not (the word does not lead the label) — creating a split rather than
  # closing one.  Both must read the same way, and neither is a zero label here.
  expect_false(grepl(canpumf:::.sentinel_pat, "Worked zero weeks", perl = TRUE))
  expect_false(grepl(canpumf:::.sentinel_pat, "Zéro semaines travaillées",
                     perl = TRUE))
})

test_that(".detect_sentinel_only derives the range from true-missing codes only", {
  # A zero label counts towards "this variable is continuous" but is a valid
  # zero, so it must stay out of the NA range (GSS 2012 ITL_Q10).
  codes <- data.frame(
    name     = rep("ITL_Q10", 4L),
    val      = c("0", "97", "98", "99"),
    label_en = c("None", "Not asked", "Not stated", "Don't know"),
    label_fr = c("Aucun don", "Non demandé", "Non déclaré", "Ne sait pas"),
    stringsAsFactors = FALSE)

  for (lc in c("label_en", "label_fr"))
    expect_equal(canpumf:::.detect_sentinel_only(codes, lc)[["ITL_Q10"]],
                 c(97, 99), info = lc)
})

test_that(".detect_sentinel_only leaves a categorical variable alone", {
  codes <- data.frame(
    name     = rep("MARSTAT", 3L),
    val      = c("1", "2", "9"),
    label_en = c("Married", "Single", "Not stated"),
    label_fr = c("Marié(e)", "Célibataire", "Non déclaré"),
    stringsAsFactors = FALSE)

  for (lc in c("label_en", "label_fr"))
    expect_false("MARSTAT" %in% names(canpumf:::.detect_sentinel_only(codes, lc)),
                 info = lc)
})
