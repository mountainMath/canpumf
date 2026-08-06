# R/pdf_repair.R — Validate a PDF frequency-dictionary parse against the
# microdata, then use it to repair or flag labels from the command-file parse.
#
# StatCan's PUMF command files (SAS/SPSS/Stata alike -- the damage is upstream
# of the flavour-specific renderers) routinely ship truncated value and variable
# labels: hard cuts at 60 characters, dropped leading text, dropped interior
# text.  The same survey's user guide carries the full text, typeset from the
# metadata before the command files were generated.
#
# Trusting a PDF scrape over a machine-readable command file would normally be a
# bad trade.  What makes it a good one here is that this PDF layout prints the
# *frequency* of every code, so the parse can be reconciled against the actual
# data file before any of it is believed:
#
#   Stage 2  parse command files  ─┐
#            parse user-guide PDF ─┼─→ validate PDF freqs against the data file
#                                  │   → repair only labels of variables whose
#                                  │     counts reconcile
#                                  └─→ metadata/label_repairs.csv (the ledger)
#
# Nothing is repaired silently: every divergence, repaired or not, lands in the
# ledger, readable with pumf_label_repairs().

# Statuses assigned by .pumf_validate_pdf_freqs():
#   "validated"  every PDF code's count matches the data, and the data holds no
#                values the PDF does not list
#   "continuous" the listed (sentinel) codes match and the remaining data values
#                are accounted for by the PDF's `lo : hi` range row -- i.e. a
#                continuous variable, correctly parsed
#   "mismatch"   counts disagree; the parse is not trusted for this variable
#   "unchecked"  no data file, no layout, or no overlapping codes to compare
.pumf_pdf_status_ok <- c("validated", "continuous")

# Document-level acceptance thresholds.  The frequency check is a per-variable
# test, but whether the guide describes this file at all is a question about the
# document, and the two corroboration channels are independent:
#
#   * frequencies -- do the printed counts reproduce a tabulation of the data?
#   * field positions -- does the guide's `Position:`/`Length:` reproduce the
#     command file's layout?
#
# A guide that fails both is simply the wrong document (PALS 2006 ships its
# pre-revision user guide alongside the revised one; every field sits eight
# columns off) and is discarded whole rather than allowed to emit thousands of
# bogus divergences.  A guide that fails the first but passes the second is the
# right document whose frequency tables were tabulated on a different base --
# PALS's are computed over the disability sub-population, so no count matches
# even though all 746 positions do.  There the counts carry no per-variable
# information and are treated as absent rather than as contradiction.
.pumf_pdf_min_freq_rate <- 0.5
.pumf_pdf_min_pos_rate  <- 0.9


# Normalise a code value for comparison across sources: the PDF prints
# zero-padded codes ("07", "00000") where the flat file may hold "7"/"0", and
# vice versa.  Anything that parses as a number is rendered back through
# .code_chr(); everything else is compared as trimmed text.
.pumf_norm_code <- function(x) {
  x <- trimws(as.character(x))
  num <- suppressWarnings(as.numeric(x))
  ok  <- !is.na(num) & nzchar(x)
  out <- x
  out[ok] <- .code_chr(num[ok])
  out
}

# Normalise a label for comparison: collapse internal whitespace and trim.
# Nothing else -- punctuation and case differences are real divergences and
# should reach the ledger.
.pumf_norm_label <- function(x) {
  x <- trimws(gsub("[[:space:]]+", " ", x))
  x[!nzchar(x)] <- NA_character_
  x
}

# Is `short` a subsequence of `long` (all characters present, in order)?
# This is what recognises the interior-drop corruption pattern, where the
# command file's label is not a substring of the true one:
#   "Single-ded house"  ⊂  "Single-detached house"
.pumf_is_subsequence <- function(short, long) {
  s <- strsplit(short, "", fixed = TRUE)[[1L]]
  l <- strsplit(long,  "", fixed = TRUE)[[1L]]
  j <- 1L
  for (ch in s) {
    hit <- which(l[j:length(l)] == ch)
    if (!length(hit)) return(FALSE)
    j <- j + hit[[1L]]
    if (j > length(l) + 1L) return(FALSE)
  }
  TRUE
}

# Length of the common prefix / suffix of two strings.
.pumf_common_prefix <- function(a, b) {
  n <- min(nchar(a), nchar(b))
  if (n == 0L) return(0L)
  av <- substring(a, seq_len(n), seq_len(n))
  bv <- substring(b, seq_len(n), seq_len(n))
  d  <- which(av != bv)
  if (length(d)) d[[1L]] - 1L else n
}

.pumf_common_suffix <- function(a, b) {
  .pumf_common_prefix(
    paste(rev(strsplit(a, "", fixed = TRUE)[[1L]]), collapse = ""),
    paste(rev(strsplit(b, "", fixed = TRUE)[[1L]]), collapse = "")
  )
}

# Decide what to do with one (command-file label, PDF label) pair.
#
# The rule is deliberately evidence-driven rather than heuristic: a label is
# only replaced when the PDF text *demonstrably extends* the command file's,
# which is exactly the fingerprint of upstream truncation.  A label that merely
# looks suspicious (e.g. is exactly 60 characters) but whose PDF counterpart is
# a different string is flagged, never rewritten.
#
# Returns one of "ok", "fill", "repair", "flag".
.pumf_repair_action <- function(cmd, pdf) {
  if (is.na(pdf) || !nzchar(pdf)) return("ok")
  if (is.na(cmd) || !nzchar(cmd)) return("fill")
  if (identical(cmd, pdf))        return("ok")
  if (nchar(pdf) <= nchar(cmd))   return("flag")

  # Truncated at either end, or a chunk lifted out of the middle.
  if (grepl(cmd, pdf, fixed = TRUE)) return("repair")

  # Interior drop: characters survive in order but a run went missing.  Require
  # a substantial anchored overlap so a short label cannot match by accident.
  if (nchar(cmd) >= 8L && .pumf_is_subsequence(cmd, pdf) &&
      (.pumf_common_prefix(cmd, pdf) >= 4L || .pumf_common_suffix(cmd, pdf) >= 4L))
    return("repair")

  "flag"
}

# Width at which a command file's labels were hard-truncated, or NA when they
# were not truncated at all.
#
# This is the difference between the two ways a guide's text can be longer than
# the command file's, which the string shapes alone cannot tell apart:
#
#   * GSS Cycle 16 truncates at 60 characters -- 1,665 of its 1,860 variable
#     labels sit at 59-60, against 42 in the six lengths below.  The guide holds
#     the missing tail, and replacing the label is a repair.
#   * SGVP 2007 and PALS 2006 print the *question wording* where the command
#     file gives a hand-written short label ("How many hours do you work per
#     week" vs "How many hours do you (does ....) usually work per week?").
#     These are different fields, not a damaged one, and the abbreviation is
#     often a subsequence of the question -- so the shape test alone would
#     "repair" a perfectly good label into a question.
#
# A hard cut leaves a spike at the ceiling; a hand-written label set thins out
# towards its longest entry.  Comparing the top two lengths against the six
# below them separates the two cleanly (GSS 40-62x, PALS codes 2.9x; SGVP 0.45x,
# CHS 0.39x, CIS 0.41x).
.pumf_truncation_width <- function(labels) {
  n <- nchar(labels[!is.na(labels) & nzchar(labels)])
  if (length(n) < 20L) return(NA_integer_)
  w   <- max(n)
  top <- sum(n >= w - 1L)
  bel <- sum(n >= w - 7L & n < w - 1L)
  if (top >= 5L && top > bel) as.integer(w) else NA_integer_
}

# Is this label sitting at the truncation ceiling -- i.e. is it plausibly one of
# the damaged ones?  False whenever the file has no ceiling at all.
.pumf_at_truncation <- function(cmd, width) {
  !is.na(width) && nchar(cmd) >= width - 1L
}

# The width fingerprint only sees labels cut at a fixed ceiling.  The other
# damage pattern -- dropped *leading* text -- leaves a short label instead, well
# below any ceiling, so it needs its own signature: what survives is a strict
# suffix of the guide's text and begins where a label would not.
#
#   PALS 2006  "relative in a family farm or business?"  (of "Working without
#              pay for your (his/her) spouse or another relative in a ...")
#   GSS 16     "foot or bus)"  (of "... as you? (30 minutes or less by foot or
#              bus)")
#
# The mirror case is a guide that *prefixes* an editorial note onto a label the
# command file has in full -- SGVP 2007's "Grouped variable: Age group" against
# "Age group", "Variable groupée : Groupe d'âge" against "Groupe d'âge".  That
# also leaves the command-file text a strict suffix, but a suffix which starts
# like a label does; requiring the survivor to start lowercase (or on
# punctuation) keeps those flagged, where replacing them would inject the
# guide's editorial prefix into a perfectly good label.
.pumf_left_truncated <- function(cmd, pdf) {
  nchar(cmd) >= 8L && nchar(pdf) > nchar(cmd) && endsWith(pdf, cmd) &&
    !grepl("^\\p{Lu}", cmd, perl = TRUE)
}


# ---------------------------------------------------------------------------
# Frequency validation against the microdata
# ---------------------------------------------------------------------------

# Read one column's raw (trimmed) values from the data file.  Mirrors how
# Stage 3 reads the data -- fixed-width via character positions with trim_ws,
# CSV as text -- so the values compared here are the values that will be
# labelled later.
.pumf_column_values <- function(data_path, layout, is_fwf, data_encoding) {
  if (is_fwf) {
    lines <- readLines(data_path, warn = FALSE, encoding = "bytes")
    function(name) {
      li <- layout[toupper(layout$name) == toupper(name), , drop = FALSE]
      if (nrow(li) == 0L) return(NULL)
      trimws(substr(lines, li$start[[1L]], li$end[[1L]]))
    }
  } else {
    df <- tryCatch(
      readr::read_csv(data_path, col_types = readr::cols(.default = "c"),
                      locale = readr::locale(encoding = data_encoding),
                      show_col_types = FALSE, progress = FALSE),
      error = function(e) NULL)
    if (is.null(df)) return(function(name) NULL)
    names(df) <- toupper(names(df))
    function(name) {
      if (!toupper(name) %in% names(df)) return(NULL)
      trimws(as.character(df[[toupper(name)]]))
    }
  }
}

#' Filter guide blocks by the layout's field position
#'
#' A user guide covering several linked files documents each of them in turn,
#' so a shared respondent key (`RECID`, `PERSONID`) gets one block per module --
#' each with that module's own frequencies.  Keyed by name alone the first
#' block wins, which then contradicts every other module's data file.
#'
#' This is the cheap first pass: the block header prints `Position:` and
#' `Length:`, and the command file's layout gives the same two numbers for this
#' module, so a name documented at different offsets in different modules is
#' resolved here.  It cannot separate blocks that share an offset (every
#' module's file starts `RECID` at position 1) -- those go to
#' [.pumf_pdf_resolve_duplicate_blocks()], which decides on the frequencies.
#'
#' @param pdf Parser output from [parse_pdf_freq_codebook()].
#' @param layout Canonical layout tibble (name/start/end), or `NULL`.
#' @return `pdf`, with `variables`/`codes`/`freqs`/`ranges` filtered to the
#'   surviving blocks.
#' @keywords internal
.pumf_pdf_select_blocks <- function(pdf, layout) {
  v <- pdf$variables
  if (is.null(v) || !"block" %in% names(v)) return(pdf)
  dup <- unique(v$name[duplicated(v$name)])
  if (!length(dup) || is.null(layout) || nrow(layout) == 0L) return(pdf)

  keep <- v$block[!v$name %in% dup]
  for (nm in dup) {
    cand <- v[v$name == nm, , drop = FALSE]
    li   <- match(nm, layout$name)
    hit  <- integer()
    if (!is.na(li)) {
      pos <- layout$start[[li]]
      len <- layout$end[[li]] - layout$start[[li]] + 1L
      hit <- which(!is.na(cand$position) & cand$position == pos &
                     (is.na(cand$length) | cand$length == len))
    }
    # No agreement at all is not evidence against any block -- keep them and
    # let the frequency check decide.
    keep <- c(keep, if (length(hit)) cand$block[hit] else cand$block)
  }
  .pumf_pdf_filter_blocks(pdf, keep)
}

.pumf_pdf_filter_blocks <- function(pdf, keep) {
  filt <- function(d) if (is.null(d) || !"block" %in% names(d)) d
                      else d[d$block %in% keep, , drop = FALSE]
  pdf$variables <- filt(pdf$variables)
  pdf$codes     <- filt(pdf$codes)
  pdf$freqs     <- filt(pdf$freqs)
  pdf$ranges    <- filt(pdf$ranges)
  pdf
}

#' Resolve remaining duplicate blocks by which one reconciles with the data
#'
#' After [.pumf_pdf_select_blocks()] a name can still map to several blocks --
#' typically a respondent key that sits at the same offset in every module of a
#' multi-module survey.  The frequencies settle it: only the block describing
#' the file actually being parsed reconciles against it.  Where none does, the
#' first block is kept and its (mismatching) status stands, so nothing is
#' repaired from a block we could not confirm.
#'
#' @param pdf Parser output, already position-filtered.
#' @param validation Block-keyed output of [.pumf_validate_pdf_freqs()].
#' @return List with the filtered `pdf` and `validation` (one row per name).
#' @keywords internal
.pumf_pdf_resolve_duplicate_blocks <- function(pdf, validation) {
  dup <- unique(validation$name[duplicated(validation$name)])
  if (!length(dup)) return(list(pdf = pdf, validation = validation))

  keep <- validation$block[!validation$name %in% dup]
  for (nm in dup) {
    cand <- validation[validation$name == nm, , drop = FALSE]
    ok   <- which(cand$status %in% .pumf_pdf_status_ok)
    keep <- c(keep, cand$block[[if (length(ok)) ok[[1L]] else 1L]])
  }
  list(pdf        = .pumf_pdf_filter_blocks(pdf, keep),
       validation = validation[validation$block %in% keep, , drop = FALSE])
}

#' Compare the guide's field positions against the command file's layout
#'
#' The structural corroboration channel, independent of the frequencies: each
#' guide block prints the variable's `Position:` (and usually `Length:`), and
#' the command file's layout gives the same two numbers.  Agreement across the
#' whole document is what tells "the right guide whose counts were tabulated on
#' a different population" apart from "the wrong guide".
#'
#' @param pdf Parser output from [parse_pdf_freq_codebook()].
#' @param layout Canonical layout tibble (name/start/end), or `NULL`.
#' @return List with `n` (blocks that could be compared), `agree`, `rate`
#'   (`NA` when nothing was comparable) and `ok`, a logical vector aligned to
#'   `pdf$variables`.
#' @keywords internal
.pumf_pdf_position_agreement <- function(pdf, layout) {
  v  <- pdf$variables
  nv <- if (is.null(v)) 0L else nrow(v)
  none <- list(n = 0L, agree = 0L, rate = NA_real_, ok = rep(FALSE, nv))
  if (nv == 0L || !"position" %in% names(v) ||
      is.null(layout) || nrow(layout) == 0L) return(none)

  li  <- match(v$name, layout$name)
  pos <- layout$start[li]
  len <- layout$end[li] - layout$start[li] + 1L
  cmp <- !is.na(li) & !is.na(v$position)
  ok  <- cmp & v$position == pos & (is.na(v$length) | v$length == len)
  ok[is.na(ok)] <- FALSE
  n <- sum(cmp)
  list(n = n, agree = sum(ok), rate = if (n) sum(ok) / n else NA_real_, ok = ok)
}

#' Pick the guide that matches this module's layout
#'
#' Detection shortlists candidates on filename and block count, which cannot
#' separate a release that ships both its original and its revised user guide:
#' PALS 2006's two English guides document the same 746 variables, but the
#' original's fields all sit eight columns to the left of the revised file's.
#' Scoring each candidate's `Position:`/`Length:` headers against the command
#' file's layout picks the right one -- and does so per language, so the English
#' and French guides of the same edition are chosen together.
#'
#' @param pdf_paths The `pdf_freq` element of [detect_formats()].
#' @param layout Canonical layout tibble (name/start/end), or `NULL`.
#' @return `pdf_paths` with `eng`/`fra` possibly replaced.
#' @keywords internal
.pumf_pdf_choose_candidates <- function(pdf_paths, layout) {
  cands <- pdf_paths$candidates
  if (is.null(cands) || !length(cands) || is.null(layout) || nrow(layout) == 0L)
    return(pdf_paths)

  score <- vapply(cands, function(c) {
    h  <- c$header
    li <- match(h$name, layout$name)
    ok <- !is.na(li) & !is.na(h$position) & h$position == layout$start[li] &
      (is.na(h$length) | h$length == layout$end[li] - layout$start[li] + 1L)
    ok[is.na(ok)] <- FALSE
    sum(ok) / max(1L, nrow(h))
  }, numeric(1L))

  for (lang in c("eng", "fra")) {
    i <- which(vapply(cands, `[[`, character(1L), "lang") == lang)
    if (!length(i)) next
    # Only override the block-count pick when the layout actually discriminates;
    # a guide with no positions at all must not displace one that parses more.
    best <- i[[which.max(score[i])]]
    if (score[[best]] >= .pumf_pdf_min_pos_rate) pdf_paths[[lang]] <- cands[[best]]$path
  }
  pdf_paths
}

#' Validate a PDF frequency parse against the microdata
#'
#' For every variable the PDF dictionary describes, compares the per-code
#' frequencies printed in the guide against a tabulation of the actual data
#' file.  This is what turns the PDF from an unverifiable transcription into a
#' checkable source: a variable whose counts do not reconcile is never used to
#' repair a label.
#'
#' @param pdf Parser output from [parse_pdf_freq_codebook()].
#' @param layout Canonical layout tibble (name/start/end), or `NULL` for CSV data.
#' @param data_path Path to the microdata file, or `NULL` when none was found.
#' @param data_encoding Encoding of the data file.
#' @return Tibble with columns `block`, `name`, `status`, `n_codes`,
#'   `n_matched`, `note` -- one row per guide block, so a name documented once
#'   per module of a multi-module guide gets one row per module.
#' @keywords internal
.pumf_validate_pdf_freqs <- function(pdf, layout, data_path,
                                     data_encoding = "CP1252") {
  # Keyed on block, not name: a guide covering several linked modules documents
  # a shared key once per module, and only one of those blocks describes the
  # file being parsed.  Checking each block separately is what tells them apart
  # (see .pumf_pdf_resolve_duplicate_blocks()).
  #
  # Continuous variables print only a `lo : hi` range row and so have no rows in
  # `freqs`; they are still checkable (the range row's count must equal the
  # number of non-blank values), so include them.
  blocks <- sort(union(unique(pdf$freqs$block), unique(pdf$ranges$block)))
  bname  <- stats::setNames(pdf$variables$name, as.character(pdf$variables$block))
  vars   <- unname(bname[as.character(blocks)])
  unchecked <- function(note) tibble::tibble(
    block = blocks, name = vars, status = "unchecked", n_codes = NA_integer_,
    n_matched = NA_integer_, note = note)

  if (is.null(data_path) || !file.exists(data_path)) return(unchecked("no data file"))
  is_fwf <- !is.null(layout) && nrow(layout) > 0L
  # A very large flat file is not worth tabulating during metadata parsing;
  # skip validation rather than stall the pipeline (no repairs will be made).
  if (file.size(data_path) > 500e6) return(unchecked("data file too large"))

  getcol <- tryCatch(.pumf_column_values(data_path, layout, is_fwf, data_encoding),
                     error = function(e) NULL)
  if (is.null(getcol)) return(unchecked("data file unreadable"))

  rng <- pdf$ranges
  # One tabulation per column, reused across a column's several blocks.
  tab_cache <- new.env(parent = emptyenv())

  out <- lapply(blocks, function(b) {
    v <- bname[[as.character(b)]]
    row <- function(status, n_codes = NA_integer_, n_matched = NA_integer_,
                    note = NA_character_)
      tibble::tibble(block = b, name = v, status = status,
                     n_codes = as.integer(n_codes),
                     n_matched = as.integer(n_matched), note = note)

    if (!exists(v, envir = tab_cache, inherits = FALSE)) {
      raw <- tryCatch(getcol(v), error = function(e) NULL)
      # Tabulate first, then normalise the (few) distinct values: normalising
      # row by row would run .code_chr() millions of times per file.
      assign(v, if (is.null(raw)) NULL else table(raw[nzchar(raw)]),
             envir = tab_cache)
    }
    tab <- get(v, envir = tab_cache, inherits = FALSE)

    if (is.null(tab)) return(row("unchecked", note = "variable not in data"))
    if (!length(tab)) return(row("unchecked", note = "column is empty"))
    obs <- vapply(split(as.integer(tab), .pumf_norm_code(names(tab))), sum,
                  integer(1L))

    pf  <- pdf$freqs[pdf$freqs$block == b, , drop = FALSE]
    exp <- stats::setNames(pf$freq, .pumf_norm_code(pf$val))
    exp <- exp[!duplicated(names(exp))]
    rr  <- rng[rng$block == b, , drop = FALSE]

    # Range-only variable (no discrete codes): the range row must account for
    # every non-blank value in the column.
    if (!length(exp)) {
      if (nrow(rr) == 0L) return(row("unchecked", 0L, 0L, "no frequency table"))
      return(if (sum(as.integer(tab)) == rr$freq[[1L]])
               row("continuous", 0L, 0L, "range row matches row count")
             else row("mismatch", 0L, 0L, "range row does not match row count"))
    }

    common <- intersect(names(obs), names(exp))
    if (!length(common))
      return(row("unchecked", length(exp), 0L, "no overlapping codes"))

    counts_ok <- all(as.integer(obs[common]) == exp[common])
    # Codes the guide reports as non-empty but that are absent from the data.
    absent <- setdiff(names(exp)[exp > 0], names(obs))
    extra  <- setdiff(names(obs), names(exp))

    if (counts_ok && !length(extra) && !length(absent))
      return(row("validated", length(exp), length(common)))

    # Continuous variable: the guide prints a `lo : hi` range row in place of
    # per-value rows, so the data values outside the code list are expected and
    # must sum to that row's frequency.
    if (counts_ok && !length(absent) && nrow(rr) > 0L &&
        sum(as.integer(obs[extra])) == rr$freq[[1L]])
      return(row("continuous", length(exp), length(common)))

    note <- paste(c(
      if (!counts_ok) "code counts differ",
      if (length(absent)) paste0(length(absent), " documented codes absent from data"),
      if (length(extra))  paste0(length(extra), " data values not documented")
    ), collapse = "; ")
    row("mismatch", length(exp), length(common), note)
  })

  dplyr::bind_rows(out)
}


# ---------------------------------------------------------------------------
# Label repair
# ---------------------------------------------------------------------------

#' Repair command-file labels from a validated PDF dictionary
#'
#' Compares every variable and value label in the merged command-file metadata
#' against the PDF dictionary, fills in labels the command file left blank,
#' replaces labels the PDF demonstrably extends (the upstream-truncation
#' fingerprint), and records every divergence -- acted on or not -- in a ledger.
#'
#' A repair is withheld only where the frequency check actively contradicts the
#' PDF parse (status `"mismatch"`); a variable the check could not reach is not
#' evidence against it. Each ledger row carries the variable's validation status
#' so corroborated repairs are distinguishable from merely uncontradicted ones.
#'
#' @param metadata Merged canonical metadata (`variables`/`codes`/`layout`).
#' @param pdf Parser output from [parse_pdf_freq_codebook()].
#' @param validation Output of [.pumf_validate_pdf_freqs()].
#' @return List with elements `metadata` (possibly modified) and `repairs`
#'   (the ledger tibble).
#' @keywords internal
.pumf_apply_pdf_repairs <- function(metadata, pdf, validation) {
  ledger <- list()

  # A repair is refused only when the frequency check actively *contradicts*
  # the parse ("mismatch") -- that is the signal of a mis-read block, e.g.
  # labels attached to the wrong variable.  A variable the check simply could
  # not reach ("unchecked": absent from this module's data file, no data file
  # at all) is not evidence against the parse, and the repair rule is already
  # self-corroborating: it fires only when the guide's text demonstrably
  # extends the command file's for that same variable or code.  The ledger
  # records each variable's validation status either way, so a repair made
  # without corroboration is visible rather than implied.
  vstatus <- function(nm) {
    i <- match(nm, validation$name)
    if (is.na(i)) "not documented" else validation$status[[i]]
  }

  add <- function(kind, name, val, lang, from, to, action, reason, validation)
    ledger[[length(ledger) + 1L]] <<- tibble::tibble(
      kind = kind, name = name, val = val, lang = lang,
      label_command_file = from, label_pdf = to,
      action = action, reason = reason, validation = validation)

  # Truncation ceilings, derived per label field from the command file itself:
  # a "repair" is only credible where the label it replaces was damaged.
  tw_var <- .pumf_truncation_width(c(metadata$variables$label_en,
                                     metadata$variables$label_fr))
  tw_cod <- .pumf_truncation_width(c(metadata$codes$label_en,
                                     metadata$codes$label_fr))
  no_trunc <- "PDF label is longer but the command-file label is not truncated"
  # A repair needs one of the two damage signatures: the label sits at the
  # command file's truncation ceiling, or it is what survives a dropped prefix.
  damaged <- function(cmd, new, width)
    .pumf_at_truncation(cmd, width) || .pumf_left_truncated(cmd, new)

  # ---- variable labels ----
  pv <- pdf$variables[!duplicated(pdf$variables$name), , drop = FALSE]
  vi <- match(metadata$variables$name, pv$name)
  for (lang in c("en", "fr")) {
    col <- paste0("label_", lang)
    for (i in which(!is.na(vi))) {
      nm  <- metadata$variables$name[[i]]
      cmd <- .pumf_norm_label(metadata$variables[[col]][[i]])
      new <- .pumf_norm_label(pv[[col]][[vi[[i]]]])
      act <- .pumf_repair_action(cmd, new)
      if (act == "ok") next
      st  <- vstatus(nm)
      if (act %in% c("fill", "repair") && st == "mismatch") {
        add("variable", nm, NA_character_, lang, cmd, new, "flagged",
            "PDF frequencies contradict the data file", st)
        next
      }
      if (act == "flag") {
        add("variable", nm, NA_character_, lang, cmd, new, "flagged",
            "labels differ but the PDF does not extend the command file", st)
        next
      }
      if (act == "repair" && !damaged(cmd, new, tw_var)) {
        add("variable", nm, NA_character_, lang, cmd, new, "flagged", no_trunc, st)
        next
      }
      metadata$variables[[col]][[i]] <- new
      add("variable", nm, NA_character_, lang, cmd, new,
          if (act == "fill") "filled" else "repaired",
          if (act == "fill") "command file had no label"
          else "PDF label extends the truncated command-file label", st)
    }
  }

  # ---- value labels ----
  if (nrow(metadata$codes) > 0L && nrow(pdf$codes) > 0L) {
    pc  <- pdf$codes
    key_cmd <- paste0(metadata$codes$name, "\r", .pumf_norm_code(metadata$codes$val))
    key_pdf <- paste0(pc$name, "\r", .pumf_norm_code(pc$val))
    pc  <- pc[!duplicated(key_pdf), , drop = FALSE]
    key_pdf <- key_pdf[!duplicated(key_pdf)]
    ci  <- match(key_cmd, key_pdf)
    for (lang in c("en", "fr")) {
      col <- paste0("label_", lang)
      for (i in which(!is.na(ci))) {
        nm  <- metadata$codes$name[[i]]
        vv  <- metadata$codes$val[[i]]
        cmd <- .pumf_norm_label(metadata$codes[[col]][[i]])
        new <- .pumf_norm_label(pc[[col]][[ci[[i]]]])
        act <- .pumf_repair_action(cmd, new)
        if (act == "ok") next
        st  <- vstatus(nm)
        if (act %in% c("fill", "repair") && st == "mismatch") {
          add("code", nm, vv, lang, cmd, new, "flagged",
              "PDF frequencies contradict the data file", st)
          next
        }
        if (act == "flag") {
          add("code", nm, vv, lang, cmd, new, "flagged",
              "labels differ but the PDF does not extend the command file", st)
          next
        }
        if (act == "repair" && !damaged(cmd, new, tw_cod)) {
          add("code", nm, vv, lang, cmd, new, "flagged", no_trunc, st)
          next
        }
        metadata$codes[[col]][[i]] <- new
        add("code", nm, vv, lang, cmd, new,
            if (act == "fill") "filled" else "repaired",
            if (act == "fill") "command file had no label"
            else "PDF label extends the truncated command-file label", st)
      }
    }

    # Codes the guide documents (and the data contains) that the command file
    # never declared.  Reported, not injected: a missing code is a registry
    # `codes_supplement` decision, not something to infer during parsing.
    # Restricted to variables the command file treats as categorical.  Where it
    # declares no codes at all the variable is continuous, and the guide's
    # zero-value rows ("No hours", "None", "Aucun don") are labels on a valid
    # numeric zero rather than codes the command file forgot.
    coded <- unique(metadata$codes$name)
    missing_keys <- setdiff(key_pdf[pc$name %in% coded], key_cmd)
    # A continuous variable's sentinels (97/98/99, 99.5 ...) are declared by the
    # command file as a MISSING VALUES range rather than as codes.  The guide
    # lists them individually, which is not a divergence -- don't report it.
    in_missing <- function(nm, val) {
      i <- match(nm, metadata$variables$name)
      if (is.na(i)) return(FALSE)
      lo <- metadata$variables$missing_low[[i]]
      hi <- metadata$variables$missing_high[[i]]
      if (is.na(lo) || is.na(hi)) return(FALSE)
      num <- suppressWarnings(as.numeric(val))
      !is.na(num) && num >= lo && num <= hi
    }
    for (k in missing_keys) {
      j  <- match(k, key_pdf)
      nm <- pc$name[[j]]
      if (in_missing(nm, pc$val[[j]])) next
      st <- vstatus(nm)
      # Only worth reporting where the frequency check reconciled: otherwise
      # the "undeclared" code may just be an artefact of a mis-read block.
      if (!st %in% .pumf_pdf_status_ok) next
      add("code", nm, pc$val[[j]], "en", NA_character_,
          .pumf_norm_label(pc$label_en[[j]]), "flagged",
          "code documented in the PDF but absent from the command file", st)
    }
  }

  repairs <- if (length(ledger)) dplyr::bind_rows(ledger) else .pumf_empty_repairs()
  list(metadata = metadata, repairs = repairs)
}

.pumf_empty_repairs <- function() {
  tibble::tibble(kind = character(), name = character(), val = character(),
                 lang = character(), label_command_file = character(),
                 label_pdf = character(), action = character(),
                 reason = character(), validation = character())
}


# ---------------------------------------------------------------------------
# Stage 2 entry point
# ---------------------------------------------------------------------------

# Run the whole PDF cross-check for one metadata directory: parse the guide,
# validate its frequencies against the data, apply repairs, write the two
# side-car CSVs.  Returns the (possibly repaired) metadata.
.pumf_pdf_crosscheck <- function(metadata, pdf_paths, version_dir, metadata_dir,
                                 file_mask = NULL, data_encoding = "CP1252") {
  pdf_paths <- .pumf_pdf_choose_candidates(pdf_paths, metadata$layout)
  pdf <- tryCatch(parse_pdf_freq_codebook(pdf_paths$eng, fra_pdf = pdf_paths$fra),
                  error = function(e) {
                    warning("Could not parse the PDF data dictionary '",
                            basename(pdf_paths$eng), "': ", conditionMessage(e),
                            call. = FALSE)
                    NULL
                  })
  if (is.null(pdf) || nrow(pdf$variables) == 0L) return(metadata)

  # One guide can document several linked modules; drop the blocks whose
  # position/length contradict this module's layout.
  pdf <- .pumf_pdf_select_blocks(pdf, metadata$layout)

  data_path <- tryCatch(
    .find_pumf_data_file(version_dir, file_mask,
                         prefer_fwf = !is.null(metadata$layout)),
    error = function(e) NULL)

  validation <- .pumf_validate_pdf_freqs(pdf, metadata$layout, data_path,
                                          data_encoding = data_encoding)
  # Blocks the position filter could not separate are settled by which one
  # reconciles with this module's data.
  sel        <- .pumf_pdf_resolve_duplicate_blocks(pdf, validation)
  pdf        <- sel$pdf
  validation <- sel$validation

  # ---- does this guide describe this file at all? ----
  pa      <- .pumf_pdf_position_agreement(pdf, metadata$layout)
  checked <- validation$status != "unchecked"
  ok_rate <- if (any(checked))
    mean(validation$status[checked] %in% .pumf_pdf_status_ok) else NA_real_
  freq_ok <- is.na(ok_rate) || ok_rate >= .pumf_pdf_min_freq_rate
  pos_ok  <- !is.na(pa$rate) && pa$rate >= .pumf_pdf_min_pos_rate

  if (!freq_ok && !pos_ok) {
    # Neither channel corroborates: this is not the guide for this file.  Keep
    # the validation table -- it is the evidence for the decision -- but record
    # an empty ledger so nothing downstream reads divergences off a document we
    # just rejected.
    readr::write_csv(validation, file.path(metadata_dir, "pdf_validation.csv"), na = "")
    readr::write_csv(.pumf_empty_repairs(),
                     file.path(metadata_dir, "label_repairs.csv"), na = "")
    message(sprintf(
      "PDF data dictionary '%s' does not describe this data file (%d/%d variables frequency-validated, %d/%d field positions agree); ignoring it.",
      basename(pdf_paths$eng), sum(validation$status %in% .pumf_pdf_status_ok),
      nrow(validation), pa$agree, pa$n))
    return(metadata)
  }

  if (!freq_ok) {
    # The positions say this is the right guide, so the counts are not evidence
    # against any particular variable -- they are simply on another base.  Treat
    # them as absent for the blocks the layout confirms; blocks the layout does
    # *not* confirm keep their "mismatch" and stay barred from repair.
    conf <- pdf$variables$block[pa$ok]
    i <- validation$status == "mismatch" & validation$block %in% conf
    validation$status[i] <- "unchecked"
    validation$note[i]   <- "guide frequencies use a different population"
  }

  res <- .pumf_apply_pdf_repairs(metadata, pdf, validation)

  readr::write_csv(validation, file.path(metadata_dir, "pdf_validation.csv"), na = "")
  readr::write_csv(res$repairs, file.path(metadata_dir, "label_repairs.csv"), na = "")

  n_rep <- sum(res$repairs$action %in% c("repaired", "filled"))
  n_flag <- sum(res$repairs$action == "flagged")
  n_ok  <- sum(validation$status %in% .pumf_pdf_status_ok)
  if (n_rep > 0L || n_flag > 0L)
    message(sprintf(
      "PDF data dictionary '%s': %d/%d variables frequency-validated against the data%s; %s. See pumf_label_repairs().",
      basename(pdf_paths$eng), n_ok, nrow(validation),
      if (!freq_ok) sprintf(", %d/%d field positions agree", pa$agree, pa$n) else "",
      paste(c(if (n_rep)  sprintf("%d labels repaired", n_rep),
              if (n_flag) sprintf("%d divergences flagged", n_flag)),
            collapse = ", ")))

  res$metadata
}


# ---------------------------------------------------------------------------
# User-facing accessor
# ---------------------------------------------------------------------------

# Resolve the metadata directory (module-aware) backing a get_pumf() tbl.
.pumf_meta_dir_from_tbl <- function(tbl) {
  prov <- .pumf_lookup_con(tbl$src$con)
  if (is.null(prov))
    stop("'tbl' has no pumf provenance. Was it created by get_pumf()?",
         call. = FALSE)
  if (identical(prov$series, "LFS"))
    return(NULL)
  module <- .pumf_tbl_module(tbl, prov)
  reg    <- pumf_registry_lookup(prov$series, prov$version)
  mods   <- .pumf_entry_modules(reg)
  subdir <- if (!is.null(module) && !is.null(mods) && !is.null(mods[[module]]))
    mods[[module]]$meta_subdir else NULL
  base   <- file.path(prov$cache_path, prov$series, prov$version, "metadata")
  if (is.null(subdir)) base else file.path(base, subdir)
}

#' Inspect label repairs and divergences found against the PDF data dictionary
#'
#' Statistics Canada's PUMF command files routinely ship truncated value and
#' variable labels -- hard cuts at 60 characters, dropped leading text, dropped
#' interior text.  The damage is upstream of the command files (SAS, SPSS and
#' Stata carry byte-identical text), but the same survey's user guide contains a
#' data dictionary with the full label text.
#'
#' For surveys that ship such a guide, `canpumf` parses it during metadata
#' preparation, validates it against the microdata using the per-code
#' frequencies the guide prints, and then repairs labels the guide demonstrably
#' extends.  This function returns the ledger of what it found: every
#' divergence between the command file and the guide, whether or not it was
#' acted on.
#'
#' `action` is one of:
#' \describe{
#'   \item{`repaired`}{The command-file label was replaced, because the guide's
#'     text extends it *and* the command-file label carries a damage signature:
#'     either it sits at the width the command file's labels were hard-cut to, or
#'     it is what a dropped prefix leaves behind (a strict suffix of the guide's
#'     text beginning mid-sentence).  A guide that merely words a label
#'     differently, or prefixes an editorial note onto one the command file has
#'     in full, is `flagged` instead.}
#'   \item{`filled`}{The command file had no label at all; the guide supplied one.}
#'   \item{`flagged`}{Recorded but not acted on -- the two simply differ, or the
#'     variable's frequencies contradicted the data file, or the guide documents
#'     a code the command file never declared.  Read `reason` for which.}
#' }
#'
#' The `validation` column carries the variable's frequency-check status
#' (`validated`, `continuous`, `unchecked`, `mismatch`, or `not documented`), so
#' a repair corroborated against the microdata can be told apart from one the
#' check simply could not reach.  See [pumf_freq_validation()].
#'
#' @param tbl A lazy `dplyr::tbl()` returned by [get_pumf()].
#' @param action Optional filter, e.g. `"repaired"` or `c("repaired", "filled")`.
#'
#' @return A tibble with columns `kind` (`"variable"`/`"code"`), `name`, `val`,
#'   `lang`, `label_command_file`, `label_pdf`, `action`, `reason` and
#'   `validation`.  Zero rows when the survey ships no parseable PDF dictionary
#'   or nothing diverged.
#'
#' @seealso [pumf_var_labels()], [pumf_freq_validation()]
#'
#' @examples
#' \donttest{
#' gss <- get_pumf("GSS", "Cycle 16 (2002)")
#' if (!is.null(gss)) {
#'   pumf_label_repairs(gss, action = "repaired")
#'   close_pumf(gss)
#' }
#' }
#' @export
pumf_label_repairs <- function(tbl, action = NULL) {
  meta_dir <- .pumf_meta_dir_from_tbl(tbl)
  path <- if (is.null(meta_dir)) NULL else file.path(meta_dir, "label_repairs.csv")
  if (is.null(path) || !file.exists(path)) return(.pumf_empty_repairs())
  out <- readr::read_csv(path, col_types = readr::cols(.default = "c"),
                         show_col_types = FALSE)
  if (!is.null(action)) out <- out[out$action %in% action, , drop = FALSE]
  tibble::as_tibble(out)
}

#' Inspect the PDF-versus-microdata frequency validation
#'
#' Companion to [pumf_label_repairs()].  Reports, per variable, whether the
#' frequencies printed in the survey's PDF data dictionary reconcile against a
#' tabulation of the actual data file.  This is the evidence `canpumf` uses to
#' decide whether a label from the guide may be trusted.
#'
#' `status` is one of:
#' \describe{
#'   \item{`validated`}{Every documented code's count matches the data exactly,
#'     and the data holds no undocumented values.}
#'   \item{`continuous`}{The documented sentinel codes match and the remaining
#'     values are accounted for by the guide's `lo : hi` range row -- a
#'     continuous variable, correctly parsed.}
#'   \item{`mismatch`}{Counts disagree; nothing from the guide is used for this
#'     variable.}
#'   \item{`unchecked`}{The check could not be run: no data file, no overlapping
#'     codes, or -- common for a multi-module survey -- the variable belongs to
#'     a sibling module and is absent from this one's data file.  Also used
#'     when the guide's field positions reproduce the command file's layout but
#'     none of its counts reproduce the data (note `"guide frequencies use a
#'     different population"`): the guide is the right document, its tables were
#'     simply tabulated on another base, so the counts are treated as absent
#'     rather than as evidence against the parse.}
#' }
#'
#' A guide corroborated by *neither* channel -- neither its counts nor its field
#' positions -- is rejected as the wrong document.  Its validation table is still
#' returned (it is the evidence for that decision), but the repair ledger is
#' empty and no label from it is used.
#'
#' @param tbl A lazy `dplyr::tbl()` returned by [get_pumf()].
#' @return A tibble with columns `block` (the guide block the check ran on),
#'   `name`, `status`, `n_codes`, `n_matched` and `note`; zero rows when the
#'   survey ships no parseable PDF dictionary.
#'
#' @seealso [pumf_label_repairs()]
#'
#' @examples
#' \donttest{
#' gss <- get_pumf("GSS", "Cycle 16 (2002)")
#' if (!is.null(gss)) {
#'   table(pumf_freq_validation(gss)$status)
#'   close_pumf(gss)
#' }
#' }
#' @export
pumf_freq_validation <- function(tbl) {
  meta_dir <- .pumf_meta_dir_from_tbl(tbl)
  path <- if (is.null(meta_dir)) NULL else file.path(meta_dir, "pdf_validation.csv")
  if (is.null(path) || !file.exists(path))
    return(tibble::tibble(name = character(), status = character(),
                          n_codes = integer(), n_matched = integer(),
                          note = character()))
  tibble::as_tibble(readr::read_csv(
    path, col_types = readr::cols(name = "c", status = "c", n_codes = "i",
                                  n_matched = "i", note = "c"),
    show_col_types = FALSE))
}
