# tools/verify_overrides.R — dev workflow for verifying registry manual
# overrides against the PDF documentation shipped with each PUMF.
# Not part of the package build (.Rbuildignore'd).
#
# Workflow (see "Override verification workflow" in .claude/CLAUDE.md):
#   devtools::load_all()
#   source("tools/verify_overrides.R")
#
#   vo_overrides()                       # all overrides + ledger status
#   vo_overrides(pending = TRUE)         # only what still needs checking
#   pdfs <- vo_find_pdfs("SGVP", "2018") # candidate PDFs in the cache
#   txt  <- vo_source_lines(pdfs[1])        # one element per text line
#   vo_locate(txt, "BRTHMACR")           # line numbers of candidate mentions
#   vo_context(txt, 1234)                # numbered snippet around a line
#   vo_record("SGVP", "2018", "codes_supplement", "BRTHMACR", "9",
#             source_file = basename(pdfs[1]), source_lines = "1234-1250",
#             status = "confirmed", note = "PDF lists only codes 1-8")
#
# Ledger statuses:
#   confirmed    — documentation explicitly supports the override
#   unverifiable — no usable PDF documentation exists in the cache
#   mismatch     — documentation contradicts the override (fix the registry!)
#   pending      — not yet checked

source(file.path("tests", "testthat", "helper-overrides.R"))

vo_ledger_path <- function() file.path("tests", "testthat", "override_verification.csv")

vo_read_ledger <- function() {
  utils::read.csv(vo_ledger_path(), colClasses = "character")
}

#' All registry overrides joined with their ledger verification status.
vo_overrides <- function(pending = FALSE) {
  ov <- enumerate_registry_overrides()
  led <- vo_read_ledger()
  idx <- match(override_key(ov), override_key(led))
  ov$status     <- ifelse(is.na(idx), "pending", led$status[idx])
  ov$source_file   <- ifelse(is.na(idx), "", led$source_file[idx])
  ov$source_lines  <- ifelse(is.na(idx), "", led$source_lines[idx])
  if (pending) ov <- ov[!ov$status %in% c("confirmed", "unverifiable"), ]
  ov
}

#' Candidate documentation PDFs for a survey version, searched recursively in
#' the cache.  Codebook/dictionary/user-guide files sort first.
vo_find_pdfs <- function(series, version = NULL,
                         cache_path = getOption("canpumf.cache_path", tempdir())) {
  dir <- if (is.null(version)) file.path(cache_path, series)
         else file.path(cache_path, series, version)
  if (!dir.exists(dir)) stop("No cache directory: ", dir)
  pdfs <- list.files(dir, pattern = "\\.pdf$", recursive = TRUE,
                     full.names = TRUE, ignore.case = TRUE)
  pref <- grepl("codebook|dictionar|cdbk|user.?guide|gid|pumf", basename(pdfs),
                ignore.case = TRUE)
  c(pdfs[pref], pdfs[!pref])
}

# Per-session cache of extracted text so repeated lookups are cheap and line
# numbers stay stable within a session (they are deterministic across sessions
# too — pdftools::pdf_text is reproducible for a given file).
.vo_text_cache <- new.env(parent = emptyenv())

#' PDF text as a character vector, one element per line (global line numbers).
vo_source_lines <- function(pdf_path) {
  key <- normalizePath(pdf_path)
  if (!is.null(.vo_text_cache[[key]])) return(.vo_text_cache[[key]])
  pages <- pdftools::pdf_text(pdf_path)
  lines <- unlist(strsplit(pages, "\n", fixed = TRUE), use.names = FALSE)
  .vo_text_cache[[key]] <- lines
  lines
}

#' Line numbers where a variable (or any pattern) is mentioned.
#' By default matches the name as a whole word, case-insensitively.
vo_locate <- function(lines, variable, fixed_word = TRUE) {
  pat <- if (fixed_word) paste0("\\b", variable, "\\b") else variable
  hits <- grep(pat, lines, ignore.case = TRUE)
  if (length(hits) == 0L) {
    message("No match for '", variable, "'")
    return(invisible(integer(0)))
  }
  data.frame(line = hits, text = trimws(lines[hits]), stringsAsFactors = FALSE)
}

#' Print a numbered snippet around a line for reading.
vo_context <- function(lines, at, before = 3L, after = 25L) {
  rng <- max(1L, at - before):min(length(lines), at + after)
  cat(sprintf("%6d | %s", rng, lines[rng]), sep = "\n")
  invisible(rng)
}

#' Append or update a ledger row and write the CSV (sorted, stable order).
vo_record <- function(series, version, override_type, variable = "", value = "",
                      source_file = "", source_lines = "", status, note = "",
                      checked_date = format(Sys.Date())) {
  stopifnot(status %in% c("confirmed", "unverifiable", "mismatch", "pending"))
  led <- vo_read_ledger()
  row <- data.frame(series = series, version = version,
                    override_type = override_type, variable = variable,
                    value = value, source_file = source_file, source_lines = source_lines,
                    status = status, checked_date = checked_date, note = note,
                    stringsAsFactors = FALSE)
  idx <- match(override_key(row), override_key(led))
  if (is.na(idx)) led <- rbind(led, row) else led[idx, ] <- row
  led <- led[order(led$series, led$version, led$override_type,
                   led$variable, led$value), ]
  utils::write.csv(led, vo_ledger_path(), row.names = FALSE)
  invisible(led)
}
