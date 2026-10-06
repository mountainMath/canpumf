# R/lfs_pipeline.R — LFS longitudinal database pipeline.
#
# The engine shared with other longitudinal series lives in R/longitudinal.R,
# together with the versions-table and append helpers (`.long_*(con, spec,
# ...)`); this file holds the LFS spec, the LFS data-file handling, the
# merged-metadata accessors and the lfs_get_pumf() wrapper.
#
# All LFS versions share a single DuckDB at <cache_path>/LFS/LFS.duckdb.
# Per-version zip and metadata live at <cache_path>/LFS/<version>/.
# The DuckDB holds up to three tables (the spec defaults derived from the
# series name, see .long_table_name() / .long_versions_table()):
#   lfs_eng      — English-labeled rows from all loaded versions
#   lfs_fra      — French-labeled rows from all loaded versions
#   lfs_versions — tracking table (version, type, survyear, survmnth, ...)
#
# SURVYEAR and SURVMNTH are stored as INTEGER in both data tables (not labeled)
# so that filtering `WHERE SURVYEAR = 2023 AND SURVMNTH = 6` works directly.
#
# Connection discipline: every write block opens a fresh RW connection and
# closes it before returning.  The returned tbl holds an open RO connection;
# callers who need to load additional versions must first collect() or
# disconnect the previous tbl (DuckDB allows at most one RW connection).


# ---- Internal helpers -------------------------------------------------------

# Parse version string → "annual" | "monthly"
.lfs_version_type <- function(v) {
  if (grepl("^[0-9]{4}$", v))        "annual"
  else if (grepl("^[0-9]{4}-[0-9]{2}$", v)) "monthly"
  else stop("Invalid LFS version string '", v,
            "'. Expected YYYY (annual) or YYYY-MM (monthly).")
}

.lfs_survyear <- function(v) as.integer(substr(v, 1L, 4L))
.lfs_survmnth <- function(v) {
  if (grepl("-", v, fixed = TRUE)) as.integer(substr(v, 6L, 7L))
  else NA_integer_
}

# Locate LFS CSV data files in a version directory.
#
# StatCan has shipped LFS data in several formats over the years:
#   (a) One annual file:          pub<YYYY>.csv        (all 12 months)
#   (b) 12 bundled monthly files: pub<MM><YY>.csv × 12 (one per month)
#   (c) Single monthly release:   pub<MM><YY>.csv × 1  (current-year update)
#
# All cases are handled identically — every matching file is returned and the
# caller binds them.  We try the known "pub*.csv" prefix first; if that yields
# nothing we fall back to any non-metadata CSV so the function keeps working if
# StatCan ever renames their files.
#
# Returns a sorted character vector of full paths, never length-0 (the caller
# stops on empty).  Files inside metadata/ are always excluded.
.lfs_find_data_files <- function(version_dir) {
  all_files <- list.files(version_dir, recursive = TRUE, full.names = TRUE)
  all_files <- all_files[!startsWith(
    normalizePath(all_files, mustWork = FALSE),
    normalizePath(file.path(version_dir, "metadata"), mustWork = FALSE)
  )]

  # Primary pattern — every known StatCan LFS release uses this prefix
  primary <- all_files[grepl("^pub[^/]*\\.csv$", basename(all_files),
                              ignore.case = TRUE, perl = TRUE)]
  if (length(primary) > 0L) return(sort(primary))

  # Fallback — any CSV that doesn't look like a codebook / layout file
  fallback <- all_files[
    grepl("\\.csv$", all_files, ignore.case = TRUE) &
    !grepl("codebook|layout|readme|lisezmoi|variables|codes",
            basename(all_files), ignore.case = TRUE)]
  sort(fallback)
}


# Classify one LFS data filename as "annual" or "monthly" based on the digit
# string that follows the "pub" prefix.  The heuristic works for all known
# StatCan naming conventions:
#
#   pub2023.csv   → first two digits "20" > 12  → annual  (year = 2023)
#   pub0120.csv   → first two digits "01" ≤ 12  → monthly (Jan 2020)
#   pub1223.csv   → first two digits "12" ≤ 12  → monthly (Dec 2023)
#   pub012024.csv → first two digits "01" ≤ 12  → monthly (Jan 2024, long year)
#
# Non-pub filenames (from the fallback path) are classified as "unknown".
.lfs_file_format <- function(path) {
  nm <- tolower(basename(path))
  if (!startsWith(nm, "pub")) return("unknown")
  # Capture the full digit string without stripping leading zeros:
  #   pub0223.csv → "0223" (not "223"), first two chars "02" → month 2 → monthly
  #   pub2023.csv → "2023", first two chars "20" → 20 > 12 → annual
  digits <- gsub("(?i)^pub([0-9]+)\\.csv$", "\\1", nm, perl = TRUE)
  if (!grepl("^[0-9]+$", digits)) return("unknown")
  if (as.integer(substr(digits, 1L, 2L)) > 12L) "annual" else "monthly"
}


# Build the labeled data frame for one LFS version (the spec's `build`).
# `int_cols` (SURVYEAR, SURVMNTH, REC_NUM from the LFS registry entry's
# force_integer fixup) stay INTEGER and unlabelled, see .long_label_frame().
.lfs_build_version <- function(version_dir, label_col, version = NULL,
                               int_cols = .pumf_longitudinal_entry("LFS")$data_fixups$force_integer) {
  meta <- read_metadata(file.path(version_dir, "metadata"))

  # Locate data files — supports all three StatCan shipping formats:
  #   (a) single annual file, (b) 12 bundled monthly files, (c) one monthly
  data_files <- .lfs_find_data_files(version_dir)
  if (length(data_files) == 0L)
    stop("No LFS data files found in ", version_dir,
         ".\nExpected pub*.csv files (or any non-metadata CSV as fallback).")

  # A directory holding both formats is read whole; the caller sorts it out.
  formats <- vapply(data_files, .lfs_file_format, character(1L))
  if (any(formats == "annual") && any(formats == "monthly"))
    warning("LFS version directory contains both annual- and monthly-format ",
            "files; all will be combined. Remove duplicates if rows are ",
            "double-counted.", call. = FALSE)

  data <- bind_rows(lapply(data_files, function(p) {
    df <- readr::read_csv(p,
                           col_types = readr::cols(.default = "c"),
                           locale    = readr::locale(encoding = "CP1252"),
                           show_col_types = FALSE)
    names(df) <- toupper(names(df))
    df
  }))

  .long_label_frame(data, meta, label_col, int_cols, series = "LFS",
                    where = paste0("\nCheck that the correct data file(s) are in ",
                                   version_dir))
}

# ---- Longitudinal spec ------------------------------------------------------

# LFS as a longitudinal series (R/longitudinal.R): 2006 onward in the current
# PUMF layout, annual files superseding the monthly ones.  Database, table
# and versions-table names are the defaults derived from the series name
# (LFS.duckdb, lfs_eng/lfs_fra, lfs_versions).
.lfs_spec <- function() {
  entry <- .pumf_longitudinal_entry("LFS")
  list(
    series       = "LFS",
    entry        = entry,
    annual_files = TRUE,
    example      = "2024",
    validate     = .lfs_version_type,
    available    = function() list_available_lfs_pumf_versions()$version,
    prepare      = function(version, cache_path, refresh, redownload) {
      version_dir <- pumf_locate_or_download("LFS", version,
                                             cache_path = cache_path,
                                             refresh    = refresh,
                                             redownload = redownload)
      pumf_parse_metadata(version_dir, refresh = refresh)
      version_dir
    },
    build        = function(version_dir, label_col, version)
      .lfs_build_version(version_dir, label_col, version,
                         int_cols = entry$data_fixups$force_integer),
    finalize     = .lfs_relocate_gender,
    variables    = function(cache_path, versions)
      .lfs_merged_metadata(cache_path, versions, "variables"),
    codes        = function(cache_path, versions)
      .lfs_merged_metadata(cache_path, versions, "codes"),
    # get_lfs_timeline(): the recodes.csv source and the column of the
    # harmonisation tables holding this series' variable names and scale.
    timeline     = list(col = "lfs", scale = "lfs_scale", from = NULL))
}

# Keep SEX and GENDER side by side in the returned tbl: the shared table
# appends GENDER (~2020) after every older column.  The stored table is not
# touched; this runs on the lazy tbl handed back to the caller.
.lfs_relocate_gender <- function(t) {
  cn <- colnames(t)
  if (!all(c("SEX", "GENDER") %in% cn)) return(t)
  if (which(cn == "SEX") > which(cn == "GENDER"))
    relocate(t, "SEX", .before = "GENDER")
  else
    relocate(t, "GENDER", .after = "SEX")
}

# Metadata across every loaded LFS version (`versions`, oldest first).
# "variables": the most recent label wins per variable, since the shared
# table is the union of all versions' columns and variables such as GENDER
# (~2020) are absent from the older versions' variables.csv.  "codes": every
# distinct wording is kept, suffixed per version the way each version was
# labelled when it was appended (.pumf_unique_code_labels()), so the shared
# table can hold an older spelling next to the current one.
.lfs_merged_metadata <- function(cache_path, versions,
                                 which = c("variables", "codes")) {
  which <- match.arg(which)
  parts <- lapply(versions, function(v) {
    md <- file.path(cache_path, "LFS", v, "metadata")
    if (!dir.exists(md)) return(NULL)
    tryCatch(
      if (which == "codes") {
        # codes.csv alone (read_metadata() would also require variables.csv)
        .pumf_unique_code_labels(readr::read_csv(
          file.path(md, "codes.csv"), col_types = .metadata_codes_cols,
          show_col_types = FALSE))
      } else read_metadata(md)$variables,
      error = function(e) NULL)
  })
  all <- do.call(rbind, parts[!vapply(parts, is.null, logical(1L))])
  if (is.null(all) || nrow(all) == 0L)
    stop("No LFS metadata found in any version directory.", call. = FALSE)
  if (which == "variables")
    all[!duplicated(all$name, fromLast = TRUE), , drop = FALSE]
  else
    all[!duplicated(all[, c("name", "val", "label_en", "label_fr")]), ,
        drop = FALSE]
}


# ---- Public function --------------------------------------------------------

#' Get Labour Force Survey PUMF data from a shared longitudinal DuckDB
#'
#' Manages a single `LFS.duckdb` file that accumulates all downloaded LFS
#' versions.  Each call either retrieves already-loaded data or downloads,
#' parses, labels, and appends a new version.
#'
#' **Version types**:
#' - `"YYYY"` (e.g. `"2023"`) — annual file released by StatCan after year-end.
#' - `"YYYY-MM"` (e.g. `"2024-06"`) — monthly file for the current year.
#'
#' When an annual file for year Y is loaded and monthly files for that year are
#' already in the database, the monthly rows are replaced (supersession).
#' Conversely, if an annual for year Y is already loaded, requesting a monthly
#' for that year returns the annual data filtered to that month without
#' re-downloading.
#'
#' **Connection note**: the returned `tbl` holds an open DuckDB connection.
#' Loading a second version (i.e. calling `lfs_get_pumf` again while holding
#' the first result) requires the first tbl's connection to be closed first.
#' Use [close_pumf()] or `dplyr::collect()` the result before the next call.
#'
#' @param version LFS version string (`"YYYY"` or `"YYYY-MM"`), or `NULL` to
#'   report database state and return the full table.
#' @param lang `"eng"` (default) or `"fra"`.
#' @param cache_path Root cache directory.
#' @param refresh `FALSE` (default), `TRUE` (re-parse and re-label the
#'   specified version from the cached raw files), or `"auto"` (download all
#'   versions not yet in the database).  `refresh = TRUE` requires a non-NULL
#'   `version`.
#' @param redownload If `TRUE`, delete the cached zip and extracted content for
#'   the specified version and re-download from StatCan before rebuilding.
#'   Implies `refresh = TRUE`.  Requires a non-NULL `version`.
#' @param read_only Open the DuckDB connection in read-only mode (default
#'   `TRUE`).  Pass `FALSE` to allow write access to the LFS DuckDB.
#'
#' @return A lazy `dplyr::tbl()`, or `invisible(NULL)` when `version = NULL`
#'   and no data has been loaded.
#' @keywords internal
lfs_get_pumf <- function(version    = NULL,
                          lang       = "eng",
                          cache_path = getOption("canpumf.cache_path",
                                                  tempdir()),
                          refresh    = FALSE,
                          redownload = FALSE,
                          read_only  = TRUE) {
  .long_get_pumf(.lfs_spec(), version = version, lang = lang,
                 cache_path = cache_path, refresh = refresh,
                 redownload = redownload, read_only = read_only)
}
