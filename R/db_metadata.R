# R/db_metadata.R -- The metadata a canpumf DuckDB file carries about itself.
#
# Every table Stage 3 (and the longitudinal write phase) writes is described
# inside the database, so that the file can be read, labelled and cited
# without the metadata/ directory, the registry or the package:
#
#   pumf_build_info    one row per table: which canpumf and duckdb built it
#                      and when (the build stamp of 0.7.0), plus where the
#                      data came from (source, URL/DOI, title, download date,
#                      data file, licence) and which registry fixups applied.
#   pumf_dictionary    the bilingual dictionary of every table (the same rows
#                      pumf_dictionary() returns): variable labels and
#                      descriptions, the value labels as applied, the registry
#                      sentinel labels, keyed by `table`.
#   COMMENT ON         every labelled column carries its variable label (in
#                      the table's language) and every table a one-line
#                      description, readable from any DuckDB client
#                      (duckdb_columns(), duckdb_tables()).
#
# The readers (.pumf_dictionary_from_prov(), .pumf_label_source(),
# .pumf_tbl_prov()) try the database first and fall back to metadata/ and the
# registry for a database built before these tables existed.  The readers
# never open a write connection (issue #18): .pumf_with_prov_con() uses the
# tbl's own connection when there is one and otherwise a transient read-only
# one, released without shutting the instance down.
#
# Stage 1 leaves a statcan_manifest.csv beside a Statistics Canada download
# (the Borealis source writes borealis_manifest.csv, R/borealis.R); Stage 3
# reads either to fill the provenance columns.  A manually deposited file has
# neither and is recorded as source "deposit".  The user can state or correct
# the provenance in a pumf_provenance.csv beside the data (and a registry
# entry in its `provenance` field); those values win over the manifests.


# ---- Download manifest (Stage 1) ---------------------------------------------

.statcan_manifest_file <- "statcan_manifest.csv"

# Record where a Statistics Canada download came from: written once the zip
# is in `dir` (the version directory, or the bundle directory of a Census
# vintage whose versions share one archive).  It survives refresh = TRUE
# (.pumf_clear_built() leaves the raw files alone) and goes with the
# directory on redownload.
.write_statcan_manifest <- function(dir, url, filename, series, version) {
  m <- data.frame(
    url      = url,
    filename = filename,
    fetched  = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    series   = series,
    version  = version,
    stringsAsFactors = FALSE)
  utils::write.csv(m, file.path(dir, .statcan_manifest_file),
                   row.names = FALSE, na = "")
  invisible(m)
}

# The Statistics Canada download manifest of a directory, or NULL.
.read_statcan_manifest <- function(dir) {
  f <- file.path(dir, .statcan_manifest_file)
  if (!file.exists(f)) return(NULL)
  m <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE,
                                colClasses = "character", na.strings = ""),
                error = function(e) NULL)
  if (is.null(m) || nrow(m) == 0L) NULL else m
}

# Statistics Canada publishes its PUMFs under the open licence; a Borealis
# deposit states its own (recorded in borealis_manifest.csv when the dataset
# declares one).
.statcan_licence     <- "Statistics Canada Open Licence"
.statcan_licence_url <- "https://www.statcan.gc.ca/en/reference/licence"

# Where the data of a build came from, for the build stamp: the Borealis
# manifest of the version directory, else the Statistics Canada manifest of
# the version directory or of the bundle directory (`source_dir`, the parent
# for a "1986/individuals" version), else a manual deposit.  `data_path` is
# the data file Stage 3 read.  The user's pumf_provenance.csv (version
# directory, then bundle directory) and the registry `provenance` list are
# laid over the result (.pumf_apply_user_provenance()).
.pumf_source_info <- function(version_dir, data_path = NULL,
                              source_dir = version_dir, provenance = NULL) {
  .pumf_apply_user_provenance(
    .pumf_manifest_source_info(version_dir, data_path, source_dir),
    registry = provenance, dirs = c(version_dir, source_dir))
}

# The source info of the download manifests alone.
.pumf_manifest_source_info <- function(version_dir, data_path = NULL,
                                       source_dir = version_dir) {
  info <- list(source = "deposit", source_url = NA_character_,
               source_doi = NA_character_, source_title = NA_character_,
               downloaded = NA_character_,
               data_file = if (is.null(data_path)) NA_character_
                           else basename(data_path),
               licence = .statcan_licence, licence_url = .statcan_licence_url)
  bm <- .borealis_read_manifest(version_dir)
  if (!is.null(bm) && nrow(bm) > 0L) {
    info$source       <- "borealis"
    info$source_doi   <- .chr1(bm$doi)
    info$source_title <- .chr1(bm$title)
    info$downloaded   <- .chr1(bm$fetched)
    info$source_url   <- if (is.na(info$source_doi)) NA_character_
                         else .borealis_doi_url(info$source_doi)
    info$licence      <- if ("licence" %in% names(bm)) .chr1(bm$licence)
                         else NA_character_
    info$licence_url  <- if ("licence_url" %in% names(bm)) .chr1(bm$licence_url)
                         else NA_character_
    return(info)
  }
  sm <- .read_statcan_manifest(version_dir)
  if (is.null(sm) && !identical(source_dir, version_dir))
    sm <- .read_statcan_manifest(source_dir)
  if (!is.null(sm)) {
    info$source     <- "statcan"
    info$source_url <- .chr1(sm$url)
    info$downloaded <- .chr1(sm$fetched)
  }
  info
}

# The landing page of a Borealis DOI ("doi:10.5683/SP3/..." or a bare DOI).
.borealis_doi_url <- function(doi) {
  paste0("https://doi.org/", sub("^doi:", "", doi))
}


# ---- User-supplied provenance ------------------------------------------------
#
# A dataset deposited in the cache by hand has no download manifest, and a
# registry default (the StatCan Open Licence, a Borealis title) is sometimes
# wrong or incomplete.  Two ways to state the provenance: a file
# pumf_provenance.csv beside the raw data (columns `field`, `value`; one row
# per field) and the registry field `provenance = list(...)` of the entry.
# Both take the same fields; the file wins over the registry, and both win
# over the manifests.  The stamp records which fields were set this way in
# `user_fields`.  Like a deposited data file, the file is read at build time
# only (refresh = TRUE rebuilds from it; redownload = TRUE deletes it with the
# version directory), so a built file never has to be opened for writing.

.user_provenance_file <- "pumf_provenance.csv"

.user_provenance_fields <- c("source", "source_url", "source_doi",
                             "source_title", "downloaded", "licence",
                             "licence_url", "citation", "note")

# Check a named list of provenance fields (from the file or a registry entry):
# every element a single string, every name a known field.  Unknown fields
# are dropped with a warning (`what` names the origin in the message); a
# wrong shape is an error.
.pumf_check_user_provenance <- function(x, what = "provenance") {
  if (is.null(x) || length(x) == 0L) return(list())
  if (!is.list(x) || is.null(names(x)) || any(names(x) == ""))
    stop(what, " must be a named list of single strings.", call. = FALSE)
  bad <- names(x)[!vapply(x, function(v)
    (is.character(v) || is.numeric(v)) && length(v) == 1L && !is.na(v),
    logical(1L))]
  if (length(bad) > 0L)
    stop(what, ": field(s) ", paste(bad, collapse = ", "),
         " must each be a single string.", call. = FALSE)
  unknown <- setdiff(names(x), .user_provenance_fields)
  if (length(unknown) > 0L) {
    warning(what, ": unknown field(s) ignored: ",
            paste(unknown, collapse = ", "), ". Recognised: ",
            paste(.user_provenance_fields, collapse = ", "), ".",
            call. = FALSE)
    x <- x[setdiff(names(x), unknown)]
  }
  lapply(x, function(v) trimws(as.character(v)))
}

# The user's provenance file of a directory as a named list (checked), or
# NULL when there is none.  A file without the `field`/`value` columns is
# reported and ignored.
.read_user_provenance <- function(dir) {
  f <- file.path(dir, .user_provenance_file)
  if (!file.exists(f)) return(NULL)
  m <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE,
                                colClasses = "character", na.strings = "",
                                encoding = "UTF-8"),
                error = function(e) NULL)
  if (is.null(m) || !all(c("field", "value") %in% names(m))) {
    warning(f, " needs the columns 'field' and 'value'; ignored.",
            call. = FALSE)
    return(NULL)
  }
  m <- m[!is.na(m$field) & nzchar(trimws(m$field)) & !is.na(m$value), ,
         drop = FALSE]
  if (nrow(m) == 0L) return(list())
  m$field <- trimws(m$field)
  if (anyDuplicated(m$field))
    stop(f, ": field(s) given more than once: ",
         paste(unique(m$field[duplicated(m$field)]), collapse = ", "),
         call. = FALSE)
  .pumf_check_user_provenance(stats::setNames(as.list(m$value), m$field),
                              what = basename(f))
}

# Lay the user's fields (file, then registry) over the source info of the
# manifests and record their names in `user_fields`.  `registry` is the
# entry's `provenance` list; `dirs` are searched for pumf_provenance.csv in
# order, the first file found is used.
.pumf_apply_user_provenance <- function(info, registry = NULL,
                                        dirs = character()) {
  reg  <- .pumf_check_user_provenance(registry, what = "registry 'provenance'")
  file <- NULL
  for (d in unique(dirs)) {
    file <- .read_user_provenance(d)
    if (!is.null(file)) break
  }
  user <- c(file, reg[setdiff(names(reg), names(file))])
  for (f in .user_provenance_fields)
    if (is.null(info[[f]])) info[[f]] <- NA_character_
  for (f in names(user)) info[[f]] <- user[[f]]
  info$user_fields <- if (length(user) == 0L) NA_character_
                      else paste(names(user), collapse = ", ")
  info
}


# ---- Build stamp -------------------------------------------------------------
#
# Stage 3 records, per table it writes, which canpumf version built it and
# when, in the small table `pumf_build_info`.  A table without a row there
# was built before the stamp existed (canpumf < 0.7.0), and so also lacks the
# pumf_row_id key and the sentinel companion.  get_pumf() says so once per
# session (.pumf_check_build_stamp()), and list_pumf_cache() reports the
# version in its `built_with` column.  Since the self-describing build the
# row also names the survey (series, version, lang, module) and its
# provenance (.pumf_source_info(); `citation`, `note` and `user_fields` come
# only from the user's pumf_provenance.csv or the registry `provenance`
# field); a row written by an earlier 0.7.0 build
# has only the first four columns, which is what .read_build_info() returns
# for it (the readers treat the missing columns as NA).  The longitudinal
# series stamp their shared tables too (version NA), on every append.
.build_info_table <- "pumf_build_info"

.build_info_cols <- c("table", "canpumf_version", "duckdb_version", "built",
                      "series", "version", "lang", "module",
                      "source", "source_url", "source_doi", "source_title",
                      "downloaded", "data_file", "licence", "licence_url",
                      "citation", "note", "user_fields", "fixups")

# Write the stamp row of `table_name`, replacing an existing one.  `info` is
# a named list of the provenance columns (anything missing is NA).  A stamp
# table from an earlier build gains the new columns before the append.
.write_build_info <- function(con, table_name, info = list()) {
  row <- list(
    table           = table_name,
    canpumf_version = as.character(utils::packageVersion("canpumf")),
    duckdb_version  = as.character(utils::packageVersion("duckdb")),
    built           = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  for (col in setdiff(.build_info_cols, names(row))) {
    v <- info[[col]]
    row[[col]] <- if (is.null(v) || length(v) == 0L) NA_character_
                  else as.character(v)[[1L]]
  }
  info <- as.data.frame(row, stringsAsFactors = FALSE)
  if (DBI::dbExistsTable(con, .build_info_table)) {
    have <- DBI::dbListFields(con, .build_info_table)
    for (col in setdiff(.build_info_cols, have))
      DBI::dbExecute(con, sprintf("ALTER TABLE %s ADD COLUMN %s VARCHAR",
                                  .qid(con, .build_info_table), .qid(con, col)))
    DBI::dbExecute(con, sprintf(
      'DELETE FROM "%s" WHERE "table" = ?', .build_info_table),
      params = list(table_name))
    DBI::dbAppendTable(con, .build_info_table, info)
  } else {
    DBI::dbWriteTable(con, .build_info_table, info)
  }
  invisible(info)
}

# The build stamp of one table (a one-row data.frame), or NULL when the
# database has no stamp for it.
.read_build_info <- function(con, table_name = NULL) {
  if (!isTRUE(tryCatch(DBI::dbExistsTable(con, .build_info_table),
                       error = function(e) FALSE)))
    return(NULL)
  info <- DBI::dbReadTable(con, .build_info_table)
  if (!is.null(table_name)) info <- info[info$table == table_name, , drop = FALSE]
  if (nrow(info) == 0L) NULL else info
}

# The provenance columns of a build stamp for a survey table: the survey's
# identity, the source (.pumf_source_info()) and the names of the registry
# data fixups that applied.
.pumf_stamp_info <- function(series, version, lang, module, src, fixups) {
  c(list(series = series, version = version, lang = lang,
         module = module %||% NA_character_,
         fixups = if (length(fixups) == 0L) NA_character_
                  else paste(names(fixups), collapse = ", ")),
    src)
}

# One line describing a table, for COMMENT ON TABLE: "SFS 2019 [eng]" plus
# the source title, DOI or URL when known.
.pumf_table_comment <- function(series, version, lang, module = NULL,
                                src = list()) {
  what <- paste0(series, if (!is.na(version %||% NA)) paste0(" ", version),
                 if (!is.null(module)) paste0(" ", module),
                 " [", lang, "]")
  from <- c(src$source_title, src$source_doi, src$source_url)
  from <- from[!is.na(from)]
  if (length(from) == 0L) return(what)
  paste0(what, " - ", from[[1L]])
}


# ---- Dictionary table -------------------------------------------------------
#
# `pumf_dictionary` holds, per table, the rows pumf_dictionary() returns
# (name, val, label_en, label_fr, description_en, description_fr,
# applied_as): the variable rows (val NA), the value labels as the build
# applied them, and the registry sentinel labels (name NA applies to every
# column).  Stage 3 writes it after the stamp, from the same objects it
# labelled the data with, so the ENUM levels and the dictionary agree.
.dictionary_table <- "pumf_dictionary"

.dictionary_cols <- c("name", "val", "label_en", "label_fr",
                      "description_en", "description_fr", "applied_as")

# Replace the dictionary rows of `table_name` with `dict`.
.write_dictionary <- function(con, table_name, dict) {
  dict <- as.data.frame(dict, stringsAsFactors = FALSE)
  for (col in setdiff(.dictionary_cols, names(dict)))
    dict[[col]] <- rep(NA_character_, nrow(dict))
  dict <- dict[, .dictionary_cols, drop = FALSE]
  for (col in names(dict)) dict[[col]] <- as.character(dict[[col]])
  dict <- cbind(data.frame(table = rep(table_name, nrow(dict)),
                           stringsAsFactors = FALSE), dict)
  if (DBI::dbExistsTable(con, .dictionary_table)) {
    DBI::dbExecute(con, sprintf(
      'DELETE FROM "%s" WHERE "table" = ?', .dictionary_table),
      params = list(table_name))
    if (nrow(dict) > 0L) DBI::dbAppendTable(con, .dictionary_table, dict)
  } else {
    DBI::dbWriteTable(con, .dictionary_table, dict)
  }
  invisible(dict)
}

# The stored dictionary of the first of `table_names` that has rows, as a
# tibble without the `table` column; NULL when the database has none (built
# before the dictionary table, or none of the tables).
.read_dictionary <- function(con, table_names) {
  if (!isTRUE(tryCatch(DBI::dbExistsTable(con, .dictionary_table),
                       error = function(e) FALSE)))
    return(NULL)
  for (t in table_names) {
    d <- DBI::dbGetQuery(con, sprintf(
      'SELECT * FROM "%s" WHERE "table" = ?', .dictionary_table),
      params = list(t))
    if (nrow(d) > 0L) {
      d$table <- NULL
      for (col in setdiff(.dictionary_cols, names(d)))
        d[[col]] <- rep(NA_character_, nrow(d))
      return(tibble::as_tibble(d[, .dictionary_cols, drop = FALSE]))
    }
  }
  NULL
}

# Does the database hold dictionary rows for `table_name`?
.dictionary_has_rows <- function(con, table_name) {
  !is.null(.read_dictionary(con, table_name))
}

# Assemble the dictionary rows of a table from its variables (name,
# label_en, label_fr, optional description columns), its value labels
# (name, val, label_en, label_fr, optional applied_as) and the registry
# sentinel rows (.pumf_sentinel_label_rows(), or NULL).  The derived LFS
# helper columns are added for the shared series.  The build falls back to
# label_en where label_fr is missing (and to the digits where neither
# exists), so the dictionary says what the table shows.
.pumf_dictionary_assemble <- function(variables, codes, sent = NULL,
                                      series = NULL) {
  variables <- as.data.frame(variables, stringsAsFactors = FALSE)
  codes     <- as.data.frame(codes, stringsAsFactors = FALSE)
  for (d in setdiff(.metadata_description_cols, names(variables)))
    variables[[d]] <- rep(NA_character_, nrow(variables))
  variables$name <- toupper(variables$name)
  codes$name     <- toupper(codes$name)
  if (!is.null(series) && .is_shared_series(series)) {
    derived <- .pumf_derived_var_rows(variables$name)
    for (d in .metadata_description_cols)
      derived[[d]] <- rep(NA_character_, nrow(derived))
    variables <- rbind(variables[, c("name", "label_en", "label_fr",
                                     .metadata_description_cols)], derived)
  }
  vars <- data.frame(name = variables$name, val = NA_character_,
                     label_en = as.character(variables$label_en),
                     label_fr = as.character(variables$label_fr),
                     description_en = as.character(variables$description_en),
                     description_fr = as.character(variables$description_fr),
                     applied_as = NA_character_, stringsAsFactors = FALSE)
  cds  <- data.frame(name = codes$name, val = as.character(codes$val),
                     label_en = as.character(codes$label_en),
                     label_fr = as.character(codes$label_fr),
                     description_en = rep(NA_character_, nrow(codes)),
                     description_fr = rep(NA_character_, nrow(codes)),
                     applied_as = if ("applied_as" %in% names(codes))
                       as.character(codes$applied_as)
                     else rep(NA_character_, nrow(codes)),
                     stringsAsFactors = FALSE)
  if (!is.null(sent) && nrow(sent) > 0L) {
    sent <- as.data.frame(sent, stringsAsFactors = FALSE)
    sent$description_en <- rep(NA_character_, nrow(sent))
    sent$description_fr <- rep(NA_character_, nrow(sent))
    sent$applied_as     <- rep(NA_character_, nrow(sent))
    sent <- sent[, .dictionary_cols, drop = FALSE]
  } else {
    sent <- NULL
  }
  out <- rbind(vars, cds, sent)
  na_fr <- is.na(out$label_fr)
  out$label_fr[na_fr] <- out$label_en[na_fr]
  na_en <- is.na(out$label_en)
  out$label_en[na_en] <- out$label_fr[na_en]
  out <- out[!is.na(out$label_en), , drop = FALSE]
  rownames(out) <- NULL
  tibble::as_tibble(out)
}


# ---- Comments ---------------------------------------------------------------

# Attach the variable labels as column comments and `comment` as the table
# comment of every table in `tables` that exists: each column named in the
# variable rows of `dict` gets its label in the language `lang`;
# pumf_row_id gets a fixed description.  Comments live in the catalog, so
# they survive the type changes Stage 3 makes afterwards and the compaction
# of .remove_pumf_lang(); any DuckDB client reads them from duckdb_columns()
# and duckdb_tables().
.pumf_comment_tables <- function(con, tables, dict, lang, comment = NULL) {
  label_col <- .pumf_label_col(lang)
  vars <- dict[is.na(dict$val) & !is.na(dict$name), , drop = FALSE]
  labels <- stats::setNames(as.character(vars[[label_col]]), toupper(vars$name))
  labels <- labels[!is.na(labels) & nzchar(labels)]
  labels <- c(labels, pumf_row_id = "Record position in the data file (1-based)")
  for (t in tables) {
    if (!DBI::dbExistsTable(con, t)) next
    if (!is.null(comment) && !is.na(comment))
      DBI::dbExecute(con, sprintf("COMMENT ON TABLE %s IS %s",
                                  .qid(con, t), .qstr(con, comment)))
    cols <- DBI::dbListFields(con, t)
    for (col in intersect(cols, names(labels)))
      DBI::dbExecute(con, sprintf("COMMENT ON COLUMN %s.%s IS %s",
                                  .qid(con, t), .qid(con, col),
                                  .qstr(con, labels[[col]])))
  }
  invisible(NULL)
}

# The column comments of a table as a named character vector (NA where a
# column has none), or NULL when the table does not exist.
.pumf_column_comments <- function(con, table_name) {
  if (!DBI::dbExistsTable(con, table_name)) return(NULL)
  d <- DBI::dbGetQuery(con, paste0(
    "SELECT column_name, comment FROM duckdb_columns() WHERE table_name = ",
    .qstr(con, table_name), " ORDER BY column_index"))
  stats::setNames(as.character(d$comment), d$column_name)
}

# The table comment, or NA.
.pumf_table_comment_of <- function(con, table_name) {
  d <- DBI::dbGetQuery(con, paste0(
    "SELECT comment FROM duckdb_tables() WHERE table_name = ",
    .qstr(con, table_name)))
  if (nrow(d) == 0L) NA_character_ else .chr1(d$comment)
}


# ---- Reading through a provenance record ------------------------------------

# The DuckDB file behind a provenance record, or NULL for LFS_TIMELINE (an
# in-memory union of two files).
.pumf_prov_db_path <- function(prov) {
  if (identical(prov$series, "LFS_TIMELINE")) return(NULL)
  if (is.null(prov$cache_path)) return(NULL)
  .pumf_db_path(prov$series, prov$version, prov$cache_path)
}

# The table a provenance record points at, followed by the other language's
# table: the dictionary rows are bilingual and identical for both, so either
# serves a series-name lookup when only one language is built.
.pumf_prov_tables <- function(prov) {
  lang  <- prov$lang %||% "eng"
  other <- setdiff(c("eng", "fra"), lang)
  vapply(c(lang, other), function(l)
    .pumf_table_name(prov$series, prov$version, l, prov$module),
    character(1L))
}

# Run `fn(con)` on the database of a provenance record: on the registered
# connection when the record has a live one (a lazy tbl), otherwise on a
# transient read-only connection released without shutting the instance down
# (a collect_pumf() frame, a series name).  Returns `default` when the file
# is missing or cannot be opened (held by a writer elsewhere); the readers
# then fall back to metadata/.  Never opens a write connection (issue #18).
.pumf_with_prov_con <- function(prov, fn, default = NULL) {
  con <- prov$con
  if (!is.null(con) && isTRUE(tryCatch(DBI::dbIsValid(con),
                                       error = function(e) FALSE)))
    return(fn(con))
  db_path <- .pumf_prov_db_path(prov)
  if (is.null(db_path) || !file.exists(db_path)) return(default)
  con <- tryCatch(.duckdb_connect_quiet(db_path, read_only = TRUE),
                  error = function(e) NULL)
  if (is.null(con)) return(default)
  on.exit(DBI::dbDisconnect(con, shutdown = FALSE))
  fn(con)
}

# The dictionary a self-describing database stores for a provenance record,
# or NULL (a database built before it, LFS_TIMELINE, a file that cannot be
# opened).
.pumf_stored_dictionary <- function(prov) {
  if (identical(prov$series, "LFS_TIMELINE")) return(NULL)
  tables <- tryCatch(.pumf_prov_tables(prov), error = function(e) NULL)
  if (is.null(tables)) return(NULL)
  .pumf_with_prov_con(prov, function(con) .read_dictionary(con, tables))
}

# Provenance recovered from the database itself, for a connection get_pumf()
# did not register (a file opened with DBI directly, a connection from
# another package): the stamp row of the tbl's remote table names the
# series, version, language and module; the cache path is the part of the
# file's directory above <series>/<version> (or <series>/ for a longitudinal
# file), NA when the file lives elsewhere.  NULL when the table has no such
# stamp (a database built before the stamp carried the survey's identity).
.pumf_prov_from_db <- function(con, tbl) {
  tname <- tryCatch(as.character(dbplyr::remote_name(tbl)),
                    error = function(e) NULL)
  if (is.null(tname) || length(tname) != 1L || is.na(tname)) return(NULL)
  info <- tryCatch(.read_build_info(con, tname), error = function(e) NULL)
  if (is.null(info) || !"series" %in% names(info) || is.na(info$series[[1L]]))
    return(NULL)
  series  <- info$series[[1L]]
  version <- info$version[[1L]]
  dbdir   <- tryCatch(normalizePath(con@driver@dbdir, winslash = "/",
                                    mustWork = FALSE),
                      error = function(e) NA_character_)
  rel <- if (.is_longitudinal(series)) series
         else file.path(series, version)
  cache_path <- NA_character_
  if (!is.na(dbdir)) {
    dir <- dirname(dbdir)
    if (endsWith(dir, paste0("/", rel)))
      cache_path <- substr(dir, 1L, nchar(dir) - nchar(rel) - 1L)
  }
  list(con = con, series = series,
       version = if (.is_longitudinal(series)) NULL else version,
       cache_path = cache_path,
       lang   = if (is.na(info$lang[[1L]] %||% NA)) "eng" else info$lang[[1L]],
       module = if (is.na(info$module[[1L]])) NULL else info$module[[1L]])
}
