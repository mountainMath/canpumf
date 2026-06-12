# Integration tests for the General Social Survey (GSS) pipeline.
# Tests run against data already in the user's canpumf cache.

.gss_cache <- function() getOption("canpumf.cache_path", "")

.gss_vdir <- function(version) {
  file.path(.gss_cache(), "GSS", version)
}

# All end-to-end verified versions (Caregiving series, registered as plain years)
.gss_verified <- c("2018", "2012", "2007", "1996")

# Non-caregiving GSS themes with their version strings
.gss_theme_verified <- c(
  "Safety 2019", "Social Identity 2020", "Time Use 2022",
  "Family 2017", "Safety 2014", "Social Identity 2013", "Time Use 2015",
  "Family 2011", "Education 2007", "Social Identity 2003",
  "Time Use 2010", "Education 1994", "Safety 1993", "Safety 1999",
  "Family 1995", "Family 2001", "Education 2002", "Time Use 1998"
)

# Versions whose StatCan distribution contains only English command files;
# no French variable labels are available in the metadata.
.gss_no_french_vars <- c("Education 1994")

# Versions where StatCan's SPSS/SAS command files carry no French code labels;
# bilingual parity tests are skipped for these.
# Education 1994: English-only command files.
# Education 2002: variables.csv has French variable names, but codes.csv has
#   none — the SPSS value-label blocks are English-only.
.gss_no_french_codes <- c("Education 1994", "Education 2002")

# GSS versions with known expected warnings (pattern matched against each warning).
# Any warning NOT matching this pattern is unexpected and fails the test.
.gss_supplement_warnings <- list(
  # 2018: force_numeric injects boundary-label variables
  "2018" = "absent from command files",
  # 2012: force_numeric injects boundary-label variables; WTBS bootstrap weights
  #        are in the layout but lack individual variable labels in the SPSS file
  "2012" = "absent from command files|Variables in layout but not in variable labels",
  # 2007: WTBS_002–WTBS_500 unlabeled in VARIABLE LABELS (StatCan only labels #1);
  #        the French SPSS file for Cycle 21 covers only ~26 of 951 variables
  "2007" = "Variables in layout but not in variable labels|no French translation",
  # Non-caregiving themes with force_numeric (boundary labels → continuous data)
  "Safety 2019"         = "absent from command files",
  "Safety 2014"         = "absent from command files",
  "Safety 1999"         = "absent from command files|Variables in layout but not in variable labels|no French translation",
  "Safety 1993"         = "absent from command files",
  "Family 2017"         = "absent from command files",
  "Family 2011"         = "absent from command files|Variables in layout but not in variable labels",
  "Family 2001"         = "absent from command files",
  "Family 1995"         = "absent from command files",
  "Social Identity 2013"= "absent from command files",
  "Social Identity 2003"= "absent from command files|Variables in layout but not in variable labels",
  "Education 2007"      = "absent from command files|Variables in layout but not in variable labels",
  "Education 2002"      = "absent from command files",
  "Education 1994"      = "absent from command files",
  "Time Use 2015"       = "absent from command files",
  "Time Use 2010"       = "Variables in layout but not in variable labels",
  "Time Use 1998"       = "absent from command files"
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
