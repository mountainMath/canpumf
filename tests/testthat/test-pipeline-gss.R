# Integration tests for the General Social Survey (GSS) pipeline.
# Tests run against data already in the user's canpumf cache.

.gss_cache <- function() getOption("canpumf.cache_path", "")

.gss_vdir <- function(version) {
  file.path(.gss_cache(), "GSS", version)
}

# All end-to-end verified versions
.gss_verified <- c("2018", "2012", "2007", "1996")

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
  "2007" = "Variables in layout but not in variable labels|no French translation"
)


# ---- Full pipeline loop (Stage 2 + Stage 3) ---------------------------------

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

      # Disconnect explicitly before assertions so no lingering connection
      # can produce a finalizer warning that bleeds into the next test.
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


# ---- Bilingual label coverage -----------------------------------------------

for (.v in .gss_verified) {
  local({
    ver <- .v
    test_that(paste0("GSS ", ver, ": metadata has English and French labels"), {
      vdir <- .gss_vdir(ver)
      skip_if_not(file.exists(file.path(vdir, "metadata", "variables.csv")),
                  paste("GSS", ver, "metadata not parsed"))

      meta <- canpumf:::read_metadata(file.path(vdir, "metadata"))

      en_vars <- sum(!is.na(meta$variables$label_en) &
                       nchar(meta$variables$label_en) > 0L)
      expect_gt(en_vars, 0L,
        label = paste0("GSS ", ver, ": should have English variable labels"))

      fr_vars <- sum(!is.na(meta$variables$label_fr) &
                       nchar(meta$variables$label_fr) > 0L)
      expect_gt(fr_vars, 0L,
        label = paste0("GSS ", ver, ": should have French variable labels"))
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
