# Integration tests for the GSS Giving, Volunteering and Participating (SGVP)
# pipeline.  Tests run against data already in the user's canpumf cache.

.sgvp_cache <- function() getOption("canpumf.cache_path", "")

.sgvp_vdir <- function(version) {
  file.path(.sgvp_cache(), "SGVP", version)
}

# All end-to-end verified versions
.sgvp_verified <- c("2023", "2018", "2013", "2010", "2007", "2004", "2000", "1997")

# SGVP versions with known expected warnings (pattern matched against each
# warning).  Any warning NOT matching this pattern is unexpected and fails.
.sgvp_supplement_warnings <- list(
  # 2018: BRTHMACR code 09 has a blank label in the PDF data dictionary and is
  # absent from the command files; the registry injects it via codes_supplement.
  "2018" = "absent from command files"
)


# ---- Full pipeline loop (Stage 2 + Stage 3) ---------------------------------

for (.v in .sgvp_verified) {
  local({
    ver <- .v
    pat <- .sgvp_supplement_warnings[[ver]]

    test_that(paste0("SGVP ", ver, ": full pipeline emits no unexpected warnings"), {
      skip_if_not(canpumf:::.version_is_extracted(.sgvp_vdir(ver)),
                  paste("SGVP", ver, "not extracted in cache"))

      reg  <- canpumf:::pumf_registry_lookup("SGVP", ver)
      tmp  <- tempfile(fileext = ".duckdb")
      con  <- NULL
      warns <- character(0L)

      withCallingHandlers(
        {
          canpumf:::pumf_parse_metadata(.sgvp_vdir(ver),
                                         metadata_encoding = reg$metadata_encoding,
                                         refresh           = TRUE)
          r   <- canpumf:::pumf_build_duckdb(.sgvp_vdir(ver), "SGVP", ver,
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
          label = paste0("SGVP ", ver, ": unexpected warnings"))
      } else {
        expect_identical(warns, character(0L),
          label = paste0("SGVP ", ver, ": should have no warnings"))
      }
    })
  })
}


# ---- Bilingual label coverage -----------------------------------------------

for (.v in .sgvp_verified) {
  local({
    ver <- .v
    test_that(paste0("SGVP ", ver, ": metadata has English and French labels"), {
      vdir <- .sgvp_vdir(ver)
      skip_if_not(file.exists(file.path(vdir, "metadata", "variables.csv")),
                  paste("SGVP", ver, "metadata not parsed"))

      meta <- canpumf:::read_metadata(file.path(vdir, "metadata"))

      en_vars <- sum(!is.na(meta$variables$label_en) &
                       nchar(meta$variables$label_en) > 0L)
      expect_gt(en_vars, 0L,
        label = paste0("SGVP ", ver, ": should have English variable labels"))

      fr_vars <- sum(!is.na(meta$variables$label_fr) &
                       nchar(meta$variables$label_fr) > 0L)
      expect_gt(fr_vars, 0L,
        label = paste0("SGVP ", ver, ": should have French variable labels"))
    })
  })
}


# ---- Bilingual parity (per verified version) --------------------------------

for (.v in .sgvp_verified) {
  local({
    ver <- .v
    test_that(paste0("SGVP ", ver, ": eng/fra bilingual parity"), {
      skip_if_not(canpumf:::.version_is_extracted(.sgvp_vdir(ver)),
                  paste("SGVP", ver, "not extracted in cache"))
      skip_if_not(file.exists(file.path(.sgvp_vdir(ver), "metadata", "variables.csv")),
                  paste("SGVP", ver, "metadata not parsed"))

      tmp <- tempfile(fileext = ".duckdb")
      on.exit(unlink(tmp), add = TRUE)

      r_eng <- suppressWarnings(
        canpumf:::pumf_build_duckdb(.sgvp_vdir(ver), "SGVP", ver,
                                     lang = "eng", db_path = tmp, refresh = TRUE))
      r_fra <- suppressWarnings(
        canpumf:::pumf_build_duckdb(.sgvp_vdir(ver), "SGVP", ver,
                                     lang = "fra", db_path = tmp, refresh = TRUE))

      eng <- .collect_pumf_table(tmp, r_eng$table_name)
      fra <- .collect_pumf_table(tmp, r_fra$table_name)

      expect_pumf_bilingual_parity(eng, fra, label = paste0("SGVP ", ver))
    })
  })
}


# ---- Multi-module cycles (MAIN + GS/VD detail tables) -----------------------

# Older SGVP cycles ship the respondent-level MAIN file plus one or more detail
# files (GS = giving, VD = volunteer detail; 1997 names them GIVE/VOLNTR) that
# join on a shared respondent key.  The key varies across cycles.
.sgvp_module_specs <- list(
  "2010" = list(key = "PUMFID",   modules = c("GS")),
  "2007" = list(key = "PUMFID",   modules = c("GS")),
  "2004" = list(key = "PUMFID",   modules = c("GS")),
  "2000" = list(key = "MICRO_ID", modules = c("GS", "VD")),
  "1997" = list(key = "IDNUM",    modules = c("GIVE", "VOLNTR"))
)

for (.v in names(.sgvp_module_specs)) {
  local({
    ver  <- .v
    spec <- .sgvp_module_specs[[ver]]

    test_that(paste0("SGVP ", ver, " registry exposes MAIN + detail modules"), {
      reg  <- canpumf:::pumf_registry_lookup("SGVP", ver)
      mods <- canpumf:::.pumf_entry_modules(reg)
      expect_identical(sort(names(mods)), sort(c("MAIN", spec$modules)))
      expect_true(mods$MAIN$is_primary)
      expect_null(mods$MAIN$meta_subdir)
      for (md in spec$modules)
        expect_identical(mods[[md]]$meta_subdir, md)
      expect_identical(canpumf:::.pumf_module_key(reg), spec$key)
    })

    test_that(paste0("SGVP ", ver, " builds joinable MAIN + detail modules"), {
      skip_if_not(canpumf:::.version_is_extracted(.sgvp_vdir(ver)),
                  paste("SGVP", ver, "not extracted in cache"))

      key  <- spec$key
      main <- suppressMessages(suppressWarnings(get_pumf("SGVP", ver)))
      on.exit(close_pumf(main), add = TRUE)
      expect_true(key %in% colnames(main))
      mk <- dplyr::distinct(dplyr::select(main, dplyr::all_of(key)))

      for (md in spec$modules) {
        sub <- suppressMessages(pumf_module(main, md))
        expect_true(key %in% colnames(sub),
                    label = paste0(ver, " ", md, " has key ", key))
        expect_identical(main$src$con, sub$src$con)
        matched <- dplyr::inner_join(dplyr::select(sub, dplyr::all_of(key)),
                                     mk, by = key)
        expect_identical(dplyr::pull(dplyr::tally(sub)),
                         dplyr::pull(dplyr::tally(matched)),
                         label = paste0(ver, " ", md, " rows all join MAIN"))
        expect_silent(suppressWarnings(label_pumf_columns(sub)))
      }
    })
  })
}


# ---- Registry fixups behave as verified against the PDF codebooks -----------

test_that("SGVP 2013: top-coded counts survive force_numeric", {
  vdir <- .sgvp_vdir("2013")
  skip_if_not(canpumf:::.version_is_extracted(vdir),
              "SGVP 2013 not extracted in cache")
  skip_if_not(file.exists(file.path(vdir, "metadata", "variables.csv")),
              "SGVP 2013 metadata not parsed")

  tmp <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp), add = TRUE)
  r   <- suppressWarnings(
    canpumf:::pumf_build_duckdb(vdir, "SGVP", "2013",
                                 lang = "eng", db_path = tmp, refresh = TRUE))
  d <- .collect_pumf_table(tmp, r$table_name)

  # HSDSIZEC top-coded at 6+, CHH0014C top-coded at 3+ (PDF codebook; see
  # override_verification.csv) — unlabeled lower counts must not be NA'd.
  expect_true(is.numeric(d$HSDSIZEC))
  expect_identical(range(d$HSDSIZEC, na.rm = TRUE), c(1, 6))
  expect_true(is.numeric(d$CHH0014C))
  expect_identical(range(d$CHH0014C, na.rm = TRUE), c(0, 3))
})
