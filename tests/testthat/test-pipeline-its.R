# Integration tests for the International Travel Survey (ITS) pipeline.
# Tests run against data already in the user's canpumf cache.
#
# ITS ships split-SPSS metadata (VTS_<year>_PUMF_{i,vale,vare,valf,varf}.sps)
# in Layout_Cards/ and a fixed-width data file selected by file_mask.

.its_vdir <- function(version) {
  file.path(getOption("canpumf.cache_path", ""), "ITS", version)
}

.its_verified <- c("2018", "2019")


# ---- Full pipeline loop (Stage 2 + Stage 3) ---------------------------------

for (.v in .its_verified) {
  local({
    ver <- .v

    test_that(paste0("ITS ", ver, ": full pipeline emits no warnings"), {
      skip_if_not(canpumf:::.version_is_extracted(.its_vdir(ver)),
                  paste("ITS", ver, "not extracted in cache"))

      reg  <- canpumf:::pumf_registry_lookup("ITS", ver)
      tmp  <- tempfile(fileext = ".duckdb")
      con  <- NULL
      warns <- character(0L)

      withCallingHandlers(
        {
          canpumf:::pumf_parse_metadata(.its_vdir(ver),
                                         metadata_encoding = reg$metadata_encoding,
                                         refresh           = TRUE)
          r   <- canpumf:::pumf_build_duckdb(.its_vdir(ver), "ITS", ver,
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

      expect_identical(warns, character(0L),
        label = paste0("ITS ", ver, ": should have no warnings"))
    })
  })
}


# ---- Bilingual label coverage -----------------------------------------------

for (.v in .its_verified) {
  local({
    ver <- .v
    test_that(paste0("ITS ", ver, ": metadata has English and French labels"), {
      vdir <- .its_vdir(ver)
      skip_if_not(file.exists(file.path(vdir, "metadata", "variables.csv")),
                  paste("ITS", ver, "metadata not parsed"))

      meta <- canpumf:::read_metadata(file.path(vdir, "metadata"))

      expect_gt(sum(!is.na(meta$variables$label_en)), 0L,
        label = paste0("ITS ", ver, ": should have English variable labels"))
      expect_gt(sum(!is.na(meta$variables$label_fr)), 0L,
        label = paste0("ITS ", ver, ": should have French variable labels"))
    })
  })
}


# ---- Bilingual parity (per verified version) --------------------------------

for (.v in .its_verified) {
  local({
    ver <- .v
    test_that(paste0("ITS ", ver, ": eng/fra bilingual parity"), {
      skip_if_not(canpumf:::.version_is_extracted(.its_vdir(ver)),
                  paste("ITS", ver, "not extracted in cache"))
      skip_if_not(file.exists(file.path(.its_vdir(ver), "metadata", "variables.csv")),
                  paste("ITS", ver, "metadata not parsed"))

      tmp <- tempfile(fileext = ".duckdb")
      on.exit(unlink(tmp), add = TRUE)

      r_eng <- suppressWarnings(
        canpumf:::pumf_build_duckdb(.its_vdir(ver), "ITS", ver,
                                     lang = "eng", db_path = tmp, refresh = TRUE))
      r_fra <- suppressWarnings(
        canpumf:::pumf_build_duckdb(.its_vdir(ver), "ITS", ver,
                                     lang = "fra", db_path = tmp, refresh = TRUE))

      eng <- .collect_pumf_table(tmp, r_eng$table_name)
      fra <- .collect_pumf_table(tmp, r_fra$table_name)

      expect_pumf_bilingual_parity(eng, fra, label = paste0("ITS ", ver))
    })
  })
}
