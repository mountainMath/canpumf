# Regression tests: survey weight columns must never contain NA values.
#
# Weight columns identified by regex — main survey weights only.
# Excluded: Census pre-shipped bootstrap replicates (WT1-WT16),
#           GSS/SGVP bootstrap (WTBS_*) and item weights (WTI_*).

.wt_cache <- function() getOption("canpumf.cache_path", "")

.is_weight_col <- function(name) {
  grepl("WEIGHT|WGHT|^WGT_|FINWGH|FAMWGT|WTPP", name, ignore.case = TRUE) &
  !grepl("^WT\\d+$|^WTBS_|^WTI", name, ignore.case = TRUE)
}

# ---- Unit: regex correctly classifies known column names -------------------

test_that(".is_weight_col: includes main survey weight columns", {
  include <- c("WEIGHT", "WEIGHTP", "WEIGHTH", "WEIGHTC", "WEIGHTD",
               "PWEIGHT", "FWEIGHT", "PFWEIGHT", "RWEIGHT",
               "WGHT_PER", "WGHT_FNL", "WGHT_HSD",
               "WGHTEPI", "WGHTFIN", "PERWGHT",
               "WGT_PUMF", "VWEIGHTP", "FINWGHT", "FAMWGT", "WTPP")
  expect_true(all(.is_weight_col(include)), info = paste("excluded:", paste(include[!.is_weight_col(include)], collapse=",")))
})

test_that(".is_weight_col: excludes bootstrap replicates and item weights", {
  exclude <- c("WT1", "WT2", "WT16", "WTBS_001", "WTI_Q110", "WTI_130", "WTI120GR")
  expect_true(!any(.is_weight_col(exclude)), info = paste("included:", paste(exclude[.is_weight_col(exclude)], collapse=",")))
})


# ---- Integration: no NA values in any weight column across all cached surveys

for (.key in canpumf:::pumf_registry_keys()) {
  local({
    key     <- .key
    parts   <- strsplit(key, "/")[[1L]]
    series  <- parts[1L]
    version <- paste(parts[-1L], collapse = "/")

    test_that(paste0(key, ": weight columns have no NA values"), {
      skip_if(.wt_cache() == "", "Cache not configured")

      vdir <- file.path(.wt_cache(), series, version)
      skip_if_not(
        dir.exists(vdir) && (
          canpumf:::.version_is_extracted(vdir) ||
          !is.null(canpumf:::.find_version_zip(vdir)) ||
          length(list.files(dirname(vdir), pattern = "\\.pdf$",
                            recursive = TRUE, ignore.case = TRUE)) > 0L
        ),
        paste(key, "not in cache")
      )

      meta_dir <- file.path(vdir, "metadata")
      skip_if_not(dir.exists(meta_dir), paste(key, "metadata not parsed"))

      vars <- tryCatch(
        read.csv(file.path(meta_dir, "variables.csv")),
        error = function(e) NULL
      )
      skip_if(is.null(vars), "Could not read variables.csv")

      wt_cols <- vars$name[.is_weight_col(vars$name) & vars$type == "numeric"]

      # Also include force_numeric registry entries with weight-like names
      # (some surveys have weight columns misclassified as character by the parser)
      config  <- tryCatch(canpumf:::pumf_registry_lookup(series, version),
                          error = function(e) NULL)
      fn_cols <- config$data_fixups$force_numeric
      if (length(fn_cols) > 0L)
        wt_cols <- unique(c(wt_cols, fn_cols[.is_weight_col(fn_cols)]))

      skip_if(length(wt_cols) == 0L, paste("no weight columns in", key))

      tbl <- suppressMessages(get_pumf(series, version, cache_path = .wt_cache()))
      on.exit(close_pumf(tbl), add = TRUE)

      # Only check columns that actually exist in the DuckDB table
      # (some metadata columns may not be in the table for all build variants)
      wt_cols <- intersect(wt_cols, colnames(tbl))
      skip_if(length(wt_cols) == 0L, "weight columns not in built table")

      for (col in wt_cols) {
        n_na <- dplyr::filter(tbl, is.na(!!dplyr::sym(col))) |>
          dplyr::count() |>
          dplyr::collect() |>
          dplyr::pull(n)
        expect_equal(n_na, 0,
          label = paste0(key, " ", col, ": NA count"))
      }
    })
  })
}
