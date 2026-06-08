# ---- pumf_registry_lookup ---------------------------------------------------

test_that("pumf_registry_lookup: returns NULL for unknown survey", {
  expect_null(canpumf:::pumf_registry_lookup("FAKE", "2099"))
  expect_null(canpumf:::pumf_registry_lookup("SFS", "1900"))
})

test_that("pumf_registry_lookup: SFS/2019 has expected fields", {
  e <- canpumf:::pumf_registry_lookup("SFS", "2019")

  expect_named(e, c("series","version","layout_mask","bsw_mask","bsw_file_mask",
                    "bsw_join_key","bsw_drop_cols","file_mask",
                    "data_encoding","metadata_encoding","data_fixups"),
               ignore.order = TRUE)

  expect_equal(e$series,        "SFS")
  expect_equal(e$version,       "2019")
  expect_equal(e$layout_mask,   "EFAM_PUMF")
  expect_equal(e$bsw_mask,      "bsweights")
  expect_equal(e$bsw_join_key,  "PEFAMID")
  expect_equal(e$bsw_drop_cols, "PWEIGHT")
  expect_equal(e$data_encoding,     "CP1252")
  expect_equal(e$metadata_encoding, "CP1252")
})

test_that("pumf_registry_lookup: SFS str_pad fixup present for 2016/2019/2023", {
  for (v in c("2016", "2019", "2023")) {
    e <- canpumf:::pumf_registry_lookup("SFS", v)
    expect_true(!is.null(e$data_fixups$str_pad),
                label = paste0("SFS/", v, " has str_pad fixup"))
    pad_spec <- e$data_fixups$str_pad[[1]]
    expect_setequal(pad_spec$cols, c("PASRBUYG", "PASRDWNG", "PASRMPFG"))
    expect_equal(pad_spec$width, 2L)
    expect_equal(pad_spec$side,  "left")
    expect_equal(pad_spec$pad,   "0")
  }
})

test_that("pumf_registry_lookup: SFS/2012 has no BSW and no fixup", {
  e <- canpumf:::pumf_registry_lookup("SFS", "2012")
  expect_null(e$bsw_mask)
  expect_null(e$bsw_join_key)
  expect_equal(length(e$data_fixups), 0L)
})

test_that("pumf_registry_lookup: CHS entries have BSW config", {
  # 2018 PUMF was released without a bootstrap weight file
  e18 <- canpumf:::pumf_registry_lookup("CHS", "2018")
  expect_true(is.null(e18$bsw_mask), label = "CHS/2018 has no BSW")
  expect_false(is.null(e18$file_mask), label = "CHS/2018 file_mask")

  for (v in c("2021", "2022")) {
    e <- canpumf:::pumf_registry_lookup("CHS", v)
    expect_false(is.null(e$bsw_mask),      label = paste0("CHS/", v, " bsw_mask"))
    expect_equal(e$bsw_join_key, "PUMFID", label = paste0("CHS/", v, " bsw_join_key"))
    expect_false(is.null(e$file_mask),     label = paste0("CHS/", v, " file_mask"))
  }
})

test_that("pumf_registry_lookup: SHS/2019 has BSW config", {
  e <- canpumf:::pumf_registry_lookup("SHS", "2019")
  expect_false(is.null(e$bsw_mask))
  expect_false(is.null(e$file_mask))
})

test_that("pumf_registry_lookup: Census/2021 (individuals) has UTF-8 metadata encoding", {
  e <- canpumf:::pumf_registry_lookup("Census", "2021 (individuals)")
  expect_equal(e$metadata_encoding, "UTF-8")
  expect_equal(e$file_mask, "\\.csv")
})

test_that("pumf_registry_lookup: Census/2021 (individuals) has RELIGION_DER rename fixup", {
  e <- canpumf:::pumf_registry_lookup("Census", "2021 (individuals)")
  expect_false(is.null(e$data_fixups$rename))
  expect_equal(names(e$data_fixups$rename), "RELIGION_DER")
  expect_equal(unname(e$data_fixups$rename), "RELIG")
})

test_that("pumf_registry_lookup: Census fixed-width versions have .dat file_mask", {
  fwf_versions <- c("2016 (individuals)", "2011 (individuals)",
                    "2006 (individuals)", "2001 (individuals)",
                    "1996 (individuals)")
  for (v in fwf_versions) {
    e <- canpumf:::pumf_registry_lookup("Census", v)
    expect_false(is.null(e$file_mask),
                 label = paste0("Census/", v, " has file_mask"))
    expect_true(grepl("dat", e$file_mask, ignore.case = TRUE),
                label = paste0("Census/", v, " file_mask includes .dat"))
  }
})

test_that("pumf_registry_lookup: older Census versions have CP1252 metadata encoding", {
  for (v in c("2016 (individuals)", "2011 (individuals)", "2006 (individuals)")) {
    e <- canpumf:::pumf_registry_lookup("Census", v)
    expect_equal(e$metadata_encoding, "CP1252",
                 label = paste0("Census/", v, " metadata_encoding"))
  }
})

# ---- pumf_registry_keys -----------------------------------------------------

test_that("pumf_registry_keys: returns character vector", {
  keys <- canpumf:::pumf_registry_keys()
  expect_type(keys, "character")
  expect_true(length(keys) > 10L)
})

test_that("pumf_registry_keys: contains expected series", {
  keys <- canpumf:::pumf_registry_keys()
  expect_true(any(grepl("^SFS/", keys)))
  expect_true(any(grepl("^CHS/", keys)))
  expect_true(any(grepl("^SHS/", keys)))
  expect_true(any(grepl("^Census/", keys)))
})

test_that("pumf_registry_keys: all keys round-trip through lookup", {
  keys <- canpumf:::pumf_registry_keys()
  for (k in keys) {
    parts  <- strsplit(k, "/", fixed = TRUE)[[1L]]
    series  <- parts[[1L]]
    version <- paste(parts[-1L], collapse = "/")
    e <- canpumf:::pumf_registry_lookup(series, version)
    expect_false(is.null(e), label = paste0("lookup(", k, ") is not NULL"))
    expect_equal(e$series,  series,  label = paste0(k, " series field"))
    expect_equal(e$version, version, label = paste0(k, " version field"))
  }
})

test_that("pumf_registry_keys: no duplicate keys", {
  keys <- canpumf:::pumf_registry_keys()
  expect_equal(length(keys), length(unique(keys)))
})

# ---- pumf_resolve_version -----------------------------------------------------

test_that("pumf_resolve_version: Census bare year aliases to individuals", {
  expect_equal(canpumf:::pumf_resolve_version("Census", "2021"),
               "2021 (individuals)")
  expect_equal(canpumf:::pumf_resolve_version("Census", "2016"),
               "2016 (individuals)")
  expect_equal(canpumf:::pumf_resolve_version("Census", "1971"),
               "1971 (individuals)")
})

test_that("pumf_resolve_version: Census full version string unchanged", {
  expect_equal(canpumf:::pumf_resolve_version("Census", "2021 (individuals)"),
               "2021 (individuals)")
  expect_equal(canpumf:::pumf_resolve_version("Census", "2021 (hierarchical)"),
               "2021 (hierarchical)")
  expect_equal(canpumf:::pumf_resolve_version("Census", "2001 (households)"),
               "2001 (households)")
})

test_that("pumf_resolve_version: non-Census series unchanged", {
  expect_equal(canpumf:::pumf_resolve_version("SFS",  "2019"), "2019")
  expect_equal(canpumf:::pumf_resolve_version("LFS",  "2023"), "2023")
  expect_equal(canpumf:::pumf_resolve_version("CPSS", "2020"), "2020")
})

test_that("pumf_resolve_version: NULL version returns NULL", {
  expect_null(canpumf:::pumf_resolve_version("Census", NULL))
  expect_null(canpumf:::pumf_resolve_version("SFS",    NULL))
})
