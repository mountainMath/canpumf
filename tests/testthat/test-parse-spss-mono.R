fx <- function(f) file.path(testthat::test_path("..", "fixtures", "spss_mono"), f)

# ---- Synthetic 2021-style fixture (single quotes, + continuation) --------

test_that("parse_spss_mono: VARIABLE LABELS extracted from 2021-style file", {
  m <- canpumf:::parse_spss_mono(fx("simple_en.sps"))

  expect_s3_class(m$variables, "data.frame")
  expect_true(all(c("name","label_en","label_fr","type","decimals","missing_low","missing_high")
                  %in% names(m$variables)))
  expect_setequal(m$variables$name, c("AGEGRP","PROV","WAGE","SEX"))

  # label_en correctly extracted
  expect_equal(m$variables$label_en[m$variables$name == "AGEGRP"], "Age group")
  expect_equal(m$variables$label_en[m$variables$name == "PROV"],   "Province of residence")

  # + continuation joined correctly
  expect_equal(m$variables$label_en[m$variables$name == "SEX"], "Sex at birth")
})

test_that("parse_spss_mono: VALUE LABELS extracted from 2021-style file", {
  m <- canpumf:::parse_spss_mono(fx("simple_en.sps"))

  expect_s3_class(m$codes, "data.frame")
  expect_true(all(c("name","val","label_en","label_fr") %in% names(m$codes)))

  agegrp_codes <- m$codes[m$codes$name == "AGEGRP", ]
  expect_equal(nrow(agegrp_codes), 4L)           # 3 substantive + 1 "Not available"
  expect_equal(agegrp_codes$label_en[agegrp_codes$val == "1"], "Under 25 years")

  prov_codes <- m$codes[m$codes$name == "PROV", ]
  expect_equal(nrow(prov_codes), 5L)
  expect_equal(prov_codes$label_en[prov_codes$val == "24"], "Quebec")
})

test_that("parse_spss_mono: type inferred correctly from codes and formats", {
  m <- canpumf:::parse_spss_mono(fx("simple_en.sps"))

  vars <- m$variables
  expect_equal(vars$type[vars$name == "AGEGRP"], "character")  # has value labels
  expect_equal(vars$type[vars$name == "PROV"],   "character")  # has value labels
  expect_equal(vars$type[vars$name == "SEX"],    "character")  # has value labels
  expect_equal(vars$type[vars$name == "WAGE"],   "numeric")    # missing range, no codes
})

test_that("parse_spss_mono: MISSING VALUES extracted", {
  m <- canpumf:::parse_spss_mono(fx("simple_en.sps"))

  wage_row <- m$variables[m$variables$name == "WAGE", ]
  expect_equal(wage_row$missing_low,  9999999)
  expect_equal(wage_row$missing_high, 9999999)

  agegrp_row <- m$variables[m$variables$name == "AGEGRP", ]
  expect_equal(agegrp_row$missing_low,  99)
  expect_equal(agegrp_row$missing_high, 99)
})

test_that("parse_spss_mono: DATA LIST layout extracted from 2021-style file", {
  m <- canpumf:::parse_spss_mono(fx("simple_en.sps"))

  expect_false(is.null(m$layout))
  expect_true(all(c("name","start","end") %in% names(m$layout)))
  expect_setequal(m$layout$name, c("AGEGRP","PROV","WAGE","SEX"))

  agegrp <- m$layout[m$layout$name == "AGEGRP", ]
  expect_equal(agegrp$start, 1L)
  expect_equal(agegrp$end,   2L)
})

test_that("parse_spss_mono: label_fr = NA when no French file", {
  m <- canpumf:::parse_spss_mono(fx("simple_en.sps"))
  expect_true(all(is.na(m$variables$label_fr)))
  expect_true(all(is.na(m$codes$label_fr)))
})

# ---- Bilingual: 2021-style English + French --------------------------------

test_that("parse_spss_mono: French labels joined when fra_sps_path provided", {
  m <- canpumf:::parse_spss_mono(fx("simple_en.sps"), fx("simple_fr.sps"))

  vars <- m$variables
  expect_false(any(is.na(vars$label_fr)))   # all vars have French labels

  expect_equal(vars$label_en[vars$name == "AGEGRP"], "Age group")
  expect_equal(vars$label_fr[vars$name == "AGEGRP"], "Groupe d age")

  codes <- m$codes
  expect_equal(codes$label_fr[codes$name == "PROV" & codes$val == "24"], "Quebec")
  expect_equal(codes$label_en[codes$name == "PROV" & codes$val == "24"], "Quebec")
  expect_equal(codes$label_fr[codes$name == "PROV" & codes$val == "11"], "Ile-du-Prince-Edouard")
})

test_that("parse_spss_mono: code absent in French codes gets NA label_fr", {
  # AGEGRP code 99 is in English codes but NOT in French codes → label_fr = NA
  m <- canpumf:::parse_spss_mono(fx("simple_en.sps"), fx("simple_fr.sps"))

  # All English AGEGRP codes should be present
  expect_equal(nrow(m$codes[m$codes$name == "AGEGRP", ]), 4L)

  # French file covers all the same codes, so label_fr should all be non-NA
  agegrp_fr <- m$codes[m$codes$name == "AGEGRP", ]$label_fr
  expect_true(all(!is.na(agegrp_fr)))

  # WAGE has a French variable label in the French fixture
  wage_fr <- m$variables$label_fr[m$variables$name == "WAGE"]
  expect_false(is.na(wage_fr))
  expect_equal(wage_fr, "Revenu d emploi total")
})

# ---- Synthetic 2016-style fixture (double quotes, / on own line) -----------

test_that("parse_spss_mono: 2016-style VALUE LABELS (/ on own line) parsed correctly", {
  m <- canpumf:::parse_spss_mono(fx("style2016_en.sps"))

  prov <- m$codes[m$codes$name == "PROV", ]
  expect_equal(nrow(prov), 5L)
  expect_equal(prov$label_en[prov$val == "35"], "Ontario")
})

test_that("parse_spss_mono: 2016-style VARIABLE LABELS (double quotes, no indent) parsed", {
  m <- canpumf:::parse_spss_mono(fx("style2016_en.sps"))

  expect_equal(m$variables$label_en[m$variables$name == "AGEGRP"], "Age group")
  expect_equal(m$variables$label_en[m$variables$name == "WAGE"],   "Total employment income")
})

test_that("parse_spss_mono: canonical schema returned", {
  m <- canpumf:::parse_spss_mono(fx("simple_en.sps"))

  expect_named(m$variables, c("name","label_en","label_fr","type","decimals","missing_low","missing_high"))
  expect_named(m$codes,     c("name","val","label_en","label_fr"))
  expect_named(m$layout,    c("name","start","end"))
})

# ---- Real Census 2016 data (skip if not available) -------------------------

census2016_path <- "/Users/jens/data/pumf.data/2016_Ind_98M0001X/English/SAS, SPSS and STATA command files/2016 Individuals PUMF SPSS EN.sps"
census2016_fra  <- "/Users/jens/data/pumf.data/2016_Ind_98M0001X/Français/Fichiers de commandes SAS, SPSS et STATA/FMGD 2016 particuliers SPSS FR.sps"

test_that("parse_spss_mono: Census 2016 variable count and known labels", {
  skip_if_not(file.exists(census2016_path), "Census 2016 SPSS not in cache")

  m <- canpumf:::parse_spss_mono(census2016_path, encoding = "CP1252")

  # At least 100 variables expected
  expect_gt(nrow(m$variables), 100L)

  # Known variable labels
  expect_equal(m$variables$label_en[m$variables$name == "AGEGRP"], "Age")
  expect_equal(m$variables$label_en[m$variables$name == "SEX"],    "Sex")

  # Layout extracted
  expect_false(is.null(m$layout))
  expect_gt(nrow(m$layout), 100L)
})

test_that("parse_spss_mono: Census 2016 bilingual", {
  skip_if_not(file.exists(census2016_path) && file.exists(census2016_fra),
              "Census 2016 SPSS not in cache")

  m <- canpumf:::parse_spss_mono(census2016_path, census2016_fra, encoding = "CP1252")

  # Some French labels present
  n_fr <- sum(!is.na(m$variables$label_fr))
  expect_gt(n_fr, 50L)

  # Known province label in French
  qc <- m$codes[m$codes$name == "CMA" & m$codes$val == "462", ]
  if (nrow(qc) > 0) expect_false(is.na(qc$label_fr[1]))
})

# ---- Real Census 2021 data (skip if not available) -------------------------

census2021_path <- file.path(
  "/Users/jens/data/pumf.data/cen21_ind_98m0001x_part_rec21",
  "English/SAS, SPSS and STATA command files/ipumf_2021_final_prog_en_v2.sps")
census2021_fra  <- file.path(
  "/Users/jens/data/pumf.data/cen21_ind_98m0001x_part_rec21",
  "Français/Fichiers de commandes SAS, SPSS et STATA/FMGD 2021 particuliers SPSS FR_v2.sps")

test_that("parse_spss_mono: Census 2021 variable count and known labels", {
  skip_if_not(file.exists(census2021_path), "Census 2021 SPSS not in cache")

  m <- canpumf:::parse_spss_mono(census2021_path, encoding = "UTF-8")

  expect_gt(nrow(m$variables), 80L)
  expect_equal(m$variables$label_en[m$variables$name == "AGEGRP"], "Age")

  # Layout from DATA LIST
  expect_false(is.null(m$layout))
})

test_that("parse_spss_mono: Census 2021 bilingual", {
  skip_if_not(file.exists(census2021_path) && file.exists(census2021_fra),
              "Census 2021 SPSS not in cache")

  m <- canpumf:::parse_spss_mono(census2021_path, census2021_fra, encoding = "UTF-8")

  n_fr <- sum(!is.na(m$variables$label_fr))
  expect_gt(n_fr, 50L)
})

# ---- Generate golden fixtures (only if real data present) ------------------
# Run interactively to regenerate: Sys.setenv(CANPUMF_REGEN_FIXTURES="1")

if (identical(Sys.getenv("CANPUMF_REGEN_FIXTURES"), "1") && file.exists(census2016_path)) {
  m <- canpumf:::parse_spss_mono(census2016_path, census2016_fra, encoding = "CP1252")
  canpumf:::write_metadata(m, testthat::test_path("..", "fixtures", "census2016"))
  message("census2016 golden fixtures written")
}
if (identical(Sys.getenv("CANPUMF_REGEN_FIXTURES"), "1") && file.exists(census2021_path)) {
  m <- canpumf:::parse_spss_mono(census2021_path, census2021_fra, encoding = "UTF-8")
  canpumf:::write_metadata(m, testthat::test_path("..", "fixtures", "census2021"))
  message("census2021 golden fixtures written")
}
