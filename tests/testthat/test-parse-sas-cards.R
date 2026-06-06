fx    <- function(...) testthat::test_path("..", "fixtures", "sas_cards", ...)
fx_at <- function(...) testthat::test_path("..", "fixtures", "sas_cards_at", ...)

# ---- Synthetic reading-card fixture (NAME start-end format) -------------

test_that("parse_sas_cards: variable labels extracted from .lbe", {
  m <- canpumf:::parse_sas_cards(fx())

  expect_s3_class(m$variables, "data.frame")
  expect_true(all(c("name","label_en","label_fr","type","missing_low","missing_high")
                  %in% names(m$variables)))
  expect_setequal(m$variables$name, c("CASEID","PROV","INCOME","AGEGRP","SEX"))
  expect_equal(m$variables$label_en[m$variables$name == "INCOME"],
               "Total household income")
  # Names are uppercased from .lbe (already uppercase in fixture)
  expect_true(all(m$variables$name == toupper(m$variables$name)))
})

test_that("parse_sas_cards: value labels extracted from .cde", {
  m <- canpumf:::parse_sas_cards(fx())

  prov <- m$codes[m$codes$name == "PROV", ]
  expect_equal(nrow(prov), 5L)
  expect_equal(prov$label_en[prov$val == "10"], "Newfoundland and Labrador")
  expect_equal(prov$label_en[prov$val == "35"], "Ontario")

  sex <- m$codes[m$codes$name == "SEX", ]
  expect_equal(nrow(sex), 3L)
  expect_equal(sex$label_en[sex$val == "1"], "Male")
})

test_that("parse_sas_cards: missing values from .mvs (including open-ended)", {
  m <- canpumf:::parse_sas_cards(fx())

  inc <- m$variables[m$variables$name == "INCOME", ]
  expect_equal(inc$missing_low,  99999996)
  expect_equal(inc$missing_high, 99999999)

  # Open-ended range: AGEGRP (9 THRU ) → missing_low = 9, missing_high = NA
  age <- m$variables[m$variables$name == "AGEGRP", ]
  expect_equal(age$missing_low, 9)
  expect_true(is.na(age$missing_high))
})

test_that("parse_sas_cards: layout from NAME start-end .lay", {
  m <- canpumf:::parse_sas_cards(fx())

  expect_false(is.null(m$layout))
  expect_setequal(m$layout$name, c("CASEID","PROV","INCOME","AGEGRP","SEX"))

  inc <- m$layout[m$layout$name == "INCOME", ]
  expect_equal(inc$start, 9L)
  expect_equal(inc$end,  16L)

  # Single-column variable
  sex <- m$layout[m$layout$name == "SEX", ]
  expect_equal(sex$start, 19L)
  expect_equal(sex$end,   19L)
})

test_that("parse_sas_cards: type inferred from codes, missing, and (A) annotation", {
  m <- canpumf:::parse_sas_cards(fx())

  vars <- m$variables
  expect_equal(vars$type[vars$name == "PROV"],   "character")  # has codes
  expect_equal(vars$type[vars$name == "AGEGRP"], "character")  # has codes
  expect_equal(vars$type[vars$name == "INCOME"], "numeric")    # has missing, no codes
  expect_equal(vars$type[vars$name == "CASEID"], "character")  # (A) annotation
})

test_that("parse_sas_cards: bilingual — label_fr from .lbf and .cdf", {
  m <- canpumf:::parse_sas_cards(fx())

  expect_false(any(is.na(m$variables$label_fr)))
  expect_equal(m$variables$label_fr[m$variables$name == "INCOME"],
               "Revenu total du menage")

  prov_fr <- m$codes[m$codes$name == "PROV" & m$codes$val == "10", ]$label_fr
  expect_equal(prov_fr, "Terre-Neuve-et-Labrador")
})

test_that("parse_sas_cards: label_fr = NA when no French files", {
  tmp <- withr::local_tempdir()
  file.copy(fx("test.lay"),  file.path(tmp, "test.lay"))
  file.copy(fx("test.lbe"),  file.path(tmp, "test.lbe"))
  file.copy(fx("test.cde"),  file.path(tmp, "test.cde"))
  file.copy(fx("test.mvs"),  file.path(tmp, "test.mvs"))

  m <- canpumf:::parse_sas_cards(tmp)
  expect_true(all(is.na(m$variables$label_fr)))
  expect_true(all(is.na(m$codes$label_fr)))
})

test_that("parse_sas_cards: canonical schema", {
  m <- canpumf:::parse_sas_cards(fx())
  expect_named(m$variables, c("name","label_en","label_fr","type","missing_low","missing_high"))
  expect_named(m$codes,     c("name","val","label_en","label_fr"))
  expect_named(m$layout,    c("name","start","end"))
})

# ---- @pos format fixture ------------------------------------------------

test_that("parse_sas_cards: @pos SAS input format layout", {
  m <- canpumf:::parse_sas_cards(fx_at())

  expect_false(is.null(m$layout))
  expect_true("CASEID" %in% m$layout$name)

  caseid <- m$layout[m$layout$name == "CASEID", ]
  expect_equal(caseid$start, 1L)
  expect_equal(caseid$end,   6L)

  income <- m$layout[m$layout$name == "INCOME", ]
  expect_equal(income$start,  9L)
  expect_equal(income$end,   18L)

  sex <- m$layout[m$layout$name == "SEX", ]
  expect_equal(sex$start, 19L)
  expect_equal(sex$end,   19L)
})

# ---- Real SHS 2019 data (skip if not in cache) -------------------------

shs2019_dir <- "/Users/jens/data/pumf.data/SHS/2019/Reading cards/SPSS"

test_that("parse_sas_cards: SHS 2019 variable count and known labels", {
  skip_if_not(dir.exists(shs2019_dir), "SHS 2019 not in cache")

  m <- canpumf:::parse_sas_cards(shs2019_dir, encoding = "Latin1")

  expect_gt(nrow(m$variables), 30L)
  expect_equal(m$variables$label_en[m$variables$name == "PROV"], "Geography")

  # Both quoted and unquoted codes present
  prov <- m$codes[m$codes$name == "PROV", ]
  expect_gt(nrow(prov), 3L)

  # Layout extracted
  expect_false(is.null(m$layout))
  expect_gt(nrow(m$layout), 30L)
})

test_that("parse_sas_cards: SHS 2019 bilingual", {
  skip_if_not(dir.exists(shs2019_dir), "SHS 2019 not in cache")

  m <- canpumf:::parse_sas_cards(shs2019_dir, encoding = "Latin1")
  n_fr <- sum(!is.na(m$variables$label_fr))
  expect_gt(n_fr, 20L)
})

test_that("parse_sas_cards: SFS 2005 SasCard @pos format", {
  sfs2005_dir <- "/Users/jens/data/pumf.data/SFS2005/Command Files/SasCard"
  skip_if_not(dir.exists(sfs2005_dir), "SFS 2005 not in cache")

  # SFS 2005 SasCard uses @pos layout and LABEL format (different from VARIABLE LABELS)
  # We expect the layout to parse but variable labels may be empty
  m <- canpumf:::parse_sas_cards(sfs2005_dir, encoding = "Latin1")
  # At minimum, layout should be extracted from the @pos .lay file
  expect_false(is.null(m$layout))
  expect_gt(nrow(m$layout), 10L)
})

# ---- Generate golden fixtures ----------------------------------------
if (identical(Sys.getenv("CANPUMF_REGEN_FIXTURES"), "1") && dir.exists(shs2019_dir)) {
  m <- canpumf:::parse_sas_cards(shs2019_dir, encoding = "Latin1")
  canpumf:::write_metadata(m, testthat::test_path("..", "fixtures", "shs2019"))
  message("shs2019 golden fixtures written")
}
