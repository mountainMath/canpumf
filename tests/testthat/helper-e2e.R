# Synthetic single-module survey ('FAKE' 2099) with a two-level PROV factor and
# a WEIGHT with a 9999 missing code.  Shared by test-api.R and
# test-translate.R; it lives in a helper so both files see it.
make_e2e_version_dir <- function(tmp, series = "FAKE", version = "2099") {
  vdir     <- file.path(tmp, series, version)
  meta_dir <- file.path(vdir, "metadata")
  dir.create(meta_dir, recursive = TRUE)

  vars  <- tibble::tibble(
    name = c("PROV","WEIGHT"),
    label_en = c("Province","Survey weight"),
    label_fr = c("Province","Poids"),
    type = c("character","numeric"),
    decimals = c(NA_integer_, 0L),
    missing_low = c(NA_real_, 9999L),
    missing_high = c(NA_real_, 9999L)
  )
  codes <- tibble::tibble(
    name = c("PROV","PROV"),
    val  = c("10","35"),
    label_en = c("Newfoundland","Ontario"),
    label_fr = c("Terre-Neuve","Ontario")
  )
  readr::write_csv(vars,  file.path(meta_dir, "variables.csv"))
  readr::write_csv(codes, file.path(meta_dir, "codes.csv"))
  readr::write_csv(
    tibble::tibble(PROV=c("10","35","10"), WEIGHT=c("100","200","9999")),
    file.path(vdir, "survey.csv")
  )
  # Minimal codebook so pumf_parse_metadata can re-parse on refresh=TRUE
  readr::write_csv(
    tibble::tibble(
      Field_Champ               = c("PROV", NA, NA, "WEIGHT"),
      Variable_Variable         = c("PROV", "10", "35", "WEIGHT"),
      EnglishLabel_EtiquetteAnglais = c("Province","Newfoundland","Ontario","Survey weight"),
      FrenchLabel_EtiquetteFrancais = c("Province","Terre-Neuve","Ontario","Poids")
    ),
    file.path(vdir, "codebook.csv")
  )
  writeLines("", file.path(vdir, "sentinel.txt"))
  vdir
}
