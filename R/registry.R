# R/registry.R — Survey-specific configuration registry.
#
# Each entry captures the non-derivable per-(series, version) configuration
# needed by the three-stage pipeline: layout/BSW masks, file masks, encoding
# overrides, and raw-data fixups applied before label mapping in Stage 3.
#
# Surveys not listed here fall back to auto-detection with no special handling.
# That covers the generic read_pumf_data() path for manually-deposited directories.
#
# data_fixups structure (applied to raw character data before label mapping):
#   str_pad:    list of list(cols, width, side, pad) — left/right-pad raw values
#   rename:     named character vector c(old_name = "new_name") — column renames
#               (applied only when the old column exists; safe for conditional renames)
#   na_values:  character vector of raw values that should become NA for all
#               numeric columns (applied in .apply_numeric_conversion)

.make_entry <- function(series,
                        version,
                        layout_mask       = NULL,
                        bsw_mask          = NULL,
                        bsw_file_mask     = NULL,
                        bsw_join_key      = NULL,
                        bsw_drop_cols     = character(0L),
                        file_mask         = NULL,
                        data_encoding     = "CP1252",
                        metadata_encoding = "Latin1",
                        data_fixups       = list()) {
  list(
    series            = series,
    version           = version,
    layout_mask       = layout_mask,
    bsw_mask          = bsw_mask,
    bsw_file_mask     = bsw_file_mask,
    bsw_join_key      = bsw_join_key,
    bsw_drop_cols     = bsw_drop_cols,
    file_mask         = file_mask,
    data_encoding     = data_encoding,
    metadata_encoding = metadata_encoding,
    data_fixups       = data_fixups
  )
}

# Census of Population income variables use 8-digit sentinel codes that are
# not declared in any machine-readable command file.  Confirmed for 2021 from
# the PUMF User Guide: every 8-char quantitative variable (CapGn, CHDBN,
# CHLDC, COVID_ERB, CQPPB, EICBN, EmpIn, GovtI, GTRfs, IncTax, Invst,
# MrkInc, OASGI, OtInc, Retir, SempI, TotInc, TotInc_AT, Value, Wages) uses:
#   99999999 = not applicable  (e.g. persons aged < 15)
#   88888888 = not available
# The 4-char SHELCO variable has no sentinel ("ranges from 0 to the maximum"),
# and cannot contain an 8-digit value regardless.
# Applying these na_values to all numeric columns is safe: any column narrower
# than 8 chars cannot produce a false positive, only a false negative (its
# own sentinel, e.g. "999999" for a 6-char field, would go unhandled).
#
# Scope: applied to 2001–2021 where the 8-char income field format is
# established.  Earlier years (1996 and before) used narrower income fields
# with different-length sentinels and should be verified separately before
# adding na_values.
.census_na  <- c("99999999", "88888888")
.census_fixup <- list(na_values = .census_na)

# SFS 2016/2019/2023: three categorical columns arrive as "1" / "2" instead of
# "01" / "02" — the SPSS command files expect the zero-padded forms.
.sfs_pad_fixup <- list(
  str_pad = list(
    list(cols  = c("PASRBUYG", "PASRDWNG", "PASRMPFG"),
         width = 2L,
         side  = "left",
         pad   = "0")
  )
)

.pumf_registry <- list(

  # ---- SFS: Survey of Financial Security ------------------------------------

  # 2023: main data uses EFAM_PUMF_[^R] SPSS files; BSW data is in the separate
  # BSWEIGHTS_PUMF.txt file whose layout comes from sfs2023_bsweights_pumf_i.sps.
  "SFS/2023" = .make_entry("SFS", "2023",
    layout_mask   = "EFAM_PUMF_[^R]",
    bsw_mask      = "bsweights_pumf",
    bsw_file_mask = "BSWEIGHTS_PUMF\\.txt",
    bsw_join_key  = "PEFAMID",
    bsw_drop_cols = "PWEIGHT",
    file_mask     = "EFAM_PUMF\\.txt",
    data_fixups   = .sfs_pad_fixup),

  "SFS/2019" = .make_entry("SFS", "2019",
    layout_mask   = "EFAM_PUMF",
    bsw_mask      = "bsweights",
    bsw_file_mask = "BSWEIGHTS_PUMF\\.txt",
    bsw_join_key  = "PEFAMID",
    bsw_drop_cols = "PWEIGHT",
    file_mask     = "EFAM_PUMF",
    data_fixups   = .sfs_pad_fixup),

  "SFS/2016" = .make_entry("SFS", "2016",
    layout_mask   = "EFAM_PUMF",
    bsw_mask      = "bsweights",
    bsw_file_mask = "BSWEIGHTS_PUMF\\.txt",
    bsw_join_key  = "PEFAMID",
    bsw_drop_cols = "PWEIGHT",
    file_mask     = "EFAM_PUMF",
    data_fixups   = .sfs_pad_fixup),

  # 2012: no bootstrap weights, no padding fixup
  "SFS/2012" = .make_entry("SFS", "2012"),

  # ---- CHS: Canadian Housing Survey ----------------------------------------

  "CHS/2018" = .make_entry("CHS", "2018",
    layout_mask   = "chs2018ecl_pumf",
    file_mask     = "CHS2018ECL_PUMF\\.csv"),

  "CHS/2021" = .make_entry("CHS", "2021",
    layout_mask   = "CHS2021ECL_PUMF",
    bsw_mask      = "chs2021ecl_PUMF_bsw",
    bsw_file_mask = "chs2021ecl_PUMF_bsw\\.csv",
    bsw_join_key  = "PUMFID",
    file_mask     = "CHS2021ECL_PUMF\\.csv"),

  "CHS/2022" = .make_entry("CHS", "2022",
    layout_mask   = "chs2022ecl_pumf",
    bsw_mask      = "chs2022ecl_pumf_bsw",
    bsw_file_mask = "chs2022ecl_pumf_bsw\\.csv",
    bsw_join_key  = "PUMFID",
    file_mask     = "chs2022ecl_pumf\\.csv"),

  # ---- SHS: Survey of Household Spending ------------------------------------

  # 2019: fixed-width flatfile; metadata is in SAS reading cards format.
  # bsw_join_key is not documented in the existing code; "PUMFID" is the
  # standard StatCan respondent ID and likely correct here too.
  "SHS/2019" = .make_entry("SHS", "2019",
    layout_mask   = "shs2019_flatfile",
    bsw_mask      = "_bsw_flatfile",
    bsw_file_mask = "_bsw_flatfile",
    bsw_join_key  = "PUMFID",
    file_mask     = "pumf_shs2019\\.txt"),

  # ---- ITS: International Travel Survey -------------------------------------
  # No ITS-specific parsing quirks discovered yet; entries are placeholders
  # so Stage 1 can find these versions in the collection without error.
  "ITS/2018" = .make_entry("ITS", "2018"),
  "ITS/2019" = .make_entry("ITS", "2019"),

  # ---- Census of Population -------------------------------------------------
  # 2021 and 2016 are downloadable. Older years are EFT-only (user deposits zip).
  # All Census files use CP1252-encoded data; 2021 uses UTF-8 metadata (command
  # files shipped as UTF-8), older years use CP1252/Latin1 metadata.
  #
  # Income variables (8-char-wide fields: CHDBN, COVID_ERB, CQPPB, CapGn, ChldC,
  # EICBN, EmpIn, GovtI, GTRfs, IncTax, Invst, MrkInc, OASGI, OtInc, Retir,
  # SempI, TotInc, TotInc_AT, Value, Wages, …) use sentinel codes not declared in
  # any machine-readable command file; handled via .census_fixup defined above.

  # 2021: CSV data, UTF-8 SPSS command files. Older releases named the religion
  # variable RELIGION_DER; newer releases fixed this to RELIG. The rename fixup
  # is applied only when the old column exists (safe for both release variants).
  "Census/2021 (individuals)" = .make_entry("Census", "2021 (individuals)",
    metadata_encoding = "UTF-8",
    file_mask         = "\\.csv",
    data_fixups       = c(.census_fixup, list(rename = c(RELIGION_DER = "RELIG")))),

  "Census/2021 (hierarchical)" = .make_entry("Census", "2021 (hierarchical)",
    metadata_encoding = "UTF-8",
    file_mask         = "\\.csv",
    data_fixups       = .census_fixup),

  # 2016: fixed-width .dat file, CP1252 metadata
  "Census/2016 (individuals)" = .make_entry("Census", "2016 (individuals)",
    file_mask   = "\\.dat",
    data_fixups = .census_fixup),

  "Census/2016 (hierarchical)" = .make_entry("Census", "2016 (hierarchical)",
    file_mask   = "\\.dat",
    data_fixups = .census_fixup),

  # 2011 NHS: fixed-width .dat
  "Census/2011 (individuals)" = .make_entry("Census", "2011 (individuals)",
    file_mask   = "\\.dat",
    data_fixups = .census_fixup),

  "Census/2011 (hierarchical)" = .make_entry("Census", "2011 (hierarchical)",
    file_mask   = "\\.dat",
    data_fixups = .census_fixup),

  # 2006: fixed-width .dat; individuals file may be in an "Individual file" subdir
  "Census/2006 (individuals)" = .make_entry("Census", "2006 (individuals)",
    file_mask   = "\\.dat",
    data_fixups = .census_fixup),

  "Census/2006 (hierarchical)" = .make_entry("Census", "2006 (hierarchical)",
    file_mask   = "\\.dat",
    data_fixups = .census_fixup),

  # 2001: fixed-width .dat; three file types
  "Census/2001 (individuals)" = .make_entry("Census", "2001 (individuals)",
    file_mask   = "\\.dat",
    data_fixups = .census_fixup),

  "Census/2001 (households)" = .make_entry("Census", "2001 (households)",
    file_mask   = "\\.dat",
    data_fixups = .census_fixup),

  "Census/2001 (families)" = .make_entry("Census", "2001 (families)",
    file_mask   = "\\.dat",
    data_fixups = .census_fixup),

  # 1996 and earlier: income field widths have not been confirmed — do NOT apply
  # census_fixup until the sentinel values are verified from the user guides.
  # (8-char fields with 99999999/88888888 are confirmed for 2001+; pre-1996
  # files likely use narrower fields with shorter all-9s/all-8s sentinels.)
  "Census/1996 (individuals)" = .make_entry("Census", "1996 (individuals)",
    file_mask   = "\\.dat"),

  "Census/1996 (households)" = .make_entry("Census", "1996 (households)",
    file_mask   = "\\.dat"),

  "Census/1996 (families)" = .make_entry("Census", "1996 (families)",
    file_mask   = "\\.dat"),

  # 1991: fixed-width files extracted from sub-archives (hhld91.zip, etc.)
  "Census/1991 (individuals)" = .make_entry("Census", "1991 (individuals)",
    file_mask   = "\\.(dat|DAT|txt)"),

  "Census/1991 (households)" = .make_entry("Census", "1991 (households)",
    file_mask   = "\\.(dat|DAT|txt)"),

  "Census/1991 (families)" = .make_entry("Census", "1991 (families)",
    file_mask   = "\\.(dat|DAT|txt)"),

  # 1986–1971: fixed-width files extracted from version-specific sub-archives.
  # 1981 data files use a .DAT extension; 1971 has CMA and PR level variants.
  "Census/1986 (individuals)" = .make_entry("Census", "1986 (individuals)",
    file_mask   = "\\.(txt|DAT)"),

  "Census/1986 (households)" = .make_entry("Census", "1986 (households)",
    file_mask   = "\\.(txt|DAT)"),

  "Census/1981 (individuals)" = .make_entry("Census", "1981 (individuals)",
    file_mask   = "\\.(txt|DAT)"),

  "Census/1981 (households)" = .make_entry("Census", "1981 (households)",
    file_mask   = "\\.(txt|DAT)"),

  "Census/1976 (individuals)" = .make_entry("Census", "1976 (individuals)",
    file_mask   = "\\.(txt|DAT)"),

  "Census/1971 (individuals)" = .make_entry("Census", "1971 (individuals)",
    file_mask   = "\\.(txt|DAT)"),

  "Census/1971 (individuals PR)" = .make_entry("Census", "1971 (individuals PR)",
    file_mask   = "\\.(txt|DAT)",
    data_fixups = .census_fixup)
)

#' Resolve version aliases
#'
#' Canonicalises user-supplied version strings.  Currently the only alias is
#' for the Census of Population: a bare four-digit year (e.g. `"2021"`)
#' resolves to `"<year> (individuals)"`.
#'
#' @param series survey series acronym
#' @param version raw version string supplied by the caller, or `NULL`
#' @return canonical version string (or `NULL` if `version` was `NULL`)
#' @keywords internal
pumf_resolve_version <- function(series, version) {
  if (is.null(version)) return(NULL)
  if (series == "Census" && grepl("^\\d{4}$", version))
    return(paste0(version, " (individuals)"))
  version
}

#' Look up survey registry configuration
#'
#' Returns the configuration entry for a given survey series and version, or
#' `NULL` if the survey falls back to auto-detection.
#'
#' @param series survey series acronym (e.g. `"SFS"`)
#' @param version survey version string (e.g. `"2019"`)
#' @return named list of configuration fields, or `NULL` if not in registry
#' @keywords internal
pumf_registry_lookup <- function(series, version) {
  .pumf_registry[[paste0(series, "/", version)]]
}

#' List all registered survey keys
#'
#' @return character vector of `"series/version"` keys
#' @keywords internal
pumf_registry_keys <- function() {
  names(.pumf_registry)
}
