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
                        metadata_encoding = "CP1252",
                        data_fixups       = list(),
                        bundled_eng_sps   = NULL,
                        bundle_source     = NULL,
                        bundle_sps_mask   = NULL) {
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
    data_fixups       = data_fixups,
    bundled_eng_sps   = bundled_eng_sps,
    bundle_source     = bundle_source,
    bundle_sps_mask   = bundle_sps_mask
  )
}

# Census of Population income variables use undeclared sentinel codes.
# The sentinel width matches the income field width, which changed across years:
#
#   2016–2021: 8-char income fields (EmpIn, MrkInc, TotInc, Value, …)
#     99999999 = not applicable  (e.g. persons aged < 15)
#     88888888 = not available
#
#   1991–2011: 7-char income fields (same variables, narrower layout)
#     9999999  = not applicable
#     8888888  = not available
#
# Do NOT combine these into one vector: "9999999" applied to 8-char fields
# would treat a valid $9,999,999 income (stored as " 9999999", trimmed to
# "9999999") as NA.
#
# 1986 and earlier: guides do not document income sentinels and a field-aligned
# data scan finds none — no na_values applied (see Census 1986 comment below).
# 1991–2001 guides document only 9999999 ("Not applicable"); 8888888 first
# appears in the 2006 documentation.  It is kept in .census_fixup_7 for
# 1991–2001 as a harmless guard: a field-aligned scan shows it never occurs in
# those years' data, and top-coding makes a legitimate $8,888,888 impossible.
# SGVP: HSDSIZEC ("household size", top-coded) and CHH0014C ("children 0-14",
# top-coded) have boundary labels alongside unlabeled lower values;
# force_numeric prevents those values from becoming NAs.  Presence varies by
# cycle: 2013 has both, 2023/2018 only HSDSIZEC (children variables are
# categorical), 2010 and earlier use different names (e.g. DH1GHHSZ) that are
# fully labeled categoricals needing no fixup.

.census_fixup_8 <- list(na_values = c("99999999", "88888888"))
.census_fixup_7 <- list(na_values = c("9999999",  "8888888"))

# 1971: SUBSAMPL is an integer sub-sample index (0 for households, 1–5 for
# individuals/families), but all six SPSS files only declare code 1 → 'one'.
# Force it to numeric so the integer values come through correctly.
.census_fixup_1971 <- list(force_numeric = "SUBSAMPL")


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
    file_mask     = "EFAM_PUMF\\.txt"),

  "SFS/2019" = .make_entry("SFS", "2019",
    layout_mask   = "EFAM_PUMF",
    bsw_mask      = "bsweights",
    bsw_file_mask = "BSWEIGHTS_PUMF\\.txt",
    bsw_join_key  = "PEFAMID",
    bsw_drop_cols = "PWEIGHT",
    file_mask     = "EFAM_PUMF\\.txt"),

  "SFS/2016" = .make_entry("SFS", "2016",
    layout_mask   = "EFAM_PUMF",
    bsw_mask      = "bsweights",
    bsw_file_mask = "BSWEIGHTS_PUMF\\.txt",
    bsw_join_key  = "PEFAMID",
    bsw_drop_cols = "PWEIGHT",
    file_mask     = "EFAM_PUMF\\.txt"),

  # 2012: no bootstrap weights, no padding fixup
  "SFS/2012" = .make_entry("SFS", "2012"),

  # 2005: no bootstrap weights; SAS cards format (SpssCard/ subdir)
  "SFS/2005" = .make_entry("SFS", "2005",
    file_mask = "ec2005ef\\.txt"),

  # 1999: DATA LIST-only SPSS file (no VARIABLE/VALUE LABELS); no bootstrap
  # weights; FWF data file is in the DATA/ subdirectory.
  "SFS/1999" = .make_entry("SFS", "1999",
    file_mask = "ec1999ef\\.sdf"),

  # ---- CIS: Canadian Income Survey ------------------------------------------
  # Data dir contains CIS{year}_PUMF.txt (FWF), CIS{year}_PUMF.csv, Readme.txt
  # and Lisezmoi.txt; file_mask selects the FWF data file unambiguously.

  "CIS/2022" = .make_entry("CIS", "2022", file_mask = "PUMF\\.txt"),
  "CIS/2021" = .make_entry("CIS", "2021", file_mask = "PUMF\\.txt"),
  "CIS/2020" = .make_entry("CIS", "2020", file_mask = "PUMF\\.txt"),
  "CIS/2019" = .make_entry("CIS", "2019", file_mask = "PUMF\\.txt"),
  "CIS/2018" = .make_entry("CIS", "2018", file_mask = "PUMF\\.txt"),

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

  # 2017: Interview and Diary files in same directory; layout_mask selects the
  # Interview reading cards. BSW layout is a SAS @pos .txt co-located with data.
  "SHS/2017" = .make_entry("SHS", "2017",
    layout_mask   = "Interview",
    bsw_file_mask = "interview_bsw_flatfile\\.txt",
    bsw_join_key  = "CASEID",
    file_mask     = "interview_flatfile\\.txt"),

  # 2019: fixed-width flatfile; BSW layout is a SAS @pos .txt co-located with data.
  "SHS/2019" = .make_entry("SHS", "2019",
    layout_mask   = "shs2019_flatfile",
    bsw_mask      = "_bsw_flatfile",
    bsw_file_mask = "bsw_flatfile\\.txt",
    bsw_join_key  = "CASEID",
    file_mask     = "shs2019_flatfile\\.txt"),

  # 2021: SPSS split-file format; BSW layout is a SAS @pos .txt file co-located
  # with the BSW data (not in the SPSS cards dir); fallback in .read_bsw_data
  # handles this automatically. Join key is CASEID (uppercased from "CaseID").
  "SHS/2021" = .make_entry("SHS", "2021",
    bsw_file_mask = "bsw_flatfile\\.txt",
    bsw_join_key  = "CASEID",
    file_mask     = "PUMF_SHS_2021\\.txt"),

  # ---- GSS: General Social Survey -------------------------------------------
  # 1996 (cycle 11): monolithic SPSS; data in C11MDFAscRecLay-Eng/.
  # The French SPSS file (C11MICF.SPS) uses CP850 (DOS-era encoding); CP1252
  # has undefined code points for bytes like 0x90 (É in CP850), causing a
  # segfault in readr.  All 40 numeric variables use "DO NOT KNOW" / "DO NOT
  # KNOW (PROXY ONLY)" rather than the contraction — handled by .sentinel_pat.
  "GSS/1996" = .make_entry("GSS", "1996",
    file_mask         = "c11mice\\.dat",
    metadata_encoding = "CP850"),

  # 2007 (cycle 21): monolithic SPSS (multiple small VARIABLE/VALUE LABELS
  # blocks, one per module). The SAS cards file provides full 951-variable
  # coverage including PROC FORMAT VALUE blocks parsed by
  # parse_sas_data_labels().  ~97 continuous count/age/amount variables have
  # boundary or group codes in their VALUE LABELS that conflict with raw
  # continuous data (e.g. AGE_CU1C: 15="15 years and less", 80="80 years and
  # more" alongside decimal ages 016.0-079.0) — force_numeric prevents valid
  # data from becoming NA; their Not asked/Not stated/Don't know sentinels
  # (997-999, 999.7-999.9, 99997-99999) become per-variable missing ranges.
  "GSS/2007" = .make_entry("GSS", "2007",
    file_mask   = "C21PUMFM\\.DAT",
    data_fixups = list(force_numeric = c(
      "NO_GRNDCHDC", "MAR_Q101C",  "AGE_LSTPDWKC", "MAR_Q174C",
      "WKWEHOHR_C",  "AGE_STARTWK_L12MTHC", "MAR_Q370", "MAR_Q470",
      "CCW_Q165C",   "RPR_Q100",   "SIP_Q120",   "CTC_Q165C",
      "CAR_Q140C",   "CAR_Q320C",  "AGE_CRP_BEGC",
      "ICG_Q140C",   "ICG_Q150C",
      "AGE_CGP_Q110C", "AGE_CGP_Q115C", "AGE_CGP_BEGC",
      "AGE_CGI_01_BEGC", "AGE_CGI_01_ENDC",
      "AGE_CGI_02_BEGC", "AGE_CGI_02_ENDC",
      "AGE_CGI_03_BEGC", "AGE_CGI_03_ENDC",
      "AGE_CGI_04_BEGC", "AGE_CGI_04_ENDC",
      "AGE_CGI_05_BEGC", "AGE_CGI_05_ENDC",
      "AGE_HLE_BEG_C",   "AGE_HLE_END_C",   "AGE_HLE_DISC_C",
      "AGE_EAH_BEG_C",   "AGE_EAH_END_C",   "AGE_EAH_DISC_C",
      "AGE_SMK_DAILY_BEGC", "AGE_SMK_OCCA_BEGC",
      "YEARSMKDAILY", "YEARSMKOCCASION",
      "MPT_Q090C", "TLE_Q120C", "TLE_Q130C", "TLE_Q230C",
      "MAR_Q383C", "MAR_Q482C", "MAR_Q483C",
      "AGE_CGP_ENDC",
      # Decimal-age/hours variables whose codes come from the SAS cards
      # PROC FORMAT blocks (marriage/common-law/children age histories):
      "AGE_MA2C", "AGE_MA3C", "AGE_SEP_MA1C", "AGE_SEP_MA2C",
      "AGE_DIV_MA2C",
      "AGE_CU1C", "AGE_CU2C", "AGE_CU3C", "AGE_CU4C", "AGE_CU5C",
      "AGE_SEP_CU1C", "AGE_SEP_CU2C", "AGE_SEP_CU3C", "AGE_SEP_CU4C",
      "AGE_DTH_CU2C",
      "AGECHDC_1", "AGECHDC_2", "AGECHDC_3", "AGECHDC_4",
      "AGECHDC_5", "AGECHDC_6", "AGECHDC_7", "AGECHDC_8",
      "AGECHDIED_1", "AGECHDIED_2", "AGECHDIED_3", "AGECHDIED_4",
      "AGECHDIED_5", "AGECHDIED_6", "AGECHDIED_7", "AGECHDIED_8",
      "AGECHDJOIN_HOMC_1", "AGECHDJOIN_HOMC_2", "AGECHDJOIN_HOMC_3",
      "AGECHDJOIN_HOMC_4", "AGECHDJOIN_HOMC_5", "AGECHDJOIN_HOMC_6",
      "AGECHDJOIN_HOMC_7", "AGECHDJOIN_HOMC_8",
      "AGECHDLEFT_HOM_1", "AGECHDLEFT_HOM_2", "AGECHDLEFT_HOM_3",
      "AGECHDLEFT_HOM_4", "AGECHDLEFT_HOM_5", "AGECHDLEFT_HOM_6",
      "AGECHDLEFT_HOMC_7", "AGECHDLEFT_HOM_8",
      "WKWEHR_C", "MAP_Q135C"
    ),
    # Child/marriage age histories carry special codes below the missing band
    # (999.3 "Knowledge of child is unknown", 999.5 "Child deceased" /
    # "No separation prior to divorce or annulment") that are not ages;
    # widen the missing range to cover them.
    missing_supplement = c(
      stats::setNames(
        rep(list(c(999.3, 999.9)), 24L),
        c(paste0("AGECHDC_", 1:8), paste0("AGECHDIED_", 1:8),
          paste0("AGECHDLEFT_HOM_", c(1:6, 8)), "AGECHDLEFT_HOMC_7")),
      list(AGE_SEP_MA1C = c(999.5, 999.9),
           AGE_SEP_MA2C = c(999.5, 999.9))
    ))),

  # 2012 (cycle 26): monolithic SPSS; data in Data Files ASCII/.
  # 27 count/age variables have boundary labels (e.g. "100 hours or more",
  # "85 years or older") alongside unlabeled numeric values — force_numeric
  # prevents valid data from being silently NAs.
  "GSS/2012" = .make_entry("GSS", "2012",
    file_mask   = "GSS26PUMFM\\.DAT",
    data_fixups = list(force_numeric = c(
      "AGEHSDYC", "NLC_Q100C", "HAR_Q10C",  "NPA_Q10C",
      "HAP_Q10C", "PRN_Q30C",  "DOA_Q30C",
      "AGE_CGI_01_BEGC", "AGE_CGI_01_ENDC",
      "AGE_CGI_02_BEGC", "AGE_CGI_02_ENDC",
      "AGE_CGI_03_BEGC", "AGE_CGI_03_ENDC",
      "AGE_CGI_04_BEGC", "AGE_CGI_04_ENDC",
      "AGE_CGI_05_BEGC", "AGE_CGI_05_ENDC",
      "WHW_Q120C", "WHW_Q130C", "WHW_Q140C",
      "ITL_Q10",   "ITL_Q40C",  "ITA_Q10",
      "IPE_Q10GR", "IPO_Q10GR", "IPO_Q20GR", "PHS_Q10"
    ))),

  # 2018 (cycle 32): split-SPSS layouts in Data_Donnees/Layouts/SPSS/.
  # An Addendum_04-2024.txt file at the root would be mistaken for a data file
  # without an explicit file_mask.
  # AGEHSDYC ("age of youngest household member") has one top-coded boundary
  # label (85 = "85 years and over") alongside unlabeled ages 0-84; force_numeric
  # prevents those 85 valid age values from being NAs.
  "GSS/2018" = .make_entry("GSS", "2018",
    file_mask    = "C32PUMFM\\.txt",
    data_fixups  = list(
      # Top-coded / boundary-labeled variables: only one labeled value exists
      # (e.g. 85="85 years and over", 0="No hours") alongside unlabeled numeric
      # data values.  force_numeric prevents those values from becoming NA.
      force_numeric = c(
        "AGEHSDYC", "DPA_10",  "PAR_10",  "DVCG120C",
        "RNA_10C",  "RNA_20C", "RNA_30C", "RNA_40C",
        "DNA_10C",  "DNA_15C", "DVDNA20C","DNA_31C",
        "DNA_32C",  "DNA_33C", "DNA_34C", "ITA_10C",
        "IPA_21C",  "IPA_22C", "IPA_23C", "PHS_10C"
      )
    )),

  # ---- CCAHS: Canadian COVID-19 Antibody and Health Survey ------------------
  # Split-SPSS layout (CCAHS_PUMF_{i,vale,vare,valf,varf,miss}.sps).
  # Both a CSV and a TXT data file are shipped; use the CSV to avoid ambiguity.
  # BSW file is ccahs_bsw_pumf.csv; WGT_PUMF appears in both main and BSW so
  # it is dropped from BSW before joining.
  "CCAHS/1" = .make_entry("CCAHS", "1",
    layout_mask   = "CCAHS_PUMF",
    bsw_mask      = "bsw",
    bsw_file_mask = "ccahs_bsw_pumf\\.csv",
    bsw_join_key  = "PUMFID",
    bsw_drop_cols = "WGT_PUMF",
    file_mask     = "ccahs_pumf\\.csv"),

  # ---- SGVP: GSS Giving, Volunteering and Participating ---------------------
  "SGVP/2023" = .make_entry("SGVP", "2023",
    file_mask   = "GVP_DBP_2023_PUMF_FMGD\\.txt",
    data_fixups = list(force_numeric = "HSDSIZEC")),

  # 2018 (cycle 33): split-SPSS in Syntax_Syntaxe/SPSS/.
  # GSS33PUMF_label.txt is a GTAB file, not the data; explicit file_mask needed.
  # layout_mask selects the split SPSS files (GSS33PUMF_vare/vale/etc.).
  # DSCORE is a continuous donor-propensity score with no VALUE LABELS.
  # BRTHMACR code 09 (140 rows) appears in the PDF data dictionary with a
  # blank label (both EN and FR); suppress via NA codes_supplement.
  "SGVP/2018" = .make_entry("SGVP", "2018",
    layout_mask = "GSS33PUMF",
    file_mask   = "GSS33PUMF\\.txt$",
    data_fixups = list(
      force_numeric    = c("HSDSIZEC", "DSCORE"),
      codes_supplement = list(
        BRTHMACR = data.frame(val = "9", label_en = NA_character_,
                              label_fr = NA_character_,
                              stringsAsFactors = FALSE)
      )
    )),

  # 2013 (cycle 27): monolithic SPSS (GSSC27GVPpumf_e.sps + French pair).
  "SGVP/2013" = .make_entry("SGVP", "2013",
    file_mask   = "GVP_PUMF_MAIN\\.txt",
    data_fixups = list(force_numeric = c("HSDSIZEC", "CHH0014C"))),

  # 2010 (cycle 22): monolithic SPSS; MAIN file only (GS subset excluded).
  # layout_mask filters to the MAIN SPSS so the GIVING subset SPSS is ignored.
  "SGVP/2010" = .make_entry("SGVP", "2010",
    layout_mask = "_MAIN_",
    file_mask   = "CSGVP2010_MAIN_PUMF\\.txt$"),

  # 2007 (cycle 17): same layout as 2010.
  "SGVP/2007" = .make_entry("SGVP", "2007",
    layout_mask = "_MAIN_",
    file_mask   = "CSGVP2007_MAIN_PUMF\\.txt$"),

  # 2004 (cycle 13): same layout; readme/lisezmoi.txt files excluded via mask.
  "SGVP/2004" = .make_entry("SGVP", "2004",
    layout_mask = "_MAIN_",
    file_mask   = "CSGVP2004_MAIN_PUMF\\.txt$"),

  # 2000 (cycle 9): MAIN file; Readme/Lisezmoi excluded via mask.
  "SGVP/2000" = .make_entry("SGVP", "2000",
    layout_mask = "_MAIN_",
    file_mask   = "NSGVP2000_MAIN_PUMF\\.txt"),

  # 1997 (cycle 4): three separate PUMF files (SGVP = combined main file).
  # SAS text files also present; layout_mask and file_mask select the SGVP main.
  # AQ03 (org-type count) has code 0="0" (numeric string, not a sentinel phrase)
  # alongside unlabeled values 1–15; force_numeric preserves those counts.
  # The fixed-width data uses SAS-style "." for missing values; na_values
  # turns them into NA in labeled columns without unmatched-value warnings.
  "SGVP/1997" = .make_entry("SGVP", "1997",
    layout_mask = "_SGVP_",
    file_mask   = "NSGVP1997_SGVP_PUMF\\.txt",
    data_fixups = list(force_numeric = "AQ03", na_values = ".")),

  # ---- ITS: International Travel Survey -------------------------------------
  # Split-SPSS layout (VTS_<year>_PUMF_{i,vale,vare,valf,varf,miss}.sps) in
  # Layout_Cards/.  Explicit file_mask avoids picking up the README.txt.
  "ITS/2018" = .make_entry("ITS", "2018",
    file_mask = "VTS_2018_PUMF\\.txt"),
  "ITS/2019" = .make_entry("ITS", "2019",
    file_mask = "VTS_2019_PUMF\\.txt"),

  # ---- Census of Population -------------------------------------------------
  # 2021 and 2016 are downloadable. Older years are EFT-only (user deposits zip).
  # All Census files use CP1252-encoded data; 2021 uses UTF-8 metadata (command
  # files shipped as UTF-8), older years use CP1252 metadata.
  #
  # Income variables (8-char-wide fields: CHDBN, COVID_ERB, CQPPB, CapGn, ChldC,
  # EICBN, EmpIn, GovtI, GTRfs, IncTax, Invst, MrkInc, OASGI, OtInc, Retir,
  # SempI, TotInc, TotInc_AT, Value, Wages, …) use sentinel codes not declared in
  # any machine-readable command file; handled via the explicit
  # .census_fixup_8 (2016/2021) and .census_fixup_7 (1991-2011) variants.
  # The former .census_fixup alias was removed after it silently pointed the
  # 2016/2021 entries at the 7-char values.

  # 2021: CSV data, UTF-8 SPSS command files. Older releases named the religion
  # variable RELIGION_DER; newer releases fixed this to RELIG. The rename fixup
  # is applied only when the old column exists (safe for both release variants).
  "Census/2021 (individuals)" = .make_entry("Census", "2021 (individuals)",
    metadata_encoding = "UTF-8",
    file_mask         = "\\.csv",
    data_fixups       = c(.census_fixup_8, list(rename = c(RELIGION_DER = "RELIG")))),

  "Census/2021 (hierarchical)" = .make_entry("Census", "2021 (hierarchical)",
    metadata_encoding = "UTF-8",
    file_mask         = "\\.csv",
    data_fixups       = .census_fixup_8),

  # 2016: fixed-width .dat file, CP1252 metadata
  "Census/2016 (individuals)" = .make_entry("Census", "2016 (individuals)",
    file_mask   = "\\.dat",
    data_fixups = .census_fixup_8),

  "Census/2016 (hierarchical)" = .make_entry("Census", "2016 (hierarchical)",
    file_mask   = "\\.dat",
    data_fixups = .census_fixup_8),

  # 2011 NHS: fixed-width .dat, 7-char income fields
  "Census/2011 (individuals)" = .make_entry("Census", "2011 (individuals)",
    file_mask   = "\\.dat",
    data_fixups = .census_fixup_7),

  "Census/2011 (hierarchical)" = .make_entry("Census", "2011 (hierarchical)",
    file_mask   = "\\.dat",
    data_fixups = .census_fixup_7),

  # 2006: fixed-width .dat, 7-char income fields
  "Census/2006 (individuals)" = .make_entry("Census", "2006 (individuals)",
    file_mask   = "\\.dat",
    data_fixups = .census_fixup_7),

  # MORGH code 8 is absent from the SPSS labels; the PDF user guide documents
  # it as "Not available" / "Non disponible" (freq 9,353).
  "Census/2006 (hierarchical)" = .make_entry("Census", "2006 (hierarchical)",
    file_mask   = "\\.dat",
    data_fixups = c(.census_fixup_7, list(
      codes_supplement = list(
        MORGH = data.frame(val = "8", label_en = "Not available",
                           label_fr = "Non disponible", stringsAsFactors = FALSE)
      )
    ))),

  # 2001: fixed-width .dat; three file types
  "Census/2001 (individuals)" = .make_entry("Census", "2001 (individuals)",
    file_mask   = "\\.dat",
    data_fixups = .census_fixup_7),

  "Census/2001 (households)" = .make_entry("Census", "2001 (households)",
    file_mask   = "\\.dat",
    data_fixups = .census_fixup_7),

  "Census/2001 (families)" = .make_entry("Census", "2001 (families)",
    file_mask   = "\\.dat",
    data_fixups = c(.census_fixup_7, list(
      codes_supplement = list(
        MODEF = data.frame(val="7", label_en="Other method",
                           label_fr="Autre moyen", stringsAsFactors=FALSE)
      )
    ))),

  # 1996: separate EFT archive per type.  Each outer zip contains a
  # second-level zip (indiv.zip, hhldv2.zip, famv2.zip) alongside a small test
  # file; pumf_locate_or_download auto-extracts inner zips.  file_mask avoids
  # matching the test file.
  "Census/1996 (individuals)" = .make_entry("Census", "1996 (individuals)",
    file_mask   = "^indiv\\.dat$",
    data_fixups = .census_fixup_7),

  "Census/1996 (households)" = .make_entry("Census", "1996 (households)",
    file_mask   = "^hhldv2\\.dat$",
    data_fixups = .census_fixup_7),

  "Census/1996 (families)" = .make_entry("Census", "1996 (families)",
    file_mask   = "^fam\\.dat$",
    data_fixups = .census_fixup_7),

  # 1991: separate EFT archive per type (individuals / households / families).
  # Each type must be deposited in its own version directory.  The downloaded
  # XMF files are French-only (CP850); English XMFs are bundled in
  # inst/extdata/census_1991/ so both label_en and label_fr are populated.
  # NOLGREP code 9 ("Sans objet") is absent from both XMF files.
  "Census/1991 (individuals)" = .make_entry("Census", "1991 (individuals)",
    file_mask         = "PUMF91\\.INDIV",
    metadata_encoding = "CP850",
    bundled_eng_sps   = "census_1991/IND91.XMF",
    data_fixups       = c(.census_fixup_7, list(
      codes_supplement = list(
        NOLGREP = data.frame(val = "9", label_en = "Not applicable",
                             label_fr = "Sans objet", stringsAsFactors = FALSE)
      )
    ))),

  "Census/1991 (households)" = .make_entry("Census", "1991 (households)",
    file_mask         = "PUMF91\\.HHLD",
    metadata_encoding = "CP850",
    bundled_eng_sps   = "census_1991/HHOLD91.XMF",
    data_fixups       = .census_fixup_7),

  "Census/1991 (families)" = .make_entry("Census", "1991 (families)",
    file_mask         = "PUMF91\\.FAM",
    metadata_encoding = "CP850",
    bundled_eng_sps   = "census_1991/CNCF91.XMF",
    data_fixups       = .census_fixup_7),

  # 1986–1971: single EFT bundle per year containing all file types.
  # Deposit the bundle zip in Census/<year>/; each type reads raw files from
  # that parent directory and writes its own metadata + DuckDB into the type
  # subdirectory (Census/<year>/<type>/).
  #
  # bundle_sps_mask selects the correct per-type SPSS files from the shared dir.
  #
  # 1986: SPSS files ind86_eng/fre.sps, hhld86_eng/fre.sps, c1986fam/r1986fam.sps
  #        data files INDIV86.DAT, HHLD86.DAT, FAM86.DAT
  # 1981: SPSS files ind81_eng/fre.sps (individuals), hhmdf81_eng/fre.sps (households)
  #        data files INDMDF81.DAT, HHMDF81.DAT  (no families file in bundle)
  # 1976: SPSS files indiv76_eng/fre.sps, hhld76eng/fre.sps, fam76eng/fre.sps
  #        data files indiv76.txt, hhld76.txt, fam76.txt (extracted from inner zips)
  # 1971: SPSS files indiv71_cma_eng/fre.sps, indiv71_prov_eng/fre.sps,
  #                   hhld71_cma_eng/fre.sps,  hhld71_prov_eng/fre.sps,
  #                   fam71_cma_eng/fre.sps,   fam71_prov_eng/fre.sps
  #        data files Indiv71_cma.txt/indiv71_prov.txt, hhld71_cma.txt/hhld71_prov.txt,
  #                   fam71_cma.txt/fam71_prov.txt  (extracted from inner zips)
  # 1986/1981: the 7-char income sentinels (.census_fixup_7) are not documented
  # in the guides and a field-aligned scan shows they never occur in the data,
  # so no na_values guard is applied for these years.
  #
  # 1986: many continuous variables (ages, hours/weeks worked, dollar amounts)
  # carry only boundary labels in the SPSS ("<$20,000", "85 yrs or more",
  # "100 hours or more") alongside unlabeled continuous values; force_numeric
  # keeps the continuous values.  Per-variable sentinels (VALUEH 999999,
  # GROSRTH/OMPH/MPPIT 9999, RENTH 999, SPAGE 0, SPWKSWK/WKSWK 99, HRSWK 999)
  # are declared in the SPS MISSING VALUES section and become NA via the
  # parsed missing_low/missing_high range.  The family SPS has an empty
  # Missing Values section, so VALUEC's 999999 sentinel (present in no other
  # field per an aligned data scan) is dropped via na_values instead.
  # ETHNICOR codes 29/30 come from the reduced legend used for the Atlantic
  # provinces, Yukon and NWT (PDF codebook p.70); they are absent from both
  # SPS files.  French labels follow the style of the SPS codes 14/19.
  "Census/1986/individuals" = .make_entry("Census", "1986/individuals",
    bundle_sps_mask = "ind86",
    file_mask       = "^INDIV86\\.DAT$",
    data_fixups     = list(
      force_numeric = c(
        "AGEP", "HRSWK", "WKSWK",
        "TOTINCP", "WAGESP", "SELFIP", "INVSTP", "RETIRP", "OTINCP"
      ),
      codes_supplement = list(
        ETHNICOR = data.frame(
          val      = c("29", "30"),
          label_en = c("Other European single responses (Atl/YT/NWT)",
                       "Asian (Atl/YT/NWT)"),
          label_fr = c("Autres origines uniques européennes (Atl/YN/TNO)",
                       "Asiatique (Atl/YN/TNO)"),
          stringsAsFactors = FALSE
        )
      )
    )),

  "Census/1986/households" = .make_entry("Census", "1986/households",
    bundle_sps_mask = "hhld86",
    file_mask       = "^HHLD86\\.DAT$",
    data_fixups     = list(force_numeric = c(
      "VALUEH", "GROSRTH", "RENTH", "OMPH", "MPPIT",
      "HMAGE", "HMWKSWK", "HMTOTINC", "SPAGE", "SPWKSWK", "SPTOTINC"
    ))),

  "Census/1986/families" = .make_entry("Census", "1986/families",
    bundle_sps_mask = "fam",
    file_mask       = "^FAM86\\.DAT$",
    data_fixups     = list(na_values = "999999")),

  "Census/1981/individuals" = .make_entry("Census", "1981/individuals",
    bundle_sps_mask = "ind81",
    file_mask       = "^INDMDF81\\.DAT$",
    data_fixups     = list(
      # In the PDF record layout the mnemonics use FA*=father/husband and
      # MA*=mother/wife (e.g. WKACTFA at 162-163 is the husband's 10-code work
      # activity; WKACTMA at 164-165 the wife's 12-code FT/PT scheme).  The SPS
      # DATA LIST matches the PDF positions, but the SPS VARIABLE LABELS and
      # VALUE LABELS were written assuming MA=male/FA=female, i.e. they are
      # transposed relative to both the DATA LIST and the PDF.  Swapping the
      # data column names keeps the intuitive MA=male reading and lets the SPS
      # labels fall on the correct data columns (verified against the PDF code
      # schemes: SPS MAOCC81 carries the husband's 0-17 occupation codes that
      # the PDF documents at positions 158-159).  Note: as a result canpumf's
      # WKACTMA/FAOCC81/FALFACT etc. are the PDF's WKACTFA/MAOCC81/MALFACT.
      cols_swap = c(WKACTMA = "WKACTFA", FAOCC81 = "MAOCC81", FALFACT = "MALFACT")
    )),

  "Census/1981/households" = .make_entry("Census", "1981/households",
    bundle_sps_mask = "hhmdf81",
    file_mask       = "^HHMDF81\\.DAT$"),

  "Census/1976/individuals" = .make_entry("Census", "1976/individuals",
    bundle_sps_mask = "indiv76",
    file_mask       = "^indiv76\\.txt$"),

  "Census/1976/households" = .make_entry("Census", "1976/households",
    bundle_sps_mask = "hhld76",
    file_mask       = "^hhld76\\.txt$"),

  "Census/1976/families" = .make_entry("Census", "1976/families",
    bundle_sps_mask = "fam76",
    file_mask       = "^fam76\\.txt$"),

  # 1971 has separate CMA (Census Metropolitan Area) and provincial (prov)
  # variants for each file type; both come from the same bundle zip.
  "Census/1971/individuals_prov" = .make_entry("Census", "1971/individuals_prov",
    bundle_sps_mask = "indiv71_prov",
    file_mask       = "^indiv71_prov\\.txt$",
    data_fixups     = .census_fixup_1971),

  "Census/1971/individuals_cma" = .make_entry("Census", "1971/individuals_cma",
    bundle_sps_mask = "indiv71_cma",
    file_mask       = "^indiv71_cma\\.txt$",
    data_fixups     = c(.census_fixup_1971, list(
      # TYPE66/TYPE71: value 0 ("Data not available") is absent from the SPSS
      # VALUE LABELS; confirmed from PDF documentation.
      codes_supplement = list(
        TYPE66 = data.frame(val = "0", label_en = "Data not available",
                            label_fr = "Données non disponibles",
                            stringsAsFactors = FALSE),
        TYPE71 = data.frame(val = "0", label_en = "Data not available",
                            label_fr = "Données non disponibles",
                            stringsAsFactors = FALSE)
      )
    ))),

  "Census/1971/households_prov" = .make_entry("Census", "1971/households_prov",
    bundle_sps_mask = "hhld71_prov",
    file_mask       = "^hhld71_prov\\.txt$",
    data_fixups     = .census_fixup_1971),

  "Census/1971/households_cma" = .make_entry("Census", "1971/households_cma",
    bundle_sps_mask = "hhld71_cma",
    file_mask       = "^hhld71_cma\\.txt$",
    data_fixups     = .census_fixup_1971),

  "Census/1971/families_prov" = .make_entry("Census", "1971/families_prov",
    bundle_sps_mask = "fam71_prov",
    file_mask       = "^fam71_prov\\.txt$",
    data_fixups     = c(.census_fixup_1971, list(
      # CMACODE is always 000 in the provincial file (no CMA detail); the SPSS
      # VALUE LABELS only list 008/021 (Montreal/Toronto from the CMA file), so
      # 000 would otherwise warn as unmatched.
      codes_supplement = list(
        CMACODE = data.frame(val = "000", label_en = NA_character_,
                             label_fr = NA_character_,
                             stringsAsFactors = FALSE)
      )
    ))),

  "Census/1971/families_cma" = .make_entry("Census", "1971/families_cma",
    bundle_sps_mask = "fam71_cma",
    file_mask       = "^fam71_cma\\.txt$",
    data_fixups     = .census_fixup_1971)
)

#' Resolve version aliases
#'
#' Canonicalises user-supplied version strings for Census of Population.
#' Any string starting with a four-digit year is parsed flexibly: the file
#' type is detected by grepping for "hierarchical", "household", or "famil"
#' (defaulting to "individuals"), and CMA vs provincial by grepping for "cma".
#' The registry is then probed to find the correct canonical format for that
#' year (e.g. `"1971/households_cma"`, `"1986/households"`, or
#' `"2001 (households)"`).
#'
#' Examples of accepted inputs (case-insensitive keywords):
#' - `"2021"` → `"2021 (individuals)"`
#' - `"1971"` → `"1971/individuals_prov"`
#' - `"1971 CMA"` → `"1971/individuals_cma"`
#' - `"1971 households CMA"` → `"1971/households_cma"`
#' - `"1986 families"` → `"1986/families"`
#'
#' @param series survey series acronym
#' @param version raw version string supplied by the caller, or `NULL`
#' @return canonical version string (or `NULL` if `version` was `NULL`)
#' @keywords internal
pumf_resolve_version <- function(series, version) {
  if (is.null(version)) return(NULL)
  if (series == "Census" && grepl("^\\d{4}", version)) {
    year <- substr(version, 1L, 4L)

    type <- if (grepl("hierarchical", version, ignore.case = TRUE)) "hierarchical"
            else if (grepl("household",    version, ignore.case = TRUE)) "households"
            else if (grepl("famil",        version, ignore.case = TRUE)) "families"
            else "individuals"

    is_cma <- grepl("cma", version, ignore.case = TRUE)

    if (is_cma) {
      k <- paste0(year, "/", type, "_cma")
      if (!is.null(.pumf_registry[[paste0("Census/", k)]])) return(k)
    }
    k <- paste0(year, "/", type, "_prov")
    if (!is.null(.pumf_registry[[paste0("Census/", k)]])) return(k)
    k <- paste0(year, "/", type)
    if (!is.null(.pumf_registry[[paste0("Census/", k)]])) return(k)
    return(paste0(year, " (", type, ")"))
  }
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
