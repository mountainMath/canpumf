# R/registry.R -- Survey-specific configuration registry.
#
# Each entry captures the non-derivable per-(series, version) configuration
# needed by the three-stage pipeline: layout/BSW masks, file masks, encoding
# overrides, and raw-data fixups applied before label mapping in Stage 3.
#
# Surveys not listed here fall back to auto-detection with no special handling.
# That covers the generic read_pumf_data() path for manually-deposited directories.
#
# data_fixups structure (applied to raw character data before label mapping):
#   str_pad:    list of list(cols, width, side, pad) -- left/right-pad raw values
#   rename:     named character vector c(old_name = "new_name") -- column renames
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
                        bsw_strata        = NULL,
                        file_mask         = NULL,
                        data_encoding     = "CP1252",
                        metadata_encoding = "CP1252",
                        data_fixups       = list(),
                        bundled_eng_sps   = NULL,
                        bundle_source     = NULL,
                        bundle_sps_mask   = NULL,
                        doc_mask          = NULL) {
  list(
    series            = series,
    version           = version,
    layout_mask       = layout_mask,
    bsw_mask          = bsw_mask,
    bsw_file_mask     = bsw_file_mask,
    bsw_join_key      = bsw_join_key,
    bsw_drop_cols     = bsw_drop_cols,
    bsw_strata        = bsw_strata,
    file_mask         = file_mask,
    data_encoding     = data_encoding,
    metadata_encoding = metadata_encoding,
    data_fixups       = data_fixups,
    bundled_eng_sps   = bundled_eng_sps,
    bundle_source     = bundle_source,
    bundle_sps_mask   = bundle_sps_mask,
    doc_mask          = doc_mask
  )
}

# Census of Population income variables use undeclared sentinel codes.
# The sentinel width matches the income field width, which changed across years:
#
#   2016-2021: 8-char income fields (EmpIn, MrkInc, TotInc, Value, ...)
#     99999999 = not applicable  (e.g. persons aged < 15)
#     88888888 = not available
#
#   1991-2011: 7-char income fields (same variables, narrower layout)
#     9999999  = not applicable
#     8888888  = not available
#
# Do NOT combine these into one vector: "9999999" applied to 8-char fields
# would treat a valid $9,999,999 income (stored as " 9999999", trimmed to
# "9999999") as NA.
#
# 1986 and earlier: guides do not document income sentinels and a field-aligned
# data scan finds none -- no na_values applied (see Census 1986 comment below).
# 1991-2001 guides document only 9999999 ("Not applicable"); 8888888 first
# appears in the 2006 documentation.  It is kept in .census_fixup_7 for
# 1991-2001 as a harmless guard: a field-aligned scan shows it never occurs in
# those years' data, and top-coding makes a legitimate $8,888,888 impossible.
# SGVP: HSDSIZEC ("household size", top-coded) and CHH0014C ("children 0-14",
# top-coded) have boundary labels alongside unlabeled lower values;
# force_numeric prevents those values from becoming NAs.  Presence varies by
# cycle: 2013 has both, 2023/2018 only HSDSIZEC (children variables are
# categorical), 2010 and earlier use different names (e.g. DH1GHHSZ) that are
# fully labeled categoricals needing no fixup.

.census_fixup_8 <- list(na_values = c("99999999", "88888888"))
.census_fixup_7 <- list(na_values = c("9999999",  "8888888"))

# 1971: SUBSAMPL is an integer sub-sample index (0 for households, 1-5 for
# individuals/families), but all six SPSS files only declare code 1 -> 'one'.
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
  "CIS/2017" = .make_entry("CIS", "2017", file_mask = "PUMF\\.txt"),

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
  # has undefined code points for bytes like 0x90 (\u00c9 in CP850), causing a
  # segfault in readr.  All 40 numeric variables use "DO NOT KNOW" / "DO NOT
  # KNOW (PROXY ONLY)" rather than the contraction -- handled by .sentinel_pat.
  "GSS/1996" = .make_entry("GSS", "1996",
    file_mask         = "c11mice\\.dat",
    metadata_encoding = "CP850"),

  # 2007 (cycle 21): monolithic SPSS (multiple small VARIABLE/VALUE LABELS
  # blocks, one per module). The SAS cards file provides full 951-variable
  # coverage including PROC FORMAT VALUE blocks parsed by
  # parse_sas_data_labels().  ~97 continuous count/age/amount variables have
  # boundary or group codes in their VALUE LABELS that conflict with raw
  # continuous data (e.g. AGE_CU1C: 15="15 years and less", 80="80 years and
  # more" alongside decimal ages 016.0-079.0) -- force_numeric prevents valid
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
  # "85 years or older") alongside unlabeled numeric values -- force_numeric
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

  # ---- GSS: Non-caregiving themes (Safety, Family, Social Identity, etc.) ---
  # Collection version strings include the theme name (e.g. "Safety 2019").
  # Modern format (2014+): split-SPSS in Layouts_MisEnPages/SPSS/ or
  #   Syntax_Syntaxe/SPSS/; data in Data_DonnÇes/; M=main, I=incident/episode.
  # Mid-era (2001-2013): monolithic SPSS + SAS PROC FORMAT.
  # layout_mask selects main dataset over incident/episode when both exist.
  # WTBS_002..WTBS_500 are present in layout but not labeled — warning is
  # expected and allowed by the test suite.

  # ---- Safety (Canadian Safety) ----------------------------------------------
  # Safety 2019 (cycle 34): split-SPSS; M (main) + I (incident) datasets.
  # NUMEVACT ("Evening activities count") has codes 0="Never" and 96-99 sentinels
  # alongside unlabeled counts 1-95 — force_numeric prevents valid data → NA.
  "GSS/Safety 2019" = .make_entry("GSS", "Safety 2019",
    layout_mask = "GSS34PUMFM",
    file_mask   = "GSS34PUMFM\\.txt",
    data_fixups = list(force_numeric = "NUMEVACT")),

  # Safety 2014 (cycle 28): split-SPSS in Syntax_Syntaxe/Main/ and Incident/.
  # 113 count/cost/incident variables have boundary labels alongside unlabeled
  # continuous data; force_numeric prevents valid data → NA.
  "GSS/Safety 2014" = .make_entry("GSS", "Safety 2014",
    layout_mask = "GSS28PUMFM",
    file_mask   = "GSS28PUMFM\\.txt",
    data_fixups = list(force_numeric = c(
      "WHR_110",  "DWLINC_C", "WHR_120",  "WHR_130",  "WHR_140",  "WHR_150",
      "PCA_10",   "PCA_20",   "PCA_30",   "PCA_40",   "PCA_45A",  "PCA_45B",
      "CIR_090",  "CIR_100",  "CIR_105A", "CIR_105B", "CIR_105D", "CIR_105E",
      "CIR_105F", "CIR_110",  "CIR_120",  "HWT_120",  "HWT_130",  "HWT_140",
      "HWT_150",  "HWT_170",  "SEXATTK",  "SEXTOUCH", "CIR_150C", "CIR_150D",
      "CIR_150E", "CIR_150F", "PHYSATTK", "CIR_150G", "INJURY",   "CIR_170",
      "CIR_173C", "MEDAT",    "CIR_190",  "BED_DAYS", "WHO_120",  "WHO_210",
      "SXOFNDSC", "YNGOFNDC", "OLDOFNDC", "RLOFNDSC", "RLOFNDC",  "PAG_10",
      "SXOFFND",  "RLOFFNDC", "CIR_310",  "HTC_120",  "HTC_155",  "HTC_180",
      "HATECRIM", "HATE_OTH", "CIR_335",  "CIR_336C", "STP_10",   "STP_20A",
      "STP_20B",  "STP_20C",  "STP_20D",  "STP_60F",  "STP_60G",  "PDM_10",
      "PDM_20A",  "PDM_20B",  "PDM_20C",  "PDM_20D",  "PDM_20E",  "VDAMGE_C",
      "TOTCOST",  "OCI_10",   "OCI_20",   "OCC_10",   "OCC_15",   "OCC_20C",
      "OCC_30",   "LOSTDAYC", "RIP_10",   "APT_20",   "AP405060", "RNP_300",
      "RNP_310",  "CIRMAINC", "GIR_10F",  "GIR_10G",  "GIR_10N",  "GIR_10O",
      "GIR_30",   "GIR_40",   "GIR_50",   "INCTRAUM", "CWC_10",   "CWC_20C",
      "NUMINC",   "NUMINCCA", "REP_STAT", "SEXASSLT", "SXASST23", "ROBBRY",
      "A_ROBBRY", "ASSAULT",  "THREAT",   "BREAK",    "A_BREAK",  "MVTHFT",
      "A_MVTHFT", "PT_MVTFT", "THFTPP",   "MSCRIME",  "MSCRIM23",
      "PEO_110",  "PEO_130",  "PEO_180",  "DRR_120",  "DRP_120",
      "NUMVACTC", "AGELSWKC", "WTI_120C"
    ))),

  # Safety 1999 (cycle 13): monolithic SPSS + SAS; M (main) + I (incident).
  # Both files share a directory; layout_mask selects only M (main) SPS files
  # so the incident SPS is not merged in (causing conflicting label warnings).
  # Incident variables (V1, V2… SEXASSLT etc.) were originally derived from the
  # incident SPS; they remain in force_numeric as they have boundary labels in
  # the main SPS too.  96 additional main-questionnaire variables also need
  # force_numeric (age, date, repeat-victimisation count, hours variables).
  "GSS/Safety 1999" = .make_entry("GSS", "Safety 1999",
    layout_mask = "c13micm",
    file_mask   = "C13MICM\\.DAT",
    data_fixups = list(force_numeric = c(
      "V1",      "V1A",     "V2",      "DWELINCC","V5",      "V6",      "V7",
      "V8",      "V8A_C01", "V8A_C02", "V8A_C03", "V9",      "V10",
      "V10A_C01","V10A_C02","V10A_C03","V10A_C04","V10A_C05","V10A_C06",
      "V12",     "V13_C01", "V13_C02", "V13_C03", "V13_C04", "V14",
      "V15_C01", "SEXATTK", "V15_C02", "SEXTOUCH","V15_C03", "V15_C04",
      "V15_C05", "V15_C07", "INJURY",  "V17",     "V17A",    "V17B",
      "MEDAT",   "V19",     "BED_DAYS","V20A",    "V20B",    "V21",
      "SXOFFND", "V23C",    "AGEOFNDC","V25",     "V29",     "RLOFNDSC",
      "V31",     "V32_C01", "V32_C02", "V32_C10", "HATECRIM","V33",
      "V33A",    "V34",     "V34A",    "V35",     "V36_C01", "V36_C02",
      "V36_C03", "V36_C04", "V36_C05", "V36_C06", "V36_C789","V36_C10",
      "V36_C11", "V36_C12", "V37",     "V42_C01", "V42_C03", "V42_C04",
      "V42_C05", "V42_C06", "VALDAMGE","TOTCOST",  "V44",     "V45",
      "V47",     "V47A",    "V48A",    "V50C",    "V51",     "V51A",
      "LOSTDAYS","V52",     "V53",     "V54B",    "V54C",    "V54E",
      "V55_C01", "V55_C03", "V55_C04", "V55_C57", "V55_C08", "V55_C10",
      "V56",     "V57_C02", "V57_C03", "V57_C05", "V57_C06", "V57_C08",
      "V57_C11", "V58A",    "V58D",    "V58G",    "V58H",    "V58I",
      "V58J",    "V58K",    "V58V59",  "V60A",    "V60B",    "V60C",
      "V60D",    "V62",     "V63",     "V64_C01", "V64_C02", "V64_C03",
      "V64_C04", "V64_C05", "V64_C06", "V64_C07", "V64_C08", "V64_C09",
      "V64_C10", "V64_C11", "V64_C12", "V64_C13", "V64_C14", "V64_C15",
      "V64_C16", "V64_C17", "NUMINC",  "REP_STAT","SEXASSLT","ROBBRY",
      "A_ROBBRY","ASSAULT", "BREAK",   "A_BREAK", "MVTHFT",  "A_MVTHFT",
      "THFTPP",  "A_THFTPP","THFTHP",  "A_THFTHP","VANDALSM","MSCRIME",
      "VBSCRNO",
      "AGECHRYC",
      "A20A",    "A20B",    "A20C",    "A20D",    "A20E",    "A20F",
      "A20G",    "A20H",
      "B1A",     "B2A",     "B3A",     "B4AA",    "B4BA",    "B4CA",
      "B6AA",    "B6BA",    "B7A",     "B8AA",    "B8BA",    "B9A",
      "B10A",    "B11AA",   "B11BA",   "B12A",    "B13A",    "B14A",
      "C1YEAR",  "C1MONTH",
      "D13",     "D14YEAR", "D14MONTH",
      "F13",     "F14YEAR", "F14MONTH",
      "H12",     "H13YEAR", "H13MONTH",
      "K16A11",  "K16A12",  "K16A13",  "K16A14",
      "K16A21",  "K16A22",  "K16A23",  "K16A24",
      "K16A31",  "K16A32",  "K16A33",  "K16A34",
      "K16A41",  "K16A42",  "K16A43",  "K16A44",
      "K16A51",  "K16A52",  "K16A53",  "K16A54",
      "K16A61",  "K16A62",  "K16A63",  "K16A64",
      "K16A71",  "K16A72",  "K16A73",  "K16A74",
      "K16A81",  "K16A82",  "K16A83",  "K16A84",
      "K16A91",  "K16A92",  "K16A93",  "K16A94",
      "K16A101", "K16A102", "K16A103", "K16A104",
      "K16A111", "K16A112",
      "L4B",     "L6A",     "L12A",    "L13A",
      "M4B",     "M6A",     "M12A",    "M13A",
      "PR5NDBED",
      "Q13",     "Q20",
      "AGELTWKC","WKWE",    "Q37",     "WKWEHR",  "WKWEHOHR"
    ))),

  # Safety 1993 (cycle 8): monolithic SPSS + SAS; three data files
  # (C8MICRO.DAT = FWF, C8MICROE.TXT, C8MICROF.txt); use the DAT.
  # French SPS uses CP850 (DOS-era); byte 0x90 (É) is undefined in CP1252.
  "GSS/Safety 1993" = .make_entry("GSS", "Safety 1993",
    file_mask         = "C8MICRO\\.DAT",
    metadata_encoding = "CP850",
    data_fixups = list(force_numeric = c(
      "DVB10A",   "DVD5D6VL", "DVWKVOL",  "D11",      "DVD12VOL", "DVD13VOL",
      "DVD14VOL", "DVSETVOL", "DVD12SET", "DVD13SET", "DVD14SET", "DVE28",
      "DVE30",    "DVE33",    "DVF23NGT", "DVF25DAY", "DVF33DAY", "DVF36COL",
      "DVG38COL", "DVG45COL", "DVG51COL", "DVG53DAY"
    ))),

  # ---- Family ----------------------------------------------------------------
  # Family 2017 (cycle 31): split-SPSS in Syntax_Syntaxe/SPSS/; single dataset.
  # The zip ships GSS31PUMF.txt in both root and Data_DonnÇes/ — identical
  # copies; pipeline picks the shallower path automatically.
  # 88 age/family-history variables have boundary labels alongside continuous
  # decimal ages/years — force_numeric prevents valid data → NA.
  "GSS/Family 2017" = .make_entry("GSS", "Family 2017",
    file_mask   = "GSS31PUMF\\.txt",
    data_fixups = list(force_numeric = c(
      "AGEC",     "AGEDC",    "APARSEPC", "APARDIVC", "AMDIEDC",  "AMOTHC",
      "AFDIEDC",  "AFATHC",   "NLFTHOMC", "ALHOMFC",  "ARTHOMFC", "ALFHOMLC",
      "ARTHOMLC", "NSEPEVR",  "ASEPMA0C", "AGEMA0C",  "APRMA0C",  "ACLMA0C",
      "AGEMA1C",  "APRMA1C",  "ACLMA1C",  "ASEPMA1C", "ADIVMA1C", "ADTHMA1C",
      "AGEMA2C",  "APRMA2C",  "ACLMA2C",  "ASEPMA2C", "ADIVMA2C", "ADTHMA2C",
      "AGEMA3C",  "APRMA3C",  "ACU0C",    "APRCU0C",  "ACU1C",    "APRCU1C",
      "ASEPCU1C", "ADTHCU1C", "ACU2C",    "APRCU2C",  "ASEPCU2C", "ACU3C",
      "APRCU3C",  "ASEPCU3C", "ALATC",    "TOTCHDC",  "ACHD_1C",  "ACHD_2C",
      "ACHD_3C",  "ACHD_4C",  "ACHD_5C",  "ACHD_6C",  "ACHD_7C",  "ACHB1C",
      "ACHB2C",   "ACHB3C",   "ACHB4C",   "ACHB5C",   "ACHB6C",   "ACHB7C",
      "ACHJ1C",   "ACHJ2C",   "ACHJ3C",   "ACHJ4C",   "ACHJ5C",   "ACHJ6C",
      "ACHJ7C",   "CHDINFTC", "CHDINPTC", "CHDOUTC",  "ARHCL1C",  "ARHCL2C",
      "ARHCL3C",  "ARHCL4C",  "ARHCL5C",  "ARHCL6C",  "ARHCL7C",  "ARNCL1C",
      "ARNCL2C",  "ARNCL3C",  "ARNCL4C",  "ARNCL5C",  "ARNCL6C",  "ARNCL7C",
      "NGRDCHDC", "AGRNDPAC", "ARSTPWKC", "ACOMPSTC"
    ))),

  # Family 2011 (cycle 25): monolithic SPSS + SAS; single data file.
  # 111 age/family-history variables (same pattern as 2007/2017 Family cycles).
  "GSS/Family 2011" = .make_entry("GSS", "Family 2011",
    file_mask   = "C25PUMF\\.DAT",
    data_fixups = list(force_numeric = c(
      "AGEC",           "AGEDC",          "AGE_RETHOMLC",   "AGE_RETHOMFC",
      "AGE_LFTHOMLC",   "AGE_MA0C",       "AGE_SEP_MA0C",   "AGE_CL_MA0C",
      "AGE_MA1C",       "AGE_CL_MA1C",    "AGE_SEP_MA1C",   "AGE_DIV_MA1C",
      "AGE_MA2C",       "AGE_CL_MA2C",    "AGE_SEP_MA2C",   "AGE_DIV_MA2C",
      "AGE_MA3C",       "AGE_CL_MA3C",    "AGE_SEP_MA3C",   "AGE_DIV_MA3C",
      "AGE_CU0C",       "AGE_CU1C",       "AGE_SEP_CU1C",   "AGE_CU2C",
      "AGE_SEP_CU2C",   "AGE_CU3C",       "AGE_SEP_CU3C",   "AGE_LATC",
      "AGECHD_1C",      "AGECHD_2C",      "AGECHD_3C",      "AGECHD_4C",
      "AGECHD_5C",      "AGECHD_6C",      "AGECHD_7C",      "AGE_CHDBORN_1C",
      "AGE_CHDBORN_2C", "AGE_CHDBORN_3C", "AGE_CHDBORN_4C", "AGE_CHDBORN_5C",
      "AGE_CHDBORN_6C", "AGE_CHDBORN_7C", "AGE_CHDJOIN_HOM_1C", "AGE_CHDJOIN_HOM_2C",
      "AGE_CHDJOIN_HOM_3C", "AGE_CHDJOIN_HOM_4C", "AGE_CHDJOIN_HOM_5C",
      "AGE_CHDJOIN_HOM_6C", "AGE_CHDJOIN_HOM_7C", "AGE_HHC_LHOM_1C",
      "AGE_HHC_LHOM_2C", "AGE_HHC_LHOM_3C", "AGE_HHC_LHOM_4C",
      "AGE_HHC_LHOM_5C", "AGE_HHC_LHOM_6C", "AGE_HHC_LHOM_7C",
      "AGE_NHHC_LHOM_1C","AGE_NHHC_LHOM_2C","AGE_NHHC_LHOM_3C",
      "AGE_NHHC_LHOM_4C","AGE_NHHC_LHOM_5C","AGE_NHHC_LHOM_6C",
      "AGE_NHHC_LHOM_7C","NO_CHRIC",       "RRB_Q110C",      "ORB_Q110C",
      "SBR_Q110C",      "AGE_DSW_Q101C",  "WKWEHR_C",       "AGE_COMPL_STUDIES_C",
      "MAP_Q135C",      "AGE_WK1BEGC",    "AGE_WK1ENDC",    "DUR_WK1C",
      "AGE_WK2BEGC",    "AGE_WK2ENDC",    "DUR_WK2C",       "AGE_WK3BEGC",
      "AGE_WK3ENDC",    "DUR_WK3C",       "AGE_WK4BEGC",    "AGE_WK4ENDC",
      "DUR_WK4C",       "AGE_WK5BEGC",    "AGE_WK5ENDC",    "DUR_WK5C",
      "AGE_RETIRED_C",  "DUR_WKTOT",      "AGE_INT1BEGC",   "AGE_INT1ENDC",
      "DUR_INT1C",      "AGE_INT2BEGC",   "AGE_INT2ENDC",   "DUR_INT2C",
      "AGE_INT3BEGC",   "AGE_INT3ENDC",   "DUR_INT3C",      "AGE_INT4BEGC",
      "AGE_INT4ENDC",   "DUR_INT4C",      "AGE_INT5BEGC",   "NO_MAT_PATC",
      "AGE_MAT_PAT1BEGC","AGE_MAT_PAT1ENDC","DUR_MAT_PAT1C",
      "AGE_MAT_PAT2BEGC","AGE_MAT_PAT2ENDC","DUR_MAT_PAT2C",
      "AGE_MAT_PAT3BEGC","AGE_MAT_PAT3ENDC","DUR_MAT_PAT3C"
    ))),

  # Family 2001 (cycle 15): monolithic SPSS + SAS; M + C + U files; use Main.
  # 80 child/arrangement variables have boundary labels alongside continuous data.
  "GSS/Family 2001" = .make_entry("GSS", "Family 2001",
    file_mask   = "C15PUMFM\\.DAT",
    data_fixups = list(force_numeric = c(
      "CHD_IMPUTED",       "AGECHDC",           "SEXCHD",
      "MSCHD",             "CHDTYPE",           "HHLDSTAT",
      "HHLDCHD",           "PRTCHDC",           "OTHERHHLDPARENT",
      "SEX",               "PRTYPE",            "REA_LIVHOMPT",
      "CHD_LIVARR",        "KM_CHDLIVE",        "AFC_Q110",
      "AFC_Q120",          "AFC_Q135",          "CCR_Q130",
      "SCC_Q210",          "SCC_Q215_C01",      "SCC_Q215_C02",
      "SCC_Q215_C03",      "SCC_Q215_C04",      "SCC_Q231",
      "SCC_Q232",          "SAT_CHDLIVARR",     "REA_DIS_CHDLIVARR",
      "SCC_Q251",          "SCC_Q252",          "CHDBAPAR_ALIVE",
      "OBP_Q120",          "OPC_Q110",          "OPF_Q110",
      "OPF_Q115_C01",      "OPF_Q115_C02",      "OPF_Q115_C03",
      "OPF_Q115_C04",      "OPF_Q120",          "OPF_Q130",
      "OPF_Q135",          "OPF_Q140",          "OPF_Q150",
      "OPS_Q130",          "OPS_Q140",          "OPS_Q150",
      "OPS_Q160",          "OPS_Q165",          "TSC_Q110",
      "REA_NHHLD_LFTHOM_C01","REA_NHHLD_LFTHOM_C02","REA_NHHLD_LFTHOM_C03",
      "REA_NHHLD_LFTHOM_C04","REA_NHHLD_LFTHOM_C05","REA_NHHLD_LFTHOM_C06",
      "REA_NHHLD_LFTHOM_C07","REA_NHHLD_LFTHOM_C08","REA_NHHLD_LFTHOM_C09",
      "LHNC_Q610",         "LHNC_Q620",         "LHNC_Q630",
      "LHHC_Q110",         "REA_HHLD_LFTHOM_C01","REA_HHLD_LFTHOM_C02",
      "REA_HHLD_LFTHOM_C03","REA_HHLD_LFTHOM_C04","REA_HHLD_LFTHOM_C05",
      "REA_HHLD_LFTHOM_C06","REA_HHLD_LFTHOM_C07","REA_HHLD_LFTHOM_C08",
      "REA_HHLD_LFTHOM_C09","DC_Q120",           "REA_DC_LFTHOM_C01",
      "REA_DC_LFTHOM_C02", "REA_DC_LFTHOM_C03", "REA_DC_LFTHOM_C04",
      "REA_DC_LFTHOM_C05", "REA_DC_LFTHOM_C06", "REA_DC_LFTHOM_C07",
      "REA_DC_LFTHOM_C08", "REA_DC_LFTHOM_C09"
    ))),

  # Family 1995 (cycle 10): monolithic SPSS + SAS; three files (Main/Child/Union).
  # French SPS files use CP850 (DOS-era); byte 0x90 (É) is undefined in CP1252.
  "GSS/Family 1995" = .make_entry("GSS", "Family 1995",
    file_mask         = "C10micme\\.dat",
    metadata_encoding = "CP850",
    data_fixups = list(force_numeric = c(
      "DVAGECHD", "DVSEXCHD", "MS",       "CHDTYPE",  "HHLDSTAT", "HHLDCHD",
      "PRTCHD",   "DVSEX",    "DVPART",   "D40",      "DVD41",    "D44D58",
      "D45A",     "DVD45",    "DVD46",    "D47",      "DVD49",    "D50",
      "D51",      "DVD52",    "D53D76",   "DVD54",    "D55",      "DVD56",
      "DVPARENT", "D67",      "DVD68",    "DVD69",    "DVD70",    "DVD72",
      "D73",      "D74",      "DVD75",    "D76",      "D78",      "DVD79",
      "D80",      "DVD84",    "D86C01",   "D86C02",   "D86C04",   "D86C05",
      "D86C06",   "D86C07",   "D86C08",   "D86C10",   "D87",      "D88",
      "D89",      "D90",      "DVD91",    "D93C01",   "D93C02",   "D93C05",
      "D93C06",   "D93C07",   "D93C08",   "DVD94",    "D98",      "DVD99",
      "D101C01",  "D101C02",  "D101C03",  "D101C06",  "D101C07",  "D101C08"
    ))),

  # ---- Social Identity -------------------------------------------------------
  # Social Identity 2020 (cycle 35): split-SPSS; single main dataset.
  # No force_numeric needed — all categorical variables are properly coded.
  "GSS/Social Identity 2020" = .make_entry("GSS", "Social Identity 2020",
    layout_mask = "GSS35PUMFM",
    file_mask   = "GSS35PUMFM\\.txt"),

  # Social Identity 2013 (cycle 27): monolithic SPSS; single file.
  # 25 count/age/household-size variables have boundary labels alongside
  # unlabeled continuous values.
  "GSS/Social Identity 2013" = .make_entry("GSS", "Social Identity 2013",
    data_fixups = list(force_numeric = c(
      "RECID",    "HSDSIZEC", "AGEHSDYC", "CHINHSDC", "CHH0014C", "RFE_10C",
      "RFE_20C",  "SCF_100C", "SCF_102C", "SCF_110C", "CWF_20C",  "SCP_110",
      "SCP_120C", "CERD230C", "GRP_10C",  "GRP_20C",  "IWO_10C",  "MCR_300C",
      "MCR_325C", "MCR_330C", "MCR_335C", "WET_110",  "AGELPDWC", "WHW_210",
      "HSDELIGC"
    ))),

  # Social Identity 2003 (cycle 17): monolithic SPSS + SAS; single main file.
  # 242 variables have boundary labels alongside unlabeled continuous values.
  "GSS/Social Identity 2003" = .make_entry("GSS", "Social Identity 2003",
    file_mask   = "C17PUMFM\\.DAT",
    data_fixups = list(force_numeric = c(
      "AGEGR5",    "AGEGR10",   "SEX",       "MARSTAT",   "AGEPRGRDIF",
      "PRTYPEC",   "AGECHRYC",  "CHRFLAG",   "PARHSDC",   "LIVARR08",
      "LIVARR12",  "FAMTYPE",   "MULTIGEN",  "PRV",       "REGION",
      "LUC_RST",   "HAL_Q110",  "HAL_Q120",  "HAL_Q150",  "HAL_Q160",
      "HAL_Q170",  "HAL_Q210",  "MSS_Q110",  "MSS_Q120",  "HS_Q110",
      "LS_Q110",   "LS_Q120",   "LS_Q130",   "LS_Q140",   "LS_Q210",
      "LS_Q310",   "LS_Q320",   "LS_Q330",   "LANCH",     "LANCHSUE",
      "LANCHSUF",  "LANCHSUO",  "LANHSDC",   "NET_Q110",  "NET_Q120",
      "NET_Q130",  "YER_Q110",  "YER_Q120",  "YER_Q130",  "YER_Q150",
      "YER_Q170",  "YER_Q180",  "YER_Q190",  "YER_Q210",  "SCR_Q120",
      "SCR_Q130",  "SCR_Q140",  "SCR_Q810",  "SCF_Q100",  "SCF_Q110",
      "NO_OFRNDS", "SCF_Q120",  "SCF_Q130",  "SCF_Q140",  "SCG_Q120",
      "SCG_Q130",  "SCG_Q150",  "SCG_Q160",  "SCG_Q170",  "SCG_Q180",
      "SCP_Q110",  "HICR_Q110_WO","HICR_Q110_TR","HICR_Q110_CH","HICR_Q110_TE",
      "HICR_Q110_EM","HICR_Q110_OT","HICR_Q110_NO","HICR_Q120","HICR_Q140_RE",
      "HICR_Q140_FR","HICR_Q140_NE","HICR_Q140_PE","HICR_Q150","HICG_Q110_WO",
      "HICG_Q110_TR","HICG_Q110_CH","HICG_Q110_TE","HICG_Q110_EM","HICG_Q110_OT",
      "HICG_Q110_NO","HICG_Q120", "HICG_Q140_RE","HICG_Q140_FR","HICG_Q140_NE",
      "HICG_Q140_PE","HICG_Q150", "VCG_Q300",  "VCG_Q310",  "VCG_Q340",
      "CE_Q110",   "CE_Q111",   "CE_Q112",   "CE_Q113",   "CE_Q114",
      "CE_Q115",   "CE_Q116",   "CE_Q240",   "CE_Q330",   "CE_Q340",
      "OMA_Q110",  "OMA_Q115",  "OMA_Q120",  "OMA_Q130",  "OMA_Q140",
      "OMA_Q150",  "OMA_Q160",  "OMA_Q170",  "OMA_Q210",  "PE_Q110",
      "PE_Q120",   "PE_Q130",   "PE_Q220",   "PE_Q230",   "PE_Q250",
      "PE_Q260",   "PE_Q270",   "PE_Q280",   "PE_Q290",   "PE_Q300",
      "PE_Q310",   "PE_Q320_NEWS","PE_Q320_MAGS","PE_Q320_TELEV","PE_Q320_RADIO",
      "PE_Q320_NET","PE_Q330",  "ACMYR",     "EDUSTAT",   "MAR_Q125",
      "MAR_Q130",  "AGE_LSTPDWKC","MAR_Q150","MAR_Q160",  "MAR_Q161",
      "WKWEHR",    "MAR_Q190",  "WKWEHOHR",  "NAICS16",   "SOC91C10",
      "MAR_Q314",  "MAR_Q315",  "MAR_Q410",  "MAR_Q480",  "MAR_Q485_REL",
      "MAR_Q485_FRND","MAR_Q485_NEIG","MAR_Q485_PERS","MAR_Q485_WORK","MAR_Q510",
      "MAR_Q520_FAMILY","MAR_Q520_JOB","MAR_Q520_ACT","MAR_Q520_EMPLY",
      "MAR_Q520_EMPREL","MAR_Q520_HEALTH","MAR_Q520_FAMREL","MAR_Q520_OTHER",
      "MAR_Q650",  "EDUYR",     "EOR_Q110",  "EOR_Q150",  "EDU5",
      "EDU10",     "EDUPR5",    "EDUPR10",   "EDUM5",     "EDUM10",
      "EDUF5",     "EDUF10",    "ACMPRYR",   "MAP_Q20",   "MAP_Q30",
      "MAP_Q32",   "MAP_Q40",   "DWELC",     "DWELLOWN",  "DOR_Q210",
      "DOR_Q222",  "DOR_Q227",  "DOR_Q228",  "DOR_Q229",  "DOR_Q230",
      "DOR_Q231",  "SOR_Q110",  "SOR_Q120",  "SOR_Q130",  "BRTHCAN",
      "BRTHPRVC",  "BRTHREGC",  "YRARRI",    "AGEARRIGRC","BRTHMCAN",
      "BRTHMREGC", "BRTHFCAN",  "BRTHFREGC", "TRT_Q110",  "TRT_Q310",
      "TRT_Q330",  "TRT_Q390",  "TRT_Q400",  "TRT_Q420",  "TRT_Q540",
      "TRT_Q570",  "TRT_Q610",  "TRT_Q630",  "TRT_Q640",  "TRT_Q650",
      "TRT_Q660",  "TRT_Q670",  "TRT_Q680",  "TRT_Q690",  "TRT_Q700",
      "VOR_Q110",  "VOR_Q120",  "DBT_Q320",  "DBT_Q330",  "DBT_Q340",
      "RELIG6",    "RL_Q105",   "RELIGATT",  "RL_Q130",   "IN_Q0021",
      "IN_Q0022",  "IN_Q0023",  "IN_Q0024",  "IN_Q0025",  "IN_Q0026",
      "IN_Q0027",  "IN_Q0028",  "IN_Q0029",  "IN_Q0030",  "IN_Q0031",
      "IN_Q0032",  "IN_Q0050",  "INCM",      "INCMHSD"
    ))),

  # ---- Education (GSS) -------------------------------------------------------
  # Education 2007 (cycle 21 = same data as registered GSS/2007 Caregiving).
  # Listed separately in the cat9 catalog directory with the same zip file.
  # force_numeric list mirrors GSS/2007 (the same cycle 21 survey data).
  "GSS/Education 2007" = .make_entry("GSS", "Education 2007",
    file_mask   = "C21PUMFM\\.DAT",
    data_fixups = list(force_numeric = c(
      "NO_GRNDCHDC",  "MAR_Q101C",    "AGE_LSTPDWKC", "MAR_Q174C",
      "WKWEHOHR_C",   "AGE_STARTWK_L12MTHC", "MAR_Q370", "MAR_Q470",
      "CCW_Q165C",    "RPR_Q100",     "SIP_Q120",     "CTC_Q165C",
      "CAR_Q140C",    "CAR_Q320C",    "AGE_CRP_BEGC", "ICG_Q140C",
      "ICG_Q150C",    "AGE_CGP_Q110C","AGE_CGP_Q115C","AGE_CGP_BEGC",
      "AGE_CGI_01_BEGC","AGE_CGI_01_ENDC","AGE_CGI_02_BEGC","AGE_CGI_02_ENDC",
      "AGE_CGI_03_BEGC","AGE_CGI_03_ENDC","AGE_CGI_04_BEGC","AGE_CGI_04_ENDC",
      "AGE_CGI_05_BEGC","AGE_CGI_05_ENDC","AGE_HLE_BEG_C","AGE_HLE_END_C",
      "AGE_HLE_DISC_C","AGE_EAH_BEG_C","AGE_EAH_END_C","AGE_EAH_DISC_C",
      "AGE_SMK_DAILY_BEGC","AGE_SMK_OCCA_BEGC","YEARSMKDAILY","YEARSMKOCCASION",
      "MPT_Q090C",    "TLE_Q120C",    "TLE_Q130C",    "TLE_Q230C",
      "MAR_Q383C",    "MAR_Q482C",    "MAR_Q483C",    "AGE_CGP_ENDC",
      "AGE_MA2C",     "AGE_MA3C",     "AGE_SEP_MA1C", "AGE_SEP_MA2C",
      "AGE_DIV_MA2C", "AGE_CU1C",     "AGE_CU2C",     "AGE_CU3C",
      "AGE_CU4C",     "AGE_CU5C",     "AGE_SEP_CU1C", "AGE_SEP_CU2C",
      "AGE_SEP_CU3C", "AGE_SEP_CU4C", "AGE_DTH_CU2C", "AGECHDC_1",
      "AGECHDC_2",    "AGECHDC_3",    "AGECHDC_4",    "AGECHDC_5",
      "AGECHDC_6",    "AGECHDC_7",    "AGECHDC_8",    "AGECHDIED_1",
      "AGECHDIED_2",  "AGECHDIED_3",  "AGECHDIED_4",  "AGECHDIED_5",
      "AGECHDIED_6",  "AGECHDIED_7",  "AGECHDIED_8",  "AGECHDJOIN_HOMC_1",
      "AGECHDJOIN_HOMC_2","AGECHDJOIN_HOMC_3","AGECHDJOIN_HOMC_4",
      "AGECHDJOIN_HOMC_5","AGECHDJOIN_HOMC_6","AGECHDJOIN_HOMC_7",
      "AGECHDJOIN_HOMC_8","AGECHDLEFT_HOM_1","AGECHDLEFT_HOM_2",
      "AGECHDLEFT_HOM_3","AGECHDLEFT_HOM_4","AGECHDLEFT_HOM_5",
      "AGECHDLEFT_HOM_6","AGECHDLEFT_HOMC_7","AGECHDLEFT_HOM_8",
      "WKWEHR_C",     "MAP_Q135C"
    ))),

  # Education 2002 (cycle 16): monolithic SPSS + SAS; MAIN + CG4/CG6/CR files;
  # use the MAIN. 111 count/education variables need force_numeric.
  "GSS/Education 2002" = .make_entry("GSS", "Education 2002",
    file_mask   = "C16PUMF_MAIN\\.DAT",
    data_fixups = list(force_numeric = c(
      "CG4_FR_Q100_C",  "CG4_FR_Q104",    "CG4_FR_Q105",    "CG4_FR_Q107",
      "CG4_FR_Q110",    "CG4_FR_Q115",    "CG4_FR_Q120",    "CG4_FR_Q125",
      "CG4_FR_Q200",    "CG4_FR_Q220",    "CG4_FR_Q230",    "CG4_FR_Q300",
      "CG4_FR_Q301",    "CG4_FR_Q315",    "CG4_FR_Q316",    "CG4_FR_Q325_AST",
      "CG4_FR_Q325_DIE","CG4_FR_Q325_RMO","CG4_FR_Q325_RJB","CG4_FR_Q325_GJB",
      "CG4_FR_Q325_OTH","CG4_FR_Q327_AST","CG4_FR_Q327_DIE","CG4_FR_Q327_GMO",
      "CG4_FR_Q327_RJB","CG4_FR_Q327_GJB","CG4_FR_Q327_OTH","CG4_FR_Q330_11",
      "CG4_FR_Q330_15", "CG4_FR_Q330_16", "CG4_FR_Q330_17", "CG4_FR_Q330_18",
      "CG4_FR_Q330_45_C","CG4_FR_Q330_80","CG4_FR_Q330_81", "CG4_FR_Q330_83",
      "CG4_FR_Q330_85_C","CG4_FR_Q330_86","CG4_FR_Q330_91", "CG4_FR_Q345_11",
      "CG4_FR_Q345_13", "CG4_FR_Q345_14", "CG4_FR_Q345_15", "CG4_FR_Q345_16",
      "CG4_FR_Q345_17", "CG4_FR_Q345_18", "CG4_FR_Q345_34", "CG4_FR_Q345_35",
      "CG4_FR_Q345_45_C","CG4_FR_Q345_80","CG4_FR_Q345_81", "CG4_FR_Q345_83",
      "CG4_FR_Q345_85_C","CG4_FR_Q345_86","CG4_FR_Q345_95", "CG4_FR_Q360",
      "CG4_FR_Q370",    "CG4_FR_Q380",    "CG4_FR_Q381_FAM","CG4_FR_Q381_TIM",
      "CG4_FR_Q381_CLS","CG4_FR_Q381_EXP","CG4_FR_Q381_HEL","CG4_FR_Q381_TRN",
      "CG4_FR_Q381_OTH","CG4_FR_Q400",    "CG4_FR_Q410",    "CG4_FR_Q420",
      "CG4_FR_Q430_11", "CG4_FR_Q430_13", "CG4_FR_Q430_14", "CG4_FR_Q430_17",
      "CG4_FR_Q430_18", "CG4_FR_Q430_45_C","CG4_FR_Q430_80","CG4_FR_Q430_81",
      "CG4_FR_Q430_83", "CG4_FR_Q430_85_C","CG4_FR_Q430_86","CG4_FR_Q500",
      "CG4_FR_Q505",    "CG4_FR_Q511",    "CG4_FR_Q512",    "CG4_FR_Q513",
      "CG4_FR_Q530",    "CG4_FR_Q600",    "CG4_FR_Q605",    "CG4_FR_Q611",
      "CG4_FR_Q612",    "CG4_FR_Q630",    "CG4_FR_Q700",    "CG4_FR_Q705",
      "CG4_FR_Q711",    "CG4_FR_Q712",    "CG4_FR_Q713",    "CG4_FR_Q730",
      "CG4_FR_Q800",    "CG4_FR_Q805",    "CG4_FR_Q830",    "CG4_FR_Q900",
      "CG4_FR_Q910",    "CG4_FR_Q920",    "CG4_FR_Q930",    "CG4_FR_Q940",
      "CG4_VT_Q951_FAM","CG4_VT_Q951_TIM","CG4_VT_Q951_CLS","CG4_VT_Q951_EXP",
      "CG4_VT_Q951_HEL","CG4_VT_Q951_TRN","CG4_VT_Q951_OTH"
    ))),

  # Education 1994 (cycle 9): monolithic SPSS + SAS; single data file.
  "GSS/Education 1994" = .make_entry("GSS", "Education 1994",
    file_mask   = "C9micro\\.dat",
    data_fixups = list(force_numeric = c("DVD7", "DVEXREAG", "L11"))),

  # ---- Time Use --------------------------------------------------------------
  # Time Use 2022: split-SPSS; Main + Episode datasets; no force_numeric needed.
  "GSS/Time Use 2022" = .make_entry("GSS", "Time Use 2022",
    layout_mask = "_Main_",
    file_mask   = "Main-Principal_PUMF\\.txt"),

  # Time Use 2015 (cycle 29): monolithic SPSS; Main + Episode datasets.
  # SPS files are named c29pumf_*.sps (Main) and c29pumfe_*.sps (Episode);
  # layout_mask selects only Main. file_mask selects the Main data file.
  # 16 location/activity-count variables have boundary labels alongside
  # unlabeled continuous values.
  "GSS/Time Use 2015" = .make_entry("GSS", "Time Use 2015",
    layout_mask = "c29pumf_",
    file_mask   = "GSS29PUMFM\\.txt",
    data_fixups = list(force_numeric = c(
      "LOCATION", "TUI_06A", "TUI_06B", "TUI_06C", "TUI_06D", "TUI_06E",
      "TUI_06F",  "TUI_06G", "TUI_06H", "TUI_06I", "TUI_06J", "TUI_03A",
      "TUI_03B",  "TUI_07",  "TECHFLAG","TUI_10",
      "AGEHSDYC", "AGELSWKC", "WLY_170C"
    ))),

  # Time Use 2010 (cycle 24): monolithic SPSS + SAS; data in PUMF/Data_DonnÇes/.
  # No force_numeric needed; WTBS_EPI_002..500 unlabeled bootstrap weights
  # produce a "Variables in layout but not in variable labels" warning.
  "GSS/Time Use 2010" = .make_entry("GSS", "Time Use 2010",
    file_mask = "C24PUMFM\\.DAT"),

  # Time Use 1998 (cycle 12): monolithic SPSS + SAS; M + E files.
  # 17 place/companion episode variables need force_numeric.
  "GSS/Time Use 1998" = .make_entry("GSS", "Time Use 1998",
    file_mask   = "C12MICME\\.DAT",
    data_fixups = list(force_numeric = c(
      "DDAY",    "PLACE",   "ALONE",   "SPOUSE",  "PARHSD",  "MEMBHSD",
      "NHSDCL15","NHSDC15P","NHSDPAR", "OTHFAM",  "FRIENDS", "OTHERS",
      "HELP65",  "HELPLIM", "HELPREL", "ORGCON",  "ENJOYAC"
    ))),

  # ---- Work and Home ---------------------------------------------------------
  # Work and Home 2016 (cycle 30): only a .sas7bdat binary file shipped (no
  # ASCII FWF or SPSS command files) — cannot be read by the current pipeline.
  # No registry entry; get_pumf() will fail with an informative error.

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
  # alongside unlabeled values 1-15; force_numeric preserves those counts.
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
  # SempI, TotInc, TotInc_AT, Value, Wages, ...) use sentinel codes not declared in
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
    data_fixups       = c(.census_fixup_8, list(rename = c(RELIGION_DER = "RELIG"))),
    doc_mask          = "User Guide"),

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

  # 1986-1971: single EFT bundle per year containing all file types.
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
    doc_mask        = "Individu|[Pp]articulier|indvls",
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
          label_fr = c("Autres origines uniques europ\u00e9ennes (Atl/YN/TNO)",
                       "Asiatique (Atl/YN/TNO)"),
          stringsAsFactors = FALSE
        )
      )
    )),

  "Census/1986/households" = .make_entry("Census", "1986/households",
    bundle_sps_mask = "hhld86",
    file_mask       = "^HHLD86\\.DAT$",
    doc_mask        = "Household|[Mm][e\u00e9]nages|hhldhsg",
    data_fixups     = list(force_numeric = c(
      "VALUEH", "GROSRTH", "RENTH", "OMPH", "MPPIT",
      "HMAGE", "HMWKSWK", "HMTOTINC", "SPAGE", "SPWKSWK", "SPTOTINC"
    ))),

  "Census/1986/families" = .make_entry("Census", "1986/families",
    bundle_sps_mask = "fam",
    file_mask       = "^FAM86\\.DAT$",
    doc_mask        = "Family|Familles",
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
                            label_fr = "Donn\u00e9es non disponibles",
                            stringsAsFactors = FALSE),
        TYPE71 = data.frame(val = "0", label_en = "Data not available",
                            label_fr = "Donn\u00e9es non disponibles",
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
#' - `"2021"` -> `"2021 (individuals)"`
#' - `"1971"` -> `"1971/individuals_prov"`
#' - `"1971 CMA"` -> `"1971/individuals_cma"`
#' - `"1971 households CMA"` -> `"1971/households_cma"`
#' - `"1986 families"` -> `"1986/families"`
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
