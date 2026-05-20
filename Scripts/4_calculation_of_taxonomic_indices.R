# ============================================================
# Taxonomic indices calculator (v5)
#
# Changes from v4:
#   - BUG FIX: After Naididae and Tubificidae were removed from the
#     BMWP_lookup table (both are Oligochaeta and
#     should adopt the Oligochaeta subclass score of 1), the bmwp
#     column for rows of those families is now NA. The v4 filter
#     `!is.na(bmwp)` was dropping these rows BEFORE the is_oligo
#     collapse could rewrite their family to "Oligochaeta" and their
#     score to 1. Result: those rows never contributed to BMWP/ASPT.
#     Fix: relax the bmwp filter to `!is.na(bmwp) | is_oligo`, so
#     oligo-flagged rows survive even when their lookup score is NA.
#     The downstream mutate (`bmwp = ifelse(is_oligo, 1, bmwp)`)
#     supplies the score = 1 for Oligochaeta as before. Applied in
#     both the main BMWP block and the diagnostic block.
#
# Changes from v3:
#   - Output now includes site_id alongside site_code and year, so the
#     merge step (script 5) and any downstream metadata joins can use
#     either key without reconstructing site_id from site_code.
#
# Changes from v2:
#   - BMWP: Oligochaeta detection switched from the `oligochaeta`
#     boolean flag column (which v2 left commented out in the
#     aggregation, causing a runtime error) to matching by
#     OTL_taxonname. Names treated as Oligochaeta for BMWP collapse
#     are defined in the constant `BMWP_OLIGOCHAETA_OTL` and currently
#     include "Oligochaeta Gen. sp.", "Naididae Gen. sp.", and
#     "Tubificidae Gen. sp.". This matches the broader convention
#     elsewhere in the project of using OTL_taxonname as the canonical
#     taxonomic identifier.
#
# Changes from v1:
#   - BMWP: removed species-level scoring exceptions
#     (Pontogammarus robustoides, Obesogammarus crassus). Pontogammaridae
#     is now in the BMWP family lookup with its own score (6), so these
#     species roll up to family like the rest.
#   - BMWP: added a GENUS-LEVEL exception for Ancylus. Ancylidae has been
#     synonymised into Planorbidae taxonomically, but Ancylidae and
#     Planorbidae retain distinct BMWP scores (6 vs 3). Ancylus sp. is
#     therefore scored as its own unit at GENUS level, parallel to (and
#     in addition to) any Planorbidae family unit at the same site.
#     Matched by OTL_taxonname == "Ancylus sp.".
#   - BMWP: Oligochaeta collapse unchanged in effect — all rows flagged
#     `oligochaeta == TRUE` collapse to a single "Oligochaeta" unit with
#     a hard-coded score of 1, regardless of family. Naididae/Tubificidae
#     entries in the input BMWP lookup (added there for DSFI) are
#     absorbed into this collapse and do not contribute separate units.
#   - Output path harmonised from "Output data/" to "Outputs/", and the
#     output file name is versioned.
# ============================================================

# libraries
library(vegan)
library(codyn)
library(mobr)
library(reshape2)
library(dplyr)
library(readxl)
library(writexl)

###############################################################################################################

all_lf <- read_excel("Inputs/All_sites_macroinvertebrate_data_long.xlsx",
                      sheet = "clean_macroinvertebrate_data",
                      guess_max = 2500)

# Filter
# Year range 2014-2022: LEPA's pre-2014 EQRs were calculated with the
# old single-DSFI method, not the LRMI. Diagnostic in
# 8_DEP_OTL_vs_validated showed 2013 residuals 4x larger than later
# years, with median residual +0.057 vs +0.010 from 2014 onwards.
# Restricting to the LRMI-era data keeps the comparison fair.
all_lf <- all_lf |>
  filter(
    !grepl("NO MATCH", OTL_taxonname, ignore.case = TRUE),
    !grepl("EXCLUDE", OTL_note_indices, ignore.case = TRUE),
    grepl("^LTR", site_code),
    year >= 2014, year <= 2022
  )

# Check for duplicates (prints any fully-duplicated rows)
all_lf |>
  group_by(across(everything())) |>
  filter(n() > 1) |>
  ungroup()

# ============================================================
# AGGREGATE OTL_TAXONNAME DUPLICATES WITHIN SITE x YEAR
# ============================================================
# Multiple raw_taxonnames can map to the same OTL_taxonname.
# To avoid counting these as separate taxa, duplicates are
# aggregated per site_id x year combination:
#   - Kick sample abundances: retained as-is
#   - Hand-picked sample abundances: set to 1 (presence only)
#     before aggregation, supplementing the kick taxon list
#   - Abundances are then summed across all sampling methods
#     within each site_id x year x OTL_taxonname group
#     (e.g., kick = 15, hand-picked = 10 → aggregated = 16)
#   - BMWP scores are NOT summed; the first value is retained
#     as these are sensitivity scores assigned per family
#
# Compound group flag columns (EPT, EHP, DEP, CrHi, EPC, ECT,
# DET, CDE, CPT, DPT, CDP, CDT, EPTC, EPTD, DEPC, CDET, CDPT,
# EPTCD) are sourced directly from the input file.
#
# Acronym key (compound group names):
#   C = Coleoptera,  D = Diptera,   E = Ephemeroptera,
#   P = Plecoptera,  T = Trichoptera, H = Hemiptera,
#   Cr = Crustacea,  Hi = Hirudinea
# Note: in _minus_Hi_* columns, Hi ALWAYS refers to Hirudinea —
# it is NOT the same as H (Hemiptera) used in the EHP group name.

all_lf_agg <- all_lf |>
  mutate(
    abundance = ifelse(sampling_method == "hand-picked", 1, abundance),
    bmwp      = as.numeric(bmwp)
  ) |>
  group_by(site_id, year, OTL_taxonname) |>
  summarise(
    site_code     = first(site_code),
    abundance     = sum(abundance, na.rm = TRUE),
    order         = first(order),
    family        = first(family),
    bmwp          = first(bmwp),
    ephemeroptera = first(ephemeroptera),
    plecoptera    = first(plecoptera),
    # trichoptera   = first(trichoptera),
    diptera       = first(diptera),
    # odonata       = first(odonata),
    # coleoptera    = first(coleoptera),
    hemiptera     = first(hemiptera),
    # neuroptera    = first(neuroptera),
    # megaloptera   = first(megaloptera),
    crustacea     = first(malacostraca),
    # bivalvia      = first(bivalvia),
    # gastropoda    = first(gastropoda),
    hirudinea     = first(hirudinea),
    # oligochaeta   = first(oligochaeta),
    # helminth      = first(helminth),
    EHP           = first(EHP),
    # EPT           = first(EPT),
    DEP           = first(DEP),
    CrHi          = first(CrHi),
    # EPC           = first(EPC),
    # ECT           = first(ECT),
    # DET           = first(DET),
    # CDE           = first(CDE),
    # CPT           = first(CPT),
    # DPT           = first(DPT),
    # CDP           = first(CDP),
    # CDT           = first(CDT),
    # EPTC          = first(EPTC),
    # EPTD          = first(EPTD),
    # DEPC          = first(DEPC),
    # CDET          = first(CDET),
    # CDPT          = first(CDPT),
    # EPTCD         = first(EPTCD),
    # EPTCBO        = first(EPTCD),
    # EPTCGO        = first(EPTCD),
    .groups = "drop"
  )

# ============================================================
# BMWP: SCORING LEVEL EXCEPTIONS
# ============================================================
# The default BMWP rule is one score per family. Two exceptions:
#
# GENUS-LEVEL: Ancylus is scored as its own unit. Ancylidae has been
#   synonymised into Planorbidae taxonomically, but the two retain
#   distinct BMWP scores. When both Ancylus and other Planorbidae
#   occur at a site, both contribute separately to BMWP/ASPT.
#
# GROUP-LEVEL: Oligochaeta is scored as a single collective unit
#   (score = 1) regardless of how many oligochaete OTUs are present.
#   Detection is by OTL_taxonname (the canonical taxonomic identifier
#   used elsewhere in the project); all listed names collapse to one
#   unit before summation. Naididae/Tubificidae entries (present in
#   the BMWP lookup for DSFI purposes) are absorbed into this collapse.

BMWP_GENUS_LEVEL     <- c("Ancylus sp.")
BMWP_OLIGOCHAETA_OTL <- c("Oligochaeta Gen. sp.",
                          "Naididae Gen. sp.",
                          "Tubificidae Gen. sp.")

# ============================================================
# DIAGNOSTIC FUNCTION — audit index components for one site-year
# ============================================================
# Compares BMWP/ASPT, DEP, and CrHi composition against external
# references (e.g. Lithuanian Environmental Agency templates).
# Reports are printed to the console.
# Usage: diagnose_site_year(all_lf_agg, "LTR1447", 2022)
# Note: 'site' refers to site_id (e.g. "LTR1447"), NOT site_code (e.g. "LTR1447_2022")
#
# NOTE on DEP: the Lithuanian template counts E and P at the
# individual taxon (OTL_taxonname) level, and D at the unique
# family level (e.g. Chironominae + Tanypodinae = 1 Chironomidae).
# Both counts are reported below for comparison.

diagnose_site_year <- function(data, site, yr) {

  sub <- data[data$site_id == site & data$year == yr, ]

  if (nrow(sub) == 0) {
    cat("No data found for site_id '", site, "' in year ", yr, ".\n", sep = "")
    return(invisible(NULL))
  }

  total_abund <- sum(sub$abundance, na.rm = TRUE)

  cat("================================================================\n")
  cat(" DIAGNOSTIC REPORT:", site, "|", yr, "\n")
  cat("================================================================\n")
  cat(" Total OTL taxa  :", nrow(sub), "\n")
  cat(" Total abundance :", total_abund, "\n\n")

  # ---- BMWP / ASPT ------------------------------------------------
  bmwp_all_taxa <- sub |>
    filter(!is.na(bmwp)) |>
    select(OTL_taxonname, order, family, bmwp, abundance) |>
    arrange(family, OTL_taxonname)

  # Genus-level exceptions (Ancylus): scored individually, not rolled up
  # to family. Co-occurring Planorbidae still contributes its own family unit.
  bmwp_genus_diag <- bmwp_all_taxa |>
    filter(OTL_taxonname %in% BMWP_GENUS_LEVEL) |>
    distinct(OTL_taxonname, bmwp)

  # All other taxa: deduplicated at the family level.
  # Oligochaeta OTUs (incl. Naididae/Tubificidae) collapse to one
  # scoring unit named "Oligochaeta" with score = 1.
  bmwp_fam_diag <- sub |>
    mutate(is_oligo = OTL_taxonname %in% BMWP_OLIGOCHAETA_OTL) |>
    # v5: allow NA bmwp through when is_oligo, so Naididae / Tubificidae
    # rows (no longer in BMWP_lookup) survive long enough to be
    # collapsed to "Oligochaeta" with score 1 by the mutate below.
    filter((!is.na(bmwp) | is_oligo),
           !OTL_taxonname %in% BMWP_GENUS_LEVEL,
           !is.na(family) | is_oligo) |>
    mutate(
      family = ifelse(is_oligo, "Oligochaeta", family),
      bmwp   = ifelse(is_oligo, 1, bmwp)
    ) |>
    distinct(family, bmwp) |>
    arrange(family)

  n_bmwp_units_diag <- nrow(bmwp_genus_diag) + nrow(bmwp_fam_diag)
  bmwp_total_diag   <- sum(bmwp_genus_diag$bmwp) + sum(bmwp_fam_diag$bmwp)

  cat("--- BMWP / ASPT -----------------------------------------\n")
  cat("All taxa with a BMWP score:\n")
  print(as.data.frame(bmwp_all_taxa), row.names = FALSE)
  if (nrow(bmwp_genus_diag) > 0) {
    cat("\nGenus-level scoring units (not rolled up to family):\n")
    print(as.data.frame(bmwp_genus_diag), row.names = FALSE)
  }
  cat("\nFamily-level scoring units (deduplicated; Oligochaeta collapsed):\n")
  print(as.data.frame(bmwp_fam_diag), row.names = FALSE)
  cat("\n BMWP score      :", bmwp_total_diag, "\n")
  cat(" N scoring units :", n_bmwp_units_diag, "\n")
  cat(" ASPT            :", round(bmwp_total_diag / n_bmwp_units_diag, 4), "\n\n")

  # ---- DEP --------------------------------------------------------
  dep_taxa <- sub |>
    filter(DEP %in% TRUE) |>
    select(OTL_taxonname, order, family, abundance) |>
    arrange(order, family, OTL_taxonname)

  cat("--- DEP (Diptera + Ephemeroptera + Plecoptera) ----------\n")
  cat(" N DEP taxa (OTL_taxonname level) :", nrow(dep_taxa), "\n")
  cat(" N E+P taxa                       :", sum(toupper(dep_taxa$order) %in% c("EPHEMEROPTERA", "PLECOPTERA")), "\n")
  cat(" N D families                     :", n_distinct(dep_taxa$family[toupper(dep_taxa$order) == "DIPTERA"]), "\n")
  cat(" N unique DEP families            :", n_distinct(dep_taxa$family), "\n")
  print(as.data.frame(dep_taxa), row.names = FALSE)
  cat("\n Unique DEP families:\n")
  print(sort(unique(dep_taxa$family)))
  cat("\n")

  # ---- EHP --------------------------------------------------------
  ehp_taxa <- sub |>
    filter(EHP %in% TRUE) |>
    select(OTL_taxonname, order, family, abundance) |>
    arrange(order, family, OTL_taxonname)

  ehp_prop <- sum(ehp_taxa$abundance, na.rm = TRUE) / total_abund

  cat("--- EHP (Ephemeroptera + Hemiptera + Plecoptera) --------\n")
  cat(" N EHP taxa      :", nrow(ehp_taxa), "\n")
  cat(" EHP abundance   :", sum(ehp_taxa$abundance, na.rm = TRUE), "\n")
  cat(" EHP proportion  :", round(ehp_prop, 7), "\n")
  print(as.data.frame(ehp_taxa), row.names = FALSE)
  cat("\n")

  # ---- CrHi -------------------------------------------------------
  crhi_taxa <- sub |>
    filter(CrHi %in% TRUE) |>
    select(OTL_taxonname, order, family, abundance) |>
    arrange(order, family, OTL_taxonname)

  crhi_prop <- sum(crhi_taxa$abundance, na.rm = TRUE) / total_abund

  cat("--- CrHi (Crustacea + Hirudinea) ------------------------\n")
  cat(" N CrHi taxa     :", nrow(crhi_taxa), "\n")
  cat(" CrHi abundance  :", sum(crhi_taxa$abundance, na.rm = TRUE), "\n")
  cat(" CrHi proportion :", round(crhi_prop, 7), "\n")
  print(as.data.frame(crhi_taxa), row.names = FALSE)
  cat("\n")
  cat("--- Summary ---------------------------------------------\n")
  cat(" BMWP score             :", bmwp_total_diag, "\n")
  cat(" N BMWP taxa            :", n_bmwp_units_diag, "\n")
  cat(" Total abundance        :", total_abund, "\n")
  cat(" ASPT                   :", round(bmwp_total_diag / n_bmwp_units_diag, 4), "\n")
  cat(" N DEP                  :", n_distinct(dep_taxa$family[toupper(dep_taxa$order) == "DIPTERA"]) + sum(toupper(dep_taxa$order) %in% c("EPHEMEROPTERA", "PLECOPTERA")), "\n")
  cat(" N E+P taxa             :", sum(toupper(dep_taxa$order) %in% c("EPHEMEROPTERA", "PLECOPTERA")), "\n")
  cat(" N D families           :", n_distinct(dep_taxa$family[toupper(dep_taxa$order) == "DIPTERA"]), "\n")
  cat(" EHP proportion         :", round(ehp_prop, 7), "\n")
  cat(" CrHi proportion        :", round(crhi_prop, 7), "\n")
  cat(" EHP - CrHi proportion  :", round(ehp_prop - crhi_prop, 7), "\n")
  cat("================================================================\n\n")

  return(invisible(list(
    bmwp_all_taxa    = as.data.frame(bmwp_all_taxa),
    bmwp_genus_units = as.data.frame(bmwp_genus_diag),
    bmwp_fam_units   = as.data.frame(bmwp_fam_diag),
    dep_taxa         = as.data.frame(dep_taxa),
    ehp_taxa         = as.data.frame(ehp_taxa),
    crhi_taxa        = as.data.frame(crhi_taxa)
  )))
}

# Example - check 5 random sites in 2022:
diagnose_site_year(all_lf_agg, "LTR1299", 2022)
diagnose_site_year(all_lf_agg, "LTR1447", 2022)
diagnose_site_year(all_lf_agg, "LTR1449", 2022)
diagnose_site_year(all_lf_agg, "LTR1610", 2022)
diagnose_site_year(all_lf_agg, "LTR162", 2022)

# ============================================================
# CALCULATE TAXONOMIC INDICES PER SITE x YEAR
# ============================================================

site_year_combos <- all_lf_agg |> distinct(site_id, year)

# Create empty dataframe to store results
TD <- NULL

for (k in seq_len(nrow(site_year_combos))) {
  site_i <- site_year_combos$site_id[k]
  year_i <- site_year_combos$year[k]

  sub <- all_lf_agg[all_lf_agg$site_id == site_i & all_lf_agg$year == year_i, ]

  # Wide-format abundance matrix (rows = site-year, columns = taxa)
  sub.m      <- dcast(sub, site_code + year ~ OTL_taxonname, sum, value.var = "abundance")
  sub.ta     <- sub.m[, 3:ncol(sub.m), drop = FALSE]   # remove site_code and year columns

  # BMWP and ASPT — mostly family-level, with one genus-level exception:
  #   Ancylus sp. is scored at GENUS level as its own independent unit
  #   (Ancylidae was synonymised into Planorbidae taxonomically, but
  #   both retain distinct BMWP scores). Co-occurring Planorbidae still
  #   contributes its own family unit.
  #   1. Genus-level taxa (Ancylus sp.): scored individually
  #   2. All other taxa: deduplicate by family (one score per family);
  #      Oligochaeta families (incl. Naididae / Tubificidae) collapse
  #      to a single "Oligochaeta" unit at score 1.
  #   3. BMWP = sum of all scoring unit scores
  #   4. ASPT = BMWP / total number of scoring units
  bmwp_genus <- sub |>
    filter(OTL_taxonname %in% BMWP_GENUS_LEVEL, !is.na(bmwp)) |>
    distinct(OTL_taxonname, bmwp)
  bmwp_fam   <- sub |>
    mutate(is_oligo = OTL_taxonname %in% BMWP_OLIGOCHAETA_OTL) |>
    # v5: allow NA bmwp through when is_oligo, so Naididae / Tubificidae
    # rows (no longer in BMWP_lookup) survive to be collapsed to
    # "Oligochaeta" with score 1 by the mutate below.
    filter(!OTL_taxonname %in% BMWP_GENUS_LEVEL,
           (!is.na(bmwp) | is_oligo),
           !is.na(family) | is_oligo) |>
    mutate(
      family = ifelse(is_oligo, "Oligochaeta", family),
      bmwp   = ifelse(is_oligo, 1, bmwp)
    ) |>
    distinct(family, bmwp)
  n_bmwp_units <- nrow(bmwp_genus) + nrow(bmwp_fam)
  BMWP         <- sum(bmwp_genus$bmwp) + sum(bmwp_fam$bmwp)
  ASPT         <- if (n_bmwp_units > 0) BMWP / n_bmwp_units else NA_real_

  SppRich <- specnumber(sub.ta)   # taxonomic richness (n taxa with abundance > 0)
  Abund   <- rowSums(sub.ta)      # total abundance

  TD.i <- data.frame(
    site_id   = site_i,
    site_code = sub.m$site_code,
    year      = sub.m$year,
    SppRich, Abund,
    BMWP, BMWP_ntaxa = n_bmwp_units, ASPT
  )

  TD <- rbind(TD, TD.i)
  rm(TD.i, sub.m, sub.ta, sub, bmwp_genus, bmwp_fam, n_bmwp_units,
     SppRich, Abund, BMWP, ASPT)
}
rm(k, site_i, year_i, site_year_combos)

# ============================================================
# TAXONOMIC GROUP SUBSETS — RICHNESS AND ABUNDANCE PER SITE x YEAR
# ============================================================
# Two helper functions calculate group-level richness and total
# abundance for any taxonomic group flagged with TRUE:
#
#  calc_group_indices   — for groups WITHOUT Diptera: richness
#    is counted at OTL_taxonname level (taxa-level richness).
#    Column suffix: _spp_richness
#
#  calc_group_indices_D — for groups WITH Diptera: richness is
#    a MIXED metric — Diptera counted at family level (unique
#    families), all other orders at OTL_taxonname level.
#    This reflects the mixed taxonomic resolution of Diptera
#    identifications in the source data.
#    Column suffix: _richness (no _spp_ prefix)

calc_group_indices <- function(data, group_col) {
  sub_lf           <- data[data[[group_col]] %in% TRUE, ]
  site_year_combos <- sub_lf |> distinct(site_id, year)
  result           <- NULL

  for (k in seq_len(nrow(site_year_combos))) {
    site_i <- site_year_combos$site_id[k]
    year_i <- site_year_combos$year[k]
    sub    <- sub_lf[sub_lf$site_id == site_i & sub_lf$year == year_i, ]
    sub.m  <- dcast(sub, site_code + year ~ OTL_taxonname, sum, value.var = "abundance")
    sub.ta <- sub.m[, 3:ncol(sub.m), drop = FALSE]
    result <- rbind(result, data.frame(
      site_code = sub.m$site_code,
      year      = sub.m$year,
      SppRich   = specnumber(sub.ta),
      Abund     = rowSums(sub.ta)
    ))
  }
  return(result)
}

calc_group_indices_D <- function(data, group_col) {
  # Diptera richness: unique families only (family-level resolution)
  # All other orders: OTL_taxonname level
  sub_lf <- data[data[[group_col]] %in% TRUE, ]
  sub_lf |>
    group_by(site_code, year) |>
    summarise(
      SppRich = n_distinct(family[diptera %in% TRUE & !is.na(family)]) +
                sum(!diptera %in% TRUE),
      Abund   = sum(abundance, na.rm = TRUE),
      .groups = "drop"
    )
}

# ---- Ephemeroptera ----
TD_ephemeroptera <- calc_group_indices(all_lf_agg, "ephemeroptera")
colnames(TD_ephemeroptera)[3:4] <- c("ephemeroptera_spp_richness", "ephemeroptera_abundance")
TD <- left_join(TD, TD_ephemeroptera, by = c("site_code", "year"))

# ---- Plecoptera ----
TD_plecoptera <- calc_group_indices(all_lf_agg, "plecoptera")
colnames(TD_plecoptera)[3:4] <- c("plecoptera_spp_richness", "plecoptera_abundance")
TD <- left_join(TD, TD_plecoptera, by = c("site_code", "year"))

# ---- Diptera ----
# Two richness measures: taxa-level (OTL_taxonname) and family-level
TD_diptera <- all_lf_agg |>
  filter(diptera %in% TRUE) |>
  group_by(site_code, year) |>
  summarise(
    diptera_spp_richness = n(),
    diptera_fam_richness = n_distinct(family[!is.na(family)]),
    diptera_abundance    = sum(abundance, na.rm = TRUE),
    .groups = "drop"
  )
TD <- left_join(TD, TD_diptera, by = c("site_code", "year"))

# ---- Hemiptera ----
TD_hemiptera <- calc_group_indices(all_lf_agg, "hemiptera")
colnames(TD_hemiptera)[3:4] <- c("hemiptera_spp_richness", "hemiptera_abundance")
TD <- left_join(TD, TD_hemiptera, by = c("site_code", "year"))

# ---- Crustacea ----
TD_crustacea <- calc_group_indices(all_lf_agg, "crustacea")
colnames(TD_crustacea)[3:4] <- c("crustacea_spp_richness", "crustacea_abundance")
TD <- left_join(TD, TD_crustacea, by = c("site_code", "year"))

# ---- Crustacea ----
TD_hirudinea <- calc_group_indices(all_lf_agg, "hirudinea")
colnames(TD_hirudinea)[3:4] <- c("hirudinea_spp_richness", "hirudinea_abundance")
TD <- left_join(TD, TD_hirudinea, by = c("site_code", "year"))

# ============================================================
# COMPOUND GROUP SUBSETS
# ============================================================
# Groups without Diptera: richness at OTL_taxonname level (_spp_richness)
# Groups with Diptera:    mixed richness — D at family level,
#                         all other orders at OTL_taxonname level (_richness)

# ---- CrHi (Crustacea + Hirudinea) ----
TD_CrHi <- calc_group_indices(all_lf_agg, "CrHi")
colnames(TD_CrHi)[3:4] <- c("CrHi_spp_richness", "CrHi_abundance")
TD <- left_join(TD, TD_CrHi, by = c("site_code", "year"))

# ---- EHP (Ephemeroptera + Hemiptera + Plecoptera) ----
TD_EHP <- calc_group_indices(all_lf_agg, "EHP")
colnames(TD_EHP)[3:4] <- c("EHP_spp_richness", "EHP_abundance")
TD <- left_join(TD, TD_EHP, by = c("site_code", "year"))

# ---- DEP (Diptera + Ephemeroptera + Plecoptera) ----
# Richness = N unique Diptera families + N individual E+P taxa (OTL_taxonname level)
# Abundance = sum of all DEP abundances
TD_DEP <- all_lf_agg |>
  filter(DEP %in% TRUE) |>
  group_by(site_code, year) |>
  summarise(
    DEP_richness  = n_distinct(family[toupper(order) == "DIPTERA"]) +
                    sum(toupper(order) %in% c("EPHEMEROPTERA", "PLECOPTERA")),
    DEP_abundance = sum(abundance, na.rm = TRUE),
    .groups = "drop"
  )
TD <- left_join(TD, TD_DEP, by = c("site_code", "year"))

# ============================================================
# REPLACE NA WITH 0 FOR GROUP RICHNESS AND ABUNDANCE COLUMNS
# ============================================================
# Absence of a group at a site-year is a true zero (the group
# was not found), not missing data. NAs arise only because
# left_join finds no matching rows for that group.

TD <- TD |>
  mutate(across(ends_with("_spp_richness") | ends_with("_fam_richness") |
                ends_with("_richness")     | ends_with("_abundance"),
                ~coalesce(., 0)))

# ============================================================
# PERCENTAGE ABUNDANCE PER GROUP (relative to total abundance)
# ============================================================
# Absent groups contribute 0% rather than NA%.

TD <- TD |>
  mutate(
    # Individual orders
    ephemeroptera_prop = ephemeroptera_abundance / Abund,
    plecoptera_prop    = plecoptera_abundance    / Abund,
    diptera_prop       = diptera_abundance       / Abund,
    hemiptera_prop     = hemiptera_abundance     / Abund,
    crustacea_prop     = crustacea_abundance     / Abund,
    # Compound groups
    CrHi_prop  = CrHi_abundance  / Abund,
    EHP_prop   = EHP_abundance   / Abund,
    DEP_prop   = DEP_abundance   / Abund
    )

# ============================================================
# DERIVED INDICES: GROUP RICHNESS AND PROPORTION MINUS Cr / Hi / CrHi
# ============================================================
# For each compound group, three subtraction variants are calculated:
#   _minus_Cr_*   : group metric minus Crustacea metric
#   _minus_Hi_*   : group metric minus Hirudinea metric
#   _minus_CrHi_* : group metric minus CrHi (Crustacea + Hirudinea) metric
#
# Richness subtraction uses the group's richness column
# (either _spp_richness for non-D groups or _richness for D groups)
# minus crustacea_spp_richness / hirudinea_spp_richness / CrHi_spp_richness.
# Proportion subtraction is analogous.
#
# Note: results can be negative for groups that do not ecologically
# overlap with Cr or Hi; these are retained as arithmetic indices.

TD <- TD |>
  mutate(
    # ---- EHP ----
    EHP_minus_CrHi_prop       = EHP_prop - CrHi_prop
  )

# Save the file we just created
write_xlsx(TD, "Outputs/4_calculated_taxonomic_indices.xlsx")

#==================== CLEAN UP WORKSPACE =====================
library(pacman)
rm(list = ls())       # Remove all objects from environment
gc()                  # Frees up unused memory
p_unload(all)         # Unload all loaded packages
graphics.off()        # Close all graphical devices
cat("\014")           # Clear the console
# Clear mind :)
