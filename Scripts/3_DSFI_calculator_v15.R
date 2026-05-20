################################################################################
# DSFI Calculator (v15)
# Danish Stream Fauna Index (Skriver et al. 2000)
#
# Input: Long-format macroinvertebrate data with columns:
#   site_code, family, genus, sampling_method, raw_abundance
#
# Naming convention (input):
#   - family / order / class / phylum: UPPERCASE
#   - genus: Title case (e.g. "Limnius")
#   - species: lowercase (e.g. "volckmari")
#
# Output: DSFI index value (1-7) per site_code with intermediate steps,
#   plus site_id and year derived by splitting site_code on "_".
#
# Changes from v14:
#   - calculate_dsfi() now adds site_id and year columns to the results,
#     derived by splitting site_code on the underscore (e.g. "LTR70_2016"
#     -> site_id "LTR70", year 2016). Columns ordered: site_code, site_id,
#     year, then the existing DSFI fields.
#
# Changes from v13:
#   - The upstream OTL standardisation now emits genus in title case
#     ("Limnius") and species in lowercase ("volckmari"), matching standard
#     binomial convention. The v13 internal title-casing of genus is no
#     longer needed and has been removed; map_to_dsfi_taxon now compares
#     genus directly. The safety guard in aggregate_for_dsfi is inverted:
#     it now warns if any genus value arrives all-uppercase, which would
#     indicate the convention regressed at the source.
#
# Changes from v12 (carried into v13/v14):
#   - BUG FIX: case sensitivity in map_to_dsfi_taxon. v12 silently fell
#     through to family-level "_other" for every genus-level indicator
#     (Elmis, Limnius, Elodes, Gammarus, Asellus, Sphaerium, Ancylus,
#     Acroloxus, Helobdella, Erpobdella, Chironomus, Eristalis, Myathropa,
#     Helophilus, Lymnaea) when input genus was uppercase.
################################################################################

library(readxl)
library(tidyverse)
library(writexl)

# =============================================================================
# STEP 1: Define DSFI taxon mapping
# =============================================================================
# This function maps each row of your data to the DSFI-relevant taxon name.
# The DSFI operates at different taxonomic levels for different groups:
#   - Plecoptera: genus level
#   - Ephemeroptera: family level
#   - Trichoptera: family level
#   - Specific genera: Gammarus, Asellus, Elmis, Limnius, Elodes, etc.
#   - Diptera: mixed (family for most, genus for Chironomus)
#   - Others: as per Table 1 of Skriver et al. 2000

map_to_dsfi_taxon <- function(family, genus, order, rank, class, phylum) {

  family <- toupper(trimws(family))
  genus  <- trimws(genus)
  order  <- toupper(trimws(order))
  rank   <- tolower(trimws(rank))
  class  <- toupper(trimws(class))

  # --- Turbellaria / Tricladida ---
  if (!is.na(order) && order == "TRICLADIDA") return("Tricladida")
  if (!is.na(family) && family %in% c("PLANARIIDAE", "DENDROCOELIDAE", "DUGESIIDAE")) {
    return("Tricladida")
  }

  # --- Hirudinea (leeches) - must be checked BEFORE Oligochaeta ---
  # Both leeches and Oligochaeta are class CLITELLATA
  if (!is.na(family) && family == "GLOSSIPHONIIDAE") {
    if (!is.na(genus) && genus == "Helobdella") return("Helobdella")
    return("Glossiphonia_other")
  }
  if (!is.na(family) && family == "ERPOBDELLIDAE") return("Erpobdella")
  if (!is.na(family) && family == "PISCICOLIDAE") return("Hirudinea_other")
  if (!is.na(family) && family == "HAEMOPIDAE") return("Hirudinea_other")
  if (!is.na(family) && family == "HIRUDINIDAE") return("Hirudinea_other")

  # --- Oligochaeta (identified by class or subclass rank) ---
  # Checked AFTER leeches to avoid misclassification
  if (!is.na(rank) && rank == "subclass" && !is.na(class) && class == "CLITELLATA") {
    return("Oligochaeta")
  }

  # --- Crustacea ---
  if (!is.na(family) && family == "ASELLIDAE") return("Asellus")
  if (!is.na(genus) && genus == "Asellus") return("Asellus")
  # Gammarus: check genus first, other Gammaridae genera are not Gammarus
  if (!is.na(genus) && genus == "Gammarus") return("Gammarus")
  if (!is.na(family) && family == "GAMMARIDAE") return("Gammaridae_other")

  # --- Ephemeroptera (family level) ---
  ephem_families <- c("AMETROPODIDAE", "BAETIDAE", "CAENIDAE", "EPHEMERIDAE",
                      "EPHEMERELLIDAE", "HEPTAGENIIDAE", "LEPTOPHLEBIIDAE",
                      "SIPHLONURIDAE")
  if (!is.na(family) && family %in% ephem_families) return(family)
  # Catch other Ephemeroptera families not in DSFI
  if (!is.na(order) && order == "EPHEMEROPTERA") return(paste0(family, "_ephem_other"))

  # --- Plecoptera (genus level) ---
  pleco_genera <- c("Amphinemura", "Brachyptera", "Capnia", "Isogenus",
                    "Isoperla", "Isoptena", "Leuctra", "Nemoura", "Nemurella",
                    "Perlodes", "Protonemura", "Siphonoperla", "Taeniopteryx")
  if (!is.na(order) && order == "PLECOPTERA") {
    if (!is.na(genus) && genus %in% pleco_genera) return(genus)
    if (!is.na(genus) && genus != "Gen.") return(paste0(genus, "_pleco_other"))
    return("Plecoptera_unid")
  }

  # --- Coleoptera (specific genera) ---
  if (!is.na(genus) && genus == "Elmis") return("Elmis")
  if (!is.na(genus) && genus == "Limnius") return("Limnius")
  if (!is.na(genus) && genus == "Elodes") return("Elodes")
  # Oulimnius in ELMIDAE maps to Elmidae_other (not a DSFI indicator)
  if (!is.na(family) && family == "ELMIDAE") return("Elmidae_other")
  if (!is.na(family) && family == "DYTISCIDAE") return("Dytiscidae")
  if (!is.na(family) && family == "HYDRAENIDAE") return("Hydraenidae")
  if (!is.na(family) && family == "HYDROPHILIDAE") return("Hydrophilidae")
  if (!is.na(order) && order == "COLEOPTERA") return(paste0(family, "_col_other"))

  # --- Megaloptera ---
  if (!is.na(family) && family == "SIALIDAE") return("Sialis")

  # --- Trichoptera (family level, split case-bearing vs caseless) ---
  caseless_trich <- c("ECNOMIDAE", "HYDROPSYCHIDAE", "PHILOPOTAMIDAE",
                      "POLYCENTROPODIDAE", "PSYCHOMYIIDAE", "RHYACOPHILIDAE")
  casebearing_trich <- c("BERAEIDAE", "BRACHYCENTRIDAE", "HYDROPTILIDAE",
                         "GOERIDAE", "GLOSSOSOMATIDAE", "LEPTOCERIDAE",
                         "LEPIDOSTOMATIDAE", "LIMNEPHILIDAE", "MOLANNIDAE",
                         "ODONTOCERIDAE", "PHRYGANEIDAE", "SERICOSTOMATIDAE")
  if (!is.na(family) && family %in% caseless_trich) return(family)
  if (!is.na(family) && family %in% casebearing_trich) return(family)
  if (!is.na(order) && order == "TRICHOPTERA") return(paste0(family, "_trich_other"))

  # --- Diptera ---
  if (!is.na(family) && family == "CHIRONOMIDAE") {
    # Need to distinguish Chironomus from other Chironomidae
    if (!is.na(genus) && genus == "Chironomus") return("Chironomus")
    return("Chironomidae")
  }
  if (!is.na(family) && family == "SIMULIIDAE") return("Simuliidae")
  if (!is.na(family) && family == "PSYCHODIDAE") return("Psychodidae")
  if (!is.na(family) && family == "SYRPHIDAE") return("Eristalini")
  # Eristalini check by genus
  if (!is.na(genus) && genus %in% c("Eristalis", "Myathropa", "Helophilus")) return("Eristalini")
  if (!is.na(family) && family == "LIMONIIDAE") return("Limoniidae")
  if (!is.na(family) && family == "ATHERICIDAE") return("Atherix")
  if (!is.na(order) && order == "DIPTERA") return(paste0(family, "_dipt_other"))

  # --- Mollusca ---
  if (!is.na(genus) && genus == "Ancylus") return("Ancylus")
  if (!is.na(genus) && genus == "Acroloxus") return("Acroloxus")  # not DSFI
  if (!is.na(family) && family == "LYMNAEIDAE") {
    if (!is.na(genus) && genus == "Lymnaea") return("Lymnaea")
    return("LYMNAEIDAE_other")  # Radix, Galba, Ampullaceana, Peregriana, Stagnicola etc.
  }
  if (!is.na(family) && family == "SPHAERIIDAE") {
    if (!is.na(genus) && genus == "Sphaerium") return("Sphaerium")
    return("Sphaeriidae_other")  # Pisidium etc.
  }
  if (!is.na(family) && family == "PLANORBIDAE") {
    if (!is.na(genus) && genus == "Ancylus") return("Ancylus")
    return("Planorbidae_other")
  }

  # --- Everything else ---
  if (!is.na(family)) return(paste0(family, "_other"))
  return("UNMAPPED")
}


# =============================================================================
# STEP 2: Aggregate data to DSFI taxon level
# =============================================================================

aggregate_for_dsfi <- function(dat) {

  # v14: Convention check. genus is expected in title case (e.g. "Limnius").
  # Warn if any value arrives all-uppercase, which would indicate the OTL
  # standardisation has regressed and genus indicators will silently fall
  # through to family-level "_other" mappings.
  if ("genus" %in% names(dat)) {
    genus_vals <- dat$genus[!is.na(dat$genus) & nchar(dat$genus) > 1]
    upper_only <- genus_vals == toupper(genus_vals) &
                  genus_vals != tolower(genus_vals)
    if (any(upper_only)) {
      warning(sprintf(
        "DSFI: %d row(s) have all-uppercase genus values (e.g. '%s'). Expected title case — check upstream OTL standardisation.",
        sum(upper_only),
        paste(head(unique(genus_vals[upper_only]), 3), collapse = "', '")))
    }
  }

  # --- Lymnaea reclassification ---
  # The DSFI (2000) treated all Lymnaea species as one taxon. Many former
  # Lymnaea species have since been reclassified under new genera (e.g.
  # L. auricularia -> Radix, L. peregra -> Peregriana, L. truncatula -> Galba,
  # L. patula -> Ampullaceana, L. glutinosa -> Myxas). For DSFI purposes,
  # if raw_taxonname indicates the original name was Lymnaea or Lymnaeidae,
  # the genus is set to "Lymnaea" so it maps correctly.
  # Note: only specimens originally identified as Lymnaea are reclassified.
  # Specimens with current names (e.g. "Radix balthica") that were never
  # called Lymnaea are left as LYMNAEIDAE_other.
  if ("raw_taxonname" %in% names(dat)) {
    lymnaea_rows <- !is.na(dat$family) & dat$family == "LYMNAEIDAE" &
      !is.na(dat$raw_taxonname) &
      grepl("^Lymnaea|^Lymnaeidae", dat$raw_taxonname)
    dat$genus[lymnaea_rows] <- "Lymnaea"
  }

  # Map each row to a DSFI taxon
  dat$dsfi_taxon <- mapply(map_to_dsfi_taxon,
                           dat$family, dat$genus, dat$order,
                           dat$rank, dat$class, dat$phylum,
                           USE.NAMES = FALSE)

  # Reclassify sampling_method
  dat$method <- ifelse(grepl("kick", dat$sampling_method, ignore.case = TRUE),
                       "kick", "hand")

  # --- Two aggregation passes ---
  # 1) UNFILTERED: used for diversity groups (Table 3) where presence of a
  #    single specimen in the fauna sample is sufficient.
  # 2) FILTERED: used for indicator group determination (Table 2) where
  #    individual taxa in the kick sample must have raw_abundance >= 2.
  #    Hand-picked samples have no minimum (>= 1 is sufficient).
  #
  # Example: if two Sericostomatidae species each have kick = 1, the
  # FILTERED kick total = 0 (both excluded), but the UNFILTERED total = 2
  # so SERICOSTOMATIDAE still counts as a positive diversity group.

  # Unfiltered aggregation (for diversity groups)
  agg_unfilt <- dat |>
    group_by(site_code, dsfi_taxon, method) |>
    summarise(abundance = sum(raw_abundance, na.rm = TRUE), .groups = "drop")

  # Filtered aggregation (for indicator groups)
  dat_filt <- dat[!(dat$method == "kick" & dat$raw_abundance < 2), ]
  agg_filt <- dat_filt |>
    group_by(site_code, dsfi_taxon, method) |>
    summarise(abundance = sum(raw_abundance, na.rm = TRUE), .groups = "drop")

  # Store the unfiltered version as an attribute
  attr(agg_filt, "unfiltered") <- agg_unfilt

  return(agg_filt)
}


# =============================================================================
# STEP 2b: Count original taxa contributing to each DSFI aggregation
# =============================================================================
# Shows how many original taxa (species, genera, or families) were aggregated
# into each DSFI taxon at each site. Useful for verifying that aggregation
# is working as expected.
#
# Usage:
#   counts <- count_contributing_taxa(dat)
#   counts[counts$site_code == "LTR1299_2022", ]

count_contributing_taxa <- function(dat) {

  # Lymnaea reclassification (same as in aggregate_for_dsfi)
  if ("raw_taxonname" %in% names(dat)) {
    lymnaea_rows <- !is.na(dat$family) & dat$family == "LYMNAEIDAE" &
      !is.na(dat$raw_taxonname) &
      grepl("^Lymnaea|^Lymnaeidae", dat$raw_taxonname)
    dat$genus[lymnaea_rows] <- "Lymnaea"
  }

  # Map each row to a DSFI taxon (same as in aggregate_for_dsfi)
  dat$dsfi_taxon <- mapply(map_to_dsfi_taxon,
                           dat$family, dat$genus, dat$order,
                           dat$rank, dat$class, dat$phylum,
                           USE.NAMES = FALSE)

  # Apply same kick sample filtering rule as aggregate_for_dsfi
  dat$method <- ifelse(grepl("kick", dat$sampling_method, ignore.case = TRUE),
                       "kick", "hand")
  dat <- dat[!(dat$method == "kick" & dat$raw_abundance < 2), ]

  # Count distinct original taxa per site_code and dsfi_taxon
  # Use OTL_taxonname if available, otherwise fall back to genus + family
  if ("OTL_taxonname" %in% names(dat)) {
    taxa_counts <- dat |>
      group_by(site_code, dsfi_taxon) |>
      summarise(
        n_original_taxa = n_distinct(OTL_taxonname),
        original_taxa = paste(sort(unique(OTL_taxonname)), collapse = "; "),
        total_abundance = sum(raw_abundance, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    taxa_counts <- dat |>
      mutate(taxon_label = paste(family, genus)) |>
      group_by(site_code, dsfi_taxon) |>
      summarise(
        n_original_taxa = n_distinct(taxon_label),
        original_taxa = paste(sort(unique(taxon_label)), collapse = "; "),
        total_abundance = sum(raw_abundance, na.rm = TRUE),
        .groups = "drop"
      )
  }

  return(taxa_counts)
}


# =============================================================================
# STEP 3: Check taxon presence according to DSFI rules
# =============================================================================
# Default: present if >=2 in kick OR >=1 in hand
# Some taxa have special thresholds

check_present <- function(agg_site, taxon, kick_min = 2, hand_min = 1) {
  kick_n <- sum(agg_site$abundance[agg_site$dsfi_taxon == taxon & agg_site$method == "kick"], na.rm = TRUE)
  hand_n <- sum(agg_site$abundance[agg_site$dsfi_taxon == taxon & agg_site$method == "hand"], na.rm = TRUE)
  return(kick_n >= kick_min | hand_n >= hand_min)
}

get_kick_abundance <- function(agg_site, taxon) {
  sum(agg_site$abundance[agg_site$dsfi_taxon == taxon & agg_site$method == "kick"], na.rm = TRUE)
}

get_total_abundance <- function(agg_site, taxon) {
  sum(agg_site$abundance[agg_site$dsfi_taxon == taxon], na.rm = TRUE)
}


# =============================================================================
# STEP 4: Calculate diversity groups (Table 3)
# =============================================================================

calc_diversity_groups <- function(agg_site) {

  # --- Positive diversity groups ---
  # Presence for diversity groups uses total abundance >= 1 (kick + hand combined).
  # Paper states: "the presence in the fauna sample of only one specimen of one
  # of the diversity groups is sufficient for that taxon to be included"
  # Exception: Oligochaeta requires >= 100.
  pos <- 0

  # Tricladida
  if (get_total_abundance(agg_site, "Tricladida") >= 1) pos <- pos + 1

  # Gammarus
  if (get_total_abundance(agg_site, "Gammarus") >= 1) pos <- pos + 1

  # Every genus of Plecoptera present (Table 3: "Every genus of Plecoptera")
  # This includes ALL Plecoptera genera, not just the 13 DSFI indicator genera.
  # Non-indicator genera (e.g., Diura, Xanthoperla, Zwicknia, Capnopsis)
  # still count as positive diversity groups.
  pleco_taxa <- agg_site$dsfi_taxon[grepl("^(Amphinemura|Brachyptera|Capnia|Isogenus|Isoperla|Isoptena|Leuctra|Nemoura|Nemurella|Perlodes|Protonemura|Siphonoperla|Taeniopteryx)$", agg_site$dsfi_taxon) |
                                      grepl("_pleco_other$", agg_site$dsfi_taxon)]
  pleco_genera_present <- unique(pleco_taxa)
  for (g in pleco_genera_present) {
    if (get_total_abundance(agg_site, g) >= 1) pos <- pos + 1
  }

  # Every family of Ephemeroptera (Table 3: "Every family of Ephemeroptera")
  # This includes ALL Ephemeroptera families, not just the 8 DSFI indicator families.
  # Non-indicator families (e.g., POTAMANTHIDAE) still count as positive diversity groups.
  ephem_dsfi <- c("AMETROPODIDAE", "BAETIDAE", "CAENIDAE", "EPHEMERIDAE",
                  "EPHEMERELLIDAE", "HEPTAGENIIDAE", "LEPTOPHLEBIIDAE",
                  "SIPHLONURIDAE")
  for (f in ephem_dsfi) {
    if (get_total_abundance(agg_site, f) >= 1) pos <- pos + 1
  }
  ephem_other <- unique(agg_site$dsfi_taxon[grepl("_ephem_other$", agg_site$dsfi_taxon)])
  for (f in ephem_other) {
    if (get_total_abundance(agg_site, f) >= 1) pos <- pos + 1
  }

  # Elmis
  if (get_total_abundance(agg_site, "Elmis") >= 1) pos <- pos + 1
  # Limnius
  if (get_total_abundance(agg_site, "Limnius") >= 1) pos <- pos + 1
  # Elodes
  if (get_total_abundance(agg_site, "Elodes") >= 1) pos <- pos + 1

  # Rhyacophilidae
  if (get_total_abundance(agg_site, "RHYACOPHILIDAE") >= 1) pos <- pos + 1

  # Every family of case-bearing Trichoptera (Table 3)
  # This includes ALL case-bearing families, not just the 12 DSFI indicator families.
  # Non-indicator case-bearing families (e.g., APATANIIDAE) still count as positive
  # diversity groups.
  cb_trich_dsfi <- c("BERAEIDAE", "BRACHYCENTRIDAE", "HYDROPTILIDAE", "GOERIDAE",
                     "GLOSSOSOMATIDAE", "LEPTOCERIDAE", "LEPIDOSTOMATIDAE",
                     "LIMNEPHILIDAE", "MOLANNIDAE", "ODONTOCERIDAE",
                     "PHRYGANEIDAE", "SERICOSTOMATIDAE")
  for (f in cb_trich_dsfi) {
    if (get_total_abundance(agg_site, f) >= 1) pos <- pos + 1
  }
  # Additional case-bearing families not in Table 1
  # These are mapped as _trich_other but are case-bearing
  casebearing_extra <- c("APATANIIDAE")
  cb_other <- unique(agg_site$dsfi_taxon[grepl(paste0("^(", paste(casebearing_extra, collapse = "|"), ")_trich_other$"), agg_site$dsfi_taxon)])
  for (f in cb_other) {
    if (get_total_abundance(agg_site, f) >= 1) pos <- pos + 1
  }

  # Ancylus
  if (get_total_abundance(agg_site, "Ancylus") >= 1) pos <- pos + 1

  # --- Negative diversity groups ---
  neg <- 0

  # Oligochaeta >= 100 (special threshold, uses total abundance)
  if (get_total_abundance(agg_site, "Oligochaeta") >= 100) neg <- neg + 1
  # All other negative groups: >= 1 total (kick + hand)
  if (get_total_abundance(agg_site, "Helobdella") >= 1) neg <- neg + 1
  if (get_total_abundance(agg_site, "Erpobdella") >= 1) neg <- neg + 1
  if (get_total_abundance(agg_site, "Asellus") >= 1) neg <- neg + 1
  if (get_total_abundance(agg_site, "Sialis") >= 1) neg <- neg + 1
  if (get_total_abundance(agg_site, "Psychodidae") >= 1) neg <- neg + 1
  if (get_total_abundance(agg_site, "Chironomus") >= 1) neg <- neg + 1
  if (get_total_abundance(agg_site, "Eristalini") >= 1) neg <- neg + 1
  if (get_total_abundance(agg_site, "Sphaerium") >= 1) neg <- neg + 1
  if (get_total_abundance(agg_site, "Lymnaea") >= 1) neg <- neg + 1

  return(list(positive = pos, negative = neg, net = pos - neg))
}


# =============================================================================
# STEP 5: Determine Indicator Group (Table 2)
# =============================================================================

determine_ig <- function(agg_site, start_ig = 1) {

  # Helper: count how many of a set of taxa are present (standard threshold)
  count_present <- function(taxa, kick_min = 2, hand_min = 1) {
    sum(sapply(taxa, function(t) check_present(agg_site, t, kick_min, hand_min)))
  }

  # Kick abundances needed for precluding rules
  asellus_kick <- get_kick_abundance(agg_site, "Asellus")
  chironomus_kick <- get_kick_abundance(agg_site, "Chironomus")
  oligochaeta_kick <- get_kick_abundance(agg_site, "Oligochaeta")
  eristalini_kick <- get_kick_abundance(agg_site, "Eristalini")
  gammarus_kick <- get_kick_abundance(agg_site, "Gammarus")
  gammarus_hand <- sum(agg_site$abundance[agg_site$dsfi_taxon == "Gammarus" & agg_site$method == "hand"], na.rm = TRUE)

  # ----- IG 1 -----
  if (start_ig <= 1) {
  ig1_taxa <- c("Brachyptera", "Capnia", "Leuctra", "Isogenus", "Isoperla",
                "Isoptena", "Perlodes", "Protonemura", "Siphonoperla",
                "EPHEMERIDAE", "Limnius", "GLOSSOSOMATIDAE", "SERICOSTOMATIDAE")
  ig1_n <- count_present(ig1_taxa)

  if (ig1_n >= 1) {
    return(list(ig = 1, n_taxa = ig1_n))
  }
  }

  # ----- IG 2 -----
  ig2_taxa <- c("Amphinemura", "Taeniopteryx", "AMETROPODIDAE",
                "EPHEMERELLIDAE", "HEPTAGENIIDAE", "LEPTOPHLEBIIDAE",
                "SIPHLONURIDAE", "Elmis", "Elodes",
                "RHYACOPHILIDAE", "GOERIDAE", "Ancylus")
  ig2_n <- count_present(ig2_taxa)

  if (start_ig <= 2) {
  # Preclude: if Asellus >= 5 in kick, skip to IG 3
  # Preclude: if Chironomus >= 5 in kick, skip to IG 4
  if (ig2_n >= 1) {
    if (asellus_kick >= 5) {
      # Fall through to IG 3
    } else if (chironomus_kick >= 5) {
      # Fall through to IG 4
    } else {
      return(list(ig = 2, n_taxa = ig2_n))
    }
  }
  }

  # ----- IG 3 -----
  # Gammarus >= 10, Caenidae, Other Trichoptera >= 5
  # "Other Trichoptera" = all Trichoptera families used in DSFI except those
  # already used as specific indicators (i.e., caseless + case-bearing families)
  # Per the paper, this means any Trichoptera family in Table 1

  # Check Gammarus >= 10 (kick threshold is 10 for IG 3)
  gammarus_present_ig3 <- (gammarus_kick >= 10) | (gammarus_hand >= 1 & (gammarus_kick + gammarus_hand) >= 10)

  # Caenidae standard presence
  caenidae_present <- check_present(agg_site, "CAENIDAE")

  # Other Trichoptera >= 5 in kick
  all_trich_families <- c("ECNOMIDAE", "HYDROPSYCHIDAE", "PHILOPOTAMIDAE",
                          "POLYCENTROPODIDAE", "PSYCHOMYIIDAE", "RHYACOPHILIDAE",
                          "BERAEIDAE", "BRACHYCENTRIDAE", "HYDROPTILIDAE",
                          "GOERIDAE", "GLOSSOSOMATIDAE", "LEPTOCERIDAE",
                          "LEPIDOSTOMATIDAE", "LIMNEPHILIDAE", "MOLANNIDAE",
                          "ODONTOCERIDAE", "PHRYGANEIDAE", "SERICOSTOMATIDAE")
  other_trich_kick <- sum(agg_site$abundance[agg_site$dsfi_taxon %in% all_trich_families & agg_site$method == "kick"], na.rm = TRUE)
  other_trich_hand <- sum(agg_site$abundance[agg_site$dsfi_taxon %in% all_trich_families & agg_site$method == "hand"], na.rm = TRUE)
  other_trich_present <- (other_trich_kick >= 5) | (other_trich_hand >= 1)

  ig3_present <- sum(c(gammarus_present_ig3, caenidae_present, other_trich_present))

  if (start_ig <= 3) {
  # Preclude: if Chironomus >= 5, skip to IG 4
  if (ig3_present >= 1) {
    if (chironomus_kick >= 5) {
      # Fall through to IG 4
    } else {
      return(list(ig = 3, n_taxa = ig3_present))
    }
  }
  }

  # ----- IG 4 -----
  if (start_ig <= 4) {
  # Gammarus >= 10, Asellus, Caenidae, Sialis, Other Trichoptera
  gammarus_present_ig4 <- (gammarus_kick >= 10) | (gammarus_hand >= 1 & (gammarus_kick + gammarus_hand) >= 10)
  asellus_present <- check_present(agg_site, "Asellus")
  sialis_present <- check_present(agg_site, "Sialis")
  other_trich_present_ig4 <- check_present(agg_site, "HYDROPSYCHIDAE") |
    check_present(agg_site, "POLYCENTROPODIDAE") |
    check_present(agg_site, "PSYCHOMYIIDAE") |
    check_present(agg_site, "LIMNEPHILIDAE") |
    check_present(agg_site, "LEPTOCERIDAE") |
    any(sapply(all_trich_families, function(f) check_present(agg_site, f)))

  ig4_taxa_present <- sum(c(gammarus_present_ig4, asellus_present,
                            caenidae_present, sialis_present,
                            other_trich_present_ig4))

  if (ig4_taxa_present >= 1) {
    return(list(ig = 4, n_taxa = ig4_taxa_present))
  }
  }

  # ----- IG 5 -----
  if (start_ig <= 5) {
  # Gammarus < 10, Baetidae, Simuliidae >= 25
  gammarus_present_ig5 <- check_present(agg_site, "Gammarus") &
    !((gammarus_kick >= 10) | (gammarus_hand >= 1 & (gammarus_kick + gammarus_hand) >= 10))

  baetidae_present <- check_present(agg_site, "BAETIDAE")

  simuliidae_kick <- get_kick_abundance(agg_site, "Simuliidae")
  simuliidae_hand <- sum(agg_site$abundance[agg_site$dsfi_taxon == "Simuliidae" & agg_site$method == "hand"], na.rm = TRUE)
  simuliidae_present <- (simuliidae_kick >= 25) | (simuliidae_hand >= 1)

  ig5_taxa_present <- sum(c(gammarus_present_ig5, baetidae_present, simuliidae_present))

  # Preclude: if Eristalini >= 2, go to IG 6
  if (eristalini_kick >= 2) {
    # Fall through to IG 6
  } else if (ig5_taxa_present >= 1) {
    # Check Oligochaeta >= 100 modifier
    oligo_high <- oligochaeta_kick >= 100
    return(list(ig = 5, n_taxa = ig5_taxa_present, oligo_high = oligo_high))
  }
  }

  # ----- IG 6 -----
  # Tubificidae, Psychodidae, Chironomidae, Eristalini
  # Any of these present = IG 6
  ig6_present <- check_present(agg_site, "Oligochaeta") |  # Tubificidae mapped as Oligochaeta
    check_present(agg_site, "Psychodidae") |
    check_present(agg_site, "Chironomidae") |
    check_present(agg_site, "Chironomus") |
    check_present(agg_site, "Eristalini")

  if (ig6_present) {
    return(list(ig = 6, n_taxa = 1))
  }

  # If nothing matches, return IG 6 with value 1
  return(list(ig = 6, n_taxa = 0))
}


# =============================================================================
# STEP 6: Look up DSFI index value from Table 2
# =============================================================================

lookup_dsfi <- function(ig, n_taxa, div_net, oligo_high = FALSE) {

  # Determine diversity group column
  if (div_net <= -2) {
    div_col <- 1
  } else if (div_net >= -1 & div_net <= 3) {
    div_col <- 2
  } else if (div_net >= 4 & div_net <= 9) {
    div_col <- 3
  } else {
    div_col <- 4  # >= 10
  }

  # Table 2 lookup matrix
  # Rows: IG1_upper, IG1_lower, IG2, IG3, IG4_upper, IG4_lower,
  #        IG5_upper, IG5_lower, IG6
  # Columns: <=−2, −1 to 3, 4 to 9, >=10
  # NA means that combination does not occur (shown as "-" in paper)

  lookup <- matrix(c(
    NA,  5,  6,  7,   # IG 1, >= 2 taxa
    NA,  4,  5,  6,   # IG 1, 1 taxon
     4,  4,  5,  5,   # IG 2
     3,  4,  4,  4,   # IG 3
     3,  3,  4, NA,   # IG 4, >= 2 taxa
     2,  3,  3, NA,   # IG 4, 1 taxon
     2,  3,  3, NA,   # IG 5, >= 2 taxa
     2,  2,  3, NA,   # IG 5, 1 taxon or Oligo >= 100
     1,  1, NA, NA    # IG 6
  ), nrow = 9, ncol = 4, byrow = TRUE)

  # Determine row
  if (ig == 1) {
    row_idx <- ifelse(n_taxa >= 2, 1, 2)
  } else if (ig == 2) {
    row_idx <- 3
  } else if (ig == 3) {
    row_idx <- 4
  } else if (ig == 4) {
    row_idx <- ifelse(n_taxa >= 2, 5, 6)
  } else if (ig == 5) {
    if (oligo_high) {
      row_idx <- 8  # lower row
    } else {
      row_idx <- ifelse(n_taxa >= 2, 7, 8)
    }
  } else {
    row_idx <- 9
  }

  dsfi_value <- lookup[row_idx, div_col]

  return(dsfi_value)
}


# =============================================================================
# STEP 7: Main DSFI calculation function
# =============================================================================

calculate_dsfi <- function(dat) {

  # Aggregate data to DSFI taxon level
  # Returns filtered aggregation (for indicator groups) with unfiltered
  # aggregation stored as an attribute (for diversity groups)
  agg <- aggregate_for_dsfi(dat)
  agg_unfilt <- attr(agg, "unfiltered")

  # Get unique site codes
  sites <- unique(agg$site_code)

  # Calculate DSFI for each site
  results <- data.frame(
    site_code = character(),
    indicator_group = integer(),
    ig_n_taxa = integer(),
    positive_div = integer(),
    negative_div = integer(),
    net_diversity = integer(),
    dsfi_value = integer(),
    stringsAsFactors = FALSE
  )

  for (s in sites) {
    agg_site <- agg[agg$site_code == s, ]
    agg_site_unfilt <- agg_unfilt[agg_unfilt$site_code == s, ]

    # Diversity groups (uses UNFILTERED data - abundance independent)
    div <- calc_diversity_groups(agg_site_unfilt)

    # Indicator group with cascading rule:
    # If the initial IG + diversity combination is undefined (NA in Table 2),
    # move to the next lower indicator group and try again.
    start_ig <- 1
    repeat {
      ig_result <- determine_ig(agg_site, start_ig = start_ig)
      oligo_high <- ifelse(!is.null(ig_result$oligo_high), ig_result$oligo_high, FALSE)
      dsfi_val <- lookup_dsfi(ig_result$ig, ig_result$n_taxa, div$net, oligo_high)

      if (!is.na(dsfi_val)) break  # valid combination found

      # Undefined combination — cascade to next IG below
      start_ig <- ig_result$ig + 1

      # Safety: if we've exhausted all IGs, assign DSFI = 1
      if (start_ig > 6) {
        dsfi_val <- 1
        break
      }
    }

    results <- rbind(results, data.frame(
      site_code = s,
      indicator_group = ig_result$ig,
      ig_n_taxa = ig_result$n_taxa,
      positive_div = div$positive,
      negative_div = div$negative,
      net_diversity = div$net,
      dsfi_value = dsfi_val,
      stringsAsFactors = FALSE
    ))
  }

  # v15: split site_code on "_" to expose site_id and year as separate columns.
  # Anything before the last "_" is site_id (handles codes like "LTRxxx_YYYY"
  # and unusual cases with extra underscores); anything after is year.
  results$site_id <- sub("_[^_]*$", "", results$site_code)
  results$year    <- suppressWarnings(as.integer(sub(".*_([^_]+)$", "\\1",
                                                     results$site_code)))
  results <- results[, c("site_code", "site_id", "year",
                         "indicator_group", "ig_n_taxa",
                         "positive_div", "negative_div",
                         "net_diversity", "dsfi_value")]

  return(results)
}


# =============================================================================
# STEP 8: Diagnostic function
# =============================================================================
# Prints detailed DSFI calculation steps for a single site or all sites.
# Use this to verify taxon mappings, presence checks, and index determination.
#
# Usage:
#   dsfi_diagnostic(dat)                    # all sites
#   dsfi_diagnostic(dat, "LTR1299_2022")    # single site

dsfi_diagnostic <- function(dat, site = NULL) {

  agg <- aggregate_for_dsfi(dat)
  agg_unfilt <- attr(agg, "unfiltered")

  if (!is.null(site)) {
    sites <- site
  } else {
    sites <- unique(agg$site_code)
  }

  for (s in sites) {

    agg_site <- agg[agg$site_code == s, ]
    agg_site_unfilt <- agg_unfilt[agg_unfilt$site_code == s, ]

    # ------------------------------------------------------------------
    # Section 1: Taxon mapping summary
    # ------------------------------------------------------------------
    message(paste0("\n", paste(rep("=", 70), collapse = "")))
    message(paste0("SITE: ", s))
    message(paste(rep("=", 70), collapse = ""))

    message("\n--- TAXON MAPPING ---")
    message(sprintf("%-35s %-8s %8s", "DSFI Taxon", "Method", "Abundance"))
    message(paste(rep("-", 55), collapse = ""))

    agg_sorted <- agg_site[order(agg_site$dsfi_taxon, agg_site$method), ]
    for (i in seq_len(nrow(agg_sorted))) {
      message(sprintf("%-35s %-8s %8d",
                      agg_sorted$dsfi_taxon[i],
                      agg_sorted$method[i],
                      agg_sorted$abundance[i]))
    }

    # Flag unmapped/other taxa for review
    other_taxa <- agg_sorted$dsfi_taxon[grepl("_other|UNMAPPED", agg_sorted$dsfi_taxon)]
    if (length(other_taxa) > 0) {
      message("\n  Note: The following taxa are not used in DSFI calculations:")
      message(paste0("    ", paste(unique(other_taxa), collapse = ", ")))
    }

    # ------------------------------------------------------------------
    # Section 2: Diversity groups detail (uses UNFILTERED data)
    # ------------------------------------------------------------------
    message("\n--- DIVERSITY GROUPS (Table 3, unfiltered abundances) ---")

    # Positive
    pos_taxa <- c()
    if (get_total_abundance(agg_site_unfilt, "Tricladida") >= 1)
      pos_taxa <- c(pos_taxa, "Tricladida")
    if (get_total_abundance(agg_site_unfilt, "Gammarus") >= 1)
      pos_taxa <- c(pos_taxa, "Gammarus")

    pleco_taxa_diag <- agg_site_unfilt$dsfi_taxon[grepl("^(Amphinemura|Brachyptera|Capnia|Isogenus|Isoperla|Isoptena|Leuctra|Nemoura|Nemurella|Perlodes|Protonemura|Siphonoperla|Taeniopteryx)$", agg_site_unfilt$dsfi_taxon) |
                                             grepl("_pleco_other$", agg_site_unfilt$dsfi_taxon)]
    pleco_genera_present <- unique(pleco_taxa_diag)
    for (g in pleco_genera_present) {
      if (get_total_abundance(agg_site_unfilt, g) >= 1) pos_taxa <- c(pos_taxa, g)
    }

    ephem_families <- c("AMETROPODIDAE", "BAETIDAE", "CAENIDAE", "EPHEMERIDAE",
                        "EPHEMERELLIDAE", "HEPTAGENIIDAE", "LEPTOPHLEBIIDAE",
                        "SIPHLONURIDAE")
    for (f in ephem_families) {
      if (get_total_abundance(agg_site_unfilt, f) >= 1) pos_taxa <- c(pos_taxa, f)
    }
    ephem_other_diag <- unique(agg_site_unfilt$dsfi_taxon[grepl("_ephem_other$", agg_site_unfilt$dsfi_taxon)])
    for (f in ephem_other_diag) {
      if (get_total_abundance(agg_site_unfilt, f) >= 1) pos_taxa <- c(pos_taxa, f)
    }

    for (t in c("Elmis", "Limnius", "Elodes")) {
      if (get_total_abundance(agg_site_unfilt, t) >= 1) pos_taxa <- c(pos_taxa, t)
    }
    if (get_total_abundance(agg_site_unfilt, "RHYACOPHILIDAE") >= 1)
      pos_taxa <- c(pos_taxa, "RHYACOPHILIDAE")

    cb_trich <- c("BERAEIDAE", "BRACHYCENTRIDAE", "HYDROPTILIDAE", "GOERIDAE",
                  "GLOSSOSOMATIDAE", "LEPTOCERIDAE", "LEPIDOSTOMATIDAE",
                  "LIMNEPHILIDAE", "MOLANNIDAE", "ODONTOCERIDAE",
                  "PHRYGANEIDAE", "SERICOSTOMATIDAE")
    for (f in cb_trich) {
      if (get_total_abundance(agg_site_unfilt, f) >= 1) pos_taxa <- c(pos_taxa, f)
    }
    casebearing_extra <- c("APATANIIDAE")
    cb_other_diag <- unique(agg_site_unfilt$dsfi_taxon[grepl(paste0("^(", paste(casebearing_extra, collapse = "|"), ")_trich_other$"), agg_site_unfilt$dsfi_taxon)])
    for (f in cb_other_diag) {
      if (get_total_abundance(agg_site_unfilt, f) >= 1) pos_taxa <- c(pos_taxa, f)
    }
    if (get_total_abundance(agg_site_unfilt, "Ancylus") >= 1)
      pos_taxa <- c(pos_taxa, "Ancylus")

    # Negative
    neg_taxa <- c()
    if (get_total_abundance(agg_site_unfilt, "Oligochaeta") >= 100)
      neg_taxa <- c(neg_taxa, "Oligochaeta (>=100)")
    for (t in c("Helobdella", "Erpobdella", "Asellus", "Sialis",
                "Psychodidae", "Chironomus", "Eristalini", "Sphaerium", "Lymnaea")) {
      if (get_total_abundance(agg_site_unfilt, t) >= 1) neg_taxa <- c(neg_taxa, t)
    }

    div <- calc_diversity_groups(agg_site_unfilt)

    message(sprintf("  Positive (%d): %s", div$positive,
                    ifelse(length(pos_taxa) > 0, paste(pos_taxa, collapse = ", "), "none")))
    message(sprintf("  Negative (%d): %s", div$negative,
                    ifelse(length(neg_taxa) > 0, paste(neg_taxa, collapse = ", "), "none")))
    message(sprintf("  Net diversity groups: %d - %d = %d",
                    div$positive, div$negative, div$net))

    # Diversity column label
    if (div$net <= -2) div_label <- "<= -2"
    else if (div$net <= 3) div_label <- "-1 to 3"
    else if (div$net <= 9) div_label <- "4 to 9"
    else div_label <- ">= 10"
    message(sprintf("  Diversity column: %s", div_label))

    # ------------------------------------------------------------------
    # Section 3: Indicator group walkthrough
    # ------------------------------------------------------------------
    message("\n--- INDICATOR GROUP DETERMINATION (Table 2) ---")

    # IG 1
    ig1_taxa <- c("Brachyptera", "Capnia", "Leuctra", "Isogenus", "Isoperla",
                  "Isoptena", "Perlodes", "Protonemura", "Siphonoperla",
                  "EPHEMERIDAE", "Limnius", "GLOSSOSOMATIDAE", "SERICOSTOMATIDAE")
    ig1_found <- ig1_taxa[sapply(ig1_taxa, function(t) check_present(agg_site, t))]
    message(sprintf("  IG 1: %d taxa present (%s)",
                    length(ig1_found),
                    ifelse(length(ig1_found) > 0, paste(ig1_found, collapse = ", "), "none")))

    # IG 2
    ig2_taxa <- c("Amphinemura", "Taeniopteryx", "AMETROPODIDAE",
                  "EPHEMERELLIDAE", "HEPTAGENIIDAE", "LEPTOPHLEBIIDAE",
                  "SIPHLONURIDAE", "Elmis", "Elodes",
                  "RHYACOPHILIDAE", "GOERIDAE", "Ancylus")
    ig2_found <- ig2_taxa[sapply(ig2_taxa, function(t) check_present(agg_site, t))]

    asellus_k <- get_kick_abundance(agg_site, "Asellus")
    chironomus_k <- get_kick_abundance(agg_site, "Chironomus")
    message(sprintf("  IG 2: %d taxa present (%s)",
                    length(ig2_found),
                    ifelse(length(ig2_found) > 0, paste(ig2_found, collapse = ", "), "none")))
    if (length(ig2_found) >= 1) {
      if (asellus_k >= 5)
        message(sprintf("    -> PRECLUDED: Asellus kick = %d (>=5), redirected to IG 3", asellus_k))
      if (chironomus_k >= 5)
        message(sprintf("    -> PRECLUDED: Chironomus kick = %d (>=5), redirected to IG 4", chironomus_k))
    }

    # IG 3
    gammarus_k <- get_kick_abundance(agg_site, "Gammarus")
    gammarus_h <- sum(agg_site$abundance[agg_site$dsfi_taxon == "Gammarus" &
                                           agg_site$method == "hand"], na.rm = TRUE)
    gammarus_ge10 <- (gammarus_k >= 10) |
      (gammarus_h >= 1 & (gammarus_k + gammarus_h) >= 10)
    caenidae_pres <- check_present(agg_site, "CAENIDAE")

    all_trich <- c("ECNOMIDAE", "HYDROPSYCHIDAE", "PHILOPOTAMIDAE",
                   "POLYCENTROPODIDAE", "PSYCHOMYIIDAE", "RHYACOPHILIDAE",
                   "BERAEIDAE", "BRACHYCENTRIDAE", "HYDROPTILIDAE",
                   "GOERIDAE", "GLOSSOSOMATIDAE", "LEPTOCERIDAE",
                   "LEPIDOSTOMATIDAE", "LIMNEPHILIDAE", "MOLANNIDAE",
                   "ODONTOCERIDAE", "PHRYGANEIDAE", "SERICOSTOMATIDAE")
    ot_kick <- sum(agg_site$abundance[agg_site$dsfi_taxon %in% all_trich &
                                        agg_site$method == "kick"], na.rm = TRUE)
    ot_hand <- sum(agg_site$abundance[agg_site$dsfi_taxon %in% all_trich &
                                        agg_site$method == "hand"], na.rm = TRUE)
    ot_pres <- (ot_kick >= 5) | (ot_hand >= 1)

    ig3_parts <- c()
    if (gammarus_ge10) ig3_parts <- c(ig3_parts,
                                      sprintf("Gammarus>=10 (kick=%d, hand=%d)", gammarus_k, gammarus_h))
    if (caenidae_pres) ig3_parts <- c(ig3_parts, "Caenidae")
    if (ot_pres) ig3_parts <- c(ig3_parts,
                                sprintf("Other Trichoptera (kick=%d, hand=%d)", ot_kick, ot_hand))

    message(sprintf("  IG 3: %d taxa present (%s)",
                    length(ig3_parts),
                    ifelse(length(ig3_parts) > 0, paste(ig3_parts, collapse = ", "), "none")))
    if (length(ig3_parts) >= 1 & chironomus_k >= 5) {
      message(sprintf("    -> PRECLUDED: Chironomus kick = %d (>=5), redirected to IG 4",
                      chironomus_k))
    }

    # IG 4
    asellus_pres <- check_present(agg_site, "Asellus")
    sialis_pres <- check_present(agg_site, "Sialis")
    ot_pres_ig4 <- any(sapply(all_trich, function(f) check_present(agg_site, f)))

    ig4_parts <- c()
    if (gammarus_ge10) ig4_parts <- c(ig4_parts, "Gammarus>=10")
    if (asellus_pres) ig4_parts <- c(ig4_parts, "Asellus")
    if (caenidae_pres) ig4_parts <- c(ig4_parts, "Caenidae")
    if (sialis_pres) ig4_parts <- c(ig4_parts, "Sialis")
    if (ot_pres_ig4) ig4_parts <- c(ig4_parts, "Other Trichoptera")

    message(sprintf("  IG 4: %d taxa present (%s)",
                    length(ig4_parts),
                    ifelse(length(ig4_parts) > 0, paste(ig4_parts, collapse = ", "), "none")))

    # IG 5
    gammarus_lt10 <- check_present(agg_site, "Gammarus") & !gammarus_ge10
    baetidae_pres <- check_present(agg_site, "BAETIDAE")
    sim_kick <- get_kick_abundance(agg_site, "Simuliidae")
    sim_hand <- sum(agg_site$abundance[agg_site$dsfi_taxon == "Simuliidae" &
                                         agg_site$method == "hand"], na.rm = TRUE)
    sim_pres <- (sim_kick >= 25) | (sim_hand >= 1)
    oligo_k <- get_kick_abundance(agg_site, "Oligochaeta")
    erist_k <- get_kick_abundance(agg_site, "Eristalini")

    ig5_parts <- c()
    if (gammarus_lt10) ig5_parts <- c(ig5_parts,
                                      sprintf("Gammarus<10 (kick=%d)", gammarus_k))
    if (baetidae_pres) ig5_parts <- c(ig5_parts, "Baetidae")
    if (sim_pres) ig5_parts <- c(ig5_parts,
                                 sprintf("Simuliidae>=25 (kick=%d)", sim_kick))

    message(sprintf("  IG 5: %d taxa present (%s)",
                    length(ig5_parts),
                    ifelse(length(ig5_parts) > 0, paste(ig5_parts, collapse = ", "), "none")))
    if (erist_k >= 2)
      message(sprintf("    -> PRECLUDED: Eristalini kick = %d (>=2), redirected to IG 6",
                      erist_k))
    if (oligo_k >= 100)
      message(sprintf("    -> Oligochaeta kick = %d (>=100): use lower row if entering IG 5",
                      oligo_k))

    # IG 6
    ig6_parts <- c()
    if (check_present(agg_site, "Oligochaeta")) ig6_parts <- c(ig6_parts, "Tubificidae/Oligochaeta")
    if (check_present(agg_site, "Psychodidae")) ig6_parts <- c(ig6_parts, "Psychodidae")
    if (check_present(agg_site, "Chironomidae")) ig6_parts <- c(ig6_parts, "Chironomidae")
    if (check_present(agg_site, "Chironomus")) ig6_parts <- c(ig6_parts, "Chironomus")
    if (check_present(agg_site, "Eristalini")) ig6_parts <- c(ig6_parts, "Eristalini")
    message(sprintf("  IG 6: %s",
                    ifelse(length(ig6_parts) > 0, paste(ig6_parts, collapse = ", "), "none")))

    # ------------------------------------------------------------------
    # Section 4: Final result (with cascading rule)
    # ------------------------------------------------------------------
    start_ig <- 1
    repeat {
      ig_result <- determine_ig(agg_site, start_ig = start_ig)
      oligo_high <- ifelse(!is.null(ig_result$oligo_high), ig_result$oligo_high, FALSE)
      dsfi_val <- lookup_dsfi(ig_result$ig, ig_result$n_taxa, div$net, oligo_high)

      if (!is.na(dsfi_val)) break

      message(sprintf("\n   IG %d with %s diversity is undefined in Table 2 -> cascade to IG %d",
                      ig_result$ig, div_label, ig_result$ig + 1))
      start_ig <- ig_result$ig + 1
      if (start_ig > 6) {
        dsfi_val <- 1
        break
      }
    }

    row_label <- ""
    if (ig_result$ig %in% c(1, 4, 5)) {
      row_label <- ifelse(ig_result$n_taxa >= 2,
                          " (>=2 taxa, upper row)", " (1 taxon, lower row)")
    }
    if (ig_result$ig == 5 & oligo_high) {
      row_label <- " (lower row, Oligochaeta >=100)"
    }

    message(sprintf("\n=> SELECTED: IG %d, %d taxa%s",
                    ig_result$ig, ig_result$n_taxa, row_label))
    message(sprintf("=> Diversity groups: %d (column: %s)", div$net, div_label))
    message(sprintf("=> DSFI INDEX VALUE: %d", dsfi_val))
    message(paste(rep("=", 70), collapse = ""))
  }

  invisible(NULL)
}


# =============================================================================
# STEP 9: Run on random data
# =============================================================================

# Read full dataset
dat_all <- read_excel("Inputs/All_sites_macroinvertebrate_data_long.xlsx",
                      sheet = "clean_macroinvertebrate_data",
                      guess_max = 2500)

# # Filter
# dat_all <- dat_all |>
#   filter(!grepl("EXCLUDE", OTL_note_dsfi))

dat_all <- dat_all |>
  filter(
    !grepl("NO MATCH", OTL_taxonname, ignore.case = TRUE),
    !grepl("EXCLUDE", OTL_note_dsfi, ignore.case = TRUE),
    grepl("^LTR", site_code),
    year >= 2014, year <= 2022
  )

# # Select 3 random sites and subset data
# set.seed(123)
# random_sites <- sample(unique(dat_all$site_code), 3)
# random_sites

# Select specific sites and subset data
selected_sites <- c("LTR379_2022", "LTR401_2020", "LTR70_2016")
dat_sample <- dat_all |>
  filter(site_code %in% selected_sites) # or random_sites
# Calculate DSFI
results_sample <- calculate_dsfi(dat_sample)
results_sample
# Run diagnostic
dsfi_diagnostic(dat_sample)

# LTR379_2022 – DSFI = 7 (11 positive – 1 negative = 10 positive – two indicator taxa –
#   Limnius volckmari -> 6, GLOSSOMATIDAE).
#
# LTR401_2020 – DSFI = 4 (5 positive – 4 negative = 1 positive – one indicator taxa
#   Ephemera vulgata -> 4).
#
# LTR70_2016 – DSFI = 5 (4 positive - 2 negative = 2 positive – one indicator taxa ->
#     Ephemera vulgata -> 4).

# Run diagnostic for a single site
# dsfi_diagnostic(dat_sample, "LTR70_2016")

# =============================================================================
# STEP 10: Run on all data
# =============================================================================

# dat_all <- dat_all |>
#   filter(modification_state == "N")

# dat_all <- dat_all |>
#   filter(!grepl("EXCLUDE", OTL_note_dsfi))

# dat_all <- dat_all |>
#   filter(modification_state == "N",
#          !grepl("EXCLUDE", OTL_note_dsfi))

# Calculate DSFI
results_all <- calculate_dsfi(dat_all)

write_xlsx(results_all, "Outputs/3_DSFI_results_all_sites.xlsx")

# v15: subset export for a specific site list, preserving the input order.
# Sites missing from results_all appear as NA rows (so the requester can see
# which were dropped, e.g. by the EXCLUDE filter or having no data).
subset_sites <- c(
  "LTR1013_2022", "LTR1027_2022", "LTR1070_2022", "LTR1076_2022",
  "LTR1101_2022", "LTR1105_2022", "LTR1192_2022", "LTR1299_2022",
  "LTR1300_2022", "LTR1304_2022", "LTR1320_2022", "LTR1339_2022",
  "LTR1349_2022", "LTR1378_2022", "LTR1379_2022", "LTR1385_2022",
  "LTR1420_2022", "LTR1447_2022", "LTR1449_2022", "LTR1459_2022",
  "LTR1461_2022", "LTR1466_2022", "LTR1467_2022", "LTR1472_2022",
  "LTR1482_2022", "LTR1486_2022", "LTR1488_2022", "LTR1492_2022",
  "LTR1495_2022", "LTR1501_2022", "LTR1502_2022", "LTR151_2022",
  "LTR1528_2022", "LTR1578_2022", "LTR1585_2022", "LTR1586_2022",
  "LTR1587_2022", "LTR1589_2022", "LTR1610_2022", "LTR1619_2022",
  "LTR162_2022",  "LTR1620_2022", "LTR1622_2022", "LTR1629_2022",
  "LTR163_2022",  "LTR1630_2022", "LTR1631_2022", "LTR1637_2022",
  "LTR1641_2022", "LTR1642_2022", "LTR1649_2022", "LTR1654_2022",
  "LTR1655_2022", "LTR1657_2022", "LTR1659_2022", "LTR1660_2022",
  "LTR1661_2022", "LTR1667_2022", "LTR1671_2022", "LTR1674_2022",
  "LTR1680_2022", "LTR1683_2022", "LTR1684_2022", "LTR1687_2022",
  "LTR1688_2022", "LTR218_2022",  "LTR219_2022",  "LTR232_2022",
  "LTR236_2022",  "LTR239_2022",  "LTR282_2022",  "LTR301_2022",
  "LTR331_2022",  "LTR337_2022",  "LTR351_2022",  "LTR360_2022",
  "LTR369_2022",  "LTR386_2022",  "LTR393_2022",  "LTR411_2022",
  "LTR428_2022",  "LTR433_2022",  "LTR463_2022",  "LTR484_2022",
  "LTR491_2022",  "LTR521_2022",  "LTR577_2022",  "LTR623_2022",
  "LTR625_2022",  "LTR703_2022",  "LTR709_2022",  "LTR726_2022",
  "LTR750_2022",  "LTR775_2022",  "LTR776_2022",  "LTR945_2022",
  "LTR963_2022"
)

results_subset <- results_all[match(subset_sites, results_all$site_code), ]
results_subset$site_code <- subset_sites  # keep order even for missing rows

missing_sites <- subset_sites[is.na(results_subset$dsfi_value)]
if (length(missing_sites) > 0) {
  message(sprintf("DSFI subset export: %d site(s) had no DSFI result and appear as NA: %s",
                  length(missing_sites), paste(missing_sites, collapse = ", ")))
}

write_xlsx(results_subset, "Outputs/3_DSFI_results_subset.xlsx")

# # Check problematic sites
# problem_sites <- results_all[results_all$net_diversity <= -2 & results_all$indicator_group == 1, ]
# problem_sites
#
# # Run diagnostic for a single site
# dsfi_diagnostic(dat_all, problem_sites[1,1])
#
# # Run diagnostic on problematic sites only
# dsfi_diagnostic(dat_all[dat_all$site_code %in% problem_sites$site_code, ])
#
# # # Run diagnostic
# # dsfi_diagnostic(dat_all)
#
# # output <- capture.output(dsfi_diagnostic(dat_all), type = "message")
# # writeLines(output, "DSFI calulcator/DSFI_diagnostic_output.txt")
#
# dsfi_diagnostic(dat_all, "LTR151_2022")
# dsfi_diagnostic(dat_all, "LTR1528_2022")
# dsfi_diagnostic(dat_all, "LTR1587_2022")
# dsfi_diagnostic(dat_all, "LTR1630_2022")
# dsfi_diagnostic(dat_all, "LTR1683_2022")
#
# # =============================================================================
# # STEP 11: Check problematic sites manually
# # =============================================================================
#
# dat_all <- read_excel("Input data/4_clean_macroinvertebrate_data_long.xlsx",
#                       sheet = "clean_macroinvertebrate_data",
#                       guess_max = 2500)
#
# # Problematic sites
# problem_sites <- c("LTR1482_2002", "LTR151_2022", "LTR1528_2022",
#                    "LTR1587_2022", "LTR1630_2022","LTR1683_2022",
#                    "LTR1684_2022", "LTR236_2022", "LTR337_2022",
#                    "LTR428_2022", "LTR623_2022", "LTR709_2022",
#                    "LTR726_2022", "LTR750_2022")
#
# dat_problem <- dat_all |>
#   filter(site_code %in% problem_sites)
#
# write_xlsx(dat_problem, "DSFI calulcator/Problematic_sites_data.xlsx")

#==================== CLEAN UP WORKSPACE =====================
library(pacman)
rm(list = ls())       # Remove all objects from environment
gc()                  # Frees up unused memory
p_unload(all)         # Unload all loaded packages
graphics.off()        # Close all graphical devices
cat("\014")           # Clear the console
# Clear mind :)

