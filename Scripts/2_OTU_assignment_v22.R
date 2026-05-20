# ============================================================================
# OTU Assignment Script — Lithuanian Macroinvertebrate Biomonitoring OTL v22
# ============================================================================
#
# This script implements the OTL Decision Tree v16 for assigning Operational
# Taxonomic Units (OTUs) to Lithuanian aquatic macroinvertebrate species.
# (No semantic rule changes from v16 — same decision-tree logic.)
#
# Changes from v20:
#   - Output column reorder: the script-derived OTL_id (added in v19 /
#     v20) is now the SECOND column in OTL_final_with_OTU, qa_otl_final
#     and hierarchical_OTL — right after sort_id. Previously OTL_id sat
#     in the middle of the OTL_final_with_OTU row, which made it easy to
#     miss when scanning. Position is the only thing changing here; the
#     values are unchanged.
#
# Changes from v19:
#   - BUG FIX: hierarchical_OTL `gen_entries` was pulling distinct
#     (phylum, class, ..., base_genus) combinations across the ENTIRE
#     taxalist, including phylum-, class- and order-level rows. For
#     those rows the script's `base_genus` (first word of validated_name)
#     is actually the higher-rank name (e.g. "Platyhelminthes" for the
#     phylum entry). The result was bogus "<Higher-rank> sp." genus
#     fallbacks sitting alongside the correct "<Higher-rank> Gen. sp."
#     entries (e.g. "Platyhelminthes sp." vs "Platyhelminthes Gen. sp.").
#     Fix: restrict gen_entries to entry_type in {species, genus} so
#     genus fallbacks are only generated from rows where a genus is
#     a meaningful concept.
#   - OTL_id (from hierarchical_OTL) is now joined into otl_output and
#     qa_otl_final via the OTL_name_script -> OTU_name match. Every
#     row in those two sheets carries an OTL_id, including rows that
#     have a blank manual OTL_name in OTL_final (curation-pending).
#   - qa_otl_final filter relaxed: blank-manual rows are no longer
#     hidden. They appear with OTL_match = FALSE and a populated
#     OTL_id. Console reports three counts now: match, real mismatch,
#     manual-blank (script-only).
#
# Changes from v18:
#   - Output column rework around exclusions. Two new boolean columns
#     are derived from the manual flags in OTL_final:
#         excluded_from_DSFI    = (dsfi_exclusion  == "EXCLUDE")
#         excluded_from_indices = (index_exclusion == "EXCLUDE")
#     These are the authoritative per-row flags for downstream filtering.
#   - The script-derived "excluded_from_EQR" is retained for QA but is
#     renamed in the output to `excluded_from_EQR_script` so its origin
#     (decision-tree derivation, not the author's manual curation) is
#     unambiguous. The hierarchical_OTL sheet — which has no manual
#     analogue — keeps the original column name unchanged.
#   - hierarchical_OTL gains an `OTL_id` column: a sequential integer
#     assigned per UNIQUE OTU_name (rather than per row). Rows that
#     share an OTU_name (e.g. multiple rows collapsing to
#     "Polychaeta Gen. sp.") share the same OTL_id. IDs are issued in
#     order of first appearance after the complexity sort. Sits as the
#     second column of the sheet, right after sort_id.
#   - summary sheet gains three new categories that count distinct
#     OTU_names within each taxonomic group:
#         "OTU counts by phylum"   — one row per phylum
#         "OTU counts by class"    — one row per class (parent phylum noted)
#         "OTU counts by order"    — one row per order (parent class noted)
#     Counts use n_distinct(OTU_name) so OTUs that span multiple rows
#     within a group are counted once.
#
# Changes from v17:
#   - Input file renamed: Inputs/LT_operational_taxalist.xlsx
#     (was LT_operational_taxalist_testing.xlsx).
#   - Sheet name "excluded_taxa" -> "Excluded_taxa" (capitalised).
#   - The input OTL_final sheet now carries the author's MANUAL OTU
#     assignments in the columns OTL_name, OTL-ID, OTL-level. The
#     script derives its own OTUs from the decision tree (independent
#     of these columns) and then joins the manual values back in for
#     a QA comparison sheet so any drift between manual and derived
#     can be reviewed row-by-row.
#   - The input now also carries per-row exclusion flags:
#       dsfi_exclusion   — "EXCLUDE" if the row is dropped before DSFI
#       index_exclusion  — "EXCLUDE" if the row is dropped before LRMI
#                          metrics other than DSFI
#     These supersede the old "Included in EQR calculation" column.
#     The script preserves these flags in its output for downstream
#     scripts (4, 5, 6) to filter on.
#   - Specialist_taxalist now also carries 100%_agreement,
#     100%_proposed_lvl, 100%_proposed_name and 100%_proposed_OTL_name.
#     The script preserves and compares against these in the QA sheet.
#
# Changes from v16:
#   - DATA-QUALITY GUARD: After reading Specialist_taxalist, OTL_final, and
#     excluded_taxa, the script now checks for cross-sheet name mismatches
#     and prints any orphans. This catches typos such as
#     "Aelosoma hemprichi" (Specialist_taxalist) vs "Aeolosoma hemprichii"
#     (excluded_taxa / OTL_final), which would otherwise silently leave
#     `directly_excluded = FALSE` and cascade into wrong group exclusions.
#
# Changes from v15:
#   - BUG FIX: Order-of-operations in `excl_family_explicit` filter. The v15
#     filter referenced spec$directly_excluded before that column was created,
#     so `all(NULL)` evaluated TRUE and no candidate families were ever
#     filtered out as "mixed". Fix: compute `directly_excluded` immediately
#     after reading the excluded_taxa sheet, before the explicit-family filter.
#   - BUG FIX: `forced_keeps_eqr` could become NA for taxa whose `subclass`
#     and `order` were both NA (e.g. Stenostomidae rows), because
#     `NA == "OLIGOCHAETA"` is NA. NA then propagated into `excluded_from_EQR`.
#     Fix: guard the comparisons with `!is.na(...)`.
#   - Lepidoptera Crambidae genera comment in the v15 header was out of sync
#     with the .md spec (listed Donacaula and Nymphula, which the .md does not).
#     The script does not hard-code these genera, so the change is comment-only.
#
# Changes from v14.1:
#   - BMWP: Pontogammarus robustoides and Obesogammarus crassus removed as
#     individual species-level entries. Pontogammaridae added as family-level
#     entry (score 6). The species-level min_level exceptions are removed.
#   - Metric group "Crustacea" renamed to "Malacostraca" throughout.
#
# Changes from v13 (carried over from v14):
#   - Decision tree restructured for clarity (no code logic changes).
#   - Phase 2 now consolidates all forced assignments in one section.
#   - Key principles removed from decision tree documentation.
#   - Version annotations and "German OTL" references removed from docs.
#
# Changes from v12:
#   - Oligochaeta order-level entries (Crassiclitellata, Enchytraeida,
#     Lumbriculida, Tubificida) now forced to SUBCLASS level -> "Oligochaeta
#     Gen. sp." (NOT excluded from EQR). Previously received order-level OTUs.
#   - Neuroptera: all species, genus, and family entries forced to FAMILY level.
#     Specialist agreement at order was too coarse. Neuroptera is NOT excluded
#     from EQR. Two families: Osmylidae and Sisyridae.
#
# Changes from v11 (carried over from v12):
#   - GENERAL RULE: Excluded taxa not in entirely excluded groups are now
#     forced to FAMILY level (not order). Replaces hardcoded per-group
#     forced overrides. Affects Talitridae, non-Asellidae Isopoda,
#     Neomysis/Praunus, Crangonidae/Palaemonidae, Hemiptera excluded
#     families, and all other individually excluded families/species.
#   - Lepidoptera: no longer entirely excluded. Freshwater Crambidae species
#     (Acentria, Cataclysta, Elophila, Parapoynx, Schoenobius — see .md spec)
#     follow Phase 3/4. Lepidoptera added as metric group
#     with minimum = family. Excluded Lepidoptera get family-level OTU.
#   - Neuroptera: no longer entirely excluded. Osmylidae and Sisyridae are
#     aquatic. v13: forced to family level (see above).
#   - Clambidae: removed from excluded_taxa (has BMWP score 5). Now
#     receives normal Coleoptera family-level OTU.
#   - Stenostomidae (Platyhelminthes): explicit forced override to PHYLUM
#     level. Order is taxonomically unavailable for this family.
#   - Fixed excl_family_explicit: mixed families (with both excluded and
#     non-excluded species) are no longer treated as entirely excluded.
#   - Fixed excl_order_explicit: orders with non-excluded species are no
#     longer treated as entirely excluded (replaces has_metric check).
#
# Changes from v10 (carried over):
#   - Oligochaeta split: Naididae -> family; others -> subclass (NOT excl)
#   - Subspecies fallback from parent species
#
# Decision tree phases:
#   Phase 1: Data preparation (read, clean, read excluded_taxa, flag exclusions)
#   Phase 1.8: Forced-level overrides:
#              - Oligochaeta: Naididae -> family, others -> subclass (not excl)
#              - Stenostomidae -> phylum (excluded)
#              - Neuroptera -> family (v13, not excluded from EQR)
#              - General rule: excluded & not entirely excluded -> family
#   Phase 2: Special group routing (defines metric minimums by group)
#   Phase 3: 100% specialist agreement (species-level entries only)
#   Phase 4: Metric compliance check (override if Phase 3 is too coarse)
#   Excluded group override: entirely excluded groups -> OTU at group level
#   Genus entries: metric-minimum-aware, capped at genus (non-excluded only)
#   Family and higher-level entries: always at their own level (non-excluded)
#
# Input:  LT_operational_taxalist_testing.xlsx
#         - Specialist_taxalist sheet (species assessments)
#         - OTL_final sheet (full taxalist)
#         - excluded_taxa sheet (EQR exclusion source of truth)
# Output: OTU_assignments_v15.xlsx
#
# Key decisions (documented in OTL_decision_tree_v15_machine_readable.md):
#   - Naididae (formerly Tubificidae) at family; other Oligochaeta at subclass
#   - Malacostraca metric group (was "Crustacea"); MAXILLOPODA handled separately
#   - Eristalinae genera identified from OTL_final subfamily column
#   - HEMIPTERA used as modern equivalent of Heteroptera
#   - No sole-representative simplification (direct assignment only)
#   - Genus entries: metric-minimum-aware, capped at genus
#   - Family and higher entries always at their own taxonomic level
#   - excluded_taxa sheet determines EQR exclusions
#   - Entirely excluded groups collapse OTU to group level
#   - Truly aquatic groups keep order+ entries for EQR
#   - General exclusion rule: excluded but not entirely excluded -> family
#   - Stenostomidae forced to phylum (missing order taxonomy)
#   - Neuroptera forced to family (specialist order too coarse; v13)
#   - Oligochaeta order entries forced to subclass (v13)
#   - Lepidoptera metric group with family minimum
#   - Genus entries for non-Gammaridae Malacostraca resolve to family via
#     taxonomy-based min_level
#   - Pontogammaridae at family (BMWP score 6); no species-level exceptions
# ============================================================================

# ---- Load packages ----

required_pkgs <- c("readxl", "dplyr", "stringr", "writexl")
missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs)
}

library(readxl)
library(dplyr)
library(stringr)
library(writexl)

# ---- Configuration ----

input_file  <- "Inputs/LT_operational_taxalist.xlsx"
output_file <- "Outputs/2_OTU_assignments_v22.xlsx"

# Taxonomic level codes and their rank (1 = finest, 8 = coarsest)
LEVEL_RANK <- c(s = 1, g = 2, sf = 3, f = 4, o = 5, sc = 6, c = 7, p = 8)

# Full label for each level code
LEVEL_LABEL <- c(
  s = "species", g = "genus", sf = "subfamily", f = "family",
  o = "order", sc = "subclass", c = "class", p = "phylum"
)

# Helper: convert "NAIDIDAE" -> "Naididae"
to_proper <- function(x) str_to_title(x)

# Truly aquatic groups: only these keep order+ entries for EQR calculations.
# All other groups' order+ entries are excluded because they could include
# terrestrial representatives.
TRULY_AQUATIC_CLASSES <- c("BIVALVIA")
TRULY_AQUATIC_ORDERS  <- c("PLECOPTERA", "EPHEMEROPTERA", "ODONATA",
                            "MEGALOPTERA", "TRICHOPTERA")


# ============================================================================
# PHASE 1: DATA PREPARATION
# ============================================================================

cat("=== PHASE 1: Data preparation ===\n")

# ---- 1.1 Read specialist assessments ----
spec <- read_excel(input_file, sheet = "Specialist_taxalist", guess_max = 10000)

# Rename columns with special characters
spec <- spec %>%
  rename(validated_name = `validated_name_GBIF+Molluscabase`)

# ---- 1.2 Remove duplicate rows ----
n_before <- nrow(spec)
spec <- spec %>% distinct(validated_name, .keep_all = TRUE)
cat(sprintf("  Removed %d duplicate rows (%d -> %d unique taxa)\n",
            n_before - nrow(spec), n_before, nrow(spec)))

# ---- 1.3 Read OTL_final and identify Eristalinae genera ----
# Note: guess_max must be large enough to detect the subfamily column,
# which has values only in rows ~2394+ (readxl defaults to guessing from first 1000 rows)
otl_final_raw <- read_excel(input_file, sheet = "OTL_final", guess_max = 10000)
eristalinae_genera <- otl_final_raw %>%
  filter(subfamily == "Eristalinae") %>%
  pull(`validated_name_GBIF+Molluscabase`) %>%
  str_extract("^[^\\s]+") %>%
  unique()

cat(sprintf("  Identified %d Eristalinae genera from OTL_final\n",
            length(eristalinae_genera)))

# ---- 1.4 Extract base genus (strip subgeneric notation) ----
# e.g., "Aedes (Ochlerotatus)" -> "Aedes"
spec <- spec %>%
  mutate(base_genus = str_extract(genus, "^[^\\s(]+"))

# ---- 1.5 Read excluded_taxa sheet ----
cat("\n  --- Reading excluded_taxa sheet ---\n")
excl_raw <- read_excel(input_file, sheet = "Excluded_taxa", guess_max = 10000)
excl_raw <- excl_raw %>%
  rename(excl_validated = `validated_name_GBIF+Molluscabase`)

cat(sprintf("  excluded_taxa: %d entries\n", nrow(excl_raw)))

# All excluded validated names (for direct species matching)
excl_name_set <- unique(excl_raw$excl_validated)

# ---- v17: Data-quality guard ----
# Cross-check validated names across the three sheets. Mismatches typically
# indicate spelling errors (e.g. v16 surfaced "Aelosoma hemprichi" in
# Specialist_taxalist vs "Aeolosoma hemprichii" in excluded_taxa / OTL_final),
# which silently break exclusion logic. Reports orphans but does not stop.

cat("\n  --- Data-quality guard: cross-sheet name consistency ---\n")

otl_name_set <- unique(otl_final_raw$`validated_name_GBIF+Molluscabase`)

# (a) Specialist_taxalist names absent from OTL_final
spec_orphans <- setdiff(spec$validated_name, otl_name_set)
if (length(spec_orphans) > 0) {
  cat(sprintf("  WARNING: %d Specialist_taxalist name(s) not found in OTL_final:\n",
              length(spec_orphans)))
  for (nm in spec_orphans) cat(sprintf("    - %s\n", nm))
} else {
  cat("  OK: every Specialist_taxalist name is present in OTL_final.\n")
}

# (b) Species-level excluded_taxa names (multi-word) absent from OTL_final.
#     Higher-level exclusion entries (single-word: family, order, class names)
#     are not expected to match a species; skip them.
excl_species_names <- excl_raw$excl_validated[grepl(" ", excl_raw$excl_validated)]
excl_orphans <- setdiff(unique(excl_species_names), otl_name_set)
if (length(excl_orphans) > 0) {
  cat(sprintf("  WARNING: %d species-level excluded_taxa name(s) not found in OTL_final:\n",
              length(excl_orphans)))
  for (nm in excl_orphans) cat(sprintf("    - %s\n", nm))
} else {
  cat("  OK: every species-level excluded_taxa name is present in OTL_final.\n")
}

# v16: flag directly-excluded species BEFORE the explicit-family filter below,
# which depends on this column. In v15 the column was created later (Step 1.6,
# Step 1), so `all(spec$directly_excluded)` reduced to `all(NULL) == TRUE` and
# the "mixed family" filter never removed anything.
spec <- spec %>%
  mutate(directly_excluded = validated_name %in% excl_name_set)

# Families explicitly excluded: the family name itself appears as validated_name
# in excluded_taxa (e.g., "Gerridae" with family=GERRIDAE).
# v12 FIX: Filter out mixed families that have non-excluded species in the
# Specialist_taxalist (e.g., Crambidae has freshwater species not in
# excluded_taxa). Only families where ALL species are also excluded count.
excl_family_explicit_raw <- excl_raw %>%
  filter(!is.na(family) & toupper(excl_validated) == family) %>%
  pull(family) %>% unique()

# Filter: keep only families where all species in specialist list are excluded
excl_family_explicit <- excl_family_explicit_raw[
  sapply(excl_family_explicit_raw, function(fam) {
    fam_species <- spec %>% filter(family == fam)
    # If no species in specialist list, trust the explicit exclusion
    nrow(fam_species) == 0 || all(fam_species$directly_excluded)
  })
]

cat(sprintf("  Explicitly excluded families: %d (of %d candidates, %d removed as mixed)\n",
            length(excl_family_explicit),
            length(excl_family_explicit_raw),
            length(excl_family_explicit_raw) - length(excl_family_explicit)))

# ---- 1.6 Determine entirely excluded groups (bottom-up) ----
# Strategy: flag each species in the specialist list as directly excluded
# (its name appears in excluded_taxa). Then determine which families have
# ALL their species excluded. Groups (orders/classes/phyla) are entirely
# excluded if ALL their families are entirely excluded.
#
# This avoids false positives from families that merely contain some excluded
# species (e.g., MYSIDAE has 4 excluded species but 6 total).

cat("\n  --- Determining entirely excluded groups ---\n")

# Step 1: directly-excluded flag was set above (v16, before explicit-family filter)
cat(sprintf("  Species directly in excluded_taxa: %d / %d\n",
            sum(spec$directly_excluded), nrow(spec)))

# Step 2: Determine entirely excluded families
# A family is entirely excluded if:
#   (a) ALL its species in the specialist list are directly excluded, OR
#   (b) The family name itself appears as validated_name in excluded_taxa
family_excl_check <- spec %>%
  group_by(family) %>%
  summarise(n_total = n(), n_excl = sum(directly_excluded),
            all_excl = n_excl == n_total, .groups = "drop")
families_all_species_excl <- family_excl_check$family[family_excl_check$all_excl]

excl_family_pool <- unique(c(families_all_species_excl, excl_family_explicit))

cat(sprintf("  Entirely excluded families: %d (by species: %d, by name: %d)\n",
            length(excl_family_pool),
            length(families_all_species_excl),
            length(excl_family_explicit)))

# Step 3: Determine entirely excluded orders
# An order is entirely excluded if ALL its families (in the specialist list)
# are in excl_family_pool. Filter out orders already within entirely excluded
# classes/phyla (those are already covered).
order_fam <- spec %>%
  filter(!is.na(order)) %>%
  distinct(order, family) %>%
  mutate(fam_excl = family %in% excl_family_pool)

orders_all_fam_excl <- order_fam %>%
  group_by(order) %>%
  summarise(all_excl = all(fam_excl), .groups = "drop") %>%
  filter(all_excl) %>% pull(order)

# Also check for orders with no species in specialist list but with an
# explicit order-level excluded_taxa entry AND no metric-relevant species.
# (These are orders where the order name appears as validated_name and
# there are no species that would participate in any metric group.)
excl_order_explicit <- excl_raw %>%
  filter(!is.na(order) & toupper(excl_validated) == order) %>%
  pull(order) %>% unique()

# v12 FIX: For explicit orders not already captured, check if ALL their
# species in the specialist list are directly excluded. If any species is
# NOT excluded, the order is NOT entirely excluded (even if no metric group
# applies). This fixes Neuroptera (Osmylidae, Sisyridae are aquatic) and
# Lepidoptera (freshwater Crambidae species are included).
for (ord in setdiff(excl_order_explicit, orders_all_fam_excl)) {
  spec_in_order <- spec %>% filter(order == ord)
  if (nrow(spec_in_order) == 0) {
    # No species in specialist list -> trust the explicit exclusion
    orders_all_fam_excl <- c(orders_all_fam_excl, ord)
  } else {
    # Has species: only entirely exclude if ALL are directly excluded
    all_excluded <- all(spec_in_order$directly_excluded)
    if (all_excluded) {
      orders_all_fam_excl <- c(orders_all_fam_excl, ord)
    }
  }
}

entirely_excl_orders <- unique(orders_all_fam_excl)

cat(sprintf("  Entirely excluded orders: %s\n",
            paste(entirely_excl_orders, collapse = ", ")))

# Step 4: Determine entirely excluded classes
# (redundant classes within excluded phyla are filtered for display later)
class_fam <- spec %>%
  filter(!is.na(class)) %>%
  distinct(class, order, family) %>%
  mutate(fam_excl = family %in% excl_family_pool |
                    order %in% entirely_excl_orders)

classes_all_fam_excl <- class_fam %>%
  group_by(class) %>%
  summarise(all_excl = all(fam_excl), .groups = "drop") %>%
  filter(all_excl) %>% pull(class)

# Also check for classes with no species but with explicit class-level entry
excl_class_explicit <- excl_raw %>%
  filter(!is.na(class) & toupper(excl_validated) == class) %>%
  pull(class) %>% unique()

# v12 FIX: same logic as orders — check if ALL species are directly excluded
for (cls in setdiff(excl_class_explicit, classes_all_fam_excl)) {
  spec_in_class <- spec %>% filter(class == cls)
  if (nrow(spec_in_class) == 0) {
    classes_all_fam_excl <- c(classes_all_fam_excl, cls)
  } else {
    all_excluded <- all(spec_in_class$directly_excluded)
    if (all_excluded) {
      classes_all_fam_excl <- c(classes_all_fam_excl, cls)
    }
  }
}

entirely_excl_classes <- unique(classes_all_fam_excl)

cat(sprintf("  Entirely excluded classes: %s\n",
            paste(entirely_excl_classes, collapse = ", ")))

# Step 5: Determine entirely excluded subclasses
sc_fam <- spec %>%
  filter(!is.na(subclass),
         !class %in% entirely_excl_classes) %>%
  distinct(subclass, order, family) %>%
  mutate(fam_excl = family %in% excl_family_pool |
                    order %in% entirely_excl_orders)

subclasses_all_fam_excl <- sc_fam %>%
  group_by(subclass) %>%
  summarise(all_excl = all(fam_excl), .groups = "drop") %>%
  filter(all_excl) %>% pull(subclass)

entirely_excl_subclasses <- unique(subclasses_all_fam_excl)

cat(sprintf("  Entirely excluded subclasses: %s\n",
            ifelse(length(entirely_excl_subclasses) == 0, "(none)",
                   paste(entirely_excl_subclasses, collapse = ", "))))

# Step 6: Determine entirely excluded phyla
# A phylum is entirely excluded if ALL its species' families/orders/classes are excluded
phylum_fam <- spec %>%
  filter(!is.na(phylum)) %>%
  distinct(phylum, class, order, family) %>%
  mutate(fam_excl = family %in% excl_family_pool |
                    order %in% entirely_excl_orders |
                    class %in% entirely_excl_classes)

phyla_all_fam_excl <- phylum_fam %>%
  group_by(phylum) %>%
  summarise(all_excl = all(fam_excl), .groups = "drop") %>%
  filter(all_excl) %>% pull(phylum)

entirely_excl_phyla <- unique(phyla_all_fam_excl)

# Display non-redundant lists (filter classes within excluded phyla, etc.)
display_excl_classes <- setdiff(entirely_excl_classes,
  spec %>% filter(phylum %in% entirely_excl_phyla) %>% pull(class) %>% unique())
display_excl_orders <- setdiff(entirely_excl_orders,
  spec %>% filter(phylum %in% entirely_excl_phyla | class %in% entirely_excl_classes) %>%
    pull(order) %>% unique())

cat(sprintf("  Entirely excluded phyla: %s\n",
            paste(entirely_excl_phyla, collapse = ", ")))
cat(sprintf("  Entirely excluded classes (non-redundant): %s\n",
            paste(display_excl_classes, collapse = ", ")))
cat(sprintf("  Entirely excluded orders (non-redundant): %s\n",
            paste(display_excl_orders, collapse = ", ")))

# ---- 1.7 Flag taxa excluded from EQR + determine excluded group level ----
# A species is excluded from EQR if:
#   (a) it is directly in excluded_taxa (validated_name in excl_name_set), OR
#   (b) its family is in excl_family_pool (entirely excluded family), OR
#   (c) it belongs to an entirely excluded order/class/phylum.
# excl_group_level = the coarsest taxonomic level at which the entire group
# is excluded. For entirely excluded groups, all entries collapse to this level.

spec <- spec %>%
  mutate(
    excluded_from_EQR = directly_excluded |
      (family %in% excl_family_pool) |
      (order %in% entirely_excl_orders) |
      (class %in% entirely_excl_classes) |
      (phylum %in% entirely_excl_phyla),

    # Also flag Eristalinae membership
    is_eristalinae = (family == "SYRPHIDAE") & (base_genus %in% eristalinae_genera),

    # Excluded group level (coarsest to finest)
    excl_group_level = case_when(
      phylum %in% entirely_excl_phyla      ~ "p",
      class %in% entirely_excl_classes     ~ "c",
      subclass %in% entirely_excl_subclasses ~ "sc",
      order %in% entirely_excl_orders      ~ "o",
      TRUE ~ NA_character_
    ),

    # Excluded group name (for OTU naming)
    excl_group_name = case_when(
      excl_group_level == "p"  ~ to_proper(phylum),
      excl_group_level == "c"  ~ to_proper(class),
      excl_group_level == "sc" ~ to_proper(subclass),
      excl_group_level == "o"  ~ to_proper(order),
      TRUE ~ NA_character_
    )
  )

cat(sprintf("\n  Flagged %d taxa as excluded from EQR\n", sum(spec$excluded_from_EQR)))
cat(sprintf("  Taxa in entirely excluded groups: %d\n", sum(!is.na(spec$excl_group_level))))
cat(sprintf("  Taxa excluded at family level only: %d\n",
            sum(spec$excluded_from_EQR & is.na(spec$excl_group_level))))
cat(sprintf("  Identified %d Eristalinae species in Specialist_taxalist\n",
            sum(spec$is_eristalinae)))

# ---- 1.8 Forced-level overrides ----
# Certain groups are forced to a specific OTU level regardless of specialist
# agreement or metric compliance.
#
# v12 REDESIGN: Three categories of forced-level overrides:
#   1. Oligochaeta (NOT excluded from EQR):
#      - Naididae -> family level ("Naididae Gen. sp.")
#      - Non-Naididae -> subclass level ("Oligochaeta Gen. sp.")
#   2. Stenostomidae (Platyhelminthes, excluded from EQR):
#      -> phylum level ("Platyhelminthes Gen. sp.") — order unavailable
#   3. Neuroptera (v13, NOT excluded from EQR):
#      -> family level ("Osmylidae Gen. sp." / "Sisyridae Gen. sp.")
#      Specialist agreement at order was too coarse for the two families.
#   4. General exclusion rule (excluded from EQR):
#      - Any taxon that is excluded_from_EQR but NOT in an entirely excluded
#        group -> forced to family level. This replaces the v11 hardcoded
#        overrides for Talitridae, non-Asellidae Isopoda, Neomysis/Praunus,
#        Crangonidae/Palaemonidae, Hemiptera excluded families, etc.
#      - Rationale: if only SOME families in an order are excluded (and others
#        are included for EQR), the excluded ones get family-level OTU, not
#        order-level. This ensures the OTU reflects the level at which
#        exclusion applies.
#   Examples: Oligochaeta non-Naididae -> subclass; Hemiptera excluded families
#   (Coreidae, Gerridae, etc.) -> family; Talitridae, Neomysis, etc. -> family

cat("\n  --- Applying forced-level overrides ---\n")

spec <- spec %>%
  mutate(
    forced_level = case_when(
      # --- Oligochaeta split (NOT excluded from EQR) ---
      subclass == "OLIGOCHAETA" & family == "NAIDIDAE" ~ "f",
      subclass == "OLIGOCHAETA" ~ "sc",
      # --- Stenostomidae: order unavailable, force to phylum (v12) ---
      family == "STENOSTOMIDAE" ~ "p",
      # --- Neuroptera: specialist order too coarse, force to family (v13) ---
      order == "NEUROPTERA" ~ "f",
      # --- General exclusion rule (v12) ---
      # Any taxon excluded from EQR but NOT in an entirely excluded group
      # is forced to FAMILY level. This replaces the v11 hardcoded overrides
      # for Talitridae, non-Asellidae Isopoda, Neomysis/Praunus,
      # Crangonidae/Palaemonidae, Hemiptera excluded families, etc.
      excluded_from_EQR & is.na(excl_group_level) ~ "f",
      TRUE ~ NA_character_
    ),
    # Forced entries flag
    forced_excluded = !is.na(forced_level),
    # Oligochaeta and Neuroptera are forced to a level but NOT excluded from EQR.
    # v16: guard with !is.na() to prevent NA propagation when subclass/order
    # are NA (e.g. Stenostomidae rows with order=NA, subclass=NA).
    forced_keeps_eqr = forced_excluded &
      ((!is.na(subclass) & subclass == "OLIGOCHAETA") |
       (!is.na(order)    & order    == "NEUROPTERA")),
    # Update excluded_from_EQR: forced entries excluded UNLESS they keep EQR
    excluded_from_EQR = excluded_from_EQR | (forced_excluded & !forced_keeps_eqr)
  )

cat(sprintf("  Forced-level overrides applied: %d taxa\n", sum(spec$forced_excluded)))
if (sum(spec$forced_excluded) > 0) {
  forced_summary <- spec %>%
    filter(forced_excluded) %>%
    count(phylum, subclass, order, family, forced_level) %>%
    mutate(desc = paste0(
      "    ",
      ifelse(!is.na(order), to_proper(order), to_proper(phylum)),
      " / ",
      ifelse(!is.na(family), to_proper(family), ifelse(!is.na(subclass), to_proper(subclass), "—")),
      " -> ", LEVEL_LABEL[forced_level], " (n=", n, ")")
    )
  cat(paste(forced_summary$desc, collapse = "\n"), "\n")
}


# ============================================================================
# PHASE 3: 100% SPECIALIST AGREEMENT (DIRECT ASSIGNMENT)
# ============================================================================

cat("\n=== PHASE 3: Specialist agreement (direct assignment) ===\n")

# ---- 3.1 Determine 100% agreement level (coarsest specialist) ----
# Vectorised: convert each specialist code to its rank, take row-wise max

spec <- spec %>%
  mutate(
    rank_s1 = LEVEL_RANK[specialist1],
    rank_s2 = LEVEL_RANK[specialist2],
    rank_s3 = LEVEL_RANK[specialist3],
    rank_s4 = LEVEL_RANK[specialist4],
    max_rank = pmax(rank_s1, rank_s2, rank_s3, rank_s4, na.rm = TRUE),
    agreement_level = names(LEVEL_RANK)[match(max_rank, LEVEL_RANK)]
  ) %>%
  select(-rank_s1, -rank_s2, -rank_s3, -rank_s4, -max_rank)

# ---- 3.2 Validate against existing 100%_proposed_lvl ----
validation <- spec %>%
  mutate(match = agreement_level == `100%_proposed_lvl`) %>%
  summarise(
    total = n(),
    matching = sum(match, na.rm = TRUE),
    mismatching = sum(!match, na.rm = TRUE)
  )
cat(sprintf("  Agreement level validation: %d/%d match existing 100%%_proposed_lvl",
            validation$matching, validation$total))
if (validation$mismatching > 0) {
  cat(sprintf(" (%d mismatches - investigate!)", validation$mismatching))
}
cat("\n")

# ---- 3.3 Direct assignment (no sole-representative simplification) ----
# The 100% agreement level is assigned directly as the Phase 3 OTU level.
# No adjustment is made based on the number of representatives in Lithuania.

spec <- spec %>%
  mutate(phase3_level = agreement_level)

cat("  Phase 3 level = agreement level (direct assignment, no simplification)\n")


# ============================================================================
# PHASE 2 / 4: METRIC GROUP ASSIGNMENT + COMPLIANCE CHECK
# ============================================================================

cat("\n=== PHASE 4: Metric compliance check ===\n")

# ---- 4.1 Assign metric group ----
# Each taxon belongs to a metric group that determines its minimum OTU level.
# Excluded taxa (EQR = TRUE) have no metric requirements.

spec <- spec %>%
  mutate(
    metric_group = case_when(
      excluded_from_EQR               ~ NA_character_,
      subclass == "OLIGOCHAETA"       ~ "Oligochaeta",
      subclass == "HIRUDINEA"         ~ "Hirudinea",
      order == "DIPTERA"              ~ "Diptera",
      class == "MALACOSTRACA"         ~ "Malacostraca",
      class == "GASTROPODA"           ~ "Gastropoda",
      class == "BIVALVIA"             ~ "Bivalvia",
      order == "PLECOPTERA"           ~ "Plecoptera",
      order == "EPHEMEROPTERA"        ~ "Ephemeroptera",
      order == "TRICHOPTERA"          ~ "Trichoptera",
      order == "COLEOPTERA"           ~ "Coleoptera",
      order == "MEGALOPTERA"          ~ "Megaloptera",
      order == "HEMIPTERA"            ~ "Hemiptera",
      order == "ODONATA"              ~ "Odonata",
      order == "LEPIDOPTERA"          ~ "Lepidoptera",   # v12: freshwater Lepidoptera
      order == "TRICLADIDA"           ~ "Turbellaria",
      TRUE                            ~ NA_character_
    )
  )

# ---- 4.2 Assign minimum identification level per Phase 2 reference table ----

spec <- spec %>%
  mutate(
    min_level = case_when(
      is.na(metric_group) ~ NA_character_,

      # --- Oligochaeta ---
      # Naididae (formerly Tubificidae) at family; others at subclass
      metric_group == "Oligochaeta" & family == "NAIDIDAE"  ~ "f",
      metric_group == "Oligochaeta"                         ~ "sc",

      # --- Hirudinea ---
      # Erpobdella & Helobdella at genus (DSFI negative diversity); others at family
      metric_group == "Hirudinea" &
        base_genus %in% c("Erpobdella", "Helobdella")      ~ "g",
      metric_group == "Hirudinea"                           ~ "f",

      # --- Diptera ---
      # Chironomus at genus (DSFI IG precluder);
      # Eristalinae at subfamily (DSFI IG5/IG6 precluder); others at family
      metric_group == "Diptera" & base_genus == "Chironomus"  ~ "g",
      metric_group == "Diptera" & is_eristalinae              ~ "sf",
      metric_group == "Diptera"                               ~ "f",

      # --- Malacostraca ---
      # Asellus & Gammarus at genus (DSFI indicator + diversity); others at family
      # Pontogammaridae at family (BMWP score 6) — no species-level exceptions
      metric_group == "Malacostraca" &
        base_genus %in% c("Asellus", "Gammarus")            ~ "g",
      metric_group == "Malacostraca"                         ~ "f",

      # --- Gastropoda ---
      # Ancylus & Lymnaea at genus (DSFI diversity); others at family
      metric_group == "Gastropoda" &
        base_genus %in% c("Ancylus", "Lymnaea")             ~ "g",
      metric_group == "Gastropoda"                           ~ "f",

      # --- Bivalvia ---
      # Sphaerium at genus (DSFI diversity); others at family
      metric_group == "Bivalvia" & base_genus == "Sphaerium" ~ "g",
      metric_group == "Bivalvia"                             ~ "f",

      # --- Plecoptera ---
      # All at genus (DSFI IG1/IG2 entrance; #DEP uses species)
      metric_group == "Plecoptera"                           ~ "g",

      # --- Coleoptera ---
      # Elmis, Limnius, Elodes at genus (DSFI IG1/IG2 + diversity); others at family
      metric_group == "Coleoptera" &
        base_genus %in% c("Elmis", "Limnius", "Elodes")     ~ "g",
      metric_group == "Coleoptera"                           ~ "f",

      # --- Megaloptera ---
      # Sialis at genus (DSFI IG4 + negative diversity)
      metric_group == "Megaloptera"                          ~ "g",

      # --- Lepidoptera (v12) ---
      # Freshwater Lepidoptera at family (ensures Crambidae Gen. sp.)
      metric_group == "Lepidoptera"                          ~ "f",

      # --- All other groups at family ---
      # Ephemeroptera, Trichoptera, Hemiptera, Odonata, Turbellaria
      metric_group %in% c("Ephemeroptera", "Trichoptera",
                           "Hemiptera", "Odonata", "Turbellaria") ~ "f",

      TRUE ~ NA_character_
    )
  )

# ---- 4.3 Apply Phase 4 override ----
# Override occurs when Phase 3 result (phase3_level) is COARSER than
# the metric minimum (min_level). The minimum is a floor, not a ceiling:
# if Phase 3 yields a finer level, the finer level is kept.
# Taxa in entirely excluded groups skip metric compliance (no override).

spec <- spec %>%
  mutate(
    phase3_rank = LEVEL_RANK[phase3_level],
    min_rank    = ifelse(!is.na(min_level), LEVEL_RANK[min_level], NA_real_),

    # Override needed when Phase 3 is coarser (higher rank) than minimum
    # AND the taxon is NOT in an entirely excluded group or forced-level group
    override = !is.na(min_rank) & phase3_rank > min_rank &
               is.na(excl_group_level) & !forced_excluded,

    # Final level: forced_level > excl_group_level > override > phase3
    final_level = case_when(
      forced_excluded            ~ forced_level,
      !is.na(excl_group_level)   ~ excl_group_level,
      override                   ~ min_level,
      TRUE                       ~ phase3_level
    ),

    override_reason = case_when(
      forced_excluded ~ paste0(
        "Forced to ", LEVEL_LABEL[forced_level], " level",
        ifelse(excluded_from_EQR, " -> excluded from EQR", "")
      ),
      !is.na(excl_group_level) ~ paste0(
        "Entirely excluded group (", excl_group_name,
        ") -> OTU at ", LEVEL_LABEL[excl_group_level], " level"
      ),
      override ~ paste0(
        "Phase 3 = ", LEVEL_LABEL[phase3_level],
        "; overridden to ", LEVEL_LABEL[min_level],
        " (", metric_group, " metric requirement)"
      ),
      TRUE ~ NA_character_
    )
  )

cat(sprintf("  %d taxa overridden for metric compliance\n", sum(spec$override)))
cat(sprintf("  %d taxa assigned to excluded group level\n", sum(!is.na(spec$excl_group_level))))
cat(sprintf("  %d taxa with forced-level overrides (%d excluded from EQR, %d in EQR)\n",
            sum(spec$forced_excluded),
            sum(spec$forced_excluded & spec$excluded_from_EQR),
            sum(spec$forced_excluded & !spec$excluded_from_EQR)))

# Override breakdown by metric group
if (sum(spec$override) > 0) {
  override_summary <- spec %>%
    filter(override) %>%
    count(metric_group, phase3_level, final_level) %>%
    mutate(desc = paste0("    ", metric_group, ": ",
                         LEVEL_LABEL[phase3_level], " -> ",
                         LEVEL_LABEL[final_level], " (n=", n, ")"))
  cat(paste(override_summary$desc, collapse = "\n"), "\n")
}


# ============================================================================
# GENERATE OTU NAMES + LABELS
# ============================================================================

cat("\n=== Generating OTU names ===\n")

spec <- spec %>%
  mutate(
    final_level_label = LEVEL_LABEL[final_level],

    OTU_name = case_when(
      # Forced-level override: OTU at forced level
      forced_excluded & forced_level == "p"  ~ paste0(to_proper(phylum), " Gen. sp."),
      forced_excluded & forced_level == "c"  ~ paste0(to_proper(class), " Gen. sp."),
      forced_excluded & forced_level == "sc" ~ paste0(to_proper(subclass), " Gen. sp."),
      forced_excluded & forced_level == "o"  ~ paste0(to_proper(order), " Gen. sp."),
      forced_excluded & forced_level == "f"  ~ paste0(to_proper(family), " Gen. sp."),
      # Entirely excluded group: OTU at group level
      !is.na(excl_group_level) ~ paste0(excl_group_name, " Gen. sp."),
      # Normal entries
      final_level == "s"  ~ validated_name,
      final_level == "g"  ~ paste0(base_genus, " sp."),
      final_level == "sf" ~ "Eristalinae Gen. sp.",
      final_level == "f"  ~ paste0(to_proper(family), " Gen. sp."),
      final_level == "o"  ~ paste0(to_proper(order), " Gen. sp."),
      final_level == "sc" ~ paste0(to_proper(subclass), " Gen. sp."),
      final_level == "c"  ~ paste0(to_proper(class), " Gen. sp."),
      final_level == "p"  ~ paste0(to_proper(phylum), " Gen. sp.")
    )
  )

# ============================================================================
# DECISION PATH (full traceability string for each taxon)
# ============================================================================

spec <- spec %>%
  mutate(
    decision_path = case_when(
      # Forced-level override
      forced_excluded ~ paste0(
        "Specialists [", specialist1, ",", specialist2, ",",
        specialist3, ",", specialist4, "] -> agreement=",
        LEVEL_LABEL[agreement_level],
        " -> FORCED to ", LEVEL_LABEL[forced_level],
        ifelse(excluded_from_EQR, " [EXCLUDED from EQR]", ""),
        " -> FINAL: ", OTU_name, " (", final_level_label, ")"
      ),
      # Entirely excluded group
      !is.na(excl_group_level) ~ paste0(
        "Specialists [", specialist1, ",", specialist2, ",",
        specialist3, ",", specialist4, "] -> agreement=",
        LEVEL_LABEL[agreement_level],
        " -> Entirely excluded group (", excl_group_name,
        " at ", LEVEL_LABEL[excl_group_level], " level)",
        " [EXCLUDED from EQR]",
        " -> FINAL: ", OTU_name, " (", final_level_label, ")"
      ),
      # Normal: excluded from EQR at family level (not entirely excluded group)
      excluded_from_EQR & is.na(excl_group_level) ~ paste0(
        "Specialists [", specialist1, ",", specialist2, ",",
        specialist3, ",", specialist4, "] -> agreement=",
        LEVEL_LABEL[agreement_level],
        " -> Phase3=", LEVEL_LABEL[phase3_level],
        ifelse(!is.na(metric_group),
               paste0(" -> metric_group=", metric_group,
                      " (min=", LEVEL_LABEL[min_level], ")"),
               ""),
        ifelse(override,
               paste0(" -> OVERRIDE to ", LEVEL_LABEL[final_level]),
               " -> no override"),
        " [EXCLUDED from EQR]",
        " -> FINAL: ", OTU_name, " (", final_level_label, ")"
      ),
      # Normal: included in EQR
      TRUE ~ paste0(
        "Specialists [", specialist1, ",", specialist2, ",",
        specialist3, ",", specialist4, "] -> agreement=",
        LEVEL_LABEL[agreement_level],
        " -> Phase3=", LEVEL_LABEL[phase3_level],
        ifelse(!is.na(metric_group),
               paste0(" -> metric_group=", metric_group,
                      " (min=", LEVEL_LABEL[min_level], ")"),
               ""),
        ifelse(override,
               paste0(" -> OVERRIDE to ", LEVEL_LABEL[final_level]),
               " -> no override"),
        " -> FINAL: ", OTU_name, " (", final_level_label, ")"
      )
    )
  )


# ============================================================================
# BIOLOGICAL COMPLEXITY SORT (shared by OTU_assignments and hierarchical OTL)
# ============================================================================
# Phyla ordered from simplest to most complex organisms.
# Within Annelida: Oligochaeta -> Hirudinea -> Polychaeta.
# Within Mollusca: Bivalvia -> Gastropoda.
# Within Arthropoda: Maxillopoda -> Malacostraca -> Arachnida -> Hexapoda.
# Malacostraca orders: evolutionary sequence (Mysida -> Isopoda -> Amphipoda -> Decapoda).
# Hexapoda: Collembola first, then Insecta orders alphabetically.
# Within each group: order -> family -> subfamily -> genus -> species.

PHYLUM_RANK <- c(
  PORIFERA        = 1,
  CNIDARIA        = 2,
  PLATYHELMINTHES = 3,
  NEMATODA        = 4,
  NEMATOMORPHA    = 5,
  NEMERTEA        = 6,
  BRYOZOA         = 7,
  ANNELIDA        = 8,
  MOLLUSCA        = 9,
  ARTHROPODA      = 10
)

# Class rank within phylum (only where non-alphabetical ordering is needed)
CLASS_RANK <- c(
  # Mollusca
  BIVALVIA       = 1,
  GASTROPODA     = 2,
  # Arthropoda — crustacean (Pancrustacea) classes grouped together
  MAXILLOPODA    = 1,
  BRANCHIOPODA   = 2,
  OSTRACODA      = 3,
  MALACOSTRACA   = 4,
  ARACHNIDA      = 5,
  COLLEMBOLA     = 6,
  INSECTA        = 7
)

# Subclass rank within Annelida (Clitellata)
SUBCLASS_RANK <- c(
  OLIGOCHAETA = 1,
  HIRUDINEA   = 2
)

# Order rank for Malacostraca (evolutionary sequence, oldest to newest)
MALACOSTRACA_ORDER_RANK <- c(
  MYSIDA    = 1,
  ISOPODA   = 2,
  AMPHIPODA = 3,
  DECAPODA  = 4
)

# Helper function to add sort keys, sort, and remove sort keys
sort_by_complexity <- function(df) {
  df %>%
    mutate(
      sort_phylum = PHYLUM_RANK[phylum],
      sort_class = ifelse(
        !is.na(class) & class %in% names(CLASS_RANK),
        CLASS_RANK[class],
        ifelse(!is.na(class), 100, 0)
      ),
      sort_subclass = ifelse(
        !is.na(subclass) & subclass %in% names(SUBCLASS_RANK),
        SUBCLASS_RANK[subclass],
        ifelse(!is.na(subclass), 100, 0)
      ),
      sort_order = ifelse(
        !is.na(order) & !is.na(class) & class == "MALACOSTRACA" &
          order %in% names(MALACOSTRACA_ORDER_RANK),
        MALACOSTRACA_ORDER_RANK[order],
        ifelse(!is.na(order), 100, 0)
      )
    ) %>%
    arrange(
      sort_phylum,
      !is.na(class), sort_class, class,
      !is.na(subclass), sort_subclass, subclass,
      !is.na(order), sort_order, order,
      !is.na(family), family,
      !is.na(subfamily), subfamily,
      !is.na(genus), genus,
      !is.na(species), species
    ) %>%
    select(-sort_phylum, -sort_class, -sort_subclass, -sort_order)
}


# ============================================================================
# PREPARE SPECIALIST OUTPUT (decision tree traceability sheet)
# ============================================================================

cat("\n=== Preparing specialist output ===\n")

# Add subfamily column to spec for sorting (from Eristalinae flag)
spec <- spec %>%
  mutate(subfamily = ifelse(is_eristalinae, "Eristalinae", NA_character_))

specialist_output <- spec %>%
  sort_by_complexity() %>%
  mutate(sort_id = row_number()) %>%
  select(
    sort_id,
    # Taxonomy
    phylum, subphylum, class, subclass, order, family, subfamily,
    genus, species, validated_name,
    # Specialist assessments
    specialist1, specialist2, specialist3, specialist4,
    # Phase 3: agreement (= Phase 3 level, direct assignment)
    agreement_level, phase3_level,
    # Phase 4: metric compliance
    metric_group, min_level,
    override, override_reason,
    # Excluded group info
    excl_group_level, excl_group_name,
    # Forced-level info
    forced_level, forced_excluded,
    # Final assignment
    final_level, final_level_label, OTU_name,
    # Flags
    excluded_from_EQR, is_eristalinae,
    # Full decision path
    decision_path
  )


# ============================================================================
# JOIN OTU ASSIGNMENTS TO OTL_FINAL
# ============================================================================
# OTL_final contains entries at multiple taxonomic levels:
#   - Species (multi-word names): majority of rows
#   - Genus (single-word, has family): ~1,000 rows
#   - Higher-level (single-word, no family): family/order/class/phylum entries
#
# Species entries: direct join to decision tree results by validated_name.
# Genus entries: OTU determined by species in that genus, capped at genus
#   (unless in entirely excluded group -> group level).
# Higher-level entries: OTU name is their own level formatted as "Name Gen. sp."
#   (unless in entirely excluded group -> group level).

cat("\n=== Joining OTU assignments to OTL_final ===\n")

otl_full <- read_excel(input_file, sheet = "OTL_final", guess_max = 10000)
otl_full <- otl_full %>%
  rename(validated_name = `validated_name_GBIF+Molluscabase`,
         original_name  = `original_taxonname_literature+EPA`)

cat(sprintf("  OTL_final rows (before gap-fill): %d\n", nrow(otl_full)))

# ---- Fill missing higher-level entries ----
# OTL_final should contain hierarchical entries at every level between species
# and phylum. Some are missing. We generate them from the taxonomy of existing
# entries, inheriting sort_id, taxo_id, and metadata columns as NA.

existing_names <- unique(otl_full$validated_name)

# Collect all required entries from the taxonomy of existing rows
new_entries <- list()

# --- Genus entries needed by species ---
species_rows <- otl_full %>% filter(str_detect(validated_name, " "))
genus_needed <- species_rows %>%
  mutate(genus_name = str_extract(validated_name, "^[^\\s(]+")) %>%
  filter(!genus_name %in% existing_names) %>%
  distinct(genus_name, .keep_all = TRUE) %>%
  transmute(
    phylum, subphylum, class, subclass, order, family, subfamily,
    validated_name = genus_name,
    original_name = NA_character_,
    added_by = "gap-fill: genus needed by species"
  )
if (nrow(genus_needed) > 0) new_entries <- c(new_entries, list(genus_needed))

# Update existing names
existing_names <- c(existing_names, genus_needed$validated_name)

# --- Family entries needed by taxa with a family ---
family_rows <- otl_full %>% filter(!is.na(family))
family_needed <- family_rows %>%
  mutate(family_name = to_proper(family)) %>%
  filter(!family_name %in% existing_names) %>%
  distinct(family_name, .keep_all = TRUE) %>%
  transmute(
    phylum, subphylum, class, subclass, order, family, subfamily = NA_character_,
    validated_name = family_name,
    original_name = NA_character_,
    added_by = "gap-fill: family entry"
  )
if (nrow(family_needed) > 0) new_entries <- c(new_entries, list(family_needed))
existing_names <- c(existing_names, family_needed$validated_name)

# --- Order entries needed by taxa with an order ---
order_rows <- otl_full %>% filter(!is.na(order))
order_needed <- order_rows %>%
  mutate(order_name = to_proper(order)) %>%
  filter(!order_name %in% existing_names) %>%
  distinct(order_name, .keep_all = TRUE) %>%
  transmute(
    phylum, subphylum, class, subclass, order,
    family = NA_character_, subfamily = NA_character_,
    validated_name = order_name,
    original_name = NA_character_,
    added_by = "gap-fill: order entry"
  )
if (nrow(order_needed) > 0) new_entries <- c(new_entries, list(order_needed))
existing_names <- c(existing_names, order_needed$validated_name)

# --- Subclass entries needed by taxa with a subclass ---
subclass_rows <- otl_full %>% filter(!is.na(subclass))
subclass_needed <- subclass_rows %>%
  mutate(subclass_name = to_proper(subclass)) %>%
  filter(!subclass_name %in% existing_names) %>%
  distinct(subclass_name, .keep_all = TRUE) %>%
  transmute(
    phylum, subphylum, class, subclass,
    order = NA_character_, family = NA_character_, subfamily = NA_character_,
    validated_name = subclass_name,
    original_name = NA_character_,
    added_by = "gap-fill: subclass entry"
  )
if (nrow(subclass_needed) > 0) new_entries <- c(new_entries, list(subclass_needed))
existing_names <- c(existing_names, subclass_needed$validated_name)

# --- Class entries needed by taxa with a class ---
class_rows <- otl_full %>% filter(!is.na(class))
class_needed <- class_rows %>%
  mutate(class_name = to_proper(class)) %>%
  filter(!class_name %in% existing_names) %>%
  distinct(class_name, .keep_all = TRUE) %>%
  transmute(
    phylum, subphylum, class,
    subclass = NA_character_, order = NA_character_,
    family = NA_character_, subfamily = NA_character_,
    validated_name = class_name,
    original_name = NA_character_,
    added_by = "gap-fill: class entry"
  )
if (nrow(class_needed) > 0) new_entries <- c(new_entries, list(class_needed))
existing_names <- c(existing_names, class_needed$validated_name)

# --- Phylum entries needed by taxa with a phylum ---
phylum_rows <- otl_full %>% filter(!is.na(phylum))
phylum_needed <- phylum_rows %>%
  mutate(phylum_name = to_proper(phylum)) %>%
  filter(!phylum_name %in% existing_names) %>%
  distinct(phylum_name, .keep_all = TRUE) %>%
  transmute(
    phylum,
    subphylum = NA_character_, class = NA_character_,
    subclass = NA_character_, order = NA_character_,
    family = NA_character_, subfamily = NA_character_,
    validated_name = phylum_name,
    original_name = NA_character_,
    added_by = "gap-fill: phylum entry"
  )
if (nrow(phylum_needed) > 0) new_entries <- c(new_entries, list(phylum_needed))

# Combine and bind to OTL_final
if (length(new_entries) > 0) {
  gap_fill <- bind_rows(new_entries)
  cat(sprintf("  Adding %d missing higher-level entries:\n", nrow(gap_fill)))

  # Report by level
  gap_summary <- gap_fill %>% count(added_by)
  for (i in seq_len(nrow(gap_summary))) {
    cat(sprintf("    %s: %d\n", gap_summary$added_by[i], gap_summary$n[i]))
  }
  # List each entry
  for (i in seq_len(nrow(gap_fill))) {
    cat(sprintf("    + %s (%s)\n", gap_fill$validated_name[i], gap_fill$added_by[i]))
  }

  # Bind to OTL_final -- unmatched columns in gap_fill become NA
  otl_full <- bind_rows(otl_full, gap_fill)
} else {
  cat("  No missing higher-level entries found.\n")
}

cat(sprintf("  OTL_final rows (after gap-fill): %d\n", nrow(otl_full)))

# ---- Build species-level OTU lookup ----
species_lookup <- spec %>%
  distinct(validated_name, .keep_all = TRUE) %>%
  select(validated_name, final_level, final_level_label, OTU_name,
         metric_group, min_level, override, override_reason,
         excluded_from_EQR, is_eristalinae, agreement_level,
         phase3_level, excl_group_level, excl_group_name,
         forced_level, forced_excluded, decision_path)

# ---- Build genus-level OTU lookup ----
# v10: Genus entries use taxonomy-based min_level (not inherited from species
#      species-level BMWP exceptions).
# v10: Forced-level overrides applied to genus entries (Talitridae, etc.).
# v10: Entirely excluded groups -> OTU at group level.
# v10: Excluded families (not entirely excluded group) -> metric-minimum-aware
#      but excluded_from_EQR = TRUE.
genus_lookup <- spec %>%
  group_by(base_genus) %>%
  summarise(
    family            = first(family),
    order             = first(order),
    subclass          = first(subclass),
    class             = first(class),
    phylum            = first(phylum),
    is_eristalinae    = first(is_eristalinae),
    excluded_from_EQR = first(excluded_from_EQR),
    excl_group_level  = first(excl_group_level),
    excl_group_name   = first(excl_group_name),
    forced_level      = first(forced_level),
    forced_excluded   = first(forced_excluded),
    .groups = "drop"
  ) %>%
  mutate(
    # v10: Compute genus-level metric_group based on taxonomy (same rules as
    # Phase 2, but using genus taxonomy directly). Forced/excluded genera
    # get no metric group.
    metric_group = case_when(
      excluded_from_EQR                ~ NA_character_,
      subclass == "OLIGOCHAETA"        ~ "Oligochaeta",
      subclass == "HIRUDINEA"          ~ "Hirudinea",
      order == "DIPTERA"               ~ "Diptera",
      class == "MALACOSTRACA"          ~ "Malacostraca",
      class == "GASTROPODA"            ~ "Gastropoda",
      class == "BIVALVIA"              ~ "Bivalvia",
      order == "PLECOPTERA"            ~ "Plecoptera",
      order == "EPHEMEROPTERA"         ~ "Ephemeroptera",
      order == "TRICHOPTERA"           ~ "Trichoptera",
      order == "COLEOPTERA"            ~ "Coleoptera",
      order == "MEGALOPTERA"           ~ "Megaloptera",
      order == "HEMIPTERA"             ~ "Hemiptera",
      order == "ODONATA"               ~ "Odonata",
      order == "LEPIDOPTERA"           ~ "Lepidoptera",   # v12
      order == "TRICLADIDA"            ~ "Turbellaria",
      TRUE                             ~ NA_character_
    ),
    # v10: Compute min_level for the genus based on taxonomy (NOT from species
    # exceptions). Genus entries default to their metric group minimum level.
    min_level = case_when(
      is.na(metric_group) ~ NA_character_,
      metric_group == "Oligochaeta" & family == "NAIDIDAE"  ~ "f",
      metric_group == "Oligochaeta"                         ~ "sc",
      metric_group == "Hirudinea" &
        base_genus %in% c("Erpobdella", "Helobdella")      ~ "g",
      metric_group == "Hirudinea"                           ~ "f",
      metric_group == "Diptera" & base_genus == "Chironomus" ~ "g",
      metric_group == "Diptera" & is_eristalinae             ~ "sf",
      metric_group == "Diptera"                              ~ "f",
      # Malacostraca: genus-level exceptions (Asellus, Gammarus at genus);
      # all others at family (Pontogammaridae at family, BMWP score 6)
      metric_group == "Malacostraca" &
        base_genus %in% c("Asellus", "Gammarus")            ~ "g",
      metric_group == "Malacostraca"                         ~ "f",
      metric_group == "Gastropoda" &
        base_genus %in% c("Ancylus", "Lymnaea")             ~ "g",
      metric_group == "Gastropoda"                           ~ "f",
      metric_group == "Bivalvia" & base_genus == "Sphaerium" ~ "g",
      metric_group == "Bivalvia"                             ~ "f",
      metric_group == "Plecoptera"                           ~ "g",
      metric_group == "Coleoptera" &
        base_genus %in% c("Elmis", "Limnius", "Elodes")     ~ "g",
      metric_group == "Coleoptera"                           ~ "f",
      metric_group == "Megaloptera"                          ~ "g",
      metric_group == "Lepidoptera"                          ~ "f",   # v12
      metric_group %in% c("Ephemeroptera", "Trichoptera",
                           "Hemiptera", "Odonata", "Turbellaria") ~ "f",
      TRUE ~ NA_character_
    ),
    # Cap the metric minimum at genus: if min is finer than genus (i.e., species),
    # use genus. If min is coarser (family, order, subclass), use that coarser level.
    # For forced entries, use forced_level. For excluded groups, use group level.
    min_rank_genus = ifelse(!is.na(min_level), LEVEL_RANK[min_level], NA_real_),
    genus_final_level = case_when(
      forced_excluded                  ~ forced_level,       # forced-level override
      !is.na(excl_group_level)         ~ excl_group_level,   # entirely excluded group
      is.na(min_level)                 ~ "g",                 # no metric requirement -> genus
      min_rank_genus <= LEVEL_RANK["g"] ~ "g",                # min is genus or finer -> genus
      TRUE ~ min_level                                         # min is coarser -> use that level
    ),
    genus_OTU_name = case_when(
      forced_excluded & forced_level == "p"  ~ paste0(to_proper(phylum), " Gen. sp."),
      forced_excluded & forced_level == "c"  ~ paste0(to_proper(class), " Gen. sp."),
      forced_excluded & forced_level == "sc" ~ paste0(to_proper(subclass), " Gen. sp."),
      forced_excluded & forced_level == "o"  ~ paste0(to_proper(order), " Gen. sp."),
      forced_excluded & forced_level == "f"  ~ paste0(to_proper(family), " Gen. sp."),
      !is.na(excl_group_level)   ~ paste0(excl_group_name, " Gen. sp."),
      genus_final_level == "g"   ~ paste0(base_genus, " sp."),
      genus_final_level == "sf"  ~ "Eristalinae Gen. sp.",
      genus_final_level == "f"   ~ paste0(to_proper(family), " Gen. sp."),
      genus_final_level == "o"   ~ paste0(to_proper(order), " Gen. sp."),
      genus_final_level == "sc"  ~ paste0(to_proper(subclass), " Gen. sp."),
      genus_final_level == "c"   ~ paste0(to_proper(class), " Gen. sp."),
      genus_final_level == "p"   ~ paste0(to_proper(phylum), " Gen. sp.")
    ),
    genus_final_level_label = LEVEL_LABEL[genus_final_level],
    genus_decision_path = case_when(
      forced_excluded ~ paste0(
        "Genus entry -> FORCED to ", LEVEL_LABEL[forced_level],
        ifelse(excluded_from_EQR, " [EXCLUDED from EQR]", ""),
        " -> FINAL: ",
        genus_OTU_name, " (", genus_final_level_label, ")"
      ),
      !is.na(excl_group_level) ~ paste0(
        "Genus entry -> entirely excluded group (",
        excl_group_name, " at ", LEVEL_LABEL[excl_group_level],
        " level) [EXCLUDED from EQR] -> FINAL: ",
        genus_OTU_name, " (", genus_final_level_label, ")"
      ),
      TRUE ~ paste0(
        "Genus entry -> metric_group=",
        ifelse(is.na(metric_group), "none", metric_group),
        " (min=", ifelse(is.na(min_level), "none", LEVEL_LABEL[min_level]),
        ", capped at genus) -> FINAL: ",
        genus_OTU_name, " (", genus_final_level_label, ")",
        ifelse(excluded_from_EQR, " [EXCLUDED from EQR]", "")
      )
    )
  ) %>%
  select(-min_rank_genus)

# ---- Classify OTL_final rows ----
# Species = multi-word; single-word with family = genus; single-word without family = higher
otl_full <- otl_full %>%
  mutate(
    has_space = str_detect(validated_name, " "),
    # Detect family-level entries: single-word name whose title-case matches the family column
    is_family_entry = !has_space & !is.na(family) &
      str_to_upper(validated_name) == family,
    entry_type = case_when(
      has_space                                     ~ "species",
      !has_space & !is.na(family) & !is_family_entry ~ "genus",
      is_family_entry                                ~ "family",
      !has_space & is.na(family) & !is.na(order)     ~ "order",
      !has_space & is.na(family) & is.na(order) &
        !is.na(subclass)                             ~ "subclass",
      !has_space & is.na(family) & is.na(order) &
        is.na(subclass) & !is.na(class)              ~ "class",
      TRUE                                           ~ "phylum"
    )
  ) %>%
  select(-has_space, -is_family_entry)

# Add genus and species columns derived from validated_name (OTL_final lacks these)
otl_full <- otl_full %>%
  mutate(
    genus = case_when(
      entry_type == "species" ~ str_extract(validated_name, "^[^\\s(]+"),
      entry_type == "genus"   ~ validated_name,
      TRUE ~ NA_character_
    ),
    species = case_when(
      entry_type == "species" ~ str_replace(validated_name, "^[^\\s]+\\s+", ""),
      TRUE ~ NA_character_
    )
  )

cat(sprintf("  Species entries: %d (%d unique)\n",
            sum(otl_full$entry_type == "species"),
            n_distinct(otl_full$validated_name[otl_full$entry_type == "species"])))
cat(sprintf("  Genus entries: %d\n", sum(otl_full$entry_type == "genus")))
cat(sprintf("  Family entries: %d\n", sum(otl_full$entry_type == "family")))
cat(sprintf("  Higher-level entries: %d (order=%d, subclass=%d, class=%d, phylum=%d)\n",
            sum(otl_full$entry_type %in% c("order", "subclass", "class", "phylum")),
            sum(otl_full$entry_type == "order"),
            sum(otl_full$entry_type == "subclass"),
            sum(otl_full$entry_type == "class"),
            sum(otl_full$entry_type == "phylum")))

# ---- Assign OTU names ----
# First, left-join the species lookup and genus lookup side by side
otl_full <- otl_full %>%
  left_join(
    species_lookup %>%
      select(validated_name,
             sp_final_level     = final_level,
             sp_final_level_lbl = final_level_label,
             sp_OTU_name        = OTU_name,
             sp_metric_group    = metric_group,
             sp_excluded        = excluded_from_EQR,
             sp_excl_grp        = excl_group_level,
             sp_excl_grp_name   = excl_group_name,
             sp_forced          = forced_excluded,
             sp_decision_path   = decision_path),
    by = "validated_name"
  ) %>%
  left_join(
    genus_lookup %>%
      select(base_genus,
             gen_final_level     = genus_final_level,
             gen_final_level_lbl = genus_final_level_label,
             gen_OTU_name        = genus_OTU_name,
             gen_metric_group    = metric_group,
             gen_excluded        = excluded_from_EQR,
             gen_excl_grp        = excl_group_level,
             gen_excl_grp_name   = excl_group_name,
             gen_forced          = forced_excluded,
             gen_decision_path   = genus_decision_path),
    by = c("genus" = "base_genus")
  )

# Determine excluded group level and forced-level for each OTL_final row
otl_full <- otl_full %>%
  mutate(
    excl_grp = case_when(
      phylum %in% entirely_excl_phyla        ~ "p",
      class %in% entirely_excl_classes       ~ "c",
      subclass %in% entirely_excl_subclasses ~ "sc",
      order %in% entirely_excl_orders        ~ "o",
      TRUE ~ NA_character_
    ),
    excl_grp_name = case_when(
      excl_grp == "p"  ~ to_proper(phylum),
      excl_grp == "c"  ~ to_proper(class),
      excl_grp == "sc" ~ to_proper(subclass),
      excl_grp == "o"  ~ to_proper(order),
      TRUE ~ NA_character_
    ),
    # v13: Detect forced-level for OTL_final rows based on taxonomy
    # For species/genus entries: inherit from lookup (sp_forced/gen_forced)
    # For family entries: Oligochaeta, Stenostomidae, Neuroptera need explicit handling
    # For order entries: Oligochaeta orders forced to subclass
    otl_forced = case_when(
      entry_type == "species" ~ ifelse(!is.na(sp_forced) & sp_forced, TRUE, FALSE),
      entry_type == "genus"   ~ ifelse(!is.na(gen_forced) & gen_forced, TRUE, FALSE),
      # Oligochaeta families: forced to family or subclass
      entry_type == "family" & !is.na(subclass) & subclass == "OLIGOCHAETA" ~ TRUE,
      # Stenostomidae: forced to phylum
      entry_type == "family" & !is.na(family) & family == "STENOSTOMIDAE" ~ TRUE,
      # Neuroptera families: forced to family (v13)
      entry_type == "family" & !is.na(order) & order == "NEUROPTERA" ~ TRUE,
      # Oligochaeta order entries: forced to subclass (v13)
      entry_type == "order" & !is.na(subclass) & subclass == "OLIGOCHAETA" ~ TRUE,
      TRUE ~ FALSE
    ),
    # Determine the forced level for family/order entries
    otl_forced_level = case_when(
      !otl_forced ~ NA_character_,
      entry_type %in% c("species", "genus") ~ NA_character_,  # from lookup
      # Oligochaeta Naididae -> family; other Oligochaeta -> subclass
      !is.na(subclass) & subclass == "OLIGOCHAETA" & !is.na(family) & family == "NAIDIDAE" ~ "f",
      !is.na(subclass) & subclass == "OLIGOCHAETA" ~ "sc",
      # Stenostomidae -> phylum
      !is.na(family) & family == "STENOSTOMIDAE" ~ "p",
      # Neuroptera -> family (v13)
      !is.na(order) & order == "NEUROPTERA" ~ "f",
      TRUE ~ NA_character_
    )
  )

# Now assign based on entry type, with forced-level and excluded group handling
otl_full <- otl_full %>%
  mutate(
    final_level = case_when(
      # Forced-level override (species/genus already handled via lookups)
      otl_forced & entry_type == "species" ~ sp_final_level,
      otl_forced & entry_type == "genus" ~ gen_final_level,
      otl_forced & !is.na(otl_forced_level) ~ otl_forced_level,
      # Entirely excluded group: all entries collapse to group level
      !is.na(excl_grp) ~ excl_grp,
      # Normal entries
      entry_type == "species"  ~ sp_final_level,
      entry_type == "genus"    ~ gen_final_level,
      entry_type == "family"   ~ "f",
      entry_type == "order"    ~ "o",
      entry_type == "subclass" ~ "sc",
      entry_type == "class"    ~ "c",
      entry_type == "phylum"   ~ "p"
    ),
    final_level_label = LEVEL_LABEL[final_level],
    OTU_name = case_when(
      # Forced-level order entries (v13: Oligochaeta orders -> subclass)
      otl_forced & entry_type == "order" & otl_forced_level == "sc" ~
        paste0(to_proper(subclass), " Gen. sp."),
      # Forced-level family entries: OTU at forced level
      otl_forced & entry_type == "family" & otl_forced_level == "o" ~
        paste0(to_proper(order), " Gen. sp."),
      otl_forced & entry_type == "family" & otl_forced_level == "sc" ~
        paste0(to_proper(subclass), " Gen. sp."),
      otl_forced & entry_type == "family" & otl_forced_level == "f" ~
        paste0(to_proper(family), " Gen. sp."),
      otl_forced & entry_type == "family" & otl_forced_level == "p" ~
        paste0(to_proper(phylum), " Gen. sp."),
      # Forced species/genus: already handled via lookups
      otl_forced & entry_type == "species" ~ sp_OTU_name,
      otl_forced & entry_type == "genus"   ~ gen_OTU_name,
      # Entirely excluded group
      !is.na(excl_grp) ~ paste0(excl_grp_name, " Gen. sp."),
      # Normal entries
      entry_type == "species"  ~ sp_OTU_name,
      entry_type == "genus"    ~ gen_OTU_name,
      entry_type == "family"   ~ paste0(to_proper(family), " Gen. sp."),
      entry_type == "order"    ~ paste0(to_proper(order), " Gen. sp."),
      entry_type == "subclass" ~ paste0(to_proper(subclass), " Gen. sp."),
      entry_type == "class"    ~ paste0(to_proper(class), " Gen. sp."),
      entry_type == "phylum"   ~ paste0(to_proper(phylum), " Gen. sp.")
    ),
    metric_group = case_when(
      otl_forced                ~ NA_character_,
      !is.na(excl_grp)         ~ NA_character_,
      entry_type == "species"  ~ sp_metric_group,
      entry_type == "genus"    ~ gen_metric_group,
      TRUE ~ NA_character_
    ),
    # EQR exclusion logic:
    # - Forced-level entries: always excluded
    # - Entirely excluded groups: always excluded
    # - Species/genus: from lookup (based on excl_family_pool)
    # - Family: excluded if family in excl_family_pool
    # - Order+: excluded unless truly aquatic
    # Oligochaeta and Neuroptera forced entries are NOT excluded from EQR
    otl_forced_keeps_eqr = otl_forced & (!is.na(subclass) & subclass == "OLIGOCHAETA" |
                                          !is.na(order) & order == "NEUROPTERA"),
    excluded_from_EQR = case_when(
      otl_forced & otl_forced_keeps_eqr ~ FALSE,
      otl_forced               ~ TRUE,
      !is.na(excl_grp)         ~ TRUE,
      entry_type == "species"  ~ sp_excluded,
      entry_type == "genus"    ~ gen_excluded,
      entry_type == "family"   ~ family %in% excl_family_pool,
      entry_type == "order"    ~ !(order %in% TRULY_AQUATIC_ORDERS |
                                     class %in% TRULY_AQUATIC_CLASSES),
      entry_type == "subclass" ~ !(class %in% TRULY_AQUATIC_CLASSES),
      entry_type == "class"    ~ !(class %in% TRULY_AQUATIC_CLASSES),
      entry_type == "phylum"   ~ TRUE
    ),
    decision_path = case_when(
      # Forced-level entries (species/genus have their own decision paths)
      otl_forced & entry_type == "species" ~ sp_decision_path,
      otl_forced & entry_type == "genus"   ~ gen_decision_path,
      otl_forced ~ paste0(
        str_to_title(entry_type), " entry -> FORCED to ",
        LEVEL_LABEL[final_level],
        ifelse(excluded_from_EQR, " [EXCLUDED from EQR]", ""),
        " -> FINAL: ",
        OTU_name, " (", final_level_label, ")"
      ),
      # Entirely excluded group (all entry types)
      !is.na(excl_grp) & entry_type == "species" ~ sp_decision_path,
      !is.na(excl_grp) & entry_type == "genus"   ~ gen_decision_path,
      !is.na(excl_grp) ~ paste0(
        str_to_title(entry_type), " entry -> entirely excluded group (",
        excl_grp_name, " at ", LEVEL_LABEL[excl_grp],
        " level) -> FINAL: ", OTU_name, " (", final_level_label,
        ") [EXCLUDED from EQR]"
      ),
      # Normal species/genus
      entry_type == "species" ~ sp_decision_path,
      entry_type == "genus"   ~ gen_decision_path,
      # Normal higher-level entries
      TRUE ~ paste0(
        str_to_title(entry_type),
        " entry -> always assigned at ", entry_type,
        " level -> FINAL: ", OTU_name, " (", final_level_label, ")",
        ifelse(excluded_from_EQR, " [EXCLUDED from EQR]", "")
      )
    )
  ) %>%
  # Drop temporary join columns
  select(-sp_final_level, -sp_final_level_lbl, -sp_OTU_name,
         -sp_metric_group, -sp_excluded, -sp_excl_grp, -sp_excl_grp_name,
         -sp_forced, -sp_decision_path,
         -gen_final_level, -gen_final_level_lbl, -gen_OTU_name,
         -gen_metric_group, -gen_excluded, -gen_excl_grp, -gen_excl_grp_name,
         -gen_forced, -gen_decision_path,
         -excl_grp, -excl_grp_name, -otl_forced, -otl_forced_level,
         -otl_forced_keeps_eqr)

# ---- Subspecies fallback (v11) ----
# Subspecies entries (containing "subsp.") in OTL_final that are not in the
# Specialist_taxalist inherit their OTU from the parent species.
# Non-breaking space (char 160) may appear before "subsp." — normalize first.
n_unmatched_pre <- sum(is.na(otl_full$OTU_name))
if (n_unmatched_pre > 0) {
  # Extract parent species name: first two words after normalizing NBSP
  otl_full <- otl_full %>%
    mutate(
      is_subsp = is.na(OTU_name) & grepl("subsp", validated_name, fixed = TRUE),
      parent_species = ifelse(is_subsp,
        word(gsub("\u00A0", " ", validated_name), 1, 2),
        NA_character_)
    )

  # Join parent species OTU from species_lookup
  subsp_match <- otl_full %>%
    filter(is_subsp & !is.na(parent_species)) %>%
    left_join(
      species_lookup %>%
        select(validated_name,
               parent_final_level = final_level,
               parent_OTU_name    = OTU_name,
               parent_excluded    = excluded_from_EQR,
               parent_decision    = decision_path),
      by = c("parent_species" = "validated_name")
    ) %>%
    filter(!is.na(parent_OTU_name)) %>%
    select(validated_name,
           parent_final_level, parent_OTU_name, parent_excluded, parent_decision)

  if (nrow(subsp_match) > 0) {
    otl_full <- otl_full %>%
      left_join(subsp_match, by = "validated_name") %>%
      mutate(
        OTU_name = ifelse(is_subsp & !is.na(parent_OTU_name), parent_OTU_name, OTU_name),
        final_level = ifelse(is_subsp & !is.na(parent_final_level), parent_final_level, final_level),
        final_level_label = ifelse(is_subsp & !is.na(parent_final_level),
                                   LEVEL_LABEL[parent_final_level], final_level_label),
        excluded_from_EQR = ifelse(is_subsp & !is.na(parent_excluded), parent_excluded, excluded_from_EQR),
        decision_path = ifelse(is_subsp & !is.na(parent_decision),
          paste0("Subspecies -> inherits from parent '", parent_species,
                 "' -> ", parent_decision),
          decision_path)
      ) %>%
      select(-parent_final_level, -parent_OTU_name, -parent_excluded, -parent_decision)

    cat(sprintf("  %d subspecies entries matched to parent species:\n", nrow(subsp_match)))
    for (i in seq_len(nrow(subsp_match))) {
      cat(sprintf("    %s -> %s\n", subsp_match$validated_name[i], subsp_match$parent_OTU_name[i]))
    }
  }

  otl_full <- otl_full %>% select(-is_subsp, -parent_species)
}

# ---- Fallback for unmatched genus/species entries ----
# These are genera or species in OTL_final that have no counterpart in the
# Specialist_taxalist. Assign OTU based on forced rules, excluded group, or family context.
n_unmatched <- sum(is.na(otl_full$OTU_name))
if (n_unmatched > 0) {
  cat(sprintf("  %d OTL_final entries not in Specialist_taxalist - assigning by context:\n",
              n_unmatched))
  otl_full <- otl_full %>%
    mutate(
      # v13: For unmatched entries, determine forced level based on taxonomy
      # Oligochaeta, Stenostomidae, and Neuroptera need explicit handling;
      # other excluded families are handled by the general exclusion rule
      fallback_forced_level = case_when(
        !is.na(OTU_name) ~ NA_character_,
        # Oligochaeta (including order-level entries, v13)
        !is.na(subclass) & subclass == "OLIGOCHAETA" &
          !is.na(family) & family == "NAIDIDAE" ~ "f",
        !is.na(subclass) & subclass == "OLIGOCHAETA" ~ "sc",
        # Stenostomidae -> phylum
        !is.na(family) & family == "STENOSTOMIDAE" ~ "p",
        # Neuroptera -> family (v13)
        !is.na(order) & order == "NEUROPTERA" ~ "f",
        # General exclusion rule: if family is in excl_family_pool -> family
        !is.na(family) & family %in% excl_family_pool ~ "f",
        TRUE ~ NA_character_
      ),
      fallback_forced = !is.na(fallback_forced_level),
      fallback_excl_grp = case_when(
        is.na(OTU_name) & !fallback_forced &
          phylum %in% entirely_excl_phyla        ~ "p",
        is.na(OTU_name) & !fallback_forced &
          class %in% entirely_excl_classes       ~ "c",
        is.na(OTU_name) & !fallback_forced &
          subclass %in% entirely_excl_subclasses ~ "sc",
        is.na(OTU_name) & !fallback_forced &
          order %in% entirely_excl_orders        ~ "o",
        TRUE ~ NA_character_
      ),
      fallback_excl_name = case_when(
        fallback_excl_grp == "p"  ~ to_proper(phylum),
        fallback_excl_grp == "c"  ~ to_proper(class),
        fallback_excl_grp == "sc" ~ to_proper(subclass),
        fallback_excl_grp == "o"  ~ to_proper(order),
        TRUE ~ NA_character_
      ),
      # Assign OTU for unmatched entries
      OTU_name = case_when(
        !is.na(OTU_name) ~ OTU_name,
        fallback_forced & fallback_forced_level == "p" ~ paste0(to_proper(phylum), " Gen. sp."),
        fallback_forced & fallback_forced_level == "sc" ~ paste0(to_proper(subclass), " Gen. sp."),
        fallback_forced & fallback_forced_level == "o" ~ paste0(to_proper(order), " Gen. sp."),
        fallback_forced & fallback_forced_level == "f" ~ paste0(to_proper(family), " Gen. sp."),
        !is.na(fallback_excl_grp) ~ paste0(fallback_excl_name, " Gen. sp."),
        !is.na(family) ~ paste0(to_proper(family), " Gen. sp."),
        TRUE ~ OTU_name
      ),
      final_level = case_when(
        !is.na(final_level) ~ final_level,
        fallback_forced ~ fallback_forced_level,
        !is.na(fallback_excl_grp) ~ fallback_excl_grp,
        !is.na(family) ~ "f",
        TRUE ~ final_level
      ),
      final_level_label = ifelse(is.na(final_level_label) & !is.na(final_level),
                                 LEVEL_LABEL[final_level], final_level_label),
      # Oligochaeta and Neuroptera forced entries keep EQR
      fallback_keeps_eqr = fallback_forced & (!is.na(subclass) & subclass == "OLIGOCHAETA" |
                                               !is.na(order) & order == "NEUROPTERA"),
      excluded_from_EQR = case_when(
        !is.na(excluded_from_EQR) ~ excluded_from_EQR,
        fallback_forced & fallback_keeps_eqr ~ FALSE,
        fallback_forced ~ TRUE,
        !is.na(fallback_excl_grp) ~ TRUE,
        !is.na(family) ~ family %in% excl_family_pool,
        TRUE ~ FALSE
      ),
      decision_path = case_when(
        !is.na(decision_path) ~ decision_path,
        fallback_forced ~ paste0(
          "Not in Specialist_taxalist -> FORCED to ",
          LEVEL_LABEL[fallback_forced_level],
          " -> FINAL: ", OTU_name, " (", LEVEL_LABEL[fallback_forced_level], ")",
          ifelse(excluded_from_EQR, " [EXCLUDED from EQR]", "")
        ),
        !is.na(fallback_excl_grp) ~ paste0(
          "Not in Specialist_taxalist -> entirely excluded group (",
          fallback_excl_name, ") -> FINAL: ", OTU_name,
          " (", LEVEL_LABEL[final_level], ") [EXCLUDED from EQR]"
        ),
        !is.na(family) ~ paste0(
          "Not in Specialist_taxalist -> assigned by family context -> FINAL: ",
          OTU_name, " (family)",
          ifelse(excluded_from_EQR, " [EXCLUDED from EQR]", "")
        ),
        TRUE ~ decision_path
      )
    ) %>%
    select(-fallback_excl_grp, -fallback_excl_name, -fallback_forced,
           -fallback_forced_level)

  unmatched <- otl_full %>%
    filter(str_detect(decision_path, "Not in Specialist_taxalist", negate = FALSE)) %>%
    select(validated_name, entry_type, family, OTU_name, excluded_from_EQR)
  for (i in seq_len(nrow(unmatched))) {
    cat(sprintf("    %s (%s) -> %s%s\n",
                unmatched$validated_name[i],
                unmatched$entry_type[i],
                unmatched$OTU_name[i],
                ifelse(unmatched$excluded_from_EQR[i], " [EXCLUDED]", "")))
  }

  # Any still unmatched (no family)?
  still_missing <- sum(is.na(otl_full$OTU_name))
  if (still_missing > 0) {
    cat(sprintf("  WARNING: %d entries still unmatched (no family information)\n",
                still_missing))
  }
} else {
  cat("  All OTL_final entries matched directly.\n")
}

# ---- Sort by biological complexity ----
otl_full <- otl_full %>%
  sort_by_complexity() %>%
  mutate(sort_id = row_number())

# ---- Derive boolean exclusion flags from manual columns (v19) ----
# The manual dsfi_exclusion / index_exclusion columns in OTL_final use
# the string "EXCLUDE" (or NA/blank). Convert to booleans for downstream
# filtering. If either column is missing from the input, the boolean
# defaults to FALSE.
norm_flag <- function(x) {
  x <- ifelse(is.na(x), "", trimws(as.character(x)))
  x == "EXCLUDE"
}
otl_full$excluded_from_DSFI <- if ("dsfi_exclusion" %in% names(otl_full)) {
  norm_flag(otl_full$dsfi_exclusion)
} else FALSE
otl_full$excluded_from_indices <- if ("index_exclusion" %in% names(otl_full)) {
  norm_flag(otl_full$index_exclusion)
} else FALSE

# ---- Select output columns ----
otl_output <- otl_full %>%
  select(
    sort_id,
    # Original OTL_final columns
    phylum, subphylum, class, subclass, order, family, subfamily,
    genus, original_name, validated_name,
    taxonomic_authority,
    # Manual assignments preserved from the input file (v18)
    any_of(c("specialist_level", "OTL_name", "OTL-ID", "OTL-level")),
    # Per-row exclusion flags (v18; raw string form from the input)
    any_of(c("dsfi_exclusion", "index_exclusion")),
    # Boolean exclusion flags (v19; derived from the above)
    excluded_from_DSFI, excluded_from_indices,
    entry_type,
    # OTU assignment (script-derived)
    final_level, final_level_label,
    OTL_name_script = OTU_name,
    # Metric info (excluded_from_EQR is the SCRIPT-derived decision-tree
    # exclusion, kept alongside the manual flags above for QA)
    metric_group,
    excluded_from_EQR_script = excluded_from_EQR,
    # Original flags
    any_of(c("DSFI", "BMWP score",
             "rare", "non-native",
             "LT_conservation_status", "LT_red_list", "EU_red_list",
             "prefered habitat", "ID-fwe", "Reference",
             "barcode_available", "sequenced_genes",
             "extra_sequenced_genes", "note")),
    # Decision path
    decision_path
  )


# ============================================================================
# HIERARCHICAL OTL (hierarchical approach)
# ============================================================================
# Built from the FULL OTL_final taxonomy. For each taxonomic group, generate
# fallback entries at all coarser levels. This accommodates damaged, juvenile,
# or poorly preserved specimens that cannot be identified to the OTU level.
#
# v9 changes:
#   - Entirely excluded groups: all entries collapse to group-level OTU
#   - Truly aquatic rule: order+ entries excluded from EQR unless truly aquatic
#   - Family entries: excluded from EQR if family in excl_family_pool

cat("\n=== Generating hierarchical OTL entries ===\n")

# Set of OTU names assigned by the decision tree (species + genus lookups)
assigned_otus <- unique(c(spec$OTU_name,
                          genus_lookup$genus_OTU_name))

# Use OTL_final as the source for full taxonomy
otl_src <- otl_full

# Extract base genus for genus entries
otl_src <- otl_src %>%
  mutate(base_genus = ifelse(entry_type == "genus",
                             validated_name,
                             str_extract(validated_name, "^[^\\s(]+")),
         is_eristalinae = (!is.na(family) & family == "SYRPHIDAE" &
                             base_genus %in% eristalinae_genera))

# Helper: determine excluded group for a given row
get_excl_grp <- function(phylum, class, subclass, order) {
  case_when(
    phylum %in% entirely_excl_phyla        ~ "p",
    class %in% entirely_excl_classes       ~ "c",
    subclass %in% entirely_excl_subclasses ~ "sc",
    order %in% entirely_excl_orders        ~ "o",
    TRUE ~ NA_character_
  )
}

get_excl_grp_name <- function(phylum, class, subclass, order, excl_grp) {
  case_when(
    excl_grp == "p"  ~ to_proper(phylum),
    excl_grp == "c"  ~ to_proper(class),
    excl_grp == "sc" ~ to_proper(subclass),
    excl_grp == "o"  ~ to_proper(order),
    TRUE ~ NA_character_
  )
}

# ---- Phylum-level entries ----
ph_entries <- otl_src %>%
  distinct(phylum) %>%
  transmute(
    phylum,
    subphylum = NA_character_, class = NA_character_,
    subclass = NA_character_, order = NA_character_,
    family = NA_character_, subfamily = NA_character_,
    genus = NA_character_, species = NA_character_,
    excl_grp = ifelse(phylum %in% entirely_excl_phyla, "p", NA_character_),
    OTU_name = paste0(to_proper(phylum), " Gen. sp."),
    OTU_level = "p", OTU_level_label = "phylum",
    excluded_from_EQR = TRUE  # all phylum entries excluded from EQR
  ) %>%
  select(-excl_grp)

# ---- Class-level entries ----
cl_entries <- otl_src %>%
  filter(!is.na(class)) %>%
  distinct(phylum, class) %>%
  mutate(excl_grp = ifelse(phylum %in% entirely_excl_phyla, "p", NA_character_)) %>%
  transmute(
    phylum, subphylum = NA_character_, class,
    subclass = NA_character_, order = NA_character_,
    family = NA_character_, subfamily = NA_character_,
    genus = NA_character_, species = NA_character_,
    OTU_name = case_when(
      excl_grp == "p" ~ paste0(to_proper(phylum), " Gen. sp."),
      TRUE ~ paste0(to_proper(class), " Gen. sp.")
    ),
    OTU_level = ifelse(!is.na(excl_grp), excl_grp, "c"),
    OTU_level_label = LEVEL_LABEL[OTU_level],
    excluded_from_EQR = case_when(
      !is.na(excl_grp) ~ TRUE,
      class %in% entirely_excl_classes ~ TRUE,
      class %in% TRULY_AQUATIC_CLASSES ~ FALSE,
      TRUE ~ TRUE  # class is order+ level, non-truly-aquatic -> excluded
    )
  )

# ---- Subclass-level entries ----
sc_entries <- otl_src %>%
  filter(!is.na(subclass)) %>%
  distinct(phylum, class, subclass) %>%
  mutate(
    excl_grp = case_when(
      phylum %in% entirely_excl_phyla  ~ "p",
      class %in% entirely_excl_classes ~ "c",
      TRUE ~ NA_character_
    ),
    excl_grp_name = case_when(
      excl_grp == "p" ~ to_proper(phylum),
      excl_grp == "c" ~ to_proper(class),
      TRUE ~ NA_character_
    )
  ) %>%
  transmute(
    phylum, subphylum = NA_character_, class, subclass,
    order = NA_character_, family = NA_character_,
    subfamily = NA_character_,
    genus = NA_character_, species = NA_character_,
    OTU_name = case_when(
      !is.na(excl_grp) ~ paste0(excl_grp_name, " Gen. sp."),
      TRUE ~ paste0(to_proper(subclass), " Gen. sp.")
    ),
    OTU_level = ifelse(!is.na(excl_grp), excl_grp, "sc"),
    OTU_level_label = LEVEL_LABEL[OTU_level],
    excluded_from_EQR = case_when(
      !is.na(excl_grp) ~ TRUE,
      subclass %in% entirely_excl_subclasses ~ TRUE,
      class %in% TRULY_AQUATIC_CLASSES ~ FALSE,
      TRUE ~ TRUE  # subclass is order+ level, non-truly-aquatic -> excluded
    )
  )

# ---- Order-level entries ----
# v13: Oligochaeta order entries forced to subclass level
ord_entries <- otl_src %>%
  filter(!is.na(order)) %>%
  distinct(phylum, class, subclass, order) %>%
  mutate(
    excl_grp = case_when(
      phylum %in% entirely_excl_phyla        ~ "p",
      class %in% entirely_excl_classes       ~ "c",
      subclass %in% entirely_excl_subclasses ~ "sc",
      order %in% entirely_excl_orders        ~ "o",
      TRUE ~ NA_character_
    ),
    excl_grp_name = case_when(
      excl_grp == "p"  ~ to_proper(phylum),
      excl_grp == "c"  ~ to_proper(class),
      excl_grp == "sc" ~ to_proper(subclass),
      excl_grp == "o"  ~ to_proper(order),
      TRUE ~ NA_character_
    ),
    # v13: Oligochaeta orders forced to subclass (NOT excluded from EQR)
    forced_lvl = case_when(
      !is.na(excl_grp) ~ NA_character_,
      !is.na(subclass) & subclass == "OLIGOCHAETA" ~ "sc",
      TRUE ~ NA_character_
    ),
    is_forced = !is.na(forced_lvl)
  ) %>%
  transmute(
    phylum, subphylum = NA_character_, class, subclass, order,
    family = NA_character_, subfamily = NA_character_,
    genus = NA_character_, species = NA_character_,
    OTU_name = case_when(
      is_forced & forced_lvl == "sc" ~ paste0(to_proper(subclass), " Gen. sp."),
      !is.na(excl_grp) ~ paste0(excl_grp_name, " Gen. sp."),
      TRUE ~ paste0(to_proper(order), " Gen. sp.")
    ),
    OTU_level = case_when(
      is_forced ~ forced_lvl,
      !is.na(excl_grp) ~ excl_grp,
      TRUE ~ "o"
    ),
    OTU_level_label = LEVEL_LABEL[OTU_level],
    excluded_from_EQR = case_when(
      is_forced & !is.na(subclass) & subclass == "OLIGOCHAETA" ~ FALSE,
      is_forced ~ TRUE,
      !is.na(excl_grp) ~ TRUE,
      order %in% TRULY_AQUATIC_ORDERS  ~ FALSE,
      class %in% TRULY_AQUATIC_CLASSES ~ FALSE,
      TRUE ~ TRUE  # order+ level, non-truly-aquatic -> excluded
    )
  )

# ---- Family-level entries ----
fam_entries <- otl_src %>%
  filter(!is.na(family)) %>%
  distinct(phylum, class, subclass, order, family) %>%
  mutate(
    # v13: Detect forced-level families — Oligochaeta, Stenostomidae, Neuroptera
    # need explicit handling. Other excluded families stay at family level
    # naturally; their EQR exclusion is via excl_family_pool.
    # Check entirely excluded group FIRST so it takes precedence.
    excl_grp = case_when(
      phylum %in% entirely_excl_phyla        ~ "p",
      class %in% entirely_excl_classes       ~ "c",
      subclass %in% entirely_excl_subclasses ~ "sc",
      order %in% entirely_excl_orders        ~ "o",
      TRUE ~ NA_character_
    ),
    forced_lvl = case_when(
      !is.na(excl_grp) ~ NA_character_,  # entirely excluded group takes precedence
      subclass == "OLIGOCHAETA" & family == "NAIDIDAE" ~ "f",
      subclass == "OLIGOCHAETA" ~ "sc",
      family == "STENOSTOMIDAE" ~ "p",
      order == "NEUROPTERA" ~ "f",  # v13: Neuroptera forced to family
      TRUE ~ NA_character_
    ),
    is_forced = !is.na(forced_lvl),
    excl_grp_name = case_when(
      excl_grp == "p"  ~ to_proper(phylum),
      excl_grp == "c"  ~ to_proper(class),
      excl_grp == "sc" ~ to_proper(subclass),
      excl_grp == "o"  ~ to_proper(order),
      TRUE ~ NA_character_
    )
  ) %>%
  transmute(
    phylum, subphylum = NA_character_, class, subclass, order, family,
    subfamily = NA_character_,
    genus = NA_character_, species = NA_character_,
    OTU_name = case_when(
      is_forced & forced_lvl == "p"  ~ paste0(to_proper(phylum), " Gen. sp."),
      is_forced & forced_lvl == "sc" ~ paste0(to_proper(subclass), " Gen. sp."),
      is_forced & forced_lvl == "o"  ~ paste0(to_proper(order), " Gen. sp."),
      is_forced & forced_lvl == "f"  ~ paste0(to_proper(family), " Gen. sp."),
      !is.na(excl_grp) ~ paste0(excl_grp_name, " Gen. sp."),
      TRUE ~ paste0(to_proper(family), " Gen. sp.")
    ),
    OTU_level = case_when(
      is_forced ~ forced_lvl,
      !is.na(excl_grp) ~ excl_grp,
      TRUE ~ "f"
    ),
    OTU_level_label = LEVEL_LABEL[OTU_level],
    excluded_from_EQR = case_when(
      is_forced & subclass == "OLIGOCHAETA" ~ FALSE,  # Oligochaeta keeps EQR
      is_forced & order == "NEUROPTERA"     ~ FALSE,  # v13: Neuroptera keeps EQR
      is_forced                 ~ TRUE,
      !is.na(excl_grp)          ~ TRUE,
      family %in% excl_family_pool ~ TRUE,
      TRUE ~ FALSE
    )
  )

# ---- Subfamily-level entries (Eristalinae only) ----
sf_entries <- otl_src %>%
  filter(!is.na(subfamily) & subfamily == "Eristalinae") %>%
  distinct(phylum, class, subclass, order, family) %>%
  transmute(
    phylum, subphylum = NA_character_, class, subclass, order, family,
    subfamily = "Eristalinae",
    genus = NA_character_, species = NA_character_,
    OTU_name = "Eristalinae Gen. sp.",
    OTU_level = "sf", OTU_level_label = "subfamily",
    excluded_from_EQR = FALSE
  )

# ---- Genus-level entries ----
gen_entries <- otl_src %>%
  # v20: only species or genus rows have a meaningful genus. Filtering
  # before the distinct() prevents higher-rank rows (where base_genus
  # is the higher-rank name) from generating spurious "<Higher> sp."
  # entries (e.g. "Platyhelminthes sp." from the phylum-level row).
  filter(entry_type %in% c("species", "genus")) %>%
  distinct(phylum, class, subclass, order, family, is_eristalinae, base_genus) %>%
  mutate(
    # v13: Check entirely excluded group FIRST, then forced-level overrides
    # Oligochaeta, Stenostomidae, Neuroptera, and excluded-family genera need forcing.
    excl_grp = case_when(
      phylum %in% entirely_excl_phyla        ~ "p",
      class %in% entirely_excl_classes       ~ "c",
      subclass %in% entirely_excl_subclasses ~ "sc",
      order %in% entirely_excl_orders        ~ "o",
      TRUE ~ NA_character_
    ),
    forced_lvl = case_when(
      !is.na(excl_grp) ~ NA_character_,  # entirely excluded group takes precedence
      subclass == "OLIGOCHAETA" & family == "NAIDIDAE" ~ "f",
      subclass == "OLIGOCHAETA" ~ "sc",
      family == "STENOSTOMIDAE" ~ "p",
      order == "NEUROPTERA" ~ "f",  # v13: Neuroptera forced to family
      # General exclusion rule: genus in excluded family -> family level
      family %in% excl_family_pool ~ "f",
      TRUE ~ NA_character_
    ),
    is_forced = !is.na(forced_lvl),
    excl_grp_name = case_when(
      excl_grp == "p"  ~ to_proper(phylum),
      excl_grp == "c"  ~ to_proper(class),
      excl_grp == "sc" ~ to_proper(subclass),
      excl_grp == "o"  ~ to_proper(order),
      TRUE ~ NA_character_
    )
  ) %>%
  transmute(
    phylum, subphylum = NA_character_, class, subclass, order, family,
    subfamily = ifelse(is_eristalinae, "Eristalinae", NA_character_),
    genus = base_genus, species = NA_character_,
    OTU_name = case_when(
      is_forced & forced_lvl == "p"  ~ paste0(to_proper(phylum), " Gen. sp."),
      is_forced & forced_lvl == "sc" ~ paste0(to_proper(subclass), " Gen. sp."),
      is_forced & forced_lvl == "o"  ~ paste0(to_proper(order), " Gen. sp."),
      is_forced & forced_lvl == "f"  ~ paste0(to_proper(family), " Gen. sp."),
      !is.na(excl_grp) ~ paste0(excl_grp_name, " Gen. sp."),
      TRUE ~ paste0(base_genus, " sp.")
    ),
    OTU_level = case_when(
      is_forced ~ forced_lvl,
      !is.na(excl_grp) ~ excl_grp,
      TRUE ~ "g"
    ),
    OTU_level_label = LEVEL_LABEL[OTU_level],
    excluded_from_EQR = case_when(
      is_forced & subclass == "OLIGOCHAETA" ~ FALSE,  # Oligochaeta keeps EQR
      is_forced & order == "NEUROPTERA"     ~ FALSE,  # v13: Neuroptera keeps EQR
      is_forced                 ~ TRUE,
      !is.na(excl_grp)          ~ TRUE,
      family %in% excl_family_pool ~ TRUE,
      TRUE ~ FALSE
    )
  )

# ---- Species-level entries (those assigned at species level by the decision tree) ----
sp_entries <- spec %>%
  filter(final_level == "s") %>%
  distinct(OTU_name, .keep_all = TRUE) %>%
  transmute(
    phylum, subphylum = NA_character_, class, subclass, order, family,
    subfamily = ifelse(is_eristalinae, "Eristalinae", NA_character_),
    genus = base_genus, species,
    OTU_name,
    OTU_level = "s", OTU_level_label = "species",
    excluded_from_EQR
  )

# ---- Combine all entries ----
hierarchical_otl <- bind_rows(
  ph_entries, cl_entries, sc_entries, ord_entries,
  fam_entries, sf_entries, gen_entries, sp_entries
)

# Mark entry type: "OTU" if the name matches a decision-tree assignment, else "fallback"
hierarchical_otl <- hierarchical_otl %>%
  mutate(entry_type = ifelse(OTU_name %in% assigned_otus, "OTU", "fallback"))

# ---- Sort by biological complexity (uses shared sort_by_complexity function) ----
hierarchical_otl <- hierarchical_otl %>%
  sort_by_complexity() %>%
  mutate(sort_id = row_number()) %>%
  select(sort_id, everything())

# ---- OTL_id: unique sequential ID per distinct OTU_name (v19) ----
# Rows that share an OTU_name (e.g. multiple class-level rows that all
# collapse to "Polychaeta Gen. sp.") get the same OTL_id. IDs are
# assigned in order of first appearance after the complexity sort, so
# they roughly track phylum-to-species ordering.
hierarchical_otl <- hierarchical_otl %>%
  mutate(OTL_id = match(OTU_name, unique(OTU_name))) %>%
  relocate(OTL_id, .after = sort_id)

# v20 patch: propagate OTL_id to otl_output too, so every OTL_final
# row carries the same per-OTU identifier. Joined via the script-
# derived OTL_name_script (which is the same string used to compute
# OTL_id in hierarchical_otl).
otu_id_lookup <- hierarchical_otl %>%
  distinct(OTU_name, OTL_id)

otl_output <- otl_output %>%
  left_join(otu_id_lookup, by = c("OTL_name_script" = "OTU_name")) %>%
  relocate(OTL_id, .after = sort_id)

cat(sprintf("  Total hierarchical OTL entries: %d\n", nrow(hierarchical_otl)))
cat(sprintf("    OTU entries: %d\n", sum(hierarchical_otl$entry_type == "OTU")))
cat(sprintf("    Fallback entries: %d\n", sum(hierarchical_otl$entry_type == "fallback")))
cat(sprintf("    Excluded from EQR: %d\n", sum(hierarchical_otl$excluded_from_EQR)))

# Entry count by level
otl_level_dist <- hierarchical_otl %>%
  count(OTU_level_label, entry_type) %>%
  arrange(match(OTU_level_label,
                c("phylum", "class", "subclass", "order", "family",
                  "subfamily", "genus", "species")))
for (i in seq_len(nrow(otl_level_dist))) {
  cat(sprintf("    %-12s %-10s %4d\n",
              otl_level_dist$OTU_level_label[i],
              otl_level_dist$entry_type[i],
              otl_level_dist$n[i]))
}


# ============================================================================
# SUMMARY STATISTICS
# ============================================================================

cat("\n=== SUMMARY ===\n")

# Final level distribution (from specialist output)
cat("\nDecision tree OTU level distribution (Specialist_taxalist, species):\n")
level_dist <- specialist_output %>%
  count(final_level_label) %>%
  arrange(match(final_level_label,
                c("species", "genus", "subfamily", "family",
                  "order", "subclass", "class", "phylum")))
for (i in seq_len(nrow(level_dist))) {
  cat(sprintf("  %-12s %4d taxa (%.1f%%)\n",
              level_dist$final_level_label[i],
              level_dist$n[i],
              100 * level_dist$n[i] / nrow(specialist_output)))
}

# Excluded group breakdown
cat("\nEntirely excluded group assignments:\n")
excl_group_dist <- specialist_output %>%
  filter(!is.na(excl_group_level)) %>%
  count(excl_group_name, excl_group_level) %>%
  arrange(match(excl_group_level, c("p", "c", "sc", "o")))
if (nrow(excl_group_dist) > 0) {
  for (i in seq_len(nrow(excl_group_dist))) {
    cat(sprintf("  %-20s (%s) %4d taxa\n",
                excl_group_dist$excl_group_name[i],
                LEVEL_LABEL[excl_group_dist$excl_group_level[i]],
                excl_group_dist$n[i]))
  }
}

# OTL_final assignment stats
cat(sprintf("\nOTL_final rows: %d\n", nrow(otl_output)))
cat(sprintf("  Species entries with OTU: %d / %d\n",
            sum(!is.na(otl_output$OTL_name_script) & otl_output$entry_type == "species"),
            sum(otl_output$entry_type == "species")))
cat(sprintf("  Genus entries with OTU: %d / %d\n",
            sum(!is.na(otl_output$OTL_name_script) & otl_output$entry_type == "genus"),
            sum(otl_output$entry_type == "genus")))
cat(sprintf("  Family entries with OTU: %d / %d\n",
            sum(!is.na(otl_output$OTL_name_script) & otl_output$entry_type == "family"),
            sum(otl_output$entry_type == "family")))
cat(sprintf("  Higher-level entries with OTU: %d / %d\n",
            sum(!is.na(otl_output$OTL_name_script) & otl_output$entry_type %in% c("order","subclass","class","phylum")),
            sum(otl_output$entry_type %in% c("order","subclass","class","phylum"))))
cat(sprintf("  Unmatched entries: %d\n", sum(is.na(otl_output$OTL_name_script))))

# Exclusion stats (v19: manual flags from OTL_final, then script-derived for QA)
cat(sprintf("\nExclusion stats — manual flags (OTL_final):\n"))
cat(sprintf("  excluded_from_DSFI    = TRUE for %d / %d rows\n",
            sum(otl_output$excluded_from_DSFI, na.rm = TRUE), nrow(otl_output)))
cat(sprintf("  excluded_from_indices = TRUE for %d / %d rows\n",
            sum(otl_output$excluded_from_indices, na.rm = TRUE), nrow(otl_output)))
cat(sprintf("\nExclusion stats — script-derived (decision tree, for QA):\n"))
cat(sprintf("  Species excluded_from_EQR_script: %d / %d\n",
            sum(otl_output$excluded_from_EQR_script[otl_output$entry_type == "species"], na.rm = TRUE),
            sum(otl_output$entry_type == "species")))
cat(sprintf("  Genus    excluded_from_EQR_script: %d / %d\n",
            sum(otl_output$excluded_from_EQR_script[otl_output$entry_type == "genus"], na.rm = TRUE),
            sum(otl_output$entry_type == "genus")))
cat(sprintf("  Family   excluded_from_EQR_script: %d / %d\n",
            sum(otl_output$excluded_from_EQR_script[otl_output$entry_type == "family"], na.rm = TRUE),
            sum(otl_output$entry_type == "family")))
cat(sprintf("  Order+   excluded_from_EQR_script: %d / %d\n",
            sum(otl_output$excluded_from_EQR_script[otl_output$entry_type %in% c("order","subclass","class","phylum")], na.rm = TRUE),
            sum(otl_output$entry_type %in% c("order","subclass","class","phylum"))))

# Unique OTU names
cat(sprintf("\nUnique OTU names (decision tree): %d\n",
            n_distinct(specialist_output$OTU_name)))
cat(sprintf("Unique OTU names (hierarchical OTL): %d\n",
            n_distinct(hierarchical_otl$OTU_name)))
cat(sprintf("Taxa excluded from EQR (specialist): %d\n",
            sum(specialist_output$excluded_from_EQR)))
cat(sprintf("Overrides for metric compliance: %d\n", sum(specialist_output$override)))

# Create summary data frame
summary_df <- bind_rows(
  level_dist %>%
    mutate(category = "Decision tree OTU level distribution",
           detail = final_level_label,
           value = as.character(n)) %>%
    select(category, detail, value),
  tibble(
    category = "Key counts",
    detail = c("Species in Specialist_taxalist",
               "OTL_final total rows",
               "OTL_final species entries",
               "OTL_final genus entries",
               "OTL_final unmatched entries",
               "Unique OTU names (decision tree)",
               "Unique OTU names (hierarchical OTL)",
               "Taxa excluded from EQR (specialist)",
               "Phase 4 metric compliance overrides",
               "Taxa with forced-level overrides",
               "Taxa in entirely excluded groups",
               "Entirely excluded phyla",
               "Entirely excluded classes",
               "Entirely excluded orders"),
    value = as.character(c(
      nrow(specialist_output),
      nrow(otl_output),
      sum(otl_output$entry_type == "species"),
      sum(otl_output$entry_type == "genus"),
      sum(is.na(otl_output$OTL_name_script)),
      n_distinct(specialist_output$OTU_name),
      n_distinct(hierarchical_otl$OTU_name),
      sum(specialist_output$excluded_from_EQR),
      sum(specialist_output$override),
      sum(specialist_output$forced_excluded),
      sum(!is.na(specialist_output$excl_group_level)),
      paste(entirely_excl_phyla, collapse = ", "),
      paste(entirely_excl_classes, collapse = ", "),
      paste(entirely_excl_orders, collapse = ", ")
    ))
  ),
  specialist_output %>%
    filter(override) %>%
    count(metric_group, name = "n_overrides") %>%
    mutate(category = "Overrides by metric group",
           detail = metric_group,
           value = as.character(n_overrides)) %>%
    select(category, detail, value),
  if (nrow(excl_group_dist) > 0) {
    excl_group_dist %>%
      mutate(category = "Entirely excluded groups",
             detail = paste0(excl_group_name, " (", LEVEL_LABEL[excl_group_level], ")"),
             value = as.character(n)) %>%
      select(category, detail, value)
  } else {
    tibble(category = character(), detail = character(), value = character())
  },
  # ---- OTU counts by taxonomic group (v19) ----
  # For the hierarchical_OTL, count unique OTU_names within each phylum,
  # each class, and each order. Same OTU_name shared across multiple
  # rows (e.g. all entries collapsed to "Polychaeta Gen. sp.") counts
  # once per group.
  hierarchical_otl %>%
    filter(!is.na(phylum)) %>%
    group_by(phylum) %>%
    summarise(n = n_distinct(OTU_name), .groups = "drop") %>%
    arrange(phylum) %>%
    transmute(category = "OTU counts by phylum",
              detail   = phylum,
              value    = as.character(n)),
  hierarchical_otl %>%
    filter(!is.na(class)) %>%
    group_by(phylum, class) %>%
    summarise(n = n_distinct(OTU_name), .groups = "drop") %>%
    arrange(phylum, class) %>%
    transmute(category = "OTU counts by class",
              detail   = paste0(class, " (", phylum, ")"),
              value    = as.character(n)),
  hierarchical_otl %>%
    filter(!is.na(order)) %>%
    group_by(class, order) %>%
    summarise(n = n_distinct(OTU_name), .groups = "drop") %>%
    arrange(class, order) %>%
    transmute(category = "OTU counts by order",
              detail   = paste0(order, " (", class, ")"),
              value    = as.character(n))
)


# ============================================================================
# SAVE OUTPUT
# ============================================================================

# ============================================================================
# QA CHECK — script-derived OTU vs manual OTL_name in OTL_final (v18)
# ============================================================================
# Compare the script-derived OTU against the author's manual OTL_name in
# the OTL_final sheet, row by row. Only rows where OTL_name is actually
# populated by the author are considered; blank manual cells are not
# treated as disagreement (they're rows the author hasn't yet curated).

cat("\n=== QA: script-derived vs manual OTU assignment ===\n")

if ("OTL_name" %in% names(otl_full)) {
  # Normalise empty strings to NA so blank cells don't count as a value.
  qa_otl_final <- otl_full %>%
    transmute(
      sort_id, validated_name, entry_type,
      OTL_name_manual = ifelse(is.na(OTL_name) |
                               trimws(as.character(OTL_name)) == "",
                               NA_character_,
                               trimws(as.character(OTL_name))),
      OTL_name_script = OTU_name,
      OTL_match = !is.na(OTL_name_manual) & !is.na(OTL_name_script) &
                  OTL_name_manual == trimws(as.character(OTL_name_script))
    ) %>%
    # v20 patch: bring OTL_id in from hierarchical_OTL so curation-
    # pending rows (blank manual OTL_name) still carry an identifier.
    left_join(otu_id_lookup,
              by = c("OTL_name_script" = "OTU_name")) %>%
    relocate(OTL_id, .after = sort_id) %>%
    # Keep all rows, including those with blank manual OTL_name —
    # those will read as OTL_match = FALSE but still get an OTL_id
    # so the author can curate them in place.
    arrange(OTL_match, sort_id)

  n_total          <- nrow(qa_otl_final)
  n_match          <- sum(qa_otl_final$OTL_match, na.rm = TRUE)
  n_manual_blank   <- sum(is.na(qa_otl_final$OTL_name_manual))
  n_real_mismatch  <- n_total - n_match - n_manual_blank
  cat(sprintf("  OTL_final QA: %d match | %d real mismatch | %d manual blank (script-only) | %d total.\n",
              n_match, n_real_mismatch, n_manual_blank, n_total))
  n_manual_total <- n_total - n_manual_blank
  cat(sprintf("  (legacy view) Manually-populated rows: %d / %d match (%.1f%%).\n",
              n_match, n_manual_total,
              if (n_manual_total > 0) 100 * n_match / n_manual_total else NA_real_))
} else {
  cat("  OTL_final has no OTL_name column — QA against manual skipped.\n")
  qa_otl_final <- tibble()
}

write_xlsx(
  list(
    OTL_final_with_OTU  = otl_output,
    decision_tree_trace = specialist_output,
    hierarchical_OTL    = hierarchical_otl,
    qa_otl_final        = qa_otl_final,
    summary             = summary_df
  ),
  path = output_file
)

cat(sprintf("\nOutput saved to: %s\n", output_file))
cat("Done.\n")
