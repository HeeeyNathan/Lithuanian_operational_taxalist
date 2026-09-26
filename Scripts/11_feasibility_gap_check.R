# ============================================================
# Diagnostic — feasibility gap between specialist agreement and
# metric-required OTU levels (v1)
# ============================================================
# Written in response to Reviewer 1 (manuscript revision, 2026-09).
#
# Part 1 — how often, and where, the OTU is finer than the level all
#   four specialists agreed they could reliably reach (specialist_gap in
#   the script 2 decision trace).
#
# Part 2 — whether the metric-required levels are reached in routine
#   practice. For every LEPA record (2014-2022) in a group with a metric
#   minimum, the level the identifier actually reached (original_level
#   of the record's validated name in the OTL sheet) is compared with the
#   group's general minimum level (reference table, Table 2: genus for
#   Plecoptera, family for the other groups). Records identified only to
#   a coarser level fall back to a coarser hierarchical OTU; the output
#   reports whether that fallback was retained for EQR calculation or
#   excluded (truly-aquatic rule).
#
#   Note: the raw data's `rank` column is the rank of the assigned OTU,
#   not the level the identifier reached, so it is not used here.
#   Genus-level exceptions within family-minimum groups (e.g. Chironomus,
#   Gammarus) cannot be assessed from family-level records and are not
#   counted separately.
#
# Inputs:
#   - Outputs/2_OTU_assignments_v23.xlsx (sheet decision_tree_trace)
#   - Operational Taxalist (OTL)/Supplement 1 - Operational taxalist.xlsx
#     (sheet OTL)
#   - Inputs/All_sites_macroinvertebrate_data_long.xlsx (raw long data)
#
# Outputs:
#   - Outputs/11_feasibility_gap_check.xlsx
#   - Console: headline numbers for the manuscript
# ============================================================

library(readxl)
library(dplyr)
library(writexl)

LEVEL_RANK  <- c(s = 1, g = 2, sf = 3, f = 4, o = 5, sc = 6, c = 7, p = 8)
LEVEL_LABEL <- c(s = "species", g = "genus", sf = "subfamily", f = "family",
                 o = "order", sc = "subclass", c = "class", p = "phylum")
NAME_RANK   <- c(species = 1, subspecies = 1, genus = 2, subfamily = 3,
                 family = 4, order = 5, subclass = 6, class = 7, phylum = 8)

# Normalise names: some cells contain non-breaking spaces.
clean_name <- function(x) trimws(gsub(" ", " ", x))

# ---- Part 1: feasibility gap among specialist-assessed species ------
trace <- read_excel("Outputs/2_OTU_assignments_v23.xlsx",
                    sheet = "decision_tree_trace", guess_max = 10000)

n_species <- nrow(trace)
phase34   <- trace |> filter(is.na(excl_group_level), is.na(forced_level))
phase34   <- phase34 |>
  mutate(outcome = case_when(
    LEVEL_RANK[agreement_level] <  LEVEL_RANK[min_level] ~ "agreement finer than minimum",
    LEVEL_RANK[agreement_level] == LEVEL_RANK[min_level] ~ "agreement equals minimum",
    LEVEL_RANK[agreement_level] >  LEVEL_RANK[min_level] ~ "feasibility gap"
  ))

phase34_outcomes <- phase34 |>
  count(outcome, name = "n_species") |>
  mutate(pct_of_phase34 = round(100 * n_species / nrow(phase34), 1))

gap <- trace |> filter(specialist_gap)
gap_by_group <- gap |>
  mutate(group = ifelse(is.na(metric_group), paste0(tools::toTitleCase(tolower(order)), " (forced)"),
                        metric_group),
         transition = paste(LEVEL_LABEL[agreement_level], "->", LEVEL_LABEL[final_level])) |>
  count(specialist_gap_source, group, transition, excluded_from_EQR, name = "n_species") |>
  arrange(specialist_gap_source, desc(n_species))

cat("=== Part 1: feasibility gap (specialist-assessed species) ===\n")
cat(sprintf("Specialist-assessed species: %d (Phase 2: %d, Phases 3-4: %d)\n",
            n_species, n_species - nrow(phase34), nrow(phase34)))
print(as.data.frame(phase34_outcomes), row.names = FALSE)
cat(sprintf("Species with a feasibility gap: %d / %d (%.1f%%), of which %d retained for EQR\n",
            nrow(gap), n_species, 100 * nrow(gap) / n_species,
            sum(!gap$excluded_from_EQR)))
print(table(gap$specialist_gap_source))

# ---- Part 2: are required levels reached in LEPA data? --------------
otl <- read_excel("Operational Taxalist (OTL)/Supplement 1 - Operational taxalist.xlsx",
                  sheet = "OTL", guess_max = 10000) |>
  transmute(validated_name = clean_name(`validated_name_GBIF+Molluscabase`),
            original_level) |>
  distinct(validated_name, .keep_all = TRUE)

# Same record filters as script 4, except that index-excluded records are
# kept: they are the coarse fallback records this check is about.
all_lf <- read_excel("Inputs/All_sites_macroinvertebrate_data_long.xlsx",
                     sheet = "clean_macroinvertebrate_data",
                     guess_max = 2500) |>
  filter(!grepl("NO MATCH", OTL_taxonname, ignore.case = TRUE),
         grepl("^LTR", site_code),
         year >= 2014, year <= 2022) |>
  mutate(validated_name = clean_name(taxonname_validated),
         excluded       = grepl("EXCLUDE", OTL_note_indices, ignore.case = TRUE)) |>
  left_join(otl, by = "validated_name")

n_unmatched <- sum(is.na(all_lf$original_level))
if (n_unmatched > 0) {
  warning(n_unmatched, " records have no OTL original_level and are skipped")
}

# Group membership and general minimum level (reference table, Table 2).
records <- all_lf |>
  mutate(
    ord = toupper(order), cls = toupper(class),
    group = case_when(
      ord == "DIPTERA"         ~ "Diptera",
      ord == "LEPIDOPTERA"     ~ "Lepidoptera",
      ord == "COLEOPTERA"      ~ "Coleoptera",
      ord == "PLECOPTERA"      ~ "Plecoptera",
      ord == "EPHEMEROPTERA"   ~ "Ephemeroptera",
      ord == "TRICHOPTERA"     ~ "Trichoptera",
      ord == "TRICLADIDA"      ~ "Tricladida",
      cls == "MALACOSTRACA"    ~ "Malacostraca",
      cls == "BIVALVIA"        ~ "Bivalvia",
      cls == "GASTROPODA" & (is.na(ord) | ord != "STYLOMMATOPHORA") ~ "Gastropoda",
      TRUE ~ NA_character_
    ),
    min_rank = ifelse(group == "Plecoptera", LEVEL_RANK[["g"]], LEVEL_RANK[["f"]]),
    id_rank  = NAME_RANK[tolower(original_level)],
    reached  = id_rank <= min_rank
  ) |>
  filter(!is.na(group), !is.na(id_rank))

reached_by_group <- records |>
  group_by(group) |>
  summarise(
    records               = n(),
    samples               = n_distinct(site_code),
    reached_minimum       = sum(reached),
    pct_reached           = round(100 * mean(reached), 1),
    below_minimum         = sum(!reached),
    below_excluded_EQR    = sum(!reached & excluded),
    below_retained_EQR    = sum(!reached & !excluded),
    .groups = "drop"
  ) |>
  arrange(desc(records))

below_detail <- records |>
  filter(!reached) |>
  count(group, taxonname_validated, original_level, OTL_taxonname, excluded,
        name = "records") |>
  arrange(group, desc(records))

tot <- colSums(reached_by_group[, c("records", "reached_minimum", "below_minimum",
                                    "below_excluded_EQR", "below_retained_EQR")])

cat("\n=== Part 2: required levels reached in LEPA data (2014-2022) ===\n")
cat(sprintf("Samples: %d\n", n_distinct(all_lf$site_code)))
print(as.data.frame(reached_by_group), row.names = FALSE)
cat(sprintf("Overall: %d / %d records (%.1f%%) identified at or finer than the minimum;\n",
            tot[["reached_minimum"]], tot[["records"]],
            100 * tot[["reached_minimum"]] / tot[["records"]]))
cat(sprintf("  %d below the minimum -> fallback OTU: %d excluded from EQR, %d retained\n",
            tot[["below_minimum"]], tot[["below_excluded_EQR"]], tot[["below_retained_EQR"]]))

write_xlsx(
  list(
    phase34_outcomes  = phase34_outcomes,
    gap_by_group      = gap_by_group,
    reached_by_group  = reached_by_group,
    below_min_detail  = below_detail
  ),
  "Outputs/11_feasibility_gap_check.xlsx"
)
cat("Saved: Outputs/11_feasibility_gap_check.xlsx\n")

#==================== CLEAN UP WORKSPACE =====================
library(pacman)
rm(list = ls())       # Remove all objects from environment
gc()                  # Frees up unused memory
p_unload(all)         # Unload all loaded packages
graphics.off()        # Close all graphical devices
cat("\014")           # Clear the console
# Clear mind :)
