# ============================================================
# Diagnostic — DEP richness: OTL vs validated taxon names (v1)
# ============================================================
# Tests the hypothesis that residual differences between LEPA's
# published EQRs and our recalculated LRMI come from the #DEP metric
# being lower under OTL standardisation (multiple validated species
# collapsing into a single OTU).
#
# Approach: for every site-year, recompute DEP_richness twice —
#   - DEP_OTL       : grouping by OTL_taxonname (matches our LRMI)
#   - DEP_validated : grouping by taxonname_validated (pre-OTL)
# then report the per-site delta and the implied effect on DEP_EQR
# and LRMI. Mathematically:
#   delta_DEP_EQR    = (DEP_validated - DEP_OTL) / 15
#   delta_LRMI_est   = delta_DEP_EQR / 4         (one of four metrics)
#
# Other LRMI inputs (DSFI, ASPT, %EHP-%CrHi) are invariant to OTL
# vs validated grouping for arithmetic reasons (family is invariant,
# abundance proportions are invariant), so DEP is the only metric
# that can move. If the LEPA-vs-LT residual matches delta_LRMI_est
# in sign and magnitude, the residual is fully explained by DEP.
#
# Inputs:
#   - Inputs/All_sites_macroinvertebrate_data_long.xlsx (raw long data)
#   - Outputs/6_LRMI_with_comparison.xlsx (existing OTL-based LRMI
#     run; we read the LRMI, DEP_richness and LEPA EQR for context)
#
# Outputs:
#   - Outputs/8_DEP_OTL_vs_validated.xlsx  — per site-year table
#   - Console: summary stats and how much of the LEPA residual is
#     explained by the DEP shift.
# ============================================================

library(readxl)
library(dplyr)
library(writexl)

# ---- Read raw long-format data with the SAME filters as script 4 ----
all_lf <- read_excel("Inputs/All_sites_macroinvertebrate_data_long.xlsx",
                     sheet = "clean_macroinvertebrate_data",
                     guess_max = 2500) |>
  filter(
    !grepl("NO MATCH", OTL_taxonname,     ignore.case = TRUE),
    !grepl("EXCLUDE",  OTL_note_indices,  ignore.case = TRUE),
    grepl("^LTR", site_code)
  )

# Sanity: make sure both name columns exist.
stopifnot(all(c("OTL_taxonname", "taxonname_validated", "DEP",
                "order", "family", "site_id", "site_code", "year")
              %in% names(all_lf)))

cat(sprintf("Rows after filter: %d\n", nrow(all_lf)))

# ---- Compute DEP richness two ways per site-year --------------------
# DEP formula (matches script 4):
#   D component = n_distinct families where order == DIPTERA
#   E+P component = number of unique taxa (rows after grouping by name)
#                   where order in {EPHEMEROPTERA, PLECOPTERA}
# Only the E+P component is sensitive to the grouping column choice;
# the D component is computed from the (invariant) family column.

dep_by_grouping <- function(df, name_col) {
  df |>
    filter(DEP %in% TRUE) |>
    group_by(site_id, site_code, year, !!sym(name_col)) |>
    summarise(order  = first(order),
              family = first(family),
              .groups = "drop") |>
    group_by(site_id, site_code, year) |>
    summarise(
      D_richness  = n_distinct(family[toupper(order) == "DIPTERA" & !is.na(family)]),
      EP_richness = sum(toupper(order) %in% c("EPHEMEROPTERA", "PLECOPTERA")),
      DEP         = D_richness + EP_richness,
      .groups = "drop"
    )
}

dep_otl <- dep_by_grouping(all_lf, "OTL_taxonname") |>
  rename(D_OTL = D_richness, EP_OTL = EP_richness, DEP_OTL = DEP)

dep_val <- dep_by_grouping(all_lf, "taxonname_validated") |>
  rename(D_validated = D_richness, EP_validated = EP_richness,
         DEP_validated = DEP)

comparison <- full_join(dep_otl, dep_val,
                        by = c("site_id", "site_code", "year")) |>
  mutate(
    delta_D            = D_validated  - D_OTL,
    delta_EP           = EP_validated - EP_OTL,
    delta_DEP          = DEP_validated - DEP_OTL,
    # Implied movement in DEP_EQR (reference = 15, lower = 0)
    delta_DEP_EQR      = delta_DEP / 15,
    # Implied movement in LRMI (one of four equally-weighted metrics)
    delta_LRMI_implied = delta_DEP_EQR / 4
  )

cat(sprintf("\nSite-years in comparison: %d\n", nrow(comparison)))
cat(sprintf("Sanity — delta_D should be 0 for nearly all rows (family is invariant):\n"))
print(table(comparison$delta_D, useNA = "ifany"))
cat(sprintf("\nDelta EP (validated - OTL):\n"))
print(summary(comparison$delta_EP))
cat(sprintf("\nDelta DEP (validated - OTL):\n"))
print(summary(comparison$delta_DEP))
cat(sprintf("\nImplied delta LRMI from DEP shift:\n"))
print(summary(comparison$delta_LRMI_implied))

# ---- Attribute the LEPA-LT residual to DEP, where LEPA EQR exists ----
lrmi <- read_excel("Outputs/6_LRMI_with_comparison.xlsx") |>
  select(site_code, EQR, LRMI, LRMI_raw, DEP_richness)

attribution <- comparison |>
  inner_join(lrmi, by = "site_code") |>
  filter(!is.na(EQR), !is.na(LRMI)) |>
  mutate(
    # The full residual we are trying to explain.
    residual_LEPA_minus_LT = EQR - LRMI,
    # How much of that residual is matched by the DEP-implied shift.
    # Positive residual + positive delta_LRMI_implied -> DEP explains it
    # (because using validated names would raise our LRMI toward LEPA).
    residual_after_DEP     = residual_LEPA_minus_LT - delta_LRMI_implied
  )

if (nrow(attribution) > 0) {
  cat(sprintf("\nAttribution pool (sites with LEPA EQR): %d\n", nrow(attribution)))
  cat(sprintf("Residual (LEPA EQR - LT-OTL LRMI):\n"))
  print(summary(attribution$residual_LEPA_minus_LT))
  cat(sprintf("\nResidual after subtracting DEP-implied shift:\n"))
  print(summary(attribution$residual_after_DEP))
  cat(sprintf("\nMean |residual| before: %.4f | after: %.4f (lower = DEP explains more)\n",
              mean(abs(attribution$residual_LEPA_minus_LT), na.rm = TRUE),
              mean(abs(attribution$residual_after_DEP),     na.rm = TRUE)))
  cat(sprintf("Correlation of delta_LRMI_implied with residual: %.3f\n",
              cor(attribution$delta_LRMI_implied,
                  attribution$residual_LEPA_minus_LT,
                  use = "pairwise.complete.obs")))
}

# ---- Save table for case-by-case follow-up ------------------------
out <- if (exists("attribution") && nrow(attribution) > 0) {
  attribution |> arrange(desc(abs(residual_LEPA_minus_LT)))
} else {
  comparison |> arrange(desc(abs(delta_DEP)))
}

write_xlsx(out, "Outputs/8_DEP_OTL_vs_validated.xlsx")
cat("Saved: Outputs/8_DEP_OTL_vs_validated.xlsx\n")
