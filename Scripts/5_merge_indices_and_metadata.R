# ============================================================
# Merge DSFI, taxonomic indices, and site metadata (v1)
# ============================================================
# Inputs:
#   - Outputs/3_DSFI_results_all_sites.xlsx
#     (site_code, site_id, year, DSFI fields)
#   - Outputs/4_calculated_taxonomic_indices.xlsx
#     (site_code, year, taxonomic indices)
#   - Inputs/All_sites_macroinvertebrate_data_long.xlsx, sheet
#     "site_meta_data" (site_id, latitude, longitude, waterbody_type,
#     river_basin, catchment_subcatchment, waterbody_name,
#     modification_state, modification_group, river_type, reference_site)
#
# Joins:
#   1. DSFI x tax_indices  on  (site_code, year)   — full_join
#      (surfaces site-years present in only one)
#   2. merged  x  site_meta_data  on  site_id      — left_join
#      (every result row keeps its metadata; metadata-only rows dropped)
#
# Output: Outputs/5_merged_indices.xlsx
# ============================================================

library(readxl)
library(dplyr)
library(writexl)

# ---- Read inputs ----
# Year scope: 2014-2022 (LRMI-era only — see script 4 rationale).
YEAR_MIN <- 2014
YEAR_MAX <- 2022

dsfi <- read_excel("Outputs/3_DSFI_results_all_sites.xlsx") |>
        filter(year >= YEAR_MIN, year <= YEAR_MAX)
tax  <- read_excel("Outputs/4_calculated_taxonomic_indices.xlsx") |>
        filter(year >= YEAR_MIN, year <= YEAR_MAX)
meta <- read_excel("Inputs/All_sites_macroinvertebrate_data_long.xlsx",
                   sheet = "site_meta_data",
                   guess_max = 2500) |>
        filter(grepl("^LTR", site_id)) |>
        distinct()
eqr <- read_excel("Inputs/All_sites_macroinvertebrate_data_long.xlsx",
                   sheet = "clean_macroinvertebrate_data",
                   guess_max = 2500) |>
       filter(grepl("^LTR", site_id),
              year >= YEAR_MIN, year <= YEAR_MAX) |>
       select(1, 4, 6, 19, 20) |>
       distinct()

# Clean LEPA EQR / EQC placeholder values. Source data uses 0 / "0"
# (rather than blank) to mark site-years that were never assessed.
# Convert these to NA so downstream comparisons aren't biased.
valid_eqc <- c("High", "Good", "Moderate", "Poor", "Bad")
n_eqr_zero <- sum(!is.na(eqr$EQR) & eqr$EQR == 0)
n_eqc_bad  <- sum(!is.na(eqr$EQC) & !(as.character(eqr$EQC) %in% valid_eqc))
eqr <- eqr |>
  mutate(
    EQR = ifelse(!is.na(EQR) & EQR == 0, NA_real_, as.numeric(EQR)),
    EQC = ifelse(as.character(EQC) %in% valid_eqc,
                 as.character(EQC), NA_character_)
  )
cat(sprintf("LEPA EQR placeholders nulled: %d rows with EQR == 0\n", n_eqr_zero))
cat(sprintf("LEPA EQC placeholders nulled: %d rows with EQC not in {%s}\n",
            n_eqc_bad, paste(valid_eqc, collapse = ", ")))

# Drop rows that are NA in EQR or EQC after cleaning — these are
# site-years LEPA never assessed and have no meaningful metadata to
# contribute to the merged file.
n_before_na_drop <- nrow(eqr)
eqr <- eqr |> filter(!is.na(EQR), !is.na(EQC))
cat(sprintf("Dropped %d row(s) with NA EQR or EQC after cleaning: %d -> %d rows\n",
            n_before_na_drop - nrow(eqr), n_before_na_drop, nrow(eqr)))

# Uniqueness checks. `meta` should be one row per site_id (physical site);
# `eqr` should be one row per site_code (site x year sample-level fields).
dup_meta <- meta$site_id[duplicated(meta$site_id)]
if (length(dup_meta) > 0) {
  cat(sprintf("WARNING: %d site_id(s) duplicated in site_meta_data:\n  %s\n",
              length(unique(dup_meta)), paste(unique(dup_meta), collapse = ", ")))
} else {
  cat("OK: every site_id has a unique site_meta_data row.\n")
}

dup_eqr <- eqr$site_code[duplicated(eqr$site_code)]
if (length(dup_eqr) > 0) {
  cat(sprintf("WARNING: %d site_code(s) have more than one EQR/EQC metadata row (columns 1-20 differ between taxon records):\n  %s\n",
              length(unique(dup_eqr)), paste(unique(dup_eqr), collapse = ", ")))
} else {
  cat("OK: every site_code has a unique EQR/EQC metadata row.\n")
}

cat(sprintf("DSFI rows: %d | tax rows: %d | site_meta rows: %d | eqr rows: %d\n",
            nrow(dsfi), nrow(tax), nrow(meta), nrow(eqr)))

# ---- 1. DSFI x tax_indices on (site_code, year) ----
# full_join so any one-sided rows surface as NAs rather than being dropped.
merged <- full_join(dsfi, tax, by = c("site_id", "site_code", "year"))

only_dsfi <- merged$site_code[is.na(merged$SppRich) & !is.na(merged$dsfi_value)]
only_tax  <- merged$site_code[is.na(merged$dsfi_value) & !is.na(merged$SppRich)]

if (length(only_dsfi) > 0) {
  cat(sprintf("WARNING: %d site_code(s) in DSFI but not in taxonomic indices:\n  %s\n",
              length(only_dsfi), paste(only_dsfi, collapse = ", ")))
}
if (length(only_tax) > 0) {
  cat(sprintf("WARNING: %d site_code(s) in taxonomic indices but not in DSFI:\n  %s\n",
              length(only_tax), paste(only_tax, collapse = ", ")))
}

# ---- 2. Attach site_meta_data on site_id ----
missing_meta <- setdiff(unique(merged$site_id), meta$site_id)
if (length(missing_meta) > 0) {
  cat(sprintf("WARNING: %d site_id(s) have no row in site_meta_data:\n  %s\n",
              length(missing_meta), paste(missing_meta, collapse = ", ")))
}
merged <- left_join(merged, meta, by = "site_id")

# ---- 3. Attach EQR / EQC + sample fields on (site_id, site_code, year) ----
# Joining on all three keeps a clean key match and avoids site_id.x/.y suffixes
# (eqr carries site_id, site_code and year in columns 1-20).
missing_eqr <- setdiff(unique(merged$site_code), eqr$site_code)
if (length(missing_eqr) > 0) {
  cat(sprintf("WARNING: %d site_code(s) have no EQR/EQC metadata row:\n  %s\n",
              length(missing_eqr), paste(missing_eqr, collapse = ", ")))
}
merged <- left_join(merged, eqr, by = c("site_id", "year", "site_code"))

merged <- merged[!merged$site_code %in% missing_eqr, ]

# ---- Order columns: identifiers first, then metadata, then indices ----
id_cols    <- c("site_code", "site_id", "year")
meta_cols  <- c(setdiff(colnames(meta), id_cols),
                setdiff(colnames(eqr),  id_cols))
other_cols <- setdiff(colnames(merged), c(id_cols, meta_cols))
merged     <- merged[, c(id_cols, meta_cols, other_cols)]

cat(sprintf("Merged rows: %d | columns: %d\n", nrow(merged), ncol(merged)))

# ---- Write ----
write_xlsx(merged, "Outputs/5_merged_indices.xlsx")
cat("Saved: Outputs/5_merged_indices.xlsx\n")

#==================== CLEAN UP WORKSPACE =====================
library(pacman)
rm(list = ls())       # Remove all objects from environment
gc()                  # Frees up unused memory
p_unload(all)         # Unload all loaded packages
graphics.off()        # Close all graphical devices
cat("\014")           # Clear the console
# Clear mind :)
