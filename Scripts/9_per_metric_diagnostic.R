# ============================================================
# Per-metric diagnostic for class-mismatch / large-residual sites
# ============================================================
# For each of a user-supplied set of site_codes, pull the four
# per-metric EQRs (DSFI, ASPT, #DEP, %EHP-%CrHi) alongside the LRMI
# and LEPA's published EQR / EQC. Report each per-metric EQR's
# percentile rank against the full natural-waterbody distribution
# so that an unusually low metric stands out as the likely cause of
# the LEPA-vs-LT discrepancy.
#
# Written before LEPA's per-metric EQRs were available, so metric
# responsibility is inferred indirectly (percentile rank). Superseded
# by script 10, which compares against LEPA's published per-metric
# EQRs directly; retained as a supporting diagnostic.
#
# Input:
#   - Outputs/6_LRMI_with_comparison.xlsx (script 6 output)
#
# Outputs:
#   - Outputs/9_per_metric_diagnostic.xlsx
#   - Console summary
# ============================================================

library(readxl)
library(dplyr)
library(writexl)

# ---- Sites to diagnose -------------------------------------------
target_sites <- c(
  "LTR1492_2022", "LTR163_2019",  "LTR127_2015",  "LTR386_2019",
  "LTR1466_2019", "LTR163_2022",  "LTR450_2015",  "LTR127_2017",
  "LTR127_2020",  "LTR625_2022",  "LTR1466_2016", "LTR1687_2016",
  "LTR1687_2022", "LTR297_2021",  "LTR1447_2015", "LTR86_2015",
  "LTR1461_2022", "LTR1461_2016", "LTR1461_2019", "LTR1466_2022",
  "LTR386_2016",  "LTR386_2022",  "LTR1488_2015", "LTR1063_2021",
  "LTR163_2016",  "LTR308_2018",  "LTR450_2020",  "LTR297_2018",
  "LTR706_2015",  "LTR1488_2016", "LTR1492_2019", "LTR1502_2015",
  "LTR337_2019",  "LTR337_2022",  "LTR337_2016",  "LTR1687_2017",
  "LTR297_2015",  "LTR1488_2022", "LTR1488_2019", "LTR1501_2016",
  "LTR1447_2022", "LTR1492_2016", "LTR1502_2019", "LTR351_2019",
  "LTR411_2022",  "LTR86_2016"
)

# ---- Read merged data --------------------------------------------
all_data <- read_excel("Outputs/6_LRMI_with_comparison.xlsx")

needed <- c("site_code", "site_id", "year", "waterbody_name", "river_type",
            "EQR", "EQC", "LRMI", "LRMI_raw", "LRMI_class",
            "DSFI_EQR", "ASPT_EQR", "DEP_EQR", "EHP_minus_CrHi_EQR")
missing <- setdiff(needed, names(all_data))
if (length(missing) > 0) {
  warning("Columns absent from input (will be NA in output): ",
          paste(missing, collapse = ", "))
}

# Use only the columns that exist
keep <- intersect(needed, names(all_data))
all_data <- all_data[, keep, drop = FALSE]

# ---- Percentile ranks (per metric) -------------------------------
# Computed on the FULL natural-waterbody pool (the loaded file already
# is filtered to natural waterbodies via script 6). A site at e.g. the
# 5th percentile on DSFI_EQR is bottom-5% — that metric is dragging
# its LRMI down relative to the typical site.
percentile_rank <- function(x) {
  r <- rank(x, na.last = "keep", ties.method = "average")
  100 * (r - 1) / (sum(!is.na(x)) - 1)
}

all_data <- all_data |>
  mutate(
    DSFI_pct           = percentile_rank(DSFI_EQR),
    ASPT_pct           = percentile_rank(ASPT_EQR),
    DEP_pct            = percentile_rank(DEP_EQR),
    EHP_minus_CrHi_pct = percentile_rank(EHP_minus_CrHi_EQR)
  )

# ---- Subset to target sites + check --------------------------------
diag <- all_data |>
  filter(site_code %in% target_sites) |>
  mutate(target_order = match(site_code, target_sites)) |>
  arrange(target_order) |>
  select(-target_order)

missing_sites <- setdiff(target_sites, diag$site_code)
if (length(missing_sites) > 0) {
  cat(sprintf("WARNING: %d site_code(s) requested but not found in merged data:\n  %s\n",
              length(missing_sites), paste(missing_sites, collapse = ", ")))
}

cat(sprintf("Diagnosing %d / %d requested sites\n",
            nrow(diag), length(target_sites)))

# ---- Identify the "lowest-percentile" metric per site -------------
# This is the metric most likely responsible for pulling LRMI below
# LEPA's EQR. Tied at the lowest percentile = listed alphabetically.
metric_cols <- c("DSFI_pct", "ASPT_pct", "DEP_pct", "EHP_minus_CrHi_pct")
metric_labs <- c("DSFI",     "ASPT",     "DEP",     "EHP-CrHi")

diag$lowest_metric <- vapply(seq_len(nrow(diag)), function(i) {
  v <- as.numeric(diag[i, metric_cols])
  if (all(is.na(v))) return(NA_character_)
  metric_labs[which.min(v)]
}, character(1))

diag$lowest_pct <- vapply(seq_len(nrow(diag)), function(i) {
  v <- as.numeric(diag[i, metric_cols])
  if (all(is.na(v))) return(NA_real_)
  min(v, na.rm = TRUE)
}, numeric(1))

# Residual sign: positive = LEPA higher than our LRMI
diag$residual <- diag$EQR - diag$LRMI

# ---- Console summary --------------------------------------------
cat("\n--- Frequency of 'lowest percentile metric' across target sites ---\n")
print(table(diag$lowest_metric, useNA = "ifany"))

cat("\nMean percentile across target sites (any metric persistently low here is the bias source):\n")
print(round(colMeans(diag[, metric_cols], na.rm = TRUE), 1))

cat("\nFull diagnostic table (sorted as requested):\n")

print(as.data.frame(diag |>
  select(site_code, year, waterbody_name, river_type,
         EQR, LRMI, residual, EQC, LRMI_class,
         DSFI_EQR, DSFI_pct,
         ASPT_EQR, ASPT_pct,
         DEP_EQR,  DEP_pct,
         EHP_minus_CrHi_EQR, EHP_minus_CrHi_pct,
         lowest_metric, lowest_pct)),
  row.names = FALSE, digits = 3)

# ---- Save --------------------------------------------------------
write_xlsx(diag, "Outputs/9_per_metric_diagnostic.xlsx")
cat("\nSaved: Outputs/9_per_metric_diagnostic.xlsx\n")

#==================== CLEAN UP WORKSPACE =====================
library(pacman)
rm(list = ls())       # Remove all objects from environment
gc()                  # Frees up unused memory
p_unload(all)         # Unload all loaded packages
graphics.off()        # Close all graphical devices
cat("\014")           # Clear the console
# Clear mind :)
