# ============================================================
# Lithuanian River Macroinvertebrate Index (LRMI) — v1
# ============================================================
# Method follows Sidagyte-Copilas & Arbaciauskas (2022),
# "A multimetric macroinvertebrate index for the assessment of the
# ecological status of Lithuanian rivers", Limnologica 97:126010.
#
# Four core metrics (paper Section 2.4 / Table 4):
#   1. DSFI            — Danish Stream Fauna Index            ref=7,   low=1
#   2. ASPT            — Average Score Per Taxon (BMWP/units) ref=7.0, low=1.0
#   3. #DEP            — Richness of D families + E, P taxa   ref=15,  low=0
#   4. %EHP - %CrHi    — Relative-abundance contrast          ref=0.6, low=-1.0
#
# Per-metric EQR (paper Section 2.5):
#   EQR_i = (observed_i - lower_i) / (reference_i - lower_i)
#   Per-metric EQRs are kept unclamped and unrounded (they are internal
#   inputs to the LRMI; the paper does not clamp them and they have no
#   LEPA-equivalent reported precision to match).
#
# LRMI = arithmetic mean of the four per-metric EQRs. The paper
# averages the remaining three if DSFI fails to key out; here we are
# stricter — if DSFI (or any of the four metrics) is NA, LRMI is left
# as NA.
#
# After averaging, LRMI is clamped at 1 and rounded to 2 dp to match
# the precision of LEPA's reported EQRs. The raw (unclamped, unrounded)
# LRMI is retained as LRMI_raw alongside the display value LRMI.
# LRMI_class is derived from the clamped + rounded LRMI so it follows
# LEPA's class-from-rounded-EQR convention.
#
# Scope: only natural waterbodies (modification_state == "N") are
# evaluated. Heavily modified / artificial waterbodies use a different
# reference-condition calculation that is not implemented here.
#
# Class boundaries (paper Table 5, bold "Current" row — uniform, no
# type-specific adjustments applied):
#   High      LRMI >= 0.80
#   Good      0.60 <= LRMI < 0.80
#   Moderate  0.40 <= LRMI < 0.60
#   Poor      0.30 <= LRMI < 0.40
#   Bad             LRMI < 0.30
#
# Inputs:
#   - Outputs/5_merged_indices.xlsx (output of script 5)
#     Required columns:
#       dsfi_value           (DSFI)
#       ASPT                 (ASPT)
#       DEP_richness         (#DEP)
#       EHP_minus_CrHi_prop  (%EHP - %CrHi)
#       EQR, EQC             (LEPA originals, for validation)
#
# Outputs:
#   - Outputs/6_LRMI_with_comparison.xlsx — per-metric EQRs (raw),
#     raw LRMI, clamped+rounded LRMI, the derived LRMI_class, and
#     original LEPA EQR/EQC alongside for inspection.
#   - Plots/Figure_2.tiff — scatterplot of calculated LRMI against
#     LEPA's original EQR with 1:1 reference and WFD class bands.
# ============================================================

library(readxl)
library(dplyr)
library(writexl)
library(ggplot2)

# ---- WFD colour palette (shared across plots) -------------------
wfd_colors <- c("High"     = "#0000FF",
                "Good"     = "#00AA00",
                "Moderate" = "#FFD700",
                "Poor"     = "#FF6600",
                "Bad"      = "#FF0000")

# River type shape scale
river_shapes <- c("1" = 16, # circle
                  "2" = 17, # triangle
                  "3" = 15, # square
                  "4" = 18, # diamond
                  "5" = 8)  # asterisk

# Define a consistent theme for all WFD-related plots (matching Figure 1)
wfd_theme <- function() {
  theme_bw() +
  theme(
    # Text elements - matching Figure 1
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),

    # Panel elements
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),

    # Legend position and appearance
    legend.position = "bottom",
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    legend.margin = margin(5, 5, 5, 5),

    # Secondary axis styling
    axis.text.y.right = element_text(color = c(wfd_colors["Bad"], wfd_colors["Poor"],
                                              wfd_colors["Moderate"], wfd_colors["Good"],
                                              wfd_colors["High"]),
                                     face = "bold", size = 10),
    axis.title.y.right = element_text(angle = 90)
  )
}

# ---- Anchors (paper Table 4) -------------------------------------
DSFI_REF <- 7;    DSFI_LOW <- 1
ASPT_REF <- 7.0;  ASPT_LOW <- 1.0
DEP_REF  <- 15;   DEP_LOW  <- 0
EHC_REF  <- 0.6;  EHC_LOW  <- -1.0

eqr <- function(obs, ref, low) (obs - low) / (ref - low)

# ---- Read merged indices -----------------------------------------
dat <- read_excel("Outputs/5_merged_indices.xlsx")

required <- c("dsfi_value", "ASPT", "DEP_richness", "EHP_minus_CrHi_prop")
missing  <- setdiff(required, names(dat))
if (length(missing) > 0) {
  stop("Missing required columns in merged input: ",
       paste(missing, collapse = ", "))
}

cat(sprintf("Rows in merged input: %d\n", nrow(dat)))

# ---- Restrict to natural waterbodies -----------------------------
# Heavily modified / artificial waterbodies use a different
# reference-condition calculation and are out of scope here.
if (!"modification_state" %in% names(dat)) {
  stop("modification_state column not found — metadata join in script 5 must run first.")
}

n_before <- nrow(dat)
ms_counts <- table(dat$modification_state, useNA = "ifany")
cat("modification_state distribution (input):\n"); print(ms_counts)

dat <- dat[!is.na(dat$modification_state) & dat$modification_state == "N", ]
cat(sprintf("Filtered to natural waterbodies (modification_state == 'N'): %d -> %d rows\n",
            n_before, nrow(dat)))

# ---- Compute per-metric EQRs, LRMI, class ------------------------
# Step 1: per-metric EQRs (paper formula, unclamped, unrounded). Kept
# as raw inputs to the LRMI mean.
result <- dat |>
  mutate(
    DSFI_EQR           = eqr(dsfi_value,          DSFI_REF, DSFI_LOW),
    ASPT_EQR           = eqr(ASPT,                ASPT_REF, ASPT_LOW),
    DEP_EQR            = eqr(DEP_richness,        DEP_REF,  DEP_LOW),
    EHP_minus_CrHi_EQR = eqr(EHP_minus_CrHi_prop, EHC_REF,  EHC_LOW),
  )

eqr_mat <- as.matrix(result[, c("DSFI_EQR", "ASPT_EQR",
                                "DEP_EQR", "EHP_minus_CrHi_EQR")])

# Step 2: LRMI_raw = arithmetic mean of the four per-metric EQRs.
# Strict rule: only computed when all four are non-NA; otherwise NA.
result$LRMI_n_metrics <- rowSums(!is.na(eqr_mat))
result$LRMI_raw       <- ifelse(result$LRMI_n_metrics == 4,
                                rowMeans(eqr_mat),
                                NA_real_)

# Step 3: clamp LRMI at 1 and round to 2 dp for comparison with LEPA
# (LEPA reports EQRs at 2 dp and capped at 1). Per-metric EQRs are
# left as raw values per agreed convention.
result$LRMI <- round(pmin(result$LRMI_raw, 1), 2)

# Step 4: class derived from the clamped + rounded LRMI.
result$LRMI_class <- cut(result$LRMI,
                         breaks = c(-Inf, 0.30, 0.40, 0.60, 0.80, Inf),
                         labels = c("Bad", "Poor", "Moderate", "Good", "High"),
                         right  = FALSE)

# Diagnostics on metric availability.
cat(sprintf("Rows with missing DSFI:              %d\n",
            sum(is.na(result$DSFI_EQR))))
cat(sprintf("Rows with missing ASPT:              %d\n",
            sum(is.na(result$ASPT_EQR))))
cat(sprintf("Rows with missing #DEP:              %d\n",
            sum(is.na(result$DEP_EQR))))
cat(sprintf("Rows with missing %%EHP-%%CrHi:        %d\n",
            sum(is.na(result$EHP_minus_CrHi_EQR))))
cat(sprintf("Rows with LRMI computed (all 4):     %d\n",
            sum(!is.na(result$LRMI))))
cat(sprintf("Rows with LRMI = NA (any missing):   %d\n",
            sum(is.na(result$LRMI))))

# ---- Class distribution -----------------------------------------
cat("\nLRMI class distribution (calculated):\n")
print(table(result$LRMI_class, useNA = "ifany"))

# Comparison against LEPA originals is restricted to rows where LEPA's
# EQR is actually available. Rows with NA in LEPA EQR contribute
# nothing to validation and are excluded from the confusion matrix,
# exact-agreement %, correlations, and the scatterplot.
lepa_available <- "EQR" %in% names(result) & !is.na(result$EQR)
cat(sprintf("\nRows with LEPA EQR available (used for comparison): %d / %d\n",
            sum(lepa_available), nrow(result)))

if ("EQC" %in% names(result)) {
  cmp <- result[lepa_available, ]

  cat("\nOriginal EQC distribution (LEPA, EQR-available rows only):\n")
  print(table(cmp$EQC, useNA = "ifany"))

  cat("\nConfusion matrix (rows = LEPA EQC, cols = calculated LRMI_class):\n")
  conf <- table(LEPA = cmp$EQC, calc = cmp$LRMI_class, useNA = "ifany")
  print(conf)

  both_ok <- !is.na(cmp$EQC) & !is.na(cmp$LRMI_class)
  if (any(both_ok)) {
    agree <- mean(as.character(cmp$EQC[both_ok]) ==
                  as.character(cmp$LRMI_class[both_ok]))
    cat(sprintf("\nExact class agreement: %.1f%% (n=%d)\n",
                100 * agree, sum(both_ok)))
  }
}

# ---- Correlation against LEPA's original EQR --------------------
# ---- Correlation against LEPA's original EQR --------------------
if ("EQR" %in% names(result)) {
  # Plot/correlation pool: drop rows with missing LRMI or LEPA EQR.
  # Script 5 cleans LEPA placeholder zeros and drops rows with NA
  # EQR/EQC at source, so no further validation is needed here.
  ok <- !is.na(result$EQR) & !is.na(result$LRMI)
  cat(sprintf("Comparison pool: %d rows\n", sum(ok)))
  if (sum(ok) >= 3) {
    pearson_test <- cor.test(result$EQR[ok], result$LRMI[ok],
                             method = "pearson")
    cor_p <- unname(pearson_test$estimate)
    # Annotation styled to match script 7 (Figure_3), with an added
    # n line so the reader can see how many sites were compared.
    corr_text <- paste0(
      "Pearson correlation:\n",
      "r = ", format(round(cor_p, 3), nsmall = 3),
      ", p ", ifelse(pearson_test$p.value < 0.001, "< 0.001",
                     paste0("= ", format(round(pearson_test$p.value, 3), nsmall = 3))),
      "\nn = ", sum(ok)
    )
    cat(sprintf("\nLRMI vs LEPA EQR (n=%d):\n", sum(ok)))
    cat(sprintf("  Pearson r  = %.4f\n", cor_p))
  } else {
    cor_p <- NA_real_
    corr_text <- NA_character_
    cat("\nNot enough non-NA pairs to compute correlation.\n")
  }

  # ---- Scatterplot --------------------------------------------------
  plot_dat <- result[ok, ]
  if ("EQC" %in% names(plot_dat)) {
    plot_dat$EQC <- factor(plot_dat$EQC,
                           levels = names(wfd_colors))
  }
  if ("river_type" %in% names(plot_dat)) {
    plot_dat$river_type <- factor(plot_dat$river_type)
  }

  # Class-mismatch subset — LEPA EQC differs from our LRMI_class.
  # Only these points get labelled in the plot.
  mismatches <- plot_dat |>
    dplyr::filter(!is.na(EQC) & !is.na(LRMI_class) &
                  as.character(EQC) != as.character(LRMI_class))
  cat(sprintf("Class mismatches (LEPA EQC != LRMI_class): %d / %d\n",
              nrow(mismatches), nrow(plot_dat)))

  # WFD class boundaries — used as primary tick marks on both axes
  wfd_breaks <- c(0, 0.30, 0.40, 0.60, 0.80, 1.0)

  # v2: harmonised with script 7 (Figure_3). Class is conveyed by the
  # diagonal WFD bands; points are uniformly black with shape mapping
  # to river_type. No OLS smoother — only the 1:1 line.
  p <- ggplot(plot_dat,
              aes(x = EQR, y = LRMI, shape = river_type)) +
    # Background colour bands for EQR classes (along the 1:1 diagonal).
    # Bands cap at 1.0 to match Figure_3 in script 7.
    annotate("rect", xmin = 0.8, xmax = 1.0, ymin = 0.8, ymax = 1.0,
             fill = wfd_colors["High"],     alpha = 0.3) +
    annotate("rect", xmin = 0.6, xmax = 0.8, ymin = 0.6, ymax = 0.8,
             fill = wfd_colors["Good"],     alpha = 0.3) +
    annotate("rect", xmin = 0.4, xmax = 0.6, ymin = 0.4, ymax = 0.6,
             fill = wfd_colors["Moderate"], alpha = 0.3) +
    annotate("rect", xmin = 0.3, xmax = 0.4, ymin = 0.3, ymax = 0.4,
             fill = wfd_colors["Poor"],     alpha = 0.3) +
    annotate("rect", xmin = 0.0, xmax = 0.3, ymin = 0.0, ymax = 0.3,
             fill = wfd_colors["Bad"],      alpha = 0.3) +
    # 1:1 reference line
    geom_abline(slope = 1, intercept = 0,
                linetype = "dashed", color = "darkgray") +
    # Points: all black, shape by river_type
    geom_point(size = 2, stroke = 1.5, alpha = 0.8, color = "black") +
    # Inline correlation annotation (bottom-right of plot panel)
    # Position matches script 7 Figure 2: y = 0.075, hjust = 1.
    annotate("text", x = 0.995, y = 0.075,
             label = corr_text,
             hjust = 1,
             size = 3, fontface = "bold") +
    scale_shape_manual(values = river_shapes, name = "River Type") +
    # Tick marks at the WFD class boundaries on both axes; upper bound
    # not capped because LEPA EQRs can exceed 1. Default expand
    # padding (5% each side) so spacing before 0 and after 1 matches
    # script 7 Figure_3.
    scale_x_continuous(breaks = wfd_breaks, limits = c(0, NA)) +
    # Secondary y-axis labels the five WFD classes at the band midpoints.
    scale_y_continuous(
      breaks = wfd_breaks, limits = c(0, NA),
      sec.axis = dup_axis(
        name   = "",
        breaks = c(0.15, 0.35, 0.5, 0.7, 0.9),
        labels = c("BAD", "POOR", "MODERATE", "GOOD", "HIGH")
      )
    ) +
    # No coord_equal(): matches script 7 Figure_3, where the panel
    # fills the 10x8 device naturally without being forced square.
    # Legend override to match script 7
    guides(shape = guide_legend(override.aes = list(size = 5, stroke = 1.5))) +
    labs(
      x = "Original LEPA EQR",
      y = "EQR after OTL standardisation"
    ) +
    wfd_theme()

  ggsave("Plots/Figure_2.tiff",
         plot = p,  width = 10, height = 8, dpi = 450, bg = "white", compression = "lzw")
  cat("Saved: Plots/Figure_2.tiff\n")
}

# ---- Write merged result table -----------------------------------
# Put new EQR / LRMI columns at the end for easy side-by-side
# inspection. Raw (unclamped, unrounded) values are retained alongside
# the clamped + rounded display values.
new_cols <- c("DSFI_EQR", "ASPT_EQR", "DEP_EQR", "EHP_minus_CrHi_EQR",
              "LRMI_n_metrics",
              "LRMI_raw", "LRMI", "LRMI_class")
result <- result[, c(setdiff(names(result), new_cols), new_cols)]

write_xlsx(result, "Outputs/6_LRMI_with_comparison.xlsx")
cat("Saved: Outputs/6_LRMI_with_comparison.xlsx\n")
