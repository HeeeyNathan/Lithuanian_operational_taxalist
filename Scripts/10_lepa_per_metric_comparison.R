# ============================================================
# LEPA vs LT-OTL — per-metric comparison (v2)
# ============================================================
# Changes from v1:
#   - Plot styling harmonised with script 7 (Figure 2 / Figure 4):
#     wfd_theme(), wfd_colors, method_colors, river_shapes loaded as
#     shared constants; titles/subtitles hidden via the theme.
#   - Per-metric scatter: WFD class bands drawn only on the EQR
#     facet (the others use per-metric EQR values that aren't
#     interpreted as WFD classes).
#   - Boxplot: WFD horizontal class bands added under the EQR facet
#     for the same reason; method fills via method_colors.
#   - The fifth facet is renamed "LRMI" -> "EQR" so the panel labels
#     are internally consistent.
# ============================================================
# LEPA publishes the four per-metric EQRs (DSFI, ASPT, #DEP,
# %EHP-%CrHi) alongside the final EQR for the 82 curated sites.
# This script lines LEPA's per-metric values up against ours
# (from script 6 output) so we can see WHICH metric (if any) is
# responsible for the residual differences in the final EQR.
#
# For each metric we report:
#   - mean and median per-site residual (LEPA - LT-OTL)
#   - mean |residual|, Pearson r, n
# Plus a per-site long table and a 5-panel scatter plot.
#
# Inputs:
#   - Inputs/LEPA metrics.xlsx               (LEPA published values)
#   - Outputs/6_LRMI_with_comparison.xlsx    (our LT-OTL run)
#
# Outputs:
#   - Outputs/10_lepa_per_metric_comparison.xlsx
#   - Plots/12_lepa_per_metric_panel.tiff  (combined A/B scatter + boxplot)
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(writexl)
library(ggplot2)
library(cowplot)   # plot_grid() for the A/B panel

# ---- Shared aesthetic constants (script 7 conventions) -----------
wfd_colors <- c("High"     = "#0000FF",
                "Good"     = "#00AA00",
                "Moderate" = "#FFD700",
                "Poor"     = "#FF6600",
                "Bad"      = "#FF0000")

river_shapes <- c("1" = 16, "2" = 17, "3" = 15, "4" = 18, "5" = 8)

method_colors <- c("LEPA" = "#5D4C46",
                   "OTL"  = "#8E24AA")

# WFD class boundary breaks — used as axis tick marks (scripts 6 / 7)
wfd_breaks <- c(0, 0.30, 0.40, 0.60, 0.80, 1.0)

# Shared theme — verbatim copy of script 7 wfd_theme() so styling
# (font sizes, panel border, legend) is identical across scripts 6,
# 7 and 10.
wfd_theme <- function() {
  theme_bw() +
  theme(
    plot.title    = element_blank(),
    plot.subtitle = element_blank(),
    axis.title    = element_text(size = 11, face = "bold"),
    axis.text     = element_text(size = 10),
    legend.title  = element_text(size = 11, face = "bold"),
    legend.text   = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border  = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position   = "bottom",
    legend.background = element_rect(fill = "white", color = NA),
    legend.key        = element_rect(fill = "white", color = NA),
    legend.margin     = margin(5, 5, 5, 5),
    axis.text.y.right  = element_text(color = c(wfd_colors["Bad"],
                                                wfd_colors["Poor"],
                                                wfd_colors["Moderate"],
                                                wfd_colors["Good"],
                                                wfd_colors["High"]),
                                      face = "bold", size = 10),
    axis.title.y.right = element_text(angle = 90)
  )
}

# ---- Read LEPA metrics --------------------------------------------
lepa <- read_excel("Inputs/LEPA metrics.xlsx") |>
  rename(
    site_id      = `site_id`,
    year         = `year`,
    lepa_DSFI    = `lepa_dsfi`,
    lepa_ASPT    = `lepa_aspt`,
    lepa_DEP     = `lepa_dep`,
    lepa_EHPCrHi = `lepa_ehp-CrHi`,
    lepa_LRMI    = `lepa_eqr`,
    lepa_class   = `lepa_eqc`
  ) |>
  mutate(
    site_code  = paste(site_id, year, sep = "_"),
    lepa_class = tools::toTitleCase(tolower(trimws(lepa_class)))
  ) |>
  select(site_code, site_id, year,
         lepa_DSFI, lepa_ASPT, lepa_DEP, lepa_EHPCrHi,
         lepa_LRMI, lepa_class)

cat(sprintf("LEPA rows: %d\n", nrow(lepa)))

# ---- Read our LT-OTL run ------------------------------------------
ours <- read_excel("Outputs/6_LRMI_with_comparison.xlsx") |>
  select(site_code,
         river_type,
         our_DSFI    = DSFI_EQR,
         our_ASPT    = ASPT_EQR,
         our_DEP     = DEP_EQR,
         our_EHPCrHi = EHP_minus_CrHi_EQR,
         our_LRMI    = LRMI,
         our_class   = LRMI_class) |>
  mutate(river_type = as.factor(river_type))

# ---- Join (inner) --------------------------------------------------
cmp <- inner_join(lepa, ours, by = "site_code")
missing <- setdiff(lepa$site_code, ours$site_code)
if (length(missing) > 0) {
  cat(sprintf("WARNING: %d LEPA site_code(s) without matching LT-OTL row:\n  %s\n",
              length(missing), paste(missing, collapse = ", ")))
}
cat(sprintf("Matched: %d / %d LEPA sites\n", nrow(cmp), nrow(lepa)))

# ---- Per-metric residuals -----------------------------------------
cmp <- cmp |>
  mutate(
    res_DSFI    = lepa_DSFI    - our_DSFI,
    res_ASPT    = lepa_ASPT    - our_ASPT,
    res_DEP     = lepa_DEP     - our_DEP,
    res_EHPCrHi = lepa_EHPCrHi - our_EHPCrHi,
    res_LRMI    = lepa_LRMI    - our_LRMI,
    class_match = !is.na(lepa_class) & !is.na(our_class) &
                  as.character(lepa_class) == as.character(our_class)
  )

# ---- Per-metric summary table -------------------------------------
metric_stats <- tibble(
  metric = c("DSFI", "ASPT", "#DEP", "%EHP–%CrHi", "LRMI"),
  lepa_col = c("lepa_DSFI", "lepa_ASPT", "lepa_DEP", "lepa_EHPCrHi", "lepa_LRMI"),
  our_col  = c("our_DSFI",  "our_ASPT",  "our_DEP",  "our_EHPCrHi",  "our_LRMI"),
  res_col  = c("res_DSFI",  "res_ASPT",  "res_DEP",  "res_EHPCrHi",  "res_LRMI")
)

stats <- metric_stats |>
  rowwise() |>
  mutate(
    n             = sum(!is.na(cmp[[lepa_col]]) & !is.na(cmp[[our_col]])),
    mean_residual = mean(cmp[[res_col]], na.rm = TRUE),
    median_residual = median(cmp[[res_col]], na.rm = TRUE),
    mean_abs_res  = mean(abs(cmp[[res_col]]), na.rm = TRUE),
    # Use cor.test so we have both r and p (matches scripts 6 / 7
    # annotation style: "Pearson correlation: R² = ..., p ... / n = ...").
    pearson_r     = suppressWarnings(
                      cor.test(cmp[[lepa_col]], cmp[[our_col]],
                               method = "pearson")$estimate),
    pearson_p     = suppressWarnings(
                      cor.test(cmp[[lepa_col]], cmp[[our_col]],
                               method = "pearson")$p.value),
    pct_exact     = mean(abs(cmp[[res_col]]) < 0.005, na.rm = TRUE) * 100
  ) |>
  ungroup() |>
  select(metric, n, mean_residual, median_residual, mean_abs_res,
         pearson_r, pearson_p, pct_exact)

cat("\n--- Per-metric summary (LEPA - LT-OTL) ---\n")
print(as.data.frame(stats), digits = 3, row.names = FALSE)

# ---- Paired Wilcoxon signed-rank test per metric ------------------
# Tests whether each metric's value shifts systematically between
# LEPA and LT-OTL. Same sites are measured under both methods, so
# the test is paired. We use the non-parametric Wilcoxon signed-rank
# test because the per-site residuals (LEPA - LT-OTL) are clearly
# non-normal (heavy zero-spike for DSFI/ASPT/#DEP, right-skewed for
# %EHP-%CrHi — see Plots/10_metric_residual_histograms.tiff).
# H0: median residual = 0, i.e. no systematic shift between methods.
# exact = FALSE uses the asymptotic normal approximation, which
# copes cleanly with the many tied-at-zero observations.
paired_tests <- metric_stats |>
  rowwise() |>
  mutate(
    n           = sum(!is.na(cmp[[lepa_col]]) & !is.na(cmp[[our_col]])),
    median_diff = median(cmp[[res_col]], na.rm = TRUE),
    W_stat      = unname(suppressWarnings(
                    wilcox.test(cmp[[lepa_col]], cmp[[our_col]],
                                paired = TRUE, exact = FALSE)$statistic)),
    W_p         = suppressWarnings(
                    wilcox.test(cmp[[lepa_col]], cmp[[our_col]],
                                paired = TRUE, exact = FALSE)$p.value)
  ) |>
  ungroup() |>
  select(metric, n, median_diff, W_stat, W_p)

cat("\n--- Paired Wilcoxon signed-rank test (LEPA vs LT-OTL, H0: no shift) ---\n")
print(as.data.frame(paired_tests), digits = 3, row.names = FALSE)

cat(sprintf("\nClass agreement: %d / %d (%.1f%%)\n",
            sum(cmp$class_match, na.rm = TRUE),
            sum(!is.na(cmp$class_match)),
            100 * mean(cmp$class_match, na.rm = TRUE)))

# ---- Scatter plots (5 panels) -------------------------------------
plot_dat <- cmp |>
  pivot_longer(
    cols = c(lepa_DSFI, lepa_ASPT, lepa_DEP, lepa_EHPCrHi, lepa_LRMI),
    names_to = "metric", values_to = "lepa_val"
  ) |>
  mutate(
    metric = recode(metric,
                    lepa_DSFI = "DSFI", lepa_ASPT = "ASPT",
                    lepa_DEP = "#DEP", lepa_EHPCrHi = "%EHP–%CrHi",
                    lepa_LRMI = "EQR"),                # v2: LRMI -> EQR
    our_val = case_when(
      metric == "DSFI"        ~ our_DSFI,
      metric == "ASPT"        ~ our_ASPT,
      metric == "#DEP"        ~ our_DEP,
      metric == "%EHP–%CrHi"  ~ our_EHPCrHi,
      metric == "EQR"         ~ our_LRMI
    )
  )

plot_dat$metric <- factor(plot_dat$metric,
                          levels = c("DSFI", "ASPT", "#DEP",
                                     "%EHP–%CrHi", "EQR"))

# Re-label the corresponding row in `stats` so its metric column
# matches the new factor levels in plot_dat (LRMI -> EQR).
stats$metric[stats$metric == "LRMI"] <- "EQR"

# Per-facet correlation labels. Uses r directly (not R²) so the plot
# matches the per-metric `pearson_r` printed in the console summary
# above — keeps the table and the figure on the same scale.
facet_r <- stats |>
  transmute(
    metric = factor(metric, levels = levels(plot_dat$metric)),
    label  = paste0(
      "Pearson correlation:\n",
      "r = ", format(round(pearson_r, 3), nsmall = 3),
      ", p ", ifelse(pearson_p < 0.001, "< 0.001",
                     paste0("= ", format(round(pearson_p, 3), nsmall = 3))),
      "\nn = ", n
    )
  )

# Per-facet anchor for the label: top-left corner of the data range
# for each metric. Used with hjust = 0 / vjust = 1 so every line of
# the annotation left-aligns at the same x (avoids the staircase that
# hjust = -0.1 produces when line widths differ, e.g. the short
# "n = NN" line drifting right of the longer lines above it).
anchor_dat <- plot_dat |>
  group_by(metric) |>
  summarise(
    x_anchor = min(lepa_val, na.rm = TRUE),
    y_anchor = max(our_val,  na.rm = TRUE),
    .groups = "drop"
  ) |>
  # The EQR facet draws WFD class bands that extend the panel down to
  # x = 0 and up to y = 1, so its visual top-left corner is (0, 1) —
  # not (min(data), max(data)). Override so the label sits flush with
  # the panel edge instead of drifting right of it.
  mutate(
    x_anchor = if_else(metric == "EQR", 0,                       x_anchor),
    y_anchor = if_else(metric == "EQR", pmax(1.0, y_anchor),     y_anchor)
  )

facet_r <- facet_r |>
  left_join(anchor_dat, by = "metric")

# Bands data: only the EQR facet gets WFD class rectangles. Bands cap
# at 1.0 to match scripts 6 / 7.
band_dat <- tibble::tibble(
  metric = factor("EQR", levels = levels(plot_dat$metric)),
  xmin   = c(0,    0.3,  0.4,  0.6,  0.8),
  xmax   = c(0.3,  0.4,  0.6,  0.8,  1.0),
  ymin   = c(0,    0.3,  0.4,  0.6,  0.8),
  ymax   = c(0.3,  0.4,  0.6,  0.8,  1.0),
  fill   = c(wfd_colors["Bad"], wfd_colors["Poor"],
             wfd_colors["Moderate"], wfd_colors["Good"],
             wfd_colors["High"])
)

p <- ggplot(plot_dat, aes(x = lepa_val, y = our_val)) +
  # WFD class bands only on the EQR facet
  geom_rect(data = band_dat,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                fill = fill),
            alpha = 0.3, inherit.aes = FALSE) +
  scale_fill_identity(guide = "none") +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "darkgray") +
  geom_point(aes(shape = river_type),
             size = 1.4, stroke = 1, alpha = 0.5, color = "black") +
  scale_shape_manual(values = river_shapes, name = "River Type") +
  guides(shape = guide_legend(override.aes = list(size = 3, stroke = 1))) +
  geom_text(data = facet_r,
            aes(x = x_anchor, y = y_anchor, label = label),
            hjust = 0, vjust = 1, size = 3,
            fontface = "bold",
            inherit.aes = FALSE) +
  # Each facet is a different metric on its own scale — let ggplot
  # pick sensible breaks per panel rather than imposing the WFD
  # boundaries (which are only meaningful for the EQR facet).
  facet_wrap(~ metric, nrow = 1, scales = "free") +
  labs(x = "Original LEPA index value",
       y = "Index value after OTL standardisation") +
  wfd_theme()

# (scatter panel `p` is combined with the boxplot below into a single
# A/B figure — individual ggsave omitted.)

# ---- Comparison boxplots (per-metric distributions) ---------------
# Side-by-side LEPA vs LT-OTL per metric so the reader can see the
# distribution shape (median, IQR, range) of each method without
# requiring per-point inspection. Useful as a companion to the scatter.

box_dat <- plot_dat |>
  pivot_longer(c(lepa_val, our_val),
               names_to = "method", values_to = "value") |>
  mutate(method = recode(method,
                         lepa_val = "LEPA",
                         our_val  = "OTL"),
         method = factor(method, levels = c("LEPA", "OTL")))

# Horizontal WFD class bands only on the EQR facet. `fill` carries
# the class NAME (matching wfd_colors keys), so the manual scale
# below maps both class names and method names from a single
# combined values vector.
band_dat_box <- tibble::tibble(
  metric = factor("EQR", levels = levels(box_dat$metric)),
  ymin   = c(0,    0.3,  0.4,  0.6,  0.8),
  ymax   = c(0.3,  0.4,  0.6,  0.8,  1.0),
  fill   = c("Bad", "Poor", "Moderate", "Good", "High")
)

p_box <- ggplot(box_dat, aes(x = method, y = value, fill = method)) +
  # WFD class bands (EQR facet only)
  geom_rect(data = band_dat_box,
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax,
                fill = fill),
            alpha = 0.2, inherit.aes = FALSE) +
  # Boxplots with solid black borders (script 7 Figure 4)
  geom_boxplot(
    alpha = 0.7,
    outlier.shape = NA,
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "black",
    linewidth = 0.5
  ) +
  # Jittered points overlaid, all black
  geom_point(
    position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
    size = 1.4, alpha = 0.5, color = "black",
    show.legend = FALSE
  ) +
  # Each facet is a different metric — give each its own y-scale.
  facet_wrap(~ metric, nrow = 1, scales = "free_y") +
  # Combined fill scale: method names ("LEPA", "LT-OTL") for the
  # boxplot fills, class names ("Bad", "Poor", ...) for the band
  # rectangles. `breaks` restricts the legend to method names only.
  scale_fill_manual(values = c(method_colors, wfd_colors),
                    name   = "EQR calculation",
                    breaks = c("LEPA", "OTL")) +
  # Legend overrides to match script 7
  guides(fill = guide_legend(override.aes = list(size = 5))) +
  labs(x = NULL, y = "Index value") +
  wfd_theme()

# ---- Combined A/B panel -------------------------------------------
# A = per-metric scatter (top), B = per-metric boxplots (bottom).
# Both share the same five facets, so stacking gives the reader a
# direct visual link between point-level agreement and distribution
# shape for each metric.
p_combined <- cowplot::plot_grid(
  p, p_box,
  ncol   = 1,
  labels = c("A", "B"),
  label_size     = 14,
  label_fontface = "bold",
  align  = "v",
  axis   = "lr"
)

ggsave("Plots/Figure_3.tiff",
       plot = p_combined, width = 14, height = 8, dpi = 450,
       bg = "white", compression = "lzw")
cat("Saved: Plots/Figure_3.tiff\n")

# ---- Residual histograms (paired-t-test assumption check) ---------
# One panel per metric showing the distribution of the LEPA - LT-OTL
# residual. Dashed vertical line at 0 (= no shift). A roughly
# symmetric, single-peaked distribution centred near 0 supports the
# paired t-test's normality / no-shift assumptions.
res_long <- cmp |>
  select(res_DSFI, res_ASPT, res_DEP, res_EHPCrHi, res_LRMI) |>
  pivot_longer(everything(), names_to = "metric", values_to = "residual") |>
  mutate(
    metric = recode(metric,
                    res_DSFI    = "DSFI",
                    res_ASPT    = "ASPT",
                    res_DEP     = "#DEP",
                    res_EHPCrHi = "%EHP–%CrHi",
                    res_LRMI    = "EQR"),
    metric = factor(metric,
                    levels = c("DSFI", "ASPT", "#DEP", "%EHP–%CrHi", "EQR"))
  )

p_hist <- ggplot(res_long, aes(x = residual)) +
  geom_histogram(bins = 20, fill = "#8E24AA", colour = "black",
                 alpha = 0.7, linewidth = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "darkgray") +
  facet_wrap(~ metric, nrow = 1, scales = "free") +
  labs(x = "Residual (LEPA − LT-OTL)", y = "Count") +
  wfd_theme()

ggsave("Plots/10_metric_residual_histograms.tiff",
       plot = p_hist, width = 14, height = 3.5, dpi = 450,
       bg = "white", compression = "lzw")
cat("Saved: Plots/10_metric_residual_histograms.tiff\n")

# ---- Publication-ready summary table ------------------------------
# Cleanly named, rounded, with per-method central tendency + dispersion
# alongside the head-to-head residual stats. Drop straight into a
# Word / LaTeX manuscript table.

method_summary <- box_dat |>
  group_by(Metric = metric, Method = method) |>
  summarise(
    n      = sum(!is.na(value)),
    Mean   = mean(value, na.rm = TRUE),
    SD     = sd(value,   na.rm = TRUE),
    Median = median(value, na.rm = TRUE),
    Min    = min(value, na.rm = TRUE),
    Max    = max(value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(across(c(Mean, SD, Median, Min, Max), \(x) round(x, 3)))

residual_summary <- stats |>
  transmute(
    Metric         = factor(metric, levels = levels(plot_dat$metric)),
    n              = n,
    `LEPA − LT-OTL (mean)`   = round(mean_residual,   4),
    `LEPA − LT-OTL (median)` = round(median_residual, 4),
    `Mean |residual|`        = round(mean_abs_res,    4),
    `Pearson r`              = round(pearson_r,       3),
    `% within ±0.005`        = round(pct_exact,       1)
  ) |>
  arrange(Metric)

cat("\n--- Publication-ready summary: per-method distribution ---\n")
print(as.data.frame(method_summary), row.names = FALSE)
cat("\n--- Publication-ready summary: head-to-head residuals ---\n")
print(as.data.frame(residual_summary), row.names = FALSE)

# ---- Output table -------------------------------------------------
out <- cmp |>
  arrange(desc(abs(res_LRMI))) |>
  select(site_code, site_id, year,
         lepa_DSFI, our_DSFI, res_DSFI,
         lepa_ASPT, our_ASPT, res_ASPT,
         lepa_DEP,  our_DEP,  res_DEP,
         lepa_EHPCrHi, our_EHPCrHi, res_EHPCrHi,
         lepa_LRMI, our_LRMI, res_LRMI,
         lepa_class, our_class, class_match)

write_xlsx(
  list(
    per_site                  = out,
    summary                   = stats,
    paired_tests              = paired_tests,
    publication_distribution  = method_summary,
    publication_residuals     = residual_summary
  ),
  "Outputs/10_lepa_per_metric_comparison.xlsx"
)
cat("Saved: Outputs/10_lepa_per_metric_comparison.xlsx\n")

#==================== CLEAN UP WORKSPACE =====================
library(pacman)
rm(list = ls())       # Remove all objects from environment
gc()                  # Frees up unused memory
p_unload(all)         # Unload all loaded packages
graphics.off()        # Close all graphical devices
cat("\014")           # Clear the console
# Clear mind :)
