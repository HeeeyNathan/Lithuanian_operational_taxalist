#-----------------------------------------------
# Outline (v4)
#-----------------------------------------------
# The paper is focused on the creation of an operational taxa list (OTL) that
# standardises taxonomic names and levels to reduce errors in misidentification
# and incorrect calculation of ecological quality ratios (EQRs) under the EU
# Water Framework Directive. We compare two EQR calculations on a curated set
# of 27 sites (2-3 replicates each, 2015-2022, spread across catchments and
# river types) covering the gradient from POOR to HIGH ecological status:
#
#   - LEPA reference (epa_eqr / epa_eqc): the Lithuanian Environmental
#     Protection Agency's published assessment, taken from the comparison
#     input file.
#   - LT-OTL (lt_otl_eqr / lt_otl_eqc): the LRMI / LRMI_class recalculated
#     after OTL-based taxonomic standardisation (script 6 output).
#
# (Previous fwe.info comparisons have been removed; we are no longer using
# freshwaterecology.info as a second standardisation reference.)
#
# Visual evidence (1:1 scatterplot, Bland-Altman, boxplots by river type)
# is intended to show whether OTL standardisation materially shifts EQR /
# EQC values. The expectation is broad agreement with LEPA, supporting the
# claim that OTL standardisation preserves assessment integrity while
# reducing human identification error.

# We selected 27 sites covering a gradient from POOR ecological status to HIGH ecological status
# between 2015 and 2022.We have 2 to 3 replicates per site across the observation period to
# cover potential temporal variation in the data. Sites are distributed across Lithuania to also
# capture spatial variation in the data. Sampling sites also cover all four of the main catchments
# in Lithuania to capture catchment scale variability. Sampling sites are also devided by river
# type to capture type-specific variability.

# The visualizations clearly support your finding that taxonomic standardization with the Lithuanian
# OTL doesn't significantly impact EQR/EQC calculations. The close alignment between methods on the
# scatterplots and the minimal differences in the Bland-Altman plots provide strong visual evidence.

# The data shows that taxonomic standardization is indeed a conservative approach, as most measurements
# remain consistent regardless of methodology. The theory that "water quality is likely better than measured"
# can be supported by showing the few cases where standardization led to improved ecological class ratings.

#-----------------------------------------------
# Libraries
#-----------------------------------------------
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(scales)
library(viridis)
library(cowplot)
library(ggrepel)
library(readxl)
library(writexl)

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeoboundaries)
library(ggspatial)

#-----------------------------------------------
# Load and prepare data
#-----------------------------------------------
# Two inputs:
#   - Inputs/EQR_data_for_comparison.xlsx — 82 selected site-years (27
#     sites, 2-3 reps each, 2015-2022) with LEPA's reference eqr / eqc
#     plus site metadata. The file no longer carries lt_otl or fwe
#     columns; those are computed elsewhere.
#   - Outputs/6_LRMI_with_comparison.xlsx — LT-OTL calculation from
#     script 6 (LRMI numeric + LRMI_class). Joined on site_code.
# (fwe.info recalculations have been dropped from this comparison.)

# LEPA reference data for the selected comparison sites.
# Year scope: 2014-2022 (LRMI-era only — pre-2014 LEPA EQRs used the
# old single-DSFI assessment, see script 4 / diagnostic 8).
YEAR_MIN <- 2014
YEAR_MAX <- 2022
data_lepa <- read_excel("Inputs/EQR_data_for_comparison.xlsx",
                        guess_max = 2500) |>
  rename(epa_eqr = eqr, epa_eqc = eqc) |>
  filter(year >= YEAR_MIN, year <= YEAR_MAX)

# LT-OTL calculation from script 6 (one row per site_code)
data_lt <- read_excel("Outputs/6_LRMI_with_comparison.xlsx") |>
  select(site_code, lt_otl_eqr = LRMI, lt_otl_eqc = LRMI_class)

# Join: keep every comparison row; warn about any LT-OTL misses.
missing_lt <- setdiff(data_lepa$site_code, data_lt$site_code)
if (length(missing_lt) > 0) {
  cat(sprintf("WARNING: %d comparison site_code(s) have no LT-OTL match:\n  %s\n",
              length(missing_lt), paste(missing_lt, collapse = ", ")))
}
data <- left_join(data_lepa, data_lt, by = "site_code")

# Clean and type-cast
data <- data |>
  mutate(
    epa_eqr     = as.numeric(epa_eqr),
    lt_otl_eqr  = as.numeric(lt_otl_eqr),
    river_type  = as.factor(river_type),
    epa_eqc     = trimws(epa_eqc),
    lt_otl_eqc  = trimws(as.character(lt_otl_eqc)),
    diff_lt_epa = lt_otl_eqr - epa_eqr,
    epa_eqc     = factor(epa_eqc,    levels = c("Bad", "Poor", "Moderate", "Good", "High")),
    lt_otl_eqc  = factor(lt_otl_eqc, levels = c("Bad", "Poor", "Moderate", "Good", "High")),
    date        = as.Date(date, format = "%d/%m/%Y"),
    year        = as.numeric(year),
    point_label = paste(site_id, year, sep = "_"),
    site_label  = paste(site_id, river_name, sep = ": ")
  ) |>
  group_by(site_id) |>
  mutate(
    latest_year_for_site   = max(year, na.rm = TRUE),
    latest_year_epa_eqc    = epa_eqc[year    == latest_year_for_site][1],
    latest_year_lt_otl_eqc = lt_otl_eqc[year == latest_year_for_site][1]
  ) |>
  ungroup()

# Aggregate data by site to get unique sampling locations and their attributes
sites_data <- data |>
  # Add this to ensure we can identify the latest year
  group_by(site_id) |>
  mutate(latest_sampling = max(year, na.rm = TRUE)) |>
  # Keep only the latest sampling for each site
  filter(year == latest_sampling) |>
  # If a site has multiple samples in the latest year, pick one (e.g., the last one)
  group_by(site_id, river_name, river_type, latitude, longitude, catchment) |>
  slice_tail(n = 1) |>
  # Now select and rename the columns you want for mapping
  select(
    site_id,
    river_name,
    river_type,
    latitude,
    longitude,
    catchment,
    latest_eqr = epa_eqr,
    latest_eqc = epa_eqc,
    year = latest_sampling,
    site_label
  ) |>
  ungroup() |>
  # Convert to numeric for mapping
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    # Create factor for ecological quality class
    latest_eqc = factor(latest_eqc, levels = c("Bad", "Poor", "Moderate", "Good", "High")),
    # Add a flag for sites to label
    label_site = TRUE
  )

# -----------------------------------------------
# Get Lithuania borders using rgeoboundaries
# -----------------------------------------------

# Get Lithuania boundaries using rgeoboundaries
lithuania_boundary <- geoboundaries(country = "Lithuania", adm_lvl = "adm0")

# Transform to a specified CRS if needed
lithuania_sf <- st_transform(lithuania_boundary, 4326)  # WGS84

# -----------------------------------------------
# Load catchment data from GeoDatabase
# -----------------------------------------------

# Define the path to the geodatabase
gdb_path <- "C:/Users/natha/OneDrive - Gamtos Tyrimu Centras/Data/Lithuanian data/Geographic data/UETK_2024-05-02.gdb/UETK_2024-05-02.gdb"

# List the feature classes in the geodatabase
gdb_layers <- st_layers(gdb_path)

# Read the catchment layer from the geodatabase
catchments <- st_read(dsn = gdb_path, layer = "upiu_baseinu_rajonai")

# Read the river layer from the geodatabase
rivers <- st_read(gdb_path, layer = "upes_l")

# Ensure the catchments and rivers are in the same CRS as our map (WGS84)
catchments <- st_transform(catchments, 4326)
rivers <- st_transform(rivers, 4326)

# Filter rivers to only include those with sampling points
# First, get the unique river names from your sampling data
sampled_rivers <- unique(data$river_name)

# Add the major Lithuanian rivers if not already included
major_rivers <- c("Nemunas", "Neris", "Venta", "Šešupė", "Šventoji", "Minija", "Jūra", "Mūša", "Nevėžis", "Daugyvenė", "Nova")
rivers_to_plot <- unique(c(sampled_rivers, major_rivers))

# Print column names to help with matching (for debugging)
cat("Rivers layer column names:\n")
print(names(rivers))
cat("\nSampled river names from data:\n")
print(sampled_rivers)
cat("\nRivers to plot (including Nemunas and Neris):\n")
print(rivers_to_plot)

# Filter rivers - adjust the column name as needed based on your geodatabase
# Common column names might be: "pavadinimas", "Pavadinimas", "PAVADINIMAS", "name", etc.
# We'll need to check which column contains the river names
rivers_filtered <- rivers |>
  filter(pavadinimas %in% rivers_to_plot)

# If the above doesn't work, try other common column names
if(nrow(rivers_filtered) == 0) {
  cat("\nTrying alternative column name 'upės_pavadinimas'...\n")
  rivers_filtered <- rivers |>
    filter(upės_pavadinimas %in% rivers_to_plot)
}

# Use the filtered rivers for plotting
rivers <- rivers_filtered

cat("\nNumber of rivers after filtering:", nrow(rivers), "\n")
cat("Rivers included:\n")
if(nrow(rivers) > 0) {
  # Try to print the river names from the filtered dataset
  if("pavadinimas" %in% names(rivers)) {
    print(unique(rivers$pavadinimas))
  } else if("upės_pavadinimas" %in% names(rivers)) {
    print(unique(rivers$upės_pavadinimas))
  }
}

# Create a mapping between your data's catchment names and the geodatabase names
catchment_mapping <- data.frame(
  data_name = c("Nemunas", "Lielupe", "Venta", "Dauguva"),
  gdb_name = c("Nemuno upių baseinų rajonas", "Lielupės upių baseinų rajonas",
               "Ventos upių baseinų rajonas", "Dauguvos upių baseinų rajonas")
)

# Add a catchment field to match with your site data
catchments$catchment <- NA
for (i in 1:nrow(catchment_mapping)) {
  catchments$catchment[catchments$baseino_raj_pavadinimas == catchment_mapping$gdb_name[i]] <-
    catchment_mapping$data_name[i]
}

# Check if the mapping was successful
print(table(catchments$catchment))

# Convert sites data to sf object for mapping
sites_sf <- sites_data|>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# -----------------------------------------------
# Prepare river labels
# -----------------------------------------------

# Create centroids for river labels
# First, identify the column name for river names
river_name_col <- if("pavadinimas" %in% names(rivers)) {
  "pavadinimas"
} else if("upės_pavadinimas" %in% names(rivers)) {
  "upės_pavadinimas"
} else {
  # If neither exists, try to find any column with "name" in it
  name_cols <- grep("name|pavadinimas", names(rivers), ignore.case = TRUE, value = TRUE)
  if(length(name_cols) > 0) name_cols[1] else NULL
}

# Create river labels if we found the name column
if(!is_null(river_name_col) && nrow(rivers) > 0) {
  # Calculate better label positions using point_on_surface instead of centroid
  # This ensures the point is actually on the river, not in empty space
  river_labels <- rivers |>
    st_transform(3857) |>  # Transform to projected CRS for better calculation
    group_by(!!sym(river_name_col)) |>
    summarise(do_union = TRUE) |>  # Union all segments of the same river
    st_point_on_surface() |>  # Get a point on the river surface
    st_transform(4326) |>  # Transform back to WGS84
    rename(river_name = !!sym(river_name_col))

  cat("\nRiver labels prepared for:", nrow(river_labels), "rivers\n")
} else {
  cat("\nWarning: Could not find river name column for labeling\n")
  river_labels <- NULL
}

# -----------------------------------------------
# WFD color scheme and map elements
# -----------------------------------------------

# WFD ecological status color scheme - enhanced for better visibility
wfd_colors <- c("High" = "#0000FF",       # Pure blue
                "Good" = "#00AA00",       # Darker green (more visible)
                "Moderate" = "#FFD700",   # Gold
                "Poor" = "#FF6600",       # Darker orange
                "Bad" = "#FF0000")        # Red

# River type shape scale
river_shapes <- c("1" = 16, # circle
                  "2" = 17, # triangle
                  "3" = 15, # square
                  "4" = 18, # diamond
                  "5" = 8)  # asterisk

# WFD class boundary breaks (used as axis tick marks in EQR plots — v2)
wfd_breaks <- c(0, 0.30, 0.40, 0.60, 0.80, 1.0)

# -----------------------------------------------
# Figure 1: Map of Lithuanian River Sampling Sites
# -----------------------------------------------

# Create the map
map <- ggplot() +
  # Add Lithuania boundary
  geom_sf(data = lithuania_sf, fill = "grey97", color = "black", size = 0.5) +

  # # Add the catchments
  # geom_sf(data = catchments, aes(fill = "transparent"), alpha = 0.15, color = "black") +

  # Add the rivers
  geom_sf(data = rivers,
          color = "steelblue",
          alpha = 0.6,
          linewidth = 0.3) +

  # Add sampling sites
  geom_sf(data = sites_sf,
          aes(shape = river_type,
              color = latest_eqc),
          size = 2,
          stroke = 1.5,
          alpha = 1) +

  # Add river labels FIRST (LOWER PRIORITY - drawn behind site labels)
  {if(!is_null(river_labels)) {
    geom_text_repel(
      data = river_labels |>
        st_coordinates() |>
        as.data.frame() |>
        bind_cols(river_labels |> st_drop_geometry()),
      aes(x = X, y = Y, label = river_name),
      color = "steelblue",
      size = 2,
      fontface = "italic",
      alpha = 0.9,
      box.padding = 0.5,
      point.padding = 0.3,
      force = 1,  # Lower force so river labels yield to site labels
      force_pull = 0.5,  # Reduced pull towards original position
      segment.color = "steelblue",
      segment.alpha = 0.5,
      segment.size = 0.3,
      min.segment.length = 0,
      max.overlaps = 20,  # Fewer max overlaps - will hide some river labels if needed
      bg.color = "white",
      bg.r = 0.1,
      seed = 123  # Same seed for consistent layout
    )
  }} +

  # Add site labels LAST for all sites in black (PRIORITY LABELS - drawn on top)
  ggrepel::geom_text_repel(
    data = st_coordinates(sites_sf)|>
      as.data.frame()|>
      bind_cols(sites_sf|> st_drop_geometry()),
    aes(x = X, y = Y, label = site_id),
    size = 2.5,  # Slightly smaller text
    fontface = "bold",
    color = "black",  # All labels in black
    box.padding = 0.3,
    point.padding = 0.2,
    force = 5,  # Higher force to ensure site labels stay in good positions
    segment.color = "gray50",
    min.segment.length = 0,
    max.overlaps = 30,  # Allow more overlaps to fit all labels
    seed = 123  # Set seed for reproducibility
  ) +

  # Apply WFD colors for ecological status
  scale_color_manual(values = wfd_colors, name = "Ecological Quality Class\n(Latest sampling year)") +

  # River type shapes
  scale_shape_manual(values = river_shapes, name = "River Type") +

  # Add catchment fill color scale
  scale_fill_viridis_d(
    option = "plasma",
    name = "River basin",
    na.value = "lightgray"  # For any catchments not matched in the mapping
  ) +

  # Ensure points are drawn on top of catchments and improve legend
  guides(
    color = guide_legend(override.aes = list(size = 5, stroke = 1.5)),
    shape = guide_legend(override.aes = list(size = 5, stroke = 1.5))
  ) +

  # Set coordinate limits for Lithuania
  coord_sf(
    xlim = c(20.5, 27),  # longitude limits
    ylim = c(53.8, 56.5)  # latitude limits
  ) +

  # Add north arrow and scale bar
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    pad_x = unit(0.2, "in"),
    pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25
  ) +

  # Add axis labels
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +

  # Apply a clean theme
  theme_bw() +
  theme(
    # Text elements - consistent with all figures
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),

    # Panel elements
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),

    # Legend position and appearance
    legend.position = "right",
    legend.box = "vertical",
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    legend.margin = margin(5, 5, 5, 5)
  )

# -----------------------------------------------
# Create a separate inset map showing Lithuania's location in Europe
# -----------------------------------------------

# Get European country boundaries
europe <- ne_countries(scale = "medium", continent = "Europe", returnclass = "sf")

# Create the inset map
inset_map <- ggplot() +
  geom_sf(data = europe, fill = "lightgray", color = "white", size = 0.1) +
  geom_sf(data = lithuania_sf, fill = "black", color = "black", size = 0.3) +
  coord_sf(
    xlim = c(-10, 40),  # Europe longitude limits
    ylim = c(35, 70)    # Europe latitude limits
  ) +
  theme_void() +
  theme(
    # panel.background = element_rect(fill = "aliceblue"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white", color = "gray", size = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )

# -----------------------------------------------
# Create final map with legend at bottom
# -----------------------------------------------

# Create map with legend at bottom
map_bottom_legend <- map +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.direction = "horizontal"
  )

# Combine with inset
combined_map <- ggdraw() +
  draw_plot(map_bottom_legend) +
  draw_plot(inset_map, x = 0.0845, y = 0.15, width = 0.203, height = 0.35)

# Save the combined map
ggsave("Plots/Figure_1.tiff", combined_map,
       width = 10, height = 8, dpi = 450, bg = "white", compression = "lzw")

#-----------------------------------------------
# Statistical analysis for correlations
#-----------------------------------------------

# Check normality of EQR values using histogram
hist_epa <- hist(data$epa_eqr)
hist_lt <- hist(data$lt_otl_eqr)

# Check normality of EQR values using Shapiro-Wilk test
shapiro_epa <- shapiro.test(data$epa_eqr)
shapiro_lt <- shapiro.test(data$lt_otl_eqr)

# Calculate both Pearson (parametric) and Spearman (non-parametric) correlations
pearson_lt_epa <- cor.test(data$epa_eqr, data$lt_otl_eqr, method = "pearson")
spearman_lt_epa <- cor.test(data$epa_eqr, data$lt_otl_eqr, method = "spearman")

# Calculate R-squared values
paste(round((pearson_lt_epa$estimate^2)*100, 2),"%")

# Select the appropriate correlation method based on normality test results
# If any dataset is non-normal (p < 0.05), use Spearman, otherwise use Pearson
use_nonparametric <- any(c(shapiro_epa$p.value, shapiro_lt$p.value) < 0.05)

# Function to format correlation text
# n = number of complete pairs feeding the correlation (added so the
# reader can see how many sites were considered; matches script 6).
n_pairs <- sum(!is.na(data$epa_eqr) & !is.na(data$lt_otl_eqr))

format_pearson_text <- function(test_result) {
  paste0("Pearson correlation:\n",
         "R² = ", format(round(test_result$estimate^2, 3), nsmall = 3),
         ", p ", ifelse(test_result$p.value < 0.001, "< 0.001",
                       paste0("= ", format(round(test_result$p.value, 3), nsmall = 3))),
         "\nn = ", n_pairs)
}

format_spearman_text <- function(test_result) {
  paste0("Spearman rank correlation:\n",
         "ρ = ", format(round(test_result$estimate, 3), nsmall = 3),
         ", p ", ifelse(test_result$p.value < 0.001, "< 0.001",
                       paste0("= ", format(round(test_result$p.value, 3), nsmall = 3))),
         "\nn = ", n_pairs)
}

# Choose the correlation text based on normality test results
if(use_nonparametric) {
  cat("Using Spearman rank correlation (non-parametric) due to non-normal data distribution\n")
  lt_otl_corr_text <- format_spearman_text(spearman_lt_epa)
} else {
  cat("Using Pearson correlation (parametric) as data appear normally distributed\n")
  lt_otl_corr_text <- format_pearson_text(pearson_lt_epa)
}

# -----------------------------------------------
# Define WFD color scheme and theme
#-----------------------------------------------

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

# #-----------------------------------------------
# # FIGURE 2: Comparison of EQR values between methods
# #-----------------------------------------------
#
# # Identify outliers that deviate from the 1:1 line
# # Calculate the distance from the 1:1 line for each point
# data <- data |>
#   mutate(
#     # Distance from 1:1 line for LT-OTL
#     lt_otl_deviation = abs(lt_otl_eqr - epa_eqr)
#   )
#
# # Define outliers as points with deviation > 0.05 (adjustable threshold)
# deviation_threshold <- 0.001
# lt_otl_outliers <- data |> filter(lt_otl_deviation > deviation_threshold)
#
# # Create scatterplot for Lithuanian OTL (Plot A)
# plot_lt_otl <- ggplot(data, aes(x = epa_eqr, y = lt_otl_eqr)) +
#   # Add background color bands for EQR classes
#   annotate("rect", xmin = 0.8, xmax = 1.0, ymin = 0.8, ymax = 1.0,
#            fill = wfd_colors["High"], alpha = 0.3) +
#   annotate("rect", xmin = 0.6, xmax = 0.8, ymin = 0.6, ymax = 0.8,
#            fill = wfd_colors["Good"], alpha = 0.3) +
#   annotate("rect", xmin = 0.4, xmax = 0.6, ymin = 0.4, ymax = 0.6,
#            fill = wfd_colors["Moderate"], alpha = 0.3) +
#   annotate("rect", xmin = 0.3, xmax = 0.4, ymin = 0.3, ymax = 0.4,
#            fill = wfd_colors["Poor"], alpha = 0.3) +
#   annotate("rect", xmin = 0.0, xmax = 0.3, ymin = 0.0, ymax = 0.3,
#            fill = wfd_colors["Bad"], alpha = 0.3) +
#   # Add perfect agreement line (1:1)
#   geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
#   # Add points with different shapes for river types (all black) - matching Figure 1 size
#   geom_point(aes(shape = river_type),
#              size = 2, stroke = 1.5, alpha = 0.8, color = "black") +
#   # Add labels for outliers that deviate from 1:1 line
#   # geom_text_repel(data = lt_otl_outliers,
#   #                 aes(x = epa_eqr, y = lt_otl_eqr, label = point_label),
#   #                 size = 2.5,
#   #                 color = "red",
#   #                 fontface = "bold",
#   #                 box.padding = 0.5,
#   #                 point.padding = 0.3,
#   #                 segment.color = "red",
#   #                 segment.alpha = 0.6,
#   #                 max.overlaps = 20,
#   #                 min.segment.length = 0) +
#   # Add correlation text in bottom right corner
#   annotate("text", x = 0.995, y = 0.075,
#            label = lt_otl_corr_text,
#            hjust = 1, size = 3, fontface = "bold") +
#   # Set proper axis limits
#   scale_x_continuous(limits = c(0, 1), breaks = wfd_breaks) +
#   scale_y_continuous(
#     limits = c(0, 1),
#     breaks = wfd_breaks,
#     sec.axis = dup_axis(
#       name = "",
#       breaks = c(0.15, 0.35, 0.5, 0.7, 0.9),
#       labels = c("BAD", "POOR", "MODERATE", "GOOD", "HIGH")
#     )
#   ) +
#   # Customize shape aesthetics
#   scale_shape_manual(values = river_shapes,
#                     name = "River Type") +
#   # Add legend overrides to match Figure 1
#   guides(
#     shape = guide_legend(override.aes = list(size = 5, stroke = 1.5))
#   ) +
#   # Labels
#   labs(
#       x = "Original LEPA EQR",
#       y = "EQR after OTL standardisation"
#   ) +
#   # Apply WFD theme
#   wfd_theme()
#
# # Display the plot
# print(plot_lt_otl)
#
# # Save the plot
# ggsave("Plots/Figure_3.tiff", plot_lt_otl,
#        width = 10, height = 8, dpi = 450, bg = "white", compression = "lzw")

# #-----------------------------------------------
# # FIGURE 3: Bland-Altman plots for agreement analysis
# #-----------------------------------------------
#
# # Prepare data for Bland-Altman plots
# data <- data |>
#   mutate(
#     # Calculate differences and means for Bland-Altman plots
#     # For Lithuanian OTL
#     lt_diff = lt_otl_eqr - epa_eqr,  # Difference
#     lt_mean = (lt_otl_eqr + epa_eqr) / 2  # Mean
#   )
#
# # Calculate Bland-Altman statistics
# # For Lithuanian OTL
# lt_mean_diff <- mean(data$lt_diff, na.rm = TRUE)
# lt_sd_diff <- sd(data$lt_diff, na.rm = TRUE)
# lt_lower_loa <- lt_mean_diff - 1.96 * lt_sd_diff
# lt_upper_loa <- lt_mean_diff + 1.96 * lt_sd_diff
#
# # Identify outliers (points outside the limits of agreement)
# data <- data |>
#   mutate(
#     lt_outlier = (lt_diff > lt_upper_loa) | (lt_diff < lt_lower_loa)
#   )
#
# # Create Bland-Altman plot for Lithuanian OTL (Plot A)
# plot_lt_bland <- ggplot(data, aes(x = lt_mean, y = lt_diff)) +
#   # Add reference lines
#   geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
#   geom_hline(yintercept = lt_mean_diff, linetype = "solid", color = "blue") +
#   geom_hline(yintercept = lt_lower_loa, linetype = "dotted", color = "red") +
#   geom_hline(yintercept = lt_upper_loa, linetype = "dotted", color = "red") +
#
#   # Add points with different shapes for river types (all black) - matching Figure 1 size
#   geom_point(aes(shape = river_type), size = 2, stroke = 1.5, alpha = 0.8, color = "black") +
#
#   # Add reference line labels
#   annotate("text", x = 0.15, y = lt_mean_diff,
#            label = paste0("Mean: ", round(lt_mean_diff, 3)),
#            hjust = 1, color = "blue", fontface = "bold", size = 3) +
#   annotate("text", x = 0.15, y = lt_lower_loa,
#            label = paste0("Lower LoA: ", round(lt_lower_loa, 3)),
#            hjust = 1, color = "red", size = 3) +
#   annotate("text", x = 0.15, y = lt_upper_loa,
#            label = paste0("Upper LoA: ", round(lt_upper_loa, 3)),
#            hjust = 1, color = "red", size = 3) +
#
#   # Add labels to outlier points, colored by ecological status
#   geom_text_repel(
#     data = subset(data, lt_outlier == TRUE),
#     aes(label = point_label, color = epa_eqc),  # Color by EPA ecological class
#     size = 2.5,
#     nudge_y = 0.005,
#     box.padding = 0.5,
#     point.padding = 0.2,
#     segment.color = "grey50",
#     fontface = "bold",
#     show.legend = FALSE  # Remove legend for this layer
#   ) +
#
#   # Set WFD colors for the labels
#   scale_color_manual(values = wfd_colors, name = "EPA EQC") +
#
#   # Set proper axis limits and labels with fixed decimal format
#   scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
#   scale_y_continuous(
#     limits = c(-0.1, 0.05),
#     breaks = seq(-0.1, 0.05, 0.01),
#     labels = function(x) sprintf("%.3f", x)  # Format as fixed decimal with 3 places
#   ) +
#
#   # Customize shape aesthetics
#   scale_shape_manual(values = river_shapes,
#                     name = "River Type") +
#
#   # Add legend overrides to match Figure 1
#   guides(
#     shape = guide_legend(override.aes = list(size = 5, stroke = 1.5))
#   ) +
#
#   # Labels
#   labs(
#     x = "Mean EQR",
#     y = "Difference (LT-OTL - LEPA)"
#   ) +
#
#   # Apply theme
#   wfd_theme()
#
# # Display the plot
# print(plot_lt_bland)
#
# # Save the plot
# ggsave("Plots/Figure3.tiff", plot_lt_bland,
#        width = 8, height = 4, dpi = 450, bg = "white", compression = "lzw")

# Bland-Altman plots evaluate agreement between two measurement methods by plotting
# differences (y-axis) against means (x-axis). Our plot shows excellent agreement
# between original EPA EQR values and the Lithuanian OTL standardization method,
# with mean differences near zero (blue line) indicating negligible systematic bias.
# The narrow 95% limits of agreement (red dotted lines) demonstrate that differences
# are minimal across the entire measurement range and across all river types. This
# strong agreement confirms that taxonomic standardization maintains ecological
# assessment integrity while potentially reducing human error in taxonomic
# identification and increasing the reliability of water quality monitoring under
# the EU Water Framework Directive.

# Points falling outside the 95% limits of agreement (LoA) represent samples where
# the difference between methods exceeds what would be expected by random variation
# alone. In our plot, the few points outside these bounds warrant individual
# investigation as they may indicate specific taxa or ecological conditions where
# standardization has a more pronounced effect. However, these outliers constitute
# less than 5% of all observations, which is consistent with our statistical
# expectation. For practical applications, these occasional larger differences are
# unlikely to affect overall ecological status classification, especially since most
# discrepancies occur in samples with moderate to good ecological quality where small
# EQR shifts rarely cross class boundaries.

# #-----------------------------------------------
# # FIGURE 4: EQR by river type comparison
# #-----------------------------------------------
#
# # Prepare data for boxplot - with enhanced approach to carry EQC values
# data_long <- data |>
#   # First create the long format with all necessary data
#   select(site_id, river_name, river_type, year, catchment,
#          epa_eqr, lt_otl_eqr,
#          epa_eqc, lt_otl_eqc) |>
#   pivot_longer(
#     cols = c(epa_eqr, lt_otl_eqr),
#     names_to = "method",
#     values_to = "eqr"
#   ) |>
#   # Add the matching EQC for each row based on method
#   mutate(
#     method = factor(method,
#                    levels = c("epa_eqr", "lt_otl_eqr"),
#                    labels = c("LEPA", "LT-OTL")),
#     # Get the corresponding EQC based on which method this row represents
#     eqc = case_when(
#       method == "LEPA" ~ as.character(epa_eqc),
#       method == "LT-OTL" ~ as.character(lt_otl_eqc)
#     ),
#     eqc = factor(eqc, levels = c("Bad", "Poor", "Moderate", "Good", "High"))
#   )
#
# # Calculate mean EQR values by river type and method for potential annotation
# river_type_stats <- data_long |>
#   group_by(river_type, method) |>
#   summarise(
#     mean_eqr = mean(eqr, na.rm = TRUE),
#     sd_eqr = sd(eqr, na.rm = TRUE),
#     n = n(),
#     .groups = "drop"
#   )
#
# # Define consistent colors for methods that won't be confused with WFD colors
# # Using a color scheme distinct from the WFD classification colors
# method_colors <- c(
#   "LEPA" = "#5D4C46",      # Dark brown
#   "LT-OTL" = "#8E24AA"     # Purple
# )
#
# # Boxplot of EQR by river type and method with WFD styling
# boxplot_river_type <- ggplot(data_long, aes(x = river_type, y = eqr, fill = method)) +
#   # Add background color bands for EQR classes
#   annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.8, ymax = 1.0,
#            fill = wfd_colors["High"], alpha = 0.2) +
#   annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.6, ymax = 0.8,
#            fill = wfd_colors["Good"], alpha = 0.2) +
#   annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.4, ymax = 0.6,
#            fill = wfd_colors["Moderate"], alpha = 0.2) +
#   annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.3, ymax = 0.4,
#            fill = wfd_colors["Poor"], alpha = 0.2) +
#   annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.0, ymax = 0.3,
#            fill = wfd_colors["Bad"], alpha = 0.2) +
#
#   # Add boxplots with solid borders
#   geom_boxplot(
#     alpha = 0.7,
#     outlier.shape = NA,   # Hide outliers as we'll show individual points
#     position = position_dodge(width = 0.8),
#     width = 0.7,
#     color = "black",      # Black border for all boxplots
#     linewidth = 0.5       # Slightly thicker border
#   ) +
#
#   # Add individual data points - all black
#   geom_point(
#     position = position_jitterdodge(
#       jitter.width = 0.1,
#       dodge.width = 0.8
#     ),
#     size = 2,
#     alpha = 0.7,
#     color = "black",      # All points in black
#     show.legend = FALSE   # Don't show duplicate legend for points
#   ) +
#
#   # Set colors for boxplots only (points are all black)
#   scale_fill_manual(
#     values = method_colors,
#     name = "EQR calculation"
#   ) +
#
#   # Add legend overrides to match Figure 1
#   guides(
#     fill = guide_legend(override.aes = list(size = 5))
#   ) +
#
#   # Set proper axis scaling
#   scale_y_continuous(
#     limits = c(0, 1),
#     breaks = wfd_breaks,
#     sec.axis = dup_axis(
#       name = "",
#       breaks = c(0.15, 0.35, 0.5, 0.7, 0.9),
#       labels = c("BAD", "POOR", "MODERATE", "GOOD", "HIGH")
#     )
#   ) +
#
#   # Labels
#   labs(
#     x = "River Type",
#     y = "EQR"
#   ) +
#
#   # Apply WFD theme
#   wfd_theme()
#
# # Display the plot
# print(boxplot_river_type)
#
# # Save the plot with appropriate dimensions
# ggsave("Plots/Figure_4.tiff", boxplot_river_type,
#        width = 10, height = 8, dpi = 450, bg = "white", compression = "lzw")

# The boxplot visualization shows the distribution of Ecological Quality Ratio (EQR)
# values across different river types using two assessment methods. The background
# bands indicate the ecological quality classes from Bad (red) to High (blue) according
# to the Water Framework Directive. Each river type displays two boxplots
# representing the original EPA assessment method and the Lithuanian Operational Taxa
# List (LT-OTL) standardization method. Individual data points are overlaid to show
# the actual distribution of measurements. The plot demonstrates that while there are
# some differences between methods, the overall ecological quality assessment remains
# consistent across standardization approaches for all river types, supporting the
# conclusion that taxonomic standardization maintains assessment integrity while
# potentially reducing errors.

#-----------------------------------------------
# Sample-level comparison (per site_code)
#-----------------------------------------------
# Builds a per-sample table that flags where LEPA and LT-OTL disagree,
# either numerically (EQR) or categorically (EQC). Sorted by absolute
# EQR difference so the largest discrepancies surface at the top, which
# is the natural starting point for case-by-case investigation.
#
# Two flag columns:
#   eqr_diff       — lt_otl_eqr - epa_eqr (signed; positive = LT-OTL higher)
#   eqc_match      — TRUE if both methods assign the same class
# The residual is reported as a continuous value; no banded categories
# are applied (the earlier 0.05 / 0.10 bands had no external anchor and
# have been dropped).

sample_compare <- data |>
  transmute(
    site_code,
    site_id,
    river_name,
    river_type,
    year,
    epa_eqr,
    lt_otl_eqr,
    eqr_diff = round(lt_otl_eqr - epa_eqr, 4),
    epa_eqc,
    lt_otl_eqc,
    eqc_match = !is.na(epa_eqc) & !is.na(lt_otl_eqc) &
                as.character(epa_eqc) == as.character(lt_otl_eqc)
  ) |>
  arrange(desc(abs(eqr_diff)))

# Console summary
cat("\n--- Sample-level comparison ---\n")
cat(sprintf("Total samples: %d\n", nrow(sample_compare)))
cat(sprintf("|EQR diff| summary:\n"))
print(summary(abs(sample_compare$eqr_diff)))
cat(sprintf("\nEQC class agreement: %d / %d (%.1f%%)\n",
            sum(sample_compare$eqc_match, na.rm = TRUE),
            sum(!is.na(sample_compare$eqc_match)),
            100 * mean(sample_compare$eqc_match, na.rm = TRUE)))

# List samples where the EQC class differs
class_mismatch <- sample_compare |> filter(!eqc_match)
if (nrow(class_mismatch) > 0) {
  cat(sprintf("\nClass mismatches (%d): LEPA -> LT-OTL\n", nrow(class_mismatch)))
  print(as.data.frame(class_mismatch |>
                        select(site_code, year, epa_eqr, lt_otl_eqr,
                               eqr_diff, epa_eqc, lt_otl_eqc)),
        row.names = FALSE)
} else {
  cat("\nNo class mismatches.\n")
}

# Top numeric differences (regardless of class)
top_diff <- head(sample_compare, 10)
cat("\nTop 10 EQR differences (by absolute value):\n")
print(as.data.frame(top_diff |>
                      select(site_code, year, epa_eqr, lt_otl_eqr,
                             eqr_diff, epa_eqc, lt_otl_eqc, eqc_match)),
      row.names = FALSE)

# Save full table for case-by-case follow-up
write_xlsx(sample_compare,
           "Outputs/7_sample_level_comparison.xlsx")
cat("Saved: Outputs/7_sample_level_comparison.xlsx\n")
