# LRMI calculation pipeline (LT-OTL)

R-based pipeline that takes raw Lithuanian macroinvertebrate sampling
data, harmonises taxonomy through a project-specific Operational
Taxalist (OTL), and recalculates the multimetric **Lithuanian River
Macroinvertebrate Index (LRMI)** under the EU Water Framework Directive
(WFD). The recalculated values are then compared against LEPA's
published EQRs/EQCs for a curated subset of sites to test whether OTL
standardisation materially alters the assessment.

The full methods write-up is in [`Pipeline_summary.qmd`](Pipeline_summary.qmd)
(rendered to [`Pipeline_summary.pdf`](Pipeline_summary.pdf)).

## Repository layout

```
.
├── Scripts/                 R scripts, numbered 1–10
├── Inputs/                  Raw input files (Excel, geodatabase paths)
├── Outputs/                 Script-generated tables (script N → N_*.xlsx)
├── Plots/                   Script-generated figures
├── Pipeline_summary.qmd     Quarto methods document
├── Pipeline_summary.pdf     Rendered version of the above
└── Calculating OTL indices.Rproj
```

Each script's outputs use the producing script's numeric prefix
(e.g. script 6 writes `Outputs/6_LRMI_with_comparison.xlsx`).

## Pipeline at a glance

| Stage | Script                                       | Purpose                                                                     |
|------:|----------------------------------------------|-----------------------------------------------------------------------------|
| 1     | `1_generate_decision_tree_html_v17.R`        | Render the OTL decision-tree HTML from editable text blocks                 |
| 2     | `2_OTU_assignment_v22.R`                     | Apply the decision tree to assign an OTU to every taxon                     |
| 3     | `3_DSFI_calculator_v15.R`                    | Compute the Danish Stream Fauna Index (DSFI) per sample                     |
| 4     | `4_calculation_of_taxonomic_indices.R`       | Compute BMWP / ASPT, group richness/abundance, compound indices             |
| 5     | `5_merge_indices_and_metadata.R`             | Join DSFI + taxonomic indices + site metadata                               |
| 6     | `6_calculate_LRMI.R`                         | Calculate the LRMI and ecological-quality class per site-year               |
| 7     | `7_comparing_eqrs.R`                         | Compare LRMI / class against LEPA's published values                        |
| 8     | `8_DEP_OTL_vs_validated.R` (diagnostic)      | Test whether residual LEPA-vs-LT differences trace to #DEP                  |
| 9     | `9_per_metric_diagnostic.R` (diagnostic)     | Per-metric breakdown for class-mismatch site-years                          |
| 10    | `10_lepa_per_metric_comparison.R` (diagnostic)| Head-to-head against LEPA's published per-metric EQRs                       |

## Running the pipeline

1. Open `Calculating OTL indices.Rproj` in RStudio so the working
   directory is set to the project root.
2. Source the scripts in numeric order (1 → 10). Each script reads
   either from `Inputs/` or from an earlier script's output in
   `Outputs/`, and writes its own outputs back to `Outputs/` (or
   `Plots/`).
3. Re-render `Pipeline_summary.qmd` via Quarto if you want a refreshed
   PDF that reflects the current code.

### Year scope

All analyses from script 4 onwards are restricted to **2014–2022**, the
"operational LRMI era". Pre-2014 LEPA EQRs were calculated under the
older single-DSFI assessment, not the LRMI, so including them would
conflate methodological era with taxonomic-standardisation effects.

### R dependencies

`tidyverse`, `readxl`, `writexl`, `vegan`, `reshape2`, `ggplot2`, `sf`,
`ggrepel`, `cowplot`, `rnaturalearth`, `rnaturalearthdata`,
`rgeoboundaries`, `ggspatial`.

R version **≥ 4.0.5** (matching the Sidagytė-Copilas & Arbačiauskas
(2022) reference environment).

## Citation

Manuscript in preparation. Please cite the rendered methods document
([`Pipeline_summary.pdf`](Pipeline_summary.pdf)) until the paper is
public.

## Licence

This repository is released under the **MIT Licence**. See
[`LICENSE`](LICENSE) for the full text.
