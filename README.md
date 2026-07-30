# **Lithuanian Operational Taxalist (OTL)**
## **Data and code for:** Standardizing freshwater macroinvertebrate taxonomy: a Lithuanian operational taxalist for ecological quality assessments and biodiversity research.

R-based pipeline that takes raw Lithuanian macroinvertebrate sampling
data, harmonises taxonomy through a project-specific Operational
Taxalist (OTL), and recalculates the multimetric **Lithuanian River
Macroinvertebrate Index (LRMI)** under the EU Water Framework Directive
(WFD). The recalculated values are then compared against LEPA's
published EQRs/EQCs for a curated subset of sites to test whether OTL
standardisation materially alters the assessment.

## The Operational Taxalist

**The OTL is the central output of this work.** It lives in its own
top-level folder, deliberately kept separate from the pipeline's
working inputs:

```
Operational Taxalist (OTL)/
└── Supplement 1 - Operational taxalist.xlsx
```

This workbook is the authoritative taxonomic reference for the project
and the document most likely to be revised in future. It contains nine
sheets:

| Sheet                 | Rows  | Contents                                                                 |
|-----------------------|------:|--------------------------------------------------------------------------|
| `Metadata`            |   129 | Data dictionary: every column of every sheet, with description and type   |
| `OTL`                 | 3,885 | The operational taxa list itself — the core reference                     |
| `Source_references`   |    46 | Full bibliographic details for each source cited in `OTL.source`          |
| `Synonyms`            | 3,885 | GBIF-derived synonymy, up to 100 synonyms per taxon, with usage keys      |
| `Excluded_taxa`       |   209 | Taxa excluded from OTU assignment                                         |
| `OTU_heirarchy`       | 1,258 | Resulting operational taxonomic units and their full taxonomic hierarchy  |
| `Specialist_taxalist` | 2,657 | Determinations from four specialists, with agreement flags               |
| `BMWP_scores`         |   105 | BMWP scores as adapted for Lithuania, mapped to OTL names                 |
| `Example_dataset`     | 4,874 | Worked example with synthetic abundances, raw name → OTL-standardised name|

**The `Metadata` sheet is the workbook's own data dictionary** — it
documents all 129 columns across the other eight sheets, giving each
column's name, a plain-language description, and its data type. Anyone
reusing the OTL should start there.

Taxonomic validation follows source-specific authorities: molluscs
against MolluscaBase; annelids, nematodes and platyhelminthes against
WoRMS; all other taxa against GBIF. Beyond names, the `OTL` sheet
carries per-taxon index flags (`dsfi_exclusion`, `index_exclusion`,
`BMWP_score`), conservation and distribution attributes (`rare`,
`non-native`, `LT_conservation_status`, `LT_red_list`, `EU_red_list`,
`prefered_habitat`), a freshwaterecology.info identifier (`ID-fwe`),
and genetic-barcode availability (`barcode_available`,
`sequenced_genes`).

[`Scripts/2_OTU_assignment_v23.R`](Scripts/2_OTU_assignment_v23.R)
reads three of these sheets — `OTL`, `Excluded_taxa` and
`Specialist_taxalist`. The remaining six are reference and
documentation for users of the taxalist rather than pipeline inputs.

> **Note:** in `Example_dataset`, the `note` column refers to
> colour-highlighted cells (green = abundances requiring aggregation,
> yellow = exclude from DSFI, red = exclude from all other indices).
> Cell fill colour is not read by `readxl`, so that information is
> available only when viewing the workbook directly.

Because the OTL is versioned alongside the code, each tagged release of
this repository captures the exact state of the taxalist used to
produce that release's results.

## Repository layout

```
.
├── Operational Taxalist (OTL)/  The OTL workbook (see above)
├── Scripts/                     R scripts, numbered 1–10
├── Inputs/                      Comparison inputs (see Data availability)
├── Outputs/                     Script-generated tables (script N → N_*.xlsx)
├── Plots/                       Script-generated figures
└── Calculating OTL indices.Rproj
```

Each script's outputs use the producing script's numeric prefix
(e.g. script 6 writes `Outputs/6_LRMI_with_comparison.xlsx`).

Two files in `Outputs/` are not script-generated and are kept as
manual QA artefacts: `3_DSFI_value_checker.xlsx` and
`1_OTL_decision_tree_v18.pdf`.

## Pipeline at a glance

| Stage | Script                                        | Purpose                                                        |
|------:|-----------------------------------------------|----------------------------------------------------------------|
| 1     | `1_generate_decision_tree_html_v18.R`         | Render the OTL decision-tree HTML from editable text blocks    |
| 2     | `2_OTU_assignment_v23.R`                      | Apply the decision tree to assign an OTU to every taxon        |
| 3     | `3_DSFI_calculator_v15.R`                     | Compute the Danish Stream Fauna Index (DSFI) per sample        |
| 4     | `4_calculation_of_taxonomic_indices.R`        | Compute BMWP / ASPT, group richness/abundance, compound indices|
| 5     | `5_merge_indices_and_metadata.R`              | Join DSFI + taxonomic indices + site metadata                  |
| 6     | `6_calculate_LRMI.R`                          | Calculate the LRMI and ecological-quality class per site-year  |
| 7     | `7_comparing_eqrs.R`                          | Compare LRMI / class against LEPA's published values           |
| 8     | `8_DEP_OTL_vs_validated.R` (diagnostic)       | Test whether residual LEPA-vs-LT differences trace to #DEP     |
| 9     | `9_per_metric_diagnostic.R` (diagnostic)      | Per-metric breakdown for class-mismatch site-years             |
| 10    | `10_lepa_per_metric_comparison.R` (diagnostic)| Head-to-head against LEPA's published per-metric EQRs          |

## Data availability

Not every file the pipeline reads is distributed through this
repository. To run it end-to-end you also need:

| File                                              | Needed by      | Where to get it |
|---------------------------------------------------|----------------|-----------------|
| `Inputs/All_sites_macroinvertebrate_data_long.xlsx` | Scripts 3, 4, 5, 8 | *Not in this repository* — see below |
| UETK geodatabase (`UETK_2024-05-02.gdb`)            | Script 7 (maps)| Lithuanian national water-body register (UETK) |

The raw macroinvertebrate dataset (~44 MB) is held outside the
repository. It can be requested from the Lithuanian Environmental
Protection Agency (LEPA):

> LEPA. (2024). *Lithuanian Environmental Protection Agency.*
> https://aaa.lrv.lt

Readers who do not have the raw dataset can still see the
standardisation in action: the `Example_dataset` sheet of the OTL
workbook carries 4,874 fake abundance records from raw taxon name through to
OTL-standardised name, OTU identifier and index-exclusion flags.

Script 7 currently points at the UETK geodatabase via an **absolute
local path** ([`7_comparing_eqrs.R:168`](Scripts/7_comparing_eqrs.R:168)).
Change `gdb_path` to your own copy before running it. The mapping
sections of script 7 are the only part of the pipeline that need it;
everything else runs without it.

The Quarto methods write-up (`Pipeline_summary.qmd` / `.pdf`) is
circulated with the manuscript rather than through this repository.

## Running the pipeline

1. Open `Calculating OTL indices.Rproj` in RStudio so the working
   directory is set to the project root.
2. Obtain the files listed under **Data availability** and place them
   at the paths shown.
3. Source the scripts in numeric order (1 → 10). Each script reads
   either from `Inputs/`, from the OTL workbook, or from an earlier
   script's output in `Outputs/`, and writes its own outputs back to
   `Outputs/` (or `Plots/`).

### Year scope

All analyses from script 4 onwards are restricted to **2014–2022**, the
"operational LRMI era". Pre-2014 LEPA EQRs were calculated under the
older single-DSFI assessment, not the LRMI, so including them would
conflate methodological era with taxonomic-standardisation effects.

### R dependencies

`tidyverse`, `readxl`, `writexl`, `vegan`, `reshape2`, `ggplot2`, `sf`,
`ggrepel`, `cowplot`, `rnaturalearth`, `rnaturalearthdata`,
`rgeoboundaries`, `ggspatial`.

R version **4.5.2**:

> R Core Team (2025). *R: A Language and Environment for Statistical
> Computing.* R Foundation for Statistical Computing, Vienna, Austria.
> https://www.R-project.org/

## Citation

If you use the Operational Taxalist or this pipeline, please cite the
accompanying manuscript:

> Osadčaja, D., Višinskienė, G., Solovjova, S., Skyrienė, G., &
> Baker, N.J. (Under review). Standardizing freshwater macroinvertebrate
> taxonomy: a Lithuanian operational taxalist for ecological quality
> assessments and biodiversity research.

<!-- TODO: add journal name, year and article DOI once accepted. -->
<!-- TODO: add the Zenodo DOI badge and software citation after the
     first tagged release is archived. -->

Machine-readable citation metadata is also provided in
[`CITATION.cff`](CITATION.cff).

## Funding

This project was funded by the Research Council of Lithuania (LMTLT),
agreement No. S-MIP-24-61.

## Licence

This repository is released under the **MIT Licence**. See
[`LICENSE`](LICENSE) for the full text.
