# ============================================================================
# OTL Decision Tree — HTML Generator
# ============================================================================
#
# This script generates the visual decision tree HTML from editable content.
# To update phrasing: edit the CONTENT section below, then source() this file.
# The HTML output is written to `output_file` (set below).
#
# Structure:
#   1. CONTENT — all text you might want to change (edit here)
#   2. REFERENCE TABLE — metric requirements (edit here)
#   3. CSS + HELPER FUNCTIONS — rarely need changing
#   4. BUILD SECTIONS — assembles the HTML from content
#   5. OUTPUT — writes the file
# ============================================================================


# ============================================================================
# 1. CONTENT — Edit text here, then source() the script to regenerate HTML
# ============================================================================

# ---- Title and subtitle ----
title    <- "Decision tree for assigning operational taxonomic units (OTUs)"
subtitle <- "Lithuanian macroinvertebrate biomonitoring &mdash; OTL v17"

# ---- Output file ----
output_file <- "Outputs/1_OTL_decision_tree_v17.html"

# ---- Changes from v16 ----
# - No semantic rule changes. Bumped for version coherence with the v17 OTU
#   assignment script, which adds a data-quality guard (cross-sheet name
#   consistency check) but does not alter any decision rules.
#
# ---- Changes from v15 ----
# - Removed Coreidae from the Hemiptera BMWP scored-taxa row (Coreidae is
#   treated as an excluded family in the .md spec and the assignment script;
#   its presence in the v15 BMWP list was a doc/script inconsistency).
# - Added two rules previously missing from the HTML but enforced by the .md
#   and the assignment script:
#     * Subspecies entries inherit OTU from the parent species.
#     * Phase 2 forced-level taxa skip the Phase 4 metric-compliance check.

# ---- Phase 1: Preparation steps ----
phase1_steps <- list(
  list(type = "header",
       text = "Step 1: Compile taxalist of aquatic macroinvertebrates occurring in Lithuania from literature and other sources"),
  list(type = "header",
       text = "Step 2: Validate and correct taxonomy against GBIF and Molluscabase backbones"),
  list(type = "header",
       text = 'Step 3: Specialists and practitioners are provided only validated taxa at the <u>species level</u> and asked to state the taxonomic level to which they can identify each species'),
  list(type = "process",
       text = paste0(
         'Step 4: Flag taxa excluded from EQR calculations using the <b>excluded_taxa sheet</b> as the source of truth. ',
         'Entirely excluded groups (all families excluded): ',
         'Porifera, Cnidaria, Nematoda, Nematomorpha, Nemertea, Bryozoa (phyla); ',
         'Polychaeta, Arachnida, Collembola, Maxillopoda, Branchiopoda (classes); ',
         'Stylommatophora, Dolichomicrostomida, Prolecithophora, Rhabdocoela, Anostraca (orders). ',
         'Additional excluded families: Coreidae, Gerridae, Hydrometridae, Mesoveliidae, Veliidae, ',
         'Chrysomelidae, Curculionidae, Succineidae, Clausiliidae, Geoplanidae, etc. ',
         'These taxa are retained but marked as excluded.'))
)

# ---- Phase 2: Forced-level assignments ----
phase2_description <- paste0(
  'Taxa whose OTU level is <b>fixed regardless of specialist agreement</b>. ',
  'These taxa <b>skip Phases 3 and 4</b> &mdash; the Phase 4 metric-compliance ',
  'check is not applied to forced-level entries. Phase 2 acts as a <b>ceiling</b> ',
  'override (caps the OTU level).')

# Phase 2 forced groups: displayed as a grid of boxes
phase2_groups <- list(
  list(title = "2.1 &mdash; Entirely excluded groups",
       text  = paste0(
         'All families in excluded_taxa &rarr; OTU at group level:<br>',
         '&bull; Entirely excluded phyla &rarr; <b>phylum</b> (e.g., Porifera Gen. sp.)<br>',
         '&bull; Entirely excluded classes &rarr; <b>class</b> (e.g., Polychaeta Gen. sp.)<br>',
         '&bull; Entirely excluded orders &rarr; <b>order</b> (e.g., Stylommatophora Gen. sp.)'),
       color = "override"),
  list(title = "2.2 &mdash; Oligochaeta",
       text  = paste0(
         '<span style="color:#27500a"><b>NOT excluded from EQR</b></span><br>',
         '&bull; Naididae &rarr; <b>family</b> (Naididae Gen. sp.)<br>',
         '&bull; All other Oligochaeta &rarr; <b>subclass</b> (Oligochaeta Gen. sp.)<br>',
         '<span style="font-size:11px">Includes order-level entries: Crassiclitellata, Enchytraeida, Lumbriculida, Tubificida</span>'),
       color = "outcome"),
  list(title = "2.3 &mdash; Neuroptera",
       text  = paste0(
         '<span style="color:#27500a"><b>NOT excluded from EQR</b></span><br>',
         '&bull; All entries forced to <b>family</b><br>',
         '&bull; Osmylidae &rarr; Osmylidae Gen. sp.<br>',
         '&bull; Sisyridae &rarr; Sisyridae Gen. sp.<br>',
         '<span style="font-size:11px">Specialist agreement at order was too coarse</span>'),
       color = "outcome"),
  list(title = "2.4 &mdash; Stenostomidae",
       text  = paste0(
         'Excluded from EQR<br>',
         'Forced to <b>phylum</b> &rarr; Platyhelminthes Gen. sp.<br>',
         '<span style="font-size:11px">Order taxonomically unavailable</span>'),
       color = "override"),
  list(title = "2.5 &mdash; Individually excluded taxa",
       text  = paste0(
         'Excluded from EQR<br>',
         'Any taxon excluded but NOT in an entirely excluded group &rarr; <b>family</b> level<br>',
         '<span style="font-size:11px">Talitridae, non-Asellidae Isopoda, Neomysis/Praunus, Crangonidae/Palaemonidae, ',
         'Hemiptera excl. families, Lepidoptera excl. families, etc.</span>'),
       color = "override")
)

# ---- Phase 3 ----
phase3_description <- paste0(
  'For each <b>species-level</b> taxon not assigned in Phase 2, determine the ',
  '<b>100% specialist agreement level</b> &mdash; the <b>coarsest</b> level assigned by any specialist. ',
  'No specialist&rsquo;s assessment is discarded.')
phase3_outcome <- 'Assign OTU at the <b>100% agreement level</b>'

# ---- Higher-level entries ----
higher_description <- paste0(
  'Family, order, subclass, class, and phylum entries are structural fallback entries. ',
  'They accommodate damaged, juvenile, or poorly preserved specimens and are always assigned ',
  'at their own taxonomic level.')
higher_family  <- 'Family &rarr; <b>family</b><br><span style="font-weight:400;font-size:11px">e.g., Baetidae Gen. sp.</span>'
higher_order   <- 'Order &rarr; <b>order</b><br><span style="font-weight:400;font-size:11px">e.g., Ephemeroptera Gen. sp.</span>'
higher_other   <- 'Subclass / class / phylum &rarr; <b>respective level</b>'

truly_aquatic_text <- paste0(
  '<b>Truly aquatic rule (order+ entries):</b> Only truly aquatic groups keep order-and-above ',
  'entries for EQR calculations: Bivalvia (class), Plecoptera, Ephemeroptera, Odonata, ',
  'Megaloptera, Trichoptera (orders). All other groups&rsquo; order+ entries are excluded from EQR.')

subspecies_text <- paste0(
  '<b>Subspecies entries</b> (e.g., <i>Nemoura cinerea</i> subsp. <i>cinerea</i>) that are not present ',
  'in the Specialist_taxalist <b>inherit their OTU assignment from the parent species</b>. ',
  'This prevents subspecies from collapsing to a coarser fallback level when the parent species ',
  'has a known specialist agreement.')

genus_entries_text <- paste0(
  '<b>Genus entries</b> are metric-minimum-aware, capped at genus. The metric minimum is determined ',
  'by the genus&rsquo;s own taxonomy (group, family), NOT inherited from species-level BMWP exceptions:<br>',
  '&bull; If the genus has a <b>forced-level override</b> (Phase 2) &rarr; resolves to the forced level ',
  '(e.g., Talitridae genus &rarr; Talitridae Gen. sp.)<br>',
  '&bull; If the minimum is <b>genus or finer</b> &rarr; genus entry stays at <b>genus</b> level ',
  '(e.g., <i>Leuctra</i> sp. &mdash; Plecoptera min = genus)<br>',
  '&bull; If the minimum is <b>coarser than genus</b> &rarr; genus entry resolves to that coarser level ',
  '(e.g., <i>Baetis</i> &rarr; Baetidae Gen. sp. &mdash; Ephemeroptera min = family)<br>',
  '&bull; If <b>no metric group</b> &rarr; genus entry defaults to <b>genus</b> level<br>',
  '&bull; Genus entries in <b>entirely excluded groups</b> collapse to the group-level OTU')

# ---- Phase 4 ----
phase4_description <- paste0(
  'For each species-level taxon, verify that the OTU level from Phase 3 meets or exceeds the ',
  'minimum identification requirement for its group (see reference table below). If the taxon&rsquo;s ',
  'group has no specific metric requirement, this check is automatically passed.')
phase4_decision   <- "Does the assigned OTU level meet or exceed the minimum metric requirement?"
phase4_yes        <- 'OTU assignment is <b>confirmed</b>.'
phase4_override   <- paste0(
  '<b>Override:</b> adjust the OTU level to the <b>minimum level required</b> by the relevant metric. ',
  'Document the override with the reason.<br>',
  '<span style="font-size:12px;font-weight:400">(e.g., &ldquo;100% agreement = family; overridden to genus ',
  'because DSFI requires <i>Leuctra</i> at genus for IG1 placement&rdquo;)</span>')
phase4_override_note <- "Overrides should be rare. Frequent overrides indicate a gap between practitioner capability and metric requirements."

# ---- Final output ----
final_output_text <- paste0(
  '<b>Final OTL entry for each taxon:</b><br>',
  'Validated name &middot; OTU level &middot; OTU name (e.g., <i>Baetis</i> sp. / Baetidae Gen. sp.) ',
  '&middot; Taxonomic rank &middot; Override flag (if applicable) &middot; EQR exclusion flag (if applicable)<br>',
  '<span style="font-weight:400;font-size:12px">Hierarchical entries (both fine-resolution OTUs and coarser ',
  'fallback entries) coexist in the taxalist, accommodating damaged, juvenile, or poorly preserved specimens.</span>')


# ============================================================================
# 2. REFERENCE TABLE — metric requirements by group (edit rows here)
# ============================================================================
#
# Each row: Group, DSFI, BMWP, Other, Minimum
# Use HTML markup (<b>, <i>, <br>, etc.) freely within strings.

ref_intro <- paste0(
  'Requirements derived from DSFI, BMWP/ASPT, #DEP, and %EHP-%CrHi. These are <b>floors, ',
  'not ceilings</b> &mdash; if 100% specialist agreement yields a finer level, the finer level is kept.')

ref_table <- data.frame(
  Group = c(
    "Malacostraca", "Gastropoda", "Bivalvia", "Plecoptera", "Ephemeroptera",
    "Trichoptera", "Coleoptera", "Diptera", "Megaloptera", "Hirudinea",
    "Oligochaeta", "Hemiptera", "Odonata", "Turbellaria", "Lepidoptera",
    "Neuroptera"
  ),
  DSFI = c(
    '<i>Asellus</i>, <i>Gammarus</i> at <b>genus</b> (IG + diversity)',
    '<i>Ancylus</i>, <i>Lymnaea</i> at <b>genus</b> (diversity)',
    '<i>Sphaerium</i> at <b>genus</b> (diversity)',
    '13 genera at <b>genus</b> (IG1/IG2). Families: Leuctridae, Capniidae, Perlodidae, Perlidae, Chloroperlidae (IG1); Taeniopterygidae, Nemouridae (IG2).',
    '8 families: Ephemeridae (IG1), Ametropodidae, Ephemerellidae, Heptageniidae, Leptophlebiidae, Siphlonuridae (IG2), Caenidae (IG3), Baetidae (IG5).',
    '<u>Case-bearing</u>: Beraeidae, Brachycentridae, Goeridae, Glossosomatidae, Hydroptilidae, Leptoceridae, Lepidostomatidae, Limnephilidae, Molannidae, Odontoceridae, Phryganeidae, Sericostomatidae. <u>Caseless</u>: Ecnomidae, Hydropsychidae, Philopotamidae, Polycentropodidae, Psychomyiidae, Rhyacophilidae.',
    '<i>Elmis</i>, <i>Limnius</i>, <i>Elodes</i> at <b>genus</b> (IG1/IG2 + diversity)',
    '<i>Chironomus</i> at <b>genus</b>; Eristalinae at <b>subfamily</b>; Simuliidae, Psychodidae, Chironomidae at <b>family</b>',
    '<i>Sialis</i> at <b>genus</b> (IG4 + neg. diversity)',
    '<i>Erpobdella</i>, <i>Helobdella</i> at <b>genus</b> (neg. diversity)',
    'Naididae at <b>family</b> (IG6; formerly Tubificidae); Oligochaeta &ge;100 individuals as precluder',
    '&mdash;',
    '&mdash;',
    'Tricladida at <b>order</b> (pos. diversity)',
    '&mdash;',
    '&mdash;'
  ),
  BMWP = c(
    'Score 8: Astacidae [f]. Score 6: Gammaridae [f], Corophiidae [f], Crangonyctidae [f], Niphargidae [f], Pontogammaridae [f]. Score 3: Asellidae [f].<br><span style="font-size:10.5px;color:#791f1f">Excluded taxa (Talitridae, non-Asellidae Isopoda, Neomysis/Praunus, Crangonidae/Palaemonidae) &rarr; family level, excl. EQR.</span>',
    'Score 6: Viviparidae [f], Neritidae [f], Ancylidae [f], <i>Ancylus</i> [g], Acroloxidae [f]. Score 5: Hydrobiidae [f]. Score 3: Lymnaeidae [f], Physidae [f], Planorbidae [f], Valvatidae [f], Bithyniidae [f].',
    'Score 6: Unionidae [f]. Score 3: Sphaeriidae [f].',
    'Score 10: Taeniopterygidae [f], Leuctridae [f], Perlodidae [f], Perlidae [f], Chloroperlidae [f], Capniidae [f]. Score 7: Nemouridae [f].',
    'Score 10: Siphlonuridae [f], Heptageniidae [f], Leptophlebiidae [f], Ephemerellidae [f], Potamanthidae [f], Ephemeridae [f]. Score 7: Caenidae [f]. Score 4: Baetidae [f].',
    'Score 10: Beraeidae [f], Brachycentridae [f], Goeridae [f], Lepidostomatidae [f], Leptoceridae [f], Molannidae [f], Odontoceridae [f], Phryganeidae [f], Sericostomatidae [f]. Score 8: Ecnomidae [f], Philopotamidae [f], Psychomyiidae [f]. Score 7: Apataniidae [f], Glossosomatidae [f], Limnephilidae [f], Polycentropodidae [f], Rhyacophilidae [f]. Score 6: Hydroptilidae [f]. Score 5: Hydropsychidae [f].',
    'Score 5: Haliplidae [f], Hygrobiidae [f], Noteridae [f], Dytiscidae [f], Gyrinidae [f], Hydrophilidae [f], Hydraenidae [f], Hydrochidae [f], Helophoridae [f], Georissidae [f], Clambidae [f], Scirtidae [f], Helodidae [f], Dryopidae [f], Elmidae [f], Chrysomelidae [f], Curculionidae [f].',
    'Score 5: Tipulidae [f], Simuliidae [f], Limoniidae [f], Pediciidae [f], Cylindrotomidae [f]. Score 2: Chironomidae [f].',
    'Score 4: Sialidae [f].',
    'Score 4: Piscicolidae [f]. Score 3: Glossiphoniidae [f], Hirudinidae [f], Erpobdellidae [f].',
    'Score 1: Oligochaeta [sc], Naididae [f].',
    'Score 10: Aphelocheiridae [f]. Score 5: Nepidae [f], Naucoridae [f], Notonectidae [f], Pleidae [f], Corixidae [f], Gerridae [f], Hydrometridae [f], Mesoveliidae [f].',
    'Score 8: Lestidae [f], Calopterygidae [f], Gomphidae [f], Cordulegastridae [f], Aeshnidae [f], Corduliidae [f], Libellulidae [f]. Score 6: Coenagrionidae [f], Platycnemididae [f].',
    'Score 5: Planariidae [f], Dendrocoelidae [f], Dugesiidae [f].',
    '&mdash;',
    '&mdash;'
  ),
  Other = c(
    "%CrHi pools Malacostraca", "&mdash;", "&mdash;", "#DEP: species", "#DEP: species",
    "&mdash;", "&mdash;", "#DEP: family", "&mdash;", "%CrHi pools Hirudinea",
    "&mdash;", "%EHP pools Hemiptera. Excluded families &rarr; family-level OTU.",
    "&mdash;", "&mdash;", "&mdash;",
    'Osmylidae and Sisyridae (aquatic). Forced to family level.'
  ),
  Minimum = c(
    '<b>Genus</b> for <i>Asellus</i>, <i>Gammarus</i>; <b>family</b> for others',
    '<b>Genus</b> for <i>Ancylus</i>, <i>Lymnaea</i>; <b>family</b> for others',
    '<b>Genus</b> for <i>Sphaerium</i>; <b>family</b> for others',
    'At least <b>genus</b>',
    'At least <b>family</b>',
    'At least <b>family</b>',
    '<b>Genus</b> for <i>Elmis</i>, <i>Limnius</i>, <i>Elodes</i>; <b>family</b> for others',
    '<b>Family</b>; <b>genus</b> for <i>Chironomus</i>; <b>subfamily</b> for Eristalinae',
    '<b>Genus</b> for <i>Sialis</i>',
    '<b>Genus</b> for <i>Erpobdella</i>, <i>Helobdella</i>; <b>family</b> for others',
    'Naididae at <b>family</b>; others at <b>subclass</b>',
    'At least <b>family</b>',
    'At least <b>family</b>',
    'At least <b>family</b>',
    'At least <b>family</b>',
    '<b>Family</b> (forced ceiling)'
  ),
  stringsAsFactors = FALSE
)


# ============================================================================
# 3. CSS + HELPER FUNCTIONS (rarely need editing)
# ============================================================================

css <- '
* { margin: 0; padding: 0; box-sizing: border-box; }
body { font-family: "Segoe UI", system-ui, sans-serif; background: #f8f7f4; color: #2c2c2a; padding: 32px 48px; }
h1 { font-size: 20px; font-weight: 600; margin-bottom: 4px; text-align: center; }
h2 { font-size: 14px; font-weight: 400; color: #5f5e5a; margin-bottom: 24px; text-align: center; }
.tree { display: flex; flex-direction: column; align-items: center; gap: 0; }
.box { border-radius: 8px; padding: 12px 20px; text-align: center; font-size: 13px; line-height: 1.55; border: 1.5px solid; }
.box b { font-weight: 600; }
.header { background: #2c2c2a; color: #f1efe8; border-color: #2c2c2a; font-weight: 600; font-size: 13.5px; }
.process { background: #e6f1fb; border-color: #85b7eb; color: #0c447c; }
.decision { background: #faeeda; border-color: #ef9f27; color: #633806; }
.outcome { background: #eaf3de; border-color: #97c459; color: #27500a; font-weight: 600; }
.override { background: #fcebeb; border-color: #f09595; color: #791f1f; }
.note { background: #eeedfe; border-color: #afa9ec; color: #3c3489; font-size: 12px; }
.min-req { background: #fff8e6; border: 1.5px dashed #d4a017; color: #633806; font-size: 12px; text-align: left; }
.ad { width: 2px; height: 22px; background: #888780; margin: 0 auto; position: relative; }
.ad::after { content:""; position:absolute; bottom:-5px; left:-4px; border-left:5px solid transparent; border-right:5px solid transparent; border-top:6px solid #888780; }
.section { margin: 40px 0 16px; padding: 10px 18px; background: #f1efe8; border-left: 4px solid #888780; font-size: 14px; font-weight: 600; color: #444441; width: 100%; max-width: 1100px; }
.legend { display: flex; flex-wrap: wrap; gap: 16px; justify-content: center; margin: 16px 0 28px; font-size: 12px; }
.legend-item { display: flex; align-items: center; gap: 6px; }
.ls { width: 18px; height: 18px; border-radius: 4px; border: 1px solid rgba(0,0,0,0.15); }
.ls.dashed { border: 2px dashed #d4a017; background: #fff8e6; }
table.branches { border-collapse: separate; border-spacing: 24px 0; margin: 0 auto; }
table.branches td { vertical-align: top; padding: 0; }
table.branches2 { border-collapse: separate; border-spacing: 16px 0; margin: 0 auto; }
table.branches2 td { vertical-align: top; padding: 0; }
table.branches3 { border-collapse: separate; border-spacing: 12px 0; margin: 0 auto; }
table.branches3 td { vertical-align: top; padding: 0; }
.branch-cell { display: flex; flex-direction: column; align-items: center; gap: 0; }
.bl { font-size: 12px; font-weight: 700; color: #5f5e5a; text-transform: uppercase; letter-spacing: 0.5px; margin: 8px 0; text-align: center; }
.footnote { max-width: 1100px; margin: 28px auto 0; font-size: 12.5px; color: #5f5e5a; line-height: 1.7; }
.footnote b { color: #2c2c2a; }
.sp { height: 10px; }
.center { display: flex; flex-direction: column; align-items: center; }
.ref-table { width: 100%; font-size: 11.5px; border-collapse: collapse; }
.ref-table th { padding: 6px 6px; text-align: left; border-bottom: 2px solid #d4a017; font-weight: 700; }
.ref-table td { padding: 5px 6px; vertical-align: top; border-bottom: 1px solid #e8d8a0; }
.ref-table tr:last-child td { border-bottom: none; }
.ref-table td:first-child { font-weight: 600; width: 110px; }
'

# --- Helper functions ---

box <- function(type, text, width = 900, font_size = NULL, padding = NULL) {
  style <- paste0("width:", width, "px")
  if (!is.null(font_size)) style <- paste0(style, ";font-size:", font_size, "px")
  if (!is.null(padding))   style <- paste0(style, ";padding:", padding)
  sprintf('  <div class="box %s" style="%s">%s</div>', type, style, text)
}

arrow  <- function() '  <div class="ad"></div>'
spacer <- function() '  <div class="sp"></div>'

section <- function(title) {
  sprintf('<div class="section">%s</div>', title)
}

branch_label <- function(text, font_size = NULL) {
  fs <- if (!is.null(font_size)) sprintf(' style="font-size:%dpx"', font_size) else ""
  sprintf('<div class="bl"%s>%s</div>', fs, text)
}

center_open  <- function(margin_top = NULL) {
  mt <- if (!is.null(margin_top)) sprintf(' style="margin-top:%dpx"', margin_top) else ""
  sprintf('<div class="center"%s>', mt)
}
center_close <- function() '</div>'

branch_cell_open  <- function() '<div class="branch-cell">'
branch_cell_close <- function() '</div>'


# ============================================================================
# 4. BUILD SECTIONS
# ============================================================================

build_html <- function() {

  h <- c()  # accumulator
  add <- function(...) h <<- c(h, ...)

  # --- Head ---
  add('<!DOCTYPE html>',
      '<html lang="en">',
      '<head>',
      '<meta charset="UTF-8">',
      '<meta name="viewport" content="width=device-width, initial-scale=1.0">',
      sprintf('<title>%s</title>', gsub("<[^>]+>", "", subtitle)),
      '<style>', css, '</style>',
      '</head>',
      '<body>', '')

  # --- Title ---
  add(sprintf('<h1>%s</h1>', title),
      sprintf('<h2>%s</h2>', subtitle), '')

  # --- Legend ---
  add('<div class="legend">',
      '  <div class="legend-item"><div class="ls" style="background:#2c2c2a"></div> Preparatory step</div>',
      '  <div class="legend-item"><div class="ls" style="background:#e6f1fb"></div> Process / action</div>',
      '  <div class="legend-item"><div class="ls" style="background:#faeeda"></div> Decision</div>',
      '  <div class="legend-item"><div class="ls" style="background:#eaf3de"></div> OTU assignment</div>',
      '  <div class="legend-item"><div class="ls" style="background:#fcebeb"></div> Metric compliance override</div>',
      '  <div class="legend-item"><div class="ls" style="background:#eeedfe"></div> Annotation</div>',
      '  <div class="legend-item"><div class="ls dashed"></div> Minimum metric requirement</div>',
      '</div>', '')

  add('<div class="tree">', '')

  # ======== PHASE 1 ========
  add(section("Phase 1 &mdash; Preparation"), '')
  add(center_open())
  for (i in seq_along(phase1_steps)) {
    s <- phase1_steps[[i]]
    add(box(s$type, s$text))
    add(arrow())
  }
  add(center_close(), '')

  # ======== PHASE 2: Forced-level assignments ========
  add(section("Phase 2 &mdash; Forced-level assignments"), '')

  add(center_open(), box("process", phase2_description), spacer(), center_close(), '')

  # First row: 3 boxes (2.1, 2.2, 2.3)
  add('<table class="branches">', '<tr>')
  for (i in 1:3) {
    fg <- phase2_groups[[i]]
    txt <- sprintf('<b>%s</b><br><br>%s', fg$title, fg$text)
    add(sprintf('<td style="width:340px">%s%s%s</td>',
                branch_cell_open(), box(fg$color, txt, width = 320, font_size = 12), branch_cell_close()))
  }
  add('</tr>', '</table>', '')

  # Second row: 2 boxes (2.4, 2.5)
  add('<table class="branches">', '<tr>')
  for (i in 4:5) {
    fg <- phase2_groups[[i]]
    txt <- sprintf('<b>%s</b><br><br>%s', fg$title, fg$text)
    add(sprintf('<td style="width:340px">%s%s%s</td>',
                branch_cell_open(), box(fg$color, txt, width = 320, font_size = 12), branch_cell_close()))
  }
  add('</tr>', '</table>', '')

  # ======== PHASE 3 ========
  add(section("Phase 3 &mdash; 100% specialist agreement (species-level entries only)"), '')
  add(center_open(),
      box("process", phase3_description),
      arrow(),
      box("outcome", phase3_outcome, width = 500),
      center_close(), '')

  # ======== HIGHER-LEVEL ENTRIES ========
  add(section("Higher-level entries &mdash; genus, family, and above"), '')
  add(center_open(), box("process", higher_description), spacer(), center_close(), '')

  add('<table class="branches">', '<tr>')
  add(sprintf('<td style="width:280px">%s%s%s</td>',
              branch_cell_open(), box("outcome", higher_family, width = 260, font_size = 12), branch_cell_close()))
  add(sprintf('<td style="width:280px">%s%s%s</td>',
              branch_cell_open(), box("outcome", higher_order, width = 260, font_size = 12), branch_cell_close()))
  add(sprintf('<td style="width:280px">%s%s%s</td>',
              branch_cell_open(), box("outcome", higher_other, width = 260, font_size = 12), branch_cell_close()))
  add('</tr>', '</table>', '')

  add(center_open(margin_top = 16), box("process", truly_aquatic_text), center_close(), '')
  add(center_open(margin_top = 16), box("process", genus_entries_text), center_close(), '')
  add(center_open(margin_top = 16), box("note", subspecies_text), center_close(), '')

  # ======== PHASE 4 ========
  add(section("Phase 4 &mdash; Metric compliance check (species-level entries only, applied after Phase 3)"), '')
  add(center_open(),
      box("process", phase4_description),
      arrow(),
      box("decision", phase4_decision, width = 600),
      center_close(), '')

  add('<table class="branches">', '<tr>')
  add('<td style="width:360px">', branch_cell_open(),
      branch_label("Yes"),
      box("outcome", phase4_yes, width = 340),
      branch_cell_close(), '</td>')
  add('<td style="width:520px">', branch_cell_open(),
      branch_label("No"),
      box("override", phase4_override, width = 500),
      spacer(),
      box("note", phase4_override_note, width = 500, font_size = 11.5),
      branch_cell_close(), '</td>')
  add('</tr>', '</table>', '')

  # ======== FINAL OUTPUT ========
  add(section("Final output"), '')
  add(center_open(),
      box("outcome", final_output_text, font_size = 13.5),
      center_close(), '')

  # ======== REFERENCE TABLE ========
  add(section("Reference table &mdash; Minimum metric requirements by group"), '')

  # Build table rows
  rows <- ""
  for (i in seq_len(nrow(ref_table))) {
    rows <- paste0(rows, sprintf(
      '      <tr>\n        <td>%s</td>\n        <td>%s</td>\n        <td>%s</td>\n        <td>%s</td>\n        <td>%s</td>\n      </tr>\n',
      ref_table$Group[i], ref_table$DSFI[i], ref_table$BMWP[i],
      ref_table$Other[i], ref_table$Minimum[i]))
  }

  add(center_open(),
      sprintf('  <div class="box min-req" style="width:1100px; line-height:1.6">\n    %s<br><br>', ref_intro),
      '    <table class="ref-table">',
      '      <tr>',
      '        <th style="width:110px">Group</th>',
      '        <th style="width:260px">DSFI requirement</th>',
      '        <th style="width:340px">BMWP/ASPT (all scored taxa)</th>',
      '        <th style="width:150px">Other metrics</th>',
      '        <th style="width:170px">Resulting minimum</th>',
      '      </tr>',
      rows,
      '    </table>',
      '  </div>',
      center_close(), '')

  add('</div>', '')  # close .tree

  add('</body>', '</html>')

  return(h)
}


# ============================================================================
# 5. OUTPUT — generate and write
# ============================================================================

html_lines <- build_html()
writeLines(html_lines, output_file)
cat(sprintf("Decision tree HTML written to: %s\n", output_file))
