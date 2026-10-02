# ============================================================================
# OTL Decision Tree — flowchart generator (v19)
# ============================================================================
#
# Draws the OTU decision tree (Supplement 2) as a traversable flowchart from
# the machine-readable specification
#     Inputs/OTL_decision_tree.yaml
# To change the tree (text, boxes, arrows, examples, reference table), edit
# the YAML file and re-run this script. Nothing about the tree's content is
# hard-coded here.
#
# Branch counts are computed from the outputs of script 2
# (Outputs/2_OTU_assignments_v23.xlsx) and script 11
# (Outputs/11_feasibility_gap_check.xlsx), so run those first. The script
# stops with an error if:
#   - the YAML refers to a node, node type or count key that does not exist;
#   - the Phase 2-4 branch counts do not add up to the number of species
#     evaluated by the specialists, or disagree with script 2's own flags;
#   - a worked example does not match what script 2 actually assigned.
#
# Outputs: Outputs/1_OTL_decision_tree_<version>.html and .pdf
#
# Changes from v18:
#   - Tree content moved out of the script into the YAML specification.
#   - Drawn as a flowchart (SVG) with explicit yes/no branches, instead of
#     a sequence of descriptive boxes; seven lookalike box types reduced to
#     visually distinct shapes and fills.
#   - Phase 3 is now the metric-required target level and Phase 4 the
#     specialist feasibility check (finer / equal / coarser), with the
#     feasibility gap flagged (Reviewer 1).
#   - The "specimen not identifiable to the required level" branch
#     (hierarchical fallback + truly aquatic rule) is drawn explicitly.
#   - Higher-level entries are a connected branch of the flowchart.
#   - Branch counts and worked examples, both checked against script 2.
#   - Reference table: Lepidoptera footnote added; stray "&mdash." fixed.
# ============================================================================

library(yaml)
library(readxl)
library(dplyr)

spec_file       <- "Inputs/OTL_decision_tree.yaml"
otu_output_file <- "Outputs/2_OTU_assignments_v23.xlsx"
gap_output_file <- "Outputs/11_feasibility_gap_check.xlsx"

spec <- yaml::read_yaml(spec_file)
output_file <- sprintf("Outputs/1_OTL_decision_tree_%s.html", spec$version)

LEVEL_RANK <- c(s = 1, g = 2, sf = 3, f = 4, o = 5, sc = 6, c = 7, p = 8)

# ============================================================================
# 1. BRANCH COUNTS (from script 2 and script 11 outputs)
# ============================================================================

trace <- read_excel(otu_output_file, sheet = "decision_tree_trace", guess_max = 10000)

# Classify every specialist-evaluated species into the node it ends in,
# using the same rule precedence as script 2 (Stenostomidae is forced to
# phylum before the entirely-excluded-group rule applies).
trace <- trace %>%
  mutate(
    is_forced = !is.na(forced_level),
    in_p34    = is.na(excl_group_level) & !is_forced,
    node = case_when(
      !is.na(family) & family == "STENOSTOMIDAE"                   ~ "o_steno",
      !is.na(excl_group_level)                                      ~ "o_exgroup",
      is_forced & !is.na(family) & family == "NAIDIDAE"              ~ "o_naid",
      is_forced & !is.na(subclass) & subclass == "OLIGOCHAETA"       ~ "o_oligo",
      is_forced & !is.na(order) & order == "NEUROPTERA"              ~ "o_neur",
      is_forced                                                      ~ "o_indiv",
      LEVEL_RANK[agreement_level] <  LEVEL_RANK[min_level]           ~ "o_finer",
      LEVEL_RANK[agreement_level] == LEVEL_RANK[min_level]           ~ "o_equal",
      LEVEL_RANK[agreement_level] >  LEVEL_RANK[min_level]           ~ "o_gap"
    )
  )
if (any(is.na(trace$node))) {
  stop("Species not classified into a decision-tree branch: ",
       paste(trace$validated_name[is.na(trace$node)], collapse = ", "))
}
n_node <- function(id) sum(trace$node == id)

# Higher-level entries: rows of the OTL sheet only (script 2's gap-fill rows
# have no original name and are not part of the published taxalist).
otl_out <- read_excel(otu_output_file, sheet = "OTL_final_with_OTU", guess_max = 10000) %>%
  filter(!is.na(original_name)) %>%
  distinct(validated_name, .keep_all = TRUE)
higher    <- otl_out %>% filter(entry_type != "species")
orderplus <- higher %>% filter(entry_type %in% c("order", "subclass", "class", "phylum"))

use <- read_excel(gap_output_file, sheet = "reached_by_group")

counts <- list(
  n_entries                = nrow(otl_out),
  n_species_entries        = sum(otl_out$entry_type == "species"),
  n_species_assessed       = nrow(trace),
  n_species_unassessed     = sum(otl_out$entry_type == "species" &
                                   !otl_out$validated_name %in% trace$validated_name),
  p2_stenostomidae         = n_node("o_steno"),
  p2_excluded_group        = n_node("o_exgroup"),
  p2_naididae              = n_node("o_naid"),
  p2_other_oligochaeta     = n_node("o_oligo"),
  p2_neuroptera            = n_node("o_neur"),
  p2_individually_excluded = n_node("o_indiv"),
  p2_total                 = sum(!trace$in_p34),
  p34_total                = sum(trace$in_p34),
  p4_finer                 = n_node("o_finer"),
  p4_equal                 = n_node("o_equal"),
  p4_gap                   = n_node("o_gap"),
  gap_total                = sum(trace$specialist_gap),
  h_total                  = nrow(higher),
  h_genus                  = sum(higher$entry_type == "genus"),
  h_family                 = sum(higher$entry_type == "family"),
  h_orderplus_retained     = sum(!orderplus$excluded_from_EQR_script),
  h_orderplus_excluded     = sum(orderplus$excluded_from_EQR_script),
  use_records              = sum(use$records),
  use_reached              = sum(use$reached_minimum),
  use_reached_pct          = round(100 * sum(use$reached_minimum) / sum(use$records), 1),
  use_below                = sum(use$below_minimum),
  use_below_retained       = sum(use$below_retained_EQR),
  use_below_excluded       = sum(use$below_excluded_EQR)
)

# ---- Consistency checks (figure must agree with script 2) -----------------
check <- function(ok, msg) if (!isTRUE(ok)) stop("Consistency check failed: ", msg)
with(counts, {
  check(p2_total + p34_total == n_species_assessed,
        "Phase 2 + Phases 3-4 != species evaluated")
  check(p2_stenostomidae + p2_excluded_group + p2_naididae + p2_other_oligochaeta +
          p2_neuroptera + p2_individually_excluded == p2_total,
        "Phase 2 branches do not add up")
  check(p4_finer + p4_equal + p4_gap == p34_total, "Phase 4 branches do not add up")
  check(p4_gap == sum(trace$override), "Phase 4 gap != script 2 metric overrides")
  check(h_genus + h_family + h_orderplus_retained + h_orderplus_excluded == h_total,
        "higher-level branches do not add up")
  check(n_species_entries + h_total == n_entries, "species + higher-level != all entries")
  check(use_below_retained + use_below_excluded == use_below,
        "fallback branches do not add up")
})
check(all(trace$excluded_from_EQR[trace$node %in% c("o_steno", "o_exgroup", "o_indiv")]),
      "an excluded Phase 2 branch contains taxa retained for EQR")
check(!any(trace$excluded_from_EQR[trace$node %in% c("o_naid", "o_oligo", "o_neur")]),
      "a retained Phase 2 branch contains taxa excluded from EQR")
check(all(trace$specialist_gap[trace$node == "o_gap"]) &&
        !any(trace$specialist_gap[trace$node %in% c("o_finer", "o_equal")]),
      "specialist_gap flag disagrees with the Phase 4 branches")

fmt_count <- function(x) {
  if (is.numeric(x) && x == round(x)) format(x, big.mark = ",") else as.character(x)
}

# Replace {key} placeholders with counts; unknown keys are an error.
fill <- function(text) {
  if (is.null(text)) return("")
  keys <- regmatches(text, gregexpr("\\{[a-z0-9_]+\\}", text))[[1]]
  for (k in unique(keys)) {
    key <- gsub("[{}]", "", k)
    if (is.null(counts[[key]])) stop("Unknown count key in YAML: ", k)
    text <- gsub(k, fmt_count(counts[[key]]), text, fixed = TRUE)
  }
  text
}

# ============================================================================
# 2. VALIDATE THE SPECIFICATION
# ============================================================================

node_types <- c("start", "process", "decision", "otu_retained", "otu_excluded",
                "otu_gap", "note")
nodes <- bind_rows(lapply(spec$nodes, function(n) {
  tibble(id = n$id, type = n$type, col = n$col, row = n$row,
         w = if (is.null(n$w)) NA_real_ else n$w,
         h = if (is.null(n$h)) NA_real_ else n$h,
         text = fill(n$text))
}))
edges <- bind_rows(lapply(spec$edges, function(e) {
  tibble(from = e$from, to = e$to, from_side = e$from_side, to_side = e$to_side,
         label = if (is.null(e$label)) "" else fill(e$label))
}))

check(!any(duplicated(nodes$id)), "duplicate node ids")
check(all(nodes$type %in% node_types),
      paste("unknown node type:", paste(setdiff(nodes$type, node_types), collapse = ", ")))
check(all(c(edges$from, edges$to) %in% nodes$id),
      paste("edge refers to unknown node:",
            paste(setdiff(c(edges$from, edges$to), nodes$id), collapse = ", ")))
check(all(c(edges$from_side, edges$to_side) %in% c("top", "bottom", "left", "right")),
      "unknown edge side")
unreached <- nodes$id[!nodes$id %in% edges$to & !nodes$type %in% c("note") & nodes$id != "start"]
check(length(unreached) == 0, paste("nodes without an incoming arrow:", paste(unreached, collapse = ", ")))
check(!any(duplicated(paste(nodes$col, nodes$row))), "two nodes share a grid cell")

# ---- Worked examples: must match script 2 ----------------------------------
hier <- read_excel(otu_output_file, sheet = "hierarchical_OTL", guess_max = 10000)
for (ex in spec$examples) {
  check(ex$outcome %in% nodes$id, paste("example outcome is not a node:", ex$outcome))
  if (ex$check == "trace") {
    r <- trace[trace$validated_name == ex$lookup, ]
    check(nrow(r) == 1, paste("example not found in decision trace:", ex$lookup))
    got_spec <- tolower(trimws(unlist(r[, c("specialist1", "specialist2",
                                              "specialist3", "specialist4")])))
    check(identical(unname(got_spec), unlist(ex$specialists)),
          paste("specialists differ for", ex$lookup))
    check(r$agreement_level == ex$agreement, paste("agreement level differs for", ex$lookup))
    check(r$min_level == ex$target, paste("target level differs for", ex$lookup))
    check(r$OTU_name == ex$otu, paste("OTU differs for", ex$lookup))
    check(r$node == ex$outcome, paste("example ends in", r$node, "not", ex$outcome, "for", ex$lookup))
  } else if (ex$check == "hierarchy") {
    r <- hier[hier$OTU_name == ex$otu, ]
    check(nrow(r) > 0, paste("example OTU not in hierarchical OTL:", ex$otu))
    check(all(r$excluded_from_EQR == ex$excluded), paste("exclusion flag differs for", ex$otu))
  } else {
    stop("Unknown example check type: ", ex$check)
  }
}
cat(sprintf("Specification OK: %d nodes, %d edges, %d worked examples checked against script 2\n",
            nrow(nodes), nrow(edges), length(spec$examples)))

# ============================================================================
# 3. LAYOUT AND DRAWING (SVG)
# ============================================================================

COL_W <- 310; ROW_H <- 156; X0 <- 175; Y0 <- 60
default_w <- c(start = 250, process = 270, decision = 240, otu_retained = 250,
               otu_excluded = 250, otu_gap = 250, note = 250)
default_h <- c(start = 54, process = 76, decision = 104, otu_retained = 66,
               otu_excluded = 66, otu_gap = 66, note = 66)

nodes <- nodes %>%
  mutate(w = ifelse(is.na(w), default_w[type], w),
         h = ifelse(is.na(h), default_h[type], h),
         x = X0 + col * COL_W,
         y = Y0 + row * ROW_H)

svg_w <- X0 + max(nodes$col) * COL_W + COL_W / 2 + 20
svg_h <- Y0 + max(nodes$row) * ROW_H + ROW_H / 2 + 10

anchor <- function(n, side) {
  switch(side,
         top    = c(n$x, n$y - n$h / 2),
         bottom = c(n$x, n$y + n$h / 2),
         left   = c(n$x - n$w / 2, n$y),
         right  = c(n$x + n$w / 2, n$y))
}

route <- function(a, b, from_side, to_side) {
  vert_from <- from_side %in% c("top", "bottom")
  vert_to   <- to_side %in% c("top", "bottom")
  if (vert_from && vert_to) {
    if (abs(a[1] - b[1]) < 1) return(sprintf("M%.1f %.1f V%.1f", a[1], a[2], b[2]))
    my <- (a[2] + b[2]) / 2
    return(sprintf("M%.1f %.1f V%.1f H%.1f V%.1f", a[1], a[2], my, b[1], b[2]))
  }
  if (!vert_from && vert_to)  return(sprintf("M%.1f %.1f H%.1f V%.1f", a[1], a[2], b[1], b[2]))
  if (vert_from && !vert_to)  return(sprintf("M%.1f %.1f V%.1f H%.1f", a[1], a[2], b[2], b[1]))
  if (abs(a[2] - b[2]) < 1) return(sprintf("M%.1f %.1f H%.1f", a[1], a[2], b[1]))
  mx <- (a[1] + b[1]) / 2
  sprintf("M%.1f %.1f H%.1f V%.1f H%.1f", a[1], a[2], mx, b[2], b[1])
}

node_shape <- function(n) {
  x <- n$x; y <- n$y; w <- n$w; h <- n$h
  if (n$type == "decision") {
    return(sprintf('<polygon class="shape decision" points="%.1f,%.1f %.1f,%.1f %.1f,%.1f %.1f,%.1f"/>',
                   x, y - h / 2, x + w / 2, y, x, y + h / 2, x - w / 2, y))
  }
  rx <- switch(n$type, start = h / 2, process = 4, note = 2, 10)
  sprintf('<rect class="shape %s" x="%.1f" y="%.1f" width="%.1f" height="%.1f" rx="%.1f"/>',
          n$type, x - w / 2, y - h / 2, w, h, rx)
}

node_text <- function(n) {
  # Decisions: text inside the diamond's inscribed rectangle.
  f  <- if (n$type == "decision") 0.66 else 1
  tw <- n$w * f - 12; th <- n$h * f - 6
  sprintf(paste0('<foreignObject x="%.1f" y="%.1f" width="%.1f" height="%.1f">',
                 '<div xmlns="http://www.w3.org/1999/xhtml" class="nt %s"><div>%s</div></div></foreignObject>'),
          n$x - tw / 2, n$y - th / 2, tw, th, n$type, n$text)
}

edge_svg <- function(e) {
  fn <- nodes[nodes$id == e$from, ]; tn <- nodes[nodes$id == e$to, ]
  a <- anchor(fn, e$from_side); b <- anchor(tn, e$to_side)
  path <- sprintf('<path class="edge" d="%s" marker-end="url(#arrow)"/>',
                  route(a, b, e$from_side, e$to_side))
  if (e$label == "") return(path)
  lx <- switch(e$from_side, right = a[1] + 8, left = a[1] - 8, a[1] + 8)
  ly <- switch(e$from_side, bottom = a[2] + 15, top = a[2] - 6, a[2] - 7)
  anchor_txt <- if (e$from_side == "left") "end" else "start"
  paste0(path, sprintf('<text class="elabel" x="%.1f" y="%.1f" text-anchor="%s">%s</text>',
                       lx, ly, anchor_txt, e$label))
}

# Phase bands: drawn around their member nodes, with room at the top for
# the label (top-right corner, clear of the arrows entering on the spine).
BAND_PAD <- 14; BAND_HEAD <- 27
bands <- bind_rows(lapply(spec$phases, function(p) {
  ids <- unlist(p$nodes)
  if (!all(ids %in% nodes$id)) {
    stop("Phase band refers to unknown node: ", paste(setdiff(ids, nodes$id), collapse = ", "))
  }
  m <- nodes[nodes$id %in% ids, ]
  tibble(label = p$label,
         x1 = min(m$x - m$w / 2) - BAND_PAD, x2 = max(m$x + m$w / 2) + BAND_PAD,
         y1 = min(m$y - m$h / 2) - BAND_HEAD, y2 = max(m$y + m$h / 2) + BAND_PAD)
}))
for (i in seq_len(nrow(bands))) for (j in seq_len(nrow(bands))) if (i < j) {
  a <- bands[i, ]; b <- bands[j, ]
  if (a$x1 < b$x2 && b$x1 < a$x2 && a$y1 < b$y2 && b$y1 < a$y2) {
    stop("Phase bands overlap: ", a$label, " / ", b$label)
  }
}

band_svg <- function(b) {
  paste0(sprintf('<rect class="band" x="%.1f" y="%.1f" width="%.1f" height="%.1f" rx="10"/>',
                 b$x1, b$y1, b$x2 - b$x1, b$y2 - b$y1),
         sprintf('<text class="blabel" x="%.1f" y="%.1f" text-anchor="end">%s</text>',
                 b$x2 - 12, b$y1 + 18, b$label))
}

chart_svg <- c(
  sprintf('<svg class="chart" xmlns="http://www.w3.org/2000/svg" width="%.0f" height="%.0f" viewBox="0 0 %.0f %.0f">',
          svg_w, svg_h, svg_w, svg_h),
  '<defs><marker id="arrow" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse"><path d="M0 0 L10 5 L0 10 z" fill="#5f5d56"/></marker></defs>',
  vapply(seq_len(nrow(bands)), function(i) band_svg(bands[i, ]), character(1)),
  vapply(seq_len(nrow(edges)), function(i) edge_svg(edges[i, ]), character(1)),
  vapply(seq_len(nrow(nodes)), function(i) paste0(node_shape(nodes[i, ]), node_text(nodes[i, ])), character(1)),
  '</svg>'
)

legend_html <- paste0(
  '<div class="legend">',
  paste(vapply(spec$legend, function(l) {
    shape <- if (l$type == "decision") {
      '<polygon class="shape decision" points="17,2 32,12 17,22 2,12"/>'
    } else {
      rx <- switch(l$type, start = 10, process = 3, note = 1, 6)
      sprintf('<rect class="shape %s" x="2" y="3" width="30" height="18" rx="%d"/>', l$type, rx)
    }
    sprintf('<span class="li"><svg width="34" height="24">%s</svg>%s</span>', shape, fill(l$label))
  }, character(1)), collapse = ""),
  '</div>')

# ============================================================================
# 4. TABLES (worked examples, reference table)
# ============================================================================

examples_html <- paste0(
  '<h3>Worked examples</h3>',
  '<table class="tbl"><thead><tr><th>Taxon</th><th>Group</th><th>Specialists 1&ndash;4</th>',
  '<th>Path through the tree</th><th>OTU</th></tr></thead><tbody>',
  paste(vapply(spec$examples, function(ex) {
    sp <- if (is.null(ex$specialists)) "&mdash;" else paste(ex$specialists, collapse = " &middot; ")
    sprintf('<tr><td>%s</td><td>%s</td><td class="c">%s</td><td>%s</td><td>%s</td></tr>',
            ex$taxon, ex$group, sp, ex$path, ex$otu)
  }, character(1)), collapse = ""),
  '</tbody></table>',
  '<p class="fn">Specialist levels: s species, g genus, f family, o order, sc subclass. ',
  'The agreement level is the coarsest level reported by any specialist. ',
  'Each example is verified against the OTU assigned by script 2.</p>')

rt <- spec$reference_table
ref_html <- paste0(
  sprintf('<h3>%s</h3>', rt$title),
  '<table class="tbl ref"><thead><tr>',
  paste(sprintf('<th>%s</th>', unlist(rt$columns)), collapse = ""),
  '</tr></thead><tbody>',
  paste(vapply(rt$rows, function(r) {
    sprintf('<tr><td class="g">%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>',
            r$group, r$dsfi, r$bmwp, r$other, r$minimum)
  }, character(1)), collapse = ""),
  '</tbody></table>',
  paste(sprintf('<p class="fn">%s</p>', unlist(rt$footnotes)), collapse = ""))

# ============================================================================
# 5. PAGE
# ============================================================================

css <- '
* { margin: 0; padding: 0; box-sizing: border-box; }
body { font-family: "Segoe UI", system-ui, sans-serif; background: #ffffff; color: #2c2c2a; }
.page { width: %WIDTH%px; margin: 0 auto; }
.part { padding: 28px 0; }
h1 { font-size: 20px; font-weight: 600; text-align: center; }
h2 { font-size: 12.5px; font-weight: 400; color: #5f5e5a; text-align: center; margin: 6px auto 14px; max-width: 900px; line-height: 1.5; }
h3 { font-size: 15px; font-weight: 600; margin: 26px 0 8px; }
.legend { display: flex; flex-wrap: wrap; gap: 8px 20px; justify-content: center; font-size: 12px; margin-bottom: 8px; }
.li { display: inline-flex; align-items: center; gap: 6px; }
.chart { display: block; margin: 0 auto; }
.band { fill: #f4f3ef; stroke: #dcd9cf; stroke-width: 1; }
.blabel { font-size: 11.5px; font-weight: 700; fill: #6b6860; letter-spacing: 0.3px; }
.shape { stroke-width: 1.6; }
.shape.start { fill: #3d3b36; stroke: #3d3b36; }
.shape.process { fill: #e6f1fb; stroke: #5b8fc7; }
.shape.decision { fill: #fdf0d5; stroke: #d99a26; }
.shape.otu_retained { fill: #e3f1d6; stroke: #6fa33a; }
.shape.otu_excluded { fill: #f5e3e3; stroke: #b85c5c; stroke-dasharray: 6 3; }
.shape.otu_gap { fill: #e3f1d6; stroke: #e07b00; stroke-width: 3.2; }
.shape.note { fill: #f4f2fd; stroke: #8f87d6; stroke-dasharray: 3 3; }
.nt { width: 100%; height: 100%; display: flex; align-items: center; justify-content: center; text-align: center; font-size: 12px; line-height: 1.32; color: #2c2c2a; }
.nt.start { color: #ffffff; font-weight: 600; }
.nt.decision { font-weight: 600; font-size: 11.5px; }
.nt.note { font-size: 11px; color: #3c3489; }
.edge { fill: none; stroke: #5f5d56; stroke-width: 1.4; }
.elabel { font-size: 11px; font-weight: 700; fill: #3d3b36; paint-order: stroke; stroke: #ffffff; stroke-width: 4px; }
.tbl { width: 100%; border-collapse: collapse; font-size: 11.5px; }
.tbl th { text-align: left; padding: 6px; border-bottom: 2px solid #d99a26; }
.tbl td { padding: 5px 6px; vertical-align: top; border-bottom: 1px solid #ece6d6; line-height: 1.45; }
.tbl td.c { text-align: center; white-space: nowrap; }
.tbl.ref td.g { font-weight: 600; width: 105px; }
.small { font-size: 10.5px; color: #7a3b3b; }
.fn { font-size: 11.5px; color: #5f5e5a; margin-top: 6px; line-height: 1.5; }
'
page_w <- ceiling(svg_w)
css <- sub("%WIDTH%", page_w, css, fixed = TRUE)

html <- c(
  '<!DOCTYPE html><html><head><meta charset="utf-8">',
  sprintf('<title>%s</title>', gsub("<[^>]+>", "", spec$title)),
  '<style>', css, '</style></head><body><div class="page">',
  '<div class="part part1">',
  sprintf('<h1>%s</h1>', spec$title),
  sprintf('<h2>%s</h2>', fill(spec$subtitle)),
  legend_html,
  chart_svg,
  '</div><div class="part part2">',
  examples_html,
  ref_html,
  '</div></div></body></html>'
)

writeLines(html, output_file, useBytes = TRUE)
cat(sprintf("Decision tree HTML written to: %s\n", output_file))

# ---- Render HTML to a vector PDF -------------------------------------
# Printed with headless Chrome (chromote) so the PDF is true vector output.
# The page size is set to the measured content: page 1 holds the whole
# flowchart (never split across pages), page 2 the worked examples and the
# reference table.
pdf_file <- sub("\\.html$", ".pdf", output_file)
if (!requireNamespace("chromote", quietly = TRUE)) {
  message("Package 'chromote' not installed. Skipping PDF export. ",
          "Install with: install.packages('chromote')")
} else {
  b <- chromote::ChromoteSession$new(width = page_w + 80, height = 1200)
  loaded <- b$Page$loadEventFired(wait_ = FALSE)
  b$Page$navigate(paste0("file:///", normalizePath(output_file, winslash = "/")),
                  wait_ = FALSE)
  b$wait_for(loaded)
  size <- b$Runtime$evaluate(paste0(
    "(() => {",
    "  const w = document.querySelector('.page').scrollWidth + 72;",
    "  const h = Math.max(document.querySelector('.part1').scrollHeight,",
    "                     document.querySelector('.part2').scrollHeight) + 24;",
    "  const s = document.createElement('style');",
    "  s.textContent = '@page { size: ' + w + 'px ' + h + 'px; margin: 0 } ' +",
    "                  '.part2 { break-before: page }';",
    "  document.head.appendChild(s);",
    "  return w + 'x' + h;",
    "})()"))$result$value
  pdf <- b$Page$printToPDF(preferCSSPageSize = TRUE, printBackground = TRUE)
  writeBin(jsonlite::base64_dec(pdf$data), pdf_file)
  b$close()
  cat(sprintf("Decision tree PDF  written to: %s (2 pages, %s px)\n", pdf_file, size))
}

#==================== CLEAN UP WORKSPACE =====================
library(pacman)
rm(list = ls())       # Remove all objects from environment
gc()                  # Frees up unused memory
p_unload(all)         # Unload all loaded packages
graphics.off()        # Close all graphical devices
cat("\014")           # Clear the console
# Clear mind :)
