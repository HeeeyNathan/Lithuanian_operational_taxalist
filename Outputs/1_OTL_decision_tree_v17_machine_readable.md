# Decision Tree for Assigning Operational Taxonomic Units (OTUs)
# Lithuanian Macroinvertebrate Biomonitoring — OTL v17
# Machine-readable version
#
# Changes from v16:
#   - No semantic rule changes. Bumped for version coherence with the v17 OTU
#     assignment script, which adds a data-quality guard (cross-sheet name
#     consistency check) but does not alter any decision rules.
#
# Changes from v15:
#   - No semantic rule changes. v16 reflects bug fixes in the assignment
#     script (excl_family_explicit order-of-operations, forced_keeps_eqr
#     NA propagation) and brings the HTML representation back in sync
#     (Coreidae removed from Hemiptera BMWP scored row; subspecies-
#     inheritance and Phase-4-skip-for-forced rules added to the HTML —
#     both already present in this .md).


## PHASE 1: PREPARATION

STEP 1.1: Compile taxalist of aquatic macroinvertebrates occurring in Lithuania from literature and other sources.

STEP 1.2: Validate and correct taxonomy against GBIF and Molluscabase backbones.

STEP 1.3: Specialists and practitioners are provided only validated taxa at the SPECIES level and asked to state the taxonomic level to which they can identify each species. Higher taxonomic levels are not provided — the purpose is for specialists to inform us of their practical identification capability starting from species, not to confirm their ability to recognise higher ranks.

STEP 1.4: Flag taxa excluded from EQR calculations using the excluded_taxa sheet as the source of truth. This sheet contains taxa and higher-level entries that should not be included in metric computations. The exclusion logic works as follows:
  - A taxon's family is checked against the set of families appearing in the excluded_taxa sheet.
  - Entirely excluded groups are determined by checking if ALL families within a phylum, class, or order are in the excluded set.
  - Currently identified entirely excluded groups:
    - Phyla: Porifera, Cnidaria, Nematoda, Nematomorpha, Nemertea, Bryozoa
    - Classes: Polychaeta, Arachnida, Collembola, Maxillopoda, Branchiopoda
    - Orders: Stylommatophora, Dolichomicrostomida, Prolecithophora, Rhabdocoela, Anostraca
  - Individual excluded families (within otherwise-included groups): Coreidae, Gerridae, Hydrometridae, Mesoveliidae, Veliidae, Chrysomelidae, Curculionidae, Succineidae, Clausiliidae, Geoplanidae, and others per excluded_taxa sheet.


## PHASE 2: FORCED-LEVEL ASSIGNMENTS

Phase 2 identifies taxa whose OTU level is fixed regardless of specialist agreement. These taxa skip Phases 3 and 4. Phase 2 acts as a CEILING override — it caps the OTU level. The rationale is to prevent non-target taxa from inflating freshwater diversity or abundance metrics, or to consolidate groups where fine-resolution identification adds no metric value.

### 2.1 Entirely excluded groups (excluded from EQR)

For taxa belonging to entirely excluded groups (all families in excluded_taxa), the OTU defaults to the group's coarsest excluded level:
  - Species in entirely excluded phyla → OTU at PHYLUM level (e.g., Porifera Gen. sp.)
  - Species in entirely excluded classes → OTU at CLASS level (e.g., Polychaeta Gen. sp.)
  - Species in entirely excluded orders → OTU at ORDER level (e.g., Stylommatophora Gen. sp.)

There is no benefit to fine-resolution OTU assignment for groups never used in metric calculations. The specialist agreement is still recorded for traceability.

### 2.2 Oligochaeta (NOT excluded from EQR)

Oligochaeta is forced to a fixed level but participates in EQR calculations.

DECISION: Is the family Naididae?
  - YES → FORCED to FAMILY level → "Naididae Gen. sp."
    REASON: DSFI uses Naididae (formerly Tubificidae) as an IG6 entrance point. BMWP scores Naididae at family (score 1).
  - NO → FORCED to SUBCLASS level → "Oligochaeta Gen. sp."
    REASON: LRMI uses Oligochaeta at class level. Other oligochaete families are too difficult for routine identification. BMWP scores Oligochaeta at subclass (score 1).

NOTE: Order-level entries (Crassiclitellata, Enchytraeida, Lumbriculida, Tubificida) are also forced to SUBCLASS level → "Oligochaeta Gen. sp." Since all non-Naididae Oligochaeta are consolidated at subclass, the order-level fallback must match.

### 2.3 Neuroptera (NOT excluded from EQR)

All Neuroptera species, genus, and family entries are forced to FAMILY level:
  - Osmylidae → "Osmylidae Gen. sp."
  - Sisyridae → "Sisyridae Gen. sp."

REASON: Specialist agreement for Neuroptera species was at order level, which is too coarse for the two aquatic families. Forcing to family ensures each family receives its own OTU. Order-level Neuroptera entries remain at order level but are excluded from EQR (not truly aquatic).

### 2.4 Stenostomidae (excluded from EQR)

Stenostomidae is forced to PHYLUM level → "Platyhelminthes Gen. sp."

REASON: The order for Stenostomidae is taxonomically unavailable/unstable. Forcing to phylum ensures a stable OTU.

### 2.5 Individually excluded taxa (excluded from EQR)

Any taxon that is excluded from EQR but NOT in an entirely excluded group is forced to FAMILY level. This ensures that when only some families in an order are excluded (while others participate in EQR), each excluded family receives its own family-level OTU rather than being pooled at a coarser level.

Examples of taxa affected:
  - Talitridae (Amphipoda) — terrestrial/semi-aquatic amphipods → "Talitridae Gen. sp."
  - Non-Asellidae Isopoda (Chaetiliidae, Cirolanidae, etc.) — marine/brackish isopods → family-level OTU per family
  - Neomysis and Praunus (Mysida) — brackish/estuarine mysids → "Mysidae Gen. sp."
    NOTE: Other Mysida genera (Hemimysis, Limnomysis, Mysis, Paramysis) are NOT forced and follow normal processing.
  - Crangonidae and Palaemonidae (Decapoda) — brackish/estuarine shrimps → family-level OTU
  - Hemiptera excluded families (Coreidae, Gerridae, Hydrometridae, Veliidae, Mesoveliidae) → family-level OTU
  - Lepidoptera excluded families → family-level OTU
    NOTE: Freshwater Crambidae species (Acentria, Cataclysta, Elophila, Parapoynx, Schoenobius) are NOT excluded and follow Phases 3–4.


## PHASE 3: 100% SPECIALIST AGREEMENT (SPECIES-LEVEL ENTRIES ONLY)

Phase 3 applies only to species-level entries — the validated species-level taxa that specialists were asked to assess in Phase 1 (Step 1.3). Genus, family, and higher-level entries are not subject to Phase 3; they are assigned at their own taxonomic level (see "Higher-level entries" below).

NOTE: Taxa assigned in Phase 2 (forced-level or entirely excluded groups) have already received their OTU and do not enter Phase 3.

For each species-level taxon, determine the 100% specialist agreement level. This is the COARSEST level assigned by any specialist — the taxonomic level to which ALL specialists can reliably identify the taxon. No specialist's assessment is discarded.

→ Assign OTU at the 100% agreement level.

No simplification based on the number of representatives in Lithuania is applied. The agreement level is assigned directly as the OTU level.


## PHASE 4: METRIC COMPLIANCE CHECK (SPECIES-LEVEL ENTRIES ONLY)

For each species-level taxon, verify that the OTU level assigned in Phase 3 meets or exceeds the minimum identification requirement for its group as listed in the reference table below. If the taxon's group has no specific metric requirement, this check is automatically passed.

NOTE: Taxa assigned in Phase 2 have already received their OTU and do not enter Phase 4.

DECISION: Does the assigned OTU level meet or exceed the minimum metric requirement?
  - YES → OTU assignment is CONFIRMED.
  - NO → OVERRIDE: Adjust the OTU level to the minimum required by the relevant metric. Document the override with the reason.
    EXAMPLE: "100% specialist agreement = family; overridden to genus because DSFI requires Leuctra at genus level for IG1 placement."
    NOTE: Overrides should be rare. Frequent overrides indicate a gap between practitioner capability and metric requirements.


## HIGHER-LEVEL ENTRIES (GENUS, FAMILY, AND ABOVE)

Family, order, subclass, class, and phylum entries are structural fallback entries. They accommodate specimens that cannot be identified to a finer level. These entries are always assigned at their own taxonomic level:
  - Family entries → OTU at FAMILY level (e.g., "Baetidae Gen. sp.")
  - Order entries → OTU at ORDER level (e.g., "Ephemeroptera Gen. sp.")
  - Subclass, class, phylum entries → OTU at their respective level

### Genus entries — metric-minimum-aware, capped at genus

Genus entries are assigned using the metric minimum requirements for their taxonomic group, capped at genus level. The metric minimum for a genus is determined by the genus's own taxonomy (group membership, family, etc.), NOT inherited from individual species-level exceptions:

  - If the genus has a forced-level override (Phase 2), it resolves to the forced level (e.g., "Talitridae Gen. sp." for Talitridae genera).
  - If the metric minimum for the genus's group is genus or finer (e.g., species), the genus entry is assigned at GENUS level (e.g., "Leuctra sp." for Plecoptera where min = genus).
  - If the metric minimum is coarser than genus (e.g., family), the genus entry resolves to that coarser level (e.g., "Baetidae Gen. sp." for Ephemeroptera where min = family, or "Pontogammaridae Gen. sp." for Pontogammarus where Malacostraca min = family).
  - If the genus belongs to no metric group, it is assigned at GENUS level by default.
  - If the genus belongs to an entirely excluded group, it collapses to the group-level OTU (e.g., a genus in Porifera → Porifera Gen. sp.).

### EQR exclusion rule for higher-level entries (order and above)

For higher-level entries (order and above), an additional EQR exclusion rule applies:
  - Only truly aquatic groups keep their order-and-above entries for EQR calculations: Bivalvia (class), Plecoptera, Ephemeroptera, Odonata, Megaloptera, Trichoptera (orders).
  - All other groups' order-and-above entries are excluded from EQR because they could include terrestrial representatives.
  - Family-level entries follow the excluded_taxa sheet: excluded if the family appears in the excluded_taxa data.
  - Genus entries in entirely excluded groups collapse to the group-level OTU.

### Subspecies inheritance

Subspecies entries (e.g., Nemoura cinerea subsp. cinerea) that are not in the Specialist_taxalist inherit their OTU assignment from the parent species. This ensures subspecies are not penalised to a coarser fallback level when the parent species has a known specialist agreement.


## FINAL OUTPUT

Each taxon receives a final OTL entry containing:
  - Validated taxonomic name
  - OTU level (species / genus / subfamily / family / order / subclass / class / phylum)
  - OTU name (formatted, e.g., "Baetis sp." or "Baetidae Gen. sp.")
  - Taxonomic rank label (e.g., "species", "genus", "family")
  - Override flag (yes/no, with reason if yes)
  - EQR exclusion flag (yes/no)

Hierarchical entries (both fine-resolution OTUs and coarser fallback entries) coexist in the taxalist, accommodating damaged, juvenile, or poorly preserved specimens.


## REFERENCE TABLE: MINIMUM METRIC REQUIREMENTS BY GROUP

Each row shows the minimum OTU level that must be met after Phase 3 assignment. If 100% specialist agreement yields a finer level, the finer level is kept. These are floors, not ceilings.

### GROUP: Malacostraca
  DSFI: Asellus and Gammarus at GENUS (indicator groups + diversity).
  BMWP/ASPT (all taxa):
    Score 8: Astacidae Gen. sp. [family]
    Score 6: Gammaridae Gen. sp. [family], Corophiidae Gen. sp. [family], Crangonyctidae Gen. sp. [family], Niphargidae Gen. sp. [family], Pontogammaridae Gen. sp. [family]
    Score 3: Asellidae Gen. sp. [family]
  OTHER: %CrHi pools Malacostraca.
  → MINIMUM: GENUS for Asellus and Gammarus; FAMILY for others.
  NOTE: Genus-level Malacostraca entries default to FAMILY level.
  EXCLUDED TAXA (see Phase 2.5): Talitridae, non-Asellidae Isopoda, Neomysis/Praunus, Crangonidae/Palaemonidae are excluded from EQR → forced to FAMILY level.

### GROUP: Gastropoda
  DSFI: Ancylus and Lymnaea at GENUS (diversity groups).
  BMWP/ASPT (all taxa):
    Score 6: Viviparidae Gen. sp. [family], Neritidae Gen. sp. [family], Ancylidae Gen. sp. [family], Ancylus sp. [genus], Acroloxidae Gen. sp. [family]
    Score 5: Hydrobiidae Gen. sp. [family]
    Score 3: Lymnaeidae Gen. sp. [family], Physidae Gen. sp. [family], Planorbidae Gen. sp. [family], Valvatidae Gen. sp. [family], Bithyniidae Gen. sp. [family]
  OTHER: —
  → MINIMUM: GENUS for Ancylus and Lymnaea; FAMILY for others.

### GROUP: Bivalvia
  DSFI: Sphaerium at GENUS (diversity group).
  BMWP/ASPT (all taxa):
    Score 6: Unionidae Gen. sp. [family]
    Score 3: Sphaeriidae Gen. sp. [family]
  OTHER: —
  → MINIMUM: GENUS for Sphaerium; FAMILY for others.

### GROUP: Plecoptera
  DSFI: 13 genera at GENUS (IG1/IG2 entrance points): Amphinemura, Brachyptera, Capnia, Isogenus, Isoperla, Isoptena, Leuctra, Nemoura, Nemurella, Perlodes, Protonemura, Siphonoperla, Taeniopteryx.
  DSFI Plecoptera families used:
    IG1 (score 10 taxa): Leuctridae (Leuctra), Capniidae (Capnia), Perlodidae (Isogenus, Isoperla, Isoptena, Perlodes), Perlidae, Chloroperlidae (Siphonoperla)
    IG2 (score 7 taxa): Taeniopterygidae (Brachyptera, Taeniopteryx), Nemouridae (Amphinemura, Nemoura, Nemurella, Protonemura)
  BMWP/ASPT (all taxa):
    Score 10: Taeniopterygidae Gen. sp. [family], Leuctridae Gen. sp. [family], Perlodidae Gen. sp. [family], Perlidae Gen. sp. [family], Chloroperlidae Gen. sp. [family], Capniidae Gen. sp. [family]
    Score 7: Nemouridae Gen. sp. [family]
  OTHER: #DEP uses Plecoptera at SPECIES.
  → MINIMUM: at least GENUS.

### GROUP: Ephemeroptera
  DSFI: 8 families as IG/diversity entries.
  DSFI Ephemeroptera families used:
    IG1 (score 10): Ephemeridae
    IG2 (score varies): Ametropodidae, Ephemerellidae, Heptageniidae, Leptophlebiidae, Siphlonuridae
    IG3 (score 7): Caenidae
    IG5 (score 4): Baetidae
  BMWP/ASPT (all taxa):
    Score 10: Siphlonuridae Gen. sp. [family], Heptageniidae Gen. sp. [family], Leptophlebiidae Gen. sp. [family], Ephemerellidae Gen. sp. [family], Potamanthidae Gen. sp. [family], Ephemeridae Gen. sp. [family]
    Score 7: Caenidae Gen. sp. [family]
    Score 4: Baetidae Gen. sp. [family]
  OTHER: #DEP uses Ephemeroptera at SPECIES.
  → MINIMUM: at least FAMILY.

### GROUP: Trichoptera
  DSFI: Case-bearing and caseless families as diversity groups and IG entrance points.
  DSFI Trichoptera families used:
    Case-bearing (positive diversity groups): Beraeidae, Brachycentridae, Goeridae, Glossosomatidae, Hydroptilidae, Leptoceridae, Lepidostomatidae, Limnephilidae, Molannidae, Odontoceridae, Phryganeidae, Sericostomatidae
    Caseless (positive diversity groups / IG entrance): Ecnomidae, Hydropsychidae, Philopotamidae, Polycentropodidae, Psychomyiidae, Rhyacophilidae
  BMWP/ASPT (all taxa):
    Score 10: Beraeidae Gen. sp. [family], Brachycentridae Gen. sp. [family], Goeridae Gen. sp. [family], Lepidostomatidae Gen. sp. [family], Leptoceridae Gen. sp. [family], Molannidae Gen. sp. [family], Odontoceridae Gen. sp. [family], Phryganeidae Gen. sp. [family], Sericostomatidae Gen. sp. [family]
    Score 8: Ecnomidae Gen. sp. [family], Philopotamidae Gen. sp. [family], Psychomyiidae Gen. sp. [family]
    Score 7: Apataniidae Gen. sp. [family], Glossosomatidae Gen. sp. [family], Limnephilidae Gen. sp. [family], Polycentropodidae Gen. sp. [family], Rhyacophilidae Gen. sp. [family]
    Score 6: Hydroptilidae Gen. sp. [family]
    Score 5: Hydropsychidae Gen. sp. [family]
  OTHER: —
  → MINIMUM: at least FAMILY.

### GROUP: Coleoptera
  DSFI: Elmis, Limnius, Elodes at GENUS (IG1/IG2 + diversity).
  BMWP/ASPT (all taxa):
    Score 5: Haliplidae Gen. sp. [family], Hygrobiidae Gen. sp. [family], Noteridae Gen. sp. [family], Dytiscidae Gen. sp. [family], Gyrinidae Gen. sp. [family], Hydrophilidae Gen. sp. [family], Hydraenidae Gen. sp. [family], Hydrochidae Gen. sp. [family], Helophoridae Gen. sp. [family], Georissidae Gen. sp. [family], Clambidae Gen. sp. [family], Scirtidae Gen. sp. [family], Helodidae Gen. sp. [family], Dryopidae Gen. sp. [family], Elmidae Gen. sp. [family], Chrysomelidae Gen. sp. [family], Curculionidae Gen. sp. [family]
  OTHER: —
  → MINIMUM: GENUS for Elmis, Limnius, Elodes; FAMILY for others.

### GROUP: Diptera
  DSFI: Chironomus at GENUS; Eristalinae at SUBFAMILY; Simuliidae, Psychodidae, Chironomidae at FAMILY.
  BMWP/ASPT (all taxa):
    Score 5: Tipulidae Gen. sp. [family], Simuliidae Gen. sp. [family], Limoniidae Gen. sp. [family], Pediciidae Gen. sp. [family], Cylindrotomidae Gen. sp. [family]
    Score 2: Chironomidae Gen. sp. [family]
  OTHER: #DEP uses Diptera FAMILY richness.
  → MINIMUM: FAMILY; GENUS for Chironomus; SUBFAMILY for Eristalinae.

### GROUP: Megaloptera
  DSFI: Sialis at GENUS (IG4 + negative diversity).
  BMWP/ASPT (all taxa):
    Score 4: Sialidae Gen. sp. [family]
  OTHER: —
  → MINIMUM: GENUS for Sialis.

### GROUP: Hirudinea
  DSFI: Erpobdella and Helobdella at GENUS (negative diversity).
  BMWP/ASPT (all taxa):
    Score 4: Piscicolidae Gen. sp. [family]
    Score 3: Glossiphoniidae Gen. sp. [family], Hirudinidae Gen. sp. [family], Erpobdellidae Gen. sp. [family]
  OTHER: %CrHi pools Hirudinea.
  → MINIMUM: GENUS for Erpobdella and Helobdella; FAMILY for others.

### GROUP: Oligochaeta
  DSFI: Naididae (formerly Tubificidae) at FAMILY (IG6); Oligochaeta >=100 individuals as precluder (not >100).
  BMWP/ASPT (all taxa):
    Score 1: Oligochaeta Gen. sp. [subclass], Naididae Gen. sp. [family]
  OTHER: —
  → MINIMUM: Naididae at FAMILY; others at SUBCLASS.

### GROUP: Hemiptera
  DSFI: —
  BMWP/ASPT (all taxa):
    Score 10: Aphelocheiridae Gen. sp. [family]
    Score 5: Nepidae Gen. sp. [family], Naucoridae Gen. sp. [family], Notonectidae Gen. sp. [family], Pleidae Gen. sp. [family], Corixidae Gen. sp. [family], Gerridae Gen. sp. [family], Hydrometridae Gen. sp. [family], Mesoveliidae Gen. sp. [family]
  OTHER: %EHP pools Hemiptera. Excluded families (Coreidae, Gerridae, Hydrometridae, Mesoveliidae, Veliidae) → family-level OTU (see Phase 2.5).
  → MINIMUM: at least FAMILY.

### GROUP: Odonata
  DSFI: —
  BMWP/ASPT (all taxa):
    Score 8: Lestidae Gen. sp. [family], Calopterygidae Gen. sp. [family], Gomphidae Gen. sp. [family], Cordulegastridae Gen. sp. [family], Aeshnidae Gen. sp. [family], Corduliidae Gen. sp. [family], Libellulidae Gen. sp. [family]
    Score 6: Coenagrionidae Gen. sp. [family], Platycnemididae Gen. sp. [family]
  OTHER: —
  → MINIMUM: at least FAMILY.

### GROUP: Turbellaria
  DSFI: Tricladida at ORDER (positive diversity).
  BMWP/ASPT (all taxa):
    Score 5: Planariidae Gen. sp. [family], Dendrocoelidae Gen. sp. [family], Dugesiidae Gen. sp. [family]
  OTHER: —
  → MINIMUM: at least FAMILY.

### GROUP: Lepidoptera
  DSFI: —
  BMWP/ASPT (all taxa): —
  OTHER: Freshwater Crambidae species (Acentria, Cataclysta, Elophila, Parapoynx, Schoenobius) are included. All other Lepidoptera families are excluded.
  → MINIMUM: at least FAMILY.

### GROUP: Neuroptera
  DSFI: —
  BMWP/ASPT (all taxa): —
  OTHER: Osmylidae and Sisyridae are aquatic. All entries forced to family level (see Phase 2.3).
  → MINIMUM: FAMILY (forced ceiling, see Phase 2.3).
