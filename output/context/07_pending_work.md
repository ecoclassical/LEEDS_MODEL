# Pending Work & Manuscript Status

## Current State (2026-03-27)

- All 14 scenarios complete and cached in `output/scenarios/shock_runs/` (ρ=0.2, except Sc5/Sc14 at ρ=0.05)
- `scenario_analysis.qmd`: all 14 Discussion sections updated with simulation numbers; new Cross-Border Transmission section added; Two Transition Regimes section revised (FD scenarios no longer blanket "low leverage")
- `discussion.qmd`: standalone prose-only file, rendered to HTML — ready for review
- Comparison plots: 9 PDFs in `output/scenarios/comparison_plots/` split by FD/Int × EU/RoW × by-term/avg-term
- **User has comments to share** (pending next session)

## Session Completion Status (2026-03-27)

### Completed this session:
- [x] All 14 shocks run with correct ρ values (force = FALSE, loaded from cache)
- [x] All 14 Discussion sections updated with ρ=0.2/0.05 actual numbers
- [x] Cross-Border Transmission section written (4 patterns)
- [x] Two Transition Regimes section revised (FD heterogeneity acknowledged)
- [x] `discussion.qmd` created and rendered
- [x] Comparison plots split into FD/Int × EU/RoW = 4 by-term plots + 2 avg-term plots + 3 all-14 versions
- [x] Context files updated (this session)

### Pending (user comments expected):
- User said "ok i have comments" — comments not yet shared; will begin next session

## Open Work Items

### High Priority (manuscript)

1. **ΔM plots:** Visualize analytical ΔM₁, ΔM₂, ΔM by scenario from R/ scripts. Decompose into structural factor (Leontief differential) × scale factor. Relevant for paper's theoretical contribution.

2. **Rebound trajectory plots:** Baseline vs shock for material use M₁ over time. Illustrate structural rebound in Sc5/Sc14 (Construction) and Sc10 (Metal). Show cumulative stock differences.

3. **Consolidate QMDs:** `scenario_design.qmd` + `scenario_analysis.qmd` + `discussion.qmd` → single `scenarios_section.qmd` for the paper.

### Medium Priority

4. **Re-render `scenario_design.html`:** `scenario_design.qmd` was edited after last render — HTML is stale.

5. **Extended intensities:** Add land, water, primary material to intensity analysis in `scenarios_section.qmd` (currently only carbon in paper2.Rmd).

6. **`paper2.Rmd` port:** Update old file paths to new conventions (`bootstrap_2026.R`, `initial_state_2026.xlsx`). IO network + carbon/matter intensities analysis needs to be accessible.

### Lower Priority / Future Work

7. **Disclaimer section:** Carbon-industry IO connections as future work, noting relevance to US–Iran context and fossil-fuel sanctions.

8. **Scenarios 3, 6:** Near-zero results — decide whether to (a) note as null results, (b) re-specify to target sectors with larger allocation shares, or (c) drop from paper.

9. **Price adjustment in RoW:** Current model abstracts from relative price adjustment in RoW markets. Ecological leakage is first-order partial equilibrium estimate. Could extend with endogenous price feedback in future version.

10. **Construction calibration discussion:** Sc5/Sc14 emission rebound is a structural feature of current calibration. Paper should discuss whether this reflects real-world energy intensity of renovation vs new build, or whether calibration needs revision.

## Key Issues to Remember

### Scenario 14 is INCLUDED (reinstated)
Earlier memory said "drop Sc14 — misspecified". This was overridden. Sc14 is included in all analyses but flagged as showing emission rebound (structural finding, not calibration error for purposes of the paper argument).

### Construction (Sc5, Sc14) ρ = 0.05
Both Construction scenarios use ρ=0.05, not 0.2. This is specified in `data/scenarios.csv`. Check this when running or loading these shocks.

### Correct FD/Int split
- Final Demand: Scenarios 1–6 (6 scenarios)
- Intermediate Demand: Scenarios 7–14 (8 scenarios)
- NOT "7 and 7" — the split is 6 + 8

### `force = FALSE` in all shock chunks
All shock RDS files are cached. Do not set `force = TRUE` unless intentionally re-running.

## R Dependencies

```r
tidyverse, openxlsx, igraph, qgraph, visNetwork, scales, deSolve,
reshape2, ggrepel, knitr, kableExtra, beepr, patchwork, stringr
```
