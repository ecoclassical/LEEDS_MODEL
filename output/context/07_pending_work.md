# Pending Work & Manuscript Status

## Current State (March 2026)

- All 13 scenarios complete and cached in RDS (Scenario 14 excluded — misspecified)
- `full_mrio_scenario_analysis.qmd` runs all shocks with `force = TRUE` and produces comparison plots
- `scenario_discussion.qmd` is a new narrative synthesis (no R code), rendered to HTML
- Bilateral MRIO model fully implemented (MVP_model_2026.R, demand_scenarios_2026.R updated)

## Active Issues / Recent Changes

### Scenario 1 (Food/Meat) employment effect
The food shift (β 7→8) shows −0.11% employment — 5× larger than other final demand scenarios. This is **correct and economically explained**, not a bug:
- Meat has a large consumption share (β = 3.84% vs 1.08% for Energy)
- Meat is significantly more labour-intensive than Other Food (n/x = 5.37 vs 4.35)
The `scenario_discussion.qmd` currently overgeneralises Scenarios 1–6 as "near-neutral employment". **Needs correction: Scenario 1 is an exception within the final demand group.**

### Scenarios 3, 5, 6 show zero effect
These scenarios target sectors with negligible allocation shares in the relevant demand channel (government energy, firm investment energy/plastics). The scenarios are defined correctly in scenarios.csv but their effective shock size is near zero. **May want to either (a) note this explicitly in text, or (b) re-specify to target sectors with larger shares.**

### scenario_design.html is stale
`scenario_design.html` was last rendered before a significant edit to `scenario_design.qmd`. Needs re-render.

## Pending Work Items (as of 2026-03-26)

### High Priority (manuscript)
1. **Correct scenario_discussion.qmd:** Update the final demand section to note that Scenario 1 (Food) has a categorically larger employment effect. Revise the claim that "all six final demand scenarios have slight/neutral employment effects."

2. **ΔM plots:** Visualize analytical ΔM₁, ΔM₂, ΔM by scenario from R/ scripts. These show the structural factor (Leontief differential) × scale factor decomposition.

3. **Rebound trajectory plots:** Baseline vs shock for material use M₁, cumulative stock differences. Illustrate the structural rebound in Scenario 10 (Metal).

### Medium Priority
4. **Extended intensities in paper2:** Add land, water, primary material to the intensity analysis (currently only carbon). Port paper2.Rmd to new QMD with bootstrap_2026.R (old file paths are broken).

5. **Re-render scenario_design.html** after verifying scenario_design.qmd is stable.

6. **Consolidate QMDs:** scenario_design.qmd + full_mrio_scenario_analysis.qmd + scenario_discussion.qmd → single `scenarios_section.qmd` for the paper.

### Lower Priority / Future Work
7. **Disclaimer section:** Carbon-industry IO connections as future work, noting relevance to US–Iran context and fossil-fuel sanctions (timely).

8. **Scenarios 3, 5, 6:** Decide whether to re-specify these to more meaningful shocks or retain as "null result" illustrations.

9. **Price adjustment in RoW:** Current model abstracts from relative price adjustment in RoW markets. Ecological leakage channel is first-order partial equilibrium. Could extend with endogenous price feedback.

## R Dependencies

```r
tidyverse, openxlsx, igraph, qgraph, visNetwork, scales, deSolve,
reshape2, ggrepel, knitr, kableExtra, beepr
```
