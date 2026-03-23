# CLAUDE.md — LEEDS_MODEL

## Project Identity

**Name:** LEEDS_MODEL
**Author:** Oriol Vallès Codina (Johns Hopkins University, Net Zero Industrial Policy Lab)
**Submission target:** *Ecological Economics* special issue on ecological macroeconomic modelling
**Companion project:** JUST2CE (Just Transition to Circular Economy)

## What This Project Is

A two-region (EU as Z1, Rest of World as Z2) **Multi-Regional Input-Output + Stock-Flow Consistent (MRIO-SFC)** macroeconomic model. It simulates **circular economy policy interventions** — primary-to-secondary material substitutions — and tracks ecological, macroeconomic, social/distributional, and external-balance outcomes across 14 scenarios.

The core research question: how do CE transitions affect the **trilemma between ecological improvement, macroeconomic stability, and social equity**, and how do impacts distribute asymmetrically between the EU (Core) and RoW (Periphery)?

## Directory Map

```
model/          Core model engine
  MVP_model_2026.R           Main dynamic SFC equations (~870 lines)
  bootstrap_2026.R           Loads all utilities + model
  demand_scenarios_2026.R    Final-demand shift logic (delta operators)
  production_scenarios_2026.R  A-matrix intermediate substitution logic

R/              Analytical scripts (run standalone after bootstrap)
  compute_M_final_demand.R        ΔM1, ΔM2, ΔM for final-demand shocks
  compute_M_final_demand_2.R      Alternative formulation
  compute_M_intermediate_demand.R ΔM1, ΔM2, ΔM for production shocks

utils/          Infrastructure + plotting
  run_utils.R                run_or_load_shock(), shock.summary(), shock.long.new(), load.init()
  scenario_analysis_2026.R   scenario_analysis_plot()
  scenario_comparison_2026.R Cross-scenario aggregation/heatmaps
  utils.R                    z.lab, zk.lab, zk.sum, zk.mean (variable labeling)

data/
  initial_state_2026.xlsx    Initial state + parameters (multi-sheet)
  scenarios.csv              14 scenario definitions (shock, target, from, to, rho, domain)
  sector_list.csv            54-sector labels
  Variable_Definitions.csv   Variable metadata
  A_matrix.csv               108×108 technical coefficients (exported)
  shock_runs/                Cached RDS per shock (DO NOT delete)
  shock_tables/              CSV summary tables per shock

output/scenarios/
  baseline_2026.RDS          Baseline simulation (prerequisite for all shocks)
  shock_runs/shock_*.RDS     14 shock runs (cached)
  shock_tables/              CSV summary tables
  p_*.pdf / p_*.png          Comparison and network plots

qmd/                         Quarto/R-Markdown documents
  scenario_design.qmd        IO theory + scenario setup (632 lines)
  scenario_analysis.qmd      Main analysis: all 14 shocks (1,600+ lines)
  scenario_takeaways.qmd     Discussion/synthesis across scenarios (223 lines)
  demand_scenario_takeaways.qmd  Earlier synthesis draft (137 lines)
  paper2.Rmd                 IO network + carbon/matter intensities (old path conventions)
  feb26_scenario_analysis.qmd  Extended variant
  workplan.qmd               Project roadmap

docs/
  JUST2CE Paper Draft.docx   Main paper under development
  D5.1_Report_JUST2CE_OVC+JBF_v6.docx  Deliverable report
```

## Entry Point

```r
# From any script in model/ or qmd/:
source(file.path(model_dir, "bootstrap_2026.R"))
initial <- load.init(initial_filename)   # loads initial_state_2026.xlsx

# Run or load a shock (n = 1..14):
run <- run_or_load_shock(n_shock, initial, mvp.model)
```

## The 14 Scenarios

| # | Domain | Description | from → to |
|---|--------|-------------|-----------|
| 1–3 | Final demand | HH/Gov food & energy shifts | 28↔3, 29↔3 |
| 4–6 | Final demand | Firm energy, plastics | 29↔3, 4↔... |
| 7–13 | Production | Wood, pulp, plastics, metal, glass, cement, energy | A-matrix |
| 14 | Production | Construction shift | A-matrix |

Key parameters: `rho = 0.2`, shock activated at `t.shock = 70`, 100-period simulation.

## Scenario Taxonomy (Key Finding)

Three **transition regimes** emerge:

1. **Construction expansion (14)**: Economically/socially expansionary, ecologically negative.
2. **Intermediate production restructuring (7–13)**: Strong ecological gains, contractionary + distributionally regressive. EU–RoW asymmetries pronounced.
3. **Final demand reallocation (1–6)**: Low leverage — small effects everywhere.

**Core trade-off**: ecological improvement ↔ macro-social contraction (except construction).

## Analytical Formulas (crucial)

**Final-demand shock:**
- ΔM₁ = ρ·δ₁·D·(l₁₂ − l₁₁)
- ΔM₂ = ρ·δ₁·D·(l₂₂ − l₂₁)
- ΔM  = ρ·δ₁·D·[(l₁₂+l₂₂) − (l₁₁+l₂₁)]

**Intermediate-demand shock (first-order):**
- ΔM₁ ≈ ρ·a₁ⱼ·xⱼ·(l₁₂ − l₁₁)
- ΔM₂ ≈ ρ·a₁ⱼ·xⱼ·(l₂₂ − l₂₁)
- ΔM  ≈ ρ·a₁ⱼ·xⱼ·[(l₁₂+l₂₂) − (l₁₁+l₂₁)]

Structural rebound if l₁₂ > l₁₁ (secondary sector has higher primary material requirements than primary sector itself).

## Variable Naming

- `Z1_*` = EU region; `Z2_*` = RoW region
- Aggregates: `Z1_c` (consumption), `Z1_g` (gov spending), `Z1_y` (gross output), `Z1_n` (employment), `Z1_mat` (material), `Z1_emis` (emissions)
- Sectoral: `Z1_x-j` (output sector j), `Z1_beta-j` (consumption share j)
- Distributional: `shw` (wage share), `shp` (profit share), `ydw` (worker income), `ydc` (capitalist income)
- External: `cab` (current account), `tb` (trade balance), `gdef` (government deficit)

## R Dependencies

```r
tidyverse, openxlsx, igraph, qgraph, visNetwork, scales, deSolve,
reshape2, ggrepel, knitr, kableExtra, beepr
```

## Current State (March 2026)

- All 14 scenarios complete and cached in RDS
- Three QMD documents exist (design, analysis, takeaways) — need consolidation into one paper section
- paper2.Rmd has IO network + carbon/matter intensity analysis (old file paths, needs updating)
- Pending: ΔM plots from analytical formulas, rebound/trajectory plots for M₁, extended intensities (land, water)
- Paper target: Ecological Economics special issue
- Timeliness: EU–RoW carbon-industry IO links are the dominant cross-border connections — highly relevant given US–Iran war context and fossil-fuel sanctions

## Pending Work (tracked here)

1. **Consolidate** scenario_design + scenario_analysis + scenario_takeaways → single `qmd/scenarios_section.qmd`
2. **ΔM plots**: visualize analytical ΔM₁, ΔM₂, ΔM by scenario (from R/ scripts)
3. **Rebound trajectory plots**: baseline vs shock for M₁, cumulative stock differences
4. **Extended intensities**: add land, water, primary material to paper2 alongside carbon
5. **Disclaimer section**: future work on carbon-industry IO connections and war/sanctions context
6. **paper2.Rmd**: update to new file path conventions (bootstrap_2026.R, initial_state_2026.xlsx)

## Notes

- `paper2.Rmd` uses old file path conventions (`functions/`, `data/baseline_nov25.RDS`) — not directly renderable with current infrastructure; content should be ported to a new QMD.
- **HTML vs QMD freshness check (2026-03-18):**
  - `scenario_analysis.html` (Feb 26 18:11) was rendered 1 min AFTER `scenario_analysis.qmd` (Feb 26 18:10) — **up to date**.
  - `scenario_design.html` (Feb 27 11:19) is OLDER than `scenario_design.qmd` (Feb 27 12:31) — **stale**, needs re-render.
- Intermediate-demand scenarios (7–14): `compute_M_intermediate_demand.R` requires a `using` column in scenarios.csv (column j of A). This column is absent. In `scenarios_section.qmd`, this is handled by summing over ALL sectors j (economy-wide adoption), consistent with how `production_scenarios_2026.R` actually applies the shock.

## Main Output Document

`qmd/scenarios_section.qmd` — consolidated paper section (created 2026-03-18) covering:
- IO structure + intensities (carbon, land, water, material)
- Scenario registry + mechanics
- Analytical ΔM table + plots (structural factor × scale factor)
- Simulation trajectories (3 representative + all-shock heatmaps)
- Triple dilemma narrative (3 regimes + EU-RoW asymmetry)
- Rebound trajectories + cumulative stock plots
- Forward look / carbon-industry disclaimer
