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

## Current State (April 2026) — SUBMITTED

**Paper submitted** to *Ecological Economics* special issue "Ecological Macroeconomics Modelling: Exploring Alternative Futures" on **2026-04-03**, via personal email to guest editor. Future work = revisions in response to referee reports.

### Submission package (`output/pdf/` and `docs/`)

| File | Description |
|---|---|
| `paper_draft.pdf` | Main manuscript, April 3 2026 |
| `title_page.pdf` | Title, abstract, highlights, CRediT, funding |
| `cover_letter.pdf` | Plain-text JHU letterhead, right-aligned in jhublue |
| `supplementary_material.pdf` | Appendices A1–A4 |
| `suggested_reviewers.pdf` | Morlin, Cano, Yajima |
| `resum_catala.pdf` | Plain-language Catalan summary with figures |

### Key finalised notation and conventions

- **Ten materials**: food, energy, wood, plastics, pulp, paper, metals, glass, cement, construction
- **Ω = Φf**: final demand volume vector; eq 2.2 = LΩ; Ω₂₁ = cross-border sub-vector (Fig 2)
- **CE sentence**: "Circular economy interventions modify sector-level entries of either A (productivity shocks) or Φ (final-demand shocks), propagating through L and Ω"
- **Four transmission regimes**: symmetric contraction, production leakage, competitive displacement, fossil-fuel collapse
- **CE trilemma**: no scenario simultaneously achieves ecological improvement, macroeconomic stability, and social equity

### Standard figure theme (`R/compute_portfolio_analysis.R`)

```r
theme_paper <- function(base_size = 13) {
  theme_grey(base_size = base_size) %+replace%
    theme(
      plot.title    = element_text(face = 'bold',   size = 16, hjust = 0),
      plot.subtitle = element_text(face = 'italic',  size = 12, hjust = 0),
      axis.text.y   = element_text(size = 11, hjust = 1, margin = margin(r = 6)),
      axis.text.x   = element_text(size = 11),
      strip.text    = element_text(size = 12)  # not bold
    )
}
```
Apply `theme_paper()` to all new figures. Add `legend.position = 'top'` as a per-plot override for bar/column charts.
