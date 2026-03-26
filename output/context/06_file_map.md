# File Map

## Root: /Users/parvulesco/Documents/R/LEEDS_MODEL/

## Entry Point

```r
root <- "/Users/parvulesco/Documents/R/LEEDS_MODEL"
utils_dir <- file.path(root, "utils")
model_dir  <- file.path(root, "model")
source(file.path(model_dir, "bootstrap_2026.R"))
initial <- load.init(file.path(root, "data", "full_mrio_initial_state.xlsx"))

# Run or load a shock (n = 1..13):
run <- run_or_load_shock(n_shock, initial, mvp.model)
```

## model/

| File | Purpose |
|------|---------|
| `MVP_model_2026.R` | Main SFC equations (~870 lines). Period-by-period dynamic simulation. |
| `bootstrap_2026.R` | Loads all utilities + model. Sets up K=54 etc. |
| `demand_scenarios_2026.R` | Final-demand shock logic (`compute_delta_eff`, `compute_all_delta_eff`). Returns modified β/σ/ι vectors without side effects. |
| `production_scenarios_2026.R` | A-matrix intermediate substitution logic. |

## utils/

| File | Purpose |
|------|---------|
| `run_utils.R` | `run_or_load_shock()`, `shock.summary()`, `shock.long.new()`, `load.init()` |
| `scenario_analysis_2026.R` | `scenario_analysis_plot()` — heatmap per-scenario |
| `scenario_comparison_2026.R` | `build_policy_comparison_plot()`, `build_policy_comparison_plot_avg_terms()` — cross-scenario comparison |
| `view_utils.R` | `view.shock()`, `plot_selected_vars()`, `view.A()`, `plot_target_fit()` |
| `utils.R` | `z.lab()`, `zk.lab()`, `zk.sum()`, `zk.mean()` — variable labeling helpers |

## data/

| File | Purpose |
|------|---------|
| `full_mrio_initial_state.xlsx` | Initial state + parameters (multi-sheet: global.pars, aggregate.pars, industry.vars, A.matrix, B.matrix, MRIO.flows, MRIO.coefficients, fd_transposed) |
| `scenarios.csv` | 13 scenario definitions (shock, target, from, to, rho, domain) |
| `sector_list.csv` | 54-sector labels |
| `Variable_Definitions.csv` | Variable metadata |
| `A_matrix.csv` | 108×108 technical coefficients |
| `mrio_table.csv` | Full MRIO table (108×108 IO + FD columns DH:DO = cols 112–119) |

## output/scenarios/

| File/Dir | Purpose |
|----------|---------|
| `baseline_2026.RDS` | Baseline simulation (no shock). **Prerequisite for all shock comparisons.** |
| `shock_runs/shock_N_run.RDS` | Cached RDS for shock N. Do NOT delete. |
| `shock_tables/shock_N_table.csv` | CSV summary table for shock N (% deviations). |
| `p_comparison_terms.pdf` | Immediate/ST/LT comparison plot (all 13 scenarios) |
| `p_comparison_avg_terms.pdf` | Time-average comparison plot (all 13 scenarios) |

## qmd/

| File | Purpose |
|------|---------|
| `full_mrio_scenario_analysis.qmd` | Main analysis: runs all 13 shocks, individual visualisations, cross-scenario comparison plots. **Primary renderable document.** |
| `scenario_discussion.qmd` | Narrative synthesis: two regimes + trilemma + policy implications (no R code, pure text). |
| `scenario_design.qmd` | IO theory + scenario setup (632 lines). Stale HTML — needs re-render. |
| `scenario_analysis.qmd` | Earlier analysis document (single-region era). |
| `scenarios_section.qmd` | Consolidated paper section (design + analysis + discussion). |
| `sketch_scenario_design.qmd` | Draft/workpad. |

## R/ (Analytical Scripts)

| File | Purpose |
|------|---------|
| `compute_M_final_demand.R` | ΔM₁, ΔM₂, ΔM analytical formulas for final-demand shocks |
| `compute_M_final_demand_2.R` | Alternative formulation |
| `compute_M_intermediate_demand.R` | ΔM₁, ΔM₂, ΔM for production shocks (requires summing over all j) |

## docs/

| File | Purpose |
|------|---------|
| `JUST2CE Paper Draft.docx` | Main manuscript under development |
| `JUST2CE Paper Draft (1).docx` | Earlier version |
