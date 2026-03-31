# File Map

## Root: /Users/parvulesco/Documents/R/LEEDS_MODEL/

## Entry Point

```r
root <- "/Users/parvulesco/Documents/R/LEEDS_MODEL"
utils_dir <- file.path(root, "utils")
model_dir  <- file.path(root, "model")
source(file.path(model_dir, "bootstrap_2026.R"))
initial <- load.init(file.path(root, "data", "initial_state_2026.xlsx"))

# Run or load a shock (n = 1..14):
run <- run_or_load_shock(n_shock, initial, mvp.model)
# All 14 shocks are cached — use force = FALSE (default) to load from RDS
```

## model/

| File | Purpose |
|------|---------|
| `MVP_model_2026.R` | Main SFC equations (~870 lines). Period-by-period dynamic simulation. |
| `bootstrap_2026.R` | Loads all utilities + model. Sets up K=54, directories, etc. |
| `demand_scenarios_2026.R` | Final-demand shock logic (β/σ/ι vector shifts). Returns modified vectors without side effects. |
| `production_scenarios_2026.R` | A-matrix intermediate substitution logic. |

## utils/

| File | Purpose |
|------|---------|
| `run_utils.R` | `run_or_load_shock()`, `shock.summary()`, `shock.long.new()`, `load.init()` |
| `scenario_analysis_2026.R` | `scenario_analysis_plot()` — heatmap per-scenario |
| `scenario_comparison_2026.R` | `build_policy_comparison_plot()` (with `region_filter` param), `build_policy_comparison_plot_avg_terms()` — cross-scenario comparison |
| `view_utils.R` | `view.shock()`, `plot_selected_vars()`, `view.A()` |
| `utils.R` | `z.lab()`, `zk.lab()`, `zk.sum()`, `zk.mean()` — variable labeling helpers |

### `build_policy_comparison_plot()` signature:
```r
build_policy_comparison_plot(
  df,                    # from shock.summary() + lookup join
  variable.table,        # Variable_Definitions metadata
  sc = NULL,             # scenario metadata (display_name, domain, shock)
  workspace_dir = NULL,  # if not NULL, saves PDF to this dir
  filename = "p_comparison.pdf",
  rho = 0.2,
  colors = NULL,         # named vector: colors["1"] through colors["14"]
  region_filter = NULL   # "EU", "RoW", or NULL (both)
)
```
When `region_filter` is set, facet formula switches from `display_name ~ domain + region` to `display_name ~ domain`.

## data/

| File | Purpose |
|------|---------|
| `initial_state_2026.xlsx` | Initial state + parameters (multi-sheet) |
| `scenarios.csv` | 14 scenario definitions (shock, domain, sector, transaction, shift, target, primary, secondary, rho, display_name) |
| `sector_list.csv` | 54-sector labels |
| `Variable_Definitions.csv` | Variable metadata (label, name, unit, dimension, category) |
| `A_matrix_wide.xlsx` | 108×108 technical coefficients (wide format) |
| `A_matrix.csv` | 108×108 technical coefficients (exported) |

## output/scenarios/

| File/Dir | Purpose |
|----------|---------|
| `baseline_2026.RDS` | Baseline simulation (no shock). **Prerequisite for all shock comparisons.** |
| `shock_runs/shock_N_run.RDS` | Cached RDS for shock N (N = 1..14). **Do NOT delete.** All 14 present. |
| `shock_tables/shock_N_table.csv` | CSV summary table for shock N (% deviations at Immediate/ST/LT for Z1/Z2). |
| `p_comparison_terms.pdf` | All-14 by-term comparison plot (EU + RoW) |
| `p_comparison_avg_terms.pdf` | All-14 time-average comparison plot |
| `comparison_plots/p_fd_eu_terms.pdf` | FD scenarios (1–6), EU effects, by term |
| `comparison_plots/p_fd_row_terms.pdf` | FD scenarios (1–6), RoW effects, by term |
| `comparison_plots/p_int_eu_terms.pdf` | Int scenarios (7–14), EU effects, by term |
| `comparison_plots/p_int_row_terms.pdf` | Int scenarios (7–14), RoW effects, by term |
| `comparison_plots/p_fd_avg_terms.pdf` | FD scenarios (1–6), time-average, EU+RoW |
| `comparison_plots/p_int_avg_terms.pdf` | Int scenarios (7–14), time-average, EU+RoW |
| `comparison_plots/p_all_eu_terms.pdf` | All-14, EU effects, by term |
| `comparison_plots/p_all_row_terms.pdf` | All-14, RoW effects, by term |
| `comparison_plots/p_all_avg_terms.pdf` | All-14, time-average, EU+RoW |

## qmd/

| File | Purpose | Status |
|------|---------|--------|
| `scenario_analysis.qmd` | Main analysis: all 14 shocks, individual + cross-scenario plots. **Primary working document.** | Current |
| `scenario_analysis.html` | Rendered HTML of above | Up to date (rendered Feb 26, updated Mar 27) |
| `discussion.qmd` | Standalone prose-only Discussion sections (no R code) for all 14 scenarios | New (Mar 27) |
| `discussion.html` | Rendered HTML of above | Current |
| `scenario_design.qmd` | IO theory + scenario setup (632 lines) | HTML stale |
| `scenario_design.html` | Rendered HTML — stale (qmd edited after last render) | Stale |
| `scenarios_section.qmd` | Consolidated paper section (design + analysis + discussion) | In progress |
| `feb26_scenario_analysis.qmd` | Extended variant (Feb 26 version) | Archive |
| `sketch_scenario_design.qmd` | Draft/workpad | Archive |

## R/ (Analytical Scripts)

| File | Purpose |
|------|---------|
| `compute_M_final_demand.R` | ΔM₁, ΔM₂, ΔM analytical formulas for final-demand shocks |
| `compute_M_final_demand_2.R` | Alternative formulation |
| `compute_M_intermediate_demand.R` | ΔM₁, ΔM₂, ΔM for production shocks (sums over all j — economy-wide) |

## docs/

| File | Purpose |
|------|---------|
| `JUST2CE Paper Draft.docx` | Main manuscript under development |
| `JUST2CE Paper Draft (1).docx` | Earlier version |
