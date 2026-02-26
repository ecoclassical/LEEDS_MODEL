# -----------------------------
# Source utilities + model code
# -----------------------------
# --- Core utilities ---
source(file.path(utils_dir, "utils.R"))

# --- Scenario logic ---
source(file.path(model_dir, "production_scenarios_2026.R"))
source(file.path(model_dir, "demand_scenarios_2026.R"))

# --- Model engine ---
source(file.path(model_dir, "run_model_2026.R"))
source(file.path(model_dir, "MVP_model_2026.R"))

# --- Scenario analysis / comparison tools ---
source(file.path(utils_dir, "scenario_analysis_2026.R"))
source(file.path(utils_dir, "scenario_comparison_2026.R"))
