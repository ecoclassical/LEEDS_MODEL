# ============================================================
# run_all_scenarios.R
# Re-runs baseline + all 14 shock scenarios from scratch,
# overwriting cached RDS files in output/scenarios/.
# Run this whenever initial_state or model code changes.
# ============================================================

# ---- 1. Paths -------------------------------------------------------
root      <- normalizePath(file.path(dirname(rstudioapi::getSourceEditorContext()$path), ".."))
model_dir <- file.path(root, "model")
utils_dir <- file.path(root, "utils")
dir_data  <- file.path(root, "data")

workspace_dir <- file.path(root, "output", "scenarios")
dir_runs      <- file.path(workspace_dir, "shock_runs")
dir_tables    <- file.path(workspace_dir, "shock_tables")
dir_logs      <- file.path(workspace_dir, "logs")

invisible(lapply(
  list(workspace_dir, dir_runs, dir_tables, dir_logs),
  dir.create, showWarnings = FALSE, recursive = TRUE
))

# ---- 2. Bootstrap ---------------------------------------------------
source(file.path(model_dir, "bootstrap_2026.R"))

initial_filename <- file.path(dir_data, "full_mrio_initial_state.xlsx")
stopifnot(file.exists(initial_filename))
initial <- load.init(initial_filename)

scenario_filename <- file.path(dir_data, "scenarios.csv")
sc <- read.csv(scenario_filename)

# ---- 3. Baseline ----------------------------------------------------
cat("\n=== Running baseline ===\n")
baseline <- run_or_load_baseline(initial, mvp.model, force = TRUE)
cat("Baseline done.\n")

# ---- 4. All 14 shocks -----------------------------------------------
n_shocks <- nrow(sc)
cat(sprintf("\n=== Running %d shock scenarios ===\n", n_shocks))

scenario_list <- vector("list", n_shocks)

for (n in seq_len(n_shocks)) {
  cat(sprintf("[%02d/%02d] %s ... ", n, n_shocks, sc$display_name[n]))
  t0 <- proc.time()["elapsed"]

  initial_shock <- initial
  initial_shock$pars["shock", "value"] <- n

  scenario_list[[n]] <- run_or_load_shock(
    n_shock   = n,
    initial   = initial_shock,
    model_fun = mvp.model,
    force     = TRUE
  )

  elapsed <- round(proc.time()["elapsed"] - t0, 1)
  cat(sprintf("done (%.1fs)\n", elapsed))
}

names(scenario_list) <- sc$display_name
cat("\nAll scenarios complete.\n")
beepr::beep(3)
