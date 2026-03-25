# ============================================================================
# test_fullMRIO_baseline.R
# Runs the fullMRIO model with no shock and compares to the existing baseline.
#
# Usage: Rscript model/test_fullMRIO_baseline.R
# ============================================================================

cat("==== Full MRIO FD Extension: Baseline Test ====\n\n")

root <- "/Users/parvulesco/Documents/R/LEEDS_MODEL"
model_dir <- file.path(root, "model")
utils_dir <- file.path(root, "utils")
dir_data  <- file.path(root, "data")

# Source bootstrap (fullMRIO version)
source(file.path(model_dir, "bootstrap_2026_fullMRIO.R"))

# ---- Load initial state (fullMRIO) ----
initial_file <- file.path(dir_data, "initial_state_2026_fullMRIO.xlsx")
stopifnot(file.exists(initial_file))

cat("Loading initial state from:", initial_file, "\n")
initial <- load.init.fullMRIO(initial_file)

cat("K =", K, "  N =", N, "\n")
cat("Number of parms rows:", nrow(initial$pars), "\n")
cat("Number of vars rows:", nrow(initial$vars), "\n")

# ---- Verify FD parms are loaded ----
fd_parms <- grep("^Z[12]_fd_", initial$pars$label, value = TRUE)
cat("FD parms loaded:", length(fd_parms), "(expected", 8 * 2 * K, ")\n")

init_scalars <- grep("_init$", initial$pars$label, value = TRUE)
cat("Init scalars:", paste(init_scalars, collapse = ", "), "\n")

# ---- Quick accounting check at t=1 ----
parms_v <- setNames(initial$pars$value, initial$pars$label)
vars_v  <- setNames(initial$vars$value, initial$vars$label)

# Compute d from fullMRIO decomposition at t=1 (scale factors = 1)
d_dom <- (
  parms_v[paste0(rep(c("Z1","Z2"), each=K), "_fd_dom_hh-", rep(1:K, 2))] +
  parms_v[paste0(rep(c("Z1","Z2"), each=K), "_fd_dom_gov-", rep(1:K, 2))] +
  parms_v[paste0(rep(c("Z1","Z2"), each=K), "_fd_dom_id-", rep(1:K, 2))] +
  parms_v[paste0(rep(c("Z1","Z2"), each=K), "_fd_dom_idg-", rep(1:K, 2))]
)

d_xbr <- (
  parms_v[paste0(rep(c("Z1","Z2"), each=K), "_fd_xbr_hh-", rep(1:K, 2))] +
  parms_v[paste0(rep(c("Z1","Z2"), each=K), "_fd_xbr_gov-", rep(1:K, 2))] +
  parms_v[paste0(rep(c("Z1","Z2"), each=K), "_fd_xbr_id-", rep(1:K, 2))] +
  parms_v[paste0(rep(c("Z1","Z2"), each=K), "_fd_xbr_idg-", rep(1:K, 2))]
)

d_new <- unname(d_dom + d_xbr)
d_old <- unname(vars_v[c(paste0("Z1_d-", 1:K), paste0("Z2_d-", 1:K))])

cat("\n==== ACCOUNTING CHECK (t=1, scale=1) ====\n")
cat("Max |d_new - d_old|:", max(abs(d_new - d_old)), "\n")
cat("Sum d_new:", sum(d_new), "  Sum d_old:", sum(d_old), "\n")
if (max(abs(d_new - d_old)) < 1e-6) {
  cat("PASS: FD decomposition reproduces baseline d vector.\n")
} else {
  cat("WARNING: FD decomposition does NOT match baseline d vector!\n")
  cat("Differences by sector:\n")
  diffs <- d_new - d_old
  names(diffs) <- c(paste0("Z1_d-", 1:K), paste0("Z2_d-", 1:K))
  print(diffs[abs(diffs) > 1e-8])
}

# ---- Load scenarios table (needed by run.model) ----
scenario_file <- file.path(dir_data, "scenarios.csv")
sc <- read.csv(scenario_file)

# Workspace dirs (needed for get_shock_filename etc.)
workspace_dir <- file.path(root, "output", "scenarios")
dir_runs  <- file.path(workspace_dir, "shock_runs_fullMRIO")
dir_logs  <- file.path(workspace_dir, "logs_fullMRIO")
dir.create(dir_runs, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_logs, showWarnings = FALSE, recursive = TRUE)

get_shock_filename <- function(n) file.path(dir_runs, paste0("shock_", n, "_run.RDS"))
get_log_filename   <- function(n) file.path(dir_logs, paste0("shock_", n, "_run.log"))

# ---- Variable table (needed by some utils) ----
variable.table <- read.csv(file.path(dir_data, "Variable_Definitions.csv"),
                           stringsAsFactors = FALSE)

# ---- Run baseline (shock = 0, no actual shock) ----
cat("\n==== RUNNING FULLMRIO BASELINE ====\n")

baseline_fullMRIO <- run.model(
  initial,
  mvp.model.fullMRIO,
  sc = sc,
  log_file = file.path(dir_logs, "baseline_fullMRIO.log"),
  log_append = FALSE,
  print_final_state = FALSE
)

cat("\nBaseline completed. Last period:", baseline_fullMRIO$last_period, "\n")

# ---- Save baseline ----
baseline_file <- file.path(workspace_dir, "baseline_2026_fullMRIO.RDS")
saveRDS(baseline_fullMRIO, baseline_file)
cat("Saved fullMRIO baseline to:", baseline_file, "\n")

# ---- Compare to old baseline ----
old_baseline_file <- file.path(workspace_dir, "baseline_2026.RDS")
if (file.exists(old_baseline_file)) {
  cat("\n==== COMPARISON WITH ORIGINAL BASELINE ====\n")
  old_base <- readRDS(old_baseline_file)

  # Compare key aggregates at various time points
  compare_vars <- c("c", "g", "id", "id_g", "y", "yn", "go", "fd",
                     "imp", "rex", "n", "emis", "mat", "v", "wb",
                     "gdef", "b_s", "cab", "tb")

  t_check <- c(1, 25, 50, 75, 100)
  t_check <- t_check[t_check <= min(old_base$last_period, baseline_fullMRIO$last_period)]

  cat("\nVariable comparison (% difference, new vs old):\n")
  cat(sprintf("%-12s", "Variable"), sprintf("%10s", paste0("t=", t_check)), "\n")
  cat(strrep("-", 12 + 10 * length(t_check)), "\n")

  max_diff <- 0
  for (vv in compare_vars) {
    for (z in c("Z1", "Z2")) {
      vname <- paste0(z, "_", vv)
      if (vname %in% rownames(baseline_fullMRIO$simulation) &&
          vname %in% rownames(old_base$simulation)) {
        new_vals <- baseline_fullMRIO$simulation[vname, t_check]
        old_vals <- old_base$simulation[vname, t_check]
        pct_diff <- ifelse(old_vals != 0,
                           100 * (new_vals - old_vals) / abs(old_vals),
                           ifelse(new_vals == 0, 0, Inf))
        max_diff <- max(max_diff, max(abs(pct_diff[is.finite(pct_diff)])))
        cat(sprintf("%-12s", vname),
            sprintf("%10.6f", pct_diff), "\n")
      }
    }
  }

  cat("\nMax absolute % difference across all checked variables:", round(max_diff, 8), "%\n")

  if (max_diff < 0.01) {
    cat("PASS: fullMRIO baseline reproduces original baseline within 0.01%.\n")
  } else if (max_diff < 1.0) {
    cat("CLOSE: Small differences detected (< 1%). Check FD decomposition assumptions.\n")
  } else {
    cat("FAIL: Significant differences detected. Investigate.\n")
  }
} else {
  cat("\nNo old baseline found at:", old_baseline_file, "\n")
  cat("Cannot compare. Run the original model first.\n")
}

cat("\n==== TEST COMPLETE ====\n")
