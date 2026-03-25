# -----------------------------
# Source utilities + model code (Full MRIO FD version)
# -----------------------------
# --- Core utilities ---
source(file.path(utils_dir, "utils.R"))

# --- Scenario logic ---
source(file.path(model_dir, "production_scenarios_2026.R"))
source(file.path(model_dir, "demand_scenarios_2026.R"))

# --- Model engine ---
source(file.path(model_dir, "run_model_2026.R"))
source(file.path(model_dir, "MVP_model_2026_fullMRIO.R"))

# --- Scenario analysis / comparison tools ---
source(file.path(utils_dir, "scenario_analysis_2026.R"))
source(file.path(utils_dir, "scenario_comparison_2026.R"))

# --- Full MRIO FD loader: reads FD_MRIO sheet and injects into parms ---
load.init.fullMRIO <- function(identif) {
  # Load standard initial state
  initial <- load.init(identif)

  # Read the FD_MRIO sheet
  fd_mrio <- openxlsx::read.xlsx(identif, sheet = "FD_MRIO")

  z1_rows <- fd_mrio[fd_mrio$region == "Z1", ]
  z2_rows <- fd_mrio[fd_mrio$region == "Z2", ]

  # Inject sector-level FD vectors into parms
  for (ch in c("hh", "gov", "id", "idg")) {
    for (type in c("dom", "xbr")) {
      col <- paste0("fd_", type, "_", ch)
      parm_Z1 <- paste0("Z1_", col, "-", 1:K)
      parm_Z2 <- paste0("Z2_", col, "-", 1:K)

      new_rows <- data.frame(
        label = c(parm_Z1, parm_Z2),
        value = c(z1_rows[[col]], z2_rows[[col]]),
        type  = rep(NA, 2 * K),
        stringsAsFactors = FALSE
      )
      rownames(new_rows) <- new_rows$label
      initial$pars <- rbind(initial$pars, new_rows)
    }
  }

  # Add init scalars for scaling — read from baseline RDS at t=2 (initial$vars
  # has all endogenous vars = 0 at t=1, which would cause division by zero).
  baseline_rds <- file.path(dirname(normalizePath(identif)),
                            "..", "output", "scenarios", "baseline_2026.RDS")
  if (file.exists(baseline_rds)) {
    base_sim <- readRDS(baseline_rds)$simulation
    init_vals <- c(as.numeric(base_sim["Z1_c",    2]),
                   as.numeric(base_sim["Z2_c",    2]),
                   as.numeric(base_sim["Z1_g",    2]),
                   as.numeric(base_sim["Z2_g",    2]),
                   as.numeric(base_sim["Z1_id",   2]),
                   as.numeric(base_sim["Z2_id",   2]),
                   as.numeric(base_sim["Z1_id_g", 2]),
                   as.numeric(base_sim["Z2_id_g", 2]))
    cat("load.init.fullMRIO: init scalars read from baseline t=2\n")
  } else {
    warning("baseline_2026.RDS not found — init scalars set to 1 (scaling disabled)")
    init_vals <- rep(1, 8)
  }
  init_scalars <- data.frame(
    label = c("Z1_c_init", "Z2_c_init",
              "Z1_g_init", "Z2_g_init",
              "Z1_id_init", "Z2_id_init",
              "Z1_idg_init", "Z2_idg_init"),
    value = init_vals,
    type  = rep(NA, 8),
    stringsAsFactors = FALSE
  )
  rownames(init_scalars) <- init_scalars$label
  initial$pars <- rbind(initial$pars, init_scalars)

  return(initial)
}
