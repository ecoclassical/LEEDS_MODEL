# ------------------------------------------------------------
# Compute ΔM1, ΔM2, ΔM for *final-demand* reallocation scenarios
# rho fixed at 0.2
# Requires: initial (from load.init()), and baseline OR at least initial$vars
# Uses: initial$A.matrix to build Leontief inverse L
# ------------------------------------------------------------

rho <- 0.2
prefix <- "Z1" # change if you use Z2, etc.
t_shock <- 70 # change if your reporting time differs

# --- read scenarios.csv (prefer your project path; fallback to uploaded file) ---
scenario_path_1 <- file.path(dir_data, "scenarios.csv")
scenario_path_2 <- "/mnt/data/scenarios.csv"

sc <- if (file.exists(scenario_path_1)) {
  read.csv(scenario_path_1, stringsAsFactors = FALSE)
} else {
  read.csv(scenario_path_2, stringsAsFactors = FALSE)
}

# --- Leontief inverse ---
A <- initial$A.matrix
L <- solve(diag(nrow(A)) - A)

# --- helpers to fetch scalar levels (c, g, id, id_g) ---
get_sim_value <- function(varname) {
  # baseline$simulation expected to be a matrix-like object with rownames
  if (exists("baseline", inherits = TRUE)) {
    sim <- get("baseline", inherits = TRUE)$simulation
    if (
      !is.null(sim) && !is.null(rownames(sim)) && varname %in% rownames(sim)
    ) {
      return(as.numeric(sim[varname, t_shock]))
    }
  }
  NA_real_
}

get_init_value <- function(varname) {
  # initial$vars expected to be a data.frame with rownames and 'value' column
  if (
    !is.null(initial$vars) &&
      !is.null(rownames(initial$vars)) &&
      varname %in% rownames(initial$vars) &&
      "value" %in% names(initial$vars)
  ) {
    return(as.numeric(initial$vars[varname, "value"]))
  }
  NA_real_
}

get_level_D <- function(target) {
  # Map "target" (delta family) -> the corresponding aggregate component D
  # Adjust these mappings if your scenarios.csv uses different labels.
  D_name <- dplyr::case_when(
    target %in% c("beta", "c", "household", "hh") ~ paste0(prefix, "_c"),
    target %in% c("sigma", "g", "gov", "government") ~ paste0(prefix, "_g"),
    target %in% c("iota", "id", "inv_private", "private_investment") ~ paste0(
      prefix,
      "_id"
    ),
    target %in% c("iota_g", "id_g", "inv_public", "public_investment") ~ paste0(
      prefix,
      "_id_g"
    ),
    TRUE ~ NA_character_
  )

  if (is.na(D_name)) {
    return(NA_real_)
  }

  val <- get_sim_value(D_name)
  if (is.na(val)) {
    val <- get_init_value(D_name)
  }
  val
}

get_delta1 <- function(target, from_sector) {
  # delta_1 is the share of the *from* sector in the relevant simplex
  delta_name <- dplyr::case_when(
    target %in% c("beta", "c", "household", "hh") ~ paste0(
      prefix,
      "_beta-",
      from_sector
    ),
    target %in% c("sigma", "g", "gov", "government") ~ paste0(
      prefix,
      "_sigma-",
      from_sector
    ),
    target %in% c("iota", "id", "inv_private", "private_investment") ~ paste0(
      prefix,
      "_iota-",
      from_sector
    ),
    target %in% c("iota_g", "id_g", "inv_public", "public_investment") ~ paste0(
      prefix,
      "_iota_g-",
      from_sector
    ),
    TRUE ~ NA_character_
  )

  if (is.na(delta_name)) {
    return(NA_real_)
  }

  val <- get_sim_value(delta_name)
  if (is.na(val)) {
    val <- get_init_value(delta_name)
  }
  val
}

# --- keep only final-demand scenarios (target identifies simplex family) ---
final_targets <- c(
  "beta",
  "sigma",
  "iota",
  "iota_g",
  "c",
  "g",
  "id",
  "id_g",
  "household",
  "government"
)
sc_fd <- sc %>%
  dplyr::filter(.data$target %in% final_targets)

# --- core computation per scenario ---
out <- sc_fd %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    from = as.integer(.data$from),
    to = as.integer(.data$to),

    # levels
    D = get_level_D(.data$target),
    delta1 = get_delta1(.data$target, .data$from),

    # pick out the needed Leontief entries
    l11 = L[from, from],
    l12 = L[from, to],
    l21 = L[to, from],
    l22 = L[to, to],

    # formulas (final-demand shift)
    DeltaM1 = rho * delta1 * D * (l12 - l11),
    DeltaM2 = rho * delta1 * D * (l22 - l21),
    DeltaM = rho * delta1 * D * ((l12 + l22) - (l11 + l21))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    shock,
    target,
    from,
    to,
    D,
    delta1,
    l11,
    l12,
    l21,
    l22,
    DeltaM1,
    DeltaM2,
    DeltaM
  )

# --- sanity checks you should not ignore ---
# 1) If D or delta1 is NA, your naming conventions differ (adjust get_level_D / get_delta1).
# 2) If DeltaM != DeltaM1 + DeltaM2 (within tolerance), something is wrong.
out <- out %>%
  dplyr::mutate(check = abs(DeltaM - (DeltaM1 + DeltaM2))) %>%
  dplyr::arrange(dplyr::desc(abs(DeltaM)))

print(out, n = min(50, nrow(out)))
# write.csv(out, file.path(workspace_dir, "deltaM_final_demand_rho0p2.csv"), row.names = FALSE)

sc2 <- sc %>%
  dplyr::mutate(
    from = as.integer(from),
    to = as.integer(to)
  ) %>%
  dplyr::left_join(
    sector_list %>%
      dplyr::mutate(sector_code = as.integer(sector_code)) %>%
      dplyr::rename(from_sector = label),
    by = c("from" = "sector_code")
  ) %>%
  dplyr::left_join(
    sector_list %>%
      dplyr::mutate(sector_code = as.integer(sector_code)) %>%
      dplyr::rename(to_sector = label),
    by = c("to" = "sector_code")
  ) %>%
  write.csv(file.path(dir_data, 'scenarios_extended.csv'), row.names = FALSE)
