# ------------------------------------------------------------
# Compute ΔM1, ΔM2, ΔM for *final-demand* reallocation scenarios
# For ALL targets: beta, sigma, iota, iota_g
# Uses corresponding aggregate quantities: c, g, id, id_g (Z1_* levels)
# ------------------------------------------------------------

rho <- 0.2
prefix <- "Z1" # change if you use Z2, etc.
t_shock <- 70 # choose the reporting time index you want to evaluate levels at

# --- read scenarios.csv (prefer project path; fallback to uploaded file) ---
scenario_path_1 <- file.path(dir_data, "scenarios.csv")
scenario_path_2 <- "/mnt/data/scenarios.csv"

sc <- if (file.exists(scenario_path_1)) {
  read.csv(scenario_path_1, stringsAsFactors = FALSE)
} else {
  read.csv(scenario_path_2, stringsAsFactors = FALSE)
}

# --- OPTIONAL: if sector_list exists in your environment, we will label sectors ---
has_sector_list <- exists("sector_list", inherits = TRUE) &&
  is.data.frame(get("sector_list", inherits = TRUE)) &&
  all(c("sector_code", "label") %in% names(get("sector_list", inherits = TRUE)))

# --- Leontief inverse ---
A <- initial$A.matrix
L <- solve(diag(nrow(A)) - A)

# --- helpers to fetch scalar levels (c, g, id, id_g) ---
get_sim_value <- function(varname) {
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
  # target in {beta, sigma, iota, iota_g} -> corresponding level (Z1_c, Z1_g, Z1_id, Z1_id_g)
  D_name <- dplyr::case_when(
    target == "beta" ~ paste0(prefix, "_c"),
    target == "sigma" ~ paste0(prefix, "_g"),
    target == "iota" ~ paste0(prefix, "_id"),
    target == "iota_g" ~ paste0(prefix, "_id_g"),
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
  # delta_1 is the share of the FROM sector in the relevant simplex
  delta_name <- dplyr::case_when(
    target == "beta" ~ paste0(prefix, "_beta-", from_sector),
    target == "sigma" ~ paste0(prefix, "_sigma-", from_sector),
    target == "iota" ~ paste0(prefix, "_iota-", from_sector),
    target == "iota_g" ~ paste0(prefix, "_iota_g-", from_sector),
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

# --- keep only the final-demand simplex families you actually want ---
sc_fd <- sc %>%
  dplyr::filter(.data$target %in% c("beta", "sigma", "iota", "iota_g")) %>%
  dplyr::mutate(
    from = as.integer(.data$from),
    to = as.integer(.data$to)
  )

# --- label sectors if sector_list exists ---
if (has_sector_list) {
  sector_list2 <- get("sector_list", inherits = TRUE) %>%
    dplyr::mutate(sector_code = as.integer(.data$sector_code))

  sc_fd <- sc_fd %>%
    dplyr::left_join(
      sector_list2 %>% dplyr::rename(from_sector = label),
      by = c("from" = "sector_code")
    ) %>%
    dplyr::left_join(
      sector_list2 %>% dplyr::rename(to_sector = label),
      by = c("to" = "sector_code")
    )
} else {
  sc_fd <- sc_fd %>%
    dplyr::mutate(
      from_sector = NA_character_,
      to_sector = NA_character_
    )
}

# --- core computation per scenario ---
out <- sc_fd %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    D = get_level_D(.data$target),
    delta1 = get_delta1(.data$target, .data$from),

    l11 = L[.data$from, .data$from],
    l12 = L[.data$from, .data$to],
    l21 = L[.data$to, .data$from],
    l22 = L[.data$to, .data$to],

    DeltaM1 = rho * delta1 * D * (l12 - l11),
    DeltaM2 = rho * delta1 * D * (l22 - l21),
    DeltaM = rho * delta1 * D * ((l12 + l22) - (l11 + l21)),

    check = abs(DeltaM - (DeltaM1 + DeltaM2))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(dplyr::desc(abs(.data$DeltaM))) %>%
  dplyr::select(
    shock,
    domain,
    sector,
    transaction,
    shift,
    target,
    from,
    from_sector,
    to,
    to_sector,
    D,
    delta1,
    l11,
    l12,
    l21,
    l22,
    DeltaM1,
    DeltaM2,
    DeltaM,
    check,
    display_name,
    rds_file,
    table_file
  )

# --- diagnostics you should actually look at ---
# 1) Naming mismatch: D or delta1 is NA -> your Z1_* naming differs from the mapping above.
# 2) check should be ~0 (numerical tolerance). Big values mean indexing/formula bug.
# 3) Missing sector labels (NA from_sector/to_sector) means sc has codes not in sector_list.
bad_levels <- out %>% dplyr::filter(is.na(D) | is.na(delta1))
bad_labels <- out %>% dplyr::filter(is.na(from_sector) | is.na(to_sector))
bad_check <- out %>% dplyr::filter(check > 1e-8)

cat("\nRows with missing D or delta1:", nrow(bad_levels), "\n")
cat("Rows with missing sector labels:", nrow(bad_labels), "\n")
cat("Rows failing DeltaM = DeltaM1 + DeltaM2 check:", nrow(bad_check), "\n\n")

print(out, n = min(50, nrow(out)))

# write.csv(out, file.path(workspace_dir, "deltaM_final_demand_ALL_rho0p2.csv"), row.names = FALSE)
