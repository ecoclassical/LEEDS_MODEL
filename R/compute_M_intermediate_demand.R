# ------------------------------------------------------------
# Compute ΔM1, ΔM2, ΔM for *intermediate-demand* substitution scenarios
# rho fixed at 0.2
#
# Interpretation:
# - from = primary input sector (row index i=1 in your math, but general)
# - to   = secondary input sector (row index i=2 in your math, but general)
# - using sector j must be provided in scenarios.csv (a column indicating the *column* of A)
#
# First-order approximation (correct sign, no typos):
#   x' = (I - A')^{-1} d,  A' = A + ΔA
#   (I - A - ΔA)^{-1} ≈ L + L(ΔA)L   where L = (I - A)^{-1}
#   ⇒ Δx ≈ L (ΔA) x
#
# With substitution in column j:
#   Δa_{from,j} = -rho * a_{from,j}
#   Δa_{to,j}   = +rho * a_{from,j}
#   ⇒ Δx_i ≈ rho * a_{from,j} * x_j * (l_{i,to} - l_{i,from})
# ------------------------------------------------------------

rho <- 0.2
prefix <- "Z1"
t_shock <- 70

# --- read scenarios.csv (prefer your project path; fallback to uploaded file) ---
scenario_path_1 <- file.path(dir_data, "scenarios.csv")
scenario_path_2 <- "/mnt/data/scenarios.csv"

sc <- if (file.exists(scenario_path_1)) {
  read.csv(scenario_path_1, stringsAsFactors = FALSE)
} else {
  read.csv(scenario_path_2, stringsAsFactors = FALSE)
}

# --- Leontief inverse from baseline A ---
A <- initial$A.matrix
L <- solve(diag(nrow(A)) - A)

# --- helper: get baseline gross output x_j at t_shock ---
get_xj <- function(j) {
  x_name <- paste0(prefix, "_x-", as.integer(j))
  if (exists("baseline", inherits = TRUE)) {
    sim <- get("baseline", inherits = TRUE)$simulation
    if (!is.null(sim) && !is.null(rownames(sim)) && x_name %in% rownames(sim)) {
      return(as.numeric(sim[x_name, t_shock]))
    }
  }
  # If baseline not present, we *cannot* do intermediate-demand impacts properly,
  # because x_j is required. Return NA and let output show missingness.
  NA_real_
}

# --- identify the "using sector" column in scenarios.csv ---
# You need a column that indicates the *column j* of A where the substitution occurs.
# Common names people use; extend here if yours differs.
using_candidates <- c(
  "using",
  "using_sector",
  "j",
  "col",
  "column",
  "sector_using"
)
using_col <- using_candidates[using_candidates %in% names(sc)][1]

if (is.na(using_col)) {
  stop(
    "Intermediate-demand scenarios need a using-sector column (the column j of A). ",
    "Add one to scenarios.csv, e.g. a column named 'using' with integers 1..N."
  )
}

# --- keep only intermediate-demand scenarios ---
# Adjust these filters to match your registry conventions.
# The key is: these are the rows where you intend to shock A (not beta/sigma/iota/iota_g).
sc_int <- sc %>%
  dplyr::filter(
    .data$target %in%
      c("A", "a", "intermediate", "intermediate_demand", "io", "tech")
  )

if (nrow(sc_int) == 0) {
  stop(
    "No intermediate-demand scenarios found after filtering on target. Check sc$target labels."
  )
}

# --- detailed, row-level contributions (each row = one (shock, from, to, using=j)) ---
detail <- sc_int %>%
  dplyr::mutate(
    from = as.integer(.data$from),
    to = as.integer(.data$to),
    using = as.integer(.data[[using_col]])
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    # baseline objects needed for first-order effect
    a_fromj = A[from, using],
    x_j = get_xj(using),

    # Leontief entries used repeatedly
    l_from_from = L[from, from],
    l_from_to = L[from, to],
    l_to_from = L[to, from],
    l_to_to = L[to, to],

    # Δx_i formulas for i = from and i = to
    DeltaM1 = rho * a_fromj * x_j * (l_from_to - l_from_from), # Δx_from
    DeltaM2 = rho * a_fromj * x_j * (l_to_to - l_to_from), # Δx_to

    # ΔM = Δx_from + Δx_to under your definition M = x_from + x_to
    DeltaM = rho *
      a_fromj *
      x_j *
      ((l_from_to + l_to_to) - (l_from_from + l_to_from))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    shock,
    target,
    from,
    to,
    using,
    a_fromj,
    x_j,
    l_from_from,
    l_from_to,
    l_to_from,
    l_to_to,
    DeltaM1,
    DeltaM2,
    DeltaM
  )

# --- shock-level totals (if a shock hits multiple using sectors, sum first-order effects) ---
out_int <- detail %>%
  dplyr::group_by(shock, target, from, to) %>%
  dplyr::summarise(
    n_using = dplyr::n(),
    DeltaM1 = sum(DeltaM1, na.rm = TRUE),
    DeltaM2 = sum(DeltaM2, na.rm = TRUE),
    DeltaM = sum(DeltaM, na.rm = TRUE),
    check = abs(DeltaM - (DeltaM1 + DeltaM2)),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(abs(DeltaM)))

print(out_int, n = min(50, nrow(out_int)))

# Optional: save both
# write.csv(detail,  file.path(workspace_dir, "deltaM_intermediate_detail_rho0p2.csv"), row.names = FALSE)
# write.csv(out_int, file.path(workspace_dir, "deltaM_intermediate_totals_rho0p2.csv"), row.names = FALSE)
