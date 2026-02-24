# model/demand_scenarios_2026.R
# Purpose:
#   Period-by-period demand scenario logic returning beta_eff (no side effects).
# Contract:
#   - Defines `compute_beta_eff(...)`
#   - Does NOT write to global env, does NOT mutate sim, does NOT mutate para

compute_beta_eff <- function(
  shock,
  rho,
  t,
  t_shock,
  beta_current
) {
  shock <- as.integer(shock)
  rho <- as.numeric(rho)
  t <- as.integer(t)
  t_shock <- as.integer(t_shock)

  stopifnot(!is.na(shock), !is.na(rho), !is.na(t), !is.na(t_shock))
  stopifnot(is.numeric(beta_current), length(beta_current) > 0)

  # Default: no change
  beta_eff <- beta_current

  # Apply only from t_shock onward
  if (t < t_shock) {
    return(beta_eff)
  }

  # Helper: robustly find exactly one index by name
  .idx1 <- function(nm) {
    ii <- which(names(beta_eff) == nm)
    if (length(ii) != 1) {
      stop(
        "compute_beta_eff: expected exactly one match for '",
        nm,
        "', got ",
        length(ii)
      )
    }
    ii
  }

  # -----------------------------
  # Demand-side shocks (local)
  # -----------------------------

  # 1) Household Diet Shift: Z1_beta-7 <-> Z1_beta-8 (example)
  if (shock == 1) {
    i7 <- .idx1("Z1_beta-7")
    i8 <- .idx1("Z1_beta-8")

    m <- beta_eff[i7] + beta_eff[i8]
    beta_eff[i7] <- (1 - rho) * m
    beta_eff[i8] <- rho * m
  }

  # 2) Household Energy Transition: Z1_beta-31 <-> Z1_beta-32
  if (shock == 2) {
    i31 <- .idx1("Z1_beta-31")
    i32 <- .idx1("Z1_beta-32")

    m <- beta_eff[i31] + beta_eff[i32]
    beta_eff[i31] <- (1 - rho) * m
    beta_eff[i32] <- rho * m
  }

  # If you later want “many downstream (54)” logic, add it here
  # in a way that only touches beta_eff (local vector) and returns it.

  return(beta_eff)
}
