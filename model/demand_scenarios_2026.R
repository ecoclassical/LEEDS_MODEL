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

compute_delta_eff <- function(
  shock,
  rho,
  t,
  t_shock,
  delta_current,
  sc,
  prefix
) {
  shock <- as.integer(shock)
  rho <- as.numeric(rho)

  if (t < t_shock) {
    return(delta_current)
  }

  if (is.null(sc)) {
    stop("compute_delta_eff: sc table required")
  }

  sc_row <- sc[sc$shock == shock, , drop = FALSE]

  if (nrow(sc_row) != 1) {
    return(delta_current) # no demand shock for this shock id
  }

  from <- as.integer(sc_row$from[[1]])
  to <- as.integer(sc_row$to[[1]])

  # Construct names
  from_name <- paste0(prefix, "-", from)
  to_name <- paste0(prefix, "-", to)

  idx_from <- which(names(delta_current) == from_name)
  idx_to <- which(names(delta_current) == to_name)

  if (length(idx_from) != 1 || length(idx_to) != 1) {
    stop("compute_delta_eff: could not uniquely identify from/to")
  }

  m <- delta_current[idx_from] + delta_current[idx_to]

  delta_eff <- delta_current

  m_from <- delta_eff[idx_from]

  delta_eff[idx_from] <- (1 - rho) * m_from
  delta_eff[idx_to] <- delta_eff[idx_to] + rho * m_from

  return(delta_eff)
}

compute_all_delta_eff <- function(
  shock,
  rho,
  t,
  t_shock,
  sim,
  i,
  sc
) {
  list(
    beta = compute_delta_eff(
      shock,
      rho,
      t,
      t_shock,
      sim[zk.lab("beta"), i],
      sc,
      "Z1_beta"
    ),
    sigma = compute_delta_eff(
      shock,
      rho,
      t,
      t_shock,
      sim[zk.lab("sigma"), i],
      sc,
      "Z1_sigma"
    ),
    iota = compute_delta_eff(
      shock,
      rho,
      t,
      t_shock,
      sim[zk.lab("iota"), i],
      sc,
      "Z1_iota"
    ),
    iota_g = compute_delta_eff(
      shock,
      rho,
      t,
      t_shock,
      sim[zk.lab("iota_g"), i],
      sc,
      "Z1_iota_g"
    )
  )
}
