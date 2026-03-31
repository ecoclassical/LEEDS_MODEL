# model/demand_scenarios_2026.R
# Purpose:
#   Period-by-period demand scenario logic returning beta_eff (no side effects).
# Contract:
#   - Defines `compute_beta_eff(...)`
#   - Does NOT write to global env, does NOT mutate sim, does NOT mutate para

compute_delta_eff <- function(
  shock,
  target,
  rho,
  t,
  t_shock,
  delta_current,
  sc,
  prefix
) {
  shock <- as.integer(shock)
  target <- as.character(target)
  rho <- as.numeric(rho)

  if (t != t_shock) {
    return(delta_current)
  }

  if (is.null(sc)) {
    stop("compute_delta_eff: sc table required")
  }

  sc_row <- sc[sc$shock == shock & sc$target == target, , drop = FALSE]

  if (nrow(sc_row) != 1) {
    return(delta_current) # no applicable row (or ambiguous) => no change
  }

  from <- as.integer(sc_row$primary[[1]])
  to <- as.integer(sc_row$secondary[[1]])

  from_name <- paste0(prefix, "-", from)
  to_name <- paste0(prefix, "-", to)

  idx_from <- which(names(delta_current) == from_name)
  idx_to <- which(names(delta_current) == to_name)

  if (length(idx_from) != 1 || length(idx_to) != 1) {
    stop("compute_delta_eff: could not uniquely identify from/to")
  }

  delta_eff <- delta_current
  m_from <- delta_eff[idx_from]

  delta_eff[idx_from] <- (1 - rho) * m_from
  delta_eff[idx_to] <- delta_eff[idx_to] + rho * m_from

  delta_eff
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
  shock <- as.integer(shock)

  sc_row <- sc[sc$shock == shock, , drop = FALSE]
  if (nrow(sc_row) != 1) {
    return(list(
      beta_Z1   = sim[zk.lab("beta_Z1"),   i],
      sigma_Z1  = sim[zk.lab("sigma_Z1"),  i],
      iota_Z1   = sim[zk.lab("iota_Z1"),   i],
      iota_g_Z1 = sim[zk.lab("iota_g_Z1"), i]
    ))
  }

  target <- as.character(sc_row$target[[1]])

  # IMPORTANT: use initial (t=1) vector as the base so the shock doesn't compound
  beta_base    <- sim[zk.lab("beta_Z1"),   1]
  sigma_base   <- sim[zk.lab("sigma_Z1"),  1]
  iota_base    <- sim[zk.lab("iota_Z1"),   1]
  iota_g_base  <- sim[zk.lab("iota_g_Z1"), 1]

  list(
    beta_Z1 = if (target == "beta") {
      # domestic bloc (Z1 consumers of Z1 goods)
      tmp <- compute_delta_eff(
        shock, "beta", rho, t, t_shock,
        delta_current = beta_base, sc = sc,
        prefix = "Z1_beta_Z1"
      )
      # cross-border bloc (Z2 consumers of Z1 goods)
      compute_delta_eff(
        shock, "beta", rho, t, t_shock,
        delta_current = tmp, sc = sc,
        prefix = "Z2_beta_Z1"
      )
    } else {
      beta_base
    },

    sigma_Z1 = if (target == "sigma") {
      tmp <- compute_delta_eff(
        shock, "sigma", rho, t, t_shock,
        delta_current = sigma_base, sc = sc,
        prefix = "Z1_sigma_Z1"
      )
      compute_delta_eff(
        shock, "sigma", rho, t, t_shock,
        delta_current = tmp, sc = sc,
        prefix = "Z2_sigma_Z1"
      )
    } else {
      sigma_base
    },

    iota_Z1 = if (target == "iota") {
      tmp <- compute_delta_eff(
        shock, "iota", rho, t, t_shock,
        delta_current = iota_base, sc = sc,
        prefix = "Z1_iota_Z1"
      )
      compute_delta_eff(
        shock, "iota", rho, t, t_shock,
        delta_current = tmp, sc = sc,
        prefix = "Z2_iota_Z1"
      )
    } else {
      iota_base
    },

    iota_g_Z1 = if (target == "iota_g") {
      tmp <- compute_delta_eff(
        shock, "iota_g", rho, t, t_shock,
        delta_current = iota_g_base, sc = sc,
        prefix = "Z1_iota_g_Z1"
      )
      compute_delta_eff(
        shock, "iota_g", rho, t, t_shock,
        delta_current = tmp, sc = sc,
        prefix = "Z2_iota_g_Z1"
      )
    } else {
      iota_g_base
    }
  )
}
