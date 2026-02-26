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

  if (t < t_shock) {
    return(delta_current)
  }
  if (is.null(sc)) {
    stop("compute_delta_eff: sc table required")
  }

  sc_row <- sc[sc$shock == shock & sc$target == target, , drop = FALSE]

  if (nrow(sc_row) != 1) {
    return(delta_current) # no applicable row (or ambiguous) => no change
  }

  from <- as.integer(sc_row$from[[1]])
  to <- as.integer(sc_row$to[[1]])

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
    # No (or ambiguous) scenario row: return current values unchanged
    return(list(
      beta = sim[zk.lab("beta"), i],
      sigma = sim[zk.lab("sigma"), i],
      iota = sim[zk.lab("iota"), i],
      iota_g = sim[zk.lab("iota_g"), i]
    ))
  }

  target <- as.character(sc_row$target[[1]])

  beta_current <- sim[zk.lab("beta"), i]
  sigma_current <- sim[zk.lab("sigma"), i]
  iota_current <- sim[zk.lab("iota"), i]
  iota_g_current <- sim[zk.lab("iota_g"), i]

  list(
    beta = if (target == "beta") {
      compute_delta_eff(
        shock,
        "beta",
        rho,
        t,
        t_shock,
        delta_current = beta_current,
        sc = sc,
        prefix = "Z1_beta"
      )
    } else {
      beta_current
    },

    sigma = if (target == "sigma") {
      compute_delta_eff(
        shock,
        "sigma",
        rho,
        t,
        t_shock,
        delta_current = sigma_current,
        sc = sc,
        prefix = "Z1_sigma"
      )
    } else {
      sigma_current
    },

    iota = if (target == "iota") {
      compute_delta_eff(
        shock,
        "iota",
        rho,
        t,
        t_shock,
        delta_current = iota_current,
        sc = sc,
        prefix = "Z1_iota"
      )
    } else {
      iota_current
    },

    iota_g = if (target == "iota_g") {
      compute_delta_eff(
        shock,
        "iota_g",
        rho,
        t,
        t_shock,
        delta_current = iota_g_current,
        sc = sc,
        prefix = "Z1_iota_g"
      )
    } else {
      iota_g_current
    }
  )
}
