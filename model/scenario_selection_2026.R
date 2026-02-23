### SCENARIOS

# 1) rho in consumption level: rho of propensities to consume?
# 2) change in consumption composition (e.g. higher share of services)
# increase consumption share of services, decrease consumption share of manufacturing; increase final investment of manufacturing
# 3) extension of products lifetime by increasing the share of durable goods
# reduce depreciation rate delta for firms; reduce Percentage of durable consumption goods discarded zeta_dc for households

shock <- para['shock']
rho <- para['rho']
nPeriods <- para['nPeriods']
t.shock <- para['t.shock']
demand.shocks <- 2

# Household Diet Shift
if (shock == 1) {
  m <- sim['Z1_beta-7', para['t.shock']] + sim['Z1_beta-8', para['t.shock']]

  sim['Z1_beta-7', para['t.shock']:nPeriods] <- m * rho
  sim['Z1_beta-8', para['t.shock']:nPeriods] <- (1 - rho) * m
}

# Household Energy Transition
if (shock == 2) {
  m <- sim['Z1_beta-31', para['t.shock']] + sim['Z1_beta-32', para['t.shock']]

  sim['Z1_beta-31', para['t.shock']:nPeriods] <- m * rho
  sim['Z1_beta-32', para['t.shock']:nPeriods] <- (1 - rho) * m
}

# Production - Wood
if (shock == 7) {
  # m = initial$B.matrix[3, 11] + initial$B.matrix[12, 2
  para[c('Z1_ce', 'Z2_ce')] <- c(1, 0)

  initial$B.matrix[11, ] <- rho
  initial$B.matrix[12, ] <- (1 - rho)
}

# # Production - Wood
# if (shock == 3 && i == 2) {
#   m <- sim['Z1_beta-11', para['t.shock']] + sim['Z1_beta-12', para['t.shock']]

#   sim['Z1_beta-11', para['t.shock']:nPeriods] <- m * rho
#   sim['Z1_beta-12', para['t.shock']:nPeriods] <- (1 - rho) * m

#   m <- B.t[3, 11] + B.t[12, 11]
#   B.t[3, 11] <- rho * m
#   B.t[12, 11] <- (1 - rho) * m
# }

# # Production - Pulp
# if (shock == 4 && i == 2) {
#   m <- sim['Z1_beta-11', para['t.shock']] + sim['Z1_beta-12', para['t.shock']]

#   sim['Z1_beta-11', para['t.shock']:nPeriods] <- m * rho
#   sim['Z1_beta-12', para['t.shock']:nPeriods] <- (1 - rho) * m

#   m <- B.t[3, 11] + B.t[12, 11]
#   B.t[3, 11] <- rho * m
#   B.t[12, 11] <- (1 - rho) * m
# }
