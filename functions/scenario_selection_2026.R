### SCENARIOS

# 1) reduction in consumption level: reduction of propensities to consume?
# 2) change in consumption composition (e.g. higher share of services)
# increase consumption share of services, decrease consumption share of manufacturing; increase final investment of manufacturing
# 3) extension of products lifetime by increasing the share of durable goods
# reduce depreciation rate delta for firms; reduce Percentage of durable consumption goods discarded zeta_dc for households

shock <- para['shock']
reduction <- para['reduction']
nPeriods <- para['nPeriods']
t.shock <- para['t.shock']

# Household Diet Shift
if (shock == 1 && i == 2) {
  sim['Z1_beta-7', para['t.shock']:nPeriods] <- reduction *
    sim['Z1_beta-7', para['t.shock']:nPeriods]

  sim['Z1_beta-8', para['t.shock']:nPeriods] <- sim[
    'Z1_beta-8',
    para['t.shock']:nPeriods
  ] +
    (1 - reduction) * sim['Z1_beta-7', para['t.shock']:nPeriods]
}

# Household Energy Transition
if (shock == 2 && i == 2) {
  sim['Z1_beta-31', para['t.shock']:nPeriods] <- reduction *
    sim['Z1_beta-31', para['t.shock']:nPeriods]

  sim['Z1_beta-32', para['t.shock']:nPeriods] <- sim[
    'Z1_beta-32',
    para['t.shock']:nPeriods
  ] +
    (1 - reduction) * sim['Z1_beta-31', para['t.shock']:nPeriods]
}
