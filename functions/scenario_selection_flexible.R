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

# CE PRACTICES BY HOUSEHOLDS AND FIRMS
if (shock == 1 && i == para['t.shock']) 
{
  para['Z1_alpha1'] <- 1.55 * reduction * para['Z1_alpha1'] 
  para['Z1_alpha2'] <- 1.55 * reduction * para['Z1_alpha2'] 
  para['Z1_alpha3'] <- 1.55 * reduction * para['Z1_alpha3'] 
}

#1.2 Shift in consumption composition towards services
if (shock == 2 && i == 2) 
{
  sim['Z1_beta-1', para['t.shock'] : nPeriods] <- reduction * sim['Z1_beta-1', para['t.shock'] : nPeriods]
  sim['Z1_beta-3', para['t.shock'] : nPeriods] <- 1 - colSums(sim[paste0('Z1_beta-', 1 : 2), para['t.shock'] : nPeriods])
}

#1.3 Product lifetime extension of Capital Goods
if (shock == 3 && i == para['t.shock']) para['Z1_delta'] <- reduction * para['Z1_delta']

#### #### HIGHER USE OF CIRCULAR ECONOMY INPUTS
#1.4 Higher use of recycled/remanufactured inputs
if (shock == 4 && i == para['t.shock']) 
{ 
  sim['Z1_delta', i] <- .3
  para['Z1_ce'] <- 1
}

if (shock == 4 & i > t.shock & i <= (t.shock + 10)) 
{
  sim['Z1_delta', i] <- 0.99 * sim['Z1_delta', i - 1] # .9895
  target.sector <- c(12, 14, 16, 18, 22, 25, 27, 29, 30, 37)
  sim[paste0('Z1_vat-', target.sector), i] <- 0 
} 
# if (shock == 4 & i == (t.shock + 11))
# {
#   sim['Z1_delta', i : nPeriods] <- sim['Z1_delta', i - 1]
# } 

########

#1.5 Higher propensity to consume green
if (shock == 5 && i == para['t.shock']) para[paste0('Z1_zeta-', 1 : 3)] <- para[paste0('Z1_zeta-', 1 : 3)] * reduction

#1.6 Lower extraction (or conversion) rate of matter
if (shock == 6 && i == 2) sim[paste0('Z1_mu_mat-', 1 : 5), para['t.shock'] : nPeriods] <- 1.5 * reduction * sim[paste0('Z1_mu_mat-', 1 : 5), para['t.shock'] : nPeriods]

#### PLE EXTENSION ####
#1.7 Lower discarding rate of socio-economic stock/ PLE of durable consumption goods
if(shock == 7 & i == 2)
{
  target.sector <- 46
  size.shock <- 0.2
  period.interval <- t.shock : (t.shock + 10)
  shock.vars <- c('delta', 'id0', 'iota_g-1', 'iota_g-46', 'zeta_dc-1', 'beta-28', 'beta-29', 'beta-41')
  
  sim['Z1_id0', period.interval] = (1 + size.shock) * sim['Z1_id0', 1]
  trp <- sim[paste0('Z1_iota_g-', 1 : K), 1] * sim['Z1_id0', t.shock]
  trp[paste0('Z1_iota_g-', target.sector)] <- trp[paste0('Z1_iota_g-', target.sector)] + size.shock * sim['Z1_id0', 1]  
  sim[paste0('Z1_iota_g-', 1 : K), period.interval] <- trp / sum(trp)
}

if (shock == 7 & i == t.shock) para['Z1_ce'] <- 1

#Reduction socioeconomic discarding rate (zeta_dc) and shift in consumption composition
if (shock == 7 & i >= (t.shock+5) & i <= (t.shock + 14)) 
{
  sim[paste0('Z1_zeta_dc-', 1 : K), i] <- sim[paste0('Z1_zeta_dc-', 1 : K), i - 1] * 0.981
  sim[paste0('Z1_beta-', 28 : 29), i] <- sim[paste0('Z1_beta-', 28 : 29), i - 1] * 0.981
  sim[paste0('Z1_beta-', 20 : 21), i] <- sim[paste0('Z1_beta-', 20 : 21), i - 1] * 0.99
  sim[paste0('Z1_beta-', 10 : 11), i] <- sim[paste0('Z1_beta-', 10 : 11), i - 1] * 0.99
  other.sectors <- setdiff(1 : K, c(10 : 11, 20 : 21, 28 : 29))
  sim[paste0('Z1_beta-', other.sectors), i] <- sim[paste0('Z1_beta-', other.sectors), i] + (1 - sum(sim[paste0('Z1_beta-', 1 : K), i])) / length(other.sectors)
  sim['Z1_delta', i] <- 0.99 * sim['Z1_delta', i - 1]
} 
if (shock == 7 & i == (t.shock + 15))
{
  sim[paste0('Z1_zeta_dc-', 1 : K), i : nPeriods] <- sim[paste0('Z1_zeta_dc-', 1 : K), i - 1]
  sim[paste0('Z1_beta-', 28 : 29), i : nPeriods] <- sim[paste0('Z1_beta-', 28 : 29), i - 1]
  sim[paste0('Z1_beta-', other.sectors), i : nPeriods] <- sim[paste0('Z1_beta-', other.sectors), i - 1]
  sim['Z1_delta', i : nPeriods] <- sim['Z1_delta', i - 1]
} 

########
#1.8 Higher renewable energy share
if (shock == 8 && i == 2) sim[paste0('Z1_eta_en-', 1 : 5), para['t.shock'] : nPeriods] <- sim[paste0('Z1_eta_en-', 1 : 5), para['t.shock'] : nPeriods] / reduction

# DIRECT CE POLICIES BY THE GOVERNMENT

#2.1- Higher government spending towards circular economy efficiency (combines 1.4 with higher government expenditure)
if (shock == 9 && i == para['t.shock']) 
{
  sim['Z1_g', para['t.shock'] : nPeriods] <- sim['Z1_g', para['t.shock'] : nPeriods] / reduction # (2 - reduction)
  para['Z1_ce'] <- 1
}

#2.2- More selective government spending towards circular economy efficiency
if (shock == 10 && i == 2) 
{
  sim['Z1_sigma-5' , para['t.shock'] : nPeriods] <- 1 - reduction
  sim[paste0('Z1_sigma-', 1 : 3), para['t.shock'] : nPeriods] <- reduction * sim[paste0('Z1_sigma-', 1 : 3), para['t.shock'] : nPeriods]
}


# INDIRECT EFFECTS ON CE FROM OTHER PRACTICES AND POLICIES
#3.1- More progressive taxation
if (shock == 21 && i == para['t.shock']) para['Z1_theta_c'] <- 1.15 * para['Z1_theta_c']