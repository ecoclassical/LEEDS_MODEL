mvp.model <- function (t, y, parms, A.mat)
{
# sim <- array(x[-1], dim = c(0.5 * length(x[-1]), 2), dimnames = list(names(x[1 + 1 : (0.5 * length(x))]), NULL))
sim <- array(y, dim = c(0.5 * length(y), 2), dimnames = list(names(y)[1 : (length(y) / 2)], NULL))
A..t <- A.mat
i <- 2

#### MATRIX OF TECHNICAL COEFFICIENTS ####
# Define technical coefficients (note: a = initial coefficients; b = target coefficients)
foo <- matrix(rep(sim[z.lab('gamma_A'), i - 1] * parms[z.lab('ce')], each = N * K ^ 2), ncol = K * N, nrow = K * N)
A <- A..t[,, i] <- A..t[,, i - 1] + foo * (B.t - A..t[,, i - 1])

A.xr.matrix <- cbind(rbind(matrix(1, ncol = K, nrow = K),  # Creates a 2N x 2N matrix of same dimensions of A
                           matrix(sim['Z2_xr', 1], ncol = K, nrow = K)), # Four block matrices: clockwise, 1s, xr2, xr1, and 1s
                     rbind(matrix(sim['Z1_xr', 1], ncol = K, nrow = K), 
                           matrix(1, ncol = K, nrow = K)))
A.xr <- A * A.xr.matrix

x.r.mat <- t(matrix(sim[z.lab('xr'), i], ncol = N, nrow = N)) # creates a matrix (1, xr1, xr2, 1) 
diag(x.r.mat) <- 1
sim[z.lab('dxr'), i] <- sim[z.lab('xr'), i] - sim[z.lab('xr'), i - 1]

# Speed of Adjustment to CE
sim[z.lab('gamma_A'), i] <- parms[z.lab('gammaA0')] + sim[z.lab('g'), i - 1] * zk.sum(sim[zk.lab('gammaA1'), i] * sim[zk.lab('sigma'), i])

#### HOUSEHOLDS ####

#Share of services in household consumption (growing) <--- JBF      
#Z1_beta[3,i] = Z1_beta[3,i-1] + Z1_beta31*Z1_ydw[,i-1]/Z1_p[3,i-1] + Z1_beta32*Z1_ydc[,i-1]/Z1_p[3,i-1]
# if(Z1_beta[3,i-1]< 1-Z1_beta10-0.01) {Z1_beta[3,i] = Z1_beta[3,i-1] + Z1_beta31*Z1_ydw[,i-1]/Z1_p[3,i-1] + Z1_beta32*Z1_ydc[,i-1]/Z1_p[3,i-1]}
# else{Z1_beta[3,i] = 1-Z1_beta10-0.01}
# 
# #Share of agricultural goods in household consumption (shrinking) 
# Z1_beta[2,i] = 1 - Z1_beta[1,i] - Z1_beta[3,i]
# 
# #Share of manufacturing goods in household consumption (constant) 
# Z1_beta[1,i] = Z1_beta10

# Total Real Consumption (ignoring inflation tax)
sim[z.lab('c'), i] <- parms[z.lab('alpha1')] * sim[z.lab('ydw'), i] + parms[z.lab('alpha2')] * sim[z.lab('ydc'), i] + parms[z.lab('alpha3')] * sim[z.lab('v'), i - 1]
if (t > 2) sim[z.lab('c'), i] <- sim[z.lab('c'), i] / sim[z.lab('pa'), i - 1]

# Disposable Income of Domestic Households
sim[z.lab('yd'), i] <- sim[z.lab('wb'), i] - sim[z.lab('t'), i] + sim[z.lab('f_b'), i] +
  sim[z.lab('rb'), i - 1] * sim[paste0(z.lab('b_s_'), zlabs), i - 1] + 
  sim[rev(z.lab('rb')), i - 1] * sim[paste0(z.lab('b_s_'), rev(zlabs)), i - 1] + 
  sim[z.lab('rm'), i - 1] * sim[z.lab('mh'), i - 1] - 
  sim[z.lab('rh'), i - 1] * sim[z.lab('lh'), i - 1] +
  sim[rev(z.lab('dxr')), i] * (sim[paste0(z.lab('b_s_'), rev(zlabs)), i - 1] + sim[paste0(z.lab('e_s_'), rev(zlabs)), i - 1])

# Auxiliary Matrix with Household Demand for Shares
e_h.matrix <- matrix(sim[apply(expand.grid(z.lab('e_h'), zlabs), 1, paste, collapse="_"), i - 1], nrow = N, ncol = N) # creates a matrix "Z(row)_e_h_Z(column)"

# Auxiliary Dividends Term
sim[z.lab('div_h1'), i] <- (1 - parms[z.lab('omega')]) * (sim[z.lab('yn'), i] - sim[z.lab('wb'), i] - sim[z.lab('af'), i] - sim[z.lab('rl'), i-1] * sim[z.lab('lf'), i-1])

# Disposable Income of Domestic Households 2
if (sum(sim[z.lab('e_s'), i - 1] == 0) > 0)
{
  sim[z.lab('yd'), i] <- sim[z.lab('yd'), i] + sim[z.lab('div_h1'), i]
} else {
  sim[z.lab('yd'), i] <- sim[z.lab('yd'), i] + (x.r.mat * e_h.matrix) %*% (sim[z.lab('div_h1'), i] / sim[z.lab('e_s'), i - 1])
}

# Disposable incomes by industry and social class
for (z in zlabs)
{
  if(sim[paste0(z, '_', 'va'), i] > 0) 
  {
    # Disposable income by industry (j=1,2,3,4,5)
    sim[paste0(z,'_yd_j-', 1 : K), i] = sim[paste0(z, '_yd'), i] * sim[paste0(z,'_va_j-', 1 : K), i] / sim[paste0(z, '_va'), i]
    
    # Disposable income of workers by industry (j=1,2,3,4,5) 
    sim[paste0(z,'_ydw_j-', 1 : K), i] = sim[paste0(z,'_wb_j-', 1 : K), i] * (1 - sim[paste0(z, '_theta_w'), i])
    
    # Disposable income of capitalists by industry
    sim[paste0(z,'_ydc_j-', 1 : K), i] = sim[paste0(z,'_yd_j-', 1 : K), i] - sim[paste0(z,'_ydw_j-', 1 : K), i]
    
    # Total disposable income of workers
    sim[paste0(z, '_ydw'), i] = sum(sim[paste0(z,'_ydw_j-', 1 : K), i])
    
    # Total disposable income of capitalists
    sim[paste0(z, '_ydc'), i] = sum(sim[paste0(z,'_ydc_j-', 1 : K), i])
    
    # Functional Inequality
    sim[paste0(z, '_ineq'), i] = abs(1 - sim[paste0(z, '_ydw'), i]  / sim[paste0(z, '_ydc'), i])
    sim[paste0(z,'_ineq_j-', 1 : K), i] <- abs(1 - sim[paste0(z,'_ydw_j-', 1 : K), i]  / sim[paste0(z,'_ydc_j-', 1 : K), i])
    
    # Shares of Disposable Income
    sim[paste0(z, '_shp'), i] <- sim[paste0(z, '_ydc'), i] / sim[paste0(z, '_yd'), i]
    sim[paste0(z, '_shw'), i] <- sim[paste0(z, '_ydw'), i] / sim[paste0(z, '_yd'), i]
    sim[paste0(z, '_shp_j-', 1 : K), i] <- sim[paste0(z,'_ydc_j-', 1 : K), i] / sim[paste0(z,'_yd_j-', 1 : K), i]
    sim[paste0(z, '_shw_j-', 1 : K), i] <- sim[paste0(z,'_ydw_j-', 1 : K), i] / sim[paste0(z,'_yd_j-', 1 : K), i]
  }
}

# Net Household Wealth
sim[z.lab('v'), i] = sim[z.lab('v'), i-1] + sim[z.lab('yd'), i] - sim[z.lab('c'), i] * sim[z.lab('pa'), i]

#### PRODUCTION FIRMS ####

# Final Demand Vector in Real Terms 
sim[zk.lab('d'), i] = sim[zk.lab('beta'), i] * rep(sim[z.lab('c'), i], each = K) + 
  sim[zk.lab('sigma'), i] * rep(sim[z.lab('g'), i], each = K) +  
  sim[zk.lab('iota'), i] * rep(sim[z.lab('id'), i], each = K) + 
  sim[zk.lab('iota_g'), i] * rep(sim[z.lab('id_g'), i], each = K) + 
  sim[unlist(lapply(rev(zlabs), function (z) paste0(z, '_eta-', 1 : K))), i] * rep(sim[z.lab('rex'), i], each = K) - 
  sim[zk.lab('eta'), i] * rep(sim[z.lab('imp'), i], each = K)

# Real Output
sim[zk.lab('x'), i] <- solve(diag(K * N) - A.t[,,i]) %*% sim[zk.lab('d'), i]

# Gross Output and Real Demand
sim[z.lab('go'), i] <- zk.sum(sim[zk.lab('x'), i])
sim[z.lab('fd'), i] <- zk.sum(sim[zk.lab('d'), i])

# Value of Gross Output
sim[z.lab('y'), i] <- zk.sum(sim[zk.lab('p'), i] * sim[zk.lab('x'), i])

# Net Income (= Total Value Added) Net of VAT and Tariffs
sim[z.lab('yn'), i] = sim[z.lab('c'), i] * sim[z.lab('pa'), i] + sim[z.lab('id'), i] * sim[z.lab('pid'), i] + sim[z.lab('id_g'), i] * sim[z.lab('pid_g'), i] + 
  sim[z.lab('g'), i] * sim[z.lab('pg'), i] + sim[z.lab('nex'), i] - sim[z.lab('nimp'), i] - sim[z.lab('vat_rev'), i] - sim[rev(z.lab('xr')), i] * sim[rev(z.lab('tar_rev')), i]

# Corporate Profit
sim[z.lab('f_f'), i] = sim[z.lab('yn'), i] - sim[z.lab('wb'), i] - sim[z.lab('af'), i] - sim[z.lab('rl'), i-1] * sim[z.lab('lf'), i-1]

# Retained Profit
sim[z.lab('f_f_u'), i] = parms[z.lab('omega')] * sim[z.lab('f_f'), i]

# Value Added by Industry: Gross Revenue (Value of Output) - Input Value
sim[zk.lab('va_j'), i] <- sim[zk.lab('p'), i] * sim[zk.lab('x'), i] - sim[zk.lab('p'), i] %*% A.xr * sim[zk.lab('x'), i]

# Total Value Added of Domestic Industry 
sim[z.lab('va'), i] <- zk.sum(sim[zk.lab('va_j'), i])

# Real Value Added by Industry: Gross Revenue (Value of Output) - Input Value
sim[zk.lab('rva_j'), i] <- sim[zk.lab('p'), i - 1] * sim[zk.lab('x'), i] - sim[zk.lab('p'), i - 1] %*% A.xr * sim[zk.lab('x'), i]

# Total Value Added of Domestic Industry 
sim[z.lab('rva'), i] <- zk.sum(sim[zk.lab('rva_j'), i])

# Growth Rate of Value Added
sim[z.lab('va_g'), i] <- sim[z.lab('rva'), i] / sim[z.lab('va'), i] - 1

# Value Added Chained Index (if i=1 Z1_va_index[,i-1]=1)
sim[z.lab('va_index'), i] <- sim[z.lab('va_index'), i - 1] * (1 + sim[z.lab('va_g'), i])

#### INVESTMENT: PURCHASE OF NEW FIXED CAPITAL ####

# Target Fixed Capital as Ratio to Outputs (alternative calculations) <--- JBF
if (t <= 3) { sim[z.lab('kt'), i] = 0
} else {sim[z.lab('kt'), i] = zk.sum(sim[zk.lab('p'), i - 1] * sim[zk.lab('kappa'), i] * sim[zk.lab('x'), i-1]) / sim[z.lab('pid'), i - 1]}

# Real Investment in Fixed Capital
sim[z.lab('id'), i] = parms[z.lab('gamma')] * (sim[z.lab('kt'), i] - sim[z.lab('k'), i-1]) + sim[z.lab('da'), i] + sim[z.lab('id_p0'), i]

# Depreciation Allowances in Real Terms 
sim[z.lab('da'), i] = sim[z.lab('delta'), i] * sim[z.lab('k'), i-1]

# Fixed Capital Stock
sim[z.lab('k'), i] = sim[z.lab('k'), i-1] + sim[z.lab('id'), i] - sim[z.lab('da'), i]

# Amortization Funds
sim[z.lab('af'), i] = sim[z.lab('da'), i] * sim[z.lab('pid'), i-1] - sim[z.lab('k'), i] * (sim[z.lab('pid'), i] - sim[z.lab('pid'), i-1])

# Corporate Demand for Bank Loans
sim[z.lab('lf'), i] = sim[z.lab('lf'), i-1] + sim[z.lab('id'), i] * sim[z.lab('pid'), i] - sim[z.lab('af'), i] - sim[z.lab('f_f_u'), i] - (sim[z.lab('e_s'), i] - sim[z.lab('e_s'), i-1]) #NEW

# Quantity of Shares Issued by Private Firms
sim[z.lab('e_s'), i] = sim[z.lab(paste0('e_h_', zlabs)), i] + sim[rev(z.lab('xr')), i] * sim[rev(z.lab(paste0('e_h_', rev(zlabs)))), i] 

# Supply of Shares to Households
sim[z.lab(paste0('e_s_', rev(zlabs))), i] = sim[z.lab(paste0('e_h_', rev(zlabs))), i] * sim[z.lab('xr'), i] #NEW

# Percentage Return Rate on Shares Issued by Private Firms
sim[z.lab('r_e'), i] = (1 - parms[z.lab('omega')]) * sim[z.lab('f_f'), i] / sim[z.lab('e_s'), i]  #NEW

# Dividends Obtained by Households
sim[z.lab('div_h1'), i] <- (1 - parms[z.lab('omega')]) * (sim[z.lab('yn'), i] - sim[z.lab('wb'), i] - sim[z.lab('af'), i] - sim[z.lab('rl'), i-1] * sim[z.lab('lf'), i-1])
if (sum(sim[z.lab('e_s'), i - 1] == 0) > 0) {
  sim[z.lab('div'), i] <- sim[z.lab('div_h1'), i]} else {
    sim[z.lab('div'), i] <- e_h.matrix %*% (sim[z.lab('div_h1'), i] / sim[z.lab('e_s'), i - 1])}

#### COMMERCIAL BANKS ####

# Supply of Bank Loans
sim[z.lab('ls'), i] = sim[z.lab('lf'), i] + sim[z.lab('lh'), i]

# Supply of Bank Deposits (on demand)
sim[z.lab('ms'), i] = sim[z.lab('mh'), i]

# Holdings of bills and demand for advances
for (z in zlabs) 
{
  if (sim[paste0(z, '_ms'), i] > sim[paste0(z, '_ls'), i]) {
    sim[paste0(z, '_b_b'), i] = sim[paste0(z, '_ms'), i] - sim[paste0(z, '_ls'), i]
    sim[paste0(z, '_a_d'), i] = 0
  } else {
    sim[paste0(z, '_a_d'), i] = sim[paste0(z, '_ls'), i] - sim[paste0(z, '_ms'), i]
    sim[paste0(z, '_b_b'), i] = 0
  }
}

# Bank Profit
sim[z.lab('f_b'), i] = sim[z.lab('rl'), i-1] * sim[z.lab('lf'), i-1] + sim[z.lab('rb'), i-1] * sim[z.lab('b_b'), i-1] - sim[z.lab('rm'), i-1] * sim[z.lab('ms'), i-1] + sim[z.lab('rh'), i-1] * sim[z.lab('lh'), i-1]

#### GOVERNMENT AND CENTRAL BANK ####

# Government Spending 
sim[z.lab('g'), i] = sim[z.lab('g'), i-1] * (1 + parms[z.lab('g_g')]) + sim[z.lab('g0'), i]

# Government INVESTMENT: PURCHASE OF NEW FIXED CAPITAL

# Government Real Investment in Fixed Capital
sim[z.lab('id_g'), i] = sim[z.lab('da_g'), i] + sim[z.lab('id0'), i]

# Government Depreciation Allowances in Real Terms 
sim[z.lab('da_g'), i] = parms[z.lab('delta_g')] * sim[z.lab('k_g'), i-1]

# Government Fixed Capital Stock
sim[z.lab('k_g'), i] = sim[z.lab('k_g'), i-1] + sim[z.lab('id_g'), i] - sim[z.lab('da_g'), i]

# Income Tax Payments
sim[z.lab('t'), i] = sim[z.lab('theta_w'), i] * sim[z.lab('wb'), i] + sim[z.lab('theta_c'), i] * (sim[z.lab('div'), i] + sim[z.lab('rb'), i-1] * sim[paste0(zlabs, '_b_s_', zlabs), i-1] + sim[z.lab('rm'), i-1] * sim[z.lab('mh'), i-1])
sim['Z1_t', i] = sim['Z1_t', i] + sim['Z1_theta_c', i] * sim['Z2_rb', i - 1] * sim['Z1_b_s_Z2', i - 1] * sim['Z2_xr', i - 1]
sim['Z2_t', i] = sim['Z2_t', i] + sim['Z2_theta_c', i] * sim['Z1_rb', i - 1] * sim['Z2_b_s_Z1', i - 1] * sim['Z2_xr', i - 1]

# Income Tax Payment by Industry
sim[zk.lab('t_j'), i] <- sim[zk.lab('va_j'), i] * rep(sim[z.lab('t'), i] / sim[z.lab('va'), i], each = K)

# Interest Payments on Bills by Industry
sim[zk.lab('int_j'), i] <- sim[zk.lab('va_j'), i] - sim[zk.lab('yd_j'), i] - sim[zk.lab('t_j'), i]

# Total VAT Revenue
sim[z.lab('vat_rev'), i] <- sim[z.lab('c'), i] * zk.sum(sim[zk.lab('p'), i] * sim[zk.lab('vat'), i] * sim[zk.lab('beta'), i] / (1 + sim[zk.lab('vat'), i]))

# Total Import Tariffs Revenue
# Z1_tar_rev[,i] = t((Z2_xr[,i] * Z2_p[,i] * Z1_tar[,i]) / (I_col[,i] + Z1_tar[,i])) %*% (Z1_eta[,i]*Z1_tot_imp[,i])
# sim[z.lab('tar_rev'), i] <- sim[rev(z.lab('xr')), i] * sim[z.lab('tot_imp'), i] * zk.sum(sim[rev.zk.lab('p'), i] * sim[zk.lab('tar'), i] * sim[zk.lab('eta'), i] / (1 + sim[zk.lab('tar'), i]))
sim[z.lab('tar_rev'), i] <- sim[rev(z.lab('xr')), i] * sim[z.lab('tot_imp'), i] * zk.sum(sim[zk.lab('tar'), i] * sim[zk.lab('eta'), i])
# zk.sum(sim[rev.zk.lab('p'), i] * sim[zk.lab('tar'), i] * sim[zk.lab('eta'), i] / (1 + sim[zk.lab('tar'), i]))

# Government Deficit #NEW
sim[z.lab('gdef'), i] = sim[z.lab('g'), i] * sim[z.lab('pg'), i] - sim[z.lab('t'), i] + sim[z.lab('rb'), i-1] * sim[z.lab('b_s'), i-1] - sim[z.lab('f_cb'), i] - sim[z.lab('vat_rev'), i] - sim[z.lab('tar_rev'), i] + sim[z.lab('id_g'), i] * sim[z.lab('pid_g'), i]

# Supply of Government Bills (Government Debt)
sim[z.lab('b_s'), i] = sim[z.lab('b_s'), i-1] + sim[z.lab('gdef'), i]

# Debt to GDP Ratio
sim[z.lab('debt_gdp'), i] <- sim[z.lab('b_s'), i] / sim[z.lab('va'), i]

# CB Advances: Supply
sim[z.lab('a_s'), i] = sim[z.lab('a_d'), i]

# Supply of Cash Money
sim[z.lab('h_s'), i] = sim[z.lab('h_h'), i] #- sim[z.lab('a_s'), i]  

# Price of Foreign Reserves 
# Z1_p_or[,i] = Z2_p_or[,i] * Z2_xr[,i]
# Z2_p_or[,i] = 1

# Supply of Bills to Households
sim[z.lab(paste0('b_s_', zlabs)), i] = sim[z.lab(paste0('b_h_', zlabs)), i] # Z1 bills to Z1, Z2 bills to Z2
sim['Z2_b_s_Z1', i] <- sim['Z2_b_h_Z1', i] * sim['Z2_xr', i] # Z1 bills to Z2; Z2 bills to Z1 relates to the XR closure

# Central Bank Profit
sim[z.lab('f_cb'), i] <- sim[z.lab('rb'), i - 1] * sim[z.lab('b_cb'), i - 1]
sim['Z1_f_cb', i] <- sim['Z1_f_cb', i] + sim['Z2_rb', i - 1] * sim['Z1_b_cb_s_Z2', i - 1] * sim['Z2_xr', i - 1] # Z1 has World Money

#Z2 bills held by Z1 central bank - Note: unnecessary equation 
sim['Z1_b_cb_d_Z2', i] <- sim['Z1_b_cb_s_Z2', i] * sim['Z2_xr', i]

# Interest rate on Z1 area deposits
sim[z.lab('rm'), i] = sim[z.lab('r_star'), i] + sim[z.lab('mu_m'), i]  #NEW

# Interest rate on Z1 area bills
sim[z.lab('rb'), i] = sim[z.lab('r_star'), i] + sim[z.lab('mu_b'), i]  #NEW

# Interest rate on Z1 area loans to firms
sim[z.lab('rl'), i] = sim[z.lab('r_star'), i] + sim[z.lab('mu_l'), i]  #NEW h and l > m and b

# Interest rate on Z1 area loans to households
sim[z.lab('rh'), i] = sim[z.lab('r_star'), i] + sim[z.lab('mu_h'), i]  #NEW

#### LABOUR MARKET ####

#Employment generated by domestic final demand
sim[z.lab('n'), i] <- zk.sum(sim[zk.lab('x'), i] / sim[zk.lab('pr'), i])

#Employment generated by domestic final demand by domestic (rows 1 to 5) and foreign (rows 6 to 10) industry
sim[zk.lab('n_j'), i] <- sim[zk.lab('x'), i] / sim[zk.lab('pr'), i]

#Female employment by domestic industry <--- JBF
sim[zk.lab('nf_j'), i] <- sim[zk.lab('n_j'), i] * sim[zk.lab('rho'), i]
sim[z.lab('nf'), i] <- zk.sum(sim[zk.lab('nf_j'), i])

# Wage Bill by Industry
sim[zk.lab('wb_j'), i] <- sim[zk.lab('w'), i] * sim[zk.lab('n_j'), i] 

# Total Wage Bill (Before Tax)
sim[z.lab('wb'), i] <- zk.sum(sim[zk.lab('wb_j'), i])

#### PORTFOLIO CHOICES OF DOMESTIC HOUSEHOLDS ####

# Domestic Households Demand for Bills
sim['Z1_b_h_Z1', i] <- parms['Z1_lambda10'] * sim['Z1_v', i] +  # Z1's domestic households demand for Z1 bills
  parms['Z1_lambda11'] * sim['Z1_rb', i-1] * sim['Z1_v', i] - 
  parms['Z1_lambda12'] * (sim['Z2_rb', i-1] + (sim['Z2_xr', i] - sim['Z2_xr', i-1]) / sim['Z2_xr', i-1]) * sim['Z1_v', i] - 
  parms['Z1_lambda13'] * sim['Z1_rm', i-1] * sim['Z1_v', i] - 
  parms['Z1_lambda14'] * sim['Z1_yd', i]

sim['Z1_b_h_Z2', i] <- parms['Z1_lambda20'] * sim['Z1_v', i] -  # Z1's domestic households demand for Z2 bills
  parms['Z1_lambda21'] * sim['Z1_rb', i-1] * sim['Z1_v', i] + 
  parms['Z1_lambda22'] * (sim['Z2_rb', i-1] + (sim['Z2_xr', i] - sim['Z2_xr', i-1]) / sim['Z2_xr', i-1]) * sim['Z1_v', i] - 
  parms['Z1_lambda23'] * sim['Z1_rm', i-1] * sim['Z1_v', i] - 
  parms['Z1_lambda24'] * sim['Z1_yd', i]

sim['Z2_b_h_Z2', i] <- parms['Z2_lambda10'] * sim['Z2_v', i] -  # Z2's domestic households demand for Z2 bills
  parms['Z2_lambda11'] * (sim['Z1_rb', i-1] + (sim['Z1_xr', i] - sim['Z1_xr', i-1]) / sim['Z1_xr', i-1]) * sim['Z2_v', i] + 
  parms['Z2_lambda12'] * sim['Z2_rb', i-1] * sim['Z2_v', i] - 
  parms['Z2_lambda13'] * sim['Z2_rm', i-1] * sim['Z2_v', i] - 
  parms['Z2_lambda14'] * sim['Z2_yd', i]

sim['Z2_b_h_Z1', i] <- parms['Z2_lambda20'] * sim['Z2_v', i] +  # Z2's domestic households demand for Z1 bills
  parms['Z2_lambda21'] * (sim['Z1_rb', i-1] + (sim['Z1_xr', i] - sim['Z1_xr', i-1]) / sim['Z1_xr', i-1]) * sim['Z2_v', i] - 
  parms['Z2_lambda22'] * sim['Z2_rb', i-1] * sim['Z2_v', i] - 
  parms['Z2_lambda23'] * sim['Z2_rm', i-1] * sim['Z2_v', i] - 
  parms['Z2_lambda24'] * sim['Z2_yd', i]

# Domestic Household Demand for Shares (to be expanded including other parmsmeters)
sim['Z1_e_h_Z1', i] <- parms['Z1_lambda30'] * sim['Z1_v', i]   # Z1's demand for Z1 shares
sim['Z1_e_h_Z2', i] <- parms['Z1_lambda40'] * sim['Z1_v', i]   # Z1's demand for Z2 shares
sim['Z2_e_h_Z1', i] <- parms['Z2_lambda30'] * sim['Z2_v', i]   # Z2's demand for Z1 shares
sim['Z2_e_h_Z2', i] <- parms['Z2_lambda40'] * sim['Z2_v', i]   # Z2's demand for Z2 shares

# Cash Held by Domestic Households
sim[z.lab('h_h'), i] <- parms[z.lab('lambdac')] * sim[z.lab('c'), i-1] * sim[z.lab('pa'), i-1]

# Domestic Households Demand for Deposits
sim[z.lab('mh'), i] <- sim[z.lab('v'), i] - sim[paste0(z.lab('b_h_'), zlabs), i] - sim[z.lab('h_h'), i] - 
  sim[paste0(z.lab('b_h_'), rev(zlabs)), i] - sim[z.lab('e_h_Z1'), i] - sim[z.lab('e_h_Z2'), i] + sim[z.lab('lh'), i]

# Households demand for personal loans #NEW --- CHANGED ON 10/08/2023
for (z in zlabs)
{
  sim[paste0(z, '_lh'), i] <- sim[paste0(z, '_lh'), i-1] * (1 - parms[paste0(z, '_rep')]) + 
    max(sim[paste0(z, '_c'), i] * sim[paste0(z, '_pa'), i] - sim[paste0(z, '_yd'), i], 
        parms[paste0(z, '_psi')] * (sum(sim[paste0(z, '_p-', 1 : K), i] * sim[paste0(z, '_dc-', 1 : K), i]) - 
                                     sum(sim[paste0(z, '_p-', 1 : K), i - 1] * sim[paste0(z, '_dc-', 1 : K), i - 1])))
}

#### PRICES AND PRODUCTION FUNCTION ####

# Reproduction prices including capital depreciation *****
# p[i,] <- ((A[i,] %*% p) * (1 + mu[i,]) + w / pr[i]) / (1 - A[i,i] * (1 + mu[i,]))
p.t.a.aux <-  sim[zk.lab('p_t'), i] %*% A.xr - sim[zk.lab('p_t'), i] * diag(A) # this is the first term of the product subtracting the diagonal terms
p.t.a.aux <- p.t.a.aux * (1 + sim[zk.lab('mu'), i]) * (1 + sim[zk.lab('kappa'), i] * rep(sim[z.lab('delta'), i], each = K)) + sim[zk.lab('w'), i] / sim[zk.lab('pr'), i]
sim[zk.lab('p_t'), i] <- p.t.a.aux / (1 - diag(A) * (1 + sim[zk.lab('mu'), i]))

# Vector of domestic potential outputs (note: matter and energy to be introduced later on)  
sim[zk.lab('x_star'), i] <- sim[zk.lab('pr'), i] * sim[zk.lab('pop_j'), i]
# Vector of market prices including VAT # --- CHANGED ON 11/08/2023  <--- JBF
# Z1_p[,i] = (Z1_p_t[,i] + Z1_gamma_x[,i]*( Z1_x[,i-1] - Z1_x_star[,i-1]))*(1+Z1_vat[,i]+Z2_tar[,i])
# (Z2_p_t[,i] + Z2_gamma_x[,i]*( Z2_x[,i-1] - Z2_x_star[,i-1]))*(1+Z2_vat[,i]+Z1_tar[,i])
# sim[zk.lab('p'), i] <- (sim[zk.lab('p_t'), i] + sim[zk.lab('gamma_x'), i] * (sim[zk.lab('x'), i] - sim[zk.lab('x_star'), i])) * (1 + sim[zk.lab('vat'), i] + sim[zk.lab('tar'), i]) 
sim[zk.lab('p'), i] <- (sim[zk.lab('p_t'), i] + sim[zk.lab('gamma_x'), i] * (sim[zk.lab('x'), i] - sim[zk.lab('x_star'), i - 1])) * (1 + sim[zk.lab('vat'), i])

# Average Price of Domestic Consumption
sim[z.lab('pa'), i] <- zk.sum(sim[zk.lab('p'), i] * sim[zk.lab('beta'), i])

# Average Price of Investment
sim[z.lab('pid'), i] <- zk.sum(sim[zk.lab('p'), i] * sim[zk.lab('iota'), i])

# Average Price of Government Investment
sim[z.lab('pid_g'), i] <- zk.sum(sim[zk.lab('p'), i] * sim[zk.lab('iota_g'), i])

# Average Price of Government Spending 
sim[z.lab('pg'), i] <- zk.sum(sim[zk.lab('p'), i] * sim[zk.lab('sigma'), i])

# Average Price of Import of Area 1 #NEW ----- CHECK TARIFFS!
sim[z.lab('pim'), i] <- zk.sum(sim[rev.zk.lab('p'), i] * (1 + sim[rev.zk.lab('tar'), i] - sim[zk.lab('vat'), i]) * sim[zk.lab('eta'), i]) * sim[rev(z.lab('xr')), i]
# sim[z.lab('pim'), i] <- zk.sum(sim[rev.zk.lab('p'), i] * (1 + sim[rev.zk.lab('tar'), i]) * sim[zk.lab('eta'), i]) * sim[rev(z.lab('xr')), i]

#### ECOLOGICAL EQUATIONS: ENERGY, RESOURCES, WASTE AND EMISSIONS ####

# Waste Stock Net of Recycling <--- JBF
sim[zk.lab('was_j'), i] <- sim[zk.lab('wa_j'), i - 1] + sim[zk.lab('x'), i] * sim[zk.lab('zeta'), i] - sim[zk.lab('x'), i] * colSums(A[seq(1, N) * K, ]) # (A[5, ] + A[10, ])
sim[zk.lab('was_j')[cumsum(rep(K, times = N))], i] <- 0

# Total Domestic Waste (Stock) Net of Recycling
sim[z.lab('was'), i] <- zk.sum(sim[zk.lab('was_j'), i])

# Waste Flow Net of Recycling <--- JBF
sim[zk.lab('wa_j'), i] <- sim[zk.lab('x'), i] * sim[zk.lab('zeta'), i] - sim[zk.lab('x'), i] * colSums(A[seq(1, N) * K, ]) # (A[5, ] + A[10, ])
sim[zk.lab('wa_j')[cumsum(rep(K, times = N))], i] <- 0

# Total Domestic Waste Flow Net of Recycling
sim[z.lab('wa'), i] <- zk.sum(sim[zk.lab('wa_j'), i])

# Annual CO2 Emissions by domestic industry <--- JBF
sim[zk.lab('emis_j'), i] <- sim[zk.lab('x'), i] * sim[zk.lab('eps'), i] * rep(parms[z.lab('beta_e')], each = K)

# Annual CO2 Emissions of the Domestic Economy --- CHANGED ON 08/08/2023 <--- JBF
sim[z.lab('emis'), i] <- zk.sum(sim[zk.lab('x'), i] * sim[zk.lab('eps'), i] * (1 - sim[zk.lab('eta_en'), i])) * parms[z.lab('beta_e')]

# C02 concentration using alternative method: Cumulative CO2 Emissions
sim[z.lab('co2_cum'), i] <- sim[z.lab('co2_cum'), i - 1] + sim[z.lab('emis'), i]

# Extraction of Matter
sim[z.lab('x_mat'), i] <- zk.sum(sim[zk.lab('x'), i] * sim[zk.lab('mu_mat'), i])  # Production of Material Goods
sim[z.lab('mat'), i] <- sim[z.lab('x_mat'), i] - sim[z.lab('rec'), i]             # Extraction of Matter 
sim[z.lab('rec'), i] <- sim[z.lab('rho_dis'), i] * sim[z.lab('dis'), i] +         # Recycled Matter (of Socioeconomic Stock + Industrial Waste)
  sim[z.lab('mu_mat-5'), i] * sim[z.lab('x-5'), i]                                
sim[z.lab('dis'), i] <- zk.sum(sim[zk.lab('mu_mat'), i] *                         # Discarded Socioeconomic Stock
                                 sim[zk.lab('zeta_dc'), i - 1] * sim[zk.lab('dc'), i - 1]) 
sim[zk.lab('dc'), i] <- sim[zk.lab('dc'), i - 1] +                                # Stock of Durable Consumption Goods
  sim[zk.lab('beta'), i] * rep(sim[z.lab('c'), i], each = K) - sim[zk.lab('zeta_dc'), i - 1] * sim[zk.lab('dc'), i - 1]
# sim[z.lab('kh'), i] <- sim[z.lab('kh'), i - 1] + sim[z.lab('x_mat'), i] -         # Socioeconomic Stock
#   sim[z.lab('dis'), i] 

# Use of Energy
sim[z.lab('en'), i] <- zk.sum(sim[zk.lab('eps_en'), i] * sim[zk.lab('x'), i])    # Energy Required for production
sim[z.lab('ren'), i] <- zk.sum(sim[zk.lab('eps_en'), i] *                        # Renewable Energy at the End of the Period
                                 sim[zk.lab('eta_en'), i] * sim[zk.lab('x'), i]) 
sim[z.lab('nen'), i] <- sim[z.lab('en'), i] - sim[z.lab('ren'), i]               # Non-Renewable Energy             

# Global Use and Depletion of Matter and Energy
sim['conv_mat', i] <- parms['sigma_mat'] * sim['res_mat', i]                      # Material Resources Converted to Reserves
sim['kmat', i] <- sim['kmat', i - 1] + sim['conv_mat', i] -                      # Global Stock of Material Reserves
  sum(sim[z.lab('mat'), i])
sim['res_mat', i] <- sim['res_mat', i - 1] - sim['conv_mat', i]                  # Global Stock of Material Resources
sim['conv_en', i] <- parms['sigma_en'] * sim['res_en', i]                         # Energy Resources Converted to Reserves
sim['ken', i] <- sim['ken', i - 1] - sim['conv_en', i] -                         # Stock of Energy Reserves
  sum(sim[z.lab('nen'), i])
sim['res_en', i] <- sim['res_en', i - 1] - sim['conv_en', i]                     # Stock of Energy Resources

# Atmospheric Temperature --- CHANGED ON 01/08/2023
sim['temp', i] <- parms['tcre'] * sum(sim[z.lab('co2_cum'), i]) / (1 - parms['fnc'])

#### BALANCE OF PAYMENTS ####

# Real Intermediate Imports from Z2 --- ADDED ON 08/09/2023 (JBF_OVC) <--- JBF
for (z in 1 : length(zlabs))
{
  foo <- c(0, cumsum(rep(K, times = N)))[z] + 1 : K # indices for the matrix and the vector 1:5, 6:10, etc
  sim[paste0('Z', z, '_M_TOT_int'), i] <- sum(A[, foo] %*% sim[zk.lab('x'), i][foo]) - sum(A[foo, foo] %*% sim[zk.lab('x'), i][foo])
}

# Real Final Imports
if (t < 10) {
  sim[z.lab('imp'), i] <- ifelse(sim[z.lab('yd'), i-1], exp(parms[z.lab('mu0')] + parms[z.lab('mu2')] * log(sim[z.lab('yd'), i-1])), 0)
} else {
  sim[z.lab('imp'), i] = exp(parms[z.lab('mu0')] - parms[z.lab('mu1')] * (log(sim[z.lab('pim'), i-1]) - log(sim[z.lab('pa'), i-1])) + parms[z.lab('mu2')] * log(sim[z.lab('yd'), i-1] / sim[z.lab('pa'), i-1]))
}

# Real TOTAL Imports
sim[z.lab('tot_imp'), i] = sim[z.lab('imp'), i] + sim[z.lab('M_TOT_int'), i]

# Nominal Imports
sim[z.lab('nimp'), i] = sim[z.lab('pim'), i] * sim[z.lab('tot_imp'), i]

# Real Exports # or should be tot_imp?
sim[z.lab('rex'), i] <- sim[rev(z.lab('imp')), i]

# Nominal Exports
sim[z.lab('nex'), i] <- rev(sim[z.lab('nimp'), i] * sim[z.lab('xr'), i])

# Trade Balance
sim[z.lab('tb'), i] = sim[z.lab('nex'), i] - sim[z.lab('nimp'), i]

# Current Account Balance
sim[z.lab('cab'), i] <- sim[z.lab('tb'), i] + 
  sim[rev(z.lab('rb')), i - 1] * sim[paste0(z.lab('b_s'), '_', rev(zlabs)), i - 1] * sim[rev(z.lab('xr')), i - 1] -
  sim[z.lab('rb'), i - 1] * sim[paste0(rev(z.lab('b_s')), '_', zlabs), i - 1] + 
  c(1, -1) * sim['Z2_rb', i - 1] * sim['Z1_b_cb_s_Z2', i - 1] * c(sim['Z2_xr', i - 1], 1)
if (!sum(sim[z.lab('e_s'), i - 1] == 0)) 
{
  sim[z.lab('cab'), i] <- sim[z.lab('cab'), i] + 
    sim[rev(z.lab('xr')), i] * (1 - parms[rev(z.lab('omega'))]) * sim[rev(z.lab('f_f')), i] * sim[paste0(z.lab('e_s'), '_', rev(zlabs)), i - 1] / sim[rev(z.lab('e_s')), i - 1] -
    (1 - parms[z.lab('omega')]) * sim[z.lab('f_f'), i] * sim[paste0(rev(z.lab('e_s')), '_', zlabs), i - 1] / sim[z.lab('e_s'), i - 1]
}    

# Financial Account Balance, Net of Official Transactions
sim[z.lab('kabp'), i] <- sim[paste0(rev(z.lab('b_s')), '_', zlabs), i] - sim[paste0(rev(z.lab('b_s')), '_', zlabs), i - 1] -
  (sim[paste0(z.lab('b_s'), '_', rev(zlabs)), i] - sim[paste0(z.lab('b_s'), '_', rev(zlabs)), i - 1]) * sim[rev(z.lab('xr')), i] +
  sim[paste0(rev(z.lab('e_s')), '_', zlabs), i] - sim[paste0(rev(z.lab('e_s')), '_', zlabs), i - 1] -
  (sim[paste0(z.lab('e_s'), '_', rev(zlabs)), i] - sim[paste0(z.lab('e_s'), '_', rev(zlabs)), i - 1]) * sim[rev(z.lab('xr')), i]

# Net Accumulation of Financial Assets (NAFA)
sim[z.lab('nafa'), i] <- sim[z.lab('gdef'), i] + sim[z.lab('cab'), i]

# Quantity of Reserves
# sim['Z2_or', i] <- 0
# sim['Z1_or', i] <- sim['Z2_or', i]

#### EXCHANGE RATE CLOSURES ####
# Note 1: equation numbers are taken from Godley & Lavoie (2007, pp. 460-464)
# Note 2: we must consider also bills held by commercial banks

# Exchange rate: value of Z2 currency in terms of Z1
sim['Z2_xr', i] <- 1 / sim['Z1_xr', i]

#Z1 CB holdings of bills ------------------------------------ 12.84 --- CHANGED ON 04/08/2023 (ADVANCES ADDED)
sim['Z1_b_cb', i] <- sim['Z1_b_cb', i - 1] + (sim['Z1_h_s', i] - sim['Z1_h_s', i - 1]) - (sim['Z1_a_s', i] - sim['Z1_a_s', i - 1]) -
  (sim['Z1_b_cb_s_Z2', i] - sim['Z1_b_cb_s_Z2', i - 1]) * sim['Z2_xr', i] - (sim['Z1_or', i] - sim['Z1_or', i - 1]) * sim['Z1_p_or', i]

# Z2 CB holdings of bills ------------------------------------ 12.83 --- CHANGED ON 04/08/2023 (ADVANCES ADDED)
sim['Z2_b_cb', i] <- sim['Z2_h_s', i] - sim['Z2_or', i] * sim['Z2_p_or', i] - sim['Z2_a_s', i]

#### 1) FIXED EXCHANGE RATE CLOSURE --- CHANGED ON 01/08/2023 ####

# Supply of Z2 bills to Z1's households --------------------------------------------------- 12.89F
sim['Z1_b_s_Z2', i] <- sim['Z1_b_h_Z2', i] * sim['Z1_xr', i]

# Supply of Z2 bills to Z1 central bank --------------------------------------------------- 12.90F
sim['Z1_b_cb_s_Z2', i] = sim['Z2_b_s', i] - sim['Z1_b_s_Z2', i] - sim['Z2_b_s_Z2', i] - sim['Z2_b_cb', i] - sim['Z2_b_b', i]

# Exchange rate: value of Z1 currency in terms of Z2 (exogenous) -------------------------- 12.91F
sim['Z1_xr', i] <- sim['Z1_xr', i - 1]

#### 2) FLOATING EXCHANGE RATE CLOSURE ####

# # Exchange rate: value of Z1 currency in terms of Z2 (endogenous) ------------------------ 12.89FL
# sim['Z1_xr', i] = sim['Z1_b_s_Z2', i] / sim['Z1_b_h_Z2', i]
# 
# # #Supply of Z2 bills to Z1's households -------------------------------------------------- 12.90FL
# sim['Z1_b_s_Z2', i] = sim['Z2_b_s', i] - sim['Z2_b_s_Z2', i] - sim['Z2_b_cb', i] - sim['Z2_b_b', i]
# 
# # #Supply of Z2 bills to Z1 central bank -------------------------------------------------- 12.91FL
# sim['Z1_b_cb_s_Z2', i] = sim['Z1_b_cb_s_Z2', i - 1]
# 
# # Z1 CB holdings of bills ---------------------------------------------------------------- 12.84FL
# sim['Z1_b_cb', i] = sim['Z1_b_cb', i-1] + (sim['Z1_h_s', i] - sim['Z1_h_s', i-1])

#### 3) MIXED REGIME: QUASI-FLOATING --- CHANGED ON 01/08/2023 ####
# if (t>75 && (shock==3 || shock==5) ){
# 
# #Exchange rate: value of Z1 currency in terms of Z2 (endogenous)
# Z1_xr[,i] = Z1_xr[,i-1] + par_xr*Z1_cab[,i-1]/Z1_yn[,i-1]
# 
# } 
# 

# Hidden equation used for consistency check --------------------------------------------- 12.82A
# sim['Z1_b_cb', i] <- sim['Z1_b_s', i] - sum(sim[z.lab('b_s_Z1'), i]) - sim['Z1_b_b', i] 

#### LABOUR FORCE AND IMMIGRATION EQUATIONS ####
if (t <= 75) {sim[zk.lab('pop_j'), i] <- sim[zk.lab('n_j'), i]
} else {sim[zk.lab('pop_j'), i] <- sim[zk.lab('pop_j'), i - 1] * (1 + sim[zk.lab('g_pop'), i]) + sim[zk.lab('imm'), i] -  sim[rev.zk.lab('imm'), i]}

# Unemployment Rate by Industry
sim[zk.lab('un'), i] <- 1 - sim[zk.lab('n_j'), i] / sim[zk.lab('pop_j'), i]

# Immigration
sim[zk.lab('imm'), i] <- sim[zk.lab('gamma_imm_0'), i] * sim[zk.lab('pop_j'), i - 1] + 
  sim[zk.lab('gamma_imm_1'), i] * sim[zk.lab('un'), i - 1] +
  sim[zk.lab('gamma_imm_2'), i] * (sim[zk.lab('w'), i] - sim[zk.lab('w'), i - 1])

# Female Employment Share by Industry
sim[zk.lab('rho'), i] <- sim[zk.lab('parw0'), i] - sim[zk.lab('parw1'), i] * (sim[zk.lab('w'), i] - sim[zk.lab('w'), i - 1])

#### CONSISTENCY CHECK ####
error <- 0.5 * ((sim['Z1_b_cb', i] - (sim['Z1_b_s', i] - sum(sim[z.lab('b_s_Z1'), i]) - sim['Z1_b_b', i])) ^ 2 + sum(sim[z.lab('or'), i] - sim[z.lab('or'), i - 1]) ^ 2)

return(list(y = c(sim[, 1], sim[, 2]), A.matrix = A..t))
# return(c(sim[, 1], sim[, 2]))
}