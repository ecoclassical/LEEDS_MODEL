variable.table <<- read.csv('data/Variable_Definitions.csv')
# source('functions/auxiliary_flexible_aug12.R') # load.new.init, auxiliary labeling functions, plotting
source('functions/auxiliary_flexible_nov25.R')

# Load Models
source("functions/run_new_model_nov25.R") # function run.new.model(initial.values, model.equations)
source("functions/MVP_model_aug14.R") # model equations

# Load Initial Values
identif <- 'data/initial_state_2026.xlsx' # is the same of 'data/perfect_init_aug14_withBmatrix_PLE_CE.xlsx'
initial <- load.new.init(identif)


baseline <- run.new.model(initial, mvp.model)


# Variable Selection
selected.list <- list(
  Economic = c('n', 'c', 'ineq', 'va', 'cab', 'nf', 'gdef', 'tb', 'go'),
  Macroeconomic = c('c', 'rva', 'go', 'id'),
  External = c('cab', 'gdef', 'tb'),
  Social = c('n', 'nf', 'shp', 'shw'),
  Prices = c('pa', 'pid', 'pg', 'pim'),
  Employment = c('n', 'nf'),
  Inequality = c('ydw', 'ydc', 'ineq', 'shp', 'shw'),
  Debt.and.Wealth = c('lh', 'lf', 'v', 'k', 'b_s'),
  Ecological = c('x_mat', 'mat', 'rec', 'emis', 'wa')
)

baseline <- readRDS(file = 'data/baseline_nov25.RDS')
ce.shock <- readRDS(file = 'data/CE_shock_aug14.RDS')

init.ce <- load.new.init(
  'data/perfect_init_aug14_withBmatrix_CE_inputs_higher_costs.xlsx'
)

t.shock <- init.ce$pars['t.shock', 'value'] # shock time period
t0 <- 55 # initial time period for visualization
tf <- init.ce$pars['nPeriods', 'value'] # final time period for visualization

init.ce$pars['shock', 'value'] <- 4

# ce.shock <- run.new.model(init.ce, mvp.model)
# saveRDS(ce.shock, file = 'data/CE_shock_aug14.RDS')
ce.shock <- readRDS(file = 'data/CE_shock_aug14.RDS')

shock.title <- 'Scenario: Higher Use of Circular Economy Inputs'

ce.table <- shock.summary(
  baseline,
  shock.run = ce.shock,
  t_ = c(t.shock + 1, t.shock + 7, t.shock + 20),
  t_names = c('Immediate', 'Short.Term', 'Long.Term')
)

kable(ce.table, booktabs = T)
