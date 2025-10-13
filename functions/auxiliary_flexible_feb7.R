# variable.table <<- read.csv('flexible_/data/Variable_Definitions.csv')
# variable.table <<- read.csv(paste0(directory, 'data/Variable_Definitions.csv'))
# scenario.table <- read.csv(paste0(directory, 'data/Scenario_List.csv'), row.names = 1)
# sectors <- c('Manufacturing', 'Agriculture', 'Services', 'Waste', 'Recycling')
# countries <- c('EU', 'RoW')

#### initialization ####
# source(paste0(directory, 'functions/auxiliary_july3.R'))
# identif <- paste0(directory, 'data/newvalues1.xlsx')
# initial <- load.new.init(identif)

#### Auxiliary Functions to Label Variables ####
z.lab <- function (variable) paste0(zlabs, '_', variable)  # e.g. z.lab('c') returns c('Z1_c', 'Z2_c')
zk.lab <- function (variable) unlist(lapply(zlabs, function (z) paste0(z, '_', variable, '-', 1 : K))) # e.g. returns industry-level variables
zk.sum <- function (vec) sapply(seq_len(N), function(i) sum(vec[seq(1, by = K, length.out = N)[i]:(seq(1, by = K, length.out = N) + K - 1)[i]]))
# zk.sum <- function (vec) array(sapply(mapply(seq, cumsum(c(0, K[-length(K)])) + 1, cumsum(K), SIMPLIFY = FALSE), function (x) sum(vec[x])), dim = N, dimnames = list(zlabs)) # sums industry-level variables into country-level (i.e. from KN to N)
# zk.mean <- function (vec) array(sapply(mapply(seq, cumsum(c(0, K[-length(K)])) + 1, cumsum(K), SIMPLIFY = FALSE), function (x) mean(vec[x])), dim = N, dimnames = list(zlabs)) # sums industry-level variables into country-level (i.e. from KN to N)
zk.mean <- function (vec) sapply(seq_len(N), function(i) mean(vec[(seq(1, by = K, length.out = N))[i]:(seq(1, by = K, length.out = N) + K - 1)[i]])) # sums industry-level variables into country-level (i.e. from KN to N)
rev.zk.lab <- function (variable) unlist(lapply(rev(zlabs), function(z) paste0(z, '_', variable, '-', 1:K))) # e.g. returns reverse industry-level variables
# rev.zk.lab <- function (variable) unlist(lapply(rev(zlabs), function (z) paste0(z, '_', variable, '-', 1 : K[z]))) # e.g. returns reverse industry-level variables

#### Packages ####
library(kableExtra)
library(reshape2)
library(dplyr)
library(ggplot2)

#### function that plots variables ####
# DISPLAY THE PLOT OF A PARTICULAR CATEGORY, AGGREGATE-LEVEL
# ARGUMENTS results and category, e.g. plot.type(results, 'govt.central.bank')
plot.vars <- function (res, vars)
{
  dt <- reshape2 :: melt(res$sim, varnames = c('var', 'time')) %>% filter(grepl('Z1', var) | grepl('Z2', var)) %>% filter(!grepl('\\-', var))
  dt$region <- substr(dt$var, 1, 2)
  dt$area <- res$initial$countries[as.numeric(substr(dt$var, 2, 2))]
  dt$variable <- substring(dt$var, 4)
  dt <- dt %>% filter(variable %in% vars)
  dt$unit <- setNames(variable.table$unit, variable.table$label)[dt$variable]
  dt$name <- ifelse(dt$unit == '', 
                    paste0(setNames(variable.table$name, variable.table$label)[dt$variable], '\n(', dt$variable,')'),
                    paste0(setNames(variable.table$name, variable.table$label)[dt$variable], '\n(', dt$variable,', in ', dt$unit, ')'))
  dt$type <- setNames(variable.table$type, variable.table$label)[dt$variable]
  
  print(dt %>% ggplot(aes(x = time, y = value, color = area)) +
          geom_hline(data = filter(target.set, var.label %in% vars), mapping = aes(yintercept = value), linetype = 'dashed', linewidth = .5) +
          facet_wrap(~ name, scales = 'free') + geom_line())
}

#### SHOCK LONG ####

shock.long.new <- function(baseline, scenario)
{
  # Baseline
  res <- baseline
  dt <- reshape2 :: melt(res$simulation, varnames = c('var', 'time')) %>% filter(grepl('Z1', var) | grepl('Z2', var)) %>% filter(!grepl('\\-', var))
  dt$region <- substr(dt$var, 1, 2)
  dt$area <- res$initial$countries[as.numeric(substr(dt$var, 2, 2))]
  dt$variable <- substring(dt$var, 4)
  dt$unit <- setNames(variable.table$unit, variable.table$label)[dt$variable]
  dt$name <- ifelse(dt$unit == '', 
                    paste0(setNames(variable.table$name, variable.table$label)[dt$variable], '\n(', dt$variable,')'),
                    paste0(setNames(variable.table$name, variable.table$label)[dt$variable], '\n(', dt$variable,', in ', dt$unit, ')'))
  dt$type <- setNames(variable.table$type, variable.table$label)[dt$variable]
  dt$scenario <- 'baseline'
  dt$shock <- 0
  df <- dt
  
  # Shock
  res <- scenario
  dt <- reshape2 :: melt(res$simulation, varnames = c('var', 'time')) %>% filter(grepl('Z1', var) | grepl('Z2', var)) %>% filter(!grepl('\\-', var))
  dt$region <- substr(dt$var, 1, 2)
  dt$area <- res$initial$countries[as.numeric(substr(dt$var, 2, 2))]
  dt$variable <- substring(dt$var, 4)
  dt$unit <- setNames(variable.table$unit, variable.table$label)[dt$variable]
  dt$name <- ifelse(dt$unit == '', 
                    paste0(setNames(variable.table$name, variable.table$label)[dt$variable], '\n(', dt$variable,')'),
                    paste0(setNames(variable.table$name, variable.table$label)[dt$variable], '\n(', dt$variable,', in ', dt$unit, ')'))
  dt$type <- setNames(variable.table$type, variable.table$label)[dt$variable]
  dt$scenario <- 'shock'
  dt$shock <- 1
  
  return (rbind(df, dt))
}

#### LOAD TARGET VALUES IN WIDE AND LONG FORMAT ####
load.target <- function (target.file)
{
  target.table.0 <- openxlsx :: read.xlsx(target.file, sheet = 'Target', rowNames = TRUE) / 10000 # WIDE FORMAT
  target.set <- data.frame(area = rep(c('Z1', 'Z2'), times = dim(target.table.0)[2]),
                           var.label = rep(c('c', 'id', 'g', 'rex', 'imp', 'M_TOT_int', 'fd', 'va', 'go', 'gdef', 'debt_gdp', 'b_s'), each = 2),
                           value = unlist(target.table.0))
  target.set$var <- paste0(target.set$area, '_', target.set$var)
  target.set$type <- setNames(variable.table$type, variable.table$label)[target.set$var.label]
  target.set$variable <- setNames(variable.table$name, variable.table$label)[target.set$var.label]
  target.set$unit <- setNames(variable.table$unit, variable.table$label)[target.set$var.label]
  target.set$name <- ifelse(target.set$unit == '', 
                            paste0(target.set$variable, '\n(', target.set$var.label, ')'),
                            paste0(target.set$variable, '\n(', target.set$var.label,', in ', target.set$unit, ')'))
  # LONG FORMAT
  rownames(target.set) <- target.set$var 
  
  return(list(wide = target.table.0, long = target.set))
}

# Auxiliary Function that returns Instrument Values
return.instr.vals <- function (res)
{
  foo <- dplyr :: bind_rows(list(agg.vars = data.frame(label = sapply(instruments$var.agg, z.lab) %>% as.vector, 
                                                       value = round(res$simulation[sapply(instruments$var.agg, z.lab) %>% as.vector, 1], 4), 
                                                       row.names = NULL),
                                 ind.vars = data.frame(label = sapply(instruments$var.ind, z.lab) %>% as.vector, 
                                                       value = round(sapply(instruments$var.ind, function (x) zk.mean(res$simulation[zk.lab(x), 1])) %>% as.vector, 4), 
                                                       row.names = NULL),
                                 # ind.vars = data.frame(label = sapply(instruments$var.ind, zk.lab) %>% as.vector, 
                                 #                       value = res$simulation[sapply(instruments$var.ind, zk.lab) %>% as.vector, 1], 
                                 #                       row.names = NULL),
                                 parameter = data.frame(label = sapply(instruments$par, z.lab) %>% as.vector, 
                                                        value = round(res$initial$pars[sapply(instruments$par, z.lab) %>% as.vector, 'value'], 4), 
                                                        row.names = NULL)),
                            .id = 'item')

  return(foo)
}

# Compute Target Table and Values
compute.target <- function (sim, select.target.vars = target.vars)
{
  # select.target.vars <- c('imp', 'M_TOT_int')
  select.target.vars <- apply(expand.grid(zlabs, select.target.vars), 1, function(x) paste0(x, collapse = "_"))
  df <- target.set[ , c('var', 'value')]
  df$target <- sim$simulation[target.set$var, sim$initial$pars['nPeriods', 'value']]
  df$ratio <- df$target / df$value
  
  total.fitness <- sum((1 - df$ratio) ^ 2, na.rm = T) / sum(!is.na(df$ratio))
  fitness <- sum((1 - df[df$var %in% select.target.vars, 'ratio']) ^ 2, na.rm = T) / sum(!is.na(df[df$var %in% select.target.vars, 'ratio']))
    
  return (list(table = t(df[, -1]), total.fit = total.fitness, fitness = fitness))
}

#### function loads new initial values ####
load.new.init <- function (identif)
{
  #identif <- 'flexible_/data/initial_values_july31_jbf_corrected_aug5.xlsx'
  initial <- list(variables = list(global = openxlsx :: read.xlsx(identif, sheet = 'global.vars'),
                                   aggregate = openxlsx :: read.xlsx(identif, sheet = 'aggregate.vars'),
                                   industry = openxlsx :: read.xlsx(identif, sheet = 'industry.vars')),
                  parameters = list(global = openxlsx :: read.xlsx(identif, sheet = 'global.pars'),
                                    aggregate = openxlsx :: read.xlsx(identif, sheet = 'aggregate.pars')), 
                  A.matrix = openxlsx :: read.xlsx(identif, sheet = 'A.matrix', rowNames = TRUE),
                  B.matrix = openxlsx :: read.xlsx(identif, sheet = 'B.matrix', rowNames = TRUE))
  
  # Create Parameter Table
  init.par <- initial$parameters
  init.agg.long <- reshape2 :: melt(init.par$aggregate, variable.name = 'area')
  pars <- data.frame(label = c(init.par$global$label, 
                                    paste0(init.agg.long$area, '_', init.agg.long$label)),
                          value = c(init.par$global$value,
                                    init.agg.long$value),
                          type = c(rep(NA, times = nrow(init.par$global)),
                                   init.agg.long$type))
  rownames(pars) <- pars$label
  initial$pars <- pars
  
  # Industry and Country Labels
  if(!sum(!grepl('\\.', dimnames(initial$A.matrix)[[1]])))
  {
    foo <- stringr :: str_extract(dimnames(initial$A.matrix)[[1]], "^[^.]+")
    initial$countries <- unique(foo)
    initial$sectors <- unique(stringr :: str_extract(dimnames(initial$A.matrix)[[1]], "(?<=\\.).+"))
    initial$N <- length(unique(foo))
    initial$K <- dim(initial$A.matrix)[1] / initial$N
  }
  
  # Create Variable Table
  init.var <- initial$variables
  init.agg.long <- reshape2 :: melt(init.var$aggregate, variable.name = 'area', id.vars = c('variable', 'name', 'industry', 'dynamic', 'type'))
  init.ind.long <- reshape2 :: melt(init.var$industry, variable.name = 'industry', id.vars = c('variable', 'name', 'area', 'dynamic', 'type'))
  vars <- data.frame(label = c(init.var$global$variable, 
                               paste0(init.agg.long$area, '_', init.agg.long$variable),
                               paste0(init.ind.long$area, '_', init.ind.long$variable, '-', init.ind.long$industry)),
                     name = c(init.var$global$name,
                              init.agg.long$name,
                              init.ind.long$name), # paste0(init.ind.long$name, ', ', initial$sectors[init.ind.long$industry]))
                     value = c(init.var$global$t1, init.agg.long$value, init.ind.long$value),
                     dynamic = c(rep('endogenous', length(init.var$global$t1)), 
                                 init.agg.long$dynamic, init.ind.long$dynamic),
                     type = c(rep(NA, length(init.var$global$t1)),
                              init.agg.long$type, init.ind.long$type))
  rownames(vars) <- vars$label
  initial$vars <- vars
  initial$identif <- identif
  
  # Industry and Country Labels
  zlabs <<- initial$zlabs <- paste0('Z', 1 : initial$N) # zlabs correspond to the labels for each country using Marco's notation
  N <<- initial$N
  K <<- initial$K # K is the variable number of industries per country, in this case they are the same
  # vars <<- initial$vars # initial state vector
  
  return(initial)
}

### Save Initial Values to xlsx File
save.init <- function(res, filename)
{
  initial <- res$initial
  initial$parameters$global$value <- initial$pars[!grepl('Z', initial$pars$label), 'value']
  initial$parameters$aggregate[ , zlabs] <- initial$pars[grepl('Z', initial$pars$label), 'value']
  initial$variables$global$t1 <- initial$vars[!grepl('Z', initial$vars$label), 'value']
  initial$variables$aggregate[ , zlabs] <- initial$vars[grepl('Z', initial$vars$label) & !grepl('-', initial$vars$label), 'value']
  initial$variables$industry[ , 5 + (1 : unique(initial$K))] <- initial$vars[grepl('Z', initial$vars$label) & grepl('-', initial$vars$label), 'value']
  
  foo <- list(global.pars = initial$parameters$global,
              aggregate.pars = initial$parameters$aggregate,
              global.vars = initial$variables$global,
              aggregate.vars = initial$variables$aggregate,
              industry.vars = initial$variables$industry)
  
  wb <- openxlsx :: createWorkbook()
  for (sheet_name in names(foo)) {
    openxlsx :: addWorksheet(wb, sheet_name)
    openxlsx :: writeData(wb, sheet = sheet_name, x = foo[[sheet_name]])
  }
  
  openxlsx :: addWorksheet(wb, "A.matrix")
  openxlsx :: writeData(wb, sheet = "A.matrix", x = initial$A.matrix, rowNames = TRUE)
  openxlsx :: addWorksheet(wb, "B.matrix")
  openxlsx :: writeData(wb, sheet = "B.matrix", x = initial$B.matrix, rowNames = TRUE)
  openxlsx :: saveWorkbook(wb, file = filename, overwrite = TRUE)
}

view.shock <- function(res, shock.vars, shock.sectors)
{
  shock.labels <- paste0(rep(zlabs, each = length(shock.vars)), '_', shock.vars)
  df <- res$simulation[shock.labels, ] %>% melt(varnames = c('var', 'time'))
  df$region <- substr(df$var, 1, 2)
  df$area <- res$initial$countries[as.numeric(substr(df$var, 2, 2))]
  df$variable <- substring(df$var, 4)
  # df$unit <- setNames(variable.table$unit, variable.table$label)[df$variable]
  df$name <- setNames(res$initial$vars$name, res$initial$vars$label)[as.character(df$var)]
  df$name <- setNames(initial$vars$name, initial$vars$label)[as.character(df$var)]
  df$industry <- setNames(shock.sectors, shock.vars)[df$variable]
  df$full.name <- sapply(df$name, insert.line.break)
  df$full.name[!is.na(df$industry)] <- paste0(df$name[!is.na(df$industry)], ',\n', initial$sectors[df$industry][!is.na(df$industry)])
  df$full.name <- paste0(df$full.name, '\n(', df$variable, ')')
  
  df %>% filter(time >= t0 & time <= tf) %>% ggplot(aes(x = time, y = value, color = area)) + facet_grid(full.name ~., scales = 'free') + geom_line() + 
    geom_vline(xintercept = initial$pars['t.shock', 'value'], linetype = 'dashed', linewidth = .4) +
    theme(strip.text.y.right = element_text(angle = 0)) +
    labs(title = shock.title, subtitle = 'Shocked Variables. Vertical dashed line indicates shock time')
}

# Plot Variables in Shocks
view.vars <- function (data, viz.vars, var.label) 
{
  df <- data %>% filter(time >= t0 & time <= tf) 
  df$ref.value <- rep(filter(df, time == (t.shock - 1)) %>% select(value), times = tf - t0 + 1) %>% unlist
  df$norm.value <- df$value / df$ref.value
  
  filter(df, variable %in% viz.vars) %>% ggplot(aes(x = time, y = value, color = area, linetype = scenario)) + ylab('Normalized Value') +
    facet_grid(name ~ area, scales = 'free_y') + geom_line() + theme(strip.text.y.right = element_text(angle = 0)) +
    geom_vline(xintercept = t.shock, linetype = 'dashed', linewidth = .4) +
    labs(title = shock.title, subtitle = paste('Selected', var.label, 'Indicators. Vertical dashed line indicates shock time')) + coord_cartesian(xlim = c(t0, tf))
}

view.scaled.vars <- function (data, viz.vars, var.label) 
{
  df <- data %>% filter(time >= t0 & time <= tf) 
  df$ref.value <- rep(filter(df, time == (t.shock - 1)) %>% select(value), times = tf - t0 + 1) %>% unlist
  df$norm.value <- df$value / df$ref.value
  
  filter(df, variable %in% viz.vars) %>% ggplot(aes(x = time, y = norm.value, color = area, linetype = scenario)) + ylab('Normalized Value') +
  facet_grid(name ~ area) + geom_line() + theme(strip.text.y.right = element_text(angle = 0)) +
  geom_vline(xintercept = t.shock, linetype = 'dashed', linewidth = .4) +
  labs(title = shock.title, subtitle = paste('Selected', var.label, 'Indicators. Vertical dashed line indicates shock time')) + coord_cartesian(xlim = c(t0, tf))
}

# View Summary Table
shock.summary <- function (baseline, shock.run, t_, t_names)
{
  df <- do.call(rbind, lapply(names(selected.list), function(name) {
    data.frame(Name = name, Variable = selected.list[[name]], stringsAsFactors = FALSE)
  }))
  
  for (z in zlabs)
  {
    for (tmp in 1 : length(t_names))
    {
      df[ , paste0(t_names[tmp], '.', z)] <- shock.run$simulation[paste0(z, '_', df$Variable), t_[tmp]] / baseline$simulation[paste0(z, '_', df$Variable), t_[tmp]] - 1
    }
  }
  df[ , - c(1 : 2)] <- round(100 * df[ , - c(1 : 2)], digits = 2)
  
  return (df)
}

insert.line.break <- function(x) {
  if (nchar(x) > 50) {
    # Find the approximate middle of the string
    middle <- nchar(x) %/% 2
    # Find the nearest space to the middle
    space_position <- regexpr(" ", x, fixed = TRUE)[1]
    if (space_position < 0) return(x) # Return the original string if no space is found
    
    # Find the closest space to the middle of the string
    closest_space <- which.min(abs(gregexpr(" ", x)[[1]] - middle))
    space_index <- gregexpr(" ", x)[[1]][closest_space]
    
    # Insert line break at the space closest to the middle
    x <- sub(paste0("^(.{", space_index - 1, "})( )"), "\\1\n", x)
  }
  return(x)
}