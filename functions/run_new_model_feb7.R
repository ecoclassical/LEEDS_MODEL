run.new.model <- function (initial, model)
{
  #### INITIALIZATION ####
  
  # Parameters
  para <<- array(initial$pars$value, 
                dim = length(initial$pars$value), 
                dimnames = list(initial$pars$label))
  
  # A and B Matrices
  A.t <<- array(unlist(initial$A.matrix), dim = c(K * N, K * N, para['nPeriods']))
  B.t <<- array(unlist(initial$B.matrix) * unlist(initial$A.matrix), dim = c(K * N, K * N))
  
  # Variables
  n <- length(initial$vars$label)
  sim <- array(initial$vars$value, dim = c(n, para['nPeriods']), dimnames = list(initial$vars$label, NULL))                                   
  
  start_time <- Sys.time()
  
  # MARKUP DETERMINATION SO MARKET PRICES ARE UNITY
  i <- 1
  foo <- (1 - sim[zk.lab('w'), i] / sim[zk.lab('pr'), i]) / (1 + sim[zk.lab('kappa'), i] * rep(sim[z.lab('delta'), i], each = K)) # (1 - w / pr) / (1 + kappa * delta)
  sim[zk.lab('mu'), ] <- foo / colSums(A.t[,,i]) - 1

  # CREATE AND RUN THE MODEL
  sim[z.lab('g'), i] <- para[z.lab('gg0')]
  
  sim[z.lab('rb'), i] <- sim[z.lab('r_star'), i] + sim[z.lab('mu_b'), i]
  sim[z.lab('rm'), i] <- sim[z.lab('r_star'), i] + sim[z.lab('mu_m'), i]  #NEW
  sim[z.lab('rl'), i] <- sim[z.lab('r_star'), i] + sim[z.lab('mu_l'), i]  #NEW
  sim[z.lab('rh'), i] <- sim[z.lab('r_star'), i] + sim[z.lab('mu_h'), i]
  
  #### FOR LOOP ####
  last.iteration <- c()
  consistency.error <- score.iter <- array(NA, dim = c(para['nPeriods'], para['max.iterations']))
  
  # Start the production at t = 2
  for (i in 2 : para['nPeriods'])
  {
    #### SCENARIOS ####
    source(paste0(directory, 'functions/scenario_selection_flexible.R'), local = TRUE)
    
    # Define iter for converging to simultaneous solution
    x.iter <- array(NA, dim = c(length(initial$vars$label), para['max.iterations']), 
                     dimnames = list(initial$vars$label, paste0('iter', 1 : para['max.iterations'])))
    
    for (iter in 1 : para['max.iterations']) 
    {
      output <- model(t = i, 
                      y = c(sim[ , i - 1], sim[ , i]), 
                      parms = para, 
                      A.mat = A.t[ ,, (i - 1) : i])
      A.t[ ,, (i - 1) : i] <- output$A.matrix
      x <- array(output$y, dim = c(n, 2), dimnames = list(initial$vars$label, NULL))
      
      #### CONSISTENCY CHECK ####
      error <- 0.5 * ((x['Z1_b_cb', 2] - (x['Z1_b_s', 2] - sum(x[z.lab('b_s_Z1'), 2]) - x['Z1_b_b', 2])) ^ 2 + sum(x[z.lab('or'), 2] - x[z.lab('or'), 1]) ^ 2)
      consistency.error[iter, i] <- error
      
      # CHECK CONVERGENCE
      # Optimize Number of Iterations
      x.iter[ , iter] <- sim[ , i] <- x[, 2]
      x.iter[is.infinite(x.iter[, iter]), iter] <- NA
    
      # Gauss-Seidel Score Function and Check Consistency
      score <- 1
      if(iter > 4)
      {
        score <- abs((x.iter[, iter] - x.iter[, iter - 1]) / x.iter[, iter - 1])
        score[is.na(score)] <- 0
      }
      score.iter[iter, i] <- ifelse(iter > 4, mean(score, na.rm = T), NA)
      
      if(sum(score < para['tolerance'], na.rm = T) == length(initial$vars$label) & error < para['consistency.threshold'])
      {
        last.iteration[i] <- iter
        # consistency.error[i] <- error
        break
      }
    } # end iter loop
    # print(c(i, last.iteration[i]))
    if (error > para['consistency.threshold']) break     # BREAK MODEL RUN IF STOCK-FLOW INCONSISTENT
 } # end time loop

  end_time <- Sys.time()
  beepr :: beep(2) ## change number for different sound at completion
  print(execution_time <- end_time - start_time)

  return(list(initial = initial, simulation = sim, A.matrix = A.t, time = execution_time))
}
