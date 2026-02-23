run.new.model <- function(initial, model) {
  #### INITIALIZATION ####
  cli_h1("LEEDS_MODEL :: New simulation run")

  start_time <- Sys.time()
  cli_h2("Initialization")

  # Parameters
  para <<- array(
    initial$pars$value,
    dim = length(initial$pars$value),
    dimnames = list(initial$pars$label)
  )

  cli_alert_info(
    "Parameters loaded: {length(para)} total, nPeriods = {para['nPeriods']}, max.iterations = {para['max.iterations']}"
  )

  # A and B Matrices
  cli_alert_info("Building A.t and B.t matrices…")
  A.t <<- array(
    unlist(initial$A.matrix),
    dim = c(K * N, K * N, para["nPeriods"])
  )
  B.t <<- array(
    unlist(initial$B.matrix) * unlist(initial$A.matrix),
    dim = c(K * N, K * N)
  )

  # Variables
  n <- length(initial$vars$label)
  sim <- array(
    initial$vars$value,
    dim = c(n, para["nPeriods"]),
    dimnames = list(initial$vars$label, NULL)
  )

  cli_alert_info(
    "State variable array 'sim' initialized: {n} variables x {para['nPeriods']} periods"
  )

  # MARKUP DETERMINATION SO MARKET PRICES ARE UNITY
  cli_h2("Markup calibration (t = 1)")

  i <- 1
  foo <- (1 - sim[zk.lab("w"), i] / sim[zk.lab("pr"), i]) /
    (1 + sim[zk.lab("kappa"), i] * rep(sim[z.lab("delta"), i], each = K))
  sim[zk.lab("mu"), ] <- foo / colSums(A.t[,, i]) - 1

  cli_alert_success("Markup vector 'mu' calibrated at t = 1")

  # CREATE AND RUN THE MODEL
  sim[z.lab("g"), i] <- para[z.lab("gg0")]

  sim[z.lab("rb"), i] <- sim[z.lab("r_star"), i] + sim[z.lab("mu_b"), i]
  sim[z.lab("rm"), i] <- sim[z.lab("r_star"), i] + sim[z.lab("mu_m"), i]
  sim[z.lab("rl"), i] <- sim[z.lab("r_star"), i] + sim[z.lab("mu_l"), i]
  sim[z.lab("rh"), i] <- sim[z.lab("r_star"), i] + sim[z.lab("mu_h"), i]

  #### FOR LOOP ####
  cli_h2("Time loop")
  cli_alert_info(
    "Starting Gauss–Seidel iterations across {para['nPeriods'] - 1} additional periods"
  )

  last.iteration <- c()
  consistency.error <- score.iter <- array(
    NA,
    dim = c(para["nPeriods"], para["max.iterations"])
  )

  # Progress bar over time periods
  pb <- cli_progress_bar(
    "Solving time periods",
    total = para["nPeriods"] - 1,
    clear = FALSE
  )

  # Start the production at t = 2
  for (i in 2:para["nPeriods"]) {
    cli_progress_update(pb, set = i - 1)

    #### SCENARIOS ####
    source(
      paste0(directory, "functions/scenario_selection_flexible.R"),
      local = TRUE
    )

    # Define iter for converging to simultaneous solution
    x.iter <- array(
      NA,
      dim = c(length(initial$vars$label), para["max.iterations"]),
      dimnames = list(
        initial$vars$label,
        paste0("iter", 1:para["max.iterations"])
      )
    )

    # Iterative solver
    for (iter in 1:para["max.iterations"]) {
      if (iter == 1) {
        cli_alert_info(
          "t = {i}: starting iterations (max = {para['max.iterations']})"
        )
      }

      output <- model(
        t = i,
        y = c(sim[, i - 1], sim[, i]),
        parms = para,
        A.mat = A.t[,, (i - 1):i]
      )

      A.t[,, (i - 1):i] <- output$A.matrix
      x <- array(
        output$y,
        dim = c(n, 2),
        dimnames = list(initial$vars$label, NULL)
      )

      #### CONSISTENCY CHECK ####
      error <- 0.5 *
        ((x["Z1_b_cb", 2] -
          (x["Z1_b_s", 2] - sum(x[z.lab("b_s_Z1"), 2]) - x["Z1_b_b", 2]))^2 +
          sum(x[z.lab("or"), 2] - x[z.lab("or"), 1])^2)
      consistency.error[i, iter] <- error

      # CHECK CONVERGENCE
      x.iter[, iter] <- sim[, i] <- x[, 2]
      x.iter[is.infinite(x.iter[, iter]), iter] <- NA

      # Gauss-Seidel Score Function and Check Consistency
      score <- 1
      if (iter > 4) {
        score <- abs((x.iter[, iter] - x.iter[, iter - 1]) / x.iter[, iter - 1])
        score[is.na(score)] <- 0
      }
      score.iter[i, iter] <- ifelse(iter > 4, mean(score, na.rm = TRUE), NA)

      if (iter %% 10 == 0 || iter == para["max.iterations"]) {
        cli_alert_info(
          "t = {i}, iter = {iter}: mean score = {round(score.iter[i, iter], 6)}, error = {signif(error, 4)}"
        )
      }

      if (
        sum(score < para["tolerance"], na.rm = TRUE) ==
          length(initial$vars$label) &&
          error < para["consistency.threshold"]
      ) {
        last.iteration[i] <- iter
        cli_alert_success(
          "t = {i}: converged at iter = {iter} with error = {signif(error, 4)}"
        )
        break
      }
    } # end iter loop

    if (error > para["consistency.threshold"]) {
      cli_alert_danger(
        "t = {i}: FAILED consistency check (error = {signif(error, 4)} > threshold = {para['consistency.threshold']}). Aborting time loop."
      )
      break # BREAK MODEL RUN IF STOCK-FLOW INCONSISTENT
    }
  } # end time loop

  end_time <- Sys.time()
  execution_time <- end_time - start_time

  # Completion & summary
  cli_h2("Simulation completed")

  if (requireNamespace("beepr", quietly = TRUE)) {
    beepr::beep(2)
  }

  cli_alert_success(
    "Total execution time: {round(as.numeric(execution_time), 3)} {attr(execution_time, 'units')}"
  )

  # Determine last successful period
  last_t <- max(which(colSums(!is.na(sim)) > 0))
  cli_alert_info("Last non-NA simulated period: t = {last_t}")

  # Print last state vector
  cli_h2("Final simulation state (sim[, last_t])")
  final_state <- sim[, last_t]
  print(final_state)

  cli_rule(center = "LEEDS_MODEL run finished")

  return(list(
    initial = initial,
    simulation = sim,
    A.matrix = A.t,
    time = execution_time,
    last_period = last_t,
    last_iteration = last.iteration,
    consistency.error = consistency.error,
    score.iter = score.iter
  ))
}
