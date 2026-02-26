run.model <- function(
  initial,
  model,
  sc = NULL,
  log_file = NULL,
  log_append = TRUE,
  log_every_iter = 25,
  show_progress = TRUE,
  print_final_state = TRUE,
  log_context = NULL
) {
  # ---------------------------
  # Logging: console (stderr) + file
  # ---------------------------

  logger <- create_logger(log_file, log_append, context = log_context)

  on.exit(logger$close(), add = TRUE)

  start_time <- Sys.time()
  logger$rule("LEEDS_MODEL :: New simulation run")
  # ---------------------------
  # INITIALIZATION (core objects)
  # ---------------------------

  logger$section("Initialization")

  # ---- Seed parameters (named vector) ----
  para0 <- setNames(initial$pars$value, initial$pars$label)
  para <- para0

  if (is.na(para["nPeriods"])) {
    stop("Parameter 'nPeriods' not found in initial$pars.")
  }
  if (is.na(para["max.iterations"])) {
    stop("Parameter 'max.iterations' not found in initial$pars.")
  }

  logger$info("Parameters ready")

  # ---------------------------
  # PRODUCTION SCENARIOS (pre-run)
  # edits: para + initial$B.matrix (and optional initial$pars sync)
  # does NOT touch sim
  # ---------------------------
  logger$section("Production scenarios")

  out <- production_scenarios(
    para = para,
    initial = initial,
    sc = sc
  )

  para <- out$para
  initial <- out$initial

  logger$rule("Production Scenarios Loaded")

  # ---- Always write para back into initial$pars so returned object is self-contained ----
  idx_pars <- match(names(para), initial$pars$label)
  if (anyNA(idx_pars)) {
    missing <- names(para)[is.na(idx_pars)]
    stop(
      "run.model: 'para' has labels missing from initial$pars$label: ",
      paste(missing, collapse = ", ")
    )
  }
  initial$pars$value[idx_pars] <- as.numeric(para)

  # ---- Dimensions AFTER production scenarios ----
  nPeriods <- as.integer(para["nPeriods"])
  maxIter <- as.integer(para["max.iterations"])

  if (is.na(nPeriods) || nPeriods < 2) {
    stop("Invalid nPeriods after production scenarios.")
  }
  if (is.na(maxIter) || maxIter < 1) {
    stop("Invalid max.iterations after production scenarios.")
  }

  logger$info(sprintf(
    "Parameters ready: nPeriods = %d, max.iterations = %d",
    nPeriods,
    maxIter
  ))
  logger$info(sprintf(
    "Scenario sanity: shock = %s, rho = %s, t.shock = %s, Z1_ce = %s, Z2_ce = %s",
    format(para["shock"], trim = TRUE),
    format(para["rho"], trim = TRUE),
    format(para["t.shock"], trim = TRUE),
    format(para["Z1_ce"], trim = TRUE),
    format(para["Z2_ce"], trim = TRUE)
  ))

  # ---------------------------
  # Build A0/B0 AFTER production scenario edits to initial$B.matrix
  # ---------------------------
  logger$info("Building A0/B0/A.t/B.t after production scenarios…")

  A0 <- matrix(unlist(initial$A.matrix), nrow = K * N, ncol = K * N)
  B0 <- matrix(
    unlist(initial$B.matrix) * unlist(initial$A.matrix),
    nrow = K * N,
    ncol = K * N
  )

  A.t <- array(rep(A0, times = nPeriods), dim = c(K * N, K * N, nPeriods))
  B.t <- B0

  logger$info(sprintf("max |B0 - A0| at init = %.6g", max(abs(B.t - A0))))

  # ---------------------------
  # Variables: allocate sim once (NO scenario scripts touch sim anymore)
  # ---------------------------
  n <- length(initial$vars$label)
  sim <- array(
    rep(initial$vars$value, times = nPeriods),
    dim = c(n, nPeriods),
    dimnames = list(initial$vars$label, NULL)
  )

  # ---------------------------
  # MARKUP DETERMINATION (t = 1)
  # ---------------------------
  logger$section("Markup calibration (t = 1)")
  i <- 1

  foo <- (1 - sim[zk.lab("w"), i] / sim[zk.lab("pr"), i]) /
    (1 + sim[zk.lab("kappa"), i] * rep(sim[z.lab("delta"), i], each = K))

  sim[zk.lab("mu"), ] <- foo / colSums(A.t[,, i]) - 1
  logger$ok("Markup vector 'mu' calibrated at t = 1")

  # Exogenous initial settings
  sim[z.lab("g"), i] <- para[z.lab("gg0")]
  sim[z.lab("rb"), i] <- sim[z.lab("r_star"), i] + sim[z.lab("mu_b"), i]
  sim[z.lab("rm"), i] <- sim[z.lab("r_star"), i] + sim[z.lab("mu_m"), i]
  sim[z.lab("rl"), i] <- sim[z.lab("r_star"), i] + sim[z.lab("mu_l"), i]
  sim[z.lab("rh"), i] <- sim[z.lab("r_star"), i] + sim[z.lab("mu_h"), i]

  # ---------------------------
  # TIME LOOP
  # ---------------------------
  logger$section("Time loop")
  logger$info(sprintf(
    "Starting Gauss–Seidel iterations across %d additional periods",
    nPeriods - 1
  ))
  logger$info(sprintf(
    "Logging to: %s",
    normalizePath(log_file, winslash = "/", mustWork = FALSE)
  ))

  last.iteration <- rep(NA_integer_, nPeriods)
  consistency.error <- score.iter <- array(NA_real_, dim = c(nPeriods, maxIter))

  for (i in 2:nPeriods) {
    if (isTRUE(show_progress)) {
      pct <- round(100 * (i - 1) / max(1, (nPeriods - 1)))
      logger$info(sprintf(
        "Solving time periods %3d%% (t = %d / %d)",
        pct,
        i,
        nPeriods
      ))
    }

    x.iter <- array(
      NA_real_,
      dim = c(n, maxIter),
      dimnames = list(initial$vars$label, paste0("iter", 1:maxIter))
    )

    for (iter in 1:maxIter) {
      if (iter == 1) {
        logger$info(sprintf(
          "t = %d: starting iterations (max = %d)",
          i,
          maxIter
        ))
      }

      output <- model(
        t = i,
        y = c(sim[, i - 1], sim[, i]),
        parms = para,
        A.mat = A.t[,, (i - 1):i],
        B.mat = B.t
      )

      # carry forward updated A for (i-1,i)
      A.t[,, (i - 1):i] <- output$A.matrix

      x <- array(
        output$y,
        dim = c(n, 2),
        dimnames = list(initial$vars$label, NULL)
      )

      # Consistency check
      error <- 0.5 *
        ((x["Z1_b_cb", 2] -
          (x["Z1_b_s", 2] - sum(x[z.lab("b_s_Z1"), 2]) - x["Z1_b_b", 2]))^2 +
          sum(x[z.lab("or"), 2] - x[z.lab("or"), 1])^2)

      consistency.error[i, iter] <- error

      # Update sim and convergence score
      x.iter[, iter] <- sim[, i] <- x[, 2]
      x.iter[is.infinite(x.iter[, iter]), iter] <- NA_real_

      score <- 1
      if (iter > 4) {
        score <- abs((x.iter[, iter] - x.iter[, iter - 1]) / x.iter[, iter - 1])
        score[is.na(score)] <- 0
      }
      score.iter[i, iter] <- ifelse(
        iter > 4,
        mean(score, na.rm = TRUE),
        NA_real_
      )

      if (iter %% log_every_iter == 0 || iter == maxIter) {
        logger$info(sprintf(
          "t = %d, iter = %d: mean score = %s, error = %.4g",
          i,
          iter,
          format(round(score.iter[i, iter], 6), trim = TRUE),
          signif(error, 4)
        ))
      }

      if (
        sum(score < para["tolerance"], na.rm = TRUE) == n &&
          error < para["consistency.threshold"]
      ) {
        last.iteration[i] <- iter
        logger$ok(sprintf(
          "t = %d: converged at iter = %d with error = %.4g",
          i,
          iter,
          signif(error, 4)
        ))
        break
      }
    } # end iter loop

    if (!is.na(error) && error > para["consistency.threshold"]) {
      logger$fail(sprintf(
        "t = %d: FAILED consistency check (error = %.4g > threshold = %s). Aborting time loop.",
        i,
        signif(error, 4),
        para["consistency.threshold"]
      ))
      break
    }
  } # end time loop

  end_time <- Sys.time()

  if (!exists("start_time", inherits = FALSE)) {
    # Should never happen, but prevents crashes if refactoring goes wrong again
    start_time <- end_time
  }

  execution_time <- end_time - start_time

  # ---------------------------
  # Completion & summary
  # ---------------------------
  logger$section("Simulation completed")

  if (requireNamespace("beepr", quietly = TRUE)) {
    beepr::beep(2)
  }

  logger$ok(sprintf(
    "Total execution time: %s %s",
    round(as.numeric(execution_time), 3),
    attr(execution_time, "units")
  ))

  last_t <- max(which(colSums(!is.na(sim)) > 0))
  logger$info(sprintf("Last non-NA simulated period: t = %d", last_t))

  if (isTRUE(print_final_state)) {
    logger$section("Final simulation state (sim[, last_t])")
    print(sim[, last_t])
  }

  logger$rule("LEEDS_MODEL run finished")

  return(list(
    initial = initial,
    simulation = sim,
    A.matrix = A.t,
    B.matrix = B.t,
    para = para,
    time = execution_time,
    last_period = last_t,
    last_iteration = last.iteration,
    consistency.error = consistency.error,
    score.iter = score.iter,
    log_file = log_file
  ))
}
