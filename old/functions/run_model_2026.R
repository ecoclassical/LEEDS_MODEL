run.model <- function(
  initial,
  model,
  log_file = NULL,
  log_append = TRUE,
  log_every_iter = 10,
  show_progress = TRUE,
  print_final_state = TRUE
) {
  # ---------------------------
  # Logging: console (stderr) + file
  # ---------------------------

  # Choose a default log file if none provided
  if (is.null(log_file)) {
    ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
    base_dir <- if (exists("project_root", inherits = TRUE)) {
      file.path(get("project_root", inherits = TRUE), "logs")
    } else {
      file.path(getwd(), "logs")
    }
    dir.create(base_dir, showWarnings = FALSE, recursive = TRUE)
    log_file <- file.path(base_dir, paste0("LEEDS_MODEL_run_", ts, ".log"))
  }

  log_con <- NULL
  if (!is.null(log_file) && nzchar(log_file)) {
    log_con <- file(
      log_file,
      open = if (isTRUE(log_append)) "a" else "w",
      encoding = "UTF-8"
    )
    on.exit(
      {
        try(close(log_con), silent = TRUE)
      },
      add = TRUE
    )
  }

  .log_line <- function(txt) {
    cat(txt, "\n", file = stderr()) # console
    if (!is.null(log_con)) {
      writeLines(txt, log_con, sep = "\n", useBytes = TRUE)
    } # file
    invisible(NULL)
  }

  .rule <- function(title = NULL, width = 110, ch = "─") {
    if (is.null(title) || !nzchar(title)) {
      .log_line(paste(rep(ch, width), collapse = ""))
    } else {
      pad <- max(1, width - nchar(title) - 2)
      left <- floor(pad / 2)
      right <- pad - left
      .log_line(paste0(
        paste(rep(ch, left), collapse = ""),
        " ",
        title,
        " ",
        paste(rep(ch, right), collapse = "")
      ))
    }
  }

  .section <- function(title) {
    .log_line("")
    .log_line(paste0("── ", title, " ──"))
  }

  .info <- function(txt) .log_line(paste0("ℹ ", txt))
  .ok <- function(txt) .log_line(paste0("✔ ", txt))
  .warn <- function(txt) .log_line(paste0("⚠ ", txt))
  .fail <- function(txt) .log_line(paste0("✖ ", txt))

  # ---------------------------
  # INITIALIZATION (seed objects for scenario script)
  # ---------------------------
  .rule("LEEDS_MODEL :: New simulation run")
  start_time <- Sys.time()
  .section("Initialization")

  # ---- Seed parameters + variables (scenario_selection_2026.R may mutate these) ----
  para0 <- setNames(initial$pars$value, initial$pars$label)
  para <- para0

  vars0 <- initial$vars$value
  names(vars0) <- initial$vars$label

  # ---------------------------
  # SCENARIO SELECTION (MUST run before nPeriods/A.t/B.t allocation)
  # ---------------------------
  .section("Scenario selection")

  source(
    file.path(root, "functions", "scenario_selection_2026.R"),
    local = environment()
  )

  # ---- Robust sync: support legacy scenarios that modify initial$pars instead of para ----
  # If 'para' was not changed by the scenario script but initial$pars was changed, refresh para.
  if (isTRUE(all.equal(as.numeric(para), as.numeric(para0)))) {
    para_from_initial <- setNames(initial$pars$value, initial$pars$label)
    if (!isTRUE(all.equal(as.numeric(para_from_initial), as.numeric(para0)))) {
      para <- para_from_initial
      .warn(
        "scenario_selection updated initial$pars; refreshed 'para' from initial$pars."
      )
    }
  }

  # Write para back into initial$pars so the returned scenario object is self-contained
  idx_pars <- match(names(para), initial$pars$label)
  if (anyNA(idx_pars)) {
    missing <- names(para)[is.na(idx_pars)]
    stop(
      "Internal error: 'para' has labels missing from initial$pars$label: ",
      paste(missing, collapse = ", ")
    )
  }
  initial$pars$value[idx_pars] <- as.numeric(para)

  # ---- Derive dimensions AFTER scenarios ----
  if (is.na(para["nPeriods"])) {
    stop("Parameter 'nPeriods' not found in para.")
  }
  nPeriods <- as.integer(para["nPeriods"])
  maxIter <- as.integer(para["max.iterations"])

  .info(sprintf(
    "Parameters ready: nPeriods = %d, max.iterations = %d",
    nPeriods,
    maxIter
  ))
  .info(sprintf(
    "Scenario sanity: Z1_ce = %s, Z2_ce = %s",
    format(para["Z1_ce"], trim = TRUE),
    format(para["Z2_ce"], trim = TRUE)
  ))

  # ---- Build A0 and B0 AFTER scenario changes (e.g., initial$B.matrix edits) ----
  A0 <- matrix(unlist(initial$A.matrix), nrow = K * N, ncol = K * N)
  B0 <- matrix(
    unlist(initial$B.matrix) * unlist(initial$A.matrix),
    nrow = K * N,
    ncol = K * N
  )

  A.t <- array(rep(A0, times = nPeriods), dim = c(K * N, K * N, nPeriods))
  B.t <- B0

  .info(sprintf("max |B0 - A0| at init = %.6g", max(abs(B.t - A0))))

  # ---- Variables AFTER we know nPeriods ----
  n <- length(initial$vars$label)

  # Use repeated t=1 values as initial guess for all periods
  sim <- array(
    rep(vars0, times = nPeriods),
    dim = c(n, nPeriods),
    dimnames = list(initial$vars$label, NULL)
  )

  # ---------------------------
  # MARKUP DETERMINATION (t = 1)
  # ---------------------------
  .section("Markup calibration (t = 1)")
  i <- 1

  foo <- (1 - sim[zk.lab("w"), i] / sim[zk.lab("pr"), i]) /
    (1 + sim[zk.lab("kappa"), i] * rep(sim[z.lab("delta"), i], each = K))

  sim[zk.lab("mu"), ] <- foo / colSums(A.t[,, i]) - 1
  .ok("Markup vector 'mu' calibrated at t = 1")

  # Exogenous initial settings
  sim[z.lab("g"), i] <- para[z.lab("gg0")]
  sim[z.lab("rb"), i] <- sim[z.lab("r_star"), i] + sim[z.lab("mu_b"), i]
  sim[z.lab("rm"), i] <- sim[z.lab("r_star"), i] + sim[z.lab("mu_m"), i]
  sim[z.lab("rl"), i] <- sim[z.lab("r_star"), i] + sim[z.lab("mu_l"), i]
  sim[z.lab("rh"), i] <- sim[z.lab("r_star"), i] + sim[z.lab("mu_h"), i]

  # ---------------------------
  # TIME LOOP
  # ---------------------------
  .section("Time loop")
  .info(sprintf(
    "Starting Gauss–Seidel iterations across %d additional periods",
    nPeriods - 1
  ))
  .info(sprintf(
    "Logging to: %s",
    normalizePath(log_file, winslash = "/", mustWork = FALSE)
  ))

  last.iteration <- rep(NA_integer_, nPeriods)
  consistency.error <- score.iter <- array(NA_real_, dim = c(nPeriods, maxIter))

  for (i in 2:nPeriods) {
    if (isTRUE(show_progress)) {
      pct <- round(100 * (i - 1) / max(1, (nPeriods - 1)))
      .info(sprintf(
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
        .info(sprintf("t = %d: starting iterations (max = %d)", i, maxIter))
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
        .info(sprintf(
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
        .ok(sprintf(
          "t = %d: converged at iter = %d with error = %.4g",
          i,
          iter,
          signif(error, 4)
        ))
        break
      }
    } # end iter loop

    if (!is.na(error) && error > para["consistency.threshold"]) {
      .fail(sprintf(
        "t = %d: FAILED consistency check (error = %.4g > threshold = %s). Aborting time loop.",
        i,
        signif(error, 4),
        para["consistency.threshold"]
      ))
      break
    }
  } # end time loop

  end_time <- Sys.time()
  execution_time <- end_time - start_time

  # ---------------------------
  # Completion & summary
  # ---------------------------
  .section("Simulation completed")

  if (requireNamespace("beepr", quietly = TRUE)) {
    beepr::beep(2)
  }

  .ok(sprintf(
    "Total execution time: %s %s",
    round(as.numeric(execution_time), 3),
    attr(execution_time, "units")
  ))

  last_t <- max(which(colSums(!is.na(sim)) > 0))
  .info(sprintf("Last non-NA simulated period: t = %d", last_t))

  if (isTRUE(print_final_state)) {
    .section("Final simulation state (sim[, last_t])")
    print(sim[, last_t])
  }

  .rule("LEEDS_MODEL run finished")

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
