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
    # Prefer project_root/logs if available; else use working directory
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
    # Console (stderr) so Quarto/knitr won't capture into HTML
    cat(txt, "\n", file = stderr())
    # Log file
    if (!is.null(log_con)) {
      writeLines(txt, con = log_con, sep = "\n", useBytes = TRUE)
    }
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
  # INITIALIZATION
  # ---------------------------
  .rule("LEEDS_MODEL :: New simulation run")
  start_time <- Sys.time()

  .section("Initialization")

  # Variables
  n <- length(initial$vars$label)
  sim <- array(
    initial$vars$value,
    dim = c(n, para["nPeriods"]),
    dimnames = list(initial$vars$label, NULL)
  )

  # Parameters
  para <- array(
    initial$pars$value,
    dim = length(initial$pars$value),
    dimnames = list(initial$pars$label)
  )

  .info(sprintf(
    "Parameters loaded: %d total, nPeriods = %s, max.iterations = %s",
    length(para),
    para["nPeriods"],
    para["max.iterations"]
  ))

  .info(sprintf(
    "State variable array 'sim' initialized: %d variables x %s periods",
    n,
    para["nPeriods"]
  ))

  # A and B Matrices
  .info("Building A.t and B.t matrices…")
  A.t <- array(
    unlist(initial$A.matrix),
    dim = c(K * N, K * N, para["nPeriods"])
  )
  B.t <- array(
    unlist(initial$B.matrix) * unlist(initial$A.matrix),
    dim = c(K * N, K * N)
  )

  #### SCENARIOS ####
  source(
    file.path(root, "functions", "scenario_selection_2026.R"),
    local = TRUE
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

  # CREATE AND RUN THE MODEL
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
    "Starting Gauss–Seidel iterations across %s additional periods",
    para["nPeriods"] - 1
  ))
  .info(sprintf(
    "Logging to: %s",
    normalizePath(log_file, winslash = "/", mustWork = FALSE)
  ))

  last.iteration <- c()
  consistency.error <- score.iter <- array(
    NA,
    dim = c(para["nPeriods"], para["max.iterations"])
  )

  # Start the production at t = 2
  nPeriods <- as.integer(para["nPeriods"])
  maxIter <- as.integer(para["max.iterations"])

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

    # Define iter for converging to simultaneous solution
    x.iter <- array(
      NA,
      dim = c(length(initial$vars$label), maxIter),
      dimnames = list(
        initial$vars$label,
        paste0("iter", 1:maxIter)
      )
    )

    # Iterative solver
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
        sum(score < para["tolerance"], na.rm = TRUE) ==
          length(initial$vars$label) &&
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

    if (error > para["consistency.threshold"]) {
      .fail(sprintf(
        "t = %d: FAILED consistency check (error = %.4g > threshold = %s). Aborting time loop.",
        i,
        signif(error, 4),
        para["consistency.threshold"]
      ))
      break # BREAK MODEL RUN IF STOCK-FLOW INCONSISTENT
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

  # Determine last successful period
  last_t <- max(which(colSums(!is.na(sim)) > 0))
  .info(sprintf("Last non-NA simulated period: t = %d", last_t))

  # Print last state vector (to stdout; will appear in rendered document)
  if (isTRUE(print_final_state)) {
    .section("Final simulation state (sim[, last_t])")
    final_state <- sim[, last_t]
    print(final_state)
  }

  .rule("LEEDS_MODEL run finished")

  return(list(
    initial = initial,
    simulation = sim,
    A.matrix = A.t,
    time = execution_time,
    last_period = last_t,
    last_iteration = last.iteration,
    consistency.error = consistency.error,
    score.iter = score.iter,
    log_file = log_file
  ))
}
