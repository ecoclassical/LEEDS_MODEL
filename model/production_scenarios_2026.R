# model/production_scenarios_2026.R
# Purpose:
#   Pre-run scenario edits ONLY: para + initial$B.matrix (+ optional initial$pars sync)
# Contract:
#   - MUST NOT reference or modify `sim`
#   - Designed to be called from run.model (or sourced with local = environment())

production_scenarios <- function(
  para,
  initial,
  sc = NULL,
  sync_pars = TRUE
) {
  # --- Pull scalars safely ---
  shock <- as.integer(para[["shock"]])
  rho <- as.numeric(para[["rho"]])
  nPeriods <- as.integer(para[["nPeriods"]])
  tshock <- as.integer(para[["t.shock"]])

  # --- Basic sanity ---
  if (is.na(shock)) {
    stop("production_scenarios: para['shock'] is NA or missing")
  }
  if (is.na(rho)) {
    stop("production_scenarios: para['rho'] is NA or missing")
  }
  if (!is.finite(rho) || rho < 0 || rho > 1) {
    stop("production_scenarios: rho must be finite and in [0, 1]")
  }

  if (is.na(nPeriods) || nPeriods < 2) {
    stop("production_scenarios: nPeriods must be >= 2")
  }
  if (is.na(tshock) || tshock < 1 || tshock > nPeriods) {
    stop("production_scenarios: t.shock must be in [1, nPeriods]")
  }

  # --- Validate initial structure we will touch ---
  if (is.null(initial$B.matrix)) {
    stop("production_scenarios: initial$B.matrix is NULL")
  }
  if (!is.matrix(initial$B.matrix)) {
    stop("production_scenarios: initial$B.matrix must be a matrix")
  }

  if (sync_pars) {
    if (
      is.null(initial$pars) ||
        is.null(initial$pars$label) ||
        is.null(initial$pars$value)
    ) {
      stop(
        "production_scenarios: sync_pars=TRUE requires initial$pars$label and initial$pars$value"
      )
    }
  }

  # -----------------------------------------
  # Production shocks (structural, pre-run)
  # -----------------------------------------
  if (shock > 6) {
    # 1) Set CE in para (mvp.model reads parms["Z1_ce"], parms["Z2_ce"])
    para[c("Z1_ce", "Z2_ce")] <- c(1, 0)

    # 2) Optional sync to initial$pars to avoid later confusion
    if (sync_pars) {
      idx1 <- match("Z1_ce", initial$pars$label)
      idx2 <- match("Z2_ce", initial$pars$label)
      if (is.na(idx1) || is.na(idx2)) {
        stop(
          "production_scenarios: could not find Z1_ce/Z2_ce in initial$pars$label"
        )
      }
      initial$pars$value[idx1] <- as.numeric(para[["Z1_ce"]])
      initial$pars$value[idx2] <- as.numeric(para[["Z2_ce"]])
    }

    # 3) B-share edits (safe here; B0 will be built AFTER this)
    if (is.null(sc)) {
      stop(
        "production_scenarios: shock > 9 requires 'sc' mapping table (with columns: shock, from, to)"
      )
    }
    if (!all(c("shock", "from", "to") %in% names(sc))) {
      stop("production_scenarios: 'sc' must contain columns: shock, from, to")
    }

    sc_row <- sc[sc$shock == shock, , drop = FALSE]
    if (nrow(sc_row) != 1) {
      stop(sprintf(
        "production_scenarios: expected exactly 1 row in sc for shock=%s, found %s",
        shock,
        nrow(sc_row)
      ))
    }

    from <- as.integer(sc_row$from[[1]])
    to <- as.integer(sc_row$to[[1]])

    if (is.na(from) || is.na(to)) {
      stop("production_scenarios: sc$from/sc$to resolved to NA")
    }

    nR <- nrow(initial$B.matrix)
    if (from < 1 || from > nR || to < 1 || to > nR) {
      stop(sprintf(
        "production_scenarios: from/to out of bounds for B.matrix rows (nrow=%s): from=%s, to=%s",
        nR,
        from,
        to
      ))
    }

    initial$B.matrix[from, ] <- rho
    initial$B.matrix[to, ] <- 1 - rho
  }

  # Return mutated inputs explicitly (don’t rely on side effects)
  list(para = para, initial = initial)
}
