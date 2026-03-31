# model/production_scenarios_2026.R
# Purpose:
#   Pre-run scenario edits ONLY: para + initial$B.matrix (+ optional initial$pars sync)
# Contract:
#   - MUST NOT reference or modify `sim`
#   - Designed to be called from run.model (or sourced with local = environment())

production_scenarios <- function(
  para,
  initial,
  sc = NULL
) {
  # --- Pull scalars safely ---
  shock <- as.integer(para[["shock"]])
  rho <- as.numeric(para[["rho"]])
  nPeriods <- as.integer(para[["nPeriods"]])
  tshock <- as.integer(para[["t.shock"]])

  # -----------------------------------------
  # Production shocks (structural, pre-run)
  # -----------------------------------------
  if (shock > 6) {
    initial$pars['Z1_ce', 'value'] <- as.numeric(para[["Z1_ce"]])
    initial$pars['Z2_ce', 'value'] <- as.numeric(para[["Z2_ce"]])

    sc_row <- sc[sc$shock == shock, , drop = FALSE]
    from <- as.integer(sc_row$primary[[1]])
    to <- as.integer(sc_row$secondary[[1]])

    # 1) Set CE in para (mvp.model reads parms["Z1_ce"], parms["Z2_ce"])
    para[c("Z1_ce", "Z2_ce")] <- c(1, 0)

    A <- as.matrix(initial$A.matrix)
    storage.mode(A) <- "double"

    # Primary row multiplier (constant)
    initial$B.matrix[from, ] <- 1 - rho

    # Secondary row multiplier (column-specific)
    ratio <- ifelse(A[to, ] == 0, 0, A[from, ] / A[to, ])

    initial$B.matrix[to, ] <- 1 + rho * ratio
  }

  # Return mutated inputs explicitly (don’t rely on side effects)
  list(para = para, initial = initial)
}
