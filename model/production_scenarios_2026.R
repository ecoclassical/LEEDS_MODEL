# model/production_scenarios_2026.R
# Purpose:
#   Pre-run scenario edits ONLY: para + initial$B.matrix (+ optional initial$pars sync)
# Contract:
#   - MUST NOT reference or modify `sim`
#   - Intended to be sourced inside run.model with local = environment()

# --- Pull scalars safely ---
shock <- as.integer(para["shock"])
rho <- as.numeric(para["rho"])
nPeriods <- as.integer(para["nPeriods"])
tshock <- as.integer(para["t.shock"])

# Basic sanity
stopifnot(!is.na(shock), !is.na(rho))
stopifnot(!is.na(nPeriods), nPeriods >= 2)
stopifnot(!is.na(tshock), tshock >= 1, tshock <= nPeriods)

# -----------------------------------------
# Production shocks (structural, pre-run)
# -----------------------------------------

# Production - Wood
if (shock == 7) {
  # 1) Set CE in para (mvp.model reads parms["Z1_ce"], parms["Z2_ce"])
  para[c("Z1_ce", "Z2_ce")] <- c(1, 0)

  # 2) Optional sync to initial$pars to avoid later confusion
  idx1 <- match("Z1_ce", initial$pars$label)
  idx2 <- match("Z2_ce", initial$pars$label)
  if (is.na(idx1) || is.na(idx2)) {
    stop(
      "production_scenarios: could not find Z1_ce/Z2_ce in initial$pars$label"
    )
  }
  initial$pars$value[idx1] <- as.numeric(para["Z1_ce"])
  initial$pars$value[idx2] <- as.numeric(para["Z2_ce"])

  # 3) B-share edits (safe here; B0 will be built AFTER this)
  #    You are editing rows 11 and 12 across all columns (as in your current approach).
  initial$B.matrix[11, ] <- rho
  initial$B.matrix[12, ] <- (1 - rho)
}

# Add other production shocks here (4,5,...), always respecting:
# - only para + initial$B.matrix edits
# - no sim edits
