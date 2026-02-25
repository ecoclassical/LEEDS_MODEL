# -----------------------------
# SCENARIO SELECTION
# -----------------------------

# --- Pull scalars safely ---
shock <- as.integer(para["shock"])
rho <- as.numeric(para["rho"])
nPeriods <- as.integer(para["nPeriods"])
tshock <- as.integer(para["t.shock"])

# Basic sanity
stopifnot(nPeriods >= 2, tshock >= 1, tshock <= nPeriods)
stopifnot(!is.na(shock), !is.na(rho))

# -----------------------------
# Demand-side shocks (edit sim paths from t.shock onward)
# -----------------------------

# Household Diet Shift
if (shock == 1) {
  m <- sim["Z1_beta-7", tshock] + sim["Z1_beta-8", tshock]
  sim["Z1_beta-7", tshock:nPeriods] <- (1 - rho) * m
  sim["Z1_beta-8", tshock:nPeriods] <- rho * m
}

# Household Energy Transition
if (shock == 2) {
  m <- sim["Z1_beta-31", tshock] + sim["Z1_beta-32", tshock]
  sim["Z1_beta-31", tshock:nPeriods] <- (1 - rho) * m
  sim["Z1_beta-32", tshock:nPeriods] <- rho * m
}

# -----------------------------
# Production shock (edits para and B-shares; NO sim edits here)
# -----------------------------

# Production - Wood
if (shock == 7) {
  # 1) set CE in para (this is what mvp.model reads)
  para[c("Z1_ce", "Z2_ce")] <- c(1, 0)

  # 2) keep initial$pars in sync (optional but ok)
  initial$pars$value[match("Z1_ce", initial$pars$label)] <- as.numeric(para[
    "Z1_ce"
  ])
  initial$pars$value[match("Z2_ce", initial$pars$label)] <- as.numeric(para[
    "Z2_ce"
  ])

  # 3) change B shares (safe to do on initial$B.matrix BEFORE B0/B.t is built)
  initial$B.matrix[11, ] <- rho
  initial$B.matrix[12, ] <- (1 - rho)
}
