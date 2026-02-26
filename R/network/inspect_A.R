A_slice <- function(run, t) run$A.matrix[,, t]

shock14 <- scenario

t0 <- baseline$initial$pars["t.shock", "value"] # or the one you used
Ab <- A_slice(baseline, t0)
As <- A_slice(scenario, t0)

DeltaA <- As - Ab

# global magnitude
fro <- function(M) sqrt(sum(M^2, na.rm = TRUE))
fro(DeltaA) / fro(Ab)

I <- diag(nrow(Ab))
Lb <- solve(I - Ab)
Ls <- solve(I - As)

fro(Ls - Lb) / fro(Lb)

t_shock <- as.integer(baseline$initial$pars["t.shock", "value"])
for (tt in c(t_shock - 1, t_shock, t_shock + 1, t_shock + 5, t_shock + 10)) {
  Ab <- baseline$A.matrix[,, tt]
  As <- shock14$A.matrix[,, tt]
  cat(tt, fro(As - Ab) / fro(Ab), "\n")
}

t_last <- baseline$last_period
vars <- c(
  "go",
  "va",
  "c",
  "id",
  "tb",
  "cab",
  "emis",
  "mat",
  "x_mat",
  "land",
  "water"
)

logger$info(sprintf(
  "APPLY scenario %d: rho=%s, operator=%s, from=%s, to=%s",
  shock_id,
  sc_row$rho[[1]],
  sc_row$operator[[1]],
  sc_row$from[[1]],
  sc_row$to[[1]]
))
