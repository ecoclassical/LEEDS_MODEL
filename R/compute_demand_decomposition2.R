suppressPackageStartupMessages({
  library(openxlsx); library(dplyr); library(tidyr)
})
setwd("/Users/parvulesco/Documents/R/LEEDS_MODEL")

rho <- 0.2; t_sh <- 70; n_sec <- 54
A_raw    <- read.xlsx('data/initial_state_2026.xlsx', sheet='A.matrix', colNames=TRUE, rowNames=TRUE)
A        <- as.matrix(A_raw)
L        <- solve(diag(nrow(A)) - A)
baseline <- readRDS('output/scenarios/baseline_2026.RDS')
sim      <- baseline$simulation
sc       <- read.csv('data/scenarios.csv', stringsAsFactors=FALSE) %>% filter(shock != 14)

x_base <- c(
  sapply(1:n_sec, function(j) sim[paste0('Z1_x-', j), t_sh]),
  sapply(1:n_sec, function(j) sim[paste0('Z2_x-', j), t_sh])
)

K_labels <- c('HH.consumption', 'Gov.consumption', 'Firm.investment', 'Public.investment')
K_vars   <- c('Z1_c', 'Z1_g', 'Z1_id', 'Z1_id_g')
K_shares <- c('Z1_beta', 'Z1_sigma', 'Z1_iota', 'Z1_iota_g')

decompose_sector <- function(i) {
  fd_comps <- sapply(seq_along(K_vars), function(k) {
    sn <- paste0(K_shares[k], '-', i)
    if (sn %in% rownames(sim)) sim[sn, t_sh] * sim[K_vars[k], t_sh] else 0
  })
  names(fd_comps) <- K_labels
  int_demand <- sum(A[i, ] * x_base, na.rm=TRUE)
  x_i        <- as.numeric(sim[paste0('Z1_x-', i), t_sh])
  list(fd_comps=fd_comps, d_i=sum(fd_comps), int_demand=int_demand, x_i=x_i)
}

unique_sc <- sc %>% distinct(shock, sector, domain, target, from, to) %>%
             mutate(i=as.integer(from), to_i=as.integer(to))

rows <- lapply(1:nrow(unique_sc), function(r) {
  row    <- unique_sc[r,]
  d      <- decompose_sector(row$i)
  struct1 <- L[row$i, row$to_i] - L[row$i, row$i]
  actual_scale <- switch(row$target,
    beta   = unname(d$fd_comps['HH.consumption']),
    sigma  = unname(d$fd_comps['Gov.consumption']),
    iota   = unname(d$fd_comps['Firm.investment']),
    iota_g = unname(d$fd_comps['Public.investment']),
    a      = d$int_demand,
    NA_real_
  )
  data.frame(
    shock        = row$shock,
    sector       = row$sector,
    target       = row$target,
    HH_cons      = round(unname(d$fd_comps['HH.consumption']), 2),
    Gov_cons     = round(unname(d$fd_comps['Gov.consumption']), 2),
    Firm_inv     = round(unname(d$fd_comps['Firm.investment']), 2),
    Pub_inv      = round(unname(d$fd_comps['Public.investment']), 2),
    FD_total     = round(d$d_i, 2),
    INT_total    = round(d$int_demand, 2),
    x_i          = round(d$x_i, 2),
    pct_FD       = round(d$d_i / d$x_i * 100, 1),
    pct_INT      = round(d$int_demand / d$x_i * 100, 1),
    struct1      = round(struct1, 4),
    actual_scale = round(actual_scale, 2),
    DeltaM1_act  = round(rho * actual_scale * struct1, 3),
    DeltaM1_full = round(rho * d$x_i * struct1, 3),
    pct_of_full  = round(actual_scale / d$x_i * 100, 1),
    stringsAsFactors = FALSE
  )
})
rows <- do.call(rbind, rows)

cat('\n=== Gross output decomposition: x_i = FD + INT ===\n')
cat('Scale factor for each FD institutional channel K: delta_i^K * D^K\n\n')
print(rows[, c('shock','sector','target','HH_cons','Gov_cons','Firm_inv','Pub_inv',
               'FD_total','INT_total','x_i','pct_FD','pct_INT')], row.names=FALSE)

cat('\n\n=== ΔM1: actual scenario vs full-CE benchmark (all channels, ρ=0.2) ===\n')
cat('pct_of_full = actual targeted scale / x_i * 100\n\n')
print(rows[, c('shock','sector','target','actual_scale','x_i','struct1',
               'DeltaM1_act','DeltaM1_full','pct_of_full')], row.names=FALSE)
