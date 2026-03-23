suppressPackageStartupMessages({
  library(openxlsx); library(dplyr); library(tidyr); library(ggplot2)
})

setwd("/Users/parvulesco/Documents/R/LEEDS_MODEL")

rho <- 0.2; t_sh <- 70; n_sec <- 54

# ── Load core data ─────────────────────────────────────────────────────────────
A_raw    <- read.xlsx('data/initial_state_2026.xlsx', sheet='A.matrix',
                      colNames=TRUE, rowNames=TRUE)
A        <- as.matrix(A_raw)
L        <- solve(diag(nrow(A)) - A)

baseline <- readRDS('output/scenarios/baseline_2026.RDS')
sim      <- baseline$simulation

sc       <- read.csv('data/scenarios.csv', stringsAsFactors=FALSE)
sec_list <- read.csv('data/sector_list.csv', stringsAsFactors=FALSE) %>%
            mutate(sector_code = as.integer(sector_code))

# ── Baseline x vector (108 elements) at t_sh ─────────────────────────────────
x_base <- c(
  sapply(1:n_sec, function(j) sim[paste0('Z1_x-', j), t_sh]),
  sapply(1:n_sec, function(j) sim[paste0('Z2_x-', j), t_sh])
)

# ── Scale factor helpers ──────────────────────────────────────────────────────
get_scale_fd <- function(target, from_j) {
  switch(target,
    beta   = sim[paste0('Z1_beta-',   from_j), t_sh] * sim['Z1_c',    t_sh],
    sigma  = sim[paste0('Z1_sigma-',  from_j), t_sh] * sim['Z1_g',    t_sh],
    iota   = sim[paste0('Z1_iota-',   from_j), t_sh] * sim['Z1_id',   t_sh],
    iota_g = sim[paste0('Z1_iota_g-', from_j), t_sh] * sim['Z1_id_g', t_sh],
    NA_real_
  )
}

get_scale_int <- function(from_j) {
  # sum_j a_{from,j} * x_j across all 108 sectors
  sum(A[from_j, ] * x_base, na.rm = TRUE)
}

# ── Part 1: Analytical comparison table ──────────────────────────────────────
results <- sc %>%
  rowwise() %>%
  mutate(
    i_from  = as.integer(from),
    i_to    = as.integer(to),
    l_ff    = L[i_from, i_from],
    l_ft    = L[i_from, i_to],
    l_tf    = L[i_to,   i_from],
    l_tt    = L[i_to,   i_to],
    lambda  = (l_ft + l_tt) - (l_ff + l_tf),
    struct1 = l_ft - l_ff,
    scale   = if (target %in% c('beta','sigma','iota','iota_g'))
                get_scale_fd(target, i_from)
              else
                get_scale_int(i_from),
    DeltaM1    = rho * scale * struct1,
    DeltaM_tot = rho * scale * lambda
  ) %>%
  ungroup()

cat('\n=== PART 1: Analytical scale factors and ΔM1 ===\n')
results %>%
  select(shock, domain, sector, target, from, to,
         scale, struct1, lambda, DeltaM1, DeltaM_tot) %>%
  mutate(across(where(is.numeric), ~round(.x, 4))) %>%
  print(n=14)

cat('\n--- Range summary ---\n')
fd  <- filter(results, target != 'a')
int <- filter(results, target == 'a')
cat(sprintf('FD  struct1: [%.4f, %.4f]   scale: [%.2f, %.2f]   |ΔM1|: [%.3f, %.3f]\n',
    min(fd$struct1), max(fd$struct1),
    min(fd$scale, na.rm=T), max(fd$scale, na.rm=T),
    min(abs(fd$DeltaM1), na.rm=T), max(abs(fd$DeltaM1), na.rm=T)))
cat(sprintf('INT struct1: [%.4f, %.4f]   scale: [%.2f, %.2f]   |ΔM1|: [%.3f, %.3f]\n',
    min(int$struct1), max(int$struct1),
    min(int$scale), max(int$scale),
    min(abs(int$DeltaM1)), max(abs(int$DeltaM1))))

# ── Part 2: Simulation trajectories ──────────────────────────────────────────
# Material sectors: name -> EU sector index
mat_sectors <- c(
  Food=7, ProcessedFood=8, Wood=11, Pulp=13,
  Plastics=17, Glass=21, Cement=24, Metals=26,
  FFElectricity=31, Renewables=32, Construction=36
)

t_start <- 65; t_end <- 100
base_sim <- baseline$simulation

extract_traj <- function(n) {
  rds_path <- file.path('output/scenarios/shock_runs',
                        paste0('shock_', n, '_run.RDS'))
  if (!file.exists(rds_path)) return(NULL)
  shock <- readRDS(rds_path)
  s_sim <- shock$simulation
  lapply(names(mat_sectors), function(nm) {
    j     <- mat_sectors[[nm]]
    vname <- paste0('Z1_x-', j)
    if (!(vname %in% rownames(s_sim))) return(NULL)
    data.frame(
      n_shock   = n,
      material  = nm,
      sector_j  = j,
      time      = t_start:t_end,
      baseline  = as.numeric(base_sim[vname, t_start:t_end]),
      shock_val = as.numeric(s_sim[vname, t_start:t_end])
    )
  }) %>% bind_rows()
}

all_traj <- lapply(1:14, extract_traj) %>% bind_rows()

traj <- all_traj %>%
  mutate(
    regime    = ifelse(n_shock <= 6, 'Final Demand', 'Production'),
    shock_lab = paste0('Sc.', n_shock, ': ', sc$sector[match(n_shock, sc$shock)],
                       ' / ', sc$transaction[match(n_shock, sc$shock)]),
    delta_x   = shock_val - baseline,
    pct_dev   = (shock_val - baseline) / baseline * 100
  ) %>%
  group_by(n_shock, material) %>%
  mutate(cum_delta = cumsum(ifelse(time < t_sh, 0, delta_x))) %>%
  ungroup()

# ── Summary: each scenario's OWN primary material sector ──────────────────────
sc_mat_map <- sc %>%
  mutate(mat_name = names(mat_sectors)[match(from, mat_sectors)]) %>%
  filter(!is.na(mat_name)) %>%
  select(n_shock = shock, mat_name)

cat('\n=== PART 2: Cumulative ΔM1 at t=100 (EU primary material output) ===\n')
traj %>%
  filter(time == 100) %>%
  inner_join(sc_mat_map, by = c('n_shock', material = 'mat_name')) %>%
  select(n_shock, shock_lab, material, baseline, shock_val, delta_x, pct_dev, cum_delta) %>%
  arrange(n_shock) %>%
  mutate(across(where(is.numeric), ~round(.x, 3))) %>%
  print(n = 20)

# ── Save results for QMD use ──────────────────────────────────────────────────
saveRDS(list(scale_table = results, trajectories = traj),
        'data/scale_trajectories.RDS')
cat('\nSaved: data/scale_trajectories.RDS\n')
