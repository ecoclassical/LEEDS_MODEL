suppressPackageStartupMessages({
  library(openxlsx); library(dplyr); library(tidyr); library(ggplot2)
})

setwd("/Users/parvulesco/Documents/R/LEEDS_MODEL")

rho <- 0.2; t_sh <- 70; n_sec <- 54

# ── Load ──────────────────────────────────────────────────────────────────────
A_raw    <- read.xlsx('data/initial_state_2026.xlsx', sheet='A.matrix',
                      colNames=TRUE, rowNames=TRUE)
A        <- as.matrix(A_raw)
L        <- solve(diag(nrow(A)) - A)
baseline <- readRDS('output/scenarios/baseline_2026.RDS')
sim      <- baseline$simulation
sc       <- read.csv('data/scenarios.csv', stringsAsFactors=FALSE) %>%
            filter(shock != 14)
sec_list <- read.csv('data/sector_list.csv', stringsAsFactors=FALSE) %>%
            mutate(sector_code = as.integer(sector_code),
                   label = gsub('<br>', ' ', label))

# ── Baseline x vector (108 elements) at t_sh ─────────────────────────────────
x_base <- c(
  sapply(1:n_sec, function(j) sim[paste0('Z1_x-', j), t_sh]),
  sapply(1:n_sec, function(j) sim[paste0('Z2_x-', j), t_sh])
)

# ── Demand channel labels and variables ──────────────────────────────────────
K_labels <- c('HH consumption', 'Gov consumption', 'Firm investment', 'Public investment')
K_vars   <- c('Z1_c', 'Z1_g', 'Z1_id', 'Z1_id_g')
K_shares <- c('Z1_beta', 'Z1_sigma', 'Z1_iota', 'Z1_iota_g')

decompose_sector <- function(i) {
  fd_comps <- sapply(seq_along(K_vars), function(k) {
    share_name <- paste0(K_shares[k], '-', i)
    if (share_name %in% rownames(sim))
      sim[share_name, t_sh] * sim[K_vars[k], t_sh]
    else
      0
  })
  names(fd_comps) <- K_labels
  int_demand <- sum(A[i, ] * x_base, na.rm = TRUE)
  x_i        <- sim[paste0('Z1_x-', i), t_sh]
  d_i        <- sum(fd_comps)
  list(fd_comps=fd_comps, d_i=d_i, int_demand=int_demand, x_i=x_i)
}

# ── Build long table ──────────────────────────────────────────────────────────
sectors_to_decomp <- sc %>%
  distinct(from, sector, domain, target, shock) %>%
  rename(i = from)

decomp_rows <- lapply(1:nrow(sectors_to_decomp), function(r) {
  row <- sectors_to_decomp[r, ]
  d   <- decompose_sector(row$i)
  fd_df <- data.frame(
    shock=row$shock, sector=row$sector, domain=row$domain,
    target=row$target, i=row$i,
    channel=names(d$fd_comps), scale=as.numeric(d$fd_comps),
    channel_type='Final Demand',
    d_i=d$d_i, int_demand=d$int_demand, x_i=d$x_i
  )
  int_df <- data.frame(
    shock=row$shock, sector=row$sector, domain=row$domain,
    target=row$target, i=row$i,
    channel='Intermediate', scale=d$int_demand,
    channel_type='Intermediate',
    d_i=d$d_i, int_demand=d$int_demand, x_i=d$x_i
  )
  bind_rows(fd_df, int_df)
}) %>% bind_rows()

# Add structural factor per scenario
struct_df <- sc %>%
  rowwise() %>%
  mutate(struct1 = L[as.integer(from), as.integer(to)] -
                   L[as.integer(from), as.integer(from)]) %>%
  ungroup() %>%
  select(shock, struct1)

decomp_rows <- decomp_rows %>%
  left_join(struct_df, by='shock') %>%
  mutate(DeltaM1_channel = rho * scale * struct1)

# ── Table 1: x_i = d_i + intermediate, with % shares ─────────────────────────
cat('\n=== TABLE 1: Gross output decomposition (EU, t=70) ===\n')
cat('x_i = final demand d_i + intermediate demand; check should be ~0\n\n')
decomp_rows %>%
  group_by(shock, sector, i, x_i) %>%
  summarise(
    fd_total  = sum(scale[channel_type == 'Final Demand']),
    int_total = first(scale[channel == 'Intermediate']),
    .groups = 'drop'
  ) %>%
  mutate(
    pct_fd  = round(fd_total  / x_i * 100, 1),
    pct_int = round(int_total / x_i * 100, 1),
    check   = round(abs(fd_total + int_total - x_i), 3)
  ) %>%
  select(shock, sector, i, x_i, fd_total, pct_fd, int_total, pct_int, check) %>%
  mutate(across(c(x_i, fd_total, int_total), ~round(.x, 2))) %>%
  print(n=20)

# ── Table 2: FD broken down by institutional channel K ───────────────────────
cat('\n=== TABLE 2: Final demand breakdown by institutional channel (δᵢᴷ · Dᴷ) ===\n')
decomp_rows %>%
  filter(channel_type == 'Final Demand') %>%
  select(shock, sector, channel, scale) %>%
  mutate(scale = round(scale, 3)) %>%
  pivot_wider(names_from=channel, values_from=scale) %>%
  print(n=20)

# ── Table 3: ΔM1 contribution per channel if targeted individually ───────────
cat('\n=== TABLE 3: ΔM1 if each channel targeted separately (ρ=0.2) ===\n')
decomp_rows %>%
  select(shock, sector, channel, channel_type, scale, struct1, DeltaM1_channel) %>%
  mutate(across(c(scale, struct1, DeltaM1_channel), ~round(.x, 3))) %>%
  arrange(shock, channel_type, desc(abs(scale))) %>%
  print(n=90)

# ── Table 4: Full-CE benchmark – apply rho to ALL channels simultaneously ────
cat('\n=== TABLE 4: Full-CE benchmark: ΔM1 = ρ · x_i · struct1 ===\n')
cat('If policy simultaneously targeted every channel (FD + intermediate)\n\n')

# Actual ΔM1 from each scenario's targeted channel
actual_scale <- decomp_rows %>%
  mutate(is_targeted = case_when(
    target == 'beta'   & channel == 'HH consumption'   ~ TRUE,
    target == 'sigma'  & channel == 'Gov consumption'  ~ TRUE,
    target == 'iota'   & channel == 'Firm investment'  ~ TRUE,
    target == 'iota_g' & channel == 'Public investment'~ TRUE,
    target == 'a'      & channel == 'Intermediate'     ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  filter(is_targeted) %>%
  select(shock, sector, targeted_scale=scale, DeltaM1_actual=DeltaM1_channel)

decomp_rows %>%
  group_by(shock, sector, i, x_i) %>%
  summarise(struct1=first(struct1), .groups='drop') %>%
  mutate(DeltaM1_full = rho * x_i * struct1) %>%
  left_join(actual_scale, by=c('shock','sector')) %>%
  mutate(
    pct_of_full = round(DeltaM1_actual / DeltaM1_full * 100, 1)
  ) %>%
  select(shock, sector, x_i, struct1, DeltaM1_full,
         targeted_scale, DeltaM1_actual, pct_of_full) %>%
  mutate(across(c(x_i, struct1, DeltaM1_full, targeted_scale,
                  DeltaM1_actual), ~round(.x, 3))) %>%
  print(n=20)

saveRDS(decomp_rows, 'data/demand_decomposition.RDS')
cat('\nSaved: data/demand_decomposition.RDS\n')
