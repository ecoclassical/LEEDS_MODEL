suppressPackageStartupMessages({
  library(openxlsx)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
})
setwd("/Users/parvulesco/Documents/R/LEEDS_MODEL")

rho <- 0.2
t_sh <- 70
n_sec <- 54

# ── Output subdirectories ──────────────────────────────────────────────────────
for (d in c(
  'output/pdf/figures/demand',
  'output/png/demand',
  'output/pdf/figures/intermediate',
  'output/png/intermediate',
  'output/pdf/figures/leontief',
  'output/png/leontief',
  'output/pdf/figures/portfolio',
  'output/png/portfolio',
  'output/pdf/figures/impact',
  'output/png/impact'
)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

sec_list <- read.csv('data/sector_list.csv', stringsAsFactors = FALSE) %>%
  mutate(
    sector_code = as.integer(sector_code),
    label = gsub('<br>', ' ', label)
  )
all_labels <- c(sec_list$label, paste0('RoW: ', sec_list$label))

# ── Load ──────────────────────────────────────────────────────────────────────
A_raw <- read.xlsx(
  'data/full_mrio_initial_state.xlsx',
  sheet = 'A.matrix',
  colNames = TRUE,
  rowNames = TRUE
)
A <- as.matrix(A_raw)
L <- solve(diag(nrow(A)) - A)
baseline <- readRDS('output/scenarios/full_mrio_baseline_2026.RDS')
sim <- baseline$simulation
sc <- read.csv('data/scenarios.csv', stringsAsFactors = FALSE) %>%
  filter(shock != 14)

x_base <- c(
  sapply(1:n_sec, function(j) sim[paste0('Z1_x-', j), t_sh]),
  sapply(1:n_sec, function(j) sim[paste0('Z2_x-', j), t_sh])
)

# ── Cross-border import propensities from MARIO (empirical, by FD channel) ────
# eta_k[j] = share of EU final demand for sector j that is imported from RoW.
# Source: MARIO - Aggregated (2).xlsx, F21 block (RoW supply -> EU final demand).
mario_ch <- read.csv(
  'data/mrio_fd_channel_shares.csv',
  stringsAsFactors = FALSE
)
# Vectors indexed 1..54 (matching LEEDS sector order)
eta_hh_ch <- mario_ch$eta_RoW_hh # HH import propensity per sector
eta_gov_ch <- mario_ch$eta_RoW_gov # Gov import propensity per sector
eta_gfcf_ch <- mario_ch$eta_RoW_gfcf # GFCF import propensity per sector
# Public investment treated as domestic (eta = 0), as calibration prior

# ── Material sectors to analyse ───────────────────────────────────────────────
mat_sectors <- data.frame(
  label = c(
    'Food',
    'Energy',
    'Plastics',
    'Wood',
    'Pulp',
    'Glass',
    'Cement',
    'Metals'
  ),
  from_j = c(7, 31, 17, 11, 13, 21, 24, 26),
  to_j = c(8, 32, 18, 12, 14, 22, 25, 27),
  stringsAsFactors = FALSE
)

# ── FD channel definitions ────────────────────────────────────────────────────
channels <- data.frame(
  key = c('HH', 'Gov', 'FirmInv', 'PubInv'),
  label = c(
    'household consumption',
    'government consumption',
    'firm investment',
    'public investment'
  ),
  var = c('Z1_c', 'Z1_g', 'Z1_id', 'Z1_id_g'),
  share = c('beta', 'sigma', 'iota', 'iota_g'), # base name; region prefix added in get_channel_scales
  stringsAsFactors = FALSE
)

# ── Portfolio definitions ─────────────────────────────────────────────────────
portfolios <- list(
  HH = list(
    fd = c('HH'),
    int = FALSE,
    label = 'private consumption\n(households)'
  ),
  Gov = list(
    fd = c('Gov'),
    int = FALSE,
    label = 'public consumption\n(government)'
  ),
  AllCons = list(
    fd = c('HH', 'Gov'),
    int = FALSE,
    label = 'all consumption\n(households + government)'
  ),
  PrivInv = list(
    fd = c('FirmInv'),
    int = FALSE,
    label = 'private investment\n(firms)'
  ),
  PubInv = list(fd = c('PubInv'), int = FALSE, label = 'public investment'),
  AllInvest = list(
    fd = c('FirmInv', 'PubInv'),
    int = FALSE,
    label = 'all investment\n(private + public)'
  ),
  PubSector = list(
    fd = c('Gov', 'PubInv'),
    int = FALSE,
    label = 'public sector\n(consumption + investment)'
  ),
  FD = list(
    fd = c('HH', 'Gov', 'FirmInv', 'PubInv'),
    int = FALSE,
    label = 'all final demand'
  ),
  INT = list(
    fd = character(0),
    int = TRUE,
    label = 'intermediate\n(firm inputs)'
  ),
  Full = list(
    fd = c('HH', 'Gov', 'FirmInv', 'PubInv'),
    int = TRUE,
    label = 'full CE\n(all channels)'
  )
)

# ── Compute channel scales for each material sector, by region ────────────────
# prefix: 'Z1' (EU) or 'Z2' (RoW); local_j: sector index 1..54 within region
# A matrix row: Z1 -> j, Z2 -> j + n_sec
get_channel_scales <- function(local_j, prefix = 'Z1') {
  a_row_idx <- if (prefix == 'Z1') local_j else local_j + n_sec
  reg_channels <- channels
  reg_channels$var <- sub('^Z1', prefix, channels$var)
  # Full MRIO share names: {buyer_region}_{share}_{seller_region}-{sector}
  reg_channels$share <- paste0(prefix, '_', channels$share, '_', prefix)

  # Domestic FD scales: delta[k,j] * D[k]
  fd_dom <- sapply(reg_channels$key, function(k) {
    row <- reg_channels[reg_channels$key == k, ]
    sn <- paste0(row$share, '-', local_j)
    if (sn %in% rownames(sim)) {
      as.numeric(sim[sn, t_sh]) * as.numeric(sim[row$var, t_sh])
    } else {
      0
    }
  })
  names(fd_dom) <- channels$key

  # Cross-border import demand: eta_k[j] * D[k]  (EU only; F21 block from MARIO).
  # For RoW (Z2), cross-border demand flows the other direction (= EU exports),
  # already captured in "Net exports / other", so fd_imp = 0 for Z2.
  if (prefix == 'Z1') {
    # Correct formula: fd_imp[k,j] = eta_k[j] / (1 - eta_k[j]) * fd_dom[k,j]
    # eta_k[j] = F21_k[j] / (F11_k[j] + F21_k[j]) from MARIO (import propensity)
    # import/domestic ratio = eta / (1 - eta)
    imp_ratio <- function(eta) ifelse(is.na(eta) | eta >= 1, 0, eta / (1 - eta))
    fd_imp <- c(
      HH = imp_ratio(eta_hh_ch[local_j]) * unname(fd_dom['HH']),
      Gov = imp_ratio(eta_gov_ch[local_j]) * unname(fd_dom['Gov']),
      FirmInv = imp_ratio(eta_gfcf_ch[local_j]) * unname(fd_dom['FirmInv']),
      PubInv = 0
    )
  } else {
    fd_imp <- c(HH = 0, Gov = 0, FirmInv = 0, PubInv = 0)
  }

  dom_col <- if (prefix == 'Z1') 1:n_sec else (n_sec + 1):(2 * n_sec)
  imp_col <- if (prefix == 'Z1') (n_sec + 1):(2 * n_sec) else 1:n_sec
  int_dom <- sum(A[a_row_idx, dom_col] * x_base[dom_col], na.rm = TRUE)
  int_imp <- sum(A[a_row_idx, imp_col] * x_base[imp_col], na.rm = TRUE)
  x_i <- as.numeric(sim[paste0(prefix, '_x-', local_j), t_sh])
  list(
    fd_dom = fd_dom,
    fd_imp = fd_imp,
    fd = fd_dom + fd_imp,
    int_dom = int_dom,
    int_imp = int_imp,
    int = int_dom + int_imp,
    x_i = x_i
  )
}

# ── Build demand structure table — primary & secondary × EU & RoW ────────────
build_struct_rows <- function(mat_label, sector_j, mat_type, region, prefix) {
  cs <- get_channel_scales(sector_j, prefix)
  bind_rows(
    # Domestic: FD channels + domestic intermediate
    data.frame(
      material = mat_label,
      mat_type = mat_type,
      region = region,
      dom_for = 'Domestic',
      channel = 'household consumption',
      scale = unname(cs$fd_dom['HH'])
    ),
    data.frame(
      material = mat_label,
      mat_type = mat_type,
      region = region,
      dom_for = 'Domestic',
      channel = 'government consumption',
      scale = unname(cs$fd_dom['Gov'])
    ),
    data.frame(
      material = mat_label,
      mat_type = mat_type,
      region = region,
      dom_for = 'Domestic',
      channel = 'firm investment',
      scale = unname(cs$fd_dom['FirmInv'])
    ),
    data.frame(
      material = mat_label,
      mat_type = mat_type,
      region = region,
      dom_for = 'Domestic',
      channel = 'public investment',
      scale = unname(cs$fd_dom['PubInv'])
    ),
    data.frame(
      material = mat_label,
      mat_type = mat_type,
      region = region,
      dom_for = 'Domestic',
      channel = 'firm intermediate consumption',
      scale = cs$int_dom
    ),
    # Cross-border: FD import demand (F21 block) + cross-border intermediate
    data.frame(
      material = mat_label,
      mat_type = mat_type,
      region = region,
      dom_for = 'Cross-border',
      channel = 'household consumption',
      scale = unname(cs$fd_imp['HH'])
    ),
    data.frame(
      material = mat_label,
      mat_type = mat_type,
      region = region,
      dom_for = 'Cross-border',
      channel = 'government consumption',
      scale = unname(cs$fd_imp['Gov'])
    ),
    data.frame(
      material = mat_label,
      mat_type = mat_type,
      region = region,
      dom_for = 'Cross-border',
      channel = 'firm investment',
      scale = unname(cs$fd_imp['FirmInv'])
    ),
    data.frame(
      material = mat_label,
      mat_type = mat_type,
      region = region,
      dom_for = 'Cross-border',
      channel = 'firm intermediate consumption',
      scale = cs$int_imp
    )
  )
}

struct_long <- lapply(1:nrow(mat_sectors), function(r) {
  ms <- mat_sectors[r, ]
  bind_rows(
    build_struct_rows(ms$label, ms$from_j, 'Primary', 'EU', 'Z1'),
    build_struct_rows(ms$label, ms$to_j, 'Secondary', 'EU', 'Z1'),
    build_struct_rows(ms$label, ms$from_j, 'Primary', 'RoW', 'Z2'),
    build_struct_rows(ms$label, ms$to_j, 'Secondary', 'RoW', 'Z2')
  )
}) %>%
  bind_rows()

# Order materials by EU primary domestic intermediate share (ascending) — FD-heavy on left
mat_order <- struct_long %>%
  filter(mat_type == 'Primary', region == 'EU', dom_for == 'Domestic') %>%
  group_by(material) %>%
  summarise(x_i = sum(scale), int = sum(scale[channel == 'Intermediate'])) %>%
  mutate(pct_int = int / x_i) %>%
  arrange(pct_int) %>%
  pull(material)

chan_levels <- c(
  'public investment',
  'firm investment',
  'government consumption',
  'household consumption',
  'firm intermediate consumption'
)
chan_colours <- c(
  'firm intermediate consumption' = '#2c7bb6',
  'household consumption' = '#d7191c',
  'government consumption' = '#e85d04',
  'firm investment' = '#fdae61',
  'public investment' = '#fee090'
)

region_dom_levels <- c(
  'EU — Domestic',
  'EU — Cross-border',
  'RoW — Domestic',
  'RoW — Cross-border'
)
struct_long$material <- factor(struct_long$material, levels = mat_order)
struct_long$channel <- factor(struct_long$channel, levels = chan_levels)
struct_long$mat_type <- factor(
  struct_long$mat_type,
  levels = c('Primary', 'Secondary')
)
struct_long$region <- factor(struct_long$region, levels = c('EU', 'RoW'))
struct_long$dom_for <- factor(
  struct_long$dom_for,
  levels = c('Domestic', 'Cross-border')
)
struct_long$region_dom <- factor(
  paste(struct_long$region, '\u2014', struct_long$dom_for),
  levels = region_dom_levels
)

# ── Shared theme for p_demand plots ───────────────────────────────────────────
theme_demand <- function(base_size = 11) {
  theme_grey(base_size = base_size) %+replace%
    theme(
      strip.text.x = element_text(face = 'bold', size = 10),
      strip.text.y = element_text(face = 'bold', size = 10, angle = 0),
      axis.text.y = element_text(size = 10, hjust = 1),
      axis.text.x = element_text(size = 8),
      legend.position = 'top',
      legend.direction = 'horizontal',
      legend.title = element_text(face = 'bold'),
      panel.grid.major.y = element_blank(),
      plot.title = element_text(face = 'bold', size = 16, hjust = 0),
      plot.subtitle = element_text(face = 'italic', size = 14, hjust = 0)
    )
}

demand_scale <- scale_fill_manual(
  values = chan_colours,
  name = 'channel',
  guide = guide_legend(nrow = 1)
)

# ── PLOT 1 v1: y = material, facet rows = mat_type ────────────────────────────
p1 <- ggplot(struct_long, aes(y = material, x = scale, fill = channel)) +
  geom_col(width = 0.75, colour = 'white', linewidth = 0.2) +
  facet_grid(vars(mat_type), vars(region, dom_for), scales = 'free_x') +
  demand_scale +
  scale_x_continuous(expand = expansion(mult = c(0, 0.06))) +
  labs(
    title = 'Demand Structure: Primary vs Secondary Material Sectors (EU & RoW, t = 70)',
    subtitle = 'Rows: Primary / Secondary. Columns: EU·Domestic, EU·Cross-border, RoW·Domestic, RoW·Cross-border.',
    x = 'Output (model units)',
    y = NULL
  ) +
  theme_demand(base_size = 22) +
  theme(
    strip.text.x = element_text(face = 'bold', size = 20),
    strip.text.y = element_text(face = 'bold', size = 20, angle = 0),
    axis.text.y  = element_text(size = 20, hjust = 1),
    axis.text.x  = element_text(size = 16),
    plot.title   = element_text(face = 'bold', size = 28, hjust = 0),
    plot.subtitle = element_text(face = 'italic', size = 24, hjust = 0)
  )

ggsave(
  'output/pdf/figures/demand/p_demand_structure.pdf',
  p1,
  width = 16,
  height = 7
)
ggsave(
  'output/png/demand/p_demand_structure.png',
  p1,
  width = 16,
  height = 7,
  dpi = 150
)
cat('Saved: p_demand_structure\n')

# ── Composition: normalised to primary + secondary combined ────────────────────
struct_comp <- struct_long %>%
  group_by(material, region, dom_for) %>%
  mutate(total_ps = sum(scale, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(pct = ifelse(total_ps > 0, scale / total_ps * 100, 0))

# ── PLOT 1b v1: y = material, facet rows = mat_type ───────────────────────────
p1b <- ggplot(struct_comp, aes(y = material, x = pct, fill = channel)) +
  geom_col(width = 0.75, colour = 'white', linewidth = 0.2) +
  facet_grid(vars(mat_type), vars(region, dom_for)) +
  demand_scale +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.03)),
    labels = function(x) paste0(x, '%')
  ) +
  labs(
    title = 'Demand Composition: Primary vs Secondary Material Sectors (EU & RoW, t = 70)',
    subtitle = 'Rows: Primary / Secondary. Bars normalised to primary + secondary combined total per material × region × origin.',
    x = '% of primary + secondary material output',
    y = NULL
  ) +
  theme_demand()

ggsave(
  'output/pdf/figures/demand/p_demand_composition.pdf',
  p1b,
  width = 16,
  height = 7
)
ggsave(
  'output/png/demand/p_demand_composition.png',
  p1b,
  width = 16,
  height = 7,
  dpi = 150
)
cat('Saved: p_demand_composition\n')

# ── PLOT 1c: EU import demand by sector and FD channel (actual MARIO eta) ──────
# Replaces the kappa/lambda approximation in compute_mrio_fd_extension.R.
# Uses fd_imp from get_channel_scales() — calibrated directly from MARIO F21 block.
import_all <- lapply(1:n_sec, function(j) {
  cs <- get_channel_scales(j, 'Z1')
  data.frame(
    sector_j = j,
    label = sec_list$label[j],
    `household consumption` = unname(cs$fd_imp['HH']),
    `government consumption` = unname(cs$fd_imp['Gov']),
    `firm investment` = unname(cs$fd_imp['FirmInv']),
    `firm intermediate consumption` = cs$int_imp,
    total_imp = sum(cs$fd_imp[c('HH', 'Gov', 'FirmInv')]) + cs$int_imp,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}) %>%
  bind_rows()

top20_imp <- import_all %>%
  arrange(desc(total_imp)) %>%
  slice(1:20) %>%
  pull(label)

import_long <- import_all %>%
  filter(label %in% top20_imp) %>%
  pivot_longer(
    cols = c(
      'household consumption',
      'government consumption',
      'firm investment',
      'firm intermediate consumption'
    ),
    names_to = 'channel',
    values_to = 'import_demand'
  ) %>%
  mutate(
    channel = factor(
      channel,
      levels = c(
        'firm intermediate consumption',
        'firm investment',
        'government consumption',
        'household consumption'
      )
    ),
    label = factor(
      label,
      levels = import_all %>%
        filter(label %in% top20_imp) %>%
        arrange(total_imp) %>%
        pull(label)
    )
  )

imp_colours <- chan_colours[c(
  'household consumption',
  'government consumption',
  'firm investment',
  'firm intermediate consumption'
)]

p1c <- ggplot(import_long, aes(x = label, y = import_demand, fill = channel)) +
  geom_col(width = 0.75, colour = 'white', linewidth = 0.2) +
  coord_flip() +
  scale_fill_manual(
    values = imp_colours,
    name = 'channel',
    guide = guide_legend(nrow = 1)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.06))) +
  labs(
    title = 'EU Imports from RoW by Sector and Demand Channel',
    subtitle = 'Top 20 sectors. Import demand = eta_k[j] / (1 - eta_k[j]) x domestic FD[k,j].',
    x = NULL,
    y = 'Import demand (model units)'
  ) +
  theme_demand(base_size = 11) +
  theme(panel.grid.major.y = element_blank())

ggsave(
  'output/pdf/figures/demand/p_import_decomp.pdf',
  p1c,
  width = 10,
  height = 8
)
ggsave(
  'output/png/demand/p_import_decomp.png',
  p1c,
  width = 10,
  height = 8,
  dpi = 150
)
cat('Saved: p_import_decomp\n')

# ── Build portfolio effectiveness table ───────────────────────────────────────
port_rows <- lapply(1:nrow(mat_sectors), function(r) {
  ms <- mat_sectors[r, ]
  cs <- get_channel_scales(ms$from_j)
  struct1 <- L[ms$from_j, ms$to_j] - L[ms$from_j, ms$from_j]

  lapply(names(portfolios), function(pname) {
    p <- portfolios[[pname]]
    # Portfolio scale: domestic FD only (consistent with EU Leontief structural factor).
    # Cross-border import demand is shown in the demand structure figure (Plot 1) but
    # excluded here because a CE substitution in imported goods requires the RoW
    # structural factor (L[j+54, k+54]), not the EU struct1 used here.
    pscale_dom <- sum(cs$fd_dom[p$fd], na.rm = TRUE) + if (p$int) cs$int else 0
    pscale_imp <- sum(cs$fd_imp[p$fd], na.rm = TRUE) # informational only
    data.frame(
      material = ms$label,
      portfolio = p$label,
      port_key = pname,
      scale = pscale_dom,
      scale_imp = round(pscale_imp, 3), # cross-border addendum
      x_i = cs$x_i,
      struct1 = struct1,
      DeltaM1 = rho * pscale_dom * struct1,
      pct_of_full = pscale_dom / cs$x_i * 100,
      stringsAsFactors = FALSE
    )
  }) %>%
    bind_rows()
}) %>%
  bind_rows()

# Print summary
cat('\n=== Portfolio effectiveness (pct_of_full = portfolio scale / x_i) ===\n')
port_rows %>%
  select(material, portfolio, scale, pct_of_full, DeltaM1) %>%
  mutate(across(c(scale, pct_of_full, DeltaM1), ~ round(.x, 2))) %>%
  pivot_wider(
    names_from = portfolio,
    values_from = pct_of_full,
    id_cols = material
  ) %>%
  print(n = 10)

# ── PLOT 2: Heatmap — portfolio × material ────────────────────────────────────
port_order <- c(
  'private consumption\n(households)',
  'public consumption\n(government)',
  'all consumption\n(households + government)',
  'private investment\n(firms)',
  'public investment',
  'all investment\n(private + public)',
  'public sector\n(consumption + investment)',
  'all final demand',
  'intermediate\n(firm inputs)',
  'full CE\n(all channels)'
)
mat_order2 <- mat_order # same ordering as bar chart (ascending INT share)

port_rows$portfolio <- factor(port_rows$portfolio, levels = port_order)
# Replace spaces with newlines in material labels for horizontal x-axis readability
mat_order2_nl <- gsub(' ', '\n', mat_order2)
port_rows$material <- factor(
  gsub(' ', '\n', port_rows$material),
  levels = mat_order2_nl
)

p2 <- ggplot(port_rows, aes(x = material, y = portfolio, fill = pct_of_full)) +
  geom_tile(colour = 'white', linewidth = 0.5) +
  geom_text(
    aes(label = sprintf('%.0f%%', pct_of_full), colour = pct_of_full > 55),
    size = 3.2,
    show.legend = FALSE
  ) +
  scale_colour_manual(values = c('TRUE' = 'white', 'FALSE' = 'black')) +
  scale_fill_gradientn(
    colours = c('#f7f7f7', '#d1e5f0', '#4393c3', '#2166ac', '#053061'),
    limits = c(0, 100),
    name = '% of full-CE\npotential'
  ) +
  labs(
    title = 'Circular Economy Policy Portfolio Effectiveness by Material Sector',
    subtitle = 'Cell value = portfolio scale / x_i x 100  (struct factor ~ -1 for all; scale dominates)',
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background = element_rect(fill = 'white', colour = NA),
    panel.background = element_rect(fill = 'white', colour = NA),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(face = 'bold'),
    panel.grid = element_blank()
  )

ggsave(
  'output/pdf/figures/portfolio/p_portfolio_heatmap.pdf',
  p2,
  width = 10,
  height = 6
)
ggsave(
  'output/png/portfolio/p_portfolio_heatmap.png',
  p2,
  width = 10,
  height = 6,
  dpi = 150
)
cat('Saved: p_portfolio_heatmap\n')

# ── PLOT 3: Intermediate demand decomposition ─────────────────────────────────
# Layout (demand-like): y = material, fill = using_sector (stacked),
#   facet rows = mat_type (Primary / Secondary),
#   facet cols = region × dom_for  (EU·Dom, EU·Cross, RoW·Dom, RoW·Cross)
top_n_sectors <- 10

# Global top-10 using sectors by total contribution across all materials and types
all_contrib_id <- Reduce(
  '+',
  lapply(1:nrow(mat_sectors), function(r) {
    ms <- mat_sectors[r, ]
    rowSums(sapply(c(ms$from_j, ms$to_j), function(i) {
      v <- A[i, 1:n_sec] * x_base[1:n_sec]
      names(v) <- sec_list$label
      v
    }))
  })
)
sectors_keep <- names(sort(all_contrib_id, decreasing = TRUE))[1:top_n_sectors]

# Build rows for each material × mat_type × region × dom_for
make_int_rows <- function(mat_label, mat_type, i_eu, i_row) {
  eu_dom <- A[i_eu, 1:n_sec] * x_base[1:n_sec]
  eu_imp <- A[i_eu, (n_sec + 1):(2 * n_sec)] * x_base[(n_sec + 1):(2 * n_sec)]
  names(eu_dom) <- names(eu_imp) <- sec_list$label
  total_eu <- sum(eu_dom, na.rm = TRUE) + sum(eu_imp, na.rm = TRUE)

  rw_dom <- A[i_row, (n_sec + 1):(2 * n_sec)] * x_base[(n_sec + 1):(2 * n_sec)]
  rw_imp <- A[i_row, 1:n_sec] * x_base[1:n_sec]
  names(rw_dom) <- names(rw_imp) <- sec_list$label
  total_rw <- sum(rw_dom, na.rm = TRUE) + sum(rw_imp, na.rm = TRUE)

  # Safe lookup: missing sectors → 0
  collapse <- function(v, total) {
    top_v <- sapply(sectors_keep, function(s) {
      if (s %in% names(v)) v[[s]] else 0
    })
    other_v <- sum(v[!names(v) %in% sectors_keep], na.rm = TRUE)
    bind_rows(
      data.frame(
        using_sector = sectors_keep,
        abs_val = as.numeric(top_v),
        stringsAsFactors = FALSE
      ),
      data.frame(
        using_sector = 'Other',
        abs_val = other_v,
        stringsAsFactors = FALSE
      )
    ) %>%
      mutate(pct = ifelse(total > 0, abs_val / total * 100, 0))
  }
  bind_rows(
    collapse(eu_dom, total_eu) %>%
      mutate(
        material = mat_label,
        mat_type = mat_type,
        region = 'EU',
        dom_for = 'Domestic'
      ),
    collapse(eu_imp, total_eu) %>%
      mutate(
        material = mat_label,
        mat_type = mat_type,
        region = 'EU',
        dom_for = 'Cross-border'
      ),
    collapse(rw_dom, total_rw) %>%
      mutate(
        material = mat_label,
        mat_type = mat_type,
        region = 'RoW',
        dom_for = 'Domestic'
      ),
    collapse(rw_imp, total_rw) %>%
      mutate(
        material = mat_label,
        mat_type = mat_type,
        region = 'RoW',
        dom_for = 'Cross-border'
      )
  )
}

int_decomp <- lapply(1:nrow(mat_sectors), function(r) {
  ms <- mat_sectors[r, ]
  bind_rows(
    make_int_rows(ms$label, 'Primary', ms$from_j, ms$from_j + n_sec),
    make_int_rows(ms$label, 'Secondary', ms$to_j, ms$to_j + n_sec)
  )
}) %>%
  bind_rows()

# Colour palette: one colour per using sector
sector_order_id <- c(sectors_keep, 'Other')
sector_pal_id <- c(
  setNames(scales::hue_pal()(top_n_sectors), sectors_keep),
  'Other' = '#cccccc'
)

int_decomp$using_sector <- factor(
  int_decomp$using_sector,
  levels = sector_order_id
)
int_decomp$material <- factor(int_decomp$material, levels = mat_order)
int_decomp$mat_type <- factor(
  int_decomp$mat_type,
  levels = c('Primary', 'Secondary')
)
int_decomp$region <- factor(int_decomp$region, levels = c('EU', 'RoW'))
int_decomp$dom_for <- factor(
  int_decomp$dom_for,
  levels = c('Domestic', 'Cross-border')
)

# ── Plot builder: demand-like stacked bars ─────────────────────────────────────
make_int_plot <- function(df, xvar, xlabel, title_suffix) {
  x_scale <- if (xvar == 'pct') {
    scale_x_continuous(
      expand = expansion(mult = c(0, 0.03)),
      labels = function(v) paste0(round(v, 0), '%')
    )
  } else {
    scale_x_continuous(expand = expansion(mult = c(0, 0.06)))
  }

  ggplot(df, aes(y = material, x = .data[[xvar]], fill = using_sector)) +
    geom_col(width = 0.75, colour = 'white', linewidth = 0.2) +
    facet_grid(vars(mat_type), vars(region, dom_for), scales = 'free_x') +
    scale_fill_manual(
      values = sector_pal_id,
      name = 'using sector',
      guide = guide_legend(nrow = 1)
    ) +
    x_scale +
    labs(
      title = paste(
        'Intermediate Demand Decomposition by Using Sector:',
        title_suffix
      ),
      subtitle = 'Rows: Primary / Secondary. Columns: EU·Domestic, EU·Cross-border, RoW·Domestic, RoW·Cross-border.',
      x = xlabel,
      y = NULL
    ) +
    theme_grey(base_size = 10) +
    theme(
      strip.text.x = element_text(face = 'bold', size = 10),
      strip.text.y = element_text(face = 'bold', size = 10, angle = 0),
      axis.text.y = element_text(size = 10, hjust = 1),
      axis.text.x = element_text(size = 8),
      legend.position = 'top',
      legend.direction = 'horizontal',
      legend.title = element_text(face = 'bold'),
      panel.grid.major.y = element_blank(),
      plot.title = element_text(face = 'bold', size = 16, hjust = 0),
      plot.subtitle = element_text(face = 'italic', size = 14, hjust = 0)
    )
}

p_int_abs <- make_int_plot(
  int_decomp,
  'abs_val',
  'Intermediate demand (model units)',
  'Absolute Levels'
)
p_int_pct <- make_int_plot(
  int_decomp,
  'pct',
  '% of total intermediate demand',
  '% of Total'
)

ggsave(
  'output/pdf/figures/intermediate/p_intermediate_abs.pdf',
  p_int_abs,
  width = 16,
  height = 8
)
ggsave(
  'output/png/intermediate/p_intermediate_abs.png',
  p_int_abs,
  width = 16,
  height = 8,
  dpi = 150
)
ggsave(
  'output/pdf/figures/intermediate/p_intermediate_pct.pdf',
  p_int_pct,
  width = 16,
  height = 8
)
ggsave(
  'output/png/intermediate/p_intermediate_pct.png',
  p_int_pct,
  width = 16,
  height = 8,
  dpi = 150
)
cat('Saved: p_intermediate_abs, p_intermediate_pct\n')

# ── PLOT 3c: Full Leontief upstream input requirements — all supply chain orders ─
# COLUMN view: (L − I)[k, i] × x[i] — what does each material sector i REQUIRE
# as upstream inputs from sector k (across all IO orders)?
# This is the opposite of Plot 3 (row view = who demands the material).
# Column view reveals fossil-fuel extraction as upstream input to Energy,
# and the structural difference between primary and secondary supply chains.
#
# Layout: rows = region × mat_type  (EU Primary / EU Secondary / RoW Primary / RoW Secondary)
#         cols = dom_for             (Domestic inputs / Cross-border inputs)
#         x    = material            (8 bars per panel)
#         fill = upstream_sector     (stacked: top-N sectors + Other)
#         scales = 'free_y'

L_minus_I <- L - diag(nrow(L))
top_n_lf <- 20

# Build full Leontief upstream rows for each material × mat_type × region × dom_for
int_decomp_lf <- lapply(1:nrow(mat_sectors), function(r) {
  ms <- mat_sectors[r, ]
  lapply(
    list(
      list(j = ms$from_j, type = 'Primary'),
      list(j = ms$to_j, type = 'Secondary')
    ),
    function(mt) {
      i_eu <- mt$j
      i_row <- mt$j + n_sec
      xi_eu <- x_base[i_eu]
      xi_row <- x_base[i_row]
      eu_dom <- L_minus_I[1:n_sec, i_eu] * xi_eu
      eu_imp <- L_minus_I[(n_sec + 1):(2 * n_sec), i_eu] * xi_eu
      rw_dom <- L_minus_I[(n_sec + 1):(2 * n_sec), i_row] * xi_row
      rw_imp <- L_minus_I[1:n_sec, i_row] * xi_row
      names(eu_dom) <- names(eu_imp) <- names(rw_dom) <- names(
        rw_imp
      ) <- sec_list$label
      make_rows <- function(contrib, region_lbl, dom_lbl) {
        data.frame(
          material = ms$label,
          mat_type = mt$type,
          region = region_lbl,
          dom_for = dom_lbl,
          using_sector = names(contrib),
          abs_val = as.numeric(contrib),
          stringsAsFactors = FALSE
        )
      }
      bind_rows(
        make_rows(eu_dom, 'EU', 'Domestic'),
        make_rows(eu_imp, 'EU', 'Cross-border'),
        make_rows(rw_dom, 'RoW', 'Domestic'),
        make_rows(rw_imp, 'RoW', 'Cross-border')
      )
    }
  ) %>%
    bind_rows()
}) %>%
  bind_rows()

# Top-20 upstream sectors ranked separately for primary and secondary
# (by max contribution to any single panel — keeps fossil-fuel sectors visible for Energy)
top_lf_pri <- int_decomp_lf %>%
  filter(mat_type == 'Primary') %>%
  group_by(using_sector) %>%
  summarise(total_val = sum(abs_val, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(total_val)) %>%
  slice(1:top_n_lf) %>%
  pull(using_sector)

top_lf_sec <- int_decomp_lf %>%
  filter(mat_type == 'Secondary') %>%
  group_by(using_sector) %>%
  summarise(total_val = sum(abs_val, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(total_val)) %>%
  slice(1:top_n_lf) %>%
  pull(using_sector)

# Collapse into Other, then add % of combined per material × region × dom_for
collapse_lf <- function(df, top_sectors) {
  df %>%
    mutate(
      using_sector = ifelse(
        using_sector %in% top_sectors,
        using_sector,
        'Other'
      )
    ) %>%
    group_by(material, mat_type, region, dom_for, using_sector) %>%
    summarise(abs_val = sum(abs_val, na.rm = TRUE), .groups = 'drop') %>%
    group_by(material, region, dom_for) %>%
    mutate(total_combined = sum(abs_val, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      pct_combined = ifelse(
        total_combined > 0,
        abs_val / total_combined * 100,
        0
      ),
      using_sector = factor(
        using_sector,
        levels = c('Other', rev(top_sectors))
      ),
      material = factor(material, levels = mat_order),
      region = factor(region, levels = c('EU', 'RoW')),
      dom_for = factor(dom_for, levels = c('Domestic', 'Cross-border'))
    )
}

lf_pri <- collapse_lf(filter(int_decomp_lf, mat_type == 'Primary'), top_lf_pri)
lf_sec <- collapse_lf(
  filter(int_decomp_lf, mat_type == 'Secondary'),
  top_lf_sec
)

# Colour palette: one colour per material
mat_pal_lf <- setNames(scales::hue_pal()(length(mat_order)), mat_order)

# ── Builder: separate Primary / Secondary plots ────────────────────────────────
# y = upstream sector (top-20, fixed), fill = material (dodged),
# facet cols = region × dom_for
make_lf_plot <- function(df, mat_type_lbl, top_sectors, xvar, xlabel) {
  x_scale <- if (xvar == 'pct_combined') {
    scale_x_continuous(
      expand = expansion(mult = c(0, 0.06)),
      labels = function(v) paste0(round(v, 0), '%')
    )
  } else {
    scale_x_continuous(expand = expansion(mult = c(0, 0.06)))
  }

  ggplot(df, aes(y = using_sector, x = .data[[xvar]], fill = material)) +
    geom_col(
      width = 0.75,
      colour = 'white',
      linewidth = 0.2,
      position = position_dodge(width = 0.8)
    ) +
    facet_grid(cols = vars(region, dom_for), scales = 'free_x') +
    scale_fill_manual(
      values = mat_pal_lf,
      name = 'material',
      guide = guide_legend(nrow = 1)
    ) +
    x_scale +
    labs(
      title = paste0(
        'Full Supply-Chain Upstream Requirements (Leontief, ',
        mat_type_lbl,
        ')'
      ),
      subtitle = 'Top 20 upstream sectors by max contribution. Columns: EU·Domestic, EU·Cross-border, RoW·Domestic, RoW·Cross-border.',
      x = xlabel,
      y = NULL
    ) +
    theme_grey(base_size = 10) +
    theme(
      strip.text.x = element_text(face = 'bold', size = 10),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 8),
      legend.position = 'top',
      legend.direction = 'horizontal',
      legend.title = element_text(face = 'bold'),
      panel.grid.major.y = element_blank(),
      plot.title = element_text(face = 'bold', size = 16, hjust = 0),
      plot.subtitle = element_text(face = 'italic', size = 14, hjust = 0)
    )
}

for (xvar in c('abs_val', 'pct_combined')) {
  xlabel <- if (xvar == 'pct_combined') {
    '% of combined (primary + secondary)'
  } else {
    'Upstream requirement (model units)'
  }
  tag <- if (xvar == 'pct_combined') 'pct' else 'abs'
  p_pri <- make_lf_plot(lf_pri, 'Primary', top_lf_pri, xvar, xlabel)
  p_sec <- make_lf_plot(lf_sec, 'Secondary', top_lf_sec, xvar, xlabel)
  ggsave(
    sprintf('output/pdf/figures/leontief/p_leontief_primary_%s.pdf', tag),
    p_pri,
    width = 16,
    height = 10
  )
  ggsave(
    sprintf('output/png/leontief/p_leontief_primary_%s.png', tag),
    p_pri,
    width = 16,
    height = 10,
    dpi = 150
  )
  ggsave(
    sprintf('output/pdf/figures/leontief/p_leontief_secondary_%s.pdf', tag),
    p_sec,
    width = 16,
    height = 10
  )
  ggsave(
    sprintf('output/png/leontief/p_leontief_secondary_%s.png', tag),
    p_sec,
    width = 16,
    height = 10,
    dpi = 150
  )
}
cat('Saved: p_leontief primary/secondary x abs/pct\n')

# EU-only versions (first two columns: EU-Domestic + EU-Cross-border)
lf_pri_eu <- dplyr::filter(lf_pri, region == 'EU')
lf_sec_eu <- dplyr::filter(lf_sec, region == 'EU')

legend_right <- list(
  ggplot2::theme(legend.position = 'right', legend.direction = 'vertical'),
  ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1))
)
p_pri_eu_abs <- make_lf_plot(
  lf_pri_eu,
  'Primary',
  top_lf_pri,
  'abs_val',
  'Upstream requirement (model units)'
) +
  legend_right
p_sec_eu_abs <- make_lf_plot(
  lf_sec_eu,
  'Secondary',
  top_lf_sec,
  'abs_val',
  'Upstream requirement (model units)'
) +
  legend_right
for (pair in list(
  list(p = p_pri_eu_abs, tag = 'primary'),
  list(p = p_sec_eu_abs, tag = 'secondary')
)) {
  ggsave(
    sprintf('output/pdf/figures/leontief/p_leontief_%s_abs_eu.pdf', pair$tag),
    pair$p,
    width = 14,
    height = 10
  )
  ggsave(
    sprintf('output/png/leontief/p_leontief_%s_abs_eu.png', pair$tag),
    pair$p,
    width = 14,
    height = 10,
    dpi = 150
  )
}
cat('Saved: p_leontief primary/secondary abs EU-only\n')

# ── Heatmap alternative: sector × material grid, 2×2 region facet ─────────────
# Rows = upstream sectors (top 20), columns = material, fill = log(upstream req)
# 4 panels: EU-Domestic, EU-Cross-border, RoW-Domestic, RoW-Cross-border
make_lf_heatmap <- function(df, mat_type_lbl, fill_var = 'pct_combined') {
  is_pct <- fill_var == 'pct_combined'

  fill_scale <- if (is_pct) {
    scale_fill_viridis_c(
      name = '% share\n(primary +\nsecondary)',
      option = 'plasma',
      labels = function(v) paste0(round(v, 0), '%')
    )
  } else {
    scale_fill_viridis_c(
      name = 'Upstream\nrequirement\n(model units)',
      option = 'plasma',
      trans = 'log1p',
      labels = scales::label_number(accuracy = 0.1)
    )
  }

  subtitle_txt <- if (is_pct) {
    'Top 20 upstream sectors. Colour = % share of combined primary + secondary upstream requirement.'
  } else {
    'Top 20 upstream sectors. Colour = log-scale upstream requirement (model units).'
  }

  df_hm <- df |>
    dplyr::filter(using_sector != 'Other') |>
    dplyr::mutate(
      using_sector = factor(using_sector, levels = levels(using_sector)),
      panel_label = paste0(region, '\n', dom_for)
    )

  panel_order <- c(
    'EU\nDomestic',
    'EU\nCross-border',
    'RoW\nDomestic',
    'RoW\nCross-border'
  )
  df_hm$panel_label <- factor(df_hm$panel_label, levels = panel_order)

  ggplot(df_hm, aes(x = material, y = using_sector, fill = .data[[fill_var]])) +
    geom_tile(colour = 'white', linewidth = 0.4) +
    fill_scale +
    facet_wrap(~panel_label, nrow = 1) +
    labs(
      title = paste0(
        'Full Supply-Chain Upstream Requirements (Leontief, ',
        mat_type_lbl,
        ')'
      ),
      subtitle = subtitle_txt,
      x = NULL,
      y = NULL
    ) +
    theme_grey(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
      axis.text.y = element_text(size = 9),
      strip.text = element_text(face = 'bold', size = 10),
      legend.position = 'right',
      panel.grid = element_blank(),
      plot.title = element_text(face = 'bold', size = 14),
      plot.subtitle = element_text(face = 'italic', size = 10)
    )
}

p_hm_pri <- make_lf_heatmap(lf_pri, 'Primary')
p_hm_sec <- make_lf_heatmap(lf_sec, 'Secondary')

for (pair in list(
  list(p = p_hm_pri, tag = 'primary'),
  list(p = p_hm_sec, tag = 'secondary')
)) {
  ggsave(
    sprintf('output/pdf/figures/leontief/p_leontief_%s_heatmap.pdf', pair$tag),
    pair$p,
    width = 14,
    height = 8
  )
  ggsave(
    sprintf('output/png/leontief/p_leontief_%s_heatmap.png', pair$tag),
    pair$p,
    width = 14,
    height = 8,
    dpi = 150
  )
}
cat('Saved: p_leontief primary/secondary heatmap\n')

# EU-only heatmaps — both % share and absolute levels
for (fv in c('pct_combined', 'abs_val')) {
  fsuffix <- if (fv == 'pct_combined') 'pct' else 'abs'
  p_hm_pri_eu <- make_lf_heatmap(lf_pri_eu, 'Primary', fill_var = fv)
  p_hm_sec_eu <- make_lf_heatmap(lf_sec_eu, 'Secondary', fill_var = fv)
  for (pair in list(
    list(p = p_hm_pri_eu, tag = 'primary'),
    list(p = p_hm_sec_eu, tag = 'secondary')
  )) {
    ggsave(
      sprintf(
        'output/pdf/figures/leontief/p_leontief_%s_heatmap_eu_%s.pdf',
        pair$tag,
        fsuffix
      ),
      pair$p,
      width = 12,
      height = 8
    )
    ggsave(
      sprintf(
        'output/png/leontief/p_leontief_%s_heatmap_eu_%s.png',
        pair$tag,
        fsuffix
      ),
      pair$p,
      width = 12,
      height = 8,
      dpi = 150
    )
  } # close inner for(pair)
} # close outer for(fv)
cat('Saved: p_leontief primary/secondary heatmap EU-only\n')

# ── Pure Leontief multipliers (L[k,i] without demand weighting) ───────────────
# Shows structural upstream requirements per unit of material sector output,
# making regions comparable regardless of their absolute economic size.
int_decomp_lf_pure <- lapply(1:nrow(mat_sectors), function(r) {
  ms <- mat_sectors[r, ]
  lapply(
    list(
      list(j = ms$from_j, type = 'Primary'),
      list(j = ms$to_j, type = 'Secondary')
    ),
    function(mt) {
      i_eu <- mt$j
      i_row <- mt$j + n_sec
      # No x[i] weighting — pure L[k,i] coefficients
      eu_dom <- L_minus_I[1:n_sec, i_eu]
      eu_imp <- L_minus_I[(n_sec + 1):(2 * n_sec), i_eu]
      rw_dom <- L_minus_I[(n_sec + 1):(2 * n_sec), i_row]
      rw_imp <- L_minus_I[1:n_sec, i_row]
      names(eu_dom) <- names(eu_imp) <- names(rw_dom) <-
        names(rw_imp) <- sec_list$label
      make_rows <- function(contrib, region_lbl, dom_lbl) {
        data.frame(
          material = ms$label,
          mat_type = mt$type,
          region = region_lbl,
          dom_for = dom_lbl,
          using_sector = names(contrib),
          abs_val = as.numeric(contrib),
          stringsAsFactors = FALSE
        )
      }
      bind_rows(
        make_rows(eu_dom, 'EU', 'Domestic'),
        make_rows(eu_imp, 'EU', 'Cross-border'),
        make_rows(rw_dom, 'RoW', 'Domestic'),
        make_rows(rw_imp, 'RoW', 'Cross-border')
      )
    }
  ) %>%
    bind_rows()
}) %>%
  bind_rows()

# Top sectors by total pure-multiplier magnitude (EU primary/secondary)
top_lf_pri_pure <- int_decomp_lf_pure %>%
  filter(mat_type == 'Primary', region == 'EU') %>%
  group_by(using_sector) %>%
  summarise(total_val = sum(abs_val, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(total_val)) %>%
  slice(1:top_n_lf) %>%
  pull(using_sector)

top_lf_sec_pure <- int_decomp_lf_pure %>%
  filter(mat_type == 'Secondary', region == 'EU') %>%
  group_by(using_sector) %>%
  summarise(total_val = sum(abs_val, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(total_val)) %>%
  slice(1:top_n_lf) %>%
  pull(using_sector)

lf_pri_pure_eu <- collapse_lf(
  filter(int_decomp_lf_pure, mat_type == 'Primary', region == 'EU'),
  top_lf_pri_pure
)
lf_sec_pure_eu <- collapse_lf(
  filter(int_decomp_lf_pure, mat_type == 'Secondary', region == 'EU'),
  top_lf_sec_pure
)

for (fv in c('pct_combined', 'abs_val')) {
  fsuffix <- if (fv == 'pct_combined') 'pct' else 'abs'
  p_hm_pri_pure <- make_lf_heatmap(lf_pri_pure_eu, 'Primary', fill_var = fv)
  p_hm_sec_pure <- make_lf_heatmap(lf_sec_pure_eu, 'Secondary', fill_var = fv)
  for (pair in list(
    list(p = p_hm_pri_pure, tag = 'primary'),
    list(p = p_hm_sec_pure, tag = 'secondary')
  )) {
    ggsave(
      sprintf(
        'output/pdf/figures/leontief/p_leontief_%s_heatmap_eu_pure_%s.pdf',
        pair$tag,
        fsuffix
      ),
      pair$p,
      width = 12,
      height = 8
    )
    ggsave(
      sprintf(
        'output/png/leontief/p_leontief_%s_heatmap_eu_pure_%s.png',
        pair$tag,
        fsuffix
      ),
      pair$p,
      width = 12,
      height = 8,
      dpi = 150
    )
  }
}
cat('Saved: p_leontief primary/secondary heatmap EU-only pure multipliers\n')

# ── Environmental intensity impacts by CE portfolio ────────────────────────────
# For a demand shift rho from sector 1 (primary) to sector 2 (secondary),
# first-order output change for all sectors k is:
#   Δx_k = rho * delta_1 * D * (L[k,2] - L[k,1])
# (same structure as ΔM1 but summed over all k with intensity weights)
#
# Environmental impact = Σ_k intensity_k * Δx_k
#   = rho * scale_FD * Σ_k intensity_k * (L[k,2] - L[k,1])
#
# intensity_k = Z1_emis_j-k / Z1_x-k  (emissions per unit output)
# Similarly for land, water, material intensity.
#
# For intermediate-demand shocks (production scenarios), the same formula holds
# with scale = Σ_j a[1,j]*x[j] (economy-wide intermediate).

# ── Extract sector-level intensities at t_sh ─────────────────────────────────
get_intensities <- function(prefix = 'Z1') {
  x_k <- sapply(1:n_sec, function(k) {
    as.numeric(sim_df[paste0(prefix, '_x-', k), t_sh])
  })
  emis_k <- sapply(1:n_sec, function(k) {
    as.numeric(sim_df[paste0(prefix, '_emis_j-', k), t_sh])
  })
  land_k <- sapply(1:n_sec, function(k) {
    as.numeric(sim_df[paste0(prefix, '_land_j-', k), t_sh])
  })
  water_k <- sapply(1:n_sec, function(k) {
    as.numeric(sim_df[paste0(prefix, '_water_j-', k), t_sh])
  })
  mat_k <- sapply(1:n_sec, function(k) {
    vn <- paste0(prefix, '_mu_mat-', k)
    if (vn %in% rownames(sim_df)) as.numeric(sim_df[vn, t_sh]) else NA_real_
  })
  # intensity = quantity per unit of output
  list(
    emis = ifelse(x_k > 0, emis_k / x_k, 0),
    land = ifelse(x_k > 0, land_k / x_k, 0),
    water = ifelse(x_k > 0, water_k / x_k, 0),
    mat = ifelse(x_k > 0, mat_k / x_k, 0)
  )
}

sim_df <- as.data.frame(sim)
int_z1 <- get_intensities('Z1')
int_z2 <- get_intensities('Z2')

# ── Compute Δenvironmental impact for each material × portfolio ───────────────
intensity_rows <- lapply(1:nrow(mat_sectors), function(r) {
  ms <- mat_sectors[r, ]
  cs <- get_channel_scales(ms$from_j)

  # EU primary → EU secondary
  from_eu <- ms$from_j
  to_eu <- ms$to_j
  # Δx_k = (L[k, to_j] - L[k, from_j]) — EU Leontief block (rows 1..54, cols 1..54)
  delta_L_eu <- L[1:n_sec, to_eu] - L[1:n_sec, from_eu]

  lapply(names(portfolios), function(pname) {
    p <- portfolios[[pname]]
    pscale <- sum(cs$fd_dom[p$fd], na.rm = TRUE) + if (p$int) cs$int_dom else 0

    delta_x_eu <- rho * pscale * delta_L_eu # length 54

    d_emis <- sum(int_z1$emis * delta_x_eu, na.rm = TRUE)
    d_land <- sum(int_z1$land * delta_x_eu, na.rm = TRUE)
    d_water <- sum(int_z1$water * delta_x_eu, na.rm = TRUE)
    d_mat <- sum(int_z1$mat * delta_x_eu, na.rm = TRUE)

    data.frame(
      material = ms$label,
      portfolio = portfolios[[pname]]$label,
      port_key = pname,
      d_emis = round(d_emis, 4),
      d_land = round(d_land, 4),
      d_water = round(d_water, 4),
      d_mat = round(d_mat, 4),
      stringsAsFactors = FALSE
    )
  }) %>%
    bind_rows()
}) %>%
  bind_rows()

cat('\n=== Environmental impacts by portfolio (EU region, rho=0.2, t=70) ===\n')
cat('d_emis = Delta CO2 (10t CO2eq) | d_mat = Delta material intensity\n')
cat(
  'Note: land/water intensities from MARIO satellite data (ha and ton per million USD)\n\n'
)
intensity_rows %>%
  filter(port_key %in% c('HH', 'Gov', 'INT', 'Full')) %>%
  select(material, portfolio, d_emis, d_mat) %>%
  mutate(
    across(c(d_emis, d_mat), ~ round(.x, 3)),
    portfolio = gsub('\n', ' ', portfolio)
  ) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

# ── Shared impact heatmap builder ─────────────────────────────────────────────
intensity_rows$portfolio <- factor(
  intensity_rows$portfolio,
  levels = port_order
)
intensity_rows$material <- factor(
  gsub(' ', '\n', intensity_rows$material),
  levels = mat_order2_nl
)

make_impact_heatmap <- function(df, var, legend_label, title, subtitle) {
  vals <- df[[var]]
  ggplot(df, aes(x = material, y = portfolio, fill = .data[[var]])) +
    geom_tile(colour = 'white', linewidth = 0.5) +
    geom_text(
      aes(
        label = sprintf('%.0f', .data[[var]]),
        colour = .data[[var]] < quantile(vals, 0.25, na.rm = TRUE)
      ),
      size = 2.8,
      show.legend = FALSE
    ) +
    scale_colour_manual(values = c('TRUE' = 'white', 'FALSE' = 'black')) +
    scale_fill_gradient2(
      low = '#4393c3',
      mid = '#f7f7f7',
      high = '#d6604d',
      midpoint = 0,
      name = legend_label
    ) +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL) +
    theme_grey(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
      axis.text.y = element_text(size = 9),
      panel.grid = element_blank(),
      plot.title = element_text(face = 'bold')
    )
}

sub_base <- 'Negative = reduction. EU, analytical first-order, rho=0.2, baseline t=70.'

p4_emis <- make_impact_heatmap(
  intensity_rows,
  'd_emis',
  'Delta\n(10t CO2eq)',
  'CO2 Emission Impact of CE Portfolios by Material Sector',
  sub_base
)

p4_mat <- make_impact_heatmap(
  intensity_rows,
  'd_mat',
  'Delta\n(mat. intensity)',
  'Material Intensity Impact of CE Portfolios by Material Sector',
  sub_base
)

p4_land <- make_impact_heatmap(
  intensity_rows,
  'd_land',
  'Delta\n(ha)',
  'Land Use Impact of CE Portfolios by Material Sector',
  sub_base
)

p4_water <- make_impact_heatmap(
  intensity_rows,
  'd_water',
  'Delta\n(l)',
  'Water Use Impact of CE Portfolios by Material Sector',
  sub_base
)

for (nm in c('emis', 'mat', 'land', 'water')) {
  p <- get(paste0('p4_', nm))
  ggsave(
    paste0('output/pdf/figures/impact/p_impact_', nm, '.pdf'),
    p,
    width = 10,
    height = 8
  )
  ggsave(
    paste0('output/png/impact/p_impact_', nm, '.png'),
    p,
    width = 10,
    height = 8,
    dpi = 150
  )
}
cat('Saved: p_impact_emis, p_impact_mat, p_impact_land, p_impact_water\n')

# Save data for QMD
saveRDS(
  list(
    struct_long = struct_long,
    port_rows = port_rows,
    int_decomp = int_decomp,
    intensity_rows = intensity_rows
  ),
  'data/portfolio_analysis.RDS'
)
cat('Saved: data/portfolio_analysis.RDS\n')
