suppressPackageStartupMessages({
  library(openxlsx); library(dplyr); library(tidyr); library(ggplot2); library(scales)
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

x_base <- c(
  sapply(1:n_sec, function(j) sim[paste0('Z1_x-', j), t_sh]),
  sapply(1:n_sec, function(j) sim[paste0('Z2_x-', j), t_sh])
)

# ── Cross-border import propensities from MARIO (empirical, by FD channel) ────
# eta_k[j] = share of EU final demand for sector j that is imported from RoW.
# Source: MARIO - Aggregated (2).xlsx, F21 block (RoW supply -> EU final demand).
mario_ch <- read.csv('data/mrio_fd_channel_shares.csv', stringsAsFactors = FALSE)
# Vectors indexed 1..54 (matching LEEDS sector order)
eta_hh_ch   <- mario_ch$eta_RoW_hh    # HH import propensity per sector
eta_gov_ch  <- mario_ch$eta_RoW_gov   # Gov import propensity per sector
eta_gfcf_ch <- mario_ch$eta_RoW_gfcf  # GFCF import propensity per sector
# Public investment treated as domestic (eta = 0), as calibration prior

# ── Material sectors to analyse ───────────────────────────────────────────────
mat_sectors <- data.frame(
  label   = c('Food', 'Carbon Energy', 'Plastics', 'Wood',
              'Pulp', 'Glass', 'Cement', 'Metals'),
  from_j  = c(7,  31, 17, 11, 13, 21, 24, 26),
  to_j    = c(8,  32, 18, 12, 14, 22, 25, 27),
  stringsAsFactors = FALSE
)

# ── FD channel definitions ────────────────────────────────────────────────────
channels <- data.frame(
  key    = c('HH',   'Gov',   'FirmInv', 'PubInv'),
  label  = c('HH consumption', 'Gov consumption',
             'Firm investment', 'Public investment'),
  var    = c('Z1_c', 'Z1_g', 'Z1_id', 'Z1_id_g'),
  share  = c('Z1_beta', 'Z1_sigma', 'Z1_iota', 'Z1_iota_g'),
  stringsAsFactors = FALSE
)

# ── Portfolio definitions ─────────────────────────────────────────────────────
portfolios <- list(
  HH        = list(fd = c('HH'),                          int = FALSE, label = 'private consumption\n(households)'),
  Gov       = list(fd = c('Gov'),                          int = FALSE, label = 'public consumption\n(government)'),
  AllCons   = list(fd = c('HH', 'Gov'),                   int = FALSE, label = 'all consumption\n(households + government)'),
  PrivInv   = list(fd = c('FirmInv'),                     int = FALSE, label = 'private investment\n(firms)'),
  PubInv    = list(fd = c('PubInv'),                      int = FALSE, label = 'public investment'),
  AllInvest = list(fd = c('FirmInv', 'PubInv'),           int = FALSE, label = 'all investment\n(private + public)'),
  PubSector = list(fd = c('Gov', 'PubInv'),               int = FALSE, label = 'public sector\n(consumption + investment)'),
  FD        = list(fd = c('HH','Gov','FirmInv','PubInv'), int = FALSE, label = 'all final demand'),
  INT       = list(fd = character(0),                     int = TRUE,  label = 'intermediate\n(firm inputs)'),
  Full      = list(fd = c('HH','Gov','FirmInv','PubInv'), int = TRUE,  label = 'full CE\n(all channels)')
)

# ── Compute channel scales for each material sector, by region ────────────────
# prefix: 'Z1' (EU) or 'Z2' (RoW); local_j: sector index 1..54 within region
# A matrix row: Z1 -> j, Z2 -> j + n_sec
get_channel_scales <- function(local_j, prefix = 'Z1') {
  a_row_idx <- if (prefix == 'Z1') local_j else local_j + n_sec
  reg_channels <- channels
  reg_channels$var   <- sub('^Z1', prefix, channels$var)
  reg_channels$share <- sub('^Z1', prefix, channels$share)

  # Domestic FD scales: delta[k,j] * D[k]
  fd_dom <- sapply(reg_channels$key, function(k) {
    row <- reg_channels[reg_channels$key == k, ]
    sn  <- paste0(row$share, '-', local_j)
    if (sn %in% rownames(sim))
      as.numeric(sim[sn, t_sh]) * as.numeric(sim[row$var, t_sh])
    else 0
  })
  names(fd_dom) <- channels$key

  # Cross-border import demand: eta_k[j] * D[k]  (EU only; F21 block from MARIO).
  # For RoW (Z2), cross-border demand flows the other direction (= EU exports),
  # already captured in "Net exports / other", so fd_imp = 0 for Z2.
  if (prefix == 'Z1') {
    D <- sapply(reg_channels$key, function(k) {
      row <- reg_channels[reg_channels$key == k, ]
      as.numeric(sim[row$var, t_sh])
    })
    names(D) <- channels$key
    fd_imp <- c(
      HH      = eta_hh_ch[local_j]   * D['HH'],
      Gov     = eta_gov_ch[local_j]  * D['Gov'],
      FirmInv = eta_gfcf_ch[local_j] * D['FirmInv'],
      PubInv  = 0
    )
  } else {
    fd_imp <- c(HH = 0, Gov = 0, FirmInv = 0, PubInv = 0)
  }

  int <- sum(A[a_row_idx, ] * x_base, na.rm = TRUE)
  x_i <- as.numeric(sim[paste0(prefix, '_x-', local_j), t_sh])
  list(fd_dom = fd_dom, fd_imp = fd_imp,
       fd = fd_dom + fd_imp,   # total per channel (for portfolio scale)
       int = int, x_i = x_i)
}

# ── Build demand structure table — primary & secondary × EU & RoW ────────────
build_struct_rows <- function(mat_label, sector_j, mat_type, region, prefix) {
  cs        <- get_channel_scales(sector_j, prefix)
  net_other <- max(cs$x_i - sum(cs$fd) - cs$int, 0)
  bind_rows(
    # Domestic components
    data.frame(material=mat_label, mat_type=mat_type, region=region, channel='HH consumption (dom)',    channel_type='Final Demand', scale=unname(cs$fd_dom['HH'])),
    data.frame(material=mat_label, mat_type=mat_type, region=region, channel='Gov consumption (dom)',   channel_type='Final Demand', scale=unname(cs$fd_dom['Gov'])),
    data.frame(material=mat_label, mat_type=mat_type, region=region, channel='Firm investment (dom)',   channel_type='Final Demand', scale=unname(cs$fd_dom['FirmInv'])),
    data.frame(material=mat_label, mat_type=mat_type, region=region, channel='Public investment',       channel_type='Final Demand', scale=unname(cs$fd_dom['PubInv'])),
    # Cross-border import demand (F21 block)
    data.frame(material=mat_label, mat_type=mat_type, region=region, channel='HH consumption (import)',  channel_type='Import FD', scale=unname(cs$fd_imp['HH'])),
    data.frame(material=mat_label, mat_type=mat_type, region=region, channel='Gov consumption (import)', channel_type='Import FD', scale=unname(cs$fd_imp['Gov'])),
    data.frame(material=mat_label, mat_type=mat_type, region=region, channel='Firm investment (import)', channel_type='Import FD', scale=unname(cs$fd_imp['FirmInv'])),
    # Intermediate and other
    data.frame(material=mat_label, mat_type=mat_type, region=region, channel='Intermediate',             channel_type='Intermediate', scale=cs$int),
    data.frame(material=mat_label, mat_type=mat_type, region=region, channel='Net exports / other',      channel_type='Other',        scale=net_other)
  )
}

struct_long <- lapply(1:nrow(mat_sectors), function(r) {
  ms <- mat_sectors[r, ]
  bind_rows(
    build_struct_rows(ms$label, ms$from_j, 'Primary',   'EU',  'Z1'),
    build_struct_rows(ms$label, ms$to_j,   'Secondary', 'EU',  'Z1'),
    build_struct_rows(ms$label, ms$from_j, 'Primary',   'RoW', 'Z2'),
    build_struct_rows(ms$label, ms$to_j,   'Secondary', 'RoW', 'Z2')
  )
}) %>% bind_rows()

# Order materials by EU primary intermediate share (ascending) — FD-heavy on left
mat_order <- struct_long %>%
  filter(mat_type == 'Primary', region == 'EU') %>%
  group_by(material) %>%
  summarise(x_i = sum(scale), int = sum(scale[channel == 'Intermediate'])) %>%
  mutate(pct_int = int / x_i) %>%
  arrange(pct_int) %>%
  pull(material)

chan_levels <- c(
  'Net exports / other',
  'Public investment',
  'Firm investment (import)', 'Firm investment (dom)',
  'Gov consumption (import)', 'Gov consumption (dom)',
  'HH consumption (import)',  'HH consumption (dom)',
  'Intermediate'
)
chan_colours <- c(
  'Intermediate'              = '#2c7bb6',
  'HH consumption (dom)'      = '#d7191c',
  'HH consumption (import)'   = '#f4a582',   # lighter red
  'Gov consumption (dom)'     = '#e85d04',
  'Gov consumption (import)'  = '#fddbc7',   # lighter orange
  'Firm investment (dom)'     = '#fdae61',
  'Firm investment (import)'  = '#fff2cc',   # lighter yellow
  'Public investment'         = '#fee090',
  'Net exports / other'       = '#cccccc'
)

struct_long$material <- factor(struct_long$material, levels = mat_order)
struct_long$channel  <- factor(struct_long$channel,  levels = chan_levels)
struct_long$mat_type <- factor(struct_long$mat_type,  levels = c('Primary', 'Secondary'))
struct_long$region   <- factor(struct_long$region,    levels = c('EU', 'RoW'))

# ── PLOT 1: facet_grid(region ~ material), Primary vs Secondary bars ──────────
p1 <- ggplot(struct_long,
             aes(x = mat_type, y = scale, fill = channel)) +
  geom_col(width = 0.75, colour = 'white', linewidth = 0.2) +
  facet_grid(region ~ material, scales = 'free_y') +
  scale_fill_manual(values = chan_colours, name = 'Channel',
                    guide = guide_legend(ncol = 1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.06))) +
  labs(
    title    = 'Demand Structure: Primary vs Secondary Material Sectors (EU & RoW, baseline t = 70)',
    subtitle = 'Gross output = intermediate + domestic FD + cross-border FD (import) + net exports\nLight shades = cross-border import demand (F21 block, empirical from MARIO)',
    x = NULL, y = 'Output (model units)'
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x        = element_text(angle = 0, hjust = 0.5, size = 8),
    strip.text.x       = element_text(face = 'bold', size = 10),
    strip.text.y       = element_text(face = 'bold', size = 11, angle = 0),
    legend.position    = 'right',
    panel.grid.major.x = element_blank(),
    plot.title         = element_text(face = 'bold')
  )

ggsave('output/figures/p_demand_structure.pdf', p1, width = 16, height = 8)
ggsave('output/figures/p_demand_structure.png', p1, width = 16, height = 8, dpi = 150)
cat('Saved: p_demand_structure\n')

# ── Build portfolio effectiveness table ───────────────────────────────────────
port_rows <- lapply(1:nrow(mat_sectors), function(r) {
  ms      <- mat_sectors[r, ]
  cs      <- get_channel_scales(ms$from_j)
  struct1 <- L[ms$from_j, ms$to_j] - L[ms$from_j, ms$from_j]

  lapply(names(portfolios), function(pname) {
    p <- portfolios[[pname]]
    # Portfolio scale: domestic FD only (consistent with EU Leontief structural factor).
    # Cross-border import demand is shown in the demand structure figure (Plot 1) but
    # excluded here because a CE substitution in imported goods requires the RoW
    # structural factor (L[j+54, k+54]), not the EU struct1 used here.
    pscale_dom <- sum(cs$fd_dom[p$fd], na.rm = TRUE) + if (p$int) cs$int else 0
    pscale_imp <- sum(cs$fd_imp[p$fd], na.rm = TRUE)   # informational only
    data.frame(
      material      = ms$label,
      portfolio     = p$label,
      port_key      = pname,
      scale         = pscale_dom,
      scale_imp     = round(pscale_imp, 3),   # cross-border addendum
      x_i           = cs$x_i,
      struct1       = struct1,
      DeltaM1       = rho * pscale_dom * struct1,
      pct_of_full   = pscale_dom / cs$x_i * 100,
      stringsAsFactors = FALSE
    )
  }) %>% bind_rows()
}) %>% bind_rows()

# Print summary
cat('\n=== Portfolio effectiveness (pct_of_full = portfolio scale / x_i) ===\n')
port_rows %>%
  select(material, portfolio, scale, pct_of_full, DeltaM1) %>%
  mutate(across(c(scale, pct_of_full, DeltaM1), ~round(.x, 2))) %>%
  pivot_wider(names_from = portfolio, values_from = pct_of_full,
              id_cols = material) %>%
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
mat_order2 <- mat_order  # same ordering as bar chart (ascending INT share)

port_rows$portfolio <- factor(port_rows$portfolio, levels = port_order)
# Replace spaces with newlines in material labels for horizontal x-axis readability
mat_order2_nl <- gsub(' ', '\n', mat_order2)
port_rows$material <- factor(gsub(' ', '\n', port_rows$material), levels = mat_order2_nl)

p2 <- ggplot(port_rows, aes(x = material, y = portfolio, fill = pct_of_full)) +
  geom_tile(colour = 'white', linewidth = 0.5) +
  geom_text(aes(label = sprintf('%.0f%%', pct_of_full),
                colour = pct_of_full > 55),
            size = 3.2, show.legend = FALSE) +
  scale_colour_manual(values = c('TRUE' = 'white', 'FALSE' = 'black')) +
  scale_fill_gradientn(
    colours = c('#f7f7f7', '#d1e5f0', '#4393c3', '#2166ac', '#053061'),
    limits  = c(0, 100), name = '% of full-CE\npotential'
  ) +
  labs(
    title    = 'Circular Economy Policy Portfolio Effectiveness by Material Sector',
    subtitle = 'Cell value = portfolio scale / x_i x 100  (struct factor ~ -1 for all; scale dominates)',
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x  = element_text(angle = 0, hjust = 0.5, size = 11),
    axis.text.y  = element_text(size = 10),
    plot.title   = element_text(face = 'bold'),
    panel.grid   = element_blank()
  )

ggsave('output/figures/p_portfolio_heatmap.pdf', p2, width = 10, height = 6)
ggsave('output/figures/p_portfolio_heatmap.png', p2, width = 10, height = 6, dpi = 150)
cat('Saved: p_portfolio_heatmap\n')

# ── PLOT 3: Intermediate demand decomposition — facet_grid(material ~ region) ─
sec_list <- read.csv('data/sector_list.csv', stringsAsFactors=FALSE) %>%
            mutate(sector_code = as.integer(sector_code),
                   label = gsub('<br>', ' ', label))
all_labels <- c(sec_list$label, paste0('RoW: ', sec_list$label))

top_n_sectors <- 5

# Step 1: find top-N EU using sectors per material
top_per_mat <- lapply(1:nrow(mat_sectors), function(r) {
  i       <- mat_sectors$from_j[r]
  contrib <- A[i, 1:n_sec] * x_base[1:n_sec]
  names(contrib) <- sec_list$label
  names(sort(contrib, decreasing = TRUE))[1:top_n_sectors]
})
names(top_per_mat) <- mat_sectors$label

# Keep only sectors appearing in >= 2 materials' top lists
sector_freq <- table(unlist(top_per_mat))
sectors_keep <- names(sector_freq[sector_freq >= 2])
# Also always include the single top sector for each material
sectors_keep <- union(sectors_keep, sapply(top_per_mat, `[`, 1))

# Step 2: for each material × region, compute % of total intermediate demand
int_decomp <- lapply(1:nrow(mat_sectors), function(r) {
  ms    <- mat_sectors[r, ]
  i_eu  <- ms$from_j
  i_row <- ms$from_j + n_sec

  contrib_eu  <- A[i_eu,  1:n_sec]          * x_base[1:n_sec]
  contrib_row <- A[i_row, (n_sec+1):(2*n_sec)] * x_base[(n_sec+1):(2*n_sec)]
  names(contrib_eu) <- names(contrib_row) <- sec_list$label

  total_eu  <- sum(contrib_eu,  na.rm = TRUE)
  total_row <- sum(contrib_row, na.rm = TRUE)

  bind_rows(
    data.frame(material=ms$label, region='EU',
               using_sector=sectors_keep,
               pct=as.numeric(contrib_eu[sectors_keep])  / total_eu  * 100,
               stringsAsFactors=FALSE),
    data.frame(material=ms$label, region='RoW',
               using_sector=sectors_keep,
               pct=as.numeric(contrib_row[sectors_keep]) / total_row * 100,
               stringsAsFactors=FALSE)
  )
}) %>% bind_rows()

# Order y-axis by mean EU % across materials (most universally important at top)
sector_order <- int_decomp %>%
  filter(region == 'EU') %>%
  group_by(using_sector) %>%
  summarise(mean_pct = mean(pct, na.rm = TRUE)) %>%
  arrange(mean_pct) %>%
  pull(using_sector)

int_decomp$using_sector <- factor(int_decomp$using_sector, levels = sector_order)
int_decomp$material     <- factor(int_decomp$material,     levels = mat_order)
int_decomp$region       <- factor(int_decomp$region,       levels = c('EU', 'RoW'))

p3 <- ggplot(int_decomp, aes(y = using_sector, x = pct)) +
  geom_col(aes(fill = region), width = 0.7) +
  facet_grid(region ~ material) +
  scale_fill_hue(guide = 'none') +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1)),
                     labels = function(x) paste0(x, '%')) +
  labs(
    title    = 'Key Using Sectors per Primary Material: Share of Intermediate Demand',
    subtitle = 'Bars = % of total intermediate demand a[i,j]*x_j; sectors in top-5 for >= 2 materials',
    x = '% of total intermediate demand', y = NULL
  ) +
  theme_bw(base_size = 9) +
  theme(
    strip.text.x         = element_text(face = 'bold', size = 9),
    strip.text.y         = element_text(face = 'bold', size = 9, angle = 0),
    strip.background     = element_rect(fill = 'grey92', colour = NA),
    axis.text.y          = element_text(size = 10),
    axis.text.x          = element_text(size = 9),
    panel.grid.major.y   = element_blank(),
    panel.background     = element_rect(fill = 'white'),
    plot.background      = element_rect(fill = 'white', colour = NA),
    plot.title           = element_text(face = 'bold', size = 18),
    plot.subtitle        = element_text(size = 13)
  )

ggsave('output/figures/p_intermediate_decomp.pdf', p3, width = 20, height = 8)
ggsave('output/figures/p_intermediate_decomp.png', p3, width = 20, height = 8, dpi = 130)
cat('Saved: p_intermediate_decomp\n')

# Save data for QMD
saveRDS(list(struct_long = struct_long, port_rows = port_rows, int_decomp = int_decomp),
        'data/portfolio_analysis.RDS')
cat('Saved: data/portfolio_analysis.RDS\n')
