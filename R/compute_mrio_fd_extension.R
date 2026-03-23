suppressPackageStartupMessages({
  library(openxlsx); library(dplyr); library(tidyr); library(ggplot2)
})

setwd("/Users/parvulesco/Documents/R/LEEDS_MODEL")

# ── Parameters ────────────────────────────────────────────────────────────────
t_sh  <- 70
n_sec <- 54   # K sectors per region
kappa <- 0.5  # government home-bias scalar (gov import intensity = kappa * eta)
lambda <- 1.2 # firm investment import-intensity scalar (inv import = lambda * eta)

# ── Load ──────────────────────────────────────────────────────────────────────
A_raw    <- read.xlsx("data/initial_state_2026.xlsx", sheet = "A.matrix",
                       colNames = TRUE, rowNames = TRUE)
A        <- as.matrix(A_raw)
L        <- solve(diag(nrow(A)) - A)          # 108 x 108 Leontief inverse

baseline <- readRDS("output/scenarios/baseline_2026.RDS")
sim      <- baseline$simulation

# ── Extract baseline aggregates at t_sh ───────────────────────────────────────
# Region Z1 (EU) FD aggregates
C1   <- sim["Z1_c",    t_sh]
G1   <- sim["Z1_g",    t_sh]
Id1  <- sim["Z1_id",   t_sh]
Idg1 <- sim["Z1_id_g", t_sh]
Imp1 <- sim["Z1_imp",  t_sh]   # total final + intermediate imports (used as proxy)

cat("=== Z1 (EU) FD aggregates at t =", t_sh, "===\n")
cat(sprintf("  C  (HH consumption)    = %.3f\n", C1))
cat(sprintf("  G  (gov consumption)   = %.3f\n", G1))
cat(sprintf("  Id (private investment)= %.3f\n", Id1))
cat(sprintf(" Idg (public investment) = %.3f\n", Idg1))
cat(sprintf(" Imp (total imports)     = %.3f\n\n", Imp1))

# ── Extract sector-level FD shares and eta ────────────────────────────────────
eta_Z1   <- sapply(1:n_sec, function(j) sim[paste0("Z1_eta-",   j), t_sh])
beta_Z1  <- sapply(1:n_sec, function(j) sim[paste0("Z1_beta-",  j), t_sh])
sigma_Z1 <- sapply(1:n_sec, function(j) sim[paste0("Z1_sigma-", j), t_sh])
iota_Z1  <- sapply(1:n_sec, function(j) sim[paste0("Z1_iota-",  j), t_sh])
iota_g_Z1<- sapply(1:n_sec, function(j) sim[paste0("Z1_iota_g-",j), t_sh])

# Current domestic FD vector (Z1 sectors only, rows 1..54 of d)
d_dom_current <- beta_Z1 * C1 + sigma_Z1 * G1 + iota_Z1 * Id1 + iota_g_Z1 * Idg1

cat("=== sum(d_dom_current) vs (C1+G1+Id1+Idg1) ===\n")
cat(sprintf("  sum(d_dom): %.3f  |  C+G+Id+Idg: %.3f\n\n",
            sum(d_dom_current), C1 + G1 + Id1 + Idg1))

# ── Import demand decomposition ───────────────────────────────────────────────
# Z1_eta[j] is the aggregate import propensity for sector j.
# Total imports of sector j goods: import_j = eta_Z1[j] * Imp1
#
# We DO NOT have channel-disaggregated import data. We distribute import_j
# across FD channels using FD-weighted import intensities:
#
#   beta  channel: intensity = 1.0   (eta applies fully to HH)
#   sigma channel: intensity = kappa (home-bias in gov procurement)
#   iota  channel: intensity = lambda (capital goods are import-intensive)
#   iota_g channel: intensity = 0    (public investment treated as domestic)
#
# Weight for each channel = FD_aggregate * intensity_scalar
# Normalise so they sum to 1 => proportions of import_j attributed per channel.

w_beta  <- C1   * 1.0
w_sigma <- G1   * kappa
w_iota  <- Id1  * lambda
w_total <- w_beta + w_sigma + w_iota

import_j <- eta_Z1 * Imp1    # total imports by sector j (length 54)

# Import demand attributed to each channel, per sector (length 54 each)
d_imp_beta  <- import_j * (w_beta  / w_total)
d_imp_sigma <- import_j * (w_sigma / w_total)
d_imp_iota  <- import_j * (w_iota  / w_total)
d_imp_total <- d_imp_beta + d_imp_sigma + d_imp_iota   # = import_j (check)

cat("=== Import demand decomposition check ===\n")
cat(sprintf("  sum(import_j)      = %.3f\n", sum(import_j)))
cat(sprintf("  sum(d_imp_total)   = %.3f   (should equal import_j total)\n", sum(d_imp_total)))
cat(sprintf("  beta  share: %.1f%%  sigma share: %.1f%%  iota share: %.1f%%\n\n",
            sum(d_imp_beta)/sum(d_imp_total)*100,
            sum(d_imp_sigma)/sum(d_imp_total)*100,
            sum(d_imp_iota)/sum(d_imp_total)*100))

# ── Build extended 2N demand vector ───────────────────────────────────────────
# In the MRIO absorption framework:
#   d_full[1..54]   = Z1 domestic absorption (demand for EU sector j output)
#   d_full[55..108] = Z1 import absorption   (demand for RoW sector j output)
#
# Note: the current model already places Z1 domestic demand in rows 1..54 of d.
# We're now making the cross-regional (import) demand explicit in rows 55..108.
#
# For domestic demand, we keep the existing beta/sigma/iota/iota_g shares as-is.
# (These are calibrated as domestic-only shares from the MRIO source.)

d_full <- numeric(2 * n_sec)
d_full[1:n_sec]              <- d_dom_current          # Z1 domestic absorption
d_full[(n_sec + 1):(2*n_sec)] <- d_imp_total            # Z1 import demand → Z2 sectors

cat("=== Extended demand vector summary ===\n")
cat(sprintf("  sum(d_full[1:54])   = %.3f  (EU domestic absorption)\n", sum(d_full[1:n_sec])))
cat(sprintf("  sum(d_full[55:108]) = %.3f  (EU import demand from RoW)\n", sum(d_full[(n_sec+1):(2*n_sec)])))
cat(sprintf("  total               = %.3f\n\n", sum(d_full)))

# ── Apply Leontief inverse ─────────────────────────────────────────────────────
# x_full = (I - A)^{-1} * d_full
# x_full[1..54]   = EU output induced by EU demand (domestic + supply-chain)
# x_full[55..108] = RoW output induced by EU demand (EU imports + RoW supply-chain)
x_full <- L %*% d_full

# For comparison: current model output (domestic demand only, rows 1..54)
d_dom_full <- numeric(2 * n_sec)
d_dom_full[1:n_sec] <- d_dom_current
x_dom <- L %*% d_dom_full

cat("=== Output induced by EU demand: full (domestic + imports) vs domestic-only ===\n")
cat(sprintf("  EU output (x[1..54]):   full = %.3f  |  dom-only = %.3f  |  diff = %.3f\n",
            sum(x_full[1:n_sec]), sum(x_dom[1:n_sec]),
            sum(x_full[1:n_sec]) - sum(x_dom[1:n_sec])))
cat(sprintf("  RoW output (x[55..108]): full = %.3f  |  dom-only = %.3f  |  diff = %.3f\n\n",
            sum(x_full[(n_sec+1):(2*n_sec)]), sum(x_dom[(n_sec+1):(2*n_sec)]),
            sum(x_full[(n_sec+1):(2*n_sec)]) - sum(x_dom[(n_sec+1):(2*n_sec)])))

# ── Build results table ───────────────────────────────────────────────────────
sec_list <- read.csv("data/sector_list.csv", stringsAsFactors = FALSE) %>%
  mutate(label = gsub("<br>", " ", label))

results <- data.frame(
  sector_j    = 1:n_sec,
  label       = sec_list$label[1:n_sec],
  eta         = round(eta_Z1, 5),
  import_j    = round(import_j, 3),
  d_imp_beta  = round(d_imp_beta, 3),
  d_imp_sigma = round(d_imp_sigma, 3),
  d_imp_iota  = round(d_imp_iota, 3),
  x_row_dom   = round(x_dom[(n_sec+1):(2*n_sec)], 3),  # RoW output, dom-only demand
  x_row_full  = round(x_full[(n_sec+1):(2*n_sec)], 3)  # RoW output, full demand
) %>%
  mutate(x_row_gain = round(x_row_full - x_row_dom, 3))

cat("=== Top 10 sectors by import demand (EU imports from RoW) ===\n")
print(results %>% arrange(desc(import_j)) %>%
        select(sector_j, label, eta, import_j, d_imp_beta, d_imp_sigma, d_imp_iota) %>%
        head(10), row.names = FALSE)

cat("\n=== Top 10 sectors by additional RoW output (full vs domestic-only) ===\n")
print(results %>% arrange(desc(x_row_gain)) %>%
        select(sector_j, label, import_j, x_row_dom, x_row_full, x_row_gain) %>%
        head(10), row.names = FALSE)

# ── Summary: import demand shares by FD channel ───────────────────────────────
cat("\n=== Import demand attribution by FD channel ===\n")
cat(sprintf("  kappa (gov home-bias)      = %.2f\n", kappa))
cat(sprintf("  lambda (inv import scalar) = %.2f\n\n", lambda))
attr_df <- data.frame(
  Channel             = c("HH consumption (beta)", "Gov consumption (sigma)",
                           "Firm investment (iota)", "Public investment (iota_g)"),
  Import_intensity    = c("eta[j]", paste0(kappa, " * eta[j]"),
                           paste0(lambda, " * eta[j]"), "0"),
  Total_import_demand = c(round(sum(d_imp_beta), 3),
                           round(sum(d_imp_sigma), 3),
                           round(sum(d_imp_iota), 3),
                           0),
  Share_pct           = c(round(sum(d_imp_beta)/sum(import_j)*100, 1),
                           round(sum(d_imp_sigma)/sum(import_j)*100, 1),
                           round(sum(d_imp_iota)/sum(import_j)*100, 1),
                           0)
)
print(attr_df, row.names = FALSE)

# ── Plot: import demand by sector, stacked by channel ─────────────────────────
plot_df <- data.frame(
  sector_j = rep(1:n_sec, 3),
  label    = rep(sec_list$label[1:n_sec], 3),
  channel  = rep(c("HH consumption", "Gov consumption", "Firm investment"), each = n_sec),
  value    = c(d_imp_beta, d_imp_sigma, d_imp_iota)
) %>%
  filter(value > 0) %>%
  mutate(label = factor(label, levels = sec_list$label[1:n_sec][order(import_j)]))

# Show only top 20 sectors by total import demand for readability
top20_labels <- results %>% arrange(desc(import_j)) %>% slice(1:20) %>% pull(label)
plot_df_top <- plot_df %>% filter(label %in% top20_labels)

p_import <- ggplot(plot_df_top, aes(x = reorder(label, value, sum),
                                     y = value, fill = channel)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Set2", name = "FD channel") +
  labs(
    title    = "EU import demand from RoW by sector and FD channel",
    subtitle = sprintf("Disaggregated using Z1_eta (aggregate import propensity); kappa=%.1f, lambda=%.1f", kappa, lambda),
    x        = NULL,
    y        = "Import demand (model units)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    legend.position  = "bottom"
  )

ggsave("output/figures/p_import_decomp.pdf", p_import, width = 10, height = 8)
ggsave("output/figures/p_import_decomp.png", p_import, width = 10, height = 8, dpi = 150)
cat("\nSaved: output/figures/p_import_decomp.pdf/.png\n")

# ── Save results RDS ──────────────────────────────────────────────────────────
mrio_fd <- list(
  d_dom_current  = d_dom_current,
  d_imp_total    = d_imp_total,
  d_imp_beta     = d_imp_beta,
  d_imp_sigma    = d_imp_sigma,
  d_imp_iota     = d_imp_iota,
  d_full         = d_full,
  x_dom          = x_dom,
  x_full         = x_full,
  params         = list(kappa = kappa, lambda = lambda, t_sh = t_sh),
  results_table  = results
)
saveRDS(mrio_fd, "data/mrio_fd_extension.RDS")
cat("Saved: data/mrio_fd_extension.RDS\n")
