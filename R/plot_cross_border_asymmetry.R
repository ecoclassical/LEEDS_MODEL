## Cross-Border Asymmetry Figure
## 5 scatter panels (n, emis, x_mat, b_s, cab) + dual inequality panel
## Points coloured by scenario (rainbow), highlighted by transmission pattern
## Output: output/png_figures/p_cross_border_asymmetry.png
##         output/pdf_figures/p_cross_border_asymmetry.pdf

library(tidyverse)
library(ggrepel)
library(patchwork)
library(ggforce)
library(ggtext)

root <- here::here()
if (!endsWith(root, "LEEDS_MODEL")) {
  root <- "/Users/parvulesco/Documents/R/LEEDS_MODEL"
}

# ── Load scenarios metadata ────────────────────────────────────────────────────
sc <- read.csv(file.path(root, "data", "scenarios.csv")) |>
  mutate(
    channel_abbr = case_when(
      grepl("Final", domain, ignore.case = TRUE) & sector == "Household" ~ "HH",
      grepl("Final", domain, ignore.case = TRUE) &
        sector == "Government" ~ "Gov",
      grepl("Final", domain, ignore.case = TRUE) & sector == "Firm" ~ "Inv",
      TRUE ~ "Int"
    ),
    rich_label = paste0(shock, " | ", shift, " ", channel_abbr),
    pattern_group = case_when(
      shock %in% c(2, 13) ~ "Fossil Import Collapse",
      shock == 10 ~ "Competitive Displacement",
      shock %in% c(1, 4, 9) ~ "Production Leakage",
      shock %in% c(5, 14) ~ "Construction Rebound",
      TRUE ~ "Symmetric Contraction"
    )
  )

# ── 14-colour rainbow palette keyed on rich_label ─────────────────────────────
sc_ordered <- sc |> arrange(shock)
scenario_colours <- setNames(scales::hue_pal()(14), sc_ordered$rich_label)

# ── Transmission pattern colours (halo highlight) ─────────────────────────────
pattern_colours <- c(
  "Fossil Import Collapse" = "#B22222",
  "Competitive Displacement" = "#1C3A6E",
  "Production Leakage" = "#228B22",
  "Construction Rebound" = "#8B4513",
  "Symmetric Contraction" = "grey80"
)

# ── Grouped scenario legend (scenarios nested under regime headers) ────────────
# Regime membership (matches pattern_group in sc)
regime_members <- list(
  "Final Demand"        = c("1 | Food HH", "2 | Energy HH", "3 | Wood HH",
                             "4 | Plastics Gov", "5 | Construction Inv", "6 | Metal Inv"),
  "Intermediate Demand" = c("7 | Wood Int", "8 | Pulp Int", "9 | Plastics Int",
                             "10 | Metal Int", "11 | Glass Int", "12 | Cement Int",
                             "13 | Energy Int", "14 | Construction Int")
)

# Section headers use plain names (no icon)
ph <- setNames(names(regime_members), names(regime_members))

# Ordered breaks: header → members, for each regime
grouped_breaks <- unlist(lapply(names(regime_members), function(r) {
  c(ph[[r]], regime_members[[r]])
}), use.names = FALSE)

# Bold markdown for section headers, plain for scenario entries
grouped_labels <- ifelse(
  grouped_breaks %in% ph,
  paste0("**", grouped_breaks, "**"),
  grouped_breaks
)

# Colour values: white for headers, rainbow for scenarios
header_colours  <- setNames(rep("white", length(ph)), ph)
grouped_colours <- c(header_colours, scenario_colours)

# alpha override: 0 for headers (invisible key), 1 for scenarios
grouped_alpha <- unlist(lapply(names(regime_members), function(r) {
  c(0, rep(1, length(regime_members[[r]])))
}), use.names = FALSE)

# ── Load and combine all shock tables ─────────────────────────────────────────
shock_files <- list.files(
  file.path(root, "output", "scenarios", "shock_tables"),
  pattern = "shock_[0-9]+_table[.]csv",
  full.names = TRUE
)
shock_files <- shock_files[!grepl("^old_", basename(shock_files))]

all_data <- map_dfr(shock_files, function(f) {
  n <- as.integer(gsub(".*shock_([0-9]+)_table.*", "\\1", f))
  read.csv(f) |> mutate(shock = n)
})

# ── Extract LT values ─────────────────────────────────────────────────────────
panel_vars <- c("n", "emis", "x_mat", "b_s", "cab")

lt_wide <- all_data |>
  filter(Variable %in% c(panel_vars, "shw")) |>
  select(shock, Variable, LT.Z1 = Long.Term.Z1, LT.Z2 = Long.Term.Z2) |>
  left_join(
    sc |> select(shock, rich_label, pattern_group),
    by = "shock"
  ) |>
  mutate(
    rich_label = factor(rich_label, levels = sc_ordered$rich_label),
    pattern_group = factor(pattern_group, levels = names(pattern_colours))
  )

# ── Panel titles ───────────────────────────────────────────────────────────────
panel_labels <- list(
  n = "Employment",
  emis = expression(CO[2] ~ Emissions),
  x_mat = "Primary Material Extraction",
  b_s = "Public Debt (Bills Stock)",
  cab = "Current Account Balance"
)

# ── Shared theme ───────────────────────────────────────────────────────────────
theme_cb <- theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey92"),
    legend.text = element_markdown(size = 14),
    legend.title = element_markdown(size = 15),
    legend.key.size = unit(0.9, "cm")
  )

# ── Build one panel ────────────────────────────────────────────────────────────
make_panel <- function(var) {
  df <- lt_wide |> filter(Variable == var)

  # Tight independent limits: 5% padding only
  lim_x <- max(abs(df$LT.Z1), na.rm = TRUE) * 1.05
  lim_y <- max(abs(df$LT.Z2), na.rm = TRUE) * 1.05
  lim_x <- ceiling(lim_x * 100) / 100
  lim_y <- ceiling(lim_y * 100) / 100

  ggplot(df, aes(x = LT.Z1, y = LT.Z2)) +
    # Shaded regime areas with direct cluster labels (static grey border)
    ggforce::geom_mark_ellipse(
      aes(fill  = pattern_group, label = pattern_group),
      colour         = "grey50",
      alpha          = 0.15,
      linewidth      = 0.45,
      linetype       = "dashed",
      expand         = unit(5, "mm"),
      label.fontsize = 9,
      label.fill     = alpha("white", 0.75),
      label.colour   = "grey25",
      con.cap        = unit(1, "mm"),
      con.size       = 0.3,
      show.legend    = TRUE
    ) +
    scale_fill_manual(
      values = pattern_colours,
      name   = "**Transmission Pattern**",
      guide  = guide_legend(ncol = 1, order = 1,
                            override.aes = list(alpha = 0.5))
    ) +
    # Quadrant lines
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "grey55", linewidth = 0.35) +
    geom_vline(xintercept = 0, linetype = "dashed",
               colour = "grey55", linewidth = 0.35) +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted",
                colour = "grey75", linewidth = 0.3) +
    # Scenario point (rainbow colour)
    geom_point(
      aes(colour = rich_label),
      size = 3.5, alpha = 0.95
    ) +
    # Label with rectangle background
    ggrepel::geom_label_repel(
      aes(label = rich_label, colour = rich_label),
      size               = 5.5,
      show.legend        = FALSE,
      max.overlaps       = Inf,
      segment.size       = 0.4,
      segment.colour     = "grey50",
      min.segment.length = 0.1,
      label.padding      = unit(0.25, "lines"),
      label.size         = 0.3,
      fill               = alpha("white", 0.82)
    ) +
    scale_colour_manual(
      values = grouped_colours,
      limits = grouped_breaks,
      breaks = grouped_breaks,
      labels = grouped_labels,
      name   = "**Scenario**",
      guide  = guide_legend(ncol = 1, order = 2,
                            override.aes = list(
                              alpha = grouped_alpha,
                              size  = ifelse(grouped_alpha == 0, 0, 5)
                            ))
    ) +
    coord_cartesian(xlim = c(-lim_x, lim_x), ylim = c(-lim_y, lim_y)) +
    labs(
      title = panel_labels[[var]],
      x = "EU  (% deviation from baseline, LT)",
      y = "RoW  (% deviation from baseline, LT)"
    ) +
    theme_cb +
    theme(legend.position = "right") +
    guides(
      colour = guide_legend(ncol = 1, override.aes = list(size = 5, alpha = 1))
    )
}

# ── Dual inequality panel ──────────────────────────────────────────────────────
make_dual_panel <- function() {
  dual <- lt_wide |>
    filter(Variable %in% c("shw", "b_s")) |>
    select(shock, Variable, LT.Z1, LT.Z2, rich_label, pattern_group) |>
    pivot_wider(names_from = Variable, values_from = c(LT.Z1, LT.Z2)) |>
    rename(eu_shw = LT.Z1_shw, row_bs = LT.Z2_b_s)

  lim_x <- max(abs(dual$eu_shw), na.rm = TRUE) * 1.05
  lim_y <- max(abs(dual$row_bs), na.rm = TRUE) * 1.05
  lim_x <- ceiling(lim_x * 100) / 100
  lim_y <- ceiling(lim_y * 100) / 100

  ggplot(dual, aes(x = eu_shw, y = row_bs)) +
    # Shaded regime areas with cluster labels (static grey border)
    ggforce::geom_mark_ellipse(
      aes(fill  = pattern_group, label = pattern_group),
      colour         = "grey50",
      alpha          = 0.15,
      linewidth      = 0.45,
      linetype       = "dashed",
      expand         = unit(5, "mm"),
      label.fontsize = 9,
      label.fill     = alpha("white", 0.75),
      label.colour   = "grey25",
      con.cap        = unit(1, "mm"),
      con.size       = 0.3,
      show.legend    = TRUE
    ) +
    scale_fill_manual(
      values = pattern_colours,
      name   = "**Transmission Pattern**",
      guide  = guide_legend(ncol = 1, order = 1,
                            override.aes = list(alpha = 0.5))
    ) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      colour = "grey55",
      linewidth = 0.35
    ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      colour = "grey55",
      linewidth = 0.35
    ) +
    annotate(
      "text",
      x = -Inf,
      y = Inf,
      label = "Dual regressivity",
      hjust = -0.1,
      vjust = 1.5,
      size = 3,
      colour = "grey45",
      fontface = "italic"
    ) +
    geom_point(aes(colour = rich_label), size = 3.5, alpha = 0.95) +
    ggrepel::geom_label_repel(
      aes(label = rich_label, colour = rich_label),
      size = 5.5,
      show.legend = FALSE,
      max.overlaps = Inf,
      segment.size = 0.4,
      segment.colour = "grey50",
      min.segment.length = 0.1,
      label.padding = unit(0.25, "lines"),
      label.size = 0.3,
      fill = alpha("white", 0.82)
    ) +
    scale_colour_manual(
      values = grouped_colours,
      limits = grouped_breaks,
      breaks = grouped_breaks,
      labels = grouped_labels,
      name   = "**Scenario**",
      guide  = guide_legend(ncol = 1, order = 2,
                            override.aes = list(
                              alpha = grouped_alpha,
                              size  = ifelse(grouped_alpha == 0, 0, 5)
                            ))
    ) +
    coord_cartesian(xlim = c(-lim_x, lim_x), ylim = c(-lim_y, lim_y)) +
    labs(
      title = "Dual Inequality",
      x = "EU wage share  (% dev. from baseline, LT)",
      y = "RoW public debt  (% dev. from baseline, LT)"
    ) +
    theme_cb +
    theme(legend.position = "right") +
    guides(
      colour = guide_legend(ncol = 1, override.aes = list(size = 5, alpha = 1))
    )
}

# ── Pattern legend panel (standalone) ─────────────────────────────────────────
pattern_legend <- ggplot(
  data.frame(
    pattern = factor(names(pattern_colours), levels = names(pattern_colours)),
    x = 1,
    y = seq_along(pattern_colours)
  ),
  aes(x = x, y = y, colour = pattern)
) +
  geom_point(size = 5, alpha = 0.5) +
  scale_colour_manual(values = pattern_colours, name = "Transmission Pattern") +
  theme_void() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.5, "cm")
  ) +
  guides(
    colour = guide_legend(ncol = 1, override.aes = list(size = 5, alpha = 0.5))
  )

# ── Combine panels: 3 rows × 2 cols ───────────────────────────────────────────
panels <- map(panel_vars, make_panel)
panel_dual <- make_dual_panel()

p_final <- (panels[[1]] + panels[[2]]) /
  (panels[[3]] + panels[[4]]) /
  (panels[[5]] + panel_dual) +
  plot_annotation(
    title = "Cross-Border Asymmetries of Circular Economy Scenarios",
    subtitle = paste0(
      "Long-term (t+20) % deviations from baseline. Each point = one CE scenario. ",
      "Halo colour = transmission pattern.\n",
      "Upper-left quadrant: EU contracts, RoW expands. ",
      "Lower-right: EU expands, RoW contracts. ",
      "Bottom-right: dual inequality (wage share vs public debt)."
    ),
    theme = theme(
      plot.title = element_text(face = "bold", size = 20),
      plot.subtitle = element_text(size = 14, colour = "grey40")
    )
  ) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "right",
    legend.text = element_markdown(size = 14),
    legend.title = element_markdown(size = 15),
    legend.key.size = unit(0.9, "cm")
  )

# ── Save ───────────────────────────────────────────────────────────────────────
out_png <- file.path(
  root,
  "output",
  "png_figures",
  "p_cross_border_asymmetry.png"
)
out_pdf <- file.path(
  root,
  "output",
  "pdf_figures",
  "p_cross_border_asymmetry.pdf"
)

ggsave(out_png, p_final, width = 20, height = 22, dpi = 300, bg = "white")
ggsave(out_pdf, p_final, width = 20, height = 22)

message("Saved: ", out_png)
message("Saved: ", out_pdf)
