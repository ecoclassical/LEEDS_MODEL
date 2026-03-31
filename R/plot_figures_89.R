## Figures 8 and 9: Scenario comparison — EU (domestic) and RoW (cross-border)
## Grey palette; legend sectioned Final Demand / Intermediate Demand
## Output: output/pdf_figures/comparison/08_comparison_eu.pdf
##         output/pdf_figures/comparison/09_comparison_row.pdf

library(tidyverse)
library(patchwork)
library(ggtext)

root <- here::here()
if (!endsWith(root, "LEEDS_MODEL")) {
  root <- "/Users/parvulesco/Documents/R/LEEDS_MODEL"
}

model_dir <- file.path(root, "model")
utils_dir <- file.path(root, "utils")
dir_data <- file.path(root, "data")
dir_pdf <- file.path(root, "output", "pdf_figures", "comparison")
dir_png <- file.path(root, "output", "png_figures", "comparison")
dir.create(dir_pdf, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_png, showWarnings = FALSE, recursive = TRUE)

# ── Bootstrap ─────────────────────────────────────────────────────────────────
source(file.path(model_dir, "bootstrap_2026.R"))

initial_filename <- file.path(dir_data, "full_mrio_initial_state.xlsx")
workspace_dir <- file.path(root, "output", "scenarios")
baseline_filename <- file.path(workspace_dir, "full_mrio_baseline_2026.RDS")
dir_runs <- file.path(workspace_dir, "shock_runs")
get_shock_filename <- function(n) {
  file.path(dir_runs, paste0("shock_", n, "_run.RDS"))
}

sc <- read.csv(file.path(dir_data, "scenarios.csv"))
initial <- load.init(initial_filename)
baseline <- run_or_load_baseline(initial, mvp.model, force = FALSE)
t.shock <- as.numeric(initial$pars["t.shock", "value"])

# ── Load all 14 shocks ────────────────────────────────────────────────────────
cat("Loading scenarios...\n")
scenario_list <- vector("list", nrow(sc))
for (n in seq_len(nrow(sc))) {
  initial_shock <- initial
  initial_shock$pars["shock", "value"] <- n
  scenario_list[[n]] <- run_or_load_shock(
    n,
    initial_shock,
    mvp.model,
    force = FALSE
  )
  cat(sprintf("  [%02d]\n", n))
}
names(scenario_list) <- paste0("Scenario ", sc$shock)

# ── Scenario metadata ──────────────────────────────────────────────────────────
sc_meta <- sc |>
  arrange(shock) |>
  mutate(
    channel_abbr = case_when(
      grepl("Final", domain, ignore.case = TRUE) & sector == "Household" ~ "HH",
      grepl("Final", domain, ignore.case = TRUE) &
        sector == "Government" ~ "Gov",
      grepl("Final", domain, ignore.case = TRUE) & sector == "Firm" ~ "Inv",
      TRUE ~ "Int"
    ),
    rich_label = paste0(shock, " | ", shift, " ", channel_abbr),
    demand_type = ifelse(
      grepl("Final", domain, ignore.case = TRUE),
      "Final Demand",
      "Intermediate Demand"
    )
  )

sc_ordered <- sc_meta |> arrange(shock)

# ── Variable selection ────────────────────────────────────────────────────────
selected.list <<- list(
  Macroeconomic = c("gdef", "cab", "tb", "go", "c"),
  Social = c("n", "shp", "shw"),
  Ecological = c("emis", "x_mat", "land", "water"),
  Financial = c("lf", "b_s", "v")
)

var_labels <- c(
  gdef  = "Government Deficit (gdef)",
  cab   = "Curr. Account Balance (cab)",
  tb    = "Trade Balance (tb)",
  go    = "Gross Output (go)",
  c     = "Consumption (c)",
  n     = "Employment (n)",
  shp   = "Profit Share (shp)",
  shw   = "Wage Share (shw)",
  emis  = "CO2 Emissions (emis)",
  x_mat = "Material Extraction (x_mat)",
  land  = "Land Use (land)",
  water = "Water Use (water)",
  lf    = "Corporate Loans (lf)",
  b_s   = "Government Debt (b_s)",
  v     = "Household Wealth (v)"
)

cat_var_order <- list(
  Macroeconomic = c("gdef", "cab", "tb", "go", "c"),
  Social = c("n", "shp", "shw"),
  Ecological = c("emis", "x_mat", "land", "water"),
  Financial = c("lf", "b_s", "v")
)

lookup <- stack(selected.list) |>
  dplyr::rename(Variable = values, Category = ind)

# ── Build shock summary tables ────────────────────────────────────────────────
t_pts <- c(t.shock + 1, t.shock + 7, t.shock + 20)
t_names <- c("Immediate", "Short.Term", "Long.Term")

scenario_tables <- lapply(scenario_list, function(x) {
  shock.summary(baseline, shock.run = x, t_ = t_pts, t_names = t_names)
})

df_wide <- dplyr::bind_rows(scenario_tables, .id = "scenario_name") |>
  dplyr::mutate(
    shock = as.integer(stringr::str_extract(scenario_name, "\\d+"))
  ) |>
  dplyr::select(-Name) |>
  dplyr::left_join(lookup, by = "Variable") |>
  dplyr::filter(!is.na(Category)) |>
  dplyr::left_join(
    sc_meta |> select(shock, rich_label, demand_type),
    by = "shock"
  )

df_long <- df_wide |>
  pivot_longer(
    cols = matches("(Immediate|Short\\.Term|Long\\.Term)\\.(Z1|Z2)"),
    names_to = c("term", "region"),
    names_pattern = "(.+)\\.(Z[12])",
    values_to = "value"
  ) |>
  mutate(
    value = as.numeric(value),
    region = recode(region, Z1 = "EU", Z2 = "RoW"),
    term = factor(
      term,
      levels = c("Immediate", "Short.Term", "Long.Term"),
      labels = c("Immediate", "Short\nTerm", "Long\nTerm")
    ),
    var_label = factor(var_labels[Variable], levels = var_labels),
    rich_label = factor(rich_label, levels = sc_ordered$rich_label),
    demand_type = factor(
      demand_type,
      levels = c("Final Demand", "Intermediate Demand")
    ),
    # Split fill aesthetics: fd_fill for Final Demand, int_fill for Intermediate
    rich_label = factor(rich_label, levels = sc_ordered$rich_label)
  ) |>
  filter(!is.na(value))

# ── Shared theme ──────────────────────────────────────────────────────────────
theme_fig <- theme_grey(base_size = 16) +
  theme(
    strip.text.x = element_text(face = "bold", size = 15),
    strip.text.y = element_text(face = "plain", size = 13, angle = 0,
                                hjust = 0, margin = margin(l = 4, r = 6)),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(0.5, "lines"),
    legend.text  = element_markdown(size = 14),
    legend.key.size = unit(0.8, "cm"),
    legend.title = element_markdown(size = 16),
    plot.title   = element_text(face = "bold", size = 19)
  )

dodge <- position_dodge2(width = 0.85, preserve = "single")

# ── Build one category panel ──────────────────────────────────────────────────
rainbow_14 <- setNames(scales::hue_pal()(14), sc_ordered$rich_label)

# Grouped legend: two sections (Final Demand / Intermediate Demand)
section_members <- list(
  "Final Demand"        = sc_ordered$rich_label[sc_ordered$demand_type == "Final Demand"],
  "Intermediate Demand" = sc_ordered$rich_label[sc_ordered$demand_type == "Intermediate Demand"]
)
ph_89 <- setNames(names(section_members), names(section_members))
grouped_breaks_89 <- unlist(lapply(names(section_members), function(s) {
  c(ph_89[[s]], section_members[[s]])
}), use.names = FALSE)
# Bold markdown for section headers, plain for scenario entries
grouped_labels_89 <- ifelse(
  grouped_breaks_89 %in% ph_89,
  paste0("**", grouped_breaks_89, "**"),
  grouped_breaks_89
)
header_cols_89  <- setNames(rep("white", length(ph_89)), ph_89)
grouped_cols_89 <- c(header_cols_89, rainbow_14)
grouped_alpha_89 <- unlist(lapply(names(section_members), function(s) {
  c(0, rep(1, length(section_members[[s]])))
}), use.names = FALSE)

make_cat_panel <- function(data, cat_name, var_order, show_legend = FALSE) {
  d <- data |>
    filter(Category == cat_name) |>
    mutate(
      var_label = factor(var_labels[Variable], levels = var_labels[var_order])
    )

  p <- ggplot(d, aes(x = term, y = value, fill = rich_label)) +
    geom_col(position = dodge, width = 0.75) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      colour = "grey50",
      linewidth = 0.3
    ) +
    scale_fill_manual(
      values = grouped_cols_89,
      limits = grouped_breaks_89,
      breaks = grouped_breaks_89,
      labels = grouped_labels_89,
      name   = "**Scenario**",
      guide  = if (show_legend)
        guide_legend(ncol = 1,
                     override.aes = list(alpha = grouped_alpha_89))
      else "none"
    ) +
    facet_grid(var_label ~ demand_type, scales = "free_y") +
    labs(title = cat_name, x = NULL, y = "% deviation from baseline") +
    theme_fig

  p
}

# ── Build and save figure ─────────────────────────────────────────────────────
build_figure <- function(region_code, fig_num, title_label) {
  d <- df_long |> filter(region == region_code)

  p_macro <- make_cat_panel(d, "Macroeconomic", cat_var_order$Macroeconomic)
  p_soc <- make_cat_panel(d, "Social", cat_var_order$Social)
  p_eco <- make_cat_panel(d, "Ecological", cat_var_order$Ecological)
  p_fin <- make_cat_panel(
    d,
    "Financial",
    cat_var_order$Financial,
    show_legend = TRUE
  )

  p_combined <- (p_macro + p_soc) /
    (p_eco + p_fin) +
    plot_annotation(
      title = title_label,
      subtitle = "Shock parameter \u03c1 = 0.2 (0.05 for Construction: Sc5 & Sc14). Long-term = t+20.",
      theme = theme(
        plot.title = element_text(face = "bold", size = 22),
        plot.subtitle = element_text(size = 15, colour = "grey40")
      )
    ) +
    plot_layout(guides = "collect") &
    theme(
      legend.position = "right",
      legend.key.size = unit(0.8, "cm")
    )

  name_base <- sprintf("%02d_comparison_%s", fig_num, tolower(region_code))
  ggsave(
    file.path(dir_pdf, paste0(name_base, ".pdf")),
    p_combined,
    width = 24,
    height = 16
  )
  ggsave(
    file.path(dir_png, paste0(name_base, ".png")),
    p_combined,
    width = 24,
    height = 16,
    dpi = 300,
    bg = "white"
  )
  message("Saved: ", name_base)
}

# ── Render ────────────────────────────────────────────────────────────────────
cat("\nBuilding Figure 8 (EU domestic)...\n")
build_figure("EU", 8, "Circular Economy Scenarios \u2014 Domestic Effects (EU)")

cat("\nBuilding Figure 9 (RoW cross-border)...\n")
build_figure(
  "RoW",
  9,
  "Circular Economy Scenarios \u2014 Cross-Border Effects (RoW)"
)

cat("\nDone.\n")
