# ============================================================
# export_comparison_plots.R
# Loads cached baseline + 14 shocks and saves all comparison
# figures as both PDF and PNG to output/scenarios/comparison_plots/
# ============================================================

# ---- 1. Paths -------------------------------------------------------
root <- normalizePath(file.path(
  dirname(rstudioapi::getSourceEditorContext()$path),
  ".."
))
model_dir <- file.path(root, "model")
utils_dir <- file.path(root, "utils")
dir_data <- file.path(root, "data")

workspace_dir <- file.path(root, "output", "scenarios")
dir_runs <- file.path(workspace_dir, "shock_runs")

dir_pdf <- file.path(root, "output", "pdf_figures", "comparison")
dir_png <- file.path(root, "output", "png_figures", "comparison")
dir.create(dir_pdf, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_png, showWarnings = FALSE, recursive = TRUE)

# ---- 2. Bootstrap ---------------------------------------------------
source(file.path(model_dir, "bootstrap_2026.R"))

initial_filename <- file.path(dir_data, "full_mrio_initial_state.xlsx")
scenario_filename <- file.path(dir_data, "scenarios.csv")
sc <- read.csv(scenario_filename)

initial <- load.init(initial_filename)
baseline <- run_or_load_baseline(initial, mvp.model, force = FALSE)

t.shock <- initial$pars["t.shock", "value"]

# ---- 3. Load all 14 shocks ------------------------------------------
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
  cat(sprintf("  [%02d] %s\n", n, sc$display_name[n]))
}
names(scenario_list) <- sc$display_name

# ---- 4. Variable setup ----------------------------------------------
variable.table <- read.csv(
  file.path(dir_data, "Variable_Definitions.csv"),
  stringsAsFactors = FALSE
)

selected.list <- list(
  Macroeconomic = c("c", "go", "id", "gdef", "cab", "tb"),
  Social = c("n", "shp", "shw"),
  Debt.and.Wealth = c("lh", "lf", "v", "b_s"),
  Ecological = c("emis", "mat", "wa")
)

lookup <- stack(selected.list) |>
  dplyr::rename(Variable = values, Name = ind)

scenario_tables_list <- lapply(scenario_list, function(x) {
  shock.summary(
    baseline,
    shock.run = x,
    t_ = c(t.shock + 1, t.shock + 7, t.shock + 20),
    t_names = c("Immediate", "Short.Term", "Long.Term")
  )
})

df <- dplyr::bind_rows(scenario_tables_list, .id = "scenario") |>
  dplyr::mutate(
    scenario = factor(scenario, levels = names(scenario_tables_list))
  ) |>
  dplyr::select(-Name) |>
  dplyr::left_join(lookup, by = "Variable") |>
  dplyr::filter(!is.na(Name))

# CE sector-pair extension (primary sectors only)
ce_var_ext <- data.frame(
  dimension = "aggregate",
  type = "production",
  label = c(
    "x-7",
    "x-11",
    "x-13",
    "x-17",
    "x-21",
    "x-24",
    "x-26",
    "x-31",
    "x-36"
  ),
  unit = "",
  name = c(
    "Meat\n(primary material)",
    "Wood\n(primary material)",
    "Pulp\n(primary material)",
    "Plastics\n(primary material)",
    "Glass\n(primary material)",
    "Cement\n(primary material)",
    "Metals\n(primary material)",
    "Fossil Energy\n(primary material)",
    "Construction\n(primary material)"
  ),
  stringsAsFactors = FALSE
)
variable.table_ext <- dplyr::bind_rows(variable.table, ce_var_ext)

selected.list_ce <- list(
  "Circular Economy" = c(
    "x-7",
    "x-11",
    "x-13",
    "x-17",
    "x-21",
    "x-24",
    "x-26",
    "x-31",
    "x-36"
  )
)
ce_lookup <- stack(selected.list_ce) |>
  dplyr::rename(Variable = values, Name = ind)

ce_tables_list <- lapply(scenario_list, function(x) {
  selected.list_bak <- selected.list
  selected.list <<- selected.list_ce
  res <- shock.summary(
    baseline,
    shock.run = x,
    t_ = c(t.shock + 1, t.shock + 7, t.shock + 20),
    t_names = c("Immediate", "Short.Term", "Long.Term")
  )
  selected.list <<- selected.list_bak
  res
})

df_ce <- dplyr::bind_rows(ce_tables_list, .id = "scenario") |>
  dplyr::mutate(scenario = factor(scenario, levels = names(ce_tables_list))) |>
  dplyr::select(-Name) |>
  dplyr::left_join(ce_lookup, by = "Variable") |>
  dplyr::filter(!is.na(Name))

# Colour palette
scenario_colors <- c(
  "1" = "#FF8C42",
  "2" = "#FFD700",
  "3" = "#90EE90",
  "4" = "#87CEEB",
  "5" = "#DEB887",
  "6" = "#B0C4DE",
  "7" = "#228B22",
  "8" = "#006400",
  "9" = "#1E90FF",
  "10" = "#1C3A6E",
  "11" = "#9370DB",
  "12" = "#696969",
  "13" = "#B8860B",
  "14" = "#8B4513"
)

rho_label <- "0.2 (0.05 for Construction, Sc5 & Sc14)"

ce_var_order <- c(
  "Meat\n(primary material)",
  "Fossil Energy\n(primary material)",
  "Wood\n(primary material)",
  "Plastics\n(primary material)",
  "Construction\n(primary material)",
  "Metals\n(primary material)",
  "Pulp\n(primary material)",
  "Glass\n(primary material)",
  "Cement\n(primary material)"
)

# ---- 5. Helper: save as PDF + PNG -----------------------------------
save_plot <- function(p, name, w, h) {
  ggplot2::ggsave(
    file.path(dir_pdf, paste0(name, ".pdf")),
    plot = p,
    width = w,
    height = h,
    units = "in"
  )
  ggplot2::ggsave(
    file.path(dir_png, paste0(name, ".png")),
    plot = p,
    width = w,
    height = h,
    units = "in",
    dpi = 150
  )
  cat(sprintf("  Saved: %s (%.0f x %.0f in)\n", name, w, h))
}

# ---- 6. Main comparison plots ---------------------------------------
cat("\nBuilding main comparison plots...\n")

p_eu <- build_policy_comparison_plot(
  df = df,
  variable.table = variable.table,
  sc = sc,
  rho = rho_label,
  colors = scenario_colors,
  region_filter = "EU",
  title = "Circular Economy Intervention Comparison | Domestic Effects (EU)",
  ncol = 2
)
save_plot(p_eu, "01_comparison_eu", w = 22, h = 12)

p_row <- build_policy_comparison_plot(
  df = df,
  variable.table = variable.table,
  sc = sc,
  rho = rho_label,
  colors = scenario_colors,
  region_filter = "RoW",
  title = "Circular Economy Intervention Comparison | Cross-Border Effects (RoW)",
  ncol = 2
)
save_plot(p_row, "02_comparison_row", w = 22, h = 12)

# ---- 7. CE scatter plots (long-term, primary vs secondary) ----------
cat("\nBuilding CE sector scatter plots...\n")

t_lt <- t.shock + 20
ce_pairs <- data.frame(
  material = c(
    "Food",
    "Wood",
    "Pulp",
    "Plastics",
    "Glass",
    "Cement",
    "Metal",
    "Energy",
    "Construction"
  ),
  sector_p = c(7, 11, 13, 17, 21, 24, 26, 31, 36),
  sector_s = c(8, 12, 14, 18, 22, 25, 27, 32, 37),
  label_p = c(
    "Meat",
    "Wood",
    "Pulp",
    "Plastics",
    "Glass",
    "Cement",
    "Metals",
    "Fossil Electricity",
    "Construction"
  ),
  label_s = c(
    "Other Food",
    "Wood Re-proc.",
    "Pulp Re-proc.",
    "Plastics Re-proc.",
    "Glass Re-proc.",
    "Clinker Re-proc.",
    "Metals Re-proc.",
    "Renewable Electricity",
    "Construction Re-proc."
  ),
  stringsAsFactors = FALSE
)

ce_df <- dplyr::bind_rows(lapply(seq_along(scenario_list), function(i) {
  scen <- scenario_list[[i]]
  dplyr::bind_rows(lapply(c("Z1", "Z2"), function(z) {
    dplyr::bind_rows(lapply(1:nrow(ce_pairs), function(j) {
      vp <- paste0(z, "_x-", ce_pairs$sector_p[j])
      vs <- paste0(z, "_x-", ce_pairs$sector_s[j])
      if (!all(c(vp, vs) %in% rownames(scen$simulation))) {
        return(NULL)
      }
      bp <- baseline$simulation[vp, t_lt]
      bs <- baseline$simulation[vs, t_lt]
      sp <- scen$simulation[vp, t_lt]
      ss <- scen$simulation[vs, t_lt]
      data.frame(
        scenario = names(scenario_list)[i],
        shock = i,
        region = ifelse(z == "Z1", "EU", "RoW"),
        material = ce_pairs$material[j],
        type = c("Primary", "Secondary"),
        sector_label = c(ce_pairs$label_p[j], ce_pairs$label_s[j]),
        pct_dev = c((sp / bp - 1) * 100, (ss / bs - 1) * 100),
        stringsAsFactors = FALSE
      )
    }))
  }))
})) |>
  dplyr::mutate(
    material = factor(material, levels = ce_pairs$material),
    type = factor(type, levels = c("Primary", "Secondary")),
    display_scenario = stringr::str_replace(
      scenario,
      "( \\| .*?)( \\| )",
      "\\1\n"
    ),
    display_scenario = factor(
      display_scenario,
      levels = unique(display_scenario[order(shock)])
    )
  )

ce_colors_scatter <- c("Primary" = "#c0392b", "Secondary" = "#2980b9")
dodge <- ggplot2::position_dodge2(width = 0.8, preserve = "single")

for (reg in c("EU", "RoW")) {
  reg_label <- if (reg == "EU") "Domestic (EU)" else "Cross-Border (RoW)"
  p <- ggplot2::ggplot(
    dplyr::filter(ce_df, region == reg),
    ggplot2::aes(x = display_scenario, y = pct_dev, fill = type)
  ) +
    ggplot2::geom_col(position = dodge, width = 0.7) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      colour = "darkgray"
    ) +
    ggplot2::scale_fill_manual(values = ce_colors_scatter) +
    ggplot2::facet_wrap(~material, scales = "free_y", ncol = 3) +
    ggplot2::labs(
      title = paste0("CE Sector Outputs | ", reg_label),
      subtitle = paste0("Shock Parameter \u03c1 = ", rho_label),
      x = NULL,
      y = "% deviation from baseline",
      fill = "Sector type"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1,
        vjust = 1,
        size = 7
      ),
      legend.position = "right",
      strip.text = ggplot2::element_text(face = "bold")
    )
  fname <- if (reg == "EU") "03_ce_scatter_eu" else "04_ce_scatter_row"
  save_plot(p, fname, w = 22, h = 12)
}

# ---- 8. CE comparison-style plots -----------------------------------
cat("\nBuilding CE comparison-style plots...\n")

p_ce_eu <- build_policy_comparison_plot(
  df = df_ce,
  variable.table = variable.table_ext,
  sc = sc,
  rho = rho_label,
  colors = scenario_colors,
  region_filter = "EU",
  title = "CE Intervention Comparison | Sector Outputs — EU (Domestic)",
  ncol = 1,
  var_name_order = ce_var_order
)
save_plot(p_ce_eu, "05_ce_comparison_eu", w = 22, h = 20)

p_ce_row <- build_policy_comparison_plot(
  df = df_ce,
  variable.table = variable.table_ext,
  sc = sc,
  rho = rho_label,
  colors = scenario_colors,
  region_filter = "RoW",
  title = "CE Intervention Comparison | Sector Outputs — Cross-Border (RoW)",
  ncol = 1,
  var_name_order = ce_var_order
)
save_plot(p_ce_row, "06_ce_comparison_row", w = 22, h = 20)

# ---- 9. Avg-terms plot ----------------------------------------------
cat("\nBuilding avg-terms plot...\n")
p_avg <- build_policy_comparison_plot_avg_terms(
  df = df,
  variable.table = variable.table,
  sc = sc,
  rho = rho_label,
  colors = scenario_colors
)
save_plot(p_avg, "07_comparison_avg_terms", w = 14, h = 16)

# ---- 10. Figure 8: Integrated figure (all categories + CE) ----------
# Layout (ncol = 2, 5 categories → 3 rows):
#   Row 1: [Macroeconomic 5]  [Ecological 5]
#   Row 2: [Social 3]         [Financial 3]
#   Row 3: [Circular Economy 9]
cat("\nBuilding figure 8 — integrated all-variable plot...\n")

# Category order determines col_assignment below:
# Col 1 (left):  Macroeconomic, Social, Ecological  (positions 1,2,3) → 13 vars
# Col 2 (right): Financial, Circular.Economy        (positions 4,5)   → 12 vars
selected.list_fig8 <- list(
  Macroeconomic = c("gdef", "cab", "tb", "go", "c"), # 5 vars
  Social = c("n", "shp", "shw"), # 3 vars
  Ecological = c("emis", "mat", "land", "water", "wa"), # 5 vars  → left col: 13
  Financial = c("lf", "b_s", "v"), # 3 vars
  "Circular Economy" = c(
    "x-7",
    "x-11",
    "x-13",
    "x-17",
    "x-21",
    "x-24",
    "x-26",
    "x-31",
    "x-36"
  ) # 9 vars
)

lookup_fig8 <- stack(selected.list_fig8) |>
  dplyr::rename(Variable = values, Name = ind)

# Temporarily swap selected.list for shock.summary (uses global)
selected.list_bak <- selected.list
selected.list <<- selected.list_fig8

tables_fig8 <- lapply(scenario_list, function(x) {
  shock.summary(
    baseline,
    shock.run = x,
    t_ = c(t.shock + 1, t.shock + 7, t.shock + 20),
    t_names = c("Immediate", "Short.Term", "Long.Term")
  )
})

selected.list <<- selected.list_bak # restore

df_fig8 <- dplyr::bind_rows(tables_fig8, .id = "scenario") |>
  dplyr::mutate(scenario = factor(scenario, levels = names(tables_fig8))) |>
  dplyr::select(-Name) |>
  dplyr::left_join(lookup_fig8, by = "Variable") |>
  dplyr::filter(!is.na(Name))

rho_label_fig8 <- paste0(
  "0.2 | \u03c1 = 0.05 for Construction (Sc5 & Sc14)",
  " | All flows except household net wealth, government debt, and loans",
  " | % deviation from baseline"
)

for (reg in c("EU", "RoW")) {
  reg_label <- if (reg == "EU") {
    "Domestic Effects | EU"
  } else {
    "Cross-Border Effects | RoW"
  }
  fname <- if (reg == "EU") "08_integrated_eu" else "09_integrated_row"

  p <- build_policy_comparison_plot(
    df = df_fig8,
    variable.table = variable.table_ext,
    sc = sc,
    rho = rho_label_fig8,
    colors = scenario_colors,
    region_filter = reg,
    title = paste0("Circular Economy Intervention Comparison | ", reg_label),
    col_assignment = c(1, 1, 1, 2, 2),
    var_name_order = ce_var_order
  )
  save_plot(p, fname, w = 17.6, h = 12.6)
}

cat("\nAll plots saved to:\n  PDF:", dir_pdf, "\n  PNG:", dir_png, "\n")
beepr::beep(3)
