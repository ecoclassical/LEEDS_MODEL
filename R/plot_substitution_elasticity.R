## Substitution Elasticity Figure
## LT % deviation / rho by material and channel, EU vs RoW
## Output: output/png_figures/p_substitution_elasticity.png
##         output/pdf_figures/p_substitution_elasticity.pdf

library(tidyverse)

root <- here::here()
if (!endsWith(root, "LEEDS_MODEL")) {
  root <- "/Users/parvulesco/Documents/R/LEEDS_MODEL"
}

# ── Load data ───────────────────────────────────────────────────────────────────
sc <- read.csv(file.path(root, "data", "scenarios.csv")) |>
  mutate(
    short_label = paste0("Sc", shock),
    domain_label = ifelse(
      grepl("Final", domain, ignore.case = TRUE),
      "Final Demand",
      "Intermediate"
    )
  )

shock_files <- list.files(
  file.path(root, "output", "scenarios", "shock_tables"),
  pattern = "shock_[0-9]+_table[.]csv",
  full.names = TRUE
)
shock_files <- shock_files[!grepl("^old_", basename(shock_files))]

all_data <- map_dfr(shock_files, function(f) {
  n <- as.integer(gsub(".*shock_(\\d+)_table.*", "\\1", f))
  read.csv(f) |> mutate(shock = n)
})

# ── Key variables ───────────────────────────────────────────────────────────────
key_vars <- c("mat", "emis")
var_labels <- c(mat = "Total Material Use", emis = "CO\u2082 Emissions")

# ── Material order (matches `shift` column in scenarios.csv) ───────────────────
material_order <- c(
  "Food",
  "Energy",
  "Wood",
  "Pulp",
  "Plastics",
  "Construction",
  "Metal",
  "Glass",
  "Cement"
)

# ── Build elasticity data ───────────────────────────────────────────────────────
elast_df <- all_data |>
  filter(Variable %in% key_vars) |>
  select(shock, Variable, Z1 = Long.Term.Z1, Z2 = Long.Term.Z2) |>
  left_join(
    sc |> select(shock, short_label, shift, domain_label, rho),
    by = "shock"
  ) |>
  pivot_longer(cols = c(Z1, Z2), names_to = "region", values_to = "lt_dev") |>
  mutate(
    elasticity = lt_dev / rho,
    region = recode(region, Z1 = "EU", Z2 = "RoW"),
    var_label = factor(var_labels[Variable], levels = var_labels),
    shift = factor(shift, levels = material_order),
    short_label = fct_reorder(short_label, shock)
  )

# ── Colours ─────────────────────────────────────────────────────────────────────
domain_colours <- c("Final Demand" = "#E07B54", "Intermediate" = "#4A90D9")
region_alpha <- c("EU" = 1.0, "RoW" = 0.42)

# ── Plot ────────────────────────────────────────────────────────────────────────
p_elast <- ggplot(
  elast_df,
  aes(
    x = short_label,
    y = elasticity,
    fill = domain_label,
    alpha = region
  )
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey50",
    linewidth = 0.3
  ) +
  scale_fill_manual(values = domain_colours, name = "Channel") +
  scale_alpha_manual(values = region_alpha, name = "Region") +
  facet_grid(var_label ~ shift, scales = "free", space = "free_x") +
  labs(
    title = "CE Substitution Elasticity by Material and Channel",
    subtitle = paste0(
      "Long-term % deviation from baseline per unit of substitution intensity (\u03c1). ",
      "\u03c1 = 0.2 for all scenarios except Construction (\u03c1 = 0.05).\n",
      "Solid bars: EU domestic effect. Transparent bars: RoW cross-border effect."
    ),
    x = NULL,
    y = "Elasticity  (LT % deviation / \u03c1)"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text.x = element_text(face = "bold", size = 8),
    strip.text.y = element_text(face = "bold", size = 8),
    legend.position = "bottom",
    panel.spacing.x = unit(0.25, "lines")
  )

# ── Save ────────────────────────────────────────────────────────────────────────
out_png <- file.path(
  root,
  "output",
  "png_figures",
  "p_substitution_elasticity.png"
)
out_pdf <- file.path(
  root,
  "output",
  "pdf_figures",
  "p_substitution_elasticity.pdf"
)

ggsave(out_png, p_elast, width = 14, height = 7, dpi = 300, bg = "white")
ggsave(out_pdf, p_elast, width = 14, height = 7)

message("Saved: ", out_png)
message("Saved: ", out_pdf)
