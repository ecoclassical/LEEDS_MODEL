## Scenario Registry — Visual Bullet Layout
## Domain shown once per section header; Channel shown once per sub-group (multirow style)
## Bullet colour = scenario (rainbow); Badge = transmission regime
## Output: output/pdf_figures/scenario_registry_visual.pdf
##         output/png_figures/scenario_registry_visual.png

library(tidyverse)
library(ggtext)

root <- here::here()
if (!endsWith(root, "LEEDS_MODEL")) {
  root <- "/Users/parvulesco/Documents/R/LEEDS_MODEL"
}

dir_pdf <- file.path(root, "output", "pdf_figures")
dir_png <- file.path(root, "output", "png_figures")

# ── Colours — must match cross-border asymmetry plot exactly ──────────────────
sc <- read.csv(file.path(root, "data", "scenarios.csv")) |> arrange(shock)

rich_labels <- sc |>
  mutate(
    channel_abbr = case_when(
      grepl("Final", domain, ignore.case = TRUE) & sector == "Household"  ~ "HH",
      grepl("Final", domain, ignore.case = TRUE) & sector == "Government" ~ "Gov",
      grepl("Final", domain, ignore.case = TRUE) & sector == "Firm"       ~ "Inv",
      TRUE ~ "Int"
    ),
    rich_label = paste0(shock, " | ", shift, " ", channel_abbr)
  ) |>
  pull(rich_label)

rainbow_14 <- setNames(scales::hue_pal()(14), rich_labels)

pattern_colours <- c(
  "Fossil Import Collapse"   = "#B22222",
  "Competitive Displacement" = "#1C3A6E",
  "Production Leakage"       = "#228B22",
  "Construction Rebound"     = "#8B4513",
  "Symmetric Contraction"    = "#888888"
)

regime_map <- c(
  "1"  = "Production Leakage",       "2"  = "Fossil Import Collapse",
  "3"  = "Symmetric Contraction",    "4"  = "Production Leakage",
  "5"  = "Construction Rebound",     "6"  = "Symmetric Contraction",
  "7"  = "Symmetric Contraction",    "8"  = "Symmetric Contraction",
  "9"  = "Production Leakage",       "10" = "Competitive Displacement",
  "11" = "Symmetric Contraction",    "12" = "Symmetric Contraction",
  "13" = "Fossil Import Collapse",   "14" = "Construction Rebound"
)

subst_labels <- c(
  "Food"         = "Meat \u2192 Other food products",
  "Energy"       = "Fossil electricity \u2192 Renewables",
  "Wood"         = "Virgin wood \u2192 Recycled wood",
  "Pulp"         = "Virgin pulp \u2192 Recycled pulp",
  "Plastics"     = "Virgin plastics \u2192 Recycled plastics",
  "Construction" = "Construction \u2192 Recycled aggregates",
  "Metal"        = "Primary metals \u2192 Recycled metals",
  "Glass"        = "Virgin glass \u2192 Recycled glass",
  "Cement"       = "Cement \u2192 Recycled clinker"
)

# ── Build row-level data ──────────────────────────────────────────────────────
tbl <- sc |>
  mutate(
    rich_label   = rich_labels,
    sc_label     = paste0("Sc", shock),
    channel_full = case_when(
      domain == "Final Demand" & sector == "Household"  ~ "Household Consumption",
      domain == "Final Demand" & sector == "Government" ~ "Government Consumption",
      domain == "Final Demand" & sector == "Firm"       ~ "Firm Investment",
      TRUE                                               ~ "Firm Production"
    ),
    substitution = subst_labels[shift],
    regime       = regime_map[as.character(shock)],
    domain_group = ifelse(grepl("Final", domain), "Final Demand", "Intermediate Demand"),
    y            = n() - row_number() + 1   # Sc1 at top
  )

n_rows <- nrow(tbl)
fd_rows  <- tbl |> filter(domain_group == "Final Demand")
int_rows <- tbl |> filter(domain_group == "Intermediate Demand")

# ── Channel sub-group centres (for multirow-style display) ────────────────────
channel_centres <- tbl |>
  group_by(channel_full, domain_group) |>
  summarise(y_centre = mean(y), y_min = min(y), y_max = max(y),
            .groups = "drop")

# ── X column positions ────────────────────────────────────────────────────────
x_bullet   <- 0.012
x_sc       <- 0.042
x_channel  <- 0.130
x_subst    <- 0.385
x_rho      <- 0.710
x_regime   <- 0.755

# ── Figure ────────────────────────────────────────────────────────────────────
p <- ggplot(tbl) +

  # Section backgrounds
  annotate("rect", xmin = 0, xmax = 1,
           ymin = min(fd_rows$y)  - 0.48, ymax = max(fd_rows$y)  + 0.80,
           fill = "#D9EEF8", alpha = 0.40) +
  annotate("rect", xmin = 0, xmax = 1,
           ymin = min(int_rows$y) - 0.48, ymax = max(int_rows$y) + 0.80,
           fill = "#FEF0DC", alpha = 0.40) +

  # Domain labels in bold black inside the channel column, top of each section
  annotate("text", x = x_channel + 0.01, y = max(fd_rows$y)  + 0.57 - 0.57,
           label = "Final Demand", hjust = 0,
           fontface = "bold", size = 4.3, colour = "grey10") +
  annotate("text", x = x_channel + 0.01, y = max(int_rows$y) + 0.57 - 0.57,
           label = "Intermediate Demand", hjust = 0,
           fontface = "bold", size = 4.3, colour = "grey10") +

  # Divider between sections
  geom_hline(yintercept = n_rows - nrow(fd_rows) + 0.5,
             colour = "grey45", linewidth = 0.8) +

  # Column header underline
  geom_hline(yintercept = n_rows + 1.25,
             colour = "grey20", linewidth = 1.0) +

  # Column headers
  annotate("text", x = x_sc,      y = n_rows + 1.60, label = "Scenario",
           hjust = 0, fontface = "bold", size = 4.8, colour = "grey10") +
  annotate("text", x = x_channel, y = n_rows + 1.60, label = "Channel",
           hjust = 0, fontface = "bold", size = 4.8, colour = "grey10") +
  annotate("text", x = x_subst,   y = n_rows + 1.60,
           label = "Primary \u2192 Secondary Substitution",
           hjust = 0, fontface = "bold", size = 4.8, colour = "grey10") +
  annotate("text", x = x_rho,     y = n_rows + 1.60, label = "\u03c1",
           hjust = 0.5, fontface = "bold", size = 4.8, colour = "grey10") +
  annotate("text", x = x_regime,  y = n_rows + 1.60,
           label = "Transmission Regime",
           hjust = 0, fontface = "bold", size = 4.8, colour = "grey10") +

  # ── Channel sub-group: vertical bracket line + label shown ONCE at centre ─────
  # Thin vertical line spanning each channel sub-group
  geom_segment(
    data = channel_centres,
    aes(x = x_channel - 0.005, xend = x_channel - 0.005,
        y = y_min - 0.38, yend = y_max + 0.38),
    colour = "grey60", linewidth = 0.5
  ) +
  # Channel label at vertical centre of sub-group
  geom_text(
    data = channel_centres,
    aes(x = x_channel + 0.005, y = y_centre, label = channel_full),
    hjust = 0, size = 4.0, colour = "grey25", fontface = "italic"
  ) +

  # ── Scenario bullet ───────────────────────────────────────────────────────────
  geom_point(aes(x = x_bullet, y = y, colour = rich_label),
             size = 5.5, show.legend = FALSE) +
  scale_colour_manual(values = rainbow_14) +

  # Sc. label
  geom_text(aes(x = x_sc, y = y, label = sc_label),
            hjust = 0, size = 4.2, fontface = "bold", colour = "grey10") +

  # Substitution
  geom_text(aes(x = x_subst, y = y, label = substitution),
            hjust = 0, size = 3.9, colour = "grey20") +

  # rho
  geom_text(aes(x = x_rho, y = y,
                label = formatC(rho, format = "f", digits = 2)),
            hjust = 0.5, size = 3.9, colour = "grey20") +

  # ── Regime badge ──────────────────────────────────────────────────────────────
  geom_label(aes(x = x_regime, y = y, label = regime, fill = regime),
             hjust = 0, colour = "white", fontface = "bold",
             size = 3.6,
             label.padding = unit(0.28, "lines"),
             label.r       = unit(0.20, "lines"),
             label.size    = 0,
             show.legend   = FALSE) +
  scale_fill_manual(values = pattern_colours) +

  # ── Layout ────────────────────────────────────────────────────────────────────
  coord_cartesian(xlim = c(0, 1),
                  ylim = c(0.35, n_rows + 2.2),
                  clip = "off") +
  theme_void() +
  labs(
    title   = "Scenario Registry",
    caption = paste0(
      "\u03c1\u2009= substitution intensity (share of primary demand shifted to secondary sector per period). ",
      "Construction scenarios (Sc5, Sc14) use \u03c1\u2009=\u20090.05 due to the larger scale of the investment channel.\n",
      "Bullet colour identifies each scenario (matches Figures 8\u20139). Badge colour denotes transmission regime (matches shaded areas in Figure 10)."
    )
  ) +
  theme(
    plot.title   = element_text(face = "bold", size = 17, margin = margin(b = 6),
                                hjust = 0),
    plot.caption = element_text(size = 9, colour = "grey45", hjust = 0,
                                margin = margin(t = 12)),
    plot.margin  = margin(t = 12, r = 20, b = 12, l = 20)
  )

# ── Save ──────────────────────────────────────────────────────────────────────
out_pdf <- file.path(dir_pdf, "scenario_registry_visual.pdf")
out_png <- file.path(dir_png, "scenario_registry_visual.png")

ggsave(out_pdf, p, width = 16, height = 9)
ggsave(out_png, p, width = 16, height = 9, dpi = 300, bg = "white")

message("Saved: ", out_pdf)
message("Saved: ", out_png)
