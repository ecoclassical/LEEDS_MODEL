## Sector List Table — ggplot2 layout (two columns, tight sizing)
## CE secondary sectors highlighted in green
## Output: output/pdf_figures/sector_list_table.pdf
##         output/png_figures/sector_list_table.png

library(tidyverse)
library(ggtext)

root <- here::here()
if (!endsWith(root, "LEEDS_MODEL")) {
  root <- "/Users/parvulesco/Documents/R/LEEDS_MODEL"
}

dir_pdf <- file.path(root, "output", "pdf_figures")
dir_png <- file.path(root, "output", "png_figures")

# ── Load and clean ────────────────────────────────────────────────────────────
ce_sectors <- c(12, 14, 18, 22, 25, 27, 30, 32, 37)

sectors <- read.csv(file.path(root, "data", "sector_list.csv")) |>
  mutate(
    label     = gsub("<br>", " ", label),
    label     = trimws(label),
    is_ce     = sector_code %in% ce_sectors,
    col_panel = ifelse(sector_code <= 27, "left", "right"),
    row_in_panel = ifelse(col_panel == "left",
                          sector_code,
                          sector_code - 27),
    y         = 28 - row_in_panel   # top-to-bottom within each panel
  )

# ── Colour helpers ────────────────────────────────────────────────────────────
bg_ce      <- "#C8E6C9"   # green highlight for CE sectors
bg_even    <- "grey94"
bg_odd     <- "white"

sectors <- sectors |>
  mutate(
    row_fill = case_when(
      is_ce            ~ bg_ce,
      row_in_panel %% 2 == 0 ~ bg_even,
      TRUE             ~ bg_odd
    ),
    text_col = "grey10",
    num_col  = "grey35"
  )

# X positions within each panel (normalised 0–1 per panel)
x_num   <- 0.08
x_label <- 0.18

panel_width <- 0.46   # each panel occupies this fraction of the total x width
gap         <- 0.08   # gap between panels

left_offset  <- 0
right_offset <- panel_width + gap

make_x <- function(x_rel, panel) {
  ifelse(panel == "left",
         left_offset  + x_rel * panel_width,
         right_offset + x_rel * panel_width)
}

sectors <- sectors |>
  mutate(
    x_bg    = make_x(0.5, col_panel),
    x_num_p = make_x(x_num,   col_panel),
    x_lab_p = make_x(x_label, col_panel)
  )

total_width <- 2 * panel_width + gap   # ≈ 1

# ── Figure ────────────────────────────────────────────────────────────────────
p <- ggplot(sectors) +

  # Row background tiles
  geom_tile(aes(x = x_bg, y = y, fill = row_fill),
            width = panel_width, height = 0.88,
            show.legend = FALSE) +
  scale_fill_identity() +

  # Column header backgrounds
  annotate("rect",
           xmin = left_offset,               xmax = left_offset  + panel_width,
           ymin = 27.52,                     ymax = 28.48,
           fill = "grey25") +
  annotate("rect",
           xmin = right_offset,              xmax = right_offset + panel_width,
           ymin = 27.52,                     ymax = 28.48,
           fill = "grey25") +

  # Column header text
  annotate("text", x = make_x(x_num,   "left"),  y = 28,
           label = "#",    hjust = 0.5, size = 4.2,
           fontface = "bold", colour = "white") +
  annotate("text", x = make_x(x_label, "left"),  y = 28,
           label = "Sector", hjust = 0, size = 4.2,
           fontface = "bold", colour = "white") +
  annotate("text", x = make_x(x_num,   "right"), y = 28,
           label = "#",    hjust = 0.5, size = 4.2,
           fontface = "bold", colour = "white") +
  annotate("text", x = make_x(x_label, "right"), y = 28,
           label = "Sector", hjust = 0, size = 4.2,
           fontface = "bold", colour = "white") +

  # Sector numbers
  geom_text(aes(x = x_num_p, y = y, label = sector_code),
            hjust = 0.5, size = 3.6, colour = "grey40") +

  # Sector labels (bold + green text for CE sectors)
  geom_text(
    data = sectors |> filter(!is_ce),
    aes(x = x_lab_p, y = y, label = label),
    hjust = 0, size = 3.6, colour = "grey10"
  ) +
  geom_text(
    data = sectors |> filter(is_ce),
    aes(x = x_lab_p, y = y, label = paste0(label, "  \u2605")),
    hjust = 0, size = 3.6, colour = "#2e7d32", fontface = "bold"
  ) +

  # Panel separator line
  geom_vline(xintercept = panel_width + gap / 2,
             colour = "grey70", linewidth = 0.5, linetype = "solid") +

  coord_cartesian(
    xlim = c(0, total_width),
    ylim = c(0.4, 28.6),
    clip = "off"
  ) +
  theme_void() +
  labs(
    title   = "Sector Classification  (54 sectors \u2014 EU Z\u2081 = RoW Z\u2082)",
    caption = paste0(
      "\u2605 Secondary / re-processing sectors involved in circular economy substitutions (highlighted). ",
      "Each region (EU = Z\u2081, RoW = Z\u2082) shares an identical 54-sector structure."
    )
  ) +
  theme(
    plot.title   = element_text(face = "bold", size = 15, hjust = 0,
                                margin = margin(b = 8)),
    plot.caption = element_text(size = 9, colour = "grey45", hjust = 0,
                                margin = margin(t = 10)),
    plot.margin  = margin(t = 12, r = 20, b = 12, l = 20)
  )

# ── Save ──────────────────────────────────────────────────────────────────────
out_pdf <- file.path(dir_pdf, "sector_list_table.pdf")
out_png <- file.path(dir_png, "sector_list_table.png")

ggsave(out_pdf, p, width = 14, height = 9)
ggsave(out_png, p, width = 14, height = 9, dpi = 300, bg = "white")

message("Saved: ", out_pdf)
message("Saved: ", out_png)
