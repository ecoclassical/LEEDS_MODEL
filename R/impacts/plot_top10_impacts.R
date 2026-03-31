# ============================================================
# plot_top10_impacts.R
# Plots top 10 sectors by total impact (land, water, emissions,
# extraction) using data/impacts/impacts.csv
# ============================================================
library(tidyverse)

root    <- normalizePath(file.path(dirname(rstudioapi::getSourceEditorContext()$path), "..", ".."))
impacts <- read.csv(file.path(root, "data", "impacts", "impacts.csv")) %>%
  filter(region %in% c("EU", "RoW"))

# Plots

impacts_top10_plot <- function(dat, impact_name) {
  df_imp <- dat %>%
    filter(impact == impact_name) %>%
    group_by(region, sector) %>%
    summarise(total_value = sum(total_value, na.rm = TRUE), .groups = "drop")

  top_sectors <- df_imp %>%
    group_by(sector) %>%
    summarise(total = sum(total_value, na.rm = TRUE), .groups = "drop") %>%
    slice_max(total, n = 10) %>%
    pull(sector)

  unit_label <- switch(impact_name,
    Emissions  = "ton GHG",
    Extraction = "ton",
    Land       = "ha",
    Water      = "ton",
    "value"
  )

  df_plot <- df_imp %>%
    filter(sector %in% top_sectors) %>%
    group_by(sector) %>%
    mutate(total_sector = sum(total_value, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(sector = fct_reorder(sector, total_sector))

  ggplot(df_plot, aes(x = total_value, y = sector, fill = region)) +
    geom_col(position = "dodge") +
    labs(
      title = paste0("Top 10 sectors — ", impact_name),
      x     = unit_label,
      y     = NULL,
      fill  = "Region"
    ) +
    scale_x_continuous(labels = scales::comma) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

p_emissions  <- impacts_top10_plot(impacts, "Emissions")
p_extraction <- impacts_top10_plot(impacts, "Extraction")
p_land       <- impacts_top10_plot(impacts, "Land")
p_water      <- impacts_top10_plot(impacts, "Water")

# ---- Save ------------------------------------------------------------
dir_pdf <- file.path(root, "output", "pdf_figures", "impact")
dir_png <- file.path(root, "output", "png_figures", "impact")
dir.create(dir_pdf, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_png, showWarnings = FALSE, recursive = TRUE)

for (nm in c("emis", "extraction", "land", "water")) {
  p    <- get(paste0("p_", if (nm == "emis") "emissions" else nm))
  stem <- paste0("p_impact_", nm)
  ggplot2::ggsave(file.path(dir_pdf, paste0(stem, ".pdf")), p, width = 10, height = 6)
  ggplot2::ggsave(file.path(dir_png, paste0(stem, ".png")), p, width = 10, height = 6, dpi = 150)
}

cat("Saved to:\n  PDF:", dir_pdf, "\n  PNG:", dir_png, "\n")

p_emissions
p_extraction
p_land
p_water
