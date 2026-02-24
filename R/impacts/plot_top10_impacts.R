# Plots

impacts_top10_plot <- function(dat, impact_name) {
  df_imp <- dat %>%
    filter(impact == impact_name) %>%
    group_by(region, sector) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  top_sectors <- df_imp %>%
    group_by(sector) %>%
    summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
    slice_max(total, n = 10) %>%
    pull(sector)

  df_plot <- df_imp %>%
    filter(sector %in% top_sectors) %>%
    group_by(sector) %>%
    mutate(total_sector = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(sector = fct_reorder(sector, total_sector))

  ggplot(df_plot, aes(x = value, y = sector, fill = region)) +
    geom_col(position = 'dodge') +
    labs(
      title = paste0("Top 10 sectors by value — ", impact_name),
      x = "Value",
      y = NULL,
      fill = "Region"
    ) +
    scale_x_continuous(labels = scales::comma) +
    theme_minimal()
}

p_emissions <- impacts_top10_plot(impacts, "Emissions")
p_extraction <- impacts_top10_plot(impacts, "Extraction")
p_land <- impacts_top10_plot(impacts, "Land")
p_water <- impacts_top10_plot(impacts, "Water")

p_emissions
p_extraction
p_land
p_water
