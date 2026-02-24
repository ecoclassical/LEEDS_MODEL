# Data
# df <- read.csv(file = 'data/Results_PLE_policy_brief_table.csv')
# df <- read.csv(file = 'data/Results_CE_inputs_policy_brief_table.csv')

# Packages
library(reshape2)
library(dplyr)
library(stringr)
library(patchwork)

# Functions
replace_second_space <- function(x) {
  # Find the positions of spaces
  space_positions <- str_locate_all(x, " ")[[1]]

  # If there are at least two spaces, replace the second one
  if (nrow(space_positions) >= 2) {
    second_space_position <- space_positions[2, "start"]
    # Replace the second space with a line break
    x <- str_sub(x, 1, second_space_position - 1) %>%
      str_c("\n", str_sub(x, second_space_position + 1, str_length(x)))
  }

  return(x)
}


dff <- shock_df %>%
  dplyr::rename(category = Name, variable = Variable) %>%
  reshape2::melt(
    id.vars = c("variable", "category"),
    variable.name = "region_term",
    value.name = "value"
  ) %>%
  dplyr::mutate(
    value = as.numeric(value),
    dimension = "aggregate",
    term = stringr::str_extract(
      region_term,
      "Immediate|Short\\.Term|Long\\.Term"
    ),
    region = stringr::str_extract(region_term, "Z1|Z2") %>%
      dplyr::recode(Z1 = "Core", Z2 = "Periphery"),
    term = factor(
      term,
      levels = c("Immediate", "Short.Term", "Long.Term"),
      labels = c("Short\nTerm", "Medium\nTerm", "Long\nTerm")
    )
  ) %>%
  dplyr::left_join(
    variable.table %>% dplyr::select(dimension, label, name, unit),
    by = c("dimension", "variable" = "label")
  ) %>%
  dplyr::mutate(
    display_name = paste0(
      sapply(name, replace_second_space),
      '\n(',
      variable,
      ', ',
      unit,
      ')'
    )
  )

col_pos <- "#17B3B8" # teal
col_neg <- "#F05A50" # red

# shock.title <- "Scenario 1: Household Demand - Green Diet Shift"
shock.subtitle <- "Teal = positive change, red = negative change (relative to baseline)"

p1 <- dff %>%
  filter(
    category == 'Macroeconomic' &
      variable != 'lf' &
      variable != 'lh'
  ) %>%
  ggplot(aes(x = term, y = value, fill = value > 0)) +
  geom_bar(stat = 'identity', position = 'dodge', show.legend = F) +
  facet_grid(display_name ~ region, scales = 'free_y') +
  geom_hline(yintercept = 0, linetype = 'dashed', linewidth = .4) +
  scale_fill_manual(
    values = c("TRUE" = "#17B3B8", "FALSE" = "#F05A50")
  ) +
  theme(
    strip.text.y.right = element_text(angle = 0),
    plot.title = element_text(face = 'bold', size = 16)
  ) +
  labs(title = 'Economic Dimension') +
  ylab('')
p11 <- dff %>%
  filter(
    variable == 'lf' |
      variable == 'lh'
  ) %>%
  ggplot(aes(x = term, y = value, fill = value > 0)) +
  geom_bar(stat = 'identity', position = 'dodge', show.legend = F) +
  facet_grid(display_name ~ region, scales = 'free_y') +
  geom_hline(yintercept = 0, linetype = 'dashed', linewidth = .4) +
  scale_fill_manual(
    values = c("TRUE" = "#17B3B8", "FALSE" = "#F05A50")
  ) +
  theme(
    strip.text.y.right = element_text(angle = 0),
    plot.title = element_text(face = 'bold', size = 16)
  ) +
  labs(title = 'Financial Dimension') +
  ylab('')
p2 <- dff %>%
  filter(category == 'Social') %>%
  ggplot(aes(x = term, y = value, fill = value > 0)) +
  geom_bar(stat = 'identity', position = 'dodge', show.legend = F) +
  geom_hline(yintercept = 0, linetype = 'dashed', linewidth = .4) +
  facet_grid(display_name ~ region, scales = 'free_y') +
  scale_fill_manual(
    values = c("TRUE" = "#17B3B8", "FALSE" = "#F05A50")
  ) +
  theme(
    strip.text.y.right = element_text(angle = 0),
    plot.title = element_text(face = 'bold', size = 16)
  ) +
  labs(title = 'Social Dimension') +
  ylab('')
p3 <- dff %>%
  filter(category == 'Ecological') %>%
  ggplot(aes(x = term, y = value, fill = value < 0)) +
  geom_bar(stat = 'identity', position = 'dodge', show.legend = F) +
  geom_hline(yintercept = 0, linetype = 'dashed', linewidth = .4) +
  facet_grid(display_name ~ region, scales = 'free_y') +
  scale_fill_manual(
    values = c("TRUE" = "#17B3B8", "FALSE" = "#F05A50")
  ) +
  theme(
    strip.text.y.right = element_text(angle = 0),
    plot.title = element_text(face = 'bold', size = 16)
  ) +
  labs(title = 'Ecological Dimension') +
  ylab('')
# gridExtra :: grid.arrange(p1, p2, p3, ncol = 1)

# Assuming you have three ggplot objects: p1, p2, p3

# Set the same y-axis limits (if applicable) for consistent scaling across all plots
# y_limits <- c(
#   min(c(min(p1$data$value), min(p2$data$value), min(p3$data$value))),
#   max(c(max(p1$data$value), max(p2$data$value), max(p3$data$value)))
# )

# p1 <- p1 + ylim(y_limits)
# p2 <- p2 + ylim(y_limits)
# p3 <- p3 + ylim(y_limits)

# aspect_ratio <- NULL # Adjust this ratio as needed for your specific plot dimensions

# p1 <- p1 + theme(aspect.ratio = aspect_ratio)
# p11 <- p11 + theme(aspect.ratio = aspect_ratio)
# p2 <- p2 + theme(aspect.ratio = aspect_ratio)
# p3 <- p3 + theme(aspect.ratio = aspect_ratio)

# Combine plots using patchwork with specific plot layout settings
# combined_plot2 <- ((p1 / p11) + plot_layout(heights = c(.7, .3)) | (p2 / p3)) # + plot_layout(ncol = 2, heights = c(0.8, .2, .5, .5))

combined_plot2 <- ((p1 / p11) + plot_layout(heights = c(.7, .3)) | (p2 / p3)) +
  patchwork::plot_annotation(
    title = shock.title,
    subtitle = shock.subtitle,
    theme = theme(
      plot.title = element_text(size = 24, face = "bold"),
      plot.subtitle = element_text(size = 12, face = 'italic')
    )
  )
# p1 / p2 / p3 +
# combined_plot1 <- p1 / p2 / p3 +
#   plot_layout(ncol = 1, heights = c(1.5, 0.75, .75)) &  # Ensure each plot gets equal height
#   theme(
#     strip.text.y = element_text(angle = 0),     # Ensure facet labels are not rotated
#     plot.margin = margin(1, 1, 1, 1)    # Ensure consistent margins around plots
#   )

# Display the combined plot
print(combined_plot2)

if (exists('combined_plot1')) {
  pdf(shock_table_1_filename, width = 8, height = 12)
  print(combined_plot1)
  dev.off()
}

if (exists('combined_plot2')) {
  pdf(shock_table_2_filename, width = 13, height = 11)
  print(combined_plot2)
  dev.off()
}

combined_plot2
