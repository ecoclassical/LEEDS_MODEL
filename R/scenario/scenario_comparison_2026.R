# Data
# df <- read.csv(file = 'data/Results_PLE_policy_brief_table.csv')
# df <- read.csv(file = 'data/Results_CE_inputs_policy_brief_table.csv')

# Packages
library(reshape2)
library(dplyr)
library(stringr)
library(patchwork)
library(ggplot2)

replace_second_space <- function(x) {
  if (is.na(x) || !nzchar(x)) {
    return(x)
  }
  space_positions <- stringr::str_locate_all(x, " ")[[1]]
  if (nrow(space_positions) >= 2) {
    pos <- space_positions[2, "start"]
    x <- paste0(substr(x, 1, pos - 1), "\n", substr(x, pos + 1, nchar(x)))
  }
  x
}

dff <- df %>%
  rename(category = Name, variable = Variable) %>%
  melt(
    id.vars = c("variable", "category", "scenario"),
    variable.name = "region_term",
    value.name = "value"
  ) %>%
  mutate(
    value = as.numeric(value),
    term = str_extract(region_term, "Immediate|Short\\.Term|Long\\.Term"),
    region = str_extract(region_term, "Z1|Z2") %>%
      recode(Z1 = "EU", Z2 = "RoW")
  ) %>%
  mutate(
    dimension = 'aggregate',
    term = factor(
      term,
      levels = c("Immediate", "Short.Term", "Long.Term"),
      labels = c("Immediate", "Short term", "Long term")
    )
  ) %>%
  # IMPORTANT: join on *dimension/category* + label to avoid many-to-many
  left_join(
    variable.table %>% select(dimension, label, name, unit),
    by = c("dimension", "variable" = "label")
  ) %>%
  mutate(
    display_name = paste0(
      sapply(name, replace_second_space),
      '\n(',
      variable,
      ', ',
      unit,
      ')'
    ),
    display_scenario = stringr::str_replace(
      scenario,
      "( \\| .*?)( \\| )",
      "\\1\n"
    ),
    shock = as.integer(stringr::str_extract(scenario, "(?<=Scenario\\s)\\d+"))
  ) %>%
  filter(!is.na(value))


cats <- levels(factor(dff$category))

plots <- lapply(cats, function(cc) {
  ggplot(
    dplyr::filter(dff, category == cc),
    aes(term, value, fill = display_scenario)
  ) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    facet_grid(display_name ~ region, scales = "free_y") +
    labs(title = cc, y = NULL, x = NULL) +
    geom_hline(yintercept = 0, linetype = 'dashed', color = 'lightgray') +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      strip.text = element_text(size = 9),
      strip.text.y.right = element_text(size = 9, angle = 0)
    )
})

p.comparison <- patchwork::wrap_plots(plots, ncol = 1, guides = "collect") &
  theme(legend.position = "top")

p.comparison
