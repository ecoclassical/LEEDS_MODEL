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
      recode(Z1 = "Core", Z2 = "Periphery")
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
    display_name = sapply(name, replace_second_space)
  )

make_cat_plot <- function(cat_name, vars) {
  dd <- dff %>%
    filter(variable %in% vars)

  # Fail fast: empty data = patchwork faceting crash later
  if (nrow(dd) == 0) {
    return(
      ggplot() +
        theme_void() +
        labs(title = paste0(cat_name, " (no matching variables)"))
    )
  }

  ggplot(dd, aes(x = term, y = value, fill = scenario)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    facet_grid(display_name ~ region, scales = "free_y") +
    labs(title = cat_name, y = NULL, x = NULL) +
    theme(
      strip.text.y.right = element_text(angle = 0),
      plot.title = element_text(face = "bold")
    )
}

plots <- Map(make_cat_plot, names(selected.list), selected.list)

p.comparison <- wrap_plots(plots, ncol = 1) +
  plot_annotation(
    title = "Scenario Comparison: Diet shift vs Energy transition"
  )

ggplot(dff, aes(y = value, x = region, fill = term)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_grid(name ~ category, scales = "free") +
  labs(
    title = "Scenario Comparison: Diet shift vs Energy transition",
    y = NULL,
    x = NULL
  ) +
  theme(
    strip.text.y.right = element_text(angle = 0),
    plot.title = element_text(face = "bold")
  )


# dff2 <- dff %>%
#   dplyr::mutate(
#     category = factor(category, levels = c(
#       "Debt.and.Wealth","Ecological","Employment","External","Inequality","Macroeconomic","Social"
#     )),
#     name = factor(name),
#     facet = factor(name,
#                    levels = unique(paste(category, name, sep = " — ")[order(category, name)]))
#   ) %>%
#   mutate(
#     display_name = sapply(name, replace_second_space)
#   )

ggplot(dff2, aes(x = region, y = value, fill = term)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~facet, ncol = 7, scales = "free_y") +
  labs(
    title = "Scenario Comparison: Diet shift vs Energy transition",
    y = NULL,
    x = NULL
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(size = 9)
  )

cats <- levels(factor(dff$category))


plots <- lapply(cats, function(cc) {
  ggplot(dplyr::filter(dff, category == cc), aes(region, value, fill = term)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    facet_grid(display_name ~ ., scales = "free_y") +
    labs(title = cc, y = NULL, x = NULL) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      strip.text = element_text(size = 9),
      strip.text.y.right = element_text(size = 9, angle = 0)
    )
})

patchwork::wrap_plots(plots, ncol = 1, guides = "collect") &
  theme(legend.position = "right")
