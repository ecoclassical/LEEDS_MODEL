library(tidyverse)
library(dplyr)
library(forcats)
library(ggplot2)

df <- read.csv("data/all_impacts.csv", check.names = FALSE) %>%
  t() %>%
  as.data.frame(stringsAsFactors = FALSE)

colnames(df) <- as.character(unlist(df[1, ]))
df <- df[-1, ]
rownames(df) <- NULL

id <- c("region_sector", "region", "country", "sector.old", "sector.new")
impact_cols <- setdiff(colnames(df), id)

df_long <- df %>%
  pivot_longer(
    cols = all_of(impact_cols),
    names_to = "impact",
    values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))


impact_names <- c(
  'Gross Output',
  'GHG emissions (GWP100) | Problem oriented approach: baseline (CML, 2001) | GWP100 (IPCC, 2007)',
  'Water Consumption Green - Agriculture',
  'Water Consumption Blue - Total',
  "Land use Crop, Forest, Pasture"
)
impact_greps <- c('Domestic Extraction Used -', 'Unused Domestic Extraction -')

dff <- df_long %>%
  filter(
    impact %in%
      impact_names |
      grepl(impact_greps[1], impact) |
      grepl(impact_greps[2], impact)
  ) %>%
  mutate(
    variable = case_when(
      grepl('Output', impact) ~ 'Output',
      grepl('Extraction', impact) ~ 'Extraction',
      grepl('Water', impact) ~ 'Water',
      grepl('GHG', impact) ~ 'Emissions',
      grepl('Land', impact) ~ 'Land',
      TRUE ~ 'Output'
    )
  ) %>%
  group_by(region, sector.new, variable) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  rename(sector = sector.new, impact = variable)

impacts <- dff %>%
  filter(impact != 'Output') %>%
  rename(total_value = value) %>%
  left_join(
    dff %>%
      filter(impact == 'Output') %>%
      rename(output = value) %>%
      select(-impact),
    by = c("region", "sector")
  ) %>%
  mutate(
    unit = case_when(
      impact == 'Emissions' ~ 'ton GHG',
      impact == 'Water' ~ 'ton',
      impact == 'Land' ~ 'ha',
      impact == 'Extraction' ~ 'ton',
      TRUE ~ 'ton'
    ),
    unit = paste(unit, 'per million USD output'),
    value = output / total_value
  )

write.csv(impacts, "data/impacts.csv", row.names = FALSE)
