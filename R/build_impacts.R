library(tidyverse)

df <- read.csv('data/impacts.csv', header = FALSE, stringsAsFactors = FALSE)
impacts <- t(df)
impact_names <- impacts[1, ]
colnames(impacts) <- impact_names

impacts <- impacts[-1, ]
all_impacts <- as.data.frame(impacts)
rownames(all_impacts) <- NULL

saveRDS(all_impacts, 'data/all_impacts_wide.RDS')
all_impacts_long <- all_impacts |>
  pivot_longer(
    cols = -c(industry, region, country, sector, impact),
    names_to = "variable",
    values_to = "value"
  )
all_impacts_long$value <- as.numeric(all_impacts_long$value)
saveRDS(all_impacts_long, 'data/all_impacts_long.RDS')

impacts_long <- all_impacts_long |>
  group_by(industry, region, impact, variable) |>
  summarize(value = sum(value, na.rm = T))

variable_names <- unique(all_impacts_long$variable)
grep('Extraction', variable_names)
grep('GHG', variable_names)

emissions <- impacts_long |>
  filter(
    variable ==
      'GHG emissions (GWP100) | Problem oriented approach: baseline (CML, 2001) | GWP100 (IPCC, 2007)'
  ) %>%
  rename(sector = impact, emissions = value) %>%
  ungroup %>%
  select(-industry, -variable)

df <- read.csv('data/impact_coefficients.csv') %>%
  left_join(emissions, by = c('region', 'sector')) %>%
  mutate(carbon.intensity = emissions / (1000 * Gross.Output))
