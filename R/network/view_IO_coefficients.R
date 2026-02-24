initial <- load.new.init(
  identif = 'data/initial_state_2026_uniform_B_coefficients_PLE_CE .xlsx'
)

idx <- which(initial$B.matrix == 1.5, arr.ind = TRUE)

targeted_sectors <- data.frame(
  row = rownames(initial$B.matrix)[idx[, 1]],
  col = colnames(initial$B.matrix)[idx[, 2]]
)

targeted_sectors %>% write.csv('data/targeted_sectors.csv', row.names = FALSE)

n_sector <- nrow(sector_list) # 54


# ---- scenario table ----

sc <- scenarios_tbl %>%
  rename(
    domain = Domain,
    shift = Shift,
    from = From,
    to = To,
    mechanism = Mechanism,
    conserved = Conserved_Quantity
  )

sc %>%
  write.csv('data/scenarios_table.csv', row.names = FALSE)

sc_long <- sc %>%
  select(shock, domain, shift, from, to, display_name) %>%
  tidyr::pivot_longer(
    cols = c(from, to),
    names_to = "endpoint", # "from" or "to"
    values_to = "sector"
  )

# ---- A matrix ----

A <- initial$A.matrix %>%
  as.data.frame() %>%
  tibble::rownames_to_column("input") %>%
  mutate(input_num = match(input, dimnames(initial$A.matrix)[[1]])) %>% # row index
  tidyr::pivot_longer(
    cols = -c(input, input_num),
    names_to = "output",
    values_to = "value"
  ) %>%
  mutate(output_num = match(output, dimnames(initial$A.matrix)[[2]])) %>% # col index
  mutate(
    input_num = ((input_num - 1) %% n_sector) + 1,
    output_num = ((output_num - 1) %% n_sector) + 1
  ) %>%
  tidyr::separate_wider_delim(
    input,
    delim = ".",
    names = c("input_region", "input_sector"),
    too_many = "merge"
  ) %>%
  tidyr::separate_wider_delim(
    output,
    delim = ".",
    names = c("output_region", "output_sector"),
    too_many = "merge"
  ) %>%
  left_join(
    sector_list %>% mutate(label = gsub("<br>", " ", label)),
    by = c("input_num" = "sector_code")
  ) %>%
  rename(input_label = label) %>%
  left_join(
    sector_list %>% mutate(label = gsub("<br>", " ", label)),
    by = c("output_num" = "sector_code")
  ) %>%
  rename(output_label = label)


# ---- prepare A ----

lvl <- sector_list %>%
  mutate(label = gsub("<br>", " ", label)) %>%
  pull(label) %>%
  rev # already in the order you want

A_filtered <- A %>%
  filter(input_num %in% sc_long$sector & input_region == 'EU') %>%
  left_join(
    sc_long,
    by = c("input_num" = "sector"),
    relationship = 'many-to-many'
  ) %>%
  filter(domain == 'Production') %>%
  mutate(
    output_label = factor(output_label, levels = lvl),
    endpoint = recode(endpoint, from = 'Brown', to = 'Clean'),
    shock = as.integer(shock),
    facet_lab = paste0("Shock ", shock, "\n", shift, "\n", output_region),
    facet_lab = factor(facet_lab, levels = unique(facet_lab[order(shock)]))
  )


# ---- plot ----

p <- A_filtered %>%
  ggplot(aes(y = output_label, x = value, fill = endpoint)) +
  facet_grid(~ facet_lab + output_region) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(
    x = '',
    y = '',
    fill = 'Target EU Sector',
    title = 'Input Incidence of Targeted EU Sectors by Scenario and Region',
    subtitle = "In which sectors' production recipes does this input appear?"
  ) +
  theme(
    plot.title = element_text(face = 'bold', size = 18),
    plot.subtitle = element_text(face = 'italic'),
    legend.title = element_text(face = 'bold'),
    legend.position = 'bottom',
    axis.text.x = element_text(size = 8)
  )

p

ggsave(
  filename = "io_shock_coefficients.png",
  plot = p,
  width = 20, # inches — increase if still cramped
  height = 12,
  dpi = 300,
  bg = "white"
)

#---- plot A2 ----

A2 <- A %>%
  filter(output_num %in% sc$from & output_region == 'EU') %>%
  left_join(
    sc,
    by = c("output_num" = "from"),
    relationship = 'many-to-many'
  ) %>%
  filter(domain == 'Production') %>%
  mutate(
    input_label = factor(input_label, levels = lvl),
    shock = as.integer(shock),
    facet_lab = paste0("Shock ", shock, "\n", shift, "\n", output_region),
    facet_lab = factor(facet_lab, levels = unique(facet_lab[order(shock)]))
  )

p2 <- A2 %>%
  ggplot(aes(y = input_label, x = value, fill = input_region)) +
  facet_grid(~facet_lab) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(
    x = '',
    y = '',
    fill = 'Target EU Sector',
    title = 'Backward Linkages (Column Coefficients) of Targeted EU Manufacturing Sectors',
    subtitle = 'From which supplying sectors does this sector obtain its inputs?'
  ) +
  theme(
    plot.title = element_text(face = 'bold', size = 18),
    plot.subtitle = element_text(face = 'italic'),
    legend.title = element_text(face = 'bold'),
    legend.position = 'bottom',
    axis.text.x = element_text(size = 8)
  )

p2

ggsave(
  filename = "io_shock_coefficients2.png",
  plot = p2,
  width = 20, # inches — increase if still cramped
  height = 12,
  dpi = 300,
  bg = "white"
)
