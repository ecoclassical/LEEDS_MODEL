initial <- load.new.init(
  identif = 'data/initial_state_2026_uniform_B_coefficients_PLE_CE .xlsx'
)

df <- initial$B.matrix
which(initial$B.matrix == 1.5, arr.ind = TRUE)

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


# sct <- dimnames(initial$A.matrix)[[1]] %>%
#   grep("EU\\.", ., value = T) %>%
#   stringr::str_remove("^EU\\.")

# sct_index <- tibble::tibble(
#   sector = sct,
#   sector_num = seq_along(sct)
# ) %>%
#   left_join(sector_list, by = c("sector_num" = "sector_code")) %>%
#   mutate(label = gsub("<br>", " ", label))

# A <- A %>%
#   left_join(sct_index, by = c("input_sector" = "sector")) %>%
#   rename(input_num = sector_num, input_label = label) %>%
#   left_join(sct_index, by = c("output_sector" = "sector")) %>%
#   rename(output_num = sector_num, output_label = label) %>%
#   filter(is.na(input_num) | is.na(output_num)) %>%
#   pull(output_sector) %>%
#   unique

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


p <- A_filtered %>%
  ggplot(aes(y = output_label, x = value, fill = endpoint)) +
  facet_grid(~ facet_lab + output_region) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(
    x = '',
    y = '',
    fill = 'Target EU Sector',
    title = 'Input-Output Coefficients of Targeted EU Sectors by Scenario and Region',
    subtitle = 'Only Production Shifts operating on IO coefficients are shown'
  ) +
  theme(
    plot.title = element_text(face = 'bold', size = 14),
    plot.subtitle = element_text(face = 'italic'),
    legend.title = element_text(face = 'bold'),
    legend.position = 'top',
    axis.text.x = element_text(size = 6)
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

# na_out <- A_filtered %>%
#   filter(is.na(output_label)) %>%
#   group_by(output_num) %>%
#   summarise(
#     n = n(),
#     total_value = sum(value, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   arrange(desc(total_value))

# na_out

# sector_list %>%
#   semi_join(na_out, by = c("sector_code" = "output_num")) %>%
#   distinct(sector_code, label)

# A_plot <- A_filtered %>%
#   group_by(shock, shift, output_label) %>%
#   mutate(total_sector = sum(abs(value), na.rm = TRUE)) %>%
#   ungroup() %>%
#   group_by(shock, shift) %>%
#   mutate(panel_total = sum(abs(value), na.rm = TRUE)) %>%
#   ungroup() %>%
#   filter(total_sector / panel_total > 0.002)   # 0.2% contribution threshold

# A_plot %>%
#   ggplot(aes(y = output_label, x = value, fill = endpoint)) +
#   facet_grid(~ output_region + paste0('Shock ', shock, '\n', shift)) +
#   geom_bar(stat = 'identity', position = 'dodge')
