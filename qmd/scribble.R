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
  )


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

sc_long <- sc %>%
  select(shock, domain, shift, from, to, display_name) %>%
  tidyr::pivot_longer(
    cols = c(from, to),
    names_to = "endpoint", # "from" or "to"
    values_to = "sector"
  )

lvl <- rev(sct_index$label) # already in the order you want

A_filtered <- A %>%
  filter(input_num %in% sc_long$sector & input_region == 'EU') %>%
  left_join(
    sc_long,
    by = c("input_num" = "sector"),
    relationship = 'many-to-many'
  ) %>%
  mutate(
    output_label = factor(output_label, levels = lvl)
  )

A_filtered %>%
  ggplot(aes(y = output_label, x = value, fill = endpoint)) +
  facet_grid(~ output_region + paste0('Shock ', shock, '\n', shift)) +
  geom_bar(stat = 'identity', position = 'dodge')


na_out <- A_filtered %>%
  filter(is.na(output_label)) %>%
  group_by(output_num) %>%
  summarise(
    n = n(),
    total_value = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_value))

na_out

sector_list %>%
  semi_join(na_out, by = c("sector_code" = "output_num")) %>%
  distinct(sector_code, label)
