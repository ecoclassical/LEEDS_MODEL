df <- read.csv(file = 'data/scenarios/scenarios_table_0.csv') %>%
  dplyr::mutate(
    display_name = paste0(
      'Scenario ',
      shock,
      ' | ',
      domain,
      ' | ',
      sector,
      ' ',
      transaction,
      ' | ',
      shift,
      ' Shift'
    ),
    rds_file = paste0('data/scenarios/shock_runs/shock_', shock, '_run.RDS'),
    table_file = paste0(
      'data/scenarios/shock_tables/shock_',
      shock,
      '_table.csv'
    )
  )

df %>%
  write.csv(
    file = file.path(root, 'data', 'scenarios', 'scenarios.csv'),
    row.names = F
  )
