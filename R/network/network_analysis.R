library(tidyverse)

ntwrk <- as.matrix(initial$A.matrix)
node_size <- rowSums(ntwrk)

top_n <- 20
ix <- order(node_size, decreasing = TRUE)[1:top_n]


top_df <- node_size[ix] |>
  as.data.frame() |>
  rownames_to_column(var = "node") %>%
  mutate(region = sub("\\..*$", "", node), sector = sub("^[^.]*\\.", "", node))

names(top_df)[2] <- "size"

ggplot(top_df, aes(y = reorder(sector, size), x = size, fill = region)) +
  geom_bar(stat = "identity") +
  labs(x = '', y = '', title = 'Top nodes by size') +
  theme(plot.title = element_text(face = 'bold', size = 14))


library(igraph)

diag(ntwrk) <- 0

g <- graph_from_adjacency_matrix(
  ntwrk,
  mode = "directed",
  weighted = TRUE
)

ec <- eigen_centrality(
  g,
  directed = TRUE,
  weights = E(g)$weight
)$vector

top_n <- length(ix) # reuse same N if you want consistency
ix_ec <- order(ec, decreasing = TRUE)[1:top_n]

top_ec_df <- ec[ix_ec] |>
  as.data.frame() |>
  rownames_to_column(var = "node") %>%
  mutate(
    region = sub("\\..*$", "", node),
    sector = sub("^[^.]*\\.", "", node),
    label = paste0(sector, "\n", region)
  )

names(top_ec_df)[2] <- "eigenvector"

ggplot(
  top_ec_df,
  aes(
    y = reorder(label, eigenvector),
    x = eigenvector,
    fill = region
  )
) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "",
    y = "",
    title = "Top Nodes by Eigenvector Centrality"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )

intensity_names <- c(
  'Share in Household Consumption',
  # 'Final Demand in Real Terms',
  'Carbon Intensity',
  'Share of Imports',
  'Share of Investment',
  'Matter Intensity of Output',
  # 'Female Share of Employment',
  'Productivity'
)

# First, determine the order based on sector numbers
ordered_industries <- sector_list$label[sort(unique(intensities$sector))]
ordered_industries <- gsub('\\n', ' ', ordered_industries)

intensities <- initial$variables$industry |>
  filter(name %in% intensity_names) |>
  mutate(
    region = recode(area, Z1 = "EU", Z2 = "RoW")
  ) |>
  pivot_longer(
    cols = matches("^\\d+$"),
    names_to = "sector",
    values_to = "value"
  ) %>%
  mutate(
    sector = as.integer(sector),
    industry = paste0(
      gsub('\\n', ' ', sector_list$label[sector])
    ),
    industry = factor(industry, levels = rev(ordered_industries))
  )


# Then your plot will maintain this order
intensities %>%
  ggplot(aes(x = value, y = industry, fill = region)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(~name, scales = 'free_x') +
  scale_x_log10()
