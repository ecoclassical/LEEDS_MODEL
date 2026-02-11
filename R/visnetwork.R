library(visNetwork)
library(scales)

# ---- adjacency matrix ----
ntwrk <- as.matrix(initial$A.matrix)
stopifnot(nrow(ntwrk) == ncol(ntwrk))

n <- nrow(ntwrk)

# remove self-loops
diag(ntwrk) <- 0

# ---- sector labels (54 sectors, repeated twice) ----
sector_list <- read.csv('data/sector_list.csv', stringsAsFactors = FALSE)
# convert the literal "<br>" sequences to actual newline characters
sector_list$label <- gsub("<br>", "\n", sector_list$label, fixed = TRUE)

n_sectors <- 54
n_regions <- n / n_sectors
stopifnot(n_regions == 2)

region <- rep(c("EU", "RoW"), each = n_sectors)

pal <- scales::hue_pal()(4)
region_colors <- c(
  EU = pal[1], # same red as igraph
  RoW = pal[3] # same green/emerald as igraph
)

labels <- rep(sector_list$label, length.out = n)

# ---- nodes ----
nodes <- data.frame(
  id = seq_len(n),
  label = labels,
  value = rowSums(ntwrk),
  group = region,
  color = region_colors[region],
  stringsAsFactors = FALSE
)

# ---- edge threshold: top 10% (given) ----
thr <- 0.001

idx <- which(ntwrk > thr, arr.ind = TRUE)
stopifnot(nrow(idx) > 0)

w <- ntwrk[idx]
w_max <- max(w)

# rescale above threshold
w_scaled <- (w - thr) / (w_max - thr)
w_scaled[w_scaled < 0] <- 0

# nonlinear alpha (visibility kicks in above threshold)
gamma_alpha <- 1.05
alpha_vals <- w_scaled^gamma_alpha

edges <- data.frame(
  from = idx[, "row"],
  to = idx[, "col"],
  value = w,
  width = 0.5 + .5 * w_scaled,
  color = alpha("steelblue", alpha_vals),
  stringsAsFactors = FALSE
)

# ---- visNetwork ----
visNetwork(nodes, edges, height = "700px", width = "100%") %>%
  visNodes(
    shape = "dot",
    scaling = list(min = 2, max = 20),
    font = list(size = 7)
  ) %>%
  visEdges(
    arrows = list(
      to = list(enabled = FALSE),
      from = list(enabled = FALSE)
    ),
    smooth = FALSE
  ) %>%
  visOptions(
    highlightNearest = TRUE,
    nodesIdSelection = TRUE
  ) %>%
  visPhysics(enabled = FALSE)
