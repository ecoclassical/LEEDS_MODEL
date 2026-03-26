raw <- read.csv(
  "data/mrio_table.csv",
  header = FALSE,
  stringsAsFactors = FALSE,
  fill = TRUE,
  quote = '"'
)

N <- 54
data_rows <- 4:(4 + 2 * N - 1)

# Extract FD matrix: cols DH to DO = 112:119
fd_matrix <- raw[data_rows, 112:119]
fd_matrix[] <- lapply(fd_matrix, function(x) suppressWarnings(as.numeric(x)))
fd_matrix[is.na(fd_matrix)] <- 0

colnames(fd_matrix) <- c(
  "EU_cons",
  "EU_gov",
  "EU_gfcf",
  "EU_exports",
  "RoW_cons",
  "RoW_gov",
  "RoW_gfcf",
  "RoW_exports"
)
# rownames(fd_matrix) <- paste0(rep(c('Z1 | ', 'Z2 | '), each = N), raw[data_rows, 3])

# B matrices (108 × 2)
B_beta <- as.matrix(fd_matrix[, c("EU_cons", "RoW_cons")])
B_sigma <- as.matrix(fd_matrix[, c("EU_gov", "RoW_gov")])
B_iota <- as.matrix(fd_matrix[, c("EU_gfcf", "RoW_gfcf")])

# Sanity check
cat("B_beta col sums:", colSums(B_beta, na.rm = TRUE), "\n") # should be 1, 1
cat("B_sigma col sums:", colSums(B_sigma, na.rm = TRUE), "\n")
cat("B_iotacol sums:", colSums(B_iota, na.rm = TRUE), "\n")

# But first confirm the column indices are right by checking spot values against what you know — run this first:

# Show what's in the FD columns for the first few rows
sim[zk.lab('d'), i] <-
  B_beta %*%
  sim[z.lab('c'), i] +
  B_sigma %*% sim[z.lab('g'), i] +
  B_iota %*% sim[z.lab('id'), i] +
  B_iota %*% sim[z.lab('id_g'), i] # iota_g = iota for simplicity
