# build_full_MRIO_table.R
# Reads MARIO - Aggregated (2).xlsx and produces full_MRIO_table.xlsx in data/
# which adds a `fd_coefficients` sheet containing bilateral final-demand shares
# delta^{rs}_{n,k} = fd_flow_{r,n,k} / column_total_k  for each channel k.
#
# Structure of source file:
#   rows 1-2   : header (region / sector name)
#   rows 3-56  : EU sectors  (N=54)
#   rows 57-110: RoW sectors (N=54)
#   cols 1-3   : row labels  (region, type, name)
#   cols 4-57  : EU intermediate demand
#   cols 58-111: RoW intermediate demand
#   cols 112-115: EU FD  (Consumption, Gov, GFCF, Exports[=0])
#   cols 116-119: RoW FD (Consumption, Gov, GFCF, Exports[=0])
#   col  120   : Total Final Demand
#   col  121   : Sector Share in Total FD  (= eta in current model)

library(openxlsx)

src  <- "data/impacts/MARIO - Aggregated (2).xlsx"
dest <- "data/full_MRIO_table.xlsx"

# ── 1. Load source sheets ─────────────────────────────────────────────────────
flows <- read.xlsx(src, sheet = "flows",        rowNames = FALSE)
coefs <- read.xlsx(src, sheet = "coefficients", rowNames = FALSE)
units <- read.xlsx(src, sheet = "units",        rowNames = FALSE)

# ── 2. Parse structure ────────────────────────────────────────────────────────
N        <- 54          # sectors per region
R        <- 2           # regions
row_eu   <- 3:(2 + N)          # rows 3-56  → EU sectors
row_row  <- (3 + N):(2 + R*N) # rows 57-110 → RoW sectors
row_sec  <- c(row_eu, row_row) # all 108 sector rows

# FD column indices (1-based in the data frame)
fd_cols  <- 112:119

# Channel labels (from header row 2, dropping Exports which are all zero)
fd_channels_raw <- c(
  "EU_Consumption", "EU_Gov", "EU_GFCF", "EU_Exports",
  "RoW_Consumption", "RoW_Gov", "RoW_GFCF", "RoW_Exports"
)
# Keep all 8 for completeness; users can see Exports=0 in the data

# Row labels
row_labels <- data.frame(
  Region  = flows[row_sec, 1],
  Type    = flows[row_sec, 2],
  Sector  = flows[row_sec, 3],
  stringsAsFactors = FALSE
)

# ── 3. Extract FD flows ───────────────────────────────────────────────────────
fd_flows <- flows[row_sec, fd_cols]
fd_flows[] <- lapply(fd_flows, function(x) suppressWarnings(as.numeric(x)))
fd_flows[is.na(fd_flows)] <- 0

colnames(fd_flows) <- fd_channels_raw

# ── 4. Compute FD coefficients delta^{rs}_{n,k} ──────────────────────────────
# delta^{rs}_{n,k} = fd_flow_{r,n,k} / sum_{r,n} fd_flow_{r,n,k}
col_totals <- colSums(fd_flows, na.rm = TRUE)

fd_coefs <- fd_flows
for (k in seq_along(fd_channels_raw)) {
  if (col_totals[k] > 0) {
    fd_coefs[, k] <- fd_flows[, k] / col_totals[k]
  } else {
    fd_coefs[, k] <- 0
  }
}

# Sanity check: each non-zero column should sum to 1
tol_check <- sapply(fd_channels_raw, function(ch) {
  if (col_totals[ch] > 0) round(sum(fd_coefs[[ch]]), 6) else NA
})
message("Column sums of fd_coefs (should be 1 or NA for zero columns):")
print(tol_check)

# Also compute "eta" = share in total final demand (col 120 = Total FD)
total_fd <- suppressWarnings(as.numeric(flows[row_sec, 120]))
total_fd[is.na(total_fd)] <- 0
grand_total_fd <- sum(total_fd, na.rm = TRUE)
eta <- total_fd / grand_total_fd

# ── 5. Build output data frames ───────────────────────────────────────────────
# 5a. fd_flows sheet: raw values + eta
fd_flows_out <- cbind(row_labels, fd_flows,
                      Total_FD        = total_fd,
                      eta_share_total = eta)

# 5b. fd_coefficients sheet: delta shares + eta
fd_coefs_out <- cbind(row_labels, fd_coefs,
                      Total_FD        = total_fd,
                      eta_share_total = eta)

# 5c. Column-total row for fd_coefs (verification)
total_row_coefs <- c(
  Region = "TOTAL", Type = "", Sector = "Column sum (should = 1 if non-zero)",
  setNames(as.list(round(col_totals_check <- colSums(fd_coefs, na.rm=TRUE), 6)),
           fd_channels_raw),
  Total_FD        = sum(total_fd),
  eta_share_total = round(sum(eta), 6)
)
fd_coefs_out <- rbind(fd_coefs_out, total_row_coefs)

# ── 6. Write new workbook ─────────────────────────────────────────────────────
wb <- createWorkbook()

# Sheet 1: flows (original, reference)
addWorksheet(wb, "flows")
writeData(wb, "flows", flows)

# Sheet 2: coefficients (A matrix, original, reference)
addWorksheet(wb, "coefficients")
writeData(wb, "coefficients", coefs)

# Sheet 3: fd_flows (raw bilateral FD flows, sector-level)
addWorksheet(wb, "fd_flows")
writeData(wb, "fd_flows", fd_flows_out)

# Sheet 4: fd_coefficients (delta^{rs}_{n,k} shares)
addWorksheet(wb, "fd_coefficients")
writeData(wb, "fd_coefficients", fd_coefs_out)

# Sheet 5: units (original, reference)
addWorksheet(wb, "units")
writeData(wb, "units", units)

# Light formatting: freeze panes and bold headers
for (sh in c("fd_flows", "fd_coefficients")) {
  freezePane(wb, sh, firstRow = TRUE, firstCol = TRUE)
  addStyle(wb, sh,
           style = createStyle(textDecoration = "bold", wrapText = TRUE),
           rows = 1, cols = 1:ncol(fd_coefs_out), gridExpand = TRUE)
}

saveWorkbook(wb, dest, overwrite = TRUE)
message("Saved: ", dest)
