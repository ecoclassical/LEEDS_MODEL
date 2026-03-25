# ============================================================================
# build_fullMRIO_xlsx.R
# Creates data/initial_state_2026_fullMRIO.xlsx from existing initial state
# by adding an FD_MRIO sheet with sector-level bilateral FD breakdown.
#
# Usage: Rscript model/build_fullMRIO_xlsx.R
# ============================================================================

library(openxlsx)

root <- tryCatch(
  normalizePath(file.path(dirname(sys.frame(1)$ofile), ".."), winslash = "/"),
  error = function(e) "/Users/parvulesco/Documents/R/LEEDS_MODEL"
)
if (!file.exists(file.path(root, "model", "bootstrap_2026.R"))) {
  root <- "/Users/parvulesco/Documents/R/LEEDS_MODEL"
}

model_dir <- file.path(root, "model")
utils_dir <- file.path(root, "utils")

# Source the utility functions needed for z.lab, zk.lab, etc.
# We need to set up N, K, zlabs before calling load.init
source(file.path(utils_dir, "aux_utils.R"))
source(file.path(utils_dir, "run_utils.R"))

# Load the initial state (for sector share vectors)
initial_file <- file.path(root, "data", "initial_state_2026.xlsx")
stopifnot(file.exists(initial_file))
initial <- load.init(initial_file)

# Read sector labels
sector_list <- read.csv(file.path(root, "data", "sector_list.csv"),
                        stringsAsFactors = FALSE)
sector_list$label <- gsub("<br>", " ", sector_list$label)

cat("K =", K, "  N =", N, "  zlabs =", paste(zlabs, collapse = ", "), "\n")

# ---- Extract aggregate demands from baseline t=2 ----
# (t=1 is all-zero initial conditions; t=2 is first computed period)
baseline_file <- file.path(root, "output", "scenarios", "baseline_2026.RDS")
stopifnot(file.exists(baseline_file))
baseline <- readRDS(baseline_file)
sim <- baseline$simulation
t_ref <- 2   # first non-zero period

gv <- function(nm) as.numeric(sim[nm, t_ref])

Z1_c   <- gv("Z1_c");   Z2_c   <- gv("Z2_c")
Z1_g   <- gv("Z1_g");   Z2_g   <- gv("Z2_g")
Z1_id  <- gv("Z1_id");  Z2_id  <- gv("Z2_id")
Z1_idg <- gv("Z1_id_g"); Z2_idg <- gv("Z2_id_g")
Z1_imp <- gv("Z1_imp"); Z2_imp <- gv("Z2_imp")

# Sector-level share vectors (from initial state vars — these are calibrated parameters)
vars <- initial$vars
v <- setNames(initial$vars$value, initial$vars$label)

Z1_beta   <- as.numeric(v[paste0("Z1_beta-",   1:K)])
Z2_beta   <- as.numeric(v[paste0("Z2_beta-",   1:K)])
Z1_sigma  <- as.numeric(v[paste0("Z1_sigma-",  1:K)])
Z2_sigma  <- as.numeric(v[paste0("Z2_sigma-",  1:K)])
Z1_iota   <- as.numeric(v[paste0("Z1_iota-",   1:K)])
Z2_iota   <- as.numeric(v[paste0("Z2_iota-",   1:K)])
Z1_iota_g <- as.numeric(v[paste0("Z1_iota_g-", 1:K)])
Z2_iota_g <- as.numeric(v[paste0("Z2_iota_g-", 1:K)])
Z1_eta    <- as.numeric(v[paste0("Z1_eta-",    1:K)])
Z2_eta    <- as.numeric(v[paste0("Z2_eta-",    1:K)])

# ---- Compute channel shares of total FD per region ----
# (used to split imports across channels proportionally)
fd_Z1 <- Z1_c + Z1_g + Z1_id + Z1_idg
mu_Z1_hh  <- as.numeric(Z1_c   / fd_Z1)
mu_Z1_gov <- as.numeric(Z1_g   / fd_Z1)
mu_Z1_id  <- as.numeric(Z1_id  / fd_Z1)
mu_Z1_idg <- as.numeric(Z1_idg / fd_Z1)

fd_Z2 <- Z2_c + Z2_g + Z2_id + Z2_idg
mu_Z2_hh  <- as.numeric(Z2_c   / fd_Z2)
mu_Z2_gov <- as.numeric(Z2_g   / fd_Z2)
mu_Z2_id  <- as.numeric(Z2_id  / fd_Z2)
mu_Z2_idg <- as.numeric(Z2_idg / fd_Z2)

cat("Z1 FD channel shares: hh=", round(mu_Z1_hh, 4),
    " gov=", round(mu_Z1_gov, 4),
    " id=", round(mu_Z1_id, 4),
    " idg=", round(mu_Z1_idg, 4), "\n")
cat("Z2 FD channel shares: hh=", round(mu_Z2_hh, 4),
    " gov=", round(mu_Z2_gov, 4),
    " id=", round(mu_Z2_id, 4),
    " idg=", round(mu_Z2_idg, 4), "\n")

# ---- Imports by channel (split proportionally) ----
imp_Z1_hh  <- as.numeric(Z1_imp) * mu_Z1_hh
imp_Z1_gov <- as.numeric(Z1_imp) * mu_Z1_gov
imp_Z1_id  <- as.numeric(Z1_imp) * mu_Z1_id
imp_Z1_idg <- as.numeric(Z1_imp) * mu_Z1_idg

imp_Z2_hh  <- as.numeric(Z2_imp) * mu_Z2_hh
imp_Z2_gov <- as.numeric(Z2_imp) * mu_Z2_gov
imp_Z2_id  <- as.numeric(Z2_imp) * mu_Z2_id
imp_Z2_idg <- as.numeric(Z2_imp) * mu_Z2_idg

# ---- Z1 domestic FD by sector and channel (Z11 blocks) ----
Z1_fd_dom_hh  <- as.numeric(Z1_beta)   * (as.numeric(Z1_c)   - imp_Z1_hh)
Z1_fd_dom_gov <- as.numeric(Z1_sigma)  * (as.numeric(Z1_g)   - imp_Z1_gov)
Z1_fd_dom_id  <- as.numeric(Z1_iota)   * (as.numeric(Z1_id)  - imp_Z1_id)
Z1_fd_dom_idg <- as.numeric(Z1_iota_g) * (as.numeric(Z1_idg) - imp_Z1_idg)

# ---- Z1 cross-border FD (Z2 institutions buying from Z1) ----
# Z2 imports from Z1 = Z1_rex = Z2_imp (in the current model)
rex_Z1 <- as.numeric(Z2_imp)  # Z1's exports = Z2's imports
Z1_fd_xbr_hh  <- as.numeric(Z1_eta) * rex_Z1 * mu_Z2_hh
Z1_fd_xbr_gov <- as.numeric(Z1_eta) * rex_Z1 * mu_Z2_gov
Z1_fd_xbr_id  <- as.numeric(Z1_eta) * rex_Z1 * mu_Z2_id
Z1_fd_xbr_idg <- as.numeric(Z1_eta) * rex_Z1 * mu_Z2_idg

# ---- Z2 domestic FD by sector and channel (Z22 blocks) ----
Z2_fd_dom_hh  <- as.numeric(Z2_beta)   * (as.numeric(Z2_c)   - imp_Z2_hh)
Z2_fd_dom_gov <- as.numeric(Z2_sigma)  * (as.numeric(Z2_g)   - imp_Z2_gov)
Z2_fd_dom_id  <- as.numeric(Z2_iota)   * (as.numeric(Z2_id)  - imp_Z2_id)
Z2_fd_dom_idg <- as.numeric(Z2_iota_g) * (as.numeric(Z2_idg) - imp_Z2_idg)

# ---- Z2 cross-border FD (Z1 institutions buying from Z2) ----
rex_Z2 <- as.numeric(Z1_imp)  # Z2's exports = Z1's imports
Z2_fd_xbr_hh  <- as.numeric(Z2_eta) * rex_Z2 * mu_Z1_hh
Z2_fd_xbr_gov <- as.numeric(Z2_eta) * rex_Z2 * mu_Z1_gov
Z2_fd_xbr_id  <- as.numeric(Z2_eta) * rex_Z2 * mu_Z1_id
Z2_fd_xbr_idg <- as.numeric(Z2_eta) * rex_Z2 * mu_Z1_idg

# ---- Build the FD_MRIO dataframe (108 rows) ----
fd_mrio <- rbind(
  data.frame(
    region      = "Z1",
    sector_j    = 1:K,
    sector_name = sector_list$label[1:K],
    fd_dom_hh   = Z1_fd_dom_hh,
    fd_dom_gov  = Z1_fd_dom_gov,
    fd_dom_id   = Z1_fd_dom_id,
    fd_dom_idg  = Z1_fd_dom_idg,
    fd_xbr_hh   = Z1_fd_xbr_hh,
    fd_xbr_gov  = Z1_fd_xbr_gov,
    fd_xbr_id   = Z1_fd_xbr_id,
    fd_xbr_idg  = Z1_fd_xbr_idg,
    stringsAsFactors = FALSE
  ),
  data.frame(
    region      = "Z2",
    sector_j    = 1:K,
    sector_name = sector_list$label[1:K],
    fd_dom_hh   = Z2_fd_dom_hh,
    fd_dom_gov  = Z2_fd_dom_gov,
    fd_dom_id   = Z2_fd_dom_id,
    fd_dom_idg  = Z2_fd_dom_idg,
    fd_xbr_hh   = Z2_fd_xbr_hh,
    fd_xbr_gov  = Z2_fd_xbr_gov,
    fd_xbr_id   = Z2_fd_xbr_id,
    fd_xbr_idg  = Z2_fd_xbr_idg,
    stringsAsFactors = FALSE
  )
)

# ---- Accounting check: new FD sums should match old d vector ----
# The original d vector from the model:
# d = beta*c + sigma*g + iota*id + iota_g*id_g + eta_other*rex - eta*imp
# Our decomposition: d = (dom_hh + dom_gov + dom_id + dom_idg) + (xbr_hh + xbr_gov + xbr_id + xbr_idg)

fd_mrio$d_new <- fd_mrio$fd_dom_hh + fd_mrio$fd_dom_gov +
                 fd_mrio$fd_dom_id + fd_mrio$fd_dom_idg +
                 fd_mrio$fd_xbr_hh + fd_mrio$fd_xbr_gov +
                 fd_mrio$fd_xbr_id + fd_mrio$fd_xbr_idg

# Get old d values from baseline simulation at t_ref
d_old <- sapply(c(paste0("Z1_d-", 1:K), paste0("Z2_d-", 1:K)),
                function(nm) as.numeric(sim[nm, t_ref]))
fd_mrio$d_old <- d_old

cat("\n==== ACCOUNTING CHECK ====\n")
cat("Max absolute difference (d_new - d_old):", max(abs(fd_mrio$d_new - fd_mrio$d_old)), "\n")
cat("Max relative difference:", max(abs((fd_mrio$d_new - fd_mrio$d_old) / fd_mrio$d_old), na.rm = TRUE), "\n")
cat("Sum d_new Z1:", sum(fd_mrio$d_new[1:K]), "  Sum d_old Z1:", sum(fd_mrio$d_old[1:K]), "\n")
cat("Sum d_new Z2:", sum(fd_mrio$d_new[(K+1):(2*K)]), "  Sum d_old Z2:", sum(fd_mrio$d_old[(K+1):(2*K)]), "\n")

# Print first few rows
cat("\nFirst 6 rows of FD_MRIO:\n")
print(head(fd_mrio[, c("region", "sector_j", "fd_dom_hh", "fd_xbr_hh", "d_new", "d_old")]))

# Remove the check columns before writing
fd_mrio_write <- fd_mrio[, c("region", "sector_j", "sector_name",
                              "fd_dom_hh", "fd_dom_gov", "fd_dom_id", "fd_dom_idg",
                              "fd_xbr_hh", "fd_xbr_gov", "fd_xbr_id", "fd_xbr_idg")]

# ---- Build the xlsx ----
out_file <- file.path(root, "data", "initial_state_2026_fullMRIO.xlsx")

wb <- loadWorkbook(initial_file)
addWorksheet(wb, "FD_MRIO")

# Style: bold headers, freeze first row
headerStyle <- createStyle(textDecoration = "bold")
writeData(wb, sheet = "FD_MRIO", x = fd_mrio_write, headerStyle = headerStyle)
freezePane(wb, sheet = "FD_MRIO", firstRow = TRUE)

# Auto-size columns
setColWidths(wb, sheet = "FD_MRIO", cols = 1:ncol(fd_mrio_write), widths = "auto")

saveWorkbook(wb, out_file, overwrite = TRUE)
cat("\nSaved:", out_file, "\n")

# ---- Also save accounting check results ----
check_file <- file.path(root, "data", "fd_mrio_accounting_check.csv")
write.csv(fd_mrio, check_file, row.names = FALSE)
cat("Saved accounting check:", check_file, "\n")
