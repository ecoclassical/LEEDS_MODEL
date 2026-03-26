# Model Structure

## Regions and Sectors

- **Z1 = EU**, **Z2 = Rest of World**
- K = 54 sectors per region; 2K = 108 total
- Variable prefix convention: `Z1_` = EU, `Z2_` = RoW
- Sectoral suffix: `Z1_x-j` = output of EU sector j

## Bilateral Final Demand Structure

Final demand vector **d** ∈ ℝ^{2K} (all sectors, both regions):

```
d = Ψ · f
```

where Ψ ∈ ℝ^{2K × RK} is the allocation matrix, f is the vector of aggregate demand scalars.

In scalar form (used in the loop for efficiency):
```
d = beta_Z1 · c_Z1 + beta_Z2 · c_Z2
  + sigma_Z1 · g_Z1 + sigma_Z2 · g_Z2
  + iota_Z1 · id_Z1 + iota_Z2 · id_Z2
  + iota_g_Z1 · id_g_Z1 + iota_g_Z2 · id_g_Z2
```

Each `beta_Z1` is a 2K vector (rows = 2K sectors, representing Z1 consumption of both Z1 and Z2 goods).

## Naming Convention for Allocation Shares

`Z{area}_beta_Z{consuming_region}-{sector}`:
- `Z1_beta_Z1-k` = Z1 household consumption allocated to sector k of Z1 (domestic)
- `Z2_beta_Z1-k` = Z2 household consumption allocated to sector k of Z1 (cross-border)
- `Z1_beta_Z2-k` = Z1 household consumption allocated to sector k of Z2

## Key Variables

### Aggregate (per region)
| Label | Description |
|-------|-------------|
| `Z1_c` | Household consumption |
| `Z1_g` | Government spending |
| `Z1_id` | Firm investment |
| `Z1_id_g` | Public investment |
| `Z1_va` | Value added |
| `Z1_go` | Gross output |
| `Z1_n` | Total employment |
| `Z1_nf` | Female employment |
| `Z1_shw` | Wage share |
| `Z1_shp` | Profit share |
| `Z1_gdef` | Government deficit |
| `Z1_cab` | Current account balance |
| `Z1_tb` | Trade balance |
| `Z1_mat` | Material use |
| `Z1_emis` | CO2 emissions |
| `Z1_land` | Land use |
| `Z1_water` | Water use |
| `Z1_wa` | Waste |
| `Z1_rec` | Recycling |
| `Z1_x_mat` | Extracted materials |

### Distributional
| Label | Description |
|-------|-------------|
| `Z1_ydw_j-k` | Worker disposable income sector k |
| `Z1_ydc_j-k` | Capitalist disposable income sector k |
| `lh` | Household loans |
| `lf` | Firm loans |
| `v` | Household net wealth |
| `k` | Capital stock |
| `b_s` | Government bills (debt) |

## CE Scenario Mechanism

### Final demand shock (domain = `beta`, `sigma`, `iota`, `iota_g`)
At t = t_shock, the allocation share of the **from** sector is reduced:
```
delta_eff[from] = (1 - ρ) · delta_current[from]
delta_eff[to]   = delta_current[to] + ρ · delta_current[from]
```
Applied to BOTH domestic (Z1_*_Z1) and cross-border (Z2_*_Z1) blocs.

### Production shock (domain = `a`)
At t = t_shock, the A-matrix technical coefficient is modified:
```
A[from, j] = (1 - ρ) · A[from, j]   for all using sectors j
A[to,   j] = A[to, j] + ρ · A[from, j]
```
Applied economy-wide (all 108 sectors as users).

## Analytical ΔM Formula (First-Order)

**Final-demand shock:**
- ΔM₁ = ρ · δ₁ · D · (l₁₂ − l₁₁)
- ΔM₂ = ρ · δ₁ · D · (l₂₂ − l₂₁)
- ΔM  = ρ · δ₁ · D · [(l₁₂+l₂₂) − (l₁₁+l₂₁)]

**Intermediate-demand shock:**
- ΔM₁ ≈ ρ · a₁ⱼ · xⱼ · (l₁₂ − l₁₁)
- ΔM₂ ≈ ρ · a₁ⱼ · xⱼ · (l₂₂ − l₂₁)

Structural rebound if l₁₂ > l₁₁ (secondary sector has higher primary material requirements than primary sector itself — this occurs for Metal, Sc. 10).

## Key Files

- `model/MVP_model_2026.R` — main SFC equations (~870 lines)
- `model/bootstrap_2026.R` — loads all utilities + model
- `model/demand_scenarios_2026.R` — final-demand shock logic
- `model/production_scenarios_2026.R` — A-matrix shock logic
- `utils/run_utils.R` — `run_or_load_shock()`, `shock.summary()`
