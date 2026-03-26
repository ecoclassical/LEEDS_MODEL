# Simulation Results

All values are **% deviation from baseline** at three horizons:
- **Immediate** = t* + 1
- **Short Term** = t* + 7
- **Long Term** = t* + 20

Columns: `Immediate.Z1 / Short.Z1 / Long.Z1 | Immediate.Z2 / Short.Z2 / Long.Z2`

---

## Scenario 1 | Household Food Shift (Meat → Other Food, β 7→8)

| Category | Variable | Imm.Z1 | ST.Z1 | LT.Z1 | Imm.Z2 | ST.Z2 | LT.Z2 |
|----------|----------|--------|-------|-------|--------|-------|-------|
| Macro | c | -0.02 | -0.02 | -0.02 | 0 | 0 | +0.01 |
| Macro | va | -0.03 | -0.03 | -0.03 | 0 | +0.01 | +0.01 |
| Macro | go | -0.05 | -0.05 | -0.05 | 0 | 0 | +0.01 |
| Macro | id | -0.03 | -0.04 | -0.05 | 0 | 0 | +0.01 |
| Macro | gdef | -3.01 | -2.85 | -2.58 | -1.06 | -1.61 | -2.22 |
| Macro | cab | -0.61 | -0.71 | -0.88 | -0.61 | -0.71 | -0.88 |
| Macro | tb | -0.36 | -0.34 | -0.34 | -0.36 | -0.34 | -0.34 |
| Social | n | **-0.11** | **-0.11** | **-0.11** | 0 | +0.01 | +0.01 |
| Social | nf | -0.11 | -0.11 | -0.11 | 0 | +0.01 | +0.01 |
| Social | shp | +0.04 | +0.05 | +0.06 | 0 | 0 | 0 |
| Social | shw | -0.02 | -0.03 | -0.03 | 0 | 0 | 0 |
| Ecology | x_mat | -0.37 | -0.37 | -0.37 | 0 | +0.01 | +0.01 |
| Ecology | land | -0.52 | -0.52 | -0.52 | 0 | 0 | 0 |
| Ecology | mat | -0.63 | -0.63 | -0.63 | +0.01 | +0.01 | +0.01 |
| Ecology | emis | -0.52 | -0.52 | -0.52 | 0 | 0 | 0 |
| Ecology | water | -0.52 | -0.52 | -0.52 | 0 | 0 | 0 |

**Note:** Employment effect is large (−0.11%) relative to other final demand scenarios. Driven by (a) large beta share of Meat (3.84%) and (b) high employment intensity of Meat vs Other Food (n/x = 5.37 vs 4.35). Also notable: meaningful ecological savings (mat −0.63%, land −0.52%).

---

## Scenario 2 | Household Energy Shift (Fossil → Renewable, β 31→32)

| Category | Variable | Imm.Z1 | ST.Z1 | LT.Z1 | Imm.Z2 | ST.Z2 | LT.Z2 |
|----------|----------|--------|-------|-------|--------|-------|-------|
| Macro | c | 0 | +0.02 | +0.04 | -0.01 | -0.01 | -0.01 |
| Macro | go | -0.02 | -0.01 | 0 | -0.01 | -0.01 | -0.01 |
| Macro | gdef | +0.16 | +1.23 | +2.54 | +1.84 | +2.57 | +3.17 |
| Macro | cab | +1.02 | +1.08 | +1.27 | +1.02 | +1.08 | +1.27 |
| Macro | tb | +0.82 | +0.76 | +0.66 | +0.82 | +0.76 | +0.66 |
| Social | n | -0.02 | -0.01 | 0 | -0.01 | -0.01 | -0.01 |
| Social | shp | +0.07 | +0.08 | +0.09 | 0 | 0 | 0 |
| Ecology | x_mat | -0.81 | -0.81 | -0.80 | -0.03 | -0.03 | -0.03 |
| Ecology | emis | -1.00 | -0.99 | -0.98 | -0.01 | -0.02 | -0.02 |
| Ecology | rec | -1.96 | -1.95 | -1.94 | -0.07 | -0.07 | -0.07 |

**Note:** Strongest ecological effect among final demand scenarios (emis −1%). Positive current account effect (energy import substitution). Slight employment improvement over time.

---

## Scenario 3 | Government Energy Shift (σ 24→25) — **Near-zero effect**

All variables ≈ 0. Target sector has negligible share in government consumption allocation vector.

---

## Scenario 4 | Government Cement Shift (σ 17→18)

| Category | Variable | Imm.Z1 | ST.Z1 | LT.Z1 | Imm.Z2 | ST.Z2 | LT.Z2 |
|----------|----------|--------|-------|-------|--------|-------|-------|
| Macro | go | -0.02 | -0.02 | -0.02 | +0.01 | +0.01 | +0.01 |
| Social | n | -0.02 | -0.02 | -0.02 | 0 | +0.01 | +0.01 |
| Ecology | mat | +0.02 | +0.02 | +0.02 | +0.01 | +0.01 | +0.01 |
| Ecology | emis | -0.05 | -0.05 | -0.06 | +0.02 | +0.03 | +0.03 |
| Ecology | rec | -0.15 | -0.15 | -0.15 | -0.02 | -0.02 | -0.02 |

**Note:** Mixed ecological signal — emissions down in Z1 but slight increase in Z2 (leakage via land/emis).

---

## Scenarios 5–6 | Firm Investment Shifts — **Near-zero effect**

Both ≈ 0 across all variables. Investment allocation shifts have negligible effect at ρ = 0.2 given small absolute sector shares.

---

## Scenario 7 | Wood Shift (A-matrix 11→12)

| Category | Variable | Imm.Z1 | ST.Z1 | LT.Z1 | Imm.Z2 | ST.Z2 | LT.Z2 |
|----------|----------|--------|-------|-------|--------|-------|-------|
| Macro | go | 0 | -0.01 | -0.01 | 0 | 0 | 0 |
| Social | n | 0 | -0.02 | -0.02 | 0 | 0 | 0 |
| Ecology | x_mat | -0.04 | -0.13 | -0.19 | 0 | 0 | 0 |
| Ecology | mat | -0.07 | -0.20 | -0.29 | 0 | 0 | 0 |

---

## Scenario 8 | Pulp Shift (A-matrix 13→14) — **Small effect**

All variables small (≤ 0.04% long-term). Wood/pulp sectors have small A-matrix share.

---

## Scenario 9 | Plastics Shift (A-matrix 17→18)

| Category | Variable | Imm.Z1 | ST.Z1 | LT.Z1 | Imm.Z2 | ST.Z2 | LT.Z2 |
|----------|----------|--------|-------|-------|--------|-------|-------|
| Macro | go | 0 | -0.01 | -0.02 | 0 | 0 | +0.01 |
| Social | n | 0 | -0.01 | -0.02 | 0 | 0 | +0.01 |
| Social | shp | 0 | +0.01 | +0.02 | 0 | 0 | 0 |
| Ecology | rec | -0.03 | -0.08 | -0.12 | 0 | -0.01 | -0.01 |
| Ecology | mat | 0 | +0.01 | +0.02 | 0 | +0.01 | +0.01 |

**Note:** Small mat *increase* (secondary plastics sector has higher material throughput than primary).

---

## Scenario 10 | Metal Shift (A-matrix 26→27) — **Anomalous: expansionary + mixed ecology**

| Category | Variable | Imm.Z1 | ST.Z1 | LT.Z1 | Imm.Z2 | ST.Z2 | LT.Z2 |
|----------|----------|--------|-------|-------|--------|-------|-------|
| Macro | c | +0.01 | +0.02 | +0.05 | 0 | -0.01 | -0.02 |
| Macro | va | +0.01 | +0.04 | +0.06 | 0 | -0.01 | -0.02 |
| Macro | go | +0.01 | +0.02 | +0.04 | 0 | -0.01 | -0.02 |
| Macro | cab | +0.36 | +1.09 | +1.79 | same | same | same |
| Social | n | +0.01 | +0.02 | +0.03 | 0 | -0.01 | -0.02 |
| Social | shp | +0.02 | +0.04 | +0.06 | 0 | 0 | 0 |
| Ecology | x_mat | -0.16 | -0.45 | -0.65 | -0.03 | -0.09 | -0.13 |
| Ecology | mat | -0.23 | -0.65 | -0.94 | -0.04 | -0.12 | -0.18 |
| Ecology | land | +0.08 | +0.22 | +0.34 | 0 | -0.01 | -0.02 |
| Ecology | emis | +0.08 | +0.22 | +0.34 | 0 | -0.01 | -0.02 |
| Ecology | rec | -0.05 | -0.15 | -0.21 | -0.01 | -0.02 | -0.03 |

**Note:** Secondary metal sector has lower material and extraction requirements than primary — driving the expansion. But secondary metal is more land and emissions intensive (mining, processing), so land/emis increase. Z2 faces employment/income contraction (asymmetric burden). This is a structural rebound case (l₁₂ > l₁₁ for some dimensions).

---

## Scenario 11 | Glass Shift (A-matrix 21→22) — **Negligible effect**

All macro/social ≈ 0. Small mat reduction (−0.21% LT). Glass sector has small A-matrix share.

---

## Scenario 12 | Cement Shift (A-matrix 24→25) — **Negligible effect**

All macro/social ≈ 0. Small material reduction (−0.05% LT). Cement sector small relative to economy.

---

## Scenario 13 | Energy Shift (A-matrix 31→32)

| Category | Variable | Imm.Z1 | ST.Z1 | LT.Z1 | Imm.Z2 | ST.Z2 | LT.Z2 |
|----------|----------|--------|-------|-------|--------|-------|-------|
| Macro | c | 0 | +0.01 | +0.06 | 0 | -0.01 | -0.02 |
| Macro | go | -0.01 | -0.02 | -0.01 | 0 | -0.01 | -0.02 |
| Macro | gdef | +0.09 | +0.98 | +3.63 | +0.78 | +2.97 | +5.28 |
| Macro | cab | +0.48 | +1.40 | +2.26 | same | same | same |
| Macro | tb | +0.40 | +1.07 | +1.39 | same | same | same |
| Social | n | -0.01 | -0.02 | -0.01 | 0 | -0.01 | -0.02 |
| Social | shp | +0.03 | +0.10 | +0.18 | 0 | 0 | -0.01 |
| Social | shw | -0.02 | -0.06 | -0.10 | 0 | 0 | 0 |
| Ecology | x_mat | -0.39 | -1.10 | -1.59 | -0.01 | -0.04 | -0.06 |
| Ecology | emis | -0.47 | -1.35 | -1.95 | -0.01 | -0.02 | -0.03 |
| Ecology | land | -0.47 | -1.35 | -1.95 | -0.01 | -0.02 | -0.03 |
| Ecology | rec | -0.93 | -2.66 | -3.87 | -0.03 | -0.10 | -0.15 |
| Ecology | water | -0.47 | -1.35 | -1.95 | -0.01 | -0.02 | -0.03 |

**Note:** Strongest ecological gains (emis −1.95% LT). Positive current account (import energy substitution). Profit share rises (wage share falls) — distributionally regressive. Z2 faces employment and income decline (asymmetric burden: EU gains ecological improvement partly by reducing energy imports from RoW). Government balance improves markedly (reduced energy subsidy/tax base shift).
