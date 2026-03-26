# Scenario Definitions

All scenarios use ρ = 0.2, shock activated at t* = 70, 100-period simulation.
**Scenario 14 (Construction) is excluded — misspecified.**

## Final Demand Scenarios (1–6)

| # | Domain | Channel | Sector shift | Label | from→to |
|---|--------|---------|--------------|-------|---------|
| 1 | Final demand | Household consumption (β) | Food: Meat → Other Food | `beta` | 7→8 |
| 2 | Final demand | Household consumption (β) | Energy: Fossil → Renewable | `beta` | 31→32 |
| 3 | Final demand | Government consumption (σ) | Energy: Fossil → Renewable | `sigma` | 24→25 |
| 4 | Final demand | Government consumption (σ) | Cement: Primary → Secondary | `sigma` | 17→18 |
| 5 | Final demand | Firm investment (ι) | Energy: Fossil → Renewable | `iota` | 24→25 |
| 6 | Final demand | Firm investment (ι) | Plastics: Primary → Secondary | `iota` | 17→18 |

Note: Scenarios 3, 5 show zero effect — the target sectors have negligible shares in the corresponding allocation vector.

## Intermediate Production Scenarios (7–13)

| # | Domain | Input substituted | Sector shift | from→to |
|---|--------|-------------------|--------------|---------|
| 7 | Production (A-matrix) | Wood: Primary → Secondary | 11→12 |
| 8 | Production (A-matrix) | Pulp: Primary → Secondary | 13→14 |
| 9 | Production (A-matrix) | Plastics: Primary → Secondary | 17→18 |
| 10 | Production (A-matrix) | Metal: Primary → Secondary | 26→27 |
| 11 | Production (A-matrix) | Glass: Primary → Secondary | 21→22 |
| 12 | Production (A-matrix) | Cement: Primary → Secondary | 24→25 |
| 13 | Production (A-matrix) | Energy: Fossil → Renewable | 31→32 |

## Key Sector Labels (selected)

| # | Sector |
|---|--------|
| 7 | Meat |
| 8 | Other Food |
| 11 | Wood (primary) |
| 12 | Wood (secondary/recycled) |
| 13 | Pulp (primary) |
| 14 | Pulp (secondary) |
| 17 | Plastics (primary) |
| 18 | Plastics (secondary) |
| 21 | Glass (primary) |
| 22 | Glass (secondary) |
| 24 | Cement (primary) |
| 25 | Cement (secondary) |
| 26 | Metal (primary) |
| 27 | Metal (secondary) |
| 31 | Fossil energy |
| 32 | Renewable energy |
