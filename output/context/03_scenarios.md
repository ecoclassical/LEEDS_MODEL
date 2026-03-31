# Scenario Definitions

All scenarios use shock activated at t* = 70, 100-period simulation.
ρ = 0.2 for all scenarios **except** Scenario 5 and Scenario 14 (Construction), which use ρ = 0.05.

## Final Demand Scenarios (1–6)

| # | Domain | Channel | Shift | Param | from→to | ρ |
|---|--------|---------|-------|-------|---------|---|
| 1 | Final Demand | Household Consumption | Food: Meat → Other Food | β | 7→8 | 0.2 |
| 2 | Final Demand | Household Consumption | Energy: Fossil → Renewable | β | 31→32 | 0.2 |
| 3 | Final Demand | Household Consumption | Wood: Primary → Secondary | β | 11→12 | 0.2 |
| 4 | Final Demand | Government Consumption | Plastics: Primary → Secondary | σ | 17→18 | 0.2 |
| 5 | Final Demand | Firm Fixed Investment | Construction: Primary → Secondary | ι | 36→37 | 0.05 |
| 6 | Final Demand | Firm Fixed Investment | Metal: Primary → Secondary | ι | 26→27 | 0.2 |

**Notes:**
- Sc3 and Sc6: near-zero effect — target sectors have negligible shares in the corresponding allocation vectors (HH Wood, Firm Metal Investment).
- Sc5 (Construction): ρ=0.05 because secondary construction sector is more emission-intensive; at ρ=0.2 the emission increase is too large. Even at ρ=0.05: emis **+5.19%**, land **+5.19%** despite mat −0.84%. Expansionary overall (c +0.08%, n +0.03%).

## Intermediate Production Scenarios (7–14)

| # | Domain | Input substituted | from→to | ρ |
|---|--------|-------------------|---------|----|
| 7 | Intermediate Demand | Wood: Primary → Secondary | 11→12 | 0.2 |
| 8 | Intermediate Demand | Pulp: Primary → Secondary | 13→14 | 0.2 |
| 9 | Intermediate Demand | Plastics: Primary → Secondary | 17→18 | 0.2 |
| 10 | Intermediate Demand | Metal: Primary → Secondary | 26→27 | 0.2 |
| 11 | Intermediate Demand | Glass: Primary → Secondary | 21→22 | 0.2 |
| 12 | Intermediate Demand | Cement: Primary → Secondary | 24→25 | 0.2 |
| 13 | Intermediate Demand | Energy: Fossil → Renewable | 31→32 | 0.2 |
| 14 | Intermediate Demand | Construction: Primary → Secondary | 36→37 | 0.05 |

**Notes:**
- Sc14 (Construction): ρ=0.05 same reason as Sc5. Even at ρ=0.05: emis **+2.15%**, land **+2.15%** despite mat −0.35%. Slightly expansionary (c +0.03%, n +0.01%).
- Sc10 (Metal): Anomalous — expansionary (go +0.04% LT) but land/emis increase (+0.34% LT). Material savings are large (mat −0.94%).
- Sc13 (Energy): Highest ecological leverage (emis −1.95% LT, rec −3.87% LT) and most asymmetric cross-border burden.

## Key Sector Labels (selected)

| # | Sector |
|---|--------|
| 7 | Meat (primary) |
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
| 36 | Construction (primary) |
| 37 | Construction (secondary/renovation) |
