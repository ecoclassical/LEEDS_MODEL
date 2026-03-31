# Project Overview

## Identity

- **Name:** LEEDS_MODEL
- **Author:** Oriol Vallès Codina (Johns Hopkins University, Net Zero Industrial Policy Lab, Department of Political Science)
- **Companion project:** JUST2CE (Just Transition to Circular Economy)
- **Submission target:** *Ecological Economics* special issue on ecological macroeconomic modelling

## Research Question

How do circular economy (CE) transitions affect the **trilemma between ecological improvement, macroeconomic stability, and social equity**? How do impacts distribute asymmetrically between the EU (Core, Z1) and Rest of World (Periphery, Z2)?

## Model Type

**Multi-Regional Input-Output + Stock-Flow Consistent (MRIO-SFC)** macroeconomic model.

- Two regions: EU (Z1) and Rest of World (Z2)
- 54 production sectors per region (108 total)
- Bilateral final demand allocation matrix Ψ ∈ ℝ^{108×RK} linking consuming-region institutional channels to supplying sectors
- Dynamic SFC accounting: households, firms, government, central bank, commercial banks
- 100-period simulation; CE shock at t* = 70, intensity ρ = 0.2 (ρ = 0.05 for Construction scenarios)

## Core Scenarios

14 circular economy scenarios:
- **Scenarios 1–6:** Final demand reallocation (household and government consumption/investment shifts)
- **Scenarios 7–14:** Intermediate production restructuring (A-matrix: primary-to-secondary input substitution)

## Two Transition Regimes (Revised Taxonomy)

The "three regime" framing (FD = low leverage, Int = strong, Construction = anomalous) is **too coarse**. Revised:

### Regime 1: Final Demand Shifts (Scenarios 1–6)

Effects vary substantially within this group:

- **Scenario 1 (HH Food/Meat):** Significant — employment −0.11%, mat −0.63%, emis −0.52%. Driven by large consumption share of Meat (3.84%) and high employment intensity differential.
- **Scenario 2 (HH Energy):** Best ecological gain in FD group (emis −1.0%), positive current account (+1.27% LT), nearly neutral employment.
- **Scenario 4 (Gov Plastics):** Small effects. Mixed ecological signal (slight Z2 leakage).
- **Scenario 5 (Firm Construction, ρ=0.05):** **Expansionary** (go −0.19% gross output but c +0.08%, n +0.03%) BUT emission *increase* of +5.19% — major rebound. Single-region analysis would miss cross-border impact.
- **Scenarios 3 (HH Wood), 6 (Firm Metal):** Near-zero effects.

### Regime 2: Intermediate Production Shifts (Scenarios 7–14)

Strong ecological gains (except Construction), contractionary, distributionally regressive. EU–RoW spillovers systematic and asymmetric.

- **Scenario 10 (Metal):** Anomalous — expansionary (go +0.04%, n +0.03% LT), material savings large (mat −0.94%), but land/emis *increase* (+0.34%) — structural rebound.
- **Scenario 13 (Energy):** Highest leverage — emis −1.95% LT, but most distributionally regressive and most asymmetric cross-border burden.
- **Scenario 14 (Construction, ρ=0.05):** Expansionary but emission *increase* (+2.15% LT) — same structural rebound as Sc5. Secondary construction is more emission-intensive than primary.

## Core Structural Trade-Off

The Leontief structural factor (l_{r1,r2} − l_{r1,r1}) ≈ −1 that drives ecological improvement is the same factor that drives gross output reduction. Structurally inseparable under current technology (except Construction where secondary sector is more emission-intensive, and Metal where secondary sector has different factor intensities).

**No scenario resolves the trilemma without complementary stabilisation policy.**
