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
- 100-period simulation; CE shock at t* = 70, intensity ρ = 0.2

## Core Scenarios

13 circular economy scenarios (Scenario 14 / Construction is excluded — misspecified):
- **Scenarios 1–6:** Final demand reallocation (household and government consumption/investment shifts away from primary to secondary material/energy sectors)
- **Scenarios 7–13:** Intermediate production restructuring (A-matrix: reducing primary material inputs, increasing secondary inputs across all using sectors)

## Three Regime Taxonomy (Key Finding)

1. **Final demand shifts (1–6):** Low leverage. Small effects everywhere. Exceptions: Scenario 1 (Food/Meat shift) has a larger employment effect due to the large consumption share and high labour intensity of the Meat sector.
2. **Intermediate production shifts (7–13):** Strong ecological gains, contractionary, distributionally regressive. EU–RoW spillovers non-trivial and asymmetric.
3. **Metal shift (Sc. 10):** Anomalous — expansionary and ecologically mixed (material savings but land/emissions increase via secondary metal sector's higher intensity in those dimensions).

## Core Structural Trade-Off

The Leontief structural factor (l_{r1,r2} − l_{r1,r1}) ≈ −1 that drives ecological improvement is the same factor that drives gross output reduction. They are structurally inseparable under the current technology matrix.

**No scenario in 1–13 resolves the trilemma without complementary stabilisation policy.**
