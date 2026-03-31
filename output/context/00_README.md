# LEEDS_MODEL — Context Index

This folder contains structured context files for the LEEDS_MODEL project, intended for reading by Claude (desktop app via MCP or other tools).

## Files

| File | Contents |
|------|----------|
| `01_project_overview.md` | What the project is, goals, author, submission target |
| `02_model_structure.md` | MRIO-SFC model mechanics, variables, regions, equations |
| `03_scenarios.md` | All 14 scenarios, definitions, domain, ρ values |
| `04_simulation_results.md` | Full numerical results for all 14 shocks (% deviation from baseline) |
| `05_key_findings.md` | Synthesised findings: trilemma, regimes, EU–RoW asymmetries, cross-border patterns |
| `06_file_map.md` | Directory map, entry points, key files |
| `07_pending_work.md` | Open tasks and manuscript status |

## Quick Summary

Two-region (EU = Z1, Rest of World = Z2) MRIO-SFC model of circular economy transitions. **14 CE scenarios** applied at ρ=0.2 (ρ=0.05 for Scenarios 5 and 14, Construction), t*=70 in a 100-period simulation.

**Two transition regimes** (not three; FD scenarios are NOT uniformly low-leverage):

1. **Final demand shifts (1–6):** Sc1 (Food) and Sc5 (Construction) have substantive effects; Sc2 (Energy) has strongest ecological gains in this group. Sc3, Sc6 near-zero.
2. **Intermediate production shifts (7–14):** Strong ecological gains (except Construction Sc14 which shows emission *increase*), contractionary, distributionally regressive. EU–RoW asymmetries pronounced.

**Construction emission rebound** (Sc5 and Sc14): Both scenarios show large emission *increases* (+5.19% and +2.15%) despite material savings — secondary construction sector is more emission-intensive than primary in current calibration.

**Four cross-border transmission patterns identified** via MRIO structure: (1) symmetric contraction, (2) asymmetric sign reversal (Sc10 Metal), (3) fiscal dividend in energy scenarios, (4) CAB/TB driven by fossil import substitution.

Core tension: ecological improvement ↔ macro-social contraction (except Construction, which is expansionary but ecologically harmful).

## Session State (2026-03-27)

- All 14 shocks computed and cached in `output/scenarios/shock_runs/`
- `scenario_analysis.qmd` updated: all 14 Discussion sections with ρ=0.2/0.05 numbers; new Cross-Border Transmission section; revised Two Transition Regimes section
- `discussion.qmd` created: standalone prose-only version for quick reading
- Comparison plots split into: FD/Int × EU/RoW × by-term/avg-term = 9 PDFs in `output/scenarios/comparison_plots/`
- User has comments to share next session
