"""
Extract channel-specific cross-regional final demand from MARIO - Aggregated (2).xlsx.

The MARIO flows sheet structure:
  Rows 3..56   = EU sectors (54, matching LEEDS order)
  Rows 57..110 = RoW sectors (54, matching LEEDS order)
  Col 111 = EU Consumption (household = beta)
  Col 112 = EU Final consumption expenditure by government (= sigma)
  Col 113 = EU GFCF (gross fixed capital formation = iota + iota_g)
  Col 114 = EU Exports

REGIONAL FLEXIBILITY:
  The MARIO file aggregates all non-EU countries into a single RoW block.
  For finer regional splits (Global North vs Global South), EXIOBASE3 is required.
  This script is structured so that the extraction logic can be reused with EXIOBASE3
  by replacing the `load_mario()` function with a `load_exiobase3()` function.

  EXIOBASE3 regions of interest:
    Global North (non-EU): US, CA, AU, NZ, JP, KR, CH, NO, TW, RU
    Global South: all remaining non-EU EXIOBASE3 regions
    EU (EXIOBASE3): AT, BE, BG, CY, CZ, DE, DK, EE, ES, FI, FR, GB, GR,
                    HR, HU, IE, IT, LT, LU, LV, MT, NL, PL, PT, RO, SE, SI, SK

  When EXIOBASE3 is available, replace the data loading section below with:
    import pymrio
    exio3 = pymrio.parse_exiobase3(path="path/to/exiobase3_pxp.zip")
    Y = exio3.Y   # MultiIndex: (region, sector) x (region, demand_category)
    # Then aggregate Y rows by region group (EU / Global North / Global South)
    # before passing to compute_channel_shares().
"""

import openpyxl
import csv

# ── Configuration: which region groups to extract ─────────────────────────────
# With MARIO (2-region): only EU and RoW available.
# With EXIOBASE3: can split RoW into Global_North and Global_South.
REGION_MODE = "mario"   # "mario" | "exiobase3"

EU_EXIO3 = [
    "AT","BE","BG","CY","CZ","DE","DK","EE","ES","FI","FR","GB","GR",
    "HR","HU","IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI","SK"
]
GLOBAL_NORTH_EXIO3 = ["US","CA","AU","NZ","JP","KR","CH","NO","TW","RU"]
# Global South = all EXIOBASE3 regions not in EU_EXIO3 or GLOBAL_NORTH_EXIO3


def load_mario(path="data/impacts/MARIO - Aggregated (2).xlsx"):
    """
    Load MARIO flows sheet and return F11, F21 for EU final demand channels.
    Returns dict with keys: sector_names, F_dom (EU->EU FD), F_imp (RoW->EU FD)
    Each F_* dict has keys: hh, gov, gfcf.
    """
    wb = openpyxl.load_workbook(path, read_only=True, data_only=True)
    ws = wb["flows"]
    rows = list(ws.iter_rows(values_only=True))

    EU_ROW_START  = 3;  EU_ROW_END   = 56   # 0-based row indices
    ROW_ROW_START = 57; ROW_ROW_END  = 110
    COL_HH   = 111;  COL_GOV = 112;  COL_GFCF = 113

    def extract(row_start, row_end, col):
        return [float(rows[i][col] or 0) for i in range(row_start, row_end + 1)]

    return {
        "sector_names" : [rows[i][2] for i in range(EU_ROW_START, EU_ROW_END + 1)],
        "region_groups": ["EU", "RoW"],
        "F_dom"  : {"hh": extract(EU_ROW_START,  EU_ROW_END,  COL_HH),
                    "gov": extract(EU_ROW_START,  EU_ROW_END,  COL_GOV),
                    "gfcf":extract(EU_ROW_START,  EU_ROW_END,  COL_GFCF)},
        # F21: RoW supply -> EU final demand (cross-regional imports)
        "F_imp_RoW": {"hh": extract(ROW_ROW_START, ROW_ROW_END, COL_HH),
                      "gov": extract(ROW_ROW_START, ROW_ROW_END, COL_GOV),
                      "gfcf":extract(ROW_ROW_START, ROW_ROW_END, COL_GFCF)},
    }


def load_exiobase3(path, target_eu_regions=None, global_north_regions=None):
    """
    Placeholder: load EXIOBASE3 and return same structure as load_mario(),
    but with additional F_imp_GlobalNorth and F_imp_GlobalSouth blocks.

    Usage (when EXIOBASE3 is available):
        import pymrio
        exio3 = pymrio.parse_exiobase3(path=path)
        Y = exio3.Y  # (region, sector) x (region, demand_category)

        eu_regs = target_eu_regions or EU_EXIO3
        gn_regs = global_north_regions or GLOBAL_NORTH_EXIO3
        gs_regs = [r for r in exio3.get_regions() if r not in eu_regs + gn_regs]

        # Aggregate Y by region group, then compute F11/F21/F_GN/F_GS as below.
        # Demand category labels in EXIOBASE3:
        #   "Final consumption expenditure by households"
        #   "Final consumption expenditure by government"
        #   "Gross fixed capital formation"
    """
    raise NotImplementedError(
        "EXIOBASE3 extraction not yet implemented. "
        "Download EXIOBASE3 via: import pymrio; pymrio.download_exiobase3("
        "storage_folder='data/exiobase3', system='pxp', years=[2011])"
    )


def compute_channel_shares(sector_names, F_dom, F_imp_dict):
    """
    Given domestic (F_dom) and import (F_imp_dict keyed by region group) flows,
    compute per-sector import propensities for each channel and region group.

    F_imp_dict: {"RoW": {"hh":[], "gov":[], "gfcf":[]},
                 "GlobalNorth": {...}, "GlobalSouth": {...}}  # last two optional
    """
    n = len(sector_names)
    results = []

    for j in range(n):
        row = {"sector_j": j + 1, "mario_name": sector_names[j]}

        # Domestic flows
        for ch in ("hh", "gov", "gfcf"):
            row[f"F11_{ch}"] = round(F_dom[ch][j], 4)

        # Import flows and propensities per region group
        for group, F_imp in F_imp_dict.items():
            for ch in ("hh", "gov", "gfcf"):
                row[f"F21_{group}_{ch}"] = round(F_imp[ch][j], 4)

            # eta_K[j] = F21[j,K] / (F11[j,K] + F21[j,K])
            for ch in ("hh", "gov", "gfcf"):
                total = F_dom[ch][j] + F_imp[ch][j]
                eta   = F_imp[ch][j] / total if total > 0 else 0
                row[f"eta_{group}_{ch}"] = round(eta, 5)

            # Per-sector kappa (gov/HH) and lambda (GFCF/HH) scalars
            eta_hh = row[f"eta_{group}_hh"]
            row[f"kappa_{group}"]  = round(row[f"eta_{group}_gov"]  / eta_hh, 4) if eta_hh > 0 else 0
            row[f"lambda_{group}"] = round(row[f"eta_{group}_gfcf"] / eta_hh, 4) if eta_hh > 0 else 0

        results.append(row)
    return results


def print_summary(results, group="RoW"):
    """Print aggregate import shares and implied kappa/lambda for a region group."""
    import_hh   = sum(r[f"F21_{group}_hh"]   for r in results)
    import_gov  = sum(r[f"F21_{group}_gov"]   for r in results)
    import_gfcf = sum(r[f"F21_{group}_gfcf"]  for r in results)
    dom_hh      = sum(r["F11_hh"]   for r in results)
    dom_gov     = sum(r["F11_gov"]  for r in results)
    dom_gfcf    = sum(r["F11_gfcf"] for r in results)

    agg_eta_hh   = import_hh   / (dom_hh   + import_hh)   if (dom_hh   + import_hh)   > 0 else 0
    agg_eta_gov  = import_gov  / (dom_gov  + import_gov)  if (dom_gov  + import_gov)  > 0 else 0
    agg_eta_gfcf = import_gfcf / (dom_gfcf + import_gfcf) if (dom_gfcf + import_gfcf) > 0 else 0

    print(f"\n=== Aggregate import shares from {group} ===")
    print(f"  HH   import share: {agg_eta_hh*100:.2f}%")
    print(f"  Gov  import share: {agg_eta_gov*100:.2f}%")
    print(f"  GFCF import share: {agg_eta_gfcf*100:.2f}%")
    print(f"  Implied kappa_{group}  (gov/HH)  = {agg_eta_gov/agg_eta_hh:.3f}  (prior=0.50)")
    print(f"  Implied lambda_{group} (GFCF/HH) = {agg_eta_gfcf/agg_eta_hh:.3f}  (prior=1.20)")

    print(f"\n=== Top 10 sectors by HH import share from {group} ===")
    top = sorted(results, key=lambda x: x[f"eta_{group}_hh"], reverse=True)[:10]
    hdr = f"{'j':>3}  {'sector':<50} {'eta_hh':>7} {'eta_gov':>8} {'eta_gfcf':>9} {'kappa':>7} {'lambda':>8}"
    print(hdr)
    for r in top:
        print(f"{r['sector_j']:>3}  {r['mario_name']:<50} "
              f"{r[f'eta_{group}_hh']:>7.4f} {r[f'eta_{group}_gov']:>8.4f} "
              f"{r[f'eta_{group}_gfcf']:>9.4f} {r[f'kappa_{group}']:>7.3f} {r[f'lambda_{group}']:>8.3f}")


# ── Main ───────────────────────────────────────────────────────────────────────
if REGION_MODE == "mario":
    data = load_mario()
    F_imp_dict = {"RoW": data["F_imp_RoW"]}
    results = compute_channel_shares(data["sector_names"], data["F_dom"], F_imp_dict)
    print_summary(results, group="RoW")

elif REGION_MODE == "exiobase3":
    data = load_exiobase3("data/exiobase3/IOT_2011_pxp.zip")
    # When implemented, F_imp_dict will have GlobalNorth and GlobalSouth keys
    # results = compute_channel_shares(data["sector_names"], data["F_dom"], F_imp_dict)
    # print_summary(results, group="GlobalNorth")
    # print_summary(results, group="GlobalSouth")

# ── Save CSV ───────────────────────────────────────────────────────────────────
out_path = "data/mrio_fd_channel_shares.csv"
with open(out_path, "w", newline="") as f:
    writer = csv.DictWriter(f, fieldnames=list(results[0].keys()))
    writer.writeheader()
    writer.writerows(results)

print(f"\nSaved: {out_path}")
