#!/usr/bin/env python3
"""Compare a Mizoguchi run against the committed baseline record.

Runs Mizo_theta.py at each baseline time point and reports the delta, so a
stage of the reformulation can be accepted or rejected from one command.
"""
import argparse
import json
import subprocess
import sys
import tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent

# (json path, label, tolerance, higher_is_better)
METRICS = [
    (("profile_metrics", "rmse"), "RMSE", 0.002, False),
    (("skill_vs_no_redistribution",), "Skill", 0.03, True),
    (("profile_metrics", "bias"), "bias", 0.005, None),
    (("profile_metrics", "max_abs_error"), "max|err|", 0.01, False),
    (("zero_crossing", "difference_m"), "front err [m]", 0.002, None),
    (("redistribution", "upper_gain_fraction_of_experiment"), "upper gain / exp", 0.15, None),
    (("solver", "nonlinear_rejection_fraction"), "NL reject frac", 0.05, False),
]


def dig(d, path):
    for k in path:
        if not isinstance(d, dict) or k not in d:
            return None
        d = d[k]
    return d


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--vtu_dir", type=Path, required=True)
    ap.add_argument("--baseline", type=Path, default=HERE / "mizo_baseline.json")
    ap.add_argument("--times", type=str, default="12,24,50")
    ap.add_argument("--label", type=str, default="run")
    args = ap.parse_args()

    baseline = json.loads(args.baseline.read_text())
    times = [t.strip() for t in args.times.split(",") if t.strip()]

    worst = 0.0
    failures = []
    for t in times:
        key = f"{t}h"
        base = baseline.get("times", {}).get(key)
        if base is None:
            print(f"[skip] no baseline for {key}")
            continue
        with tempfile.NamedTemporaryFile(suffix=".json", delete=False) as tmp:
            out = Path(tmp.name)
        cmd = [sys.executable, str(HERE / "Mizo_theta.py"),
               "--vtu_dir", str(args.vtu_dir), "--time", t,
               "--json_output", str(out)]
        r = subprocess.run(cmd, capture_output=True, text=True)
        if r.returncode != 0:
            print(f"[FAIL] Mizo_theta.py failed at {key}\n{r.stdout[-2000:]}\n{r.stderr[-2000:]}")
            failures.append(f"{key}: evaluation failed")
            continue
        cur = json.loads(out.read_text())
        out.unlink(missing_ok=True)

        print(f"\n=== t = {key}  ({args.label} vs baseline) ===")
        print(f"{'metric':22s} {'baseline':>12s} {'run':>12s} {'delta':>12s}  tol")
        for path, label, tol, higher_better in METRICS:
            b, c = dig(base, path), dig(cur, path)
            if b is None or c is None:
                continue
            d = c - b
            flag = ""
            if abs(d) > tol:
                # a move in the good direction is reported, not failed
                good = (higher_better is True and d > 0) or (higher_better is False and d < 0)
                flag = "  better" if good else "  *** OVER TOL"
                if not good:
                    failures.append(f"{key} {label}: {b:.6g} -> {c:.6g} (tol {tol})")
            worst = max(worst, abs(d) / tol if tol else 0.0)
            print(f"{label:22s} {b:12.6g} {c:12.6g} {d:+12.6g}  {tol:g}{flag}")

    print("\n" + "=" * 60)
    if failures:
        print("REGRESSION:")
        for f in failures:
            print("  -", f)
        return 1
    print("OK: every metric within tolerance of the baseline")
    return 0


if __name__ == "__main__":
    sys.exit(main())
