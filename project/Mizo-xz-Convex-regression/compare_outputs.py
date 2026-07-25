#!/usr/bin/env python3
"""Compare two FTCMS solver_history.log files for the outer-coupling regression case.

Usage:
    python compare_outputs.py <golden_log> <candidate_log>
    python compare_outputs.py <golden_log> <candidate_log> --summary-only

Purpose: quick, minimal regression check for the WP0-WP4 outer-coupling redesign
(see /home/devuser/.claude/plans/spicy-sauteeing-scroll.md). Two things it answers:

1. Bit-identical check (expected for additive/plumbing WPs such as WP1): the two
   files are compared byte-for-byte first (sha256). If identical, nothing else
   to do - print PASS and exit 0.
2. Trend comparison (expected for behavior-changing WPs such as WP2/WP3b): if the
   files differ, parse both by column name (read from the "# attempt ..." header
   line, so this keeps working if a later WP adds columns) and print, per
   time-step attempt, the columns that actually differ - focused on the ones
   that matter for judging the outer-loop fix: dt_used_s, accepted, status,
   outer_iter, nl_work, phase_converged.

This intentionally does not attempt VTU field comparison (unlike the older
setup/assemble campaign's tool at the now-removed regression/compare_outputs.py) -
solver_history.log already encodes per-step diagnostics (dq_norm_W, lte_rel,
phase_dqi_max, phase_eq_Pa, ...) derived from the full field state each
attempt, so a byte-identical log across an entire run is already strong
evidence of a bit-identical field trajectory. Add a VTU checksum comparison
here only if a future WP needs to distinguish "logs match by coincidence"
from "fields truly match" - not needed for WP0/WP1.
"""

import argparse
import hashlib
import sys
from pathlib import Path

# Columns worth calling out explicitly when logs differ; anything else is
# still shown (all columns are printed for a differing row) but these are
# the ones a human should look at first when judging the outer-loop fix.
HEADLINE_COLUMNS = (
    "dt_used_s", "accepted", "status", "inner_last", "inner_max",
    "outer_iter", "nl_work", "phase_converged",
)


def sha256_of(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def parse_log(path: Path):
    """Return (column_names, list[dict]) parsed from a solver_history.log."""
    columns = None
    rows = []
    with path.open() as f:
        for line in f:
            stripped = line.strip()
            if not stripped:
                continue
            if stripped.startswith("#"):
                # The column-name header is the one comment line that starts
                # with "# attempt" (schema may gain/lose trailing columns).
                if stripped.startswith("# attempt"):
                    columns = stripped.lstrip("#").split()
                continue
            if columns is None:
                raise ValueError(f"{path}: found a data line before the '# attempt ...' header")
            values = stripped.split()
            if len(values) != len(columns):
                raise ValueError(
                    f"{path}: line has {len(values)} fields, header has {len(columns)}: {stripped!r}"
                )
            rows.append(dict(zip(columns, values)))
    if columns is None:
        raise ValueError(f"{path}: no '# attempt ...' header line found")
    return columns, rows


def print_trend_diff(golden_path: Path, candidate_path: Path):
    g_cols, g_rows = parse_log(golden_path)
    c_cols, c_rows = parse_log(candidate_path)

    if g_cols != c_cols:
        print("Column layout differs:")
        print(f"  golden:    {g_cols}")
        print(f"  candidate: {c_cols}")
        print("(falling back to whichever columns are common to both)")

    common_cols = [c for c in g_cols if c in c_cols]
    headline = [c for c in HEADLINE_COLUMNS if c in common_cols]

    n = max(len(g_rows), len(c_rows))
    diff_count = 0
    header = "attempt".ljust(9) + "".join(c.ljust(14) for c in ["golden/cand"] + headline)
    print(header)
    for i in range(n):
        g = g_rows[i] if i < len(g_rows) else None
        c = c_rows[i] if i < len(c_rows) else None
        if g is None or c is None or any(g[k] != c[k] for k in common_cols):
            diff_count += 1
            attempt = (g or c).get("attempt", str(i + 1))
            g_line = "golden".ljust(14) + "".join((g[k] if g else "-").ljust(14) for k in headline)
            c_line = "cand".ljust(14) + "".join((c[k] if c else "-").ljust(14) for k in headline)
            print(attempt.ljust(9) + g_line)
            print("".ljust(9) + c_line)

    print()
    print(f"Total attempts: golden={len(g_rows)}, candidate={len(c_rows)}; differing rows: {diff_count}")


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("golden_log", type=Path)
    parser.add_argument("candidate_log", type=Path)
    parser.add_argument("--summary-only", action="store_true",
                         help="only print the sha256/bit-identical verdict, skip the trend table")
    args = parser.parse_args()

    if not args.golden_log.is_file():
        print(f"error: golden log not found: {args.golden_log}", file=sys.stderr)
        return 2
    if not args.candidate_log.is_file():
        print(f"error: candidate log not found: {args.candidate_log}", file=sys.stderr)
        return 2

    g_hash = sha256_of(args.golden_log)
    c_hash = sha256_of(args.candidate_log)

    print(f"golden:    {args.golden_log}  sha256={g_hash}")
    print(f"candidate: {args.candidate_log}  sha256={c_hash}")

    if g_hash == c_hash:
        print("PASS: bit-identical")
        return 0

    print("DIFFER: not bit-identical")
    if not args.summary_only:
        print()
        print_trend_diff(args.golden_log, args.candidate_log)
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
