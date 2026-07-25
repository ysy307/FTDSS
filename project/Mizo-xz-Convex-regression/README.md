# Mizo-xz-Convex outer-coupling regression case

Shortened reproduction case for the freezing-onset outer phase-equilibrium loop
work (see plan `/home/devuser/.claude/plans/spicy-sauteeing-scroll.md`, WP0-WP4).
`Input/Conditions.json` here sets `simulation_period.end = 1.0` hour: this reliably
covers the clean pre-onset steps, the freezing-onset step (~t=2400s), and the
worst observed outer-loop blowup steps (t~2700-3600s, outer_iter in the
dozens-to-hundreds range) within a run that finishes in well under an hour of
wall-clock time. `Input/Basic.json` pins `num_threads: 8` (not the 24 the older
setup/assemble campaign used) to keep results deterministic and consistent with
the thread count used throughout this investigation.

This directory reuses the fixture left behind by the earlier setup/assemble
optimization campaign (`docs` for that campaign were removed after it concluded;
see commit `3c3fb0a27`) rather than inventing a new convention. The old
`regression/compare_outputs.py` (VTU-diffing, multi-file) was also removed at
that point; `compare_outputs.py` in this directory is a smaller, purpose-built
replacement — see its docstring for why a VTU diff isn't needed for this work.

## Running the case

```bash
OMP_NUM_THREADS=8 FTCMS_PROJECT_PATH=/workspaces/FTCMS/project/Mizo-xz-Convex-regression \
    mpirun -n 1 --bind-to none /workspaces/FTCMS/bin/FTCMS > <label>-stdout.log 2>&1
```

Run this in `tmux` per AGENTS.md (it takes several minutes). `bin/FTCMS` must be
built via `ninja FTCMS` in `CMakeBuild/intel` — `scripts/compile/compile.py -t main`
only builds `test_main`, not the app binary (confirmed this session).

Save each run's `Output/` directory before the next run overwrites it, e.g.:

```bash
mv Output Output-<label>
mv <label>-stdout.log ./
```

(`Output*/` and `*.log` are gitignored — see `project/*/Output*/` and `*.log`
in the repo `.gitignore` — so these accumulate freely without polluting `git status`.)

## Comparing against golden

```bash
python3 compare_outputs.py Output-golden/solver_history.log Output-<label>/solver_history.log
```

- Additive/plumbing WPs (WP1, WP3a, WP4) must print `PASS: bit-identical`.
- Behavior-changing WPs (WP2, WP3b) are expected to `DIFFER`; the script then
  prints a per-attempt trend table (dt_used_s, accepted, status, outer_iter,
  nl_work, phase_converged, ...) to judge whether the change is the intended
  improvement (same accepted-step/time trajectory, fewer outer iterations at
  onset) rather than a regression.

## Golden provenance

`Output-golden/` was produced from the tree at the point WP0 was built: the
crash-fix-only state (`convergence_control.F90`'s `dq_effective` overflow fix
from the previous round), with none of WP1-WP4 applied yet. Regenerate it only
if a change is intentionally meant to alter results for *all* future WPs (i.e.
essentially never — a new golden invalidates every WP's bit-identical claim
made against the old one).
