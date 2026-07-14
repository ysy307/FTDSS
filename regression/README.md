# FTCMS regression baseline (WP0)

Golden-reference regression setup for verifying that source changes keep
results bit-identical.

## Case

`project/Mizo-xz-Convex-regression` — a copy of `project/Mizo-xz-Convex`
with the simulated end time shortened from 50 h to 0.75 h (2700 s)
(`Input/Conditions.json`, `simulation_period.end`). Mesh, materials,
solver, and output settings are unchanged. Single rank
(`Mizoguchi_unsaturated_v_v2_0.vtu`).

The 0.75 h window covers the smooth pre-freezing phase (dt 100-300 s)
and the onset of freezing at t ~ 2650 s where the adaptive time step
collapses to 5-25 s with 7-14 nonlinear iterations per step. Reference
run: 22 step attempts (21 accepted, 1 retry), 124 nonlinear iterations,
10 field outputs (300 s interval), ~7.5 min wall on 24 threads.

## Build

```bash
python /workspaces/FTCMS/scripts/compile/compile.py -c intel -t main
cmake --build /workspaces/FTCMS/CMakeBuild/intel --target FTCMS --parallel
```

## Run the regression case

```bash
cd /workspaces/FTCMS
FTCMS_PROJECT_PATH=/workspaces/FTCMS/project/Mizo-xz-Convex-regression \
  mpirun -n 1 ./bin/FTCMS >| /tmp/regression-stdout.log 2>&1
```

Outputs land in `project/Mizo-xz-Convex-regression/Output/`.
Wall time is roughly 5–10 minutes; use tmux per AGENTS.md.

## Compare against golden

```bash
python /workspaces/FTCMS/regression/compare_outputs.py \
  /workspaces/FTCMS/regression/wp0-golden/Output \
  /workspaces/FTCMS/project/Mizo-xz-Convex-regression/Output \
  --log-a /workspaces/FTCMS/regression/wp0-golden/run-stdout.log \
  --log-b /tmp/regression-stdout.log
```

Exit code 0 iff all files are bit-identical (run.log is skipped because it
contains wall-clock timestamps; the profiler table lives there). For VTU
files that differ at byte level the script reports per-DataArray max
absolute differences. Requires Python 3 with numpy.

## Golden contents

- `wp0-golden/Output/` — full Output tree of the reference run
- `wp0-golden/run-stdout.log` — stdout of the reference run
- `wp0-golden/profiler.md` — Time Profiler Results table of the reference run
