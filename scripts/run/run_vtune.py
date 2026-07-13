#!/usr/bin/env python3
import os
import sys
import glob
import csv
import argparse
import shutil
import subprocess

def resolve_tool(name, preferred_globs):
    """Resolve an external tool, preferring explicit install paths over $PATH.
    The default container $PATH may put an incompatible launcher first
    (e.g. an OpenMPI mpirun that rejects the Intel-MPI '-genv' flag), so the
    Intel oneAPI locations are tried before falling back to $PATH."""
    for pattern in preferred_globs:
        matches = sorted(glob.glob(pattern))
        if matches:
            return matches[-1]
    found = shutil.which(name)
    return found if found else name

def resolve_binary(binary):
    """Accept a bare test name (e.g. 'field', 'test_field') or an explicit path
    and return an executable path under ./bin when not given as a path."""
    if "/" in binary:
        return binary
    name = binary if binary.startswith("test_") else f"test_{binary}"
    return os.path.join("./bin", name)

def parse_threads(spec):
    """Parse a comma-separated thread-count list like '1,4,8' into [1,4,8]."""
    out = []
    for tok in spec.split(","):
        tok = tok.strip()
        if tok:
            out.append(int(tok))
    return out or [1]

def read_summary_metrics(top_csv):
    """Extract a few headline metrics from the VTune summary CSV for a quick
    reasonableness check (Elapsed / CPU / Spin time)."""
    wanted = ("Elapsed Time", "CPU Time", "Spin Time")
    metrics = {}
    if os.path.isfile(top_csv) and os.path.getsize(top_csv) > 0:
        with open(top_csv, "r", encoding="utf-8") as f:
            # VTune summary CSV is tab-delimited: [Hierarchy Level, Metric Name, Metric Value].
            for row in csv.reader(f, delimiter="\t"):
                if len(row) >= 3 and row[1] in wanted and row[1] not in metrics:
                    metrics[row[1]] = row[2]
    return metrics

def profile_once(target_bin, bin_name, mpirun, vtune, nprocs, threads,
                 project, workspace_vtune_dir, local_vtune_dir, tmp_dir,
                 data_limit_mb, duration_s):
    """Run one VTune hotspots collection at a fixed OpenMP thread count and
    emit CSV + Markdown reports. Returns headline metrics for the run."""
    # Per-(binary, thread-count) result prefix to avoid collisions.
    next_idx = 0
    while True:
        result_base = f"{bin_name}_t{threads:02d}_r{next_idx:03d}hs"
        if not glob.glob(os.path.join(workspace_vtune_dir, f"{result_base}*")):
            break
        next_idx += 1

    result_dir = os.path.join(local_vtune_dir, result_base)
    print(f"\n=== {bin_name}: OMP_NUM_THREADS={threads}, MPI procs={nprocs} ===")
    print(f"Target Result Directory (Local): {result_dir}")

    env = os.environ.copy()
    env["FTCMS_PROJECT_PATH"] = project
    env["TMPDIR"] = tmp_dir
    env["VTUNE_LOG_DIR"] = tmp_dir
    env["OMP_NUM_THREADS"] = str(threads)

    # Resource caps to protect the container from OOM/disk exhaustion:
    #   -data-limit : stop collection once the result reaches this many MB (0 = unlimited).
    #   -d          : stop collection after this many seconds (0 = until the app exits).
    vtune_cmd = [
        vtune, "-collect", "hotspots",
        "-knob", "sampling-mode=sw",
        "-data-limit", str(data_limit_mb),
    ]
    if duration_s > 0:
        vtune_cmd += ["-d", str(duration_s)]
    vtune_cmd += ["-result-dir", result_dir, "--", target_bin]

    # For a single process, run VTune directly (no MPI launcher): the test
    # binaries MPI_Init themselves as a singleton, and avoiding mpirun sidesteps
    # the launcher's abnormal-termination handling.
    if nprocs > 1:
        vtune_cmd = [
            mpirun,
            "-genv", "FTCMS_PROJECT_PATH", project,
            "-genv", "OMP_NUM_THREADS", str(threads),
            "-n", str(nprocs),
        ] + vtune_cmd

    subprocess.run(vtune_cmd, env=env, check=False)

    created = [d for d in glob.glob(os.path.join(local_vtune_dir, f"{result_base}*")) if os.path.isdir(d)]
    if not created:
        print("Error: VTune result directory was not created.")
        return None

    latest_dir_local = created[0]
    actual_dir_name = os.path.basename(latest_dir_local)

    workspace_out_prefix = os.path.join(workspace_vtune_dir, actual_dir_name)
    report_csv = f"{workspace_out_prefix}_report.csv"
    src_csv = f"{workspace_out_prefix}_source_line.csv"
    top_csv = f"{workspace_out_prefix}_summary.csv"

    export_cmds = [
        [vtune, "-report", "hotspots", "-result-dir", latest_dir_local, "-format", "csv", "-report-output", report_csv],
        [vtune, "-report", "hotspots", "-result-dir", latest_dir_local, "-group-by", "source-line", "-format", "csv", "-report-output", src_csv],
        [vtune, "-report", "summary", "-result-dir", latest_dir_local, "-format", "csv", "-report-output", top_csv],
    ]
    for cmd in export_cmds:
        subprocess.run(cmd, check=False, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

    out_md = f"{workspace_out_prefix}_summary.md"
    with open(out_md, "w", encoding="utf-8") as md:
        md.write("# VTune Profiling Summary\n\n")
        md.write(f"Target binary: `{target_bin}`\n\n")
        md.write(f"OMP_NUM_THREADS: {threads}  |  MPI procs: {nprocs}\n\n")
        md.write(f"Result directory (Raw data in container `/tmp`): `{latest_dir_local}`\n\n")

        if os.path.isfile(report_csv) and os.path.getsize(report_csv) > 0:
            md.write("## Top Functions\n\n")
            with open(report_csv, "r", encoding="utf-8") as f:
                lines = f.readlines()
                if len(lines) > 1:
                    md.writelines(lines[1:21])
            md.write("\n")

        if os.path.isfile(src_csv) and os.path.getsize(src_csv) > 0:
            md.write("## Parallel Overhead Indicators\n\n")
            spin_sum = 0.0
            imbal_sum = 0.0
            with open(src_csv, "r", encoding="utf-8") as f:
                reader = csv.reader(f)
                try:
                    next(reader)
                    for row in reader:
                        if len(row) >= 6:
                            try:
                                spin_sum += float(row[4]) if row[4] else 0.0
                                imbal_sum += float(row[5]) if row[5] else 0.0
                            except ValueError:
                                pass
                except StopIteration:
                    pass
            md.write(f"- Total Spin Time: {spin_sum}\n")
            md.write(f"- Load Imbalance Spin: {imbal_sum}\n\n")

        if os.path.isfile(top_csv) and os.path.getsize(top_csv) > 0:
            md.write("## Top-Down Summary\n\n")
            with open(top_csv, "r", encoding="utf-8") as f:
                md.writelines(f.readlines()[:15])
            md.write("\n")

    metrics = read_summary_metrics(top_csv)
    print(f"Report: {out_md}")
    print(f"  metrics: {metrics}")
    return metrics

def main():
    parser = argparse.ArgumentParser(description="Run Intel VTune hotspots profiling on an FTCMS test binary across an OpenMP thread sweep.")
    parser.add_argument("binary", nargs="?", default="test_main",
                        help="Test target name (e.g. 'field', 'matrix', 'main') or an explicit binary path. Default: test_main")
    parser.add_argument("-n", "--nprocs", type=int, default=1, help="Number of MPI processes (default: 1)")
    parser.add_argument("-t", "--threads", default="1,4,8",
                        help="Comma-separated OpenMP thread counts to sweep (default: 1,4,8)")
    parser.add_argument("-p", "--project", default="/workspaces/FTCMS/project/1Domain-Square2nd-modified",
                        help="FTCMS_PROJECT_PATH (Input/Output data folder). Unit tests usually ignore it.")
    parser.add_argument("--data-limit", type=int, default=100,
                        help="VTune result size cap in MB (stops collection when reached; protects against OOM). Default: 100")
    parser.add_argument("--duration", type=int, default=60,
                        help="Max collection seconds (0 = until the app exits). Default: 60")
    args = parser.parse_args()

    target_bin = resolve_binary(args.binary)
    bin_name = os.path.basename(target_bin)
    threads_list = parse_threads(args.threads)

    mpirun = resolve_tool("mpirun", [
        "/opt/intel/oneapi/mpi/latest/bin/mpirun",
        "/opt/intel/oneapi/mpi/*/bin/mpirun",
    ])
    vtune = resolve_tool("vtune", [
        "/opt/intel/oneapi/vtune/latest/bin64/vtune",
        "/opt/intel/oneapi/vtune/*/bin64/vtune",
    ])

    print(f"--- Start VTune Profiling Script (target: {bin_name}) ---")
    print(f"mpirun : {mpirun}")
    print(f"vtune  : {vtune}")
    print(f"threads: {threads_list}")

    if not os.path.isfile(target_bin):
        print(f"Error: binary not found: {target_bin}")
        print(f"Build it first, e.g.: python scripts/compile/compile.py -c intel -t {bin_name.replace('test_', '', 1)}")
        sys.exit(1)

    workspace_vtune_dir = "/workspaces/FTCMS/log/vtune"
    local_vtune_dir = "/tmp/vtune_results"
    tmp_dir = os.path.join(local_vtune_dir, "tmp")
    os.makedirs(workspace_vtune_dir, exist_ok=True)
    os.makedirs(local_vtune_dir, exist_ok=True)
    os.makedirs(tmp_dir, exist_ok=True)

    # Allow ptrace for VTune in WSL2/Docker
    ptrace_file = "/proc/sys/kernel/yama/ptrace_scope"
    if os.path.exists(ptrace_file):
        subprocess.run(["sudo", "tee", ptrace_file], input=b"0\n", stdout=subprocess.DEVNULL, check=False)

    elapsed_by_threads = {}
    for threads in threads_list:
        metrics = profile_once(target_bin, bin_name, mpirun, vtune, args.nprocs, threads,
                               args.project, workspace_vtune_dir, local_vtune_dir, tmp_dir,
                               args.data_limit, args.duration)
        if metrics and "Elapsed Time" in metrics:
            elapsed_by_threads[threads] = metrics["Elapsed Time"]

    # Scaling overview across the thread sweep (quick reasonableness check).
    if elapsed_by_threads:
        print("\n--- Elapsed time vs threads ---")
        for t in threads_list:
            if t in elapsed_by_threads:
                print(f"  threads={t:>2}: Elapsed={elapsed_by_threads[t]} s")
    print("--- Done ---")

if __name__ == "__main__":
    main()
