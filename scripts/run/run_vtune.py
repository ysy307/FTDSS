#!/usr/bin/env python3
import os
import sys
import glob
import csv
import subprocess

def main():
    print("--- Start VTune Profiling Script ---")

    # =========================
    # Settings
    # =========================
    ftdss_project_path = "/workspaces/FTDSS/project/1Domain-Square2nd-modified"
    vtune_dir = "/workspaces/FTDSS/log/vtune"
    tmp_dir = os.path.join(vtune_dir, "tmp")

    os.makedirs(vtune_dir, exist_ok=True)
    os.makedirs(tmp_dir, exist_ok=True)

    # Allow ptrace for VTune in WSL2/Docker
    ptrace_file = "/proc/sys/kernel/yama/ptrace_scope"
    if os.path.exists(ptrace_file):
        subprocess.run(["sudo", "tee", ptrace_file], input=b"0\n", stdout=subprocess.DEVNULL, check=False)

    # =========================
    # Determine Result Directory
    # =========================
    print("Determining result directory...")
    next_idx = 0
    while True:
        result_base = f"r{next_idx:03d}hs"
        existing = glob.glob(os.path.join(vtune_dir, f"{result_base}*"))
        if not existing:
            break
        next_idx += 1
    
    result_dir = os.path.join(vtune_dir, result_base)
    print(f"Target Result Directory: {result_dir}")

    # =========================
    # Run VTune
    # =========================
    print("Running VTune...")
    env = os.environ.copy()
    env["FTDSS_PROJECT_PATH"] = ftdss_project_path
    env["TMPDIR"] = tmp_dir

    # 修正箇所: mpirunでvtuneをラップし，-genvの引数を2つに分割
    vtune_cmd = [
        "mpirun",
        "-genv", "FTDSS_PROJECT_PATH", ftdss_project_path,
        "-n", "1",
        "vtune", "-collect", "hotspots",
        "-knob", "sampling-mode=sw",
        "-result-dir", result_dir,
        "--", "./bin/test_main"
    ]
    
    subprocess.run(vtune_cmd, env=env, check=False)

    # Find the actually created directory
    created_dirs = glob.glob(os.path.join(vtune_dir, f"{result_base}*"))
    valid_dirs = [d for d in created_dirs if os.path.isdir(d)]
    
    if not valid_dirs:
        print("Error: VTune result directory was not created.")
        sys.exit(1)
        
    latest_dir = valid_dirs[0]
    print(f"Actual Result Directory: {latest_dir}")

    # =========================
    # Export CSV Reports
    # =========================
    print("Exporting CSV reports...")
    report_csv = f"{latest_dir}_report.csv"
    src_csv = f"{latest_dir}_source_line.csv"
    top_csv = f"{latest_dir}_summary.csv"

    export_cmds = [
        ["vtune", "-report", "hotspots", "-result-dir", latest_dir, "-format", "csv", "-report-output", report_csv],
        ["vtune", "-report", "hotspots", "-result-dir", latest_dir, "-group-by", "source-line", "-format", "csv", "-report-output", src_csv],
        ["vtune", "-report", "summary", "-result-dir", latest_dir, "-format", "csv", "-report-output", top_csv]
    ]

    for cmd in export_cmds:
        subprocess.run(cmd, check=False, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

    # =========================
    # Markdown Summary Generation
    # =========================
    print("Generating Markdown report...")
    out_md = f"{latest_dir}_summary.md"

    with open(out_md, "w", encoding="utf-8") as md:
        md.write("# VTune Profiling Summary\n\n")
        md.write(f"Result directory: `{latest_dir}`\n\n")

        # Top Functions
        if os.path.isfile(report_csv) and os.path.getsize(report_csv) > 0:
            md.write("## Top Functions\n\n")
            with open(report_csv, "r", encoding="utf-8") as f:
                lines = f.readlines()
                if len(lines) > 1:
                    md.writelines(lines[1:21])
            md.write("\n")

        # Parallel Overhead Indicators
        if os.path.isfile(src_csv) and os.path.getsize(src_csv) > 0:
            md.write("## Parallel Overhead Indicators\n\n")
            spin_sum = 0.0
            imbal_sum = 0.0
            with open(src_csv, "r", encoding="utf-8") as f:
                reader = csv.reader(f)
                try:
                    next(reader)  # Skip header
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

        # Top-Down Summary
        if os.path.isfile(top_csv) and os.path.getsize(top_csv) > 0:
            md.write("## Top-Down Summary\n\n")
            with open(top_csv, "r", encoding="utf-8") as f:
                lines = f.readlines()
                md.writelines(lines[:15])
            md.write("\n")

    print(f"Markdown report written to: {out_md}")
    print("--- Done ---")

if __name__ == "__main__":
    main()