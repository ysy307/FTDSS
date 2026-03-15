#!/usr/bin/env python3
import os
import sys
import glob
import csv
import subprocess
import shutil

def main():
    print("--- Start VTune Profiling Script ---")

    # =========================
    # Settings
    # =========================
    ftcms_project_path = "/workspaces/FTCMS/project/1Domain-Square2nd-modified"
    
    # 最終的なレポートの保存先（マウントボリューム）
    workspace_vtune_dir = "/workspaces/FTCMS/log/vtune"
    
    # VTuneの生データ保存先（ワークスペース内）
    local_vtune_dir = "/workspaces/FTCMS/log/vtune/raw"
    tmp_dir = os.path.join(local_vtune_dir, "tmp")

    os.makedirs(workspace_vtune_dir, exist_ok=True)
    os.makedirs(local_vtune_dir, exist_ok=True)
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
        # ワークスペース側に同名のレポートがないかチェック
        existing = glob.glob(os.path.join(workspace_vtune_dir, f"{result_base}*"))
        if not existing:
            break
        next_idx += 1
    
    # 実際の計測ディレクトリは log/vtune/raw 以下
    result_dir = os.path.join(local_vtune_dir, result_base)
    print(f"Target Result Directory (Local): {result_dir}")

    # =========================
    # Run VTune
    # =========================
    print("Running VTune...")
    env = os.environ.copy()
    env["FTCMS_PROJECT_PATH"] = ftcms_project_path
    env["TMPDIR"] = tmp_dir
    # オーナーシップ警告の回避
    env["VTUNE_LOG_DIR"] = tmp_dir 

    vtune_cmd = [
        "mpirun",
        "-genv", "FTCMS_PROJECT_PATH", ftcms_project_path,
        "-n", "1",
        "vtune", "-collect", "hotspots",
        "-knob", "sampling-mode=sw",
        "-result-dir", result_dir,
        "--", "./bin/test_main"
    ]
    
    subprocess.run(vtune_cmd, env=env, check=False)

    # Find the actually created directory in log/vtune/raw
    created_dirs = glob.glob(os.path.join(local_vtune_dir, f"{result_base}*"))
    valid_dirs = [d for d in created_dirs if os.path.isdir(d)]
    
    if not valid_dirs:
        print("Error: VTune result directory was not created.")
        sys.exit(1)
        
    latest_dir_local = valid_dirs[0]
    actual_dir_name = os.path.basename(latest_dir_local)
    print(f"Actual Result Directory: {latest_dir_local}")

    # =========================
    # Export CSV Reports
    # =========================
    print("Exporting CSV reports...")
    
    # 出力先をワークスペースにする
    workspace_out_prefix = os.path.join(workspace_vtune_dir, actual_dir_name)
    
    report_csv = f"{workspace_out_prefix}_report.csv"
    src_csv = f"{workspace_out_prefix}_source_line.csv"
    top_csv = f"{workspace_out_prefix}_summary.csv"

    export_cmds = [
        ["vtune", "-report", "hotspots", "-result-dir", latest_dir_local, "-format", "csv", "-report-output", report_csv],
        ["vtune", "-report", "hotspots", "-result-dir", latest_dir_local, "-group-by", "source-line", "-format", "csv", "-report-output", src_csv],
        ["vtune", "-report", "summary", "-result-dir", latest_dir_local, "-format", "csv", "-report-output", top_csv]
    ]

    for cmd in export_cmds:
        subprocess.run(cmd, check=False, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

    # =========================
    # Markdown Summary Generation
    # =========================
    print("Generating Markdown report...")
    out_md = f"{workspace_out_prefix}_summary.md"

    with open(out_md, "w", encoding="utf-8") as md:
        md.write("# VTune Profiling Summary\n\n")
        md.write(f"Result directory (Raw data): `{latest_dir_local}`\n\n")

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