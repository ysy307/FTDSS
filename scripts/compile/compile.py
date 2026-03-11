#!/usr/bin/env python3
import subprocess
import sys
import argparse
from pathlib import Path
from check_log import analyze_log

def run_and_log(cmd, log_file_obj):
    # errors="replace" を追加し、デコードエラーによる強制終了を防止
    with subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True, encoding="utf-8", errors="replace") as p:
        if p.stdout:
            for line in p.stdout:
                sys.stdout.write(line)
                sys.stdout.flush()
                log_file_obj.write(line)
                log_file_obj.flush()  # ファイルへ即座に書き出す
        p.wait()
    
    if p.returncode != 0:
        raise subprocess.CalledProcessError(p.returncode, cmd)

def main():
    parser = argparse.ArgumentParser(description="Build and Analyze FTDSS.")
    parser.add_argument("-t", "--target", default="main", help="Target test module")
    parser.add_argument("-c", "--compiler", default="intel", choices=["intel", "gnu", "nvidia"], help="Compiler toolchain")
    args = parser.parse_args()

    compilers = {
        "intel": {"fc": "ifx", "cc": "icx", "cxx": "icpx"},
        "gnu": {"fc": "gfortran", "cc": "gcc", "cxx": "g++"},
        "nvidia": {"fc": "nvfortran", "cc": "nvc", "cxx": "nvc++"}
    }
    
    fc = compilers[args.compiler]["fc"]
    cc = compilers[args.compiler]["cc"]
    cxx = compilers[args.compiler]["cxx"]

    log_dir = Path("/workspaces/FTDSS/log")
    log_dir.mkdir(parents=True, exist_ok=True)
    log_file = log_dir / "compile.log"
    build_dir = f"CMakeBuild/{args.compiler}"

    config_cmd = [
        "cmake", "-S", ".", "-B", build_dir,
        "-DBUILD_TESTING=ON",
        "-DCMAKE_BUILD_TYPE=Debug",
        f"-DCMAKE_Fortran_COMPILER={fc}",
        f"-DCMAKE_C_COMPILER={cc}",
        f"-DCMAKE_CXX_COMPILER={cxx}",
        "-DMKL_SYCL_LINK=OFF",
        "-G", "Ninja"
    ]

    build_cmd = [
        "cmake", "--build", build_dir,
        "--target", f"test_{args.target}",
        "--parallel", "4"
    ]

    print(f"--- Build and Analysis Started: {args.target} ({args.compiler}) ---")
    
    success = False
    with open(log_file, "w", encoding="utf-8", errors="replace") as f:
        try:
            print("--- Stage 1: Configure ---")
            run_and_log(config_cmd, f)
            print("--- Stage 2: Build ---")
            run_and_log(build_cmd, f)
            print("\n✅ Build finished successfully.")
            success = True
        except subprocess.CalledProcessError as e:
            print(f"\n❌ Build failed (Exit Code: {e.returncode}).")
            success = False

    print("\n--- Stage 3: Analyzing Log ---")
    try:
        analyze_log(
            log_path_str=str(log_file),
            compiler=args.compiler
        )
    except Exception as e:
        print(f"⚠️ Log analysis failed: {e}")

if __name__ == "__main__":
    main()
