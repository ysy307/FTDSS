import json
import shutil
import subprocess
from pathlib import Path
import sys
from typing import Tuple
import traceback
import zipfile

# --- グローバル定数 (基本的に変更不要) ---
PROJECT_PATH_FILE = Path("/workspaces/FTCMS/ProjectPath.dir")
EXECUTABLE_PATH = Path("/workspaces/FTCMS/bin/test")
# ------------------------------------

def update_json_settings(json_path: Path, num_threads: int, geometry_filename: str) -> Tuple[bool, str]:
    try:
        if not json_path.is_file():
            return False, f"Cannot find the json file: {json_path}"
        with open(json_path, 'r', encoding='utf-8') as f:
            data = json.load(f)

        data['geometry_settings']['file_name'] = geometry_filename
        data['solver_settings']['parallel_settings']['threads']['num_threads'] = num_threads
        
        with open(json_path, 'w', encoding='utf-8') as f:
            json.dump(data, f, indent=4)
        return True, "JSON update succeeded"
    except KeyError as e:
        return False, f"Cannot find expected key in JSON: {e}"
    except Exception:
        return False, f"Unexpected error occurred while updating JSON:\n{traceback.format_exc()}"

def run_simulation() -> Tuple[bool, str]:
    if not EXECUTABLE_PATH.is_file():
        return False, f"Cannot find the executable file: '{EXECUTABLE_PATH}'"
    try:
        subprocess.run(
            [str(EXECUTABLE_PATH)],
            capture_output=True, text=True, check=True, encoding='utf-8'
        )
        return True, "Success"
    except subprocess.CalledProcessError as e:
        log_content = f"--- STDOUT ---\n{e.stdout}\n\n--- STDERR ---\n{e.stderr}"
        return False, f"Simulation failed (exit code: {e.returncode}). Please check the log after copying.\n{log_content}"

def archive_results(src_dir: Path, dest_zip: Path, compress_level: int = 9):
    dest_zip.parent.mkdir(parents=True, exist_ok=True)
    with zipfile.ZipFile(dest_zip, 'w', compression=zipfile.ZIP_DEFLATED, compresslevel=compress_level) as zipf:
        for file_path in src_dir.rglob('*'):
            zipf.write(file_path, file_path.relative_to(src_dir))

def main():
    WORKING_DIR_PATHS = [
        "/workspaces/FTCMS/project/1Domain-Square1st",
        "/workspaces/FTCMS/project/1Domain-Square2nd",
        "/workspaces/FTCMS/project/1Domain-Triangle1st",
        "/workspaces/FTCMS/project/1Domain-Triangle2nd"
    ]

    RESULTS_BASE_DIR_PATHS = [
        "/workspaces/FTCMS/Results/SQ1",
        "/workspaces/FTCMS/Results/SQ2",
        "/workspaces/FTCMS/Results/TR1",
        "/workspaces/FTCMS/Results/TR2"
    ]
    
    start = 1
    WORKING_DIR_PATHS = WORKING_DIR_PATHS[start:start+1]
    RESULTS_BASE_DIR_PATHS = RESULTS_BASE_DIR_PATHS[start:start+1]

    GEOMETRY_COUNT = 4
    
    # 各シミュレーションで使用するスレッド数のリスト
    THREAD_LIST = [28]
    # THREAD_LIST = [1,2,4,8,12,16,32]
    
    # 各シミュレーションを何回繰り返すか
    REPEAT_COUNT = 3
    # ======================================================================

    results_summary = []
    
    if len(WORKING_DIR_PATHS) != len(RESULTS_BASE_DIR_PATHS):
        print("❌Critical Error: Mismatch WORKING_DIR_PATHS and RESULTS_BASE_DIR_PATHS size.")
        sys.exit(1)

    for work_path, result_base_path in zip(WORKING_DIR_PATHS, RESULTS_BASE_DIR_PATHS):
        working_dir = Path(work_path)
        results_base_dir = Path(result_base_path)
        json_to_modify = working_dir / "Input" / "Basic.json"

        print("\n" + "#"*80)
        print(f"## Start new batch: {working_dir.name} -> {results_base_dir.name}")
        print("#"*80)

        if not working_dir.is_dir():
            print(f"❌Error: Cannot find the work directory: '{working_dir}'. This batch will be skipped.")
            results_summary.append(f"[Skip] Cannot find {working_dir}.")
            continue
        if not json_to_modify.is_file():
            print(f"❌Error: Cannot find the setting json: '{json_to_modify}'. This batch will be skipped.")
            results_summary.append(f"[Skip] Cannot find {json_to_modify}.")
            continue

        try:
            PROJECT_PATH_FILE.write_text(str(working_dir))
        except Exception as e:
            print(f"❌Error: Failed to write to '{PROJECT_PATH_FILE}': {e}. This batch will be skipped.")
            results_summary.append(f"[Skip] Failed to write to {PROJECT_PATH_FILE}.")
            continue

        for geo_num in range(1, GEOMETRY_COUNT + 1):
            geometry_name = f"Geometry_{geo_num}"
            geometry_filename = f"{geometry_name}.vtk"

            for num_threads in THREAD_LIST:
                for i in range(1, REPEAT_COUNT + 1):
                    result_dest_dir = results_base_dir / geometry_name / f"thread{num_threads}" / f"p{i}"
                    result_zip = result_dest_dir / "results"

                    print(f"\n--- Processing: {result_dest_dir} ---")

                    success, message = update_json_settings(json_to_modify, num_threads, geometry_filename)
                    if not success:
                        results_summary.append(f"[Failed] {result_dest_dir} | {message}")
                        print(f"❌ {message}")
                        continue

                    success, message = run_simulation()
                    if not success:
                        try:
                            result_dest_dir.mkdir(parents=True, exist_ok=True)
                            (result_dest_dir / 'run_failed.log').write_text(message)
                        except Exception as copy_e:
                            message += f"\nMoreover, failed to save the failed results: {copy_e}"
                        results_summary.append(f"[Failed] {result_dest_dir} | {message.splitlines()[0]}")
                        print(f"❌ Failed the simulation.")
                        continue

                    try:
                        archive_results(working_dir, result_zip.with_suffix(".zip"))
                        results_summary.append(f"[Success] {result_zip}.zip")
                        print(f"✅ Success: Results saved to {result_zip}.zip")
                    except Exception as e:
                        message = f"Simulation succeeded, but failed to archive results: {e}"
                        results_summary.append(f"[Failed] {result_dest_dir} | {message}")
                        print(f"❌ {message}")

    print("\n" + "="*80)
    print("All simulations are complete.")
    print("## Results Summary")
    print("---")
    for result in results_summary:
        print(result)
    print("="*80)

if __name__ == "__main__":
    main()