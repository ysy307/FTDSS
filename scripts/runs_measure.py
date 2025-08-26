import json
import shutil
import subprocess
from pathlib import Path
import sys
from typing import Tuple
import traceback

# --- グローバル定数 (基本的に変更不要) ---
PROJECT_PATH_FILE = Path("/workspaces/FTDSS/ProjectPath.dir")
EXECUTABLE_PATH = Path("/workspaces/FTDSS/bin/test")
# ------------------------------------

def update_json_settings(json_path: Path, num_threads: int, geometry_filename: str) -> Tuple[bool, str]:
    """
    指定されたパスのJSONファイルを直接更新する。
    """
    try:
        if not json_path.is_file():
            return False, f"JSONファイルが見つかりません: {json_path}"
        with open(json_path, 'r', encoding='utf-8') as f:
            data = json.load(f)

        # 正しい階層をたどって値を更新
        data['geometry_settings']['file_name'] = geometry_filename
        data['solver_settings']['parallel_settings']['threads']['num_threads'] = num_threads
        
        with open(json_path, 'w', encoding='utf-8') as f:
            json.dump(data, f, indent=4)
        return True, "JSON更新成功"
    except KeyError as e:
        return False, f"JSON内に期待されるキーが見つかりません: {e}"
    except Exception:
        return False, f"JSON更新中に予期せぬエラーが発生しました:\n{traceback.format_exc()}"

def run_simulation() -> Tuple[bool, str]:
    """
    シミュレーションを実行する。
    作業ディレクトリはPROJECT_PATH_FILEによって指定されている。
    """
    if not EXECUTABLE_PATH.is_file():
        return False, f"実行ファイルが見つかりません: '{EXECUTABLE_PATH}'"
    try:
        subprocess.run(
            [str(EXECUTABLE_PATH)],
            capture_output=True, text=True, check=True, encoding='utf-8'
        )
        return True, "成功"
    except subprocess.CalledProcessError as e:
        # エラーログは専用ディレクトリにコピーされた後に確認できる
        log_content = f"--- STDOUT ---\n{e.stdout}\n\n--- STDERR ---\n{e.stderr}"
        return False, f"シミュレーション失敗 (終了コード: {e.returncode})。詳細はコピー後のログを確認してください。\n{log_content}"

def main():
    """この関数内の設定項目を編集して、スクリプトを実行してください。"""
    # ======================================================================
    # --- ユーザー設定項目 (ここを編集してください) ---
    # ======================================================================
    # 「作業ディレクトリ」のパス。InputとOutputがこの中にある。
    WORKING_DIR_PATH = "/workspaces/FTDSS/Inout/1Domain-Square2nd"
    # 最終的な結果を格納するベースディレクトリのパス
    RESULTS_BASE_DIR_PATH = "/workspaces/FTDSS/Results/SQ2"
    THREAD_LIST = [16, 32]
    # THREAD_LIST = [1, 2, 4, 8, 16, 32]
    GEOMETRY_COUNT = 1
    # GEOMETRY_COUNT = 4
    # ======================================================================

    results_summary = []
    working_dir = Path(WORKING_DIR_PATH)
    results_base_dir = Path(RESULTS_BASE_DIR_PATH)
    json_to_modify = working_dir / "Input" / "Basic.json"

    # --- 事前チェック ---
    if not working_dir.is_dir():
        print(f"❌致命的エラー: 作業ディレクトリが見つかりません: '{working_dir}'")
        sys.exit(1)
    if not json_to_modify.is_file():
        print(f"❌致命的エラー: 設定ファイルが見つかりません: '{json_to_modify}'")
        sys.exit(1)

    # --- シミュレーション全体の準備 ---
    print("シミュレーションバッチ処理を開始します...")
    print(f"作業ディレクトリ: {working_dir}")
    print(f"結果保存先: {results_base_dir}")
    
    # ProjectPath.dirを作業ディレクトリに設定（一度だけでOK）
    try:
        PROJECT_PATH_FILE.write_text(str(working_dir))
    except Exception as e:
        print(f"❌致命的エラー: '{PROJECT_PATH_FILE}' の書き込みに失敗しました: {e}")
        sys.exit(1)

    # --- メインループ ---
    for geo_num in range(1, GEOMETRY_COUNT + 1):
        geometry_name = "Geometry_4"
        # geometry_name = f"Geometry_{geo_num}"
        geometry_filename = f"{geometry_name}.vtk"

        for num_threads in THREAD_LIST:
            for i in range(1, 4):
                # 最終的な結果保存先を定義
                result_dest_dir = results_base_dir / geometry_name / f"thread{num_threads}" / f"p{i}"
                
                print(f"\n--- 処理中: {result_dest_dir} ---")

                # 1. 作業ディレクトリのJSONを直接変更
                success, message = update_json_settings(json_to_modify, num_threads, geometry_filename)
                if not success:
                    results_summary.append(f"[失敗] {result_dest_dir} | {message}")
                    print(f"❌ {message}")
                    continue

                # 2. シミュレーション実行
                success, message = run_simulation()
                if not success:
                    # 失敗した場合でも、その時点のInputと失敗ログを保存試行
                    try:
                        result_dest_dir.mkdir(parents=True, exist_ok=True)
                        shutil.copytree(working_dir, result_dest_dir, dirs_exist_ok=True)
                        (result_dest_dir / 'run_failed.log').write_text(message)
                    except Exception as copy_e:
                        message += f"\nさらに、失敗結果のコピーにも失敗しました: {copy_e}"
                    results_summary.append(f"[失敗] {result_dest_dir} | {message.splitlines()[0]}")
                    print(f"❌ シミュレーション失敗")
                    continue
                
                # 3. 成功した場合、作業ディレクトリ全体を結果保存先にコピー
                try:
                    result_dest_dir.mkdir(parents=True, exist_ok=True)
                    shutil.copytree(working_dir, result_dest_dir, dirs_exist_ok=True)
                    results_summary.append(f"[成功] {result_dest_dir}")
                    print(f"✅ 成功。結果をコピーしました。")
                except Exception as e:
                    message = f"シミュレーションは成功しましたが、結果のコピーに失敗しました: {e}"
                    results_summary.append(f"[失敗] {result_dest_dir} | {message}")
                    print(f"❌ {message}")

    # --- 最終結果の出力 ---
    print("\n" + "="*80)
    print("全シミュレーションが完了しました。")
    print("## 実行結果サマリー")
    print("---")
    for result in results_summary:
        print(result)
    print("="*80)

if __name__ == "__main__":
    main()