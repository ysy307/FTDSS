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
        log_content = f"--- STDOUT ---\n{e.stdout}\n\n--- STDERR ---\n{e.stderr}"
        return False, f"シミュレーション失敗 (終了コード: {e.returncode})。詳細はコピー後のログを確認してください。\n{log_content}"

def main():
    """この関数内の設定項目を編集して、スクリプトを実行してください。"""
    # ======================================================================
    # --- ユーザー設定項目 (ここを編集してください) ---
    # ======================================================================
    # 「作業ディレクトリ」のパスのリスト。
    WORKING_DIR_PATHS = [
        "/workspaces/FTDSS/Inout/1Domain-Square1st",
        "/workspaces/FTDSS/Inout/2Domain-Square2nd",
        "/workspaces/FTDSS/Inout/2Domain-Triangle1st",
        "/workspaces/FTDSS/Inout/2Domain-Triangle2nd"
    ]

    # 最終的な結果を格納するベースディレクトリのパスのリスト。
    # 上のWORKING_DIR_PATHSと1対1で対応します。
    RESULTS_BASE_DIR_PATHS = [
        "/workspaces/FTDSS/Results/SQ1", # 1Domain-Square1st の結果
        "/workspaces/FTDSS/Results/SQ2", # 2Domain-Square2nd の結果
        "/workspaces/FTDSS/Results/TR1", # 2Domain-Triangle1st の結果
        "/workspaces/FTDSS/Results/TR2"  # 2Domain-Triangle2nd の結果
    ]
    
    # 各ディレクトリで試行するジオメトリの総数 (Geometry_1 から Geometry_N まで)
    GEOMETRY_COUNT = 4
    
    # 各シミュレーションで使用するスレッド数のリスト
    THREAD_LIST = [16]
    
    # 各シミュレーションを何回繰り返すか
    REPEAT_COUNT = 1
    # ======================================================================

    results_summary = []
    
    if len(WORKING_DIR_PATHS) != len(RESULTS_BASE_DIR_PATHS):
        print("❌致命的エラー: WORKING_DIR_PATHSとRESULTS_BASE_DIR_PATHSのリストの要素数が一致しません。")
        sys.exit(1)

    # --- メインループ ---
    # 1. 作業ディレクトリと結果保存先ディレクトリのペアでループ
    for work_path, result_base_path in zip(WORKING_DIR_PATHS, RESULTS_BASE_DIR_PATHS):
        working_dir = Path(work_path)
        results_base_dir = Path(result_base_path)
        json_to_modify = working_dir / "Input" / "Basic.json"

        print("\n" + "#"*80)
        print(f"## 新しいバッチを開始します: {working_dir.name} -> {results_base_dir.name}")
        print("#"*80)

        # --- 事前チェック ---
        if not working_dir.is_dir():
            print(f"❌エラー: 作業ディレクトリが見つかりません: '{working_dir}'。このバッチをスキップします。")
            results_summary.append(f"[スキップ] {working_dir} が見つかりません。")
            continue
        if not json_to_modify.is_file():
            print(f"❌エラー: 設定ファイルが見つかりません: '{json_to_modify}'。このバッチをスキップします。")
            results_summary.append(f"[スキップ] {json_to_modify} が見つかりません。")
            continue

        # --- シミュレーションの準備 ---
        try:
            PROJECT_PATH_FILE.write_text(str(working_dir))
        except Exception as e:
            print(f"❌エラー: '{PROJECT_PATH_FILE}' の書き込みに失敗: {e}。このバッチをスキップします。")
            results_summary.append(f"[スキップ] {PROJECT_PATH_FILE} への書き込み失敗。")
            continue

        # 2. ジオメトリ番号のループ (1からGEOMETRY_COUNTまで)
        for geo_num in range(1, GEOMETRY_COUNT + 1):
            geometry_name = f"Geometry_{geo_num}"
            geometry_filename = f"{geometry_name}.vtk"

            # 3. スレッド数のループ
            for num_threads in THREAD_LIST:
                
                # 4. 繰り返し回数のループ
                for i in range(1, REPEAT_COUNT + 1):
                    result_dest_dir = results_base_dir / geometry_name / f"thread{num_threads}" / f"p{i}"
                    
                    print(f"\n--- 処理中: {result_dest_dir} ---")

                    # JSONファイルを更新
                    success, message = update_json_settings(json_to_modify, num_threads, geometry_filename)
                    if not success:
                        results_summary.append(f"[失敗] {result_dest_dir} | {message}")
                        print(f"❌ {message}")
                        continue

                    # シミュレーション実行
                    success, message = run_simulation()
                    if not success:
                        try:
                            result_dest_dir.mkdir(parents=True, exist_ok=True)
                            shutil.copytree(working_dir, result_dest_dir, dirs_exist_ok=True)
                            (result_dest_dir / 'run_failed.log').write_text(message)
                        except Exception as copy_e:
                            message += f"\nさらに、失敗結果のコピーにも失敗しました: {copy_e}"
                        results_summary.append(f"[失敗] {result_dest_dir} | {message.splitlines()[0]}")
                        print(f"❌ シミュレーション失敗")
                        continue
                    
                    # 成功した場合、結果をコピー
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