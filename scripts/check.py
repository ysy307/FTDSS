#!/usr/bin/env python3
import re
from pathlib import Path

# --- 設定 ---
# コンパイルログの絶対パス
log_path = Path("/workspaces/FTDSS/log/compile.log")
if not log_path.exists():
    print(f"ログファイルが見つかりません: {log_path}")
    exit(1)

# 出力ファイル（同じフォルダに作成）
output_file = log_path.parent / "compile_summary.txt"

# --- 正規表現パターン (Intel oneAPI Fortran/C/C++) ---
patterns = [
    # C/C++: filename:line(:column): warning/error
    re.compile(r"^(.*?):(\d+):(?:(\d+):)?\s*warning: (.*)$", re.IGNORECASE),
    re.compile(r"^(.*?):(\d+):(?:(\d+):)?\s*error: (.*)$", re.IGNORECASE),

    # Fortran: filename(line): warning/error/remark #番号
    re.compile(r"^(.*)\((\d+)(?:,(\d+))?\):\s*warning.*:\s*(.*)$", re.IGNORECASE),
    re.compile(r"^(.*)\((\d+)(?:,(\d+))?\):\s*error.*:\s*(.*)$", re.IGNORECASE),
    re.compile(r"^(.*)\((\d+)(?:,(\d+))?\):\s*remark.*:\s*(.*)$", re.IGNORECASE),

    # Fortran: Warning:/Error:/Remark:
    re.compile(r"^Warning:\s*(.*)$", re.IGNORECASE),
    re.compile(r"^Error:\s*(.*)$", re.IGNORECASE),
    re.compile(r"^Remark:\s*(.*)$", re.IGNORECASE),

    # Fortran: warning/error/remark #番号
    re.compile(r"^warning #\d+:\s*(.*)$", re.IGNORECASE),
    re.compile(r"^error #\d+:\s*(.*)$", re.IGNORECASE),
    re.compile(r"^remark #\d+:\s*(.*)$", re.IGNORECASE),
]

# --- メッセージリスト ---
errors = []
warnings = []
remarks = []

# --- ログ解析 ---
with open(log_path, "r", encoding="utf-8", errors="ignore") as f:
    for line in f:
        line = line.strip()
        if not line:
            continue

        matched = False
        for pat in patterns:
            m = pat.match(line)
            if m:
                # メッセージを整理
                if 'warning' in pat.pattern.lower():
                    warnings.append(line)
                elif 'error' in pat.pattern.lower():
                    errors.append(line)
                elif 'remark' in pat.pattern.lower():
                    remarks.append(line)

                matched = True
                break


# --- 1つのファイルにまとめて書き込む ---
with open(output_file, "w", encoding="utf-8") as f:
    f.write(f"=== ERRORS ({len(errors)}) ===\n")
    f.write("\n".join(errors) + "\n\n")

    f.write(f"=== WARNINGS ({len(warnings)}) ===\n")
    f.write("\n".join(warnings) + "\n\n")

    f.write(f"=== REMARKS ({len(remarks)}) ===\n")
    f.write("\n".join(remarks) + "\n")

