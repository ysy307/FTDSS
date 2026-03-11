#!/usr/bin/env python3
import re
import argparse
from pathlib import Path
from collections import defaultdict

def clean_path(path_str, project_root):
    try:
        p = Path(path_str)
        if p.is_absolute() and p.is_relative_to(project_root):
            return str(p.relative_to(project_root))
        elif p.is_absolute() and p.is_relative_to(project_root.parent):
            return str(p.relative_to(project_root.parent))
        return path_str
    except (ValueError, TypeError):
        return path_str

def analyze_log(
    log_path_str="/workspaces/FTDSS/log/compile.log",
    output_path_str=None,
    project_root_str="/workspaces/FTDSS/src",
    compiler="intel"
):
    log_path = Path(log_path_str)
    project_root = Path(project_root_str)
    
    if output_path_str:
        output_file = Path(output_path_str)
    else:
        output_file = log_path.parent / "compile_summary.md"

    if not log_path.exists():
        print(f"Error: Log file not found at: {log_path}")
        return False

    compiler_noise_map = {
        "intel": r"ifx|icx|icpx",
        "gnu": r"gfortran|gcc|g\+\+",
        "nvidia": r"nvfortran|nvc|nvc\+\+"
    }
    comp_regex = compiler_noise_map.get(compiler.lower(), compiler_noise_map["intel"])

    # GNUの「fatal error」や「note」も含めるように拡張
    re_intel_msg = re.compile(r"^\s*(.*?)\((\d+)(?:,\d+)?\):\s*(warning|error|remark)\s*(#\d+)?:?\s*(.*)$", re.IGNORECASE)
    re_gnu_msg = re.compile(r"^\s*(.*?):(\d+):(?:\d+:)?\s*(warning|error|fatal error|note):\s*(.*)$", re.IGNORECASE)
    
    re_cmd_msg = re.compile(r"^\s*([a-zA-Z0-9_\-]+):\s*command line\s*(warning|error|remark)\s*(#\d+)?:?\s*(.*)$", re.IGNORECASE)
    re_linker_obj = re.compile(r"^ld:\s*(.*?):\s*in function\s*[`'](.*)['`]:", re.IGNORECASE)
    re_linker_def = re.compile(r".*undefined reference to\s*[`'](.*)['`]", re.IGNORECASE)
    re_noise = re.compile(fr"^(\[\d+/\d+\]|/opt/|/usr/|{comp_regex}|ninja:|make:).*", re.IGNORECASE)

    file_messages = defaultdict(list)
    general_messages = []
    linker_issues = defaultdict(set)
    global_name_warnings = set()

    current_entry = None
    current_linker_obj = "Unknown Object"

    with open(log_path, "r", encoding="utf-8", errors="replace") as f:
        for line in f:
            line = line.rstrip()
            if not line:
                continue

            if re_noise.match(line):
                current_entry = None
                continue

            m_obj = re_linker_obj.match(line)
            if m_obj:
                raw_obj = m_obj.group(1).strip()
                current_linker_obj = clean_path(raw_obj, project_root)
                current_entry = None
                continue

            m_def = re_linker_def.match(line)
            if m_def:
                symbol = m_def.group(1).strip()
                linker_issues[symbol].add(current_linker_obj)
                current_entry = None
                continue
            
            m_intel = re_intel_msg.match(line)
            m_gnu = re_gnu_msg.match(line)

            if m_intel or m_gnu:
                # Pylanceエラーを排除し、型と値を確定させる
                if m_intel:
                    raw_filename = m_intel.group(1)
                    line_no = m_intel.group(2)
                    severity = m_intel.group(3)
                    msg_body = m_intel.group(5)
                elif m_gnu:
                    raw_filename = m_gnu.group(1)
                    line_no = m_gnu.group(2)
                    severity = m_gnu.group(3)
                    msg_body = m_gnu.group(4)
                else:
                    continue

                raw_filename = raw_filename.strip()
                filename = clean_path(raw_filename, project_root)
                severity = severity.lower()
                msg_body = msg_body.strip()

                if "Global name too long" in msg_body:
                    global_name_warnings.add(msg_body)
                    current_entry = None
                    continue

                current_entry = {
                    "line": line_no,
                    "type": severity,
                    "msg": msg_body,
                    "context": []
                }
                file_messages[filename].append(current_entry)
                continue

            m_cmd = re_cmd_msg.match(line)
            if m_cmd:
                general_messages.append({
                    "tool": m_cmd.group(1),
                    "type": m_cmd.group(2).lower(),
                    "msg": m_cmd.group(4).strip()
                })
                current_entry = None
                continue

            if current_entry is not None:
                current_entry["context"].append(line)

    with open(output_file, "w", encoding="utf-8") as f:
        f.write("# Compile Log Summary\n\n")

        if linker_issues:
            f.write("## 🚨 Linker Errors (Undefined References)\n\n")
            f.write("> **Note:** The following symbols are missing, causing the build to fail.\n\n")
            for symbol, callers in linker_issues.items():
                f.write(f"### 🛑 Missing Symbol: `{symbol}`\n")
                f.write("Required by:\n")
                for caller in sorted(callers):
                    f.write(f"- `{caller}`\n")
                f.write("\n")
            f.write("---\n\n")

        if global_name_warnings:
            f.write("## ⚠️ Global Name Length Warnings\n\n")
            f.write("> **Note:** These symbols were shortened by the compiler because they exceeded the length limit. This warning appeared across multiple files but is listed here once per symbol.\n\n")
            for msg in sorted(global_name_warnings):
                f.write(f"- ⚠️ {msg}\n")
            f.write("\n---\n\n")

        if general_messages:
            f.write("## ⚙️ Command Line & General Warnings\n\n")
            for entry in general_messages:
                sev_icon = "🛑" if entry['type'] in ('error', 'fatal error') else "⚠️"
                f.write(f"- {sev_icon} **{entry['tool']}**: {entry['msg']}\n")
            f.write("\n---\n\n")

        if not file_messages and not linker_issues and not general_messages and not global_name_warnings:
            f.write("✅ **Success:** No warnings or errors found.\n")
        
        sorted_files = sorted(file_messages.keys(), key=lambda k: (
            0 if any(e['type'] in ('error', 'fatal error') for e in file_messages[k]) else 1, 
            k
        ))

        for filename in sorted_files:
            has_error = any(e['type'] in ('error', 'fatal error') for e in file_messages[filename])
            icon = "❌" if has_error else "⚠️"
            
            f.write(f"## {icon} File: `{filename}`\n\n")

            for entry in file_messages[filename]:
                sev_icon = "🛑" if entry['type'] in ('error', 'fatal error') else "⚠️" if entry['type'] == 'warning' else "ℹ️"
                f.write(f"- **Line {entry['line']}**: {sev_icon} **{entry['type'].upper()}**: {entry['msg']}\n")
                
                if entry['context']:
                    f.write("\n  ```fortran\n")
                    for ctx in entry['context']:
                        f.write(f"  {ctx}\n")
                    f.write("  ```\n")
                f.write("\n")
            
            f.write("---\n\n")

    print(f"Analysis complete. Summary saved to: {output_file}")
    return True

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Analyze compilation log.")
    parser.add_argument("--log", default="/workspaces/FTDSS/log/compile.log", help="Path to compile log")
    parser.add_argument("--out", default=None, help="Path to output markdown file")
    parser.add_argument("--root", default="/workspaces/FTDSS/src", help="Project root directory")
    parser.add_argument("-c", "--compiler", default="intel", choices=["intel", "gnu", "nvidia"], help="Compiler type used for noise filtering")
    
    args = parser.parse_args()
    analyze_log(
        log_path_str=args.log,
        output_path_str=args.out,
        project_root_str=args.root,
        compiler=args.compiler
    )