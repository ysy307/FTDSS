#!/usr/bin/env python3
import re
from pathlib import Path
from collections import defaultdict

# --- Configuration ---
log_path = Path("/workspaces/FTDSS/log/compile.log")
output_file = log_path.parent / "compile_summary.md"
project_root = Path("/workspaces/FTDSS/src")

if not log_path.exists():
    print(f"Error: Log file not found at: {log_path}")
    exit(1)

# --- Helper Function: Shorten Path ---
def clean_path(path_str):
    try:
        p = Path(path_str)
        if p.is_absolute() and p.is_relative_to(project_root):
            return str(p.relative_to(project_root))
        elif p.is_absolute() and p.is_relative_to(project_root.parent):
            return str(p.relative_to(project_root.parent))
        return path_str
    except (ValueError, TypeError):
        return path_str

# --- Regex Patterns ---
re_compile_msg = re.compile(r"^\s*(.*?)\((\d+)(?:,\d+)?\):\s*(warning|error|remark)\s*(#\d+)?:?\s*(.*)$", re.IGNORECASE)
re_cmd_msg = re.compile(r"^\s*([a-zA-Z0-9_\-]+):\s*command line\s*(warning|error|remark)\s*(#\d+)?:?\s*(.*)$", re.IGNORECASE)
re_linker_obj = re.compile(r"^ld:\s*(.*?):\s*in function\s*[`'](.*)['`]:", re.IGNORECASE)
re_linker_def = re.compile(r".*undefined reference to\s*[`'](.*)['`]", re.IGNORECASE)
re_noise = re.compile(r"^(\[\d+/\d+\]|/opt/|/usr/|mpiifx|ifx|gfortran|ninja:|make:).*", re.IGNORECASE)

# --- Data Structures ---
file_messages = defaultdict(list)
general_messages = []
linker_issues = defaultdict(set)
global_name_warnings = set()  # Store unique "Global name too long" messages

# --- Parsing Logic ---
current_entry = None
current_linker_obj = "Unknown Object"

with open(log_path, "r", encoding="utf-8", errors="ignore") as f:
    for line in f:
        line = line.rstrip()
        if not line:
            continue

        # Ignore build system noise
        if re_noise.match(line):
            current_entry = None
            continue

        # --- A. Linker Error Parsing ---
        m_obj = re_linker_obj.match(line)
        if m_obj:
            raw_obj = m_obj.group(1).strip()
            current_linker_obj = clean_path(raw_obj)
            current_entry = None
            continue

        m_def = re_linker_def.match(line)
        if m_def:
            symbol = m_def.group(1).strip()
            linker_issues[symbol].add(current_linker_obj)
            current_entry = None
            continue
        
        # --- B. Standard Compiler Message ---
        m_compile = re_compile_msg.match(line)
        if m_compile:
            raw_filename = m_compile.group(1).strip()
            filename = clean_path(raw_filename)
            line_no = m_compile.group(2)
            severity = m_compile.group(3).lower()
            msg_body = m_compile.group(5).strip()

            # --- SPECIAL HANDLING: Global name too long ---
            # If the message is about global name length, store it separately and unique it.
            if "Global name too long" in msg_body:
                global_name_warnings.add(msg_body)
                current_entry = None # Do not capture context for this
                continue

            current_entry = {
                "line": line_no,
                "type": severity,
                "msg": msg_body,
                "context": []
            }
            file_messages[filename].append(current_entry)
            continue

        # --- C. Command Line Message ---
        m_cmd = re_cmd_msg.match(line)
        if m_cmd:
            general_messages.append({
                "tool": m_cmd.group(1),
                "type": m_cmd.group(2).lower(),
                "msg": m_cmd.group(4).strip()
            })
            current_entry = None
            continue

        # --- D. Context Capture ---
        if current_entry:
            current_entry["context"].append(line)
        else:
            current_entry = None

# --- Generate Markdown Output ---
with open(output_file, "w", encoding="utf-8") as f:
    f.write("# Compile Log Summary\n\n")

    # 1. Linker Errors
    if linker_issues:
        f.write("## 🚨 Linker Errors (Undefined References)\n\n")
        f.write("> **Note:** The following symbols are missing, causing the build to fail.\n\n")
        for symbol, callers in linker_issues.items():
            f.write(f"### 🛑 Missing Symbol: `{symbol}`\n")
            f.write("Required by:\n")
            for caller in sorted(callers):
                f.write(f"- `{caller}`\n")
            f.write("\n")
        f.write("---\n")

    # 2. Global Name Warnings (Deduped)
    if global_name_warnings:
        f.write("## ⚠️ Global Name Length Warnings\n\n")
        f.write("> **Note:** These symbols were shortened by the compiler because they exceeded the length limit. This warning appeared across multiple files but is listed here once per symbol.\n\n")
        for msg in sorted(global_name_warnings):
             f.write(f"- ⚠️ {msg}\n")
        f.write("\n---\n")

    # 3. General Warnings
    if general_messages:
        f.write("## ⚙️ Command Line & General Warnings\n\n")
        for entry in general_messages:
            sev_icon = "🛑" if entry['type'] == 'error' else "⚠️"
            f.write(f"- {sev_icon} **{entry['tool']}**: {entry['msg']}\n")
        f.write("\n---\n")

    # 4. File-based Errors
    if not file_messages and not linker_issues and not general_messages and not global_name_warnings:
        f.write("✅ **Success:** No warnings or errors found.\n")
    
    # Sort files: Errors first, then Warnings
    sorted_files = sorted(file_messages.keys(), key=lambda k: (
        0 if any(e['type'] == 'error' for e in file_messages[k]) else 1, 
        k
    ))

    for filename in sorted_files:
        has_error = any(e['type'] == 'error' for e in file_messages[filename])
        icon = "❌" if has_error else "⚠️"
        
        f.write(f"## {icon} File: `{filename}`\n\n")

        for entry in file_messages[filename]:
            sev_icon = "🛑" if entry['type'] == 'error' else "⚠️" if entry['type'] == 'warning' else "ℹ️"
            f.write(f"- **Line {entry['line']}**: {sev_icon} **{entry['type'].upper()}**: {entry['msg']}\n")
            
            if entry['context']:
                f.write("\n  ```fortran\n")
                for ctx in entry['context']:
                    f.write(f"  {ctx}\n")
                f.write("  ```\n")
            f.write("\n")
        
        f.write("---\n")

print(f"Analysis complete. Summary saved to: {output_file}")