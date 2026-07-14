#!/usr/bin/env python3
"""Compare two FTCMS output directories for regression testing.

Usage:
    python compare_outputs.py <golden_dir> <test_dir> [--log-a A.log --log-b B.log]

Checks, in order:
  1. File set equality (files present in one dir but not the other are reported).
  2. Per-file byte comparison (cmp-style).
  3. For .vtu files that differ at byte level, parses the VTK XML and reports
     the per-DataArray maximum absolute difference (inline base64 binary or
     ascii formats are supported).
  4. Optionally compares step/iteration statistics extracted from stdout logs
     (--log-a / --log-b); wall-clock timings are ignored.

Exit code 0 iff every compared file is bit-identical (and, when logs are
given, step/iteration counts match). Exit code 1 otherwise.
"""

import argparse
import base64
import filecmp
import re
import struct
import sys
import xml.etree.ElementTree as ET
from pathlib import Path

import numpy as np

VTK_DTYPES = {
    "Float64": np.float64,
    "Float32": np.float32,
    "Int64": np.int64,
    "Int32": np.int32,
    "UInt64": np.uint64,
    "UInt32": np.uint32,
    "Int16": np.int16,
    "UInt16": np.uint16,
    "Int8": np.int8,
    "UInt8": np.uint8,
}

HEADER_DTYPES = {
    "UInt32": ("<I", 4),
    "UInt64": ("<Q", 8),
}


def decode_data_array(elem, header_fmt, header_size):
    """Return a numpy array for one <DataArray> element (binary/ascii)."""
    dtype = VTK_DTYPES[elem.get("type")]
    fmt = elem.get("format", "ascii")
    text = (elem.text or "").strip()
    if fmt == "ascii":
        if not text:
            return np.array([], dtype=dtype)
        return np.array(text.split(), dtype=dtype)
    if fmt == "binary":
        raw = base64.b64decode(text)
        (nbytes,) = struct.unpack(header_fmt, raw[:header_size])
        payload = raw[header_size:header_size + nbytes]
        return np.frombuffer(payload, dtype=dtype)
    raise ValueError(f"unsupported DataArray format: {fmt}")


def compare_vtu(path_a, path_b):
    """Compare two VTU files array-by-array.

    Returns a list of (array_name, status, max_abs_diff_or_msg).
    """
    results = []
    try:
        root_a = ET.parse(path_a).getroot()
        root_b = ET.parse(path_b).getroot()
    except ET.ParseError as exc:
        return [("<xml>", "PARSE_ERROR", str(exc))]

    if root_a.get("compressor") or root_b.get("compressor"):
        return [("<xml>", "UNSUPPORTED", "compressed VTU not supported")]

    def header_info(root):
        ht = root.get("header_type", "UInt32")
        return HEADER_DTYPES[ht]

    fmt_a, size_a = header_info(root_a)
    fmt_b, size_b = header_info(root_b)

    def collect(root):
        arrays = {}
        for da in root.iter("DataArray"):
            name = da.get("Name", "<unnamed>")
            arrays.setdefault(name, []).append(da)
        return arrays

    arrays_a = collect(root_a)
    arrays_b = collect(root_b)

    names = sorted(set(arrays_a) | set(arrays_b))
    for name in names:
        la = arrays_a.get(name, [])
        lb = arrays_b.get(name, [])
        if len(la) != len(lb):
            results.append((name, "COUNT_MISMATCH",
                            f"{len(la)} vs {len(lb)} occurrences"))
            continue
        for idx, (ea, eb) in enumerate(zip(la, lb)):
            tag = name if len(la) == 1 else f"{name}[{idx}]"
            try:
                va = decode_data_array(ea, fmt_a, size_a)
                vb = decode_data_array(eb, fmt_b, size_b)
            except Exception as exc:  # noqa: BLE001
                results.append((tag, "DECODE_ERROR", str(exc)))
                continue
            if va.shape != vb.shape:
                results.append((tag, "SHAPE_MISMATCH",
                                f"{va.shape} vs {vb.shape}"))
                continue
            if va.size == 0:
                continue
            if np.array_equal(va, vb):
                continue
            diff = np.max(np.abs(va.astype(np.float64)
                                 - vb.astype(np.float64)))
            results.append((tag, "DIFFERS", f"max|diff| = {diff:.6e}"))
    return results


STEP_PATTERNS = [
    # Lines that report step / iteration progress. Wall-clock timings are
    # deliberately not captured.
    re.compile(r"step\s*[:=]?\s*(\d+)", re.IGNORECASE),
    re.compile(r"iter(?:ation)?s?\s*[:=]?\s*(\d+)", re.IGNORECASE),
]

TIME_LIKE = re.compile(
    r"(elapsed|wall|cpu|time \(s\)|\d{4}-\d{2}-\d{2}T)", re.IGNORECASE)


def log_signature(path):
    """Extract an order-preserving list of step/iteration numbers from a
    stdout log, skipping lines that contain wall-clock information."""
    sig = []
    with open(path, errors="replace") as fh:
        for line in fh:
            if TIME_LIKE.search(line):
                continue
            for pat in STEP_PATTERNS:
                for m in pat.finditer(line):
                    sig.append((pat.pattern, m.group(1)))
    return sig


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("dir_a", type=Path, help="golden output directory")
    ap.add_argument("dir_b", type=Path, help="test output directory")
    ap.add_argument("--log-a", type=Path, help="stdout log of run A")
    ap.add_argument("--log-b", type=Path, help="stdout log of run B")
    ap.add_argument("--ignore", action="append", default=["run.log"],
                    help="file names to skip (default: run.log; repeatable)")
    args = ap.parse_args()

    if not args.dir_a.is_dir() or not args.dir_b.is_dir():
        print("ERROR: both arguments must be directories")
        return 2

    files_a = {p.relative_to(args.dir_a) for p in args.dir_a.rglob("*")
               if p.is_file()}
    files_b = {p.relative_to(args.dir_b) for p in args.dir_b.rglob("*")
               if p.is_file()}
    ignored = set(args.ignore)
    files_a = {f for f in files_a if f.name not in ignored}
    files_b = {f for f in files_b if f.name not in ignored}

    ok = True
    only_a = sorted(files_a - files_b)
    only_b = sorted(files_b - files_a)
    if only_a:
        ok = False
        print(f"Files only in {args.dir_a}:")
        for f in only_a:
            print(f"  {f}")
    if only_b:
        ok = False
        print(f"Files only in {args.dir_b}:")
        for f in only_b:
            print(f"  {f}")

    n_identical = 0
    n_diff = 0
    for rel in sorted(files_a & files_b):
        pa, pb = args.dir_a / rel, args.dir_b / rel
        if filecmp.cmp(pa, pb, shallow=False):
            n_identical += 1
            continue
        ok = False
        n_diff += 1
        print(f"DIFFERS (bytes): {rel}")
        if rel.suffix == ".vtu":
            details = compare_vtu(pa, pb)
            if not details:
                print("    (all DataArrays numerically identical; "
                      "difference is metadata only)")
            for name, status, info in details:
                print(f"    [{status}] {name}: {info}")

    if args.log_a and args.log_b:
        sig_a = log_signature(args.log_a)
        sig_b = log_signature(args.log_b)
        if sig_a == sig_b:
            print(f"Log step/iteration signature: MATCH "
                  f"({len(sig_a)} entries)")
        else:
            ok = False
            print(f"Log step/iteration signature: MISMATCH "
                  f"({len(sig_a)} vs {len(sig_b)} entries)")
            for i, (a, b) in enumerate(zip(sig_a, sig_b)):
                if a != b:
                    print(f"  first divergence at entry {i}: {a} vs {b}")
                    break

    total = len(files_a & files_b)
    print(f"\nSummary: {n_identical}/{total} files bit-identical, "
          f"{n_diff} differ, {len(only_a) + len(only_b)} unmatched")
    print("RESULT:", "PASS (bit-identical)" if ok else "FAIL")
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
