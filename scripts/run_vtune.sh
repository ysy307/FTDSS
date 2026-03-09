#!/bin/zsh
set -e
set -o pipefail

echo "--- Start VTune Profiling Script ---"

# =========================
# Settings
# =========================
export FTDSS_PROJECT_PATH="/workspaces/FTDSS/project/1Domain-Square2nd-modified"
VTUNE_DIR="/workspaces/FTDSS/log/vtune"

if [[ ! -d "$VTUNE_DIR" ]]; then
    mkdir -p "$VTUNE_DIR"
fi

export TMPDIR="$VTUNE_DIR"

if [[ -f /proc/sys/kernel/yama/ptrace_scope ]]; then
    echo 0 | sudo tee /proc/sys/kernel/yama/ptrace_scope >/dev/null || true
fi

# =========================
# Determine Result Directory
# =========================
echo "Determining result directory..."
NEXT_IDX=0
while [[ -n $(find "$VTUNE_DIR" -maxdepth 1 -name "r$(printf "%03d" $NEXT_IDX)hs*") ]]; do
    NEXT_IDX=$((NEXT_IDX + 1))
done
RESULT_BASE="r$(printf "%03d" $NEXT_IDX)hs"
RESULT_DIR="${VTUNE_DIR}/${RESULT_BASE}"

echo "Target Result Directory: $RESULT_DIR"

# =========================
# Run VTune
# =========================
echo "Running VTune..."
set +e
# Execute mpirun inside vtune to preserve environment variables
vtune -collect hotspots \
    -knob sampling-mode=sw \
    -result-dir "$RESULT_DIR" \
    -- mpirun -genv FTDSS_PROJECT_PATH="$FTDSS_PROJECT_PATH" -n 1 ./bin/test_main
set -e

LATEST_DIR=$(find "$VTUNE_DIR" -maxdepth 1 -type d -name "${RESULT_BASE}*" | head -n 1)

if [[ -z "$LATEST_DIR" || ! -d "$LATEST_DIR" ]]; then
    echo "Error: VTune result directory was not created."
    exit 1
fi

echo "Actual Result Directory: $LATEST_DIR"

# =========================
# Export CSV Reports
# =========================
echo "Exporting CSV reports..."
vtune -report hotspots \
    -result-dir "$LATEST_DIR" \
    -format csv \
    -report-output "${LATEST_DIR}_report.csv" || true

vtune -report hotspots \
    -result-dir "$LATEST_DIR" \
    -group-by source-line \
    -format csv \
    -report-output "${LATEST_DIR}_source_line.csv" || true

vtune -report summary \
    -result-dir "$LATEST_DIR" \
    -format csv \
    -report-output "${LATEST_DIR}_summary.csv" || true

# =========================
# Markdown Summary Generation
# =========================
echo "Generating Markdown report..."
REPORT="${LATEST_DIR}_report.csv"
SRC="${LATEST_DIR}_source_line.csv"
TOP="${LATEST_DIR}_summary.csv"
OUT_MD="${LATEST_DIR}_summary.md"

{
    echo "# VTune Profiling Summary"
    echo ""
    echo "Result directory: \`$LATEST_DIR\`"
    echo ""

    if [[ -f "$REPORT" ]]; then
        echo "## Top Functions"
        echo ""
        tail -n +2 "$REPORT" | head -n 20
        echo ""
    fi

    if [[ -f "$SRC" ]]; then
        echo "## Parallel Overhead Indicators"
        echo ""
        SPIN=$(awk -F, 'NR>1 {sum+=$5} END{print sum}' "$SRC")
        IMBAL=$(awk -F, 'NR>1 {sum+=$6} END{print sum}' "$SRC")
        echo "- Total Spin Time: ${SPIN:-0}"
        echo "- Load Imbalance Spin: ${IMBAL:-0}"
        echo ""
    fi

    if [[ -f "$TOP" ]]; then
        echo "## Top-Down Summary"
        echo ""
        head -n 15 "$TOP"
    fi
} > "$OUT_MD"

echo "Markdown report written to: $OUT_MD"
echo "--- Done ---"