#!/bin/zsh
set -e

# =========================
# Settings
# =========================
# プロジェクトパスの設定
export FTDSS_PROJECT_PATH="/workspaces/FTDSS/project/1Domain-Square2nd-modified"

VTUNE_DIR="${FTDSS_PROJECT_PATH}/log/vtune"

# ディレクトリが存在しない場合のみ作成
if [[ ! -d "$VTUNE_DIR" ]]; then
    mkdir -p "$VTUNE_DIR"
fi

export TMPDIR="$VTUNE_DIR"

# ptrace_scopeの設定
if [[ -f /proc/sys/kernel/yama/ptrace_scope ]]; then
    echo 0 | sudo tee /proc/sys/kernel/yama/ptrace_scope >/dev/null
fi

# =========================
# Run VTune
# =========================
# -- を使用して引数を明確に分離
vtune -collect hotspots \
    -knob sampling-mode=sw \
    -result-dir "${VTUNE_DIR}/r@@@hs" \
    -- ./bin/test_main

# 最新の結果ディレクトリを取得
LATEST_DIR=$(ls -td ${VTUNE_DIR}/r*hs | head -n 1)

# =========================
# Export CSV Reports
# =========================
# レポート生成。失敗しても処理を継続
vtune -report hotspots \
    -result-dir "$LATEST_DIR" \
    -columns="CPU Time,CPU Time:Self,Function,Source File" \
    -format csv \
    -report-output "${LATEST_DIR}_report.csv" || true

vtune -report hotspots \
    -result-dir "$LATEST_DIR" \
    -group-by source-line \
    -columns="Source File,Source Line,CPU Time,CPU Time:Effective Time,CPU Time:Spin Time,CPU Time:Spin Time:Imbalance or Serial Spinning,CPU Time:Spin Time:Lock Contention,CPU Time:Spin Time:Other,CPU Time:Overhead Time" \
    -format csv \
    -report-output "${LATEST_DIR}_source_line.csv" || true

vtune -report summary \
    -result-dir "$LATEST_DIR" \
    -format csv \
    -report-output "${LATEST_DIR}_summary.csv" || true

# =========================
# Markdown Summary Generation
# =========================
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
        echo "## Top Functions (CPU Time)"
        echo ""
        echo "| CPU Time | Self | Function | File |"
        echo "|---|---|---|---|"
        tail -n +2 "$REPORT" | sort -t, -k1 -nr | head -n 20 | awk -F, '{printf("| %.3f | %.3f | %s | %s |\n",$1,$2,$3,$4)}'
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
        head -n 15 "$TOP" | awk -F, '
        NR==1{
            printf("|")
            for(i=1;i<=NF;i++) printf(" %s |",$i)
            printf("\n|")
            for(i=1;i<=NF;i++) printf("---|")
            printf("\n")
            next
        }
        {
            printf("|")
            for(i=1;i<=NF;i++) printf(" %s |",$i)
            printf("\n")
        }
        '
    fi
} > "$OUT_MD"

echo "Markdown report written to: $OUT_MD"