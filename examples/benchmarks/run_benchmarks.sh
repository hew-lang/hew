#!/usr/bin/env bash
# Benchmark runner: compares Hew vs Go algorithm performance
# Usage: ./run_benchmarks.sh [filter]
# Example: ./run_benchmarks.sh sort   (only run benchmarks matching "sort")

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
HEW_DIR="$SCRIPT_DIR/hew"
GO_DIR="$SCRIPT_DIR/go"
ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
HEW_COMPILER="$ROOT/target/release/hew"
RESULTS_FILE="$SCRIPT_DIR/results.csv"
FILTER="${1:-}"

# Pre-compile Go benchmarks
GO_BIN_DIR=$(mktemp -d)
trap "rm -rf $GO_BIN_DIR" EXIT

echo "=== Hew vs Go Algorithm Benchmark ==="
echo ""
echo "Compiling Go benchmarks..."
compiled=0
for gofile in "$GO_DIR"/bench_*.go; do
    name=$(basename "$gofile" .go)
    if [ -n "$FILTER" ] && [[ "$name" != *"$FILTER"* ]]; then continue; fi
    go build -o "$GO_BIN_DIR/$name" "$gofile" 2>/dev/null && compiled=$((compiled + 1))
done
echo "Compiled $compiled Go benchmarks."
echo ""

# CSV header
echo "algorithm,hew_seconds,go_seconds,hew_ops_sec,go_ops_sec,ratio,iters" >"$RESULTS_FILE"

# Table header
printf "%-28s %12s %12s %14s %14s %8s\n" "Algorithm" "Hew (s)" "Go (s)" "Hew ops/s" "Go ops/s" "Ratio"
printf "%-28s %12s %12s %14s %14s %8s\n" "---" "---" "---" "---" "---" "---"

for hewfile in "$HEW_DIR"/bench_*.hew; do
    name=$(basename "$hewfile" .hew)
    algo=${name#bench_}

    if [ -n "$FILTER" ] && [[ "$name" != *"$FILTER"* ]]; then continue; fi

    gobin="$GO_BIN_DIR/$name"
    if [ ! -f "$gobin" ]; then continue; fi

    # Run Hew benchmark
    start_ns=$(date +%s%N)
    hew_out=$("$HEW_COMPILER" run "$hewfile" 2>/dev/null || echo "ERROR")
    end_ns=$(date +%s%N)
    hew_ns=$((end_ns - start_ns))
    hew_secs=$(echo "scale=4; $hew_ns / 1000000000" | bc)
    # Find the first line that looks like a pure integer (the iteration count)
    hew_iters=$(echo "$hew_out" | grep -m1 '^[0-9]\+$' || echo "ERROR")

    # Run Go benchmark
    start_ns=$(date +%s%N)
    go_out=$("$gobin" 2>/dev/null || echo "ERROR")
    end_ns=$(date +%s%N)
    go_ns=$((end_ns - start_ns))
    go_secs=$(echo "scale=4; $go_ns / 1000000000" | bc)
    go_iters=$(echo "$go_out" | grep -m1 '^[0-9]\+$' || echo "ERROR")

    # Calculate ops/sec
    if [ "$hew_iters" != "ERROR" ] && [ "$hew_ns" -gt 0 ] && [ "$hew_iters" -gt 0 ] 2>/dev/null; then
        hew_ops=$(echo "scale=0; $hew_iters * 1000000000 / $hew_ns" | bc)
        go_ops=$(echo "scale=0; $go_iters * 1000000000 / $go_ns" | bc)
        if [ "$hew_ops" -gt 0 ]; then
            ratio=$(echo "scale=2; $go_ops / $hew_ops" | bc)
        else
            ratio="N/A"
        fi
    else
        hew_ops="ERR"
        go_ops="ERR"
        ratio="N/A"
    fi

    printf "%-28s %12s %12s %14s %14s %8s\n" "$algo" "$hew_secs" "$go_secs" "$hew_ops" "$go_ops" "${ratio}x"
    echo "$algo,$hew_secs,$go_secs,$hew_ops,$go_ops,$ratio,$hew_iters" >>"$RESULTS_FILE"
done

echo ""
echo "Results saved to $RESULTS_FILE"
