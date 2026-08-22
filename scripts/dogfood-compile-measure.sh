#!/usr/bin/env bash
# Compile the dogfood-shaped corpus fixture with the staged release-lib compiler.
#
# Usage:
#   HEW_BIN=build/bin/hew scripts/dogfood-compile-measure.sh
#   HEW_BIN=build/bin/hew scripts/dogfood-compile-measure.sh --update
#
# --update is deliberately the sole baseline-writing path. Normal verification
# never changes the checked-in measurement values.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
FIXTURE="$ROOT/tests/compile-measure/dogfood-shape.hew"
BASELINE="$ROOT/tests/compile-measure/dogfood-shape-baseline.txt"
HEW_BIN="${HEW_BIN:-$ROOT/build/bin/hew}"
UPDATE=0

if [[ "${1:-}" == "--update" ]]; then
    UPDATE=1
    shift
fi
if [[ $# -ne 0 ]]; then
    echo "usage: $0 [--update]" >&2
    exit 2
fi
if [[ ! -x "$HEW_BIN" ]]; then
    echo "dogfood-compile-measure: compiler binary not found at $HEW_BIN" >&2
    exit 2
fi
if [[ "$HEW_BIN" != /* ]]; then
    HEW_BIN="$(cd "$(dirname "$HEW_BIN")" && pwd)/$(basename "$HEW_BIN")"
fi
if [[ ! -f "$FIXTURE" || ! -f "$BASELINE" ]]; then
    echo "dogfood-compile-measure: fixture or baseline is missing" >&2
    exit 2
fi

tmpdir="$(mktemp -d "${TMPDIR:-/tmp}/hew-dogfood-compile-measure.XXXXXX")"
trap 'rm -rf "$tmpdir"' EXIT
log="$tmpdir/compiler.log"

wall_start="$(python3 -c 'import time; print(time.perf_counter_ns())')"
if ! (
    cd "$tmpdir"
    HEW_MEASURE_TIMINGS=1 "$HEW_BIN" build --emit-llvm "$FIXTURE"
) >"$log" 2>&1; then
    cat "$log" >&2
    exit 1
fi
wall_end="$(python3 -c 'import time; print(time.perf_counter_ns())')"

ir="$tmpdir/dogfood-shape.ll"
if [[ ! -f "$ir" ]]; then
    echo "dogfood-compile-measure: --emit-llvm produced no LLVM IR" >&2
    exit 1
fi

# LLVM prints the target datalayout and triple in the module header. Those
# lines describe the host target, not the dogfood program, so count only the
# complete `define … }` blocks just as the LLVM identity oracle does.
ll_bytes="$(
    awk '
        /^define / { in_function = 1 }
        in_function { printf "%s\n", $0 }
        in_function && /^}/ { in_function = 0 }
    ' "$ir" | wc -c | tr -d ' '
)"
defines="$(grep -c '^define ' "$ir" || true)"
basic_blocks="$(
    awk '
        /^define / { in_function = 1; blocks += 1; next }
        in_function && /^[[:space:]]*[[:alnum:]_.-]+:$/ { blocks += 1; next }
        in_function && /^}/ { in_function = 0 }
        END { print blocks }
    ' "$ir"
)"
wall_ms="$(( (wall_end - wall_start) / 1000000 ))"

if (( UPDATE == 1 )); then
    {
        echo "# Generated only by:"
        echo "#   make dogfood-compile-measure DOGFOOD_MEASURE_UPDATE=1"
        echo "#"
        echo "# The gate compares exact LLVM define-block bytes and structural counts."
        echo "ll_bytes=$ll_bytes"
        echo "defines=$defines"
        echo "basic_blocks=$basic_blocks"
    } >"$BASELINE"
    echo "dogfood-compile-measure: updated $BASELINE"
fi

baseline_value() {
    local baseline="$1"
    local key="$2"
    local value
    value="$(sed -n "s/^${key}=//p" "$baseline")"
    if [[ ! "$value" =~ ^[1-9][0-9]*$ ]]; then
        echo "dogfood-compile-measure: baseline lacks a positive $key value" >&2
        exit 2
    fi
    printf '%s\n' "$value"
}

expect_exact() {
    local key="$1"
    local actual="$2"
    local expected
    expected="$(baseline_value "$BASELINE" "$key")"
    if [[ "$actual" != "$expected" ]]; then
        echo "dogfood-compile-measure: $key changed: expected $expected, got $actual" >&2
        echo "  Review the IR change, then explicitly run:" >&2
        echo "  make dogfood-compile-measure DOGFOOD_MEASURE_UPDATE=1" >&2
        exit 1
    fi
}

baseline_matches() {
    local baseline="$1"
    [[ "$ll_bytes" == "$(baseline_value "$baseline" ll_bytes)" ]] \
        && [[ "$defines" == "$(baseline_value "$baseline" defines)" ]] \
        && [[ "$basic_blocks" == "$(baseline_value "$baseline" basic_blocks)" ]]
}

expect_exact ll_bytes "$ll_bytes"
expect_exact defines "$defines"
expect_exact basic_blocks "$basic_blocks"

# This counterfactual proves that the comparison is load-bearing: changing the
# recorded byte count must make the same assertion fail.
counterfactual_baseline="$tmpdir/mutated-baseline.txt"
sed 's/^ll_bytes=.*/ll_bytes=1/' "$BASELINE" >"$counterfactual_baseline"
if baseline_matches "$counterfactual_baseline"; then
    echo "dogfood-compile-measure: mutated baseline unexpectedly passed" >&2
    exit 1
fi
echo "CF-dogfood-compile-measure: mutated baseline rejected"

echo "dogfood-compile-measure: exact IR gate passed"
echo "  LLVM define-block bytes: $ll_bytes"
echo "  defines: $defines"
echo "  basic blocks: $basic_blocks"
grep '^hew measure: MIR lowering ' "$log" || echo "hew measure: MIR lowering unavailable"
grep '^hew measure: backend ' "$log" || echo "hew measure: backend unavailable"
echo "hew measure: wall ${wall_ms} ms"
