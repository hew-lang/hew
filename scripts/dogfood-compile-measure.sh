#!/usr/bin/env bash
# Compile the dogfood-shaped corpus fixture with the staged release-lib compiler.
#
# Usage:
#   HEW_BIN=build/bin/hew scripts/dogfood-compile-measure.sh
#
# Normal verification never changes the checked-in ceiling. Tighten it manually
# after a material, reviewed IR shrink; a self-updating ceiling would not defend
# against a size regression.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
FIXTURE="$ROOT/tests/compile-measure/dogfood-shape.hew"
BASELINE="$ROOT/tests/compile-measure/dogfood-shape-baseline.txt"
HEW_BIN="${HEW_BIN:-$ROOT/build/bin/hew}"
if [[ $# -ne 0 ]]; then
    echo "usage: $0" >&2
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
# lines describe the host target, not the dogfood program, so the size ceiling
# counts only complete `define … }` blocks.
ll_bytes="$(
    awk '
        /^define / { in_function = 1 }
        in_function { printf "%s\n", $0 }
        in_function && /^}/ { in_function = 0 }
    ' "$ir" | wc -c | tr -d ' '
)"
wall_ms="$(((wall_end - wall_start) / 1000000))"

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

expect_at_most() {
    local key="$1"
    local actual="$2"
    local ceiling
    ceiling="$(baseline_value "$BASELINE" "$key")"
    if ((actual > ceiling)); then
        echo "dogfood-compile-measure: $key exceeds ceiling: $actual bytes > $ceiling bytes" >&2
        echo "  Review the IR change. After a material reviewed shrink, lower the ceiling manually." >&2
        exit 1
    fi
}

expect_at_most ll_bytes_ceiling "$ll_bytes"

ceiling="$(baseline_value "$BASELINE" ll_bytes_ceiling)"

echo "dogfood-compile-measure: IR-size ceiling gate passed"
echo "  LLVM define-block bytes: $ll_bytes"
echo "  LLVM define-block byte ceiling: $ceiling"
grep '^hew measure: MIR lowering ' "$log" || echo "hew measure: MIR lowering unavailable"
grep '^hew measure: backend ' "$log" || echo "hew measure: backend unavailable"
echo "hew measure: wall ${wall_ms} ms"
