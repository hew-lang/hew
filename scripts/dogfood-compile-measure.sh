#!/usr/bin/env bash
# Compile the dogfood-shaped corpus fixture with the staged release-lib
# compiler and REPORT what it produced.
#
# Usage:
#   HEW_BIN=build/bin/hew scripts/dogfood-compile-measure.sh
#
# This is telemetry, not a gate on IR shape. It used to compare exact LLVM
# define-block bytes, define count and basic-block count against a committed
# baseline; every benign codegen change broke it, and the fix was always to
# regenerate the baseline. A number whose only response is "update the number"
# measures nothing anybody decides on, and it cost time on the required lint
# job to do it.
#
# What remains is a real behavioural floor -- the fixture must COMPILE and the
# compiler must emit IR containing at least one function -- plus the
# measurements themselves, printed for a human and into the run summary when
# one exists. A regression in what this fixture compiles to is caught by the
# ll-byte-identity oracle, where byte identity IS the contract, and by the
# compiled-Hew behaviour suites, which run the programs.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
FIXTURE="$ROOT/tests/compile-measure/dogfood-shape.hew"
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
if [[ ! -f "$FIXTURE" ]]; then
    echo "dogfood-compile-measure: fixture is missing at $FIXTURE" >&2
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

# Anti-vacuity floor, and the only thing here that can fail: a fixture that
# compiles to no functions at all means the compile silently produced nothing,
# which would make every number below a confident zero.
if (( defines < 1 )); then
    echo "dogfood-compile-measure: --emit-llvm produced IR with no functions" >&2
    exit 1
fi

report() {
    echo "dogfood-compile-measure: telemetry (no shape is asserted)"
    echo "  LLVM define-block bytes: $ll_bytes"
    echo "  defines: $defines"
    echo "  basic blocks: $basic_blocks"
    echo "  wall: ${wall_ms} ms"
}
report
if [[ -n "${GITHUB_STEP_SUMMARY:-}" ]]; then
    {
        echo "### Dogfood compile telemetry"
        echo ""
        echo "| Measure | Value |"
        echo "| --- | ---: |"
        echo "| LLVM define-block bytes | ${ll_bytes} |"
        echo "| defines | ${defines} |"
        echo "| basic blocks | ${basic_blocks} |"
        echo "| wall (ms) | ${wall_ms} |"
    } >> "$GITHUB_STEP_SUMMARY"
fi

grep '^hew measure: MIR lowering ' "$log" || echo "hew measure: MIR lowering unavailable"
grep '^hew measure: backend ' "$log" || echo "hew measure: backend unavailable"
echo "hew measure: wall ${wall_ms} ms"
