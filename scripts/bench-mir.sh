#!/usr/bin/env bash
# Time `hew check` on the MIR-lowering-shape fixture and fail above the budget.
#
# Usage:
#   HEW_BIN=build/bin/hew scripts/bench-mir.sh
#
# The fixture is one emitter body with ~50 string concatenations, guarded field
# emissions, and nested loops over `Vec<record>` — the shape whose ownership
# replay dominates MIR lowering. The budget guards the cost model, not the
# host: it is generous enough that ordinary machine-to-machine variation and a
# loaded CI runner stay inside it, and tight enough that a return to per-block
# CFG replay blows through it by an order of magnitude.
#
# The run is repeated and the FASTEST wall time is scored. A minimum-of-N is
# the right statistic for a CPU-bound, deterministic workload: noise on a
# shared runner only ever adds time, so the minimum estimates the true cost and
# a slow neighbour cannot fail the gate on its own.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
FIXTURE="$ROOT/tests/compile-measure/mir-lowering-shape.hew"
BUDGET="$ROOT/tests/compile-measure/mir-lowering-budget.txt"
HEW_BIN="${HEW_BIN:-$ROOT/build/bin/hew}"
RUNS="${BENCH_MIR_RUNS:-3}"

if [[ $# -ne 0 ]]; then
    echo "usage: $0" >&2
    exit 2
fi
if [[ ! -x "$HEW_BIN" ]]; then
    echo "bench-mir: compiler binary not found at $HEW_BIN" >&2
    exit 2
fi
if [[ "$HEW_BIN" != /* ]]; then
    HEW_BIN="$(cd "$(dirname "$HEW_BIN")" && pwd)/$(basename "$HEW_BIN")"
fi
if [[ ! -f "$FIXTURE" || ! -f "$BUDGET" ]]; then
    echo "bench-mir: fixture or budget file is missing" >&2
    exit 2
fi

ceiling="$(sed -n 's/^check_wall_ms_ceiling=//p' "$BUDGET")"
if [[ ! "$ceiling" =~ ^[1-9][0-9]*$ ]]; then
    echo "bench-mir: budget lacks a positive check_wall_ms_ceiling value" >&2
    exit 2
fi

tmpdir="$(mktemp -d "${TMPDIR:-/tmp}/hew-bench-mir.XXXXXX")"
trap 'rm -rf "$tmpdir"' EXIT
log="$tmpdir/check.log"

export HEW_STD="${HEW_STD:-$ROOT/std}"
best=""
for _ in $(seq "$RUNS"); do
    start="$(python3 -c 'import time; print(time.perf_counter_ns())')"
    if ! "$HEW_BIN" check "$FIXTURE" >"$log" 2>&1; then
        cat "$log" >&2
        echo "bench-mir: the fixture must compile clean; a rejected fixture measures nothing" >&2
        exit 1
    fi
    end="$(python3 -c 'import time; print(time.perf_counter_ns())')"
    elapsed_ms=$(((end - start) / 1000000))
    if [[ -z "$best" || "$elapsed_ms" -lt "$best" ]]; then
        best="$elapsed_ms"
    fi
done

echo "bench-mir: fastest of $RUNS runs: ${best} ms (ceiling ${ceiling} ms)"
if ((best > ceiling)); then
    echo "bench-mir: MIR lowering time exceeds the budget: ${best} ms > ${ceiling} ms" >&2
    echo "  Re-run with HEW_MEASURE_TIMINGS=1 to see which pass and which function grew:" >&2
    echo "    HEW_STD=$ROOT/std HEW_MEASURE_TIMINGS=1 $HEW_BIN check $FIXTURE" >&2
    exit 1
fi
