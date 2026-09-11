#!/usr/bin/env bash
# Compile-time scaling gate for the SIR/MIR verifiers.
#
# Usage:
#   HEW_BIN=build/bin/hew tests/perf/verify-linear.sh
#
# A chain of last-use record transfers across actor calls is the shape a real
# actor body has: every `await` adds a suspension with its resume, cancel and
# unwind edges, so the block count grows with the number of awaits in one
# function. Lowering that chain to physical MIR must stay proportional to the
# chain length. The gate compiles the chain at three lengths and compares the
# longest against the shortest; a verifier whose per-block state or dominance
# query is quadratic blows past the ratio long before it blows past a wall
# clock ceiling, and the ratio stays meaningful on a slower or busier machine.
#
# The ratio ceiling is 6 for a 4x change in chain length: linear with headroom
# for the fixed per-compile cost that shrinks the ratio at the small end and
# for ordinary scheduling noise.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
HEW_BIN="${HEW_BIN:-$ROOT/build/bin/hew}"
LENGTHS=(128 256 512)
RATIO_LIMIT=6
# Virtual-memory ceiling for one compile. Quadratic per-block state shows up
# here before it shows up in the clock. `ulimit -v` is a no-op on some hosts;
# the ratio remains the portable check.
ADDRESS_SPACE_KB=4194304

if [[ $# -ne 0 ]]; then
    echo "usage: $0" >&2
    exit 2
fi
if [[ "$HEW_BIN" != /* ]]; then
    HEW_BIN="$(cd "$(dirname "$HEW_BIN")" && pwd)/$(basename "$HEW_BIN")"
fi
if [[ ! -x "$HEW_BIN" ]]; then
    echo "verify-linear: compiler binary not found at $HEW_BIN" >&2
    exit 2
fi

tmpdir="$(mktemp -d "${TMPDIR:-/tmp}/hew-verify-linear.XXXXXX")"
trap 'rm -rf "$tmpdir"' EXIT

# Emit the chain: N awaits, each moving the whole record into the next call.
generate() {
    local n="$1" out="$2" i
    {
        echo "type Payload {"
        for i in 0 1 2 3 4 5 6 7; do echo "    f$i: string,"; done
        echo "}"
        echo
        echo "actor Gate {"
        echo "    receive fn pass(value: Payload) -> Payload { value }"
        echo "}"
        echo
        echo "fn main() {"
        echo "    let gate = spawn Gate();"
        printf '    let value0 = Payload {'
        for i in 0 1 2 3 4 5 6 7; do printf ' f%s: "ok",' "$i"; done
        echo " };"
        for ((i = 1; i <= n; i++)); do
            echo "    let value$i = gate.pass(value$((i - 1))).expect(\"reply\");"
        done
        echo "    println(value$n.f0);"
        echo "    close(gate);"
        echo "}"
    } >"$out"
}

now_ns() {
    python3 -c 'import time; print(time.perf_counter_ns())'
}

# Two runs, keep the faster: the first pays for reading the compiler and the
# standard library off disk, which is not what this gate measures.
measure() {
    local source="$1" log="$2" best="" start end elapsed
    for _ in 1 2; do
        start="$(now_ns)"
        if ! (
            ulimit -v "$ADDRESS_SPACE_KB" 2>/dev/null || true
            "$HEW_BIN" tool compile "$source" --dump-mir physical
        ) >"$log" 2>&1; then
            cat "$log" >&2
            return 1
        fi
        end="$(now_ns)"
        elapsed="$(((end - start) / 1000000))"
        if [[ -z "$best" || "$elapsed" -lt "$best" ]]; then
            best="$elapsed"
        fi
    done
    echo "$best"
}

declare -A elapsed_ms
for n in "${LENGTHS[@]}"; do
    generate "$n" "$tmpdir/chain$n.hew"
    if ! elapsed_ms[$n]="$(measure "$tmpdir/chain$n.hew" "$tmpdir/chain$n.log")"; then
        echo "verify-linear: compiling the chain at N=$n failed" >&2
        exit 1
    fi
    echo "verify-linear: N=$n physical lowering ${elapsed_ms[$n]} ms"
done

first="${LENGTHS[0]}"
last="${LENGTHS[${#LENGTHS[@]} - 1]}"
base="${elapsed_ms[$first]}"
if [[ "$base" -lt 1 ]]; then
    base=1
fi
ratio="$(python3 -c "print(f'{${elapsed_ms[$last]} / $base:.2f}')")"
echo "verify-linear: N=$last / N=$first ratio $ratio (limit $RATIO_LIMIT for a ${last}/${first}x chain)"

if python3 -c "import sys; sys.exit(0 if ${elapsed_ms[$last]} / $base > $RATIO_LIMIT else 1)"; then
    echo "verify-linear: physical lowering is superlinear in the chain length" >&2
    exit 1
fi
echo "verify-linear: physical lowering scales with the chain length"
