#!/usr/bin/env bash
# Compile-time scaling gate for the SIR and physical-MIR verifiers.
#
# Usage:
#   HEW_BIN=build/bin/hew tests/perf/verify-linear.sh
#
# A chain of last-use record transfers across actor calls is the shape a real
# actor body has: every await adds a suspension with its resume, cancel and
# unwind edges. The gate compiles the chain at three lengths and measures the
# `physical lowering` phase, which covers source through verified physical MIR.
#
# What it asserts is cost per emitted SIR operation, not cost per await. SIR
# scope exit ends every in-scope binding place on every fault edge, so the chain
# lowers to a quadratic number of `end_lifetime` operations, and no verifier can
# be linear in the await count while that holds. The verifiers can
# and must be linear in the body they are handed, which is what this measures: a
# dominance relation stored as sets, or a whole-function fixed point recomputed
# per terminator, makes the per-operation cost grow with the body and fails here.
#
# Flip the commented assertion below to the await count once scope-exit cleanup
# is shared rather than duplicated per fault edge.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
HEW_BIN="${HEW_BIN:-$ROOT/build/bin/hew}"
LENGTHS=(128 256 512)
# Cost per operation at the longest chain, over the shortest. One is flat; the
# headroom covers cache behaviour at a body of several hundred thousand
# operations and ordinary scheduling noise.
PER_OP_LIMIT=2
# Virtual-memory ceiling for one compile. Per-block verifier state that scales
# with the body shows up here before it shows up in the clock.
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

# Milliseconds the compiler reports for source through verified physical MIR.
# Two runs, keep the faster: the first pays for reading the compiler and the
# standard library off disk, which is not what this gate measures.
lowering_ms() {
    local source="$1" log="$2" best="" reported
    for _ in 1 2; do
        if ! (
            ulimit -v "$ADDRESS_SPACE_KB" 2>/dev/null || true
            HEW_MEASURE_TIMINGS=1 "$HEW_BIN" build "$source" -o "$tmpdir/chain.out"
        ) >"$log" 2>&1; then
            cat "$log" >&2
            return 1
        fi
        reported="$(
            awk '/^hew measure: physical lowering /{ reported = $5 } END { printf "%d", reported }' "$log"
        )"
        if [[ -z "$reported" ]]; then
            echo "verify-linear: the compiler reported no physical lowering phase" >&2
            return 1
        fi
        if [[ -z "$best" || "$reported" -lt "$best" ]]; then
            best="$reported"
        fi
    done
    echo "$best"
}

# Operations in the SIR the phase above verifies. Operation lines are the
# indented body of a block; block headers and terminators are not counted, and
# the exact convention does not matter as long as it is the same at every N.
operations() {
    "$HEW_BIN" tool compile "$1" --dump-sir 2>/dev/null | grep -cE '^    [a-z%$]'
}

declare -A elapsed_ms ops per_op
for n in "${LENGTHS[@]}"; do
    generate "$n" "$tmpdir/chain$n.hew"
    if ! elapsed_ms[$n]="$(lowering_ms "$tmpdir/chain$n.hew" "$tmpdir/chain$n.log")"; then
        echo "verify-linear: compiling the chain at N=$n failed" >&2
        exit 1
    fi
    ops[$n]="$(operations "$tmpdir/chain$n.hew")"
    if [[ "${ops[$n]}" -lt 1 ]]; then
        echo "verify-linear: the chain at N=$n lowered to no SIR operations" >&2
        exit 1
    fi
    per_op[$n]="$(python3 -c "print(f'{1000 * ${elapsed_ms[$n]} / ${ops[$n]}:.2f}')")"
    echo "verify-linear: N=$n physical lowering ${elapsed_ms[$n]} ms over ${ops[$n]} SIR operations, ${per_op[$n]} us each"
done

first="${LENGTHS[0]}"
last="${LENGTHS[${#LENGTHS[@]} - 1]}"
ratio="$(python3 -c "print(f'{${per_op[$last]} / ${per_op[$first]}:.2f}')")"
echo "verify-linear: per-operation cost at N=$last is ${ratio}x its cost at N=$first (limit $PER_OP_LIMIT)"

# Once scope-exit cleanup is shared across fault edges the operation count
# becomes linear in N and this becomes the direct assertion:
#   ${elapsed_ms[$last]} / ${elapsed_ms[$first]} <= 6

if python3 -c "import sys; sys.exit(0 if ${per_op[$last]} / ${per_op[$first]} > $PER_OP_LIMIT else 1)"; then
    echo "verify-linear: verification cost per operation grows with the body" >&2
    exit 1
fi
echo "verify-linear: verification cost per operation stays flat"
