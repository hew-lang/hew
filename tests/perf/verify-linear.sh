#!/usr/bin/env bash
# Compile-time scaling gate for the SIR and physical-MIR verifiers.
#
# Usage:
#   HEW_BIN=build/bin/hew tests/perf/verify-linear.sh
#
# A chain of record transfers across actor calls is the shape a real actor body
# has: every await adds a suspension with its resume, cancel and unwind edges,
# so a function with a few hundred awaits is a function with a few thousand
# blocks. Reaching physical MIR must stay proportional to the awaits, and a
# verifier that walks a whole table per operation does not.
#
# The record chain moves one record through one binding, so the body it lowers
# to grows with the await count and nothing else. The owner chain declares one
# closable binding per await and holds an owned string temporary across each
# await, so every await's fault edges owe one more release than the last; its
# body stays linear only while fault sites share their cleanup. Its clock is not
# gated: every block of that body carries all N live bindings in the builder's
# and both verifiers' dense per-block state, so its lowering time grows with
# awaits times bindings even when the body does not. The gate checks the
# operation count first: a
# lowering that makes the operation count superlinear would turn the timing
# below into a measurement of the lowering rather than of the verifiers, and
# says so instead of passing quietly.
#
# The measurement is the compiler's own `physical lowering` phase, which covers
# source through verified physical MIR. It is not `--dump-mir physical`: that
# text is itself quadratic in the chain length and would swamp the phase.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
HEW_BIN="${HEW_BIN:-$ROOT/build/bin/hew}"
LENGTHS=(128 256 512)
# Elapsed and emitted operations at the longest chain, over the shortest. A
# four-fold chain costs four times as much when both are linear; the headroom
# covers the fixed per-compile cost that inflates neither end and ordinary
# scheduling noise.
RATIO_LIMIT=6
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

# Emit the record chain: N awaits, each moving the record out of the binding
# and the reply back into it.
generate_record() {
    local n="$1" out="$2" i
    {
        echo "type Payload {"
        for i in 0 1 2 3 4 5 6 7; do echo "    f$i: string;"; done
        echo "}"
        echo
        echo "actor Gate {"
        echo "    receive fn pass(value: Payload) -> Payload { value }"
        echo "}"
        echo
        echo "fn main() {"
        echo "    let gate = spawn Gate();"
        printf '    var value = Payload {'
        for i in 0 1 2 3 4 5 6 7; do printf ' f%s: "ok",' "$i"; done
        echo " };"
        for ((i = 1; i <= n; i++)); do
            echo "    value = gate.pass(value).expect(\"reply\");"
        done
        echo "    println(value.f0);"
        echo "    stop(gate);"
        echo "    stopped(gate);"
        echo "}"
    } >"$out"
}

# Emit the owner chain: N awaits, each after declaring one more closable
# binding and while an owned string temporary is live.
generate_owners() {
    local n="$1" out="$2" i
    {
        echo "#[resource]"
        echo "type Lease {"
        echo "    id: i64;"
        echo "}"
        echo
        echo "impl Lease {"
        echo "    fn close(consume self) {"
        echo "        if self.id < 0 { println(\"negative lease\"); }"
        echo "    }"
        echo "}"
        echo
        echo "actor Gate {"
        echo "    receive fn pass(value: string) -> i64 { value.len() }"
        echo "}"
        echo
        echo "fn width(text: string, n: i64) -> i64 { text.len() + n }"
        echo
        echo "fn main() {"
        echo "    let gate = spawn Gate();"
        echo "    var total = 0;"
        for ((i = 1; i <= n; i++)); do
            echo "    var lease$i = Lease { id: $i };"
            echo "    total = total + width(f\"t{total}\", gate.pass(\"k\").expect(\"reply\"));"
        done
        echo "    println(total);"
        echo "    stop(gate);"
        echo "    stopped(gate);"
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

report() {
    python3 -c "print(f'{$1 / $2:.2f}')"
}
exceeds() {
    python3 -c "import sys; sys.exit(0 if $1 / $2 > $RATIO_LIMIT else 1)"
}

check_chain() {
    local chain="$1" timed="$2"
    local -A elapsed_ms ops
    local n
    for n in "${LENGTHS[@]}"; do
        "generate_$chain" "$n" "$tmpdir/$chain$n.hew"
        if ! elapsed_ms[$n]="$(lowering_ms "$tmpdir/$chain$n.hew" "$tmpdir/$chain$n.log")"; then
            echo "verify-linear: compiling the $chain chain at N=$n failed" >&2
            exit 1
        fi
        ops[$n]="$(operations "$tmpdir/$chain$n.hew")"
        if [[ "${ops[$n]}" -lt 1 ]]; then
            echo "verify-linear: the $chain chain at N=$n lowered to no SIR operations" >&2
            exit 1
        fi
        echo "verify-linear: $chain N=$n physical lowering ${elapsed_ms[$n]} ms over ${ops[$n]} SIR operations"
    done

    local first="${LENGTHS[0]}"
    local last="${LENGTHS[${#LENGTHS[@]} - 1]}"
    local operation_ratio elapsed_ratio
    operation_ratio="$(report "${ops[$last]}" "${ops[$first]}")"
    echo "verify-linear: $chain N=$last emits ${operation_ratio}x the operations of N=$first (limit $RATIO_LIMIT)"
    if exceeds "${ops[$last]}" "${ops[$first]}"; then
        echo "verify-linear: lowering the $chain chain is superlinear, so the timing below measures the lowering" >&2
        exit 1
    fi

    elapsed_ratio="$(report "${elapsed_ms[$last]}" "${elapsed_ms[$first]}")"
    if [[ "$timed" != timed ]]; then
        echo "verify-linear: $chain N=$last costs ${elapsed_ratio}x N=$first (not gated)"
        return
    fi
    echo "verify-linear: $chain N=$last costs ${elapsed_ratio}x N=$first (limit $RATIO_LIMIT)"
    if exceeds "${elapsed_ms[$last]}" "${elapsed_ms[$first]}"; then
        echo "verify-linear: reaching physical MIR is superlinear in the awaits per function" >&2
        exit 1
    fi
}

check_chain record timed
check_chain owners untimed
echo "verify-linear: reaching physical MIR scales with the awaits per function"
