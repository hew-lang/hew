#!/usr/bin/env bash
# Gate MIR lowering cost on two fixtures: a wall-clock ceiling, and a
# machine-independent check that the cost model is still proportional to the
# pass sequence rather than to block count.
#
# Usage:
#   HEW_BIN=build/bin/hew scripts/bench-mir.sh
#
# The fixtures are one emitter body with string concatenations, guarded field
# emissions, and nested loops over `Vec<record>` — the shape whose ownership
# replay dominates MIR lowering. `mir-lowering-shape.hew` carries the large
# body; `mir-lowering-shape-small.hew` is identical to it except that the body
# is short. Every other declaration, helper, and call is the same, so the two
# compile the same number of function bodies through the same pass sequence and
# the only variable between them is that one body's block count.
#
# Two gates, because they fail on different things:
#
#   wall time     — the fixture's `hew check` must stay under a budget. Guards
#                   the cost model, not the host: generous enough that ordinary
#                   machine-to-machine variation and a loaded CI runner stay
#                   inside it. The run is repeated and the FASTEST wall time is
#                   scored, which is the right statistic for a CPU-bound,
#                   deterministic workload: noise on a shared runner only ever
#                   adds time, so the minimum estimates the true cost and a slow
#                   neighbour cannot fail the gate on its own.
#
#   derivations   — the number of whole-function owner-state QUERIES must not
#                   grow much between the small fixture and the large one.
#                   A pass that asks once per invocation contributes the same
#                   count to both; a pass that asks inside a per-block loop
#                   contributes the body's block count, which differs by 253
#                   between the two. This gate has no clock in it, so it fails
#                   identically on a fast laptop and a loaded CI runner, and it
#                   names the defect rather than its symptom.
#
#   replays       — how many of those queries actually recomputed, per body.
#                   Growth alone cannot see a large FIXED cost: fifty passes
#                   each replaying the whole function once contributes the same
#                   count to both fixtures and passes the growth gate while
#                   being exactly the cost F5 exists to remove. This ceiling is
#                   absolute, so it does see it.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
FIXTURE="$ROOT/tests/compile-measure/mir-lowering-shape.hew"
SMALL="$ROOT/tests/compile-measure/mir-lowering-shape-small.hew"
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
if [[ ! -f "$FIXTURE" || ! -f "$SMALL" || ! -f "$BUDGET" ]]; then
    echo "bench-mir: a fixture or the budget file is missing" >&2
    exit 2
fi

tmpdir="$(mktemp -d "${TMPDIR:-/tmp}/hew-bench-mir.XXXXXX")"
trap 'rm -rf "$tmpdir"' EXIT
log="$tmpdir/check.log"

export HEW_STD="${HEW_STD:-$ROOT/std}"

# The pair only measures one variable while everything except `emit_document`
# is identical between them. Nothing else in the tree enforces that, so the gate
# checks it before it trusts either reading: strip the leading comment banner
# and the `emit_document` body from both files and require the remainder to
# match byte for byte.
outside_emit_document() {
    awk '
        /^fn emit_document\(/ { skip = 1 }
        skip && /^}$/          { skip = 0; next }
        skip                   { next }
        /^\/\// && !seen      { next }
        { seen = 1; print }
    ' "$1"
}

if ! diff -u <(outside_emit_document "$SMALL") <(outside_emit_document "$FIXTURE") >"$tmpdir/twin.diff" 2>&1; then
    cat "$tmpdir/twin.diff" >&2
    echo "bench-mir: the two fixtures differ outside \`emit_document\`" >&2
    echo "  They must differ in that body alone, or the derivation-growth gate below" >&2
    echo "  is comparing two programs rather than two sizes of one." >&2
    exit 2
fi

budget_value() {
    local name="$1" value
    value="$(sed -n "s/^$name=//p" "$BUDGET")"
    if [[ ! "$value" =~ ^[1-9][0-9]*$ ]]; then
        echo "bench-mir: budget lacks a positive $name value" >&2
        exit 2
    fi
    printf '%s' "$value"
}

ceiling="$(budget_value check_wall_ms_ceiling)"
growth_ceiling="$(budget_value derivation_growth_ceiling)"
replay_ceiling="$(budget_value replays_per_100_bodies_ceiling)"

# One fixture's owner-state accounting, summed over every analysis the timing
# report names. `hew check` prints these to stderr under HEW_MEASURE_TIMINGS as
# `mir derivations <analysis> <count> bodies <n>` (queries) and
# `mir replays <analysis> <count> bodies <n>` (the ones that recomputed).
# Prints "<queries> <replays> <bodies>". awk does the summing: a `bc` that is
# not installed would abort the assignment under `set -e` with no diagnostic,
# and these are integers.
measure() {
    local source="$1" report="$tmpdir/derivations.log" reading
    if ! HEW_MEASURE_TIMINGS=1 "$HEW_BIN" check "$source" >"$report" 2>&1; then
        cat "$report" >&2
        echo "bench-mir: the fixture must compile clean; a rejected fixture measures nothing" >&2
        exit 1
    fi
    reading="$(awk '
        $4 == "derivations" { queries += $6; bodies = $8 }
        $4 == "replays"     { replays += $6; bodies = $8 }
        END { print queries + 0, replays + 0, bodies + 0 }
    ' "$report")"
    read -r queries replays bodies <<<"$reading"
    if ((queries == 0 || replays == 0 || bodies == 0)); then
        echo "bench-mir: no owner-state accounting in the timing report for $source" >&2
        echo "  HEW_MEASURE_TIMINGS must name every owner-state query and replay; an" >&2
        echo "  empty reading would let this gate pass while measuring nothing." >&2
        exit 2
    fi
    printf '%s %s %s' "$queries" "$replays" "$bodies"
}

read -r large_derivations large_replays large_bodies <<<"$(measure "$FIXTURE")"
read -r small_derivations _ _ <<<"$(measure "$SMALL")"
growth=$((large_derivations - small_derivations))
echo "bench-mir: owner-state derivations: $large_derivations on the large body," \
    "$small_derivations on the small one (growth $growth, ceiling $growth_ceiling)"
if ((growth > growth_ceiling)); then
    echo "bench-mir: the derivation count grew with body size: $growth > $growth_ceiling" >&2
    echo "  A pass is deriving owner state inside a per-block loop. The per-site" >&2
    echo "  table that names it is a debug-build facility (\`#[track_caller]\` costs" >&2
    echo "  measurable time on the shipping path), so build one and read it:" >&2
    echo "    make hew-native" >&2
    echo "    HEW_STD=$ROOT/std HEW_MEASURE_TIMINGS=1 target/debug/hew check $SMALL 2>&1 | grep 'mir derivation '" >&2
    echo "  and compare the per-site counts against the same run on $FIXTURE." >&2
    exit 1
fi

replays_per_100_bodies=$((large_replays * 100 / large_bodies))
echo "bench-mir: owner-state replays: $large_replays over $large_bodies bodies" \
    "($replays_per_100_bodies per 100 bodies, ceiling $replay_ceiling)"
if ((replays_per_100_bodies > replay_ceiling)); then
    echo "bench-mir: owner state is being replayed too often per body:" \
        "$replays_per_100_bodies > $replay_ceiling per 100 bodies" >&2
    echo "  A query that the memo used to answer is recomputing. Either a pass" >&2
    echo "  now rewrites the ownership operations where it did not before, or a" >&2
    echo "  caller bypassed \`exact_owner_states\`/\`maybe_owner_states\`." >&2
    echo "  Read the per-site table from a debug build to see which:" >&2
    echo "    make hew-native" >&2
    echo "    HEW_STD=$ROOT/std HEW_MEASURE_TIMINGS=1 target/debug/hew check $FIXTURE 2>&1 | grep 'mir derivation '" >&2
    exit 1
fi

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
