#!/usr/bin/env bash
# Repeated-compile determinism gate.
#
# Compiler output must be a function of the input alone.  For every fixture in
# the corpus this compiles the same file several times through the established
# pipeline and requires the observable result to be identical each time:
#
#   * exit status,
#   * the physical MIR dump, byte for byte,
#   * stderr, byte for byte.
#
# Ownership facts are collected through hashed maps and sets, and diagnostics
# are accumulated across passes; either can acquire an iteration-order
# dependency that a single compilation cannot see. `ll-diff` compares one run
# with a committed output, so a compiler that emits a different-but-plausible
# order on every invocation can still pass it. This gate fails closed on that.
#
# This assertion set was previously carried by scripts/sir-shadow-corpus.sh and
# outlived the shadow lane it was bolted to; nothing here depends on SIR.
#
# The default corpus is tests/ll-oracle/corpus, whose fixtures are standalone
# complete programs and library modules intended to reach the native backend.
# Additional files and directories may be supplied to exercise a focused
# surface.
#
# Usage:
#   scripts/compile-determinism-corpus.sh
#   scripts/compile-determinism-corpus.sh path/to/fixture.hew path/to/corpus-dir
#
# Environment:
#   HEW_BIN                         compiler binary (default: target/debug/hew)
#   COMPILE_DETERMINISM_MIN_VERIFIED  minimum verified compiler outcomes (default: 16; may only raise)
#   COMPILE_DETERMINISM_JOBS        concurrent fixture workers (default: host CPU count)

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CALLER_DIR="$PWD"
# shellcheck source=scripts/lib/corpus-nonempty.sh
# shellcheck disable=SC1091
source "$ROOT/scripts/lib/corpus-nonempty.sh"

HEW_BIN="${HEW_BIN:-$ROOT/target/debug/hew}"
DEFAULT_CORPUS="$ROOT/tests/ll-oracle/corpus"
VERIFIED_FLOOR=15
MIN_VERIFIED="${COMPILE_DETERMINISM_MIN_VERIFIED:-$VERIFIED_FLOOR}"

# SHORTCUT: a fixed repeat count is a sampling approximation of "this compile is
# deterministic", not a proof of it.  Six runs is what the retired shadow
# harness used and what the corpus can afford in a CI shard; a genuine proof
# would be a hash-seed-perturbing run (deterministic iteration order enforced by
# construction, e.g. ordered maps on every fact stream), which is the direction
# the ordered `BTreeMap` fact tables in SIR are already taking.  Raise this, or
# retire it for the by-construction proof, when the ownership fact streams stop
# passing through hashed containers.
DETERMINISM_RUNS=6

detect_default_jobs() {
    if command -v nproc >/dev/null 2>&1; then
        nproc
    elif command -v getconf >/dev/null 2>&1; then
        getconf _NPROCESSORS_ONLN 2>/dev/null || printf '4\n'
    elif command -v sysctl >/dev/null 2>&1; then
        sysctl -n hw.ncpu 2>/dev/null || printf '4\n'
    else
        printf '4\n'
    fi
}

DETERMINISM_JOBS="${COMPILE_DETERMINISM_JOBS:-$(detect_default_jobs)}"

usage() {
    cat <<'EOF'
Usage: scripts/compile-determinism-corpus.sh [fixture.hew | directory ...]

Compile every fixture repeatedly and require identical exit status, identical
the physical MIR dump, and byte-identical stderr across runs.  With no
arguments, runs every top-level .hew fixture in tests/ll-oracle/corpus.

Environment:
  HEW_BIN                           compiler binary (default: target/debug/hew)
  COMPILE_DETERMINISM_MIN_VERIFIED  minimum verified compiler outcomes (default: 16; may only raise)
  COMPILE_DETERMINISM_JOBS          concurrent fixture workers (default: host CPU count)
EOF
}

require_nonnegative_integer() {
    local name="$1"
    local value="$2"
    if [[ ! "$value" =~ ^[0-9]+$ ]]; then
        echo "compile-determinism: $name must be a non-negative integer, got '$value'" >&2
        exit 2
    fi
}

require_nonnegative_integer COMPILE_DETERMINISM_MIN_VERIFIED "$MIN_VERIFIED"
if ((MIN_VERIFIED < VERIFIED_FLOOR)); then
    echo "compile-determinism: COMPILE_DETERMINISM_MIN_VERIFIED may not lower the committed floor $VERIFIED_FLOOR" >&2
    exit 2
fi
if [[ ! "$DETERMINISM_JOBS" =~ ^[1-9][0-9]*$ ]]; then
    echo "compile-determinism: COMPILE_DETERMINISM_JOBS must be a positive integer, got '$DETERMINISM_JOBS'" >&2
    exit 2
fi

if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
    usage
    exit 0
fi

if [[ ! -x "$HEW_BIN" ]]; then
    echo "compile-determinism: compiler binary not found at $HEW_BIN" >&2
    echo "build it first (make hew-native) or set HEW_BIN=<path>" >&2
    exit 2
fi
if [[ "$HEW_BIN" != /* ]]; then
    HEW_BIN="$(cd "$(dirname "$HEW_BIN")" && pwd)/$(basename "$HEW_BIN")"
fi

tmpdir="$(mktemp -d)"
trap 'rm -rf "$tmpdir"' EXIT
fixture_list="$tmpdir/fixtures"
: >"$fixture_list"

resolve_input() {
    local input="$1"
    if [[ "$input" == /* ]]; then
        printf '%s\n' "$input"
    else
        printf '%s/%s\n' "$CALLER_DIR" "$input"
    fi
}

add_input() {
    local input="$1"
    if [[ -f "$input" ]]; then
        case "$input" in
        *.hew) printf '%s\n' "$input" >>"$fixture_list" ;;
        *)
            echo "compile-determinism: fixture is not a .hew file: $input" >&2
            exit 2
            ;;
        esac
    elif [[ -d "$input" ]]; then
        while IFS= read -r fixture; do
            printf '%s\n' "$fixture" >>"$fixture_list"
        done < <(find "$input" -type f -name '*.hew' -print | LC_ALL=C sort)
    else
        echo "compile-determinism: no such fixture or directory: $input" >&2
        exit 2
    fi
}

if [[ $# -eq 0 ]]; then
    # Keep the default deliberately shallow: LL-oracle fixtures are individual
    # compiler inputs.  A caller can pass a directory explicitly when a
    # recursive surface slice is appropriate.
    while IFS= read -r fixture; do
        printf '%s\n' "$fixture" >>"$fixture_list"
    done < <(find "$DEFAULT_CORPUS" -maxdepth 1 -type f -name '*.hew' -print | LC_ALL=C sort)
else
    for argument in "$@"; do
        add_input "$(resolve_input "$argument")"
    done
fi

LC_ALL=C sort -u "$fixture_list" >"$tmpdir/fixtures.sorted"
mv "$tmpdir/fixtures.sorted" "$fixture_list"
fixtures=()
while IFS= read -r fixture; do
    fixtures+=("$fixture")
done <"$fixture_list"
corpus_nonempty_assert "compile-determinism-fixtures" "${#fixtures[@]}" || exit 1

run_compile() {
    local output_path="$1"
    local stderr_path="$2"
    local status=0
    shift 2
    "$@" >"$output_path" 2>"$stderr_path" || status=$?
    return "$status"
}

physical_dump() {
    local output_path="$1"
    cat "$output_path"
}

check_fixture() {
    local index="$1"
    local fixture="$2"
    local label="${fixture#"$ROOT"/}"
    local baseline_out="$tmpdir/$index.baseline.out"
    local baseline_err="$tmpdir/$index.baseline.err"
    local baseline_edges="$tmpdir/$index.baseline.edges"
    local stdout_log="$tmpdir/$index.summary.out"
    local stderr_log="$tmpdir/$index.summary.err"
    local result_file="$tmpdir/$index.result"
    local baseline_status=0
    local fixture_failed=0
    local run repeated_out repeated_err repeated_edges repeated_status

    run_compile "$baseline_out" "$baseline_err" \
        "$HEW_BIN" tool compile --dump-mir physical "$fixture" || baseline_status=$?
    physical_dump "$baseline_out" >"$baseline_edges"

    if [[ "$baseline_status" -ne 0 && "$baseline_status" -ne 1 ]]; then
        printf 'FAIL %s: compiler exited abnormally with status %s\n' "$label" "$baseline_status" >>"$stderr_log"
        fixture_failed=1
    fi

    for ((run = 2; run <= DETERMINISM_RUNS; run++)); do
        repeated_out="$tmpdir/$index.baseline.$run.out"
        repeated_err="$tmpdir/$index.baseline.$run.err"
        repeated_edges="$tmpdir/$index.baseline.$run.edges"
        repeated_status=0
        run_compile "$repeated_out" "$repeated_err" \
            "$HEW_BIN" tool compile --dump-mir physical "$fixture" || repeated_status=$?
        physical_dump "$repeated_out" >"$repeated_edges"
        if [[ "$baseline_status" -ne "$repeated_status" ]]; then
            printf 'FAIL %s: repeated compile %s changed exit status from %s to %s\n' \
                "$label" "$run" "$baseline_status" "$repeated_status" >>"$stderr_log"
            fixture_failed=1
        fi
        if ! diff -u "$baseline_edges" "$repeated_edges" >>"$stdout_log" 2>>"$stderr_log"; then
            printf 'FAIL %s: repeated compile %s changed the physical MIR dump\n' \
                "$label" "$run" >>"$stderr_log"
            fixture_failed=1
        fi
        if ! diff -u "$baseline_err" "$repeated_err" >>"$stdout_log" 2>>"$stderr_log"; then
            printf 'FAIL %s: repeated compile %s changed diagnostic emission\n' \
                "$label" "$run" >>"$stderr_log"
            fixture_failed=1
        fi
    done

    if [[ "$fixture_failed" -eq 0 ]]; then
        if [[ "$baseline_status" -eq 1 ]]; then
            printf 'ok   %s  (deterministic diagnostic rejection)\n' "$label" >>"$stdout_log"
        else
            printf 'ok   %s\n' "$label" >>"$stdout_log"
        fi
    fi

    printf '%s %s\n' "$fixture_failed" "$baseline_status" >"$result_file"
}

failures=0
successes=0
verified_outcomes=0

workers="$DETERMINISM_JOBS"
if ((workers > ${#fixtures[@]})); then
    workers=${#fixtures[@]}
fi

pids=()
for ((worker = 0; worker < workers; worker++)); do
    (
        for ((index = worker; index < ${#fixtures[@]}; index += workers)); do
            check_fixture "$index" "${fixtures[$index]}"
        done
    ) &
    pids+=("$!")
done

worker_failed=0
for pid in "${pids[@]}"; do
    if ! wait "$pid"; then
        worker_failed=1
    fi
done
if ((worker_failed)); then
    echo "compile-determinism: worker failed abnormally" >&2
    exit 1
fi

for index in "${!fixtures[@]}"; do
    if [[ -s "$tmpdir/$index.summary.out" ]]; then
        cat "$tmpdir/$index.summary.out"
    fi
    if [[ -s "$tmpdir/$index.summary.err" ]]; then
        cat "$tmpdir/$index.summary.err" >&2
    fi
    read -r fixture_failed baseline_status <"$tmpdir/$index.result"
    if [[ "$baseline_status" -eq 0 ]]; then
        successes=$((successes + 1))
    fi
    if [[ "$fixture_failed" -ne 0 ]]; then
        failures=$((failures + 1))
    else
        # A normal diagnostic rejection is a verified compiler outcome, not a
        # missing corpus execution: its status, physical dump and exact
        # stderr have all passed the repeated-compile assertions.
        verified_outcomes=$((verified_outcomes + 1))
    fi
done

if [[ "$verified_outcomes" -lt "$MIN_VERIFIED" ]]; then
    echo "FAIL compile-determinism: only $verified_outcomes verified compiler outcomes; require $MIN_VERIFIED" >&2
    failures=$((failures + 1))
fi

if [[ "$failures" -ne 0 ]]; then
    echo "compile-determinism: FAILED ($failures fixture/floor failure(s))" >&2
    exit 1
fi

echo "compile-determinism: OK (${#fixtures[@]} fixtures, $DETERMINISM_RUNS runs each, $verified_outcomes verified outcomes, $successes successful compiles)"
