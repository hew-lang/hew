#!/usr/bin/env bash
# o2-differential.sh — the -O0-vs-O2 differential-exec parity gate (RC9).
#
# The no-miscompile oracle for the LLVM middle-end pipeline: every compiled
# `.hew` program must behave IDENTICALLY at -O0 and -O2. The optimizer may
# reshape IR arbitrarily; it must never change what a program DOES.
#
# WHAT it proves: RUNTIME identity (test pass/fail set + per-test outcome),
# NOT IR/ll identity — `default<O2>` reshapes IR by design, so byte identity is
# meaningless here. A divergence between the O0 and O2 run IS a miscompile and a
# FULL STOP: root-cause it to the upstream UB (a missed lifetime marker, a wrong
# ABI attribute, an aliasing violation), NEVER weaken the pipeline.
#
# HOW: runs `hew test tests/hew/` twice over the same binary — once at the O0
# default, once with `HEW_OPT_LEVEL=2` forcing the whole corpus through the O2
# pipeline (the env FLOOR that raises O0->O2 without a per-subcommand flag). The
# two failing-test sets must be identical.
#
# This gate is re-run by every future optimization lane (PGO, LTO, target-cpu)
# as the permanent guard that the optimization did not change behaviour.
#
# Usage:
#   scripts/o2-differential.sh            # uses target/debug/hew (or HEW_BIN)
#   HEW_BIN=/path/to/hew scripts/o2-differential.sh
#   scripts/o2-differential.sh --tests-dir <dir>

set -uo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
HEW_BIN="${HEW_BIN:-$REPO_ROOT/target/debug/hew}"
TESTS_DIR="$REPO_ROOT/tests/hew"

while [[ $# -gt 0 ]]; do
    case "$1" in
        --tests-dir) shift; TESTS_DIR="$1"; shift ;;
        --help|-h)
            grep '^#' "$0" | sed 's/^# \{0,1\}//'
            exit 0 ;;
        *) echo "error: unknown argument: $1" >&2; exit 1 ;;
    esac
done

if [[ ! -x "$HEW_BIN" ]]; then
    echo "error: hew binary not found/executable at $HEW_BIN" >&2
    echo "       Build it first (make hew), or set HEW_BIN." >&2
    exit 1
fi
if [[ ! -d "$TESTS_DIR" ]]; then
    echo "error: tests dir not found: $TESTS_DIR" >&2
    exit 1
fi

# Extract the sorted "test <name> ... PASSED|FAILED" outcome lines from a
# `hew test` run, stripping ANSI. The full per-test outcome set (not just the
# count) is the comparison key — a miscompile that flips one test from PASS to
# FAIL (or vice versa, or changes which tests fail) is caught.
run_outcomes() {
    local opt_env="$1"
    local raw
    raw="$(env HEW_OPT_LEVEL="$opt_env" "$HEW_BIN" test "$TESTS_DIR" 2>&1)" || true
    # Fail closed if there is no summary — a runner crash must not read as match.
    local clean
    clean="$(printf '%s\n' "$raw" | sed $'s/\x1b\\[[0-9;]*m//g')"
    if ! printf '%s\n' "$clean" | grep -q "^test result:"; then
        echo "__NO_SUMMARY__"
        printf '%s\n' "$raw" >&2
        return
    fi
    printf '%s\n' "$clean" \
        | grep -E "^test .* \.\.\. (ok|PASSED|FAILED|ignored)" \
        | sort
}

echo "==> O2 differential-exec parity gate"
echo "    binary:  $HEW_BIN"
echo "    corpus:  $TESTS_DIR"
echo ""

echo "--> baseline run (O0, default)"
O0_OUTCOMES="$(run_outcomes 0)"
echo "--> optimized run (O2, HEW_OPT_LEVEL=2)"
O2_OUTCOMES="$(run_outcomes 2)"

if [[ "$O0_OUTCOMES" == "__NO_SUMMARY__" || "$O2_OUTCOMES" == "__NO_SUMMARY__" ]]; then
    echo ""
    echo "==> Differential gate: FAILED (no test-result summary; runner crash)"
    exit 1
fi

if [[ "$O0_OUTCOMES" == "$O2_OUTCOMES" ]]; then
    n="$(printf '%s\n' "$O0_OUTCOMES" | grep -c . || true)"
    echo ""
    echo "==> Differential gate: PASSED — O0 and O2 outcome sets identical ($n tests)"
    exit 0
fi

echo ""
echo "==> Differential gate: FAILED — O0 vs O2 outcome DIVERGENCE (a miscompile):"
echo ""
diff <(printf '%s\n' "$O0_OUTCOMES") <(printf '%s\n' "$O2_OUTCOMES")
echo ""
echo "  A divergence IS a miscompile. Root-cause to the upstream UB"
echo "  (missed lifetime marker / wrong ABI attribute / aliasing violation)."
echo "  NEVER weaken the pipeline to make this pass."
exit 1
