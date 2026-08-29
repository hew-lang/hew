#!/usr/bin/env bash
# o2-differential.sh — the -O0-vs-O2 differential-exec parity gate.
#
# The no-miscompile oracle for the LLVM middle-end pipeline: every compiled
# `.hew` program must behave IDENTICALLY at -O0 and -O2. The optimizer may
# reshape IR arbitrarily; it must never change what a program DOES.
#
# WHAT it proves: RUNTIME identity (path-qualified identity + per-test outcome +
# semantic failure kind), NOT IR/ll identity — `default<O2>` reshapes IR by
# design, so byte identity is meaningless here. A divergence between the O0 and
# O2 run IS a miscompile and a FULL STOP: root-cause it to the upstream UB (a
# missed lifetime marker, a wrong ABI attribute, an aliasing violation), NEVER
# weaken the pipeline.
#
# HOW: runs `hew test tests/hew/` twice over the same binary — once at the O0
# default, once with `HEW_OPT_LEVEL=2` forcing the whole corpus through the O2
# pipeline (the env FLOOR that raises O0->O2 without a per-subcommand flag). The
# two complete per-test outcome sets must be identical.
#
# This gate is re-run by every future optimization lane (PGO, LTO, target-cpu)
# as the permanent guard that the optimization did not change behaviour.
#
# Usage:
#   scripts/o2-differential.sh            # uses Cargo's resolved debug output (or HEW_BIN)
#   HEW_BIN=/path/to/hew scripts/o2-differential.sh
#   scripts/o2-differential.sh --tests-dir <dir>
#   scripts/o2-differential.sh --o0-outcomes <path>
#
# --o0-outcomes <path> skips this gate's own O0 pass and reuses a pre-captured
# outcome file instead (produced by scripts/corpus-ratchet.sh hew-suite's
# --emit-o0-outcomes, which runs the same corpus and compiler at O0 one step
# earlier in the same CI job). This is the CI-only
# fast path; a
# standalone invocation (no flag) always runs its own O0 pass so the gate
# behaves identically when run outside the ratchet→differential handoff. The
# file must exist and be non-empty when the flag is given — a missing/empty
# handoff file is a wiring bug, not a reason to silently fall back and mask it.
#
# NONEMPTY OUTCOMES: the comparison above is `[[ "$O0_OUTCOMES" == "$O2_OUTCOMES" ]]`,
# and TWO EMPTY SETS MATCH. A wrong directory, a renamed fixture set or a build
# that produced no tests would otherwise print "PASSED (0 tests)" and stay green
# forever while proving nothing about miscompiles. The default corpus rejects
# an empty outcome set, and custom corpora accept a caller-supplied minimum. A
# caller that points the
# gate somewhere else must say how big that somewhere else is.
#
#   scripts/o2-differential.sh --tests-dir <dir> --min-outcomes <n>

set -uo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
# shellcheck source=scripts/lib/corpus-nonempty.sh
# shellcheck disable=SC1091
source "$REPO_ROOT/scripts/lib/corpus-nonempty.sh"
# shellcheck source=scripts/lib/cargo-output-dir.sh
# shellcheck disable=SC1091
source "$REPO_ROOT/scripts/lib/cargo-output-dir.sh"
HEW_BIN="${HEW_BIN:-$(cargo_debug_dir "$REPO_ROOT")/hew}"
HEW_JUNIT_PY="$REPO_ROOT/scripts/lib/hew_junit.py"
DEFAULT_TESTS_DIR="$REPO_ROOT/tests/hew"
TESTS_DIR="$DEFAULT_TESTS_DIR"
O0_OUTCOMES_FILE=""
MIN_OUTCOMES=""
OUTCOME_TMP=""
if ! OUTCOME_TMP="$(mktemp -d /tmp/hew-o2-differential.XXXXXX)"; then
    echo "error: cannot create differential-report directory" >&2
    exit 1
fi
trap 'rm -rf "$OUTCOME_TMP"' EXIT

while [[ $# -gt 0 ]]; do
    case "$1" in
    --tests-dir)
        shift
        TESTS_DIR="$1"
        shift
        ;;
    --o0-outcomes)
        shift
        O0_OUTCOMES_FILE="$1"
        shift
        ;;
    --min-outcomes)
        shift
        MIN_OUTCOMES="$1"
        shift
        ;;
    --help | -h)
        grep '^#' "$0" | sed 's/^# \{0,1\}//'
        exit 0
        ;;
    *)
        echo "error: unknown argument: $1" >&2
        exit 1
        ;;
    esac
done

if [[ "$TESTS_DIR" != "$DEFAULT_TESTS_DIR" && -z "$MIN_OUTCOMES" ]]; then
    echo "error: --tests-dir points at a corpus this gate has no floor for; pass --min-outcomes <n>" >&2
    echo "       The O0/O2 comparison passes vacuously on an empty outcome set, so every" >&2
    echo "       corpus this gate runs over must declare how many outcomes it expects." >&2
    exit 1
fi
if [[ -n "$MIN_OUTCOMES" && ! "$MIN_OUTCOMES" =~ ^[1-9][0-9]*$ ]]; then
    echo "error: --min-outcomes must be a positive integer, got '$MIN_OUTCOMES'" >&2
    exit 1
fi

if [[ ! -x "$HEW_BIN" ]]; then
    echo "error: hew binary not found/executable at $HEW_BIN" >&2
    echo "       Build it first (make hew), or set HEW_BIN." >&2
    exit 1
fi
if [[ ! -d "$TESTS_DIR" ]]; then
    echo "error: tests dir not found: $TESTS_DIR" >&2
    exit 1
fi

# Extract sorted path-qualified outcomes from the same JUnit schema CI reads.
# The full per-test outcome set (not just the count) is the comparison key — a
# miscompile that flips one test from PASS to FAIL, changes the failure kind,
# or changes which test fails is caught, including swaps between same-named
# tests in different source files.
run_outcomes() {
    local opt_env="$1"
    local label="$2"
    local report="$OUTCOME_TMP/$label.xml"
    local stderr="$OUTCOME_TMP/$label.stderr"
    local parsed=""
    local rc=0
    env HEW_OPT_LEVEL="$opt_env" "$HEW_BIN" test "$TESTS_DIR" --format junit \
        >"$report" 2>"$stderr" || rc=$?
    if ! parsed="$("${PYTHON:-python3}" "$HEW_JUNIT_PY" --runner-exit "$rc" "$report")"; then
        echo "__INVALID_REPORT__"
        cat "$stderr" >&2
        return
    fi
    printf '%s\n' "$parsed" |
        awk -F'\t' '$1 != "__SUMMARY__" {
            line = "test " $2 " ... " $1
            if ($3 != "") line = line "\t" $3
            print line
        }' |
        sort
}

echo "==> O2 differential-exec parity gate"
echo "    binary:  $HEW_BIN"
echo "    corpus:  $TESTS_DIR"
echo ""

if [[ -n "$O0_OUTCOMES_FILE" ]]; then
    echo "--> baseline (O0) reused from pre-captured outcomes: $O0_OUTCOMES_FILE"
    if [[ ! -s "$O0_OUTCOMES_FILE" ]]; then
        echo "error: --o0-outcomes file missing or empty: $O0_OUTCOMES_FILE" >&2
        echo "       The ratchet->differential handoff did not produce a usable file." >&2
        echo "       Refusing to silently fall back to a fresh O0 run for a caller that" >&2
        echo "       explicitly requested the handoff — fix the wiring instead." >&2
        exit 1
    fi
    O0_OUTCOMES="$(sort "$O0_OUTCOMES_FILE")"
else
    echo "--> baseline run (O0, default)"
    O0_OUTCOMES="$(run_outcomes 0 o0)"
fi
echo "--> optimized run (O2, HEW_OPT_LEVEL=2)"
O2_OUTCOMES="$(run_outcomes 2 o2)"

if [[ "$O0_OUTCOMES" == "__INVALID_REPORT__" || "$O2_OUTCOMES" == "__INVALID_REPORT__" ]]; then
    echo ""
    echo "==> Differential gate: FAILED (invalid or incomplete JUnit result)"
    exit 1
fi

# Floor the enumeration BEFORE comparing: an empty or shrunken outcome set
# satisfies the comparison below without proving anything.
n_o0="$(printf '%s\n' "$O0_OUTCOMES" | grep -c . || true)"
n_o2="$(printf '%s\n' "$O2_OUTCOMES" | grep -c . || true)"
echo ""
if [[ -n "$MIN_OUTCOMES" ]]; then
    for pair in "O0:$n_o0" "O2:$n_o2"; do
        if [[ "${pair#*:}" -lt "$MIN_OUTCOMES" ]]; then
            echo "==> Differential gate: FAILED — ${pair%%:*} run produced ${pair#*:} outcome(s), below the" >&2
            echo "    caller-declared floor of $MIN_OUTCOMES for $TESTS_DIR." >&2
            exit 1
        fi
    done
    echo "outcome floor OK: O0=$n_o0 O2=$n_o2 (caller floor $MIN_OUTCOMES)"
else
    corpus_nonempty_assert "o2-differential-outcomes" "$n_o0" "O0 run" || exit 1
    corpus_nonempty_assert "o2-differential-outcomes" "$n_o2" "O2 run" || exit 1
fi

if [[ "$O0_OUTCOMES" == "$O2_OUTCOMES" ]]; then
    echo ""
    echo "==> Differential gate: PASSED — O0 and O2 outcome sets identical ($n_o0 tests)"
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
