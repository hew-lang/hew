#!/usr/bin/env bash
# hew-suite-ratchet.sh — Run `hew test tests/hew/` with a ratcheted expected-failures list.
#
# Behaviour:
#   - Exits 0 if the set of failing tests exactly matches the list in
#     scripts/hew-suite-expected-failures.txt.
#   - Exits 1 if any NEW failure appears (unexpected regression).
#   - Exits 1 if any LISTED failure no longer fails (unexpected fix — delete
#     the entry from the list to accept the green).
#
# WHY: The Hew test suite is ~700 tests converging toward green via in-flight
# lanes.  Gating on zero failures would block the integration branch.  Gating
# on nothing would silently accept regressions.  This ratchet is the middle
# path: known failures are explicitly tracked, anything else is a hard fail.
#
# Parses `hew test --format junit` output (via scripts/lib/hew_junit.py)
# rather than regex-matching the human-readable text — a cosmetic text-format
# change can no longer silently break pass/fail extraction, and the same
# JUnit report this script parses is the one uploaded to the CI checks UI.
#
# WHEN OBSOLETE: When the list is empty and all tests are green, drop the
# ratchet wrapper and have the gate target run `hew test tests/hew/` directly.
#
# REAL SOLUTION: Fix the underlying failures (tracked per entry in the list).
#
# Usage:
#   scripts/hew-suite-ratchet.sh [--help]
#   scripts/hew-suite-ratchet.sh [--expected-failures <path>]
#   scripts/hew-suite-ratchet.sh [--emit-o0-outcomes <path>]
#   scripts/hew-suite-ratchet.sh [--junit-output <path>]
#
# Options:
#   --expected-failures <path>   Override default expected-failures file path.
#   --emit-o0-outcomes <path>    Write the sorted per-test outcome lines (the
#                                same "test <name> ... ok|PASSED|FAILED|ignored"
#                                format scripts/o2-differential.sh compares) to
#                                <path>. This script's `hew test` run is O0 (its
#                                default opt level), so a CI job can hand the
#                                captured file to o2-differential.sh's
#                                --o0-outcomes flag instead of re-running the
#                                identical O0 pass a second time.
#   --junit-output <path>        Where to write the `hew test --format junit`
#                                report this script parses. Default:
#                                target/hew-test-reports/hew-suite-ratchet.xml
#                                — a stable path a CI job's upload step reads,
#                                the same way it reads target/nextest/*/junit.xml.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
# shellcheck source=scripts/lib/line-set.sh
# shellcheck disable=SC1091
source "$REPO_ROOT/scripts/lib/line-set.sh"
# shellcheck source=scripts/lib/corpus-floor.sh
# shellcheck disable=SC1091
source "$REPO_ROOT/scripts/lib/corpus-floor.sh"
# shellcheck source=scripts/lib/cargo-output-dir.sh
# shellcheck disable=SC1091
source "$REPO_ROOT/scripts/lib/cargo-output-dir.sh"
EXPECTED_FAILURES_FILE="$REPO_ROOT/scripts/hew-suite-expected-failures.txt"
# HEW_BIN is overridable for parser tests (point it at a stub that replays
# captured runner output); production callers use the default.
HEW_BIN="${HEW_BIN:-$(cargo_debug_dir "$REPO_ROOT")/hew}"
TESTS_DIR="$REPO_ROOT/tests/hew"
EMIT_O0_OUTCOMES_FILE=""
JUNIT_OUTPUT="$REPO_ROOT/target/hew-test-reports/hew-suite-ratchet.xml"
HEW_JUNIT_PY="$REPO_ROOT/scripts/lib/hew_junit.py"

usage() {
    cat <<'EOF'
Usage: scripts/hew-suite-ratchet.sh [--expected-failures <path>] [--emit-o0-outcomes <path>]

Run `hew test tests/hew/` and assert the result matches the tracked expected-failures list.

Exits 0 if the failing test set exactly matches the list (or both are empty).
Exits 1 on any unexpected failure (not in list) or unexpected pass (was in list, now passes).

Options:
  --expected-failures <path>   Override the default expected-failures file.
                               Default: scripts/hew-suite-expected-failures.txt
  --emit-o0-outcomes <path>    Write the full sorted per-test outcome set to <path>
                               for a downstream gate to reuse instead of re-running.
EOF
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --expected-failures)
            shift
            [[ $# -gt 0 ]] || { echo "error: --expected-failures requires a path" >&2; exit 1; }
            EXPECTED_FAILURES_FILE="$1"
            shift
            ;;
        --emit-o0-outcomes)
            shift
            [[ $# -gt 0 ]] || { echo "error: --emit-o0-outcomes requires a path" >&2; exit 1; }
            EMIT_O0_OUTCOMES_FILE="$1"
            shift
            ;;
        --junit-output)
            shift
            [[ $# -gt 0 ]] || { echo "error: --junit-output requires a path" >&2; exit 1; }
            JUNIT_OUTPUT="$1"
            shift
            ;;
        --help|-h)
            usage
            exit 0
            ;;
        *)
            echo "error: unknown argument: $1" >&2
            usage >&2
            exit 1
            ;;
    esac
done

if [[ ! -f "$HEW_BIN" ]]; then
    echo "error: hew binary not found at $HEW_BIN" >&2
    echo "       Run: cargo build -p hew-cli" >&2
    exit 1
fi

if [[ ! -d "$TESTS_DIR" ]]; then
    echo "error: tests/hew/ directory not found" >&2
    exit 1
fi

if [[ ! -f "$EXPECTED_FAILURES_FILE" ]]; then
    echo "error: expected-failures file not found: $EXPECTED_FAILURES_FILE" >&2
    exit 1
fi

# Read the expected failures list (ignore blank lines and # comments).
# Store as newline-separated string for portable bash 3 compatibility.
EXPECTED_STR=""
while IFS= read -r line; do
    # Strip comments and leading/trailing whitespace.
    name="${line%%#*}"
    name="${name#"${name%%[! ]*}"}"  # ltrim
    name="${name%"${name##*[! ]}"}"  # rtrim
    [[ -z "$name" ]] && continue
    # Use only the first field (path/name before any whitespace).
    name="${name%% *}"
    [[ -z "$name" ]] && continue
    EXPECTED_STR="${EXPECTED_STR}${name}"$'\n'
done < "$EXPECTED_FAILURES_FILE"

# Run hew test, writing a JUnit report (exit code is non-zero when tests
# fail; we determine pass/fail from the parsed report, not the exit code).
mkdir -p "$(dirname "$JUNIT_OUTPUT")"
STDERR_FILE="$(mktemp /tmp/hew-suite-ratchet-stderr.XXXXXX)"
trap 'rm -f "$STDERR_FILE"' EXIT
"$HEW_BIN" test "$TESTS_DIR" --format junit > "$JUNIT_OUTPUT" 2> "$STDERR_FILE" || true

# Fail closed if the runner produced no report at all: a runner crash before
# any output must never read as an empty (thus vacuously matching) run.
if [[ ! -s "$JUNIT_OUTPUT" ]]; then
    echo "error: hew test produced no JUnit report at $JUNIT_OUTPUT; refusing to ratchet" >&2
    echo "==> stderr from the run:" >&2
    cat "$STDERR_FILE" >&2
    exit 1
fi

# scripts/lib/hew_junit.py parses the report and prints one "<status>\t<name>"
# line per test plus a trailing "__SUMMARY__\t<total>\t<failures>\t<skipped>"
# line. It exits non-zero (with a diagnostic on stderr) on malformed XML or a
# <testsuites> whose failures attribute disagrees with its own <failure>
# element count — fail closed on either, the same "refuse to ratchet against
# an inconsistent run" posture the old text-parser's checks had.
PARSED=""
if ! PARSED="$(python3 "$HEW_JUNIT_PY" "$JUNIT_OUTPUT")"; then
    echo "error: could not parse JUnit report at $JUNIT_OUTPUT; refusing to ratchet" >&2
    echo "==> stderr from the run:" >&2
    cat "$STDERR_FILE" >&2
    exit 1
fi

SUMMARY_LINE="$(printf '%s\n' "$PARSED" | grep '^__SUMMARY__' || true)"
if [[ -z "$SUMMARY_LINE" ]]; then
    echo "error: hew_junit.py produced no __SUMMARY__ line; refusing to ratchet" >&2
    exit 1
fi
IFS=$'\t' read -r _ report_total report_failures _report_skipped <<< "$SUMMARY_LINE"

# Emit the full per-test outcome set for the O2-differential gate to reuse as
# its O0 baseline (C1: dedup the identical O0 re-run), reconstructed in the
# "test <name> ... ok|PASSED|FAILED|ignored" text form
# scripts/o2-differential.sh's run_outcomes() already extracts from a plain
# `hew test` run, so that consumer needs no change. Written before the
# ratchet verdict is known — the outcome set is valid regardless of whether
# the ratchet itself passes or fails.
if [[ -n "$EMIT_O0_OUTCOMES_FILE" ]]; then
    printf '%s\n' "$PARSED" \
        | awk -F'\t' '$1 != "__SUMMARY__" { print "test " $2 " ... " $1 }' \
        | sort > "$EMIT_O0_OUTCOMES_FILE"
fi

# Floor the size of the run itself. The ratchet compares a failing-test set
# against the expected list; a run that executed no tests at all reports an
# empty failing set, which agrees with an empty expected list and would ratchet
# green over nothing.
if ! corpus_floor_assert "hew-suite-tests" "$report_total"; then
    cat "$STDERR_FILE" >&2
    exit 1
fi

# Extract names of failing tests.
ACTUAL_STR=""
while IFS=$'\t' read -r status name; do
    [[ "$status" == "FAILED" ]] || continue
    [[ -n "$name" ]] && ACTUAL_STR="${ACTUAL_STR}${name}"$'\n'
done <<< "$(printf '%s\n' "$PARSED" | grep -v '^__SUMMARY__')"

parsed_failed=0
if [[ -n "$ACTUAL_STR" ]]; then
    parsed_failed="$(line_set_count "$ACTUAL_STR")"
fi
if [[ "$parsed_failed" -ne "$report_failures" ]]; then
    echo "error: parsed $parsed_failed FAILED test(s) but report summary reports $report_failures failed; refusing to ratchet" >&2
    exit 1
fi

# Sort actual for deterministic display.
sorted_actual=""
if [[ -n "$ACTUAL_STR" ]]; then
    sorted_actual="$(printf '%s' "$ACTUAL_STR" | sort)"
fi

# Count entries.
count_expected=0
if [[ -n "$EXPECTED_STR" ]]; then
    count_expected="$(line_set_count "$EXPECTED_STR")"
fi

count_actual=0
if [[ -n "$ACTUAL_STR" ]]; then
    count_actual="$(line_set_count "$ACTUAL_STR")"
fi

# Find unexpected failures (in actual but not in expected).
unexpected_failures=""
while IFS= read -r name; do
    [[ -z "$name" ]] && continue
    if ! line_set_contains "$EXPECTED_STR" "$name"; then
        unexpected_failures="${unexpected_failures}${name}"$'\n'
    fi
done <<< "$ACTUAL_STR"

# Find unexpected passes (in expected but not in actual).
unexpected_passes=""
while IFS= read -r name; do
    [[ -z "$name" ]] && continue
    if ! line_set_contains "$ACTUAL_STR" "$name"; then
        unexpected_passes="${unexpected_passes}${name}"$'\n'
    fi
done <<< "$EXPECTED_STR"

echo "==> Hew suite ratchet"
echo "Expected failures: $count_expected"
echo "Actual failures:   $count_actual"
echo ""

count_unexpected_fail=0
[[ -n "$unexpected_failures" ]] && count_unexpected_fail="$(line_set_count "$unexpected_failures")"

count_unexpected_pass=0
[[ -n "$unexpected_passes" ]] && count_unexpected_pass="$(line_set_count "$unexpected_passes")"

if [[ $count_unexpected_fail -eq 0 && $count_unexpected_pass -eq 0 ]]; then
    if [[ $count_actual -eq 0 ]]; then
        echo "All tests passed. Remove the expected-failures file entries when the list is empty."
    else
        echo "Expected failure set matches. Tracked failures: $count_actual"
        while IFS= read -r name; do
            [[ -z "$name" ]] && continue
            echo "  - $name"
        done <<< "$sorted_actual"
    fi
    echo ""
    echo "==> Ratchet: PASSED"
    exit 0
fi

# Report problems.
if [[ $count_unexpected_fail -gt 0 ]]; then
    echo "RATCHET FAIL: $count_unexpected_fail UNEXPECTED failure(s) — not in expected list:"
    while IFS= read -r name; do
        [[ -z "$name" ]] && continue
        echo "  UNEXPECTED: $name"
    done <<< "$unexpected_failures"
    echo ""
    echo "  To accept these as known failures, add them to:"
    echo "  $EXPECTED_FAILURES_FILE"
    echo ""
fi

if [[ $count_unexpected_pass -gt 0 ]]; then
    echo "RATCHET FAIL: $count_unexpected_pass listed failure(s) now PASS — remove from list:"
    while IFS= read -r name; do
        [[ -z "$name" ]] && continue
        echo "  NOW-PASSES: $name"
    done <<< "$unexpected_passes"
    echo ""
    echo "  Delete these lines from:"
    echo "  $EXPECTED_FAILURES_FILE"
    echo "  (Do not restore a failing entry to make this green — fix the test.)"
    echo ""
fi

# Print the run's stderr (build/FFI errors, warnings) and point at the full
# JUnit report — the same report a CI job's upload step reads into the
# checks UI, so failure detail is one click away there too.
echo "==> stderr from the run:"
cat "$STDERR_FILE"
echo ""
echo "==> Full JUnit report: $JUNIT_OUTPUT"

echo ""
echo "==> Ratchet: FAILED"
exit 1
