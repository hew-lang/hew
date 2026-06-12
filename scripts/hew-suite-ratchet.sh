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
# WHEN OBSOLETE: When the list is empty and all tests are green, remove the
# ratchet wrapper and wire make test-hew directly into gates.
#
# REAL SOLUTION: Fix the underlying failures (tracked per entry in the list).
#
# Usage:
#   scripts/hew-suite-ratchet.sh [--help]
#   scripts/hew-suite-ratchet.sh [--expected-failures <path>]
#
# Options:
#   --expected-failures <path>   Override default expected-failures file path.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
EXPECTED_FAILURES_FILE="$REPO_ROOT/scripts/hew-suite-expected-failures.txt"
# HEW_BIN is overridable for parser tests (point it at a stub that replays
# captured runner output); production callers use the default.
HEW_BIN="${HEW_BIN:-$REPO_ROOT/target/debug/hew}"
TESTS_DIR="$REPO_ROOT/tests/hew"

usage() {
    cat <<'EOF'
Usage: scripts/hew-suite-ratchet.sh [--expected-failures <path>]

Run `hew test tests/hew/` and assert the result matches the tracked expected-failures list.

Exits 0 if the failing test set exactly matches the list (or both are empty).
Exits 1 on any unexpected failure (not in list) or unexpected pass (was in list, now passes).

Options:
  --expected-failures <path>   Override the default expected-failures file.
                               Default: scripts/hew-suite-expected-failures.txt
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

# Run hew test and capture output (exit code is non-zero when tests fail;
# we determine pass/fail from the parsed output, not the exit code).
RAW_OUTPUT=""
RAW_OUTPUT=$("$HEW_BIN" test "$TESTS_DIR" 2>&1) || true

# hew test colours its output even when not attached to a TTY; strip ANSI
# escape sequences before parsing so "FAILED" matches literally.
CLEAN_OUTPUT="$(printf '%s\n' "$RAW_OUTPUT" | sed $'s/\x1b\\[[0-9;]*m//g')"

# Fail closed if the runner produced no summary line: a runner crash or an
# output-format change must never read as success.
if ! printf '%s\n' "$CLEAN_OUTPUT" | grep -q "^test result:"; then
    echo "error: no 'test result:' summary in hew test output; refusing to ratchet" >&2
    printf '%s\n' "$RAW_OUTPUT"
    exit 1
fi

# Extract names of failing tests from lines matching "test <name> ... FAILED".
ACTUAL_STR=""
while IFS= read -r line; do
    case "$line" in
        "test "*"... FAILED"*)
            name="${line#test }"
            name="${name%% ...*}"
            [[ -n "$name" ]] && ACTUAL_STR="${ACTUAL_STR}${name}"$'\n'
            ;;
    esac
done <<< "$CLEAN_OUTPUT"

# Cross-check the parsed failure count against the runner's own summary.
# A mismatch means the per-test parse missed lines (format drift) — fail
# closed rather than ratcheting against an undercounted set.
summary_failed="$(printf '%s\n' "$CLEAN_OUTPUT" | sed -n 's/^test result:.*[^0-9]\([0-9][0-9]*\) failed.*/\1/p' | tail -1)"
[[ -z "$summary_failed" ]] && summary_failed=0
parsed_failed=0
if [[ -n "$ACTUAL_STR" ]]; then
    parsed_failed="$(printf '%s' "$ACTUAL_STR" | grep -c .)"
fi
if [[ "$parsed_failed" -ne "$summary_failed" ]]; then
    echo "error: parsed $parsed_failed FAILED line(s) but runner summary reports $summary_failed failed; refusing to ratchet" >&2
    printf '%s\n' "$RAW_OUTPUT"
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
    count_expected="$(printf '%s' "$EXPECTED_STR" | grep -c .)"
fi

count_actual=0
if [[ -n "$ACTUAL_STR" ]]; then
    count_actual="$(printf '%s' "$ACTUAL_STR" | grep -c .)"
fi

# Find unexpected failures (in actual but not in expected).
unexpected_failures=""
while IFS= read -r name; do
    [[ -z "$name" ]] && continue
    if ! printf '%s\n' "$EXPECTED_STR" | grep -qxF "$name"; then
        unexpected_failures="${unexpected_failures}${name}"$'\n'
    fi
done <<< "$ACTUAL_STR"

# Find unexpected passes (in expected but not in actual).
unexpected_passes=""
while IFS= read -r name; do
    [[ -z "$name" ]] && continue
    if ! printf '%s\n' "$ACTUAL_STR" | grep -qxF "$name"; then
        unexpected_passes="${unexpected_passes}${name}"$'\n'
    fi
done <<< "$EXPECTED_STR"

echo "==> Hew suite ratchet"
echo "Expected failures: $count_expected"
echo "Actual failures:   $count_actual"
echo ""

count_unexpected_fail=0
[[ -n "$unexpected_failures" ]] && count_unexpected_fail="$(printf '%s' "$unexpected_failures" | grep -c .)"

count_unexpected_pass=0
[[ -n "$unexpected_passes" ]] && count_unexpected_pass="$(printf '%s' "$unexpected_passes" | grep -c .)"

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

# Print the raw runner output so CI surfaces it.
echo "==> Raw hew test output:"
printf '%s\n' "$RAW_OUTPUT"

echo ""
echo "==> Ratchet: FAILED"
exit 1
