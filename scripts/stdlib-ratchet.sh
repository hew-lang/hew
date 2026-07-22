#!/usr/bin/env bash
# stdlib-ratchet.sh — Run stdlib type-check with a ratcheted expected-failures list.
#
# Behaviour:
#   - Exits 0 if the set of failing stdlib files exactly matches the list in
#     scripts/stdlib-expected-failures.txt.
#   - Exits 1 if any NEW stdlib file fails (unexpected regression).
#   - Exits 1 if any LISTED file no longer fails (unexpected fix — delete the
#     entry from the list to accept the green).
#
# WHY: The Hew stdlib type-check suite has known failures that converging lanes
# are fixing.  Gating on zero failures would block integration; gating on nothing
# would silently accept regressions.  This ratchet is the middle path.
#
# WHEN OBSOLETE: When the list is empty and all stdlib files pass, wire
# make test-stdlib directly into gates without this wrapper.
#
# REAL SOLUTION: Fix the underlying failures (tracked per entry in the list).
#
# Usage:
#   scripts/stdlib-ratchet.sh [--help]
#   scripts/stdlib-ratchet.sh [--expected-failures <path>]
#
# Options:
#   --expected-failures <path>   Override default expected-failures file path.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
# shellcheck source=scripts/lib/line-set.sh
# shellcheck disable=SC1091
source "$REPO_ROOT/scripts/lib/line-set.sh"
EXPECTED_FAILURES_FILE="$REPO_ROOT/scripts/stdlib-expected-failures.txt"
HEW_BIN="${HEW_BIN:-$REPO_ROOT/target/debug/hew}"
STDLIB_DIR="$REPO_ROOT/std"

usage() {
    cat <<'EOF'
Usage: scripts/stdlib-ratchet.sh [--expected-failures <path>]

Type-check all stdlib .hew files and assert the result matches the tracked
expected-failures list.  Exits 0 on exact match, 1 on any deviation.

Options:
  --expected-failures <path>   Override the default expected-failures file.
                               Default: scripts/stdlib-expected-failures.txt
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

if [[ ! -d "$STDLIB_DIR" ]]; then
    echo "error: std/ directory not found" >&2
    exit 1
fi

if [[ ! -f "$EXPECTED_FAILURES_FILE" ]]; then
    echo "error: expected-failures file not found: $EXPECTED_FAILURES_FILE" >&2
    exit 1
fi

# Read the expected failures list (ignore blank lines and # comments).
# Entries are paths relative to the repo root (e.g. std/builtins.hew).
EXPECTED_STR=""
while IFS= read -r line; do
    name="${line%%#*}"
    name="${name#"${name%%[! ]*}"}"  # ltrim
    name="${name%"${name##*[! ]}"}"  # rtrim
    [[ -z "$name" ]] && continue
    name="${name%% *}"
    [[ -z "$name" ]] && continue
    EXPECTED_STR="${EXPECTED_STR}${name}"$'\n'
done < "$EXPECTED_FAILURES_FILE"

# Type-check each stdlib file and collect failures.
ACTUAL_STR=""
TOTAL=0

while IFS= read -r -d $'\0' f; do
    TOTAL=$((TOTAL + 1))
    # Normalize path to be relative to repo root.
    relpath="${f#"$REPO_ROOT"/}"
    if ! "$HEW_BIN" check "$f" >/dev/null 2>&1; then
        ACTUAL_STR="${ACTUAL_STR}${relpath}"$'\n'
    fi
done < <(find "$STDLIB_DIR" -name '*.hew' -not -path '*/target/*' -print0 | sort -z)

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
while IFS= read -r path; do
    [[ -z "$path" ]] && continue
    if ! line_set_contains "$EXPECTED_STR" "$path"; then
        unexpected_failures="${unexpected_failures}${path}"$'\n'
    fi
done <<< "$ACTUAL_STR"

# Find unexpected passes (in expected but not in actual).
unexpected_passes=""
while IFS= read -r path; do
    [[ -z "$path" ]] && continue
    if ! line_set_contains "$ACTUAL_STR" "$path"; then
        unexpected_passes="${unexpected_passes}${path}"$'\n'
    fi
done <<< "$EXPECTED_STR"

echo "==> Stdlib type-check ratchet"
echo "Files checked:     $TOTAL"
echo "Expected failures: $count_expected"
echo "Actual failures:   $count_actual"
echo ""

count_unexpected_fail=0
[[ -n "$unexpected_failures" ]] && count_unexpected_fail="$(line_set_count "$unexpected_failures")"

count_unexpected_pass=0
[[ -n "$unexpected_passes" ]] && count_unexpected_pass="$(line_set_count "$unexpected_passes")"

if [[ $count_unexpected_fail -eq 0 && $count_unexpected_pass -eq 0 ]]; then
    if [[ $count_actual -eq 0 ]]; then
        echo "All stdlib files pass type-check. Remove entries from expected-failures file."
    else
        echo "Expected failure set matches. Tracked failures: $count_actual"
        while IFS= read -r path; do
            [[ -z "$path" ]] && continue
            echo "  - $path"
        done <<< "$sorted_actual"
    fi
    echo ""
    echo "==> Ratchet: PASSED"
    exit 0
fi

# Report problems.
if [[ $count_unexpected_fail -gt 0 ]]; then
    echo "RATCHET FAIL: $count_unexpected_fail UNEXPECTED failure(s) — not in expected list:"
    while IFS= read -r path; do
        [[ -z "$path" ]] && continue
        echo "  UNEXPECTED: $path"
        "$HEW_BIN" check "$REPO_ROOT/$path" 2>&1 | head -3 | sed 's/^/    /'
    done <<< "$unexpected_failures"
    echo ""
    echo "  To accept these as known failures, add them to:"
    echo "  $EXPECTED_FAILURES_FILE"
    echo ""
fi

if [[ $count_unexpected_pass -gt 0 ]]; then
    echo "RATCHET FAIL: $count_unexpected_pass listed failure(s) now PASS — remove from list:"
    while IFS= read -r path; do
        [[ -z "$path" ]] && continue
        echo "  NOW-PASSES: $path"
    done <<< "$unexpected_passes"
    echo ""
    echo "  Delete these lines from:"
    echo "  $EXPECTED_FAILURES_FILE"
    echo "  (Do not restore a failing entry to make this green — fix the stdlib file.)"
    echo ""
fi

echo "==> Ratchet: FAILED"
exit 1
