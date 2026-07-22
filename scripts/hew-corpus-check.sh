#!/usr/bin/env bash
# hew-corpus-check.sh — Repo-wide `hew check` sweep over the full tracked .hew corpus.
#
# Behaviour:
#   - Discovers every tracked .hew file via `git ls-files '*.hew'`.
#   - EXCLUDES intentional-reject fixtures: any file whose path contains
#     `/reject/` (multi-file reject test directories) or whose filename ends
#     with `_reject.hew` (single-file reject test convention). These files are
#     already covered by test-vertical-slice / test-pkg-import / fuzz-oracle.
#   - Runs `hew check` on every non-excluded file.
#   - Ratchets the result against scripts/hew-corpus-expected-failures.txt:
#     exits 0 if the failing set exactly matches the list, exits 1 on any
#     unexpected failure (new regression) or unexpected pass (file was listed
#     but now compiles — remove it from the list to accept the green).
#
# WHY: The corpus sweep is the migrate-forward safety net. It prevents the
# class of bug where a breaking API change (symbol rename, type change) is
# landed in the compiler but fixture files in crates, tests/, or examples/ are
# silently left behind with the old call-site. Each such miss is a latent
# "undefined symbol" or type error invisible to all narrower gates.
# The ratchet tracks deferred failures explicitly so the gate is never gated
# on zero-failures while in-flight work is still landing.
#
# WHEN OBSOLETE: When the expected-failures list is empty and the full corpus
# compiles clean, remove the list file and ratchet wrapper and wire
# `make hew-check-all` directly to `hew check` with no tracking overhead.
#
# REAL SOLUTION: Fix the underlying failures (each entry is a deferred item).
#
# Usage:
#   scripts/hew-corpus-check.sh [--help]
#   scripts/hew-corpus-check.sh [--expected-failures <path>] [--hew-bin <path>]
#
# Options:
#   --expected-failures <path>   Override default expected-failures file path.
#   --hew-bin <path>             Override the hew binary path.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
# shellcheck source=scripts/lib/line-set.sh
# shellcheck disable=SC1091
source "$REPO_ROOT/scripts/lib/line-set.sh"
EXPECTED_FAILURES_FILE="$REPO_ROOT/scripts/hew-corpus-expected-failures.txt"
HEW_BIN="${HEW_BIN:-$REPO_ROOT/target/debug/hew}"

usage() {
    cat <<'EOF'
Usage: scripts/hew-corpus-check.sh [--expected-failures <path>] [--hew-bin <path>]

Run `hew check` on every tracked .hew file (excluding intentional reject
fixtures) and assert the result matches the tracked expected-failures list.

Exits 0 if the failing set exactly matches the list (or both are empty).
Exits 1 on any unexpected failure (not in list) or unexpected pass (was in
list, now compiles — remove the entry to accept the green).

Options:
  --expected-failures <path>   Override the default expected-failures file.
                               Default: scripts/hew-corpus-expected-failures.txt
  --hew-bin <path>             Override the hew binary.
                               Default: target/debug/hew (or $HEW_BIN)
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
        --hew-bin)
            shift
            [[ $# -gt 0 ]] || { echo "error: --hew-bin requires a path" >&2; exit 1; }
            HEW_BIN="$1"
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

if [[ ! -f "$EXPECTED_FAILURES_FILE" ]]; then
    echo "error: expected-failures file not found: $EXPECTED_FAILURES_FILE" >&2
    exit 1
fi

# is_reject_fixture — returns 0 (true) if the path is an intentional-reject fixture.
#
# Two conventions cover all known reject fixtures in the repo:
#   1. Path contains /reject/ — files inside reject/ subdirectories, which are
#      used by test-vertical-slice, test-pkg-import, and fuzz-oracle. Multi-file
#      reject cases include helper .hew files in the same directory that pass
#      `hew check` individually; they are still part of the reject fixture and
#      are excluded here.
#   2. The basename of the file contains "reject" — single-file reject tests
#      use several naming conventions: *_reject.hew, *_rejected.hew,
#      reject_*.hew, lsp_reject_*.hew, *_reject_reversed.hew, etc. Matching on
#      the basename substring captures all variants without false positives.
is_reject_fixture() {
    local path="$1"
    local base
    base="$(basename "$path")"
    case "$path" in
        *"/reject/"*)
            return 0
            ;;
    esac
    case "$base" in
        *"reject"*)
            return 0
            ;;
    esac
    return 1
}

# Read the expected failures list (ignore blank lines and # comments).
# Each entry is a repo-root-relative path, e.g. examples/foo.hew.
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

# Run hew check on every tracked non-reject .hew file and collect failures.
ACTUAL_STR=""
TOTAL=0
EXCLUDED=0

while IFS= read -r f; do
    if is_reject_fixture "$f"; then
        EXCLUDED=$((EXCLUDED + 1))
        continue
    fi
    TOTAL=$((TOTAL + 1))
    if ! "$HEW_BIN" check "$REPO_ROOT/$f" >/dev/null 2>&1; then
        ACTUAL_STR="${ACTUAL_STR}${f}"$'\n'
    fi
done < <(cd "$REPO_ROOT" && git ls-files '*.hew')

# Sort for deterministic output.
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

# Find unexpected failures (in actual but not in expected list).
unexpected_failures=""
while IFS= read -r path; do
    [[ -z "$path" ]] && continue
    if ! line_set_contains "$EXPECTED_STR" "$path"; then
        unexpected_failures="${unexpected_failures}${path}"$'\n'
    fi
done <<< "$ACTUAL_STR"

# Find unexpected passes (in expected list but no longer failing).
unexpected_passes=""
while IFS= read -r path; do
    [[ -z "$path" ]] && continue
    if ! line_set_contains "$ACTUAL_STR" "$path"; then
        unexpected_passes="${unexpected_passes}${path}"$'\n'
    fi
done <<< "$EXPECTED_STR"

echo "==> Hew corpus compile sweep"
echo "Files checked:          $TOTAL"
echo "Reject fixtures skipped: $EXCLUDED"
echo "Expected failures:      $count_expected"
echo "Actual failures:        $count_actual"
echo ""

count_unexpected_fail=0
[[ -n "$unexpected_failures" ]] && count_unexpected_fail="$(line_set_count "$unexpected_failures")"

count_unexpected_pass=0
[[ -n "$unexpected_passes" ]] && count_unexpected_pass="$(line_set_count "$unexpected_passes")"

if [[ $count_unexpected_fail -eq 0 && $count_unexpected_pass -eq 0 ]]; then
    if [[ $count_actual -eq 0 ]]; then
        echo "All corpus files pass hew check. The expected-failures list is empty."
    else
        echo "Expected failure set matches. Tracked failures: $count_actual"
        while IFS= read -r path; do
            [[ -z "$path" ]] && continue
            echo "  - $path"
        done <<< "$sorted_actual"
    fi
    echo ""
    echo "==> Corpus sweep: PASSED"
    exit 0
fi

# Report problems.
if [[ $count_unexpected_fail -gt 0 ]]; then
    echo "CORPUS FAIL: $count_unexpected_fail UNEXPECTED failure(s) — not in expected list:"
    while IFS= read -r path; do
        [[ -z "$path" ]] && continue
        echo "  UNEXPECTED: $path"
        "$HEW_BIN" check "$REPO_ROOT/$path" 2>&1 | head -3 | sed 's/^/    /'
    done <<< "$unexpected_failures"
    echo ""
    echo "  If these are deferred (NYI feature), add them to:"
    echo "  $EXPECTED_FAILURES_FILE"
    echo "  with a comment classifying the failure reason."
    echo ""
fi

if [[ $count_unexpected_pass -gt 0 ]]; then
    echo "CORPUS FAIL: $count_unexpected_pass listed failure(s) now PASS — remove from list:"
    while IFS= read -r path; do
        [[ -z "$path" ]] && continue
        echo "  NOW-PASSES: $path"
    done <<< "$unexpected_passes"
    echo ""
    echo "  Delete these lines from:"
    echo "  $EXPECTED_FAILURES_FILE"
    echo "  (Do not restore a failing entry to make this green — fix the file.)"
    echo ""
fi

echo "==> Corpus sweep: FAILED"
exit 1
