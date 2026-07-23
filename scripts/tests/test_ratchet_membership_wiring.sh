#!/usr/bin/env bash
# Fail-closed source contracts for every newline-set ratchet consumer.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
PASSES=0
FAILURES=0

pass() {
    echo "PASS: $*"
    PASSES=$(( PASSES + 1 ))
}

fail() {
    echo "FAIL: $*" >&2
    FAILURES=$(( FAILURES + 1 ))
}

assert_exact_source_line() {
    local script="$1"
    local expected="$2"
    local label="$3"
    local line
    local matches=0

    while IFS= read -r line; do
        [[ "$line" == "$expected" ]] && matches=$(( matches + 1 ))
    done < "$REPO_ROOT/$script"

    if (( matches == 1 )); then
        pass "$label"
    else
        fail "$label (expected one exact call site, found $matches)"
    fi
}

# shellcheck disable=SC2016
assert_exact_source_line scripts/extract-doc-fences.sh \
    '    if ! line_set_contains "$EXPECTED_STR" "$name"; then' \
    "doc ratchet expected-set lookup stays pipe-safe"
# shellcheck disable=SC2016
assert_exact_source_line scripts/extract-doc-fences.sh \
    '    if ! line_set_contains "$ACTUAL_STR" "$name"; then' \
    "doc ratchet actual-set lookup stays pipe-safe"

# shellcheck disable=SC2016
assert_exact_source_line scripts/hew-suite-ratchet.sh \
    '    if ! line_set_contains "$EXPECTED_STR" "$name"; then' \
    "Hew-suite ratchet expected-set lookup stays pipe-safe"
# shellcheck disable=SC2016
assert_exact_source_line scripts/hew-suite-ratchet.sh \
    '    if ! line_set_contains "$ACTUAL_STR" "$name"; then' \
    "Hew-suite ratchet actual-set lookup stays pipe-safe"

# shellcheck disable=SC2016
assert_exact_source_line scripts/stdlib-ratchet.sh \
    '    if ! line_set_contains "$EXPECTED_STR" "$path"; then' \
    "stdlib ratchet expected-set lookup stays pipe-safe"
# shellcheck disable=SC2016
assert_exact_source_line scripts/stdlib-ratchet.sh \
    '    if ! line_set_contains "$ACTUAL_STR" "$path"; then' \
    "stdlib ratchet actual-set lookup stays pipe-safe"

# shellcheck disable=SC2016
assert_exact_source_line scripts/hew-corpus-check.sh \
    '    if ! line_set_contains "$EXPECTED_STR" "$path"; then' \
    "corpus ratchet expected-set lookup stays pipe-safe"
# shellcheck disable=SC2016
assert_exact_source_line scripts/hew-corpus-check.sh \
    '    if ! line_set_contains "$ACTUAL_STR" "$path"; then' \
    "corpus ratchet actual-set lookup stays pipe-safe"

echo ""
echo "Ratchet membership wiring self-test: $PASSES passed, $FAILURES failed"
(( FAILURES == 0 ))
