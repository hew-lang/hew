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

# The four corpus ratchets share one set-comparison implementation
# (scripts/corpus-ratchet.sh), so there is exactly one call site per direction
# to pin instead of eight that could drift apart.
# shellcheck disable=SC2016
assert_exact_source_line scripts/corpus-ratchet.sh \
    '        if ! line_set_contains "$against" "$entry"; then' \
    "corpus ratchet set difference stays pipe-safe"

# The reporters intentionally re-run a known-failing source to print a compact
# diagnostic.  The corpus sweep must not use `head` under `pipefail`: a source
# with more than three diagnostic lines would receive SIGPIPE and make the
# reporter exit 141 before its deliberate gate verdict.  Pin the draining `sed`
# form.
# shellcheck disable=SC2016
assert_exact_source_line scripts/corpus-ratchet.sh \
    '    "$HEW_BIN" check "$REPO_ROOT/$1" 2>&1 | sed -n '\''1,3{s/^/    /;p;}'\'' || true' \
    "corpus reporter drains diagnostics beyond its three-line display"

echo ""
echo "Ratchet membership wiring self-test: $PASSES passed, $FAILURES failed"
(( FAILURES == 0 ))
