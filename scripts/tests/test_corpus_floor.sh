#!/usr/bin/env bash
# test_corpus_floor.sh — teeth for the shared corpus-floor assertion.
#
# Drives EVERY row of scripts/corpus-floors.tsv through both implementations
# (scripts/lib/corpus-floor.sh and scripts/lib/corpus_floor.py) and proves, per
# row:
#
#   0            → FAIL   the empty enumeration every gated comparison passes
#                         vacuously over
#   floor - 1    → FAIL   the corpus that shrank by one fixture
#   floor        → PASS   the normal run
#   floor + 1    → FAIL on exact rows (a grown corpus must update its count)
#   floor+slack+1→ FAIL on min rows (an unraised floor stops being a floor)
#
# and that the shell and Python implementations return the SAME verdict for
# every one of those counts, so a gate's language cannot change its floor.
#
# Also checks the two ways a gate might try to slip out of the registry: an
# unknown key and a non-numeric count both fail closed.

set -uo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SH_HELPER="$REPO_ROOT/scripts/lib/corpus-floor.sh"
PY_HELPER="$REPO_ROOT/scripts/lib/corpus_floor.py"
REGISTRY="$REPO_ROOT/scripts/corpus-floors.tsv"

checks=0
failures=0

# expect <want-rc> <key> <count> <label>
expect() {
    local want="$1" key="$2" count="$3" label="$4"
    local sh_rc=0 py_rc=0

    bash "$SH_HELPER" "$key" "$count" >/dev/null 2>&1 || sh_rc=$?
    python3 "$PY_HELPER" "$key" "$count" >/dev/null 2>&1 || py_rc=$?

    checks=$(( checks + 1 ))
    if [[ "$sh_rc" -ne "$want" ]]; then
        echo "  FAIL [sh]  $label: expected rc $want, got $sh_rc" >&2
        failures=$(( failures + 1 ))
    fi
    if [[ "$py_rc" -ne "$want" ]]; then
        echo "  FAIL [py]  $label: expected rc $want, got $py_rc" >&2
        failures=$(( failures + 1 ))
    fi
    if [[ "$sh_rc" -ne "$py_rc" ]]; then
        echo "  FAIL [==]  $label: shell rc $sh_rc but Python rc $py_rc" >&2
        failures=$(( failures + 1 ))
    fi
}

echo "==> corpus-floor helper self-test"

rows=0
while IFS=$'\t' read -r key mode floor slack _description; do
    case "$key" in ''|'#'*) continue ;; esac
    rows=$(( rows + 1 ))

    expect 1 "$key" 0 "$key: empty enumeration"
    expect 1 "$key" $(( floor - 1 )) "$key: one below floor"
    expect 0 "$key" "$floor" "$key: at floor"
    if [[ "$mode" == "exact" ]]; then
        expect 1 "$key" $(( floor + 1 )) "$key: one above an exact count"
    else
        expect 0 "$key" $(( floor + slack )) "$key: within slack"
        expect 1 "$key" $(( floor + slack + 1 )) "$key: past floor+slack"
    fi
done < "$REGISTRY"

if [[ "$rows" -eq 0 ]]; then
    echo "  FAIL: no rows read from $REGISTRY — the self-test proved nothing" >&2
    failures=$(( failures + 1 ))
fi

expect 1 "no-such-corpus-key" 999 "unknown key fails closed"
expect 1 "corpus-floor-registry" "not-a-number" "non-numeric count fails closed"

echo ""
if [[ "$failures" -ne 0 ]]; then
    echo "corpus-floor self-test: FAILED ($failures of $checks checks, $rows rows)" >&2
    exit 1
fi
echo "corpus-floor self-test: all $checks checks PASS across $rows registry rows"
