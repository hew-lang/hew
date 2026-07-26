#!/usr/bin/env bash
# check-corpus-floors.sh — keep the corpus-floor registry honest.
#
# scripts/corpus-floors.tsv records the tracked minimum size of every gated
# enumeration in the tree (see that file and scripts/lib/corpus-floor.sh). This
# checker proves the registry itself cannot rot:
#
#   1. Every row is well formed: five tab-separated fields, a known mode, a
#      positive floor, an integer slack on min rows.
#   2. Keys are unique and sorted, so a duplicate row cannot shadow another.
#   3. Every key has a live call site in scripts/ or the Makefile. A floor that
#      nothing reads is a floor that stops firing the moment somebody deletes
#      the assertion, which is exactly the silent-green defect the registry
#      exists to prevent.
#   4. The registry's own row count is itself floored (key
#      corpus-floor-registry), so emptying the registry fails here rather than
#      turning every gate's floor off at once.
#
# Usage: scripts/check-corpus-floors.sh

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
# shellcheck source=scripts/lib/corpus-floor.sh
# shellcheck disable=SC1091
source "$REPO_ROOT/scripts/lib/corpus-floor.sh"

REGISTRY="$REPO_ROOT/scripts/corpus-floors.tsv"
REGISTRY_DISPLAY="scripts/corpus-floors.tsv"

if [[ ! -f "$REGISTRY" ]]; then
    echo "error: registry not found: $REGISTRY_DISPLAY" >&2
    exit 1
fi

fail=0
rows=0
prev_key=""
seen_keys=""

echo "==> Corpus-floor registry check"

while IFS= read -r line || [[ -n "$line" ]]; do
    case "$line" in ''|'#'*) continue ;; esac
    rows=$(( rows + 1 ))

    field_count="$(awk -F'\t' '{print NF}' <<< "$line")"
    if [[ "$field_count" -ne 5 ]]; then
        echo "  MALFORMED ROW ($field_count fields, want 5): $line" >&2
        fail=1
        continue
    fi

    IFS=$'\t' read -r key mode floor slack description <<< "$line"

    if [[ -z "$description" ]]; then
        echo "  ROW WITHOUT DESCRIPTION: $key" >&2
        fail=1
    fi
    case "$mode" in
        exact)
            if [[ "$slack" != "-" ]]; then
                echo "  BAD SLACK: $key is mode=exact and must use '-' (got '$slack')" >&2
                fail=1
            fi
            ;;
        min)
            if [[ ! "$slack" =~ ^[0-9]+$ ]]; then
                echo "  BAD SLACK: $key is mode=min and needs an integer slack (got '$slack')" >&2
                fail=1
            fi
            ;;
        *)
            echo "  BAD MODE: $key has mode '$mode' (want exact or min)" >&2
            fail=1
            ;;
    esac
    if [[ ! "$floor" =~ ^[0-9]+$ ]] || [[ "$floor" -lt 1 ]]; then
        echo "  BAD FLOOR: $key has floor '$floor'; a floor of zero is not a floor" >&2
        fail=1
    fi

    if [[ -n "$prev_key" && ! "$key" > "$prev_key" ]]; then
        echo "  OUT OF ORDER: $key must sort after $prev_key (keys are sorted and unique)" >&2
        fail=1
    fi
    prev_key="$key"
    seen_keys="${seen_keys}${key}"$'\n'

    # A key with no reader is a floor that has already stopped firing.
    if ! git -C "$REPO_ROOT" grep -q --fixed-strings -- "$key" -- scripts Makefile ':!scripts/corpus-floors.tsv'; then
        echo "  ORPHAN KEY: $key is declared but no gate in scripts/ or the Makefile reads it" >&2
        echo "              Either wire the assertion back up or delete the row." >&2
        fail=1
    fi
done < "$REGISTRY"

if ! corpus_floor_assert "corpus-floor-registry" "$rows"; then
    fail=1
fi

if [[ "$fail" -ne 0 ]]; then
    echo "==> Corpus-floor registry check: FAILED" >&2
    exit 1
fi

echo "==> Corpus-floor registry check: PASSED ($rows floors, all with live call sites)"
