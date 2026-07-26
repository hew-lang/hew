#!/usr/bin/env bash
# scripts/lib/corpus-floor.sh — the tracked-minimum assertion for every gate
# that enumerates a corpus and then compares or asserts over it.
#
# WHY THIS EXISTS
# ───────────────
# A gate that enumerates a fixture set, a symbol list or a test set and then
# compares the result proves nothing when the enumeration is EMPTY: two empty
# sets are identical, an empty loop reports zero failures, and the gate exits 0
# forever. The same defect in slower motion is a corpus that silently shrinks
# from 57 fixtures to 3 — still "green", still proving almost nothing.
#
# So every such gate declares the size it expects in scripts/corpus-floors.tsv
# and calls this helper with the size it actually enumerated. One helper and
# one registry, so the rule stops depending on each script's author remembering
# to re-derive it.
#
# USAGE (sourced)
#   source "$REPO_ROOT/scripts/lib/corpus-floor.sh"
#   corpus_floor_assert <key> <actual-count> [context]
#
# USAGE (executed, for Makefile recipes)
#   bash scripts/lib/corpus-floor.sh <key> <actual-count> [context]
#
# Exit status: 0 when the count satisfies the registry row, 1 otherwise.
# An unknown key is an error, never a pass — a gate cannot opt out by dropping
# its row.
#
# REGISTRY FORMAT — scripts/corpus-floors.tsv, tab-separated:
#   key <TAB> mode <TAB> floor <TAB> slack <TAB> description
#
#   mode=exact   the corpus is curated and stable: any change, up or down,
#                fails and must be acknowledged by editing the row in the same
#                commit that changes the corpus.
#   mode=min     the corpus legitimately grows: `actual` may exceed `floor`,
#                but only by `slack`. Past floor+slack the gate fails and asks
#                for the floor to be raised, so a "minimum" can never drift far
#                below reality and quietly stop being a floor.

# corpus_floor_row <key> — echo the registry row for <key>, or return 1.
corpus_floor_row() {
    local key="$1"
    local registry
    registry="$(corpus_floor_registry_path)"
    local row_key row_rest
    while IFS=$'\t' read -r row_key row_rest; do
        case "$row_key" in ''|'#'*) continue ;; esac
        if [[ "$row_key" == "$key" ]]; then
            printf '%s\t%s\n' "$row_key" "$row_rest"
            return 0
        fi
    done < "$registry"
    return 1
}

corpus_floor_registry_path() {
    local here
    here="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
    printf '%s\n' "$here/corpus-floors.tsv"
}

# corpus_floor_assert <key> <actual> [context]
corpus_floor_assert() {
    local key="$1"
    local actual="$2"
    local context="${3:-}"
    local registry
    registry="$(corpus_floor_registry_path)"

    if [[ ! -f "$registry" ]]; then
        echo "corpus-floor: registry not found: $registry" >&2
        return 1
    fi
    if [[ ! "$actual" =~ ^[0-9]+$ ]]; then
        echo "corpus-floor: $key: actual count must be a non-negative integer, got '$actual'" >&2
        return 1
    fi

    local row mode floor slack description
    if ! row="$(corpus_floor_row "$key")"; then
        echo "corpus-floor: no registry row for '$key' in $registry" >&2
        echo "              A gate may not assert against a floor it has not declared." >&2
        echo "              Add a row: <key> <TAB> exact|min <TAB> <floor> <TAB> <slack|-> <TAB> <description>" >&2
        return 1
    fi
    IFS=$'\t' read -r _ mode floor slack description <<< "$row"

    if [[ ! "$floor" =~ ^[0-9]+$ ]] || [[ "$floor" -lt 1 ]]; then
        echo "corpus-floor: $key: registry floor must be a positive integer, got '$floor'" >&2
        return 1
    fi

    local where="scripts/corpus-floors.tsv"
    local label="$key"
    [[ -n "$context" ]] && label="$key ($context)"

    case "$mode" in
    exact)
        if [[ "$actual" -ne "$floor" ]]; then
            echo "" >&2
            echo "CORPUS FLOOR: $label enumerated $actual, expected exactly $floor" >&2
            echo "              $description" >&2
            if [[ "$actual" -lt "$floor" ]]; then
                echo "              The corpus SHRANK. Everything this gate compares, it now" >&2
                echo "              compares over less than it did last time — an empty or" >&2
                echo "              shrunken enumeration passes every comparison vacuously." >&2
                echo "              If the removal is intended, set the count to $actual in" >&2
                echo "              $where in the SAME commit and justify it in the body." >&2
            else
                echo "              The corpus GREW. Set the count to $actual in $where in the" >&2
                echo "              SAME commit that adds the fixtures — that is how the next" >&2
                echo "              contributor inherits a floor that still means something." >&2
            fi
            return 1
        fi
        ;;
    min)
        if [[ ! "$slack" =~ ^[0-9]+$ ]]; then
            echo "corpus-floor: $key: mode=min requires an integer slack, got '$slack'" >&2
            return 1
        fi
        if [[ "$actual" -lt "$floor" ]]; then
            echo "" >&2
            echo "CORPUS FLOOR: $label enumerated $actual, floor is $floor" >&2
            echo "              $description" >&2
            echo "              The corpus SHRANK below its tracked minimum. This gate's" >&2
            echo "              comparison is only worth the corpus it ran over; below the" >&2
            echo "              floor it proves less than it did last time." >&2
            echo "              Check first for a wrong directory, a renamed fixture, a" >&2
            echo "              filter that matches nothing, or a build that produced no" >&2
            echo "              tests. If the shrink is intended, lower the floor to $actual" >&2
            echo "              in $where in the SAME commit and justify it in the body." >&2
            return 1
        fi
        if [[ "$actual" -gt $(( floor + slack )) ]]; then
            echo "" >&2
            echo "CORPUS FLOOR: $label enumerated $actual, floor is $floor (slack $slack)" >&2
            echo "              $description" >&2
            echo "              The corpus has grown $(( actual - floor )) past its floor, so the" >&2
            echo "              floor no longer protects what is actually there. Raise it to" >&2
            echo "              $actual in $where in this commit." >&2
            return 1
        fi
        ;;
    *)
        echo "corpus-floor: $key: unknown mode '$mode' (want exact or min)" >&2
        return 1
        ;;
    esac

    printf 'corpus floor OK: %s = %d (%s %d)\n' "$label" "$actual" "$mode" "$floor"
    return 0
}

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    if [[ $# -lt 2 ]]; then
        echo "usage: bash scripts/lib/corpus-floor.sh <key> <actual-count> [context]" >&2
        exit 2
    fi
    corpus_floor_assert "$@"
fi
