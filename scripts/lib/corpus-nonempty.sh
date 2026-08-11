#!/usr/bin/env bash
# Reject an empty selection before a corpus gate compares or executes it.

corpus_nonempty_assert() {
    local key="$1"
    local actual="$2"
    local context="${3:-}"
    local label="$key"
    [[ -n "$context" ]] && label="$key ($context)"

    if [[ ! "$actual" =~ ^[0-9]+$ ]]; then
        echo "corpus selection: $label count must be a non-negative integer, got '$actual'" >&2
        return 1
    fi
    if [[ "$actual" -eq 0 ]]; then
        echo "corpus selection: $label selected nothing" >&2
        return 1
    fi
    return 0
}

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    if [[ $# -lt 2 ]]; then
        echo "usage: bash scripts/lib/corpus-nonempty.sh <label> <actual-count> [context]" >&2
        exit 2
    fi
    corpus_nonempty_assert "$@"
fi
