#!/usr/bin/env bash
# scripts/lib/line-set.sh — Exact newline-delimited set helpers for shell ratchets.
#
# Source this file; do NOT execute it directly.
#
# These helpers intentionally avoid producer | grep -q membership pipelines.
# Under `set -o pipefail`, grep may exit after an early match, deliver SIGPIPE
# to the producer, and turn a present entry into a false absence.

line_set_contains() {
    local line_set="$1"
    local needle="$2"
    local line

    while IFS= read -r line; do
        if [[ "$line" == "$needle" ]]; then
            return 0
        fi
    done <<< "$line_set"

    return 1
}

line_set_count() {
    local line_set="$1"
    local line
    local count=0

    while IFS= read -r line; do
        [[ -z "$line" ]] && continue
        count=$(( count + 1 ))
    done <<< "$line_set"

    printf '%d\n' "$count"
}
