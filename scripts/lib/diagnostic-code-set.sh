#!/usr/bin/env bash
# Exact structured-diagnostic helpers shared by compiler corpus gates.
# Source this file; do not execute it directly.

diagnostic_code_set() {
    local log="$1"
    LC_ALL=C grep -Eo 'E_[A-Z0-9_]+' "$log" | LC_ALL=C sort -u || true
}

diagnostic_log_has_exact_code() {
    local log="$1" expected="$2" observed
    observed="$(diagnostic_code_set "$log")"
    [[ "$observed" == "$expected" ]]
}
