#!/usr/bin/env bash
# Shared bare-variant ratchet reporting contract.

bare_variant_ratchet_failure_message() {
    local count="$1"

    printf 'RATCHET FAIL: %s bare variant diagnostic(s)' "$count"
}
