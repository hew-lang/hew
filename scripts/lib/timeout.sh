#!/usr/bin/env bash
# scripts/lib/timeout.sh — Shared timeout helpers for Hew shell scripts.
#
# Source this file; do NOT execute it directly.
#
# Provides:
#   TIMEOUT_CMD  — array set by _pick_timeout_cmd; used by run_with_timeout.
#   run_with_timeout <seconds> <cmd> [args...]
#       Runs <cmd> with a time budget.  Logs to stderr and returns the
#       timeout exit code (124 = soft, 137 = SIGKILL from --kill-after).

# Detect a timeout binary; prefer one that supports GNU --kill-after=N.
# Sets TIMEOUT_CMD array: ("binary" ["--kill-after=5"])
_pick_timeout_cmd() {
    local bin
    for bin in timeout gtimeout; do
        command -v "$bin" >/dev/null 2>&1 || continue
        # Probe: run 'true' with a 10s budget; GNU timeout accepts --kill-after here.
        if "$bin" --kill-after=1 10 true 2>/dev/null; then
            TIMEOUT_CMD=("$bin" --kill-after=5)
            return 0
        fi
    done
    # Fallback: use whatever is available without --kill-after (e.g. BSD/wrapper)
    for bin in timeout gtimeout; do
        command -v "$bin" >/dev/null 2>&1 || continue
        TIMEOUT_CMD=("$bin")
        return 0
    done
    echo "error: timeout or gtimeout is required for bounded execution" >&2
    exit 1
}
TIMEOUT_CMD=()
_pick_timeout_cmd
unset -f _pick_timeout_cmd

run_with_timeout() {
    local seconds="$1"
    shift
    "${TIMEOUT_CMD[@]}" "$seconds" "$@"
    local status=$?
    # 124 = soft timeout; 137 = SIGKILL from --kill-after fired
    if [[ "$status" -eq 124 || "$status" -eq 137 ]]; then
        echo "FATAL: timed out after ${seconds}s: $*" >&2
    fi
    return "$status"
}
