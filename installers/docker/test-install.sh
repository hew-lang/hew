#!/usr/bin/env bash
# Ubuntu 24.04 installation verification script.
# Tests that all expected files are in place and the full compile+run pipeline works.
#
# Usage: ./installers/docker/test-install.sh [--timeout SECS]
#        --timeout  Timeout in seconds for `hew run` (default: 60)
#        -h, --help Show this help
set -euo pipefail

TIMEOUT=60

_usage() {
    awk '
        NR == 1 { next }
        /^#/ { sub(/^# ?/, ""); print; next }
        { exit }
    ' "$0"
    exit 0
}

while [[ $# -gt 0 ]]; do
    case "$1" in
    --timeout)
        TIMEOUT="$2"
        shift 2
        ;;
    -h | --help)
        _usage
        ;;
    *)
        echo "Unknown option: $1" >&2
        exit 1
        ;;
    esac
done

if command -v timeout >/dev/null 2>&1; then
    TIMEOUT_BIN=timeout
elif command -v gtimeout >/dev/null 2>&1; then
    TIMEOUT_BIN=gtimeout
else
    echo "error: timeout or gtimeout is required for bounded execution" >&2
    exit 1
fi

run_with_timeout() {
    local seconds="$1"
    shift
    "$TIMEOUT_BIN" --kill-after=5 "$seconds" "$@"
}

echo "==> Verifying hew installation on Ubuntu 24.04"
echo ""

# 1. Binaries
echo "--- Binaries"
test -x /usr/local/bin/hew && echo "  hew: ok"

# 2. Combined library
echo ""
echo "--- Combined library"
test -f /usr/local/lib/libhew.a && echo "  libhew.a: ok"

# 3. Stdlib sources
echo ""
echo "--- Stdlib sources"
test -f /usr/local/share/hew/std/string.hew && echo "  std/string.hew: ok"
test -f /usr/local/share/hew/std/fs.hew && echo "  std/fs.hew: ok"
test -d /usr/local/share/hew/std/net && echo "  std/net/: ok"
hew_count=$(find /usr/local/share/hew/std -name "*.hew" | wc -l)
echo "  total .hew files: $hew_count"

# 4. Full compile + link + run via hew run
# This exercises: module resolution → type-check → hew-codegen (MLIR) → C linker → execute
echo ""
echo "--- Full compile + run (hew run)"
if run_with_timeout "$TIMEOUT" /usr/local/bin/hew run /work/test.hew; then
    echo "  hew run: ok"
else
    status=$?
    if [[ "$status" -eq 124 ]]; then
        echo "  hew run: timed out after ${TIMEOUT}s" >&2
    fi
    exit "$status"
fi

echo ""
echo "==> All checks passed on Ubuntu 24.04."
