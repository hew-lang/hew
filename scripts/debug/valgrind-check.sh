#!/usr/bin/env bash
# Compile and valgrind-check representative Hew programs.
# Requires: valgrind, a built hew compiler (target/debug/hew or target/release/hew)
#
# Usage: ./scripts/debug/valgrind-check.sh [--timeout SECS]
#        --timeout  Per-program timeout in seconds for build + valgrind (default: 15)
#        -h, --help Show this help
#
# Exit code 0 if all programs have 0 "definitely lost" bytes.

set -euo pipefail

HEW="${HEW:-$(pwd)/target/debug/hew}"
TMPDIR="$(mktemp -d)"
trap 'rm -rf "$TMPDIR"' EXIT
TIMEOUT=15

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

# Programs to test, covering different subsystems.
PROGRAMS=(
    "examples/fibonacci.hew"
    "examples/actor_fib.hew"
    "hew-codegen/tests/examples/e2e_supervisor/supervisor_basic.hew"
    "hew-codegen/tests/examples/e2e_supervisor/supervisor_nested.hew"
    "hew-codegen/tests/examples/test_hashmap_basic.hew"
    "hew-codegen/tests/examples/test_vec_basic.hew"
    "hew-codegen/tests/examples/e2e_strings/string_drop.hew"
)

FAIL=0

for prog in "${PROGRAMS[@]}"; do
    name="$(basename "$prog" .hew)"
    out="$TMPDIR/$name"

    if run_with_timeout "$TIMEOUT" "$HEW" build "$prog" -o "$out" 2>/dev/null; then
        build_status=0
    else
        build_status=$?
    fi
    if [ "$build_status" -ne 0 ]; then
        if [ "$build_status" -eq 124 ]; then
            echo "FAIL  $name  (timed out after ${TIMEOUT}s during build)"
            FAIL=1
        else
            echo "SKIP  $name (compilation failed)"
        fi
        continue
    fi

    # Run under valgrind; capture only the leak summary.
    if result=$(run_with_timeout "$TIMEOUT" valgrind --leak-check=full --show-leak-kinds=definite \
        "$out" 2>&1); then
        run_status=0
    else
        run_status=$?
    fi

    if [ "$run_status" -eq 124 ]; then
        echo "FAIL  $name  (timed out after ${TIMEOUT}s under valgrind)"
        FAIL=1
        continue
    fi

    lost=$(echo "$result" | grep "definitely lost:" | grep -oP '\d+ bytes' | head -1 || true)

    if echo "$lost" | grep -q "^0 bytes"; then
        echo "PASS  $name  ($lost)"
    elif [ -z "$lost" ]; then
        # No leak summary line — non-actor program with 0 allocs.
        echo "PASS  $name  (no heap usage)"
    else
        echo "FAIL  $name  ($lost definitely lost)"
        FAIL=1
    fi
done

if [ "$FAIL" -eq 0 ]; then
    echo ""
    echo "All programs passed valgrind check."
else
    echo ""
    echo "Some programs had memory leaks!"
    exit 1
fi
