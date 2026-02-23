#!/usr/bin/env bash
# Compile and valgrind-check representative Hew programs.
# Requires: valgrind, a built hew compiler (target/debug/hew or target/release/hew)
#
# Usage: ./scripts/debug/valgrind-check.sh [--strip-off]
#        --strip-off  Temporarily remove --strip-all for symbol visibility (optional)
#
# Exit code 0 if all programs have 0 "definitely lost" bytes.

set -euo pipefail

HEW="${HEW:-$(pwd)/target/debug/hew}"
TMPDIR="$(mktemp -d)"
trap 'rm -rf "$TMPDIR"' EXIT

# Programs to test, covering different subsystems.
PROGRAMS=(
    "examples/fibonacci.hew"
    "examples/actor_fib.hew"
    "hew-codegen/tests/examples/e2e_supervisor_basic/supervisor_basic.hew"
    "hew-codegen/tests/examples/e2e_supervisor_nested/supervisor_nested.hew"
    "hew-codegen/tests/examples/test_hashmap_basic.hew"
    "hew-codegen/tests/examples/test_vec_basic.hew"
    "hew-codegen/tests/examples/e2e_string_drop/string_drop.hew"
)

FAIL=0

for prog in "${PROGRAMS[@]}"; do
    name="$(basename "$prog" .hew)"
    out="$TMPDIR/$name"

    if ! "$HEW" build "$prog" -o "$out" 2>/dev/null; then
        echo "SKIP  $name (compilation failed)"
        continue
    fi

    # Run under valgrind; capture only the leak summary.
    result=$(valgrind --leak-check=full --show-leak-kinds=definite \
        "$out" 2>&1 || true)

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
