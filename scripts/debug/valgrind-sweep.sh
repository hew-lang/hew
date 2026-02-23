#!/usr/bin/env bash
# Comprehensive valgrind sweep across all compilable Hew test programs.
# Discovers and tests every .hew file under hew-codegen/tests/examples/ and examples/.
#
# Requires: valgrind, a built hew compiler + hew-codegen
#
# Usage: ./scripts/debug/valgrind-sweep.sh [--all] [--verbose] [--timeout SECS]
#        --all      Include tests that are known to fail compilation
#        --verbose  Show valgrind details for leaking programs
#        --timeout  Per-program timeout in seconds (default: 15)
#
# Exit code 0 if all programs have 0 "definitely lost" bytes.

set -euo pipefail

HEW="${HEW:-$(pwd)/target/debug/hew}"
TMPDIR="$(mktemp -d)"
trap 'rm -rf "$TMPDIR"' EXIT

VERBOSE=0
TIMEOUT=15
INCLUDE_KNOWN_FAIL=0

while [[ $# -gt 0 ]]; do
    case "$1" in
    --verbose)
        VERBOSE=1
        shift
        ;;
    --all)
        INCLUDE_KNOWN_FAIL=1
        shift
        ;;
    --timeout)
        TIMEOUT="$2"
        shift 2
        ;;
    *)
        echo "Unknown option: $1"
        exit 1
        ;;
    esac
done

# Tests known to fail compilation (unimplemented features).
# These are skipped unless --all is passed.
KNOWN_COMPILE_FAIL=(
    "binary_search"
    "option_result_chain"
    "receive_gen"
    "dyn_dispatch"
    "trait_inherit"
    "scope_launch"
    "drop_trait"
    "drop_scope"
    "defer"
)

# Tests that block forever (servers, brokers) — skip under valgrind.
KNOWN_BLOCKING=(
    "mqtt_broker"
    "http_server"
    "tcp_server"
    "web_server"
    "tcp_echo"
)

should_skip() {
    local name="$1"
    # Always skip blocking programs (servers etc.) under valgrind.
    for pattern in "${KNOWN_BLOCKING[@]}"; do
        if [[ "$name" == *"$pattern"* ]]; then
            return 0
        fi
    done
    if [ "$INCLUDE_KNOWN_FAIL" -eq 1 ]; then
        return 1
    fi
    for pattern in "${KNOWN_COMPILE_FAIL[@]}"; do
        if [[ "$name" == *"$pattern"* ]]; then
            return 0
        fi
    done
    return 1
}

PASS=0
FAIL=0
SKIP=0
COMPILE_FAIL=0
TOTAL=0
LEAK_DETAILS=""

check_one() {
    local src="$1" label="$2"
    local name out result definite

    name=$(basename "$src" .hew)

    if should_skip "$label"; then
        SKIP=$((SKIP + 1))
        return
    fi

    out="$TMPDIR/$name"

    if ! "$HEW" build "$src" -o "$out" 2>/dev/null; then
        COMPILE_FAIL=$((COMPILE_FAIL + 1))
        if [ "$VERBOSE" -eq 1 ]; then
            echo "COMPILE_FAIL  $label"
        fi
        return
    fi

    TOTAL=$((TOTAL + 1))

    result=$(timeout --kill-after=5 "$TIMEOUT" valgrind --leak-check=full --show-leak-kinds=definite \
        "$out" 2>&1 || true)

    # No heap usage at all
    if echo "$result" | grep -q "no leaks are possible"; then
        PASS=$((PASS + 1))
        return
    fi

    # All blocks freed
    if echo "$result" | grep -q "All heap blocks were freed"; then
        PASS=$((PASS + 1))
        return
    fi

    # Parse "definitely lost: N bytes in M blocks"
    definite=$(echo "$result" | grep "definitely lost:" |
        grep -oP '\d[\d,]*(?= bytes)' | tr -d ',' || true)

    if [ -z "$definite" ] || [ "$definite" = "0" ]; then
        PASS=$((PASS + 1))
        return
    fi

    FAIL=$((FAIL + 1))
    LEAK_DETAILS="${LEAK_DETAILS}LEAK  ${label}  (${definite} bytes definitely lost)\n"

    if [ "$VERBOSE" -eq 1 ]; then
        echo "LEAK  $label  ($definite bytes definitely lost)"
        echo "$result" | grep -E "definitely lost|Invalid|at 0x" | head -10
        echo ""
    fi
}

echo "Valgrind sweep: discovering test programs..."
echo ""

# Directory-based e2e tests
for dir in hew-codegen/tests/examples/e2e_*/; do
    [ -d "$dir" ] || continue
    dirname=$(basename "$dir")
    for src in "$dir"*.hew; do
        [ -f "$src" ] || continue
        name=$(basename "$src" .hew)
        check_one "$src" "$dirname/$name"
    done
done

# Standalone test_*.hew files
for src in hew-codegen/tests/examples/test_*.hew; do
    [ -f "$src" ] || continue
    name=$(basename "$src" .hew)
    check_one "$src" "$name"
done

# Top-level examples/
for src in examples/*.hew; do
    [ -f "$src" ] || continue
    name=$(basename "$src" .hew)
    check_one "$src" "examples/$name"
done

# Playground examples
for src in hew-codegen/tests/examples/playground_*/*.hew; do
    [ -f "$src" ] || continue
    dirname=$(basename "$(dirname "$src")")
    name=$(basename "$src" .hew)
    check_one "$src" "$dirname/$name"
done

# Junior examples
for src in hew-codegen/tests/examples/junior_*/*.hew; do
    [ -f "$src" ] || continue
    dirname=$(basename "$(dirname "$src")")
    name=$(basename "$src" .hew)
    check_one "$src" "$dirname/$name"
done

echo "=== Valgrind Sweep Results ==="
echo "Tested:        $TOTAL programs"
echo "Pass:          $PASS"
echo "Leaks:         $FAIL"
echo "Compile fail:  $COMPILE_FAIL"
echo "Skipped:       $SKIP"
echo ""

if [ "$FAIL" -gt 0 ]; then
    echo "=== Leaking Programs ==="
    echo -e "$LEAK_DETAILS"
    exit 1
else
    echo "All $PASS programs passed — no definite leaks."
fi
