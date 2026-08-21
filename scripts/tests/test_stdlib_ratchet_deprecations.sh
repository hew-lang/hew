#!/usr/bin/env bash
# Prove that successful stdlib checks cannot hide deprecation warnings.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
HARNESS="$REPO_ROOT/scripts/stdlib-ratchet.sh"
TMP_ROOT="$(mktemp -d "${TMPDIR:-/tmp}/hew-stdlib-deprecation-test.XXXXXX")"

cleanup() {
    rm -rf "$TMP_ROOT"
}
trap cleanup EXIT

FAKE_HEW="$TMP_ROOT/hew"
EXPECTED_FAILURES="$TMP_ROOT/expected-failures.txt"
touch "$EXPECTED_FAILURES"

cat > "$FAKE_HEW" <<'FAKE_HEW_EOF'
#!/usr/bin/env bash
set -euo pipefail

[[ $# -eq 2 && "$1" == "check" ]] || exit 64

if [[ "${FAKE_HEW_MODE:-clean}" == "deprecated" && "${2##*/}" == "arena.hew" ]]; then
    printf '%s:1:1: warning: E_FAKE_DEPRECATION: fake syntax is deprecated\n' "$2" >&2
fi
FAKE_HEW_EOF
chmod +x "$FAKE_HEW"

clean_output=""
clean_status=0
clean_output="$(
    FAKE_HEW_MODE=clean HEW_BIN="$FAKE_HEW" \
        "$HARNESS" --expected-failures "$EXPECTED_FAILURES" 2>&1
)" || clean_status=$?

if (( clean_status != 0 )); then
    echo "FAIL: clean successful checks were rejected with status $clean_status" >&2
    printf '%s\n' "$clean_output" >&2
    exit 1
fi
echo "PASS: clean successful checks satisfy the stdlib ratchet"

deprecated_output=""
deprecated_status=0
deprecated_output="$(
    FAKE_HEW_MODE=deprecated HEW_BIN="$FAKE_HEW" \
        "$HARNESS" --expected-failures "$EXPECTED_FAILURES" 2>&1
)" || deprecated_status=$?

printf '%s\n' "$deprecated_output" | sed 's/^/CF-[deprecated-check] /'

if (( deprecated_status == 0 )); then
    echo "FAIL: a successful check with a deprecation warning passed" >&2
    exit 1
fi
if [[ "$deprecated_output" != *"RATCHET FAIL: 1 deprecation warning(s)"* ]]; then
    echo "FAIL: rejection did not report the deprecation count" >&2
    exit 1
fi
if [[ "$deprecated_output" != *"E_FAKE_DEPRECATION"* ]]; then
    echo "FAIL: rejection did not preserve the compiler diagnostic" >&2
    exit 1
fi

echo "PASS: a successful check with a deprecation warning fails the stdlib ratchet"
