#!/usr/bin/env bash
# Prove that a bare-variant refusal in a stdlib source fails the stdlib ratchet.
#
# Both bare-variant rules are hard errors, so a stdlib file carrying one exits
# non-zero and must land in the unexpected-failure set rather than being
# absorbed by a clean run. The counterfactual is the clean mode: the same
# harness over the same tree with no refusal must pass.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
HARNESS="$REPO_ROOT/scripts/corpus-ratchet.sh"
TMP_ROOT="$(mktemp -d "${TMPDIR:-/tmp}/hew-stdlib-bare-variant-test.XXXXXX")"

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

if [[ "${FAKE_HEW_MODE:-clean}" == "bare-variants" && "${2##*/}" == "arena.hew" ]]; then
    printf '%s:1:1: error: E_BARE_VARIANT_PATTERN: bare variant pattern `Some` is not a pattern\n' "$2" >&2
    printf '%s:1:1: error: E_BARE_VARIANT_EXPR: bare variant `Some` is not an expression\n' "$2" >&2
    exit 1
fi
FAKE_HEW_EOF
chmod +x "$FAKE_HEW"

clean_output=""
clean_status=0
clean_output="$(
    FAKE_HEW_MODE=clean HEW_BIN="$FAKE_HEW" "$HARNESS" stdlib \
        --expected-failures "$EXPECTED_FAILURES" 2>&1
)" || clean_status=$?

if (( clean_status != 0 )); then
    echo "FAIL: clean successful checks were rejected with status $clean_status" >&2
    printf '%s\n' "$clean_output" >&2
    exit 1
fi
echo "PASS: clean successful checks satisfy the stdlib ratchet"

bare_variant_output=""
bare_variant_status=0
bare_variant_output="$(
    FAKE_HEW_MODE=bare-variants HEW_BIN="$FAKE_HEW" "$HARNESS" stdlib \
        --expected-failures "$EXPECTED_FAILURES" 2>&1
)" || bare_variant_status=$?

printf '%s\n' "$bare_variant_output" | sed 's/^/CF-[bare-variant-check] /'

if (( bare_variant_status == 0 )); then
    echo "FAIL: a stdlib source with a bare-variant refusal passed the ratchet" >&2
    exit 1
fi
if [[ "$bare_variant_output" != *"std/arena.hew"* ]]; then
    echo "FAIL: rejection did not name the refusing stdlib source" >&2
    exit 1
fi
if [[ "$bare_variant_output" != *"E_BARE_VARIANT_PATTERN"* ]]; then
    echo "FAIL: rejection did not surface the pattern diagnostic" >&2
    exit 1
fi

echo "PASS: a bare-variant refusal fails the stdlib ratchet"
