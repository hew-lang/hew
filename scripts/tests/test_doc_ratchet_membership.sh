#!/usr/bin/env bash
# Deterministic regression and mutation tests for the doc-fence ratchet.

set -euo pipefail

LEGACY_HANDSHAKE_TIMEOUT_SECONDS=2
LEGACY_HANDSHAKE_TIMEOUT_STATUS=125
LEGACY_HANDSHAKE_POLL_SECONDS=0.01

legacy_large_set_producer() {
    local pipe_closed_marker="$1"
    local deadline=$(( SECONDS + LEGACY_HANDSHAKE_TIMEOUT_SECONDS ))

    printf '%s\n' "present"
    while [[ ! -e "$pipe_closed_marker" ]]; do
        if (( SECONDS >= deadline )); then
            return "$LEGACY_HANDSHAKE_TIMEOUT_STATUS"
        fi
        sleep "$LEGACY_HANDSHAKE_POLL_SECONDS"
    done
    printf '%s\n' "${LARGE_SET#*$'\n'}"
}

# Internal child mode for the watchdog-backed missing-marker tooth below.
if [[ "${1:-}" == "--probe-legacy-handshake-timeout" ]]; then
    probe_root="$(mktemp -d "${TMPDIR:-/tmp}/hew-doc-ratchet-timeout.XXXXXX")"
    probe_status=0
    unset LARGE_SET 2>/dev/null || true
    legacy_large_set_producer "$probe_root/never-created" >/dev/null \
        || probe_status=$?
    rmdir "$probe_root"
    exit "$probe_status"
fi

if [[ $# -ne 0 ]]; then
    echo "error: unexpected argument: $1" >&2
    exit 64
fi

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
HARNESS="$REPO_ROOT/scripts/extract-doc-fences.sh"

# shellcheck source=scripts/lib/line-set.sh
# shellcheck disable=SC1091
source "$REPO_ROOT/scripts/lib/line-set.sh"

# shellcheck source=scripts/lib/timeout.sh
# shellcheck disable=SC1091
source "$REPO_ROOT/scripts/lib/timeout.sh"

PASSES=0
FAILURES=0

pass() {
    echo "PASS: $*"
    PASSES=$(( PASSES + 1 ))
}

fail() {
    echo "FAIL: $*" >&2
    FAILURES=$(( FAILURES + 1 ))
}

assert_contains() {
    local output="$1"
    local expected="$2"
    local label="$3"

    if [[ "$output" == *"$expected"* ]]; then
        pass "$label"
    else
        fail "$label (missing: $expected)"
    fi
}

assert_production_membership_wiring() {
    # These are literal source contracts; variable expansion would weaken the
    # assertion by substituting the self-test's current values.
    # shellcheck disable=SC2016
    local expected_lookup='    if ! line_set_contains "$EXPECTED_STR" "$name"; then'
    # shellcheck disable=SC2016
    local actual_lookup='    if ! line_set_contains "$ACTUAL_STR" "$name"; then'

    if grep -Fqx "$expected_lookup" "$HARNESS" \
        && grep -Fqx "$actual_lookup" "$HARNESS"; then
        pass "production ratchet uses exact membership for both set comparisons"
    else
        fail "production ratchet bypasses exact membership wiring"
    fi
}

TMP_ROOT="$(mktemp -d "${TMPDIR:-/tmp}/hew-doc-ratchet-test.XXXXXX")"
cleanup() {
    rm -rf "$TMP_ROOT"
}
trap cleanup EXIT

FAKE_HEW="$TMP_ROOT/hew"
FAIL_IDS="$TMP_ROOT/fail-ids.txt"
CALL_LOG="$TMP_ROOT/calls.txt"
OUTDIR="$TMP_ROOT/fences"
EMPTY_EXPECTED="$TMP_ROOT/expected-empty.txt"
BASELINE_EXPECTED="$TMP_ROOT/expected-baseline.txt"
BASELINE_FAIL_IDS="$TMP_ROOT/fail-baseline.txt"
NOW_PASS_IDS="$TMP_ROOT/fail-now-pass.txt"
NEW_FAILURE_IDS="$TMP_ROOT/fail-new-failure.txt"
STALE_EXPECTED="$TMP_ROOT/expected-stale.txt"

touch "$FAIL_IDS" "$CALL_LOG" "$EMPTY_EXPECTED"

cat > "$FAKE_HEW" <<'FAKE_HEW_EOF'
#!/usr/bin/env bash
set -euo pipefail

[[ $# -eq 2 && "$1" == "check" ]] || exit 64

fence_id="${2##*/}"
fence_id="${fence_id%.hew}"
printf '%s\n' "$fence_id" >> "${HEW_CALL_LOG:?}"

while IFS= read -r failing_id; do
    [[ "$failing_id" == "$fence_id" ]] && exit 1
done < "${HEW_FAIL_IDS:?}"

exit 0
FAKE_HEW_EOF
chmod +x "$FAKE_HEW"

HARNESS_OUTPUT=""
HARNESS_STATUS=0
run_harness() {
    local expected_file="$1"
    local fail_file="$2"
    local call_log="$3"

    HARNESS_STATUS=0
    HARNESS_OUTPUT="$({
        HEW_FAIL_IDS="$fail_file" \
        HEW_CALL_LOG="$call_log" \
        "$HARNESS" \
            --expected-failures "$expected_file" \
            --outdir "$OUTDIR" \
            --hew-bin "$FAKE_HEW"
    } 2>&1)" || HARNESS_STATUS=$?
}

# First extract the real documentation corpus and discover every non-skipped ID.
run_harness "$EMPTY_EXPECTED" "$FAIL_IDS" "$CALL_LOG"
if [[ "$HARNESS_STATUS" -eq 0 ]]; then
    pass "discovery harness accepts an all-pass fake compiler"
else
    fail "discovery harness failed with status $HARNESS_STATUS"
fi

# Keep the regression tooth attached to the production ratchet. A large helper
# test alone would remain green if either production lookup were restored to a
# producer-to-grep pipeline, so require both call sites explicitly.
assert_production_membership_wiring

# Reproduce the old false-absence mechanism deterministically. The set is much
# larger than a pipe buffer and the present needle is first. The consumer closes
# its pipe after grep's early match, records that closure, and only then lets the
# producer write the remaining set and receive SIGPIPE under pipefail.
LARGE_SET="$(awk 'BEGIN { print "present"; for (i = 0; i < 200000; i++) printf "filler-%06d\\n", i }')"
LARGE_SET_MIN_BYTES=$(( 2 * 1024 * 1024 ))
if (( ${#LARGE_SET} > LARGE_SET_MIN_BYTES )); then
    pass "membership regression set exceeds a 2 MiB pipe-capacity bound"
else
    fail "membership regression set does not exceed the pipe-capacity bound"
fi
LEGACY_PIPE_CLOSED="$TMP_ROOT/legacy-pipe-closed"
legacy_early_exit_consumer() {
    grep -qxF "present"
    exec 0<&-
    : > "$LEGACY_PIPE_CLOSED"
}

legacy_status=0
legacy_large_set_producer "$LEGACY_PIPE_CLOSED" 2>/dev/null \
    | legacy_early_exit_consumer || legacy_status=$?
case "$legacy_status" in
    0)
        fail "legacy producer-to-grep membership did not reproduce SIGPIPE"
        ;;
    "$LEGACY_HANDSHAKE_TIMEOUT_STATUS")
        fail "legacy producer handshake timed out before consumer pipe closure"
        ;;
    *)
        pass "legacy producer-to-grep membership fails on a present large-set entry"
        ;;
esac

# Exercise the absent-marker path in a separate process group. The producer's
# dedicated status must win before the outer watchdog; removing or bypassing
# the internal deadline therefore fails quickly instead of hanging this gate.
timeout_probe_status=0
timeout_probe_budget=$(( LEGACY_HANDSHAKE_TIMEOUT_SECONDS + 3 ))
run_with_timeout "$timeout_probe_budget" "$BASH" "${BASH_SOURCE[0]}" \
    --probe-legacy-handshake-timeout >/dev/null 2>&1 \
    || timeout_probe_status=$?
case "$timeout_probe_status" in
    "$LEGACY_HANDSHAKE_TIMEOUT_STATUS")
        pass "missing-marker handshake reports its dedicated timeout status"
        ;;
    124|137)
        fail "missing-marker handshake exceeded its external watchdog"
        ;;
    *)
        fail "missing-marker handshake returned unexpected status $timeout_probe_status"
        ;;
esac

if line_set_contains "$LARGE_SET" "present"; then
    pass "exact membership keeps a present large-set entry present"
else
    fail "exact membership lost a present large-set entry"
fi

if line_set_contains "$LARGE_SET" "absent"; then
    fail "exact membership accepted an absent large-set entry"
else
    pass "exact membership rejects an absent large-set entry"
fi

# Build a sizeable tracked-failure subset while retaining at least one passing
# fence for the new-failure mutation.
baseline_count=0
passing_id=""
while IFS= read -r fence_id; do
    [[ -z "$fence_id" ]] && continue
    if (( baseline_count < 200 )); then
        printf '%s\n' "$fence_id" >> "$BASELINE_FAIL_IDS"
        checksum_output="$(cksum "$OUTDIR/${fence_id}.hew")"
        read -r checksum _ <<< "$checksum_output"
        printf '%s %s\n' "$fence_id" "$checksum" >> "$BASELINE_EXPECTED"
        baseline_count=$(( baseline_count + 1 ))
    elif [[ -z "$passing_id" ]]; then
        passing_id="$fence_id"
    fi
done < "$CALL_LOG"

if (( baseline_count == 200 )) && [[ -n "$passing_id" ]]; then
    pass "fixture retains 200 tracked failures and an untracked passing fence"
else
    fail "fixture corpus is too small for mutation coverage"
fi

run_harness "$BASELINE_EXPECTED" "$BASELINE_FAIL_IDS" /dev/null
if [[ "$HARNESS_STATUS" -eq 0 ]]; then
    pass "matching large expected and actual failure sets pass"
else
    fail "matching failure sets rejected with status $HARNESS_STATUS"
fi

# Mutation 1: a listed failure now passes and must be rejected.
first_failure=""
while IFS= read -r fence_id; do
    [[ -z "$fence_id" ]] && continue
    if [[ -z "$first_failure" ]]; then
        first_failure="$fence_id"
        continue
    fi
    printf '%s\n' "$fence_id" >> "$NOW_PASS_IDS"
done < "$BASELINE_FAIL_IDS"

run_harness "$BASELINE_EXPECTED" "$NOW_PASS_IDS" /dev/null
if [[ "$HARNESS_STATUS" -ne 0 ]]; then
    pass "now-pass mutation is rejected"
else
    fail "now-pass mutation was accepted"
fi
assert_contains "$HARNESS_OUTPUT" "NOW-PASSES: $first_failure" \
    "now-pass mutation names the exact fence"

# Mutation 2: a previously passing fence fails and must be rejected.
cp "$BASELINE_FAIL_IDS" "$NEW_FAILURE_IDS"
printf '%s\n' "$passing_id" >> "$NEW_FAILURE_IDS"
run_harness "$BASELINE_EXPECTED" "$NEW_FAILURE_IDS" /dev/null
if [[ "$HARNESS_STATUS" -ne 0 ]]; then
    pass "new-failure mutation is rejected"
else
    fail "new-failure mutation was accepted"
fi
assert_contains "$HARNESS_OUTPUT" "UNEXPECTED: $passing_id" \
    "new-failure mutation names the exact fence"

# Mutation 3: a tracked fence checksum drifts and must be rejected.
first_entry=1
while read -r fence_id checksum; do
    [[ -z "$fence_id" ]] && continue
    if (( first_entry == 1 )); then
        checksum=$(( checksum + 1 ))
        first_entry=0
    fi
    printf '%s %s\n' "$fence_id" "$checksum" >> "$STALE_EXPECTED"
done < "$BASELINE_EXPECTED"

run_harness "$STALE_EXPECTED" "$BASELINE_FAIL_IDS" /dev/null
if [[ "$HARNESS_STATUS" -ne 0 ]]; then
    pass "stale-metadata mutation is rejected"
else
    fail "stale-metadata mutation was accepted"
fi
assert_contains "$HARNESS_OUTPUT" "STALE METADATA: $first_failure" \
    "stale-metadata mutation names the exact fence"

echo ""
echo "Doc-ratchet membership self-test: $PASSES passed, $FAILURES failed"
(( FAILURES == 0 ))
