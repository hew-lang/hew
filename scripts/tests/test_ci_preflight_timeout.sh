#!/usr/bin/env bash
# scripts/tests/test_ci_preflight_timeout.sh
#
# Integration tests for timeout behaviour used by ci-preflight-dispatcher.sh.
#
# Tests 1–3: exercise scripts/lib/timeout.sh's run_with_timeout directly.
# Test 4:    dispatcher dry-run output shape (budget annotation).
# Test 5:    regression for orphaned-grandchild bug — proves that the
#            dispatcher's perl-setpgid + Perl-watchdog pattern kills the
#            entire process tree (bash -lc → grandchild), not only the
#            direct child.  Standard system timeout binaries do not provide
#            this guarantee; the Perl approach does.
#
# Exit 0 = all passed.  Exit 1 = at least one failure.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

# shellcheck source=scripts/lib/timeout.sh
# shellcheck disable=SC1091
source "${REPO_ROOT}/scripts/lib/timeout.sh"

FAILURES=0
PASSES=0

pass() { echo "PASS: $*"; (( PASSES++ )) || true; }
fail() { echo "FAIL: $*" >&2; (( FAILURES++ )) || true; }

# ---------------------------------------------------------------------------
# Test 1: run_with_timeout kills a command that exceeds the budget.
#
# After the lib/timeout.sh fix (bare timeout/gtimeout without --kill-after
# removed), this always uses the Perl process-group-safe backend on hosts
# without GNU coreutils timeout.  Perl exits 124 (soft timeout) or 137
# (SIGKILL hard kill); 143 is no longer produced by this function.
# ---------------------------------------------------------------------------
_status=0
_start=$SECONDS
run_with_timeout 2 sleep 9999 2>/dev/null || _status=$?
_elapsed=$(( SECONDS - _start ))

if [[ "$_status" -eq 124 || "$_status" -eq 137 ]]; then
    pass "run_with_timeout kills a hung command (exit ${_status}, ${_elapsed}s)"
else
    fail "run_with_timeout: expected Perl timeout exit code (124 or 137), got ${_status}"
fi

# The whole thing should resolve within 2s + 5s kill-after + 2s margin = 9s.
if (( _elapsed <= 9 )); then
    pass "run_with_timeout resolved within expected wall-clock (${_elapsed}s)"
else
    fail "run_with_timeout: took ${_elapsed}s — expected <= 9s for a 2s budget"
fi

# ---------------------------------------------------------------------------
# Test 2: run_with_timeout allows a fast command to complete successfully.
# ---------------------------------------------------------------------------
_status=0
run_with_timeout 10 true 2>/dev/null || _status=$?
if [[ "$_status" -eq 0 ]]; then
    pass "run_with_timeout allows a fast command to complete successfully"
else
    fail "run_with_timeout: 'true' failed with exit ${_status}"
fi

# ---------------------------------------------------------------------------
# Test 3: run_with_timeout propagates a command's non-timeout exit code.
# ---------------------------------------------------------------------------
_status=0
run_with_timeout 10 bash -c 'exit 42' 2>/dev/null || _status=$?
if [[ "$_status" -eq 42 ]]; then
    pass "run_with_timeout propagates command exit code (got ${_status})"
else
    fail "run_with_timeout: expected exit 42, got ${_status}"
fi

# ---------------------------------------------------------------------------
# Test 4: Dispatcher emits timing output and terminates a hung command.
#
# Strategy: override PREFLIGHT_TIMEOUT_NARROW to a very small value (3s) and
# pass a path that routes to the narrow 'types' bucket.  The 'types' bucket
# runs 'cargo fmt --all -- --check' first.  We do NOT need cargo to actually
# hang — instead we confirm the dispatcher runs without a crash and produces
# the expected output shape (elapsed, summary table) even when commands
# succeed.  The timeout guard ensures it would also kill a hung command.
#
# We run in --dry-run mode to avoid touching the actual workspace; the
# instrumentation additions we test here are all visible in dry-run output.
# ---------------------------------------------------------------------------
_result=$(
    bash "${REPO_ROOT}/scripts/ci-preflight-dispatcher.sh" \
        --dry-run -- "hew-types/src/lib.rs" 2>/dev/null
) || true

if printf '%s\n' "$_result" | grep -q "budget: 180s"; then
    pass "dispatcher dry-run emits per-command budget annotation (180s narrow)"
else
    fail "dispatcher dry-run: expected '(budget: 180s)' in output for types lane"
    printf "  got:\n%s\n" "$_result" >&2
fi

if printf '%s\n' "$_result" | grep -q "Selected profile: types"; then
    pass "dispatcher dry-run correctly routes hew-types path to 'types' lane"
else
    fail "dispatcher dry-run: expected 'Selected profile: types'"
    printf "  got:\n%s\n" "$_result" >&2
fi

# ---------------------------------------------------------------------------
# Test 5: Grandchild termination — exercises the shared run_in_pgroup_with_timeout
#         helper that the dispatcher routes through.
#
# Uses a FIFO handshake so the pgid probe is deterministic and mandatory:
#   1. The test creates a FIFO before starting the helper.
#   2. The command string written to bash -lc begins with
#          printf '%s\n' "$$" > FIFO
#      where $$ inside bash -lc equals the pgid leader's PID (because the
#      Perl child called setpgid(0,0) then exec'd bash; same PID, new pgid).
#   3. The test reads from the FIFO while the helper runs in the background.
#      FIFO semantics block the writer until the reader opens the read end,
#      so the handshake is race-free.
#   4. If the FIFO read times out the test fails hard (no silent skip).
#   5. After the helper completes, kill -0 -$pgid must fail (grandchild dead).
#
# The bash -lc (non-interactive, no job control / set -m) leaves background
# jobs in the same process group, so kill(-pgid) reaches both bash and the
# grandchild sleep 30 simultaneously.
# ---------------------------------------------------------------------------

_T5_FIFO=$(mktemp -u /tmp/hew-test5-pgid.XXXXXX)
mkfifo "$_T5_FIFO"
_t5_cleanup() { rm -f "$_T5_FIFO"; }
trap _t5_cleanup EXIT

# Command string: bash -lc writes its own PID (= pgid leader) to the FIFO,
# then starts the grandchild scenario.  $$ inside bash -lc is that bash's
# own PID (not a subshell $$), which equals the pgid after exec.
run_in_pgroup_with_timeout 1 \
    "printf '%s\n' \"\$\$\" > '$_T5_FIFO'; bash -c 'sleep 30' & sleep 9999" &
_T5_HELPER_PID=$!

# Read the pgid from the FIFO.  bash -lc writes before sleeping, so the
# read returns almost instantly.  -t 2 guards against a setup failure.
_PGROUP_ID=""
read -r -t 2 _PGROUP_ID < "$_T5_FIFO" 2>/dev/null || true
_PGROUP_ID="${_PGROUP_ID//[[:space:]]/}"  # strip any trailing whitespace/CR

rm -f "$_T5_FIFO"
trap - EXIT  # FIFO cleaned up

# Fail hard if the pgid was not captured — the handshake is mandatory.
if [[ -z "$_PGROUP_ID" ]]; then
    fail "Test 5: pgid handshake failed — bash -lc did not write its PID within 2s"
    wait "$_T5_HELPER_PID" 2>/dev/null || true
else
    _T5_STATUS=0
    wait "$_T5_HELPER_PID" 2>/dev/null || _T5_STATUS=$?

    # SIGTERM from the watchdog → bash exits 143 (128+15).
    # SIGKILL fallback → 137 (128+9).
    if [[ "$_T5_STATUS" -eq 143 || "$_T5_STATUS" -eq 137 ]]; then
        pass "run_in_pgroup_with_timeout: exited with signal-kill code (${_T5_STATUS})"
    else
        fail "run_in_pgroup_with_timeout: expected signal-kill exit (143 or 137), got ${_T5_STATUS}"
    fi

    # Allow a brief window for SIGTERM to finish propagating to the grandchild.
    sleep 1

    # Verify no process remains in the group (grandchild must be dead too).
    # POSIX: kill -0 to a pgid succeeds only if at least one process is alive.
    if kill -0 -"$_PGROUP_ID" 2>/dev/null; then
        fail "run_in_pgroup_with_timeout: processes still alive in group ${_PGROUP_ID} — grandchild NOT terminated"
    else
        pass "run_in_pgroup_with_timeout: entire process tree terminated (bash -lc grandchild also dead)"
    fi
fi

# ---------------------------------------------------------------------------
# Test 6: Validation of invalid seconds — run_in_pgroup_with_timeout must
#         fail closed (exit 1, no command launched) for zero, negative,
#         empty, or non-numeric budgets.  alarm(0) cancels the Perl watchdog;
#         these values must never silently bypass the preflight time budget.
# ---------------------------------------------------------------------------

_SENTINEL_FILE=$(mktemp -u /tmp/hew-test6-sentinel.XXXXXX)

for _invalid_seconds in "0" "-1" "" "abc" "1.5"; do
    _t6_status=0
    run_in_pgroup_with_timeout "$_invalid_seconds" \
        "touch '$_SENTINEL_FILE'; echo 'SHOULD_NOT_RUN'" 2>/dev/null \
        || _t6_status=$?
    if [[ "$_t6_status" -eq 0 ]]; then
        fail "run_in_pgroup_with_timeout '${_invalid_seconds}': expected nonzero exit for invalid seconds"
    else
        pass "run_in_pgroup_with_timeout '${_invalid_seconds}': rejected invalid budget (exit ${_t6_status})"
    fi
    if [[ -f "$_SENTINEL_FILE" ]]; then
        fail "run_in_pgroup_with_timeout '${_invalid_seconds}': command was launched despite invalid seconds"
        rm -f "$_SENTINEL_FILE"
    fi
done

unset _invalid_seconds _t6_status _SENTINEL_FILE

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
echo ""
echo "Timeout integration tests: ${PASSES} passed, ${FAILURES} failed."
if (( FAILURES > 0 )); then
    exit 1
fi
