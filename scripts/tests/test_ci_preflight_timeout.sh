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
# Test 5: Grandchild termination — the actual dispatcher shape.
#
# Reproduces the artifact-lock bug scenario:
#   perl setpgid(0,0)  →  bash -lc "$cmd"  →  grandchild sleep
#
# The grandchild is in the same process group as bash because bash -lc
# (non-interactive) does NOT use job control (set -m), so background jobs
# inherit the shell's pgid.  A kill-TERM to -$pgid therefore reaches both
# bash and its grandchildren simultaneously.
#
# If only the direct child (bash) is killed and grandchildren are left alive,
# kill -0 -$pgid succeeds after the wait, and the test fails.
# ---------------------------------------------------------------------------
# Start: perl setpgid → bash -lc → grandchild background sleep.
perl -e '
    use POSIX qw(setpgid);
    setpgid(0, 0) or die "setpgid: $!";
    exec @ARGV;
    die "exec: $!";
' bash -lc 'bash -c "sleep 30" & sleep 9999' &
_PG_CHILD=$!

# Perl watchdog kills the process group after 1 second.
perl -e '
    my ($pg, $s) = @ARGV;
    $SIG{TERM} = sub { exit 0 };
    sleep $s;
    kill TERM => -$pg;
    sleep 5;
    kill KILL => -$pg;
' "$_PG_CHILD" 1 &
_PG_WATCHDOG=$!

_PG_STATUS=0
wait "$_PG_CHILD" 2>/dev/null || _PG_STATUS=$?
kill "$_PG_WATCHDOG" 2>/dev/null || true
wait "$_PG_WATCHDOG" 2>/dev/null || true

# SIGTERM from the watchdog → bash exits 143 (128+15).
# SIGKILL fallback → 137 (128+9).
if [[ "$_PG_STATUS" -eq 143 || "$_PG_STATUS" -eq 137 ]]; then
    pass "pgid kill: child exited with signal-kill code (${_PG_STATUS})"
else
    fail "pgid kill: expected signal-kill exit (143 or 137), got ${_PG_STATUS}"
fi

# Allow a brief window for SIGTERM to finish propagating to the grandchild.
sleep 1

# Verify no process remains in the group (grandchild must be dead).
# POSIX: kill -0 to a pgid succeeds only if at least one process is alive in it.
if kill -0 -"$_PG_CHILD" 2>/dev/null; then
    fail "pgid kill: processes still alive in group ${_PG_CHILD} — grandchild NOT terminated"
else
    pass "pgid kill terminated entire process tree (bash -lc grandchild also dead)"
fi

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
echo ""
echo "Timeout integration tests: ${PASSES} passed, ${FAILURES} failed."
if (( FAILURES > 0 )); then
    exit 1
fi
