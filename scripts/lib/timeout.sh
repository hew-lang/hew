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
# Sets TIMEOUT_CMD array: ("binary" ["--kill-after=5"]) and TIMEOUT_BACKEND.
_pick_timeout_cmd() {
    local bin
    for bin in timeout gtimeout; do
        command -v "$bin" >/dev/null 2>&1 || continue
        # Probe: run 'true' with a 10s budget; GNU timeout accepts --kill-after here.
        if "$bin" --kill-after=1 10 true 2>/dev/null; then
            TIMEOUT_BACKEND="cmd"
            TIMEOUT_CMD=("$bin" --kill-after=5)
            return 0
        fi
    done
    # Bare timeout/gtimeout without --kill-after support are intentionally
    # skipped: they only kill the direct child process, not the full process
    # group, so cargo/make grandchildren survive and hold artifact locks.
    # Fall through to the Perl process-group-safe backend.
    command -v perl >/dev/null 2>&1 || {
        echo "error: GNU timeout (with --kill-after) or perl is required for bounded execution" >&2
        exit 1
    }
    TIMEOUT_BACKEND="perl"
    TIMEOUT_CMD=()
}
TIMEOUT_BACKEND=""
TIMEOUT_CMD=()
_pick_timeout_cmd
unset -f _pick_timeout_cmd

run_with_timeout() {
    local seconds="$1"
    shift
    local status
    if [[ "$TIMEOUT_BACKEND" == "perl" ]]; then
        perl -e '
            use strict;
            use warnings;
            use POSIX qw(setpgid);

            my ($seconds, $kill_after, @cmd) = @ARGV;
            die "timeout requires a command\n" unless @cmd;

            my $pid = fork();
            die "fork failed: $!\n" unless defined $pid;

            if ($pid == 0) {
                setpgid(0, 0) or die "setpgid failed: $!\n";
                exec @cmd;
                die "exec failed: $!\n";
            }
            setpgid($pid, $pid);

            my $timed_out = 0;
            my $hard_kill = 0;
            local $SIG{ALRM} = sub {
                if (!$timed_out) {
                    $timed_out = 1;
                    kill "TERM", -$pid;
                    alarm $kill_after;
                    return;
                }
                $hard_kill = 1;
                kill "KILL", -$pid;
            };

            alarm $seconds;
            waitpid($pid, 0);
            alarm 0;

            if ($? == -1) {
                exit 1;
            }
            if ($timed_out) {
                exit($hard_kill ? 137 : 124);
            }
            if ($? & 127) {
                exit 128 + ($? & 127);
            }
            exit $? >> 8;
        ' "$seconds" 5 "$@"
        status=$?
    else
        "${TIMEOUT_CMD[@]}" "$seconds" "$@"
        status=$?
    fi
    # 124 = soft timeout; 137 = SIGKILL from --kill-after fired
    if [[ "$status" -eq 124 || "$status" -eq 137 ]]; then
        echo "FATAL: timed out after ${seconds}s: $*" >&2
    fi
    return "$status"
}

# run_in_pgroup_with_timeout <seconds> <cmd_string>
#
# Runs <cmd_string> via "bash -lc" in a dedicated process group, with a
# SIGTERM-then-SIGKILL watchdog implemented in a single Perl process.
#
# Designed for the dispatcher's use case: bash -lc forks cargo/make/ctest
# grandchildren that must all die on timeout.  kill(-$pgid) reaches every
# process in the tree simultaneously, preventing artifact-lock contention
# from orphaned cargo processes.
#
# <seconds> MUST be a positive integer (>= 1).  Zero or negative values are
# rejected before the command is launched: alarm(0) in Perl cancels the
# watchdog, which would allow the command to run without any time budget —
# a silent preflight bypass.  Empty, non-numeric, or fractional values are
# also rejected.  All invalid inputs produce a diagnostic on stderr and
# return exit code 1 without running the command.
#
# Exit-code semantics differ from run_with_timeout INTENTIONALLY:
#   Signal exits are surfaced raw — no translation to 124/137.
#   SIGTERM kill → 143 (128+15)   SIGKILL fallback → 137 (128+9)
#
# Three independent assertions lock this contract and must not be relaxed:
#   scripts/tests/test_ci_preflight_timeout.sh (Test 5): exit ∈ {143, 137}
#   scripts/tests/test_ci_preflight_dispatcher.py:       JSON status ∈ {137, 143}
#   scripts/ci-preflight-dispatcher.sh:                  ==> TIMEOUT: keyed on 137||143
run_in_pgroup_with_timeout() {
    local seconds="$1"
    local cmd_string="$2"
    # Validate seconds before touching Perl.  alarm(0) disables the watchdog;
    # alarm(negative) is undefined in Perl.  Both would let the command run
    # unbounded, silently defeating the preflight budget.
    if [[ ! "$seconds" =~ ^[1-9][0-9]*$ ]]; then
        echo "error: run_in_pgroup_with_timeout: seconds must be a positive integer (>= 1), got '${seconds}'" >&2
        return 1
    fi
    perl -e '
        use strict;
        use warnings;
        use POSIX qw(setpgid);

        my ($seconds, $cmd_string) = @ARGV;
        die "run_in_pgroup_with_timeout: seconds and cmd_string required\n"
            unless defined $cmd_string;

        my $pid = fork();
        die "fork failed: $!\n" unless defined $pid;

        if ($pid == 0) {
            setpgid(0, 0) or die "setpgid: $!\n";
            exec "bash", "-lc", $cmd_string;
            die "exec failed: $!\n";
        }
        setpgid($pid, $pid);

        my $timed_out = 0;
        local $SIG{ALRM} = sub {
            if (!$timed_out) {
                $timed_out = 1;
                kill "TERM", -$pid;
                alarm 5;
                return;
            }
            kill "KILL", -$pid;
        };

        alarm $seconds;
        waitpid($pid, 0);
        alarm 0;

        if ($? == -1) {
            exit 1;
        }
        # Surface raw signal exit codes.  Do NOT translate to 124/137 as
        # run_with_timeout does: the dispatcher and its tests key on 143/137.
        if ($? & 127) {
            exit 128 + ($? & 127);
        }
        exit $? >> 8;
    ' "$seconds" "$cmd_string"
}
