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
    # Fallback: use whatever is available without --kill-after (e.g. BSD/wrapper)
    for bin in timeout gtimeout; do
        command -v "$bin" >/dev/null 2>&1 || continue
        TIMEOUT_BACKEND="cmd"
        TIMEOUT_CMD=("$bin")
        return 0
    done
    command -v perl >/dev/null 2>&1 || {
        echo "error: timeout, gtimeout, or perl is required for bounded execution" >&2
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
