#!/usr/bin/env bash
# Counterfactual for the actor-box balance check (hew-runtime/src/actor_balance.rs).
#
# Accounting that has quietly stopped counting passes every fixture while
# proving nothing, so the check has to be shown failing before any gate trusts
# it. `HEW_ACTOR_LEAK_SELFTEST=skip-free` makes the runtime's shutdown sweep
# omit the free of exactly one actor it would otherwise reclaim; the same
# program must then exit HEW_EXIT_ACTOR_LEAK (93).
#
# The retired checked-mir harness ran this counterfactual before trusting the
# check on its corpus. The corpus is now core-acceptance cases, whose runner
# arms `HEW_ACTOR_LEAK_CHECK=1` per case but has no place for a deliberately
# leaking run, so the counterfactual lives here.
#
# WHEN OBSOLETE: when the balance check is always-on rather than opt-in, and
# the runtime's own tests can drive a real shutdown sweep end to end.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
# shellcheck source=scripts/lib/cargo-output-dir.sh
# shellcheck disable=SC1091
source "$REPO_ROOT/scripts/lib/cargo-output-dir.sh"
HEW_BIN="${HEW_BIN:-$(cargo_debug_dir "$REPO_ROOT")/hew}"
ACTOR_LEAK_EXIT=93

if [[ ! -x "$HEW_BIN" ]]; then
    echo "error: hew binary not found at $HEW_BIN (build it with: make hew-debug)" >&2
    exit 1
fi

WORK="$(mktemp -d "${TMPDIR:-/tmp}/hew-actor-leak-counterfactual.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT

cat >"$WORK/probe.hew" <<'HEW'
actor Counter {
    var total: i64 = 0,
    receive fn bump(n: i64) {
        total = total + n;
    }
}

fn main() {
    let counter = spawn Counter(total: 0);
    let _ = counter.bump(1);
    println("done");
}
HEW

"$HEW_BIN" compile --emit-dir "$WORK/emit" "$WORK/probe.hew" >"$WORK/compile.log" 2>&1 || {
    echo "FAIL: the probe program does not build" >&2
    cat "$WORK/compile.log" >&2
    exit 1
}

run_probe() {
    local status=0
    env HEW_ACTOR_LEAK_CHECK=1 "$@" "$WORK/emit/probe" >"$WORK/run.out" 2>"$WORK/run.err" || status=$?
    printf '%s' "$status"
}

baseline="$(run_probe)"
if [[ "$baseline" -eq "$ACTOR_LEAK_EXIT" ]]; then
    echo "FAIL: the probe leaks an actor on its own (exit $baseline); the counterfactual proves nothing" >&2
    cat "$WORK/run.err" >&2
    exit 1
fi

leaked="$(run_probe HEW_ACTOR_LEAK_SELFTEST=skip-free)"
if [[ "$leaked" -ne "$ACTOR_LEAK_EXIT" ]]; then
    echo "FAIL: with one actor free omitted the probe exited $leaked, expected $ACTOR_LEAK_EXIT" >&2
    echo "  the actor-box balance check is not catching a leaked actor" >&2
    cat "$WORK/run.err" >&2
    exit 1
fi

echo "PASS: actor-box balance check reports a leaked actor (baseline exit $baseline; one free omitted -> exit $leaked)"
