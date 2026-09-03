#!/usr/bin/env bash
# repros/journeys/week-one-local.sh
# Day three: the first service. One node, a supervised store, the HTTP loop in
# an actor with a timed ask, a bad request answered without an exit, an operator
# crash the supervisor recovers from, and the metrics that show it. Standard
# library only, no registry, no cluster.
# Source: hew-orchestration/plans/hew-platform-program.md §0.1 (the bash
# fence starting at that document's line 665), copied verbatim except for
# the step-reporting harness described below (V060-FD-1). The package under
# repros/journeys/week-one-local/ (hew.toml, main.hew, main_test.hew) is
# copied the same way from that section's file-headered fences (lines 434,
# 447, 638) — Sites for V060-FD-1 name only the three *.sh scripts, but
# week-one-local.sh has no journey to run without its own package, so this
# fixture directory travels with it.
#
# Run: make test-journeys JOURNEY=week-one-local   (HEW_BIN on PATH, TMPDIR outside any hew root)
# Ports come from JOURNEY_PORT_BASE (default 8080) so a busy port or a parallel shard cannot fail the gate.
#
# Every assertion is a numbered step (`week-one-local.<n>`), printed to stdout
# as `step week-one-local.<n>: pass` or `step week-one-local.<n>: fail`. A
# failing step is recorded and the script continues; scripts/journeys-expected.tsv
# is the ratchet that decides whether a given failure is already known. No
# `-e`: a failure here must never abort the script before every step is reported.
set -uo pipefail
HEW=${HEW_BIN:-hew}
SRC=$(cd "$(dirname "$0")/week-one-local" && pwd) || exit 1
PORT=${JOURNEY_PORT_BASE:-8080}
WORK=$(mktemp -d "${TMPDIR:?TMPDIR must point outside the checkout}/journey-week-one-local-XXXXXX") || exit 1
SVC=
# shellcheck disable=SC2329 # invoked indirectly via `trap cleanup EXIT` below
cleanup() {
    if [ -n "$SVC" ]; then
        kill "$SVC" 2>/dev/null || true
        wait "$SVC" 2>/dev/null || true
    fi
    rm -rf "$WORK"
}
trap cleanup EXIT

NAME=week-one-local
N=0
ok() {
    N=$((N + 1))
    echo "step ${NAME}.${N}: pass"
}
bad() {
    N=$((N + 1))
    echo "step ${NAME}.${N}: fail"
    echo "${NAME}: step ${N} FAIL: $*" >&2
}
until_ok() {
    local tries=$1
    shift
    for _ in $(seq "$tries"); do
        "$@" && return 0
        sleep 1
    done
    return 1
}
# `producer | grep -q` under pipefail fails when the producer writes after the match; capture first.
code() { curl -s -m 5 -o /dev/null -w '%{http_code}' "$@"; }
has() {
    local out
    out=$(curl -s -m 5 "$1") || return 1
    echo "$out" | grep -qE "$2"
}
# Per-handler attribution is opt-in (§4.6); the service runs with it on.
export HEW_OBSERVE=1

cp -r "$SRC" "$WORK/kvlocal" && cd "$WORK/kvlocal" || exit 1
OUT=$("$HEW" test 2>&1)
if echo "$OUT" | grep -q '3 passed; 0 failed'; then ok; else bad "package tests: $OUT"; fi
OUT=$("$HEW" build --release 2>&1)
if echo "$OUT" | grep -q 'target/release/kvlocal'; then ok; else bad "build: $OUT"; fi

# 1. Up: one binary hosts the supervised store and the HTTP actor.
KV_DB="$WORK/kv.json" KV_ADDR="127.0.0.1:$PORT" ./target/release/kvlocal >"$WORK/svc.log" 2>&1 &
SVC=$!
HEALTH_URL="localhost:${PORT}/health"
if until_ok 20 sh -c "[ \"\$(curl -s -m 2 -o /dev/null -w '%{http_code}' $HEALTH_URL)\" = 200 ]"; then ok; else bad "service never came up: $(cat "$WORK/svc.log" 2>/dev/null)"; fi

# 2. http + json + the store, through a timed ask (`await .. | after 2s` inside the Api actor).
if [ "$(code -X POST -d '{"key":"a","val":"1"}' localhost:"$PORT"/items)" = 201 ]; then ok; else bad "POST /items"; fi
if has localhost:"$PORT"/items/a '"val":"1"'; then ok; else bad "GET /items/a"; fi
if [ "$(code localhost:"$PORT"/items/zzz)" = 404 ]; then ok; else bad "GET missing is 404"; fi

# 3. A bad request is a response, never an exit: 400, and the service is still up.
if [ "$(code -X POST -d 'not json' localhost:"$PORT"/items)" = 400 ]; then ok; else bad "bad body is a 400"; fi
if [ "$(code localhost:"$PORT"/health)" = 200 ]; then ok; else bad "service died on a bad body: $(cat "$WORK/svc.log" 2>/dev/null)"; fi

# 4. The operator's view: per-handler attribution under the declaration-dotpath label (§4.6, v0.6.0).
if has localhost:"$PORT"/metrics 'handler="Store.handle"'; then ok; else bad "per-handler attribution"; fi

# 5. Crash the store: the supervisor restarts it, #[on(start)] reloads the snapshot, the data is
#    intact, and the restart is counted per child (§4.4, v0.6.0).
if [ "$(code -X POST localhost:"$PORT"/admin/crash)" = 202 ]; then ok; else bad "POST /admin/crash"; fi
if until_ok 10 has localhost:"$PORT"/items/a '"val":"1"'; then ok; else bad "store did not come back with its data: $(cat "$WORK/svc.log" 2>/dev/null)"; fi
if has localhost:"$PORT"/metrics 'supervisor_restarts_by_child_total\{[^}]*child="store"[^}]*\} 1'; then ok; else bad "restart is not visible in /metrics"; fi
if [ "$(grep -c 'store: up' "$WORK/svc.log" 2>/dev/null)" -eq 2 ]; then ok; else bad "log does not show the second start: $(cat "$WORK/svc.log" 2>/dev/null)"; fi

# 6. The snapshot on disk is the store's state: a second process started on it serves the data.
kill "$SVC" 2>/dev/null
wait "$SVC" 2>/dev/null || true
SVC=
KV_DB="$WORK/kv.json" KV_ADDR="127.0.0.1:$PORT" ./target/release/kvlocal >"$WORK/svc2.log" 2>&1 &
SVC=$!
if until_ok 20 has localhost:"$PORT"/items/a '"val":"1"'; then ok; else bad "restarted process did not serve the snapshot: $(cat "$WORK/svc2.log" 2>/dev/null)"; fi

echo "week-one-local: ${N} steps reported"
exit 0
