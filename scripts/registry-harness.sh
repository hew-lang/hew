#!/usr/bin/env bash
# scripts/registry-harness.sh <cmd...>
#
# Starts a disposable local hew-registry dev server (`wrangler dev`, local
# simulated KV/R2/Durable Objects, no cloud credentials), exports
# HEW_REGISTRY to point at it, runs <cmd...> with that in its environment,
# and tears the server down afterwards regardless of how <cmd...> exited.
#
# The registry checkout is a sibling of this repo by convention
# (HEW_SYNC_REGISTRY overrides), matching scripts/sync-downstream.sh's
# sibling-directory pattern for the other downstream repos. When `wrangler`
# is not on PATH, the checkout is missing, or the dev server never comes up,
# this prints one line explaining why and exits 75 (EX_UNAVAILABLE) without
# running <cmd...> — the caller (scripts/run-journeys.sh, day-two's own
# per-step registry guards) treats that as "no local registry" and records
# the registry-dependent steps as failed rather than hanging on a dead
# endpoint (V060-FD-1).
set -uo pipefail

EX_UNAVAILABLE=75

if [ "$#" -eq 0 ]; then
    echo "registry-harness: usage: registry-harness.sh <cmd...>" >&2
    exit 64
fi

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
# The sibling-repos directory is the main checkout's parent, not this
# script's own parent: a worktree under <repo>/.claude/worktrees/<name>/
# has REPO_ROOT there, whose parent is .claude/worktrees, never the
# sibling checkouts. `git rev-parse --git-common-dir` names the main
# repo's .git regardless of which worktree runs this script, so two
# levels up from it is the sibling-repos parent in every worktree.
GIT_COMMON_DIR="$(cd "$REPO_ROOT" && git rev-parse --git-common-dir 2>/dev/null)"
case "$GIT_COMMON_DIR" in
/*) ;;
"") ;;
*) GIT_COMMON_DIR="$REPO_ROOT/$GIT_COMMON_DIR" ;;
esac
if [ -n "$GIT_COMMON_DIR" ]; then
    DOWNSTREAM_PARENT="$(dirname "$(dirname "$GIT_COMMON_DIR")")"
else
    DOWNSTREAM_PARENT="$(dirname "$REPO_ROOT")"
fi
REGISTRY_DIR="${HEW_SYNC_REGISTRY:-$DOWNSTREAM_PARENT/hew-registry}"
WORKER_DIR="$REGISTRY_DIR/registry-worker"

unavailable() {
    echo "registry-harness: $* — registry-dependent journey steps run as failed" >&2
    exit "$EX_UNAVAILABLE"
}

command -v wrangler >/dev/null 2>&1 || unavailable "wrangler not found on PATH"
[ -d "$WORKER_DIR" ] || unavailable "no hew-registry checkout at $REGISTRY_DIR"
command -v python3 >/dev/null 2>&1 || unavailable "python3 not found on PATH (needed to pick a free port)"

PORT=$(python3 -c 'import socket; s = socket.socket(); s.bind(("127.0.0.1", 0)); print(s.getsockname()[1]); s.close()' 2>/dev/null)
[ -n "$PORT" ] || unavailable "could not find a free port"

LOG=$(mktemp "${TMPDIR:-/tmp}/registry-harness-log-XXXXXX")
(cd "$WORKER_DIR" && exec wrangler dev --port "$PORT" >"$LOG" 2>&1) &
WRANGLER_PID=$!

cleanup() {
    kill "$WRANGLER_PID" 2>/dev/null || true
    wait "$WRANGLER_PID" 2>/dev/null || true
    rm -f "$LOG"
}
trap cleanup EXIT

# A hang is a failure, not a wait: give the dev server 30s to accept connections.
UP=0
for _ in $(seq 1 30); do
    if curl -s -m 1 -o /dev/null "http://127.0.0.1:$PORT/"; then
        UP=1
        break
    fi
    kill -0 "$WRANGLER_PID" 2>/dev/null || break
    sleep 1
done
if [ "$UP" -ne 1 ]; then
    echo "registry-harness: wrangler dev on port $PORT did not come up:" >&2
    cat "$LOG" >&2
    exit "$EX_UNAVAILABLE"
fi

export HEW_REGISTRY="http://127.0.0.1:${PORT}/api/v1"
"$@"
