#!/usr/bin/env bash
# examples/distributed/run-node-e2e.sh
#
# Pre-compile both kv_server and kv_client, then start the server in the
# background, wait for it to register its actor, run the client, and report
# the result.
#
# Compiling first (sequentially) avoids two parallel LLVM codegen processes
# fighting over CPU and exceeding the server's readiness window.  Once
# compiled the binaries start in < 1 s each.
#
# Exit 0 if the client exits 0.  Exit non-zero with a diagnostic summary
# otherwise.
#
# Usage:
#   bash examples/distributed/run-node-e2e.sh [path/to/hew] [port]
#
# Defaults: HEW binary = ./target/debug/hew, PORT = 9100
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
HEW="${1:-$REPO_ROOT/target/debug/hew}"
PORT="${2:-9100}"

SERVER_HEW="$REPO_ROOT/examples/distributed/kv_server.hew"
CLIENT_HEW="$REPO_ROOT/examples/distributed/kv_client.hew"

# Temp dir for compiled binaries (cleaned up on exit).
BIN_DIR="$(mktemp -d -t kv-e2e-bins)"
SERVER_BIN="$BIN_DIR/kv_server"
CLIENT_BIN="$BIN_DIR/kv_client"

# macOS mktemp only randomises trailing X's; a non-X suffix produces a
# literal filename. Use -t (portable) to get a randomised path.
SERVER_LOG="$(mktemp -t kv-server)"
CLIENT_LOG="$(mktemp -t kv-client)"

# shellcheck disable=SC2329  # invoked via trap EXIT, not by name
cleanup() {
    if [[ -n "${SERVER_PID:-}" ]] && kill -0 "$SERVER_PID" 2>/dev/null; then
        kill "$SERVER_PID" 2>/dev/null || true
        # Bounded wait: give the server up to 3 s to exit after SIGTERM, then
        # SIGKILL to ensure the trap never hangs (the server's sleep_ms(120000)
        # would otherwise block `wait` for two minutes).
        local deadline=$(( $(date +%s) + 3 ))
        while kill -0 "$SERVER_PID" 2>/dev/null && [[ $(date +%s) -lt $deadline ]]; do
            sleep 0.1
        done
        if kill -0 "$SERVER_PID" 2>/dev/null; then
            kill -9 "$SERVER_PID" 2>/dev/null || true
        fi
        wait "$SERVER_PID" 2>/dev/null || true
    fi
    rm -f "$SERVER_LOG" "$CLIENT_LOG"
    rm -rf "$BIN_DIR"
}
trap cleanup EXIT

echo "[harness] hew binary : $HEW"
echo "[harness] port       : $PORT"
echo "[harness] server log : $SERVER_LOG"
echo "[harness] client log : $CLIENT_LOG"

# Build hew-cli if the binary is missing.
if [[ ! -x "$HEW" ]]; then
    echo "[harness] hew binary not found — building..."
    cargo build -p hew-cli --manifest-path "$REPO_ROOT/Cargo.toml" 2>&1
fi

# Compile from REPO_ROOT so the module resolver finds this worktree's std/
# rather than a sibling worktree's (the CWD candidate is checked before the
# in-worktree tier-2 probe).
HEW_COMPILE() { (cd "$REPO_ROOT" && "$HEW" compile --emit-dir "$BIN_DIR" "$@"); }

# Pre-compile both programs sequentially.  This keeps peak CPU under control
# and ensures the server binary starts in < 1 s so the readiness window is
# not wasted on compilation.
echo "[harness] compiling server..."
COMPILE_START=$(date +%s)
HEW_COMPILE "$SERVER_HEW" 2>&1
echo "[harness] server compiled in $(( $(date +%s) - COMPILE_START ))s"

echo "[harness] compiling client..."
COMPILE_START=$(date +%s)
HEW_COMPILE "$CLIENT_HEW" 2>&1
echo "[harness] client compiled in $(( $(date +%s) - COMPILE_START ))s"

if [[ ! -x "$SERVER_BIN" || ! -x "$CLIENT_BIN" ]]; then
    echo "[harness] ERROR: compiled binaries not found in $BIN_DIR"
    ls -la "$BIN_DIR"
    exit 1
fi

# Start the server in the background.
echo "[harness] starting server..."
HEW_TRANSPORT=tcp "$SERVER_BIN" "$PORT" >"$SERVER_LOG" 2>&1 &
SERVER_PID=$!
echo "[harness] server PID=$SERVER_PID"

# Wait for the server to report the actor is registered (up to 15 s).
# Once compiled, the server starts in well under 1 s; 15 s is generous.
# Wall-clock tracking via `date +%s` avoids the original `elapsed += 1 per
# 0.2 s` bug that gave only ~2 s of effective timeout.
READY_MSG="actor registered"
READY_TIMEOUT=15
SERVER_START_S=$(date +%s)
while ! grep -q "$READY_MSG" "$SERVER_LOG" 2>/dev/null; do
    if ! kill -0 "$SERVER_PID" 2>/dev/null; then
        echo "[harness] ERROR: server exited before becoming ready"
        echo "--- server log ---"
        cat "$SERVER_LOG"
        exit 1
    fi
    elapsed_s=$(( $(date +%s) - SERVER_START_S ))
    if [[ $elapsed_s -ge $READY_TIMEOUT ]]; then
        echo "[harness] ERROR: server did not become ready within ${READY_TIMEOUT}s"
        echo "--- server log so far ---"
        cat "$SERVER_LOG"
        exit 1
    fi
    sleep 0.2
done
echo "[harness] server is ready"

# Run the client and time the round-trip.
echo "[harness] starting client..."
CLIENT_START=$(date +%s)
HEW_TRANSPORT=tcp "$CLIENT_BIN" "127.0.0.1" "$PORT" >"$CLIENT_LOG" 2>&1
CLIENT_EXIT=$?
CLIENT_ELAPSED=$(( $(date +%s) - CLIENT_START ))

echo "[harness] client exit=$CLIENT_EXIT  duration=${CLIENT_ELAPSED}s"

# Allow the server a moment to finish its own cleanup.
sleep 2

echo ""
echo "=== SERVER OUTPUT ==="
cat "$SERVER_LOG"
echo ""
echo "=== CLIENT OUTPUT ==="
cat "$CLIENT_LOG"
echo ""

if [[ $CLIENT_EXIT -ne 0 ]]; then
    echo "[harness] FAIL: client exited $CLIENT_EXIT"
    exit $CLIENT_EXIT
fi

echo "[harness] PASS: node-to-node e2e round-trip succeeded in ${CLIENT_ELAPSED}s"
exit 0
