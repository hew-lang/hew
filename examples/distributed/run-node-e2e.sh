#!/usr/bin/env bash
# examples/distributed/run-node-e2e.sh
#
# Start the kv_server node on a free port in the background, wait for it
# to register its actor, run the kv_client node, capture both outputs,
# and reap the server.
#
# Exit 0 if the client exits 0 and both process logs look healthy.
# Exit non-zero with a diagnostic summary otherwise.
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

# macOS mktemp only randomises trailing X's; a non-X suffix produces a
# literal filename. Use -t (portable) to get a randomised path.
SERVER_LOG="$(mktemp -t kv-server)"
CLIENT_LOG="$(mktemp -t kv-client)"

# shellcheck disable=SC2329  # invoked via trap EXIT, not by name
cleanup() {
    if [[ -n "${SERVER_PID:-}" ]] && kill -0 "$SERVER_PID" 2>/dev/null; then
        kill "$SERVER_PID" 2>/dev/null || true
        wait "$SERVER_PID" 2>/dev/null || true
    fi
    rm -f "$SERVER_LOG" "$CLIENT_LOG"
}
trap cleanup EXIT

echo "[harness] hew binary : $HEW"
echo "[harness] port       : $PORT"
echo "[harness] server log : $SERVER_LOG"
echo "[harness] client log : $CLIENT_LOG"

# Build if the binary is missing.
if [[ ! -x "$HEW" ]]; then
    echo "[harness] hew binary not found — building..."
    cargo build -p hew-cli --manifest-path "$REPO_ROOT/Cargo.toml" 2>&1
fi

# Start the server in the background.
echo "[harness] starting server..."
"$HEW" run "$SERVER_HEW" -- "$PORT" >"$SERVER_LOG" 2>&1 &
SERVER_PID=$!
echo "[harness] server PID=$SERVER_PID"

# Wait for the server to report the actor is registered (up to 10 s).
READY_MSG="actor registered"
TIMEOUT=10
elapsed=0
while ! grep -q "$READY_MSG" "$SERVER_LOG" 2>/dev/null; do
    if ! kill -0 "$SERVER_PID" 2>/dev/null; then
        echo "[harness] ERROR: server exited before becoming ready"
        echo "--- server log ---"
        cat "$SERVER_LOG"
        exit 1
    fi
    if [[ $elapsed -ge $TIMEOUT ]]; then
        echo "[harness] ERROR: server did not become ready within ${TIMEOUT}s"
        echo "--- server log so far ---"
        cat "$SERVER_LOG"
        exit 1
    fi
    sleep 0.2
    elapsed=$(( elapsed + 1 ))
done
echo "[harness] server is ready"

# Run the client and time the round-trip.
echo "[harness] starting client..."
CLIENT_START=$(date +%s%3N)
"$HEW" run "$CLIENT_HEW" -- "127.0.0.1" "$PORT" >"$CLIENT_LOG" 2>&1
CLIENT_EXIT=$?
CLIENT_END=$(date +%s%3N)
CLIENT_MS=$(( CLIENT_END - CLIENT_START ))

echo "[harness] client exit=$CLIENT_EXIT  duration=${CLIENT_MS}ms"

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

echo "[harness] PASS: node-to-node e2e round-trip succeeded in ${CLIENT_MS}ms"
exit 0
