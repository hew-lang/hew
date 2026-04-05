#!/usr/bin/env sh
# test_quic_service.sh — Test script for the archived QUIC remote service probe
#
# Runs both server and client in sequence, demonstrating end-to-end communication
# over QUIC/TLS 1.3 with zero configuration.
#
# Usage:
#   sh test_quic_service.sh
#   HEW_QUIC_SERVICE_PORT=4545 sh test_quic_service.sh

set -e

SCRIPT_DIR=$(CDPATH='' cd -- "$(dirname "$0")" && pwd)
cd "$SCRIPT_DIR"
REPO_STD=$(CDPATH='' cd -- "$SCRIPT_DIR/../../.." && pwd)/std
export HEW_STD="$REPO_STD"

SERVER_PID=""
trap 'if [ -n "$SERVER_PID" ]; then kill "$SERVER_PID" 2>/dev/null || true; fi' EXIT INT TERM

echo "=== Hew QUIC Remote Service Probe Test ==="
echo ""
echo "Building server..."
hew build service_server.hew > /dev/null 2>&1

echo "Building client..."
hew build service_client.hew > /dev/null 2>&1

echo ""
echo "Starting server (PID background)..."
hew run service_server.hew &
SERVER_PID=$!
sleep 2

echo "Running client..."
if hew run service_client.hew; then
    CLIENT_EXIT=0
else
    CLIENT_EXIT=$?
fi

echo ""
echo "Waiting for server to finish..."
if wait "$SERVER_PID" 2>/dev/null; then
    SERVER_EXIT=0
else
    SERVER_EXIT=$?
fi
SERVER_PID=""

echo ""
if [ $CLIENT_EXIT -eq 0 ] && [ $SERVER_EXIT -eq 0 ]; then
    echo "✅ SUCCESS: Both client and server completed successfully"
    echo ""
    echo "Key observations:"
    echo "  - QUIC connection established and TLS handshake completed"
    echo "  - Bidirectional stream communication successful"
    echo "  - Message round-trip verified (client ↔ server)"
    echo "  - Clean shutdown of all resources"
    exit 0
else
    echo "❌ FAILURE: Client exit=$CLIENT_EXIT, Server exit=$SERVER_EXIT"
    exit 1
fi
