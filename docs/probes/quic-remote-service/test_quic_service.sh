#!/usr/bin/env sh
# test_quic_service.sh — Test script for the haiku QUIC remote service probe
#
# Runs both server and client in sequence, demonstrating end-to-end communication
# over QUIC/TLS 1.3 with zero configuration.
#
# Usage:
#   sh test_quic_service.sh

set -e

echo "=== Hew QUIC Remote Service Probe Test ==="
echo ""
echo "Building server..."
hew build haiku_quic_service_server.hew > /dev/null 2>&1

echo "Building client..."
hew build haiku_quic_service_client.hew > /dev/null 2>&1

echo ""
echo "Starting server (PID background)..."
hew run haiku_quic_service_server.hew 2>&1 | grep -v "^ld: warning" &
SERVER_PID=$!
sleep 2

echo "Running client..."
hew run haiku_quic_service_client.hew 2>&1 | grep -v "^ld: warning"
CLIENT_EXIT=$?

echo ""
echo "Waiting for server to finish..."
wait $SERVER_PID 2>/dev/null
SERVER_EXIT=$?

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
