#!/usr/bin/env bash
# Real MQTT publish/delivery oracle for examples/mqtt_broker.hew.
#
# Requires mosquitto_sub and mosquitto_pub. By default the script builds the
# broker with build/bin/hew; set HEW_BIN to another compiler, or set
# HEW_MQTT_BROKER_BIN to exercise an already-built broker counterfactual.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
WORK_DIR="$(mktemp -d "${TMPDIR:-/tmp}/hew-mqtt-broker-e2e.XXXXXX")"
BROKER_PID=""
SUB_PID=""
PUB_PID=""

cleanup() {
    local pid
    local i
    local any_alive
    for pid in "$PUB_PID" "$SUB_PID" "$BROKER_PID"; do
        if [[ -n "$pid" ]] && kill -0 "$pid" 2>/dev/null; then
            kill "$pid" 2>/dev/null || true
        fi
    done
    for ((i = 0; i < 20; i++)); do
        any_alive=0
        for pid in "$PUB_PID" "$SUB_PID" "$BROKER_PID"; do
            if [[ -n "$pid" ]] && kill -0 "$pid" 2>/dev/null; then
                any_alive=1
            fi
        done
        if [[ "$any_alive" == "0" ]]; then
            break
        fi
        sleep 0.1
    done
    for pid in "$PUB_PID" "$SUB_PID" "$BROKER_PID"; do
        if [[ -n "$pid" ]] && kill -0 "$pid" 2>/dev/null; then
            kill -KILL "$pid" 2>/dev/null || true
        fi
    done
    for pid in "$PUB_PID" "$SUB_PID" "$BROKER_PID"; do
        if [[ -n "$pid" ]]; then
            wait "$pid" 2>/dev/null || true
        fi
    done
    if [[ "${KEEP_MQTT_TEST_TMP:-0}" != "1" ]]; then
        rm -rf "$WORK_DIR"
    fi
}
trap cleanup EXIT

dump_diagnostics() {
    echo "mqtt-broker-e2e: diagnostics from $WORK_DIR" >&2
    for file in broker.log sub.out sub.err pub.out pub.err; do
        if [[ -f "$WORK_DIR/$file" ]]; then
            echo "==> $file" >&2
            sed -n '1,240p' "$WORK_DIR/$file" >&2
        fi
    done
}

fail() {
    echo "mqtt-broker-e2e: FAIL: $*" >&2
    dump_diagnostics
    exit 1
}

wait_for_log() {
    local pattern="$1"
    local attempts="${2:-100}"
    local i
    for ((i = 0; i < attempts; i++)); do
        if grep -q "$pattern" "$WORK_DIR/broker.log"; then
            return 0
        fi
        if [[ -n "$BROKER_PID" ]] && ! kill -0 "$BROKER_PID" 2>/dev/null; then
            return 1
        fi
        sleep 0.1
    done
    return 1
}

for command in mosquitto_sub mosquitto_pub perl; do
    command -v "$command" >/dev/null 2>&1 ||
        fail "required command not found: $command"
done

if [[ -n "${MQTT_TEST_PORT:-}" ]]; then
    PORT="$MQTT_TEST_PORT"
else
    PORT="$(
        perl -MIO::Socket::INET -e '
            my $socket = IO::Socket::INET->new(
                LocalAddr => "127.0.0.1",
                LocalPort => 0,
                Proto => "tcp",
                Listen => 1,
            ) or die "ephemeral port: $!\n";
            print $socket->sockport;
        '
    )"
fi

if [[ -n "${HEW_MQTT_BROKER_BIN:-}" ]]; then
    BROKER_BIN="$HEW_MQTT_BROKER_BIN"
    [[ -x "$BROKER_BIN" ]] ||
        fail "HEW_MQTT_BROKER_BIN is not executable: $BROKER_BIN"
else
    HEW="${HEW_BIN:-$REPO_ROOT/build/bin/hew}"
    [[ -x "$HEW" ]] || fail "Hew compiler is not executable: $HEW"
    BROKER_BIN="$WORK_DIR/mqtt_broker"
    "$HEW" build "$REPO_ROOT/examples/mqtt_broker.hew" -o "$BROKER_BIN" ||
        fail "broker build failed"
fi

PAYLOAD="hew-mqtt-delivery-$PORT"
TOPIC="hew/rc1/e2e"

"$BROKER_BIN" "$PORT" >"$WORK_DIR/broker.log" 2>&1 &
BROKER_PID=$!
wait_for_log "MQTT broker ready" ||
    fail "broker did not become ready within 10 seconds"

mosquitto_sub \
    -h 127.0.0.1 \
    -p "$PORT" \
    -t "$TOPIC" \
    -C 1 \
    -W 10 \
    >"$WORK_DIR/sub.out" \
    2>"$WORK_DIR/sub.err" &
SUB_PID=$!
wait_for_log "SUBSCRIBE processed" ||
    fail "subscriber did not complete CONNECT/SUBSCRIBE within 10 seconds"

mosquitto_pub \
    -h 127.0.0.1 \
    -p "$PORT" \
    -t "$TOPIC" \
    -m "$PAYLOAD" \
    >"$WORK_DIR/pub.out" \
    2>"$WORK_DIR/pub.err" &
PUB_PID=$!

delivered=0
for ((i = 0; i < 100; i++)); do
    if grep -qx "$PAYLOAD" "$WORK_DIR/sub.out"; then
        delivered=1
        break
    fi
    if ! kill -0 "$BROKER_PID" 2>/dev/null; then
        break
    fi
    sleep 0.1
done
[[ "$delivered" == "1" ]] ||
    fail "subscriber did not receive the exact publication within 10 seconds"

wait_for_log "PUBLISH topic=$TOPIC qos=0" 50 ||
    fail "broker did not parse the publication"
wait_for_log "\\[router\\] publish topic=$TOPIC" 50 ||
    fail "router did not fan out the publication"

for ((i = 0; i < 50; i++)); do
    disconnects="$(grep -c "\\[router\\] client disconnected" "$WORK_DIR/broker.log" || true)"
    if [[ "$disconnects" -ge 2 ]]; then
        break
    fi
    sleep 0.1
done
[[ "${disconnects:-0}" -ge 2 ]] ||
    fail "both MQTT clients did not complete disconnect cleanup"

kill -0 "$BROKER_PID" 2>/dev/null ||
    fail "broker exited during the round trip"

echo "mqtt-broker-e2e: PASS port=$PORT payload=$PAYLOAD"
