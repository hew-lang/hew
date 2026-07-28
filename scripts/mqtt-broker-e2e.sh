#!/usr/bin/env bash
# Real MQTT publish/delivery oracle for examples/mqtt_broker.hew.
#
# Requires mosquitto_sub and mosquitto_pub. By default the script builds the
# broker with target/debug/hew; set HEW_BIN to another compiler, or set
# HEW_MQTT_BROKER_BIN to exercise an already-built broker counterfactual.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
WORK_DIR="$(mktemp -d "${TMPDIR:-/tmp}/hew-mqtt-broker-e2e.XXXXXX")"
BROKER_PID=""
SUB_PID=""
PUB_PID=""
DUP_PID=""

cleanup() {
    local pid
    local i
    local any_alive
    for pid in "$DUP_PID" "$PUB_PID" "$SUB_PID" "$BROKER_PID"; do
        if [[ -n "$pid" ]] && kill -0 "$pid" 2>/dev/null; then
            kill "$pid" 2>/dev/null || true
        fi
    done
    for ((i = 0; i < 20; i++)); do
        any_alive=0
        for pid in "$DUP_PID" "$PUB_PID" "$SUB_PID" "$BROKER_PID"; do
            if [[ -n "$pid" ]] && kill -0 "$pid" 2>/dev/null; then
                any_alive=1
            fi
        done
        if [[ "$any_alive" == "0" ]]; then
            break
        fi
        sleep 0.1
    done
    for pid in "$DUP_PID" "$PUB_PID" "$SUB_PID" "$BROKER_PID"; do
        if [[ -n "$pid" ]] && kill -0 "$pid" 2>/dev/null; then
            kill -KILL "$pid" 2>/dev/null || true
        fi
    done
    for pid in "$DUP_PID" "$PUB_PID" "$SUB_PID" "$BROKER_PID"; do
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
    for file in broker.log sub.out sub.err dup.out dup.err pub.out pub.err; do
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
    HEW="${HEW_BIN:-$REPO_ROOT/target/debug/hew}"
    [[ -x "$HEW" ]] || fail "Hew compiler is not executable: $HEW"
    BROKER_BIN="$WORK_DIR/mqtt_broker"
    "$HEW" build "$REPO_ROOT/examples/mqtt_broker.hew" -o "$BROKER_BIN" ||
        fail "broker build failed"
fi

PAYLOAD="hew-mqtt-delivery-$PORT"
REJECTED_PAYLOAD="hew-mqtt-duplicate-must-not-deliver-$PORT"
TOPIC="hew/rc1/e2e"
DUPLICATE_ID="hew-duplicate-$PORT"
PUBLISHER_ID="hew-publisher-$PORT"
FORGED_CONNECT_PROBES=32

"$BROKER_BIN" "$PORT" >"$WORK_DIR/broker.log" 2>&1 &
BROKER_PID=$!
wait_for_log "MQTT broker ready" ||
    fail "broker did not become ready within 10 seconds"

# A fifth Remaining Length byte is forbidden by MQTT 3.1.1. The broker must
# close the offending connection instead of retaining its unbounded prefix,
# and the acceptor must remain available for subsequent valid clients.
perl -MIO::Socket::INET -e '
    $SIG{ALRM} = sub { die "timed out waiting for malformed connection close\n" };
    alarm 3;
    my $socket = IO::Socket::INET->new(
        PeerAddr => "127.0.0.1",
        PeerPort => $ARGV[0],
        Proto => "tcp",
    ) or die "malformed probe connect: $!\n";
    print {$socket} pack("C*", 0x30, 0x80, 0x80, 0x80, 0x80)
        or die "malformed probe write: $!\n";
    shutdown($socket, 1) or die "malformed probe shutdown: $!\n";
    my $read = sysread($socket, my $byte, 1);
    die "malformed probe read: $!\n" unless defined $read;
    die "broker retained malformed connection\n" unless $read == 0;
    alarm 0;
' "$PORT" ||
    fail "malformed Remaining Length was not rejected"
wait_for_log "malformed remaining length" ||
    fail "broker did not classify the malformed Remaining Length"
kill -0 "$BROKER_PID" 2>/dev/null ||
    fail "broker exited after rejecting malformed Remaining Length"

# Every MQTT string length is peer-controlled. Repeated minimal CONNECT frames
# with a forged 65,535-byte protocol-name length must close cleanly instead of
# panicking the handler and stranding one descriptor/router row per packet.
perl -MIO::Socket::INET -e '
    $SIG{ALRM} = sub { die "timed out waiting for forged CONNECT close\n" };
    my @frames = (
        [ "PUBLISH topic length",    0x30, 0x02, 0xFF, 0xFF ],
        [ "SUBSCRIBE topic length",  0x82, 0x04, 0x00, 0x01, 0xFF, 0xFF ],
        [ "UNSUBSCRIBE topic length",0xA2, 0x04, 0x00, 0x01, 0xFF, 0xFF ],
        [ "PINGREQ body",            0xC0, 0x01, 0x00 ],
        [ "DISCONNECT body",         0xE0, 0x01, 0x00 ],
        [ "unsupported packet",      0xF0, 0x00 ],
        [ "oversize Remaining Length", 0x30, 0x81, 0x80, 0x40 ],
    );
    for my $probe (1 .. $ARGV[1] + scalar @frames) {
        alarm 3;
        my $socket = IO::Socket::INET->new(
            PeerAddr => "127.0.0.1",
            PeerPort => $ARGV[0],
            Proto => "tcp",
        ) or die "forged CONNECT $probe connect: $!\n";
        my @frame = $probe <= $ARGV[1]
            ? (0x10, 0x04, 0xFF, 0xFF, 0x00, 0x00)
            : @{$frames[$probe - $ARGV[1] - 1]}[1 .. $#{$frames[$probe - $ARGV[1] - 1]}];
        print {$socket} pack("C*", @frame)
            or die "malformed packet $probe write: $!\n";
        shutdown($socket, 1)
            or die "malformed packet $probe shutdown: $!\n";
        my $read = sysread($socket, my $byte, 1);
        die "malformed packet $probe read: $!\n" unless defined $read;
        die "broker retained malformed packet $probe\n" unless $read == 0;
        alarm 0;
    }
' "$PORT" "$FORGED_CONNECT_PROBES" ||
    fail "malformed MQTT length/body probe was not rejected"
wait_for_log "malformed CONNECT" ||
    fail "broker did not classify the forged CONNECT"
wait_for_log "packet exceeds size limit" ||
    fail "broker did not classify the oversize Remaining Length"
kill -0 "$BROKER_PID" 2>/dev/null ||
    fail "broker exited after rejecting malformed MQTT packets"

mosquitto_sub \
    -h 127.0.0.1 \
    -p "$PORT" \
    -V mqttv311 \
    -i "$DUPLICATE_ID" \
    -t "$TOPIC" \
    -C 1 \
    -W 10 \
    >"$WORK_DIR/sub.out" \
    2>"$WORK_DIR/sub.err" &
SUB_PID=$!
wait_for_log "SUBSCRIBE processed" ||
    fail "subscriber did not complete CONNECT/SUBSCRIBE within 10 seconds"

# A second real MQTT client claiming the live subscriber's ClientId must be
# rejected atomically. The first handler must remain authoritative for its
# subscription and receive the later publication from a distinct client.
mosquitto_pub \
    -h 127.0.0.1 \
    -p "$PORT" \
    -V mqttv311 \
    -i "$DUPLICATE_ID" \
    -t "$TOPIC" \
    -m "$REJECTED_PAYLOAD" \
    >"$WORK_DIR/dup.out" \
    2>"$WORK_DIR/dup.err" &
DUP_PID=$!
wait_for_log "duplicate client_id=$DUPLICATE_ID rejected" ||
    fail "broker did not reject the duplicate ClientId within 10 seconds"

duplicate_status=0
wait "$DUP_PID" || duplicate_status=$?
DUP_PID=""
if [[ "$duplicate_status" == "0" ]]; then
    fail "duplicate ClientId publisher reported success"
fi
grep -qi "identifier rejected" "$WORK_DIR/dup.err" ||
    fail "Mosquitto did not observe CONNACK identifier-rejected"
if grep -qx "$REJECTED_PAYLOAD" "$WORK_DIR/sub.out"; then
    fail "duplicate ClientId redirected a publication to the live subscriber"
fi
kill -0 "$SUB_PID" 2>/dev/null ||
    fail "original subscriber was disconnected by duplicate ClientId rejection"

mosquitto_pub \
    -h 127.0.0.1 \
    -p "$PORT" \
    -V mqttv311 \
    -i "$PUBLISHER_ID" \
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

if ! wait "$PUB_PID"; then
    fail "publisher exited unsuccessfully"
fi
PUB_PID=""
if ! wait "$SUB_PID"; then
    fail "subscriber exited unsuccessfully"
fi
SUB_PID=""

# The last router mutation is the authority, not a count of log messages.
# Every registered handler must remove exactly its own immutable session row,
# and the final row state after both real clients exit must be empty.
router_state=""
for ((i = 0; i < 50; i++)); do
    router_state="$(
        grep "\\[router\\] state event=" "$WORK_DIR/broker.log" |
            tail -n 1 || true
    )"
    if [[ "$router_state" == *"clients=0 subscriptions=0" ]]; then
        break
    fi
    sleep 0.1
done
[[ "$router_state" == *"clients=0 subscriptions=0" ]] ||
    fail "router retained client/subscription rows after all clients exited: $router_state"
if grep -q "\\[router\\] state event=disconnect .*removed_clients=0" "$WORK_DIR/broker.log"; then
    fail "router observed a disconnect without removing its session row"
fi

if grep -Eq "PANIC:|panicked" "$WORK_DIR/broker.log"; then
    fail "broker logged an actor panic while handling the probe sequence"
fi

kill -0 "$BROKER_PID" 2>/dev/null ||
    fail "broker exited during the round trip"

echo "mqtt-broker-e2e: PASS port=$PORT payload=$PAYLOAD"
