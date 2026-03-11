# hew-std-net-quic

Hew `std::net::quic` — QUIC transport for internode messaging.

Part of the [Hew](https://hew.sh) standard library. See the [std overview](../../README.md) for all modules.

## Overview

Provides multiplexed, bidirectional byte streams over QUIC/UDP connections.
QUIC connections carry multiple independent streams so that head-of-line
blocking never stalls unrelated traffic — ideal for actor supervisor chains
and distributed messaging between Hew nodes.

Backed by [Quinn](https://github.com/quinn-rs/quinn), a pure-Rust QUIC implementation.

The module exposes both blocking event handles (`on_event()`) and
non-destructive observation snapshots (`observe()`). Use queued events to wait
for lifecycle changes, and snapshots to inspect addresses, counters, telemetry,
and the last observed error without consuming anything.

## Usage

### Server

```hew
import std::net::quic;

fn main() {
    let ep = quic.new_server(":4433");
    let conn = ep.accept();
    let stream = conn.accept_stream();
    let data = stream.recv();
    stream.send(data);
    stream.finish();
    stream.close();
    conn.disconnect();
    ep.close();
}
```

### Client

```hew
import std::net::quic;

fn main() {
    let ep = quic.new_client();
    let conn = ep.connect("127.0.0.1:4433", "localhost");
    let stream = conn.open_stream();
    stream.send(b"hello quic");
    let reply = stream.recv();
    stream.finish();
    stream.close();
    conn.disconnect();
    ep.close();
}
```

## API

### Types

| Type                        | Description                                         |
| --------------------------- | --------------------------------------------------- |
| `QUICEndpoint`              | A bound UDP endpoint (client or server)             |
| `QUICConnection`            | An established connection to a remote peer          |
| `QUICStream`                | A bidirectional stream within a connection          |
| `QUICEndpointObservation`   | Snapshot of endpoint state                          |
| `QUICConnectionObservation` | Snapshot of connection state and telemetry          |
| `QUICStreamObservation`     | Snapshot of stream state                            |
| `QUICEvent`                 | An observed connection or stream state-change event |

`new_client()` and `new_server()` are development helpers. For deployed systems,
prefer the explicit TLS constructors shown in the API table below.

### Module Functions

| Function                                            | Description                               |
| --------------------------------------------------- | ----------------------------------------- |
| `quic.new_client()`                                 | Create a development client endpoint      |
| `quic.new_client_with_ca(ca_pem)`                   | Create a client endpoint with trusted CAs |
| `quic.new_server(addr)`                             | Bind a development server endpoint        |
| `quic.new_server_with_tls(addr, cert_pem, key_pem)` | Bind a server endpoint with explicit TLS  |

### QUICEndpoint Methods

| Method                          | Description                                  |
| ------------------------------- | -------------------------------------------- |
| `ep.connect(addr, server_name)` | Dial a remote server                         |
| `ep.accept()`                   | Accept next incoming connection              |
| `ep.close()`                    | Close the endpoint                           |
| `ep.on_event()`                 | Block until the next queued endpoint event   |
| `ep.observe()`                  | Read endpoint state without consuming events |

### QUICConnection Methods

| Method                 | Description                                    |
| ---------------------- | ---------------------------------------------- |
| `conn.open_stream()`   | Open a new bidirectional stream                |
| `conn.accept_stream()` | Accept next incoming stream from remote peer   |
| `conn.disconnect()`    | Gracefully close connection                    |
| `conn.on_event()`      | Block until the next queued connection event   |
| `conn.observe()`       | Read connection state, counters, and telemetry |

### QUICStream Methods

| Method                    | Description                                   |
| ------------------------- | --------------------------------------------- |
| `stream.send(data)`       | Send bytes on the stream                      |
| `stream.recv()`           | Receive bytes (blocks until data available)   |
| `stream.finish()`         | Signal end-of-send without freeing the handle |
| `stream.stop(error_code)` | Abort the stream with an error code           |
| `stream.close()`          | Release the local stream handle               |
| `stream.observe()`        | Read stream counters and closure state        |

### QUICEvent Methods

| Method         | Description                                     |
| -------------- | ----------------------------------------------- |
| `event.kind()` | Return the event kind integer (see table below) |
| `event.free()` | Release event resources                         |

**Event kinds:** `0` connected · `1` disconnected · `2` stream opened ·
`3` stream closed · `-1` error.

### Observation fields

- `QUICEndpointObservation`: `local_addr`, `accepted_connections`, `last_error`
- `QUICConnectionObservation`: `local_addr`, `peer_addr`, `opened_streams`,
  `accepted_streams`, `rtt_ms`, `bytes_sent`, `bytes_received`, `closed`,
  `last_error`
- `QUICStreamObservation`: `bytes_sent`, `bytes_received`, `send_closed`,
  `recv_closed`, `last_error`
