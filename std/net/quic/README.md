# hew-std-net-quic

Hew `std::net::quic` — QUIC transport for internode messaging.

Part of the [Hew](https://hew.sh) standard library. See the [std overview](../../README.md) for all modules.

## Overview

Provides multiplexed, bidirectional byte streams over QUIC/UDP connections.
QUIC connections carry multiple independent streams so that head-of-line
blocking never stalls unrelated traffic — ideal for actor supervisor chains
and distributed messaging between Hew nodes.

Backed by [Quinn](https://github.com/quinn-rs/quinn), a pure-Rust QUIC implementation.

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
    conn.disconnect();
    ep.close();
}
```

## API

### Types

| Type             | Description                                           |
|------------------|-------------------------------------------------------|
| `QUICEndpoint`   | A bound UDP endpoint (client or server)               |
| `QUICConnection` | An established connection to a remote peer            |
| `QUICStream`     | A bidirectional stream within a connection            |
| `QUICEvent`      | An observed connection or stream state-change event   |

### Module Functions

| Function                          | Description                                 |
|-----------------------------------|---------------------------------------------|
| `quic.new_client()`               | Create a client endpoint                    |
| `quic.new_server(addr)`           | Bind a server endpoint on `addr`            |

### QUICEndpoint Methods

| Method                                    | Description                              |
|-------------------------------------------|------------------------------------------|
| `ep.connect(addr, server_name)`           | Dial a remote server                     |
| `ep.accept()`                             | Accept next incoming connection          |
| `ep.close()`                              | Close the endpoint                       |
| `ep.on_event()`                           | Block until the next endpoint event      |

### QUICConnection Methods

| Method                        | Description                                      |
|-------------------------------|--------------------------------------------------|
| `conn.open_stream()`          | Open a new bidirectional stream                  |
| `conn.accept_stream()`        | Accept next incoming stream from remote peer     |
| `conn.disconnect()`           | Gracefully close connection                      |
| `conn.on_event()`             | Block until the next connection event            |

### QUICStream Methods

| Method                        | Description                                      |
|-------------------------------|--------------------------------------------------|
| `stream.send(data)`           | Send bytes on the stream                         |
| `stream.recv()`               | Receive bytes (blocks until data available)      |
| `stream.finish()`             | Signal end-of-send                               |
| `stream.stop(error_code)`     | Abort the stream with an error code              |

### QUICEvent Methods

| Method          | Description                                              |
|-----------------|----------------------------------------------------------|
| `event.kind()`  | Return the event kind integer (see table below)          |
| `event.free()`  | Release event resources                                  |

**Event kinds:** `0` connected · `1` disconnected · `2` stream opened ·
`3` stream closed · `4` data ready · `-1` error.
