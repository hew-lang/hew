# std::net::quic

QUIC transport protocol for bidirectional communication.

Provides a QUIC endpoint and connection API for low-latency, multiplexed, reliable communication over UDP. Designed for internode messaging in distributed actor systems.

## Features

- Client and server QUIC endpoints
- Multiplexed bidirectional streams
- TLS 1.3 security by default
- Connection pooling and management
- Stream lifecycle control

## Usage

### Server Example

```hew
import std::net::quic;

fn main() {
    let endpoint = quic.listen(":4433", "cert.pem", "key.pem");
    let conn = endpoint.accept();
    let stream = conn.accept_stream();
    let data = stream.recv();
    stream.send(data);
    stream.close();
    conn.close();
    endpoint.close();
}
```

### Client Example

```hew
import std::net::quic;

fn main() {
    let endpoint = quic.endpoint();
    let conn = endpoint.connect("localhost:4433");
    let stream = conn.open_stream();
    stream.send("hello");
    let response = stream.recv();
    stream.close();
    conn.close();
    endpoint.close();
}
```

## Implementation

Uses the [Quinn](https://github.com/quinn-rs/quinn) QUIC implementation in Rust, providing a stable, high-performance QUIC protocol stack.
