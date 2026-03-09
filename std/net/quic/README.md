# Hew std::net::quic

QUIC transport for Hew programs, built on [`quinn`](https://github.com/quinn-rs/quinn).

Features:
- Runtime QUIC listener (`listen`) that auto-generates a self-signed certificate.
- Client connect with native roots or supplied DER certificate, plus an explicit
  `connect_insecure` escape hatch for local testing.
- Bidirectional channels (QUIC streams) with blocking `send`/`recv` helpers.

## Quick start

```hew
import std::net::quic;

fn main() {
    // Start a listener; retrieve the generated certificate to trust on clients.
    let server = quic.listen(":0");
    let cert = server.certificate_der();
    let addr = server.address();

    // Dial using the server's certificate.
    let client = quic.connect_with_ca(addr, cert);
    let server_conn = server.accept();

    // Open a bidirectional channel from the client and exchange bytes.
    let chan = client.open_channel();
    chan.send("ping");
    let chunk = chan.recv(4096);
    if !chunk.is_end() {
        // do something with chunk.data()
    }
    chan.finish();
    chunk.free();

    server_conn.close();
    client.close();
    server.close();
}
```

See `quic.hew` for the full API surface.
