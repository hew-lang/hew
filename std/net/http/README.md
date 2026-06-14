# std::net::http

std::net::http — HTTP client and server

The package contains the inbound server surface at `std::net::http` and the
bounded outbound client surface at `std::net::http::http_client`.

## Async (`await`) HTTP/1.1 surface

Two `await`-suspended codecs let an actor serve and fetch HTTP/1.1 without
stranding a scheduler worker — the suspend points free the worker while I/O is
pending and the reactor resumes the handler when it is ready:

- `std::net::http::http_async_client` — build a request with `build_get` /
  `build_request`, split a URL with `split_address` / `host_of`, then drive an
  inline `await conn.read()` loop and parse the bytes with `parse_response`
  into an `AsyncResponse` (`status()` / `body()` / `header()` / `content_type()`).
- `std::net::http::http_async_server` — create a listener with `net.listen`,
  `await listener.accept()` a connection, drive an inline `await conn.read()`
  loop until `request_complete()`, parse with `parse_request`, and reply with
  the bytes from `response_text` / `response_json` / `build_response`.

The suspend points are the inline `await listener.accept()` (the
`SuspendingAccept` carrier) and `await conn.read()` (the `SuspendingRead`
carrier); the codecs themselves are pure Hew. `Listener`/`Connection` handles
are handler LOCALs, never actor state (the supervisor-restart clone gate rejects
opaque handles in state).

**Scope (v0.5.0):** HTTP/1.1 only, fully-buffered request/response bodies, one
connection per request/accept (`Connection: close`, read-to-close framing).
Streaming bodies are NEW-7; HTTPS is BUG-NET-3; connection pooling and a
suspending connect/write are v0.5.1. The blocking `http_client` (native engine)
and blocking `http` server stay for `main`/synchronous callers. See
`examples/net/await_http_roundtrip.hew` for a runnable round trip.

Part of the [Hew](https://hew.sh) standard library. See the [std overview](../../README.md) for all modules.
