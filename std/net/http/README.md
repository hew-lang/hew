# std.net.http

HTTP client and server helpers. The package contains the inbound server at
`std.net.http.server` and outbound requests at `std.net.http.client`.

[`std.net.http.router`](router/README.md) provides pure Hew method/path routing
with typed route values and fallible named-parameter extraction. Pass either
server API's method and raw target to the same table.

## HTTP/1.1 codecs

The pure Hew `std.net.http.codec` module operates on `std.net` connections:

- Build a request with `build_get` or
  `build_request`, read with `conn.recv()`, and parse accumulated bytes with
  `parse_response`. `Response` exposes `status()`, `body()`, `header()`
  and `content_type()`.
- Call `listener.accept()`, read until
  `request_complete()`, parse with `parse_request`, and write a response built
  by `response_text`, `response_json` or `build_response`.

Calls wait directly. Accept and read can suspend the caller while I/O is
pending. Use `fork` when the client and server must make progress concurrently;
`await` joins the resulting task. Keep connection handles local to the handler
and release them through consuming `close()` or scope cleanup.

These codecs buffer HTTP/1.1 bodies and use a connection per request. They do
not provide HTTPS or connection pooling. The `std.net.http.client` module uses a
blocking native engine. Codec declarations and examples are not evidence that
all native or sandbox networking paths have passed acceptance.

See the [standard library overview](../../README.md) for all modules.
