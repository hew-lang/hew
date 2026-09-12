# std.net.http

HTTP client and server helpers. The package contains the inbound server at
`std.net.http` and outbound requests at `std.net.http.http_client`.

## HTTP/1.1 codecs

The pure Hew codecs operate on `std.net` connections:

- `std.net.http.http_async_client`: build a request with `build_get` or
  `build_request`, read with `conn.read()`, and parse accumulated bytes with
  `parse_response`. `AsyncResponse` exposes `status()`, `body()`, `header()`
  and `content_type()`.
- `std.net.http.http_async_server`: call `listener.accept()`, read until
  `request_complete()`, parse with `parse_request`, and write a response built
  by `response_text`, `response_json` or `build_response`.

Calls wait directly. Accept and read can suspend the caller while I/O is
pending. Use `fork` when the client and server must make progress concurrently;
`await` joins the resulting task. Keep connection handles local to the handler
and release them through consuming `close()` or scope cleanup.

These codecs buffer HTTP/1.1 bodies and use a connection per request. They do
not provide HTTPS or connection pooling. The separate `http_client` uses a
blocking native engine. Codec declarations and examples are not evidence that
all native or sandbox networking paths have passed acceptance.

See the [standard library overview](../../README.md) for all modules.
