# std.net.websocket

std.net.websocket — WebSocket client and server

`connect(url, options)` opens `ws://` or `wss://` connections; `wss://`
verifies the server with the same `tls.Trust` and handshake deadline as
`std.net.tls`, and `options.subprotocols` are offered in order. A `Conn`
sends with `send_text` and `send_binary` and receives `Message.Text` or
`Message.Binary` from `recv` and `recv_timeout`, which return `.None` once the
peer closes. Failures are `net.NetError` values with the detail in
`last_error()`.

Part of the [Hew](https://hew.sh) standard library. See the [std overview](../../README.md) for all modules.

Inbound WebSocket handshakes default to an 8 MiB message cap and a 1 MiB
frame cap. Override them process-wide with `HEW_WS_MAX_MESSAGE_SIZE` and
`HEW_WS_MAX_FRAME_SIZE` (byte counts) when a deployment needs different
limits.
