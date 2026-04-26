# hew-std-net-websocket

Hew hew-std-net-websocket — WebSocket client

Part of the [Hew](https://hew.sh) standard library. See the [std overview](../../README.md) for all modules.

Inbound WebSocket handshakes default to an 8 MiB message cap and a 1 MiB
frame cap. Override them process-wide with `HEW_WS_MAX_MESSAGE_SIZE` and
`HEW_WS_MAX_FRAME_SIZE` (byte counts) when a deployment needs different
limits.
