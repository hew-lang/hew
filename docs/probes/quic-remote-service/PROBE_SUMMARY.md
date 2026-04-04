# Haiku Remote Service Probe - Summary

## Objective
Create a minimal bounded remote/distributed service using only Hew code and the public `std::net::quic` transport surface, demonstrating a small textual round-trip without relying on runtime internals or user-authored FFI helpers.

> **Archive refresh note:** This summary now matches the checked-in probe files in
> `docs/probes/quic-remote-service/`.

## Constraints Satisfied
✅ **Only public stdlib transport modules** - Used `std::net::quic` endpoint/connection/stream APIs
✅ **No runtime internals** - Did not use `Node`, `Actor` transport mechanisms, or undocumented internals
✅ **No user-authored FFI helpers** - String payloads use `stream.send_string()` / `stream.recv_string()`
✅ **Clean bounded runtime** - Both server and client initialize, run, and gracefully shut down
✅ **Two-endpoint interaction** - Real client-server QUIC communication with bidirectional streams

## Solution: QUIC-Based Remote Service

### Files
- **service_server.hew** - Minimal QUIC server endpoint
- **service_client.hew** - Minimal QUIC client endpoint

### Architecture
```
┌─────────────────────────────────────┐
│ QUIC Server @ :4433                │
│ ┌──────────────────────────────────┤
│ │ QUICEndpoint (server)             │
│ │ └─ QUICConnection (1 peer)        │
│ │    └─ QUICStream (1 stream)       │
│ │       recv: "Hello from client"   │
│ │       send: "Echo from server"    │
│ └──────────────────────────────────┘
└─────────────────────────────────────┘
          QUIC/UDP + TLS 1.3
         (self-signed dev certs)
          ↕
┌─────────────────────────────────────┐
│ QUIC Client                         │
│ ┌──────────────────────────────────┤
│ │ QUICEndpoint (client)             │
│ │ └─ QUICConnection (to server)     │
│ │    └─ QUICStream (opened)         │
│ │       send: "Hello from client"   │
│ │       recv: "Echo from server"    │
│ └──────────────────────────────────┘
└─────────────────────────────────────┘
```

### Execution Flow

**Server:**
```
1. Create QUIC server endpoint on :4433
2. Block on ep.accept() → waits for remote connection
3. Upon connection, block on conn.accept_stream() → waits for client stream
4. Receive message: "Hello from client"
5. Send response: "Echo from server"
6. Clean shutdown: finish stream → close stream → wait for close/disconnect events → disconnect conn → close endpoint
```

**Client:**
```
1. Create QUIC client endpoint (ephemeral local port)
2. Connect to 127.0.0.1:4433 with SNI "localhost"
3. Open a bidirectional stream on the connection
4. Send message: "Hello from client"
5. Finish send side (signal EOF for client→server direction)
6. Receive response: "Echo from server"
7. Drain EOF, then close stream → wait for stream-closed event → disconnect conn → close endpoint
```

### Test Results

Expected output with the checked-in probe:

**Server output:**
```
[server] starting minimal QUIC service...
[server] listening on :4433
[server] accepted connection
[server] accepted stream
[server] received: Hello from client
[server] sent response
[server] shutdown complete
```

**Client output:**
```
[client] starting minimal QUIC service client...
[client] created client endpoint
[client] connected to server
[client] opened stream
[client] sent message
[client] received: Echo from server
[client] shutdown complete
```

### Key Findings

#### ✅ Public QUIC API is Viable for This Probe
The `std::net::quic` module provides everything this archived round-trip probe needs:
- **QUICEndpoint** - Bind server or create client with permissive TLS verifier
- **QUICConnection** - Establish and manage connections to peers
- **QUICStream** - Multiplexed bidirectional byte streams with independent flow control
- **Trait methods** - All transport operations are accessible as methods on the handle types (e.g., `ep.accept()`, `conn.open_stream()`, `stream.send_string()`, `stream.recv_string()`)

#### ✅ TLS and Certificates Work Out-of-the-Box
- `quic.new_server(addr)` auto-generates self-signed certificates for development
- `quic.new_client()` accepts permissive TLS verification (suitable for dev/testing)
- Zero-configuration needed for local testing

#### ✅ Stream API Semantics are Correct
- Calling `stream.finish()` signals EOF for the send side but **does not** prevent receiving
- Both endpoints can independently manage their send/receive directions
- No head-of-line blocking: streams are independent on a single connection

#### ⚠️ Zero-Value Error Handling is Limited
- When connection/stream operations fail, they return zero-valued structs (default-initialized types)
- Cannot directly test `if conn as i64 == 0` because the type system won't allow casting struct types to i64
- **Workaround:** Remove explicit error checks; rely on subsequent operations to fail gracefully or use the `observe()` methods to check status
- This is acceptable for a minimal probe but would need refinement for production services

#### ✅ Public String Helpers Remove Manual Byte Plumbing
- The checked-in probe uses `stream.send_string()` and `stream.recv_string()` for UTF-8 payloads
- These are convenience wrappers over the underlying byte stream; they do **not** add message framing
- They are suitable for UTF-8 text payloads, not binary or NUL-bearing protocols

### Public Surface Assessment

**What the public QUIC API supports:**
- ✅ Server-side: bind endpoint, accept connections, accept streams
- ✅ Client-side: create endpoint, dial remote server, open streams
- ✅ Bidirectional communication: send and receive on streams
- ✅ UTF-8 payload helpers: `send_string()` and `recv_string()`
- ✅ Clean shutdown: graceful close of streams, connections, endpoints
- ✅ Observation/telemetry: `endpoint.observe()`, `conn.observe()`, `stream.observe()`
- ✅ Multiple streams per connection: fully multiplexed and independent
- ✅ Event-driven API: `on_event()` methods available for non-blocking patterns

**What is NOT directly exposed via public surface:**
- Node registry/lookup (requires Node builtins)
- Actor-based remote dispatch (requires Node/actor runtime)
- RPC frameworks (would need to be built on top)

### Blockers: None Found for This Archived Probe

The probe successfully demonstrates that:
1. **No blockers remain** for this minimal archived QUIC round-trip on the public transport API
2. The stdlib is sufficient for two-endpoint patterns with textual payloads
3. Bounded shutdown depends on preserving the finish/recv/close ordering in the sample
4. Error handling can be improved with better public APIs for zero-value detection, but not a blocker

### Alternative Approaches Not Needed

This probe intentionally avoided:
- ❌ Runtime Node/actor transport (not part of public stdlib)
- ❌ HTTP-based RPC (would add complexity, QUIC is more appropriate)
- ❌ Custom protocol layers (raw bytes work fine for this demo)
- ❌ Async/await patterns (blocking API is sufficient and simpler for the probe)

### Conclusion

The Hew public stdlib's QUIC API is sufficient for this archived textual probe. The checked-in sample now:
1. ✅ Compiles without errors
2. ✅ Runs end-to-end with real network communication
3. ✅ Demonstrates two-endpoint remote interaction
4. ✅ Shows clean, bounded resource management
5. ✅ Uses only public Hew code and stdlib

The only path to more advanced patterns (transparent remote actor calls, cross-node service discovery) would require building on top of this foundation or using the higher-level Node API.

---

**Archive location:** `docs/probes/quic-remote-service/`
**Original branch:** `probe/haiku-remote-service` (archived, closed Apr 2026)
**Date:** 2026-04-03
**Status:** ✅ Successful archived probe refresh
