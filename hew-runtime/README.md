# hew-runtime

The Hew actor runtime library (`libhew_runtime.a`).

Every compiled Hew program links against this runtime, which provides:

- **Actor scheduler** — work-stealing, M:N threading for lightweight actors
- **Arena allocator** — fast bump allocation for actor-local memory
- **Stream I/O** — file streams, byte streams, channels, and pipe operations
- **String operations** — UTF-8 string primitives exposed via C ABI
- **Networking** — TCP/UDP sockets, HTTP server primitives
- **Concurrency** — mailboxes, actor groups, supervisors
- **Distributed tracing** — W3C Trace Context-compatible span recording
- **OTel exporter** — OTLP/HTTP trace export to Jaeger, Grafana Tempo, and any OTel-compatible backend

## Build targets

```sh
# Native static library
make runtime

# WebAssembly (for browser/WASI targets)
make wasm-runtime-debug
```

## C ABI

All public functions use `#[no_mangle] pub extern "C"` calling convention so they can be called from LLVM-generated code. The runtime's C interface is the contract between the Rust code generator and the Rust runtime.

## Distributed tracing

The runtime contains trace-context storage, actor-dispatch hooks and a bounded
span buffer. These facilities do not establish complete tracing through the
final compiler path. Core trace/telemetry integration remains incomplete; no
new public tracing API or automatic application-wide propagation is promised.

### OpenTelemetry OTLP exporter

The optional OTel exporter sends recorded runtime spans to an OTLP/HTTP
collector. Export configuration does not fill missing compiler instrumentation.

#### Enabling

The runtime must be built with its optional `otel` feature. The default
`make runtime` target does not enable it; the environment variables below
configure an already-enabled exporter.

Set environment variables before running your Hew program:

| Variable                 | Required | Default               | Description                                                       |
| ------------------------ | -------- | --------------------- | ----------------------------------------------------------------- |
| `HEW_OTEL_ENDPOINT`      | **yes**  | _(exporter disabled)_ | Base URL of the OTLP/HTTP collector, e.g. `http://localhost:4318` |
| `HEW_SERVICE_NAME`       | no       | binary name           | `service.name` resource attribute                                 |
| `HEW_OTEL_INTERVAL_SECS` | no       | `5`                   | Flush interval in seconds                                         |
| `HEW_OTEL_BATCH_SIZE`    | no       | `512`                 | Maximum spans per flush                                           |

#### Connecting to Jaeger

```bash
# Start Jaeger all-in-one (OTLP/HTTP on port 4318, UI on 16686)
docker run --rm \
  -p 4317:4317 -p 4318:4318 -p 16686:16686 \
  jaegertracing/all-in-one

# Run your Hew program with tracing
HEW_OTEL_ENDPOINT=http://localhost:4318 \
HEW_SERVICE_NAME=my-service \
./my_hew_program

# Open http://localhost:16686/ to explore traces
```

#### Connecting to Grafana Tempo

```bash
# With Grafana Agent or the OpenTelemetry Collector forwarding to Tempo:
HEW_OTEL_ENDPOINT=http://localhost:4318 \
HEW_SERVICE_NAME=my-service \
./my_hew_program
```

#### Implementation notes

- Uses a dedicated OS thread (`hew-otel-exporter`) — same pattern as the built-in profiler.
- Sends OTLP/HTTP JSON (`Content-Type: application/json`, path `/v1/traces`).
- Uses [`ureq`](https://crates.io/crates/ureq) for synchronous HTTP — **no `tokio` or async runtime**.
- When `HEW_OTEL_ENDPOINT` is unset, the exporter thread is not spawned.
- Each span carries `hew.actor_id` and `hew.msg_type` attributes for per-actor analysis.

## Part of the Hew compiler

This crate is an internal component of the [Hew](https://github.com/hew-lang/hew) compiler toolchain.
