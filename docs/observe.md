# Hew observability (`std::observe` and `hew-observe`)

Hew's observability surface is runtime-owned and read-only from Hew code. Use it
when you want to inspect memory, scheduler, actor, coroutine, reactor, and
runtime-hook counters that the native runtime already records.

There are three primary metric APIs in `std::observe`:

```hew
import std::observe;

observe.read("heap.live_bytes"); // i64
observe.series();                 // string
observe.scrape();                 // string
```

For actor attribution and allocation/turn counters, start the program with the
hot tier enabled:

```sh
HEW_OBSERVE=hot hew eval --timeout 30 -f observe_demo.hew
```

## Hew API

### `observe.read(name: string) -> i64`

Reads one scalar metric by its canonical dotted name and returns its current
value as an `i64`.

```hew
import std::observe;

println(observe.read("heap.live_bytes"));
println(observe.read("actors.turns_total"));
println(observe.read("does.not.exist")); // -1
```

Unknown names return `-1`. Runtime `u64` values are converted to `i64`; values
larger than `i64::MAX` saturate at `i64::MAX`.

`read` only addresses scalar metrics. Labelled actor-attribution series are
visible in `scrape()` output, not by a labelled `read` call.

### `observe.series() -> string`

Returns the canonical metric names, one per line. It lists the 28 scalar names
accepted by `observe.read` plus the two labelled actor-attribution series names.

```hew
import std::observe;

println(observe.series());
```

Use this as the discovery surface when you are not sure which metric names are
available.

### `observe.scrape() -> string`

Returns Prometheus-style text for the current registry:

```hew
import std::observe;

println(observe.scrape());
```

Scrape output uses underscore metric names (`heap_live_bytes`) while `read` and
`series` use dotted names (`heap.live_bytes`). Each scalar metric is emitted with
a `# TYPE` line and an integer sample. When actor attribution has samples, the
scrape also includes labelled per-handler series.

Example excerpt:

```text
# TYPE heap_live_bytes gauge
heap_live_bytes 32
# TYPE actors_attributed_turns_by_handler_total counter
actors_attributed_turns_by_handler_total{dispatch="0x...",msg_type="...",handler="Counter::increment"} 2
```

## Getting started: read and scrape a small workload

Save this as `observe_demo.hew`:

```hew
import std::observe;

actor Counter {
    var count: i64;

    receive fn increment(n: i64) {
        count = count + n;
    }

    receive fn total() -> i64 {
        count
    }
}

let counter = spawn Counter(count: 0);
counter.increment(1);
counter.increment(2);
let total = await counter.total();
let _barrier = observe.barrier();

println(total);
println(observe.read("actors.turns_total"));
println(observe.series());
observe.scrape()
```

Run it with hot counters enabled:

```sh
HEW_OBSERVE=hot hew eval --timeout 30 -f observe_demo.hew
```

Expected useful output includes:

- `3` from the actor's `total` handler.
- A non-zero `actors.turns_total` scalar.
- `actors.attributed_turns_by_handler_total` in the series list.
- Scrape samples labelled with handlers such as `Counter::increment` and
  `Counter::total`.

`observe.barrier()` is a current synchronization helper, not a metric API. It
waits until actor dispatches that started before the call have reached their
observe-attribution point. It returns `0` on success, `-1` when called from
inside an actor dispatch, and `-2` if the native runtime's bounded 30-second wait
expires. In WASM it is currently a no-op success because the WASM scheduler does
not have the native attribution probe path.

## Metrics surface

The scalar registry has 28 metrics. `Kind` is the scrape type; `Hot?` means the
value only increments when `HEW_OBSERVE` enables the hot tier.

| Metric | Kind | Hot? | What it measures |
| --- | --- | --- | --- |
| `heap.live_bytes` | gauge | no | Current allocator live bytes. On WASM this currently reads `0`. |
| `heap.allocated_total` | counter | yes | Total bytes recorded by hot-tier heap allocation probes. |
| `heap.freed_total` | counter | yes | Total bytes recorded by hot-tier heap free probes. |
| `heap.allocations_total` | counter | yes | Number of heap allocations recorded by hot-tier probes. |
| `heap.frees_total` | counter | yes | Number of heap frees recorded by hot-tier probes. |
| `scheduler.workers` | gauge | no | Runtime worker count; WASM reports `1`. |
| `scheduler.runnable_actors` | gauge | no | Actors currently runnable according to scheduler/actor-registry metrics; WASM reports `0`. |
| `scheduler.runnable_coroutines` | gauge | no | Runnable coroutine count; WASM reports `0`. |
| `scheduler.queue_depth` | gauge | no | Scheduler queue depth; WASM reports `0`. |
| `scheduler.steals_total` | counter | no | Work-steal count from scheduler metrics. |
| `scheduler.parks_total` | counter | no | Scheduler park events recorded by runtime hooks. |
| `scheduler.unparks_total` | counter | no | Scheduler unpark events recorded by runtime hooks. |
| `actors.live` | gauge | no | Live actors currently registered with the profiler actor registry; WASM reports `0`. |
| `actors.turns_total` | counter | yes | Actor turns completed by the scheduler. |
| `actors.turn_duration_ns_total` | counter | yes | Sum of actor-turn duration in nanoseconds. |
| `actors.attributed_turns_total` | counter | yes | Sum of all per-handler attributed actor turns. |
| `actors.attributed_turn_duration_ns_total` | counter | yes | Sum of all per-handler attributed turn duration in nanoseconds. |
| `actors.crashes_total` | counter | no | Actor crash count recorded by runtime hooks. |
| `actors.restarts_total` | counter | no | Actor restart count recorded by runtime hooks. |
| `coroutines.live` | gauge | no | Live coroutine frame count. |
| `coroutines.suspended` | gauge | no | Suspended coroutine count. |
| `coroutines.resumes_total` | counter | no | Coroutine resume count. |
| `coroutines.suspends_total` | counter | no | Coroutine suspend count. |
| `coroutines.frame_bytes_live` | gauge | no | Bytes held by live coroutine frames. |
| `threads.blocking_count` | gauge | no | Current blocking-thread count. |
| `reactor.registrations_live` | gauge | no | Live reactor registration count. |
| `reactor.ready_events_total` | counter | no | Reactor ready-event count. |
| `arena.resets_total` | counter | no | Arena reset count. |

The two labelled series are emitted by `observe.scrape()` when attribution data
exists:

| Scrape series | Kind | Labels | What it measures |
| --- | --- | --- | --- |
| `actors_attributed_turns_by_handler_total` | counter | `dispatch`, `msg_type`, `handler` | Actor turn count grouped by dispatch pointer, message type id, and resolved handler name. |
| `actors_attributed_turn_duration_ns_by_handler_total` | counter | `dispatch`, `msg_type`, `handler` | Total actor turn duration in nanoseconds for the same label set. |

How to read the labelled series:

- `handler` is the most useful label. With profiler support enabled, it resolves
  to names such as `Counter::increment`.
- `msg_type` is an integer message type id. It is useful for joining samples but
  is not meant to be hand-decoded.
- `dispatch` is the dispatch function pointer rendered as hex. Treat it as a
  process-local grouping key, not a stable cross-run identifier.
- The labelled series are scrape-only. Use the aggregate scalar metrics when you
  need `observe.read`.

## Actor attribution

Actor attribution is enabled by the hot tier. During actor dispatch, the runtime
records a turn count and duration against the actor handler. This lets you answer
questions such as:

- Which actor handler is receiving the most turns?
- Which handler is accumulating the most processing time?
- Did a specific workload reach the handler you expected?

Use this pattern in examples and tests that need deterministic scrape output:

```hew
let actor = spawn Counter(count: 0);
actor.increment(1);
let value = await actor.total();
let _barrier = observe.barrier();
println(observe.scrape());
```

Then look for handler-labelled lines in the scrape text. Without
`HEW_OBSERVE=hot`, attribution counters remain zero and labelled series may be
absent.

## `HEW_OBSERVE=hot`

`HEW_OBSERVE` controls the hot observe tier. The runtime enables it when the
environment variable is one of:

```text
1, true, yes, on, hot
```

Use `HEW_OBSERVE=hot` when you need:

- Heap allocation totals and counts.
- Actor turn totals and duration totals.
- Per-handler actor attribution in scrape output.

Cheap gauges and many runtime-hook counters are readable without the hot tier,
but hot-tier counters stay at zero until enabled. Hew code cannot enable the hot
tier at runtime today; set the environment before launching the program.

## Profiler HTTP endpoints and `hew-observe`

Start a Hew program with the runtime profiler enabled:

```sh
hew run --profile myapp.hew
```

On Unix, `--profile` sets `HEW_PPROF=auto` if it is unset, which uses a per-user
Unix socket that `hew-observe` can discover. To use a TCP endpoint directly:

```sh
HEW_PPROF=:6067 HEW_OBSERVE=hot hew run --profile myapp.hew
```

Then query the HTTP endpoints:

```sh
curl -fsS http://127.0.0.1:6067/api/observe/scrape
curl -fsS http://127.0.0.1:6067/api/actors
curl -fsS http://127.0.0.1:6067/api/metrics
```

The key endpoints for observe users are:

| Endpoint | Format | Use for |
| --- | --- | --- |
| `GET /api/observe/scrape` | plain text | Full observe registry in scrape format, including hot counters and labelled actor attribution. |
| `GET /api/actors` | JSON envelope | Per-actor rows: `id`, `pid`, `actor_type`, `state`, `msgs`, `time_ns`, `mbox_depth`, `mbox_hwm`. |
| `GET /api/metrics` | JSON envelope | Current profiler metrics: timestamp, task/message counters, active workers, allocator stats, and TCP counters. |
| `GET /api/metrics/history` | JSON envelope | Five-minute, one-sample-per-second profiler ring buffer with abbreviated keys. |
| `GET /api/memory` | JSON envelope | Current allocator stats. |
| `GET /debug/pprof/heap` | gzip protobuf | pprof-compatible heap profile. |
| `GET /debug/pprof/profile` | text | Flat profile text. |

JSON endpoints are wrapped as:

```json
{"schema_version":"v0.5","data":{}}
```

They also include the `X-Hew-Schema-Version: v0.5` header. The scrape endpoint
is plain text and is not JSON-enveloped.

For the terminal UI, build or install the sibling binary and attach it to the
running profiler:

```sh
hew-observe --list
hew-observe --pid 12345
hew-observe --addr 127.0.0.1:6067
```

`hew observe ...` delegates to a `hew-observe` binary in the same directory as
`hew` or on `PATH`. If that sibling binary is not built or installed, the
delegation fails; run `hew-observe` directly or build/install it alongside
`hew`.

## Limitations / not yet

- **No custom application metrics.** Hew programs can read runtime-owned metrics,
  but cannot define counters, gauges, histograms, labels, spans, or traces
  through `std::observe`.
- **Fragmented surfaces.** `observe.read`, `observe.scrape`, `/api/metrics`,
  `/api/actors`, and `/api/metrics/history` do not expose one identical schema.
  In particular, `/api/metrics` currently omits several fields that the runtime
  captures internally and that are visible through scrape or actor-specific
  endpoints.
- **Metric metadata is minimal.** `series()` lists names and `scrape()` includes
  `# TYPE`, but there is no API for descriptions, units, stability, or hot-tier
  requirements.
- **No programmatic hot-tier check in Hew.** The runtime has an internal hot-tier
  flag, but `std::observe` does not expose `hot_enabled()`.
- **`observe.barrier` is narrow.** It is present as a synchronization helper for
  attribution visibility, but it is not a custom metric or flush API.
- **`hew observe` depends on a sibling binary.** The main `hew` CLI delegates to
  `hew-observe`; it does not embed the TUI.
- **The TUI is terminal-oriented.** Use HTTP endpoints directly for scripts,
  CI, or non-TTY contexts.
