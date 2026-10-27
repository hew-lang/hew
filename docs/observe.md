# Hew observability (`std.observe` and `hew-observe`)

Hew's observability surface is runtime-owned and read-only from Hew code. Use it
when you want to inspect memory, scheduler, actor, coroutine, reactor, and
runtime-hook counters that the native runtime already records.

There are three primary metric APIs in `std.observe`:

```hew
import std.observe;

observe.read("heap.live_bytes"); // Option<i64>
observe.series();                 // string
observe.scrape();                 // string
```

For actor attribution and allocation/turn counters, start the program with the
hot tier enabled:

```sh
HEW_OBSERVE=hot hew eval --timeout 30 -f observe_demo.hew
```

## Hew API

### `observe.read(name: string) -> Option<i64>`

Reads one scalar metric by its canonical dotted name and returns its current
value as `Option<i64>`.

```hew
import std.observe;

println(observe.read("heap.live_bytes") ?? 0);
println(observe.read("actors.turns_total") ?? 0);
match observe.read("does.not.exist") {
    .Some(value) => println(value),
    .None => println("None"),
}
```

Unknown names return `None`. Runtime `u64` values are converted to `i64`; values
larger than `i64::MAX` saturate at `i64::MAX`.

`read` only addresses scalar metrics. Labelled actor-attribution series are
visible in `scrape()` output, not by a labelled `read` call.

### `observe.series() -> string`

Returns the canonical metric names, one per line. It lists scalar names
accepted by `observe.read` and labelled series for actor attribution,
supervisor restarts and mailbox loss.

```hew
import std.observe;

println(observe.series());
```

Use this as the discovery surface when you are not sure which metric names are
available.

### `observe.scrape() -> string`

Returns Prometheus-style text for the current registry:

```hew
import std.observe;

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
actors_attributed_turns_by_handler_total{dispatch="0x...",msg_type="...",handler="Counter.increment"} 2
# TYPE supervisor_restarts_by_child_total counter
supervisor_restarts_by_child_total{supervisor="supervisor:1",child="store"} 1
```

## Getting started: read and scrape a small workload

Save this as `observe_demo.hew`:

```hew
import std.observe;

actor Counter {
    var count: i64;

    receive fn increment(n: i64) {
        count = count + n;
    }

    receive fn total() -> i64 {
        count
    }
}

let counter = spawn Counter { count: 0 };
counter.increment(1).expect("increment completes");
counter.increment(2).expect("increment completes");
let total = counter.total().expect("total query completes");
let _barrier = observe.barrier().expect("barrier succeeds");

println(total);
println(observe.read("actors.turns_total") ?? 0);
println(observe.series());
println(observe.scrape());
```

Run it with hot counters enabled:

```sh
HEW_OBSERVE=hot hew eval --timeout 30 -f observe_demo.hew
```

Expected useful output includes:

- `3` from the actor's `total` handler.
- A non-zero `actors.turns_total` scalar.
- `actors.attributed_turns_by_handler_total` in the series list.
- Scrape samples labelled with handlers such as `Counter.increment` and
  `Counter.total`.

`observe.barrier()` is a current synchronization helper, not a metric API. It
waits until actor dispatches that started before the call have reached their
observe-attribution point and returns `Result<(), ObserveError>`. It returns
`ObserveError.ActorTurn` inside an actor dispatch and `ObserveError.TimedOut`
when the native runtime's bounded 30-second wait expires. In WASM it is
currently a no-op success because the WASM scheduler does not have the native
attribution probe path.

## Metrics surface

`Kind` is the scrape type; `Hot?` means the
value only increments when `HEW_OBSERVE` enables the hot tier.

| Metric | Kind | Hot? | What it measures |
| --- | --- | --- | --- |
| `observe.hot_enabled` | gauge | no | `1` when hot counters are enabled, otherwise `0`; zero hot counters alone do not establish that no events occurred. |
| `heap.live_bytes` | gauge | no | Live bytes held by every allocation Hew owns. On WASM this currently reads `0`. |
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
| `threads.blocking_count` | gauge | no | Blocking-pool jobs running now. |
| `reactor.registrations_live` | gauge | no | Operations currently waiting on socket readiness. |
| `reactor.ready_events_total` | counter | no | Readiness reports handed to waiting operations. |
| `io.handles_live` | gauge | no | Open socket and listener handles. |
| `pool.threads` | gauge | no | Threads in the blocking-work pool. |
| `pool.saturations_total` | counter | no | Jobs that queued because every pool thread was busy at the cap. |
| `arena.resets_total` | counter | no | Arena reset count. |
| `mailbox.depth` | gauge | no | Sum of live actors' queued user messages and bounded fast-path producer reservations. |
| `mailbox.depth_max` | gauge | no | Largest current actor mailbox depth. |
| `mailbox.depth_p99` | gauge | no | Nearest-rank 99th percentile of current actor mailbox depths. |
| `node.peers` | gauge | no | Published authenticated peer claims, excluding reserved, retired and unverified connections. |
| `node.routes` | gauge | no | Current routes in the node's routing table. |
| `node.gossip_in_total` | counter | yes | Authenticated, validated SWIM and registry control frames received during this node lifetime. |
| `node.gossip_out_total` | counter | yes | Successfully sent SWIM and registry control frames during this node lifetime; failed attempts do not increment it. |
| `node.pending_asks` | gauge | no | Outbound asks awaiting a reply; service offers, imports and link setup are excluded. |
| `swim.alive`, `swim.suspect`, `swim.dead` | gauge | no | Membership entries in each failure-detector state. |
| `swim.quarantined` | gauge | no | Peers currently quarantined by this runtime. |
| `registry.names` | gauge | no | Names in the running node's distributed registry, or local registered names when no node runs. |
| `registry.conflicts_total` | counter | yes | Duplicate-name refusals by the local registry during this runtime lifetime. |

Native node gauges report zero when no node runs. Node counters reset when a
new node is constructed; distributed registry winner policy is unchanged.

### What the heap counters cover

The `heap.*` counters wrap the Rust global allocator, and every allocation Hew
owns goes through it: collection storage, `bytes` and `string` buffers, the
per-object headers, mailbox payloads, reply values, actor state wrappers and
serialization buffers. The coverage is structural rather than instrumented -
there is no separate accounting call a new allocation could forget to make.

Two things are outside the counters:

- Memory a native package allocates with its own allocator. The package owns
  both ends of that allocation, so the runtime never sees it. The one
  documented list of such boundaries is
  [allocation boundaries](internal/allocation-boundaries.md).
- Memory the process holds that is not a Hew allocation: the executable, thread
  stacks, and whatever the C library keeps.

A drop in `heap.live_bytes` says the allocator was told the memory is free. It
does not say resident memory fell: the allocator usually keeps freed pages for
the next request, so RSS follows later, or not at all.

The labelled series are emitted by `observe.scrape()` when attribution data
exists:

| Scrape series | Kind | Labels | What it measures |
| --- | --- | --- | --- |
| `actors_attributed_turns_by_handler_total` | counter | `dispatch`, `msg_type`, `handler` | Actor turn count grouped by dispatch pointer, message type id, and resolved handler name. |
| `actors_attributed_turn_duration_ns_by_handler_total` | counter | `dispatch`, `msg_type`, `handler` | Total actor turn duration in nanoseconds for the same label set. |
| `supervisor_restarts_by_child_total` | counter | `supervisor`, `child` | Each supervised child's lifetime restart count, one row per child across every registered supervisor tree. |
| `mailbox_dropped_new_total` | counter | `actor`, `spawn_serial` | Incoming user messages discarded by the declared policy. |
| `mailbox_dropped_old_total` | counter | `actor`, `spawn_serial` | Queued one-way messages evicted to admit a new message. |
| `mailbox_coalesced_total` | counter | `actor`, `spawn_serial` | Queued user messages superseded by a matching key. |
| `mailbox_dropped_on_stop_total` | counter | `actor`, `spawn_serial` | Queued user messages retired at terminal stop, termination or fault. |

Mailbox loss counters require the hot tier. They belong to an actor incarnation
and disappear when its retained actor allocation is freed. A refused send is
not a discarded message. System-lane signals and unpublished blocked senders
are excluded. The actor API also reports capacity, policy and these exact
counters beside depth and the high-water mark.

The Supervisors tab supports read-only drill-down. Select a declared role to
inspect its current incarnation, strategy, lifetime restart count and mailbox;
press Enter in the TUI to focus that actor's events. The desktop app also
offers **View supervisor decisions** for the role's owning supervisor.
Selection uses producer and incarnation identities, not display labels.
Older v1 producers remain readable and explicitly show unavailable details.
The additive HTTP `runtime_series` list uses the same `std.observe` catalogue
and exact decimal counter values; it does not introduce a second registry.

How to read the labelled series:

- `handler` is the most useful label. With profiler support enabled, it resolves
  to names such as `Counter.increment`.
- `msg_type` is an integer message type id. It is useful for joining samples but
  is not meant to be hand-decoded.
- `dispatch` is the dispatch function pointer rendered as hex. Treat it as a
  process-local grouping key, not a stable cross-run identifier.
- `child` is the declared child slot name (e.g. `store`), not the actor type.
  `supervisor` identifies the owning supervisor by its actor id and is a
  process-local grouping key like `dispatch`, not a stable cross-run identifier.
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

This fragment uses the `Counter` actor and `std.observe` import from the example above.

```hew,ignore
let counter = spawn Counter { count: 0 };
counter.increment(1).expect("increment completes");
let value = counter.total().expect("total query completes");
let _barrier = observe.barrier().expect("barrier succeeds");
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
| `GET /api/cluster/members` | JSON envelope | Cluster membership and relative last-seen ages. |
| `GET /api/connections` | JSON envelope | Connection-manager state and monotonic last-activity readings. |
| `GET /api/routing/table` | JSON envelope | Full node identities, route slots, session incarnations, and connection IDs. |
| `GET /api/traces` | JSON envelope | Reads a non-destructive, cursor-addressed page from the bounded process trace stream. |
| `GET /api/supervisors` | JSON envelope | Flattened supervision tree rows. |
| `GET /api/crashes` | JSON envelope | Recent actor crash records and resolved trap kinds. |
| `GET /debug/pprof/heap` | gzip protobuf | pprof-compatible heap profile. |
| `GET /debug/pprof/profile` | text | Flat profile text. |

JSON endpoints are wrapped as:

```json
{"schema_version":"v1","data":{}}
```

They also include the `X-Hew-Schema-Version: v1` header. The scrape endpoint
is plain text and is not JSON-enveloped.

All signed and unsigned 64-bit integer fields are canonical base-10 JSON
strings. Bounded 32-bit and 16-bit fields remain JSON numbers. This preserves
full precision through ordinary JavaScript JSON parsing and native-app IPC.

The machine-readable authority is
[`protocol/observe/v1/openapi.json`](../protocol/observe/v1/openapi.json).
It defines required and nullable fields, integer widths, units, clock semantics,
open string taxonomies, error bodies, and both TCP and Unix-socket transports.
Rust, Swift, and TypeScript bindings are generated from that file; run
`make observe-protocol-check` to validate the schema, generated-code drift, and
lossless 64-bit codec tests.

The first trace request omits `stream_id` and `after` and starts at the oldest
event still retained. Continue with the returned stream epoch and exclusive
cursor:

```sh
curl -fsS 'http://127.0.0.1:6067/api/traces?stream_id=<stream_id>&after=<next_cursor>&limit=256'
```

Both cursor parameters must be supplied together. Each observer maintains its
own cursor, so polling does not consume events needed by another observer. A
page containing 256 events only reached the response limit: `has_more` reports
whether retained events remain, while a non-null `overrun` gives the exact
sequence range lost to bounded retention.

`producer_instance_id` changes across process restarts, and `stream_id` changes
when tracing is cleared, reset, or re-enabled. A `409 trace_stream_mismatch`
therefore starts a new observation segment; an old cursor must never be applied
to the new stream. Event `timestamp_ns` values use the producer's monotonic
clock. `clock_anchor` supplies an approximate mapping to Unix time with an
explicit uncertainty, not a cross-node synchronization guarantee.

Trace events carry an honest nullable `subject`, `source_actor`, and
`destination_actor`. Unknown attribution stays `null`; consumers must not infer
causality or destinations from timestamp proximity. Producer overwrite and
sampling counters describe source-side loss. Consumer storage or transport
loss must be tracked separately by the consumer.

For the terminal UI, attach to the running profiler:

```sh
hew observe --list
hew observe --pid 12345
hew observe --pid 12345 --pid 23456
hew observe --addr 127.0.0.1:6067
```

Repeat `--pid` (or use comma-separated PIDs) to watch several processes in one
observer. Use `[` and `]` to switch the active process in the per-process views;
the messages and timeline views include events from all selected processes.

`hew observe` is included in the `hew` CLI.

## Choosing an observation surface

- Use `std.metrics` to define application counters, gauges, and histograms;
  `std.observe` reads runtime-owned measurements.
- Choose the endpoint for the shape you need: `observe.read`,
  `observe.scrape`, `/api/metrics`, `/api/actors`, and
  `/api/metrics/history` expose purpose-specific schemas.
- `series()` lists metric names and `scrape()` includes names and `# TYPE`.
  The current API has no description or unit fields; document those alongside
  the application that defines each metric.
- `observe.barrier` waits for attribution visibility. It does not flush or
  publish custom metrics.
- `hew observe` provides the terminal observer in the `hew` CLI.
- The TUI targets interactive terminals. Use the HTTP endpoints for scripts,
  CI, and other non-TTY clients.
