# hew-observe + dev-tools alignment plan

## Status: Superseded — cut from v0.5 scope

Cut by `.tmp/orchestration/v05-strategy-consult-2026-05-19.md:72–79`. Defer to
v0.6.

**Task**: #232  
**Date**: 2026-05-18  
**Analyst**: Sonnet 4.6  
**Output target**: analysis + aligned lane proposals, no code written.

---

## 1. Current hew-observe wire schema inventory

### Transport / connection model

`hew-observe` is a TUI (ratatui) binary that polls the runtime profiler over HTTP (TCP
or Unix domain socket), then renders seven tab views. The profiler server lives in
`hew-runtime/src/profiler/server.rs`. The observer client in
`hew-observe/src/client.rs` pulls data by polling REST endpoints.

### API endpoints and what they return

| Endpoint | Client struct | Payload | Where populated |
|---|---|---|---|
| `GET /api/metrics` | `Metrics` | Scheduler counters: tasks_spawned/completed, steals, messages_sent/received, active_workers, alloc/dealloc, bytes_live/peak, TCP I/O | `profiler/metrics.rs` ring buffer, sampled every 1 s |
| `GET /api/metrics/history` | `Vec<HistoryEntry>` | Time-series of above (abbreviated field names) | Same ring buffer |
| `GET /api/actors` | `Vec<ActorInfo>` | Per-actor: id, pid, actor_type (string), state, msgs, time_ns, mbox_depth, mbox_hwm | `profiler/actor_registry.rs` REGISTRY global |
| `GET /api/traces` | `Vec<TraceEvent>` | Distributed trace events drained from ring buffer | `tracing.rs` TRACE_EVENTS ring |
| `GET /api/supervisors` | `Vec<SupervisorRow>` | Flattened supervision tree rows | `supervisor.rs::snapshot_tree_json` |
| `GET /api/crashes` | `Vec<CrashEntry>` | Recent crash reports (signal, msg_type, fault_addr) | `crash.rs` RECENT_CRASHES |
| `GET /api/cluster/members` | `Vec<ClusterMember>` | Cluster membership gossip | `cluster.rs` |
| `GET /api/connections` | `Vec<ConnectionInfo>` | Per-connection state | `connection.rs` |
| `GET /api/routing/table` | `RoutingSnapshot` | Routing table | `routing.rs` |

### TraceEvent wire schema (what `/api/traces` returns)

Fields (`hew-observe/src/client.rs:271–303`):
- `trace_id: String` (128-bit hex)
- `span_id: u64`, `parent_span_id: u64`
- `actor_id: u64`
- `actor_type_id: u64` — dispatch fn pointer cast; **0 when not registered** (`client.rs:285–291`)
- `actor_type: Option<String>` — None when `hew_actor_register_type` not called by codegen (`client.rs:289`, `tracing.rs:693`)
- `event_type: String` — the actionable type string
- `msg_type: i32`
- `timestamp_ns: u64`
- `handler_name: Option<String>` — None when `hew_register_handler_name` not emitted (`client.rs:300–303`)

### Actionable event types (`client.rs:310–325`)

```
"send" | "spawn" | "crash" | "stop"
"duplex_created" | "duplex_half_split" | "duplex_closed"
"sink_closed" | "stream_closed"
"lambda_spawned" | "lambda_released"
```

Non-actionable (filtered at ingestion, `app.rs:783–786`): `"begin"`, `"end"`

### SPAN_* constants defined in `tracing.rs:64–94`

```
SPAN_BEGIN=0, SPAN_END=1, SPAN_SPAWN=2, SPAN_CRASH=3, SPAN_STOP=4, SPAN_SEND=5,
SPAN_IO_ACCEPT=6, SPAN_IO_RECV=7,
SPAN_DUPLEX_CREATED=8, SPAN_DUPLEX_HALF_SPLIT=9, SPAN_DUPLEX_CLOSED=10,
SPAN_SINK_CLOSED=11, SPAN_STREAM_CLOSED=12,
SPAN_LAMBDA_SPAWNED=13, SPAN_LAMBDA_RELEASED=14
```

No SPAN constant exists for: lock_acquire, lock_release, lock_poison, cancel_fired,
scope_cancelled, cooperate_checked, closure_env_init, generator_yield, overflow_trap,
heap_exceeded, link_down, partition_detected.

### Emission sites (what actually calls into tracing.rs today)

| Substrate event | Call site | Status |
|---|---|---|
| Actor spawn | `actor.rs:1486` `hew_trace_lifecycle(actor_id, SPAN_SPAWN)` | Emitted |
| Actor stop | `actor.rs:2023, 2061` `SPAN_STOP` | Emitted |
| Actor crash | `actor.rs:3325–3329` `SPAN_CRASH` | Emitted |
| Message send | `actor.rs:2902` `record_send(trace_actor_id, msg_type)` | Emitted |
| Dispatch begin/end | codegen calls `hew_trace_begin` / `hew_trace_end` | Emitted (filtered in TUI) |
| Duplex created | `duplex.rs:754` `SPAN_DUPLEX_CREATED` | Emitted |
| Duplex closed | `duplex.rs:1111` `SPAN_DUPLEX_CLOSED` | Emitted |
| Duplex half-split | `duplex.rs:1153, 1188` `SPAN_DUPLEX_HALF_SPLIT` | Emitted |
| Sink closed | `stream.rs:1441` `SPAN_SINK_CLOSED` | Emitted |
| Stream closed | `stream.rs:1394` `SPAN_STREAM_CLOSED` | Emitted |
| Lambda spawned | `lambda_actor.rs:288` `SPAN_LAMBDA_SPAWNED` | Emitted |
| Lambda released | `lambda_actor.rs:445` `SPAN_LAMBDA_RELEASED` | Emitted |
| Lock acquire | — | **Not emitted** |
| Lock release | — | **Not emitted** |
| Lock poison | — | **Not emitted** |
| Scope cancel | — | **Not emitted** |
| Cooperate check | — | **Not emitted** |
| Closure env init | — | **Not emitted** |
| Overflow trap | — | **Not emitted** |
| Heap exceeded | — | **Not emitted** |
| Link/monitor down | — | **Not emitted** |
| Partition detected | — | **Not emitted** |

### Named-but-empty: actor_type / handler_name

**This is the most critical named-but-empty surface.** Both fields are declared in the wire
schema, documented, and tested — but they are zero/None on every event for all programs
compiled with codegen-rs today.

Evidence:
- `tracing.rs:693`: `actor_type` requires `hew_actor_register_type` from codegen; see `#1258`.
- `hew-codegen-rs/src/llvm.rs` has zero calls to `hew_actor_register_type` or `hew_register_handler_name`.
- `bridge.rs:797–831`: WASM builds have no-op stubs annotated `WASM-TODO(#1451)`.
- The hew-observe Actors tab and Timeline tab render `actor_type` as `"Actor"` (fallback at
  `client.rs:166–170`) for every actor in every real program.
- The Messages tab renders `handler_name` as `None` for every event.

A user looking at hew-observe sees an actor list full of rows labelled `"Actor"` with
zero handler names. This is worse than nothing: it presents a named column that is
silently empty, giving the impression the tooling is working when it is not.

### Consumers of trace events

Only hew-observe itself. There is no external exporter, no OpenTelemetry bridge, no
file-based export path. The ring buffer is drained by the profiler HTTP server on
`/api/traces` requests.

---

## 2. Substrate coverage gaps

### D24-1 — Auto-injected locks

**What exists:**
- Runtime ABI: `hew_actor_state_lock_acquire`, `hew_actor_state_lock_release`,
  `hew_actor_state_lock_release_after_panic`, `hew_actor_state_lock_poison_after_panic`
  fully implemented in `actor.rs:222–414`.
- HIR threading: checker produces lock-acquisition facts, HIR consumes them
  (commit `0345fa7d`, D24-1 slice 2).

**What is missing for observability:**
- Codegen does not emit `hew_actor_state_lock_acquire`/`_release` calls yet. The
  auto-locks worktree (branch `feat/auto-injected-locks`) has HIR+runtime but no
  codegen emission. `hew-codegen-rs/src/llvm.rs` has zero calls to these symbols.
- No SPAN constants for lock events in `tracing.rs`. `hew_actor_state_lock_acquire`
  and `hew_actor_state_lock_release` do not call any trace recording function.
- Observer has no tab, column, or filter for lock contention, poison events, or
  lock wait time.
- **Named-but-empty status**: The lock substrate exists but emits nothing to the
  trace ring. When codegen emission lands, the TUI will still show nothing because
  no SPAN variant is reserved and no display path exists.

**Gap severity**: Blocking for D24-1 debugging. When two actors share state and a
lock contention deadlock or poison occurs, the observer offers zero signal.

### D24-2 — Cancellation tokens

**What exists:**
- `hew_scope_cancel` (`scope.rs:308`) and `hew_scope_is_cancelled` (`scope.rs:324`) are
  implemented.
- MIR lowering and cancellation token substrate landed in `e5e4c3c6` (D24-2).
- Cooperate-site emission (`hew_actor_cooperate` call at every MIR cooperate-site)
  was landed independently in `f25b998f` as part of the reductions-budget work (RB-3,
  `feat(codegen): emit hew_actor_cooperate at MIR cooperate-sites`). The cancellation
  tokens (D24-2) and the reductions-budget cooperate-site emission (RB-3) both use
  the same ABI function; RB-3 added the emit sites, D24-2 added the cancel-check on
  the return value.

**What is missing for observability:**
- `hew_scope_cancel` does not call any trace recording function. There is no SPAN
  constant for cancellation events.
- `hew_scope_is_cancelled` does not emit. Cooperate-site checks do not emit.
- The observer has no way to show: which scope cancelled, when it fired, which actors
  were executing at cooperate points when they checked and found cancellation.
- **Named-but-empty status**: Cancellation is functional at the ABI level but
  completely invisible to observability. A program that hangs due to a missed
  cancel check cannot be diagnosed with hew-observe today.

**Gap severity**: Serious for debugging cancellation bugs in production actors.
Not blocking for correctness, but the v0.5 contract includes cancellation as a
first-class substrate — invisible cancellation is an observability regression.

### D24-3 — Closures + generators

**What exists:**
- Parser and HIR for closure captures landed (commit `04ea44bb` and earlier slices).
- `SPAN_LAMBDA_SPAWNED` and `SPAN_LAMBDA_RELEASED` exist in `tracing.rs:91–94` and
  are emitted by `lambda_actor.rs`.

**What is missing for observability:**
- Lambda actors are traced, but closure environment construction (env materialization,
  capture allocation) emits nothing.
- Generator `yield` suspensions and resumptions are not traced.
- `generator.rs` has no calls to `record_channel_event` or any trace function.
- The observer's Timeline tab cannot distinguish a lambda-actor suspend (yield) from
  an ordinary inter-message wait.
- **Named-but-empty**: `lambda_spawned`/`lambda_released` events exist and are emitted,
  but they carry no information about what the closure captured or how large the
  environment is. This is incomplete, not absent.

**Gap severity**: Moderate. Lambda lifecycle is visible; capture internals are not.
Acceptable for v0.5 since closures are a new substrate.

### Supervisor / failure substrate

**What exists:**
- `snapshot_tree_json()` (`supervisor.rs:148`) is implemented, walks registered
  supervisors, and returns real rows.
- `snapshot_crashes_json()` (`crash.rs:336`) is implemented, emits real crash reports.
- Crash reports include: time_s, actor_id, signal, msg_type, fault_addr (`crash.rs:336–358`).
- `push_crash_report` is called from `signal.rs:181` (native signal handler) on actual
  crashes.

**What is missing:**
- Supervisor restart counts and circuit-breaker state are not in the tree snapshot.
  `snapshot_tree_json` calls `append_supervisor_rows` which emits depth/label/state but
  no restart_count, no circuit_open status, no backoff_ms.
- `ChildSlotReason` variants (`supervisor.rs:168–184`) — `BackoffDelay`, `CircuitOpen`,
  `BudgetExhausted` — are never represented in the observer UI.
- `#[max_heap(N)]` exceeded crashes appear in the crash log (via `HeapExceeded` supervisor
  exit code, `supervisor.rs:490, 634`), but the crash entry `signal` field is synthetic
  (SIGSEGV) and there is no `trap_kind` field to distinguish overflow-trap from
  heap-exceeded from OOB in the observer UI.
- Overflow trap events: `Terminator::Trap` with `TrapKind` discriminants `201–205` are
  emitted by codegen-rs (`39172e85`) but the crash log does not carry the `TrapKind`.
- Link/monitor down: no trace event type. The `monitor` builtin (`5d99a76c`) has no
  observability hook.
- `PartitionDetected` (`duplex.rs:143`): triggers via `RecvError::PartitionDetected` but
  emits no trace event.

**Named-but-empty: crash signal field.** The crash log `signal` field always carries
the OS signal number (e.g., SIGSEGV=11) even for Hew-originated traps (overflow, heap,
divide-by-zero). This means the Crashes tab shows `SIGSEGV` for what is actually a
controlled trap. The `msg_type` field is the distinguisher but only for message-dispatch
crashes; for programmatic traps the `msg_type` is 0. The display pretends to show crash
reason when it cannot distinguish overflow from OOB from actual memory corruption.

### Channels: Stream/Sink/Duplex

**What exists:**
- `duplex_created`, `duplex_half_split`, `duplex_closed`, `sink_closed`, `stream_closed`
  are all emitted (confirmed above).

**What is missing:**
- Backpressure: no trace event for "producer blocked waiting for consumer" on a bounded
  channel.
- `PartitionDetected` on duplex recv has no trace event (distinct from `duplex_closed`).
- No metrics on channel throughput (items/sec, bytes/sec through a duplex/stream).
- The observer Messages tab displays these channel events in the trace scroll but
  cannot correlate duplex send-side with recv-side actors (both sides use the handle
  address as actor_id, not the actual actor pid).

### Traits / associated types / Iterator / Index

**What exists:** None. These are type-system concepts, not runtime events, so they
have no runtime observability hooks by design.

**What is reasonable to ask for:** The machine diagram tool (`hew-cli/src/machine.rs`)
understands `HirMachineDecl` but does not understand actors, supervisors, trait impls,
or the new v0.5 substrates. It is a pre-v0.5 tool.

---

## 3. Dev-tools / visualization inventory

### hew-observe TUI (the primary tool)

**Version alignment**: Reflects pre-D24 actor substrate (actors, supervisors, crashes,
cluster). The channel events (duplex/stream/lambda) were added recently and are
present. The v0.5 substrates from D24 (locks, cancellation, closures, generators) are
entirely absent from the TUI data model.

**Tabs and their status:**

| Tab | What it shows | Status |
|---|---|---|
| Overview | Metrics sparklines: msg rate, active workers, memory | Active. No v0.5 substrate data. |
| Actors | List: id, type, state, msgs, mbox_depth, processing_time | Active but `actor_type` is always `"Actor"` — named-but-empty. |
| Supervisors | Flattened supervision tree with state | Active but missing restart counts, circuit-breaker state. |
| Crashes | List: time, actor_id, signal, msg_type, fault_addr | Active but `signal` is ambiguous for Hew traps. |
| Cluster | Members, connections, routing table | Active. Cluster is v0.6 surface; this is premature. |
| Messages | Trace event scroll: send/spawn/crash/channel events | Active but handler_name is always None. |
| Timeline | Horizontal swimlane per actor | Active but actor names are `"Actor"` since actor_type_id is 0. |

**Stale elements:**
- Cluster tab reflects v0.6 distributed features (cluster, connections, routing) which
  are not v0.5 ship items (see mission v0.6 boundary). It shows demo data but no
  real cluster exists.
- Actor type names are `"Actor"` everywhere due to missing codegen emission (`#1258`).
- Handler names are `None` everywhere for the same reason (`#1259`).
- The demo mode (`load_demo_data`, `app.rs:877–1192`) hard-codes actor types like
  "Counter", "Logger", "Supervisor" that real programs do not expose today.

**Demo mode creates false expectations.** The screenshots in `hew-observe/docs/screenshots/`
(7 tab screenshots) show rich actor types and handler names — data that no real Hew
program produces today because codegen never calls `hew_actor_register_type`.

### hew machine diagram (`hew-cli/src/machine.rs`)

A static analysis tool that generates Mermaid/Graphviz/JSON diagrams from Hew source
files using `HirMachineDecl`. It operates on `machine` declarations (hierarchical state
machines, a v0.6 feature per the mission). It understands:
- `HirMachineDecl` and `HirMachineState`
- State transitions
- `--no-check` mode to bypass HIR checks

**What it does not understand (v0.5 substrate blindness):**
- `actor` declarations (it ignores them entirely)
- `supervisor` declarations
- Trait impls, closures, generators
- It is a tool for a v0.6 feature (`Hierarchical machine states`) while v0.5 is in progress.

**Alignment**: This tool is intentionally forward-looking for v0.6 hierarchical machines.
It is not stale; it is simply out of scope for v0.5 observability work.

### hew playground verify (`hew-cli/src/playground.rs`)

A CI-gate tool that compiles and runs curated playground examples against expected output.
It is a correctness regression tool, not an observability tool. No changes needed for
observability alignment.

### hew-wasm (`hew-wasm/src/lib.rs`)

Browser-analysis WASM for the documentation site. It is analysis-only (lexer + parser +
type-checker + HIR); it never runs programs. It is the "browser-side analysis WASM"
per the two-WASM-worlds memory (`project_wasm_two_worlds.md`). Not an observability tool.

### Profiler dashboard (`hew-runtime/src/profiler/dashboard/`)

An embedded single-page HTML/JS dashboard served at `http://localhost:6060/` when the
profiler is active. It uses the same HTTP API as hew-observe. Its status mirrors
hew-observe: actor types are anonymous, handler names absent. It predates the v0.5
substrates.

---

## 4. Concrete alignment lane proposals

### Lane OBS-1 — Codegen emits actor type and handler registration

**Goal**: Every actor type registration call (`hew_actor_register_type`) and every
handler name registration call (`hew_register_handler_name`) is emitted by codegen-rs
at LLVM module construction time so that the observe Actors and Messages tabs show real
type and handler names instead of `"Actor"` and `None`.

**Substrate touched**: `hew-codegen-rs/src/llvm.rs` (codegen emission), profiler
`actor_registry.rs` (already receives these calls; no runtime change needed).

**ABI/wire impact**: No new API endpoints or wire fields. These calls already exist at
runtime (`actor.rs:2464–2515`). Codegen simply needs to emit them at the right
program-init site (once per actor type, before any spawn).

**Fail-closed boundary**: If the codegen does not emit registration calls, the
fallback is `"Actor"` and `None` — the current (broken) state. No regression path.
Registration calls are idempotent (second call for same dispatch ptr is ignored per
`actor_registry.rs:86–`); safe to emit early.

**Tests**:
1. Compile a two-actor program through codegen and verify the emitted LLVM IR contains
   calls to `hew_actor_register_type` and `hew_register_handler_name` for each type.
2. Run the compiled binary with `HEW_PPROF=auto` and assert `/api/actors` returns non-`"Actor"`
   type names.
3. Assert `/api/traces` returns non-`None` `handler_name` on send events.

**Composition**: Required by OBS-2, OBS-4, OBS-5. Without this lane, those lanes
produce output but cannot link events to meaningful actor names.

**Sizing**: One Sonnet dispatch. Evidence gathered: the gap is a codegen emission
omission, not an architecture decision. The registration ABI is fully specified.

---

### Lane OBS-2 — Add TrapKind to crash wire and observer UI

**Goal**: Crash entries carry a `trap_kind` field that distinguishes: OS signal
(SIGSEGV, SIGABRT), overflow trap (TrapKind 201), OOB trap (202), divide-by-zero
(203), shift trap (204), heap-exceeded (205). The Crashes tab renders this
as a human-readable reason column.

**Substrate touched**: `crash.rs` `CrashReport` struct (add `trap_kind: u8` field),
`signal.rs` `build_crash_report` (populate from `TrapKind` discriminant),
profiler `server.rs` `snapshot_crashes_json` (emit the field),
`hew-observe/src/client.rs` `CrashEntry` (add `trap_kind: u8` field),
`hew-observe/src/ui.rs` Crashes tab render.

**ABI/wire impact**: Additive JSON field. Old observers silently default to 0
(unknown trap kind) via `#[serde(default)]`. No version break.

**Fail-closed boundary**: The Crashes tab today shows ambiguous `SIGSEGV` for all
Hew-originated traps. Adding `trap_kind` makes the display accurate without removing
the signal field.

**Tests**:
1. Compile a program that triggers an overflow trap (use existing `codegen` trap tests
   at `1d21b982`). Assert the crash report carries `trap_kind = 201`.
2. Compile a program that exceeds `#[max_heap(N)]`. Assert `trap_kind = 205`.
3. Assert `/api/crashes` JSON includes `"trap_kind"` field.
4. Test observer `CrashEntry` deserialization handles missing `trap_kind` (old-profiler compat).

**Composition**: Independent. Can run in parallel with OBS-1.

**Sizing**: One Sonnet dispatch. The `TrapKind` discriminants are already wired in
codegen (`39172e85`); this is plumbing across crash→profiler→observer.

---

### Lane OBS-3 — Add restart / circuit-breaker fields to supervisor snapshot

**Goal**: `snapshot_tree_json` emits per-child `restart_count`, `circuit_open: bool`,
and `budget_exhausted: bool` for each child slot. The Supervisors tab renders a
`Restarts` column and a `Circuit` state indicator.

**Substrate touched**: `supervisor.rs` (`InternalChildSpec` fields, `append_supervisor_rows`),
profiler server `snapshot_crashes_json`, `hew-observe/src/client.rs` `SupervisorRow`,
`hew-observe/src/ui.rs` Supervisors tab render.

**ABI/wire impact**: Additive JSON fields on SupervisorRow. Old observers ignore
unknown fields. No version break.

**Fail-closed boundary**: `InternalChildSpec` already tracks `restart_count` and
circuit-breaker state (`supervisor.rs:848–`, `ChildSlotReason` variants). This lane
just exposes what exists. No logic change.

**Tests**:
1. Start a program with a supervised actor that crashes repeatedly. Assert `/api/supervisors`
   returns rows with `restart_count > 0`.
2. Trigger `BudgetExhausted`. Assert the row reflects `circuit_open: true` or
   `budget_exhausted: true`.
3. Assert `SupervisorRow` deserialization handles missing fields (old-profiler compat).

**Composition**: Independent. Can run in parallel with OBS-1 and OBS-2.

**Sizing**: One Sonnet dispatch.

---

### Lane OBS-4 — Lock acquire/release trace events (SPAN_LOCK_*)

**Goal**: Once auto-locks codegen emission (D24-1 codegen slice) lands, each lock
acquire and release emits a trace event visible in the Messages and Timeline tabs.
This lane adds the SPAN constants, `record_lock_event` internal function, and wires
it to `hew_actor_state_lock_acquire` / `hew_actor_state_lock_release` / `hew_actor_state_lock_poison_after_panic`.

**Substrate touched**: `tracing.rs` (two new SPAN constants: SPAN_LOCK_ACQUIRE=15,
SPAN_LOCK_RELEASE=16, SPAN_LOCK_POISON=17; `record_lock_event` function),
`actor.rs` `hew_actor_state_lock_acquire` and `hew_actor_state_lock_release` (add
`record_lock_event` call),
`tracing.rs` `drain_events_json` string mapping (three new arm),
`hew-observe/src/client.rs` `TraceEvent::is_actionable` (add `"lock_acquire"` etc.),
no new API endpoint.

**ABI/wire impact**: Three new `event_type` string values in existing `/api/traces`
response. Old observers silently ignore unknown event types (they fall through
`is_actionable` returning false). No version break.

**Fail-closed boundary**: Lock events are `record_lock_event` calls at the beginning
of acquire and end of release. If tracing is disabled (`hew_trace_is_enabled() == 0`),
the `record_channel_event` / `record_lock_event` path is a no-op — the existing guard
in `tracing.rs:293–`.

**Prerequisite**: D24-1 codegen emission must be live for this lane to produce any
events in real programs. This lane can be merged before D24-1 codegen lands (the
hooks exist; they just won't fire until codegen emits the ABI calls). It is NOT
blocked on OBS-1 (type registration is separate).

**Tests**:
1. Unit test: call `hew_actor_state_lock_acquire` and `_release` directly; assert
   `hew_trace_event_count()` increments with SPAN_LOCK_ACQUIRE and SPAN_LOCK_RELEASE events.
2. Assert poison path emits SPAN_LOCK_POISON.
3. Assert `drain_events_json` serialises the new constants to `"lock_acquire"`,
   `"lock_release"`, `"lock_poison"` strings.
4. Assert `TraceEvent::is_actionable` returns true for all three.
5. Existing lock tests (`actor_state_lock_contract`) must continue to pass.

**Composition**: Composes with OBS-1 (type names make lock events attributable to
actor types). Composes with D24-1 codegen emission.

**Sizing**: One Sonnet dispatch. Narrow scope: two constants, one small function,
three new arms in a match, one new line in `is_actionable`.

---

### Lane OBS-5 — Cancellation trace events (SPAN_SCOPE_CANCEL, SPAN_COOPERATE_CHECKED)

**Goal**: `hew_scope_cancel` emits a trace event visible in Messages and Timeline.
`hew_actor_cooperate` (codegen-emitted at cooperate sites) emits a sampled trace event
so cancellation latency (time from `hew_scope_cancel` to first observed cooperate check)
is observable.

**Substrate touched**: `tracing.rs` (two new SPAN constants: SPAN_SCOPE_CANCEL=18,
SPAN_COOPERATE_CANCEL_HIT=19; `record_scope_event` internal function),
`scope.rs` `hew_scope_cancel` at line 308 (add trace call immediately before the
`cancelled.store(true, ...)` line),
`scheduler.rs` `hew_actor_cooperate` at line 1031 (native codegen ABI; add trace call
on the cancellation branch only — return value 2 means cancellation detected, per
`task_scope.rs:1423` test `cooperate_observes_current_task_scope_cancel_without_actor`;
NOT on every cooperate check, which would be too hot on the hot cooperative-scheduling path),
`scheduler_wasm.rs:1357` (WASM variant; same branch, same guard),
`drain_events_json` string mapping,
`client.rs` `is_actionable`.

**ABI/wire impact**: Two new `event_type` values in `/api/traces`. Additive; no break.

**Fail-closed boundary**: The cancellation trace is emitted only when cancellation
fires (i.e. the cooperate-site check returns true). Hot-path cooperate checks with no
cancellation active emit nothing. This is safe: the trace ring is bounded.

**Prerequisite**: Cancellation substrate is already live (commit `e5e4c3c6`). This lane
has no substrate prerequisites.

**Tests**:
1. Call `hew_scope_cancel` on a scope; assert `hew_trace_event_count()` increments with
   SPAN_SCOPE_CANCEL.
2. Call `hew_actor_cooperate` after cancel is set; assert SPAN_COOPERATE_CANCEL_HIT emits.
3. Call `hew_actor_cooperate` with no cancel set; assert no event emits (hot path clean).
4. Assert `drain_events_json` round-trips the new strings.
5. Assert `is_actionable` returns true for the new types.

**Composition**: Composes with OBS-1 (actor names make cancellation events attributable
to the cancelling scope's actor). Independent of OBS-4.

**Sizing**: One Sonnet dispatch. Narrow: two constants, two record calls, two match arms.

---

### Lane OBS-6 — Scrub demo mode false claims

**Goal**: Remove or clearly gate the fabricated `actor_type` and `handler_name` data
from `App::load_demo_data`. Demo data that shows richer capability than real programs
produce is actively misleading. Options:

(a) Remove `actor_type` from demo actors (revert to fallback `"Actor"` like real programs).
(b) Gate demo mode behind a compile-time feature flag and document it as illustrative only.
(c) Keep rich demo data but add a `[DEMO]` badge in the header so users understand it is synthetic.

Recommended: option (c). Demo mode exists to let people explore the TUI without a running
Hew program. That purpose is valid. But it must be unambiguous that demo data is synthetic.

**Substrate touched**: `hew-observe/src/app.rs` (`load_demo_data`, `app.rs:877–1192`),
`hew-observe/src/ui.rs` (header bar to add demo badge).

**ABI/wire impact**: None.

**Tests**:
1. Assert `App::new_demo()` has `demo_mode == true`.
2. Assert UI header renders a visible `[DEMO]` indicator when `demo_mode == true`.
3. Existing demo tests (`demo_app()` fixtures) continue to pass.

**Composition**: Independent. Can be the smallest PR in this cluster.

**Sizing**: One Sonnet dispatch. Trivial UI patch.

---

### Lane OBS-7 — Fail-closed channel and link observability (SPAN_PARTITION_DETECTED)

**Goal**: `PartitionDetected` on a duplex recv emits a trace event distinct from
`duplex_closed`, so the observer can show that a channel died due to partition (network
or process isolation) rather than clean close. Link/monitor down events get their own
trace type so the Crashes tab can show link failures without requiring a supervisor
crash report.

**Substrate touched**: `tracing.rs` (new SPAN constants: SPAN_PARTITION_DETECTED=20,
SPAN_LINK_DOWN=21; `record_channel_event` already wired, so this is two new constants
and two new arms in `drain_events_json`),
`duplex.rs` recv path at `RecvError::PartitionDetected` handling (line 143 region) —
emit SPAN_PARTITION_DETECTED,
`actor.rs` or `monitor.rs` at the monitor-down notification path — emit SPAN_LINK_DOWN,
`client.rs` `is_actionable` (add `"partition_detected"`, `"link_down"`),
no new API endpoints.

**ABI/wire impact**: Two new `event_type` values in `/api/traces`. Additive; old
observers ignore unknown types. No version break.

**Fail-closed boundary**: These events signal that something failed. Not emitting them
is itself a fail-open: the observer silently shows a channel disappear with no cause.
`PartitionDetected` is the v0.5 analogue of a network partition signal; it must be
distinguishable from normal close.

**Scope note**: This lane covers trace-level events only. Full backpressure metrics
(items/sec, blocked-producer duration) are v0.6+ surface (they require per-channel
counters not yet in the runtime). Backpressure is explicitly deferred in §6.

**Tests**:
1. Unit test: call the duplex recv path with an injected `PartitionDetected` error;
   assert `hew_trace_event_count()` increments with SPAN_PARTITION_DETECTED.
2. Assert `drain_events_json` maps SPAN_PARTITION_DETECTED to `"partition_detected"`.
3. Assert `is_actionable` returns true for `"partition_detected"` and `"link_down"`.
4. Existing duplex-closed emission tests must continue to pass (the two events are
   distinct; one cannot shadow the other).

**Composition**: Independent. Can run in parallel with all other OBS lanes.

**Sizing**: One Sonnet dispatch. Narrower than OBS-4: two constants, two emit calls,
two arms, two `is_actionable` additions.

---

## 5. Priority ranking

### Priority tier: P0 — Debugging blockers

**OBS-1 (actor type / handler registration)** — HIGHEST PRIORITY.

Rationale: Every other trace event is attributed to `"Actor"` with `None` handler name.
This is not a gap; it is a fundamental misrepresentation of running programs. A user
trying to debug message routing cannot identify which actor type is at fault. The
named-but-empty surface (the `actor_type` and `handler_name` columns in the TUI) is the
observability equivalent of a compiler that silently emits zeroes. Per Hew tenet 1
(reliability: "users never get fabricated values masquerading as correct"), this is the
most urgent fix. It is also the enabler for every other trace-based lane.

**OBS-2 (TrapKind in crash wire)** — HIGH PRIORITY.

Rationale: The Crashes tab today misrepresents the cause of every programmatic trap as
`SIGSEGV`. A `#[max_heap(N)]` exceeded crash and a real segfault look identical. This
is a named-but-empty surface on the `signal` field — it says `11` for everything. For
Hew's target user (distributed systems engineers debugging production actors), crash
cause fidelity is foundational. The fix is narrow and additive.

### Priority tier: P1 — Substrate alignment

**OBS-4 (lock trace events)** — HIGH after D24-1 codegen lands.

Rationale: Auto-injected locks are a v0.5 ship item. When they land in codegen, actors
will acquire/release locks on every message handler dispatch. Deadlocks and poison events
are a class of bugs that will be completely invisible without trace hooks. This lane
should land immediately after D24-1 codegen emission.

**OBS-5 (cancellation trace events)** — HIGH, no prerequisite.

Rationale: Cancellation tokens are a v0.5 ship item already live in the runtime.
`hew_scope_cancel` emits nothing today. A cancelled scope that doesn't propagate
(because the cooperate-site check is missed) will appear as a hung actor with no
observable cause. This lane is two constants + two record calls.

**OBS-3 (supervisor restart / circuit-breaker)** — MEDIUM.

Rationale: Supervisor restart counts and circuit-breaker state already exist in
`InternalChildSpec`. Exposing them is a low-cost correctness improvement to the
Supervisors tab, which currently shows "Supervisor" / "Running" / "Idle" with no
restart accounting. For distributed systems debugging, knowing which child is in
circuit-break state is high value. Lower priority than lock/cancel because
supervisors are partially wired already.

**OBS-7 (partition/link trace events)** — MEDIUM.

Rationale: `PartitionDetected` is a fail-closed channel event that currently silently
conflates with clean close. Distinguishing partition from close is basic fidelity for
a distributed systems tool. The lane is narrow (two constants + two emit calls) and
fully independent.

### Priority tier: P2 — Quality / honesty

**OBS-6 (demo mode badge)** — LOW but ships quickly.

Rationale: The false-positive impression from the demo screenshots is a user trust issue,
not a correctness issue. Fix it in one small PR, typically as a companion to OBS-1 to
avoid the situation where OBS-1 lands and users compare real output to the screenshots.

### Summary table

| Lane | Priority | Prerequisite | Size | Impact |
|---|---|---|---|---|
| OBS-1 (actor type registration codegen) | P0 | None | 1 Sonnet | Unblocks all named-but-empty actor attribution |
| OBS-2 (TrapKind in crash wire) | P0 | None | 1 Sonnet | Crash cause fidelity |
| OBS-4 (lock trace events) | P1 | D24-1 codegen slice | 1 Sonnet | Lock debugging visibility |
| OBS-5 (cancellation trace events) | P1 | None (CT substrate live) | 1 Sonnet | Cancel debugging visibility |
| OBS-3 (supervisor restart / circuit) | P1 | None | 1 Sonnet | Supervisor debugging depth |
| OBS-7 (partition/link trace events) | P1 | None | 1 Sonnet | Fail-closed channel fidelity |
| OBS-6 (demo mode badge) | P2 | None | 1 Sonnet | User trust / expectation setting |

---

## 6. What this plan does NOT propose

- No new API endpoint designs. All lanes use additive fields on existing endpoints.
- No OpenTelemetry / OTLP export path. That is v0.6+ surface (cross-platform parity
  with distributed tracing ecosystems is out of v0.5 scope).
- No `hew machine diagram` changes for actor/supervisor visualization. The machine diagram
  tool targets v0.6 hierarchical machines; mixing it with v0.5 actor layout would confuse
  both use cases.
- No changes to the WASM observability path. `WASM-TODO(#1451)` covers that; the profiler
  and tracing ring are native-only until WASI threads land.
- No generator yield or closure environment tracing (D24-3). Generator `yield`
  suspensions and resumptions emit nothing today (`generator.rs` has no trace calls).
  Closure environment construction (env materialization, capture allocation) also emits
  nothing. Deferred because generators are mid-flight as a substrate; the lambda
  lifecycle events (SPAN_LAMBDA_SPAWNED, SPAN_LAMBDA_RELEASED) are sufficient for v0.5.
  A future lane should add SPAN_GENERATOR_YIELD and SPAN_GENERATOR_RESUME once the
  generator ABI stabilises.
- No backpressure metrics (items/sec, blocked-producer duration on bounded channels).
  These require per-channel counters not yet in the runtime. Deferred to v0.6 channel
  surface work.
- No cluster observability (already deferred to v0.6 per mission).

---

## Key evidence references

- Named-but-empty actor_type: `hew-observe/src/client.rs:289`, `tracing.rs:693`, `#1258`
- Named-but-empty handler_name: `client.rs:300–303`, `bridge.rs:154` `WASM-TODO(#1451)`
- Zero codegen registration calls: `hew-codegen-rs/src/llvm.rs` (grep confirms none)
- SPAN constants: `tracing.rs:64–94`
- Emission coverage table: `actor.rs`, `duplex.rs`, `stream.rs`, `lambda_actor.rs` (above)
- Lock ABI exists, no trace hook: `actor.rs:222–414`
- Scope cancel exists, no trace hook: `scope.rs:308`
- Crash signal ambiguity: `crash.rs:336–358`, `signal.rs:181`
- Supervisor snapshot missing restart: `supervisor.rs:148–157`, `InternalChildSpec` fields
- Demo false claims: `app.rs:877–1192`, `hew-observe/docs/screenshots/`
