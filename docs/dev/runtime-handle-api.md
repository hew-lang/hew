# Runtime handle API design

Stage 0 records the runtime-handle shape needed by #1228 and the JIT/session
work tracked by #1226. It is intentionally documentation-only: no runtime,
codegen, C ABI, scheduler-threading, JIT-session, FFI guard, or move-checker
implementation changes are part of this stage.

## Current singleton surfaces

The runtime is process-scoped today. The new handle API must first make these
surfaces owned by a `Runtime`, then keep the existing exported symbols as
compatibility shims over one default runtime:

- Native scheduler: `hew-runtime/src/scheduler.rs:125-148` defines
  `SCHEDULER: AtomicPtr<Scheduler>` and `get_scheduler()`. The current comment
  explicitly chooses `AtomicPtr` because shutdown can drop and later re-create
  the scheduler.
- Native scheduler cleanup: `hew-runtime/src/scheduler.rs:459-492` stops the
  periodic ticker, reaps workers, frees supervisors, cleans live actors, clears
  the registry, and drops the scheduler.
- Native scheduler shutdown: `hew-runtime/src/scheduler.rs:427-447` joins
  workers, writes exit profiler data, then fires `session_reset()`. This is
  separate from `hew_runtime_cleanup()` today, but `Runtime::drop` must preserve
  the combined lifecycle invariant.
- Live actors:
  `hew-runtime/src/lifetime/live_actors.rs:43`,
  `hew-runtime/src/lifetime/live_actors.rs:179-184`, and
  `hew-runtime/src/lifetime/live_actors.rs:188-214` own the live-actor map and
  deferred actor-free thread joins. `hew-runtime/src/actor.rs:553-565` drains
  both during cleanup.
- Actor id allocation: `hew-runtime/src/actor.rs:371` owns
  `NEXT_ACTOR_SERIAL`, which must move with the live-actor registry so
  explicit-handle APIs can treat actor ids as runtime-local.
- Name registry: `hew-runtime/src/registry.rs:68` owns `REGISTRY`; the native
  cleanup path clears it through `hew_registry_clear()` at
  `hew-runtime/src/scheduler.rs:479-480`.
- Shutdown globals:
  `hew-runtime/src/shutdown.rs:40-62` owns `SHUTDOWN_PHASE` and
  `TOP_LEVEL_SUPERVISORS`.
- Timer globals:
  `hew-runtime/src/timer_periodic.rs:34-37` owns `GLOBAL_WHEEL`,
  `TICKER_RUNNING`, `TICKER_STOP`, and `TICKER_HANDLE`;
  `hew-runtime/src/timer_wheel.rs:63-67` defines the heap-owned wheel.
- Session reset hooks: `hew-runtime/src/session.rs:36-61` owns `RESET_HOOKS`
  and fires hooks in registration order.
- WASM scheduler mirror:
  `hew-runtime/src/scheduler_wasm.rs:1-18` documents the single-threaded
  scheduler shape and its C ABI mirror.

## Pinned `Runtime` field set

Stage 1 should introduce an internal, opaque runtime owner equivalent to the
following shape. Names may be adapted to local module style, but fields may not
be silently omitted; removing or merging a field requires updating this document
with the replacement ownership path.

```rust,ignore
pub struct Runtime {
    // Scheduler execution state.
    scheduler: ManuallyDrop<SchedulerState>,

    // Actor ownership and liveness.
    live_actors: ManuallyDrop<LiveActorsState>,
    deferred_actor_free_threads: ManuallyDrop<DeferredActorFreeThreads>,
    next_actor_serial: AtomicU64,

    // Name lookup and supervision.
    registry: ManuallyDrop<ShardedRegistry>,
    top_level_supervisors: ManuallyDrop<SupervisorRoots>,
    shutdown_phase: AtomicI32,

    // Time.
    timer_wheel: ManuallyDrop<TimerWheelState>,
    periodic_ticker: ManuallyDrop<PeriodicTickerState>,

    // Per-runtime subsystem reset hooks.
    reset_hooks: ManuallyDrop<ResetHookRegistry>,

    // Lifecycle guard used by Drop and public destroy functions.
    drop_state: AtomicI32,
}
```

Field meanings:

- `SchedulerState` is the state currently hidden behind
  `hew-runtime/src/scheduler.rs::Scheduler`: worker handles, global queue,
  local queues, stealers, parkers, worker-count metadata, and the native
  shutdown flag. On WASM it is the cooperative run queue state from
  `hew-runtime/src/scheduler_wasm.rs`.
- `LiveActorsState` owns the `HashMap<ActorId, ActorPtr>` currently held in
  `hew-runtime/src/lifetime/live_actors.rs`. `DeferredActorFreeThreads` is
  native-only but must still be owned by the runtime on native targets because
  cleanup waits for those joins before draining the map.
- `next_actor_serial` moves with the actor registry. For explicit-handle APIs an
  `ActorId` is only meaningful with the `Runtime` that produced it; the
  non-suffixed compatibility API continues to use the default runtime.
- `ShardedRegistry` is the current named actor registry. It is per-runtime, not
  process-wide, so two explicit runtimes can register the same actor name
  independently.
- `SupervisorRoots` and `shutdown_phase` replace `TOP_LEVEL_SUPERVISORS` and
  `SHUTDOWN_PHASE`.
- `TimerWheelState` owns the heap timer wheel; `PeriodicTickerState` owns the
  native ticker join handle and stop/running flags. On WASM this state may be a
  no-thread cooperative timer state, but the field remains present behind target
  cfgs so the ABI posture stays the same.
- `ResetHookRegistry` replaces the process-global `RESET_HOOKS`. Hooks are
  registered with a runtime, and `Runtime::drop` fires only that runtime's hook
  list.
- `drop_state` is not user visible. It makes `Runtime::drop`,
  `hew_runtime_destroy`, and legacy cleanup shims idempotent and fail-closed
  under double-destroy attempts.

The fields are wrapped in `ManuallyDrop` to make Rust declaration order
irrelevant. The only valid teardown is the explicit order below.

## Cleanup and drop order

`Runtime::drop` must implement the full shutdown/cleanup sequence explicitly.
It must not rely on field declaration order, and it must be safe if the caller
never called the legacy `hew_sched_shutdown()` / `hew_runtime_cleanup()` pair.

Required order:

1. Atomically move `drop_state` out of `running`. A second destroy returns
   without touching fields.
2. Stop the periodic ticker and free the timer wheel, matching the first cleanup
   step in `hew_runtime_cleanup()` (`hew-runtime/src/scheduler.rs:461-463`)
   and the ticker shutdown contract in
   `hew-runtime/src/timer_periodic.rs:358-405`.
3. Tear down workers with the reaper-promotion path, matching
   `teardown_workers(get_scheduler(), None, true)` in
   `hew-runtime/src/scheduler.rs:465-468`. This must join or detach every
   worker before actor, timer, registry, or supervisor memory can be reclaimed.
4. Write exit profiler data before reset hooks if the profiler feature is
   active, preserving `hew_sched_shutdown()`'s ordering at
   `hew-runtime/src/scheduler.rs:435-446`.
5. Fire this runtime's `reset_hooks` in registration order, preserving the
   `hew-runtime/src/session.rs:48-61` hook contract. Hooks must not come after
   registry or dispatch-type cleanup if those hooks need the side tables to be
   present.
6. Free registered top-level supervisors, matching
   `hew-runtime/src/scheduler.rs:469-474`.
7. Drain deferred actor-free threads, drain live actors, and finalize actor
   cleanup, matching `hew-runtime/src/actor.rs:553-565`.
8. Clear the per-runtime name registry, matching
   `hew-runtime/src/scheduler.rs:479-480`.
9. Drop scheduler queues, deques, stealers, parkers, and worker metadata last,
   matching `hew-runtime/src/scheduler.rs:490-491`.
10. Mark `drop_state` complete.

The current public API splits this lifecycle between `hew_sched_shutdown()` and
`hew_runtime_cleanup()`. The handle API may keep those functions as shims, but
`Runtime::drop` is the safety authority: it must be able to run the whole order
once for explicit runtimes.

## Default singleton wrapper

Existing compiled Hew programs must continue to call the non-suffixed exported
symbols. Those symbols resolve a default process runtime through a compatibility
wrapper:

```rust,ignore
static DEFAULT_RUNTIME: AtomicPtr<Runtime> = AtomicPtr::new(ptr::null_mut());

fn rt_default() -> &'static Runtime;
```

Decision: use an `AtomicPtr<Runtime>` wrapper for the active default runtime,
not `OnceLock<&'static Runtime>`, for Stage 1/2.

Rationale:

- The current scheduler intentionally uses `AtomicPtr` rather than `OnceLock`
  because shutdown drops the allocation and tests may reinitialize it
  (`hew-runtime/src/scheduler.rs:125-129`).
- `OnceLock<&'static Runtime>` would either leak the default runtime forever or
  require an additional indirection that can be cleared. Leaking would conflict
  with `hew_runtime_cleanup()`'s documented purpose of freeing runtime
  resources (`hew-runtime/src/scheduler.rs:449-458`).
- `AtomicPtr` keeps the legacy no-op second init behaviour expressible:
  `hew_sched_init()` installs the default once; concurrent calls that lose the
  compare-exchange drop their candidate and return success.

Posture:

- The singleton is a compatibility wrapper only. New internal Rust APIs take
  `&Runtime`; new public C ABI variants take `HewRuntime *`.
- `hew_sched_init()` and explicit `Runtime::new` / `hew_runtime_new()` are
  mutually exclusive for the default slot. A program that uses explicit handles
  must not call `hew_sched_init()` for those handles.
- `hew_sched_shutdown()` and `hew_runtime_cleanup()` remain stable exported
  symbols forever. They become wrappers that operate on `rt_default()` and do
  not close over process-global subsystem state.

## C ABI suffix convention

Stage 3 introduces handle-aware symbols by appending `_with_runtime` to the
existing symbol name:

- `hew_actor_spawn` -> `hew_actor_spawn_with_runtime`
- `hew_actor_send_by_id` -> `hew_actor_send_by_id_with_runtime`
- `hew_registry_register` -> `hew_registry_register_with_runtime`
- `hew_timer_wheel_schedule` -> `hew_timer_wheel_schedule_with_runtime`

Rules:

- The first parameter is always `HewRuntime *rt`.
- The rest of the parameter list preserves the non-suffixed symbol order and
  meaning.
- The non-suffixed symbol remains ABI-stable and forwards to the default
  runtime.
- New exported runtime lifetime symbols use nouns, not the suffix:
  `hew_runtime_new`, `hew_runtime_destroy`, and, if needed,
  `hew_runtime_default`.
- A null `HewRuntime *` fails closed with the same error family as other C ABI
  guard failures; it must not silently fall back to the default runtime.

The append-suffix form is chosen over a new prefix because it keeps existing
symbol groups adjacent in generated headers and makes compatibility shims
obvious during symbol-set review.

## WASM stance

WASM must expose the same handle-shaped ABI names as native once Stage 3 starts.
The behavioural target is parity for one runtime. Stage 4's "two runtimes in one
process" guarantee is native-first and may remain unavailable on WASM until the
cooperative scheduler can prove independent queues, timers, actor ids, and reset
hooks without hidden process globals.

Decision criteria before claiming multi-runtime WASM support:

1. `hew-runtime/src/scheduler_wasm.rs` owns all run-queue, actor, and reset
   state through `Runtime`, not module statics.
2. No WASM exported `*_with_runtime` symbol ignores a non-null runtime handle.
3. Timer and bridge metadata are per-runtime or explicitly proven immutable.
4. Tests cover two WASM runtimes in one host process, or the gap is documented
   with a tracked WASM-TODO and the native multi-runtime test is cfg-gated.

Until those criteria pass, WASM accepts the handle shape for ABI parity but only
promises one live runtime. Native-only behaviour must be named in code and PR
text; it must not be silent.

## JIT/session forward compatibility

This design does not choose the #1226 session model. It reserves the ownership
surface that each candidate needs:

- A per-session model can give every JIT session its own `Runtime`.
- A shared-runtime model can pass one `Runtime` through several session scopes
  while keeping reset hooks and actor roots explicit.
- A hybrid model can share scheduler workers while still placing actor roots,
  registry entries, timers, and reset hooks behind session-specific runtime
  fields if a later design splits `SchedulerState` out of `Runtime`.

The JIT host ABI classification remains conservative: lifecycle and default
runtime management symbols stay internal, matching
`docs/internal/jit-host-abi.md`. JIT-compiled modules should receive only the
stable handle-aware operations they need and the opaque `HewRuntime *` for their
session.

## Stage 0 decision

Go to Stage 1 once this document is reviewed. Stage 1 may introduce the private
`Runtime` owner and move no call sites. It must come back to Stage 0 if any
field above cannot be represented without a hidden process-global owner.

Stage 0 advances #1228 and #1226, but it does not close #1228.
