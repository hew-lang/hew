//! Runtime instance state — the de-globalized home for process-wide
//! authorities.
//!
//! # Why this module exists
//!
//! Runtime authorities (the scheduler, the live-actor table, the name registry,
//! shutdown phase, timers, the reactor, the node slot, …) need a single owned
//! teardown root. That keeps teardown order explicit and makes multi-runtime
//! support possible without shared-global test serialization.
//!
//! [`RuntimeInner`] gathers those authorities into one owned struct. A single
//! **droppable** default slot ([`DEFAULT_RUNTIME`]) holds the one runtime that
//! today's AOT and JIT programs use. The slot is an [`AtomicPtr`] (not a
//! `OnceLock`) because the runtime must be *freeable* on
//! `hew_runtime_cleanup`, releasing the scheduler's
//! crossbeam deques, parkers, and stealer handles. A bare `OnceLock<RuntimeInner>`
//! could never free.
//!
//! # M1 scope (internal de-globalization only)
//!
//! This module adds **no** new `#[no_mangle] extern "C"` symbol. `RuntimeInner`,
//! and the slot helpers are pure Rust internals. The default runtime is
//! constructed and torn down exclusively through the existing lifecycle entry
//! points (`hew_sched_init` / `hew_runtime_cleanup` / `hew_sched_shutdown`).
//! The public host/embedding handle API (`hew_runtime_new/retain/release/...`)
//! is a later deliverable and is intentionally absent here.
//!
//! Subsystems migrate into [`RuntimeInner`] one at a time; each migration flips
//! a resolver's *source* to read its field off [`rt_default`] without changing
//! any caller or any observable behaviour. Single-runtime behaviour is
//! identical: there is exactly one runtime, and every resolver reads it.

#![cfg(not(target_arch = "wasm32"))]

use std::cell::Cell;
use std::sync::atomic::{AtomicI32, AtomicPtr, Ordering};

use crate::hew_node::NodeSlot;
use crate::lifetime::live_actors::LiveActors;
use crate::lifetime::poison_safe::PoisonSafe;
use crate::registry::ShardedRegistry;
use crate::scheduler::Scheduler;
use crate::shutdown::SupervisorPtr;

// `RuntimeId` is defined in the always-compiled `runtime_id` module (it must
// exist on wasm too, where this native `runtime` module is configured out, so
// `HewActor` and its wasm mirror can stamp the same type). Re-exported here so
// native callers keep using `crate::runtime::RuntimeId`.
pub use crate::runtime_id::RuntimeId;

/// The owned state of one Hew runtime instance.
///
/// Each field is an owned runtime authority. Subsystems migrate into this
/// struct one authority at a time.
///
/// Dropping a `RuntimeInner` drops every owned field in declaration order.
/// For the scheduler this releases the crossbeam deques, parkers, stealer
/// handles, and global injector queue — exactly the resources the old
/// `AtomicPtr<Scheduler>` slot existed to free on cleanup.
pub(crate) struct RuntimeInner {
    /// Stable identity of this runtime instance.
    id: RuntimeId,
    /// M:N work-stealing scheduler. Was the `SCHEDULER` global pointer.
    pub(crate) scheduler: Scheduler,
    /// Live-actor liveness registry + deferred-teardown join handles. Was the
    /// `LIVE_ACTORS` + `DEFERRED_TEARDOWN_THREADS` globals.
    pub(crate) live_actors: LiveActors,
    /// Name registry mapping actor names to pointers (256 `RwLock` shards). Was
    /// the `REGISTRY` global. Swept by `hew_registry_clear` during cleanup while
    /// the runtime is still installed, then dropped with the runtime.
    pub(crate) registry: ShardedRegistry,
    /// Current graceful-shutdown phase (running/quiesce/drain/terminate/done/
    /// failed). Was the `SHUTDOWN_PHASE` global. `PHASE_RUNNING == 0`, so a
    /// freshly-constructed runtime starts running.
    pub(crate) shutdown_phase: AtomicI32,
    /// Registered top-level supervisors stopped (bottom-up) during graceful
    /// shutdown and freed by `hew_runtime_cleanup`. Was the
    /// `TOP_LEVEL_SUPERVISORS` global.
    pub(crate) supervisor_roots: PoisonSafe<Vec<SupervisorPtr>>,
    /// Distributed-node state: the active node, local node id, known-node list,
    /// and remote-ask reply table. Was the `CURRENT_NODE` + `pid::LOCAL_NODE_ID`
    /// + `KNOWN_NODES` + `REPLY_TABLE` globals.
    pub(crate) node: NodeSlot,
    /// User-defined metric registry (`std::metrics`) and its fail-closed
    /// self-metrics. Was the `metrics::REGISTRY` + `NAMES_DROPPED` /
    /// `SERIES_DROPPED` / `INVALID_OPS` / `COLLISION_REJECTED` globals; resolved
    /// through `rt_current().metrics` by the `metrics` module. A second runtime
    /// scrapes its own counters without aliasing another's.
    pub(crate) metrics: crate::metrics::MetricsState,
    /// Actor monitor table and monitor-reference counter. Was the
    /// `monitor::MONITOR_TABLE` + `MONITOR_REF_COUNTER` globals; resolved
    /// through `rt_current().monitors` by the monitor module.
    pub(crate) monitors: crate::monitor::MonitorState,
    /// Distributed (cross-node) monitor table. Resolved through
    /// `rt_current().dist_monitors` by the `dist_monitor` module — the
    /// node-keyed analogue of `monitors`, holding watcher-side registrations
    /// (the `hew_node_monitor_recv` observation slots) and target-side remote
    /// watchers (the terminal-sweep fan-out list).
    pub(crate) dist_monitors: crate::dist_monitor::DistMonitorState,
    /// Per-instance drop observer for the "exactly once" teardown oracles.
    ///
    /// Replaces the former process-global `RUNTIME_INNER_DROPS` counter, whose
    /// delta-based (`before` → `before + 1`) assertions were corrupted under
    /// parallel test execution: any `RuntimeInner` built and dropped by a test
    /// in another module (which does not hold `SCHED_TEST_MUTEX`) bumped the
    /// shared counter between an oracle's `before` read and its final assert,
    /// producing an intermittent off-by-one (issue #2572). A probe is bound to a
    /// single runtime instance, so it counts *only that instance's* drop and is
    /// immune to concurrent drops elsewhere. Left `None` for every runtime a
    /// test does not explicitly observe.
    #[cfg(test)]
    pub(crate) drop_probe: std::sync::Mutex<Option<std::sync::Arc<std::sync::atomic::AtomicUsize>>>,
}

impl RuntimeInner {
    /// Construct the inner state for a runtime that owns `scheduler`.
    pub(crate) fn new(scheduler: Scheduler) -> Self {
        Self {
            id: RuntimeId::DEFAULT,
            scheduler,
            live_actors: LiveActors::new(),
            registry: ShardedRegistry::new(),
            shutdown_phase: AtomicI32::new(crate::shutdown::PHASE_RUNNING),
            supervisor_roots: PoisonSafe::new(Vec::new()),
            node: NodeSlot::new(),
            metrics: crate::metrics::MetricsState::new(),
            monitors: crate::monitor::MonitorState::new(),
            dist_monitors: crate::dist_monitor::DistMonitorState::new(),
            #[cfg(test)]
            drop_probe: std::sync::Mutex::new(None),
        }
    }

    /// Test-only constructor minting a runtime that carries an explicit id.
    ///
    /// Production [`RuntimeInner::new`] hardcodes [`RuntimeId::DEFAULT`] because
    /// single-runtime AOT/JIT programs have exactly one runtime; minting a
    /// `RuntimeId > DEFAULT` in production is the M4 multi-runtime milestone.
    /// This `#[cfg(test)]` constructor lets the multi-runtime trap tests stand
    /// up a second worker-less `RuntimeInner` carrying e.g. `RuntimeId(1)`, so
    /// the fail-closed nets in [`enter_actor_runtime`] and the scheduler wake
    /// path can be *observed* firing for a foreign-runtime resolution rather
    /// than passing vacuously
    /// (`static-classification-vacuates-refcount-and-sanitizer`).
    #[cfg(test)]
    pub(crate) fn new_with_id_for_test(scheduler: Scheduler, id: RuntimeId) -> Self {
        let mut inner = Self::new(scheduler);
        inner.id = id;
        inner
    }

    /// This runtime's identity.
    ///
    /// The actor spawn path stamps every actor with its spawning runtime's id
    /// (`build_spawned_actor` reads `rt_current().runtime_id()`), and the
    /// cross-runtime send/ask/by-id check compares the calling runtime's id
    /// against the target actor's stamped id. Named `runtime_id` rather than
    /// `id` to disambiguate from the actor PID, which is also an `id`.
    #[inline]
    #[allow(
        dead_code,
        reason = "consumed by the actor spawn stamp and cross-runtime send check in the following commit"
    )]
    pub(crate) fn runtime_id(&self) -> RuntimeId {
        self.id
    }
}

/// Test-only per-instance drop observer: bind a fresh counter to a single
/// installed `RuntimeInner` so an "exactly once" teardown oracle counts only
/// *that instance's* drop.
///
/// This replaces the former process-global `RUNTIME_INNER_DROPS` counter, whose
/// delta assertions were corrupted under parallel test execution by unrelated
/// `RuntimeInner`s built and dropped in other modules (issue #2572). The caller
/// must hold `SCHED_TEST_MUTEX` and pass a pointer to a live runtime it owns.
#[cfg(test)]
pub(crate) fn install_drop_probe_for_test(
    ptr: *const RuntimeInner,
) -> std::sync::Arc<std::sync::atomic::AtomicUsize> {
    let probe = std::sync::Arc::new(std::sync::atomic::AtomicUsize::new(0));
    // SAFETY: `ptr` refers to a live `RuntimeInner` the caller owns and keeps
    // installed for the duration of the test; `drop_probe` uses interior
    // mutability (a `Mutex`) so a shared reference may set it.
    let inner = unsafe { &*ptr };
    *inner
        .drop_probe
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner) = Some(std::sync::Arc::clone(&probe));
    probe
}

/// Test-only drop observer. Production builds compile no `Drop` impl, so the
/// inner frees by field-drop; the `#[cfg(test)]` impl only records that the
/// field-drop happened (into an optional per-instance probe), then lets the
/// fields drop normally as it returns.
#[cfg(test)]
impl Drop for RuntimeInner {
    fn drop(&mut self) {
        if let Ok(mut guard) = self.drop_probe.lock() {
            if let Some(probe) = guard.take() {
                probe.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
            }
        }
    }
}

// ── Default runtime slot ────────────────────────────────────────────────

/// Default runtime pointer. Installed once by `hew_sched_init()` (via
/// [`install_default`]), freed by `hew_runtime_cleanup()` (via
/// [`take_default`]). Using `AtomicPtr` rather than `OnceLock` keeps the
/// inner state *droppable*: cleanup detaches the pointer and drops the boxed
/// `RuntimeInner`, freeing the scheduler's deques/parkers/stealers — a
/// `OnceLock<RuntimeInner>` could never free.
pub(crate) static DEFAULT_RUNTIME: AtomicPtr<RuntimeInner> = AtomicPtr::new(std::ptr::null_mut());

/// Resolve a reference to the default `RuntimeInner`, if one is installed.
///
/// # Safety of the returned reference
///
/// The reference is valid until `hew_runtime_cleanup()` detaches and drops the
/// inner. Cleanup runs only after all worker threads have been joined, so any
/// caller reachable from worker/dispatch code holds a valid reference for the
/// duration of its use.
#[inline]
pub(crate) fn rt_default() -> Option<&'static RuntimeInner> {
    let ptr = DEFAULT_RUNTIME.load(Ordering::Acquire);
    if ptr.is_null() {
        None
    } else {
        // SAFETY: non-null means `install_default` published a boxed
        // `RuntimeInner`, valid until `take_default` frees it after all
        // workers are joined.
        Some(unsafe { &*ptr })
    }
}

thread_local! {
    /// Per-thread current runtime, selected by `enter()` at a dispatch
    /// boundary or worker entry. Null when this thread is off the dispatch
    /// path (the I/O reactor, the teardown reaper drain, the periodic-timer
    /// ticker, a fresh program thread before `hew_sched_init`), in which case
    /// [`rt_current`] resolves through the [`DEFAULT_RUNTIME`] fallback.
    ///
    /// Const-init null mirrors `CURRENT_EXECUTION_CONTEXT`
    /// (`execution_context.rs`). The pointer, when non-null, was installed by
    /// `enter()` against a `RuntimeInner` that outlives the installing guard on
    /// this thread — scheduler workers `enter()` their owning runtime at loop
    /// entry, and the actor terminate body `enter()`s it beside the
    /// execution-context install so user `on(stop)` code resolves the right
    /// runtime. The slot is non-null on those entered paths and null off them
    /// (where the read falls through to the default).
    static CURRENT_RUNTIME: Cell<*const RuntimeInner> = const { Cell::new(std::ptr::null()) };
}

/// Resolve the runtime that owns the work on this thread — TLS-first, then the
/// default-slot fallback, fail-closed when neither is installed.
///
/// This is the resolver every de-globalized subsystem reads through (the
/// live-actor registry, name registry, shutdown phase, supervisor roots,
/// timers, node slot). It reads the per-thread [`CURRENT_RUNTIME`] selection
/// first; off the dispatch path (the slot is null) it returns the installed
/// default runtime; with neither installed it **panics**.
///
/// # The default-slot fallback (off-dispatch path)
///
/// The `else` arm reads [`DEFAULT_RUNTIME`] and is **load-bearing in M2, not a
/// fail-open shortcut**. Off-dispatch threads never `enter()` — the I/O reactor
/// reading `live_actors`, the teardown reaper drain reading
/// `deferred_teardown_threads` — and resolve their (single, default) runtime
/// through this arm. Deleting it would re-trap every off-dispatch reader. It is
/// removed at M4 (the two-runtime / JIT-entry milestone), once a TLS install
/// covers every dispatch path and a null slot can fail closed as a missed
/// `enter()`; `no-fail-open-fallback-after-authority` fires there, not here —
/// single-runtime totality is not yet proven.
///
/// # Why fail-closed, not lazy-default
///
/// The explicit-install model (Q119/A119) requires a runtime to be installed
/// before any runtime authority is touched: AOT and JIT programs install it via
/// `hew_sched_init`; tests install it via the runtime test guard
/// (`crate::runtime_test_guard`) or `NoWorkerSchedulerForTest`. There is no
/// lazy auto-create — both the TLS slot and the default slot being unset is a
/// hard logic error, so it traps loudly instead of fabricating state that would
/// leak past the missing lifecycle call.
#[inline]
pub(crate) fn rt_current() -> &'static RuntimeInner {
    let p = CURRENT_RUNTIME.with(Cell::get);
    if p.is_null() {
        // Off-dispatch / single-runtime fallback (M1 behaviour preserved).
        // Removed at M4; required now for the off-dispatch readers above.
        rt_default().unwrap_or_else(|| {
            panic!(
                "hew-runtime: no runtime installed — a runtime authority was used \
                 before hew_sched_init (production) or without a runtime test guard \
                 (tests). Install a default RuntimeInner first."
            )
        })
    } else {
        // SAFETY: a non-null TLS pointer was installed by `enter()` against a
        // `RuntimeInner` that outlives the installing guard on this thread
        // (the guard restores the previous pointer on drop). This arm is taken
        // on scheduler workers (worker-loop `enter()`) and in the actor
        // terminate body (terminate `enter()`).
        unsafe { &*p }
    }
}

/// Resolve the id of the runtime bound on this thread, without panicking when
/// none is installed.
///
/// Same TLS-first → default-slot resolution order as [`rt_current`], but
/// returns `None` instead of trapping when neither is installed. The
/// cross-runtime send boundary uses this so it can run on a thread that has no
/// runtime bound (an alias send issued before `hew_sched_init`, or in a unit
/// test that drives a send path without a runtime guard): with no runtime
/// installed there is no second runtime an actor could be foreign to, so the
/// boundary treats the pointer as in-runtime rather than fabricating a trap the
/// pre-check never used to take. Reads only the id discriminant, never a
/// borrowed authority.
#[inline]
pub(crate) fn rt_current_id() -> Option<RuntimeId> {
    let p = CURRENT_RUNTIME.with(Cell::get);
    if p.is_null() {
        rt_default().map(RuntimeInner::runtime_id)
    } else {
        // SAFETY: a non-null TLS pointer was installed by `enter()` against a
        // `RuntimeInner` that outlives the installing guard on this thread,
        // exactly as in `rt_current`.
        Some(unsafe { (*p).runtime_id() })
    }
}

/// Like [`rt_current`] but returns `None` instead of panicking when no runtime
/// is installed. Same TLS-first → default-slot resolution order.
///
/// The observability read/reset surface (`metrics::render_snapshot`,
/// `metrics::self_metrics`, `metrics::session_reset_metrics`) resolves through
/// this: a host may scrape or reset before `hew_sched_init` installs a runtime
/// or after teardown drops it, where there are simply no per-runtime metrics to
/// read or clear. The register/mutate surface keeps the fail-closed
/// [`rt_current`] resolver, since emitting a metric requires its owning runtime.
#[inline]
pub(crate) fn rt_current_opt() -> Option<&'static RuntimeInner> {
    let p = CURRENT_RUNTIME.with(Cell::get);
    if p.is_null() {
        rt_default()
    } else {
        // SAFETY: a non-null TLS pointer was installed by `enter()` against a
        // `RuntimeInner` that outlives the installing guard on this thread,
        // exactly as in `rt_current`.
        Some(unsafe { &*p })
    }
}

/// Install `rt` as this thread's [`CURRENT_RUNTIME`] for the lifetime of the
/// returned guard, restoring the previously-installed runtime when the guard
/// drops.
///
/// Mirrors [`crate::execution_context::set_current_context`] (a `Cell::replace`
/// returning the previous pointer) plus the `TestExecutionContext` RAII restore
/// — the same save/restore precedent the dispatch boundary already trusts. The
/// guard's `Drop` restores the saved pointer on **every** exit edge (normal
/// return, panic, trap), so nested `enter()` calls compose: an inner guard
/// restores the outer thread-current on drop, and unwinding through an
/// `enter()` scope still rebinds the previous runtime (`lifecycle-symmetry`).
///
/// `#[must_use]`: dropping the guard immediately would restore the previous
/// runtime on the very next line, defeating the install.
///
/// # Safety
///
/// `rt` must outlive **every** [`rt_current`] dereference on this thread until
/// the returned [`EnterGuard`] drops. `enter` stores `rt` as a raw pointer in
/// [`CURRENT_RUNTIME`], and `rt_current` hands that pointer back as an apparent
/// `&'static RuntimeInner` (see its `# SAFETY` arm). A `&RuntimeInner` borrow
/// shorter than the guard's scope would let `rt_current` observe a dangling
/// reference after `rt` is freed — a use-after-free. The borrow's lifetime is
/// not checked by the compiler against the guard, so the caller carries that
/// obligation. No Arc or refcount is taken; ownership stays with whoever owns
/// the runtime.
///
/// The production callers discharge this as follows:
/// - scheduler workers hold a `*const RuntimeInner` valid until join
///   (`WorkerRuntimePtr`), so the entered runtime outlives the worker loop;
/// - the actor terminate body enters the default runtime (`rt_default`), which
///   is process-lifetime once installed.
#[must_use]
#[cfg_attr(
    target_arch = "wasm32",
    allow(
        dead_code,
        reason = "the production callers (scheduler workers, actor terminate body) are native-only (cfg(not(wasm32))); the wasm32 cooperative runtime has no threads to enter, so this is unused there but kept for parity with the native resolver"
    )
)]
pub(crate) unsafe fn enter(rt: &RuntimeInner) -> EnterGuard {
    let prev = CURRENT_RUNTIME.with(|c| c.replace(rt as *const RuntimeInner));
    EnterGuard { prev }
}

/// Enter the runtime that owns `actor`, with a fail-closed trap on a
/// multi-runtime skew.
///
/// This is the single choke point off-dispatch producers will call when they
/// already have an actor pointer but no worker-thread `enter()` binding. Actors
/// spawned by this runtime carry a non-owning pointer to their owning
/// [`RuntimeInner`], sourced from the same `rt_current()` read that stamps
/// `runtime_id` (`build_spawned_actor`). Legacy/test actors may carry null.
///
/// # Posture (`no-fail-open-fallback-after-authority`)
///
/// The `Option` return and the M2 single-runtime [`rt_default`] fallback are
/// kept **only** for `runtime_id == RuntimeId::DEFAULT` — the legitimate
/// legacy/test/off-dispatch readers of the one default runtime. The moment an
/// actor carries `runtime_id != DEFAULT` and would resolve through the default
/// (the skew/contradiction branch: a non-null actor with a null `runtime`
/// stamp, which spawn makes impossible), this **traps** rather than silently
/// binding the default or returning `None`. Returning `None` would not be safe:
/// the producer would proceed on the bare `rt_current()` default, re-opening
/// the same silent-default hazard. Outright deletion of the fallback is M4; this
/// lane instruments it (assert-net first), it does not delete it.
///
/// # Safety
///
/// If `actor` is non-null, it must point to a live [`crate::actor::HewActor`] for
/// the duration of this call. The stored runtime pointer is non-owning: it is
/// valid because `RuntimeInner` owns/outlives its actors and cleanup drops the
/// runtime only after actors/workers are drained. No ownership or refcount is
/// taken, so this avoids a runtime→actor→runtime cycle.
#[cfg_attr(
    not(test),
    allow(
        dead_code,
        reason = "Stage 2 slice 1 installs + tests the choke point and its fail-closed trap; the off-dispatch producer cutovers (timer/reactor/teardown, slices 2-4) make it reachable in non-test builds"
    )
)]
#[must_use]
pub(crate) unsafe fn enter_actor_runtime(
    actor: *const crate::actor::HewActor,
) -> Option<EnterGuard> {
    let rt = if actor.is_null() {
        // Branch (A) — genuinely actor-less caller. No actor carries a
        // `runtime_id` the resolved runtime could contradict, so the M2
        // single-runtime default fallback is correct and is kept. Stage-2
        // producers always hold an actor pointer at the cutover, so this arm is
        // unreachable from them.
        rt_default()?
    } else {
        // SAFETY: caller guarantees a non-null actor pointer is live for this
        // read; the field is an immutable spawn-time stamp.
        let actor_rt = unsafe { (*actor).runtime };
        // SAFETY: as above — `runtime_id` is an immutable spawn-time stamp on the
        // same live actor.
        let actor_id = unsafe { (*actor).runtime_id };
        if actor_rt.is_null() {
            // Branch (B) — non-null actor with a null owning-runtime stamp.
            //
            // A single-runtime actor (`runtime_id == DEFAULT`) legitimately
            // carries a null stamp: legacy/test actors and the off-dispatch
            // readers resolve the installed default. That M2 fallback stays.
            //
            // A multi-runtime actor (`runtime_id != DEFAULT`) with a null stamp
            // is a skew CONTRADICTION: spawn stamps `runtime` and `runtime_id`
            // from the SAME `rt_current()` read, so a non-DEFAULT id can never
            // pair with a null stamp unless an invariant is broken. Silently
            // entering the default would re-open the hazard the stamp closes, so
            // it TRAPS.
            assert!(
                actor_id == RuntimeId::DEFAULT,
                "hew-runtime: enter_actor_runtime: actor stamped runtime_id {} carries a \
                 null owning-runtime pointer; spawn stamps `runtime` and `runtime_id` from \
                 the same runtime, so a non-default id with no stamp is corruption — \
                 refusing to silently bind the default runtime",
                actor_id.as_u64(),
            );
            let fallback = rt_default()?;
            // The resolved default must own this actor. Single-runtime both ids
            // are DEFAULT and this holds; a mismatch means the default slot does
            // not own the actor, so fail closed rather than bind it.
            assert!(
                fallback.runtime_id() == actor_id,
                "hew-runtime: enter_actor_runtime: default runtime id {} does not match \
                 actor runtime_id {}; refusing to bind a foreign runtime",
                fallback.runtime_id().as_u64(),
                actor_id.as_u64(),
            );
            fallback
        } else {
            // Owning branch — the actor carries its owning runtime. The stamp's
            // id must equal the actor's `runtime_id` (the by-construction skew
            // invariant); a mismatch is a corrupted stamp, so fail closed rather
            // than enter a runtime the actor does not claim.
            // SAFETY: the actor's non-owning runtime pointer outlives the actor
            // by the RuntimeInner owns/outlives actors contract documented above.
            let owner = unsafe { &*actor_rt };
            assert!(
                owner.runtime_id() == actor_id,
                "hew-runtime: enter_actor_runtime: owning-runtime stamp id {} disagrees with \
                 actor runtime_id {} (skew); refusing to bind a mismatched runtime",
                owner.runtime_id().as_u64(),
                actor_id.as_u64(),
            );
            owner
        }
    };

    // SAFETY: `rt` came from either the actor's owning-runtime stamp or the
    // installed default runtime; both outlive the returned guard under their
    // respective lifecycle contracts.
    Some(unsafe { enter(rt) })
}

/// RAII guard returned by [`enter`]; restores the previous [`CURRENT_RUNTIME`]
/// pointer when dropped.
///
/// Holds the pointer that was current when [`enter`] ran (null when the thread
/// was off the dispatch path). The guard never owns or frees the runtime it
/// installed — it only owns the *restore* of the previous selection — so it is
/// safe to drop after the entered runtime itself is gone, as long as the
/// previous pointer it restores is still valid (it is, by the same
/// outlives-the-guard contract that governs the entered runtime).
#[cfg_attr(
    target_arch = "wasm32",
    allow(
        dead_code,
        reason = "constructed only by `enter`, whose production callers are native-only; unused on the cooperative wasm32 runtime"
    )
)]
pub(crate) struct EnterGuard {
    prev: *const RuntimeInner,
}

impl Drop for EnterGuard {
    fn drop(&mut self) {
        CURRENT_RUNTIME.with(|c| c.set(self.prev));
    }
}

/// Publish `inner` as the default runtime via compare-exchange.
///
/// Returns `true` when this call installed `inner`. Returns `false` when a
/// runtime was already installed; in that case the passed-in box is dropped so a
/// second `hew_sched_init` remains a harmless no-op.
pub(crate) fn install_default(inner: Box<RuntimeInner>) -> bool {
    let ptr = Box::into_raw(inner);
    if DEFAULT_RUNTIME
        .compare_exchange(
            std::ptr::null_mut(),
            ptr,
            Ordering::AcqRel,
            Ordering::Relaxed,
        )
        .is_err()
    {
        // Another initialization beat us — drop ours.
        // SAFETY: we just allocated this box via `Box::into_raw`.
        drop(unsafe { Box::from_raw(ptr) });
        return false;
    }
    true
}

/// Detach and reclaim the default runtime, if one is installed.
///
/// The caller owns the returned box and decides when it drops — letting
/// `hew_runtime_cleanup` perform the scheduler-frees-last drop order. Returns
/// `None` when no runtime was installed.
pub(crate) fn take_default() -> Option<Box<RuntimeInner>> {
    let ptr = DEFAULT_RUNTIME.swap(std::ptr::null_mut(), Ordering::AcqRel);
    if ptr.is_null() {
        None
    } else {
        // SAFETY: the pointer was installed by `install_default`; the caller
        // guarantees (via the cleanup contract) that all workers are joined,
        // so no thread can still reference it.
        Some(unsafe { Box::from_raw(ptr) })
    }
}

/// Raw default-runtime pointer for the worker-loop swap-detection handshake.
///
/// Workers cache the pointer they bound to and compare against this on each
/// iteration; the test-only scheduler-swap harness relies on the comparison
/// to drain cleanly before freeing a transient runtime. Production never swaps
/// the pointer after init, so this comparison is compiled out of release
/// builds — hence the `#[cfg(test)]` gate.
#[cfg(test)]
#[inline]
pub(crate) fn default_runtime_ptr(ordering: Ordering) -> *mut RuntimeInner {
    DEFAULT_RUNTIME.load(ordering)
}

// ── Test-only default-slot manipulation ──────────────────────────────────
//
// The scheduler's free-path UAF tests install a worker-less scheduler and
// later restore the previous one. Before de-globalization they swapped the
// `SCHEDULER` pointer directly; now they swap the `RuntimeInner` pointer. These
// helpers keep that manipulation in one place and out of the production path.

/// Report whether the runtime at `ptr` owns a worker-backed scheduler.
///
/// The runtime test guard installs only **worker-less** placeholder runtimes,
/// and must reclaim its placeholder on drop only while it is still that
/// placeholder. Pointer-equality alone is unsound: when a test upgrades to a
/// real scheduler (`init_real_scheduler_for_test` frees the placeholder, then
/// `hew_sched_init` installs a worker-backed runtime), the allocator may hand
/// the freed box's address back for the new runtime (an ABA alias) — so a
/// guard that frees on pointer-equality alone would free a *live* worker-backed
/// runtime out from under its running workers. Checking that the slot's runtime
/// is still worker-less defeats that alias: a reused address now holds a
/// worker-backed scheduler, so the guard correctly declines to free it. A
/// `NoWorkerSchedulerForTest` that swaps the placeholder out and back leaves it
/// worker-less, so the guard still reclaims it (no leak).
///
/// # Safety
///
/// `ptr` must be null or a valid pointer to an installed `RuntimeInner` (the
/// caller holds the scheduler-test lock, so the slot is not concurrently freed).
#[cfg(test)]
pub(crate) unsafe fn runtime_ptr_is_worker_backed(ptr: *mut RuntimeInner) -> bool {
    if ptr.is_null() {
        return false;
    }
    // SAFETY: caller guarantees `ptr` is a live installed runtime.
    let inner = unsafe { &*ptr };
    inner.scheduler.is_worker_backed()
}

/// Install `inner` as the default runtime, returning the previously-installed
/// pointer (null if none). Test-only unconditional swap (no compare-exchange):
/// the caller restores `previous` on teardown.
#[cfg(test)]
pub(crate) fn test_swap_default(inner: *mut RuntimeInner) -> *mut RuntimeInner {
    DEFAULT_RUNTIME.swap(inner, Ordering::SeqCst)
}

/// Store `inner` as the default runtime with release ordering (test-only).
#[cfg(test)]
pub(crate) fn test_store_default(inner: *mut RuntimeInner) {
    DEFAULT_RUNTIME.store(inner, Ordering::Release);
}

/// Free a `RuntimeInner` raw pointer previously boxed for a test guard
/// (test-only). No-op on null.
///
/// # Safety
///
/// `inner` must be a pointer obtained from `Box::into_raw` for a `RuntimeInner`
/// that is no longer installed in the slot and is unreferenced by any thread
/// (the caller — the runtime test guard — holds `SCHED_TEST_MUTEX` and frees on
/// teardown, after any spawned worker has been joined).
#[cfg(test)]
pub(crate) unsafe fn free_runtime_for_test(inner: *mut RuntimeInner) {
    if !inner.is_null() {
        // SAFETY: see fn docs — the caller guarantees `inner` came from
        // `Box::into_raw` and is now unreferenced.
        drop(unsafe { Box::from_raw(inner) });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scheduler::worker_less_scheduler;

    /// Read the raw `CURRENT_RUNTIME` thread-local pointer without resolving
    /// through `rt_current` (which would panic when no default is installed).
    fn current_slot() -> *const RuntimeInner {
        CURRENT_RUNTIME.with(Cell::get)
    }

    /// Force the slot back to null on construction and on drop, so the test is
    /// independent of which pooled thread runs it (mirrors `ContextResetGuard`
    /// in `execution_context.rs`). The slot is private to this module, so the
    /// test rather than a production caller owns the reset.
    struct RuntimeSlotResetGuard;

    impl RuntimeSlotResetGuard {
        fn new() -> Self {
            CURRENT_RUNTIME.with(|c| c.set(std::ptr::null()));
            Self
        }
    }

    impl Drop for RuntimeSlotResetGuard {
        fn drop(&mut self) {
            CURRENT_RUNTIME.with(|c| c.set(std::ptr::null()));
        }
    }

    /// Nested `enter()` saves and restores the previous selection: entering A
    /// then B makes B current; dropping B's guard restores A; dropping A's guard
    /// restores null. This is the `lifecycle-symmetry` contract the dispatch
    /// boundary and worker entry rely on (the shape of
    /// `execution_context::nested_install_restores_previous_context`).
    #[test]
    fn nested_enter_restores_previous_runtime() {
        let _reset = RuntimeSlotResetGuard::new();
        let rt_a = RuntimeInner::new(worker_less_scheduler());
        let rt_b = RuntimeInner::new(worker_less_scheduler());
        let a_ptr = &raw const rt_a;
        let b_ptr = &raw const rt_b;

        assert!(current_slot().is_null(), "slot starts null");

        // SAFETY: `rt_a`/`rt_b` are stack locals that outlive their guards —
        // each guard is dropped before its runtime leaves scope — satisfying
        // `enter`'s lifetime obligation.
        let guard_a = unsafe { enter(&rt_a) };
        assert_eq!(current_slot(), a_ptr, "A is current after entering A");

        {
            // SAFETY: as above; `guard_b` is dropped before `rt_b` leaves scope.
            let guard_b = unsafe { enter(&rt_b) };
            assert_eq!(current_slot(), b_ptr, "B is current after entering B");
            drop(guard_b);
        }
        assert_eq!(current_slot(), a_ptr, "dropping B restores A");

        drop(guard_a);
        assert!(current_slot().is_null(), "dropping A restores null");
    }

    /// An `enter()` guard restores the previous selection even when the scope
    /// unwinds: the guard's `Drop` runs during panic unwinding, so a panic
    /// inside an `enter(B)` scope rebinds A, not leaks B. Without the matched
    /// restore on the panic edge, `rt_current()` on this thread would keep
    /// resolving the unwound runtime (`lifecycle-symmetry`).
    #[test]
    fn enter_restores_previous_runtime_on_panic() {
        let _reset = RuntimeSlotResetGuard::new();
        let rt_a = RuntimeInner::new(worker_less_scheduler());
        let rt_b = RuntimeInner::new(worker_less_scheduler());
        let a_ptr = &raw const rt_a;

        // SAFETY: `rt_a`/`rt_b` are stack locals that outlive their guards —
        // `guard_a` is dropped before `rt_a` leaves scope, and `_guard_b` drops
        // on the panic edge before `rt_b` leaves scope — satisfying `enter`'s
        // lifetime obligation.
        let guard_a = unsafe { enter(&rt_a) };
        assert_eq!(current_slot(), a_ptr, "A is current");

        let unwound = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            // SAFETY: as above; `_guard_b` drops during unwinding, before `rt_b`
            // leaves scope.
            let _guard_b = unsafe { enter(&rt_b) };
            assert_eq!(current_slot(), &raw const rt_b, "B is current");
            panic!("unwind through the enter(B) scope");
        }));
        assert!(unwound.is_err(), "the inner scope panicked");

        // Drop during unwinding must have restored A, not left B installed.
        assert_eq!(current_slot(), a_ptr, "panic-path drop restores A");

        drop(guard_a);
        assert!(current_slot().is_null(), "dropping A restores null");
    }
}
