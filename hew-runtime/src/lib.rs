//! Pure Rust runtime for the Hew actor language.
//!
//! This crate provides the core runtime support library (`libhew_rt`) for
//! compiled Hew programs. All public functions use `#[no_mangle] extern "C"`
//! to maintain ABI compatibility with the LLVM-compiled binaries.
//!
//! Most ecosystem modules (encoding, crypto, net, time, text, db) have been
//! extracted into standalone packages under `std/` and `ecosystem/`. This
//! crate now contains only the core actor runtime, wire protocol, and a few
//! optional features.
//!
//! # Architecture
//!
//! ```text
//! Layer 0: print, string, vec, hashmap, io_time (no internal deps)
//! Layer 1: mpsc, deque (atomic primitives)
//! Layer 2: mailbox, scheduler (L0+L1)
//! Layer 3: actor, scope, actor_group (L2)
//! Layer 4: task_scope, timer_wheel, blocking_pool (L3)
//! Layer 5: wire, transport, node, supervisor (L3+wire)
//! Layer 6: encryption (snow)
//! ```
//!
//! # Cargo Features
//!
//! - `full` (default) — encryption + profiler
//! - `encryption` — Noise protocol encryption via `snow`
//! - `profiler` — built-in profiler dashboard and pprof export
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

#[cfg(test)]
use std::cell::Cell;
use std::cell::RefCell;
use std::ffi::{c_char, CString};
// live on not(wasm32) — hew_wasm_register_actor_meta stub; dead here; caller lib.rs:84
#[cfg(not(target_arch = "wasm32"))]
use std::ffi::c_void;
use std::io::Write;

thread_local! {
    static LAST_ERROR: RefCell<Option<CString>> = const { RefCell::new(None) };
}

/// Set the last error message for the current thread.
pub(crate) fn set_last_error(msg: impl Into<String>) {
    LAST_ERROR.with(|e| {
        *e.borrow_mut() =
            Some(CString::new(msg.into()).unwrap_or_else(|_| {
                CString::new("(error message contained embedded NUL)").unwrap()
            }));
    });
}

/// Get a pointer to the last error message. Returns null if no error.
/// The pointer is valid until the next error is set on this thread.
#[no_mangle]
pub extern "C" fn hew_last_error() -> *const c_char {
    LAST_ERROR.with(|e| match e.borrow().as_ref() {
        Some(s) => s.as_ptr(),
        None => std::ptr::null(),
    })
}

/// Clear the last error.
#[no_mangle]
pub extern "C" fn hew_clear_error() {
    LAST_ERROR.with(|e| *e.borrow_mut() = None);
}

/// Native no-op for the target-neutral actor metadata registration call.
///
/// The v0.5 LLVM emitter builds one textual module before object emission, so
/// actor spawn IR can contain the WASM host metadata registration call even
/// when the same module is compiled to a native object. Native hosts do not
/// query WASM actor metadata; they only need this symbol to link cleanly.
/// NATIVE-TODO(#1259): replace this stub with real native metadata
/// registration when native metadata consumers exist.
///
/// # Safety
///
/// The pointer is intentionally ignored on native targets.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_wasm_register_actor_meta(_meta: *const c_void) {}

/// Terminate the current process with a Hew integer exit code.
#[no_mangle]
pub extern "C" fn hew_exit(code: i64) {
    hew_exit_impl(code, hew_exit_process_terminate);
}

fn hew_exit_impl(code: i64, terminate: impl FnOnce(i32)) {
    let Ok(code) = i32::try_from(code) else {
        eprintln!("hew_exit: exit code {code} is outside the supported i32 range");
        std::process::abort();
    };

    if let Err(error) = std::io::stdout().flush() {
        eprintln!("hew_exit: failed to flush stdout before exit: {error}");
        std::process::abort();
    }
    if let Err(error) = std::io::stderr().flush() {
        eprintln!("hew_exit: failed to flush stderr before exit: {error}");
        std::process::abort();
    }

    #[cfg(target_arch = "wasm32")]
    crate::scheduler_wasm::hew_wasm_runtime_exit();

    terminate(code);
}

fn hew_exit_process_terminate(status: i32) {
    #[cfg(test)]
    if let Some(terminate) = HEW_EXIT_TEST_TERMINATOR.with(Cell::get) {
        terminate(status);
        return;
    }

    std::process::exit(status);
}

#[cfg(test)]
thread_local! {
    static HEW_EXIT_TEST_TERMINATOR: Cell<Option<fn(i32)>> = const { Cell::new(None) };
}

#[cfg(test)]
#[cfg_attr(
    not(target_arch = "wasm32"),
    expect(dead_code, reason = "wasm32-only hew_exit regression test hook")
)]
pub(crate) fn with_hew_exit_terminator_for_test<R>(terminate: fn(i32), f: impl FnOnce() -> R) -> R {
    struct Reset(Option<fn(i32)>);

    impl Drop for Reset {
        fn drop(&mut self) {
            HEW_EXIT_TEST_TERMINATOR.with(|slot| slot.set(self.0));
        }
    }

    let previous = HEW_EXIT_TEST_TERMINATOR.with(|slot| slot.replace(Some(terminate)));
    let _reset = Reset(previous);
    f()
}

macro_rules! cabi_guard {
    ($cond:expr) => {
        if $cond {
            $crate::set_last_error(&format!("C-ABI guard failed: {}", stringify!($cond)));
            return;
        }
    };
    ($cond:expr, $ret:expr) => {
        if $cond {
            $crate::set_last_error(&format!("C-ABI guard failed: {}", stringify!($cond)));
            return $ret;
        }
    };
}

pub(crate) mod lifetime;
pub(crate) mod util;

/// CBOR wire envelope types — the Rust-native representation of the Hew wire
/// protocol plus the fail-closed encode/decode helpers used by runtime
/// transport paths.
pub mod envelope;

/// Cross-node actor-message payload serialization codec: the runtime half of
/// the codegen-driven value↔wire encoder that lets `Serializable` payloads
/// survive a process hop without shipping in-memory heap pointers.
pub mod xnode_serial;

/// CBOR wire-body codec: the runtime primitives the compiler's
/// `__hew_cbor_*` thunks drive to turn a `#[wire]` value into CBOR bytes and back.
/// Reuses the envelope's `ciborium` dependency; the bytes ride the envelope's
/// CBOR `bstr` payload slot unchanged.
pub mod cbor_serial;

/// Text wire-body codec: the CBOR↔JSON/YAML bridge the compiler's
/// `__hew_wire_to_json_*` / `__hew_wire_from_json_*` (and yaml) thunks drive.
/// Reuses the binary CBOR walk above and transcodes its value tree to/from text
/// via a per-type tag↔name descriptor — no parallel per-format struct/enum walk.
pub mod wire_text;

/// Test-only RAII guard that serializes runtime-touching tests AND installs a
/// default `RuntimeInner` so the de-globalized authority resolvers
/// (`crate::runtime::rt_current`) have a runtime to read.
///
/// Under the explicit-install model (Q119/A119) there is no lazy auto-create
/// and no silent `None` fallback: any test that exercises a runtime authority
/// (the live-actor registry, name registry, shutdown phase, timers, node slot)
/// must have a runtime installed. This guard is that install point for the ~100
/// shutdown/registry/actor test sites — acquiring it installs a worker-less
/// default runtime at the outermost (depth-0) acquisition and tears it down
/// when the outermost guard drops.
///
/// It shares `crate::scheduler::SCHED_TEST_MUTEX` with `NoWorkerSchedulerForTest`
/// so every runtime-installing test is mutually serialized on the single
/// process-global default-runtime slot. A test that nests this guard and then a
/// `NoWorkerSchedulerForTest` composes correctly: the inner guard swaps its
/// transient runtime in (saving this guard's as `previous`) and restores it on
/// drop, before this guard removes its own on the outermost drop.
#[cfg(all(test, not(target_arch = "wasm32")))]
pub(crate) struct RuntimeTestGuard {
    /// Re-entrant scheduler-test lock, shared with `NoWorkerSchedulerForTest`
    /// so every runtime-installing test serializes on the single default-runtime
    /// slot. The outermost guard on a thread holds the real lock.
    _lock_guard: crate::scheduler::SchedTestLock,
    /// The default `RuntimeInner` this guard installed (only the outermost guard
    /// installs one); reclaimed on drop. Null for re-entrant guards or when a
    /// runtime was already installed. Always a **worker-less** placeholder.
    installed_runtime: *mut crate::runtime::RuntimeInner,
}

#[cfg(all(test, not(target_arch = "wasm32")))]
impl Drop for RuntimeTestGuard {
    fn drop(&mut self) {
        if !self.installed_runtime.is_null() {
            // Reclaim our installed worker-less placeholder — but only if the
            // slot still holds exactly that pointer AND that runtime is still
            // worker-less. A test may legitimately have torn the runtime down
            // itself (e.g. `hew_runtime_cleanup`, which `take_default()`s and
            // drops it) or replaced it (`init_real_scheduler_for_test`, which
            // frees our placeholder and installs a worker-backed runtime —
            // possibly at the SAME address the allocator just freed). The
            // worker-backed check defeats that ABA alias: a reused address now
            // holds a worker-backed scheduler, so we must NOT free it — doing so
            // would be a use-after-free against its running workers. A
            // `NoWorkerSchedulerForTest` that swapped our placeholder out and
            // back leaves it worker-less, so we still reclaim it (no leak). We
            // hold the lock, so no *other* test races this check.
            let slot = crate::runtime::default_runtime_ptr(std::sync::atomic::Ordering::SeqCst);
            // SAFETY: under the scheduler-test lock the slot is not concurrently
            // freed; `runtime_ptr_is_worker_backed` reads it only if non-null.
            let slot_is_worker_backed =
                unsafe { crate::runtime::runtime_ptr_is_worker_backed(slot) };
            if slot == self.installed_runtime && !slot_is_worker_backed {
                // Join any background teardown threads this test left in flight
                // (a deferred supervisor-stop or restart-free registered via
                // `push_deferred_teardown_thread`) BEFORE detaching and freeing
                // the runtime. Those threads dereference the runtime-owned
                // live-actor registry; freeing the runtime out from under a
                // still-running one would trap it in `rt_current()`. This
                // mirrors production `hew_runtime_cleanup`'s join-before-free
                // ordering, with the runtime still installed for the join.
                #[cfg(not(target_arch = "wasm32"))]
                crate::lifetime::live_actors::drain_deferred_teardown_threads();
                crate::runtime::test_store_default(std::ptr::null_mut());
                // SAFETY: `installed_runtime` came from `worker_less_runtime_box`
                // (Box::into_raw), is still the slot's pointer (just detached),
                // and is unreferenced (the lock serializes installers; the drain
                // above joined every background teardown that held a borrow).
                unsafe { crate::runtime::free_runtime_for_test(self.installed_runtime) };
            }
        }
    }
}

#[cfg(all(test, not(target_arch = "wasm32")))]
pub(crate) fn runtime_test_guard() -> RuntimeTestGuard {
    let outermost = !crate::scheduler::SchedTestLock::is_held();
    let lock_guard = crate::scheduler::SchedTestLock::acquire();

    // Only the outermost guard installs a runtime, and only if the slot is empty
    // (a nested NoWorkerSchedulerForTest or a self-managing lifecycle test may
    // own the slot). We reclaim on drop only what we install.
    let installed_runtime = if outermost && crate::runtime::rt_default().is_none() {
        let rt = crate::scheduler::worker_less_runtime_box();
        crate::runtime::test_store_default(rt);
        rt
    } else {
        std::ptr::null_mut()
    };

    RuntimeTestGuard {
        _lock_guard: lock_guard,
        installed_runtime,
    }
}

/// WASM test-only RAII guard: serializes `scheduler_wasm` and `bridge` tests
/// that call `crate::runtime_test_guard()`.
///
/// On `wasm32` there is no process-global `RuntimeInner` to install — the
/// cooperative scheduler manages its own state. This stub holds a
/// process-global `Mutex` to give test-thread isolation equivalent to the
/// native `SchedTestLock`, without referencing any `cfg(not(wasm32))` module.
#[cfg(all(test, target_arch = "wasm32"))]
pub(crate) struct RuntimeTestGuard {
    _lock: std::sync::MutexGuard<'static, ()>,
}

#[cfg(all(test, target_arch = "wasm32"))]
pub(crate) fn runtime_test_guard() -> RuntimeTestGuard {
    use std::sync::Mutex;
    static WASM_TEST_LOCK: Mutex<()> = Mutex::new(());
    // Unwrap: a poisoned lock in tests is a prior panic; let the test fail.
    RuntimeTestGuard {
        _lock: WASM_TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner()),
    }
}

// Profiler (must be declared before other modules so the global
// allocator is installed before any allocations occur).
// Not available on WASM — profiler requires HTTP server for dashboard.
#[cfg(all(feature = "profiler", not(target_arch = "wasm32")))]
pub mod profiler;

// WASM-TODO(#1451): pprof/sampler require OS threads + HTTP; no WASM path planned until WASI threads land
// When the profiler feature is disabled (or on WASM), provide a minimal stub
// so that scheduler.rs can still call profiler::maybe_start() etc.
#[cfg(any(not(feature = "profiler"), target_arch = "wasm32"))]
pub mod profiler {
    //! Profiler stubs when the `profiler` feature is disabled.
    pub mod actor_registry {
        /// Stub actor snapshots when profiler registry is unavailable.
        #[must_use]
        pub fn snapshot_all() -> Vec<()> {
            Vec::new()
        }

        /// Stub live-state count when profiler registry is unavailable.
        #[must_use]
        pub fn state_count(_state: i32) -> u64 {
            0
        }

        /// Stub runnable-coroutine count when profiler registry is unavailable.
        #[must_use]
        pub fn runnable_coroutine_count() -> u64 {
            0
        }

        /// Stub dispatch-type lookup when profiler registry is unavailable.
        ///
        /// Always returns `"Actor"` — the same default the real registry returns
        /// for unregistered pointers.
        #[must_use]
        pub fn lookup_dispatch_type_by_ptr(_dispatch_ptr: usize) -> &'static str {
            "Actor"
        }

        /// Stub owned dispatch-type lookup when profiler registry is unavailable.
        ///
        /// Always returns `"Actor".to_owned()` — the same default the real
        /// registry returns for unregistered pointers. Mirrors
        /// `lookup_dispatch_type_by_ptr_owned`, which signal/tracing callers use
        /// to copy the name under the registry lock; the stub has no registry, so
        /// there is no lock or race to mirror — only the default value.
        #[must_use]
        pub fn lookup_dispatch_type_by_ptr_owned(_dispatch_ptr: usize) -> String {
            "Actor".to_owned()
        }

        /// Stub handler-name lookup when profiler registry is unavailable.
        ///
        /// Always returns `None` — the same default the real registry returns
        /// for unregistered (`dispatch_fn`, `msg_type`) pairs.
        #[must_use]
        pub fn handler_name_by_ptr(_dispatch_ptr: usize, _msg_type: i32) -> Option<String> {
            None
        }
    }

    pub mod allocator {
        /// Zeroed allocator statistics when profiler counters are unavailable.
        #[derive(Debug, Clone, Copy)]
        pub struct AllocStats {
            /// Total allocation calls.
            pub alloc_count: u64,
            /// Total deallocation calls.
            pub dealloc_count: u64,
            /// Cumulative bytes allocated.
            pub bytes_allocated: u64,
            /// Cumulative bytes freed.
            pub bytes_freed: u64,
            /// Approximate bytes currently live.
            pub bytes_live: u64,
            /// Peak bytes live.
            pub peak_bytes_live: u64,
        }

        /// Capture zeroed allocator stats when profiler is disabled.
        #[must_use]
        pub fn snapshot() -> AllocStats {
            AllocStats {
                alloc_count: 0,
                dealloc_count: 0,
                bytes_allocated: 0,
                bytes_freed: 0,
                bytes_live: 0,
                peak_bytes_live: 0,
            }
        }

        /// No-op allocator pass-through when profiler is disabled.
        #[derive(Debug)]
        pub struct ProfilingAllocator;

        // SAFETY: ProfilingAllocator delegates directly to the system allocator
        // with no additional state or side effects.
        unsafe impl std::alloc::GlobalAlloc for ProfilingAllocator {
            unsafe fn alloc(&self, layout: std::alloc::Layout) -> *mut u8 {
                // SAFETY: Caller guarantees layout is valid (non-zero size,
                // power-of-two alignment). We forward directly to the system
                // allocator which upholds the same contract.
                unsafe { std::alloc::System.alloc(layout) }
            }
            unsafe fn dealloc(&self, ptr: *mut u8, layout: std::alloc::Layout) {
                // SAFETY: Caller guarantees ptr was allocated by this allocator
                // with the same layout. We forward to System which performed the
                // original allocation.
                unsafe { std::alloc::System.dealloc(ptr, layout) }
            }
        }
    }
    /// No-op: profiler feature is disabled.
    pub fn maybe_start() {}
    /// No-op: profiler feature is disabled.
    ///
    /// On non-wasm targets the parameters match the real function signature so
    /// call sites in `hew_node.rs` compile without casts.  On wasm32 the typed
    /// modules are absent, so raw pointers are used instead.
    #[cfg(not(target_arch = "wasm32"))]
    pub fn maybe_start_with_context(
        _cluster: *mut crate::cluster::HewCluster,
        _connmgr: *mut crate::connection::HewConnMgr,
        _routing: *mut crate::routing::HewRoutingTable,
    ) {
    }
    #[cfg(target_arch = "wasm32")]
    pub fn maybe_start_with_context(
        _cluster: *mut std::ffi::c_void,
        _connmgr: *mut std::ffi::c_void,
        _routing: *mut std::ffi::c_void,
    ) {
    }
    /// No-op: profiler feature is disabled.
    pub fn maybe_write_on_exit() {}
    /// No-op: profiler feature is disabled.
    pub fn shutdown() {}
}

// Global allocator — only on native targets. On WASM, the default Rust
// allocator is used (via wasi-libc's malloc).
#[cfg(not(target_arch = "wasm32"))]
#[global_allocator]
static GLOBAL: profiler::allocator::ProfilingAllocator = profiler::allocator::ProfilingAllocator;

// ── Core modules (always compiled) ──────────────────────────────────────────

pub mod arc;
pub mod assert;
pub mod auto_mutex;
pub use auto_mutex::{
    hew_auto_mutex_alloc, hew_auto_mutex_free, hew_auto_mutex_lock, hew_auto_mutex_unlock,
    HewAutoMutex,
};
pub mod cabi;
/// Stackless continuation substrate: `HewCont` heap-frame + C ABI (W6.007).
/// The runtime side of the unified suspension representation — the coro frame
/// allocator and the resume/done/poll/destroy verbs the poll/resume executor
/// drives. Target-agnostic (routes through `mem`); no wasm cfg gate.
pub mod cont;
/// Slice-4 poll/resume executor guards: the per-continuation lifecycle tag
/// transitions (FG1/FG2/FG4) and the two-phase park (FG3) that serialize
/// resume vs destroy on a parked `HewCont`. Target-agnostic; drives the
/// `cont` ABI, holds no scheduler queue state.
pub mod coro_exec;
pub mod hashmap;
pub mod hashset;
pub mod layout_intrinsics;
pub mod mem;
pub mod print;
pub mod random;
pub mod rc;
pub mod string;
pub mod trait_object;
pub mod vec;
pub mod vecdeque;

pub mod bytes;
mod channel_common;
pub mod duration;
pub mod machine_emit;
/// Process-wide monotonic clock epoch shared by every subsystem that reports
/// "duration since process start" (timers, supervisor, crash, tracing, SWIM).
pub mod monotonic;
pub use machine_emit::{
    hew_machine_emit_push, hew_machine_emit_step_enter, hew_machine_emit_step_exit,
    DrainError as MachineEmitDrainError, EmitEvent, EmitQueue, EmitQueueAppend,
    MachineEmitReentrancyExceeded,
};
pub mod parse_error_slot;

pub mod internal;
mod send_ptr;
mod trap_code;
// On WASM, provide shims for runtime functions used by codegen but not
// applicable to WASM (no threads, no native timer support).
// Arena functions (hew_arena_malloc / hew_arena_free / etc.) are now
// provided by the wasm32 arena module (pub mod arena below) instead of
// these stubs.
#[cfg(target_arch = "wasm32")]
pub mod wasm_stubs {
    //! Fail-closed shims for runtime functions that are not implementable on
    //! the wasm32 cooperative scheduler.
    //!
    //! ## Design rules
    //!
    //! - **Sleep**: records the wakeup deadline via
    //!   [`crate::scheduler_wasm::request_sleep`] and returns immediately.
    //!   The cooperative scheduler parks the actor at the message boundary and
    //!   re-enqueues it once the deadline passes (host calls
    //!   [`crate::scheduler_wasm::hew_wasm_timer_tick`] or the implicit drain
    //!   inside [`crate::scheduler_wasm::hew_wasm_sched_tick`]).
    //!
    //! - **Clock**: `hew_now_ms` mirrors the native runtime's monotonic
    //!   process-relative clock so timeout comparisons stay consistent on
    //!   `wasm32-wasip1`.
    //!
    //! - **Channels**: the bounded non-blocking slice is implemented in
    //!   [`crate::channel_wasm`] (`channel.new`, the layout-witness
    //!   `hew_channel_send_layout` / `hew_channel_try_recv_layout` entries,
    //!   clone/close helpers). Blocking `hew_channel_recv_layout` remains an
    //!   explicit trap until cooperative scheduler yield/resume parity exists.

    use std::ffi::c_void;

    // ── Sleep ────────────────────────────────────────────────────────────────

    /// WASM sleep: park the current actor until `ms` milliseconds have elapsed.
    ///
    /// Records the wakeup deadline and returns immediately.  After the current
    /// message dispatch completes, [`crate::scheduler_wasm`] parks the actor in
    /// the sleep queue.  The host re-enqueues it once the deadline passes by
    /// calling [`crate::scheduler_wasm::hew_wasm_timer_tick`] with the current
    /// time, or via the implicit drain inside [`crate::scheduler_wasm::hew_wasm_sched_tick`].
    ///
    /// This replaces the former intentional no-op.  The semantics are
    /// cooperative: `sleep_ms` takes effect at the *message boundary* — code
    /// running after `sleep_ms` in the same handler still executes before the
    /// park happens.
    ///
    /// # Safety
    ///
    /// No preconditions — may be called from any context.
    #[no_mangle]
    pub unsafe extern "C" fn hew_sleep_ms(ms: i64) {
        if ms <= 0 {
            return;
        }
        // Compute the absolute deadline from the current clock.
        // SAFETY: hew_now_ms has no preconditions.
        let now = unsafe { hew_now_ms() };
        #[expect(clippy::cast_sign_loss, reason = "guarded by ms > 0")]
        let deadline_ms = now.saturating_add(ms as u64);
        crate::scheduler_wasm::request_sleep(deadline_ms);
    }

    /// WASM sleep (nanosecond ABI): park until `ns` nanoseconds have elapsed.
    ///
    /// Converts to milliseconds and delegates to the ms-granularity sleep queue.
    ///
    /// # Safety
    ///
    /// No preconditions.
    #[no_mangle]
    pub unsafe extern "C" fn hew_sleep_ns(ns: i64) {
        if ns <= 0 {
            return;
        }
        let ms = (ns / 1_000_000).max(1);
        // SAFETY: hew_now_ms has no preconditions.
        let now = unsafe { hew_now_ms() };
        #[expect(clippy::cast_sign_loss, reason = "guarded by ms > 0")]
        let deadline_ms = now.saturating_add(ms as u64);
        crate::scheduler_wasm::request_sleep(deadline_ms);
    }

    /// WASM sleep_until: park until the given nanosecond `instant`.
    ///
    /// Computes remaining ns from `now`, converts to ms, and parks.
    ///
    /// # Safety
    ///
    /// No preconditions.
    #[no_mangle]
    pub unsafe extern "C" fn hew_sleep_until_ns(instant_ns: i64) {
        // SAFETY: hew_now_ms has no preconditions.
        let now_ms = unsafe { hew_now_ms() };
        let now_ns = (now_ms as i64).saturating_mul(1_000_000);
        let remaining_ns = instant_ns.saturating_sub(now_ns);
        if remaining_ns <= 0 {
            return;
        }
        let delay_ms = (remaining_ns / 1_000_000).max(1);
        #[expect(clippy::cast_sign_loss, reason = "guarded by delay_ms > 0")]
        let deadline_ms = now_ms.saturating_add(delay_ms as u64);
        crate::scheduler_wasm::request_sleep(deadline_ms);
    }

    // ── Clock ────────────────────────────────────────────────────────────────

    /// WASM shim: monotonic clock in milliseconds.
    ///
    /// Returns monotonic milliseconds since the first call. The `deterministic`
    /// simulation-time module is unavailable on `wasm32`; simulated time is not
    /// supported in the WASM cooperative scheduler.
    ///
    /// # Safety
    ///
    /// No preconditions.
    #[no_mangle]
    pub unsafe extern "C" fn hew_now_ms() -> u64 {
        crate::monotonic::monotonic_ms()
    }

    // ── Channels (blocking recv still deferred on wasm32) ────────────────────
    //
    // The non-blocking channel ABI surface lives in `channel_wasm.rs`.
    // Blocking recv still needs cooperative yield/resume when the queue is
    // empty but live senders remain, so those entry points keep trapping.

    /// WASM stub: blocking channel recv is not supported.
    ///
    /// # Safety
    ///
    /// Never returns — traps unconditionally.
    #[no_mangle]
    pub unsafe extern "C" fn hew_channel_recv_layout(
        _receiver: *mut c_void,
        _out: *mut c_void,
        _layout: *const c_void,
    ) -> i32 {
        unreachable!("hew_channel_recv_layout: not supported on wasm32")
    }
}

// ── Actor/scheduling modules ─────────────────────────────────────────────────
// Native modules require threads, signals, and networking. WASM modules provide
// cooperative single-threaded alternatives (mailbox_wasm, scheduler_wasm, bridge).

#[cfg(not(target_arch = "wasm32"))]
pub mod file_io;
#[cfg(not(target_arch = "wasm32"))]
pub mod io_time;
#[cfg(not(target_arch = "wasm32"))]
pub mod iter;
#[cfg(not(target_arch = "wasm32"))]
pub mod path;
#[cfg(not(target_arch = "wasm32"))]
pub mod stdio;

#[cfg(any(target_arch = "wasm32", test))]
pub mod bridge;
#[cfg(not(target_arch = "wasm32"))]
pub mod crash;
#[cfg(not(target_arch = "wasm32"))]
pub mod deque;
#[cfg(not(target_arch = "wasm32"))]
pub mod mailbox;
/// Mailbox envelope payload classification and cross-node send guards.
pub mod mailbox_envelope;
#[cfg(any(target_arch = "wasm32", test))]
pub mod mailbox_wasm;
#[cfg(not(target_arch = "wasm32"))]
pub mod mpsc;
/// Runtime-instance state — the de-globalized home for process-wide
/// authorities that previously lived as free `static`s (native-only; the WASM
/// cooperative scheduler keeps its own state model).
#[cfg(not(target_arch = "wasm32"))]
pub mod runtime;
/// Public host/embedder handle to a Hew runtime — the external, refcounted
/// owner token for the runtime lifecycle (native-only; no wasm host ABI).
#[cfg(not(target_arch = "wasm32"))]
pub mod runtime_handle;
/// Process-wide runtime-instance identity (`RuntimeId`). Compiled on every
/// target so `HewActor` and its wasm mirror can stamp the same type, even
/// though the native-only `runtime` module below is configured out on wasm.
pub mod runtime_id;
#[cfg(not(target_arch = "wasm32"))]
pub mod scheduler;
#[cfg(any(target_arch = "wasm32", test))]
pub mod scheduler_wasm;
#[cfg(not(target_arch = "wasm32"))]
pub mod shutdown;
#[cfg(not(target_arch = "wasm32"))]
pub mod signal;

pub mod actor;
#[cfg(not(target_arch = "wasm32"))]
pub mod actor_group;
#[cfg(not(target_arch = "wasm32"))]
pub mod arena;
#[cfg(target_arch = "wasm32")]
#[path = "arena_wasm.rs"]
pub mod arena;
// Expose arena_wasm as a distinct module in native test builds so its unit
// tests run under CI.  The #[cfg_attr(target_arch = "wasm32", no_mangle)]
// guard in arena_wasm.rs prevents duplicate symbol collisions with arena.rs.
pub(crate) mod alloc_tracker;
#[cfg(all(not(target_arch = "wasm32"), test))]
pub mod arena_wasm;
#[cfg(not(target_arch = "wasm32"))]
pub mod channel;
#[cfg(any(target_arch = "wasm32", test))]
mod channel_wasm;
#[cfg(not(target_arch = "wasm32"))]
pub mod duplex;
pub mod execution_context;
#[cfg(not(target_arch = "wasm32"))]
pub mod lambda_actor;
#[cfg(not(target_arch = "wasm32"))]
pub mod read_slot;
#[cfg(not(target_arch = "wasm32"))]
pub mod reply_channel;
#[cfg(any(target_arch = "wasm32", test))]
pub mod reply_channel_wasm;
#[cfg(not(target_arch = "wasm32"))]
pub mod semaphore;

#[cfg(not(target_arch = "wasm32"))]
pub mod await_cancel;
#[cfg(not(target_arch = "wasm32"))]
pub mod blocking_pool;
#[cfg(not(target_arch = "wasm32"))]
pub mod task_scope;
#[cfg(not(target_arch = "wasm32"))]
pub mod timer_periodic;
#[cfg(any(target_arch = "wasm32", test))]
pub mod timer_periodic_wasm;
// timer_wheel compiles on every target: native uses it with the background
// ticker thread (timer_periodic); WASM uses it with a host-driven tick
// (scheduler_wasm + timer_periodic_wasm).
pub mod timer_wheel;

#[cfg(all(test, not(target_arch = "wasm32")))]
mod wasm_parity_tests;

#[cfg(not(target_arch = "wasm32"))]
pub mod hew_node;
#[cfg(not(target_arch = "wasm32"))]
pub mod supervisor;
#[cfg(not(target_arch = "wasm32"))]
pub mod transport;
// Deterministic in-process SimTransport — gated behind `cfg(test)` for unit
// tests inside the crate and the `sim-transport` feature for integration
// tests. Never compiled into a release runtime; the module's own attribute
// gate enforces this and excludes wasm32 per HEW-DIST-SPEC §15.
#[cfg(all(not(target_arch = "wasm32"), any(test, feature = "sim-transport")))]
pub mod sim_transport;

#[cfg(not(target_arch = "wasm32"))]
pub mod channel_core;
#[cfg(not(target_arch = "wasm32"))]
pub mod cluster;
#[cfg(not(target_arch = "wasm32"))]
pub mod connection;
#[cfg(not(target_arch = "wasm32"))]
pub mod deterministic;
#[cfg(not(target_arch = "wasm32"))]
pub mod env;
#[cfg(not(target_arch = "wasm32"))]
pub mod link;
pub mod metrics;
#[cfg(not(target_arch = "wasm32"))]
pub mod monitor;
pub mod observe;
#[cfg(not(target_arch = "wasm32"))]
pub mod phi_accrual;
#[cfg(not(target_arch = "wasm32"))]
pub mod pid;
#[cfg(not(target_arch = "wasm32"))]
pub mod pool;
#[cfg(not(target_arch = "wasm32"))]
pub mod process;
/// Active-mode network I/O reactor ("I/O completion as a mailbox message").
/// Native (non-WASM) on all targets: epoll on Linux, kqueue on macOS/FreeBSD,
/// and an IOCP/AFD_POLL readiness backend on Windows
/// ([`crate::io_time::HewIoPoller`]). The shared engine is platform-independent
/// Rust over the poller's `c_int` token; only the per-platform readiness source
/// differs. WASM fails closed via the type checker's
/// `WasmUnsupportedFeature::TcpNetworking` gate (the reactor is not compiled).
#[cfg(not(target_arch = "wasm32"))]
pub mod reactor;
pub mod registry;
#[cfg(not(target_arch = "wasm32"))]
pub mod routing;
/// Session-scoped reset-hook registry.  Unconditionally compiled — both the
/// WASM cooperative scheduler and the native work-stealing scheduler share
/// this module.  Hooks are registered per-platform at init time.
pub mod session;
#[cfg(not(target_arch = "wasm32"))]
pub mod stream;
/// Single-owner stream/sink error channel. Ungated: it owns the `hew_stream_*`
/// C ABI for the whole linked image (see the module docs for why this must be
/// the only definition), so it is compiled on every target that links libhew.
pub mod stream_error;
#[cfg(not(target_arch = "wasm32"))]
pub mod swim_driver;
pub mod tracing;

// ── Ecosystem modules (feature-gated) ───────────────────────────────────────

#[cfg(all(feature = "encryption", not(target_arch = "wasm32")))]
pub mod encryption;

#[cfg(all(feature = "quic", not(target_arch = "wasm32")))]
pub mod quic_transport;

#[cfg(all(feature = "quic", not(target_arch = "wasm32")))]
pub mod quic_mesh;

// OTel exporter: background thread + OTLP/HTTP, activated by HEW_OTEL_ENDPOINT.
// Not available on WASM (no OS threads for the background exporter).
#[cfg(all(feature = "otel", not(target_arch = "wasm32")))]
pub mod otel;

// When the otel feature is disabled, provide a zero-cost stub so scheduler.rs
// can call otel::maybe_start() unconditionally.
#[cfg(any(not(feature = "otel"), target_arch = "wasm32"))]
pub mod otel {
    //! `OTel` exporter stubs when the `otel` feature is disabled.
    /// No-op: otel feature is disabled.
    pub fn maybe_start() {}
}

pub mod log_core;

pub use execution_context::{
    current_context, set_current_context, HewExecutionContext, HEW_CTX_FLAG_REPLY_CHANNEL_CONSUMED,
    HEW_CTX_OFFSET_ACTOR, HEW_CTX_OFFSET_ACTOR_ID, HEW_CTX_OFFSET_ARENA,
    HEW_CTX_OFFSET_CANCEL_TOKEN, HEW_CTX_OFFSET_FLAGS, HEW_CTX_OFFSET_LOCK_SEAT,
    HEW_CTX_OFFSET_PARENT_SUPERVISOR, HEW_CTX_OFFSET_PARTITION_POLICY, HEW_CTX_OFFSET_PREV_CONTEXT,
    HEW_CTX_OFFSET_REPLY_CHANNEL, HEW_CTX_OFFSET_SUPERVISOR_CHILD_INDEX, HEW_CTX_OFFSET_TASK_SCOPE,
    HEW_CTX_OFFSET_TRACE, HEW_CTX_OFFSET_TRACE_SPAN,
};

// ── WASM entry point ─────────────────────────────────────────────────────────
// Provides `_start` for WASI command modules. The compiler keeps the
// freestanding export as `main` and also defines `__original_main` for the
// WASI runtime entry point.

#[cfg(all(target_arch = "wasm32", not(test)))]
extern "C" {
    fn __original_main() -> i32;
}

/// WASI entry point — delegates to the compiler-generated `__original_main`.
#[cfg(all(target_arch = "wasm32", not(test)))]
#[no_mangle]
pub extern "C" fn _start() {
    // SAFETY: `__original_main` is emitted by hew-codegen for every WASI-linked
    // Hew program and has the signature `() -> i32`.
    let code = unsafe { __original_main() };
    if code != 0 {
        std::process::exit(code);
    }
}
// test
