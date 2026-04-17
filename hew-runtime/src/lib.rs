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

#[cfg(test)]
use std::cell::Cell;
use std::cell::RefCell;
use std::ffi::{c_char, CString};

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

#[cfg(test)]
pub(crate) struct RuntimeTestGuard {
    _lock_guard: Option<std::sync::MutexGuard<'static, ()>>,
}

#[cfg(test)]
thread_local! {
    static RUNTIME_TEST_LOCK_DEPTH: Cell<usize> = const { Cell::new(0) };
}

#[cfg(test)]
impl Drop for RuntimeTestGuard {
    fn drop(&mut self) {
        RUNTIME_TEST_LOCK_DEPTH.with(|depth| {
            let current = depth.get();
            depth.set(current.saturating_sub(1));
        });
    }
}

#[cfg(test)]
pub(crate) fn runtime_test_guard() -> RuntimeTestGuard {
    static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    let lock = LOCK.get_or_init(|| std::sync::Mutex::new(()));

    let held = RUNTIME_TEST_LOCK_DEPTH.with(|depth| {
        let current = depth.get();
        depth.set(current + 1);
        current > 0
    });

    if held {
        RuntimeTestGuard { _lock_guard: None }
    } else {
        // Recover from poisoned lock (a previous test panicked while
        // holding it). This matches the old per-module lock_or_recover()
        // behaviour and prevents a cascade where every subsequent test
        // on this thread skips lock acquisition due to a stuck depth
        // counter.
        let guard = lock
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        RuntimeTestGuard {
            _lock_guard: Some(guard),
        }
    }
}

// Profiler (must be declared before other modules so the global
// allocator is installed before any allocations occur).
// Not available on WASM — profiler requires HTTP server for dashboard.
#[cfg(all(feature = "profiler", not(target_arch = "wasm32")))]
pub mod profiler;

// When the profiler feature is disabled (or on WASM), provide a minimal stub
// so that scheduler.rs can still call profiler::maybe_start() etc.
#[cfg(any(not(feature = "profiler"), target_arch = "wasm32"))]
pub mod profiler {
    //! Profiler stubs when the `profiler` feature is disabled.
    pub mod allocator {
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
pub mod cabi;
pub mod hashmap;
pub mod hashset;
pub mod option;
pub mod print;
pub mod random;
pub mod rc;
pub mod result;
pub mod string;
pub mod vec;
pub mod vecdeque;

pub mod bytes;

pub mod internal;
mod tagged_union;

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
    //!   [`crate::channel_wasm`] (`channel.new`, `send`, `send_int`,
    //!   `try_recv`, `try_recv_int`, clone/close helpers). Blocking
    //!   `hew_channel_recv*` remains an explicit trap until cooperative
    //!   scheduler yield/resume parity exists.

    use std::ffi::{c_char, c_int, c_void};

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
    pub unsafe extern "C" fn hew_sleep_ms(ms: c_int) {
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
        use std::sync::OnceLock;
        use std::time::Instant;

        static EPOCH: OnceLock<Instant> = OnceLock::new();
        let epoch = EPOCH.get_or_init(Instant::now);
        #[expect(
            clippy::cast_possible_truncation,
            reason = "monotonic ms since process start won't exceed u64"
        )]
        {
            epoch.elapsed().as_millis() as u64
        }
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
    pub unsafe extern "C" fn hew_channel_recv(_receiver: *mut c_void) -> *mut c_char {
        unreachable!("hew_channel_recv: not supported on wasm32")
    }

    /// WASM stub: blocking integer channel recv is not supported.
    ///
    /// # Safety
    ///
    /// Never returns — traps unconditionally.
    #[no_mangle]
    pub unsafe extern "C" fn hew_channel_recv_int(
        _receiver: *mut c_void,
        _out_valid: *mut i32,
    ) -> i64 {
        unreachable!("hew_channel_recv_int: not supported on wasm32")
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
pub mod stdio;

#[cfg(any(target_arch = "wasm32", test))]
pub mod bridge;
#[cfg(not(target_arch = "wasm32"))]
pub mod coro;
#[cfg(not(target_arch = "wasm32"))]
pub mod crash;
#[cfg(not(target_arch = "wasm32"))]
pub mod deque;
#[cfg(not(target_arch = "wasm32"))]
pub mod mailbox;
#[cfg(any(target_arch = "wasm32", test))]
pub mod mailbox_wasm;
#[cfg(not(target_arch = "wasm32"))]
pub mod mpsc;
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
#[cfg(all(not(target_arch = "wasm32"), test))]
pub mod arena_wasm;
#[cfg(not(target_arch = "wasm32"))]
pub mod channel;
#[cfg(any(target_arch = "wasm32", test))]
mod channel_wasm;
#[cfg(not(target_arch = "wasm32"))]
pub mod reply_channel;
#[cfg(any(target_arch = "wasm32", test))]
pub mod reply_channel_wasm;
#[cfg(not(target_arch = "wasm32"))]
pub mod scope;
#[cfg(not(target_arch = "wasm32"))]
pub mod semaphore;

#[cfg(not(target_arch = "wasm32"))]
pub mod blocking_pool;
#[cfg(not(target_arch = "wasm32"))]
pub mod task_scope;
#[cfg(not(target_arch = "wasm32"))]
pub mod timer;
#[cfg(not(target_arch = "wasm32"))]
pub mod timer_periodic;
#[cfg(any(target_arch = "wasm32", test))]
pub mod timer_periodic_wasm;
#[cfg(not(target_arch = "wasm32"))]
pub mod timer_wheel;

#[cfg(not(target_arch = "wasm32"))]
pub mod hew_node;
#[cfg(not(target_arch = "wasm32"))]
pub mod supervisor;
#[cfg(not(target_arch = "wasm32"))]
pub mod transport;
pub mod wire;

#[cfg(not(target_arch = "wasm32"))]
pub mod cluster;
#[cfg(not(target_arch = "wasm32"))]
pub mod connection;
#[cfg(not(target_arch = "wasm32"))]
pub mod deterministic;
#[cfg(not(target_arch = "wasm32"))]
pub mod env;
#[cfg(not(target_arch = "wasm32"))]
pub mod generator;
#[cfg(not(target_arch = "wasm32"))]
pub mod link;
#[cfg(not(target_arch = "wasm32"))]
pub mod monitor;
#[cfg(not(target_arch = "wasm32"))]
pub mod pid;
#[cfg(not(target_arch = "wasm32"))]
pub mod pool;
#[cfg(not(target_arch = "wasm32"))]
pub mod process;
pub mod registry;
#[cfg(not(target_arch = "wasm32"))]
pub mod remote_sup;
#[cfg(not(target_arch = "wasm32"))]
pub mod routing;
/// Session-scoped reset-hook registry.  Unconditionally compiled — both the
/// WASM cooperative scheduler and the native work-stealing scheduler share
/// this module.  Hooks are registered per-platform at init time.
pub mod session;
#[cfg(not(target_arch = "wasm32"))]
pub mod stream;
pub mod tracing;

// ── Ecosystem modules (feature-gated) ───────────────────────────────────────

#[cfg(all(feature = "encryption", not(target_arch = "wasm32")))]
pub mod encryption;

#[cfg(all(feature = "quic", not(target_arch = "wasm32")))]
pub mod quic_transport;

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

// ── WASM entry point ─────────────────────────────────────────────────────────
// Provides `_start` for WASI command modules. The compiler renames the user's
// `main()` to `__original_main` when targeting WASM.

#[cfg(all(target_arch = "wasm32", not(test)))]
extern "C" {
    fn __original_main() -> i32;
}

/// WASI entry point — delegates to the compiler-generated `__original_main`.
#[cfg(all(target_arch = "wasm32", not(test)))]
#[no_mangle]
pub extern "C" fn _start() {
    // SAFETY: `__original_main` is always emitted by hew-codegen for every
    // Hew program and has the signature `() -> i32`.
    let code = unsafe { __original_main() };
    if code != 0 {
        std::process::exit(code);
    }
}
// test
