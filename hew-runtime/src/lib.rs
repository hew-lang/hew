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
//! - `export-meta` — emit `__hew_export_meta_*` companion functions

macro_rules! cabi_guard {
    ($cond:expr) => {
        if $cond {
            return;
        }
    };
    ($cond:expr, $default:expr) => {
        if $cond {
            return $default;
        }
    };
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
    pub fn maybe_write_on_exit() {}
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
pub mod option;
pub mod print;
pub mod rc;
pub mod result;
pub mod string;
pub mod vec;

pub mod internal;
mod tagged_union;

// On WASM, provide a minimal arena stub — no per-actor scoping, just malloc.
#[cfg(target_arch = "wasm32")]
pub mod wasm_stubs {
    //! Minimal stubs for runtime functions used by codegen but not applicable
    //! to WASM (no actors, no arena scoping, no threads).
    use std::os::raw::c_void;

    /// WASM stub: allocate via libc malloc (no arena scoping on WASM).
    ///
    /// # Safety
    ///
    /// Called from compiled Hew programs via C ABI.
    #[no_mangle]
    pub unsafe extern "C" fn hew_arena_malloc(size: usize) -> *mut c_void {
        // SAFETY: size is a valid allocation size from codegen.
        unsafe { libc::malloc(size) }
    }

    /// WASM stub: free via libc free.
    ///
    /// # Safety
    ///
    /// `ptr` must have been allocated by `hew_arena_malloc` or be null.
    #[no_mangle]
    pub unsafe extern "C" fn hew_arena_free(ptr: *mut c_void) {
        // SAFETY: Caller guarantees ptr was allocated by hew_arena_malloc.
        unsafe { libc::free(ptr) };
    }
}

// ── Actor/scheduling modules ─────────────────────────────────────────────────
// Native modules require threads, signals, and networking. WASM modules provide
// cooperative single-threaded alternatives (mailbox_wasm, scheduler_wasm, bridge).

#[cfg(not(target_arch = "wasm32"))]
pub mod datetime;
#[cfg(not(target_arch = "wasm32"))]
pub mod file_io;
#[cfg(not(target_arch = "wasm32"))]
pub mod io_time;
#[cfg(not(target_arch = "wasm32"))]
pub mod iter;

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
#[cfg(not(target_arch = "wasm32"))]
pub mod reply_channel;
#[cfg(target_arch = "wasm32")]
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
pub mod timer_wheel;

#[cfg(not(target_arch = "wasm32"))]
pub mod hew_node;
#[cfg(not(target_arch = "wasm32"))]
pub mod supervisor;
#[cfg(not(target_arch = "wasm32"))]
pub mod transport;
#[cfg(not(target_arch = "wasm32"))]
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
#[cfg(not(target_arch = "wasm32"))]
pub mod stream;
#[cfg(not(target_arch = "wasm32"))]
pub mod tracing;

// ── Ecosystem modules (feature-gated) ───────────────────────────────────────

#[cfg(feature = "encryption")]
pub mod encryption;

pub mod log_core;

#[cfg(feature = "export-meta")]
pub mod export_meta;

// ── WASM entry point ─────────────────────────────────────────────────────────
// Provides `_start` for WASI command modules. The compiler renames the user's
// `main()` to `__original_main` when targeting WASM.

#[cfg(target_arch = "wasm32")]
extern "C" {
    fn __original_main() -> i32;
}

/// WASI entry point — delegates to the compiler-generated `__original_main`.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn _start() {
    // SAFETY: `__original_main` is always emitted by hew-codegen for every
    // Hew program and has the signature `() -> i32`.
    let code = unsafe { __original_main() };
    if code != 0 {
        std::process::exit(code);
    }
}
