//! Crash signal handling for the Hew actor runtime.
//!
//! Provides per-worker alternate signal stacks and signal handlers for
//! synchronous crash signals (SEGV, SIGBUS, SIGFPE, SIGILL). When a
//! dispatch function crashes, the signal handler uses `siglongjmp` to
//! recover, marks the actor as Crashed, and the scheduler continues
//! processing other actors.
//!
//! # Safety Design
//!
//! The signal handler is async-signal-safe:
//! - Per-thread context is accessed via `pthread_getspecific` (POSIX
//!   async-signal-safe) rather than Rust's `thread_local!`.
//! - Recovery uses `sigsetjmp`/`siglongjmp` (POSIX-correct for signal
//!   contexts), not `setjmp`/`longjmp`.
//! - No memory allocation, locking, or I/O in the handler.
//!
//! # Platform Support
//!
//! Active on Unix-like platforms (Linux, macOS). Other platforms
//! (Windows, WASM) get no-op stubs.
//!
//! # Trap-as-actor-crash boundary (two-tier policy)
//!
//! This module codifies the formal boundary between traps that route to
//! an actor crash (recoverable) and traps that terminate the process
//! (program-terminating). The substrate below already implements the
//! policy; this docstring records the invariant so future changes do not
//! silently shift the line.
//!
//! ## Recoverable tier — actor-crash
//!
//! Synchronous fault signals delivered to a worker thread that holds a
//! valid `sigjmp_buf` recovery context are caught by
//! `crash_signal_handler`, which calls `siglongjmp` back to the
//! scheduler. The scheduler marks the actor `Crashed` and continues
//! processing other actors. The fault classes routed through this path:
//!
//! - `SIGSEGV`, `SIGBUS`, `SIGFPE`, `SIGILL`, `SIGTRAP`
//! - Integer overflow trap (LLVM `*.with.overflow` branch → `ud2` →
//!   `SIGILL`)
//! - OOB indexing trap (bounds check → trap)
//! - Integer divide / modulo / shift traps
//! - Explicit `panic(...)` and `hew_panic()`
//! - `HeapExceeded` (arena cap exceeded → `hew_trap_with_code` stamps
//!   the discriminator before `siglongjmp`)
//! - `PartitionDetected` (mailbox / duplex disconnect surfaces as a
//!   typed recv error, not a trap; included here for completeness as a
//!   fail-closed exit class the supervisor observes)
//! - FFI null-pointer dereference reaching the runtime
//!
//! Stack-overflow on the worker stack is recoverable: handlers run on
//! the per-worker alternate signal stack installed via `SA_ONSTACK` and
//! `sigaltstack`. A signal raised while the recovery path is already
//! running (double-fault) is the boundary — see below.
//!
//! ## Program-terminating tier — `_exit`
//!
//! The handler calls `libc::_exit(128 + sig)` (async-signal-safe) in
//! exactly these cases:
//!
//! - **Double-fault**: a signal arrives while `in_recovery` is set. See
//!   the re-entrancy check in `crash_signal_handler` below — if the
//!   recovery path itself faulted, there is no safe `siglongjmp` target
//!   and continuing would corrupt scheduler state.
//! - **No recovery context**: the faulting thread has no valid
//!   `sigjmp_buf` installed (signal raised in scheduler internals,
//!   pre-dispatch Rust code, or any worker before `init_worker_recovery`
//!   has run). Recovery would jump into uninitialised memory.
//! - **Allocator or scheduler invariant break**: detected by upstream
//!   code, not the signal handler directly; reachable via the same
//!   `_exit` path when the invariant guard cannot proceed.
//!
//! ## Why this is locked
//!
//! Both tiers are load-bearing for v0.5's fail-closed substrate. The
//! recoverable tier guarantees that a single actor's bug cannot down
//! the runtime; the terminating tier guarantees that scheduler-internal
//! corruption cannot silently propagate. Adding a fault class to the
//! recoverable list requires a corresponding `sigjmp_buf` reachability
//! proof; demoting a fault class to the terminating list is a
//! breaking change to the actor-isolation contract.

// ── Shared recovery logic ────────────────────────────────────────────────
//
// The shared module contains platform-independent recovery state and helper
// functions used by both the Unix and Windows implementations. This avoids
// duplicating the identical crash recovery logic in each platform module.

#[cfg(any(unix, windows))]
mod shared {
    use std::ffi::c_void;
    use std::ptr;
    use std::sync::atomic::{AtomicBool, AtomicI32, Ordering};

    use crate::actor::HewActor;

    /// Platform-independent recovery state.
    ///
    /// Contains all per-worker crash recovery fields except the `jmp_buf`
    /// (which differs between Unix `sigjmp_buf` and Windows custom layout).
    /// Each platform's `WorkerRecoveryCtx` embeds this struct alongside its
    /// platform-specific `SigJmpBuf`.
    ///
    /// # Layout
    ///
    /// All fields are written by the normal code path and read by the
    /// signal handler (same thread, so no cross-thread races).
    #[repr(C)]
    pub(super) struct RecoveryState {
        /// Whether `jmp_buf` contains a valid recovery point.
        pub(super) jmp_buf_valid: AtomicBool,
        /// Pointer to the actor currently being dispatched.
        pub(super) current_actor: *mut HewActor,
        /// Pointer to the current message node (for cleanup).
        pub(super) current_msg: *mut c_void,
        /// Signal number that caused the crash (set by handler).
        pub(super) crash_signal: AtomicI32,
        /// Fault address from `siginfo_t` (set by handler).
        pub(super) fault_addr: usize,
        /// Re-entrancy guard — prevents nested signal recovery.
        pub(super) in_recovery: AtomicBool,
        /// Worker thread ID for crash reporting.
        pub(super) worker_id: u32,
        /// Message type being processed when crash occurred.
        pub(super) msg_type: AtomicI32,
        /// `true` when the recovery was entered through the runtime's
        /// intentional direct-longjmp path (a Hew `panic()` or a `HEW_TRAP_*`
        /// runtime trap), `false` when it was entered through the hardware
        /// fault signal handler. The intentional path reuses the `SIGSEGV`
        /// signal number (11) as its marker, so without this flag an
        /// intentional panic and a genuine SIGSEGV null-deref are reported
        /// identically — letting real faults hide among the supervised
        /// panic-and-restart traffic. Reset to `false` on every dispatch.
        pub(super) intentional_panic: AtomicBool,
    }

    impl RecoveryState {
        /// Create a new recovery state with all fields zeroed/null.
        pub(super) fn new(worker_id: u32) -> Self {
            Self {
                jmp_buf_valid: AtomicBool::new(false),
                current_actor: ptr::null_mut(),
                current_msg: ptr::null_mut(),
                crash_signal: AtomicI32::new(0),
                fault_addr: 0,
                in_recovery: AtomicBool::new(false),
                worker_id,
                msg_type: AtomicI32::new(0),
                intentional_panic: AtomicBool::new(false),
            }
        }
    }

    /// Map a signal number to a human-readable name.
    ///
    /// Uses Unix signal names since they are the canonical representation
    /// across all platforms. Windows exception codes are mapped to their
    /// Unix equivalents before reaching this function.
    pub(super) fn signal_name(signal: i32) -> &'static str {
        match signal {
            11 => "SIGSEGV",
            7 => "SIGBUS",
            8 => "SIGFPE",
            4 => "SIGILL",
            5 => "SIGTRAP",
            _ => "UNKNOWN",
        }
    }

    /// Format `value` as `0x`-prefixed lowercase hex into `buf`, returning the
    /// written byte slice. Async-signal-safe: no allocation, no locking, pure
    /// arithmetic into a caller-owned stack buffer.
    ///
    /// `buf` must be at least 18 bytes (`0x` + 16 hex digits) to hold any
    /// `usize` on a 64-bit target; the caller sizes it accordingly.
    #[cfg(unix)]
    fn fmt_hex_usize(value: usize, buf: &mut [u8; 18]) -> &[u8] {
        const HEX: &[u8; 16] = b"0123456789abcdef";
        buf[0] = b'0';
        buf[1] = b'x';
        // Number of significant hex digits (at least 1, so a zero fault address
        // renders as `0x0`). `usize::BITS - leading_zeros` rounded up to nibbles.
        let significant_bits = (usize::BITS - value.leading_zeros()).max(1);
        let digits = significant_bits.div_ceil(4) as usize;
        // Emit nibbles MSB-first into buf[2..2+digits].
        for i in 0..digits {
            let shift = (digits - 1 - i) * 4;
            let nibble = (value >> shift) & 0xf;
            buf[2 + i] = HEX[nibble];
        }
        &buf[..2 + digits]
    }

    /// Emit a crash diagnostic to stderr using only async-signal-safe operations.
    ///
    /// This function must only call `write(2)` (POSIX async-signal-safe) and
    /// perform no heap allocation, no locking, and no I/O buffering — it is
    /// safe to call from a signal handler.
    ///
    /// Used when a synchronous fault signal is caught and there is no recovery
    /// context (main/free-fn context), so the process is about to `_exit`. A
    /// diagnostic is always emitted before process termination.
    ///
    /// The diagnostic carries the faulting address (from `siginfo_t.si_addr`)
    /// and the faulting thread's name, so an off-dispatch crash (the I/O accept
    /// / connection-reader / SWIM-driver threads, which have no recovery
    /// `jmp_buf` and so reach this terminal path) localizes to a concrete
    /// pointer + thread instead of an opaque "SIGSEGV somewhere". This is what
    /// turns a rare, load-only crash on a CI runner into an actionable fault
    /// site on its next occurrence.
    ///
    /// `eprintln!` is NOT async-signal-safe (it acquires a lock on the stderr
    /// handle). `write(2)` on fd 2 is the correct alternative here.
    #[cfg(unix)]
    pub(super) fn emit_crash_diagnostic_sig_safe(sig: i32, fault_addr: usize) {
        // Faulting thread name: pthread_getname_np writes a NUL-terminated
        // string into a caller buffer and is async-signal-safe on Linux/macOS.
        //
        // The symbol is declared locally rather than via `libc::` because the
        // libc crate exposes `pthread_getname_np` for the BSD/Apple targets but
        // not the `linux_like` module — yet glibc (>=2.12) and musl both export
        // the identical 3-arg signature, so a local extern is portable across
        // every unix target the runtime builds for, including the Linux CI host
        // where the off-dispatch crash actually fires.
        extern "C" {
            fn pthread_getname_np(
                thread: libc::pthread_t,
                name: *mut libc::c_char,
                len: libc::size_t,
            ) -> libc::c_int;
        }

        // Write fixed prefix, then signal name, then fault address, then the
        // faulting thread name, then newline — no allocation. Each write is a
        // separate syscall; partial writes are acceptable for a terminal
        // diagnostic before _exit.
        const PREFIX: &[u8] = b"hew: crash in main context: ";
        const AT: &[u8] = b" at ";
        const THREAD: &[u8] = b" thread=";
        const SUFFIX: &[u8] = b"\n";
        let name = signal_name(sig).as_bytes();
        let mut addr_buf = [0u8; 18];
        let addr = fmt_hex_usize(fault_addr, &mut addr_buf);

        // A failed name lookup leaves the buffer as-is; we render "<unknown>".
        let mut tname = [0u8; 32];
        // SAFETY: pthread_self / pthread_getname_np are async-signal-safe; the
        // buffer is stack-owned and sized for the platform's max thread name.
        let tname_ok = unsafe {
            pthread_getname_np(libc::pthread_self(), tname.as_mut_ptr().cast(), tname.len()) == 0
        };
        let tname_slice: &[u8] = if tname_ok {
            let end = tname.iter().position(|&b| b == 0).unwrap_or(tname.len());
            if end == 0 {
                b"<main>"
            } else {
                &tname[..end]
            }
        } else {
            b"<unknown>"
        };

        // SAFETY: write(2) is POSIX async-signal-safe. fd 2 (STDERR_FILENO) is
        // always open. The byte slices are valid static/stack data. We ignore
        // the return value — a failed write before _exit is not actionable.
        unsafe {
            libc::write(2, PREFIX.as_ptr().cast(), PREFIX.len());
            libc::write(2, name.as_ptr().cast(), name.len());
            libc::write(2, AT.as_ptr().cast(), AT.len());
            libc::write(2, addr.as_ptr().cast(), addr.len());
            libc::write(2, THREAD.as_ptr().cast(), THREAD.len());
            libc::write(2, tname_slice.as_ptr().cast(), tname_slice.len());
            libc::write(2, SUFFIX.as_ptr().cast(), SUFFIX.len());
        }
    }

    /// Store dispatch metadata in the recovery state.
    ///
    /// Called at the start of each dispatch to record the actor and message
    /// being processed, so crash recovery can clean up appropriately.
    ///
    /// # Safety
    ///
    /// `actor` must be a valid pointer to a live `HewActor`. `msg` must be
    /// a valid `*mut HewMsgNode` or null.
    pub(super) unsafe fn prepare_dispatch_impl(
        state: &mut RecoveryState,
        actor: *mut HewActor,
        msg: *mut c_void,
    ) {
        state.current_actor = actor;
        state.current_msg = msg;
        state.crash_signal.store(0, Ordering::Relaxed);
        state.fault_addr = 0;
        state.in_recovery.store(false, Ordering::Release);
        // Fresh dispatch: any crash from here is a genuine hardware fault
        // until the intentional direct-longjmp path explicitly marks itself.
        state.intentional_panic.store(false, Ordering::Relaxed);

        // Extract message type from HewMsgNode for crash reporting.
        let msg_type = if msg.is_null() {
            0
        } else {
            // SAFETY: msg is valid HewMsgNode from hew_mailbox_try_recv.
            // We cast c_void back to HewMsgNode to read msg_type.
            unsafe { (*(msg.cast::<crate::mailbox::HewMsgNode>())).msg_type }
        };
        state.msg_type.store(msg_type, Ordering::Relaxed);
    }

    /// Mark the `jmp_buf` as valid after `sigsetjmp` returns 0 (normal path).
    pub(super) fn mark_recovery_active_impl(state: &mut RecoveryState) {
        state.jmp_buf_valid.store(true, Ordering::Release);
    }

    /// Handle crash recovery after sigsetjmp returned non-zero (crash path).
    ///
    /// Marks the actor as Crashed, builds and pushes a crash report, logs
    /// the crash, and clears the recovery context. Returns `(signal, fault_addr)`.
    ///
    /// # Safety
    ///
    /// Must only be called immediately after sigsetjmp returned non-zero,
    /// on the same thread.
    pub(super) unsafe fn handle_crash_recovery_impl(state: &mut RecoveryState) -> (i32, usize) {
        let signal = state.crash_signal.load(Ordering::Acquire);
        let fault_addr = state.fault_addr;
        let actor = state.current_actor;
        let msg_type = state.msg_type.load(Ordering::Acquire);
        let worker_id = state.worker_id;
        let intentional = state.intentional_panic.load(Ordering::Acquire);

        // Cache actor data before supervisor notification to avoid race.
        // After hew_actor_trap, another thread could process the supervisor
        // notification and call hew_actor_free before we dereference actor.
        let (actor_id, cached_pid, dispatch_ptr, report) = if actor.is_null() {
            (0u64, 0u64, 0usize, None)
        } else {
            // SAFETY: actor pointer was stored in prepare_dispatch_recovery
            // and the actor is still alive (it's Running — only the current
            // worker thread can transition it, and we haven't freed it).
            unsafe {
                let id = (*actor).id;
                let dispatch_ptr = (*actor).dispatch.map_or(0, |f| f as usize);
                let report = crate::crash::build_crash_report(
                    actor, signal,
                    0, // signal_code - not available from siginfo_t in current handler
                    fault_addr, msg_type, worker_id,
                );
                (id, id, dispatch_ptr, Some(report))
            }
        };

        // NOW notify supervisor (may trigger actor free on another thread).
        if !actor.is_null() {
            // SAFETY: actor pointer is valid (checked above).
            unsafe { crate::actor::hew_actor_trap(actor, signal) };
        }

        // Push crash report to global log using cached data.
        if let Some(report) = report {
            crate::crash::push_crash_report(report);
        }

        // Crash logging with cached actor data. Distinguish an intentional Hew
        // `panic()` / `HEW_TRAP_*` runtime trap (recovered via the direct
        // longjmp marker, which reuses signal 11) from a genuine hardware fault
        // so a real null-deref SIGSEGV is reported distinctly instead of being
        // masked by the identical-looking supervised panic-and-restart traffic.
        //
        // A diagnostic is always emitted — actor_id == 0 means the trap
        // occurred in main/free-fn context (outside any actor dispatch) and
        // is equally important to surface.
        if actor_id != 0 {
            // Resolve a human-readable context name: prefer the registered
            // handler name ("ActorType::handler") for the (dispatch_fn, msg_type)
            // pair, fall back to the actor type name, then the raw msg_type.
            let context =
                crate::profiler::actor_registry::handler_name_by_ptr(dispatch_ptr, msg_type)
                    .unwrap_or_else(|| {
                        // Owned variant: copies the name under
                        // DISPATCH_TYPE_REGISTRY's lock, closing the
                        // lookup-to-copy TOCTOU window against a concurrent
                        // `clear_dispatch_registry` reclaim (the borrowing
                        // variant releases the lock before returning).
                        let type_name =
                            crate::profiler::actor_registry::lookup_dispatch_type_by_ptr_owned(
                                dispatch_ptr,
                            );
                        if type_name == "Actor" {
                            format!("msg_type={msg_type}")
                        } else {
                            format!("{type_name} (msg_type={msg_type})")
                        }
                    });
            if intentional {
                eprintln!(
                    "hew: actor {actor_id} (pid={cached_pid}) panicked in {context}, worker={worker_id}"
                );
            } else {
                let sig_name = signal_name(signal);
                eprintln!(
                    "hew: actor {actor_id} (pid={cached_pid}) crashed with {sig_name} at {fault_addr:#x} in {context}, worker={worker_id}"
                );
            }
        } else {
            // Trap in main/free-fn context (no actor dispatch active).
            if intentional {
                eprintln!("hew: panic in main context (trap code {signal}), worker={worker_id}");
            } else {
                let sig_name = signal_name(signal);
                eprintln!(
                    "hew: crash in main context: {sig_name} at {fault_addr:#x}, worker={worker_id}"
                );
            }
        }

        // Clear recovery context.
        state.current_actor = ptr::null_mut();
        state.current_msg = ptr::null_mut();
        state.in_recovery.store(false, Ordering::Release);

        (signal, fault_addr)
    }

    /// Clear the dispatch recovery context after a successful dispatch.
    ///
    /// Invalidates the jump buffer so stale signals can't jump to a
    /// dead recovery point.
    pub(super) fn clear_dispatch_recovery_impl(state: &mut RecoveryState) {
        state.jmp_buf_valid.store(false, Ordering::Release);
        state.current_actor = ptr::null_mut();
        state.current_msg = ptr::null_mut();
        state.msg_type.store(0, Ordering::Relaxed);
    }

    /// Perform pre-longjmp checks for intentional panic recovery.
    ///
    /// Validates the recovery context, checks re-entrancy, and records
    /// crash metadata. Returns `true` if the caller should proceed with
    /// the platform-specific longjmp call.
    ///
    /// The SIGSEGV signal number (11) is used as the canonical "intentional
    /// panic" marker across all platforms.
    pub(super) fn try_direct_longjmp_preamble(state: &mut RecoveryState) -> bool {
        try_direct_longjmp_preamble_with_code(state, 11) // SIGSEGV
    }

    /// Like [`try_direct_longjmp_preamble`] but records a custom crash code
    /// instead of the SIGSEGV default.
    ///
    /// Used by callers that need a named exit reason (e.g. `HeapExceeded`)
    /// rather than the generic SIGSEGV-equivalent marker. The code is stored
    /// in `crash_signal`, which `handle_crash_recovery_impl` forwards to
    /// `hew_actor_trap` as the `error_code`, where it lands in the actor's
    /// `error_code` field and is observable via `hew_actor_get_error`.
    pub(super) fn try_direct_longjmp_preamble_with_code(
        state: &mut RecoveryState,
        code: i32,
    ) -> bool {
        if !state.jmp_buf_valid.load(Ordering::Acquire) {
            return false;
        }
        if state.in_recovery.swap(true, Ordering::Acquire) {
            return false;
        }
        state.crash_signal.store(code, Ordering::Release);
        state.fault_addr = 0;
        // Mark this recovery as an intentional Hew panic / runtime trap so the
        // reporter does not mislabel it as a hardware SIGSEGV (the marker code
        // reuses signal 11). Set before clearing `jmp_buf_valid` so it is
        // visible by the time recovery reads it.
        state.intentional_panic.store(true, Ordering::Release);
        state.jmp_buf_valid.store(false, Ordering::Release);
        true
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        /// B2 de-masking invariant: the runtime's intentional direct-longjmp
        /// path (a Hew `panic()` / `HEW_TRAP_*` runtime trap) reuses the
        /// `SIGSEGV` signal number (11) as its recovery marker. Without an
        /// explicit flag, an intentional supervised panic-and-restart is
        /// reported identically to a genuine hardware SIGSEGV null-deref, so a
        /// real first-spawn fault hides among the panic traffic. This pins the
        /// flag that lets the reporter tell them apart.
        #[test]
        fn intentional_panic_flag_distinguishes_trap_from_hardware_fault() {
            let mut state = RecoveryState::new(0);
            // Fresh state: a crash would be treated as a genuine hardware fault.
            assert!(!state.intentional_panic.load(Ordering::Acquire));

            // Entering a dispatch leaves the flag false (hardware-fault default):
            // a SIGSEGV from handler code must still report "crashed with SIGSEGV".
            // SAFETY: null actor/msg is the documented quiescent case for the
            // metadata store (no dereference of either pointer).
            unsafe { prepare_dispatch_impl(&mut state, ptr::null_mut(), ptr::null_mut()) };
            assert!(!state.intentional_panic.load(Ordering::Acquire));

            // The intentional panic/trap path marks the recovery so the reporter
            // labels it a panic, not a SIGSEGV null-deref.
            mark_recovery_active_impl(&mut state);
            assert!(try_direct_longjmp_preamble_with_code(&mut state, 11));
            assert!(state.intentional_panic.load(Ordering::Acquire));

            // The next dispatch resets it: a subsequent genuine fault is reported
            // as a real crash rather than being masked by the prior panic.
            // SAFETY: same quiescent null-pointer case as above.
            unsafe { prepare_dispatch_impl(&mut state, ptr::null_mut(), ptr::null_mut()) };
            assert!(!state.intentional_panic.load(Ordering::Acquire));
        }

        /// The async-signal-safe hex formatter backs the terminal crash
        /// diagnostic for off-dispatch faults (accept / connection-reader /
        /// SWIM-driver threads). It must render the exact fault address — a
        /// wrong value would mislocate the next CI crash — including the zero,
        /// single-digit, and full-width boundaries.
        #[cfg(unix)]
        #[test]
        fn fmt_hex_usize_renders_fault_addresses() {
            let mut buf = [0u8; 18];
            assert_eq!(fmt_hex_usize(0, &mut buf), b"0x0");
            assert_eq!(fmt_hex_usize(0xf, &mut buf), b"0xf");
            assert_eq!(fmt_hex_usize(0x10, &mut buf), b"0x10");
            assert_eq!(fmt_hex_usize(0xdead_beef, &mut buf), b"0xdeadbeef");
            // A typical low-half null-region offset deref (the classic
            // null-struct-field fault shape).
            assert_eq!(fmt_hex_usize(0x28, &mut buf), b"0x28");
            // Full-width usize: every nibble significant, no leading-zero trim.
            assert_eq!(fmt_hex_usize(usize::MAX, &mut buf), b"0xffffffffffffffff");
        }
    }
}

// ── Unix implementation (Linux + macOS) ─────────────────────────────────

#[cfg(unix)]
mod platform {
    use std::ffi::c_void;
    use std::ptr;
    use std::sync::atomic::Ordering;
    use std::sync::OnceLock;

    use crate::actor::HewActor;

    use super::shared::RecoveryState;

    // ── Constants ───────────────────────────────────────────────────────

    /// Alternate signal stack size. 128 KiB provides ample headroom for
    /// the signal handler, `siglongjmp`, and any kernel-injected frames.
    /// At 16 workers this costs 2 MiB total — negligible.
    const ALT_STACK_SIZE: usize = 128 * 1024;

    // ── FFI bindings (libc) ─────────────────────────────────────────────

    // NOTE: We bind libc functions directly rather than depending on the
    // `libc` crate for signal types, because Rust's libc crate doesn't
    // expose `sigjmp_buf` as a usable type.

    /// `sigjmp_buf` — platform-specific save buffer for `sigsetjmp`.
    /// On `x86_64` Linux (glibc), `sigjmp_buf` is `__jmp_buf_tag[1]` where
    /// `__jmp_buf_tag` is 200 bytes. We over-allocate to 256 bytes.
    #[repr(C, align(16))]
    pub(crate) struct SigJmpBuf {
        #[cfg(target_arch = "x86_64")]
        _buf: [u8; 256],
        #[cfg(not(target_arch = "x86_64"))]
        _buf: [u8; 512], // conservative for other arches
    }

    impl SigJmpBuf {
        const fn zeroed() -> Self {
            Self {
                _buf: [0u8; {
                    #[cfg(target_arch = "x86_64")]
                    {
                        256
                    }
                    #[cfg(not(target_arch = "x86_64"))]
                    {
                        512
                    }
                }],
            }
        }
    }

    extern "C" {
        // POSIX signal functions
        fn sigaction(
            sig: libc::c_int,
            act: *const libc::sigaction,
            oldact: *mut libc::sigaction,
        ) -> libc::c_int;

        fn sigaltstack(ss: *const libc::stack_t, old_ss: *mut libc::stack_t) -> libc::c_int;

        fn pthread_sigmask(
            how: libc::c_int,
            set: *const libc::sigset_t,
            oldset: *mut libc::sigset_t,
        ) -> libc::c_int;

        // POSIX async-signal-safe TLS
        fn pthread_key_create(
            key: *mut libc::pthread_key_t,
            dtor: Option<unsafe extern "C" fn(*mut c_void)>,
        ) -> libc::c_int;
        fn pthread_setspecific(key: libc::pthread_key_t, value: *const c_void) -> libc::c_int;
        fn pthread_getspecific(key: libc::pthread_key_t) -> *mut c_void;

        // sigsetjmp/siglongjmp — the correct pair for signal handlers.
        // On glibc, sigsetjmp is a macro that expands to __sigsetjmp.
        // On macOS, sigsetjmp is the actual symbol name.
        #[cfg_attr(target_os = "linux", link_name = "__sigsetjmp")]
        pub(crate) fn sigsetjmp(env: *mut SigJmpBuf, savemask: libc::c_int) -> libc::c_int;
        fn siglongjmp(env: *mut SigJmpBuf, val: libc::c_int) -> !;
    }

    // ── Per-worker recovery context ─────────────────────────────────────

    /// Per-worker crash recovery context.
    ///
    /// Stored via `pthread_setspecific` (async-signal-safe) rather than
    /// Rust's `thread_local!` (not async-signal-safe).
    #[repr(C)]
    struct WorkerRecoveryCtx {
        /// `sigsetjmp` save buffer.
        jmp_buf: SigJmpBuf,
        /// Platform-independent recovery state.
        state: RecoveryState,
    }

    impl WorkerRecoveryCtx {
        fn new_boxed(worker_id: u32) -> Box<Self> {
            Box::new(Self {
                jmp_buf: SigJmpBuf::zeroed(),
                state: RecoveryState::new(worker_id),
            })
        }
    }

    /// `pthread_key_t` for per-thread `WorkerRecoveryCtx`.
    static RECOVERY_KEY: OnceLock<libc::pthread_key_t> = OnceLock::new();

    /// Destructor called by pthreads when a thread exits.
    ///
    /// # Safety
    ///
    /// `ptr` is a `Box<WorkerRecoveryCtx>` that was created in
    /// `init_worker_recovery`.
    unsafe extern "C" fn recovery_ctx_dtor(ptr: *mut c_void) {
        if !ptr.is_null() {
            // SAFETY: ptr was created via Box::into_raw in init_worker_recovery.
            drop(unsafe { Box::from_raw(ptr.cast::<WorkerRecoveryCtx>()) }); // ALLOCATOR-PAIRING: GlobalAlloc
        }
    }

    /// Get the current thread's recovery context.
    ///
    /// Returns null if `init_worker_recovery` hasn't been called on this
    /// thread yet.
    ///
    /// # Safety
    ///
    /// This function is async-signal-safe: `pthread_getspecific` is in the
    /// POSIX async-signal-safe list.
    #[inline]
    unsafe fn get_recovery_ctx() -> *mut WorkerRecoveryCtx {
        let Some(&key) = RECOVERY_KEY.get() else {
            return ptr::null_mut();
        };
        // SAFETY: key is valid (created in init_crash_handling).
        // pthread_getspecific is async-signal-safe.
        unsafe { pthread_getspecific(key) }.cast::<WorkerRecoveryCtx>()
    }

    // ── Signal handler ──────────────────────────────────────────────────

    /// Signal handler for synchronous crash signals.
    ///
    /// # Async-Signal-Safety
    ///
    /// This function ONLY calls:
    /// - `pthread_getspecific` (async-signal-safe per POSIX)
    /// - Atomic loads/stores (lock-free, async-signal-safe)
    /// - `siglongjmp` (async-signal-safe per POSIX)
    /// - `libc::_exit` (async-signal-safe per POSIX)
    extern "C" fn crash_signal_handler(
        sig: libc::c_int,
        info: *mut libc::siginfo_t,
        _ucontext: *mut c_void,
    ) {
        // SAFETY: pthread_getspecific is async-signal-safe.
        let ctx = unsafe { get_recovery_ctx() };
        if ctx.is_null() {
            // No recovery context (main/free-fn context) — emit a diagnostic
            // then terminate. F1.3: a crash must never be silent. Capture the
            // faulting address from siginfo so an off-dispatch crash (accept /
            // connection-reader / SWIM-driver threads have no recovery jmp_buf
            // and land here) localizes to a concrete pointer instead of an
            // opaque "SIGSEGV somewhere".
            let fault_addr = if info.is_null() {
                0usize
            } else {
                // `si_addr` is an async-signal-safe read of the kernel-provided
                // siginfo_t — a method on Linux, a field on macOS — matched to
                // the recovered path below.
                #[cfg(target_os = "linux")]
                {
                    // SAFETY: `info` was validated non-null above.
                    (unsafe { (*info).si_addr() }) as usize
                }
                #[cfg(not(target_os = "linux"))]
                {
                    // SAFETY: `info` was validated non-null above.
                    (unsafe { (*info).si_addr }) as usize
                }
            };
            // write(2) is POSIX async-signal-safe; see emit_crash_diagnostic_sig_safe.
            super::shared::emit_crash_diagnostic_sig_safe(sig, fault_addr);
            // SAFETY: _exit is async-signal-safe.
            unsafe { libc::_exit(128 + sig) };
        }

        // SAFETY: ctx is valid (created in init_worker_recovery, same thread).
        let ctx = unsafe { &mut *ctx };

        // Re-entrancy check: if we're already recovering from a signal,
        // a second crash means the recovery path itself is broken.
        if ctx.state.in_recovery.swap(true, Ordering::Acquire) {
            // SAFETY: _exit is async-signal-safe.
            unsafe { libc::_exit(128 + sig) };
        }

        // Check that we have a valid recovery point.
        if !ctx.state.jmp_buf_valid.load(Ordering::Acquire) {
            ctx.state.in_recovery.store(false, Ordering::Release);
            // SAFETY: _exit is async-signal-safe.
            unsafe { libc::_exit(128 + sig) };
        }

        // Record crash metadata.
        ctx.state.crash_signal.store(sig, Ordering::Release);
        if !info.is_null() {
            // SAFETY: info is valid in signal context. si_addr accesses
            // the siginfo_t field (async-signal-safe read).
            // On Linux it's a method, on macOS it's a field.
            #[cfg(target_os = "linux")]
            {
                // SAFETY: `info` was validated non-null above; si_addr() is an
                // async-signal-safe read from the kernel-provided siginfo_t.
                ctx.state.fault_addr = unsafe { (*info).si_addr() } as usize;
            }
            #[cfg(not(target_os = "linux"))]
            {
                // SAFETY: `info` is a valid siginfo_t provided by the kernel.
                ctx.state.fault_addr = unsafe { (*info).si_addr } as usize;
            }
        }

        // Invalidate the jump buffer to prevent re-use.
        ctx.state.jmp_buf_valid.store(false, Ordering::Release);

        // Jump back to the scheduler's recovery point.
        //
        // SAFETY: jmp_buf was set by sigsetjmp in activate_actor
        // on the same thread. The stack frame that called sigsetjmp is
        // still on the stack (it's the activate_actor → dispatch chain).
        // siglongjmp restores the signal mask saved by sigsetjmp.
        unsafe { siglongjmp(&raw mut ctx.jmp_buf, 1) };
    }

    // ── Public API ──────────────────────────────────────────────────────

    /// Initialize crash handling infrastructure.
    ///
    /// Creates the pthread key for per-thread recovery contexts and
    /// installs signal handlers. Called once from `hew_sched_init`.
    pub(crate) fn init_crash_handling() {
        // Create pthread key (once).
        RECOVERY_KEY.get_or_init(|| {
            let mut key: libc::pthread_key_t = 0;
            // SAFETY: key is a valid out-pointer; recovery_ctx_dtor is a
            // valid function pointer.
            let ret = unsafe { pthread_key_create(&raw mut key, Some(recovery_ctx_dtor)) };
            assert!(ret == 0, "pthread_key_create failed: {ret}");
            key
        });

        // Install signal handlers.
        //
        // SIGTRAP is included because `llvm.trap` lowers to `brk #1` on
        // Linux aarch64, which the kernel delivers as SIGTRAP (not SIGILL).
        // On macOS aarch64 `brk #1` delivers SIGILL, so SIGTRAP is harmless
        // there. On x86-64 `ud2` delivers SIGILL regardless of OS.
        let crash_signals = [
            libc::SIGSEGV,
            libc::SIGBUS,
            libc::SIGFPE,
            libc::SIGILL,
            libc::SIGTRAP,
        ];

        for &sig in &crash_signals {
            // SAFETY: sa is fully initialized. sigaction is safe to call
            // for these signal numbers.
            unsafe {
                let mut sa: libc::sigaction = std::mem::zeroed();
                sa.sa_flags = libc::SA_SIGINFO | libc::SA_ONSTACK;
                // Fill the mask to block all signals during handler execution.
                libc::sigfillset(&raw mut sa.sa_mask);
                sa.sa_sigaction = crash_signal_handler
                    as extern "C" fn(libc::c_int, *mut libc::siginfo_t, *mut c_void)
                    as usize;

                let ret = sigaction(sig, &raw const sa, ptr::null_mut());
                assert!(ret == 0, "sigaction({sig}) failed");
            }
        }
    }

    /// Set up per-worker recovery infrastructure.
    ///
    /// Allocates a 128 KiB alternate signal stack and a recovery context.
    /// Called at the start of each `worker_loop` with the worker ID.
    pub(crate) fn init_worker_recovery(worker_id: u32) {
        // Allocate alternate signal stack.
        //
        // SAFETY: mmap with MAP_PRIVATE | MAP_ANONYMOUS creates a fresh
        // anonymous mapping. We check for MAP_FAILED.
        let stack_mem = unsafe {
            libc::mmap(
                ptr::null_mut(),
                ALT_STACK_SIZE,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_PRIVATE | libc::MAP_ANON,
                -1,
                0,
            )
        };
        if stack_mem == libc::MAP_FAILED {
            eprintln!(
                "warning: failed to mmap signal stack, crash recovery disabled for this worker"
            );
            return;
        }

        let ss = libc::stack_t {
            ss_sp: stack_mem,
            ss_flags: 0,
            ss_size: ALT_STACK_SIZE,
        };
        // SAFETY: ss is valid and stack_mem is a valid mapping.
        let ret = unsafe { sigaltstack(&raw const ss, ptr::null_mut()) };
        if ret != 0 {
            eprintln!("warning: sigaltstack failed, crash recovery disabled for this worker");
            // SAFETY: stack_mem was returned by mmap.
            unsafe { libc::munmap(stack_mem, ALT_STACK_SIZE) };
            return;
        }

        // Allocate and install per-thread recovery context.
        let ctx = WorkerRecoveryCtx::new_boxed(worker_id);
        let ctx_ptr = Box::into_raw(ctx); // ALLOCATOR-PAIRING: GlobalAlloc
        let key = *RECOVERY_KEY
            .get()
            .expect("init_crash_handling must be called before init_worker_recovery");
        // SAFETY: key is valid (created in init_crash_handling), ctx_ptr
        // is a valid heap pointer.
        let ret = unsafe { pthread_setspecific(key, ctx_ptr.cast()) };
        assert!(ret == 0, "pthread_setspecific failed: {ret}");

        // Block async signals in worker threads so they don't interfere
        // with dispatch. Only the main thread should handle SIGTERM etc.
        //
        // SAFETY: set is initialized by sigemptyset/sigaddset, both valid
        // calls.
        unsafe {
            let mut set: libc::sigset_t = std::mem::zeroed();
            libc::sigemptyset(&raw mut set);
            libc::sigaddset(&raw mut set, libc::SIGTERM);
            libc::sigaddset(&raw mut set, libc::SIGINT);
            libc::sigaddset(&raw mut set, libc::SIGQUIT);
            libc::sigaddset(&raw mut set, libc::SIGHUP);
            libc::sigaddset(&raw mut set, libc::SIGPIPE);
            pthread_sigmask(libc::SIG_BLOCK, &raw const set, ptr::null_mut());
        }
    }

    /// Prepare the recovery context for a dispatch call WITHOUT calling
    /// `sigsetjmp`. The caller must call `sigsetjmp` directly in its own
    /// stack frame (to keep the `jmp_buf` valid for the duration of dispatch).
    ///
    /// Returns the `jmp_buf` pointer for the caller's `sigsetjmp`, or null
    /// if no recovery context is available.
    ///
    /// # Safety
    ///
    /// `actor` must be a valid pointer to a live `HewActor` that will
    /// remain valid for the duration of the dispatch call. `msg` must be
    /// a valid `*mut HewMsgNode` or null.
    pub(crate) unsafe fn prepare_dispatch_recovery(
        actor: *mut HewActor,
        msg: *mut c_void,
    ) -> *mut SigJmpBuf {
        // SAFETY: called from a worker thread that ran init_worker_recovery.
        let ctx = unsafe { get_recovery_ctx() };
        if ctx.is_null() {
            return ptr::null_mut();
        }

        // SAFETY: ctx is valid (same thread that created it).
        let ctx = unsafe { &mut *ctx };

        // Store dispatch metadata via shared helper.
        // SAFETY: actor and msg validity guaranteed by caller.
        unsafe { super::shared::prepare_dispatch_impl(&mut ctx.state, actor, msg) };

        &raw mut ctx.jmp_buf
    }

    /// Mark the `jmp_buf` as valid after `sigsetjmp` returns 0 (normal path).
    pub(crate) fn mark_recovery_active() {
        // SAFETY: called from a worker thread with a valid recovery context.
        let ctx = unsafe { get_recovery_ctx() };
        if ctx.is_null() {
            return;
        }
        // SAFETY: ctx is valid (same thread).
        let ctx = unsafe { &mut *ctx };
        super::shared::mark_recovery_active_impl(&mut ctx.state);
    }

    /// Handle crash recovery after sigsetjmp returned non-zero (crash path).
    ///
    /// Marks the actor as Crashed, logs the crash, and clears the
    /// recovery context. Returns `(signal, fault_addr)`.
    ///
    /// # Safety
    ///
    /// Must only be called immediately after sigsetjmp returned non-zero,
    /// on the same thread.
    pub(crate) unsafe fn handle_crash_recovery() -> (i32, usize) {
        // SAFETY: called from a worker thread with a valid recovery context.
        let ctx = unsafe { get_recovery_ctx() };
        if ctx.is_null() {
            return (0, 0);
        }

        // SAFETY: ctx is valid (same thread).
        let ctx = unsafe { &mut *ctx };

        // SAFETY: called immediately after sigsetjmp returned non-zero.
        unsafe { super::shared::handle_crash_recovery_impl(&mut ctx.state) }
    }

    /// Clear the dispatch recovery context after a successful dispatch.
    ///
    /// Invalidates the jump buffer so stale signals can't jump to a
    /// dead recovery point.
    pub(crate) fn clear_dispatch_recovery() {
        // SAFETY: called from a worker thread with a valid recovery context.
        let ctx = unsafe { get_recovery_ctx() };
        if ctx.is_null() {
            return;
        }
        // SAFETY: ctx is valid (same thread).
        let ctx = unsafe { &mut *ctx };
        super::shared::clear_dispatch_recovery_impl(&mut ctx.state);
    }

    /// Attempt direct longjmp recovery from an intentional panic.
    ///
    /// On Unix the signal handler already works, but direct longjmp is
    /// faster (skips the signal round-trip) and consistent with the
    /// Windows implementation.
    ///
    /// # Safety
    ///
    /// Must be called from a dispatch context.
    pub(crate) unsafe fn try_direct_longjmp() {
        // SAFETY: accesses the thread-local recovery context via pthread key;
        // caller guarantees we are in a dispatch context on the correct thread.
        let ctx = unsafe { get_recovery_ctx() };
        if ctx.is_null() {
            return;
        }
        // SAFETY: ctx is non-null and exclusively owned by this thread.
        let ctx = unsafe { &mut *ctx };
        if !super::shared::try_direct_longjmp_preamble(&mut ctx.state) {
            return;
        }
        // SAFETY: jmp_buf was set by sigsetjmp in activate_actor on this
        // thread. The stack frame that called sigsetjmp is still live.
        unsafe {
            siglongjmp(&raw mut ctx.jmp_buf, 1);
        }
    }

    /// Like [`try_direct_longjmp`] but records `code` as the crash reason
    /// instead of the default SIGSEGV (11) marker.
    ///
    /// # Safety
    ///
    /// Must be called from a dispatch context on the worker thread.
    pub(crate) unsafe fn try_direct_longjmp_with_code(code: i32) {
        // SAFETY: accesses the thread-local recovery context via pthread key;
        // caller guarantees we are in a dispatch context on the correct thread.
        let ctx = unsafe { get_recovery_ctx() };
        if ctx.is_null() {
            return;
        }
        // SAFETY: ctx is non-null and exclusively owned by this thread.
        let ctx = unsafe { &mut *ctx };
        if !super::shared::try_direct_longjmp_preamble_with_code(&mut ctx.state, code) {
            return;
        }
        // SAFETY: jmp_buf was set by sigsetjmp in activate_actor on this
        // thread. The stack frame that called sigsetjmp is still live.
        unsafe {
            siglongjmp(&raw mut ctx.jmp_buf, 1);
        }
    }
}

// ── Windows implementation (Vectored Exception Handling) ────────────────

#[cfg(windows)]
mod platform {
    use std::ffi::c_void;
    use std::ptr;
    use std::sync::atomic::Ordering;
    use std::sync::OnceLock;

    use crate::actor::HewActor;

    use super::shared::RecoveryState;

    // ── Windows exception codes ─────────────────────────────────────────
    const EXCEPTION_ACCESS_VIOLATION: u32 = 0xC0000005;
    const EXCEPTION_IN_PAGE_ERROR: u32 = 0xC0000006;
    const EXCEPTION_INT_DIVIDE_BY_ZERO: u32 = 0xC0000094;
    const EXCEPTION_ILLEGAL_INSTRUCTION: u32 = 0xC000001D;
    const EXCEPTION_CONTINUE_SEARCH: i32 = 0;

    // Map Windows exception codes to Unix signal numbers for reporting.
    fn exception_to_signal(code: u32) -> i32 {
        match code {
            EXCEPTION_ACCESS_VIOLATION | EXCEPTION_IN_PAGE_ERROR => 11, // SIGSEGV
            EXCEPTION_INT_DIVIDE_BY_ZERO => 8,                          // SIGFPE
            EXCEPTION_ILLEGAL_INSTRUCTION => 4,                         // SIGILL
            _ => -1,
        }
    }

    // ── FFI types ───────────────────────────────────────────────────────

    #[repr(C)]
    struct ExceptionRecord {
        exception_code: u32,
        exception_flags: u32,
        exception_record: *mut ExceptionRecord,
        exception_address: *mut c_void,
        number_parameters: u32,
        exception_information: [usize; 15], // EXCEPTION_MAXIMUM_PARAMETERS
    }

    #[repr(C)]
    struct ExceptionPointers {
        exception_record: *mut ExceptionRecord,
        context_record: *mut c_void,
    }

    /// `jmp_buf` for Windows x86_64.
    /// Layout: 10 × u64 (callee-saved regs + RSP + return addr) = 80 bytes.
    /// Over-allocate to 256 bytes for alignment headroom and future XMM slots.
    #[repr(C, align(16))]
    pub(crate) struct SigJmpBuf {
        _buf: [u8; 256],
    }

    impl SigJmpBuf {
        const fn zeroed() -> Self {
            Self { _buf: [0u8; 256] }
        }
    }

    // Custom setjmp/longjmp that bypass Windows RtlUnwindEx.
    //
    // The standard MSVC `longjmp` calls `RtlUnwindEx` to walk and unwind
    // SEH frames between the current RSP and the target RSP. If any
    // frame lacks proper `.pdata` unwind info (e.g., JIT'd Hew dispatch
    // code), or if the SEH chain is in an unexpected state, this raises
    // STATUS_BAD_STACK (0xc0000028).
    //
    // Our custom implementation does a raw register save/restore, which
    // is safe for crash recovery where we don't need SEH frame cleanup.
    //
    // Windows x64 callee-saved: RBX, RBP, RDI, RSI, R12-R15.
    // We also save RSP and the return address.

    /// Save callee-saved registers + RSP + return address into `env`.
    /// Returns 0 on initial call, non-zero when reached via `longjmp`.
    ///
    /// # Safety
    ///
    /// `env` must point to a valid, aligned `SigJmpBuf`.
    #[unsafe(naked)]
    pub(crate) unsafe extern "C" fn sigsetjmp(
        _env: *mut SigJmpBuf,
        _savemask: libc::c_int,
    ) -> libc::c_int {
        // Windows x64 calling convention: RCX = env, RDX = savemask
        std::arch::naked_asm!(
            "mov [rcx + 0*8], rbx",
            "mov [rcx + 1*8], rbp",
            "mov [rcx + 2*8], rdi",
            "mov [rcx + 3*8], rsi",
            "mov [rcx + 4*8], r12",
            "mov [rcx + 5*8], r13",
            "mov [rcx + 6*8], r14",
            "mov [rcx + 7*8], r15",
            // Save RSP as it will be after our return (pop return addr).
            "lea rax, [rsp + 8]",
            "mov [rcx + 8*8], rax",
            // Save return address (top of stack on entry).
            "mov rax, [rsp]",
            "mov [rcx + 9*8], rax",
            // Return 0 (initial call).
            "xor eax, eax",
            "ret",
        );
    }

    /// Restore registers saved by `sigsetjmp` and jump back to the
    /// save point, making `sigsetjmp` return `val` (or 1 if val is 0).
    ///
    /// # Safety
    ///
    /// `env` must have been initialized by a prior `sigsetjmp` call whose
    /// frame is still live on the stack.
    #[unsafe(naked)]
    unsafe extern "C" fn longjmp(_env: *mut SigJmpBuf, _val: i32) -> ! {
        // Windows x64 calling convention: RCX = env, EDX = val
        std::arch::naked_asm!(
            "mov rbx, [rcx + 0*8]",
            "mov rbp, [rcx + 1*8]",
            "mov rdi, [rcx + 2*8]",
            "mov rsi, [rcx + 3*8]",
            "mov r12, [rcx + 4*8]",
            "mov r13, [rcx + 5*8]",
            "mov r14, [rcx + 6*8]",
            "mov r15, [rcx + 7*8]",
            "mov rsp, [rcx + 8*8]",
            // Return val (or 1 if val == 0).
            "mov eax, edx",
            "test eax, eax",
            "jnz 2f",
            "mov eax, 1",
            "2:",
            // Jump to saved return address (resumes after sigsetjmp call).
            "jmp [rcx + 9*8]",
        );
    }

    extern "system" {
        fn TlsAlloc() -> u32;
        fn TlsGetValue(index: u32) -> *mut c_void;
        fn TlsSetValue(index: u32, value: *mut c_void) -> i32;
        fn AddVectoredExceptionHandler(
            first: u32,
            handler: unsafe extern "system" fn(*mut ExceptionPointers) -> i32,
        ) -> *mut c_void;
    }

    const TLS_OUT_OF_INDEXES: u32 = 0xFFFFFFFF;

    // ── Per-worker recovery context ─────────────────────────────────────

    #[repr(C)]
    struct WorkerRecoveryCtx {
        jmp_buf: SigJmpBuf,
        /// Platform-independent recovery state.
        state: RecoveryState,
    }

    impl WorkerRecoveryCtx {
        fn new_boxed(worker_id: u32) -> Box<Self> {
            Box::new(Self {
                jmp_buf: SigJmpBuf::zeroed(),
                state: RecoveryState::new(worker_id),
            })
        }
    }

    static TLS_KEY: OnceLock<u32> = OnceLock::new();

    #[inline]
    unsafe fn get_recovery_ctx() -> *mut WorkerRecoveryCtx {
        let Some(&key) = TLS_KEY.get() else {
            return ptr::null_mut();
        };
        // SAFETY: `key` was allocated by `TlsAlloc` in `init_crash_handling`
        // and is valid for the lifetime of the process.
        unsafe { TlsGetValue(key) }.cast::<WorkerRecoveryCtx>()
    }

    // ── Vectored Exception Handler ──────────────────────────────────────

    unsafe extern "system" fn veh_handler(info: *mut ExceptionPointers) -> i32 {
        if info.is_null() {
            return EXCEPTION_CONTINUE_SEARCH;
        }
        // SAFETY: `info` is non-null (checked above). The OS guarantees
        // `exception_record` is a valid pointer within a VEH callback.
        let record = unsafe { &*(*info).exception_record };
        let code = record.exception_code;

        // Only handle crash-like exceptions.
        if code != EXCEPTION_ACCESS_VIOLATION
            && code != EXCEPTION_IN_PAGE_ERROR
            && code != EXCEPTION_INT_DIVIDE_BY_ZERO
            && code != EXCEPTION_ILLEGAL_INSTRUCTION
        {
            return EXCEPTION_CONTINUE_SEARCH;
        }

        // SAFETY: Retrieves the per-thread recovery context via TLS.
        // May return null (handled below).
        let ctx = unsafe { get_recovery_ctx() };
        if ctx.is_null() {
            return EXCEPTION_CONTINUE_SEARCH;
        }

        // SAFETY: `ctx` is non-null (checked above) and was allocated via
        // `Box::into_raw` in `init_worker_recovery`. Exclusive access is
        // guaranteed because each worker thread has its own TLS slot.
        let ctx = unsafe { &mut *ctx };

        // Re-entrancy guard.
        if ctx.state.in_recovery.swap(true, Ordering::Acquire) {
            return EXCEPTION_CONTINUE_SEARCH;
        }

        if !ctx.state.jmp_buf_valid.load(Ordering::Acquire) {
            ctx.state.in_recovery.store(false, Ordering::Release);
            return EXCEPTION_CONTINUE_SEARCH;
        }

        // Record crash metadata.
        ctx.state
            .crash_signal
            .store(exception_to_signal(code), Ordering::Release);
        ctx.state.fault_addr = record.exception_address as usize;
        ctx.state.jmp_buf_valid.store(false, Ordering::Release);

        // NOTE: We intentionally do NOT call longjmp here.
        // On Windows x64, longjmp from a VEH handler corrupts the SEH
        // unwind chain → STATUS_BAD_STACK (0xc0000028).
        // Intentional panics go through try_direct_longjmp() in
        // hew_panic() before the null dereference, so they never reach
        // this handler. Real crashes (actual bugs) propagate normally.
        ctx.state.in_recovery.store(false, Ordering::Release);
        EXCEPTION_CONTINUE_SEARCH
    }

    // ── Public API ──────────────────────────────────────────────────────

    pub(crate) fn init_crash_handling() {
        TLS_KEY.get_or_init(|| {
            // SAFETY: `TlsAlloc` has no preconditions; it allocates a new
            // TLS index from the OS. Called once via `OnceLock`.
            let key = unsafe { TlsAlloc() };
            assert!(key != TLS_OUT_OF_INDEXES, "TlsAlloc failed");
            key
        });

        // SAFETY: `veh_handler` is a valid function pointer with the correct
        // `PVECTORED_EXCEPTION_HANDLER` signature. Passing `first=1` inserts
        // it at the head of the VEH chain. Called once during initialization.
        unsafe {
            let h = AddVectoredExceptionHandler(1, veh_handler);
            assert!(!h.is_null(), "AddVectoredExceptionHandler failed");
        }
    }

    pub(crate) fn init_worker_recovery(worker_id: u32) {
        let ctx = WorkerRecoveryCtx::new_boxed(worker_id);
        let ctx_ptr = Box::into_raw(ctx); // ALLOCATOR-PAIRING: GlobalAlloc
        let key = *TLS_KEY
            .get()
            .expect("init_crash_handling must be called before init_worker_recovery");
        // SAFETY: `key` was allocated by `TlsAlloc`. `ctx_ptr` is from
        // `Box::into_raw` and is valid. Each worker calls this exactly once.
        let ret = unsafe { TlsSetValue(key, ctx_ptr.cast()) };
        assert!(ret != 0, "TlsSetValue failed");
    }

    pub(crate) unsafe fn prepare_dispatch_recovery(
        actor: *mut HewActor,
        msg: *mut c_void,
    ) -> *mut SigJmpBuf {
        // SAFETY: Retrieves the per-thread recovery context via TLS.
        // May return null (handled below).
        let ctx = unsafe { get_recovery_ctx() };
        if ctx.is_null() {
            return ptr::null_mut();
        }

        // SAFETY: `ctx` is non-null (checked above) and was allocated via
        // `Box::into_raw` in `init_worker_recovery`. Exclusive access is
        // guaranteed because each worker thread has its own TLS slot.
        let ctx = unsafe { &mut *ctx };

        // Store dispatch metadata via shared helper.
        // SAFETY: actor and msg validity guaranteed by caller.
        unsafe { super::shared::prepare_dispatch_impl(&mut ctx.state, actor, msg) };

        &raw mut ctx.jmp_buf
    }

    pub(crate) fn mark_recovery_active() {
        // SAFETY: Retrieves the per-thread recovery context via TLS.
        // May return null (handled below).
        let ctx = unsafe { get_recovery_ctx() };
        if ctx.is_null() {
            return;
        }
        // SAFETY: `ctx` is non-null (checked above) and was allocated via
        // `Box::into_raw` in `init_worker_recovery`. Exclusive access is
        // guaranteed because each worker thread has its own TLS slot.
        let ctx = unsafe { &mut *ctx };
        super::shared::mark_recovery_active_impl(&mut ctx.state);
    }

    pub(crate) unsafe fn handle_crash_recovery() -> (i32, usize) {
        // SAFETY: Retrieves the per-thread recovery context via TLS.
        // May return null (handled below).
        let ctx = unsafe { get_recovery_ctx() };
        if ctx.is_null() {
            return (0, 0);
        }

        // SAFETY: `ctx` is non-null (checked above) and was allocated via
        // `Box::into_raw` in `init_worker_recovery`. Exclusive access is
        // guaranteed because each worker thread has its own TLS slot.
        let ctx = unsafe { &mut *ctx };

        // SAFETY: called immediately after sigsetjmp returned non-zero.
        unsafe { super::shared::handle_crash_recovery_impl(&mut ctx.state) }
    }

    pub(crate) fn clear_dispatch_recovery() {
        // SAFETY: Retrieves the per-thread recovery context via TLS.
        // May return null (handled below).
        let ctx = unsafe { get_recovery_ctx() };
        if ctx.is_null() {
            return;
        }
        // SAFETY: `ctx` is non-null (checked above) and was allocated via
        // `Box::into_raw` in `init_worker_recovery`. Exclusive access is
        // guaranteed because each worker thread has its own TLS slot.
        let ctx = unsafe { &mut *ctx };
        super::shared::clear_dispatch_recovery_impl(&mut ctx.state);
    }

    /// Attempt direct longjmp recovery from an intentional panic.
    ///
    /// Called by `hew_panic()` BEFORE the null dereference. If a recovery
    /// context exists, longjmps directly from the actor's dispatch stack
    /// back to the scheduler. This avoids the VEH handler entirely
    /// (calling longjmp from a VEH handler causes STATUS_BAD_STACK on
    /// Windows x64).
    ///
    /// # Safety
    ///
    /// Must be called from a dispatch context (actor's stack frame chain
    /// includes the scheduler's sigsetjmp frame).
    pub(crate) unsafe fn try_direct_longjmp() {
        // SAFETY: Retrieves the per-thread recovery context via TLS.
        // May return null (handled below).
        let ctx = unsafe { get_recovery_ctx() };
        if ctx.is_null() {
            return;
        }
        // SAFETY: `ctx` is non-null (checked above) and was allocated via
        // `Box::into_raw` in `init_worker_recovery`. Exclusive access is
        // guaranteed because each worker thread has its own TLS slot.
        let ctx = unsafe { &mut *ctx };
        if !super::shared::try_direct_longjmp_preamble(&mut ctx.state) {
            return;
        }
        // SAFETY: `jmp_buf` was set by `sigsetjmp` in `activate_actor` on the
        // same thread. `try_direct_longjmp_preamble` confirmed the buffer is
        // valid and marked it consumed. The longjmp unwinds back to the
        // scheduler's recovery frame.
        unsafe { longjmp(&raw mut ctx.jmp_buf, 1) };
    }

    /// Like [`try_direct_longjmp`] but records `code` as the crash reason
    /// instead of the default SIGSEGV (11) marker.
    ///
    /// # Safety
    ///
    /// Must be called from a dispatch context on the worker thread.
    pub(crate) unsafe fn try_direct_longjmp_with_code(code: i32) {
        // SAFETY: Retrieves the per-thread recovery context via TLS.
        // May return null (handled below).
        let ctx = unsafe { get_recovery_ctx() };
        if ctx.is_null() {
            return;
        }
        // SAFETY: `ctx` is non-null (checked above) and exclusively owned by
        // this thread.
        let ctx = unsafe { &mut *ctx };
        if !super::shared::try_direct_longjmp_preamble_with_code(&mut ctx.state, code) {
            return;
        }
        // SAFETY: `jmp_buf` was set by `sigsetjmp` in `activate_actor` on the
        // same thread. The longjmp unwinds back to the scheduler's recovery frame.
        unsafe { longjmp(&raw mut ctx.jmp_buf, 1) };
    }
}

// ── WASM stubs ──────────────────────────────────────────────────────────

#[cfg(not(any(unix, windows)))]
mod platform {
    use std::ffi::c_void;

    /// Stub jmp_buf for WASM.
    #[repr(C, align(16))]
    pub(crate) struct SigJmpBuf {
        _buf: [u8; 256],
    }

    pub(crate) fn init_crash_handling() {}
    pub(crate) fn init_worker_recovery(_worker_id: u32) {}

    /// # Safety
    ///
    /// No-op on WASM. Always returns null.
    pub(crate) unsafe fn prepare_dispatch_recovery(
        _actor: *mut crate::actor::HewActor,
        _msg: *mut c_void,
    ) -> *mut SigJmpBuf {
        std::ptr::null_mut()
    }

    pub(crate) fn mark_recovery_active() {}

    /// Stub sigsetjmp — always returns 0.
    ///
    /// # Safety
    ///
    /// No-op on WASM.
    pub(crate) unsafe fn sigsetjmp(_env: *mut SigJmpBuf, _savemask: libc::c_int) -> libc::c_int {
        0
    }

    /// # Safety
    ///
    /// No-op on WASM.
    pub(crate) unsafe fn handle_crash_recovery() -> (i32, usize) {
        (0, 0)
    }

    pub(crate) fn clear_dispatch_recovery() {}

    /// No-op on WASM — no crash recovery.
    ///
    /// # Safety
    ///
    /// No-op on WASM.
    pub(crate) unsafe fn try_direct_longjmp() {}

    /// No-op on WASM — no crash recovery; arena null propagates to caller.
    ///
    /// On WASM there is no signal-based longjmp seam. Cap-exhaustion nulls
    /// propagate as a null pointer; the Hew runtime's WASM panic (`catch_unwind`)
    /// path handles the resulting trap if the caller dereferences the null.
    ///
    /// # Safety
    ///
    /// No-op on WASM.
    pub(crate) unsafe fn try_direct_longjmp_with_code(_code: i32) {}
}

// Re-export platform-specific implementations.
pub(crate) use platform::{
    clear_dispatch_recovery, handle_crash_recovery, init_crash_handling, init_worker_recovery,
    mark_recovery_active, prepare_dispatch_recovery, sigsetjmp, try_direct_longjmp,
    try_direct_longjmp_with_code,
};

// ── Terminal off-dispatch crash diagnostic (subprocess test) ─────────────────

/// End-to-end proof that an off-dispatch fault (a thread with NO recovery
/// `jmp_buf` — exactly the I/O accept / connection-reader / SWIM-driver threads
/// in the distributed runtime) terminates with a diagnostic that pins the
/// faulting ADDRESS and THREAD NAME, not just the signal. This is the surface
/// that turns issue-#1963's opaque "crash in main context: SIGSEGV" on a CI
/// runner into a localized fault site on its next occurrence.
///
/// The crash path `_exit`s the process, so it can only be exercised in a child:
/// the child helper installs the crash handler, spawns a named thread, and
/// dereferences a wild pointer there; the parent re-execs that helper and
/// asserts the captured stderr.
#[cfg(all(test, unix, not(target_arch = "wasm32")))]
mod terminal_diag_tests {
    const HELPER_ENV: &str = "HEW_SIGNAL_TERMINAL_DIAG_HELPER";
    const FAULT_THREAD_NAME: &str = "hew-conn-diag";
    // A fixed wild address in the unmapped low region: deterministic in the
    // diagnostic output and reliably faults on read across unix targets.
    const FAULT_ADDR: usize = 0x28;

    /// Child helper: install the crash handler on the main thread (so the
    /// process-wide signal disposition is set), then fault on a *named* thread
    /// that never installed a per-thread recovery context. The handler's
    /// no-recovery branch must emit the terminal diagnostic and `_exit(139)`.
    #[test]
    fn signal_terminal_diag_helper() {
        if std::env::var(HELPER_ENV).as_deref() != Ok("1") {
            return;
        }
        // Install the process signal handlers. The main thread also installs a
        // per-thread recovery context inside init; the spawned thread below does
        // NOT, so its fault takes the no-recovery terminal path.
        super::init_crash_handling();

        let handle = std::thread::Builder::new()
            .name(FAULT_THREAD_NAME.to_owned())
            .spawn(|| {
                let wild = FAULT_ADDR as *const u8;
                // SAFETY: intentional wild read to drive the fault on an
                // off-dispatch thread; the process is expected to terminate.
                let v = unsafe { std::ptr::read_volatile(wild) };
                // Prevent the read from being optimized away.
                std::process::exit(i32::from(v));
            })
            .expect("spawn fault thread");
        let _ = handle.join();
        // Reaching here means the fault did not terminate the process — fail
        // loudly so the parent's assertion surfaces the regression.
        std::process::exit(0);
    }

    #[test]
    fn off_dispatch_fault_diagnostic_pins_address_and_thread() {
        let exe = std::env::current_exe().expect("test binary path");
        let output = std::process::Command::new(exe)
            .args([
                "--exact",
                "signal::terminal_diag_tests::signal_terminal_diag_helper",
                "--nocapture",
            ])
            .env(HELPER_ENV, "1")
            .env("RUST_TEST_THREADS", "1")
            .output()
            .expect("spawn signal terminal-diag helper");

        let stderr = String::from_utf8_lossy(&output.stderr);
        let code = output.status.code();

        // Terminal off-dispatch fault: SIGSEGV (or SIGBUS on some targets) →
        // _exit(128 + sig). 139 = 128 + 11 (SIGSEGV); 135 = 128 + 7 (SIGBUS).
        assert!(
            matches!(code, Some(139 | 135)),
            "expected terminal _exit(128+SIGSEGV/SIGBUS); got {code:?}\nstderr:\n{stderr}"
        );
        assert!(
            stderr.contains("hew: crash in main context:"),
            "diagnostic prefix missing\nstderr:\n{stderr}"
        );
        // The fault ADDRESS must be pinned (the load-bearing localization). The
        // exact wild address may be sanitized by the kernel on some targets, so
        // accept either the precise address or any ` at 0x…` token.
        assert!(
            stderr.contains(" at 0x"),
            "diagnostic must carry the faulting address\nstderr:\n{stderr}"
        );
        // The faulting THREAD NAME must be pinned so a multi-threaded runtime
        // crash names its origin thread.
        assert!(
            stderr.contains(&format!("thread={FAULT_THREAD_NAME}")),
            "diagnostic must name the faulting thread `{FAULT_THREAD_NAME}`\nstderr:\n{stderr}"
        );
    }
}
