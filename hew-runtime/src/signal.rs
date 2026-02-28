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

// ── Unix implementation (Linux + macOS) ─────────────────────────────────

#[cfg(unix)]
mod platform {
    use std::ffi::c_void;
    use std::ptr;
    use std::sync::atomic::{AtomicBool, AtomicI32, Ordering};
    use std::sync::OnceLock;

    use crate::actor::HewActor;

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
    ///
    /// # Layout
    ///
    /// All fields are written by the normal code path and read by the
    /// signal handler (same thread, so no cross-thread races).
    #[repr(C)]
    struct WorkerRecoveryCtx {
        /// `sigsetjmp` save buffer.
        jmp_buf: SigJmpBuf,
        /// Whether `jmp_buf` contains a valid recovery point.
        jmp_buf_valid: AtomicBool,
        /// Pointer to the actor currently being dispatched.
        current_actor: *mut HewActor,
        /// Pointer to the current message node (for cleanup).
        current_msg: *mut c_void,
        /// Signal number that caused the crash (set by handler).
        crash_signal: AtomicI32,
        /// Fault address from `siginfo_t` (set by handler).
        fault_addr: usize,
        /// Re-entrancy guard — prevents nested signal recovery.
        in_recovery: AtomicBool,
        /// Worker thread ID for crash reporting.
        worker_id: u32,
        /// Message type being processed when crash occurred.
        msg_type: AtomicI32,
    }

    impl WorkerRecoveryCtx {
        fn new_boxed(worker_id: u32) -> Box<Self> {
            Box::new(Self {
                jmp_buf: SigJmpBuf::zeroed(),
                jmp_buf_valid: AtomicBool::new(false),
                current_actor: ptr::null_mut(),
                current_msg: ptr::null_mut(),
                crash_signal: AtomicI32::new(0),
                fault_addr: 0,
                in_recovery: AtomicBool::new(false),
                worker_id,
                msg_type: AtomicI32::new(0),
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
            drop(unsafe { Box::from_raw(ptr.cast::<WorkerRecoveryCtx>()) });
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
            // No recovery context — can't recover. Terminate immediately.
            // SAFETY: _exit is async-signal-safe.
            unsafe { libc::_exit(128 + sig) };
        }

        // SAFETY: ctx is valid (created in init_worker_recovery, same thread).
        let ctx = unsafe { &mut *ctx };

        // Re-entrancy check: if we're already recovering from a signal,
        // a second crash means the recovery path itself is broken.
        if ctx.in_recovery.swap(true, Ordering::Acquire) {
            // SAFETY: _exit is async-signal-safe.
            unsafe { libc::_exit(128 + sig) };
        }

        // Check that we have a valid recovery point.
        if !ctx.jmp_buf_valid.load(Ordering::Acquire) {
            ctx.in_recovery.store(false, Ordering::Release);
            // SAFETY: _exit is async-signal-safe.
            unsafe { libc::_exit(128 + sig) };
        }

        // Record crash metadata.
        ctx.crash_signal.store(sig, Ordering::Release);
        if !info.is_null() {
            // SAFETY: info is valid in signal context. si_addr accesses
            // the siginfo_t field (async-signal-safe read).
            // On Linux it's a method, on macOS it's a field.
            #[cfg(target_os = "linux")]
            {
                ctx.fault_addr = unsafe { (*info).si_addr() } as usize;
            }
            #[cfg(not(target_os = "linux"))]
            {
                ctx.fault_addr = unsafe { (*info).si_addr } as usize;
            }
        }

        // Invalidate the jump buffer to prevent re-use.
        ctx.jmp_buf_valid.store(false, Ordering::Release);

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
        let crash_signals = [libc::SIGSEGV, libc::SIGBUS, libc::SIGFPE, libc::SIGILL];

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
        let ctx_ptr = Box::into_raw(ctx);
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

        // Store dispatch metadata for crash cleanup.
        ctx.current_actor = actor;
        ctx.current_msg = msg;
        ctx.crash_signal.store(0, Ordering::Relaxed);
        ctx.fault_addr = 0;
        ctx.in_recovery.store(false, Ordering::Release);

        // Extract message type from HewMsgNode for crash reporting.
        let msg_type = if msg.is_null() {
            0
        } else {
            // SAFETY: msg is valid HewMsgNode from hew_mailbox_try_recv.
            // We cast c_void back to HewMsgNode to read msg_type.
            unsafe { (*(msg.cast::<crate::mailbox::HewMsgNode>())).msg_type }
        };
        ctx.msg_type.store(msg_type, Ordering::Relaxed);

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
        ctx.jmp_buf_valid.store(true, Ordering::Release);
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

        let signal = ctx.crash_signal.load(Ordering::Acquire);
        let fault_addr = ctx.fault_addr;
        let actor = ctx.current_actor;
        let msg_type = ctx.msg_type.load(Ordering::Acquire);
        let worker_id = ctx.worker_id;

        // Notify supervisor by marking actor as Crashed.
        if !actor.is_null() {
            // SAFETY: actor pointer was stored in prepare_dispatch_recovery
            // and the actor is still alive (it's Running — only the current
            // worker thread can transition it, and we haven't freed it).
            unsafe { crate::actor::hew_actor_trap(actor, signal) };
        }

        // Build detailed crash report for forensics.
        // SAFETY: actor pointer is valid (checked above).
        let report = unsafe {
            crate::crash::build_crash_report(
                actor, signal,
                0, // signal_code - not available from siginfo_t in current handler
                fault_addr, msg_type, worker_id,
            )
        };

        // Push to global crash log.
        crate::crash::push_crash_report(report);

        // Enhanced crash logging with more details.
        let signal_name = match signal {
            libc::SIGSEGV => "SIGSEGV",
            libc::SIGBUS => "SIGBUS",
            libc::SIGFPE => "SIGFPE",
            libc::SIGILL => "SIGILL",
            _ => "UNKNOWN",
        };
        if !actor.is_null() {
            // SAFETY: actor is valid (see above).
            {
                // SAFETY: actor pointer is valid (checked above).
                let (id, pid) = unsafe { ((*actor).id, (*actor).pid) };
                eprintln!(
                    "hew: actor {id} (pid={pid}) crashed with {signal_name} at {fault_addr:#x}, msg_type={msg_type}, worker={worker_id}"
                );
            }
        }

        // Clear recovery context.
        ctx.current_actor = ptr::null_mut();
        ctx.current_msg = ptr::null_mut();
        ctx.in_recovery.store(false, Ordering::Release);

        (signal, fault_addr)
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
        ctx.jmp_buf_valid.store(false, Ordering::Release);
        ctx.current_actor = ptr::null_mut();
        ctx.current_msg = ptr::null_mut();
        ctx.msg_type.store(0, Ordering::Relaxed);
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
        let ctx = unsafe { get_recovery_ctx() };
        if ctx.is_null() {
            return;
        }
        let ctx = unsafe { &mut *ctx };
        if !ctx.jmp_buf_valid.load(Ordering::Acquire) {
            return;
        }
        if ctx.in_recovery.swap(true, Ordering::Acquire) {
            return;
        }
        // Record intentional panic as SIGSEGV equivalent.
        ctx.crash_signal.store(libc::SIGSEGV, Ordering::Release);
        ctx.fault_addr = 0;
        ctx.jmp_buf_valid.store(false, Ordering::Release);
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
    use std::sync::atomic::{AtomicBool, AtomicI32, Ordering};
    use std::sync::OnceLock;

    use crate::actor::HewActor;

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
        jmp_buf_valid: AtomicBool,
        current_actor: *mut HewActor,
        current_msg: *mut c_void,
        crash_signal: AtomicI32,
        fault_addr: usize,
        in_recovery: AtomicBool,
        worker_id: u32,
        msg_type: AtomicI32,
    }

    impl WorkerRecoveryCtx {
        fn new_boxed(worker_id: u32) -> Box<Self> {
            Box::new(Self {
                jmp_buf: SigJmpBuf::zeroed(),
                jmp_buf_valid: AtomicBool::new(false),
                current_actor: ptr::null_mut(),
                current_msg: ptr::null_mut(),
                crash_signal: AtomicI32::new(0),
                fault_addr: 0,
                in_recovery: AtomicBool::new(false),
                worker_id,
                msg_type: AtomicI32::new(0),
            })
        }
    }

    static TLS_KEY: OnceLock<u32> = OnceLock::new();

    #[inline]
    unsafe fn get_recovery_ctx() -> *mut WorkerRecoveryCtx {
        let Some(&key) = TLS_KEY.get() else {
            return ptr::null_mut();
        };
        unsafe { TlsGetValue(key) }.cast::<WorkerRecoveryCtx>()
    }

    // ── Vectored Exception Handler ──────────────────────────────────────

    unsafe extern "system" fn veh_handler(info: *mut ExceptionPointers) -> i32 {
        if info.is_null() {
            return EXCEPTION_CONTINUE_SEARCH;
        }
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

        let ctx = unsafe { get_recovery_ctx() };
        if ctx.is_null() {
            return EXCEPTION_CONTINUE_SEARCH;
        }

        let ctx = unsafe { &mut *ctx };

        // Re-entrancy guard.
        if ctx.in_recovery.swap(true, Ordering::Acquire) {
            return EXCEPTION_CONTINUE_SEARCH;
        }

        if !ctx.jmp_buf_valid.load(Ordering::Acquire) {
            ctx.in_recovery.store(false, Ordering::Release);
            return EXCEPTION_CONTINUE_SEARCH;
        }

        // Record crash metadata.
        ctx.crash_signal
            .store(exception_to_signal(code), Ordering::Release);
        ctx.fault_addr = record.exception_address as usize;
        ctx.jmp_buf_valid.store(false, Ordering::Release);

        // NOTE: We intentionally do NOT call longjmp here.
        // On Windows x64, longjmp from a VEH handler corrupts the SEH
        // unwind chain → STATUS_BAD_STACK (0xc0000028).
        // Intentional panics go through try_direct_longjmp() in
        // hew_panic() before the null dereference, so they never reach
        // this handler. Real crashes (actual bugs) propagate normally.
        ctx.in_recovery.store(false, Ordering::Release);
        EXCEPTION_CONTINUE_SEARCH
    }

    // ── Public API ──────────────────────────────────────────────────────

    pub(crate) fn init_crash_handling() {
        TLS_KEY.get_or_init(|| {
            let key = unsafe { TlsAlloc() };
            assert!(key != TLS_OUT_OF_INDEXES, "TlsAlloc failed");
            key
        });

        // Register VEH handler (first=1 → called before SEH frames).
        unsafe {
            let h = AddVectoredExceptionHandler(1, veh_handler);
            assert!(!h.is_null(), "AddVectoredExceptionHandler failed");
        }
    }

    pub(crate) fn init_worker_recovery(worker_id: u32) {
        let ctx = WorkerRecoveryCtx::new_boxed(worker_id);
        let ctx_ptr = Box::into_raw(ctx);
        let key = *TLS_KEY
            .get()
            .expect("init_crash_handling must be called before init_worker_recovery");
        let ret = unsafe { TlsSetValue(key, ctx_ptr.cast()) };
        assert!(ret != 0, "TlsSetValue failed");
    }

    pub(crate) unsafe fn prepare_dispatch_recovery(
        actor: *mut HewActor,
        msg: *mut c_void,
    ) -> *mut SigJmpBuf {
        let ctx = unsafe { get_recovery_ctx() };
        if ctx.is_null() {
            return ptr::null_mut();
        }

        let ctx = unsafe { &mut *ctx };
        ctx.current_actor = actor;
        ctx.current_msg = msg;
        ctx.crash_signal.store(0, Ordering::Relaxed);
        ctx.fault_addr = 0;
        ctx.in_recovery.store(false, Ordering::Release);

        let msg_type = if msg.is_null() {
            0
        } else {
            unsafe { (*(msg.cast::<crate::mailbox::HewMsgNode>())).msg_type }
        };
        ctx.msg_type.store(msg_type, Ordering::Relaxed);

        &raw mut ctx.jmp_buf
    }

    pub(crate) fn mark_recovery_active() {
        let ctx = unsafe { get_recovery_ctx() };
        if ctx.is_null() {
            return;
        }
        let ctx = unsafe { &mut *ctx };
        ctx.jmp_buf_valid.store(true, Ordering::Release);
    }

    pub(crate) unsafe fn handle_crash_recovery() -> (i32, usize) {
        let ctx = unsafe { get_recovery_ctx() };
        if ctx.is_null() {
            return (0, 0);
        }

        let ctx = unsafe { &mut *ctx };

        let signal = ctx.crash_signal.load(Ordering::Acquire);
        let fault_addr = ctx.fault_addr;
        let actor = ctx.current_actor;
        let msg_type = ctx.msg_type.load(Ordering::Acquire);
        let worker_id = ctx.worker_id;

        if !actor.is_null() {
            unsafe { crate::actor::hew_actor_trap(actor, signal) };
        }

        let report = unsafe {
            crate::crash::build_crash_report(actor, signal, 0, fault_addr, msg_type, worker_id)
        };
        crate::crash::push_crash_report(report);

        let signal_name = match signal {
            11 => "ACCESS_VIOLATION",
            8 => "INT_DIVIDE_BY_ZERO",
            4 => "ILLEGAL_INSTRUCTION",
            _ => "UNKNOWN",
        };
        if !actor.is_null() {
            let (id, pid) = unsafe { ((*actor).id, (*actor).pid) };
            eprintln!(
                "hew: actor {id} (pid={pid}) crashed with {signal_name} at {fault_addr:#x}, msg_type={msg_type}, worker={worker_id}"
            );
        }

        ctx.current_actor = ptr::null_mut();
        ctx.current_msg = ptr::null_mut();
        ctx.in_recovery.store(false, Ordering::Release);

        (signal, fault_addr)
    }

    pub(crate) fn clear_dispatch_recovery() {
        let ctx = unsafe { get_recovery_ctx() };
        if ctx.is_null() {
            return;
        }
        let ctx = unsafe { &mut *ctx };
        ctx.jmp_buf_valid.store(false, Ordering::Release);
        ctx.current_actor = ptr::null_mut();
        ctx.current_msg = ptr::null_mut();
        ctx.msg_type.store(0, Ordering::Relaxed);
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
        let ctx = unsafe { get_recovery_ctx() };
        if ctx.is_null() {
            return;
        }
        let ctx = unsafe { &mut *ctx };
        if !ctx.jmp_buf_valid.load(Ordering::Acquire) {
            return;
        }
        if ctx.in_recovery.swap(true, Ordering::Acquire) {
            return;
        }
        // Record intentional panic as SIGSEGV equivalent.
        ctx.crash_signal.store(11, Ordering::Release);
        ctx.fault_addr = 0;
        ctx.jmp_buf_valid.store(false, Ordering::Release);
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
}

// Re-export platform-specific implementations.
pub(crate) use platform::{
    clear_dispatch_recovery, handle_crash_recovery, init_crash_handling, init_worker_recovery,
    mark_recovery_active, prepare_dispatch_recovery, sigsetjmp, try_direct_longjmp,
};
