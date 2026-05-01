//! Rust FFI bridge for the in-process LLJIT evaluation path.
//!
//! Wraps the `HewJitSession` C ABI exposed by hew-codegen so the rest of
//! hew-cli can call it from safe Rust.
//!
//! Design: each call to `run_jit` creates a fresh C++ `HewJitSession`, hands
//! it the MessagePack-encoded AST, calls main, and destroys the session.
//! No cross-cell symbol sharing.
//!
//! SHIM: synchronous, one-session-per-cell — the minimal M1 keystone (#1235).
//! WHY: prevents state from leaking between REPL cells while the crash-
//! survivability seam (#1227) is not yet in place.
//! WHEN obsolete: when #1228 introduces a Runtime handle with a persistent
//! JIT session across cells.
//! WHAT the real solution looks like: a long-lived `JitSession` that accumulates
//! symbol tables, wrapped in `catch_unwind` (#1227).

#[cfg(hew_embedded_codegen)]
use std::ffi::CStr;

/// Errors that can arise during in-process JIT execution.
///
/// Variants are constructed inside the `hew_embedded_codegen` cfg branch
/// and consumed via `Display`; the type-system flags them as dead in a
/// non-codegen build. Kept on the public surface so #1227's worker-mode
/// trampoline and future `ORCv2` wiring can pattern-match on the same
/// shape.
#[allow(
    dead_code,
    reason = "constructed inside hew_embedded_codegen cfg; public surface for #1227/#1232 trampolines"
)]
#[derive(Debug, Clone)]
pub enum JitError {
    /// JIT session creation failed.
    SessionCreateFailed(String),
    /// Compilation or execution of the program failed.
    ExecFailed(String),
    /// A Rust panic propagated out of the JIT trampoline.
    ///
    /// Only reachable when the process panic strategy is `unwind` (test
    /// profiles).  In dev/release builds (`panic = "abort"`) the process
    /// aborts before `catch_unwind` gets a chance to catch anything.
    PanicCaught(String),
}

impl std::fmt::Display for JitError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SessionCreateFailed(msg) => write!(f, "JIT session creation failed: {msg}"),
            Self::ExecFailed(msg) => write!(f, "JIT execution failed: {msg}"),
            Self::PanicCaught(msg) => write!(f, "JIT trampoline caught a panic: {msg}"),
        }
    }
}

impl std::error::Error for JitError {}

// -- C ABI declarations -------------------------------------------------------

#[cfg(hew_embedded_codegen)]
#[repr(C)]
struct HewJitSessionOpaque {
    _private: [u8; 0],
}

#[cfg(hew_embedded_codegen)]
unsafe extern "C" {
    fn hew_jit_session_create() -> *mut HewJitSessionOpaque;

    fn hew_jit_session_eval_msgpack(
        session: *mut HewJitSessionOpaque,
        data: *const u8,
        size: usize,
        out_exit_code: *mut i64,
    ) -> std::ffi::c_int;

    fn hew_jit_session_destroy(session: *mut HewJitSessionOpaque);

    fn hew_jit_session_last_error() -> *const std::ffi::c_char;
}

// -- Safe wrapper -------------------------------------------------------------

/// Read the C-side last-error string.
///
/// Must only be called immediately after a failed JIT ABI call and before
/// any other call that may clobber the thread-local error buffer.
#[cfg(hew_embedded_codegen)]
fn last_jit_error() -> String {
    // SAFETY: hew_jit_session_last_error returns a pointer to a thread-local
    // C string valid until the next JIT ABI call on this thread.
    let ptr = unsafe { hew_jit_session_last_error() };
    if ptr.is_null() {
        "JIT operation failed (no error message)".into()
    } else {
        // SAFETY: ptr is non-null and NUL-terminated per the C ABI contract.
        unsafe { CStr::from_ptr(ptr) }
            .to_string_lossy()
            .into_owned()
    }
}

// -- Drop guard ---------------------------------------------------------------

/// Owns a `HewJitSession` pointer and destroys it on drop.
///
/// This ensures `hew_jit_session_destroy` is called even when a panic unwinds
/// through `run_jit` (e.g. in test profiles where `panic = "unwind"`).
#[cfg(hew_embedded_codegen)]
struct JitSessionGuard(*mut HewJitSessionOpaque);

#[cfg(hew_embedded_codegen)]
impl Drop for JitSessionGuard {
    fn drop(&mut self) {
        if !self.0.is_null() {
            // SAFETY: pointer was returned by hew_jit_session_create and has
            // not yet been freed.  This is the only call site for destroy on
            // this pointer.
            unsafe { hew_jit_session_destroy(self.0) };
        }
    }
}

// -- Public entry point -------------------------------------------------------

/// Run a pre-serialised `MessagePack` AST via LLJIT in the current process.
///
/// Returns `Ok(exit_code)` where `exit_code` is the i64 return value of
/// `main` (0 when main is void).
///
/// The JIT'd code runs in-process: output from print/println goes directly
/// to the parent's stdout fd and is not captured.
///
/// ## Panic recovery
///
/// The call into JIT-emitted code is wrapped in `std::panic::catch_unwind`.
/// If a Rust panic propagates out of the trampoline the session is destroyed
/// (via [`JitSessionGuard`]) and `Err(JitError::PanicCaught)` is returned
/// instead of crashing the host process.
///
/// SHIM: `catch_unwind` only works in profiles where `panic = "unwind"` (i.e.
/// test builds).  In dev and release builds the workspace uses `panic = "abort"`,
/// so a panic terminates the process before `catch_unwind` can intervene.
/// WHY this shim exists: the M1 trusted-local bet (#1235) runs JIT in-process
/// because no IPC worker is yet available; `catch_unwind` is the best available
/// containment at this layer.
/// WHEN obsolete: when the `--jit=worker` path (#1227) proves out and becomes
/// the default, so crashes are isolated to a child process rather than relying
/// on unwind.
/// WHAT the real solution looks like: move to the `--jit=worker` AOT+spawn
/// path as the default; keep `--jit=inprocess` as an opt-in for developers.
///
/// SHIM: stdout is not captured — the JIT path writes to the parent fd
/// directly.  WHY: the synchronous single-cell model does not need captured
/// output for the M1 warm-path.
/// WHEN obsolete: when #1227 adds output redirection for the interactive REPL
/// path.
#[cfg(hew_embedded_codegen)]
pub fn run_jit(msgpack_data: &[u8]) -> Result<i64, JitError> {
    // SAFETY: hew_jit_session_create returns a valid heap-allocated pointer
    // or null with an error string.
    let session = unsafe { hew_jit_session_create() };
    if session.is_null() {
        return Err(JitError::SessionCreateFailed(last_jit_error()));
    }

    // The guard ensures hew_jit_session_destroy is called even when a panic
    // unwinds through this frame (test profiles with panic = "unwind").
    let guard = JitSessionGuard(session);

    let mut exit_code: i64 = 0;

    // SHIM: catch_unwind wraps the call into JIT-emitted code.
    // WHY: the M1 trusted-local bet runs JIT in-process; catch_unwind is the
    //   minimal survivability seam while the worker-process path is not ready.
    // WHEN obsolete: when --jit=worker becomes the default (#1227).
    // WHAT the real solution: worker-process isolation via --jit=worker.
    //
    // SAFETY note for AssertUnwindSafe: `session` (via `guard.0`) is a raw
    // pointer to C++ heap memory.  Capturing it in an unwind-safe closure is
    // acceptable because:
    //   (a) on panic the Drop impl on `guard` calls hew_jit_session_destroy,
    //       which is the sole owner of the C++ session object, so no double-free
    //       or use-after-free can occur;
    //   (b) `exit_code` is a plain i64 on the stack — trivially unwind-safe;
    //   (c) `msgpack_data` is a shared reference — trivially unwind-safe.
    // The invariant that matters is that `guard` is the sole owner and its Drop
    // is always called exactly once, which the borrow checker guarantees here.
    let catch_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        // SAFETY: session (guard.0) is non-null. msgpack_data is valid for the
        // call duration. exit_code is a valid local out-parameter.
        unsafe {
            hew_jit_session_eval_msgpack(
                guard.0,
                msgpack_data.as_ptr(),
                msgpack_data.len(),
                &raw mut exit_code,
            )
        }
    }));

    // `guard` is dropped here (or on panic-unwind above), calling
    // hew_jit_session_destroy exactly once in both paths.
    drop(guard);

    match catch_result {
        Ok(0) => Ok(exit_code),
        Ok(_) => Err(JitError::ExecFailed(last_jit_error())),
        Err(payload) => {
            let msg = payload
                .downcast_ref::<&str>()
                .copied()
                .or_else(|| payload.downcast_ref::<String>().map(String::as_str))
                .unwrap_or("unknown panic payload");
            Err(JitError::PanicCaught(msg.to_owned()))
        }
    }
}

/// Stub for builds without the embedded codegen backend.
#[cfg(not(hew_embedded_codegen))]
pub fn run_jit(_msgpack_data: &[u8]) -> Result<i64, JitError> {
    Err(JitError::ExecFailed(
        "JIT evaluation requires the embedded MLIR/LLVM backend; \
         rebuild with LLVM_PREFIX or LLVM_DIR/MLIR_DIR configured"
            .into(),
    ))
}
