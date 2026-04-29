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
}

impl std::fmt::Display for JitError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SessionCreateFailed(msg) => write!(f, "JIT session creation failed: {msg}"),
            Self::ExecFailed(msg) => write!(f, "JIT execution failed: {msg}"),
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

/// Run a pre-serialised `MessagePack` AST via LLJIT in the current process.
///
/// Returns `Ok(exit_code)` where `exit_code` is the i64 return value of
/// `main` (0 when main is void).
///
/// The JIT'd code runs in-process: output from print/println goes directly
/// to the parent's stdout fd and is not captured.
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

    let mut exit_code: i64 = 0;

    // SAFETY: session is non-null. msgpack_data is valid for the call duration.
    // exit_code is a valid local out-parameter.
    let status = unsafe {
        hew_jit_session_eval_msgpack(
            session,
            msgpack_data.as_ptr(),
            msgpack_data.len(),
            &raw mut exit_code,
        )
    };

    // Destroy the session before returning regardless of success or failure.
    // SAFETY: session is non-null and not yet freed.
    unsafe { hew_jit_session_destroy(session) };

    if status != 0 {
        return Err(JitError::ExecFailed(last_jit_error()));
    }

    Ok(exit_code)
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
