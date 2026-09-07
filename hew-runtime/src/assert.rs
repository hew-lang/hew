//! Hew runtime: `assert` module.
//!
//! The boolean `assert` shim exposed with C ABI for compiled Hew programs.
//! `assert_eq` and `assert_ne` are normalized in HIR into an ordinary
//! comparison and `panic`, so they need no runtime helper of their own.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

/// Assert that a condition (as `i64`) is truthy (non-zero).
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_assert(cond: u8) {
    if cond == 0 {
        eprintln!("assertion failed");
        // SAFETY: abort() is always safe to call.
        unsafe { libc::abort() };
    }
}
