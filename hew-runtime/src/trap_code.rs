//! Target-shared trap-code helpers.

use std::ffi::c_int;
use std::sync::atomic::Ordering;

/// Stamp `code` on the actor in the current execution context.
///
/// Returns `true` when an actor lane was present and updated. Returns `false`
/// outside actor dispatch so callers can preserve their target-specific
/// non-actor fallback (native `llvm.trap`, WASM `llvm.trap`, or explicit panic).
#[cfg_attr(
    not(any(target_arch = "wasm32", test)),
    allow(
        dead_code,
        reason = "native exports hew_trap_with_code from supervisor.rs; this helper is used by wasm32 and native parity tests"
    )
)]
pub(crate) fn stamp_current_actor_error_code(code: c_int) -> bool {
    let ctx = crate::execution_context::current_context();
    if ctx.is_null() {
        return false;
    }

    // SAFETY: a non-null canonical context points to the live activation frame
    // installed by the scheduler. The actor lane may still be null for
    // non-actor contexts, which is handled below.
    let actor = unsafe { (*ctx).actor };
    if actor.is_null() {
        return false;
    }

    // SAFETY: the context actor lane is the actor currently being dispatched;
    // schedulers observe this code when transitioning the actor to Crashed.
    unsafe {
        (*actor).error_code.store(code, Ordering::Release);
    }
    true
}

/// WASM trap-code bridge used by codegen before `llvm.trap`.
///
/// Native implements the exported symbol in `supervisor.rs` because it routes
/// through the longjmp recovery seam. wasm32 has no longjmp seam, so the bridge
/// stamps the current actor's `error_code` and panics; the cooperative
/// scheduler's `catch_unwind` activation boundary transitions the actor to
/// `Crashed` and leaves the discriminator observable through
/// `ExitReason::from_error_code`.
///
/// Outside an actor context this returns so the caller's following `llvm.trap`
/// remains the fail-closed non-actor fallback, matching native.
///
/// # Safety
///
/// May be called from generated code in or out of actor dispatch. The current
/// execution context, if present, is scheduler-installed and valid for the
/// duration of the call.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_trap_with_code(code: c_int) {
    if stamp_current_actor_error_code(code) {
        panic!("hew_trap_with_code: trap code {code}");
    }
}
