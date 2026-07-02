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

/// Route a runtime-internal bounds violation through the target trap seam.
///
/// Codegen-emitted traps pair `hew_trap_with_code` with a following machine
/// trap for non-actor contexts. Runtime FFI entry points do not have that
/// generated fallback, so this helper aborts if the trap call returns.
///
/// # Safety
///
/// Call only from a fail-closed bounds/precondition path where execution must
/// not continue after the diagnostic has been emitted.
#[cfg_attr(
    not(any(test, target_arch = "wasm32")),
    allow(dead_code, reason = "call sites are wired in subsequent runtime stages")
)]
pub(crate) unsafe fn runtime_bounds_trap(code: c_int) -> ! {
    #[cfg(not(target_arch = "wasm32"))]
    {
        // SAFETY: the native trap bridge checks whether an actor recovery
        // context is active and returns only when no recovery context exists.
        unsafe { crate::supervisor::hew_trap_with_code(code) };
        std::process::abort();
    }

    #[cfg(target_arch = "wasm32")]
    {
        // SAFETY: the wasm bridge records actor context or exits known
        // non-actor canonical traps. Unknown codes must still fail closed.
        unsafe { hew_trap_with_code(code) };
        std::process::abort();
    }
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
/// Outside an actor context, canonical Hew trap codes become WASI process exit
/// statuses. Unknown codes return so the caller's following `llvm.trap` remains
/// the fail-closed non-actor fallback, matching native's unknown-code sink.
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
    if let Some(exit_code) = crate::internal::types::canonical_trap_wasi_exit_code(code) {
        eprintln!("__hew_wasi_trap_exit_code={exit_code}");
        // JUSTIFIED: wasm32 non-actor traps terminate the process immediately,
        // so bypassing Rust Drop is deliberate and the WASI host reclaims
        // process resources. Wasmtime rejects WASI proc_exit statuses above
        // 126, so the Hew CLI captures the sentinel above and maps this
        // transport status back to the canonical trap code.
        std::process::exit(1);
    }
}

#[cfg(all(test, not(target_arch = "wasm32")))]
mod tests {
    use super::*;
    use crate::internal::types::HEW_TRAP_INDEX_OUT_OF_BOUNDS;

    #[test]
    #[cfg_attr(
        miri,
        ignore = "spawns a subprocess to observe abort(); Miri cannot posix_spawn"
    )]
    fn runtime_bounds_trap_aborts_without_actor_context() {
        let output = std::process::Command::new(std::env::current_exe().unwrap())
            .args([
                "--exact",
                "--nocapture",
                "trap_code::tests::_helper_runtime_bounds_trap_aborts_without_actor_context",
            ])
            .env("RUST_TEST_THREADS", "1")
            .env(
                "HEW_DEATH_TEST",
                "_helper_runtime_bounds_trap_aborts_without_actor_context",
            )
            .output()
            .unwrap();

        assert!(
            !output.status.success(),
            "runtime bounds trap must terminate without actor context"
        );
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("hew: trap in main context: IndexOutOfBounds"),
            "runtime bounds trap should preserve the trap code diagnostic; got: {stderr}"
        );
    }

    #[test]
    fn _helper_runtime_bounds_trap_aborts_without_actor_context() {
        if std::env::var("HEW_DEATH_TEST").map_or(
            true,
            |value| value != "_helper_runtime_bounds_trap_aborts_without_actor_context",
        ) {
            return;
        }

        // SAFETY: this helper intentionally exercises the terminating path.
        unsafe { runtime_bounds_trap(HEW_TRAP_INDEX_OUT_OF_BOUNDS) };
    }
}
