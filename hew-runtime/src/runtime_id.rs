//! Process-wide runtime-instance identity.
//!
//! `RuntimeId` is defined here — outside the native-only `runtime` module — so
//! it is available on every target. The native `runtime` module re-exports it
//! (`pub use crate::runtime_id::RuntimeId;`) for its callers, and `HewActor`
//! (compiled on both native and wasm) stamps it as an appended field. The wasm
//! `scheduler_wasm::HewActor` mirror references the *same* type, so the
//! compile-time layout assert is type-identical, not merely size-identical.
//!
//! The id is a plain `u64` discriminant, never a borrowed pointer or handle.
//! Cross-runtime routing fails closed by comparing two ids — it never
//! dereferences a foreign runtime to decide ownership.

/// Process-wide runtime identity.
///
/// Distinct from a PID's `node_id`: a `RuntimeId` tags the runtime instance
/// that owns an actor/timer/capability, so that — once more than one runtime
/// can exist — cross-runtime routing can fail closed on an id mismatch without
/// dereferencing any handle. In single-runtime AOT/JIT programs there is
/// exactly one runtime, always [`RuntimeId::DEFAULT`].
///
/// `#[repr(transparent)]` over `u64` makes this FFI-safe: the host handle ABI
/// (the `*_with_runtime` resolution forms) passes a runtime id across the C
/// boundary as a plain `u64` with no layout surprise. The discriminant — never
/// a borrowed pointer — is what crosses, so a stale or foreign id can be
/// compared and rejected without dereferencing any handle.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(transparent)]
pub struct RuntimeId(pub u64);

impl RuntimeId {
    /// The id of the single default runtime used by AOT and JIT programs.
    pub const DEFAULT: RuntimeId = RuntimeId(0);

    /// The raw `u64` discriminant, for the C-ABI handle forms that pass a
    /// runtime id across the boundary as a plain integer.
    #[inline]
    #[must_use]
    pub const fn as_u64(self) -> u64 {
        self.0
    }
}
