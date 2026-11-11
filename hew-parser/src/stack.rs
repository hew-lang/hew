//! Stack growth for recursive walks over source-shaped trees.
//!
//! Recursive expression walkers and genuinely nested parsing enter through
//! [`ensure_sufficient_stack`], which continues on a fresh heap-allocated stack
//! segment when the current one runs low. The parser can then reach its existing
//! nesting diagnostic even on a small compiler stack.
//!
//! On wasm32 this grows the linear-memory stack, not the engine's call stack.
//! Flat binary runs use ordered steps, and unbounded receiver walks must remain
//! iterative where engine depth would otherwise depend on source length.

/// Remaining stack below which a walker continues on a new segment. It must
/// exceed the stack one walker level consumes between two entries, which is
/// largest in unoptimized builds.
const RED_ZONE: usize = 1024 * 1024;

/// Size of each additional stack segment.
const SEGMENT: usize = 16 * 1024 * 1024;

/// Run `f`, first moving to a new stack segment if fewer than [`RED_ZONE`]
/// bytes of the current one remain.
#[inline]
pub fn ensure_sufficient_stack<R>(f: impl FnOnce() -> R) -> R {
    stacker::maybe_grow(RED_ZONE, SEGMENT, f)
}
