//! Stack growth for recursive walks over source-shaped trees.
//!
//! A left-associative chain such as `a + b + c + …` or `x.f().g().h()…` is
//! flat in the source but one tree level per operator, so every stage that
//! walks an expression recursively descends once per term. The parser builds
//! these chains in a loop; the walkers after it recurse, and a long generated
//! chain would otherwise exhaust the compiler thread's fixed stack. Each
//! recursive expression walker enters through [`ensure_sufficient_stack`], which
//! continues on a fresh heap-allocated stack segment when the current one runs
//! low, so the depth such a walk can reach is bounded by memory rather than by
//! the thread's stack size.

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
