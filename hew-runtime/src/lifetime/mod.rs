//! Ownership-first primitives for runtime-global lifetime management.
//!
//! This module hosts poison-safe, closure-only wrappers around
//! [`std::sync::Mutex`] / [`std::sync::RwLock`] for use by named
//! runtime globals. The wrappers are the only blessed access path: the
//! inner primitive is module-private, so the `.lock().unwrap()`,
//! `.read().unwrap()`, `.write().unwrap()`, and
//! `if let Ok(g) = m.lock()` patterns — along with the silent
//! poison-skipping variants — are unreachable by type from outside the
//! module.
//!
//! Callers use [`PoisonSafe::access`] for exclusive access,
//! [`PoisonSafeRw::read_access`] for shared-read access, and
//! [`PoisonSafeRw::access`] for exclusive write access; both wrappers
//! offer `try_access` for non-blocking attempts that distinguish
//! contention (`None`) from poison (recovered transparently).

pub(crate) mod poison_safe;

// The wrapper types and their methods become reachable from non-test
// code as the LINK_TABLE / ENV_LOCK migration lands in the follow-on
// commit on this branch. Until then they are exercised by
// `poison_safe::tests` only, which is non-public code.
#[allow(
    unused_imports,
    reason = "first callers land with the LINK_TABLE/ENV_LOCK sweep"
)]
pub(crate) use poison_safe::{PoisonSafe, PoisonSafeRw};
