//! Shared OS / WASI entropy seam.

use super::imp;

/// Fill an array of `u32` seed words from OS or WASI entropy.
///
/// Returns `None` on targets without an OS/WASI entropy source (for example,
/// bare non-WASI `wasm32`), letting callers fall back deterministically without
/// introducing an extra host ABI.
#[inline]
pub(crate) fn seed_words<const N: usize>() -> Option<[u32; N]> {
    imp::entropy::seed_words()
}

/// Sample a pair of 64-bit identifiers from OS or WASI entropy.
///
/// Returns `None` when no OS/WASI entropy backend is linked on this target.
#[inline]
pub(crate) fn random_u64_pair() -> Option<(u64, u64)> {
    imp::entropy::random_u64_pair()
}
