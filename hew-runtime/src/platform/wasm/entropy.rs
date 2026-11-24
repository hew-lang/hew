//! WASI / bare-wasm32 entropy backend.
//!
//! Under WASI (`target_os = "wasi"`), `rand::rng()` draws from WASI
//! `random_get`. On bare non-WASI `wasm32`, no OS entropy ABI is linked so
//! callers fall back deterministically without requiring a browser entropy ABI.

#[cfg(target_os = "wasi")]
#[inline]
pub(crate) fn seed_words<const N: usize>() -> Option<[u32; N]> {
    use rand::RngExt as _;
    let mut rng = rand::rng();
    Some(std::array::from_fn(|_| rng.random()))
}

#[cfg(not(target_os = "wasi"))]
#[inline]
pub(crate) fn seed_words<const N: usize>() -> Option<[u32; N]> {
    None
}

#[cfg(target_os = "wasi")]
#[inline]
pub(crate) fn random_u64_pair() -> Option<(u64, u64)> {
    use rand::RngExt as _;
    let mut rng = rand::rng();
    Some((rng.random(), rng.random()))
}

#[cfg(not(target_os = "wasi"))]
#[inline]
pub(crate) fn random_u64_pair() -> Option<(u64, u64)> {
    None
}
