//! Native OS entropy backend using `rand::rng()`.

use rand::RngExt as _;

#[expect(
    clippy::unnecessary_wraps,
    reason = "cross-platform seam returns Option when the wasm32 backend lacks entropy"
)]
#[inline]
pub(crate) fn seed_words<const N: usize>() -> Option<[u32; N]> {
    let mut rng = rand::rng();
    Some(std::array::from_fn(|_| rng.random()))
}

#[expect(
    clippy::unnecessary_wraps,
    reason = "cross-platform seam returns Option when the wasm32 backend lacks entropy"
)]
#[inline]
pub(crate) fn random_u64_pair() -> Option<(u64, u64)> {
    let mut rng = rand::rng();
    Some((rng.random(), rng.random()))
}
