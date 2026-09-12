//! Hew runtime: `random` module.
//!
//! CPython-compatible MT19937 Mersenne Twister PRNG with `#[no_mangle]
//! extern "C"` FFI functions. State is thread-local (one generator per thread).
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use std::cell::RefCell;
use std::mem;

// Re-export HewVec so we can accept vec pointers.
pub use hew_cabi::vec::{ElemKind, HewVec};

unsafe fn validate_vec_shape(
    v: *mut HewVec,
    expected_size: usize,
    context: &str,
) -> Option<*mut HewVec> {
    if v.is_null() {
        return None;
    }
    // SAFETY: caller guarantees `v` is a live HewVec when non-null.
    let vec = unsafe { &*v };
    // SAFETY: a descriptor-backed vector owns its live layout storage.
    let plain = if let Some(layout) = unsafe { vec.layout.as_ref() } {
        layout.ownership_kind == crate::vec::HewTypeOwnershipKind::Plain
    } else {
        vec.elem_kind == ElemKind::Plain
    };
    if !plain {
        crate::set_last_error(format!("{context}: expected plain elements"));
        return None;
    }
    if vec.elem_size != expected_size {
        crate::set_last_error(format!(
            "{context}: expected elem_size {expected_size}, got {}",
            vec.elem_size
        ));
        return None;
    }
    Some(v)
}

// ── MT19937 constants ───────────────────────────────────────────────────────
const N: usize = 624;
const M: usize = 397;
const MATRIX_A: u32 = 0x9908_b0df;
const UPPER_MASK: u32 = 0x8000_0000;
const LOWER_MASK: u32 = 0x7fff_ffff;

// ── MT19937 state ───────────────────────────────────────────────────────────

struct MtState {
    mt: [u32; N],
    mti: usize,
    /// Cached second variate from Box-Muller (NaN = empty).
    gauss_spare: f64,
    gauss_has_spare: bool,
}

impl MtState {
    fn new() -> Self {
        let mut s = MtState {
            mt: [0u32; N],
            mti: N + 1,
            gauss_spare: 0.0,
            gauss_has_spare: false,
        };
        // Auto-seed from OS entropy via rand (already a dependency).
        // WASM doesn't have reliable OS entropy, so use fixed seed there.
        #[cfg(not(target_arch = "wasm32"))]
        {
            use rand::RngExt;
            let mut rng = rand::rng();
            let key: [u32; 4] = std::array::from_fn(|_| rng.random());
            s.init_by_array(&key);
        }
        #[cfg(target_arch = "wasm32")]
        {
            s.init_genrand(19_650_218);
        }
        s
    }

    /// `CPython` `init_genrand`.
    #[expect(
        clippy::cast_possible_truncation,
        reason = "MT19937 array index is always < 624, fits in u32"
    )]
    fn init_genrand(&mut self, seed: u32) {
        self.mt[0] = seed;
        for i in 1..N {
            self.mt[i] = 1_812_433_253u32
                .wrapping_mul(self.mt[i - 1] ^ (self.mt[i - 1] >> 30))
                .wrapping_add(i as u32);
        }
        self.mti = N;
    }

    /// `CPython` `init_by_array`.
    #[expect(
        clippy::cast_possible_truncation,
        reason = "MT19937 array index is always < 624, fits in u32"
    )]
    fn init_by_array(&mut self, init_key: &[u32]) {
        self.init_genrand(19_650_218);
        let mut i: usize = 1;
        let mut j: usize = 0;
        let k = if N > init_key.len() {
            N
        } else {
            init_key.len()
        };
        for _ in 0..k {
            self.mt[i] = (self.mt[i]
                ^ ((self.mt[i - 1] ^ (self.mt[i - 1] >> 30)).wrapping_mul(1_664_525)))
            .wrapping_add(init_key[j])
            .wrapping_add(j as u32);
            i += 1;
            j += 1;
            if i >= N {
                self.mt[0] = self.mt[N - 1];
                i = 1;
            }
            if j >= init_key.len() {
                j = 0;
            }
        }
        for _ in 0..(N - 1) {
            self.mt[i] = (self.mt[i]
                ^ ((self.mt[i - 1] ^ (self.mt[i - 1] >> 30)).wrapping_mul(1_566_083_941)))
            .wrapping_sub(i as u32);
            i += 1;
            if i >= N {
                self.mt[0] = self.mt[N - 1];
                i = 1;
            }
        }
        self.mt[0] = 0x8000_0000;
    }

    /// Generate a random u32.
    fn genrand_uint32(&mut self) -> u32 {
        static MAG01: [u32; 2] = [0, MATRIX_A];

        if self.mti >= N {
            for kk in 0..(N - M) {
                let y = (self.mt[kk] & UPPER_MASK) | (self.mt[kk + 1] & LOWER_MASK);
                self.mt[kk] = self.mt[kk + M] ^ (y >> 1) ^ MAG01[(y & 1) as usize];
            }
            for kk in (N - M)..(N - 1) {
                let y = (self.mt[kk] & UPPER_MASK) | (self.mt[kk + 1] & LOWER_MASK);
                self.mt[kk] = self.mt[kk.wrapping_add(M).wrapping_sub(N)]
                    ^ (y >> 1)
                    ^ MAG01[(y & 1) as usize];
            }
            let y = (self.mt[N - 1] & UPPER_MASK) | (self.mt[0] & LOWER_MASK);
            self.mt[N - 1] = self.mt[M - 1] ^ (y >> 1) ^ MAG01[(y & 1) as usize];
            self.mti = 0;
        }

        let mut y = self.mt[self.mti];
        self.mti += 1;

        // Tempering
        y ^= y >> 11;
        y ^= (y << 7) & 0x9d2c_5680;
        y ^= (y << 15) & 0xefc6_0000;
        y ^= y >> 18;
        y
    }

    /// `CPython` `random()` — 53-bit precision float in [0.0, 1.0).
    fn random(&mut self) -> f64 {
        let a = self.genrand_uint32() >> 5; // 27 bits
        let b = self.genrand_uint32() >> 6; // 26 bits
        (f64::from(a) * 67_108_864.0 + f64::from(b)) / 9_007_199_254_740_992.0
    }

    /// Number of bits needed to represent `n`.
    fn bit_length(n: u64) -> u32 {
        if n == 0 {
            0
        } else {
            64 - n.leading_zeros()
        }
    }

    /// `CPython` `getrandbits(k)` — generate a k-bit random integer.
    fn getrandbits(&mut self, k: u32) -> u64 {
        if k == 0 {
            return 0;
        }
        let full_words = k / 32;
        let extra_bits = k % 32;
        let mut result: u64 = 0;
        for i in 0..full_words {
            result |= u64::from(self.genrand_uint32()) << (i * 32);
        }
        if extra_bits > 0 {
            result |= u64::from(self.genrand_uint32() >> (32 - extra_bits)) << (full_words * 32);
        }
        result
    }

    /// `CPython` `_randbelow(n)` using rejection sampling.
    fn randbelow(&mut self, n: u64) -> u64 {
        if n <= 1 {
            return 0;
        }
        let k = Self::bit_length(n);
        loop {
            let r = self.getrandbits(k);
            if r < n {
                return r;
            }
        }
    }

    /// CPython-compatible gauss using standard Box-Muller with caching.
    fn gauss(&mut self, mu: f64, sigma: f64) -> f64 {
        if self.gauss_has_spare {
            self.gauss_has_spare = false;
            return mu + sigma * self.gauss_spare;
        }

        let x2pi = self.random() * std::f64::consts::TAU;
        let g2rad = (-2.0 * (1.0 - self.random()).ln()).sqrt();
        let z = x2pi.cos() * g2rad;
        self.gauss_spare = x2pi.sin() * g2rad;
        self.gauss_has_spare = true;
        mu + sigma * z
    }
}

thread_local! {
    static MT_STATE: RefCell<MtState> = RefCell::new(MtState::new());
}

// ── FFI functions ───────────────────────────────────────────────────────────

/// Seed the PRNG using `init_by_array` with key `[seed & 0xFFFFFFFF]`.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI.
#[no_mangle]
#[expect(clippy::cast_sign_loss, reason = "seed is masked to low 32 bits")]
pub unsafe extern "C" fn hew_random_seed(seed: i64) {
    MT_STATE.with(|s| {
        let mut st = s.borrow_mut();
        let key = [(seed as u64 & 0xFFFF_FFFF) as u32];
        st.init_by_array(&key);
        st.gauss_has_spare = false;
    });
}

/// Random float in [0.0, 1.0) with 53-bit precision.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI.
#[no_mangle]
pub unsafe extern "C" fn hew_random_random() -> f64 {
    MT_STATE.with(|s| s.borrow_mut().random())
}

/// Gaussian random with given mean and sigma.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI.
#[no_mangle]
pub unsafe extern "C" fn hew_random_gauss(mu: f64, sigma: f64) -> f64 {
    MT_STATE.with(|s| s.borrow_mut().gauss(mu, sigma))
}

/// Random integer in [lo, hi).
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI.
#[no_mangle]
#[expect(
    clippy::cast_sign_loss,
    reason = "hi > lo is guaranteed by the check above"
)]
#[expect(
    clippy::cast_possible_wrap,
    reason = "randbelow(range) < range which fits in i64"
)]
pub unsafe extern "C" fn hew_random_randint(lo: i64, hi: i64) -> i64 {
    if hi <= lo {
        return lo;
    }
    let range = (hi - lo) as u64;
    MT_STATE.with(|s| lo + s.borrow_mut().randbelow(range) as i64)
}

/// Shuffle a `HewVec` of i64 in-place (Fisher-Yates).
///
/// # Safety
///
/// `v` must be a valid `HewVec` pointer containing i64 elements.
#[no_mangle]
#[expect(
    clippy::cast_ptr_alignment,
    reason = "HewVec data is always properly aligned for its element type"
)]
#[expect(
    clippy::cast_possible_truncation,
    reason = "shuffle index is bounded by vec length"
)]
pub unsafe extern "C" fn hew_random_shuffle_i64(v: *mut HewVec) {
    // SAFETY: caller guarantees `v` is either null or a valid HewVec pointer.
    if unsafe { validate_vec_shape(v, mem::size_of::<i64>(), "hew_random_shuffle_i64") }.is_none() {
        return;
    }
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        let vec = &mut *v;
        let len = vec.len;
        if len <= 1 {
            return;
        }
        let data = vec.data.cast::<i64>();
        MT_STATE.with(|s| {
            let mut st = s.borrow_mut();
            for i in (1..len).rev() {
                let j = st.randbelow((i + 1) as u64) as usize;
                let pi = data.add(i);
                let pj = data.add(j);
                core::ptr::swap(pi, pj);
            }
        });
    }
}

/// Weighted choice using bisect on cumulative weights. Returns the chosen index.
/// Accepts a `HewVec` of f64 cumulative weights, the total weight, and n (unused,
/// reserved for multi-sample).
///
/// # Safety
///
/// `v` must be a valid `HewVec` pointer containing f64 cumulative weights.
#[no_mangle]
#[expect(
    clippy::cast_ptr_alignment,
    reason = "HewVec data is always properly aligned for its element type"
)]
#[expect(
    clippy::cast_possible_wrap,
    reason = "bisect index is bounded by vec length"
)]
pub unsafe extern "C" fn hew_random_choices_vec(v: *mut HewVec, total: f64, _n: i64) -> i64 {
    // SAFETY: caller guarantees `v` is either null or a valid HewVec pointer.
    if unsafe { validate_vec_shape(v, mem::size_of::<f64>(), "hew_random_choices_vec") }.is_none() {
        return 0;
    }
    // SAFETY: caller guarantees `v` is valid and contains f64 data.
    unsafe {
        let vec = &*v;
        let len = vec.len;
        if len == 0 {
            return 0;
        }
        let data = vec.data.cast::<f64>();
        let r = MT_STATE.with(|s| s.borrow_mut().random()) * total;
        // bisect_right
        let mut lo: usize = 0;
        let mut hi: usize = len;
        while lo < hi {
            let mid = usize::midpoint(lo, hi);
            if r >= *data.add(mid) {
                lo = mid + 1;
            } else {
                hi = mid;
            }
        }
        lo as i64
    }
}

// ── xoshiro256++ (`random.Rng`) ──────────────────────────────────────────
//
// `random.Rng` is a seedable, non-cryptographic generator distinct from the
// thread-local MT19937 above: MT19937 seeds one implicit generator per
// thread, while `Rng` is an explicit value a caller can hold, pass around,
// and run several independent streams from in the same thread. xoshiro256++
// is chosen for its small state (4 u64 words, carried as an ordinary
// `Vec<u64>` field on the Hew side — no opaque handle required), its speed,
// and its long track record of passing empirical randomness test suites
// (BigCrush, PractRand) despite not being cryptographically secure. Secure
// randomness is a separate, unrelated need served by `crypto_bytes`/
// `crypto_u64` in `std/random/random.hew`, which delegate to
// `crypto.random_bytes` (the existing `ring`-backed OS-entropy authority)
// rather than duplicating a second entropy source here.
//
// Seed expansion uses splitmix64, the standard companion generator for
// xoshiro: a single `i64` seed does not fill 256 bits of state on its own,
// and splitmix64 is the reference algorithm's own recommended way to expand
// a small seed into well-mixed state words.

/// One splitmix64 step: advances `state` and returns the next output word.
fn splitmix64_next(state: &mut u64) -> u64 {
    *state = state.wrapping_add(0x9E37_79B9_7F4A_7C15);
    let mut z = *state;
    z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
    z ^ (z >> 31)
}

/// Expand an `i64` seed into 4 xoshiro256++ state words via 4 splitmix64
/// steps, matching the reference construction.
#[expect(
    clippy::cast_sign_loss,
    reason = "seed is a bit pattern here, not a magnitude; every i64 value is a valid seed"
)]
fn xoshiro_seed_words(seed: i64) -> [u64; 4] {
    let mut s = seed as u64;
    std::array::from_fn(|_| splitmix64_next(&mut s))
}

/// One xoshiro256++ step: advances `state` in place and returns the next
/// output word. Reference: <https://prng.di.unimi.it/xoshiro256plusplus.c>.
fn xoshiro256pp_next(state: &mut [u64; 4]) -> u64 {
    let result = state[0]
        .wrapping_add(state[3])
        .rotate_left(23)
        .wrapping_add(state[0]);
    let t = state[1] << 17;
    state[2] ^= state[0];
    state[3] ^= state[1];
    state[1] ^= state[2];
    state[0] ^= state[3];
    state[2] ^= t;
    state[3] = state[3].rotate_left(45);
    result
}

/// Draw an unbiased value in `[0, n)` from an xoshiro256++ state via
/// mask-and-reject sampling (Lemire's simple approach): `n` is rounded up to
/// the next power of two to get a bitmask, and draws outside `[0, n)` are
/// rejected. Plain modulo would bias the low end of the range whenever `n`
/// does not evenly divide 2^64.
fn xoshiro_randbelow(state: &mut [u64; 4], n: u64) -> u64 {
    if n <= 1 {
        return 0;
    }
    let mask = n.next_power_of_two() - 1;
    loop {
        let r = xoshiro256pp_next(state) & mask;
        if r < n {
            return r;
        }
    }
}

/// Load a 4-word xoshiro256++ state from a Hew `Vec<u64>` state handle.
///
/// # Safety
///
/// `v` must be either null or a valid `HewVec` pointer.
#[expect(
    clippy::cast_ptr_alignment,
    reason = "HewVec data is always properly aligned for its element type"
)]
unsafe fn load_rng_state(v: *mut HewVec, context: &str) -> Option<[u64; 4]> {
    // SAFETY: caller guarantees `v` is either null or a valid HewVec pointer.
    let v = unsafe { validate_vec_shape(v, mem::size_of::<u64>(), context) }?;
    // SAFETY: validate_vec_shape confirmed `v` is non-null and well-formed.
    let vec = unsafe { &*v };
    if vec.len != 4 {
        crate::set_last_error(format!(
            "{context}: expected a 4-word Rng state, got {}",
            vec.len
        ));
        return None;
    }
    let data = vec.data.cast::<u64>();
    // SAFETY: elem_size == 8 and len == 4 were confirmed above.
    Some(unsafe { [*data, *data.add(1), *data.add(2), *data.add(3)] })
}

/// Write a 4-word xoshiro256++ state back into a Hew `Vec<u64>` state handle.
///
/// # Safety
///
/// `v` must be a valid, non-null `HewVec` pointer already validated by
/// [`load_rng_state`] against the same allocation (`elem_size` 8, len 4).
#[expect(
    clippy::cast_ptr_alignment,
    reason = "HewVec data is always properly aligned for its element type"
)]
unsafe fn store_rng_state(v: *mut HewVec, state: [u64; 4]) {
    // SAFETY: caller guarantees `v` is a valid HewVec with elem_size 8, len 4.
    unsafe {
        let data = (*v).data.cast::<u64>();
        data.write(state[0]);
        data.add(1).write(state[1]);
        data.add(2).write(state[2]);
        data.add(3).write(state[3]);
    }
}

/// Create a fresh xoshiro256++ state, seeded from `seed` via 4 splitmix64
/// expansions, as an owned `Vec<u64>` (a plain `HewVec` with
/// `elem_size == 8`, `len == 4`).
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. The returned pointer is an
/// ordinary owned `Vec<u64>` value; the caller releases it the same way as
/// any other Hew-constructed vec.
#[no_mangle]
#[expect(
    clippy::cast_possible_wrap,
    reason = "size_of::<u64>() is 8, fits in i64"
)]
pub unsafe extern "C" fn hew_rng_new(seed: i64) -> *mut HewVec {
    let words = xoshiro_seed_words(seed);
    // SAFETY: hew_vec_new_with_elem_size and hew_vec_push_generic are the
    // ordinary vec constructors used throughout this crate; the pushed data
    // pointers are valid `u64` references for the duration of the call.
    unsafe {
        let v = crate::vec::hew_vec_new_with_elem_size(mem::size_of::<u64>() as i64);
        for word in &words {
            crate::vec::hew_vec_push_generic(v, (word as *const u64).cast());
        }
        v
    }
}

/// Draw the next `u64` from an `Rng` state, advancing it in place.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. `v` must be either null or a
/// valid `HewVec` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_rng_next_u64(v: *mut HewVec) -> u64 {
    // SAFETY: caller guarantees `v` is either null or a valid HewVec pointer.
    let Some(mut state) = (unsafe { load_rng_state(v, "hew_rng_next_u64") }) else {
        return 0;
    };
    let result = xoshiro256pp_next(&mut state);
    // SAFETY: load_rng_state validated `v`'s shape above.
    unsafe { store_rng_state(v, state) };
    result
}

/// Draw the next `f64` in `[0.0, 1.0)` from an `Rng` state, advancing it in
/// place. Uses the top 53 bits of a `next_u64` draw, the precision of an
/// `f64` mantissa (the standard xoshiro float-conversion recipe).
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. `v` must be either null or a
/// valid `HewVec` pointer.
#[no_mangle]
#[expect(
    clippy::cast_precision_loss,
    reason = "bits >> 11 fits in 53 bits, exactly an f64 mantissa's width, and 1u64 << 53 is an exact power of two"
)]
pub unsafe extern "C" fn hew_rng_next_f64(v: *mut HewVec) -> f64 {
    // SAFETY: forwarding to hew_rng_next_u64 with the same contract.
    let bits = unsafe { hew_rng_next_u64(v) };
    ((bits >> 11) as f64) * (1.0 / (1u64 << 53) as f64)
}

/// Draw a random integer in the half-open range `[lo, hi)` from an `Rng`
/// state, advancing it in place. Returns `lo` when `hi <= lo`.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. `v` must be either null or a
/// valid `HewVec` pointer.
#[no_mangle]
#[expect(
    clippy::cast_sign_loss,
    reason = "hi > lo is guaranteed by the check above"
)]
#[expect(
    clippy::cast_possible_wrap,
    reason = "xoshiro_randbelow(range) < range which fits in i64"
)]
pub unsafe extern "C" fn hew_rng_range(v: *mut HewVec, lo: i64, hi: i64) -> i64 {
    if hi <= lo {
        return lo;
    }
    // SAFETY: caller guarantees `v` is either null or a valid HewVec pointer.
    let Some(mut state) = (unsafe { load_rng_state(v, "hew_rng_range") }) else {
        return lo;
    };
    let range = (hi - lo) as u64;
    let r = xoshiro_randbelow(&mut state, range);
    // SAFETY: load_rng_state validated `v`'s shape above.
    unsafe { store_rng_state(v, state) };
    lo + r as i64
}

/// Shuffle a `Vec<i64>` in-place (Fisher-Yates) using an `Rng` state,
/// advancing the state in place.
///
/// # Safety
///
/// Called from compiled Hew programs via C ABI. `state` must be either null
/// or a valid `HewVec` pointer over `u64` elements; `target` must be either
/// null or a valid `HewVec` pointer over `i64` elements.
#[no_mangle]
#[expect(
    clippy::cast_ptr_alignment,
    reason = "HewVec data is always properly aligned for its element type"
)]
#[expect(
    clippy::cast_possible_truncation,
    reason = "shuffle index is bounded by vec length"
)]
pub unsafe extern "C" fn hew_rng_shuffle_i64(state: *mut HewVec, target: *mut HewVec) {
    // SAFETY: caller guarantees `state` is either null or a valid HewVec pointer.
    let Some(mut st) = (unsafe { load_rng_state(state, "hew_rng_shuffle_i64") }) else {
        return;
    };
    // SAFETY: caller guarantees `target` is either null or a valid HewVec pointer.
    if unsafe { validate_vec_shape(target, mem::size_of::<i64>(), "hew_rng_shuffle_i64") }.is_none()
    {
        return;
    }
    // SAFETY: `target`'s shape was validated above.
    unsafe {
        let vec = &mut *target;
        let len = vec.len;
        if len > 1 {
            let data = vec.data.cast::<i64>();
            for i in (1..len).rev() {
                let j = xoshiro_randbelow(&mut st, (i + 1) as u64) as usize;
                core::ptr::swap(data.add(i), data.add(j));
            }
        }
    }
    // SAFETY: `state`'s shape was validated by load_rng_state above.
    unsafe { store_rng_state(state, st) };
}

#[cfg(test)]
#[expect(
    clippy::cast_possible_truncation,
    reason = "test data: values are small enough to fit in target types"
)]
mod tests {
    use super::*;

    /// Reference sequence computed independently in Python from the
    /// published splitmix64 and xoshiro256++ algorithms (not derived from
    /// this Rust implementation) — see the xoshiro256++ module doc comment
    /// for the algorithms and <https://prng.di.unimi.it/xoshiro256plusplus.c>.
    #[test]
    fn xoshiro256pp_seed42_matches_independent_reference() {
        let mut state = xoshiro_seed_words(42);
        assert_eq!(
            state,
            [
                13_679_457_532_755_275_413,
                2_949_826_092_126_892_291,
                5_139_283_748_462_763_858,
                6_349_198_060_258_255_764,
            ]
        );
        let expected: [u64; 5] = [
            15_021_278_609_987_233_951,
            5_881_210_131_331_364_753,
            18_149_643_915_985_481_100,
            12_933_668_939_759_105_464,
            14_637_574_242_682_825_331,
        ];
        for want in expected {
            assert_eq!(xoshiro256pp_next(&mut state), want);
        }
    }

    #[test]
    fn rng_randbelow_never_reaches_bound() {
        let mut state = xoshiro_seed_words(7);
        for _ in 0..10_000 {
            let n = xoshiro_randbelow(&mut state, 37);
            assert!(n < 37, "randbelow(37) produced {n}");
        }
    }

    #[test]
    fn test_cpython_seed42_random() {
        MT_STATE.with(|s| {
            let mut st = s.borrow_mut();
            let key = [42u32];
            st.init_by_array(&key);

            let r0 = st.random();
            let r1 = st.random();
            // CPython: random.seed(42); random.random() ≈ 0.6394267984578837
            assert!(
                (r0 - 0.639_426_798_457_883_7).abs() < 1e-15,
                "r0 = {r0}, expected 0.639_426_798_457_883_7"
            );
            // CPython: random.random() ≈ 0.025010755222666936
            assert!(
                (r1 - 0.025_010_755_222_666_936).abs() < 1e-15,
                "r1 = {r1}, expected 0.025_010_755_222_666_936"
            );
        });
    }

    #[test]
    fn test_cpython_seed42_shuffle_10() {
        // Python 3: random.seed(42); l = list(range(10)); random.shuffle(l)
        // Result: [7, 3, 2, 8, 5, 6, 9, 4, 0, 1]
        let expected = [7i64, 3, 2, 8, 5, 6, 9, 4, 0, 1];

        MT_STATE.with(|s| {
            let mut st = s.borrow_mut();
            let key = [42u32];
            st.init_by_array(&key);
            st.gauss_has_spare = false;

            let mut arr: Vec<i64> = (0..10).collect();
            let len = arr.len();
            for i in (1..len).rev() {
                let j = st.randbelow((i + 1) as u64) as usize;
                arr.swap(i, j);
            }
            assert_eq!(arr, expected, "shuffle mismatch: got {arr:?}");
        });
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn entropy_seeded_states_diverge() {
        let mut st1 = MtState::new();
        let mut st2 = MtState::new();

        // Generate 10 values from each — entropy-seeded states should differ
        // in at least one position. Probability of all 10 matching by chance
        // with different seeds is ~2^(-320).
        let vals1: Vec<f64> = (0..10).map(|_| st1.random()).collect();
        let vals2: Vec<f64> = (0..10).map(|_| st2.random()).collect();
        assert_ne!(vals1, vals2, "two independently seeded MTs should diverge");
    }

    #[test]
    fn shuffle_i64_rejects_wrong_elem_shape() {
        crate::hew_clear_error();
        // SAFETY: test creates and frees the vector around the invalid-shape call.
        unsafe {
            let v = crate::vec::hew_vec_new_str();
            hew_random_shuffle_i64(v);
            let error = crate::hew_last_error();
            assert!(
                !error.is_null(),
                "invalid vector shape must report an error"
            );
            let err = std::ffi::CStr::from_ptr(error)
                .to_str()
                .unwrap()
                .to_string();
            assert!(
                err.contains("expected plain elements"),
                "unexpected error: {err}"
            );
            crate::hew_clear_error();
            assert_eq!(hew_random_choices_vec(v, 1.0, 1), 0);
            let error = crate::hew_last_error();
            assert!(!error.is_null(), "choices must reject owned elements");
            assert!(std::ffi::CStr::from_ptr(error)
                .to_str()
                .unwrap()
                .contains("expected plain elements"));
            crate::vec::hew_vec_free(v);
        }
    }

    #[test]
    fn choices_vec_rejects_wrong_elem_size() {
        crate::hew_clear_error();
        // SAFETY: test creates and frees the vector around the invalid-shape call.
        unsafe {
            let v = crate::vec::hew_vec_new();
            let result = hew_random_choices_vec(v, 1.0, 1);
            assert_eq!(result, 0);
            let error = crate::hew_last_error();
            assert!(
                !error.is_null(),
                "invalid vector shape must report an error"
            );
            let err = std::ffi::CStr::from_ptr(error)
                .to_str()
                .unwrap()
                .to_string();
            assert!(
                err.contains("expected elem_size 8"),
                "unexpected error: {err}"
            );
            crate::vec::hew_vec_free(v);
        }
    }
}
