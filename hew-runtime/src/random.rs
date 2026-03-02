//! Hew runtime: `random` module.
//!
//! CPython-compatible MT19937 Mersenne Twister PRNG with `#[no_mangle]
//! extern "C"` FFI functions. State is thread-local (one generator per thread).

use std::cell::RefCell;

// Re-export HewVec so we can accept vec pointers.
pub use hew_cabi::vec::HewVec;

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
        // Default seed so calls before seed() still work.
        s.init_genrand(19650218);
        s
    }

    /// CPython `init_genrand`.
    fn init_genrand(&mut self, seed: u32) {
        self.mt[0] = seed;
        for i in 1..N {
            self.mt[i] = 1812433253u32
                .wrapping_mul(self.mt[i - 1] ^ (self.mt[i - 1] >> 30))
                .wrapping_add(i as u32);
        }
        self.mti = N;
    }

    /// CPython `init_by_array`.
    fn init_by_array(&mut self, init_key: &[u32]) {
        self.init_genrand(19650218);
        let mut i: usize = 1;
        let mut j: usize = 0;
        let k = if N > init_key.len() {
            N
        } else {
            init_key.len()
        };
        for _ in 0..k {
            self.mt[i] = (self.mt[i]
                ^ ((self.mt[i - 1] ^ (self.mt[i - 1] >> 30)).wrapping_mul(1664525)))
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
                ^ ((self.mt[i - 1] ^ (self.mt[i - 1] >> 30)).wrapping_mul(1566083941)))
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

    /// CPython `random()` — 53-bit precision float in [0.0, 1.0).
    fn random(&mut self) -> f64 {
        let a = self.genrand_uint32() >> 5; // 27 bits
        let b = self.genrand_uint32() >> 6; // 26 bits
        (f64::from(a) * 67108864.0 + f64::from(b)) / 9007199254740992.0
    }

    /// Number of bits needed to represent `n`.
    fn bit_length(n: u64) -> u32 {
        if n == 0 {
            0
        } else {
            64 - n.leading_zeros()
        }
    }

    /// CPython `getrandbits(k)` — generate a k-bit random integer.
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

    /// CPython `_randbelow(n)` using rejection sampling.
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
pub unsafe extern "C" fn hew_random_shuffle_i64(v: *mut HewVec) {
    if v.is_null() {
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
pub unsafe extern "C" fn hew_random_choices_vec(v: *mut HewVec, total: f64, _n: i64) -> i64 {
    if v.is_null() {
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
            let mid = (lo + hi) / 2;
            if r >= *data.add(mid) {
                lo = mid + 1;
            } else {
                hi = mid;
            }
        }
        lo as i64
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
                (r0 - 0.6394267984578837).abs() < 1e-15,
                "r0 = {r0}, expected 0.6394267984578837"
            );
            // CPython: random.random() ≈ 0.025010755222666936
            assert!(
                (r1 - 0.025010755222666936).abs() < 1e-15,
                "r1 = {r1}, expected 0.025010755222666936"
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
}
