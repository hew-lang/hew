//! Hew `std::math` — mathematical operations.
//!
//! Provides floating-point math functions for compiled Hew programs,
//! wrapping the standard Rust `f64` methods (backed by libm).

#[cfg(test)]
extern crate hew_runtime;

// ---------------------------------------------------------------------------
// Float operations
// ---------------------------------------------------------------------------

/// Absolute value of a float.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_math_abs_f(x: f64) -> f64 {
    x.abs()
}

/// Square root.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_math_sqrt(x: f64) -> f64 {
    x.sqrt()
}

/// Raise `base` to the power `exp`.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_math_pow(base: f64, exp: f64) -> f64 {
    base.powf(exp)
}

/// Floor — largest integer value less than or equal to `x`.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_math_floor(x: f64) -> f64 {
    x.floor()
}

/// Ceil — smallest integer value greater than or equal to `x`.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_math_ceil(x: f64) -> f64 {
    x.ceil()
}

/// Round to the nearest integer, with ties rounding away from zero.
///
/// # Safety
///
/// No preconditions.
#[no_mangle]
pub unsafe extern "C" fn hew_math_round(x: f64) -> f64 {
    x.round()
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // ── abs_f ───────────────────────────────────────────────────────

    #[test]
    #[expect(clippy::float_cmp, reason = "Exact IEEE 754 comparison is expected for math function round-trip tests")]
    fn abs_f_positive() {
        assert_eq!(unsafe { hew_math_abs_f(3.5) }, 3.5);
    }

    #[test]
    #[expect(clippy::float_cmp, reason = "Exact IEEE 754 comparison is expected for math function round-trip tests")]
    fn abs_f_negative() {
        assert_eq!(unsafe { hew_math_abs_f(-7.25) }, 7.25);
    }

    #[test]
    #[expect(clippy::float_cmp, reason = "Exact IEEE 754 comparison is expected for math function round-trip tests")]
    fn abs_f_zero() {
        assert_eq!(unsafe { hew_math_abs_f(0.0) }, 0.0);
    }

    #[test]
    fn abs_f_nan() {
        assert!(unsafe { hew_math_abs_f(f64::NAN) }.is_nan());
    }

    // ── sqrt ────────────────────────────────────────────────────────

    #[test]
    #[expect(clippy::float_cmp, reason = "Exact IEEE 754 comparison is expected for math function round-trip tests")]
    fn sqrt_perfect_square() {
        assert_eq!(unsafe { hew_math_sqrt(25.0) }, 5.0);
    }

    #[test]
    #[expect(clippy::float_cmp, reason = "Exact IEEE 754 comparison is expected for math function round-trip tests")]
    fn sqrt_zero() {
        assert_eq!(unsafe { hew_math_sqrt(0.0) }, 0.0);
    }

    #[test]
    fn sqrt_negative_is_nan() {
        assert!(unsafe { hew_math_sqrt(-1.0) }.is_nan());
    }

    // ── pow ─────────────────────────────────────────────────────────

    #[test]
    #[expect(clippy::float_cmp, reason = "Exact IEEE 754 comparison is expected for math function round-trip tests")]
    fn pow_positive_exponent() {
        assert_eq!(unsafe { hew_math_pow(2.0, 10.0) }, 1024.0);
    }

    #[test]
    #[expect(clippy::float_cmp, reason = "Exact IEEE 754 comparison is expected for math function round-trip tests")]
    fn pow_zero_exponent() {
        assert_eq!(unsafe { hew_math_pow(5.0, 0.0) }, 1.0);
    }

    #[test]
    #[expect(clippy::float_cmp, reason = "Exact IEEE 754 comparison is expected for math function round-trip tests")]
    fn pow_negative_exponent() {
        assert_eq!(unsafe { hew_math_pow(2.0, -1.0) }, 0.5);
    }

    // ── floor ───────────────────────────────────────────────────────

    #[test]
    #[expect(clippy::float_cmp, reason = "Exact IEEE 754 comparison is expected for math function round-trip tests")]
    fn floor_positive_fraction() {
        assert_eq!(unsafe { hew_math_floor(3.7) }, 3.0);
    }

    #[test]
    #[expect(clippy::float_cmp, reason = "Exact IEEE 754 comparison is expected for math function round-trip tests")]
    fn floor_negative_fraction() {
        assert_eq!(unsafe { hew_math_floor(-2.3) }, -3.0);
    }

    #[test]
    #[expect(clippy::float_cmp, reason = "Exact IEEE 754 comparison is expected for math function round-trip tests")]
    fn floor_whole_number() {
        assert_eq!(unsafe { hew_math_floor(5.0) }, 5.0);
    }

    // ── ceil ────────────────────────────────────────────────────────

    #[test]
    #[expect(clippy::float_cmp, reason = "Exact IEEE 754 comparison is expected for math function round-trip tests")]
    fn ceil_positive_fraction() {
        assert_eq!(unsafe { hew_math_ceil(3.1) }, 4.0);
    }

    #[test]
    #[expect(clippy::float_cmp, reason = "Exact IEEE 754 comparison is expected for math function round-trip tests")]
    fn ceil_negative_fraction() {
        assert_eq!(unsafe { hew_math_ceil(-2.7) }, -2.0);
    }

    #[test]
    #[expect(clippy::float_cmp, reason = "Exact IEEE 754 comparison is expected for math function round-trip tests")]
    fn ceil_whole_number() {
        assert_eq!(unsafe { hew_math_ceil(5.0) }, 5.0);
    }

    // ── round ───────────────────────────────────────────────────────

    #[test]
    #[expect(clippy::float_cmp, reason = "Exact IEEE 754 comparison is expected for math function round-trip tests")]
    fn round_down() {
        assert_eq!(unsafe { hew_math_round(2.3) }, 2.0);
    }

    #[test]
    #[expect(clippy::float_cmp, reason = "Exact IEEE 754 comparison is expected for math function round-trip tests")]
    fn round_up() {
        assert_eq!(unsafe { hew_math_round(2.7) }, 3.0);
    }

    #[test]
    #[expect(clippy::float_cmp, reason = "Exact IEEE 754 comparison is expected for math function round-trip tests")]
    fn round_half_away_from_zero() {
        // Rust f64::round ties away from zero: 0.5 → 1.0, -0.5 → -1.0
        assert_eq!(unsafe { hew_math_round(0.5) }, 1.0);
        assert_eq!(unsafe { hew_math_round(-0.5) }, -1.0);
    }

    #[test]
    #[expect(clippy::float_cmp, reason = "Exact IEEE 754 comparison is expected for math function round-trip tests")]
    fn round_zero() {
        assert_eq!(unsafe { hew_math_round(0.0) }, 0.0);
    }

    // ── additional coverage ─────────────────────────────────────────

    #[test]
    #[expect(clippy::float_cmp, reason = "Exact IEEE 754 comparison is expected for math function round-trip tests")]
    fn sqrt_large_value() {
        assert_eq!(unsafe { hew_math_sqrt(1_000_000.0) }, 1000.0);
    }

    #[test]
    #[expect(clippy::float_cmp, reason = "Exact IEEE 754 comparison is expected for math function round-trip tests")]
    fn pow_negative_base() {
        assert_eq!(unsafe { hew_math_pow(-2.0, 3.0) }, -8.0);
    }

    #[test]
    #[expect(clippy::float_cmp, reason = "Exact IEEE 754 comparison is expected for math function round-trip tests")]
    fn floor_point_five() {
        assert_eq!(unsafe { hew_math_floor(2.5) }, 2.0);
        assert_eq!(unsafe { hew_math_floor(-2.5) }, -3.0);
    }

    #[test]
    #[expect(clippy::float_cmp, reason = "Exact IEEE 754 comparison is expected for math function round-trip tests")]
    fn ceil_point_five() {
        assert_eq!(unsafe { hew_math_ceil(2.5) }, 3.0);
        assert_eq!(unsafe { hew_math_ceil(-2.5) }, -2.0);
    }

    #[test]
    #[expect(clippy::float_cmp, reason = "Exact IEEE 754 comparison is expected for math function round-trip tests")]
    fn round_point_one() {
        assert_eq!(unsafe { hew_math_round(2.1) }, 2.0);
    }

    #[test]
    #[expect(clippy::float_cmp, reason = "Exact IEEE 754 comparison is expected for math function round-trip tests")]
    fn round_point_nine() {
        assert_eq!(unsafe { hew_math_round(2.9) }, 3.0);
    }
}
