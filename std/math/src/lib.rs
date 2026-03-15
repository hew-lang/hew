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
