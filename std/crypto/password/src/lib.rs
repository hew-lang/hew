//! Hew runtime: password hashing and verification using Argon2id.
//!
//! Provides C ABI functions for hashing passwords, verifying passwords
//! against PHC-format hashes, and hashing with a custom cost parameter.

use std::ffi::c_char;

use argon2::password_hash::SaltString;
use argon2::{Algorithm, Argon2, Params, PasswordHash, PasswordHasher, PasswordVerifier, Version};
use hew_cabi::cabi::{cstr_to_str, str_to_malloc};

/// Hash `password` with Argon2id default parameters.
///
/// Returns a `malloc`'d PHC string (e.g. `$argon2id$v=19$m=...`) on success,
/// or null on error.
///
/// # Safety
///
/// `password` must be a valid, null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_password_hash(password: *const c_char) -> *mut c_char {
    // SAFETY: caller guarantees password is null or a valid C string.
    let pw_str = unsafe { cstr_to_str(password) };
    let Some(pw_str) = pw_str else {
        return std::ptr::null_mut();
    };
    let salt = SaltString::generate(&mut argon2::password_hash::rand_core::OsRng);
    let Ok(hash) = Argon2::default().hash_password(pw_str.as_bytes(), &salt) else {
        return std::ptr::null_mut();
    };
    str_to_malloc(&hash.to_string())
}

/// Verify `password` against a PHC-format `hash`.
///
/// Returns `1` if the password matches, `0` if it does not, or `-1` on error
/// (e.g. null pointers, invalid UTF-8, malformed hash).
///
/// # Safety
///
/// Both `password` and `hash` must be valid, null-terminated C strings.
#[no_mangle]
pub unsafe extern "C" fn hew_password_verify(password: *const c_char, hash: *const c_char) -> i32 {
    // SAFETY: caller guarantees both are null or valid C strings.
    let (Some(pw_str), Some(hash_str)) = (unsafe { cstr_to_str(password) }, unsafe {
        cstr_to_str(hash)
    }) else {
        return -1;
    };
    let Ok(parsed) = PasswordHash::new(hash_str) else {
        return -1;
    };
    i32::from(
        Argon2::default()
            .verify_password(pw_str.as_bytes(), &parsed)
            .is_ok(),
    )
}

/// Hash `password` with Argon2id using a custom cost (iteration count).
///
/// `cost` controls the time cost (number of iterations). Memory cost
/// and parallelism use sensible defaults (19456 KiB, 1 thread).
/// Returns a `malloc`'d PHC string on success, or null on error.
///
/// # Safety
///
/// `password` must be a valid, null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_password_hash_custom(
    password: *const c_char,
    cost: i32,
) -> *mut c_char {
    // SAFETY: caller guarantees password is null or a valid C string.
    let Some(pw_str) = (unsafe { cstr_to_str(password) }) else {
        return std::ptr::null_mut();
    };
    let Ok(cost_u32) = u32::try_from(cost) else {
        return std::ptr::null_mut();
    };
    if cost_u32 < 1 {
        return std::ptr::null_mut();
    }
    let Ok(params) = Params::new(
        Params::DEFAULT_M_COST,
        cost_u32,
        Params::DEFAULT_P_COST,
        None,
    ) else {
        return std::ptr::null_mut();
    };
    let hasher = Argon2::new(Algorithm::Argon2id, Version::V0x13, params);
    let salt = SaltString::generate(&mut argon2::password_hash::rand_core::OsRng);
    let Ok(hash) = hasher.hash_password(pw_str.as_bytes(), &salt) else {
        return std::ptr::null_mut();
    };
    str_to_malloc(&hash.to_string())
}

/// Free a hash string returned by [`hew_password_hash`] or
/// [`hew_password_hash_custom`].
///
/// # Safety
///
/// `s` must be null or a pointer previously returned by one of the
/// `hew_password_hash*` functions.
#[no_mangle]
pub unsafe extern "C" fn hew_password_free(s: *mut c_char) {
    if !s.is_null() {
        // SAFETY: s was allocated by libc::malloc in str_to_malloc per caller contract.
        unsafe { libc::free(s.cast()) };
    }
}

#[cfg(test)]
mod tests {
    use std::ffi::{CStr, CString};

    use super::*;

    #[test]
    fn hash_produces_valid_phc_string() {
        let pw = CString::new("hunter2").unwrap();
        // SAFETY: pw is a valid C string.
        let hash_ptr = unsafe { hew_password_hash(pw.as_ptr()) };
        assert!(!hash_ptr.is_null());
        // SAFETY: hash_ptr is a valid malloc'd C string.
        let hash_str = unsafe { CStr::from_ptr(hash_ptr) }.to_str().unwrap();
        assert!(hash_str.starts_with("$argon2id$"));
        // SAFETY: hash_ptr was returned by hew_password_hash.
        unsafe { hew_password_free(hash_ptr) };
    }

    #[test]
    fn verify_correct_password_returns_1() {
        let pw = CString::new("correct-horse-battery-staple").unwrap();
        // SAFETY: pw is a valid C string.
        let hash_ptr = unsafe { hew_password_hash(pw.as_ptr()) };
        assert!(!hash_ptr.is_null());
        // SAFETY: both are valid C strings.
        let result = unsafe { hew_password_verify(pw.as_ptr(), hash_ptr) };
        assert_eq!(result, 1);
        // SAFETY: hash_ptr was returned by hew_password_hash.
        unsafe { hew_password_free(hash_ptr) };
    }

    #[test]
    fn verify_wrong_password_returns_0() {
        let pw = CString::new("right-password").unwrap();
        let wrong = CString::new("wrong-password").unwrap();
        // SAFETY: pw is a valid C string.
        let hash_ptr = unsafe { hew_password_hash(pw.as_ptr()) };
        assert!(!hash_ptr.is_null());
        // SAFETY: both are valid C strings.
        let result = unsafe { hew_password_verify(wrong.as_ptr(), hash_ptr) };
        assert_eq!(result, 0);
        // SAFETY: hash_ptr was returned by hew_password_hash.
        unsafe { hew_password_free(hash_ptr) };
    }

    #[test]
    fn custom_cost_produces_valid_hash() {
        let pw = CString::new("custom-test").unwrap();
        // SAFETY: pw is a valid C string; cost=2 is a valid iteration count.
        let hash_ptr = unsafe { hew_password_hash_custom(pw.as_ptr(), 2) };
        assert!(!hash_ptr.is_null());
        // SAFETY: hash_ptr is a valid malloc'd C string.
        let hash_str = unsafe { CStr::from_ptr(hash_ptr) }.to_str().unwrap();
        assert!(hash_str.starts_with("$argon2id$"));
        // Verify the custom hash works with verify
        // SAFETY: both are valid C strings.
        let result = unsafe { hew_password_verify(pw.as_ptr(), hash_ptr) };
        assert_eq!(result, 1);
        // SAFETY: hash_ptr was returned by hew_password_hash_custom.
        unsafe { hew_password_free(hash_ptr) };
    }

    #[test]
    fn null_inputs_return_error() {
        // SAFETY: testing null handling.
        assert!(unsafe { hew_password_hash(std::ptr::null()) }.is_null());
        // SAFETY: null inputs are explicitly handled.
        assert_eq!(
            unsafe { hew_password_verify(std::ptr::null(), std::ptr::null()) },
            -1
        );
        // SAFETY: null password is explicitly handled.
        assert!(unsafe { hew_password_hash_custom(std::ptr::null(), 2) }.is_null());
    }

    #[test]
    fn invalid_cost_returns_null() {
        let pw = CString::new("test").unwrap();
        // SAFETY: pw is a valid C string; cost=0 is invalid.
        assert!(unsafe { hew_password_hash_custom(pw.as_ptr(), 0) }.is_null());
        // SAFETY: pw is a valid C string; cost=-1 is invalid.
        assert!(unsafe { hew_password_hash_custom(pw.as_ptr(), -1) }.is_null());
    }
}
