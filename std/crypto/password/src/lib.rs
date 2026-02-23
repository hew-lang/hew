//! Hew runtime: password hashing and verification using Argon2.
//!
//! Provides C ABI functions for hashing passwords with Argon2id,
//! verifying passwords against PHC-format hashes, and hashing with
//! custom cost parameters.

use std::ffi::{CStr, CString};
use std::os::raw::c_char;

use argon2::password_hash::SaltString;
use argon2::{Algorithm, Argon2, Params, PasswordHash, PasswordHasher, PasswordVerifier, Version};

/// Helper: allocate a C string via `libc::malloc` and copy `s` into it.
///
/// Returns a `malloc`'d pointer the caller must free with [`hew_password_free`],
/// or null on allocation failure.
fn to_malloc_cstr(s: &str) -> *mut c_char {
    let Ok(cs) = CString::new(s) else {
        return std::ptr::null_mut();
    };
    let bytes = cs.as_bytes_with_nul();
    // SAFETY: libc::malloc returns a valid pointer or null.
    let ptr = unsafe { libc::malloc(bytes.len()) };
    if ptr.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: ptr is valid for bytes.len() bytes (just allocated).
    unsafe { std::ptr::copy_nonoverlapping(bytes.as_ptr(), ptr.cast::<u8>(), bytes.len()) };
    ptr.cast::<c_char>()
}

/// Hash `password` with Argon2id default parameters.
///
/// Returns a `malloc`'d PHC string (e.g. `$argon2id$v=19$m=...`) on success,
/// or null on error. The caller must free the result with [`hew_password_free`].
///
/// # Safety
///
/// `password` must be a valid, null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_password_hash(password: *const c_char) -> *mut c_char {
    if password.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: password is non-null and null-terminated per caller contract.
    let pw = unsafe { CStr::from_ptr(password) };
    let Ok(pw_str) = pw.to_str() else {
        return std::ptr::null_mut();
    };
    let salt = SaltString::generate(&mut argon2::password_hash::rand_core::OsRng);
    let Ok(hash) = Argon2::default().hash_password(pw_str.as_bytes(), &salt) else {
        return std::ptr::null_mut();
    };
    to_malloc_cstr(&hash.to_string())
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
    if password.is_null() || hash.is_null() {
        return -1;
    }
    // SAFETY: password is non-null and null-terminated per caller contract.
    let pw = unsafe { CStr::from_ptr(password) };
    // SAFETY: hash is non-null and null-terminated per caller contract.
    let hash_cstr = unsafe { CStr::from_ptr(hash) };
    let (Ok(pw_str), Ok(hash_val)) = (pw.to_str(), hash_cstr.to_str()) else {
        return -1;
    };
    let Ok(parsed) = PasswordHash::new(hash_val) else {
        return -1;
    };
    i32::from(
        Argon2::default()
            .verify_password(pw_str.as_bytes(), &parsed)
            .is_ok(),
    )
}

/// Hash `password` with custom Argon2id parameters.
///
/// Returns a `malloc`'d PHC string on success, or null on error. The caller
/// must free the result with [`hew_password_free`].
///
/// # Safety
///
/// `password` must be a valid, null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_password_hash_custom(
    password: *const c_char,
    mem_cost_kib: u32,
    time_cost: u32,
    parallelism: u32,
) -> *mut c_char {
    if password.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: password is non-null and null-terminated per caller contract.
    let pw = unsafe { CStr::from_ptr(password) };
    let Ok(pw_str) = pw.to_str() else {
        return std::ptr::null_mut();
    };
    let Ok(params) = Params::new(mem_cost_kib, time_cost, parallelism, None) else {
        return std::ptr::null_mut();
    };
    let hasher = Argon2::new(Algorithm::Argon2id, Version::V0x13, params);
    let salt = SaltString::generate(&mut argon2::password_hash::rand_core::OsRng);
    let Ok(hash) = hasher.hash_password(pw_str.as_bytes(), &salt) else {
        return std::ptr::null_mut();
    };
    to_malloc_cstr(&hash.to_string())
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
        // SAFETY: s was allocated by libc::malloc in to_malloc_cstr per caller contract.
        unsafe { libc::free(s.cast()) };
    }
}

#[cfg(test)]
mod tests {
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
    fn custom_params_produce_valid_hash() {
        let pw = CString::new("custom-test").unwrap();
        // SAFETY: pw is a valid C string; parameters are valid for Argon2id.
        let hash_ptr = unsafe { hew_password_hash_custom(pw.as_ptr(), 8192, 2, 1) };
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
        // SAFETY: null inputs are explicitly handled by hew_password_verify.
        assert_eq!(
            unsafe { hew_password_verify(std::ptr::null(), std::ptr::null()) },
            -1
        );
        // SAFETY: null password is explicitly handled by hew_password_hash_custom.
        assert!(unsafe { hew_password_hash_custom(std::ptr::null(), 8192, 2, 1) }.is_null());
    }
}
