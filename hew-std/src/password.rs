//! Hew runtime: password hashing and verification using Argon2id.
//!
//! Provides C ABI functions for hashing passwords, verifying passwords
//! against PHC-format hashes, and hashing with a custom cost parameter. All
//! returned strings are managed strings, released with `hew_string_drop`.
use argon2::password_hash::SaltString;
use argon2::{Algorithm, Argon2, Params, PasswordHash, PasswordHasher, PasswordVerifier, Version};
use hew_cabi::string::{string_as_str, string_from_str, HewString};

/// Hash `password` with Argon2id default parameters.
///
/// Returns an owned managed string holding a PHC string (e.g.
/// `$argon2id$v=19$m=...`) on success, or null on error. Release it with
/// `hew_string_drop`.
///
/// # Safety
///
/// `password` must be null (canonical empty) or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_password_hash(password: *const HewString) -> *mut HewString {
    // SAFETY: password is null (canonical empty) or a live managed string handle.
    let pw_str = unsafe { string_as_str(password) };
    let salt = SaltString::generate(&mut argon2::password_hash::rand_core::OsRng);
    let Ok(hash) = Argon2::default().hash_password(pw_str.as_bytes(), &salt) else {
        return std::ptr::null_mut();
    };
    string_from_str(&hash.to_string())
}

/// Verify `password` against a PHC-format `hash`.
///
/// Returns `1` if the password matches, `0` if it does not, or `-1` on error
/// (e.g. invalid UTF-8, malformed hash).
///
/// # Safety
///
/// Both `password` and `hash` must be null (canonical empty) or live managed
/// string handles.
#[no_mangle]
pub unsafe extern "C" fn hew_password_verify(
    password: *const HewString,
    hash: *const HewString,
) -> i32 {
    // SAFETY: password and hash are null (canonical empty) or live managed string handles.
    let pw_str = unsafe { string_as_str(password) };
    // SAFETY: password and hash are null (canonical empty) or live managed string handles.
    let hash_str = unsafe { string_as_str(hash) };
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
/// Returns an owned managed string holding a PHC string on success, or null
/// on error. Release it with `hew_string_drop`.
///
/// # Safety
///
/// `password` must be null (canonical empty) or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_password_hash_custom(
    password: *const HewString,
    cost: i32,
) -> *mut HewString {
    // SAFETY: password is null (canonical empty) or a live managed string handle.
    let pw_str = unsafe { string_as_str(password) };
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
    string_from_str(&hash.to_string())
}

#[cfg(test)]
mod tests {
    use crate::test_string::ManagedString;
    use hew_cabi::string::string_release;

    use super::*;

    #[test]
    fn hash_produces_valid_phc_string() {
        let pw = ManagedString::new("hunter2");
        // SAFETY: pw is a live managed string handle.
        let hash_ptr = unsafe { hew_password_hash(pw.as_ptr()) };
        assert!(!hash_ptr.is_null());
        // SAFETY: hash_ptr is a live managed string owned by this test.
        let encoded = unsafe { string_as_str(hash_ptr) };
        assert!(encoded.starts_with("$argon2id$"));
        // SAFETY: hash_ptr was returned by hew_password_hash.
        unsafe { string_release(hash_ptr) };
    }

    #[test]
    fn verify_correct_password_returns_1() {
        let pw = ManagedString::new("correct-horse-battery-staple");
        // SAFETY: pw is a live managed string handle.
        let hash_ptr = unsafe { hew_password_hash(pw.as_ptr()) };
        assert!(!hash_ptr.is_null());
        // SAFETY: both are live managed string handles.
        let result = unsafe { hew_password_verify(pw.as_ptr(), hash_ptr) };
        assert_eq!(result, 1);
        // SAFETY: hash_ptr was returned by hew_password_hash.
        unsafe { string_release(hash_ptr) };
    }

    #[test]
    fn verify_wrong_password_returns_0() {
        let pw = ManagedString::new("right-password");
        let wrong = ManagedString::new("wrong-password");
        // SAFETY: pw is a live managed string handle.
        let hash_ptr = unsafe { hew_password_hash(pw.as_ptr()) };
        assert!(!hash_ptr.is_null());
        // SAFETY: both are live managed string handles.
        let result = unsafe { hew_password_verify(wrong.as_ptr(), hash_ptr) };
        assert_eq!(result, 0);
        // SAFETY: hash_ptr was returned by hew_password_hash.
        unsafe { string_release(hash_ptr) };
    }

    #[test]
    fn custom_cost_produces_valid_hash() {
        let pw = ManagedString::new("custom-test");
        // SAFETY: pw is a live managed string handle; cost=2 is a valid iteration count.
        let hash_ptr = unsafe { hew_password_hash_custom(pw.as_ptr(), 2) };
        assert!(!hash_ptr.is_null());
        // SAFETY: hash_ptr is a live managed string owned by this test.
        let encoded = unsafe { string_as_str(hash_ptr) };
        assert!(encoded.starts_with("$argon2id$"));
        // Verify the custom hash works with verify
        // SAFETY: both are live managed string handles.
        let result = unsafe { hew_password_verify(pw.as_ptr(), hash_ptr) };
        assert_eq!(result, 1);
        // SAFETY: hash_ptr was returned by hew_password_hash_custom.
        unsafe { string_release(hash_ptr) };
    }

    /// A null password/hash is the canonical empty string under the
    /// managed-string ABI, not a distinguishable "missing" marker: hashing a
    /// null password succeeds exactly like hashing an explicit `""` (see
    /// `empty_password_hashes_and_verifies`), and verifying with a null hash
    /// still fails because `""` is not a valid PHC string.
    #[test]
    fn null_password_hashes_like_empty_and_null_hash_is_malformed() {
        // SAFETY: null is the canonical empty managed string.
        let hash_ptr = unsafe { hew_password_hash(std::ptr::null()) };
        assert!(
            !hash_ptr.is_null(),
            "a null (empty) password hashes successfully"
        );
        // SAFETY: null is the canonical empty managed string, and the empty
        // hash is never a valid PHC string.
        let verified = unsafe { hew_password_verify(std::ptr::null(), std::ptr::null()) };
        assert_eq!(verified, -1);
        // SAFETY: null is the canonical empty managed string.
        assert!(!unsafe { hew_password_hash_custom(std::ptr::null(), 2) }.is_null());
        // SAFETY: hash_ptr was returned by hew_password_hash.
        unsafe { string_release(hash_ptr) };
    }

    #[test]
    fn invalid_cost_returns_null() {
        let pw = ManagedString::new("test");
        // SAFETY: pw is a live managed string handle; cost=0 is invalid.
        assert!(unsafe { hew_password_hash_custom(pw.as_ptr(), 0) }.is_null());
        // SAFETY: pw is a live managed string handle; cost=-1 is invalid.
        assert!(unsafe { hew_password_hash_custom(pw.as_ptr(), -1) }.is_null());
    }

    /// Empty password is valid — should hash and verify. A null pointer is
    /// the same managed value as an explicit `""` (see
    /// `null_password_hashes_like_empty_and_null_hash_is_malformed`).
    #[test]
    fn empty_password_hashes_and_verifies() {
        let pw = ManagedString::new("");
        // SAFETY: pw is a live managed string handle (empty).
        let hash_ptr = unsafe { hew_password_hash(pw.as_ptr()) };
        assert!(!hash_ptr.is_null());
        // SAFETY: both are live managed string handles.
        let result = unsafe { hew_password_verify(pw.as_ptr(), hash_ptr) };
        assert_eq!(result, 1, "empty password should verify against its hash");
        // SAFETY: hash_ptr was returned by hew_password_hash.
        unsafe { string_release(hash_ptr) };
    }

    /// Verify with a malformed (non-PHC) hash string returns -1.
    #[test]
    fn verify_malformed_hash_returns_error() {
        let pw = ManagedString::new("password");
        let bad_hash = ManagedString::new("not-a-phc-hash");
        // SAFETY: both are live managed string handles.
        let result = unsafe { hew_password_verify(pw.as_ptr(), bad_hash.as_ptr()) };
        assert_eq!(result, -1, "malformed hash must return -1");
    }

    /// Two hashes of the same password differ (random salt).
    #[test]
    fn same_password_produces_different_hashes() {
        let pw = ManagedString::new("same-password");
        // SAFETY: pw is a live managed string handle.
        let hash1_ptr = unsafe { hew_password_hash(pw.as_ptr()) };
        // SAFETY: pw is a live managed string handle.
        let hash2_ptr = unsafe { hew_password_hash(pw.as_ptr()) };
        assert!(!hash1_ptr.is_null());
        assert!(!hash2_ptr.is_null());
        // SAFETY: hash1_ptr and hash2_ptr are live managed strings owned by this test.
        let (hash1, hash2) = unsafe { (string_as_str(hash1_ptr), string_as_str(hash2_ptr)) };
        assert_ne!(hash1, hash2, "random salt should produce different hashes");
        // Both should still verify.
        // SAFETY: both are live managed string handles.
        assert_eq!(unsafe { hew_password_verify(pw.as_ptr(), hash1_ptr) }, 1);
        // SAFETY: both are live managed string handles.
        assert_eq!(unsafe { hew_password_verify(pw.as_ptr(), hash2_ptr) }, 1);
        // SAFETY: both were returned by hew_password_hash.
        unsafe {
            string_release(hash1_ptr);
            string_release(hash2_ptr);
        }
    }

    /// Custom-cost hash is verifiable by the default verifier.
    #[test]
    fn custom_cost_hash_verified_by_default_verifier() {
        let pw = ManagedString::new("cross-verify");
        // SAFETY: pw is a live managed string handle; cost=2 is valid.
        let hash_ptr = unsafe { hew_password_hash_custom(pw.as_ptr(), 2) };
        assert!(!hash_ptr.is_null());
        // Default verifier should accept it.
        // SAFETY: both are live managed string handles.
        let result = unsafe { hew_password_verify(pw.as_ptr(), hash_ptr) };
        assert_eq!(result, 1);
        // SAFETY: hash_ptr was returned by hew_password_hash_custom.
        unsafe { string_release(hash_ptr) };
    }

    /// Negative cost values are rejected (i32 → u32 conversion fails).
    #[test]
    fn extreme_negative_cost_returns_null() {
        let pw = ManagedString::new("test");
        // SAFETY: pw is a live managed string handle.
        assert!(unsafe { hew_password_hash_custom(pw.as_ptr(), i32::MIN) }.is_null());
    }
}
