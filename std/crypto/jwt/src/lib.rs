//! Hew runtime: JWT (JSON Web Token) creation and validation.
//!
//! Provides HMAC-based JWT encoding, decoding, and validation for compiled Hew
//! programs. All returned strings are allocated with `libc::malloc` and
//! NUL-terminated. Free them with [`hew_jwt_free`].

use hew_cabi::cabi::{cstr_to_str, str_to_malloc};
use std::os::raw::c_char;

use jsonwebtoken::{Algorithm, DecodingKey, EncodingKey, Header, Validation};

/// Map an algorithm integer to a [`jsonwebtoken::Algorithm`].
///
/// 0 = HS256, 1 = HS384, 2 = HS512. Returns `None` for unknown values.
fn algo_from_i32(algo: i32) -> Option<Algorithm> {
    match algo {
        0 => Some(Algorithm::HS256),
        1 => Some(Algorithm::HS384),
        2 => Some(Algorithm::HS512),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// C ABI exports
// ---------------------------------------------------------------------------

/// Create a signed JWT from a JSON payload string.
///
/// `payload_json` is a JSON object string containing the claims. `algo`
/// selects the HMAC algorithm: 0 = HS256, 1 = HS384, 2 = HS512.
///
/// Returns a `malloc`-allocated, NUL-terminated JWT string, or null on error.
/// The caller must free the returned string with [`hew_jwt_free`].
///
/// # Safety
///
/// `payload_json` and `secret` must be valid NUL-terminated C strings.
#[no_mangle]
pub unsafe extern "C" fn hew_jwt_encode(
    payload_json: *const c_char,
    secret: *const c_char,
    algo: i32,
) -> *mut c_char {
    let encode_inner = || -> Option<String> {
        // SAFETY: Caller guarantees valid NUL-terminated C strings.
        let payload_str = unsafe { cstr_to_str(payload_json) }?;
        // SAFETY: Caller guarantees valid NUL-terminated C strings.
        let secret_str = unsafe { cstr_to_str(secret) }?;
        let algorithm = algo_from_i32(algo)?;

        let claims: serde_json::Value = serde_json::from_str(payload_str).ok()?;

        let header = Header::new(algorithm);
        let key = EncodingKey::from_secret(secret_str.as_bytes());
        jsonwebtoken::encode(&header, &claims, &key).ok()
    };

    match encode_inner() {
        Some(token) => str_to_malloc(&token),
        None => std::ptr::null_mut(),
    }
}

/// Decode and validate a JWT, returning the payload as a JSON string.
///
/// Returns a `malloc`-allocated, NUL-terminated JSON string, or null if the
/// token is invalid or expired. The caller must free the returned string with
/// [`hew_jwt_free`].
///
/// # Safety
///
/// `token` and `secret` must be valid NUL-terminated C strings.
#[no_mangle]
pub unsafe extern "C" fn hew_jwt_decode(
    token: *const c_char,
    secret: *const c_char,
    algo: i32,
) -> *mut c_char {
    let decode_inner = || -> Option<String> {
        // SAFETY: Caller guarantees valid NUL-terminated C strings.
        let token_str = unsafe { cstr_to_str(token) }?;
        // SAFETY: Caller guarantees valid NUL-terminated C strings.
        let secret_str = unsafe { cstr_to_str(secret) }?;
        let algorithm = algo_from_i32(algo)?;

        let key = DecodingKey::from_secret(secret_str.as_bytes());
        let mut validation = Validation::new(algorithm);
        validation.required_spec_claims.clear();
        let data = jsonwebtoken::decode::<serde_json::Value>(token_str, &key, &validation).ok()?;
        serde_json::to_string(&data.claims).ok()
    };

    match decode_inner() {
        Some(json) => str_to_malloc(&json),
        None => std::ptr::null_mut(),
    }
}

/// Decode a JWT without signature verification (for inspection only).
///
/// **Warning:** This does NOT verify the token's signature or validate claims.
/// Do not trust the output for security decisions.
///
/// Returns a `malloc`-allocated, NUL-terminated JSON string, or null on error.
/// The caller must free the returned string with [`hew_jwt_free`].
///
/// # Safety
///
/// `token` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_jwt_decode_insecure(token: *const c_char) -> *mut c_char {
    let decode_inner = || -> Option<String> {
        // SAFETY: Caller guarantees a valid NUL-terminated C string.
        let token_str = unsafe { cstr_to_str(token) }?;

        let data = jsonwebtoken::dangerous::insecure_decode::<serde_json::Value>(token_str).ok()?;
        serde_json::to_string(&data.claims).ok()
    };

    match decode_inner() {
        Some(json) => str_to_malloc(&json),
        None => std::ptr::null_mut(),
    }
}

/// Validate a JWT without returning the payload.
///
/// Returns:
/// -  `1` — valid
/// -  `0` — invalid (bad signature, malformed, etc.)
/// - `-1` — expired
/// - `-2` — error (null inputs, unknown algorithm)
///
/// # Safety
///
/// `token` and `secret` must be valid NUL-terminated C strings.
#[no_mangle]
pub unsafe extern "C" fn hew_jwt_validate(
    token: *const c_char,
    secret: *const c_char,
    algo: i32,
) -> i32 {
    // SAFETY: Caller guarantees valid NUL-terminated C strings.
    let Some(token_str) = (unsafe { cstr_to_str(token) }) else {
        return -2;
    };
    // SAFETY: Caller guarantees a valid NUL-terminated C string.
    let Some(secret_str) = (unsafe { cstr_to_str(secret) }) else {
        return -2;
    };
    let Some(algorithm) = algo_from_i32(algo) else {
        return -2;
    };

    let key = DecodingKey::from_secret(secret_str.as_bytes());
    let mut validation = Validation::new(algorithm);
    validation.required_spec_claims.clear();

    match jsonwebtoken::decode::<serde_json::Value>(token_str, &key, &validation) {
        Ok(_) => 1,
        Err(e) => match e.kind() {
            jsonwebtoken::errors::ErrorKind::ExpiredSignature => -1,
            _ => 0,
        },
    }
}

/// Free a C string previously returned by [`hew_jwt_encode`],
/// [`hew_jwt_decode`], or [`hew_jwt_decode_insecure`].
///
/// # Safety
///
/// `s` must be a pointer previously returned by a `hew_jwt_*` function,
/// and must not have been freed already.
#[no_mangle]
pub unsafe extern "C" fn hew_jwt_free(s: *mut c_char) {
    if s.is_null() {
        return;
    }
    // SAFETY: s was allocated with libc::malloc and has not been freed.
    unsafe { libc::free(s.cast()) };
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::{CStr, CString};

    /// Helper: read a C string pointer and free it.
    unsafe fn read_and_free(ptr: *mut c_char) -> String {
        assert!(!ptr.is_null(), "expected non-null C string");
        // SAFETY: ptr is a valid NUL-terminated C string from malloc.
        let s = unsafe { CStr::from_ptr(ptr) }.to_str().unwrap().to_owned();
        // SAFETY: ptr was allocated with malloc.
        unsafe { hew_jwt_free(ptr) };
        s
    }

    #[test]
    fn encode_decode_roundtrip() {
        let payload = CString::new(r#"{"sub":"user1","role":"admin"}"#).unwrap();
        let secret = CString::new("test-secret-key").unwrap();

        // SAFETY: CStrings are valid NUL-terminated C strings.
        unsafe {
            let token_ptr = hew_jwt_encode(payload.as_ptr(), secret.as_ptr(), 0);
            assert!(!token_ptr.is_null());
            let token = read_and_free(token_ptr);
            assert!(token.contains('.'), "JWT should contain dots");

            // Re-encode to get a pointer for decode
            let token_c = CString::new(token).unwrap();
            let decoded_ptr = hew_jwt_decode(token_c.as_ptr(), secret.as_ptr(), 0);
            let decoded = read_and_free(decoded_ptr);

            let claims: serde_json::Value = serde_json::from_str(&decoded).unwrap();
            assert_eq!(claims["sub"], "user1");
            assert_eq!(claims["role"], "admin");
        }
    }

    #[test]
    fn invalid_token_returns_null() {
        let bad_token = CString::new("not.a.valid.jwt").unwrap();
        let secret = CString::new("secret").unwrap();

        // SAFETY: CStrings are valid NUL-terminated C strings.
        unsafe {
            let result = hew_jwt_decode(bad_token.as_ptr(), secret.as_ptr(), 0);
            assert!(result.is_null());

            let status = hew_jwt_validate(bad_token.as_ptr(), secret.as_ptr(), 0);
            assert_eq!(status, 0);
        }
    }

    #[test]
    fn expired_token_detected() {
        // Create a token with an `exp` claim in the past.
        let payload = CString::new(r#"{"sub":"user1","exp":1}"#).unwrap();
        let secret = CString::new("secret").unwrap();

        // SAFETY: CStrings are valid NUL-terminated C strings.
        unsafe {
            // Encode with exp validation disabled (encode doesn't check exp).
            let token_ptr = hew_jwt_encode(payload.as_ptr(), secret.as_ptr(), 0);
            assert!(!token_ptr.is_null());
            let token = read_and_free(token_ptr);

            let token_c = CString::new(token).unwrap();

            // Decode should fail (expired).
            let decoded = hew_jwt_decode(token_c.as_ptr(), secret.as_ptr(), 0);
            assert!(decoded.is_null());

            // Validate should return -1 (expired).
            let status = hew_jwt_validate(token_c.as_ptr(), secret.as_ptr(), 0);
            assert_eq!(status, -1);
        }
    }

    #[test]
    fn insecure_decode_skips_verification() {
        let payload = CString::new(r#"{"sub":"user1","data":"hello"}"#).unwrap();
        let secret = CString::new("secret").unwrap();

        // SAFETY: CStrings are valid NUL-terminated C strings.
        unsafe {
            let token_ptr = hew_jwt_encode(payload.as_ptr(), secret.as_ptr(), 0);
            assert!(!token_ptr.is_null());
            let token = read_and_free(token_ptr);

            let token_c = CString::new(token).unwrap();

            // Insecure decode should succeed without the secret.
            let decoded_ptr = hew_jwt_decode_insecure(token_c.as_ptr());
            let decoded = read_and_free(decoded_ptr);

            let claims: serde_json::Value = serde_json::from_str(&decoded).unwrap();
            assert_eq!(claims["sub"], "user1");
            assert_eq!(claims["data"], "hello");
        }
    }

    #[test]
    fn algorithm_selection() {
        let payload = CString::new(r#"{"sub":"user1"}"#).unwrap();
        let secret = CString::new("test-key").unwrap();

        // SAFETY: CStrings are valid NUL-terminated C strings.
        unsafe {
            // HS256 (algo=0)
            let t0 = hew_jwt_encode(payload.as_ptr(), secret.as_ptr(), 0);
            assert!(!t0.is_null());
            let tok0 = read_and_free(t0);

            // HS384 (algo=1)
            let t1 = hew_jwt_encode(payload.as_ptr(), secret.as_ptr(), 1);
            assert!(!t1.is_null());
            let tok1 = read_and_free(t1);

            // HS512 (algo=2)
            let t2 = hew_jwt_encode(payload.as_ptr(), secret.as_ptr(), 2);
            assert!(!t2.is_null());
            let tok2 = read_and_free(t2);

            // Tokens should differ (different algorithms produce different signatures).
            assert_ne!(tok0, tok1);
            assert_ne!(tok1, tok2);

            // Each token should decode with its matching algorithm.
            let c0 = CString::new(tok0.clone()).unwrap();
            let c1 = CString::new(tok1.clone()).unwrap();
            let c2 = CString::new(tok2).unwrap();

            assert_eq!(hew_jwt_validate(c0.as_ptr(), secret.as_ptr(), 0), 1);
            assert_eq!(hew_jwt_validate(c1.as_ptr(), secret.as_ptr(), 1), 1);
            assert_eq!(hew_jwt_validate(c2.as_ptr(), secret.as_ptr(), 2), 1);

            // Wrong algorithm should fail.
            assert_eq!(hew_jwt_validate(c0.as_ptr(), secret.as_ptr(), 1), 0);
            assert_eq!(hew_jwt_validate(c1.as_ptr(), secret.as_ptr(), 0), 0);
        }
    }

    #[test]
    fn null_inputs_handled() {
        // SAFETY: Null pointers are explicitly handled by all functions.
        unsafe {
            assert!(hew_jwt_encode(std::ptr::null(), std::ptr::null(), 0).is_null());
            assert!(hew_jwt_decode(std::ptr::null(), std::ptr::null(), 0).is_null());
            assert!(hew_jwt_decode_insecure(std::ptr::null()).is_null());
            assert_eq!(hew_jwt_validate(std::ptr::null(), std::ptr::null(), 0), -2);

            // Invalid algorithm.
            let p = CString::new(r#"{"sub":"x"}"#).unwrap();
            let s = CString::new("secret").unwrap();
            assert!(hew_jwt_encode(p.as_ptr(), s.as_ptr(), 99).is_null());
            assert_eq!(hew_jwt_validate(p.as_ptr(), s.as_ptr(), 99), -2);
        }
    }

    #[test]
    fn wrong_secret_fails_validation() {
        let payload = CString::new(r#"{"sub":"user1"}"#).unwrap();
        let secret = CString::new("correct-secret").unwrap();
        let wrong = CString::new("wrong-secret").unwrap();

        // SAFETY: CStrings are valid NUL-terminated C strings.
        unsafe {
            let token_ptr = hew_jwt_encode(payload.as_ptr(), secret.as_ptr(), 0);
            assert!(!token_ptr.is_null());
            let token = read_and_free(token_ptr);
            let token_c = CString::new(token).unwrap();

            // Validate with wrong secret should fail.
            assert_eq!(hew_jwt_validate(token_c.as_ptr(), wrong.as_ptr(), 0), 0);

            // Decode with wrong secret should return null.
            assert!(hew_jwt_decode(token_c.as_ptr(), wrong.as_ptr(), 0).is_null());
        }
    }
}
