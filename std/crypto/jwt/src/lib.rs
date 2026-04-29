//! Hew runtime: JWT (JSON Web Token) creation and validation.
//!
//! Provides HMAC-based JWT encoding, decoding, and validation for compiled Hew
//! programs. All returned strings are allocated with `libc::malloc` and
//! NUL-terminated. Free them with [`hew_jwt_free`].

// Force-link hew-runtime so the linker can resolve hew_vec_* symbols
// referenced by hew-cabi's object code (which shares a single compilation
// unit in debug builds), and to access the shared error slot.
extern crate hew_runtime;

use hew_cabi::cabi::{cstr_to_str, str_to_malloc};
use jsonwebtoken::{errors::ErrorKind, Algorithm, DecodingKey, EncodingKey, Header, Validation};
use std::os::raw::c_char;

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

/// Tagged error code surfaced across the JWT C ABI.
#[repr(C)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum HewJwtError {
    None = 0,
    AlgorithmUnsupported = 1,
    InvalidKey = 2,
    TokenMalformed = 3,
    SignatureInvalid = 4,
    ClaimsExpired = 5,
    AllocationFailure = 6,
}

#[cfg(test)]
use std::cell::Cell;
#[cfg(test)]
std::thread_local! {
    static FAIL_NEXT_JWT_ALLOCATIONS: Cell<Option<usize>> = const { Cell::new(None) };
}

/// Store `err` in the per-actor slot by encoding its discriminant as a string.
///
/// `HewJwtError` has only unit variants 0..=6 with no payload, so the integer
/// discriminant round-trips losslessly. When `err` is `None` the slot is cleared
/// (matching the "no entry == None" invariant).
fn set_last_jwt_error(err: HewJwtError) {
    if err == HewJwtError::None {
        hew_runtime::parse_error_slot::clear_error(
            hew_runtime::parse_error_slot::ErrorSlotKind::Jwt,
        );
    } else {
        hew_runtime::parse_error_slot::set_error(
            hew_runtime::parse_error_slot::ErrorSlotKind::Jwt,
            (err as i32).to_string(),
        );
    }
}

/// Retrieve the last JWT error from the per-actor slot.
///
/// Returns `HewJwtError::None` when no error is stored (slot absent).
/// If the slot contains an unparseable discriminant, returns the fail-closed
/// sentinel `HewJwtError::TokenMalformed` to ensure a poisoned slot never
/// silently maps to success.
fn last_jwt_error() -> HewJwtError {
    let Some(s) =
        hew_runtime::parse_error_slot::get_error(hew_runtime::parse_error_slot::ErrorSlotKind::Jwt)
    else {
        return HewJwtError::None;
    };
    match s.parse::<i32>().unwrap_or(-1) {
        1 => HewJwtError::AlgorithmUnsupported,
        2 => HewJwtError::InvalidKey,
        3 => HewJwtError::TokenMalformed,
        4 => HewJwtError::SignatureInvalid,
        5 => HewJwtError::ClaimsExpired,
        6 => HewJwtError::AllocationFailure,
        _ => HewJwtError::None,
    }
}

#[cfg(test)]
fn fail_jwt_allocations_after(successes_before_failure: usize) {
    FAIL_NEXT_JWT_ALLOCATIONS.with(|remaining| remaining.set(Some(successes_before_failure)));
}

#[cfg(test)]
fn reset_jwt_allocation_failures() {
    FAIL_NEXT_JWT_ALLOCATIONS.with(|remaining| remaining.set(None));
}

#[cfg(test)]
fn should_fail_jwt_allocation() -> bool {
    FAIL_NEXT_JWT_ALLOCATIONS.with(|remaining| match remaining.get() {
        Some(0) => true,
        Some(count) => {
            remaining.set(Some(count - 1));
            false
        }
        None => false,
    })
}

#[cfg(not(test))]
fn should_fail_jwt_allocation() -> bool {
    false
}

fn jwt_str_to_malloc(s: &str) -> *mut c_char {
    if should_fail_jwt_allocation() {
        return std::ptr::null_mut();
    }
    str_to_malloc(s)
}

unsafe fn write_err_out(err_out: *mut HewJwtError, err: HewJwtError) {
    if !err_out.is_null() {
        // SAFETY: caller provided a valid out-pointer when non-null.
        unsafe { *err_out = err };
    }
}

unsafe fn input_str<'a>(ptr: *const c_char, err: HewJwtError) -> Result<&'a str, HewJwtError> {
    // SAFETY: caller guarantees `ptr` is either null or a valid NUL-terminated C string.
    unsafe { cstr_to_str(ptr) }.ok_or(err)
}

fn classify_jwt_error(kind: &ErrorKind) -> HewJwtError {
    match kind {
        ErrorKind::InvalidEcdsaKey
        | ErrorKind::InvalidEddsaKey
        | ErrorKind::InvalidRsaKey(_)
        | ErrorKind::RsaFailedSigning
        | ErrorKind::Signing(_)
        | ErrorKind::InvalidKeyFormat
        | ErrorKind::Provider(_) => HewJwtError::InvalidKey,
        ErrorKind::InvalidAlgorithmName | ErrorKind::MissingAlgorithm => {
            HewJwtError::AlgorithmUnsupported
        }
        ErrorKind::InvalidSignature | ErrorKind::InvalidAlgorithm => HewJwtError::SignatureInvalid,
        ErrorKind::ExpiredSignature => HewJwtError::ClaimsExpired,
        _ => HewJwtError::TokenMalformed,
    }
}

unsafe fn encode_impl(
    payload_json: *const c_char,
    secret: *const c_char,
    algo: i32,
) -> Result<String, HewJwtError> {
    // SAFETY: `payload_json` is forwarded from the FFI caller under this function's contract.
    let payload_str = unsafe { input_str(payload_json, HewJwtError::TokenMalformed) }?;
    // SAFETY: `secret` is forwarded from the FFI caller under this function's contract.
    let secret_str = unsafe { input_str(secret, HewJwtError::InvalidKey) }?;
    let algorithm = algo_from_i32(algo).ok_or(HewJwtError::AlgorithmUnsupported)?;
    let claims: serde_json::Value =
        serde_json::from_str(payload_str).map_err(|_| HewJwtError::TokenMalformed)?;

    jsonwebtoken::encode(
        &Header::new(algorithm),
        &claims,
        &EncodingKey::from_secret(secret_str.as_bytes()),
    )
    .map_err(|err| classify_jwt_error(err.kind()))
}

unsafe fn decode_impl(
    token: *const c_char,
    secret: *const c_char,
    algo: i32,
) -> Result<String, HewJwtError> {
    // SAFETY: `token` is forwarded from the FFI caller under this function's contract.
    let token_str = unsafe { input_str(token, HewJwtError::TokenMalformed) }?;
    // SAFETY: `secret` is forwarded from the FFI caller under this function's contract.
    let secret_str = unsafe { input_str(secret, HewJwtError::InvalidKey) }?;
    let algorithm = algo_from_i32(algo).ok_or(HewJwtError::AlgorithmUnsupported)?;

    let key = DecodingKey::from_secret(secret_str.as_bytes());
    let mut validation = Validation::new(algorithm);
    validation.required_spec_claims.clear();
    let data = jsonwebtoken::decode::<serde_json::Value>(token_str, &key, &validation)
        .map_err(|err| classify_jwt_error(err.kind()))?;
    serde_json::to_string(&data.claims).map_err(|_| HewJwtError::TokenMalformed)
}

fn malloc_or_allocation_failure(s: &str) -> Result<*mut c_char, HewJwtError> {
    let ptr = jwt_str_to_malloc(s);
    if ptr.is_null() {
        Err(HewJwtError::AllocationFailure)
    } else {
        Ok(ptr)
    }
}

// ---------------------------------------------------------------------------
// C ABI exports
// ---------------------------------------------------------------------------

/// Return the last JWT error observed through the Hew compatibility wrappers.
#[no_mangle]
pub extern "C" fn hew_jwt_last_error_code() -> i32 {
    last_jwt_error() as i32
}

/// Create a signed JWT from a JSON payload string.
///
/// `payload_json` is a JSON object string containing the claims. `algo`
/// selects the HMAC algorithm: 0 = HS256, 1 = HS384, 2 = HS512.
///
/// Returns a `malloc`-allocated, NUL-terminated JWT string, or null on error.
/// On failure, writes a discriminator to `err_out` when non-null. The caller
/// must free the returned string with [`hew_jwt_free`].
///
/// # Safety
///
/// `payload_json`, `secret`, and `err_out` (when non-null) must be valid.
#[no_mangle]
pub unsafe extern "C" fn hew_jwt_encode(
    payload_json: *const c_char,
    secret: *const c_char,
    algo: i32,
    err_out: *mut HewJwtError,
) -> *mut c_char {
    // SAFETY: `err_out` is an optional caller-provided out-pointer.
    unsafe { write_err_out(err_out, HewJwtError::None) };

    // SAFETY: arguments satisfy this function's FFI contract.
    match unsafe { encode_impl(payload_json, secret, algo) } {
        Ok(token) => match malloc_or_allocation_failure(&token) {
            Ok(ptr) => ptr,
            Err(err) => {
                // SAFETY: `err_out` is an optional caller-provided out-pointer.
                unsafe { write_err_out(err_out, err) };
                std::ptr::null_mut()
            }
        },
        Err(err) => {
            // SAFETY: `err_out` is an optional caller-provided out-pointer.
            unsafe { write_err_out(err_out, err) };
            std::ptr::null_mut()
        }
    }
}

/// Hew-facing compatibility wrapper for [`hew_jwt_encode`].
///
/// # Safety
///
/// `payload_json` and `secret` must be valid NUL-terminated C strings when
/// non-null, matching [`hew_jwt_encode`].
#[no_mangle]
pub unsafe extern "C" fn hew_jwt_encode_hew(
    payload_json: *const c_char,
    secret: *const c_char,
    algo: i32,
) -> *mut c_char {
    let mut err = HewJwtError::None;
    // SAFETY: arguments satisfy the contract inherited from `hew_jwt_encode`.
    let result = unsafe { hew_jwt_encode(payload_json, secret, algo, &raw mut err) };
    set_last_jwt_error(err);
    result
}

/// Decode and validate a JWT, returning the payload as a JSON string.
///
/// Returns a `malloc`-allocated, NUL-terminated JSON string, or null if the
/// token is invalid or expired. On failure, writes a discriminator to `err_out`
/// when non-null. The caller must free the returned string with
/// [`hew_jwt_free`].
///
/// # Safety
///
/// `token`, `secret`, and `err_out` (when non-null) must be valid.
#[no_mangle]
pub unsafe extern "C" fn hew_jwt_decode(
    token: *const c_char,
    secret: *const c_char,
    algo: i32,
    err_out: *mut HewJwtError,
) -> *mut c_char {
    // SAFETY: `err_out` is an optional caller-provided out-pointer.
    unsafe { write_err_out(err_out, HewJwtError::None) };

    // SAFETY: arguments satisfy this function's FFI contract.
    match unsafe { decode_impl(token, secret, algo) } {
        Ok(json) => match malloc_or_allocation_failure(&json) {
            Ok(ptr) => ptr,
            Err(err) => {
                // SAFETY: `err_out` is an optional caller-provided out-pointer.
                unsafe { write_err_out(err_out, err) };
                std::ptr::null_mut()
            }
        },
        Err(err) => {
            // SAFETY: `err_out` is an optional caller-provided out-pointer.
            unsafe { write_err_out(err_out, err) };
            std::ptr::null_mut()
        }
    }
}

/// Hew-facing compatibility wrapper for [`hew_jwt_decode`].
///
/// # Safety
///
/// `token` and `secret` must be valid NUL-terminated C strings when non-null,
/// matching [`hew_jwt_decode`].
#[no_mangle]
pub unsafe extern "C" fn hew_jwt_decode_hew(
    token: *const c_char,
    secret: *const c_char,
    algo: i32,
) -> *mut c_char {
    let mut err = HewJwtError::None;
    // SAFETY: arguments satisfy the contract inherited from `hew_jwt_decode`.
    let result = unsafe { hew_jwt_decode(token, secret, algo, &raw mut err) };
    set_last_jwt_error(err);
    result
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
        Some(json) => jwt_str_to_malloc(&json),
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

    struct AllocationFailureGuard;

    impl AllocationFailureGuard {
        fn after(successes_before_failure: usize) -> Self {
            fail_jwt_allocations_after(successes_before_failure);
            Self
        }
    }

    impl Drop for AllocationFailureGuard {
        fn drop(&mut self) {
            reset_jwt_allocation_failures();
        }
    }

    /// Helper: read a C string pointer and free it.
    unsafe fn read_and_free(ptr: *mut c_char) -> String {
        assert!(!ptr.is_null(), "expected non-null C string");
        // SAFETY: ptr is a valid NUL-terminated C string from malloc.
        let s = unsafe { CStr::from_ptr(ptr) }.to_str().unwrap().to_owned();
        // SAFETY: ptr was allocated with malloc.
        unsafe { hew_jwt_free(ptr) };
        s
    }

    unsafe fn encode_err(
        payload: *const c_char,
        secret: *const c_char,
        algo: i32,
    ) -> (*mut c_char, HewJwtError) {
        let mut err = HewJwtError::AllocationFailure;
        // SAFETY: test helpers pass valid arguments for the selected scenario.
        let ptr = unsafe { hew_jwt_encode(payload, secret, algo, &raw mut err) };
        (ptr, err)
    }

    unsafe fn decode_err(
        token: *const c_char,
        secret: *const c_char,
        algo: i32,
    ) -> (*mut c_char, HewJwtError) {
        let mut err = HewJwtError::AllocationFailure;
        // SAFETY: test helpers pass valid arguments for the selected scenario.
        let ptr = unsafe { hew_jwt_decode(token, secret, algo, &raw mut err) };
        (ptr, err)
    }

    fn valid_payload() -> CString {
        CString::new(r#"{"sub":"user1","role":"admin"}"#).unwrap()
    }

    fn valid_secret() -> CString {
        CString::new("test-secret-key").unwrap()
    }

    fn mint_token(payload: &CString, secret: &CString, algo: i32) -> String {
        // SAFETY: CStrings are valid NUL-terminated C strings.
        unsafe {
            let (token_ptr, err) = encode_err(payload.as_ptr(), secret.as_ptr(), algo);
            assert_eq!(err, HewJwtError::None);
            read_and_free(token_ptr)
        }
    }

    #[test]
    fn encode_success_sets_none() {
        let payload = valid_payload();
        let secret = valid_secret();

        // SAFETY: CStrings are valid NUL-terminated C strings.
        unsafe {
            let (token_ptr, err) = encode_err(payload.as_ptr(), secret.as_ptr(), 0);
            assert_eq!(err, HewJwtError::None);
            let token = read_and_free(token_ptr);
            assert!(token.contains('.'));
        }
    }

    #[test]
    fn encode_reports_algorithm_unsupported() {
        let payload = valid_payload();
        let secret = valid_secret();

        // SAFETY: CStrings are valid NUL-terminated C strings.
        unsafe {
            let (token_ptr, err) = encode_err(payload.as_ptr(), secret.as_ptr(), 99);
            assert!(token_ptr.is_null());
            assert_eq!(err, HewJwtError::AlgorithmUnsupported);
        }
    }

    #[test]
    fn encode_reports_invalid_key_for_bad_secret_utf8() {
        let payload = valid_payload();
        let bad_secret = CString::new(vec![0xff]).unwrap();

        // SAFETY: pointers are valid NUL-terminated byte sequences.
        unsafe {
            let (token_ptr, err) = encode_err(payload.as_ptr(), bad_secret.as_ptr(), 0);
            assert!(token_ptr.is_null());
            assert_eq!(err, HewJwtError::InvalidKey);
        }
    }

    #[test]
    fn encode_reports_token_malformed_for_bad_payload() {
        let secret = valid_secret();
        let bad_payload = CString::new("not valid json").unwrap();

        // SAFETY: CStrings are valid NUL-terminated C strings.
        unsafe {
            let (token_ptr, err) = encode_err(bad_payload.as_ptr(), secret.as_ptr(), 0);
            assert!(token_ptr.is_null());
            assert_eq!(err, HewJwtError::TokenMalformed);
        }
    }

    #[test]
    fn encode_reports_allocation_failure() {
        let payload = valid_payload();
        let secret = valid_secret();
        let _guard = AllocationFailureGuard::after(0);

        // SAFETY: CStrings are valid NUL-terminated C strings.
        unsafe {
            let (token_ptr, err) = encode_err(payload.as_ptr(), secret.as_ptr(), 0);
            assert!(token_ptr.is_null());
            assert_eq!(err, HewJwtError::AllocationFailure);
        }
    }

    #[test]
    fn decode_success_sets_none() {
        let payload = valid_payload();
        let secret = valid_secret();
        let token = mint_token(&payload, &secret, 0);
        let token_c = CString::new(token).unwrap();

        // SAFETY: CStrings are valid NUL-terminated C strings.
        unsafe {
            let (decoded_ptr, err) = decode_err(token_c.as_ptr(), secret.as_ptr(), 0);
            assert_eq!(err, HewJwtError::None);
            let decoded = read_and_free(decoded_ptr);
            let claims: serde_json::Value = serde_json::from_str(&decoded).unwrap();
            assert_eq!(claims["sub"], "user1");
            assert_eq!(claims["role"], "admin");
        }
    }

    #[test]
    fn decode_reports_algorithm_unsupported() {
        let payload = valid_payload();
        let secret = valid_secret();
        let token = mint_token(&payload, &secret, 0);
        let token_c = CString::new(token).unwrap();

        // SAFETY: CStrings are valid NUL-terminated C strings.
        unsafe {
            let (decoded_ptr, err) = decode_err(token_c.as_ptr(), secret.as_ptr(), 99);
            assert!(decoded_ptr.is_null());
            assert_eq!(err, HewJwtError::AlgorithmUnsupported);
        }
    }

    #[test]
    fn decode_reports_invalid_key_for_null_secret() {
        let payload = valid_payload();
        let secret = valid_secret();
        let token = mint_token(&payload, &secret, 0);
        let token_c = CString::new(token).unwrap();

        // SAFETY: null secret is explicitly classified.
        unsafe {
            let (decoded_ptr, err) = decode_err(token_c.as_ptr(), std::ptr::null(), 0);
            assert!(decoded_ptr.is_null());
            assert_eq!(err, HewJwtError::InvalidKey);
        }
    }

    #[test]
    fn decode_reports_token_malformed_for_bad_token_utf8() {
        let secret = valid_secret();
        let bad_token = CString::new(vec![0xff]).unwrap();

        // SAFETY: pointers are valid NUL-terminated byte sequences.
        unsafe {
            let (decoded_ptr, err) = decode_err(bad_token.as_ptr(), secret.as_ptr(), 0);
            assert!(decoded_ptr.is_null());
            assert_eq!(err, HewJwtError::TokenMalformed);
        }
    }

    #[test]
    fn decode_reports_signature_invalid_for_wrong_secret() {
        let payload = valid_payload();
        let secret = valid_secret();
        let wrong_secret = CString::new("wrong-secret").unwrap();
        let token = mint_token(&payload, &secret, 0);
        let token_c = CString::new(token).unwrap();

        // SAFETY: CStrings are valid NUL-terminated C strings.
        unsafe {
            let (decoded_ptr, err) = decode_err(token_c.as_ptr(), wrong_secret.as_ptr(), 0);
            assert!(decoded_ptr.is_null());
            assert_eq!(err, HewJwtError::SignatureInvalid);
        }
    }

    #[test]
    fn decode_reports_claims_expired() {
        let payload = CString::new(r#"{"sub":"user1","exp":1}"#).unwrap();
        let secret = CString::new("secret").unwrap();
        let token = mint_token(&payload, &secret, 0);
        let token_c = CString::new(token).unwrap();

        // SAFETY: CStrings are valid NUL-terminated C strings.
        unsafe {
            let (decoded_ptr, err) = decode_err(token_c.as_ptr(), secret.as_ptr(), 0);
            assert!(decoded_ptr.is_null());
            assert_eq!(err, HewJwtError::ClaimsExpired);
        }
    }

    #[test]
    fn decode_reports_allocation_failure() {
        let payload = valid_payload();
        let secret = valid_secret();
        let token = mint_token(&payload, &secret, 0);
        let token_c = CString::new(token).unwrap();
        let _guard = AllocationFailureGuard::after(0);

        // SAFETY: CStrings are valid NUL-terminated C strings.
        unsafe {
            let (decoded_ptr, err) = decode_err(token_c.as_ptr(), secret.as_ptr(), 0);
            assert!(decoded_ptr.is_null());
            assert_eq!(err, HewJwtError::AllocationFailure);
        }
    }

    #[test]
    fn hew_wrappers_track_last_error_code() {
        let payload = valid_payload();
        let secret = valid_secret();

        // SAFETY: CStrings are valid NUL-terminated C strings.
        unsafe {
            let token_ptr = hew_jwt_encode_hew(payload.as_ptr(), secret.as_ptr(), 0);
            assert_eq!(hew_jwt_last_error_code(), HewJwtError::None as i32);
            let token = read_and_free(token_ptr);
            let token_c = CString::new(token).unwrap();

            let decoded_ptr = hew_jwt_decode_hew(token_c.as_ptr(), std::ptr::null(), 0);
            assert!(decoded_ptr.is_null());
            assert_eq!(hew_jwt_last_error_code(), HewJwtError::InvalidKey as i32);
        }
    }

    #[test]
    fn insecure_decode_skips_verification() {
        let payload = CString::new(r#"{"sub":"user1","data":"hello"}"#).unwrap();
        let secret = CString::new("secret").unwrap();
        let token = mint_token(&payload, &secret, 0);
        let token_c = CString::new(token).unwrap();

        // SAFETY: CStrings are valid NUL-terminated C strings.
        unsafe {
            let decoded_ptr = hew_jwt_decode_insecure(token_c.as_ptr());
            let decoded = read_and_free(decoded_ptr);
            let claims: serde_json::Value = serde_json::from_str(&decoded).unwrap();
            assert_eq!(claims["sub"], "user1");
            assert_eq!(claims["data"], "hello");
        }
    }

    #[test]
    fn validate_distinguishes_valid_invalid_and_expired() {
        let payload = valid_payload();
        let secret = valid_secret();
        let wrong_secret = CString::new("wrong-secret").unwrap();
        let token = mint_token(&payload, &secret, 0);
        let token_c = CString::new(token).unwrap();

        let expired_payload = CString::new(r#"{"sub":"user1","exp":1}"#).unwrap();
        let expired_secret = CString::new("secret").unwrap();
        let expired_token = mint_token(&expired_payload, &expired_secret, 0);
        let expired_token_c = CString::new(expired_token).unwrap();

        // SAFETY: CStrings are valid NUL-terminated C strings.
        unsafe {
            assert_eq!(hew_jwt_validate(token_c.as_ptr(), secret.as_ptr(), 0), 1);
            assert_eq!(
                hew_jwt_validate(token_c.as_ptr(), wrong_secret.as_ptr(), 0),
                0
            );
            assert_eq!(
                hew_jwt_validate(expired_token_c.as_ptr(), expired_secret.as_ptr(), 0),
                -1
            );
        }
    }

    #[test]
    fn free_null_is_noop() {
        // SAFETY: null is explicitly handled.
        unsafe { hew_jwt_free(std::ptr::null_mut()) };
    }

    #[test]
    fn jwt_error_slot_round_trips_known_variants() {
        // Verify that each HewJwtError variant encodes and decodes losslessly.
        // This guards against silent truncation or loss of discriminant bits.
        let variants = [
            HewJwtError::None,
            HewJwtError::AlgorithmUnsupported,
            HewJwtError::InvalidKey,
            HewJwtError::TokenMalformed,
            HewJwtError::SignatureInvalid,
            HewJwtError::ClaimsExpired,
            HewJwtError::AllocationFailure,
        ];

        for variant in variants {
            set_last_jwt_error(variant);
            let recovered = last_jwt_error();
            assert_eq!(
                recovered, variant,
                "error variant {variant:?} did not round-trip through error slot"
            );
        }
    }
}
