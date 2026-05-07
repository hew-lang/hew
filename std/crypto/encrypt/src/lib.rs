//! Hew runtime: `encrypt` module.
//!
//! Provides AES-256-GCM seal/open helpers for compiled Hew programs.
//! Returned `bytes` use the runtime `HewVec` allocation path; returned strings
//! are allocated with `libc::malloc` and must follow the standard runtime drop
//! path (`free`).
// WASM-TODO: `std::crypto::encrypt` mirrors the sibling native-only crypto
// modules and is excluded from the wasm runtime's ecosystem-FFI link set.

extern crate hew_runtime;

use hew_cabi::{
    cabi::{cstr_to_str, str_to_malloc},
    vec::{hwvec_to_u8, u8_to_hwvec, HewVec},
};
use ring::{
    aead::{self, Aad, LessSafeKey, Nonce, UnboundKey},
    rand::{SecureRandom, SystemRandom},
};
use std::{ffi::c_char, ptr};

const AES_256_KEY_LEN: usize = 32;
const NONCE_LEN: usize = 12;
const TAG_LEN: usize = 16;

const SEAL_FAILURE_MSG: &[u8] =
    b"encrypt.seal failed: expected a 32-byte key and a valid plaintext string\0";
const OPEN_FAILURE_MSG: &[u8] =
    b"encrypt.open failed: authentication failed or ciphertext was malformed\0";
const OPEN_ALLOC_FAILURE_MSG: &[u8] = b"encrypt.open failed: native string allocation failed\0";

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum EncryptError {
    InvalidKeyLength,
    RngFailure,
    ShortCiphertext,
    AuthFailed,
    InvalidUtf8,
}

fn panic_with_message(message: &'static [u8]) -> ! {
    // SAFETY: message is a valid static NUL-terminated string.
    unsafe { hew_runtime::actor::hew_panic_msg(message.as_ptr().cast::<c_char>()) };
    unreachable!("hew_panic_msg should not return");
}

unsafe fn input_bytes<'a>(ptr: *const u8, len: usize) -> Option<&'a [u8]> {
    if ptr.is_null() {
        return (len == 0).then_some(&[]);
    }
    // SAFETY: caller guarantees `ptr` is valid for `len` bytes when non-null.
    Some(unsafe { std::slice::from_raw_parts(ptr, len) })
}

fn make_key(key_bytes: &[u8]) -> Result<LessSafeKey, EncryptError> {
    if key_bytes.len() != AES_256_KEY_LEN {
        return Err(EncryptError::InvalidKeyLength);
    }
    let key = UnboundKey::new(&aead::AES_256_GCM, key_bytes)
        .map_err(|_| EncryptError::InvalidKeyLength)?;
    Ok(LessSafeKey::new(key))
}

fn seal_impl(key_bytes: &[u8], plaintext: &str) -> Result<Vec<u8>, EncryptError> {
    let key = make_key(key_bytes)?;
    let rng = SystemRandom::new();
    let mut nonce_bytes = [0u8; NONCE_LEN];
    rng.fill(&mut nonce_bytes)
        .map_err(|_| EncryptError::RngFailure)?;

    let nonce = Nonce::assume_unique_for_key(nonce_bytes);
    let mut in_out = plaintext.as_bytes().to_vec();
    key.seal_in_place_append_tag(nonce, Aad::empty(), &mut in_out)
        .map_err(|_| EncryptError::RngFailure)?;

    let mut ciphertext = Vec::with_capacity(NONCE_LEN + in_out.len());
    ciphertext.extend_from_slice(&nonce_bytes);
    ciphertext.extend_from_slice(&in_out);
    Ok(ciphertext)
}

fn open_impl(key_bytes: &[u8], ciphertext: &[u8]) -> Result<String, EncryptError> {
    if ciphertext.len() < NONCE_LEN + TAG_LEN {
        return Err(EncryptError::ShortCiphertext);
    }

    let key = make_key(key_bytes)?;
    let (nonce_bytes, encrypted) = ciphertext.split_at(NONCE_LEN);
    let nonce =
        Nonce::try_assume_unique_for_key(nonce_bytes).map_err(|_| EncryptError::ShortCiphertext)?;
    let mut in_out = encrypted.to_vec();
    let plaintext = key
        .open_in_place(nonce, Aad::empty(), &mut in_out)
        .map_err(|_| EncryptError::AuthFailed)?;
    String::from_utf8(plaintext.to_vec()).map_err(|_| EncryptError::InvalidUtf8)
}

unsafe fn copy_bytes_to_out(out: *mut u8, out_len: usize, bytes: &[u8]) -> usize {
    let required = bytes.len();
    if out.is_null() || out_len < required {
        return required;
    }
    if required > 0 {
        // SAFETY: `out` is valid for `required` bytes per caller contract.
        unsafe { ptr::copy_nonoverlapping(bytes.as_ptr(), out, required) };
    }
    required
}

/// Encrypt `plaintext` with AES-256-GCM, writing `nonce || ciphertext || tag`
/// into `out` when capacity is sufficient.
///
/// Returns the required output size. Returns `0` on invalid key/plaintext input
/// or encryption failure.
///
/// # Safety
///
/// - `key` must be valid for reading `key_len` bytes when non-null.
/// - `plaintext` must be a valid NUL-terminated UTF-8 string.
/// - `out` must be valid for writing `out_len` bytes when non-null.
#[no_mangle]
pub unsafe extern "C" fn hew_encrypt_seal(
    key: *const u8,
    key_len: usize,
    plaintext: *const c_char,
    out: *mut u8,
    out_len: usize,
) -> usize {
    // SAFETY: validity is required by this function's FFI contract.
    let Some(key_bytes) = (unsafe { input_bytes(key, key_len) }) else {
        return 0;
    };
    // SAFETY: validity is required by this function's FFI contract.
    let Some(plaintext_str) = (unsafe { cstr_to_str(plaintext) }) else {
        return 0;
    };
    match seal_impl(key_bytes, plaintext_str) {
        // SAFETY: `out` is a caller-provided buffer under this function's contract.
        Ok(ciphertext) => unsafe { copy_bytes_to_out(out, out_len, &ciphertext) },
        Err(_) => 0,
    }
}

/// Decrypt an AES-256-GCM `ciphertext`, writing a NUL-terminated UTF-8
/// plaintext string into `out` when capacity is sufficient.
///
/// Returns the required output size including the trailing NUL. Returns `0` on
/// invalid key/ciphertext input or authentication failure.
///
/// # Safety
///
/// - `key` must be valid for reading `key_len` bytes when non-null.
/// - `ciphertext` must be valid for reading `ciphertext_len` bytes when non-null.
/// - `out` must be valid for writing `out_len` bytes when non-null.
#[no_mangle]
pub unsafe extern "C" fn hew_encrypt_open(
    key: *const u8,
    key_len: usize,
    ciphertext: *const u8,
    ciphertext_len: usize,
    out: *mut u8,
    out_len: usize,
) -> usize {
    // SAFETY: validity is required by this function's FFI contract.
    let Some(key_bytes) = (unsafe { input_bytes(key, key_len) }) else {
        return 0;
    };
    // SAFETY: validity is required by this function's FFI contract.
    let Some(ciphertext_bytes) = (unsafe { input_bytes(ciphertext, ciphertext_len) }) else {
        return 0;
    };
    match open_impl(key_bytes, ciphertext_bytes) {
        Ok(plaintext) => {
            let mut buf = plaintext.into_bytes();
            buf.push(0);
            // SAFETY: `out` is a caller-provided buffer under this function's contract.
            unsafe { copy_bytes_to_out(out, out_len, &buf) }
        }
        Err(_) => 0,
    }
}

/// Hew-facing compatibility wrapper for [`hew_encrypt_seal`].
///
/// The returned `HewVec` is owned by the Hew caller and must follow the
/// standard runtime vec drop path. No custom free entrypoint is introduced.
///
/// # Safety
///
/// `key` must be a valid `bytes` `HewVec` and `plaintext` must be a valid
/// NUL-terminated string pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_encrypt_seal_hew(
    key: *mut HewVec,
    plaintext: *const c_char,
) -> *mut HewVec {
    // SAFETY: Hew caller provides a valid bytes HewVec.
    let key_bytes = unsafe { hwvec_to_u8(key) };
    // SAFETY: arguments satisfy `hew_encrypt_seal`.
    let required = unsafe {
        hew_encrypt_seal(
            key_bytes.as_ptr(),
            key_bytes.len(),
            plaintext,
            ptr::null_mut(),
            0,
        )
    };
    if required == 0 {
        panic_with_message(SEAL_FAILURE_MSG);
    }

    let mut out = vec![0u8; required];
    // SAFETY: `out` is allocated with the required capacity.
    let written = unsafe {
        hew_encrypt_seal(
            key_bytes.as_ptr(),
            key_bytes.len(),
            plaintext,
            out.as_mut_ptr(),
            out.len(),
        )
    };
    if written != required {
        panic_with_message(SEAL_FAILURE_MSG);
    }

    // SAFETY: `out` is a valid byte slice copied into the runtime-owned HewVec.
    unsafe { u8_to_hwvec(&out) }
}

// JUSTIFIED: the public Hew API exposes `open(...) -> String` without a
// structured error channel. This wrapper traps loudly on invalid key or
// ciphertext instead of inventing a partial `last_error` protocol here;
// a `try_open` surface is deferred by plan.
///
/// Hew-facing compatibility wrapper for [`hew_encrypt_open`].
///
/// The returned C string is allocated with `libc::malloc` via
/// [`str_to_malloc`]; the Hew runtime's standard string drop path (`free`) is
/// the canonical release. `open` fails loudly rather than silently returning an
/// empty string on authentication failure.
/// # Safety
///
/// `key` and `ciphertext` must be valid `bytes` `HewVec` pointers.
#[no_mangle]
pub unsafe extern "C" fn hew_encrypt_open_hew(
    key: *mut HewVec,
    ciphertext: *mut HewVec,
) -> *mut c_char {
    // SAFETY: Hew caller provides valid bytes HewVec pointers.
    let key_bytes = unsafe { hwvec_to_u8(key) };
    // SAFETY: Hew caller provides valid bytes HewVec pointers.
    let ciphertext_bytes = unsafe { hwvec_to_u8(ciphertext) };
    // SAFETY: arguments satisfy `hew_encrypt_open`.
    let required = unsafe {
        hew_encrypt_open(
            key_bytes.as_ptr(),
            key_bytes.len(),
            ciphertext_bytes.as_ptr(),
            ciphertext_bytes.len(),
            ptr::null_mut(),
            0,
        )
    };
    if required == 0 {
        panic_with_message(OPEN_FAILURE_MSG);
    }

    let mut out = vec![0u8; required];
    // SAFETY: `out` is allocated with the required capacity.
    let written = unsafe {
        hew_encrypt_open(
            key_bytes.as_ptr(),
            key_bytes.len(),
            ciphertext_bytes.as_ptr(),
            ciphertext_bytes.len(),
            out.as_mut_ptr(),
            out.len(),
        )
    };
    if written != required || out.last().copied() != Some(0) {
        panic_with_message(OPEN_FAILURE_MSG);
    }

    let Ok(plaintext) = std::str::from_utf8(&out[..out.len() - 1]) else {
        panic_with_message(OPEN_FAILURE_MSG);
    };
    let ptr = str_to_malloc(plaintext);
    if ptr.is_null() {
        panic_with_message(OPEN_ALLOC_FAILURE_MSG);
    }
    ptr
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::{CStr, CString};

    const TEST_KEY: [u8; AES_256_KEY_LEN] = [
        0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E,
        0x0F, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D,
        0x1E, 0x1F,
    ];

    #[test]
    fn roundtrip_recovers_plaintext() {
        let ciphertext = seal_impl(&TEST_KEY, "hello").expect("seal should succeed");
        let plaintext = open_impl(&TEST_KEY, &ciphertext).expect("open should succeed");
        assert_eq!(plaintext, "hello");
    }

    #[test]
    fn empty_plaintext_roundtrips() {
        let ciphertext = seal_impl(&TEST_KEY, "").expect("seal should succeed");
        let plaintext = open_impl(&TEST_KEY, &ciphertext).expect("open should succeed");
        assert_eq!(plaintext, "");
    }

    #[test]
    fn tampered_ciphertext_fails_authentication() {
        let mut ciphertext = seal_impl(&TEST_KEY, "hello").expect("seal should succeed");
        let last = ciphertext
            .last_mut()
            .expect("ciphertext must contain nonce and tag");
        *last ^= 0x01;
        let err = open_impl(&TEST_KEY, &ciphertext).expect_err("tampering must fail");
        assert_eq!(err, EncryptError::AuthFailed);
    }

    #[test]
    fn wrong_key_fails_authentication() {
        let ciphertext = seal_impl(&TEST_KEY, "hello").expect("seal should succeed");
        let wrong_key = [0xAA; AES_256_KEY_LEN];
        let err = open_impl(&wrong_key, &ciphertext).expect_err("wrong key must fail");
        assert_eq!(err, EncryptError::AuthFailed);
    }

    #[test]
    fn short_ciphertext_fails_without_ub() {
        let err = open_impl(&TEST_KEY, &[0x01, 0x02, 0x03]).expect_err("short input must fail");
        assert_eq!(err, EncryptError::ShortCiphertext);
    }

    #[test]
    fn raw_abi_roundtrip_uses_two_pass_contract() {
        let plaintext = CString::new("hello").expect("static string has no NUL");
        // SAFETY: pointers are valid for this test invocation.
        let required = unsafe {
            hew_encrypt_seal(
                TEST_KEY.as_ptr(),
                TEST_KEY.len(),
                plaintext.as_ptr(),
                ptr::null_mut(),
                0,
            )
        };
        assert!(required > NONCE_LEN + TAG_LEN);

        let mut ciphertext = vec![0u8; required];
        // SAFETY: output buffer has the requested capacity.
        let written = unsafe {
            hew_encrypt_seal(
                TEST_KEY.as_ptr(),
                TEST_KEY.len(),
                plaintext.as_ptr(),
                ciphertext.as_mut_ptr(),
                ciphertext.len(),
            )
        };
        assert_eq!(written, required);

        // SAFETY: pointers are valid for this test invocation.
        let open_required = unsafe {
            hew_encrypt_open(
                TEST_KEY.as_ptr(),
                TEST_KEY.len(),
                ciphertext.as_ptr(),
                ciphertext.len(),
                ptr::null_mut(),
                0,
            )
        };
        assert_eq!(open_required, "hello".len() + 1);

        let mut out = vec![0u8; open_required];
        // SAFETY: output buffer has the requested capacity.
        let open_written = unsafe {
            hew_encrypt_open(
                TEST_KEY.as_ptr(),
                TEST_KEY.len(),
                ciphertext.as_ptr(),
                ciphertext.len(),
                out.as_mut_ptr(),
                out.len(),
            )
        };
        assert_eq!(open_written, open_required);
        let recovered = CStr::from_bytes_with_nul(&out)
            .expect("open output must be NUL-terminated")
            .to_str()
            .expect("plaintext must be UTF-8");
        assert_eq!(recovered, "hello");
    }
}
