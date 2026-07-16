//! Hew runtime: `encrypt` module.
//!
//! Provides AES-256-GCM seal/open helpers for compiled Hew programs.
//! Returned `bytes` use the runtime `HewVec` allocation path; returned strings
//! are allocated with `libc::malloc` and must follow the standard runtime drop
//! path (`free`).
// WASM-TODO(#1451): `std::crypto::encrypt` mirrors the sibling native-only crypto
// modules and is excluded from the wasm runtime's ecosystem-FFI link set.
use base64::{engine::general_purpose::STANDARD as BASE64_STANDARD, Engine as _};
use hew_cabi::cabi::{cstr_to_str, str_to_malloc};
use hew_runtime::bytes::BytesTriple;
use ring::{
    aead::{self, Aad, LessSafeKey, Nonce, UnboundKey},
    rand::{SecureRandom, SystemRandom},
};
use std::{cell::Cell, ffi::c_char, ptr};

const AES_256_KEY_LEN: usize = 32;
const NONCE_LEN: usize = 12;
const TAG_LEN: usize = 16;

const SEAL_FAILURE_MSG: &[u8] =
    b"encrypt.seal failed: expected a 32-byte key and a valid plaintext string\0";
const OPEN_FAILURE_MSG: &[u8] =
    b"encrypt.open failed: authentication failed or ciphertext was malformed\0";
const OPEN_ALLOC_FAILURE_MSG: &[u8] = b"encrypt.open failed: native string allocation failed\0";

#[repr(C)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum HewEncryptError {
    None = 0,
    InvalidKey = 1,
    ShortCiphertext = 2,
    AuthFailed = 3,
    InvalidUtf8 = 4,
    AllocationFailure = 5,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum EncryptError {
    InvalidKeyLength,
    RngFailure,
    ShortCiphertext,
    AuthFailed,
    InvalidUtf8,
}

impl From<EncryptError> for HewEncryptError {
    fn from(err: EncryptError) -> Self {
        match err {
            EncryptError::InvalidKeyLength => HewEncryptError::InvalidKey,
            EncryptError::ShortCiphertext => HewEncryptError::ShortCiphertext,
            EncryptError::AuthFailed | EncryptError::RngFailure => HewEncryptError::AuthFailed,
            EncryptError::InvalidUtf8 => HewEncryptError::InvalidUtf8,
        }
    }
}

thread_local! {
    static LAST_OPEN_ERROR: Cell<HewEncryptError> = const { Cell::new(HewEncryptError::None) };
}

fn set_last_open_error(err: HewEncryptError) {
    LAST_OPEN_ERROR.with(|slot| slot.set(err));
}

fn last_open_error() -> HewEncryptError {
    LAST_OPEN_ERROR.with(Cell::get)
}

unsafe fn write_encrypt_err_out(err_out: *mut HewEncryptError, err: HewEncryptError) {
    if !err_out.is_null() {
        // SAFETY: caller provided a valid out-pointer when non-null.
        unsafe { *err_out = err };
    }
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

/// Copy a Hew `bytes` value's active region out of a caller-provided
/// [`BytesTriple`] pointer (`is_bytes_by_pointer_consumer` convention).
///
/// A null triple pointer, or a triple whose `ptr` is null with a non-zero
/// `len`, is rejected (`None`); an empty triple yields an empty `Vec`. The
/// active region is `ptr[offset .. offset + len]`, so the copy is offset-aware.
///
/// # Safety
///
/// `triple` must be null or point to a valid [`BytesTriple`] whose active
/// region `[offset, offset + len)` is in bounds for reading.
unsafe fn bytes_triple_to_vec(triple: *const BytesTriple) -> Option<Vec<u8>> {
    if triple.is_null() {
        return None;
    }
    // SAFETY: `triple` is non-null and points to the caller's valid triple slot.
    let triple = unsafe { &*triple };
    let len = usize::try_from(triple.len).ok()?;
    if len == 0 {
        return Some(Vec::new());
    }
    if triple.ptr.is_null() {
        return None;
    }
    let offset = usize::try_from(triple.offset).ok()?;
    // SAFETY: caller guarantees the triple's active region is a valid `bytes` value.
    let ptr = unsafe { triple.ptr.add(offset) };
    // SAFETY: caller guarantees the active `bytes` region is valid for `len` bytes.
    Some(unsafe { std::slice::from_raw_parts(ptr, len) }.to_vec())
}

/// Return the last `try_open` error observed through the Hew wrapper.
#[no_mangle]
pub extern "C" fn hew_encrypt_last_open_error_code() -> i32 {
    last_open_error() as i32
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

/// Decrypt an AES-256-GCM `ciphertext` and return a malloc-owned plaintext.
///
/// Returns null on failure and writes a structured error discriminator to
/// `err_out` when non-null.
///
/// # Safety
///
/// - `key` must be valid for reading `key_len` bytes when non-null.
/// - `ciphertext` must be valid for reading `ciphertext_len` bytes when non-null.
/// - `err_out` must be valid for writing when non-null.
#[no_mangle]
pub unsafe extern "C" fn hew_encrypt_try_open(
    key: *const u8,
    key_len: usize,
    ciphertext: *const u8,
    ciphertext_len: usize,
    err_out: *mut HewEncryptError,
) -> *mut c_char {
    // SAFETY: `err_out` is an optional caller-provided out-pointer.
    unsafe { write_encrypt_err_out(err_out, HewEncryptError::None) };

    // SAFETY: validity is required by this function's FFI contract.
    let Some(key_bytes) = (unsafe { input_bytes(key, key_len) }) else {
        // SAFETY: `err_out` is an optional caller-provided out-pointer.
        unsafe { write_encrypt_err_out(err_out, HewEncryptError::InvalidKey) };
        return ptr::null_mut();
    };
    // SAFETY: validity is required by this function's FFI contract.
    let Some(ciphertext_bytes) = (unsafe { input_bytes(ciphertext, ciphertext_len) }) else {
        // SAFETY: `err_out` is an optional caller-provided out-pointer.
        unsafe { write_encrypt_err_out(err_out, HewEncryptError::AuthFailed) };
        return ptr::null_mut();
    };

    match open_impl(key_bytes, ciphertext_bytes) {
        Ok(plaintext) => {
            let ptr = str_to_malloc(&plaintext);
            if ptr.is_null() {
                // SAFETY: `err_out` is an optional caller-provided out-pointer.
                unsafe { write_encrypt_err_out(err_out, HewEncryptError::AllocationFailure) };
            }
            ptr
        }
        Err(err) => {
            // SAFETY: `err_out` is an optional caller-provided out-pointer.
            unsafe { write_encrypt_err_out(err_out, err.into()) };
            ptr::null_mut()
        }
    }
}

/// Hew-facing seal wrapper that returns base64 text.
///
/// The Hew surface decodes this back to `bytes` using the canonical pure-Hew
/// bytes builder, avoiding the platform-specific aggregate return ABI mismatch
/// for module-level externs returning `bytes`.
///
/// # Safety
///
/// `key` must be a valid `bytes` value and `plaintext` must be a valid
/// NUL-terminated string pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_encrypt_seal_base64_hew(
    key: *const BytesTriple,
    plaintext: *const c_char,
) -> *mut c_char {
    // SAFETY: Hew caller provides a valid bytes value.
    let Some(key_bytes) = (unsafe { bytes_triple_to_vec(key) }) else {
        panic_with_message(SEAL_FAILURE_MSG);
    };
    // SAFETY: Hew caller provides a valid string pointer.
    let Some(plaintext_str) = (unsafe { cstr_to_str(plaintext) }) else {
        panic_with_message(SEAL_FAILURE_MSG);
    };

    let Ok(ciphertext) = seal_impl(&key_bytes, plaintext_str) else {
        panic_with_message(SEAL_FAILURE_MSG);
    };
    let encoded = BASE64_STANDARD.encode(ciphertext);
    let ptr = str_to_malloc(&encoded);
    if ptr.is_null() {
        panic_with_message(SEAL_FAILURE_MSG);
    }
    ptr
}

/// Hew-facing fallible wrapper for [`hew_encrypt_try_open`].
///
/// Authentication failure and malformed ciphertext return a null string pointer
/// plus an error tag observable through [`hew_encrypt_last_open_error_code`];
/// no process abort is triggered on the fallible path.
///
/// # Safety
///
/// `key` and `ciphertext` must be valid `bytes` values.
#[no_mangle]
pub unsafe extern "C" fn hew_encrypt_try_open_hew(
    key: *const BytesTriple,
    ciphertext: *const BytesTriple,
) -> *mut c_char {
    // SAFETY: Hew caller provides valid bytes values.
    let Some(key_bytes) = (unsafe { bytes_triple_to_vec(key) }) else {
        set_last_open_error(HewEncryptError::InvalidKey);
        return ptr::null_mut();
    };
    // SAFETY: Hew caller provides valid bytes values.
    let Some(ciphertext_bytes) = (unsafe { bytes_triple_to_vec(ciphertext) }) else {
        set_last_open_error(HewEncryptError::AuthFailed);
        return ptr::null_mut();
    };

    let mut err = HewEncryptError::None;
    // SAFETY: arguments satisfy the contract inherited from `hew_encrypt_try_open`.
    let result = unsafe {
        hew_encrypt_try_open(
            key_bytes.as_ptr(),
            key_bytes.len(),
            ciphertext_bytes.as_ptr(),
            ciphertext_bytes.len(),
            &raw mut err,
        )
    };
    set_last_open_error(err);
    result
}

///
/// Hew-facing compatibility wrapper for [`hew_encrypt_open`].
///
/// The returned C string is allocated with `libc::malloc` via
/// [`str_to_malloc`]; the Hew runtime's standard string drop path (`free`) is
/// the canonical release. `open` fails loudly rather than silently returning an
/// empty string on authentication failure.
/// # Safety
///
/// `key` and `ciphertext` must be valid `bytes` values.
#[no_mangle]
pub unsafe extern "C" fn hew_encrypt_open_hew(
    key: *const BytesTriple,
    ciphertext: *const BytesTriple,
) -> *mut c_char {
    // SAFETY: arguments satisfy `hew_encrypt_try_open_hew`.
    let ptr = unsafe { hew_encrypt_try_open_hew(key, ciphertext) };
    match last_open_error() {
        HewEncryptError::None if !ptr.is_null() => ptr,
        HewEncryptError::AllocationFailure => panic_with_message(OPEN_ALLOC_FAILURE_MSG),
        _ => panic_with_message(OPEN_FAILURE_MSG),
    }
}

/// Hew-facing explicit panicking wrapper for [`hew_encrypt_try_open_hew`].
///
/// # Safety
///
/// `key` and `ciphertext` must be valid `bytes` values.
#[no_mangle]
pub unsafe extern "C" fn hew_encrypt_must_open_hew(
    key: *const BytesTriple,
    ciphertext: *const BytesTriple,
) -> *mut c_char {
    // SAFETY: arguments satisfy `hew_encrypt_open_hew`.
    unsafe { hew_encrypt_open_hew(key, ciphertext) }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hew_cabi::cabi::free_cstring;
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
    fn try_open_roundtrip_returns_plaintext() {
        let ciphertext = seal_impl(&TEST_KEY, "hello").expect("seal should succeed");
        let mut err = HewEncryptError::AuthFailed;
        // SAFETY: pointers are valid for this test invocation.
        let ptr = unsafe {
            hew_encrypt_try_open(
                TEST_KEY.as_ptr(),
                TEST_KEY.len(),
                ciphertext.as_ptr(),
                ciphertext.len(),
                &raw mut err,
            )
        };
        assert_eq!(err, HewEncryptError::None);
        assert!(!ptr.is_null());

        // SAFETY: `ptr` is a non-null NUL-terminated C string allocated by the FFI.
        let recovered = unsafe { CStr::from_ptr(ptr) }
            .to_str()
            .expect("plaintext must be UTF-8");
        assert_eq!(recovered, "hello");
        // SAFETY: `ptr` was allocated by `str_to_malloc` and is no longer used.
        unsafe { free_cstring(ptr) };
    }

    #[test]
    fn try_open_tampered_ciphertext_returns_err_without_aborting() {
        let mut ciphertext = seal_impl(&TEST_KEY, "hello").expect("seal should succeed");
        let last = ciphertext
            .last_mut()
            .expect("ciphertext must contain nonce and tag");
        *last ^= 0x01;

        let mut err = HewEncryptError::None;
        // SAFETY: pointers are valid for this test invocation.
        let ptr = unsafe {
            hew_encrypt_try_open(
                TEST_KEY.as_ptr(),
                TEST_KEY.len(),
                ciphertext.as_ptr(),
                ciphertext.len(),
                &raw mut err,
            )
        };

        assert!(ptr.is_null());
        assert_eq!(err, HewEncryptError::AuthFailed);
        assert_eq!(2 + 2, 4, "process must continue after auth failure");
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

    /// FFI signature-parity guard (`uuid.rs`-style scalar pin).
    ///
    /// `hew_encrypt_last_open_error_code` returns `i32` on the Rust side
    /// (`#[no_mangle]` above). The Hew binding in `encrypt.hew` must declare
    /// the same return type, or the compiled call reads the error code
    /// through a mismatched ABI width. This test parses the `.hew` extern
    /// block and fails closed if the declared return type is anything other
    /// than `-> i32`.
    #[test]
    fn hew_binding_declares_encrypt_last_open_error_code_returns_i32() {
        let hew_src = include_str!("../../std/crypto/encrypt/encrypt.hew");

        let decl = hew_src
            .lines()
            .map(str::trim)
            .find(|line| line.starts_with("fn hew_encrypt_last_open_error_code"))
            .expect("encrypt.hew must declare an extern `fn hew_encrypt_last_open_error_code`");

        let signature = decl.split("//").next().unwrap_or(decl).trim();

        assert!(
            signature.contains("-> i32"),
            "encrypt.hew binding for hew_encrypt_last_open_error_code must \
             return i32 to match the Rust `#[no_mangle] -> i32` signature; \
             found: {signature:?}"
        );
    }

    /// Crypto parity gate mirroring JWT's poisoned-slot fail-closed
    /// regression test (`poisoned_error_slot_fails_closed_to_token_malformed`
    /// in `jwt.rs`).
    ///
    /// `hew_encrypt_last_open_error_code` returns a raw i32 that the Hew-side
    /// `crypto_error_from_i32` (in `encrypt.hew`) decodes into a
    /// `CryptoError`. That catch-all arm is already fail-closed in
    /// direction (`_ => CryptoError::AuthFailed`, not `_ => CryptoError::None`)
    /// — unlike the original JWT bug — but had no regression test pinning
    /// it. This test parses the `crypto_error_from_i32` match block via
    /// `include_str!` and asserts the catch-all arm never decodes to
    /// `CryptoError::None`, and that the explicit `CryptoError::None` arm
    /// stays pinned to status `0` rather than becoming the catch-all.
    #[test]
    fn hew_binding_pins_encrypt_poisoned_code_never_decodes_to_none() {
        let hew_src = include_str!("../../std/crypto/encrypt/encrypt.hew");

        let match_start = hew_src
            .find("fn crypto_error_from_i32")
            .expect("encrypt.hew must declare `fn crypto_error_from_i32`");
        let match_block = &hew_src[match_start..];

        let catch_all = match_block
            .lines()
            .map(str::trim)
            .find(|line| line.starts_with("_ =>"))
            .expect("crypto_error_from_i32 must have a catch-all `_` arm");
        assert!(
            !catch_all.contains("CryptoError::None"),
            "encrypt.hew's crypto_error_from_i32 catch-all arm must NOT \
             decode an out-of-range/poisoned code to CryptoError::None \
             (fail-open); found: {catch_all:?}"
        );

        let none_arm = match_block
            .lines()
            .map(str::trim)
            .find(|line| line.contains("=> CryptoError::None"))
            .expect("crypto_error_from_i32 must have an explicit None arm");
        assert!(
            none_arm.starts_with("0 =>"),
            "encrypt.hew's crypto_error_from_i32 CryptoError::None arm must \
             be pinned to status 0, not the catch-all; found: {none_arm:?}"
        );
    }
}
