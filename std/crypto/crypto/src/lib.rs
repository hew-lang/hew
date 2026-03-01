//! Hew runtime: `crypto` module.
//!
//! Provides cryptographic hashing (SHA-256/384/512), HMAC-SHA256,
//! cryptographically secure random bytes, and constant-time comparison
//! for compiled Hew programs.

// Force-link hew-runtime so the linker can resolve hew_vec_* symbols
// referenced by hew-cabi's object code.
#[cfg(test)]
extern crate hew_runtime;

use ring::digest;
use ring::hmac;
use ring::rand::{SecureRandom, SystemRandom};

/// Compute the SHA-256 hash of `data` and write the 32-byte digest to `out`.
///
/// # Safety
///
/// - `data` must be valid for reading `len` bytes (may be null only if `len == 0`).
/// - `out` must be valid for writing 32 bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_sha256(data: *const u8, len: usize, out: *mut u8) {
    if out.is_null() {
        return;
    }
    // SAFETY: data is valid for len bytes per caller contract (or len == 0).
    let input = if data.is_null() || len == 0 {
        &[]
    } else {
        // SAFETY: data is non-null and valid for len bytes per caller contract.
        unsafe { std::slice::from_raw_parts(data, len) }
    };
    let hash = digest::digest(&digest::SHA256, input);
    // SAFETY: out is valid for 32 bytes per caller contract; SHA-256 always produces 32 bytes.
    unsafe { std::ptr::copy_nonoverlapping(hash.as_ref().as_ptr(), out, 32) };
}

/// Compute the SHA-384 hash of `data` and write the 48-byte digest to `out`.
///
/// # Safety
///
/// - `data` must be valid for reading `len` bytes (may be null only if `len == 0`).
/// - `out` must be valid for writing 48 bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_sha384(data: *const u8, len: usize, out: *mut u8) {
    if out.is_null() {
        return;
    }
    // SAFETY: data is valid for len bytes per caller contract (or len == 0).
    let input = if data.is_null() || len == 0 {
        &[]
    } else {
        // SAFETY: data is non-null and valid for len bytes per caller contract.
        unsafe { std::slice::from_raw_parts(data, len) }
    };
    let hash = digest::digest(&digest::SHA384, input);
    // SAFETY: out is valid for 48 bytes per caller contract; SHA-384 always produces 48 bytes.
    unsafe { std::ptr::copy_nonoverlapping(hash.as_ref().as_ptr(), out, 48) };
}

/// Compute the SHA-512 hash of `data` and write the 64-byte digest to `out`.
///
/// # Safety
///
/// - `data` must be valid for reading `len` bytes (may be null only if `len == 0`).
/// - `out` must be valid for writing 64 bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_sha512(data: *const u8, len: usize, out: *mut u8) {
    if out.is_null() {
        return;
    }
    // SAFETY: data is valid for len bytes per caller contract (or len == 0).
    let input = if data.is_null() || len == 0 {
        &[]
    } else {
        // SAFETY: data is non-null and valid for len bytes per caller contract.
        unsafe { std::slice::from_raw_parts(data, len) }
    };
    let hash = digest::digest(&digest::SHA512, input);
    // SAFETY: out is valid for 64 bytes per caller contract; SHA-512 always produces 64 bytes.
    unsafe { std::ptr::copy_nonoverlapping(hash.as_ref().as_ptr(), out, 64) };
}

/// Compute HMAC-SHA256 over `data` using `key` and write the 32-byte tag to
/// `out`.
///
/// # Safety
///
/// - `key` must be valid for reading `key_len` bytes.
/// - `data` must be valid for reading `data_len` bytes (may be null only if `data_len == 0`).
/// - `out` must be valid for writing 32 bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_hmac_sha256(
    key: *const u8,
    key_len: usize,
    data: *const u8,
    data_len: usize,
    out: *mut u8,
) {
    if key.is_null() || out.is_null() {
        return;
    }
    // SAFETY: key is valid for key_len bytes per caller contract.
    let key_bytes = unsafe { std::slice::from_raw_parts(key, key_len) };
    // SAFETY: data is valid for data_len bytes per caller contract (or data_len == 0).
    let data_bytes = if data.is_null() || data_len == 0 {
        &[]
    } else {
        // SAFETY: data is non-null and valid for data_len bytes per caller contract.
        unsafe { std::slice::from_raw_parts(data, data_len) }
    };
    let signing_key = hmac::Key::new(hmac::HMAC_SHA256, key_bytes);
    let tag = hmac::sign(&signing_key, data_bytes);
    // SAFETY: out is valid for 32 bytes per caller contract; HMAC-SHA256 produces 32 bytes.
    unsafe { std::ptr::copy_nonoverlapping(tag.as_ref().as_ptr(), out, 32) };
}

/// Fill `buf` with `len` cryptographically secure random bytes.
///
/// # Safety
///
/// `buf` must be valid for writing `len` bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_random_bytes(buf: *mut u8, len: usize) {
    if buf.is_null() || len == 0 {
        return;
    }
    // SAFETY: buf is valid for writing len bytes per caller contract.
    let slice = unsafe { std::slice::from_raw_parts_mut(buf, len) };
    let rng = SystemRandom::new();
    let _ = rng.fill(slice);
}

/// Perform a constant-time comparison of two byte buffers.
///
/// Returns `1` if the buffers are equal, `0` otherwise. The comparison always
/// examines all `len` bytes to avoid timing side-channels.
///
/// # Safety
///
/// Both `a` and `b` must be valid for reading `len` bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_constant_time_eq(a: *const u8, b: *const u8, len: usize) -> i32 {
    if a.is_null() || b.is_null() {
        return 0;
    }
    // SAFETY: a and b are valid for len bytes per caller contract.
    let a_slice = unsafe { std::slice::from_raw_parts(a, len) };
    // SAFETY: b is valid for len bytes per caller contract.
    let b_slice = unsafe { std::slice::from_raw_parts(b, len) };
    // XOR all bytes and accumulate; constant-time because every byte is visited.
    let mut diff: u8 = 0;
    for i in 0..len {
        diff |= a_slice[i] ^ b_slice[i];
    }
    i32::from(diff == 0)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Known SHA-256 test vector: SHA-256("") = e3b0c44298fc1c149afbf4c8996fb924
    ///                                          27ae41e4649b934ca495991b7852b855
    #[test]
    fn test_sha256_empty() {
        let expected = [
            0xe3, 0xb0, 0xc4, 0x42, 0x98, 0xfc, 0x1c, 0x14, 0x9a, 0xfb, 0xf4, 0xc8, 0x99, 0x6f,
            0xb9, 0x24, 0x27, 0xae, 0x41, 0xe4, 0x64, 0x9b, 0x93, 0x4c, 0xa4, 0x95, 0x99, 0x1b,
            0x78, 0x52, 0xb8, 0x55,
        ];
        let mut out = [0u8; 32];
        // SAFETY: empty input and out is a valid 32-byte buffer.
        unsafe { hew_sha256(std::ptr::null(), 0, out.as_mut_ptr()) };
        assert_eq!(out, expected);
    }

    /// Known SHA-256 test vector: SHA-256("abc") = ba7816bf8f01cfea414140de5dae2223
    ///                                             b00361a396177a9cb410ff61f20015ad
    #[test]
    fn test_sha256_abc() {
        let expected = [
            0xba, 0x78, 0x16, 0xbf, 0x8f, 0x01, 0xcf, 0xea, 0x41, 0x41, 0x40, 0xde, 0x5d, 0xae,
            0x22, 0x23, 0xb0, 0x03, 0x61, 0xa3, 0x96, 0x17, 0x7a, 0x9c, 0xb4, 0x10, 0xff, 0x61,
            0xf2, 0x00, 0x15, 0xad,
        ];
        let data = b"abc";
        let mut out = [0u8; 32];
        // SAFETY: data and out are valid buffers.
        unsafe { hew_sha256(data.as_ptr(), data.len(), out.as_mut_ptr()) };
        assert_eq!(out, expected);
    }

    /// HMAC-SHA256 test vector from RFC 4231 Test Case 2:
    /// Key = "Jefe", Data = "what do ya want for nothing?"
    #[test]
    fn test_hmac_sha256_rfc4231() {
        let expected = [
            0x5b, 0xdc, 0xc1, 0x46, 0xbf, 0x60, 0x75, 0x4e, 0x6a, 0x04, 0x24, 0x26, 0x08, 0x95,
            0x75, 0xc7, 0x5a, 0x00, 0x3f, 0x08, 0x9d, 0x27, 0x39, 0x83, 0x9d, 0xec, 0x58, 0xb9,
            0x64, 0xec, 0x38, 0x43,
        ];
        let key = b"Jefe";
        let data = b"what do ya want for nothing?";
        let mut out = [0u8; 32];
        // SAFETY: key, data, and out are valid buffers.
        unsafe {
            hew_hmac_sha256(
                key.as_ptr(),
                key.len(),
                data.as_ptr(),
                data.len(),
                out.as_mut_ptr(),
            );
        }
        assert_eq!(out, expected);
    }

    #[test]
    fn test_random_bytes_fills_buffer() {
        let mut buf = [0u8; 64];
        // SAFETY: buf is a valid 64-byte buffer.
        unsafe { hew_random_bytes(buf.as_mut_ptr(), buf.len()) };
        // Extremely unlikely that 64 random bytes are all zero.
        assert_ne!(buf, [0u8; 64]);
    }

    #[test]
    fn test_constant_time_eq() {
        let a = [1u8, 2, 3, 4];
        let b = [1u8, 2, 3, 4];
        let c = [1u8, 2, 3, 5];
        // SAFETY: all slices are valid for 4 bytes.
        assert_eq!(
            unsafe { hew_constant_time_eq(a.as_ptr(), b.as_ptr(), 4) },
            1
        );
        // SAFETY: all slices are valid for 4 bytes.
        assert_eq!(
            unsafe { hew_constant_time_eq(a.as_ptr(), c.as_ptr(), 4) },
            0
        );
    }

    #[test]
    fn test_sha512_empty() {
        // Known SHA-512("") first 8 bytes
        let mut out = [0u8; 64];
        // SAFETY: empty input and out is a valid 64-byte buffer.
        unsafe { hew_sha512(std::ptr::null(), 0, out.as_mut_ptr()) };
        // SHA-512("") starts with cf83e1357eefb8bd...
        assert_eq!(out[0], 0xcf);
        assert_eq!(out[1], 0x83);
        assert_eq!(out[2], 0xe1);
        assert_eq!(out[3], 0x35);
    }
}

// ---------------------------------------------------------------------------
// HewVec-ABI wrappers (used by std/crypto.hew)
// ---------------------------------------------------------------------------

/// Compute SHA-256 of a `bytes` `HewVec`, returning a 32-byte `bytes` `HewVec`.
///
/// # Safety
///
/// `data` must be a valid, non-null pointer to a `HewVec` (i32 elements).
#[no_mangle]
pub unsafe extern "C" fn hew_sha256_hew(
    data: *mut hew_cabi::vec::HewVec,
) -> *mut hew_cabi::vec::HewVec {
    // SAFETY: data validity forwarded to hwvec_to_u8.
    let input = unsafe { hew_cabi::vec::hwvec_to_u8(data) };
    let mut out = [0u8; 32];
    // SAFETY: input slice is valid; out is a 32-byte writable buffer.
    unsafe { hew_sha256(input.as_ptr(), input.len(), out.as_mut_ptr()) };
    // SAFETY: out is valid for 32 bytes.
    unsafe { hew_cabi::vec::u8_to_hwvec(&out) }
}

/// Compute SHA-384 of a `bytes` `HewVec`, returning a 48-byte `bytes` `HewVec`.
///
/// # Safety
///
/// `data` must be a valid, non-null pointer to a `HewVec` (i32 elements).
#[no_mangle]
pub unsafe extern "C" fn hew_sha384_hew(
    data: *mut hew_cabi::vec::HewVec,
) -> *mut hew_cabi::vec::HewVec {
    // SAFETY: data validity forwarded to hwvec_to_u8.
    let input = unsafe { hew_cabi::vec::hwvec_to_u8(data) };
    let mut out = [0u8; 48];
    // SAFETY: input slice is valid; out is a 48-byte writable buffer.
    unsafe { hew_sha384(input.as_ptr(), input.len(), out.as_mut_ptr()) };
    // SAFETY: out is valid for 48 bytes.
    unsafe { hew_cabi::vec::u8_to_hwvec(&out) }
}

/// Compute SHA-512 of a `bytes` `HewVec`, returning a 64-byte `bytes` `HewVec`.
///
/// # Safety
///
/// `data` must be a valid, non-null pointer to a `HewVec` (i32 elements).
#[no_mangle]
pub unsafe extern "C" fn hew_sha512_hew(
    data: *mut hew_cabi::vec::HewVec,
) -> *mut hew_cabi::vec::HewVec {
    // SAFETY: data validity forwarded to hwvec_to_u8.
    let input = unsafe { hew_cabi::vec::hwvec_to_u8(data) };
    let mut out = [0u8; 64];
    // SAFETY: input slice is valid; out is a 64-byte writable buffer.
    unsafe { hew_sha512(input.as_ptr(), input.len(), out.as_mut_ptr()) };
    // SAFETY: out is valid for 64 bytes.
    unsafe { hew_cabi::vec::u8_to_hwvec(&out) }
}

/// Compute HMAC-SHA-256 with key/data `HewVecs`, returning a 32-byte `bytes` `HewVec`.
///
/// # Safety
///
/// Both `key` and `data` must be valid, non-null pointers to `HewVecs` (i32 elements).
#[no_mangle]
pub unsafe extern "C" fn hew_hmac_sha256_hew(
    key: *mut hew_cabi::vec::HewVec,
    data: *mut hew_cabi::vec::HewVec,
) -> *mut hew_cabi::vec::HewVec {
    // SAFETY: key/data validity forwarded to hwvec_to_u8.
    let key_bytes = unsafe { hew_cabi::vec::hwvec_to_u8(key) };
    // SAFETY: data validity forwarded to hwvec_to_u8.
    let data_bytes = unsafe { hew_cabi::vec::hwvec_to_u8(data) };
    let mut out = [0u8; 32];
    // SAFETY: key_bytes and data_bytes slices are valid; out is a 32-byte buffer.
    unsafe {
        hew_hmac_sha256(
            key_bytes.as_ptr(),
            key_bytes.len(),
            data_bytes.as_ptr(),
            data_bytes.len(),
            out.as_mut_ptr(),
        );
    };
    // SAFETY: out is valid for 32 bytes.
    unsafe { hew_cabi::vec::u8_to_hwvec(&out) }
}

/// Fill and return a `bytes` `HewVec` of `len` cryptographically random bytes.
///
/// # Safety
///
/// None — all memory is managed by the runtime allocator.
#[no_mangle]
pub unsafe extern "C" fn hew_random_bytes_hew(len: i64) -> *mut hew_cabi::vec::HewVec {
    #[expect(
        clippy::cast_sign_loss,
        clippy::cast_possible_truncation,
        reason = "len > 0 checked; practical buffer sizes fit in usize"
    )]
    let n = if len > 0 { len as usize } else { 0 };
    let mut buf = vec![0u8; n];
    if n > 0 {
        // SAFETY: buf is valid for n bytes.
        unsafe { hew_random_bytes(buf.as_mut_ptr(), n) };
    }
    // SAFETY: buf slice is valid.
    unsafe { hew_cabi::vec::u8_to_hwvec(&buf) }
}

/// Compare two `bytes` `HewVecs` in constant time.
///
/// Returns 1 if equal, 0 if different or different lengths.
///
/// # Safety
///
/// Both `a` and `b` must be valid, non-null pointers to `HewVecs` (i32 elements).
#[no_mangle]
pub unsafe extern "C" fn hew_constant_time_eq_hew(
    a: *mut hew_cabi::vec::HewVec,
    b: *mut hew_cabi::vec::HewVec,
) -> i32 {
    // SAFETY: a validity forwarded to hwvec_to_u8.
    let a_bytes = unsafe { hew_cabi::vec::hwvec_to_u8(a) };
    // SAFETY: b validity forwarded to hwvec_to_u8.
    let b_bytes = unsafe { hew_cabi::vec::hwvec_to_u8(b) };
    if a_bytes.len() != b_bytes.len() {
        return 0;
    }
    // SAFETY: both slices are valid and same length.
    unsafe { hew_constant_time_eq(a_bytes.as_ptr(), b_bytes.as_ptr(), a_bytes.len()) }
}
