//! Hew runtime: gzip, deflate, and zlib compression/decompression.
//!
//! Provides compression and decompression using gzip, raw deflate, and zlib
//! formats for compiled Hew programs. All returned buffers are allocated with
//! `libc::malloc` so callers can free them with [`hew_compress_free`].

use std::io::Read;

use flate2::read::{
    DeflateDecoder, DeflateEncoder, GzDecoder, GzEncoder, ZlibDecoder, ZlibEncoder,
};
use flate2::Compression;

/// Read input through `reader` into a `malloc`-allocated buffer.
///
/// Returns a pointer to the buffer and writes its length to `out_len`.
/// Returns null on read or allocation failure.
///
/// # Safety
///
/// `out_len` must point to a writable `usize`.
unsafe fn read_to_malloc(mut reader: impl Read, out_len: *mut usize) -> *mut u8 {
    let mut buf = Vec::new();
    if reader.read_to_end(&mut buf).is_err() {
        return std::ptr::null_mut();
    }
    let buf_len = buf.len();
    let alloc_size = if buf_len == 0 { 1 } else { buf_len };

    // SAFETY: We request alloc_size bytes from malloc; alloc_size >= 1,
    // avoiding implementation-defined malloc(0).
    let ptr = unsafe { libc::malloc(alloc_size) }.cast::<u8>();
    if ptr.is_null() {
        return std::ptr::null_mut();
    }
    if buf_len > 0 {
        // SAFETY: ptr is freshly allocated with at least buf_len bytes;
        // buf bytes are valid and non-overlapping with the malloc'd region.
        unsafe { std::ptr::copy_nonoverlapping(buf.as_ptr(), ptr, buf_len) };
    }
    // SAFETY: out_len is a valid writable pointer per caller contract.
    unsafe { *out_len = buf_len };
    ptr
}

/// Build a byte slice from a raw pointer and length, returning `None` on
/// invalid arguments.
///
/// # Safety
///
/// `data` must point to at least `len` readable bytes, or be null when `len`
/// is 0.
unsafe fn input_slice<'a>(data: *const u8, len: usize) -> Option<&'a [u8]> {
    if data.is_null() && len > 0 {
        return None;
    }
    if len == 0 {
        Some(&[])
    } else {
        // SAFETY: Caller guarantees data is valid for len bytes.
        Some(unsafe { std::slice::from_raw_parts(data, len) })
    }
}

/// Gzip compress `data`.
///
/// Returns a `malloc`-allocated buffer and writes its length to `out_len`. The
/// caller must free the buffer with [`hew_compress_free`]. Returns null on
/// error or if `data` is null with `len > 0`.
///
/// # Safety
///
/// `data` must point to at least `len` readable bytes, or be null when `len`
/// is 0. `out_len` must point to a writable `usize`.
#[no_mangle]
pub unsafe extern "C" fn hew_gzip_compress(
    data: *const u8,
    len: usize,
    out_len: *mut usize,
) -> *mut u8 {
    if out_len.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: Caller guarantees data validity; forwarding contract.
    let Some(slice) = (unsafe { input_slice(data, len) }) else {
        return std::ptr::null_mut();
    };
    let encoder = GzEncoder::new(slice, Compression::default());
    // SAFETY: out_len is valid per caller contract; forwarding to read_to_malloc.
    unsafe { read_to_malloc(encoder, out_len) }
}

/// Gzip decompress `data`.
///
/// Returns a `malloc`-allocated buffer and writes its length to `out_len`. The
/// caller must free the buffer with [`hew_compress_free`]. Returns null on
/// error or if `data` is null with `len > 0`.
///
/// # Safety
///
/// `data` must point to at least `len` readable bytes, or be null when `len`
/// is 0. `out_len` must point to a writable `usize`.
#[no_mangle]
pub unsafe extern "C" fn hew_gzip_decompress(
    data: *const u8,
    len: usize,
    out_len: *mut usize,
) -> *mut u8 {
    if out_len.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: Caller guarantees data validity; forwarding contract.
    let Some(slice) = (unsafe { input_slice(data, len) }) else {
        return std::ptr::null_mut();
    };
    let decoder = GzDecoder::new(slice);
    // SAFETY: out_len is valid per caller contract; forwarding to read_to_malloc.
    unsafe { read_to_malloc(decoder, out_len) }
}

/// Raw deflate compress `data`.
///
/// Returns a `malloc`-allocated buffer and writes its length to `out_len`. The
/// caller must free the buffer with [`hew_compress_free`]. Returns null on
/// error or if `data` is null with `len > 0`.
///
/// # Safety
///
/// `data` must point to at least `len` readable bytes, or be null when `len`
/// is 0. `out_len` must point to a writable `usize`.
#[no_mangle]
pub unsafe extern "C" fn hew_deflate_compress(
    data: *const u8,
    len: usize,
    out_len: *mut usize,
) -> *mut u8 {
    if out_len.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: Caller guarantees data validity; forwarding contract.
    let Some(slice) = (unsafe { input_slice(data, len) }) else {
        return std::ptr::null_mut();
    };
    let encoder = DeflateEncoder::new(slice, Compression::default());
    // SAFETY: out_len is valid per caller contract; forwarding to read_to_malloc.
    unsafe { read_to_malloc(encoder, out_len) }
}

/// Raw deflate decompress `data`.
///
/// Returns a `malloc`-allocated buffer and writes its length to `out_len`. The
/// caller must free the buffer with [`hew_compress_free`]. Returns null on
/// error or if `data` is null with `len > 0`.
///
/// # Safety
///
/// `data` must point to at least `len` readable bytes, or be null when `len`
/// is 0. `out_len` must point to a writable `usize`.
#[no_mangle]
pub unsafe extern "C" fn hew_deflate_decompress(
    data: *const u8,
    len: usize,
    out_len: *mut usize,
) -> *mut u8 {
    if out_len.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: Caller guarantees data validity; forwarding contract.
    let Some(slice) = (unsafe { input_slice(data, len) }) else {
        return std::ptr::null_mut();
    };
    let decoder = DeflateDecoder::new(slice);
    // SAFETY: out_len is valid per caller contract; forwarding to read_to_malloc.
    unsafe { read_to_malloc(decoder, out_len) }
}

/// Zlib compress `data`.
///
/// Returns a `malloc`-allocated buffer and writes its length to `out_len`. The
/// caller must free the buffer with [`hew_compress_free`]. Returns null on
/// error or if `data` is null with `len > 0`.
///
/// # Safety
///
/// `data` must point to at least `len` readable bytes, or be null when `len`
/// is 0. `out_len` must point to a writable `usize`.
#[no_mangle]
pub unsafe extern "C" fn hew_zlib_compress(
    data: *const u8,
    len: usize,
    out_len: *mut usize,
) -> *mut u8 {
    if out_len.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: Caller guarantees data validity; forwarding contract.
    let Some(slice) = (unsafe { input_slice(data, len) }) else {
        return std::ptr::null_mut();
    };
    let encoder = ZlibEncoder::new(slice, Compression::default());
    // SAFETY: out_len is valid per caller contract; forwarding to read_to_malloc.
    unsafe { read_to_malloc(encoder, out_len) }
}

/// Zlib decompress `data`.
///
/// Returns a `malloc`-allocated buffer and writes its length to `out_len`. The
/// caller must free the buffer with [`hew_compress_free`]. Returns null on
/// error or if `data` is null with `len > 0`.
///
/// # Safety
///
/// `data` must point to at least `len` readable bytes, or be null when `len`
/// is 0. `out_len` must point to a writable `usize`.
#[no_mangle]
pub unsafe extern "C" fn hew_zlib_decompress(
    data: *const u8,
    len: usize,
    out_len: *mut usize,
) -> *mut u8 {
    if out_len.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: Caller guarantees data validity; forwarding contract.
    let Some(slice) = (unsafe { input_slice(data, len) }) else {
        return std::ptr::null_mut();
    };
    let decoder = ZlibDecoder::new(slice);
    // SAFETY: out_len is valid per caller contract; forwarding to read_to_malloc.
    unsafe { read_to_malloc(decoder, out_len) }
}

/// Free a buffer previously returned by any `hew_gzip_*`, `hew_deflate_*`, or
/// `hew_zlib_*` function.
///
/// # Safety
///
/// `ptr` must be a pointer previously returned by one of the compression
/// functions in this module, and must not have been freed already. Null is
/// accepted (no-op).
#[no_mangle]
pub unsafe extern "C" fn hew_compress_free(ptr: *mut u8) {
    if ptr.is_null() {
        return;
    }
    // SAFETY: ptr was allocated with libc::malloc in read_to_malloc.
    unsafe { libc::free(ptr.cast()) };
}

#[cfg(test)]
extern crate hew_runtime; // Link hew_vec_* symbol implementations
mod tests {
    use super::*;

    /// Helper: compress then decompress, asserting the roundtrip matches.
    unsafe fn assert_roundtrip(
        input: &[u8],
        compress_fn: unsafe extern "C" fn(*const u8, usize, *mut usize) -> *mut u8,
        decompress_fn: unsafe extern "C" fn(*const u8, usize, *mut usize) -> *mut u8,
    ) {
        let mut compressed_len: usize = 0;
        // SAFETY: input is a valid slice; compressed_len is writable.
        let compressed = unsafe { compress_fn(input.as_ptr(), input.len(), &mut compressed_len) };
        assert!(!compressed.is_null(), "compression returned null");
        assert!(compressed_len > 0 || input.is_empty());

        let mut decompressed_len: usize = 0;
        // SAFETY: compressed is valid for compressed_len bytes; decompressed_len is writable.
        let decompressed =
            unsafe { decompress_fn(compressed, compressed_len, &mut decompressed_len) };
        assert!(!decompressed.is_null(), "decompression returned null");
        assert_eq!(decompressed_len, input.len());

        // SAFETY: decompressed is valid for decompressed_len bytes.
        let result = unsafe { std::slice::from_raw_parts(decompressed, decompressed_len) };
        assert_eq!(result, input);

        // SAFETY: both pointers were allocated by compression functions.
        unsafe {
            hew_compress_free(compressed);
            hew_compress_free(decompressed);
        };
    }

    #[test]
    fn test_gzip_roundtrip() {
        let input = b"Hello, Hew compression!";
        // SAFETY: input is a valid byte slice.
        unsafe { assert_roundtrip(input, hew_gzip_compress, hew_gzip_decompress) };
    }

    #[test]
    fn test_deflate_roundtrip() {
        let input = b"Deflate roundtrip test data";
        // SAFETY: input is a valid byte slice.
        unsafe { assert_roundtrip(input, hew_deflate_compress, hew_deflate_decompress) };
    }

    #[test]
    fn test_zlib_roundtrip() {
        let input = b"Zlib roundtrip test data";
        // SAFETY: input is a valid byte slice.
        unsafe { assert_roundtrip(input, hew_zlib_compress, hew_zlib_decompress) };
    }

    #[test]
    fn test_large_data() {
        // 64 KiB of repeating pattern — should compress well.
        let input: Vec<u8> = (0..65_536).map(|i| (i % 251) as u8).collect();
        // SAFETY: input is a valid byte slice.
        unsafe {
            assert_roundtrip(&input, hew_gzip_compress, hew_gzip_decompress);
            assert_roundtrip(&input, hew_deflate_compress, hew_deflate_decompress);
            assert_roundtrip(&input, hew_zlib_compress, hew_zlib_decompress);
        };

        // Verify compression actually reduced size.
        let mut compressed_len: usize = 0;
        // SAFETY: input is valid; compressed_len is writable.
        let compressed =
            unsafe { hew_gzip_compress(input.as_ptr(), input.len(), &mut compressed_len) };
        assert!(!compressed.is_null());
        assert!(
            compressed_len < input.len(),
            "expected compression to reduce size: {compressed_len} >= {}",
            input.len()
        );
        // SAFETY: pointer was allocated by hew_gzip_compress.
        unsafe { hew_compress_free(compressed) };
    }

    #[test]
    fn test_null_handling() {
        // Null data with len > 0 should return null.
        let mut out_len: usize = 0;
        // SAFETY: testing null handling; out_len is writable.
        let result = unsafe { hew_gzip_compress(std::ptr::null(), 10, &mut out_len) };
        assert!(result.is_null());

        // Null out_len should return null.
        let data = b"test";
        // SAFETY: data is valid; testing null out_len.
        let result = unsafe { hew_gzip_compress(data.as_ptr(), data.len(), std::ptr::null_mut()) };
        assert!(result.is_null());

        // Free null is a no-op.
        // SAFETY: null is explicitly allowed.
        unsafe { hew_compress_free(std::ptr::null_mut()) };
    }
}

// ---------------------------------------------------------------------------
// HewVec-ABI wrappers (used by std/compress.hew)
// ---------------------------------------------------------------------------

/// Helper to call a compress/decompress function with `*const u8, usize, *mut usize` ABI.
///
/// # Safety
///
/// Caller must ensure the `f` function is called with valid pointers.
unsafe fn compress_op(
    v: *mut hew_cabi::vec::HewVec,
    f: unsafe extern "C" fn(*const u8, usize, *mut usize) -> *mut u8,
) -> *mut hew_cabi::vec::HewVec {
    // SAFETY: v validity forwarded to hwvec_to_u8.
    let input = unsafe { hew_cabi::vec::hwvec_to_u8(v) };
    let mut out_len: usize = 0;
    // SAFETY: input slice is valid; out_len is writable.
    let ptr = unsafe { f(input.as_ptr(), input.len(), &raw mut out_len) };
    if ptr.is_null() {
        // SAFETY: hew_vec_new allocates a valid empty HewVec.
        return unsafe { hew_cabi::vec::hew_vec_new() };
    }
    // SAFETY: ptr is valid for out_len bytes.
    let slice = unsafe { std::slice::from_raw_parts(ptr, out_len) };
    // SAFETY: slice is valid.
    let result = unsafe { hew_cabi::vec::u8_to_hwvec(slice) };
    // SAFETY: ptr was allocated by the codec function via libc::malloc.
    unsafe { hew_compress_free(ptr) };
    result
}

/// Gzip-compress a `bytes` `HewVec`, returning a new `bytes` `HewVec`.
///
/// # Safety
///
/// `v` must be a valid, non-null pointer to a `HewVec` (i32 elements).
#[no_mangle]
pub unsafe extern "C" fn hew_gzip_compress_hew(
    v: *mut hew_cabi::vec::HewVec,
) -> *mut hew_cabi::vec::HewVec {
    // SAFETY: v validity forwarded to compress_op.
    unsafe { compress_op(v, hew_gzip_compress) }
}

/// Gzip-decompress a `bytes` `HewVec`, returning a new `bytes` `HewVec`.
///
/// # Safety
///
/// `v` must be a valid, non-null pointer to a `HewVec` (i32 elements).
#[no_mangle]
pub unsafe extern "C" fn hew_gzip_decompress_hew(
    v: *mut hew_cabi::vec::HewVec,
) -> *mut hew_cabi::vec::HewVec {
    // SAFETY: v validity forwarded to compress_op.
    unsafe { compress_op(v, hew_gzip_decompress) }
}

/// Deflate-compress a `bytes` `HewVec`, returning a new `bytes` `HewVec`.
///
/// # Safety
///
/// `v` must be a valid, non-null pointer to a `HewVec` (i32 elements).
#[no_mangle]
pub unsafe extern "C" fn hew_deflate_compress_hew(
    v: *mut hew_cabi::vec::HewVec,
) -> *mut hew_cabi::vec::HewVec {
    // SAFETY: v validity forwarded to compress_op.
    unsafe { compress_op(v, hew_deflate_compress) }
}

/// Deflate-decompress a `bytes` `HewVec`, returning a new `bytes` `HewVec`.
///
/// # Safety
///
/// `v` must be a valid, non-null pointer to a `HewVec` (i32 elements).
#[no_mangle]
pub unsafe extern "C" fn hew_deflate_decompress_hew(
    v: *mut hew_cabi::vec::HewVec,
) -> *mut hew_cabi::vec::HewVec {
    // SAFETY: v validity forwarded to compress_op.
    unsafe { compress_op(v, hew_deflate_decompress) }
}

/// Zlib-compress a `bytes` `HewVec`, returning a new `bytes` `HewVec`.
///
/// # Safety
///
/// `v` must be a valid, non-null pointer to a `HewVec` (i32 elements).
#[no_mangle]
pub unsafe extern "C" fn hew_zlib_compress_hew(
    v: *mut hew_cabi::vec::HewVec,
) -> *mut hew_cabi::vec::HewVec {
    // SAFETY: v validity forwarded to compress_op.
    unsafe { compress_op(v, hew_zlib_compress) }
}

/// Zlib-decompress a `bytes` `HewVec`, returning a new `bytes` `HewVec`.
///
/// # Safety
///
/// `v` must be a valid, non-null pointer to a `HewVec` (i32 elements).
#[no_mangle]
pub unsafe extern "C" fn hew_zlib_decompress_hew(
    v: *mut hew_cabi::vec::HewVec,
) -> *mut hew_cabi::vec::HewVec {
    // SAFETY: v validity forwarded to compress_op.
    unsafe { compress_op(v, hew_zlib_decompress) }
}
