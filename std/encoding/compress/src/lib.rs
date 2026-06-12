//! Hew runtime: gzip, deflate, and zlib compression/decompression.
//!
//! Provides compression and decompression using gzip, raw deflate, and zlib
//! formats for compiled Hew programs. Callers must pass an explicit
//! `max_output_len` to each decompression entry point so compressed input fails
//! closed instead of expanding until OOM. A conservative starting cap is
//! [`DEFAULT_MAX_OUTPUT_LEN`] bytes; tighten it per call site when a smaller
//! decoded payload is expected. All returned buffers are allocated with
//! `libc::malloc` so callers can free them with [`hew_compress_free`].

// Force-link hew-runtime so the linker can resolve hew_vec_* and
// hew_bytes_* symbols referenced by hew-cabi and hew-runtime's object code.
#[cfg(test)]
extern crate hew_runtime;

use hew_runtime::bytes::BytesTriple;
use std::io::{self, Read};

use flate2::read::{
    DeflateDecoder, DeflateEncoder, GzDecoder, GzEncoder, ZlibDecoder, ZlibEncoder,
};
use flate2::Compression;
use hew_cabi::cabi::malloc_bytes;

/// Conservative starting point for explicit decompression caps.
pub const DEFAULT_MAX_OUTPUT_LEN: usize = 64 * 1024 * 1024;

std::thread_local! {
    static LAST_ERROR: std::cell::RefCell<Option<String>> =
        const { std::cell::RefCell::new(None) };
}

fn set_last_error(msg: impl Into<String>) {
    LAST_ERROR.with(|error| *error.borrow_mut() = Some(msg.into()));
}

fn clear_last_error() {
    LAST_ERROR.with(|error| *error.borrow_mut() = None);
}

#[cfg(test)]
fn get_last_error() -> String {
    LAST_ERROR.with(|error| error.borrow().clone().unwrap_or_else(String::new))
}

#[derive(Debug)]
enum CompressError {
    InvalidInput(&'static str),
    InvalidMaxOutputLen(i64),
    OutputLimitExceeded { limit: usize },
    Read(io::Error),
    AllocationFailed,
}

impl std::fmt::Display for CompressError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidInput(message) => f.write_str(message),
            Self::InvalidMaxOutputLen(limit) => {
                write!(f, "max output length must be non-negative, got {limit}")
            }
            Self::OutputLimitExceeded { limit } => {
                write!(f, "output exceeded {limit} byte limit")
            }
            Self::Read(err) => write!(f, "stream read failed: {err}"),
            Self::AllocationFailed => f.write_str("malloc failed"),
        }
    }
}

fn hew_max_output_len(max_output_len: i64) -> Result<usize, CompressError> {
    usize::try_from(max_output_len).map_err(|_| CompressError::InvalidMaxOutputLen(max_output_len))
}

/// Read input through `reader` into a `malloc`-allocated buffer.
///
/// Returns a pointer to the buffer and writes its length to `out_len`.
/// Returns null on read, size-limit, or allocation failure.
///
/// # Safety
///
/// `out_len` must point to a writable `usize`.
unsafe fn read_to_malloc(reader: impl Read, out_len: *mut usize, max_output_len: usize) -> *mut u8 {
    let take_limit = (max_output_len as u64).saturating_add(1);
    let mut limited_reader = reader.take(take_limit);
    let mut buf = Vec::new();
    if let Err(err) = limited_reader.read_to_end(&mut buf) {
        set_last_error(CompressError::Read(err).to_string());
        return std::ptr::null_mut();
    }
    if buf.len() > max_output_len {
        set_last_error(
            CompressError::OutputLimitExceeded {
                limit: max_output_len,
            }
            .to_string(),
        );
        return std::ptr::null_mut();
    }
    let buf_len = buf.len();
    let ptr = malloc_bytes(&buf);
    if ptr.is_null() {
        set_last_error(CompressError::AllocationFailed.to_string());
        return std::ptr::null_mut();
    }
    // SAFETY: out_len is a valid writable pointer per caller contract.
    unsafe { *out_len = buf_len };
    clear_last_error();
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

unsafe fn prepare_input<'a>(
    data: *const u8,
    len: usize,
    out_len: *mut usize,
) -> Result<&'a [u8], *mut u8> {
    if out_len.is_null() {
        set_last_error("output length pointer was null");
        return Err(std::ptr::null_mut());
    }
    // SAFETY: out_len is non-null and writable per caller contract.
    unsafe { *out_len = 0 };
    // SAFETY: caller guarantees data validity; forwarding contract.
    let Some(slice) = (unsafe { input_slice(data, len) }) else {
        set_last_error(
            CompressError::InvalidInput("input pointer was null with non-zero length").to_string(),
        );
        return Err(std::ptr::null_mut());
    };
    Ok(slice)
}

unsafe fn hew_gzip_compress_with_limit(
    data: *const u8,
    len: usize,
    out_len: *mut usize,
    max_output_len: usize,
) -> *mut u8 {
    // SAFETY: caller contract is forwarded to prepare_input.
    let slice = match unsafe { prepare_input(data, len, out_len) } {
        Ok(slice) => slice,
        Err(ptr) => return ptr,
    };
    // SAFETY: out_len is valid per caller contract; forwarding to read_to_malloc.
    unsafe {
        read_to_malloc(
            GzEncoder::new(slice, Compression::default()),
            out_len,
            max_output_len,
        )
    }
}

unsafe fn hew_gzip_decompress_with_limit(
    data: *const u8,
    len: usize,
    out_len: *mut usize,
    max_output_len: usize,
) -> *mut u8 {
    // SAFETY: caller contract is forwarded to prepare_input.
    let slice = match unsafe { prepare_input(data, len, out_len) } {
        Ok(slice) => slice,
        Err(ptr) => return ptr,
    };
    // SAFETY: out_len is valid per caller contract; forwarding to read_to_malloc.
    unsafe { read_to_malloc(GzDecoder::new(slice), out_len, max_output_len) }
}

unsafe fn hew_deflate_compress_with_limit(
    data: *const u8,
    len: usize,
    out_len: *mut usize,
    max_output_len: usize,
) -> *mut u8 {
    // SAFETY: caller contract is forwarded to prepare_input.
    let slice = match unsafe { prepare_input(data, len, out_len) } {
        Ok(slice) => slice,
        Err(ptr) => return ptr,
    };
    // SAFETY: out_len is valid per caller contract; forwarding to read_to_malloc.
    unsafe {
        read_to_malloc(
            DeflateEncoder::new(slice, Compression::default()),
            out_len,
            max_output_len,
        )
    }
}

unsafe fn hew_deflate_decompress_with_limit(
    data: *const u8,
    len: usize,
    out_len: *mut usize,
    max_output_len: usize,
) -> *mut u8 {
    // SAFETY: caller contract is forwarded to prepare_input.
    let slice = match unsafe { prepare_input(data, len, out_len) } {
        Ok(slice) => slice,
        Err(ptr) => return ptr,
    };
    // SAFETY: out_len is valid per caller contract; forwarding to read_to_malloc.
    unsafe { read_to_malloc(DeflateDecoder::new(slice), out_len, max_output_len) }
}

unsafe fn hew_zlib_compress_with_limit(
    data: *const u8,
    len: usize,
    out_len: *mut usize,
    max_output_len: usize,
) -> *mut u8 {
    // SAFETY: caller contract is forwarded to prepare_input.
    let slice = match unsafe { prepare_input(data, len, out_len) } {
        Ok(slice) => slice,
        Err(ptr) => return ptr,
    };
    // SAFETY: out_len is valid per caller contract; forwarding to read_to_malloc.
    unsafe {
        read_to_malloc(
            ZlibEncoder::new(slice, Compression::default()),
            out_len,
            max_output_len,
        )
    }
}

unsafe fn hew_zlib_decompress_with_limit(
    data: *const u8,
    len: usize,
    out_len: *mut usize,
    max_output_len: usize,
) -> *mut u8 {
    // SAFETY: caller contract is forwarded to prepare_input.
    let slice = match unsafe { prepare_input(data, len, out_len) } {
        Ok(slice) => slice,
        Err(ptr) => return ptr,
    };
    // SAFETY: out_len is valid per caller contract; forwarding to read_to_malloc.
    unsafe { read_to_malloc(ZlibDecoder::new(slice), out_len, max_output_len) }
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
    // SAFETY: caller contract is forwarded to hew_gzip_compress_with_limit.
    unsafe { hew_gzip_compress_with_limit(data, len, out_len, usize::MAX) }
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
    max_output_len: usize,
) -> *mut u8 {
    // SAFETY: caller contract is forwarded to hew_gzip_decompress_with_limit.
    unsafe { hew_gzip_decompress_with_limit(data, len, out_len, max_output_len) }
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
    // SAFETY: caller contract is forwarded to hew_deflate_compress_with_limit.
    unsafe { hew_deflate_compress_with_limit(data, len, out_len, usize::MAX) }
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
/// is 0. `out_len` must point to a writable `usize`. `max_output_len` is the
/// caller-chosen decompression cap.
#[no_mangle]
pub unsafe extern "C" fn hew_deflate_decompress(
    data: *const u8,
    len: usize,
    out_len: *mut usize,
    max_output_len: usize,
) -> *mut u8 {
    // SAFETY: caller contract is forwarded to hew_deflate_decompress_with_limit.
    unsafe { hew_deflate_decompress_with_limit(data, len, out_len, max_output_len) }
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
    // SAFETY: caller contract is forwarded to hew_zlib_compress_with_limit.
    unsafe { hew_zlib_compress_with_limit(data, len, out_len, usize::MAX) }
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
/// is 0. `out_len` must point to a writable `usize`. `max_output_len` is the
/// caller-chosen decompression cap.
#[no_mangle]
pub unsafe extern "C" fn hew_zlib_decompress(
    data: *const u8,
    len: usize,
    out_len: *mut usize,
    max_output_len: usize,
) -> *mut u8 {
    // SAFETY: caller contract is forwarded to hew_zlib_decompress_with_limit.
    unsafe { hew_zlib_decompress_with_limit(data, len, out_len, max_output_len) }
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
    unsafe { libc::free(ptr.cast()) }; // CSTRING-FREE: libc-bytes (read_to_malloc = malloc_bytes byte buffer)
}

// ---------------------------------------------------------------------------
// BytesTriple-ABI wrappers (used by std/compress.hew)
//
// All `_hew` entry points follow the v0.5 BytesTriple ABI:
//   - `bytes` input args are passed as `*const BytesTriple` (by-pointer consumer).
//   - `bytes` return values are returned as `BytesTriple` by value, with a
//     matching `_raw` sibling that writes through a caller-supplied `*mut BytesTriple`
//     for use in the compiler's call-site out-pointer path.
//
// WHEN OBSOLETE: when codegen emits the correct C-ABI coercion for by-value
// struct arguments/returns (via `byval`/coerced-int lowering), the by-pointer
// consumer convention and `_raw` sidecars can be removed and the Rust functions
// can use `BytesTriple` directly in all positions.
// ---------------------------------------------------------------------------

/// Extract a byte slice from a `BytesTriple` by-pointer consumer argument.
///
/// Returns an empty slice for a null pointer or a zero-length triple.
///
/// # Safety
///
/// If non-null, `triple` must point to a live `BytesTriple` whose payload
/// is valid for at least `offset + len` bytes.
unsafe fn bytes_slice_from_triple<'a>(triple: *const BytesTriple) -> &'a [u8] {
    if triple.is_null() {
        return &[];
    }
    // SAFETY: caller guarantees triple is non-null and valid.
    let t = unsafe { &*triple };
    if t.len == 0 || t.ptr.is_null() {
        return &[];
    }
    let offset = t.offset as usize;
    let len = t.len as usize;
    // SAFETY: t.ptr + offset is valid for len bytes per BytesTriple invariant.
    unsafe { std::slice::from_raw_parts(t.ptr.add(offset), len) }
}

/// Allocate a `BytesTriple` from a byte slice via the runtime allocator.
fn bytes_triple_from_slice(data: &[u8]) -> BytesTriple {
    if data.is_empty() {
        return BytesTriple {
            ptr: std::ptr::null_mut(),
            offset: 0,
            len: 0,
        };
    }
    let len = u32::try_from(data.len()).unwrap_or(u32::MAX);
    // SAFETY: data is valid for len bytes; hew_bytes_from_static copies the slice.
    unsafe { hew_runtime::bytes::hew_bytes_from_static(data.as_ptr(), len) }
}

/// Helper: compress input triple via `f(ptr, len, &out_len) -> *mut u8`.
///
/// # Safety
///
/// `triple` must be null or a valid `*const BytesTriple`.
unsafe fn compress_triple(
    triple: *const BytesTriple,
    f: unsafe extern "C" fn(*const u8, usize, *mut usize) -> *mut u8,
) -> BytesTriple {
    // SAFETY: forwarded per function contract.
    let input = unsafe { bytes_slice_from_triple(triple) };
    let mut out_len: usize = 0;
    // SAFETY: input slice is valid; out_len is writable.
    let ptr = unsafe { f(input.as_ptr(), input.len(), &raw mut out_len) };
    if ptr.is_null() {
        return BytesTriple {
            ptr: std::ptr::null_mut(),
            offset: 0,
            len: 0,
        };
    }
    // SAFETY: ptr is valid for out_len bytes.
    let slice = unsafe { std::slice::from_raw_parts(ptr, out_len) };
    let result = bytes_triple_from_slice(slice);
    // SAFETY: ptr was allocated by the codec function via libc::malloc.
    unsafe { hew_compress_free(ptr) };
    result
}

/// Helper: decompress input triple via `f(ptr, len, &out_len, max) -> *mut u8`.
///
/// # Safety
///
/// `triple` must be null or a valid `*const BytesTriple`.
unsafe fn decompress_triple(
    triple: *const BytesTriple,
    max_output_len: i64,
    f: unsafe extern "C" fn(*const u8, usize, *mut usize, usize) -> *mut u8,
) -> BytesTriple {
    let max_output_len = match hew_max_output_len(max_output_len) {
        Ok(limit) => limit,
        Err(err) => {
            set_last_error(err.to_string());
            return BytesTriple {
                ptr: std::ptr::null_mut(),
                offset: 0,
                len: 0,
            };
        }
    };
    // SAFETY: forwarded per function contract.
    let input = unsafe { bytes_slice_from_triple(triple) };
    let mut out_len: usize = 0;
    // SAFETY: input slice is valid; out_len is writable.
    let ptr = unsafe {
        f(
            input.as_ptr(),
            input.len(),
            &raw mut out_len,
            max_output_len,
        )
    };
    if ptr.is_null() {
        return BytesTriple {
            ptr: std::ptr::null_mut(),
            offset: 0,
            len: 0,
        };
    }
    // SAFETY: ptr is valid for out_len bytes.
    let slice = unsafe { std::slice::from_raw_parts(ptr, out_len) };
    let result = bytes_triple_from_slice(slice);
    // SAFETY: ptr was allocated by the codec function via libc::malloc.
    unsafe { hew_compress_free(ptr) };
    result
}

/// Gzip-compress `data` (`bytes`), returning a new `BytesTriple`.
///
/// Receives `data` as `*const BytesTriple` (by-pointer consumer convention).
/// Returns a `BytesTriple` by value; see `hew_gzip_compress_hew_raw` for the
/// out-pointer variant used by some codegen paths.
///
/// # Safety
///
/// `data` must be null or a valid `*const BytesTriple` for the duration of
/// the call.
#[no_mangle]
pub unsafe extern "C" fn hew_gzip_compress_hew(data: *const BytesTriple) -> BytesTriple {
    // SAFETY: data is null-or-valid per caller contract.
    unsafe { compress_triple(data, hew_gzip_compress) }
}

/// Out-pointer sibling for `hew_gzip_compress_hew`.
///
/// # Safety
///
/// `data` must be null or a valid `*const BytesTriple`. `out` must be a valid,
/// writable `*mut BytesTriple` slot.
#[no_mangle]
pub unsafe extern "C" fn hew_gzip_compress_hew_raw(
    data: *const BytesTriple,
    out: *mut BytesTriple,
) {
    // SAFETY: caller contract forwarded.
    let triple = unsafe { hew_gzip_compress_hew(data) };
    // SAFETY: out is a valid writable slot per caller contract.
    unsafe { out.write(triple) };
}

/// Gzip-decompress `data` (`bytes`), returning a new `BytesTriple`.
///
/// # Safety
///
/// `data` must be null or a valid `*const BytesTriple`. `max_output_len` must
/// be non-negative.
#[no_mangle]
pub unsafe extern "C" fn hew_gzip_decompress_hew(
    data: *const BytesTriple,
    max_output_len: i64,
) -> BytesTriple {
    // SAFETY: data is null-or-valid per caller contract.
    unsafe { decompress_triple(data, max_output_len, hew_gzip_decompress) }
}

/// Out-pointer sibling for `hew_gzip_decompress_hew`.
///
/// # Safety
///
/// `data` must be null or a valid `*const BytesTriple`. `out` must be a valid,
/// writable `*mut BytesTriple` slot.
#[no_mangle]
pub unsafe extern "C" fn hew_gzip_decompress_hew_raw(
    data: *const BytesTriple,
    max_output_len: i64,
    out: *mut BytesTriple,
) {
    // SAFETY: caller contract forwarded.
    let triple = unsafe { hew_gzip_decompress_hew(data, max_output_len) };
    // SAFETY: out is a valid writable slot per caller contract.
    unsafe { out.write(triple) };
}

/// Deflate-compress `data` (`bytes`), returning a new `BytesTriple`.
///
/// # Safety
///
/// `data` must be null or a valid `*const BytesTriple`.
#[no_mangle]
pub unsafe extern "C" fn hew_deflate_compress_hew(data: *const BytesTriple) -> BytesTriple {
    // SAFETY: data is null-or-valid per caller contract.
    unsafe { compress_triple(data, hew_deflate_compress) }
}

/// Out-pointer sibling for `hew_deflate_compress_hew`.
///
/// # Safety
///
/// `data` must be null or a valid `*const BytesTriple`. `out` must be a valid,
/// writable `*mut BytesTriple` slot.
#[no_mangle]
pub unsafe extern "C" fn hew_deflate_compress_hew_raw(
    data: *const BytesTriple,
    out: *mut BytesTriple,
) {
    // SAFETY: caller contract forwarded.
    let triple = unsafe { hew_deflate_compress_hew(data) };
    // SAFETY: out is a valid writable slot per caller contract.
    unsafe { out.write(triple) };
}

/// Deflate-decompress `data` (`bytes`), returning a new `BytesTriple`.
///
/// # Safety
///
/// `data` must be null or a valid `*const BytesTriple`. `max_output_len` must
/// be non-negative.
#[no_mangle]
pub unsafe extern "C" fn hew_deflate_decompress_hew(
    data: *const BytesTriple,
    max_output_len: i64,
) -> BytesTriple {
    // SAFETY: data is null-or-valid per caller contract.
    unsafe { decompress_triple(data, max_output_len, hew_deflate_decompress) }
}

/// Out-pointer sibling for `hew_deflate_decompress_hew`.
///
/// # Safety
///
/// `data` must be null or a valid `*const BytesTriple`. `out` must be a valid,
/// writable `*mut BytesTriple` slot.
#[no_mangle]
pub unsafe extern "C" fn hew_deflate_decompress_hew_raw(
    data: *const BytesTriple,
    max_output_len: i64,
    out: *mut BytesTriple,
) {
    // SAFETY: caller contract forwarded.
    let triple = unsafe { hew_deflate_decompress_hew(data, max_output_len) };
    // SAFETY: out is a valid writable slot per caller contract.
    unsafe { out.write(triple) };
}

/// Zlib-compress `data` (`bytes`), returning a new `BytesTriple`.
///
/// # Safety
///
/// `data` must be null or a valid `*const BytesTriple`.
#[no_mangle]
pub unsafe extern "C" fn hew_zlib_compress_hew(data: *const BytesTriple) -> BytesTriple {
    // SAFETY: data is null-or-valid per caller contract.
    unsafe { compress_triple(data, hew_zlib_compress) }
}

/// Out-pointer sibling for `hew_zlib_compress_hew`.
///
/// # Safety
///
/// `data` must be null or a valid `*const BytesTriple`. `out` must be a valid,
/// writable `*mut BytesTriple` slot.
#[no_mangle]
pub unsafe extern "C" fn hew_zlib_compress_hew_raw(
    data: *const BytesTriple,
    out: *mut BytesTriple,
) {
    // SAFETY: caller contract forwarded.
    let triple = unsafe { hew_zlib_compress_hew(data) };
    // SAFETY: out is a valid writable slot per caller contract.
    unsafe { out.write(triple) };
}

/// Zlib-decompress `data` (`bytes`), returning a new `BytesTriple`.
///
/// # Safety
///
/// `data` must be null or a valid `*const BytesTriple`. `max_output_len` must
/// be non-negative.
#[no_mangle]
pub unsafe extern "C" fn hew_zlib_decompress_hew(
    data: *const BytesTriple,
    max_output_len: i64,
) -> BytesTriple {
    // SAFETY: data is null-or-valid per caller contract.
    unsafe { decompress_triple(data, max_output_len, hew_zlib_decompress) }
}

/// Out-pointer sibling for `hew_zlib_decompress_hew`.
///
/// # Safety
///
/// `data` must be null or a valid `*const BytesTriple`. `out` must be a valid,
/// writable `*mut BytesTriple` slot.
#[no_mangle]
pub unsafe extern "C" fn hew_zlib_decompress_hew_raw(
    data: *const BytesTriple,
    max_output_len: i64,
    out: *mut BytesTriple,
) {
    // SAFETY: caller contract forwarded.
    let triple = unsafe { hew_zlib_decompress_hew(data, max_output_len) };
    // SAFETY: out is a valid writable slot per caller contract.
    unsafe { out.write(triple) };
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    fn copy_and_free(ptr: *mut u8, len: usize) -> Vec<u8> {
        assert!(!ptr.is_null(), "pointer must be non-null");
        // SAFETY: ptr is valid for len bytes and allocated by this module.
        let bytes = unsafe { std::slice::from_raw_parts(ptr, len) }.to_vec();
        // SAFETY: ptr was allocated by this module.
        unsafe { hew_compress_free(ptr) };
        bytes
    }

    fn assert_limit_error_contains(expected: &str) {
        let err = get_last_error();
        assert!(
            err.contains(expected),
            "expected error containing {expected:?}, got {err:?}"
        );
    }

    fn gzip_zeros_stream(total_len: usize) -> Vec<u8> {
        let mut encoder = flate2::write::GzEncoder::new(Vec::new(), Compression::best());
        let chunk = vec![0_u8; 64 * 1024];
        let mut remaining = total_len;
        while remaining > 0 {
            let take = remaining.min(chunk.len());
            encoder
                .write_all(&chunk[..take])
                .expect("streaming gzip write should succeed");
            remaining -= take;
        }
        encoder.finish().expect("finishing gzip should succeed")
    }

    /// Helper: compress then decompress, asserting the roundtrip matches.
    unsafe fn assert_roundtrip(
        input: &[u8],
        compress_fn: unsafe extern "C" fn(*const u8, usize, *mut usize) -> *mut u8,
        decompress_fn: unsafe extern "C" fn(*const u8, usize, *mut usize, usize) -> *mut u8,
    ) {
        let mut compressed_len: usize = 0;
        // SAFETY: input is a valid slice; compressed_len is writable.
        let compressed =
            unsafe { compress_fn(input.as_ptr(), input.len(), &raw mut compressed_len) };
        assert!(!compressed.is_null(), "compression returned null");
        assert!(compressed_len > 0 || input.is_empty());

        let mut decompressed_len: usize = 0;
        // SAFETY: compressed is valid for compressed_len bytes; decompressed_len is writable.
        let decompressed = unsafe {
            decompress_fn(
                compressed,
                compressed_len,
                &raw mut decompressed_len,
                input.len(),
            )
        };
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
    fn empty_input_compress_returns_nonnull_and_roundtrips() {
        // Empty-input path exercises the malloc_bytes sentinel allocation.
        // Gzip always emits a non-empty stream (header + trailer) even for
        // empty input, so compressed_len > 0 and the decompressed result is
        // empty but the returned pointer is non-null (1-byte sentinel).
        // SAFETY: empty slice is a valid input for all compression functions.
        unsafe {
            let mut compressed_len: usize = 0;
            let compressed = hew_gzip_compress([].as_ptr(), 0, &raw mut compressed_len);
            assert!(
                !compressed.is_null(),
                "gzip compress empty must return non-null"
            );

            let mut decompressed_len: usize = 999;
            let decompressed = hew_gzip_decompress(
                compressed,
                compressed_len,
                &raw mut decompressed_len,
                DEFAULT_MAX_OUTPUT_LEN,
            );
            assert!(
                !decompressed.is_null(),
                "gzip decompress empty must return non-null"
            );
            assert_eq!(
                decompressed_len, 0,
                "decompressed length of empty input must be 0"
            );

            hew_compress_free(compressed);
            hew_compress_free(decompressed);
        }
    }

    #[test]
    fn test_large_data() {
        // 64 KiB of repeating pattern — should compress well.
        #[expect(
            clippy::cast_sign_loss,
            reason = "test data: modular result always fits in u8"
        )]
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
            unsafe { hew_gzip_compress(input.as_ptr(), input.len(), &raw mut compressed_len) };
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
        let result = unsafe { hew_gzip_compress(std::ptr::null(), 10, &raw mut out_len) };
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

    #[test]
    fn test_gzip_zero_burst_roundtrip_with_explicit_limit() {
        let input = vec![0_u8; 10 * 1024 * 1024];

        let mut compressed_len: usize = 0;
        // SAFETY: input is a valid byte slice.
        let compressed =
            unsafe { hew_gzip_compress(input.as_ptr(), input.len(), &raw mut compressed_len) };
        assert!(!compressed.is_null(), "compression returned null");
        assert!(
            compressed_len < 32 * 1024,
            "expected 10 MiB of zeros to compress below 32 KiB, got {compressed_len}"
        );

        let mut decompressed_len: usize = 0;
        // SAFETY: compressed is valid for compressed_len bytes.
        let decompressed = unsafe {
            hew_gzip_decompress(
                compressed,
                compressed_len,
                &raw mut decompressed_len,
                DEFAULT_MAX_OUTPUT_LEN,
            )
        };
        assert!(!decompressed.is_null(), "decompression returned null");
        assert_eq!(decompressed_len, input.len());
        // SAFETY: decompressed is valid for decompressed_len bytes.
        let roundtrip = unsafe { std::slice::from_raw_parts(decompressed, decompressed_len) };
        assert_eq!(roundtrip, input);

        // SAFETY: both pointers were allocated by this module.
        unsafe {
            hew_compress_free(compressed);
            hew_compress_free(decompressed);
        };
    }

    #[test]
    fn test_gzip_decompress_rejects_large_bomb() {
        const BOMB_UNCOMPRESSED_LEN: usize = 10 * 1024 * 1024;
        const LOW_LIMIT: usize = 1024 * 1024;

        let compressed = gzip_zeros_stream(BOMB_UNCOMPRESSED_LEN);
        assert!(
            compressed.len() < 16 * 1024,
            "expected a small compressed bomb, got {} bytes",
            compressed.len()
        );

        let mut out_len: usize = 123;
        // SAFETY: compressed is a valid byte slice.
        let decompressed = unsafe {
            hew_gzip_decompress(
                compressed.as_ptr(),
                compressed.len(),
                &raw mut out_len,
                LOW_LIMIT,
            )
        };
        assert!(decompressed.is_null(), "bomb decompression should fail");
        assert_eq!(out_len, 0, "failing decode must zero out_len");
        assert_limit_error_contains("1048576 byte limit");
    }

    #[test]
    fn test_gzip_decompress_boundary_at_limit() {
        const LIMIT: usize = 64 * 1024;

        let exact_input = vec![0_u8; LIMIT];
        let over_input = vec![0_u8; LIMIT + 1];
        let mut compressed_exact_len: usize = 0;
        let mut compressed_over_len: usize = 0;
        // SAFETY: both inputs are valid byte slices.
        let compressed_exact = unsafe {
            hew_gzip_compress_with_limit(
                exact_input.as_ptr(),
                exact_input.len(),
                &raw mut compressed_exact_len,
                usize::MAX,
            )
        };
        // SAFETY: over_input is a valid byte slice.
        let compressed_over = unsafe {
            hew_gzip_compress_with_limit(
                over_input.as_ptr(),
                over_input.len(),
                &raw mut compressed_over_len,
                usize::MAX,
            )
        };
        let compressed_exact_bytes = copy_and_free(compressed_exact, compressed_exact_len);
        let compressed_over_bytes = copy_and_free(compressed_over, compressed_over_len);

        let mut exact_len: usize = 0;
        // SAFETY: compressed_exact_bytes is a valid byte slice.
        let exact = unsafe {
            hew_gzip_decompress_with_limit(
                compressed_exact_bytes.as_ptr(),
                compressed_exact_bytes.len(),
                &raw mut exact_len,
                LIMIT,
            )
        };
        assert!(!exact.is_null(), "exact-limit decode should succeed");
        assert_eq!(exact_len, LIMIT);
        // SAFETY: exact was allocated by this module.
        unsafe { hew_compress_free(exact) };

        let mut over_len: usize = LIMIT;
        // SAFETY: compressed_over_bytes is a valid byte slice.
        let over = unsafe {
            hew_gzip_decompress_with_limit(
                compressed_over_bytes.as_ptr(),
                compressed_over_bytes.len(),
                &raw mut over_len,
                LIMIT,
            )
        };
        assert!(over.is_null(), "cap+1 decode should fail");
        assert_eq!(over_len, 0, "failing decode must zero out_len");
        assert_limit_error_contains("65536 byte limit");
    }

    #[test]
    fn test_gzip_compress_boundary_at_limit() {
        #[expect(
            clippy::cast_possible_truncation,
            reason = "test data masks values to one byte before the cast"
        )]
        let input: Vec<u8> = (0usize..131_072)
            .map(|i| ((i.wrapping_mul(17) ^ (i >> 3)) & 0xff) as u8)
            .collect();

        let mut baseline_len: usize = 0;
        // SAFETY: input is a valid byte slice.
        let baseline = unsafe {
            hew_gzip_compress_with_limit(
                input.as_ptr(),
                input.len(),
                &raw mut baseline_len,
                usize::MAX,
            )
        };
        assert!(!baseline.is_null(), "baseline compression should succeed");
        // SAFETY: baseline was allocated by this module.
        unsafe { hew_compress_free(baseline) };

        let mut exact_len: usize = 0;
        // SAFETY: input is a valid byte slice.
        let exact = unsafe {
            hew_gzip_compress_with_limit(
                input.as_ptr(),
                input.len(),
                &raw mut exact_len,
                baseline_len,
            )
        };
        assert!(
            !exact.is_null(),
            "exact compressed-size limit should succeed"
        );
        assert_eq!(exact_len, baseline_len);
        // SAFETY: exact was allocated by this module.
        unsafe { hew_compress_free(exact) };

        let mut over_len: usize = baseline_len;
        // SAFETY: input is a valid byte slice.
        let over = unsafe {
            hew_gzip_compress_with_limit(
                input.as_ptr(),
                input.len(),
                &raw mut over_len,
                baseline_len.saturating_sub(1),
            )
        };
        assert!(
            over.is_null(),
            "compressed output above the cap should fail"
        );
        assert_eq!(over_len, 0, "failing encode must zero out_len");
        assert_limit_error_contains(&format!("{} byte limit", baseline_len - 1));
    }
}
