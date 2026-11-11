//! `std.encoding.compress`: gzip, raw deflate and zlib streams.
//!
//! Input: one selector byte, a two-byte output limit, then the stream.
//! Properties: decompression never panics and never returns more than the
//! caller's limit, so a small bomb is refused rather than inflated; below the
//! limit it agrees with `flate2` read directly; and compressing any input
//! then decompressing it gives the input back.
#![no_main]

use std::io::Read;

use hew_std::compress::{
    hew_compress_free, hew_deflate_compress, hew_deflate_decompress, hew_gzip_compress,
    hew_gzip_decompress, hew_zlib_compress, hew_zlib_decompress,
};
use libfuzzer_sys::fuzz_target;

type Compress = unsafe extern "C" fn(*const u8, usize, *mut usize) -> *mut u8;
type Decompress = unsafe extern "C" fn(*const u8, usize, *mut usize, usize) -> *mut u8;

fn collect(ptr: *mut u8, len: usize) -> Option<Vec<u8>> {
    if ptr.is_null() {
        return None;
    }
    // SAFETY: a non-null result holds `len` bytes until freed below.
    let bytes = unsafe { std::slice::from_raw_parts(ptr, len) }.to_vec();
    // SAFETY: a codec result, released once.
    unsafe { hew_compress_free(ptr) };
    Some(bytes)
}

fn reference(selector: u8, stream: &[u8], limit: usize) -> Option<Vec<u8>> {
    let take = u64::try_from(limit).ok()? + 1;
    let mut out = Vec::new();
    let read = match selector {
        0 => flate2::read::GzDecoder::new(stream)
            .take(take)
            .read_to_end(&mut out),
        1 => flate2::read::DeflateDecoder::new(stream)
            .take(take)
            .read_to_end(&mut out),
        _ => flate2::read::ZlibDecoder::new(stream)
            .take(take)
            .read_to_end(&mut out),
    };
    (read.is_ok() && out.len() <= limit).then_some(out)
}

fuzz_target!(|data: &[u8]| {
    let [selector, hi, lo, stream @ ..] = data else {
        return;
    };
    let selector = selector % 3;
    let (compress, decompress): (Compress, Decompress) = match selector {
        0 => (hew_gzip_compress, hew_gzip_decompress),
        1 => (hew_deflate_compress, hew_deflate_decompress),
        _ => (hew_zlib_compress, hew_zlib_decompress),
    };
    let limit = usize::from(u16::from_be_bytes([*hi, *lo])) * 64;

    let mut len = 0usize;
    // SAFETY: `stream` is live and `len` writable.
    let out = collect(
        unsafe { decompress(stream.as_ptr(), stream.len(), &raw mut len, limit) },
        len,
    );
    if let Some(out) = &out {
        assert!(
            out.len() <= limit,
            "{} bytes exceed the {limit}-byte limit",
            out.len()
        );
    }
    assert_eq!(out, reference(selector, stream, limit), "decoders disagree");

    let mut packed_len = 0usize;
    // SAFETY: as above.
    let packed = collect(
        unsafe { compress(stream.as_ptr(), stream.len(), &raw mut packed_len) },
        packed_len,
    )
    .expect("compression accepts any input");
    let mut back_len = 0usize;
    // SAFETY: as above.
    let back = collect(
        unsafe {
            decompress(
                packed.as_ptr(),
                packed.len(),
                &raw mut back_len,
                stream.len(),
            )
        },
        back_len,
    );
    assert_eq!(
        back.as_deref(),
        Some(stream),
        "round trip changed the bytes"
    );
});
