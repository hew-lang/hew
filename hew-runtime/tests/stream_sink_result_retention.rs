//! Measured ownership of the file-backed `Sink` constructor.
//!
//! A `fresh` FFI row is not enough to mint a caller-side `hew_sink_close`: the
//! retention question is whether the runtime keeps a second authority over the
//! handle it just returned. `hew_stream_from_file_write` is the sink twin of
//! `hew_stream_from_file_read`, which already carries the measured row; without
//! the same evidence for the write side the compiler cannot establish ownership
//! of a `Sink` payload returned through `std.stream.to_file`.
//!
//! The string oracles' three probes translate to a handle as follows.
//! R1 keeps two results live and requires distinct addresses, so the runtime
//! cannot be handing back one cached handle. R2 is the sole-owner reading: a
//! `Sink` has no refcount header, so the equivalent evidence is that releasing
//! one handle leaves the other fully usable — a retained second authority would
//! have closed or invalidated it. R3 releases both and then produces a third
//! from the same path, establishing that the caller's release did not consume
//! producer state. Together they establish that the one close authority at
//! handoff is the caller's.
//!
//! What this cannot see is the same blind spot the file header records for the
//! string oracles: a runtime that hands back a sole-owned box and separately
//! stashes a raw pointer it frees at teardown. Read against the implementation:
//! `hew_stream_from_file_write` returns `into_write_sink_ptr(fs::File::create)`,
//! which is a bare `Box::into_raw` of a freshly built `HewSink`, and the runtime
//! records no table entry or global for it.

#![cfg(unix)]

use std::ffi::CString;

use hew_runtime::stream::{
    hew_sink_close, hew_sink_is_valid, hew_sink_write_string, hew_stream_from_file_write,
};

fn c_path(path: &std::path::Path) -> CString {
    CString::new(path.to_string_lossy().as_bytes()).expect("temporary path contains no NUL")
}

#[test]
fn file_write_sink_result_is_transferred() {
    let dir = tempfile::tempdir().expect("temporary directory");
    let first_path = c_path(&dir.path().join("first.txt"));
    let second_path = c_path(&dir.path().join("second.txt"));

    // SAFETY: both paths are live NUL-terminated C strings for these calls.
    let (first, second) = unsafe {
        (
            hew_stream_from_file_write(first_path.as_ptr()),
            hew_stream_from_file_write(second_path.as_ptr()),
        )
    };
    assert!(
        !first.is_null() && !second.is_null(),
        "hew_stream_from_file_write: expected two non-null sinks"
    );

    // R1: a runtime-cached handle would return one live address twice. These
    // two callers must own independent sinks simultaneously.
    assert_ne!(
        first, second,
        "hew_stream_from_file_write: two live sinks share an address rather than \
         transferring fresh handles"
    );

    // R2: the caller's close is the sole close authority. Releasing `first`
    // must leave `second` usable; a retained second authority over the same
    // backing would have closed or invalidated it here.
    // SAFETY: `first` is a live sink returned above and is not used again.
    unsafe { hew_sink_close(first) };
    assert_eq!(
        hew_sink_is_valid(second),
        1,
        "hew_stream_from_file_write: releasing one result invalidated the other"
    );
    let payload = CString::new("sink retention witness").expect("literal contains no NUL");
    // SAFETY: `second` is still live and `payload` outlives the call.
    unsafe { hew_sink_write_string(second, payload.as_ptr()) };
    // SAFETY: `second` is live and is not used again.
    unsafe { hew_sink_close(second) };

    // R3: producer state survives the caller's releases — a third sink over the
    // first path still opens and still writes.
    // SAFETY: `first_path` is unchanged and still NUL-terminated.
    let third = unsafe { hew_stream_from_file_write(first_path.as_ptr()) };
    assert!(
        !third.is_null(),
        "hew_stream_from_file_write: producer state did not survive releasing \
         earlier results"
    );
    // SAFETY: `third` is live and `payload` outlives the call.
    unsafe { hew_sink_write_string(third, payload.as_ptr()) };
    // SAFETY: `third` is live and is not used again.
    unsafe { hew_sink_close(third) };

    let written = std::fs::read(dir.path().join("second.txt")).expect("read second sink output");
    assert_eq!(
        written,
        payload.as_bytes(),
        "hew_stream_from_file_write: the surviving sink did not write its payload"
    );
    let rewritten = std::fs::read(dir.path().join("first.txt")).expect("read third sink output");
    assert_eq!(
        rewritten,
        payload.as_bytes(),
        "hew_stream_from_file_write: the post-release sink did not write its payload"
    );
}
