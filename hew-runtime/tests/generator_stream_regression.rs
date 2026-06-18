//! Regression tests for stream bugs.
//!
//! The generator regression tests that lived here exercised the thread-based
//! generator runtime (`hew_runtime::generator`), which was deleted when
//! generators moved onto the `llvm.coro` continuation substrate. The generator
//! behaviours they covered (post-completion `next` returns null, a null yield
//! does not terminate) are now proven on the coro path by the generator `.hew`
//! corpus (`tests/hew/generator_coro_corpus_test.hew`).

use hew_runtime::stream::{hew_stream_lines, hew_stream_next_sized, HewStream};

/// Helper: create a `HewStream` from a byte slice using `hew_stream_from_bytes`.
unsafe fn stream_from_bytes(data: &[u8]) -> *mut HewStream {
    // SAFETY: data is a valid byte slice; caller guarantees pointer validity.
    unsafe { hew_runtime::stream::hew_stream_from_bytes(data.as_ptr(), data.len(), 0) }
}

#[test]
fn stream_lines_empty_line_preserved() {
    // SAFETY: All stream FFI calls use valid pointers from hew_stream_from_bytes/hew_stream_lines.
    unsafe {
        let input = b"hello\n\nworld\n";
        let raw = stream_from_bytes(input);
        let lines = hew_stream_lines(raw);

        let mut items: Vec<String> = Vec::new();
        loop {
            let mut size: usize = 0;
            let ptr = hew_stream_next_sized(lines, std::ptr::addr_of_mut!(size));
            if ptr.is_null() {
                break;
            }
            // Empty items return a non-null buffer with size 0 — only EOF is
            // null, preserving the empty-line invariant on the sized accessor.
            let bytes = std::slice::from_raw_parts(ptr.cast::<u8>(), size);
            items.push(String::from_utf8_lossy(bytes).into_owned());
            libc::free(ptr); // ALLOCATOR-PAIRING: libc
        }

        assert_eq!(items.len(), 3, "expected 3 lines, got {items:?}");
        assert_eq!(items[0], "hello");
        assert_eq!(items[1], "", "second line should be empty");
        assert_eq!(items[2], "world");

        hew_runtime::stream::hew_stream_close(lines);
    }
}
