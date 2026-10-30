//! A declared container length is untrusted: decoding reserves heap for the
//! elements an input holds, not for the count its headers claim.

use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicUsize, Ordering};

use hew_codec::{Format, Source};

struct Counting;

static LIVE: AtomicUsize = AtomicUsize::new(0);
static PEAK: AtomicUsize = AtomicUsize::new(0);

// SAFETY: every method forwards to `System` and only adjusts counters.
unsafe impl GlobalAlloc for Counting {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        // SAFETY: forwarded unchanged.
        let ptr = unsafe { System.alloc(layout) };
        if !ptr.is_null() {
            let live = LIVE.fetch_add(layout.size(), Ordering::SeqCst) + layout.size();
            PEAK.fetch_max(live, Ordering::SeqCst);
        }
        ptr
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        LIVE.fetch_sub(layout.size(), Ordering::SeqCst);
        // SAFETY: forwarded unchanged.
        unsafe { System.dealloc(ptr, layout) };
    }
}

#[global_allocator]
static ALLOC: Counting = Counting;

/// Peak live heap while parsing `input` in `format`, beyond what was live before.
fn parse_peak(format: Format, input: &[u8]) -> usize {
    let base = LIVE.load(Ordering::SeqCst);
    PEAK.store(base, Ordering::SeqCst);
    let parsed = Source::new(format, input);
    let peak = PEAK.load(Ordering::SeqCst) - base;
    assert!(parsed.is_err(), "the truncated document is refused");
    peak
}

/// One nesting level per header: a map claiming 2^32 - 1 entries whose first
/// key is the integer 0, then the next level as its value.
fn nested(format: Format, levels: usize) -> Vec<u8> {
    let level: &[u8] = match format {
        Format::Cbor => &[0xba, 0xff, 0xff, 0xff, 0xff, 0x00],
        Format::Msgpack => &[0xdf, 0xff, 0xff, 0xff, 0xff, 0x00],
        _ => unreachable!("binary formats only"),
    };
    level.repeat(levels)
}

#[test]
fn nested_declared_lengths_do_not_reserve_heap() {
    for format in [Format::Cbor, Format::Msgpack] {
        let input = nested(format, 100);
        let peak = parse_peak(format, &input);
        assert!(
            peak < 64 * 1024,
            "{format:?}: a {}-byte input reserved {peak} bytes",
            input.len()
        );
    }
}
