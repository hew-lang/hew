//! Shared harness pieces: a heap meter and the format selector.
//!
//! The meter wraps the system allocator and records the peak live heap inside
//! one [`bounded`] call, so a decoder that preallocates from a declared length
//! fails the run even when the pages are never touched and RSS stays low.

use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicUsize, Ordering::Relaxed};

use hew_codec::Format;

struct Meter;

static LIVE: AtomicUsize = AtomicUsize::new(0);
static PEAK: AtomicUsize = AtomicUsize::new(0);

fn grew(by: usize) {
    let now = LIVE.fetch_add(by, Relaxed) + by;
    PEAK.fetch_max(now, Relaxed);
}

// SAFETY: every call forwards to `System` unchanged; the counters only observe.
unsafe impl GlobalAlloc for Meter {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        // SAFETY: forwarded caller contract.
        let ptr = unsafe { System.alloc(layout) };
        if !ptr.is_null() {
            grew(layout.size());
        }
        ptr
    }

    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        // SAFETY: forwarded caller contract.
        let ptr = unsafe { System.alloc_zeroed(layout) };
        if !ptr.is_null() {
            grew(layout.size());
        }
        ptr
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        LIVE.fetch_sub(layout.size(), Relaxed);
        // SAFETY: forwarded caller contract.
        unsafe { System.dealloc(ptr, layout) };
    }

    unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
        // SAFETY: forwarded caller contract.
        let new = unsafe { System.realloc(ptr, layout, new_size) };
        if !new.is_null() {
            LIVE.fetch_sub(layout.size(), Relaxed);
            grew(new_size);
        }
        new
    }
}

#[global_allocator]
static GLOBAL: Meter = Meter;

/// Run `f` and fail when its peak live heap exceeds `limit` bytes.
pub fn bounded<R>(what: &str, limit: usize, f: impl FnOnce() -> R) -> R {
    let base = LIVE.load(Relaxed);
    PEAK.store(base, Relaxed);
    let result = f();
    let used = PEAK.load(Relaxed).saturating_sub(base);
    assert!(
        used <= limit,
        "{what}: peak heap {used} bytes exceeds the {limit}-byte bound"
    );
    result
}

/// Heap a parse of `input_len` bytes may use: generous per-byte expansion
/// over a fixed floor. A declared length or alias fan-out blows far past it.
#[must_use]
pub fn heap_bound(input_len: usize) -> usize {
    (16 << 20) + input_len * 512
}

/// The format a leading selector byte names.
#[allow(
    dead_code,
    reason = "shared by every fuzz target; each target uses a subset"
)]
#[must_use]
pub fn format(selector: u8) -> Format {
    match selector % 5 {
        0 => Format::Json,
        1 => Format::Yaml,
        2 => Format::Toml,
        3 => Format::Cbor,
        _ => Format::Msgpack,
    }
}
