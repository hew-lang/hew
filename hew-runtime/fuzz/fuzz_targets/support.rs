//! Allocation accounting shared by the runtime fuzz targets.
//!
//! Without a sanitizer libFuzzer's `-malloc_limit_mb` is inert, and a
//! reservation for a declared length that is never touched does not raise
//! RSS. Counting live bytes catches a decoder that sizes a buffer from a
//! length the peer declared rather than from the bytes it sent.

use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicUsize, Ordering};

pub struct Counting;

static LIVE: AtomicUsize = AtomicUsize::new(0);
static PEAK: AtomicUsize = AtomicUsize::new(0);

fn grew(size: usize) {
    let live = LIVE.fetch_add(size, Ordering::Relaxed) + size;
    PEAK.fetch_max(live, Ordering::Relaxed);
}

// SAFETY: every method forwards to `System` and only adjusts counters.
unsafe impl GlobalAlloc for Counting {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        // SAFETY: forwarded unchanged.
        let ptr = unsafe { System.alloc(layout) };
        if !ptr.is_null() {
            grew(layout.size());
        }
        ptr
    }

    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        // SAFETY: forwarded unchanged.
        let ptr = unsafe { System.alloc_zeroed(layout) };
        if !ptr.is_null() {
            grew(layout.size());
        }
        ptr
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        LIVE.fetch_sub(layout.size(), Ordering::Relaxed);
        // SAFETY: forwarded unchanged.
        unsafe { System.dealloc(ptr, layout) };
    }

    unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
        // SAFETY: forwarded unchanged.
        let new = unsafe { System.realloc(ptr, layout, new_size) };
        if !new.is_null() {
            LIVE.fetch_sub(layout.size(), Ordering::Relaxed);
            grew(new_size);
        }
        new
    }
}

/// Live bytes a decoder may hold per input byte, plus a fixed allowance for
/// tables and diagnostics. The TOML parser holds about 600 bytes per byte of
/// malformed input; a reservation sized from a declared length holds tens of
/// thousands.
const BYTES_PER_INPUT_BYTE: usize = 1024;
const FIXED_ALLOWANCE: usize = 1 << 20;

/// Run `work` and fail if it held more live heap than `input_len` justifies.
pub fn bounded<R>(what: &str, input_len: usize, work: impl FnOnce() -> R) -> R {
    let base = LIVE.load(Ordering::Relaxed);
    PEAK.store(base, Ordering::Relaxed);
    let result = work();
    let held = PEAK.load(Ordering::Relaxed).saturating_sub(base);
    let limit = input_len * BYTES_PER_INPUT_BYTE + FIXED_ALLOWANCE;
    assert!(
        held <= limit,
        "{what} held {held} live heap bytes for a {input_len}-byte input (limit {limit})"
    );
    result
}
