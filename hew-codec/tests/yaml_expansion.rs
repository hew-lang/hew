//! A failed alias walk must stop parsing before a partial tree is allocated.

use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicUsize, Ordering};

use hew_codec::{yaml_value, Format, Source};

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

#[test]
fn recursive_aliases_are_rejected_with_bounded_heap() {
    // Both fuzz regressions replay a broad mapping through a self-reference.
    // Rejecting only after building that tree exceeds the wire fuzz budget.
    for input in [
        include_str!("fixtures/yaml-recursive-collection-keys.yaml"),
        include_str!("fixtures/yaml-recursive-scalar-keys.yaml"),
    ] {
        for wire in [true, false] {
            let base = LIVE.load(Ordering::SeqCst);
            PEAK.store(base, Ordering::SeqCst);
            let refused = if wire {
                Source::new(Format::Yaml, input.as_bytes()).is_err()
            } else {
                yaml_value::parse(input).is_err()
            };
            let peak = PEAK.load(Ordering::SeqCst).saturating_sub(base);
            assert!(refused, "recursive aliases cannot form a finite tree");
            let limit = input.len() * 1024 + (1 << 20);
            assert!(
                peak <= limit,
                "wire={wire}: {peak} live heap bytes for {} input bytes (limit {limit})",
                input.len()
            );
        }
    }
}
