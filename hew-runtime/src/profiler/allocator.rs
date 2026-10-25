//! Profiling allocator that tracks allocation statistics.
//!
//! Wraps the system allocator to track cheap live/peak gauges at all times.
//! The legacy profiler cumulative counters stay live at all times; observe's
//! hot cumulative counters are routed through the observe tier and only counted
//! when `HEW_OBSERVE` enables them.
//!
//! Successful reallocation retires one old allocation and creates one new
//! allocation event, even when the system allocator reuses the address. Live
//! bytes change by the size difference; peak tracks live requested storage,
//! excluding any temporary storage internal to the system allocator. Failed
//! allocation/reallocation leaves every counter unchanged.

use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicU64, Ordering};

// ── Counters ────────────────────────────────────────────────────────────

/// Approximate bytes currently live.
static BYTES_LIVE: AtomicU64 = AtomicU64::new(0);

/// Peak concurrent bytes live (high-water mark).
static PEAK_BYTES_LIVE: AtomicU64 = AtomicU64::new(0);

// Fixed storage survives thread exit and never allocates on the allocator path.
// Workers use the existing observe shard identity; non-worker threads share
// shard 0 safely through atomic updates.
const SHARD_COUNT: usize = crate::actor::HEW_MAX_WORKERS + 1;

#[repr(align(128))]
struct CumulativeCounters {
    alloc_count: AtomicU64,
    dealloc_count: AtomicU64,
    bytes_allocated: AtomicU64,
    bytes_freed: AtomicU64,
}

impl CumulativeCounters {
    const fn new() -> Self {
        Self {
            alloc_count: AtomicU64::new(0),
            dealloc_count: AtomicU64::new(0),
            bytes_allocated: AtomicU64::new(0),
            bytes_freed: AtomicU64::new(0),
        }
    }
}

static CUMULATIVE: [CumulativeCounters; SHARD_COUNT] =
    [const { CumulativeCounters::new() }; SHARD_COUNT];
fn cumulative_counters() -> &'static CumulativeCounters {
    &CUMULATIVE[crate::observe::current_shard()]
}

fn update_peak(live: u64) {
    let mut peak = PEAK_BYTES_LIVE.load(Ordering::Relaxed);
    while live > peak {
        match PEAK_BYTES_LIVE.compare_exchange_weak(
            peak,
            live,
            Ordering::Relaxed,
            Ordering::Relaxed,
        ) {
            Ok(_) => break,
            Err(actual) => peak = actual,
        }
    }
}

fn record_allocation(size: u64) {
    let counters = cumulative_counters();
    counters.alloc_count.fetch_add(1, Ordering::Relaxed);
    counters.bytes_allocated.fetch_add(size, Ordering::Relaxed);
    let live = BYTES_LIVE
        .fetch_add(size, Ordering::Relaxed)
        .saturating_add(size);
    crate::observe::record_heap_alloc(size);
    update_peak(live);
}

// ── Allocator wrapper ───────────────────────────────────────────────────

/// A thin allocator wrapper that delegates to [`System`] while counting
/// allocation events and tracking live requested bytes.
#[derive(Debug)]
pub struct ProfilingAllocator;

// SAFETY: Every allocation operation delegates to System with the caller's
// original contract. Bookkeeping uses only static atomics and non-allocating
// worker-local identity; failure leaves the original allocation and totals intact.
unsafe impl GlobalAlloc for ProfilingAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        // SAFETY: the caller supplies the valid allocation layout.
        let ptr = unsafe { System.alloc(layout) };
        if !ptr.is_null() {
            record_allocation(layout.size() as u64);
        }
        ptr
    }

    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        // SAFETY: the caller supplies the valid allocation layout; System
        // supplies the zeroed storage directly rather than a separate memset.
        let ptr = unsafe { System.alloc_zeroed(layout) };
        if !ptr.is_null() {
            record_allocation(layout.size() as u64);
        }
        ptr
    }

    unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
        // SAFETY: the caller supplies a live allocation with this layout and
        // a non-zero new size satisfying GlobalAlloc::realloc's contract.
        let replacement = unsafe { System.realloc(ptr, layout, new_size) };
        if !replacement.is_null() {
            let old_size = layout.size() as u64;
            let new_size = new_size as u64;
            let counters = cumulative_counters();
            counters.alloc_count.fetch_add(1, Ordering::Relaxed);
            counters.dealloc_count.fetch_add(1, Ordering::Relaxed);
            counters
                .bytes_allocated
                .fetch_add(new_size, Ordering::Relaxed);
            counters.bytes_freed.fetch_add(old_size, Ordering::Relaxed);
            if new_size > old_size {
                let growth = new_size - old_size;
                let live = BYTES_LIVE
                    .fetch_add(growth, Ordering::Relaxed)
                    .saturating_add(growth);
                update_peak(live);
            } else if new_size < old_size {
                BYTES_LIVE.fetch_sub(old_size - new_size, Ordering::Relaxed);
            }
            crate::observe::record_heap_alloc(new_size);
            crate::observe::record_heap_free(old_size);
        }
        replacement
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        let size = layout.size() as u64;
        let counters = cumulative_counters();
        counters.dealloc_count.fetch_add(1, Ordering::Relaxed);
        counters.bytes_freed.fetch_add(size, Ordering::Relaxed);
        BYTES_LIVE.fetch_sub(size, Ordering::Relaxed);
        crate::observe::record_heap_free(size);
        // SAFETY: the caller supplies a live allocation with this exact layout.
        unsafe { System.dealloc(ptr, layout) };
    }
}

// ── Query API (used by profiler server) ─────────────────────────────────

/// Current snapshot of allocator statistics.
#[derive(Debug, Clone, Copy)]
pub struct AllocStats {
    /// Successful allocation events, including replacement blocks from reallocation.
    pub alloc_count: u64,
    /// Retired allocation events, including old blocks from reallocation.
    pub dealloc_count: u64,
    /// Cumulative bytes allocated.
    pub bytes_allocated: u64,
    /// Cumulative bytes freed.
    pub bytes_freed: u64,
    /// Approximate bytes currently live (allocated - freed).
    pub bytes_live: u64,
    /// Peak bytes live (high-water mark).
    pub peak_bytes_live: u64,
}

/// Capture a snapshot of allocator statistics.
pub fn snapshot() -> AllocStats {
    AllocStats {
        alloc_count: hew_memory_alloc_count(),
        dealloc_count: hew_memory_dealloc_count(),
        bytes_allocated: hew_memory_bytes_allocated(),
        bytes_freed: hew_memory_bytes_freed(),
        bytes_live: BYTES_LIVE.load(Ordering::Relaxed),
        peak_bytes_live: PEAK_BYTES_LIVE.load(Ordering::Relaxed),
    }
}

// ── C ABI ───────────────────────────────────────────────────────────────

/// Return successful allocation events, including reallocation, since startup.
#[no_mangle]
pub extern "C" fn hew_memory_alloc_count() -> u64 {
    CUMULATIVE.iter().fold(0_u64, |total, counters| {
        total.wrapping_add(counters.alloc_count.load(Ordering::Relaxed))
    })
}

/// Return retired allocation events, including reallocation, since startup.
#[no_mangle]
pub extern "C" fn hew_memory_dealloc_count() -> u64 {
    CUMULATIVE.iter().fold(0_u64, |total, counters| {
        total.wrapping_add(counters.dealloc_count.load(Ordering::Relaxed))
    })
}

/// Return the approximate number of bytes currently live.
#[no_mangle]
pub extern "C" fn hew_memory_bytes_live() -> u64 {
    BYTES_LIVE.load(Ordering::Relaxed)
}

/// Return the peak number of bytes that were live at any point.
#[no_mangle]
pub extern "C" fn hew_memory_peak_bytes() -> u64 {
    PEAK_BYTES_LIVE.load(Ordering::Relaxed)
}

/// Return the cumulative bytes allocated since startup.
#[no_mangle]
pub extern "C" fn hew_memory_bytes_allocated() -> u64 {
    CUMULATIVE.iter().fold(0_u64, |total, counters| {
        total.wrapping_add(counters.bytes_allocated.load(Ordering::Relaxed))
    })
}

/// Return the cumulative bytes freed since startup.
#[no_mangle]
pub extern "C" fn hew_memory_bytes_freed() -> u64 {
    CUMULATIVE.iter().fold(0_u64, |total, counters| {
        total.wrapping_add(counters.bytes_freed.load(Ordering::Relaxed))
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn zeroed_growth_shrink_and_same_size_reallocation_keep_accounting_balanced() {
        let _guard = crate::runtime_test_guard();
        let before = snapshot();
        let layout = Layout::from_size_align(64, 16).unwrap();
        let grown_size = 64 * 1024;
        // SAFETY: all layouts/sizes are valid; each successful reallocation
        // transfers the original owner, and its final layout frees it once.
        unsafe {
            let ptr = ProfilingAllocator.alloc_zeroed(layout);
            assert!(!ptr.is_null());
            assert_eq!(ptr as usize % layout.align(), 0);
            assert!(std::slice::from_raw_parts(ptr, layout.size())
                .iter()
                .all(|byte| *byte == 0));
            std::ptr::write_bytes(ptr, 0x5a, layout.size());
            let grown = ProfilingAllocator.realloc(ptr, layout, grown_size);
            assert!(!grown.is_null());
            assert_eq!(grown as usize % layout.align(), 0);
            assert!(std::slice::from_raw_parts(grown, layout.size())
                .iter()
                .all(|byte| *byte == 0x5a));
            let peak = snapshot();
            assert_eq!(peak.bytes_live, before.bytes_live + grown_size as u64);
            assert_eq!(
                peak.peak_bytes_live,
                before
                    .peak_bytes_live
                    .max(before.bytes_live + grown_size as u64)
            );
            let grown_layout = Layout::from_size_align(grown_size, layout.align()).unwrap();
            let smaller = ProfilingAllocator.realloc(grown, grown_layout, 16);
            assert!(!smaller.is_null());
            let small_layout = Layout::from_size_align(16, layout.align()).unwrap();
            let same = ProfilingAllocator.realloc(smaller, small_layout, 16);
            assert!(!same.is_null());
            assert!(std::slice::from_raw_parts(same, 16)
                .iter()
                .all(|byte| *byte == 0x5a));
            assert_eq!(snapshot().bytes_live, before.bytes_live + 16);
            ProfilingAllocator.dealloc(same, small_layout);
            let after = snapshot();
            assert_eq!(after.alloc_count - before.alloc_count, 4);
            assert_eq!(after.dealloc_count - before.dealloc_count, 4);
            assert_eq!(
                after.bytes_allocated - before.bytes_allocated,
                grown_size as u64 + 96
            );
            assert_eq!(
                after.bytes_freed - before.bytes_freed,
                grown_size as u64 + 96
            );
            assert_eq!(after.bytes_live, before.bytes_live);
            assert_eq!(after.peak_bytes_live, peak.peak_bytes_live);
        }
    }

    #[test]
    fn counts_survive_thread_exit_and_cross_thread_deallocation() {
        let _guard = crate::runtime_test_guard();
        let before = snapshot();
        let layout = Layout::from_size_align(64, 8).unwrap();
        let mut threads = Vec::new();
        for _ in 0..8 {
            // SAFETY: layout is valid; every successful allocation is freed
            // exactly once on another thread with the same layout.
            let ptr = unsafe { ProfilingAllocator.alloc(layout) };
            assert!(!ptr.is_null());
            let address = ptr as usize;
            threads.push(std::thread::spawn(move || {
                // SAFETY: the allocating thread transferred ownership of ptr.
                unsafe { ProfilingAllocator.dealloc(address as *mut u8, layout) };
                (0..32)
                    .map(|_| {
                        // SAFETY: layout is valid; ownership transfers back to
                        // the joining thread for deallocation.
                        let ptr = unsafe { ProfilingAllocator.alloc(layout) };
                        assert!(!ptr.is_null());
                        ptr as usize
                    })
                    .collect::<Vec<_>>()
            }));
        }
        let allocations = threads
            .into_iter()
            .flat_map(|thread| thread.join().unwrap())
            .collect::<Vec<_>>();
        let retained = snapshot();
        assert_eq!(retained.alloc_count - before.alloc_count, 8 * 33);
        assert_eq!(retained.dealloc_count - before.dealloc_count, 8);
        assert_eq!(retained.bytes_live - before.bytes_live, 8 * 32 * 64);
        assert!(retained.peak_bytes_live >= retained.bytes_live);
        for address in allocations {
            // SAFETY: each address is a live allocation transferred from a
            // joined thread; layout matches the original allocation.
            unsafe { ProfilingAllocator.dealloc(address as *mut u8, layout) };
        }
        let after = snapshot();
        assert_eq!(after.alloc_count - before.alloc_count, 8 * 33);
        assert_eq!(after.dealloc_count - before.dealloc_count, 8 * 33);
        assert_eq!(after.bytes_allocated - before.bytes_allocated, 8 * 33 * 64);
        assert_eq!(after.bytes_freed - before.bytes_freed, 8 * 33 * 64);
        assert_eq!(after.bytes_live, before.bytes_live);
        assert!(after.peak_bytes_live >= retained.peak_bytes_live);
    }
}
