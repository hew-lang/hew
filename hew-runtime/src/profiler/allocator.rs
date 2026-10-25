//! Profiling allocator that tracks allocation statistics.
//!
//! Wraps the system allocator to track cheap live/peak gauges at all times.
//! The legacy profiler cumulative counters stay live at all times; observe's
//! hot cumulative counters are routed through the observe tier and only counted
//! when `HEW_OBSERVE` enables them.

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

// ── Allocator wrapper ───────────────────────────────────────────────────

/// A thin allocator wrapper that delegates to [`System`] while counting
/// allocations and tracking live bytes.
#[derive(Debug)]
pub struct ProfilingAllocator;

// SAFETY: We delegate all allocation to `System` (which is a valid
// `GlobalAlloc` implementation) and only add atomic counter increments.
unsafe impl GlobalAlloc for ProfilingAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        // SAFETY: Caller upholds `GlobalAlloc::alloc` safety contract;
        // we forward directly to `System`.
        let ptr = unsafe { System.alloc(layout) };
        if !ptr.is_null() {
            let size = layout.size() as u64;
            let counters = cumulative_counters();
            counters.alloc_count.fetch_add(1, Ordering::Relaxed);
            counters.bytes_allocated.fetch_add(size, Ordering::Relaxed);
            let live = BYTES_LIVE
                .fetch_add(size, Ordering::Relaxed)
                .saturating_add(size);
            crate::observe::record_heap_alloc(size);
            // Update peak with a CAS loop (only grows).
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
        ptr
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        let size = layout.size() as u64;
        let counters = cumulative_counters();
        counters.dealloc_count.fetch_add(1, Ordering::Relaxed);
        counters.bytes_freed.fetch_add(size, Ordering::Relaxed);
        BYTES_LIVE.fetch_sub(size, Ordering::Relaxed);
        crate::observe::record_heap_free(size);
        // SAFETY: Caller upholds `GlobalAlloc::dealloc` safety contract;
        // we forward directly to `System`.
        unsafe { System.dealloc(ptr, layout) };
    }
}

// ── Query API (used by profiler server) ─────────────────────────────────

/// Current snapshot of allocator statistics.
#[derive(Debug, Clone, Copy)]
pub struct AllocStats {
    /// Total allocation calls.
    pub alloc_count: u64,
    /// Total deallocation calls.
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

/// Return the total number of allocations since startup.
#[no_mangle]
pub extern "C" fn hew_memory_alloc_count() -> u64 {
    CUMULATIVE.iter().fold(0_u64, |total, counters| {
        total.wrapping_add(counters.alloc_count.load(Ordering::Relaxed))
    })
}

/// Return the total number of deallocations since startup.
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
