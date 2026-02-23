//! Profiling allocator that tracks allocation statistics.
//!
//! Wraps the system allocator to count allocations, deallocations, and
//! bytes in flight. The counters are always active (one atomic fetch-add
//! per alloc/dealloc) but the HTTP dashboard only starts when `HEW_PPROF`
//! is set.

use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicU64, Ordering};

// ── Counters ────────────────────────────────────────────────────────────

/// Total number of allocation calls since startup.
static ALLOC_COUNT: AtomicU64 = AtomicU64::new(0);

/// Total number of deallocation calls since startup.
static DEALLOC_COUNT: AtomicU64 = AtomicU64::new(0);

/// Cumulative bytes allocated (monotonically increasing).
static BYTES_ALLOCATED: AtomicU64 = AtomicU64::new(0);

/// Cumulative bytes freed (monotonically increasing).
static BYTES_FREED: AtomicU64 = AtomicU64::new(0);

/// Peak concurrent bytes live (high-water mark).
static PEAK_BYTES_LIVE: AtomicU64 = AtomicU64::new(0);

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
            ALLOC_COUNT.fetch_add(1, Ordering::Relaxed);
            let prev_allocated = BYTES_ALLOCATED.fetch_add(size, Ordering::Relaxed);
            let freed = BYTES_FREED.load(Ordering::Relaxed);
            let live = (prev_allocated + size).saturating_sub(freed);
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
        DEALLOC_COUNT.fetch_add(1, Ordering::Relaxed);
        BYTES_FREED.fetch_add(layout.size() as u64, Ordering::Relaxed);
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
    let bytes_allocated = BYTES_ALLOCATED.load(Ordering::Relaxed);
    let bytes_freed = BYTES_FREED.load(Ordering::Relaxed);
    AllocStats {
        alloc_count: ALLOC_COUNT.load(Ordering::Relaxed),
        dealloc_count: DEALLOC_COUNT.load(Ordering::Relaxed),
        bytes_allocated,
        bytes_freed,
        bytes_live: bytes_allocated.saturating_sub(bytes_freed),
        peak_bytes_live: PEAK_BYTES_LIVE.load(Ordering::Relaxed),
    }
}

// ── C ABI ───────────────────────────────────────────────────────────────

/// Return the total number of allocations since startup.
#[no_mangle]
pub extern "C" fn hew_memory_alloc_count() -> u64 {
    ALLOC_COUNT.load(Ordering::Relaxed)
}

/// Return the total number of deallocations since startup.
#[no_mangle]
pub extern "C" fn hew_memory_dealloc_count() -> u64 {
    DEALLOC_COUNT.load(Ordering::Relaxed)
}

/// Return the approximate number of bytes currently live.
#[no_mangle]
pub extern "C" fn hew_memory_bytes_live() -> u64 {
    let allocated = BYTES_ALLOCATED.load(Ordering::Relaxed);
    let freed = BYTES_FREED.load(Ordering::Relaxed);
    allocated.saturating_sub(freed)
}

/// Return the peak number of bytes that were live at any point.
#[no_mangle]
pub extern "C" fn hew_memory_peak_bytes() -> u64 {
    PEAK_BYTES_LIVE.load(Ordering::Relaxed)
}

/// Return the cumulative bytes allocated since startup.
#[no_mangle]
pub extern "C" fn hew_memory_bytes_allocated() -> u64 {
    BYTES_ALLOCATED.load(Ordering::Relaxed)
}

/// Return the cumulative bytes freed since startup.
#[no_mangle]
pub extern "C" fn hew_memory_bytes_freed() -> u64 {
    BYTES_FREED.load(Ordering::Relaxed)
}
