//! Profiling allocator that tracks allocation statistics.
//!
//! Wraps the system allocator to keep cumulative allocation counters at all
//! times. Live bytes are derived from those counters (allocated minus freed)
//! when read, and peak bytes is a sampled high-water mark of live bytes.
//! Observe's hot cumulative counters are routed through the observe tier and
//! only counted when `HEW_OBSERVE` enables them.
//!
//! The allocator path takes no shared atomic read-modify-write. Every
//! scheduler worker owns one counter shard and updates it with a plain load
//! and store, which no other thread writes; threads that are not workers share
//! shard 0 through atomic adds. Readers sum the shards in use.
//!
//! Peak is sampled whenever a shard's cumulative allocated bytes cross a
//! [`PEAK_SAMPLE_BYTES`] boundary and on every read, so it never exceeds the
//! true high-water mark and misses less than `PEAK_SAMPLE_BYTES` per shard of
//! a short-lived excursion.
//!
//! Successful reallocation retires one old allocation and creates one new
//! allocation event, even when the system allocator reuses the address. Live
//! bytes change by the size difference; peak tracks live requested storage,
//! excluding any temporary storage internal to the system allocator. Failed
//! allocation/reallocation leaves every counter unchanged.

use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicU64, Ordering};

// ── Counters ────────────────────────────────────────────────────────────

/// Sampled peak of concurrent bytes live (high-water mark).
static PEAK_BYTES_LIVE: AtomicU64 = AtomicU64::new(0);

/// Allocated bytes per shard between peak samples.
const PEAK_SAMPLE_BYTES: u64 = 64 * 1024;

// Fixed storage survives thread exit and never allocates on the allocator path.
// Workers use the observe shard identity; non-worker threads share shard 0.
const SHARD_COUNT: usize = crate::actor::HEW_MAX_WORKERS + 1;
const SHARED_SHARD: usize = 0;

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

/// The calling thread's shard and whether this thread is its only writer.
struct ShardWriter {
    counters: &'static CumulativeCounters,
    owned: bool,
}

impl ShardWriter {
    #[inline]
    fn current() -> Self {
        let shard = crate::observe::current_shard();
        Self {
            counters: &CUMULATIVE[shard],
            owned: shard != SHARED_SHARD,
        }
    }

    /// Add `value` and return the previous total.
    #[inline]
    fn add(&self, counter: impl Fn(&CumulativeCounters) -> &AtomicU64, value: u64) -> u64 {
        let counter = counter(self.counters);
        if self.owned {
            // Only this worker writes its shard, so a plain load and store
            // cannot lose an update; readers see a whole value either way.
            let previous = counter.load(Ordering::Relaxed);
            counter.store(previous.wrapping_add(value), Ordering::Relaxed);
            previous
        } else {
            counter.fetch_add(value, Ordering::Relaxed)
        }
    }

    #[inline]
    fn record_allocation(&self, size: u64) {
        self.add(|c| &c.alloc_count, 1);
        let previous = self.add(|c| &c.bytes_allocated, size);
        if previous / PEAK_SAMPLE_BYTES != previous.wrapping_add(size) / PEAK_SAMPLE_BYTES {
            sample_peak();
        }
    }

    #[inline]
    fn record_deallocation(&self, size: u64) {
        self.add(|c| &c.dealloc_count, 1);
        self.add(|c| &c.bytes_freed, size);
    }
}

fn shard_sum(counter: impl Fn(&CumulativeCounters) -> &AtomicU64) -> u64 {
    CUMULATIVE[..crate::observe::shards_in_use()]
        .iter()
        .fold(0_u64, |total, counters| {
            total.wrapping_add(counter(counters).load(Ordering::Relaxed))
        })
}

/// Live bytes from the cumulative totals. Allocated bytes are read before
/// freed bytes, and both only grow, so the result never exceeds the bytes live
/// when the allocated sum was complete. Churn between the two reads can only
/// lower it, saturating at zero.
fn bytes_live() -> u64 {
    let allocated = shard_sum(|c| &c.bytes_allocated);
    allocated.saturating_sub(shard_sum(|c| &c.bytes_freed))
}

/// Fold the current live bytes into the peak and return them.
fn sample_peak() -> u64 {
    let live = bytes_live();
    PEAK_BYTES_LIVE.fetch_max(live, Ordering::Relaxed);
    live
}

// ── Allocator wrapper ───────────────────────────────────────────────────

/// A thin allocator wrapper that delegates to [`System`] while counting
/// allocation events and requested bytes.
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
            let size = layout.size() as u64;
            ShardWriter::current().record_allocation(size);
            crate::observe::record_heap_alloc(size);
        }
        ptr
    }

    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        // SAFETY: the caller supplies the valid allocation layout; System
        // supplies the zeroed storage directly rather than a separate memset.
        let ptr = unsafe { System.alloc_zeroed(layout) };
        if !ptr.is_null() {
            let size = layout.size() as u64;
            ShardWriter::current().record_allocation(size);
            crate::observe::record_heap_alloc(size);
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
            let shard = ShardWriter::current();
            // Retire the old block first so live bytes never count both.
            shard.record_deallocation(old_size);
            shard.record_allocation(new_size);
            crate::observe::record_heap_alloc(new_size);
            crate::observe::record_heap_free(old_size);
        }
        replacement
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        let size = layout.size() as u64;
        ShardWriter::current().record_deallocation(size);
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
    /// Sampled peak bytes live (high-water mark).
    pub peak_bytes_live: u64,
}

/// Capture a snapshot of allocator statistics.
pub fn snapshot() -> AllocStats {
    let bytes_live = sample_peak();
    AllocStats {
        alloc_count: hew_memory_alloc_count(),
        dealloc_count: hew_memory_dealloc_count(),
        bytes_allocated: hew_memory_bytes_allocated(),
        bytes_freed: hew_memory_bytes_freed(),
        bytes_live,
        peak_bytes_live: PEAK_BYTES_LIVE.load(Ordering::Relaxed),
    }
}

// ── C ABI ───────────────────────────────────────────────────────────────

/// Return successful allocation events, including reallocation, since startup.
#[no_mangle]
pub extern "C" fn hew_memory_alloc_count() -> u64 {
    shard_sum(|c| &c.alloc_count)
}

/// Return retired allocation events, including reallocation, since startup.
#[no_mangle]
pub extern "C" fn hew_memory_dealloc_count() -> u64 {
    shard_sum(|c| &c.dealloc_count)
}

/// Return the approximate number of bytes currently live.
#[no_mangle]
pub extern "C" fn hew_memory_bytes_live() -> u64 {
    sample_peak()
}

/// Return the sampled peak number of bytes live at any point.
#[no_mangle]
pub extern "C" fn hew_memory_peak_bytes() -> u64 {
    sample_peak();
    PEAK_BYTES_LIVE.load(Ordering::Relaxed)
}

/// Return the cumulative bytes allocated since startup.
#[no_mangle]
pub extern "C" fn hew_memory_bytes_allocated() -> u64 {
    shard_sum(|c| &c.bytes_allocated)
}

/// Return the cumulative bytes freed since startup.
#[no_mangle]
pub extern "C" fn hew_memory_bytes_freed() -> u64 {
    shard_sum(|c| &c.bytes_freed)
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
    fn worker_shards_balance_cross_shard_frees_and_sample_bursts_into_peak() {
        let _guard = crate::runtime_test_guard();
        let before = snapshot();
        let layout = Layout::from_size_align(256, 16).unwrap();
        // Four peak sample intervals, so the burst always crosses a boundary.
        let burst = Layout::from_size_align(256 * 1024, 16).unwrap();
        assert!(burst.size() as u64 >= 4 * PEAK_SAMPLE_BYTES);
        let workers = 4;
        let pinned = |worker: usize| {
            move || {
                crate::observe::set_current_worker_shard(worker);
                (0..100)
                    .map(|_| {
                        // SAFETY: layout is valid; ownership transfers to the
                        // joining thread, which hands it to another worker.
                        let ptr = unsafe { ProfilingAllocator.alloc(layout) };
                        assert!(!ptr.is_null());
                        ptr as usize
                    })
                    .collect::<Vec<_>>()
            }
        };
        let blocks = (0..workers)
            .map(|worker| std::thread::spawn(pinned(worker)))
            .collect::<Vec<_>>()
            .into_iter()
            .map(|thread| thread.join().unwrap())
            .collect::<Vec<_>>();
        assert_eq!(snapshot().bytes_live, before.bytes_live + 4 * 100 * 256);
        let frees = blocks
            .into_iter()
            .enumerate()
            .map(|(owner, addresses)| {
                std::thread::spawn(move || {
                    // Free on the next worker's shard, and allocate a burst
                    // that is gone again before anything reads the counters.
                    crate::observe::set_current_worker_shard((owner + 1) % workers);
                    for address in addresses {
                        // SAFETY: each address is a live block of this layout
                        // transferred from the thread that allocated it.
                        unsafe { ProfilingAllocator.dealloc(address as *mut u8, layout) };
                    }
                    // SAFETY: burst is a valid layout, freed once below.
                    let ptr = unsafe { ProfilingAllocator.alloc(burst) };
                    assert!(!ptr.is_null());
                    // SAFETY: ptr is the live burst block allocated above.
                    unsafe { ProfilingAllocator.dealloc(ptr, burst) };
                })
            })
            .collect::<Vec<_>>();
        for thread in frees {
            thread.join().unwrap();
        }
        let after = snapshot();
        let blocks = (workers * 100) as u64;
        assert_eq!(
            after.alloc_count - before.alloc_count,
            blocks + workers as u64
        );
        assert_eq!(
            after.dealloc_count - before.dealloc_count,
            blocks + workers as u64
        );
        assert_eq!(after.bytes_live, before.bytes_live);
        assert!(after.peak_bytes_live >= before.bytes_live + burst.size() as u64);
    }

    #[test]
    fn concurrent_reads_never_overstate_peak_under_churn() {
        let _guard = crate::runtime_test_guard();
        let before = snapshot();
        let workers = 8;
        let block = Layout::from_size_align(64 * 1024, 16).unwrap();
        let stop = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
        let readers = (0..8)
            .map(|_| {
                let stop = stop.clone();
                std::thread::spawn(move || {
                    while !stop.load(Ordering::Relaxed) {
                        std::hint::black_box(hew_memory_peak_bytes());
                    }
                })
            })
            .collect::<Vec<_>>();
        let churners = (0..workers)
            .map(|worker| {
                std::thread::spawn(move || {
                    crate::observe::set_current_worker_shard(worker);
                    for _ in 0..50_000 {
                        // SAFETY: block is a valid layout, freed at once.
                        unsafe {
                            let ptr = ProfilingAllocator.alloc(block);
                            assert!(!ptr.is_null());
                            ProfilingAllocator.dealloc(ptr, block);
                        }
                    }
                })
            })
            .collect::<Vec<_>>();
        for thread in churners {
            thread.join().unwrap();
        }
        stop.store(true, Ordering::Relaxed);
        for thread in readers {
            thread.join().unwrap();
        }
        let bound = before
            .peak_bytes_live
            .max(before.bytes_live + (workers * block.size()) as u64);
        let after = snapshot();
        assert!(
            after.peak_bytes_live <= bound,
            "peak {} exceeds the true high-water mark {bound}",
            after.peak_bytes_live
        );
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
