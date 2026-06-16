//! Per-actor arena bump allocator for the Hew actor runtime.
//!
//! # Lifecycle contract
//!
//! ```text
//! hew_arena_new()
//!   └─► ctx.arena = arena              ← install in canonical context
//!         │
//!         │  [actor dispatch runs]
//!         │    hew_arena_malloc(n)   → bump-allocates from the arena
//!         │    hew_arena_free(ptr)   → **no-op** (bulk-free is cheaper than per-pointer tracking)
//!         │
//!       ctx.arena = null              ← uninstall; alloc/free fail closed
//!         │
//!         ├── hew_arena_reset(arena)  → cursor back to zero, chunks *retained* (fast reuse)
//!         │     └─► ctx.arena = arena … repeat for next dispatch
//!         │
//!         └── hew_arena_free_all(arena) → munmap/VirtualFree every chunk; pointer invalid
//! ```
//!
//! When no execution context is installed, C ABI allocation/free readers set
//! `hew_last_error` and return their sentinel instead of pretending direct
//! `libc` ownership.
//!
//! # Chunk growth
//!
//! Chunks are allocated via `mmap` (Unix) / `VirtualAlloc` (Windows) and grow geometrically
//! (doubling) up to `max_chunk_size`.  The OS guarantees page-aligned base pointers, which
//! satisfies any practical Rust/C alignment requirement (≤ 4096 bytes).
//!
//! # Alignment invariant
//!
//! Every `alloc` call rounds the current cursor up to the requested alignment using the
//! standard power-of-two mask: `(cursor + align - 1) & !(align - 1)`.
//! **`align` must be a power of two** — a `debug_assert!` guards this in debug builds.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use std::os::raw::c_void;
use std::ptr;

// ── Windows virtual memory API ──────────────────────────────────────────────

#[cfg(windows)]
#[link(name = "kernel32")]
unsafe extern "system" {
    fn VirtualAlloc(addr: *mut c_void, size: usize, alloc_type: u32, protect: u32) -> *mut c_void;
    fn VirtualFree(addr: *mut c_void, size: usize, free_type: u32) -> i32;
}

#[cfg(windows)]
const MEM_COMMIT: u32 = 0x1000;
#[cfg(windows)]
const MEM_RESERVE: u32 = 0x2000;
#[cfg(windows)]
const MEM_RELEASE: u32 = 0x8000;
#[cfg(windows)]
const PAGE_READWRITE: u32 = 0x04;

// ── Miri allocator shim ─────────────────────────────────────────────────────
//
// Miri interprets MIR and cannot execute the raw `mmap`/`VirtualAlloc` syscalls,
// so under `cfg(miri)` chunks come from the global allocator instead.  Chunks
// are allocated page-aligned to preserve the same ≤4096-byte base-pointer
// alignment guarantee the OS mappings provide, so arena provenance and aliasing
// behaviour is identical to the production path the interpreter is validating.
#[cfg(miri)]
const MIRI_CHUNK_ALIGN: usize = 4096;

/// Memory chunk allocated via mmap (Unix) or `VirtualAlloc` (Windows).
#[derive(Debug)]
struct ArenaChunk {
    base: *mut u8,
    size: usize,
}

impl ArenaChunk {
    /// Create a new chunk with the specified size.
    fn new(size: usize) -> Option<Self> {
        #[cfg(all(unix, not(miri)))]
        let base = {
            // SAFETY: mmap with valid flags and no file descriptor
            let p = unsafe {
                libc::mmap(
                    ptr::null_mut(),
                    size,
                    libc::PROT_READ | libc::PROT_WRITE,
                    libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
                    -1,
                    0,
                )
            };
            if p == libc::MAP_FAILED {
                return None;
            }
            p.cast::<u8>()
        };

        #[cfg(all(windows, not(miri)))]
        let base = {
            // SAFETY: VirtualAlloc with MEM_COMMIT | MEM_RESERVE for rw pages.
            let p = unsafe {
                VirtualAlloc(
                    ptr::null_mut(),
                    size,
                    MEM_COMMIT | MEM_RESERVE,
                    PAGE_READWRITE,
                )
            };
            if p.is_null() {
                return None;
            }
            p.cast::<u8>()
        };

        // Global-allocator chunk for Miri; the matching `dealloc` lives in
        // `Drop`.  A `size == 0` request returns `None` to match the production
        // `mmap(len=0)`/`VirtualAlloc(0)` contract above — both reject a
        // zero-length mapping (`MAP_FAILED`/null) — and because
        // `std::alloc::alloc` is undefined behaviour on a zero-sized layout.  A
        // layout error (only reachable for a pathologically large `size`)
        // likewise maps to `None`.
        #[cfg(miri)]
        let base = {
            if size == 0 {
                return None;
            }
            let layout = std::alloc::Layout::from_size_align(size, MIRI_CHUNK_ALIGN).ok()?;
            // SAFETY: the guard above rules out `size == 0` and
            // `MIRI_CHUNK_ALIGN` is a non-zero power of two, so `layout` has the
            // non-zero size that `std::alloc::alloc` requires.  The pointer is
            // freed with this exact layout in `ArenaChunk::drop`.
            let p = unsafe { std::alloc::alloc(layout) };
            if p.is_null() {
                return None;
            }
            p
        };

        Some(ArenaChunk { base, size })
    }
}

/// Per-actor arena bump allocator.
#[derive(Debug)]
pub struct ActorArena {
    chunks: Vec<ArenaChunk>,
    current_chunk: usize,
    cursor: usize,
    initial_chunk_size: usize,
    max_chunk_size: usize,
    /// Byte cap for this arena. `0` means unbounded (legacy behaviour).
    ///
    /// When non-zero, `alloc` returns null once total allocated bytes would
    /// exceed this value. `used` is reset to zero by `reset()` so per-dispatch
    /// accounting restarts each cycle. Enforced at allocation time (Hew has no
    /// GC — alloc-time is the natural enforcement point, per `boundary-fail-closed`).
    pub cap: usize,
    /// Bytes allocated since the last `reset()`. Incremented on each successful
    /// alloc when `cap > 0`. Reset to zero by `reset()`. Ignored when `cap == 0`.
    used: usize,
}

impl ActorArena {
    /// Create a new actor arena with default settings.
    #[must_use]
    pub fn new() -> Option<Self> {
        Self::new_with_sizes(64 * 1024, 16 * 1024 * 1024) // 64 KiB initial, 16 MiB max
    }

    /// Create a new actor arena with custom chunk sizes.
    #[must_use]
    pub fn new_with_sizes(initial_chunk_size: usize, max_chunk_size: usize) -> Option<Self> {
        let initial_chunk = ArenaChunk::new(initial_chunk_size)?;

        Some(ActorArena {
            chunks: vec![initial_chunk],
            current_chunk: 0,
            cursor: 0,
            initial_chunk_size,
            max_chunk_size,
            cap: 0,
            used: 0,
        })
    }

    /// Create a new actor arena with a per-dispatch byte cap.
    ///
    /// `cap_bytes` is the maximum number of bytes the arena will serve in a
    /// single dispatch cycle (i.e. between a `reset` call and the next
    /// `reset`). Once the cap would be exceeded `alloc` returns null.
    ///
    /// Passing `cap_bytes = 0` is equivalent to calling `new()` (unbounded).
    #[must_use]
    pub fn new_with_cap(cap_bytes: usize) -> Option<Self> {
        let mut arena = Self::new()?;
        arena.cap = cap_bytes;
        Some(arena)
    }

    /// Allocate memory with the specified size and alignment.
    ///
    /// `align` must be a power of two (asserted in debug builds).
    /// Returns a null pointer if allocation fails (OOM), if `size` is zero,
    /// or if a non-zero cap has been set and this allocation would exceed it.
    pub fn alloc(&mut self, size: usize, align: usize) -> *mut u8 {
        debug_assert!(
            align.is_power_of_two(),
            "align must be a power of two, got {align}"
        );

        if size == 0 {
            return ptr::null_mut();
        }

        // Cap enforcement (0 = unbounded).
        if self.cap > 0 {
            match self.used.checked_add(size) {
                Some(new_used) if new_used <= self.cap => {
                    self.used = new_used;
                }
                _ => {
                    return ptr::null_mut();
                }
            }
        }

        while self.current_chunk < self.chunks.len() {
            let aligned_cursor = (self.cursor + align - 1) & !(align - 1);
            let end_offset = aligned_cursor + size;
            let chunk = &self.chunks[self.current_chunk];
            if end_offset <= chunk.size {
                self.cursor = end_offset;
                // SAFETY: aligned_cursor is within chunk bounds
                return unsafe { chunk.base.add(aligned_cursor) };
            }
            self.current_chunk += 1;
            self.cursor = 0;
        }

        // Need a new chunk
        let next_chunk_size = if self.chunks.is_empty() {
            self.initial_chunk_size
        } else {
            let last_size = self.chunks[self.chunks.len() - 1].size;
            std::cmp::min(last_size * 2, self.max_chunk_size)
        };

        // Ensure chunk is large enough for this allocation
        let chunk_size = std::cmp::max(next_chunk_size, size + align);

        let Some(new_chunk) = ArenaChunk::new(chunk_size) else {
            return ptr::null_mut();
        };

        self.chunks.push(new_chunk);
        self.current_chunk = self.chunks.len() - 1;

        // Allocate from the new chunk starting at offset 0.
        // The OS guarantees page-aligned base pointers (mmap / VirtualAlloc return
        // memory aligned to at least the system page size, typically 4096 bytes).
        // For any power-of-two alignment ≤ page size, rounding 0 up is still 0.
        let aligned_cursor = 0_usize;
        self.cursor = aligned_cursor + size;

        // SAFETY: aligned_cursor is within new chunk bounds
        unsafe { self.chunks[self.current_chunk].base.add(aligned_cursor) }
    }

    /// Reset the arena for reuse after a completed dispatch cycle.
    ///
    /// The cursor returns to zero and `current_chunk` rewinds to the first chunk.
    /// All previously allocated chunks are **retained** so subsequent allocations reuse
    /// the already-mapped virtual memory.  Any pointers into the arena that were live
    /// before this call are invalidated — do not access them afterwards.
    ///
    /// `used` is reset to zero so that cap accounting restarts for the next cycle.
    pub fn reset(&mut self) {
        self.current_chunk = 0;
        self.cursor = 0;
        self.used = 0;
    }

    /// Free all chunks and destroy the arena.
    ///
    /// Every mapped chunk is returned to the OS via `munmap` / `VirtualFree`.  The
    /// `ActorArena` value is consumed; any raw pointer to this arena (e.g. one previously
    /// passed to `set_current_arena`) becomes invalid after this call.
    pub fn free_all(self) {
        // chunks will be dropped, calling ArenaChunk::drop which calls free()
    }
}

impl Drop for ArenaChunk {
    fn drop(&mut self) {
        #[cfg(all(unix, not(miri)))]
        {
            // SAFETY: base and size are valid from successful mmap
            unsafe {
                libc::munmap(self.base.cast::<c_void>(), self.size);
            }
        }
        #[cfg(all(windows, not(miri)))]
        {
            // SAFETY: base was allocated by VirtualAlloc with MEM_COMMIT | MEM_RESERVE.
            unsafe {
                VirtualFree(self.base.cast(), 0, MEM_RELEASE);
            }
        }
        #[cfg(miri)]
        {
            // SAFETY: `base` came from `std::alloc::alloc` in `ArenaChunk::new`
            // with this exact layout; `size` is fixed for the chunk's lifetime,
            // so reconstructing the layout reproduces the allocation's layout.
            let layout = std::alloc::Layout::from_size_align(self.size, MIRI_CHUNK_ALIGN)
                .expect("layout validated in ArenaChunk::new");
            unsafe {
                std::alloc::dealloc(self.base, layout);
            }
        }
    }
}

impl Default for ActorArena {
    fn default() -> Self {
        Self::new().expect("Failed to create default arena")
    }
}

/// Set the current context's arena lane and return the previous one.
pub fn set_current_arena(arena: *mut ActorArena) -> *mut ActorArena {
    let ctx = crate::execution_context::require_current_context();
    if ctx.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: a non-null canonical context points to a live context slot owned
    // by the current dispatch/scope boundary.
    unsafe {
        let previous = (*ctx).arena;
        (*ctx).arena = arena;
        previous
    }
}

/// Get the current context's arena lane.
fn get_current_arena() -> *mut ActorArena {
    let ctx = crate::execution_context::require_current_context();
    if ctx.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: a non-null canonical context points to a live context slot owned
    // by the current dispatch/scope boundary.
    unsafe { (*ctx).arena }
}

// C ABI functions

/// Allocate memory from the current execution context's arena.
///
/// **Requires an installed execution context with an active arena.** Returns
/// null and sets `hew_last_error` to `"execution context not installed"` if
/// no context is installed. Returns null (without setting an error) if a
/// context is installed but its arena lane is null.
///
/// Use `libc::malloc` for allocation outside dispatch; do not call this
/// function from code that runs without a scheduler-installed execution
/// context.
///
/// When an arena with a non-zero cap is active and this allocation would
/// exceed that cap, returns null **and** triggers a `HeapExceeded` actor
/// crash via the longjmp/supervisor seam. The actor's `error_code` is set
/// to `HEW_TRAP_HEAP_EXCEEDED` (200). Teardown (timers, links, monitors,
/// arena free) runs via the normal crash path.
///
/// On WASM the longjmp seam is absent; the null is returned to the caller
/// and the WASM `catch_unwind` path handles any subsequent trap.
///
/// # Safety
///
/// The returned pointer must be freed via `hew_arena_reset` or
/// `hew_arena_free_all` — never via `free()`. There is no per-pointer
/// ownership outside the arena; mixing arena pointers with `free()` is
/// undefined behaviour.
#[no_mangle]
pub unsafe extern "C" fn hew_arena_malloc(size: usize) -> *mut c_void {
    let arena_ptr = get_current_arena();
    if arena_ptr.is_null() {
        ptr::null_mut()
    } else {
        // SAFETY: arena_ptr is valid (set by hew_arena_set_current)
        let arena = unsafe { &mut *arena_ptr };
        let ptr = arena
            .alloc(size, std::mem::align_of::<*mut c_void>())
            .cast::<c_void>();

        // If the allocation failed and the arena has a cap set, this is a
        // HeapExceeded condition — route through the crash seam so the
        // supervisor sees a named exit reason instead of a generic SIGSEGV.
        #[cfg(not(target_arch = "wasm32"))]
        if ptr.is_null() && arena.cap > 0 {
            // SAFETY: must be called from an actor dispatch context on a
            // worker thread (hew_arena_set_current is only called from the
            // scheduler's activate_actor path). The longjmp unwinds to the
            // sigsetjmp frame in the scheduler without returning here.
            unsafe {
                crate::signal::try_direct_longjmp_with_code(
                    crate::supervisor::HEW_TRAP_HEAP_EXCEEDED,
                );
            }
        }

        ptr
    }
}

/// Free memory — intentional no-op.
///
/// Arena memory is reclaimed in bulk via `hew_arena_reset` or
/// `hew_arena_free_all`; per-pointer tracking is not supported. This
/// function exists solely to satisfy C callers that pair every `malloc`
/// with a `free`.
///
/// When no execution context is installed, `hew_last_error` is set (via
/// `get_current_arena` → `require_current_context`) and this function
/// returns without touching the pointer. The pointer must **not** be
/// passed to `libc::free` — it was allocated from an arena, not from the
/// system heap.
///
/// # Safety
///
/// `ptr` must be a valid pointer returned by `hew_arena_malloc`, or null.
/// Do **not** pass pointers allocated with `libc::malloc` or
/// `libc::calloc` to this function.
#[no_mangle]
pub unsafe extern "C" fn hew_arena_free(_ptr: *mut c_void) {
    let _arena_ptr = get_current_arena();
    // If arena is active, this is a no-op (arena memory will be reset/freed in bulk)
}

/// Create a new arena.
#[no_mangle]
pub extern "C" fn hew_arena_new() -> *mut ActorArena {
    match ActorArena::new() {
        Some(arena) => Box::into_raw(Box::new(arena)),
        None => ptr::null_mut(),
    }
}

/// Create a new arena with a per-dispatch byte cap.
///
/// `cap_bytes = 0` is unbounded (equivalent to `hew_arena_new`).
/// Returns null if memory allocation for the initial chunk fails.
#[no_mangle]
pub extern "C" fn hew_arena_new_with_cap(cap_bytes: usize) -> *mut ActorArena {
    match ActorArena::new_with_cap(cap_bytes) {
        Some(arena) => Box::into_raw(Box::new(arena)),
        None => ptr::null_mut(),
    }
}

/// Reset an arena for reuse.
///
/// # Safety
///
/// arena must be a valid pointer returned by `hew_arena_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_arena_reset(arena: *mut ActorArena) {
    if !arena.is_null() {
        // SAFETY: caller guarantees arena is valid
        let arena = unsafe { &mut *arena };
        arena.reset();
        crate::observe::record_arena_reset();
    }
}

// Address of the arena most recently freed by `hew_arena_free_all` on this thread.
// Thread-local so parallel tests on other threads cannot interfere with the
// snapshot/assert window.  Storing usize avoids holding a dangling typed pointer.
#[cfg(test)]
thread_local! {
    pub static LAST_FREED_ARENA_ADDR: std::cell::Cell<usize> =
        const { std::cell::Cell::new(0) };
}

/// Free all memory and destroy the arena.
///
/// # Safety
///
/// arena must be a valid pointer returned by `hew_arena_new`.
/// After this call, arena is no longer valid.
#[no_mangle]
pub unsafe extern "C" fn hew_arena_free_all(arena: *mut ActorArena) {
    if !arena.is_null() {
        #[cfg(test)]
        LAST_FREED_ARENA_ADDR.with(|c| c.set(arena as usize));
        // SAFETY: caller guarantees arena is valid
        let arena = unsafe { Box::from_raw(arena) };
        arena.free_all();
    }
}

/// Set the current context's arena lane.
///
/// # Safety
///
/// arena must be a valid pointer returned by `hew_arena_new` or null.
#[no_mangle]
pub unsafe extern "C" fn hew_arena_set_current(arena: *mut ActorArena) {
    set_current_arena(arena);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::execution_context::{HewExecutionContext, TestExecutionContext};

    #[test]
    fn arena_alloc_basic() {
        let mut arena = ActorArena::new().expect("Failed to create arena");

        let ptr1 = arena.alloc(100, 1);
        assert!(!ptr1.is_null());

        let ptr2 = arena.alloc(200, 1);
        assert!(!ptr2.is_null());
        assert_ne!(ptr1, ptr2);
    }

    /// A zero-length chunk request must fail closed on every backend: the
    /// production `mmap`/`VirtualAlloc` path rejects `len == 0`
    /// (`MAP_FAILED`/null → `None`), and the `cfg(miri)` `std::alloc` shim must
    /// match — passing a zero-sized layout to `std::alloc::alloc` is undefined
    /// behaviour.  Neither backend may fabricate an allocation for size zero.
    #[test]
    fn arena_zero_size_chunk_fails_closed() {
        assert!(
            ArenaChunk::new(0).is_none(),
            "a zero-size chunk must not allocate"
        );
        assert!(
            ActorArena::new_with_sizes(0, 4096).is_none(),
            "an arena with a zero initial chunk must not construct"
        );
    }

    #[test]
    fn arena_alloc_with_alignment() {
        let mut arena = ActorArena::new().expect("Failed to create arena");

        let ptr1 = arena.alloc(1, 1);
        assert!(!ptr1.is_null());

        let ptr2 = arena.alloc(8, 8);
        assert!(!ptr2.is_null());
        assert_eq!(ptr2 as usize % 8, 0);

        let ptr3 = arena.alloc(16, 16);
        assert!(!ptr3.is_null());
        assert_eq!(ptr3 as usize % 16, 0);
    }

    #[test]
    fn arena_reset_and_reuse() {
        let mut arena = ActorArena::new().expect("Failed to create arena");

        let ptr1 = arena.alloc(100, 1);
        assert!(!ptr1.is_null());

        arena.reset();

        let ptr2 = arena.alloc(100, 1);
        assert!(!ptr2.is_null());
        assert_eq!(ptr1, ptr2); // Should reuse the same memory
    }

    #[test]
    fn arena_free_all() {
        let arena = ActorArena::new().expect("Failed to create arena");

        // This test mainly checks that free_all doesn't panic
        arena.free_all();
    }

    #[test]
    fn multiple_chunks() {
        // Create arena with small initial chunk to force multiple chunks
        let mut arena =
            ActorArena::new_with_sizes(1024, 16 * 1024).expect("Failed to create arena");

        // Allocate more than initial chunk size
        let ptr1 = arena.alloc(2048, 1);
        assert!(!ptr1.is_null());

        let ptr2 = arena.alloc(2048, 1);
        assert!(!ptr2.is_null());
        assert_ne!(ptr1, ptr2);

        assert!(arena.chunks.len() >= 2);
    }

    #[test]
    fn reset_reuses_retained_chunks_after_first_chunk_fills_again() {
        let mut arena = ActorArena::new_with_sizes(32, 128).expect("Failed to create arena");

        let first = arena.alloc(32, 1);
        assert!(!first.is_null());
        let second = arena.alloc(40, 1);
        assert!(!second.is_null());
        assert_eq!(arena.chunks.len(), 2);
        let retained_second_base = arena.chunks[1].base;

        arena.reset();

        let reused_first = arena.alloc(32, 1);
        assert_eq!(reused_first, first);
        let chunk_count_before = arena.chunks.len();
        let reused_second = arena.alloc(16, 1);

        assert_eq!(reused_second, retained_second_base);
        assert_eq!(arena.chunks.len(), chunk_count_before);
        assert_eq!(arena.current_chunk, 1);
    }

    #[test]
    fn hew_arena_malloc_without_context_fails_closed() {
        crate::hew_clear_error();
        // SAFETY: testing fail-closed sentinel with no installed context.
        let ptr = unsafe { hew_arena_malloc(100) };
        assert!(ptr.is_null());
        let err = crate::hew_last_error();
        assert!(!err.is_null());
        // SAFETY: hew_last_error returned a non-null C string.
        let err = unsafe { std::ffi::CStr::from_ptr(err).to_str().unwrap() };
        assert_eq!(
            err,
            crate::execution_context::EXECUTION_CONTEXT_NOT_INSTALLED
        );

        crate::hew_clear_error();
    }

    #[test]
    fn hew_arena_c_api() {
        let arena = hew_arena_new();
        assert!(!arena.is_null());
        let _ctx = TestExecutionContext::install(HewExecutionContext::default());

        // SAFETY: arena is valid from hew_arena_new
        unsafe { hew_arena_set_current(arena) };

        // SAFETY: testing malloc with arena active
        let ptr = unsafe { hew_arena_malloc(100) };
        assert!(!ptr.is_null());

        // SAFETY: ptr is valid from malloc above
        unsafe { hew_arena_free(ptr) }; // Should be no-op

        // SAFETY: arena is valid
        unsafe { hew_arena_reset(arena) };

        // Clear current arena
        // SAFETY: null is always safe
        unsafe { hew_arena_set_current(ptr::null_mut()) };

        // SAFETY: arena is valid
        unsafe { hew_arena_free_all(arena) };
    }

    #[test]
    fn zero_size_alloc() {
        let mut arena = ActorArena::new().expect("Failed to create arena");

        let ptr = arena.alloc(0, 1);
        assert!(ptr.is_null());
    }

    // ── Cap enforcement tests ──────────────────────────────────────────────

    /// cap=0 means unbounded: allocations succeed as before.
    #[test]
    fn arena_cap_zero_is_unbounded() {
        let mut arena = ActorArena::new_with_cap(0).expect("arena creation must succeed");
        // Allocate well beyond any would-be small cap.
        for _ in 0..100 {
            let p = arena.alloc(1024, 1);
            assert!(!p.is_null(), "unbounded arena must not return null");
        }
    }

    /// Allocations below the cap succeed; the first one that would exceed it returns null.
    #[test]
    fn arena_cap_alloc_under_cap_succeeds() {
        let cap = 512_usize;
        let mut arena = ActorArena::new_with_cap(cap).expect("arena creation must succeed");

        let p1 = arena.alloc(256, 1);
        assert!(!p1.is_null(), "first alloc under cap must succeed");

        let p2 = arena.alloc(256, 1);
        assert!(!p2.is_null(), "second alloc exactly at cap must succeed");
    }

    /// An allocation that would push total bytes over cap returns null (fail-closed).
    #[test]
    fn arena_cap_alloc_over_cap_returns_null() {
        let cap = 512_usize;
        let mut arena = ActorArena::new_with_cap(cap).expect("arena creation must succeed");

        let p1 = arena.alloc(512, 1);
        assert!(!p1.is_null(), "alloc exactly at cap must succeed");

        // This allocation would push used to 513 > cap.
        let p2 = arena.alloc(1, 1);
        assert!(p2.is_null(), "alloc that exceeds cap must return null");
    }

    /// After `reset()`, used resets to zero and allocations up to cap succeed again.
    #[test]
    fn arena_cap_reset_clears_used() {
        let cap = 256_usize;
        let mut arena = ActorArena::new_with_cap(cap).expect("arena creation must succeed");

        // Fill to cap.
        let p1 = arena.alloc(256, 1);
        assert!(!p1.is_null());
        // Over cap: must fail.
        let p2 = arena.alloc(1, 1);
        assert!(p2.is_null());

        // Reset restarts the cycle.
        arena.reset();
        let p3 = arena.alloc(256, 1);
        assert!(!p3.is_null(), "alloc after reset must succeed up to cap");
        let p4 = arena.alloc(1, 1);
        assert!(
            p4.is_null(),
            "alloc after reset must still fail when over cap"
        );
    }

    /// `hew_arena_new_with_cap` is the C ABI constructor; verify it enforces the cap.
    #[test]
    fn hew_arena_new_with_cap_c_api() {
        let cap = 128_usize;
        let arena = hew_arena_new_with_cap(cap);
        assert!(!arena.is_null(), "hew_arena_new_with_cap must succeed");
        let _ctx = TestExecutionContext::install(HewExecutionContext::default());

        // SAFETY: arena is a valid pointer from hew_arena_new_with_cap.
        unsafe { hew_arena_set_current(arena) };

        // Under cap.
        // SAFETY: arena is installed; malloc routes through it.
        let p1 = unsafe { hew_arena_malloc(64) };
        assert!(!p1.is_null(), "malloc under cap must succeed");

        // SAFETY: arena is installed; second malloc routes through it.
        let p2 = unsafe { hew_arena_malloc(64) };
        assert!(!p2.is_null(), "malloc exactly at cap must succeed");

        // Over cap.
        // SAFETY: arena is installed; malloc must return null when cap exhausted.
        let p3 = unsafe { hew_arena_malloc(1) };
        assert!(p3.is_null(), "malloc over cap must return null");

        // SAFETY: null is always safe.
        unsafe { hew_arena_set_current(ptr::null_mut()) };

        // SAFETY: arena is valid and not installed.
        unsafe { hew_arena_free_all(arena) };
    }
}
