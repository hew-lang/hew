//! Per-actor arena bump allocator for WASM targets.
//!
//! This is the wasm32 counterpart of the native [`arena`] module.  The
//! lifecycle contract is identical:
//!
//! ```text
//! hew_arena_new()
//!   └─► hew_arena_set_current(arena)   ← install as current; alloc/free route here
//!         │
//!         │  [actor dispatch runs]
//!         │    hew_arena_malloc(n)   → bump-allocates from the arena
//!         │    hew_arena_free(ptr)   → **no-op** (bulk-free is cheaper)
//!         │
//!       hew_arena_set_current(null)   ← uninstall; alloc/free revert to libc
//!         │
//!         ├── hew_arena_reset(arena)  → cursor back to zero, chunks retained
//!         │     └─► hew_arena_set_current(arena) … repeat for next dispatch
//!         │
//!         └── hew_arena_free_all(arena) → free every chunk; pointer invalid
//! ```
//!
//! **When no arena is active** (`CURRENT_ARENA` is null) `hew_arena_malloc`
//! delegates to `libc::malloc` and `hew_arena_free` delegates to `libc::free`.
//!
//! # Backing allocator
//!
//! On wasm32 there is no `mmap` or `VirtualAlloc`.  Each arena chunk is backed
//! by a heap allocation obtained via [`std::alloc`], which resolves to the
//! embedded malloc bundled with the WASM runtime (typically dlmalloc or
//! wasm-opt's built-in allocator).  Chunks are sized and aligned to 16 bytes so
//! that offset 0 of any fresh chunk satisfies all standard Rust/C alignment
//! requirements (≤ 16 bytes).
//!
//! # Chunk growth
//!
//! Identical to the native arena: initial chunk is 64 KiB, subsequent chunks
//! double up to a 16 MiB cap.
//!
//! # Alignment invariant
//!
//! Every `alloc` call rounds the cursor up with the standard power-of-two mask:
//! `(cursor + align - 1) & !(align - 1)`.  **`align` must be a power of two**
//! — a `debug_assert!` guards this in debug builds.

use std::alloc::{alloc, dealloc, Layout};
use std::cell::Cell;
use std::os::raw::c_void;
use std::ptr;

/// Minimum chunk alignment: covers all standard Rust/C alignment requirements.
const CHUNK_ALIGN: usize = 16;

/// A chunk of backing memory allocated via [`std::alloc`].
#[derive(Debug)]
struct ArenaChunk {
    base: *mut u8,
    size: usize,
}

impl ArenaChunk {
    fn new(size: usize) -> Option<Self> {
        debug_assert!(size > 0, "chunk size must be non-zero");
        // SAFETY: size > 0 and CHUNK_ALIGN is a valid power-of-two alignment.
        let layout = Layout::from_size_align(size, CHUNK_ALIGN).ok()?;
        let base = unsafe { alloc(layout) };
        if base.is_null() {
            return None;
        }
        Some(ArenaChunk { base, size })
    }
}

impl Drop for ArenaChunk {
    fn drop(&mut self) {
        // SAFETY: base was allocated with the same layout.
        let layout = Layout::from_size_align(self.size, CHUNK_ALIGN).expect("layout must be valid");
        unsafe { dealloc(self.base, layout) };
    }
}

// ArenaChunk holds a raw pointer; assure the compiler it is Send/Sync
// (safe: single-threaded WASM, no concurrent access).
// SAFETY: WASM is single-threaded; no concurrent access to chunk memory.
unsafe impl Send for ArenaChunk {}
unsafe impl Sync for ArenaChunk {}

/// Per-actor arena bump allocator (WASM variant).
///
/// Identical public API and lifecycle contract to the native [`ActorArena`].
#[derive(Debug)]
pub struct ActorArena {
    chunks: Vec<ArenaChunk>,
    current_chunk: usize,
    cursor: usize,
    initial_chunk_size: usize,
    max_chunk_size: usize,
}

impl ActorArena {
    /// Create a new actor arena with default chunk sizes (64 KiB initial, 16 MiB max).
    #[must_use]
    pub fn new() -> Option<Self> {
        Self::new_with_sizes(64 * 1024, 16 * 1024 * 1024)
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
        })
    }

    /// Allocate `size` bytes with `align`-byte alignment.
    ///
    /// `align` must be a power of two (asserted in debug builds).
    /// Returns null if `size` is zero or memory is exhausted.
    pub fn alloc(&mut self, size: usize, align: usize) -> *mut u8 {
        debug_assert!(
            align.is_power_of_two(),
            "align must be a power of two, got {align}"
        );

        if size == 0 {
            return ptr::null_mut();
        }

        let aligned_cursor = (self.cursor + align - 1) & !(align - 1);
        let end_offset = aligned_cursor + size;

        if self.current_chunk < self.chunks.len() {
            let chunk = &self.chunks[self.current_chunk];
            if end_offset <= chunk.size {
                self.cursor = end_offset;
                // SAFETY: aligned_cursor is within chunk bounds.
                return unsafe { chunk.base.add(aligned_cursor) };
            }
        }

        // Need a new chunk.
        let next_size = if self.chunks.is_empty() {
            self.initial_chunk_size
        } else {
            let last = self.chunks[self.chunks.len() - 1].size;
            std::cmp::min(last * 2, self.max_chunk_size)
        };
        let chunk_size = std::cmp::max(next_size, size + align);

        let Some(new_chunk) = ArenaChunk::new(chunk_size) else {
            return ptr::null_mut();
        };

        self.chunks.push(new_chunk);
        self.current_chunk = self.chunks.len() - 1;

        // Offset 0 is aligned to CHUNK_ALIGN (≥ any requested align up to 16 bytes).
        // For larger alignments the alignment computation still works because
        // rounding 0 up by any power-of-two is still 0.
        self.cursor = size;

        // SAFETY: offset 0 is within the new chunk bounds.
        self.chunks[self.current_chunk].base
    }

    /// Reset the arena for reuse after a completed dispatch cycle.
    ///
    /// The cursor returns to zero and `current_chunk` rewinds to the first
    /// chunk.  All previously mapped chunks are **retained** so subsequent
    /// activations can reuse already-allocated backing memory.  Any pointers
    /// into the arena that were live before this call are invalidated.
    pub fn reset(&mut self) {
        self.current_chunk = 0;
        self.cursor = 0;
    }

    /// Free all chunks and destroy the arena.
    ///
    /// Every chunk is deallocated.  The `ActorArena` value is consumed; any
    /// pointer to it (e.g. one previously passed to `hew_arena_set_current`)
    /// becomes invalid after this call.
    pub fn free_all(self) {
        // Drop self — `chunks` is a Vec<ArenaChunk> whose Drop impl calls
        // dealloc on each chunk's backing allocation.
    }
}

impl Default for ActorArena {
    fn default() -> Self {
        Self::new().expect("Failed to create default arena")
    }
}

// ActorArena holds raw pointers (inside ArenaChunk); assure the compiler.
// SAFETY: WASM is single-threaded; no concurrent access possible.
unsafe impl Send for ActorArena {}
unsafe impl Sync for ActorArena {}

// ── Thread-local current arena ────────────────────────────────────────────
//
// On wasm32 there is only one thread, so a thread-local is effectively a
// global.  We still use thread_local! to keep the API identical to native
// and to satisfy potential single-threaded wasm-bindgen / WASI thread models.

thread_local! {
    static CURRENT_ARENA: Cell<*mut ActorArena> = const { Cell::new(ptr::null_mut()) };
}

/// Set the current thread-local arena and return the previous one.
pub fn set_current_arena(arena: *mut ActorArena) -> *mut ActorArena {
    CURRENT_ARENA.with(|current| current.replace(arena))
}

/// Get the current thread-local arena.
fn get_current_arena() -> *mut ActorArena {
    CURRENT_ARENA.with(std::cell::Cell::get)
}

// ── C ABI ─────────────────────────────────────────────────────────────────
//
// The `#[no_mangle]` attribute is guarded by `#[cfg(target_arch = "wasm32")]`
// so that this file can also be compiled as `pub mod arena_wasm` in native
// test builds (for CI coverage) without clashing with the identical symbols
// exported by `arena.rs`.  On wasm32 the symbols are always exported.

/// Allocate memory from the current arena, falling back to `libc::malloc`.
///
/// # Safety
///
/// The returned pointer must eventually be passed to `hew_arena_free` (while
/// the same arena is active) or to `libc::free` (when no arena is active).
#[cfg_attr(target_arch = "wasm32", no_mangle)]
pub unsafe extern "C" fn hew_arena_malloc(size: usize) -> *mut c_void {
    let arena_ptr = get_current_arena();
    if arena_ptr.is_null() {
        // SAFETY: libc malloc is always safe to call.
        unsafe { libc::malloc(size) }
    } else {
        // SAFETY: arena_ptr is a valid pointer set by hew_arena_set_current.
        let arena = unsafe { &mut *arena_ptr };
        arena
            .alloc(size, std::mem::align_of::<*mut c_void>())
            .cast::<c_void>()
    }
}

/// Free memory — no-op during arena dispatch, forwards to `libc::free` otherwise.
///
/// # Safety
///
/// `ptr` must be a valid pointer returned by `hew_arena_malloc` (or null).
#[cfg_attr(target_arch = "wasm32", no_mangle)]
pub unsafe extern "C" fn hew_arena_free(ptr: *mut c_void) {
    let arena_ptr = get_current_arena();
    if arena_ptr.is_null() {
        // SAFETY: caller guarantees ptr is valid.
        unsafe { libc::free(ptr) };
    }
    // Arena active → no-op; memory reclaimed in bulk via reset / free_all.
}

/// Create a new arena and return an owning raw pointer.
#[cfg_attr(target_arch = "wasm32", no_mangle)]
pub extern "C" fn hew_arena_new() -> *mut ActorArena {
    match ActorArena::new() {
        Some(arena) => Box::into_raw(Box::new(arena)),
        None => ptr::null_mut(),
    }
}

/// Reset an arena for reuse (cursor → 0, chunks retained).
///
/// # Safety
///
/// `arena` must be a valid pointer returned by `hew_arena_new` or null.
#[cfg_attr(target_arch = "wasm32", no_mangle)]
pub unsafe extern "C" fn hew_arena_reset(arena: *mut ActorArena) {
    if !arena.is_null() {
        // SAFETY: caller guarantees arena is valid.
        let arena = unsafe { &mut *arena };
        arena.reset();
    }
}

/// Free all chunks and destroy the arena.
///
/// # Safety
///
/// `arena` must be a valid pointer returned by `hew_arena_new` or null.
/// After this call `arena` is no longer valid.
#[cfg_attr(target_arch = "wasm32", no_mangle)]
pub unsafe extern "C" fn hew_arena_free_all(arena: *mut ActorArena) {
    if !arena.is_null() {
        // SAFETY: caller guarantees arena is valid.
        let arena = unsafe { Box::from_raw(arena) };
        arena.free_all();
    }
}

/// Install `arena` as the current thread-local arena.
///
/// # Safety
///
/// `arena` must be a valid pointer returned by `hew_arena_new` or null.
#[cfg_attr(target_arch = "wasm32", no_mangle)]
pub unsafe extern "C" fn hew_arena_set_current(arena: *mut ActorArena) {
    set_current_arena(arena);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn arena_alloc_basic() {
        let mut arena = ActorArena::new().expect("arena creation must succeed");

        let ptr1 = arena.alloc(100, 1);
        assert!(!ptr1.is_null());

        let ptr2 = arena.alloc(200, 1);
        assert!(!ptr2.is_null());
        assert_ne!(ptr1, ptr2);
    }

    #[test]
    fn arena_alloc_with_alignment() {
        let mut arena = ActorArena::new().expect("arena creation must succeed");

        let ptr1 = arena.alloc(1, 1);
        assert!(!ptr1.is_null());

        let ptr2 = arena.alloc(8, 8);
        assert!(!ptr2.is_null());
        assert_eq!(ptr2 as usize % 8, 0, "8-byte alignment");

        let ptr3 = arena.alloc(16, 16);
        assert!(!ptr3.is_null());
        assert_eq!(ptr3 as usize % 16, 0, "16-byte alignment");
    }

    #[test]
    fn arena_reset_and_reuse() {
        let mut arena = ActorArena::new().expect("arena creation must succeed");

        let ptr1 = arena.alloc(100, 1);
        assert!(!ptr1.is_null());

        arena.reset();

        let ptr2 = arena.alloc(100, 1);
        assert!(!ptr2.is_null());
        // After reset the cursor is back to 0; the same base pointer is reused.
        assert_eq!(ptr1, ptr2, "reset must reuse the same backing memory");
    }

    #[test]
    fn arena_free_all() {
        let arena = ActorArena::new().expect("arena creation must succeed");
        // Mainly checks that free_all does not panic or leak.
        arena.free_all();
    }

    #[test]
    fn multiple_chunks() {
        let mut arena =
            ActorArena::new_with_sizes(1024, 16 * 1024).expect("arena creation must succeed");

        // Allocate more than the initial chunk size to force a second chunk.
        let ptr1 = arena.alloc(2048, 1);
        assert!(!ptr1.is_null());

        let ptr2 = arena.alloc(2048, 1);
        assert!(!ptr2.is_null());
        assert_ne!(ptr1, ptr2);

        assert!(
            arena.chunks.len() >= 2,
            "must have spilled into a second chunk"
        );
    }

    #[test]
    fn hew_arena_malloc_free_fallback() {
        // No arena active — must delegate to libc malloc/free.
        // SAFETY: testing with valid allocation size.
        let ptr = unsafe { hew_arena_malloc(100) };
        assert!(!ptr.is_null());
        // SAFETY: ptr is valid from hew_arena_malloc above.
        unsafe { hew_arena_free(ptr) };
    }

    #[test]
    fn hew_arena_c_api() {
        let arena = hew_arena_new();
        assert!(!arena.is_null(), "hew_arena_new must succeed");

        // SAFETY: arena is a valid pointer from hew_arena_new.
        unsafe { hew_arena_set_current(arena) };

        // SAFETY: arena is installed; allocation routes through it.
        let ptr = unsafe { hew_arena_malloc(100) };
        assert!(!ptr.is_null());

        // SAFETY: ptr is arena-allocated; free is a no-op while arena is active.
        unsafe { hew_arena_free(ptr) };

        // SAFETY: arena is valid.
        unsafe { hew_arena_reset(arena) };

        // Clear the current arena.
        // SAFETY: null is always safe.
        unsafe { hew_arena_set_current(ptr::null_mut()) };

        // SAFETY: arena is valid and no longer installed.
        unsafe { hew_arena_free_all(arena) };
    }

    #[test]
    fn zero_size_alloc() {
        let mut arena = ActorArena::new().expect("arena creation must succeed");
        let ptr = arena.alloc(0, 1);
        assert!(ptr.is_null(), "zero-size alloc must return null");
    }

    #[test]
    fn arena_install_restore_round_trip() {
        // Verify set_current_arena returns the previously installed arena.
        let arena = hew_arena_new();
        assert!(!arena.is_null());

        let prev = set_current_arena(arena);
        assert!(prev.is_null(), "no arena was active before install");

        let prev2 = set_current_arena(ptr::null_mut());
        assert_eq!(
            prev2, arena,
            "restoring null must return the installed arena"
        );

        // SAFETY: arena is valid, not installed.
        unsafe { hew_arena_free_all(arena) };
    }
}
