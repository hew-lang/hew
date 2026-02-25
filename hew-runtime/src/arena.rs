//! Per-actor arena bump allocator for the Hew actor runtime.
//!
//! This provides fast allocation during actor dispatch and clean bulk-free on crash.
//! Uses memory-mapped chunks that grow as needed, with bump allocation within chunks.
//! Thread-local arena tracking enables transparent malloc/free redirection during dispatch.

use std::cell::Cell;
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

/// Memory chunk allocated via mmap (Unix) or VirtualAlloc (Windows).
#[derive(Debug)]
struct ArenaChunk {
    base: *mut u8,
    size: usize,
}

impl ArenaChunk {
    /// Create a new chunk with the specified size.
    fn new(size: usize) -> Option<Self> {
        #[cfg(unix)]
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

        #[cfg(windows)]
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
        })
    }

    /// Allocate memory with the specified size and alignment.
    ///
    /// Returns null pointer if allocation fails.
    pub fn alloc(&mut self, size: usize, align: usize) -> *mut u8 {
        if size == 0 {
            return ptr::null_mut();
        }

        // Align the cursor
        let aligned_cursor = (self.cursor + align - 1) & !(align - 1);
        let end_offset = aligned_cursor + size;

        // Check if current chunk has enough space
        if self.current_chunk < self.chunks.len() {
            let chunk = &self.chunks[self.current_chunk];
            if end_offset <= chunk.size {
                self.cursor = end_offset;
                // SAFETY: aligned_cursor is within chunk bounds
                return unsafe { chunk.base.add(aligned_cursor) };
            }
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

        // Allocate from the new chunk
        let aligned_cursor = (align - 1) & !(align - 1); // Start of chunk, aligned
        self.cursor = aligned_cursor + size;

        // SAFETY: aligned_cursor is within new chunk bounds
        unsafe { self.chunks[self.current_chunk].base.add(aligned_cursor) }
    }

    /// Reset the arena for reuse, keeping memory allocated.
    pub fn reset(&mut self) {
        self.current_chunk = 0;
        self.cursor = 0;
    }

    /// Free all chunks and destroy the arena.
    pub fn free_all(self) {
        // chunks will be dropped, calling ArenaChunk::drop which calls free()
    }
}

impl Drop for ArenaChunk {
    fn drop(&mut self) {
        #[cfg(unix)]
        {
            // SAFETY: base and size are valid from successful mmap
            unsafe {
                libc::munmap(self.base.cast::<c_void>(), self.size);
            }
        }
        #[cfg(windows)]
        {
            // SAFETY: base was allocated by VirtualAlloc with MEM_COMMIT | MEM_RESERVE.
            unsafe {
                VirtualFree(self.base.cast(), 0, MEM_RELEASE);
            }
        }
    }
}

impl Default for ActorArena {
    fn default() -> Self {
        Self::new().expect("Failed to create default arena")
    }
}

// Thread-local current arena tracking
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

// C ABI functions

/// Allocate memory from the current arena, falling back to libc malloc.
///
/// # Safety
///
/// The returned pointer must be freed with `hew_arena_free` or `free()` depending
/// on whether an arena was active during allocation.
#[no_mangle]
pub unsafe extern "C" fn hew_arena_malloc(size: usize) -> *mut c_void {
    let arena_ptr = get_current_arena();
    if arena_ptr.is_null() {
        // No arena active, use libc malloc
        // SAFETY: libc malloc is safe for any size
        unsafe { libc::malloc(size) }
    } else {
        // SAFETY: arena_ptr is valid (set by hew_arena_set_current)
        let arena = unsafe { &mut *arena_ptr };
        arena
            .alloc(size, std::mem::align_of::<*mut c_void>())
            .cast::<c_void>()
    }
}

/// Free memory - no-op during arena dispatch, forwards to libc free otherwise.
///
/// # Safety
///
/// ptr must be a valid pointer returned by `hew_arena_malloc` or `malloc()`.
#[no_mangle]
pub unsafe extern "C" fn hew_arena_free(ptr: *mut c_void) {
    let arena_ptr = get_current_arena();
    if arena_ptr.is_null() {
        // No arena active, use libc free
        // SAFETY: caller guarantees ptr is valid
        unsafe { libc::free(ptr) };
    }
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
    }
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
        // SAFETY: caller guarantees arena is valid
        let arena = unsafe { Box::from_raw(arena) };
        arena.free_all();
    }
}

/// Set the current thread-local arena.
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

    #[test]
    fn arena_alloc_basic() {
        let mut arena = ActorArena::new().expect("Failed to create arena");

        let ptr1 = arena.alloc(100, 1);
        assert!(!ptr1.is_null());

        let ptr2 = arena.alloc(200, 1);
        assert!(!ptr2.is_null());
        assert_ne!(ptr1, ptr2);
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
    fn hew_arena_malloc_free_fallback() {
        // Test without arena active
        // SAFETY: testing malloc/free with valid size
        let ptr = unsafe { hew_arena_malloc(100) };
        assert!(!ptr.is_null());

        // SAFETY: ptr is valid from malloc above
        unsafe { hew_arena_free(ptr) };
    }

    #[test]
    fn hew_arena_c_api() {
        let arena = hew_arena_new();
        assert!(!arena.is_null());

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
}
