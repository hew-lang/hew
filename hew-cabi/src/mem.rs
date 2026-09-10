//! Sized heap blocks on the Rust global allocator (D463).
//!
//! Every allocation Hew owns goes through the Rust global allocator. Where the
//! size and alignment are known at both the allocation and the release site,
//! the caller uses `hew_alloc` / `hew_dealloc` (`hew-runtime/src/mem.rs`) or
//! plain `Box` / `Vec` / `alloc::alloc`. This module covers the other shape:
//! a raw C-ABI byte buffer whose release site holds only the pointer -- a
//! mailbox payload, a reply value, a serialized frame, an actor state wrapper.
//! [`buf_alloc`] stores the payload size in a header immediately before the
//! returned pointer, so [`buf_free`] reconstructs the exact [`Layout`] the
//! global allocator needs.
//!
//! # Why not `libc::malloc`
//!
//! `libc::malloc` and the Rust global allocator are one heap on POSIX only by
//! coincidence. On Windows the Rust `System` allocator is `HeapAlloc` while
//! libc is the UCRT heap, so releasing a block through the other family
//! corrupts the heap; over-aligned blocks differ on every target. Routing
//! through one allocator also makes `std.observe`'s heap counters complete by
//! construction, because `ProfilingAllocator` wraps `GlobalAlloc`.
//!
//! # Why the header is also the oracle
//!
//! On Linux the Rust `System` allocator forwards to `libc::malloc` for
//! alignments up to 16, so a plain family swap that got a pair wrong would be
//! invisible to ASan and would only surface on Windows. Because these blocks
//! carry a header, the returned pointer is *not* an allocation base: a stray
//! `libc::free` on it, or a header read on a libc block, is an invalid free
//! that ASan reports on Linux. The header is what makes `make core-safety` an
//! oracle for this migration.
//!
//! # Layout contract
//!
//! ```text
//! base                      ptr = base + HEADER
//!  |                         |
//!  v                         v
//!  [ size: usize | padding ] [ payload: size bytes ]
//!  \------ 16 bytes -------/
//! ```
//!
//! The block is allocated with alignment [`BUF_ALIGN`] (16), matching what
//! `libc::malloc` guaranteed before the migration, and the header is exactly
//! [`BUF_ALIGN`] bytes so the payload keeps that alignment.
//!
//! A zero-length request allocates a one-byte payload, so the returned pointer
//! is always non-null and always releasable -- the same sentinel behaviour the
//! `libc::malloc(n.max(1))` call sites relied on.

use std::alloc::{alloc, dealloc, handle_alloc_error, realloc, Layout};
use std::ffi::c_void;

/// Alignment of every block this module hands out.
///
/// 16 bytes: what `libc::malloc` guaranteed on every target Hew supports, so
/// payloads that were over-aligned before the migration stay over-aligned.
pub const BUF_ALIGN: usize = 16;

/// Bytes reserved before the payload for the size header. Equal to
/// [`BUF_ALIGN`] so the payload pointer keeps the block's alignment.
const HEADER: usize = BUF_ALIGN;

/// Block layout for a `size`-byte payload, or `None` when the total would
/// overflow `isize`.
#[inline]
fn block_layout(size: usize) -> Option<Layout> {
    let total = size.checked_add(HEADER)?;
    Layout::from_size_align(total, BUF_ALIGN).ok()
}

/// Read the payload size recorded for a block handed out by [`buf_alloc`].
///
/// # Safety
///
/// `ptr` must be a non-null pointer returned by [`buf_alloc`] or
/// [`buf_realloc`] and still live.
#[inline]
unsafe fn payload_size(ptr: *mut c_void) -> usize {
    // SAFETY: caller guarantees ptr came from this module, so the 16 bytes
    // before it are the header and are suitably aligned for `usize`.
    unsafe { ptr.cast::<u8>().sub(HEADER).cast::<usize>().read() }
}

/// Allocate a `size`-byte payload, returning null when the allocator cannot
/// satisfy the request.
///
/// A `size` of 0 allocates a one-byte sentinel, so a successful result is
/// never null. Use this where the caller has a real recovery path (mailbox
/// admission reports an OOM outcome to the sender); use [`buf_alloc`] where
/// it does not.
///
/// The result must be released with [`buf_free`], never `libc::free`.
#[must_use]
pub fn buf_try_alloc(size: usize) -> *mut c_void {
    let size = size.max(1);
    let Some(layout) = block_layout(size) else {
        return std::ptr::null_mut();
    };
    // SAFETY: layout has non-zero size (HEADER > 0).
    let base = unsafe { alloc(layout) }; // ALLOCATOR-PAIRING: GlobalAlloc
    if base.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: base is a fresh block of at least HEADER bytes, aligned to
    // BUF_ALIGN >= align_of::<usize>().
    unsafe { base.cast::<usize>().write(size) };
    // SAFETY: the block is HEADER + size bytes, so base + HEADER is in range.
    unsafe { base.add(HEADER).cast::<c_void>() }
}

/// Allocate a `size`-byte payload from the Rust global allocator, aborting on
/// allocation failure through [`handle_alloc_error`].
///
/// A `size` of 0 allocates a one-byte sentinel, so the result is never null.
///
/// The result must be released with [`buf_free`], never `libc::free`.
#[must_use]
pub fn buf_alloc(size: usize) -> *mut c_void {
    let ptr = buf_try_alloc(size);
    if ptr.is_null() {
        handle_alloc_error(block_layout(size.max(1)).unwrap_or(Layout::new::<usize>()));
    }
    ptr
}

/// Resize the block at `ptr` to `new_size` bytes, preserving the leading
/// `min(old, new)` payload bytes.
///
/// A null `ptr` allocates. Aborts on OOM, like [`buf_alloc`].
///
/// # Safety
///
/// `ptr`, if non-null, must have come from [`buf_alloc`] or [`buf_realloc`]
/// and must not have been released. It is invalid after this call.
#[must_use]
pub unsafe fn buf_realloc(ptr: *mut c_void, new_size: usize) -> *mut c_void {
    if ptr.is_null() {
        return buf_alloc(new_size);
    }
    let new_size = new_size.max(1);
    // SAFETY: caller guarantees ptr came from this module.
    let old_size = unsafe { payload_size(ptr) };
    // SAFETY: same guarantee; the base is HEADER bytes before the payload.
    let base = unsafe { ptr.cast::<u8>().sub(HEADER) };
    let Some(old_layout) = block_layout(old_size) else {
        // The recorded size came from a successful block_layout, so this is
        // unreachable unless the header was corrupted.
        handle_alloc_error(Layout::new::<usize>());
    };
    let Some(new_layout) = block_layout(new_size) else {
        handle_alloc_error(old_layout);
    };
    // SAFETY: base was allocated with old_layout; new_layout has the same
    // alignment and a non-zero size.
    let grown = unsafe { realloc(base, old_layout, new_layout.size()) }; // ALLOCATOR-PAIRING: GlobalAlloc
    if grown.is_null() {
        handle_alloc_error(new_layout);
    }
    // SAFETY: grown is a live block of at least HEADER bytes.
    unsafe { grown.cast::<usize>().write(new_size) };
    // SAFETY: the block is HEADER + new_size bytes.
    unsafe { grown.add(HEADER).cast::<c_void>() }
}

/// Release a block handed out by [`buf_alloc`] or [`buf_realloc`].
///
/// Null is a no-op, matching `libc::free(NULL)`.
///
/// # Safety
///
/// `ptr`, if non-null, must have come from this module and must not have been
/// released already.
pub unsafe fn buf_free(ptr: *mut c_void) {
    if ptr.is_null() {
        return;
    }
    // SAFETY: caller guarantees ptr came from this module.
    let size = unsafe { payload_size(ptr) };
    // SAFETY: same guarantee; the base is HEADER bytes before the payload.
    let base = unsafe { ptr.cast::<u8>().sub(HEADER) };
    let Some(layout) = block_layout(size) else {
        // Only reachable through a corrupted header; deallocating with a wrong
        // layout is undefined behaviour, so fail closed instead.
        handle_alloc_error(Layout::new::<usize>());
    };
    // SAFETY: base was allocated with exactly this layout.
    unsafe { dealloc(base, layout) }; // ALLOCATOR-PAIRING: GlobalAlloc
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn alloc_write_read_free_roundtrips() {
        let ptr = buf_alloc(64);
        assert!(!ptr.is_null());
        assert_eq!(ptr as usize % BUF_ALIGN, 0, "payload keeps block alignment");
        // SAFETY: ptr owns 64 writable bytes and is freed once.
        unsafe {
            let bytes = ptr.cast::<u8>();
            for i in 0..64usize {
                bytes.add(i).write(u8::try_from(i).unwrap());
            }
            for i in 0..64usize {
                assert_eq!(bytes.add(i).read(), u8::try_from(i).unwrap());
            }
            buf_free(ptr);
        }
    }

    #[test]
    fn zero_size_yields_a_freeable_sentinel() {
        let ptr = buf_alloc(0);
        assert!(!ptr.is_null(), "zero-length request must still be freeable");
        // SAFETY: ptr owns a one-byte payload.
        unsafe {
            ptr.cast::<u8>().write(0xAB);
            assert_eq!(ptr.cast::<u8>().read(), 0xAB);
            buf_free(ptr);
        }
    }

    #[test]
    fn realloc_preserves_bytes_and_grows() {
        // SAFETY: single-owner block, grown once, freed once.
        unsafe {
            let ptr = buf_alloc(8);
            for i in 0..8usize {
                ptr.cast::<u8>().add(i).write(u8::try_from(i + 1).unwrap());
            }
            let grown = buf_realloc(ptr, 4096);
            for i in 0..8usize {
                assert_eq!(
                    grown.cast::<u8>().add(i).read(),
                    u8::try_from(i + 1).unwrap(),
                    "byte {i} must survive the grow"
                );
            }
            assert_eq!(payload_size(grown), 4096);
            buf_free(grown);
        }
    }

    #[test]
    fn realloc_of_null_allocates() {
        // SAFETY: null base is an explicit allocate; the result is freed once.
        unsafe {
            let ptr = buf_realloc(std::ptr::null_mut(), 32);
            assert!(!ptr.is_null());
            assert_eq!(payload_size(ptr), 32);
            buf_free(ptr);
        }
    }

    #[test]
    fn free_of_null_is_a_no_op() {
        // SAFETY: null is the documented no-op input.
        unsafe { buf_free(std::ptr::null_mut()) };
    }

    /// A high-iteration alloc/free loop completes, proving the blocks really
    /// go back to the allocator rather than leaking one header at a time.
    #[test]
    fn free_returns_memory() {
        // SAFETY: each block is written and freed before the next iteration.
        unsafe {
            for i in 0..50_000usize {
                let ptr = buf_alloc(4096);
                ptr.cast::<u8>().write(u8::try_from(i & 0xff).unwrap());
                ptr.cast::<u8>().add(4095).write(0);
                buf_free(ptr);
            }
        }
    }
}
