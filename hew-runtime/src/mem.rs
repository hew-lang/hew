//! General-purpose heap allocator for the Hew memory-intrinsic floor (F1b).
//!
//! These are the symmetric `alloc`/`realloc`/`dealloc` primitives that back
//! every future Hew-defined generic container's raw heap storage. They are the
//! runtime side of the `mem.*` `#[intrinsic]` floor (`std/mem/mem.hew`):
//! codegen intercepts the `mem.alloc` / `mem.realloc` / `mem.dealloc`
//! intrinsic call sites and lowers them to `build_call` of these symbols.
//!
//! # Relationship to the per-actor arena (`arena.rs`)
//!
//! This is **not** the arena. The arena's free (`hew_arena_free`) is a
//! deliberate no-op (bulk-free at dispatch end) and its C-ABI malloc takes
//! size only. That model is structurally unfit to back heap containers, which
//! need a deterministic, symmetric free. This module is a true symmetric pair
//! over Rust's `GlobalAlloc`. The arena keeps its per-actor-dispatch role
//! unchanged; this is a *new* general allocator alongside it.
//!
//! # `ALLOCATOR-PAIRING`: `GlobalAlloc`
//!
//! `hew_alloc` / `hew_realloc` / `hew_dealloc` all route through
//! `std::alloc::{alloc, realloc, dealloc}` with a [`Layout`] reconstructed from
//! the caller-supplied `(size, align)`. The W4.045-class alloc/free symmetry
//! rule applies to the raw allocator: the **EXACT same** `(size, align)` passed
//! to `hew_alloc` MUST be passed to the matching `hew_dealloc`, and the
//! `old_size` / `align` passed to `hew_realloc` must match the block's original
//! `hew_alloc`. Mismatched layout is undefined behaviour in `GlobalAlloc`;
//! callers (the compiler's container lowering) own threading the matching
//! `(size, align)` through every free edge — see CLAUDE.md §1 drop-safety.
//!
//! # Fail-closed contract (CLAUDE.md §2)
//!
//! - **Invalid layout** — `Layout::from_size_align` rejects a non-power-of-two
//!   `align` or a `size` that overflows `isize` when rounded up to `align`. On
//!   that error `hew_alloc` / `hew_realloc` return null (the caller checks null
//!   before use); they never fabricate a `Layout` or invoke UB.
//! - **Zero size** — `hew_alloc(0, align)` returns null. The floor never
//!   allocates zero bytes (empty containers special-case a dangling/no-op
//!   pointer); a null return makes that contract explicit rather than handing
//!   out a dangling pointer that looks allocated. `hew_dealloc(_, 0, _)` and
//!   `hew_dealloc(null, _, _)` are no-ops — the symmetric partner of a null
//!   alloc.
//! - **Allocation failure (OOM)** — `hew_alloc` / `hew_realloc` abort via
//!   [`std::alloc::handle_alloc_error`] (mirrors `rc.rs:101`). This is
//!   fail-closed: we never return a null that a caller could mistake for a
//!   valid allocation when the request itself was well-formed. (A null caused
//!   by an invalid/zero request is distinguishable: the caller passed a
//!   degenerate `(size, align)`.)
//! - **Invalid layout at free** — `hew_dealloc` with a `(size, align)` that
//!   does not form a valid `Layout` cannot correspond to any block this
//!   allocator handed out (a valid `hew_alloc` would have returned null for
//!   that layout). Calling `dealloc` with a reconstructed-wrong `Layout` is UB,
//!   so we abort instead: a mismatched free is a compiler bug, not a
//!   recoverable condition.
//!
//! # WASM parity (CLAUDE.md §4)
//!
//! Same source on native and `wasm32`. The runtime links a global allocator on
//! both targets: native via `ProfilingAllocator` (`lib.rs:241`), wasm32 via the
//! default Rust allocator backed by wasi-libc/dlmalloc (`lib.rs:238-242`,
//! `arena_wasm.rs:26-33`). `std::alloc::{alloc, realloc, dealloc}` therefore
//! resolve on wasm32 with no target-specific code, and so there is no wasm
//! parity gap to track for this module.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use std::alloc::{alloc, dealloc, realloc, Layout};
use std::ptr;

/// Reconstruct the allocation [`Layout`] for a `(size, align)` pair.
///
/// Convert a fixed-width C-ABI `u64` operand to a host `usize`, fail-closed.
///
/// The `mem.*` C-ABI uses `u64` for sizes/aligns/offsets so the codegen
/// lowering passes the same width on every target (Hew integers are 64-bit).
/// On a 32-bit target (wasm32) a value that does not fit `usize` cannot name a
/// real allocation, so the conversion fails closed (`None`) rather than
/// silently truncating.
#[inline]
fn to_usize(v: u64) -> Option<usize> {
    usize::try_from(v).ok()
}

/// Reconstruct the allocation [`Layout`] for a `(size, align)` pair.
///
/// Returns `None` when the pair is degenerate: `size == 0` (the floor never
/// allocates zero bytes) or `Layout::from_size_align` rejects the pair (align
/// not a power of two, or size overflows `isize` when rounded up to align).
#[inline]
fn floor_layout(size: usize, align: usize) -> Option<Layout> {
    if size == 0 {
        return None;
    }
    Layout::from_size_align(size, align).ok()
}

/// Allocate `size` bytes aligned to `align` from the general heap.
///
/// Returns a pointer to uninitialised storage, or null when the request is
/// degenerate (`size == 0`, an invalid `(size, align)` layout, or a
/// size/align that does not fit the host `usize` — see the module fail-closed
/// contract). On genuine allocation failure (OOM) this aborts via
/// [`std::alloc::handle_alloc_error`] rather than returning null.
///
/// The returned block MUST be released with `hew_dealloc(ptr, size, align)`
/// using the **same** `(size, align)` — see `// ALLOCATOR-PAIRING: GlobalAlloc`.
///
/// # Safety
///
/// Safe to call with any `(size, align)`; degenerate inputs return null. The
/// caller owns the returned allocation and must free it exactly once.
#[no_mangle]
pub unsafe extern "C" fn hew_alloc(size: u64, align: u64) -> *mut u8 {
    let (Some(size), Some(align)) = (to_usize(size), to_usize(align)) else {
        return ptr::null_mut();
    };
    let Some(layout) = floor_layout(size, align) else {
        return ptr::null_mut();
    };
    // SAFETY: layout has non-zero size (floor_layout rejects size == 0).
    let ptr = unsafe { alloc(layout) }; // ALLOCATOR-PAIRING: GlobalAlloc
    if ptr.is_null() {
        // Fail-closed: a well-formed request that cannot be satisfied aborts
        // rather than handing back a null the caller might treat as valid.
        std::alloc::handle_alloc_error(layout);
    }
    ptr
}

/// Resize the block at `ptr` (originally `old_size` bytes, `align`-aligned) to
/// `new_size` bytes, preserving the first `min(old_size, new_size)` bytes.
///
/// Edge cases, all fail-closed:
/// - `ptr` null → behaves like `hew_alloc(new_size, align)`.
/// - `new_size == 0` → frees the block via `hew_dealloc` and returns null.
/// - invalid `(old_size, align)` or `(new_size, align)` layout, or a size/align
///   that does not fit the host `usize` → returns null without touching the
///   block (the caller checks null).
/// - reallocation failure (OOM) → aborts via [`std::alloc::handle_alloc_error`].
///
/// The returned block MUST be released with `hew_dealloc(ret, new_size, align)`.
///
/// # Safety
///
/// `ptr`, if non-null, must have come from `hew_alloc` / `hew_realloc` with the
/// same `align` and a current size of `old_size`. The old pointer is invalid
/// after a successful (non-null, non-`new_size==0`) return.
#[no_mangle]
pub unsafe extern "C" fn hew_realloc(
    ptr: *mut u8,
    old_size: u64,
    new_size: u64,
    align: u64,
) -> *mut u8 {
    if ptr.is_null() {
        // No existing block: a grow-from-nothing is a fresh allocation.
        // SAFETY: hew_alloc is safe for any (size, align); delegates fully.
        return unsafe { hew_alloc(new_size, align) };
    }
    if new_size == 0 {
        // Shrink-to-empty is a free; mirror hew_alloc(0) returning null.
        // SAFETY: ptr came from this allocator with (old_size, align) per the
        // fn contract; hew_dealloc reconstructs the matching layout.
        unsafe { hew_dealloc(ptr, old_size, align) };
        return ptr::null_mut();
    }
    let (Some(old_size), Some(new_size), Some(align)) =
        (to_usize(old_size), to_usize(new_size), to_usize(align))
    else {
        return ptr::null_mut();
    };
    // The existing block's layout must be reconstructable to realloc safely.
    let Some(old_layout) = floor_layout(old_size, align) else {
        return ptr::null_mut();
    };
    // Validate the target layout up front: realloc requires that new_size,
    // rounded up to align, not overflow isize. Reject fail-closed.
    if floor_layout(new_size, align).is_none() {
        return ptr::null_mut();
    }
    // SAFETY: ptr came from this allocator with old_layout; new_size is
    // non-zero and forms a valid layout under the same align (checked above).
    let new_ptr = unsafe { realloc(ptr, old_layout, new_size) }; // ALLOCATOR-PAIRING: GlobalAlloc
    if new_ptr.is_null() {
        // Fail-closed on OOM: the old block is still live and owned by the
        // caller (realloc leaves it intact on failure); aborting avoids both
        // a leak-on-null-return ambiguity and UB.
        std::alloc::handle_alloc_error(
            // The layout we *tried* to grow to, for the diagnostic.
            Layout::from_size_align(new_size, align).unwrap_or(old_layout),
        );
    }
    new_ptr
}

/// Release the block at `ptr` that was allocated with `(size, align)`.
///
/// No-op when `ptr` is null or `size == 0` (the symmetric partner of a null
/// `hew_alloc`). Aborts if `(size, align)` does not form a valid `Layout` (or
/// does not fit the host `usize`): such a free cannot correspond to any block
/// this allocator returned, and calling `dealloc` with a wrong layout is UB.
///
/// # Safety
///
/// `ptr`, if non-null, must have come from `hew_alloc` / `hew_realloc` with the
/// **same** `(size, align)` and must not have been freed already. After this
/// call `ptr` is dangling.
#[no_mangle]
pub unsafe extern "C" fn hew_dealloc(ptr: *mut u8, size: u64, align: u64) {
    if ptr.is_null() || size == 0 {
        return;
    }
    let layout = to_usize(size)
        .zip(to_usize(align))
        .and_then(|(size, align)| floor_layout(size, align));
    let Some(layout) = layout else {
        // A non-null pointer paired with an invalid layout is an unpaired free
        // (the matching hew_alloc would have returned null for this layout).
        // Deallocating with a wrong Layout is UB; abort fail-closed.
        std::alloc::handle_alloc_error(Layout::new::<u8>());
    };
    // SAFETY: caller guarantees ptr came from hew_alloc/hew_realloc with this
    // exact (size, align); layout reconstructs the original allocation layout.
    unsafe { dealloc(ptr, layout) }; // ALLOCATOR-PAIRING: GlobalAlloc
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Round-trip: alloc → write a real value → read it back → dealloc.
    /// Exercises a scalar (i64) and a non-trivial alignment (16).
    #[test]
    #[expect(
        clippy::cast_ptr_alignment,
        reason = "block was allocated with align == 16 >= align_of::<i64>()"
    )]
    fn alloc_roundtrips_scalar_value() {
        let align = 16u64;
        let size = std::mem::size_of::<i64>() as u64;
        // SAFETY: well-formed request; we write/read within the block and free
        // it with the same (size, align).
        unsafe {
            let p = hew_alloc(size, align);
            assert!(!p.is_null(), "alloc of {size}B@{align} must succeed");
            assert_eq!(
                p as usize % usize::try_from(align).unwrap(),
                0,
                "returned pointer must be aligned"
            );
            let typed = p.cast::<i64>();
            let sentinel: i64 = -0x0123_4567_89ab_cdef;
            typed.write(sentinel);
            let read_back = typed.read();
            assert_eq!(
                read_back, sentinel,
                "value must round-trip through the allocation"
            );
            hew_dealloc(p, size, align);
        }
    }

    /// Round-trip a multi-field struct through the raw allocation to prove the
    /// floor handles composite payloads, not just scalars.
    #[test]
    #[expect(
        clippy::cast_ptr_alignment,
        reason = "block allocated with align == align_of::<Pair>()"
    )]
    fn alloc_roundtrips_struct_value() {
        #[derive(Debug, PartialEq, Clone, Copy)]
        #[repr(C)]
        struct Pair {
            a: i64,
            b: u32,
            c: i16,
        }
        let align = std::mem::align_of::<Pair>() as u64;
        let size = std::mem::size_of::<Pair>() as u64;
        let value = Pair {
            a: -42,
            b: 0xCAFE_BABE,
            c: 1234,
        };
        // SAFETY: aligned for Pair, write/read one Pair, free with same layout.
        unsafe {
            let p = hew_alloc(size, align);
            assert!(!p.is_null());
            let typed = p.cast::<Pair>();
            typed.write(value);
            assert_eq!(typed.read(), value, "struct must round-trip");
            hew_dealloc(p, size, align);
        }
    }

    /// realloc grows the block and preserves the original bytes; the old block
    /// is not leaked (realloc frees it on success).
    #[test]
    fn realloc_grows_and_preserves_bytes() {
        let align = 8u64;
        let n = 64usize;
        // SAFETY: write a known pattern across n bytes, grow to 2n, verify the
        // first n bytes survived, then free with the new size.
        unsafe {
            let p = hew_alloc(n as u64, align);
            assert!(!p.is_null());
            for byte in 0..n {
                p.add(byte).write(u8::try_from(byte % 251).unwrap());
            }
            let grown = hew_realloc(p, n as u64, 2 * n as u64, align);
            assert!(!grown.is_null(), "grow must succeed");
            for i in 0..n {
                assert_eq!(
                    grown.add(i).read(),
                    u8::try_from(i % 251).unwrap(),
                    "byte {i} must survive the grow"
                );
            }
            hew_dealloc(grown, 2 * n as u64, align);
        }
    }

    /// realloc with a null base behaves like a fresh allocation; realloc to
    /// zero frees and returns null.
    #[test]
    #[expect(
        clippy::cast_ptr_alignment,
        reason = "block allocated with align == 8 >= align_of::<u64>()"
    )]
    fn realloc_null_and_zero_edge_cases() {
        let align = 8u64;
        // SAFETY: null base → fresh alloc; then realloc-to-zero frees it.
        unsafe {
            let fresh = hew_realloc(ptr::null_mut(), 0, 32, align);
            assert!(!fresh.is_null(), "realloc(null, _, 32) must allocate");
            fresh.cast::<u64>().write(7);
            assert_eq!(fresh.cast::<u64>().read(), 7);
            let freed = hew_realloc(fresh, 32, 0, align);
            assert!(freed.is_null(), "realloc(_, _, 0) frees and returns null");
        }
    }

    /// Invalid layout fails closed: a non-power-of-two align returns null
    /// instead of invoking UB. Zero size also returns null.
    #[test]
    fn invalid_layout_fails_closed() {
        // SAFETY: degenerate requests must return null, never allocate.
        unsafe {
            assert!(
                hew_alloc(16, 3).is_null(),
                "non-power-of-two align must return null"
            );
            assert!(hew_alloc(0, 8).is_null(), "zero size must return null");
            // Huge size that overflows isize when rounded to align.
            assert!(
                hew_alloc(u64::MAX, 16).is_null(),
                "overflowing size must return null"
            );
            // dealloc of null / zero size is a no-op (must not crash).
            hew_dealloc(ptr::null_mut(), 0, 8);
            hew_dealloc(ptr::null_mut(), 16, 8);
        }
    }

    /// Leak symmetry: a high-iteration alloc/dealloc loop completes without
    /// OOM, proving `hew_dealloc` actually returns memory to the allocator.
    /// If dealloc were a no-op (the arena's model), this would request ~6 GiB
    /// of live memory and abort.
    #[test]
    fn dealloc_returns_memory_no_leak() {
        let align = 16u64;
        let block = 64 * 1024usize; // 64 KiB
        let iters = 100_000usize; // 100k * 64 KiB ≈ 6 GiB if never freed
                                  // SAFETY: each block is written (touch the pages) then freed with the
                                  // same (size, align) before the next iteration.
        unsafe {
            for i in 0..iters {
                let p = hew_alloc(block as u64, align);
                assert!(!p.is_null());
                // Touch first and last byte so the pages are real.
                let tag = u8::try_from(i & 0xff).unwrap();
                p.write(tag);
                p.add(block - 1).write(tag);
                hew_dealloc(p, block as u64, align);
            }
        }
    }
}
