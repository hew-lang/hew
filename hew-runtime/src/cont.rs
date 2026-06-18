//! Stackless continuation substrate — the `HewCont` heap-frame + C ABI.
//!
//! This is the runtime side of Hew's unified suspension representation
//! (R326/R327, W6.007). A suspending Hew function — actor `await`, `scope`
//! join, blocking `.recv()`, generator `yield` — lowers to an LLVM
//! switched-resume coroutine (`llvm.coro.*`). CoroSplit turns that single
//! `presplitcoroutine` function into a ramp function plus `.resume` /
//! `.destroy` / `.cleanup` outlines, and stores all state that is live across
//! a suspend into one **heap frame**. The pointer `llvm.coro.begin` returns
//! IS the [`HewCont`] handle.
//!
//! This module owns two responsibilities:
//!
//! 1. **The coro frame allocator** (`hew_cont_frame_alloc` / `hew_cont_frame_free`).
//!    `llvm.coro.alloc` / `llvm.coro.free` bridge to a size-only / pointer-only
//!    allocator (the C++ `operator new`/`delete` shape). LLVM's
//!    `coro.free` only hands back the raw frame pointer, never its size, so this
//!    allocator stores the block size in an 8-byte header it prepends to every
//!    frame and reads back at free time. The bytes themselves route through the
//!    runtime's general heap allocator [`crate::mem::hew_alloc`] /
//!    [`crate::mem::hew_dealloc`] — NOT libc `malloc`, which is the wasip1
//!    requirement the W6.006 spike pinned (criterion C3). The frame is
//!    `O(live state)`, not `O(stack)`.
//!
//! 2. **The continuation handle ABI** (`hew_cont_resume` / `hew_cont_done` /
//!    `hew_cont_poll` / `hew_cont_destroy`). These are the thin runtime verbs
//!    the slice-4 poll/resume executor drives, each a direct mapping to a coro
//!    intrinsic the compiler emits into the ramp/driver:
//!      - `resume`  → `llvm.coro.resume(handle)`  — run the body to its next
//!                    suspend (or to completion).
//!      - `done`    → `llvm.coro.done(handle)`    — has the coroutine reached
//!                    its final suspend?
//!      - `destroy` → `llvm.coro.destroy(handle)` — run the single `cleanup`
//!                    outline (frees frame-owned heap values, then the frame
//!                    via `coro.free` → `hew_cont_frame_free`).
//!      - `poll`    → read the value the body published to its out-pointer slot
//!                    before suspending + `done`, packaged as a [`ResumePoll`]
//!                    tag. The value channel is an explicit out-pointer the
//!                    compiler threads through the frame, NOT the C++
//!                    `std::coroutine` promise: a non-null `coro.id` promise
//!                    pointer segfaults LLVM 22's `normalizeCoroutine`
//!                    (spike constraint 1), so Hew always passes `ptr null`
//!                    there and routes payloads through this out-pointer.
//!
//! # Ownership / teardown (single owner)
//!
//! The coroutine frame is owned by whoever holds the [`HewCont`] handle (the
//! runtime's continuation table / actor slot, once slice 4 wires it). There is
//! exactly ONE teardown owner: `hew_cont_destroy` → the `cleanup` outline.
//! Normal completion (the body running off its end through the final
//! `coro.suspend(i1 true)`) frees only the body's locals and leaves the frame
//! live for the executor to observe `done == true` and reclaim via `destroy`.
//! A completed coroutine must be destroyed exactly once; resuming a
//! final-suspended coroutine is a use-error the compiler's `trap` arm guards
//! against. This single-owner discipline is what the spike's MallocScribble +
//! `leaks --atExit` accounting proved leak-/double-free-clean (criterion C4).
//!
//! # WASM parity (CLAUDE.md §4)
//!
//! Identical source on native and `wasm32`. The frame allocator routes through
//! `crate::mem` (target-agnostic `GlobalAlloc`), and the handle verbs are pure
//! pointer plumbing — the coro intrinsics they mirror lower to in-module linear
//! memory on wasm32 with no host import (spike criterion C3: the linked module
//! imports only `fd_write`/`proc_exit`, no malloc, no asyncify, no
//! stack-switching feature). The divergence between native (M:N OS-thread pool
//! calls `resume` on any worker) and wasm (single-thread cooperative tick loop
//! calls `resume`) is an EXECUTION-MODEL difference owned by the slice-4
//! scheduler, not a representation difference: both drive this same ABI.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use std::ffi::c_void;
use std::ptr;

use crate::mem::{hew_alloc, hew_dealloc};

/// Alignment for a coroutine frame. LLVM's `CoroSplit` picks the frame's
/// natural alignment from the spilled state; 16 bytes covers every Hew scalar /
/// pointer / aggregate the frame can hold on the targets Hew supports
/// (`x86_64` / aarch64 / wasm32), so a 16-byte frame alignment is always
/// sufficient and never under-aligns a spilled value. The header reserves a
/// full 16-byte stride so the returned frame pointer keeps this alignment.
const FRAME_ALIGN: usize = 16;

/// Bytes reserved ahead of the frame for the stored block size. A full
/// [`FRAME_ALIGN`] stride (not just 8) so the pointer handed to LLVM stays
/// 16-byte aligned. The size is stored as a `u64` at the start of this header.
const FRAME_HEADER: usize = FRAME_ALIGN;

/// The outcome of polling a continuation, as a C-ABI tagged value.
///
/// `repr(C)` + `repr(i32)` pin a stable two-state discriminant the codegen /
/// executor read directly. `Pending` means the coroutine suspended with a
/// freshly published value available at the poll's out-pointer; `Ready` means
/// the coroutine reached its final suspend (`coro.done == true`) and the
/// out-pointer carries its last/return value (if any). The executor reclaims a
/// `Ready` continuation via [`hew_cont_destroy`] exactly once.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(i32)]
pub enum ResumePoll {
    /// Suspended at a non-final suspend point; a yielded/awaited value is
    /// published at the poll out-pointer. Resume again to advance.
    Pending = 0,
    /// Reached the final suspend point (`coro.done`); the continuation is
    /// complete and must be destroyed by its owner.
    Ready = 1,
}

/// Allocate a coroutine frame of `size` bytes routed through the Hew heap.
///
/// `llvm.coro.alloc` gates whether a frame needs dynamic allocation;
/// `llvm.coro.size.i64` folds to the exact frame size per coroutine, and the
/// codegen passes that constant here. The returned pointer is the frame LLVM's
/// `coro.begin` adopts — i.e. the [`HewCont`] handle.
///
/// The block is `FRAME_HEADER + size` bytes from [`crate::mem::hew_alloc`]; the
/// first 8 bytes store the *full* block size so [`hew_cont_frame_free`] can
/// reconstruct the symmetric `(size, align)` `hew_dealloc` requires — `coro.free`
/// only hands back the frame pointer, never the size. The frame pointer returned
/// to LLVM is `base + FRAME_HEADER`, keeping [`FRAME_ALIGN`] alignment.
///
/// Returns null on a degenerate `size` (zero) or when the underlying heap
/// allocation is degenerate — the executor / ramp checks null before adopting
/// the frame, the same fail-closed contract as [`crate::mem::hew_alloc`].
///
/// # Safety
///
/// Safe to call with any `size`. The returned block (if non-null) MUST be
/// released exactly once via [`hew_cont_frame_free`]; the caller (LLVM's
/// `coro.free` lowering) owns that single free edge.
#[no_mangle]
pub unsafe extern "C" fn hew_cont_frame_alloc(size: u64) -> *mut c_void {
    if size == 0 {
        return ptr::null_mut();
    }
    // A frame larger than the host `usize` cannot name a real allocation; on a
    // 32-bit target (wasm32) this fails closed rather than truncating.
    let Ok(size) = usize::try_from(size) else {
        return ptr::null_mut();
    };
    let Some(total) = size.checked_add(FRAME_HEADER) else {
        // A size so large the header push overflows cannot name a real
        // allocation; fail closed rather than wrap.
        return ptr::null_mut();
    };
    // SAFETY: hew_alloc is safe for any (size, align); FRAME_ALIGN is a power
    // of two. Returns null only for a degenerate/over-large request, which we
    // propagate.
    let base = unsafe { hew_alloc(total as u64, FRAME_ALIGN as u64) };
    if base.is_null() {
        return ptr::null_mut();
    }
    // Store the full block size in the header so the free edge can reconstruct
    // the exact (size, align) pair hew_dealloc requires. `write_unaligned` is
    // unconditionally sound (base is in fact FRAME_ALIGN-aligned, but this does
    // not rely on the static alignment of a `*mut u8`).
    // SAFETY: base points to at least FRAME_HEADER (>= 8) writable bytes that
    // hew_alloc just handed out.
    unsafe { ptr::write_unaligned(base.cast::<u64>(), total as u64) };
    crate::observe::record_coroutine_frame_alloc(size as u64);
    // SAFETY: the allocation is total = FRAME_HEADER + size bytes, so advancing
    // by FRAME_HEADER lands within the block with `size` usable bytes ahead.
    unsafe { base.add(FRAME_HEADER).cast::<c_void>() }
}

/// Release a coroutine frame previously returned by [`hew_cont_frame_alloc`].
///
/// `llvm.coro.free` produces the frame pointer (the value `coro.begin`
/// returned); this recovers the block base (`frame - FRAME_HEADER`), reads the
/// stored block size, and frees via [`crate::mem::hew_dealloc`] with the exact
/// `(size, align)` pair — the symmetric partner of the alloc.
///
/// No-op on a null frame (mirrors `hew_cont_frame_alloc` returning null and the
/// `coro.free` conditional that only frees when the frame was heap-allocated).
///
/// # Safety
///
/// `frame`, if non-null, MUST be a pointer returned by
/// [`hew_cont_frame_alloc`] and not yet freed. After this call it is dangling.
/// The 8-byte header it reads must be intact (it is, unless the frame was
/// written out of bounds — a compiler bug, not a recoverable condition).
#[no_mangle]
pub unsafe extern "C" fn hew_cont_frame_free(frame: *mut c_void) {
    if frame.is_null() {
        return;
    }
    // SAFETY: frame came from hew_cont_frame_alloc as base + FRAME_HEADER, so
    // subtracting FRAME_HEADER recovers the original block base.
    let base = unsafe { frame.cast::<u8>().sub(FRAME_HEADER) };
    // SAFETY: the header at base holds the u64 block size written at alloc.
    // `read_unaligned` matches the `write_unaligned` at alloc time.
    let total = unsafe { ptr::read_unaligned(base.cast::<u64>()) };
    crate::observe::record_coroutine_frame_free(total.saturating_sub(FRAME_HEADER as u64));
    // SAFETY: base/total/FRAME_ALIGN are exactly the (ptr, size, align) triple
    // hew_alloc returned, so this is the symmetric free hew_dealloc requires.
    unsafe { hew_dealloc(base, total, FRAME_ALIGN as u64) };
}

/// Resume a suspended continuation — `llvm.coro.resume(handle)`.
///
/// Runs the coroutine body from its current suspend point to the next suspend
/// (or to completion). After this returns, the executor should [`hew_cont_poll`]
/// (or [`hew_cont_done`]) to observe whether a new value was published or the
/// coroutine finished.
///
/// # Safety
///
/// `handle` MUST be a live continuation handle (a frame pointer from
/// `coro.begin`, i.e. [`hew_cont_frame_alloc`]'s output adopted by `coro.begin`)
/// that is currently SUSPENDED — not completed (`done`) and not destroyed.
/// Resuming a completed or destroyed continuation is undefined behaviour the
/// compiler's emission and the executor's [`ResumePoll`] discipline prevent.
#[no_mangle]
pub unsafe extern "C" fn hew_cont_resume(handle: *mut c_void) {
    if handle.is_null() {
        return;
    }
    // SAFETY: handle is a live, suspended coroutine frame per the fn contract.
    // The transmute targets the resume fn-ptr stored at frame slot 0 by
    // CoroSplit; LLVM's coro lowering guarantees that layout for any frame
    // produced by coro.begin.
    unsafe { coro_resume(handle) }
}

/// Report whether a continuation has reached its final suspend —
/// `llvm.coro.done(handle)`.
///
/// `true` once the body ran off its end through the final `coro.suspend(i1 true)`;
/// the executor then reclaims the frame via [`hew_cont_destroy`].
///
/// # Safety
///
/// `handle` MUST be a live (suspended or completed, not destroyed) continuation
/// handle from `coro.begin`.
#[no_mangle]
pub unsafe extern "C" fn hew_cont_done(handle: *mut c_void) -> bool {
    if handle.is_null() {
        // A null handle has no coroutine to be pending; treat as done so a
        // mis-driven executor reclaims rather than spins.
        return true;
    }
    // SAFETY: handle is a live coroutine frame per the fn contract.
    unsafe { coro_done(handle) }
}

/// Poll a continuation after a resume: read the published value + done state.
///
/// The coroutine publishes its yielded/awaited value to an out-pointer slot the
/// compiler threads through the frame BEFORE each suspend (the explicit value
/// channel that replaces the forbidden non-null `coro.id` promise). This reads
/// the current done state and reports it as a [`ResumePoll`]:
///   - [`ResumePoll::Pending`] — suspended at a non-final point; the value at
///     the body's out-pointer is the freshly yielded value. Resume to advance.
///   - [`ResumePoll::Ready`] — `coro.done`; the continuation is complete.
///
/// `out_value`, when non-null, is unused by the primitive itself: the body
/// writes its payload directly to its own threaded out-pointer, so this verb is
/// the done-state read the executor pairs with that out-pointer. The parameter
/// is reserved for the slice-4 executor to pass the slot it wants the published
/// value mirrored into once value-routing is wired; today it is accepted and
/// ignored so the ABI is stable across the slice boundary.
///
/// # Safety
///
/// `handle` MUST be a live continuation handle from `coro.begin`. `out_value`,
/// if non-null, must point to writable storage of the continuation's value type
/// (reserved; not written today).
#[no_mangle]
pub unsafe extern "C" fn hew_cont_poll(handle: *mut c_void, out_value: *mut c_void) -> ResumePoll {
    let _ = out_value; // reserved for slice-4 value routing; see doc comment.
                       // SAFETY: handle is a live coroutine frame per the fn contract.
    if unsafe { hew_cont_done(handle) } {
        ResumePoll::Ready
    } else {
        ResumePoll::Pending
    }
}

/// Destroy a completed (or abandoned) continuation — `llvm.coro.destroy(handle)`.
///
/// Runs the single `cleanup` outline: drops any frame-owned Hew heap values
/// still live, then frees the frame via `coro.free` → [`hew_cont_frame_free`].
/// This is the SOLE teardown owner; it must be called exactly once per
/// continuation, by the handle's owner, after observing [`ResumePoll::Ready`]
/// (or to abandon a still-suspended continuation, e.g. scope cancellation).
///
/// # Safety
///
/// `handle` MUST be a live continuation handle from `coro.begin` that has NOT
/// already been destroyed. After this call the handle (and its frame) is
/// dangling. Destroying twice is a double-free the single-owner discipline
/// prevents.
#[no_mangle]
pub unsafe extern "C" fn hew_cont_destroy(handle: *mut c_void) {
    if handle.is_null() {
        return;
    }
    // SAFETY: handle is a live, not-yet-destroyed coroutine frame per the fn
    // contract.
    unsafe { coro_destroy(handle) }
}

/// Destroy a generator's coro **companion** — the heap block a `Generator<Y, R>`
/// value points at, laid out
/// `{ ptr handle, ptr env, ptr out_drop_thunk, i8 started, i8 pending, Y out }`
/// and allocated by codegen via [`hew_cont_frame_alloc`]. The three leading
/// `ptr` fields (handle at offset 0, env at offset `ptr_width`, out-drop thunk
/// at offset `2 * ptr_width`) and the two `i8` flags that follow them are read
/// here at fixed offsets without knowing `Y`.
///
/// This is the SOLE teardown owner of a generator value, called exactly once at
/// the generator's scope-exit drop (or early drop while suspended). It:
///   1. reads the coro handle at offset 0 and [`hew_cont_destroy`]s it — the
///      coro `cleanup` outline drops every value the body still owns in its
///      frame (a value suspended mid-iteration, a cross-yield-live owned local),
///      then frees the coro frame. Exactly the single-owner destroy discipline.
///   2. reads the heap env at offset `ptr_width` and frees it via
///      [`hew_cont_frame_free`] (null for a capture-free generator → no-op). The
///      env holds only plain-copyable captures (`gen_env_capture_admissible`
///      rejects owned/opaque), so a flat free with no per-field drop is sound.
///   3. **typed-drops the `out` value IFF it is a live, UN-consumed owned value.**
///      A `yield` is lowered as a MOVE: the body publishes the value into the
///      companion `out` slot and never drops it, so until a `.next()` reads it
///      out (moving it into the consumer's `Option<Y>` payload) the companion is
///      the SOLE owner of that value. Codegen sets the `pending` flag to 1 when
///      such a value is live in `out`, and clears it to 0 the moment a `.next()`
///      consumes it. A generator constructed and dropped before its first
///      `.next()` (or otherwise dropped while a yielded value is pending) would
///      LEAK that owned `out` value if we did nothing — destroy must drop it.
///      When `pending != 0` and the codegen planted a non-null `out_drop_thunk`
///      (null when `Y` is `BitCopy` — nothing to drop), this calls
///      `out_drop_thunk(companion)`; the per-`Y` thunk GEPs to the `out` field
///      and runs the typed drop for `Y` exactly once. When `pending == 0` the
///      `out` slot is either a stale BIT-COPY of an already-consumed value (the
///      consumer owns the moved-out copy) or never-written, so it is NOT dropped
///      — doing so would double-free the consumer's value.
///   4. frees the companion block via [`hew_cont_frame_free`] (the symmetric
///      partner of the `hew_cont_frame_alloc` codegen used).
///
/// Null-safe (a never-constructed / already-dropped generator). After this the
/// companion, its env, and its coro frame are dangling.
///
/// # Safety
///
/// `companion`, if non-null, MUST be a generator companion block from
/// `hew_cont_frame_alloc` whose offset-0 word is a live (or null) coro handle,
/// offset-`ptr_width` word is a live (or null) env block (also from
/// `hew_cont_frame_alloc`), offset-`2*ptr_width` word is a `void(ptr)` typed
/// out-drop thunk (or null), followed by the `started` / `pending` flag bytes —
/// not yet destroyed. Called exactly once per generator value.
#[no_mangle]
pub unsafe extern "C" fn hew_gen_coro_destroy(companion: *mut c_void) {
    if companion.is_null() {
        return;
    }
    // SAFETY: offset 0 of the companion is the coro handle (a `*mut c_void`),
    // written by the `MakeGenerator` codegen. `read` is aligned (the companion
    // is FRAME_ALIGN-aligned and the handle is the first field).
    let handle = unsafe { ptr::read(companion.cast::<*mut c_void>()) };
    // SAFETY: handle is the generator's coro frame handle (or null); destroy is
    // the single teardown owner and runs the cleanup outline.
    unsafe { hew_cont_destroy(handle) };
    // The env pointer is the SECOND field — one pointer-width past the handle.
    // Use the target pointer width (4 bytes on wasm32, 8 on native) so the
    // offset matches the companion layout the codegen emits on this target.
    let ptr_width = core::mem::size_of::<*mut c_void>();
    // SAFETY: offset ptr_width (one pointer past handle) is within the companion
    // block (it has at least the three leading pointer fields), so advancing the
    // base by one pointer width lands at the env-pointer field.
    let env_slot = unsafe { companion.cast::<u8>().add(ptr_width) };
    // SAFETY: the env field is a `*mut c_void` written by the MakeGenerator
    // codegen (or null). `read_unaligned` is sound regardless of the static
    // alignment of the byte-offset cast (the field is in fact pointer-aligned).
    let env = unsafe { ptr::read_unaligned(env_slot.cast::<*mut c_void>()) };
    // SAFETY: env came from hew_cont_frame_alloc (or is null); symmetric free.
    unsafe { hew_cont_frame_free(env) };

    // Typed-drop the `out` value IFF it is a live, un-consumed owned value.
    // `pending` is the byte immediately after the three leading pointer fields
    // and the `started` byte: offset `3 * ptr_width + 1`. The codegen lays the
    // companion out so this byte holds 1 while an owned yielded value is live in
    // `out` and 0 once a `.next()` has moved it out (or for a value the body
    // never yielded). The `out_drop_thunk` is the THIRD pointer field
    // (offset `2 * ptr_width`), null when `Y` is `BitCopy` (nothing to drop).
    // SAFETY: offset 3*ptr_width+1 is within the companion (after handle, env,
    // thunk pointers and the `started` byte), so advancing the base lands at the
    // `pending` flag byte.
    let pending_slot = unsafe { companion.cast::<u8>().add(ptr_width.saturating_mul(3) + 1) };
    // SAFETY: pending_slot points at the in-bounds `pending` flag byte the
    // MakeGenerator codegen wrote; a single-byte `read` is sound.
    let pending = unsafe { ptr::read(pending_slot) };
    if pending != 0 {
        // SAFETY: offset 2*ptr_width is the out-drop thunk pointer field, within
        // the three leading pointer fields the codegen always emits.
        let thunk_slot = unsafe { companion.cast::<u8>().add(ptr_width.saturating_mul(2)) };
        // SAFETY: the thunk field is a `void(ptr)` fn-ptr written by the
        // MakeGenerator codegen (or null). `read_unaligned` is sound regardless
        // of the byte-offset cast's static alignment (the field is pointer-aligned).
        let thunk = unsafe {
            ptr::read_unaligned(thunk_slot.cast::<Option<unsafe extern "C" fn(*mut c_void)>>())
        };
        if let Some(thunk) = thunk {
            // SAFETY: the thunk is the codegen-synthesised per-`Y` out-drop
            // thunk; it GEPs to the `out` field of THIS companion and runs the
            // typed drop for the un-consumed owned value exactly once. The value
            // is solely-owned (a moved-but-never-read yield), so this is the only
            // drop edge — no double-free.
            unsafe { thunk(companion) };
        }
    }

    // SAFETY: companion came from hew_cont_frame_alloc; this is its symmetric
    // free (reads the size header hew_cont_frame_alloc stored).
    unsafe { hew_cont_frame_free(companion) };
}

// ── coro-frame fn-pointer dispatch ────────────────────────────────────────
//
// CoroSplit stores the `.resume` and `.destroy` fn pointers at the start of
// every coroutine frame: slot 0 is the resume fn, slot 1 is the destroy fn
// (the destroy path also runs `cleanup`). `llvm.coro.resume` / `coro.destroy`
// / `coro.done` are themselves lowered by LLVM to loads of these slots + an
// indirect call (resume/destroy) or a null-check of the resume slot (done).
//
// The runtime drives a continuation through these same frame slots so the
// handle ABI does not depend on the coroutine being a C++ `std::coroutine`
// (it never is — Hew passes `ptr null` for the promise). The layout LLVM
// commits to for a switched-resume frame is:
//   { ptr resume_fn, ptr destroy_fn, ... spilled state ... }
// and `coro.done(h)` is `load ptr, h /*slot 0*/; icmp eq ptr null`.

/// Frame prefix `CoroSplit` writes: resume fn-ptr, destroy fn-ptr.
#[repr(C)]
struct CoroFramePrefix {
    resume: Option<unsafe extern "C" fn(*mut c_void)>,
    destroy: Option<unsafe extern "C" fn(*mut c_void)>,
}

/// `llvm.coro.resume(handle)`: indirect-call the frame's resume fn-ptr.
///
/// # Safety
/// `handle` is a live, suspended coroutine frame (slot 0 = resume fn).
#[inline]
unsafe fn coro_resume(handle: *mut c_void) {
    let prefix = handle.cast::<CoroFramePrefix>();
    // SAFETY: handle is a live suspended frame; slot 0 holds the resume fn-ptr
    // CoroSplit stored, non-null while suspended.
    if let Some(resume) = unsafe { (*prefix).resume } {
        // SAFETY: resume is the CoroSplit-emitted `.resume` outline; calling it
        // with the frame pointer is exactly what `llvm.coro.resume` lowers to.
        unsafe { resume(handle) }
    }
}

/// `llvm.coro.destroy(handle)`: indirect-call the frame's destroy fn-ptr.
///
/// # Safety
/// `handle` is a live, not-yet-destroyed coroutine frame (slot 1 = destroy fn).
#[inline]
unsafe fn coro_destroy(handle: *mut c_void) {
    let prefix = handle.cast::<CoroFramePrefix>();
    // SAFETY: handle is a live frame; slot 1 holds the destroy fn-ptr CoroSplit
    // stored.
    if let Some(destroy) = unsafe { (*prefix).destroy } {
        // SAFETY: destroy is the CoroSplit-emitted `.destroy` outline (which
        // runs `cleanup`); calling it with the frame pointer is exactly what
        // `llvm.coro.destroy` lowers to.
        unsafe { destroy(handle) }
    }
}

/// `llvm.coro.done(handle)`: a coroutine is done when its resume slot is null.
///
/// `CoroSplit` nulls the resume fn-ptr (slot 0) when the coroutine reaches its
/// final suspend, which is exactly the test `llvm.coro.done` performs.
///
/// # Safety
/// `handle` is a live (suspended or completed) coroutine frame.
#[inline]
unsafe fn coro_done(handle: *mut c_void) -> bool {
    let prefix = handle.cast::<CoroFramePrefix>();
    // SAFETY: handle is a live frame; slot 0 is the resume fn-ptr, nulled at
    // the final suspend.
    unsafe { (*prefix).resume.is_none() }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The frame allocator round-trips: a non-zero request returns a usable,
    /// aligned block whose stored size lets the symmetric free reconstruct the
    /// layout. Exercises the header push/pop and the (ptr, size, align)
    /// symmetry the spike's accounting depends on.
    #[test]
    fn frame_alloc_round_trips_aligned_and_frees_symmetric() {
        // SAFETY: round-trip a well-formed request through the FFI pair.
        unsafe {
            let frame = hew_cont_frame_alloc(56);
            assert!(!frame.is_null(), "non-zero frame request must allocate");
            assert_eq!(
                frame as usize % FRAME_ALIGN,
                0,
                "returned frame must keep FRAME_ALIGN alignment"
            );
            // The full block (header + body) is usable: write across it.
            ptr::write_bytes(frame.cast::<u8>(), 0xAB, 56);
            // Symmetric free reads the stored size; a leak/double-free here
            // would surface under MallocScribble in the exec test.
            hew_cont_frame_free(frame);
        }
    }

    /// A zero-size frame request fails closed (null), mirroring `hew_alloc(0)`.
    #[test]
    fn frame_alloc_zero_size_returns_null() {
        // SAFETY: degenerate request; must not allocate.
        let frame = unsafe { hew_cont_frame_alloc(0) };
        assert!(frame.is_null(), "zero-size frame request must return null");
    }

    /// Freeing a null frame is a no-op (the `coro.free` conditional path when
    /// the frame was never heap-allocated). Must not crash.
    #[test]
    fn frame_free_null_is_noop() {
        // SAFETY: null free is a documented no-op.
        unsafe { hew_cont_frame_free(ptr::null_mut()) };
    }

    /// `done`/`destroy`/`resume`/`poll` on a null handle are fail-closed: done
    /// reports true (reclaim, don't spin), the others no-op, poll reports Ready.
    #[test]
    fn null_handle_verbs_fail_closed() {
        // SAFETY: every verb documents null as a fail-closed no-op / done.
        unsafe {
            assert!(hew_cont_done(ptr::null_mut()), "null handle reports done");
            assert_eq!(
                hew_cont_poll(ptr::null_mut(), ptr::null_mut()),
                ResumePoll::Ready,
                "polling a null handle reports Ready"
            );
            hew_cont_resume(ptr::null_mut());
            hew_cont_destroy(ptr::null_mut());
        }
    }

    /// `coro_done` reads slot 0: a frame with a live resume fn-ptr is not done;
    /// nulling it (what `CoroSplit` does at the final suspend) flips it to done.
    /// Exercises the frame-prefix layout the handle ABI commits to.
    #[test]
    fn coro_done_tracks_resume_slot_nulling() {
        unsafe extern "C" fn noop_resume(_: *mut c_void) {}
        let mut prefix = CoroFramePrefix {
            resume: Some(noop_resume),
            destroy: None,
        };
        let handle = (&raw mut prefix).cast::<c_void>();
        // SAFETY: handle points to a live CoroFramePrefix with a non-null
        // resume slot, then we null it to model the final-suspend transition.
        unsafe {
            assert!(!coro_done(handle), "live resume slot => not done");
            assert!(!hew_cont_done(handle), "ABI mirror agrees: not done");
            (*handle.cast::<CoroFramePrefix>()).resume = None;
            assert!(coro_done(handle), "nulled resume slot => done");
            assert!(hew_cont_done(handle), "ABI mirror agrees: done");
            assert_eq!(
                hew_cont_poll(handle, ptr::null_mut()),
                ResumePoll::Ready,
                "poll reports Ready once done"
            );
        }
    }

    // ── Generator companion typed out-drop dispatch (the leak fix) ─────────────
    //
    // The companion the codegen emits is laid out
    // `{ ptr handle, ptr env, ptr out_drop_thunk, i8 started, i8 pending, Y out }`.
    // `hew_gen_coro_destroy` reads the three leading pointers + the two flag
    // bytes at fixed (pointer-width-derived) offsets and, when `pending != 0`
    // and the thunk is non-null, calls `thunk(companion)` to typed-drop the
    // un-consumed owned `out` value exactly once. These tests model that block
    // directly (no LLVM) and assert the dispatch fires exactly when pending,
    // never when consumed, and exactly once.

    use std::sync::atomic::{AtomicU32, Ordering};

    // Separate counters per test so the dispatch tests stay isolated under
    // nextest's parallel execution (a shared counter would race).
    static PENDING_DROP_CALLS: AtomicU32 = AtomicU32::new(0);
    static CONSUMED_DROP_CALLS: AtomicU32 = AtomicU32::new(0);

    /// Stand-in for a codegen-emitted typed out-drop thunk: bumps the
    /// pending-path counter. A real thunk GEPs to the companion's `out` field and
    /// runs `Y`'s drop; the dispatch contract this pins is "called once iff pending".
    unsafe extern "C" fn pending_counting_thunk(_companion: *mut c_void) {
        PENDING_DROP_CALLS.fetch_add(1, Ordering::SeqCst);
    }

    /// Stand-in thunk for the consumed-path test (bumps a distinct counter).
    unsafe extern "C" fn consumed_counting_thunk(_companion: *mut c_void) {
        CONSUMED_DROP_CALLS.fetch_add(1, Ordering::SeqCst);
    }

    /// Build a minimal companion block matching the codegen layout, with a null
    /// coro handle + null env (so handle/env teardown are no-ops) and the given
    /// `pending` flag + out-drop thunk. Returns the companion pointer (owned by
    /// `hew_cont_frame_alloc`, freed by `hew_gen_coro_destroy`).
    unsafe fn make_companion(
        pending: u8,
        thunk: Option<unsafe extern "C" fn(*mut c_void)>,
    ) -> *mut c_void {
        let ptr_width = core::mem::size_of::<*mut c_void>();
        // Three pointers + two flag bytes is the only region the runtime reads;
        // size the block generously past that so the flag/thunk writes are in
        // bounds (the real `out` field follows but the runtime never reads it).
        let size = (ptr_width * 4) as u64;
        // SAFETY: hew_cont_frame_alloc is safe for any size and returns a block
        // freed by hew_gen_coro_destroy below.
        let companion = unsafe { hew_cont_frame_alloc(size) };
        assert!(!companion.is_null());
        // SAFETY: every write below targets an in-bounds field of the companion
        // block just allocated (handle @0, env @ptr_width, thunk @2*ptr_width,
        // started @3*ptr_width, pending @3*ptr_width+1), matching the codegen
        // layout the runtime destroy reads.
        unsafe {
            ptr::write(companion.cast::<*mut c_void>(), ptr::null_mut());
            ptr::write_unaligned(
                companion.cast::<u8>().add(ptr_width).cast::<*mut c_void>(),
                ptr::null_mut(),
            );
            // out_drop_thunk @2*ptr_width.
            ptr::write_unaligned(
                companion
                    .cast::<u8>()
                    .add(ptr_width * 2)
                    .cast::<Option<unsafe extern "C" fn(*mut c_void)>>(),
                thunk,
            );
            // started @3*ptr_width (irrelevant to destroy), pending @3*ptr_width+1.
            ptr::write(companion.cast::<u8>().add(ptr_width * 3), 0u8);
            ptr::write(companion.cast::<u8>().add(ptr_width * 3 + 1), pending);
        }
        companion
    }

    /// A companion dropped with `pending == 1` and a non-null thunk runs the
    /// typed out-drop EXACTLY once — the leak fix for an un-consumed owned yield.
    #[test]
    fn destroy_runs_out_drop_thunk_exactly_once_when_pending() {
        PENDING_DROP_CALLS.store(0, Ordering::SeqCst);
        // SAFETY: companion is a well-formed block from make_companion; destroy
        // is its sole teardown owner.
        unsafe {
            let companion = make_companion(1, Some(pending_counting_thunk));
            hew_gen_coro_destroy(companion);
        }
        assert_eq!(
            PENDING_DROP_CALLS.load(Ordering::SeqCst),
            1,
            "a pending un-consumed owned yield must be typed-dropped exactly once"
        );
    }

    /// A companion dropped with `pending == 0` (the value was consumed by a
    /// `.next()`, or never yielded) must NOT run the thunk — dropping a consumed
    /// value would double-free the copy the consumer now owns.
    #[test]
    fn destroy_skips_out_drop_thunk_when_not_pending() {
        CONSUMED_DROP_CALLS.store(0, Ordering::SeqCst);
        // SAFETY: companion is a well-formed block from make_companion.
        unsafe {
            let companion = make_companion(0, Some(consumed_counting_thunk));
            hew_gen_coro_destroy(companion);
        }
        assert_eq!(
            CONSUMED_DROP_CALLS.load(Ordering::SeqCst),
            0,
            "a consumed (pending == 0) out value must NOT be re-dropped"
        );
    }

    /// A `BitCopy` `Y` plants a null thunk; destroy with pending set must be a
    /// no-op on the drop side (nothing to free) and not deref a null thunk.
    #[test]
    fn destroy_null_thunk_pending_is_noop_drop() {
        // SAFETY: companion has a null thunk; destroy must skip the call.
        unsafe {
            let companion = make_companion(1, None);
            hew_gen_coro_destroy(companion);
        }
        // No crash, no double-free; reaching here is the assertion.
    }
}
