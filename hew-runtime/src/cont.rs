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
//! 1. **The coro frame allocators** (`hew_cont_frame_alloc`,
//!    `hew_cont_frame_alloc_tracked`, and `hew_cont_frame_free`).
//!    `llvm.coro.alloc` / `llvm.coro.free` bridge to a size-only / pointer-only
//!    allocator (the C++ `operator new`/`delete` shape). LLVM's
//!    `coro.free` only hands back the raw frame pointer, never its size, so this
//!    allocator stores the block size and a tracked-frame marker in a 16-byte
//!    header it prepends to every frame and reads back at free time. Coroutine
//!    ramps use the tracked sibling so native crash recovery can identify only
//!    allocations known to be live on the killed synchronous call stack;
//!    generator companions remain untracked. The bytes themselves route through the
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
//! After a ramp hands the coroutine frame to its caller, it is owned by whoever
//! holds the [`HewCont`] handle (the runtime's continuation table / actor slot,
//! once slice 4 wires it). There is exactly ONE ordinary teardown owner:
//! `hew_cont_destroy` → the `cleanup` outline.
//! Normal completion (the body running off its end through the final
//! `coro.suspend(i1 true)`) frees only the body's locals and leaves the frame
//! live for the executor to observe `done == true` and reclaim via `destroy`.
//! A completed coroutine must be destroyed exactly once; resuming a
//! final-suspended coroutine is a use-error the compiler's `trap` arm guards
//! against. A native trap that kills a ramp/resume before handoff is the narrow
//! exception: crash recovery raw-frees only positively tracked active frames,
//! never invokes `coro.destroy` on a running frame, and excludes the
//! scheduler-owned resumed root so its existing destroy authority remains
//! unique. This single-owner discipline is what the spike's MallocScribble +
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

use std::cell::RefCell;
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

/// Marker stored in the second word of a coroutine frame header.
///
/// Only allocations made by [`hew_cont_frame_alloc_tracked`] carry this marker.
/// Generator companions and environments continue to use
/// [`hew_cont_frame_alloc`] and remain deliberately outside crash-frame
/// reclamation: they require typed teardown that a raw crash unwind cannot
/// provide.
const TRACKED_COROUTINE_FRAME_MAGIC: u64 = 0x4845_5743_4f52_4f31;

thread_local! {
    /// Coroutine frames synchronously executing on this worker thread.
    ///
    /// A tracked ramp allocation pushes immediately. A normal ramp return hands
    /// the frame to its caller and pops it. `hew_cont_resume` brackets the
    /// CoroSplit resume outline with the same enter/leave pair. A signal
    /// longjmp skips the normal pop and leaves the positively tracked frames
    /// here for scheduler crash recovery to reclaim in LIFO order.
    static ACTIVE_COROUTINE_FRAMES: RefCell<Vec<ActiveCoroutineFrame>> =
        const { RefCell::new(Vec::new()) };
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ActiveCoroutinePhase {
    /// A newly allocated coroutine ramp is executing before returning a handle.
    Ramp,
    /// `hew_cont_resume` is driving a `CoroSplit` `.resume` outline.
    Resume,
}

#[derive(Clone, Copy, Debug)]
struct ActiveCoroutineFrame {
    frame: *mut c_void,
    phase: ActiveCoroutinePhase,
}

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
    // SAFETY: this is the untracked allocation entry point; the shared helper
    // accepts any size and returns a frame requiring one matching free.
    unsafe { allocate_frame(size, false) }
}

/// Allocate and activate a coroutine frame for a `CoroSplit` ramp.
///
/// Unlike [`hew_cont_frame_alloc`], this entry point marks the allocation as a
/// real coroutine frame and pushes it onto the current thread's active-frame
/// stack. Generated coroutine prologues use only this sibling. A normal ramp
/// return calls [`hew_cont_frame_handoff`] immediately before returning the
/// handle; a trap/longjmp skips that handoff, leaving the frame positively
/// identified for crash recovery.
///
/// # Safety
///
/// Same allocation contract as [`hew_cont_frame_alloc`]. The returned block,
/// if non-null, must be handed off or reclaimed exactly once.
#[no_mangle]
pub unsafe extern "C" fn hew_cont_frame_alloc_tracked(size: u64) -> *mut c_void {
    // SAFETY: the shared helper accepts any size and records successful tracked
    // allocations on the current active stack.
    unsafe { allocate_frame(size, true) }
}

unsafe fn allocate_frame(size: u64, tracked: bool) -> *mut c_void {
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
    // Store the full block size and the optional tracked-coroutine marker in
    // the header. `write_unaligned` is unconditionally sound (base is in fact
    // FRAME_ALIGN-aligned, but this does not rely on the static alignment of a
    // `*mut u8`).
    // SAFETY: base points to at least FRAME_HEADER (>= 8) writable bytes that
    // hew_alloc just handed out.
    unsafe {
        ptr::write_unaligned(base.cast::<u64>(), total as u64);
        ptr::write_unaligned(
            base.add(size_of::<u64>()).cast::<u64>(),
            if tracked {
                TRACKED_COROUTINE_FRAME_MAGIC
            } else {
                0
            },
        );
    }
    crate::observe::record_coroutine_frame_alloc(size as u64);
    // SAFETY: the allocation is total = FRAME_HEADER + size bytes, so advancing
    // by FRAME_HEADER lands within the block with `size` usable bytes ahead.
    let frame = unsafe { base.add(FRAME_HEADER).cast::<c_void>() };
    if tracked {
        ACTIVE_COROUTINE_FRAMES.with(|active| {
            active.borrow_mut().push(ActiveCoroutineFrame {
                frame,
                phase: ActiveCoroutinePhase::Ramp,
            });
        });
    }
    frame
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
    remove_matching_active_frame(frame);
    // SAFETY: caller guarantees `frame` is a live allocation from one of the
    // frame allocator siblings.
    unsafe { free_frame_allocation(frame) };
}

unsafe fn free_frame_allocation(frame: *mut c_void) {
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

unsafe fn frame_is_tracked(frame: *mut c_void) -> bool {
    if frame.is_null() {
        return false;
    }
    // SAFETY: callers supply a live frame allocation. Its header begins one
    // FRAME_HEADER stride before the public frame pointer.
    let base = unsafe { frame.cast::<u8>().sub(FRAME_HEADER) };
    // SAFETY: the marker occupies the second u64 word in the 16-byte header.
    unsafe {
        ptr::read_unaligned(base.add(size_of::<u64>()).cast::<u64>())
            == TRACKED_COROUTINE_FRAME_MAGIC
    }
}

/// Return the currently executing tracked coroutine frame when `slot..slot+size`
/// lies wholly within its payload.
///
/// The frame pointer is sourced from the active-frame TLS rather than from the
/// caller. That makes the header reads below safe and prevents crash-cleanup
/// registration from acquiring authority over an arbitrary allocation.
pub(crate) fn active_top_frame_containing(slot: *mut c_void, size: u64) -> Option<*mut c_void> {
    ACTIVE_COROUTINE_FRAMES.with(|active| {
        let frame = active.borrow().last()?.frame;
        tracked_frame_contains_range(frame, slot, size).then_some(frame)
    })
}

/// Check that a positively tracked active frame still contains a registered
/// cleanup range. Crash unwind uses this as a debug-time invariant before
/// invoking a typed drop thunk.
pub(crate) fn active_frame_contains_range(
    frame: *mut c_void,
    slot: *mut c_void,
    size: u64,
) -> bool {
    let is_active = ACTIVE_COROUTINE_FRAMES
        .with(|active| active.borrow().iter().any(|record| record.frame == frame));
    is_active && tracked_frame_contains_range(frame, slot, size)
}

fn tracked_frame_contains_range(frame: *mut c_void, slot: *mut c_void, size: u64) -> bool {
    if slot.is_null() {
        return false;
    }
    let Ok(size) = usize::try_from(size) else {
        return false;
    };
    // SAFETY: callers source `frame` from the live active-frame stack. The
    // marker is rechecked so only positively tracked coroutine allocations
    // qualify.
    if !unsafe { frame_is_tracked(frame) } {
        return false;
    }

    // SAFETY: a tracked frame has the allocator header immediately before its
    // public payload pointer.
    let base = unsafe { frame.cast::<u8>().sub(FRAME_HEADER) };
    // SAFETY: the first header word is the allocation size written by
    // `allocate_frame`.
    let total = unsafe { ptr::read_unaligned(base.cast::<u64>()) };
    let Ok(total) = usize::try_from(total) else {
        return false;
    };
    let Some(payload_size) = total.checked_sub(FRAME_HEADER) else {
        return false;
    };
    let frame_start = frame as usize;
    let Some(frame_end) = frame_start.checked_add(payload_size) else {
        return false;
    };
    let slot_start = slot as usize;
    let Some(slot_end) = slot_start.checked_add(size) else {
        return false;
    };
    slot_start >= frame_start && slot_end <= frame_end
}

fn active_coroutine_enter(frame: *mut c_void) -> bool {
    // SAFETY: this helper is called only with a live continuation handle. The
    // marker gates admission so untracked companion/environment allocations can
    // never enter the raw crash-reclamation authority.
    if frame.is_null() || !unsafe { frame_is_tracked(frame) } {
        return false;
    }
    ACTIVE_COROUTINE_FRAMES.with(|active| {
        active.borrow_mut().push(ActiveCoroutineFrame {
            frame,
            phase: ActiveCoroutinePhase::Resume,
        });
    });
    true
}

fn active_coroutine_leave(frame: *mut c_void, phase: ActiveCoroutinePhase) -> bool {
    if frame.is_null() {
        return false;
    }
    ACTIVE_COROUTINE_FRAMES.with(|active| {
        let mut active = active.borrow_mut();
        if !active
            .last()
            .is_some_and(|record| record.frame == frame && record.phase == phase)
        {
            return false;
        }
        active.pop();
        true
    })
}

fn remove_matching_active_frame(frame: *mut c_void) -> bool {
    if frame.is_null() {
        return false;
    }
    ACTIVE_COROUTINE_FRAMES.with(|active| {
        let mut active = active.borrow_mut();
        let Some(index) = active
            .iter()
            .rposition(|candidate| candidate.frame == frame)
        else {
            return false;
        };
        active.remove(index);
        true
    })
}

/// Transfer a normally-returned ramp frame from the active TLS stack to its
/// caller.
///
/// The handoff is intentionally pointer-only and does not inspect the frame:
/// `CoroSplit` may retain the shared return block in cleanup outlines after the
/// frame has already been freed. In that outline the pointer is dangling but
/// cannot match a live active record, so the call is a safe no-op.
#[no_mangle]
pub extern "C" fn hew_cont_frame_handoff(frame: *mut c_void) {
    // CoroSplit may clone the presplit shared return block into a `.resume`
    // outline. The phase check makes that cloned call a no-op: only a newly
    // allocated ramp record can be handed off; `hew_cont_resume` owns the
    // matching Resume-phase leave.
    let _ = active_coroutine_leave(frame, ActiveCoroutinePhase::Ramp);
}

#[cfg(any(not(target_arch = "wasm32"), test))]
unsafe fn drain_active_coroutine_frames_excluding(
    excluded: *mut c_void,
    mut reclaim: impl FnMut(*mut c_void),
) -> usize {
    ACTIVE_COROUTINE_FRAMES.with(|active| {
        let mut active = active.borrow_mut();
        let mut retained_excluded = None;
        let mut reclaimed = 0;
        while let Some(record) = active.pop() {
            let frame = record.frame;
            if !excluded.is_null() && frame == excluded && retained_excluded.is_none() {
                retained_excluded = Some(record);
                continue;
            }
            // SAFETY: only the tracked allocator and tracked resume-enter path
            // can populate this stack. Re-check the header marker before raw
            // reclamation so corrupted/mismatched records fail closed.
            if unsafe { frame_is_tracked(frame) } {
                reclaim(frame);
                reclaimed += 1;
            }
        }
        if let Some(record) = retained_excluded {
            // The pop loop preserved the Vec's capacity, so restoring the one
            // scheduler-owned root does not allocate on the crash path.
            active.push(record);
        }
        reclaimed
    })
}

/// Raw-reclaim every crash-abandoned active coroutine except one optional
/// scheduler-owned root.
///
/// Frames are drained in LIFO order. This never calls `coro.destroy`: each
/// reclaimed frame was RUNNING when signal recovery killed its native stack,
/// so its suspend cleanup outline is not legal to re-enter. The operation frees
/// only positively tracked coroutine allocations and therefore makes no claim
/// to run typed destructors for arbitrary frame-owned values.
///
/// `excluded` is used by resumed-handler recovery: the actor slot remains the
/// sole owner of that root allocation, and `abandon_resuming_after_crash` frees
/// it after nested frames have been drained. Typed field drops run separately
/// before this raw drain.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) unsafe fn reclaim_active_coroutine_frames_excluding(excluded: *mut c_void) -> usize {
    // SAFETY: scheduler calls this only after longjmp/unwind has killed every
    // non-excluded active frame on this thread.
    unsafe {
        drain_active_coroutine_frames_excluding(excluded, |frame| {
            // SAFETY: the drain admitted only a positively tracked live frame
            // whose synchronous execution was abandoned by crash recovery.
            free_frame_allocation(frame);
        })
    }
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
    let tracked = active_coroutine_enter(handle);
    // SAFETY: handle is a live, suspended coroutine frame per the fn contract.
    // The transmute targets the resume fn-ptr stored at frame slot 0 by
    // CoroSplit; LLVM's coro lowering guarantees that layout for any frame
    // produced by coro.begin.
    unsafe { coro_resume(handle) };
    if tracked {
        debug_assert!(
            active_coroutine_leave(handle, ActiveCoroutinePhase::Resume),
            "tracked coroutine resume returned with a mismatched active-frame stack"
        );
    }
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
/// Resumes the coroutine at its `coro.suspend` cleanup edge (case 1), running
/// the coroutine's OWN cleanup funclet before the frame is freed. Codegen emits
/// that funclet, so the drops live in the compiled coroutine, not in this
/// runtime shim. For a continuation abandoned WHILE SUSPENDED the funclet runs,
/// in order: (a) the suspend kind's per-park bookkeeping (slot cancel/free,
/// observer deregister, deadline cancel), then (b) the drop of every frame-owned
/// Hew heap value live across the park — the suspend exit's elaborated drop plan
/// (#2395, the previously-unimplemented "cleanup outline") — and finally the
/// shared `coro.cleanup` frees the frame via `coro.free` →
/// [`hew_cont_frame_free`]. A continuation destroyed AFTER completing already
/// ran its return-path drops; its value-free final suspend frees the frame only.
/// This is the SOLE teardown owner; it must be called exactly once per
/// continuation, by the handle's owner, after observing [`ResumePoll::Ready`]
/// (or to abandon a still-suspended continuation, e.g. scope cancellation /
/// supervisor stop).
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
/// `{ ptr handle, ptr env, ptr env_drop_thunk, ptr out_drop_thunk,
///    i8 started, i8 pending, Y out }`
/// and allocated by codegen via [`hew_cont_frame_alloc`]. The four leading
/// `ptr` fields (handle at offset 0, env at offset `ptr_width`, env-drop thunk
/// at offset `2 * ptr_width`, out-drop thunk at offset `3 * ptr_width`) and the
/// two `i8` flags that follow them are read
/// here at fixed offsets without knowing `Y`.
///
/// This is the SOLE teardown owner of a generator value, called exactly once at
/// the generator's scope-exit drop (or early drop while suspended). It:
///   1. reads the coro handle at offset 0 and [`hew_cont_destroy`]s it — the
///      coro `cleanup` funclet (codegen's yield abandon edge, #2395) drops every
///      value the body still owns in its frame (a cross-yield-live owned local),
///      then frees the coro frame. Exactly the single-owner destroy discipline.
///      The just-yielded value in `out` is NOT among them (it is a MOVE into
///      `out`, `Consumed` at the yield, so excluded from that plan); it is
///      dropped by step 3 below, keeping the companion its sole owner.
///   2. reads the heap env and env-drop thunk. When the thunk is non-null it
///      typed-drops the env payload fields, then frees the env allocation via
///      [`hew_cont_frame_free`] (null for a capture-free generator → no-op).
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
/// `hew_cont_frame_alloc`), offset-`2*ptr_width` word is a `void(ptr env)` typed
/// env-drop thunk (or null), offset-`3*ptr_width` word is a
/// `void(ptr companion)` typed out-drop thunk (or null), followed by the
/// `started` / `pending` flag bytes — not yet destroyed. Called exactly once
/// per generator value.
#[no_mangle]
#[cfg(not(target_arch = "wasm32"))]
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
    // block (it has at least the four leading pointer fields), so advancing the
    // base by one pointer width lands at the env-pointer field.
    let env_slot = unsafe { companion.cast::<u8>().add(ptr_width) };
    // SAFETY: the env field is a `*mut c_void` written by the MakeGenerator
    // codegen (or null). `read_unaligned` is sound regardless of the static
    // alignment of the byte-offset cast (the field is in fact pointer-aligned).
    let env = unsafe { ptr::read_unaligned(env_slot.cast::<*mut c_void>()) };
    // SAFETY: offset 2*ptr_width is the env-drop thunk pointer field.
    let env_thunk_slot = unsafe { companion.cast::<u8>().add(ptr_width.saturating_mul(2)) };
    // SAFETY: the thunk field is a `void(ptr)` fn-ptr written by codegen (or null).
    let env_thunk = unsafe {
        ptr::read_unaligned(env_thunk_slot.cast::<Option<unsafe extern "C" fn(*mut c_void)>>())
    };
    if let Some(thunk) = env_thunk {
        // SAFETY: the codegen-synthesised thunk expects this generator's env
        // allocation and drops payload fields without freeing the allocation.
        unsafe { thunk(env) };
    }
    // SAFETY: env came from hew_cont_frame_alloc (or is null); symmetric free.
    unsafe { hew_cont_frame_free(env) };

    // Typed-drop the `out` value IFF it is a live, un-consumed owned value.
    // `pending` is the byte immediately after the four leading pointer fields
    // and the `started` byte: offset `4 * ptr_width + 1`. The codegen lays the
    // companion out so this byte holds 1 while an owned yielded value is live in
    // `out` and 0 once a `.next()` has moved it out (or for a value the body
    // never yielded). The `out_drop_thunk` is the FOURTH pointer field
    // (offset `3 * ptr_width`), null when `Y` is `BitCopy` (nothing to drop).
    // SAFETY: offset 4*ptr_width+1 is within the companion (after handle, env,
    // thunk pointers and the `started` byte), so advancing the base lands at the
    // `pending` flag byte.
    let pending_slot = unsafe { companion.cast::<u8>().add(ptr_width.saturating_mul(4) + 1) };
    // SAFETY: pending_slot points at the in-bounds `pending` flag byte the
    // MakeGenerator codegen wrote; a single-byte `read` is sound.
    let pending = unsafe { ptr::read(pending_slot) };
    if pending != 0 {
        // SAFETY: offset 3*ptr_width is the out-drop thunk pointer field, within
        // the four leading pointer fields the codegen always emits.
        let thunk_slot = unsafe { companion.cast::<u8>().add(ptr_width.saturating_mul(3)) };
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

    fn active_frames() -> Vec<*mut c_void> {
        ACTIVE_COROUTINE_FRAMES
            .with(|active| active.borrow().iter().map(|record| record.frame).collect())
    }

    #[test]
    fn tracked_crash_frames_drain_in_lifo_order() {
        // SAFETY: allocate three tracked test frames and raw-free them exactly
        // once after the drain transfers their ownership into `order`.
        unsafe {
            let outer = hew_cont_frame_alloc_tracked(32);
            let child = hew_cont_frame_alloc_tracked(48);
            let nested = hew_cont_frame_alloc_tracked(64);
            let mut order = Vec::new();
            let reclaimed = drain_active_coroutine_frames_excluding(ptr::null_mut(), |frame| {
                order.push(frame);
            });
            assert_eq!(reclaimed, 3);
            assert_eq!(order, [nested, child, outer]);
            assert!(active_frames().is_empty());
            for frame in order {
                free_frame_allocation(frame);
            }
        }
    }

    #[test]
    fn active_frame_mismatch_preserves_lifo_stack() {
        // SAFETY: allocate two tracked frames and free each exactly once after
        // exercising the pointer-only handoff discipline.
        unsafe {
            let outer = hew_cont_frame_alloc_tracked(32);
            let inner = hew_cont_frame_alloc_tracked(48);
            assert_eq!(active_frames(), [outer, inner]);

            hew_cont_frame_handoff(outer);
            assert_eq!(
                active_frames(),
                [outer, inner],
                "a non-top handoff must not punch through a nested active frame"
            );
            hew_cont_frame_handoff(inner);
            hew_cont_frame_handoff(outer);
            assert!(active_frames().is_empty());

            hew_cont_frame_free(inner);
            hew_cont_frame_free(outer);
        }
    }

    #[test]
    fn null_and_untracked_frames_never_enter_active_stack() {
        // SAFETY: zero-sized tracked allocation is a documented null result;
        // the untracked allocation is freed exactly once below.
        unsafe {
            let null = hew_cont_frame_alloc_tracked(0);
            assert!(null.is_null());
            hew_cont_frame_handoff(null);
            assert!(!active_coroutine_enter(null));
            assert!(!active_coroutine_leave(null, ActiveCoroutinePhase::Resume));

            let companion = hew_cont_frame_alloc(32);
            assert!(!active_coroutine_enter(companion));
            assert!(active_frames().is_empty());
            hew_cont_frame_free(companion);
        }
    }

    #[test]
    fn normal_handoff_and_frame_free_remove_active_records() {
        // SAFETY: both tracked frames are freed exactly once. `handoff` removes
        // the first; the public free removes the second matching active record.
        unsafe {
            let handed_off = hew_cont_frame_alloc_tracked(32);
            hew_cont_frame_handoff(handed_off);
            assert!(active_frames().is_empty());
            hew_cont_frame_free(handed_off);

            let freed_while_active = hew_cont_frame_alloc_tracked(48);
            assert_eq!(active_frames(), [freed_while_active]);
            hew_cont_frame_free(freed_while_active);
            assert!(active_frames().is_empty());
        }
    }

    #[test]
    fn split_resume_handoff_clone_does_not_consume_resume_record() {
        // SAFETY: one tracked frame is handed off from its ramp, entered as a
        // resume, then freed exactly once after the resume record is removed.
        unsafe {
            let frame = hew_cont_frame_alloc_tracked(48);
            hew_cont_frame_handoff(frame);
            assert!(active_frames().is_empty());

            assert!(active_coroutine_enter(frame));
            assert_eq!(active_frames(), [frame]);
            // CoroSplit clones the shared return block into `.resume`; this
            // generated Ramp-phase handoff must not steal Resume ownership.
            hew_cont_frame_handoff(frame);
            assert_eq!(active_frames(), [frame]);
            assert!(active_coroutine_leave(frame, ActiveCoroutinePhase::Resume));
            assert!(active_frames().is_empty());
            hew_cont_frame_free(frame);
        }
    }

    #[test]
    fn resumed_root_exclusion_reclaims_only_nested_frames() {
        // SAFETY: the drain transfers child/grandchild ownership into `order`;
        // the excluded root remains active and is released by its sole owner.
        unsafe {
            let root = hew_cont_frame_alloc_tracked(32);
            let child = hew_cont_frame_alloc_tracked(48);
            let nested = hew_cont_frame_alloc_tracked(64);
            let mut order = Vec::new();
            let reclaimed = drain_active_coroutine_frames_excluding(root, |frame| {
                order.push(frame);
            });
            assert_eq!(reclaimed, 2);
            assert_eq!(order, [nested, child]);
            assert_eq!(
                active_frames(),
                [root],
                "the scheduler-owned resumed root remains for \
                 abandon_resuming_after_crash"
            );
            for frame in order {
                free_frame_allocation(frame);
            }
            hew_cont_frame_free(root);
            assert!(active_frames().is_empty());
        }
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
    // `{ ptr handle, ptr env, ptr env_drop_thunk, ptr out_drop_thunk,
    //    i8 started, i8 pending, Y out }`.
    // `hew_gen_coro_destroy` reads the four leading pointers + the two flag
    // bytes at fixed (pointer-width-derived) offsets and, when `pending != 0`
    // and the thunk is non-null, calls `thunk(companion)` to typed-drop the
    // un-consumed owned `out` value exactly once. These tests model that block
    // directly (no LLVM) and assert the dispatch fires exactly when pending,
    // never when consumed, and exactly once.
    //
    // `hew_gen_coro_destroy` is not emitted for wasm32 (teardown is handled by
    // the synthesized IR on that target), so the helpers and tests in this block
    // are gated off wasm32.

    #[cfg(not(target_arch = "wasm32"))]
    use std::sync::atomic::{AtomicU32, Ordering};

    // Separate counters per test so the dispatch tests stay isolated under
    // nextest's parallel execution (a shared counter would race).
    #[cfg(not(target_arch = "wasm32"))]
    static PENDING_DROP_CALLS: AtomicU32 = AtomicU32::new(0);
    #[cfg(not(target_arch = "wasm32"))]
    static CONSUMED_DROP_CALLS: AtomicU32 = AtomicU32::new(0);
    #[cfg(not(target_arch = "wasm32"))]
    static ENV_DROP_CALLS: AtomicU32 = AtomicU32::new(0);

    /// Stand-in for a codegen-emitted typed out-drop thunk: bumps the
    /// pending-path counter. A real thunk GEPs to the companion's `out` field and
    /// runs `Y`'s drop; the dispatch contract this pins is "called once iff pending".
    #[cfg(not(target_arch = "wasm32"))]
    unsafe extern "C" fn pending_counting_thunk(_companion: *mut c_void) {
        PENDING_DROP_CALLS.fetch_add(1, Ordering::SeqCst);
    }

    /// Stand-in thunk for the consumed-path test (bumps a distinct counter).
    #[cfg(not(target_arch = "wasm32"))]
    unsafe extern "C" fn consumed_counting_thunk(_companion: *mut c_void) {
        CONSUMED_DROP_CALLS.fetch_add(1, Ordering::SeqCst);
    }

    #[cfg(not(target_arch = "wasm32"))]
    unsafe extern "C" fn env_counting_thunk(_env: *mut c_void) {
        ENV_DROP_CALLS.fetch_add(1, Ordering::SeqCst);
    }

    /// Build a minimal companion block matching the codegen layout, with a null
    /// coro handle + null env (so handle/env teardown are no-ops) and the given
    /// `pending` flag + out-drop thunk. Returns the companion pointer (owned by
    /// `hew_cont_frame_alloc`, freed by `hew_gen_coro_destroy`).
    #[cfg(not(target_arch = "wasm32"))]
    unsafe fn make_companion(
        pending: u8,
        out_thunk: Option<unsafe extern "C" fn(*mut c_void)>,
        env: *mut c_void,
        env_thunk: Option<unsafe extern "C" fn(*mut c_void)>,
    ) -> *mut c_void {
        let ptr_width = core::mem::size_of::<*mut c_void>();
        // Four pointers + two flag bytes is the only region the runtime reads;
        // size the block generously past that so the flag/thunk writes are in
        // bounds (the real `out` field follows but the runtime never reads it).
        let size = (ptr_width * 5) as u64;
        // SAFETY: hew_cont_frame_alloc is safe for any size and returns a block
        // freed by hew_gen_coro_destroy below.
        let companion = unsafe { hew_cont_frame_alloc(size) };
        assert!(!companion.is_null());
        // SAFETY: every write below targets an in-bounds field of the companion
        // block just allocated (handle @0, env @ptr_width, env thunk @2*ptr_width,
        // out thunk @3*ptr_width, started @4*ptr_width,
        // pending @4*ptr_width+1), matching the codegen
        // layout the runtime destroy reads.
        unsafe {
            ptr::write(companion.cast::<*mut c_void>(), ptr::null_mut());
            ptr::write_unaligned(
                companion.cast::<u8>().add(ptr_width).cast::<*mut c_void>(),
                env,
            );
            ptr::write_unaligned(
                companion
                    .cast::<u8>()
                    .add(ptr_width * 2)
                    .cast::<Option<unsafe extern "C" fn(*mut c_void)>>(),
                env_thunk,
            );
            ptr::write_unaligned(
                companion
                    .cast::<u8>()
                    .add(ptr_width * 3)
                    .cast::<Option<unsafe extern "C" fn(*mut c_void)>>(),
                out_thunk,
            );
            ptr::write(companion.cast::<u8>().add(ptr_width * 4), 0u8);
            ptr::write(companion.cast::<u8>().add(ptr_width * 4 + 1), pending);
        }
        companion
    }

    /// A companion dropped with `pending == 1` and a non-null thunk runs the
    /// typed out-drop EXACTLY once — the leak fix for an un-consumed owned yield.
    /// `hew_gen_coro_destroy` is not emitted for wasm32 (the IR path handles
    /// teardown there); this test covers the native Rust implementation only.
    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn destroy_runs_out_drop_thunk_exactly_once_when_pending() {
        PENDING_DROP_CALLS.store(0, Ordering::SeqCst);
        // SAFETY: companion is a well-formed block from make_companion; destroy
        // is its sole teardown owner.
        unsafe {
            let companion = make_companion(1, Some(pending_counting_thunk), ptr::null_mut(), None);
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
    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn destroy_skips_out_drop_thunk_when_not_pending() {
        CONSUMED_DROP_CALLS.store(0, Ordering::SeqCst);
        // SAFETY: companion is a well-formed block from make_companion.
        unsafe {
            let companion = make_companion(0, Some(consumed_counting_thunk), ptr::null_mut(), None);
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
    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn destroy_null_thunk_pending_is_noop_drop() {
        // SAFETY: companion has a null thunk; destroy must skip the call.
        unsafe {
            let companion = make_companion(1, None, ptr::null_mut(), None);
            hew_gen_coro_destroy(companion);
        }
        // No crash, no double-free; reaching here is the assertion.
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn destroy_runs_env_drop_thunk_exactly_once() {
        ENV_DROP_CALLS.store(0, Ordering::SeqCst);
        // SAFETY: both allocations are created by the frame allocator and the
        // companion is destroyed exactly once by the sole teardown owner.
        unsafe {
            let env = hew_cont_frame_alloc(8);
            let companion = make_companion(0, None, env, Some(env_counting_thunk));
            hew_gen_coro_destroy(companion);
        }
        assert_eq!(ENV_DROP_CALLS.load(Ordering::SeqCst), 1);
    }
}
