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
//!    allocator stores the block size, tracked-frame marker, and typed-cleanup
//!    registry pointer in a 32-byte header it prepends to every frame and reads
//!    back at free time. Coroutine
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

use std::cell::{Cell, RefCell};
use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::{AtomicU64, Ordering};

use crate::mem::{hew_alloc, hew_dealloc};

/// Alignment for a coroutine frame. LLVM's `CoroSplit` picks the frame's
/// natural alignment from the spilled state; 16 bytes covers every Hew scalar /
/// pointer / aggregate the frame can hold on the targets Hew supports
/// (`x86_64` / aarch64 / wasm32), so a 16-byte frame alignment is always
/// sufficient and never under-aligns a spilled value. The header reserves two
/// full 16-byte strides so the returned frame pointer keeps this alignment.
const FRAME_ALIGN: usize = 16;

/// Maximum alignment requested for actor-state crash snapshots. Hew's current
/// target ABIs require at most 16 bytes for generated state, while 64 leaves
/// conservative headroom without turning an unusually aligned allocator
/// address (for example, a page boundary) into a multi-kilobyte layout request.
const MAX_STATE_ESCROW_ALIGN: usize = 64;

/// Bytes reserved ahead of the frame for the stored block size, tracked-frame
/// marker, and crash-cleanup-registry pointer. Two full [`FRAME_ALIGN`]
/// strides keep the pointer handed to LLVM 16-byte aligned on every target.
const FRAME_HEADER: usize = FRAME_ALIGN * 2;

/// Byte offset of the crash-cleanup registry pointer in the private frame
/// header. The first two words remain the allocation size and tracked marker.
const FRAME_CLEANUP_REGISTRY_OFFSET: usize = size_of::<u64>() * 2;

/// Marker stored in the second word of a coroutine frame header.
///
/// Only allocations made by [`hew_cont_frame_alloc_tracked`] carry this marker.
/// Generator companions and environments continue to use
/// [`hew_cont_frame_alloc`] and remain deliberately outside crash-frame
/// reclamation: they require typed teardown that a raw crash unwind cannot
/// provide.
const TRACKED_COROUTINE_FRAME_MAGIC: u64 = 0x4845_5743_4f52_4f31;

/// `hew_cont_crash_cleanup_arm` result indicating malformed relocation input
/// or a snapshot allocation that could not be represented. Zero is reserved
/// for the ordinary "no active tracked coroutine" no-op.
pub const CRASH_CLEANUP_ARM_FAILED: u64 = u64::MAX;

/// Rust-authored cleanup callbacks may explicitly use `C-unwind`, allowing the
/// drain to quarantine their unwind on compatible host builds. Generated LLVM
/// cleanup thunks call plain-`C` runtime symbols: a Rust panic in one of those
/// symbols is process-fatal at that boundary and never reaches this quarantine.
type CrashCleanupThunk = unsafe extern "C-unwind" fn(*mut c_void);
type StateCrashCleanupThunk = unsafe extern "C" fn(*mut c_void);

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
enum CrashCleanupRunState {
    #[default]
    Pending,
    Running,
    Done,
}

/// How a typed slot may be escrowed for post-longjmp cleanup.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u32)]
pub enum CrashCleanupRelocation {
    /// The `ElabDrop` ritual is valid on an ABI-aligned bytewise snapshot.
    Bitwise = 0,
    /// The snapshot contains an interior pointer into its owner coroutine
    /// frame (currently `dyn Trait` with `FrameOwned` storage). The runtime
    /// additionally proves that pointee lies in that frame before accepting it.
    FrameInterior = 1,
}

/// Where crash cleanup reads the current value.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u32)]
pub enum CrashCleanupStorage {
    /// The slot itself lies in the tracked coroutine allocation and remains
    /// readable after longjmp until raw frame reclamation.
    DirectFrame = 0,
    /// The slot lies on a nested synchronous helper stack. Runtime keeps an
    /// ABI-aligned emergency byte snapshot because that stack dies at longjmp.
    Snapshot = 1,
}

struct CrashCleanupEntry {
    token: u64,
    owner_registry: *mut CrashCleanupRegistry,
    owner_frame: *mut c_void,
    slot: *mut c_void,
    snapshot: *mut u8,
    size: u64,
    align: u64,
    thunk: CrashCleanupThunk,
    storage: CrashCleanupStorage,
    relocation: CrashCleanupRelocation,
    active: bool,
    order: u64,
    run_state: CrashCleanupRunState,
}

#[derive(Default)]
struct CrashCleanupRegistry {
    /// Entries remain boxed so registry mutation cannot move them while crash
    /// drain owns their typed metadata. Compiler-held tokens are independent,
    /// process-unique generations: a freed Box address can never make a stale
    /// token name a later logical entry.
    entries: Vec<*mut CrashCleanupEntry>,
    next_order: u64,
    /// Dispatch scopes additionally escrow the actor's complete state. The
    /// snapshot is kept structurally drop-safe across field updates by the
    /// generated begin/prepare transaction hooks below; coroutine-frame
    /// registries leave these fields null/zero.
    state_source: *mut u8,
    state_snapshot: *mut u8,
    state_size: u64,
    state_align: u64,
    state_drop: Option<CrashCleanupThunk>,
    state_run_state: CrashCleanupRunState,
    /// A generated field replacement transaction has begun changing the
    /// escrow's ownership shape. Once true, even a host-only caught Rust unwind
    /// cannot return typed state authority to the original live wrapper: its
    /// field bytes may already name a partially finalized value.
    state_mutation_began: bool,
    /// An actor-state overwrite has neutralized its old escrow field and may
    /// be running a non-idempotent old-value finalizer. Hardware faults,
    /// intentional traps, and unwind attempts in this interval are all
    /// process-fatal; no actor recovery may abandon indeterminate authority.
    state_finalizer_critical: bool,
}

/// Process-wide non-reusing crash-cleanup identity source.
///
/// Zero is the benign "no active tracked frame" token and `u64::MAX` is the
/// hard-failure sentinel, so neither is ever issued. Exhaustion fails closed
/// rather than wrapping and reviving an ancient token.
static NEXT_CRASH_CLEANUP_TOKEN: AtomicU64 = AtomicU64::new(1);

fn next_crash_cleanup_token() -> Option<u64> {
    NEXT_CRASH_CLEANUP_TOKEN
        .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |next| {
            (next < CRASH_CLEANUP_ARM_FAILED).then(|| next + 1)
        })
        .ok()
}

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

    /// Scheduler-bracketed cooperative crash domains. Ordinary handler and
    /// free-function stack owners attach here when no tracked coroutine frame
    /// is active. Native longjmp and unwind-capable host parity recovery detach
    /// and drain the top scope; normal dispatch completion discards its state
    /// escrow only after generated lexical owners have retired their tokens.
    /// The production wasm32-wasip1 panic=abort artifact has no unwind edge.
    static DISPATCH_CRASH_CLEANUP_SCOPES: RefCell<Vec<*mut CrashCleanupRegistry>> =
        const { RefCell::new(Vec::new()) };

    /// Non-zero while a detached registry is invoking generated finalizers.
    /// Registration and recovery APIs fail closed in this phase so re-entry
    /// cannot attach to, or accidentally pop, an older dispatch scope.
    static CRASH_CLEANUP_DRAIN_DEPTH: Cell<u32> = const { Cell::new(0) };
}

struct CrashCleanupDrainGuard;

#[inline]
fn publish_crash_cleanup_drain_to_signal_handler(active: bool) {
    #[cfg(not(target_arch = "wasm32"))]
    crate::signal::set_crash_cleanup_drain_active(active);

    // wasm32 has no native signal module or hardware-fault longjmp boundary.
    // The thread-local drain-depth guard remains authoritative for cooperative
    // trap rejection without claiming native signal containment.
    #[cfg(target_arch = "wasm32")]
    let _ = active;
}

impl CrashCleanupDrainGuard {
    fn enter() -> Option<Self> {
        CRASH_CLEANUP_DRAIN_DEPTH.with(|depth| {
            let current = depth.get();
            let next = current.checked_add(1)?;
            depth.set(next);
            if current == 0 {
                publish_crash_cleanup_drain_to_signal_handler(true);
            }
            Some(Self)
        })
    }
}

impl Drop for CrashCleanupDrainGuard {
    fn drop(&mut self) {
        CRASH_CLEANUP_DRAIN_DEPTH.with(|depth| {
            let next = depth.get().saturating_sub(1);
            depth.set(next);
            if next == 0 {
                publish_crash_cleanup_drain_to_signal_handler(false);
            }
        });
    }
}

/// Whether this thread is executing a detached crash-cleanup finalizer.
///
/// Trap bridges use this to reject a nested longjmp/unwind. A finalizer that
/// traps may already have performed non-idempotent work; retrying it or jumping
/// over the durable drain would make memory safety unknowable. The supported
/// policy is therefore: Rust unwinds are caught per entry where unwinding is
/// available, while Hew/hardware traps during finalization abort the process
/// deterministically with a diagnostic.
pub(crate) fn crash_cleanup_drain_active() -> bool {
    CRASH_CLEANUP_DRAIN_DEPTH.with(|depth| depth.get() != 0)
}

/// Reject a cooperative trap raised by a cleanup finalizer.
///
/// Longjmp/unwind would abandon a registry whose current entry is Running and
/// may already have performed a non-idempotent close. There is no sound retry
/// point. Rust panics crossing the `C-unwind` callback are caught per entry;
/// Hew traps take this explicit process-fatal edge instead.
pub(crate) fn abort_if_crash_cleanup_finalizer_trap(kind: &str) {
    if crash_cleanup_drain_active() {
        eprintln!(
            "fatal: {kind} raised during crash-cleanup finalization; refusing to retry a partially executed finalizer"
        );
        std::process::abort();
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ActiveCoroutinePhase {
    /// A newly allocated coroutine ramp is executing before returning a handle.
    Ramp,
    /// `hew_cont_resume` is driving a `CoroSplit` `.resume` outline.
    Resume,
    /// `hew_cont_destroy` is driving a suspended frame's cleanup outline.
    Destroy,
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
    // SAFETY: base points to at least FRAME_HEADER writable bytes that
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
        ptr::write_unaligned(
            base.add(FRAME_CLEANUP_REGISTRY_OFFSET)
                .cast::<*mut CrashCleanupRegistry>(),
            ptr::null_mut(),
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
    // A normal `coro.destroy`/completion path must have disarmed every typed
    // escrow before the frame reaches its raw allocation free.
    // SAFETY: this validates that no live typed owner is being silently
    // discarded, then releases only inactive escrow storage.
    unsafe { discard_frame_crash_cleanup_registry(frame) };
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

#[cfg(test)]
pub(crate) fn frame_has_tracked_header_for_test(frame: *mut c_void) -> bool {
    // SAFETY: test callers pass a live handle allocated by the continuation
    // frame allocator; this counterfactual proves synthetic executor frames
    // carry the private header before cross-worker resume probes it.
    unsafe { frame_is_tracked(frame) }
}

#[cfg(test)]
pub(crate) const fn frame_alignment_for_test() -> usize {
    FRAME_ALIGN
}

#[expect(
    clippy::cast_ptr_alignment,
    reason = "the allocator guarantees 16-byte base alignment and the registry word is at offset 16"
)]
unsafe fn frame_cleanup_registry_slot(frame: *mut c_void) -> *mut *mut CrashCleanupRegistry {
    // SAFETY: every continuation frame returned by this module has a
    // FRAME_HEADER-byte private prefix, and the registry word is within it.
    let base = unsafe { frame.cast::<u8>().sub(FRAME_HEADER) };
    // SAFETY: the offset names the third word in the private header.
    unsafe {
        base.add(FRAME_CLEANUP_REGISTRY_OFFSET)
            .cast::<*mut CrashCleanupRegistry>()
    }
}

#[cfg(test)]
unsafe fn frame_cleanup_registry(frame: *mut c_void) -> *mut CrashCleanupRegistry {
    // SAFETY: test callers supply a live frame allocation.
    unsafe { ptr::read_unaligned(frame_cleanup_registry_slot(frame)) }
}

unsafe fn ensure_frame_cleanup_registry(frame: *mut c_void) -> *mut CrashCleanupRegistry {
    // SAFETY: caller supplies a live tracked frame.
    let slot = unsafe { frame_cleanup_registry_slot(frame) };
    // SAFETY: initialized header word, as above.
    let existing = unsafe { ptr::read_unaligned(slot) };
    if !existing.is_null() {
        return existing;
    }
    let registry = Box::into_raw(Box::new(CrashCleanupRegistry::default()));
    // SAFETY: slot is the live frame's private registry header word.
    unsafe { ptr::write_unaligned(slot, registry) };
    registry
}

unsafe fn take_frame_cleanup_registry(frame: *mut c_void) -> *mut CrashCleanupRegistry {
    // SAFETY: caller supplies a live frame from this allocator.
    let slot = unsafe { frame_cleanup_registry_slot(frame) };
    // SAFETY: read with respect to the frame's exclusive owner; no other worker
    // may resume/destroy this frame concurrently.
    let registry = unsafe { ptr::read_unaligned(slot) };
    // SAFETY: the same exclusive ownership permits clearing the header word.
    unsafe { ptr::write_unaligned(slot, ptr::null_mut()) };
    registry
}

/// Run one arbitrary crash-cleanup callback without allowing its unwind or
/// panic payload to escape the runtime's recovery boundary.
///
/// Dropping a caught payload is normally required to release the allocation
/// owned by `catch_unwind`. That drop is arbitrary Rust code too: a custom
/// payload may panic from `Drop`. Catch that secondary unwind and dispose its
/// replacement payload as well. A recursively hostile payload chain cannot be
/// recovered safely; abort after the secondary disposal attempt rather than
/// leaking a payload or unwinding through an FFI caller.
fn run_quarantined_crash_cleanup(callback: impl FnOnce()) -> bool {
    let Err(mut payload) = std::panic::catch_unwind(std::panic::AssertUnwindSafe(callback)) else {
        return false;
    };

    // The first attempt releases an ordinary payload. The second releases the
    // replacement payload produced if the original payload's destructor
    // panics. Box drop glue still deallocates the original box while unwinding.
    for _ in 0..2 {
        match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| drop(payload))) {
            Ok(()) => return true,
            Err(next_payload) => payload = next_payload,
        }
    }

    eprintln!("fatal: recursively panicking crash-cleanup payload destructor");
    std::process::abort();
}

unsafe fn free_crash_cleanup_entry(entry: *mut CrashCleanupEntry, run: bool) -> bool {
    if entry.is_null() {
        return false;
    }
    // SAFETY: every pointer stored in a registry came from Box::into_raw and
    // is removed exactly once before this reconstruction.
    let mut entry = unsafe { Box::from_raw(entry) };
    let mut faulted = false;
    if run && entry.active && entry.run_state == CrashCleanupRunState::Pending {
        // Commit Running before crossing into arbitrary generated code. If the
        // callback faults, it is never retried: resource finalizers are not
        // generally idempotent and may have completed a partial close.
        entry.run_state = CrashCleanupRunState::Running;
        let cleanup_slot = match entry.storage {
            CrashCleanupStorage::DirectFrame => entry.slot,
            CrashCleanupStorage::Snapshot => entry.snapshot.cast(),
        };
        // SAFETY: codegen supplied the exact `void(ptr)` ElabDrop thunk for
        // this live frame slot or ABI-aligned byte snapshot. It remains live
        // throughout the call and the abandoned original will never be dropped.
        // A genuinely unwind-capable `C-unwind` callback (including the
        // Rust-authored test/plugin boundary) is quarantined here. Generated
        // LLVM thunks call plain-C runtime symbols, whose Rust panic policy is
        // process-fatal before this catch can observe an Err.
        faulted = run_quarantined_crash_cleanup(|| unsafe {
            (entry.thunk)(cleanup_slot);
        });
        entry.run_state = CrashCleanupRunState::Done;
    }
    if !entry.snapshot.is_null() {
        // SAFETY: snapshot storage, when present, was allocated by arm with the
        // exact stored size/alignment.
        unsafe { hew_dealloc(entry.snapshot, entry.size, entry.align) };
    }
    faulted
}

unsafe fn free_dispatch_state_snapshot(registry: &mut CrashCleanupRegistry, run: bool) -> bool {
    let snapshot = std::mem::replace(&mut registry.state_snapshot, ptr::null_mut());
    if snapshot.is_null() {
        return false;
    }
    let mut faulted = false;
    if run {
        if let Some(drop) = registry.state_drop.take() {
            // SAFETY: dispatch begin copied a fully initialized actor state and
            // every generated field update cleared its range before a
            // potentially trapping release, publishing new bytes only after
            // the replacement became valid. The generated state-drop thunk is
            // therefore valid for this escrow even after longjmp/unwind.
            registry.state_run_state = CrashCleanupRunState::Running;
            // This catch remains defensive for a callback that can genuinely
            // cross C-unwind. Current generated state-drop thunks call plain-C
            // runtime symbols and therefore have process-fatal panic behavior.
            faulted = run_quarantined_crash_cleanup(|| {
                // SAFETY: the state snapshot and generated descriptor remain
                // live for this detached registry's exclusive drain.
                unsafe { drop(snapshot.cast()) };
            });
            registry.state_run_state = CrashCleanupRunState::Done;
        }
    }
    // SAFETY: begin allocated the escrow with this exact size/alignment.
    unsafe { hew_dealloc(snapshot, registry.state_size, registry.state_align) };
    faulted
}

unsafe fn free_crash_cleanup_registry(
    registry: *mut CrashCleanupRegistry,
    run_entries: bool,
    run_state: bool,
) -> bool {
    if registry.is_null() {
        return false;
    }
    let Some(_drain_guard) = CrashCleanupDrainGuard::enter() else {
        eprintln!("fatal: crash-cleanup drain nesting depth overflow");
        std::process::abort();
    };
    // SAFETY: the registry was detached from its sole TLS/frame owner before
    // this reconstruction.
    let mut registry = unsafe { Box::from_raw(registry) };
    let mut entries = std::mem::take(&mut registry.entries);
    entries.sort_unstable_by_key(|entry| {
        // SAFETY: every registry member is a live stable box.
        unsafe { (**entry).order }
    });
    let mut faulted = false;
    for entry in entries.into_iter().rev() {
        // SAFETY: detachment transferred the sole typed authority here.
        faulted |= unsafe { free_crash_cleanup_entry(entry, run_entries) };
    }
    // Actor state predates every lexical owner, so it is released last.
    // SAFETY: detachment transferred sole ownership of the escrow allocation
    // and generated typed drop metadata to this drain.
    faulted |= unsafe { free_dispatch_state_snapshot(&mut registry, run_state) };
    faulted
}

unsafe fn discard_frame_crash_cleanup_registry(frame: *mut c_void) {
    // SAFETY: caller supplies a live frame whose raw allocation is about to be
    // freed through the ordinary MIR/coro.destroy authority.
    let registry = unsafe { take_frame_cleanup_registry(frame) };
    if registry.is_null() {
        return;
    }
    // SAFETY: pointer came from Box::into_raw in ensure_frame_cleanup_registry.
    let registry = unsafe { Box::from_raw(registry) };
    if registry.entries.iter().any(|entry| {
        entry.is_null() || {
            // SAFETY: every non-null registry member is a live stable box until
            // this function consumes it below.
            unsafe { (**entry).active }
        }
    }) {
        eprintln!(
            "fatal: raw coroutine frame free attempted with an active typed crash-cleanup owner"
        );
        std::process::abort();
    }
    for entry in registry.entries {
        // SAFETY: the fail-closed scan proved this entry inactive, so ordinary
        // cleanup has already consumed/transferred the typed value. Discard
        // only the non-owning escrow bytes.
        let _ = unsafe { free_crash_cleanup_entry(entry, false) };
    }
}

unsafe fn run_frame_crash_cleanups(frame: *mut c_void) {
    // Detach before running user/resource thunks so recursive runtime calls
    // cannot observe a half-drained registry.
    // SAFETY: crash recovery owns this abandoned live frame exclusively.
    let registry = unsafe { take_frame_cleanup_registry(frame) };
    if registry.is_null() {
        return;
    }
    // SAFETY: detachment transferred the frame registry's sole authority;
    // frame registries never carry actor-state escrow.
    let _ = unsafe { free_crash_cleanup_registry(registry, true, false) };
}

fn crash_cleanup_ranges_overlap(
    lhs_slot: *mut c_void,
    lhs_size: u64,
    rhs_slot: *mut c_void,
    rhs_size: u64,
) -> bool {
    let (Ok(lhs_size), Ok(rhs_size)) = (usize::try_from(lhs_size), usize::try_from(rhs_size))
    else {
        return true;
    };
    let lhs_start = lhs_slot as usize;
    let rhs_start = rhs_slot as usize;
    let (Some(lhs_end), Some(rhs_end)) = (
        lhs_start.checked_add(lhs_size),
        rhs_start.checked_add(rhs_size),
    ) else {
        return true;
    };
    lhs_start < rhs_end && rhs_start < lhs_end
}

fn parse_crash_cleanup_storage(value: u32) -> Option<CrashCleanupStorage> {
    match value {
        value if value == CrashCleanupStorage::DirectFrame as u32 => {
            Some(CrashCleanupStorage::DirectFrame)
        }
        value if value == CrashCleanupStorage::Snapshot as u32 => {
            Some(CrashCleanupStorage::Snapshot)
        }
        _ => None,
    }
}

fn parse_crash_cleanup_relocation(value: u32) -> Option<CrashCleanupRelocation> {
    match value {
        value if value == CrashCleanupRelocation::Bitwise as u32 => {
            Some(CrashCleanupRelocation::Bitwise)
        }
        value if value == CrashCleanupRelocation::FrameInterior as u32 => {
            Some(CrashCleanupRelocation::FrameInterior)
        }
        _ => None,
    }
}

fn validate_crash_cleanup_slot(
    owner_frame: *mut c_void,
    slot: *mut c_void,
    size: u64,
    align: u64,
    storage: CrashCleanupStorage,
    relocation: CrashCleanupRelocation,
) -> bool {
    let Ok(align_host) = usize::try_from(align) else {
        return false;
    };
    if slot.is_null()
        || size == 0
        || align_host == 0
        || !align_host.is_power_of_two()
        || !(slot as usize).is_multiple_of(align_host)
    {
        return false;
    }
    if storage == CrashCleanupStorage::DirectFrame
        && !active_frame_contains_range(owner_frame, slot, size)
    {
        return false;
    }
    if relocation == CrashCleanupRelocation::FrameInterior {
        let Ok(size_host) = usize::try_from(size) else {
            return false;
        };
        if size_host < size_of::<crate::trait_object::HewTraitObject>() {
            return false;
        }
        // A FrameOwned trait-object stores `(data, vtable)`. The vtable prefix
        // carries the complete concrete size/alignment; validating one byte
        // would admit a pointee that straddles the frame boundary.
        let words = slot.cast::<*mut c_void>();
        // SAFETY: the fat-slot size check proves both pointer words readable,
        // and `add(1)` remains within that checked slot.
        let (data, vtable) = unsafe {
            (
                ptr::read_unaligned(words),
                ptr::read_unaligned(words.add(1)).cast::<crate::trait_object::HewVtable>(),
            )
        };
        if data.is_null() || vtable.is_null() {
            return false;
        }
        // SAFETY: generated trait objects carry a codegen-emitted static vtable
        // whose prefix has the runtime's repr(C) `(drop,size,align)` layout.
        let (concrete_size, concrete_align) = unsafe { ((*vtable).size_of, (*vtable).align_of) };
        if concrete_align == 0
            || !concrete_align.is_power_of_two()
            || !(data as usize).is_multiple_of(concrete_align)
        {
            return false;
        }
        let Ok(concrete_size) = u64::try_from(concrete_size) else {
            return false;
        };
        if !active_frame_contains_range(owner_frame, data, concrete_size) {
            return false;
        }
    }
    true
}

unsafe fn copy_crash_cleanup_snapshot(entry: &mut CrashCleanupEntry, slot: *mut c_void) -> bool {
    if entry.storage != CrashCleanupStorage::Snapshot {
        return true;
    }
    let Ok(align_host) = usize::try_from(entry.align) else {
        return false;
    };
    if align_host == 0 || !align_host.is_power_of_two() {
        return false;
    }
    let Ok(size_host) = usize::try_from(entry.size) else {
        return false;
    };
    if entry.snapshot.is_null() {
        // Perform every fallible descriptor conversion before allocating so a
        // rejected new arm cannot strand an unpublished snapshot pointer.
        // SAFETY: hew_alloc validates the DataLayout-derived size/alignment.
        entry.snapshot = unsafe { hew_alloc(entry.size, entry.align) };
        if entry.snapshot.is_null() {
            return false;
        }
    }
    if !(entry.snapshot as usize).is_multiple_of(align_host) {
        // An allocator contract violation must still fail atomically: release
        // the unpublished escrow before returning the hard-failure sentinel.
        let snapshot = std::mem::replace(&mut entry.snapshot, ptr::null_mut());
        // SAFETY: snapshot came from the matching allocation immediately above
        // or an earlier successful arm with the same immutable descriptor.
        unsafe { hew_dealloc(snapshot, entry.size, entry.align) };
        return false;
    }
    // SAFETY: generated code guarantees the slot holds an initialized value of
    // exactly this descriptor type; the snapshot is a distinct matching block.
    unsafe {
        ptr::copy_nonoverlapping(slot.cast::<u8>(), entry.snapshot, size_host);
    }
    true
}

/// Arm or reactivate one exact typed owner for native crash recovery.
///
/// A zero `token` creates a stable entry on the active tracked frame. A
/// non-zero token reactivates that same entry after reassignment without
/// changing its first-activation order. `DirectFrame` entries point at a
/// range-validated coroutine slot; `Snapshot` entries keep an ABI-aligned
/// emergency copy of a synchronous helper's stack slot.
///
/// Returns zero only for a NEW arm when no tracked coroutine is active,
/// [`CRASH_CLEANUP_ARM_FAILED`] for every hard error, or the stable token.
///
/// # Safety
///
/// `slot` must point to a live initialized value of the descriptor type
/// represented by `size`, `align`, and `thunk`. A non-zero `token` must have
/// been returned by an earlier arm of the currently active tracked frame and
/// must not have been retired.
#[no_mangle]
pub unsafe extern "C" fn hew_cont_crash_cleanup_arm(
    token: u64,
    slot: *mut c_void,
    size: u64,
    align: u64,
    thunk: Option<CrashCleanupThunk>,
    storage: u32,
    relocation: u32,
) -> u64 {
    let Some((registry, owner_frame)) = current_crash_cleanup_registry() else {
        return if token == 0 {
            0
        } else {
            CRASH_CLEANUP_ARM_FAILED
        };
    };
    let (Some(thunk), Some(storage), Some(relocation)) = (
        thunk,
        parse_crash_cleanup_storage(storage),
        parse_crash_cleanup_relocation(relocation),
    ) else {
        return CRASH_CLEANUP_ARM_FAILED;
    };
    if align == 0
        || !align.is_power_of_two()
        || !validate_crash_cleanup_slot(owner_frame, slot, size, align, storage, relocation)
    {
        return CRASH_CLEANUP_ARM_FAILED;
    }

    // SAFETY: the executing frame has exclusive access to its registry.
    let registry = unsafe { &mut *registry };

    let entry = if token == 0 {
        None
    } else {
        if token == CRASH_CLEANUP_ARM_FAILED {
            return CRASH_CLEANUP_ARM_FAILED;
        }
        let Some(entry) = registry.entries.iter().copied().find(|entry| {
            // SAFETY: all registry members remain live until removed by
            // retirement or whole-frame drain.
            unsafe { (**entry).token == token }
        }) else {
            return CRASH_CLEANUP_ARM_FAILED;
        };
        Some(entry)
    };

    if registry.entries.iter().any(|candidate| {
        if Some(*candidate) == entry {
            return false;
        }
        // SAFETY: all registry members are live stable boxes.
        let candidate = unsafe { &**candidate };
        candidate.active && crash_cleanup_ranges_overlap(candidate.slot, candidate.size, slot, size)
    }) {
        return CRASH_CLEANUP_ARM_FAILED;
    }

    if let Some(entry) = entry {
        // SAFETY: membership proves this is a live stable entry in the current
        // owner frame.
        let entry = unsafe { &mut *entry };
        if entry.owner_registry != ptr::from_mut(registry)
            || entry.owner_frame != owner_frame
            || entry.active
            || entry.slot != slot
            || entry.size != size
            || entry.align != align
            || entry.thunk as usize != thunk as usize
            || entry.storage != storage
            || entry.relocation != relocation
        {
            return CRASH_CLEANUP_ARM_FAILED;
        }
        // SAFETY: descriptor and allocation metadata were proven unchanged.
        if !unsafe { copy_crash_cleanup_snapshot(entry, slot) } {
            return CRASH_CLEANUP_ARM_FAILED;
        }
        entry.active = true;
        entry.run_state = CrashCleanupRunState::Pending;
        return token;
    }

    let Some(order) = registry.next_order.checked_add(1) else {
        return CRASH_CLEANUP_ARM_FAILED;
    };
    let Some(token) = next_crash_cleanup_token() else {
        return CRASH_CLEANUP_ARM_FAILED;
    };
    registry.next_order = order;
    let mut boxed = Box::new(CrashCleanupEntry {
        token,
        owner_registry: ptr::from_mut(registry),
        owner_frame,
        slot,
        snapshot: ptr::null_mut(),
        size,
        align,
        thunk,
        storage,
        relocation,
        active: false,
        order,
        run_state: CrashCleanupRunState::Pending,
    });
    // SAFETY: the entry is not yet published and its descriptor is complete.
    if !unsafe { copy_crash_cleanup_snapshot(&mut boxed, slot) } {
        return CRASH_CLEANUP_ARM_FAILED;
    }
    boxed.active = true;
    let entry = Box::into_raw(boxed);
    registry.entries.push(entry);
    token
}

/// Temporarily deactivate an entry before an ownership transfer, drop, or
/// overwrite. The stable token and first-activation order remain available for
/// a later reassignment/reactivation.
///
/// # Safety
///
/// A non-zero `token` must have been returned by
/// [`hew_cont_crash_cleanup_arm`] for the currently active tracked frame and
/// must still name an active, unretired entry.
#[no_mangle]
pub unsafe extern "C" fn hew_cont_crash_cleanup_deactivate(token: u64) -> bool {
    if token == 0 {
        return true;
    }
    if token == CRASH_CLEANUP_ARM_FAILED {
        return false;
    }
    let Some((registry, owner_frame)) = current_crash_cleanup_registry() else {
        return false;
    };
    // Resolve the process-unique generation through the live owner registry.
    // A forged/stale FFI integer therefore fails closed without becoming an
    // arbitrary raw-pointer read or aliasing a recycled allocation address.
    // SAFETY: the executing frame exclusively owns its registry.
    let entries = unsafe { &(*registry).entries };
    let Some(&entry) = entries.iter().find(|entry| {
        // SAFETY: all registry members are live stable boxes.
        unsafe { (***entry).token == token }
    }) else {
        return false;
    };
    // SAFETY: registry membership proves a live stable token.
    let entry = unsafe { &mut *entry };
    if entry.owner_registry != registry || entry.owner_frame != owner_frame || !entry.active {
        return false;
    }
    entry.active = false;
    true
}

/// Permanently retire a stable entry at lexical lifetime end.
///
/// # Safety
///
/// A non-zero `token` must have been returned by
/// [`hew_cont_crash_cleanup_arm`] for the currently active tracked frame and
/// must not already have been retired.
#[no_mangle]
pub unsafe extern "C" fn hew_cont_crash_cleanup_retire(token: u64) -> bool {
    if token == 0 {
        return true;
    }
    if token == CRASH_CLEANUP_ARM_FAILED {
        return false;
    }
    let Some((registry, owner_frame)) = current_crash_cleanup_registry() else {
        return false;
    };
    // SAFETY: frame lifecycle excludes concurrent mutation.
    let entries = unsafe { &mut (*registry).entries };
    let Some(index) = entries.iter().position(|entry| {
        // SAFETY: all registry members are live stable boxes.
        unsafe {
            (**entry).token == token
                && (**entry).owner_registry == registry
                && (**entry).owner_frame == owner_frame
        }
    }) else {
        return false;
    };
    let removed = entries.remove(index);
    // SAFETY: removal transfers sole ownership of the stable entry.
    unsafe { free_crash_cleanup_entry(removed, false) };
    true
}

fn active_dispatch_cleanup_registry() -> Option<*mut CrashCleanupRegistry> {
    if crash_cleanup_drain_active() {
        return None;
    }
    DISPATCH_CRASH_CLEANUP_SCOPES.with(|scopes| scopes.borrow().last().copied())
}

fn current_crash_cleanup_registry() -> Option<(*mut CrashCleanupRegistry, *mut c_void)> {
    if crash_cleanup_drain_active() {
        return None;
    }
    if let Some(frame) = active_top_tracked_frame() {
        // SAFETY: the tracked frame is live and exclusively executing. Lazily
        // allocate its registry exactly as the historical frame-only path did.
        let registry = unsafe { ensure_frame_cleanup_registry(frame) };
        return Some((registry, frame));
    }
    active_dispatch_cleanup_registry().map(|registry| (registry, ptr::null_mut()))
}

/// Open one cooperative scheduler dispatch crash domain.
///
/// The actor-state bytes are escrowed before generated handler code runs. The
/// state snapshot is not an independent live owner on the normal path: it is
/// raw-discarded at dispatch completion. Crash recovery instead runs the
/// generated state-drop thunk on the escrow and later raw-frees the abandoned
/// original state wrapper.
///
/// Returns `false` only when the state escrow could not be represented.
///
/// # Safety
///
/// `state`, when non-null, must name `state_size` initialized bytes and remain
/// live through the matching finish/recover call. `state_drop` must be the
/// generated typed drop thunk for those bytes.
pub(crate) unsafe fn begin_dispatch_crash_cleanup(
    state: *mut c_void,
    state_size: usize,
    state_drop: Option<StateCrashCleanupThunk>,
) -> bool {
    if crash_cleanup_drain_active() {
        return false;
    }
    let mut registry = Box::new(CrashCleanupRegistry::default());
    if !state.is_null() && state_size != 0 {
        let Ok(state_size_u64) = u64::try_from(state_size) else {
            return false;
        };
        registry.state_source = state.cast();
        registry.state_size = state_size_u64;
    }
    if !state.is_null() && state_size != 0 && state_drop.is_some() {
        let Ok(state_size_u64) = u64::try_from(state_size) else {
            return false;
        };
        // Derive a conservative effective alignment from the live state
        // allocation itself. Every valid LLVM actor-state layout's ABI
        // alignment divides this address, so allocating the escrow at the
        // address's largest power-of-two divisor is at least as aligned as the
        // generated typed drop thunk requires. This avoids assuming that every
        // actor state fits the coroutine frame's fixed 16-byte alignment.
        let state_addr = state as usize;
        let state_align = (1usize << state_addr.trailing_zeros()).min(MAX_STATE_ESCROW_ALIGN);
        let Ok(state_align_u64) = u64::try_from(state_align) else {
            return false;
        };
        // SAFETY: hew_alloc validates this non-zero, power-of-two layout.
        let snapshot = unsafe { hew_alloc(state_size_u64, state_align_u64) };
        if snapshot.is_null() {
            return false;
        }
        // SAFETY: caller guarantees the source range; snapshot is a distinct
        // allocation of the exact same size.
        unsafe { ptr::copy_nonoverlapping(state.cast::<u8>(), snapshot, state_size) };
        registry.state_snapshot = snapshot;
        registry.state_align = state_align_u64;
        // Generated state drops are LLVM functions using the platform C
        // calling convention. The callback pointer representation matches the
        // runtime's C-unwind slot, but this does not manufacture containment:
        // their plain-C runtime callees abort if a Rust panic reaches that ABI.
        registry.state_drop = state_drop.map(|drop| {
            // SAFETY: generated state thunks use the platform C calling
            // convention; C-unwind has the same argument/result machine ABI.
            unsafe { std::mem::transmute::<StateCrashCleanupThunk, CrashCleanupThunk>(drop) }
        });
    }
    let registry = Box::into_raw(registry);
    DISPATCH_CRASH_CLEANUP_SCOPES.with(|scopes| scopes.borrow_mut().push(registry));
    true
}

fn take_dispatch_crash_cleanup_registry() -> Option<*mut CrashCleanupRegistry> {
    if crash_cleanup_drain_active() {
        return None;
    }
    DISPATCH_CRASH_CLEANUP_SCOPES.with(|scopes| scopes.borrow_mut().pop())
}

/// Close a normally returned dispatch scope.
///
/// Returns false if generated code left a typed lexical token registered. The
/// scheduler treats that as a compiler/runtime invariant failure rather than
/// silently discarding an owner.
pub(crate) unsafe fn finish_dispatch_crash_cleanup() -> bool {
    let Some(registry) = take_dispatch_crash_cleanup_registry() else {
        return false;
    };
    // SAFETY: detachment gives this function sole registry access.
    let has_entries = unsafe { !(*registry).entries.is_empty() };
    // A normal return after an unclosed non-idempotent finalizer phase is a
    // compiler/runtime invariant failure, not a recoverable cleanup omission.
    // SAFETY: detachment gives this function sole registry access.
    if unsafe { (*registry).state_finalizer_critical } {
        hew_dispatch_state_cleanup_abort_invariant();
    }
    // SAFETY: normal return owns the original actor state and every lexical
    // value; discard escrow bytes only, never run typed drops.
    let faulted = unsafe { free_crash_cleanup_registry(registry, false, false) };
    debug_assert!(!faulted, "normal cleanup discard cannot invoke finalizers");
    !has_entries
}

/// Drain a crash-abandoned dispatch scope in lexical LIFO order.
///
/// `drop_state` is true for cooperative Hew actor crashes. Test-only/raw Rust
/// unwinds on an unwind-capable host parity build may drain abandoned lexical
/// owners while preserving the original state. Production wasm32-wasip1 is
/// panic=abort and has no recoverable unwind edge.
#[cfg(test)]
unsafe fn recover_dispatch_crash_cleanup(drop_state: bool) -> bool {
    // SAFETY: this wrapper forwards the caller's exclusive recovery authority.
    let outcome = unsafe { recover_dispatch_crash_cleanup_with_outcome(drop_state) };
    outcome.registry_found && !outcome.finalizer_faulted
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub(crate) struct DispatchCrashCleanupOutcome {
    pub registry_found: bool,
    /// The detached escrow held the only remaining typed drop authority for
    /// actor state and has now consumed/quarantined it. Final actor teardown
    /// must raw-free the original wrapper without invoking `state_drop` again.
    pub state_authority_consumed: bool,
    pub finalizer_faulted: bool,
}

/// Outcome-bearing crash drain used by schedulers to transfer actor-state
/// teardown authority explicitly. The bool wrapper above remains for existing
/// frame/registry callers that only need success/failure.
pub(crate) unsafe fn recover_dispatch_crash_cleanup_with_outcome(
    drop_state: bool,
) -> DispatchCrashCleanupOutcome {
    let Some(registry) = take_dispatch_crash_cleanup_registry() else {
        return DispatchCrashCleanupOutcome::default();
    };
    // No failure may actor-recover from an indeterminate old-value finalizer.
    // A rejected prepare call leaves the phase active; only a later successful
    // exact-token preparation can close it.
    // SAFETY: detachment transfers exclusive registry ownership here.
    if unsafe { (*registry).state_finalizer_critical } {
        hew_dispatch_state_cleanup_abort_invariant();
    }
    // SAFETY: the registry has been detached from TLS and is exclusively ours.
    // A non-null state snapshot exists iff begin accepted a generated typed
    // state drop. Once its invocation begins it must never be retried, even if
    // that finalizer faults after partially closing a resource.
    let state_mutation_began = unsafe { (*registry).state_mutation_began };
    let consume_state = drop_state || state_mutation_began;
    // SAFETY: the detached registry remains exclusively owned here until the
    // drain call below consumes it.
    let state_authority_consumed =
        consume_state && unsafe { !(*registry).state_snapshot.is_null() };
    // Detach before calling arbitrary generated/user cleanup code so re-entry
    // cannot observe a half-drained scope.
    // SAFETY: the popped TLS entry transfers sole registry ownership here.
    let faulted = unsafe { free_crash_cleanup_registry(registry, true, consume_state) };
    if faulted {
        eprintln!(
            "hew: crash-cleanup finalizer panicked; the failed owner was quarantined and remaining owners were drained"
        );
    }
    DispatchCrashCleanupOutcome {
        registry_found: true,
        state_authority_consumed,
        finalizer_faulted: faulted,
    }
}

fn dispatch_state_snapshot_range(
    field: *mut c_void,
    size: u64,
) -> Option<(*mut CrashCleanupRegistry, *mut u8, *mut u8, usize)> {
    let registry = active_dispatch_cleanup_registry()?;
    // SAFETY: the active scheduler scope exclusively owns this registry.
    let registry = unsafe { &mut *registry };
    let size = usize::try_from(size).ok()?;
    if field.is_null() || size == 0 {
        return None;
    }
    let source_start = registry.state_source as usize;
    let field_start = field as usize;
    let offset = field_start.checked_sub(source_start)?;
    let state_size = usize::try_from(registry.state_size).ok()?;
    if offset.checked_add(size)? > state_size {
        return None;
    }
    // SAFETY: the validated range lies in both equal-sized source/snapshot
    // allocations.
    let snapshot = if registry.state_snapshot.is_null() {
        ptr::null_mut()
    } else {
        // SAFETY: offset + size was checked inside the escrow range above.
        unsafe { registry.state_snapshot.add(offset) }
    };
    // SAFETY: offset + size was checked within the live source range above.
    Some(unsafe {
        (
            ptr::from_mut(registry),
            registry.state_source.add(offset),
            snapshot,
            size,
        )
    })
}

unsafe fn reset_state_finalizer_critical(registry: *mut CrashCleanupRegistry) {
    // SAFETY: callers hold exclusive active or detached registry authority.
    if unsafe { (*registry).state_finalizer_critical } {
        // Keep the signal-visible depth active until registry state is closed.
        // SAFETY: callers hold exclusive active or detached registry authority.
        unsafe { (*registry).state_finalizer_critical = false };
        if !crate::signal::leave_state_field_finalizer() {
            hew_dispatch_state_cleanup_abort_invariant();
        }
    }
}

/// Enter the non-idempotent actor-state overwrite phase and neutralize the old
/// escrow field before generated code begins its release ritual.
///
/// All validation precedes mutation. Once accepted, hardware faults,
/// intentional Hew traps, and unwind attempts are process-fatal until
/// prepare/prepare-transfer establishes replacement escrow authority.
///
/// # Safety
///
/// `field..field+size` must be a live field range in the current actor state.
#[no_mangle]
pub unsafe extern "C" fn hew_dispatch_state_cleanup_begin_replace(
    field: *mut c_void,
    size: u64,
) -> bool {
    let Some((registry, _source, snapshot, size)) = dispatch_state_snapshot_range(field, size)
    else {
        return false;
    };
    // SAFETY: range validation returned the exclusively active registry.
    if unsafe { (*registry).state_finalizer_critical } {
        return false;
    }
    // Publish one nested process-fatal phase before changing typed escrow
    // bytes. Overflow cannot wrap to zero and conceal active outer phases.
    if !crate::signal::enter_state_field_finalizer() {
        hew_dispatch_state_cleanup_abort_invariant();
    }
    // SAFETY: no fallible work remains after the signal-visible phase begins.
    unsafe {
        (*registry).state_finalizer_critical = true;
        (*registry).state_mutation_began = true;
    }
    if !snapshot.is_null() {
        // SAFETY: range validation proved `size` writable snapshot bytes.
        unsafe { ptr::write_bytes(snapshot, 0, size) };
    }
    true
}

/// Remove one actor-state field from the crash escrow before its old-value
/// release/overwrite begins. Zero is the generated state-drop spine's neutral
/// representation for every owned state leaf (pointer handles and recursively
/// null-filled aggregates).
///
/// # Safety
///
/// `field..field+size` must be a live field range within the actor state named
/// by the current dispatch cleanup scope.
#[no_mangle]
pub unsafe extern "C" fn hew_dispatch_state_cleanup_clear(field: *mut c_void, size: u64) -> bool {
    let Some((registry, _source, snapshot, size)) = dispatch_state_snapshot_range(field, size)
    else {
        return false;
    };
    if snapshot.is_null() {
        return true;
    }
    // Commit the one-way authority transition before touching snapshot bytes.
    // A caught unwind after this point must consume the typed snapshot and raw
    // free the possibly stale live wrapper.
    // SAFETY: range validation returned the active registry exclusively owned
    // by this dispatch.
    unsafe { (*registry).state_mutation_began = true };
    // SAFETY: range validation above proved `size` writable snapshot bytes.
    unsafe { ptr::write_bytes(snapshot, 0, size) };
    true
}

/// Prepare a fully initialized replacement field in crash escrow before the
/// corresponding live actor-state store becomes observable.
///
/// # Safety
///
/// `replacement..replacement+size` must contain the exact initialized bytes
/// that generated code will subsequently store into `field`; the field range
/// must belong to the current dispatch cleanup scope.
#[no_mangle]
pub unsafe extern "C" fn hew_dispatch_state_cleanup_prepare(
    replacement: *const c_void,
    field: *mut c_void,
    size: u64,
) {
    if replacement.is_null() {
        hew_dispatch_state_cleanup_abort_invariant();
    }
    let Some((registry, _source, snapshot, size)) = dispatch_state_snapshot_range(field, size)
    else {
        hew_dispatch_state_cleanup_abort_invariant();
    };
    // SAFETY: range validation returned the exclusively active registry.
    if unsafe { !(*registry).state_finalizer_critical } {
        hew_dispatch_state_cleanup_abort_invariant();
    }
    // Preparation replaces the neutralized escrow field with the incoming owner
    // after generated code releases the old live field and before it exposes the
    // replacement through the live store. Mark it before copying so a fault
    // during this copy remains process-fatal instead of consuming partial bytes.
    // SAFETY: range validation returned the active registry exclusively owned
    // by this dispatch.
    unsafe { (*registry).state_mutation_began = true };
    if !snapshot.is_null() {
        // SAFETY: caller guarantees the initialized replacement range and range
        // validation proved the snapshot destination range.
        unsafe { ptr::copy_nonoverlapping(replacement.cast::<u8>(), snapshot, size) };
    }
    // Replacement escrow is authoritative before the process-fatal phase is
    // relaxed. SAFETY: registry remains exclusively active.
    unsafe { reset_state_finalizer_critical(registry) };
}

/// Terminate on an impossible post-begin actor-state transaction mismatch.
/// Actor recovery would leak an untracked materialized replacement on the
/// no-source path, so this boundary must never degrade to cooperative panic.
#[cold]
#[no_mangle]
pub extern "C" fn hew_dispatch_state_cleanup_abort_invariant() -> ! {
    eprintln!("hew: actor-state cleanup preparation invariant failed");
    std::process::abort();
}

/// Prepare an actor-state replacement from one active lexical cleanup token
/// before the corresponding live store becomes observable.
///
/// Validation is completed before either authority changes. On `false`, the
/// source token remains active and the escrow is untouched. After validation,
/// the escrow holds the exact replacement bytes and the lexical token is
/// inactive, so recovery has one typed authority before and after the live
/// store.
///
/// # Safety
///
/// `token` must name `owner_source`, the initialized lexical owner being
/// consumed. `replacement..replacement+size` must contain the exact bytes
/// generated code will subsequently store into `field`; it may differ from
/// `owner_source` after borrow-gated materialization.
#[no_mangle]
pub unsafe extern "C" fn hew_dispatch_state_cleanup_prepare_transfer(
    token: u64,
    owner_source: *mut c_void,
    replacement: *const c_void,
    field: *mut c_void,
    size: u64,
) -> bool {
    if token == 0 || token == CRASH_CLEANUP_ARM_FAILED || replacement.is_null() {
        return false;
    }
    let Some((state_registry, _source, snapshot, size)) =
        dispatch_state_snapshot_range(field, size)
    else {
        return false;
    };
    // SAFETY: range validation returned the exclusively active registry. A
    // transfer outside the begun replace phase is a compiler/runtime mismatch.
    if unsafe { !(*state_registry).state_finalizer_critical } {
        return false;
    }
    let Some((registry, owner_frame)) = current_crash_cleanup_registry() else {
        return false;
    };
    // SAFETY: the executing generated code exclusively owns its current
    // registry and every member remains boxed until retirement/drain.
    let entries = unsafe { &(*registry).entries };
    let Some(&entry) = entries.iter().find(|entry| {
        // SAFETY: registry membership keeps the stable entry live.
        unsafe { (***entry).token == token }
    }) else {
        return false;
    };
    // SAFETY: membership above proves the stable entry remains live.
    let entry = unsafe { &mut *entry };
    if entry.owner_registry != registry
        || entry.owner_frame != owner_frame
        || !entry.active
        || entry.slot != owner_source
        || usize::try_from(entry.size).ok() != Some(size)
    {
        return false;
    }

    // All validation precedes authority mutation. The following flag write,
    // memcpy, and source deactivation call no generated/user code.
    if !snapshot.is_null() {
        // SAFETY: range validation returned the active state registry, and all
        // source-token validation completed before this authority mutation.
        unsafe { (*state_registry).state_mutation_began = true };
        // SAFETY: caller guarantees the initialized replacement range;
        // dispatch_state_snapshot_range proved the destination range.
        unsafe { ptr::copy_nonoverlapping(replacement.cast::<u8>(), snapshot, size) };
    }
    // Commit lexical handoff only after the replacement bytes are prepared.
    entry.active = false;
    // Escrow now owns the replacement and the lexical source is inactive.
    // SAFETY: state_registry remains exclusively active.
    unsafe { reset_state_finalizer_critical(state_registry) };
    true
}

/// Return the currently executing positively tracked coroutine frame.
///
/// Individual typed slots are range-checked when they are armed.
pub(crate) fn active_top_tracked_frame() -> Option<*mut c_void> {
    ACTIVE_COROUTINE_FRAMES.with(|active| {
        let frame = active.borrow().last()?.frame;
        // SAFETY: the frame came from the live active-frame stack.
        unsafe { frame_is_tracked(frame) }.then_some(frame)
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

fn active_coroutine_enter_phase(frame: *mut c_void, phase: ActiveCoroutinePhase) -> bool {
    // SAFETY: this helper is called only with a live continuation handle. The
    // marker gates admission so untracked companion/environment allocations can
    // never enter the raw crash-reclamation authority.
    if frame.is_null() || !unsafe { frame_is_tracked(frame) } {
        return false;
    }
    ACTIVE_COROUTINE_FRAMES.with(|active| {
        active
            .borrow_mut()
            .push(ActiveCoroutineFrame { frame, phase });
    });
    true
}

fn active_coroutine_enter(frame: *mut c_void) -> bool {
    active_coroutine_enter_phase(frame, ActiveCoroutinePhase::Resume)
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

fn active_coroutine_contains(frame: *mut c_void) -> bool {
    ACTIVE_COROUTINE_FRAMES
        .with(|active| active.borrow().iter().any(|record| record.frame == frame))
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
    // Transfer the abandoned activation stack out of TLS before invoking any
    // typed thunk. Resource close functions are arbitrary generated code and
    // may allocate, hand off, destroy, or even crash-recover another tracked
    // continuation; holding RefCell::borrow_mut across that re-entry would
    // panic before the cleanup authority could finish.
    let mut abandoned =
        ACTIVE_COROUTINE_FRAMES.with(|active| std::mem::take(&mut *active.borrow_mut()));
    let mut retained_excluded = None;
    let mut reclaimed = 0;
    while let Some(record) = abandoned.pop() {
        let frame = record.frame;
        // Typed escrow is independent of raw-allocation ownership. Even an
        // excluded resumed root has lost its running stack and therefore
        // must discharge its typed owners now; only its empty allocation
        // remains reserved for the actor-slot authority.
        // SAFETY: every active record names a live tracked frame abandoned
        // by the recovered native stack.
        unsafe { run_frame_crash_cleanups(frame) };
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
        ACTIVE_COROUTINE_FRAMES.with(|active| {
            // Reentrant thunks may have established newer live activations.
            // The retained scheduler-owned root predates them, so restore it
            // at the bottom rather than claiming the top-of-stack position.
            active.borrow_mut().insert(0, record);
        });
    }
    reclaimed
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
    // Make the frame registry discoverable to cleanup-outline deactivate /
    // retire calls. The outline's eventual `hew_cont_frame_free` removes this
    // active record; a hand-built/no-free destroy thunk falls back to the
    // pointer-only leave below.
    let tracked = active_coroutine_enter_phase(handle, ActiveCoroutinePhase::Destroy);
    // SAFETY: handle is a live, not-yet-destroyed coroutine frame per contract.
    unsafe { coro_destroy(handle) };
    if tracked {
        let left = active_coroutine_leave(handle, ActiveCoroutinePhase::Destroy);
        debug_assert!(
            left || !active_coroutine_contains(handle),
            "tracked coroutine destroy returned with a mismatched active-frame stack"
        );
    }
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

    thread_local! {
        static CRASH_CLEANUP_TEST_DROPS: RefCell<Vec<u64>> =
            const { RefCell::new(Vec::new()) };
        static CRASH_CLEANUP_PANIC_PAYLOAD_DROPS: Cell<u64> = const { Cell::new(0) };
    }

    static MIGRATED_CRASH_CLEANUP_VALUE: AtomicU64 = AtomicU64::new(0);

    unsafe extern "C-unwind" fn record_u64_cleanup(slot: *mut c_void) {
        // SAFETY: these tests register only initialized u64 slots/snapshots.
        let value = unsafe { ptr::read_unaligned(slot.cast::<u64>()) };
        CRASH_CLEANUP_TEST_DROPS.with(|drops| drops.borrow_mut().push(value));
    }

    unsafe extern "C" fn record_u64_state_cleanup(slot: *mut c_void) {
        // SAFETY: state-cleanup tests use the same initialized u64 contract.
        unsafe { record_u64_cleanup(slot) };
    }

    unsafe extern "C-unwind" fn record_migrated_u64_cleanup(slot: *mut c_void) {
        // SAFETY: the migration test registers one initialized u64 frame slot.
        let value = unsafe { ptr::read_unaligned(slot.cast::<u64>()) };
        MIGRATED_CRASH_CLEANUP_VALUE.store(value, Ordering::Release);
    }

    unsafe extern "C-unwind" fn record_u64_cleanup_with_cont_reentry(slot: *mut c_void) {
        // SAFETY: same initialized u64 contract as record_u64_cleanup.
        unsafe { record_u64_cleanup(slot) };
        // Exercise every TLS mutation that used to collide with the outer
        // drain's RefCell::borrow_mut: tracked allocation pushes, handoff pops,
        // and ordinary free checks/removes a matching activation.
        let nested = hew_cont_frame_alloc_tracked(16);
        assert!(!nested.is_null());
        hew_cont_frame_handoff(nested);
        // SAFETY: nested is a live allocation from the tracked allocator.
        unsafe { hew_cont_frame_free(nested) };
    }

    struct CleanupPanicPayload {
        _allocation_marker: u64,
    }

    impl Drop for CleanupPanicPayload {
        fn drop(&mut self) {
            CRASH_CLEANUP_PANIC_PAYLOAD_DROPS.with(|drops| drops.set(drops.get() + 1));
            // Payload disposal is itself arbitrary Rust code. This second
            // panic must also remain inside the cleanup quarantine.
            panic!("intentional panic-payload destructor fault");
        }
    }

    fn take_crash_cleanup_panic_payload_drops() -> u64 {
        CRASH_CLEANUP_PANIC_PAYLOAD_DROPS.with(|drops| drops.replace(0))
    }

    unsafe extern "C-unwind" fn record_u64_cleanup_then_panic(slot: *mut c_void) {
        // SAFETY: tests register initialized u64 snapshots.
        unsafe { record_u64_cleanup(slot) };
        std::panic::panic_any(CleanupPanicPayload {
            _allocation_marker: 0x4845_5750_414e_4943,
        });
    }

    #[cfg(unix)]
    unsafe extern "C-unwind" fn raise_sigsegv_during_crash_cleanup(_slot: *mut c_void) {
        // SAFETY: raising SIGSEGV is intentional in the subprocess death test;
        // the installed native handler must take its async-signal-safe fatal
        // edge before this callback can return.
        unsafe { libc::raise(libc::SIGSEGV) };
        panic!("SIGSEGV cleanup finalizer unexpectedly returned");
    }

    unsafe extern "C-unwind" fn trap_during_crash_cleanup(_slot: *mut c_void) {
        crate::actor::hew_panic();
    }

    static NESTED_DRAIN_REENTRY_REFUSED: AtomicU64 = AtomicU64::new(0);

    unsafe extern "C-unwind" fn attempt_nested_dispatch_drain(slot: *mut c_void) {
        // SAFETY: tests register initialized u64 snapshots.
        unsafe { record_u64_cleanup(slot) };
        // A detached drain must hide every older dispatch registry.
        // SAFETY: this callback runs inside the active detached drain whose
        // reentry refusal is the condition under test.
        if !unsafe { recover_dispatch_crash_cleanup(false) } {
            NESTED_DRAIN_REENTRY_REFUSED.fetch_add(1, Ordering::SeqCst);
        }
    }

    unsafe extern "C" fn noop_dyn_drop(_data: *mut u8) {}

    static U64_DYN_VTABLE: crate::trait_object::HewVtable = crate::trait_object::HewVtable {
        drop_in_place: noop_dyn_drop,
        size_of: size_of::<u64>(),
        align_of: align_of::<u64>(),
    };

    fn take_crash_cleanup_test_drops() -> Vec<u64> {
        CRASH_CLEANUP_TEST_DROPS.with(|drops| std::mem::take(&mut *drops.borrow_mut()))
    }

    #[test]
    fn crash_cleanup_new_arm_without_active_frame_is_benign() {
        let mut value = 7_u64;
        // SAFETY: value is a live initialized u64 slot for the duration of arm.
        let token = unsafe {
            hew_cont_crash_cleanup_arm(
                0,
                ptr::from_mut(&mut value).cast(),
                size_of::<u64>() as u64,
                align_of::<u64>() as u64,
                Some(record_u64_cleanup),
                CrashCleanupStorage::Snapshot as u32,
                CrashCleanupRelocation::Bitwise as u32,
            )
        };
        assert_eq!(token, 0, "new arm outside a tracked frame is a no-op");
        // SAFETY: token zero is the documented benign no-op sibling.
        assert!(unsafe { hew_cont_crash_cleanup_deactivate(token) });
        // SAFETY: token zero is also the documented benign retire no-op.
        assert!(unsafe { hew_cont_crash_cleanup_retire(token) });
    }

    #[test]
    fn dispatch_crash_cleanup_tracks_ordinary_stack_owners_lifo() {
        let _ = take_crash_cleanup_test_drops();
        // SAFETY: the scheduler-style scope brackets both initialized stack
        // snapshots and crash recovery consumes the detached escrow exactly
        // once.
        unsafe {
            assert!(begin_dispatch_crash_cleanup(ptr::null_mut(), 0, None));
            let mut outer_value = 41_u64;
            let mut nested_value = 42_u64;
            for slot in [
                ptr::from_mut(&mut outer_value).cast(),
                ptr::from_mut(&mut nested_value).cast(),
            ] {
                let token = hew_cont_crash_cleanup_arm(
                    0,
                    slot,
                    size_of::<u64>() as u64,
                    align_of::<u64>() as u64,
                    Some(record_u64_cleanup),
                    CrashCleanupStorage::Snapshot as u32,
                    CrashCleanupRelocation::Bitwise as u32,
                );
                assert_ne!(token, 0, "dispatch fallback must issue a real token");
                assert_ne!(token, CRASH_CLEANUP_ARM_FAILED);
            }
            assert!(recover_dispatch_crash_cleanup(true));
        }
        assert_eq!(
            take_crash_cleanup_test_drops(),
            [42, 41],
            "callee/transitive owner must release before its caller owner"
        );
    }

    #[test]
    fn normal_dispatch_retirement_discards_escrow_without_drop() {
        let _ = take_crash_cleanup_test_drops();
        // SAFETY: the initialized stack snapshot remains live through token
        // retirement and the matching normal scope finish.
        unsafe {
            assert!(begin_dispatch_crash_cleanup(ptr::null_mut(), 0, None));
            let mut owner = 51_u64;
            let token = hew_cont_crash_cleanup_arm(
                0,
                ptr::from_mut(&mut owner).cast(),
                size_of::<u64>() as u64,
                align_of::<u64>() as u64,
                Some(record_u64_cleanup),
                CrashCleanupStorage::Snapshot as u32,
                CrashCleanupRelocation::Bitwise as u32,
            );
            assert_ne!(token, 0);
            assert!(hew_cont_crash_cleanup_retire(token));
            assert!(finish_dispatch_crash_cleanup());
        }
        assert!(
            take_crash_cleanup_test_drops().is_empty(),
            "normal return must never run the crash-only typed thunk"
        );
    }

    #[test]
    fn dispatch_state_transfer_rejects_without_registry() {
        let mut field = 90_u64;
        // SAFETY: the field range is valid; absence of a dispatch registry is
        // the condition under test and must fail before any access/mutation.
        assert!(!unsafe {
            hew_dispatch_state_cleanup_prepare_transfer(
                1,
                ptr::from_mut(&mut field).cast(),
                ptr::from_mut(&mut field).cast(),
                ptr::from_mut(&mut field).cast(),
                size_of::<u64>() as u64,
            )
        });
    }

    #[test]
    fn dispatch_state_transfer_failures_leave_source_active() {
        let _ = take_crash_cleanup_test_drops();
        let mut state = 90_u64;
        let mut source = 91_u64;
        let mut replacement = 92_u64;
        // SAFETY: both initialized u64 slots remain live through recovery; the
        // transaction is exercised with forged token/range inputs before the
        // one valid authority handoff.
        unsafe {
            assert!(begin_dispatch_crash_cleanup(
                ptr::from_mut(&mut state).cast(),
                size_of::<u64>(),
                Some(record_u64_state_cleanup),
            ));
            let token = hew_cont_crash_cleanup_arm(
                0,
                ptr::from_mut(&mut source).cast(),
                size_of::<u64>() as u64,
                align_of::<u64>() as u64,
                Some(record_u64_cleanup),
                CrashCleanupStorage::Snapshot as u32,
                CrashCleanupRelocation::Bitwise as u32,
            );
            assert_ne!(token, 0);
            assert_ne!(token, CRASH_CLEANUP_ARM_FAILED);
            assert!(hew_dispatch_state_cleanup_begin_replace(
                ptr::from_mut(&mut state).cast(),
                size_of::<u64>() as u64,
            ));
            assert!(!hew_dispatch_state_cleanup_prepare_transfer(
                token.wrapping_add(1),
                ptr::from_mut(&mut source).cast(),
                ptr::from_mut(&mut replacement).cast(),
                ptr::from_mut(&mut state).cast(),
                size_of::<u64>() as u64,
            ));
            let outside = ptr::from_mut(&mut state)
                .cast::<u8>()
                .wrapping_add(size_of::<u64>());
            assert!(!hew_dispatch_state_cleanup_prepare_transfer(
                token,
                ptr::from_mut(&mut source).cast(),
                ptr::from_mut(&mut replacement).cast(),
                outside.cast(),
                size_of::<u64>() as u64,
            ));

            // Both rejected validations left the source active, so the same
            // token can still complete the one valid transfer.
            assert!(hew_dispatch_state_cleanup_prepare_transfer(
                token,
                ptr::from_mut(&mut source).cast(),
                ptr::from_mut(&mut replacement).cast(),
                ptr::from_mut(&mut state).cast(),
                size_of::<u64>() as u64,
            ));
            // Counterfactual async crash window: the live state still contains
            // its stale old bytes. Prepared escrow must nevertheless own and
            // drop the actual replacement, while the lexical token is inactive.
            assert!(recover_dispatch_crash_cleanup(true));
        }
        assert_eq!(
            take_crash_cleanup_test_drops(),
            [92],
            "pre-store token handoff must drop the materialized replacement, not the stale state or source bytes"
        );
    }

    #[test]
    fn dispatch_state_transfer_rejects_valid_token_for_different_source() {
        let _ = take_crash_cleanup_test_drops();
        let mut state = 71_u64;
        let mut intended = 71_u64;
        let mut forged = 72_u64;
        // SAFETY: all test slots are initialized and remain live through the
        // rejected forged transfer and final authoritative recovery.
        unsafe {
            assert!(begin_dispatch_crash_cleanup(
                ptr::from_mut(&mut state).cast(),
                size_of::<u64>(),
                Some(record_u64_state_cleanup),
            ));
            let intended_token = hew_cont_crash_cleanup_arm(
                0,
                ptr::from_mut(&mut intended).cast(),
                size_of::<u64>() as u64,
                align_of::<u64>() as u64,
                Some(record_u64_cleanup),
                CrashCleanupStorage::Snapshot as u32,
                CrashCleanupRelocation::Bitwise as u32,
            );
            let forged_token = hew_cont_crash_cleanup_arm(
                0,
                ptr::from_mut(&mut forged).cast(),
                size_of::<u64>() as u64,
                align_of::<u64>() as u64,
                Some(record_u64_cleanup),
                CrashCleanupStorage::Snapshot as u32,
                CrashCleanupRelocation::Bitwise as u32,
            );
            assert_ne!(intended_token, CRASH_CLEANUP_ARM_FAILED);
            assert_ne!(forged_token, CRASH_CLEANUP_ARM_FAILED);
            assert!(hew_dispatch_state_cleanup_begin_replace(
                ptr::from_mut(&mut state).cast(),
                size_of::<u64>() as u64,
            ));
            assert!(!hew_dispatch_state_cleanup_prepare_transfer(
                forged_token,
                ptr::from_mut(&mut intended).cast(),
                ptr::from_mut(&mut intended).cast(),
                ptr::from_mut(&mut state).cast(),
                size_of::<u64>() as u64,
            ));
            // The rejected call mutated nothing, so the intended token can
            // still complete preparation and close the critical phase.
            assert!(hew_dispatch_state_cleanup_prepare_transfer(
                intended_token,
                ptr::from_mut(&mut intended).cast(),
                ptr::from_mut(&mut intended).cast(),
                ptr::from_mut(&mut state).cast(),
                size_of::<u64>() as u64,
            ));
            assert!(recover_dispatch_crash_cleanup(true));
        }
        assert_eq!(
            take_crash_cleanup_test_drops(),
            [72, 71],
            "wrong-token rejection must leave both independent source authorities active"
        );
    }

    #[repr(C)]
    struct CrashStatePair {
        updated: *mut u64,
        sibling: *mut u64,
    }

    unsafe extern "C" fn drop_crash_state_pair(state: *mut c_void) {
        let state = state.cast::<CrashStatePair>();
        for field in [
            // SAFETY: the state escrow has this exact repr(C) shape.
            unsafe { &raw mut (*state).updated },
            // SAFETY: the state escrow has this exact repr(C) shape.
            unsafe { &raw mut (*state).sibling },
        ] {
            // SAFETY: field points into the writable snapshot.
            let value = unsafe { ptr::replace(field, ptr::null_mut()) };
            if !value.is_null() {
                // SAFETY: tests populate each live slot from Box::into_raw and
                // crash cleanup is its sole release authority.
                let value = unsafe { Box::from_raw(value) };
                CRASH_CLEANUP_TEST_DROPS.with(|drops| drops.borrow_mut().push(*value));
            }
        }
    }

    #[test]
    fn actor_state_transaction_operations_refuse_a_missing_dispatch_domain() {
        let mut field = 17_u64;
        // SAFETY: the field is live for the exact supplied size. Range
        // validity alone is deliberately insufficient: ordinary handler code
        // must not succeed when the scheduler forgot its state domain.
        unsafe {
            assert!(!hew_dispatch_state_cleanup_clear(
                ptr::from_mut(&mut field).cast(),
                size_of::<u64>() as u64,
            ));
            assert!(!hew_dispatch_state_cleanup_prepare_transfer(
                1,
                ptr::from_mut(&mut field).cast(),
                ptr::from_mut(&mut field).cast(),
                ptr::from_mut(&mut field).cast(),
                size_of::<u64>() as u64,
            ));
        }
    }

    #[test]
    fn actor_state_escrow_alignment_is_clamped_to_supported_abi_headroom() {
        #[repr(C, align(4096))]
        struct PageAlignedState(u64);

        let mut state = PageAlignedState(17);
        // SAFETY: state is initialized and remains live through normal escrow
        // retirement; the test inspects only the active registry metadata.
        unsafe {
            assert!(begin_dispatch_crash_cleanup(
                ptr::from_mut(&mut state).cast(),
                size_of::<PageAlignedState>(),
                Some(record_u64_state_cleanup),
            ));
            let registry = active_dispatch_cleanup_registry().expect("active dispatch registry");
            assert!((*registry).state_align <= MAX_STATE_ESCROW_ALIGN as u64);
            assert!((*registry).state_align.is_power_of_two());
            assert!(finish_dispatch_crash_cleanup());
        }
    }

    #[test]
    fn actor_state_escrow_withholds_only_inflight_field_and_drops_sibling_once() {
        let _ = take_crash_cleanup_test_drops();
        let mut state = CrashStatePair {
            updated: Box::into_raw(Box::new(61)),
            sibling: Box::into_raw(Box::new(62)),
        };
        let abandoned_updated = state.updated;
        // SAFETY: the initialized state remains live through matching recovery;
        // the generated-shape test drop owns the escrow only.
        unsafe {
            assert!(begin_dispatch_crash_cleanup(
                ptr::from_mut(&mut state).cast(),
                size_of::<CrashStatePair>(),
                Some(drop_crash_state_pair),
            ));
            // Model a trap during the old field's release: codegen clears the
            // escrow range before entering that arbitrary close ritual.
            assert!(hew_dispatch_state_cleanup_clear(
                ptr::from_mut(&mut state.updated).cast(),
                size_of::<*mut u64>() as u64,
            ));
            assert!(recover_dispatch_crash_cleanup(true));
            // The live-state wrapper is raw-abandoned on crash. Reclaim only
            // the withheld field's test allocation; sibling was owned by the
            // escrow drop and must not be touched again.
            drop(Box::from_raw(abandoned_updated));
        }
        assert_eq!(
            take_crash_cleanup_test_drops(),
            [62],
            "an in-flight field is withheld while every untouched sibling is released once"
        );
    }

    #[test]
    fn actor_state_escrow_prepares_no_source_owner_before_live_store() {
        let _ = take_crash_cleanup_test_drops();
        let mut state = CrashStatePair {
            updated: Box::into_raw(Box::new(71)),
            sibling: Box::into_raw(Box::new(72)),
        };
        let stale_old = state.updated;
        let replacement: *mut u64 = Box::into_raw(Box::new(73));
        // SAFETY: the initialized state remains live through matching recovery;
        // prepared escrow owns replacement while the stale live field is raw.
        unsafe {
            assert!(begin_dispatch_crash_cleanup(
                ptr::from_mut(&mut state).cast(),
                size_of::<CrashStatePair>(),
                Some(drop_crash_state_pair),
            ));
            assert!(hew_dispatch_state_cleanup_begin_replace(
                ptr::from_mut(&mut state.updated).cast(),
                size_of::<*mut u64>() as u64,
            ));
            // Old release completes under the fatal critical phase.
            drop(Box::from_raw(stale_old));
            hew_dispatch_state_cleanup_prepare(
                std::ptr::from_ref(&replacement).cast(),
                ptr::from_mut(&mut state.updated).cast(),
                size_of::<*mut u64>() as u64,
            );
            // Counterfactual async actor crash immediately before the store.
            assert!(recover_dispatch_crash_cleanup(true));
        }
        assert_eq!(
            take_crash_cleanup_test_drops(),
            [73, 72],
            "pre-store no-source preparation must release the new owner and sibling exactly once"
        );
    }

    #[test]
    fn actor_state_escrow_keeps_prepared_owner_after_live_store() {
        let _ = take_crash_cleanup_test_drops();
        let mut state = CrashStatePair {
            updated: Box::into_raw(Box::new(74)),
            sibling: Box::into_raw(Box::new(75)),
        };
        let replacement = Box::into_raw(Box::new(76));
        // SAFETY: old live ownership is released before the store; prepared
        // escrow remains the crash authority for the replacement afterward.
        unsafe {
            assert!(begin_dispatch_crash_cleanup(
                ptr::from_mut(&mut state).cast(),
                size_of::<CrashStatePair>(),
                Some(drop_crash_state_pair),
            ));
            assert!(hew_dispatch_state_cleanup_begin_replace(
                ptr::from_mut(&mut state.updated).cast(),
                size_of::<*mut u64>() as u64,
            ));
            drop(Box::from_raw(state.updated));
            hew_dispatch_state_cleanup_prepare(
                std::ptr::from_ref(&replacement).cast(),
                ptr::from_mut(&mut state.updated).cast(),
                size_of::<*mut u64>() as u64,
            );
            state.updated = replacement;
            assert_eq!(state.updated, replacement);
            assert!(recover_dispatch_crash_cleanup(true));
        }
        assert_eq!(
            take_crash_cleanup_test_drops(),
            [76, 75],
            "post-store crash must release the new field and untouched sibling exactly once"
        );
    }

    #[test]
    fn normal_actor_state_escrow_does_not_double_drop_live_state() {
        let _ = take_crash_cleanup_test_drops();
        let mut state = CrashStatePair {
            updated: Box::into_raw(Box::new(81)),
            sibling: Box::into_raw(Box::new(82)),
        };
        let replacement = Box::into_raw(Box::new(83));
        // SAFETY: the initialized state remains live through prepare/store and
        // matching finish, after which ordinary teardown is sole authority.
        unsafe {
            assert!(begin_dispatch_crash_cleanup(
                ptr::from_mut(&mut state).cast(),
                size_of::<CrashStatePair>(),
                Some(drop_crash_state_pair),
            ));
            assert!(hew_dispatch_state_cleanup_begin_replace(
                ptr::from_mut(&mut state.updated).cast(),
                size_of::<*mut u64>() as u64,
            ));
            drop(Box::from_raw(state.updated));
            hew_dispatch_state_cleanup_prepare(
                std::ptr::from_ref(&replacement).cast(),
                ptr::from_mut(&mut state.updated).cast(),
                size_of::<*mut u64>() as u64,
            );
            state.updated = replacement;
            assert!(finish_dispatch_crash_cleanup());
            assert!(take_crash_cleanup_test_drops().is_empty());
            // Normal actor teardown remains the sole typed owner.
            drop_crash_state_pair(ptr::from_mut(&mut state).cast());
        }
        assert_eq!(take_crash_cleanup_test_drops(), [83, 82]);
    }

    #[test]
    #[cfg(any(unix, windows))]
    fn nested_state_finalizer_completion_preserves_outer_depth() {
        crate::signal::init_crash_handling();
        crate::signal::init_worker_recovery(u32::MAX);
        assert_eq!(crate::signal::state_field_finalizer_depth(), 0);

        let mut outer = 101_u64;
        let outer_replacement = 102_u64;
        let mut inner = 201_u64;
        let inner_replacement = 202_u64;
        // SAFETY: both nested state slots and their replacements remain live
        // through their exactly nested prepare/finish transactions.
        unsafe {
            assert!(begin_dispatch_crash_cleanup(
                ptr::from_mut(&mut outer).cast(),
                size_of::<u64>(),
                Some(record_u64_state_cleanup),
            ));
            assert!(hew_dispatch_state_cleanup_begin_replace(
                ptr::from_mut(&mut outer).cast(),
                size_of::<u64>() as u64,
            ));
            assert_eq!(crate::signal::state_field_finalizer_depth(), 1);

            assert!(begin_dispatch_crash_cleanup(
                ptr::from_mut(&mut inner).cast(),
                size_of::<u64>(),
                Some(record_u64_state_cleanup),
            ));
            assert!(hew_dispatch_state_cleanup_begin_replace(
                ptr::from_mut(&mut inner).cast(),
                size_of::<u64>() as u64,
            ));
            assert_eq!(crate::signal::state_field_finalizer_depth(), 2);
            hew_dispatch_state_cleanup_prepare(
                ptr::from_ref(&inner_replacement).cast(),
                ptr::from_mut(&mut inner).cast(),
                size_of::<u64>() as u64,
            );
            assert_eq!(
                crate::signal::state_field_finalizer_depth(),
                1,
                "closing an inner transaction must preserve the outer fatal guard"
            );
            assert!(finish_dispatch_crash_cleanup());

            hew_dispatch_state_cleanup_prepare(
                ptr::from_ref(&outer_replacement).cast(),
                ptr::from_mut(&mut outer).cast(),
                size_of::<u64>() as u64,
            );
            assert_eq!(
                crate::signal::state_field_finalizer_depth(),
                0,
                "the final outer preparation must close the last fatal phase"
            );
            assert!(finish_dispatch_crash_cleanup());
        }
    }

    #[test]
    #[cfg(any(unix, windows))]
    fn sequential_state_finalizer_transactions_return_depth_to_zero() {
        crate::signal::init_crash_handling();
        crate::signal::init_worker_recovery(u32::MAX);
        assert_eq!(crate::signal::state_field_finalizer_depth(), 0);

        let mut state = 301_u64;
        let first = 302_u64;
        let second = 303_u64;
        // SAFETY: the state and replacement slots remain initialized and live
        // through both sequential transactions and the matching finish.
        unsafe {
            assert!(begin_dispatch_crash_cleanup(
                ptr::from_mut(&mut state).cast(),
                size_of::<u64>(),
                Some(record_u64_state_cleanup),
            ));
            for replacement in [&first, &second] {
                assert!(hew_dispatch_state_cleanup_begin_replace(
                    ptr::from_mut(&mut state).cast(),
                    size_of::<u64>() as u64,
                ));
                assert_eq!(crate::signal::state_field_finalizer_depth(), 1);
                hew_dispatch_state_cleanup_prepare(
                    ptr::from_ref(replacement).cast(),
                    ptr::from_mut(&mut state).cast(),
                    size_of::<u64>() as u64,
                );
                assert_eq!(crate::signal::state_field_finalizer_depth(), 0);
            }
            assert!(finish_dispatch_crash_cleanup());
        }
    }

    #[test]
    fn crash_cleanup_snapshot_reactivation_preserves_first_activation_order() {
        // SAFETY: allocate one tracked frame, publish two stack snapshots, then
        // transfer raw ownership through the test drain exactly once.
        unsafe {
            let frame = hew_cont_frame_alloc_tracked(64);
            let mut older = 11_u64;
            let mut newer = 22_u64;
            let older_token = hew_cont_crash_cleanup_arm(
                0,
                ptr::from_mut(&mut older).cast(),
                size_of::<u64>() as u64,
                align_of::<u64>() as u64,
                Some(record_u64_cleanup),
                CrashCleanupStorage::Snapshot as u32,
                CrashCleanupRelocation::Bitwise as u32,
            );
            let newer_token = hew_cont_crash_cleanup_arm(
                0,
                ptr::from_mut(&mut newer).cast(),
                size_of::<u64>() as u64,
                align_of::<u64>() as u64,
                Some(record_u64_cleanup),
                CrashCleanupStorage::Snapshot as u32,
                CrashCleanupRelocation::Bitwise as u32,
            );
            assert_ne!(older_token, 0);
            assert_ne!(newer_token, 0);
            assert!(hew_cont_crash_cleanup_deactivate(older_token));
            older = 33;
            assert_eq!(
                hew_cont_crash_cleanup_arm(
                    older_token,
                    ptr::from_mut(&mut older).cast(),
                    size_of::<u64>() as u64,
                    align_of::<u64>() as u64,
                    Some(record_u64_cleanup),
                    CrashCleanupStorage::Snapshot as u32,
                    CrashCleanupRelocation::Bitwise as u32,
                ),
                older_token
            );

            let mut reclaimed = Vec::new();
            assert_eq!(
                drain_active_coroutine_frames_excluding(ptr::null_mut(), |raw| {
                    reclaimed.push(raw);
                }),
                1
            );
            assert_eq!(
                take_crash_cleanup_test_drops(),
                [22, 33],
                "reactivating the older lexical owner must not move it above the newer owner"
            );
            free_frame_allocation(frame);
        }
    }

    #[test]
    fn crash_cleanup_drain_releases_tls_before_reentrant_thunks_and_stays_lifo() {
        // SAFETY: both snapshots remain initialized until drain transfers their
        // sole typed authority; the reentrant thunk creates and normally frees
        // an independent nested tracked frame.
        unsafe {
            let frame = hew_cont_frame_alloc_tracked(64);
            let mut older = 11_u64;
            let mut newer = 22_u64;
            for (slot, thunk) in [
                (
                    ptr::from_mut(&mut older).cast(),
                    record_u64_cleanup as CrashCleanupThunk,
                ),
                (
                    ptr::from_mut(&mut newer).cast(),
                    record_u64_cleanup_with_cont_reentry as CrashCleanupThunk,
                ),
            ] {
                let token = hew_cont_crash_cleanup_arm(
                    0,
                    slot,
                    size_of::<u64>() as u64,
                    align_of::<u64>() as u64,
                    Some(thunk),
                    CrashCleanupStorage::Snapshot as u32,
                    CrashCleanupRelocation::Bitwise as u32,
                );
                assert_ne!(token, 0);
                assert_ne!(token, CRASH_CLEANUP_ARM_FAILED);
            }

            let mut reclaimed = Vec::new();
            assert_eq!(
                drain_active_coroutine_frames_excluding(ptr::null_mut(), |raw| {
                    reclaimed.push(raw);
                }),
                1
            );
            assert_eq!(reclaimed, [frame]);
            assert_eq!(
                take_crash_cleanup_test_drops(),
                [22, 11],
                "thunk reentry must not disturb lexical LIFO cleanup order"
            );
            free_frame_allocation(frame);
        }
    }

    // This verifies Rust unwinding through a `C-unwind` cleanup callback.
    // WASI uses aborting panics, so there is no in-process unwind contract.
    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn rust_c_unwind_cleanup_panic_is_quarantined_and_siblings_run_once() {
        let _ = take_crash_cleanup_test_drops();
        let _ = take_crash_cleanup_panic_payload_drops();
        let mut state = 99_u64;
        let mut older = 11_u64;
        let mut faulty = 22_u64;
        // SAFETY: all registered values remain initialized through the
        // detached drain. This deliberately uses a Rust-authored `C-unwind`
        // callback; generated LLVM thunks call plain-C runtime symbols and a
        // panic there is process-fatal before catch_unwind can observe it.
        unsafe {
            assert!(begin_dispatch_crash_cleanup(
                ptr::from_mut(&mut state).cast(),
                size_of::<u64>(),
                Some(record_u64_state_cleanup),
            ));
            for (slot, thunk) in [
                (
                    ptr::from_mut(&mut older).cast(),
                    record_u64_cleanup as CrashCleanupThunk,
                ),
                (
                    ptr::from_mut(&mut faulty).cast(),
                    record_u64_cleanup_then_panic as CrashCleanupThunk,
                ),
            ] {
                assert_ne!(
                    hew_cont_crash_cleanup_arm(
                        0,
                        slot,
                        size_of::<u64>() as u64,
                        align_of::<u64>() as u64,
                        Some(thunk),
                        CrashCleanupStorage::Snapshot as u32,
                        CrashCleanupRelocation::Bitwise as u32,
                    ),
                    CRASH_CLEANUP_ARM_FAILED
                );
            }
            let outcome = recover_dispatch_crash_cleanup_with_outcome(true);
            assert!(outcome.registry_found);
            assert!(outcome.state_authority_consumed);
            assert!(outcome.finalizer_faulted);
        }
        assert_eq!(
            take_crash_cleanup_test_drops(),
            [22, 11, 99],
            "the failed entry is never retried; independent sibling and state drops continue once"
        );
        assert_eq!(
            take_crash_cleanup_panic_payload_drops(),
            1,
            "the caught entry payload must be disposed even when its destructor also panics"
        );
    }

    // The actor-state snapshot is drained after every lexical owner. Exercise
    // its independent callback path with the same hostile payload used above:
    // the old mem::forget counterfactual leaves the observed drop count at 0.
    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn rust_c_unwind_state_cleanup_panic_is_quarantined_after_siblings_run_once() {
        let _ = take_crash_cleanup_test_drops();
        let _ = take_crash_cleanup_panic_payload_drops();
        let mut state = 99_u64;
        let mut older = 11_u64;
        let mut newer = 22_u64;
        // SAFETY: state and both lexical snapshots remain initialized until
        // the detached drain consumes their sole cleanup authorities.
        unsafe {
            assert!(begin_dispatch_crash_cleanup(
                ptr::from_mut(&mut state).cast(),
                size_of::<u64>(),
                Some(record_u64_state_cleanup),
            ));
            let registry = active_dispatch_cleanup_registry()
                .expect("dispatch begin must publish its cleanup registry");
            // Substitute a genuinely unwind-capable Rust test callback. The
            // production field normally contains a generated plain-C thunk.
            (*registry).state_drop = Some(record_u64_cleanup_then_panic);
            for slot in [
                ptr::from_mut(&mut older).cast(),
                ptr::from_mut(&mut newer).cast(),
            ] {
                assert_ne!(
                    hew_cont_crash_cleanup_arm(
                        0,
                        slot,
                        size_of::<u64>() as u64,
                        align_of::<u64>() as u64,
                        Some(record_u64_cleanup),
                        CrashCleanupStorage::Snapshot as u32,
                        CrashCleanupRelocation::Bitwise as u32,
                    ),
                    CRASH_CLEANUP_ARM_FAILED
                );
            }
            let outcome = recover_dispatch_crash_cleanup_with_outcome(true);
            assert!(outcome.registry_found);
            assert!(outcome.state_authority_consumed);
            assert!(outcome.finalizer_faulted);
        }
        assert_eq!(
            take_crash_cleanup_test_drops(),
            [22, 11, 99],
            "lexical siblings run once before the failed state snapshot and are never retried"
        );
        assert_eq!(
            take_crash_cleanup_panic_payload_drops(),
            1,
            "the caught state payload must be disposed even when its destructor also panics"
        );
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    #[cfg_attr(
        miri,
        ignore = "spawns a subprocess to observe abort(); Miri cannot posix_spawn"
    )]
    fn hew_trap_during_crash_cleanup_is_deterministically_process_fatal() {
        const HELPER: &str = "cont::tests::_helper_hew_trap_during_crash_cleanup_is_process_fatal";
        let output = std::process::Command::new(std::env::current_exe().unwrap())
            .args(["--exact", "--nocapture", HELPER])
            .env("RUST_TEST_THREADS", "1")
            .env("HEW_CONT_FINALIZER_TRAP_DEATH_TEST", "1")
            .output()
            .expect("spawn crash-cleanup finalizer trap helper");
        assert!(
            !output.status.success(),
            "a Hew trap from arbitrary finalizer code must terminate the process"
        );
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("Hew panic raised during crash-cleanup finalization"),
            "death helper must identify the non-retriable finalizer edge; stderr:\n{stderr}"
        );
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn _helper_hew_trap_during_crash_cleanup_is_process_fatal() {
        if std::env::var_os("HEW_CONT_FINALIZER_TRAP_DEATH_TEST").is_none() {
            return;
        }
        let mut value = 37_u64;
        // SAFETY: one initialized snapshot is registered. Its thunk
        // intentionally invokes the supported deterministic-fatal edge.
        unsafe {
            assert!(begin_dispatch_crash_cleanup(ptr::null_mut(), 0, None));
            assert_ne!(
                hew_cont_crash_cleanup_arm(
                    0,
                    ptr::from_mut(&mut value).cast(),
                    size_of::<u64>() as u64,
                    align_of::<u64>() as u64,
                    Some(trap_during_crash_cleanup),
                    CrashCleanupStorage::Snapshot as u32,
                    CrashCleanupRelocation::Bitwise as u32,
                ),
                CRASH_CLEANUP_ARM_FAILED
            );
            let _ = recover_dispatch_crash_cleanup(true);
        }
        panic!("finalizer trap helper unexpectedly survived");
    }

    #[test]
    #[cfg(unix)]
    #[cfg_attr(
        miri,
        ignore = "spawns a subprocess and raises SIGSEGV; Miri has no native signal handler"
    )]
    fn hardware_fault_during_crash_cleanup_uses_async_signal_safe_exit() {
        const HELPER: &str =
            "cont::tests::_helper_hardware_fault_during_crash_cleanup_is_process_fatal";
        let output = std::process::Command::new(std::env::current_exe().unwrap())
            .args(["--exact", "--nocapture", HELPER])
            .env("RUST_TEST_THREADS", "1")
            .env("HEW_CONT_FINALIZER_SIGSEGV_DEATH_TEST", "1")
            .output()
            .expect("spawn crash-cleanup SIGSEGV helper");
        assert_eq!(
            output.status.code(),
            Some(128 + libc::SIGSEGV),
            "cleanup SIGSEGV must _exit instead of longjmp across the drain; stderr:\n{}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    #[test]
    #[cfg(unix)]
    #[cfg_attr(
        miri,
        ignore = "spawns subprocesses that raise SIGSEGV/SIGABRT; Miri has no native signal handler"
    )]
    fn state_field_finalizer_phase_is_fatal_for_hardware_and_direct_traps() {
        use std::os::unix::process::ExitStatusExt;

        const HELPER: &str = "cont::tests::_helper_state_field_finalizer_phase_is_process_fatal";
        let hardware = std::process::Command::new(std::env::current_exe().unwrap())
            .args(["--exact", "--nocapture", HELPER])
            .env("RUST_TEST_THREADS", "1")
            .env("HEW_STATE_FINALIZER_FATAL_TEST", "hardware")
            .output()
            .expect("spawn state-finalizer hardware-fault helper");
        assert_eq!(
            hardware.status.code(),
            Some(128 + libc::SIGSEGV),
            "hardware fault in pre-release phase must _exit, not actor-longjmp; stderr:\n{}",
            String::from_utf8_lossy(&hardware.stderr)
        );

        let direct = std::process::Command::new(std::env::current_exe().unwrap())
            .args(["--exact", "--nocapture", HELPER])
            .env("RUST_TEST_THREADS", "1")
            .env("HEW_STATE_FINALIZER_FATAL_TEST", "direct")
            .output()
            .expect("spawn state-finalizer direct-trap helper");
        assert_eq!(
            direct.status.signal(),
            Some(libc::SIGABRT),
            "intentional Hew panic in pre-release phase must abort, not actor-longjmp; stderr:\n{}",
            String::from_utf8_lossy(&direct.stderr)
        );

        let prepare_failure = std::process::Command::new(std::env::current_exe().unwrap())
            .args(["--exact", "--nocapture", HELPER])
            .env("RUST_TEST_THREADS", "1")
            .env("HEW_STATE_FINALIZER_FATAL_TEST", "prepare_failure")
            .output()
            .expect("spawn state-finalizer no-source prepare-failure helper");
        assert_eq!(
            prepare_failure.status.signal(),
            Some(libc::SIGABRT),
            "post-begin no-source validation failure must abort, not actor-recover; stderr:\n{}",
            String::from_utf8_lossy(&prepare_failure.stderr)
        );

        let underflow = std::process::Command::new(std::env::current_exe().unwrap())
            .args(["--exact", "--nocapture", HELPER])
            .env("RUST_TEST_THREADS", "1")
            .env("HEW_STATE_FINALIZER_FATAL_TEST", "underflow")
            .output()
            .expect("spawn state-finalizer depth-underflow helper");
        assert_eq!(
            underflow.status.signal(),
            Some(libc::SIGABRT),
            "an unmatched finalizer-depth reset must abort instead of wrapping; stderr:\n{}",
            String::from_utf8_lossy(&underflow.stderr)
        );
    }

    #[test]
    #[cfg(unix)]
    fn _helper_state_field_finalizer_phase_is_process_fatal() {
        let Some(mode) = std::env::var_os("HEW_STATE_FINALIZER_FATAL_TEST") else {
            return;
        };
        crate::signal::init_crash_handling();
        crate::signal::init_worker_recovery(u32::MAX);
        // Install a valid actor recovery target. Surviving via longjmp returns
        // the sentinel 77, making both counterfactuals distinguishable from the
        // required process-fatal disposition.
        // SAFETY: null actor/message are accepted test metadata.
        let jmp_buf =
            unsafe { crate::signal::prepare_dispatch_recovery(ptr::null_mut(), ptr::null_mut()) };
        assert!(!jmp_buf.is_null());
        // SAFETY: the helper frame remains live until the child terminates.
        if unsafe { crate::signal::sigsetjmp(jmp_buf, 1) } != 0 {
            std::process::exit(77);
        }
        crate::signal::mark_recovery_active();

        let mut state = 19_u64;
        // SAFETY: state remains live through the death-test process.
        unsafe {
            assert!(begin_dispatch_crash_cleanup(
                ptr::from_mut(&mut state).cast(),
                size_of::<u64>(),
                Some(record_u64_state_cleanup),
            ));
        }
        if mode == "underflow" {
            // Manufacture the impossible registry/counter drift that reset
            // must treat as process-fatal rather than wrapping the depth.
            let registry = active_dispatch_cleanup_registry()
                .expect("underflow helper must have an active registry");
            // SAFETY: this death-test exclusively owns the active registry.
            unsafe { (*registry).state_finalizer_critical = true };
            // SAFETY: deliberate invariant violation under test.
            unsafe { reset_state_finalizer_critical(registry) };
            std::process::exit(79);
        }
        // SAFETY: state remains live and begin_replace validates its exact range
        // before publishing the fatal phase.
        unsafe {
            assert!(hew_dispatch_state_cleanup_begin_replace(
                ptr::from_mut(&mut state).cast(),
                size_of::<u64>() as u64,
            ));
        }
        assert_eq!(crate::signal::state_field_finalizer_depth(), 1);

        // Re-enter through an inner dispatch transaction. Completing it must
        // decrement only its own phase and leave the outer guard observable to
        // both the hardware handler and the intentional direct-trap preamble.
        let mut inner = 29_u64;
        let inner_replacement = 30_u64;
        // SAFETY: inner state and replacement remain live until the nested
        // transaction is prepared and normally finished.
        unsafe {
            assert!(begin_dispatch_crash_cleanup(
                ptr::from_mut(&mut inner).cast(),
                size_of::<u64>(),
                Some(record_u64_state_cleanup),
            ));
            assert!(hew_dispatch_state_cleanup_begin_replace(
                ptr::from_mut(&mut inner).cast(),
                size_of::<u64>() as u64,
            ));
            assert_eq!(crate::signal::state_field_finalizer_depth(), 2);
            hew_dispatch_state_cleanup_prepare(
                ptr::from_ref(&inner_replacement).cast(),
                ptr::from_mut(&mut inner).cast(),
                size_of::<u64>() as u64,
            );
            assert_eq!(crate::signal::state_field_finalizer_depth(), 1);
            assert!(finish_dispatch_crash_cleanup());
        }
        match mode.to_str() {
            Some("hardware") => {
                // SAFETY: intentional death-test signal.
                unsafe { libc::raise(libc::SIGSEGV) };
            }
            Some("prepare_failure") => {
                // SAFETY: null replacement deliberately exercises the
                // process-fatal post-begin no-source invariant boundary.
                unsafe {
                    hew_dispatch_state_cleanup_prepare(
                        ptr::null(),
                        ptr::from_mut(&mut state).cast(),
                        size_of::<u64>() as u64,
                    );
                }
            }
            _ => crate::actor::hew_panic(),
        }
        std::process::exit(78);
    }

    #[test]
    #[cfg(unix)]
    fn _helper_hardware_fault_during_crash_cleanup_is_process_fatal() {
        if std::env::var_os("HEW_CONT_FINALIZER_SIGSEGV_DEATH_TEST").is_none() {
            return;
        }
        crate::signal::init_crash_handling();
        crate::signal::init_worker_recovery(u32::MAX);
        // Install a valid jump target so the counterfactual is meaningful: the
        // old handler would siglongjmp here and bypass the drain guard.
        // SAFETY: null actor/message are accepted test metadata; the returned
        // buffer belongs to this initialized worker thread.
        let jmp_buf =
            unsafe { crate::signal::prepare_dispatch_recovery(ptr::null_mut(), ptr::null_mut()) };
        assert!(!jmp_buf.is_null());
        // SAFETY: jmp_buf is the current thread's live recovery buffer and this
        // helper frame remains active until the expected process exit.
        let jumped = unsafe { crate::signal::sigsetjmp(jmp_buf, 1) };
        if jumped != 0 {
            std::process::exit(77);
        }
        crate::signal::mark_recovery_active();

        let mut value = 37_u64;
        // SAFETY: value stays initialized through the detached drain; the
        // registered callback deliberately terminates the subprocess.
        unsafe {
            assert!(begin_dispatch_crash_cleanup(ptr::null_mut(), 0, None));
            assert_ne!(
                hew_cont_crash_cleanup_arm(
                    0,
                    ptr::from_mut(&mut value).cast(),
                    size_of::<u64>() as u64,
                    align_of::<u64>() as u64,
                    Some(raise_sigsegv_during_crash_cleanup),
                    CrashCleanupStorage::Snapshot as u32,
                    CrashCleanupRelocation::Bitwise as u32,
                ),
                CRASH_CLEANUP_ARM_FAILED
            );
            let _ = recover_dispatch_crash_cleanup(true);
        }
        panic!("cleanup SIGSEGV helper unexpectedly survived");
    }

    #[test]
    fn crash_cleanup_drain_reentry_cannot_consume_older_dispatch_scope() {
        let _ = take_crash_cleanup_test_drops();
        NESTED_DRAIN_REENTRY_REFUSED.store(0, Ordering::SeqCst);
        // SAFETY: both nested scopes and the registered u64 remain live until
        // their explicit finish/recovery edges below.
        unsafe {
            assert!(begin_dispatch_crash_cleanup(ptr::null_mut(), 0, None));
            assert!(begin_dispatch_crash_cleanup(ptr::null_mut(), 0, None));
            let mut value = 44_u64;
            assert_ne!(
                hew_cont_crash_cleanup_arm(
                    0,
                    ptr::from_mut(&mut value).cast(),
                    size_of::<u64>() as u64,
                    align_of::<u64>() as u64,
                    Some(attempt_nested_dispatch_drain),
                    CrashCleanupStorage::Snapshot as u32,
                    CrashCleanupRelocation::Bitwise as u32,
                ),
                CRASH_CLEANUP_ARM_FAILED
            );
            assert!(recover_dispatch_crash_cleanup(true));
            assert_eq!(NESTED_DRAIN_REENTRY_REFUSED.load(Ordering::SeqCst), 1);
            assert!(
                finish_dispatch_crash_cleanup(),
                "older scope must remain intact"
            );
        }
        assert_eq!(take_crash_cleanup_test_drops(), [44]);
    }

    #[test]
    fn crash_cleanup_snapshot_retire_discards_escrow_without_drop() {
        // SAFETY: the helper value remains live while its snapshot is armed;
        // lexical retirement must free only the escrow allocation.
        unsafe {
            let frame = hew_cont_frame_alloc_tracked(64);
            let mut value = 17_u64;
            let token = hew_cont_crash_cleanup_arm(
                0,
                ptr::from_mut(&mut value).cast(),
                size_of::<u64>() as u64,
                align_of::<u64>() as u64,
                Some(record_u64_cleanup),
                CrashCleanupStorage::Snapshot as u32,
                CrashCleanupRelocation::Bitwise as u32,
            );
            assert_ne!(token, 0);
            assert_ne!(token, CRASH_CLEANUP_ARM_FAILED);
            assert!(hew_cont_crash_cleanup_retire(token));
            assert!(
                take_crash_cleanup_test_drops().is_empty(),
                "normal lexical retirement must not invoke the typed destructor"
            );
            hew_cont_frame_handoff(frame);
            hew_cont_frame_free(frame);
        }
    }

    #[test]
    fn raw_frame_free_accepts_inactive_crash_cleanup_entries() {
        let _ = take_crash_cleanup_test_drops();
        // SAFETY: the snapshot source remains live through deactivation. Raw
        // free owns the handed-off frame and may discard the inactive escrow.
        unsafe {
            let frame = hew_cont_frame_alloc_tracked(64);
            let mut value = 23_u64;
            let token = hew_cont_crash_cleanup_arm(
                0,
                ptr::from_mut(&mut value).cast(),
                size_of::<u64>() as u64,
                align_of::<u64>() as u64,
                Some(record_u64_cleanup),
                CrashCleanupStorage::Snapshot as u32,
                CrashCleanupRelocation::Bitwise as u32,
            );
            assert_ne!(token, 0);
            assert_ne!(token, CRASH_CLEANUP_ARM_FAILED);
            assert!(hew_cont_crash_cleanup_deactivate(token));
            hew_cont_frame_handoff(frame);
            hew_cont_frame_free(frame);
        }
        assert!(
            take_crash_cleanup_test_drops().is_empty(),
            "inactive escrow must be freed without running its typed thunk"
        );
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    #[cfg_attr(
        miri,
        ignore = "spawns a subprocess to observe abort(); Miri cannot posix_spawn"
    )]
    fn raw_frame_free_rejects_active_crash_cleanup_entries() {
        const HELPER: &str =
            "cont::tests::_helper_raw_frame_free_rejects_active_crash_cleanup_entries";
        let output = std::process::Command::new(std::env::current_exe().unwrap())
            .args(["--exact", "--nocapture", HELPER])
            .env("RUST_TEST_THREADS", "1")
            .env("HEW_CONT_ACTIVE_CLEANUP_DEATH_TEST", "1")
            .output()
            .expect("spawn active-cleanup raw-free death helper");
        assert!(
            !output.status.success(),
            "raw frame free with an active typed owner must terminate"
        );
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("active typed crash-cleanup owner"),
            "death helper must identify the violated ownership invariant; stderr:\n{stderr}"
        );
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn _helper_raw_frame_free_rejects_active_crash_cleanup_entries() {
        if std::env::var_os("HEW_CONT_ACTIVE_CLEANUP_DEATH_TEST").is_none() {
            return;
        }
        // SAFETY: the helper intentionally violates the normal-free contract
        // after publishing one valid active snapshot. The process must abort
        // before either the typed value or its escrow can be silently leaked.
        unsafe {
            let frame = hew_cont_frame_alloc_tracked(64);
            let mut value = 29_u64;
            let token = hew_cont_crash_cleanup_arm(
                0,
                ptr::from_mut(&mut value).cast(),
                size_of::<u64>() as u64,
                align_of::<u64>() as u64,
                Some(record_u64_cleanup),
                CrashCleanupStorage::Snapshot as u32,
                CrashCleanupRelocation::Bitwise as u32,
            );
            assert_ne!(token, 0);
            assert_ne!(token, CRASH_CLEANUP_ARM_FAILED);
            hew_cont_frame_handoff(frame);
            hew_cont_frame_free(frame);
        }
        panic!("raw frame free unexpectedly accepted an active cleanup entry");
    }

    #[test]
    fn crash_cleanup_direct_frame_reads_latest_bytes_and_rejects_overlap() {
        // SAFETY: every slot lies within the live 64-byte tracked payload.
        unsafe {
            let frame = hew_cont_frame_alloc_tracked(64);
            let whole = frame.cast::<u64>();
            ptr::write(whole, 41);
            ptr::write(whole.add(1), 42);
            let whole_token = hew_cont_crash_cleanup_arm(
                0,
                whole.cast(),
                (size_of::<u64>() * 2) as u64,
                align_of::<u64>() as u64,
                Some(record_u64_cleanup),
                CrashCleanupStorage::DirectFrame as u32,
                CrashCleanupRelocation::Bitwise as u32,
            );
            assert_ne!(whole_token, 0);
            let overlap = hew_cont_crash_cleanup_arm(
                0,
                whole.add(1).cast(),
                size_of::<u64>() as u64,
                align_of::<u64>() as u64,
                Some(record_u64_cleanup),
                CrashCleanupStorage::DirectFrame as u32,
                CrashCleanupRelocation::Bitwise as u32,
            );
            assert_eq!(
                overlap, CRASH_CLEANUP_ARM_FAILED,
                "aggregate and projected field obligations must not coexist"
            );
            assert!(hew_cont_crash_cleanup_deactivate(whole_token));
            let field_token = hew_cont_crash_cleanup_arm(
                0,
                whole.add(1).cast(),
                size_of::<u64>() as u64,
                align_of::<u64>() as u64,
                Some(record_u64_cleanup),
                CrashCleanupStorage::DirectFrame as u32,
                CrashCleanupRelocation::Bitwise as u32,
            );
            assert_ne!(field_token, CRASH_CLEANUP_ARM_FAILED);
            assert!(hew_cont_crash_cleanup_deactivate(field_token));
            assert!(hew_cont_crash_cleanup_retire(field_token));
            assert!(hew_cont_crash_cleanup_retire(whole_token));
            hew_cont_frame_handoff(frame);
            hew_cont_frame_free(frame);
        }
    }

    // Cross-worker migration is the native M:N scheduler contract.  The
    // wasm32 runtime has no OS-thread worker migration to exercise.
    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn crash_cleanup_registry_survives_cross_worker_frame_migration() {
        MIGRATED_CRASH_CLEANUP_VALUE.store(0, Ordering::Release);

        // Allocate and arm on the first worker, then perform the ordinary ramp
        // handoff. The registry lives in the frame header, not this thread's
        // active-frame TLS.
        // SAFETY: this thread exclusively owns the live tracked frame and slot
        // until handoff transfers the raw addresses below.
        let (frame_addr, slot_addr, token) = unsafe {
            let frame = hew_cont_frame_alloc_tracked(64);
            assert!(!frame.is_null());
            let slot = frame.cast::<u64>().add(4);
            ptr::write(slot, 71);
            let token = hew_cont_crash_cleanup_arm(
                0,
                slot.cast(),
                size_of::<u64>() as u64,
                align_of::<u64>() as u64,
                Some(record_migrated_u64_cleanup),
                CrashCleanupStorage::DirectFrame as u32,
                CrashCleanupRelocation::Bitwise as u32,
            );
            assert_ne!(token, 0);
            assert_ne!(token, CRASH_CLEANUP_ARM_FAILED);
            hew_cont_frame_handoff(frame);
            (frame as usize, slot as usize, token)
        };

        // Resume ownership on another real OS thread, exercise token lookup
        // and reactivation there, then model the worker's crash drain. The
        // direct cleanup must read the latest frame bytes on that worker.
        // SAFETY: the first worker handed off the still-live frame; this worker
        // becomes its exclusive active owner and drains it before returning.
        let reclaimed_addr = std::thread::spawn(move || unsafe {
            let frame = frame_addr as *mut c_void;
            let slot = slot_addr as *mut u64;
            assert!(active_coroutine_enter(frame));
            assert!(hew_cont_crash_cleanup_deactivate(token));
            ptr::write(slot, 89);
            assert_eq!(
                hew_cont_crash_cleanup_arm(
                    token,
                    slot.cast(),
                    size_of::<u64>() as u64,
                    align_of::<u64>() as u64,
                    Some(record_migrated_u64_cleanup),
                    CrashCleanupStorage::DirectFrame as u32,
                    CrashCleanupRelocation::Bitwise as u32,
                ),
                token
            );
            ptr::write(slot, 97);
            let mut reclaimed = None;
            assert_eq!(
                drain_active_coroutine_frames_excluding(ptr::null_mut(), |raw| {
                    reclaimed = Some(raw as usize);
                }),
                1
            );
            reclaimed.expect("migrated frame must transfer to crash reclamation")
        })
        .join()
        .expect("migration worker panicked");

        assert_eq!(
            MIGRATED_CRASH_CLEANUP_VALUE.load(Ordering::Acquire),
            97,
            "the migrated worker must run the frame registry's sole typed cleanup"
        );
        assert_eq!(reclaimed_addr, frame_addr);
        // SAFETY: the other worker transferred raw allocation ownership to us
        // after draining the typed registry exactly once.
        unsafe { free_frame_allocation(reclaimed_addr as *mut c_void) };
    }

    #[test]
    fn crash_cleanup_rejects_misaligned_direct_and_snapshot_slots() {
        // SAFETY: the deliberately unaligned slots still lie inside readable
        // live allocations; arm must reject them before copying or publishing
        // an entry.
        unsafe {
            let frame = hew_cont_frame_alloc_tracked(64);
            let direct_unaligned = frame.cast::<u8>().add(1).cast::<c_void>();
            assert_eq!(
                hew_cont_crash_cleanup_arm(
                    0,
                    direct_unaligned,
                    size_of::<u64>() as u64,
                    align_of::<u64>() as u64,
                    Some(record_u64_cleanup),
                    CrashCleanupStorage::DirectFrame as u32,
                    CrashCleanupRelocation::Bitwise as u32,
                ),
                CRASH_CLEANUP_ARM_FAILED
            );

            let mut helper_bytes = [0_u8; size_of::<u64>() + align_of::<u64>()];
            let snapshot_unaligned = helper_bytes.as_mut_ptr().add(1).cast::<c_void>();
            assert_eq!(
                hew_cont_crash_cleanup_arm(
                    0,
                    snapshot_unaligned,
                    size_of::<u64>() as u64,
                    align_of::<u64>() as u64,
                    Some(record_u64_cleanup),
                    CrashCleanupStorage::Snapshot as u32,
                    CrashCleanupRelocation::Bitwise as u32,
                ),
                CRASH_CLEANUP_ARM_FAILED
            );

            hew_cont_frame_handoff(frame);
            hew_cont_frame_free(frame);
        }
    }

    #[test]
    fn crash_cleanup_frame_interior_refuses_helper_stack_pointee() {
        // SAFETY: direct fat slot and its pointee are both inside the tracked
        // payload; the helper twin deliberately points outside and is refused
        // before any cleanup entry is published.
        unsafe {
            let frame = hew_cont_frame_alloc_tracked(64);
            let fat_slot = frame.cast::<*mut c_void>().add(2);
            let concrete = frame.cast::<u8>().add(48).cast::<c_void>();
            ptr::write(fat_slot, concrete);
            ptr::write(
                fat_slot.add(1),
                ptr::from_ref(&U64_DYN_VTABLE).cast_mut().cast(),
            );
            let direct = hew_cont_crash_cleanup_arm(
                0,
                fat_slot.cast(),
                size_of::<crate::trait_object::HewTraitObject>() as u64,
                align_of::<crate::trait_object::HewTraitObject>() as u64,
                Some(record_u64_cleanup),
                CrashCleanupStorage::DirectFrame as u32,
                CrashCleanupRelocation::FrameInterior as u32,
            );
            assert_ne!(direct, CRASH_CLEANUP_ARM_FAILED);
            assert!(hew_cont_crash_cleanup_deactivate(direct));
            assert!(hew_cont_crash_cleanup_retire(direct));

            let mut helper_concrete = 9_u64;
            let mut helper_fat = crate::trait_object::HewTraitObject {
                data: ptr::from_mut(&mut helper_concrete).cast(),
                vtable: ptr::from_ref(&U64_DYN_VTABLE),
            };
            let refused = hew_cont_crash_cleanup_arm(
                0,
                ptr::from_mut(&mut helper_fat).cast(),
                size_of::<crate::trait_object::HewTraitObject>() as u64,
                align_of::<crate::trait_object::HewTraitObject>() as u64,
                Some(record_u64_cleanup),
                CrashCleanupStorage::Snapshot as u32,
                CrashCleanupRelocation::FrameInterior as u32,
            );
            assert_eq!(refused, CRASH_CLEANUP_ARM_FAILED);

            ptr::write(fat_slot, frame.cast::<u8>().add(64).cast());
            let out_of_range = hew_cont_crash_cleanup_arm(
                0,
                fat_slot.cast(),
                size_of::<crate::trait_object::HewTraitObject>() as u64,
                align_of::<crate::trait_object::HewTraitObject>() as u64,
                Some(record_u64_cleanup),
                CrashCleanupStorage::DirectFrame as u32,
                CrashCleanupRelocation::FrameInterior as u32,
            );
            assert_eq!(
                out_of_range, CRASH_CLEANUP_ARM_FAILED,
                "the full concrete vtable size must fit, not merely data[0]"
            );
            hew_cont_frame_handoff(frame);
            hew_cont_frame_free(frame);
        }
    }

    #[test]
    fn crash_cleanup_forged_stale_and_inactive_tokens_fail_without_dereference() {
        // SAFETY: the only dereference-worthy token is created by arm; forged
        // and stale values are checked against registry membership first.
        unsafe {
            let frame = hew_cont_frame_alloc_tracked(64);
            assert!(!hew_cont_crash_cleanup_deactivate(0x1000));
            assert!(!hew_cont_crash_cleanup_retire(0x1000));

            let mut value = 5_u64;
            let token = hew_cont_crash_cleanup_arm(
                0,
                ptr::from_mut(&mut value).cast(),
                size_of::<u64>() as u64,
                align_of::<u64>() as u64,
                Some(record_u64_cleanup),
                CrashCleanupStorage::Snapshot as u32,
                CrashCleanupRelocation::Bitwise as u32,
            );
            assert_ne!(token, 0);
            assert!(hew_cont_crash_cleanup_deactivate(token));
            assert!(hew_cont_crash_cleanup_retire(token));
            assert!(!hew_cont_crash_cleanup_deactivate(token));
            assert!(!hew_cont_crash_cleanup_retire(token));

            hew_cont_frame_handoff(frame);
            assert_eq!(
                hew_cont_crash_cleanup_arm(
                    0x1000,
                    ptr::from_mut(&mut value).cast(),
                    size_of::<u64>() as u64,
                    align_of::<u64>() as u64,
                    Some(record_u64_cleanup),
                    CrashCleanupStorage::Snapshot as u32,
                    CrashCleanupRelocation::Bitwise as u32,
                ),
                CRASH_CLEANUP_ARM_FAILED,
                "reactivation with no active frame must fail before token access"
            );
            hew_cont_frame_free(frame);
        }
    }

    #[test]
    fn crash_cleanup_same_address_new_generation_rejects_stale_token() {
        // Model the allocator's strongest ABA case deterministically: the
        // replacement logical entry occupies the exact same Box address. Only
        // its process-unique generation changes, so the retired generation
        // must not deactivate or retire the replacement.
        // SAFETY: the test owns the tracked frame and stack value for the whole
        // registry mutation, and frees the frame exactly once after restoring
        // a valid live replacement entry.
        unsafe {
            let frame = hew_cont_frame_alloc_tracked(64);
            let mut value = 13_u64;
            let stale = hew_cont_crash_cleanup_arm(
                0,
                ptr::from_mut(&mut value).cast(),
                size_of::<u64>() as u64,
                align_of::<u64>() as u64,
                Some(record_u64_cleanup),
                CrashCleanupStorage::Snapshot as u32,
                CrashCleanupRelocation::Bitwise as u32,
            );
            assert_ne!(stale, 0);
            assert_ne!(stale, CRASH_CLEANUP_ARM_FAILED);
            assert!(hew_cont_crash_cleanup_deactivate(stale));

            let registry = frame_cleanup_registry(frame);
            assert!(!registry.is_null());
            let entry = (&(*registry).entries)[0];
            assert_ne!(
                stale, entry as usize as u64,
                "the public token must be independent of the allocation address"
            );
            let replacement = next_crash_cleanup_token().expect("test token space");
            (*entry).token = replacement;
            (*entry).active = true;

            assert!(
                !hew_cont_crash_cleanup_deactivate(stale),
                "an allocator-reused address must not revive a stale generation"
            );
            assert!(
                !hew_cont_crash_cleanup_retire(stale),
                "a stale generation must not retire the replacement entry"
            );
            assert!(hew_cont_crash_cleanup_deactivate(replacement));
            assert!(hew_cont_crash_cleanup_retire(replacement));

            hew_cont_frame_handoff(frame);
            hew_cont_frame_free(frame);
        }
    }

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
    use std::sync::atomic::AtomicU32;

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
