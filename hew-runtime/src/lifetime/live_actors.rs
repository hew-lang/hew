//! Runtime-owned actor-liveness registry (`LiveActors`).
//!
//! All access to the actor liveness registry goes through the free functions
//! exported here (`track_actor`, `untrack_actor`, `drain_all_for_cleanup`)
//! or through the liveness-probe functions (`with_live_actor`,
//! `with_live_actor_by_id`, `is_actor_live`). Those free functions resolve the
//! registry off the current runtime ([`crate::runtime::rt_current`]); the
//! [`LiveActors`] struct itself is owned by `RuntimeInner`.
//!
//! # Lock-free 2-level paged slab + cold overflow shards
//!
//! Local actor IDs pack a 16-bit route slot and a 48-bit monotonic serial
//! (`1..67_108_864` on the fast path). `LiveActorSlab` indexes directly by
//! `serial` across a demand-paged 2-level radix tree (`256 * 256 * 1024`
//! slots), giving each actor its own independent atomic reader-writer lease
//! word (`LiveSlabSlot::state`) with **zero OS mutexes** on `pin_actor_by_id`,
//! `with_actor_send_by_id`, `with_live_incarnation`, `track_actor`, and
//! `untrack_actor`. Synthetic unit-test IDs outside the slab span or colliding
//! on the masked serial transparently fall back to sharded overflow maps gated
//! behind `overflow_count`.

use rustc_hash::FxHashMap;
use std::alloc::{alloc_zeroed, dealloc, handle_alloc_error, Layout};
use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::{AtomicPtr, AtomicU64, AtomicUsize, Ordering};

use crate::actor::HewActor;
use crate::lifetime::poison_safe::PoisonSafe;

/// Mask for the 48-bit serial portion of a packed actor ID (`pid.rs`).
const SERIAL_MASK: u64 = (1u64 << 48) - 1;

// 2-level lazy paged slab geometry: 256 * 256 * 1024 = 67,108,864 slots.
const SLAB_L0_LEN: usize = 256;
const SLAB_L1_LEN: usize = 256;
const SLAB_PAGE_BITS: usize = 10;
const SLAB_PAGE_LEN: usize = 1 << SLAB_PAGE_BITS; // 1024
const SLAB_PAGE_MASK: usize = SLAB_PAGE_LEN - 1;
const SLAB_L1_BITS: usize = 8;
const SLAB_L1_MASK: usize = SLAB_L1_LEN - 1;
const LIVE_SLAB_CAPACITY: usize = SLAB_L0_LEN * SLAB_L1_LEN * SLAB_PAGE_LEN;

// Per-slot atomic state flags (portable across 32-bit wasm32 and 64-bit native).
const SLOT_LIVE: usize = 1usize << (usize::BITS - 1);
const SLOT_INIT: usize = 1usize << (usize::BITS - 2);
const SLOT_WRITER: usize = 1usize << (usize::BITS - 3);
const LEASE_MASK: usize = !(SLOT_LIVE | SLOT_INIT | SLOT_WRITER);

/// Cold overflow shards for synthetic test IDs (`serial >= LIVE_SLAB_CAPACITY`
/// or multi-node serial collisions in unit tests).
const OVERFLOW_SHARDS: usize = 16;

#[inline]
fn overflow_shard_index(actor_id: u64) -> usize {
    #[expect(
        clippy::cast_possible_truncation,
        reason = "shard index is masked to OVERFLOW_SHARDS - 1 (0..16)"
    )]
    {
        (actor_id as usize) & (OVERFLOW_SHARDS - 1)
    }
}

#[inline]
fn slab_index_of(actor_id: u64) -> Option<usize> {
    let serial = actor_id & SERIAL_MASK;
    if serial == 0 {
        return None;
    }
    let idx = usize::try_from(serial).ok()?;
    (idx < LIVE_SLAB_CAPACITY).then_some(idx)
}

/// Opaque wrapper around `*mut HewActor` that makes it `Send`.
///
/// # Safety
///
/// Actor pointers are managed by the runtime and only freed under
/// controlled conditions (shutdown or explicit free via `hew_actor_free`).
pub(crate) struct ActorPtr(pub(crate) *mut HewActor);

// SAFETY: see doc on ActorPtr above.
unsafe impl Send for ActorPtr {}
// SAFETY: `HewActor` is `Sync` and all reads under shared registry leases only
// access synchronized/atomic actor fields.
unsafe impl Sync for ActorPtr {}

struct LiveActor {
    actor: ActorPtr,
}

fn retire_actor_route(actor: *mut HewActor, actor_id: u64) {
    // SAFETY: callers invoke this only after successful removal from the live
    // registry and retain allocation ownership until finalization.
    let token = unsafe { (*actor).local_pid_id };
    if token == crate::lifetime::local_handles::HewLocalPidId::INVALID {
        return;
    }
    // SAFETY: callers invoke this only after removal from the live registry while
    // retaining allocation ownership until finalization.
    let runtime = unsafe { (*actor).runtime };
    // SAFETY: the owning runtime outlives tracked actor route retirement.
    unsafe { crate::platform::executor::retire_tracked_actor_route(runtime, token, actor_id) };
}

/// One direct-indexed actor liveness slot (`32` bytes on 64-bit).
#[repr(C, align(32))]
struct LiveSlabSlot {
    state: AtomicUsize,
    actor_id: AtomicU64,
    actor_ptr: AtomicPtr<HewActor>,
}

struct SlotLeaseGuard<'a>(&'a LiveSlabSlot);

impl Drop for SlotLeaseGuard<'_> {
    #[inline]
    fn drop(&mut self) {
        self.0.state.fetch_sub(1, Ordering::Release);
    }
}

enum SlabLookup<'a> {
    Empty,
    Matched(*mut HewActor, SlotLeaseGuard<'a>),
    Collision,
}

impl LiveSlabSlot {
    #[inline]
    fn try_lease_by_id(&self, expected_id: u64) -> SlabLookup<'_> {
        let mut cur = self.state.load(Ordering::Acquire);
        loop {
            if (cur & (SLOT_INIT | SLOT_WRITER)) != 0 {
                std::hint::spin_loop();
                cur = self.state.load(Ordering::Acquire);
                continue;
            }
            if (cur & SLOT_LIVE) == 0 {
                return SlabLookup::Empty;
            }
            debug_assert!((cur & LEASE_MASK) < LEASE_MASK);
            match self.state.compare_exchange_weak(
                cur,
                cur + 1,
                Ordering::AcqRel,
                Ordering::Acquire,
            ) {
                Ok(_) => {
                    let guard = SlotLeaseGuard(self);
                    if self.actor_id.load(Ordering::Relaxed) == expected_id {
                        let ptr = self.actor_ptr.load(Ordering::Relaxed);
                        return SlabLookup::Matched(ptr, guard);
                    }
                    drop(guard);
                    return SlabLookup::Collision;
                }
                Err(actual) => cur = actual,
            }
        }
    }

    #[inline]
    fn try_lease_any(&self) -> Option<(u64, *mut HewActor, SlotLeaseGuard<'_>)> {
        let mut cur = self.state.load(Ordering::Acquire);
        loop {
            if (cur & (SLOT_INIT | SLOT_WRITER)) != 0 {
                std::hint::spin_loop();
                cur = self.state.load(Ordering::Acquire);
                continue;
            }
            if (cur & SLOT_LIVE) == 0 {
                return None;
            }
            debug_assert!((cur & LEASE_MASK) < LEASE_MASK);
            match self.state.compare_exchange_weak(
                cur,
                cur + 1,
                Ordering::AcqRel,
                Ordering::Acquire,
            ) {
                Ok(_) => {
                    let guard = SlotLeaseGuard(self);
                    let id = self.actor_id.load(Ordering::Relaxed);
                    let ptr = self.actor_ptr.load(Ordering::Relaxed);
                    return Some((id, ptr, guard));
                }
                Err(actual) => cur = actual,
            }
        }
    }

    /// Acquire exclusive writer access (`SLOT_WRITER`) if this slot is live,
    /// waiting for any in-flight reader leases on this slot to drain.
    #[inline]
    fn acquire_writer(&self) -> bool {
        let mut cur = self.state.load(Ordering::Acquire);
        loop {
            if (cur & (SLOT_INIT | SLOT_WRITER)) != 0 {
                std::hint::spin_loop();
                cur = self.state.load(Ordering::Acquire);
                continue;
            }
            if (cur & SLOT_LIVE) == 0 {
                return false;
            }
            match self.state.compare_exchange_weak(
                cur,
                cur | SLOT_WRITER,
                Ordering::AcqRel,
                Ordering::Acquire,
            ) {
                Ok(_) => {
                    while (self.state.load(Ordering::Acquire) & LEASE_MASK) != 0 {
                        std::hint::spin_loop();
                    }
                    return true;
                }
                Err(actual) => cur = actual,
            }
        }
    }

    #[inline]
    fn release_writer_keep_live(&self) {
        self.state.fetch_and(!SLOT_WRITER, Ordering::Release);
    }

    #[inline]
    fn clear_writer_and_remove(&self) {
        self.actor_ptr.store(ptr::null_mut(), Ordering::Relaxed);
        self.actor_id.store(0, Ordering::Relaxed);
        self.state.store(0, Ordering::Release);
    }
}

#[repr(C)]
struct LiveSlabPage {
    slots: [LiveSlabSlot; SLAB_PAGE_LEN],
}

#[repr(C)]
struct LiveSlabL1 {
    pages: [AtomicPtr<LiveSlabPage>; SLAB_L1_LEN],
}

struct LiveActorSlab {
    l0: [AtomicPtr<LiveSlabL1>; SLAB_L0_LEN],
    /// Highest slab index ever inserted in this runtime, bounding whole-slab scans.
    max_index: AtomicUsize,
}

impl LiveActorSlab {
    const fn new() -> Self {
        Self {
            l0: [const { AtomicPtr::new(ptr::null_mut()) }; SLAB_L0_LEN],
            max_index: AtomicUsize::new(0),
        }
    }

    #[expect(
        clippy::cast_ptr_alignment,
        reason = "alloc_zeroed receives Layout::new::<LiveSlabL1>() which guarantees LiveSlabL1 alignment"
    )]
    fn alloc_l1() -> *mut LiveSlabL1 {
        let layout = Layout::new::<LiveSlabL1>();
        // SAFETY: `LiveSlabL1` is an array of `AtomicPtr` valid when zero-initialized.
        let raw = unsafe { alloc_zeroed(layout) }.cast::<LiveSlabL1>();
        if raw.is_null() {
            handle_alloc_error(layout);
        }
        raw
    }

    #[expect(
        clippy::cast_ptr_alignment,
        reason = "alloc_zeroed receives Layout::new::<LiveSlabPage>() which guarantees LiveSlabPage alignment"
    )]
    fn alloc_page() -> *mut LiveSlabPage {
        let layout = Layout::new::<LiveSlabPage>();
        // SAFETY: `LiveSlabPage` contains only zero-valid atomics (`0` = empty slot).
        let raw = unsafe { alloc_zeroed(layout) }.cast::<LiveSlabPage>();
        if raw.is_null() {
            handle_alloc_error(layout);
        }
        raw
    }

    #[inline]
    fn get_slot(&self, idx: usize) -> Option<&LiveSlabSlot> {
        let l0_idx = idx >> (SLAB_PAGE_BITS + SLAB_L1_BITS);
        let l1_idx = (idx >> SLAB_PAGE_BITS) & SLAB_L1_MASK;
        let page_idx = idx & SLAB_PAGE_MASK;

        let l1 = self.l0[l0_idx].load(Ordering::Acquire);
        if l1.is_null() {
            return None;
        }
        // SAFETY: non-null `l1` lives until `LiveActorSlab::drop`.
        let page = unsafe { (*l1).pages[l1_idx].load(Ordering::Acquire) };
        if page.is_null() {
            return None;
        }
        // SAFETY: non-null `page` lives until `LiveActorSlab::drop`.
        Some(unsafe { &(*page).slots[page_idx] })
    }

    #[inline]
    fn ensure_slot(&self, idx: usize) -> &LiveSlabSlot {
        let l0_idx = idx >> (SLAB_PAGE_BITS + SLAB_L1_BITS);
        let l1_idx = (idx >> SLAB_PAGE_BITS) & SLAB_L1_MASK;
        let page_idx = idx & SLAB_PAGE_MASK;

        let mut l1 = self.l0[l0_idx].load(Ordering::Acquire);
        if l1.is_null() {
            let fresh = Self::alloc_l1();
            match self.l0[l0_idx].compare_exchange(
                ptr::null_mut(),
                fresh,
                Ordering::AcqRel,
                Ordering::Acquire,
            ) {
                Ok(_) => l1 = fresh,
                Err(existing) => {
                    // SAFETY: `fresh` was never published.
                    unsafe { dealloc(fresh.cast::<u8>(), Layout::new::<LiveSlabL1>()) };
                    l1 = existing;
                }
            }
        }

        // SAFETY: `l1` is non-null and valid for the lifetime of `self`.
        let page_slot = unsafe { &(*l1).pages[l1_idx] };
        let mut page = page_slot.load(Ordering::Acquire);
        if page.is_null() {
            let fresh = Self::alloc_page();
            match page_slot.compare_exchange(
                ptr::null_mut(),
                fresh,
                Ordering::AcqRel,
                Ordering::Acquire,
            ) {
                Ok(_) => page = fresh,
                Err(existing) => {
                    // SAFETY: `fresh` was never published.
                    unsafe { dealloc(fresh.cast::<u8>(), Layout::new::<LiveSlabPage>()) };
                    page = existing;
                }
            }
        }

        self.max_index.fetch_max(idx, Ordering::AcqRel);
        // SAFETY: `page` is non-null and valid for the lifetime of `self`.
        unsafe { &(*page).slots[page_idx] }
    }

    /// Visit every allocated slot up to `max_index`, stopping early when `visit`
    /// returns `Some(R)`. Unallocated `L0`/`L1` subtrees are skipped in `O(1)`.
    fn visit_allocated_slots<R>(
        &self,
        mut visit: impl FnMut(&LiveSlabSlot) -> Option<R>,
    ) -> Option<R> {
        let bound = self.max_index.load(Ordering::Acquire);
        if bound == 0 {
            return None;
        }
        let max_l0 = bound >> (SLAB_PAGE_BITS + SLAB_L1_BITS);
        for l0_idx in 0..=max_l0.min(SLAB_L0_LEN - 1) {
            let l1 = self.l0[l0_idx].load(Ordering::Acquire);
            if l1.is_null() {
                continue;
            }
            let max_l1 = if l0_idx == max_l0 {
                (bound >> SLAB_PAGE_BITS) & SLAB_L1_MASK
            } else {
                SLAB_L1_LEN - 1
            };
            for l1_idx in 0..=max_l1 {
                // SAFETY: `l1` is live until `LiveActorSlab::drop`.
                let page = unsafe { (*l1).pages[l1_idx].load(Ordering::Acquire) };
                if page.is_null() {
                    continue;
                }
                let page_base =
                    (l0_idx << (SLAB_PAGE_BITS + SLAB_L1_BITS)) | (l1_idx << SLAB_PAGE_BITS);
                let max_slot = if page_base + SLAB_PAGE_LEN - 1 > bound {
                    bound - page_base
                } else {
                    SLAB_PAGE_LEN - 1
                };
                for slot_idx in 0..=max_slot {
                    // SAFETY: `page` is live until `LiveActorSlab::drop`.
                    let slot = unsafe { &(*page).slots[slot_idx] };
                    if let Some(out) = visit(slot) {
                        return Some(out);
                    }
                }
            }
        }
        None
    }
}

impl Drop for LiveActorSlab {
    fn drop(&mut self) {
        for l0_entry in &mut self.l0 {
            let l1 = *l0_entry.get_mut();
            if l1.is_null() {
                continue;
            }
            // SAFETY: exclusive `&mut self` during drop.
            unsafe {
                for page_entry in &mut (*l1).pages {
                    let page = *page_entry.get_mut();
                    if !page.is_null() {
                        dealloc(page.cast::<u8>(), Layout::new::<LiveSlabPage>());
                    }
                }
                dealloc(l1.cast::<u8>(), Layout::new::<LiveSlabL1>());
            }
        }
    }
}

/// Cold overflow shard for out-of-range or colliding synthetic actor IDs.
#[repr(align(64))]
struct OverflowShard(PoisonSafe<Option<FxHashMap<u64, LiveActor>>>);

/// Runtime-owned actor-liveness state.
pub(crate) struct LiveActors {
    slab: LiveActorSlab,
    overflow_count: AtomicUsize,
    overflow: [OverflowShard; OVERFLOW_SHARDS],
    /// Exact count of tracked actors across slab + overflow.
    len: AtomicUsize,
    /// Background teardown threads that hold raw actor/supervisor pointers.
    #[cfg(not(target_arch = "wasm32"))]
    deferred_teardown_threads: PoisonSafe<Vec<std::thread::JoinHandle<()>>>,
}

impl LiveActors {
    /// Construct an empty liveness registry for a runtime.
    pub(crate) const fn new() -> Self {
        Self {
            slab: LiveActorSlab::new(),
            overflow_count: AtomicUsize::new(0),
            overflow: [const { OverflowShard(PoisonSafe::new(None)) }; OVERFLOW_SHARDS],
            len: AtomicUsize::new(0),
            #[cfg(not(target_arch = "wasm32"))]
            deferred_teardown_threads: PoisonSafe::new(Vec::new()),
        }
    }

    #[inline]
    fn has_overflow(&self) -> bool {
        self.overflow_count.load(Ordering::Acquire) != 0
    }
}

enum SlabInsert {
    Inserted,
    DuplicateId,
    NeedsOverflow,
}

fn try_insert_slab(actors: &LiveActors, idx: usize, id: u64, actor: *mut HewActor) -> SlabInsert {
    let slot = actors.slab.ensure_slot(idx);
    let mut cur = slot.state.load(Ordering::Acquire);
    loop {
        if cur == 0 {
            match slot.state.compare_exchange_weak(
                0,
                SLOT_INIT,
                Ordering::AcqRel,
                Ordering::Acquire,
            ) {
                Ok(_) => {
                    slot.actor_id.store(id, Ordering::Relaxed);
                    slot.actor_ptr.store(actor, Ordering::Relaxed);
                    slot.state.store(SLOT_LIVE, Ordering::Release);
                    actors.len.fetch_add(1, Ordering::AcqRel);
                    return SlabInsert::Inserted;
                }
                Err(actual) => {
                    cur = actual;
                    continue;
                }
            }
        }
        if (cur & (SLOT_INIT | SLOT_WRITER)) != 0 {
            std::hint::spin_loop();
            cur = slot.state.load(Ordering::Acquire);
            continue;
        }
        match slot.try_lease_by_id(id) {
            SlabLookup::Matched(_, _guard) => return SlabInsert::DuplicateId,
            SlabLookup::Collision => return SlabInsert::NeedsOverflow,
            SlabLookup::Empty => {
                cur = slot.state.load(Ordering::Acquire);
            }
        }
    }
}

fn try_remove_matching(actors: &LiveActors, actor_id: u64, expected: *mut HewActor) -> bool {
    if let Some(idx) = slab_index_of(actor_id) {
        if let Some(slot) = actors.slab.get_slot(idx) {
            if slot.acquire_writer() {
                let id_matches = slot.actor_id.load(Ordering::Relaxed) == actor_id;
                let ptr_matches = slot.actor_ptr.load(Ordering::Relaxed) == expected;
                if id_matches && ptr_matches {
                    slot.clear_writer_and_remove();
                    actors.len.fetch_sub(1, Ordering::AcqRel);
                    return true;
                }
                slot.release_writer_keep_live();
                if id_matches {
                    return false;
                }
            }
        }
    }
    if !actors.has_overflow() {
        return false;
    }
    actors.overflow[overflow_shard_index(actor_id)]
        .0
        .access(|map| {
            if let Some(m) = map.as_mut() {
                if let std::collections::hash_map::Entry::Occupied(entry) = m.entry(actor_id) {
                    if entry.get().actor.0 == expected {
                        entry.remove();
                        actors.overflow_count.fetch_sub(1, Ordering::AcqRel);
                        actors.len.fetch_sub(1, Ordering::AcqRel);
                        return true;
                    }
                }
            }
            false
        })
}

/// Register an actor in the live tracking map.
///
/// # Safety
///
/// `actor` must be a valid, fully initialised `HewActor` pointer whose
/// `id` field is already set.
pub(crate) unsafe fn track_actor(actor: *mut HewActor) -> bool {
    // SAFETY: caller guarantees `actor` is valid and initialised.
    let id = unsafe { (*actor).id };
    let actors = crate::platform::executor::current_live_actors();
    if actors.has_overflow()
        && actors.overflow[overflow_shard_index(id)]
            .0
            .access(|map| map.as_ref().is_some_and(|m| m.contains_key(&id)))
    {
        return false;
    }
    if let Some(idx) = slab_index_of(id) {
        match try_insert_slab(actors, idx, id, actor) {
            SlabInsert::Inserted => return true,
            SlabInsert::DuplicateId => return false,
            SlabInsert::NeedsOverflow => {}
        }
    }
    actors.overflow[overflow_shard_index(id)].0.access(|map| {
        match map.get_or_insert_with(FxHashMap::default).entry(id) {
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(LiveActor {
                    actor: ActorPtr(actor),
                });
                actors.overflow_count.fetch_add(1, Ordering::AcqRel);
                actors.len.fetch_add(1, Ordering::AcqRel);
                true
            }
            std::collections::hash_map::Entry::Occupied(_) => false,
        }
    })
}

#[cfg(test)]
pub(crate) fn actor_count_for_test() -> usize {
    crate::platform::executor::current_live_actors_opt()
        .map_or(0, |actors| actors.len.load(Ordering::Acquire))
}

/// Remove an actor from the live tracking map.
pub(crate) fn untrack_actor(actor: *mut HewActor) -> bool {
    // SAFETY: caller guarantees `actor` is valid and not yet freed.
    let id = unsafe { (*actor).id };
    let Some(actors) = crate::platform::executor::current_live_actors_opt() else {
        return false;
    };
    let removed = try_remove_matching(actors, id, actor);
    if removed {
        retire_actor_route(actor, id);
    }
    removed
}

/// Remove and return the actor tracked under `actor_id` if it still matches `expected`.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn take_actor_by_id(actor_id: u64, expected: *mut HewActor) -> Option<*mut HewActor> {
    let actors = crate::platform::executor::current_live_actors_opt()?;
    if try_remove_matching(actors, actor_id, expected) {
        retire_actor_route(expected, actor_id);
        Some(expected)
    } else {
        None
    }
}

/// Check whether an actor pointer is still live without dereferencing `actor`
/// until registry membership is confirmed.
pub(crate) fn with_live_actor<R>(
    actor: *mut HewActor,
    f: impl FnOnce(&HewActor) -> R,
) -> Option<R> {
    let actors = crate::platform::executor::current_live_actors_opt()?;
    let mut f = Some(f);
    let found = actors.slab.visit_allocated_slots(|slot| {
        let (_, ptr, _guard) = slot.try_lease_any()?;
        if ptr == actor {
            let func = f.take().expect("closure runs at most once");
            // SAFETY: `_guard` holds the slot's reader lease across `func`.
            Some(func(unsafe { &*actor }))
        } else {
            None
        }
    });
    if found.is_some() {
        return found;
    }
    if !actors.has_overflow() {
        return None;
    }
    for shard in &actors.overflow {
        let result = shard.0.access(|map| {
            if map
                .as_ref()
                .is_some_and(|m| m.values().any(|ptr| ptr.actor.0 == actor))
            {
                let func = f.take().expect("closure runs at most once");
                // SAFETY: `actor` is tracked under this overflow shard's lock.
                Some(func(unsafe { &*actor }))
            } else {
                None
            }
        });
        if result.is_some() {
            return result;
        }
    }
    None
}

/// Swap `slot` to null under the owning actor's per-slot writer lock (or
/// overflow shard lock), serializing against [`has_drain_blocking_suspended_actor`].
pub(crate) fn swap_slot_under_registry_lock(
    actor_id: u64,
    slot: &AtomicPtr<c_void>,
) -> *mut c_void {
    let Some(actors) = crate::platform::executor::current_live_actors_opt() else {
        return slot.swap(ptr::null_mut(), Ordering::AcqRel);
    };
    if let Some(idx) = slab_index_of(actor_id) {
        if let Some(slab_slot) = actors.slab.get_slot(idx) {
            if slab_slot.acquire_writer() {
                if slab_slot.actor_id.load(Ordering::Relaxed) == actor_id {
                    let prev = slot.swap(ptr::null_mut(), Ordering::AcqRel);
                    slab_slot.release_writer_keep_live();
                    return prev;
                }
                slab_slot.release_writer_keep_live();
            }
        }
    }
    if actors.has_overflow() {
        return actors.overflow[overflow_shard_index(actor_id)]
            .0
            .access(|_| slot.swap(ptr::null_mut(), Ordering::AcqRel));
    }
    slot.swap(ptr::null_mut(), Ordering::AcqRel)
}

/// Check whether an actor ID still maps to the expected live actor pointer.
pub(crate) fn with_live_actor_by_id<R>(
    actor_id: u64,
    expected: *mut HewActor,
    f: impl FnOnce(&HewActor) -> R,
) -> Option<R> {
    let actors = crate::platform::executor::current_live_actors_opt()?;
    if let Some(idx) = slab_index_of(actor_id) {
        if let Some(slot) = actors.slab.get_slot(idx) {
            match slot.try_lease_by_id(actor_id) {
                SlabLookup::Matched(ptr, _guard) => {
                    return (ptr == expected).then(|| {
                        // SAFETY: `_guard` holds the slot lease across `f`.
                        f(unsafe { &*expected })
                    });
                }
                SlabLookup::Empty => {
                    if !actors.has_overflow() {
                        return None;
                    }
                }
                SlabLookup::Collision => {}
            }
        } else if !actors.has_overflow() {
            return None;
        }
    }
    if !actors.has_overflow() {
        return None;
    }
    actors.overflow[overflow_shard_index(actor_id)]
        .0
        .access(|map| {
            if map
                .as_ref()
                .and_then(|m| m.get(&actor_id))
                .is_some_and(|ptr| ptr.actor.0 == expected)
            {
                // SAFETY: `expected` is tracked under this overflow shard's lock.
                Some(f(unsafe { &*expected }))
            } else {
                None
            }
        })
}

/// Look up an actor by ID and return a copy of its raw pointer if live.
pub(crate) fn get_actor_ptr_by_id(actor_id: u64) -> Option<*mut HewActor> {
    let actors = crate::platform::executor::current_live_actors_opt()?;
    if let Some(idx) = slab_index_of(actor_id) {
        if let Some(slot) = actors.slab.get_slot(idx) {
            match slot.try_lease_by_id(actor_id) {
                SlabLookup::Matched(ptr, _guard) => {
                    return (!ptr.is_null()).then_some(ptr);
                }
                SlabLookup::Empty => {
                    if !actors.has_overflow() {
                        return None;
                    }
                }
                SlabLookup::Collision => {}
            }
        } else if !actors.has_overflow() {
            return None;
        }
    }
    if !actors.has_overflow() {
        return None;
    }
    actors.overflow[overflow_shard_index(actor_id)]
        .0
        .access(|map| {
            map.as_ref()
                .and_then(|m| m.get(&actor_id).map(|ptr| ptr.actor.0))
                .filter(|ptr| !ptr.is_null())
        })
}

/// RAII guard that decrements `HewActor.send_pin_count` on drop.
pub(crate) struct ActorPin(*mut HewActor);

// SAFETY: `HewActor` is `Send`; the guard only touches the atomic
// `send_pin_count` field.
unsafe impl Send for ActorPin {}

impl Drop for ActorPin {
    #[inline]
    fn drop(&mut self) {
        // SAFETY: the pointer was validated-live when the pin was taken.
        // The free path defers reclamation until `send_pin_count` reaches 0.
        unsafe { (*self.0).send_pin_count.fetch_sub(1, Ordering::Release) };
    }
}

impl ActorPin {
    /// Borrow the pinned actor allocation.
    #[inline]
    #[must_use]
    pub(crate) fn actor(&self) -> &HewActor {
        // SAFETY: construction increments `send_pin_count` while the actor is
        // tracked; the free path cannot reclaim it until this guard drops.
        unsafe { &*self.0 }
    }

    /// Return the pinned actor pointer for existing internal operations.
    #[inline]
    #[must_use]
    pub(crate) fn as_ptr(&self) -> *mut HewActor {
        self.0
    }
}

/// Resolve one `ActorId` and hold its liveness/send pin until drop (**lock-free**
/// on the 67M-entry paged slab fast path).
#[inline]
pub(crate) fn pin_actor_by_id(actor_id: u64) -> Option<ActorPin> {
    let actors = crate::platform::executor::current_live_actors_opt()?;
    if let Some(idx) = slab_index_of(actor_id) {
        if let Some(slot) = actors.slab.get_slot(idx) {
            match slot.try_lease_by_id(actor_id) {
                SlabLookup::Matched(ptr, guard) => {
                    if ptr.is_null() {
                        return None;
                    }
                    // SAFETY: `guard` holds the slot's reader lease, preventing
                    // `untrack_actor` from completing before `send_pin_count`
                    // is incremented.
                    unsafe { (*ptr).send_pin_count.fetch_add(1, Ordering::AcqRel) };
                    drop(guard);
                    return Some(ActorPin(ptr));
                }
                SlabLookup::Empty => {
                    if !actors.has_overflow() {
                        return None;
                    }
                }
                SlabLookup::Collision => {}
            }
        } else if !actors.has_overflow() {
            return None;
        }
    }
    if !actors.has_overflow() {
        return None;
    }
    let ptr = actors.overflow[overflow_shard_index(actor_id)]
        .0
        .access(|map| {
            let ptr = map
                .as_ref()
                .and_then(|m| m.get(&actor_id).map(|p| p.actor.0))
                .filter(|p| !p.is_null())?;
            // SAFETY: tracked under the overflow shard's lock.
            unsafe { (*ptr).send_pin_count.fetch_add(1, Ordering::AcqRel) };
            Some(ptr)
        })?;
    Some(ActorPin(ptr))
}

/// Look up an actor by ID and, if live, call `f` with its raw pointer while
/// guaranteeing the allocation cannot be freed for the duration of `f`.
#[inline]
pub(crate) fn with_actor_send_by_id<R>(
    actor_id: u64,
    f: impl FnOnce(*mut HewActor) -> R,
) -> Option<R> {
    let pin = pin_actor_by_id(actor_id)?;
    Some(f(pin.as_ptr()))
}

/// Like [`with_actor_send_by_id`], additionally verifying `spawn_serial`.
#[inline]
pub(crate) fn with_actor_send_by_identity<R>(
    actor_id: u64,
    expected_serial: u64,
    f: impl FnOnce(&ActorPin) -> R,
) -> Option<R> {
    let pin = pin_actor_by_id(actor_id)?;
    if pin.actor().spawn_serial != expected_serial {
        return None;
    }
    Some(f(&pin))
}

/// The exact identity of one actor incarnation: the location-transparent `id`
/// plus the never-reissued [`HewActor::spawn_serial`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct ActorIncarnation {
    actor_id: u64,
    spawn_serial: u64,
}

impl ActorIncarnation {
    /// No wake target.
    pub(crate) const NONE: Self = Self {
        actor_id: 0,
        spawn_serial: 0,
    };

    /// Capture the incarnation of a live actor, or [`Self::NONE`] for null.
    ///
    /// # Safety
    ///
    /// `actor`, if non-null, must be live for the duration of this call.
    #[inline]
    pub(crate) unsafe fn of(actor: *mut HewActor) -> Self {
        if actor.is_null() {
            return Self::NONE;
        }
        // SAFETY: caller guarantees `actor` is live for this read.
        unsafe {
            Self {
                actor_id: (*actor).id,
                spawn_serial: (*actor).spawn_serial,
            }
        }
    }

    /// Rebuild an incarnation from its two scalars (an ABI-boundary record).
    #[inline]
    pub(crate) const fn from_parts(actor_id: u64, spawn_serial: u64) -> Self {
        Self {
            actor_id,
            spawn_serial,
        }
    }

    /// Whether this records no target at all.
    #[inline]
    pub(crate) const fn is_none(self) -> bool {
        self.actor_id == 0
    }

    /// The location-transparent actor id half of the pair.
    #[inline]
    pub(crate) const fn actor_id(self) -> u64 {
        self.actor_id
    }

    /// The never-reissued serial half of the pair.
    #[inline]
    pub(crate) const fn spawn_serial(self) -> u64 {
        self.spawn_serial
    }
}

/// Pin the exact incarnation named by `target` and run `f` while it is held.
#[inline]
pub(crate) fn with_live_incarnation<R>(
    target: ActorIncarnation,
    f: impl FnOnce(&ActorPin) -> R,
) -> Option<R> {
    if target.is_none() {
        return None;
    }
    with_actor_send_by_identity(target.actor_id, target.spawn_serial, f)
}

/// Check whether an actor pointer is still live (tracked and not yet freed).
#[cfg_attr(
    not(test),
    allow(
        dead_code,
        reason = "supervisor and actor tests rely on the liveness probe"
    )
)]
pub(crate) fn is_actor_live(actor: *mut HewActor) -> bool {
    with_live_actor(actor, |_| ()).is_some()
}

/// Check whether `actor_id` still maps to the expected live actor pointer.
#[cfg_attr(
    not(test),
    allow(
        dead_code,
        reason = "supervisor and actor tests rely on the liveness probe"
    )
)]
pub(crate) fn is_actor_live_with_id(actor_id: u64, expected: *mut HewActor) -> bool {
    with_live_actor_by_id(actor_id, expected, |_| ()).is_some()
}

#[cfg(not(target_arch = "wasm32"))]
mod native {
    use super::{retire_actor_route, ActorPtr, FxHashMap, HewActor, Ordering};
    use crate::runtime::{rt_current, rt_current_opt};
    use std::collections::HashMap;

    #[inline]
    unsafe fn is_actor_drain_blocking(actor: *mut HewActor) -> bool {
        // SAFETY: caller holds either the slot reader lease or the overflow shard lock.
        let a = unsafe { &*actor };
        if a.actor_state.load(Ordering::Acquire)
            != crate::internal::types::HewActorState::Suspended as i32
        {
            return false;
        }
        if !a.checked_invocation.load(Ordering::Acquire).is_null() {
            return true;
        }
        let gate = a.parked_ask_channel.load(Ordering::Acquire);
        if gate.is_null() {
            return true;
        }
        // SAFETY: `parked_ask_channel` is swapped to null under `SLOT_WRITER`
        // (or the overflow shard lock) before releasing the retained channel.
        let cancelled = unsafe {
            (*gate.cast::<crate::reply_channel::HewReplyChannel>())
                .cancelled
                .load(Ordering::Acquire)
        };
        !cancelled
    }

    pub(crate) fn has_drain_blocking_suspended_actor() -> bool {
        let Some(actors) = crate::platform::executor::current_live_actors_opt() else {
            return false;
        };
        if actors
            .slab
            .visit_allocated_slots(|slot| {
                let (_, ptr, _guard) = slot.try_lease_any()?;
                // SAFETY: `_guard` holds the slot reader lease across inspection,
                // serializing against `swap_slot_under_registry_lock` and `untrack_actor`.
                unsafe { is_actor_drain_blocking(ptr) }.then_some(true)
            })
            .is_some()
        {
            return true;
        }
        if !actors.has_overflow() {
            return false;
        }
        for shard in &actors.overflow {
            let blocked = shard.0.access(|map| {
                map.as_ref().is_some_and(|entries| {
                    entries
                        .values()
                        // SAFETY: overflow shard lock held across inspection.
                        .any(|entry| unsafe { is_actor_drain_blocking(entry.actor.0) })
                })
            });
            if blocked {
                return true;
            }
        }
        false
    }

    /// Safe to call only once worker threads are joined: the returned pointers
    /// are unpinned, so a concurrent free would dangle them.
    pub(crate) fn snapshot_live_actor_ptrs() -> Vec<*mut HewActor> {
        let Some(actors) = crate::platform::executor::current_live_actors_opt() else {
            return Vec::new();
        };
        let mut ptrs = Vec::new();
        let _ = actors.slab.visit_allocated_slots::<()>(|slot| {
            if let Some((_, ptr, _guard)) = slot.try_lease_any() {
                ptrs.push(ptr);
            }
            None
        });
        if actors.has_overflow() {
            for shard in &actors.overflow {
                shard.0.access(|map| {
                    if let Some(m) = map.as_ref() {
                        ptrs.extend(m.values().map(|entry| entry.actor.0));
                    }
                });
            }
        }
        ptrs
    }

    /// Copy identities while workers are live. Resolve each through a fresh pin
    /// before accessing its allocation; an actor may retire after this snapshot.
    pub(crate) fn snapshot_live_actor_ids() -> Vec<u64> {
        let Some(actors) = crate::platform::executor::current_live_actors_opt() else {
            return Vec::new();
        };
        let mut ids = Vec::new();
        let _ = actors.slab.visit_allocated_slots::<()>(|slot| {
            if let Some((id, _, _guard)) = slot.try_lease_any() {
                ids.push(id);
            }
            None
        });
        if actors.has_overflow() {
            for shard in &actors.overflow {
                shard.0.access(|map| {
                    if let Some(m) = map.as_ref() {
                        ids.extend(m.keys().copied());
                    }
                });
            }
        }
        ids
    }

    /// Take all currently tracked actors out of the live registry.
    pub(crate) fn drain_all_for_cleanup() -> HashMap<u64, ActorPtr> {
        let Some(actors) = crate::platform::executor::current_live_actors_opt() else {
            return HashMap::new();
        };
        let mut drained = HashMap::new();
        let _ = actors.slab.visit_allocated_slots::<()>(|slot| {
            if slot.acquire_writer() {
                let actor_id = slot.actor_id.load(Ordering::Relaxed);
                let ptr = slot.actor_ptr.load(Ordering::Relaxed);
                slot.clear_writer_and_remove();
                actors.len.fetch_sub(1, Ordering::AcqRel);
                if !ptr.is_null() {
                    retire_actor_route(ptr, actor_id);
                }
                drained.insert(actor_id, ActorPtr(ptr));
            }
            None
        });
        if actors.has_overflow() {
            for shard in &actors.overflow {
                let taken = shard.0.access(|map| match map.as_mut() {
                    Some(m) => {
                        let entries = std::mem::take(m);
                        let n = entries.len();
                        if n != 0 {
                            actors.overflow_count.fetch_sub(n, Ordering::AcqRel);
                            actors.len.fetch_sub(n, Ordering::AcqRel);
                        }
                        entries
                    }
                    None => FxHashMap::default(),
                });
                for (actor_id, entry) in taken {
                    if !entry.actor.0.is_null() {
                        retire_actor_route(entry.actor.0, actor_id);
                    }
                    drained.insert(actor_id, entry.actor);
                }
            }
        }
        drained
    }

    pub(crate) fn push_deferred_teardown_thread(handle: std::thread::JoinHandle<()>) {
        let finished = rt_current()
            .live_actors
            .deferred_teardown_threads
            .access(|threads| {
                threads.push(handle);
                let mut finished = Vec::new();
                for i in (0..threads.len()).rev() {
                    if threads[i].is_finished() {
                        finished.push(threads.swap_remove(i));
                    }
                }
                finished
            });
        for handle in finished {
            crate::util::report_join_panic("deferred teardown thread", handle.join());
        }
    }

    pub(crate) fn drain_deferred_teardown_threads() {
        let Some(rt) = rt_current_opt() else { return };
        let threads = &rt.live_actors.deferred_teardown_threads;
        loop {
            let handles = threads.access(|threads| {
                if threads.is_empty() {
                    None
                } else {
                    Some(std::mem::take(threads))
                }
            });
            let Some(handles) = handles else { return };
            for handle in handles {
                crate::util::report_join_panic("deferred teardown thread", handle.join());
            }
        }
    }

    #[cfg(test)]
    pub(crate) fn deferred_teardown_thread_count() -> usize {
        rt_current()
            .live_actors
            .deferred_teardown_threads
            .access(|threads| threads.len())
    }
}

#[cfg(all(test, not(target_arch = "wasm32")))]
pub(crate) use native::deferred_teardown_thread_count;
#[cfg(not(target_arch = "wasm32"))]
pub(crate) use native::{
    drain_all_for_cleanup, drain_deferred_teardown_threads, has_drain_blocking_suspended_actor,
    push_deferred_teardown_thread, snapshot_live_actor_ids, snapshot_live_actor_ptrs,
};

#[cfg(target_arch = "wasm32")]
mod wasm_guest {
    use super::{ActorIncarnation, HewActor, Ordering};

    fn is_guest_actor_active(ptr: *mut HewActor) -> bool {
        use crate::internal::types::HewActorState;
        // SAFETY: caller holds the slot lease or overflow shard lock.
        let actor = unsafe { &*ptr };
        let state = actor.actor_state.load(Ordering::Acquire);
        ![
            HewActorState::Idle,
            HewActorState::Stopped,
            HewActorState::Crashed,
        ]
        .iter()
        .any(|idle| state == *idle as i32)
            || !actor.checked_invocation.load(Ordering::Acquire).is_null()
    }

    /// Retained guest snapshots require every actor turn and its cleanup to finish.
    pub(crate) fn has_active_guest_actor() -> bool {
        let Some(actors) = crate::platform::executor::current_live_actors_opt() else {
            return false;
        };
        if actors
            .slab
            .visit_allocated_slots(|slot| {
                let (_, ptr, _guard) = slot.try_lease_any()?;
                is_guest_actor_active(ptr).then_some(true)
            })
            .is_some()
        {
            return true;
        }
        if !actors.has_overflow() {
            return false;
        }
        for shard in &actors.overflow {
            let active = shard.0.access(|map| {
                map.as_ref().is_some_and(|entries| {
                    entries
                        .values()
                        .any(|entry| is_guest_actor_active(entry.actor.0))
                })
            });
            if active {
                return true;
            }
        }
        false
    }

    /// Copy exact incarnations before cooperative cleanup advances the driver.
    pub(crate) fn snapshot_live_incarnations() -> Vec<ActorIncarnation> {
        let Some(actors) = crate::platform::executor::current_live_actors_opt() else {
            return Vec::new();
        };
        let mut incarnations = Vec::new();
        let _ = actors.slab.visit_allocated_slots::<()>(|slot| {
            if let Some((_, ptr, _guard)) = slot.try_lease_any() {
                // SAFETY: `_guard` retains this tracked allocation.
                incarnations.push(unsafe { ActorIncarnation::of(ptr) });
            }
            None
        });
        if actors.has_overflow() {
            for shard in &actors.overflow {
                shard.0.access(|map| {
                    if let Some(entries) = map.as_ref() {
                        incarnations.extend(entries.values().map(|entry| {
                            // SAFETY: overflow shard lock retains this tracked allocation.
                            unsafe { ActorIncarnation::of(entry.actor.0) }
                        }));
                    }
                });
            }
        }
        incarnations
    }
}

#[cfg(target_arch = "wasm32")]
pub(crate) use wasm_guest::{has_active_guest_actor, snapshot_live_incarnations};

#[cfg(all(test, not(target_arch = "wasm32")))]
mod tests {
    use super::*;

    #[test]
    fn liveness_reads_treat_missing_runtime_as_empty_registry() {
        let _lock = crate::scheduler::SchedTestLock::acquire();
        assert!(
            crate::runtime::rt_default().is_none(),
            "test requires the runtime slot to be empty"
        );

        let actor = std::ptr::dangling_mut::<HewActor>();
        assert_eq!(with_live_actor(actor, |_| unreachable!()), None);
        assert_eq!(with_live_actor_by_id(42, actor, |_| unreachable!()), None);
        assert_eq!(get_actor_ptr_by_id(42), None);
        assert!(!is_actor_live(actor));
        assert!(!is_actor_live_with_id(42, actor));
        assert!(drain_all_for_cleanup().is_empty());
        drain_deferred_teardown_threads();
    }

    #[test]
    fn push_deferred_teardown_thread_reaps_already_finished_handles() {
        use std::sync::atomic::AtomicBool;
        use std::sync::Arc;

        let _rt = crate::scheduler::NoWorkerSchedulerForTest::install();

        for _ in 0..3 {
            let handle = std::thread::spawn(|| {});
            while !handle.is_finished() {
                std::thread::yield_now();
            }
            push_deferred_teardown_thread(handle);
        }

        let handle = std::thread::spawn(|| {});
        while !handle.is_finished() {
            std::thread::yield_now();
        }
        push_deferred_teardown_thread(handle);
        assert_eq!(deferred_teardown_thread_count(), 0);

        let release = Arc::new(AtomicBool::new(false));
        let release2 = Arc::clone(&release);
        push_deferred_teardown_thread(std::thread::spawn(move || {
            while !release2.load(Ordering::Acquire) {
                std::thread::sleep(std::time::Duration::from_millis(2));
            }
        }));

        let handle = std::thread::spawn(|| {});
        while !handle.is_finished() {
            std::thread::yield_now();
        }
        push_deferred_teardown_thread(handle);
        assert_eq!(deferred_teardown_thread_count(), 1);

        release.store(true, Ordering::Release);
        drain_deferred_teardown_threads();
        assert_eq!(deferred_teardown_thread_count(), 0);
    }

    #[test]
    fn paged_slab_and_overflow_concurrent_stress() {
        use std::sync::{Arc, Barrier};

        const THREADS: usize = 8;
        const ACTORS_PER_THREAD: usize = 32;

        let _rt = crate::scheduler::NoWorkerSchedulerForTest::install();
        assert_eq!(actor_count_for_test(), 0);

        let mut owned: Vec<Box<HewActor>> = Vec::with_capacity(THREADS * ACTORS_PER_THREAD + 2);
        for idx in 0..(THREADS * ACTORS_PER_THREAD) {
            let mut actor = Box::new(crate::test_actor::stub_actor());
            actor.id = (idx as u64) + 1;
            actor.spawn_serial = actor.id;
            owned.push(actor);
        }

        let mut node_collision = Box::new(crate::test_actor::stub_actor());
        node_collision.id = (3u64 << 48) | 1;
        node_collision.spawn_serial = 999_999;
        let collision_raw: *mut HewActor = &raw mut *node_collision;

        let ptrs: Arc<Vec<usize>> = Arc::new(
            owned
                .iter_mut()
                .map(|actor| (&raw mut **actor) as usize)
                .collect(),
        );
        let start = Arc::new(Barrier::new(THREADS + 1));

        let mut workers = Vec::with_capacity(THREADS);
        for tid in 0..THREADS {
            let ptrs = Arc::clone(&ptrs);
            let start = Arc::clone(&start);
            workers.push(std::thread::spawn(move || {
                let base = tid * ACTORS_PER_THREAD;
                start.wait();
                for offset in 0..ACTORS_PER_THREAD {
                    let raw = ptrs[base + offset] as *mut HewActor;
                    let id = (base + offset) as u64 + 1;
                    // SAFETY: `owned` remains live until all threads join.
                    assert!(unsafe { track_actor(raw) });
                    // SAFETY: `owned` remains live until all threads join.
                    assert!(!unsafe { track_actor(raw) });

                    assert!(is_actor_live(raw));
                    assert!(is_actor_live_with_id(id, raw));
                    assert_eq!(get_actor_ptr_by_id(id), Some(raw));

                    let pin = pin_actor_by_id(id).expect("tracked actor must pin");
                    assert_eq!(pin.actor().id, id);
                    drop(pin);

                    // SAFETY: `raw` is live until threads join.
                    let slot = unsafe { &(*raw).parked_ask_channel };
                    let _ = swap_slot_under_registry_lock(id, slot);

                    if offset % 2 == 0 {
                        assert!(untrack_actor(raw));
                        assert!(!is_actor_live_with_id(id, raw));
                    }
                }
            }));
        }

        start.wait();
        for _ in 0..64 {
            let _ = has_drain_blocking_suspended_actor();
            let _ = snapshot_live_actor_ids();
        }

        for worker in workers {
            worker.join().expect("live_actors stress worker panicked");
        }

        let raw_1 = ptrs[0] as *mut HewActor;
        // SAFETY: `owned` and `collision_actor` remain live through cleanup.
        assert!(unsafe { track_actor(raw_1) });
        // SAFETY: `collision_actor` remains live through cleanup.
        assert!(unsafe { track_actor(collision_raw) });
        // SAFETY: `collision_actor` remains live through cleanup.
        assert!(!unsafe { track_actor(collision_raw) });
        assert_eq!(get_actor_ptr_by_id(1), Some(raw_1));
        assert_eq!(get_actor_ptr_by_id((3u64 << 48) | 1), Some(collision_raw));
        assert!(untrack_actor(raw_1));
        // SAFETY: `collision_actor` remains live through cleanup.
        assert!(!unsafe { track_actor(collision_raw) });
        assert_eq!(get_actor_ptr_by_id((3u64 << 48) | 1), Some(collision_raw));
        assert!(untrack_actor(collision_raw));

        let expected_remaining = (THREADS * ACTORS_PER_THREAD) / 2;
        assert_eq!(actor_count_for_test(), expected_remaining);
        assert_eq!(snapshot_live_actor_ptrs().len(), expected_remaining);

        let drained = drain_all_for_cleanup();
        assert_eq!(drained.len(), expected_remaining);
        assert_eq!(actor_count_for_test(), 0);
        assert!(snapshot_live_actor_ids().is_empty());
    }
}
