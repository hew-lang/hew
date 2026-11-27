//! Proven dependencies of strict local actor turns.
//!
//! Direct nested invocations retain their actor turn identity. Independent task
//! invocations do not: executing a forked ask alone does not block its actor.
//! A readiness wake invalidates the outgoing dependency before scheduling the
//! actor, so a satisfied wait cannot participate in a later false cycle.

use crate::coro_state::HewCoroState;
use crate::lifetime::{live_actors, local_handles};
use crate::util::MutexExt;
use live_actors::ActorIncarnation;
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::VecDeque;
use std::ptr;
use std::sync::atomic::{fence, AtomicU64, AtomicU8, AtomicUsize, Ordering};
use std::sync::{Arc, LazyLock, Mutex, OnceLock};

const STATE_DISARMED: u8 = 0;
const STATE_ARMED: u8 = 1;
const STATE_PENDING: u8 = 2;

const FLAG_CAN_BLOCK: u8 = 1 << 0;
const FLAG_HAS_EXTRA: u8 = 1 << 1;

/// Per-actor inline outgoing wait-graph slot stored inside [`crate::actor::HewActor`].
///
/// Foreign threads (`ready`, `is_target_pending`, `detect_cycle_slow`) inspect
/// or disarm this slot while holding a `live_actors` pin on the owning actor,
/// without touching any global wait-graph shard mutex or dereferencing the
/// operation-owned [`HewActorWaitEdge`] handle.
#[derive(Debug, Default)]
pub(crate) struct ActorWaitSlot {
    /// Monotonic seqlock/generation counter:
    /// - `0`: never installed.
    /// - Odd (`seq & 1 == 1`): `install` is updating target metadata.
    /// - Even (`seq >= 2`): published registration ID (or cleared generation).
    seq: AtomicU64,
    /// `STATE_DISARMED` (0), `STATE_ARMED` (1), or `STATE_PENDING` (2).
    state: AtomicU8,
    flags: AtomicU8,
    primary_actor_id: AtomicU64,
    primary_spawn_serial: AtomicU64,
    /// Cold overflow list populated only when a multi-arm `select` registers
    /// two or more actor targets. Single-target asks and waits never lock or
    /// allocate this field.
    extra_targets: Mutex<Option<(u64, Box<[ActorIncarnation]>)>>,
}

#[derive(Clone)]
struct PendingNode {
    primary_target: Option<ActorIncarnation>,
    extra_targets: Box<[ActorIncarnation]>,
    can_block: bool,
}

impl PendingNode {
    fn targets(&self) -> impl Iterator<Item = ActorIncarnation> + '_ {
        self.primary_target
            .into_iter()
            .chain(self.extra_targets.iter().copied())
    }
}

impl ActorWaitSlot {
    #[inline]
    pub(crate) const fn new() -> Self {
        Self {
            seq: AtomicU64::new(0),
            state: AtomicU8::new(STATE_DISARMED),
            flags: AtomicU8::new(0),
            primary_actor_id: AtomicU64::new(0),
            primary_spawn_serial: AtomicU64::new(0),
            extra_targets: Mutex::new(None),
        }
    }

    #[inline]
    fn install(
        &self,
        primary_target: Option<ActorIncarnation>,
        extra_targets: &[ActorIncarnation],
        external_progress: bool,
    ) -> u64 {
        self.state.swap(STATE_DISARMED, Ordering::SeqCst);

        let mut observed = self.seq.load(Ordering::Acquire);
        let odd = loop {
            if (observed & 1) != 0 {
                std::hint::spin_loop();
                observed = self.seq.load(Ordering::Acquire);
                continue;
            }
            match self.seq.compare_exchange_weak(
                observed,
                observed + 1,
                Ordering::AcqRel,
                Ordering::Acquire,
            ) {
                Ok(_) => break observed + 1,
                Err(next) => observed = next,
            }
        };
        let reg_seq = odd + 1;

        let mut flags = 0u8;
        if !external_progress && primary_target.is_some() {
            flags |= FLAG_CAN_BLOCK;
        }
        if !extra_targets.is_empty() {
            flags |= FLAG_HAS_EXTRA;
        }

        let (pid, serial) = primary_target.map_or((0, 0), |t| (t.actor_id(), t.spawn_serial()));
        self.primary_actor_id.store(pid, Ordering::Relaxed);
        self.primary_spawn_serial.store(serial, Ordering::Relaxed);
        self.flags.store(flags, Ordering::Relaxed);

        if (flags & FLAG_HAS_EXTRA) != 0 {
            *self.extra_targets.lock_or_recover() = Some((reg_seq, extra_targets.into()));
        }

        self.seq.store(reg_seq, Ordering::Release);
        reg_seq
    }

    #[inline]
    pub(crate) fn disarm(&self) {
        // RMW `swap` forms a C11 synchronization edge with `prepare`'s `swap`,
        // ensuring any predicate release store sequenced before `disarm`
        // happens-before the predicate check sequenced after `prepare`.
        self.state.swap(STATE_DISARMED, Ordering::SeqCst);
    }

    #[inline]
    fn prepare(&self, reg_seq: u64) {
        if self.seq.load(Ordering::Acquire) != reg_seq {
            return;
        }
        self.state.swap(STATE_ARMED, Ordering::SeqCst);
        if self.seq.load(Ordering::Acquire) != reg_seq {
            self.state.swap(STATE_DISARMED, Ordering::SeqCst);
        }
    }

    #[inline]
    fn try_mark_pending(&self, reg_seq: u64) -> bool {
        if self.seq.load(Ordering::Acquire) != reg_seq {
            return false;
        }
        if self
            .state
            .compare_exchange(
                STATE_ARMED,
                STATE_PENDING,
                Ordering::SeqCst,
                Ordering::Acquire,
            )
            .is_err()
        {
            return false;
        }
        if self.seq.load(Ordering::Acquire) != reg_seq {
            self.state.swap(STATE_DISARMED, Ordering::SeqCst);
            return false;
        }
        true
    }

    #[inline]
    fn is_pending(&self) -> bool {
        self.state.load(Ordering::SeqCst) == STATE_PENDING
    }

    #[inline]
    fn is_pending_for(&self, reg_seq: u64) -> bool {
        self.seq.load(Ordering::Acquire) == reg_seq
            && self.state.load(Ordering::SeqCst) == STATE_PENDING
    }

    #[inline]
    fn clear_on_free(&self, reg_seq: u64, had_extra: bool) -> bool {
        if self.seq.load(Ordering::Acquire) != reg_seq {
            return false;
        }
        self.state.swap(STATE_DISARMED, Ordering::SeqCst);
        if had_extra {
            let mut extra = self.extra_targets.lock_or_recover();
            if extra.as_ref().is_some_and(|(s, _)| *s == reg_seq) {
                *extra = None;
            }
        }
        self.seq
            .compare_exchange(reg_seq, reg_seq + 2, Ordering::AcqRel, Ordering::Acquire)
            .is_ok()
    }

    fn snapshot_if_pending(&self) -> Option<PendingNode> {
        loop {
            let seq1 = self.seq.load(Ordering::Acquire);
            if seq1 == 0 || (seq1 & 1) != 0 {
                return None;
            }
            if self.state.load(Ordering::SeqCst) != STATE_PENDING {
                return None;
            }
            let flags = self.flags.load(Ordering::Relaxed);
            let pid = self.primary_actor_id.load(Ordering::Relaxed);
            let serial = self.primary_spawn_serial.load(Ordering::Relaxed);
            let extra_targets = if (flags & FLAG_HAS_EXTRA) != 0 {
                self.extra_targets
                    .lock_or_recover()
                    .as_ref()
                    .filter(|(s, _)| *s == seq1)
                    .map(|(_, targets)| targets.clone())
                    .unwrap_or_default()
            } else {
                Box::default()
            };
            fence(Ordering::Acquire);
            let seq2 = self.seq.load(Ordering::Relaxed);
            if seq1 == seq2 {
                if self.state.load(Ordering::SeqCst) != STATE_PENDING {
                    return None;
                }
                let primary_target = (pid != 0).then(|| ActorIncarnation::from_parts(pid, serial));
                let can_block = (flags & FLAG_CAN_BLOCK) != 0;
                return Some(PendingNode {
                    primary_target,
                    extra_targets,
                    can_block,
                });
            }
            std::hint::spin_loop();
        }
    }
}

/// Cold fallback registry used ONLY when an `ActorIncarnation` is not backed by
/// a tracked `HewActor` in `live_actors` (for example, standalone unit tests
/// constructing synthetic `ActorIncarnation::from_parts(900_001_001, 1)`).
/// `FALLBACK_COUNT` remains `0` across normal runtime execution so production
/// hot paths never lock `FALLBACK_WAITS`.
static FALLBACK_COUNT: AtomicUsize = AtomicUsize::new(0);
static FALLBACK_WAITS: LazyLock<Mutex<FxHashMap<ActorIncarnation, Arc<ActorWaitSlot>>>> =
    LazyLock::new(|| Mutex::new(FxHashMap::default()));

/// Serializes rare potential-cycle graph snapshots so two actors closing the
/// same knot simultaneously do not race diagnostic publication.
static CYCLE_CHECK_LOCK: Mutex<()> = Mutex::new(());

#[derive(Debug)]
pub struct HewActorWaitEdge {
    owner: ActorIncarnation,
    /// Inline first target avoids a heap `Vec` on single-target asks and waits.
    primary_target: Option<ActorIncarnation>,
    /// Additional targets for multi-arm `select` registrations.
    extra_targets: Box<[ActorIncarnation]>,
    external_progress: bool,
    operation: i32,
    reg_seq: u64,
    fallback_slot: Option<Arc<ActorWaitSlot>>,
    diagnostic: OnceLock<String>,
}

std::thread_local! {
    static WAIT_EDGE_POOL: crate::util::TlsBoxPool<HewActorWaitEdge> =
        const { crate::util::TlsBoxPool::new() };
}

impl HewActorWaitEdge {
    fn targets(&self) -> impl Iterator<Item = ActorIncarnation> + '_ {
        self.primary_target
            .into_iter()
            .chain(self.extra_targets.iter().copied())
    }

    fn can_block(&self) -> bool {
        !self.external_progress && self.primary_target.is_some()
    }
}

/// Run `f` against `owner`'s inline [`ActorWaitSlot`] using the current
/// thread's active dispatch context when it matches `owner` (zero registry
/// lookups), falling back to `live_actors::with_live_incarnation`.
#[inline]
fn with_actor_wait_slot<R>(
    owner: ActorIncarnation,
    f: impl FnOnce(&ActorWaitSlot) -> R,
) -> Option<R> {
    let current = crate::actor::current_actor_silent();
    if !current.is_null() {
        // SAFETY: `current_actor_silent()` returns a non-null pointer only
        // while the current stack frame's `HewExecutionContext` pins `current`.
        let current_incarnation = unsafe { ActorIncarnation::of(current) };
        if current_incarnation == owner {
            // SAFETY: `current` is pinned by the active execution context.
            return Some(f(unsafe { &*current }.active_wait_edge()));
        }
    }
    live_actors::with_live_incarnation(owner, |pin| f(pin.actor().active_wait_edge()))
}

#[cold]
fn install_fallback_slot(
    owner: ActorIncarnation,
    primary_target: Option<ActorIncarnation>,
    extra_targets: &[ActorIncarnation],
    external_progress: bool,
) -> (u64, Arc<ActorWaitSlot>) {
    let slot = {
        let mut map = FALLBACK_WAITS.lock_or_recover();
        match map.entry(owner) {
            std::collections::hash_map::Entry::Occupied(entry) => Arc::clone(entry.get()),
            std::collections::hash_map::Entry::Vacant(entry) => {
                let fresh = Arc::new(ActorWaitSlot::new());
                entry.insert(Arc::clone(&fresh));
                FALLBACK_COUNT.fetch_add(1, Ordering::Release);
                fresh
            }
        }
    };
    let reg_seq = slot.install(primary_target, extra_targets, external_progress);
    (reg_seq, slot)
}

#[cold]
fn release_fallback_slot(
    owner: ActorIncarnation,
    slot: &Arc<ActorWaitSlot>,
    reg_seq: u64,
    had_extra: bool,
) {
    if !slot.clear_on_free(reg_seq, had_extra) {
        return;
    }
    let mut map = FALLBACK_WAITS.lock_or_recover();
    // Re-check under the fallback lock so a concurrent replacement on the same
    // synthetic incarnation is not evicted.
    if slot.seq.load(Ordering::Acquire) == reg_seq + 2
        && map
            .get(&owner)
            .is_some_and(|stored| Arc::ptr_eq(stored, slot))
    {
        map.remove(&owner);
        FALLBACK_COUNT.fetch_sub(1, Ordering::AcqRel);
    }
}

#[cold]
fn fallback_ready(owner: ActorIncarnation) {
    let slot = FALLBACK_WAITS.lock_or_recover().get(&owner).cloned();
    if let Some(slot) = slot {
        slot.disarm();
    }
}

#[cold]
fn fallback_is_target_pending(target: ActorIncarnation) -> bool {
    let slot = FALLBACK_WAITS.lock_or_recover().get(&target).cloned();
    slot.is_some_and(|s| s.is_pending())
}

#[cold]
fn fallback_snapshot_if_pending(owner: ActorIncarnation) -> Option<PendingNode> {
    let slot = FALLBACK_WAITS.lock_or_recover().get(&owner).cloned()?;
    slot.snapshot_if_pending()
}

#[inline]
fn is_target_pending(target: ActorIncarnation) -> bool {
    if target.is_none() {
        return false;
    }
    if let Some(pending) = live_actors::with_live_incarnation(target, |pin| {
        pin.actor().active_wait_edge().is_pending()
    }) {
        return pending;
    }
    if FALLBACK_COUNT.load(Ordering::Acquire) != 0 {
        return fallback_is_target_pending(target);
    }
    false
}

fn snapshot_pending_actor(owner: ActorIncarnation) -> Option<PendingNode> {
    if owner.is_none() {
        return None;
    }
    if let Some(snapshot) = live_actors::with_live_incarnation(owner, |pin| {
        pin.actor().active_wait_edge().snapshot_if_pending()
    }) {
        return snapshot;
    }
    if FALLBACK_COUNT.load(Ordering::Acquire) != 0 {
        return fallback_snapshot_if_pending(owner);
    }
    None
}

impl HewActorWaitEdge {
    fn alloc_pooled(value: Self) -> *const Self {
        let mut slot = Some(value);
        WAIT_EDGE_POOL
            .try_with(|pool| pool.alloc(slot.take().expect("wait edge value present")))
            .unwrap_or_else(|_| {
                Box::into_raw(Box::new(slot.take().expect("wait edge value present")))
            })
            .cast_const()
    }

    fn register(owner: ActorIncarnation, target: ActorIncarnation, operation: i32) -> *const Self {
        let primary_target = Some(target);
        let (reg_seq, fallback_slot) = if let Some(seq) =
            with_actor_wait_slot(owner, |slot| slot.install(primary_target, &[], false))
        {
            (seq, None)
        } else {
            let (seq, slot) = install_fallback_slot(owner, primary_target, &[], false);
            (seq, Some(slot))
        };
        Self::alloc_pooled(Self {
            owner,
            primary_target,
            extra_targets: Box::default(),
            external_progress: false,
            operation,
            reg_seq,
            fallback_slot,
            diagnostic: OnceLock::new(),
        })
    }

    fn register_alternatives(
        owner: ActorIncarnation,
        targets: Vec<ActorIncarnation>,
        external_progress: bool,
        operation: i32,
    ) -> *const Self {
        let mut iter = targets.into_iter();
        let primary_target = iter.next();
        let extra_targets: Box<[ActorIncarnation]> = iter.collect();
        let (reg_seq, fallback_slot) = if let Some(seq) = with_actor_wait_slot(owner, |slot| {
            slot.install(primary_target, &extra_targets, external_progress)
        }) {
            (seq, None)
        } else {
            let (seq, slot) =
                install_fallback_slot(owner, primary_target, &extra_targets, external_progress);
            (seq, Some(slot))
        };
        Self::alloc_pooled(Self {
            owner,
            primary_target,
            extra_targets,
            external_progress,
            operation,
            reg_seq,
            fallback_slot,
            diagnostic: OnceLock::new(),
        })
    }

    #[inline]
    fn disarm_self(&self) {
        if let Some(slot) = &self.fallback_slot {
            if slot.seq.load(Ordering::Acquire) == self.reg_seq {
                slot.disarm();
            }
            return;
        }
        let _ = with_actor_wait_slot(self.owner, |slot| {
            if slot.seq.load(Ordering::Acquire) == self.reg_seq {
                slot.disarm();
            }
        });
    }

    #[inline]
    fn is_still_pending(&self) -> bool {
        if let Some(slot) = &self.fallback_slot {
            return slot.is_pending_for(self.reg_seq);
        }
        with_actor_wait_slot(self.owner, |slot| slot.is_pending_for(self.reg_seq)).unwrap_or(false)
    }
}

/// Invalidate before readiness makes the actor runnable. The operation arms
/// its candidate again before the next poll, then confirms only a pending poll.
#[inline]
pub(crate) fn ready(owner: ActorIncarnation) {
    if owner.is_none() {
        return;
    }
    if live_actors::with_live_incarnation(owner, |pin| {
        pin.actor().active_wait_edge().disarm();
    })
    .is_some()
    {
        return;
    }
    if FALLBACK_COUNT.load(Ordering::Acquire) != 0 {
        fallback_ready(owner);
    }
}

pub(crate) fn resolve_target(target: local_handles::HewLocalPidId) -> Option<ActorIncarnation> {
    local_handles::resolve_current_actor(target).and_then(|id| {
        live_actors::with_actor_send_by_id(id, |actor| {
            // SAFETY: this guard pins the exact destination during capture.
            unsafe { ActorIncarnation::of(actor) }
        })
    })
}

/// Select dependencies are alternatives. A timer or an independently driven
/// source is a possible escape, so that turn cannot prove a closed actor knot.
/// This registration replaces the owner's single strict dependency atomically.
pub(crate) fn select_alternatives(
    owner: ActorIncarnation,
    targets: Vec<ActorIncarnation>,
    external_progress: bool,
) -> *const HewActorWaitEdge {
    if owner.is_none() {
        return ptr::null();
    }
    HewActorWaitEdge::register_alternatives(owner, targets, external_progress, 3)
}

/// Create a candidate for an exact local actor dependency; null means the
/// invocation does not hold a proven actor turn or the destination has retired.
///
/// # Safety
/// `state` is the live invocation state for this operation.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_wait_edge_new(
    state: *const HewCoroState,
    target: local_handles::HewLocalPidId,
    operation: i32,
) -> *const HewActorWaitEdge {
    // SAFETY: the caller holds its invocation state throughout construction.
    let owner = unsafe { &*state }.execution().actor_turn;
    if owner.is_none() {
        return ptr::null();
    }
    let target = resolve_target(target);
    target.map_or(ptr::null(), |target| {
        HewActorWaitEdge::register(owner, target, operation)
    })
}

/// Register a dependency on the target incarnation already captured by a
/// local actor call during submission, avoiding a redundant handle and
/// live-actor registry lookup.
///
/// # Safety
/// `state` and `call` remain live through this registration.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_wait_edge_new_for_call(
    state: *const HewCoroState,
    call: *const crate::actor_call_native::HewActorCall,
    operation: i32,
) -> *const HewActorWaitEdge {
    // SAFETY: the caller retains both pointers for this call.
    let owner = unsafe { &*state }.execution().actor_turn;
    if owner.is_none() || call.is_null() {
        return ptr::null();
    }
    // SAFETY: the caller exclusively owns this live call operation.
    let Some(target) = unsafe { &*call }.target else {
        return ptr::null();
    };
    HewActorWaitEdge::register(owner, target, operation)
}

/// Register a dependency on the incarnation selected by a terminal observer.
/// A role's owner token is a supervisor, so resolving that token independently
/// cannot prove an actor wait cycle and can select a different child after a
/// restart.
///
/// # Safety
/// `state` and `wait` remain live through this registration.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_wait_edge_new_for_wait(
    state: *const HewCoroState,
    wait: *const crate::actor_native::HewNativeActorWait,
    operation: i32,
) -> *const HewActorWaitEdge {
    // SAFETY: the caller retains both pointers for this call.
    let owner = unsafe { &*state }.execution().actor_turn;
    // SAFETY: the same caller retains this wait until registration returns.
    let target = unsafe { &*wait }.target;
    if owner.is_none() || target.is_none() {
        return ptr::null();
    }
    HewActorWaitEdge::register(owner, target, operation)
}

/// Arm before checking the operation predicate, closing registration/wake races.
///
/// # Safety
/// The pointer is null or one live operation-owned edge.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_wait_edge_prepare(edge: *const HewActorWaitEdge) {
    // SAFETY: the caller retains this optional immutable edge.
    let Some(edge) = (unsafe { edge.as_ref() }) else {
        return;
    };
    if let Some(slot) = &edge.fallback_slot {
        slot.prepare(edge.reg_seq);
        return;
    }
    let _ = with_actor_wait_slot(edge.owner, |slot| slot.prepare(edge.reg_seq));
}

/// Confirm a pending predicate and return 1 only when it closes a local cycle.
/// Wakes and confirmations synchronize through the slot's `SeqCst` state so
/// neither can resurrect a wake that happened between predicate inspection and
/// this confirmation, and two actors closing a cycle concurrently cannot both
/// miss each other's pending state.
///
/// # Safety
/// The pointer is null or a live edge prepared before the last pending poll.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_wait_edge_pending(edge: *const HewActorWaitEdge) -> i32 {
    // SAFETY: the operation holds its edge for this confirmation.
    let Some(edge) = (unsafe { edge.as_ref() }) else {
        return 0;
    };
    let marked = if let Some(slot) = &edge.fallback_slot {
        slot.try_mark_pending(edge.reg_seq)
    } else {
        with_actor_wait_slot(edge.owner, |slot| slot.try_mark_pending(edge.reg_seq))
            .unwrap_or(false)
    };
    if !marked {
        return 0;
    }

    // Drain store buffers under RC11/Loom `SeqCst` semantics so two actors
    // concurrently transitioning `STATE_ARMED -> STATE_PENDING` on distinct
    // `ActorWaitSlot` atomics cannot both read `STATE_ARMED` from each other.
    fence(Ordering::SeqCst);

    // An independent or non-pending alternative rules out this edge before
    // the graph walk. Keep the edge pending so a later confirmation can close
    // a cycle through it.
    if !edge.can_block() || edge.targets().any(|target| !is_target_pending(target)) {
        return 0;
    }

    detect_cycle_slow(edge)
}

#[cold]
#[inline(never)]
fn detect_cycle_slow(edge: &HewActorWaitEdge) -> i32 {
    let _cycle_guard = CYCLE_CHECK_LOCK.lock_or_recover();
    if !edge.is_still_pending() {
        return 0;
    }

    // Walk only the subgraph reachable from `edge.owner` through `live_actors`
    // (or `FALLBACK_WAITS` in standalone unit tests), rather than scanning a
    // global table. A turn is proven blocked only when every alternative is
    // another proven blocked turn.
    let mut active: FxHashMap<ActorIncarnation, PendingNode> = FxHashMap::default();
    let mut queue: VecDeque<ActorIncarnation> = VecDeque::new();
    let mut enqueued: FxHashSet<ActorIncarnation> = FxHashSet::default();

    queue.push_back(edge.owner);
    enqueued.insert(edge.owner);

    while let Some(curr) = queue.pop_front() {
        let Some(node) = snapshot_pending_actor(curr) else {
            continue;
        };
        if !node.can_block {
            continue;
        }
        for target in node.targets() {
            if enqueued.insert(target) {
                queue.push_back(target);
            }
        }
        active.insert(curr, node);
    }

    let mut blocked: FxHashSet<ActorIncarnation> = active.keys().copied().collect();
    loop {
        let escaped: Vec<_> = blocked
            .iter()
            .filter(|owner| {
                active[owner]
                    .targets()
                    .any(|target| !blocked.contains(&target))
            })
            .copied()
            .collect();
        if escaped.is_empty() {
            break;
        }
        for owner in escaped {
            blocked.remove(&owner);
        }
    }
    if !blocked.contains(&edge.owner) {
        return 0;
    }

    let mut path = Vec::new();
    let mut next = edge.owner;
    let mut visited = FxHashSet::default();
    while visited.insert(next) {
        path.push(next.actor_id());
        next = active[&next].primary_target.expect("blocked edge target");
    }
    path.push(next.actor_id());
    let operation = match edge.operation {
        0 => "ask",
        1 => "mailbox wait",
        2 => "actor termination wait",
        _ => "select",
    };
    let path = path
        .iter()
        .map(u64::to_string)
        .collect::<Vec<_>>()
        .join(" -> ");
    let _ = edge
        .diagnostic
        .set(format!("local actor wait cycle at {operation}: {path}"));
    edge.disarm_self();
    1
}

/// Materialize the diagnosed cycle as an ordinary owned logical fault.
///
/// # Safety
/// `edge` is live and its pending confirmation reported a cycle.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_wait_edge_fault(
    edge: *const HewActorWaitEdge,
) -> *mut crate::fault::HewFault {
    // SAFETY: the operation retains the diagnosed edge until this copy returns.
    let edge = unsafe { &*edge };
    Box::into_raw(Box::new(crate::fault::HewFault::with_message(
        crate::internal::types::HEW_TRAP_USER_PANIC,
        edge.diagnostic
            .get()
            .expect("confirmed actor cycle")
            .clone(),
    )))
}

/// Release the registration before the enclosing invocation can finish.
///
/// # Safety
/// The pointer is null or the unique edge pointer returned at construction.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_wait_edge_free(edge: *const HewActorWaitEdge) {
    if edge.is_null() {
        return;
    }
    let raw = edge.cast_mut();
    {
        // SAFETY: this call consumes the operation's unique edge ownership; borrow
        // metadata only before `free` drops the value in place.
        let edge_ref = unsafe { &*raw };
        let had_extra = !edge_ref.extra_targets.is_empty();
        if let Some(slot) = &edge_ref.fallback_slot {
            release_fallback_slot(edge_ref.owner, slot, edge_ref.reg_seq, had_extra);
        } else {
            let _ = with_actor_wait_slot(edge_ref.owner, |slot| {
                slot.clear_on_free(edge_ref.reg_seq, had_extra);
            });
        }
    }
    if WAIT_EDGE_POOL
        .try_with(|pool| {
            // SAFETY: `raw` was allocated by `WAIT_EDGE_POOL` (or `Box::into_raw`)
            // and is no longer referenced anywhere else.
            unsafe { pool.free(raw) };
        })
        .is_err()
    {
        // SAFETY: fallback when thread-local storage is already tearing down.
        drop(unsafe { Box::from_raw(raw) });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn select_requires_every_alternative_to_close_the_actor_knot() {
        for (external_progress, escaping_actor) in [(false, false), (true, false), (false, true)] {
            let a = ActorIncarnation::from_parts(900_001_001, 1);
            let b = ActorIncarnation::from_parts(900_001_002, 2);
            let c = ActorIncarnation::from_parts(900_001_003, 3);
            let outside = ActorIncarnation::from_parts(900_001_004, 4);
            let select = select_alternatives(a, vec![b, c], external_progress);
            let first = HewActorWaitEdge::register(b, a, 0);
            let second = HewActorWaitEdge::register(c, if escaping_actor { outside } else { a }, 0);
            // SAFETY: the fixture exclusively drives each live registration.
            unsafe {
                hew_actor_wait_edge_prepare(first);
                assert_eq!(hew_actor_wait_edge_pending(first), 0);
                hew_actor_wait_edge_prepare(second);
                assert_eq!(hew_actor_wait_edge_pending(second), 0);
                hew_actor_wait_edge_prepare(select);
                let cycle = hew_actor_wait_edge_pending(select);
                assert_eq!(cycle != 0, !external_progress && !escaping_actor);
                if cycle != 0 {
                    let fault = hew_actor_wait_edge_fault(select);
                    crate::fault::hew_fault_drop(fault);
                }
                hew_actor_wait_edge_free(select);
                hew_actor_wait_edge_free(first);
                hew_actor_wait_edge_free(second);
            }
        }
    }

    #[test]
    fn readiness_breaks_old_dependencies_before_actor_resumption() {
        let a = ActorIncarnation::from_parts(900_000_001, 1);
        let b = ActorIncarnation::from_parts(900_000_002, 2);
        let first = HewActorWaitEdge::register(a, b, 0);
        let second = HewActorWaitEdge::register(b, a, 2);
        // SAFETY: each pointer is an exclusively driven, live registration.
        unsafe {
            hew_actor_wait_edge_prepare(first);
            assert_eq!(hew_actor_wait_edge_pending(first), 0);
            ready(a);
            hew_actor_wait_edge_prepare(second);
            assert_eq!(hew_actor_wait_edge_pending(second), 0);
            hew_actor_wait_edge_prepare(first);
            assert_eq!(hew_actor_wait_edge_pending(first), 1);
            let fault = hew_actor_wait_edge_fault(first);
            crate::fault::hew_fault_drop(fault);
            hew_actor_wait_edge_free(first);
            hew_actor_wait_edge_free(second);
            let replacement = ActorIncarnation::from_parts(a.actor_id(), 3);
            let first = HewActorWaitEdge::register(a, b, 1);
            let second = HewActorWaitEdge::register(b, replacement, 1);
            hew_actor_wait_edge_prepare(first);
            assert_eq!(hew_actor_wait_edge_pending(first), 0);
            hew_actor_wait_edge_prepare(second);
            assert_eq!(hew_actor_wait_edge_pending(second), 0);
            hew_actor_wait_edge_free(first);
            hew_actor_wait_edge_free(second);
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn tracked_actors_use_inline_actor_wait_slot_without_fallback() {
        let _guard = crate::runtime_test_guard();
        let _sched = crate::scheduler::NoWorkerSchedulerForTest::install();
        let actor_a = crate::test_actor::TrackedTestActor::install_parked();
        let actor_b = crate::test_actor::TrackedTestActor::install_parked();
        // SAFETY: both test actors are live and tracked in `_sched`.
        let (inc_a, inc_b) = unsafe {
            (
                ActorIncarnation::of(actor_a.ptr()),
                ActorIncarnation::of(actor_b.ptr()),
            )
        };
        let fallback_before = FALLBACK_COUNT.load(Ordering::Acquire);
        let edge_ab = HewActorWaitEdge::register(inc_a, inc_b, 0);
        let edge_ba = HewActorWaitEdge::register(inc_b, inc_a, 0);
        assert_eq!(
            FALLBACK_COUNT.load(Ordering::Acquire),
            fallback_before,
            "tracked actors must install directly on HewActor::active_wait_edge"
        );
        // SAFETY: each edge is uniquely owned by this test.
        unsafe {
            assert!((*edge_ab).fallback_slot.is_none());
            assert!((*edge_ba).fallback_slot.is_none());
            hew_actor_wait_edge_prepare(edge_ab);
            assert_eq!(hew_actor_wait_edge_pending(edge_ab), 0);
            ready(inc_a);
            hew_actor_wait_edge_prepare(edge_ba);
            assert_eq!(hew_actor_wait_edge_pending(edge_ba), 0);
            hew_actor_wait_edge_prepare(edge_ab);
            assert_eq!(hew_actor_wait_edge_pending(edge_ab), 1);
            let fault = hew_actor_wait_edge_fault(edge_ab);
            crate::fault::hew_fault_drop(fault);
            hew_actor_wait_edge_free(edge_ab);
            hew_actor_wait_edge_free(edge_ba);
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn loom_concurrent_two_actor_cycle_is_never_missed() {
        use loom::sync::atomic::{
            fence as loom_fence, AtomicU8 as LoomAtomicU8, Ordering as LoomOrdering,
        };
        use loom::sync::Arc as LoomArc;

        loom::model(|| {
            let state_a = LoomArc::new(LoomAtomicU8::new(STATE_ARMED));
            let state_b = LoomArc::new(LoomAtomicU8::new(STATE_ARMED));

            let a_for_b = LoomArc::clone(&state_a);
            let b_for_b = LoomArc::clone(&state_b);
            let tb = loom::thread::spawn(move || {
                b_for_b
                    .compare_exchange(
                        STATE_ARMED,
                        STATE_PENDING,
                        LoomOrdering::SeqCst,
                        LoomOrdering::Acquire,
                    )
                    .expect("B transitions armed -> pending");
                loom_fence(LoomOrdering::SeqCst);
                a_for_b.load(LoomOrdering::SeqCst) == STATE_PENDING
            });

            state_a
                .compare_exchange(
                    STATE_ARMED,
                    STATE_PENDING,
                    LoomOrdering::SeqCst,
                    LoomOrdering::Acquire,
                )
                .expect("A transitions armed -> pending");
            loom_fence(LoomOrdering::SeqCst);
            let a_saw_b = state_b.load(LoomOrdering::SeqCst) == STATE_PENDING;
            let b_saw_a = tb.join().expect("thread B");

            assert!(
                a_saw_b || b_saw_a,
                "concurrent 2-cycle must be observed by at least one participant"
            );
        });
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn loom_wake_disarm_never_leaves_satisfied_wait_pending() {
        use loom::sync::atomic::{
            AtomicBool as LoomAtomicBool, AtomicU8 as LoomAtomicU8, Ordering as LoomOrdering,
        };
        use loom::sync::Arc as LoomArc;

        loom::model(|| {
            let predicate_ready = LoomArc::new(LoomAtomicBool::new(false));
            let slot_state = LoomArc::new(LoomAtomicU8::new(STATE_DISARMED));

            let pred_waker = LoomArc::clone(&predicate_ready);
            let state_waker = LoomArc::clone(&slot_state);
            let waker = loom::thread::spawn(move || {
                pred_waker.store(true, LoomOrdering::Release);
                state_waker.swap(STATE_DISARMED, LoomOrdering::SeqCst);
            });

            // Poller protocol: prepare -> check predicate -> confirm pending.
            slot_state.swap(STATE_ARMED, LoomOrdering::SeqCst);
            let became_pending = if predicate_ready.load(LoomOrdering::Acquire) {
                false
            } else {
                slot_state
                    .compare_exchange(
                        STATE_ARMED,
                        STATE_PENDING,
                        LoomOrdering::SeqCst,
                        LoomOrdering::Acquire,
                    )
                    .is_ok()
            };

            waker.join().expect("waker thread");
            assert!(
                !became_pending || slot_state.load(LoomOrdering::SeqCst) == STATE_DISARMED,
                "after waker completes, slot must never remain stuck in STATE_PENDING"
            );
        });
    }
}
