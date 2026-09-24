//! Supervisor pool management and native (non-Hew) child spawning.
#[allow(
    clippy::wildcard_imports,
    reason = "submodule split from a single large file; re-exports its parent scope"
)]
use super::*;

/// Register a new pool slot on the supervisor.
///
/// Allocates a fresh `HewActorPool` and appends it to `pool_slots`/`pool_specs`,
/// returning the pool slot index (≥ 0) on success, or -1 on error.
///
/// The checker assigns pool slot indices in source-declaration order, matching
/// the order this function is called during supervisor construction. The returned
/// index is the `pool_key` parameter for [`hew_supervisor_pool_child_get`] and
/// sibling pool ABI functions.
///
/// # Safety
///
/// - `sup` must be a valid pointer returned by [`hew_supervisor_new`].
/// - `name` must be a valid, null-terminated C string pointer; it is copied
///   internally and the caller retains ownership.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_pool_add_slot(
    sup: *mut HewSupervisor,
    name: *const c_char,
    strategy: c_int,
    max_members: usize,
) -> c_int {
    cabi_guard!(sup.is_null(), -1);

    let pool_strategy = match strategy {
        1 => PoolStrategy::Random,
        _ => PoolStrategy::RoundRobin,
    };

    // SAFETY: name is guaranteed valid by caller; hew_pool_new copies nothing —
    // we strdup separately so InternalPoolSpec owns the allocation.
    let name_copy: *mut c_char = if name.is_null() {
        ptr::null_mut()
    } else {
        // SAFETY: caller guarantees name is a valid C string.
        // Portable strdup (libc::strdup unavailable on Windows-MSVC, #2505).
        unsafe { crate::cabi::cstr_strdup(name) }
    };

    // Allocate the pool. hew_pool_new takes a *const c_char that must stay
    // valid for the pool's lifetime; we pass name_copy which is owned by the
    // parallel InternalPoolSpec and freed in InternalPoolSpec::drop after the
    // pool itself is freed.
    // SAFETY: name_copy is valid (non-null checked below); if null, we pass null.
    let pool = unsafe { crate::pool::hew_pool_new(name_copy, pool_strategy as c_int) };
    if pool.is_null() {
        // Free the duplicated name on allocation failure.
        if !name_copy.is_null() {
            // SAFETY: name_copy was allocated with libc::strdup.
            unsafe { crate::mem::buf_free(name_copy.cast::<c_void>()) }; // ALLOCATOR-PAIRING: GlobalAlloc
        }
        return -1;
    }

    // SAFETY: caller keeps `sup` live; pool metadata shares the roster lock so
    // static membership cannot race dynamic child removal.
    let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard serializes this scoped mutable pool-roster access.
    let s = &mut *guard;
    #[expect(
        clippy::cast_possible_truncation,
        clippy::cast_possible_wrap,
        reason = "pool slot count fits in c_int for any realistic supervisor"
    )]
    let index = s.pool_slots.len() as c_int;
    s.pool_slots.push(pool);
    s.pool_specs.push(InternalPoolSpec {
        name: name_copy,
        strategy: pool_strategy,
        max_members,
        static_members: Vec::new(),
    });

    index
}

/// Add an actor PID to an existing pool slot.
///
/// `pool_key` is the index returned by [`hew_supervisor_pool_add_slot`].
///
/// Returns 0 on success, -1 if `sup` is null, `pool_key` is out of range, or
/// the pool's `max_members` limit would be exceeded.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_pool_member_add(
    sup: *mut HewSupervisor,
    pool_key: u32,
    actor_pid: u64,
) -> c_int {
    cabi_guard!(sup.is_null(), -1);
    // SAFETY: caller keeps `sup` live through this membership update.
    let guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard protects the pool roster and selected pool lifetime.
    let s = &*guard;
    let i = pool_key as usize;
    if i >= s.pool_slots.len() {
        set_last_error("hew_supervisor_pool_member_add: pool_key out of range");
        return -1;
    }
    let pool = s.pool_slots[i];
    if pool.is_null() {
        set_last_error("hew_supervisor_pool_member_add: pool slot is null");
        return -1;
    }
    // Enforce max_members if configured.
    let max = s.pool_specs[i].max_members;
    if max > 0 {
        // SAFETY: pool is valid.
        let current = unsafe { crate::pool::hew_pool_size(pool) };
        if current >= max {
            set_last_error("hew_supervisor_pool_member_add: pool at max_members capacity");
            return -1;
        }
    }
    // SAFETY: pool is valid.
    unsafe { crate::pool::hew_pool_add(pool, actor_pid) }
}

/// Register a STATIC-backed pool member: a pool member whose actor lives in the
/// supervisor's `children[]` table at `static_idx`.
///
/// A static pool (`pool name: Type count: N`) spawns its N members as ordinary
/// static children, then records each member's static-child index here (in
/// member order). The accessor [`hew_supervisor_pool_child_get`] resolves member
/// `i` through the LIVE static slot `children[static_idx]`, so a restarted member
/// is re-resolved automatically — the restart machinery re-fills the static slot
/// and the pool view picks up the fresh actor with no stale PID cached.
///
/// Returns 0 on success; -1 if `sup` is null, `pool_key` is out of range, the
/// pool slot is null, `static_idx` is out of range, or the pool's `max_members`
/// limit would be exceeded.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_pool_member_add_static(
    sup: *mut HewSupervisor,
    pool_key: u32,
    static_idx: u32,
) -> c_int {
    cabi_guard!(sup.is_null(), -1);
    // SAFETY: caller keeps `sup` live through this static membership update.
    let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard serializes child bounds and pool membership together.
    let s = &mut *guard;
    let i = pool_key as usize;
    if i >= s.pool_slots.len() {
        set_last_error("hew_supervisor_pool_member_add_static: pool_key out of range");
        return -1;
    }
    if s.pool_slots[i].is_null() {
        set_last_error("hew_supervisor_pool_member_add_static: pool slot is null");
        return -1;
    }
    let static_idx = static_idx as usize;
    if static_idx >= s.child_count {
        set_last_error("hew_supervisor_pool_member_add_static: static_idx out of range");
        return -1;
    }
    // Enforce max_members if configured (counts current static members).
    let max = s.pool_specs[i].max_members;
    if max > 0 && s.pool_specs[i].static_members.len() >= max {
        set_last_error("hew_supervisor_pool_member_add_static: pool at max_members capacity");
        return -1;
    }
    s.pool_specs[i].static_members.push(static_idx);
    0
}

/// Remove an actor PID from a pool slot.
///
/// Returns 0 on success, -1 if the pool or PID is not found.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_pool_member_remove(
    sup: *mut HewSupervisor,
    pool_key: u32,
    actor_pid: u64,
) -> c_int {
    cabi_guard!(sup.is_null(), -1);
    // SAFETY: caller keeps `sup` live through this membership update.
    let guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard protects the pool roster and selected pool lifetime.
    let s = &*guard;
    let i = pool_key as usize;
    if i >= s.pool_slots.len() {
        set_last_error("hew_supervisor_pool_member_remove: pool_key out of range");
        return -1;
    }
    let pool = s.pool_slots[i];
    if pool.is_null() {
        return -1;
    }
    // SAFETY: pool is valid.
    unsafe { crate::pool::hew_pool_remove(pool, actor_pid) }
}

/// Resolve a pool member by its position index within the pool's current
/// membership snapshot.
///
/// Returns a [`ChildLookupResult`] discriminated as:
///
/// - `Live(handle)` — `handle` carries the actor PID (u64) of the indexed
///   member, encoded as a pointer-width integer. Callers route messages via
///   the PID rather than dereferencing the value as a raw pointer. Use
///   `hew_pid_resolve` (when available) to obtain the `*mut HewActor` for
///   direct dispatch.
/// - `Dead(UnknownSlot)` — `pool_key` is out of range, the pool slot is null,
///   the supervisor is shut down, or `index` is beyond the current member
///   count (which may have shrunk due to dynamic pool member removal).
///
/// The index is **unstable**: pool members are stored as an unordered set
/// internally. A member removed between two calls (via
/// [`hew_supervisor_pool_member_remove`]) may shift other members' indices
/// because the pool uses `swap_remove`. Do not cache an index across removals.
///
/// `index` is `u64` (not `u32`) specifically so this function can
/// bounds-check the caller's *original, untruncated* index. A pool's real
/// member count always fits comfortably in a `u32`, so any `index` that does
/// not itself fit in `usize`/`u32` range is unconditionally out of bounds and
/// is rejected as `Dead(UnknownSlot)` before any narrowing cast — matching
/// `Vec[i]` OOB-parity semantics for arbitrarily large indices instead of
/// silently wrapping (see hew-lang/hew#2244).
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_pool_child_get(
    sup: *mut HewSupervisor,
    pool_key: u32,
    index: u64,
) -> ChildLookupResult {
    if sup.is_null() {
        return ChildLookupResult::dead(ChildSlotReason::SupervisorShutdown);
    }
    // Fast-path: supervisor-level shutdown.
    // SAFETY: caller keeps `sup` live for these atomic reads.
    if unsafe {
        (*sup).cancelled.load(Ordering::Acquire) || (*sup).running.load(Ordering::Acquire) == 0
    } {
        return ChildLookupResult::dead(ChildSlotReason::SupervisorShutdown);
    }

    // `index` arrives untruncated here (u64). No real pool ever has anywhere
    // near `usize::MAX` members, so any index that overflows `usize` (only
    // possible on a 32-bit `usize` target) is unconditionally OOB. On the
    // universally-supported 64-bit targets this `try_into` is infallible, but
    // keeping the target-generic checked conversion (rather than a bare `as`)
    // is what actually closes the truncation-and-wraparound gap this function
    // exists to prevent (hew-lang/hew#2244) -- an `as usize` here would just
    // move the same silent-wraparound bug from `i64->u32` (codegen) to
    // `u64->usize` (here) on a 32-bit target instead of removing it.
    let Ok(index) = usize::try_from(index) else {
        return ChildLookupResult::dead(ChildSlotReason::UnknownSlot);
    };

    let i = pool_key as usize;
    // SAFETY: caller keeps `sup` live through this pool/child roster lookup.
    let roster_guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard protects this scoped pool/child roster access.
    let s = &*roster_guard;
    if i >= s.pool_slots.len() {
        return ChildLookupResult::dead(ChildSlotReason::UnknownSlot);
    }

    let pool = s.pool_slots[i];
    if pool.is_null() {
        return ChildLookupResult::dead(ChildSlotReason::UnknownSlot);
    }

    // ── Static-backed pool path (`pool name: Type count: N`) ────────────────
    //
    // A static pool resolves member `i` through the LIVE static child slot, NOT
    // a cached PID. This is the restart re-resolution contract: the restart
    // machinery re-fills `children[static_idx]` with a FRESH actor on every
    // crash, and reading the slot here picks that up automatically — no stale
    // PID is ever returned, no pointer is cached across restart (LESSONS
    // `replaceable-resource-handle-is-fungible-reference`). The member's
    // liveness, circuit-breaker, and backoff classification reuse the static
    // child resolver verbatim.
    let static_members = &s.pool_specs[i].static_members;
    if !static_members.is_empty() {
        let Some(&static_idx) = static_members.get(index) else {
            // Index beyond the member count → out of bounds (Vec[i] OOB parity).
            return ChildLookupResult::dead(ChildSlotReason::UnknownSlot);
        };
        // Resolve through the static slot resolver, which holds `roster`
        // and reads the (pointer, CB-state, backoff) triple coherently.
        #[expect(
            clippy::cast_possible_truncation,
            reason = "static child count is always small; u32 is the ABI key type"
        )]
        let key = static_idx as u32;
        drop(roster_guard);
        // SAFETY: sup is valid (checked above); the resolver re-checks bounds.
        return unsafe { hew_supervisor_child_get(sup, key) };
    }

    // ── Dynamic PID-set pool path ────────────────────────────────────────────
    // Resolve the index within the pool's member list via the public ABI so
    // we stay within the module's encapsulation boundary.
    // SAFETY: pool was created by hew_pool_new and has not been freed.
    let pid = unsafe { crate::pool::hew_pool_get_at(pool, index) };
    if pid == 0 {
        // 0 is returned both for out-of-range and for an empty pool;
        // both cases are dead from the caller's perspective.
        return ChildLookupResult::dead(ChildSlotReason::UnknownSlot);
    }

    // Encode the PID as a *mut HewActor. Callers must treat this as an
    // opaque u64 PID — not a dereferenceable pointer — until hew_pid_resolve
    // is available to translate it.
    #[expect(
        clippy::cast_possible_truncation,
        reason = "u64 PID encoded as pointer-width integer; caller interprets as u64"
    )]
    let handle = pid as usize as *mut HewActor;
    ChildLookupResult::live(handle)
}

/// Resolve a static-pool member index to its stable child slot descriptor.
///
/// This lookup deliberately ignores the current child incarnation and slot
/// liveness. A valid pool position remains a valid `ChildRef<T>` while its
/// member is restarting or permanently stopped; the later role send/ask is the
/// sole liveness authority and fails closed there. Dynamic PID-set pools have
/// no stable supervisor child slot and are rejected.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub extern "C" fn hew_local_pid_supervisor_pool_child_ref_get(
    token: crate::lifetime::local_handles::HewLocalPidId,
    pool_key: u32,
    index: u64,
) -> ChildLookupResult {
    let Some(pin) = crate::lifetime::local_handles::pin_current_supervisor(token) else {
        return ChildLookupResult::dead(ChildSlotReason::SupervisorShutdown);
    };
    let Ok(index) = usize::try_from(index) else {
        return ChildLookupResult::dead(ChildSlotReason::UnknownSlot);
    };
    let sup = pin.supervisor();
    // SAFETY: the stable-identity pin keeps the supervisor allocation live.
    let roster = unsafe { &(*sup).roster }.lock_or_recover();
    let Ok(pool_index) = usize::try_from(pool_key) else {
        return ChildLookupResult::dead(ChildSlotReason::UnknownSlot);
    };
    let Some(pool_spec) = roster.pool_specs.get(pool_index) else {
        return ChildLookupResult::dead(ChildSlotReason::UnknownSlot);
    };
    let Some(&static_slot) = pool_spec.static_members.get(index) else {
        return ChildLookupResult::dead(ChildSlotReason::UnknownSlot);
    };
    // The descriptor word is an integer, not a dereferenceable actor pointer.
    let descriptor = std::ptr::with_exposed_provenance_mut::<HewActor>(static_slot);
    ChildLookupResult::live(descriptor)
}

#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn hew_local_pid_supervisor_pool_child_ref_get(
    _token: crate::lifetime::local_handles::HewLocalPidId,
    _pool_key: u32,
    _index: u64,
) -> ChildLookupResult {
    ChildLookupResult::dead(ChildSlotReason::SupervisorShutdown)
}

/// Return the number of live members in a pool slot.
///
/// Returns -1 if `sup` is null or `pool_key` is out of range.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
#[no_mangle]
#[expect(
    clippy::cast_possible_wrap,
    reason = "member count fits in i64 for any realistic pool"
)]
pub unsafe extern "C" fn hew_supervisor_pool_len(sup: *mut HewSupervisor, pool_key: u32) -> i64 {
    if sup.is_null() {
        return -1;
    }
    // SAFETY: caller keeps `sup` live through this pool-roster read.
    let guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard protects this scoped pool metadata access.
    let s = &*guard;
    let i = pool_key as usize;
    if i >= s.pool_slots.len() {
        return -1;
    }
    let pool = s.pool_slots[i];
    if pool.is_null() {
        return -1;
    }
    // A static-backed pool's size is its fixed static-member count (it never
    // shrinks; restart re-fills slots in place). A dynamic PID-set pool's size
    // is the live PID count.
    let static_members = &s.pool_specs[i].static_members;
    if !static_members.is_empty() {
        return static_members.len() as i64;
    }
    // SAFETY: pool is valid.
    unsafe { crate::pool::hew_pool_size(pool) as i64 }
}

// ---------------------------------------------------------------------------
// Native declared-supervisor ABI
// ---------------------------------------------------------------------------
//
// A declared supervisor is addressed only by its stable `LocalPid` token; no
// supervisor pointer ever becomes a program value. Construction is
// `new` → `add_child` (once per declared child, in declaration order) →
// `start`. Lookup slots are indexed within each child kind; restart identities
// preserve declaration order across both kinds.

/// [`HewNativeChildSpec::role_kind`] for a declared actor child.
pub const ROLE_KIND_ACTOR: c_int = 0;
/// [`HewNativeChildSpec::role_kind`] for a declared supervisor child.
pub const ROLE_KIND_SUPERVISOR: c_int = 1;

/// One declared child in construction order.
#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct HewNativeChildSpec {
    /// [`RESTART_PERMANENT`], [`RESTART_TRANSIENT`] or [`RESTART_TEMPORARY`].
    pub restart_policy: c_int,
    /// [`ROLE_KIND_ACTOR`] or [`ROLE_KIND_SUPERVISOR`].
    pub role_kind: c_int,
    /// The adapter that produces one incarnation from the config.
    pub spawn: HewNativeChildSpawnFn,
    /// The declared child name as a NUL-terminated string, or null. It labels
    /// the child in the profiler's restart series and tree dump.
    pub name: *const c_char,
}

fn register_native_child(s: &mut SupervisorRoster, child: &HewNativeChildSpec) -> Option<usize> {
    // ROSTER-GUARDED-HELPER: construction holds the owning supervisor mutex
    // throughout registration; this helper invokes no callbacks or waits.
    let invalid = crate::lifetime::local_handles::HewLocalPidId::INVALID;
    let next_identity = s.next_child_spec_identity.checked_add(1)?;
    let index = if child.role_kind == ROLE_KIND_SUPERVISOR {
        let index = s.child_supervisors.len();
        s.child_supervisors.push(ptr::null_mut());
        s.child_supervisor_tokens.push(invalid);
        s.child_supervisor_specs.push(Some(SupervisorChildSpec {
            spawn: SupervisorChildSpawn::Native {
                spawn: child.spawn,
                config: s.config_buf.cast_const(),
            },
            identity: s.next_child_spec_identity,
            restart_policy: child.restart_policy,
            spent: false,
        }));
        index
    } else {
        let index = s.children.len();
        let mut spec = InternalChildSpec::default();
        spec.identity = s.next_child_spec_identity;
        spec.restart_policy = child.restart_policy;
        spec.native_spawn = Some(child.spawn);
        spec.config = s.config_buf;
        if !child.name.is_null() {
            // SAFETY: the generated table stores each name as a C string literal.
            spec.name = unsafe { crate::cabi::cstr_strdup(child.name) };
        }
        s.child_specs.push(spec);
        s.children.push(ptr::null_mut());
        s.child_count += 1;
        index
    };
    s.next_child_spec_identity = next_identity;
    Some(index)
}

/// Construct a declared supervisor: adopt its config, spawn every declared
/// child in order and start supervising.
///
/// A child that cannot be spawned fails the whole construction — a supervisor
/// with a slot nothing can fill would report every send as "restarting"
/// forever. On failure the supervisor and every child already spawned are torn
/// down, the config is released through `config_drop`, and `INVALID` is
/// returned with `fault` set.
///
/// # Safety
///
/// `config` is a unique allocation `config_drop` can release, or null.
/// `children` is readable for `child_count` entries and `fault` is a writable,
/// initially null fault slot.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_native_spawn(
    strategy: c_int,
    max_restarts: c_int,
    window_secs: c_int,
    config: *mut c_void,
    config_drop: Option<unsafe extern "C" fn(*mut c_void)>,
    children: *const HewNativeChildSpec,
    child_count: usize,
    fault: *mut *mut crate::fault::HewFault,
) -> crate::lifetime::local_handles::HewLocalPidId {
    let invalid = crate::lifetime::local_handles::HewLocalPidId::INVALID;
    let refuse = |code| {
        // SAFETY: the generated caller provides a writable empty fault slot.
        unsafe { *fault = crate::fault::hew_fault_new(code) };
        invalid
    };
    // SAFETY: construction has no preconditions.
    let sup = unsafe { hew_supervisor_new(strategy, max_restarts, window_secs) };
    if sup.is_null() {
        if let Some(drop_fn) = config_drop {
            // SAFETY: nothing adopted the config, so this call still owns it.
            unsafe { drop_fn(config) };
        }
        if !config.is_null() {
            // SAFETY: the buffer came from the generated caller's allocator.
            unsafe { crate::mem::buf_free(config) }; // ALLOCATOR-PAIRING: GlobalAlloc
        }
        return refuse(crate::internal::types::HewError::ErrOom as i32);
    }
    {
        // SAFETY: the fresh allocation is not yet reachable by another thread.
        let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
        guard.config_buf = config;
        guard.config_drop_fn = config_drop;
    }
    // SAFETY: the caller supplies `child_count` readable child specs.
    let children = unsafe { std::slice::from_raw_parts(children, child_count) };
    for child in children {
        let index = {
            // SAFETY: construction owns the parent until start publishes it.
            let mut roster = unsafe { &(*sup).roster }.lock_or_recover();
            register_native_child(&mut roster, child)
        };
        let Some(index) = index else {
            // SAFETY: failed registration has not transferred another child.
            unsafe { hew_supervisor_stop(sup) };
            return refuse(crate::internal::types::HewError::ErrOom as i32);
        };
        // The initial spawn runs the adapter directly so its refusal reaches
        // this caller instead of the restart path's discard.
        // SAFETY: the caller supplies a writable, initially null fault slot.
        let token = unsafe { (child.spawn)(config.cast_const(), fault) };
        if child.role_kind == ROLE_KIND_SUPERVISOR {
            if let Some(pin) = crate::lifetime::local_handles::pin_current_supervisor(token) {
                let nested = pin.supervisor();
                // SAFETY: the adapter transferred this fresh subtree and the pin
                // protects it while its parent ownership edge is installed.
                unsafe {
                    (*nested).parent = sup;
                    (*nested).index_in_parent = index;
                    crate::shutdown::hew_shutdown_unregister_supervisor(nested);
                }
                // SAFETY: construction retains the parent allocation.
                let mut roster = unsafe { &(*sup).roster }.lock_or_recover();
                roster.child_supervisors[index] = nested;
                roster.child_supervisor_tokens[index] = token;
                continue;
            }
        }
        let actor = if token == invalid {
            ptr::null_mut()
        } else {
            crate::lifetime::local_handles::resolve_current_actor(token)
                .and_then(crate::lifetime::live_actors::get_actor_ptr_by_id)
                .unwrap_or(ptr::null_mut())
        };
        if actor.is_null() {
            // SAFETY: nothing published this supervisor's handle yet, so this
            // teardown is unobservable; it releases the config and the
            // children that did spawn.
            unsafe { hew_supervisor_stop(sup) };
            // SAFETY: the adapter already named the refusal unless it never
            // reached one, in which case the slot is still the caller's null.
            if unsafe { (*fault).is_null() } {
                return refuse(crate::internal::types::HewError::ErrOom as i32);
            }
            return invalid;
        }
        // SAFETY: the adapter returned a live, published actor and this thread
        // is the only writer of its supervision edge.
        unsafe {
            (*actor).supervisor = sup.cast::<c_void>();
            #[expect(
                clippy::cast_possible_truncation,
                clippy::cast_possible_wrap,
                reason = "declared child index fits i32 for any declarable supervisor"
            )]
            {
                (*actor).supervisor_child_index = index as i32;
            }
        }
        {
            // SAFETY: construction still owns the roster.
            let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
            guard.children[index] = actor;
        }
    }
    // SAFETY: every declared child occupies its slot.
    if unsafe { hew_supervisor_start(sup) } != 0 {
        // SAFETY: start failed before publication; teardown owns the rest.
        unsafe { hew_supervisor_stop(sup) };
        return refuse(crate::internal::types::HewError::ErrOom as i32);
    }
    // SAFETY: `hew_supervisor_new` published this allocation's token.
    unsafe { (*sup).local_pid_id }
}

/// Resolve one declared child's current incarnation, reporting what the role
/// holds in `out_tag`: `0` live, `1` restarting, `2` spent. `INVALID` is
/// returned for anything but a live occupant.
///
/// # Safety
///
/// `out_tag` is writable or null.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_native_child(
    token: crate::lifetime::local_handles::HewLocalPidId,
    slot: u32,
    out_tag: *mut c_int,
) -> usize {
    let result = hew_local_pid_supervisor_child_get(token, slot);
    if !out_tag.is_null() {
        // SAFETY: the caller supplies a writable occupancy slot.
        unsafe { out_tag.write(c_int::from(result.tag)) };
    }
    if result.tag == 0 {
        result.handle as usize
    } else {
        crate::lifetime::local_handles::HewLocalPidId::INVALID.as_usize()
    }
}

/// Block the calling thread until one declared child is Live again or is
/// permanently gone. The contextless restart barrier for `main`.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub extern "C" fn hew_supervisor_native_await_restart(
    token: crate::lifetime::local_handles::HewLocalPidId,
    slot: u32,
    role_kind: c_int,
) {
    let Some(pin) = crate::lifetime::local_handles::pin_current_supervisor(token) else {
        return;
    };
    // SAFETY: the pin keeps the allocation live for the blocking wait.
    unsafe {
        supervisor_restart_await_blocking(
            pin.supervisor(),
            slot,
            role_kind == ROLE_KIND_SUPERVISOR,
        );
    }
}

/// Retain a stable nested owner path for another declared child projection.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub extern "C" fn hew_supervisor_native_role_owner(
    owner: crate::lifetime::local_handles::HewLocalPidId,
    slot: u32,
) -> crate::lifetime::local_handles::HewLocalPidId {
    crate::lifetime::local_handles::current_supervisor_role_owner(owner, slot)
}

/// Observe the incarnation occupying a nested role at this call's resolution.
/// A closing observer retains completion before requesting cooperative stop.
///
/// # Safety
/// `waker` describes a live wake target for registration.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_native_role_wait_new(
    owner: crate::lifetime::local_handles::HewLocalPidId,
    slot: u32,
    waker: *const crate::wake::HewWaker,
    closing: c_int,
) -> *mut crate::actor_native::HewNativeActorWait {
    let owner_pin = crate::lifetime::local_handles::pin_current_supervisor(owner);
    let target = owner_pin
        .as_ref()
        .and_then(|pin| nested_child_token(pin.supervisor(), slot))
        .unwrap_or(crate::lifetime::local_handles::HewLocalPidId::INVALID);
    // SAFETY: registration clones completion and the caller supplied the waker.
    let wait = unsafe { crate::actor_native::hew_actor_wait_new(target, waker) };
    if closing != 0 {
        if let (Some(owner_pin), Some(child_pin)) = (
            owner_pin.as_ref(),
            crate::lifetime::local_handles::pin_current_supervisor(target),
        ) {
            child_pin
                .control()
                .notify_parent_on_stop(owner_pin.control().direct_id(), slot);
        }
        // Teardown must never drain a pin retained by its own caller.
        drop(owner_pin);
        stop_local_supervisor(target, true);
    }
    wait
}

/// Copy a nested incarnation identity while the caller pins its owner.
pub(crate) fn nested_child_token(
    supervisor: *mut HewSupervisor,
    slot: u32,
) -> Option<crate::lifetime::local_handles::HewLocalPidId> {
    // SAFETY: the caller retains an owner pin across these projected reads.
    if unsafe {
        (*supervisor).cancelled.load(Ordering::Acquire)
            || (*supervisor).running.load(Ordering::Acquire) == 0
    } {
        return None;
    }
    // SAFETY: the owner pin retains the roster's allocation through the lock.
    let roster = unsafe { &(*supervisor).roster }.lock_or_recover();
    let index = slot as usize;
    if roster.child_supervisors.get(index)?.is_null() {
        return None;
    }
    roster.child_supervisor_tokens.get(index).copied()
}

#[cfg(test)]
#[cfg(not(target_arch = "wasm32"))]
#[allow(
    unused_unsafe,
    reason = "test-owned raw supervisors often group several unsafe operations"
)]
#[expect(
    clippy::undocumented_unsafe_blocks,
    reason = "pool slot unit tests — safety invariants are documented per-test in comments"
)]
#[cfg(all(test, not(target_arch = "wasm32")))]
mod tests {
    //! Unit tests for the pool slot substrate.
    //!
    //! Tests use a stopped supervisor (running == 0) deliberately: pool slot
    //! lookup checks supervisor shutdown state and returns
    //! `Dead(SupervisorShutdown)` when `running == 0`. Tests that need `Live`
    //! results call `hew_supervisor_start` first or set `running` directly
    //! via the returned raw pointer.

    use std::ffi::c_void;
    use std::ptr;
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::{Arc, Barrier};

    use crate::exit_status::{FaultRecord, FaultRuling};

    macro_rules! locked_roster {
        ($sup:expr) => {{
            unsafe { &(*$sup).roster }
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
        }};
    }

    use super::{
        actor, install_dynamic_child_reserved_hook_for_test, restart_child_from_spec,
        restart_with_budget_and_strategy, supervisor_sys_dispatch_impl, take_child_slot,
        ChildEvent, ChildSlotReason, HewChildInitResult, HewChildSpec, HewSupervisor,
        OVERFLOW_DROP_NEW, RESTART_PERMANENT, RESTART_TEMPORARY,
    };
    use crate::internal::types::HewActorState;
    use crate::mailbox_header::HewSysMsg;
    use crate::supervisor::{
        hew_supervisor_add_child_dynamic, hew_supervisor_add_child_spec, hew_supervisor_new,
        hew_supervisor_pool_add_slot, hew_supervisor_pool_child_get, hew_supervisor_pool_len,
        hew_supervisor_pool_member_add, hew_supervisor_pool_member_add_static,
        hew_supervisor_pool_member_remove, hew_supervisor_remove_child,
        hew_supervisor_set_child_state_drop, hew_supervisor_start, hew_supervisor_stop,
    };
    use crate::util::MutexExt;

    const STRATEGY_ONE_FOR_ONE: std::ffi::c_int = 0;
    const STRATEGY_SIMPLE_ONE_FOR_ONE: std::ffi::c_int = 3;

    /// No-op child dispatch for the init-closure tests (mirrors the main test
    /// module's helper; defined locally to keep `pool_slot_tests` self-contained).
    unsafe extern "C-unwind" fn noop_child_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        std::ptr::null_mut()
    }
    const ROUND_ROBIN: std::ffi::c_int = 0;

    unsafe fn make_sup() -> *mut HewSupervisor {
        unsafe { hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 3, 5) }
    }

    /// Mark the supervisor running so `pool_child_get` doesn't short-circuit.
    unsafe fn mark_running(sup: *mut HewSupervisor) {
        unsafe { (*sup).running.store(1, Ordering::Release) };
    }

    #[test]
    fn dynamic_add_reservation_cannot_publish_into_removed_or_swapped_slot() {
        let _rt = crate::runtime_test_guard();
        let sup = unsafe { make_sup() };
        assert!(!sup.is_null());
        let base = HewChildSpec {
            name: ptr::null(),
            init_state: ptr::null_mut(),
            init_state_size: 0,
            dispatch: Some(noop_child_dispatch),
            sys_dispatch: None,
            restart_policy: RESTART_PERMANENT,
            mailbox_capacity: -1,
            overflow: OVERFLOW_DROP_NEW,
            coalesce_key_fn: None,
            coalesce_fallback: OVERFLOW_DROP_NEW,
            message_drop_fn: None,
            arena_cap_bytes: 0,
            cycle_capable: 0,
            on_crash: None,
            lifecycle_fn: None,
            init_fn: None,
            config: ptr::null_mut(),
            config_size: 0,
        };
        assert_eq!(
            unsafe { hew_supervisor_add_child_spec(sup, &raw const base) },
            0
        );
        unsafe { mark_running(sup) };

        let rendezvous = Arc::new(Barrier::new(2));
        let hook_gate = Arc::clone(&rendezvous);
        let _hook = install_dynamic_child_reserved_hook_for_test(Arc::new(move || {
            hook_gate.wait();
            hook_gate.wait();
        }));
        let sup_addr = sup as usize;
        let add = std::thread::spawn(move || {
            let sup = sup_addr as *mut HewSupervisor;
            let dynamic = HewChildSpec {
                name: ptr::null(),
                init_state: ptr::null_mut(),
                init_state_size: 0,
                dispatch: Some(noop_child_dispatch),
                sys_dispatch: None,
                restart_policy: RESTART_PERMANENT,
                mailbox_capacity: -1,
                overflow: OVERFLOW_DROP_NEW,
                coalesce_key_fn: None,
                coalesce_fallback: OVERFLOW_DROP_NEW,
                message_drop_fn: None,
                arena_cap_bytes: 0,
                cycle_capable: 0,
                on_crash: None,
                lifecycle_fn: None,
                init_fn: None,
                config: ptr::null_mut(),
                config_size: 0,
            };
            unsafe { hew_supervisor_add_child_dynamic(sup, &raw const dynamic) }
        });

        // Dynamic add has atomically reserved slot 1 but has not spawned. Remove
        // slot 0, which swap-moves the reserved identity to slot 0, then restart
        // that exact current slot. The stale add continuation must not publish a
        // second actor into its former index 1.
        rendezvous.wait();
        assert_eq!(unsafe { hew_supervisor_remove_child(sup, 0) }, 0);
        let published = unsafe { restart_child_from_spec(sup, 0) };
        assert!(!published.is_null());
        rendezvous.wait();
        assert_eq!(add.join().expect("dynamic add thread panicked"), 1);

        let roster_guard = unsafe { &(*sup).roster }.lock_or_recover();
        let s = &*roster_guard;
        assert_eq!(s.child_count, 1);
        assert_eq!(s.children, vec![published]);
        drop(roster_guard);
        unsafe { hew_supervisor_stop(sup) };
    }

    #[test]
    fn pool_child_get_live_returns_pid_as_handle() {
        // hew_supervisor_stop unregisters from the runtime-owned supervisor
        // roots, so the test needs a runtime installed (explicit-install model).
        let _rt = crate::runtime_test_guard();
        // Arrange: supervisor with one pool slot, two members.
        let sup = unsafe { make_sup() };
        assert!(!sup.is_null());

        let name = std::ffi::CString::new("workers").unwrap();
        let key = unsafe { hew_supervisor_pool_add_slot(sup, name.as_ptr(), ROUND_ROBIN, 0) };
        assert_eq!(key, 0, "first pool slot index should be 0");

        assert_eq!(unsafe { hew_supervisor_pool_member_add(sup, 0, 1001) }, 0);
        assert_eq!(unsafe { hew_supervisor_pool_member_add(sup, 0, 2002) }, 0);

        unsafe { mark_running(sup) };

        // Act: look up index 0 and index 1.
        let r0 = unsafe { hew_supervisor_pool_child_get(sup, 0, 0) };
        let r1 = unsafe { hew_supervisor_pool_child_get(sup, 0, 1) };

        // Assert: both are Live; handles encode the PIDs.
        assert!(r0.is_live(), "index 0 should be Live");
        assert!(r1.is_live(), "index 1 should be Live");
        assert_eq!(r0.handle as u64, 1001, "handle encodes PID 1001");
        assert_eq!(r1.handle as u64, 2002, "handle encodes PID 2002");

        // Cleanup.
        unsafe { hew_supervisor_stop(sup) };
    }

    #[test]
    fn pool_child_get_out_of_range_index_returns_dead_unknown_slot() {
        let _rt = crate::runtime_test_guard();
        let sup = unsafe { make_sup() };
        let name = std::ffi::CString::new("workers").unwrap();
        unsafe { hew_supervisor_pool_add_slot(sup, name.as_ptr(), ROUND_ROBIN, 0) };
        unsafe { hew_supervisor_pool_member_add(sup, 0, 42) };
        unsafe { mark_running(sup) };

        // Index 1 is beyond the single member.
        let r = unsafe { hew_supervisor_pool_child_get(sup, 0, 1) };
        assert_eq!(r.tag, 2, "should be Dead");
        assert_eq!(r.reason, ChildSlotReason::UnknownSlot as u8);
        assert!(r.handle.is_null());

        unsafe { hew_supervisor_stop(sup) };
    }

    /// hew-lang/hew#2244 regression: before this fix, codegen truncated the
    /// index to i32 before this function ever saw it, so a caller-supplied
    /// index of `2^32 + k` silently wrapped to `k` and could alias an
    /// unrelated Live member instead of failing bounds-checking. This test
    /// exercises the *runtime* half of the fix directly: `index` is now a
    /// `u64` ABI param specifically so a huge, never-legitimately-in-range
    /// index is rejected as `Dead(UnknownSlot)` on its own, with no
    /// dependence on codegen not truncating it upstream (belt-and-suspenders
    /// with the codegen-side fix in `hew-codegen-rs/src/runtime_abi.rs`).
    #[test]
    fn pool_child_get_huge_index_returns_dead_unknown_slot_not_aliased_member() {
        let _rt = crate::runtime_test_guard();
        let sup = unsafe { make_sup() };
        let name = std::ffi::CString::new("workers").unwrap();
        unsafe { hew_supervisor_pool_add_slot(sup, name.as_ptr(), ROUND_ROBIN, 0) };
        // Two members: PID 1001 at index 0, PID 2002 at index 1. If a huge
        // index silently wrapped instead of failing bounds-checking, these
        // are exactly the (wrong) Live handles it could alias.
        unsafe { hew_supervisor_pool_member_add(sup, 0, 1001) };
        unsafe { hew_supervisor_pool_member_add(sup, 0, 2002) };
        unsafe { mark_running(sup) };

        // 2^32 would wrap to 0 (aliasing PID 1001) under the old i32-truncating
        // ABI; 2^32 + 1 would wrap to 1 (aliasing PID 2002). Both must be
        // rejected as out of bounds now, never resolving to either handle.
        for huge_index in [1u64 << 32, (1u64 << 32) + 1, u64::MAX] {
            let r = unsafe { hew_supervisor_pool_child_get(sup, 0, huge_index) };
            assert_eq!(r.tag, 2, "index {huge_index} should be Dead, not Live");
            assert_eq!(r.reason, ChildSlotReason::UnknownSlot as u8);
            assert!(r.handle.is_null());
            assert_ne!(
                r.handle as u64, 1001,
                "index {huge_index} must not alias member 0's PID"
            );
            assert_ne!(
                r.handle as u64, 2002,
                "index {huge_index} must not alias member 1's PID"
            );
        }

        // Sanity: the real in-range indices still resolve Live (the fix
        // widened the ABI/added a bounds check, it did not break ordinary
        // lookups).
        let r0 = unsafe { hew_supervisor_pool_child_get(sup, 0, 0) };
        let r1 = unsafe { hew_supervisor_pool_child_get(sup, 0, 1) };
        assert!(r0.is_live() && r0.handle as u64 == 1001);
        assert!(r1.is_live() && r1.handle as u64 == 2002);

        unsafe { hew_supervisor_stop(sup) };
    }

    #[test]
    fn pool_child_get_after_member_removal_returns_dead_unknown_slot() {
        // Simulates "pool member dynamically scaled out": remove a member,
        // then look up by the (now-invalid) index → Dead(UnknownSlot).
        let _rt = crate::runtime_test_guard();
        let sup = unsafe { make_sup() };
        let name = std::ffi::CString::new("workers").unwrap();
        unsafe { hew_supervisor_pool_add_slot(sup, name.as_ptr(), ROUND_ROBIN, 0) };
        unsafe { hew_supervisor_pool_member_add(sup, 0, 100) };
        unsafe { mark_running(sup) };

        // Verify it's Live before removal.
        let before = unsafe { hew_supervisor_pool_child_get(sup, 0, 0) };
        assert!(before.is_live(), "should be Live before removal");

        // Remove the member.
        assert_eq!(unsafe { hew_supervisor_pool_member_remove(sup, 0, 100) }, 0);

        // Now index 0 is beyond the empty member list.
        let after = unsafe { hew_supervisor_pool_child_get(sup, 0, 0) };
        assert_eq!(after.tag, 2, "should be Dead after removal");
        assert_eq!(after.reason, ChildSlotReason::UnknownSlot as u8);

        unsafe { hew_supervisor_stop(sup) };
    }

    #[test]
    fn pool_child_get_unknown_pool_key_returns_dead() {
        let _rt = crate::runtime_test_guard();
        let sup = unsafe { make_sup() };
        unsafe { mark_running(sup) };

        // No pools added — pool_key 0 is invalid.
        let r = unsafe { hew_supervisor_pool_child_get(sup, 0, 0) };
        assert_eq!(r.tag, 2, "should be Dead");
        assert_eq!(r.reason, ChildSlotReason::UnknownSlot as u8);

        unsafe { hew_supervisor_stop(sup) };
    }

    #[test]
    fn pool_child_get_null_supervisor_returns_dead_shutdown() {
        // SAFETY: intentionally passing null to verify guard.
        let r = unsafe { hew_supervisor_pool_child_get(ptr::null_mut(), 0, 0) };
        assert_eq!(r.tag, 2, "should be Dead");
        assert_eq!(r.reason, ChildSlotReason::SupervisorShutdown as u8);
    }

    #[test]
    fn pool_child_get_stopped_supervisor_returns_dead_shutdown() {
        let _rt = crate::runtime_test_guard();
        let sup = unsafe { make_sup() };
        // Supervisor was never started (running == 0).
        let r = unsafe { hew_supervisor_pool_child_get(sup, 0, 0) };
        assert_eq!(r.tag, 2, "should be Dead (supervisor not running)");
        assert_eq!(r.reason, ChildSlotReason::SupervisorShutdown as u8);

        unsafe { hew_supervisor_stop(sup) };
    }

    #[test]
    fn pool_len_tracks_member_add_and_remove() {
        let _rt = crate::runtime_test_guard();
        let sup = unsafe { make_sup() };
        let name = std::ffi::CString::new("sizers").unwrap();
        let key = unsafe { hew_supervisor_pool_add_slot(sup, name.as_ptr(), ROUND_ROBIN, 0) };
        assert_eq!(key, 0);

        assert_eq!(unsafe { hew_supervisor_pool_len(sup, 0) }, 0);
        unsafe { hew_supervisor_pool_member_add(sup, 0, 11) };
        unsafe { hew_supervisor_pool_member_add(sup, 0, 22) };
        assert_eq!(unsafe { hew_supervisor_pool_len(sup, 0) }, 2);
        unsafe { hew_supervisor_pool_member_remove(sup, 0, 11) };
        assert_eq!(unsafe { hew_supervisor_pool_len(sup, 0) }, 1);

        unsafe { hew_supervisor_stop(sup) };
    }

    #[test]
    fn pool_len_invalid_key_returns_minus_one() {
        let _rt = crate::runtime_test_guard();
        let sup = unsafe { make_sup() };
        assert_eq!(unsafe { hew_supervisor_pool_len(sup, 99) }, -1);
        unsafe { hew_supervisor_stop(sup) };
    }

    #[test]
    fn multiple_pool_slots_have_disjoint_key_spaces() {
        let _rt = crate::runtime_test_guard();
        let sup = unsafe { make_sup() };
        let n0 = std::ffi::CString::new("alpha").unwrap();
        let n1 = std::ffi::CString::new("beta").unwrap();

        let k0 = unsafe { hew_supervisor_pool_add_slot(sup, n0.as_ptr(), ROUND_ROBIN, 0) };
        let k1 = unsafe { hew_supervisor_pool_add_slot(sup, n1.as_ptr(), ROUND_ROBIN, 0) };
        assert_eq!(k0, 0);
        assert_eq!(k1, 1);

        // Add different PIDs to each pool.
        unsafe { hew_supervisor_pool_member_add(sup, 0, 111) };
        unsafe { hew_supervisor_pool_member_add(sup, 1, 222) };
        unsafe { mark_running(sup) };

        let r0 = unsafe { hew_supervisor_pool_child_get(sup, 0, 0) };
        let r1 = unsafe { hew_supervisor_pool_child_get(sup, 1, 0) };

        assert!(r0.is_live());
        assert!(r1.is_live());
        assert_eq!(r0.handle as u64, 111);
        assert_eq!(r1.handle as u64, 222);

        unsafe { hew_supervisor_stop(sup) };
    }

    // ── v0.6 init-closure restart model (C1 memory-safety core) ──────────────
    //
    // These tests exercise the init-thunk path directly at the runtime ABI: the
    // thunk produces a fresh, independently-owned state on the initial spawn and
    // every restart; thunk OOM fails closed; the config buffer is freed exactly
    // once at teardown; the byte-copy template is bypassed for init_fn children.

    use std::sync::atomic::AtomicUsize;

    /// Thunk-produced actor state: a single owned heap pointer (the "owned value"
    /// the init-closure model must re-clone fresh per incarnation) plus a tag.
    /// Models the `string`/`Vec`-bearing actor state without pulling the Hew
    /// runtime string type in — the ownership shape is identical.
    #[repr(C)]
    struct InitClosureState {
        /// Owned heap allocation; freed exactly once by the registered drop fn.
        owned: *mut u8,
        /// Tag distinguishing incarnations (read from config in the thunk).
        tag: u64,
    }

    /// Count of live `InitClosureState.owned` allocations. A correct
    /// first-spawn/crash-drop/restart-reclone/teardown sequence returns this to
    /// its starting value: every thunk allocation is matched by exactly one drop.
    static INIT_CLOSURE_LIVE_OWNED: AtomicUsize = AtomicUsize::new(0);
    /// Count of thunk invocations (proves restart re-RUNS the thunk).
    static INIT_CLOSURE_THUNK_CALLS: AtomicUsize = AtomicUsize::new(0);
    /// When set, the next thunk call returns a null state (models thunk OOM).
    static INIT_CLOSURE_FAIL_NEXT: AtomicBool = AtomicBool::new(false);

    /// The config struct the supervisor captures and the thunk reads.
    #[repr(C)]
    struct InitClosureConfig {
        seed: u64,
    }

    /// Codegen-shaped init thunk: malloc a fresh state + a fresh owned heap
    /// allocation, reading `config.seed` into the state tag. Returns a null
    /// state when `INIT_CLOSURE_FAIL_NEXT` is set (models OOM, fail-closed).
    unsafe extern "C" fn init_closure_thunk(config: *const c_void) -> HewChildInitResult {
        INIT_CLOSURE_THUNK_CALLS.fetch_add(1, Ordering::SeqCst);
        if INIT_CLOSURE_FAIL_NEXT.swap(false, Ordering::SeqCst) {
            return HewChildInitResult {
                state: ptr::null_mut(),
                size: 0,
            };
        }
        // SAFETY: config is the supervisor-owned buffer or null.
        let seed = if config.is_null() {
            0
        } else {
            unsafe { (*config.cast::<InitClosureConfig>()).seed }
        };
        // Fresh owned allocation — a NEW heap each incarnation, never aliased.
        // SAFETY: 8-byte alloc; null-checked by the caller's fail-closed path.
        let owned = crate::mem::buf_try_alloc(8).cast::<u8>();
        if owned.is_null() {
            return HewChildInitResult {
                state: ptr::null_mut(),
                size: 0,
            };
        }
        INIT_CLOSURE_LIVE_OWNED.fetch_add(1, Ordering::SeqCst);
        // SAFETY: state wrapper alloc; null-checked below.
        let state = crate::mem::buf_try_alloc(std::mem::size_of::<InitClosureState>())
            .cast::<InitClosureState>();
        if state.is_null() {
            // Free the owned alloc we just took before failing closed (no leak).
            // SAFETY: owned was just allocated via the sized-block allocator.
            unsafe { crate::mem::buf_free(owned.cast::<c_void>()) };
            INIT_CLOSURE_LIVE_OWNED.fetch_sub(1, Ordering::SeqCst);
            return HewChildInitResult {
                state: ptr::null_mut(),
                size: 0,
            };
        }
        // SAFETY: state is valid for InitClosureState.
        unsafe {
            (*state).owned = owned;
            (*state).tag = seed;
        }
        HewChildInitResult {
            state: state.cast::<c_void>(),
            size: std::mem::size_of::<InitClosureState>(),
        }
    }

    /// Codegen-shaped state drop fn: frees the owned inner allocation exactly
    /// once (the wrapper itself is freed by the runtime's `buf_free`).
    unsafe extern "C" fn init_closure_drop(state: *mut c_void) {
        if state.is_null() {
            return;
        }
        let s = state.cast::<InitClosureState>();
        // SAFETY: s is a valid InitClosureState produced by the thunk.
        unsafe {
            if !(*s).owned.is_null() {
                crate::mem::buf_free((*s).owned.cast::<c_void>());
                (*s).owned = ptr::null_mut();
                INIT_CLOSURE_LIVE_OWNED.fetch_sub(1, Ordering::SeqCst);
            }
        }
    }

    /// Allocate a supervisor-owned config buffer (the capture codegen emits).
    fn make_config_buf(seed: u64) -> (*mut c_void, usize) {
        let size = std::mem::size_of::<InitClosureConfig>();
        // SAFETY: alloc + init; ownership transfers to the supervisor.
        let buf = crate::mem::buf_try_alloc(size).cast::<InitClosureConfig>();
        assert!(!buf.is_null());
        // SAFETY: buf is valid.
        unsafe { (*buf).seed = seed };
        (buf.cast::<c_void>(), size)
    }

    fn init_closure_spec(config: *mut c_void, config_size: usize) -> HewChildSpec {
        HewChildSpec {
            name: ptr::null(),
            init_state: ptr::null_mut(),
            init_state_size: 0,
            dispatch: Some(noop_child_dispatch),
            sys_dispatch: None,
            restart_policy: RESTART_PERMANENT,
            mailbox_capacity: -1,
            overflow: OVERFLOW_DROP_NEW,
            coalesce_key_fn: None,
            coalesce_fallback: OVERFLOW_DROP_NEW,
            message_drop_fn: None,
            arena_cap_bytes: 0,
            cycle_capable: 0,
            on_crash: None,
            lifecycle_fn: None,
            init_fn: Some(init_closure_thunk),
            config,
            config_size,
        }
    }

    #[test]
    fn init_fn_first_spawn_produces_fresh_owned_state_from_config() {
        let _rt = crate::runtime_test_guard();
        INIT_CLOSURE_LIVE_OWNED.store(0, Ordering::SeqCst);
        INIT_CLOSURE_THUNK_CALLS.store(0, Ordering::SeqCst);
        INIT_CLOSURE_FAIL_NEXT.store(false, Ordering::SeqCst);

        let sup = unsafe { hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 3, 5) };
        assert!(!sup.is_null());
        let (cfg, cfg_size) = make_config_buf(256);

        let spec = init_closure_spec(cfg, cfg_size);
        // The thunk is the state source: add_child_spec spawns via the thunk.
        assert_eq!(
            unsafe { hew_supervisor_add_child_spec(sup, &raw const spec) },
            0
        );
        // Register the drop fn so the actor frees its owned field on teardown.
        unsafe { hew_supervisor_set_child_state_drop(sup, 0, init_closure_drop) };
        assert_eq!(unsafe { hew_supervisor_start(sup) }, 0);

        // The thunk ran exactly once (initial spawn) and produced one owned alloc.
        assert_eq!(INIT_CLOSURE_THUNK_CALLS.load(Ordering::SeqCst), 1);
        assert_eq!(INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst), 1);

        // The byte-copy template was BYPASSED: the spec carries no init_state.
        {
            let s = locked_roster!(sup);
            assert!(s.child_specs[0].state_template.allocation.state.is_null());
            assert_eq!(s.child_specs[0].state_template.allocation.size, 0);
            assert!(s.child_specs[0].init_fn.is_some());
            // The config buffer was adopted by the supervisor.
            assert_eq!(s.config_buf, cfg);
        }

        // The child actor holds the thunk-produced state with the config seed.
        unsafe {
            let child = locked_roster!(sup).children[0];
            assert!(!child.is_null());
            let st = (*child).state.cast::<InitClosureState>();
            assert!(!st.is_null());
            assert_eq!((*st).tag, 256, "tag reflects the DYNAMIC config seed");
        }

        unsafe { hew_supervisor_stop(sup) };
        // Teardown freed the owned alloc (drop fn) AND the config buffer — once.
        assert_eq!(
            INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst),
            0,
            "no leak: the owned alloc was freed exactly once at teardown"
        );
    }

    #[test]
    fn production_crash_event_drops_fresh_init_fn_state() {
        let _rt = crate::runtime_test_guard();
        INIT_CLOSURE_LIVE_OWNED.store(0, Ordering::SeqCst);
        INIT_CLOSURE_THUNK_CALLS.store(0, Ordering::SeqCst);
        INIT_CLOSURE_FAIL_NEXT.store(false, Ordering::SeqCst);

        let sup = unsafe { hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 3, 5) };
        assert!(!sup.is_null());
        let (cfg, cfg_size) = make_config_buf(257);
        let mut spec = init_closure_spec(cfg, cfg_size);
        spec.restart_policy = RESTART_TEMPORARY;
        assert_eq!(
            unsafe { hew_supervisor_add_child_spec(sup, &raw const spec) },
            0
        );
        unsafe { hew_supervisor_set_child_state_drop(sup, 0, init_closure_drop) };
        let child = unsafe { locked_roster!(sup).children[0] };
        assert!(!child.is_null());
        assert!(
            !unsafe { (*child).state_drop_borrowed.load(Ordering::Acquire) },
            "init-thunk state is independently owned"
        );
        assert_eq!(INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst), 1);

        // No crash escrow consumed this state: the production terminal-event
        // path must therefore perform its one exact final drop.
        unsafe {
            (*child)
                .actor_state
                .store(HewActorState::Crashed as i32, Ordering::Release);
            (*sup).running.store(1, Ordering::Release);
            let event = ChildEvent {
                child_index: 0,
                child_id: (*child).id,
                exit_state: HewActorState::Crashed as i32,
                crash_code: 0,
                fault_record: 0,
            };
            supervisor_sys_dispatch_impl(
                ptr::null_mut(),
                sup.cast::<c_void>(),
                HewSysMsg::ChildCrashed as i32,
                (&raw const event).cast_mut().cast::<c_void>(),
                std::mem::size_of::<ChildEvent>(),
            );
        }
        assert_eq!(
            INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst),
            0,
            "an unconsumed init-thunk incarnation must be dropped by terminal-event teardown"
        );
        assert!(unsafe { locked_roster!(sup).children[0].is_null() });
        unsafe { hew_supervisor_stop(sup) };
    }

    #[test]
    fn init_fn_restart_re_runs_thunk_producing_independent_fresh_state() {
        let _rt = crate::runtime_test_guard();
        INIT_CLOSURE_LIVE_OWNED.store(0, Ordering::SeqCst);
        INIT_CLOSURE_THUNK_CALLS.store(0, Ordering::SeqCst);
        INIT_CLOSURE_FAIL_NEXT.store(false, Ordering::SeqCst);

        let sup_ptr = unsafe { hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 5, 60) };
        assert!(!sup_ptr.is_null());
        let (cfg, cfg_size) = make_config_buf(99);
        let spec = init_closure_spec(cfg, cfg_size);
        assert_eq!(
            unsafe { hew_supervisor_add_child_spec(sup_ptr, &raw const spec) },
            0
        );
        unsafe { hew_supervisor_set_child_state_drop(sup_ptr, 0, init_closure_drop) };

        // Initial spawn: one thunk call, one live owned alloc.
        assert_eq!(INIT_CLOSURE_THUNK_CALLS.load(Ordering::SeqCst), 1);
        assert_eq!(INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst), 1);

        // Drive a restart directly through the restart helper (the per-strategy
        // arm); the crashed child is freed by the runtime, then the thunk
        // re-runs to produce a SECOND independent state.
        unsafe {
            let s = &mut *sup_ptr;
            // Free the old child first (mirrors the crash teardown that
            // restart_with_budget_and_strategy / apply_restart performs before
            // re-spawn). This drops the first incarnation's owned alloc.
            let old = take_child_slot(s, 0);
            assert!(!old.is_null());
            actor::hew_actor_free(old);
        }
        // After the crash-drop, the first incarnation's owned alloc is gone.
        assert_eq!(
            INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst),
            0,
            "crash-drop freed the first incarnation's owned state exactly once"
        );

        // Restart: the thunk re-runs and produces fresh state #2.
        let new_child = unsafe { restart_child_from_spec(sup_ptr, 0) };
        assert!(!new_child.is_null(), "restart re-spawned the child");
        assert_eq!(
            INIT_CLOSURE_THUNK_CALLS.load(Ordering::SeqCst),
            2,
            "restart RE-RAN the thunk"
        );
        // The live-owned count discipline is the load-bearing memory-safety
        // invariant: state #1 was dropped (count→0 above), state #2's thunk
        // allocated a fresh owned heap (count→1). Equal/unequal wrapper
        // *addresses* prove nothing (a freed wrapper's address can be reused by
        // the allocator); the count + a valid, config-derived state #2 is the
        // real proof the thunk re-ran and produced fresh, unaliased owned data.
        assert_eq!(
            INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst),
            1,
            "exactly one live owned alloc after restart (fresh state #2)"
        );

        // State #2 holds a fresh, non-null owned pointer and the config-derived
        // tag — proving the thunk re-cloned from config rather than replaying a
        // stale/aliased template value.
        let second_owned = unsafe { (*new_child).state.cast::<InitClosureState>() };
        unsafe {
            assert!(
                !(*second_owned).owned.is_null(),
                "state #2 owns a fresh heap allocation"
            );
            assert_eq!((*second_owned).tag, 99, "re-cloned from the same config");
        }

        unsafe { hew_supervisor_stop(sup_ptr) };
        assert_eq!(
            INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst),
            0,
            "no leak after restart: every thunk alloc freed exactly once"
        );
    }

    #[test]
    fn init_fn_thunk_oom_fails_closed_null_slot_no_breaker_advance() {
        let _rt = crate::runtime_test_guard();
        INIT_CLOSURE_LIVE_OWNED.store(0, Ordering::SeqCst);
        INIT_CLOSURE_THUNK_CALLS.store(0, Ordering::SeqCst);
        INIT_CLOSURE_FAIL_NEXT.store(false, Ordering::SeqCst);

        let sup_ptr = unsafe { hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 5, 60) };
        let (cfg, cfg_size) = make_config_buf(7);
        let spec = init_closure_spec(cfg, cfg_size);
        assert_eq!(
            unsafe { hew_supervisor_add_child_spec(sup_ptr, &raw const spec) },
            0
        );
        unsafe { hew_supervisor_set_child_state_drop(sup_ptr, 0, init_closure_drop) };

        // Free the live child, then make the next thunk call fail (OOM).
        unsafe {
            let old = take_child_slot(sup_ptr, 0);
            actor::hew_actor_free(old);
        }
        assert_eq!(INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst), 0);

        let breaker_state_before =
            unsafe { locked_roster!(sup_ptr).child_specs[0].circuit_breaker.state };
        INIT_CLOSURE_FAIL_NEXT.store(true, Ordering::SeqCst);

        // Restart with a failing thunk: fail closed (null new_child, null slot).
        let new_child = unsafe { restart_child_from_spec(sup_ptr, 0) };
        assert!(new_child.is_null(), "thunk OOM => fail closed (null child)");
        {
            let s = locked_roster!(sup_ptr);
            assert!(
                s.children[0].is_null(),
                "the slot is left null on fail-closed"
            );
            assert_eq!(
                s.child_specs[0].circuit_breaker.state, breaker_state_before,
                "thunk OOM must NOT advance the circuit breaker (mirror clone-OOM)"
            );
        }
        // No owned alloc leaked on the OOM path (the thunk freed nothing because
        // it returned before allocating, or freed what it took).
        assert_eq!(INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst), 0);

        unsafe { hew_supervisor_stop(sup_ptr) };
        assert_eq!(INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst), 0);
    }

    /// A restart that produces no child is NOT a recovery.
    ///
    /// `restart_with_budget_and_strategy` used to discard the nullable restart
    /// result and notify restart waiters unconditionally, reporting a recovery
    /// that never happened. It must instead take the same
    /// `stop_and_maybe_escalate` route the nested-supervisor path takes on its
    /// own null restart, and report the fault as unrecovered.
    #[test]
    fn failed_restart_is_reported_as_an_unrecovered_fault() {
        let _rt = crate::runtime_test_guard();
        INIT_CLOSURE_LIVE_OWNED.store(0, Ordering::SeqCst);
        INIT_CLOSURE_THUNK_CALLS.store(0, Ordering::SeqCst);
        INIT_CLOSURE_FAIL_NEXT.store(false, Ordering::SeqCst);

        let sup_ptr = unsafe { hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 5, 60) };
        let (cfg, cfg_size) = make_config_buf(21);
        let spec = init_closure_spec(cfg, cfg_size);
        assert_eq!(
            unsafe { hew_supervisor_add_child_spec(sup_ptr, &raw const spec) },
            0
        );
        unsafe { hew_supervisor_set_child_state_drop(sup_ptr, 0, init_closure_drop) };
        assert_eq!(unsafe { hew_supervisor_start(sup_ptr) }, 0);
        assert_eq!(unsafe { (*sup_ptr).running.load(Ordering::Acquire) }, 1);
        let identity = unsafe { locked_roster!(sup_ptr).child_specs[0].identity };

        // Retire the live child and make the next init thunk fail, so the
        // restart below fails closed with a null child.
        unsafe {
            let old = take_child_slot(sup_ptr, 0);
            actor::hew_actor_free(old);
        }
        INIT_CLOSURE_FAIL_NEXT.store(true, Ordering::SeqCst);

        let outcome =
            unsafe { restart_with_budget_and_strategy(sup_ptr, identity, FaultRecord::NONE) };
        assert_eq!(
            outcome,
            FaultRuling::Unrecovered,
            "a restart that produced no child recovered nothing"
        );
        assert!(
            unsafe { locked_roster!(sup_ptr).children[0] }.is_null(),
            "the slot stays null on a failed restart"
        );
        assert_eq!(
            unsafe { (*sup_ptr).running.load(Ordering::Acquire) },
            0,
            "a supervisor that could not restart its child stops rather than \
             reporting a successful recovery to restart waiters"
        );

        unsafe { hew_supervisor_stop(sup_ptr) };
        assert_eq!(INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst), 0);
    }

    /// The counterfactual for the test above: a restart that DOES produce a
    /// child is a recovery, reported as handled, and leaves the supervisor
    /// running. Without this pair, returning `Unrecovered` unconditionally
    /// would pass.
    #[test]
    fn successful_restart_is_reported_as_handled() {
        let _rt = crate::runtime_test_guard();
        INIT_CLOSURE_LIVE_OWNED.store(0, Ordering::SeqCst);
        INIT_CLOSURE_THUNK_CALLS.store(0, Ordering::SeqCst);
        INIT_CLOSURE_FAIL_NEXT.store(false, Ordering::SeqCst);

        let sup_ptr = unsafe { hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 5, 60) };
        let (cfg, cfg_size) = make_config_buf(22);
        let spec = init_closure_spec(cfg, cfg_size);
        assert_eq!(
            unsafe { hew_supervisor_add_child_spec(sup_ptr, &raw const spec) },
            0
        );
        unsafe { hew_supervisor_set_child_state_drop(sup_ptr, 0, init_closure_drop) };
        assert_eq!(unsafe { hew_supervisor_start(sup_ptr) }, 0);
        assert_eq!(unsafe { (*sup_ptr).running.load(Ordering::Acquire) }, 1);
        let identity = unsafe { locked_roster!(sup_ptr).child_specs[0].identity };

        unsafe {
            let old = take_child_slot(sup_ptr, 0);
            actor::hew_actor_free(old);
        }

        let outcome =
            unsafe { restart_with_budget_and_strategy(sup_ptr, identity, FaultRecord::NONE) };
        assert_eq!(outcome, FaultRuling::Handled);
        assert!(
            !unsafe { locked_roster!(sup_ptr).children[0] }.is_null(),
            "the restart published a fresh child"
        );
        assert_eq!(
            unsafe { (*sup_ptr).running.load(Ordering::Acquire) },
            1,
            "a supervisor that recovered its child keeps running"
        );

        unsafe { hew_supervisor_stop(sup_ptr) };
        assert_eq!(INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst), 0);
    }

    #[test]
    fn init_fn_config_buffer_freed_exactly_once_at_teardown() {
        // Two init_fn children sharing ONE config buffer: the buffer is adopted
        // once and freed once at teardown (no double-free across children).
        let _rt = crate::runtime_test_guard();
        INIT_CLOSURE_LIVE_OWNED.store(0, Ordering::SeqCst);
        INIT_CLOSURE_THUNK_CALLS.store(0, Ordering::SeqCst);
        INIT_CLOSURE_FAIL_NEXT.store(false, Ordering::SeqCst);

        let sup_ptr = unsafe { hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 3, 5) };
        let (cfg, cfg_size) = make_config_buf(11);

        let spec0 = init_closure_spec(cfg, cfg_size);
        let spec1 = init_closure_spec(cfg, cfg_size);
        assert_eq!(
            unsafe { hew_supervisor_add_child_spec(sup_ptr, &raw const spec0) },
            0
        );
        assert_eq!(
            unsafe { hew_supervisor_add_child_spec(sup_ptr, &raw const spec1) },
            0
        );
        unsafe { hew_supervisor_set_child_state_drop(sup_ptr, 0, init_closure_drop) };
        unsafe { hew_supervisor_set_child_state_drop(sup_ptr, 1, init_closure_drop) };

        {
            // Both specs borrow the same single supervisor-owned buffer.
            let s = locked_roster!(sup_ptr);
            assert_eq!(s.config_buf, cfg);
            assert_eq!(s.child_specs[0].config, cfg);
            assert_eq!(s.child_specs[1].config, cfg);
            assert_eq!(s.config_size, cfg_size);
        }
        // Two children => two thunk calls => two live owned allocs.
        assert_eq!(INIT_CLOSURE_THUNK_CALLS.load(Ordering::SeqCst), 2);
        assert_eq!(INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst), 2);

        // Teardown frees both owned allocs (drop fns) and the config buffer once.
        // A double-free of the config buffer would abort under a hardened
        // allocator; reaching the assertion proves the single-free contract.
        unsafe { hew_supervisor_stop(sup_ptr) };
        assert_eq!(INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst), 0);
    }

    // ── Static-backed pool (S3/S4): members resolve through live static slots ──

    /// Register N static children, then bind them as pool members via
    /// `pool_member_add_static`. The accessor resolves each member through its
    /// LIVE static slot, `pool_len` reports N, and an OOB index is Dead.
    #[test]
    fn static_backed_pool_resolves_members_through_live_slots() {
        let _rt = crate::runtime_test_guard();
        INIT_CLOSURE_LIVE_OWNED.store(0, Ordering::SeqCst);
        INIT_CLOSURE_THUNK_CALLS.store(0, Ordering::SeqCst);
        INIT_CLOSURE_FAIL_NEXT.store(false, Ordering::SeqCst);

        let sup = unsafe { hew_supervisor_new(STRATEGY_SIMPLE_ONE_FOR_ONE, 5, 60) };
        assert!(!sup.is_null());
        let (cfg, cfg_size) = make_config_buf(3);

        // Spawn 3 fungible members as static children (the bootstrap shape).
        for _ in 0..3 {
            let spec = init_closure_spec(cfg, cfg_size);
            assert_eq!(
                unsafe { hew_supervisor_add_child_spec(sup, &raw const spec) },
                0
            );
        }
        for idx in 0..3 {
            unsafe { hew_supervisor_set_child_state_drop(sup, idx, init_closure_drop) };
        }

        // Register the pool slot and bind each static child as a member.
        let name = std::ffi::CString::new("workers").unwrap();
        let key = unsafe { hew_supervisor_pool_add_slot(sup, name.as_ptr(), ROUND_ROBIN, 0) };
        assert_eq!(key, 0);
        for idx in 0..3u32 {
            assert_eq!(
                unsafe { hew_supervisor_pool_member_add_static(sup, 0, idx) },
                0,
                "static member {idx} registered"
            );
        }

        unsafe { (*sup).running.store(1, Ordering::Release) };

        // len reports the fixed static-member count.
        assert_eq!(unsafe { hew_supervisor_pool_len(sup, 0) }, 3);

        // Each member resolves Live to its static slot's actor.
        for idx in 0..3u32 {
            let r = unsafe { hew_supervisor_pool_child_get(sup, 0, u64::from(idx)) };
            assert!(r.is_live(), "member {idx} should be Live");
            let expected = unsafe { locked_roster!(sup).children[idx as usize] };
            assert_eq!(
                r.handle, expected,
                "member {idx} resolves to its live static-slot actor"
            );
        }

        // OOB index → Dead(UnknownSlot) (Vec[i] OOB parity).
        let oob = unsafe { hew_supervisor_pool_child_get(sup, 0, 3) };
        assert_eq!(oob.tag, 2, "index 3 is beyond the 3 members → Dead");
        assert_eq!(oob.reason, ChildSlotReason::UnknownSlot as u8);

        unsafe { hew_supervisor_stop(sup) };
        assert_eq!(INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst), 0);
    }

    /// After a static-backed member crashes and restarts, the accessor
    /// re-resolves to the FRESH actor — no stale PID is cached. This is the
    /// load-bearing restart re-resolution contract for the static pool.
    #[test]
    fn static_backed_pool_member_reresolves_after_restart() {
        let _rt = crate::runtime_test_guard();
        INIT_CLOSURE_LIVE_OWNED.store(0, Ordering::SeqCst);
        INIT_CLOSURE_THUNK_CALLS.store(0, Ordering::SeqCst);
        INIT_CLOSURE_FAIL_NEXT.store(false, Ordering::SeqCst);

        let sup = unsafe { hew_supervisor_new(STRATEGY_SIMPLE_ONE_FOR_ONE, 5, 60) };
        let (cfg, cfg_size) = make_config_buf(42);
        // Two members.
        for _ in 0..2 {
            let spec = init_closure_spec(cfg, cfg_size);
            assert_eq!(
                unsafe { hew_supervisor_add_child_spec(sup, &raw const spec) },
                0
            );
        }
        for idx in 0..2 {
            unsafe { hew_supervisor_set_child_state_drop(sup, idx, init_closure_drop) };
        }
        let name = std::ffi::CString::new("workers").unwrap();
        unsafe { hew_supervisor_pool_add_slot(sup, name.as_ptr(), ROUND_ROBIN, 0) };
        for idx in 0..2u32 {
            unsafe { hew_supervisor_pool_member_add_static(sup, 0, idx) };
        }
        unsafe { (*sup).running.store(1, Ordering::Release) };

        // Snapshot member 1's actor before the crash.
        let before = unsafe { hew_supervisor_pool_child_get(sup, 0, 1) };
        assert!(before.is_live());
        let crashed_actor = before.handle;
        let thunks_before = INIT_CLOSURE_THUNK_CALLS.load(Ordering::SeqCst);
        // Crash member 1: free its static slot (mirrors the crash teardown), then
        // restart it through the per-child helper (the SIMPLE_ONE_FOR_ONE arm
        // calls exactly this).
        unsafe {
            let old = take_child_slot(&raw mut *sup, 1);
            assert_eq!(old, crashed_actor);
            actor::hew_actor_free(old);
        }
        // While the slot is null, the accessor reports the member as restarting
        // (Transient), NEVER the freed actor — a stale-PID cache would return the
        // crashed handle here.
        let mid = unsafe { hew_supervisor_pool_child_get(sup, 0, 1) };
        assert_ne!(
            mid.tag, 0,
            "a null slot must not resolve as Live (no stale PID)"
        );

        let new_actor = unsafe { restart_child_from_spec(sup, 1) };
        assert!(!new_actor.is_null(), "member 1 restarted");
        assert_eq!(
            INIT_CLOSURE_THUNK_CALLS.load(Ordering::SeqCst),
            thunks_before + 1,
            "restart RE-RAN the per-member init thunk"
        );

        // The accessor re-resolves member 1 to the FRESH actor through the LIVE
        // static slot — no stale PID is cached in the pool. (Comparing against the
        // crashed pointer proves nothing: a freed wrapper's address can be
        // reused; the live-slot identity is the real proof.)
        let after = unsafe { hew_supervisor_pool_child_get(sup, 0, 1) };
        assert!(after.is_live(), "member 1 is Live again after restart");
        assert_eq!(
            after.handle, new_actor,
            "pool re-resolves to the restarted actor through the live static slot"
        );
        assert_eq!(after.handle, unsafe { locked_roster!(sup).children[1] });

        // Member 0 is untouched by the SIMPLE_ONE_FOR_ONE per-member restart.
        let member0 = unsafe { hew_supervisor_pool_child_get(sup, 0, 0) };
        assert!(member0.is_live(), "member 0 stayed live");

        unsafe { hew_supervisor_stop(sup) };
        assert_eq!(INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst), 0);
    }

    /// Driving the `SIMPLE_ONE_FOR_ONE` strategy arm (not the helper directly)
    /// restarts the crashed pool member per-member and the pool re-resolves it.
    /// This proves the arm is wired (it was previously an empty no-op).
    #[test]
    fn simple_one_for_one_arm_restarts_crashed_pool_member() {
        let _rt = crate::runtime_test_guard();
        INIT_CLOSURE_LIVE_OWNED.store(0, Ordering::SeqCst);
        INIT_CLOSURE_THUNK_CALLS.store(0, Ordering::SeqCst);
        INIT_CLOSURE_FAIL_NEXT.store(false, Ordering::SeqCst);

        let sup = unsafe { hew_supervisor_new(STRATEGY_SIMPLE_ONE_FOR_ONE, 5, 60) };
        let (cfg, cfg_size) = make_config_buf(13);
        for _ in 0..2 {
            let spec = init_closure_spec(cfg, cfg_size);
            assert_eq!(
                unsafe { hew_supervisor_add_child_spec(sup, &raw const spec) },
                0
            );
        }
        for idx in 0..2 {
            unsafe { hew_supervisor_set_child_state_drop(sup, idx, init_closure_drop) };
        }
        let name = std::ffi::CString::new("workers").unwrap();
        unsafe { hew_supervisor_pool_add_slot(sup, name.as_ptr(), ROUND_ROBIN, 0) };
        for idx in 0..2u32 {
            unsafe { hew_supervisor_pool_member_add_static(sup, 0, idx) };
        }
        unsafe { (*sup).running.store(1, Ordering::Release) };

        let thunks_before = INIT_CLOSURE_THUNK_CALLS.load(Ordering::SeqCst);
        let failed_identity = unsafe { (*sup).roster.lock_or_recover().child_specs[0].identity };
        // Crash member 0: free its slot, then drive the strategy arm (which now
        // routes SIMPLE_ONE_FOR_ONE → restart_child_from_spec for the member).
        unsafe {
            let old = take_child_slot(sup, 0);
            actor::hew_actor_free(old);
        }
        unsafe { restart_with_budget_and_strategy(sup, failed_identity, FaultRecord::NONE) };

        assert_eq!(
            INIT_CLOSURE_THUNK_CALLS.load(Ordering::SeqCst),
            thunks_before + 1,
            "the SIMPLE_ONE_FOR_ONE arm restarted the member (re-ran its thunk)"
        );
        let after = unsafe { hew_supervisor_pool_child_get(sup, 0, 0) };
        assert!(
            after.is_live(),
            "member 0 is Live again after the arm restart"
        );
        assert_eq!(after.handle, unsafe { locked_roster!(sup).children[0] });

        unsafe { hew_supervisor_stop(sup) };
        assert_eq!(INIT_CLOSURE_LIVE_OWNED.load(Ordering::SeqCst), 0);
    }
}
