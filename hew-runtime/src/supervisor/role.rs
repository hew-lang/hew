//! Role-keyed send/ask, circuit breaker and restart-await entry points.
#[allow(
    clippy::wildcard_imports,
    reason = "submodule split from a single large file; re-exports its parent scope"
)]
use super::*;

#[cfg(all(test, not(target_arch = "wasm32")))]
fn fire_role_ask_submit_gap_hook() {
    let hook = ROLE_ASK_SUBMIT_GAP_HOOK.lock_or_recover().clone();
    if let Some(hook) = hook {
        hook();
    }
}

/// Test-only hook fired by the role-ask entry points AFTER `roster` is
/// released and BEFORE the ID-pinned submission. Two regressions install it:
/// the lock-order test probes `roster` from a helper thread and asserts
/// it is FREE (the enqueue — including a Block-policy capacity wait — never
/// runs under the slot lock), and the retirement-interleaving test frees the
/// resolved incarnation here and asserts the submission fails closed instead
/// of touching reclaimed storage.
#[cfg(all(test, not(target_arch = "wasm32")))]
pub(crate) static ROLE_ASK_PINNED_SUBMIT_HOOK: Mutex<Option<Arc<dyn Fn() + Send + Sync>>> =
    Mutex::new(None);

#[cfg(all(test, not(target_arch = "wasm32")))]
fn fire_role_ask_pinned_submit_hook() {
    let hook = ROLE_ASK_PINNED_SUBMIT_HOOK.lock_or_recover().clone();
    if let Some(hook) = hook {
        hook();
    }
}

/// Name a [`ChildSlotReason`] discriminant for the role-ask refusal
/// diagnostic. Fail-closed: an out-of-range discriminant (ABI drift) names
/// itself as such rather than borrowing a real reason's name.
#[cfg(not(target_arch = "wasm32"))]
const fn child_slot_reason_name(reason: u8) -> &'static str {
    match reason {
        r if r == ChildSlotReason::Ok as u8 => "Ok",
        r if r == ChildSlotReason::Restarting as u8 => "Restarting",
        r if r == ChildSlotReason::BackoffDelay as u8 => "BackoffDelay",
        r if r == ChildSlotReason::CircuitOpen as u8 => "CircuitOpen",
        r if r == ChildSlotReason::BudgetExhausted as u8 => "BudgetExhausted",
        r if r == ChildSlotReason::SupervisorShutdown as u8 => "SupervisorShutdown",
        r if r == ChildSlotReason::UnknownSlot as u8 => "UnknownSlot",
        _ => "(unrecognized ChildSlotReason discriminant)",
    }
}

/// Refuse an owner-scoped role ask closed, recording the classified slot
/// state so a Dead slot is never conflated with a Transient one at the
/// diagnostic surface (the tag semantics of the classified lookup ABIs —
/// contrast the tag-unchecked handle extraction the retired token path
/// performed, which collapsed every non-Live state into a token-0 send).
#[cfg(not(target_arch = "wasm32"))]
fn role_refuse(reason: u8, key: u32, record_ask_error: bool) -> i32 {
    set_last_error(format!(
        "stable-role ask refused: child slot {key} is {}",
        child_slot_reason_name(reason)
    ));
    // Classify the refusal in the TLS ask-error slot: the suspending
    // with-channel caller binds its Err kind from
    // `hew_actor_ask_take_last_error`, and an unwritten slot misreports this
    // genuine refusal as `AskError::NoError` (dogfood F1, mechanism 2).
    if record_ask_error {
        crate::actor::record_ask_error(crate::internal::types::AskError::ActorStopped);
    }
    crate::internal::types::HewError::ErrActorStopped as i32
}

/// Resolve the CURRENT incarnation of a stable-role slot to its actor ID
/// under ONE `roster` critical section, with every refusal classified
/// by slot state (see [`role_ask_refuse`]).
///
/// This is phase one of the owner-scoped role ask. The lock covers resolution
/// and classification ONLY — never the mailbox submission. LOCK-ORDER
/// INVARIANT: `roster` must not be held across a mailbox enqueue,
/// because a Block-policy mailbox at capacity WAITS for space
/// (`mailbox.rs` `HewOverflowPolicy::Block`), and the child draining that
/// mailbox may itself issue a stable-role ask that acquires `roster` —
/// holding the lock across the wait closes a cycle (submitter waits for the
/// drainer, drainer waits for the lock). Phase two therefore submits against
/// the returned ID via the `LIVE_ACTORS` send pin (`with_actor_send_by_id`),
/// the same liveness protocol every by-ID send uses: the pin guarantees the
/// allocation outlives the submission, and a retirement that lands between
/// the phases fails CLOSED instead of touching reclaimed storage.
///
/// The returned pair is `(packed id, full spawn serial)`. The packed `id`
/// masks the serial to 48 bits (`pid::hew_pid_make`), so two incarnations can in
/// principle collide on `id`; phase two therefore matches the pinned actor's
/// full serial against the resolved one
/// (`live_actors::with_actor_send_by_identity`) so an aliased `id` refuses
/// closed rather than delivering to the wrong actor. The spawn allocator refuses
/// past `pid::MAX_ACTOR_SERIAL` rather than wrapping, so the collision is not
/// reachable in production; the serial match is what keeps wrong-actor delivery
/// unrepresentable at this seam regardless. Both scalars are copied out under
/// `roster`; no pointer crosses the lock boundary.
#[cfg(not(target_arch = "wasm32"))]
fn role_resolve_current_child_id(
    token: crate::lifetime::local_handles::HewLocalPidId,
    key: u32,
    record_ask_error: bool,
) -> Result<(u64, u64), i32> {
    let Some(pin) = crate::lifetime::local_handles::pin_current_supervisor(token) else {
        return Err(role_refuse(
            ChildSlotReason::SupervisorShutdown as u8,
            key,
            record_ask_error,
        ));
    };
    let sup = pin.supervisor();

    // Fast-path shutdown check (atomics, no lock).
    // SAFETY: the stable-identity pin keeps `sup` live for these atomic reads.
    if unsafe {
        (*sup).cancelled.load(Ordering::Acquire) || (*sup).running.load(Ordering::Acquire) == 0
    } {
        return Err(role_refuse(
            ChildSlotReason::SupervisorShutdown as u8,
            key,
            record_ask_error,
        ));
    }
    // SAFETY: the stable-identity pin keeps `sup` live through this lookup.
    let guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard protects this scoped child/spec roster access.
    let s = &*guard;

    // Re-check shutdown under the lock (the supervisor can be cancelled
    // between the atomic check above and acquiring the lock).
    // SAFETY: the stable-identity pin keeps the allocation live through this
    // atomic re-check.
    if unsafe {
        (*sup).cancelled.load(Ordering::Acquire) || (*sup).running.load(Ordering::Acquire) == 0
    } {
        return Err(role_refuse(
            ChildSlotReason::SupervisorShutdown as u8,
            key,
            record_ask_error,
        ));
    }

    let i = key as usize;
    if i >= s.child_count {
        return Err(role_refuse(
            ChildSlotReason::UnknownSlot as u8,
            key,
            record_ask_error,
        ));
    }

    let child = s.children.get(i).copied().unwrap_or(ptr::null_mut());
    if child.is_null() {
        // Mid-restart (Transient) or permanently dead: refuse rather than
        // guess a future incarnation. Nothing was enqueued; the refusal
        // carries the classified slot state (the tag semantics the lookup
        // ABIs expose) so a Dead slot is never conflated with a Transient
        // one at the diagnostic surface.
        return Err(role_refuse(
            classify_null_child_slot(s, i).reason,
            key,
            record_ask_error,
        ));
    }

    #[cfg(test)]
    fire_role_ask_submit_gap_hook();

    // SAFETY: `child` is the slot's live incarnation and cannot be replaced or
    // reclaimed while `roster` is held; only the scalar id + full serial
    // are copied out — no pointer crosses the lock boundary. The full serial is
    // the aliasing-proof discriminator phase two re-checks against the pinned
    // actor (the packed id alone can alias after 2^48 allocations).
    Ok(unsafe { ((*child).id, (*child).spawn_serial) })
}

/// The classified refusal for a resolution that succeeded but whose
/// incarnation was retired before the ID-pinned submission could begin
/// (`with_actor_send_by_id` found the ID no longer live). Fail-closed and
/// named: the ask enqueued nothing.
#[cfg(not(target_arch = "wasm32"))]
fn role_ask_refuse_retired(key: u32) -> i32 {
    set_last_error(format!(
        "stable-role ask refused: child slot {key} incarnation retired during submission"
    ));
    // Same TLS classification contract as `role_ask_refuse`.
    crate::actor::record_ask_error(crate::internal::types::AskError::ActorStopped);
    crate::internal::types::HewError::ErrActorStopped as i32
}

/// Submit a tell through a stable `(supervisor token, child slot)` role.
/// No actor pointer escapes the owner-scoped resolution and identity-pinned
/// enqueue sequence. A restarting, stopped, or retired child returns
/// `ErrActorStopped`; the caller retains its payload on every error.
///
/// # Safety
///
/// `data` must point to at least `size` readable bytes, or be null for zero size.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_role_send(
    token: crate::lifetime::local_handles::HewLocalPidId,
    key: u32,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> i32 {
    let (child_id, child_serial) = match role_resolve_current_child_id(token, key, false) {
        Ok(ids) => ids,
        Err(code) => return code,
    };
    crate::lifetime::live_actors::with_actor_send_by_identity(child_id, child_serial, |pin| {
        // SAFETY: the identity pin keeps the resolved incarnation live and
        // the caller supplies the readable payload range.
        unsafe { crate::actor::actor_send_pinned(pin, msg_type, data, size) }
    })
    .unwrap_or_else(|| {
        set_last_error(format!(
            "stable-role tell refused: child slot {key} incarnation retired during submission"
        ));
        crate::internal::types::HewError::ErrActorStopped as i32
    })
}

#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn hew_supervisor_role_send(
    _token: crate::lifetime::local_handles::HewLocalPidId,
    _key: u32,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
) -> i32 {
    crate::internal::types::HewError::ErrActorStopped as i32
}

/// Cooperatively submit a bounded `overflow block` tell through a stable
/// supervisor role. Resolution and mailbox registration occur under one
/// identity pin; on acceptance, `out_actor_id` receives the exact incarnation
/// whose mailbox owns the waiter so cancellation can detach from that
/// mailbox, and `out_registration_id` receives the
/// [`crate::mailbox::BlockedSenderId`] minted for a parked waiter (`0` when
/// admitted immediately). Detach must name the registration id rather than
/// the read-slot address: slot allocations are recycled, so a detach keyed on
/// the pointer could remove a different registration that reused the freed
/// address (#3147).
///
/// # Safety
///
/// `data`, `sender`, and `slot` must satisfy
/// [`crate::actor::actor_await_send_pinned`]. `out_actor_id` and
/// `out_registration_id` must be writable.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_role_await_send(
    token: crate::lifetime::local_handles::HewLocalPidId,
    key: u32,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    sender: *mut HewActor,
    slot: *mut crate::read_slot::HewReadSlot,
    out_actor_id: *mut u64,
    out_registration_id: *mut u64,
) -> c_int {
    if out_actor_id.is_null() || out_registration_id.is_null() {
        set_last_error("stable-role cooperative tell received a null output".to_string());
        return crate::internal::types::HewError::ErrActorStopped as i32;
    }
    // SAFETY: the null check above establishes the caller-provided outputs
    // are writable. Initialize them even on refusal because codegen loads
    // before branching on status and only uses the values on the suspend edge.
    unsafe {
        out_actor_id.write(0);
        out_registration_id.write(0);
    }
    let (child_id, child_serial) = match role_resolve_current_child_id(token, key, false) {
        Ok(ids) => ids,
        Err(code) => return code,
    };
    let (rc, registration_id) = crate::lifetime::live_actors::with_actor_send_by_identity(
        child_id,
        child_serial,
        |pin| {
            // SAFETY: the identity pin keeps this incarnation live for the
            // complete mailbox registration.
            unsafe {
                crate::actor::actor_await_send_pinned(
                    pin.as_ptr(),
                    msg_type,
                    data,
                    size,
                    sender,
                    slot,
                )
            }
        },
    )
    .unwrap_or_else(|| {
        set_last_error(format!(
            "stable-role cooperative tell refused: child slot {key} incarnation retired during submission"
        ));
        (crate::internal::types::HewError::ErrActorStopped as i32, 0)
    });
    if rc >= 0 {
        // SAFETY: non-null writable outputs are part of this ABI's contract.
        unsafe {
            out_actor_id.write(child_id);
            out_registration_id.write(registration_id);
        }
    }
    rc
}

#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn hew_supervisor_role_await_send(
    _token: crate::lifetime::local_handles::HewLocalPidId,
    _key: u32,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
    _sender: *mut HewActor,
    _slot: *mut crate::read_slot::HewReadSlot,
    _out_actor_id: *mut u64,
    _out_registration_id: *mut u64,
) -> c_int {
    crate::internal::types::HewError::ErrActorStopped as i32
}

/// Submit an ask with a caller-owned reply channel through a fungible
/// `(stable supervisor token, static slot)` role.
///
/// Two-phase owner-scoped submission, replacing the racy
/// lookup-token-then-send shape (`hew_local_pid_supervisor_child_get` followed
/// by `hew_local_pid_ask_with_channel`), whose unlocked gap let the restart
/// machinery advance the slot between resolution and submission — the
/// resolved identity went stale, or the ask was accepted by an incarnation
/// the supervisor was about to retire, orphaning the reply with no diagnostic
/// at the join site:
///
/// 1. [`role_resolve_current_child_id`]: resolve + classify under
///    `roster`; only the incarnation's scalar ID leaves the lock.
/// 2. ID-pinned submission via `with_actor_send_by_id`: the `LIVE_ACTORS` pin
///    keeps the allocation live for the complete enqueue (including a
///    Block-policy capacity wait, which deliberately runs OUTSIDE
///    `roster` — see the lock-order invariant on phase one). A
///    retirement interposing between the phases fails closed with a named
///    refusal; an ask accepted and then retired before dispatch resolves
///    through the classified-null machinery (`hew_reply_channel_failure_kind`).
///
/// Returns `HewError::Ok` on submission; every refusal is classified in the
/// error slot. The channel-reference discipline matches
/// [`crate::actor::hew_actor_ask_with_channel`]: the caller's creator ref
/// survives failure.
///
/// # Safety
///
/// `data` and `ch` must satisfy
/// [`crate::actor::hew_actor_ask_with_channel`]'s contract.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_role_ask_with_channel(
    token: crate::lifetime::local_handles::HewLocalPidId,
    key: u32,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
    ch: *mut c_void,
) -> i32 {
    let (child_id, child_serial) = match role_resolve_current_child_id(token, key, true) {
        Ok(ids) => ids,
        Err(code) => return code,
    };
    #[cfg(test)]
    fire_role_ask_pinned_submit_hook();
    crate::lifetime::live_actors::with_actor_send_by_identity(child_id, child_serial, |pin| {
        // SAFETY: the send pin keeps `actor` live for the submission;
        // `data`/`ch` follow this fn's contract. The identity-verified pin
        // refuses closed (returns None) if the id aliased a different
        // incarnation, so no wrong-actor enqueue can occur here.
        unsafe { crate::actor::ask_with_channel_pinned(pin.as_ptr(), msg_type, data, size, ch) }
    })
    .unwrap_or_else(|| role_ask_refuse_retired(key))
}

/// Blocking twin of [`hew_supervisor_role_ask_with_channel`] for callers with
/// no parkable continuation (`main` / free functions): resolve the role under
/// `roster` (classified refusals), then run the ID-pinned blocking ask
/// (`hew_actor_ask_by_id` — the same pin + reply-wait protocol every by-ID ask
/// uses).
///
/// This replaces the raw lookup-then-ask pair the blocking fungible path
/// emitted (`hew_supervisor_child_get` returning an UNPINNED `*mut HewActor`,
/// then `hew_actor_ask` dereferencing it), whose gap let a restart free the
/// incarnation between the lookup and the deref — a use-after-free, not a
/// refusal. No pointer crosses the resolve/submit boundary here; a retirement
/// interposing between the phases fails closed to a null reply with
/// `AskError::ActorStopped`.
///
/// Return contract matches [`crate::actor::hew_actor_ask`]: the reply buffer
/// (caller frees) or null with the ask error recorded for
/// `hew_actor_ask_take_last_error`.
///
/// # Safety
///
/// `data` must point to at least `size` readable bytes, or be null when
/// `size` is 0.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_role_ask(
    token: crate::lifetime::local_handles::HewLocalPidId,
    key: u32,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> *mut c_void {
    let Ok((child_id, child_serial)) = role_resolve_current_child_id(token, key, true) else {
        return crate::actor::actor_ask_null_actor_stopped();
    };
    #[cfg(test)]
    fire_role_ask_pinned_submit_hook();
    // SAFETY: the identity-verified by-ID ask pins the resolved actor for the
    // submission and blocks on the reply channel; `data`/`size` follow this
    // fn's contract. A serial mismatch (aliased id) fails closed to a null
    // reply with `AskError::ActorStopped`, never a wrong-actor delivery.
    unsafe { crate::actor::hew_actor_ask_by_identity(child_id, child_serial, msg_type, data, size) }
}

/// Supervisors are unavailable on the wasm runtime; the owner-scoped role ask
/// keeps symbol parity and fails closed exactly like the lookup twin above.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn hew_supervisor_role_ask_with_channel(
    _token: crate::lifetime::local_handles::HewLocalPidId,
    _key: u32,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
    _ch: *mut c_void,
) -> i32 {
    // Same TLS classification contract as the native refusal paths: the
    // with-channel caller binds its Err kind from the ask-error slot.
    crate::actor::record_ask_error(crate::internal::types::AskError::ActorStopped);
    crate::internal::types::HewError::ErrActorStopped as i32
}

/// Supervisors are unavailable on the wasm runtime; the blocking role ask
/// keeps symbol parity and fails closed to a null reply.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub extern "C" fn hew_supervisor_role_ask(
    _token: crate::lifetime::local_handles::HewLocalPidId,
    _key: u32,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
) -> *mut c_void {
    // Null reply + classified error slot, matching the native blocking twin's
    // `actor_ask_null_actor_stopped` refusal surface.
    crate::actor::record_ask_error(crate::internal::types::AskError::ActorStopped);
    ptr::null_mut()
}

/// Look up a nested child supervisor by its compile-time-assigned slot index.
///
/// Used for traversing supervision trees one dot segment at a time:
/// `app.api.auth` calls this for `.api` (returning `*mut HewSupervisor`
/// cast as `handle`), then [`hew_supervisor_child_get`] for `.auth`.
///
/// The returned `handle` field carries a `*mut HewSupervisor` bit-pattern.
/// The compile-time type at the call site disambiguates — codegen reinterprets
/// the pointer without an additional tag because the checker has already typed
/// the dot segment as a supervisor child.
///
/// Discrimination: same FSM as [`hew_supervisor_child_get`], but over
/// `child_supervisors` and `child_supervisor_specs`. A null supervisor slot
/// (child supervisor being restarted) returns `Transient(Restarting)`;
/// an out-of-range `key` returns `Dead(UnknownSlot)`.
///
/// The roster's pointer is a cached address; the slot's stable token is the one
/// authority for that allocation's lifetime. A slot whose token no longer pins
/// holds a retired occupant, which reads exactly like an empty slot: the ruling
/// that replaces it is in flight.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`] (or by a
/// prior nested lookup). Behaviour is undefined if `sup` has been freed.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_nested_get(
    sup: *mut HewSupervisor,
    key: u32,
) -> ChildLookupResult {
    if sup.is_null() {
        return ChildLookupResult::dead(ChildSlotReason::SupervisorShutdown);
    }
    // SAFETY: caller keeps `sup` live for these atomic reads.
    if unsafe {
        (*sup).cancelled.load(Ordering::Acquire) || (*sup).running.load(Ordering::Acquire) == 0
    } {
        return ChildLookupResult::dead(ChildSlotReason::SupervisorShutdown);
    }

    let i = key as usize;
    // The pointer, its stable token and the spec are one parallel-roster entry
    // protected by `roster`; restart, nested stop, and public lookup cannot
    // observe different generations of the triple. The roster is released
    // before the token is pinned, so no roster/handle-registry nesting.
    let entry = {
        // SAFETY: caller keeps `sup` live through this nested-roster lookup.
        let guard = unsafe { &(*sup).roster }.lock_or_recover();
        // SAFETY: the guard protects this scoped parallel-roster access.
        let s = &*guard;
        debug_assert_eq!(s.child_supervisors.len(), s.child_supervisor_tokens.len());
        if i >= s.child_supervisors.len() {
            return ChildLookupResult::dead(ChildSlotReason::UnknownSlot);
        }
        let spent = s
            .child_supervisor_specs
            .get(i)
            .and_then(Option::as_ref)
            .is_some_and(|spec| spec.spent);
        (s.child_supervisors[i], s.child_supervisor_tokens[i], spent)
    };
    let (child_sup, token, spent) = entry;

    if crate::lifetime::local_handles::pin_current_supervisor(token)
        .is_some_and(|pin| pin.supervisor() == child_sup)
    {
        // Reinterpret the supervisor pointer as HewActor* for the shared
        // result struct. Codegen reconstructs the *mut HewSupervisor at the
        // typed call site. The cast is a bit-pattern reinterpretation only;
        // neither type is read through at this point.
        // SAFETY: cast is a pointer-size-preserving reinterpretation; the
        // MIR call site at the dotted-access lowering casts back to
        // *mut HewSupervisor before dereferencing.
        return ChildLookupResult::live(child_sup.cast::<HewActor>());
    }

    if spent {
        return ChildLookupResult::dead(ChildSlotReason::SupervisorShutdown);
    }
    // Empty or retired slot — the child supervisor is being restarted, was
    // never started, or its occupant is already retired.
    ChildLookupResult::transient(ChildSlotReason::Restarting)
}

/// Return whether the supervisor is still running (1) or stopped (0).
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_is_running(sup: *mut HewSupervisor) -> c_int {
    cabi_guard!(sup.is_null(), 0);
    // SAFETY: caller guarantees sup is valid for this atomic load.
    unsafe { (*sup).running.load(Ordering::Acquire) }
}

/// Configure circuit breaker settings for a child.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
/// `child_index` must be within the range of added children.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_set_circuit_breaker(
    sup: *mut HewSupervisor,
    child_index: c_int,
    max_crashes: u32,
    window_secs: u32,
    cooldown_secs: u32,
) -> c_int {
    if sup.is_null() || child_index < 0 {
        return -1;
    }

    #[expect(
        clippy::cast_sign_loss,
        reason = "child_index is checked to be non-negative"
    )]
    let index = child_index as usize;

    // SAFETY: caller keeps `sup` live through this metadata update.
    let mut guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard serializes this scoped mutable roster access.
    let s = &mut *guard;
    if index >= s.child_count {
        return -1;
    }

    let spec = &mut s.child_specs[index];
    spec.circuit_breaker.max_crashes = max_crashes;
    spec.circuit_breaker.window_secs = window_secs;
    spec.circuit_breaker.cooldown_secs = cooldown_secs;

    0
}

/// Get the current circuit breaker state for a child.
///
/// Returns 0 for CLOSED, 1 for OPEN, 2 for `HALF_OPEN`, -1 for error.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
/// `child_index` must be within the range of added children.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_get_child_circuit_state(
    sup: *mut HewSupervisor,
    child_index: c_int,
) -> c_int {
    if sup.is_null() || child_index < 0 {
        return -1;
    }

    #[expect(
        clippy::cast_sign_loss,
        reason = "child_index is checked to be non-negative"
    )]
    let index = child_index as usize;

    // SAFETY: caller keeps `sup` live through this metadata read.
    let guard = unsafe { &(*sup).roster }.lock_or_recover();
    // SAFETY: the guard protects this scoped roster access.
    let s = &*guard;
    if index >= s.child_count {
        return -1;
    }

    s.child_specs[index].circuit_breaker.state
}

// ---------------------------------------------------------------------------
// Dynamic Supervision — Add/Remove Children at Runtime
// ---------------------------------------------------------------------------

/// Register a suspending `await_restart sup.child`.
///
/// Returns [`RESTART_AWAIT_READY`] when the child slot is already Live (no wait
/// needed) OR permanently Dead (`SupervisorShutdown` / `UnknownSlot` /
/// `BudgetExhausted` — will never restart, so the caller fails closed on resume
/// rather than parking forever, the R4 contract). Returns
/// [`RESTART_AWAIT_SUSPEND`] after parking the continuation as a restart
/// observer when the slot is Transient (mid-restart / backoff / circuit-open).
/// The caller MUST `coro.suspend` on SUSPEND and bind (re-fetch) on READY /
/// resume.
///
/// This is the COOPERATIVE analogue of [`hew_supervisor_restart_await_blocking`];
/// it never thread-blocks the single scheduler. `key` is the static-child slot
/// index.
///
/// # Safety
///
/// - `sup` must be a valid pointer returned by [`hew_supervisor_new`].
/// - `actor` is the awaiting actor (`hew_actor_self`).
/// - `slot` is a live read slot the caller created and holds the creator ref to.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_restart_await_suspend(
    sup: *mut HewSupervisor,
    key: u32,
    actor: *mut HewActor,
    slot: *mut crate::read_slot::HewReadSlot,
) -> i32 {
    if sup.is_null() || slot.is_null() {
        crate::set_last_error(
            "C-ABI guard failed: sup/slot null in hew_supervisor_restart_await_suspend",
        );
        // Fail closed: report READY so the caller binds immediately rather than
        // parking forever; the bind re-fetch fails closed on a dead slot.
        return RESTART_AWAIT_READY;
    }

    // Snapshot the restart epoch BEFORE the pre-park check. `notify_restart`
    // bumps it before it drains waiters (under `restart_await_waiters`), so
    // re-reading it inside the registration critical section detects a restart
    // that completed in the gap between the pre-park check and the push — the
    // lost-wakeup guard (mirrors the `baseline` discipline in
    // `hew_supervisor_restart_await_blocking`).
    // SAFETY: the caller keeps `sup` live through this inline-field read.
    let baseline = *unsafe { &(*sup).restart_epoch }.0.lock_or_recover();

    // SAFETY: the caller keeps `sup` live through this inline-field read.
    let owner = unsafe { (*sup).local_pid_id };
    let role = RoleKey {
        owner,
        slot: key,
        nested: false,
    };

    // Pre-park state check (R4 / issue #2124): inspect the current slot before
    // parking so a settled role resumes immediately and a permanently-Dead child
    // never hangs. Same settlement contract as the blocking barrier: Dead (2) →
    // SupervisorShutdown / UnknownSlot / BudgetExhausted, will never restart, so
    // fail closed and resume (the bind re-fetch surfaces the dead slot
    // recoverably rather than hanging — R4); Live (0) with no fault pending
    // under the role → nothing to wait for. A Transient slot, and a Live slot
    // with an open record, park.
    // SAFETY: `sup`/`key` are the FFI contract; `child_get` does its own guards.
    let current = unsafe { hew_supervisor_child_get(sup, key) };
    if current.tag == 2
        || (role_holds_running_incarnation(sup, key, false)
            && !crate::exit_status::role_has_unsettled_fault(role))
    {
        return RESTART_AWAIT_READY;
    }

    // Test-only: deterministically drive the racing restart cycle here, in the
    // gap the lost-wakeup window opens. No-op in production builds.
    #[cfg(all(test, not(target_arch = "wasm32")))]
    fire_restart_await_park_gap_hook();

    // Park under the waiters lock, but first re-check whether the restart already
    // landed in the gap above (the lost-wakeup race the multi-worker scheduler
    // makes reachable). Holding `restart_await_waiters` while we re-read the
    // counter is the synchronization edge: `notify_restart` bumps the counter
    // before it acquires `restart_await_waiters` to drain, so if its drain already
    // ran (finding our waiter absent), the bump it performed is visible here and
    // we resolve READY instead of parking against a wake that already fired.
    // SAFETY: caller keeps `sup` live while its waiter registry is updated.
    let mut waiters = unsafe { &(*sup).restart_await_waiters }.lock_or_recover();
    // SAFETY: the caller keeps `sup` live through this inline-field read.
    let advanced = *unsafe { &(*sup).restart_epoch }.0.lock_or_recover() != baseline;
    // A terminal ruling wakes waiters WITHOUT advancing the epoch, and publishes
    // the Dead state before it drains. Re-reading the slot here closes the same
    // gap for that path, and re-reading the role's pending faults closes it for
    // a record that settled in the gap.
    // SAFETY: `sup`/`key` are the FFI contract; `child_get` does its own guards.
    let settled = unsafe { hew_supervisor_child_get(sup, key) };
    if advanced
        || settled.tag == 2
        || (role_holds_running_incarnation(sup, key, false)
            && !crate::exit_status::role_has_unsettled_fault(role))
    {
        // The restart cycle completed, the supervisor ruled the slot spent, or
        // the role's last fault settled since the pre-park snapshot. The wake we
        // would park against has already fired against an empty registry;
        // resolve READY and let the bind re-fetch resolve the now-settled slot
        // rather than hang forever.
        drop(waiters);
        return RESTART_AWAIT_READY;
    }
    // Park: the observer takes an in-flight ref so the wake cannot free the slot
    // out from under the abandon edge.
    // SAFETY: caller holds the creator ref, so the slot is live to retain.
    unsafe { crate::read_slot::read_slot_retain(slot) };
    // SAFETY: `actor` is the awaiting actor, live for this registration.
    let actor = unsafe { ActorIncarnation::of(actor) };
    waiters.push(RestartAwaitWaiter { actor, slot });
    drop(waiters);
    RESTART_AWAIT_SUSPEND
}

/// Detach an abandoned suspending `await_restart` (the codegen abandon edge).
///
/// Removes the waiter from `restart_await_waiters` if still registered and
/// releases the observer's retained in-flight ref on the slot. If the waiter
/// already fired (drained by `notify_restart`), this is a no-op for the registry
/// and the ref was already released by the fire path — so it does NOT
/// double-free: the lookup-and-remove is the single authority that the ref is
/// still held here.
///
/// # Safety
///
/// - `sup` must be a valid pointer returned by [`hew_supervisor_new`].
/// - `slot` is the read slot handed to [`hew_supervisor_restart_await_suspend`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_restart_await_detach(
    sup: *mut HewSupervisor,
    slot: *mut crate::read_slot::HewReadSlot,
) {
    if sup.is_null() || slot.is_null() {
        return;
    }
    // SAFETY: caller keeps `sup` live while its waiter registry is updated.
    let mut waiters = unsafe { &(*sup).restart_await_waiters }.lock_or_recover();
    if let Some(pos) = waiters.iter().position(|w| w.slot == slot) {
        waiters.swap_remove(pos);
        drop(waiters);
        // Cancel the slot so a racing wake drops, then release the retained ref.
        // SAFETY: the observer held this ref; removing the waiter is the single
        // authority that it is still live to release here.
        unsafe { crate::read_slot::hew_read_slot_free(slot) };
    }
    // If not found, the waiter already fired and released its ref — no-op.
}

/// Blocking `await_restart` for a CONTEXTLESS caller (`main` / a free fn with
/// no parkable coroutine continuation). Blocks the calling thread until the role
/// is SETTLED, then returns. The contextless analogue of
/// [`hew_supervisor_restart_await_suspend`].
///
/// A role is settled when it reads Dead — nothing will ever refill it, so the
/// caller's re-fetch fails closed — or when it holds a RUNNING incarnation with
/// no fault still pending under it. The first half covers a `close(role)`,
/// which opens no fault record: the slot keeps the stopped occupant until the
/// ruling replaces it, and that occupant's own terminal state says so. An empty
/// slot is a ruling in flight and is never settled. "Pending" is the fact the
/// crash site opened
/// before it woke anybody: a supervised crash opens its record at the crash
/// site and attributes it to the crashing child's role and every ancestor role,
/// and the ruling that settles the record clears the attribution. So the
/// barrier resolves on supervision facts, never on a clock.
///
/// That makes the two shapes that matter deterministic under any load. A caller
/// whose own completion call returned `Err` has the record open by construction
/// (it is opened before the terminal wake that produces that `Err`), so
/// `let _ = child.fail(); await_restart child` waits for the ruling. A healthy
/// role with nothing pending returns at once.
///
/// The one shape it does not cover, by ratified design: a fault from a one-way
/// `mailbox(...)` submission that has not been processed when the barrier is
/// entered has no record yet, so there is nothing to wait on.
///
/// This is safe to thread-block ONLY off the cooperative scheduler: `main` runs
/// on its own thread while the supervisor fires restarts on scheduler worker
/// threads, so there is no self-deadlock (unlike an actor handler, which MUST
/// use the suspending observer). Codegen routes a `Default`-callconv
/// `await_restart` here exactly as it routes a contextless `await` to a blocking
/// ask.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_restart_await_blocking(sup: *mut HewSupervisor, key: u32) {
    // SAFETY: forward the caller's live supervisor contract.
    unsafe { supervisor_restart_await_blocking(sup, key, false) };
}

/// Whether the role's slot holds an incarnation that is still running.
///
/// ONE roster acquisition answers both halves, because they race each other.
/// The ruling empties the slot under `roster` and refills it under `roster`, so
/// asking "is the slot occupied?" and "is its occupant terminal?" in two
/// separate critical sections can see the crashed occupant in the first and an
/// empty slot in the second, and read the pair as a settled role.
///
/// An EMPTY slot is not settled: a ruling is mid-flight, and the restart or the
/// spent mark that ends it wakes the barrier. An occupant that has already gone
/// terminal is not settled either. `hew_supervisor_child_get` and
/// `hew_supervisor_nested_get` classify it Live because that is what a send
/// re-resolves, and a send to a terminal incarnation fails closed on its own;
/// the barrier needs the stricter question, because "the slot still holds the
/// thing I just stopped" is exactly what it has to wait out, and a
/// `close(role)` opens no fault record to say so.
///
/// An actor slot is read under `roster`, which owns the pointer: a ruling takes
/// the occupant out of the slot under the same lock before freeing it. A nested
/// supervisor slot is NOT that: a nested supervisor is reclaimed through its own
/// token teardown, and the parent's slot is only cleared later, when the parent
/// dispatches `ChildSupervisorStopped`. Its stable token, resolved through the
/// local-handle registry, is the one authority for that allocation's lifetime,
/// so the barrier pins the token rather than dereferencing the cached pointer.
fn role_holds_running_incarnation(sup: *mut HewSupervisor, key: u32, nested: bool) -> bool {
    let index = key as usize;
    if nested {
        let entry = {
            // SAFETY: the caller keeps `sup` live for this lookup.
            let roster = unsafe { &(*sup).roster }.lock_or_recover();
            roster
                .child_supervisors
                .get(index)
                .copied()
                .zip(roster.child_supervisor_tokens.get(index).copied())
        };
        let Some((child, token)) = entry else {
            return false;
        };
        let Some(pin) = crate::lifetime::local_handles::pin_current_supervisor(token) else {
            return false;
        };
        if pin.supervisor() != child {
            return false;
        }
        // SAFETY: the pin holds this nested supervisor's allocation live.
        unsafe {
            !(*child).cancelled.load(Ordering::Acquire)
                && (*child).running.load(Ordering::Acquire) != 0
        }
    } else {
        // SAFETY: the caller keeps `sup` live for this lookup.
        let roster = unsafe { &(*sup).roster }.lock_or_recover();
        let Some(child) = roster.children.get(index).copied() else {
            return false;
        };
        if child.is_null() {
            return false;
        }
        // SAFETY: the roster guard owns this child pointer.
        let state = unsafe { &*child }.actor_state.load(Ordering::Acquire);
        state != HewActorState::Stopped as i32 && state != HewActorState::Crashed as i32
    }
}

pub(crate) unsafe fn supervisor_restart_await_blocking(
    sup: *mut HewSupervisor,
    key: u32,
    nested: bool,
) {
    if sup.is_null() {
        return;
    }
    // SAFETY: the caller keeps the allocation live for the whole wait.
    let owner = unsafe { (*sup).local_pid_id };
    let role = RoleKey {
        owner,
        slot: key,
        nested,
    };

    loop {
        // Snapshot the supervision generation BEFORE reading the slot. A
        // transition published in the gap between that read and the wait then
        // reads as a change rather than a wake nobody was there to receive.
        let seen = crate::exit_status::supervision_generation();
        let current = if nested {
            // SAFETY: the caller retains the supervisor while waiting.
            unsafe { hew_supervisor_nested_get(sup, key) }
        } else {
            // SAFETY: the caller retains the supervisor while waiting.
            unsafe { hew_supervisor_child_get(sup, key) }
        };
        // Dead (2): permanent — never restarts. Fail closed: return now.
        if current.tag == 2 {
            return;
        }
        let live = role_holds_running_incarnation(sup, key, nested);
        if crate::exit_status::role_barrier_outcome(role, live, seen)
            == crate::exit_status::RoleBarrier::Settled
        {
            return;
        }
    }
}

// ── Restart observation (deterministic testing) ─────────────────────────────

/// Block until the supervisor's restart epoch reaches at least `target`, or
/// `timeout_ms` milliseconds elapse.
///
/// Returns the current epoch on success, or `0` on timeout. The epoch starts at
/// 0 when the supervisor is constructed, counts completed restart cycles
/// (including the cycle that exhausts the budget), and never resets.
///
/// Test-support only — reads the same `restart_epoch` counter/Condvar the
/// contextless blocking `await_restart` path
/// ([`hew_supervisor_restart_await_blocking`]) synchronizes on, so it is not a
/// second authority for restart completion. Not part of the C ABI: no
/// `#[no_mangle]`, no entry in `scripts/cabi-surface.json` or
/// `scripts/runtime-export-classification.toml`. Callers are Rust test code in
/// this workspace (`hew-runtime/tests/*.rs`, this module's own unit tests),
/// never generated or native code.
///
/// # Safety
///
/// `sup` must be a valid pointer returned by [`hew_supervisor_new`].
#[doc(hidden)]
pub unsafe fn test_wait_for_restart(
    sup: *mut HewSupervisor,
    target: usize,
    timeout_ms: u64,
) -> usize {
    let target = u64::try_from(target).unwrap_or(u64::MAX);
    // SAFETY: the caller keeps the allocation live for the whole wait.
    let (lock, cv) = unsafe { &(*sup).restart_epoch };
    let timeout = std::time::Duration::from_millis(timeout_ms);
    let deadline = std::time::Instant::now() + timeout;
    let mut count = lock.lock_or_recover();
    while *count < target {
        let remaining = deadline.saturating_duration_since(std::time::Instant::now());
        if remaining.is_zero() {
            return 0;
        }
        let (guard, wait_result) = cv.wait_timeout_or_recover(count, remaining);
        count = guard;
        if wait_result.timed_out() && *count < target {
            return 0;
        }
    }
    usize::try_from(*count).unwrap_or(usize::MAX)
}

// ---------------------------------------------------------------------------
// Pool slot substrate — Phase 2.0.b
// ---------------------------------------------------------------------------
