//! Actor spawn machinery: opts, arena allocation, and the `hew_actor_spawn*` C ABI.
#[allow(
    clippy::wildcard_imports,
    reason = "submodule split from a single large file; re-exports its parent scope"
)]
use super::*;

/// Actor spawn options for [`hew_actor_spawn_opts`].
#[repr(C)]
#[derive(Debug)]
pub struct HewActorOpts {
    /// Pointer to initial state (deep-copied).
    pub init_state: *mut c_void,
    /// Size of `init_state` in bytes.
    pub state_size: usize,
    /// Dispatch function.
    pub dispatch: Option<HewDispatchFn>,
    /// Mailbox capacity (`-1` or `0` = unbounded).
    pub mailbox_capacity: i32,
    /// Overflow policy (see [`HewOverflowPolicy`]).
    pub overflow: i32,
    /// Optional coalesce key function.
    pub coalesce_key_fn: Option<unsafe extern "C" fn(i32, *mut c_void, usize) -> u64>,
    /// Fallback policy used when coalescing finds no key match.
    pub coalesce_fallback: i32,
    /// Messages per activation (`0` = default).
    pub budget: i32,
    /// Per-actor arena cap in bytes (`0` = unbounded, same as `hew_arena_new`).
    ///
    /// Non-zero values cause [`hew_actor_spawn_opts`] to call
    /// `hew_arena_new_with_cap(arena_cap_bytes)` instead of `hew_arena_new()`.
    /// Set from the `#[max_heap(N)]` actor attribute; callers that do not use
    /// the attribute must supply `0`.
    pub arena_cap_bytes: usize,
    /// Non-zero when the checker determined this actor participates in an
    /// actor-ref cycle. Future consumer: cycle-detection / Machine Lane B
    /// cycle handling.
    pub cycle_capable: i32,
    /// Typed destructor for queued message payloads evicted before dispatch.
    pub message_drop_fn: Option<unsafe extern "C" fn(i32, *mut c_void, usize)>,
}

fn parse_overflow_policy(policy: i32) -> HewOverflowPolicy {
    match policy {
        x if x == HewOverflowPolicy::Block as i32 => HewOverflowPolicy::Block,
        x if x == HewOverflowPolicy::DropOld as i32 => HewOverflowPolicy::DropOld,
        x if x == HewOverflowPolicy::Fail as i32 => HewOverflowPolicy::Fail,
        x if x == HewOverflowPolicy::Coalesce as i32 => HewOverflowPolicy::Coalesce,
        _ => HewOverflowPolicy::DropNew,
    }
}

// ── Spawn ───────────────────────────────────────────────────────────────
// All spawn functions use native mailbox/scheduler and are not available on WASM.
// WASM actors are created through the bridge module instead.

fn actor_state_malloc(size: usize) -> *mut c_void {
    #[cfg(all(test, not(target_arch = "wasm32")))]
    {
        if should_fail_actor_state_alloc() {
            return ptr::null_mut();
        }
    }

    // SAFETY: `size` is forwarded to libc unchanged.
    crate::mem::buf_try_alloc(size)
}

/// Deep-copy `src` into a new sized-block buffer.
///
/// Returns null if `src` is null, `size` is 0, or allocation fails.
/// On allocation failure, sets `hew_last_error` with the details.
///
/// # Safety
///
/// `src` must point to at least `size` readable bytes.
pub(crate) unsafe fn deep_copy_state(src: *mut c_void, size: usize) -> *mut c_void {
    if src.is_null() || size == 0 {
        return ptr::null_mut();
    }
    // SAFETY: Caller guarantees `src` is readable for `size` bytes.
    unsafe {
        let dst = actor_state_malloc(size);
        if dst.is_null() {
            crate::set_last_error(format!(
                "OOM: failed to allocate {size} bytes for actor state copy"
            ));
            return ptr::null_mut();
        }
        ptr::copy_nonoverlapping(src.cast::<u8>(), dst.cast::<u8>(), size);
        dst
    }
}

/// Configuration for the internal actor spawn helper.
///
/// All three public spawn functions build one of these and delegate to
/// [`spawn_actor_internal`].
/// A native crash hook borrows initialized state and a managed diagnostic.
pub type HewNativeCrashFn =
    unsafe extern "C" fn(*mut c_void, i64, *const hew_cabi::string::HewString) -> i32;

struct ActorSpawnConfig {
    native_crash: Option<HewNativeCrashFn>,
    native_state_release: Option<hew_cabi::value::HewValueReleaseStart>,
    /// The generated `#[on(stop)]` sequence as a resumable continuation that
    /// terminal cleanup runs on the live state before releasing it.
    native_stop_release: Option<hew_cabi::value::HewValueReleaseStart>,
    rejected_state_release: *mut *mut crate::release_walker::HewReleaseCursor,
    dispatch_ownership: HewDispatchOwnership,
    /// The generated `#[on(stop)]` sequence, installed before publication so
    /// no stop request can observe an actor without its hooks.
    terminate_fn: Option<unsafe extern "C-unwind" fn(*mut c_void)>,
    state_drop_fn: Option<unsafe extern "C" fn(*mut c_void)>,
    state_clone_fn: Option<HewStateCloneFn>,
    state: *mut c_void,
    state_size: usize,
    dispatch: Option<HewDispatchFn>,
    sys_dispatch: Option<HewSysDispatchFn>,
    mailbox: *mut c_void,
    budget: i32,
    coalesce_key_fn: Option<unsafe extern "C" fn(i32, *mut c_void, usize) -> u64>,
    /// Checker-derived cycle capability for future Machine Lane B handling.
    #[allow(
        dead_code,
        reason = "receiver-side ABI bit is staged for the Machine Lane B cycle-detection consumer"
    )]
    cycle_capable: bool,
    /// Arena cap in bytes. `0` = unbounded (calls `hew_arena_new`).
    /// Non-zero calls `hew_arena_new_with_cap(cap_bytes)`.
    cap_bytes: usize,
    /// When true, `spawn_actor_internal` adopts `state` as a pre-built
    /// `malloc`-compatible clone (set by [`hew_actor_spawn_opts_adopt`]) and
    /// skips the second `deep_copy_state` that would create `init_state`.
    /// The actor's `init_state` slot is left null: the supervisor's spec
    /// holds the source-of-truth clone template, and direct-spawn restart
    /// paths must re-allocate via `state_clone_fn` rather than reading
    /// `actor.init_state`. Required to avoid byte-aliasing the cloned
    /// wrapper's owned fields with a sibling `init_state` byte-copy (C1).
    adopt: bool,
}

unsafe fn free_spawn_mailbox(mailbox: *mut c_void) {
    let mb = mailbox.cast::<HewMailbox>();
    if !mb.is_null() {
        // SAFETY: `mb` came from the native mailbox constructors used by spawn.
        unsafe { mailbox::hew_mailbox_free(mb) };
    }
}

/// Release spawn-owned inputs when actor construction fails before tracking.
///
/// # Safety
///
/// - `config.state` and `init_state` must be allocations owned by the spawn path,
///   or null.
/// - `config.mailbox` must be a mailbox pointer transferred to the spawn path, or null.
unsafe fn cleanup_failed_spawn(config: &ActorSpawnConfig, init_state: *mut c_void) {
    // SAFETY: caller guarantees these pointers are owned by the in-progress spawn.
    unsafe {
        if let Some(start) = config.native_state_release {
            assert!(!config.rejected_state_release.is_null());
            config
                .rejected_state_release
                .write(crate::release_walker::HewReleaseCursor::payload(
                    config.state,
                    config.state_size,
                    start,
                    true,
                ));
        } else {
            if !config.state.is_null() {
                if let Some(drop) = config.state_drop_fn {
                    drop(config.state);
                }
            }
            crate::mem::buf_free(config.state);
        }
        if !init_state.is_null() {
            crate::mem::buf_free(init_state);
        }
        free_spawn_mailbox(config.mailbox);
    }
}

/// A freshly allocated actor identity: the packed, location-transparent `id`
/// (masked serial) plus the full un-masked `serial` used as the aliasing-proof
/// incarnation discriminator (see [`HewActor::spawn_serial`]).
#[derive(Clone, Copy)]
struct SpawnIdentity {
    id: u64,
    serial: u64,
}

/// Allocate the next spawn identity, or `None` when the serial space is
/// exhausted.
///
/// Exhaustion is a hard refusal, not a wrap: the packed `id` masks the serial to
/// 48 bits, so the allocation after `MAX_ACTOR_SERIAL` would mint PID `0` (the
/// invalid-actor sentinel) and every one after that would alias an id already
/// issued. The caller turns the `None` into a failed spawn.
///
/// The test override is checked FIRST and is deliberately not validated: its
/// whole purpose is to fabricate the out-of-range serial that proves the
/// downstream identity checks refuse an aliased `id` (`supervisor.rs`
/// `role_ask_masked_id_alias_refuses_closed_never_enqueues`).
fn next_spawn_actor_identity() -> Option<SpawnIdentity> {
    #[cfg(test)]
    if let Some((id, serial)) = NEXT_SPAWN_ACTOR_ID_OVERRIDE.with(Cell::take) {
        return Some(SpawnIdentity { id, serial });
    }
    let serial = allocate_actor_serial()?;
    Some(SpawnIdentity {
        id: crate::pid::next_actor_id(serial)?,
        serial,
    })
}

#[expect(
    clippy::needless_pass_by_value,
    reason = "config is a lightweight aggregate of Copy fields; consuming it reads clearly at call sites"
)]
fn build_spawned_actor(
    config: ActorSpawnConfig,
    identity: SpawnIdentity,
    init_state: *mut c_void,
    arena: *mut crate::arena::ActorArena,
) -> Box<HewActor> {
    Box::new(HewActor {
        dispatch_ownership: config.dispatch_ownership,
        sched_link_next: AtomicPtr::new(ptr::null_mut()),
        id: identity.id,
        state: config.state,
        state_size: config.state_size,
        dispatch: config.dispatch,
        mailbox: config.mailbox,
        actor_state: AtomicI32::new(HewActorState::Idle as i32),
        budget: AtomicI32::new(config.budget),
        init_state,
        init_state_size: config.state_size,
        coalesce_key_fn: config.coalesce_key_fn,
        terminate_fn: config.terminate_fn,
        state_drop_fn: config.state_drop_fn,
        state_clone_fn: config.state_clone_fn,
        terminate_called: AtomicBool::new(false),
        terminate_finished: AtomicBool::new(false),
        dispatch_active: AtomicBool::new(false),
        error_code: AtomicI32::new(0),
        supervisor: ptr::null_mut(),
        supervisor_child_index: -1,
        priority: AtomicI32::new(HEW_PRIORITY_NORMAL),
        reductions: AtomicI32::new(HEW_DEFAULT_REDUCTIONS),
        idle_count: AtomicI32::new(0),
        hibernation_threshold: AtomicI32::new(0),
        hibernating: AtomicI32::new(0),
        prof_messages_processed: AtomicU64::new(0),
        prof_processing_time_ns: AtomicU64::new(0),
        arena,
        suspended_cont: AtomicPtr::new(ptr::null_mut()),
        cont_tag: AtomicI32::new(crate::internal::types::ContTag::Empty as i32),
        pending_wake: AtomicBool::new(false),
        suspended_reply_channel: AtomicPtr::new(std::ptr::null_mut()),
        suspended_cancel_token: AtomicPtr::new(std::ptr::null_mut()),
        // Stamp the spawning runtime's id. Native resolves the runtime this
        // spawn runs under (TLS-first via `rt_current`); single-runtime
        // programs always resolve `RuntimeId::DEFAULT`. The wasm cooperative
        // runtime is single-runtime by construction and has no `rt_current`,
        // so it stamps `DEFAULT` directly.
        runtime_id: owning_runtime_id(),
        #[cfg(not(target_arch = "wasm32"))]
        runtime: crate::runtime::rt_current() as *const crate::runtime::RuntimeInner,
        send_pin_count: AtomicU32::new(0),
        gen_sink: AtomicPtr::new(ptr::null_mut()),
        local_pid_id: crate::lifetime::local_handles::HewLocalPidId::INVALID,
        spawn_serial: identity.serial,
        sys_dispatch: config.sys_dispatch,
        state_drop_consumed: AtomicBool::new(false),
        state_drop_borrowed: AtomicBool::new(false),
        parked_ask_channel: AtomicPtr::new(std::ptr::null_mut()),
        checked_invocation: AtomicPtr::new(std::ptr::null_mut()),
        pending_external_trap_code: AtomicI32::new(0),
        native_completion: (config.dispatch_ownership == HewDispatchOwnership::UniqueEnvelope)
            .then(|| {
                let completion =
                    crate::actor_native::NativeActorCompletion::with_crash(config.native_crash);
                completion
                    .cleanup
                    .set_state_release(config.native_state_release);
                completion
                    .cleanup
                    .set_stop_release(config.native_stop_release);
                std::sync::Arc::new(completion)
            }),
    })
}

unsafe fn finalize_spawned_actor(raw: *mut HewActor, actor_id: u64) -> bool {
    // The local-PID control table lives on the runtime instance, which is a
    // native authority; `hew_local_pid_*` is gated off wasm32, so nothing there
    // resolves a token and the actor publishes its liveness directly.
    #[cfg(not(target_arch = "wasm32"))]
    {
        // SAFETY: same caller guarantee; `runtime_id` is initialized before track.
        let runtime_id = unsafe { (*raw).runtime_id };
        // SAFETY: native spawn stamps this non-null owner from the same runtime
        // read as `runtime_id`; the owner outlives actor construction and routes.
        let owner = unsafe { &*(*raw).runtime };
        let publication = match crate::lifetime::local_handles::begin_actor_publication_in(
            &owner.local_handles,
        ) {
            Ok(publication) => publication,
            Err(error) => {
                crate::set_last_error(format!(
                    "hew_actor_spawn: local handle publication failed: {error:?}"
                ));
                return false;
            }
        };
        let token = match publication.register_actor(runtime_id, actor_id) {
            Ok(token) => token,
            Err(error) => {
                crate::set_last_error(format!(
                    "hew_actor_spawn: local handle allocation failed: {error:?}"
                ));
                return false;
            }
        };
        // Initialise the complete semantic identity before publishing liveness.
        // SAFETY: raw is owned by this unpublished spawn and fully initialised.
        unsafe { (*raw).local_pid_id = token };
        #[cfg(test)]
        run_spawn_publication_hook();
        // SAFETY: caller guarantees raw is valid and fully initialised.
        if !unsafe { live_actors::track_actor(raw) } {
            let retired = publication.retire_actor(token, actor_id);
            debug_assert_eq!(
                retired,
                crate::lifetime::local_handles::RetireActorResult::Retired
            );
            crate::set_last_error(format!(
                "hew_actor_spawn: actor identity collision: {actor_id}"
            ));
            return false;
        }
    }
    #[cfg(target_arch = "wasm32")]
    {
        // SAFETY: same caller guarantee; `runtime_id` is initialized before track.
        let runtime_id = unsafe { (*raw).runtime_id };
        let publication = match crate::lifetime::local_handles::begin_current_actor_publication() {
            Ok(publication) => publication,
            Err(error) => {
                crate::set_last_error(format!(
                    "hew_actor_spawn: local handle publication failed: {error:?}"
                ));
                return false;
            }
        };
        let token = match publication.register_actor(runtime_id, actor_id) {
            Ok(token) => token,
            Err(error) => {
                crate::set_last_error(format!(
                    "hew_actor_spawn: local handle allocation failed: {error:?}"
                ));
                return false;
            }
        };
        // SAFETY: raw is owned by this unpublished spawn and fully initialised.
        unsafe { (*raw).local_pid_id = token };
        // SAFETY: caller guarantees raw is valid and fully initialised.
        if !unsafe { live_actors::track_actor(raw) } {
            let retired = publication.retire_actor(token, actor_id);
            debug_assert_eq!(
                retired,
                crate::lifetime::local_handles::RetireActorResult::Retired
            );
            crate::set_last_error(format!(
                "hew_actor_spawn: actor identity collision: {actor_id}"
            ));
            return false;
        }
    }
    #[cfg(feature = "profiler")]
    // SAFETY: `raw` was just allocated by `Box::into_raw` and is valid.
    unsafe {
        crate::profiler::actor_registry::register(raw);
    };
    crate::tracing::hew_trace_lifecycle(actor_id, crate::tracing::SPAN_SPAWN);
    true
}

/// Allocate the per-actor arena for a native spawn.
///
/// Centralises the `cap_bytes == 0` / `cap_bytes > 0` branch and provides a
/// test-only injection point: when `FAIL_ARENA_ALLOC_NEXT` is set, the first
/// call returns null to simulate an OOM on the arena allocation step.
fn alloc_actor_arena(cap_bytes: usize) -> *mut crate::arena::ActorArena {
    #[cfg(test)]
    if should_fail_arena_alloc() {
        return ptr::null_mut();
    }
    if cap_bytes > 0 {
        crate::arena::hew_arena_new_with_cap(cap_bytes)
    } else {
        crate::arena::hew_arena_new()
    }
}

/// Shared implementation for all native actor spawn functions.
///
/// # Safety
///
/// - `config.state` must be a deep-copied allocation (or null for zero-sized state).
/// - `config.mailbox` must be a valid mailbox pointer (already configured).
unsafe fn spawn_actor_internal(config: ActorSpawnConfig) -> *mut HewActor {
    // Adopt-state path (set by hew_actor_spawn_opts_adopt): `config.state` is
    // already an independently-owned deep clone (state_clone_fn output); skip
    // the second `deep_copy_state` and leave `init_state` null so the spec's
    // clone-template is the sole source-of-truth for future restarts. Avoids
    // byte-aliasing the cloned wrapper's owned fields with a sibling
    // `init_state` byte-copy (root cause of C1).
    let init_state = if config.adopt {
        ptr::null_mut()
    } else {
        // SAFETY: Caller already deep-copied state; make a second copy for restart.
        unsafe { deep_copy_state(config.state, config.state_size) }
    };

    // OOM on the restart-state copy: free resources the caller transferred
    // ownership of and propagate the failure as null. Skipped on the adopt
    // path because we never allocated `init_state`.
    if !config.adopt && !config.state.is_null() && config.state_size > 0 && init_state.is_null() {
        // SAFETY: `config` still owns the transferred state/mailbox on this failure path.
        unsafe { cleanup_failed_spawn(&config, ptr::null_mut()) };
        return ptr::null_mut();
    }

    // Allocate the per-actor arena bump allocator.  Mirror the wasm path:
    // if allocation fails, free all resources already owned and return null.
    let arena = alloc_actor_arena(config.cap_bytes);
    if arena.is_null() {
        // SAFETY: `init_state` was created above and ownership has not been
        // transferred.  `cleanup_failed_spawn` also frees `config.state` and
        // the mailbox; on the adopt path `init_state` is null (no extra alloc).
        unsafe { cleanup_failed_spawn(&config, init_state) };
        return ptr::null_mut();
    }
    let Some(identity) = next_spawn_actor_identity() else {
        crate::set_last_error(
            "hew_actor_spawn: actor serial space exhausted; refusing to mint an aliased actor id",
        );
        // SAFETY: the arena was allocated above and its ownership has not been
        // transferred to an actor.
        unsafe { crate::arena::hew_arena_free_all(arena) };
        // SAFETY: `config` still owns the transferred state/mailbox on this failure path.
        unsafe { cleanup_failed_spawn(&config, init_state) };
        return ptr::null_mut();
    };
    let rejected_state_release = config.rejected_state_release;
    let state_release = config.native_state_release;
    let actor = build_spawned_actor(config, identity, init_state, arena);
    let raw = Box::into_raw(actor);
    // The single site that mints an actor box. Counted here, at the allocation
    // itself, so the balance in `actor_balance` is over the boxes the runtime
    // actually handed out (see that module for why exit status alone cannot
    // see a leaked actor).
    crate::actor_balance::record_actor_box_alloc();
    register_actor_state_lock(raw);
    // SAFETY: `raw` comes from `Box::into_raw` and has not yet been tracked.
    if !unsafe { finalize_spawned_actor(raw, identity.id) } {
        // SAFETY: registration failed after liveness was rolled back; no caller
        // or scheduler can observe `raw`.
        unsafe {
            if let Some(start) = state_release {
                rejected_state_release.write(crate::release_walker::HewReleaseCursor::payload(
                    (*raw).state,
                    (*raw).state_size,
                    start,
                    true,
                ));
                (*raw).state = ptr::null_mut();
                (*raw).state_drop_consumed.store(true, Ordering::Release);
            }
            free_actor_resources(raw);
        };
        return ptr::null_mut();
    }
    raw
}

/// Spawn a new actor with an unbounded mailbox.
///
/// The initial state is deep-copied. The returned pointer must be freed
/// with [`hew_actor_free`]. Returns null on allocation failure
/// (details via [`hew_last_error`]).
///
/// # Safety
///
/// - `state` must point to at least `state_size` readable bytes, or be
///   null when `state_size` is 0.
/// - `dispatch` will be called from worker threads with the actor's
///   state pointer.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_spawn(
    state: *mut c_void,
    state_size: usize,
    dispatch: Option<HewDispatchFn>,
) -> *mut HewActor {
    // SAFETY: Caller guarantees `state` validity.
    let actor_state = unsafe { deep_copy_state(state, state_size) };
    if !state.is_null() && state_size > 0 && actor_state.is_null() {
        return ptr::null_mut();
    }

    // SAFETY: hew_mailbox_new returns a valid pointer.
    let mailbox = unsafe { mailbox::hew_mailbox_new() };
    // SAFETY: mailbox pointer is valid.
    unsafe {
        mailbox::hew_mailbox_set_coalesce_config(mailbox, None, HewOverflowPolicy::DropOld);
    }

    // SAFETY: actor_state is a fresh deep-copy; mailbox is valid.
    unsafe {
        spawn_actor_internal(ActorSpawnConfig {
            native_crash: None,
            native_state_release: None,
            native_stop_release: None,
            rejected_state_release: ptr::null_mut(),
            dispatch_ownership: HewDispatchOwnership::CopiedPayload,
            terminate_fn: None,
            state_drop_fn: None,
            state_clone_fn: None,
            state: actor_state,
            state_size,
            dispatch,
            sys_dispatch: None,
            mailbox: mailbox.cast(),
            budget: HEW_MSG_BUDGET,
            coalesce_key_fn: None,
            cycle_capable: false,
            cap_bytes: 0,
            adopt: false,
        })
    }
}

/// Spawn a new actor from a [`HewActorOpts`] struct.
///
/// Uses a bounded mailbox if `opts.mailbox_capacity > 0`.
///
/// # Safety
///
/// - `opts` must be a valid pointer to a [`HewActorOpts`].
/// - Same state/dispatch requirements as [`hew_actor_spawn`].
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_spawn_opts(opts: *const HewActorOpts) -> *mut HewActor {
    if opts.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: Caller guarantees `opts` is valid.
    let opts = unsafe { &*opts };

    // SAFETY: Caller guarantees state validity.
    let actor_state = unsafe { deep_copy_state(opts.init_state, opts.state_size) };
    if !opts.init_state.is_null() && opts.state_size > 0 && actor_state.is_null() {
        return ptr::null_mut();
    }

    let mailbox = if opts.mailbox_capacity > 0 {
        let capacity = usize::try_from(opts.mailbox_capacity).unwrap_or(usize::MAX);
        let policy = parse_overflow_policy(opts.overflow);
        // SAFETY: Returns a valid pointer.
        unsafe { mailbox::hew_mailbox_new_with_policy(capacity, policy) }
    } else {
        // SAFETY: Returns a valid pointer.
        unsafe { mailbox::hew_mailbox_new() }
    };
    let coalesce_fallback = parse_overflow_policy(opts.coalesce_fallback);
    // SAFETY: mailbox pointer is valid.
    unsafe {
        mailbox::hew_mailbox_set_coalesce_config(mailbox, opts.coalesce_key_fn, coalesce_fallback);
        mailbox::hew_mailbox_set_message_drop_fn(mailbox, opts.message_drop_fn);
    }

    let budget = if opts.budget > 0 {
        opts.budget
    } else {
        HEW_MSG_BUDGET
    };

    // SAFETY: actor_state is a fresh deep-copy; mailbox is valid.
    unsafe {
        spawn_actor_internal(ActorSpawnConfig {
            native_crash: None,
            native_state_release: None,
            native_stop_release: None,
            rejected_state_release: ptr::null_mut(),
            dispatch_ownership: HewDispatchOwnership::CopiedPayload,
            terminate_fn: None,
            state_drop_fn: None,
            state_clone_fn: None,
            state: actor_state,
            state_size: opts.state_size,
            dispatch: opts.dispatch,
            sys_dispatch: None,
            mailbox: mailbox.cast(),
            budget,
            coalesce_key_fn: opts.coalesce_key_fn,
            cycle_capable: opts.cycle_capable != 0,
            cap_bytes: opts.arena_cap_bytes,
            adopt: false,
        })
    }
}

/// Spawn a new actor that adopts a pre-built deep-clone of its initial state.
///
/// Companion to [`hew_actor_spawn_opts`] for supervisor-restart and other
/// clone-aware spawn paths. The caller passes a freshly heap-allocated state
/// wrapper (`cloned_state`) — typically the return value of the actor's
/// codegen-emitted [`HewStateCloneFn`] — and this function consumes ownership
/// of that allocation, wiring it directly into the new actor's `state` slot
/// **without** an additional `deep_copy_state` (which would byte-alias the
/// cloned wrapper's owned fields and re-introduce the C1 UAF) and **without**
/// allocating an `init_state` byte-copy alongside it.
///
/// `opts.init_state` is ignored on this path; the runtime reads only the
/// scalar opts (`state_size`, `dispatch`, mailbox/overflow/coalesce/budget,
/// arena cap, cycle bit) and the adopted `cloned_state`.
///
/// **Ownership / failure**: on success, the returned actor owns
/// `cloned_state` (released via `state_drop_fn` + `buf_free` at teardown).
/// On failure (null return), this function performs a `buf_free` of
/// `cloned_state`. The caller's `state_drop_fn` is **not** invoked on the
/// failure path, so any owned heap fields inside the wrapper are leaked.
/// This is a known Lane A1 limitation (proper failure-path drop is Lane A3
/// work). Callers wanting safe failure cleanup should null-check the spawn
/// result and call `state_drop_fn(cloned_state); crate::mem::buf_free(cloned_state);`
/// themselves before returning — but they must NOT then call this function
/// (i.e. they must pre-allocate via a probe). In practice the supervisor
/// restart path tolerates the leak because spawn-failure here implies
/// system-wide OOM and the supervisor will escalate.
///
/// Chosen over an `adopt_state: bool` field on `HewActorOpts` because the
/// LLVM codegen at `hew-codegen-rs/src/llvm.rs` builds the opts struct as a
/// fixed-shape 10-field literal — appending an `adopt_state` bit would read
/// uninitialized stack pad from codegen-emitted callers and spuriously
/// consume their borrowed `init_state` pointer (UAF). A separate ABI entry
/// is forward-compatible without touching codegen.
///
/// # Safety
///
/// - `opts` must be a valid pointer to a [`HewActorOpts`].
/// - `cloned_state` must be either null (when `state_size == 0`) or a
///   `malloc`-compatible heap allocation of exactly `opts.state_size` bytes
///   whose owned fields are independent deep clones (the
///   [`HewStateCloneFn`] contract).
/// - After this call returns, the caller MUST NOT free or otherwise touch
///   `cloned_state`; ownership has transferred.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_spawn_opts_adopt(
    opts: *const HewActorOpts,
    cloned_state: *mut c_void,
) -> *mut HewActor {
    if opts.is_null() {
        // Caller still owns `cloned_state`; we have no way to release it
        // safely (no state_drop_fn in scope), so we must not free it here.
        // Returning null with the pointer retained matches the precondition.
        return ptr::null_mut();
    }
    // SAFETY: Caller guarantees `opts` is valid.
    let opts = unsafe { &*opts };

    let mailbox = if opts.mailbox_capacity > 0 {
        let capacity = usize::try_from(opts.mailbox_capacity).unwrap_or(usize::MAX);
        let policy = parse_overflow_policy(opts.overflow);
        // SAFETY: Returns a valid pointer.
        unsafe { mailbox::hew_mailbox_new_with_policy(capacity, policy) }
    } else {
        // SAFETY: Returns a valid pointer.
        unsafe { mailbox::hew_mailbox_new() }
    };
    let coalesce_fallback = parse_overflow_policy(opts.coalesce_fallback);
    // SAFETY: mailbox pointer is valid.
    unsafe {
        mailbox::hew_mailbox_set_coalesce_config(mailbox, opts.coalesce_key_fn, coalesce_fallback);
        mailbox::hew_mailbox_set_message_drop_fn(mailbox, opts.message_drop_fn);
    }

    let budget = if opts.budget > 0 {
        opts.budget
    } else {
        HEW_MSG_BUDGET
    };

    // SAFETY: cloned_state ownership has been transferred to us; mailbox is valid.
    unsafe {
        spawn_actor_internal(ActorSpawnConfig {
            native_crash: None,
            native_state_release: None,
            native_stop_release: None,
            rejected_state_release: ptr::null_mut(),
            dispatch_ownership: HewDispatchOwnership::CopiedPayload,
            terminate_fn: None,
            state_drop_fn: None,
            state_clone_fn: None,
            state: cloned_state,
            state_size: opts.state_size,
            dispatch: opts.dispatch,
            sys_dispatch: None,
            mailbox: mailbox.cast(),
            budget,
            coalesce_key_fn: opts.coalesce_key_fn,
            cycle_capable: opts.cycle_capable != 0,
            cap_bytes: opts.arena_cap_bytes,
            adopt: true,
        })
    }
}

/// One compiler-selected periodic handler, shared by direct and supervised spawn.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct HewNativePeriodicHandler {
    pub message: i32,
    pub interval_ms: u64,
}

/// Publish initialized native state together with both lifetime callbacks.
///
/// # Safety
/// State is a unique malloc allocation of `size` bytes, with initialized
/// fields described by `state_drop` and `state_clone`. Callbacks and dispatch
/// remain valid for the actor's lifetime. The function consumes state on every
/// outcome. `stop_release` is null or the generated `#[on(stop)]` sequence,
/// which terminal cleanup runs once on the live state after a cooperative
/// stop, suspending as its hooks do, before the state is released. `fault`
/// is a writable, initially null fault slot. `periodic` points to
/// `periodic_count` valid descriptors, or is null when the count is zero.
#[no_mangle]
#[allow(
    clippy::too_many_arguments,
    reason = "private generated actor ABI publishes one complete state contract"
)]
pub unsafe extern "C" fn hew_actor_spawn_native(
    state: *mut c_void,
    size: usize,
    dispatch: HewDispatchFn,
    state_drop: unsafe extern "C" fn(*mut c_void),
    state_clone: HewStateCloneFn,
    stop_release: Option<hew_cabi::value::HewValueReleaseStart>,
    capacity: i32,
    overflow: i32,
    cap_bytes: usize,
    periodic: *const HewNativePeriodicHandler,
    periodic_count: usize,
    sys_dispatch: Option<HewSysDispatchFn>,
    native_crash: Option<HewNativeCrashFn>,
    fault: *mut *mut crate::fault::HewFault,
    coalesce_key: Option<mailbox::HewCoalesceKeyFn>,
    coalesce_fallback: i32,
    state_release: Option<hew_cabi::value::HewValueReleaseStart>,
    rejected_state_release: *mut *mut crate::release_walker::HewReleaseCursor,
) -> crate::lifetime::local_handles::HewLocalPidId {
    // SAFETY: generated code supplies an empty cursor output.
    if !rejected_state_release.is_null() {
        // SAFETY: generated code supplies a writable optional cursor output.
        unsafe { rejected_state_release.write(ptr::null_mut()) };
    }
    #[cfg(not(target_arch = "wasm32"))]
    let Ok(_ingress) = crate::shutdown::admit_external_work() else {
        // SAFETY: publication was refused; the caller drains transferred state.
        unsafe {
            if let Some(start) = state_release {
                rejected_state_release.write(crate::release_walker::HewReleaseCursor::payload(
                    state, size, start, true,
                ));
            } else {
                state_drop(state);
                crate::mem::buf_free(state);
            }
            *fault =
                crate::fault::hew_fault_new(crate::internal::types::HEW_TRAP_ACTOR_SEND_FAILED);
        }
        return crate::lifetime::local_handles::HewLocalPidId::INVALID;
    };
    // SAFETY: constructors return an owned native mailbox.
    let mailbox = unsafe {
        if capacity > 0 {
            mailbox::hew_mailbox_new_with_policy(
                usize::try_from(capacity).unwrap_or(usize::MAX),
                parse_overflow_policy(overflow),
            )
        } else {
            mailbox::hew_mailbox_new()
        }
    };
    // Configure coalescing before the actor is published: no sender can reach
    // this mailbox yet, so the key extractor is in place for the first send.
    if coalesce_key.is_some() {
        // SAFETY: the constructor above returned a live mailbox.
        unsafe {
            mailbox::hew_mailbox_set_coalesce_config(
                mailbox,
                coalesce_key,
                parse_overflow_policy(coalesce_fallback),
            );
        }
    }
    // SAFETY: ownership and callbacks are supplied atomically before publication.
    let actor = unsafe {
        spawn_actor_internal(ActorSpawnConfig {
            native_crash,
            native_state_release: state_release,
            native_stop_release: stop_release,
            rejected_state_release,
            dispatch_ownership: HewDispatchOwnership::UniqueEnvelope,
            terminate_fn: None,
            state_drop_fn: Some(state_drop),
            state_clone_fn: Some(state_clone),
            state,
            state_size: size,
            dispatch: Some(dispatch),
            sys_dispatch,
            mailbox: mailbox.cast(),
            budget: HEW_MSG_BUDGET,
            coalesce_key_fn: coalesce_key,
            cycle_capable: false,
            cap_bytes,
            adopt: true,
        })
    };
    if actor.is_null() {
        // SAFETY: the generated caller provides a writable empty fault slot.
        unsafe {
            *fault = crate::fault::hew_fault_new(crate::internal::types::HewError::ErrOom as i32);
        };
        crate::lifetime::local_handles::HewLocalPidId::INVALID
    } else {
        // No external handle exists yet. Pin before the first timer can run,
        // since a periodic handler may immediately fault and retire this actor.
        // SAFETY: successful construction returns a live actor before timer arming.
        let (id, token) = unsafe { ((*actor).id, (*actor).local_pid_id) };
        let armed = live_actors::with_actor_send_by_id(id, |actor| {
            // SAFETY: the compiler supplies this static descriptor slice, and
            // the send guard pins the actor through the complete arming sequence.
            let handlers = if periodic_count == 0 {
                &[]
            } else {
                // SAFETY: the caller supplies this complete static descriptor slice.
                unsafe { std::slice::from_raw_parts(periodic, periodic_count) }
            };
            handlers.iter().all(|handler| {
                // SAFETY: the actor is pinned and each checked interval is positive.
                !unsafe {
                    crate::timer_periodic::hew_actor_schedule_periodic(
                        actor,
                        handler.message,
                        handler.interval_ms,
                    )
                }
                .is_null()
            })
        })
        .unwrap_or(false);
        if armed {
            token
        } else {
            // Stop owns timer cancellation and typed state cleanup, including
            // timers already armed before a later allocation failed.
            crate::actor_native::hew_actor_close_native(token);
            // SAFETY: the caller supplies an empty writable fault slot.
            unsafe {
                *fault =
                    crate::fault::hew_fault_new(crate::internal::types::HewError::ErrOom as i32);
            };
            crate::lifetime::local_handles::HewLocalPidId::INVALID
        }
    }
}

/// Transfer one native message envelope through a stable actor incarnation.
///
/// # Safety
/// `envelope` is uniquely owned and transfers only on successful admission.
pub(crate) unsafe fn try_submit_native_envelope(
    token: crate::lifetime::local_handles::HewLocalPidId,
    message: i32,
    envelope: *mut crate::mailbox::HewMsgEnvelope,
) -> crate::mailbox::SendOutcome {
    // SAFETY: sends have no reply reference; the envelope transfers on admission.
    unsafe { try_submit_native_request(token, message, envelope, std::ptr::null_mut()) }
}

/// Register native capacity readiness while the exact destination is pinned.
pub(crate) fn register_native_capacity(
    token: crate::lifetime::local_handles::HewLocalPidId,
    waker: &std::sync::Arc<crate::wake::OwnedWaker>,
) {
    if let Some(actor_id) = crate::lifetime::local_handles::resolve_current_actor(token) {
        live_actors::with_actor_send_by_id(actor_id, |actor| {
            // SAFETY: the send guard pins the actor and its mailbox during registration.
            let mailbox = unsafe { &*(*actor).mailbox.cast::<HewMailbox>() };
            mailbox.native_capacity.register(waker);
        });
    }
}

/// Submit an envelope and optional reply sender reference to the exact target.
///
/// # Safety
/// Both unpublished references transfer only on successful admission.
pub(crate) unsafe fn try_submit_native_request(
    token: crate::lifetime::local_handles::HewLocalPidId,
    message: i32,
    envelope: *mut crate::mailbox::HewMsgEnvelope,
    reply: *mut c_void,
) -> crate::mailbox::SendOutcome {
    // SAFETY: forwards the uniquely owned request and its optional reply.
    unsafe { submit_native_request(token, message, envelope, reply, false) }
}

pub(crate) unsafe fn submit_native_terminal(
    token: crate::lifetime::local_handles::HewLocalPidId,
    message: i32,
    envelope: *mut crate::mailbox::HewMsgEnvelope,
) -> crate::mailbox::SendOutcome {
    // SAFETY: the caller uniquely owns this terminal event envelope.
    unsafe { submit_native_request(token, message, envelope, std::ptr::null_mut(), true) }
}

unsafe fn submit_native_request(
    token: crate::lifetime::local_handles::HewLocalPidId,
    message: i32,
    envelope: *mut crate::mailbox::HewMsgEnvelope,
    reply: *mut c_void,
    terminal: bool,
) -> crate::mailbox::SendOutcome {
    // Attachment termination is completion of work already admitted by its
    // owner. Ordinary root/external sends must finish publication before drain.
    #[cfg(not(target_arch = "wasm32"))]
    let _ingress = if terminal {
        None
    } else {
        match crate::shutdown::admit_external_work() {
            Ok(permit) => Some(permit),
            Err(()) => return mailbox::SendOutcome::Closed,
        }
    };
    let Some(actor_id) = crate::lifetime::local_handles::resolve_current_actor(token) else {
        return mailbox::SendOutcome::Closed;
    };
    live_actors::with_actor_send_by_id(actor_id, |actor| {
        // SAFETY: this guard pins the exact actor incarnation and its mailbox.
        let a = unsafe { &*actor };
        if actor_send_is_terminal(a) {
            return mailbox::SendOutcome::Closed;
        }
        // Reserve receiver cleanup before queue publication or policy disposal.
        // SAFETY: the send pin retains this receiver and the envelope is unpublished.
        if !unsafe { crate::cow_envelope::bind_receiver(envelope, a) } {
            return mailbox::SendOutcome::Closed;
        }
        // EXIT(drop-fault-injection): the deterministic harness asks us to lose
        // this message. The receiver never consumes it, so this path releases
        // the envelope itself, and the loss reports as a loss rather than as
        // delivery - the same seam the copy-mode send carries.
        if crate::deterministic::check_drop_fault(a.id) {
            // SAFETY: the caller transferred one refcount on `envelope`.
            unsafe { crate::mailbox::hew_msg_envelope_release(envelope) };
            // SAFETY: an unadmitted ask still owns one sender-side reference.
            unsafe { crate::mailbox::retire_orphaned_ask_sender_ref(reply) };
            return mailbox::SendOutcome::Dropped;
        }
        // SAFETY: the pinned mailbox consumes only an admitted envelope.
        let outcome = unsafe {
            if terminal {
                mailbox::admit_native_terminal(&*a.mailbox.cast(), message, envelope)
            } else {
                mailbox::try_admit_native_request(&*a.mailbox.cast(), message, envelope, reply)
            }
        };
        if matches!(
            outcome,
            mailbox::SendOutcome::Failed | mailbox::SendOutcome::Closed | mailbox::SendOutcome::Oom
        ) {
            // SAFETY: refusal preserved the unpublished envelope with its caller.
            unsafe { crate::cow_envelope::unbind_receiver(envelope, a) };
        }
        if matches!(outcome, mailbox::SendOutcome::Enqueued) {
            // SAFETY: a message reached the live, pinned actor's mailbox.
            unsafe { schedule_actor_after_enqueue(actor, a, message) };
        }
        outcome
    })
    .unwrap_or(mailbox::SendOutcome::Closed)
}

/// Spawn a new actor with a bounded mailbox.
///
/// # Safety
///
/// Same requirements as [`hew_actor_spawn`].
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_actor_spawn_bounded(
    state: *mut c_void,
    state_size: usize,
    dispatch: Option<HewDispatchFn>,
    capacity: i32,
) -> *mut HewActor {
    // SAFETY: Caller guarantees `state` validity.
    let actor_state = unsafe { deep_copy_state(state, state_size) };
    if !state.is_null() && state_size > 0 && actor_state.is_null() {
        return ptr::null_mut();
    }

    // SAFETY: Returns a valid pointer.
    let mailbox = unsafe { mailbox::hew_mailbox_new_bounded(capacity) };
    // SAFETY: mailbox pointer is valid.
    unsafe {
        mailbox::hew_mailbox_set_coalesce_config(mailbox, None, HewOverflowPolicy::DropOld);
    }

    // SAFETY: actor_state is a fresh deep-copy; mailbox is valid.
    unsafe {
        spawn_actor_internal(ActorSpawnConfig {
            native_crash: None,
            native_state_release: None,
            native_stop_release: None,
            rejected_state_release: ptr::null_mut(),
            dispatch_ownership: HewDispatchOwnership::CopiedPayload,
            terminate_fn: None,
            state_drop_fn: None,
            state_clone_fn: None,
            state: actor_state,
            state_size,
            dispatch,
            sys_dispatch: None,
            mailbox: mailbox.cast(),
            budget: HEW_MSG_BUDGET,
            coalesce_key_fn: None,
            cycle_capable: false,
            cap_bytes: 0,
            adopt: false,
        })
    }
}
