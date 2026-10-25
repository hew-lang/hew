//! Actor spawn machinery: opts, arena allocation, and the `hew_actor_spawn*` C ABI.
#[allow(
    clippy::wildcard_imports,
    reason = "submodule split from a single large file; re-exports its parent scope"
)]
use super::*;
use crate::util::MutexExt;

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
    pub coalesce_match_fn: Option<mailbox::HewCoalesceMatchFn>,
    /// Fallback policy used when coalescing finds no key match.
    pub coalesce_fallback: i32,
    /// Messages per activation (`0` = default).
    pub budget: i32,
    /// Per-actor arena cap in bytes (`0` = unbounded, same as `hew_arena_new`).
    ///
    /// Non-zero values cause [`hew_actor_spawn_opts`] to call
    /// `hew_arena_new_with_cap(arena_cap_bytes)` instead of `hew_arena_new()`.
    /// This explicit arena limit is separate from managed actor-value budgets.
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

/// A native crash hook borrows retained inputs through its release continuation.
pub type HewNativeCrashFn = hew_cabi::value::HewValueReleaseStart;

/// Default graceful-stop budget shared by declared children and crash hooks.
pub const DEFAULT_STOP_BUDGET_NS: i64 = 5_000_000_000;

/// Configuration shared by the public actor spawn functions.
struct ActorSpawnConfig {
    initial_state: HewActorState,
    native_completion: Option<std::sync::Arc<crate::actor_native::NativeActorCompletion>>,
    native_state_release: Option<hew_cabi::value::HewValueReleaseStart>,
    rejected_state_release: *mut *mut crate::release_walker::HewReleaseCursor,
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
    coalesce_match_fn: Option<mailbox::HewCoalesceMatchFn>,
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

fn build_spawned_actor(
    config: ActorSpawnConfig,
    identity: SpawnIdentity,
    init_state: *mut c_void,
    arena: *mut crate::arena::ActorArena,
) -> Box<HewActor> {
    Box::new(HewActor {
        sched_link_next: AtomicPtr::new(ptr::null_mut()),
        id: identity.id,
        state: config.state,
        state_size: config.state_size,
        dispatch: config.dispatch,
        mailbox: config.mailbox,
        actor_state: AtomicI32::new(config.initial_state as i32),
        budget: AtomicI32::new(config.budget),
        init_state,
        init_state_size: config.state_size,
        coalesce_match_fn: config.coalesce_match_fn,
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
        local_pid_id: crate::lifetime::local_handles::HewLocalPidId::INVALID,
        spawn_serial: identity.serial,
        sys_dispatch: config.sys_dispatch,
        state_drop_consumed: AtomicBool::new(false),
        state_drop_borrowed: AtomicBool::new(false),
        parked_ask_channel: AtomicPtr::new(std::ptr::null_mut()),
        checked_invocation: AtomicPtr::new(std::ptr::null_mut()),
        pending_external_trap_code: AtomicI32::new(0),
        native_completion: config.native_completion,
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
            initial_state: HewActorState::Idle,
            native_completion: None,
            native_state_release: None,
            rejected_state_release: ptr::null_mut(),
            terminate_fn: None,
            state_drop_fn: None,
            state_clone_fn: None,
            state: actor_state,
            state_size,
            dispatch,
            sys_dispatch: None,
            mailbox: mailbox.cast(),
            budget: HEW_MSG_BUDGET,
            coalesce_match_fn: None,
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
        mailbox::hew_mailbox_set_coalesce_config(
            mailbox,
            opts.coalesce_match_fn,
            coalesce_fallback,
        );
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
            initial_state: HewActorState::Idle,
            native_completion: None,
            native_state_release: None,
            rejected_state_release: ptr::null_mut(),
            terminate_fn: None,
            state_drop_fn: None,
            state_clone_fn: None,
            state: actor_state,
            state_size: opts.state_size,
            dispatch: opts.dispatch,
            sys_dispatch: None,
            mailbox: mailbox.cast(),
            budget,
            coalesce_match_fn: opts.coalesce_match_fn,
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
        mailbox::hew_mailbox_set_coalesce_config(
            mailbox,
            opts.coalesce_match_fn,
            coalesce_fallback,
        );
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
            initial_state: HewActorState::Idle,
            native_completion: None,
            native_state_release: None,
            rejected_state_release: ptr::null_mut(),
            terminate_fn: None,
            state_drop_fn: None,
            state_clone_fn: None,
            state: cloned_state,
            state_size: opts.state_size,
            dispatch: opts.dispatch,
            sys_dispatch: None,
            mailbox: mailbox.cast(),
            budget,
            coalesce_match_fn: opts.coalesce_match_fn,
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

/// Reserve actor identity and cleanup before init and start run.
///
/// # Panics
/// Panics if the native actor allocation loses its required completion owner.
///
/// # Safety
/// State is a unique malloc allocation of `size` bytes, with initialized
/// fields described by `state_drop` and `state_clone`. Callbacks and dispatch
/// remain valid for the actor's lifetime. The function consumes state on every
/// outcome. `stop_release` is null or the generated `#[on(stop)]` sequence,
/// which terminal cleanup runs once on the live state after a cooperative
/// stop, suspending as its hooks do, before the state is released. `fault`
/// is a writable, initially null fault slot. `construction` is the live, unique
/// constructor invocation and remains live until publication or rollback.
#[no_mangle]
#[allow(
    clippy::too_many_arguments,
    clippy::too_many_lines,
    reason = "private generated actor ABI publishes one complete state contract"
)]
pub unsafe extern "C" fn hew_actor_construct_native(
    state: *mut c_void,
    size: usize,
    dispatch: HewDispatchFn,
    state_drop: unsafe extern "C" fn(*mut c_void),
    state_clone: HewStateCloneFn,
    stop_release: Option<hew_cabi::value::HewValueReleaseStart>,
    capacity: i32,
    overflow: i32,
    max_heap_bytes: u64,
    sys_dispatch: Option<HewSysDispatchFn>,
    native_crash: Option<HewNativeCrashFn>,
    fault: *mut *mut crate::fault::HewFault,
    coalesce_match: Option<mailbox::HewCoalesceMatchFn>,
    coalesce_fallback: i32,
    state_release: Option<hew_cabi::value::HewValueReleaseStart>,
    rejected_state_release: *mut *mut crate::release_walker::HewReleaseCursor,
    construction: *mut crate::coro_state::HewCoroState,
    has_heap_limit: u32,
    state_layout: *const hew_cabi::value::HewValueLayout,
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
    // this mailbox yet, so the key matcher is in place for the first send.
    if coalesce_match.is_some() {
        // SAFETY: the constructor above returned a live mailbox.
        unsafe {
            mailbox::hew_mailbox_set_coalesce_config(
                mailbox,
                coalesce_match,
                parse_overflow_policy(coalesce_fallback),
            );
        }
    }
    let mut completion = crate::actor_native::NativeActorCompletion::with_crash(native_crash);
    // SAFETY: generated code retains the source descriptor during construction;
    // acquire its independent actor owner before any scheduler publication.
    completion.cleanup.code = unsafe { state_layout.as_ref() }.map(|layout| {
        // SAFETY: the constructing invocation retains the defining generation.
        unsafe { crate::code_owner::CodeRoot::acquire(layout.code_owner) }
    });
    if let Some(owner) =
        // SAFETY: construction remains live through publication.
        unsafe { construction.as_ref() }.and_then(|state| state.execution().rc_owner.clone())
    {
        completion.cleanup.rc = owner;
    }
    completion.cleanup.value_owner = (has_heap_limit != 0)
        .then(|| crate::owned_value_budget::ValueOwner::new(Some(max_heap_bytes)));
    completion
        .cleanup
        .state_layout
        .store(state_layout.cast_mut(), Ordering::Relaxed);
    // SAFETY: construction retains its invocation context through admission.
    *completion.cleanup.state_source_owner.lock_or_recover() =
        Some(unsafe { crate::owned_value_budget::materialize::invocation_owner(construction) });
    completion.cleanup.set_state_release(state_release);
    completion.cleanup.set_stop_release(stop_release);
    // SAFETY: ownership and callbacks are supplied atomically before publication.
    let actor = unsafe {
        spawn_actor_internal(ActorSpawnConfig {
            initial_state: HewActorState::Constructing,
            native_completion: Some(std::sync::Arc::new(completion)),
            native_state_release: state_release,
            rejected_state_release,
            terminate_fn: None,
            state_drop_fn: Some(state_drop),
            state_clone_fn: Some(state_clone),
            state,
            state_size: size,
            dispatch: Some(dispatch),
            sys_dispatch,
            mailbox: mailbox.cast(),
            budget: HEW_MSG_BUDGET,
            coalesce_match_fn: coalesce_match,
            cycle_capable: false,
            cap_bytes: 0,
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
        // SAFETY: construction owns its context and the new actor cannot dispatch.
        unsafe {
            let actor = &*actor;
            if let Some(owner) = actor
                .native_completion
                .as_ref()
                .and_then(|completion| completion.cleanup.value_owner.as_ref())
            {
                owner.bind_incarnation(std::ptr::from_ref(actor).cast_mut());
            }
            let construction = &mut *construction;
            construction.execution_mut().actor_turn =
                live_actors::ActorIncarnation::of(std::ptr::from_ref(actor).cast_mut());
            *actor
                .native_completion
                .as_ref()
                .expect("native completion")
                .construction
                .lock_or_recover() =
                Some(crate::coro_state::RetainedCancellation::new(construction));
            if mailbox::mailbox_is_closed(actor.mailbox.cast()) {
                actor
                    .native_completion
                    .as_ref()
                    .expect("native completion")
                    .cancel_construction(crate::fault::HEW_FAULT_CANCELLED);
            }
            actor.local_pid_id
        }
    }
}

/// Publish a successfully initialized actor and enable its queued messages.
/// # Panics
/// Panics if the supplied actor has no native completion owner.
/// # Safety
/// The construction invocation still uniquely owns the actor's initialized state.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_publish_native(
    token: crate::lifetime::local_handles::HewLocalPidId,
    periodic: *const HewNativePeriodicHandler,
    periodic_count: usize,
    construction: *mut crate::coro_state::HewCoroState,
    fault: *mut *mut crate::fault::HewFault,
) -> i32 {
    let Some(id) = crate::lifetime::local_handles::resolve_current_actor(token) else {
        return 1;
    };
    live_actors::with_actor_send_by_id(id, |raw| {
        // SAFETY: the guard pins the actor; its constructor is the sole state owner.
        let actor = unsafe { &*raw };
        let completion = actor.native_completion.as_ref().expect("native completion");
        let mut registration = completion.construction.lock_or_recover();
        // SAFETY: construction retains its context and the actor retains its mailbox.
        let cancelled = unsafe {
            crate::coro_state::hew_coro_state_is_cancelled(construction) != 0
                || mailbox::mailbox_is_closed(actor.mailbox.cast())
        };
        if cancelled {
            // SAFETY: the caller supplies an empty fault output.
            unsafe {
                *fault = crate::fault::hew_fault_new(
                    crate::coro_state::hew_coro_state_cancel_code(construction),
                );
            }
            return 1;
        }
        let handlers = if periodic_count == 0 {
            &[]
        } else {
            // SAFETY: generated code provides this static descriptor slice.
            unsafe { std::slice::from_raw_parts(periodic, periodic_count) }
        };
        // SAFETY: the actor is pinned and undispatchable until publication.
        if !unsafe { crate::timer_periodic::start_actor_periodic(raw, handlers) } {
            // SAFETY: the caller provides an empty fault output.
            unsafe {
                *fault =
                    crate::fault::hew_fault_new(crate::internal::types::HewError::ErrOom as i32);
            }
            return 1;
        }
        // SAFETY: successful construction transfers the exact Rc list to the actor.
        unsafe {
            (*construction).execution_mut().rc_owner = None;
        }
        registration.take();
        actor
            .actor_state
            .swap(HewActorState::Idle as i32, Ordering::AcqRel);
        drop(registration);
        // Producers that linked work during construction could not wake it.
        // Publish Idle before checking both lanes, then share their wake CAS;
        // a later producer either wins that CAS or finds its work scheduled.
        // SAFETY: the send guard retains the actor and its mailbox.
        let pending = unsafe {
            mailbox::hew_mailbox_has_messages(actor.mailbox.cast()) != 0
                || mailbox::mailbox_is_closed(actor.mailbox.cast())
        };
        if pending || actor.pending_external_trap_code.load(Ordering::Acquire) != 0 {
            // SAFETY: the send guard pins the actor through its wake handoff.
            unsafe { finish_mailbox_enqueue(raw, actor) };
        }
        0
    })
    .unwrap_or(1)
}

/// Return failed construction to the actor's ordinary terminal cleanup owner.
/// # Panics
/// Panics if the supplied actor has no native completion owner.
/// # Safety
/// Init/start have finished and no frame still borrows the actor's state.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_abort_construction(
    token: crate::lifetime::local_handles::HewLocalPidId,
    construction: *mut crate::coro_state::HewCoroState,
) {
    let Some(id) = crate::lifetime::local_handles::resolve_current_actor(token) else {
        return;
    };
    live_actors::with_actor_send_by_id(id, |raw| {
        // SAFETY: the guard retains the exclusively constructed actor.
        let actor = unsafe { &*raw };
        let completion = actor.native_completion.as_ref().expect("native completion");
        completion.construction.lock_or_recover().take();
        completion.construction_budget.lock_or_recover().take();
        completion.cleanup.set_stop_release(None);
        // SAFETY: construction relinquishes its Rc list and self identity before joining cleanup.
        unsafe {
            let execution = (*construction).execution_mut();
            execution.rc_owner = None;
            execution.actor_turn = live_actors::ActorIncarnation::NONE;
            mailbox::mailbox_close(actor.mailbox.cast());
            mailbox::mailbox_request_terminate(actor.mailbox.cast());
            mailbox::mailbox_request_stop(actor.mailbox.cast());
        }
        crate::timer_periodic::cancel_all_timers_for_actor(raw);
        // Timer cancellation joins every winning fault handoff. The constructor
        // already owns its failure; rollback must not run a crash hook on it.
        actor.pending_external_trap_code.store(0, Ordering::Release);
        actor
            .actor_state
            .store(HewActorState::Runnable as i32, Ordering::Release);
        crate::resume::sched_enqueue(raw);
    });
}

/// Transfer one native message envelope through a stable actor incarnation.
///
/// # Safety
/// `envelope` is uniquely owned and transfers only on successful admission.
pub(crate) unsafe fn try_submit_native_envelope(
    token: crate::lifetime::local_handles::HewLocalPidId,
    message: i32,
    envelope: *mut crate::mailbox::HewMsgEnvelope,
    caller: crate::actor_caller::HewCallerStamp,
) -> crate::mailbox::SendOutcome {
    // SAFETY: sends have no reply reference; the envelope transfers on admission.
    unsafe {
        try_submit_native_request(
            token,
            message,
            envelope,
            std::ptr::null_mut(),
            false,
            caller,
            None,
        )
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
    obligated: bool,
    caller: crate::actor_caller::HewCallerStamp,
    capacity: Option<&std::sync::Arc<crate::wake::OwnedWaker>>,
) -> crate::mailbox::SendOutcome {
    // SAFETY: forwards the uniquely owned request and its optional reply.
    unsafe {
        submit_native_request(
            token, message, envelope, reply, obligated, false, caller, capacity,
        )
    }
}

pub(crate) unsafe fn submit_native_terminal(
    token: crate::lifetime::local_handles::HewLocalPidId,
    message: i32,
    envelope: *mut crate::mailbox::HewMsgEnvelope,
) -> crate::mailbox::SendOutcome {
    // SAFETY: the caller uniquely owns this terminal event envelope.
    unsafe {
        submit_native_request(
            token,
            message,
            envelope,
            std::ptr::null_mut(),
            true,
            true,
            crate::actor_caller::local_sender(),
            None,
        )
    }
}

#[allow(
    clippy::too_many_arguments,
    reason = "the admission boundary carries the checked request and its wake contract"
)]
unsafe fn submit_native_request(
    token: crate::lifetime::local_handles::HewLocalPidId,
    message: i32,
    envelope: *mut crate::mailbox::HewMsgEnvelope,
    reply: *mut c_void,
    obligated: bool,
    terminal: bool,
    caller: crate::actor_caller::HewCallerStamp,
    capacity: Option<&std::sync::Arc<crate::wake::OwnedWaker>>,
) -> crate::mailbox::SendOutcome {
    // Attachment termination is completion of work already admitted by its
    // owner. Ordinary root/external sends must finish publication before drain.
    #[cfg(not(target_arch = "wasm32"))]
    let _ingress = if terminal {
        None
    } else {
        match crate::shutdown::admit_external_work() {
            Ok(permit) => Some(permit),
            // The destination is alive; the runtime refuses new root work.
            Err(()) => return mailbox::SendOutcome::ShuttingDown,
        }
    };
    crate::lifetime::local_handles::with_current_actor(token, |actor| {
        // SAFETY: this guard pins the exact actor incarnation and its mailbox.
        let a = unsafe { &*actor };
        if let Some(waker) = capacity {
            // SAFETY: this same pin covers registration and the admission check.
            unsafe { &*a.mailbox.cast::<HewMailbox>() }
                .native_capacity
                .register(waker);
        }
        if actor_send_is_terminal(a) {
            return mailbox::SendOutcome::Closed;
        }
        // Reserve receiver cleanup before queue publication or policy disposal.
        // SAFETY: the send pin retains this receiver and the envelope is unpublished.
        if !unsafe { crate::cow_envelope::bind_receiver(envelope, a) } {
            return mailbox::SendOutcome::Closed;
        }
        // SAFETY: the send pin and receiver binding cover publication.
        let outcome = unsafe {
            admit_bound_native_request(actor, message, envelope, reply, obligated, terminal, caller)
        };

        if matches!(
            outcome,
            mailbox::SendOutcome::Failed | mailbox::SendOutcome::Closed | mailbox::SendOutcome::Oom
        ) {
            // SAFETY: refusal preserved the unpublished envelope with its caller.
            unsafe { crate::cow_envelope::unbind_receiver(envelope, a) };
        }

        outcome
    })
    .unwrap_or(mailbox::SendOutcome::Closed)
}

/// Admit a timer's already-bound payload through the ordinary mailbox rules.
/// # Safety
/// The caller pins this actor and transfers one receiver-bound envelope only
/// on an Enqueued or Dropped outcome.
pub(crate) unsafe fn submit_bound_native_envelope(
    actor: *mut HewActor,
    message: i32,
    envelope: *mut crate::mailbox::HewMsgEnvelope,
    caller: crate::actor_caller::HewCallerStamp,
) -> crate::mailbox::SendOutcome {
    // SAFETY: the timer's incarnation pin and cleanup registration remain live.
    unsafe {
        admit_bound_native_request(
            actor,
            message,
            envelope,
            ptr::null_mut(),
            false,
            false,
            caller,
        )
    }
}

#[expect(
    clippy::too_many_lines,
    reason = "the ownership transaction stays between reservation and publication"
)]
unsafe fn admit_bound_native_request(
    actor: *mut HewActor,
    message: i32,
    envelope: *mut crate::mailbox::HewMsgEnvelope,
    reply: *mut c_void,
    obligated: bool,
    terminal: bool,
    caller: crate::actor_caller::HewCallerStamp,
) -> crate::mailbox::SendOutcome {
    // SAFETY: the caller retains this exact actor allocation.
    let a = unsafe { &*actor };
    if actor_send_is_terminal(a) {
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
    // Providers quiesce without holding queue/ledger locks. The source keeps
    // the unpublished envelope while waiting, including on a restarted role.
    // SAFETY: the send pin retains this incarnation and the producer owns its envelope.
    let frozen = unsafe {
        let env = &mut *envelope;
        if env.layout.is_null() {
            Ok(true)
        } else {
            let identity = crate::lifetime::live_actors::ActorIncarnation::of(actor);
            if env
                .transfer
                .as_ref()
                .is_some_and(|transfer| transfer.target != identity)
            {
                env.transfer.take();
            }
            if env.transfer.is_none() {
                let destination = a
                    .native_completion
                    .as_ref()
                    .and_then(|completion| completion.cleanup.value_owner.clone());
                let mut value = crate::owned_value_budget::transfer::ValueTransfer::begin(
                    env.value_owner.clone(),
                    destination,
                    &raw const crate::cow_envelope::ENVELOPE_LAYOUT,
                    envelope.cast(),
                );
                value.set_destination_actor(actor);
                if let Some(waker) = env.transfer_waker.clone() {
                    value.set_waker(waker);
                }
                env.transfer = Some(crate::cow_envelope::EnvelopeTransfer {
                    target: identity,
                    value,
                    pending: false,
                });
            }
            let transfer = env.transfer.as_mut().unwrap();
            let frozen = transfer.value.poll_freeze();
            transfer.pending = matches!(frozen, Ok(false));
            frozen
        }
    };
    if matches!(frozen, Ok(false)) {
        return mailbox::SendOutcome::Failed;
    }
    // The policy decides whether this admission accepts before recipient quota
    // is consulted. Reservation pins a coalesced/evicted victim without loss.
    // SAFETY: this exact mailbox and both unpublished request references remain pinned.
    let reservation = match unsafe {
        mailbox::reserve_native_request(
            &*a.mailbox.cast(),
            message,
            envelope,
            reply,
            obligated,
            terminal,
            caller,
        )
    } {
        Ok(reservation) => reservation,
        Err(outcome) => {
            if !matches!(outcome, mailbox::SendOutcome::Dropped) {
                // SAFETY: refusal preserves the original unpublished envelope.
                unsafe {
                    (*envelope).transfer.take();
                }
            }
            return outcome;
        }
    };
    // SAFETY: accepted capacity pins this envelope while its providers stay frozen.
    let acquired = frozen.and_then(|_| unsafe {
        (*envelope)
            .transfer
            .as_mut()
            .map_or(Ok(()), |transfer| transfer.value.acquire())
    });
    if acquired.is_err() {
        drop(reservation);
        // SAFETY: no ownership committed; receiver cleanup disposes the sender-owned request.
        unsafe {
            (*envelope).transfer.take();
            // Refusal is the recipient's supervised fault. Its established
            // completion has no returned request and never faults the sender.
            hew_actor_trap(actor, crate::internal::types::HEW_TRAP_HEAP_EXCEEDED);
            if !reply.is_null() {
                crate::reply_channel::hew_reply_channel_publish_crash_fallback(reply.cast());
            }
            crate::mailbox::hew_msg_envelope_release(envelope);
        }
        return mailbox::SendOutcome::Enqueued;
    }
    // SAFETY: the accepted unpublished root is frozen through whole-graph commit and resume.
    unsafe {
        if let Some(transfer) = (*envelope).transfer.take() {
            transfer
                .value
                .commit()
                .unwrap_or_else(|_| std::process::abort());
            (*envelope).value_owner = a
                .native_completion
                .as_ref()
                .and_then(|completion| completion.cleanup.value_owner.clone());
        }
        (*envelope).transfer_waker = None;
        (*envelope).admission_pending = false;
    }
    // No receiver can access the borrowed root before graph-wide commit/resume.
    // SAFETY: ownership has committed and the caller still pins this mailbox.
    let outcome = unsafe { reservation.publish() };
    if matches!(
        outcome,
        mailbox::SendOutcome::Enqueued | mailbox::SendOutcome::Coalesced
    ) {
        // SAFETY: a message reached the live, pinned actor's mailbox.
        unsafe { schedule_actor_after_enqueue(actor, a, message) };
    }
    outcome
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
            initial_state: HewActorState::Idle,
            native_completion: None,
            native_state_release: None,
            rejected_state_release: ptr::null_mut(),
            terminate_fn: None,
            state_drop_fn: None,
            state_clone_fn: None,
            state: actor_state,
            state_size,
            dispatch,
            sys_dispatch: None,
            mailbox: mailbox.cast(),
            budget: HEW_MSG_BUDGET,
            coalesce_match_fn: None,
            cycle_capable: false,
            cap_bytes: 0,
            adopt: false,
        })
    }
}
