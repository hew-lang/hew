// native-only: the active-mode reactor uses a platform readiness poller
// (epoll/kqueue/IOCP) + OS threads, neither available on WASM. The WASM build
// fails closed via the type checker's `WasmUnsupportedFeature::TcpNetworking`
// gate before any reactor call.
//! Active-mode network I/O reactor — "I/O completion as a mailbox message".
//!
//! A single non-scheduler background thread (the *reactor*) owns a platform
//! readiness poller ([`crate::io_time::HewIoPoller`], epoll on Linux / kqueue
//! on macOS+FreeBSD / IOCP+AFD_POLL on Windows) and a registry mapping each
//! registered connection token to the actor that should receive its data. When
//! a registered socket becomes readable the reactor reads the available bytes
//! and delivers them to the
//! owning actor's mailbox as an ordinary `on_data(bytes)` message; on EOF or
//! error it delivers a single `on_close()` message and unregisters the fd.
//!
//! A scheduler worker thread therefore never blocks in a socket `read()` —
//! only the reactor thread parks in the readiness wait, preserving the
//! cooperative-scheduler invariant that a worker must never park in a syscall.
//!
//! # Concurrency discipline (the `ownership-over-locks` decision)
//!
//! The poller's `entries` map is documented "no concurrent access"
//! ([`crate::io_time`]); active mode would otherwise violate that by having
//! workers register while the reactor polls. The discipline chosen here keeps
//! that assertion TRUE:
//!
//! - **The reactor thread is the sole owner of the [`HewIoPoller`].** No other
//!   thread ever touches the poller. The reactor uses the readiness-reporting
//!   `hew_io_poller_poll_ready` variant (which only *reports* ready fds; it
//!   does not auto-send), so the reactor — not the poller — performs the
//!   liveness check, the read, and the mailbox delivery.
//! - **Registration crosses the thread boundary through a lock-guarded global
//!   registry, never through the poller.** Workers (`attach`) and the
//!   actor-teardown path (`detach_actor`) push add/remove requests into a
//!   pending queue under the [`ReactorState`] mutex. They never call into the
//!   poller and never invoke an actor send while holding the lock.
//! - **The reactor drains the pending queue into its private poller between
//!   bounded polls**, and looks up the actor for each ready fd in the registry
//!   (lock held only for the lookup — released before the read and the send).
//!
//! This mirrors the proven timer-wheel pattern (`timer_periodic.rs`:
//! lazy-started background thread + `PoisonSafe` global registry + bounded
//! tick loop + Dekker-style liveness via [`HewActorRef`] snapshots). It avoids
//! the `held-lock-invokes-callback` anti-pattern entirely: no actor send ever
//! runs under the registry lock.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI-adjacent module; SAFETY documented at each unsafe site."
)]

use std::collections::HashMap;
use std::ffi::{c_int, c_void};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Mutex, OnceLock};
use std::thread::JoinHandle;

use crate::actor::{hew_actor_send_guaranteed, hew_actor_try_send, HewActor};
use crate::bytes::{hew_bytes_from_static, BytesTriple};
use crate::io_time::{
    hew_io_poller_new, hew_io_poller_poll_ready, hew_io_poller_register, hew_io_poller_stop,
    hew_io_poller_unregister, HewIoPoller, HEW_IO_ERROR, HEW_IO_HUP, HEW_IO_READ,
};
use crate::lifetime::poison_safe::PoisonSafe;
use crate::transport::{
    actor_ref_local_ptr, hew_actor_ref_is_alive, tcp_conn_raw_fd, tcp_conn_read_available,
    tcp_conn_set_nonblocking, tcp_listener_accept_nonblocking, tcp_listener_raw_fd,
    tcp_listener_set_nonblocking, AcceptOutcome, ActiveReadOutcome, HewActorRef,
};

/// How long each readiness wait blocks before the reactor wakes to drain the
/// pending add/remove queue and re-check the stop flag. Bounded so a fresh
/// registration from a worker is honoured within this window even though the
/// reactor is the sole writer of the poller. 50 ms keeps the registration
/// latency low without busy-spinning.
const POLL_TIMEOUT_MS: c_int = 50;

/// Maximum ready fds drained per poll. Matches the poller's internal cap;
/// excess fds stay ready and surface on the next poll.
const MAX_READY: c_int = 64;
/// `MAX_READY` as a `usize` for buffer sizing.
const MAX_READY_USIZE: usize = MAX_READY as usize;

/// A pending registry mutation, queued by a worker/teardown thread and applied
/// by the reactor thread into its private poller on the next loop iteration.
enum Pending {
    /// Register `conn`'s fd for read readiness, delivering to the registration.
    Add { fd: c_int, reg: Registration },
    /// Unregister `conn` (by handle). Removes the registry entry and the poller
    /// registration. Idempotent.
    Remove { conn: c_int },
    /// Remove a specific fd from the poller. Queued by the synchronous
    /// `reactor_detach_actor`, which has ALREADY removed the registry entry
    /// itself; this only carries the poller-side `EPOLL_CTL_DEL` to the reactor
    /// thread (the sole owner of the poller).
    UnregisterFd { fd: c_int },
}

/// The readiness ACTION a registration carries — the central design seam (D-3).
/// One readiness loop, two consumption modes:
///
/// - [`RegMode::AutoSend`] — active mode (LANDED): on `Data` the reactor
///   auto-sends an `on_data(bytes)` mailbox message; on close it sends
///   `on_close()`. Inverted control flow (`conn.attach(handler)`).
/// - [`RegMode::Resume`] — await-suspension (NEW-1): on `Data`/EOF/error the
///   reactor deposits the result into the suspending handler's read slot and
///   wakes its parked continuation via `enqueue_resume`. Straight-line control
///   flow (`await conn.read()`). One-shot: the registration is removed after the
///   single deposit+wake (an `await` reads once; a loop re-registers).
///
/// NEW-2 (async HTTP/connection client) instantiates `Resume` without rework.
enum RegMode {
    /// Active-mode auto-send to the actor's `on_data` / `on_close` handlers.
    AutoSend {
        /// `msg_type` index for `on_data(bytes)` delivery.
        on_data_type: i32,
        /// `msg_type` index for the one-shot `on_close()` delivery.
        on_close_type: i32,
    },
    /// Await-suspension: deposit into the read slot + `enqueue_resume`.
    Resume {
        /// The suspending handler's read slot — the value-routing vehicle held
        /// across the OS-thread suspend. The reactor holds a ref (taken in
        /// `reactor_await_read`) it releases after the deposit+wake (or when the
        /// registration is scrubbed). Raw pointer because the slot crosses the
        /// thread boundary by value on the `Registration`; the refcount upholds
        /// validity, not a borrow.
        read_slot: *mut crate::read_slot::HewReadSlot,
    },
    /// Accept-suspension (NEW-2 `await listener.accept()`): the listener-readiness
    /// sibling of [`RegMode::Resume`]. On readiness the reactor `accept()`s a new
    /// connection, deposits its i64 handle into the read slot, and
    /// `enqueue_resume`s the parked continuation. One-shot, identical slot
    /// refcount discipline (`reactor_await_accept` takes the reactor ref, `Drop
    /// for Registration` releases it). The registration's `conn` field carries
    /// the LISTENER handle the fd belongs to.
    Accept {
        /// The suspending handler's read slot — held across the suspend; carries
        /// the deposited i64 `Connection` handle rather than bytes.
        read_slot: *mut crate::read_slot::HewReadSlot,
    },
}

/// Per-connection registration. The `actor_ref` is a by-value [`HewActorRef`]
/// snapshot so liveness stays checkable after the registering worker has moved
/// on (mirrors the websocket attach reader, which owns a `Box<HewActorRef>`).
struct Registration {
    /// The user-facing TCP connection handle this fd belongs to.
    conn: c_int,
    /// By-value actor-ref snapshot; dereferenced only on the reactor thread.
    actor_ref: HewActorRef,
    /// Stable actor identity (`*mut HewActor` as `usize`) for
    /// `reactor_detach_actor` matching on actor teardown.
    actor_key: usize,
    /// The readiness action (auto-send vs resume — the D-3 seam).
    mode: RegMode,
    /// Set once a terminal close (`on_close` / EOF / error deposit) has been
    /// reported for this conn, so it is reported exactly once even if readiness
    /// fires again before removal.
    closed: bool,
}

// SAFETY: the `HewActorRef` snapshot inside `Registration` is `#[repr(C)]`
// plain data — a `kind` tag plus a union of a raw `*mut HewActor` (local) or
// an id+conn+`*mut HewTransport` triple (remote). Moving it across threads
// only copies those scalar/pointer bytes; nothing is dereferenced during the
// move. The snapshot is dereferenced solely on the reactor thread, after the
// registry lock has handed the `Registration` over, so the cross-thread
// transfer never races a dereference. The `RegMode::Resume.read_slot` raw
// pointer is likewise only dereferenced on the reactor thread; its validity is
// upheld by the read slot's manual refcount (the reactor ref taken in
// `reactor_await_read`), not by a borrow. `Registration`'s own `Send` impl is
// what authorizes the move; the carried pointers' validity is upheld by the
// liveness protocol (the Dekker `DELIVERING_ACTOR` guard plus the synchronous
// `reactor_detach_actor` on `hew_actor_free`) and the slot refcount, not by
// this impl.
unsafe impl Send for Registration {}

impl Drop for Registration {
    /// SINGLE AUTHORITY for releasing the reactor's read-slot ref. A resume-mode
    /// registration owns one ref on its `read_slot` (taken in
    /// `reactor_await_read`). Whenever a `Registration` is dropped — removed from
    /// the registry by `unregister_fd`, scrubbed by `evict_actor_state` on the
    /// abandon edge, cleared by `reactor_shutdown`, or consumed by the
    /// `apply_add` poller-register-failure path — that ref is released here
    /// exactly once. Deposit/wake sites must NOT free the slot themselves; they
    /// drop the registration and let this impl release the ref. Active-mode
    /// (`AutoSend`) registrations carry no slot, so the drop is a no-op for them.
    fn drop(&mut self) {
        if let RegMode::Resume { read_slot } | RegMode::Accept { read_slot } = self.mode {
            // SAFETY: the registration held one live ref on `read_slot`; this
            // releases it. The slot box is freed when its last ref drops.
            unsafe { crate::read_slot::hew_read_slot_free(read_slot) };
        }
    }
}

/// The reactor's shared state. The `poller` is owned exclusively by the
/// reactor thread (only it dereferences the pointer); the mutex guards the
/// `pending` queue and the `registry` map.
struct ReactorState {
    /// fd → live registration. Mutated only by the reactor thread (draining
    /// `pending`), read by the reactor thread on readiness. Workers never
    /// touch it directly — they enqueue into `pending`.
    registry: HashMap<c_int, Registration>,
    /// conn-handle → fd, so a `Remove { conn }` can find the fd to deregister.
    conn_to_fd: HashMap<c_int, c_int>,
    /// Queued mutations from worker/teardown threads.
    pending: Vec<Pending>,
}

impl ReactorState {
    fn new() -> Self {
        Self {
            registry: HashMap::new(),
            conn_to_fd: HashMap::new(),
            pending: Vec::new(),
        }
    }
}

// `HashMap::new()` is not `const`, so the registry state is built lazily on
// first access (mirrors `transport.rs`'s `LazyLock<PoisonSafe<TcpApiState>>`).
static REACTOR_STATE: std::sync::LazyLock<PoisonSafe<ReactorState>> =
    std::sync::LazyLock::new(|| PoisonSafe::new(ReactorState::new()));
static REACTOR_RUNNING: AtomicBool = AtomicBool::new(false);
static REACTOR_STOP: AtomicBool = AtomicBool::new(false);
static REACTOR_HANDLE: OnceLock<Mutex<Option<JoinHandle<()>>>> = OnceLock::new();

/// The actor key (`*mut HewActor` as `usize`) the reactor is currently
/// delivering a message to, or `0` when idle. This is the Dekker-protocol
/// in-flight guard (mirroring `timer_periodic`): the reactor publishes the
/// target before each `hew_actor_try_send` and clears it after, so the
/// synchronous `reactor_detach_actor` (called from `hew_actor_free`) can
/// spin-wait until any in-flight delivery to the actor being freed has
/// finished before allowing the free to proceed. Without this, a readiness
/// event that passed the liveness check could send to a mailbox the freeing
/// thread tears down concurrently.
static DELIVERING_ACTOR: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

/// The actor key (`*mut HewActor` as `usize`) whose `Pending::Add` the reactor
/// is currently *promoting* into the registry, or `0` when idle. This is the
/// promotion-phase counterpart to [`DELIVERING_ACTOR`].
///
/// A free can race the window where an `attach` is still queued as a
/// `Pending::Add`. `reactor_detach_actor` phase 1 scrubs the actor's entries
/// from BOTH `pending` and `registry` under the [`ReactorState`] lock, but the
/// reactor drains `pending` on its own thread: it removes the add from
/// `pending` (so the detach scrub can no longer see it there) and only inserts
/// the registration into `registry` a moment later. To keep the registration
/// continuously visible to a concurrent detach across that hand-off, the
/// reactor publishes this guard *in the same locked section that removes the
/// add from `pending`*, and clears it only after the registry insert (or the
/// fail-closed abort) completes. `reactor_detach_actor` phase 2 spin-waits
/// while `PROMOTING_ACTOR == actor_key`, then re-scrubs the registry — so an
/// add that lands during the wait is still evicted before the actor is freed.
static PROMOTING_ACTOR: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

/// Ensure the reactor thread is running. Lazily started on first `attach`.
/// Returns `true` if the reactor is running (or was just started).
fn ensure_reactor_started() -> bool {
    if REACTOR_RUNNING.swap(true, Ordering::SeqCst) {
        return true; // already running
    }

    #[cfg(test)]
    if should_fail_reactor_spawn() {
        REACTOR_RUNNING.store(false, Ordering::SeqCst);
        crate::set_last_error("hew_tcp_attach: failed to spawn active-mode reactor thread");
        return false;
    }

    // Create the poller on the spawning thread; hand it to the reactor as a
    // usize address so the closure is `Send` (the poller pointer is then owned
    // solely by the reactor thread per the concurrency discipline above).
    // SAFETY: hew_io_poller_new has no preconditions.
    let poller = unsafe { hew_io_poller_new() };
    if poller.is_null() {
        REACTOR_RUNNING.store(false, Ordering::SeqCst);
        crate::set_last_error("hew_tcp_attach: failed to create I/O poller for reactor");
        return false;
    }
    let poller_addr = poller as usize;

    REACTOR_STOP.store(false, Ordering::SeqCst);
    let spawn = std::thread::Builder::new()
        .name("hew-io-reactor".into())
        .spawn(move || {
            let poller = poller_addr as *mut HewIoPoller;
            reactor_loop(poller);
            // SAFETY: the reactor owns the poller exclusively; on loop exit no
            // other thread can reference it, so teardown here is sound.
            unsafe { hew_io_poller_stop(poller) };
        });

    let Ok(handle) = spawn else {
        REACTOR_RUNNING.store(false, Ordering::SeqCst);
        // SAFETY: the spawn failed, so no reactor thread owns the poller.
        unsafe { hew_io_poller_stop(poller) };
        crate::set_last_error("hew_tcp_attach: failed to spawn active-mode reactor thread");
        return false;
    };
    let slot = REACTOR_HANDLE.get_or_init(|| Mutex::new(None));
    *slot
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner) = Some(handle);
    true
}

/// The reactor thread body. Owns `poller` exclusively. Loops: drain pending
/// registry mutations into the poller, wait for readiness, deliver to actors.
fn reactor_loop(poller: *mut HewIoPoller) {
    let mut ready_fds = [0_i32; MAX_READY_USIZE];
    let mut ready_events = [0_i32; MAX_READY_USIZE];

    loop {
        if REACTOR_STOP.load(Ordering::Acquire) {
            break;
        }

        drain_pending(poller);

        // SAFETY: `poller` is valid for the reactor's lifetime; the buffers are
        // MAX_READY long; we own the poller exclusively.
        let n = unsafe {
            hew_io_poller_poll_ready(
                poller,
                POLL_TIMEOUT_MS,
                ready_fds.as_mut_ptr(),
                ready_events.as_mut_ptr(),
                MAX_READY,
            )
        };
        if n <= 0 {
            // n == 0: timeout (loop to re-drain pending + re-check stop).
            // n < 0: poll error; back off one tick rather than busy-loop.
            if n < 0 {
                std::thread::sleep(std::time::Duration::from_millis(1));
            }
            continue;
        }

        #[expect(clippy::cast_sign_loss, reason = "n > 0 checked above")]
        let count = (n as usize).min(MAX_READY_USIZE);
        for i in 0..count {
            if REACTOR_STOP.load(Ordering::Acquire) {
                return;
            }
            handle_ready_fd(poller, ready_fds[i], ready_events[i]);
        }
    }
}

/// Apply queued add/remove requests into the reactor-private poller. Runs only
/// on the reactor thread, so the poller's "no concurrent access" assertion
/// holds.
///
/// Items are popped one at a time under the [`ReactorState`] lock rather than
/// bulk-taken. For a `Pending::Add` this matters: the pop and the publication
/// of the [`PROMOTING_ACTOR`] guard happen in the *same* locked section, so a
/// concurrent `reactor_detach_actor` either still sees the add in `pending`
/// (and scrubs it) or observes the guard already set (and waits it out before
/// re-scrubbing the registry). There is no window in which the registration is
/// invisible to a detach.
fn drain_pending(poller: *mut HewIoPoller) {
    loop {
        // Pop the next pending request under the lock. For an Add, publish the
        // promotion guard in this SAME locked section so the registration is
        // never simultaneously absent from `pending` and unguarded.
        let next = REACTOR_STATE.access(|state| {
            if state.pending.is_empty() {
                return None;
            }
            let req = state.pending.remove(0);
            if let Pending::Add { reg, .. } = &req {
                PROMOTING_ACTOR.store(reg.actor_key, Ordering::SeqCst);
            }
            Some(req)
        });
        let Some(req) = next else { break };
        match req {
            Pending::Add { fd, reg } => apply_add(poller, fd, reg),
            Pending::Remove { conn } => apply_remove_by_conn(poller, conn),
            Pending::UnregisterFd { fd } => apply_unregister_fd(poller, fd),
        }
    }
}

/// Promote a queued registration into the registry. The [`PROMOTING_ACTOR`]
/// guard was published by [`drain_pending`] for this `reg.actor_key` and is
/// cleared here once the registration has either landed in the registry or been
/// abandoned (poller-register failure). A concurrent `reactor_detach_actor`
/// spin-waits on that guard, so the registration is always evicted before the
/// owning actor is freed.
fn apply_add(poller: *mut HewIoPoller, fd: c_int, reg: Registration) {
    let conn = reg.conn;
    // SAFETY: poller is reactor-owned and valid; fd is a live socket fd. The
    // registered actor pointer is never dereferenced by the poller in the
    // readiness-reporting path (we pass a null actor + dummy msg_type because
    // the reactor does the lookup/read/send itself).
    let rc = unsafe { hew_io_poller_register(poller, fd, std::ptr::null_mut(), 0, HEW_IO_READ) };
    if rc < 0 {
        // OS poller registration failed (bad fd / already closed). Fail closed:
        // deliver on_close so the actor is not left waiting; do not insert.
        deliver_orphan_close(&reg);
        // Promotion abandoned; release the guard so a waiting detach proceeds.
        PROMOTING_ACTOR.store(0, Ordering::SeqCst);
        return;
    }
    REACTOR_STATE.access(|state| {
        state.conn_to_fd.insert(conn, fd);
        state.registry.insert(fd, reg);
    });
    crate::observe::record_reactor_registration();
    // Registration is now in the registry where `reactor_detach_actor` phase 1
    // can find it; release the promotion guard.
    PROMOTING_ACTOR.store(0, Ordering::SeqCst);
}

fn apply_remove_by_conn(poller: *mut HewIoPoller, conn: c_int) {
    let fd = REACTOR_STATE.access(|state| state.conn_to_fd.remove(&conn));
    if let Some(fd) = fd {
        unregister_fd(poller, fd);
    }
}

/// Apply a poller-side `EPOLL_CTL_DEL` for an fd whose registry entry was
/// already removed synchronously by `reactor_detach_actor`. Reactor-thread
/// only (sole poller owner).
fn apply_unregister_fd(poller: *mut HewIoPoller, fd: c_int) {
    // SAFETY: poller is reactor-owned and valid; double-unregister is benign
    // (the kqueue backend ignores ENOENT; epoll returns -1 harmlessly).
    unsafe {
        hew_io_poller_unregister(poller, fd);
    }
}

/// Remove an fd from the poller and the registry (reactor thread only).
fn unregister_fd(poller: *mut HewIoPoller, fd: c_int) {
    // SAFETY: poller is reactor-owned and valid.
    unsafe {
        hew_io_poller_unregister(poller, fd);
    }
    let removed = REACTOR_STATE.access(|state| {
        let removed = state.registry.remove(&fd).is_some();
        state.conn_to_fd.retain(|_, mapped| *mapped != fd);
        removed
    });
    if removed {
        crate::observe::record_reactor_unregistration(1);
    }
}

/// The mode-specific fields `handle_ready_fd` needs after the registry lock is
/// released. Mirrors the registration's [`RegMode`] but holds the resolved
/// resume-mode slot pointer by value so the lock is not held across the
/// deposit+wake.
enum ReadyMode {
    AutoSend {
        on_data_type: i32,
        on_close_type: i32,
    },
    Resume {
        read_slot: *mut crate::read_slot::HewReadSlot,
    },
    /// Accept-readiness (NEW-2): `accept()` a new connection and deposit its i64
    /// handle into the slot. The accept-path sibling of [`ReadyMode::Resume`].
    Accept {
        read_slot: *mut crate::read_slot::HewReadSlot,
    },
}

/// A snapshot of the fields `handle_ready_fd` needs, taken under the registry
/// lock and used after the lock is released.
struct ReadySnapshot {
    conn: c_int,
    actor_ref: HewActorRef,
    actor_local: *mut HewActor,
    mode: ReadyMode,
    already_closed: bool,
}

/// RAII release of the IN-FLIGHT read-slot ref `handle_ready_fd` takes for a
/// resume-mode delivery.
///
/// P1-A guard. The `Registration` snapshot copies `read_slot` by raw pointer,
/// and the deposit in `handle_ready_resume` runs WITHOUT the registry lock. A
/// concurrent `reactor_detach_actor` is scrub-then-wait: its Phase-1
/// `evict_actor_state` drops the `Registration` (and thus the registration-owned
/// slot ref) BEFORE the Phase-2 `DELIVERING_ACTOR` wait. Teardown destroys the
/// coroutine first (the codegen cleanup drops the creator ref), so the Phase-1
/// scrub can drop the LAST ref and free the slot while the reactor is mid-deposit
/// on this pointer — the `DELIVERING_ACTOR` guard protects the actor, not the
/// slot.
///
/// `handle_ready_fd` retains its OWN ref on the slot UNDER the registry lock (in
/// the snapshot closure, where the `Registration`'s ref guarantees the slot is
/// live), independent of the registration-owned ref. This guard releases that
/// in-flight ref on EVERY exit from `handle_ready_fd` (successful deposit, the
/// `!still_registered || !alive` abort, the spurious-`WouldBlock` no-deposit
/// return inside `handle_ready_resume`, and any future path). `Drop for
/// Registration` remains the single authority for the registration-owned ref;
/// this in-flight ref is separate and never double-released.
struct InflightSlotRef(*mut crate::read_slot::HewReadSlot);

impl Drop for InflightSlotRef {
    fn drop(&mut self) {
        // SAFETY: the ref was taken under the registry lock in `handle_ready_fd`'s
        // snapshot closure (mirroring `reactor_await_read`'s retain). Releasing it
        // here drops exactly that one in-flight ref; the slot box is reclaimed
        // only when its last ref (creator/registration/in-flight) drops.
        unsafe { crate::read_slot::hew_read_slot_free(self.0) };
    }
}

/// Liveness for a snapshotted actor ref WITHOUT a raw deref of a possibly-freed
/// local actor pointer.
///
/// The reactor releases the registry lock between snapshotting `actor_ref` and
/// publishing the `DELIVERING_ACTOR` guard. A concurrent `reactor_detach_actor`
/// that runs entirely inside that window observes the guard as 0, does not
/// spin-wait, and lets `hew_actor_free_inner` reclaim the actor box. A raw
/// `hew_actor_ref_is_alive` probe dereferences `(*actor).actor_state` for a LOCAL
/// ref (transport.rs), so calling it here would be a use-after-free.
///
/// This routes the LOCAL case through `with_live_actor`, which holds the
/// `LIVE_ACTORS` registry lock across the closure. Every free path
/// (`hew_actor_free_inner`, `drain_quiesced_actor`, `cleanup_all_actors`) removes
/// the actor from `LIVE_ACTORS` BEFORE reclaiming the box, so while the closure
/// runs the box cannot be freed; if the actor was already torn down the closure
/// never runs and the actor is reported dead. The `actor_state` load inside the
/// closure preserves the original semantics (a Stopping/Crashed/Stopped actor is
/// reported dead, not just an untracked one).
///
/// A REMOTE ref (`actor_local` null) carries no actor pointer to deref; its
/// liveness is `hew_actor_ref_is_alive`'s connection-validity check on the
/// by-value snapshot, which never touches actor memory.
fn actor_snapshot_alive(actor_local: *mut HewActor, actor_ref: &HewActorRef) -> bool {
    if actor_local.is_null() {
        // REMOTE (or null-local) ref: `hew_actor_ref_is_alive` reads only the
        // by-value snapshot's connection handle; no actor pointer is dereferenced.
        // SAFETY: `actor_ref` is a valid stack snapshot owned by the caller.
        return unsafe { hew_actor_ref_is_alive(std::ptr::addr_of!(*actor_ref)) != 0 };
    }
    // LOCAL ref: check liveness + terminal state under the `LIVE_ACTORS` lock so
    // a freed actor is never dereferenced. `None` => not tracked => freed/dead.
    crate::lifetime::live_actors::with_live_actor(actor_local, |a| {
        let state = a.actor_state.load(Ordering::Acquire);
        state != crate::internal::types::HewActorState::Stopped as i32
            && state != crate::internal::types::HewActorState::Crashed as i32
    })
    .unwrap_or(false)
}

/// Handle one ready fd: liveness-check the owning actor, read available bytes,
/// and EITHER auto-send `on_data`/`on_close` mailbox messages (active mode) OR
/// deposit the result into the suspending handler's read slot + `enqueue_resume`
/// (await-suspension). Never holds the registry lock across the read or the
/// send/wake.
fn handle_ready_fd(poller: *mut HewIoPoller, fd: c_int, events: c_int) {
    // Snapshot what we need under the lock, then release it.
    let snapshot = REACTOR_STATE.access(|state| {
        state.registry.get(&fd).map(|reg| ReadySnapshot {
            conn: reg.conn,
            // SAFETY: HewActorRef is Copy-able plain data; we duplicate the
            // snapshot so liveness can be checked after the lock is released.
            actor_ref: unsafe { std::ptr::read(std::ptr::addr_of!(reg.actor_ref)) },
            actor_local: actor_ref_local_ptr(&reg.actor_ref).cast::<HewActor>(),
            mode: match &reg.mode {
                RegMode::AutoSend {
                    on_data_type,
                    on_close_type,
                } => ReadyMode::AutoSend {
                    on_data_type: *on_data_type,
                    on_close_type: *on_close_type,
                },
                RegMode::Resume { read_slot } => {
                    // P1-A: take an IN-FLIGHT ref on the slot UNDER the registry
                    // lock, independent of the registration-owned ref. While the
                    // lock is held the `Registration` exists and holds its own
                    // ref, so the slot is live for this retain (mirrors
                    // `reactor_await_read`'s retain). The `InflightSlotRef` guard
                    // below releases it on every exit; a concurrent detach's
                    // Phase-1 scrub can then drop the registration's ref without
                    // freeing the slot out from under the in-flight deposit.
                    // SAFETY: the lock is held and the registration holds a ref,
                    // so `*read_slot` is a live slot.
                    unsafe { crate::read_slot::read_slot_retain(*read_slot) };
                    ReadyMode::Resume {
                        read_slot: *read_slot,
                    }
                }
                // NEW-2 accept-readiness: same in-flight-ref discipline as
                // `Resume` (the slot crosses the lock by raw pointer; the retain
                // keeps it live for the lock-free accept+deposit below).
                RegMode::Accept { read_slot } => {
                    // SAFETY: the lock is held and the registration holds a ref,
                    // so `*read_slot` is a live slot.
                    unsafe { crate::read_slot::read_slot_retain(*read_slot) };
                    ReadyMode::Accept {
                        read_slot: *read_slot,
                    }
                }
            },
            already_closed: reg.closed,
        })
    });
    let Some(snap) = snapshot else {
        // Registration vanished between poll and lookup (raced a remove).
        unregister_fd(poller, fd);
        return;
    };
    crate::observe::record_reactor_ready_event();

    // Bind the in-flight slot ref to an RAII guard so it is released on EVERY
    // exit from here on (the retain was taken under the lock in the snapshot
    // closure above). Active-mode snapshots carry no slot, so no guard is bound.
    let _inflight_slot = match &snap.mode {
        ReadyMode::Resume { read_slot } | ReadyMode::Accept { read_slot } => {
            Some(InflightSlotRef(*read_slot))
        }
        ReadyMode::AutoSend { .. } => None,
    };

    let actor_key = snap.actor_local as usize;

    // Publish the in-flight target BEFORE re-validating + sending (Dekker
    // protocol with `reactor_detach_actor`). Ordering with the registry-lock
    // revalidation below guarantees that a concurrent `hew_actor_free` either
    // (a) removes the registration before we re-read it under the lock — so we
    // see it gone and abort — or (b) observes `DELIVERING_ACTOR == actor_key`
    // and spin-waits until we clear it. Either way no send/wake reaches a freed
    // actor. The resume-mode wake (`enqueue_resume`) is ALSO independently
    // fail-safe (its own liveness check + `Suspended → Runnable` CAS), so the
    // guard + the waker are belt-and-braces for the abandon edge.
    DELIVERING_ACTOR.store(actor_key, Ordering::SeqCst);

    // Re-validate under the lock AFTER publishing the guard: if the actor was
    // detached (freed) between the snapshot and now, the registration is gone
    // and we must not deliver. This pairs with the synchronous registry removal
    // in `reactor_detach_actor`. On the resume-mode abort path the in-flight slot
    // ref taken above is released by the `InflightSlotRef` guard on return; the
    // registration-owned ref is released by `reactor_detach_actor`'s scrub.
    let still_registered = REACTOR_STATE.access(|state| state.registry.contains_key(&fd));
    // Liveness as a second guard (the actor may be Stopping but not yet freed).
    // For a LOCAL actor this MUST NOT raw-deref the snapshot pointer: a
    // concurrent `reactor_detach_actor` that observed the guard as 0 in the
    // snapshot→publish window can free the actor before this point, so a raw
    // `(*actor).actor_state` load would be a use-after-free. `actor_snapshot_alive`
    // routes the LOCAL case through `with_live_actor` (the `LIVE_ACTORS`-guarded
    // discipline `enqueue_resume` uses): the membership check + the state load run
    // under the registry lock that every free path takes before reclaiming the
    // box, so a freed actor is reported dead without ever being dereferenced.
    let alive = actor_snapshot_alive(snap.actor_local, &snap.actor_ref);
    if !still_registered || !alive {
        DELIVERING_ACTOR.store(0, Ordering::SeqCst);
        unregister_fd(poller, fd);
        return;
    }

    let hard_close = events & (HEW_IO_HUP | HEW_IO_ERROR) != 0;

    // NEW-2 accept-readiness: the registered fd is a LISTENER, not a connected
    // stream, so it must NOT be read with `tcp_conn_read_available`. `accept()`
    // a connection and deposit its handle; the listener stays usable for the next
    // `await accept()` (re-registered by the handler).
    if let ReadyMode::Accept { read_slot } = &snap.mode {
        handle_ready_accept(poller, fd, &snap, hard_close, *read_slot);
        DELIVERING_ACTOR.store(0, Ordering::SeqCst);
        return;
    }

    // Read whatever is available (drains the kernel buffer). Even on a HUP/ERR
    // event there may be buffered bytes to deliver before the close.
    let outcome = if events & HEW_IO_READ != 0 || hard_close {
        tcp_conn_read_available(snap.conn)
    } else {
        ActiveReadOutcome::WouldBlock
    };

    match &snap.mode {
        ReadyMode::AutoSend {
            on_data_type,
            on_close_type,
        } => handle_ready_auto_send(
            poller,
            fd,
            &snap,
            outcome,
            hard_close,
            *on_data_type,
            *on_close_type,
        ),
        ReadyMode::Resume { read_slot } => {
            handle_ready_resume(poller, fd, &snap, outcome, hard_close, *read_slot);
        }
        // Handled above (before the connected-stream read) — the listener fd is
        // never read as a stream.
        ReadyMode::Accept { .. } => unreachable!("accept readiness dispatched above"),
    }

    // Delivery (if any) is complete; release the in-flight guard so a waiting
    // `reactor_detach_actor` may proceed with the free.
    DELIVERING_ACTOR.store(0, Ordering::SeqCst);
}

/// Active-mode readiness handling: auto-send `on_data(bytes)` for each chunk and
/// a single `on_close()` on EOF/error. Unchanged from the pre-NEW-1 behaviour.
fn handle_ready_auto_send(
    poller: *mut HewIoPoller,
    fd: c_int,
    snap: &ReadySnapshot,
    outcome: ActiveReadOutcome,
    hard_close: bool,
    on_data_type: i32,
    on_close_type: i32,
) {
    match outcome {
        ActiveReadOutcome::Data(data) => {
            deliver_data(snap.actor_local, on_data_type, &data);
            if hard_close {
                deliver_close_once(
                    poller,
                    fd,
                    snap.actor_local,
                    on_close_type,
                    snap.already_closed,
                );
            }
        }
        ActiveReadOutcome::WouldBlock => {
            if hard_close {
                deliver_close_once(
                    poller,
                    fd,
                    snap.actor_local,
                    on_close_type,
                    snap.already_closed,
                );
            }
        }
        ActiveReadOutcome::Eof | ActiveReadOutcome::Closed => {
            deliver_close_once(
                poller,
                fd,
                snap.actor_local,
                on_close_type,
                snap.already_closed,
            );
        }
    }
}

/// An empty `bytes` value (null ptr, len 0). The resume edge binds this for an
/// EOF / error / over-length read, matching the blocking `hew_tcp_read` empty
/// convention.
fn empty_bytes_triple() -> BytesTriple {
    BytesTriple {
        ptr: std::ptr::null_mut(),
        offset: 0,
        len: 0,
    }
}

/// Await-suspension readiness handling (NEW-1): deposit the read result into the
/// suspending handler's read slot and `enqueue_resume` the parked continuation,
/// then remove the (one-shot) registration. An `await conn.read()` reads once;
/// the handler re-registers for the next read on its next `await`.
///
/// - `Data` → deposit `Ok(bytes)` + wake.
/// - `Eof`/`Closed` → deposit an `Eof`/`Error` status + wake (the handler
///   resumes with an empty `bytes`; NEW-6 layers a typed-error surface on top).
///   A `hard_close` with buffered data delivers the data first; the next
///   readiness reports EOF, but the registration is already gone, so the handler
///   re-`await`ing observes the close.
/// - `WouldBlock` with no hard close → a spurious wake; leave the registration
///   in place and wait for the next readiness (no deposit, no wake).
///
/// `read_slot` validity for the lock-free deposit below is upheld by the
/// IN-FLIGHT ref `handle_ready_fd` took under the registry lock (P1-A), NOT by
/// the registration-owned ref: a concurrent `reactor_detach_actor`'s Phase-1
/// scrub may drop the registration's ref while this deposit runs, but the
/// in-flight ref keeps the slot box alive until `handle_ready_fd` returns and its
/// `InflightSlotRef` guard drops.
fn handle_ready_resume(
    poller: *mut HewIoPoller,
    fd: c_int,
    snap: &ReadySnapshot,
    outcome: ActiveReadOutcome,
    hard_close: bool,
    read_slot: *mut crate::read_slot::HewReadSlot,
) {
    use crate::read_slot::ReadStatus;
    // P1-A forced-ordering injection point: a test runs the scrub-then-creator-
    // free ordering here, after the in-flight ref is held but before the deposit,
    // so the deposit below lands while the registration-owned ref is already gone.
    #[cfg(test)]
    fire_resume_pre_deposit_hook();
    let deposit = match outcome {
        ActiveReadOutcome::Data(data) => {
            // Build an owned refcount-1 bytes value the slot takes ownership of.
            let triple = match u32::try_from(data.len()) {
                Ok(0) => empty_bytes_triple(),
                Ok(len) => {
                    // SAFETY: data is valid for len bytes; copied into a fresh
                    // refcount-1 buffer the slot then owns.
                    unsafe { hew_bytes_from_static(data.as_ptr(), len) }
                }
                Err(_) => {
                    crate::set_last_error("hew reactor: await read chunk exceeds u32 range");
                    empty_bytes_triple()
                }
            };
            // SAFETY: the reactor holds a ref to `read_slot`; deposit checks the
            // cancelled flag and drops the buffer if the abandon edge won.
            let wake = unsafe { crate::read_slot::read_slot_deposit_data(read_slot, triple) };
            DepositOutcome {
                wake,
                slot_done: true,
            }
        }
        ActiveReadOutcome::Eof => {
            // SAFETY: reactor holds a ref.
            let wake =
                unsafe { crate::read_slot::read_slot_deposit_status(read_slot, ReadStatus::Eof) };
            DepositOutcome {
                wake,
                slot_done: true,
            }
        }
        ActiveReadOutcome::Closed => {
            // SAFETY: reactor holds a ref.
            let wake =
                unsafe { crate::read_slot::read_slot_deposit_status(read_slot, ReadStatus::Error) };
            DepositOutcome {
                wake,
                slot_done: true,
            }
        }
        ActiveReadOutcome::WouldBlock => {
            if hard_close {
                // SAFETY: reactor holds a ref.
                let wake = unsafe {
                    crate::read_slot::read_slot_deposit_status(read_slot, ReadStatus::Error)
                };
                DepositOutcome {
                    wake,
                    slot_done: true,
                }
            } else {
                // Spurious readiness with no data and no close: leave the
                // registration live, wait for the next readiness.
                DepositOutcome {
                    wake: false,
                    slot_done: false,
                }
            }
        }
    };

    if !deposit.slot_done {
        return;
    }

    if deposit.wake {
        // SAFETY: `enqueue_resume` re-confirms liveness under the registry lock
        // and only flips state + enqueues; a freed actor drops the wake.
        unsafe { crate::scheduler::enqueue_resume(snap.actor_local, std::ptr::null_mut()) };
    }
    // One-shot: remove the registration. Dropping the `Registration` releases the
    // REGISTRATION-OWNED slot ref (the `Drop for Registration` single authority
    // for THAT ref); no further readiness can reach this slot via the registry
    // afterwards. The separate IN-FLIGHT ref taken by `handle_ready_fd` is
    // released by its `InflightSlotRef` guard when it returns.
    let _ = read_slot;
    unregister_fd(poller, fd);
}

/// Accept-suspension readiness handling (NEW-2 `await listener.accept()`): the
/// listener-readiness sibling of [`handle_ready_resume`]. `accept()` one
/// connection, deposit its i64 handle into the slot, `enqueue_resume` the parked
/// continuation, then remove the (one-shot) registration. An `await
/// listener.accept()` accepts once; the handler re-registers for the next accept
/// on its next `await`.
///
/// - `Accepted(handle)` → deposit the connection handle + wake.
/// - `Closed` / `hard_close` → deposit the invalid sentinel (`-1`) + wake so the
///   handler resumes with an invalid `Connection` (`hew_connection_is_valid`
///   rejects it) rather than hanging; never a panic (DI-014).
/// - `WouldBlock` with no hard close → a spurious wake; leave the registration
///   in place and wait for the next readiness (no deposit, no wake).
///
/// `read_slot` validity for the lock-free deposit is upheld by the IN-FLIGHT ref
/// `handle_ready_fd` took under the registry lock (P1-A), exactly as in
/// [`handle_ready_resume`].
fn handle_ready_accept(
    poller: *mut HewIoPoller,
    fd: c_int,
    snap: &ReadySnapshot,
    hard_close: bool,
    read_slot: *mut crate::read_slot::HewReadSlot,
) {
    use crate::read_slot::INVALID_CONNECTION_HANDLE;
    let (deposit_handle, slot_done) = match tcp_listener_accept_nonblocking(snap.conn) {
        AcceptOutcome::Accepted(handle) => {
            // Record the accepted handle for the abandon-race close regression
            // test so it can assert the specific handle was closed without
            // depending on the global streams-table size (which concurrent
            // transport tests also modify).
            #[cfg(test)]
            LAST_ACCEPTED_CONN_FOR_TEST.set(handle);
            (i64::from(handle), true)
        }
        AcceptOutcome::Closed => (INVALID_CONNECTION_HANDLE, true),
        AcceptOutcome::WouldBlock => {
            if hard_close {
                (INVALID_CONNECTION_HANDLE, true)
            } else {
                // Spurious readiness: leave the registration live for the next
                // accept readiness (no deposit, no wake).
                return;
            }
        }
    };
    let _ = slot_done;

    // SAFETY: the reactor holds an in-flight ref to `read_slot`; the deposit
    // checks the cancelled flag before publishing + waking.
    let wake = unsafe { crate::read_slot::read_slot_deposit_handle(read_slot, deposit_handle) };
    if wake {
        // SAFETY: `enqueue_resume` re-confirms liveness under the registry lock
        // and only flips state + enqueues; a freed actor drops the wake.
        unsafe { crate::scheduler::enqueue_resume(snap.actor_local, std::ptr::null_mut()) };
    } else if deposit_handle != INVALID_CONNECTION_HANDLE {
        // Deposit FAILED on a real accepted connection: the suspended handler was
        // abandoned/cancelled (cancelled flag set, or a cancellation/deadline won
        // the status CAS), so the resume edge binds an INVALID `Connection` — not
        // this handle — and no Hew-side owner will ever close the socket we just
        // accepted. Close it here so the accepted handle is freed EXACTLY ONCE.
        //
        // Exactly-once argument: `read_slot_deposit_handle` returns `true` on
        // exactly the one path where a resume edge takes ownership (and closes
        // via `hew_tcp_close`), and `false` on exactly the paths where no resume
        // owner exists. The two are mutually exclusive, so closing here precisely
        // when it returned `false` (and we accepted a real conn) closes the handle
        // once and only once — no double-close on the happy path, no leak on the
        // abandon race. Mirrors the NEW-7 close-sink / abandon discipline.
        if let Ok(handle) = c_int::try_from(deposit_handle) {
            crate::transport::tcp_close_orphan_conn(handle);
        }
    }
    // One-shot: remove the registration. `Drop for Registration` releases the
    // REGISTRATION-OWNED slot ref; the IN-FLIGHT ref is released by the
    // `InflightSlotRef` guard when `handle_ready_fd` returns.
    let _ = read_slot;
    unregister_fd(poller, fd);
}
struct DepositOutcome {
    /// Whether the parked continuation should be woken (`enqueue_resume`).
    wake: bool,
    /// Whether the slot received a terminal deposit (so the one-shot
    /// registration must be removed + the reactor's slot ref released). `false`
    /// only for a spurious `WouldBlock` with no hard close.
    slot_done: bool,
}

/// Deliver an `on_data(bytes)` message. The mailbox deep-copies the
/// `BytesTriple` struct; the triple's heap buffer (refcount 1, from
/// `hew_bytes_from_static`) is handed off to the actor, which drops it after
/// `on_data` returns. Exactly one refcount transferred per send.
fn deliver_data(actor_local: *mut HewActor, on_data_type: i32, data: &[u8]) {
    if actor_local.is_null() || data.is_empty() {
        return;
    }
    let Ok(len) = u32::try_from(data.len()) else {
        crate::set_last_error("hew reactor: on_data chunk exceeds u32 range");
        return;
    };
    // SAFETY: data is valid for data.len() bytes; hew_bytes_from_static copies
    // them into a fresh refcount-1 buffer and returns an owned triple.
    let mut triple: BytesTriple = unsafe { hew_bytes_from_static(data.as_ptr(), len) };
    // SAFETY: actor_local is a live local actor pointer (liveness checked by
    // the caller); the mailbox deep-copies sizeof(BytesTriple) bytes, taking
    // ownership of the triple's single refcount.
    let rc = unsafe {
        hew_actor_try_send(
            actor_local,
            on_data_type,
            std::ptr::addr_of_mut!(triple).cast::<c_void>(),
            std::mem::size_of::<BytesTriple>(),
        )
    };
    if rc != 0 {
        // Delivery rejected (mailbox full / actor stopping). The mailbox did
        // NOT take the triple, so release the refcount here to avoid a leak.
        // SAFETY: triple.ptr owns one refcount the mailbox did not consume.
        unsafe { crate::bytes::hew_bytes_drop(triple.ptr) };
    }
}

/// Deliver an `on_close()` once, then unregister the fd. Guards against a
/// second close if readiness fires again before the remove is applied.
fn deliver_close_once(
    poller: *mut HewIoPoller,
    fd: c_int,
    actor_local: *mut HewActor,
    on_close_type: i32,
    already_closed: bool,
) {
    if !already_closed && !actor_local.is_null() {
        // The terminal close event is GUARANTEED-DELIVERED: it must reach the
        // actor even when the mailbox is full under data backpressure, or the
        // actor never runs teardown and the connection leaks forever. A plain
        // `hew_actor_try_send` drops on a full mailbox; `hew_actor_send_guaranteed`
        // appends past the bounded-capacity policy (FIFO-after buffered on_data,
        // never the priority system queue) without ever blocking — so the single
        // reactor thread is not stalled and cannot deadlock with the synchronous
        // `reactor_detach_actor` spin-wait on `DELIVERING_ACTOR`. The Dekker
        // in-flight guard published by `handle_ready_fd` still bounds this send:
        // it is a bounded enqueue + atomic wake, so the guard clears promptly.
        // SAFETY: actor_local is a live local actor pointer (liveness checked
        // by the caller); on_close carries no payload.
        unsafe {
            hew_actor_send_guaranteed(actor_local, on_close_type, std::ptr::null_mut(), 0);
        }
    }
    // Mark closed (in case the remove is deferred) then unregister.
    REACTOR_STATE.access(|state| {
        if let Some(reg) = state.registry.get_mut(&fd) {
            reg.closed = true;
        }
    });
    unregister_fd(poller, fd);
}

/// Deliver a terminal close for a registration that never made it into the
/// registry (poller register failed). No fd to unregister.
///
/// - `AutoSend`: send the one-shot `on_close()` mailbox message.
/// - `Resume`: deposit an `Error` status into the read slot + `enqueue_resume`
///   so the suspending handler resumes with an error rather than hanging
///   forever, then release the reactor's slot ref.
fn deliver_orphan_close(reg: &Registration) {
    let actor_local = actor_ref_local_ptr(&reg.actor_ref).cast::<HewActor>();
    match reg.mode {
        RegMode::AutoSend { on_close_type, .. } => {
            if actor_local.is_null() {
                return;
            }
            // SAFETY: snapshot deref; the actor was alive at attach time. A
            // try_send to a since-stopped actor returns an error and is harmless.
            unsafe {
                hew_actor_try_send(actor_local, on_close_type, std::ptr::null_mut(), 0);
            }
        }
        RegMode::Resume { read_slot } => {
            resume_with_status(actor_local, read_slot, crate::read_slot::ReadStatus::Error);
        }
        RegMode::Accept { read_slot } => {
            // NEW-2: the accept registration never made it into the poller; wake
            // the parked handler with an invalid `Connection` (fail-closed) so it
            // resumes rather than hanging.
            // SAFETY: the reactor holds a ref (taken in `reactor_await_accept`);
            // the deposit checks the cancelled flag before publishing.
            let should_wake = unsafe {
                crate::read_slot::read_slot_deposit_handle(
                    read_slot,
                    crate::read_slot::INVALID_CONNECTION_HANDLE,
                )
            };
            if should_wake && !actor_local.is_null() {
                // SAFETY: `enqueue_resume` re-confirms liveness; a freed actor
                // drops the wake.
                unsafe { crate::scheduler::enqueue_resume(actor_local, std::ptr::null_mut()) };
            }
        }
    }
}

/// Deposit a terminal status into a resume-mode read slot and wake the parked
/// continuation. The deposit is dropped (no wake) if the slot was cancelled by
/// an abandon edge. Runs on the reactor thread; the `enqueue_resume` waker
/// performs its own liveness check + atomic `Suspended → Runnable` CAS, so an
/// actor freed concurrently drops the wake.
///
/// Does NOT release the reactor's slot ref — the caller drops the owning
/// `Registration`, whose `Drop` impl is the single authority for that release.
fn resume_with_status(
    actor_local: *mut HewActor,
    read_slot: *mut crate::read_slot::HewReadSlot,
    status: crate::read_slot::ReadStatus,
) {
    // SAFETY: the reactor holds a ref to `read_slot` (taken in
    // `reactor_await_read`); the deposit checks the cancelled flag before
    // publishing.
    let should_wake = unsafe { crate::read_slot::read_slot_deposit_status(read_slot, status) };
    if should_wake && !actor_local.is_null() {
        // SAFETY: `enqueue_resume` does NOT trust the pointer — it re-confirms
        // liveness under the registry lock and only flips state + enqueues; a
        // freed actor drops the wake.
        unsafe { crate::scheduler::enqueue_resume(actor_local, std::ptr::null_mut()) };
    }
}

// ---------------------------------------------------------------------------
// Public registration API (called from FFI in 4c)
// ---------------------------------------------------------------------------

/// Register a TCP connection handle for active-mode delivery to an actor.
///
/// Sets the socket non-blocking, snapshots the actor-ref, and queues the fd
/// for the reactor to add to its poller. Returns 0 on success, -1 on failure
/// (reactor could not start, unknown conn handle, or non-blocking set failed).
///
/// # Safety
///
/// `actor_ref` must point to a valid [`HewActorRef`] for the duration of this
/// call (a by-value snapshot is taken). `conn` must be a valid TCP connection
/// handle obtained from the stdlib `net` API.
pub(crate) unsafe fn reactor_attach(
    conn: c_int,
    actor_ref: *const HewActorRef,
    on_data_type: i32,
    on_close_type: i32,
) -> c_int {
    if actor_ref.is_null() {
        crate::set_last_error("hew_tcp_attach: null actor reference");
        return -1;
    }
    let Some(fd) = tcp_conn_raw_fd(conn) else {
        crate::set_last_error("hew_tcp_attach: unknown TCP connection handle");
        return -1;
    };
    if !tcp_conn_set_nonblocking(conn, true) {
        crate::set_last_error("hew_tcp_attach: failed to set connection non-blocking");
        return -1;
    }
    if !ensure_reactor_started() {
        // last_error set by ensure_reactor_started; restore blocking mode.
        let _ = tcp_conn_set_nonblocking(conn, false);
        return -1;
    }

    // SAFETY: caller guarantees actor_ref is valid for this call; we copy it.
    let snapshot = unsafe { std::ptr::read(actor_ref) };
    let actor_local = actor_ref_local_ptr(&snapshot).cast::<HewActor>();
    let actor_key = actor_local as usize;

    // Fail closed on a leak-prone mailbox. Active-mode `on_data` is delivered
    // as a raw (`envelope == null`) node whose embedded `BytesTriple` refcount
    // is dropped ONLY by the handler that consumes it. A bounded `DropOld` /
    // `Coalesce` mailbox can evict (or in-place replace) a queued node before
    // its handler ever runs, freeing the triple container as plain bytes and
    // leaking the underlying refcounted buffer. Refuse the attach rather than
    // ship that leak (or risk a double-free were the node given evict-time
    // drop glue the happy path would re-run). Unbounded / `DropNew` / `Fail` /
    // `Block` mailboxes never evict a queued node and are accepted.
    if !actor_local.is_null() {
        // SAFETY: liveness is the caller's contract for the duration of attach;
        // we read the (possibly null) mailbox pointer and classify its policy.
        let leak_prone = unsafe {
            let mb = (*actor_local).mailbox.cast::<crate::mailbox::HewMailbox>();
            crate::mailbox::mailbox_overflow_evicts_queued_payload(mb)
        };
        if leak_prone {
            let _ = tcp_conn_set_nonblocking(conn, false);
            crate::set_last_error(
                "hew_tcp_attach: active-mode on_data requires an unbounded, DropNew, Fail, \
                 or Block mailbox; a DropOld/Coalesce mailbox would leak evicted read \
                 buffers",
            );
            return -1;
        }
    }

    let reg = Registration {
        conn,
        actor_ref: snapshot,
        actor_key,
        mode: RegMode::AutoSend {
            on_data_type,
            on_close_type,
        },
        closed: false,
    };
    REACTOR_STATE.access(|state| {
        state.pending.push(Pending::Add { fd, reg });
    });
    0
}

/// Register a TCP connection for a SUSPENDING `await conn.read()` (NEW-1, the
/// resume-mode sibling of [`reactor_attach`]).
///
/// The suspending handler has created a [`crate::read_slot::HewReadSlot`] (held
/// across its suspend in the coro frame) and registered its parked continuation
/// on the actor. This sets the socket non-blocking, snapshots the actor-ref,
/// takes a REACTOR ref on the slot, and queues the fd for the reactor to add to
/// its poller as a [`RegMode::Resume`] registration. When the reactor reports
/// the fd ready it reads the available bytes, deposits the result into the slot,
/// and `enqueue_resume`s the parked continuation.
///
/// One-shot: an `await conn.read()` reads once; the handler re-registers on its
/// next `await`. Reuses the SAME eviction-prone mailbox refusal is NOT needed —
/// the resume path delivers via the slot, not the mailbox, so no `on_data`
/// eviction-leak hazard exists.
///
/// Returns 0 on success, -1 on failure (reactor could not start, unknown conn
/// handle, or non-blocking set failed). On failure the caller's slot ref is
/// untouched (the abandon/err edge frees it); no reactor ref was taken.
///
/// # Safety
///
/// `actor_ref` must point to a valid [`HewActorRef`] for the duration of this
/// call (a by-value snapshot is taken). `conn` must be a valid TCP connection
/// handle. `read_slot` must be a valid live `HewReadSlot` the caller holds a ref
/// to (a reactor ref is taken on success).
pub(crate) unsafe fn reactor_await_read(
    conn: c_int,
    actor_ref: *const HewActorRef,
    read_slot: *mut crate::read_slot::HewReadSlot,
) -> c_int {
    if actor_ref.is_null() {
        crate::set_last_error("hew_conn_await_read: null actor reference");
        return -1;
    }
    if read_slot.is_null() {
        crate::set_last_error("hew_conn_await_read: null read slot");
        return -1;
    }
    let Some(fd) = tcp_conn_raw_fd(conn) else {
        crate::set_last_error("hew_conn_await_read: unknown TCP connection handle");
        return -1;
    };
    if !tcp_conn_set_nonblocking(conn, true) {
        crate::set_last_error("hew_conn_await_read: failed to set connection non-blocking");
        return -1;
    }
    if !ensure_reactor_started() {
        let _ = tcp_conn_set_nonblocking(conn, false);
        return -1;
    }

    // SAFETY: caller guarantees actor_ref is valid for this call; we copy it.
    let snapshot = unsafe { std::ptr::read(actor_ref) };
    let actor_local = actor_ref_local_ptr(&snapshot).cast::<HewActor>();
    let actor_key = actor_local as usize;

    // Take the REACTOR ref on the slot BEFORE queueing the registration, so the
    // slot cannot be freed out from under the reactor between the queue and the
    // promotion. The ref is owned by the `Registration` and released by its
    // `Drop` (the single authority) on removal.
    // SAFETY: caller holds a ref, so the slot is live for this retain.
    unsafe { crate::read_slot::read_slot_retain(read_slot) };

    let reg = Registration {
        conn,
        actor_ref: snapshot,
        actor_key,
        mode: RegMode::Resume { read_slot },
        closed: false,
    };
    REACTOR_STATE.access(|state| {
        state.pending.push(Pending::Add { fd, reg });
    });
    0
}

/// Register a TCP listener for a SUSPENDING `await listener.accept()` (NEW-2,
/// the listener-readiness sibling of [`reactor_await_read`]).
///
/// The suspending handler has created a [`crate::read_slot::HewReadSlot`] (held
/// across its suspend in the coro frame) and registered its parked continuation
/// on the actor. This sets the listener non-blocking, snapshots the actor-ref,
/// takes a REACTOR ref on the slot, and queues the listener fd for the reactor to
/// add to its poller as a [`RegMode::Accept`] registration. When the reactor
/// reports the listener readable it `accept()`s one connection, deposits its i64
/// handle into the slot, and `enqueue_resume`s the parked continuation.
///
/// One-shot: an `await listener.accept()` accepts once; the handler re-registers
/// on its next `await`.
///
/// Returns 0 on success, -1 on failure (reactor could not start, unknown listener
/// handle, or non-blocking set failed). On failure the caller's slot ref is
/// untouched (the abandon/err edge frees it); no reactor ref was taken.
///
/// # Safety
///
/// `actor_ref` must point to a valid [`HewActorRef`] for the duration of this
/// call (a by-value snapshot is taken). `listener` must be a valid TCP listener
/// handle. `read_slot` must be a valid live `HewReadSlot` the caller holds a ref
/// to (a reactor ref is taken on success).
pub(crate) unsafe fn reactor_await_accept(
    listener: c_int,
    actor_ref: *const HewActorRef,
    read_slot: *mut crate::read_slot::HewReadSlot,
) -> c_int {
    if actor_ref.is_null() {
        crate::set_last_error("hew_listener_await_accept: null actor reference");
        return -1;
    }
    if read_slot.is_null() {
        crate::set_last_error("hew_listener_await_accept: null read slot");
        return -1;
    }
    let Some(fd) = tcp_listener_raw_fd(listener) else {
        crate::set_last_error("hew_listener_await_accept: unknown TCP listener handle");
        return -1;
    };
    if !tcp_listener_set_nonblocking(listener, true) {
        crate::set_last_error("hew_listener_await_accept: failed to set listener non-blocking");
        return -1;
    }
    if !ensure_reactor_started() {
        let _ = tcp_listener_set_nonblocking(listener, false);
        return -1;
    }

    // SAFETY: caller guarantees actor_ref is valid for this call; we copy it.
    let snapshot = unsafe { std::ptr::read(actor_ref) };
    let actor_local = actor_ref_local_ptr(&snapshot).cast::<HewActor>();
    let actor_key = actor_local as usize;

    // Take the REACTOR ref on the slot BEFORE queueing the registration (the same
    // single-authority discipline as `reactor_await_read`); the `Registration`'s
    // `Drop` releases it on removal.
    // SAFETY: caller holds a ref, so the slot is live for this retain.
    unsafe { crate::read_slot::read_slot_retain(read_slot) };

    let reg = Registration {
        // The accept registration's `conn` field carries the LISTENER handle the
        // fd belongs to (the reactor `accept()`s on it; it is not read as a
        // stream).
        conn: listener,
        actor_ref: snapshot,
        actor_key,
        mode: RegMode::Accept { read_slot },
        closed: false,
    };
    REACTOR_STATE.access(|state| {
        state.pending.push(Pending::Add { fd, reg });
    });
    0
}

/// Detach a connection handle from the reactor (queue an unregister).
pub(crate) fn reactor_detach_conn(conn: c_int) {
    REACTOR_STATE.access(|state| state.pending.push(Pending::Remove { conn }));
}

/// Remove every registration owned by `actor` (its `*mut HewActor` address).
/// Called SYNCHRONOUSLY from the actor-teardown hook
/// (`prepare_quiescent_actor_for_cleanup`) while the actor is still valid but
/// about to be freed.
///
/// Two-phase, mirroring `timer_periodic::cancel_all_timers_for_actor`:
///
/// 1. **Synchronously** remove the actor's registrations from BOTH the registry
///    AND the `pending` queue (under the lock) so the reactor's revalidation in
///    `handle_ready_fd` misses them and no NEW delivery to this actor can start,
///    and so a still-queued `attach` (`Pending::Add`) is never promoted into a
///    dangling registration after the actor is freed. Queue the poller-side
///    `EPOLL_CTL_DEL` for each evicted fd (only the reactor thread owns the
///    poller); the fd staying briefly in the poller is harmless because the
///    registry lookup already fails. A scrubbed `Pending::Add` was never given
///    to the poller, so it needs no `EPOLL_CTL_DEL`.
/// 2. **Scrub-then-wait loop** to quiescence. Each iteration waits out any
///    in-flight delivery/promotion guard for THIS actor (`DELIVERING_ACTOR` /
///    `PROMOTING_ACTOR == actor_key`), then **re-scrubs** the registry + pending
///    (a promotion that had already left `pending` may have inserted a
///    registration after the previous scrub), and re-checks quiescence. It
///    repeats until a re-scrub finds the key absent from BOTH `registry` and
///    `pending` with neither guard set. The scrub is ordered BEFORE the wait so
///    that any delivery the reactor begins afterwards re-validates under the lock
///    in `handle_ready_fd`, sees the registration gone, and aborts — and the
///    wait drains only a delivery the reactor had already published the guard for
///    before the scrub. Once this returns, no `hew_actor_try_send` to the actor
///    is or can become in flight, so the caller may free the actor box.
pub(crate) fn reactor_detach_actor(actor_key: usize) {
    // Fast path: nothing registered or pending, and no in-flight delivery or
    // promotion for this actor — avoid taking the lock / spinning.
    let has_state = REACTOR_STATE.access(|state| {
        !state.registry.is_empty() || !state.conn_to_fd.is_empty() || !state.pending.is_empty()
    });
    if !has_state
        && DELIVERING_ACTOR.load(Ordering::SeqCst) != actor_key
        && PROMOTING_ACTOR.load(Ordering::SeqCst) != actor_key
    {
        return;
    }

    // Phase 1: synchronously evict this actor's registrations (registry) AND its
    // still-queued attach requests (pending), then queue the poller-side removal
    // for the fds that were already registered.
    let fds = evict_actor_state(actor_key);
    queue_unregister_fds(&fds);

    // Phase 2: drain to quiescence with a *scrub-then-wait* loop. Each iteration
    //
    //   (B) re-scrub the registry + pending,
    //   (A) wait out any in-flight delivery/promotion guard for this actor,
    //
    // and repeats until a re-scrub observes the key fully quiescent: absent from
    // BOTH `registry` and `pending`, with neither `DELIVERING_ACTOR` nor
    // `PROMOTING_ACTOR` set to it. Phase 1 already did the first scrub, so the
    // loop body opens with the wait.
    //
    // Why scrub-then-wait (not the old wait-then-scrub): the registry scrub must
    // be ordered BEFORE the wait so that any delivery the reactor begins after
    // the scrub will re-validate under the lock (`handle_ready_fd`), find the
    // registration gone, and abort without sending. The only delivery the wait
    // must still drain is one the reactor published `DELIVERING_ACTOR` for
    // *before* this scrub removed the registration; the spin-wait covers exactly
    // that. The previous code waited once and then re-scrubbed with no trailing
    // wait, leaving a window: a delivery begun between the wait's exit and the
    // re-scrub (same poll cycle as a just-finished promotion) was never drained,
    // so the caller could free the actor box mid-`hew_actor_try_send`.
    //
    // Termination (cannot livelock): `reactor_detach_actor` runs synchronously
    // from the actor-teardown hook while the actor is quiescent, so the actor
    // itself enqueues no further `attach`. After phase 1 scrubbed `pending`, no
    // NEW `Pending::Add` for this key can ever appear. The reactor (single
    // thread, one promotion in flight at a time) can therefore promote only a
    // bounded, monotonically shrinking set of this key's adds: each scrub
    // removes any freshly-promoted registration, and no add can re-enter
    // `pending` to replace it. Once the last add is drained and scrubbed, the
    // reactor's re-validate fails for this key and it starts no further
    // delivery, so `DELIVERING_ACTOR`/`PROMOTING_ACTOR` are published for this
    // key at most a bounded number more times. The quiescent count strictly
    // decreases to zero, so the loop converges.
    loop {
        // (A) Wait out any in-flight delivery to, or promotion of, this actor.
        while DELIVERING_ACTOR.load(Ordering::SeqCst) == actor_key
            || PROMOTING_ACTOR.load(Ordering::SeqCst) == actor_key
        {
            std::hint::spin_loop();
        }

        // (B) Re-scrub: a promotion may have inserted a registration after the
        // previous scrub (it had already left `pending`, so that scrub could not
        // catch it). Evict any such entry and queue its poller-side removal.
        let late_fds = evict_actor_state(actor_key);
        queue_unregister_fds(&late_fds);

        // Quiescence check, ordered AFTER the scrub: the actor is fully drained
        // only when no registration or pending add remains AND no guard is set
        // for it. If a guard was (re)published, or an entry slipped in, between
        // the scrub and this check, loop again — the next scrub-then-wait drains
        // it. This re-establishes the Dekker ordering the single-wait code broke.
        let quiescent = REACTOR_STATE.access(|state| {
            let in_registry = state
                .registry
                .values()
                .any(|reg| reg.actor_key == actor_key);
            let in_pending = state.pending.iter().any(|req| match req {
                Pending::Add { reg, .. } => reg.actor_key == actor_key,
                _ => false,
            });
            !in_registry && !in_pending
        }) && DELIVERING_ACTOR.load(Ordering::SeqCst) != actor_key
            && PROMOTING_ACTOR.load(Ordering::SeqCst) != actor_key;

        if quiescent {
            return;
        }
    }
}

/// Remove the one-shot resume registration that owns `read_slot`.
///
/// This is the deadline/cancellation sibling of actor teardown: scrub the
/// registration before waking the parked actor so late readiness cannot deposit
/// into the cancelled slot. The `Registration` drop remains the single authority
/// for releasing the reactor-owned read-slot ref.
pub(crate) fn reactor_detach_read_slot(read_slot: *mut crate::read_slot::HewReadSlot) -> bool {
    if read_slot.is_null() {
        return false;
    }
    let fds = REACTOR_STATE.access(|state| {
        let owned: Vec<c_int> = state
            .registry
            .iter()
            .filter_map(|(fd, reg)| match reg.mode {
                RegMode::Resume { read_slot: slot } | RegMode::Accept { read_slot: slot }
                    if slot == read_slot =>
                {
                    Some(*fd)
                }
                _ => None,
            })
            .collect();
        for fd in &owned {
            state.registry.remove(fd);
            state.conn_to_fd.retain(|_, mapped| mapped != fd);
        }
        let before_pending = state.pending.len();
        state.pending.retain(|req| match req {
            Pending::Add { reg, .. } => match reg.mode {
                RegMode::Resume { read_slot: slot } | RegMode::Accept { read_slot: slot } => {
                    slot != read_slot
                }
                RegMode::AutoSend { .. } => true,
            },
            _ => true,
        });
        if !owned.is_empty() {
            crate::observe::record_reactor_unregistration(owned.len() as u64);
        }
        (owned, before_pending != state.pending.len())
    });
    queue_unregister_fds(&fds.0);
    !fds.0.is_empty() || fds.1
}

/// Remove every registry + pending entry owned by `actor_key` under the
/// [`ReactorState`] lock, returning the fds whose registrations were already in
/// the registry (those need a poller-side `EPOLL_CTL_DEL`). Scrubbed
/// `Pending::Add` entries were never handed to the poller, so they are not
/// returned.
fn evict_actor_state(actor_key: usize) -> Vec<c_int> {
    REACTOR_STATE.access(|state| {
        let owned: Vec<c_int> = state
            .registry
            .iter()
            .filter(|(_, reg)| reg.actor_key == actor_key)
            .map(|(fd, _)| *fd)
            .collect();
        for fd in &owned {
            state.registry.remove(fd);
            state.conn_to_fd.retain(|_, mapped| mapped != fd);
        }
        if !owned.is_empty() {
            crate::observe::record_reactor_unregistration(owned.len() as u64);
        }
        // Drop any still-queued attach requests for this actor so they are never
        // promoted into a dangling registration after the actor is freed. Their
        // conn handles never reached `conn_to_fd`, so nothing else references
        // them.
        state.pending.retain(|req| match req {
            Pending::Add { reg, .. } => reg.actor_key != actor_key,
            _ => true,
        });
        owned
    })
}

/// Queue a poller-side `EPOLL_CTL_DEL` for each fd on the reactor thread (the
/// sole poller owner). No-op for an empty slice.
fn queue_unregister_fds(fds: &[c_int]) {
    if fds.is_empty() {
        return;
    }
    REACTOR_STATE.access(|state| {
        for fd in fds {
            state.pending.push(Pending::UnregisterFd { fd: *fd });
        }
    });
}

/// Stop the reactor thread and clear the registry. Called from runtime
/// shutdown BEFORE actors are freed, so an in-flight poll cannot deliver to a
/// freed actor. Safe to call when the reactor was never started, and
/// idempotent.
pub(crate) fn reactor_shutdown() {
    REACTOR_STOP.store(true, Ordering::Release);
    if let Some(slot) = REACTOR_HANDLE.get() {
        if let Ok(mut guard) = slot.lock() {
            if let Some(handle) = guard.take() {
                let _ = handle.join();
            }
        }
    }
    REACTOR_RUNNING.store(false, Ordering::SeqCst);
    REACTOR_STOP.store(false, Ordering::SeqCst);
    // The reactor thread has been joined, so no promotion can be in flight;
    // clear the guard in case it was left set by an aborted drain.
    PROMOTING_ACTOR.store(0, Ordering::SeqCst);
    REACTOR_STATE.access(|state| {
        state.registry.clear();
        state.conn_to_fd.clear();
        state.pending.clear();
    });
}

#[cfg(test)]
pub(crate) fn pending_count_for_test() -> usize {
    REACTOR_STATE.access(|state| state.pending.len())
}

#[cfg(test)]
pub(crate) fn registration_count_for_test() -> usize {
    REACTOR_STATE.access(|state| state.registry.len())
}

/// Inject a registration directly into the registry (test-only), bypassing the
/// pending queue and the OS poller register. Lets the dead-actor and
/// detach-by-actor races be exercised without a live scheduler.
#[cfg(test)]
#[cfg_attr(
    not(unix),
    allow(
        dead_code,
        reason = "only consumed by unix-gated reactor engine tests (pipe-fd based)"
    )
)]
pub(crate) fn inject_registration_for_test(
    fd: c_int,
    conn: c_int,
    actor_ref: HewActorRef,
    actor_key: usize,
) {
    REACTOR_STATE.access(|state| {
        state.conn_to_fd.insert(conn, fd);
        state.registry.insert(
            fd,
            Registration {
                conn,
                actor_ref,
                actor_key,
                mode: RegMode::AutoSend {
                    on_data_type: 1,
                    on_close_type: 2,
                },
                closed: false,
            },
        );
    });
}

/// Inject a RESUME-mode registration directly into the registry (test-only),
/// bypassing the pending queue + OS poller. The reactor ref on `read_slot` must
/// be taken by the caller (mirrors `reactor_await_read`'s retain); the
/// registration's `Drop` releases it on removal. Lets the resume branch + the
/// detach scrub be exercised against a real read slot without a live socket.
#[cfg(test)]
pub(crate) fn inject_resume_registration_for_test(
    fd: c_int,
    conn: c_int,
    actor_ref: HewActorRef,
    actor_key: usize,
    read_slot: *mut crate::read_slot::HewReadSlot,
) {
    REACTOR_STATE.access(|state| {
        state.conn_to_fd.insert(conn, fd);
        state.registry.insert(
            fd,
            Registration {
                conn,
                actor_ref,
                actor_key,
                mode: RegMode::Resume { read_slot },
                closed: false,
            },
        );
    });
}

/// Drive `handle_ready_fd` against a given poller (test-only) so the
/// dead-actor-drop path can be exercised deterministically.
#[cfg(test)]
pub(crate) fn handle_ready_fd_for_test(poller: *mut HewIoPoller, fd: c_int, events: c_int) {
    handle_ready_fd(poller, fd, events);
}

// Test-only thread-local: the most-recently accepted connection handle inside
// `handle_ready_accept`. Lets the accept/abandon-race test assert that the
// specific accepted handle was closed without depending on the global
// `TCP_API_STATE.streams` count (which concurrent transport tests also modify).
#[cfg(test)]
std::thread_local! {
    pub(crate) static LAST_ACCEPTED_CONN_FOR_TEST: std::cell::Cell<c_int> =
        const { std::cell::Cell::new(-1) };
}

/// Drive `handle_ready_accept` directly (test-only) against a registered LISTENER
/// fd, bypassing the `handle_ready_fd` snapshot + liveness gate. Lets the NEW-2
/// accept/abandon-race close discipline be exercised deterministically: the
/// reactor accepts a real connection, then the cancelled-slot deposit fails and
/// the just-accepted handle must be closed (not leaked). The deposit-fail path
/// never touches `actor_local`, so a dead actor-ref is sufficient.
#[cfg(test)]
pub(crate) fn handle_ready_accept_for_test(
    poller: *mut HewIoPoller,
    fd: c_int,
    listener_conn: c_int,
    actor_ref: HewActorRef,
    read_slot: *mut crate::read_slot::HewReadSlot,
    hard_close: bool,
) {
    let snap = ReadySnapshot {
        conn: listener_conn,
        actor_local: actor_ref_local_ptr(&actor_ref).cast::<HewActor>(),
        actor_ref,
        mode: ReadyMode::Accept { read_slot },
        already_closed: false,
    };
    handle_ready_accept(poller, fd, &snap, hard_close, read_slot);
}

/// Drive `deliver_close_once` against a given poller/fd/actor (test-only) so the
/// terminal-close delivery can be exercised against a real (live) actor mailbox
/// without standing up a TCP socket + scheduler. The fd need not be registered;
/// `deliver_close_once` tolerates a missing registry entry (it only marks
/// `closed` if present) and still unregisters the fd from the poller.
#[cfg(test)]
pub(crate) fn deliver_close_once_for_test(
    poller: *mut HewIoPoller,
    fd: c_int,
    actor_local: *mut HewActor,
    on_close_type: i32,
    already_closed: bool,
) {
    deliver_close_once(poller, fd, actor_local, on_close_type, already_closed);
}

/// Publish the in-flight-delivery guard for a given actor key (test-only),
/// simulating the window during which the reactor thread is mid-`hew_actor_try_send`
/// inside `handle_ready_fd`. Lets the Dekker Phase-2 spin-wait in
/// `reactor_detach_actor` be exercised deterministically without pausing a real
/// send. Pass `0` to clear (delivery finished).
#[cfg(test)]
pub(crate) fn set_delivering_actor_for_test(actor_key: usize) {
    DELIVERING_ACTOR.store(actor_key, Ordering::SeqCst);
}

/// Publish the promotion guard for a given actor key (test-only), simulating
/// the window during which the reactor thread is mid-`apply_add` for a queued
/// attach. Lets the Phase-2 spin-wait + registry re-scrub in
/// `reactor_detach_actor` be exercised deterministically. Pass `0` to clear.
#[cfg(test)]
pub(crate) fn set_promoting_actor_for_test(actor_key: usize) {
    PROMOTING_ACTOR.store(actor_key, Ordering::SeqCst);
}

/// Enqueue a `Pending::Add` for a given fd/conn/actor (test-only), exercising
/// the real attach → pending → promote path without a live socket. The actor
/// ref is a by-value snapshot; `actor_key` is the stable identity used by
/// `reactor_detach_actor`'s pending scrub.
#[cfg(test)]
#[cfg_attr(
    not(unix),
    allow(
        dead_code,
        reason = "only consumed by unix-gated reactor engine tests (pipe-fd based)"
    )
)]
pub(crate) fn enqueue_pending_add_for_test(
    fd: c_int,
    conn: c_int,
    actor_ref: HewActorRef,
    actor_key: usize,
) {
    REACTOR_STATE.access(|state| {
        state.pending.push(Pending::Add {
            fd,
            reg: Registration {
                conn,
                actor_ref,
                actor_key,
                mode: RegMode::AutoSend {
                    on_data_type: 1,
                    on_close_type: 2,
                },
                closed: false,
            },
        });
    });
}

/// Run `f` while holding the `REACTOR_STATE` lock (test-only). Lets a test
/// force `reactor_detach_actor`'s phase-2 re-scrub (which takes this lock) to
/// block, so the test can deterministically publish the `DELIVERING_ACTOR`
/// guard in the window between the spin-wait exit and the re-scrub — the
/// promote-during-detach UAF window. Insert a registration here to model the
/// reactor's `apply_add` landing the entry while detach is mid-teardown.
#[cfg(test)]
fn with_reactor_state_locked_for_test<R>(f: impl FnOnce(&mut ReactorState) -> R) -> R {
    REACTOR_STATE.access(f)
}

#[cfg(test)]
thread_local! {
    static FAIL_REACTOR_SPAWN: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };

    /// Mid-deposit injection hook (P1-A forced-ordering). Fires inside
    /// `handle_ready_resume` immediately BEFORE the lock-free deposit, after the
    /// in-flight slot ref has been taken. A test installs it to run the exact
    /// scrub-then-creator-free ordering at the mid-deposit point, so the deposit
    /// that follows lands while the registration-owned ref is already gone — the
    /// P1-A window. With the in-flight ref the slot survives the deposit; without
    /// it the deposit is a use-after-free (a sanitizer trap).
    static RESUME_PRE_DEPOSIT_HOOK: std::cell::RefCell<Option<Box<dyn Fn()>>> =
        const { std::cell::RefCell::new(None) };
}

#[cfg(test)]
fn should_fail_reactor_spawn() -> bool {
    FAIL_REACTOR_SPAWN.with(std::cell::Cell::get)
}

/// Install (or clear) the mid-deposit hook. Test-only.
#[cfg(test)]
fn set_resume_pre_deposit_hook(hook: Option<Box<dyn Fn()>>) {
    RESUME_PRE_DEPOSIT_HOOK.with(|h| *h.borrow_mut() = hook);
}

/// Fire the mid-deposit hook if installed (test-only). Called by
/// `handle_ready_resume` just before the deposit.
#[cfg(test)]
fn fire_resume_pre_deposit_hook() {
    // Take the closure out so a re-entrant deposit cannot re-fire it (one-shot).
    let hook = RESUME_PRE_DEPOSIT_HOOK.with(|h| h.borrow_mut().take());
    if let Some(hook) = hook {
        hook();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Mutex as StdMutex;
    use std::time::{Duration, Instant};

    /// Serialises every test touching the process-wide reactor globals.
    static REACTOR_TEST_MUTEX: StdMutex<()> = StdMutex::new(());

    fn reset_reactor() {
        reactor_shutdown();
        REACTOR_STATE.access(|state| {
            state.registry.clear();
            state.conn_to_fd.clear();
            state.pending.clear();
        });
        // Clear the Dekker in-flight + promotion guards so a prior test cannot
        // leave either set.
        set_delivering_actor_for_test(0);
        set_promoting_actor_for_test(0);
    }

    // 4a oracle (unit form): the reactor starts on demand, runs its poll loop,
    // and stops with a clean join — no hung thread — within a bounded window.
    #[test]
    fn reactor_starts_and_stops_cleanly() {
        let _guard = REACTOR_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_reactor();

        assert!(ensure_reactor_started(), "reactor should start");
        assert!(REACTOR_RUNNING.load(Ordering::SeqCst));

        let start = Instant::now();
        reactor_shutdown();
        let elapsed = start.elapsed();

        assert!(
            !REACTOR_RUNNING.load(Ordering::SeqCst),
            "reactor must be marked stopped after shutdown"
        );
        // The poll timeout is 50 ms; a clean join must complete within a couple
        // of poll cycles. A hung thread would blow far past this.
        assert!(
            elapsed < Duration::from_millis(500),
            "reactor join took too long ({elapsed:?}) — possible hung thread"
        );
    }

    // `ensure_reactor_started` is idempotent: a second call is a no-op.
    #[test]
    fn reactor_start_is_idempotent() {
        let _guard = REACTOR_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_reactor();

        assert!(ensure_reactor_started());
        assert!(ensure_reactor_started(), "second start is a harmless no-op");
        reactor_shutdown();
    }

    // shutdown without a prior start (and a double shutdown) must not panic or
    // hang — the cleanup-all-exits invariant on the "never started" path.
    #[test]
    fn reactor_shutdown_without_start_is_safe() {
        let _guard = REACTOR_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_reactor();
        reactor_shutdown();
        reactor_shutdown();
        assert!(!REACTOR_RUNNING.load(Ordering::SeqCst));
    }

    // A spawn failure fails closed: the reactor is not marked running and the
    // last-error carries context (mirrors the timer-ticker spawn-failure test).
    #[test]
    fn reactor_spawn_failure_fails_closed() {
        let _guard = REACTOR_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_reactor();

        FAIL_REACTOR_SPAWN.with(|f| f.set(true));
        let started = ensure_reactor_started();
        FAIL_REACTOR_SPAWN.with(|f| f.set(false));

        assert!(!started, "spawn failure must report failure");
        assert!(
            !REACTOR_RUNNING.load(Ordering::SeqCst),
            "failed start must not leave the reactor marked running"
        );
    }

    // The synchronous detach takes the fast path when nothing is registered and
    // no delivery is in flight to the actor: no pending mutation, no spin.
    #[test]
    fn detach_actor_fast_path_when_empty() {
        let _guard = REACTOR_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_reactor();

        reactor_detach_actor(0xdead_beef);
        assert_eq!(
            pending_count_for_test(),
            0,
            "empty registry must not enqueue a poller-unregister request"
        );
    }

    // A detach-by-conn request is queued for the reactor to apply.
    #[test]
    fn detach_conn_enqueues_pending_remove() {
        let _guard = REACTOR_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_reactor();

        reactor_detach_conn(7);
        assert_eq!(pending_count_for_test(), 1);
        // Clear so the next test starts clean.
        reset_reactor();
        assert_eq!(pending_count_for_test(), 0);
    }

    // ---- 4b: fd→mailbox registration + actor-liveness race ----------------

    /// A REMOTE actor-ref with an invalid connection handle is reported dead by
    /// `hew_actor_ref_is_alive` (transport.rs), letting us exercise the
    /// dead-actor-drop path without constructing a full `HewActor` fixture.
    fn dead_actor_ref() -> HewActorRef {
        // SAFETY: hew_actor_ref_remote has no preconditions; a null transport
        // with HEW_CONN_INVALID is the canonical "dead remote ref".
        unsafe { crate::transport::hew_actor_ref_remote(0, -1, std::ptr::null_mut()) }
    }

    #[cfg(unix)]
    fn make_pipe() -> (c_int, c_int) {
        let mut fds = [0i32; 2];
        // SAFETY: fds is a valid 2-element array.
        assert_eq!(unsafe { libc::pipe(fds.as_mut_ptr()) }, 0);
        (fds[0], fds[1])
    }

    // 4b oracle (unit form): a readiness event for an actor that has stopped
    // (dead actor-ref) is DROPPED — no delivery — and the fd is unregistered,
    // so a post-stop readiness event never reaches a freed actor.
    #[cfg(unix)]
    #[test]
    fn ready_event_for_dead_actor_is_dropped_and_unregistered() {
        let _guard = REACTOR_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_reactor();

        // SAFETY: no preconditions for new.
        let poller = unsafe { hew_io_poller_new() };
        assert!(!poller.is_null());
        let (rfd, wfd) = make_pipe();
        // SAFETY: poller valid; rfd is a live read fd; dummy actor never used by
        // the readiness-reporting path.
        let rc =
            unsafe { hew_io_poller_register(poller, rfd, std::ptr::null_mut(), 0, HEW_IO_READ) };
        assert_eq!(rc, 0);

        // Register a dead actor against this fd, then drive a readiness event.
        inject_registration_for_test(rfd, 100, dead_actor_ref(), 0xabc);
        assert_eq!(registration_count_for_test(), 1);

        // The actor is dead → the event must be dropped and the fd removed.
        handle_ready_fd_for_test(poller, rfd, HEW_IO_READ);
        assert_eq!(
            registration_count_for_test(),
            0,
            "a readiness event for a dead actor must unregister the fd, not deliver"
        );

        // SAFETY: closing our own fds and surrendering the poller.
        unsafe {
            libc::close(rfd);
            libc::close(wfd);
            hew_io_poller_stop(poller);
        }
        reset_reactor();
    }

    // The actor-teardown hook path: detach-by-actor removes every registration
    // owned by that actor key (so a stopped actor leaks no fd registration).
    #[cfg(unix)]
    #[test]
    fn detach_actor_removes_all_registrations_for_that_actor() {
        let _guard = REACTOR_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_reactor();

        // SAFETY: no preconditions for new.
        let poller = unsafe { hew_io_poller_new() };
        assert!(!poller.is_null());
        let (rfd1, wfd1) = make_pipe();
        let (rfd2, wfd2) = make_pipe();
        // SAFETY: poller valid; rfds live; dummy actor unused in this path.
        unsafe {
            assert_eq!(
                hew_io_poller_register(poller, rfd1, std::ptr::null_mut(), 0, HEW_IO_READ),
                0
            );
            assert_eq!(
                hew_io_poller_register(poller, rfd2, std::ptr::null_mut(), 0, HEW_IO_READ),
                0
            );
        }

        // Two fds owned by the same actor key, one by a different actor.
        inject_registration_for_test(rfd1, 201, dead_actor_ref(), 0xAA);
        inject_registration_for_test(rfd2, 202, dead_actor_ref(), 0xAA);
        let (rfd3, wfd3) = make_pipe();
        // SAFETY: poller valid; rfd3 live.
        unsafe {
            assert_eq!(
                hew_io_poller_register(poller, rfd3, std::ptr::null_mut(), 0, HEW_IO_READ),
                0
            );
        }
        inject_registration_for_test(rfd3, 203, dead_actor_ref(), 0xBB);
        assert_eq!(registration_count_for_test(), 3);

        // The synchronous actor-teardown detach removes actor 0xAA's
        // registrations immediately (under the lock) and queues the poller
        // EPOLL_CTL_DEL for the reactor thread.
        reactor_detach_actor(0xAA);
        assert_eq!(
            registration_count_for_test(),
            1,
            "only the other actor's registration should remain after detach"
        );

        // SAFETY: closing our own fds and surrendering the poller.
        unsafe {
            libc::close(rfd1);
            libc::close(wfd1);
            libc::close(rfd2);
            libc::close(wfd2);
            libc::close(rfd3);
            libc::close(wfd3);
            hew_io_poller_stop(poller);
        }
        reset_reactor();
    }

    // ---- NEW-1 resume-mode registration (await conn.read) -----------------
    //
    // A resume-mode `Registration` carries a `HewReadSlot` instead of the
    // `on_data`/`on_close` msg types. On fd readiness the reactor deposits the
    // read result into the slot + `enqueue_resume`s the parked continuation
    // (instead of auto-sending a mailbox message). The detach scrub keyed on
    // `actor_key` covers it UNCHANGED, and dropping the registration releases the
    // reactor's slot ref (the abandon-edge no-leak property).

    /// Slice 1: a resume-mode registration is scrubbed by `reactor_detach_actor`
    /// exactly like an active-mode one (the scrub is keyed on `actor_key`, mode-
    /// agnostic), and dropping the evicted registration releases the reactor's
    /// slot ref so the slot is reclaimed once the creator ref also drops (the
    /// abandon edge: handler freed while parked on the fd).
    #[cfg(unix)]
    #[test]
    #[allow(
        clippy::undocumented_unsafe_blocks,
        reason = "test-only FFI: every pointer is a fresh local slot/poller/conn the \
                  test body sets up and tears down; the lifecycle is described inline"
    )]
    fn resume_registration_scrubbed_by_detach_releases_slot_ref() {
        const KEY: usize = 0x00DE_AD01;

        let _guard = REACTOR_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_reactor();

        let (rfd, wfd) = make_pipe();
        // Creator ref (models the suspending handler's frame ref).
        let slot = crate::read_slot::hew_read_slot_new();
        // Reactor ref (models `reactor_await_read`'s retain), owned by the reg.
        // SAFETY: creator holds a ref so the slot is live.
        unsafe { crate::read_slot::read_slot_retain(slot) };
        inject_resume_registration_for_test(rfd, 701, dead_actor_ref(), KEY, slot);
        assert_eq!(registration_count_for_test(), 1);

        // The handler is freed while parked: detach scrubs the registration and
        // its Drop releases the reactor slot ref.
        reactor_detach_actor(KEY);
        assert_eq!(
            registration_count_for_test(),
            0,
            "resume-mode registration must be scrubbed by detach like any other"
        );

        // The abandon edge cancels + frees the creator ref. With both refs gone
        // the slot is reclaimed (no leak, no double-free).
        // SAFETY: slot is still live (creator ref held).
        unsafe { crate::read_slot::hew_read_slot_cancel(slot) };
        unsafe { crate::read_slot::hew_read_slot_free(slot) };

        // SAFETY: closing our own fds.
        unsafe {
            libc::close(rfd);
            libc::close(wfd);
        }
        reset_reactor();
    }

    /// Slice 2 (liveness abort): a resume-mode readiness for a DEAD actor (the
    /// handler was freed while parked) drops the event WITHOUT reading or
    /// depositing — the slot stays `Pending` and the fd is unregistered. The
    /// reactor's liveness re-validation (shared with active mode) protects the
    /// resume branch exactly as it protects the auto-send branch. The live-actor
    /// data-routing + wake path is exercised end-to-end in
    /// `scheduler::tests::reactor_data_deposit_resumes_parked_handler_with_bytes`.
    #[test]
    #[allow(
        clippy::undocumented_unsafe_blocks,
        reason = "test-only FFI: every pointer is a fresh local slot/poller/conn the \
                  test body sets up and tears down; the lifecycle is described inline"
    )]
    fn resume_readiness_for_dead_actor_drops_event_no_deposit() {
        use std::io::Write;
        const KEY: usize = 0x00DA_7A01;

        let _guard = REACTOR_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_reactor();

        // SAFETY: no preconditions for new.
        let poller = unsafe { hew_io_poller_new() };
        assert!(!poller.is_null());
        let (conn, mut client) = crate::transport::tcp_socketpair_conn_for_test();
        let fd = crate::transport::tcp_conn_raw_fd(conn).expect("conn fd");
        assert!(crate::transport::tcp_conn_set_nonblocking(conn, true));
        // SAFETY: poller valid; fd live; dummy actor unused by the reporting path.
        assert_eq!(
            unsafe { hew_io_poller_register(poller, fd, std::ptr::null_mut(), 0, HEW_IO_READ) },
            0
        );

        let slot = crate::read_slot::hew_read_slot_new();
        // SAFETY: creator ref held.
        unsafe { crate::read_slot::read_slot_retain(slot) }; // reactor ref (owned by reg)
        inject_resume_registration_for_test(fd, conn, dead_actor_ref(), KEY, slot);

        // Peer writes bytes that would be read IF the actor were alive.
        client
            .write_all(b"unread-because-dead")
            .expect("client write");
        client.flush().ok();
        std::thread::sleep(Duration::from_millis(20));

        handle_ready_fd_for_test(poller, fd, HEW_IO_READ);

        // The dead-actor liveness guard unregistered the fd WITHOUT reading.
        assert_eq!(
            registration_count_for_test(),
            0,
            "a readiness event for a dead actor must unregister the fd"
        );
        // No deposit landed — the slot is still Pending (no garbage bytes).
        assert_eq!(
            unsafe { crate::read_slot::hew_read_slot_status(slot) },
            crate::read_slot::ReadStatus::Pending as i32,
            "a dead-actor readiness must not deposit a read result"
        );
        // Creator ref free reclaims the slot.
        // SAFETY: creator ref held.
        unsafe { crate::read_slot::hew_read_slot_free(slot) };

        drop(client);
        // SAFETY: cleanup.
        unsafe {
            crate::transport::tcp_close_raw_for_test(conn);
            hew_io_poller_stop(poller);
        }
        reset_reactor();
    }

    /// Slice 2 (abandon): a resume-mode readiness whose slot was CANCELLED (the
    /// handler abandoned before the deposit) drops the deposit + suppresses the
    /// wake — no buffer leak, no wake to a freed actor.
    #[test]
    #[allow(
        clippy::undocumented_unsafe_blocks,
        reason = "test-only FFI: every pointer is a fresh local slot/poller/conn the \
                  test body sets up and tears down; the lifecycle is described inline"
    )]
    fn resume_cancelled_slot_drops_deposit_no_wake() {
        use std::io::Write;
        const KEY: usize = 0x00CA_4CE1;

        let _guard = REACTOR_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_reactor();

        // SAFETY: no preconditions.
        let poller = unsafe { hew_io_poller_new() };
        assert!(!poller.is_null());
        let (conn, mut client) = crate::transport::tcp_socketpair_conn_for_test();
        let fd = crate::transport::tcp_conn_raw_fd(conn).expect("conn fd");
        assert!(crate::transport::tcp_conn_set_nonblocking(conn, true));
        // SAFETY: poller valid; fd live.
        assert_eq!(
            unsafe { hew_io_poller_register(poller, fd, std::ptr::null_mut(), 0, HEW_IO_READ) },
            0
        );

        let slot = crate::read_slot::hew_read_slot_new();
        // SAFETY: creator ref held.
        unsafe { crate::read_slot::read_slot_retain(slot) }; // reactor ref
                                                             // Abandon edge fired first: the slot is cancelled.
        unsafe { crate::read_slot::hew_read_slot_cancel(slot) };
        inject_resume_registration_for_test(fd, conn, dead_actor_ref(), KEY, slot);

        // Make the fd readable.
        client.write_all(b"dropped").expect("client write");
        client.flush().ok();
        std::thread::sleep(Duration::from_millis(20));

        handle_ready_fd_for_test(poller, fd, HEW_IO_READ);

        // The deposit was dropped (cancelled): status is typed, no buffer.
        assert_eq!(
            unsafe { crate::read_slot::hew_read_slot_status(slot) },
            crate::read_slot::ReadStatus::Cancelled as i32,
            "a cancelled slot must not receive a data deposit"
        );
        // Creator ref free reclaims the slot (no leak).
        // SAFETY: creator ref held.
        unsafe { crate::read_slot::hew_read_slot_free(slot) };

        drop(client);
        // SAFETY: cleanup.
        unsafe {
            crate::transport::tcp_close_raw_for_test(conn);
            hew_io_poller_stop(poller);
        }
        reset_reactor();
    }

    /// F1 (NEW-2 accept/abandon race): the reactor accepts a connection but the
    /// suspended handler was abandoned/cancelled before the deposit lands. The
    /// just-accepted `Connection` handle has no resume owner, so
    /// `handle_ready_accept` MUST close it — the `TCP_API_STATE.streams` table
    /// must NOT grow (the pre-fix bug leaked the socket here, a path to fd
    /// exhaustion under a peer connecting to a stopped/cancelled acceptor).
    #[test]
    #[allow(
        clippy::undocumented_unsafe_blocks,
        reason = "test-only FFI: every pointer is a fresh local slot/poller/listener the \
                  test body sets up and tears down; the lifecycle is described inline"
    )]
    fn accept_deposit_failure_closes_handle_no_stream_leak() {
        let _guard = REACTOR_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_reactor();

        // SAFETY: no preconditions.
        let poller = unsafe { hew_io_poller_new() };
        assert!(!poller.is_null());

        // A listener with exactly one connection queued (the client stays alive).
        let (listener, client) = crate::transport::tcp_listener_with_pending_conn_for_test();
        let fd = crate::transport::tcp_listener_raw_fd(listener).expect("listener fd");
        // Register the listener fd so the terminal `unregister_fd` is a clean no-op.
        // SAFETY: poller valid; fd live.
        assert_eq!(
            unsafe { hew_io_poller_register(poller, fd, std::ptr::null_mut(), 0, HEW_IO_READ) },
            0
        );

        let accepts_before = crate::transport::tcp_counters_snapshot().accept_count;
        // Reset the per-test thread-local so a stale value from a prior call
        // cannot mask a WouldBlock (no-accept) outcome.
        LAST_ACCEPTED_CONN_FOR_TEST.set(-1);

        let slot = crate::read_slot::hew_read_slot_new();
        // SAFETY: creator ref held.
        unsafe { crate::read_slot::read_slot_retain(slot) }; // reactor ref
                                                             // Abandon edge fired first: the slot is cancelled before the deposit.
                                                             // SAFETY: creator + reactor refs held.
        unsafe { crate::read_slot::hew_read_slot_cancel(slot) };

        // Drive the accept-readiness handler directly. It accepts a real
        // connection, then the cancelled-slot deposit returns `false`.
        handle_ready_accept_for_test(poller, fd, listener, dead_actor_ref(), slot, false);

        // The accept actually happened (a real socket was produced)…
        let accepts_after = crate::transport::tcp_counters_snapshot().accept_count;
        assert_eq!(
            accepts_after,
            accepts_before + 1,
            "the regression requires a real accept to have occurred (not WouldBlock)"
        );
        // …but the accepted handle was CLOSED on the deposit-failure path: the
        // specific handle must NOT remain in the streams table.  We check the
        // handle directly rather than comparing global table sizes, because
        // concurrent transport tests can add/remove their own handles between
        // the two measurements and cause false failures (the pre-fix leak would
        // leave the SPECIFIC accepted handle in the table, and this check still
        // catches it without being sensitive to the global count).
        let accepted_handle = LAST_ACCEPTED_CONN_FOR_TEST.get();
        assert_ne!(
            accepted_handle, -1,
            "LAST_ACCEPTED_CONN_FOR_TEST not set — handle_ready_accept did not record a real accept"
        );
        assert!(
            !crate::transport::tcp_streams_has_handle_for_test(accepted_handle),
            "accepted handle {accepted_handle} must be closed, not leaked into streams table"
        );

        // The cancelled slot took no deposit and fired no wake.
        assert_eq!(
            unsafe { crate::read_slot::hew_read_slot_status(slot) },
            crate::read_slot::ReadStatus::Cancelled as i32,
            "a cancelled accept slot must not receive a handle deposit"
        );

        // Creator ref free reclaims the slot (no leak).
        // SAFETY: creator ref held.
        unsafe { crate::read_slot::hew_read_slot_free(slot) };

        drop(client);
        // SAFETY: cleanup; the listener handle is released from TCP_API_STATE.
        unsafe {
            crate::transport::hew_tcp_close(listener);
            hew_io_poller_stop(poller);
        }
        reset_reactor();
    }

    // ---- P1-B: snapshot→publish actor-ref deref UAF guard ------------------
    //
    // `handle_ready_fd` snapshots `actor_ref`/`actor_local` and releases the
    // registry lock BEFORE publishing the `DELIVERING_ACTOR` guard. A concurrent
    // `reactor_detach_actor` that runs entirely inside that window observes the
    // guard as 0, does NOT spin-wait, and lets `hew_actor_free` reclaim the actor
    // box. The pre-fix liveness check called `hew_actor_ref_is_alive`, which for a
    // LOCAL ref does `(*actor).actor_state.load(..)` — a raw deref of the freed
    // box (use-after-free; a sanitizer trap, and logically may read reclaimed
    // memory and falsely report the actor alive). The fix routes the LOCAL case
    // through `with_live_actor`, which reports a freed (untracked) actor dead
    // WITHOUT dereferencing it.

    /// Direct invariant proof: a LOCAL actor freed (untracked + box reclaimed)
    /// between the snapshot and the liveness check is reported dead by
    /// `actor_snapshot_alive` without dereferencing the freed box.
    ///
    /// FAIL-BEFORE / PASS-AFTER: with the pre-fix raw `hew_actor_ref_is_alive`
    /// probe this dereferences the freed actor (`(*actor).actor_state`) — a
    /// use-after-free the sanitizer suite traps, and which may read reclaimed
    /// memory and return `true` (asserted-against below). With the fix the
    /// `with_live_actor` membership miss returns `false` with no deref.
    #[test]
    fn actor_snapshot_alive_reports_freed_local_actor_dead_without_deref() {
        let _guard = REACTOR_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_reactor();

        // A real, LIVE_ACTORS-tracked local actor (spawn → track_actor).
        let actor = spawn_full_reject_actor();
        // SAFETY: `actor` is a live, test-owned local actor.
        let actor_ref = unsafe { crate::transport::hew_actor_ref_local(actor) };
        let actor_local = actor_ref_local_ptr(&actor_ref).cast::<HewActor>();
        assert_eq!(
            actor_local, actor,
            "local ref must resolve to the actor ptr"
        );

        // While tracked + non-terminal it is reported alive.
        assert!(
            actor_snapshot_alive(actor_local, &actor_ref),
            "a live tracked local actor must be reported alive"
        );

        // The detacher frees the actor inside the snapshot→publish window:
        // untrack (removes from LIVE_ACTORS) then reclaim the box.
        free_parked_actor(actor);

        // The snapshot still holds the (now dangling) pointer, exactly as
        // `handle_ready_fd` would after the lock release. The liveness check MUST
        // report dead WITHOUT dereferencing it.
        assert!(
            !actor_snapshot_alive(actor_local, &actor_ref),
            "a freed (untracked) local actor must be reported dead — the pre-fix raw \
             hew_actor_ref_is_alive deref of the freed box was a use-after-free"
        );

        reset_reactor();
    }

    /// Full-path proof: drive `handle_ready_fd` for a registration whose LOCAL
    /// actor was freed in the snapshot→publish window. The abort path must
    /// unregister the fd and NOT deref the freed actor. Models the reactor
    /// reaching delivery for an fd whose owning actor a racing detach already
    /// reclaimed.
    ///
    /// FAIL-BEFORE / PASS-AFTER: pre-fix the unconditional `hew_actor_ref_is_alive`
    /// dereferences the freed box on the abort path (UAF). Post-fix the
    /// `with_live_actor` miss aborts cleanly.
    #[cfg(unix)]
    #[test]
    #[allow(
        clippy::undocumented_unsafe_blocks,
        reason = "test-only FFI: every pointer is a fresh local slot/poller/conn the \
                  test body sets up and tears down; the lifecycle is described inline"
    )]
    fn handle_ready_fd_aborts_without_deref_when_local_actor_freed() {
        let _guard = REACTOR_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_reactor();

        // SAFETY: no preconditions for new.
        let poller = unsafe { hew_io_poller_new() };
        assert!(!poller.is_null());
        let (rfd, wfd) = make_pipe();
        // SAFETY: poller valid; rfd is a live read fd.
        assert_eq!(
            unsafe { hew_io_poller_register(poller, rfd, std::ptr::null_mut(), 0, HEW_IO_READ) },
            0
        );

        // A real LOCAL actor + a resume-mode registration referencing it. The
        // slot carries the reactor ref the Registration's Drop will release.
        let actor = spawn_full_reject_actor();
        let actor_key = actor as usize;
        let actor_ref = unsafe { crate::transport::hew_actor_ref_local(actor) };
        let slot = crate::read_slot::hew_read_slot_new();
        // SAFETY: creator ref held → slot live for this retain.
        unsafe { crate::read_slot::read_slot_retain(slot) }; // reactor ref (owned by reg)
        inject_resume_registration_for_test(rfd, 911, actor_ref, actor_key, slot);
        assert_eq!(registration_count_for_test(), 1);

        // The detacher freed the actor in the snapshot→publish window (untrack +
        // reclaim). The registration still points at it; the snapshot the reactor
        // takes will carry the dangling pointer.
        free_parked_actor(actor);

        // Drive the readiness: the liveness check must report dead (no deref) and
        // abort — unregistering the fd without depositing.
        handle_ready_fd_for_test(poller, rfd, HEW_IO_READ);
        assert_eq!(
            registration_count_for_test(),
            0,
            "a readiness whose local actor was freed mid-window must abort + unregister"
        );
        // No deposit landed (the abort fired before any read/deposit).
        assert_eq!(
            unsafe { crate::read_slot::hew_read_slot_status(slot) },
            crate::read_slot::ReadStatus::Pending as i32,
            "the abort path must not deposit a read result"
        );
        // The Registration drop released the reactor ref; the creator ref free
        // reclaims the slot (no leak).
        // SAFETY: creator ref still held.
        unsafe { crate::read_slot::hew_read_slot_free(slot) };

        // SAFETY: closing our own fds and surrendering the poller.
        unsafe {
            libc::close(rfd);
            libc::close(wfd);
            hew_io_poller_stop(poller);
        }
        reset_reactor();
    }

    // ---- P1-A: read-slot UAF during in-flight deposit ----------------------
    //
    // `handle_ready_fd` snapshots `RegMode::Resume { read_slot }` by raw pointer.
    // The deposit in `handle_ready_resume` runs WITHOUT the registry lock. A
    // concurrent `reactor_detach_actor` is scrub-then-wait: Phase-1
    // `evict_actor_state` drops the `Registration` (and the registration-owned
    // slot ref) BEFORE the Phase-2 `DELIVERING_ACTOR` wait. Teardown destroys the
    // coroutine first (drops the creator ref), so the Phase-1 scrub can drop the
    // LAST ref and free the slot while the reactor is mid-deposit — the
    // `DELIVERING_ACTOR` guard protects the ACTOR, not the SLOT.
    //
    // The fix: `handle_ready_fd` takes its OWN in-flight ref on the slot under the
    // registry lock (in the snapshot closure), released by the `InflightSlotRef`
    // guard on every exit. This forced-ordering test drives the EXACT mid-deposit
    // ordering via the `RESUME_PRE_DEPOSIT_HOOK`: with the in-flight ref held, the
    // slot survives a creator-free + registration-scrub that together would
    // otherwise drop the last ref, so the deposit that follows is valid.

    /// Forced-ordering proof: at the mid-deposit point (in-flight ref held), run
    /// the teardown ordering that drops BOTH the creator ref and the
    /// registration-owned ref. The in-flight ref must keep the slot alive so the
    /// deposit lands on valid memory; the refcount across the scrub is asserted
    /// directly, and the deposit itself runs (the UAF site) so the sanitizer suite
    /// traps the pre-fix shape.
    ///
    /// FAIL-BEFORE / PASS-AFTER: without the in-flight ref the creator-free +
    /// scrub drop the slot's last ref, so the deposit dereferences a freed slot
    /// (`(*slot).cancelled` / `(*slot).status`) — a use-after-free the sanitizer
    /// suite traps.
    /// With the fix the in-flight ref holds refs >= 1 across the scrub and the
    /// deposit is valid; the guard releases the final ref on return.
    #[test]
    #[allow(
        clippy::undocumented_unsafe_blocks,
        reason = "test-only FFI: every pointer is a fresh local slot/poller/conn the \
                  test body sets up and tears down; the lifecycle is described inline"
    )]
    fn inflight_slot_ref_survives_scrub_during_deposit() {
        use std::io::Write;
        const KEY: usize = 0x00F1_1761;

        let _guard = REACTOR_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_reactor();

        // SAFETY: no preconditions for new.
        let poller = unsafe { hew_io_poller_new() };
        assert!(!poller.is_null());
        let (conn, mut client) = crate::transport::tcp_socketpair_conn_for_test();
        let fd = crate::transport::tcp_conn_raw_fd(conn).expect("conn fd");
        assert!(crate::transport::tcp_conn_set_nonblocking(conn, true));
        // SAFETY: poller valid; fd live.
        assert_eq!(
            unsafe { hew_io_poller_register(poller, fd, std::ptr::null_mut(), 0, HEW_IO_READ) },
            0
        );

        // A LIVE local actor so `handle_ready_fd` passes the liveness check and
        // reaches the deposit (this test exercises the SLOT UAF, not the actor).
        let actor = spawn_full_reject_actor();
        let actor_ref = unsafe { crate::transport::hew_actor_ref_local(actor) };

        // Slot lifecycle: creator ref (=1), then the registration ref (=2) the
        // `reactor_await_read` retain models, owned by the injected registration.
        let slot = crate::read_slot::hew_read_slot_new();
        // SAFETY: creator ref held.
        unsafe { crate::read_slot::read_slot_retain(slot) }; // registration ref
        assert_eq!(
            unsafe { crate::read_slot::read_slot_refs_for_test(slot) },
            2
        );
        inject_resume_registration_for_test(fd, conn, actor_ref, KEY, slot);

        // Make the fd readable so the deposit path runs.
        client.write_all(b"deposit-race").expect("client write");
        client.flush().ok();
        std::thread::sleep(Duration::from_millis(20));

        // Install the mid-deposit hook: at the moment the reactor is about to
        // deposit (in-flight ref held), run the teardown ordering that the
        // verdict describes — creator-free FIRST, then the Phase-1 registration
        // scrub. Together these drop the creator + registration refs; only the
        // in-flight ref keeps the slot alive for the deposit that follows.
        let slot_addr = slot as usize;
        let scrub_observed = std::sync::Arc::new(std::sync::atomic::AtomicUsize::new(0));
        let scrub_observed_h = std::sync::Arc::clone(&scrub_observed);
        set_resume_pre_deposit_hook(Some(Box::new(move || {
            let slot = slot_addr as *mut crate::read_slot::HewReadSlot;
            // With the fix the in-flight ref is held: refs == 3 here
            // (creator + registration + in-flight). Pre-fix it would be 2.
            let refs_at_hook = unsafe { crate::read_slot::read_slot_refs_for_test(slot) };
            // Teardown destroys the coroutine first: drop the creator ref.
            unsafe { crate::read_slot::hew_read_slot_free(slot) };
            // Phase-1 scrub: remove the registration → its Drop releases the
            // registration-owned ref. (We cannot call `reactor_detach_actor` here:
            // it would spin-wait on DELIVERING_ACTOR, which this same thread holds
            // inside `handle_ready_fd`. Removing the registry entry models exactly
            // the Phase-1 `evict_actor_state` drop of the `Registration`.)
            with_reactor_state_locked_for_test(|state| {
                state.registry.remove(&fd);
                state.conn_to_fd.retain(|_, mapped| *mapped != fd);
            });
            // After creator-free + scrub: pre-fix refs == 0 (slot FREED → the
            // deposit below is a UAF). Post-fix refs == 1 (the in-flight ref
            // survives → the deposit is valid).
            let refs_after_scrub = unsafe { crate::read_slot::read_slot_refs_for_test(slot) };
            scrub_observed_h.store((refs_at_hook << 8) | refs_after_scrub, Ordering::SeqCst);
        })));

        // Drive the readiness: snapshot (takes the in-flight ref) → hook (creator-
        // free + scrub) → deposit (the UAF site pre-fix) → guard releases the
        // in-flight ref on return.
        handle_ready_fd_for_test(poller, fd, HEW_IO_READ);

        // Clear the hook so no later test inherits it.
        set_resume_pre_deposit_hook(None);

        let observed = scrub_observed.load(Ordering::SeqCst);
        let refs_at_hook = observed >> 8;
        let refs_after_scrub = observed & 0xff;
        assert_eq!(
            refs_at_hook, 3,
            "at the deposit point the in-flight ref must be held (creator + \
             registration + in-flight = 3)"
        );
        assert_eq!(
            refs_after_scrub, 1,
            "after the creator-free + registration scrub the in-flight ref must be \
             the sole surviving ref (1), keeping the slot alive across the deposit"
        );

        // The slot was reclaimed exactly once when `handle_ready_fd` returned and
        // its `InflightSlotRef` guard dropped the final ref (no leak, no
        // double-free — proven leak-/double-free-clean under the sanitizer suite).

        drop(client);
        // SAFETY: cleanup.
        unsafe {
            crate::transport::tcp_close_raw_for_test(conn);
            hew_io_poller_stop(poller);
        }
        free_parked_actor(actor);
        reset_reactor();
    }

    // ---- attach-then-free-before-promotion UAF guard (pending scrub) -------
    //
    // An `attach` queues a `Pending::Add` and returns; the reactor promotes it
    // into the registry on its next drain. If the owning actor is freed in that
    // window, the synchronous teardown detach must evict the still-queued add —
    // otherwise the reactor later promotes a registration pointing at freed
    // memory and a readiness event delivers `on_close`/`on_data` to it. These
    // tests prove the pending scrub (phase 1) and the promotion-guard re-scrub
    // (phase 2) close that window.

    /// Phase-1 pending scrub: a `Pending::Add` owned by the freed actor must be
    /// removed by `reactor_detach_actor` so the reactor never promotes it.
    /// Before the fix the add survived the detach and `drain_pending` promoted a
    /// dangling registration; this test asserts it is gone from `pending` and
    /// never reaches the registry.
    #[cfg(unix)]
    #[test]
    fn detach_actor_scrubs_pending_add_before_promotion() {
        const KEY: usize = 0x00A7_7AC4;

        let _guard = REACTOR_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_reactor();

        // SAFETY: no preconditions for new.
        let poller = unsafe { hew_io_poller_new() };
        assert!(!poller.is_null());
        let (rfd, wfd) = make_pipe();

        // Attach is still queued (not yet promoted): the add sits in `pending`.
        enqueue_pending_add_for_test(rfd, 301, dead_actor_ref(), KEY);
        assert_eq!(pending_count_for_test(), 1, "add should be queued");
        assert_eq!(registration_count_for_test(), 0, "not yet promoted");

        // The actor is freed: the synchronous teardown detach must scrub the
        // still-queued add so a later drain cannot promote a dangling reg.
        reactor_detach_actor(KEY);
        assert_eq!(
            pending_count_for_test(),
            0,
            "detach must remove the freed actor's still-queued Pending::Add"
        );

        // Drain now: with the add scrubbed, nothing is promoted — no dangling
        // registration pointing at the freed actor.
        drain_pending(poller);
        assert_eq!(
            registration_count_for_test(),
            0,
            "no registration may be promoted for a freed actor (UAF guard)"
        );

        // SAFETY: closing our own fds and surrendering the poller.
        unsafe {
            libc::close(rfd);
            libc::close(wfd);
            hew_io_poller_stop(poller);
        }
        reset_reactor();
    }

    /// A `Pending::Add` for a DIFFERENT live actor must survive the detach: the
    /// scrub is keyed by actor identity, not a blanket pending clear.
    #[cfg(unix)]
    #[test]
    fn detach_actor_pending_scrub_spares_other_actors() {
        const FREED: usize = 0x00DE_AD01;
        const LIVE: usize = 0x0011_FE01;

        let _guard = REACTOR_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_reactor();

        let (rfd1, wfd1) = make_pipe();
        let (rfd2, wfd2) = make_pipe();
        enqueue_pending_add_for_test(rfd1, 401, dead_actor_ref(), FREED);
        enqueue_pending_add_for_test(rfd2, 402, dead_actor_ref(), LIVE);
        assert_eq!(pending_count_for_test(), 2);

        reactor_detach_actor(FREED);
        assert_eq!(
            pending_count_for_test(),
            1,
            "only the freed actor's queued add should be scrubbed"
        );

        // SAFETY: closing our own fds.
        unsafe {
            libc::close(rfd1);
            libc::close(wfd1);
            libc::close(rfd2);
            libc::close(wfd2);
        }
        reset_reactor();
    }

    /// Phase-2 promotion guard + re-scrub: model the hand-off window in which
    /// the reactor has removed an add from `pending` (so phase-1's pending scrub
    /// cannot see it) but has not yet inserted it into the registry. The detach
    /// must spin-wait on `PROMOTING_ACTOR`, and once the registration lands it
    /// must be re-scrubbed before the detach returns — never left dangling.
    #[cfg(unix)]
    #[test]
    fn detach_actor_waits_out_promotion_then_rescrubs_registry() {
        const KEY: usize = 0x0C0F_FEE5;

        let _guard = REACTOR_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_reactor();

        let (rfd, wfd) = make_pipe();

        // Model the reactor mid-promotion: guard published, add already removed
        // from `pending` (so phase-1's scrub misses it). The registration is not
        // yet in the registry.
        set_promoting_actor_for_test(KEY);
        assert_eq!(pending_count_for_test(), 0, "add already left pending");
        assert_eq!(registration_count_for_test(), 0, "not yet inserted");

        // Spawn the synchronous detach; it must block in phase 2 on the guard.
        let detach_returned = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
        let detach_returned_t = std::sync::Arc::clone(&detach_returned);
        let handle = std::thread::spawn(move || {
            reactor_detach_actor(KEY);
            detach_returned_t.store(true, Ordering::SeqCst);
        });

        // While the guard is held, detach must not return.
        let blocked_deadline = Instant::now() + Duration::from_millis(40);
        while Instant::now() < blocked_deadline {
            assert!(
                !detach_returned.load(Ordering::SeqCst),
                "reactor_detach_actor returned while PROMOTING_ACTOR == actor_key — \
                 the phase-2 promotion guard is not gating the free (UAF risk)"
            );
            std::thread::sleep(Duration::from_millis(2));
        }

        // The reactor "finishes" the promotion: insert the registration, then
        // clear the guard — exactly the order `apply_add` uses.
        inject_registration_for_test(rfd, 501, dead_actor_ref(), KEY);
        set_promoting_actor_for_test(0);

        // Detach must now return AND have re-scrubbed the just-promoted entry.
        let release_deadline = Instant::now() + Duration::from_secs(1);
        while !detach_returned.load(Ordering::SeqCst) {
            assert!(
                Instant::now() < release_deadline,
                "reactor_detach_actor did not return after the promotion guard cleared"
            );
            std::thread::sleep(Duration::from_millis(2));
        }
        handle.join().expect("detach thread should join cleanly");

        assert_eq!(
            registration_count_for_test(),
            0,
            "the freshly-promoted registration must be re-scrubbed by phase 2 (UAF guard)"
        );

        // SAFETY: closing our own fds.
        unsafe {
            libc::close(rfd);
            libc::close(wfd);
        }
        reset_reactor();
    }

    /// TC-1b regression: a delivery the reactor begins in the window between the
    /// phase-2 spin-wait exit and the phase-2 re-scrub must still be waited out.
    /// This is the residual UAF the single-wait-then-rescrub code left open: the
    /// reactor finishes a promotion (clears `PROMOTING_ACTOR`), the detach's
    /// spin-wait exits, and the reactor then enters delivery for the
    /// freshly-promoted fd in the *same poll cycle* (publishes `DELIVERING_ACTOR`)
    /// before the detach's re-scrub runs. With the old code the re-scrub removed
    /// the registration and `reactor_detach_actor` returned while
    /// `DELIVERING_ACTOR == key`, so the caller freed the actor box mid-send.
    ///
    /// Forced-ordering probe (deterministic, no reliance on timing luck): hold
    /// the `REACTOR_STATE` lock so the detach's re-scrub blocks; while it is
    /// blocked, clear `PROMOTING_ACTOR` (so the spin-wait has already exited) and
    /// publish `DELIVERING_ACTOR` + insert the registration (modelling the
    /// reactor entering delivery right after the spin-wait exit). Then assert the
    /// detach does NOT return while `DELIVERING_ACTOR == key`. Fails on the
    /// single-wait code (detach returns mid-delivery); passes once phase 2 loops
    /// scrub-then-wait until the key is fully drained.
    #[cfg(unix)]
    #[test]
    fn detach_actor_waits_out_delivery_begun_after_promotion_spinwait() {
        const KEY: usize = 0x00DE_117B;

        let _guard = REACTOR_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_reactor();

        let (rfd, wfd) = make_pipe();

        // Pin the detach in its phase-2 spin-wait via the promotion guard: model
        // the reactor mid-promotion, add already popped from `pending` (so
        // phase-1's pending scrub finds nothing) and not yet in the registry.
        set_promoting_actor_for_test(KEY);
        assert_eq!(pending_count_for_test(), 0, "add already left pending");
        assert_eq!(registration_count_for_test(), 0, "not yet inserted");

        let detach_returned = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
        let detach_returned_t = std::sync::Arc::clone(&detach_returned);
        let handle = std::thread::spawn(move || {
            // Phase 1 runs (takes + releases the lock), then phase 2's first
            // spin-wait spins on `PROMOTING_ACTOR == KEY` (lock-free).
            reactor_detach_actor(KEY);
            detach_returned_t.store(true, Ordering::SeqCst);
        });

        // Give the detach thread time to clear phase 1 and reach the lock-free
        // phase-2 spin-wait on the promotion guard.
        std::thread::sleep(Duration::from_millis(20));
        assert!(
            !detach_returned.load(Ordering::SeqCst),
            "detach must still be spin-waiting on the promotion guard"
        );

        // Force the bug window deterministically: hold the registry lock so the
        // phase-2 re-scrub (`evict_actor_state`) blocks. While holding it, clear
        // PROMOTING (the spin-wait now exits with DELIVERING still 0, so the
        // reactor is "past" the wait) and then publish DELIVERING + insert the
        // registration — modelling the reactor entering delivery in the same
        // poll cycle, after the wait exited but before the re-scrub completes.
        with_reactor_state_locked_for_test(|state| {
            // Spin-wait observes PROMOTING == 0 (and DELIVERING == 0) and exits;
            // the detach then reaches the re-scrub and blocks on this held lock.
            set_promoting_actor_for_test(0);
            // The reactor begins delivery for the freshly-promoted fd.
            set_delivering_actor_for_test(KEY);
            state.registry.insert(
                rfd,
                Registration {
                    conn: 601,
                    actor_ref: dead_actor_ref(),
                    actor_key: KEY,
                    mode: RegMode::AutoSend {
                        on_data_type: 1,
                        on_close_type: 2,
                    },
                    closed: false,
                },
            );
            // While we still hold the lock, the re-scrub cannot run and the
            // detach cannot return.
            assert!(
                !detach_returned.load(Ordering::SeqCst),
                "detach returned while the registry lock (and re-scrub) was held"
            );
        });

        // Lock released: the re-scrub now runs and removes the registration.
        // DELIVERING_ACTOR == KEY is still set (the modelled send is in flight),
        // so the detach MUST loop back and keep waiting — it must NOT return.
        // Pre-fix it returns here (the bug); post-fix it stays blocked.
        let blocked_deadline = Instant::now() + Duration::from_millis(60);
        while Instant::now() < blocked_deadline {
            assert!(
                !detach_returned.load(Ordering::SeqCst),
                "reactor_detach_actor returned while DELIVERING_ACTOR == actor_key after the \
                 re-scrub — a delivery begun between the spin-wait exit and the re-scrub was \
                 not waited out (residual promote-during-detach UAF)"
            );
            std::thread::sleep(Duration::from_millis(2));
        }

        // The modelled delivery completes: clear the guard. Detach must now drain
        // (scrub finds nothing, no guard set) and return.
        set_delivering_actor_for_test(0);
        let release_deadline = Instant::now() + Duration::from_secs(1);
        while !detach_returned.load(Ordering::SeqCst) {
            assert!(
                Instant::now() < release_deadline,
                "reactor_detach_actor did not return after DELIVERING_ACTOR cleared"
            );
            std::thread::sleep(Duration::from_millis(2));
        }
        handle.join().expect("detach thread should join cleanly");

        assert_eq!(
            registration_count_for_test(),
            0,
            "the registration delivered-then-detached must be scrubbed before return (UAF guard)"
        );

        // SAFETY: closing our own fds.
        unsafe {
            libc::close(rfd);
            libc::close(wfd);
        }
        reset_reactor();
    }

    // ---- 4d: Dekker detach-during-in-flight-delivery UAF guard -------------
    //
    // `reactor_detach_actor` runs on the actor-teardown path (synchronously from
    // `hew_actor_free`) and MUST NOT return while the reactor thread is
    // mid-delivery to the same actor (`DELIVERING_ACTOR == actor_key`). If it
    // returned early, `hew_actor_free` would tear the mailbox down while an
    // in-flight `hew_actor_try_send` is still writing to it — a use-after-free.
    // The Phase-2 spin-wait (reactor.rs Phase 2) is that guard. These tests
    // drive the guard deterministically via `set_delivering_actor_for_test`,
    // which stands in for the reactor's publish-before-send window.

    /// The synchronous detach must BLOCK in its Phase-2 spin-wait while a
    /// delivery to the actor is in flight, and must return only once the guard
    /// is cleared. Negative control: with the spin-wait removed, the spawned
    /// detach returns while the guard is still set and the "still blocked"
    /// assertion fails.
    #[cfg(unix)]
    #[test]
    fn detach_actor_blocks_while_delivery_in_flight() {
        const KEY: usize = 0xD1F_F00D;

        let _guard = REACTOR_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_reactor();

        // Inject a registration owned by KEY so Phase 1 has work and Phase 2 is
        // reached. A dead ref is fine: this test never delivers — it only
        // exercises the spin-wait against the published guard.
        let (rfd, wfd) = make_pipe();
        inject_registration_for_test(rfd, 401, dead_actor_ref(), KEY);

        // Publish the in-flight guard: the reactor is "mid-delivery" to KEY.
        set_delivering_actor_for_test(KEY);

        // Spawn the synchronous detach; it must spin-wait in Phase 2.
        let detach_returned = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
        let detach_returned_t = std::sync::Arc::clone(&detach_returned);
        let handle = std::thread::spawn(move || {
            reactor_detach_actor(KEY);
            detach_returned_t.store(true, Ordering::SeqCst);
        });

        // While the guard is held, detach must remain blocked. Poll a short
        // window; if it returns here, the spin-wait is not gating the free.
        let blocked_deadline = Instant::now() + Duration::from_millis(50);
        while Instant::now() < blocked_deadline {
            assert!(
                !detach_returned.load(Ordering::SeqCst),
                "reactor_detach_actor returned while DELIVERING_ACTOR == actor_key — \
                 the Phase-2 spin-wait is not guarding the in-flight delivery (UAF risk)"
            );
            std::thread::sleep(Duration::from_millis(2));
        }

        // Clear the guard: the in-flight delivery has finished. Detach must now
        // make progress and return promptly.
        set_delivering_actor_for_test(0);
        let release_deadline = Instant::now() + Duration::from_secs(1);
        while !detach_returned.load(Ordering::SeqCst) {
            assert!(
                Instant::now() < release_deadline,
                "reactor_detach_actor did not return within 1s after the guard cleared — \
                 the spin-wait failed to observe the cleared guard"
            );
            std::thread::sleep(Duration::from_millis(2));
        }
        handle.join().expect("detach thread should join cleanly");

        // SAFETY: closing our own fds.
        unsafe {
            libc::close(rfd);
            libc::close(wfd);
        }
        reset_reactor();
    }

    /// End-to-end UAF ordering proof: a free that follows the actor-teardown
    /// detach must happen-after the in-flight delivery's last touch of actor
    /// memory. A heap sentinel stands in for the actor's mailbox/state; the
    /// "reactor" thread reads it inside the published-guard window, the
    /// "teardown" thread detaches then frees it. Because the Phase-2 spin-wait
    /// blocks the detach until the guard clears, the free is strictly ordered
    /// after the last read. With the guard removed, the detach returns early and
    /// the free races (precedes) the read — the ordering assertion fails (and
    /// the sentinel read is a genuine use-after-free under a sanitizer). Run
    /// over many iterations so any residual interleaving surfaces.
    #[test]
    fn detach_then_free_never_races_in_flight_delivery() {
        const KEY: usize = 0xBADC_0FFEE;

        let _guard = REACTOR_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);

        for _ in 0..200 {
            reset_reactor();

            // Heap sentinel = stand-in for the actor's owned memory the
            // in-flight send touches. Shared as a raw address across threads;
            // exactly one of the two threads frees it.
            let sentinel: *mut u64 = Box::into_raw(Box::new(0xA5A5_A5A5_u64));
            let sentinel_addr = sentinel as usize;

            // Coordination: the reactor publishes the guard and signals it is
            // "in the delivery window" before touching the sentinel; the
            // teardown thread waits for that signal, then runs detach+free.
            let in_window = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
            let last_read_ns = std::sync::Arc::new(std::sync::atomic::AtomicU64::new(0));
            let freed_ns = std::sync::Arc::new(std::sync::atomic::AtomicU64::new(0));
            let epoch = Instant::now();

            let in_window_r = std::sync::Arc::clone(&in_window);
            let last_read_r = std::sync::Arc::clone(&last_read_ns);
            // Reactor thread: publish guard → signal → read sentinel for a
            // window → clear guard. Reading the sentinel models the in-flight
            // `hew_actor_try_send` writing into the mailbox.
            let reactor = std::thread::spawn(move || {
                set_delivering_actor_for_test(KEY);
                in_window_r.store(true, Ordering::SeqCst);
                let read_until = Instant::now() + Duration::from_millis(5);
                let p = sentinel_addr as *const u64;
                while Instant::now() < read_until {
                    // SAFETY: while DELIVERING_ACTOR == KEY, the teardown thread
                    // must not have freed the sentinel — that is the invariant
                    // under test. The volatile read prevents the loop being
                    // optimized away.
                    let v = unsafe { std::ptr::read_volatile(p) };
                    assert_eq!(v, 0xA5A5_A5A5_u64, "sentinel corrupted mid-delivery (UAF)");
                    #[expect(clippy::cast_possible_truncation, reason = "test timing fits u64")]
                    last_read_r.store(epoch.elapsed().as_nanos() as u64, Ordering::SeqCst);
                    std::hint::spin_loop();
                }
                // Delivery complete: release the in-flight guard.
                set_delivering_actor_for_test(0);
            });

            let in_window_t = std::sync::Arc::clone(&in_window);
            let freed_t = std::sync::Arc::clone(&freed_ns);
            // Teardown thread: wait until the reactor is in its delivery window,
            // then run the synchronous detach (which must spin-wait) and free.
            let teardown = std::thread::spawn(move || {
                while !in_window_t.load(Ordering::SeqCst) {
                    std::hint::spin_loop();
                }
                reactor_detach_actor(KEY);
                #[expect(clippy::cast_possible_truncation, reason = "test timing fits u64")]
                freed_t.store(epoch.elapsed().as_nanos() as u64, Ordering::SeqCst);
                // SAFETY: detach has returned, so no in-flight delivery is
                // touching the sentinel; we own it and free it exactly once.
                let _ = unsafe { Box::from_raw(sentinel_addr as *mut u64) };
            });

            reactor.join().expect("reactor thread joins");
            teardown.join().expect("teardown thread joins");

            // The free must happen-after the reactor's last read of the
            // sentinel. The spin-wait is the only thing enforcing this.
            let last_read = last_read_ns.load(Ordering::SeqCst);
            let freed = freed_ns.load(Ordering::SeqCst);
            assert!(
                freed >= last_read,
                "free ({freed} ns) preceded the in-flight delivery's last read \
                 ({last_read} ns) — detach did not wait out the in-flight delivery (UAF)"
            );
        }

        reset_reactor();
    }

    // ---- terminal close is non-droppable under mailbox backpressure --------
    //
    // The active-mode reactor's one-shot `on_close` MUST reach the actor even
    // when the mailbox is full under data backpressure. Before the fix
    // `deliver_close_once` used the non-blocking `hew_actor_try_send`, which
    // silently dropped the close on a full mailbox — the actor then never ran
    // teardown and the connection leaked forever. These tests force the bug
    // ordering (mailbox filled to capacity BEFORE close fires) against a real,
    // live actor and assert the close is delivered exactly once, FIFO-after the
    // buffered data.

    /// Spawn a live local actor with a capacity-1 `Fail`-policy mailbox so a
    /// second `try_send` is hard-rejected — the exact policy under which the old
    /// reactor dropped the close. Returns the actor pointer (free with
    /// `hew_actor_free`).
    fn spawn_full_reject_actor() -> *mut HewActor {
        // No-op dispatch: the scheduler is not running in this unit test, so the
        // actor stays Idle and we inspect its mailbox directly.
        unsafe extern "C-unwind" fn noop_dispatch(
            _ctx: *mut crate::execution_context::HewExecutionContext,
            _state: *mut c_void,
            _msg_type: i32,
            _data: *mut c_void,
            _size: usize,
            _borrow_mode: i32,
        ) -> *mut c_void {
            std::ptr::null_mut()
        }
        let opts = crate::actor::HewActorOpts {
            init_state: std::ptr::null_mut(),
            state_size: 0,
            dispatch: Some(noop_dispatch),
            mailbox_capacity: 1,
            overflow: crate::internal::types::HewOverflowPolicy::Fail as i32,
            coalesce_key_fn: None,
            coalesce_fallback: 0,
            budget: 0,
            arena_cap_bytes: 0,
            cycle_capable: 0,
        };
        // SAFETY: opts is a fully-initialised local; spawn copies what it needs.
        let actor = unsafe { crate::actor::hew_actor_spawn_opts(&raw const opts) };
        assert!(!actor.is_null(), "actor spawn must succeed");
        // Park the actor in Running so the wake-path CAS (Idle → Runnable) fails
        // and never calls `sched_enqueue` — there is no scheduler in this unit
        // test. The mailbox enqueue (the behaviour under test) is unaffected; we
        // inspect the mailbox queue directly rather than via dispatch.
        // SAFETY: spawn returned a non-null, live actor we own exclusively.
        unsafe {
            (*actor).actor_state.store(
                crate::internal::types::HewActorState::Running as i32,
                std::sync::atomic::Ordering::SeqCst,
            );
        }
        actor
    }

    /// Free an actor that was parked in `Running` by `spawn_full_reject_actor`:
    /// drop it back to a quiescent state first so `hew_actor_free`'s wait-for-
    /// quiescence loop does not time out.
    fn free_parked_actor(actor: *mut HewActor) {
        // SAFETY: `actor` is the live, test-owned actor from `spawn_full_reject_actor`;
        // we drop it to a quiescent state and free it exactly once.
        unsafe {
            (*actor).actor_state.store(
                crate::internal::types::HewActorState::Stopped as i32,
                std::sync::atomic::Ordering::SeqCst,
            );
            crate::actor::hew_actor_free(actor);
        }
    }

    /// Recv one message from a test-owned mailbox.
    /// SAFETY wrapper: the test is the sole consumer of `mb`.
    fn recv_one(mb: *mut crate::mailbox::HewMailbox) -> *mut crate::mailbox::HewMsgNode {
        // SAFETY: `mb` is the live mailbox of a test-owned actor; the test is the
        // single consumer (single-consumer invariant upheld).
        unsafe { crate::mailbox::hew_mailbox_try_recv(mb) }
    }

    /// Read a node's `msg_type` and free it.
    /// SAFETY wrapper: `node` came from `recv_one` and is owned here.
    fn drain_msg_type(node: *mut crate::mailbox::HewMsgNode) -> i32 {
        // SAFETY: `node` is a non-null node returned by `hew_mailbox_try_recv`,
        // owned here and freed exactly once.
        unsafe {
            let ty = (*node).msg_type;
            crate::mailbox::hew_msg_node_free(node);
            ty
        }
    }

    const ON_DATA_TYPE: i32 = 11;
    const ON_CLOSE_TYPE: i32 = 12;

    /// Fill the actor's capacity-1 mailbox with a buffered `on_data`, then prove
    /// a plain `try_send` of the close is rejected (the bug) while
    /// `deliver_close_once` (now using the guaranteed channel) still delivers it.
    #[test]
    fn close_delivered_to_full_mailbox_under_backpressure() {
        let _guard = REACTOR_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_reactor();

        // SAFETY: no preconditions for new.
        let poller = unsafe { hew_io_poller_new() };
        assert!(!poller.is_null());

        let actor = spawn_full_reject_actor();
        // SAFETY: `actor` is the live, test-owned actor; its mailbox pointer is
        // valid for the actor's lifetime.
        let mb = unsafe { (*actor).mailbox }.cast::<crate::mailbox::HewMailbox>();

        // FORCE THE ORDERING: fill the single mailbox slot with on_data BEFORE
        // the close fires. The mailbox is now at capacity.
        let datum: i32 = 0x5151;
        // SAFETY: `actor` is live; `datum` is a valid i32 readable for its size.
        let rc = unsafe {
            hew_actor_try_send(
                actor,
                ON_DATA_TYPE,
                (&raw const datum).cast_mut().cast(),
                std::mem::size_of::<i32>(),
            )
        };
        assert_eq!(rc, 0, "buffered on_data must enqueue into the empty slot");

        // Reproduce the bug: a plain try_send for the close is REJECTED now that
        // the mailbox is full — this is the silent drop the fix removes.
        // SAFETY: `actor` is live; the close carries no payload (null/0).
        let bug_rc = unsafe { hew_actor_try_send(actor, ON_CLOSE_TYPE, std::ptr::null_mut(), 0) };
        assert_ne!(
            bug_rc, 0,
            "try_send must drop the close on a full mailbox (the bug being fixed)"
        );

        // The fix: deliver_close_once admits the terminal event past capacity.
        deliver_close_once_for_test(poller, 9001, actor, ON_CLOSE_TYPE, false);

        // The actor observes the buffered on_data FIRST, then the close — exactly
        // once, FIFO-preserved.
        let first = recv_one(mb);
        assert!(!first.is_null(), "buffered on_data must still be present");
        assert_eq!(
            drain_msg_type(first),
            ON_DATA_TYPE,
            "buffered on_data drains before the close (per-connection FIFO)"
        );

        let second = recv_one(mb);
        assert!(
            !second.is_null(),
            "the terminal close MUST be delivered even though the mailbox was full \
             (non-droppable terminal event)"
        );
        assert_eq!(
            drain_msg_type(second),
            ON_CLOSE_TYPE,
            "the delivered terminal event must be on_close"
        );

        // Exactly-once: no third message.
        assert!(
            recv_one(mb).is_null(),
            "exactly one on_data + one on_close; no duplicate close"
        );

        free_parked_actor(actor);
        // SAFETY: the reactor never started in this test, so we own the poller;
        // surrender it.
        unsafe { hew_io_poller_stop(poller) };
        reset_reactor();
    }

    /// Exactly-once still holds under backpressure: a second close (e.g.
    /// readiness fires again before the unregister applies) with
    /// `already_closed == true` must NOT enqueue a duplicate, even though the
    /// guaranteed channel could physically admit one.
    #[test]
    fn close_is_exactly_once_even_when_guaranteed_under_backpressure() {
        let _guard = REACTOR_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        reset_reactor();

        // SAFETY: no preconditions for new.
        let poller = unsafe { hew_io_poller_new() };
        assert!(!poller.is_null());

        let actor = spawn_full_reject_actor();
        // SAFETY: `actor` is the live, test-owned actor; its mailbox pointer is
        // valid for the actor's lifetime.
        let mb = unsafe { (*actor).mailbox }.cast::<crate::mailbox::HewMailbox>();

        // Fill the slot, then deliver the first close (guaranteed, past capacity).
        let datum: i32 = 1;
        // SAFETY: `actor` is live; `datum` is a valid i32 readable for its size.
        let _ = unsafe {
            hew_actor_try_send(
                actor,
                ON_DATA_TYPE,
                (&raw const datum).cast_mut().cast(),
                std::mem::size_of::<i32>(),
            )
        };
        deliver_close_once_for_test(poller, 9002, actor, ON_CLOSE_TYPE, false);

        // A SECOND close attempt with already_closed = true must be suppressed by
        // the dedupe guard — no duplicate enqueue.
        deliver_close_once_for_test(poller, 9002, actor, ON_CLOSE_TYPE, true);

        // Drain: on_data, then exactly one on_close.
        let first = recv_one(mb);
        assert!(!first.is_null(), "buffered on_data must be present");
        assert_eq!(drain_msg_type(first), ON_DATA_TYPE);

        let second = recv_one(mb);
        assert!(!second.is_null(), "the one terminal close must be present");
        assert_eq!(drain_msg_type(second), ON_CLOSE_TYPE);

        assert!(
            recv_one(mb).is_null(),
            "the already_closed guard must suppress the duplicate close"
        );

        free_parked_actor(actor);
        // SAFETY: the reactor never started in this test, so we own the poller;
        // surrender it.
        unsafe { hew_io_poller_stop(poller) };
        reset_reactor();
    }
}
