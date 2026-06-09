// native-only: the active-mode reactor uses epoll/kqueue + OS threads, neither
// available on WASM. The WASM build fails closed via the type checker's
// `WasmUnsupportedFeature::TcpNetworking` gate before any reactor call.
//! Active-mode network I/O reactor — "I/O completion as a mailbox message".
//!
//! A single non-scheduler background thread (the *reactor*) owns a platform
//! readiness poller ([`crate::io_time::HewIoPoller`], epoll on Linux / kqueue
//! on macOS+FreeBSD) and a registry mapping each registered connection fd to
//! the actor that should receive its data. When a registered socket becomes
//! readable the reactor reads the available bytes and delivers them to the
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

use crate::actor::{hew_actor_try_send, HewActor};
use crate::bytes::{hew_bytes_from_static, BytesTriple};
use crate::io_time::{
    hew_io_poller_new, hew_io_poller_poll_ready, hew_io_poller_register, hew_io_poller_stop,
    hew_io_poller_unregister, HewIoPoller, HEW_IO_ERROR, HEW_IO_HUP, HEW_IO_READ,
};
use crate::lifetime::poison_safe::PoisonSafe;
use crate::transport::{
    actor_ref_local_ptr, hew_actor_ref_is_alive, tcp_conn_raw_fd, tcp_conn_read_available,
    tcp_conn_set_nonblocking, ActiveReadOutcome, HewActorRef,
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
    /// `msg_type` index for `on_data(bytes)` delivery.
    on_data_type: i32,
    /// `msg_type` index for the one-shot `on_close()` delivery.
    on_close_type: i32,
    /// Set once an `on_close` has been delivered for this conn, so EOF/error
    /// is reported exactly once even if readiness fires again before removal.
    closed: bool,
}

// SAFETY: the `HewActorRef` snapshot inside `Registration` is `#[repr(C)]`
// plain data — a `kind` tag plus a union of a raw `*mut HewActor` (local) or
// an id+conn+`*mut HewTransport` triple (remote). Moving it across threads
// only copies those scalar/pointer bytes; nothing is dereferenced during the
// move. The snapshot is dereferenced solely on the reactor thread, after the
// registry lock has handed the `Registration` over, so the cross-thread
// transfer never races a dereference. `Registration`'s own `Send` impl below
// is what authorizes that move; the carried pointers' validity is upheld by
// the liveness protocol (the Dekker `DELIVERING_ACTOR` guard plus the
// synchronous `reactor_detach_actor` on `hew_actor_free`), not by this impl.
unsafe impl Send for Registration {}

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
    REACTOR_STATE.access(|state| {
        state.registry.remove(&fd);
        state.conn_to_fd.retain(|_, mapped| *mapped != fd);
    });
}

/// A snapshot of the fields `handle_ready_fd` needs, taken under the registry
/// lock and used after the lock is released.
struct ReadySnapshot {
    conn: c_int,
    actor_ref: HewActorRef,
    actor_local: *mut HewActor,
    on_data_type: i32,
    on_close_type: i32,
    already_closed: bool,
}

/// Handle one ready fd: liveness-check the owning actor, read available bytes,
/// deliver `on_data` / `on_close`. Never holds the registry lock across the
/// read or the send.
fn handle_ready_fd(poller: *mut HewIoPoller, fd: c_int, events: c_int) {
    // Snapshot what we need under the lock, then release it.
    let snapshot = REACTOR_STATE.access(|state| {
        state.registry.get(&fd).map(|reg| ReadySnapshot {
            conn: reg.conn,
            // SAFETY: HewActorRef is Copy-able plain data; we duplicate the
            // snapshot so liveness can be checked after the lock is released.
            actor_ref: unsafe { std::ptr::read(std::ptr::addr_of!(reg.actor_ref)) },
            actor_local: actor_ref_local_ptr(&reg.actor_ref).cast::<HewActor>(),
            on_data_type: reg.on_data_type,
            on_close_type: reg.on_close_type,
            already_closed: reg.closed,
        })
    });
    let Some(snap) = snapshot else {
        // Registration vanished between poll and lookup (raced a remove).
        unregister_fd(poller, fd);
        return;
    };

    let actor_key = snap.actor_local as usize;

    // Publish the in-flight target BEFORE re-validating + sending (Dekker
    // protocol with `reactor_detach_actor`). Ordering with the registry-lock
    // revalidation below guarantees that a concurrent `hew_actor_free` either
    // (a) removes the registration before we re-read it under the lock — so we
    // see it gone and abort — or (b) observes `DELIVERING_ACTOR == actor_key`
    // and spin-waits until we clear it. Either way no send reaches a freed
    // actor.
    DELIVERING_ACTOR.store(actor_key, Ordering::SeqCst);

    // Re-validate under the lock AFTER publishing the guard: if the actor was
    // detached (freed) between the snapshot and now, the registration is gone
    // and we must not deliver. This pairs with the synchronous registry removal
    // in `reactor_detach_actor`.
    let still_registered = REACTOR_STATE.access(|state| state.registry.contains_key(&fd));
    // Liveness on the by-value ref snapshot is a second guard (the actor may be
    // Stopping but not yet freed). SAFETY: the snapshot owns a valid ref.
    let alive = unsafe { hew_actor_ref_is_alive(std::ptr::addr_of!(snap.actor_ref)) != 0 };
    if !still_registered || !alive {
        DELIVERING_ACTOR.store(0, Ordering::SeqCst);
        unregister_fd(poller, fd);
        return;
    }

    let hard_close = events & (HEW_IO_HUP | HEW_IO_ERROR) != 0;

    // Read whatever is available (drains the kernel buffer). Even on a HUP/ERR
    // event there may be buffered bytes to deliver before the close.
    let outcome = if events & HEW_IO_READ != 0 || hard_close {
        tcp_conn_read_available(snap.conn)
    } else {
        ActiveReadOutcome::WouldBlock
    };

    match outcome {
        ActiveReadOutcome::Data(data) => {
            deliver_data(snap.actor_local, snap.on_data_type, &data);
            if hard_close {
                deliver_close_once(
                    poller,
                    fd,
                    snap.actor_local,
                    snap.on_close_type,
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
                    snap.on_close_type,
                    snap.already_closed,
                );
            }
        }
        ActiveReadOutcome::Eof | ActiveReadOutcome::Closed => {
            deliver_close_once(
                poller,
                fd,
                snap.actor_local,
                snap.on_close_type,
                snap.already_closed,
            );
        }
    }

    // Delivery (if any) is complete; release the in-flight guard so a waiting
    // `reactor_detach_actor` may proceed with the free.
    DELIVERING_ACTOR.store(0, Ordering::SeqCst);
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
        // SAFETY: actor_local is a live local actor pointer (liveness checked
        // by the caller); on_close carries no payload.
        unsafe {
            hew_actor_try_send(actor_local, on_close_type, std::ptr::null_mut(), 0);
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

/// Deliver `on_close` for a registration that never made it into the registry
/// (poller register failed). No fd to unregister.
fn deliver_orphan_close(reg: &Registration) {
    let actor_local = actor_ref_local_ptr(&reg.actor_ref).cast::<HewActor>();
    if actor_local.is_null() {
        return;
    }
    // SAFETY: snapshot deref; the actor was alive at attach time. A try_send to
    // a since-stopped actor returns an error and is harmless.
    unsafe {
        hew_actor_try_send(actor_local, reg.on_close_type, std::ptr::null_mut(), 0);
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
        on_data_type,
        on_close_type,
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
                on_data_type: 1,
                on_close_type: 2,
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
                on_data_type: 1,
                on_close_type: 2,
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
}

#[cfg(test)]
fn should_fail_reactor_spawn() -> bool {
    FAIL_REACTOR_SPAWN.with(std::cell::Cell::get)
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

    fn make_pipe() -> (c_int, c_int) {
        let mut fds = [0i32; 2];
        // SAFETY: fds is a valid 2-element array.
        assert_eq!(unsafe { libc::pipe(fds.as_mut_ptr()) }, 0);
        (fds[0], fds[1])
    }

    // 4b oracle (unit form): a readiness event for an actor that has stopped
    // (dead actor-ref) is DROPPED — no delivery — and the fd is unregistered,
    // so a post-stop readiness event never reaches a freed actor.
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
                    on_data_type: 1,
                    on_close_type: 2,
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
}
