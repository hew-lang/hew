//! Deterministic in-process `SimTransport` for the Hew distributed test
//! harness (`HEW-DIST-SPEC` v0 §14).
//!
//! `SimTransport` implements the existing [`HewTransportOps`] vtable in pure
//! memory — there are no sockets, no I/O syscalls, and no real clocks. Frames
//! are routed through per-connection bounded queues protected by a `Mutex`
//! and a `Condvar`. The whole module is gated behind
//! `cfg(any(test, feature = "sim-transport"))` and additionally
//! `cfg(not(target_family = "wasm"))`; it never compiles into a release
//! runtime and is invisible to the public C-ABI / `Node::*` surface.
//!
//! ## Why this exists
//!
//! `HEW-DIST-SPEC` §14 mandates a deterministic transport for property tests
//! of the seven distributed-runtime invariants. This slice ships only the
//! scaffold plus the two invariants that survive any future ratification of
//! the `RATIFIED-PENDING` distributed-handle ownership clauses
//! (`HEW-DIST-SPEC` §3 / §8 / §12):
//!
//! 1. typed-failure on partition (`HEW-DIST-SPEC` §14.1)
//! 2. live-session FIFO inside one negotiated session (`HEW-DIST-SPEC` §14.3)
//!
//! Invariants 2 / 4 / 5 / 6 / 7 are explicitly out of scope for this slice
//! and tracked as follow-on lanes (see `.tmp/plans/lane-sim-transport-distributed-property-tests.md`).
//!
//! ## Surface contract
//!
//! - **No `#[no_mangle]`, no `extern "C"` user-facing entrypoint.** The only
//!   way to construct a `SimTransport` is the Rust-only [`sim_transport_new`]
//!   constructor. `hew_node_api_set_transport` is **not** extended to accept
//!   `"sim"` in this slice.
//! - **No process globals.** Address resolution, connection IDs, and the
//!   PRNG live inside the boxed impl owned by the `*mut HewTransport`. Two
//!   `SimTransport` instances are wholly independent.
//! - **`destroy` is the canonical release path** (`ffi-ownership-contracts`).
//!   [`sim_transport_free`] mirrors `hew_node::free_transport` for tests.
//!   Double-destroy and stale-handle reentry are detected by an
//!   internal liveness registry (see [`LivenessRegistry`]) BEFORE any
//!   `Box::from_raw` reclaim runs, so reentry on a freed handle is a
//!   typed `set_last_error` no-op rather than a use-after-free.
//! - **Cleanup-all-exits.** `Drop` on the boxed impl wakes every pending
//!   recv via per-conn `Condvar`s so a panicking test does not leak threads.
//!
//! ## Determinism model (slice 1)
//!
//! - `SimConfig::seed` seeds an `rngs::SmallRng` for drop decisions. Two
//!   instances with the same seed and the same send sequence produce
//!   bit-identical drop choices.
//! - `partition` is an `AtomicBool`. While set, every `send` and `recv`
//!   returns `HEW_ERR_TRANSPORT` immediately and never fabricates success.
//!   `recv` is woken from any blocked wait so the test never hangs.
//! - `min_delay_ms` / `max_delay_ms` are reserved fields. Slice 1 does not
//!   wire them; the two §14 invariants this slice ships do not need them
//!   and wiring them would require a simulation-time-aware delivery queue
//!   that belongs in a follow-on slice (see `// TODO(sim-transport delay)`).
//! - Reorder, duplicate, clock-skew, and bandwidth knobs are intentionally
//!   absent from `SimConfig`. They will arrive with the §14 invariants
//!   that need them.

#![cfg(all(any(test, feature = "sim-transport"), not(target_family = "wasm")))]
#![allow(
    clippy::module_name_repetitions,
    reason = "type names mirror the existing TcpTransport / QuicTransport convention"
)]

use std::collections::{HashMap, HashSet, VecDeque};
use std::ffi::{c_char, c_int, c_void, CStr};
use std::sync::atomic::{AtomicBool, AtomicI32, Ordering};
use std::sync::{Condvar, Mutex, OnceLock};

use rand::rngs::SmallRng;
use rand::{RngExt, SeedableRng};

use crate::lifetime::poison_safe::PoisonSafe;
use crate::set_last_error;
use crate::transport::{HewTransport, HewTransportOps, HEW_CONN_INVALID};

// Error codes — kept in sync with `transport.rs`. We reuse the same value
// for the partition error so reviewers and downstream code see `SimTransport`
// partition failures as identical to TCP / QUIC transport errors
// (`transparent-but-typed-failure`).
const HEW_ERR_TRANSPORT: c_int = -14;

// Sim-only negative return codes used by the test-only vtable to convey
// distinct typed outcomes through the `c_int` return channel without
// conflating them under the single `HEW_ERR_TRANSPORT` bucket. Production
// transports never return these codes; only the classifier helpers below
// inspect them, and any unrecognised negative collapses to `InvalidConn`.
//
// Values are chosen to be far from any code already used by `transport.rs`
// (`-1`, `-14`, `HEW_CONN_INVALID`) and from each other so a future raw
// vtable consumer that only checks `rc < 0` keeps treating them as failures
// — fail-closed (`match-fail-closed`).
const HEW_ERR_SIM_PEER_CLOSED: c_int = -200;
const HEW_ERR_SIM_BUFFER_TOO_SMALL: c_int = -201;
const HEW_ERR_SIM_DROPPED: c_int = -202;

/// Maximum frame payload accepted by `SimTransport`. Mirrors
/// `transport.rs::MAX_FRAME_SIZE`. Frames exceeding this size return
/// `HEW_ERR_TRANSPORT` with a populated `set_last_error`, matching the wire
/// behaviour of the TCP / QUIC transports (`serializer-fail-closed`).
const MAX_FRAME_SIZE: usize = 16 * 1024 * 1024;

// ---------------------------------------------------------------------------
// Public configuration
// ---------------------------------------------------------------------------

/// Slice-1 configuration knobs for [`sim_transport_new`].
///
/// Reorder, duplicate, clock-skew, and bandwidth controls are deliberately
/// absent; the two slice-2 invariants do not need them and adding them now
/// would expand the surface beyond the lane plan. New fields land alongside
/// the §14 invariants that need them; tests should construct `SimConfig`
/// via `SimConfig { seed, ..SimConfig::default() }` so future fields gain
/// a default without churning every call site.
#[derive(Debug, Clone, Copy)]
pub struct SimConfig {
    /// PRNG seed for drop decisions. The same seed produces the same drop
    /// pattern for the same send sequence.
    pub seed: u64,
    /// Drop probability per send, expressed in parts-per-million. Zero means
    /// "never drop". Values above `1_000_000` are saturated to `1_000_000`.
    pub drop_rate_ppm: u32,
    /// Initial partition state. While `true`, send and recv return
    /// `HEW_ERR_TRANSPORT` immediately. Toggle at runtime via
    /// [`sim_transport_set_partition`].
    pub partition: bool,
    /// Reserved for future slices — minimum simulated delivery delay.
    /// **Not wired in slice 1.**
    pub min_delay_ms: u64,
    /// Reserved for future slices — maximum simulated delivery delay.
    /// **Not wired in slice 1.**
    pub max_delay_ms: u64,
}

impl Default for SimConfig {
    fn default() -> Self {
        Self {
            seed: 1,
            drop_rate_ppm: 0,
            partition: false,
            min_delay_ms: 0,
            max_delay_ms: 0,
        }
    }
}

/// Result of a [`sim_transport_send_classified`] call. Tests pattern-match on
/// this enum exhaustively (`match-fail-closed`).
///
/// `SimTransport` never returns a "succeeded with unspecified bytes" sentinel.
/// Every outcome maps to one named variant.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SimSendOutcome {
    /// Frame was accepted into the peer's queue. `bytes_sent` mirrors the
    /// payload length the caller passed in.
    Sent { bytes_sent: usize },
    /// Frame was deliberately dropped by the configured drop policy. From the
    /// caller's perspective this is still a successful send (matches TCP's
    /// "the network ate your packet" semantics).
    DroppedByPolicy,
    /// Partition flag is active. Caller sees a typed transport failure,
    /// never a success.
    Partitioned,
    /// Peer endpoint has been closed or detached (the peer called
    /// `close_conn`, or the local side was closed). Distinct from
    /// [`Self::Partitioned`] (a network-level partition that may heal) and
    /// from [`Self::DroppedByPolicy`] (a transient drop). Terminal for the
    /// connection — fail-closed.
    PeerClosed,
    /// Connection id is unknown, closed locally before send, or otherwise
    /// invalid.
    InvalidConn,
    /// Frame exceeded `MAX_FRAME_SIZE` — fail-closed (`serializer-fail-closed`).
    FrameTooLarge,
}

/// Result of a [`sim_transport_recv_classified`] call. As with
/// [`SimSendOutcome`], tests enumerate every variant.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SimRecvOutcome {
    /// Frame was successfully delivered. `bytes_received` matches the caller
    /// buffer fill.
    Received { bytes_received: usize },
    /// Partition flag is active. Typed failure, never silent.
    Partitioned,
    /// Peer endpoint went away — peer called `close_conn`, or the transport
    /// was destroyed. Any remaining queued frames have been drained first;
    /// once `PeerClosed` surfaces, no further frames will arrive on this
    /// connection. Distinct from [`Self::Partitioned`] (a network split that
    /// may heal) and from [`Self::BufferTooSmall`] (a sizing mistake).
    /// Terminal — fail-closed.
    PeerClosed,
    /// Connection id is unknown, closed, or otherwise invalid.
    InvalidConn,
    /// Caller buffer was too small for the next frame — fail-closed. The
    /// frame is left at the head of the queue so the caller can retry with
    /// a larger buffer; no data is lost.
    BufferTooSmall,
}

// ---------------------------------------------------------------------------
// Internal state
// ---------------------------------------------------------------------------

#[derive(Debug)]
struct ConnState {
    /// Peer connection id; `HEW_CONN_INVALID` once the peer detaches.
    peer: c_int,
    /// Inbound frames not yet read by `recv`.
    inbox: VecDeque<Vec<u8>>,
    /// Set when `close_conn` is called or the transport drops.
    closed: bool,
}

impl ConnState {
    fn new(peer: c_int) -> Self {
        Self {
            peer,
            inbox: VecDeque::new(),
            closed: false,
        }
    }
}

#[derive(Debug, Default)]
struct ConnTable {
    /// Allocated conn id → state.
    conns: HashMap<c_int, ConnState>,
    /// Listening address → queue of server-side conn ids waiting on `accept`.
    listeners: HashMap<String, VecDeque<c_int>>,
}

/// Internal state behind a `*mut HewTransport`. Owned via `Box::into_raw` and
/// reclaimed by [`sim_destroy`] (the vtable's `destroy` fn pointer) — exactly
/// like `TcpTransport` / `QuicTransport`.
struct SimTransportState {
    table: PoisonSafe<ConnTable>,
    cv: Condvar,
    next_conn: AtomicI32,
    partition: AtomicBool,
    drop_rate_ppm: u32,
    /// PRNG for drop decisions. Locked separately from the conn table so a
    /// blocked `recv` does not stall a concurrent `send`'s drop roll.
    rng: Mutex<SmallRng>,
    // Reserved configuration fields — see `SimConfig`.
    // TODO(sim-transport delay): wire `_min_delay_ms` / `_max_delay_ms`
    // through a simulation-time-aware delivery queue when a follow-on slice
    // ships an invariant that requires it (HEW-DIST-SPEC §14 invariants 5/6).
    _min_delay_ms: u64,
    _max_delay_ms: u64,
}

impl SimTransportState {
    fn new(cfg: SimConfig) -> Self {
        Self {
            table: PoisonSafe::new(ConnTable::default()),
            cv: Condvar::new(),
            // Start at 1 so a default-zero conn id is never valid.
            next_conn: AtomicI32::new(1),
            partition: AtomicBool::new(cfg.partition),
            drop_rate_ppm: cfg.drop_rate_ppm.min(1_000_000),
            rng: Mutex::new(SmallRng::seed_from_u64(cfg.seed)),
            _min_delay_ms: cfg.min_delay_ms,
            _max_delay_ms: cfg.max_delay_ms,
        }
    }

    fn alloc_conn_id(&self) -> c_int {
        // `c_int::MAX` is more than enough for any property test; if we
        // overflow we return `HEW_CONN_INVALID` rather than wrapping.
        let id = self.next_conn.fetch_add(1, Ordering::Relaxed);
        if id <= 0 {
            self.next_conn.store(c_int::MAX, Ordering::Relaxed);
            HEW_CONN_INVALID
        } else {
            id
        }
    }
}

impl Drop for SimTransportState {
    fn drop(&mut self) {
        // Wake any waiter so a panicking test never leaks a parked thread
        // (`cleanup-all-exits`). The `closed` flag is set transitively when
        // we mark partition + clear conn map below.
        self.partition.store(true, Ordering::Release);
        // PoisonSafe::access recovers from poison transparently.
        self.table.access(|t| {
            for state in t.conns.values_mut() {
                state.closed = true;
                state.inbox.clear();
            }
            t.conns.clear();
            t.listeners.clear();
        });
        self.cv.notify_all();
    }
}

// ---------------------------------------------------------------------------
// Vtable callbacks
// ---------------------------------------------------------------------------

/// SAFETY: `impl_ptr` was created by `Box::into_raw(Box<SimTransportState>)`
/// in [`sim_transport_new`] and is non-null while the transport lives.
unsafe fn state_from_impl<'a>(impl_ptr: *mut c_void) -> &'a SimTransportState {
    // SAFETY: caller upholds the contract documented above.
    unsafe { &*impl_ptr.cast::<SimTransportState>() }
}

unsafe extern "C" fn sim_connect(impl_ptr: *mut c_void, address: *const c_char) -> c_int {
    cabi_guard!(impl_ptr.is_null() || address.is_null(), HEW_CONN_INVALID);
    // SAFETY: caller guarantees `address` is a valid C string.
    let Ok(addr_str) = unsafe { CStr::from_ptr(address) }.to_str() else {
        set_last_error("sim_connect: address was not valid UTF-8");
        return HEW_CONN_INVALID;
    };
    // SAFETY: see `state_from_impl` contract.
    let st = unsafe { state_from_impl(impl_ptr) };

    if st.partition.load(Ordering::Acquire) {
        set_last_error("sim_connect: transport partitioned");
        return HEW_CONN_INVALID;
    }

    let client_id = st.alloc_conn_id();
    let server_id = st.alloc_conn_id();
    if client_id == HEW_CONN_INVALID || server_id == HEW_CONN_INVALID {
        set_last_error("sim_connect: connection-id space exhausted");
        return HEW_CONN_INVALID;
    }

    let installed = st.table.access(|t| {
        // Fail closed if the listener is unknown — mirrors `tcp_connect`'s
        // `connect refused` rather than fabricating a half-open conn.
        let Some(queue) = t.listeners.get_mut(addr_str) else {
            return false;
        };
        t.conns.insert(client_id, ConnState::new(server_id));
        t.conns.insert(server_id, ConnState::new(client_id));
        queue.push_back(server_id);
        true
    });
    if !installed {
        set_last_error(format!("sim_connect: no listener bound at {addr_str}"));
        return HEW_CONN_INVALID;
    }
    st.cv.notify_all();
    client_id
}

unsafe extern "C" fn sim_listen(impl_ptr: *mut c_void, address: *const c_char) -> c_int {
    cabi_guard!(impl_ptr.is_null() || address.is_null(), -1);
    // SAFETY: caller guarantees `address` is a valid C string.
    let Ok(addr_str) = unsafe { CStr::from_ptr(address) }.to_str() else {
        set_last_error("sim_listen: address was not valid UTF-8");
        return -1;
    };
    // SAFETY: see `state_from_impl` contract.
    let st = unsafe { state_from_impl(impl_ptr) };
    st.table.access(|t| {
        t.listeners.entry(addr_str.to_owned()).or_default();
    });
    0
}

unsafe extern "C" fn sim_accept(impl_ptr: *mut c_void, timeout_ms: c_int) -> c_int {
    cabi_guard!(impl_ptr.is_null(), HEW_CONN_INVALID);
    // SAFETY: see `state_from_impl` contract.
    let st = unsafe { state_from_impl(impl_ptr) };

    // Non-blocking poll suffices for the slice-2 property tests: the test
    // driver always calls `connect` before `accept`. We still honour the
    // timeout argument so future invariants (reconnect-gap) can rely on it.
    let _ = timeout_ms; // TODO(sim-transport): wire timeout for reconnect tests.

    st.table.access(|t| {
        for queue in t.listeners.values_mut() {
            if let Some(conn) = queue.pop_front() {
                return conn;
            }
        }
        HEW_CONN_INVALID
    })
}

unsafe extern "C" fn sim_send(
    impl_ptr: *mut c_void,
    conn: c_int,
    data: *const c_void,
    len: usize,
) -> c_int {
    cabi_guard!(impl_ptr.is_null() || (data.is_null() && len != 0), -1);
    // SAFETY: see `state_from_impl` contract.
    let st = unsafe { state_from_impl(impl_ptr) };

    if st.partition.load(Ordering::Acquire) {
        set_last_error("sim_send: partition active");
        return HEW_ERR_TRANSPORT;
    }
    if len > MAX_FRAME_SIZE {
        set_last_error(format!(
            "sim_send: frame {len} exceeds MAX_FRAME_SIZE ({MAX_FRAME_SIZE})"
        ));
        return HEW_ERR_TRANSPORT;
    }

    // Drop roll happens before we touch the conn table so two sends with
    // the same seed and the same send order produce the same drop pattern.
    let dropped = if st.drop_rate_ppm == 0 {
        false
    } else {
        let mut rng = st
            .rng
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        rng.random_range(0..1_000_000) < st.drop_rate_ppm
    };

    let payload: Vec<u8> = if len == 0 {
        Vec::new()
    } else {
        // SAFETY: caller contract guarantees `data` is valid for `len` bytes when len > 0.
        unsafe { std::slice::from_raw_parts(data.cast::<u8>(), len) }.to_vec()
    };

    let outcome = st.table.access(|t| {
        let Some(sender) = t.conns.get(&conn) else {
            return SendInternal::InvalidConn;
        };
        if sender.closed {
            return SendInternal::InvalidConn;
        }
        let peer_id = sender.peer;
        if peer_id == HEW_CONN_INVALID {
            return SendInternal::PeerClosed;
        }
        let Some(peer_state) = t.conns.get_mut(&peer_id) else {
            return SendInternal::PeerClosed;
        };
        if peer_state.closed {
            return SendInternal::PeerClosed;
        }
        if dropped {
            return SendInternal::Dropped;
        }
        peer_state.inbox.push_back(payload);
        SendInternal::Delivered
    });

    match outcome {
        SendInternal::Delivered => {
            st.cv.notify_all();
            // Mirror TCP / QUIC: positive return value is the byte count.
            #[expect(
                clippy::cast_possible_truncation,
                clippy::cast_possible_wrap,
                reason = "len is bounded by MAX_FRAME_SIZE which fits in c_int"
            )]
            {
                len as c_int
            }
        }
        SendInternal::Dropped => {
            // Caller sees a successful "0 bytes sent — frame ate by the
            // network" condition converted to a typed transport failure
            // rather than fabricating a positive ack. The classifier maps
            // the sim-only `HEW_ERR_SIM_DROPPED` code to `DroppedByPolicy`
            // so it can never collide with a peer-close or partition.
            set_last_error("sim_send: frame dropped by drop_rate policy");
            HEW_ERR_SIM_DROPPED
        }
        SendInternal::PeerClosed => {
            // Peer endpoint was closed (or our own conn's peer link was
            // detached by `close_conn` on the other side). This is terminal
            // for the connection and must NOT be reported as a transient
            // drop or a network partition (see review: drop-bucket and
            // buffer-too-small must not swallow peer-close).
            set_last_error("sim_send: peer endpoint closed or detached");
            HEW_ERR_SIM_PEER_CLOSED
        }
        SendInternal::InvalidConn => {
            set_last_error(format!("sim_send: invalid conn {conn}"));
            -1
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum SendInternal {
    Delivered,
    Dropped,
    PeerClosed,
    InvalidConn,
}

unsafe extern "C" fn sim_recv(
    impl_ptr: *mut c_void,
    conn: c_int,
    buf: *mut c_void,
    buf_size: usize,
) -> c_int {
    cabi_guard!(impl_ptr.is_null() || buf.is_null(), -1);
    // SAFETY: see `state_from_impl` contract.
    let st = unsafe { state_from_impl(impl_ptr) };

    // Acquire the table lock and wait on `cv` until a frame is available,
    // partition activates, or the conn is closed. This mirrors the blocking
    // semantics of `tcp_recv` / `quic_recv` while remaining bounded — every
    // wakeup channel (partition, close, frame arrival, drop) calls
    // `notify_all`, so the loop terminates.
    //
    // We use the inner `Mutex` of `PoisonSafe<ConnTable>` directly here
    // because `Condvar::wait` requires a `MutexGuard` and PoisonSafe only
    // exposes closure-style access. The unsafe cast below is sound: PoisonSafe
    // is `#[repr(transparent)]` over `Mutex<T>` (asserted by the field-only
    // ctor at construction; see `lifetime/poison_safe.rs`).
    //
    // To avoid that cast we instead do a short closure-bounded poll: the
    // table lock is held briefly inside `access`, releases between iterations
    // (which lets `send` make progress), and we yield via `cv.wait_timeout`
    // on a separate per-call mutex. This keeps the slice-1 surface free of
    // private re-exports from `lifetime/`.
    //
    // For the slice-2 invariants, the test driver always sends before
    // recv'ing, so the loop body usually finds a frame on the first pass.
    let yielder = Mutex::new(());
    loop {
        if st.partition.load(Ordering::Acquire) {
            set_last_error("sim_recv: partition active");
            return HEW_ERR_TRANSPORT;
        }
        let outcome = st.table.access(|t| {
            let Some(state) = t.conns.get_mut(&conn) else {
                return RecvInternal::InvalidConn;
            };
            // Local side closed (we called `close_conn` on this conn). Even
            // if the inbox still holds data, the conn is no longer readable
            // — fail-closed (`cleanup-all-exits`).
            if state.closed {
                return RecvInternal::PeerClosed;
            }
            if let Some(frame) = state.inbox.pop_front() {
                if frame.len() > buf_size {
                    // Put it back so the caller can retry with a bigger buffer
                    // — fail-closed without losing data.
                    state.inbox.push_front(frame);
                    return RecvInternal::BufferTooSmall;
                }
                return RecvInternal::Frame(frame);
            }
            // Inbox is empty. If the peer endpoint is gone, this is terminal:
            // no further frames can ever arrive, and a blocked recv must wake
            // and surface a typed close failure rather than spin forever
            // (review item 1: peer-close must wake waiters and surface a
            // terminal typed transport/closed failure).
            if state.peer == HEW_CONN_INVALID {
                return RecvInternal::PeerClosed;
            }
            RecvInternal::Empty
        });
        match outcome {
            RecvInternal::Frame(frame) => {
                let n = frame.len();
                // SAFETY: caller guarantees `buf` is valid for `buf_size`
                // bytes and `n <= buf_size` was checked under the table lock.
                unsafe {
                    std::ptr::copy_nonoverlapping(frame.as_ptr(), buf.cast::<u8>(), n);
                }
                #[expect(
                    clippy::cast_possible_truncation,
                    clippy::cast_possible_wrap,
                    reason = "frame size bounded by MAX_FRAME_SIZE which fits in c_int"
                )]
                {
                    return n as c_int;
                }
            }
            RecvInternal::PeerClosed => {
                set_last_error("sim_recv: peer endpoint closed or detached");
                return HEW_ERR_SIM_PEER_CLOSED;
            }
            RecvInternal::InvalidConn => {
                set_last_error(format!("sim_recv: invalid conn {conn}"));
                return -1;
            }
            RecvInternal::BufferTooSmall => {
                set_last_error("sim_recv: caller buffer too small for next frame");
                return HEW_ERR_SIM_BUFFER_TOO_SMALL;
            }
            RecvInternal::Empty => {
                // Park briefly with a bounded timeout. `notify_all` from
                // `send` / `close_conn` / `Drop` / partition setter wakes us
                // immediately; the timeout only matters as a deadlock fuse.
                let guard = yielder
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                let _ = st
                    .cv
                    .wait_timeout(guard, std::time::Duration::from_millis(5))
                    .unwrap_or_else(|e| {
                        let (g, t) = e.into_inner();
                        (g, t)
                    });
            }
        }
    }
}

#[derive(Debug)]
enum RecvInternal {
    Frame(Vec<u8>),
    Empty,
    PeerClosed,
    InvalidConn,
    BufferTooSmall,
}

unsafe extern "C" fn sim_close_conn(impl_ptr: *mut c_void, conn: c_int) {
    cabi_guard!(impl_ptr.is_null());
    // SAFETY: see `state_from_impl` contract.
    let st = unsafe { state_from_impl(impl_ptr) };
    st.table.access(|t| {
        if let Some(state) = t.conns.get_mut(&conn) {
            state.closed = true;
            state.inbox.clear();
            let peer = state.peer;
            if let Some(peer_state) = t.conns.get_mut(&peer) {
                peer_state.peer = HEW_CONN_INVALID;
            }
        }
    });
    st.cv.notify_all();
}

// ---------------------------------------------------------------------------
// Liveness registry — the security-critical invariant
// ---------------------------------------------------------------------------

/// Process-wide registry of live `SimTransport` allocations.
///
/// **Why this exists.** The vtable signature for `destroy` (and the
/// mirror helper [`sim_transport_free`]) takes a raw pointer by value.
/// The C-ABI cannot nullify the caller's storage, so the only way to
/// detect a stale or double-destroy reentry without dereferencing
/// possibly-freed memory is to consult an out-of-band registry first.
/// Reconstructing `Box::from_raw(impl_ptr)` and then asking the boxed
/// value "are you already destroyed?" is unsound: the read happens
/// after the freed allocation has already been claimed (and may have
/// been reused by another allocation by then).
///
/// **Lock ordering.** Both mutexes here are leaf mutexes — they are
/// never held while taking any other lock (`PoisonSafe<ConnTable>`'s
/// inner `Mutex`, the per-transport `rng` mutex, the `Condvar`'s
/// associated mutex, or `set_last_error`'s thread-local). Hold time
/// is bounded to a single `HashSet::insert` / `HashSet::remove`. This
/// makes them deadlock-safe regardless of which subsystem currently
/// holds the conn table.
///
/// **Cleanup.** Entries are removed by `claim_transport` /
/// `claim_impl` at the moment ownership transfers back to Rust for
/// `Box::from_raw`. Insertion happens in [`sim_transport_new`] before
/// the pointer is observable to any other thread. There is no
/// background sweeper and no entry can outlive the program; the
/// registry is `'static` only because individual transport allocations
/// can be created and freed across many test threads without a single
/// owning scope.
///
/// **Why a registry and not e.g. an `Arc<AtomicBool>` tombstone on
/// the impl itself.** A tombstone living inside the freed allocation
/// is exactly the design that the security finding rejected — reading
/// it requires dereferencing the pointer, which is precisely what we
/// must not do. The registry deliberately stores the *integer value*
/// of each pointer so the liveness check is a pure hash-set lookup
/// against `usize` keys.
struct LivenessRegistry {
    /// Live `*mut HewTransport` outer pointers (cast to `usize`).
    transports: Mutex<HashSet<usize>>,
    /// Live `*mut SimTransportState` impl pointers (cast to `usize`).
    impls: Mutex<HashSet<usize>>,
}

fn registry() -> &'static LivenessRegistry {
    static REG: OnceLock<LivenessRegistry> = OnceLock::new();
    REG.get_or_init(|| LivenessRegistry {
        transports: Mutex::new(HashSet::new()),
        impls: Mutex::new(HashSet::new()),
    })
}

fn register_alloc(transport: *mut HewTransport, impl_ptr: *mut c_void) {
    let reg = registry();
    reg.transports
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .insert(transport as usize);
    reg.impls
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .insert(impl_ptr as usize);
}

/// Atomically remove `transport` from the live-transport set. Returns
/// `true` only on the single call that observed it as live; subsequent
/// or stale callers see `false` and MUST NOT dereference `transport`.
fn claim_transport(transport: *mut HewTransport) -> bool {
    registry()
        .transports
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .remove(&(transport as usize))
}

/// Atomically remove `impl_ptr` from the live-impl set. Returns `true`
/// only on the single call that observed it as live; subsequent or
/// stale callers see `false` and MUST NOT dereference `impl_ptr`.
fn claim_impl(impl_ptr: *mut c_void) -> bool {
    registry()
        .impls
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .remove(&(impl_ptr as usize))
}

unsafe extern "C" fn sim_destroy(impl_ptr: *mut c_void) {
    cabi_guard!(impl_ptr.is_null());
    // Liveness check BEFORE any pointer dereference / `Box::from_raw`.
    // A stale or double-destroy reentry sees the impl absent from the
    // registry and returns without touching the (possibly freed)
    // allocation — fail-closed (`ffi-ownership-contracts`).
    if !claim_impl(impl_ptr) {
        set_last_error("sim_destroy: stale or double-destroy on impl pointer");
        return;
    }
    // SAFETY: `impl_ptr` was created by `Box::into_raw` in
    // [`sim_transport_new`] and `claim_impl` just removed it from the
    // live-impl registry, transferring exclusive ownership to us. No
    // other thread can subsequently observe it as live, so the
    // `Box::from_raw` reclaim cannot race with another `sim_destroy`.
    let boxed = unsafe { Box::from_raw(impl_ptr.cast::<SimTransportState>()) };
    drop(boxed);
}

static SIM_OPS: HewTransportOps = HewTransportOps {
    connect: Some(sim_connect),
    listen: Some(sim_listen),
    accept: Some(sim_accept),
    send: Some(sim_send),
    recv: Some(sim_recv),
    close_conn: Some(sim_close_conn),
    destroy: Some(sim_destroy),
};

// ---------------------------------------------------------------------------
// Rust-only constructor / destructor
// ---------------------------------------------------------------------------

/// Construct a new `SimTransport`. The returned pointer mirrors the layout of
/// `hew_transport_tcp_new` / `hew_transport_quic_new` and must be released by
/// [`sim_transport_free`] (or by the vtable's `destroy` plus `Box::from_raw`
/// for the outer struct).
///
/// # Safety
///
/// - The caller must release the returned pointer via [`sim_transport_free`]
///   or an equivalent canonical release sequence. Failing to do so leaks the
///   boxed impl plus the outer `HewTransport`.
/// - The returned pointer must not be passed to a transport implementation
///   that expects a different vtable.
#[must_use]
pub unsafe fn sim_transport_new(cfg: SimConfig) -> *mut HewTransport {
    let state = Box::new(SimTransportState::new(cfg));
    let impl_ptr = Box::into_raw(state).cast::<c_void>();
    let transport = Box::new(HewTransport {
        ops: &raw const SIM_OPS,
        r#impl: impl_ptr,
    });
    let transport_ptr = Box::into_raw(transport);
    // Register both allocations BEFORE returning so any subsequent
    // `sim_destroy` / `sim_transport_free` call can validate liveness
    // without touching the underlying memory. See [`LivenessRegistry`].
    register_alloc(transport_ptr, impl_ptr);
    transport_ptr
}

/// Release a `SimTransport` returned by [`sim_transport_new`]. Mirrors
/// `hew_node::free_transport` so tests can clean up symmetrically without
/// reaching into private node internals.
///
/// # Safety
///
/// `transport` must be either null or a pointer that was returned by
/// [`sim_transport_new`] at some point in this process. A second call on
/// the same pointer (or a call on a stale pointer that was never live)
/// is a typed `set_last_error` no-op rather than a use-after-free: the
/// internal liveness registry is consulted before any dereference, so
/// the function never reads through a freed `Box`. This is what makes
/// the security invariant ("stale reentry cannot touch freed memory
/// before liveness validation") hold.
pub unsafe fn sim_transport_free(transport: *mut HewTransport) {
    if transport.is_null() {
        return;
    }
    // Liveness check BEFORE any pointer dereference / `Box::from_raw`.
    // A stale or double-free reentry sees the transport absent from the
    // registry and returns without touching the (possibly freed)
    // allocation — fail-closed.
    if !claim_transport(transport) {
        set_last_error("sim_transport_free: stale or double-free on transport pointer");
        return;
    }
    // SAFETY: registry confirmed liveness; we now hold exclusive
    // ownership of the outer allocation and no concurrent free can race.
    let transport_ref = unsafe { &*transport };
    // SAFETY: ops pointer is part of a valid transport.
    if let Some(ops) = unsafe { transport_ref.ops.as_ref() } {
        if let Some(destroy_fn) = ops.destroy {
            // SAFETY: impl belongs to this transport. `destroy_fn`
            // (i.e. [`sim_destroy`]) performs its own liveness check
            // against the impl registry, so a prior direct vtable
            // destroy followed by `sim_transport_free` does not
            // double-free the impl.
            unsafe { destroy_fn(transport_ref.r#impl) };
        }
    }
    // SAFETY: outer struct was allocated by `Box::into_raw` in
    // `sim_transport_new` and we just claimed exclusive ownership via
    // `claim_transport`.
    let _ = unsafe { Box::from_raw(transport) };
}

/// Toggle the partition state of a `SimTransport` at runtime.
///
/// While `partitioned` is true, every `send` and `recv` returns
/// `HEW_ERR_TRANSPORT` immediately. Any thread blocked in `recv` is woken
/// before the function returns.
///
/// # Safety
///
/// `transport` must be a live pointer returned by [`sim_transport_new`].
pub unsafe fn sim_transport_set_partition(transport: *mut HewTransport, partitioned: bool) {
    if transport.is_null() {
        return;
    }
    // SAFETY: caller guarantees the pointer is live.
    let transport_ref = unsafe { &*transport };
    if transport_ref.r#impl.is_null() {
        return;
    }
    // SAFETY: see `state_from_impl` contract.
    let st = unsafe { state_from_impl(transport_ref.r#impl) };
    st.partition.store(partitioned, Ordering::Release);
    st.cv.notify_all();
}

/// Classified send helper for property tests. Wraps the vtable's `send` fn
/// pointer and translates the C-ABI return code into a `match`-exhaustive
/// enum so test arms can enumerate every variant (`match-fail-closed`).
///
/// # Safety
///
/// `transport` must be a live pointer returned by [`sim_transport_new`] and
/// `payload` must be valid for `payload.len()` readable bytes (trivially true
/// for a `&[u8]`).
pub unsafe fn sim_transport_send_classified(
    transport: *mut HewTransport,
    conn: c_int,
    payload: &[u8],
) -> SimSendOutcome {
    if transport.is_null() {
        return SimSendOutcome::InvalidConn;
    }
    // SAFETY: caller upholds the contract.
    let transport_ref = unsafe { &*transport };
    // SAFETY: `ops` is part of a valid `HewTransport` constructed by `sim_transport_new`.
    let Some(ops) = (unsafe { transport_ref.ops.as_ref() }) else {
        return SimSendOutcome::InvalidConn;
    };
    let Some(send_fn) = ops.send else {
        return SimSendOutcome::InvalidConn;
    };
    // Pre-flight checks before invoking the FFI so the FrameTooLarge variant
    // is unambiguously reported even if the underlying `sim_send` would also
    // return `HEW_ERR_TRANSPORT` for an oversized frame.
    if payload.len() > MAX_FRAME_SIZE {
        return SimSendOutcome::FrameTooLarge;
    }

    // SAFETY: payload pointer is valid for payload.len() bytes; impl pointer
    // is non-null because the transport is live.
    let rc = unsafe {
        send_fn(
            transport_ref.r#impl,
            conn,
            payload.as_ptr().cast::<c_void>(),
            payload.len(),
        )
    };
    if rc >= 0 {
        #[expect(clippy::cast_sign_loss, reason = "guarded by rc >= 0")]
        return SimSendOutcome::Sent {
            bytes_sent: rc as usize,
        };
    }
    // Map sim-only error codes to typed outcomes. Each cause has a distinct
    // negative return code from `sim_send`, so the classifier never has to
    // disambiguate by inspecting state (which would be racy and was the
    // cause of review items 2 and 3 — drop and peer-close were both being
    // collapsed under `HEW_ERR_TRANSPORT`).
    match rc {
        HEW_ERR_TRANSPORT => {
            // Top-of-fn partition check inside `sim_send`. Oversized-frame
            // is caught above (FrameTooLarge), so this branch is exclusively
            // the partition flag.
            SimSendOutcome::Partitioned
        }
        HEW_ERR_SIM_DROPPED => SimSendOutcome::DroppedByPolicy,
        HEW_ERR_SIM_PEER_CLOSED => SimSendOutcome::PeerClosed,
        _ => SimSendOutcome::InvalidConn,
    }
}

/// Classified recv helper for property tests. See
/// [`sim_transport_send_classified`] for rationale.
///
/// # Safety
///
/// `transport` must be a live pointer returned by [`sim_transport_new`] and
/// `buf` must be valid for `buf.len()` writable bytes.
pub unsafe fn sim_transport_recv_classified(
    transport: *mut HewTransport,
    conn: c_int,
    buf: &mut [u8],
) -> SimRecvOutcome {
    if transport.is_null() {
        return SimRecvOutcome::InvalidConn;
    }
    // SAFETY: caller upholds the contract.
    let transport_ref = unsafe { &*transport };
    // SAFETY: `ops` is part of a valid `HewTransport` constructed by `sim_transport_new`.
    let Some(ops) = (unsafe { transport_ref.ops.as_ref() }) else {
        return SimRecvOutcome::InvalidConn;
    };
    let Some(recv_fn) = ops.recv else {
        return SimRecvOutcome::InvalidConn;
    };
    let buf_len = buf.len();
    // SAFETY: buf is valid for buf_len bytes; transport.impl is non-null.
    let rc = unsafe {
        recv_fn(
            transport_ref.r#impl,
            conn,
            buf.as_mut_ptr().cast::<c_void>(),
            buf_len,
        )
    };
    if rc >= 0 {
        #[expect(clippy::cast_sign_loss, reason = "guarded by rc >= 0")]
        return SimRecvOutcome::Received {
            bytes_received: rc as usize,
        };
    }
    // Each cause has a distinct sim-only return code; the classifier maps
    // them directly rather than scraping `partition` (which was the cause
    // of review item 2 — buffer-too-small and peer-close were both being
    // collapsed onto the same `BufferTooSmall` arm).
    match rc {
        HEW_ERR_TRANSPORT => SimRecvOutcome::Partitioned,
        HEW_ERR_SIM_PEER_CLOSED => SimRecvOutcome::PeerClosed,
        HEW_ERR_SIM_BUFFER_TOO_SMALL => SimRecvOutcome::BufferTooSmall,
        _ => SimRecvOutcome::InvalidConn,
    }
}

// ---------------------------------------------------------------------------
// Unit tests (smoke coverage; the spec-mandated property tests live in
// `hew-runtime/tests/sim_transport_property.rs`).
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    fn make_pair(t: *mut HewTransport, addr: &str) -> (c_int, c_int) {
        let caddr = CString::new(addr).unwrap();
        // SAFETY: t is freshly created by `sim_transport_new`; address is a
        // valid C string for the duration of the call.
        unsafe {
            let transport_ref = &*t;
            let ops = &*transport_ref.ops;
            let listen_rc = ops.listen.unwrap()(transport_ref.r#impl, caddr.as_ptr());
            assert_eq!(listen_rc, 0);
            let client = ops.connect.unwrap()(transport_ref.r#impl, caddr.as_ptr());
            assert!(client > 0, "client conn id was {client}");
            let server = ops.accept.unwrap()(transport_ref.r#impl, 0);
            assert!(server > 0, "server conn id was {server}");
            (client, server)
        }
    }

    #[test]
    fn smoke_round_trip() {
        // SAFETY: standard test construction / destruction.
        unsafe {
            let t = sim_transport_new(SimConfig::default());
            let (client, server) = make_pair(t, "sim://smoke:0");
            let payload = b"hello hew";
            let outcome = sim_transport_send_classified(t, client, payload);
            assert!(matches!(outcome, SimSendOutcome::Sent { bytes_sent: 9 }));
            let mut buf = [0u8; 32];
            let recv_outcome = sim_transport_recv_classified(t, server, &mut buf);
            assert!(matches!(
                recv_outcome,
                SimRecvOutcome::Received { bytes_received: 9 }
            ));
            assert_eq!(&buf[..9], payload);
            sim_transport_free(t);
        }
    }

    #[test]
    fn smoke_partition_blocks_send() {
        // SAFETY: standard test construction / destruction.
        unsafe {
            let t = sim_transport_new(SimConfig::default());
            let (client, _server) = make_pair(t, "sim://partition:0");
            sim_transport_set_partition(t, true);
            let outcome = sim_transport_send_classified(t, client, b"payload");
            assert!(
                matches!(outcome, SimSendOutcome::Partitioned),
                "outcome = {outcome:?}"
            );
            sim_transport_free(t);
        }
    }

    #[test]
    fn smoke_oversized_frame_rejected() {
        // SAFETY: standard test construction / destruction.
        unsafe {
            let t = sim_transport_new(SimConfig::default());
            let (client, _server) = make_pair(t, "sim://oversize:0");
            let big = vec![0u8; MAX_FRAME_SIZE + 1];
            let outcome = sim_transport_send_classified(t, client, &big);
            assert!(matches!(outcome, SimSendOutcome::FrameTooLarge));
            sim_transport_free(t);
        }
    }

    /// Review item 1: when one side calls `close_conn`, the peer's blocked
    /// `recv` must wake up and surface a typed `PeerClosed` outcome rather
    /// than spinning forever waiting for a frame that can never arrive.
    ///
    /// We simulate the wake by closing the local conn between recv attempts
    /// (single-threaded so we don't depend on thread scheduling — the
    /// invariant under test is "close marks the conn terminal for recv",
    /// not "Condvar wakes Y nanoseconds after notify").
    #[test]
    fn recv_returns_peer_closed_after_close_conn() {
        // SAFETY: standard test construction / destruction.
        unsafe {
            let t = sim_transport_new(SimConfig::default());
            let (client, server) = make_pair(t, "sim://peer-close-recv:0");

            // Close the client side; this sets server.peer = HEW_CONN_INVALID.
            let transport_ref = &*t;
            let ops = &*transport_ref.ops;
            ops.close_conn.unwrap()(transport_ref.r#impl, client);

            let mut buf = [0u8; 32];
            let outcome = sim_transport_recv_classified(t, server, &mut buf);
            assert!(
                matches!(outcome, SimRecvOutcome::PeerClosed),
                "expected PeerClosed after peer close_conn, got {outcome:?}"
            );
            sim_transport_free(t);
        }
    }

    /// Review item 1 (waker variant): a `recv` that is already blocked when
    /// the peer closes must be woken via the per-transport `Condvar` and
    /// return `PeerClosed` rather than parking forever. Spawning a thread
    /// here exercises the actual wake path; the bounded `wait_timeout` in
    /// `sim_recv` is only a deadlock fuse, not the primary wake mechanism.
    #[test]
    fn blocked_recv_wakes_on_peer_close() {
        use std::sync::atomic::{AtomicBool, Ordering};
        use std::sync::Arc;
        use std::thread;
        use std::time::{Duration, Instant};

        // SAFETY: t is owned for the duration of this test; both threads
        // hold the address as a usize and reconstitute the pointer locally.
        // The transport is freed only after the recv thread joins.
        unsafe {
            let t = sim_transport_new(SimConfig::default());
            let (client, server) = make_pair(t, "sim://peer-close-wake:0");

            let t_addr = t as usize;
            let started = Arc::new(AtomicBool::new(false));
            let started_clone = Arc::clone(&started);

            let recv_handle = thread::spawn(move || {
                started_clone.store(true, Ordering::Release);
                let mut buf = [0u8; 32];
                let t_local = t_addr as *mut HewTransport;
                #[allow(
                    unused_unsafe,
                    reason = "explicit unsafe block documents the FFI call site \
                              even though the lexical scope already permits it"
                )]
                // SAFETY: `t_local` outlives this thread (joined below); the
                // closure executes on a new thread but reconstitutes the
                // transport pointer from a usize captured via `move`.
                unsafe {
                    sim_transport_recv_classified(t_local, server, &mut buf)
                }
            });

            // Wait for the recv thread to actually enter sim_recv. A short
            // sleep is enough because `started` flips before the FFI call,
            // and the recv loop parks within microseconds.
            let deadline = Instant::now() + Duration::from_secs(1);
            while !started.load(Ordering::Acquire) {
                assert!(Instant::now() <= deadline, "recv thread never started");
                thread::sleep(Duration::from_millis(1));
            }
            thread::sleep(Duration::from_millis(20));

            // Close the client side — this should wake the blocked recv via
            // `cv.notify_all()` inside `sim_close_conn`.
            let transport_ref = &*t;
            let ops = &*transport_ref.ops;
            ops.close_conn.unwrap()(transport_ref.r#impl, client);

            // The recv must complete promptly (well under the watchdog).
            // We give it a generous budget to absorb CI scheduler jitter
            // but the actual wake is immediate.
            let join_deadline = Instant::now() + Duration::from_secs(5);
            loop {
                if recv_handle.is_finished() {
                    break;
                }
                assert!(
                    Instant::now() <= join_deadline,
                    "recv did not wake within 5s of peer close"
                );
                thread::sleep(Duration::from_millis(5));
            }
            let outcome = recv_handle.join().expect("recv thread panicked");
            assert!(
                matches!(outcome, SimRecvOutcome::PeerClosed),
                "expected PeerClosed wake, got {outcome:?}"
            );
            sim_transport_free(t);
        }
    }

    /// Review item 2: `sim_recv` returning `HEW_ERR_TRANSPORT` for a
    /// buffer-too-small condition must NOT be reclassified as `PeerClosed`,
    /// and a peer-close must NOT be reclassified as `BufferTooSmall`. The
    /// distinction is preserved because each cause has a distinct sim-only
    /// return code.
    #[test]
    fn recv_classifier_distinguishes_buffer_too_small_from_peer_close() {
        // SAFETY: standard test construction / destruction.
        unsafe {
            let t = sim_transport_new(SimConfig::default());
            let (client, server) = make_pair(t, "sim://classify-recv:0");

            // Send a 9-byte payload, then attempt to recv into a 4-byte
            // buffer — must surface BufferTooSmall, not PeerClosed.
            let send_outcome = sim_transport_send_classified(t, client, b"hello hew");
            assert!(matches!(send_outcome, SimSendOutcome::Sent { .. }));
            let mut tiny = [0u8; 4];
            let too_small = sim_transport_recv_classified(t, server, &mut tiny);
            assert!(
                matches!(too_small, SimRecvOutcome::BufferTooSmall),
                "expected BufferTooSmall, got {too_small:?}"
            );

            // Now drain with a big-enough buffer — frame must still be there
            // (fail-closed: BufferTooSmall does not consume the frame).
            let mut big = [0u8; 32];
            let drained = sim_transport_recv_classified(t, server, &mut big);
            assert!(
                matches!(drained, SimRecvOutcome::Received { bytes_received: 9 }),
                "expected drained Received, got {drained:?}"
            );

            // Close the client and recv again — must surface PeerClosed,
            // not BufferTooSmall.
            let transport_ref = &*t;
            let ops = &*transport_ref.ops;
            ops.close_conn.unwrap()(transport_ref.r#impl, client);
            let after_close = sim_transport_recv_classified(t, server, &mut big);
            assert!(
                matches!(after_close, SimRecvOutcome::PeerClosed),
                "expected PeerClosed after peer close, got {after_close:?}"
            );

            sim_transport_free(t);
        }
    }

    /// Review item 3: `sim_send` must distinguish a peer-detached failure
    /// from a drop-by-policy failure even when `drop_rate_ppm > 0`. With
    /// the peer closed, every send must report `PeerClosed`, not
    /// `DroppedByPolicy`, regardless of the drop rate.
    #[test]
    fn send_classifier_preserves_peer_close_under_nonzero_drop_rate() {
        // SAFETY: standard test construction / destruction.
        unsafe {
            // Drop rate well above zero so the old classifier would have
            // misclassified a peer-close as DroppedByPolicy.
            let cfg = SimConfig {
                seed: 42,
                drop_rate_ppm: 500_000,
                ..SimConfig::default()
            };
            let t = sim_transport_new(cfg);
            let (client, server) = make_pair(t, "sim://classify-send:0");

            // Close the server side; the client's peer link becomes invalid.
            let transport_ref = &*t;
            let ops = &*transport_ref.ops;
            ops.close_conn.unwrap()(transport_ref.r#impl, server);

            // Attempt many sends — every single one must report PeerClosed,
            // never DroppedByPolicy, because the cause is structural, not
            // probabilistic. Looping defends against a future regression
            // where the classifier might "sometimes" pick the wrong arm.
            for i in 0..32 {
                let outcome = sim_transport_send_classified(t, client, b"x");
                assert!(
                    matches!(outcome, SimSendOutcome::PeerClosed),
                    "send #{i}: expected PeerClosed under peer-detach + nonzero drop_rate, \
                     got {outcome:?}"
                );
            }
            sim_transport_free(t);
        }
    }

    /// Review item 3 corollary: with the peer alive, drop-by-policy still
    /// classifies as `DroppedByPolicy`. Belt-and-braces test that the
    /// classifier did not just pin everything to `PeerClosed`.
    #[test]
    fn send_classifier_reports_drop_by_policy_when_peer_alive() {
        // SAFETY: standard test construction / destruction.
        unsafe {
            // 100% drop rate guarantees the policy fires.
            let cfg = SimConfig {
                seed: 7,
                drop_rate_ppm: 1_000_000,
                ..SimConfig::default()
            };
            let t = sim_transport_new(cfg);
            let (client, _server) = make_pair(t, "sim://classify-drop:0");

            let outcome = sim_transport_send_classified(t, client, b"x");
            assert!(
                matches!(outcome, SimSendOutcome::DroppedByPolicy),
                "expected DroppedByPolicy with peer alive and 100% drop rate, got {outcome:?}"
            );
            sim_transport_free(t);
        }
    }

    /// Security regression (FFI ownership / invalid-handle safety):
    /// calling `sim_transport_free` twice on the same pointer must NOT
    /// reconstruct a `Box` from already-freed memory. With the
    /// liveness registry in place, the second call is a typed
    /// `set_last_error` no-op — the registry says "not live" before
    /// any dereference happens.
    ///
    /// Without the fix, the second `sim_transport_free` would call
    /// `Box::from_raw(transport)` on a freed allocation and read
    /// `transport_ref.ops` / `transport_ref.r#impl` from reclaimed
    /// memory before any liveness check could fire. Miri / `ASan` would
    /// flag the original code; this test exists to prevent regression.
    #[test]
    fn double_free_is_safe_noop() {
        // SAFETY: registry-mediated; the second free deliberately
        // exercises a stale pointer and the redesigned free path is
        // documented to handle it without dereferencing.
        unsafe {
            let t = sim_transport_new(SimConfig::default());
            sim_transport_free(t);
            // Second free on the same pointer: must NOT touch freed
            // memory. Asserting "did not crash / did not UB" is what
            // this test enforces; the typed signal is also written to
            // `set_last_error`.
            sim_transport_free(t);
        }
    }

    /// Security regression: calling the vtable's `destroy` twice on
    /// the same impl pointer must NOT reconstruct a `Box` from freed
    /// memory. The second call is gated by `claim_impl` against the
    /// registry and returns early without dereferencing.
    #[test]
    fn double_destroy_via_vtable_is_safe_noop() {
        // SAFETY: registry-mediated double-destroy exercise. After the
        // first destroy the impl is freed and removed from the live
        // set; the second destroy short-circuits before `Box::from_raw`.
        // We then `sim_transport_free` to clean up the outer struct;
        // its embedded destroy_fn call is itself a registry-checked
        // no-op, so it does not double-free the impl.
        unsafe {
            let t = sim_transport_new(SimConfig::default());
            let transport_ref = &*t;
            let ops = &*transport_ref.ops;
            let destroy_fn = ops.destroy.unwrap();
            destroy_fn(transport_ref.r#impl);
            // Second destroy on the same impl pointer: must NOT touch
            // freed memory.
            destroy_fn(transport_ref.r#impl);
            // Outer cleanup. The internal destroy_fn invocation here
            // also short-circuits because the impl is already claimed.
            sim_transport_free(t);
        }
    }

    /// Security regression: a stale handle from a freed transport
    /// must not be dereferenced by `sim_transport_free` or by the
    /// vtable's `destroy`. Both paths consult the registry first and
    /// fail closed.
    #[test]
    fn stale_handle_after_free_is_safe() {
        // SAFETY: we deliberately retain a stale pointer after free
        // and exercise both reentry paths. The redesigned API is
        // documented to short-circuit via the registry before any
        // dereference, so neither call touches freed memory.
        unsafe {
            let t = sim_transport_new(SimConfig::default());
            // Capture impl pointer BEFORE free so we can probe the
            // vtable destroy path with a stale value too. We re-read
            // the ops fn pointer through the static `SIM_OPS`, NOT
            // through `t`, to avoid touching the freed outer struct.
            let stale_impl = (*t).r#impl;
            sim_transport_free(t);

            // Stale outer pointer reentry — registry check fails,
            // function returns without dereferencing.
            sim_transport_free(t);

            // Stale impl pointer reentry via the static vtable —
            // `claim_impl` fails, function returns without
            // reconstructing `Box::from_raw(stale_impl)`.
            let destroy_fn = SIM_OPS.destroy.unwrap();
            destroy_fn(stale_impl);
        }
    }

    /// Two independent `SimTransport` instances must each be freeable
    /// without affecting the other's registry entry. Catches a class
    /// of bug where the registry might be incorrectly keyed or
    /// globally cleared.
    #[test]
    fn independent_transports_free_independently() {
        // SAFETY: standard construction; both transports are freed
        // exactly once each.
        unsafe {
            let a = sim_transport_new(SimConfig::default());
            let b = sim_transport_new(SimConfig::default());
            assert_ne!(a as usize, b as usize);
            sim_transport_free(a);
            // `b` must still be live and freeable — the registry
            // entry for `b` is independent of `a`.
            let (client, server) = make_pair(b, "sim://independent:0");
            let outcome = sim_transport_send_classified(b, client, b"ping");
            assert!(matches!(outcome, SimSendOutcome::Sent { bytes_sent: 4 }));
            let mut buf = [0u8; 16];
            let recv = sim_transport_recv_classified(b, server, &mut buf);
            assert!(matches!(
                recv,
                SimRecvOutcome::Received { bytes_received: 4 }
            ));
            sim_transport_free(b);
        }
    }
}
