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
//! and remain tracked as follow-up distributed-runtime property tests.
//!
//! ## Surface contract
//!
//! - **No `#[no_mangle]`, no `extern "C"` user-facing entrypoint.** The only
//!   way to construct a `SimTransport` is the Rust-only [`sim_transport_new`]
//!   constructor. `hew_node_api_set_transport` is **not** extended to accept
//!   `"sim"` in this slice.
//! - **No process globals for transport state.** Address resolution,
//!   connection IDs, and the PRNG live inside the heavy
//!   [`SimTransportState`] held behind an `Arc` inside a per-instance
//!   leaked handle shell. Two `SimTransport` instances are wholly
//!   independent.
//! - **[`sim_transport_free`] is the only canonical release path**
//!   (`ffi-ownership-contracts`). The vtable's `destroy` op is a
//!   sub-step that tombstones the inner heavy state; it does **not**
//!   permit the historical "vtable destroy + `Box::from_raw` outer"
//!   pattern (the outer is not a `Box` you can reclaim — see below).
//! - **Stale-handle / address-reuse safety by design.** Both the outer
//!   `*mut HewTransport` and the inner impl pointer are addresses of
//!   permanently-leaked fixed-size handle shells (see
//!   [`TransportShell`] / [`ImplShell`]). Because we never return those
//!   addresses to the allocator, no later `sim_transport_new` (or any
//!   other allocation in the process) can produce a pointer that
//!   aliases a stale one. A stale reentry therefore reads a
//!   tombstoned (state-`None`) shell and fails closed via
//!   `set_last_error` rather than authenticating a different live
//!   allocation. The bounded leak (a few dozen bytes per construction)
//!   is acceptable because this entire module is dev/test-only and
//!   never compiled into release/WASM. Heavy resources — bounded
//!   queues, listener tables, RNG — still release on `destroy` /
//!   `sim_transport_free` because they live behind an `Arc` inside the
//!   shell, not inside the leaked shell itself.
//! - **Cleanup-all-exits.** `destroy` (and `Drop` on
//!   `SimTransportState`) clears partition / closes every conn and
//!   calls `cv.notify_all()`, so any blocked `recv` wakes and exits
//!   before its `Arc` clone is dropped — no parked thread leak.
//!
//! ## Determinism model
//!
//! - `SimConfig::seed` seeds domain-separated `rngs::SmallRng` streams for
//!   drop and delivery-delay decisions. Two instances with the same seed,
//!   config, and send sequence produce bit-identical drop choices and delivery
//!   schedules.
//! - `partition` is guarded by the transport's coordinated `Mutex<SimCore>`.
//!   While set, every `send` and `recv` returns `HEW_ERR_TRANSPORT`
//!   immediately and never fabricates success. Any blocked `recv` is woken
//!   immediately so tests never hang.
//! - `min_delay_ms` / `max_delay_ms` schedule accepted frames onto a
//!   per-instance virtual clock (`clock_ms`, starting at `0` and advanced via
//!   [`sim_transport_advance_time_ms`]). Frames are held in a per-connection
//!   min-heap ordered by `(deliver_at_ms, send_seq)` so zero or constant
//!   delays preserve strict FIFO while variable delays produce deterministic
//!   reordering.
//! - `accept(timeout_ms)` honours `timeout_ms` against the same per-instance
//!   virtual clock (`0` polls without blocking, `> 0` waits until a peer
//!   connects or virtual time reaches `start_ms + timeout_ms`, `< 0` waits
//!   until a peer connects or the transport shuts down).
//! - Duplicate, clock-skew, and bandwidth knobs are intentionally absent from
//!   `SimConfig`. They will arrive with the §14 invariants that need them.

#![cfg(all(any(test, feature = "sim-transport"), not(target_family = "wasm")))]
#![allow(
    clippy::module_name_repetitions,
    reason = "type names mirror the existing TcpTransport / QuicTransport convention"
)]

use std::cmp::Ordering as CmpOrdering;
use std::collections::{BinaryHeap, HashMap, HashSet, VecDeque};
use std::ffi::{c_char, c_int, c_void, CStr};
use std::sync::atomic::{AtomicBool, AtomicI32, Ordering};
use std::sync::{Arc, Condvar, Mutex};

use rand::rngs::SmallRng;
use rand::{RngExt, SeedableRng};

use crate::set_last_error;
use crate::transport::{HewTransport, HewTransportOps, HEW_CONN_INVALID};

// Partition failures retain the simulated transport's negative error code.
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

/// Simulated frames follow the production transport's payload bound.
const MAX_FRAME_SIZE: usize = crate::transport::MAX_FRAME_SIZE;

// ---------------------------------------------------------------------------
// Public configuration
// ---------------------------------------------------------------------------

/// Configuration knobs for [`sim_transport_new`].
///
/// Duplicate, clock-skew, and bandwidth controls are deliberately absent; new
/// fields land alongside the §14 invariants that need them. Tests should
/// construct `SimConfig` via `SimConfig { seed, ..SimConfig::default() }` so
/// future fields gain a default without churning every call site.
#[derive(Debug, Clone, Copy)]
pub struct SimConfig {
    /// PRNG seed for drop and delivery-delay decisions. The same seed produces
    /// the same drop and delay schedule for the same send sequence.
    pub seed: u64,
    /// Drop probability per send, expressed in parts-per-million. Zero means
    /// "never drop". Values above `1_000_000` are saturated to `1_000_000`.
    pub drop_rate_ppm: u32,
    /// Initial partition state. While `true`, send and recv return
    /// `HEW_ERR_TRANSPORT` immediately. Toggle at runtime via
    /// [`sim_transport_set_partition`].
    pub partition: bool,
    /// Minimum simulated delivery delay in virtual milliseconds. Zero means
    /// frames can be delivered at the current virtual tick without advancing
    /// time.
    pub min_delay_ms: u64,
    /// Maximum simulated delivery delay in virtual milliseconds. Clamped to at
    /// least `min_delay_ms`. When `max_delay_ms > min_delay_ms`, each accepted
    /// frame draws a deterministic delay in `min_delay_ms..=max_delay_ms` from
    /// the seeded PRNG, allowing later frames with shorter delays to overtake
    /// earlier ones.
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

#[derive(Debug, Eq, PartialEq)]
struct ScheduledFrame {
    deliver_at_ms: u64,
    seq: u64,
    payload: Vec<u8>,
}

impl Ord for ScheduledFrame {
    fn cmp(&self, other: &Self) -> CmpOrdering {
        // Min-heap ordering: earliest virtual delivery timestamp first, then
        // enqueue sequence for deterministic FIFO tie-breaking.
        other
            .deliver_at_ms
            .cmp(&self.deliver_at_ms)
            .then_with(|| other.seq.cmp(&self.seq))
            .then_with(|| other.payload.cmp(&self.payload))
    }
}

impl PartialOrd for ScheduledFrame {
    fn partial_cmp(&self, other: &Self) -> Option<CmpOrdering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug)]
struct ConnState {
    /// Peer connection id; `HEW_CONN_INVALID` once the peer detaches.
    peer: c_int,
    /// Inbound frames scheduled on the per-instance virtual clock.
    inbox: BinaryHeap<ScheduledFrame>,
    /// Monotonic enqueue sequence for deterministic same-tick tie-breaking.
    next_seq: u64,
    /// Set when `close_conn` is called or the transport drops.
    closed: bool,
}

impl ConnState {
    fn new(peer: c_int) -> Self {
        Self {
            peer,
            inbox: BinaryHeap::new(),
            next_seq: 0,
            closed: false,
        }
    }
}

#[derive(Debug, Default)]
struct ConnTable {
    /// Allocated conn id → state.
    conns: HashMap<c_int, ConnState>,
    /// Listening addresses bound via `listen`.
    listeners: HashSet<String>,
    /// Server-side conn ids waiting on `accept`, in deterministic arrival order.
    pending_accepts: VecDeque<c_int>,
}

/// Domain-separated seeded PRNGs for drop and delay rolls so enabling delivery
/// delay does not shift the drop sequence for a given [`SimConfig::seed`].
struct SimRng {
    drop: SmallRng,
    delay: SmallRng,
}

impl SimRng {
    const DELAY_SEED_SALT: u64 = 0x9E37_79B9_7F4A_7C15;

    fn from_seed(seed: u64) -> Self {
        Self {
            drop: SmallRng::seed_from_u64(seed),
            delay: SmallRng::seed_from_u64(seed ^ Self::DELAY_SEED_SALT),
        }
    }
}

/// All wait-predicate state coordinated under one [`Mutex`] paired directly
/// with [`SimTransportState::cv`].
///
/// Guarding `table`, `clock_ms`, `partition`, and `destroyed` under the exact
/// mutex handed to `Condvar::wait` rules out lost wakeups for both `sim_recv`
/// and `sim_accept` without any real-time polling timeout fuse.
#[derive(Debug)]
struct SimCore {
    table: ConnTable,
    /// Per-instance virtual clock in milliseconds, advanced via
    /// [`sim_transport_advance_time_ms`].
    clock_ms: u64,
    /// Active network-partition fault flag.
    partition: bool,
    /// Terminal teardown marker set by [`SimTransportState::shutdown`].
    destroyed: bool,
    /// Number of threads currently parked inside `sim_recv`'s `cv.wait`.
    parked_recvs: usize,
    /// Test-only count of `sim_recv` wakes observed after `cv.wait` returns.
    #[cfg(test)]
    recv_wakes: usize,
    /// Number of threads currently parked inside `sim_accept`'s `cv.wait`.
    parked_accepts: usize,
    /// Test-only count of `sim_accept` wakes observed after `cv.wait` returns.
    #[cfg(test)]
    accept_wakes: usize,
}

/// Internal heavy state for a `SimTransport`. Held behind an `Arc`
/// inside an [`ImplShell`]; reclaimed when the last `Arc` clone is
/// dropped (i.e. after `sim_destroy` has tombstoned the shell *and*
/// every in-flight vtable call has returned).
struct SimTransportState {
    core: Mutex<SimCore>,
    cv: Condvar,
    /// Condvar notified whenever `parked_recvs` / `parked_accepts` increments
    /// right before handing `core` into `cv.wait`, or when `shutdown` sets
    /// `destroyed`.
    parked_cv: Condvar,
    next_conn: AtomicI32,
    drop_rate_ppm: u32,
    min_delay_ms: u64,
    max_delay_ms: u64,
    /// Leaf PRNG mutex for drop and delay rolls. Never acquired across
    /// `core` lock acquisition (strict lock order: `core` → `rng`).
    rng: Mutex<SimRng>,
}

impl SimTransportState {
    fn new(cfg: SimConfig) -> Self {
        let min_delay_ms = cfg.min_delay_ms;
        let max_delay_ms = cfg.max_delay_ms.max(min_delay_ms);
        Self {
            core: Mutex::new(SimCore {
                table: ConnTable::default(),
                clock_ms: 0,
                partition: cfg.partition,
                destroyed: false,
                parked_recvs: 0,
                #[cfg(test)]
                recv_wakes: 0,
                parked_accepts: 0,
                #[cfg(test)]
                accept_wakes: 0,
            }),
            cv: Condvar::new(),
            parked_cv: Condvar::new(),
            // Start at 1 so a default-zero conn id is never valid.
            next_conn: AtomicI32::new(1),
            drop_rate_ppm: cfg.drop_rate_ppm.min(1_000_000),
            min_delay_ms,
            max_delay_ms,
            rng: Mutex::new(SimRng::from_seed(cfg.seed)),
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

    /// Sample whether an accepted send should be dropped by `drop_rate_ppm`.
    fn roll_drop(&self) -> bool {
        if self.drop_rate_ppm == 0 {
            return false;
        }
        let mut rng = self
            .rng
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        rng.drop.random_range(0..1_000_000) < self.drop_rate_ppm
    }

    /// Sample a deterministic virtual delivery delay in `min_delay_ms..=max_delay_ms`.
    fn sample_delay_ms(&self) -> u64 {
        if self.min_delay_ms == self.max_delay_ms {
            return self.min_delay_ms;
        }
        let mut rng = self
            .rng
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        rng.delay
            .random_range(self.min_delay_ms..=self.max_delay_ms)
    }

    /// Mark the transport as terminally torn-down: forces every later
    /// `send` / `recv` / `accept` to fail closed, and wakes any thread
    /// currently parked inside `sim_recv` or `sim_accept`.
    ///
    /// Idempotent: safe to call from both [`sim_destroy`] and from the
    /// `Drop` impl.
    fn shutdown(&self) {
        {
            let mut core = self
                .core
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            core.partition = true;
            core.destroyed = true;
            for state in core.table.conns.values_mut() {
                state.closed = true;
                state.inbox.clear();
            }
            core.table.conns.clear();
            core.table.listeners.clear();
            core.table.pending_accepts.clear();
        }
        self.cv.notify_all();
        self.parked_cv.notify_all();
    }

    /// Block until at least one `sim_recv` call has acquired `core`,
    /// incremented `parked_recvs`, and atomically released `core` into
    /// `cv.wait` (or the transport is destroyed). Returns `true` if a live
    /// receiver is parked, `false` if destroyed.
    fn wait_until_recv_parked(&self) -> bool {
        let guard = self
            .core
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let guard = self
            .parked_cv
            .wait_while(guard, |core| core.parked_recvs == 0 && !core.destroyed)
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        !guard.destroyed
    }

    /// Snapshot how many `sim_recv` wakes have occurred so a test can wait for
    /// a parked receiver to re-evaluate the predicate and re-park.
    #[cfg(test)]
    fn recv_wakes(&self) -> usize {
        self.core
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .recv_wakes
    }

    /// Wait until a parked `sim_recv` has woken past `wakes_before`, checked
    /// the updated state, and parked back inside `cv.wait`.
    #[cfg(test)]
    fn wait_until_recv_reparked(&self, wakes_before: usize) {
        let guard = self
            .core
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let _guard = self
            .parked_cv
            .wait_while(guard, |core| {
                !core.destroyed && (core.recv_wakes <= wakes_before || core.parked_recvs == 0)
            })
            .unwrap_or_else(std::sync::PoisonError::into_inner);
    }

    /// Block until at least one `sim_accept` call is genuinely registered
    /// inside `cv.wait` (or the transport is destroyed). Returns `true` if a
    /// live acceptor is parked, `false` if destroyed.
    fn wait_until_accept_parked(&self) -> bool {
        let guard = self
            .core
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let guard = self
            .parked_cv
            .wait_while(guard, |core| core.parked_accepts == 0 && !core.destroyed)
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        !guard.destroyed
    }

    /// Snapshot how many `sim_accept` wakes have occurred so a test can wait
    /// for a parked acceptor to observe an intermediate tick and re-park.
    #[cfg(test)]
    fn accept_wakes(&self) -> usize {
        self.core
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .accept_wakes
    }

    /// Wait until a parked `sim_accept` has woken past `wakes_before`, checked
    /// the updated virtual clock, and parked back inside `cv.wait`.
    #[cfg(test)]
    fn wait_until_accept_reparked(&self, wakes_before: usize) {
        let guard = self
            .core
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let _guard = self
            .parked_cv
            .wait_while(guard, |core| {
                !core.destroyed && (core.accept_wakes <= wakes_before || core.parked_accepts == 0)
            })
            .unwrap_or_else(std::sync::PoisonError::into_inner);
    }
}

impl Drop for SimTransportState {
    fn drop(&mut self) {
        // `shutdown` is the single source of truth for "wake every
        // waiter and release queued frames" (`cleanup-all-exits`).
        // `sim_destroy` calls it explicitly so blocked recvs/accepts wake
        // *before* their `Arc<SimTransportState>` clones are dropped;
        // this `Drop` call is the safety net for the case where the
        // last `Arc` is held by something other than `sim_destroy`
        // (e.g. an in-flight op that outlived destroy).
        self.shutdown();
    }
}

// ---------------------------------------------------------------------------
// Vtable callbacks
// ---------------------------------------------------------------------------

/// Resolve a vtable `impl_ptr` (passed by the C-ABI) into a strong
/// reference to the live `SimTransportState`, or return `None` if the
/// transport has been destroyed (or was never live and the caller
/// violated the SAFETY contract — same fail-closed return).
///
/// Because [`ImplShell`] is permanently leaked by [`sim_transport_new`],
/// any address we ever minted remains a valid `&ImplShell` for the
/// entire program lifetime. Stale impl pointers therefore read a
/// tombstoned (`state == None`) shell rather than reused/freed memory:
/// no `Box::from_raw` happens here, and address reuse by the system
/// allocator cannot fabricate liveness for a stale pointer because
/// the address is still owned by us.
///
/// # SAFETY
///
/// `impl_ptr` must be either null or a pointer that was produced by
/// [`sim_transport_new`] and embedded in a `HewTransport.r#impl` slot.
/// Passing an arbitrary integer is undefined behaviour, exactly as for
/// the rest of the C-ABI.
unsafe fn shell_state(impl_ptr: *mut c_void) -> Option<Arc<SimTransportState>> {
    if impl_ptr.is_null() {
        return None;
    }
    // SAFETY: caller upholds the contract documented above. `ImplShell`
    // is permanently leaked, so the dereference is sound regardless of
    // how many destroy/free cycles have occurred against this address.
    let shell = unsafe { &*impl_ptr.cast::<ImplShell>() };
    shell
        .state
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .as_ref()
        .map(Arc::clone)
}

unsafe extern "C" fn sim_connect(impl_ptr: *mut c_void, address: *const c_char) -> c_int {
    cabi_guard!(impl_ptr.is_null() || address.is_null(), HEW_CONN_INVALID);
    // SAFETY: caller guarantees `address` is a valid C string.
    let Ok(addr_str) = unsafe { CStr::from_ptr(address) }.to_str() else {
        set_last_error("sim_connect: address was not valid UTF-8");
        return HEW_CONN_INVALID;
    };
    // SAFETY: see `shell_state` contract; fail-closed if tombstoned.
    let Some(st) = (unsafe { shell_state(impl_ptr) }) else {
        set_last_error("sim_connect: transport destroyed or stale");
        return HEW_CONN_INVALID;
    };

    let result = {
        let mut core = st
            .core
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if core.destroyed || core.partition {
            Err("sim_connect: transport partitioned")
        } else if !core.table.listeners.contains(addr_str) {
            Err("sim_connect: no listener bound")
        } else {
            let client_id = st.alloc_conn_id();
            let server_id = st.alloc_conn_id();
            if client_id == HEW_CONN_INVALID || server_id == HEW_CONN_INVALID {
                Err("sim_connect: connection-id space exhausted")
            } else {
                core.table
                    .conns
                    .insert(client_id, ConnState::new(server_id));
                core.table
                    .conns
                    .insert(server_id, ConnState::new(client_id));
                core.table.pending_accepts.push_back(server_id);
                Ok(client_id)
            }
        }
    };
    match result {
        Ok(client_id) => {
            st.cv.notify_all();
            client_id
        }
        Err(msg) => {
            if msg == "sim_connect: no listener bound" {
                set_last_error(format!("sim_connect: no listener bound at {addr_str}"));
            } else {
                set_last_error(msg);
            }
            HEW_CONN_INVALID
        }
    }
}

unsafe extern "C" fn sim_listen(impl_ptr: *mut c_void, address: *const c_char) -> c_int {
    cabi_guard!(impl_ptr.is_null() || address.is_null(), -1);
    // SAFETY: caller guarantees `address` is a valid C string.
    let Ok(addr_str) = unsafe { CStr::from_ptr(address) }.to_str() else {
        set_last_error("sim_listen: address was not valid UTF-8");
        return -1;
    };
    // SAFETY: see `shell_state` contract; fail-closed if tombstoned.
    let Some(st) = (unsafe { shell_state(impl_ptr) }) else {
        set_last_error("sim_listen: transport destroyed or stale");
        return -1;
    };
    let mut core = st
        .core
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    if core.destroyed {
        set_last_error("sim_listen: transport destroyed");
        return -1;
    }
    core.table.listeners.insert(addr_str.to_owned());
    0
}

unsafe extern "C" fn sim_accept(impl_ptr: *mut c_void, timeout_ms: c_int) -> c_int {
    cabi_guard!(impl_ptr.is_null(), HEW_CONN_INVALID);
    // SAFETY: see `shell_state` contract; fail-closed if tombstoned.
    let Some(st) = (unsafe { shell_state(impl_ptr) }) else {
        set_last_error("sim_accept: transport destroyed or stale");
        return HEW_CONN_INVALID;
    };

    let mut core = st
        .core
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let deadline_ms = if timeout_ms < 0 {
        None
    } else {
        #[expect(clippy::cast_sign_loss, reason = "guarded by timeout_ms >= 0")]
        Some(core.clock_ms.saturating_add(timeout_ms as u64))
    };

    loop {
        if core.destroyed {
            drop(core);
            set_last_error("sim_accept: transport destroyed");
            return HEW_CONN_INVALID;
        }
        if !core.partition {
            if let Some(conn) = core.table.pending_accepts.pop_front() {
                return conn;
            }
        }
        if deadline_ms.is_some_and(|deadline| core.clock_ms >= deadline) {
            drop(core);
            set_last_error("sim_accept: timed out waiting for connection");
            return HEW_CONN_INVALID;
        }
        core.parked_accepts += 1;
        st.parked_cv.notify_all();
        core = st
            .cv
            .wait(core)
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        core.parked_accepts -= 1;
        #[cfg(test)]
        {
            core.accept_wakes += 1;
        }
    }
}

unsafe extern "C" fn sim_send(
    impl_ptr: *mut c_void,
    conn: c_int,
    data: *const c_void,
    len: usize,
) -> c_int {
    cabi_guard!(impl_ptr.is_null() || (data.is_null() && len != 0), -1);
    // SAFETY: see `shell_state` contract; fail-closed if tombstoned.
    let Some(st) = (unsafe { shell_state(impl_ptr) }) else {
        set_last_error("sim_send: transport destroyed or stale");
        return HEW_ERR_TRANSPORT;
    };

    if len > MAX_FRAME_SIZE {
        set_last_error(format!(
            "sim_send: frame {len} exceeds MAX_FRAME_SIZE ({MAX_FRAME_SIZE})"
        ));
        return HEW_ERR_TRANSPORT;
    }

    let payload: Vec<u8> = if len == 0 {
        Vec::new()
    } else {
        // SAFETY: caller contract guarantees `data` is valid for `len` bytes when len > 0.
        unsafe { std::slice::from_raw_parts(data.cast::<u8>(), len) }.to_vec()
    };

    let outcome = {
        let mut core = st
            .core
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if core.destroyed || core.partition {
            SendInternal::Partitioned
        } else {
            let now_ms = core.clock_ms;
            let peer_id = match core.table.conns.get(&conn) {
                None => None,
                Some(sender) if sender.closed => None,
                Some(sender) => Some(sender.peer),
            };
            match peer_id {
                None => SendInternal::InvalidConn,
                Some(HEW_CONN_INVALID) => SendInternal::PeerClosed,
                Some(peer_id) => match core.table.conns.get_mut(&peer_id) {
                    None => SendInternal::PeerClosed,
                    Some(peer_state) if peer_state.closed => SendInternal::PeerClosed,
                    Some(peer_state) => {
                        if st.roll_drop() {
                            SendInternal::Dropped
                        } else {
                            let delay_ms = st.sample_delay_ms();
                            let deliver_at_ms = now_ms.saturating_add(delay_ms);
                            let seq = peer_state.next_seq;
                            peer_state.next_seq = peer_state.next_seq.saturating_add(1);
                            peer_state.inbox.push(ScheduledFrame {
                                deliver_at_ms,
                                seq,
                                payload,
                            });
                            SendInternal::Delivered
                        }
                    }
                },
            }
        }
    };

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
        SendInternal::Partitioned => {
            set_last_error("sim_send: partition active");
            HEW_ERR_TRANSPORT
        }
        SendInternal::Dropped => {
            // Caller sees a typed transport failure rather than fabricating a
            // positive ack. The classifier maps the sim-only
            // `HEW_ERR_SIM_DROPPED` code to `DroppedByPolicy` so it can never
            // collide with a peer-close or partition.
            set_last_error("sim_send: frame dropped by drop_rate policy");
            HEW_ERR_SIM_DROPPED
        }
        SendInternal::PeerClosed => {
            // Peer endpoint was closed (or our own conn's peer link was
            // detached by `close_conn` on the other side). Terminal for the
            // connection and distinct from a drop or network partition.
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
    Partitioned,
    Dropped,
    PeerClosed,
    InvalidConn,
}

fn poll_recv_locked(core: &mut SimCore, conn: c_int, buf_size: usize) -> RecvInternal {
    if core.destroyed || core.partition {
        return RecvInternal::Partitioned;
    }
    let now_ms = core.clock_ms;
    let Some(state) = core.table.conns.get_mut(&conn) else {
        return RecvInternal::InvalidConn;
    };
    // Local side closed (we called `close_conn` on this conn). Even if the
    // inbox still holds data, the conn is no longer readable — fail-closed
    // (`cleanup-all-exits`).
    if state.closed {
        return RecvInternal::PeerClosed;
    }
    if let Some(head) = state.inbox.peek() {
        if head.deliver_at_ms <= now_ms {
            if head.payload.len() > buf_size {
                // Leave the frame in place so the caller can retry with a
                // larger buffer — fail-closed without losing data.
                return RecvInternal::BufferTooSmall;
            }
            let frame = state.inbox.pop().expect("peeked frame must be present");
            return RecvInternal::Frame(frame.payload);
        }
        // In-flight delayed frame is still travelling on the simulated wire;
        // wait for virtual time to reach `deliver_at_ms` before surfacing
        // `PeerClosed` if the peer has since detached.
        return RecvInternal::Empty;
    }
    // Inbox is empty. If the peer endpoint is gone, this is terminal: no
    // further frames can ever arrive, and a blocked recv must wake and surface
    // a typed close failure rather than spin forever.
    if state.peer == HEW_CONN_INVALID {
        return RecvInternal::PeerClosed;
    }
    RecvInternal::Empty
}

unsafe extern "C" fn sim_recv(
    impl_ptr: *mut c_void,
    conn: c_int,
    buf: *mut c_void,
    buf_size: usize,
) -> c_int {
    cabi_guard!(impl_ptr.is_null() || buf.is_null(), -1);
    // SAFETY: see `shell_state` contract; fail-closed if tombstoned.
    let Some(st) = (unsafe { shell_state(impl_ptr) }) else {
        set_last_error("sim_recv: transport destroyed or stale");
        return HEW_ERR_TRANSPORT;
    };

    let mut core = st
        .core
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    loop {
        match poll_recv_locked(&mut core, conn, buf_size) {
            RecvInternal::Frame(frame) => {
                drop(core);
                let n = frame.len();
                if n > 0 {
                    // SAFETY: caller guarantees `buf` is valid for `buf_size`
                    // bytes and `n <= buf_size` was checked under `core`.
                    unsafe {
                        std::ptr::copy_nonoverlapping(frame.as_ptr(), buf.cast::<u8>(), n);
                    }
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
            RecvInternal::Partitioned => {
                drop(core);
                set_last_error("sim_recv: partition active");
                return HEW_ERR_TRANSPORT;
            }
            RecvInternal::PeerClosed => {
                drop(core);
                set_last_error("sim_recv: peer endpoint closed or detached");
                return HEW_ERR_SIM_PEER_CLOSED;
            }
            RecvInternal::InvalidConn => {
                drop(core);
                set_last_error(format!("sim_recv: invalid conn {conn}"));
                return -1;
            }
            RecvInternal::BufferTooSmall => {
                drop(core);
                set_last_error("sim_recv: caller buffer too small for next frame");
                return HEW_ERR_SIM_BUFFER_TOO_SMALL;
            }
            RecvInternal::Empty => {
                core.parked_recvs += 1;
                st.parked_cv.notify_all();
                core = st
                    .cv
                    .wait(core)
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                core.parked_recvs -= 1;
                #[cfg(test)]
                {
                    core.recv_wakes += 1;
                }
            }
        }
    }
}

#[derive(Debug)]
enum RecvInternal {
    Frame(Vec<u8>),
    Partitioned,
    Empty,
    PeerClosed,
    InvalidConn,
    BufferTooSmall,
}

unsafe extern "C" fn sim_close_conn(impl_ptr: *mut c_void, conn: c_int) {
    cabi_guard!(impl_ptr.is_null());
    // SAFETY: see `shell_state` contract; fail-closed if tombstoned.
    let Some(st) = (unsafe { shell_state(impl_ptr) }) else {
        set_last_error("sim_close_conn: transport destroyed or stale");
        return;
    };
    {
        let mut core = st
            .core
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if let Some(state) = core.table.conns.get_mut(&conn) {
            state.closed = true;
            state.inbox.clear();
            let peer = state.peer;
            if let Some(peer_state) = core.table.conns.get_mut(&peer) {
                peer_state.peer = HEW_CONN_INVALID;
            }
        }
    }
    st.cv.notify_all();
}

// ---------------------------------------------------------------------------
// Handle shells — the security-critical invariant
// ---------------------------------------------------------------------------

/// Inner handle slab. Permanently leaked by [`sim_transport_new`] so
/// the address of the slab — which the C-ABI vtable carries around as
/// the `impl_ptr` — can never be reused by the system allocator for a
/// different live object. That is how stale-impl-pointer reentry stays
/// bounded to a tombstoned shell rather than authenticating a later
/// allocation that happened to land at the same address (the
/// security-review-r2 / local-review-r3 ABA finding).
///
/// The heavy [`SimTransportState`] (queues, conn table, RNG) lives
/// behind an `Arc` inside the shell. On `sim_destroy` (or
/// [`sim_transport_free`], which calls it) we `take()` the `Arc`,
/// wake any parked recv via [`SimTransportState::shutdown`], then
/// drop our `Arc` clone. In-flight vtable callers each hold their
/// own `Arc` clone obtained via [`shell_state`]; the heavy state is
/// reclaimed when the last `Arc` clone (ours or theirs) is dropped,
/// so heavy resources still release.
///
/// `state` is a `Mutex<Option<...>>` rather than an `AtomicPtr` so the
/// destroy-and-take and the shell_state-and-clone sequences are both
/// trivially race-free. The mutex is a leaf — never held across any
/// other lock, never held across an `Arc::clone`-and-return — so it
/// cannot deadlock with the conn-table lock or the recv `Condvar`.
struct ImplShell {
    state: Mutex<Option<Arc<SimTransportState>>>,
}

/// Outer handle slab. Permanently leaked for the same ABA-resistance
/// reason as [`ImplShell`]: the address handed back from
/// [`sim_transport_new`] (cast from the shell address) can never be
/// reused by the allocator while a stale `*mut HewTransport` exists.
/// `repr(C)` + `inner` first guarantees the `*mut HewTransport`
/// pointer arithmetic is layout-equivalent to a `*mut TransportShell`
/// cast.
///
/// `freed` makes [`sim_transport_free`] idempotent without needing a
/// process-global registry: a second free observes `freed == true`
/// and short-circuits before the inner `destroy` is invoked.
#[repr(C)]
struct TransportShell {
    inner: HewTransport,
    freed: AtomicBool,
}

unsafe extern "C" fn sim_destroy(impl_ptr: *mut c_void) {
    cabi_guard!(impl_ptr.is_null());
    // SAFETY: `ImplShell` is permanently leaked by `sim_transport_new`,
    // so the dereference is sound for any pointer this module ever
    // minted, regardless of how many destroy/free cycles preceded it.
    // The C-ABI safety contract (`impl_ptr` must be a sim-minted
    // address) covers the everything-else case.
    let shell = unsafe { &*impl_ptr.cast::<ImplShell>() };
    let arc_opt = shell
        .state
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .take();
    let Some(arc) = arc_opt else {
        // Either a double-destroy or a stale pointer reentry. Either
        // way, fail-closed: no dereference of heavy state, no
        // double-drop of the `Arc`.
        set_last_error("sim_destroy: stale or double-destroy on impl pointer");
        return;
    };
    // Wake any parked recv BEFORE dropping our `Arc` clone — otherwise
    // a thread parked inside `cv.wait_timeout` would keep its own
    // `Arc` clone alive and the heavy state would not release until
    // the watchdog timeout fires.
    arc.shutdown();
    // Heavy state releases when the refcount hits zero. That happens
    // here if no in-flight op holds a clone, or in the in-flight op's
    // own `drop(arc)` otherwise. Either way `SimTransportState::Drop`
    // runs (idempotent with `shutdown`) and reclaims queues / RNG.
    drop(arc);
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

/// Construct a new `SimTransport`. The returned pointer must be released
/// by [`sim_transport_free`]. Direct vtable `destroy` followed by
/// `Box::from_raw` on the outer pointer is **not** a valid release path
/// — the outer is not a `Box` you can reclaim; it is a pointer into a
/// permanently-leaked [`TransportShell`] (see module docs for the
/// stale-handle / address-reuse rationale).
///
/// # Safety
///
/// - The returned pointer must be released via [`sim_transport_free`].
///   Failing to call it leaks the heavy [`SimTransportState`] (queues,
///   listener table, RNG); the surrounding handle shells are
///   intentionally bounded leaks.
/// - The returned pointer must not be passed to a transport
///   implementation that expects a different vtable.
#[must_use]
pub unsafe fn sim_transport_new(cfg: SimConfig) -> *mut HewTransport {
    // Heavy state behind an `Arc`. `Arc::new` keeps it alive even
    // after `sim_destroy` drops its own clone, until every in-flight
    // op also drops its `shell_state` clone.
    let state = Arc::new(SimTransportState::new(cfg));
    // `Box::into_raw` returns a `*mut` we never reclaim — that is the
    // intentional shell leak. See [`ImplShell`] docs for why the
    // address must outlive any stale pointer to it.
    let impl_shell_ptr: *mut ImplShell = Box::into_raw(Box::new(ImplShell {
        state: Mutex::new(Some(state)),
    }));
    let impl_ptr = impl_shell_ptr.cast::<c_void>();
    let outer_shell_ptr: *mut TransportShell = Box::into_raw(Box::new(TransportShell {
        inner: HewTransport {
            ops: &raw const SIM_OPS,
            r#impl: impl_ptr,
        },
        freed: AtomicBool::new(false),
    }));
    // `repr(C)` + `inner` first guarantees the address of the shell
    // and the address of `shell.inner` are equal, so a later
    // `*mut HewTransport`-to-`*mut TransportShell` cast in
    // `sim_transport_free` is sound.
    outer_shell_ptr.cast::<HewTransport>()
}

/// Release a `SimTransport` returned by [`sim_transport_new`]. This is
/// the **only** canonical release path. It tombstones the inner heavy
/// state via the vtable's `destroy` op (which atomically takes the
/// `Arc<SimTransportState>` out of the [`ImplShell`] and wakes
/// waiters), then marks the outer shell as freed. Both shells remain
/// leaked by design so subsequent stale-pointer reentry sees
/// tombstoned slabs rather than reused/freed memory — the
/// stale-handle / address-reuse invariant.
///
/// # Safety
///
/// `transport` must be either null or a pointer returned by
/// [`sim_transport_new`] in this process. A second call on the same
/// pointer (or a stale call from a sim-minted pointer) is a typed
/// `set_last_error` no-op rather than a use-after-free: the outer
/// shell's `freed` flag short-circuits before the inner destroy runs,
/// and the inner destroy itself is idempotent because the shell's
/// `Mutex<Option<Arc<...>>>::take()` returns `None` on the second call.
pub unsafe fn sim_transport_free(transport: *mut HewTransport) {
    if transport.is_null() {
        return;
    }
    // SAFETY: `TransportShell` is permanently leaked by
    // `sim_transport_new`, so this dereference is sound regardless of
    // how many free/destroy cycles preceded it. C-ABI contract covers
    // the everything-else case (caller must pass a sim-minted ptr).
    let shell = unsafe { &*transport.cast::<TransportShell>() };
    if shell
        .freed
        .compare_exchange(false, true, Ordering::AcqRel, Ordering::Acquire)
        .is_err()
    {
        // Another free already won the race (or this is a second call
        // from the same caller). Fail-closed.
        set_last_error("sim_transport_free: stale or double-free on transport pointer");
        return;
    }
    // We won the CAS. Tombstone the inner state via the vtable; the
    // destroy op is itself idempotent (so a prior direct vtable
    // `destroy` followed by `sim_transport_free` is harmless).
    // SAFETY: ops pointer is part of the leaked shell's HewTransport.
    if let Some(ops) = unsafe { shell.inner.ops.as_ref() } {
        if let Some(destroy_fn) = ops.destroy {
            // SAFETY: `r#impl` is the address of our leaked `ImplShell`
            // (or a previously-tombstoned one if vtable destroy already
            // fired); both are safe to pass to `sim_destroy`.
            unsafe { destroy_fn(shell.inner.r#impl) };
        }
    }
    // Outer shell is intentionally NOT freed — see [`TransportShell`]
    // docs. Heavy state inside `ImplShell.state` released above (or
    // when the last in-flight `Arc` clone drops).
}

/// Toggle the partition state of a `SimTransport` at runtime.
///
/// While `partitioned` is true, every `send` and `recv` returns
/// `HEW_ERR_TRANSPORT` immediately. Any thread blocked in `recv` is woken
/// before the function returns. Calling on a destroyed/stale transport
/// is a typed no-op (fail-closed).
///
/// # Safety
///
/// `transport` must be either null or a pointer returned by
/// [`sim_transport_new`] in this process. Stale or destroyed pointers
/// are detected via the [`ImplShell`] tombstone and short-circuit
/// without dereferencing heavy state.
pub unsafe fn sim_transport_set_partition(transport: *mut HewTransport, partitioned: bool) {
    if transport.is_null() {
        return;
    }
    // SAFETY: leaked `TransportShell`; dereference is always sound.
    let shell = unsafe { &*transport.cast::<TransportShell>() };
    if shell.inner.r#impl.is_null() {
        return;
    }
    // SAFETY: see `shell_state` contract; fail-closed if tombstoned.
    let Some(st) = (unsafe { shell_state(shell.inner.r#impl) }) else {
        set_last_error("sim_transport_set_partition: transport destroyed or stale");
        return;
    };
    {
        let mut core = st
            .core
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if !core.destroyed {
            core.partition = partitioned;
        }
    }
    st.cv.notify_all();
}

/// Read the current per-instance virtual clock (in milliseconds) for a
/// `SimTransport`. Returns `0` for null or destroyed/stale pointers.
///
/// # Safety
///
/// `transport` must be either null or a pointer returned by
/// [`sim_transport_new`] in this process.
#[must_use]
pub unsafe fn sim_transport_now_ms(transport: *mut HewTransport) -> u64 {
    if transport.is_null() {
        return 0;
    }
    // SAFETY: leaked `TransportShell`; dereference is always sound.
    let shell = unsafe { &*transport.cast::<TransportShell>() };
    if shell.inner.r#impl.is_null() {
        return 0;
    }
    // SAFETY: see `shell_state` contract; fail-closed if tombstoned.
    let Some(st) = (unsafe { shell_state(shell.inner.r#impl) }) else {
        set_last_error("sim_transport_now_ms: transport destroyed or stale");
        return 0;
    };
    let now = st
        .core
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .clock_ms;
    now
}

/// Advance the per-instance virtual clock of a `SimTransport` by `delta_ms`
/// (saturating at `u64::MAX`), wake any parked `recv` or `accept` waiters, and
/// return the updated virtual timestamp in milliseconds. Returns `0` for null
/// or destroyed/stale pointers.
///
/// # Safety
///
/// `transport` must be either null or a pointer returned by
/// [`sim_transport_new`] in this process.
pub unsafe fn sim_transport_advance_time_ms(transport: *mut HewTransport, delta_ms: u64) -> u64 {
    if transport.is_null() {
        return 0;
    }
    // SAFETY: leaked `TransportShell`; dereference is always sound.
    let shell = unsafe { &*transport.cast::<TransportShell>() };
    if shell.inner.r#impl.is_null() {
        return 0;
    }
    // SAFETY: see `shell_state` contract; fail-closed if tombstoned.
    let Some(st) = (unsafe { shell_state(shell.inner.r#impl) }) else {
        set_last_error("sim_transport_advance_time_ms: transport destroyed or stale");
        return 0;
    };
    let now = {
        let mut core = st
            .core
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        core.clock_ms = core.clock_ms.saturating_add(delta_ms);
        core.clock_ms
    };
    st.cv.notify_all();
    now
}

/// Block until at least one `accept` call is parked waiting on this
/// transport's virtual clock/connect condvar, or until the transport is
/// destroyed. Returns `true` if a live acceptor is parked, `false` for null or
/// destroyed transports.
///
/// # Safety
///
/// `transport` must be either null or a pointer returned by
/// [`sim_transport_new`] in this process.
#[must_use]
pub unsafe fn sim_transport_wait_until_accept_parked(transport: *mut HewTransport) -> bool {
    if transport.is_null() {
        return false;
    }
    // SAFETY: leaked `TransportShell`; dereference is always sound.
    let shell = unsafe { &*transport.cast::<TransportShell>() };
    if shell.inner.r#impl.is_null() {
        return false;
    }
    // SAFETY: see `shell_state` contract; fail-closed if tombstoned.
    let Some(st) = (unsafe { shell_state(shell.inner.r#impl) }) else {
        set_last_error("sim_transport_wait_until_accept_parked: transport destroyed or stale");
        return false;
    };
    st.wait_until_accept_parked()
}

/// Block until at least one `recv` call is parked waiting on this transport's
/// virtual clock/delivery condvar, or until the transport is destroyed.
/// Returns `true` if a live receiver is parked, `false` for null or destroyed
/// transports.
///
/// # Safety
///
/// `transport` must be either null or a pointer returned by
/// [`sim_transport_new`] in this process.
#[must_use]
pub unsafe fn sim_transport_wait_until_recv_parked(transport: *mut HewTransport) -> bool {
    if transport.is_null() {
        return false;
    }
    // SAFETY: leaked `TransportShell`; dereference is always sound.
    let shell = unsafe { &*transport.cast::<TransportShell>() };
    if shell.inner.r#impl.is_null() {
        return false;
    }
    // SAFETY: see `shell_state` contract; fail-closed if tombstoned.
    let Some(st) = (unsafe { shell_state(shell.inner.r#impl) }) else {
        set_last_error("sim_transport_wait_until_recv_parked: transport destroyed or stale");
        return false;
    };
    st.wait_until_recv_parked()
}

/// Inspect the earliest scheduled virtual delivery timestamp (in milliseconds)
/// for a connection's inbound queue, or `None` if no frames are queued, the
/// connection is closed/invalid, or the transport is partitioned/destroyed.
///
/// # Safety
///
/// `transport` must be either null or a pointer returned by
/// [`sim_transport_new`] in this process.
#[must_use]
pub unsafe fn sim_transport_next_delivery_ms(
    transport: *mut HewTransport,
    conn: c_int,
) -> Option<u64> {
    if transport.is_null() {
        return None;
    }
    // SAFETY: leaked `TransportShell`; dereference is always sound.
    let shell = unsafe { &*transport.cast::<TransportShell>() };
    if shell.inner.r#impl.is_null() {
        return None;
    }
    // SAFETY: see `shell_state` contract; fail-closed if tombstoned.
    let st = unsafe { shell_state(shell.inner.r#impl) }?;
    let core = st
        .core
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    if core.destroyed || core.partition {
        return None;
    }
    let state = core.table.conns.get(&conn)?;
    if state.closed {
        return None;
    }
    state.inbox.peek().map(|frame| frame.deliver_at_ms)
}

/// Non-blocking classified receive at the transport's current virtual clock.
///
/// Returns `None` when the connection is healthy and unpartitioned but no
/// frame is deliverable at the current virtual timestamp (either the inbox is
/// empty or all queued frames have `deliver_at_ms > now_ms`). Returns
/// `Some(outcome)` immediately for delivered frames, partitions, peer close,
/// invalid connections, or undersized buffers.
///
/// # Safety
///
/// `transport` must be either null or a pointer returned by
/// [`sim_transport_new`] in this process, and `buf` must be valid for
/// `buf.len()` writable bytes.
pub unsafe fn sim_transport_try_recv_classified(
    transport: *mut HewTransport,
    conn: c_int,
    buf: &mut [u8],
) -> Option<SimRecvOutcome> {
    if transport.is_null() {
        return Some(SimRecvOutcome::InvalidConn);
    }
    // SAFETY: leaked `TransportShell`; dereference is always sound.
    let shell = unsafe { &*transport.cast::<TransportShell>() };
    if shell.inner.r#impl.is_null() {
        return Some(SimRecvOutcome::InvalidConn);
    }
    // SAFETY: see `shell_state` contract; fail-closed if tombstoned.
    let Some(st) = (unsafe { shell_state(shell.inner.r#impl) }) else {
        set_last_error("sim_transport_try_recv_classified: transport destroyed or stale");
        return Some(SimRecvOutcome::Partitioned);
    };
    let outcome = {
        let mut core = st
            .core
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        poll_recv_locked(&mut core, conn, buf.len())
    };
    match outcome {
        RecvInternal::Frame(frame) => {
            let n = frame.len();
            buf[..n].copy_from_slice(&frame);
            Some(SimRecvOutcome::Received { bytes_received: n })
        }
        RecvInternal::Empty => None,
        RecvInternal::Partitioned => {
            set_last_error("sim_recv: partition active");
            Some(SimRecvOutcome::Partitioned)
        }
        RecvInternal::PeerClosed => {
            set_last_error("sim_recv: peer endpoint closed or detached");
            Some(SimRecvOutcome::PeerClosed)
        }
        RecvInternal::InvalidConn => {
            set_last_error(format!("sim_recv: invalid conn {conn}"));
            Some(SimRecvOutcome::InvalidConn)
        }
        RecvInternal::BufferTooSmall => {
            set_last_error("sim_recv: caller buffer too small for next frame");
            Some(SimRecvOutcome::BufferTooSmall)
        }
    }
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
        use std::thread;

        // SAFETY: t is owned for the duration of this test; both threads
        // hold the address as a usize and reconstitute the pointer locally.
        // The transport is freed only after the recv thread joins.
        unsafe {
            let t = sim_transport_new(SimConfig::default());
            let (client, server) = make_pair(t, "sim://peer-close-wake:0");

            let t_addr = t as usize;
            let state = shell_state((*t).r#impl).expect("transport must be live");

            let recv_handle = thread::spawn(move || {
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

            // Wait until the recv loop has parked on the empty connection.
            assert!(state.wait_until_recv_parked());
            drop(state);

            // Close the client side — this should wake the blocked recv via
            // `cv.notify_all()` inside `sim_close_conn`.
            let transport_ref = &*t;
            let ops = &*transport_ref.ops;
            ops.close_conn.unwrap()(transport_ref.r#impl, client);

            // The close wakes the parked recv.
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

    /// Security regression (ABA / stale-handle after address reuse —
    /// security review r2 finding 1, local review r3 blocker).
    ///
    /// The previous design kept a `HashSet<usize>` of live pointer
    /// addresses; if the system allocator reused an old transport's
    /// address for a new live transport, the stale pointer could
    /// authenticate against the registry and free/destroy the new
    /// allocation. The new design defeats this by leaking the outer
    /// `TransportShell` and inner `ImplShell` permanently — the
    /// allocator can never hand out the same address for any other
    /// allocation while we still own it.
    ///
    /// This test pins both halves of the invariant deterministically:
    ///
    ///   1. After `sim_transport_free(t1)`, no subsequent
    ///      `sim_transport_new` call may produce a `*mut HewTransport`
    ///      with the same address as `t1` — proving the outer shell
    ///      is not returned to the allocator.
    ///   2. The same property holds for the inner impl pointer
    ///      (`(*t).r#impl`) — proving the inner shell is not returned
    ///      either, so a stale impl pointer cannot alias a new live
    ///      `ImplShell`.
    ///   3. Reentry on the stale outer/impl pointers must be a typed
    ///      no-op (no UB) regardless of the new allocations.
    ///
    /// "Many" here is `64`. That is overkill for the address-reuse
    /// invariant (a single new alloc would be enough proof if the
    /// design is sound) but it makes the test robust against
    /// allocator-pool warmup quirks and gives the property a wide
    /// margin.
    #[test]
    fn stale_handle_cannot_authenticate_after_address_reuse() {
        // SAFETY: stale-pointer reentry exercises the leaked-shell
        // tombstone path. No `Box::from_raw` is reconstructed; both
        // dereferences are sound because the shells are leaked.
        unsafe {
            let t1 = sim_transport_new(SimConfig::default());
            let stale_outer = t1 as usize;
            let stale_impl = (*t1).r#impl as usize;
            sim_transport_free(t1);

            // Allocate many fresh transports. Save their addresses
            // so we can later assert no aliasing against the stale
            // pair, and also so we can free them at the end.
            let mut fresh: Vec<*mut HewTransport> = Vec::with_capacity(64);
            for _ in 0..64 {
                fresh.push(sim_transport_new(SimConfig::default()));
            }
            for &p in &fresh {
                assert_ne!(
                    p as usize, stale_outer,
                    "outer shell address was reused — leaked-shell invariant violated"
                );
                assert_ne!(
                    (*p).r#impl as usize,
                    stale_impl,
                    "inner shell address was reused — leaked-shell invariant violated"
                );
            }

            // Reentry on the stale outer pointer: must be a typed
            // no-op (idempotent freed flag short-circuits before the
            // inner destroy). Crucially, this MUST NOT free or
            // destroy any of the freshly-allocated transports.
            sim_transport_free(stale_outer as *mut HewTransport);

            // Reentry on the stale impl pointer via the static
            // vtable: must be a typed no-op (the tombstoned
            // `ImplShell.state == None` short-circuits before any
            // heavy-state access). MUST NOT touch any fresh impl.
            let destroy_fn = SIM_OPS.destroy.unwrap();
            destroy_fn(stale_impl as *mut c_void);

            // Every fresh transport must still be fully functional
            // — proves the stale reentry did not free/destroy them.
            for &p in &fresh {
                let (client, server) = make_pair(p, &format!("sim://aba:{:p}", p as *const ()));
                let send = sim_transport_send_classified(p, client, b"ok");
                assert!(
                    matches!(send, SimSendOutcome::Sent { bytes_sent: 2 }),
                    "fresh transport at {p:p} was disturbed by stale reentry: {send:?}"
                );
                let mut buf = [0u8; 8];
                let recv = sim_transport_recv_classified(p, server, &mut buf);
                assert!(
                    matches!(recv, SimRecvOutcome::Received { bytes_received: 2 }),
                    "fresh transport at {p:p} recv was disturbed: {recv:?}"
                );
            }

            for p in fresh {
                sim_transport_free(p);
            }
        }
    }

    /// Security regression (release-contract symmetry — security
    /// review r2 finding 2). Previously the docs allowed a caller to
    /// call the vtable's `destroy` and then `Box::from_raw` the outer
    /// pointer themselves; only `sim_transport_free` removed the
    /// outer pointer from the registry, so the registry kept a stale
    /// entry for freed memory.
    ///
    /// The new design closes this: the outer is not a `Box` to
    /// reclaim; it is a leaked `TransportShell`. After a direct
    /// vtable destroy, every later vtable op on the same outer
    /// pointer fails closed (the inner `ImplShell.state` is `None`),
    /// and a follow-on `sim_transport_free` is harmless (the inner
    /// destroy is idempotent and the outer `freed` CAS still wins
    /// exactly once).
    #[test]
    fn vtable_destroy_then_outer_ops_fail_closed() {
        // SAFETY: standard construction; the direct vtable destroy
        // tombstones the inner shell, after which every vtable op
        // must fail closed without UB.
        unsafe {
            let t = sim_transport_new(SimConfig::default());
            let (client, _server) = make_pair(t, "sim://vtable-destroy:0");

            // Call the vtable's destroy directly — the historical
            // "alternate" release path. With the new design this
            // tombstones the inner state but does NOT release the
            // outer shell.
            let transport_ref = &*t;
            let ops = &*transport_ref.ops;
            ops.destroy.unwrap()(transport_ref.r#impl);

            // Subsequent vtable ops must fail closed. Send → typed
            // partition (HEW_ERR_TRANSPORT). This proves the inner
            // state is no longer accessible.
            let send = sim_transport_send_classified(t, client, b"x");
            assert!(
                matches!(send, SimSendOutcome::Partitioned),
                "send after vtable destroy must fail closed, got {send:?}"
            );
            let mut buf = [0u8; 8];
            let recv = sim_transport_recv_classified(t, client, &mut buf);
            assert!(
                matches!(recv, SimRecvOutcome::Partitioned),
                "recv after vtable destroy must fail closed, got {recv:?}"
            );

            // A second vtable destroy on the same impl is also a
            // typed no-op (Mutex<Option<Arc<…>>>::take returned
            // None), not a double-free.
            ops.destroy.unwrap()(transport_ref.r#impl);

            // sim_transport_free still completes cleanly — the
            // inner destroy it invokes is idempotent, and the outer
            // `freed` flag flips on the first (and only) call.
            sim_transport_free(t);

            // After the canonical release, a second sim_transport_free
            // is also a typed no-op via the outer `freed` CAS.
            sim_transport_free(t);
        }
    }

    /// Security regression: `sim_destroy` must release the heavy
    /// inner state (queues, listener table, RNG) so a long-running
    /// test process does not accumulate `SimTransportState` heaps
    /// across many destroy/free cycles. The shells themselves leak
    /// by design (bounded ~ tens of bytes per construction); the
    /// heavy state must not.
    ///
    /// We pin this with `Arc::strong_count` indirectly: after
    /// `sim_destroy`, the inner shell's `state` slot must be `None`
    /// — i.e. no `Arc<SimTransportState>` reference is retained by
    /// the shell. Combined with no in-flight ops, the heavy state
    /// drops immediately.
    #[test]
    fn sim_destroy_releases_inner_heavy_state() {
        // SAFETY: we read the leaked `ImplShell` directly to confirm
        // its `state` slot tombstones to `None` after destroy.
        unsafe {
            let t = sim_transport_new(SimConfig::default());
            let impl_ptr = (*t).r#impl;
            // Pre-destroy: state is Some.
            {
                let shell = &*impl_ptr.cast::<ImplShell>();
                assert!(
                    shell.state.lock().unwrap().is_some(),
                    "pre-destroy: shell state must be Some"
                );
            }
            // Direct vtable destroy.
            let ops = &*(*t).ops;
            ops.destroy.unwrap()(impl_ptr);
            // Post-destroy: state is None — Arc has been taken and
            // dropped, heavy state released (no in-flight clones).
            {
                let shell = &*impl_ptr.cast::<ImplShell>();
                assert!(
                    shell.state.lock().unwrap().is_none(),
                    "post-destroy: shell state must be None (heavy state released)"
                );
            }
            sim_transport_free(t);
        }
    }

    /// Security regression: a `sim_transport_free` must wake any
    /// recv parked inside `cv.wait` so the parked thread's
    /// `Arc<SimTransportState>` clone is dropped promptly and the
    /// heavy state can be reclaimed.
    ///
    /// Because `sim_recv` uses an unbounded `cv.wait` paired directly with the
    /// `core` mutex that `shutdown()` updates before calling `cv.notify_all()`,
    /// there is no polling fuse to race — the only way the recv loop can resume
    /// is a genuine destroy-side notify, so the deterministic outcome
    /// (`Partitioned`) is itself the proof.
    #[test]
    fn destroy_wakes_blocked_recv() {
        use std::thread;

        // SAFETY: we pass `t` to the recv thread via a usize and
        // call sim_transport_free from the main thread; the recv
        // thread only holds an `Arc` clone of the heavy state, so
        // tombstoning the outer pointer is safe.
        unsafe {
            let t = sim_transport_new(SimConfig::default());
            let (_client, server) = make_pair(t, "sim://destroy-wake:0");
            let t_addr = t as usize;

            let state = shell_state((*t).r#impl).expect("transport must be live");

            let recv_handle = thread::spawn(move || {
                let mut buf = [0u8; 32];
                let t_local = t_addr as *mut HewTransport;
                #[allow(unused_unsafe, reason = "explicit unsafe block at the FFI call site")]
                // SAFETY: t_local points to a leaked TransportShell;
                // dereference is sound for the entire program lifetime.
                unsafe {
                    sim_transport_recv_classified(t_local, server, &mut buf)
                }
            });

            assert!(state.wait_until_recv_parked());
            drop(state);

            sim_transport_free(t);

            let outcome = recv_handle.join().expect("recv thread panicked");
            assert!(
                matches!(outcome, SimRecvOutcome::Partitioned),
                "expected a destroy wake (Partitioned), got {outcome:?}"
            );
        }
    }

    #[test]
    fn fixed_delay_holds_frame_until_exact_virtual_tick() {
        // SAFETY: standard test construction / destruction.
        unsafe {
            // `max_delay_ms < min_delay_ms` clamps to a fixed `min_delay_ms` delay.
            let cfg = SimConfig {
                seed: 11,
                min_delay_ms: 25,
                max_delay_ms: 0,
                ..SimConfig::default()
            };
            let t = sim_transport_new(cfg);
            let (client, server) = make_pair(t, "sim://fixed-delay:0");
            assert_eq!(sim_transport_now_ms(t), 0);

            assert_eq!(
                sim_transport_send_classified(t, client, b"delayed"),
                SimSendOutcome::Sent { bytes_sent: 7 }
            );
            assert_eq!(sim_transport_next_delivery_ms(t, server), Some(25));

            let mut buf = [0u8; 16];
            assert_eq!(sim_transport_try_recv_classified(t, server, &mut buf), None);

            assert_eq!(sim_transport_advance_time_ms(t, 24), 24);
            assert_eq!(sim_transport_try_recv_classified(t, server, &mut buf), None);

            assert_eq!(sim_transport_advance_time_ms(t, 1), 25);
            assert_eq!(
                sim_transport_try_recv_classified(t, server, &mut buf),
                Some(SimRecvOutcome::Received { bytes_received: 7 })
            );
            assert_eq!(&buf[..7], b"delayed");
            assert_eq!(sim_transport_next_delivery_ms(t, server), None);

            sim_transport_free(t);
        }
    }

    #[test]
    fn blocking_recv_waits_for_virtual_clock_delay_then_delivers() {
        use std::thread;

        // SAFETY: `t` outlives the spawned recv thread, which is joined before free.
        unsafe {
            let cfg = SimConfig {
                seed: 17,
                min_delay_ms: 30,
                max_delay_ms: 30,
                ..SimConfig::default()
            };
            let t = sim_transport_new(cfg);
            let (client, server) = make_pair(t, "sim://blocking-delay:0");
            assert_eq!(
                sim_transport_send_classified(t, client, b"on-time"),
                SimSendOutcome::Sent { bytes_sent: 7 }
            );

            let state = shell_state((*t).r#impl).expect("transport must be live");
            let t_addr = t as usize;
            let recv_handle = thread::spawn(move || {
                let mut buf = [0u8; 16];
                let t_local = t_addr as *mut HewTransport;
                #[allow(unused_unsafe, reason = "explicit unsafe block at FFI call site")]
                // SAFETY: `t_local` remains live until joined below.
                let outcome = unsafe { sim_transport_recv_classified(t_local, server, &mut buf) };
                (outcome, buf)
            });

            assert!(state.wait_until_recv_parked());
            let wakes = state.recv_wakes();
            assert_eq!(sim_transport_advance_time_ms(t, 29), 29);
            state.wait_until_recv_reparked(wakes);
            drop(state);

            assert_eq!(sim_transport_advance_time_ms(t, 1), 30);
            let (outcome, buf) = recv_handle.join().expect("recv thread panicked");
            assert_eq!(outcome, SimRecvOutcome::Received { bytes_received: 7 });
            assert_eq!(&buf[..7], b"on-time");

            sim_transport_free(t);
        }
    }

    #[test]
    fn variable_delay_reorders_frames_deterministically_by_seed() {
        fn run_trace(seed: u64) -> Vec<(u64, u32)> {
            // SAFETY: standard test construction / destruction.
            unsafe {
                let cfg = SimConfig {
                    seed,
                    drop_rate_ppm: 0,
                    min_delay_ms: 5,
                    max_delay_ms: 50,
                    ..SimConfig::default()
                };
                let t = sim_transport_new(cfg);
                let (client, server) = make_pair(t, "sim://reorder:0");

                for id in 0u32..24 {
                    let bytes = id.to_le_bytes();
                    assert_eq!(
                        sim_transport_send_classified(t, client, &bytes),
                        SimSendOutcome::Sent { bytes_sent: 4 }
                    );
                }

                let mut trace = Vec::with_capacity(24);
                for tick in 0u64..=50 {
                    if tick > 0 {
                        assert_eq!(sim_transport_advance_time_ms(t, 1), tick);
                    }
                    loop {
                        let mut buf = [0u8; 4];
                        match sim_transport_try_recv_classified(t, server, &mut buf) {
                            Some(SimRecvOutcome::Received { bytes_received: 4 }) => {
                                assert!(
                                    (5..=50).contains(&tick),
                                    "frame delivered at {tick}, outside [5, 50]"
                                );
                                trace.push((tick, u32::from_le_bytes(buf)));
                            }
                            None => break,
                            other => panic!("unexpected recv outcome at tick {tick}: {other:?}"),
                        }
                    }
                }
                assert_eq!(trace.len(), 24);
                sim_transport_free(t);
                trace
            }
        }

        let first = run_trace(0xC0FF_EE42);
        let second = run_trace(0xC0FF_EE42);
        assert_eq!(
            first, second,
            "same seed must replay bit-identical delivery ticks and frame ordering"
        );
        let ids: Vec<u32> = first.iter().map(|&(_, id)| id).collect();
        let in_order: Vec<u32> = (0u32..24).collect();
        assert_ne!(
            ids, in_order,
            "variable delay in [5, 50] ms across 24 frames should observe packet reordering"
        );
    }

    #[test]
    fn delayed_frames_survive_peer_close_until_virtual_delivery() {
        // SAFETY: standard test construction / destruction.
        unsafe {
            let cfg = SimConfig {
                seed: 5,
                min_delay_ms: 15,
                max_delay_ms: 15,
                ..SimConfig::default()
            };
            let t = sim_transport_new(cfg);
            let (client, server) = make_pair(t, "sim://delay-close:0");
            assert_eq!(
                sim_transport_send_classified(t, client, b"last-word"),
                SimSendOutcome::Sent { bytes_sent: 9 }
            );
            let tref = &*t;
            let ops = &*tref.ops;
            ops.close_conn.unwrap()(tref.r#impl, client);

            let mut buf = [0u8; 16];
            assert_eq!(sim_transport_advance_time_ms(t, 14), 14);
            assert_eq!(sim_transport_try_recv_classified(t, server, &mut buf), None);

            assert_eq!(sim_transport_advance_time_ms(t, 1), 15);
            assert_eq!(
                sim_transport_try_recv_classified(t, server, &mut buf),
                Some(SimRecvOutcome::Received { bytes_received: 9 })
            );
            assert_eq!(&buf[..9], b"last-word");

            // Once all in-flight frames are drained, PeerClosed surfaces.
            assert_eq!(
                sim_transport_try_recv_classified(t, server, &mut buf),
                Some(SimRecvOutcome::PeerClosed)
            );
            sim_transport_free(t);
        }
    }

    #[test]
    fn accept_zero_timeout_polls_without_blocking() {
        // SAFETY: standard test construction / destruction.
        unsafe {
            let t = sim_transport_new(SimConfig::default());
            let caddr = CString::new("sim://poll-accept:0").unwrap();
            let tref = &*t;
            let ops = &*tref.ops;
            assert_eq!(ops.listen.unwrap()(tref.r#impl, caddr.as_ptr()), 0);
            assert_eq!(ops.accept.unwrap()(tref.r#impl, 0), HEW_CONN_INVALID);
            let client = ops.connect.unwrap()(tref.r#impl, caddr.as_ptr());
            assert!(client > 0);
            let server = ops.accept.unwrap()(tref.r#impl, 0);
            assert!(server > 0);
            sim_transport_free(t);
        }
    }

    #[test]
    fn accept_timeout_expires_at_exact_virtual_deadline() {
        use std::thread;

        // SAFETY: `t` outlives the acceptor thread joined below.
        unsafe {
            let t = sim_transport_new(SimConfig::default());
            let caddr = CString::new("sim://accept-timeout:0").unwrap();
            let tref = &*t;
            let ops = &*tref.ops;
            assert_eq!(ops.listen.unwrap()(tref.r#impl, caddr.as_ptr()), 0);

            let state = shell_state(tref.r#impl).expect("transport must be live");
            let t_addr = t as usize;
            let accept_handle = thread::spawn(move || {
                let t_local = t_addr as *mut HewTransport;
                #[allow(unused_unsafe, reason = "explicit unsafe block at FFI call site")]
                // SAFETY: `t_local` remains live until joined below.
                unsafe {
                    let tref = &*t_local;
                    let ops = &*tref.ops;
                    ops.accept.unwrap()(tref.r#impl, 200)
                }
            });

            assert!(state.wait_until_accept_parked());
            let wakes = state.accept_wakes();
            assert_eq!(sim_transport_advance_time_ms(t, 199), 199);
            state.wait_until_accept_reparked(wakes);
            drop(state);

            assert_eq!(sim_transport_advance_time_ms(t, 1), 200);
            assert_eq!(
                accept_handle.join().expect("accept thread panicked"),
                HEW_CONN_INVALID
            );

            sim_transport_free(t);
        }
    }

    #[test]
    fn accept_reconnect_recovers_after_partition_heal_before_timeout() {
        use std::thread;

        // SAFETY: `t` outlives the acceptor thread joined below.
        unsafe {
            let t = sim_transport_new(SimConfig::default());
            let addr = "sim://reconnect-heal:0";
            let caddr = CString::new(addr).unwrap();
            let (client1, server1) = make_pair(t, addr);

            // Simulate partition breaking session 1.
            sim_transport_set_partition(t, true);
            assert_eq!(
                sim_transport_send_classified(t, client1, b"lost"),
                SimSendOutcome::Partitioned
            );
            let tref = &*t;
            let ops = &*tref.ops;
            ops.close_conn.unwrap()(tref.r#impl, client1);
            ops.close_conn.unwrap()(tref.r#impl, server1);

            let state = shell_state(tref.r#impl).expect("transport must be live");
            let t_addr = t as usize;
            let accept_handle = thread::spawn(move || {
                let t_local = t_addr as *mut HewTransport;
                #[allow(unused_unsafe, reason = "explicit unsafe block at FFI call site")]
                // SAFETY: `t_local` remains live until joined below.
                unsafe {
                    let tref = &*t_local;
                    let ops = &*tref.ops;
                    ops.accept.unwrap()(tref.r#impl, 300)
                }
            });

            assert!(state.wait_until_accept_parked());
            // While still partitioned at t=100, client reconnect fails closed.
            let wakes = state.accept_wakes();
            assert_eq!(sim_transport_advance_time_ms(t, 100), 100);
            state.wait_until_accept_reparked(wakes);
            assert_eq!(
                ops.connect.unwrap()(tref.r#impl, caddr.as_ptr()),
                HEW_CONN_INVALID
            );
            drop(state);

            // Heal before deadline (t=150 < 300) and reconnect.
            sim_transport_set_partition(t, false);
            assert_eq!(sim_transport_advance_time_ms(t, 50), 150);
            let client2 = ops.connect.unwrap()(tref.r#impl, caddr.as_ptr());
            assert!(client2 > 0);
            let server2 = accept_handle.join().expect("accept thread panicked");
            assert!(server2 > 0);

            assert_eq!(
                sim_transport_send_classified(t, client2, b"recovered"),
                SimSendOutcome::Sent { bytes_sent: 9 }
            );
            let mut buf = [0u8; 16];
            assert_eq!(
                sim_transport_recv_classified(t, server2, &mut buf),
                SimRecvOutcome::Received { bytes_received: 9 }
            );
            assert_eq!(&buf[..9], b"recovered");

            sim_transport_free(t);
        }
    }

    #[test]
    fn destroy_wakes_blocked_accept() {
        use std::thread;

        // SAFETY: `t` points to a leaked shell; acceptor thread is joined before return.
        unsafe {
            let t = sim_transport_new(SimConfig::default());
            let caddr = CString::new("sim://destroy-accept:0").unwrap();
            let tref = &*t;
            let ops = &*tref.ops;
            assert_eq!(ops.listen.unwrap()(tref.r#impl, caddr.as_ptr()), 0);

            let state = shell_state(tref.r#impl).expect("transport must be live");
            let t_addr = t as usize;
            let accept_handle = thread::spawn(move || {
                let t_local = t_addr as *mut HewTransport;
                #[allow(unused_unsafe, reason = "explicit unsafe block at FFI call site")]
                // SAFETY: `t_local` points to a leaked TransportShell.
                unsafe {
                    let tref = &*t_local;
                    let ops = &*tref.ops;
                    ops.accept.unwrap()(tref.r#impl, -1)
                }
            });

            assert!(state.wait_until_accept_parked());
            drop(state);

            sim_transport_free(t);
            assert_eq!(
                accept_handle.join().expect("accept thread panicked"),
                HEW_CONN_INVALID
            );
        }
    }
}
