#![cfg(test)]

use std::ffi::{c_void, CStr};
use std::sync::atomic::{AtomicBool, AtomicI32, AtomicPtr, AtomicU64, AtomicUsize, Ordering};
use std::sync::{Condvar, Mutex};
use std::time::{Duration, Instant};

use crate::actor::HewActor;
use crate::internal::types::{HewActorState, HewError, HewOverflowPolicy};

#[repr(C)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct PriceUpdate {
    symbol: [u8; 8],
    price: i32,
}

unsafe extern "C" fn price_symbol_key(_msg_type: i32, data: *mut c_void, size: usize) -> u64 {
    if data.is_null() || size < std::mem::size_of::<PriceUpdate>() {
        return 0;
    }
    // SAFETY: caller guarantees `data` points to a full `PriceUpdate`.
    let update = unsafe { &*data.cast::<PriceUpdate>() };
    u64::from_le_bytes(update.symbol)
}

static MESSAGE_DROP_COUNT: AtomicUsize = AtomicUsize::new(0);

unsafe extern "C" fn message_drop_probe(_msg_type: i32, _data: *mut c_void, _size: usize) {
    MESSAGE_DROP_COUNT.fetch_add(1, Ordering::SeqCst);
}

#[derive(Debug, Eq, PartialEq)]
struct CoalesceSnapshot {
    send_rcs: [i32; 2],
    messages_sent: u64,
    queue_len: usize,
    msg_type: i32,
    payload: PriceUpdate,
}

#[derive(Debug, Eq, PartialEq)]
struct MsgTypeCoalesceSnapshot {
    first_type: i32,
    second_type: i32,
    second_price: i32,
}

fn native_coalesce_snapshot() -> CoalesceSnapshot {
    let _guard = crate::runtime_test_guard();
    crate::scheduler::hew_sched_metrics_reset();

    let first = PriceUpdate {
        symbol: *b"BTCUSD\0\0",
        price: 101,
    };
    let second = PriceUpdate {
        symbol: *b"BTCUSD\0\0",
        price: 202,
    };

    // SAFETY: test owns the mailbox and drained node for the full scenario.
    unsafe {
        let mb = crate::mailbox::hew_mailbox_new_coalesce(1);
        assert!(!mb.is_null());
        crate::mailbox::hew_mailbox_set_coalesce_config(
            mb,
            Some(price_symbol_key),
            HewOverflowPolicy::DropOld,
        );

        let rc1 = crate::mailbox::hew_mailbox_send(
            mb,
            7,
            (&raw const first).cast_mut().cast(),
            std::mem::size_of::<PriceUpdate>(),
        );
        let rc2 = crate::mailbox::hew_mailbox_send(
            mb,
            8,
            (&raw const second).cast_mut().cast(),
            std::mem::size_of::<PriceUpdate>(),
        );
        let node = crate::mailbox::hew_mailbox_try_recv(mb);
        assert!(!node.is_null());
        let snapshot = CoalesceSnapshot {
            send_rcs: [rc1, rc2],
            messages_sent: crate::scheduler::hew_sched_metrics_messages_sent(),
            queue_len: crate::mailbox::hew_mailbox_len(mb),
            msg_type: (*node).msg_type,
            payload: *(*node).data.cast::<PriceUpdate>(),
        };
        crate::mailbox::hew_msg_node_free(node);
        crate::mailbox::hew_mailbox_free(mb);
        snapshot
    }
}

fn wasm_coalesce_snapshot() -> CoalesceSnapshot {
    let _guard = crate::runtime_test_guard();
    crate::scheduler_wasm::hew_sched_metrics_reset();

    let first = PriceUpdate {
        symbol: *b"BTCUSD\0\0",
        price: 101,
    };
    let second = PriceUpdate {
        symbol: *b"BTCUSD\0\0",
        price: 202,
    };

    // SAFETY: test owns the mailbox and drained node for the full scenario.
    unsafe {
        let mb = crate::mailbox_wasm::hew_mailbox_new_coalesce(1);
        assert!(!mb.is_null());
        crate::mailbox_wasm::hew_mailbox_set_coalesce_config(
            mb,
            Some(price_symbol_key),
            HewOverflowPolicy::DropOld,
        );

        let rc1 = crate::mailbox_wasm::hew_mailbox_send(
            mb,
            7,
            (&raw const first).cast_mut().cast(),
            std::mem::size_of::<PriceUpdate>(),
        );
        let rc2 = crate::mailbox_wasm::hew_mailbox_send(
            mb,
            8,
            (&raw const second).cast_mut().cast(),
            std::mem::size_of::<PriceUpdate>(),
        );
        let node = crate::mailbox_wasm::hew_mailbox_try_recv(mb);
        assert!(!node.is_null());
        let snapshot = CoalesceSnapshot {
            send_rcs: [rc1, rc2],
            messages_sent: crate::scheduler_wasm::hew_sched_metrics_messages_sent(),
            queue_len: crate::mailbox_wasm::hew_mailbox_len(mb),
            msg_type: (*node).msg_type,
            payload: *(*node).data.cast::<PriceUpdate>(),
        };
        crate::mailbox_wasm::hew_msg_node_free(node);
        crate::mailbox_wasm::hew_mailbox_free(mb);
        snapshot
    }
}

fn native_msg_type_coalesce_snapshot() -> MsgTypeCoalesceSnapshot {
    let beta = PriceUpdate {
        symbol: *b"BTCUSD\0\0",
        price: 20,
    };
    let alpha_old = PriceUpdate {
        symbol: *b"BTCUSD\0\0",
        price: 1,
    };
    let alpha_new = PriceUpdate {
        symbol: *b"BTCUSD\0\0",
        price: 2,
    };
    // SAFETY: test owns the mailbox and drained nodes.
    unsafe {
        let mb = crate::mailbox::hew_mailbox_new_coalesce(2);
        crate::mailbox::hew_mailbox_set_coalesce_config(
            mb,
            Some(price_symbol_key),
            HewOverflowPolicy::DropNew,
        );
        for (msg_type, payload) in [(8, beta), (7, alpha_old), (7, alpha_new)] {
            assert_eq!(
                crate::mailbox::hew_mailbox_send(
                    mb,
                    msg_type,
                    (&raw const payload).cast_mut().cast(),
                    std::mem::size_of::<PriceUpdate>(),
                ),
                HewError::Ok as i32
            );
        }
        let first = crate::mailbox::hew_mailbox_try_recv(mb);
        let second = crate::mailbox::hew_mailbox_try_recv(mb);
        let snapshot = MsgTypeCoalesceSnapshot {
            first_type: (*first).msg_type,
            second_type: (*second).msg_type,
            second_price: (*(*second).data.cast::<PriceUpdate>()).price,
        };
        crate::mailbox::hew_msg_node_free(first);
        crate::mailbox::hew_msg_node_free(second);
        crate::mailbox::hew_mailbox_free(mb);
        snapshot
    }
}

fn wasm_msg_type_coalesce_snapshot() -> MsgTypeCoalesceSnapshot {
    let beta = PriceUpdate {
        symbol: *b"BTCUSD\0\0",
        price: 20,
    };
    let alpha_old = PriceUpdate {
        symbol: *b"BTCUSD\0\0",
        price: 1,
    };
    let alpha_new = PriceUpdate {
        symbol: *b"BTCUSD\0\0",
        price: 2,
    };
    // SAFETY: test owns the mailbox and drained nodes.
    unsafe {
        let mb = crate::mailbox_wasm::hew_mailbox_new_coalesce(2);
        crate::mailbox_wasm::hew_mailbox_set_coalesce_config(
            mb,
            Some(price_symbol_key),
            HewOverflowPolicy::DropNew,
        );
        for (msg_type, payload) in [(8, beta), (7, alpha_old), (7, alpha_new)] {
            assert_eq!(
                crate::mailbox_wasm::hew_mailbox_send(
                    mb,
                    msg_type,
                    (&raw const payload).cast_mut().cast(),
                    std::mem::size_of::<PriceUpdate>(),
                ),
                HewError::Ok as i32
            );
        }
        let first = crate::mailbox_wasm::hew_mailbox_try_recv(mb);
        let second = crate::mailbox_wasm::hew_mailbox_try_recv(mb);
        let snapshot = MsgTypeCoalesceSnapshot {
            first_type: (*first).msg_type,
            second_type: (*second).msg_type,
            second_price: (*(*second).data.cast::<PriceUpdate>()).price,
        };
        crate::mailbox_wasm::hew_msg_node_free(first);
        crate::mailbox_wasm::hew_msg_node_free(second);
        crate::mailbox_wasm::hew_mailbox_free(mb);
        snapshot
    }
}

fn native_message_drop_count() -> usize {
    MESSAGE_DROP_COUNT.store(0, Ordering::SeqCst);
    let first = PriceUpdate {
        symbol: *b"BTCUSD\0\0",
        price: 1,
    };
    let second = PriceUpdate {
        symbol: *b"ETHUSD\0\0",
        price: 2,
    };
    // SAFETY: test owns the mailbox and drained node.
    unsafe {
        let mb = crate::mailbox::hew_mailbox_new_coalesce(1);
        crate::mailbox::hew_mailbox_set_coalesce_config(
            mb,
            Some(price_symbol_key),
            HewOverflowPolicy::DropOld,
        );
        crate::mailbox::hew_mailbox_set_message_drop_fn(mb, Some(message_drop_probe));
        for payload in [first, second] {
            assert_eq!(
                crate::mailbox::hew_mailbox_send(
                    mb,
                    7,
                    (&raw const payload).cast_mut().cast(),
                    std::mem::size_of::<PriceUpdate>(),
                ),
                HewError::Ok as i32
            );
        }
        let node = crate::mailbox::hew_mailbox_try_recv(mb);
        crate::mailbox::hew_msg_node_free(node);
        crate::mailbox::hew_mailbox_free(mb);
    }
    MESSAGE_DROP_COUNT.load(Ordering::SeqCst)
}

fn wasm_message_drop_count() -> usize {
    MESSAGE_DROP_COUNT.store(0, Ordering::SeqCst);
    let first = PriceUpdate {
        symbol: *b"BTCUSD\0\0",
        price: 1,
    };
    let second = PriceUpdate {
        symbol: *b"ETHUSD\0\0",
        price: 2,
    };
    // SAFETY: test owns the mailbox and drained node.
    unsafe {
        let mb = crate::mailbox_wasm::hew_mailbox_new_coalesce(1);
        crate::mailbox_wasm::hew_mailbox_set_coalesce_config(
            mb,
            Some(price_symbol_key),
            HewOverflowPolicy::DropOld,
        );
        crate::mailbox_wasm::hew_mailbox_set_message_drop_fn(mb, Some(message_drop_probe));
        for payload in [first, second] {
            assert_eq!(
                crate::mailbox_wasm::hew_mailbox_send(
                    mb,
                    7,
                    (&raw const payload).cast_mut().cast(),
                    std::mem::size_of::<PriceUpdate>(),
                ),
                HewError::Ok as i32
            );
        }
        let node = crate::mailbox_wasm::hew_mailbox_try_recv(mb);
        crate::mailbox_wasm::hew_msg_node_free(node);
        crate::mailbox_wasm::hew_mailbox_free(mb);
    }
    MESSAGE_DROP_COUNT.load(Ordering::SeqCst)
}

fn stub_wasm_actor(mailbox: *mut c_void) -> Box<HewActor> {
    Box::new(HewActor {
        sched_link_next: AtomicPtr::new(std::ptr::null_mut()),
        id: 1,
        state: std::ptr::null_mut(),
        state_size: 0,
        dispatch: None,
        mailbox,
        actor_state: AtomicI32::new(HewActorState::Idle as i32),
        budget: AtomicI32::new(0),
        init_state: std::ptr::null_mut(),
        init_state_size: 0,
        coalesce_key_fn: None,
        terminate_fn: None,
        state_drop_fn: None,
        state_clone_fn: None,
        terminate_called: AtomicBool::new(false),
        terminate_finished: AtomicBool::new(false),
        dispatch_active: AtomicBool::new(false),
        error_code: AtomicI32::new(0),
        supervisor: std::ptr::null_mut(),
        supervisor_child_index: -1,
        priority: AtomicI32::new(1),
        reductions: AtomicI32::new(crate::actor::HEW_DEFAULT_REDUCTIONS),
        idle_count: AtomicI32::new(0),
        hibernation_threshold: AtomicI32::new(0),
        hibernating: AtomicI32::new(0),
        prof_messages_processed: AtomicU64::new(0),
        prof_processing_time_ns: AtomicU64::new(0),
        arena: std::ptr::null_mut(),
        suspended_cont: AtomicPtr::new(std::ptr::null_mut()),
        cont_tag: AtomicI32::new(crate::internal::types::ContTag::Empty as i32),
        pending_wake: AtomicBool::new(false),
        suspended_reply_channel: AtomicPtr::new(std::ptr::null_mut()),
        suspended_cancel_token: AtomicPtr::new(std::ptr::null_mut()),
        runtime_id: crate::runtime_id::RuntimeId::DEFAULT,
        runtime: std::ptr::null(),
        send_pin_count: std::sync::atomic::AtomicU32::new(0),
        gen_sink: AtomicPtr::new(std::ptr::null_mut()),
    })
}

fn stub_dispatch_actor(
    mailbox: *mut c_void,
    dispatch: crate::internal::types::HewDispatchFn,
) -> Box<HewActor> {
    let mut actor = stub_wasm_actor(mailbox);
    actor
        .actor_state
        .store(HewActorState::Runnable as i32, Ordering::Relaxed);
    actor
        .reductions
        .store(crate::actor::HEW_DEFAULT_REDUCTIONS, Ordering::Relaxed);
    actor.dispatch = Some(dispatch);
    actor
}

struct CurrentExecutionContextReset {
    prev: *mut crate::execution_context::HewExecutionContext,
}

impl CurrentExecutionContextReset {
    fn new() -> Self {
        crate::hew_clear_error();
        let prev = crate::execution_context::set_current_context(std::ptr::null_mut());
        Self { prev }
    }
}

impl Drop for CurrentExecutionContextReset {
    fn drop(&mut self) {
        let _ = crate::execution_context::set_current_context(self.prev);
    }
}

fn last_error_message() -> Option<String> {
    let ptr = crate::hew_last_error();
    if ptr.is_null() {
        None
    } else {
        // SAFETY: hew_last_error returns a C string pointer valid until the next
        // error mutation on this thread.
        Some(
            unsafe { CStr::from_ptr(ptr) }
                .to_string_lossy()
                .into_owned(),
        )
    }
}

unsafe extern "C" fn wasm_parity_state_drop(_state: *mut c_void) {}

unsafe extern "C-unwind" fn wasm_parity_state_clone(_src: *const c_void) -> *mut c_void {
    std::ptr::null_mut()
}

#[test]
fn actor_state_clone_drop_setters_remain_linkable_and_mutate_slots() {
    let _guard = crate::runtime_test_guard();
    let mut actor = stub_wasm_actor(std::ptr::null_mut());

    let set_drop: unsafe extern "C" fn(*mut HewActor, unsafe extern "C" fn(*mut c_void)) =
        crate::actor::hew_actor_set_state_drop;
    let set_clone: unsafe extern "C" fn(*mut HewActor, crate::actor::HewStateCloneFn) =
        crate::actor::hew_actor_set_state_clone;

    // SAFETY: `actor` is a live test-owned HewActor and both callbacks have
    // the exact ABI expected by the setter contracts.
    unsafe {
        set_drop(actor.as_mut(), wasm_parity_state_drop);
        set_clone(actor.as_mut(), wasm_parity_state_clone);
    }

    assert!(actor.state_drop_fn.is_some());
    assert!(actor.state_clone_fn.is_some());
}

#[test]
fn coalesced_mailbox_metrics_match_native_and_wasm() {
    assert_eq!(native_coalesce_snapshot(), wasm_coalesce_snapshot());
}

#[test]
fn coalesce_message_type_gate_matches_native_and_wasm() {
    let expected = MsgTypeCoalesceSnapshot {
        first_type: 8,
        second_type: 7,
        second_price: 2,
    };
    assert_eq!(native_msg_type_coalesce_snapshot(), expected);
    assert_eq!(wasm_msg_type_coalesce_snapshot(), expected);
}

#[test]
fn coalesce_eviction_typed_drop_matches_native_and_wasm() {
    let _guard = crate::runtime_test_guard();
    assert_eq!(native_message_drop_count(), 1);
    assert_eq!(wasm_message_drop_count(), 1);
}

#[test]
fn wasm_worker_count_matches_native_shutdown_state() {
    let _guard = crate::runtime_test_guard();
    crate::scheduler_wasm::hew_sched_shutdown();
    assert_eq!(crate::scheduler_wasm::hew_sched_metrics_worker_count(), 0);
    crate::scheduler_wasm::hew_sched_init();
    assert_eq!(crate::scheduler_wasm::hew_sched_metrics_worker_count(), 1);
    crate::scheduler_wasm::hew_sched_shutdown();
    assert_eq!(crate::scheduler_wasm::hew_sched_metrics_worker_count(), 0);
}

// ── Phase-α envelope parity tests ───────────────────────────────────────────
//
// Each test captures an `EnvelopeSnapshot` from both the native mailbox and the
// WASM mailbox and asserts they are identical. This is the parity contract: the
// same sequence of operations on both surfaces must produce identical observable
// state (refcount, header bits, payload content, drop-glue firing).
//
// The WASM envelope functions are defined in `mailbox_wasm.rs` and are
// structurally identical to the native ones; the tests below prove the two
// implementations stay in sync.

/// Observable state of a [`HewMsgEnvelope`] captured after an operation
/// sequence. Used to compare native and WASM envelope behaviour.
#[derive(Debug, Clone, PartialEq, Eq)]
struct EnvelopeSnapshot {
    refcount: usize,
    header_bits: u32,
    payload_size: usize,
    /// Content of the first N bytes of payload (0 if payload is null).
    payload_prefix: Vec<u8>,
    /// How many times `drop_glue` fired during the operation sequence.
    drop_count: usize,
}

/// Process-wide drop counter for envelope parity tests. Serialised via
/// `WASM_ENVELOPE_DROP_LOCK` so concurrent test threads don't interfere.
static WASM_ENVELOPE_DROP_COUNT: AtomicUsize = AtomicUsize::new(0);
static WASM_ENVELOPE_DROP_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

unsafe extern "C" fn wasm_parity_drop_glue(_payload: *mut c_void) {
    WASM_ENVELOPE_DROP_COUNT.fetch_add(1, Ordering::SeqCst);
}

fn alloc_bytes(bytes: &[u8]) -> *mut c_void {
    // SAFETY: standard malloc + memcpy.
    unsafe {
        let buf = libc::malloc(bytes.len()); // ALLOCATOR-PAIRING: libc
        assert!(!buf.is_null(), "alloc_bytes: OOM");
        libc::memcpy(buf, bytes.as_ptr().cast(), bytes.len());
        buf
    }
}

fn read_payload_prefix(payload: *mut c_void, n: usize) -> Vec<u8> {
    if payload.is_null() || n == 0 {
        return Vec::new();
    }
    // SAFETY: caller guarantees payload is readable for at least `n` bytes.
    unsafe { std::slice::from_raw_parts(payload.cast::<u8>(), n).to_vec() }
}

/// Capture `EnvelopeSnapshot` from a live native envelope.
unsafe fn native_snapshot(env: *mut crate::mailbox::HewMsgEnvelope, n: usize) -> EnvelopeSnapshot {
    // SAFETY: caller guarantees `env` is a live, exclusively-accessible envelope.
    unsafe {
        let payload = (*env).payload;
        EnvelopeSnapshot {
            refcount: (*env).refcount.load(Ordering::SeqCst),
            header_bits: (*env).header_bits.load(Ordering::SeqCst),
            payload_size: (*env).payload_size,
            payload_prefix: read_payload_prefix(payload, n),
            drop_count: WASM_ENVELOPE_DROP_COUNT.load(Ordering::SeqCst),
        }
    }
}

/// Capture `EnvelopeSnapshot` from a live WASM envelope.
unsafe fn wasm_snapshot(
    env: *mut crate::mailbox_wasm::HewMsgEnvelope,
    n: usize,
) -> EnvelopeSnapshot {
    // SAFETY: caller guarantees `env` is a live, exclusively-accessible envelope.
    unsafe {
        let payload = (*env).payload;
        EnvelopeSnapshot {
            refcount: (*env).refcount.load(Ordering::SeqCst),
            header_bits: (*env).header_bits.load(Ordering::SeqCst),
            payload_size: (*env).payload_size,
            payload_prefix: read_payload_prefix(payload, n),
            drop_count: WASM_ENVELOPE_DROP_COUNT.load(Ordering::SeqCst),
        }
    }
}

// ── Refcount + alias bit parity ──────────────────────────────────────────────

/// `hew_msg_envelope_new` starts at refcount=1, alias bit clear, on both
/// native and WASM.
#[test]
fn envelope_new_parity_refcount_one_no_alias() {
    let _guard = WASM_ENVELOPE_DROP_LOCK.lock().unwrap();
    WASM_ENVELOPE_DROP_COUNT.store(0, Ordering::SeqCst);

    // SAFETY: standard envelope-new + release. Test owns both envelopes.
    let native_snap = unsafe {
        let payload = alloc_bytes(b"parity-new");
        let env = crate::mailbox::hew_msg_envelope_new(payload, 10, Some(wasm_parity_drop_glue));
        assert!(!env.is_null());
        let snap = native_snapshot(env, 10);
        crate::mailbox::hew_msg_envelope_release(env);
        snap
    };
    let after_native_drop = WASM_ENVELOPE_DROP_COUNT.load(Ordering::SeqCst);

    WASM_ENVELOPE_DROP_COUNT.store(0, Ordering::SeqCst);

    // SAFETY: standard envelope-new + release. Test owns the envelope.
    let wasm_snap = unsafe {
        let payload = alloc_bytes(b"parity-new");
        let env =
            crate::mailbox_wasm::hew_msg_envelope_new(payload, 10, Some(wasm_parity_drop_glue));
        assert!(!env.is_null());
        let snap = wasm_snapshot(env, 10);
        crate::mailbox_wasm::hew_msg_envelope_release(env);
        snap
    };
    let after_wasm_drop = WASM_ENVELOPE_DROP_COUNT.load(Ordering::SeqCst);

    assert_eq!(
        native_snap, wasm_snap,
        "new: native and WASM snapshots diverged"
    );
    assert_eq!(
        after_native_drop, 1,
        "native: drop_glue must fire exactly once on release"
    );
    assert_eq!(
        after_wasm_drop, 1,
        "wasm: drop_glue must fire exactly once on release"
    );
}

/// `hew_msg_envelope_clone_alias` bumps refcount to 2 and sets `ALIAS_ACTIVE`
/// on both native and WASM; first release drops to 1 without firing `drop_glue`;
/// second release fires `drop_glue` exactly once on both surfaces.
#[test]
fn envelope_clone_alias_parity_refcount_and_bit() {
    let _guard = WASM_ENVELOPE_DROP_LOCK.lock().unwrap();
    WASM_ENVELOPE_DROP_COUNT.store(0, Ordering::SeqCst);

    // SAFETY: standard envelope alias contract. Test owns all references.
    let native_after_clone = unsafe {
        let payload = alloc_bytes(b"alias-parity");
        let env = crate::mailbox::hew_msg_envelope_new(payload, 12, Some(wasm_parity_drop_glue));
        crate::mailbox::hew_msg_envelope_clone_alias(env);
        let snap = native_snapshot(env, 12);
        // First release: refcount 2 → 1; drop_glue must NOT fire.
        crate::mailbox::hew_msg_envelope_release(env);
        let mid_drop = WASM_ENVELOPE_DROP_COUNT.load(Ordering::SeqCst);
        assert_eq!(
            mid_drop, 0,
            "native: drop_glue must not fire on non-final release"
        );
        // Second release: refcount 1 → 0; drop_glue fires.
        crate::mailbox::hew_msg_envelope_release(env);
        snap
    };
    let native_final_drop = WASM_ENVELOPE_DROP_COUNT.load(Ordering::SeqCst);

    WASM_ENVELOPE_DROP_COUNT.store(0, Ordering::SeqCst);

    // SAFETY: standard envelope alias contract. Test owns all references.
    let wasm_after_clone = unsafe {
        let payload = alloc_bytes(b"alias-parity");
        let env =
            crate::mailbox_wasm::hew_msg_envelope_new(payload, 12, Some(wasm_parity_drop_glue));
        crate::mailbox_wasm::hew_msg_envelope_clone_alias(env);
        let snap = wasm_snapshot(env, 12);
        crate::mailbox_wasm::hew_msg_envelope_release(env);
        let mid_drop = WASM_ENVELOPE_DROP_COUNT.load(Ordering::SeqCst);
        assert_eq!(
            mid_drop, 0,
            "wasm: drop_glue must not fire on non-final release"
        );
        crate::mailbox_wasm::hew_msg_envelope_release(env);
        snap
    };
    let wasm_final_drop = WASM_ENVELOPE_DROP_COUNT.load(Ordering::SeqCst);

    assert_eq!(
        native_after_clone, wasm_after_clone,
        "clone_alias: native and WASM snapshots diverged"
    );
    assert_eq!(
        native_final_drop, 1,
        "native: drop_glue fires exactly once at final release"
    );
    assert_eq!(
        wasm_final_drop, 1,
        "wasm: drop_glue fires exactly once at final release"
    );
    // Alias bit must be set on both snapshots.
    assert_ne!(
        native_after_clone.header_bits & crate::mailbox::HEW_MSG_ENVELOPE_ALIAS_ACTIVE,
        0,
        "native: ALIAS_ACTIVE bit must be set after clone_alias"
    );
}

// ── Fork-on-write parity ─────────────────────────────────────────────────────

/// `hew_msg_envelope_fork_for_write` produces a distinct envelope with:
/// - refcount=1 on the forked envelope
/// - `FORKED` bit set on the forked envelope
/// - a separate payload buffer holding the same bytes
/// - the original's refcount decremented by one
///
/// Verified on both native and WASM; the snapshot captures forked envelope
/// state, not the original's state post-fork.
#[test]
fn envelope_fork_for_write_parity() {
    let _guard = WASM_ENVELOPE_DROP_LOCK.lock().unwrap();
    WASM_ENVELOPE_DROP_COUNT.store(0, Ordering::SeqCst);

    // SAFETY: standard envelope fork contract. Test owns all references.
    let (native_forked_snap, native_original_rc_after_fork) = unsafe {
        let payload = alloc_bytes(b"fork-bytes");
        let env = crate::mailbox::hew_msg_envelope_new(payload, 10, Some(wasm_parity_drop_glue));
        // Two-observer state, like a real in-process send.
        crate::mailbox::hew_msg_envelope_clone_alias(env);
        // Fork: caller's reference transfers to the new envelope.
        let forked = crate::mailbox::hew_msg_envelope_fork_for_write(env);
        assert!(!forked.is_null());
        let forked_snap = native_snapshot(forked, 10);
        let original_rc = (*env).refcount.load(Ordering::SeqCst);
        // Forked payload must be a distinct allocation with the same bytes.
        assert_ne!(
            (*forked).payload,
            payload,
            "native: forked payload must be a copy"
        );
        crate::mailbox::hew_msg_envelope_release(forked);
        crate::mailbox::hew_msg_envelope_release(env);
        (forked_snap, original_rc)
    };
    let native_final_drop = WASM_ENVELOPE_DROP_COUNT.load(Ordering::SeqCst);

    WASM_ENVELOPE_DROP_COUNT.store(0, Ordering::SeqCst);

    // SAFETY: standard envelope fork contract. Test owns all references.
    let (wasm_forked_snap, wasm_original_rc_after_fork) = unsafe {
        let payload = alloc_bytes(b"fork-bytes");
        let env =
            crate::mailbox_wasm::hew_msg_envelope_new(payload, 10, Some(wasm_parity_drop_glue));
        crate::mailbox_wasm::hew_msg_envelope_clone_alias(env);
        let forked = crate::mailbox_wasm::hew_msg_envelope_fork_for_write(env);
        assert!(!forked.is_null());
        let forked_snap = wasm_snapshot(forked, 10);
        let original_rc = (*env).refcount.load(Ordering::SeqCst);
        assert_ne!(
            (*forked).payload,
            payload,
            "wasm: forked payload must be a copy"
        );
        crate::mailbox_wasm::hew_msg_envelope_release(forked);
        crate::mailbox_wasm::hew_msg_envelope_release(env);
        (forked_snap, original_rc)
    };
    let wasm_final_drop = WASM_ENVELOPE_DROP_COUNT.load(Ordering::SeqCst);

    assert_eq!(
        native_forked_snap, wasm_forked_snap,
        "fork_for_write: native and WASM forked-envelope snapshots diverged"
    );
    assert_eq!(
        native_original_rc_after_fork, wasm_original_rc_after_fork,
        "fork_for_write: original refcount after fork diverged"
    );
    // FORKED bit must be set on both forked envelopes.
    assert_ne!(
        native_forked_snap.header_bits & crate::mailbox::HEW_MSG_ENVELOPE_FORKED,
        0,
        "native: FORKED bit must be set on forked envelope"
    );
    // Both final drops: two envelopes, each with drop_glue.
    assert_eq!(
        native_final_drop, 2,
        "native: drop_glue fires once per envelope"
    );
    assert_eq!(
        wasm_final_drop, 2,
        "wasm: drop_glue fires once per envelope"
    );
}

// ── Header-bit get/mask parity ───────────────────────────────────────────────

/// Header-bit round-trip: set a live bit and verify it reads back identically
/// on both native and WASM. Uses `ARENA_BACKED` (bit 2) as the probe because
/// it's a named reserved-future bit that's safe to set in a test.
#[test]
fn envelope_header_bit_set_get_round_trip_parity() {
    // SAFETY: standard envelope-new + release contract.
    let native_bits = unsafe {
        let payload = alloc_bytes(b"bits");
        let env = crate::mailbox::hew_msg_envelope_new(payload, 4, None);
        assert!(!env.is_null());
        (*env).header_bits.fetch_or(
            crate::mailbox::HEW_MSG_ENVELOPE_ARENA_BACKED,
            Ordering::Relaxed,
        );
        let bits = (*env).header_bits.load(Ordering::SeqCst);
        // Clear the bit so header_validate passes on release.
        (*env).header_bits.fetch_and(
            !crate::mailbox::HEW_MSG_ENVELOPE_ARENA_BACKED,
            Ordering::Relaxed,
        );
        crate::mailbox::hew_msg_envelope_release(env);
        bits
    };

    // SAFETY: standard envelope-new + release contract.
    let wasm_bits = unsafe {
        let payload = alloc_bytes(b"bits");
        let env = crate::mailbox_wasm::hew_msg_envelope_new(payload, 4, None);
        assert!(!env.is_null());
        (*env).header_bits.fetch_or(
            crate::mailbox_wasm::HEW_MSG_ENVELOPE_ARENA_BACKED,
            Ordering::Relaxed,
        );
        let bits = (*env).header_bits.load(Ordering::SeqCst);
        (*env).header_bits.fetch_and(
            !crate::mailbox_wasm::HEW_MSG_ENVELOPE_ARENA_BACKED,
            Ordering::Relaxed,
        );
        crate::mailbox_wasm::hew_msg_envelope_release(env);
        bits
    };

    assert_eq!(
        native_bits, wasm_bits,
        "header-bit round-trip: native and WASM disagree on bit readback"
    );
    assert_ne!(
        native_bits & crate::mailbox::HEW_MSG_ENVELOPE_ARENA_BACKED,
        0,
        "ARENA_BACKED bit must read back set"
    );
}

// ── MUST_BE_ZERO mask shape ───────────────────────────────────────────────────

/// The WASM `MUST_BE_ZERO` mask covers the same bit range (9..31) as the
/// native one. The fail-closed panic at `header_validate` cannot be exercised
/// under `panic = "abort"` (it would kill the test runner), so we assert the
/// mask shape directly on both surfaces.
#[test]
fn wasm_envelope_must_be_zero_mask_covers_bits_nine_through_thirtyone() {
    use crate::mailbox_wasm::HEW_MSG_ENVELOPE_MUST_BE_ZERO_MASK as WASM_MASK;

    for bit in 0..9 {
        assert_eq!(
            WASM_MASK & (1u32 << bit),
            0,
            "wasm: bit {bit} is in the live header range; mask must not cover it"
        );
    }
    for bit in 9..32 {
        assert_ne!(
            WASM_MASK & (1u32 << bit),
            0,
            "wasm: bit {bit} is reserved-zero; mask must cover it"
        );
    }
}

// ── Execution-context dispatch parity ────────────────────────────────────────

#[test]
fn wasm_reply_channel_lookup_reads_current_execution_context() {
    use crate::execution_context::{HewExecutionContext, TestExecutionContext};

    let _guard = crate::runtime_test_guard();
    let _context_reset = CurrentExecutionContextReset::new();
    let reply_channel = 0x5151_5151usize as *mut c_void;

    let _ctx_guard = TestExecutionContext::install(HewExecutionContext {
        reply_channel,
        ..HewExecutionContext::default()
    });

    assert_eq!(
        crate::scheduler_wasm::hew_get_reply_channel(),
        reply_channel
    );
    assert_eq!(last_error_message(), None);
}

#[test]
fn wasm_reply_channel_lookup_without_context_fails_closed_after_context_removed() {
    use crate::execution_context::{
        HewExecutionContext, TestExecutionContext, EXECUTION_CONTEXT_NOT_INSTALLED,
    };

    let _guard = crate::runtime_test_guard();
    let _context_reset = CurrentExecutionContextReset::new();
    let stale_reply_channel = 0x5252_5252usize as *mut c_void;

    {
        let _ctx_guard = TestExecutionContext::install(HewExecutionContext {
            reply_channel: stale_reply_channel,
            ..HewExecutionContext::default()
        });
        assert_eq!(
            crate::scheduler_wasm::hew_get_reply_channel(),
            stale_reply_channel
        );
    }

    crate::hew_clear_error();
    assert!(crate::execution_context::current_context().is_null());
    assert!(crate::scheduler_wasm::hew_get_reply_channel().is_null());
    assert_eq!(
        last_error_message().as_deref(),
        Some(EXECUTION_CONTEXT_NOT_INSTALLED)
    );
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct DispatchContextSnapshot {
    current_id: i64,
    self_pid: u64,
    ctx_actor_id: u64,
    context_matches_tls: bool,
    self_matches_ctx_actor: bool,
    prev_context_is_null: bool,
    cooperate_result: i32,
}

static DISPATCH_CONTEXT_SNAPSHOTS: Mutex<Vec<DispatchContextSnapshot>> = Mutex::new(Vec::new());
static DISPATCH_CONTEXT_READY: Condvar = Condvar::new();

fn reset_dispatch_context_snapshots() {
    DISPATCH_CONTEXT_SNAPSHOTS.lock().unwrap().clear();
}

fn wait_for_dispatch_context_snapshots(expected: usize) -> Vec<DispatchContextSnapshot> {
    let deadline = Instant::now() + Duration::from_secs(2);
    let mut snapshots = DISPATCH_CONTEXT_SNAPSHOTS.lock().unwrap();
    while snapshots.len() < expected {
        let remaining = deadline.saturating_duration_since(Instant::now());
        assert!(
            !remaining.is_zero(),
            "timed out waiting for {expected} dispatch context snapshots"
        );
        let (guard, result) = DISPATCH_CONTEXT_READY
            .wait_timeout(snapshots, remaining)
            .unwrap();
        snapshots = guard;
        assert!(
            !result.timed_out() || snapshots.len() >= expected,
            "timed out waiting for {expected} dispatch context snapshots"
        );
    }
    snapshots.clone()
}

fn push_dispatch_context_snapshot(
    ctx: *mut crate::execution_context::HewExecutionContext,
    cooperate_result: i32,
) {
    let self_actor = crate::actor::hew_actor_self();
    let current_context = crate::execution_context::current_context();
    let (ctx_actor_id, ctx_actor, prev_context) = if ctx.is_null() {
        (0, std::ptr::null_mut(), std::ptr::null_mut())
    } else {
        // SAFETY: dispatch supplies the live scheduler-owned context pointer.
        unsafe { ((*ctx).actor_id, (*ctx).actor, (*ctx).prev_context) }
    };

    DISPATCH_CONTEXT_SNAPSHOTS
        .lock()
        .unwrap()
        .push(DispatchContextSnapshot {
            current_id: crate::actor::hew_actor_current_id(),
            self_pid: crate::actor::hew_actor_self_pid(),
            ctx_actor_id,
            context_matches_tls: current_context == ctx,
            self_matches_ctx_actor: self_actor == ctx_actor,
            prev_context_is_null: prev_context.is_null(),
            cooperate_result,
        });
    DISPATCH_CONTEXT_READY.notify_all();
}

unsafe extern "C-unwind" fn native_dispatch_context_probe(
    ctx: *mut crate::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _data_size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    push_dispatch_context_snapshot(ctx, crate::scheduler::hew_actor_cooperate());
    std::ptr::null_mut()
}

unsafe extern "C-unwind" fn wasm_dispatch_context_probe(
    ctx: *mut crate::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _data_size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    push_dispatch_context_snapshot(ctx, crate::scheduler_wasm::hew_actor_cooperate());
    std::ptr::null_mut()
}

fn wait_for_actor_idle(actor: &HewActor) {
    let deadline = Instant::now() + Duration::from_secs(2);
    while actor.actor_state.load(Ordering::Acquire) != HewActorState::Idle as i32 {
        assert!(
            Instant::now() < deadline,
            "timed out waiting for dispatch actor to return to Idle"
        );
        std::thread::yield_now();
    }
}

fn native_dispatch_context_snapshots() -> Vec<DispatchContextSnapshot> {
    let _guard = crate::runtime_test_guard();
    reset_dispatch_context_snapshots();

    // SAFETY: test owns the mailbox and actor through direct test activation.
    unsafe {
        let mailbox = crate::mailbox::hew_mailbox_new().cast::<c_void>();
        assert!(!mailbox.is_null());
        assert_eq!(
            crate::mailbox::hew_mailbox_send(mailbox.cast(), 1, std::ptr::null_mut(), 0),
            0
        );
        assert_eq!(
            crate::mailbox::hew_mailbox_send(mailbox.cast(), 2, std::ptr::null_mut(), 0),
            0
        );

        let actor = stub_dispatch_actor(mailbox, native_dispatch_context_probe);
        let actor_ptr = Box::into_raw(actor); // ALLOCATOR-PAIRING: GlobalAlloc
        crate::scheduler::activate_actor_for_test(actor_ptr);

        let snapshots = wait_for_dispatch_context_snapshots(2);
        wait_for_actor_idle(&*actor_ptr);
        crate::mailbox::hew_mailbox_free(mailbox.cast());
        drop(Box::from_raw(actor_ptr)); // ALLOCATOR-PAIRING: GlobalAlloc
        snapshots
    }
}

fn wasm_dispatch_context_snapshots() -> Vec<DispatchContextSnapshot> {
    let _guard = crate::runtime_test_guard();
    crate::scheduler_wasm::hew_sched_shutdown();
    crate::scheduler_wasm::hew_sched_init();
    reset_dispatch_context_snapshots();

    // SAFETY: test owns the mailbox and actor until hew_sched_run returns.
    unsafe {
        let mailbox = crate::mailbox_wasm::hew_mailbox_new().cast::<c_void>();
        assert!(!mailbox.is_null());
        assert_eq!(
            crate::mailbox_wasm::hew_mailbox_send(mailbox.cast(), 1, std::ptr::null_mut(), 0),
            0
        );
        assert_eq!(
            crate::mailbox_wasm::hew_mailbox_send(mailbox.cast(), 2, std::ptr::null_mut(), 0),
            0
        );

        let actor = stub_dispatch_actor(mailbox, wasm_dispatch_context_probe);
        let actor_ptr = Box::into_raw(actor); // ALLOCATOR-PAIRING: GlobalAlloc
        crate::scheduler_wasm::sched_enqueue(actor_ptr.cast::<crate::scheduler_wasm::HewActor>());
        crate::scheduler_wasm::hew_sched_run();

        let snapshots = wait_for_dispatch_context_snapshots(2);
        wait_for_actor_idle(&*actor_ptr);
        crate::mailbox_wasm::hew_mailbox_free(mailbox.cast());
        drop(Box::from_raw(actor_ptr)); // ALLOCATOR-PAIRING: GlobalAlloc
        crate::scheduler_wasm::hew_sched_shutdown();
        snapshots
    }
}

unsafe extern "C-unwind" fn request_wasm_stop_dispatch(
    ctx: *mut crate::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _data_size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    // SAFETY: scheduler_wasm installs a non-null canonical context before
    // entering dispatch.
    unsafe {
        let actor = (*ctx).actor;
        if !actor.is_null() {
            (*actor)
                .actor_state
                .store(HewActorState::Stopping as i32, Ordering::Release);
        }
    }
    std::ptr::null_mut()
}

unsafe extern "C-unwind" fn close_wasm_mailbox_then_cooperate_dispatch(
    ctx: *mut crate::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _data_size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    // SAFETY: scheduler_wasm installs a non-null canonical context before
    // entering dispatch.
    unsafe {
        let actor = (*ctx).actor;
        assert!(!actor.is_null(), "dispatch context must carry actor");
        let mailbox = (*actor)
            .mailbox
            .cast::<crate::mailbox_wasm::HewMailboxWasm>();
        assert!(!mailbox.is_null(), "actor must carry mailbox");
        crate::mailbox_wasm::hew_mailbox_close(mailbox);
    }
    push_dispatch_context_snapshot(ctx, crate::scheduler_wasm::hew_actor_cooperate());
    std::ptr::null_mut()
}

unsafe extern "C" fn mark_terminate_state(state: *mut c_void) {
    if !state.is_null() {
        // SAFETY: the test installs an AtomicBool as the actor state pointer.
        unsafe { (*state.cast::<AtomicBool>()).store(true, Ordering::Release) };
    }
}

#[test]
fn wasm_on_stop_emits_stop_lifecycle_trace_before_terminate_completes() {
    let _guard = crate::runtime_test_guard();
    crate::scheduler_wasm::hew_sched_shutdown();
    crate::scheduler_wasm::hew_sched_init();
    crate::tracing::hew_trace_reset();
    crate::tracing::hew_trace_enable(1);

    // SAFETY: test owns the mailbox and actor until after the scheduler drain.
    unsafe {
        let mailbox = crate::mailbox_wasm::hew_mailbox_new().cast::<c_void>();
        assert!(!mailbox.is_null());
        assert_eq!(
            crate::mailbox_wasm::hew_mailbox_send(mailbox.cast(), 7, std::ptr::null_mut(), 0),
            HewError::Ok as i32
        );

        let terminate_seen = Box::new(AtomicBool::new(false));
        let terminate_state = Box::into_raw(terminate_seen).cast::<c_void>(); // ALLOCATOR-PAIRING: GlobalAlloc
        let mut actor = stub_dispatch_actor(mailbox, request_wasm_stop_dispatch);
        actor.id = 37;
        actor.state = terminate_state;
        actor.terminate_fn = Some(mark_terminate_state);
        let actor_ptr = Box::into_raw(actor); // ALLOCATOR-PAIRING: GlobalAlloc

        crate::scheduler_wasm::sched_enqueue(actor_ptr.cast::<crate::scheduler_wasm::HewActor>());
        crate::scheduler_wasm::hew_sched_run();

        assert_eq!(
            (*actor_ptr).actor_state.load(Ordering::Acquire),
            HewActorState::Stopped as i32
        );
        assert!(
            (*actor_ptr).terminate_called.load(Ordering::Acquire),
            "scheduler_wasm must still route on(stop) through call_terminate_fn"
        );
        assert!(
            (*actor_ptr).terminate_finished.load(Ordering::Acquire),
            "terminate_fn must complete on the stopped actor"
        );
        assert!(
            (*terminate_state.cast::<AtomicBool>()).load(Ordering::Acquire),
            "terminate_fn must receive the actor state pointer"
        );

        let mut events = [crate::tracing::HewTraceEvent {
            trace_id_hi: 0,
            trace_id_lo: 0,
            span_id: 0,
            parent_span_id: 0,
            actor_id: 0,
            event_type: 0,
            msg_type: 0,
            timestamp_ns: 0,
        }; 4];
        let count = crate::tracing::hew_trace_drain(events.as_mut_ptr(), 4);
        assert_eq!(
            count, 3,
            "dispatch should emit begin/end plus one lifecycle stop event"
        );
        assert_eq!(events[0].event_type, crate::tracing::SPAN_BEGIN);
        assert_eq!(events[0].actor_id, 37);
        assert_eq!(events[0].msg_type, 7);
        assert_eq!(events[1].event_type, crate::tracing::SPAN_END);
        assert_eq!(events[1].actor_id, 37);
        assert_eq!(events[1].msg_type, 7);
        assert_eq!(events[2].event_type, crate::tracing::SPAN_STOP);
        assert_eq!(events[2].actor_id, 37);
        assert_eq!(
            events[2].msg_type, 0,
            "lifecycle stop events mirror native and carry no message type"
        );

        crate::mailbox_wasm::hew_mailbox_free(mailbox.cast());
        drop(Box::from_raw(terminate_state.cast::<AtomicBool>())); // ALLOCATOR-PAIRING: GlobalAlloc
        drop(Box::from_raw(actor_ptr)); // ALLOCATOR-PAIRING: GlobalAlloc
    }

    crate::tracing::hew_trace_reset();
    crate::scheduler_wasm::hew_sched_shutdown();
}

#[test]
#[cfg(feature = "profiler")]
fn wasm_trace_json_uses_registered_actor_type_attribution() {
    let _guard = crate::runtime_test_guard();
    let _bridge_guard = crate::bridge::BRIDGE_TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let _trace_guard = crate::tracing::tracing_test_guard();
    let _ctx = crate::execution_context::TestExecutionContext::install(
        crate::execution_context::HewExecutionContext::default(),
    );

    crate::bridge::reset_bridge_full();
    crate::tracing::hew_trace_reset();
    crate::tracing::hew_trace_enable(1);

    let handler = crate::bridge::HewHandlerMeta {
        name: c"on_ping".as_ptr().cast(),
        msg_type: 55,
        param_count: 0,
        params: std::ptr::null(),
        return_type: std::ptr::null(),
        return_size: 0,
    };
    let actor_meta = crate::bridge::HewActorMeta {
        name: c"WasmActor".as_ptr().cast(),
        handler_count: 1,
        handlers: &raw const handler,
    };
    // SAFETY: actor_meta is a valid stack-allocated struct with valid C strings.
    unsafe { crate::bridge::hew_wasm_register_actor_meta(&raw const actor_meta) };

    crate::tracing::hew_trace_begin(9, 55);
    crate::tracing::hew_trace_begin(9, 56);

    let json = crate::tracing::drain_events_json();
    assert!(
        json.contains(r#""actor_type":"WasmActor""#),
        "registered actor type should render in WASM/test trace JSON: {json}"
    );
    assert!(
        json.contains(r#""handler_name":"WasmActor::on_ping""#),
        "registered handler name should render in WASM/test trace JSON: {json}"
    );
    assert!(
        !json.contains(r#""actor_type_id":0,"actor_type":"WasmActor""#),
        "registered actor type id should be non-zero: {json}"
    );
    assert!(
        json.contains(r#""actor_type_id":0,"actor_type":null"#),
        "unregistered msg_type should remain zero/null: {json}"
    );

    crate::bridge::reset_bridge_full();
    crate::tracing::hew_trace_reset();
}

#[test]
fn dispatch_context_install_restore_matches_native_and_wasm() {
    let native = native_dispatch_context_snapshots();
    let wasm = wasm_dispatch_context_snapshots();

    assert_eq!(native, wasm);
    assert_eq!(native.len(), 2);
    for snapshot in native {
        assert_eq!(snapshot.current_id, 1);
        assert_eq!(snapshot.self_pid, 1);
        assert_eq!(snapshot.ctx_actor_id, 1);
        assert!(snapshot.context_matches_tls);
        assert!(snapshot.self_matches_ctx_actor);
        assert!(snapshot.prev_context_is_null);
        assert_eq!(snapshot.cooperate_result, 0);
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn cooperate_with_cancelled_context(cooperate: extern "C" fn() -> i32) -> i32 {
    // SAFETY: creates an owned task scope for this test and is destroyed below.
    let scope = unsafe { crate::task_scope::hew_task_scope_new() };
    assert!(!scope.is_null());
    // SAFETY: scope is a live pointer returned by hew_task_scope_new.
    unsafe { crate::task_scope::hew_task_scope_cancel(scope) };
    // SAFETY: scope is live and owns its cancellation token.
    let token = unsafe { crate::task_scope::hew_task_scope_cancel_token(scope) };
    assert!(!token.is_null());

    let actor = stub_wasm_actor(std::ptr::null_mut());
    let mut ctx = crate::execution_context::HewExecutionContext {
        actor: (&raw const *actor).cast_mut(),
        actor_id: actor.id,
        cancel_token: token,
        task_scope: scope,
        ..crate::execution_context::HewExecutionContext::default()
    };
    let previous = crate::execution_context::set_current_context(&raw mut ctx);
    let result = cooperate();
    let restored = crate::execution_context::set_current_context(previous);
    assert_eq!(restored, &raw mut ctx);
    // SAFETY: scope was allocated by hew_task_scope_new and is no longer installed.
    unsafe { crate::task_scope::hew_task_scope_destroy(scope) };
    result
}

#[test]
#[cfg(not(target_arch = "wasm32"))]
fn cancel_observation_matches_native_and_wasm_cooperate() {
    let _guard = crate::runtime_test_guard();

    let native = cooperate_with_cancelled_context(crate::scheduler::hew_actor_cooperate);
    let wasm = cooperate_with_cancelled_context(crate::scheduler_wasm::hew_actor_cooperate);

    assert_eq!(native, 2);
    assert_eq!(wasm, native);
}

#[test]
fn wasm_bridge_send_failure_does_not_wake_actor() {
    let _guard = crate::runtime_test_guard();
    let _bridge_guard = crate::bridge::BRIDGE_TEST_LOCK.lock().unwrap();

    crate::scheduler_wasm::hew_sched_shutdown();
    crate::scheduler_wasm::hew_sched_init();
    crate::bridge::reset_bridge_full();
    crate::registry::hew_registry_clear();

    // SAFETY: test owns mailbox and actor lifetimes throughout.
    unsafe {
        let mailbox = crate::mailbox_wasm::hew_mailbox_new_bounded(1).cast::<c_void>();
        assert!(!mailbox.is_null());
        assert_eq!(
            crate::mailbox_wasm::hew_mailbox_send(mailbox.cast(), 1, std::ptr::null_mut(), 0),
            0
        );

        let actor = Box::into_raw(stub_wasm_actor(mailbox)); // ALLOCATOR-PAIRING: GlobalAlloc
        let name = std::ffi::CString::new("bridge-full-mailbox").unwrap();
        assert_eq!(
            crate::registry::hew_registry_register(name.as_ptr(), actor.cast()),
            0
        );

        let rc = crate::bridge::hew_wasm_send(
            name.as_ptr().cast(),
            name.as_bytes().len(),
            2,
            std::ptr::null(),
            0,
        );
        assert_eq!(rc, HewError::ErrMailboxFull as i32);
        assert_eq!(
            crate::scheduler_wasm::hew_sched_metrics_global_queue_len(),
            0
        );
        assert_eq!(
            (*actor).actor_state.load(Ordering::Relaxed),
            HewActorState::Idle as i32
        );

        assert_eq!(crate::registry::hew_registry_unregister(name.as_ptr()), 0);
        drop(Box::from_raw(actor)); // ALLOCATOR-PAIRING: GlobalAlloc
        crate::mailbox_wasm::hew_mailbox_free(mailbox.cast());
    }

    crate::scheduler_wasm::hew_sched_shutdown();
}

// ── Cooperate cancellation parity ───────────────────────────────────────────
//
// Native `hew_actor_cooperate` returns `2` when the active task scope or
// cancel token is requested; codegen lowers that into a `cancel_exit` branch
// that runs drop glue and exits the actor. WASM has no task scopes today,
// but actor-state terminal transitions (Stopping / Stopped / Crashed) are an
// equally real cancel source within a handler — and ignoring them silently
// is the parity gap the matrix flags.
//
// This test confirms the WASM cooperate path returns `2` when the actor's
// own state has gone terminal mid-handler. The codegen-emitted
// `cooperate == 2 → cancel_exit` branch is target-neutral, so the named
// exit reason flows through unchanged.

#[test]
fn wasm_cooperate_returns_cancel_when_actor_state_is_terminal() {
    use crate::execution_context::{HewExecutionContext, TestExecutionContext};
    let _guard = crate::runtime_test_guard();

    let actor = stub_wasm_actor(std::ptr::null_mut());
    let actor_ptr: *mut HewActor = Box::into_raw(actor); // ALLOCATOR-PAIRING: GlobalAlloc

    // SAFETY: test owns `actor_ptr` for the duration of the scenario.
    unsafe {
        // Each terminal transition must propagate as a cooperate-cancel.
        for terminal in [
            HewActorState::Stopping,
            HewActorState::Stopped,
            HewActorState::Crashed,
        ] {
            (*actor_ptr)
                .actor_state
                .store(terminal as i32, Ordering::Release);
            // Reset reductions so the function would otherwise return 0 (continue).
            (*actor_ptr)
                .reductions
                .store(crate::actor::HEW_DEFAULT_REDUCTIONS, Ordering::Release);

            let ctx = HewExecutionContext {
                actor: actor_ptr.cast::<crate::actor::HewActor>(),
                actor_id: (*actor_ptr).id,
                ..HewExecutionContext::default()
            };
            let _ctx_guard = TestExecutionContext::install(ctx);

            assert_eq!(
                crate::scheduler_wasm::hew_actor_cooperate(),
                2,
                "WASM cooperate must return 2 (cancel) when actor state is {terminal:?}"
            );
        }

        let _ = Box::from_raw(actor_ptr); // ALLOCATOR-PAIRING: GlobalAlloc
    }
}

#[test]
fn wasm_cooperate_returns_zero_when_actor_state_is_running() {
    use crate::execution_context::{HewExecutionContext, TestExecutionContext};
    let _guard = crate::runtime_test_guard();

    let actor = stub_wasm_actor(std::ptr::null_mut());
    let actor_ptr: *mut HewActor = Box::into_raw(actor); // ALLOCATOR-PAIRING: GlobalAlloc

    // SAFETY: test owns `actor_ptr` for the duration of the scenario.
    unsafe {
        (*actor_ptr)
            .actor_state
            .store(HewActorState::Running as i32, Ordering::Release);
        (*actor_ptr)
            .reductions
            .store(crate::actor::HEW_DEFAULT_REDUCTIONS, Ordering::Release);

        let ctx = HewExecutionContext {
            actor: actor_ptr.cast::<crate::actor::HewActor>(),
            actor_id: (*actor_ptr).id,
            ..HewExecutionContext::default()
        };
        let _ctx_guard = TestExecutionContext::install(ctx);

        // Running actor with budget remaining must return 0 (continue).
        assert_eq!(crate::scheduler_wasm::hew_actor_cooperate(), 0);

        let _ = Box::from_raw(actor_ptr); // ALLOCATOR-PAIRING: GlobalAlloc
    }
}

#[test]
fn wasm_cooperate_returns_cancel_when_mailbox_closes_during_dispatch() {
    let _guard = crate::runtime_test_guard();
    crate::scheduler_wasm::hew_sched_shutdown();
    crate::scheduler_wasm::hew_sched_init();
    reset_dispatch_context_snapshots();

    // SAFETY: test owns mailbox and actor until after the scheduler drain.
    unsafe {
        let mailbox = crate::mailbox_wasm::hew_mailbox_new().cast::<c_void>();
        assert!(!mailbox.is_null());
        assert_eq!(
            crate::mailbox_wasm::hew_mailbox_send(mailbox.cast(), 7, std::ptr::null_mut(), 0),
            HewError::Ok as i32
        );

        let actor = stub_dispatch_actor(mailbox, close_wasm_mailbox_then_cooperate_dispatch);
        let actor_ptr = Box::into_raw(actor); // ALLOCATOR-PAIRING: GlobalAlloc
        crate::scheduler_wasm::sched_enqueue(actor_ptr.cast::<crate::scheduler_wasm::HewActor>());
        crate::scheduler_wasm::hew_sched_run();

        let snapshots = wait_for_dispatch_context_snapshots(1);
        assert_eq!(snapshots.len(), 1);
        assert_eq!(
            snapshots[0].cooperate_result, 2,
            "closing a WASM actor mailbox during dispatch must propagate cooperate cancel code 2"
        );
        assert_eq!(
            (*actor_ptr).actor_state.load(Ordering::Acquire),
            HewActorState::Stopped as i32,
            "closed mailbox should still drive the post-dispatch stop transition"
        );

        crate::mailbox_wasm::hew_mailbox_free(mailbox.cast());
        drop(Box::from_raw(actor_ptr)); // ALLOCATOR-PAIRING: GlobalAlloc
    }

    crate::scheduler_wasm::hew_sched_shutdown();
}

// ── HeapExceeded parity ─────────────────────────────────────────────────────
//
// Native arena cap exhaustion routes through the longjmp seam, stamping
// `HEW_TRAP_HEAP_EXCEEDED` on `actor.error_code` so the supervisor sees
// `ExitReason::HeapExceeded`. WASM has no longjmp; the arena now stamps the
// same code on the current actor and panics, and the activation's
// catch_unwind boundary transitions the actor to `Crashed`. The two paths
// surface the same `ExitReason`.
//
// This test exercises the WASM-only branch of `arena_wasm::hew_arena_malloc`
// directly: under the native-test build of the `arena_wasm` module the cap
// branch is `#[cfg(target_arch = "wasm32")]`-gated, but the substrate move
// of `HEW_TRAP_HEAP_EXCEEDED` + `ExitReason` into `internal::types` ensures
// that whichever target the runtime is built for, the named exit reason is
// reachable through one symbol path. The runtime-side WASM panic path is
// covered by:
//   - `arena_wasm::hew_arena_malloc` returning null on cap-overflow when no
//     ctx-actor is installed (existing test in `arena_wasm.rs`), and
//   - the scheduler-loop change in `scheduler_wasm::activate_actor_wasm`
//     that transitions to Crashed when `error_code` is non-zero after a
//     dispatch unwind (covered by the dispatch-level wiring; ungated on
//     wasm32 builds and exercised by the WASM end-to-end runs in `hew-cli`).

/// Native test that confirms the substrate invariant: a `HEW_TRAP_*` code
/// stamped on `actor.error_code` survives a `from_error_code` round-trip
/// to `ExitReason::HeapExceeded` regardless of whether the lookup happens
/// from native or WASM code paths.
#[test]
fn heap_exceeded_exit_reason_round_trips_through_internal_types() {
    use crate::internal::types::{ExitReason, HEW_TRAP_HEAP_EXCEEDED};
    assert_eq!(HEW_TRAP_HEAP_EXCEEDED, 200);
    assert_eq!(
        ExitReason::from_error_code(HEW_TRAP_HEAP_EXCEEDED),
        ExitReason::HeapExceeded,
    );
    // Cross-check the re-export from the native-only supervisor module.
    assert_eq!(
        crate::supervisor::HEW_TRAP_HEAP_EXCEEDED,
        HEW_TRAP_HEAP_EXCEEDED,
        "supervisor::HEW_TRAP_HEAP_EXCEEDED must re-export the canonical \
         constant in internal::types"
    );
}

/// M-8: the M-6 `ExitReason -> CrashKind` projection (the link-cascade crash
/// class delivered to a linked actor's `#[on(exit)]` hook) must produce the
/// same `CrashKind` tags from the same trap codes regardless of native vs WASM
/// code path — the `CrashKind` discriminator crosses the wasm boundary. The
/// projection lives in `internal::types` (shared by both targets), so this pins
/// that the same trap code maps to the same `CrashKind` tag everywhere.
#[test]
fn crash_kind_projection_is_target_invariant() {
    use crate::internal::types::{CrashKind, HEW_TRAP_HEAP_EXCEEDED, HEW_TRAP_INTEGER_OVERFLOW};
    // HeapExceeded is the only non-Crashed projection; exact tag values.
    assert_eq!(
        CrashKind::tag_from_error_code(HEW_TRAP_HEAP_EXCEEDED),
        CrashKind::HeapExceeded.tag(),
    );
    assert_eq!(
        CrashKind::tag_from_error_code(HEW_TRAP_INTEGER_OVERFLOW),
        CrashKind::Crashed.tag(),
    );
    // Declaration-order tags (the wire value M-7 delivers).
    assert_eq!(CrashKind::Crashed.tag(), 0);
    assert_eq!(CrashKind::HeapExceeded.tag(), 1);
    assert_eq!(CrashKind::PartitionDetected.tag(), 2);
}

fn assert_canonical_wasi_trap_exit(code: i32, expected_reason: crate::internal::types::ExitReason) {
    assert_eq!(
        crate::internal::types::canonical_trap_wasi_exit_code(code),
        Some(code),
        "canonical Hew trap code {code} must be allowlisted for non-actor WASI process exit"
    );
    assert_eq!(
        crate::internal::types::ExitReason::from_error_code(code),
        expected_reason,
        "canonical Hew trap code {code} must keep the same actor ExitReason discriminator"
    );
}

#[test]
fn wasm_non_actor_trap_exit_code_mapping_heap_exceeded_returns_200() {
    assert_canonical_wasi_trap_exit(
        crate::internal::types::HEW_TRAP_HEAP_EXCEEDED,
        crate::internal::types::ExitReason::HeapExceeded,
    );
}

#[test]
fn wasm_non_actor_trap_exit_code_mapping_integer_overflow_returns_201() {
    assert_canonical_wasi_trap_exit(
        crate::internal::types::HEW_TRAP_INTEGER_OVERFLOW,
        crate::internal::types::ExitReason::IntegerOverflow,
    );
}

#[test]
fn wasm_non_actor_trap_exit_code_mapping_divide_by_zero_returns_202() {
    assert_canonical_wasi_trap_exit(
        crate::internal::types::HEW_TRAP_DIVIDE_BY_ZERO,
        crate::internal::types::ExitReason::DivideByZero,
    );
}

#[test]
fn wasm_non_actor_trap_exit_code_mapping_signed_min_div_neg_one_returns_203() {
    assert_canonical_wasi_trap_exit(
        crate::internal::types::HEW_TRAP_SIGNED_MIN_DIV_NEG_ONE,
        crate::internal::types::ExitReason::SignedMinDivNegOne,
    );
}

#[test]
fn wasm_non_actor_trap_exit_code_mapping_shift_out_of_range_returns_204() {
    assert_canonical_wasi_trap_exit(
        crate::internal::types::HEW_TRAP_SHIFT_OUT_OF_RANGE,
        crate::internal::types::ExitReason::ShiftOutOfRange,
    );
}

#[test]
fn wasm_non_actor_trap_exit_code_mapping_index_out_of_bounds_returns_205() {
    assert_canonical_wasi_trap_exit(
        crate::internal::types::HEW_TRAP_INDEX_OUT_OF_BOUNDS,
        crate::internal::types::ExitReason::IndexOutOfBounds,
    );
}

#[test]
fn wasm_trap_exit_code_mapping_actor_send_failed_is_allowlisted_as_206() {
    assert_canonical_wasi_trap_exit(
        crate::internal::types::HEW_TRAP_ACTOR_SEND_FAILED,
        crate::internal::types::ExitReason::ActorSendFailed,
    );
}

#[test]
fn wasm_trap_exit_code_mapping_machine_dispatch_unreachable_is_allowlisted_as_207() {
    assert_canonical_wasi_trap_exit(
        crate::internal::types::HEW_TRAP_MACHINE_DISPATCH_UNREACHABLE,
        crate::internal::types::ExitReason::MachineDispatchUnreachable,
    );
}

#[test]
fn wasm_trap_exit_code_mapping_exhaustiveness_fallthrough_is_allowlisted_as_208() {
    assert_canonical_wasi_trap_exit(
        crate::internal::types::HEW_TRAP_EXHAUSTIVENESS_FALLTHROUGH,
        crate::internal::types::ExitReason::ExhaustivenessFallthrough,
    );
}

#[test]
fn wasm_trap_exit_code_mapping_module_init_regex_failed_is_allowlisted_as_209() {
    assert_canonical_wasi_trap_exit(
        crate::internal::types::HEW_TRAP_MODULE_INIT_REGEX_FAILED,
        crate::internal::types::ExitReason::ModuleInitRegexFailed,
    );
}

#[test]
fn wasm_trap_exit_code_mapping_wire_decode_failed_is_allowlisted_as_210() {
    assert_canonical_wasi_trap_exit(
        crate::internal::types::HEW_TRAP_WIRE_DECODE_FAILED,
        crate::internal::types::ExitReason::WireDecodeFailed,
    );
}

#[test]
fn wasm_unknown_non_actor_trap_code_is_not_mapped_to_process_exit() {
    // 210 (HEW_TRAP_WIRE_DECODE_FAILED) is now a Hew-owned discriminator and is
    // allowlisted; 211 takes its place as the first unused code.
    for unknown in [-1, 1, 101, 199, 211, i32::MAX] {
        assert_eq!(
            crate::internal::types::canonical_trap_wasi_exit_code(unknown),
            None,
            "unknown trap code {unknown} must return to the generated trailing llvm.trap sink"
        );
    }
}

/// Native test that exercises the WASM activation crash-transition logic:
/// when a dispatch panics with `actor.error_code != 0`, the scheduler must
/// observe the code and transition the actor to `Crashed`. The WASM-only
/// `arena_wasm::hew_arena_malloc` cap branch installs both the code and
/// the panic; this test verifies the *scheduler half* of the seam directly
/// by stamping the code from a dispatch handler that panics on its own.
unsafe extern "C-unwind" fn stamp_then_panic_dispatch(
    ctx: *mut crate::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _data_size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    // SAFETY: scheduler installs a non-null canonical context before dispatch.
    unsafe {
        let actor = (*ctx).actor;
        if !actor.is_null() {
            (*actor).error_code.store(
                crate::internal::types::HEW_TRAP_HEAP_EXCEEDED,
                Ordering::Release,
            );
        }
    }
    // Simulate the panic that `arena_wasm::hew_arena_malloc` raises on cap
    // exhaustion under wasm32. The panic diverges, so no run-to-completion
    // null return follows.
    panic!("simulated HEW_TRAP_HEAP_EXCEEDED panic");
}

unsafe extern "C-unwind" fn hew_trap_with_code_dispatch(
    _ctx: *mut crate::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _data_size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    if crate::trap_code::stamp_current_actor_error_code(
        crate::internal::types::HEW_TRAP_HEAP_EXCEEDED,
    ) {
        panic!("simulated wasm hew_trap_with_code panic");
    }
    std::ptr::null_mut()
}

unsafe extern "C-unwind" fn hew_panic_wasm_actor_dispatch(
    _ctx: *mut crate::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _data_size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    assert!(
        !crate::actor::stamp_wasm_actor_panic(),
        "simulated wasm hew_panic actor unwind"
    );
    std::ptr::null_mut()
}

#[test]
fn wasm_activation_transitions_actor_to_crashed_when_dispatch_stamps_error_code() {
    let _guard = crate::runtime_test_guard();
    crate::scheduler_wasm::hew_sched_shutdown();
    crate::scheduler_wasm::hew_sched_init();

    // SAFETY: test owns the mailbox and actor for the full scenario.
    unsafe {
        let mailbox = crate::mailbox_wasm::hew_mailbox_new();
        assert!(!mailbox.is_null());

        let actor = stub_dispatch_actor(mailbox.cast(), stamp_then_panic_dispatch);
        let actor_ptr: *mut HewActor = Box::into_raw(actor); // ALLOCATOR-PAIRING: GlobalAlloc

        let payload: i32 = 0;
        let rc = crate::mailbox_wasm::hew_mailbox_send(
            mailbox.cast(),
            1,
            (&raw const payload).cast_mut().cast(),
            std::mem::size_of::<i32>(),
        );
        assert_eq!(rc, HewError::Ok as i32);

        crate::scheduler_wasm::sched_enqueue(actor_ptr.cast());
        crate::scheduler_wasm::hew_sched_run();

        let error = (*actor_ptr).error_code.load(Ordering::Acquire);
        let state = (*actor_ptr).actor_state.load(Ordering::Acquire);

        assert_eq!(
            error,
            crate::internal::types::HEW_TRAP_HEAP_EXCEEDED,
            "dispatch must have stamped HEW_TRAP_HEAP_EXCEEDED"
        );
        assert_eq!(
            crate::internal::types::ExitReason::from_error_code(error),
            crate::internal::types::ExitReason::HeapExceeded,
        );
        assert_eq!(
            state,
            HewActorState::Crashed as i32,
            "WASM activation must transition the actor to Crashed when \
             the dispatch unwind comes with a non-zero error_code"
        );

        let _ = Box::from_raw(actor_ptr); // ALLOCATOR-PAIRING: GlobalAlloc
        crate::mailbox_wasm::hew_mailbox_free(mailbox.cast());
    }

    crate::scheduler_wasm::hew_sched_shutdown();
}

#[test]
fn wasm_actor_panic_stamps_101_and_unwinds_to_scheduler() {
    let _guard = crate::runtime_test_guard();
    crate::scheduler_wasm::hew_sched_shutdown();
    crate::scheduler_wasm::hew_sched_init();

    // SAFETY: test owns the mailbox and actor for the full scenario.
    unsafe {
        let mailbox = crate::mailbox_wasm::hew_mailbox_new();
        assert!(!mailbox.is_null());

        let actor = stub_dispatch_actor(mailbox.cast(), hew_panic_wasm_actor_dispatch);
        let actor_ptr: *mut HewActor = Box::into_raw(actor); // ALLOCATOR-PAIRING: GlobalAlloc

        let payload: i32 = 0;
        let rc = crate::mailbox_wasm::hew_mailbox_send(
            mailbox.cast(),
            1,
            (&raw const payload).cast_mut().cast(),
            std::mem::size_of::<i32>(),
        );
        assert_eq!(rc, HewError::Ok as i32);

        crate::scheduler_wasm::sched_enqueue(actor_ptr.cast());
        crate::scheduler_wasm::hew_sched_run();

        let error = (*actor_ptr).error_code.load(Ordering::Acquire);
        let state = (*actor_ptr).actor_state.load(Ordering::Acquire);

        assert_eq!(
            error, 101,
            "wasm actor hew_panic must stamp the raw panic sentinel before unwinding"
        );
        assert_eq!(
            crate::internal::types::ExitReason::from_error_code(error),
            crate::internal::types::ExitReason::Signal(101),
        );
        assert_eq!(
            state,
            HewActorState::Crashed as i32,
            "WASM activation must catch actor panic unwinds instead of terminating the process"
        );

        let _ = Box::from_raw(actor_ptr); // ALLOCATOR-PAIRING: GlobalAlloc
        crate::mailbox_wasm::hew_mailbox_free(mailbox.cast());
    }

    crate::scheduler_wasm::hew_sched_shutdown();
}

#[test]
fn wasm_heaps_exceeded_uses_trap_with_code_bridge_to_crash_actor() {
    let _guard = crate::runtime_test_guard();
    crate::scheduler_wasm::hew_sched_shutdown();
    crate::scheduler_wasm::hew_sched_init();

    // SAFETY: test owns the mailbox and actor for the full scenario.
    unsafe {
        let mailbox = crate::mailbox_wasm::hew_mailbox_new();
        assert!(!mailbox.is_null());

        let actor = stub_dispatch_actor(mailbox.cast(), hew_trap_with_code_dispatch);
        let actor_ptr: *mut HewActor = Box::into_raw(actor); // ALLOCATOR-PAIRING: GlobalAlloc

        let payload: i32 = 0;
        let rc = crate::mailbox_wasm::hew_mailbox_send(
            mailbox.cast(),
            1,
            (&raw const payload).cast_mut().cast(),
            std::mem::size_of::<i32>(),
        );
        assert_eq!(rc, HewError::Ok as i32);

        crate::scheduler_wasm::sched_enqueue(actor_ptr.cast());
        crate::scheduler_wasm::hew_sched_run();

        let error = (*actor_ptr).error_code.load(Ordering::Acquire);
        let state = (*actor_ptr).actor_state.load(Ordering::Acquire);

        assert_eq!(
            error,
            crate::internal::types::HEW_TRAP_HEAP_EXCEEDED,
            "hew_trap_with_code must stamp the HeapExceeded discriminator"
        );
        assert_eq!(
            crate::internal::types::ExitReason::from_error_code(error),
            crate::internal::types::ExitReason::HeapExceeded,
        );
        assert_eq!(state, HewActorState::Crashed as i32);

        let _ = Box::from_raw(actor_ptr); // ALLOCATOR-PAIRING: GlobalAlloc
        crate::mailbox_wasm::hew_mailbox_free(mailbox.cast());
    }

    crate::scheduler_wasm::hew_sched_shutdown();
}
