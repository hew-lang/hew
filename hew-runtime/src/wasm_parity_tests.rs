#![cfg(test)]

use std::ffi::c_void;
use std::sync::atomic::{AtomicBool, AtomicI32, AtomicPtr, AtomicU64, Ordering};

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

#[derive(Debug, Eq, PartialEq)]
struct CoalesceSnapshot {
    send_rcs: [i32; 2],
    messages_sent: u64,
    queue_len: usize,
    msg_type: i32,
    payload: PriceUpdate,
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

fn stub_wasm_actor(mailbox: *mut c_void) -> Box<HewActor> {
    Box::new(HewActor {
        sched_link_next: AtomicPtr::new(std::ptr::null_mut()),
        id: 1,
        pid: 1,
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
        terminate_called: AtomicBool::new(false),
        terminate_finished: AtomicBool::new(false),
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
    })
}

#[test]
fn coalesced_mailbox_metrics_match_native_and_wasm() {
    assert_eq!(native_coalesce_snapshot(), wasm_coalesce_snapshot());
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

        let actor = Box::into_raw(stub_wasm_actor(mailbox));
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
        drop(Box::from_raw(actor));
        crate::mailbox_wasm::hew_mailbox_free(mailbox.cast());
    }

    crate::scheduler_wasm::hew_sched_shutdown();
}
