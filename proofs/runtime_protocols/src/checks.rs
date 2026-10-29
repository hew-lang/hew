//! Production kernels checked with symbolic inputs and bounded heap graphs.
use crate::wake::{HewWaker, OwnedWaker};
use std::cell::Cell;
use std::ffi::c_void;
#[cfg(feature = "waker")]
#[derive(Default)]
struct Probe {
    refs: std::sync::atomic::AtomicUsize,
    wakes: std::sync::atomic::AtomicUsize,
}
#[cfg(feature = "waker")]
unsafe extern "C" fn retain(context: *mut c_void) {
    let probe = &*context.cast::<Probe>();
    assert!(
        probe.refs.fetch_add(1, std::sync::atomic::Ordering::SeqCst) > 0,
        "protocol: retained a dead target"
    );
}
#[cfg(feature = "waker")]
unsafe extern "C" fn release(context: *mut c_void) {
    let probe = &*context.cast::<Probe>();
    assert!(
        probe.refs.fetch_sub(1, std::sync::atomic::Ordering::SeqCst) > 0,
        "protocol: target reference released twice"
    );
}
#[cfg(feature = "waker")]
unsafe extern "C" fn wake(context: *mut c_void) {
    let probe = &*context.cast::<Probe>();
    assert!(
        probe.refs.load(std::sync::atomic::Ordering::SeqCst) > 0,
        "protocol: woke a released target"
    );
    probe
        .wakes
        .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
}
#[cfg(feature = "waker")]
fn descriptor(probe: &Probe) -> HewWaker {
    probe.refs.store(1, std::sync::atomic::Ordering::SeqCst);
    HewWaker {
        context: std::ptr::from_ref(probe).cast_mut().cast(),
        wake,
        retain,
        release,
    }
}
#[cfg(feature = "waker")]
#[kani::stub(core::panicking::panic_fmt, unexpected_panic)]
#[kani::proof]
#[kani::unwind(5)]
fn core_waker_retains_clones_and_releases_exactly_once() {
    let probe = Probe::default();
    let descriptor = descriptor(&probe);
    let mut slots = [Some(unsafe { OwnedWaker::retain(&descriptor) }), None];
    let mut creator = true;
    let mut expected_wakes = 0;
    for _ in 0..4 {
        let operation: u8 = kani::any();
        let source: usize = kani::any();
        let target: usize = kani::any();
        kani::assume(operation < 4 && source < 2 && target < 2);
        match operation {
            0 => {
                if slots[target].is_none() {
                    slots[target] = slots[source].clone();
                }
            }
            1 => {
                if let Some(waker) = &slots[source] {
                    waker.wake();
                    expected_wakes += 1;
                }
            }
            2 => drop(slots[source].take()),
            _ => {
                if creator {
                    unsafe { release(descriptor.context) };
                    creator = false;
                }
            }
        }
        let owners = usize::from(slots[0].is_some())
            + usize::from(slots[1].is_some())
            + usize::from(creator);
        assert_eq!(
            probe.refs.load(std::sync::atomic::Ordering::SeqCst),
            owners,
            "protocol: waker ownership accounting disagrees"
        );
        assert_eq!(
            probe.wakes.load(std::sync::atomic::Ordering::SeqCst),
            expected_wakes,
            "protocol: readiness callback count changed"
        );
    }
    drop(slots);
    if creator {
        unsafe { release(descriptor.context) };
    }
    assert_eq!(
        probe.refs.load(std::sync::atomic::Ordering::SeqCst),
        0,
        "protocol: retained target leaked"
    );
}

#[cfg(feature = "cancellation")]
#[kani::proof]
fn core_cancellation_preserves_the_first_nonzero_reason() {
    use crate::cancel_state::request;
    use crate::runtime_sync::AtomicI32;
    use std::sync::atomic::Ordering;
    let old_state: i32 = kani::any();
    let old_reason: i32 = kani::any();
    let first: i32 = kani::any();
    let second: i32 = kani::any();
    kani::assume((0..=4).contains(&old_state));
    let state = AtomicI32::new(old_state);
    let reason = AtomicI32::new(old_reason);
    let won = request(&state, &reason, first);
    let expected_reason = if old_reason != 0 {
        old_reason
    } else if first == 0 {
        1
    } else {
        first
    };
    assert_eq!(
        won,
        old_state == 0,
        "protocol: cancellation publication admitted the wrong owner"
    );
    assert_eq!(
        reason.load(Ordering::Acquire),
        expected_reason,
        "protocol: winning cancellation reason was replaced"
    );
    assert!(
        !request(&state, &reason, second),
        "protocol: cancellation was published twice"
    );
    assert_eq!(
        reason.load(Ordering::Acquire),
        expected_reason,
        "protocol: later requester replaced the cancellation reason"
    );
    assert_eq!(
        state.load(Ordering::Acquire),
        if old_state == 0 { 1 } else { old_state },
        "protocol: cancellation reversed an existing state"
    );
}

#[cfg(feature = "await")]
#[kani::proof]
fn core_await_settlement_authorizes_cleanup_once() {
    use crate::await_state::{cancellation_status, settle, AwaitCancelStatus};
    use crate::runtime_sync::AtomicI32;
    use std::sync::atomic::Ordering;
    let previous: i32 = kani::any();
    let raw_cancel: i32 = kani::any();
    let complete: bool = kani::any();
    let wake_actor: bool = kani::any();
    kani::assume((0..=3).contains(&previous));
    let state = AtomicI32::new(previous);
    let status = if complete {
        AwaitCancelStatus::Completed
    } else {
        cancellation_status(raw_cancel)
    };
    let expected = if complete {
        1
    } else if raw_cancel == 3 {
        3
    } else {
        2
    };
    assert_eq!(
        status as i32, expected,
        "protocol: cancellation input selected an invalid terminal outcome"
    );
    let result = settle(&state, status, wake_actor);
    assert_eq!(
        result.is_some(),
        previous == 0,
        "protocol: await admitted a second terminal owner"
    );
    if let Some(effects) = result {
        assert_eq!(
            effects.cleanup, !complete,
            "protocol: await selected the wrong cleanup policy"
        );
        assert_eq!(
            effects.wake,
            !complete && wake_actor,
            "protocol: await selected the wrong wake policy"
        );
    }
    assert_eq!(
        state.load(Ordering::Acquire),
        if previous == 0 { expected } else { previous },
        "protocol: losing settlement changed the terminal outcome"
    );
    assert!(
        settle(&state, cancellation_status(kani::any()), true).is_none(),
        "protocol: await authorized cleanup twice"
    );
}
#[cfg(feature = "timer")]
unsafe extern "C" fn timer_count(data: *mut c_void) {
    let count = &*data.cast::<Cell<usize>>();
    count.set(count.get() + 1);
}

#[cfg(feature = "timer")]
#[kani::stub(core::panicking::panic_fmt, unexpected_panic)]
#[kani::proof]
#[kani::unwind(4)]
fn core_timer_removal_preserves_sibling_links() {
    use crate::timer_kernel::*;
    let target: usize = kani::any();
    let wrong_generation: u64 = kani::any();
    kani::assume(target < 3 && wrong_generation != target as u64 + 1);
    let payloads = [Cell::new(0usize), Cell::new(0usize), Cell::new(0usize)];
    let mut nodes = [std::ptr::null_mut(); 3];
    let mut head = std::ptr::null_mut();
    for i in 0..3 {
        let node = Box::into_raw(Box::new(HewTimerEntry {
            fraction_ns: 0,
            deadline_ms: 1,
            generation: i as u64 + 1,
            cb: Some(timer_count),
            data: std::ptr::from_ref(&payloads[i]).cast_mut().cast(),
            cancelled: 0,
            next: head,
        }));
        nodes[i] = node;
        head = node;
    }
    assert!(
        remove_entry_from_list(&mut head, nodes[target], wrong_generation).is_none(),
        "protocol: stale timer generation claimed a live payload"
    );
    assert_eq!(
        remove_entry_from_list(&mut head, nodes[target], target as u64 + 1),
        Some(std::ptr::from_ref(&payloads[target]).cast_mut().cast()),
        "protocol: timer removal transferred the wrong payload"
    );
    assert!(
        remove_entry_from_list(&mut head, nodes[target], target as u64 + 1).is_none(),
        "protocol: timer payload was removed twice"
    );
    for i in (0..3).rev() {
        if i != target {
            assert_eq!(
                head, nodes[i],
                "protocol: timer removal corrupted a sibling link"
            );
            head = unsafe { (*head).next };
            unsafe {
                drop(Box::from_raw(nodes[i]));
            }
        }
    }
    assert!(
        head.is_null(),
        "protocol: timer removal retained a dangling list link"
    );
}

#[cfg(feature = "timer")]
#[kani::stub(core::panicking::panic_fmt, unexpected_panic)]
#[kani::proof]
#[kani::unwind(2)]
fn core_timer_removal_and_collection_have_one_payload_owner() {
    use crate::timer_kernel::*;
    let deadline: u64 = kani::any();
    let now: u64 = kani::any();
    let fraction: u32 = kani::any();
    let now_fraction: u32 = kani::any();
    kani::assume(fraction < 1_000_000 && now_fraction < 1_000_000);
    let generation: u64 = kani::any();
    kani::assume(generation != 0);
    let remove_first: bool = kani::any();
    let count = Cell::new(0usize);
    let data = std::ptr::from_ref(&count).cast_mut().cast();
    let node = Box::into_raw(Box::new(HewTimerEntry {
        fraction_ns: fraction,
        deadline_ms: deadline,
        generation,
        cb: Some(timer_count),
        data,
        cancelled: 0,
        next: std::ptr::null_mut(),
    }));
    let mut head = node;
    let mut removed = if remove_first {
        remove_entry_from_list(&mut head, node, generation)
    } else {
        None
    };
    let mut collected = Vec::new();
    collect_expired_entries_at(&raw mut head, crate::timer_deadline::Deadline { millis: now, fraction_ns: now_fraction }, &mut collected);
    let after_collection = remove_entry_from_list(&mut head, node, generation);
    if removed.is_none() {
        removed = after_collection;
    } else {
        assert!(
            after_collection.is_none(),
            "protocol: timer removal claimed a payload twice"
        );
    }
    let fired = fire_expired_entries(collected);
    assert_eq!(
        fired,
        i32::from(!remove_first && (deadline < now || (deadline == now && fraction <= now_fraction))),
        "protocol: timer fired early or after removal"
    );
    assert_eq!(
        count.get() + usize::from(removed.is_some()),
        1,
        "protocol: removal and collection lost or duplicated payload ownership"
    );
    assert!(head.is_null(), "protocol: terminal timer stayed linked");
}

#[cfg(feature = "arena")]
#[kani::proof]
fn arena_layout_and_quota_are_exact_for_all_machine_integers() {
    use crate::arena_layout::{budget, fit};
    let cursor: usize = kani::any();
    let size: usize = kani::any();
    let align: usize = kani::any();
    let capacity: usize = kani::any();
    let valid = size > 0
        && size <= isize::MAX as usize
        && align > 0
        && align <= 4096
        && align.count_ones() == 1
        && capacity <= isize::MAX as usize;
    let result = fit(cursor, size, align, capacity);
    if valid {
        let wide_align = align as u128;
        let start = (cursor as u128 + wide_align - 1) & !(wide_align - 1);
        let end = start + size as u128;
        assert_eq!(
            result.is_some(),
            end <= capacity as u128,
            "arena: fit disagrees with representable extent"
        );
        if let Some(extent) = result {
            assert_eq!(extent.start as u128, start, "arena: alignment wrapped");
            assert_eq!(extent.end as u128, end, "arena: extent wrapped");
            assert!(
                extent.start >= cursor,
                "arena: overlap with previous allocation"
            );
            assert_eq!(extent.start % align, 0, "arena: invalid alignment");
            assert_eq!(extent.end - extent.start, size, "arena: undersized extent");
        }
    } else {
        assert!(result.is_none(), "arena: invalid request was accepted");
    }
    let used: usize = kani::any();
    let cap: usize = kani::any();
    let expected = if cap == 0 {
        Some(used)
    } else if used as u128 + size as u128 <= cap as u128 {
        Some((used as u128 + size as u128) as usize)
    } else {
        None
    };
    assert_eq!(
        budget(used, cap, size),
        expected,
        "arena: quota wrapped or rejected available capacity"
    );
}

fn unexpected_panic(_: std::fmt::Arguments<'_>) -> ! {
    assert!(false, "protocol: unexpected runtime panic");
    kani::assume(false);
    loop {}
}

#[cfg(feature = "supervisor")]
#[kani::proof]
fn supervisor_pin_count_cannot_wrap_or_reopen_closed_admission() {
    let state: usize = kani::any();
    let expected = if (state as u128) < (1u128 << (usize::BITS - 1)) - 1 {
        Some(state + 1)
    } else {
        None
    };
    assert_eq!(
        crate::pin_state::next_pin(state),
        expected,
        "lifecycle: pin overflow or closed admission"
    );
}

#[cfg(feature = "read_slot")]
#[kani::proof]
#[kani::unwind(3)]
#[kani::stub(core::panicking::panic_fmt, unexpected_panic)]
fn read_slot_teardown_preserves_terminal_status_and_retained_lifetime() {
    use crate::read_slot::*;
    let cancel_first: bool = kani::any();
    let raw_reason: i32 = kani::any();
    let choice: u8 = kani::any();
    let status = match choice {
        0 => ReadStatus::Data,
        1 => ReadStatus::Eof,
        _ => ReadStatus::Error,
    };
    let cancelled = if raw_reason == 3 { 5 } else { 4 };
    unsafe {
        let slot = hew_read_slot_new();
        read_slot_retain(slot); // queued registration's independent lifetime
        if cancel_first {
            hew_read_slot_cancel_cleanup(slot.cast(), raw_reason);
        }
        let notified = read_slot_deposit_status(slot, status);
        assert_eq!(
            notified, !cancel_first,
            "lifecycle: slot woke cancelled work or lost readiness"
        );
        hew_read_slot_cancel_cleanup(slot.cast(), raw_reason);
        hew_read_slot_free(slot); // creator gone; registration still owns slot
        assert_eq!(
            hew_read_slot_status(slot),
            if cancel_first {
                cancelled
            } else {
                status as i32
            },
            "lifecycle: read-slot terminal outcome was replaced"
        );
        assert!(
            !read_slot_deposit_status(slot, ReadStatus::Eof),
            "lifecycle: read-slot readiness published twice"
        );
        assert!(
            hew_read_slot_take(slot).ptr.is_null(),
            "lifecycle: readiness slot fabricated a payload"
        );
        hew_read_slot_free(slot);
    }
}

#[cfg(feature = "managed_budget")]
#[kani::proof]
fn managed_quota_counts_all_live_and_reserved_bytes_without_overflow() {
    let live: u64 = kani::any();
    let reserved: u64 = kani::any();
    let bytes: u64 = kani::any();
    let limit: Option<u64> = kani::any();
    let total = u128::from(live) + u128::from(reserved) + u128::from(bytes);
    let expected =
        total <= u128::from(u64::MAX) && limit.is_none_or(|cap| total <= u128::from(cap));
    assert_eq!(
        crate::budget_limit::admits(live, reserved, bytes, limit),
        expected,
        "lifecycle: managed quota overflowed or ignored an outstanding reservation"
    );
}
