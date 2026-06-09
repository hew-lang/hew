use std::panic::{catch_unwind, AssertUnwindSafe};

use hew_runtime::machine_emit::{
    hew_machine_emit_push, hew_machine_emit_step_enter, hew_machine_emit_step_exit,
    thread_emit_clear, thread_emit_pending, EmitQueue, MachineEmitReentrancyExceeded,
    DEFAULT_REENTRANCY_CAP,
};

#[test]
fn push_drain_fifo_across_nested_emit_at_outermost_step() {
    let mut queue = EmitQueue::new();
    let mut nested_drain_count = 0usize;

    queue.enter_step();
    queue.push(0, std::ptr::null());

    queue.enter_step();
    queue.push(1, std::ptr::null());
    queue
        .exit_step(|_, _| {
            nested_drain_count += 1;
            Ok::<(), ()>(())
        })
        .expect("nested step exit must not drain");
    assert_eq!(nested_drain_count, 0);
    assert_eq!(queue.pending(), 2);

    let mut drained = Vec::new();
    queue
        .exit_step(|event, append| {
            drained.push(event.tag);
            if event.tag == 0 {
                append.push(2, std::ptr::null());
            }
            Ok::<(), ()>(())
        })
        .expect("outermost step exit must drain");

    assert_eq!(drained, vec![0, 1, 2]);
    assert_eq!(queue.pending(), 0);
}

#[test]
fn cap_exceeded_panics_with_typed_machine_emit_reentrancy_exceeded() {
    let mut queue = EmitQueue::with_cap(DEFAULT_REENTRANCY_CAP);

    let panic = catch_unwind(AssertUnwindSafe(|| {
        for _ in 0..=DEFAULT_REENTRANCY_CAP {
            queue.enter_step();
        }
    }))
    .expect_err("65th nested step must exceed the default cap");

    let typed = panic
        .downcast_ref::<MachineEmitReentrancyExceeded>()
        .expect("panic payload must be MachineEmitReentrancyExceeded");
    assert_eq!(typed.depth, DEFAULT_REENTRANCY_CAP + 1);
    assert_eq!(typed.cap, DEFAULT_REENTRANCY_CAP);
}

#[test]
fn drop_clears_queue_after_panic_mid_drain() {
    let mut queue = EmitQueue::new();
    queue.push(0, std::ptr::null());
    queue.push(1, std::ptr::null());

    let panic = catch_unwind(AssertUnwindSafe(|| {
        let _: Result<(), hew_runtime::machine_emit::DrainError<()>> =
            queue.drain(|event, _append| -> Result<(), ()> {
                assert_eq!(event.tag, 0);
                panic!("forced panic mid-drain");
            });
    }));
    assert!(panic.is_err());
    assert_eq!(queue.pending(), 0);

    queue.push(9, std::ptr::null());
    let mut drained = Vec::new();
    queue
        .drain(|event, _append| {
            drained.push(event.tag);
            Ok::<(), ()>(())
        })
        .expect("queue must be reusable after caught panic");
    assert_eq!(drained, vec![9]);
}

#[test]
fn c_abi_step_exit_drains_only_outermost_frame() {
    thread_emit_clear();
    let queue = std::ptr::null_mut();

    // SAFETY: null selects the calling thread's emit queue; all calls happen
    // synchronously on this test thread.
    unsafe {
        assert_eq!(hew_machine_emit_step_enter(queue), 0);
        assert_eq!(hew_machine_emit_push(queue, 0, std::ptr::null()), 0);
        assert_eq!(hew_machine_emit_step_enter(queue), 0);
        assert_eq!(hew_machine_emit_push(queue, 1, std::ptr::null()), 0);

        assert_eq!(hew_machine_emit_step_exit(queue), 0);
        assert_eq!(thread_emit_pending(), 2);

        assert_eq!(hew_machine_emit_step_exit(queue), 0);
        assert_eq!(thread_emit_pending(), 0);
    }
}

#[test]
fn wasm_parity_has_no_target_specific_gate() {
    assert_eq!(DEFAULT_REENTRANCY_CAP, 64);
}
