#![expect(
    clippy::undocumented_unsafe_blocks,
    reason = "FFI spawn tests assert raw ABI state directly"
)]

use std::ffi::c_void;
use std::ptr;

use hew_runtime::actor::{hew_actor_free, hew_actor_spawn_opts, HewActorOpts};
use hew_runtime::supervisor::{
    hew_supervisor_add_child_spec, hew_supervisor_new, hew_supervisor_start, hew_supervisor_stop,
    HewChildSpec, RESTART_PERMANENT, STRATEGY_ONE_FOR_ONE,
};

unsafe extern "C-unwind" fn noop_dispatch(
    _ctx: *mut hew_runtime::execution_context::HewExecutionContext,
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
    _borrow_mode: i32,
) -> *mut c_void {
    std::ptr::null_mut()
}

fn spawn_with_cycle_flag(cycle_capable: i32) -> *mut hew_runtime::actor::HewActor {
    let opts = HewActorOpts {
        init_state: ptr::null_mut(),
        state_size: 0,
        dispatch: Some(noop_dispatch),
        mailbox_capacity: 0,
        overflow: 0,
        coalesce_key_fn: None,
        coalesce_fallback: 0,
        budget: 0,
        arena_cap_bytes: 0,
        cycle_capable,
    };

    unsafe { hew_actor_spawn_opts(&raw const opts) }
}

#[test]
fn spawn_opts_accepts_nonzero_cycle_capable_bit() {
    let actor = spawn_with_cycle_flag(1);
    assert!(!actor.is_null(), "cycle-capable spawn opts should succeed");
    assert_eq!(unsafe { hew_actor_free(actor) }, 0);
}

#[test]
fn spawn_opts_accepts_zero_cycle_capable_default() {
    let actor = spawn_with_cycle_flag(0);
    assert!(!actor.is_null(), "non-cycle spawn opts should succeed");
    assert_eq!(unsafe { hew_actor_free(actor) }, 0);
}

#[test]
fn supervisor_child_spec_accepts_cycle_capable_bit() {
    let sup = unsafe { hew_supervisor_new(STRATEGY_ONE_FOR_ONE, 1, 1) };
    assert!(!sup.is_null(), "supervisor allocation should succeed");

    let spec = HewChildSpec {
        name: ptr::null(),
        init_state: ptr::null_mut(),
        init_state_size: 0,
        dispatch: Some(noop_dispatch),
        restart_policy: RESTART_PERMANENT,
        mailbox_capacity: -1,
        overflow: 1,
        arena_cap_bytes: 0,
        cycle_capable: 1,
        on_crash: None,
    };

    assert_eq!(
        unsafe { hew_supervisor_add_child_spec(sup, &raw const spec) },
        0
    );
    assert_eq!(unsafe { hew_supervisor_start(sup) }, 0);

    unsafe { hew_supervisor_stop(sup) };
}
