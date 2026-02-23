//! Integration tests for actor links and monitors.

use hew_runtime::actor::{hew_actor_free, hew_actor_spawn};
use hew_runtime::link::{hew_actor_link, hew_actor_unlink};
use hew_runtime::monitor::{hew_actor_demonitor, hew_actor_monitor};
use std::ffi::c_void;

unsafe extern "C" fn test_dispatch(
    _state: *mut c_void,
    _msg_type: i32,
    _data: *mut c_void,
    _size: usize,
) {
    // Simple test dispatch - does nothing
}

#[test]
fn test_link_and_monitor_basic() {
    // Create two test actors
    let actor_a = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(test_dispatch)) };

    let actor_b = unsafe { hew_actor_spawn(std::ptr::null_mut(), 0, Some(test_dispatch)) };

    assert!(!actor_a.is_null());
    assert!(!actor_b.is_null());

    // Create link between actors
    unsafe {
        hew_actor_link(actor_a, actor_b);
    }

    // Create monitor from A to B
    let ref_id = unsafe { hew_actor_monitor(actor_a, actor_b) };
    assert_ne!(ref_id, 0);

    // Remove the link and monitor
    unsafe {
        hew_actor_unlink(actor_a, actor_b);
    }
    hew_actor_demonitor(ref_id);

    // Clean up
    unsafe {
        hew_actor_free(actor_a);
        hew_actor_free(actor_b);
    }
}

#[test]
fn test_null_handling() {
    // Test that null pointers are handled gracefully
    unsafe {
        hew_actor_link(std::ptr::null_mut(), std::ptr::null_mut());
        hew_actor_unlink(std::ptr::null_mut(), std::ptr::null_mut());
    }

    let ref_id = unsafe { hew_actor_monitor(std::ptr::null_mut(), std::ptr::null_mut()) };
    assert_eq!(ref_id, 0);

    hew_actor_demonitor(0);
    hew_actor_demonitor(99999);
}
