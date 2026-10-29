//! Independent verification host; never linked into Hew.
#![allow(
    dead_code,
    unused_imports,
    unused_macros,
    unexpected_cfgs,
    unsafe_op_in_unsafe_fn
)]
macro_rules! cabi_guard {
    ($condition:expr) => {
        if $condition {
            return;
        }
    };
    ($condition:expr, $result:expr) => {
        if $condition {
            return $result;
        }
    };
}

#[cfg(any(feature = "await_race", feature = "read_slot"))]
#[path = "../../../hew-runtime/src/await_cancel.rs"]
mod await_cancel;
#[cfg(any(feature = "await", feature = "read_slot"))]
#[path = "../../../hew-runtime/src/await_state.rs"]
mod await_state;
#[cfg(feature = "managed_budget")]
#[path = "../../../hew-runtime/src/owned_value_budget/budget_limit.rs"]
mod budget_limit;
#[cfg(feature = "read_slot")]
#[path = "../../../hew-runtime/src/bytes_value.rs"]
mod bytes_value;
#[cfg(feature = "cancellation")]
#[path = "../../../hew-runtime/src/cancel_state.rs"]
mod cancel_state;
#[cfg(any(
    feature = "cancellation_delivery",
    feature = "await_race",
    feature = "read_slot"
))]
#[path = "../../../hew-runtime/src/cancel_token.rs"]
mod cancel_token;
#[cfg(feature = "sleep_teardown")]
#[path = "../../../hew-runtime/src/coro_sleep.rs"]
mod coro_sleep;
#[cfg(all(test, feature = "failure_properties"))]
#[path = "timer_deadline_properties.rs"]
mod deadline_failure_properties;
mod environment;
#[cfg(all(kani, feature = "deadline"))]
#[path = "timer_deadline.rs"]
mod formal_deadline;
#[cfg(feature = "managed_budget")]
#[path = "../../../hew-runtime/src/owned_value_budget/account.rs"]
mod managed_budget;
#[cfg(any(feature = "heap", feature = "vector"))]
#[path = "../../../hew-runtime/src/mem.rs"]
mod mem;
#[cfg(feature = "supervisor")]
#[path = "../../../hew-runtime/src/lifetime/pin_state.rs"]
mod pin_state;
#[cfg(feature = "read_slot")]
#[path = "../../../hew-runtime/src/read_slot.rs"]
mod read_slot;
#[cfg(feature = "semaphore")]
#[path = "../../../hew-runtime/src/semaphore.rs"]
mod semaphore;
#[cfg(any(
    feature = "deadline",
    feature = "sleep_teardown",
    feature = "timer",
    feature = "await_race",
    feature = "read_slot",
    feature = "semaphore",
    feature = "reactor",
    feature = "periodic_loom"
))]
#[path = "../../../hew-runtime/src/timer_deadline.rs"]
mod timer_deadline;
#[cfg(feature = "timer")]
type HewTimerCb = unsafe extern "C" fn(*mut std::ffi::c_void);
#[cfg(feature = "arena")]
#[path = "../../../hew-runtime/src/arena.rs"]
mod arena;
#[cfg(feature = "arena")]
extern crate self as libc;
#[cfg(feature = "arena")]
mod arena_host;
#[cfg(feature = "arena")]
#[path = "../../../hew-runtime/src/arena_layout.rs"]
mod arena_layout;
#[cfg(feature = "arena")]
pub use arena_host::*;
#[path = "../../../hew-runtime/src/request_deadline.rs"]
mod request_deadline;
#[path = "sync.rs"]
mod runtime_sync;
#[cfg(feature = "timer")]
#[path = "../../../hew-runtime/src/timer_entry.rs"]
mod timer_kernel;
#[cfg(any(
    feature = "timer",
    feature = "await_race",
    feature = "sleep_teardown",
    feature = "read_slot",
    feature = "semaphore",
    feature = "reactor_loom",
    feature = "periodic_loom"
))]
#[path = "../../../hew-runtime/src/timer_wheel.rs"]
mod timer_wheel;
#[path = "wake.rs"]
mod wake;
#[cfg(not(any(feature = "executor", feature = "periodic_loom")))]
use environment::actor;
#[cfg(feature = "read_slot")]
use environment::bytes;
#[cfg(any(feature = "arena", feature = "semaphore", feature = "vector"))]
use environment::internal;
#[cfg(feature = "arena")]
use environment::observe;
#[cfg(any(
    feature = "timer",
    feature = "await_race",
    feature = "sleep_teardown",
    feature = "read_slot",
    feature = "semaphore",
    feature = "reactor_loom"
))]
use environment::timer_periodic;
#[cfg(any(feature = "arena", feature = "vector"))]
use environment::trap_code;
use environment::{clock, coro_state, execution_context, fault, lifetime, resume, util};
#[cfg(feature = "executor")]
use executor::{actor, cont, internal};
#[cfg(feature = "executor")]
#[path = "../../../hew-runtime/src/internal/cont_tag.rs"]
mod cont_tag;
#[cfg(feature = "executor")]
#[path = "../../../hew-runtime/src/coro_exec.rs"]
mod coro_exec;
#[cfg(feature = "executor")]
mod executor;
#[cfg(feature = "executor_loom")]
pub use executor::verify as verify_executor;
fn set_last_error(_: impl AsRef<str>) {}
#[cfg(kani)]
mod checks;
#[cfg(all(kani, feature = "root_driver"))]
mod root_driver;
#[cfg(all(kani, feature = "root_driver"))]
use root_driver::cont;
#[cfg(feature = "loom")]
mod concurrent;
#[cfg(feature = "loom")]
mod lifecycle;
#[cfg(all(kani, feature = "root_driver"))]
#[path = "../../../hew-runtime/src/coro_root/poll.rs"]
mod root_poll;
#[cfg(feature = "loom")]
pub fn verify_concurrent_compositions(selection: Option<&str>) {
    if matches!(
        selection,
        Some("semaphore" | "supervisor" | "managed_budget" | "read_slot")
    ) {
        lifecycle::verify(selection.unwrap());
    } else {
        concurrent::verify(selection);
        if selection.is_none() {
            for name in ["semaphore", "supervisor", "managed_budget", "read_slot"] {
                lifecycle::verify(name);
            }
        }
    }
}

#[cfg(feature = "vector")]
mod vector_host;
#[cfg(feature = "vector")]
use vector_host::{code_owner, owned_value_budget, release_walker, string, trait_object};
#[cfg(feature = "vector")]
#[path = "../../../hew-runtime/src/vec.rs"]
pub mod vec;

#[cfg(feature = "channel")]
mod channel_host;
#[cfg(feature = "channel")]
use channel_host::{channel_common, code_owner, driver, release_walker, shutdown};
#[cfg(feature = "channel")]
#[path = "../../../hew-runtime/src/channel_core.rs"]
mod channel_core;
#[cfg(feature = "channel")]
#[path = "../../../hew-runtime/src/channel_envelope.rs"]
mod channel_envelope;
#[cfg(feature = "channel")]
mod channels;
#[cfg(feature = "channel_loom")]
pub use channels::verify;

#[cfg(feature = "fault_records")]
#[path = "../../../hew-runtime/src/exit_status/state.rs"]
mod fault_records;

#[cfg(feature = "fault_loom")]
pub fn verify_faults() {
    fault_records::formal::verify();
}

#[cfg(feature = "restart")]
#[path = "../../../hew-runtime/src/supervisor/restart_history.rs"]
mod restart_history;
#[cfg(feature = "restart")]
#[path = "../../../hew-runtime/src/supervisor/restart_policy.rs"]
mod restart_policy;

#[cfg(feature = "reactor_loom")]
mod reactor;
#[cfg(feature = "reactor")]
#[path = "../../../hew-runtime/src/reactor_wait.rs"]
mod reactor_wait;
#[cfg(feature = "reactor_loom")]
pub use reactor::verify as verify_reactor;

#[cfg(all(kani, feature = "reactor"))]
mod reactor_ordering;

#[cfg(feature = "periodic_loom")]
mod periodic;
#[cfg(feature = "periodic_loom")]
use periodic::{actor, actor_caller};
#[cfg(feature = "periodic_loom")]
#[path = "periodic_host.rs"]
mod timer_periodic;
#[cfg(feature = "periodic_loom")]
pub use periodic::verify as verify_periodic;

#[cfg(feature = "pipeline")]
pub mod pipeline_ledger;

#[cfg(feature = "one_shot")]
#[path = "../../../hew-runtime/src/timer_actor/protocol.rs"]
mod one_shot_protocol;
