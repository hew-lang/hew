//! Primitive boundary: normal library types for Kani; Loom instruments the
//! mutex and publication atomics for the concurrent compositions.
#[cfg(any(
    feature = "loom",
    feature = "executor_loom",
    feature = "channel_loom",
    feature = "fault_loom",
    feature = "reactor_loom",
    feature = "periodic_loom"
))]
pub use loom::sync::atomic::{AtomicBool, AtomicI32, AtomicPtr, AtomicU64, AtomicUsize};
#[cfg(any(
    feature = "loom",
    feature = "executor_loom",
    feature = "channel_loom",
    feature = "fault_loom",
    feature = "reactor_loom",
    feature = "periodic_loom"
))]
pub use loom::sync::{Condvar, Mutex, MutexGuard};
#[cfg(not(any(
    feature = "loom",
    feature = "executor_loom",
    feature = "channel_loom",
    feature = "fault_loom",
    feature = "reactor_loom",
    feature = "periodic_loom"
)))]
pub use std::sync::atomic::{AtomicBool, AtomicI32, AtomicPtr, AtomicU64, AtomicUsize};
#[cfg(not(any(
    feature = "loom",
    feature = "executor_loom",
    feature = "channel_loom",
    feature = "fault_loom",
    feature = "reactor_loom",
    feature = "periodic_loom"
)))]
pub use std::sync::{Condvar, Mutex, MutexGuard};

#[cfg(feature = "periodic_loom")]
pub use loom::thread::yield_now as spin_loop;
#[cfg(not(feature = "periodic_loom"))]
pub use std::hint::spin_loop;

#[cfg(feature = "periodic_loom")]
pub use crate::periodic::ScBool as SeqCstFlag;
#[cfg(not(feature = "periodic_loom"))]
pub use std::sync::atomic::AtomicBool as SeqCstFlag;
