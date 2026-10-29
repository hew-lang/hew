//! Standard primitives; the verification host instruments the same source.
pub(crate) use std::hint::spin_loop;
pub(crate) use std::sync::atomic::{AtomicBool, AtomicI32, AtomicPtr, AtomicU64, AtomicUsize};
pub(crate) use std::sync::{Condvar, Mutex, MutexGuard};

// Periodic cancellation relies on a total order across its two flags.
pub(crate) use std::sync::atomic::AtomicBool as SeqCstFlag;
