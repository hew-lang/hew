//! Production callback ownership and admission gates without hosting APIs.
#[path = "../../../hew-runtime/src/waker_target.rs"]
mod target;
pub use target::{HewWaker, OwnedWaker};
#[cfg(feature = "channel")]
pub(crate) use target::WakeTarget;

#[path = "../../../hew-runtime/src/ready_state.rs"]
mod state;
pub(crate) mod scheduled {
    pub(crate) use super::state::ReadyState;
}

#[cfg(feature = "loom")]
mod modeled {
    use super::state::AtomicByte;
    use loom::sync::atomic::AtomicU8;
    use std::sync::atomic::Ordering;
    impl AtomicByte for AtomicU8 {
        fn load(&self, order: Ordering) -> u8 {
            self.load(order)
        }
        fn fetch_or(&self, value: u8, order: Ordering) -> u8 {
            self.fetch_or(value, order)
        }
        fn fetch_and(&self, value: u8, order: Ordering) -> u8 {
            self.fetch_and(value, order)
        }
        fn compare_exchange(
            &self,
            old: u8,
            new: u8,
            success: Ordering,
            failure: Ordering,
        ) -> Result<u8, u8> {
            self.compare_exchange(old, new, success, failure)
        }
        fn compare_exchange_weak(
            &self,
            old: u8,
            new: u8,
            success: Ordering,
            failure: Ordering,
        ) -> Result<u8, u8> {
            self.compare_exchange_weak(old, new, success, failure)
        }
    }
}

#[cfg(feature = "loom")]
pub(crate) fn gate(queued: bool) -> state::ReadyState<loom::sync::atomic::AtomicU8> {
    state::ReadyState::from_atomic(loom::sync::atomic::AtomicU8::new(u8::from(queued)))
}

#[cfg(feature = "semaphore")]
pub mod blocking {
    pub struct Readiness;
    impl Readiness {
        pub fn new() -> (Self, super::OwnedWaker) {
            panic!("blocking adapter outside model")
        }
        pub fn wait(&self) {
            panic!("blocking adapter outside model")
        }
    }
}

#[cfg(feature = "channel")]
pub(crate) fn wait_for_peer<'a, T>(_: &'a crate::runtime_sync::Mutex<T>, _: &crate::runtime_sync::Condvar, _: crate::runtime_sync::MutexGuard<'a,T>, _: std::time::Duration) -> crate::runtime_sync::MutexGuard<'a,T> {
    panic!("channel: blocking driver path outside this model")
}
