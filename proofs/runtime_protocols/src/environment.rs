//! Explicit host boundaries. Unmodeled execution paths fail closed.
use std::sync::atomic::{AtomicI64, AtomicPtr, AtomicUsize, Ordering};

pub mod util {
    use crate::runtime_sync::{Condvar, Mutex, MutexGuard};
    pub trait MutexExt<T> {
        fn lock_or_recover(&self) -> MutexGuard<'_, T>;
    }
    impl<T> MutexExt<T> for Mutex<T> {
        fn lock_or_recover(&self) -> MutexGuard<'_, T> {
            self.lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
        }
    }
    pub trait CondvarExt {
        fn wait_or_recover<'a, T>(&self, guard: MutexGuard<'a, T>) -> MutexGuard<'a, T>;
    }
    impl CondvarExt for Condvar {
        fn wait_or_recover<'a, T>(&self, _: MutexGuard<'a, T>) -> MutexGuard<'a, T> {
            panic!("blocking waits are outside the verification host")
        }
    }
}

pub mod clock {
    use super::*;
    pub static NOW: AtomicI64 = AtomicI64::new(0);
    pub unsafe fn hew_instant_now() -> i64 {
        NOW.load(Ordering::Relaxed)
    }
    pub unsafe extern "C" fn hew_now_ms() -> u64 {
        (NOW.load(Ordering::Relaxed) as u64) / 1_000_000
    }
}

pub mod actor {
    #[derive(Debug)]
    pub struct HewPanic {
        pub code: i32,
    }
    pub fn hew_actor_current_id_silent() -> i64 { -1 }
    pub struct HewActor {
        pub id: usize,
    }
}

pub mod lifetime {
    pub mod live_actors {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub struct ActorIncarnation(pub usize);
        impl ActorIncarnation {
            pub unsafe fn of(actor: *mut crate::actor::HewActor) -> Self {
                Self(if actor.is_null() { 0 } else { (*actor).id })
            }
        }
    }
}

#[cfg(not(any(
    feature = "cancellation_delivery",
    feature = "await_race",
    feature = "read_slot"
)))]
type Token = ();
#[cfg(any(
    feature = "cancellation_delivery",
    feature = "await_race",
    feature = "read_slot"
))]
type Token = crate::cancel_token::HewCancellationToken;

#[cfg(feature = "arena")]
type Arena = crate::arena::ActorArena;
#[cfg(not(feature = "arena"))]
type Arena = ();
pub mod execution_context {
    use super::*;
    pub struct Context {
        pub arena: *mut Arena,
        pub cancel_token: *mut Token,
        pub request_deadline: crate::request_deadline::RequestDeadline,
    }
    // Model the installed thread-local pointer directly. Fixtures install it
    // before spawning workers and clear it before those workers execute.
    pub struct ContextSlot(std::cell::UnsafeCell<*mut Context>);
    // SAFETY: this fixture's writes are serialized; worker phases never mutate it.
    unsafe impl Sync for ContextSlot {}
    impl ContextSlot {
        pub fn load(&self, _: Ordering) -> *mut Context {
            unsafe { *self.0.get() }
        }
        pub fn store(&self, value: *mut Context, _: Ordering) {
            unsafe {
                *self.0.get() = value;
            }
        }
    }
    pub static CURRENT: ContextSlot = ContextSlot(std::cell::UnsafeCell::new(std::ptr::null_mut()));
    pub fn require_current_context() -> *mut Context {
        current_context()
    }
    pub fn current_context_can_unwind() -> bool {
        false
    }
    pub fn current_context() -> *mut Context {
        CURRENT.load(Ordering::Relaxed)
    }
}

pub mod resume {
    use super::*;
    pub static WAKES: AtomicUsize = AtomicUsize::new(0);
    pub fn enqueue_resume_by_incarnation(actor: crate::lifetime::live_actors::ActorIncarnation) {
        if actor.0 != 0 {
            WAKES.fetch_add(1, Ordering::Relaxed);
        }
    }
}

pub mod fault {
    pub struct HewFault;
    impl HewFault {
        pub fn with_message(_: i32, _: String) -> Self {
            panic!("blocking fault path outside host")
        }
    }
    pub unsafe fn hew_fault_trap(_: i32, _: *mut HewFault) {
        panic!("blocking trap outside host")
    }

    pub const HEW_FAULT_DEADLINE: i32 = -2;
    pub const HEW_FAULT_RACE_LOST: i32 = -3;
}

pub mod coro_state {
    #[cfg(feature = "channel")]
    pub use crate::channel_host::ActorStreamCancellation;
    #[cfg(feature = "vector")]
    pub struct HewCoroState;
    #[cfg(all(kani, feature = "root_driver"))]
    pub use crate::root_driver::{
        hew_coro_state_private_status, hew_coro_state_status, HewCoroState,
    };
    #[repr(i32)]
    pub enum CoroStatus {
        Pending = 0,
        Complete = 1,
        Fault = 2,
        Cancelled = 3,
        Yielded = 4,
    }
}

#[cfg(any(
    feature = "timer",
    feature = "await_race",
    feature = "sleep_teardown",
    feature = "read_slot",
    feature = "semaphore",
    feature = "reactor_loom"
))]
pub mod timer_periodic {
    use super::*;
    pub static WHEEL: AtomicPtr<crate::timer_wheel::HewTimerWheel> =
        AtomicPtr::new(std::ptr::null_mut());
    pub fn global_wheel() -> *mut crate::timer_wheel::HewTimerWheel {
        WHEEL.load(Ordering::Relaxed)
    }
}

#[cfg(any(feature = "arena", feature = "semaphore", feature = "vector"))]
pub mod internal {
    pub mod types {
        pub const HEW_TRAP_INDEX_OUT_OF_BOUNDS: i32 = 205;
        pub const HEW_TRAP_HEAP_EXCEEDED: i32 = 200;
        pub const HEW_TRAP_USER_PANIC: i32 = 1;
    }
}
#[cfg(any(feature = "arena", feature = "vector"))]
pub mod trap_code {
    pub unsafe fn write_stderr(_: &[u8]) {}
    pub fn fmt_decimal_usize(_: usize, _: &mut [u8; 20]) -> &[u8] {
        panic!("unmodeled diagnostic")
    }
    pub unsafe fn runtime_bounds_trap(_: i32) -> ! {
        panic!("vector: unexpected bounds trap")
    }
    pub fn stamp_current_actor_error_code(_: i32) -> bool {
        true
    }
}
#[cfg(feature = "arena")]
pub mod observe {
    pub fn record_arena_reset() {}
}

#[cfg(feature = "read_slot")]
pub mod bytes {
    pub use crate::bytes_value::BytesTriple;
    pub unsafe fn hew_bytes_drop(_: *mut u8) {
        panic!("payload buffers are outside the readiness-slot contract");
    }
}
