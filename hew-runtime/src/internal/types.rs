//! Internal type definitions shared across runtime modules.

/// Actor dispatch function signature (4-param canonical).
///
/// `void (*dispatch)(void *state, int msg_type, void *data, size_t data_size)`
pub type HewDispatchFn = unsafe extern "C" fn(
    state: *mut std::ffi::c_void,
    msg_type: i32,
    data: *mut std::ffi::c_void,
    data_size: usize,
);

/// Overflow policy for bounded mailboxes.
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HewOverflowPolicy {
    Block = 0,
    DropNew = 1,
    DropOld = 2,
    Fail = 3,
    Coalesce = 4,
}

/// Actor state (7-state CAS machine).
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HewActorState {
    Idle = 0,
    Runnable = 1,
    Running = 2,
    Blocked = 3,
    Stopping = 4,
    Crashed = 5,
    Stopped = 6,
}

/// Error codes returned by runtime functions.
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HewError {
    Ok = 0,
    ErrMailboxFull = -1,
    ErrActorStopped = -2,
    ErrTimeout = -3,
    ErrClosed = -4,
    ErrOom = -5,
}

/// Task state (cooperative scheduling within an actor).
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HewTaskState {
    Ready = 0,
    Running = 1,
    Suspended = 2,
    Done = 3,
}

/// Task error codes.
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HewTaskError {
    None = 0,
    Cancelled = 1,
    Timeout = 2,
    Panic = 3,
}

/// Restart policy for supervised actors.
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HewRestartPolicy {
    Permanent = 0,
    Transient = 1,
    Temporary = 2,
}

/// Restart strategy for supervisors.
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HewRestartStrategy {
    /// Restart only the failed child.
    OneForOne = 0,
    /// Restart all children when any child fails.
    OneForAll = 1,
    /// Restart the failed child and all children started after it.
    RestForOne = 2,
}

/// Circuit breaker states for supervised actors.
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HewCircuitBreakerState {
    /// Normal operation, restarts are allowed.
    Closed = 0,
    /// Circuit is open, restarts are blocked.
    Open = 1,
    /// Circuit allows one probe restart to test if the issue is resolved.
    HalfOpen = 2,
}
