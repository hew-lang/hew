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

impl HewTaskState {
    /// Convert from the raw `i32` representation.
    ///
    /// Returns `None` for values outside the valid range.
    #[must_use]
    pub fn from_i32(v: i32) -> Option<Self> {
        match v {
            0 => Some(Self::Ready),
            1 => Some(Self::Running),
            2 => Some(Self::Suspended),
            3 => Some(Self::Done),
            _ => None,
        }
    }
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

/// Typed failure reason for an ask (local or remote).
///
/// Written to a thread-local slot whenever `hew_node_api_ask` or
/// `hew_actor_ask` / `hew_actor_ask_timeout` returns `NULL`.
/// Remote callers retrieve the discriminant via `hew_node_ask_take_last_error`.
/// Local callers retrieve it via `hew_actor_ask_take_last_error`.
///
/// Values are stable across releases; do not reorder or reuse.
///
/// Defined here (rather than in `hew_node`) so that both native and WASM
/// targets can reference it without importing the native-only `hew_node` module.
#[repr(i32)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AskError {
    /// No failure — used to reset the slot after a successful ask.
    None = 0,
    /// No active node is installed in this process.
    NodeNotRunning = 1,
    /// The target PID could not be mapped to a connection.
    RoutingFailed = 2,
    /// Wire-encoding the request envelope failed.
    EncodeFailed = 3,
    /// Sending the encoded envelope over the connection failed.
    SendFailed = 4,
    /// The reply did not arrive before the deadline elapsed.
    Timeout = 5,
    /// The underlying connection was dropped before the reply arrived.
    ConnectionDropped = 6,
    /// The reply payload size did not match the expected reply type size.
    PayloadSizeMismatch = 7,
    /// The remote node's inbound ask worker pool is at capacity.
    ///
    /// The actor was never dispatched. The ask may be retried after a
    /// brief back-off. This is distinct from [`ConnectionDropped`]: the
    /// connection is healthy, but the remote end shed load deliberately.
    WorkerAtCapacity = 8,
    /// The target actor was stopped, or the mailbox rejected the send
    /// (e.g. actor not found, mailbox closed, or `ErrClosed` in WASM).
    ActorStopped = 9,
    /// The target actor's mailbox was full and the message was dropped.
    ///
    /// Only raised when the mailbox overflow policy is `DropNew` or `Fail`.
    MailboxFull = 10,
    /// The ask was orphaned: the actor's mailbox was torn down before the
    /// handler called `hew_reply`.
    ///
    /// This happens when an actor stops mid-dispatch without replying.  The
    /// reply channel's sender-side reference is retired by the mailbox teardown
    /// path, which signals the waiter with an empty null payload.
    ///
    /// DROP-SAFETY: the orphaned flag is set on the reply channel before the
    /// null sentinel is published, so the waiter always observes it correctly.
    OrphanedAsk = 11,
    /// WASM cooperative ask only: no runnable work remains in the scheduler,
    /// so the ask loop cannot make further progress before returning control
    /// to the host.
    ///
    /// This is distinct from [`Timeout`]: the deadline has not necessarily
    /// expired, but the scheduler is idle and cannot advance the ask.
    NoRunnableWork = 12,
}
