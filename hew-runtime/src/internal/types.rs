//! Internal type definitions shared across runtime modules.

use crate::execution_context::HewExecutionContext;

/// Actor dispatch function signature (context-leading canonical).
///
/// `void (*dispatch)(HewExecutionContext *ctx, void *state, int msg_type, void *data, size_t data_size)`
pub type HewDispatchFn = unsafe extern "C-unwind" fn(
    ctx: *mut HewExecutionContext,
    state: *mut std::ffi::c_void,
    msg_type: i32,
    data: *mut std::ffi::c_void,
    data_size: usize,
);

/// Crash handler function signature for supervised actors.
///
/// Called by the supervisor when a child actor crashes, before the restart
/// policy is applied. Receives the execution context, the crash code
/// (trap kind integer), and the actor's current state pointer.
///
/// `crash_code` is i64 to match the `code: i64` field of `PanicInfo` in
/// `std/failure.hew`. The supervisor's internal plumbing tracks the code as
/// `c_int` and widens it to `i64` at the call site so the internal event
/// struct and the public C ABI (`hew_supervisor_notify_child_event`) stay
/// unchanged.
///
/// `void (*on_crash)(HewExecutionContext *ctx, int64_t crash_code, void *actor_state_ptr)`
pub type HewOnCrashFn = unsafe extern "C" fn(
    ctx: *mut HewExecutionContext,
    crash_code: i64,
    actor_state_ptr: *mut std::ffi::c_void,
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

/// Actor state (8-state CAS machine).
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
    /// Actor is parked in the WASM sleep queue (cooperative `sleep_ms` path).
    /// Distinguished from `Idle` so that message sends do not wake the actor
    /// early; messages queue in the mailbox and are processed when the timer
    /// fires.  Only the cooperative WASM scheduler sets/clears this state.
    Sleeping = 7,
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

// ── Trap error codes ─────────────────────────────────────────────────────
//
// Codes stored in `actor.error_code` to distinguish named exit kinds from
// raw OS signal numbers. OS signal numbers are < 32 on all supported
// platforms. Hew-specific codes start at 200 to leave room for POSIX signals
// and any future OS-specific ranges.
//
// Defined here (rather than in the native-only `supervisor` module) so that
// both native and WASM arena/dispatch paths can stamp the canonical code on
// an actor crash. The native supervisor module re-exports each constant for
// callers that import from `crate::supervisor::*`.

/// Error code stored in `actor.error_code` when an actor's arena cap is
/// exhausted. Produced by `hew_arena_malloc` routing through the longjmp
/// crash seam (native) or the unwind crash seam (WASM) when `cap > 0` and
/// an allocation would exceed it.
///
/// Distinct from any POSIX signal number (which are < 32 on all supported
/// platforms). Code 200 is reserved for this purpose and must not be reused
/// for any other exit kind.
pub const HEW_TRAP_HEAP_EXCEEDED: i32 = 200;

/// Error code recorded when a MIR `Terminator::Trap { kind: IntegerOverflow }`
/// fires inside an actor dispatch.
pub const HEW_TRAP_INTEGER_OVERFLOW: i32 = 201;

/// Error code recorded for `Terminator::Trap { kind: DivideByZero }`.
pub const HEW_TRAP_DIVIDE_BY_ZERO: i32 = 202;

/// Error code recorded for `Terminator::Trap { kind: SignedMinDivNegOne }`
/// — signed integer division of the minimum value by `-1`, whose mathematical
/// result is not representable in the operand width.
pub const HEW_TRAP_SIGNED_MIN_DIV_NEG_ONE: i32 = 203;

/// Error code recorded for `Terminator::Trap { kind: ShiftOutOfRange }`.
pub const HEW_TRAP_SHIFT_OUT_OF_RANGE: i32 = 204;

/// Error code recorded for `Terminator::Trap { kind: IndexOutOfBounds }`.
pub const HEW_TRAP_INDEX_OUT_OF_BOUNDS: i32 = 205;

/// Error code recorded when codegen checks the i32 return value of
/// `hew_actor_send_by_id` and finds it nonzero — the recipient was gone, the
/// queue was full, or the actor ID routed to a remote partition that rejected
/// the message.
pub const HEW_TRAP_ACTOR_SEND_FAILED: i32 = 206;

/// Convert a canonical Hew trap discriminator into the WASI process exit code
/// used when a trap escapes outside actor dispatch.
///
/// This is an untrusted wasm-to-host boundary: only Hew-owned trap
/// discriminators may become process exit statuses. Unknown values must return
/// `None` so the generated trailing `llvm.trap` remains the fail-closed sink.
#[must_use]
pub fn canonical_trap_wasi_exit_code(code: i32) -> Option<i32> {
    match code {
        HEW_TRAP_HEAP_EXCEEDED
        | HEW_TRAP_INTEGER_OVERFLOW
        | HEW_TRAP_DIVIDE_BY_ZERO
        | HEW_TRAP_SIGNED_MIN_DIV_NEG_ONE
        | HEW_TRAP_SHIFT_OUT_OF_RANGE
        | HEW_TRAP_INDEX_OUT_OF_BOUNDS
        | HEW_TRAP_ACTOR_SEND_FAILED => Some(code),
        _ => None,
    }
}

/// Named exit reason for a crashed actor.
///
/// Interprets the i32 `error_code` stored on a `HewActor` after a crash.
/// Callers can use `ExitReason::from_error_code(hew_actor_get_error(actor))`
/// to get a named reason.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExitReason {
    /// Actor's per-dispatch arena cap was exceeded (error code 200).
    HeapExceeded,
    /// Actor crashed on integer overflow (error code 201).
    IntegerOverflow,
    /// Actor crashed on `/` or `%` with a zero divisor (error code 202).
    DivideByZero,
    /// Actor crashed on signed `i{N}::MIN / -1` (error code 203).
    SignedMinDivNegOne,
    /// Actor crashed on a shift count outside `[0, width)` (error code 204).
    ShiftOutOfRange,
    /// Actor crashed on an out-of-bounds index (error code 205).
    IndexOutOfBounds,
    /// Actor crashed because `hew_actor_send_by_id` returned a nonzero status
    /// (error code 206).
    ActorSendFailed,
    /// Actor crashed with a hardware signal or via `hew_panic`. The raw
    /// signal number is preserved.
    Signal(i32),
    /// Actor stopped normally (`error_code` == 0).
    Normal,
}

impl ExitReason {
    /// Convert a raw `error_code` from `hew_actor_get_error` into a named
    /// `ExitReason`.
    #[must_use]
    pub fn from_error_code(code: i32) -> Self {
        match code {
            0 => ExitReason::Normal,
            HEW_TRAP_HEAP_EXCEEDED => ExitReason::HeapExceeded,
            HEW_TRAP_INTEGER_OVERFLOW => ExitReason::IntegerOverflow,
            HEW_TRAP_DIVIDE_BY_ZERO => ExitReason::DivideByZero,
            HEW_TRAP_SIGNED_MIN_DIV_NEG_ONE => ExitReason::SignedMinDivNegOne,
            HEW_TRAP_SHIFT_OUT_OF_RANGE => ExitReason::ShiftOutOfRange,
            HEW_TRAP_INDEX_OUT_OF_BOUNDS => ExitReason::IndexOutOfBounds,
            HEW_TRAP_ACTOR_SEND_FAILED => ExitReason::ActorSendFailed,
            sig => ExitReason::Signal(sig),
        }
    }
}
