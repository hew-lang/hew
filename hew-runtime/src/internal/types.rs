//! Internal type definitions shared across runtime modules.

use crate::execution_context::HewExecutionContext;

/// Actor dispatch function signature (context-leading canonical).
///
/// `void (*dispatch)(HewExecutionContext *ctx, void *state, int msg_type, void *data, size_t data_size, int32_t borrow_mode)`
///
/// # `borrow_mode` (P5-RX sub-stage 1 — dormant receive-ABI scaffolding)
///
/// Copy-mode receipt (`borrow_mode == 0`): `data`/`data_size` carry the
/// payload buffer the message node owns; the generated trampoline loads the
/// handler param value from `data` exactly as before.
///
/// Borrow-mode receipt (`borrow_mode == 1`): reserved for the envelope-mode
/// (aliased) receive path. In that mode `data` carries the
/// [`crate::mailbox::HewMsgEnvelope`] pointer and the trampoline resolves the
/// borrowed payload via `hew_msg_envelope_payload_ptr` rather than treating
/// `data` as the buffer directly. The single drop stays owned by
/// `hew_msg_envelope_release`.
///
/// This sub-stage wires the parameter and the trampoline's borrow-load path
/// but leaves it **dormant**: the scheduler passes `0` unconditionally and the
/// envelope-mode dispatch guard still fails closed before any envelope node
/// reaches dispatch, so the `borrow_mode == 1` arm is unreachable at runtime
/// until the live send/guard-removal sub-stages land.
pub type HewDispatchFn = unsafe extern "C-unwind" fn(
    ctx: *mut HewExecutionContext,
    state: *mut std::ffi::c_void,
    msg_type: i32,
    data: *mut std::ffi::c_void,
    data_size: usize,
    borrow_mode: i32,
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

/// Actor state CAS machine.
///
/// Discriminant `3` is the `Suspended` state: an actor whose current dispatch
/// suspended at a non-final `coro.suspend` and is parked against a readiness
/// source with a live continuation frame in [`crate::actor::HewActor`]'s
/// resume slot.  It is driven by the slice-4 poll/resume executor.
///
/// `3` was previously assigned to a `Blocked` variant that the v0.5 actor
/// model never transitioned into; the gap was preserved so that a cached
/// integer state value coming back from a stale profiler snapshot would map
/// to the catch-all "unknown" label rather than silently aliasing onto a
/// different state.  The slice-4 executor repurposes the gap (no other
/// discriminant moves, the `#[repr(i32)]` width is unchanged), so every
/// profiler / state-name decode now maps `3` to "Suspended" rather than
/// "unknown".  A stale snapshot that genuinely predates this repurpose can no
/// longer occur within one process: the value is only ever written by this
/// executor, which also owns the decode.
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HewActorState {
    Idle = 0,
    Runnable = 1,
    Running = 2,
    /// Actor dispatch suspended at a non-final `coro.suspend` and is parked
    /// against a readiness source.  The actor owns a live continuation frame
    /// (the [`crate::actor::HewActor::suspended_cont`] slot is non-null) and a
    /// per-continuation state tag that serializes resume/destroy.
    ///
    /// `Suspended` is deliberately **not** quiescent
    /// (`actor_free_state_is_quiescent` excludes it): a suspended actor holds a
    /// live frame and possibly a held resource, so a `hew_actor_free` caller
    /// must block through the `Suspended` window (destroying the parked
    /// continuation exactly once before freeing) rather than treating the
    /// actor as terminal.  Mirrors the `Sleeping`/`Crashing` non-quiescent
    /// discipline; see `LESSONS.md` rows `cleanup-all-exits` and
    /// `raii-null-after-move`.  Only the slice-4 executor sets/clears it.
    Suspended = 3,
    Stopping = 4,
    Crashed = 5,
    Stopped = 6,
    /// Actor is parked in the WASM sleep queue (cooperative `sleep_ms` path).
    /// Distinguished from `Idle` so that message sends do not wake the actor
    /// early; messages queue in the mailbox and are processed when the timer
    /// fires.  Only the cooperative WASM scheduler sets/clears this state.
    Sleeping = 7,
    /// Intermediate state during native crash recovery.  Mirrors the
    /// `Stopping → Stopped` two-step: the worker that owns the activation
    /// transitions `Running → Crashing` (or `Stopping → Crashing` when the
    /// handler self-stopped before panicking — crash dominates the pending
    /// stop) immediately after the crash signal is caught, finalises
    /// per-activation cleanup (arena reset, msg-node free, late
    /// crash-reply), and only then publishes the terminal `Crashed` state
    /// via `hew_actor_trap`.
    ///
    /// `actor_free_state_is_quiescent` deliberately does **not** include
    /// `Crashing`, so any thread waiting on the actor (e.g. a
    /// `hew_actor_free` caller spinning on the actor state) blocks through
    /// the `Crashing` window and cannot free `a.arena`/`a.mailbox` out
    /// from under the worker.  See `LESSONS.md` rows `cleanup-all-exits`
    /// and `raii-null-after-move`.
    Crashing = 8,
}

/// Per-continuation lifecycle tag the slice-4 executor CAS-transitions to
/// serialize resume and destroy against a single parked `HewCont` handle.
///
/// The continuation REPRESENTATION (`cont.rs`) is fail-closed on a NULL handle
/// but has no scheduler state, so it cannot detect a *double-resume*, a
/// *destroy-after-destroy*, or a *concurrent resume+destroy* on a non-null
/// dangling handle — those are exactly the executor's job (FG1, FG2, FG4).
/// This tag is the executor's serialization point: every transition is a CAS
/// from a single expected current tag, so an unexpected current tag fails
/// closed (the caller refuses the operation) rather than corrupting memory.
///
/// The per-actor lock (`hew_actor_state_lock_*`) is RELEASED while an actor is
/// `Suspended` (the suspend edge must release it so senders do not deadlock),
/// so this tag — not the actor lock — is what serializes resume vs destroy on
/// the handle.
///
/// Lifecycle (the only legal transitions):
/// ```text
///   (slot empty) --park--> Parked
///   Parked --resume--> Resuming --(Pending)--> Parked   (suspended again)
///   Parked --resume--> Resuming --(Ready)----> Done --destroy--> Destroyed
///   Parked --abandon--> Destroyed                    (cancel — skips Done)
///   Done   --destroy--> Destroyed
/// ```
/// A second `resume` (tag already `Resuming`/`Done`/`Destroyed`) refuses; a
/// second `destroy` (tag already `Destroyed`) refuses. `Destroyed` is terminal
/// and the slot is nulled in the same critical section (FG4: no use-after-
/// destroy).
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ContTag {
    /// No continuation is parked on the actor (the slot is null). This is the
    /// initial / quiescent tag and the value a fresh `HewActor` zero-inits to.
    Empty = 0,
    /// A continuation is parked against a readiness source, suspended at a
    /// non-final `coro.suspend`, awaiting resume. The handle is live and
    /// `resume` may transition it to `Resuming`.
    Parked = 1,
    /// The executor is currently driving `hew_cont_resume` on the handle. No
    /// concurrent `resume` or `destroy` may proceed; both refuse against this
    /// tag (FG2). Returns to `Parked` on `Pending` or advances to `Done` on
    /// `Ready`.
    Resuming = 2,
    /// The continuation reached its final suspend (`ResumePoll::Ready`). It
    /// awaits exactly one `destroy`. `resume` refuses against this tag.
    /// Abandoned continuations skip this state and go directly to `Destroyed`
    /// via a `Parked → Destroyed` CAS in `destroy_parked`.
    Done = 3,
    /// The continuation has been destroyed exactly once (FG1). Terminal:
    /// every further `resume` and `destroy` refuses (FG2/FG4), and the actor's
    /// `suspended_cont` slot was nulled in the same critical section as this
    /// transition so no later activation reads the freed handle.
    Destroyed = 4,
}

impl ContTag {
    /// Convert from the raw `i32` representation. Returns `None` for any value
    /// outside the legal tag range — a fail-closed decode for the executor's
    /// CAS, which only ever compares against a known-good expected tag.
    #[must_use]
    pub fn from_i32(v: i32) -> Option<Self> {
        match v {
            0 => Some(Self::Empty),
            1 => Some(Self::Parked),
            2 => Some(Self::Resuming),
            3 => Some(Self::Done),
            4 => Some(Self::Destroyed),
            _ => None,
        }
    }
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
    /// The inbound request payload could not be deserialized into a value in
    /// the receiving node's address space (no codec registered for the
    /// `msg_type`, or a malformed / truncated wire payload). The handler was
    /// never dispatched — fail-closed rather than delivering garbage.
    DecodeFailure = 13,
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

/// Error code recorded for `Terminator::Trap { kind: MachineDispatchUnreachable }`.
///
/// Fires when a synthesised `<Name>__step` machine dispatch function reaches a
/// state×event combination that has no declared transition. Per LESSONS
/// `fail-closed-not-pretend` (P0), the trap is the substrate's fail-closed
/// surface — HIR exhaustiveness guarantees this code is dead in well-typed
/// programs; the trap proves the property at runtime.
pub const HEW_TRAP_MACHINE_DISPATCH_UNREACHABLE: i32 = 207;

/// Discriminator for a `Terminator::Trap { kind: ExhaustivenessFallthrough }`.
///
/// Fires when a `match`-expression dispatch chain falls through every arm at
/// runtime. The type checker pre-gates this code path by rejecting
/// non-exhaustive enum matches; this trap is the belt-and-braces fail-closed
/// surface mandated by LESSONS `match-fail-closed` (P0) — dead in well-typed
/// programs, observable if a producer regression ever lets an unreached value
/// reach the dispatch chain.
pub const HEW_TRAP_EXHAUSTIVENESS_FALLTHROUGH: i32 = 208;

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
        | HEW_TRAP_ACTOR_SEND_FAILED
        | HEW_TRAP_MACHINE_DISPATCH_UNREACHABLE
        | HEW_TRAP_EXHAUSTIVENESS_FALLTHROUGH => Some(code),
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
    /// Actor crashed because a synthesised machine `<Name>__step` dispatch
    /// reached a state×event combination with no transition (error code 207).
    MachineDispatchUnreachable,
    /// Actor crashed because a `match` expression dispatch chain fell through
    /// every arm at runtime (error code 208). The checker pre-gates this with
    /// exhaustiveness errors; reaching this trap indicates a producer-side
    /// regression in match-arm lowering.
    ExhaustivenessFallthrough,
    /// Actor crashed with a hardware signal or via `hew_panic`. The raw
    /// signal number is preserved.
    Signal(i32),
    /// Actor stopped normally (`error_code` == 0).
    Normal,
}

impl ExitReason {
    /// Return the canonical slug name for this exit reason.
    ///
    /// Stable string identifiers consumed by the profiler/observe event
    /// surface (`/api/crashes` JSON `trap_kind` field). The "Signal" variant
    /// collapses every OS-signal exit into a single bucket; downstream
    /// consumers can still inspect the raw `signal` field for the signal
    /// number. "Normal" appears when an actor stops cleanly (no trap).
    #[must_use]
    pub const fn trap_kind_name(self) -> &'static str {
        match self {
            ExitReason::HeapExceeded => "HeapExceeded",
            ExitReason::IntegerOverflow => "IntegerOverflow",
            ExitReason::DivideByZero => "DivideByZero",
            ExitReason::SignedMinDivNegOne => "SignedMinDivNegOne",
            ExitReason::ShiftOutOfRange => "ShiftOutOfRange",
            ExitReason::IndexOutOfBounds => "IndexOutOfBounds",
            ExitReason::ActorSendFailed => "ActorSendFailed",
            ExitReason::MachineDispatchUnreachable => "MachineDispatchUnreachable",
            ExitReason::ExhaustivenessFallthrough => "ExhaustivenessFallthrough",
            ExitReason::Signal(_) => "Signal",
            ExitReason::Normal => "Normal",
        }
    }

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
            HEW_TRAP_MACHINE_DISPATCH_UNREACHABLE => ExitReason::MachineDispatchUnreachable,
            HEW_TRAP_EXHAUSTIVENESS_FALLTHROUGH => ExitReason::ExhaustivenessFallthrough,
            sig => ExitReason::Signal(sig),
        }
    }
}
