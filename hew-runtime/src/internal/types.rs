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
/// Actor dispatch trampoline ABI (D-A.2, R326/R327, W6.007).
///
/// Returns the dispatch's **suspend outcome**, encoded as a nullable
/// continuation handle:
/// - `null` — the handler ran to completion (the run-to-completion path: it did
///   NOT suspend). This is what EVERY handler returns today — no source
///   construct produces a `coro.suspend` in production yet (the suspend
///   substrate is dormant). The scheduler treats `null` exactly as the
///   pre-D-A.2 void return.
/// - non-null — the handler suspended at a non-final `coro.suspend`; the
///   returned pointer is the `coro.begin` frame handle (`HewCont`). The
///   scheduler parks it against the executor (release the per-actor lock, CAS
///   `Running → Suspended`) and resumes it later via `hew_cont_resume`.
///
/// The handle surfaces through the RETURN VALUE rather than a `HewActor` field:
/// the suspend edge stores it into the existing `suspended_cont` slot via
/// `coro_exec::finish_park`, so no new actor field / offset-mirror change is
/// needed (the dispatch fn pointer is registered as an opaque `ptr` in codegen,
/// so widening the return type does not change the spawn-registration ABI).
pub type HewDispatchFn = unsafe extern "C-unwind" fn(
    ctx: *mut HewExecutionContext,
    state: *mut std::ffi::c_void,
    msg_type: i32,
    data: *mut std::ffi::c_void,
    data_size: usize,
    borrow_mode: i32,
) -> *mut std::ffi::c_void;

/// Crash handler function signature for supervised actors.
///
/// Called by the supervisor when a child actor crashes, before the restart
/// policy is applied. Receives the execution context, the crash code
/// (trap kind integer), the crash message (diagnostic string, may be null),
/// and the actor's current state pointer; returns the hook's `CrashAction`
/// control decision as an `i32` tag.
///
/// # Arguments
///
/// - `crash_code: i64` — the trap-kind integer captured from the crashed
///   actor's `error_code` slot. `i64` matches the `code: i64` field of
///   `CrashInfo` in `std/failure.hew`. The supervisor's internal plumbing
///   tracks the code as `c_int` and widens it to `i64` at the call site so
///   the internal event struct and the public C ABI
///   (`hew_supervisor_notify_child_event`) stay unchanged.
/// - `crash_message: *const c_char` — a NUL-terminated diagnostic string for
///   the `CrashInfo.message` field, or null when no message is available. The
///   callee (the codegen-emitted `__on_crash` body) BORROWS this pointer to
///   construct its own owned `CrashInfo.message` string; ownership of the
///   underlying buffer stays with the caller (the supervisor), which is
///   responsible for its lifetime across the call. Null is rendered as the
///   empty string by the codegen prologue.
///
/// # Return value
///
/// The hook's `CrashAction` decision, returned as the `CrashAction`
/// tagged-union value by its NATURAL enum-return ABI. The codegen-emitted
/// `__on_crash` returns the `CrashAction` LLVM struct
/// (`%CrashAction = { i8, [1 x i8] }`); [`HewCrashActionAbi`] mirrors that
/// layout `#[repr(C)]` so the supervisor reads the variant `tag` byte directly.
/// Returning the value naturally (rather than extracting a tag in MIR) lets
/// EVERY return position — the tail expression and any explicit
/// `return CrashAction::X;` — lower identically through the existing
/// enum-return path. The tag is `Restart = 0`, `Escalate = 1`, `Kill = 2`
/// (declaration order); the supervisor decodes it, treating any value outside
/// `0..=2` fail-closed as `Restart` — see
/// `hew-runtime/src/supervisor.rs::apply_restart`.
///
/// `HewCrashActionAbi (*on_crash)(HewExecutionContext *ctx, int64_t crash_code, const char *crash_message, void *actor_state_ptr)`
pub type HewOnCrashFn = unsafe extern "C" fn(
    ctx: *mut HewExecutionContext,
    crash_code: i64,
    crash_message: *const std::ffi::c_char,
    actor_state_ptr: *mut std::ffi::c_void,
) -> HewCrashActionAbi;

/// `#[repr(C)]` mirror of the `CrashAction` tagged-union LLVM struct
/// (`%CrashAction = { i8, [1 x i8] }`) — the by-value return of
/// [`HewOnCrashFn`].
///
/// A `CrashAction` is a payload-free 3-variant enum; its tagged-union layout is
/// a 1-byte discriminant tag plus a 1-byte zero-sized-payload pad (codegen rounds
/// the 2-bit tag up to `i8`). The supervisor reads [`Self::tag`] to recover the
/// variant. ABI-critical: this layout MUST match what codegen emits for the
/// `CrashAction` enum — a mismatch is wrong-code at the FFI boundary.
///
/// ONLY FIELD 0 (`tag`) IS ABI-LOAD-BEARING. Cross-target correctness rests on
/// the supervisor reading `tag` (field 0) and nothing else: a 2-byte all-integer
/// struct returns its field 0 in the low byte of the return register on both
/// `x86_64` System V (AL of RAX) and ARM64 `AAPCS64` (low byte of x0) under both
/// the LLVM raw-aggregate producer and the `#[repr(C)]` consumer, so the decoded
/// `tag` always agrees. The codegen producer uses LLVM's raw aggregate return,
/// which scatters `payload_pad` to a DIFFERENT register than the C-ABI consumer
/// expects and leaves it undef — so `payload_pad` is GARBAGE across the boundary.
/// Do NOT read `payload_pad`, derive `PartialEq`/`Eq`-based comparisons of the
/// whole struct across the FFI boundary, or widen this struct without re-checking
/// the producer/consumer register agreement on every target (Windows x64 MSVC in
/// particular is unverified — a 2-byte struct returns in RAX there so it is very
/// likely fine, but flag it if Windows becomes a target). The fail-closed
/// `0..=2 → Restart` default in `apply_restart` is a second layer of protection.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HewCrashActionAbi {
    /// Variant discriminant: `Restart = 0`, `Escalate = 1`, `Kill = 2`. The ONLY
    /// ABI-load-bearing field — read via [`Self::tag_i32`]; see the type doc.
    pub tag: u8,
    /// Zero-sized-payload pad matching the LLVM `[1 x i8]` payload slot.
    /// ABI-required (keeps the struct's 2-byte size in sync with the enum
    /// layout); NOT read by the supervisor and GARBAGE across the FFI boundary
    /// (the producer scatters it to a different register and leaves it undef).
    pub payload_pad: [u8; 1],
}

impl HewCrashActionAbi {
    /// The variant tag as an `i32`, for decoding against the
    /// `CRASH_ACTION_*` supervisor constants.
    #[must_use]
    pub const fn tag_i32(self) -> i32 {
        self.tag as i32
    }
}

/// Lifecycle wrapper function signature for supervised actors.
///
/// Codegen emits one `__hew_lifecycle_<Actor>(*mut HewActor)` per child actor
/// type that declares an `init` or a `#[on(start)]` hook. The supervisor calls
/// it on the newly spawned child inside `restart_child_from_spec` — the single
/// firing site that covers BOTH the initial supervised spawn and every
/// supervisor-triggered restart — so a supervised actor runs its `init()` /
/// `#[on(start)]` exactly once per incarnation, identically to a directly
/// spawned actor.
///
/// The wrapper takes only the actor pointer: it builds its own execution
/// context from the actor (mirroring the direct-spawn lifecycle path), acquires
/// the actor state lock, runs `__init` then `__on_start`, releases the lock, and
/// registers the terminate trampoline. It is zero-arg — supervised init
/// *parameters* are seeded into the spec's `init_state` template, not threaded
/// through this wrapper.
///
/// `void (*lifecycle)(HewActor *actor)`
pub type HewLifecycleFn = unsafe extern "C" fn(actor: *mut crate::actor::HewActor);

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
    /// A send/ask/by-id targeted an actor owned by a different runtime than the
    /// caller's. The boundary fails closed (refuse + diagnose) rather than
    /// routing a foreign pointer. Never returned in a single-runtime program,
    /// where every actor carries the same `RuntimeId`. Additive: existing codes
    /// are unchanged.
    ErrForeignRuntime = -6,
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
    /// The target actor's partition is unreachable or the network is
    /// partitioned. Distinct from `ConnectionDropped`: the node is reachable
    /// but the routing topology has isolated the target partition.
    Partition = 14,
    /// The target actor reference is stale: the actor existed at bind time
    /// but has since stopped or been replaced by a newer generation.
    StaleRef = 15,
    /// The ask was cancelled by the caller before a reply was received.
    Cancelled = 16,
    /// The local node is shutting down; no further asks can be dispatched.
    LocalShutdown = 17,
    /// The remote node's wire protocol version is incompatible with the
    /// caller's version. The ask was refused before dispatch.
    VersionMismatch = 18,
    /// The caller is not authorized to ask the target actor. The remote
    /// capability check refused the request before dispatch.
    Unauthorized = 19,
    /// The remote node's inbound ask pipeline is applying backpressure:
    /// the caller should back off and retry. Distinct from `WorkerAtCapacity`
    /// (pool exhaustion) — this is a deliberate flow-control signal.
    Backpressure = 20,
    /// The monitor subscription for the ask's reply was lost before the
    /// reply arrived (e.g. the monitor actor was stopped or evicted).
    MonitorLost = 21,
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

/// Error code recorded when a synthesised codegen helper hits an internal
/// compiler-invariant violation that is dead code in correctly-compiled
/// programs: a module-init regex literal that failed to compile (patterns are
/// validated by the type-checker) or an enum-clone helper synthesised for an
/// opaque-handle-bearing variant whose clone direction was never meant to be
/// reachable. Both codegen sites emit `hew_trap_with_code(209)` as the
/// fail-closed sink so the path traps loudly rather than aliasing a handle or
/// proceeding past a malformed pattern.
///
/// The single source of truth for the codegen literal: `hew-codegen-rs`
/// imports this constant so a renumber here fails the codegen build closed.
pub const HEW_TRAP_MODULE_INIT_REGEX_FAILED: i32 = 209;

/// Error code recorded when a wire-type `Type.decode(bytes)` is handed bytes
/// that are not a valid encoding of the type. The deserialize thunk frees its
/// partial reconstruction and returns null; the codegen call site branches on
/// that null and emits `hew_trap_with_code(210)` so malformed/adversarial input
/// fails CLOSED (a clean trap) instead of dereferencing the null return and
/// segfaulting. Per LESSONS `fail-closed-not-pretend` (P0): the boundary refuses
/// loudly rather than crashing on a null deref.
///
/// The single source of truth for the codegen literal: `hew-codegen-rs` imports
/// this constant so a renumber here fails the codegen build closed.
pub const HEW_TRAP_WIRE_DECODE_FAILED: i32 = 210;

/// Error code recorded when a `join{}` branch's reply resolves without a
/// value. The join surface awaits every branch and binds a tuple — it has no
/// per-branch `Result` binding — so a branch failure is fail-closed
/// (§4.11.2: cancel the remaining branches and propagate). Pre-fix the null
/// branch emitted a bare `llvm.trap` with no code and no diagnostic; the
/// failure now traps with THIS code after `hew_join_branch_failed` names the
/// classified cause (actor stopped, cancelled, handler trapped, or reply
/// allocation failure — the observable-honesty axiom applied to the join
/// site).
///
/// The single source of truth for the codegen literal: `hew-codegen-rs`
/// imports this constant so a renumber here fails the codegen build closed.
pub const HEW_TRAP_JOIN_BRANCH_FAILED: i32 = 211;

// ── Reply-failure classification ─────────────────────────────────────────
//
// Discriminants recorded on a reply channel (`HewReplyChannel.fail_reason`,
// first-write-wins) so a null reply is status-bearing: the waiter can name
// WHY no value arrived instead of collapsing every failure into "null".
// Consumed by `hew_reply_channel_failure_kind` / `hew_join_branch_failed`.

/// No failure recorded: a null reply with this kind is a legitimate null.
pub const HEW_REPLY_FAIL_NONE: i32 = 0;
/// The target actor stopped (mailbox teardown retired the queued ask before
/// dispatch — the orphaned-ask class).
pub const HEW_REPLY_FAIL_ACTOR_STOPPED: i32 = 1;
/// The waiting side cancelled the ask before a reply arrived.
pub const HEW_REPLY_FAIL_CANCELLED: i32 = 2;
/// The handler trapped mid-dispatch; the scheduler's crash fallback resolved
/// the waiter with an empty reply.
pub const HEW_REPLY_FAIL_HANDLER_TRAPPED: i32 = 3;
/// The reply payload could not be materialized (allocation failure on the
/// deposit or submission path).
pub const HEW_REPLY_FAIL_PAYLOAD_ALLOC_FAILED: i32 = 4;

/// Name a reply-failure discriminant for diagnostics. Fail-closed: an
/// unrecognized discriminant names itself rather than borrowing a real
/// kind's name.
#[must_use]
pub const fn reply_fail_kind_name(kind: i32) -> &'static str {
    match kind {
        HEW_REPLY_FAIL_NONE => "no failure recorded",
        HEW_REPLY_FAIL_ACTOR_STOPPED => "actor stopped before replying",
        HEW_REPLY_FAIL_CANCELLED => "ask cancelled before a reply arrived",
        HEW_REPLY_FAIL_HANDLER_TRAPPED => "handler trapped during dispatch",
        HEW_REPLY_FAIL_PAYLOAD_ALLOC_FAILED => "reply payload allocation failed",
        _ => "(unrecognized reply-failure discriminant)",
    }
}

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
        | HEW_TRAP_EXHAUSTIVENESS_FALLTHROUGH
        | HEW_TRAP_MODULE_INIT_REGEX_FAILED
        | HEW_TRAP_WIRE_DECODE_FAILED
        | HEW_TRAP_JOIN_BRANCH_FAILED => Some(code),
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
    /// Actor crashed because a synthesised codegen helper hit an internal
    /// compiler-invariant violation (error code 209): a module-init regex
    /// literal that failed to compile, or an enum-clone helper reached on an
    /// opaque-handle-bearing variant. Dead in correctly-compiled programs;
    /// reaching this trap indicates a producer-side regression.
    ModuleInitRegexFailed,
    /// Actor crashed because a wire-type `Type.decode(bytes)` was handed bytes
    /// that are not a valid encoding of the type (error code 210). The
    /// deserialize thunk freed its partial reconstruction and returned null;
    /// the decode call site traps on that null. Reachable on malformed or
    /// adversarial input — the fail-closed boundary, not a producer regression.
    WireDecodeFailed,
    /// Actor crashed because a `join{}` branch's reply resolved without a
    /// value (error code 211): the branch's target actor stopped, the ask was
    /// cancelled, the handler trapped, or the reply payload failed to
    /// materialize. `hew_join_branch_failed` names the classified cause in the
    /// diagnostic before the trap fires. Reachable whenever a joined actor
    /// dies mid-join — an environmental failure, not a producer regression.
    JoinBranchFailed,
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
            ExitReason::ModuleInitRegexFailed => "ModuleInitRegexFailed",
            ExitReason::WireDecodeFailed => "WireDecodeFailed",
            ExitReason::JoinBranchFailed => "JoinBranchFailed",
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
            HEW_TRAP_MODULE_INIT_REGEX_FAILED => ExitReason::ModuleInitRegexFailed,
            HEW_TRAP_WIRE_DECODE_FAILED => ExitReason::WireDecodeFailed,
            HEW_TRAP_JOIN_BRANCH_FAILED => ExitReason::JoinBranchFailed,
            sig => ExitReason::Signal(sig),
        }
    }

    /// Project this runtime `ExitReason` into the link-cascade `CrashKind`
    /// surfaced to a linked actor.
    ///
    /// This is the M-6 projection: the runtime distinguishes 13 exit reasons,
    /// but a linked actor only observes the coarse CLASS of a peer's failure
    /// (`std/failure.hew::CrashKind`), never the peer's private trap details.
    ///
    /// The match is INTENTIONALLY no-wildcard and exhaustive: adding an
    /// `ExitReason` variant must force a deliberate `CrashKind` mapping decision
    /// here (the `boundary-fail-closed` / `exhaustive-coverage` invariant). A
    /// new reason MUST NOT default-swallow into `Crashed` silently — the
    /// compiler error here is the forcing function.
    ///
    /// Current mapping (per `std/failure.hew::CrashKind` contract):
    /// - `HeapExceeded` → `CrashKind::HeapExceeded` (the per-actor arena cap).
    /// - every other fault class (traps, signals, send failures, internal
    ///   compiler-invariant traps, wire-decode failures) → `CrashKind::Crashed`.
    /// - `Normal` is not a crash; it projects to `Crashed` defensively but a
    ///   normal exit never reaches the crash-propagation path
    ///   (`propagate_exit_to_links` is called from the trap path, not clean stop).
    ///
    /// `CrashKind::PartitionDetected` has NO source `ExitReason` today: the
    /// runtime carries no duplex/mailbox-partition exit reason yet. The variant
    /// is reserved (the std doc's additive-widening note) for when one is added;
    /// the new reason's arm here will map to it. A `CrashKind` variant that no
    /// `ExitReason` projects to is the documented-allowed direction (the runtime
    /// may surface fewer classes than `CrashKind` names).
    #[must_use]
    pub const fn to_crash_kind(self) -> CrashKind {
        match self {
            ExitReason::HeapExceeded => CrashKind::HeapExceeded,
            ExitReason::IntegerOverflow
            | ExitReason::DivideByZero
            | ExitReason::SignedMinDivNegOne
            | ExitReason::ShiftOutOfRange
            | ExitReason::IndexOutOfBounds
            | ExitReason::ActorSendFailed
            | ExitReason::MachineDispatchUnreachable
            | ExitReason::ExhaustivenessFallthrough
            | ExitReason::ModuleInitRegexFailed
            | ExitReason::WireDecodeFailed
            | ExitReason::JoinBranchFailed
            | ExitReason::Signal(_)
            | ExitReason::Normal => CrashKind::Crashed,
        }
    }
}

/// Class of a crash propagated to a linked actor — the runtime mirror of
/// `std/failure.hew::CrashKind`.
///
/// The discriminants match the std enum's DECLARATION ORDER (the tagged-union
/// tag a Hew `CrashKind` value carries): `Crashed = 0`, `HeapExceeded = 1`,
/// `PartitionDetected = 2`. M-7 delivers this tag in a typed
/// `CrashNotification { actor_id, kind }` to a linked actor's exit hook; the
/// `as i32` tag is the wire value.
///
/// Adding a variant here REQUIRES a matching variant in `std/failure.hew`
/// (and the embedded `FAILURE_HEW`) at the same declaration index, and a
/// matching arm in `ExitReason::to_crash_kind`.
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CrashKind {
    /// Generic crash from `panic(...)`, `hew_panic()`, or an unclassified trap.
    Crashed = 0,
    /// Per-actor arena cap exceeded.
    HeapExceeded = 1,
    /// A duplex / mailbox partition was observed on a recv path the crashed
    /// actor was awaiting. Reserved: no `ExitReason` projects to it yet.
    PartitionDetected = 2,
}

impl CrashKind {
    /// The `i32` tag a Hew `CrashKind` value carries (its tagged-union tag).
    #[must_use]
    pub const fn tag(self) -> i32 {
        self as i32
    }

    /// Project a raw actor `error_code` straight to the `CrashKind` tag — the
    /// integer the link-cascade delivery boundary (`propagate_exit_to_links`'s
    /// `reason: i32`) carries. Composes `ExitReason::from_error_code` with
    /// `to_crash_kind` so callers at the delivery boundary do not re-derive the
    /// projection.
    #[must_use]
    pub fn tag_from_error_code(code: i32) -> i32 {
        ExitReason::from_error_code(code).to_crash_kind().tag()
    }
}

#[cfg(test)]
mod crash_kind_projection_tests {
    use super::*;

    /// The `CrashKind` tags match `std/failure.hew`'s declaration order — the
    /// wire value M-7 delivers. Exact-value, not `> 0`: a wrong tag is a
    /// cross-actor mis-classification.
    #[test]
    fn crash_kind_tags_match_std_declaration_order() {
        assert_eq!(CrashKind::Crashed.tag(), 0);
        assert_eq!(CrashKind::HeapExceeded.tag(), 1);
        assert_eq!(CrashKind::PartitionDetected.tag(), 2);
    }

    /// `HeapExceeded` is the ONLY reason that projects to a non-`Crashed` kind.
    /// Exact-value assertion per `wire-contract-test-presence`.
    #[test]
    fn heap_exceeded_projects_to_heap_exceeded_kind() {
        assert_eq!(
            ExitReason::HeapExceeded.to_crash_kind(),
            CrashKind::HeapExceeded
        );
        assert_eq!(
            CrashKind::tag_from_error_code(HEW_TRAP_HEAP_EXCEEDED),
            CrashKind::HeapExceeded.tag()
        );
    }

    /// Every non-heap fault class projects to `Crashed`. Exhaustive over the
    /// runtime trap-code surface: if a NEW `ExitReason` variant is added, the
    /// no-wildcard match in `to_crash_kind` fails to compile until mapped, and
    /// THIS test must gain the new code's expected projection — the two together
    /// are the `exhaustive-coverage` forcing function.
    #[test]
    fn every_non_heap_fault_projects_to_crashed() {
        for code in [
            HEW_TRAP_INTEGER_OVERFLOW,
            HEW_TRAP_DIVIDE_BY_ZERO,
            HEW_TRAP_SIGNED_MIN_DIV_NEG_ONE,
            HEW_TRAP_SHIFT_OUT_OF_RANGE,
            HEW_TRAP_INDEX_OUT_OF_BOUNDS,
            HEW_TRAP_ACTOR_SEND_FAILED,
            HEW_TRAP_MACHINE_DISPATCH_UNREACHABLE,
            HEW_TRAP_EXHAUSTIVENESS_FALLTHROUGH,
            HEW_TRAP_MODULE_INIT_REGEX_FAILED,
            HEW_TRAP_WIRE_DECODE_FAILED,
            HEW_TRAP_JOIN_BRANCH_FAILED,
        ] {
            assert_eq!(
                CrashKind::tag_from_error_code(code),
                CrashKind::Crashed.tag(),
                "trap code {code} must project to Crashed"
            );
        }
        // A raw signal number (not a known trap code) is a hardware-signal exit
        // → Crashed.
        assert_eq!(
            ExitReason::Signal(11).to_crash_kind(),
            CrashKind::Crashed,
            "a hardware signal exit projects to Crashed"
        );
        // Normal is not a crash; it projects defensively to Crashed (it never
        // reaches the crash-propagation path).
        assert_eq!(ExitReason::Normal.to_crash_kind(), CrashKind::Crashed);
    }

    /// No `ExitReason` projects to `PartitionDetected` today — the variant is
    /// reserved (the std doc's additive-widening note). This pins the current
    /// contract: if a partition exit reason is added later, this test changes
    /// deliberately rather than the projection silently gaining a mapping.
    #[test]
    fn no_exit_reason_projects_to_partition_yet() {
        let all = [
            ExitReason::HeapExceeded,
            ExitReason::IntegerOverflow,
            ExitReason::DivideByZero,
            ExitReason::SignedMinDivNegOne,
            ExitReason::ShiftOutOfRange,
            ExitReason::IndexOutOfBounds,
            ExitReason::ActorSendFailed,
            ExitReason::MachineDispatchUnreachable,
            ExitReason::ExhaustivenessFallthrough,
            ExitReason::ModuleInitRegexFailed,
            ExitReason::WireDecodeFailed,
            ExitReason::JoinBranchFailed,
            ExitReason::Signal(-1),
            ExitReason::Normal,
        ];
        assert!(
            all.iter()
                .all(|r| r.to_crash_kind() != CrashKind::PartitionDetected),
            "no current ExitReason should project to PartitionDetected"
        );
    }
}
