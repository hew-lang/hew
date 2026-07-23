//! Suspending-terminator lowering for the LLVM backend.
//!
//! Pure relocation (R4 god-module carve) of the await/recv/accept/stream/ask/
//! sleep/scope-deadline suspending-terminator lowering out of llvm.rs. Each
//! suspending terminator lowers to a `crate::coro` suspend point plus its
//! kind-specific runtime dispatch and resume-bind block; this module owns those
//! per-kind emitters and their `*Emit` argument structs. `crate::llvm`'s
//! `lower_terminator` / `dispatch_collapsed_suspend` construct the `*Emit`
//! structs and call into `crate::suspend::*`; this module imports the shared
//! codegen context and helpers from `crate::llvm` and the suspend skeleton from
//! `crate::coro`.
//!
//! Mirrors the `crate::coro` carve exactly: no behaviour change — every emitted
//! `.ll` is byte-identical before and after.

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::intrinsics::Intrinsic;
use inkwell::module::{Linkage, Module as LlvmModule};
use inkwell::targets::TargetData;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StructType};
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, PointerValue};
use inkwell::{AddressSpace, IntPredicate};

use hew_hir::{mangle_dotted_name, HirRestartPolicy, HirSupervisorStrategy};
use hew_mir::{
    ActorLayout, ChildInitArg, Place, PoolCount, RecordLayout, StateFieldCloneKind,
    SupervisorChildLayout, SupervisorLayout,
};
use hew_runtime::internal::types::HEW_TRAP_ACTOR_SEND_FAILED;
use hew_types::ResolvedTy;

use crate::llvm::CoroState;
#[allow(unused_imports)]
use crate::llvm::*;
use crate::runtime_abi::{fn_type_for_return, intern_runtime_decl, reconcile_int_width_signed};
use crate::wire::declare_codec_prim;

pub(crate) struct RemoteAskEmit<'a> {
    pub(crate) actor: Place,
    pub(crate) msg_type: i32,
    pub(crate) value: Place,
    pub(crate) timeout_ms: Place,
    pub(crate) result_dest: Place,
    pub(crate) reply_dest: Place,
    pub(crate) error_dest: Place,
    pub(crate) reply_ty: &'a ResolvedTy,
    pub(crate) next: u32,
}

/// Carrier for [`emit_suspending_remote_actor_ask_terminator`] — the suspending
/// cross-node `await remote.ask(...)` ramp (`Terminator::SuspendingRemoteAsk`).
pub(crate) struct SuspendingRemoteAskEmit<'a> {
    pub(crate) actor: Place,
    pub(crate) msg_type: i32,
    pub(crate) value: Place,
    pub(crate) timeout_ms: Place,
    pub(crate) result_dest: Place,
    pub(crate) reply_dest: Place,
    pub(crate) error_dest: Place,
    pub(crate) reply_ty: &'a ResolvedTy,
    pub(crate) resume: u32,
    pub(crate) cleanup: u32,
}

pub(crate) struct SuspendingAskEmit {
    pub(crate) actor: Place,
    pub(crate) msg_type: i32,
    pub(crate) value: Place,
    pub(crate) cleanup_plan: Option<hew_mir::state_clone::ValueSnapshotPlan>,
    pub(crate) result_dest: Place,
    pub(crate) reply_dest: Place,
    pub(crate) error_dest: Place,
    pub(crate) resume: u32,
    pub(crate) cleanup: u32,
    /// NEW-6b `await … | after d` deadline (nanoseconds). `Some(ns)` schedules a
    /// fail-closed timeout against the suspend's cancel registration; on expiry
    /// the resume edge binds `Err(AskError::Timeout)` instead of waiting for a
    /// reply. `None` is a plain suspending ask.
    pub(crate) deadline_ns: Option<i64>,
}

pub(crate) struct SuspendingReadEmit {
    pub(crate) conn: Place,
    pub(crate) result_dest: Place,
    pub(crate) deadline_result_dest: Option<Place>,
    pub(crate) error_dest: Option<Place>,
    /// `true` when the source was `await conn.read_string() | after d`: codegen
    /// converts `result_dest` (bytes) to a string via `hew_bytes_to_string`
    /// before wrapping in `Ok(_)` into `deadline_result_dest`. Ignored (treated
    /// as `false`) when `deadline_result_dest` is `None`.
    pub(crate) to_string: bool,
    pub(crate) resume: u32,
    pub(crate) cleanup: u32,
    /// NEW-6c `await conn.read() | after d` deadline (nanoseconds). `Some(ns)`
    /// schedules the common await-cancel arbiter against the read slot; timeout
    /// binds `Err(NetError::TimedOut(0))`. `None` is a plain suspending read.
    pub(crate) deadline_ns: Option<i64>,
}

/// Carrier for [`emit_suspending_accept_terminator`] — the suspending
/// `await listener.accept()` ramp (`Terminator::SuspendingAccept`).
pub(crate) struct SuspendingAcceptEmit {
    pub(crate) listener: Place,
    pub(crate) result_dest: Place,
    /// When present, this accept is the source of `await ln.accept() | after d`.
    /// `result_dest` remains the raw `Connection` slot; codegen binds
    /// `Ok(result_dest)` or `Err(NetError::TimedOut)` into this outer Result slot
    /// after resolving the await-cancel arbiter.
    pub(crate) deadline_result_dest: Option<Place>,
    /// `NetError` payload slot for the deadline Err arm. Present exactly when
    /// `deadline_result_dest` is present.
    pub(crate) error_dest: Option<Place>,
    /// NEW-6d `await ln.accept() | after d` deadline (nanoseconds). `Some(ns)`
    /// schedules the common await-cancel arbiter against the read slot; timeout
    /// binds `Err(NetError::TimedOut(0))`. `None` is a plain suspending accept.
    pub(crate) deadline_ns: Option<i64>,
    pub(crate) resume: u32,
    pub(crate) cleanup: u32,
}

/// Carrier for [`emit_suspending_stream_next_terminator`] — the suspending
/// `await stream.recv()` ramp (`Terminator::SuspendingStreamNext`).
pub(crate) struct SuspendingStreamNextEmit {
    pub(crate) stream: Place,
    pub(crate) result_dest: Place,
    /// The stream's element type. The bind edge synthesizes a
    /// `HewVecElemLayout` witness from it
    /// ([`channel_elem_layout_witness_ptr`]) and pops via
    /// `hew_stream_pop_layout` — one mechanism for every describable element
    /// type. Mirrors `elem_ty` on [`SuspendingChannelRecvEmit`].
    pub(crate) elem_ty: ResolvedTy,
    /// When present, wraps `result_dest` in `Ok(_)` or emits
    /// `Err(TimeoutError::Timeout)` here on deadline expiry.
    pub(crate) deadline_result_dest: Option<Place>,
    /// `TimeoutError` payload slot for the deadline Err arm.
    pub(crate) error_dest: Option<Place>,
    /// Absolute deadline in nanoseconds from the source expression, when
    /// `await stream.recv() | after d` was the form.
    pub(crate) deadline_ns: Option<i64>,
    pub(crate) resume: u32,
    pub(crate) cleanup: u32,
}

/// Carrier for [`emit_suspending_stream_send_terminator`] — the suspending
/// `await sink.send(x)` ramp (`Terminator::SuspendingStreamSend`).
pub(crate) struct SuspendingStreamSendEmit {
    pub(crate) sink: Place,
    pub(crate) value: Place,
    pub(crate) resume: u32,
    pub(crate) cleanup: u32,
}

/// Carrier for [`emit_suspending_channel_recv_terminator`] — the suspending
/// `await rx.recv()` ramp (`Terminator::SuspendingChannelRecv`).
pub(crate) struct SuspendingChannelRecvEmit {
    pub(crate) receiver: Place,
    pub(crate) result_dest: Place,
    /// The channel's element type. The bind edge synthesizes a
    /// `HewVecElemLayout` witness from it
    /// ([`channel_elem_layout_witness_ptr`]) and pops via the non-blocking
    /// `hew_channel_try_recv_layout` — one mechanism for every describable
    /// element type.
    pub(crate) elem_ty: ResolvedTy,
    /// When present, wraps `result_dest` in `Ok(_)` or emits
    /// `Err(TimeoutError::Timeout)` here on deadline expiry.
    pub(crate) deadline_result_dest: Option<Place>,
    /// `TimeoutError` payload slot for the deadline Err arm.
    pub(crate) error_dest: Option<Place>,
    /// Absolute deadline in nanoseconds from the source expression, when
    /// `await rx.recv() | after d` was the form.
    pub(crate) deadline_ns: Option<i64>,
    pub(crate) resume: u32,
    pub(crate) cleanup: u32,
}

/// Carrier for [`emit_suspending_call_closure_terminator`] — the
/// suspendable-callee driver (`Terminator::SuspendingCallClosure`).
pub(crate) struct SuspendingCallClosureEmit<'a> {
    pub(crate) callee: Place,
    pub(crate) args: Vec<Place>,
    pub(crate) ret_ty: &'a ResolvedTy,
    pub(crate) result_dest: Option<Place>,
    pub(crate) resume: u32,
    pub(crate) cleanup: u32,
}

/// Carrier for [`emit_suspending_task_await_terminator`] — the suspending
/// `await t` ramp (`Terminator::SuspendingTaskAwait`).
pub(crate) struct SuspendingTaskAwaitEmit {
    pub(crate) scope: Place,
    pub(crate) task: Place,
    /// `Some(slot)` reads the task result via `hew_task_get_result` on the bind
    /// edge and copies it into the slot at the `T` element width (value task);
    /// `None` for a unit task (nothing to bind).
    pub(crate) result_dest: Option<Place>,
    pub(crate) resume: u32,
    pub(crate) cleanup: u32,
}

/// Carrier for [`emit_suspending_restart_wait_terminator`] — the suspending
/// `await_restart sup.child` ramp (`SuspendKind::RestartWait`). The supervisor
/// analogue of [`SuspendingTaskAwaitEmit`].
pub(crate) struct SuspendingRestartWaitEmit {
    /// The supervisor PID place (the restart-observer registration target).
    pub(crate) sup_place: Place,
    /// The static-child slot index to wait on (the pre-park state check key).
    pub(crate) slot_index: u32,
    /// The handle slot re-fetched on the bind edge. The MIR resume edge already
    /// re-resolves the slot via `hew_supervisor_child_get`; codegen only parks
    /// and resumes here (the re-fetch is a separate MIR instruction on `resume`).
    pub(crate) result_dest: Place,
    pub(crate) resume: u32,
    pub(crate) cleanup: u32,
}

/// Carrier for [`emit_suspending_sleep_terminator`] — the suspending
/// `sleep(d)` ramp (`Terminator::SuspendingSleep`).
pub(crate) struct SuspendingSleepEmit {
    /// Place holding the `duration` value (i64 nanoseconds).
    /// Codegen converts to milliseconds before arming the timer wheel.
    pub(crate) duration_ns: Place,
    pub(crate) resume: u32,
    pub(crate) cleanup: u32,
}

/// Carrier for [`emit_suspending_sleep_until_terminator`] — the suspending
/// `sleep_until(i)` ramp (`Terminator::SuspendingSleepUntil`).
pub(crate) struct SuspendingSleepUntilEmit {
    /// Place holding the `instant` value (i64 nanoseconds, monotonic).
    /// Codegen computes `max(0, instant − now) / 1_000_000` then arms the wheel.
    pub(crate) instant_ns: Place,
    pub(crate) resume: u32,
    pub(crate) cleanup: u32,
}

/// Carrier for [`emit_suspending_scope_deadline_terminator`] — the suspending
/// `scope { } after(d) { body }` ramp (`Terminator::SuspendingScopeDeadline`).
pub(crate) struct SuspendingScopeDeadlineEmit {
    pub(crate) scope: Place,
    pub(crate) duration_ms: Place,
    pub(crate) timeout_body_block: u32,
    pub(crate) resume: u32,
    pub(crate) cleanup: u32,
}

#[allow(
    clippy::too_many_arguments,
    reason = "suspend-point lowering is parameterized by the shared coroutine \
              blocks plus per-kind abandon/resume continuations"
)]
pub(crate) fn emit_suspend_point<'ctx, FAbandon, FResume>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    coro: CoroState<'ctx>,
    parent: FunctionValue<'ctx>,
    resume_bind_bb: inkwell::basic_block::BasicBlock<'ctx>,
    label: &'static str,
    abandon_cleanup_label: &'static str,
    abandon_branch_context: &'static str,
    emit_abandon_cleanup: FAbandon,
    emit_resume_bind: FResume,
) -> CodegenResult<()>
where
    FAbandon: FnOnce() -> CodegenResult<()>,
    FResume: FnOnce() -> CodegenResult<()>,
{
    let abandon_cleanup_bb = fn_ctx.ctx.append_basic_block(parent, abandon_cleanup_label);
    let cc = crate::coro::CoroContext {
        ctx: fn_ctx.ctx,
        llvm_mod: fn_ctx.llvm_mod,
        builder: &fn_ctx.builder,
        function: parent,
        handle: coro.handle,
        id_token: coro.id_token,
    };
    cc.emit_suspend(
        resume_bind_bb,
        abandon_cleanup_bb,
        coro.suspend_return_block,
        false,
        label,
    )?;

    fn_ctx.builder.position_at_end(abandon_cleanup_bb);
    emit_abandon_cleanup()?;
    // #2395 — the abandon (destroy-while-parked) edge. After the per-kind
    // bookkeeping cleanup (slot cancel/free, observer deregister, deadline
    // cancel) and BEFORE joining the frame-free `coro.cleanup` epilogue, drop
    // the frame-owned Hew heap values live across this suspend. This is the
    // never-implemented "cleanup outline": the MIR elaborator populated this
    // block's `ExitPath::Suspend` plan with the `drops_for_exit`
    // `BindingState`-filtered owned locals (a moved-out local is `Consumed` and
    // excluded — no double-free), and the block loop suppressed its normal-flow
    // emission so these fire ONLY here on the case-1 edge, never on resume. The
    // block id is threaded via `FnCtx::suspend_abandon_block` (set by
    // `lower_terminator`) so this one helper covers all 13 collapsed emitters.
    crate::llvm::emit_elab_drops(
        fn_ctx,
        fn_ctx.suspend_abandon_block.get(),
        fn_ctx.drop_plans,
    )?;
    fn_ctx
        .builder
        .build_unconditional_branch(coro.cleanup_block)
        .llvm_ctx(abandon_branch_context)?;

    fn_ctx.builder.position_at_end(resume_bind_bb);
    emit_resume_bind()
}

/// Construct the one-shot `HewAwaitCancel` arbiter every suspending terminator
/// races its deadline/cancel ramp on.
///
/// `hew_await_cancel_new(self, cleanup, ctx)` mints the first-ready CAS record
/// that the deadline arm (`schedule_deadline_ms`) and the readiness arm (slot
/// take / reply deposit / scope join) settle exactly once. The three pointer
/// arguments are the only per-terminator variation in the constructor itself:
/// - `self_actor` — the parked-continuation actor a timer/reactor wake
///   re-enqueues (always `hew_actor_self()`, never a spilled param).
/// - `cleanup` — the timer-fire teardown callback, or null for the deadline-only
///   ramps (sleep / sleep_until / scope) whose abandon/resume arms do the
///   teardown directly with full pointer context.
/// - `ctx` — the cleanup callback's argument (the read slot, a recv cancel-ctx
///   struct, or the reply channel), or null when `cleanup` is null.
///
/// Returns the arbiter pointer or fails closed — the constructor never fabricates
/// a registration from a void return. Each call site keeps its own
/// `set_await_cancel` wiring, deadline schedule, and per-kind timeout
/// continuation; only the fail-closed constructor lives here.
pub(crate) fn emit_await_cancel_new<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    self_actor: inkwell::values::PointerValue<'ctx>,
    cleanup: inkwell::values::PointerValue<'ctx>,
    ctx_arg: inkwell::values::PointerValue<'ctx>,
    name: &str,
) -> CodegenResult<inkwell::values::PointerValue<'ctx>> {
    let cancel_new = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_await_cancel_new",
    )?;
    Ok(fn_ctx
        .builder
        .build_call(
            cancel_new,
            &[self_actor.into(), cleanup.into(), ctx_arg.into()],
            name,
        )
        .llvm_ctx("hew_await_cancel_new call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_await_cancel_new returned void".into()))?
        .into_pointer_value())
}

/// Emit the caller-side non-blocking `await conn.read()` (NEW-1
/// `Terminator::SuspendingRead`). The fd-readiness analogue of
/// [`emit_suspending_ask_terminator`].
///
/// Shape (the suspendable read ramp):
/// ```text
///   self      = hew_actor_self()                 ; the parked-continuation actor
///   conn      = <load conn handle>               ; pointer-shaped (opaque)
///   slot      = hew_read_slot_new()
///   rc        = hew_conn_await_read(conn, self, slot)
///   br (rc != 0) -> register_err, do_suspend
/// register_err:                                  ; registration failed, no read
///   hew_read_slot_cancel(slot); hew_read_slot_free(slot)
///   result_dest = empty bytes                    ; EOF/empty convention
///   br resume_bb
/// do_suspend:                                    ; coro.suspend (non-final)
///   switch coro.suspend [default -> return handle, 0 -> read_bind, 1 -> cleanup]
/// abandon_cleanup:                               ; parked cont destroyed
///   hew_read_slot_cancel(slot); hew_read_slot_free(slot); br shared cleanup
/// read_bind:                                     ; the reactor resumed us
///   triple = hew_read_slot_take(slot)            ; the bytes (already deposited)
///   store triple -> result_dest
///   hew_read_slot_free(slot)                     ; release the creator ref
///   br resume_bb
/// ```
/// The bytes travel through `slot` (a frame-spilled local) across the suspend;
/// on resume `hew_read_slot_take` returns them on the fast path (the reactor
/// deposited them before `enqueue_resume` woke us). Slot ref counting: `new`
/// (+1 creator ref); `hew_conn_await_read` takes its own reactor ref on success;
/// the single `hew_read_slot_free` on each terminal edge releases the creator
/// ref — the reactor releases its ref when its `Registration` is dropped.
#[allow(
    clippy::too_many_lines,
    reason = "the full caller-side read ramp — registration + suspend + the \
              resume-edge bytes binding — is kept in one place so the suspend \
              point and the value routing it depends on are read together"
)]
pub(crate) fn emit_suspending_read_terminator<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    term: SuspendingReadEmit,
) -> CodegenResult<()> {
    // The coro prologue must be present (lower_function detects the SuspendingRead
    // carrier via `has_suspend` and emits it). Fail closed otherwise (R2 / the
    // Lane-B silent-no-op class) — a SuspendingRead without coro state is a
    // producer that ran ahead of the prologue.
    let coro = fn_ctx.coro.ok_or_else(|| {
        CodegenError::FailClosed(
            "Terminator::SuspendingRead reached codegen but the function carries no \
             coro prologue state — lower_function must detect the suspend carrier \
             (has_suspend) and emit the prologue before the body"
                .into(),
        )
    })?;
    let resume_bb = *fn_ctx.blocks.get(&term.resume).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "SuspendingRead resume target bb{} not found",
            term.resume
        ))
    })?;
    if !fn_ctx.blocks.contains_key(&term.cleanup) {
        return Err(CodegenError::FailClosed(format!(
            "SuspendingRead cleanup target bb{} not found",
            term.cleanup
        )));
    }
    if term.deadline_ns.is_some()
        && (term.deadline_result_dest.is_none() || term.error_dest.is_none())
    {
        return Err(CodegenError::FailClosed(
            "SuspendingRead deadline reached codegen without Result/NetError destinations".into(),
        ));
    }

    // self = the current actor — the parked-continuation waiter the reactor wake
    // re-enqueues. MUST come from `hew_actor_self()` (the thread-local execution
    // context), NOT a spilled param: across a suspend the coroutine frame spills
    // param 0, but the scheduler's per-dispatch context it pointed to is freed at
    // the first suspend. On RESUME the scheduler installs a fresh context
    // (`resume_suspended_activation`), so the thread-local read returns the live
    // actor. This is the SAME single-authority live-context accessor the
    // SuspendingAsk ramp uses (FIX-THE-CLASS).
    let actor_self_fn = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_actor_self",
    )?;
    let self_actor = fn_ctx
        .builder
        .build_call(actor_self_fn, &[], "suspending_read_self")
        .llvm_ctx("hew_actor_self call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_actor_self returned void".into()))?
        .into_pointer_value();
    let conn_ptr = load_duplex_handle(fn_ctx, term.conn, "suspending_read conn")?;

    let slot = fn_ctx.call_runtime_ptr(
        "hew_read_slot_new",
        &[],
        "suspending_read_slot",
        "hew_read_slot_new call",
    )?;

    let i32_ty = fn_ctx.ctx.i32_type();
    let i64_ty = fn_ctx.ctx.i64_type();
    let reg: Option<inkwell::values::PointerValue<'ctx>> = if term.deadline_ns.is_some() {
        let cleanup_fn = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_read_slot_cancel_cleanup",
        )?;
        let cleanup_ptr = cleanup_fn.as_global_value().as_pointer_value();
        let reg = emit_await_cancel_new(
            fn_ctx,
            self_actor,
            cleanup_ptr,
            slot,
            "suspending_read_deadline_reg",
        )?;
        let set_await_cancel = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_read_slot_set_await_cancel",
        )?;
        fn_ctx
            .builder
            .build_call(
                set_await_cancel,
                &[slot.into(), reg.into()],
                "suspending_read_set_await_cancel",
            )
            .llvm_ctx("hew_read_slot_set_await_cancel call")?;
        Some(reg)
    } else {
        None
    };

    let rc = fn_ctx.call_runtime_int(
        "hew_conn_await_read",
        &[conn_ptr.into(), self_actor.into(), slot.into()],
        "suspending_read_register",
        "hew_conn_await_read call",
    )?;

    let parent = fn_ctx
        .builder
        .get_insert_block()
        .and_then(|bb| bb.get_parent())
        .ok_or_else(|| CodegenError::Llvm("suspending read block has no parent function".into()))?;
    let register_err_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "suspending_read_register_err");
    let do_suspend_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "suspending_read_suspend");
    let register_ok = fn_ctx
        .builder
        .build_int_compare(
            IntPredicate::EQ,
            rc,
            rc.get_type().const_zero(),
            "suspending_read_register_ok",
        )
        .llvm_ctx("suspending read register-ok compare")?;
    fn_ctx
        .builder
        .build_conditional_branch(register_ok, do_suspend_bb, register_err_bb)
        .llvm_ctx("suspending read register branch")?;

    let slot_cancel = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_read_slot_cancel",
    )?;
    let slot_free = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_read_slot_free",
    )?;

    // ── register_err: the registration failed; no read will ever arrive. Cancel
    // + free the slot and bind an empty `bytes` without suspending (no worker to
    // free — we never parked). ────────────────────────────────────────────────
    fn_ctx.builder.position_at_end(register_err_bb);
    fn_ctx
        .builder
        .build_call(slot_cancel, &[slot.into()], "suspending_read_err_cancel")
        .llvm_ctx("hew_read_slot_cancel (register err) call")?;
    fn_ctx
        .builder
        .build_call(slot_free, &[slot.into()], "suspending_read_err_free")
        .llvm_ctx("hew_read_slot_free (register err) call")?;
    if let Some(reg) = reg {
        let status = fn_ctx.call_runtime_int(
            "hew_await_cancel_status",
            &[reg.into()],
            "suspending_read_register_status",
            "hew_await_cancel_status (read register err) call",
        )?;
        let cancelled = fn_ctx
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                i32_ty.const_int(2, false),
                "suspending_read_register_cancelled",
            )
            .llvm_ctx("suspending read register-cancelled compare")?;
        let cancelled_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_read_register_cancelled");
        let timeout_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_read_register_timeout");
        fn_ctx
            .builder
            .build_conditional_branch(cancelled, cancelled_bb, timeout_bb)
            .llvm_ctx("suspending read register-error status branch")?;

        let result_dest = term.deadline_result_dest.ok_or_else(|| {
            CodegenError::FailClosed("SuspendingRead register error missing Result dest".into())
        })?;
        let error_dest = term.error_dest.ok_or_else(|| {
            CodegenError::FailClosed("SuspendingRead register error missing NetError dest".into())
        })?;

        fn_ctx.builder.position_at_end(cancelled_bb);
        fn_ctx.call_runtime_void(
            "hew_await_cancel_free",
            &[reg.into()],
            "suspending_read_err_cancelled_reg_free",
            "hew_await_cancel_free (read register cancelled) call",
        )?;
        emit_read_deadline_cancelled_err(fn_ctx, result_dest, error_dest)?;
        fn_ctx
            .builder
            .build_unconditional_branch(resume_bb)
            .llvm_ctx("suspending read register-cancelled br")?;

        fn_ctx.builder.position_at_end(timeout_bb);
        fn_ctx.call_runtime_void(
            "hew_await_cancel_free",
            &[reg.into()],
            "suspending_read_err_timeout_reg_free",
            "hew_await_cancel_free (read register timeout) call",
        )?;
        emit_read_deadline_timeout_err(fn_ctx, result_dest, error_dest)?;
        fn_ctx
            .builder
            .build_unconditional_branch(resume_bb)
            .llvm_ctx("suspending read register-timeout br")?;
    } else {
        store_empty_bytes(fn_ctx, term.result_dest)?;
        fn_ctx
            .builder
            .build_unconditional_branch(resume_bb)
            .llvm_ctx("suspending read register-err br")?;
    }

    // ── do_suspend: park the continuation (non-final suspend). The default edge
    // returns the coro handle to the trampoline (which parks it on this actor);
    // case 0 resumes into the read-bind block; case 1 tears down. ──────────────
    fn_ctx.builder.position_at_end(do_suspend_bb);
    if let (Some(reg), Some(ns)) = (reg, term.deadline_ns) {
        let delay_ms = (ns / 1_000_000).max(1) as u64;
        let tw_fn = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_global_timer_wheel",
        )?;
        let tw = fn_ctx
            .builder
            .build_call(tw_fn, &[], "suspending_read_deadline_tw")
            .llvm_ctx("hew_global_timer_wheel call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("hew_global_timer_wheel returned void".into()))?
            .into_pointer_value();
        let schedule = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_schedule_deadline_ms",
        )?;
        let delay_val = i64_ty.const_int(delay_ms, false);
        let sched_rc = fn_ctx
            .builder
            .build_call(
                schedule,
                &[reg.into(), tw.into(), delay_val.into()],
                "suspending_read_schedule_deadline",
            )
            .llvm_ctx("hew_await_cancel_schedule_deadline_ms call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_await_cancel_schedule_deadline_ms returned void".into(),
                )
            })?
            .into_int_value();
        let armed = fn_ctx
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                sched_rc,
                sched_rc.get_type().const_zero(),
                "suspending_read_deadline_armed",
            )
            .llvm_ctx("suspending read schedule-armed compare")?;
        let deadline_proceed_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_read_deadline_proceed");
        let deadline_check_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_read_deadline_check");
        fn_ctx
            .builder
            .build_conditional_branch(armed, deadline_proceed_bb, deadline_check_bb)
            .llvm_ctx("suspending read schedule-armed branch")?;

        fn_ctx.builder.position_at_end(deadline_check_bb);
        let status_fn = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_status",
        )?;
        let sched_status = fn_ctx
            .builder
            .build_call(status_fn, &[reg.into()], "suspending_read_schedule_status")
            .llvm_ctx("hew_await_cancel_status (schedule) call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| {
                CodegenError::FailClosed("hew_await_cancel_status returned void".into())
            })?
            .into_int_value();
        let read_completed = fn_ctx
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                sched_status,
                i32_ty.const_int(1, false),
                "suspending_read_schedule_read_completed",
            )
            .llvm_ctx("suspending read schedule read-completed compare")?;
        let deadline_failclosed_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_read_deadline_failclosed");
        fn_ctx
            .builder
            .build_conditional_branch(read_completed, deadline_proceed_bb, deadline_failclosed_bb)
            .llvm_ctx("suspending read schedule read-completed branch")?;

        fn_ctx.builder.position_at_end(deadline_failclosed_bb);
        let reg_cancel = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_cancel",
        )?;
        let cancelled_status = i32_ty.const_int(2, false); // AwaitCancelStatus::Cancelled
        let no_wake = i32_ty.const_zero();
        fn_ctx
            .builder
            .build_call(
                reg_cancel,
                &[reg.into(), cancelled_status.into(), no_wake.into()],
                "suspending_read_failclosed_reg_cancel",
            )
            .llvm_ctx("hew_await_cancel_cancel (read fail-closed) call")?;
        let reg_free = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_free",
        )?;
        fn_ctx
            .builder
            .build_call(
                reg_free,
                &[reg.into()],
                "suspending_read_failclosed_reg_free",
            )
            .llvm_ctx("hew_await_cancel_free (read fail-closed) call")?;
        fn_ctx
            .builder
            .build_call(
                slot_cancel,
                &[slot.into()],
                "suspending_read_failclosed_slot_cancel",
            )
            .llvm_ctx("hew_read_slot_cancel (fail-closed) call")?;
        fn_ctx
            .builder
            .build_call(slot_free, &[slot.into()], "suspending_read_failclosed_free")
            .llvm_ctx("hew_read_slot_free (fail-closed) call")?;
        let result_dest = term.deadline_result_dest.ok_or_else(|| {
            CodegenError::FailClosed("SuspendingRead fail-closed missing Result dest".into())
        })?;
        let error_dest = term.error_dest.ok_or_else(|| {
            CodegenError::FailClosed("SuspendingRead fail-closed missing NetError dest".into())
        })?;
        emit_read_deadline_timeout_err(fn_ctx, result_dest, error_dest)?;
        fn_ctx
            .builder
            .build_unconditional_branch(resume_bb)
            .llvm_ctx("suspending read deadline fail-closed br")?;

        fn_ctx.builder.position_at_end(deadline_proceed_bb);
    }
    let read_bind_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "suspending_read_bind");
    // ── abandon_cleanup edge: when a parked SuspendingRead continuation is
    // DESTROYED without resuming (actor stop/crash while suspended, begin_park
    // refusal, the C1 free-path destroy_parked), `coro.suspend`'s case-1 edge
    // runs. The shared `coro.cleanup_block` only frees the coro frame — it never
    // sees `slot` (a codegen-local SSA value with no MIR Place), so without this
    // interposition `slot` leaks. The abandon continuation CANCELS + FREES the
    // creator ref before joining the shared cleanup. The cancel makes a racing
    // reactor deposit drop its read buffer + skip the wake; the free releases
    // this abandoned caller's creator-side ref. The late wake itself is
    // independently fail-safe (enqueue_resume drops a wake to a freed caller).
    emit_suspend_point(
        fn_ctx,
        coro,
        parent,
        read_bind_bb,
        "suspending_read",
        "suspending_read_abandon_cleanup",
        "suspending read abandon -> shared cleanup br",
        || {
            // ── abandon_cleanup: cancel + free the read slot, then join the shared
            // coro cleanup (frame-free + coro.end). ─────────────────────────────
            if let Some(reg) = reg {
                let reg_cancel = intern_runtime_decl(
                    fn_ctx.ctx,
                    fn_ctx.llvm_mod,
                    &mut fn_ctx.runtime_decls.borrow_mut(),
                    "hew_await_cancel_cancel",
                )?;
                let cancelled_status = i32_ty.const_int(2, false); // AwaitCancelStatus::Cancelled
                let no_wake = i32_ty.const_zero();
                fn_ctx
                    .builder
                    .build_call(
                        reg_cancel,
                        &[reg.into(), cancelled_status.into(), no_wake.into()],
                        "suspending_read_abandon_reg_cancel",
                    )
                    .llvm_ctx("hew_await_cancel_cancel (read abandon) call")?;
                let reg_free = intern_runtime_decl(
                    fn_ctx.ctx,
                    fn_ctx.llvm_mod,
                    &mut fn_ctx.runtime_decls.borrow_mut(),
                    "hew_await_cancel_free",
                )?;
                fn_ctx
                    .builder
                    .build_call(reg_free, &[reg.into()], "suspending_read_abandon_reg_free")
                    .llvm_ctx("hew_await_cancel_free (read abandon) call")?;
            }
            fn_ctx
                .builder
                .build_call(
                    slot_cancel,
                    &[slot.into()],
                    "suspending_read_abandon_cancel",
                )
                .llvm_ctx("hew_read_slot_cancel (abandon) call")?;
            fn_ctx
                .builder
                .build_call(slot_free, &[slot.into()], "suspending_read_abandon_free")
                .llvm_ctx("hew_read_slot_free (abandon) call")?;
            Ok(())
        },
        || emit_suspending_read_bind(fn_ctx, &term, slot, slot_free, reg, resume_bb, parent),
    )?;

    Ok(())
}

/// Resume-bind continuation of [`emit_suspending_read_terminator`]: the reactor
/// resumed us (enqueue_resume). The bytes are already deposited in `slot`;
/// `hew_read_slot_take` writes them into `result_dest` on the fast path.
/// On a deadline carrier the deadline-vs-read arbiter runs first (timeout binds
/// `Err(NetError::TimedOut)`). Release the creator ref and branch to the MIR
/// resume block. Split out so the suspend-point seam owns the suspend + abandon
/// scaffolding while this owns the value routing.
#[allow(
    clippy::too_many_lines,
    reason = "the full read-bind edge — deadline arbiter + the bytes/string \
              binding — is kept in one place so the deadline resolution and the \
              value routing it gates are read together"
)]
fn emit_suspending_read_bind<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    term: &SuspendingReadEmit,
    slot: inkwell::values::PointerValue<'ctx>,
    slot_free: FunctionValue<'ctx>,
    reg: Option<inkwell::values::PointerValue<'ctx>>,
    resume_bb: inkwell::basic_block::BasicBlock<'ctx>,
    parent: FunctionValue<'ctx>,
) -> CodegenResult<()> {
    let i32_ty = fn_ctx.ctx.i32_type();
    if let Some(reg) = reg {
        let complete = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_complete",
        )?;
        fn_ctx
            .builder
            .build_call(complete, &[reg.into()], "suspending_read_deadline_complete")
            .llvm_ctx("hew_await_cancel_complete call")?;
        let status_fn = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_status",
        )?;
        let status = fn_ctx
            .builder
            .build_call(status_fn, &[reg.into()], "suspending_read_deadline_status")
            .llvm_ctx("hew_await_cancel_status call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| {
                CodegenError::FailClosed("hew_await_cancel_status returned void".into())
            })?
            .into_int_value();
        // Three-way resume arbiter (was two-way TimedOut-vs-proceed). A runtime
        // shutdown sweep can deposit `AwaitCancelStatus::Cancelled (2)` on a
        // parked wait; route that to a typed `Err(NetError::Cancelled)` instead
        // of falling through to the proceed arm (which would wrap an invalid
        // read as `Ok(_)`). Order: Cancelled (2) → TimedOut (3) → proceed.
        let cancelled = fn_ctx
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                i32_ty.const_int(2, false),
                "suspending_read_cancelled",
            )
            .llvm_ctx("suspending read cancelled compare")?;
        let cancelled_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_read_cancelled");
        let timeout_check_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_read_timeout_check");
        fn_ctx
            .builder
            .build_conditional_branch(cancelled, cancelled_bb, timeout_check_bb)
            .llvm_ctx("suspending read cancelled branch")?;

        // Cancelled arm — structurally identical to the timeout arm but emits
        // `NetError::Cancelled`: free the deadline reg, free the slot, store the
        // typed error, and resume.
        fn_ctx.builder.position_at_end(cancelled_bb);
        let reg_free_cancel = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_free",
        )?;
        fn_ctx
            .builder
            .build_call(
                reg_free_cancel,
                &[reg.into()],
                "suspending_read_cancelled_reg_free",
            )
            .llvm_ctx("hew_await_cancel_free (read cancelled) call")?;
        fn_ctx
            .builder
            .build_call(slot_free, &[slot.into()], "suspending_read_cancelled_free")
            .llvm_ctx("hew_read_slot_free (cancelled) call")?;
        let cancel_result_dest = term.deadline_result_dest.ok_or_else(|| {
            CodegenError::FailClosed("SuspendingRead cancelled missing Result dest".into())
        })?;
        let cancel_error_dest = term.error_dest.ok_or_else(|| {
            CodegenError::FailClosed("SuspendingRead cancelled missing NetError dest".into())
        })?;
        emit_read_deadline_cancelled_err(fn_ctx, cancel_result_dest, cancel_error_dest)?;
        fn_ctx
            .builder
            .build_unconditional_branch(resume_bb)
            .llvm_ctx("suspending read cancelled br")?;

        fn_ctx.builder.position_at_end(timeout_check_bb);
        let timed_out = fn_ctx
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                i32_ty.const_int(3, false),
                "suspending_read_timed_out",
            )
            .llvm_ctx("suspending read timed-out compare")?;
        let timeout_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_read_timeout");
        let read_proceed_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_read_proceed");
        fn_ctx
            .builder
            .build_conditional_branch(timed_out, timeout_bb, read_proceed_bb)
            .llvm_ctx("suspending read deadline branch")?;

        fn_ctx.builder.position_at_end(timeout_bb);
        let reg_free = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_free",
        )?;
        fn_ctx
            .builder
            .build_call(reg_free, &[reg.into()], "suspending_read_timeout_reg_free")
            .llvm_ctx("hew_await_cancel_free (read timeout) call")?;
        fn_ctx
            .builder
            .build_call(slot_free, &[slot.into()], "suspending_read_timeout_free")
            .llvm_ctx("hew_read_slot_free (timeout) call")?;
        let result_dest = term.deadline_result_dest.ok_or_else(|| {
            CodegenError::FailClosed("SuspendingRead timeout missing Result dest".into())
        })?;
        let error_dest = term.error_dest.ok_or_else(|| {
            CodegenError::FailClosed("SuspendingRead timeout missing NetError dest".into())
        })?;
        emit_read_deadline_timeout_err(fn_ctx, result_dest, error_dest)?;
        fn_ctx
            .builder
            .build_unconditional_branch(resume_bb)
            .llvm_ctx("suspending read timeout br")?;

        fn_ctx.builder.position_at_end(read_proceed_bb);
    }
    // Canonical `hew_read_slot_take(slot) -> BytesTriple`, classified per target
    // by the R5 ABI classifier (register pair on SysV/AAPCS, sret on MSVC),
    // replacing the `hew_read_slot_take_raw` out-pointer twin that faked MSVC's
    // sret ABI. The result triple is stored into the bytes dest slot.
    let (dest_ptr, dest_ty) = place_pointer(fn_ctx, term.result_dest)?;
    if !matches!(dest_ty, BasicTypeEnum::StructType(_)) {
        return Err(CodegenError::FailClosed(format!(
            "SuspendingRead result_dest must be a bytes struct slot, got {dest_ty:?}"
        )));
    }
    let bytes_triple_ty = crate::runtime_abi::bytes_triple_llvm_ty(fn_ctx);
    let triple = fn_ctx.llvm_mod.get_triple();
    let triple_str = triple.as_str().to_string_lossy();
    let (slot_take, return_abi) = crate::abi_class::declare_aggregate_return(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        fn_ctx.target_data,
        &triple_str,
        "hew_read_slot_take",
        bytes_triple_ty,
        &[fn_ctx.ctx.ptr_type(inkwell::AddressSpace::default()).into()],
    )?;
    crate::runtime_abi::store_classified_bytes_return(
        fn_ctx,
        slot_take,
        return_abi,
        &[slot.into()],
        dest_ptr,
        "hew_read_slot_take",
    )?;
    if let Some(result_dest) = term.deadline_result_dest {
        if term.to_string {
            // `await conn.read_string() | after d` success path: convert the raw
            // bytes (`result_dest`) to a string before wrapping in `Ok(_)`.
            // `hew_bytes_to_string` takes a `*bytes` (by-pointer consumer convention)
            // and returns a header-pointer-shaped string (ptr).
            let bytes_to_string = intern_runtime_decl(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                &mut fn_ctx.runtime_decls.borrow_mut(),
                "hew_bytes_to_string",
            )?;
            let (bytes_ptr, _) = place_pointer(fn_ctx, term.result_dest)?;
            let string_val = fn_ctx
                .builder
                .build_call(
                    bytes_to_string,
                    &[bytes_ptr.into()],
                    "suspending_read_bytes_to_string",
                )
                .llvm_ctx("hew_bytes_to_string call")?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| {
                    CodegenError::FailClosed("hew_bytes_to_string returned void".into())
                })?;
            // result_dest is Result<string, NetError>; tag=0, Ok variant field 0 = ptr.
            // Write the string pointer directly into the Ok variant field — no
            // intermediate alloca needed since `string_val` is already a ptr value.
            let dest_local = composite_dest_local(result_dest, "SuspendingRead read_string ok")?;
            store_composite_tag(fn_ctx, dest_local, 0, "SuspendingRead read_string ok")?;
            let (ok_field_ptr, ok_field_ty) = place_pointer(
                fn_ctx,
                Place::MachineVariant {
                    local: dest_local,
                    variant_idx: 0,
                    field_idx: 0,
                },
            )?;
            if !matches!(ok_field_ty, BasicTypeEnum::PointerType(_)) {
                return Err(CodegenError::FailClosed(format!(
                    "SuspendingRead read_string Ok field must be ptr-typed, got {ok_field_ty:?}"
                )));
            }
            fn_ctx
                .builder
                .build_store(ok_field_ptr, string_val)
                .llvm_ctx("SuspendingRead read_string Ok field store")?;
        } else {
            emit_result_ok(fn_ctx, result_dest, Some(term.result_dest))?;
        }
    }
    fn_ctx
        .builder
        .build_call(slot_free, &[slot.into()], "suspending_read_ok_free")
        .llvm_ctx("hew_read_slot_free (ok) call")?;
    if let Some(reg) = reg {
        let reg_free = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_free",
        )?;
        fn_ctx
            .builder
            .build_call(reg_free, &[reg.into()], "suspending_read_ok_reg_free")
            .llvm_ctx("hew_await_cancel_free (read ok) call")?;
    }
    fn_ctx
        .builder
        .build_unconditional_branch(resume_bb)
        .llvm_ctx("suspending read ok br")?;

    Ok(())
}

/// Emit the caller-side non-blocking `await listener.accept()` (NEW-2
/// `Terminator::SuspendingAccept`). The listener-readiness analogue of
/// [`emit_suspending_read_terminator`].
///
/// Shape (the suspendable accept ramp):
/// ```text
///   self      = hew_actor_self()                 ; the parked-continuation actor
///   listener  = <load listener handle>           ; pointer-shaped (opaque)
///   slot      = hew_read_slot_new()
///   rc        = hew_listener_await_accept(listener, self, slot)
///   br (rc != 0) -> register_err, do_suspend
/// register_err:                                  ; registration failed, no accept
///   hew_read_slot_cancel(slot); hew_read_slot_free(slot)
///   result_dest = inttoptr(-1)                   ; invalid Connection convention
///   br resume_bb
/// do_suspend:                                    ; coro.suspend (non-final)
///   switch coro.suspend [default -> return handle, 0 -> accept_bind, 1 -> cleanup]
/// abandon_cleanup:                               ; parked cont destroyed
///   hew_read_slot_cancel(slot); hew_read_slot_free(slot); br shared cleanup
/// accept_bind:                                   ; the reactor resumed us
///   conn = hew_read_slot_take_handle(slot)       ; the accepted handle (as ptr)
///   store conn -> result_dest
///   hew_read_slot_free(slot)                     ; release the creator ref
///   br resume_bb
/// ```
/// The accepted connection handle travels through `slot` (a frame-spilled local)
/// across the suspend; on resume `hew_read_slot_take_handle` returns it on the
/// fast path (the reactor deposited it before `enqueue_resume` woke us). Slot ref
/// counting is identical to the read ramp: `new` (+1 creator); the await register
/// takes its own reactor ref on success; the single `hew_read_slot_free` on each
/// terminal edge releases the creator ref.
#[allow(
    clippy::too_many_lines,
    reason = "the full caller-side accept ramp — registration + suspend + the \
              resume-edge connection binding — is kept in one place so the \
              suspend point and the value routing it depends on are read together"
)]
pub(crate) fn emit_suspending_accept_terminator<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    term: SuspendingAcceptEmit,
) -> CodegenResult<()> {
    // The coro prologue must be present (lower_function detects the
    // SuspendingAccept carrier via `has_suspend`). Fail closed otherwise.
    let coro = fn_ctx.coro.ok_or_else(|| {
        CodegenError::FailClosed(
            "Terminator::SuspendingAccept reached codegen but the function carries no \
             coro prologue state — lower_function must detect the suspend carrier \
             (has_suspend) and emit the prologue before the body"
                .into(),
        )
    })?;
    let resume_bb = *fn_ctx.blocks.get(&term.resume).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "SuspendingAccept resume target bb{} not found",
            term.resume
        ))
    })?;
    if !fn_ctx.blocks.contains_key(&term.cleanup) {
        return Err(CodegenError::FailClosed(format!(
            "SuspendingAccept cleanup target bb{} not found",
            term.cleanup
        )));
    }
    if term.deadline_ns.is_some()
        && (term.deadline_result_dest.is_none() || term.error_dest.is_none())
    {
        return Err(CodegenError::FailClosed(
            "SuspendingAccept deadline reached codegen without Result/NetError destinations".into(),
        ));
    }

    // self = the current actor — the parked-continuation waiter the reactor wake
    // re-enqueues. From `hew_actor_self()` (the live thread-local context), the
    // same single-authority accessor the SuspendingRead/Ask ramps use.
    let actor_self_fn = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_actor_self",
    )?;
    let self_actor = fn_ctx
        .builder
        .build_call(actor_self_fn, &[], "suspending_accept_self")
        .llvm_ctx("hew_actor_self call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_actor_self returned void".into()))?
        .into_pointer_value();
    let listener_ptr = load_duplex_handle(fn_ctx, term.listener, "suspending_accept listener")?;

    let slot_new = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_read_slot_new",
    )?;
    let slot = fn_ctx
        .builder
        .build_call(slot_new, &[], "suspending_accept_slot")
        .llvm_ctx("hew_read_slot_new call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_read_slot_new returned void".into()))?
        .into_pointer_value();

    let i32_ty = fn_ctx.ctx.i32_type();
    let i64_ty = fn_ctx.ctx.i64_type();

    // ── Deadline cancel registration (when `| after d` is present). ────────────
    // Wire before `hew_listener_await_accept` so the registration is in place
    // before the reactor can fire. Mirrors the SuspendingRead cancel-registration
    // sequence (hew_await_cancel_new → hew_read_slot_set_await_cancel).
    let reg: Option<inkwell::values::PointerValue<'ctx>> = if term.deadline_ns.is_some() {
        let cleanup_fn = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_read_slot_cancel_cleanup",
        )?;
        let cleanup_ptr = cleanup_fn.as_global_value().as_pointer_value();
        let reg = emit_await_cancel_new(
            fn_ctx,
            self_actor,
            cleanup_ptr,
            slot,
            "suspending_accept_deadline_reg",
        )?;
        let set_await_cancel = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_read_slot_set_await_cancel",
        )?;
        fn_ctx
            .builder
            .build_call(
                set_await_cancel,
                &[slot.into(), reg.into()],
                "suspending_accept_set_await_cancel",
            )
            .llvm_ctx("hew_read_slot_set_await_cancel call")?;
        Some(reg)
    } else {
        None
    };

    let rc = fn_ctx.call_runtime_int(
        "hew_listener_await_accept",
        &[listener_ptr.into(), self_actor.into(), slot.into()],
        "suspending_accept_register",
        "hew_listener_await_accept call",
    )?;

    let parent = fn_ctx
        .builder
        .get_insert_block()
        .and_then(|bb| bb.get_parent())
        .ok_or_else(|| {
            CodegenError::Llvm("suspending accept block has no parent function".into())
        })?;
    let register_err_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "suspending_accept_register_err");
    let do_suspend_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "suspending_accept_suspend");
    let register_ok = fn_ctx
        .builder
        .build_int_compare(
            IntPredicate::EQ,
            rc,
            rc.get_type().const_zero(),
            "suspending_accept_register_ok",
        )
        .llvm_ctx("suspending accept register-ok compare")?;
    fn_ctx
        .builder
        .build_conditional_branch(register_ok, do_suspend_bb, register_err_bb)
        .llvm_ctx("suspending accept register branch")?;

    let slot_cancel = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_read_slot_cancel",
    )?;
    let slot_free = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_read_slot_free",
    )?;

    // ── register_err: the registration failed; no accept will ever arrive.
    // Cancel + free the slot. On a deadline path, emit NetError::TimedOut into the
    // Result destination. On the plain path, bind an INVALID `Connection`. ──────
    fn_ctx.builder.position_at_end(register_err_bb);
    fn_ctx
        .builder
        .build_call(slot_cancel, &[slot.into()], "suspending_accept_err_cancel")
        .llvm_ctx("hew_read_slot_cancel (register err) call")?;
    fn_ctx
        .builder
        .build_call(slot_free, &[slot.into()], "suspending_accept_err_free")
        .llvm_ctx("hew_read_slot_free (register err) call")?;
    if let Some(reg) = reg {
        let status = fn_ctx.call_runtime_int(
            "hew_await_cancel_status",
            &[reg.into()],
            "suspending_accept_register_status",
            "hew_await_cancel_status (accept register err) call",
        )?;
        let cancelled = fn_ctx
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                i32_ty.const_int(2, false),
                "suspending_accept_register_cancelled",
            )
            .llvm_ctx("suspending accept register-cancelled compare")?;
        let cancelled_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_accept_register_cancelled");
        let timeout_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_accept_register_timeout");
        fn_ctx
            .builder
            .build_conditional_branch(cancelled, cancelled_bb, timeout_bb)
            .llvm_ctx("suspending accept register-error status branch")?;

        let result_dest = term.deadline_result_dest.ok_or_else(|| {
            CodegenError::FailClosed("SuspendingAccept register error missing Result dest".into())
        })?;
        let error_dest = term.error_dest.ok_or_else(|| {
            CodegenError::FailClosed("SuspendingAccept register error missing NetError dest".into())
        })?;

        fn_ctx.builder.position_at_end(cancelled_bb);
        fn_ctx.call_runtime_void(
            "hew_await_cancel_free",
            &[reg.into()],
            "suspending_accept_err_cancelled_reg_free",
            "hew_await_cancel_free (accept register cancelled) call",
        )?;
        emit_read_deadline_cancelled_err(fn_ctx, result_dest, error_dest)?;
        fn_ctx
            .builder
            .build_unconditional_branch(resume_bb)
            .llvm_ctx("suspending accept register-cancelled br")?;

        fn_ctx.builder.position_at_end(timeout_bb);
        fn_ctx.call_runtime_void(
            "hew_await_cancel_free",
            &[reg.into()],
            "suspending_accept_err_timeout_reg_free",
            "hew_await_cancel_free (accept register timeout) call",
        )?;
        emit_read_deadline_timeout_err(fn_ctx, result_dest, error_dest)?;
        fn_ctx
            .builder
            .build_unconditional_branch(resume_bb)
            .llvm_ctx("suspending accept register-timeout br")?;
    } else {
        store_invalid_connection(fn_ctx, term.result_dest)?;
        fn_ctx
            .builder
            .build_unconditional_branch(resume_bb)
            .llvm_ctx("suspending accept register-err br")?;
    }

    // ── do_suspend: park the continuation (non-final suspend). ─────────────────
    fn_ctx.builder.position_at_end(do_suspend_bb);
    if let (Some(reg), Some(ns)) = (reg, term.deadline_ns) {
        let delay_ms = (ns / 1_000_000).max(1) as u64;
        let tw_fn = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_global_timer_wheel",
        )?;
        let tw = fn_ctx
            .builder
            .build_call(tw_fn, &[], "suspending_accept_deadline_tw")
            .llvm_ctx("hew_global_timer_wheel call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("hew_global_timer_wheel returned void".into()))?
            .into_pointer_value();
        let schedule = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_schedule_deadline_ms",
        )?;
        let delay_val = i64_ty.const_int(delay_ms, false);
        let sched_rc = fn_ctx
            .builder
            .build_call(
                schedule,
                &[reg.into(), tw.into(), delay_val.into()],
                "suspending_accept_schedule_deadline",
            )
            .llvm_ctx("hew_await_cancel_schedule_deadline_ms call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_await_cancel_schedule_deadline_ms returned void".into(),
                )
            })?
            .into_int_value();
        let armed = fn_ctx
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                sched_rc,
                sched_rc.get_type().const_zero(),
                "suspending_accept_deadline_armed",
            )
            .llvm_ctx("suspending accept schedule-armed compare")?;
        let deadline_proceed_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_accept_deadline_proceed");
        let deadline_check_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_accept_deadline_check");
        fn_ctx
            .builder
            .build_conditional_branch(armed, deadline_proceed_bb, deadline_check_bb)
            .llvm_ctx("suspending accept schedule-armed branch")?;

        fn_ctx.builder.position_at_end(deadline_check_bb);
        let status_fn = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_status",
        )?;
        let sched_status = fn_ctx
            .builder
            .build_call(
                status_fn,
                &[reg.into()],
                "suspending_accept_schedule_status",
            )
            .llvm_ctx("hew_await_cancel_status (schedule) call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| {
                CodegenError::FailClosed("hew_await_cancel_status returned void".into())
            })?
            .into_int_value();
        let accept_completed = fn_ctx
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                sched_status,
                i32_ty.const_int(1, false),
                "suspending_accept_schedule_accept_completed",
            )
            .llvm_ctx("suspending accept schedule accept-completed compare")?;
        let deadline_failclosed_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_accept_deadline_failclosed");
        fn_ctx
            .builder
            .build_conditional_branch(
                accept_completed,
                deadline_proceed_bb,
                deadline_failclosed_bb,
            )
            .llvm_ctx("suspending accept schedule accept-completed branch")?;

        fn_ctx.builder.position_at_end(deadline_failclosed_bb);
        let reg_cancel = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_cancel",
        )?;
        let cancelled_status = i32_ty.const_int(2, false); // AwaitCancelStatus::Cancelled
        let no_wake = i32_ty.const_zero();
        fn_ctx
            .builder
            .build_call(
                reg_cancel,
                &[reg.into(), cancelled_status.into(), no_wake.into()],
                "suspending_accept_failclosed_reg_cancel",
            )
            .llvm_ctx("hew_await_cancel_cancel (accept fail-closed) call")?;
        let reg_free = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_free",
        )?;
        fn_ctx
            .builder
            .build_call(
                reg_free,
                &[reg.into()],
                "suspending_accept_failclosed_reg_free",
            )
            .llvm_ctx("hew_await_cancel_free (accept fail-closed) call")?;
        fn_ctx
            .builder
            .build_call(
                slot_cancel,
                &[slot.into()],
                "suspending_accept_failclosed_slot_cancel",
            )
            .llvm_ctx("hew_read_slot_cancel (fail-closed) call")?;
        fn_ctx
            .builder
            .build_call(
                slot_free,
                &[slot.into()],
                "suspending_accept_failclosed_free",
            )
            .llvm_ctx("hew_read_slot_free (fail-closed) call")?;
        let result_dest = term.deadline_result_dest.ok_or_else(|| {
            CodegenError::FailClosed("SuspendingAccept fail-closed missing Result dest".into())
        })?;
        let error_dest = term.error_dest.ok_or_else(|| {
            CodegenError::FailClosed("SuspendingAccept fail-closed missing NetError dest".into())
        })?;
        emit_read_deadline_timeout_err(fn_ctx, result_dest, error_dest)?;
        fn_ctx
            .builder
            .build_unconditional_branch(resume_bb)
            .llvm_ctx("suspending accept deadline fail-closed br")?;

        fn_ctx.builder.position_at_end(deadline_proceed_bb);
    }
    let accept_bind_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "suspending_accept_bind");
    // ── abandon_cleanup edge: when a parked SuspendingAccept continuation is
    // DESTROYED without resuming, `coro.suspend`'s case-1 edge runs. The abandon
    // continuation CANCELS + FREES the creator ref before joining the shared
    // cleanup (identical discipline to the read ramp).
    emit_suspend_point(
        fn_ctx,
        coro,
        parent,
        accept_bind_bb,
        "suspending_accept",
        "suspending_accept_abandon_cleanup",
        "suspending accept abandon -> shared cleanup br",
        || {
            // ── abandon_cleanup: cancel + free the read slot, then join the shared
            // coro cleanup (frame-free + coro.end). ─────────────────────────────
            if let Some(reg) = reg {
                let reg_cancel = intern_runtime_decl(
                    fn_ctx.ctx,
                    fn_ctx.llvm_mod,
                    &mut fn_ctx.runtime_decls.borrow_mut(),
                    "hew_await_cancel_cancel",
                )?;
                let cancelled_status = i32_ty.const_int(2, false); // AwaitCancelStatus::Cancelled
                let no_wake = i32_ty.const_zero();
                fn_ctx
                    .builder
                    .build_call(
                        reg_cancel,
                        &[reg.into(), cancelled_status.into(), no_wake.into()],
                        "suspending_accept_abandon_reg_cancel",
                    )
                    .llvm_ctx("hew_await_cancel_cancel (accept abandon) call")?;
                let reg_free = intern_runtime_decl(
                    fn_ctx.ctx,
                    fn_ctx.llvm_mod,
                    &mut fn_ctx.runtime_decls.borrow_mut(),
                    "hew_await_cancel_free",
                )?;
                fn_ctx
                    .builder
                    .build_call(
                        reg_free,
                        &[reg.into()],
                        "suspending_accept_abandon_reg_free",
                    )
                    .llvm_ctx("hew_await_cancel_free (accept abandon) call")?;
            }
            fn_ctx
                .builder
                .build_call(
                    slot_cancel,
                    &[slot.into()],
                    "suspending_accept_abandon_cancel",
                )
                .llvm_ctx("hew_read_slot_cancel (abandon) call")?;
            fn_ctx
                .builder
                .build_call(slot_free, &[slot.into()], "suspending_accept_abandon_free")
                .llvm_ctx("hew_read_slot_free (abandon) call")?;
            Ok(())
        },
        || emit_suspending_accept_bind(fn_ctx, &term, slot, slot_free, reg, resume_bb, parent),
    )?;

    Ok(())
}

/// Resume-bind continuation of [`emit_suspending_accept_terminator`]: the reactor
/// resumed us (enqueue_resume). The accepted connection handle is already
/// deposited in `slot`; `hew_read_slot_take_handle` returns it (as the
/// pointer-shaped `Connection`) on the fast path. Store it into `result_dest`,
/// release the creator ref, and branch to the MIR resume block. When a deadline
/// is active, first resolve the arbiter (complete vs timed-out) before binding.
/// Split out so the suspend-point seam owns the suspend + abandon scaffolding
/// while this owns the value routing.
#[allow(
    clippy::too_many_lines,
    reason = "the full accept-bind edge — deadline arbiter + the Connection \
              binding — is kept in one place so the deadline resolution and the \
              value routing it gates are read together"
)]
fn emit_suspending_accept_bind<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    term: &SuspendingAcceptEmit,
    slot: inkwell::values::PointerValue<'ctx>,
    slot_free: FunctionValue<'ctx>,
    reg: Option<inkwell::values::PointerValue<'ctx>>,
    resume_bb: inkwell::basic_block::BasicBlock<'ctx>,
    parent: FunctionValue<'ctx>,
) -> CodegenResult<()> {
    let i32_ty = fn_ctx.ctx.i32_type();
    if let Some(reg) = reg {
        let complete = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_complete",
        )?;
        fn_ctx
            .builder
            .build_call(
                complete,
                &[reg.into()],
                "suspending_accept_deadline_complete",
            )
            .llvm_ctx("hew_await_cancel_complete call")?;
        let status_fn = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_status",
        )?;
        let status = fn_ctx
            .builder
            .build_call(
                status_fn,
                &[reg.into()],
                "suspending_accept_deadline_status",
            )
            .llvm_ctx("hew_await_cancel_status call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| {
                CodegenError::FailClosed("hew_await_cancel_status returned void".into())
            })?
            .into_int_value();
        // A shutdown sweep resolves the common arbiter as Cancelled before
        // waking the parked actor. Route that status to the typed NetError arm
        // instead of treating the invalid accept handle as a successful value.
        let cancelled = fn_ctx
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                i32_ty.const_int(2, false),
                "suspending_accept_cancelled",
            )
            .llvm_ctx("suspending accept cancelled compare")?;
        let cancelled_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_accept_cancelled");
        let timeout_check_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_accept_timeout_check");
        fn_ctx
            .builder
            .build_conditional_branch(cancelled, cancelled_bb, timeout_check_bb)
            .llvm_ctx("suspending accept cancelled branch")?;

        fn_ctx.builder.position_at_end(cancelled_bb);
        let reg_free_cancel = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_free",
        )?;
        fn_ctx
            .builder
            .build_call(
                reg_free_cancel,
                &[reg.into()],
                "suspending_accept_cancelled_reg_free",
            )
            .llvm_ctx("hew_await_cancel_free (accept cancelled) call")?;
        fn_ctx
            .builder
            .build_call(
                slot_free,
                &[slot.into()],
                "suspending_accept_cancelled_free",
            )
            .llvm_ctx("hew_read_slot_free (cancelled) call")?;
        let cancel_result_dest = term.deadline_result_dest.ok_or_else(|| {
            CodegenError::FailClosed("SuspendingAccept cancelled missing Result dest".into())
        })?;
        let cancel_error_dest = term.error_dest.ok_or_else(|| {
            CodegenError::FailClosed("SuspendingAccept cancelled missing NetError dest".into())
        })?;
        emit_read_deadline_cancelled_err(fn_ctx, cancel_result_dest, cancel_error_dest)?;
        fn_ctx
            .builder
            .build_unconditional_branch(resume_bb)
            .llvm_ctx("suspending accept cancelled br")?;

        fn_ctx.builder.position_at_end(timeout_check_bb);
        let timed_out = fn_ctx
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                i32_ty.const_int(3, false),
                "suspending_accept_timed_out",
            )
            .llvm_ctx("suspending accept timed-out compare")?;
        let timeout_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_accept_timeout");
        let accept_proceed_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_accept_proceed");
        fn_ctx
            .builder
            .build_conditional_branch(timed_out, timeout_bb, accept_proceed_bb)
            .llvm_ctx("suspending accept deadline branch")?;

        fn_ctx.builder.position_at_end(timeout_bb);
        let reg_free = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_free",
        )?;
        fn_ctx
            .builder
            .build_call(
                reg_free,
                &[reg.into()],
                "suspending_accept_timeout_reg_free",
            )
            .llvm_ctx("hew_await_cancel_free (accept timeout) call")?;
        fn_ctx
            .builder
            .build_call(slot_free, &[slot.into()], "suspending_accept_timeout_free")
            .llvm_ctx("hew_read_slot_free (timeout) call")?;
        let result_dest = term.deadline_result_dest.ok_or_else(|| {
            CodegenError::FailClosed("SuspendingAccept timeout missing Result dest".into())
        })?;
        let error_dest = term.error_dest.ok_or_else(|| {
            CodegenError::FailClosed("SuspendingAccept timeout missing NetError dest".into())
        })?;
        emit_read_deadline_timeout_err(fn_ctx, result_dest, error_dest)?;
        fn_ctx
            .builder
            .build_unconditional_branch(resume_bb)
            .llvm_ctx("suspending accept timeout br")?;

        fn_ctx.builder.position_at_end(accept_proceed_bb);
    }
    let conn = fn_ctx.call_runtime_basic(
        "hew_read_slot_take_handle",
        &[slot.into()],
        "suspending_accept_take",
        "hew_read_slot_take_handle call",
    )?;
    let (dest_ptr, dest_ty) = place_pointer(fn_ctx, term.result_dest)?;
    if !matches!(dest_ty, BasicTypeEnum::PointerType(_)) {
        return Err(CodegenError::FailClosed(format!(
            "SuspendingAccept result_dest must be a pointer-shaped Connection slot, got {dest_ty:?}"
        )));
    }
    fn_ctx
        .builder
        .build_store(dest_ptr, conn)
        .llvm_ctx("suspending accept connection store")?;
    if let Some(result_dest) = term.deadline_result_dest {
        // `await ln.accept() | after d` success path: wrap the accepted Connection
        // in `Ok(_)` into the Result<Connection, NetError> destination.
        emit_result_ok(fn_ctx, result_dest, Some(term.result_dest))?;
    }
    fn_ctx
        .builder
        .build_call(slot_free, &[slot.into()], "suspending_accept_ok_free")
        .llvm_ctx("hew_read_slot_free (ok) call")?;
    if let Some(reg) = reg {
        let reg_free = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_free",
        )?;
        fn_ctx
            .builder
            .build_call(reg_free, &[reg.into()], "suspending_accept_ok_reg_free")
            .llvm_ctx("hew_await_cancel_free (accept ok) call")?;
    }
    fn_ctx
        .builder
        .build_unconditional_branch(resume_bb)
        .llvm_ctx("suspending accept ok br")?;

    Ok(())
}

/// Emit the caller-side non-blocking `await stream.recv()` (NEW-7
/// `Terminator::SuspendingStreamNext`). The channel-readiness analogue of
/// [`emit_suspending_read_terminator`].
///
/// Shape (the suspending stream-recv ramp):
/// ```text
///   self    = hew_actor_self()                       ; the parked-cont actor
///   stream  = <load stream handle>                   ; pointer-shaped (opaque)
///   slot    = hew_read_slot_new()
///   rc      = hew_stream_await_next(stream, self, slot)
///   br (rc == 0) -> do_suspend, bind                 ; 0 = parked, 1 = ready-now
/// do_suspend:                                        ; coro.suspend (non-final)
///   switch coro.suspend [default -> return handle, 0 -> bind, 1 -> abandon]
/// abandon:                                           ; parked cont destroyed
///   hew_read_slot_cancel(slot)
///   hew_stream_detach_await(stream, slot)            ; release the core ref
///   hew_read_slot_free(slot); br shared cleanup
/// bind:                                              ; ready-now OR resumed
///   rc = hew_stream_pop_layout(stream, out, witness) ; pop the queued item
///   <select Option<T> tag from rc into result_dest>  ; rc 0 -> None, 1 -> Some
///   hew_read_slot_free(slot)                         ; release the creator ref
///   br resume_bb
/// ```
/// The item travels through the channel QUEUE across the suspend (NOT the slot —
/// the slot is a pure readiness signal); on resume `hew_stream_pop_layout` pops
/// it exactly once on the consumer's own edge. Slot refs: `new` (+1 creator);
/// `hew_stream_await_next` takes the channel core's in-flight ref only on the
/// park path; the single `hew_read_slot_free` on each terminal edge releases the
/// creator ref. The abandon edge cancels + detaches before freeing so a racing
/// producer deposit drops its signal and a freed consumer is never woken.
#[allow(
    clippy::too_many_lines,
    reason = "the full caller-side recv ramp — registration + suspend + the \
              resume-edge Option<bytes> binding — is kept in one place so the \
              suspend point and the value routing it depends on are read together"
)]
pub(crate) fn emit_suspending_stream_next_terminator<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    term: SuspendingStreamNextEmit,
) -> CodegenResult<()> {
    let coro = fn_ctx.coro.ok_or_else(|| {
        CodegenError::FailClosed(
            "Terminator::SuspendingStreamNext reached codegen but the function \
             carries no coro prologue state — lower_function must detect the \
             suspend carrier (has_suspend) and emit the prologue before the body"
                .into(),
        )
    })?;
    let resume_bb = *fn_ctx.blocks.get(&term.resume).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "SuspendingStreamNext resume target bb{} not found",
            term.resume
        ))
    })?;
    if !fn_ctx.blocks.contains_key(&term.cleanup) {
        return Err(CodegenError::FailClosed(format!(
            "SuspendingStreamNext cleanup target bb{} not found",
            term.cleanup
        )));
    }
    if term.deadline_ns.is_some()
        && (term.deadline_result_dest.is_none() || term.error_dest.is_none())
    {
        return Err(CodegenError::FailClosed(
            "SuspendingStreamNext deadline reached codegen without Result/TimeoutError destinations"
                .into(),
        ));
    }
    let Place::Local(dest_local) = term.result_dest else {
        return Err(CodegenError::FailClosed(format!(
            "SuspendingStreamNext result_dest must be a local Option<T> slot, got {:?}",
            term.result_dest
        )));
    };

    let i32_ty = fn_ctx.ctx.i32_type();
    let i64_ty = fn_ctx.ctx.i64_type();

    // self = the awaiting actor (the live thread-local context, NOT a spilled
    // param) — the same single-authority accessor the SuspendingRead/Ask ramps
    // use across a suspend.
    let actor_self_fn = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_actor_self",
    )?;
    let self_actor = fn_ctx
        .builder
        .build_call(actor_self_fn, &[], "suspending_stream_next_self")
        .llvm_ctx("hew_actor_self call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_actor_self returned void".into()))?
        .into_pointer_value();
    let stream_ptr = load_duplex_handle(fn_ctx, term.stream, "suspending_stream_next stream")?;

    let slot_new = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_read_slot_new",
    )?;
    let slot = fn_ctx
        .builder
        .build_call(slot_new, &[], "suspending_stream_next_slot")
        .llvm_ctx("hew_read_slot_new call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_read_slot_new returned void".into()))?
        .into_pointer_value();

    // ── Deadline cancel registration (when `| after d` is present). ─────────────
    // Wire before `hew_stream_await_next` so the registration is in place before
    // the reactor can fire. Uses `hew_stream_recv_cancel_cleanup` (which calls
    // hew_read_slot_cancel + hew_stream_detach_await) as the one-shot CAS arbiter
    // cleanup callback.
    //
    // The cleanup requires a `HewStreamRecvCancelCtx { slot, stream }` struct
    // (two pointer-sized fields) so it can perform both the slot cancel AND the
    // stream-core detach in one atomic callback. Allocate that struct on the
    // coroutine frame (alloca) and populate it before registering the cancel.
    // The coroutine frame's lifetime spans the suspend point, so the alloca is
    // live when the deadline timer fires and calls the cleanup.
    let reg: Option<inkwell::values::PointerValue<'ctx>> = if term.deadline_ns.is_some() {
        let cleanup_fn = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_stream_recv_cancel_cleanup",
        )?;
        let cleanup_ptr = cleanup_fn.as_global_value().as_pointer_value();
        // Allocate HewStreamRecvCancelCtx { slot: ptr, stream: ptr }.
        let ptr_ty = fn_ctx.ctx.ptr_type(inkwell::AddressSpace::default());
        let cancel_ctx_ty = fn_ctx
            .ctx
            .struct_type(&[ptr_ty.into(), ptr_ty.into()], false);
        let cancel_ctx = fn_ctx
            .builder
            .build_alloca(cancel_ctx_ty, "suspending_stream_next_cancel_ctx")
            .llvm_ctx("stream next cancel ctx alloca")?;
        // Store slot into field 0 of the cancel context.
        let ctx_slot_ptr = fn_ctx
            .builder
            .build_struct_gep(
                cancel_ctx_ty,
                cancel_ctx,
                0,
                "suspending_stream_next_cancel_ctx_slot",
            )
            .llvm_ctx("stream next cancel ctx slot gep")?;
        fn_ctx
            .builder
            .build_store(ctx_slot_ptr, slot)
            .llvm_ctx("stream next cancel ctx slot store")?;
        // Store stream into field 1 of the cancel context.
        let ctx_stream_ptr = fn_ctx
            .builder
            .build_struct_gep(
                cancel_ctx_ty,
                cancel_ctx,
                1,
                "suspending_stream_next_cancel_ctx_stream",
            )
            .llvm_ctx("stream next cancel ctx stream gep")?;
        fn_ctx
            .builder
            .build_store(ctx_stream_ptr, stream_ptr)
            .llvm_ctx("stream next cancel ctx stream store")?;
        let reg = emit_await_cancel_new(
            fn_ctx,
            self_actor,
            cleanup_ptr,
            cancel_ctx,
            "suspending_stream_next_deadline_reg",
        )?;
        let set_await_cancel = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_read_slot_set_await_cancel",
        )?;
        fn_ctx
            .builder
            .build_call(
                set_await_cancel,
                &[slot.into(), reg.into()],
                "suspending_stream_next_set_await_cancel",
            )
            .llvm_ctx("hew_read_slot_set_await_cancel call")?;
        Some(reg)
    } else {
        None
    };

    let rc = fn_ctx.call_runtime_int(
        "hew_stream_await_next",
        &[stream_ptr.into(), self_actor.into(), slot.into()],
        "suspending_stream_next_register",
        "hew_stream_await_next call",
    )?;

    let parent = fn_ctx
        .builder
        .get_insert_block()
        .and_then(|bb| bb.get_parent())
        .ok_or_else(|| {
            CodegenError::Llvm("suspending stream-next block has no parent function".into())
        })?;
    let do_suspend_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "suspending_stream_next_suspend");
    let bind_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "suspending_stream_next_bind");
    // rc == 0 (STREAM_AWAIT_SUSPEND) → park; else (READY) bind immediately.
    let is_suspend = fn_ctx
        .builder
        .build_int_compare(
            IntPredicate::EQ,
            rc,
            rc.get_type().const_zero(),
            "suspending_stream_next_is_suspend",
        )
        .llvm_ctx("suspending stream-next suspend compare")?;
    fn_ctx
        .builder
        .build_conditional_branch(is_suspend, do_suspend_bb, bind_bb)
        .llvm_ctx("suspending stream-next register branch")?;

    let slot_cancel = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_read_slot_cancel",
    )?;
    let slot_free = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_read_slot_free",
    )?;
    let detach_await = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_stream_detach_await",
    )?;

    // ── do_suspend: park the continuation (non-final). Default → return the coro
    // handle to the trampoline; case 0 → bind; case 1 → abandon teardown. ──────
    fn_ctx.builder.position_at_end(do_suspend_bb);
    if let (Some(reg), Some(ns)) = (reg, term.deadline_ns) {
        let delay_ms = (ns / 1_000_000).max(1) as u64;
        let tw_fn = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_global_timer_wheel",
        )?;
        let tw = fn_ctx
            .builder
            .build_call(tw_fn, &[], "suspending_stream_next_deadline_tw")
            .llvm_ctx("hew_global_timer_wheel call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("hew_global_timer_wheel returned void".into()))?
            .into_pointer_value();
        let schedule = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_schedule_deadline_ms",
        )?;
        let delay_val = i64_ty.const_int(delay_ms, false);
        let sched_rc = fn_ctx
            .builder
            .build_call(
                schedule,
                &[reg.into(), tw.into(), delay_val.into()],
                "suspending_stream_next_schedule_deadline",
            )
            .llvm_ctx("hew_await_cancel_schedule_deadline_ms call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_await_cancel_schedule_deadline_ms returned void".into(),
                )
            })?
            .into_int_value();
        let armed = fn_ctx
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                sched_rc,
                sched_rc.get_type().const_zero(),
                "suspending_stream_next_deadline_armed",
            )
            .llvm_ctx("suspending stream-next schedule-armed compare")?;
        let deadline_proceed_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_stream_next_deadline_proceed");
        let deadline_check_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_stream_next_deadline_check");
        fn_ctx
            .builder
            .build_conditional_branch(armed, deadline_proceed_bb, deadline_check_bb)
            .llvm_ctx("suspending stream-next schedule-armed branch")?;

        fn_ctx.builder.position_at_end(deadline_check_bb);
        let status_fn = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_status",
        )?;
        let sched_status = fn_ctx
            .builder
            .build_call(
                status_fn,
                &[reg.into()],
                "suspending_stream_next_schedule_status",
            )
            .llvm_ctx("hew_await_cancel_status (schedule) call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| {
                CodegenError::FailClosed("hew_await_cancel_status returned void".into())
            })?
            .into_int_value();
        let recv_completed = fn_ctx
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                sched_status,
                i32_ty.const_int(1, false),
                "suspending_stream_next_schedule_recv_completed",
            )
            .llvm_ctx("suspending stream-next schedule recv-completed compare")?;
        let deadline_failclosed_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_stream_next_deadline_failclosed");
        fn_ctx
            .builder
            .build_conditional_branch(recv_completed, deadline_proceed_bb, deadline_failclosed_bb)
            .llvm_ctx("suspending stream-next schedule recv-completed branch")?;

        fn_ctx.builder.position_at_end(deadline_failclosed_bb);
        let reg_cancel = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_cancel",
        )?;
        let cancelled_status = i32_ty.const_int(2, false); // AwaitCancelStatus::Cancelled
        let no_wake = i32_ty.const_zero();
        fn_ctx
            .builder
            .build_call(
                reg_cancel,
                &[reg.into(), cancelled_status.into(), no_wake.into()],
                "suspending_stream_next_failclosed_reg_cancel",
            )
            .llvm_ctx("hew_await_cancel_cancel (stream next fail-closed) call")?;
        let reg_free = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_free",
        )?;
        fn_ctx
            .builder
            .build_call(
                reg_free,
                &[reg.into()],
                "suspending_stream_next_failclosed_reg_free",
            )
            .llvm_ctx("hew_await_cancel_free (stream next fail-closed) call")?;
        fn_ctx
            .builder
            .build_call(
                slot_cancel,
                &[slot.into()],
                "suspending_stream_next_failclosed_slot_cancel",
            )
            .llvm_ctx("hew_read_slot_cancel (fail-closed) call")?;
        fn_ctx
            .builder
            .build_call(
                slot_free,
                &[slot.into()],
                "suspending_stream_next_failclosed_free",
            )
            .llvm_ctx("hew_read_slot_free (fail-closed) call")?;
        let result_dest = term.deadline_result_dest.ok_or_else(|| {
            CodegenError::FailClosed("SuspendingStreamNext fail-closed missing Result dest".into())
        })?;
        let error_dest = term.error_dest.ok_or_else(|| {
            CodegenError::FailClosed(
                "SuspendingStreamNext fail-closed missing TimeoutError dest".into(),
            )
        })?;
        emit_recv_deadline_timeout_err(fn_ctx, result_dest, error_dest)?;
        fn_ctx
            .builder
            .build_unconditional_branch(resume_bb)
            .llvm_ctx("suspending stream-next deadline fail-closed br")?;

        fn_ctx.builder.position_at_end(deadline_proceed_bb);
    }
    // ── abandon_cleanup edge: cancel the slot (a racing producer deposit drops
    // its signal + skips the wake), detach the channel-await registration (release
    // the core's in-flight ref), free the creator ref, then join shared cleanup.
    // ── bind edge: ready-now OR resumed. Pop the queued item through the
    // layout-witness entry (`hew_stream_pop_layout` decodes the element directly
    // into the Option<T> Some payload slot per the element witness), release the
    // creator ref, branch to the MIR resume. One mechanism for every describable
    // element type — mirror of the `SuspendingChannelRecv` ramp's bind edge.
    emit_suspend_point(
        fn_ctx,
        coro,
        parent,
        bind_bb,
        "suspending_stream_next",
        "suspending_stream_next_abandon_cleanup",
        "suspending stream-next abandon -> shared cleanup br",
        || {
            if let Some(reg) = reg {
                let reg_cancel = intern_runtime_decl(
                    fn_ctx.ctx,
                    fn_ctx.llvm_mod,
                    &mut fn_ctx.runtime_decls.borrow_mut(),
                    "hew_await_cancel_cancel",
                )?;
                let cancelled_status = i32_ty.const_int(2, false);
                let no_wake = i32_ty.const_zero();
                fn_ctx
                    .builder
                    .build_call(
                        reg_cancel,
                        &[reg.into(), cancelled_status.into(), no_wake.into()],
                        "suspending_stream_next_abandon_reg_cancel",
                    )
                    .llvm_ctx("hew_await_cancel_cancel (stream next abandon) call")?;
                let reg_free = intern_runtime_decl(
                    fn_ctx.ctx,
                    fn_ctx.llvm_mod,
                    &mut fn_ctx.runtime_decls.borrow_mut(),
                    "hew_await_cancel_free",
                )?;
                fn_ctx
                    .builder
                    .build_call(
                        reg_free,
                        &[reg.into()],
                        "suspending_stream_next_abandon_reg_free",
                    )
                    .llvm_ctx("hew_await_cancel_free (stream next abandon) call")?;
            }
            fn_ctx
                .builder
                .build_call(
                    slot_cancel,
                    &[slot.into()],
                    "suspending_stream_next_abandon_cancel",
                )
                .llvm_ctx("hew_read_slot_cancel (abandon) call")?;
            fn_ctx
                .builder
                .build_call(
                    detach_await,
                    &[stream_ptr.into(), slot.into()],
                    "suspending_stream_next_abandon_detach",
                )
                .llvm_ctx("hew_stream_detach_await (abandon) call")?;
            fn_ctx
                .builder
                .build_call(
                    slot_free,
                    &[slot.into()],
                    "suspending_stream_next_abandon_free",
                )
                .llvm_ctx("hew_read_slot_free (abandon) call")?;
            Ok(())
        },
        || {
            emit_suspending_stream_next_bind(
                fn_ctx, &term, stream_ptr, slot, slot_free, dest_local, reg, resume_bb, parent,
            )
        },
    )?;

    Ok(())
}

/// Resume-bind continuation of [`emit_suspending_stream_next_terminator`]:
/// ready-now or resumed. Resolve the deadline arbiter (timeout binds
/// `Err(TimeoutError)`), pop the queued item through `hew_stream_pop_layout`,
/// release the creator ref, and branch to the MIR resume block. Split out so the
/// suspend-point seam owns the suspend + abandon scaffolding while this owns the
/// value routing.
#[allow(
    clippy::too_many_arguments,
    clippy::too_many_lines,
    reason = "the full stream-recv bind edge — deadline arbiter + Option<T> \
              layout pop — is kept in one place so the deadline resolution and \
              the value routing it gates are read together"
)]
fn emit_suspending_stream_next_bind<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    term: &SuspendingStreamNextEmit,
    stream_ptr: inkwell::values::PointerValue<'ctx>,
    slot: inkwell::values::PointerValue<'ctx>,
    slot_free: FunctionValue<'ctx>,
    dest_local: u32,
    reg: Option<inkwell::values::PointerValue<'ctx>>,
    resume_bb: inkwell::basic_block::BasicBlock<'ctx>,
    parent: FunctionValue<'ctx>,
) -> CodegenResult<()> {
    let i32_ty = fn_ctx.ctx.i32_type();
    if let Some(reg) = reg {
        // Deadline active: resolve the arbiter (complete → check status).
        let complete = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_complete",
        )?;
        fn_ctx
            .builder
            .build_call(
                complete,
                &[reg.into()],
                "suspending_stream_next_deadline_complete",
            )
            .llvm_ctx("hew_await_cancel_complete call")?;
        let status_fn = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_status",
        )?;
        let status = fn_ctx
            .builder
            .build_call(
                status_fn,
                &[reg.into()],
                "suspending_stream_next_deadline_status",
            )
            .llvm_ctx("hew_await_cancel_status call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| {
                CodegenError::FailClosed("hew_await_cancel_status returned void".into())
            })?
            .into_int_value();
        let timed_out = fn_ctx
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                i32_ty.const_int(3, false), // AwaitCancelStatus::TimedOut = 3
                "suspending_stream_next_timed_out",
            )
            .llvm_ctx("suspending stream-next timed-out compare")?;
        let timeout_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_stream_next_timeout");
        let stream_proceed_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_stream_next_proceed");
        fn_ctx
            .builder
            .build_conditional_branch(timed_out, timeout_bb, stream_proceed_bb)
            .llvm_ctx("suspending stream-next deadline branch")?;

        fn_ctx.builder.position_at_end(timeout_bb);
        let reg_free = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_free",
        )?;
        fn_ctx
            .builder
            .build_call(
                reg_free,
                &[reg.into()],
                "suspending_stream_next_timeout_reg_free",
            )
            .llvm_ctx("hew_await_cancel_free (stream next timeout) call")?;
        fn_ctx
            .builder
            .build_call(
                slot_free,
                &[slot.into()],
                "suspending_stream_next_timeout_free",
            )
            .llvm_ctx("hew_read_slot_free (timeout) call")?;
        let result_dest = term.deadline_result_dest.ok_or_else(|| {
            CodegenError::FailClosed("SuspendingStreamNext timeout missing Result dest".into())
        })?;
        let error_dest = term.error_dest.ok_or_else(|| {
            CodegenError::FailClosed(
                "SuspendingStreamNext timeout missing TimeoutError dest".into(),
            )
        })?;
        emit_recv_deadline_timeout_err(fn_ctx, result_dest, error_dest)?;
        fn_ctx
            .builder
            .build_unconditional_branch(resume_bb)
            .llvm_ctx("suspending stream-next timeout br")?;

        fn_ctx.builder.position_at_end(stream_proceed_bb);
    }
    store_recv_option_via_layout(
        fn_ctx,
        stream_ptr,
        dest_local,
        "hew_stream_pop_layout",
        &term.elem_ty,
    )?;
    fn_ctx
        .builder
        .build_call(
            slot_free,
            &[slot.into()],
            "suspending_stream_next_bind_free",
        )
        .llvm_ctx("hew_read_slot_free (bind) call")?;
    if let Some(result_dest) = term.deadline_result_dest {
        // `await stream.recv() | after d` success path: wrap Option<T> in `Ok(_)`.
        emit_result_ok(fn_ctx, result_dest, Some(term.result_dest))?;
    }
    if let Some(reg) = reg {
        let reg_free = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_free",
        )?;
        fn_ctx
            .builder
            .build_call(
                reg_free,
                &[reg.into()],
                "suspending_stream_next_ok_reg_free",
            )
            .llvm_ctx("hew_await_cancel_free (stream next ok) call")?;
    }
    fn_ctx
        .builder
        .build_unconditional_branch(resume_bb)
        .llvm_ctx("suspending stream-next bind -> resume br")?;

    Ok(())
}

/// Emit the caller-side non-blocking `await rx.recv()` over a `std::channel`
/// `Receiver<T>` (NEW-4 `Terminator::SuspendingChannelRecv`). The std-channel
/// analogue of [`emit_suspending_stream_next_terminator`].
///
/// Shape (the suspending channel-recv ramp):
/// ```text
///   self    = hew_actor_self()                       ; the parked-cont actor
///   rx      = <load receiver handle>                 ; pointer-shaped (opaque)
///   slot    = hew_read_slot_new()
///   rc      = hew_channel_await_recv(rx, self, slot)
///   br (rc == 0) -> do_suspend, bind                 ; 0 = parked, 1 = ready-now
/// do_suspend:                                        ; coro.suspend (non-final)
///   switch coro.suspend [default -> return handle, 0 -> bind, 1 -> abandon]
/// abandon:                                           ; parked cont destroyed
///   hew_read_slot_cancel(slot)
///   hew_channel_detach_recv(rx, slot)               ; release the core ref
///   hew_read_slot_free(slot); br shared cleanup
/// bind:                                              ; ready-now OR resumed
///   rc = hew_channel_try_recv_layout(rx, out, witness) ; rc 0 -> None, 1 -> Some
///   hew_read_slot_free(slot)                         ; release the creator ref
///   br resume_bb
/// ```
/// The item travels through the channel QUEUE across the suspend (NOT the slot —
/// the slot is a pure readiness signal); on resume the single consumer pops it
/// exactly once on its own edge via the non-blocking `try_recv` (an immediate
/// close → null / out-valid 0 → `None`). The receiver handle is BORROWED for
/// registration + pop — never consumed or double-closed. Slot refs mirror the
/// stream-recv ramp: `new` (+1 creator); `hew_channel_await_recv` takes the
/// channel core's in-flight ref only on the park path; the single
/// `hew_read_slot_free` on each terminal edge releases the creator ref. The
/// abandon edge cancels + detaches before freeing so a racing sender deposit
/// drops its signal and a freed consumer is never woken. The `Some(string)`
/// payload is the owned malloc'd pointer the MIR drop spine balances with
/// `hew_string_drop` on EVERY consumption path (match arm or discard), so a
/// non-match discard frees it exactly once — never the parked branch's
/// match-scrutinee-only leak fix.
#[allow(
    clippy::too_many_lines,
    reason = "the full caller-side recv ramp — registration + suspend + the \
              resume-edge Option<T> binding for both the string and int element \
              kinds — is kept in one place so the suspend point and the value \
              routing it depends on are read together"
)]
pub(crate) fn emit_suspending_channel_recv_terminator<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    term: SuspendingChannelRecvEmit,
) -> CodegenResult<()> {
    let coro = fn_ctx.coro.ok_or_else(|| {
        CodegenError::FailClosed(
            "Terminator::SuspendingChannelRecv reached codegen but the function \
             carries no coro prologue state — lower_function must detect the \
             suspend carrier (has_suspend) and emit the prologue before the body"
                .into(),
        )
    })?;
    let resume_bb = *fn_ctx.blocks.get(&term.resume).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "SuspendingChannelRecv resume target bb{} not found",
            term.resume
        ))
    })?;
    if !fn_ctx.blocks.contains_key(&term.cleanup) {
        return Err(CodegenError::FailClosed(format!(
            "SuspendingChannelRecv cleanup target bb{} not found",
            term.cleanup
        )));
    }
    if term.deadline_ns.is_some()
        && (term.deadline_result_dest.is_none() || term.error_dest.is_none())
    {
        return Err(CodegenError::FailClosed(
            "SuspendingChannelRecv deadline reached codegen without Result/TimeoutError destinations"
                .into(),
        ));
    }
    let Place::Local(dest_local) = term.result_dest else {
        return Err(CodegenError::FailClosed(format!(
            "SuspendingChannelRecv result_dest must be a local Option<T> slot, got {:?}",
            term.result_dest
        )));
    };

    let i32_ty = fn_ctx.ctx.i32_type();
    let i64_ty = fn_ctx.ctx.i64_type();

    // self = the awaiting actor (the live thread-local context) — the same
    // single-authority accessor the stream/read ramps use across a suspend.
    let actor_self_fn = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_actor_self",
    )?;
    let self_actor = fn_ctx
        .builder
        .build_call(actor_self_fn, &[], "suspending_channel_recv_self")
        .llvm_ctx("hew_actor_self call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_actor_self returned void".into()))?
        .into_pointer_value();
    let rx_ptr = load_duplex_handle(fn_ctx, term.receiver, "suspending_channel_recv rx")?;

    let slot_new = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_read_slot_new",
    )?;
    let slot = fn_ctx
        .builder
        .build_call(slot_new, &[], "suspending_channel_recv_slot")
        .llvm_ctx("hew_read_slot_new call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_read_slot_new returned void".into()))?
        .into_pointer_value();

    // ── Deadline cancel registration (when `| after d` is present). ─────────────
    // Wire before `hew_channel_await_recv` so the registration is in place before
    // the reactor can fire. Uses `hew_channel_recv_cancel_cleanup` (which calls
    // hew_read_slot_cancel + hew_channel_detach_recv) as the one-shot CAS arbiter
    // cleanup callback.
    //
    // The cleanup requires a `HewChannelRecvCancelCtx { slot, receiver }` struct
    // (two pointer-sized fields) so it can perform both the slot cancel AND the
    // channel-core detach in one atomic callback. Allocate that struct on the
    // coroutine frame (alloca) and populate it before registering the cancel.
    // The coroutine frame's lifetime spans the suspend point, so the alloca is
    // live when the deadline timer fires and calls the cleanup.
    let reg: Option<inkwell::values::PointerValue<'ctx>> = if term.deadline_ns.is_some() {
        let cleanup_fn = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_channel_recv_cancel_cleanup",
        )?;
        let cleanup_ptr = cleanup_fn.as_global_value().as_pointer_value();
        // Allocate HewChannelRecvCancelCtx { slot: ptr, receiver: ptr }.
        let ptr_ty = fn_ctx.ctx.ptr_type(inkwell::AddressSpace::default());
        let cancel_ctx_ty = fn_ctx
            .ctx
            .struct_type(&[ptr_ty.into(), ptr_ty.into()], false);
        let cancel_ctx = fn_ctx
            .builder
            .build_alloca(cancel_ctx_ty, "suspending_channel_recv_cancel_ctx")
            .llvm_ctx("channel recv cancel ctx alloca")?;
        // Store slot into field 0 of the cancel context.
        let ctx_slot_ptr = fn_ctx
            .builder
            .build_struct_gep(
                cancel_ctx_ty,
                cancel_ctx,
                0,
                "suspending_channel_recv_cancel_ctx_slot",
            )
            .llvm_ctx("channel recv cancel ctx slot gep")?;
        fn_ctx
            .builder
            .build_store(ctx_slot_ptr, slot)
            .llvm_ctx("channel recv cancel ctx slot store")?;
        // Store receiver into field 1 of the cancel context.
        let ctx_rx_ptr = fn_ctx
            .builder
            .build_struct_gep(
                cancel_ctx_ty,
                cancel_ctx,
                1,
                "suspending_channel_recv_cancel_ctx_rx",
            )
            .llvm_ctx("channel recv cancel ctx rx gep")?;
        fn_ctx
            .builder
            .build_store(ctx_rx_ptr, rx_ptr)
            .llvm_ctx("channel recv cancel ctx rx store")?;
        let reg = emit_await_cancel_new(
            fn_ctx,
            self_actor,
            cleanup_ptr,
            cancel_ctx,
            "suspending_channel_recv_deadline_reg",
        )?;
        let set_await_cancel = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_read_slot_set_await_cancel",
        )?;
        fn_ctx
            .builder
            .build_call(
                set_await_cancel,
                &[slot.into(), reg.into()],
                "suspending_channel_recv_set_await_cancel",
            )
            .llvm_ctx("hew_read_slot_set_await_cancel call")?;
        Some(reg)
    } else {
        None
    };

    let rc = fn_ctx.call_runtime_int(
        "hew_channel_await_recv",
        &[rx_ptr.into(), self_actor.into(), slot.into()],
        "suspending_channel_recv_register",
        "hew_channel_await_recv call",
    )?;

    let parent = fn_ctx
        .builder
        .get_insert_block()
        .and_then(|bb| bb.get_parent())
        .ok_or_else(|| {
            CodegenError::Llvm("suspending channel-recv block has no parent function".into())
        })?;
    let do_suspend_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "suspending_channel_recv_suspend");
    let bind_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "suspending_channel_recv_bind");
    // rc == 0 (CHANNEL_AWAIT_SUSPEND) → park; else (READY) bind immediately.
    let is_suspend = fn_ctx
        .builder
        .build_int_compare(
            IntPredicate::EQ,
            rc,
            rc.get_type().const_zero(),
            "suspending_channel_recv_is_suspend",
        )
        .llvm_ctx("suspending channel-recv suspend compare")?;
    fn_ctx
        .builder
        .build_conditional_branch(is_suspend, do_suspend_bb, bind_bb)
        .llvm_ctx("suspending channel-recv register branch")?;

    let slot_cancel = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_read_slot_cancel",
    )?;
    let slot_free = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_read_slot_free",
    )?;
    let detach_recv = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_channel_detach_recv",
    )?;

    // ── do_suspend: park the continuation (non-final). Default → return the coro
    // handle to the trampoline; case 0 → bind; case 1 → abandon teardown. ──────
    fn_ctx.builder.position_at_end(do_suspend_bb);
    if let (Some(reg), Some(ns)) = (reg, term.deadline_ns) {
        let delay_ms = (ns / 1_000_000).max(1) as u64;
        let tw_fn = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_global_timer_wheel",
        )?;
        let tw = fn_ctx
            .builder
            .build_call(tw_fn, &[], "suspending_channel_recv_deadline_tw")
            .llvm_ctx("hew_global_timer_wheel call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("hew_global_timer_wheel returned void".into()))?
            .into_pointer_value();
        let schedule = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_schedule_deadline_ms",
        )?;
        let delay_val = i64_ty.const_int(delay_ms, false);
        let sched_rc = fn_ctx
            .builder
            .build_call(
                schedule,
                &[reg.into(), tw.into(), delay_val.into()],
                "suspending_channel_recv_schedule_deadline",
            )
            .llvm_ctx("hew_await_cancel_schedule_deadline_ms call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_await_cancel_schedule_deadline_ms returned void".into(),
                )
            })?
            .into_int_value();
        let armed = fn_ctx
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                sched_rc,
                sched_rc.get_type().const_zero(),
                "suspending_channel_recv_deadline_armed",
            )
            .llvm_ctx("suspending channel-recv schedule-armed compare")?;
        let deadline_proceed_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_channel_recv_deadline_proceed");
        let deadline_check_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_channel_recv_deadline_check");
        fn_ctx
            .builder
            .build_conditional_branch(armed, deadline_proceed_bb, deadline_check_bb)
            .llvm_ctx("suspending channel-recv schedule-armed branch")?;

        fn_ctx.builder.position_at_end(deadline_check_bb);
        let status_fn = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_status",
        )?;
        let sched_status = fn_ctx
            .builder
            .build_call(
                status_fn,
                &[reg.into()],
                "suspending_channel_recv_schedule_status",
            )
            .llvm_ctx("hew_await_cancel_status (schedule) call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| {
                CodegenError::FailClosed("hew_await_cancel_status returned void".into())
            })?
            .into_int_value();
        let recv_completed = fn_ctx
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                sched_status,
                i32_ty.const_int(1, false),
                "suspending_channel_recv_schedule_recv_completed",
            )
            .llvm_ctx("suspending channel-recv schedule recv-completed compare")?;
        let deadline_failclosed_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_channel_recv_deadline_failclosed");
        fn_ctx
            .builder
            .build_conditional_branch(recv_completed, deadline_proceed_bb, deadline_failclosed_bb)
            .llvm_ctx("suspending channel-recv schedule recv-completed branch")?;

        fn_ctx.builder.position_at_end(deadline_failclosed_bb);
        let reg_cancel = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_cancel",
        )?;
        let cancelled_status = i32_ty.const_int(2, false); // AwaitCancelStatus::Cancelled
        let no_wake = i32_ty.const_zero();
        fn_ctx
            .builder
            .build_call(
                reg_cancel,
                &[reg.into(), cancelled_status.into(), no_wake.into()],
                "suspending_channel_recv_failclosed_reg_cancel",
            )
            .llvm_ctx("hew_await_cancel_cancel (channel recv fail-closed) call")?;
        let reg_free = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_free",
        )?;
        fn_ctx
            .builder
            .build_call(
                reg_free,
                &[reg.into()],
                "suspending_channel_recv_failclosed_reg_free",
            )
            .llvm_ctx("hew_await_cancel_free (channel recv fail-closed) call")?;
        fn_ctx
            .builder
            .build_call(
                slot_cancel,
                &[slot.into()],
                "suspending_channel_recv_failclosed_slot_cancel",
            )
            .llvm_ctx("hew_read_slot_cancel (fail-closed) call")?;
        fn_ctx
            .builder
            .build_call(
                slot_free,
                &[slot.into()],
                "suspending_channel_recv_failclosed_free",
            )
            .llvm_ctx("hew_read_slot_free (fail-closed) call")?;
        let result_dest = term.deadline_result_dest.ok_or_else(|| {
            CodegenError::FailClosed("SuspendingChannelRecv fail-closed missing Result dest".into())
        })?;
        let error_dest = term.error_dest.ok_or_else(|| {
            CodegenError::FailClosed(
                "SuspendingChannelRecv fail-closed missing TimeoutError dest".into(),
            )
        })?;
        emit_recv_deadline_timeout_err(fn_ctx, result_dest, error_dest)?;
        fn_ctx
            .builder
            .build_unconditional_branch(resume_bb)
            .llvm_ctx("suspending channel-recv deadline fail-closed br")?;

        fn_ctx.builder.position_at_end(deadline_proceed_bb);
    }
    // ── abandon_cleanup edge: cancel the slot (a racing sender deposit drops its
    // signal + skips the wake), detach the channel-await registration (release the
    // core's in-flight ref), free the creator ref, then join shared cleanup.
    // ── bind edge: ready-now OR resumed. Pop the queued item via the non-blocking
    // try_recv (the single consumer's own edge), map it into the Option<T> dest,
    // release the creator ref, branch to the MIR resume.
    emit_suspend_point(
        fn_ctx,
        coro,
        parent,
        bind_bb,
        "suspending_channel_recv",
        "suspending_channel_recv_abandon_cleanup",
        "suspending channel-recv abandon -> shared cleanup br",
        || {
            if let Some(reg) = reg {
                let reg_cancel = intern_runtime_decl(
                    fn_ctx.ctx,
                    fn_ctx.llvm_mod,
                    &mut fn_ctx.runtime_decls.borrow_mut(),
                    "hew_await_cancel_cancel",
                )?;
                let cancelled_status = i32_ty.const_int(2, false);
                let no_wake = i32_ty.const_zero();
                fn_ctx
                    .builder
                    .build_call(
                        reg_cancel,
                        &[reg.into(), cancelled_status.into(), no_wake.into()],
                        "suspending_channel_recv_abandon_reg_cancel",
                    )
                    .llvm_ctx("hew_await_cancel_cancel (channel recv abandon) call")?;
                let reg_free = intern_runtime_decl(
                    fn_ctx.ctx,
                    fn_ctx.llvm_mod,
                    &mut fn_ctx.runtime_decls.borrow_mut(),
                    "hew_await_cancel_free",
                )?;
                fn_ctx
                    .builder
                    .build_call(
                        reg_free,
                        &[reg.into()],
                        "suspending_channel_recv_abandon_reg_free",
                    )
                    .llvm_ctx("hew_await_cancel_free (channel recv abandon) call")?;
            }
            fn_ctx
                .builder
                .build_call(
                    slot_cancel,
                    &[slot.into()],
                    "suspending_channel_recv_abandon_cancel",
                )
                .llvm_ctx("hew_read_slot_cancel (abandon) call")?;
            fn_ctx
                .builder
                .build_call(
                    detach_recv,
                    &[rx_ptr.into(), slot.into()],
                    "suspending_channel_recv_abandon_detach",
                )
                .llvm_ctx("hew_channel_detach_recv (abandon) call")?;
            fn_ctx
                .builder
                .build_call(
                    slot_free,
                    &[slot.into()],
                    "suspending_channel_recv_abandon_free",
                )
                .llvm_ctx("hew_read_slot_free (abandon) call")?;
            Ok(())
        },
        || {
            emit_suspending_channel_recv_bind(
                fn_ctx, &term, rx_ptr, slot, slot_free, dest_local, reg, resume_bb, parent,
            )
        },
    )?;

    Ok(())
}

/// Resume-bind continuation of [`emit_suspending_channel_recv_terminator`]:
/// ready-now or resumed. Resolve the deadline arbiter (timeout binds
/// `Err(TimeoutError)`), pop the queued item through the non-blocking
/// `hew_channel_try_recv_layout`, release the creator ref, and branch to the MIR
/// resume block. Split out so the suspend-point seam owns the suspend + abandon
/// scaffolding while this owns the value routing.
#[allow(
    clippy::too_many_arguments,
    clippy::too_many_lines,
    reason = "the full channel-recv bind edge — deadline arbiter + Option<T> \
              layout pop — is kept in one place so the deadline resolution and \
              the value routing it gates are read together"
)]
fn emit_suspending_channel_recv_bind<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    term: &SuspendingChannelRecvEmit,
    rx_ptr: inkwell::values::PointerValue<'ctx>,
    slot: inkwell::values::PointerValue<'ctx>,
    slot_free: FunctionValue<'ctx>,
    dest_local: u32,
    reg: Option<inkwell::values::PointerValue<'ctx>>,
    resume_bb: inkwell::basic_block::BasicBlock<'ctx>,
    parent: FunctionValue<'ctx>,
) -> CodegenResult<()> {
    let i32_ty = fn_ctx.ctx.i32_type();
    if let Some(reg) = reg {
        // Deadline active: resolve the arbiter (complete → check status).
        let complete = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_complete",
        )?;
        fn_ctx
            .builder
            .build_call(
                complete,
                &[reg.into()],
                "suspending_channel_recv_deadline_complete",
            )
            .llvm_ctx("hew_await_cancel_complete call")?;
        let status_fn = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_status",
        )?;
        let status = fn_ctx
            .builder
            .build_call(
                status_fn,
                &[reg.into()],
                "suspending_channel_recv_deadline_status",
            )
            .llvm_ctx("hew_await_cancel_status call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| {
                CodegenError::FailClosed("hew_await_cancel_status returned void".into())
            })?
            .into_int_value();
        let timed_out = fn_ctx
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                i32_ty.const_int(3, false), // AwaitCancelStatus::TimedOut = 3
                "suspending_channel_recv_timed_out",
            )
            .llvm_ctx("suspending channel-recv timed-out compare")?;
        let timeout_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_channel_recv_timeout");
        let recv_proceed_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_channel_recv_proceed");
        fn_ctx
            .builder
            .build_conditional_branch(timed_out, timeout_bb, recv_proceed_bb)
            .llvm_ctx("suspending channel-recv deadline branch")?;

        fn_ctx.builder.position_at_end(timeout_bb);
        let reg_free = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_free",
        )?;
        fn_ctx
            .builder
            .build_call(
                reg_free,
                &[reg.into()],
                "suspending_channel_recv_timeout_reg_free",
            )
            .llvm_ctx("hew_await_cancel_free (channel recv timeout) call")?;
        fn_ctx
            .builder
            .build_call(
                slot_free,
                &[slot.into()],
                "suspending_channel_recv_timeout_free",
            )
            .llvm_ctx("hew_read_slot_free (timeout) call")?;
        let result_dest = term.deadline_result_dest.ok_or_else(|| {
            CodegenError::FailClosed("SuspendingChannelRecv timeout missing Result dest".into())
        })?;
        let error_dest = term.error_dest.ok_or_else(|| {
            CodegenError::FailClosed(
                "SuspendingChannelRecv timeout missing TimeoutError dest".into(),
            )
        })?;
        emit_recv_deadline_timeout_err(fn_ctx, result_dest, error_dest)?;
        fn_ctx
            .builder
            .build_unconditional_branch(resume_bb)
            .llvm_ctx("suspending channel-recv timeout br")?;

        fn_ctx.builder.position_at_end(recv_proceed_bb);
    }
    // The await already ensured readiness (an item is queued or the channel
    // closed), so the bind edge pops via the NON-BLOCKING layout entry — the
    // single consumer's own edge, popping the queued item exactly once
    // (close → rc 0 → None). The element witness decodes any describable T.
    store_recv_option_via_layout(
        fn_ctx,
        rx_ptr,
        dest_local,
        "hew_channel_try_recv_layout",
        &term.elem_ty,
    )?;
    fn_ctx
        .builder
        .build_call(
            slot_free,
            &[slot.into()],
            "suspending_channel_recv_bind_free",
        )
        .llvm_ctx("hew_read_slot_free (bind) call")?;
    if let Some(result_dest) = term.deadline_result_dest {
        // `await rx.recv() | after d` success path: wrap the Option<T> in `Ok(_)`.
        emit_result_ok(fn_ctx, result_dest, Some(term.result_dest))?;
    }
    if let Some(reg) = reg {
        let reg_free = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_free",
        )?;
        fn_ctx
            .builder
            .build_call(
                reg_free,
                &[reg.into()],
                "suspending_channel_recv_ok_reg_free",
            )
            .llvm_ctx("hew_await_cancel_free (channel recv ok) call")?;
    }
    fn_ctx
        .builder
        .build_unconditional_branch(resume_bb)
        .llvm_ctx("suspending channel-recv bind -> resume br")?;

    Ok(())
}

/// Emit the caller-side non-blocking `await t` over a scope-owned child
/// `Task<T>` (`Terminator::SuspendingTaskAwait`). The task-completion analogue
/// of [`emit_suspending_channel_recv_terminator`].
///
/// Shape (the suspending task-await ramp):
/// ```text
///   self  = hew_actor_self()                       ; the parked-cont actor
///   scope = <load scope token>                     ; pointer-shaped (opaque)
///   task  = <load task handle>                     ; pointer-shaped (opaque)
///   slot  = hew_read_slot_new()
///   rc    = hew_task_await_suspend(scope, task, self, slot)
///   br (rc == 0) -> do_suspend, bind               ; 0 = parked, 1 = already-done
/// do_suspend:                                       ; coro.suspend (non-final)
///   switch coro.suspend [default -> return handle, 0 -> bind, 1 -> abandon]
/// abandon:                                          ; parked cont destroyed
///   hew_task_detach_await(scope, task, slot); br shared cleanup
/// bind:                                             ; already-done OR resumed
///   (value task) result = hew_task_get_result(task); store -> result_dest
///   hew_read_slot_free(slot)                        ; release the creator ref
///   br resume_bb
/// ```
/// The task is BORROWED across the suspend (the scope-join owns its free). On
/// resume the task is `Done` and `hew_task_get_result` deep-reads its result —
/// the value channel is the task's own result buffer, NOT the gen out-pointer.
/// Slot refs mirror the channel-recv ramp: `new` (+1 creator);
/// `hew_task_await_suspend` takes the observer's in-flight ref only on the park
/// path; the single `hew_read_slot_free` on the bind edge releases the creator
/// ref. The abandon edge cancels + detaches before the creator-ref free so a
/// racing task `Done` drops its wake against a freed continuation.
pub(crate) fn emit_suspending_task_await_terminator<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    term: SuspendingTaskAwaitEmit,
) -> CodegenResult<()> {
    let coro = fn_ctx.coro.ok_or_else(|| {
        CodegenError::FailClosed(
            "Terminator::SuspendingTaskAwait reached codegen but the function \
             carries no coro prologue state — lower_function must detect the \
             suspend carrier (has_suspend) and emit the prologue before the body"
                .into(),
        )
    })?;
    let resume_bb = *fn_ctx.blocks.get(&term.resume).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "SuspendingTaskAwait resume target bb{} not found",
            term.resume
        ))
    })?;
    if !fn_ctx.blocks.contains_key(&term.cleanup) {
        return Err(CodegenError::FailClosed(format!(
            "SuspendingTaskAwait cleanup target bb{} not found",
            term.cleanup
        )));
    }
    // A value-returning task carries `result_dest` — the slot the child's `T`
    // is read into on the bind edge. The task body published its result through
    // `hew_task_set_result` (the codegen task wrapper); on the resume /
    // immediate-ready edge the task is `Done` and `hew_task_get_result` returns
    // the result buffer, which the bind edge copies into `result_dest` at the
    // `T` element width. A unit task carries `None` and binds nothing.
    let actor_self_fn = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_actor_self",
    )?;
    let self_actor = fn_ctx
        .builder
        .build_call(actor_self_fn, &[], "suspending_task_await_self")
        .llvm_ctx("hew_actor_self call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_actor_self returned void".into()))?
        .into_pointer_value();
    let scope_ptr = load_duplex_handle(fn_ctx, term.scope, "suspending_task_await scope")?;
    let task_ptr = load_duplex_handle(fn_ctx, term.task, "suspending_task_await task")?;

    let slot_new = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_read_slot_new",
    )?;
    let slot = fn_ctx
        .builder
        .build_call(slot_new, &[], "suspending_task_await_slot")
        .llvm_ctx("hew_read_slot_new call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_read_slot_new returned void".into()))?
        .into_pointer_value();

    let rc = fn_ctx.call_runtime_int(
        "hew_task_await_suspend",
        &[
            scope_ptr.into(),
            task_ptr.into(),
            self_actor.into(),
            slot.into(),
        ],
        "suspending_task_await_register",
        "hew_task_await_suspend call",
    )?;

    let parent = fn_ctx
        .builder
        .get_insert_block()
        .and_then(|bb| bb.get_parent())
        .ok_or_else(|| {
            CodegenError::Llvm("suspending task-await block has no parent function".into())
        })?;
    let do_suspend_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "suspending_task_await_suspend");
    let bind_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "suspending_task_await_bind");
    // rc == 0 (TASK_AWAIT_SUSPEND) → park; else (READY) bind immediately.
    let is_suspend = fn_ctx
        .builder
        .build_int_compare(
            IntPredicate::EQ,
            rc,
            rc.get_type().const_zero(),
            "suspending_task_await_is_suspend",
        )
        .llvm_ctx("suspending task-await suspend compare")?;
    fn_ctx
        .builder
        .build_conditional_branch(is_suspend, do_suspend_bb, bind_bb)
        .llvm_ctx("suspending task-await register branch")?;

    let slot_free = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_read_slot_free",
    )?;
    let detach_await = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_task_detach_await",
    )?;

    // ── do_suspend: park the continuation (non-final). Default → return the coro
    // handle to the trampoline; case 0 → bind; case 1 → abandon teardown. ──────
    fn_ctx.builder.position_at_end(do_suspend_bb);
    emit_suspend_point(
        fn_ctx,
        coro,
        parent,
        bind_bb,
        "suspending_task_await",
        "suspending_task_await_abandon_cleanup",
        "suspending task-await abandon -> shared cleanup br",
        || {
            // Abandon: detach the observer (cancel the slot so a racing Done
            // drops its wake) + release the creator ref. The observer's own
            // in-flight ref is released when it fires against the cancelled slot.
            fn_ctx
                .builder
                .build_call(
                    detach_await,
                    &[scope_ptr.into(), task_ptr.into(), slot.into()],
                    "suspending_task_await_abandon_detach",
                )
                .llvm_ctx("hew_task_detach_await (abandon) call")?;
            Ok(())
        },
        || {
            // Bind: already-done OR resumed. The task is `Done`. For a value
            // task, read the published result and copy it into `result_dest` at
            // the `T` element width: `hew_task_get_result` returns the
            // task-owned result buffer (the bytes `hew_task_set_result` deep-
            // copied from the body's return); the load+store MOVES the value
            // representation out of the buffer into the awaiter's binding slot.
            // The task frees the buffer (as raw bytes) at scope join, so a
            // managed handle lives exactly once — in `result_dest`.
            //
            // Null-buffer guard: `hew_task_get_result` returns `t.result`, which
            // is NULL whenever the task reached `Done` WITHOUT publishing a result
            // — i.e. a `Done(Cancelled)` task whose body never ran to
            // `hew_task_set_result` (the runtime documents this null contract on
            // `hew_task_await_suspend`). On a null buffer the bind edge copies
            // nothing rather than dereferencing null: the awaiter is being torn
            // down by the same scope cancel, so the binding is never observed —
            // exactly the unit-task discipline. Mirrors the suspending-ask bind
            // edge, which null-checks `reply_ptr` before the load.
            //
            // WHY a guard for a path the current surface cannot reach: no v0.6
            // user construct cancels a sibling task while another value-awaits in
            // the same scope (the empty-body `scope … after(d)` deadline arms but
            // joins the forked task rather than preempting its await — verified),
            // so this branch is presently unexercised. It honours the runtime's
            // explicit null-return contract so the value path is correct the day a
            // cancelling-await surface lands (await-cancel deadline on the task, a
            // failing-sibling scope-cancel), rather than shipping a latent null
            // deref behind a future feature. WHEN obsolete: never — the runtime
            // contract permits a null result buffer regardless of surface.
            if let Some(result_dest) = term.result_dest {
                let result_buf = fn_ctx.call_runtime_ptr(
                    "hew_task_get_result",
                    &[task_ptr.into()],
                    "suspending_task_await_result",
                    "hew_task_get_result (bind) call",
                )?;
                let copy_bb = fn_ctx
                    .ctx
                    .append_basic_block(parent, "suspending_task_await_result_copy");
                let bind_join_bb = fn_ctx
                    .ctx
                    .append_basic_block(parent, "suspending_task_await_result_join");
                let result_is_null = fn_ctx
                    .builder
                    .build_is_null(result_buf, "suspending_task_await_result_is_null")
                    .llvm_ctx("value-task result null compare")?;
                // null (cancelled / no result) → skip copy; non-null → copy.
                fn_ctx
                    .builder
                    .build_conditional_branch(result_is_null, bind_join_bb, copy_bb)
                    .llvm_ctx("value-task result null branch")?;

                fn_ctx.builder.position_at_end(copy_bb);
                let (dest_ptr, dest_ty) = place_pointer(fn_ctx, result_dest)?;
                let loaded = fn_ctx
                    .builder
                    .build_load(dest_ty, result_buf, "suspending_task_await_result_load")
                    .llvm_ctx("value-task result load")?;
                fn_ctx
                    .builder
                    .build_store(dest_ptr, loaded)
                    .llvm_ctx("value-task result store")?;
                fn_ctx
                    .builder
                    .build_unconditional_branch(bind_join_bb)
                    .llvm_ctx("value-task result copy -> join br")?;

                fn_ctx.builder.position_at_end(bind_join_bb);
            }
            // Release the creator ref, branch to the MIR resume.
            fn_ctx
                .builder
                .build_call(slot_free, &[slot.into()], "suspending_task_await_bind_free")
                .llvm_ctx("hew_read_slot_free (bind) call")?;
            fn_ctx
                .builder
                .build_unconditional_branch(resume_bb)
                .llvm_ctx("suspending task-await bind -> resume br")?;
            Ok(())
        },
    )
}

/// Emit the caller-side cooperative `await_restart sup.child`
/// (`SuspendKind::RestartWait`). The supervisor-restart-readiness analogue of
/// [`emit_suspending_task_await_terminator`].
///
/// Shape (the suspending restart-wait ramp):
/// ```text
///   self = hew_actor_self()                              ; the parked-cont actor
///   sup  = <load sup_place>                              ; the supervisor PID
///   slot = hew_read_slot_new()
///   rc   = hew_supervisor_restart_await_suspend(sup, slot_index, self, slot)
///   br (rc == 0) -> do_suspend, bind                     ; 0 = parked, 1 = ready
/// do_suspend:                                            ; coro.suspend (non-final)
///   switch coro.suspend [default -> return handle, 0 -> bind, 1 -> abandon]
/// abandon:                                               ; parked cont destroyed
///   hew_supervisor_restart_await_detach(sup, slot); br shared cleanup
/// bind:                                                  ; ready OR resumed
///   hew_read_slot_free(slot)                             ; release the creator ref
///   br resume_bb                                         ; MIR resume re-fetches the
///                                                        ; now-Live handle separately
/// ```
/// The pre-park state check lives in the runtime observer: a Live child returns
/// READY (bind immediately), a permanently-Dead child returns READY (resume +
/// fail closed at the send re-resolve, never hang — R4), only a Transient child
/// parks. The re-fetch of the now-Live handle is a separate MIR instruction on
/// the resume edge (`emit_child_get_into`), NOT this terminator — so codegen
/// only parks and resumes here, binding nothing. Slot refs mirror the task-await
/// ramp: `new` (+1 creator); the observer takes its in-flight ref only on the
/// park path; the single `hew_read_slot_free` on the bind edge releases the
/// creator ref. The abandon edge detaches (cancelling the slot so a racing
/// `notify_restart` drops its wake) before the creator-ref free.
pub(crate) fn emit_suspending_restart_wait_terminator<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    term: SuspendingRestartWaitEmit,
) -> CodegenResult<()> {
    let coro = fn_ctx.coro.ok_or_else(|| {
        CodegenError::FailClosed(
            "SuspendKind::RestartWait reached codegen but the function carries no \
             coro prologue state — lower_function must detect the suspend carrier \
             (has_suspend) and emit the prologue before the body"
                .into(),
        )
    })?;
    let resume_bb = *fn_ctx.blocks.get(&term.resume).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "RestartWait resume target bb{} not found",
            term.resume
        ))
    })?;
    if !fn_ctx.blocks.contains_key(&term.cleanup) {
        return Err(CodegenError::FailClosed(format!(
            "RestartWait cleanup target bb{} not found",
            term.cleanup
        )));
    }
    let _ = term.result_dest; // re-fetched by the MIR resume edge, not here.

    let i32_ty = fn_ctx.ctx.i32_type();
    let actor_self_fn = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_actor_self",
    )?;
    let self_actor = fn_ctx
        .builder
        .build_call(actor_self_fn, &[], "suspending_restart_wait_self")
        .llvm_ctx("hew_actor_self call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_actor_self returned void".into()))?
        .into_pointer_value();
    let sup_ptr = load_duplex_handle(fn_ctx, term.sup_place, "suspending_restart_wait_sup")?;

    let slot_new = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_read_slot_new",
    )?;
    let slot = fn_ctx
        .builder
        .build_call(slot_new, &[], "suspending_restart_wait_slot")
        .llvm_ctx("hew_read_slot_new call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_read_slot_new returned void".into()))?
        .into_pointer_value();

    let slot_index_val = i32_ty.const_int(u64::from(term.slot_index), false);
    let rc = fn_ctx.call_runtime_int(
        "hew_supervisor_restart_await_suspend",
        &[
            sup_ptr.into(),
            slot_index_val.into(),
            self_actor.into(),
            slot.into(),
        ],
        "suspending_restart_wait_register",
        "hew_supervisor_restart_await_suspend call",
    )?;

    let parent = fn_ctx
        .builder
        .get_insert_block()
        .and_then(|bb| bb.get_parent())
        .ok_or_else(|| {
            CodegenError::Llvm("suspending restart-wait block has no parent function".into())
        })?;
    let do_suspend_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "suspending_restart_wait_suspend");
    let bind_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "suspending_restart_wait_bind");
    // rc == 0 (RESTART_AWAIT_SUSPEND) → park; else (READY) bind immediately.
    let is_suspend = fn_ctx
        .builder
        .build_int_compare(
            IntPredicate::EQ,
            rc,
            rc.get_type().const_zero(),
            "suspending_restart_wait_is_suspend",
        )
        .llvm_ctx("suspending restart-wait suspend compare")?;
    fn_ctx
        .builder
        .build_conditional_branch(is_suspend, do_suspend_bb, bind_bb)
        .llvm_ctx("suspending restart-wait register branch")?;

    let slot_free = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_read_slot_free",
    )?;
    let detach = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_supervisor_restart_await_detach",
    )?;

    fn_ctx.builder.position_at_end(do_suspend_bb);
    emit_suspend_point(
        fn_ctx,
        coro,
        parent,
        bind_bb,
        "suspending_restart_wait",
        "suspending_restart_wait_abandon_cleanup",
        "suspending restart-wait abandon -> shared cleanup br",
        || {
            // Abandon: detach the observer (cancel the slot so a racing
            // notify_restart drops its wake) + release the creator ref. The
            // detach releases the observer's own in-flight ref.
            fn_ctx
                .builder
                .build_call(
                    detach,
                    &[sup_ptr.into(), slot.into()],
                    "suspending_restart_wait_abandon_detach",
                )
                .llvm_ctx("hew_supervisor_restart_await_detach (abandon) call")?;
            Ok(())
        },
        || {
            // Bind: ready (already Live / permanent-Dead) OR resumed (restarted).
            // Release the creator ref; the MIR resume edge re-fetches the handle.
            fn_ctx
                .builder
                .build_call(
                    slot_free,
                    &[slot.into()],
                    "suspending_restart_wait_bind_free",
                )
                .llvm_ctx("hew_read_slot_free (restart-wait bind) call")?;
            fn_ctx
                .builder
                .build_unconditional_branch(resume_bb)
                .llvm_ctx("suspending restart-wait bind -> resume br")?;
            Ok(())
        },
    )
}

/// Emit the caller-side cooperative `sleep_ms(d)` (`Terminator::SuspendingSleep`).
/// The timer-readiness analogue of [`emit_suspending_task_await_terminator`]:
/// instead of `std::thread::sleep` pinning the worker, arm an await-cancel
/// deadline on the process-global timer wheel carrying the parked actor, suspend
/// (freeing the worker), and resume when the timer fires (`enqueue_resume`).
///
/// Shape (the suspending sleep ramp):
/// ```text
///   self = hew_actor_self()                  ; the parked-cont actor
///   d    = <load duration_ms>                ; i64
///   reg  = hew_await_cancel_new(self, null, null)
///   tw   = hew_global_timer_wheel()
///   rc   = hew_await_cancel_schedule_deadline_ms(reg, tw, d)
///   br (rc == 0) -> do_suspend, bind         ; 0 = armed, else = arm failed
/// do_suspend:                                ; coro.suspend (non-final)
///   switch coro.suspend [default -> return handle, 0 -> bind, 1 -> abandon]
/// abandon:                                   ; parked cont destroyed
///   hew_await_cancel_cancel(reg, Cancelled, 0); hew_await_cancel_free(reg)
///   br shared cleanup
/// bind:                                      ; timer fired (or arm failed)
///   hew_await_cancel_complete(reg); hew_await_cancel_free(reg)
///   br resume_bb
/// ```
/// `sleep` is unit, so the resume edge binds nothing. The deadline-fire path
/// (`await_cancel_finish(TimedOut)`) calls `enqueue_resume(self, null)` — the
/// SAME wake the `await x | after d` deadline already proves. If the arm fails
/// (wheel/ticker unavailable) the bind edge proceeds immediately — a fail-safe
/// zero-length sleep, never a hang of the timer subsystem.
pub(crate) fn emit_suspending_sleep_terminator<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    term: SuspendingSleepEmit,
) -> CodegenResult<()> {
    let coro = fn_ctx.coro.ok_or_else(|| {
        CodegenError::FailClosed(
            "Terminator::SuspendingSleep reached codegen but the function carries \
             no coro prologue state — lower_function must detect the suspend \
             carrier (has_suspend) and emit the prologue before the body"
                .into(),
        )
    })?;
    let resume_bb = *fn_ctx.blocks.get(&term.resume).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "SuspendingSleep resume target bb{} not found",
            term.resume
        ))
    })?;
    if !fn_ctx.blocks.contains_key(&term.cleanup) {
        return Err(CodegenError::FailClosed(format!(
            "SuspendingSleep cleanup target bb{} not found",
            term.cleanup
        )));
    }

    let i32_ty = fn_ctx.ctx.i32_type();
    let i64_ty = fn_ctx.ctx.i64_type();
    let actor_self_fn = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_actor_self",
    )?;
    let self_actor = fn_ctx
        .builder
        .build_call(actor_self_fn, &[], "suspending_sleep_self")
        .llvm_ctx("hew_actor_self call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_actor_self returned void".into()))?
        .into_pointer_value();
    // `duration_ns` carries nanoseconds; `hew_await_cancel_schedule_deadline_ms`
    // expects milliseconds — divide by 1_000_000 (saturate to 0 on negative).
    let duration_ns =
        load_place_as_basic(fn_ctx, term.duration_ns, "suspending_sleep_dur_ns")?.into_int_value();
    let ns_per_ms = i64_ty.const_int(1_000_000, false);
    let dur_ms_raw = fn_ctx
        .builder
        .build_int_signed_div(duration_ns, ns_per_ms, "suspending_sleep_dur_ms_raw")
        .llvm_ctx("sleep dur ns→ms sdiv")?;
    let zero_i64 = i64_ty.const_zero();
    let is_neg = fn_ctx
        .builder
        .build_int_compare(
            IntPredicate::SLT,
            dur_ms_raw,
            zero_i64,
            "suspending_sleep_dur_neg",
        )
        .llvm_ctx("sleep dur neg cmp")?;
    let duration = fn_ctx
        .builder
        .build_select(is_neg, zero_i64, dur_ms_raw, "suspending_sleep_dur_ms")
        .llvm_ctx("sleep dur clamp")?
        .into_int_value();

    let null_ptr = fn_ctx
        .ctx
        .ptr_type(inkwell::AddressSpace::default())
        .const_null();
    // A pure timer→enqueue_resume registration: no cleanup source (sleep owns no
    // read slot / channel registration to tear down — the wheel cancel on the
    // abandon edge is the whole teardown).
    let reg = emit_await_cancel_new(
        fn_ctx,
        self_actor,
        null_ptr,
        null_ptr,
        "suspending_sleep_reg",
    )?;

    let tw_fn = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_global_timer_wheel",
    )?;
    let tw = fn_ctx
        .builder
        .build_call(tw_fn, &[], "suspending_sleep_tw")
        .llvm_ctx("hew_global_timer_wheel call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_global_timer_wheel returned void".into()))?
        .into_pointer_value();
    let schedule = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_await_cancel_schedule_deadline_ms",
    )?;
    let sched_rc = fn_ctx
        .builder
        .build_call(
            schedule,
            &[reg.into(), tw.into(), duration.into()],
            "suspending_sleep_schedule",
        )
        .llvm_ctx("hew_await_cancel_schedule_deadline_ms call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| {
            CodegenError::FailClosed("hew_await_cancel_schedule_deadline_ms returned void".into())
        })?
        .into_int_value();

    let parent = fn_ctx
        .builder
        .get_insert_block()
        .and_then(|bb| bb.get_parent())
        .ok_or_else(|| {
            CodegenError::Llvm("suspending sleep block has no parent function".into())
        })?;
    let do_suspend_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "suspending_sleep_suspend");
    let bind_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "suspending_sleep_bind");
    // rc == 0 (armed) → park; else (arm failed) bind immediately (fail-safe).
    let armed = fn_ctx
        .builder
        .build_int_compare(
            IntPredicate::EQ,
            sched_rc,
            sched_rc.get_type().const_zero(),
            "suspending_sleep_armed",
        )
        .llvm_ctx("suspending sleep armed compare")?;
    fn_ctx
        .builder
        .build_conditional_branch(armed, do_suspend_bb, bind_bb)
        .llvm_ctx("suspending sleep arm branch")?;

    let reg_complete = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_await_cancel_complete",
    )?;
    let reg_cancel = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_await_cancel_cancel",
    )?;
    let reg_free = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_await_cancel_free",
    )?;

    fn_ctx.builder.position_at_end(do_suspend_bb);
    emit_suspend_point(
        fn_ctx,
        coro,
        parent,
        bind_bb,
        "suspending_sleep",
        "suspending_sleep_abandon_cleanup",
        "suspending sleep abandon -> shared cleanup br",
        || {
            // Abandon: cancel the scheduled deadline (a racing fire drops its
            // wake) WITHOUT waking, then release the registration.
            let cancelled_status = i32_ty.const_int(2, false); // Cancelled
            let no_wake = i32_ty.const_zero();
            fn_ctx
                .builder
                .build_call(
                    reg_cancel,
                    &[reg.into(), cancelled_status.into(), no_wake.into()],
                    "suspending_sleep_abandon_cancel",
                )
                .llvm_ctx("hew_await_cancel_cancel (sleep abandon) call")?;
            fn_ctx
                .builder
                .build_call(reg_free, &[reg.into()], "suspending_sleep_abandon_free")
                .llvm_ctx("hew_await_cancel_free (sleep abandon) call")?;
            Ok(())
        },
        || {
            // Bind: the timer fired (or the arm failed → immediate). Settle the
            // arbiter and release the registration. `sleep` is unit — nothing to
            // bind.
            fn_ctx
                .builder
                .build_call(reg_complete, &[reg.into()], "suspending_sleep_complete")
                .llvm_ctx("hew_await_cancel_complete call")?;
            fn_ctx
                .builder
                .build_call(reg_free, &[reg.into()], "suspending_sleep_bind_free")
                .llvm_ctx("hew_await_cancel_free (sleep bind) call")?;
            fn_ctx
                .builder
                .build_unconditional_branch(resume_bb)
                .llvm_ctx("suspending sleep bind -> resume br")?;
            Ok(())
        },
    )
}

/// Emit the caller-side cooperative `sleep_until(i)` (`Terminator::SuspendingSleepUntil`).
///
/// Identical structure to [`emit_suspending_sleep_terminator`] except that the
/// deadline is computed as `max(0, instant_ns − now_ns) / 1_000_000` milliseconds
/// rather than being supplied directly as a duration.
pub(crate) fn emit_suspending_sleep_until_terminator<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    term: SuspendingSleepUntilEmit,
) -> CodegenResult<()> {
    let coro = fn_ctx.coro.ok_or_else(|| {
        CodegenError::FailClosed(
            "Terminator::SuspendingSleepUntil reached codegen but the function carries \
             no coro prologue state"
                .into(),
        )
    })?;
    let resume_bb = *fn_ctx.blocks.get(&term.resume).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "SuspendingSleepUntil resume target bb{} not found",
            term.resume
        ))
    })?;
    if !fn_ctx.blocks.contains_key(&term.cleanup) {
        return Err(CodegenError::FailClosed(format!(
            "SuspendingSleepUntil cleanup target bb{} not found",
            term.cleanup
        )));
    }

    let i32_ty = fn_ctx.ctx.i32_type();
    let i64_ty = fn_ctx.ctx.i64_type();

    let actor_self_fn = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_actor_self",
    )?;
    let self_actor = fn_ctx
        .builder
        .build_call(actor_self_fn, &[], "sleep_until_self")
        .llvm_ctx("hew_actor_self call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_actor_self returned void".into()))?
        .into_pointer_value();

    // Compute remaining = max(0, instant_ns − now_ns) / 1_000_000 ms.
    let instant_ns =
        load_place_as_basic(fn_ctx, term.instant_ns, "sleep_until_instant_ns")?.into_int_value();
    let now_fn = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_instant_now",
    )?;
    let now_ns = fn_ctx
        .builder
        .build_call(now_fn, &[], "sleep_until_now_ns")
        .llvm_ctx("hew_instant_now call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_instant_now returned void".into()))?
        .into_int_value();
    let remaining_ns = fn_ctx
        .builder
        .build_int_sub(instant_ns, now_ns, "sleep_until_remaining_ns")
        .llvm_ctx("sleep_until remaining_ns sub")?;
    let zero_i64 = i64_ty.const_zero();
    let is_past = fn_ctx
        .builder
        .build_int_compare(
            IntPredicate::SLE,
            remaining_ns,
            zero_i64,
            "sleep_until_is_past",
        )
        .llvm_ctx("sleep_until past cmp")?;
    let remaining_clamped = fn_ctx
        .builder
        .build_select(
            is_past,
            zero_i64,
            remaining_ns,
            "sleep_until_remaining_clamped",
        )
        .llvm_ctx("sleep_until clamp")?
        .into_int_value();
    let ns_per_ms = i64_ty.const_int(1_000_000, false);
    let delay_ms = fn_ctx
        .builder
        .build_int_signed_div(remaining_clamped, ns_per_ms, "sleep_until_delay_ms")
        .llvm_ctx("sleep_until ns→ms sdiv")?;

    let null_ptr = fn_ctx
        .ctx
        .ptr_type(inkwell::AddressSpace::default())
        .const_null();
    let reg = emit_await_cancel_new(fn_ctx, self_actor, null_ptr, null_ptr, "sleep_until_reg")?;

    let tw_fn = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_global_timer_wheel",
    )?;
    let tw = fn_ctx
        .builder
        .build_call(tw_fn, &[], "sleep_until_tw")
        .llvm_ctx("hew_global_timer_wheel call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_global_timer_wheel returned void".into()))?
        .into_pointer_value();
    let schedule = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_await_cancel_schedule_deadline_ms",
    )?;
    let sched_rc = fn_ctx
        .builder
        .build_call(
            schedule,
            &[reg.into(), tw.into(), delay_ms.into()],
            "sleep_until_schedule",
        )
        .llvm_ctx("hew_await_cancel_schedule_deadline_ms call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| {
            CodegenError::FailClosed("hew_await_cancel_schedule_deadline_ms returned void".into())
        })?
        .into_int_value();

    let parent = fn_ctx
        .builder
        .get_insert_block()
        .and_then(|bb| bb.get_parent())
        .ok_or_else(|| {
            CodegenError::Llvm("suspending sleep_until block has no parent function".into())
        })?;
    let do_suspend_bb = fn_ctx.ctx.append_basic_block(parent, "sleep_until_suspend");
    let bind_bb = fn_ctx.ctx.append_basic_block(parent, "sleep_until_bind");

    let armed = fn_ctx
        .builder
        .build_int_compare(
            IntPredicate::EQ,
            sched_rc,
            sched_rc.get_type().const_zero(),
            "sleep_until_armed",
        )
        .llvm_ctx("sleep_until armed compare")?;
    fn_ctx
        .builder
        .build_conditional_branch(armed, do_suspend_bb, bind_bb)
        .llvm_ctx("sleep_until arm branch")?;

    let reg_complete = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_await_cancel_complete",
    )?;
    let reg_cancel = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_await_cancel_cancel",
    )?;
    let reg_free = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_await_cancel_free",
    )?;

    fn_ctx.builder.position_at_end(do_suspend_bb);
    emit_suspend_point(
        fn_ctx,
        coro,
        parent,
        bind_bb,
        "sleep_until",
        "sleep_until_abandon_cleanup",
        "sleep_until abandon -> shared cleanup br",
        || {
            let cancelled_status = i32_ty.const_int(2, false); // Cancelled
            let no_wake = i32_ty.const_zero();
            fn_ctx
                .builder
                .build_call(
                    reg_cancel,
                    &[reg.into(), cancelled_status.into(), no_wake.into()],
                    "sleep_until_abandon_cancel",
                )
                .llvm_ctx("hew_await_cancel_cancel (sleep_until abandon) call")?;
            fn_ctx
                .builder
                .build_call(reg_free, &[reg.into()], "sleep_until_abandon_free")
                .llvm_ctx("hew_await_cancel_free (sleep_until abandon) call")?;
            Ok(())
        },
        || {
            fn_ctx
                .builder
                .build_call(reg_complete, &[reg.into()], "sleep_until_complete")
                .llvm_ctx("hew_await_cancel_complete call")?;
            fn_ctx
                .builder
                .build_call(reg_free, &[reg.into()], "sleep_until_bind_free")
                .llvm_ctx("hew_await_cancel_free (sleep_until bind) call")?;
            fn_ctx
                .builder
                .build_unconditional_branch(resume_bb)
                .llvm_ctx("sleep_until bind -> resume br")?;
            Ok(())
        },
    )
}

/// `scope { } after(d) { body }` with a NON-EMPTY timeout body
/// (`Terminator::SuspendingScopeDeadline`).
///
/// The two-arm sibling of [`emit_suspending_sleep_terminator`]: a single shared
/// `HewAwaitCancel` arbiter (the first-ready one-shot CAS the suspending select
/// uses) races the scope-wide child JOIN (wait-ALL) against a timer-wheel
/// DEADLINE. The deadline-won edge cancels the remaining children and routes to
/// the lowered `after(...)` body; the join-won edge skips the body. Whichever arm
/// wins the CAS settles the wait, so deadline-vs-completion is mutually exclusive.
///
/// Shape (the suspending scope-deadline ramp):
/// ```text
///   self  = hew_actor_self()                       ; the parked-cont actor
///   scope = <load scope>                           ; the task scope
///   d_ms  = max(0, <load duration> / 1_000_000)    ; ns → ms
///   reg   = hew_await_cancel_new(self, null, null) ; ONE shared arbiter
///   tw    = hew_global_timer_wheel()
///   hew_await_cancel_schedule_deadline_ms(reg, tw, d_ms)   ; DEADLINE arm
///   ready = hew_task_scope_completion_observe(scope, reg, self) ; JOIN arm
///   br (ready == SCOPE_JOIN_READY) -> scan, do_suspend
/// do_suspend:                                       ; coro.suspend (non-final)
///   switch coro.suspend [default -> return handle, 0 -> scan, 1 -> abandon]
/// abandon:                                          ; parked cont destroyed
///   hew_await_cancel_cancel(reg, Cancelled, no_wake)   ; cancels the timer
///   hew_task_scope_detach_completion(scope)
///   hew_task_scope_cancel(scope)                       ; force children Done
///   hew_await_cancel_free(reg); br shared cleanup
/// scan (resume):                                    ; join completed OR deadline fired
///   status = hew_await_cancel_status(reg)
///   hew_await_cancel_cancel(reg, Cancelled, no_wake)   ; idempotent settle (timer)
///   br (status == TimedOut) -> deadline_won, join_won
/// deadline_won:
///   hew_task_scope_cancel(scope); hew_task_scope_join_all(scope)  ; reap
///   hew_await_cancel_free(reg); br timeout_body_block  ; run after-body -> resume
/// join_won:
///   hew_task_scope_join_all(scope)                     ; reap (all done)
///   hew_await_cancel_free(reg); br resume_bb           ; skip the body
/// ```
/// The deadline arm cannot fail-safe to immediate the way `sleep` does (the join
/// arm is still live if the wheel is unavailable), so the schedule rc is ignored:
/// a failed arm means no deadline fires and the join decides the winner. The
/// `scope` local is frame-spilled, so loading the carrier's `scope` place is
/// valid before the suspend (arm) and after resume (cancel + join). The `cleanup`
/// edge is the coro abandon outline; the resume/abandon arms hold every teardown.
#[allow(
    clippy::too_many_lines,
    reason = "the full scope-deadline ramp — arbiter setup + two-arm arming + \
              suspend + the resume-edge winner branch (deadline body vs \
              scope-complete) + the abandon teardown — is kept in one place so \
              the arbiter's exactly-once teardown is read together"
)]
pub(crate) fn emit_suspending_scope_deadline_terminator<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    term: SuspendingScopeDeadlineEmit,
) -> CodegenResult<()> {
    let coro = fn_ctx.coro.ok_or_else(|| {
        CodegenError::FailClosed(
            "Terminator::SuspendingScopeDeadline reached codegen but the function \
             carries no coro prologue state — lower_function must detect the \
             suspend carrier (has_suspend) and emit the prologue before the body"
                .into(),
        )
    })?;
    let resume_bb = *fn_ctx.blocks.get(&term.resume).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "SuspendingScopeDeadline resume target bb{} not found",
            term.resume
        ))
    })?;
    let timeout_body_bb = *fn_ctx.blocks.get(&term.timeout_body_block).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "SuspendingScopeDeadline timeout-body target bb{} not found",
            term.timeout_body_block
        ))
    })?;
    if !fn_ctx.blocks.contains_key(&term.cleanup) {
        return Err(CodegenError::FailClosed(format!(
            "SuspendingScopeDeadline cleanup target bb{} not found",
            term.cleanup
        )));
    }

    let ctx = fn_ctx.ctx;
    let i32_ty = ctx.i32_type();
    let i64_ty = ctx.i64_type();
    let ptr_ty = ctx.ptr_type(inkwell::AddressSpace::default());

    // self = the parked-continuation actor the timer / join wake re-enqueues.
    // MUST come from hew_actor_self() (the live thread-local context), not a
    // spilled param: across a suspend the per-dispatch context is freed; on
    // resume the scheduler installs a fresh one (the FIX-THE-CLASS accessor the
    // ask / read / sleep ramps use).
    let actor_self_fn = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_actor_self",
    )?;
    let self_actor = fn_ctx
        .builder
        .build_call(actor_self_fn, &[], "suspending_scope_deadline_self")
        .llvm_ctx("hew_actor_self call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_actor_self returned void".into()))?
        .into_pointer_value();

    // The scope being raced (a frame-spilled local; valid across the suspend).
    let scope_ptr = load_place_as_basic(fn_ctx, term.scope, "suspending_scope_deadline_scope")?
        .into_pointer_value();

    // ns → ms, clamped non-negative (`after(d)` carries a duration value in
    // nanoseconds, like the select AfterTimer arm; the schedule takes u64 ms).
    let dur_ns = load_place_as_basic(fn_ctx, term.duration_ms, "suspending_scope_deadline_dur")?
        .into_int_value();
    let ms_per_ns = i64_ty.const_int(1_000_000, false);
    let dur_ms = fn_ctx
        .builder
        .build_int_signed_div(dur_ns, ms_per_ns, "suspending_scope_deadline_dur_ms")
        .llvm_ctx("scope deadline dur sdiv")?;
    let zero_i64 = i64_ty.const_zero();
    let neg = fn_ctx
        .builder
        .build_int_compare(
            IntPredicate::SLT,
            dur_ms,
            zero_i64,
            "suspending_scope_deadline_neg",
        )
        .llvm_ctx("scope deadline neg cmp")?;
    let dur_ms_nonneg = fn_ctx
        .builder
        .build_select(neg, zero_i64, dur_ms, "suspending_scope_deadline_nonneg")
        .llvm_ctx("scope deadline nonneg select")?
        .into_int_value();

    // ONE shared first-ready arbiter (no source cleanup — the resume/abandon arms
    // do the scope cancel + join + arbiter free with full pointer context, like
    // the sleep ramp's null-cleanup arbiter).
    let null_ptr = ptr_ty.const_null();
    let reg = emit_await_cancel_new(
        fn_ctx,
        self_actor,
        null_ptr,
        null_ptr,
        "suspending_scope_deadline_reg",
    )?;

    // DEADLINE arm: arm the timeout on the shared arbiter via the global wheel.
    // Ignore the schedule rc: the JOIN arm still decides the winner if the wheel
    // is unavailable, so a failed deadline arm never hangs the wait.
    let tw_fn = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_global_timer_wheel",
    )?;
    let tw = fn_ctx
        .builder
        .build_call(tw_fn, &[], "suspending_scope_deadline_tw")
        .llvm_ctx("hew_global_timer_wheel call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_global_timer_wheel returned void".into()))?
        .into_pointer_value();
    let schedule = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_await_cancel_schedule_deadline_ms",
    )?;
    fn_ctx
        .builder
        .build_call(
            schedule,
            &[reg.into(), tw.into(), dur_ms_nonneg.into()],
            "suspending_scope_deadline_schedule",
        )
        .llvm_ctx("hew_await_cancel_schedule_deadline_ms call")?;

    // JOIN arm: wire the scope's wait-ALL completion onto the same arbiter. A
    // return of SCOPE_JOIN_READY (1) means the scope already joined — bind the
    // scope-complete edge without suspending (the immediate path).
    let scope_observe = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_task_scope_completion_observe",
    )?;
    let join_ready = fn_ctx
        .builder
        .build_call(
            scope_observe,
            &[scope_ptr.into(), reg.into(), self_actor.into()],
            "suspending_scope_deadline_observe",
        )
        .llvm_ctx("hew_task_scope_completion_observe call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| {
            CodegenError::FailClosed("hew_task_scope_completion_observe returned void".into())
        })?
        .into_int_value();

    let parent = fn_ctx
        .builder
        .get_insert_block()
        .and_then(|bb| bb.get_parent())
        .ok_or_else(|| {
            CodegenError::Llvm("suspending scope deadline block has no parent function".into())
        })?;
    let do_suspend_bb = ctx.append_basic_block(parent, "suspending_scope_deadline_suspend");
    let scan_bb = ctx.append_basic_block(parent, "suspending_scope_deadline_scan");

    // SCOPE_JOIN_READY (1) → bind immediately on the scan edge; else suspend.
    let scope_join_ready = i32_ty.const_int(1, false);
    let already_done = fn_ctx
        .builder
        .build_int_compare(
            IntPredicate::EQ,
            join_ready,
            scope_join_ready,
            "suspending_scope_deadline_ready",
        )
        .llvm_ctx("scope deadline ready cmp")?;
    fn_ctx
        .builder
        .build_conditional_branch(already_done, scan_bb, do_suspend_bb)
        .llvm_ctx("scope deadline ready branch")?;

    let reg_cancel = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_await_cancel_cancel",
    )?;
    let reg_free = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_await_cancel_free",
    )?;
    let reg_status = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_await_cancel_status",
    )?;
    let scope_cancel = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_task_scope_cancel",
    )?;
    let scope_join = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_task_scope_join_all",
    )?;
    let scope_detach = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_task_scope_detach_completion",
    )?;

    let cancelled_status = i32_ty.const_int(2, false); // AwaitCancelStatus::Cancelled
    let timed_out_status = i32_ty.const_int(3, false); // AwaitCancelStatus::TimedOut
    let no_wake = i32_ty.const_zero();

    fn_ctx.builder.position_at_end(do_suspend_bb);
    emit_suspend_point(
        fn_ctx,
        coro,
        parent,
        scan_bb,
        "suspending_scope_deadline",
        "suspending_scope_deadline_abandon_cleanup",
        "suspending scope deadline abandon -> shared cleanup br",
        || {
            // Abandon: the parked continuation is being destroyed. Cancel the
            // arbiter WITHOUT waking (cancels the armed deadline so a racing fire
            // drops its wake), detach the join arm, then force the scope's
            // children Done (scope-cancel) so every per-child join observer fires
            // and reclaims its box, then free the arbiter's creator ref. Each call
            // runs exactly once on this single abandon edge.
            fn_ctx
                .builder
                .build_call(
                    reg_cancel,
                    &[reg.into(), cancelled_status.into(), no_wake.into()],
                    "suspending_scope_deadline_abandon_cancel",
                )
                .llvm_ctx("scope deadline abandon arbiter cancel")?;
            fn_ctx
                .builder
                .build_call(
                    scope_detach,
                    &[scope_ptr.into()],
                    "suspending_scope_deadline_abandon_detach",
                )
                .llvm_ctx("scope deadline abandon detach")?;
            fn_ctx
                .builder
                .build_call(
                    scope_cancel,
                    &[scope_ptr.into()],
                    "suspending_scope_deadline_abandon_scope_cancel",
                )
                .llvm_ctx("scope deadline abandon scope cancel")?;
            fn_ctx
                .builder
                .build_call(
                    reg_free,
                    &[reg.into()],
                    "suspending_scope_deadline_abandon_free",
                )
                .llvm_ctx("scope deadline abandon arbiter free")?;
            Ok(())
        },
        || {
            // Resume / immediate: the join completed OR the deadline fired. Read
            // the terminal status to decide which arm won, settle the arbiter
            // (cancel-no-wake is idempotent — cancels the timer if the join won),
            // then branch. Both arms reap the children with join_all and free the
            // arbiter's creator ref exactly once.
            let status = fn_ctx
                .builder
                .build_call(
                    reg_status,
                    &[reg.into()],
                    "suspending_scope_deadline_status",
                )
                .llvm_ctx("hew_await_cancel_status call")?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| {
                    CodegenError::FailClosed("hew_await_cancel_status returned void".into())
                })?
                .into_int_value();
            fn_ctx
                .builder
                .build_call(
                    reg_cancel,
                    &[reg.into(), cancelled_status.into(), no_wake.into()],
                    "suspending_scope_deadline_resume_cancel",
                )
                .llvm_ctx("scope deadline resume arbiter cancel")?;

            let deadline_won = fn_ctx
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    status,
                    timed_out_status,
                    "suspending_scope_deadline_timed_out",
                )
                .llvm_ctx("scope deadline timed-out cmp")?;
            let deadline_won_bb =
                ctx.append_basic_block(parent, "suspending_scope_deadline_deadline_won");
            let join_won_bb = ctx.append_basic_block(parent, "suspending_scope_deadline_join_won");
            fn_ctx
                .builder
                .build_conditional_branch(deadline_won, deadline_won_bb, join_won_bb)
                .llvm_ctx("scope deadline winner branch")?;

            // Deadline won: cancel the remaining children, reap them, free the
            // arbiter, then run the lowered after(...) body (which converges on
            // resume).
            fn_ctx.builder.position_at_end(deadline_won_bb);
            fn_ctx
                .builder
                .build_call(
                    scope_cancel,
                    &[scope_ptr.into()],
                    "suspending_scope_deadline_won_cancel",
                )
                .llvm_ctx("scope deadline won scope cancel")?;
            fn_ctx
                .builder
                .build_call(
                    scope_join,
                    &[scope_ptr.into()],
                    "suspending_scope_deadline_won_join",
                )
                .llvm_ctx("scope deadline won join_all")?;
            fn_ctx
                .builder
                .build_call(
                    reg_free,
                    &[reg.into()],
                    "suspending_scope_deadline_won_free",
                )
                .llvm_ctx("scope deadline won arbiter free")?;
            fn_ctx
                .builder
                .build_unconditional_branch(timeout_body_bb)
                .llvm_ctx("scope deadline won -> body br")?;

            // Join won: reap the (already-complete) children, free the arbiter,
            // and skip the after-body — the scope completed in time.
            fn_ctx.builder.position_at_end(join_won_bb);
            fn_ctx
                .builder
                .build_call(
                    scope_join,
                    &[scope_ptr.into()],
                    "suspending_scope_deadline_join_join",
                )
                .llvm_ctx("scope deadline join join_all")?;
            fn_ctx
                .builder
                .build_call(
                    reg_free,
                    &[reg.into()],
                    "suspending_scope_deadline_join_free",
                )
                .llvm_ctx("scope deadline join arbiter free")?;
            fn_ctx
                .builder
                .build_unconditional_branch(resume_bb)
                .llvm_ctx("scope deadline join -> resume br")?;
            Ok(())
        },
    )
}

/// Emit the caller-side non-blocking `await sink.send(x)` (NEW-7
/// `Terminator::SuspendingStreamSend`). The backpressure analogue of
/// [`emit_suspending_stream_next_terminator`].
///
/// Shape (the suspending stream-send ramp):
/// ```text
///   self    = hew_actor_self()
///   sink    = <load sink handle>
///   value   = <address of the bytes-triple alloca>      ; passed by pointer
///   slot    = hew_read_slot_new()
///   rc      = hew_stream_await_send(sink, self, slot, value)
///   br (rc == 0) -> do_suspend, bind                     ; 0 = parked (full ring)
/// do_suspend:                                            ; coro.suspend (non-final)
///   switch coro.suspend [default -> return handle, 0 -> bind, 1 -> abandon]
/// abandon:                                               ; parked cont destroyed
///   hew_read_slot_cancel(slot)
///   hew_sink_detach_await(sink, slot)                    ; drop pending item + ref
///   hew_read_slot_free(slot); br shared cleanup
/// bind:                                                  ; ready-now OR resumed
///   hew_read_slot_free(slot); br resume_bb               ; send is unit, no bind
/// ```
/// The item is copied into the channel queue / the parked producer's waiter by
/// `hew_stream_await_send`, so it is owned by the runtime across the suspend and
/// re-enqueued exactly once by the consumer's drain. Slot ref discipline matches
/// the recv ramp; the abandon edge cancels + detaches before freeing.
pub(crate) fn emit_suspending_stream_send_terminator<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    term: SuspendingStreamSendEmit,
) -> CodegenResult<()> {
    let coro = fn_ctx.coro.ok_or_else(|| {
        CodegenError::FailClosed(
            "Terminator::SuspendingStreamSend reached codegen but the function \
             carries no coro prologue state — lower_function must detect the \
             suspend carrier (has_suspend) and emit the prologue before the body"
                .into(),
        )
    })?;
    let resume_bb = *fn_ctx.blocks.get(&term.resume).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "SuspendingStreamSend resume target bb{} not found",
            term.resume
        ))
    })?;
    if !fn_ctx.blocks.contains_key(&term.cleanup) {
        return Err(CodegenError::FailClosed(format!(
            "SuspendingStreamSend cleanup target bb{} not found",
            term.cleanup
        )));
    }

    let actor_self_fn = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_actor_self",
    )?;
    let self_actor = fn_ctx
        .builder
        .build_call(actor_self_fn, &[], "suspending_stream_send_self")
        .llvm_ctx("hew_actor_self call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_actor_self returned void".into()))?
        .into_pointer_value();
    let sink_ptr = load_duplex_handle(fn_ctx, term.sink, "suspending_stream_send sink")?;
    // The value is passed BY POINTER (the triple alloca address for
    // bytes/string, or the plain slot address for anything else), exactly
    // like the blocking `hew_sink_write_bytes` / `hew_stream_send_layout`
    // consumers.
    let (value_ptr, _value_ty) = place_pointer(fn_ctx, term.value)?;

    let slot_new = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_read_slot_new",
    )?;
    let slot = fn_ctx
        .builder
        .build_call(slot_new, &[], "suspending_stream_send_slot")
        .llvm_ctx("hew_read_slot_new call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_read_slot_new returned void".into()))?
        .into_pointer_value();

    // Source-level `await sink.send(bytes)` — a genuine 16-byte
    // `BytesTriple { ptr, offset: u32, len: u32 }` slot — is the ONE
    // producer of the native `hew_stream_await_send`, whose `data` param is
    // dereferenced as exactly that triple. It stays on the native path.
    //
    // Everything else the `receive gen fn` pump forwards
    // (`SuspendKind::StreamSend` minted directly by
    // `build_stream_producer_pump`, hew-mir/src/lower.rs — never a
    // source-level `await sink.send`) rides the layout-witness sibling
    // `hew_stream_await_send_layout`, which reads the value through its
    // type's layout witness. `string` MUST take this path: a `string` local
    // is a single 8-byte `char*` slot, not a triple; routing it through the
    // native path reads a `BytesTriple` header out of the 8-byte slot (word
    // one is the adjacent coro-frame field, parsed as offset/len) → a wild
    // read that yields empty content or SIGSEGVs. `i64`/record/enum yields
    // are misread the same way. Only `bytes` is triple-shaped, so only
    // `bytes` is correct on the native path.
    let value_resolved_ty = place_resolved_ty(fn_ctx, term.value)?.clone();
    let rc = if matches!(value_resolved_ty, ResolvedTy::Bytes) {
        fn_ctx.call_runtime_int(
            "hew_stream_await_send",
            &[
                sink_ptr.into(),
                self_actor.into(),
                slot.into(),
                value_ptr.into(),
            ],
            "suspending_stream_send_register",
            "hew_stream_await_send call",
        )?
    } else {
        let witness = crate::layout::channel_elem_layout_witness_ptr(
            fn_ctx,
            &value_resolved_ty,
            "suspending_stream_send_layout",
        )?;
        fn_ctx.call_runtime_int(
            "hew_stream_await_send_layout",
            &[
                sink_ptr.into(),
                self_actor.into(),
                slot.into(),
                value_ptr.into(),
                witness.into(),
            ],
            "suspending_stream_send_layout_register",
            "hew_stream_await_send_layout call",
        )?
    };

    let parent = fn_ctx
        .builder
        .get_insert_block()
        .and_then(|bb| bb.get_parent())
        .ok_or_else(|| {
            CodegenError::Llvm("suspending stream-send block has no parent function".into())
        })?;
    let do_suspend_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "suspending_stream_send_suspend");
    let bind_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "suspending_stream_send_bind");
    let is_suspend = fn_ctx
        .builder
        .build_int_compare(
            IntPredicate::EQ,
            rc,
            rc.get_type().const_zero(),
            "suspending_stream_send_is_suspend",
        )
        .llvm_ctx("suspending stream-send suspend compare")?;
    fn_ctx
        .builder
        .build_conditional_branch(is_suspend, do_suspend_bb, bind_bb)
        .llvm_ctx("suspending stream-send register branch")?;

    let slot_cancel = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_read_slot_cancel",
    )?;
    let slot_free = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_read_slot_free",
    )?;
    let detach_await = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_sink_detach_await",
    )?;

    // ── do_suspend: park the producer (non-final). ────────────────────────────
    fn_ctx.builder.position_at_end(do_suspend_bb);
    emit_suspend_point(
        fn_ctx,
        coro,
        parent,
        bind_bb,
        "suspending_stream_send",
        "suspending_stream_send_abandon_cleanup",
        "suspending stream-send abandon -> shared cleanup br",
        || {
            // ── abandon_cleanup: cancel + detach (drop the pending item + release
            // the core ref) + free, then join shared cleanup. ───────────────────
            fn_ctx
                .builder
                .build_call(
                    slot_cancel,
                    &[slot.into()],
                    "suspending_stream_send_abandon_cancel",
                )
                .llvm_ctx("hew_read_slot_cancel (abandon) call")?;
            fn_ctx
                .builder
                .build_call(
                    detach_await,
                    &[sink_ptr.into(), slot.into()],
                    "suspending_stream_send_abandon_detach",
                )
                .llvm_ctx("hew_sink_detach_await (abandon) call")?;
            fn_ctx
                .builder
                .build_call(
                    slot_free,
                    &[slot.into()],
                    "suspending_stream_send_abandon_free",
                )
                .llvm_ctx("hew_read_slot_free (abandon) call")?;
            Ok(())
        },
        || {
            // ── bind: ready-now OR resumed. `send` is unit — just release the
            // creator ref and branch to the MIR resume block. ───────────────────
            fn_ctx
                .builder
                .build_call(
                    slot_free,
                    &[slot.into()],
                    "suspending_stream_send_bind_free",
                )
                .llvm_ctx("hew_read_slot_free (bind) call")?;
            fn_ctx
                .builder
                .build_unconditional_branch(resume_bb)
                .llvm_ctx("suspending stream-send bind -> resume br")?;
            Ok(())
        },
    )?;

    Ok(())
}

/// Emit the caller-side non-blocking actor ask (W6.010 `Terminator::SuspendingAsk`).
///
/// Shape (the suspendable caller ramp):
/// ```text
///   self      = ctx.actor                         ; current_actor_ptr
///   ch        = hew_reply_channel_new()
///                hew_reply_channel_set_parked_waiter(ch, self)
///   rc        = hew_actor_ask_with_channel(actor, msg_type, payload, size, ch)
///   br (rc != 0) -> send_err, do_suspend
/// send_err:                                        ; submit failed, no reply ever
///   hew_reply_channel_free(ch)
///   error_dest = hew_actor_ask_take_last_error(); result_dest = Err(error_dest)
///   br resume_bb (MIR `resume` block — the bound result is read there)
/// do_suspend:                                      ; coro.suspend (non-final)
///   switch coro.suspend [default -> return handle, 0 -> reply_bind, 1 -> cleanup]
/// reply_bind:                                      ; the executor resumed us
///   reply = hew_reply_wait(ch)                     ; fast path — already ready
///   br (reply == null) -> ask_err, ask_ok
/// ask_ok:  reply_dest = *reply; result_dest = Ok(reply_dest); free(reply); free(ch); br resume_bb
/// ask_err: free(ch); error_dest = hew_actor_ask_take_last_error(); result_dest = Err; br resume_bb
/// ```
/// The value travels through `ch` (a frame-spilled local) across the suspend;
/// on resume `hew_reply_wait` returns immediately (the reply was deposited
/// before `enqueue_resume` woke us). The reply-channel ref count: `new` (+1
/// creator ref); `hew_actor_ask_with_channel` keeps the creator ref (the send
/// retains its own sender ref); `hew_reply_wait` does NOT consume a ref; the
/// single `hew_reply_channel_free` here releases the creator ref — matching the
/// blocking `hew_actor_ask` ownership model exactly.
#[allow(
    clippy::too_many_lines,
    reason = "the full caller-side ask ramp — send setup + suspend + the resume-edge \
              Ok/Err reply binding — is kept in one place so the suspend point and \
              the value routing it depends on are read together"
)]
pub(crate) fn emit_suspending_ask_terminator<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    term: SuspendingAskEmit,
) -> CodegenResult<()> {
    let ptr_ty = fn_ctx.ctx.ptr_type(AddressSpace::default());
    // The coro prologue must be present (lower_function detects the SuspendingAsk
    // carrier via `has_suspend` and emits it). Fail closed otherwise (R2 / the
    // Lane-B silent-no-op class) — a SuspendingAsk without coro state is a
    // producer that ran ahead of the prologue.
    let coro = fn_ctx.coro.ok_or_else(|| {
        CodegenError::FailClosed(
            "Terminator::SuspendingAsk reached codegen but the function carries no \
             coro prologue state — lower_function must detect the suspend carrier \
             (has_suspend) and emit the prologue before the body"
                .into(),
        )
    })?;
    let resume_bb = *fn_ctx.blocks.get(&term.resume).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "SuspendingAsk resume target bb{} not found",
            term.resume
        ))
    })?;
    if !fn_ctx.blocks.contains_key(&term.cleanup) {
        return Err(CodegenError::FailClosed(format!(
            "SuspendingAsk cleanup target bb{} not found",
            term.cleanup
        )));
    }

    // self = the current actor — the parked-continuation waiter the reply
    // re-enqueues. MUST come from `hew_actor_self()` (the thread-local execution
    // context), NOT the spilled `ctx` parameter: across a suspend the coroutine
    // frame spills param 0, but the scheduler's per-dispatch context it pointed
    // to is freed when the dispatch returns at the first suspend. On RESUME the
    // scheduler installs a fresh context (`resume_suspended_activation`), so the
    // thread-local read returns the live actor; a spilled-param read would
    // dereference freed stack (the multi-await crash). `hew_get_reply_channel`
    // in the body's reply deposit reads the same thread-local, so both halves of
    // the value routing agree on the current context.
    let actor_self_fn = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_actor_self",
    )?;
    let self_actor = fn_ctx
        .builder
        .build_call(actor_self_fn, &[], "suspending_ask_self")
        .llvm_ctx("hew_actor_self call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_actor_self returned void".into()))?
        .into_pointer_value();
    let actor_ptr = load_duplex_handle(fn_ctx, term.actor, "suspending_ask receiver")?;
    let (payload_ptr, payload_size) =
        actor_payload_ptr_size(fn_ctx, term.value, "suspending_ask_payload")?;

    let ch_new = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply_channel_new",
    )?;
    let ch = fn_ctx
        .builder
        .build_call(ch_new, &[], "suspending_ask_ch")
        .llvm_ctx("hew_reply_channel_new call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_reply_channel_new returned void".into()))?
        .into_pointer_value();
    fn_ctx.call_runtime_void(
        "hew_reply_channel_set_parked_waiter",
        &[ch.into(), self_actor.into()],
        "suspending_ask_set_waiter",
        "hew_reply_channel_set_parked_waiter call",
    )?;

    // #1739: register the reply type R's destructor on the channel before the
    // ask submits. This is THE timeout/cancel leak path — an `await … | after d`
    // (or an actor-shutdown race) can deposit an owned reply that no waiter ever
    // consumes; the channel free leg then runs this destructor on the
    // never-consumed buffer instead of leaking R's embedded heap. `ch` is the
    // channel just created above; the reply type comes from the receiver's
    // `Duplex<Msg, Reply>` handle type.
    let suspending_ask_reply_ty = ask_reply_ty(fn_ctx, term.reply_dest)?.clone();
    wire_reply_drop_fn(fn_ctx, ch, &suspending_ask_reply_ty)?;

    // ── NEW-6b await-deadline registration. Attach a one-shot cancel/deadline
    // record to the reply channel BEFORE the ask submits so the reply-deposit
    // path (`hew_await_cancel_complete` on the channel's `await_cancel`) is the
    // deadline-vs-reply arbiter. The timer itself is armed only on the commit
    // (`do_suspend`) path; `schedule_deadline_ms` no-ops if a reply already
    // completed the registration. `reg` is a frame-spilled local live across the
    // suspend (like `ch`). `None` when this ask carries no deadline. ───────────
    let i32_ty = fn_ctx.ctx.i32_type();
    let i64_ty = fn_ctx.ctx.i64_type();
    let reg: Option<inkwell::values::PointerValue<'ctx>> = if term.deadline_ns.is_some() {
        let cleanup_fn = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_reply_channel_cancel_cleanup",
        )?;
        let cleanup_ptr = cleanup_fn.as_global_value().as_pointer_value();
        let reg = emit_await_cancel_new(
            fn_ctx,
            self_actor,
            cleanup_ptr,
            ch,
            "suspending_ask_deadline_reg",
        )?;
        let set_await_cancel = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_reply_channel_set_await_cancel",
        )?;
        fn_ctx
            .builder
            .build_call(
                set_await_cancel,
                &[ch.into(), reg.into()],
                "suspending_ask_set_await_cancel",
            )
            .llvm_ctx("hew_reply_channel_set_await_cancel call")?;
        Some(reg)
    } else {
        None
    };

    let ask_fn = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_actor_ask_with_channel",
    )?;
    let msg_type_val = fn_ctx.ctx.i32_type().const_int(term.msg_type as u64, false);
    // `payload_size` is built as i64; the `size` param is `usize`/`size_t`
    // (i32 on wasm32). Reconcile to the target-correct width.
    let suspending_ask_size_ty = runtime_size_ty(fn_ctx.ctx, fn_ctx.llvm_mod);
    let payload_size = reconcile_int_width_signed(
        fn_ctx,
        payload_size.into(),
        suspending_ask_size_ty.into(),
        "suspending ask payload size",
    )?;
    let rc = fn_ctx
        .builder
        .build_call(
            ask_fn,
            &[
                actor_ptr.into(),
                msg_type_val.into(),
                payload_ptr.into(),
                payload_size.into(),
                ch.into(),
            ],
            "suspending_ask_submit",
        )
        .llvm_ctx("hew_actor_ask_with_channel call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_actor_ask_with_channel returned void".into()))?
        .into_int_value();

    let parent = fn_ctx
        .builder
        .get_insert_block()
        .and_then(|bb| bb.get_parent())
        .ok_or_else(|| CodegenError::Llvm("suspending ask block has no parent function".into()))?;
    let send_err_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "suspending_ask_send_err");
    let do_suspend_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "suspending_ask_suspend");
    let send_ok = fn_ctx
        .builder
        .build_int_compare(
            IntPredicate::EQ,
            rc,
            rc.get_type().const_zero(),
            "suspending_ask_send_ok",
        )
        .llvm_ctx("suspending ask send-ok compare")?;
    fn_ctx
        .builder
        .build_conditional_branch(send_ok, do_suspend_bb, send_err_bb)
        .llvm_ctx("suspending ask send branch")?;

    // ── send_err: the submit failed; no reply will ever arrive. Free the
    // channel and bind Err(AskError) without suspending (no worker to free —
    // we never parked). ──────────────────────────────────────────────────────
    fn_ctx.builder.position_at_end(send_err_bb);
    if let Some(plan) = &term.cleanup_plan {
        crate::llvm::emit_prepared_carrier_drop(fn_ctx, term.value, plan)?;
    }
    let ch_free = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply_channel_free",
    )?;
    fn_ctx
        .builder
        .build_call(ch_free, &[ch.into()], "suspending_ask_send_err_free")
        .llvm_ctx("hew_reply_channel_free (send err) call")?;
    if let Some(reg) = reg {
        // Submit failed → no reply, no suspend. Release the registration creator
        // ref (no timer was armed; the channel's retained ref drops at final free).
        let reg_free = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_free",
        )?;
        fn_ctx
            .builder
            .build_call(reg_free, &[reg.into()], "suspending_ask_send_err_reg_free")
            .llvm_ctx("hew_await_cancel_free (send err) call")?;
    }
    emit_suspending_ask_err(fn_ctx, term.result_dest, term.error_dest)?;
    fn_ctx
        .builder
        .build_unconditional_branch(resume_bb)
        .llvm_ctx("suspending ask send-err br")?;

    // ── do_suspend: park the continuation (non-final suspend). The default
    // edge returns the coro handle to the trampoline (which parks it on this
    // actor); case 0 resumes into the reply-bind block; case 1 tears down. ────
    fn_ctx.builder.position_at_end(do_suspend_bb);
    // ── NEW-6b: arm the deadline timer now that we are committed to suspending.
    // `schedule_deadline_ms` returns 0 once armed, and -1 either because (a) a
    // reply already completed the registration in the submit→here window — a
    // BENIGN fast-reply race, so the reply-bind path will return Ok — or because
    // (b) the timer wheel could not be obtained / the schedule genuinely failed
    // (`hew_global_timer_wheel()` null on init failure, or a null timer entry).
    // Case (b) must FAIL CLOSED: a `| after d` ask whose deadline cannot be armed
    // would otherwise silently degrade to an un-deadlined await that can hang,
    // contradicting the never-hang contract. We distinguish (a) from (b) by the
    // registration's terminal state: (a) leaves it `Completed`, (b) leaves it
    // `Pending`. On (b) we tear the ask down deterministically and bind
    // `Err(AskError::Timeout)` (the deadline cannot be honoured → conservatively
    // report the deadline as elapsed) WITHOUT parking — never a silent
    // un-deadlined await, never a crash.
    if let (Some(reg), Some(ns)) = (reg, term.deadline_ns) {
        // ns → ms (the runtime timer-wheel granularity), floored to ≥ 1 ms so a
        // sub-millisecond literal still arms one tick rather than rounding to 0.
        let delay_ms = (ns / 1_000_000).max(1) as u64;
        let tw_fn = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_global_timer_wheel",
        )?;
        // `hew_global_timer_wheel` returns null on init failure; the schedule call
        // below null-checks `tw` and returns -1, which we then route to fail-closed.
        let tw = fn_ctx
            .builder
            .build_call(tw_fn, &[], "suspending_ask_deadline_tw")
            .llvm_ctx("hew_global_timer_wheel call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("hew_global_timer_wheel returned void".into()))?
            .into_pointer_value();
        let schedule = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_schedule_deadline_ms",
        )?;
        let delay_val = i64_ty.const_int(delay_ms, false);
        let sched_rc = fn_ctx
            .builder
            .build_call(
                schedule,
                &[reg.into(), tw.into(), delay_val.into()],
                "suspending_ask_schedule_deadline",
            )
            .llvm_ctx("hew_await_cancel_schedule_deadline_ms call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| {
                CodegenError::FailClosed(
                    "hew_await_cancel_schedule_deadline_ms returned void".into(),
                )
            })?
            .into_int_value();
        let armed = fn_ctx
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                sched_rc,
                sched_rc.get_type().const_zero(),
                "suspending_ask_deadline_armed",
            )
            .llvm_ctx("suspending ask schedule-armed compare")?;
        let deadline_proceed_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_ask_deadline_proceed");
        let deadline_check_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_ask_deadline_check");
        fn_ctx
            .builder
            .build_conditional_branch(armed, deadline_proceed_bb, deadline_check_bb)
            .llvm_ctx("suspending ask schedule-armed branch")?;

        // ── deadline_check: schedule returned -1. If the registration is already
        // `Completed`, a reply won the race (benign) → proceed to the reply bind.
        // Otherwise the deadline could not be armed → fail closed. ───────────────
        fn_ctx.builder.position_at_end(deadline_check_bb);
        let status_fn = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_status",
        )?;
        let sched_status = fn_ctx
            .builder
            .build_call(status_fn, &[reg.into()], "suspending_ask_schedule_status")
            .llvm_ctx("hew_await_cancel_status (schedule) call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| {
                CodegenError::FailClosed("hew_await_cancel_status returned void".into())
            })?
            .into_int_value();
        // AwaitCancelStatus::Completed = 1.
        let reply_completed = fn_ctx
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                sched_status,
                i32_ty.const_int(1, false),
                "suspending_ask_schedule_reply_completed",
            )
            .llvm_ctx("suspending ask schedule reply-completed compare")?;
        let deadline_failclosed_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_ask_deadline_failclosed");
        fn_ctx
            .builder
            .build_conditional_branch(reply_completed, deadline_proceed_bb, deadline_failclosed_bb)
            .llvm_ctx("suspending ask schedule reply-completed branch")?;

        // ── deadline_failclosed: the deadline could not be armed and no reply
        // completed the registration. Terminalize the registration (disarms any
        // timer, tombstones the channel so a late replier releases its sender
        // ref), cancel + free the reply channel, release the creator ref, bind
        // `Err(AskError::Timeout)`, and resume WITHOUT ever parking. ─────────────
        fn_ctx.builder.position_at_end(deadline_failclosed_bb);
        let reg_cancel = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_cancel",
        )?;
        let cancelled_status = i32_ty.const_int(2, false); // AwaitCancelStatus::Cancelled
        let no_wake = i32_ty.const_zero();
        fn_ctx
            .builder
            .build_call(
                reg_cancel,
                &[reg.into(), cancelled_status.into(), no_wake.into()],
                "suspending_ask_failclosed_reg_cancel",
            )
            .llvm_ctx("hew_await_cancel_cancel (fail-closed) call")?;
        let reg_free = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_free",
        )?;
        fn_ctx
            .builder
            .build_call(
                reg_free,
                &[reg.into()],
                "suspending_ask_failclosed_reg_free",
            )
            .llvm_ctx("hew_await_cancel_free (fail-closed) call")?;
        let ch_cancel = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_reply_channel_cancel",
        )?;
        fn_ctx
            .builder
            .build_call(
                ch_cancel,
                &[ch.into()],
                "suspending_ask_failclosed_ch_cancel",
            )
            .llvm_ctx("hew_reply_channel_cancel (fail-closed) call")?;
        let ch_free = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_reply_channel_free",
        )?;
        fn_ctx
            .builder
            .build_call(ch_free, &[ch.into()], "suspending_ask_failclosed_ch_free")
            .llvm_ctx("hew_reply_channel_free (fail-closed) call")?;
        // AskError::Timeout = 5.
        let timeout_code = i32_ty.const_int(5, false);
        emit_suspending_ask_err_with_code(fn_ctx, term.result_dest, term.error_dest, timeout_code)?;
        fn_ctx
            .builder
            .build_unconditional_branch(resume_bb)
            .llvm_ctx("suspending ask deadline fail-closed br")?;

        // Proceed: the deadline armed (or a reply already won) — continue to the
        // coro suspend. Subsequent emit_suspend uses the current builder block.
        fn_ctx.builder.position_at_end(deadline_proceed_bb);
    }
    let reply_bind_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "suspending_ask_reply_bind");
    // ── abandon_cleanup edge (S2): when a parked SuspendingAsk continuation is
    // DESTROYED without resuming (actor stop/crash while suspended, begin_park
    // refusal, the C1 free-path destroy_parked), `coro.suspend`'s case-1 edge
    // runs. The shared `coro.cleanup_block` only frees the coro frame — it never
    // sees `ch` (a codegen-local SSA value with no MIR Place), so without this
    // interposition `ch` leaks. The abandon continuation CANCELS + FREES the
    // creator ref before joining the shared cleanup. Cancel-then-free mirrors the
    // blocking-ask timeout abandon (actor.rs): the cancel makes a late replier
    // (the callee still holds the sender ref) release its own ref and skip the
    // wake; the free releases this abandoned caller's creator-side ref. The late
    // wake itself is independently fail-safe (the caller-actor UAF guard in
    // enqueue_resume drops a wake to a freed caller).
    emit_suspend_point(
        fn_ctx,
        coro,
        parent,
        reply_bind_bb,
        "suspending_ask",
        "suspending_ask_abandon_cleanup",
        "suspending ask abandon -> shared cleanup br",
        || {
            // ── abandon_cleanup: free the awaited reply channel, then join the
            // shared coro cleanup (frame-free + coro.end). ──────────────────────
            if let Some(reg) = reg {
                // Parked continuation destroyed without resuming: force the
                // registration terminal (disarms the deadline timer, releasing its
                // retained ref and running the channel-tombstone cleanup while `ch`
                // is still live), then release the creator ref. wake_actor = 0: we
                // are tearing down, not resuming. The channel's own retained ref
                // drops at its final free below.
                let reg_cancel = intern_runtime_decl(
                    fn_ctx.ctx,
                    fn_ctx.llvm_mod,
                    &mut fn_ctx.runtime_decls.borrow_mut(),
                    "hew_await_cancel_cancel",
                )?;
                let cancelled_status = i32_ty.const_int(2, false); // AwaitCancelStatus::Cancelled
                let no_wake = i32_ty.const_zero();
                fn_ctx
                    .builder
                    .build_call(
                        reg_cancel,
                        &[reg.into(), cancelled_status.into(), no_wake.into()],
                        "suspending_ask_abandon_reg_cancel",
                    )
                    .llvm_ctx("hew_await_cancel_cancel (abandon) call")?;
                let reg_free = intern_runtime_decl(
                    fn_ctx.ctx,
                    fn_ctx.llvm_mod,
                    &mut fn_ctx.runtime_decls.borrow_mut(),
                    "hew_await_cancel_free",
                )?;
                fn_ctx
                    .builder
                    .build_call(reg_free, &[reg.into()], "suspending_ask_abandon_reg_free")
                    .llvm_ctx("hew_await_cancel_free (abandon) call")?;
            }
            let ch_cancel = intern_runtime_decl(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                &mut fn_ctx.runtime_decls.borrow_mut(),
                "hew_reply_channel_cancel",
            )?;
            fn_ctx
                .builder
                .build_call(ch_cancel, &[ch.into()], "suspending_ask_abandon_cancel")
                .llvm_ctx("hew_reply_channel_cancel (abandon) call")?;
            fn_ctx
                .builder
                .build_call(ch_free, &[ch.into()], "suspending_ask_abandon_free")
                .llvm_ctx("hew_reply_channel_free (abandon) call")?;
            Ok(())
        },
        || emit_suspending_ask_reply_bind(fn_ctx, &term, ch, reg, resume_bb, parent),
    )?;

    let _ = ptr_ty;
    Ok(())
}

/// Resume-bind continuation of [`emit_suspending_ask_terminator`]: the executor
/// resumed us (enqueue_resume). The reply is already deposited on `ch`;
/// `hew_reply_wait` returns it on the fast path. Bind Ok(reply)/Err(AskError)
/// exactly as Terminator::Ask does, then free the reply payload + the channel,
/// and branch to the MIR resume block. Split out so the suspend-point seam owns
/// the suspend + abandon scaffolding while this owns the value routing.
#[allow(
    clippy::too_many_lines,
    reason = "the full reply-bind edge — deadline-vs-reply resolution + the \
              Ok/Err binding — is kept in one place so the deadline arbiter and \
              the value routing it gates are read together"
)]
fn emit_suspending_ask_reply_bind<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    term: &SuspendingAskEmit,
    ch: inkwell::values::PointerValue<'ctx>,
    reg: Option<inkwell::values::PointerValue<'ctx>>,
    resume_bb: inkwell::basic_block::BasicBlock<'ctx>,
    parent: FunctionValue<'ctx>,
) -> CodegenResult<()> {
    let i32_ty = fn_ctx.ctx.i32_type();
    let ch_free = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply_channel_free",
    )?;
    let ch_cancel = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply_channel_cancel",
    )?;
    // ── NEW-6b deadline-vs-reply resolution at the single resume convergence
    // point. Whichever wake fired (reply deposit or deadline timer), the one-shot
    // registration is authoritative: `complete` claims the reply if still pending
    // (which cancels the timer); if the timer already won, `complete` is a no-op
    // and `status` reads TimedOut. On TimedOut we bind `Err(AskError::Timeout)`
    // and tear the channel down WITHOUT waiting on a reply that will never come.
    if let Some(reg) = reg {
        let complete = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_complete",
        )?;
        fn_ctx
            .builder
            .build_call(complete, &[reg.into()], "suspending_ask_deadline_complete")
            .llvm_ctx("hew_await_cancel_complete call")?;
        let status_fn = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_status",
        )?;
        let status = fn_ctx
            .builder
            .build_call(status_fn, &[reg.into()], "suspending_ask_deadline_status")
            .llvm_ctx("hew_await_cancel_status call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| {
                CodegenError::FailClosed("hew_await_cancel_status returned void".into())
            })?
            .into_int_value();
        // AwaitCancelStatus::TimedOut = 3.
        let timed_out = fn_ctx
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                status,
                i32_ty.const_int(3, false),
                "suspending_ask_timed_out",
            )
            .llvm_ctx("suspending ask timed-out compare")?;
        let timeout_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_ask_timeout");
        let reply_proceed_bb = fn_ctx
            .ctx
            .append_basic_block(parent, "suspending_ask_reply_proceed");
        fn_ctx
            .builder
            .build_conditional_branch(timed_out, timeout_bb, reply_proceed_bb)
            .llvm_ctx("suspending ask deadline branch")?;

        // ── timeout: the deadline elapsed first. Release the registration creator
        // ref, cancel + free the reply channel (a late replier releases its sender
        // ref on the tombstone), bind `Err(AskError::Timeout)`, and resume. ───────
        fn_ctx.builder.position_at_end(timeout_bb);
        let reg_free = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_free",
        )?;
        fn_ctx
            .builder
            .build_call(reg_free, &[reg.into()], "suspending_ask_timeout_reg_free")
            .llvm_ctx("hew_await_cancel_free (timeout) call")?;
        fn_ctx
            .builder
            .build_call(ch_cancel, &[ch.into()], "suspending_ask_timeout_ch_cancel")
            .llvm_ctx("hew_reply_channel_cancel (timeout) call")?;
        fn_ctx
            .builder
            .build_call(ch_free, &[ch.into()], "suspending_ask_timeout_ch_free")
            .llvm_ctx("hew_reply_channel_free (timeout) call")?;
        // AskError::Timeout = 5 (runtime internal::types::AskError; the Hew std
        // AskError enum shares the same discriminant).
        let timeout_code = i32_ty.const_int(5, false);
        emit_suspending_ask_err_with_code(fn_ctx, term.result_dest, term.error_dest, timeout_code)?;
        fn_ctx
            .builder
            .build_unconditional_branch(resume_bb)
            .llvm_ctx("suspending ask timeout br")?;

        // Reply landed before the deadline: continue the normal Ok/Err binding.
        fn_ctx.builder.position_at_end(reply_proceed_bb);
    }
    let reply_wait = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply_wait",
    )?;
    let reply_ptr = fn_ctx
        .builder
        .build_call(reply_wait, &[ch.into()], "suspending_ask_reply")
        .llvm_ctx("hew_reply_wait call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_reply_wait returned void".into()))?
        .into_pointer_value();
    let ask_ok_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "suspending_ask_reply_ok");
    let ask_err_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "suspending_ask_reply_err");
    let reply_is_null = fn_ctx
        .builder
        .build_is_null(reply_ptr, "suspending_ask_reply_is_null")
        .llvm_ctx("suspending ask reply null compare")?;
    fn_ctx
        .builder
        .build_conditional_branch(reply_is_null, ask_err_bb, ask_ok_bb)
        .llvm_ctx("suspending ask reply branch")?;

    // Ok path: load reply value -> reply_dest -> Result::Ok; free reply + ch.
    fn_ctx.builder.position_at_end(ask_ok_bb);
    let (reply_dest_ptr, reply_dest_ty) = place_pointer(fn_ctx, term.reply_dest)?;
    let reply_val = fn_ctx
        .builder
        .build_load(reply_dest_ty, reply_ptr, "suspending_ask_reply_value")
        .llvm_ctx("suspending ask reply load")?;
    fn_ctx
        .builder
        .build_store(reply_dest_ptr, reply_val)
        .llvm_ctx("suspending ask reply store")?;
    emit_result_ok(fn_ctx, term.result_dest, Some(term.reply_dest))?;
    let free = get_or_declare_free(fn_ctx);
    fn_ctx
        .builder
        .build_call(free, &[reply_ptr.into()], "suspending_ask_reply_free")
        .llvm_ctx("free suspending ask reply")?;
    fn_ctx
        .builder
        .build_call(ch_free, &[ch.into()], "suspending_ask_ok_ch_free")
        .llvm_ctx("hew_reply_channel_free (ok) call")?;
    if let Some(reg) = reg {
        let reg_free = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_free",
        )?;
        fn_ctx
            .builder
            .build_call(reg_free, &[reg.into()], "suspending_ask_ok_reg_free")
            .llvm_ctx("hew_await_cancel_free (ok) call")?;
    }
    fn_ctx
        .builder
        .build_unconditional_branch(resume_bb)
        .llvm_ctx("suspending ask ok br")?;

    // Err path: null reply -> Err(AskError). A null reply is either a mailbox
    // teardown (the channel's `orphaned` flag is set → AskError::OrphanedAsk) or
    // a send/other failure recorded in the TLS last-error slot. Read the
    // channel-local orphaned flag BEFORE freeing the ref, then bind the SAME
    // discriminator the blocking-ask path binds (P2a): `select(orphaned,
    // OrphanedAsk, take_last_error())`. Without this the resume edge bound the
    // TLS last-error, which never carries the channel-local orphaned fact, so
    // mailbox-teardown asks reported the wrong error variant.
    fn_ctx.builder.position_at_end(ask_err_bb);
    let orphaned_i32 = fn_ctx.call_runtime_int(
        "hew_reply_channel_is_orphaned",
        &[ch.into()],
        "suspending_ask_is_orphaned",
        "hew_reply_channel_is_orphaned call",
    )?;
    // Free the caller-side ref AFTER reading the orphaned flag.
    fn_ctx
        .builder
        .build_call(ch_free, &[ch.into()], "suspending_ask_err_ch_free")
        .llvm_ctx("hew_reply_channel_free (err) call")?;
    if let Some(reg) = reg {
        let reg_free = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_free",
        )?;
        fn_ctx
            .builder
            .build_call(reg_free, &[reg.into()], "suspending_ask_err_reg_free")
            .llvm_ctx("hew_await_cancel_free (err) call")?;
    }
    let is_orphaned = fn_ctx
        .builder
        .build_int_compare(
            IntPredicate::NE,
            orphaned_i32,
            i32_ty.const_zero(),
            "suspending_ask_orphaned_flag",
        )
        .llvm_ctx("suspending ask orphaned compare")?;
    let last_error = fn_ctx.call_runtime_int(
        "hew_actor_ask_take_last_error",
        &[],
        "suspending_ask_err_take_last_error",
        "hew_actor_ask_take_last_error call",
    )?;
    // AskError::OrphanedAsk = 11 (hew-runtime internal::types::AskError); the Hew
    // AskError enum tag uses the same discriminants the blocking path stores.
    let orphaned_ask_code = i32_ty.const_int(11, false);
    let err_code = fn_ctx
        .builder
        .build_select(
            is_orphaned,
            orphaned_ask_code,
            last_error,
            "suspending_ask_err_code",
        )
        .llvm_ctx("suspending ask err-code select")?
        .into_int_value();
    emit_suspending_ask_err_with_code(fn_ctx, term.result_dest, term.error_dest, err_code)?;
    fn_ctx
        .builder
        .build_unconditional_branch(resume_bb)
        .llvm_ctx("suspending ask err br")?;

    Ok(())
}

/// Emit the suspendable-callee driver (`Terminator::SuspendingCallClosure`): call
/// a closure whose body `await`s across the coroutine boundary, drive that callee
/// coroutine, and PROPAGATE its suspension up into THIS calling coroutine.
///
/// Shape (the driver loop):
/// ```text
///   ctx       = <live execution context>            ; the closure's ctx arg
///   (fn,env)  = <load closure pair>
///   ch        = hew_reply_channel_new()             ; driver-owned result box
///   hew_context_reply_channel_swap_push(ch)         ; scoped: route child Return -> ch
///   child     = fn(ctx, env, args...)               ; ramp -> coro handle (ptr)
///               hew_context_reply_channel_swap_pop()
///   br check_done
/// check_done:                                        ; driver loop header
///   poll = hew_cont_poll(child, null)               ; Ready(1) => child completed
///   br (poll == Ready) -> finish, do_suspend
/// do_suspend:                                        ; coro.suspend (non-final)
///   switch coro.suspend [default -> return handle, 0 -> resume_child, 1 -> abandon]
/// resume_child:                                      ; reactor woke the actor
///   hew_context_reply_channel_swap_push(ch)
///   hew_cont_resume(child)                           ; run child to next suspend/done
///   hew_context_reply_channel_swap_pop()
///   br check_done                                    ; multi-suspend re-parks here
/// abandon:                                           ; caller destroyed while parked
///   hew_cont_destroy(child); cancel+free(ch); br shared cleanup
/// finish:                                            ; child completed
///   reply = hew_reply_wait(ch)                       ; the deposited return value
///   *result_dest = *reply; free(reply)               ; non-unit only
///   [unit only: free(ch) sender ref]                 ; child deposited nothing
///   hew_reply_channel_free(ch); hew_cont_destroy(child)
///   br resume_bb
/// ```
/// The child's `Return` runs synchronously inside the ramp/`hew_cont_resume`
/// call, so the swap window covers its `hew_reply` deposit. The swap is a SCOPED
/// transfer of BOTH the reply-channel pointer AND the consumed bit (saved on
/// push, restored on pop), and the scheduler crash-recovery edge unwinds any
/// still-open swap (`reply_channel_swap_unwind`) — so the outer dispatch's
/// reply routing is restored and the driver channel is torn down on EVERY exit
/// edge (normal return, trap, cancel, unwind), not just the normal-return path.
/// The child's OWN read
/// suspend registers `self = hew_actor_self()` as the wake target, so the reactor
/// wakes the calling ACTOR, whose parked outer coroutine resumes at `resume_child`
/// and re-resumes the child handle (R2 wake-target threading).
#[allow(
    clippy::too_many_lines,
    reason = "the full suspendable-callee driver — ramp invoke + the park/resume \
              loop + the completion value binding — is kept in one place so the \
              suspend point and the value routing it depends on are read together"
)]
pub(crate) fn emit_suspending_call_closure_terminator<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    term: SuspendingCallClosureEmit<'_>,
) -> CodegenResult<()> {
    let ptr_ty = fn_ctx.ctx.ptr_type(AddressSpace::default());
    let i32_ty = fn_ctx.ctx.i32_type();
    // The coro prologue must be present (lower_function detects the suspend
    // carrier via `has_suspend` and emits it). Fail closed otherwise (R2 / the
    // Lane-B silent-no-op class).
    let coro = fn_ctx.coro.ok_or_else(|| {
        CodegenError::FailClosed(
            "Terminator::SuspendingCallClosure reached codegen but the function carries \
             no coro prologue state — lower_function must detect the suspend carrier \
             (has_suspend) and emit the prologue before the body"
                .into(),
        )
    })?;
    let resume_bb = *fn_ctx.blocks.get(&term.resume).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "SuspendingCallClosure resume target bb{} not found",
            term.resume
        ))
    })?;
    if !fn_ctx.blocks.contains_key(&term.cleanup) {
        return Err(CodegenError::FailClosed(format!(
            "SuspendingCallClosure cleanup target bb{} not found",
            term.cleanup
        )));
    }

    // Resolve the live execution context (the closure's leading ctx arg — the
    // resume-installed context after a suspend, not the dangling spilled param).
    let ctx_ptr = crate::thunks::closure_call_context(fn_ctx)?;

    // Load the two-pointer closure pair (fn-ptr + env-ptr) exactly as
    // `lower_call_closure` does.
    let (callee_ptr, callee_ty) = place_pointer(fn_ctx, term.callee)?;
    let pair_ty = match callee_ty {
        BasicTypeEnum::StructType(st) if st.count_fields() == 2 => st,
        other => {
            return Err(CodegenError::FailClosed(format!(
                "SuspendingCallClosure callee must be the two-pointer closure pair, got {other:?}"
            )));
        }
    };
    let pair = fn_ctx
        .builder
        .build_load(pair_ty, callee_ptr, "suspending_closure_pair_load")
        .llvm_ctx("SuspendingCallClosure pair load")?
        .into_struct_value();
    let fn_ptr = fn_ctx
        .builder
        .build_extract_value(pair, 0, "suspending_closure_fn_extract")
        .llvm_ctx("SuspendingCallClosure fn extract")?
        .into_pointer_value();
    let env_ptr = fn_ctx
        .builder
        .build_extract_value(pair, 1, "suspending_closure_env_extract")
        .llvm_ctx("SuspendingCallClosure env extract")?
        .into_pointer_value();

    // Build the ramp call signature. The callee is a coroutine RAMP whose LLVM
    // return is the `coro.begin` handle (`ptr`), NOT `ret_ty` — this is the fix
    // to the plain `lower_call_closure` ABI mismatch.
    let mut param_tys: Vec<BasicMetadataTypeEnum> =
        Vec::with_capacity(term.args.len().saturating_add(2));
    param_tys.push(ptr_ty.into());
    param_tys.push(ptr_ty.into());
    let mut arg_vals: Vec<inkwell::values::BasicMetadataValueEnum> =
        Vec::with_capacity(term.args.len().saturating_add(2));
    arg_vals.push(ctx_ptr.into());
    arg_vals.push(env_ptr.into());
    for arg in &term.args {
        let (arg_ptr, arg_ty) = place_pointer(fn_ctx, *arg)?;
        param_tys.push(metadata_type_from_basic(arg_ty));
        let loaded = fn_ctx
            .builder
            .build_load(arg_ty, arg_ptr, "suspending_closure_arg")
            .llvm_ctx("SuspendingCallClosure arg load")?;
        arg_vals.push(metadata_value_from_basic(loaded));
    }
    let ramp_fn_ty = fn_type_for_return(fn_ctx.ctx, Some(ptr_ty.into()), &param_tys);

    // Driver-owned reply channel: the child closure coroutine's `Return` arm
    // deposits its logical value here (via the swapped-in context reply channel).
    let ch_new = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply_channel_new",
    )?;
    let ch = fn_ctx
        .builder
        .build_call(ch_new, &[], "suspending_closure_ch")
        .llvm_ctx("hew_reply_channel_new call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_reply_channel_new returned void".into()))?
        .into_pointer_value();
    // #1739: register the return type's destructor on the channel before the
    // child closure can deposit a reply. If this suspending call is abandoned
    // (cancel / coro destroy / shutdown) after the child deposited but before
    // the driver consumed, the channel free leg runs this destructor on the
    // never-consumed buffer rather than leaking the reply's embedded heap. The
    // reply type is the closure's return type.
    wire_reply_drop_fn(fn_ctx, ch, term.ret_ty)?;
    // Ref accounting (mirrors the SuspendingAsk model): `new` gives the +1
    // wait-side (creator) ref this driver frees on the completion edge. We
    // UNCONDITIONALLY retain one sender ref here (Option B): for a non-unit
    // return the child's `Return` deposit (`hew_reply`) RELEASES this sender ref
    // (otherwise it would free the channel out from under `hew_reply_wait` —
    // use-after-free); for a unit/never return the child deposits nothing, so
    // the sender ref stays live and the unit finish path releases it explicitly
    // below. Keeping the retain unconditional means the channel always carries
    // exactly two refs whenever the child has NOT deposited (the abandon and the
    // crash-unwind edges both free two), so trap/cancel/unwind teardown is a
    // single uniform rule.
    fn_ctx.call_runtime_void(
        "hew_reply_channel_retain",
        &[ch.into()],
        "suspending_closure_ch_retain",
        "hew_reply_channel_retain call",
    )?;

    let swap_push_fn = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_context_reply_channel_swap_push",
    )?;
    let swap_pop_fn = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_context_reply_channel_swap_pop",
    )?;
    let cont_poll_fn = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_cont_poll",
    )?;
    let cont_resume_fn = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_cont_resume",
    )?;
    let cont_destroy_fn = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_cont_destroy",
    )?;
    let ch_free = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply_channel_free",
    )?;

    // Helper: open/close a SCOPED swap of `ch` around a synchronous child call.
    // `swap_in` saves the outer reply-channel pointer + consumed bit and installs
    // `ch`; `swap_out` restores both. The crash/trap edge is covered separately
    // by the scheduler's `reply_channel_swap_unwind`, so restoration is
    // structurally guaranteed on every exit path, not just the normal return.
    let swap_in = |label: &str| -> CodegenResult<()> {
        fn_ctx
            .builder
            .build_call(swap_push_fn, &[ch.into()], label)
            .llvm_ctx("hew_context_reply_channel_swap_push call")?;
        Ok(())
    };
    let swap_out = |label: &str| -> CodegenResult<()> {
        fn_ctx
            .builder
            .build_call(swap_pop_fn, &[], label)
            .llvm_ctx("hew_context_reply_channel_swap_pop call")?;
        Ok(())
    };

    // ── invoke the ramp under the swapped-in channel ───────────────────────────
    swap_in("suspending_closure_swap_in")?;
    let child = fn_ctx
        .builder
        .build_indirect_call(ramp_fn_ty, fn_ptr, &arg_vals, "suspending_closure_child")
        .llvm_ctx("SuspendingCallClosure ramp call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| {
            CodegenError::FailClosed(
                "suspending closure ramp returned void; a coroutine ramp must return its \
                 coro.begin handle (ptr)"
                    .into(),
            )
        })?
        .into_pointer_value();
    swap_out("suspending_closure_swap_out")?;

    let parent = fn_ctx
        .builder
        .get_insert_block()
        .and_then(|bb| bb.get_parent())
        .ok_or_else(|| {
            CodegenError::Llvm("suspending closure call block has no parent function".into())
        })?;
    let check_done_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "suspending_closure_check_done");
    let do_suspend_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "suspending_closure_suspend");
    let resume_child_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "suspending_closure_resume_child");
    let finish_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "suspending_closure_finish");

    fn_ctx
        .builder
        .build_unconditional_branch(check_done_bb)
        .llvm_ctx("suspending closure -> check_done br")?;

    // ── check_done: poll the child for completion (Ready) vs still-suspended. ──
    fn_ctx.builder.position_at_end(check_done_bb);
    let poll = fn_ctx
        .builder
        .build_call(
            cont_poll_fn,
            &[child.into(), ptr_ty.const_null().into()],
            "suspending_closure_poll",
        )
        .llvm_ctx("hew_cont_poll call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_cont_poll returned void".into()))?
        .into_int_value();
    // ResumePoll::Ready == 1 → the child reached its final suspend (completed).
    let is_ready = fn_ctx
        .builder
        .build_int_compare(
            IntPredicate::EQ,
            poll,
            i32_ty.const_int(1, false),
            "suspending_closure_is_ready",
        )
        .llvm_ctx("suspending closure ready compare")?;
    fn_ctx
        .builder
        .build_conditional_branch(is_ready, finish_bb, do_suspend_bb)
        .llvm_ctx("suspending closure poll branch")?;

    // ── do_suspend: park THIS coroutine (non-final). default -> executor; case
    // 0 -> resume_child; case 1 -> abandon. ───────────────────────────────────
    fn_ctx.builder.position_at_end(do_suspend_bb);
    emit_suspend_point(
        fn_ctx,
        coro,
        parent,
        resume_child_bb,
        "suspending_closure",
        "suspending_closure_abandon",
        "suspending closure abandon -> shared cleanup br",
        || {
            // ── abandon: the parked continuation was destroyed without resuming.
            // Destroy the child handle + cancel/free the driver channel, then
            // join the shared coro cleanup (frame-free + coro.end). ─────────────
            fn_ctx
                .builder
                .build_call(
                    cont_destroy_fn,
                    &[child.into()],
                    "suspending_closure_abandon_destroy",
                )
                .llvm_ctx("hew_cont_destroy (abandon) call")?;
            let ch_cancel = intern_runtime_decl(
                fn_ctx.ctx,
                fn_ctx.llvm_mod,
                &mut fn_ctx.runtime_decls.borrow_mut(),
                "hew_reply_channel_cancel",
            )?;
            fn_ctx
                .builder
                .build_call(ch_cancel, &[ch.into()], "suspending_closure_abandon_cancel")
                .llvm_ctx("hew_reply_channel_cancel (abandon) call")?;
            // The destroyed child never deposits, so its retained sender ref is
            // never released by `hew_reply`; release BOTH the sender ref and the
            // creator ref here so the channel is freed exactly once (no leak).
            fn_ctx
                .builder
                .build_call(
                    ch_free,
                    &[ch.into()],
                    "suspending_closure_abandon_free_sender",
                )
                .llvm_ctx("hew_reply_channel_free (abandon sender) call")?;
            fn_ctx
                .builder
                .build_call(
                    ch_free,
                    &[ch.into()],
                    "suspending_closure_abandon_free_creator",
                )
                .llvm_ctx("hew_reply_channel_free (abandon creator) call")?;
            Ok(())
        },
        || {
            // ── resume_child: the reactor woke the calling actor; re-resume the
            // child (under the swapped-in channel) and re-poll. Multi-suspend
            // closure bodies re-park here on the next yield. ───────────────────
            swap_in("suspending_closure_resume_swap_in")?;
            fn_ctx
                .builder
                .build_call(cont_resume_fn, &[child.into()], "suspending_closure_resume")
                .llvm_ctx("hew_cont_resume call")?;
            swap_out("suspending_closure_resume_swap_out")?;
            fn_ctx
                .builder
                .build_unconditional_branch(check_done_bb)
                .llvm_ctx("suspending closure resume -> check_done br")?;
            Ok(())
        },
    )?;

    // ── finish: the child completed and (for a non-unit closure) deposited its
    // return value onto `ch`. Bind it, free the reply payload + channel, destroy
    // the child frame, and branch to the MIR resume block. ────────────────────
    fn_ctx.builder.position_at_end(finish_bb);
    if let Some(dest) = term.result_dest {
        // A unit/never logical return deposits no reply (the coro Return arm only
        // deposits a non-unit logical value), so `hew_reply_wait` is reached ONLY
        // when a value was deposited — it returns it on the fast path (the deposit
        // ran synchronously inside the resume above, before `done`).
        let _ = term.ret_ty;
        let reply_wait = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_reply_wait",
        )?;
        let reply_ptr = fn_ctx
            .builder
            .build_call(reply_wait, &[ch.into()], "suspending_closure_reply")
            .llvm_ctx("hew_reply_wait call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("hew_reply_wait returned void".into()))?
            .into_pointer_value();
        let (dest_ptr, dest_ty) = place_pointer(fn_ctx, dest)?;
        let reply_val = fn_ctx
            .builder
            .build_load(dest_ty, reply_ptr, "suspending_closure_reply_value")
            .llvm_ctx("suspending closure reply load")?;
        fn_ctx
            .builder
            .build_store(dest_ptr, reply_val)
            .llvm_ctx("suspending closure reply store")?;
        let free = get_or_declare_free(fn_ctx);
        fn_ctx
            .builder
            .build_call(free, &[reply_ptr.into()], "suspending_closure_reply_free")
            .llvm_ctx("free suspending closure reply")?;
    } else {
        // Unit/never logical return: the child deposited NOTHING (the coro Return
        // arm only deposits a non-unit logical value), so the sender ref retained
        // above was never released by `hew_reply`. Release it here so the unit
        // finish path frees BOTH refs (sender now + creator below) and the channel
        // is reclaimed exactly once — fixing the unit-closure reply-channel leak.
        fn_ctx
            .builder
            .build_call(
                ch_free,
                &[ch.into()],
                "suspending_closure_ok_unit_free_sender",
            )
            .llvm_ctx("hew_reply_channel_free (ok unit sender) call")?;
    }
    fn_ctx
        .builder
        .build_call(ch_free, &[ch.into()], "suspending_closure_ok_ch_free")
        .llvm_ctx("hew_reply_channel_free (ok) call")?;
    fn_ctx
        .builder
        .build_call(
            cont_destroy_fn,
            &[child.into()],
            "suspending_closure_ok_destroy",
        )
        .llvm_ctx("hew_cont_destroy (ok) call")?;
    fn_ctx
        .builder
        .build_unconditional_branch(resume_bb)
        .llvm_ctx("suspending closure ok -> resume br")?;

    Ok(())
}

/// Emit `result_dest = Err(AskError::<hew_actor_ask_take_last_error()>)` —
/// the SuspendingAsk send-failure Err-binding (no channel exists yet, so the
/// TLS last-error slot is authoritative), identical to the `Terminator::Ask`
/// err arm.
fn emit_suspending_ask_err<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    result_dest: Place,
    error_dest: Place,
) -> CodegenResult<()> {
    let err_fn = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_actor_ask_take_last_error",
    )?;
    let err_code = fn_ctx
        .builder
        .build_call(err_fn, &[], "suspending_ask_take_last_error_call")
        .llvm_ctx("hew_actor_ask_take_last_error call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| {
            CodegenError::FailClosed("hew_actor_ask_take_last_error returned void".into())
        })?
        .into_int_value();
    emit_suspending_ask_err_with_code(fn_ctx, result_dest, error_dest, err_code)
}

/// Emit `result_dest = Err(AskError::<err_code>)` — binds the AskError tag from
/// a precomputed discriminant. The null-reply resume path uses this to bind
/// `OrphanedAsk` from the channel's orphaned flag (matching the blocking-ask
/// path), which the TLS last-error slot does not carry.
fn emit_suspending_ask_err_with_code<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    result_dest: Place,
    error_dest: Place,
    err_code: inkwell::values::IntValue<'ctx>,
) -> CodegenResult<()> {
    let error_local = composite_dest_local(error_dest, "SuspendingAsk AskError")?;
    let (error_tag_ptr, error_tag_ty) = place_pointer(fn_ctx, Place::MachineTag(error_local))?;
    let error_tag_int_ty = match error_tag_ty {
        BasicTypeEnum::IntType(t) => t,
        other => {
            return Err(CodegenError::FailClosed(format!(
                "SuspendingAsk AskError tag projection must be integer, got {other:?}"
            )));
        }
    };
    let err_code = match err_code
        .get_type()
        .get_bit_width()
        .cmp(&error_tag_int_ty.get_bit_width())
    {
        std::cmp::Ordering::Equal => err_code,
        std::cmp::Ordering::Less => fn_ctx
            .builder
            .build_int_z_extend(err_code, error_tag_int_ty, "suspending_ask_err_zext")
            .llvm_ctx("suspending ask err zext")?,
        std::cmp::Ordering::Greater => fn_ctx
            .builder
            .build_int_truncate(err_code, error_tag_int_ty, "suspending_ask_err_trunc")
            .llvm_ctx("suspending ask err trunc")?,
    };
    fn_ctx
        .builder
        .build_store(error_tag_ptr, err_code)
        .llvm_ctx("store SuspendingAsk AskError tag")?;
    emit_result_err(fn_ctx, result_dest, error_dest)
}

/// Bind a `Result::Err(AskError)` into `result_dest` from the node ask-error
/// slot (`hew_node_ask_take_last_error`). Shared by the blocking and suspending
/// remote-ask failure edges — both read the same thread-local discriminant set
/// by a null return from the runtime ask path.
fn emit_remote_ask_err_from_last_error<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    result_dest: Place,
    error_dest: Place,
) -> CodegenResult<()> {
    let err_fn = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_node_ask_take_last_error",
    )?;
    let err_code = fn_ctx
        .builder
        .build_call(err_fn, &[], "hew_node_ask_take_last_error_call")
        .llvm_ctx("hew_node_ask_take_last_error call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| {
            CodegenError::FailClosed("hew_node_ask_take_last_error returned void".into())
        })?
        .into_int_value();
    let error_local = composite_dest_local(error_dest, "RemoteAsk AskError")?;
    let (error_tag_ptr, error_tag_ty) = place_pointer(fn_ctx, Place::MachineTag(error_local))?;
    let error_tag_int_ty = match error_tag_ty {
        BasicTypeEnum::IntType(t) => t,
        other => {
            return Err(CodegenError::FailClosed(format!(
                "RemoteAsk AskError tag projection must be integer, got {other:?}"
            )));
        }
    };
    let err_code = match err_code
        .get_type()
        .get_bit_width()
        .cmp(&error_tag_int_ty.get_bit_width())
    {
        std::cmp::Ordering::Equal => err_code,
        std::cmp::Ordering::Less => fn_ctx
            .builder
            .build_int_z_extend(err_code, error_tag_int_ty, "remote_ask_err_zext")
            .llvm_ctx("remote ask err zext")?,
        std::cmp::Ordering::Greater => fn_ctx
            .builder
            .build_int_truncate(err_code, error_tag_int_ty, "remote_ask_err_trunc")
            .llvm_ctx("remote ask err trunc")?,
    };
    fn_ctx
        .builder
        .build_store(error_tag_ptr, err_code)
        .llvm_ctx("store RemoteAsk AskError tag")?;
    emit_result_err(fn_ctx, result_dest, error_dest)
}

/// Emit the caller-side non-blocking cross-node `await remote.ask(...)` (NEW-5
/// `Terminator::SuspendingRemoteAsk`). The wire-reply analogue of
/// [`emit_suspending_ask_terminator`]: instead of blocking an OS worker in
/// `hew_node_api_ask`, submit the ask + register the parked continuation
/// (`hew_node_api_ask_async`), suspend (freeing the worker), and on the resume
/// edge drain the deposited reply (`hew_node_api_ask_finish`), binding
/// `Result<Reply, AskError>`.
///
/// Shape (the suspendable remote-ask ramp):
/// ```text
///   self    = hew_actor_self()                       ; the parked-cont actor
///   handle  = hew_node_api_ask_async(pid, msg_type, payload, size, timeout, self)
///   br (handle == null) ? submit_err : do_suspend
/// submit_err:                                         ; setup failed, never parked
///   bind Result::Err(hew_node_ask_take_last_error()); br resume
/// do_suspend:                                         ; park the continuation
///   coro.suspend → { default: executor, resume: reply_bind, cleanup: abandon }
/// abandon_cleanup:                                    ; parked cont destroyed
///   hew_node_api_ask_cancel(handle); br shared cleanup
/// reply_bind:                                         ; wire reply woke us
///   reply = hew_node_api_ask_finish(handle, msg_type, reply_size)
///   br (reply == null) ? err : ok
/// ok:   bind Result::Ok(reply); free(reply); br resume
/// err:  bind Result::Err(hew_node_ask_take_last_error()); br resume
/// ```
/// The reply travels through the reply routing table (the `request_id`-keyed
/// pending slot) across the suspend, NOT a terminator operand — exactly as
/// `SuspendingAsk`'s reply travels through its reply channel.
#[allow(
    clippy::too_many_lines,
    reason = "the full caller-side remote-ask ramp — submit + suspend + the \
              resume-edge reply binding — is kept in one place so the suspend \
              point and the value routing it depends on are read together"
)]
pub(crate) fn emit_suspending_remote_actor_ask_terminator<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    term: SuspendingRemoteAskEmit<'_>,
) -> CodegenResult<()> {
    // The coro prologue must be present (lower_function detects the
    // SuspendingRemoteAsk carrier via `has_suspend`). Fail closed otherwise
    // (R2 / the Lane-B silent-no-op class).
    let coro = fn_ctx.coro.ok_or_else(|| {
        CodegenError::FailClosed(
            "Terminator::SuspendingRemoteAsk reached codegen but the function carries no \
             coro prologue state — lower_function must detect the suspend carrier"
                .into(),
        )
    })?;
    let resume_bb = *fn_ctx.blocks.get(&term.resume).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "SuspendingRemoteAsk resume target bb{} not found",
            term.resume
        ))
    })?;
    if !fn_ctx.blocks.contains_key(&term.cleanup) {
        return Err(CodegenError::FailClosed(format!(
            "SuspendingRemoteAsk cleanup target bb{} not found",
            term.cleanup
        )));
    }

    let (pid_slot, pid_slot_ty) = place_pointer(fn_ctx, term.actor)?;
    if !matches!(pid_slot_ty, BasicTypeEnum::StructType(t) if t.count_fields() == 5) {
        return Err(CodegenError::FailClosed(format!(
            "SuspendingRemoteAsk actor place must be the five-field Location aggregate, got {pid_slot_ty:?}"
        )));
    }
    let (payload_ptr, payload_size) =
        actor_payload_ptr_size(fn_ctx, term.value, "remote_ask_payload")?;
    let (timeout_ptr, timeout_ty) = place_pointer(fn_ctx, term.timeout_ms)?;
    let timeout_val = fn_ctx
        .builder
        .build_load(timeout_ty, timeout_ptr, "remote_ask_timeout")
        .llvm_ctx("load SuspendingRemoteAsk timeout")?
        .into_int_value();
    if timeout_val.get_type().get_bit_width() != 64 {
        return Err(CodegenError::FailClosed(format!(
            "SuspendingRemoteAsk timeout_ms must lower to a 64-bit integer, got {} bits",
            timeout_val.get_type().get_bit_width()
        )));
    }
    let reply_size = static_type_size_i64(fn_ctx, term.reply_ty, "remote_ask_reply")?;
    // Target actor type's dispatch global — the `(dispatch, msg_type)` codec key
    // for both the request encode (ask_async) and the reply decode (ask_finish).
    let dispatch_ptr = remote_ask_dispatch_ptr(fn_ctx, term.actor)?;

    // self = the current actor — the parked-continuation waiter the wire reply
    // re-enqueues. MUST come from `hew_actor_self()` (the live thread-local
    // execution context); across a suspend the spilled `ctx` param's context is
    // freed, but the thread-local read returns the live actor on resume. The
    // same single-authority live-context accessor the SuspendingRead/Ask ramps
    // use.
    let actor_self_fn = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_actor_self",
    )?;
    let self_actor = fn_ctx
        .builder
        .build_call(actor_self_fn, &[], "suspending_remote_ask_self")
        .llvm_ctx("hew_actor_self call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_actor_self returned void".into()))?
        .into_pointer_value();

    let ask_async_fn = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_node_api_ask_async_location",
    )?;
    let msg_type = fn_ctx.ctx.i32_type().const_int(term.msg_type as u64, false);
    let pending_handle = fn_ctx
        .builder
        .build_call(
            ask_async_fn,
            &[
                pid_slot.into(),
                dispatch_ptr.into(),
                msg_type.into(),
                payload_ptr.into(),
                payload_size.into(),
                timeout_val.into(),
                self_actor.into(),
            ],
            "hew_node_api_ask_async_call",
        )
        .llvm_ctx("hew_node_api_ask_async call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_node_api_ask_async returned void".into()))?
        .into_pointer_value();

    let parent = fn_ctx
        .builder
        .get_insert_block()
        .and_then(|bb| bb.get_parent())
        .ok_or_else(|| {
            CodegenError::Llvm("suspending remote ask block has no parent function".into())
        })?;
    // ── submit_err: setup failed (null handle); no reply will ever arrive and we
    // never parked. Bind the typed Err without suspending (no worker to free).
    let submit_err_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "suspending_remote_ask_submit_err");
    let do_suspend_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "suspending_remote_ask_suspend");
    let handle_is_null = fn_ctx
        .builder
        .build_is_null(pending_handle, "suspending_remote_ask_handle_is_null")
        .llvm_ctx("suspending remote ask handle null compare")?;
    fn_ctx
        .builder
        .build_conditional_branch(handle_is_null, submit_err_bb, do_suspend_bb)
        .llvm_ctx("suspending remote ask submit branch")?;

    fn_ctx.builder.position_at_end(submit_err_bb);
    emit_remote_ask_err_from_last_error(fn_ctx, term.result_dest, term.error_dest)?;
    fn_ctx
        .builder
        .build_unconditional_branch(resume_bb)
        .llvm_ctx("suspending remote ask submit-err br")?;

    // ── do_suspend: park the continuation (non-final suspend). The default edge
    // returns the coro handle to the trampoline (which parks it on this actor);
    // case 0 resumes into the reply-bind block; case 1 tears down. ─────────────
    fn_ctx.builder.position_at_end(do_suspend_bb);
    let reply_bind_bb = fn_ctx
        .ctx
        .append_basic_block(parent, "suspending_remote_ask_reply_bind");
    // Abandon-cleanup edge: a parked SuspendingRemoteAsk continuation DESTROYED
    // without resuming (actor stop/crash while suspended, the C1 free-path
    // destroy_parked) runs `coro.suspend`'s case-1 edge. The shared cleanup only
    // frees the coro frame — it never sees `pending_handle` (a codegen-local SSA
    // value with no MIR Place), so without this interposition the pending entry
    // leaks. Route case 1 to a block that CANCELS the pending reply (removing it
    // from the routing table) before joining the shared cleanup. A racing late
    // reply then finds nothing; the late wake itself is independently fail-safe
    // (enqueue_resume drops a wake to a freed caller).
    emit_suspend_point(
        fn_ctx,
        coro,
        parent,
        reply_bind_bb,
        "suspending_remote_ask",
        "suspending_remote_ask_abandon_cleanup",
        "suspending remote ask abandon -> shared cleanup br",
        || {
            // ── abandon_cleanup: cancel the pending reply, then join the shared
            // coro cleanup (frame-free + coro.end). ─────────────────────────────
            fn_ctx.call_runtime_void(
                "hew_node_api_ask_cancel",
                &[pending_handle.into()],
                "suspending_remote_ask_cancel",
                "hew_node_api_ask_cancel call",
            )?;
            Ok(())
        },
        || {
            // ── reply_bind: the wire reply (or peer-drop / timeout failure)
            // resumed us. Drain the deposited outcome; null is the typed-failure
            // sentinel. ─────────────────────────────────────────────────────────
            let reply_ptr = fn_ctx.call_runtime_ptr(
                "hew_node_api_ask_finish",
                &[
                    pending_handle.into(),
                    dispatch_ptr.into(),
                    msg_type.into(),
                    reply_size.into(),
                ],
                "hew_node_api_ask_finish_call",
                "hew_node_api_ask_finish call",
            )?;

            let ok_bb = fn_ctx
                .ctx
                .append_basic_block(parent, "suspending_remote_ask_ok");
            let err_bb = fn_ctx
                .ctx
                .append_basic_block(parent, "suspending_remote_ask_err");
            let is_null = fn_ctx
                .builder
                .build_is_null(reply_ptr, "suspending_remote_ask_is_null")
                .llvm_ctx("suspending remote ask null compare")?;
            fn_ctx
                .builder
                .build_conditional_branch(is_null, err_bb, ok_bb)
                .llvm_ctx("suspending remote ask result branch")?;

            fn_ctx.builder.position_at_end(ok_bb);
            if matches!(term.reply_ty, ResolvedTy::Unit) {
                emit_result_ok(fn_ctx, term.result_dest, None)?;
            } else {
                let (reply_dest_ptr, reply_dest_ty) = place_pointer(fn_ctx, term.reply_dest)?;
                let reply_val = fn_ctx
                    .builder
                    .build_load(
                        reply_dest_ty,
                        reply_ptr,
                        "suspending_remote_ask_reply_value",
                    )
                    .llvm_ctx("suspending remote ask reply load")?;
                fn_ctx
                    .builder
                    .build_store(reply_dest_ptr, reply_val)
                    .llvm_ctx("suspending remote ask reply store")?;
                emit_result_ok(fn_ctx, term.result_dest, Some(term.reply_dest))?;
                let free = get_or_declare_free(fn_ctx);
                fn_ctx
                    .builder
                    .build_call(
                        free,
                        &[reply_ptr.into()],
                        "suspending_remote_ask_reply_free",
                    )
                    .llvm_ctx("free suspending remote ask reply")?;
            }
            fn_ctx
                .builder
                .build_unconditional_branch(resume_bb)
                .llvm_ctx("suspending remote ask ok br")?;

            fn_ctx.builder.position_at_end(err_bb);
            emit_remote_ask_err_from_last_error(fn_ctx, term.result_dest, term.error_dest)?;
            fn_ctx
                .builder
                .build_unconditional_branch(resume_bb)
                .llvm_ctx("suspending remote ask err br")?;
            Ok(())
        },
    )?;

    Ok(())
}

pub(crate) fn emit_remote_actor_ask_terminator<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    term: RemoteAskEmit<'_>,
) -> CodegenResult<()> {
    let (pid_slot, pid_slot_ty) = place_pointer(fn_ctx, term.actor)?;
    if !matches!(pid_slot_ty, BasicTypeEnum::StructType(t) if t.count_fields() == 5) {
        return Err(CodegenError::FailClosed(format!(
            "RemoteAsk actor place must be the five-field Location aggregate, got {pid_slot_ty:?}"
        )));
    }
    let (payload_ptr, payload_size) =
        actor_payload_ptr_size(fn_ctx, term.value, "remote_ask_payload")?;
    let (timeout_ptr, timeout_ty) = place_pointer(fn_ctx, term.timeout_ms)?;
    let timeout_val = fn_ctx
        .builder
        .build_load(timeout_ty, timeout_ptr, "remote_ask_timeout")
        .llvm_ctx("load RemoteAsk timeout")?
        .into_int_value();
    if timeout_val.get_type().get_bit_width() != 64 {
        return Err(CodegenError::FailClosed(format!(
            "RemoteAsk timeout_ms must lower to a 64-bit integer, got {} bits",
            timeout_val.get_type().get_bit_width()
        )));
    }
    let reply_size = static_type_size_i64(fn_ctx, term.reply_ty, "remote_ask_reply")?;
    // Target actor type's dispatch global — the `(dispatch, msg_type)` codec key
    // for BOTH the request encode and the reply decode on this ask.
    let dispatch_ptr = remote_ask_dispatch_ptr(fn_ctx, term.actor)?;
    let ask_fn = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_node_api_ask_location",
    )?;
    let msg_type = fn_ctx.ctx.i32_type().const_int(term.msg_type as u64, false);
    let reply_ptr = fn_ctx
        .builder
        .build_call(
            ask_fn,
            &[
                pid_slot.into(),
                dispatch_ptr.into(),
                msg_type.into(),
                payload_ptr.into(),
                payload_size.into(),
                timeout_val.into(),
                reply_size.into(),
            ],
            "hew_node_api_ask_call",
        )
        .llvm_ctx("hew_node_api_ask call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_node_api_ask returned void".into()))?
        .into_pointer_value();

    let parent = fn_ctx
        .builder
        .get_insert_block()
        .and_then(|bb| bb.get_parent())
        .ok_or_else(|| CodegenError::Llvm("remote ask block has no parent function".into()))?;
    let ok_bb = fn_ctx.ctx.append_basic_block(parent, "remote_ask_ok");
    let err_bb = fn_ctx.ctx.append_basic_block(parent, "remote_ask_err");
    let is_null = fn_ctx
        .builder
        .build_is_null(reply_ptr, "remote_ask_is_null")
        .llvm_ctx("remote ask null compare")?;
    fn_ctx
        .builder
        .build_conditional_branch(is_null, err_bb, ok_bb)
        .llvm_ctx("remote ask result branch")?;
    let next_bb = *fn_ctx.blocks.get(&term.next).ok_or_else(|| {
        CodegenError::FailClosed(format!("RemoteAsk next bb{} missing", term.next))
    })?;

    fn_ctx.builder.position_at_end(ok_bb);
    if matches!(term.reply_ty, ResolvedTy::Unit) {
        emit_result_ok(fn_ctx, term.result_dest, None)?;
    } else {
        let (reply_dest_ptr, reply_dest_ty) = place_pointer(fn_ctx, term.reply_dest)?;
        let reply_val = fn_ctx
            .builder
            .build_load(reply_dest_ty, reply_ptr, "remote_ask_reply_value")
            .llvm_ctx("remote ask reply load")?;
        fn_ctx
            .builder
            .build_store(reply_dest_ptr, reply_val)
            .llvm_ctx("remote ask reply store")?;
        emit_result_ok(fn_ctx, term.result_dest, Some(term.reply_dest))?;
        let free = get_or_declare_free(fn_ctx);
        fn_ctx
            .builder
            .build_call(free, &[reply_ptr.into()], "remote_ask_reply_free")
            .llvm_ctx("free remote ask reply")?;
    }
    fn_ctx
        .builder
        .build_unconditional_branch(next_bb)
        .llvm_ctx("remote ask ok br")?;

    fn_ctx.builder.position_at_end(err_bb);
    emit_remote_ask_err_from_last_error(fn_ctx, term.result_dest, term.error_dest)?;
    fn_ctx
        .builder
        .build_unconditional_branch(next_bb)
        .llvm_ctx("remote ask err br")?;

    Ok(())
}

// ===========================================================================
// Actor spawn + supervisor bootstrap emission (god-module carve)
// ===========================================================================
// Pure relocation from llvm.rs: `spawn`-actor lowering, supervisor bootstrap /
// child-spec registration, static-pool member emission, and the child-init
// thunk. Byte-identical IR before and after.

#[allow(clippy::too_many_arguments)]
pub(crate) fn emit_spawn_actor(
    fn_ctx: &FnCtx<'_, '_>,
    actor_name: &str,
    state: Option<Place>,
    init_args: &[Place],
    dest: Place,
    max_heap_bytes: Option<u64>,
    cycle_capable: bool,
    mailbox_capacity: Option<u32>,
    overflow_policy: Option<&hew_parser::ast::OverflowPolicy>,
) -> CodegenResult<()> {
    let ptr_ty = fn_ctx.ctx.ptr_type(AddressSpace::default());
    let i32_ty = fn_ctx.ctx.i32_type();
    let i64_ty = fn_ctx.ctx.i64_type();
    let (state_ptr, state_size) = if let Some(state_place) = state {
        let (slot, slot_ty) = place_pointer(fn_ctx, state_place)?;
        let size = slot_ty.size_of().ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "spawn `{actor_name}` state type has no statically known size: {slot_ty:?}"
            ))
        })?;
        let size = if size.get_type() == i64_ty {
            size
        } else {
            fn_ctx
                .builder
                .build_int_z_extend(size, i64_ty, "actor_state_size")
                .llvm_ctx("actor state size zext")?
        };
        (slot, size)
    } else {
        (ptr_ty.const_null(), i64_ty.const_zero())
    };
    let dispatch_name = format!("__hew_actor_dispatch_{}", mangle_dotted_name(actor_name));
    let dispatch = fn_ctx
        .llvm_mod
        .get_function(&dispatch_name)
        .ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "spawn `{actor_name}` requires dispatch trampoline `{dispatch_name}`"
            ))
        })?;
    fn_ctx.call_runtime_void(
        "hew_sched_init",
        &[],
        "hew_sched_init_call",
        "hew_sched_init call",
    )?;
    emit_wasm_actor_metadata_registration(fn_ctx, actor_name)?;
    emit_native_actor_metadata_registration(fn_ctx, actor_name, dispatch)?;

    let spawned = if max_heap_bytes.is_some() || cycle_capable || mailbox_capacity.is_some() {
        // `#[max_heap(N)]` and/or `cycle_capable` and/or a declared `mailbox`
        // clause is set — route through `hew_actor_spawn_opts` so the
        // runtime applies all spawn policy bits. The `HewActorOpts` struct
        // is stack-allocated, populated with the minimal fields, and passed
        // by pointer.
        //
        // `HewActorOpts` `#[repr(C)]` field order (hew-runtime/src/actor.rs:1456):
        //   0  init_state:       *mut c_void   → ptr
        //   1  state_size:       usize         → i64
        //   2  dispatch:         Option<fn>    → ptr
        //   3  mailbox_capacity: i32           → i32
        //   4  overflow:         i32           → i32
        //   5  coalesce_key_fn:  Option<fn>    → ptr
        //   6  coalesce_fallback: i32          → i32
        //   7  budget:           i32           → i32
        //   8  arena_cap_bytes:  usize         → i64
        //   9  cycle_capable:    i32           → i32
        let arena_cap = max_heap_bytes.unwrap_or(0);
        let cycle_flag = if cycle_capable { 1 } else { 0 };
        // `-1`/`0` = unbounded (`HewActorOpts.mailbox_capacity` doc). No
        // declared `mailbox` clause stores `0`; a declared capacity clamps
        // to `i32::MAX` on the (practically unreachable) overflow case
        // rather than wrapping into a negative/unbounded value.
        let mailbox_capacity_i32 = mailbox_capacity
            .map(|n| i32::try_from(n).unwrap_or(i32::MAX))
            .unwrap_or(0);
        let overflow_i32 = overflow_policy_to_i32(actor_name, overflow_policy)?;
        let (coalesce_key_fn, coalesce_fallback) = match overflow_policy {
            Some(hew_parser::ast::OverflowPolicy::Coalesce { fallback, .. }) => {
                let key_name = crate::thunks::coalesce_key_fn_name(actor_name);
                let key_fn = fn_ctx.llvm_mod.get_function(&key_name).ok_or_else(|| {
                    CodegenError::FailClosed(format!(
                        "spawn `{actor_name}` requires coalesce key callback `{key_name}`"
                    ))
                })?;
                (
                    key_fn.as_global_value().as_pointer_value(),
                    overflow_fallback_to_i32(fallback.as_ref()),
                )
            }
            _ => (ptr_ty.const_null(), 0),
        };
        let drop_name = crate::thunks::message_drop_fn_name(actor_name);
        let message_drop_fn = fn_ctx
            .llvm_mod
            .get_function(&drop_name)
            .ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "spawn `{actor_name}` requires message drop callback `{drop_name}`"
                ))
            })?
            .as_global_value()
            .as_pointer_value();
        let opts_ty = fn_ctx.ctx.struct_type(
            &[
                ptr_ty.into(), // init_state
                i64_ty.into(), // state_size
                ptr_ty.into(), // dispatch
                i32_ty.into(), // mailbox_capacity
                i32_ty.into(), // overflow
                ptr_ty.into(), // coalesce_key_fn
                i32_ty.into(), // coalesce_fallback
                i32_ty.into(), // budget
                i64_ty.into(), // arena_cap_bytes
                i32_ty.into(), // cycle_capable
                ptr_ty.into(), // message_drop_fn
            ],
            false,
        );
        let opts_slot = fn_ctx
            .builder
            .build_alloca(opts_ty, "actor_spawn_opts")
            .llvm_ctx("HewActorOpts alloca")?;
        let opts_fields: [(u32, BasicValueEnum<'_>); 11] = [
            (0, state_ptr.into()),
            (1, state_size.into()),
            (2, dispatch.as_global_value().as_pointer_value().into()),
            (
                3,
                i32_ty.const_int(mailbox_capacity_i32 as u64, true).into(),
            ),
            (4, i32_ty.const_int(overflow_i32 as u64, true).into()),
            (5, coalesce_key_fn.into()),
            (6, i32_ty.const_int(coalesce_fallback as u64, true).into()),
            (7, i32_ty.const_zero().into()),
            (8, i64_ty.const_int(arena_cap, false).into()),
            (9, i32_ty.const_int(cycle_flag, false).into()),
            (10, message_drop_fn.into()),
        ];
        for (field_idx, value) in opts_fields {
            let gep = fn_ctx
                .builder
                .build_struct_gep(opts_ty, opts_slot, field_idx, &format!("opts_f{field_idx}"))
                .llvm_ctx_with(|| format!("HewActorOpts GEP field {field_idx}"))?;
            fn_ctx
                .builder
                .build_store(gep, value)
                .llvm_ctx_with(|| format!("HewActorOpts store field {field_idx}"))?;
        }
        fn_ctx.call_runtime_ptr(
            "hew_actor_spawn_opts",
            &[opts_slot.into()],
            "hew_actor_spawn_opts_call",
            "hew_actor_spawn_opts call",
        )?
    } else {
        // No arena cap — use the lighter 3-arg `hew_actor_spawn` path.
        // `state_size` is built as i64; `hew_actor_spawn`'s `state_size`
        // param is `usize`/`size_t` (i32 on wasm32). Reconcile the value to
        // the target-correct width so the call matches the declaration.
        let size_ty = runtime_size_ty(fn_ctx.ctx, fn_ctx.llvm_mod);
        let spawn_state_size = reconcile_int_width_signed(
            fn_ctx,
            state_size.into(),
            size_ty.into(),
            "spawn state_size",
        )?;
        fn_ctx.call_runtime_ptr(
            "hew_actor_spawn",
            &[
                state_ptr.into(),
                spawn_state_size.into(),
                dispatch.as_global_value().as_pointer_value().into(),
            ],
            "hew_actor_spawn_call",
            "hew_actor_spawn call",
        )?
    };

    // W2.002 Stage 2: register state_clone + state_drop on the freshly
    // spawned actor before lifecycle hooks (init / on_start / terminate)
    // run. The setters consume `spawned` as a pointer arg; the runtime
    // null-tolerates a null `spawned` from OOM via `cabi_guard!` so no
    // extra null check is needed at the call site. See
    // `emit_actor_state_clone_drop_registration` for the no-op fallback
    // path (paired-absent symbols on the ActorLayout).
    let actor_layout = fn_ctx
        .actor_layouts
        .iter()
        .find(|l| l.name == actor_name)
        .ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "spawn `{actor_name}` has no ActorLayout for state clone/drop registration"
            ))
        })?;
    emit_actor_state_clone_drop_registration(fn_ctx, actor_name, spawned, actor_layout)?;
    crate::llvm::emit_actor_message_drop_registration(fn_ctx, actor_name, spawned)?;

    emit_actor_spawn_lifecycle(fn_ctx, actor_name, spawned, init_args)?;
    emit_periodic_handler_arming(fn_ctx, actor_name, spawned, actor_layout)?;
    let (dest_ptr, dest_ty) = place_pointer(fn_ctx, dest)?;
    if !matches!(dest_ty, BasicTypeEnum::PointerType(_)) {
        return Err(CodegenError::FailClosed(format!(
            "SpawnActor destination is not pointer-typed: {dest_ty:?}"
        )));
    }
    fn_ctx
        .builder
        .build_store(dest_ptr, spawned)
        .llvm_ctx("SpawnActor store")?;
    Ok(())
}

/// Arm one `hew_actor_schedule_periodic` timer per `#[every(duration)]`
/// receive handler on the freshly spawned actor.
///
/// The `msg_type` comes from the actor's `ActorHandlerLayout` row — the same
/// protocol-descriptor id the send/ask paths use (`lower_actor_handler_layouts`,
/// hew-mir/src/lower.rs) — so a periodic tick and a user send of the same
/// handler are indistinguishable in dispatch.
///
/// Fail-closed: `hew_actor_schedule_periodic` returns null when it cannot arm
/// the timer (invalid interval, or the timer-wheel ticker failed to start —
/// the runtime records the detail in its last-error slot). A null handle
/// branches to `hew_trap_with_code(HEW_TRAP_ACTOR_SEND_FAILED=206)`; the
/// periodic timer IS this handler's send path, and 206 is the established
/// "send substrate failed" discriminator (`emit_select_setup_failure_trap`
/// reuses it the same way). Never a silent continue.
///
/// Arming happens AFTER `emit_actor_spawn_lifecycle`, so `init`/`on_start`
/// have completed before the first tick can be dispatched. The returned
/// handle is discarded: cancellation is actor-lifetime-scoped
/// (`hew_actor_free` → `cancel_all_timers_for_actor`), and user-facing cancel
/// handles are out of scope for the v0.5 surface.
fn emit_periodic_handler_arming(
    fn_ctx: &FnCtx<'_, '_>,
    actor_name: &str,
    spawned: PointerValue<'_>,
    actor_layout: &ActorLayout,
) -> CodegenResult<()> {
    if actor_layout.handlers.iter().all(|h| h.every_ms.is_none()) {
        return Ok(());
    }
    let ptr_ty = fn_ctx.ctx.ptr_type(AddressSpace::default());
    let i32_ty = fn_ctx.ctx.i32_type();
    let i64_ty = fn_ctx.ctx.i64_type();
    let schedule_periodic = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_actor_schedule_periodic",
    )?;
    for handler in &actor_layout.handlers {
        let Some(interval_ms) = handler.every_ms else {
            continue;
        };
        let msg_type = i32_ty.const_int(handler.msg_type as u64, false);
        let interval = i64_ty.const_int(interval_ms, false);
        let handle = fn_ctx
            .builder
            .build_call(
                schedule_periodic,
                &[spawned.into(), msg_type.into(), interval.into()],
                &format!("periodic_arm_{}", handler.name),
            )
            .llvm_ctx("hew_actor_schedule_periodic call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| {
                CodegenError::FailClosed("hew_actor_schedule_periodic returned void".into())
            })?
            .into_pointer_value();
        let arm_failed = fn_ctx
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                handle,
                ptr_ty.const_null(),
                &format!("periodic_arm_failed_{}", handler.name),
            )
            .llvm_ctx("periodic arm null compare")?;
        let parent_fn = fn_ctx
            .builder
            .get_insert_block()
            .and_then(|bb| bb.get_parent())
            .ok_or_else(|| {
                CodegenError::Llvm(format!(
                    "spawn `{actor_name}` periodic arming block has no parent function"
                ))
            })?;
        let fail_bb = fn_ctx
            .ctx
            .append_basic_block(parent_fn, &format!("periodic_arm_fail_{}", handler.name));
        let ok_bb = fn_ctx
            .ctx
            .append_basic_block(parent_fn, &format!("periodic_arm_ok_{}", handler.name));
        fn_ctx
            .builder
            .build_conditional_branch(arm_failed, fail_bb, ok_bb)
            .llvm_ctx("periodic arm branch")?;
        fn_ctx.builder.position_at_end(fail_bb);
        emit_trap_with_code(
            fn_ctx,
            HEW_TRAP_ACTOR_SEND_FAILED as u64,
            "periodic_arm_fail",
        )?;
        fn_ctx.builder.position_at_end(ok_bb);
    }
    Ok(())
}

/// Map a `HirSupervisorStrategy` to the runtime `STRATEGY_*` integer.
///
/// Mirrors the `pub const STRATEGY_*` constants at
/// `hew-runtime/src/supervisor.rs:315-324`. The values are part of the ABI:
/// changing them requires a coordinated runtime + codegen edit.
fn supervisor_strategy_to_int(strategy: HirSupervisorStrategy) -> i32 {
    match strategy {
        HirSupervisorStrategy::OneForOne => 0,
        HirSupervisorStrategy::OneForAll => 1,
        HirSupervisorStrategy::RestForOne => 2,
        HirSupervisorStrategy::SimpleOneForOne => 3,
    }
}

/// Map a `HirRestartPolicy` to the runtime `RESTART_*` integer.
///
/// Mirrors the `pub const RESTART_*` constants at
/// `hew-runtime/src/supervisor.rs:329-331`.
fn restart_policy_to_int(policy: HirRestartPolicy) -> i32 {
    match policy {
        HirRestartPolicy::Permanent => 0,
        HirRestartPolicy::Transient => 1,
        HirRestartPolicy::Temporary => 2,
    }
}

/// Build the LLVM struct type for `HewChildSpec`.
///
/// The struct mirrors the `#[repr(C)]` Rust layout at
/// `hew-runtime/src/supervisor.rs::HewChildSpec` exactly. Field order MUST match;
/// drift here is wrong-code at the FFI boundary.
///
/// Field map (Rust → LLVM):
/// - `name: *const c_char`                  → `ptr`
/// - `init_state: *mut c_void`              → `ptr`
/// - `init_state_size: usize`               → `i64`  (64-bit spine only)
/// - `dispatch: Option<HewDispatchFn>`      → `ptr`  (opaque-pointer mode)
/// - `restart_policy: c_int`                → `i32`
/// - `mailbox_capacity: c_int`              → `i32`
/// - `overflow: c_int`                      → `i32`
/// - `coalesce_key_fn: Option<fn>`          → `ptr`
/// - `coalesce_fallback: c_int`             → `i32`
/// - `arena_cap_bytes: usize`               → `i64`
/// - `cycle_capable: c_int`                 → `i32`
/// - `on_crash: Option<HewOnCrashFn>`       → `ptr`
/// - `lifecycle_fn: Option<HewLifecycleFn>` → `ptr`
///
/// The three consecutive `i32` fields followed by `i64`, and the trailing
/// `cycle_capable: i32` followed by two pointers, produce natural padding under
/// `#[repr(C)]`; LLVM's default (non-packed) struct alignment matches it.
///
/// ABI P0: `lifecycle_fn` is the trailing field — it MUST occupy the same
/// position as `HewChildSpec.lifecycle_fn` in `hew-runtime/src/supervisor.rs`
/// (appended after `on_crash`). Field-order drift here is wrong-code at the
/// FFI boundary; the layout assertion below pins the two struct mirrors.
pub(crate) fn hew_child_spec_struct_type<'ctx>(ctx: &'ctx Context) -> StructType<'ctx> {
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i32_ty = ctx.i32_type();
    let i64_ty = ctx.i64_type();
    ctx.struct_type(
        &[
            ptr_ty.into(), // name
            ptr_ty.into(), // init_state
            i64_ty.into(), // init_state_size
            ptr_ty.into(), // dispatch
            i32_ty.into(), // restart_policy
            i32_ty.into(), // mailbox_capacity
            i32_ty.into(), // overflow
            ptr_ty.into(), // coalesce_key_fn
            i32_ty.into(), // coalesce_fallback
            i64_ty.into(), // arena_cap_bytes (alignment introduces 4B pad after `overflow`)
            i32_ty.into(), // cycle_capable
            ptr_ty.into(), // on_crash fn-pointer: null when child's actor has no #[on(crash)]; otherwise pointer to `{actor_name}__on_crash`
            ptr_ty.into(), // lifecycle_fn fn-pointer: null when child's actor has no init/#[on(start)]; otherwise pointer to `__hew_lifecycle_{actor_name}`
            // ── v0.6 init-closure restart model trailing fields ──────────────
            // These mirror the trailing `HewChildSpec` fields added in the
            // runtime (`hew-runtime/src/supervisor.rs`): `init_fn` / `config` /
            // `config_size`. Field-order drift here is wrong-code at the FFI
            // boundary — the runtime reads these by offset. Until the init-thunk
            // codegen lands, the literal stores null/null/0 so the runtime takes
            // the existing template path (init_fn == null).
            ptr_ty.into(), // init_fn: per-child init thunk (null on the template path)
            ptr_ty.into(), // config: borrowed supervisor config buffer (null when no init_fn)
            i64_ty.into(), // config_size: bytes of `config` (0 when null)
            ptr_ty.into(), // message_drop_fn
        ],
        false,
    )
}

/// Emit the body of a supervisor bootstrap function.
///
/// The bootstrap symbol is declared by `declare_function` like any other
/// `FunctionCallConv::ActorHandler` function (one leading
/// `*mut HewExecutionContext` param the body ignores). This helper substitutes
/// the MIR-side synthesised body wholesale with the canonical
/// `hew_supervisor_*` call sequence:
///
/// 1. `%sup = call hew_supervisor_new(strategy, max_restarts, window_secs)`
/// 2. for each child: alloca `HewChildSpec`, populate fields, call
///    `hew_supervisor_add_child_spec(%sup, &spec)`
/// 3. `%rc = call hew_supervisor_start(%sup)`; trap on non-zero (fail-closed)
/// 4. `ret %sup`
///
/// Fail-closed posture:
/// - Missing per-child dispatch trampoline → `FailClosed` (must be emitted by
///   `emit_actor_dispatch_trampoline` ahead of this helper).
/// - Non-integer `window` literal → `FailClosed`. The fixture uses
///   `window: 60`; the `"60s"` form is deferred to a follow-on slice.
/// - `hew_supervisor_start` non-zero return → `llvm.trap; unreachable`. The
///   supervisor surface is one of Hew's fail-closed boundaries (LESSONS
///   boundary-fail-closed); a start that the runtime rejected is wrong-code
///   territory, not a recoverable error.
#[allow(
    clippy::too_many_arguments,
    reason = "the bootstrap emitter threads the same module-wide tables as the \
              MIR lowering path plus the config buffer it materialises"
)]
pub(crate) fn emit_supervisor_bootstrap_body<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    target_data: &TargetData,
    layout: &SupervisorLayout,
    fn_symbols: &FnSymbolMap<'ctx>,
    actor_layouts: &[ActorLayout],
    record_layouts: &RecordLayoutMap<'ctx>,
    mir_record_layouts: &[RecordLayout],
) -> CodegenResult<()> {
    let symbol = *fn_symbols.get(&layout.bootstrap_symbol).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "supervisor `{}` bootstrap symbol `{}` was not declared before body emission",
            layout.name, layout.bootstrap_symbol
        ))
    })?;
    let (llvm_fn, return_ty_llvm, _returns_unit) =
        symbol.real(&layout.bootstrap_symbol, "emit_supervisor_bootstrap_body")?;
    if !matches!(return_ty_llvm, BasicTypeEnum::PointerType(_)) {
        return Err(CodegenError::FailClosed(format!(
            "supervisor `{}` bootstrap return type must be a pointer (LocalPid<Sup>); got {return_ty_llvm:?}",
            layout.name
        )));
    }

    let builder = ctx.create_builder();
    let entry_bb = ctx.append_basic_block(llvm_fn, "entry");
    builder.position_at_end(entry_bb);

    let i32_ty = ctx.i32_type();
    let i64_ty = ctx.i64_type();
    let ptr_ty = ctx.ptr_type(AddressSpace::default());

    // ── Strategy / max_restarts / window literals ───────────────────────
    //
    // `None` falls back to `0`; the runtime treats `0` for max_restarts as
    // "no automatic restarts" and `0` for window_secs as "policy clamps to a
    // safe minimum". The fixture sets explicit values so the runtime path
    // exercises real numbers.
    let strategy_int = layout.strategy.map(supervisor_strategy_to_int).unwrap_or(0);
    let max_restarts_int: i32 = layout
        .max_restarts
        .and_then(|n| i32::try_from(n).ok())
        .unwrap_or(0);
    // Window parsing: the restart-budget window arrives as the raw duration
    // source string from the `intensity: N within <duration>` field (e.g.
    // `"60s"`, `"5m"`). Duration unit interpretation is centralised in the
    // lexer/parser, so convert via `hew_parser::parse_duration_ns` rather than
    // reimplementing the unit math here. A bare integer is also accepted as a
    // count of seconds for internally-constructed layouts (e.g. tests). Any
    // unparseable string fails closed — silently coercing to 0 would hide a
    // parser/MIR drift bug.
    let window_secs_int: i32 = match layout.window.as_deref() {
        None => 0,
        Some(s) => {
            let s = s.trim();
            let secs: i64 = if let Some(nanos) = hew_parser::parse_duration_ns(s) {
                nanos / 1_000_000_000
            } else if let Ok(n) = s.parse::<i64>() {
                n
            } else {
                return Err(CodegenError::FailClosed(format!(
                    "supervisor `{}` window literal `{s:?}` is not a duration literal \
                     (e.g. `60s`, `5m`) or an integer-seconds value",
                    layout.name
                )));
            };
            i32::try_from(secs).map_err(|_| {
                CodegenError::FailClosed(format!(
                    "supervisor `{}` window `{s:?}` ({secs}s) does not fit in i32 seconds",
                    layout.name
                ))
            })?
        }
    };

    let mut runtime_decls = RuntimeDeclMap::new();

    // ── hew_sched_init() ────────────────────────────────────────────────────
    // The plain-actor spawn path calls hew_sched_init before hew_actor_spawn
    // (llvm.rs `emit_spawn_actor`).  The supervisor bootstrap must do the same
    // before hew_supervisor_new, or the runtime panics "scheduler not
    // initialized" (exit 134).
    let sched_init = intern_runtime_decl(ctx, llvm_mod, &mut runtime_decls, "hew_sched_init")?;
    builder
        .build_call(sched_init, &[], "hew_sched_init_call")
        .llvm_ctx("hew_sched_init call in supervisor bootstrap")?;

    // ── %sup = call hew_supervisor_new(strategy, max_restarts, window_secs)
    let sup_new = intern_runtime_decl(ctx, llvm_mod, &mut runtime_decls, "hew_supervisor_new")?;
    let sup = builder
        .build_call(
            sup_new,
            &[
                i32_ty.const_int(strategy_int as u64, true).into(),
                i32_ty.const_int(max_restarts_int as u64, true).into(),
                i32_ty.const_int(window_secs_int as u64, true).into(),
            ],
            "hew_supervisor_new_call",
        )
        .llvm_ctx("hew_supervisor_new call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_supervisor_new returned void".into()))?
        .into_pointer_value();

    // ── Construction-time config buffer (v0.6 init-closure restart model) ──
    //
    // When the supervisor declares a config param (`supervisor App(config: T)`),
    // the bootstrap fn's param 0 is the config struct passed by value. Spill it
    // to a fresh malloc'd, supervisor-owned buffer here so every per-child init
    // thunk can read `config.field` (a BORROW) on the initial spawn and on every
    // restart. The buffer is adopted ONCE by the runtime (the first
    // `set_child_init_fn` / `add_child_spec` carrying it) and freed ONCE at
    // supervisor teardown (`stop_supervisor_owned`). The thunk never frees it.
    //
    // `None` → a no-config supervisor; no buffer, every child takes the literal
    // template path (init_fn null), unchanged from C1-C3.
    //
    // GUARD: only materialise the buffer when at least one child actually reads a
    // config field (has a ConfigField init arg). A config supervisor whose children
    // ALL use literal-only init args never calls set_child_init_fn, so no child
    // adopts the buffer — malloc-without-adopt leaks sizeof(config) per supervisor
    // start. When no ConfigField is present, skip the buffer entirely and let all
    // children take the literal-template path.
    let any_child_reads_config_field = layout.children.iter().any(|child| {
        child
            .init_state_fields
            .iter()
            .any(|(_, arg)| matches!(arg, ChildInitArg::ConfigField { .. }))
    });
    let config_buf: Option<(PointerValue<'ctx>, IntValue<'ctx>)> = match &layout.config_param {
        Some(_) if !any_child_reads_config_field => {
            // Config param declared but no child reads it — skip the buffer.
            // All children take the literal-template path (init_fn null).
            None
        }
        Some(config_param) => {
            // Resolve the config struct's LLVM type for the size + spill.
            let config_struct_ty = *record_layouts
                .get(config_param.config_ty_name.as_str())
                .ok_or_else(|| {
                    CodegenError::FailClosed(format!(
                        "supervisor `{}` config param type `{}` has no record layout; the \
                         config struct must be registered before supervisor bootstrap emission",
                        layout.name, config_param.config_ty_name
                    ))
                })?;

            // Param 0 is the config struct value (FunctionCallConv::Default →
            // no leading ctx param). Spill it to a stack slot so we can take its
            // address for the buffer memcpy.
            let config_param_val = llvm_fn.get_nth_param(0).ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "supervisor `{}` bootstrap declares a config param but the LLVM fn has \
                     no parameter 0",
                    layout.name
                ))
            })?;
            let config_stack = builder
                .build_alloca(config_struct_ty, "config_param_spill")
                .llvm_ctx("config param spill alloca")?;
            builder
                .build_store(config_stack, config_param_val)
                .llvm_ctx("config param spill store")?;

            // Buffer size = sizeof(config struct).
            let config_size_val = config_struct_ty.size_of().ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "supervisor `{}` config struct `{}` has no statically known size",
                    layout.name, config_param.config_ty_name
                ))
            })?;
            let config_size_i64 = if config_size_val.get_type() == i64_ty {
                config_size_val
            } else {
                builder
                    .build_int_z_extend(config_size_val, i64_ty, "config_size_i64")
                    .llvm_ctx("config size zext")?
            };

            // Malloc the supervisor-owned buffer (libc — ALLOCATOR-PAIRING with
            // the teardown `libc::free` at supervisor.rs stop_supervisor_owned).
            let malloc_fn = get_or_declare_libc_malloc(ctx, llvm_mod);
            let malloc_size_ty = runtime_size_ty(ctx, llvm_mod);
            let malloc_size = if malloc_size_ty == i64_ty {
                config_size_i64
            } else {
                builder
                    .build_int_truncate(config_size_i64, malloc_size_ty, "config_size_trunc")
                    .llvm_ctx("config size trunc")?
            };
            let buf_ptr = builder
                .build_call(malloc_fn, &[malloc_size.into()], "config_buf_malloc")
                .llvm_ctx("config buffer malloc")?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| {
                    CodegenError::FailClosed("malloc returned void — signature mismatch".into())
                })?
                .into_pointer_value();

            // memcpy the config value into the buffer. The config struct is
            // BitCopy at the registration boundary (the per-field owned clone,
            // when present, happens INSIDE the thunk from this buffer — the
            // buffer itself is a flat snapshot). Use the struct's ABI alignment.
            let config_align = host_target_data().get_abi_alignment(&config_struct_ty);
            builder
                .build_memcpy(
                    buf_ptr,
                    config_align,
                    config_stack,
                    config_align,
                    config_size_i64,
                )
                .llvm_ctx("config buffer memcpy")?;

            // Register the config struct's drop glue so teardown releases the
            // buffer's OWNED inner fields (`string`/`bytes`/…) before the flat
            // free. The buffer OWNS those fields (the thunks only CLONE from
            // them); without this they leak at teardown. Only when the config
            // struct has an owned field — an all-scalar config has nothing to
            // drop. The `__hew_record_drop_inplace_<ConfigTy>` body is seeded
            // for synthesis via `collect_supervisor_config_drop_seeds`.
            let config_has_owned = mir_record_layouts
                .iter()
                .find(|r| r.name == config_param.config_ty_name)
                .is_some_and(|r| {
                    r.field_tys.iter().any(|ty| {
                        let mut visited = std::collections::HashSet::new();
                        hew_mir::classify_state_field(ty, mir_record_layouts, &mut visited)
                            .is_ok_and(|kind| !matches!(kind, StateFieldCloneKind::BitCopy { .. }))
                    })
                });
            if config_has_owned {
                let drop_fn =
                    get_or_declare_record_drop_inplace(ctx, llvm_mod, &config_param.config_ty_name);
                let set_config_drop = intern_runtime_decl(
                    ctx,
                    llvm_mod,
                    &mut runtime_decls,
                    "hew_supervisor_set_config_drop_fn",
                )?;
                builder
                    .build_call(
                        set_config_drop,
                        &[
                            sup.into(),
                            drop_fn.as_global_value().as_pointer_value().into(),
                        ],
                        "hew_supervisor_set_config_drop_fn_call",
                    )
                    .llvm_ctx("hew_supervisor_set_config_drop_fn call")?;
            }

            Some((buf_ptr, config_size_i64))
        }
        None => None,
    };

    // ── For each child: alloca HewChildSpec, populate, register ─────────
    let child_spec_ty = hew_child_spec_struct_type(ctx);
    let add_child_spec = intern_runtime_decl(
        ctx,
        llvm_mod,
        &mut runtime_decls,
        "hew_supervisor_add_child_spec",
    )?;
    // W2.002 Stage 2: per-child state_clone + state_drop setters. Interned
    // once outside the loop so all children share the same FunctionValue.
    let set_child_state_drop = intern_runtime_decl(
        ctx,
        llvm_mod,
        &mut runtime_decls,
        "hew_supervisor_set_child_state_drop",
    )?;
    let set_child_state_clone = intern_runtime_decl(
        ctx,
        llvm_mod,
        &mut runtime_decls,
        "hew_supervisor_set_child_state_clone",
    )?;
    // Lifecycle setter (parity/back-fill). The load-bearing initial-spawn fire
    // reads the literal-carried lifecycle_fn inside add_child_spec; this setter
    // only stores the pointer and never re-fires (see the runtime setter doc).
    let set_child_lifecycle = intern_runtime_decl(
        ctx,
        llvm_mod,
        &mut runtime_decls,
        "hew_supervisor_set_child_lifecycle",
    )?;
    // Per-child init-thunk setter (v0.6 init-closure restart model). Adopts the
    // supervisor-owned config buffer once + installs the thunk; the literal
    // carrier already installs it for the initial spawn, so this is back-fill /
    // symmetry with the other per-child setters. Interned once outside the loop.
    let set_child_init_fn = intern_runtime_decl(
        ctx,
        llvm_mod,
        &mut runtime_decls,
        "hew_supervisor_set_child_init_fn",
    )?;
    // A nested-supervisor child (`child api: AuthSupervisor;`) registers through
    // a different runtime seam than an actor child: call the child supervisor's
    // own bootstrap function (which builds and starts the child supervisor and
    // returns its `*mut HewSupervisor`), then register it with the parent via
    // `hew_supervisor_add_child_supervisor_with_init`, passing the same
    // bootstrap as the restart init_fn. Interned once outside the loop so all
    // nested children share the same FunctionValue.
    let add_child_supervisor_with_init = intern_runtime_decl(
        ctx,
        llvm_mod,
        &mut runtime_decls,
        "hew_supervisor_add_child_supervisor_with_init",
    )?;
    // Static-pool ABI: `pool_add_slot` reserves the pool slot once, and
    // `pool_member_add_static` binds each spawned member's static-child index
    // into it. Interned once outside the loop so all pool members share the
    // same FunctionValue.
    let pool_add_slot = intern_runtime_decl(
        ctx,
        llvm_mod,
        &mut runtime_decls,
        "hew_supervisor_pool_add_slot",
    )?;
    let pool_member_add_static = intern_runtime_decl(
        ctx,
        llvm_mod,
        &mut runtime_decls,
        "hew_supervisor_pool_member_add_static",
    )?;
    // Actor children and nested supervisors are registered into two separate
    // runtime tables, each 0-based. `hew_supervisor_add_child_spec` pushes actor
    // children sequentially into `children[]`, so an actor child's runtime index
    // is its position among ACTOR children only — NOT the combined loop index.
    // The per-child setters (`set_child_state_drop/clone/lifecycle`) key by that
    // runtime index, so passing the combined loop index would target the wrong
    // slot whenever a nested supervisor precedes an actor child. Track an
    // actor-only counter that advances only for actor children, mirroring the
    // accessor's `partitioned_static_slot_index`.
    //
    // Pool members ARE static children: each member registers into children[]
    // via add_child_spec and so advances `actor_child_index` (the
    // `simple_one_for_one` checker guarantees a pool supervisor has NO static
    // `child` declarations, so the pool's members are the only static slots and
    // a post-pool static accessor cannot mis-index). The `is_pool` skip below
    // applies to the pool DECLARATION's no-op default; the members it spawns DO
    // occupy static slots.
    let mut actor_child_index = 0usize;
    for child in &layout.children {
        // Pool children (`pool name: Type(count: N)`) reserve a slot in the
        // runtime's disjoint `pool_slots[]` space, then spawn N fungible members
        // as static children (registered into `children[]` via add_child_spec)
        // and bind each member's static index into the pool via
        // `pool_member_add_static`. The accessor resolves a member through its
        // LIVE static slot, so a restarted member re-resolves automatically.
        //
        // A literal count unrolls into N member registrations at compile time; a
        // config-derived count is COMPILE-TIME rejected (CodegenError::FailClosed)
        // until the dynamic 0..N bootstrap loop lands. Each member advances
        // `actor_child_index`.
        if child.is_pool {
            actor_child_index = emit_static_pool_members(
                ctx,
                llvm_mod,
                &builder,
                &child_spec_ty,
                sup,
                add_child_spec,
                set_child_state_drop,
                set_child_state_clone,
                set_child_lifecycle,
                set_child_init_fn,
                pool_add_slot,
                pool_member_add_static,
                actor_child_index,
                child,
                &layout.name,
                layout.config_param.as_ref(),
                config_buf,
                actor_layouts,
                record_layouts,
                mir_record_layouts,
                target_data,
            )?;
            continue;
        }
        if let Some(nested_bootstrap) = &child.nested_bootstrap_symbol {
            debug_assert!(
                !child.occupies_static_child_slot(),
                "codegen bootstrap: nested-supervisor child `{}` must not occupy a \
                 static child slot",
                child.name
            );
            emit_nested_supervisor_register(
                llvm_mod,
                &builder,
                sup,
                add_child_supervisor_with_init,
                nested_bootstrap,
                child,
                &layout.name,
            )?;
        } else {
            // A plain actor child: occupies the static slot at `actor_child_index`
            // and advances it. The shared helper agrees with the accessor side.
            debug_assert!(
                child.occupies_static_child_slot(),
                "codegen bootstrap: actor child `{}` must occupy a static child slot \
                 — diverged from the accessor's partitioned index",
                child.name
            );
            emit_supervisor_child_spec_and_register(
                ctx,
                llvm_mod,
                &builder,
                &child_spec_ty,
                sup,
                add_child_spec,
                set_child_state_drop,
                set_child_state_clone,
                set_child_lifecycle,
                set_child_init_fn,
                actor_child_index,
                child,
                &layout.name,
                layout.config_param.as_ref(),
                config_buf,
                actor_layouts,
                record_layouts,
                mir_record_layouts,
                target_data,
            )?;
            actor_child_index += 1;
        }
    }

    // ── %rc = call hew_supervisor_start(%sup); trap on non-zero ─────────
    let sup_start = intern_runtime_decl(ctx, llvm_mod, &mut runtime_decls, "hew_supervisor_start")?;
    let rc = builder
        .build_call(sup_start, &[sup.into()], "hew_supervisor_start_call")
        .llvm_ctx("hew_supervisor_start call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_supervisor_start returned void".into()))?
        .into_int_value();
    let zero = i32_ty.const_zero();
    let is_ok = builder
        .build_int_compare(IntPredicate::EQ, rc, zero, "sup_start_ok")
        .llvm_ctx("sup start cmp")?;
    let ok_bb = ctx.append_basic_block(llvm_fn, "sup_start_ok");
    let trap_bb = ctx.append_basic_block(llvm_fn, "sup_start_trap");
    builder
        .build_conditional_branch(is_ok, ok_bb, trap_bb)
        .llvm_ctx("sup start cond br")?;
    builder.position_at_end(trap_bb);
    // llvm.trap; unreachable — fail-closed on supervisor start failure.
    let trap_intrinsic = Intrinsic::find("llvm.trap").ok_or_else(|| {
        CodegenError::FailClosed("llvm.trap intrinsic not available in this LLVM build".into())
    })?;
    let trap_fn = trap_intrinsic
        .get_declaration(llvm_mod, &[])
        .ok_or_else(|| CodegenError::FailClosed("llvm.trap declaration missing".into()))?;
    builder
        .build_call(trap_fn, &[], "sup_start_trap_call")
        .llvm_ctx("sup start trap call")?;
    builder
        .build_unreachable()
        .llvm_ctx("sup start unreachable")?;

    // ── ret sup ─────────────────────────────────────────────────────────
    builder.position_at_end(ok_bb);
    let _ = ptr_ty;
    let _ = i64_ty;
    builder
        .build_return(Some(&sup))
        .llvm_ctx("sup bootstrap ret")?;
    Ok(())
}

/// Register a nested-supervisor child with its parent.
///
/// Unlike an actor child (which lowers to a `HewChildSpec` + dispatch
/// trampoline), a nested supervisor is brought up by its own bootstrap
/// function — the same function `spawn ChildSupervisor` would call. That
/// bootstrap builds the child supervisor, registers its children, starts it,
/// and returns its `*mut HewSupervisor`. We then attach it to the parent via
/// `hew_supervisor_add_child_supervisor_with_init`, passing the same bootstrap
/// as the restart `init_fn` so the parent can rebuild the subtree on
/// escalation.
///
/// The child's bootstrap is a real MIR function declared ahead of supervisor
/// body emission; fail closed if it is missing — a null child supervisor would
/// SIGSEGV at the first nested accessor.
fn emit_nested_supervisor_register<'ctx>(
    llvm_mod: &LlvmModule<'ctx>,
    builder: &Builder<'ctx>,
    sup: PointerValue<'ctx>,
    add_child_supervisor_with_init: FunctionValue<'ctx>,
    nested_bootstrap: &str,
    child: &SupervisorChildLayout,
    sup_name: &str,
) -> CodegenResult<()> {
    let bootstrap_fn = llvm_mod.get_function(nested_bootstrap).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "supervisor `{sup_name}` nested child `{}` requires the child supervisor \
             bootstrap function `{nested_bootstrap}`, which was not declared before \
             body emission",
            child.name
        ))
    })?;

    // %child_sup = call <nested_bootstrap>()
    let child_sup = builder
        .build_call(
            bootstrap_fn,
            &[],
            &format!("nested_sup_{}_bootstrap", child.name),
        )
        .llvm_ctx("nested supervisor bootstrap call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "nested supervisor `{}` bootstrap `{nested_bootstrap}` returned void; \
                 expected a *mut HewSupervisor",
                child.name
            ))
        })?
        .into_pointer_value();

    // call hew_supervisor_add_child_supervisor_with_init(%parent, %child_sup,
    //                                                    <nested_bootstrap>)
    // The bootstrap doubles as the restart init_fn (() -> *mut HewSupervisor).
    let init_fn_ptr = bootstrap_fn.as_global_value().as_pointer_value();
    builder
        .build_call(
            add_child_supervisor_with_init,
            &[sup.into(), child_sup.into(), init_fn_ptr.into()],
            &format!("nested_sup_{}_register", child.name),
        )
        .llvm_ctx("hew_supervisor_add_child_supervisor_with_init call")?;

    Ok(())
}

/// Register a supervised child actor's type name and per-handler names into the
/// runtime profiler registry so a crash inside one of its handlers is reported
/// by name (`Worker::run`) rather than the bare `msg_type` discriminant.
///
/// The direct-`spawn` path emits the equivalent registration via
/// `emit_native_actor_metadata_registration`. A supervised child, however, is
/// spawned by the runtime supervisor from its child spec and never passes
/// through a codegen `spawn` site, so without this call its `(dispatch, msg_type)
/// → name` rows are never seeded and the crash reporter
/// (`hew-runtime/src/signal.rs`) has nothing to resolve. Registration is
/// idempotent in the runtime (first-write-wins), so a child that is also spawned
/// directly elsewhere still registers exactly once.
///
/// `dispatch_ptr` is the child actor TYPE's `__hew_actor_dispatch_<name>`
/// trampoline — the same pointer the runtime stores into `HewActor.dispatch` and
/// the crash reporter reads back, so the registry key matches at lookup time.
/// The runtime entry points are no-ops when the `profiler` feature is disabled,
/// matching the direct-spawn path's behaviour.
fn emit_supervised_child_handler_name_registration<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    builder: &Builder<'ctx>,
    actor_name: &str,
    dispatch_ptr: PointerValue<'ctx>,
    actor_layouts: &[ActorLayout],
    name_prefix: &str,
) -> CodegenResult<()> {
    let Some(layout) = actor_layouts.iter().find(|l| l.name == actor_name) else {
        // No native ActorLayout for this child means the actor type was not
        // classified for native dispatch; the spawn itself fails closed
        // upstream, so there is nothing to register here.
        return Ok(());
    };

    let void_ty = ctx.void_type();
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i32_ty = ctx.i32_type();
    let actor_fragment = llvm_global_name_fragment(actor_name);

    // hew_actor_register_type(dispatch: ptr, name: ptr) — same canonical
    // signature as `intern_runtime_decl`; `declare_codec_prim` reuses an
    // existing declaration so this never conflicts with the spawn-path decl.
    let register_type = declare_codec_prim(
        ctx,
        llvm_mod,
        "hew_actor_register_type",
        void_ty.fn_type(&[ptr_ty.into(), ptr_ty.into()], false),
    );
    let type_name_global = builder
        .build_global_string_ptr(
            actor_name,
            &format!("str_sup_actor_type_{name_prefix}_{actor_fragment}"),
        )
        .llvm_ctx("supervised actor type name global")?;
    builder
        .build_call(
            register_type,
            &[
                dispatch_ptr.into(),
                type_name_global.as_pointer_value().into(),
            ],
            "hew_actor_register_type_call",
        )
        .llvm_ctx("hew_actor_register_type call")?;

    if layout.handlers.is_empty() {
        return Ok(());
    }

    // hew_register_handler_name(dispatch: ptr, msg_type: i32, name: ptr).
    let register_handler = declare_codec_prim(
        ctx,
        llvm_mod,
        "hew_register_handler_name",
        void_ty.fn_type(&[ptr_ty.into(), i32_ty.into(), ptr_ty.into()], false),
    );
    for (h_idx, handler) in layout.handlers.iter().enumerate() {
        let handler_name = format!("{actor_name}::{}", handler.name);
        let handler_fragment = llvm_global_name_fragment(&handler.name);
        let handler_name_global = builder
            .build_global_string_ptr(
                &handler_name,
                &format!("str_sup_handler_{name_prefix}_{h_idx}_{handler_fragment}"),
            )
            .llvm_ctx("supervised handler name global")?;
        builder
            .build_call(
                register_handler,
                &[
                    dispatch_ptr.into(),
                    i32_ty.const_int(handler.msg_type as u64, true).into(),
                    handler_name_global.as_pointer_value().into(),
                ],
                "hew_register_handler_name_call",
            )
            .llvm_ctx("hew_register_handler_name call")?;
    }
    Ok(())
}

/// Emit one `HewChildSpec` literal + `hew_supervisor_add_child_spec` call.
///
/// Per-field semantics:
/// - `name` — global C-string `@.str.child.<sup>.<idx>` set to `child.name`
///   (the slot name, not the actor type; matches the surface name visible
///   to user-level `sup.<name>` access).
/// - `init_state` / `init_state_size` — `null` / `0`. The fixture has no
///   per-child init args; richer init lowering is deferred.
/// - `dispatch` — pointer to `__hew_actor_dispatch_<actor_name>`, declared
///   by `emit_actor_dispatch_trampoline` in `build_module` ahead of this
///   helper. Fail-closed if missing.
/// - `restart_policy` — child's policy via `restart_policy_to_int`; `None`
///   defaults to `RESTART_PERMANENT` (`0`) to match runtime defaults.
/// - `mailbox_capacity` / `overflow` / coalesce callback + fallback — mirrored
///   from the child actor layout so initial spawn and restart are identical.
/// - `arena_cap_bytes` / `cycle_capable` — mirrored from the child's
///   `ActorLayout` through `SupervisorChildLayout` so restarts preserve the
///   same spawn policy bits as direct spawn.
#[allow(clippy::too_many_arguments)]
fn emit_supervisor_child_spec_and_register<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    builder: &Builder<'ctx>,
    child_spec_ty: &StructType<'ctx>,
    sup: PointerValue<'ctx>,
    add_child_spec: FunctionValue<'ctx>,
    set_child_state_drop: FunctionValue<'ctx>,
    set_child_state_clone: FunctionValue<'ctx>,
    set_child_lifecycle: FunctionValue<'ctx>,
    set_child_init_fn: FunctionValue<'ctx>,
    idx: usize,
    child: &SupervisorChildLayout,
    sup_name: &str,
    config_param: Option<&hew_mir::SupervisorConfigParam>,
    config_buf: Option<(PointerValue<'ctx>, IntValue<'ctx>)>,
    actor_layouts: &[ActorLayout],
    record_layouts: &RecordLayoutMap<'ctx>,
    mir_record_layouts: &[RecordLayout],
    target_data: &TargetData,
) -> CodegenResult<()> {
    let i32_ty = ctx.i32_type();
    let i64_ty = ctx.i64_type();
    let ptr_ty = ctx.ptr_type(AddressSpace::default());

    let dispatch_name = format!(
        "__hew_actor_dispatch_{}",
        mangle_dotted_name(&child.actor_name)
    );
    let dispatch_fn = llvm_mod.get_function(&dispatch_name).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "supervisor `{sup_name}` child `{}` requires dispatch trampoline `{dispatch_name}`",
            child.name
        ))
    })?;

    // Register the child actor's type name and per-handler names into the
    // runtime profiler registry. A supervised child is spawned by the runtime
    // supervisor from this spec and never passes through a codegen `spawn` site
    // (where `emit_native_actor_metadata_registration` would do this), so
    // without this its handler-name registry stays empty and a crash inside one
    // of its handlers is reported by the bare `msg_type` discriminant instead of
    // the handler name (e.g. `Worker::run`).
    emit_supervised_child_handler_name_registration(
        ctx,
        llvm_mod,
        builder,
        &child.actor_name,
        dispatch_fn.as_global_value().as_pointer_value(),
        actor_layouts,
        &format!("{sup_name}_{idx}"),
    )?;

    // Resolve on_crash fn-pointer: Some(symbol) → pointer to the declared
    // function; None → null. The on_crash function is produced by MIR
    // lowering (as `{actor_name}__on_crash`) and declared by `declare_function`
    // over `raw_mir` before `emit_supervisor_bootstrap_body` runs, so
    // `get_function` must find it if the symbol is populated.
    //
    // Fail-closed: a missing symbol means Slice 2 didn't produce the
    // function body, which is wrong-code territory — diagnose clearly rather
    // than silently falling back to null.
    let on_crash_ptr: BasicValueEnum<'ctx> = match &child.on_crash_symbol {
        Some(symbol) => {
            let on_crash_fn = llvm_mod.get_function(symbol).ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "supervisor `{sup_name}` child `{}` has on_crash_symbol `{symbol}` \
                     but no function with that name was declared in the module; \
                     the MIR lowering pass must emit the on_crash function before codegen",
                    child.name
                ))
            })?;
            on_crash_fn.as_global_value().as_pointer_value().into()
        }
        None => ptr_ty.const_null().into(),
    };

    // Resolve the lifecycle wrapper fn-pointer. `Some(symbol)` (the MIR layout
    // marked this child's actor as init/on_start-bearing) → emit the per-actor
    // wrapper and take its address; `None` → null (the actor has no lifecycle
    // hook, so the runtime fires nothing). Fail-closed when the symbol is
    // present but the wrapper cannot be emitted — a null here would silently
    // skip init/on_start under supervision (the exact bug this change fixes).
    let lifecycle_ptr: BasicValueEnum<'ctx> = match &child.lifecycle_symbol {
        Some(_) => {
            let wrapper = emit_actor_lifecycle_wrapper(ctx, llvm_mod, &child.actor_name)?;
            wrapper.as_global_value().as_pointer_value().into()
        }
        None => ptr_ty.const_null().into(),
    };

    // Per-child stack alloca for the spec struct.
    let spec_slot = builder
        .build_alloca(*child_spec_ty, &format!("child_spec_{idx}"))
        .llvm_ctx("child spec alloca")?;

    // name → @.str.child.<sup>.<idx>
    let name_global = builder
        .build_global_string_ptr(&child.name, &format!("str_child_name_{sup_name}_{idx}"))
        .llvm_ctx("child name global")?;
    let name_ptr = name_global.as_pointer_value();

    let restart_int = child.restart_policy.map(restart_policy_to_int).unwrap_or(0);
    // arena_cap_bytes: lifted from the child actor's `#[max_heap(N)]`
    // annotation (mirrored into SupervisorChildLayout.max_heap_bytes by the
    // MIR post-loop pass). Zero means unbounded — matches runtime default.
    let arena_cap = child.max_heap_bytes.unwrap_or(0);
    let cycle_flag = if child.cycle_capable { 1 } else { 0 };
    // mailbox_capacity / overflow: mirrored from ActorLayout by the MIR
    // post-loop pass. Without this, every supervised actor stays unbounded
    // across (re)spawns regardless of its declared `mailbox` clause — the
    // second of the two codegen zero-hardcode sites closed by this fix.
    // `-1`/`0` = unbounded, matching `HewActorOpts.mailbox_capacity`.
    let mailbox_capacity_i32 = child
        .mailbox_capacity
        .map(|n| i32::try_from(n).unwrap_or(i32::MAX))
        .unwrap_or(0);
    let overflow_i32 = overflow_policy_to_i32(&child.actor_name, child.overflow_policy.as_ref())?;
    let (coalesce_key_fn, coalesce_fallback) = match child.overflow_policy.as_ref() {
        Some(hew_parser::ast::OverflowPolicy::Coalesce { fallback, .. }) => {
            let key_name = crate::thunks::coalesce_key_fn_name(&child.actor_name);
            let key_fn = llvm_mod.get_function(&key_name).ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "supervised child `{}` requires coalesce key callback `{key_name}`",
                    child.actor_name
                ))
            })?;
            (
                key_fn.as_global_value().as_pointer_value(),
                overflow_fallback_to_i32(fallback.as_ref()),
            )
        }
        _ => (ptr_ty.const_null(), 0),
    };
    let drop_name = crate::thunks::message_drop_fn_name(&child.actor_name);
    let message_drop_fn = llvm_mod
        .get_function(&drop_name)
        .ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "supervised child `{}` requires message drop callback `{drop_name}`",
                child.actor_name
            ))
        })?
        .as_global_value()
        .as_pointer_value();

    // ── Init-closure (config) path vs literal-template path ─────────────
    //
    // A child whose init args read `config.field` (any `ConfigField` arg) is
    // produced by a per-child init thunk that re-runs the config-field reads on
    // the initial spawn AND every restart — there is NO captured byte-copy
    // template (the template path replays the SAME bytes, which would alias
    // owned config-derived fields). A literal-only child keeps the template
    // path (init_fn null). Mixed literal + config args are produced wholly by
    // the thunk (it materialises the literals too), so the presence of ANY
    // config-field arg selects the thunk path.
    let has_config_field = child
        .init_state_fields
        .iter()
        .any(|(_, arg)| matches!(arg, ChildInitArg::ConfigField { .. }));

    // For the thunk path, the init thunk produces the state; the spec's
    // init_state template is null/0 (the runtime ignores it when init_fn is
    // set). For the literal path, build the const template below.
    let init_fn_ptr: BasicValueEnum<'ctx>;
    let config_ptr_for_spec: BasicValueEnum<'ctx>;
    let config_size_for_spec: BasicValueEnum<'ctx>;
    let mut owned_init_state_template: Option<PointerValue<'ctx>> = None;

    let (init_state_ptr, init_state_size): (BasicValueEnum<'ctx>, BasicValueEnum<'ctx>) =
        if has_config_field {
            // ── Init-closure thunk path ─────────────────────────────────────
            let config_param = config_param.ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "supervisor `{sup_name}` child `{}` reads config fields but the supervisor \
                 layout carries no config param; MIR should have rejected this",
                    child.name
                ))
            })?;
            let (cfg_buf, cfg_size) = config_buf.ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "supervisor `{sup_name}` child `{}` reads config fields but no config buffer \
                 was materialised in the bootstrap; codegen ordering bug",
                    child.name
                ))
            })?;
            // Emit the per-child init thunk and take its address.
            let thunk = emit_child_init_thunk(
                ctx,
                llvm_mod,
                sup_name,
                child,
                config_param,
                actor_layouts,
                record_layouts,
                mir_record_layouts,
                target_data,
            )?;
            init_fn_ptr = thunk.as_global_value().as_pointer_value().into();
            config_ptr_for_spec = cfg_buf.into();
            config_size_for_spec = cfg_size.into();
            // Thunk path: no const template — the runtime ignores init_state when
            // init_fn is set, and capturing one would re-introduce the aliasing
            // hazard the thunk model fixes.
            (ptr_ty.const_null().into(), i64_ty.const_zero().into())
        } else {
            init_fn_ptr = ptr_ty.const_null().into();
            config_ptr_for_spec = ptr_ty.const_null().into();
            config_size_for_spec = i64_ty.const_zero().into();

            // ── State template: build init_state / init_state_size ──────────────
            //
            // Fail-closed invariant: if the child actor has non-empty state fields
            // but init_state_fields is empty, we must NOT emit a null template —
            // that causes a NULL state pointer dereference at first field access.
            //
            // Three cases:
            //   A. Stateless actor (state_field_names empty) → null/zero is correct.
            //   B. Stateful actor + init_state_fields populated → build template.
            //   C. Stateful actor + init_state_fields empty → CodegenError::FailClosed.
            // Resolve the actor layout for state_field_names check.
            let child_actor_layout_for_state = actor_layouts
                .iter()
                .find(|l| l.name == child.actor_name)
                .ok_or_else(|| {
                    CodegenError::FailClosed(format!(
                        "supervisor `{sup_name}` child `{}` actor `{}` has no ActorLayout for \
                     state field check",
                        child.name, child.actor_name
                    ))
                })?;

            if child_actor_layout_for_state.state_field_names.is_empty() {
                // Case A: stateless actor — null/zero is correct.
                (ptr_ty.const_null().into(), i64_ty.const_zero().into())
            } else if child.init_state_fields.is_empty() {
                // Case C: stateful actor but no init args — hard fail-closed.
                return Err(CodegenError::FailClosed(format!(
                    "supervisor `{sup_name}` child `{}`: actor `{}` has non-empty state fields \
                 ({:?}) but SupervisorChildLayout.init_state_fields is empty — a null \
                 init_state template would cause a SIGSEGV at first field access; build \
                 the state template or reject at parse/HIR",
                    child.name, child.actor_name, child_actor_layout_for_state.state_field_names
                )));
            } else {
                // Case B: stateful actor with init args — build the state template.
                //
                // SHIM: first slice supports POD (i64/i32/bool/f64) state only.
                // WHY: owned-heap fields (String/Vec) require verified semantic clone
                //   via `state_clone_fn`; byte-copy is only correct for plain-old-data.
                // WHEN obsolete: follow-up slice after clone verification is proven.
                // WHAT: extend ChildInitArg and verify `state_clone_fn` covers owned types.
                let state_struct_ty = record_layouts.get(&child.actor_name).ok_or_else(|| {
                    CodegenError::FailClosed(format!(
                        "supervisor `{sup_name}` child `{}` actor `{}` state struct type \
                     not found in record_layouts; the actor state must be registered \
                     before supervisor bootstrap emission",
                        child.name, child.actor_name
                    ))
                })?;

                // Alloca a stack slot for the state struct, fill it field by field.
                let state_slot = builder
                    .build_alloca(*state_struct_ty, &format!("child_state_template_{idx}"))
                    .llvm_ctx("child state template alloca")?;

                for (field_name, init_arg) in &child.init_state_fields {
                    let field_idx = child_actor_layout_for_state
                        .state_field_names
                        .iter()
                        .position(|n| n == field_name)
                        .ok_or_else(|| {
                            CodegenError::FailClosed(format!(
                                "supervisor `{sup_name}` child `{}` init arg `{field_name}` not \
                             found in actor `{}` state_field_names; MIR validation should \
                             have caught this",
                                child.name, child.actor_name
                            ))
                        })?;

                    // Materialise each init arg at the field's exact width. LLVM
                    // integer types are sign-agnostic — the const's bit pattern
                    // carries the value — so the unsigned variants share the same
                    // `iN` type as their signed siblings; `build_store` then writes
                    // exactly N/8 bytes into the field slot. The signed widths
                    // sign-extend the literal into the const (`true`); the unsigned
                    // widths zero-extend (`false`), so a value that uses the high
                    // bit (e.g. `u8` 200) keeps its bit pattern.
                    let field_val: BasicValueEnum<'ctx> = match init_arg {
                        ChildInitArg::I8(n) => ctx.i8_type().const_int(*n as u64, true).into(),
                        ChildInitArg::I16(n) => ctx.i16_type().const_int(*n as u64, true).into(),
                        ChildInitArg::I32(n) => i32_ty.const_int(*n as u64, true).into(),
                        ChildInitArg::I64(n) => i64_ty.const_int(*n as u64, true).into(),
                        ChildInitArg::U8(n) => ctx.i8_type().const_int(u64::from(*n), false).into(),
                        ChildInitArg::U16(n) => {
                            ctx.i16_type().const_int(u64::from(*n), false).into()
                        }
                        ChildInitArg::U32(n) => i32_ty.const_int(u64::from(*n), false).into(),
                        ChildInitArg::U64(n) => i64_ty.const_int(*n, false).into(),
                        ChildInitArg::Bool(b) => ctx
                            .bool_type()
                            .const_int(if *b { 1 } else { 0 }, false)
                            .into(),
                        ChildInitArg::F64(f) => ctx.f64_type().const_float(*f).into(),
                        // A `ConfigField` init arg routes the WHOLE child through the
                        // init thunk path (selected by `has_config_field` above), so
                        // this const-template loop never sees one. If it does, the
                        // branch selection diverged — fail closed rather than emit a
                        // garbage const for a config-derived field.
                        ChildInitArg::ConfigField {
                            config_param_name,
                            field_name: cfg_field,
                            ..
                        } => {
                            return Err(CodegenError::FailClosed(format!(
                                "supervisor `{sup_name}` child `{}`: a config-field init arg \
                             (`{config_param_name}.{cfg_field}`) reached the literal-template \
                             loop; a config child must take the init-thunk path \
                             (has_config_field branch) — codegen branch-selection bug",
                                child.name
                            )));
                        }
                    };

                    let field_gep = builder
                        .build_struct_gep(
                            *state_struct_ty,
                            state_slot,
                            u32::try_from(field_idx).expect("field index fits u32"),
                            &format!("child_state_{idx}_field_{field_name}"),
                        )
                        .llvm_ctx("child state template field gep")?;
                    builder
                        .build_store(field_gep, field_val)
                        .llvm_ctx("child state template field store")?;
                }

                // Get the size of the state struct.
                let state_size_val = state_struct_ty.size_of().ok_or_else(|| {
                    CodegenError::FailClosed(format!(
                        "supervisor `{sup_name}` child `{}` actor `{}` state struct has no \
                     statically known size",
                        child.name, child.actor_name
                    ))
                })?;
                let state_size_i64 = if state_size_val.get_type() == i64_ty {
                    state_size_val
                } else {
                    builder
                        .build_int_z_extend(state_size_val, i64_ty, "child_state_size")
                        .llvm_ctx("child state size zext")?
                };

                // Malloc a heap buffer and memcpy the stack template into it.
                // `hew_supervisor_add_child_spec` synchronously deep-copies this
                // caller-owned template. We retain ownership of the original and
                // free it immediately after the call; the runtime owns only its
                // independent copy, which InternalChildSpec::drop later releases.
                //
                // WHY malloc rather than alloca: the runtime's add_child_spec
                // does a synchronous memcpy before returning, so passing a stack
                // pointer would be safe. However, heap allocation makes ownership
                // explicit and avoids any LLVM lifetime reasoning across the C call.
                let malloc_fn = get_or_declare_libc_malloc(ctx, llvm_mod);
                // `malloc`'s size param is `size_t` (i32 on wasm32, i64 native).
                // Supervisors are HIR-gated off wasm32 so this path is unreachable
                // there, but keep the call width-correct: truncate i64 → size_ty
                // when the target is narrower. On native, size_ty == i64 → no-op.
                let malloc_size_ty = runtime_size_ty(ctx, llvm_mod);
                let child_state_size = if malloc_size_ty == i64_ty {
                    state_size_i64
                } else {
                    builder
                        .build_int_truncate(
                            state_size_i64,
                            malloc_size_ty,
                            "child_state_size_trunc",
                        )
                        .llvm_ctx("child state size trunc")?
                };
                let heap_ptr = builder
                    .build_call(
                        malloc_fn,
                        &[child_state_size.into()],
                        &format!("child_state_heap_{idx}"),
                    )
                    .llvm_ctx("child state malloc")?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| {
                        CodegenError::FailClosed(
                            "malloc returned void — signature mismatch".to_string(),
                        )
                    })?
                    .into_pointer_value();

                // Derive the ABI alignment of the state struct from the host
                // target data so the memcpy instruction carries the real
                // alignment rather than the conservative align=1. Supervisors
                // are HIR-gated off wasm32, so `host_target_data` is always
                // the right authority here. Over-claiming alignment is UB;
                // `get_abi_alignment` returns the alignment LLVM guarantees
                // for this struct on the ABI boundary, which is exact.
                let state_align = host_target_data().get_abi_alignment(state_struct_ty);
                builder
                    .build_memcpy(
                        heap_ptr,
                        state_align,
                        state_slot,
                        state_align,
                        state_size_i64,
                    )
                    .llvm_ctx("child state template memcpy")?;

                owned_init_state_template = Some(heap_ptr);
                (heap_ptr.into(), state_size_i64.into())
            }
        };

    let field_values: [(u32, BasicValueEnum<'ctx>); 17] = [
        (0, name_ptr.into()),
        (1, init_state_ptr),
        (2, init_state_size),
        (3, dispatch_fn.as_global_value().as_pointer_value().into()),
        (4, i32_ty.const_int(restart_int as u64, true).into()),
        (
            5,
            i32_ty.const_int(mailbox_capacity_i32 as u64, true).into(),
        ), // mailbox_capacity from declared `mailbox N;`
        (6, i32_ty.const_int(overflow_i32 as u64, true).into()), // overflow: mapped by name from declared `overflow <policy>;`
        (7, coalesce_key_fn.into()),
        (8, i32_ty.const_int(coalesce_fallback as u64, true).into()),
        (9, i64_ty.const_int(arena_cap, false).into()), // arena_cap_bytes from #[max_heap(N)]
        (10, i32_ty.const_int(cycle_flag, false).into()), // cycle_capable from checker side-table
        (11, on_crash_ptr), // on_crash: fn-pointer when child's actor has #[on(crash)], null otherwise
        (12, lifecycle_ptr), // lifecycle_fn: __hew_lifecycle_<actor> when child's actor has init/#[on(start)], null otherwise
        // v0.6 init-closure restart model trailing fields. For a config-init
        // child these carry the per-child init thunk + the borrowed,
        // supervisor-owned config buffer + its size; the literal carrier IS the
        // load-bearing path for the INITIAL spawn (add_child_spec reads init_fn
        // from the literal before any post-hoc setter runs), exactly like
        // lifecycle_fn. A literal-only child leaves init_fn null (the runtime
        // then ignores config / config_size and takes the template path).
        (13, init_fn_ptr), // init_fn: per-child thunk (null on template path)
        (14, config_ptr_for_spec), // config: borrowed supervisor config buffer (null when no thunk)
        (15, config_size_for_spec), // config_size: buffer size (0 when no thunk)
        (16, message_drop_fn.into()),
    ];
    for (field_idx, value) in field_values {
        let gep = builder
            .build_struct_gep(
                *child_spec_ty,
                spec_slot,
                field_idx,
                &format!("child_spec_{idx}_f{field_idx}"),
            )
            .llvm_ctx("child spec gep")?;
        builder
            .build_store(gep, value)
            .llvm_ctx("child spec store")?;
    }

    builder
        .build_call(
            add_child_spec,
            &[sup.into(), spec_slot.into()],
            &format!("hew_supervisor_add_child_spec_call_{idx}"),
        )
        .llvm_ctx("hew_supervisor_add_child_spec call")?;

    if let Some(template) = owned_init_state_template {
        let free_fn = get_or_declare_libc_free(ctx, llvm_mod);
        builder
            .build_call(
                free_fn,
                &[template.into()],
                &format!("free_child_state_template_{idx}"),
            )
            .llvm_ctx("free child state template")?;
    }

    // Register the init thunk + config buffer for parity / back-fill symmetry
    // with the other per-child setters. The literal carrier above already
    // installed init_fn for the INITIAL spawn (add_child_spec reads it); this
    // setter adopts the supervisor-owned config buffer once and re-installs the
    // thunk so an out-of-tree C caller that installs post-hoc is self-consistent
    // (the runtime adopt is idempotent on the same pointer). Emitted only for a
    // config-init child (init_fn non-null).
    if has_config_field {
        let (cfg_buf, cfg_size) = config_buf.ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "supervisor `{sup_name}` child `{}` is a config-init child but no config \
                 buffer is available at the setter site — codegen ordering bug",
                child.name
            ))
        })?;
        let child_idx_const = i32_ty.const_int(idx as u64, true);
        builder
            .build_call(
                set_child_init_fn,
                &[
                    sup.into(),
                    child_idx_const.into(),
                    init_fn_ptr.into_pointer_value().into(),
                    cfg_buf.into(),
                    cfg_size.into(),
                ],
                &format!("hew_supervisor_set_child_init_fn_call_{idx}"),
            )
            .llvm_ctx("hew_supervisor_set_child_init_fn call")?;
    }

    // Register the lifecycle wrapper for parity/back-fill symmetry with the
    // state setters. The INITIAL-spawn lifecycle fire already ran inside the
    // add_child_spec call above (reading the literal-carried lifecycle_fn); the
    // setter only stores the pointer and does NOT re-fire (see the runtime
    // setter doc). Emitted only when the child's actor is hook-bearing.
    if child.lifecycle_symbol.is_some() {
        let child_idx_const = i32_ty.const_int(idx as u64, true);
        builder
            .build_call(
                set_child_lifecycle,
                &[
                    sup.into(),
                    child_idx_const.into(),
                    lifecycle_ptr.into_pointer_value().into(),
                ],
                &format!("hew_supervisor_set_child_lifecycle_call_{idx}"),
            )
            .llvm_ctx("hew_supervisor_set_child_lifecycle call")?;
    }

    // ── W2.002 Stage 2: register state_drop + state_clone fn pointers
    // against this child slot. Mirrors `__hew_actor_dispatch_<actor>`
    // resolution above: `child.actor_name` → per-actor synthesised symbol
    // (Stage 3 defines bodies; Stage 2 declares externs so
    // a missing Stage 3 fails clearly at link time, not silently).
    //
    // The matching `ActorLayout` carries the symbol pair on
    // `state_clone_fn_symbol` / `state_drop_fn_symbol` (paired by Stage 1
    // — see `hew-mir/src/model.rs:328-350`). Absent symbols on the layout
    // mean Stage 1 declined classification for that actor; in that case
    // the runtime fall-through (`state_clone_fn=NULL` blocks restart per
    // `hew-runtime/src/actor.rs:766`) is the safe default and no setter
    // calls are emitted.
    let child_actor_layout = actor_layouts
        .iter()
        .find(|l| l.name == child.actor_name)
        .ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "supervisor `{sup_name}` child `{}` references actor `{}` which has no \
                 ActorLayout for state clone/drop registration",
                child.name, child.actor_name
            ))
        })?;
    if let Some((clone_sym, drop_sym)) =
        resolve_state_clone_drop_symbols(&child.actor_name, child_actor_layout)?
    {
        let clone_fn_ptr = lookup_or_declare_state_clone_fn(ctx, llvm_mod, clone_sym);
        let drop_fn_ptr = lookup_or_declare_state_drop_fn(ctx, llvm_mod, drop_sym);
        let child_idx_const = i32_ty.const_int(idx as u64, true);
        builder
            .build_call(
                set_child_state_drop,
                &[sup.into(), child_idx_const.into(), drop_fn_ptr.into()],
                &format!("hew_supervisor_set_child_state_drop_call_{idx}"),
            )
            .llvm_ctx("hew_supervisor_set_child_state_drop call")?;
        builder
            .build_call(
                set_child_state_clone,
                &[sup.into(), child_idx_const.into(), clone_fn_ptr.into()],
                &format!("hew_supervisor_set_child_state_clone_call_{idx}"),
            )
            .llvm_ctx("hew_supervisor_set_child_state_clone call")?;
    }
    Ok(())
}

/// Emit the bootstrap registration for a static pool (`pool name: Type(count:
/// N)`): reserve the pool slot, spawn N fungible members as static children, and
/// bind each member's static index into the pool via `pool_member_add_static`.
///
/// Members are homogeneous (A204): one shared init template / init thunk, the
/// same actor, the same setters — only the per-member static index differs.
/// A LITERAL count unrolls into N compile-time member registrations (reusing
/// `emit_supervisor_child_spec_and_register`); a CONFIG-derived count loads the
/// size from the config buffer and emits a real `0..N` runtime loop that traps
/// fail-closed on `N <= 0`.
///
/// Returns the advanced `actor_child_index` (base + N) so subsequent static
/// children — none, for a `simple_one_for_one` pool, but the contract holds —
/// register at the correct slot.
#[allow(clippy::too_many_arguments)]
fn emit_static_pool_members<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    builder: &Builder<'ctx>,
    child_spec_ty: &StructType<'ctx>,
    sup: PointerValue<'ctx>,
    add_child_spec: FunctionValue<'ctx>,
    set_child_state_drop: FunctionValue<'ctx>,
    set_child_state_clone: FunctionValue<'ctx>,
    set_child_lifecycle: FunctionValue<'ctx>,
    set_child_init_fn: FunctionValue<'ctx>,
    pool_add_slot: FunctionValue<'ctx>,
    pool_member_add_static: FunctionValue<'ctx>,
    base_child_index: usize,
    child: &SupervisorChildLayout,
    sup_name: &str,
    config_param: Option<&hew_mir::SupervisorConfigParam>,
    config_buf: Option<(PointerValue<'ctx>, IntValue<'ctx>)>,
    actor_layouts: &[ActorLayout],
    record_layouts: &RecordLayoutMap<'ctx>,
    mir_record_layouts: &[RecordLayout],
    target_data: &TargetData,
) -> CodegenResult<usize> {
    let i32_ty = ctx.i32_type();
    let i64_ty = ctx.i64_type();
    let size_ty = runtime_size_ty(ctx, llvm_mod); // `size_t`/`usize` width; supervisors are HIR-gated off wasm32, so i64 on native and correct on 32-bit x86.

    let pool_count = child.pool_count.as_ref().ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "supervisor `{sup_name}` pool child `{}` carries no pool_count; the checker \
             requires a `count:` arg and MIR lowers it — a missing count here is a \
             lowering bug",
            child.name
        ))
    })?;

    // ── Reserve the pool slot ────────────────────────────────────────────────
    // name → @.str.pool.<sup>.<child>; strategy ROUND_ROBIN (0); max_members 0
    // (unbounded, so a later dynamic-add can grow past N). The returned pool_key
    // indexes pool_slots[]; for `simple_one_for_one` there is exactly one pool,
    // so it is 0, but we thread the runtime return rather than assuming.
    let name_global = builder
        .build_global_string_ptr(
            &child.name,
            &format!("str_pool_name_{sup_name}_{}", child.name),
        )
        .llvm_ctx("pool name global")?;
    let pool_key = builder
        .build_call(
            pool_add_slot,
            &[
                sup.into(),
                name_global.as_pointer_value().into(),
                i32_ty.const_zero().into(),  // ROUND_ROBIN
                size_ty.const_zero().into(), // max_members = 0 (unbounded)
            ],
            &format!("pool_add_slot_{}", child.name),
        )
        .llvm_ctx("hew_supervisor_pool_add_slot call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("pool_add_slot returned void".into()))?
        .into_int_value();

    match pool_count {
        PoolCount::Literal(n) => {
            // The checker proved n > 0; unroll into n compile-time registrations.
            let n = usize::try_from(*n).map_err(|_| {
                CodegenError::FailClosed(format!(
                    "supervisor `{sup_name}` pool child `{}` has a negative literal count; the \
                     checker must have rejected this",
                    child.name
                ))
            })?;
            let mut idx = base_child_index;
            for _ in 0..n {
                emit_supervisor_child_spec_and_register(
                    ctx,
                    llvm_mod,
                    builder,
                    child_spec_ty,
                    sup,
                    add_child_spec,
                    set_child_state_drop,
                    set_child_state_clone,
                    set_child_lifecycle,
                    set_child_init_fn,
                    idx,
                    child,
                    sup_name,
                    config_param,
                    config_buf,
                    actor_layouts,
                    record_layouts,
                    mir_record_layouts,
                    target_data,
                )?;
                // Bind this member's static index into the pool. `idx as u64`
                // widens usize → u64 (the const_int arg); the runtime takes the
                // low 32 bits as the u32 static index — a child count never
                // exceeds u32, so this is exact.
                let member_idx = i32_ty.const_int(idx as u64, false);
                builder
                    .build_call(
                        pool_member_add_static,
                        &[sup.into(), pool_key.into(), member_idx.into()],
                        &format!("pool_member_add_static_{}_{idx}", child.name),
                    )
                    .llvm_ctx("hew_supervisor_pool_member_add_static call")?;
                idx += 1;
            }
            Ok(idx)
        }
        PoolCount::ConfigField { field_name, .. } => {
            // SHIM: a config-derived pool count (`count: config.workers`) needs a
            // runtime `0..N` loop that registers homogeneous members at a runtime
            // static index — a from-scratch loop emission in the bootstrap, with
            // per-member runtime-index setters.
            // WHY a fail-closed gate: the literal-count path (the proving gate and
            //   the common case) is fully wired; the dynamic loop is a contained
            //   follow-up that rides the same `pool_member_add_static` ABU.
            // WHEN obsolete: when the dynamic-count loop lands (alongside the
            //   dynamic pool surface, `pool.spawn`/`pool.remove`).
            // WHAT: build the member spec once, load N from the config buffer,
            //   trap on N <= 0, then loop add_child_spec + setters(base+i) +
            //   pool_member_add_static(pool_key, base+i).
            //
            // Fail closed (not a silent skip): a program with a dynamic pool
            // count refuses to compile with a clear diagnostic rather than
            // emitting an empty pool.
            let _ = pool_key;
            let _ = (size_ty, i64_ty);
            Err(CodegenError::FailClosed(format!(
                "supervisor `{sup_name}` pool child `{}` uses a config-derived `count: \
                 config.{field_name}`; a dynamic (non-literal) pool size is not yet emitted. \
                 Use a literal `count: N` for now.",
                child.name
            )))
        }
    }
}

/// Build the LLVM struct type for `HewChildInitResult` (`{ state: ptr, size:
/// i64 }`), the by-value return of a per-child init thunk.
///
/// MUST match `hew-runtime/src/supervisor.rs::HewChildInitResult` field order
/// (`state`, then `size`). The `#[repr(C)]` two-field {ptr, usize} returns in
/// two registers on the native ABIs supervisors target (supervisors are
/// HIR-gated off wasm32). Field-order drift here is wrong-code at the FFI
/// boundary, so this mirror is pinned by `hew_child_init_result_struct_matches_runtime_abi`.
pub(crate) fn hew_child_init_result_struct_type(ctx: &Context) -> StructType<'_> {
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i64_ty = ctx.i64_type();
    ctx.struct_type(&[ptr_ty.into(), i64_ty.into()], false)
}

/// Emit a per-child init thunk `__hew_child_init_<sup>_<child>` matching the
/// `HewChildInitFn` ABI (`unsafe extern "C" fn(config: *const c_void) ->
/// HewChildInitResult`).
///
/// The thunk produces a FRESH, fully-owned actor-state wrapper on every call
/// (initial spawn AND every restart): it mallocs the actor state struct, then
/// for each init arg either loads a scalar from the config buffer
/// (`config.field`), deep-clones an owned config field (S3), or materialises a
/// literal constant, storing each into the fresh state. The config pointer is a
/// BORROW (supervisor-owned, freed once at teardown); the thunk only READS it.
/// Returns `{ null, 0 }` on malloc OOM (fail-closed — the restart path then
/// applies backoff and leaves the slot null).
#[allow(clippy::too_many_arguments)]
fn emit_child_init_thunk<'ctx>(
    ctx: &'ctx Context,
    llvm_mod: &LlvmModule<'ctx>,
    sup_name: &str,
    child: &SupervisorChildLayout,
    config_param: &hew_mir::SupervisorConfigParam,
    actor_layouts: &[ActorLayout],
    record_layouts: &RecordLayoutMap<'ctx>,
    mir_record_layouts: &[RecordLayout],
    target_data: &TargetData,
) -> CodegenResult<FunctionValue<'ctx>> {
    let thunk_name = format!(
        "__hew_child_init_{}_{}",
        mangle_dotted_name(sup_name),
        mangle_dotted_name(&child.name)
    );
    if let Some(existing) = llvm_mod.get_function(&thunk_name) {
        if existing.count_basic_blocks() > 0 {
            return Ok(existing);
        }
    }

    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i64_ty = ctx.i64_type();
    let i32_ty = ctx.i32_type();

    let result_ty = hew_child_init_result_struct_type(ctx);
    let thunk_ty = result_ty.fn_type(&[ptr_ty.into()], false);
    let thunk = llvm_mod
        .get_function(&thunk_name)
        .unwrap_or_else(|| llvm_mod.add_function(&thunk_name, thunk_ty, Some(Linkage::Internal)));

    // Resolve the actor state struct type + its field-name order.
    let actor_state_ty = *record_layouts
        .get(child.actor_name.as_str())
        .ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "init thunk for supervisor `{sup_name}` child `{}`: actor `{}` state struct type \
             not found in record_layouts",
                child.name, child.actor_name
            ))
        })?;
    let child_actor_layout = actor_layouts
        .iter()
        .find(|l| l.name == child.actor_name)
        .ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "init thunk for supervisor `{sup_name}` child `{}`: actor `{}` has no ActorLayout",
                child.name, child.actor_name
            ))
        })?;

    // Resolve the config struct type + field order (for the GEP per ConfigField).
    let config_struct_ty = *record_layouts
        .get(config_param.config_ty_name.as_str())
        .ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "init thunk for supervisor `{sup_name}` child `{}`: config struct `{}` not found \
                 in record_layouts",
                child.name, config_param.config_ty_name
            ))
        })?;
    let config_mir_layout = mir_record_layouts
        .iter()
        .find(|r| r.name == config_param.config_ty_name)
        .ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "init thunk for supervisor `{sup_name}` child `{}`: config struct `{}` has no MIR \
                 record layout for field-index resolution",
                child.name, config_param.config_ty_name
            ))
        })?;

    let builder = ctx.create_builder();
    let entry = ctx.append_basic_block(thunk, "entry");
    let oom_bb = ctx.append_basic_block(thunk, "oom");
    let build_bb = ctx.append_basic_block(thunk, "build");
    builder.position_at_end(entry);

    let config_ptr = thunk
        .get_nth_param(0)
        .ok_or_else(|| {
            CodegenError::FailClosed(format!("init thunk `{thunk_name}` missing config param"))
        })?
        .into_pointer_value();

    // Malloc the fresh actor state wrapper (libc — ALLOCATOR-PAIRING with the
    // runtime's hew_actor_free / state_drop_fn).
    let state_size_val = actor_state_ty.size_of().ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "init thunk for child `{}`: actor state struct has no statically known size",
            child.name
        ))
    })?;
    let state_size_i64 = if state_size_val.get_type() == i64_ty {
        state_size_val
    } else {
        builder
            .build_int_z_extend(state_size_val, i64_ty, "state_size_i64")
            .llvm_ctx("init thunk state size zext")?
    };
    let malloc_fn = get_or_declare_libc_malloc(ctx, llvm_mod);
    let malloc_size_ty = runtime_size_ty(ctx, llvm_mod);
    let malloc_size = if malloc_size_ty == i64_ty {
        state_size_i64
    } else {
        builder
            .build_int_truncate(state_size_i64, malloc_size_ty, "state_size_trunc")
            .llvm_ctx("init thunk state size trunc")?
    };
    let state_ptr = builder
        .build_call(malloc_fn, &[malloc_size.into()], "init_thunk_state_malloc")
        .llvm_ctx("init thunk state malloc")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("malloc returned void".into()))?
        .into_pointer_value();
    let is_null = builder
        .build_is_null(state_ptr, "state_is_null")
        .llvm_ctx("init thunk null cmp")?;
    builder
        .build_conditional_branch(is_null, oom_bb, build_bb)
        .llvm_ctx("init thunk oom branch")?;

    // ── OOM: return { null, 0 } (fail closed) ───────────────────────────
    builder.position_at_end(oom_bb);
    let oom_result = result_ty.const_zero();
    builder
        .build_return(Some(&oom_result))
        .llvm_ctx("init thunk oom return")?;

    // ── Build: populate each init field from config / literal ───────────
    builder.position_at_end(build_bb);
    for (field_name, init_arg) in &child.init_state_fields {
        // Resolve the destination field index in the actor state struct.
        let dst_idx = child_actor_layout
            .state_field_names
            .iter()
            .position(|n| n == field_name)
            .ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "init thunk for child `{}`: init arg `{field_name}` not found in actor `{}` \
                     state_field_names",
                    child.name, child.actor_name
                ))
            })?;
        let dst_gep = builder
            .build_struct_gep(
                actor_state_ty,
                state_ptr,
                u32::try_from(dst_idx).expect("field index fits u32"),
                &format!("init_thunk_dst_{field_name}"),
            )
            .llvm_ctx("init thunk dst gep")?;

        match init_arg {
            ChildInitArg::ConfigField {
                config_ty_name,
                field_name: cfg_field,
                field_ty,
                owned,
                ..
            } => {
                // Defence-in-depth: the per-field config struct name must match
                // the supervisor's single config param type.
                if config_ty_name != &config_param.config_ty_name {
                    return Err(CodegenError::FailClosed(format!(
                        "init thunk for child `{}`: config field `{cfg_field}` names config type \
                         `{config_ty_name}` but the supervisor config param is `{}`",
                        child.name, config_param.config_ty_name
                    )));
                }
                // GEP the config field by its index in the config struct.
                let cfg_idx = config_mir_layout
                    .field_names
                    .iter()
                    .position(|n| n == cfg_field)
                    .ok_or_else(|| {
                        CodegenError::FailClosed(format!(
                            "init thunk for child `{}`: config field `{cfg_field}` not found in \
                             config struct `{config_ty_name}` field names",
                            child.name
                        ))
                    })?;
                let cfg_gep = builder
                    .build_struct_gep(
                        config_struct_ty,
                        config_ptr,
                        u32::try_from(cfg_idx).expect("config field index fits u32"),
                        &format!("init_thunk_cfg_{cfg_field}"),
                    )
                    .llvm_ctx("init thunk config gep")?;

                if *owned {
                    // S3 owned per-field clone-in-thunk.
                    emit_owned_config_field_clone(
                        ctx,
                        llvm_mod,
                        &builder,
                        child,
                        field_ty,
                        mir_record_layouts,
                        cfg_gep,
                        dst_gep,
                    )?;
                } else {
                    // Scalar config field: load the value at its resolved width
                    // and store it into the fresh state field.
                    let scalar_llvm = resolve_ty(ctx, target_data, field_ty, record_layouts)?;
                    let loaded = builder
                        .build_load(
                            scalar_llvm,
                            cfg_gep,
                            &format!("init_thunk_load_{cfg_field}"),
                        )
                        .llvm_ctx("init thunk config scalar load")?;
                    builder
                        .build_store(dst_gep, loaded)
                        .llvm_ctx("init thunk scalar store")?;
                }
            }
            // Literal init args: materialise the const at the field's width.
            literal => {
                let field_val: BasicValueEnum<'ctx> = match literal {
                    ChildInitArg::I8(n) => ctx.i8_type().const_int(*n as u64, true).into(),
                    ChildInitArg::I16(n) => ctx.i16_type().const_int(*n as u64, true).into(),
                    ChildInitArg::I32(n) => i32_ty.const_int(*n as u64, true).into(),
                    ChildInitArg::I64(n) => i64_ty.const_int(*n as u64, true).into(),
                    ChildInitArg::U8(n) => ctx.i8_type().const_int(u64::from(*n), false).into(),
                    ChildInitArg::U16(n) => ctx.i16_type().const_int(u64::from(*n), false).into(),
                    ChildInitArg::U32(n) => i32_ty.const_int(u64::from(*n), false).into(),
                    ChildInitArg::U64(n) => i64_ty.const_int(*n, false).into(),
                    ChildInitArg::Bool(b) => ctx.bool_type().const_int(u64::from(*b), false).into(),
                    ChildInitArg::F64(f) => ctx.f64_type().const_float(*f).into(),
                    ChildInitArg::ConfigField { .. } => unreachable!(
                        "ConfigField handled in the arm above; this branch is the literal set"
                    ),
                };
                builder
                    .build_store(dst_gep, field_val)
                    .llvm_ctx("init thunk literal store")?;
            }
        }
    }

    // Return { state_ptr, state_size }.
    let mut result = result_ty.get_undef();
    result = builder
        .build_insert_value(result, state_ptr, 0, "init_result_state")
        .llvm_ctx("init thunk result insert state")?
        .into_struct_value();
    result = builder
        .build_insert_value(result, state_size_i64, 1, "init_result_size")
        .llvm_ctx("init thunk result insert size")?
        .into_struct_value();
    builder
        .build_return(Some(&result))
        .llvm_ctx("init thunk return")?;

    Ok(thunk)
}

// ===========================================================================
// `Terminator::Select` / `Join` emission (god-module carve)
// ===========================================================================
// Pure relocation from llvm.rs: the select arm-setup / winner-dispatch, the
// suspending select ramp, the join terminator, and their stream-callback and
// trap helpers. Byte-identical IR before and after.

/// Emit the LLVM IR for `Terminator::Select` with `ActorAsk` +
/// optional `AfterTimer` arms.
///
/// Producer contract (`Terminator::Select { arms, next }`):
/// - Every arm carries `body_block` (the block reached on win) and
///   `binding` (the per-arm reply slot for `ActorAsk` arms; `None`
///   for `AfterTimer`).
/// - At most one `AfterTimer` arm is present (HIR enforces).
/// - `StreamNext` / `TaskAwait` arms are not in scope for this change
///   and remain fail-closed below (defence-in-depth — the MIR
///   producer rejects them before this codegen runs).
///
/// Emitted shape (per slice 3 plan §6 + runtime ABI). For each
/// `ActorAsk` arm: allocate `ch = hew_reply_channel_new()`, store
/// into a stack array slot, then call `hew_actor_ask_with_channel`.
/// If that call returns non-zero, branch to a mid-setup error
/// recovery block that frees every channel allocated so far and
/// traps (Risk R3). Read the AfterTimer arm's duration if present,
/// convert ns → ms, saturate to `i32::MAX`; otherwise use -1
/// (wait indefinitely). Call `hew_select_first(arr, n_asks, timeout_ms)`
/// to get the winner index. Dispatch via switch: an ActorAsk arm
/// index routes to that arm's winner block; the default routes to
/// the AfterTimer winner block (when present) or a trap. In each
/// winner block, `hew_reply_wait(ch[winner])` reads the reply, the
/// value is stored into `arm.binding`'s slot, `libc::free(reply_ptr)`
/// releases the buffer, and `hew_reply_channel_free(ch[winner])`
/// releases the channel. For every loser j, `hew_reply_channel_cancel(ch[j])`
/// runs BEFORE `hew_reply_channel_free(ch[j])` — Risk R4: cancel
/// before free prevents UAF when a reply races our free.
/// Finally each winner block branches to the arm's MIR-allocated
/// `body_block`.
fn stream_callback_type_suffix(item_ty: BasicTypeEnum<'_>) -> String {
    let mut hash = 0xcbf29ce484222325_u64;
    for byte in item_ty.print_to_string().to_string().bytes() {
        hash ^= u64::from(byte);
        hash = hash.wrapping_mul(0x100000001b3);
    }
    format!("{hash:016x}")
}

fn get_or_create_select_stream_callback<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    item_ty: BasicTypeEnum<'ctx>,
) -> CodegenResult<FunctionValue<'ctx>> {
    let callback_name = format!(
        "hew_select_stream_reply_ready_{}",
        stream_callback_type_suffix(item_ty)
    );
    if let Some(fv) = fn_ctx.llvm_mod.get_function(&callback_name) {
        return Ok(fv);
    }

    let ctx = fn_ctx.ctx;
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i64_ty = ctx.i64_type();
    // Target-correct `usize`/`size_t`: i32 on wasm32, i64 on native.
    // Used to reconcile the `size` parameter of `hew_reply` to its actual ABI width.
    let reply_size_ty = runtime_size_ty(ctx, fn_ctx.llvm_mod);
    let item_size = item_ty.size_of().ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "stream callback item type has no static size: {item_ty:?}"
        ))
    })?;
    let fn_ty = ctx
        .void_type()
        .fn_type(&[ptr_ty.into(), ptr_ty.into()], false);
    let callback = fn_ctx
        .llvm_mod
        .add_function(&callback_name, fn_ty, Some(Linkage::Private));
    let entry = ctx.append_basic_block(callback, "entry");
    let null_item_bb = ctx.append_basic_block(callback, "null_item");
    let non_null_bb = ctx.append_basic_block(callback, "non_null");
    let delivered_bb = ctx.append_basic_block(callback, "delivered");
    let not_delivered_bb = ctx.append_basic_block(callback, "not_delivered");
    let free_item_bb = ctx.append_basic_block(callback, "free_item");
    let return_bb = ctx.append_basic_block(callback, "return");

    let builder = ctx.create_builder();
    builder.position_at_end(entry);
    let ch = callback
        .get_nth_param(0)
        .ok_or_else(|| CodegenError::Llvm("stream callback missing channel param".into()))?
        .into_pointer_value();
    let item = callback
        .get_nth_param(1)
        .ok_or_else(|| CodegenError::Llvm("stream callback missing item param".into()))?
        .into_pointer_value();
    let item_is_null = builder
        .build_is_null(item, "stream_item_is_null")
        .llvm_ctx("stream callback item null cmp")?;
    builder
        .build_conditional_branch(item_is_null, null_item_bb, non_null_bb)
        .llvm_ctx("stream callback item null branch")?;

    let reply = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply",
    )?;

    builder.position_at_end(null_item_bb);
    builder
        .build_call(
            reply,
            &[
                ch.into(),
                ptr_ty.const_null().into(),
                // null-item path: size is zero; use the target-correct zero width.
                reply_size_ty.const_zero().into(),
            ],
            "stream_reply_null_item",
        )
        .llvm_ctx("stream callback null hew_reply call")?;
    builder
        .build_unconditional_branch(return_bb)
        .llvm_ctx("stream callback null return")?;

    builder.position_at_end(non_null_bb);
    let item_value = builder
        .build_load(item_ty, item, "stream_item_value")
        .llvm_ctx("stream callback item load")?;
    let item_slot = builder
        .build_alloca(item_ty, "stream_item_slot")
        .llvm_ctx("stream callback item slot alloca")?;
    builder
        .build_store(item_slot, item_value)
        .llvm_ctx("stream callback item store")?;
    // Reconcile item_size (always i64 from LLVM size_of) to the target-correct
    // `usize`/`size_t` width required by `hew_reply`'s `size` parameter.
    let item_size = if item_size.get_type() == reply_size_ty {
        item_size
    } else if reply_size_ty.get_bit_width() < i64_ty.get_bit_width() {
        builder
            .build_int_truncate(item_size, reply_size_ty, "stream_item_size_trunc")
            .llvm_ctx("stream callback item size trunc")?
    } else {
        builder
            .build_int_z_extend(item_size, reply_size_ty, "stream_item_size_zext")
            .llvm_ctx("stream callback item size zext")?
    };
    let delivered = builder
        .build_call(
            reply,
            &[ch.into(), item_slot.into(), item_size.into()],
            "stream_reply_delivered",
        )
        .llvm_ctx("stream callback hew_reply call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_reply returned void".into()))?
        .into_int_value();
    builder
        .build_conditional_branch(delivered, delivered_bb, not_delivered_bb)
        .llvm_ctx("stream callback delivered branch")?;

    builder.position_at_end(delivered_bb);
    builder
        .build_unconditional_branch(free_item_bb)
        .llvm_ctx("stream callback delivered return")?;

    builder.position_at_end(not_delivered_bb);
    builder
        .build_unconditional_branch(free_item_bb)
        .llvm_ctx("stream callback item free branch")?;

    builder.position_at_end(free_item_bb);
    let free_fn = match fn_ctx.llvm_mod.get_function("free") {
        Some(fv) => fv,
        None => fn_ctx.llvm_mod.add_function(
            "free",
            ctx.void_type().fn_type(&[ptr_ty.into()], false),
            Some(Linkage::External),
        ),
    };
    builder
        .build_call(free_fn, &[item.into()], "stream_item_free")
        .llvm_ctx("stream callback item free")?;
    builder
        .build_unconditional_branch(return_bb)
        .llvm_ctx("stream callback free return")?;

    builder.position_at_end(return_bb);
    builder
        .build_return(None)
        .llvm_ctx("stream callback return")?;
    Ok(callback)
}

fn load_current_task_scope<'ctx>(fn_ctx: &FnCtx<'_, 'ctx>) -> CodegenResult<PointerValue<'ctx>> {
    let ctx_ptr = fn_ctx.execution_context.ok_or_else(|| {
        CodegenError::FailClosed("select{} TaskAwait arm requires an execution context".into())
    })?;
    let offset = fn_ctx
        .ctx
        .i64_type()
        .const_int(HEW_CTX_OFFSET_TASK_SCOPE as u64, false);
    // W6.010: a `select` TaskAwait after a suspend must read the task scope from
    // the resume-installed thread-local context, not the unwound spilled `ctx`
    // param (same coro-aware routing as `current_actor_ptr`/`lower_context_field`).
    let read_ctx = live_execution_context_ptr(fn_ctx, ctx_ptr)?;
    let field_ptr = unsafe {
        fn_ctx
            .builder
            .build_gep(
                fn_ctx.ctx.i8_type(),
                read_ctx,
                &[offset],
                "select_task_scope_ptr",
            )
            .llvm_ctx("select task scope gep")?
    };
    Ok(fn_ctx
        .builder
        .build_load(
            fn_ctx.ctx.ptr_type(AddressSpace::default()),
            field_ptr,
            "select_task_scope",
        )
        .llvm_ctx("select task scope load")?
        .into_pointer_value())
}

pub(crate) fn emit_select_terminator<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    arms: &[hew_mir::SelectArm],
) -> CodegenResult<()> {
    use hew_mir::SelectArmKind;

    if arms.is_empty() {
        // HIR rejects empty selects with SelectNoArms; defence-in-depth.
        return Err(CodegenError::FailClosed(
            "select{} terminator carries zero arms (HIR should have rejected with SelectNoArms)"
                .to_string(),
        ));
    }

    // Partition arms by kind, preserving original arm indices for the
    // body-block dispatch. ActorAsk arms feed the `hew_select_first`
    // channel array; the AfterTimer arm (at most one — HIR enforces)
    // supplies the deadline.
    let mut wait_arm_indices: Vec<usize> = Vec::with_capacity(arms.len());
    let mut after_arm_index: Option<usize> = None;
    for (i, arm) in arms.iter().enumerate() {
        match &arm.kind {
            SelectArmKind::ActorAsk { .. }
            | SelectArmKind::StreamNext { .. }
            | SelectArmKind::TaskAwait { .. }
            | SelectArmKind::ChannelRecv { .. } => wait_arm_indices.push(i),
            SelectArmKind::AfterTimer { .. } => {
                if after_arm_index.is_some() {
                    return Err(CodegenError::FailClosed(
                        "select{} carries more than one AfterTimer arm \
                         (HIR should have rejected)"
                            .to_string(),
                    ));
                }
                after_arm_index = Some(i);
            }
        }
    }

    if wait_arm_indices.is_empty() {
        // A select with only an `after` arm is structurally valid but
        // has no race to dispatch: we'd reduce to "wait then run the
        // after-arm body". The HIR forbids select{} composed only of
        // an `after` arm (a sealed-shape invariant); fail closed.
        return Err(CodegenError::FailClosed(
            "select{} carries no value-producing arms (only AfterTimer): \
             HIR should have rejected as a sealed-shape violation"
                .to_string(),
        ));
    }

    let n_waits: usize = wait_arm_indices.len();
    let n_waits_i32 = i32::try_from(n_waits).map_err(|_| {
        CodegenError::FailClosed(format!(
            "select{{}} arm count {n_waits} exceeds i32::MAX — runtime ABI is i32-bound"
        ))
    })?;

    let ctx = fn_ctx.ctx;
    let i32_ty = ctx.i32_type();
    let i64_ty = ctx.i64_type();
    let ptr_ty = ctx.ptr_type(AddressSpace::default());

    // Allocate the channel array on the stack: [N x ptr]. Per-ActorAsk
    // arm index i stores the freshly-allocated `*mut HewReplyChannel`
    // at `channel_array[i]`. The array is referenced by the winner
    // dispatch (free/cancel) and by `hew_select_first`.
    let arr_ty = ptr_ty.array_type(n_waits_i32 as u32);
    let arr_ptr = fn_ctx
        .builder
        .build_alloca(arr_ty, "select_channels")
        .llvm_ctx("select channel array alloca")?;
    let pending_id_arr_ty = i64_ty.array_type(n_waits_i32 as u32);
    let pending_id_arr_ptr = fn_ctx
        .builder
        .build_alloca(pending_id_arr_ty, "select_pending_read_ids")
        .llvm_ctx("select pending-read array alloca")?;

    // Resolve the parent function once for block allocation.
    let parent_fn = fn_ctx
        .builder
        .get_insert_block()
        .and_then(|bb| bb.get_parent())
        .ok_or_else(|| CodegenError::Llvm("select block has no parent function".into()))?;

    // The keep path needs only the blocking wait entry; the per-arm channel /
    // observer decls and the winner/loser dispatch decls are interned inside the
    // shared `emit_select_arm_setup` / `emit_select_winner_dispatch` helpers.
    let select_first = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_select_first",
    )?;

    // Per-arm readiness setup (channel alloc + ask/poll/observe + setup-fail
    // recovery). Shared with the suspending select path, which passes its shared
    // await-cancel arbiter so each arm's channel re-enqueues the parked
    // continuation on readiness; the blocking path passes `None`.
    emit_select_arm_setup(
        fn_ctx,
        arms,
        &wait_arm_indices,
        arr_ty,
        arr_ptr,
        pending_id_arr_ty,
        pending_id_arr_ptr,
        parent_fn,
        None,
    )?;

    // Determine the deadline. If the select carries an AfterTimer arm,
    // load its duration (i64 ns), convert to i32 ms (saturating). With
    // no AfterTimer arm, use -1 (wait indefinitely).
    let timeout_ms_val = if let Some(idx) = after_arm_index {
        let duration_place = match &arms[idx].kind {
            SelectArmKind::AfterTimer { duration } => *duration,
            _ => unreachable!("after_arm_index only set for AfterTimer arms"),
        };
        let (dur_ptr, dur_ty) = place_pointer(fn_ctx, duration_place)?;
        let dur_ns = fn_ctx
            .builder
            .build_load(dur_ty, dur_ptr, "select_after_dur_ns")
            .llvm_ctx("select after dur load")?
            .into_int_value();
        // ns → ms = dur_ns / 1_000_000.
        let ms_per_ns = i64_ty.const_int(1_000_000, false);
        let dur_ms_i64 = fn_ctx
            .builder
            .build_int_signed_div(dur_ns, ms_per_ns, "select_after_dur_ms_i64")
            .llvm_ctx("select after dur sdiv")?;
        // Saturate to i32::MAX (the runtime ABI is i32; a duration
        // > ~24.8 days clamps to "effectively wait forever" but stays
        // non-negative so `hew_select_first` doesn't interpret it as
        // -1 = infinite).
        let i32_max_as_i64 = i64_ty.const_int(i64::from(i32::MAX) as u64, true);
        let too_big = fn_ctx
            .builder
            .build_int_compare(
                inkwell::IntPredicate::SGT,
                dur_ms_i64,
                i32_max_as_i64,
                "select_after_dur_too_big",
            )
            .llvm_ctx("select after dur cmp")?;
        let clamped = fn_ctx
            .builder
            .build_select(
                too_big,
                i32_max_as_i64,
                dur_ms_i64,
                "select_after_dur_clamped",
            )
            .llvm_ctx("select after dur select")?
            .into_int_value();
        // Negative durations clamp to 0 (immediate timeout) so we
        // never accidentally collide with the -1 wait-forever
        // sentinel.
        let zero_i64 = i64_ty.const_zero();
        let neg = fn_ctx
            .builder
            .build_int_compare(
                inkwell::IntPredicate::SLT,
                clamped,
                zero_i64,
                "select_after_dur_neg",
            )
            .llvm_ctx("select after dur neg cmp")?;
        let clamped_nonneg = fn_ctx
            .builder
            .build_select(neg, zero_i64, clamped, "select_after_dur_nonneg")
            .llvm_ctx("select after dur nonneg select")?
            .into_int_value();
        fn_ctx
            .builder
            .build_int_truncate(clamped_nonneg, i32_ty, "select_after_dur_ms")
            .llvm_ctx("select after dur trunc")?
    } else {
        // No AfterTimer arm: wait indefinitely.
        i32_ty.const_int(u64::from(u32::MAX), true) // -1 in two's complement
    };

    // GEP the array down to a `*mut *mut HewReplyChannel` for the call.
    let idx0 = i32_ty.const_zero();
    let arr_first_ptr = unsafe {
        fn_ctx
            .builder
            .build_gep(arr_ty, arr_ptr, &[idx0, idx0], "select_channels_first")
            .llvm_ctx("select arr first gep")?
    };
    let count_val = i32_ty.const_int(n_waits_i32 as u64, true);
    let winner = fn_ctx
        .builder
        .build_call(
            select_first,
            &[
                arr_first_ptr.into(),
                count_val.into(),
                timeout_ms_val.into(),
            ],
            "select_winner_idx",
        )
        .llvm_ctx("hew_select_first call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_select_first returned void".into()))?
        .into_int_value();

    // The winner switch + per-arm winner/loser dispatch is identical to the
    // suspending-select resume edge (the only difference is HOW `winner` is
    // produced: a blocking `hew_select_first` here vs a non-blocking
    // `hew_select_ready_index` after the coro suspend). Share it.
    emit_select_winner_dispatch(
        fn_ctx,
        arms,
        &wait_arm_indices,
        after_arm_index,
        arr_ty,
        arr_ptr,
        pending_id_arr_ty,
        pending_id_arr_ptr,
        parent_fn,
        winner,
        None,
    )
}

/// Emit the winner switch + per-arm winner/loser dispatch shared by
/// [`emit_select_terminator`] (blocking) and
/// [`emit_suspending_select_terminator`] (coro-suspend). `winner` is the
/// first-ready arm index (or -1 → the AfterTimer winner / no-winner trap). When
/// `teardown` is `Some`, each terminal edge (every winner block AND the
/// AfterTimer winner block) calls it BEFORE branching into the arm body, so the
/// suspending path can cancel + free its shared await-cancel arbiter on every
/// resume edge exactly once. The blocking path passes `None`.
#[allow(
    clippy::too_many_arguments,
    reason = "the winner dispatch threads the channel + pending-id arrays, the \
              arm partition, the parent function, the winner value, and the \
              optional arbiter-teardown hook — all are needed in one place so \
              the per-arm loser cancel + free runs against the same array view \
              both select paths built"
)]
fn emit_select_winner_dispatch<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    arms: &[hew_mir::SelectArm],
    wait_arm_indices: &[usize],
    after_arm_index: Option<usize>,
    arr_ty: inkwell::types::ArrayType<'ctx>,
    arr_ptr: PointerValue<'ctx>,
    pending_id_arr_ty: inkwell::types::ArrayType<'ctx>,
    pending_id_arr_ptr: PointerValue<'ctx>,
    parent_fn: FunctionValue<'ctx>,
    winner: IntValue<'ctx>,
    teardown: Option<&dyn Fn() -> CodegenResult<()>>,
) -> CodegenResult<()> {
    use hew_mir::SelectArmKind;

    let ctx = fn_ctx.ctx;
    let i32_ty = ctx.i32_type();
    let i64_ty = ctx.i64_type();
    let ptr_ty = ctx.ptr_type(AddressSpace::default());

    let channel_cancel = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply_channel_cancel",
    )?;
    let channel_free = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply_channel_free",
    )?;
    let reply_wait = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply_wait",
    )?;
    let stream_cancel = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_stream_cancel_pending_read",
    )?;
    let channel_poll_cancel = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_channel_cancel_pending_read",
    )?;
    let signal_ready = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply_channel_signal_ready",
    )?;
    let task_completion_unobserve = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_task_completion_unobserve",
    )?;
    let task_get_result = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_task_get_result",
    )?;
    let libc_free = get_or_declare_free(fn_ctx);

    // Channel-array-slot GEP helper: `&channel_array[i]` — reconstructed here so
    // the dispatch can run as a free function shared by both select paths.
    let slot_ptr = |i: usize| -> CodegenResult<PointerValue<'ctx>> {
        let i_u32 = u32::try_from(i).map_err(|_| {
            CodegenError::FailClosed(format!("select arm index {i} exceeds u32::MAX"))
        })?;
        let idx0 = i32_ty.const_zero();
        let idx1 = i32_ty.const_int(u64::from(i_u32), false);
        let gep = unsafe {
            fn_ctx
                .builder
                .build_gep(arr_ty, arr_ptr, &[idx0, idx1], &format!("ch_slot_{i}"))
                .llvm_ctx("ch slot gep")?
        };
        Ok(gep)
    };
    let pending_id_slot_ptr = |i: usize| -> CodegenResult<PointerValue<'ctx>> {
        let i_u32 = u32::try_from(i).map_err(|_| {
            CodegenError::FailClosed(format!("select arm index {i} exceeds u32::MAX"))
        })?;
        let idx0 = i32_ty.const_zero();
        let idx1 = i32_ty.const_int(u64::from(i_u32), false);
        let gep = unsafe {
            fn_ctx
                .builder
                .build_gep(
                    pending_id_arr_ty,
                    pending_id_arr_ptr,
                    &[idx0, idx1],
                    &format!("pending_read_slot_{i}"),
                )
                .llvm_ctx("pending-read slot gep")?
        };
        Ok(gep)
    };

    // Allocate per-arm winner blocks and an AfterTimer-winner block
    // (if applicable). Each winner block handles its own loser
    // cleanup, reply read, and branch to the MIR-allocated body block.
    let mut winner_bbs: Vec<inkwell::basic_block::BasicBlock<'_>> =
        Vec::with_capacity(wait_arm_indices.len());
    for slot_idx in 0..wait_arm_indices.len() {
        winner_bbs.push(ctx.append_basic_block(parent_fn, &format!("select_win_ask_{slot_idx}")));
    }
    let after_winner_bb =
        after_arm_index.map(|_| ctx.append_basic_block(parent_fn, "select_win_after"));
    // Fallback "unreachable" block: -1 with no AfterTimer arm should
    // never happen because we passed -1 timeout, but defence-in-depth.
    let no_winner_bb = ctx.append_basic_block(parent_fn, "select_no_winner_trap");

    // Build the switch on `winner`:
    //   case 0 → winner_bbs[0]
    //   case 1 → winner_bbs[1]
    //   ...
    //   default → after_winner_bb (if present) else no_winner_bb
    let default_bb = after_winner_bb.unwrap_or(no_winner_bb);
    let cases: Vec<(IntValue<'_>, inkwell::basic_block::BasicBlock<'_>)> = winner_bbs
        .iter()
        .enumerate()
        .map(|(slot_idx, bb)| (i32_ty.const_int(slot_idx as u64, true), *bb))
        .collect();
    fn_ctx
        .builder
        .build_switch(winner, default_bb, &cases)
        .llvm_ctx("select winner switch")?;

    // No-winner fallback: trap. (Reachable only if the runtime
    // contract is violated — `hew_select_first` returned a value
    // outside `0..n_asks` and there was no AfterTimer arm.)
    fn_ctx.builder.position_at_end(no_winner_bb);
    emit_select_no_winner_trap(fn_ctx)?;

    // For each ActorAsk winner block: read the reply, cancel+free
    // every loser, branch to the arm body block.
    for (winner_slot, &arm_idx) in wait_arm_indices.iter().enumerate() {
        let arm = &arms[arm_idx];
        let binding_place = arm.binding.ok_or_else(|| {
            CodegenError::FailClosed(format!(
                 "select{{}} value-producing arm {arm_idx} carries no binding Place (the producer must \
                 populate `SelectArm.binding` with the per-arm reply slot)"
            ))
        })?;
        let body_block_id = arm.body_block;

        fn_ctx.builder.position_at_end(winner_bbs[winner_slot]);

        // Load the winning channel from the array.
        let win_slot_ptr = slot_ptr(winner_slot)?;
        let win_ch = fn_ctx
            .builder
            .build_load(
                ptr_ty,
                win_slot_ptr,
                &format!("select_win_ch_load_{winner_slot}"),
            )
            .llvm_ctx("select winner load")?
            .into_pointer_value();

        let (dest_ptr, dest_ty) = place_pointer(fn_ctx, binding_place)?;
        if let SelectArmKind::ChannelRecv { receiver, elem_ty } = &arm.kind {
            // The poll fired `signal_ready` on readiness WITHOUT consuming; the
            // winning consumer pops the queued item itself on its own edge via
            // the non-blocking try_recv and materialises `Option<T>` (close →
            // `None`). The owned `Some(string)` pointer is balanced by the MIR
            // drop spine — it never crosses the park thread, so a losing arm
            // (left queued + cancelled) cannot leak it.
            let Place::Local(dest_local) = binding_place else {
                return Err(CodegenError::FailClosed(format!(
                    "select{{}} ChannelRecv arm {arm_idx} binding must be Place::Local, got {binding_place:?}"
                )));
            };
            let rx_ptr = load_duplex_handle(fn_ctx, *receiver, "select_channel_winner")?;
            store_recv_option_via_layout(
                fn_ctx,
                rx_ptr,
                dest_local,
                "hew_channel_try_recv_layout",
                elem_ty,
            )?;
            let _ = dest_ptr;
            let _ = dest_ty;
            // Free the winning channel's caller-side ref (its poll already
            // cleared its own pending_read; no cancel needed).
            fn_ctx
                .builder
                .build_call(
                    channel_free,
                    &[win_ch.into()],
                    &format!("select_win_chan_ch_free_{winner_slot}"),
                )
                .llvm_ctx("select channel winner ch free")?;
        } else {
            let task_result_ptr = if matches!(arm.kind, SelectArmKind::TaskAwait { .. }) {
                let task_place = match &arm.kind {
                    SelectArmKind::TaskAwait { task } => *task,
                    _ => unreachable!("checked above"),
                };
                let task_ptr = load_duplex_handle(fn_ctx, task_place, "select_task_winner")?;
                Some(
                    fn_ctx
                        .builder
                        .build_call(
                            task_get_result,
                            &[task_ptr.into()],
                            &format!("select_task_get_result_{winner_slot}"),
                        )
                        .llvm_ctx("hew_task_get_result call")?
                        .try_as_basic_value()
                        .basic()
                        .ok_or_else(|| {
                            CodegenError::FailClosed("hew_task_get_result returned void".into())
                        })?
                        .into_pointer_value(),
                )
            } else {
                None
            };
            let reply_ptr = if let Some(result_ptr) = task_result_ptr {
                result_ptr
            } else {
                fn_ctx
                    .builder
                    .build_call(
                        reply_wait,
                        &[win_ch.into()],
                        &format!("select_reply_wait_{winner_slot}"),
                    )
                    .llvm_ctx("hew_reply_wait call")?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| CodegenError::FailClosed("hew_reply_wait returned void".into()))?
                    .into_pointer_value()
            };

            // Defensive null check on the reply pointer. `hew_reply_wait`
            // returns null when the published value pointer was null —
            // which happens on the allocation-failure-during-publish path
            // (`take_ready_reply` at `hew-runtime/src/reply_channel.rs:164`
            // returns null after `hew_reply_channel_mark_allocation_failed`
            // toggled the flag). `hew_select_first` will route us here
            // because the channel reached `ready=true` with a null value;
            // dereferencing would SIGSEGV. Mirrors `Terminator::Ask`'s
            // null-trap shape (`llvm.rs:4831-4854`).
            let null_bb =
                ctx.append_basic_block(parent_fn, &format!("select_reply_null_trap_{winner_slot}"));
            let ok_bb =
                ctx.append_basic_block(parent_fn, &format!("select_reply_ok_{winner_slot}"));
            let is_null = fn_ctx
                .builder
                .build_is_null(reply_ptr, &format!("select_reply_is_null_{winner_slot}"))
                .llvm_ctx("select reply null cmp")?;
            fn_ctx
                .builder
                .build_conditional_branch(is_null, null_bb, ok_bb)
                .llvm_ctx("select reply null branch")?;
            // Null-reply trap branch.
            fn_ctx.builder.position_at_end(null_bb);
            let trap_intrinsic = Intrinsic::find("llvm.trap").ok_or_else(|| {
                CodegenError::Llvm("llvm.trap intrinsic not found in LLVM build".into())
            })?;
            let trap_fn = trap_intrinsic
                .get_declaration(fn_ctx.llvm_mod, &[])
                .ok_or_else(|| CodegenError::Llvm("llvm.trap declaration failed".into()))?;
            fn_ctx
                .builder
                .build_call(
                    trap_fn,
                    &[],
                    &format!("select_reply_null_trap_call_{winner_slot}"),
                )
                .llvm_ctx("select reply null trap")?;
            fn_ctx
                .builder
                .build_unreachable()
                .llvm_ctx("select reply null unreachable")?;
            // Ok branch: load + store + frees.
            fn_ctx.builder.position_at_end(ok_bb);
            let reply_val = fn_ctx
                .builder
                .build_load(
                    dest_ty,
                    reply_ptr,
                    &format!("select_reply_value_{winner_slot}"),
                )
                .llvm_ctx("select reply load")?;
            fn_ctx
                .builder
                .build_store(dest_ptr, reply_val)
                .llvm_ctx("select reply store")?;

            if task_result_ptr.is_none() {
                fn_ctx
                    .builder
                    .build_call(
                        libc_free,
                        &[reply_ptr.into()],
                        &format!("select_reply_free_{winner_slot}"),
                    )
                    .llvm_ctx("select reply free")?;
            }

            // Free the winning channel's caller-side ref (no cancel — we
            // consumed its reply, the channel is not abandoned).
            fn_ctx
                .builder
                .build_call(
                    channel_free,
                    &[win_ch.into()],
                    &format!("select_win_ch_free_{winner_slot}"),
                )
                .llvm_ctx("select winner ch free")?;
        }

        // Loser cleanup: every OTHER ActorAsk arm gets cancel + free,
        // in that order (Risk R4 — cancel BEFORE free is the UAF
        // mitigation for late-reply races on cancelled channels).
        for (loser_slot, &loser_arm_idx) in wait_arm_indices.iter().enumerate() {
            if loser_slot == winner_slot {
                continue;
            }
            let loser_slot_ptr = slot_ptr(loser_slot)?;
            let loser_ch = fn_ctx
                .builder
                .build_load(
                    ptr_ty,
                    loser_slot_ptr,
                    &format!("select_loser_load_w{winner_slot}_l{loser_slot}"),
                )
                .llvm_ctx("select loser load")?
                .into_pointer_value();
            match &arms[loser_arm_idx].kind {
                SelectArmKind::StreamNext { stream } => {
                    let stream_ptr = load_duplex_handle(fn_ctx, *stream, "select_loser_stream")?;
                    let pending_slot = pending_id_slot_ptr(loser_slot)?;
                    let pending_id = fn_ctx
                        .builder
                        .build_load(
                            i64_ty,
                            pending_slot,
                            &format!("select_loser_pending_id_w{winner_slot}_l{loser_slot}"),
                        )
                        .llvm_ctx("select loser pending id load")?
                        .into_int_value();
                    fn_ctx
                        .builder
                        .build_call(
                            stream_cancel,
                            &[stream_ptr.into(), pending_id.into()],
                            &format!("select_loser_stream_cancel_w{winner_slot}_l{loser_slot}"),
                        )
                        .llvm_ctx("select loser stream cancel")?;
                }
                SelectArmKind::TaskAwait { task } => {
                    fn_ctx
                        .builder
                        .build_call(
                            channel_cancel,
                            &[loser_ch.into()],
                            &format!("select_loser_cancel_w{winner_slot}_l{loser_slot}"),
                        )
                        .llvm_ctx("select loser cancel")?;
                    let scope_ptr = load_current_task_scope(fn_ctx)?;
                    let task_ptr = load_duplex_handle(fn_ctx, *task, "select_loser_task")?;
                    fn_ctx
                        .builder
                        .build_call(
                            task_completion_unobserve,
                            &[
                                scope_ptr.into(),
                                task_ptr.into(),
                                signal_ready.as_global_value().as_pointer_value().into(),
                                loser_ch.into(),
                            ],
                            &format!("select_loser_task_unobserve_w{winner_slot}_l{loser_slot}"),
                        )
                        .llvm_ctx("select loser task unobserve")?;
                }
                SelectArmKind::ActorAsk { .. } => {
                    fn_ctx
                        .builder
                        .build_call(
                            channel_cancel,
                            &[loser_ch.into()],
                            &format!("select_loser_cancel_w{winner_slot}_l{loser_slot}"),
                        )
                        .llvm_ctx("select loser cancel")?;
                }
                SelectArmKind::ChannelRecv { receiver, .. } => {
                    let rx_ptr = load_duplex_handle(fn_ctx, *receiver, "select_loser_channel")?;
                    let pending_slot = pending_id_slot_ptr(loser_slot)?;
                    let pending_id = fn_ctx
                        .builder
                        .build_load(
                            i64_ty,
                            pending_slot,
                            &format!("select_loser_chan_pending_id_w{winner_slot}_l{loser_slot}"),
                        )
                        .llvm_ctx("select loser channel pending id load")?
                        .into_int_value();
                    fn_ctx
                        .builder
                        .build_call(
                            channel_poll_cancel,
                            &[rx_ptr.into(), pending_id.into()],
                            &format!("select_loser_channel_cancel_w{winner_slot}_l{loser_slot}"),
                        )
                        .llvm_ctx("select loser channel cancel")?;
                }
                SelectArmKind::AfterTimer { .. } => {
                    unreachable!("wait_arm_indices excludes AfterTimer")
                }
            }
            fn_ctx
                .builder
                .build_call(
                    channel_free,
                    &[loser_ch.into()],
                    &format!("select_loser_free_w{winner_slot}_l{loser_slot}"),
                )
                .llvm_ctx("select loser free")?;
        }

        // Suspending path: the won + all losers are deregistered; tear down the
        // shared await-cancel arbiter (cancel-no-wake + free) on this resume
        // edge before continuing — exactly once per winning arm. The blocking
        // path passes no teardown.
        if let Some(teardown) = teardown {
            teardown()?;
        }

        // Branch into the arm body block.
        let body_bb = *fn_ctx.blocks.get(&body_block_id).ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "select{{}} ActorAsk arm {arm_idx} body block {body_block_id} \
                 missing from FnCtx.blocks"
            ))
        })?;
        fn_ctx
            .builder
            .build_unconditional_branch(body_bb)
            .llvm_ctx("select win br")?;
    }

    // AfterTimer winner: every ActorAsk arm is a loser; cancel + free
    // each, then branch to the AfterTimer arm's body block.
    if let (Some(after_idx), Some(after_bb)) = (after_arm_index, after_winner_bb) {
        let after_arm = &arms[after_idx];
        let body_block_id = after_arm.body_block;
        fn_ctx.builder.position_at_end(after_bb);
        for (loser_slot, &loser_arm_idx) in wait_arm_indices.iter().enumerate() {
            let loser_slot_ptr = slot_ptr(loser_slot)?;
            let loser_ch = fn_ctx
                .builder
                .build_load(
                    ptr_ty,
                    loser_slot_ptr,
                    &format!("select_after_loser_load_{loser_slot}"),
                )
                .llvm_ctx("select after loser load")?
                .into_pointer_value();
            match &arms[loser_arm_idx].kind {
                SelectArmKind::StreamNext { stream } => {
                    let stream_ptr =
                        load_duplex_handle(fn_ctx, *stream, "select_after_loser_stream")?;
                    let pending_slot = pending_id_slot_ptr(loser_slot)?;
                    let pending_id = fn_ctx
                        .builder
                        .build_load(
                            i64_ty,
                            pending_slot,
                            &format!("select_after_loser_pending_id_{loser_slot}"),
                        )
                        .llvm_ctx("select after loser pending id load")?
                        .into_int_value();
                    fn_ctx
                        .builder
                        .build_call(
                            stream_cancel,
                            &[stream_ptr.into(), pending_id.into()],
                            &format!("select_after_loser_stream_cancel_{loser_slot}"),
                        )
                        .llvm_ctx("select after loser stream cancel")?;
                }
                SelectArmKind::TaskAwait { task } => {
                    fn_ctx
                        .builder
                        .build_call(
                            channel_cancel,
                            &[loser_ch.into()],
                            &format!("select_after_loser_cancel_{loser_slot}"),
                        )
                        .llvm_ctx("select after loser cancel")?;
                    let scope_ptr = load_current_task_scope(fn_ctx)?;
                    let task_ptr = load_duplex_handle(fn_ctx, *task, "select_after_loser_task")?;
                    fn_ctx
                        .builder
                        .build_call(
                            task_completion_unobserve,
                            &[
                                scope_ptr.into(),
                                task_ptr.into(),
                                signal_ready.as_global_value().as_pointer_value().into(),
                                loser_ch.into(),
                            ],
                            &format!("select_after_loser_task_unobserve_{loser_slot}"),
                        )
                        .llvm_ctx("select after loser task unobserve")?;
                }
                SelectArmKind::ActorAsk { .. } => {
                    fn_ctx
                        .builder
                        .build_call(
                            channel_cancel,
                            &[loser_ch.into()],
                            &format!("select_after_loser_cancel_{loser_slot}"),
                        )
                        .llvm_ctx("select after loser cancel")?;
                }
                SelectArmKind::ChannelRecv { receiver, .. } => {
                    let rx_ptr =
                        load_duplex_handle(fn_ctx, *receiver, "select_after_loser_channel")?;
                    let pending_slot = pending_id_slot_ptr(loser_slot)?;
                    let pending_id = fn_ctx
                        .builder
                        .build_load(
                            i64_ty,
                            pending_slot,
                            &format!("select_after_loser_chan_pending_id_{loser_slot}"),
                        )
                        .llvm_ctx("select after loser channel pending id load")?
                        .into_int_value();
                    fn_ctx
                        .builder
                        .build_call(
                            channel_poll_cancel,
                            &[rx_ptr.into(), pending_id.into()],
                            &format!("select_after_loser_channel_cancel_{loser_slot}"),
                        )
                        .llvm_ctx("select after loser channel cancel")?;
                }
                SelectArmKind::AfterTimer { .. } => {
                    unreachable!("wait_arm_indices excludes AfterTimer")
                }
            }
            fn_ctx
                .builder
                .build_call(
                    channel_free,
                    &[loser_ch.into()],
                    &format!("select_after_loser_free_{loser_slot}"),
                )
                .llvm_ctx("select after loser free")?;
        }
        // Suspending path: tear down the shared arbiter on the deadline-won
        // resume edge before running the timeout body (exactly once).
        if let Some(teardown) = teardown {
            teardown()?;
        }
        let body_bb = *fn_ctx.blocks.get(&body_block_id).ok_or_else(|| {
            CodegenError::FailClosed(format!(
                "select{{}} AfterTimer arm {after_idx} body block {body_block_id} \
                 missing from FnCtx.blocks"
            ))
        })?;
        fn_ctx
            .builder
            .build_unconditional_branch(body_bb)
            .llvm_ctx("select after br")?;
    }

    Ok(())
}

/// Resolve one fungible `(stable supervisor token, static slot)` role to the
/// current child `LocalPid` token immediately before request submission.
///
/// The runtime pins the supervisor control and copies the child token under
/// `children_lock`. The returned token can become retired after this call, but
/// `hew_local_pid_ask_with_channel` then fails closed by identity; no allocation
/// pointer crosses this boundary. The aggregate return follows the same
/// register-pair/MSVC-sret classifier as the legacy child lookup.
fn resolve_stable_role_token<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    role: hew_mir::StableActorRole,
    label: &str,
) -> CodegenResult<IntValue<'ctx>> {
    let i32_ty = fn_ctx.ctx.i32_type();
    let i64_ty = fn_ctx.ctx.i64_type();
    let ptr_ty = fn_ctx.ctx.ptr_type(AddressSpace::default());
    let supervisor_token = load_int_arg(
        fn_ctx,
        role.supervisor_token,
        i64_ty,
        &format!("{label}_supervisor_token"),
    )?;
    let slot = i32_ty.const_int(u64::from(role.slot_index), false);
    let result_ty = fn_ctx.ctx.struct_type(
        &[
            fn_ctx.ctx.i8_type().into(),
            fn_ctx.ctx.i8_type().into(),
            fn_ctx.ctx.i8_type().array_type(6).into(),
            ptr_ty.into(),
        ],
        false,
    );
    let triple = fn_ctx.llvm_mod.get_triple();
    let triple_str = triple.as_str().to_string_lossy();
    let (lookup, return_abi) = crate::abi_class::declare_aggregate_return(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        fn_ctx.target_data,
        &triple_str,
        "hew_local_pid_supervisor_child_get",
        result_ty,
        &[i64_ty.into(), i32_ty.into()],
    )?;
    match return_abi {
        crate::abi_class::AggregateReturnAbi::RegisterPair { .. } => {
            let pair = fn_ctx
                .builder
                .build_call(
                    lookup,
                    &[supervisor_token.into(), slot.into()],
                    &format!("{label}_lookup"),
                )
                .llvm_ctx("stable supervisor role lookup")?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| {
                    CodegenError::FailClosed(
                        "hew_local_pid_supervisor_child_get returned void".into(),
                    )
                })?
                .into_array_value();
            Ok(fn_ctx
                .builder
                .build_extract_value(pair, 1, &format!("{label}_child_token"))
                .llvm_ctx("stable role child token extract")?
                .into_int_value())
        }
        crate::abi_class::AggregateReturnAbi::Sret => {
            let result_slot = fn_ctx
                .builder
                .build_alloca(result_ty, &format!("{label}_lookup_result"))
                .llvm_ctx("stable role sret alloca")?;
            fn_ctx
                .builder
                .build_call(
                    lookup,
                    &[result_slot.into(), supervisor_token.into(), slot.into()],
                    &format!("{label}_lookup"),
                )
                .llvm_ctx("stable supervisor role sret lookup")?;
            let handle_field = fn_ctx
                .builder
                .build_struct_gep(result_ty, result_slot, 3, &format!("{label}_handle_gep"))
                .llvm_ctx("stable role handle gep")?;
            let handle = fn_ctx
                .builder
                .build_load(ptr_ty, handle_field, &format!("{label}_handle"))
                .llvm_ctx("stable role handle load")?
                .into_pointer_value();
            fn_ctx
                .builder
                .build_ptr_to_int(handle, i64_ty, &format!("{label}_child_token"))
                .llvm_ctx("stable role token ptrtoint")
        }
    }
}

/// Emit the per-arm readiness setup shared by [`emit_select_terminator`]
/// (blocking) and [`emit_suspending_select_terminator`] (coro-suspend): for each
/// wait arm allocate its reply channel, store it into `channel_array[slot]`,
/// register the arm's readiness observer (ActorAsk → `hew_actor_ask_with_channel`,
/// StreamNext → `hew_stream_poll`, TaskAwait → `hew_task_completion_observe`,
/// ChannelRecv → `hew_channel_poll`) so the source fires `signal_ready(ch)` on
/// readiness, and on any setup failure free every channel allocated so far and
/// trap (Risk R3). When `arbiter` is `Some((reg, self_actor))`, each
/// successfully-set-up channel additionally attaches the shared await-cancel
/// arbiter (`hew_reply_channel_set_await_cancel`) + the parked actor
/// (`hew_reply_channel_set_parked_waiter`), so the FIRST arm to fire wins the
/// arbiter's one-shot CAS and re-enqueues the parked continuation; the setup-fail
/// path then also cancels (no wake) + frees the arbiter so a partially-built
/// waitset leaks nothing. The blocking path passes `None`.
#[allow(
    clippy::too_many_arguments,
    reason = "the arm setup threads the channel + pending-id arrays, the arm \
              partition, the parent function, and the optional shared arbiter — \
              all needed in one place so the setup-fail recovery frees against \
              the same array view it built"
)]
fn emit_select_arm_setup<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    arms: &[hew_mir::SelectArm],
    wait_arm_indices: &[usize],
    arr_ty: inkwell::types::ArrayType<'ctx>,
    arr_ptr: PointerValue<'ctx>,
    pending_id_arr_ty: inkwell::types::ArrayType<'ctx>,
    pending_id_arr_ptr: PointerValue<'ctx>,
    parent_fn: FunctionValue<'ctx>,
    arbiter: Option<(PointerValue<'ctx>, PointerValue<'ctx>)>,
) -> CodegenResult<()> {
    use hew_mir::SelectArmKind;

    let ctx = fn_ctx.ctx;
    let i32_ty = ctx.i32_type();
    let i64_ty = ctx.i64_type();
    let ptr_ty = ctx.ptr_type(AddressSpace::default());

    let channel_new = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply_channel_new",
    )?;
    let ask_with_channel = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_actor_ask_with_channel",
    )?;
    let local_pid_ask_with_channel = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_local_pid_ask_with_channel",
    )?;
    let channel_cancel = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply_channel_cancel",
    )?;
    let channel_free = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply_channel_free",
    )?;
    let channel_retain = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply_channel_retain",
    )?;
    let signal_ready = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply_channel_signal_ready",
    )?;
    let stream_poll = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_stream_poll",
    )?;
    let stream_cancel = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_stream_cancel_pending_read",
    )?;
    let channel_poll = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_channel_poll",
    )?;
    let channel_poll_cancel = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_channel_cancel_pending_read",
    )?;
    let task_completion_observe = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_task_completion_observe",
    )?;
    let task_completion_unobserve = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_task_completion_unobserve",
    )?;
    let set_await_cancel = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply_channel_set_await_cancel",
    )?;
    let set_parked_waiter = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply_channel_set_parked_waiter",
    )?;
    let cancel_arbiter = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_await_cancel_cancel",
    )?;
    let free_arbiter = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_await_cancel_free",
    )?;

    let slot_ptr = |i: usize| -> CodegenResult<PointerValue<'ctx>> {
        let i_u32 = u32::try_from(i).map_err(|_| {
            CodegenError::FailClosed(format!("select arm index {i} exceeds u32::MAX"))
        })?;
        let idx0 = i32_ty.const_zero();
        let idx1 = i32_ty.const_int(u64::from(i_u32), false);
        let gep = unsafe {
            fn_ctx
                .builder
                .build_gep(arr_ty, arr_ptr, &[idx0, idx1], &format!("ch_slot_{i}"))
                .llvm_ctx("ch slot gep")?
        };
        Ok(gep)
    };
    let pending_id_slot_ptr = |i: usize| -> CodegenResult<PointerValue<'ctx>> {
        let i_u32 = u32::try_from(i).map_err(|_| {
            CodegenError::FailClosed(format!("select arm index {i} exceeds u32::MAX"))
        })?;
        let idx0 = i32_ty.const_zero();
        let idx1 = i32_ty.const_int(u64::from(i_u32), false);
        let gep = unsafe {
            fn_ctx
                .builder
                .build_gep(
                    pending_id_arr_ty,
                    pending_id_arr_ptr,
                    &[idx0, idx1],
                    &format!("pending_read_slot_{i}"),
                )
                .llvm_ctx("pending-read slot gep")?
        };
        Ok(gep)
    };

    for (slot_idx, &arm_idx) in wait_arm_indices.iter().enumerate() {
        // Allocate the channel.
        let ch_val = fn_ctx
            .builder
            .build_call(channel_new, &[], &format!("select_ch_new_{slot_idx}"))
            .llvm_ctx("hew_reply_channel_new call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("hew_reply_channel_new returned void".into()))?
            .into_pointer_value();

        // Store into channel_array[slot_idx]. We perform the store
        // BEFORE issuing the ask so the recovery block sees a
        // consistent array view if the ask issue fails.
        let slot = slot_ptr(slot_idx)?;
        fn_ctx
            .builder
            .build_store(slot, ch_val)
            .llvm_ctx("ch slot store")?;

        // Suspending path: attach the shared arbiter + parked actor to this
        // arm's channel BEFORE arming its readiness source (the ask / poll /
        // observe below). The readiness arms (`hew_channel_poll`,
        // `hew_stream_poll`) wait on a foreign park thread that fires
        // `signal_ready` the instant the source is already readable — for a
        // channel whose item was queued + sink closed before the select, that
        // fire happens synchronously inside the arming call. If `caller_actor`
        // were still null at that fire (the attach done afterward, as it was),
        // `publish_reply_from_sender_ref` takes the condvar wake path
        // (`cond.notify_one()`) — but the suspending caller parks on the
        // coroutine, never on the condvar, so the wake is silently lost and the
        // actor parks forever (empty output, clean exit). macOS schedules the
        // park thread fast enough to win that race the vast majority of the
        // time; Linux usually loses it, hence the platform-specific flake.
        // Setting the parked waiter first routes any immediate fire through
        // `enqueue_resume` (recorded as `pending_wake` while still Running and
        // drained by the suspend edge — the FG3 two-phase park), so the wake is
        // never lost regardless of park-thread timing.
        if let Some((reg, self_actor)) = arbiter {
            fn_ctx
                .builder
                .build_call(
                    set_await_cancel,
                    &[ch_val.into(), reg.into()],
                    &format!("select_set_await_cancel_{slot_idx}"),
                )
                .llvm_ctx("hew_reply_channel_set_await_cancel call")?;
            fn_ctx
                .builder
                .build_call(
                    set_parked_waiter,
                    &[ch_val.into(), self_actor.into()],
                    &format!("select_set_parked_waiter_{slot_idx}"),
                )
                .llvm_ctx("hew_reply_channel_set_parked_waiter call")?;
        }

        // `recoverable` marks an arm whose non-zero setup status is a
        // FAIL-CLOSED send failure (the receiver was unreachable — e.g. a
        // FUNGIBLE supervisor-child ref resolved to a dead/null child), NOT an
        // internal setup error. A recoverable failure retires THIS arm (frees
        // its channel, nulls its array slot so the dispatch + teardown skip it)
        // and continues setting up the remaining arms, mirroring the single-shot
        // `hew_actor_ask` path which binds `Err(AskError::*)` on any non-zero
        // send status instead of trapping. Only ActorAsk is a send surface;
        // stream/task/channel poll failures stay process-fatal (genuine
        // registration failures, not a send the program can recover from).
        let (status, current_has_retained_observer, recoverable) = match &arms[arm_idx].kind {
            SelectArmKind::ActorAsk {
                actor,
                stable_role,
                msg_type,
                value,
                cleanup_plan: _,
                ..
            } => {
                // #1739: register R's destructor on this arm's channel BEFORE
                // the ask issues. In a select, at most one arm wins; a reply
                // delivered to a LOSING ask arm (another arm won, or the timer
                // fired) is never consumed, and the loser-cleanup `cancel+free`
                // below reaches the channel free leg with `value` still set.
                // Without this the free leg would `libc::free` the buffer alone
                // and leak the reply's embedded heap. R is the arm's reply
                // binding type (the slot the winner edge loads the reply into).
                let arm_reply_binding = arms[arm_idx].binding.ok_or_else(|| {
                    CodegenError::FailClosed(format!(
                        "select{{}} ActorAsk arm {arm_idx} carries no binding Place \
                         (the per-arm reply slot whose type is R)"
                    ))
                })?;
                let arm_reply_ty = ask_reply_ty(fn_ctx, arm_reply_binding)?.clone();
                wire_reply_drop_fn(fn_ctx, ch_val, &arm_reply_ty)?;
                let (payload_ptr, payload_size) =
                    actor_payload_ptr_size(fn_ctx, *value, "select_ask_payload")?;
                let msg_type_val = i32_ty.const_int(*msg_type as u64, false);
                // `payload_size` is built as i64; the `size` param is `usize`/
                // `size_t` (i32 on wasm32). Reconcile to the target-correct width.
                let select_ask_size_ty = runtime_size_ty(fn_ctx.ctx, fn_ctx.llvm_mod);
                let payload_size = reconcile_int_width_signed(
                    fn_ctx,
                    payload_size.into(),
                    select_ask_size_ty.into(),
                    "select ask payload size",
                )?;
                let (ask_fn, actor_arg) = if let Some(role) = stable_role {
                    (
                        local_pid_ask_with_channel,
                        resolve_stable_role_token(
                            fn_ctx,
                            *role,
                            &format!("select_role_{slot_idx}"),
                        )?
                        .into(),
                    )
                } else {
                    (
                        ask_with_channel,
                        load_duplex_handle(fn_ctx, *actor, "select_actor_handle")?.into(),
                    )
                };
                let status = fn_ctx
                    .builder
                    .build_call(
                        ask_fn,
                        &[
                            actor_arg,
                            msg_type_val.into(),
                            payload_ptr.into(),
                            payload_size.into(),
                            ch_val.into(),
                        ],
                        &format!("select_ask_issue_{slot_idx}"),
                    )
                    .llvm_ctx("hew_actor_ask_with_channel call")?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| {
                        CodegenError::FailClosed("hew_actor_ask_with_channel returned void".into())
                    })?
                    .into_int_value();
                (status, false, true)
            }
            SelectArmKind::StreamNext { stream } => {
                let binding_place = arms[arm_idx].binding.ok_or_else(|| {
                    CodegenError::FailClosed(format!(
                        "select{{}} StreamNext arm {arm_idx} carries no binding Place (the producer must \
                         populate `SelectArm.binding` with the stream item slot)"
                    ))
                })?;
                let (_, item_ty) = place_pointer(fn_ctx, binding_place)?;
                let stream_callback = get_or_create_select_stream_callback(fn_ctx, item_ty)?;
                fn_ctx
                    .builder
                    .build_call(
                        channel_retain,
                        &[ch_val.into()],
                        &format!("select_stream_ch_retain_{slot_idx}"),
                    )
                    .llvm_ctx("stream channel retain")?;
                let stream_ptr = load_duplex_handle(fn_ctx, *stream, "select_stream_handle")?;
                let pending_id = fn_ctx
                    .builder
                    .build_call(
                        stream_poll,
                        &[
                            stream_ptr.into(),
                            stream_callback.as_global_value().as_pointer_value().into(),
                            ch_val.into(),
                        ],
                        &format!("select_stream_poll_{slot_idx}"),
                    )
                    .llvm_ctx("hew_stream_poll call")?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| {
                        CodegenError::FailClosed("hew_stream_poll returned void".into())
                    })?
                    .into_int_value();
                let pending_slot = pending_id_slot_ptr(slot_idx)?;
                fn_ctx
                    .builder
                    .build_store(pending_slot, pending_id)
                    .llvm_ctx("pending-read id store")?;
                let failed = fn_ctx
                    .builder
                    .build_int_compare(
                        inkwell::IntPredicate::EQ,
                        pending_id,
                        i64_ty.const_zero(),
                        &format!("select_stream_poll_failed_{slot_idx}"),
                    )
                    .llvm_ctx("stream poll cmp")?;
                let status = fn_ctx
                    .builder
                    .build_int_z_extend(failed, i32_ty, &format!("select_stream_status_{slot_idx}"))
                    .llvm_ctx("stream poll status zext")?;
                (status, true, false)
            }
            SelectArmKind::TaskAwait { task } => {
                fn_ctx
                    .builder
                    .build_call(
                        channel_retain,
                        &[ch_val.into()],
                        &format!("select_task_ch_retain_{slot_idx}"),
                    )
                    .llvm_ctx("task channel retain")?;
                let scope_ptr = load_current_task_scope(fn_ctx)?;
                let task_ptr = load_duplex_handle(fn_ctx, *task, "select_task_handle")?;
                let status = fn_ctx
                    .builder
                    .build_call(
                        task_completion_observe,
                        &[
                            scope_ptr.into(),
                            task_ptr.into(),
                            signal_ready.as_global_value().as_pointer_value().into(),
                            ch_val.into(),
                        ],
                        &format!("select_task_observe_{slot_idx}"),
                    )
                    .llvm_ctx("hew_task_completion_observe call")?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| {
                        CodegenError::FailClosed("hew_task_completion_observe returned void".into())
                    })?
                    .into_int_value();
                (status, true, false)
            }
            SelectArmKind::ChannelRecv { receiver, .. } => {
                // Signal-only readiness poll (mirrors TaskAwait's observe +
                // StreamNext's pending-id): register a poll on the channel that
                // fires `signal_ready(ch)` when readable. The winner edge pops
                // the item itself via try_recv, so nothing is consumed here.
                fn_ctx
                    .builder
                    .build_call(
                        channel_retain,
                        &[ch_val.into()],
                        &format!("select_channel_ch_retain_{slot_idx}"),
                    )
                    .llvm_ctx("channel poll channel retain")?;
                let rx_ptr = load_duplex_handle(fn_ctx, *receiver, "select_channel_handle")?;
                let pending_id = fn_ctx
                    .builder
                    .build_call(
                        channel_poll,
                        &[
                            rx_ptr.into(),
                            signal_ready.as_global_value().as_pointer_value().into(),
                            ch_val.into(),
                            // Cancel-path release for the retained observer ref:
                            // a withdrawn poll frees exactly one reference of
                            // `ch_val` WITHOUT firing the readiness callback,
                            // matching `signal_ready`'s consume on the fire path
                            // (exactly-once ownership handoff — H1).
                            channel_free.as_global_value().as_pointer_value().into(),
                        ],
                        &format!("select_channel_poll_{slot_idx}"),
                    )
                    .llvm_ctx("hew_channel_poll call")?
                    .try_as_basic_value()
                    .basic()
                    .ok_or_else(|| {
                        CodegenError::FailClosed("hew_channel_poll returned void".into())
                    })?
                    .into_int_value();
                let pending_slot = pending_id_slot_ptr(slot_idx)?;
                fn_ctx
                    .builder
                    .build_store(pending_slot, pending_id)
                    .llvm_ctx("channel poll pending-id store")?;
                let failed = fn_ctx
                    .builder
                    .build_int_compare(
                        inkwell::IntPredicate::EQ,
                        pending_id,
                        i64_ty.const_zero(),
                        &format!("select_channel_poll_failed_{slot_idx}"),
                    )
                    .llvm_ctx("channel poll cmp")?;
                let status = fn_ctx
                    .builder
                    .build_int_z_extend(
                        failed,
                        i32_ty,
                        &format!("select_channel_status_{slot_idx}"),
                    )
                    .llvm_ctx("channel poll status zext")?;
                (status, true, false)
            }
            SelectArmKind::AfterTimer { .. } => {
                unreachable!("wait_arm_indices excludes AfterTimer")
            }
        };

        // Branch on status: 0 → next ask setup or the wait dispatch; non-zero
        // → setup recovery. For a RECOVERABLE arm (fungible ActorAsk send to a
        // dead/unreachable child) the recovery retires this single arm and
        // continues (see `setup_recover_bb` below); for a non-recoverable arm
        // (stream/task/channel poll registration failure) it frees every channel
        // allocated through `slot_idx` inclusive, then traps.
        let zero = i32_ty.const_zero();
        let failed = fn_ctx
            .builder
            .build_int_compare(
                inkwell::IntPredicate::NE,
                status,
                zero,
                &format!("select_ask_failed_{slot_idx}"),
            )
            .llvm_ctx("select ask cmp")?;
        let setup_ok_bb = ctx.append_basic_block(parent_fn, &format!("select_setup_ok_{slot_idx}"));

        if recoverable {
            // FAIL-CLOSED recoverable arm: a fungible ActorAsk whose send found
            // an unreachable receiver (dead/null child) returns a non-zero
            // status from `hew_actor_ask_with_channel`, exactly as the
            // single-shot ask does before it binds `Err(AskError::*)`. Rather
            // than abandon the whole select with a process-fatal trap, retire
            // ONLY this arm and continue: free its channel and NULL its array
            // slot so `hew_select_first`/`hew_select_ready_index` skip it
            // (both `continue` on a null slot) and every loser/teardown leg
            // (all null-guarded `cancel`/`free`) treats it as absent. The arm
            // never becomes ready, so the select races the remaining ready
            // arms (or falls through to the `after` deadline if it is the only
            // / last live arm) — never traps.
            let setup_recover_bb =
                ctx.append_basic_block(parent_fn, &format!("select_setup_recover_{slot_idx}"));
            fn_ctx
                .builder
                .build_conditional_branch(failed, setup_recover_bb, setup_ok_bb)
                .llvm_ctx("select setup recoverable br")?;

            fn_ctx.builder.position_at_end(setup_recover_bb);
            if let SelectArmKind::ActorAsk {
                value,
                cleanup_plan: Some(plan),
                ..
            } = &arms[arm_idx].kind
            {
                crate::llvm::emit_prepared_carrier_drop(fn_ctx, *value, plan)?;
            }
            // Release the caller-side reference the runtime kept on send failure
            // (`KeepCreatorRef`): `hew_reply_channel_free` runs the channel's
            // attached await-cancel teardown too, dropping this arm's ref on the
            // shared suspending-select arbiter (no-op on the blocking path).
            fn_ctx
                .builder
                .build_call(
                    channel_free,
                    &[ch_val.into()],
                    &format!("select_recover_free_{slot_idx}"),
                )
                .llvm_ctx("setup-recover free")?;
            // NULL the slot so the dispatch + every teardown leg skip this arm.
            let recover_slot = slot_ptr(slot_idx)?;
            fn_ctx
                .builder
                .build_store(recover_slot, ptr_ty.const_null())
                .llvm_ctx("setup-recover null slot store")?;
            fn_ctx
                .builder
                .build_unconditional_branch(setup_ok_bb)
                .llvm_ctx("setup-recover continue br")?;

            // Continue emitting the remaining arms in the OK block.
            fn_ctx.builder.position_at_end(setup_ok_bb);
            continue;
        }

        let setup_fail_bb =
            ctx.append_basic_block(parent_fn, &format!("select_setup_fail_{slot_idx}"));
        fn_ctx
            .builder
            .build_conditional_branch(failed, setup_fail_bb, setup_ok_bb)
            .llvm_ctx("select setup br")?;

        // Recovery block: free channels [0..=slot_idx] (each was
        // allocated and stored; the ask-issue failure also released
        // the queued sender ref on the failing channel inside the
        // runtime, but the caller-side ref is still live — see
        // `submit_ask_with_reply_channel`'s KeepCreatorRef behaviour).
        // Cancel before free for any channel where an ask was
        // successfully submitted (i < slot_idx); the failing channel
        // (slot_idx) only needs `free`.
        fn_ctx.builder.position_at_end(setup_fail_bb);
        for (j, &prev_arm_idx) in wait_arm_indices.iter().enumerate().take(slot_idx) {
            let cleanup_slot = slot_ptr(j)?;
            let cleanup_ch = fn_ctx
                .builder
                .build_load(ptr_ty, cleanup_slot, &format!("select_cleanup_load_{j}"))
                .llvm_ctx("setup-fail load")?
                .into_pointer_value();
            match &arms[prev_arm_idx].kind {
                SelectArmKind::StreamNext { stream } => {
                    let stream_ptr = load_duplex_handle(fn_ctx, *stream, "select_cleanup_stream")?;
                    let pending_slot = pending_id_slot_ptr(j)?;
                    let pending_id = fn_ctx
                        .builder
                        .build_load(
                            i64_ty,
                            pending_slot,
                            &format!("select_cleanup_pending_id_{j}"),
                        )
                        .llvm_ctx("setup-fail pending id load")?
                        .into_int_value();
                    fn_ctx
                        .builder
                        .build_call(
                            stream_cancel,
                            &[stream_ptr.into(), pending_id.into()],
                            &format!("select_cleanup_stream_cancel_{j}"),
                        )
                        .llvm_ctx("setup-fail stream cancel")?;
                }
                SelectArmKind::TaskAwait { task } => {
                    fn_ctx
                        .builder
                        .build_call(
                            channel_cancel,
                            &[cleanup_ch.into()],
                            &format!("select_cleanup_cancel_{j}"),
                        )
                        .llvm_ctx("setup-fail cancel")?;
                    let scope_ptr = load_current_task_scope(fn_ctx)?;
                    let task_ptr = load_duplex_handle(fn_ctx, *task, "select_cleanup_task")?;
                    fn_ctx
                        .builder
                        .build_call(
                            task_completion_unobserve,
                            &[
                                scope_ptr.into(),
                                task_ptr.into(),
                                signal_ready.as_global_value().as_pointer_value().into(),
                                cleanup_ch.into(),
                            ],
                            &format!("select_cleanup_task_unobserve_{j}"),
                        )
                        .llvm_ctx("setup-fail task unobserve")?;
                }
                SelectArmKind::ActorAsk { .. } => {
                    fn_ctx
                        .builder
                        .build_call(
                            channel_cancel,
                            &[cleanup_ch.into()],
                            &format!("select_cleanup_cancel_{j}"),
                        )
                        .llvm_ctx("setup-fail cancel")?;
                }
                SelectArmKind::ChannelRecv { receiver, .. } => {
                    let rx_ptr = load_duplex_handle(fn_ctx, *receiver, "select_cleanup_channel")?;
                    let pending_slot = pending_id_slot_ptr(j)?;
                    let pending_id = fn_ctx
                        .builder
                        .build_load(
                            i64_ty,
                            pending_slot,
                            &format!("select_cleanup_chan_pending_id_{j}"),
                        )
                        .llvm_ctx("setup-fail channel pending id load")?
                        .into_int_value();
                    fn_ctx
                        .builder
                        .build_call(
                            channel_poll_cancel,
                            &[rx_ptr.into(), pending_id.into()],
                            &format!("select_cleanup_channel_cancel_{j}"),
                        )
                        .llvm_ctx("setup-fail channel cancel")?;
                }
                SelectArmKind::AfterTimer { .. } => {
                    unreachable!("wait_arm_indices excludes AfterTimer")
                }
            }
            fn_ctx
                .builder
                .build_call(
                    channel_free,
                    &[cleanup_ch.into()],
                    &format!("select_cleanup_free_{j}"),
                )
                .llvm_ctx("setup-fail free")?;
        }
        // Failing channel: free the caller-side ref (no cancel — no
        // ask was successfully submitted, so no late replier exists).
        fn_ctx
            .builder
            .build_call(
                channel_free,
                &[ch_val.into()],
                &format!("select_setup_fail_free_self_{slot_idx}"),
            )
            .llvm_ctx("setup-fail self free")?;
        if current_has_retained_observer {
            fn_ctx
                .builder
                .build_call(
                    channel_free,
                    &[ch_val.into()],
                    &format!("select_setup_fail_free_observer_{slot_idx}"),
                )
                .llvm_ctx("setup-fail observer free")?;
        }

        // Suspending path: a partially-built waitset is being abandoned by the
        // trap; cancel (no wake) + free the shared arbiter so its registration
        // and any retained timer ref are reclaimed before the trap. The
        // per-channel frees above already released the arbiter refs the
        // successfully-set-up channels took; this drops the codegen-held ref.
        // (The trap aborts the process, but freeing keeps ASan/LSan clean and
        // matches the abandon-edge teardown.)
        if let Some((reg, _self_actor)) = arbiter {
            let cancelled_status = i32_ty.const_int(2, false); // Cancelled
            let no_wake = i32_ty.const_zero();
            fn_ctx
                .builder
                .build_call(
                    cancel_arbiter,
                    &[reg.into(), cancelled_status.into(), no_wake.into()],
                    &format!("select_setup_fail_arbiter_cancel_{slot_idx}"),
                )
                .llvm_ctx("setup-fail arbiter cancel")?;
            fn_ctx
                .builder
                .build_call(
                    free_arbiter,
                    &[reg.into()],
                    &format!("select_setup_fail_arbiter_free_{slot_idx}"),
                )
                .llvm_ctx("setup-fail arbiter free")?;
        }

        // Trap with HEW_TRAP_ACTOR_SEND_FAILED (the same diagnostic
        // code `Terminator::Send` uses on send-status failure — the
        // ask submission failed because the receiver mailbox was
        // unreachable, which is the same supervisor-visible failure).
        emit_select_setup_failure_trap(fn_ctx)?;

        // Setup OK: this arm's channel is live + observing. The shared arbiter +
        // parked actor were attached BEFORE the readiness source was armed
        // (above), so the FIRST arm to fire — including a synchronous fire
        // during the arming call for an already-ready source — wins the
        // one-shot CAS and re-enqueues the parked continuation through
        // `enqueue_resume` (the N-source generalisation of the `await x |
        // after d` waker). Nothing left to do here but fall through to the next
        // arm's setup.
        fn_ctx.builder.position_at_end(setup_ok_bb);
    }

    Ok(())
}

/// Emit the LLVM IR for `Terminator::SuspendingSelect` — the coro-suspend
/// sibling of [`emit_select_terminator`].
///
/// Shape (the suspending select ramp):
/// ```text
///   self = hew_actor_self()                       ; the parked-cont actor
///   <build channel_array + pending_id_array>      ; identical to the keep path
///   reg  = hew_await_cancel_new(self, null, null) ; ONE shared first-ready arbiter
///   <per arm: channel_new + ask/poll/observe>     ; emit_select_arm_setup, arbiter
///   <          set_await_cancel(ch, reg)>         ;   attached per channel so the
///   <          set_parked_waiter(ch, self)>       ;   first-ready arm wins + wakes
///   [AfterTimer] hew_await_cancel_schedule_deadline_ms(reg, global_wheel, ms)
///   br (no AfterTimer || armed) -> do_suspend, immediate
/// immediate:                                       ; deadline arm failed to arm —
///   <ready_index scan; if none, deadline fail-safe wins>
/// do_suspend:                                       ; coro.suspend (non-final)
///   switch coro.suspend [default -> return handle, 0 -> resume, 1 -> abandon]
/// abandon:                                          ; parked cont destroyed
///   cancel(reg, Cancelled, no_wake)
///   <per arm: cancel + free its channel / detach observer>
///   free(reg); br shared cleanup
/// resume:                                           ; first-ready (or deadline) woke us
///   winner = hew_select_ready_index(arr, count)    ; -1 => deadline won
///   switch winner -> per-arm winner blocks / AfterTimer winner block
///   (each winner block: bind value, cancel losers, cancel+free reg, -> body)
/// ```
/// The shared `HewAwaitCancel` arbiter is the N-source generalisation of the
/// single-source `await x | after d` waker: each arm's channel carries the SAME
/// arbiter, so the first arm to fire wins the one-shot CAS (`hew_await_cancel_complete`
/// inside `publish_reply_from_sender_ref`) and re-enqueues the parked actor exactly
/// once; the AfterTimer deadline rides the SAME arbiter on the global timer wheel.
/// The losers are cancelled on the resume edge by the shared winner dispatch.
pub(crate) fn emit_suspending_select_terminator<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    arms: &[hew_mir::SelectArm],
    resume: u32,
    cleanup: u32,
) -> CodegenResult<()> {
    use hew_mir::SelectArmKind;

    let coro = fn_ctx.coro.ok_or_else(|| {
        CodegenError::FailClosed(
            "Terminator::SuspendingSelect reached codegen but the function carries \
             no coro prologue state — lower_function must detect the suspend \
             carrier (has_suspend) and emit the prologue before the body"
                .into(),
        )
    })?;
    if !fn_ctx.blocks.contains_key(&resume) {
        return Err(CodegenError::FailClosed(format!(
            "SuspendingSelect resume target bb{resume} not found"
        )));
    }
    if !fn_ctx.blocks.contains_key(&cleanup) {
        return Err(CodegenError::FailClosed(format!(
            "SuspendingSelect cleanup target bb{cleanup} not found"
        )));
    }

    if arms.is_empty() {
        return Err(CodegenError::FailClosed(
            "select{} terminator carries zero arms (HIR should have rejected with SelectNoArms)"
                .to_string(),
        ));
    }

    // Partition arms (identical to the blocking path).
    let mut wait_arm_indices: Vec<usize> = Vec::with_capacity(arms.len());
    let mut after_arm_index: Option<usize> = None;
    for (i, arm) in arms.iter().enumerate() {
        match &arm.kind {
            SelectArmKind::ActorAsk { .. }
            | SelectArmKind::StreamNext { .. }
            | SelectArmKind::TaskAwait { .. }
            | SelectArmKind::ChannelRecv { .. } => wait_arm_indices.push(i),
            SelectArmKind::AfterTimer { .. } => {
                if after_arm_index.is_some() {
                    return Err(CodegenError::FailClosed(
                        "select{} carries more than one AfterTimer arm (HIR should have rejected)"
                            .to_string(),
                    ));
                }
                after_arm_index = Some(i);
            }
        }
    }
    if wait_arm_indices.is_empty() {
        return Err(CodegenError::FailClosed(
            "select{} carries no value-producing arms (only AfterTimer): \
             HIR should have rejected as a sealed-shape violation"
                .to_string(),
        ));
    }

    let n_waits = wait_arm_indices.len();
    let n_waits_i32 = i32::try_from(n_waits).map_err(|_| {
        CodegenError::FailClosed(format!(
            "select{{}} arm count {n_waits} exceeds i32::MAX — runtime ABI is i32-bound"
        ))
    })?;

    let ctx = fn_ctx.ctx;
    let i32_ty = ctx.i32_type();
    let i64_ty = ctx.i64_type();
    let ptr_ty = ctx.ptr_type(AddressSpace::default());

    // Allocate the channel + pending-id arrays (identical to the blocking path).
    let arr_ty = ptr_ty.array_type(n_waits_i32 as u32);
    let arr_ptr = fn_ctx
        .builder
        .build_alloca(arr_ty, "select_channels")
        .llvm_ctx("select channel array alloca")?;
    let pending_id_arr_ty = i64_ty.array_type(n_waits_i32 as u32);
    let pending_id_arr_ptr = fn_ctx
        .builder
        .build_alloca(pending_id_arr_ty, "select_pending_read_ids")
        .llvm_ctx("select pending-read array alloca")?;

    let parent_fn = fn_ctx
        .builder
        .get_insert_block()
        .and_then(|bb| bb.get_parent())
        .ok_or_else(|| {
            CodegenError::Llvm("suspending select block has no parent function".into())
        })?;

    // self = the parked-continuation actor the readiness/timer wake re-enqueues.
    // MUST come from hew_actor_self() (the live thread-local context), NOT a
    // spilled param: across a suspend the per-dispatch context the spilled param
    // pointed to is freed; on RESUME the scheduler installs a fresh context, so
    // the thread-local read returns the live actor (the FIX-THE-CLASS accessor
    // the ask/read/sleep ramps use).
    let actor_self_fn = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_actor_self",
    )?;
    let self_actor = fn_ctx
        .builder
        .build_call(actor_self_fn, &[], "suspending_select_self")
        .llvm_ctx("hew_actor_self call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_actor_self returned void".into()))?
        .into_pointer_value();

    // ONE shared first-ready arbiter. No source cleanup callback (the resume +
    // abandon edges do the per-arm loser cancel / observer detach with full
    // pointer context, like the sleep ramp's null-cleanup arbiter).
    let null_ptr = ptr_ty.const_null();
    let cancel_new = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_await_cancel_new",
    )?;
    let reg = fn_ctx
        .builder
        .build_call(
            cancel_new,
            &[self_actor.into(), null_ptr.into(), null_ptr.into()],
            "suspending_select_reg",
        )
        .llvm_ctx("hew_await_cancel_new call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_await_cancel_new returned void".into()))?
        .into_pointer_value();

    // Per-arm readiness setup + arbiter attachment.
    emit_select_arm_setup(
        fn_ctx,
        arms,
        &wait_arm_indices,
        arr_ty,
        arr_ptr,
        pending_id_arr_ty,
        pending_id_arr_ptr,
        parent_fn,
        Some((reg, self_actor)),
    )?;

    // AfterTimer arm: arm the deadline on the SAME arbiter via the global timer
    // wheel (the shared wheel cut-task-sleep uses). The timer-fired finish wakes
    // the actor; a non-timer arm winning cancels the timer through the arbiter's
    // one-shot complete. With no AfterTimer arm the select waits indefinitely on
    // the readiness arms.
    if let Some(idx) = after_arm_index {
        let duration_place = match &arms[idx].kind {
            SelectArmKind::AfterTimer { duration } => *duration,
            _ => unreachable!("after_arm_index only set for AfterTimer arms"),
        };
        let (dur_ptr, dur_ty) = place_pointer(fn_ctx, duration_place)?;
        let dur_ns = fn_ctx
            .builder
            .build_load(dur_ty, dur_ptr, "suspending_select_after_dur_ns")
            .llvm_ctx("select after dur load")?
            .into_int_value();
        // ns → ms, clamped non-negative (a negative duration is an immediate
        // deadline). The schedule takes u64 ms.
        let ms_per_ns = i64_ty.const_int(1_000_000, false);
        let dur_ms = fn_ctx
            .builder
            .build_int_signed_div(dur_ns, ms_per_ns, "suspending_select_after_dur_ms")
            .llvm_ctx("select after dur sdiv")?;
        let zero_i64 = i64_ty.const_zero();
        let neg = fn_ctx
            .builder
            .build_int_compare(
                inkwell::IntPredicate::SLT,
                dur_ms,
                zero_i64,
                "suspending_select_after_neg",
            )
            .llvm_ctx("select after neg cmp")?;
        let dur_ms_nonneg = fn_ctx
            .builder
            .build_select(neg, zero_i64, dur_ms, "suspending_select_after_nonneg")
            .llvm_ctx("select after nonneg select")?
            .into_int_value();
        let tw_fn = intern_runtime_decl(
            ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_global_timer_wheel",
        )?;
        let tw = fn_ctx
            .builder
            .build_call(tw_fn, &[], "suspending_select_tw")
            .llvm_ctx("hew_global_timer_wheel call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("hew_global_timer_wheel returned void".into()))?
            .into_pointer_value();
        let schedule = intern_runtime_decl(
            ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_schedule_deadline_ms",
        )?;
        // The deadline arm cannot fail-safe to "immediate" the way sleep does
        // (a select still has live readiness arms to wait on if the wheel is
        // unavailable), so we ignore the schedule rc: a failed arm simply means
        // no deadline fires and the readiness arms decide the winner. This never
        // hangs the timer subsystem.
        fn_ctx
            .builder
            .build_call(
                schedule,
                &[reg.into(), tw.into(), dur_ms_nonneg.into()],
                "suspending_select_schedule",
            )
            .llvm_ctx("hew_await_cancel_schedule_deadline_ms call")?;
    }

    // Suspend the continuation. The abandon arm cancels the arbiter (no wake),
    // deregisters every observer + cancels the timer, and frees the arbiter; the
    // resume arm scans for the winner and dispatches.
    let resume_bb = *fn_ctx.blocks.get(&resume).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "SuspendingSelect resume target bb{resume} not found"
        ))
    })?;
    let parent = parent_fn;
    let do_suspend_bb = ctx.append_basic_block(parent, "suspending_select_suspend");
    fn_ctx
        .builder
        .build_unconditional_branch(do_suspend_bb)
        .llvm_ctx("suspending select -> suspend br")?;
    let scan_bb = ctx.append_basic_block(parent, "suspending_select_scan");

    fn_ctx.builder.position_at_end(do_suspend_bb);
    let cancel_arbiter = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_await_cancel_cancel",
    )?;
    let free_arbiter = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_await_cancel_free",
    )?;
    let channel_cancel = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply_channel_cancel",
    )?;
    let channel_free = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply_channel_free",
    )?;
    let stream_cancel = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_stream_cancel_pending_read",
    )?;
    let channel_poll_cancel = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_channel_cancel_pending_read",
    )?;
    let task_completion_unobserve = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_task_completion_unobserve",
    )?;
    let signal_ready = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply_channel_signal_ready",
    )?;

    // GEP helpers for the abandon-edge per-arm deregister.
    let slot_ptr = |i: usize| -> CodegenResult<PointerValue<'ctx>> {
        let idx0 = i32_ty.const_zero();
        let idx1 = i32_ty.const_int(i as u64, false);
        Ok(unsafe {
            fn_ctx
                .builder
                .build_gep(
                    arr_ty,
                    arr_ptr,
                    &[idx0, idx1],
                    &format!("abandon_ch_slot_{i}"),
                )
                .llvm_ctx("abandon ch slot gep")?
        })
    };
    let pending_id_slot_ptr = |i: usize| -> CodegenResult<PointerValue<'ctx>> {
        let idx0 = i32_ty.const_zero();
        let idx1 = i32_ty.const_int(i as u64, false);
        Ok(unsafe {
            fn_ctx
                .builder
                .build_gep(
                    pending_id_arr_ty,
                    pending_id_arr_ptr,
                    &[idx0, idx1],
                    &format!("abandon_pending_slot_{i}"),
                )
                .llvm_ctx("abandon pending slot gep")?
        })
    };

    crate::suspend::emit_suspend_point(
        fn_ctx,
        coro,
        parent,
        scan_bb,
        "suspending_select",
        "suspending_select_abandon_cleanup",
        "suspending select abandon -> shared cleanup br",
        || {
            // Abandon: the parked continuation is being destroyed. Cancel the
            // arbiter WITHOUT waking (cancels the armed deadline so a racing fire
            // drops its wake), then deregister + free EVERY arm's channel
            // (cancel before free — UAF mitigation R4), then free the arbiter.
            // Each call runs exactly once on this single abandon edge.
            let cancelled_status = i32_ty.const_int(2, false); // Cancelled
            let no_wake = i32_ty.const_zero();
            fn_ctx
                .builder
                .build_call(
                    cancel_arbiter,
                    &[reg.into(), cancelled_status.into(), no_wake.into()],
                    "suspending_select_abandon_arbiter_cancel",
                )
                .llvm_ctx("abandon arbiter cancel")?;
            for (slot_idx, &arm_idx) in wait_arm_indices.iter().enumerate() {
                let ch = fn_ctx
                    .builder
                    .build_load(
                        ptr_ty,
                        slot_ptr(slot_idx)?,
                        &format!("abandon_ch_load_{slot_idx}"),
                    )
                    .llvm_ctx("abandon ch load")?
                    .into_pointer_value();
                match &arms[arm_idx].kind {
                    SelectArmKind::StreamNext { stream } => {
                        let stream_ptr = load_duplex_handle(fn_ctx, *stream, "abandon_stream")?;
                        let pending_id = fn_ctx
                            .builder
                            .build_load(
                                i64_ty,
                                pending_id_slot_ptr(slot_idx)?,
                                &format!("abandon_stream_pending_{slot_idx}"),
                            )
                            .llvm_ctx("abandon stream pending load")?
                            .into_int_value();
                        fn_ctx
                            .builder
                            .build_call(
                                stream_cancel,
                                &[stream_ptr.into(), pending_id.into()],
                                &format!("abandon_stream_cancel_{slot_idx}"),
                            )
                            .llvm_ctx("abandon stream cancel")?;
                    }
                    SelectArmKind::TaskAwait { task } => {
                        fn_ctx
                            .builder
                            .build_call(
                                channel_cancel,
                                &[ch.into()],
                                &format!("abandon_task_cancel_{slot_idx}"),
                            )
                            .llvm_ctx("abandon task cancel")?;
                        let scope_ptr = load_current_task_scope(fn_ctx)?;
                        let task_ptr = load_duplex_handle(fn_ctx, *task, "abandon_task")?;
                        fn_ctx
                            .builder
                            .build_call(
                                task_completion_unobserve,
                                &[
                                    scope_ptr.into(),
                                    task_ptr.into(),
                                    signal_ready.as_global_value().as_pointer_value().into(),
                                    ch.into(),
                                ],
                                &format!("abandon_task_unobserve_{slot_idx}"),
                            )
                            .llvm_ctx("abandon task unobserve")?;
                    }
                    SelectArmKind::ActorAsk { .. } => {
                        fn_ctx
                            .builder
                            .build_call(
                                channel_cancel,
                                &[ch.into()],
                                &format!("abandon_ask_cancel_{slot_idx}"),
                            )
                            .llvm_ctx("abandon ask cancel")?;
                    }
                    SelectArmKind::ChannelRecv { receiver, .. } => {
                        let rx_ptr = load_duplex_handle(fn_ctx, *receiver, "abandon_channel")?;
                        let pending_id = fn_ctx
                            .builder
                            .build_load(
                                i64_ty,
                                pending_id_slot_ptr(slot_idx)?,
                                &format!("abandon_chan_pending_{slot_idx}"),
                            )
                            .llvm_ctx("abandon channel pending load")?
                            .into_int_value();
                        fn_ctx
                            .builder
                            .build_call(
                                channel_poll_cancel,
                                &[rx_ptr.into(), pending_id.into()],
                                &format!("abandon_channel_cancel_{slot_idx}"),
                            )
                            .llvm_ctx("abandon channel cancel")?;
                    }
                    SelectArmKind::AfterTimer { .. } => {
                        unreachable!("wait_arm_indices excludes AfterTimer")
                    }
                }
                fn_ctx
                    .builder
                    .build_call(
                        channel_free,
                        &[ch.into()],
                        &format!("abandon_ch_free_{slot_idx}"),
                    )
                    .llvm_ctx("abandon ch free")?;
            }
            fn_ctx
                .builder
                .build_call(
                    free_arbiter,
                    &[reg.into()],
                    "suspending_select_abandon_arbiter_free",
                )
                .llvm_ctx("abandon arbiter free")?;
            Ok(())
        },
        || {
            // Resume: a readiness arm fired (or the deadline). Scan the readiness
            // flags once (non-blocking) to find the winner. A -1 scan is
            // AMBIGUOUS — the deadline may have won, OR the wake was stale (a
            // folded/leaked `pending_wake` with no readiness behind it) — so -1
            // consults the arbiter's one-shot state before dispatch: ONLY
            // TimedOut may enter the `after` arm. Pending re-suspends (the arms
            // and the deadline are all still armed; nothing was torn down), so a
            // stale wake never fabricates a timeout — the observable-honesty
            // axiom at the runtime level. Completed/Cancelled with no ready arm
            // violates the publish-ready-before-complete contract
            // (`publish_reply_from_sender_ref`) and traps fail-closed. A real
            // winner (>= 0) dispatches through the shared winner blocks, which
            // bind the winner, cancel the losers, and tear down the arbiter
            // (cancel-no-wake + free) on every resume edge exactly once.
            let idx0 = i32_ty.const_zero();
            let arr_first = unsafe {
                fn_ctx
                    .builder
                    .build_gep(
                        arr_ty,
                        arr_ptr,
                        &[idx0, idx0],
                        "suspending_select_arr_first",
                    )
                    .llvm_ctx("suspending select arr first gep")?
            };
            let ready_index = intern_runtime_decl(
                ctx,
                fn_ctx.llvm_mod,
                &mut fn_ctx.runtime_decls.borrow_mut(),
                "hew_select_ready_index",
            )?;
            let count_val = i32_ty.const_int(n_waits_i32 as u64, true);
            let winner = fn_ctx
                .builder
                .build_call(
                    ready_index,
                    &[arr_first.into(), count_val.into()],
                    "suspending_select_winner_idx",
                )
                .llvm_ctx("hew_select_ready_index call")?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| {
                    CodegenError::FailClosed("hew_select_ready_index returned void".into())
                })?
                .into_int_value();

            // -1 gate: route through the arbiter status before any dispatch.
            let dispatch_bb = ctx.append_basic_block(parent, "suspending_select_dispatch");
            let no_ready_bb = ctx.append_basic_block(parent, "suspending_select_no_ready");
            let pending_check_bb =
                ctx.append_basic_block(parent, "suspending_select_pending_check");
            let respark_bb = ctx.append_basic_block(parent, "suspending_select_respark");
            let stale_trap_bb = ctx.append_basic_block(parent, "suspending_select_stale_trap");
            let neg_one = i32_ty.const_all_ones();
            let no_ready = fn_ctx
                .builder
                .build_int_compare(
                    inkwell::IntPredicate::EQ,
                    winner,
                    neg_one,
                    "suspending_select_scan_no_ready",
                )
                .llvm_ctx("suspending select no-ready compare")?;
            fn_ctx
                .builder
                .build_conditional_branch(no_ready, no_ready_bb, dispatch_bb)
                .llvm_ctx("suspending select no-ready branch")?;

            fn_ctx.builder.position_at_end(no_ready_bb);
            let status_fn = intern_runtime_decl(
                ctx,
                fn_ctx.llvm_mod,
                &mut fn_ctx.runtime_decls.borrow_mut(),
                "hew_await_cancel_status",
            )?;
            let status = fn_ctx
                .builder
                .build_call(status_fn, &[reg.into()], "suspending_select_wake_status")
                .llvm_ctx("hew_await_cancel_status (select wake) call")?
                .try_as_basic_value()
                .basic()
                .ok_or_else(|| {
                    CodegenError::FailClosed("hew_await_cancel_status returned void".into())
                })?
                .into_int_value();
            let timed_out = fn_ctx
                .builder
                .build_int_compare(
                    inkwell::IntPredicate::EQ,
                    status,
                    i32_ty.const_int(3, false), // AwaitCancelStatus::TimedOut
                    "suspending_select_deadline_won",
                )
                .llvm_ctx("suspending select timed-out compare")?;
            fn_ctx
                .builder
                .build_conditional_branch(timed_out, dispatch_bb, pending_check_bb)
                .llvm_ctx("suspending select timed-out branch")?;

            fn_ctx.builder.position_at_end(pending_check_bb);
            let still_pending = fn_ctx
                .builder
                .build_int_compare(
                    inkwell::IntPredicate::EQ,
                    status,
                    i32_ty.const_zero(), // AwaitCancelStatus::Pending
                    "suspending_select_stale_wake",
                )
                .llvm_ctx("suspending select pending compare")?;
            fn_ctx
                .builder
                .build_conditional_branch(still_pending, respark_bb, stale_trap_bb)
                .llvm_ctx("suspending select pending branch")?;

            // Stale wake with the wait still Pending: re-suspend. Every arm
            // registration and the deadline timer are still armed (this edge
            // tore nothing down), so looping back to the suspend point re-parks
            // the continuation against the SAME readiness sources — the
            // executor's `ResumePoll::Pending` re-park path. The next genuine
            // wake re-enters the scan above.
            fn_ctx.builder.position_at_end(respark_bb);
            fn_ctx
                .builder
                .build_unconditional_branch(do_suspend_bb)
                .llvm_ctx("suspending select respark br")?;

            // Completed or Cancelled with NO ready arm: the fire path publishes
            // channel readiness BEFORE completing the arbiter, so this state is
            // unreachable under the contract — fail closed, never fabricate a
            // winner.
            fn_ctx.builder.position_at_end(stale_trap_bb);
            emit_select_no_winner_trap(fn_ctx)?;

            fn_ctx.builder.position_at_end(dispatch_bb);

            // Tear down the shared arbiter once per resume edge (cancel-no-wake
            // settles the one-shot if the deadline won; the timer is already
            // cancelled by the winning fire's complete), then free it. This runs
            // BEFORE the arm body so the body sees no live registration.
            let teardown = || -> CodegenResult<()> {
                let cancelled_status = i32_ty.const_int(2, false); // Cancelled
                let no_wake = i32_ty.const_zero();
                fn_ctx
                    .builder
                    .build_call(
                        cancel_arbiter,
                        &[reg.into(), cancelled_status.into(), no_wake.into()],
                        "suspending_select_resume_arbiter_cancel",
                    )
                    .llvm_ctx("resume arbiter cancel")?;
                fn_ctx
                    .builder
                    .build_call(
                        free_arbiter,
                        &[reg.into()],
                        "suspending_select_resume_arbiter_free",
                    )
                    .llvm_ctx("resume arbiter free")?;
                Ok(())
            };

            emit_select_winner_dispatch(
                fn_ctx,
                arms,
                &wait_arm_indices,
                after_arm_index,
                arr_ty,
                arr_ptr,
                pending_id_arr_ty,
                pending_id_arr_ptr,
                parent,
                winner,
                Some(&teardown),
            )
        },
    )?;
    let _ = resume_bb;
    Ok(())
}

/// Emit the LLVM IR for `Terminator::Join` — the wait-ALL sibling of
/// [`emit_select_terminator`].
///
/// Every branch is an actor-ask. The setup PREAMBLE is identical to the
/// `select` ActorAsk path: allocate one reply channel per branch, store
/// it into a stack `[N x ptr]` array, and issue the ask; any ask-issue
/// failure frees the channels allocated so far and traps
/// (`emit_select_setup_failure_trap`). The back half differs: instead of
/// a `hew_select_first` winner race, we WAIT on EVERY channel in
/// declaration order (`hew_reply_wait`), materialise each reply into the
/// `result` tuple's matching element, and free each channel exactly once.
///
/// Per HEW-SPEC-2026 §4.11.2, a branch error cancels the remaining
/// branches and the trap propagates. The substrate surfaces a branch
/// failure as a null reply (`hew_reply_wait` returns null when the
/// published value pointer was null — the allocation-failure /
/// unreachable-mailbox path); on a null reply we cancel + free every
/// not-yet-consumed channel (cancel-rest) and trap (propagate).
pub(crate) fn emit_join_terminator<'ctx>(
    fn_ctx: &FnCtx<'_, 'ctx>,
    branches: &[hew_mir::JoinBranch],
    result: Place,
    next: u32,
) -> CodegenResult<()> {
    if branches.is_empty() {
        // HIR rejects empty joins with JoinNoBranches; defence-in-depth.
        return Err(CodegenError::FailClosed(
            "join{} terminator carries zero branches (HIR should have rejected with JoinNoBranches)"
                .to_string(),
        ));
    }

    let n = branches.len();
    let n_i32 = i32::try_from(n).map_err(|_| {
        CodegenError::FailClosed(format!(
            "join{{}} branch count {n} exceeds i32::MAX — runtime ABI is i32-bound"
        ))
    })?;

    let ctx = fn_ctx.ctx;
    let i32_ty = ctx.i32_type();
    let ptr_ty = ctx.ptr_type(AddressSpace::default());

    // Channel array on the stack: [N x ptr]. Branch i stores its freshly
    // allocated `*mut HewReplyChannel` at `channel_array[i]`.
    let arr_ty = ptr_ty.array_type(n_i32 as u32);
    let arr_ptr = fn_ctx
        .builder
        .build_alloca(arr_ty, "join_channels")
        .llvm_ctx("join channel array alloca")?;

    let parent_fn = fn_ctx
        .builder
        .get_insert_block()
        .and_then(|bb| bb.get_parent())
        .ok_or_else(|| CodegenError::Llvm("join block has no parent function".into()))?;

    let channel_new = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply_channel_new",
    )?;
    let ask_with_channel = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_actor_ask_with_channel",
    )?;
    let local_pid_ask_with_channel = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_local_pid_ask_with_channel",
    )?;
    let channel_cancel = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply_channel_cancel",
    )?;
    let channel_free = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply_channel_free",
    )?;
    let reply_wait = intern_runtime_decl(
        ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_reply_wait",
    )?;
    let libc_free = get_or_declare_free(fn_ctx);

    // `&channel_array[i]` helper.
    let slot_ptr = |i: usize| -> CodegenResult<PointerValue<'ctx>> {
        let i_u32 = u32::try_from(i).map_err(|_| {
            CodegenError::FailClosed(format!("join branch index {i} exceeds u32::MAX"))
        })?;
        let idx0 = i32_ty.const_zero();
        let idx1 = i32_ty.const_int(u64::from(i_u32), false);
        let gep = unsafe {
            fn_ctx
                .builder
                .build_gep(arr_ty, arr_ptr, &[idx0, idx1], &format!("join_ch_slot_{i}"))
                .llvm_ctx("join ch slot gep")?
        };
        Ok(gep)
    };

    // ── Setup PREAMBLE: allocate channel + issue ask per branch ──────
    for (i, branch) in branches.iter().enumerate() {
        let ch_val = fn_ctx
            .builder
            .build_call(channel_new, &[], &format!("join_ch_new_{i}"))
            .llvm_ctx("hew_reply_channel_new call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("hew_reply_channel_new returned void".into()))?
            .into_pointer_value();

        // Store BEFORE issuing the ask so the recovery path sees a
        // consistent array view if the ask issue fails.
        let slot = slot_ptr(i)?;
        fn_ctx
            .builder
            .build_store(slot, ch_val)
            .llvm_ctx("join ch slot store")?;

        // #1739: register this branch's reply destructor on its channel BEFORE
        // the ask issues. `join` awaits every branch, but if the join is
        // abandoned (coro destroy / actor shutdown) after a branch deposited
        // its reply yet before `hew_reply_wait` consumed it, the abandon/cancel
        // teardown reaches this channel's free leg with `value` still set.
        // Without the destructor the free leg would `libc::free` the buffer
        // alone and leak R's embedded heap. R is the branch's reply type.
        wire_reply_drop_fn(fn_ctx, ch_val, &branch.reply_ty)?;

        let (payload_ptr, payload_size) =
            actor_payload_ptr_size(fn_ctx, branch.value, "join_ask_payload")?;
        let msg_type_val = i32_ty.const_int(branch.msg_type as u64, false);
        // `payload_size` is built as i64; the `size` param of
        // `hew_actor_ask_with_channel` is `usize`/`size_t` (i32 on wasm32).
        // Reconcile to the target-correct width, matching every sibling emitter.
        let join_ask_size_ty = runtime_size_ty(fn_ctx.ctx, fn_ctx.llvm_mod);
        let payload_size = reconcile_int_width_signed(
            fn_ctx,
            payload_size.into(),
            join_ask_size_ty.into(),
            "join ask payload size",
        )?;
        let (ask_fn, actor_arg) = if let Some(role) = branch.stable_role {
            (
                local_pid_ask_with_channel,
                resolve_stable_role_token(fn_ctx, role, &format!("join_role_{i}"))?.into(),
            )
        } else {
            (
                ask_with_channel,
                load_duplex_handle(fn_ctx, branch.actor, "join_actor_handle")?.into(),
            )
        };
        let status = fn_ctx
            .builder
            .build_call(
                ask_fn,
                &[
                    actor_arg,
                    msg_type_val.into(),
                    payload_ptr.into(),
                    payload_size.into(),
                    ch_val.into(),
                ],
                &format!("join_ask_issue_{i}"),
            )
            .llvm_ctx("hew_actor_ask_with_channel call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| {
                CodegenError::FailClosed("hew_actor_ask_with_channel returned void".into())
            })?
            .into_int_value();

        // Branch on status: 0 → next ask; non-zero → mid-setup recovery
        // (free every channel allocated through `i` inclusive, then trap).
        let zero = i32_ty.const_zero();
        let failed = fn_ctx
            .builder
            .build_int_compare(
                inkwell::IntPredicate::NE,
                status,
                zero,
                &format!("join_ask_failed_{i}"),
            )
            .llvm_ctx("join ask cmp")?;
        let setup_ok_bb = ctx.append_basic_block(parent_fn, &format!("join_setup_ok_{i}"));
        let setup_fail_bb = ctx.append_basic_block(parent_fn, &format!("join_setup_fail_{i}"));
        fn_ctx
            .builder
            .build_conditional_branch(failed, setup_fail_bb, setup_ok_bb)
            .llvm_ctx("join setup br")?;

        // Recovery block: cancel + free channels [0..i] (each had an ask
        // successfully submitted), then free the failing channel `i`
        // (no cancel — no ask was submitted), then trap.
        fn_ctx.builder.position_at_end(setup_fail_bb);
        if let Some(plan) = &branch.cleanup_plan {
            crate::llvm::emit_prepared_carrier_drop(fn_ctx, branch.value, plan)?;
        }
        for j in 0..i {
            let cleanup_slot = slot_ptr(j)?;
            let cleanup_ch = fn_ctx
                .builder
                .build_load(ptr_ty, cleanup_slot, &format!("join_cleanup_load_{j}"))
                .llvm_ctx("join setup-fail load")?
                .into_pointer_value();
            fn_ctx
                .builder
                .build_call(
                    channel_cancel,
                    &[cleanup_ch.into()],
                    &format!("join_cleanup_cancel_{j}"),
                )
                .llvm_ctx("join setup-fail cancel")?;
            fn_ctx
                .builder
                .build_call(
                    channel_free,
                    &[cleanup_ch.into()],
                    &format!("join_cleanup_free_{j}"),
                )
                .llvm_ctx("join setup-fail free")?;
        }
        fn_ctx
            .builder
            .build_call(
                channel_free,
                &[ch_val.into()],
                &format!("join_setup_fail_free_self_{i}"),
            )
            .llvm_ctx("join setup-fail self free")?;
        emit_select_setup_failure_trap(fn_ctx)?;

        fn_ctx.builder.position_at_end(setup_ok_bb);
    }

    // ── WAIT-ALL back half: wait every channel, materialise the tuple ─
    let (result_ptr, result_ty) = place_pointer(fn_ctx, result)?;
    // A multi-branch join binds a tuple struct; a single-branch join
    // binds the lone reply type directly (the checker's `Expr::Join`
    // rule). Resolve the per-branch element type accordingly.
    let result_struct_ty = match result_ty {
        BasicTypeEnum::StructType(st) => Some(st),
        _ => {
            if n != 1 {
                return Err(CodegenError::FailClosed(format!(
                    "join{{}} with {n} branches must bind a tuple struct result, got {result_ty:?}"
                )));
            }
            None
        }
    };

    for (i, branch) in branches.iter().enumerate() {
        let win_slot_ptr = slot_ptr(i)?;
        let win_ch = fn_ctx
            .builder
            .build_load(ptr_ty, win_slot_ptr, &format!("join_ch_load_{i}"))
            .llvm_ctx("join channel load")?
            .into_pointer_value();

        let reply_ptr = fn_ctx
            .builder
            .build_call(
                reply_wait,
                &[win_ch.into()],
                &format!("join_reply_wait_{i}"),
            )
            .llvm_ctx("hew_reply_wait call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("hew_reply_wait returned void".into()))?
            .into_pointer_value();

        // Null reply ⇒ this branch errored. Per §4.11.2 cancel the rest
        // (every not-yet-consumed channel: this one + branches i+1..n)
        // and trap (propagate).
        let null_bb = ctx.append_basic_block(parent_fn, &format!("join_reply_null_{i}"));
        let ok_bb = ctx.append_basic_block(parent_fn, &format!("join_reply_ok_{i}"));
        let is_null = fn_ctx
            .builder
            .build_is_null(reply_ptr, &format!("join_reply_is_null_{i}"))
            .llvm_ctx("join reply null cmp")?;
        fn_ctx
            .builder
            .build_conditional_branch(is_null, null_bb, ok_bb)
            .llvm_ctx("join reply null branch")?;

        // Cancel-rest + trap branch.
        fn_ctx.builder.position_at_end(null_bb);
        // Free the failing channel's caller-side ref (its reply was null).
        fn_ctx
            .builder
            .build_call(
                channel_free,
                &[win_ch.into()],
                &format!("join_err_free_self_{i}"),
            )
            .llvm_ctx("join error self free")?;
        // Cancel + free every still-pending sibling channel.
        for j in (i + 1)..n {
            let rest_slot = slot_ptr(j)?;
            let rest_ch = fn_ctx
                .builder
                .build_load(ptr_ty, rest_slot, &format!("join_err_rest_load_{i}_{j}"))
                .llvm_ctx("join error rest load")?
                .into_pointer_value();
            fn_ctx
                .builder
                .build_call(
                    channel_cancel,
                    &[rest_ch.into()],
                    &format!("join_err_rest_cancel_{i}_{j}"),
                )
                .llvm_ctx("join error rest cancel")?;
            fn_ctx
                .builder
                .build_call(
                    channel_free,
                    &[rest_ch.into()],
                    &format!("join_err_rest_free_{i}_{j}"),
                )
                .llvm_ctx("join error rest free")?;
        }
        let trap_intrinsic = Intrinsic::find("llvm.trap").ok_or_else(|| {
            CodegenError::Llvm("llvm.trap intrinsic not found in LLVM build".into())
        })?;
        let trap_fn = trap_intrinsic
            .get_declaration(fn_ctx.llvm_mod, &[])
            .ok_or_else(|| CodegenError::Llvm("llvm.trap declaration failed".into()))?;
        fn_ctx
            .builder
            .build_call(trap_fn, &[], &format!("join_reply_null_trap_{i}"))
            .llvm_ctx("join reply null trap")?;
        fn_ctx
            .builder
            .build_unreachable()
            .llvm_ctx("join reply null unreachable")?;

        // Ok branch: materialise the reply into result tuple element `i`.
        fn_ctx.builder.position_at_end(ok_bb);
        let (elem_ptr, elem_ty) = if let Some(struct_ty) = result_struct_ty {
            let elem_ty = struct_ty.get_field_type_at_index(i as u32).ok_or_else(|| {
                CodegenError::FailClosed(format!(
                    "join{{}} result tuple has no field at index {i} (branch/element mismatch)"
                ))
            })?;
            let field_ptr = fn_ctx
                .builder
                .build_struct_gep(
                    struct_ty,
                    result_ptr,
                    i as u32,
                    &format!("join_elem_{i}_gep"),
                )
                .llvm_ctx("join tuple elem gep")?;
            (field_ptr, elem_ty)
        } else {
            (result_ptr, result_ty)
        };
        let reply_val = fn_ctx
            .builder
            .build_load(elem_ty, reply_ptr, &format!("join_reply_value_{i}"))
            .llvm_ctx("join reply load")?;
        fn_ctx
            .builder
            .build_store(elem_ptr, reply_val)
            .llvm_ctx("join reply store")?;
        // Free the heap reply payload and the caller-side channel ref.
        fn_ctx
            .builder
            .build_call(
                libc_free,
                &[reply_ptr.into()],
                &format!("join_reply_free_{i}"),
            )
            .llvm_ctx("join reply free")?;
        fn_ctx
            .builder
            .build_call(channel_free, &[win_ch.into()], &format!("join_ch_free_{i}"))
            .llvm_ctx("join channel free")?;
        let _ = branch;
    }

    // All replies landed and the tuple is bound — continue at `next`.
    let next_bb = *fn_ctx.blocks.get(&next).ok_or_else(|| {
        CodegenError::FailClosed(format!(
            "join{{}} next block {next} missing from FnCtx.blocks"
        ))
    })?;
    fn_ctx
        .builder
        .build_unconditional_branch(next_bb)
        .llvm_ctx("join next br")?;
    Ok(())
}

/// Emit a fail-closed trap for a `Terminator::Select` mid-setup
/// failure (an ask-issue returned non-zero, leaving partially-allocated
/// channels which the recovery path frees before reaching here). Uses
/// `hew_trap_with_code(HEW_TRAP_ACTOR_SEND_FAILED=206)` so the
/// supervisor sees the same diagnostic as `Terminator::Send` failures;
/// follows with `llvm.trap` + `unreachable` as the non-actor fallback
/// (mirrors the `Terminator::Send` send-fail trap shape).
fn emit_select_setup_failure_trap<'ctx>(fn_ctx: &FnCtx<'_, 'ctx>) -> CodegenResult<()> {
    // Delegate to the single-sourced emit_trap_with_code helper so the
    // hew_trap_with_code declaration, llvm.trap, and unreachable are
    // emitted in one place. The trap code is single-sourced from the runtime.
    emit_trap_with_code(
        fn_ctx,
        HEW_TRAP_ACTOR_SEND_FAILED as u64,
        "select_setup_fail_trap",
    )
}

/// Emit a fail-closed trap for the unreachable "no winner" arm of the
/// select winner-dispatch switch. Reachable only if the runtime
/// contract is violated (e.g. `hew_select_first` returned -1 even
/// though we passed `-1` as the timeout and provided non-empty
/// channels).
fn emit_select_no_winner_trap<'ctx>(fn_ctx: &FnCtx<'_, 'ctx>) -> CodegenResult<()> {
    let trap_intrinsic = Intrinsic::find("llvm.trap")
        .ok_or_else(|| CodegenError::Llvm("llvm.trap intrinsic not found in LLVM build".into()))?;
    let trap_fn = trap_intrinsic
        .get_declaration(fn_ctx.llvm_mod, &[])
        .ok_or_else(|| CodegenError::Llvm("llvm.trap declaration failed".into()))?;
    fn_ctx
        .builder
        .build_call(trap_fn, &[], "select_no_winner_trap")
        .llvm_ctx("select no winner trap")?;
    fn_ctx
        .builder
        .build_unreachable()
        .llvm_ctx("select no winner unreachable")?;
    Ok(())
}
