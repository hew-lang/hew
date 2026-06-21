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

use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum};
use inkwell::values::FunctionValue;
use inkwell::{AddressSpace, IntPredicate};

use hew_mir::Place;
use hew_types::ResolvedTy;

use crate::llvm::CoroState;
#[allow(unused_imports)]
use crate::llvm::*;

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

/// Carrier for [`emit_suspending_sleep_terminator`] — the suspending
/// `sleep_ms(d)` ramp (`Terminator::SuspendingSleep`).
pub(crate) struct SuspendingSleepEmit {
    pub(crate) duration_ms: Place,
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
    fn_ctx
        .builder
        .build_unconditional_branch(coro.cleanup_block)
        .llvm_ctx(abandon_branch_context)?;

    fn_ctx.builder.position_at_end(resume_bind_bb);
    emit_resume_bind()
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
        let cancel_new = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_new",
        )?;
        let cleanup_fn = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_read_slot_cancel_cleanup",
        )?;
        let cleanup_ptr = cleanup_fn.as_global_value().as_pointer_value();
        let reg = fn_ctx
            .builder
            .build_call(
                cancel_new,
                &[self_actor.into(), cleanup_ptr.into(), slot.into()],
                "suspending_read_deadline_reg",
            )
            .llvm_ctx("hew_await_cancel_new call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("hew_await_cancel_new returned void".into()))?
            .into_pointer_value();
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
        fn_ctx.call_runtime_void(
            "hew_await_cancel_free",
            &[reg.into()],
            "suspending_read_err_reg_free",
            "hew_await_cancel_free (register err) call",
        )?;
    }
    if let (Some(result_dest), Some(error_dest)) = (term.deadline_result_dest, term.error_dest) {
        emit_read_deadline_timeout_err(fn_ctx, result_dest, error_dest)?;
    } else {
        store_empty_bytes(fn_ctx, term.result_dest)?;
    }
    fn_ctx
        .builder
        .build_unconditional_branch(resume_bb)
        .llvm_ctx("suspending read register-err br")?;

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
/// `hew_read_slot_take_raw` writes them into `result_dest` on the fast path.
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
        let cancel_new = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_new",
        )?;
        let cleanup_fn = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_read_slot_cancel_cleanup",
        )?;
        let cleanup_ptr = cleanup_fn.as_global_value().as_pointer_value();
        let reg = fn_ctx
            .builder
            .build_call(
                cancel_new,
                &[self_actor.into(), cleanup_ptr.into(), slot.into()],
                "suspending_accept_deadline_reg",
            )
            .llvm_ctx("hew_await_cancel_new call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("hew_await_cancel_new returned void".into()))?
            .into_pointer_value();
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
        let reg_free = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_free",
        )?;
        fn_ctx
            .builder
            .build_call(reg_free, &[reg.into()], "suspending_accept_err_reg_free")
            .llvm_ctx("hew_await_cancel_free (register err) call")?;
    }
    if let (Some(result_dest), Some(error_dest)) = (term.deadline_result_dest, term.error_dest) {
        emit_read_deadline_timeout_err(fn_ctx, result_dest, error_dest)?;
    } else {
        store_invalid_connection(fn_ctx, term.result_dest)?;
    }
    fn_ctx
        .builder
        .build_unconditional_branch(resume_bb)
        .llvm_ctx("suspending accept register-err br")?;

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
        let cancel_new = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_new",
        )?;
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
        let reg = fn_ctx
            .builder
            .build_call(
                cancel_new,
                &[self_actor.into(), cleanup_ptr.into(), cancel_ctx.into()],
                "suspending_stream_next_deadline_reg",
            )
            .llvm_ctx("hew_await_cancel_new call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("hew_await_cancel_new returned void".into()))?
            .into_pointer_value();
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
        let cancel_new = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_new",
        )?;
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
        let reg = fn_ctx
            .builder
            .build_call(
                cancel_new,
                &[self_actor.into(), cleanup_ptr.into(), cancel_ctx.into()],
                "suspending_channel_recv_deadline_reg",
            )
            .llvm_ctx("hew_await_cancel_new call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("hew_await_cancel_new returned void".into()))?
            .into_pointer_value();
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
    let duration = load_place_as_basic(fn_ctx, term.duration_ms, "suspending_sleep_duration")?
        .into_int_value();

    let null_ptr = fn_ctx
        .ctx
        .ptr_type(inkwell::AddressSpace::default())
        .const_null();
    let cancel_new = intern_runtime_decl(
        fn_ctx.ctx,
        fn_ctx.llvm_mod,
        &mut fn_ctx.runtime_decls.borrow_mut(),
        "hew_await_cancel_new",
    )?;
    // A pure timer→enqueue_resume registration: no cleanup source (sleep owns no
    // read slot / channel registration to tear down — the wheel cancel on the
    // abandon edge is the whole teardown).
    let reg = fn_ctx
        .builder
        .build_call(
            cancel_new,
            &[self_actor.into(), null_ptr.into(), null_ptr.into()],
            "suspending_sleep_reg",
        )
        .llvm_ctx("hew_await_cancel_new call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_await_cancel_new returned void".into()))?
        .into_pointer_value();

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
            "suspending_scope_deadline_reg",
        )
        .llvm_ctx("hew_await_cancel_new call")?
        .try_as_basic_value()
        .basic()
        .ok_or_else(|| CodegenError::FailClosed("hew_await_cancel_new returned void".into()))?
        .into_pointer_value();

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
    // The bytes value is passed BY POINTER (the triple alloca address), exactly
    // like the blocking `hew_sink_write_bytes` consumer.
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

    let rc = fn_ctx.call_runtime_int(
        "hew_stream_await_send",
        &[
            sink_ptr.into(),
            self_actor.into(),
            slot.into(),
            value_ptr.into(),
        ],
        "suspending_stream_send_register",
        "hew_stream_await_send call",
    )?;

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
        let cancel_new = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_await_cancel_new",
        )?;
        let cleanup_fn = intern_runtime_decl(
            fn_ctx.ctx,
            fn_ctx.llvm_mod,
            &mut fn_ctx.runtime_decls.borrow_mut(),
            "hew_reply_channel_cancel_cleanup",
        )?;
        let cleanup_ptr = cleanup_fn.as_global_value().as_pointer_value();
        let reg = fn_ctx
            .builder
            .build_call(
                cancel_new,
                &[self_actor.into(), cleanup_ptr.into(), ch.into()],
                "suspending_ask_deadline_reg",
            )
            .llvm_ctx("hew_await_cancel_new call")?
            .try_as_basic_value()
            .basic()
            .ok_or_else(|| CodegenError::FailClosed("hew_await_cancel_new returned void".into()))?
            .into_pointer_value();
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
    if !matches!(pid_slot_ty, BasicTypeEnum::IntType(t) if t.get_bit_width() == 64) {
        return Err(CodegenError::FailClosed(format!(
            "SuspendingRemoteAsk actor place must be the packed i64 RemotePid<T>, got {pid_slot_ty:?}"
        )));
    }
    let pid_val = fn_ctx
        .builder
        .build_load(pid_slot_ty, pid_slot, "remote_ask_pid")
        .llvm_ctx("load SuspendingRemoteAsk pid")?
        .into_int_value();
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
        "hew_node_api_ask_async",
    )?;
    let msg_type = fn_ctx.ctx.i32_type().const_int(term.msg_type as u64, false);
    let pending_handle = fn_ctx
        .builder
        .build_call(
            ask_async_fn,
            &[
                pid_val.into(),
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
    if !matches!(pid_slot_ty, BasicTypeEnum::IntType(t) if t.get_bit_width() == 64) {
        return Err(CodegenError::FailClosed(format!(
            "RemoteAsk actor place must be the packed i64 RemotePid<T>, got {pid_slot_ty:?}"
        )));
    }
    let pid_val = fn_ctx
        .builder
        .build_load(pid_slot_ty, pid_slot, "remote_ask_pid")
        .llvm_ctx("load RemoteAsk pid")?
        .into_int_value();
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
        "hew_node_api_ask",
    )?;
    let msg_type = fn_ctx.ctx.i32_type().const_int(term.msg_type as u64, false);
    let reply_ptr = fn_ctx
        .builder
        .build_call(
            ask_fn,
            &[
                pid_val.into(),
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
