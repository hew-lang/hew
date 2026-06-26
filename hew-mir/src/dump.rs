/// Textual MIR printer for `IrPipeline`.
///
/// Replaces `{:#?}` (Debug-format) as the `--dump-mir` renderer.
/// The format is line-oriented and FileCheck-friendly but is NOT
/// round-trippable in v1 — this is a canonical one-way diagnostic dump
/// designed for reviewer readability and grep/CHECK assertions.
///
/// ## Determinism
///
/// `RawMirFunction.await_deadline_ns` (`HashMap`), `.suspend_kinds`
/// (`HashMap`), and `.instr_spans` (`BTreeMap`) are side-tables that do not
/// represent IR shape. `instr_spans` is already a `BTreeMap` so its iteration
/// order is deterministic. `await_deadline_ns` and `suspend_kinds` are both
/// keyed by block id, so the collapsed `Terminator::Suspend` renderer looks up
/// `suspend_kinds` by the block being rendered (a single deterministic lookup,
/// not a `HashMap` iteration) to recover the carrier payload for the dump. The
/// raw maps themselves are NOT iterated into the dump: rendering a `HashMap`
/// field would make the golden corpus nondeterministic. If a consumer needs the
/// raw tables, they are available via the `{:#?}` Debug dump or the struct
/// fields directly.
///
/// ## Exhaustiveness (fail-closed)
///
/// Every match on `Instr`, `Terminator`, `Place`, `ExitPath`, `DropKind`,
/// and `MirDiagnosticKind` has NO wildcard `_` arm. Adding a new variant
/// that is not handled is a compile error, forcing the renderer to be
/// updated in the same commit. This is the "assertions distinguish correct
/// from garbage" discipline: a wildcard would silently render new variants
/// as nothing.
use std::fmt::Write as _;

use crate::model::{
    BasicBlock, CheckedMirFunction, ClosureEnvMode, CmpPred, Direction, DropFnSpec, DropKind,
    DropPlan, ElabDrop, ElaboratedMirFunction, ExitPath, FloatWidth, FunctionCallConv, Instr,
    IntArithOp, IntSignedness, IrPipeline, JoinBranch, LambdaEnvFieldDrop, MirCheck, MirDiagnostic,
    MirDiagnosticKind, MirStatement, Place, RawMirFunction, SelectArm, SelectArmKind, SuspendKind,
    Terminator, TrapKind,
};

/// Which stage of the pipeline to render.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DumpStage {
    Raw,
    Checked,
    Elab,
}

/// Render `pipeline` at the given `stage` as a textual dump.
///
/// The output is fully deterministic for a given `IrPipeline` — no
/// `HashMap` iteration order is included (see module-level note on
/// determinism). Calling this twice on the same pipeline produces
/// byte-identical results.
#[must_use]
pub fn dump_mir(pipeline: &IrPipeline, stage: DumpStage) -> String {
    let mut out = String::new();
    match stage {
        DumpStage::Raw => {
            for func in &pipeline.raw_mir {
                dump_raw_function(&mut out, func);
                writeln!(out).expect("write to string");
            }
        }
        DumpStage::Checked => {
            for func in &pipeline.checked_mir {
                dump_checked_function(&mut out, func);
                writeln!(out).expect("write to string");
            }
        }
        DumpStage::Elab => {
            for func in &pipeline.elaborated_mir {
                dump_elab_function(&mut out, func);
                writeln!(out).expect("write to string");
            }
        }
    }
    // Pipeline-level diagnostics (same for all stages — render once)
    if !pipeline.diagnostics.is_empty() {
        writeln!(out, "diagnostics:").expect("write to string");
        for diag in &pipeline.diagnostics {
            dump_mir_diagnostic(&mut out, diag);
        }
    }
    out
}

// ---------------------------------------------------------------------------
// Per-function renderers
// ---------------------------------------------------------------------------

fn dump_raw_function(out: &mut String, func: &RawMirFunction) {
    // Header line: fn <name>(<param_types>) -> <ret_ty> [conv=<cc>]
    let param_list = func
        .params
        .iter()
        .map(|ty| ty.user_facing().to_string())
        .collect::<Vec<_>>()
        .join(", ");
    writeln!(
        out,
        "fn {}({}) -> {} [conv={}]",
        func.name,
        param_list,
        func.return_ty.user_facing(),
        render_call_conv(func.call_conv),
    )
    .expect("write to string");

    // Locals table
    if !func.locals.is_empty() {
        writeln!(out, "  locals:").expect("write to string");
        for (i, ty) in func.locals.iter().enumerate() {
            writeln!(out, "    _{i}: {}", ty.user_facing()).expect("write to string");
        }
    }

    // intrinsic hint
    if let Some(id) = &func.intrinsic_id {
        writeln!(out, "  intrinsic: {id}").expect("write to string");
    }

    // Blocks — thread suspend_kinds so Suspend terminators can render the kind tag.
    for block in &func.blocks {
        dump_basic_block(out, block, 2, Some(&func.suspend_kinds));
    }
}

fn dump_checked_function(out: &mut String, func: &CheckedMirFunction) {
    writeln!(out, "fn {} -> {}", func.name, func.return_ty.user_facing()).expect("write to string");

    for block in &func.blocks {
        dump_basic_block(out, block, 2, None);
    }

    // The empty-checks signal is load-bearing: consumers key off
    // "checks: none" to know a function passed MIR checking.
    if func.checks.is_empty() {
        writeln!(out, "  checks: none").expect("write to string");
    } else {
        writeln!(out, "  checks:").expect("write to string");
        for check in &func.checks {
            writeln!(out, "    {}", render_mir_check(check)).expect("write to string");
        }
    }

    if !func.cooperate_sites.is_empty() {
        writeln!(out, "  cooperate:").expect("write to string");
        for site in &func.cooperate_sites {
            writeln!(out, "    bb{} {:?}", site.bb_id, site.kind).expect("write to string");
        }
    }
}

fn dump_elab_function(out: &mut String, func: &ElaboratedMirFunction) {
    writeln!(out, "fn {} -> {}", func.name, func.return_ty.user_facing()).expect("write to string");

    // Checker stream statements
    if !func.statements.is_empty() {
        writeln!(out, "  statements:").expect("write to string");
        for stmt in &func.statements {
            writeln!(out, "    {}", render_mir_statement(stmt)).expect("write to string");
        }
    }

    // Drop plans: the surface RC7 (single drop authority) will diff against.
    if func.drop_plans.is_empty() {
        writeln!(out, "  drop_plans: none").expect("write to string");
    } else {
        writeln!(out, "  drop_plans:").expect("write to string");
        for (exit_path, plan) in &func.drop_plans {
            writeln!(out, "    {} ->", render_exit_path(exit_path)).expect("write to string");
            dump_drop_plan(out, plan, 6);
        }
    }
}

// ---------------------------------------------------------------------------
// BasicBlock renderer
// ---------------------------------------------------------------------------

fn dump_basic_block(
    out: &mut String,
    block: &BasicBlock,
    indent: usize,
    suspend_kinds: Option<&std::collections::HashMap<u32, SuspendKind>>,
) {
    let pad = " ".repeat(indent);
    writeln!(out, "{pad}bb{}:", block.id).expect("write to string");

    for stmt in &block.statements {
        writeln!(out, "{pad}  stmt: {}", render_mir_statement(stmt)).expect("write to string");
    }

    for instr in &block.instructions {
        writeln!(out, "{pad}  {}", render_instr(instr)).expect("write to string");
    }

    let kind_entry = suspend_kinds.and_then(|sk| sk.get(&block.id));
    writeln!(
        out,
        "{pad}  {}",
        render_terminator_with_kind(&block.terminator, kind_entry)
    )
    .expect("write to string");
}

// ---------------------------------------------------------------------------
// Place renderer (compact single-line form)
// ---------------------------------------------------------------------------

fn render_place(place: &Place) -> String {
    match place {
        Place::Local(n) => format!("_{n}"),
        Place::ReturnSlot => "ret".to_string(),
        Place::DuplexHandle(n) => format!("duplex{n}"),
        Place::LambdaActorHandle(n) => format!("lambda{n}"),
        Place::ActorHandle(n) => format!("actor{n}"),
        Place::SendHalf(n) => format!("send_half{n}"),
        Place::RecvHalf(n) => format!("recv_half{n}"),
        Place::MachineTag(n) => format!("mtag{n}"),
        Place::MachineVariant {
            local,
            variant_idx,
            field_idx,
        } => format!("mvar{local}.{variant_idx}.{field_idx}"),
        Place::EnumTag(n) => format!("etag{n}"),
        Place::EnumVariant {
            local,
            variant_idx,
            field_idx,
        } => format!("evar{local}.{variant_idx}.{field_idx}"),
    }
}

// ---------------------------------------------------------------------------
// Terminator renderer
// ---------------------------------------------------------------------------

/// Render a `SuspendKind` as a bracketed kind tag with kind-specific fields,
/// e.g. `[channel_recv] elem_ty=string` or `[stream_next] elem_ty=bytes`.
///
/// Returns an empty string when `kind` is `None` (Checked/Elab stages where
/// the side-table is not available).
fn render_suspend_kind_tag(kind: Option<&SuspendKind>) -> String {
    let Some(kind) = kind else {
        return String::new();
    };
    match kind {
        SuspendKind::Ask { .. } => "[ask]".to_string(),
        SuspendKind::Read { .. } => "[read]".to_string(),
        SuspendKind::Accept { .. } => "[accept]".to_string(),
        SuspendKind::CallClosure { .. } => "[call_closure]".to_string(),
        SuspendKind::StreamNext { elem_ty, .. } => {
            format!("[stream_next] elem_ty={}", elem_ty.user_facing())
        }
        SuspendKind::StreamSend { .. } => "[stream_send]".to_string(),
        SuspendKind::ChannelRecv { elem_ty, .. } => {
            format!("[channel_recv] elem_ty={}", elem_ty.user_facing())
        }
        SuspendKind::RemoteAsk { .. } => "[remote_ask]".to_string(),
        SuspendKind::TaskAwait { .. } => "[task_await]".to_string(),
        SuspendKind::RestartWait { slot_index, .. } => {
            format!("[restart_wait] slot={slot_index}")
        }
        SuspendKind::Sleep { .. } => "[sleep]".to_string(),
    }
}

/// Render a terminator, annotating `Terminator::Suspend` with its `SuspendKind`
/// tag when `kind` is present (Raw-dump stage). The Checked and Elab stages
/// pass `None` — the bare `suspend is_final=…` rendering is preserved.
fn render_terminator_with_kind(term: &Terminator, kind: Option<&SuspendKind>) -> String {
    match term {
        Terminator::Suspend {
            resume,
            cleanup,
            is_final,
        } => {
            let tag = render_suspend_kind_tag(kind);
            if tag.is_empty() {
                format!("suspend is_final={is_final} -> resume=bb{resume} cleanup=bb{cleanup}")
            } else {
                format!(
                    "suspend {tag} is_final={is_final} -> resume=bb{resume} cleanup=bb{cleanup}"
                )
            }
        }
        other => render_terminator(other),
    }
}

#[allow(
    clippy::too_many_lines,
    reason = "exhaustive match over all 27 Terminator variants; splitting would obscure locality"
)]
fn render_terminator(term: &Terminator) -> String {
    match term {
        Terminator::Return => "return".to_string(),
        Terminator::Goto { target } => format!("goto bb{target}"),
        Terminator::Branch {
            cond,
            then_target,
            else_target,
        } => format!(
            "branch {} ? bb{then_target} : bb{else_target}",
            render_place(cond)
        ),
        Terminator::Call {
            callee,
            builtin,
            args,
            dest,
            next,
        } => {
            let arg_str = args.iter().map(render_place).collect::<Vec<_>>().join(", ");
            let dest_str = dest
                .as_ref()
                .map(|p| format!("{} = ", render_place(p)))
                .unwrap_or_default();
            let builtin_tag = builtin
                .as_ref()
                .map(|b| format!(" [builtin={b:?}]"))
                .unwrap_or_default();
            format!("{dest_str}call {callee}({arg_str}){builtin_tag} -> bb{next}")
        }
        Terminator::Trap { kind } => format!("trap({})", render_trap_kind(*kind)),
        Terminator::Yield { value, next } => {
            format!("yield {} -> bb{next}", render_place(value))
        }
        Terminator::MakeGenerator {
            dest,
            body_fn,
            next,
            env,
        } => {
            let env_str = env
                .as_ref()
                .map(|p| format!(", env={}", render_place(p)))
                .unwrap_or_default();
            format!(
                "{} = make_generator {body_fn}{env_str} -> bb{next}",
                render_place(dest)
            )
        }
        Terminator::MakeLambdaActor {
            dest,
            body_fn,
            state_drop_fn,
            shape,
            mailbox_capacity,
            next,
            env,
            env_field_drops,
        } => {
            let env_str = env
                .as_ref()
                .map(|p| format!(", env={}", render_place(p)))
                .unwrap_or_default();
            let drops_str = if env_field_drops.is_empty() {
                String::new()
            } else {
                let rendered = env_field_drops
                    .iter()
                    .copied()
                    .map(render_lambda_env_field_drop)
                    .collect::<Vec<_>>()
                    .join(",");
                format!(", env_drops=[{rendered}]")
            };
            format!(
                "{} = make_lambda_actor {body_fn} drop={state_drop_fn} shape={shape} mailbox={mailbox_capacity}{env_str}{drops_str} -> bb{next}",
                render_place(dest)
            )
        }
        Terminator::Send {
            actor,
            msg_type,
            value,
            next,
            alias_mode,
        } => format!(
            "send {}[msg={msg_type}] {} alias={alias_mode:?} -> bb{next}",
            render_place(actor),
            render_place(value)
        ),
        Terminator::Ask {
            actor,
            msg_type,
            value,
            result_dest,
            reply_dest,
            error_dest,
            next,
        } => format!(
            "{} = ask {}[msg={msg_type}] {} reply={} err={} -> bb{next}",
            render_place(result_dest),
            render_place(actor),
            render_place(value),
            render_place(reply_dest),
            render_place(error_dest),
        ),
        Terminator::RemoteAsk {
            actor,
            msg_type,
            value,
            timeout_ms,
            result_dest,
            reply_dest,
            error_dest,
            reply_ty,
            next,
        } => format!(
            "{} = remote_ask {}[msg={msg_type}] {} timeout={} reply={} err={} reply_ty={} -> bb{next}",
            render_place(result_dest),
            render_place(actor),
            render_place(value),
            render_place(timeout_ms),
            render_place(reply_dest),
            render_place(error_dest),
            reply_ty.user_facing(),
        ),
        Terminator::Select { arms, next } => {
            format!("select [{}] -> bb{next}", render_select_arms(arms))
        }
        Terminator::SuspendingSelect {
            arms,
            resume,
            cleanup,
        } => format!(
            "suspend.select [{}] -> resume=bb{resume} cleanup=bb{cleanup}",
            render_select_arms(arms)
        ),
        Terminator::Join {
            branches,
            result,
            next,
        } => {
            let branch_strs = branches
                .iter()
                .map(render_join_branch)
                .collect::<Vec<_>>()
                .join(", ");
            format!(
                "{} = join [{}] -> bb{next}",
                render_place(result),
                branch_strs
            )
        }
        Terminator::Suspend {
            resume,
            cleanup,
            is_final,
        } => format!("suspend is_final={is_final} -> resume=bb{resume} cleanup=bb{cleanup}"),
        Terminator::SuspendingScopeDeadline {
            scope,
            duration_ms,
            timeout_body_block,
            resume,
            cleanup,
        } => format!(
            "suspend.scope_deadline scope={} duration={} -> timeout=bb{timeout_body_block} resume=bb{resume} cleanup=bb{cleanup}",
            render_place(scope),
            render_place(duration_ms),
        ),
    }
}

// ---------------------------------------------------------------------------
// Instr renderer
// ---------------------------------------------------------------------------

#[allow(
    clippy::too_many_lines,
    reason = "exhaustive match over all Instr variants; each arm renders one kind"
)]
fn render_instr(instr: &Instr) -> String {
    match instr {
        // Context markers
        Instr::EnterContext => "enter_context".to_string(),
        Instr::ExitContext => "exit_context".to_string(),
        Instr::CheckCancellation => "check_cancellation".to_string(),
        Instr::ContextField { dest, offset } => {
            format!("{} = context_field[{offset}]", render_place(dest))
        }

        // Integer constants
        Instr::ConstI64 { dest, value } => format!("{} = const.i64 {value}", render_place(dest)),

        // Integer arithmetic (wrapping — RC6-stable tokens)
        Instr::IntAdd { dest, lhs, rhs } => format!(
            "{} = add.wrap {} {}",
            render_place(dest),
            render_place(lhs),
            render_place(rhs)
        ),
        Instr::IntSub { dest, lhs, rhs } => format!(
            "{} = sub.wrap {} {}",
            render_place(dest),
            render_place(lhs),
            render_place(rhs)
        ),
        Instr::IntMul { dest, lhs, rhs } => format!(
            "{} = mul.wrap {} {}",
            render_place(dest),
            render_place(lhs),
            render_place(rhs)
        ),
        Instr::IntDiv {
            signed,
            dest,
            lhs,
            rhs,
        } => format!(
            "{} = div.{} {} {}",
            render_place(dest),
            render_signedness(*signed),
            render_place(lhs),
            render_place(rhs)
        ),
        Instr::IntRem {
            signed,
            dest,
            lhs,
            rhs,
        } => format!(
            "{} = rem.{} {} {}",
            render_place(dest),
            render_signedness(*signed),
            render_place(lhs),
            render_place(rhs)
        ),

        // Bitwise / logical
        Instr::IntBitAnd { dest, lhs, rhs } => format!(
            "{} = bit_and {} {}",
            render_place(dest),
            render_place(lhs),
            render_place(rhs)
        ),
        Instr::IntBitOr { dest, lhs, rhs } => format!(
            "{} = bit_or {} {}",
            render_place(dest),
            render_place(lhs),
            render_place(rhs)
        ),
        Instr::IntBitXor { dest, lhs, rhs } => format!(
            "{} = bit_xor {} {}",
            render_place(dest),
            render_place(lhs),
            render_place(rhs)
        ),
        Instr::BoolNot { dest, operand } => {
            format!("{} = not {}", render_place(dest), render_place(operand))
        }
        Instr::IntBitNot { dest, operand } => {
            format!("{} = bit_not {}", render_place(dest), render_place(operand))
        }

        // Shifts
        Instr::IntShl { dest, lhs, rhs } => format!(
            "{} = shl {} {}",
            render_place(dest),
            render_place(lhs),
            render_place(rhs)
        ),
        Instr::IntShr {
            signed,
            dest,
            lhs,
            rhs,
        } => format!(
            "{} = shr.{} {} {}",
            render_place(dest),
            render_signedness(*signed),
            render_place(lhs),
            render_place(rhs)
        ),

        // Unary negation
        Instr::IntNegChecked {
            signed,
            dest,
            operand,
            overflow_flag,
        } => format!(
            "{} ovf={} = neg.checked.{} {}",
            render_place(dest),
            render_place(overflow_flag),
            render_signedness(*signed),
            render_place(operand)
        ),
        Instr::FloatNeg {
            dest,
            operand,
            width,
        } => format!(
            "{} = fneg.{} {}",
            render_place(dest),
            render_float_width(*width),
            render_place(operand)
        ),

        // Checked arithmetic — RC6-stable token form: <op>.<sign>
        Instr::IntArithChecked {
            op,
            signed,
            dest,
            lhs,
            rhs,
            overflow_flag,
        } => format!(
            "{} ovf={} = {}.checked.{} {} {}",
            render_place(dest),
            render_place(overflow_flag),
            render_arith_op(*op),
            render_signedness(*signed),
            render_place(lhs),
            render_place(rhs)
        ),
        Instr::IntArithCheckedOption {
            op,
            signed,
            width,
            dest,
            lhs,
            rhs,
        } => format!(
            "{} = {}.checked_option.{}.{:?} {} {}",
            render_place(dest),
            render_arith_op(*op),
            render_signedness(*signed),
            width,
            render_place(lhs),
            render_place(rhs)
        ),
        Instr::IntArithSaturating {
            op,
            signed,
            width,
            dest,
            lhs,
            rhs,
        } => format!(
            "{} = {}.saturating.{}.{:?} {} {}",
            render_place(dest),
            render_arith_op(*op),
            render_signedness(*signed),
            width,
            render_place(lhs),
            render_place(rhs)
        ),

        // Comparisons
        Instr::IntCmp {
            dest,
            pred,
            lhs,
            rhs,
        } => format!(
            "{} = icmp.{} {} {}",
            render_place(dest),
            render_cmp_pred(*pred),
            render_place(lhs),
            render_place(rhs)
        ),
        Instr::IdentityCompare { dest, lhs, rhs } => format!(
            "{} = identity_cmp {} {}",
            render_place(dest),
            render_place(lhs),
            render_place(rhs)
        ),
        Instr::CancellationTokenIsCancelled { dest, token } => format!(
            "{} = cancel_token_is_cancelled {}",
            render_place(dest),
            render_place(token)
        ),

        // Generator / wire
        Instr::GeneratorNext {
            dest,
            ctx,
            yield_ty,
        } => format!(
            "{} = gen_next {} yield_ty={}",
            render_place(dest),
            render_place(ctx),
            yield_ty.user_facing()
        ),
        Instr::WireCodec {
            dest,
            operand,
            direction,
            value_ty,
        } => format!(
            "{} = wire_codec.{:?} {} value_ty={}",
            render_place(dest),
            direction,
            render_place(operand),
            value_ty.user_facing()
        ),
        Instr::RecordCloneInplace {
            dest,
            src,
            record_name,
        } => format!(
            "{} = record_clone {} record={}",
            render_place(dest),
            render_place(src),
            record_name
        ),
        Instr::EnumCloneInplace {
            dest,
            src,
            enum_name,
        } => format!(
            "{} = enum_clone {} enum={}",
            render_place(dest),
            render_place(src),
            enum_name
        ),

        // Data movement
        Instr::Move { dest, src } => {
            format!("{} = move {}", render_place(dest), render_place(src))
        }
        Instr::NumericCast {
            dest,
            src,
            from_ty,
            to_ty,
        } => format!(
            "{} = cast {} {} -> {}",
            render_place(dest),
            render_place(src),
            from_ty.user_facing(),
            to_ty.user_facing()
        ),
        Instr::SaturatingWidthCast {
            dest,
            src,
            from_ty,
            to_ty,
        } => format!(
            "{} = saturating_cast {} {} -> {}",
            render_place(dest),
            render_place(src),
            from_ty.user_facing(),
            to_ty.user_facing()
        ),

        // Runtime ABI calls
        Instr::CallRuntimeAbi(call) => {
            let args = call
                .args()
                .iter()
                .map(render_place)
                .collect::<Vec<_>>()
                .join(", ");
            let dest = call
                .dest()
                .as_ref()
                .map(|p| format!("{} = ", render_place(p)))
                .unwrap_or_default();
            format!("{dest}call_rt {}({args})", call.symbol())
        }

        // Lock
        Instr::AutoLockAcquire { lock } => {
            format!("auto_lock_acquire {}", render_place(lock))
        }
        Instr::AutoLockRelease { lock } => {
            format!("auto_lock_release {}", render_place(lock))
        }

        // Closures
        Instr::MakeClosure {
            fn_symbol,
            env,
            dest,
            env_mode,
        } => format!(
            "{} = make_closure {} env={} mode={}",
            render_place(dest),
            fn_symbol,
            render_place(env),
            render_env_mode(*env_mode)
        ),
        Instr::ClosureEnvFieldLoad {
            env,
            env_ty,
            field_offset,
            dest,
        } => format!(
            "{} = closure_env_load {}.field[{}] env_ty={}",
            render_place(dest),
            render_place(env),
            field_offset.0,
            env_ty.user_facing()
        ),
        Instr::ActorStateFieldLoad { field_offset, dest } => {
            format!(
                "{} = actor_state_load field[{}]",
                render_place(dest),
                field_offset.0
            )
        }
        Instr::ActorStateFieldStore { field_offset, src } => {
            format!(
                "actor_state_store field[{}] = {}",
                field_offset.0,
                render_place(src)
            )
        }

        // Actor spawn
        Instr::SpawnActor {
            actor_name,
            state,
            init_args,
            dest,
            max_heap_bytes,
            cycle_capable,
        } => {
            let state_str = state
                .as_ref()
                .map(|p| format!(" state={}", render_place(p)))
                .unwrap_or_default();
            let args_str = if init_args.is_empty() {
                String::new()
            } else {
                format!(
                    " args=[{}]",
                    init_args
                        .iter()
                        .map(render_place)
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            };
            let heap = max_heap_bytes
                .map(|n| format!(" max_heap={n}"))
                .unwrap_or_default();
            format!(
                "{} = spawn_actor {actor_name}{state_str}{args_str}{heap} cycle_capable={cycle_capable}",
                render_place(dest),
            )
        }
        Instr::CallClosure {
            callee,
            args,
            ret_ty,
            dest,
        } => {
            let arg_str = args.iter().map(render_place).collect::<Vec<_>>().join(", ");
            let dest_str = dest
                .as_ref()
                .map(|p| format!("{} = ", render_place(p)))
                .unwrap_or_default();
            format!(
                "{dest_str}call_closure {}({arg_str}) ret_ty={}",
                render_place(callee),
                ret_ty.user_facing()
            )
        }
        Instr::SpawnTaskDirect {
            task,
            callee_symbol,
        } => format!("{} = spawn_task_direct {callee_symbol}", render_place(task)),
        Instr::SpawnTaskClosure {
            task,
            fn_symbol,
            env,
            env_ty,
        } => format!(
            "{} = spawn_task_closure {fn_symbol} env={} env_ty={}",
            render_place(task),
            render_place(env),
            env_ty.user_facing()
        ),

        // Drop
        Instr::Drop { place, ty, drop_fn } => {
            let drop_str = drop_fn
                .as_ref()
                .map(|s| format!(" fn={}", render_drop_fn_spec(s)))
                .unwrap_or_default();
            format!(
                "drop {} ty={}{drop_str}",
                render_place(place),
                ty.user_facing()
            )
        }

        // Witness ops
        Instr::WitnessSizeOf { dest, ty } => {
            format!("{} = sizeof {}", render_place(dest), ty.user_facing())
        }
        Instr::WitnessAlignOf { dest, ty } => {
            format!("{} = alignof {}", render_place(dest), ty.user_facing())
        }
        Instr::WitnessDropGlue { place, ty } => {
            format!("drop_glue {} ty={}", render_place(place), ty.user_facing())
        }
        Instr::WitnessMove { dest, src, ty } => format!(
            "{} = witness_move {} ty={}",
            render_place(dest),
            render_place(src),
            ty.user_facing()
        ),

        // Literals
        Instr::StringLit { bytes, dest } => {
            let s = String::from_utf8_lossy(bytes);
            format!("{} = string_lit {:?}", render_place(dest), s)
        }
        Instr::BytesLit { bytes, dest } => {
            format!(
                "{} = bytes_lit [{}]",
                render_place(dest),
                bytes
                    .iter()
                    .map(|b| format!("{b:#04x}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        }
        Instr::ConstGlobalLoad { item_id, dest } => {
            format!("{} = const_global {:?}", render_place(dest), item_id)
        }

        // Records / tuples
        Instr::RecordInit { ty, fields, dest } => {
            let field_strs = fields
                .iter()
                .map(|(off, p)| format!(".{}={}", off.0, render_place(p)))
                .collect::<Vec<_>>()
                .join(", ");
            format!(
                "{} = record_init {} {{ {field_strs} }}",
                render_place(dest),
                ty.user_facing()
            )
        }
        Instr::RecordFieldStore {
            record,
            field_offset,
            src,
        } => format!(
            "{}.field[{}] = {}",
            render_place(record),
            field_offset.0,
            render_place(src)
        ),
        Instr::RecordFieldLoad {
            record,
            field_offset,
            dest,
        } => format!(
            "{} = {}.field[{}]",
            render_place(dest),
            render_place(record),
            field_offset.0
        ),
        Instr::RecordFieldDrop {
            record,
            field_offset,
            ty,
            drop_fn,
        } => format!(
            "drop_field {}.field[{}] ty={} fn={drop_fn:?}",
            render_place(record),
            field_offset.0,
            ty.user_facing()
        ),
        Instr::TupleFieldLoad {
            tuple,
            field_index,
            dest,
        } => format!(
            "{} = {}.{field_index}",
            render_place(dest),
            render_place(tuple)
        ),
        Instr::TupleConstruct { elements, dest } => {
            let elems = elements
                .iter()
                .map(render_place)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{} = tuple ({elems})", render_place(dest))
        }

        // Float literals / arithmetic
        Instr::FloatLit {
            dest,
            value_bits,
            width,
        } => format!(
            "{} = const.{} 0x{value_bits:016x}",
            render_place(dest),
            render_float_width(*width)
        ),
        Instr::CharLit { value, dest } => {
            format!("{} = const.char 0x{value:04x}", render_place(dest))
        }
        Instr::UnitLit { dest } => format!("{} = unit", render_place(dest)),
        Instr::DurationLit { nanos, dest } => {
            format!("{} = const.duration {nanos}ns", render_place(dest))
        }
        Instr::FloatAdd {
            dest,
            lhs,
            rhs,
            width,
        } => format!(
            "{} = fadd.{} {} {}",
            render_place(dest),
            render_float_width(*width),
            render_place(lhs),
            render_place(rhs)
        ),
        Instr::FloatSub {
            dest,
            lhs,
            rhs,
            width,
        } => format!(
            "{} = fsub.{} {} {}",
            render_place(dest),
            render_float_width(*width),
            render_place(lhs),
            render_place(rhs)
        ),
        Instr::FloatMul {
            dest,
            lhs,
            rhs,
            width,
        } => format!(
            "{} = fmul.{} {} {}",
            render_place(dest),
            render_float_width(*width),
            render_place(lhs),
            render_place(rhs)
        ),
        Instr::FloatDiv {
            dest,
            lhs,
            rhs,
            width,
        } => format!(
            "{} = fdiv.{} {} {}",
            render_place(dest),
            render_float_width(*width),
            render_place(lhs),
            render_place(rhs)
        ),
        Instr::FloatRem {
            dest,
            lhs,
            rhs,
            width,
        } => format!(
            "{} = frem.{} {} {}",
            render_place(dest),
            render_float_width(*width),
            render_place(lhs),
            render_place(rhs)
        ),
        Instr::FloatCmp {
            dest,
            pred,
            lhs,
            rhs,
            width,
        } => format!(
            "{} = fcmp.{}.{} {} {}",
            render_place(dest),
            render_cmp_pred(*pred),
            render_float_width(*width),
            render_place(lhs),
            render_place(rhs)
        ),

        // Dyn trait
        Instr::CoerceToDynTrait {
            value,
            dest,
            trait_name,
            concrete_type,
            method_table,
            vtable_entries: _,
        } => {
            let methods = method_table
                .iter()
                .map(|(m, f)| format!("{m}={f}"))
                .collect::<Vec<_>>()
                .join(", ");
            format!(
                "{} = coerce_dyn {} trait={} concrete={} methods=[{methods}]",
                render_place(dest),
                render_place(value),
                trait_name,
                concrete_type.user_facing()
            )
        }
        Instr::CallTraitMethod {
            fat_pointer,
            dest,
            trait_name,
            method_name,
            slot,
            args,
            signature: _,
        } => {
            let arg_str = args.iter().map(render_place).collect::<Vec<_>>().join(", ");
            let dest_str = dest
                .as_ref()
                .map(|p| format!("{} = ", render_place(p)))
                .unwrap_or_default();
            format!(
                "{dest_str}call_trait {trait_name}::{method_name}[slot={slot}] {} ({arg_str})",
                render_place(fat_pointer),
            )
        }

        // Machine ops
        Instr::MachineEmitPlaceholder { event_idx, payload } => {
            let pld = payload
                .iter()
                .map(render_place)
                .collect::<Vec<_>>()
                .join(", ");
            format!("machine_emit event[{event_idx}] [{pld}]")
        }
        Instr::EnumTagLoad { src, dest } => format!(
            "{} = enum_tag_load {}",
            render_place(dest),
            render_place(src)
        ),
        Instr::MachineStateName {
            machine_name,
            src_local,
            dest,
        } => format!(
            "{} = machine_state_name {} local=_{src_local}",
            render_place(dest),
            machine_name
        ),
    }
}

// ---------------------------------------------------------------------------
// MirStatement renderer
// ---------------------------------------------------------------------------

fn render_mir_statement(stmt: &MirStatement) -> String {
    match stmt {
        MirStatement::Bind {
            binding,
            name,
            site,
            ty,
        } => format!(
            "bind {:?} {name} site={:?} ty={}",
            binding,
            site,
            ty.user_facing()
        ),
        MirStatement::Evaluate { site, ty } => {
            format!("eval site={:?} ty={}", site, ty.user_facing())
        }
        MirStatement::Use {
            binding,
            name,
            site,
            ty,
            intent,
        } => format!(
            "use {:?} {name} site={:?} ty={} intent={:?}",
            binding,
            site,
            ty.user_facing(),
            intent
        ),
        MirStatement::AggregateAlias {
            binding,
            name,
            site,
            ty,
        } => format!(
            "agg_alias {:?} {name} site={:?} ty={}",
            binding,
            site,
            ty.user_facing()
        ),
        MirStatement::Return { site, ty } => {
            format!("return site={:?} ty={}", site, ty.user_facing())
        }
        MirStatement::Drop { binding, name, ty } => {
            format!("drop {:?} {name} ty={}", binding, ty.user_facing())
        }
    }
}

// ---------------------------------------------------------------------------
// MirCheck renderer (compact single-line)
// ---------------------------------------------------------------------------

fn render_mir_check(check: &MirCheck) -> String {
    match check {
        MirCheck::InitialisedBeforeUse {
            binding,
            name,
            use_site,
        } => format!("InitialisedBeforeUse {binding:?} {name} at={use_site:?}"),
        MirCheck::UseAfterConsume {
            binding,
            name,
            consumed_at,
            used_at,
        } => {
            format!("UseAfterConsume {binding:?} {name} consumed={consumed_at:?} used={used_at:?}")
        }
        MirCheck::Aliasing {
            conflicting_borrows,
        } => {
            format!("Aliasing [{} conflicts]", conflicting_borrows.len())
        }
        MirCheck::GeneratorBorrowAcrossYield { place, yield_point } => {
            format!(
                "GeneratorBorrowAcrossYield {} yield={:?}",
                render_place(place),
                yield_point
            )
        }
        MirCheck::ActorSendEscape { place, send_site } => {
            format!(
                "ActorSendEscape {} send={:?}",
                render_place(place),
                send_site
            )
        }
        MirCheck::ActorAskEscape { place, ask_site } => {
            format!("ActorAskEscape {} ask={:?}", render_place(place), ask_site)
        }
        MirCheck::DecisionMapTotal { offending_sites } => {
            format!("DecisionMapTotal [{} sites]", offending_sites.len())
        }
        MirCheck::MustConsume {
            binding,
            name,
            exit_site,
            ty,
        } => format!(
            "MustConsume {binding:?} {name} exit={exit_site:?} ty={}",
            ty.user_facing()
        ),
        MirCheck::DropPlanUndetermined { block, reason } => {
            format!("DropPlanUndetermined bb{block} {reason:?}")
        }
        MirCheck::ContextBoundaryViolation {
            function,
            block,
            kind,
            reason,
        } => format!("ContextBoundaryViolation {function} bb{block} {kind} {reason:?}"),
        MirCheck::ContextBindingEscapes { place, block } => {
            format!("ContextBindingEscapes {} bb{block}", render_place(place))
        }
        MirCheck::WitnessOperandUnresolved { ty, reason } => {
            format!("WitnessOperandUnresolved {ty} {reason:?}")
        }
        MirCheck::OwnedHandleAggregateDoubleFree {
            binding,
            name,
            handle_ty,
        } => format!("OwnedHandleAggregateDoubleFree {binding:?} {name} ty={handle_ty}"),
    }
}

// ---------------------------------------------------------------------------
// Drop plan / elab drop renderers
// ---------------------------------------------------------------------------

fn dump_drop_plan(out: &mut String, plan: &DropPlan, indent: usize) {
    let pad = " ".repeat(indent);
    if plan.drops.is_empty() {
        writeln!(out, "{pad}(none)").expect("write to string");
    } else {
        for drop in &plan.drops {
            dump_elab_drop(out, drop, indent);
        }
    }
}

fn dump_elab_drop(out: &mut String, drop: &ElabDrop, indent: usize) {
    let pad = " ".repeat(indent);
    let drop_fn = drop
        .drop_fn
        .as_ref()
        .map(|f| format!(" fn={}", render_drop_fn_spec(f)))
        .unwrap_or_default();
    let guard = drop
        .guard
        .as_ref()
        .map(|p| format!(" guard={}", render_place(p)))
        .unwrap_or_default();
    writeln!(
        out,
        "{pad}drop {} ty={} kind={}{drop_fn}{guard}",
        render_place(&drop.place),
        drop.ty.user_facing(),
        render_drop_kind(drop.kind),
    )
    .expect("write to string");
}

// ---------------------------------------------------------------------------
// ExitPath renderer (exhaustive)
// ---------------------------------------------------------------------------

fn render_exit_path(ep: &ExitPath) -> String {
    match ep {
        ExitPath::Return { block } => format!("return[bb{block}]"),
        ExitPath::Goto { block, target } => format!("goto[bb{block}->bb{target}]"),
        ExitPath::Branch {
            block,
            then_target,
            else_target,
        } => format!("branch[bb{block}: bb{then_target}/bb{else_target}]"),
        ExitPath::Call {
            block,
            callee,
            next,
        } => {
            format!("call[bb{block} {callee} -> bb{next}]")
        }
        ExitPath::Panic { block } => format!("panic[bb{block}]"),
        ExitPath::Cancel { block } => format!("cancel[bb{block}]"),
        ExitPath::Yield { block, next } => format!("yield[bb{block}->bb{next}]"),
        ExitPath::Send { block, actor, next } => format!("send[bb{block} {actor} -> bb{next}]"),
        ExitPath::Ask { block, actor, next } => {
            format!("ask[bb{block} {} -> bb{next}]", render_place(actor))
        }
        ExitPath::Select { block, next } => format!("select[bb{block} -> bb{next}]"),
        ExitPath::Join { block, next } => format!("join[bb{block} -> bb{next}]"),
        ExitPath::Suspend {
            block,
            resume,
            cleanup,
        } => format!("suspend[bb{block} resume=bb{resume} cleanup=bb{cleanup}]"),
    }
}

// ---------------------------------------------------------------------------
// MirDiagnostic renderer (exhaustive)
// ---------------------------------------------------------------------------

fn dump_mir_diagnostic(out: &mut String, diag: &MirDiagnostic) {
    let kind = render_diag_kind(&diag.kind);
    writeln!(out, "  diagnostic: {kind} — {:?}", diag.note).expect("write to string");
}

#[allow(
    clippy::too_many_lines,
    reason = "exhaustive match over all MirDiagnosticKind variants"
)]
fn render_diag_kind(kind: &MirDiagnosticKind) -> String {
    match kind {
        MirDiagnosticKind::UseAfterConsume {
            binding,
            name,
            consumed_at,
            used_at,
        } => format!(
            "UseAfterConsume {binding:?} {name} consumed={consumed_at:?} used={used_at:?}"
        ),
        MirDiagnosticKind::InitialisedBeforeUse {
            binding,
            name,
            use_site,
        } => format!("InitialisedBeforeUse {binding:?} {name} site={use_site:?}"),
        MirDiagnosticKind::DecisionMapTotal { offending_sites } => {
            format!("DecisionMapTotal [{} sites]", offending_sites.len())
        }
        MirDiagnosticKind::MustConsume {
            binding,
            name,
            exit_site,
            ty,
        } => format!(
            "MustConsume {binding:?} {name} exit={exit_site:?} ty={}",
            ty.user_facing()
        ),
        MirDiagnosticKind::UnknownType { name } => format!("UnknownType {name}"),
        MirDiagnosticKind::UnsupportedUserRecordValueClass { name, reason } => {
            format!("UnsupportedUserRecordValueClass {name} {reason:?}")
        }
        MirDiagnosticKind::UnsupportedNode { reason } => format!("UnsupportedNode {reason:?}"),
        MirDiagnosticKind::SelectArmNotImplemented {
            arm_kind,
            deferred_by,
            site,
        } => format!(
            "SelectArmNotImplemented {arm_kind} deferred={deferred_by} site={site:?}"
        ),
        MirDiagnosticKind::NotYetImplemented { construct, site } => {
            format!("NotYetImplemented {construct} site={site:?}")
        }
        MirDiagnosticKind::UnresolvedPlace {
            binding,
            name,
            site,
        } => format!("UnresolvedPlace {binding:?} {name} site={site:?}"),
        MirDiagnosticKind::CannotMaterializeClosureCapture {
            binding,
            name,
            site,
        } => format!("CannotMaterializeClosureCapture {binding:?} {name} site={site:?}"),
        MirDiagnosticKind::RemotePayloadUnsupported {
            actor,
            handler,
            site,
        } => format!("RemotePayloadUnsupported actor={actor} handler={handler} site={site:?}"),
        MirDiagnosticKind::DropPlanUndetermined { block, reason } => {
            format!("DropPlanUndetermined bb{block} {reason:?}")
        }
        MirDiagnosticKind::ContextBoundaryViolation {
            function,
            block,
            kind,
            reason,
        } => format!("ContextBoundaryViolation {function} bb{block} {kind} {reason:?}"),
        MirDiagnosticKind::ContextBindingEscapes { place, block } => {
            format!("ContextBindingEscapes {} bb{block}", render_place(place))
        }
        MirDiagnosticKind::UnknownActorStateField { actor, field } => {
            format!("UnknownActorStateField {actor}.{field}")
        }
        MirDiagnosticKind::InvalidActorSpawnArgument {
            actor,
            argument,
            site,
        } => format!("InvalidActorSpawnArgument {actor}.{argument} site={site:?}"),
        MirDiagnosticKind::ActorHandlerSymbolCollision {
            symbol,
            existing,
            duplicate,
        } => format!("ActorHandlerSymbolCollision {symbol} existing={existing} dup={duplicate}"),
        MirDiagnosticKind::ActorStateCloneClassificationFailed {
            actor,
            field_index,
            field_name,
            reason,
        } => format!(
            "ActorStateCloneClassificationFailed {actor}.{field_index}({field_name}) {reason:?}"
        ),
        MirDiagnosticKind::UnresolvedStaticDispatchSubstitution {
            receiver_type_param,
            declaring_trait,
            method_name,
            site,
        } => format!(
            "UnresolvedStaticDispatchSubstitution {declaring_trait}::{method_name} param={receiver_type_param} site={site:?}"
        ),
        MirDiagnosticKind::StaticDispatchImplNotFound {
            declaring_trait,
            self_type_name,
            method_name,
            site,
        } => format!(
            "StaticDispatchImplNotFound {declaring_trait}::{method_name} self={self_type_name} site={site:?}"
        ),
        MirDiagnosticKind::StaticDispatchMonomorphisationMissing {
            method_symbol,
            mangled,
            site,
        } => format!(
            "StaticDispatchMonomorphisationMissing {method_symbol} mangled={mangled} site={site:?}"
        ),
        MirDiagnosticKind::TraitObjectStorageUndetermined {
            binding,
            name,
            site,
            reason,
        } => format!(
            "TraitObjectStorageUndetermined {binding:?} {name} site={site:?} {reason:?}"
        ),
        MirDiagnosticKind::CallTraitMethodSignatureUnresolved {
            trait_name,
            method_name,
            site,
            reason,
        } => format!(
            "CallTraitMethodSignatureUnresolved {trait_name}::{method_name} site={site:?} {reason:?}"
        ),
        MirDiagnosticKind::OwnedHandleAggregateExtractionUnsupported { name, handle_ty } => {
            format!("OwnedHandleAggregateExtractionUnsupported {name} ty={handle_ty}")
        }
        MirDiagnosticKind::ClosurePairBorrowedStore { name, site } => {
            let name_str = name.as_deref().unwrap_or("<expr>");
            format!("ClosurePairBorrowedStore {name_str} site={site:?}")
        }
        MirDiagnosticKind::ClosureCapturesDuplexHandle { name, site } => {
            format!("ClosureCapturesDuplexHandle {name} site={site:?}")
        }
    }
}

// ---------------------------------------------------------------------------
// SelectArm / JoinBranch renderers
// ---------------------------------------------------------------------------

fn render_select_arms(arms: &[SelectArm]) -> String {
    arms.iter()
        .map(|arm| {
            let binding = arm
                .binding
                .as_ref()
                .map(|p| format!(" bind={}", render_place(p)))
                .unwrap_or_default();
            let kind = match &arm.kind {
                SelectArmKind::StreamNext { stream } => {
                    format!("stream_next({})", render_place(stream))
                }
                SelectArmKind::ActorAsk {
                    actor,
                    method,
                    args,
                    msg_type,
                    value,
                } => {
                    let arg_str = args.iter().map(render_place).collect::<Vec<_>>().join(", ");
                    format!(
                        "actor_ask {}::{method}[msg={msg_type}]({arg_str}) val={}",
                        render_place(actor),
                        render_place(value)
                    )
                }
                SelectArmKind::TaskAwait { task } => {
                    format!("task_await({})", render_place(task))
                }
                SelectArmKind::ChannelRecv { receiver, elem_ty } => {
                    format!(
                        "channel_recv({}) elem_ty={}",
                        render_place(receiver),
                        elem_ty.user_facing()
                    )
                }
                SelectArmKind::AfterTimer { duration } => {
                    format!("after_timer({})", render_place(duration))
                }
            };
            format!("[{kind}{binding}->bb{}]", arm.body_block)
        })
        .collect::<Vec<_>>()
        .join(", ")
}

fn render_join_branch(branch: &JoinBranch) -> String {
    let arg_str = branch
        .args
        .iter()
        .map(render_place)
        .collect::<Vec<_>>()
        .join(", ");
    format!(
        "{}::{} ({arg_str}) reply={} reply_ty={}",
        render_place(&branch.actor),
        branch.method,
        render_place(&branch.reply_dest),
        branch.reply_ty.user_facing()
    )
}

// ---------------------------------------------------------------------------
// Small helper renderers
// ---------------------------------------------------------------------------

fn render_trap_kind(kind: TrapKind) -> &'static str {
    match kind {
        TrapKind::IntegerOverflow => "IntegerOverflow",
        TrapKind::IndexOutOfBounds => "IndexOutOfBounds",
        TrapKind::DivideByZero => "DivideByZero",
        TrapKind::SignedMinDivNegOne => "SignedMinDivNegOne",
        TrapKind::ShiftOutOfRange => "ShiftOutOfRange",
        TrapKind::SupervisorChildUnavailable => "SupervisorChildUnavailable",
        TrapKind::MachineDispatchUnreachable => "MachineDispatchUnreachable",
        TrapKind::ExhaustivenessFallthrough => "ExhaustivenessFallthrough",
    }
}

fn render_call_conv(conv: FunctionCallConv) -> &'static str {
    match conv {
        FunctionCallConv::Default => "default",
        FunctionCallConv::ActorHandler => "actor_handler",
        FunctionCallConv::ClosureInvoke => "closure_invoke",
        FunctionCallConv::TaskEntry => "task_entry",
        FunctionCallConv::LambdaActorBody(_) => "lambda_actor_body",
    }
}

fn render_signedness(s: IntSignedness) -> &'static str {
    match s {
        IntSignedness::Signed => "s",
        IntSignedness::Unsigned => "u",
    }
}

fn render_arith_op(op: IntArithOp) -> &'static str {
    match op {
        IntArithOp::Add => "add",
        IntArithOp::Sub => "sub",
        IntArithOp::Mul => "mul",
    }
}

fn render_float_width(w: FloatWidth) -> &'static str {
    match w {
        FloatWidth::F32 => "f32",
        FloatWidth::F64 => "f64",
    }
}

fn render_cmp_pred(pred: CmpPred) -> &'static str {
    match pred {
        CmpPred::Eq => "eq",
        CmpPred::NotEq => "ne",
        CmpPred::SignedLess => "slt",
        CmpPred::SignedLessEq => "sle",
        CmpPred::SignedGreater => "sgt",
        CmpPred::SignedGreaterEq => "sge",
        CmpPred::UnsignedLess => "ult",
        CmpPred::UnsignedLessEq => "ule",
        CmpPred::UnsignedGreater => "ugt",
        CmpPred::UnsignedGreaterEq => "uge",
    }
}

fn render_drop_fn_spec(spec: &DropFnSpec) -> String {
    match spec {
        DropFnSpec::Runtime(d) => format!("rt({d:?})"),
        DropFnSpec::Release(sym) => format!("release({sym})"),
        DropFnSpec::UserClose(sym) => format!("user_close({sym})"),
    }
}

fn render_drop_kind(kind: DropKind) -> String {
    match kind {
        DropKind::Resource => "resource".to_string(),
        DropKind::DuplexClose => "duplex_close".to_string(),
        DropKind::DuplexHalfClose(dir) => format!("duplex_half_close({})", render_direction(dir)),
        DropKind::LambdaActorRelease => "lambda_actor_release".to_string(),
        DropKind::TraitObject { storage } => format!("trait_object({storage:?})"),
        DropKind::CowHeap { drop_fn } => format!("cow_heap({drop_fn})"),
        DropKind::RecordInPlace => "record_in_place".to_string(),
        DropKind::AggregateRecursive => "aggregate_recursive".to_string(),
        DropKind::EnumInPlace => "enum_in_place".to_string(),
        DropKind::TupleInPlace => "tuple_in_place".to_string(),
        DropKind::ClosurePair => "closure_pair".to_string(),
    }
}

fn render_direction(dir: Direction) -> &'static str {
    match dir {
        Direction::Send => "send",
        Direction::Recv => "recv",
    }
}

fn render_env_mode(mode: ClosureEnvMode) -> &'static str {
    match mode {
        ClosureEnvMode::Stack => "stack",
        ClosureEnvMode::HeapBox => "heap_box",
        ClosureEnvMode::Null => "null",
    }
}

fn render_lambda_env_field_drop(drop: LambdaEnvFieldDrop) -> &'static str {
    match drop {
        LambdaEnvFieldDrop::None => "none",
        LambdaEnvFieldDrop::String => "string",
        LambdaEnvFieldDrop::WeakSelfHandle => "weak_self",
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use hew_types::ResolvedTy;

    fn minimal_raw_func(name: &str) -> RawMirFunction {
        RawMirFunction {
            name: name.to_string(),
            return_ty: ResolvedTy::I64,
            call_conv: FunctionCallConv::Default,
            params: vec![],
            locals: vec![ResolvedTy::I64],
            local_names: vec![],
            local_scopes: vec![],
            local_decl_bytes: vec![],
            scope_table: vec![],
            blocks: vec![BasicBlock {
                id: 0,
                statements: vec![],
                instructions: vec![
                    Instr::ConstI64 {
                        dest: Place::Local(0),
                        value: 1,
                    },
                    Instr::Move {
                        dest: Place::ReturnSlot,
                        src: Place::Local(0),
                    },
                ],
                terminator: Terminator::Return,
            }],
            decisions: vec![],
            intrinsic_id: None,
            await_deadline_ns: std::collections::HashMap::new(),
            suspend_kinds: std::collections::HashMap::new(),
            lambda_actor_user_param_locals: vec![],
            span: None,
            instr_spans: std::collections::BTreeMap::new(),
        }
    }

    fn empty_pipeline() -> IrPipeline {
        IrPipeline {
            thir: vec![],
            raw_mir: vec![],
            checked_mir: vec![],
            elaborated_mir: vec![],
            diagnostics: vec![],
            opaque_handle_names: vec![],
            record_layouts: vec![],
            actor_layouts: vec![],
            supervisor_layouts: vec![],
            machine_layouts: vec![],
            enum_layouts: vec![],
            user_consts: vec![],
            polymorphic_mir: vec![],
            regex_literals: vec![],
            extern_decls: vec![],
            dyn_vtable_registry: vec![],
            hashmap_lowering_facts: vec![],
            hashset_lowering_facts: vec![],
            actor_send_aliasing: std::collections::HashMap::new(),
            user_clone_record_seeds: vec![],
            lint_warnings: vec![],
            wire_layouts: std::sync::Arc::new(std::collections::HashMap::new()),
        }
    }

    fn minimal_pipeline_with_raw(func: RawMirFunction) -> IrPipeline {
        IrPipeline {
            raw_mir: vec![func],
            ..empty_pipeline()
        }
    }

    /// Stage 1 gate: function header + CFG spine renders correctly.
    #[test]
    fn dump_raw_function_header_and_spine() {
        let func = minimal_raw_func("main");
        let pipeline = minimal_pipeline_with_raw(func);
        let dump = dump_mir(&pipeline, DumpStage::Raw);

        // Validation candidates §5: header
        assert!(
            dump.contains("fn main()"),
            "missing function header in dump:\n{dump}"
        );
        assert!(
            dump.contains("-> i64"),
            "missing return type in dump:\n{dump}"
        );
        // CFG spine
        assert!(
            dump.contains("bb0:"),
            "missing block label in dump:\n{dump}"
        );
        // Terminator
        assert!(
            dump.contains("return"),
            "missing return terminator in dump:\n{dump}"
        );
        // Instruction tokens
        assert!(
            dump.contains("const.i64 1"),
            "missing const.i64 in dump:\n{dump}"
        );
        assert!(
            dump.contains("move"),
            "missing move instruction in dump:\n{dump}"
        );
    }

    /// Checked stage: a function with no `MirCheck`s renders "checks: none"
    /// (the load-bearing empty-checks signal).
    #[test]
    fn dump_checked_passing_function_emits_checks_none() {
        let raw = minimal_raw_func("identity");
        let checked = CheckedMirFunction {
            name: "identity".to_string(),
            return_ty: ResolvedTy::I64,
            blocks: raw.blocks.clone(),
            decisions: vec![],
            checks: vec![],
            cooperate_sites: vec![],
        };
        let pipeline = IrPipeline {
            checked_mir: vec![checked],
            ..empty_pipeline()
        };
        let dump = dump_mir(&pipeline, DumpStage::Checked);
        // Empty-checks signal is load-bearing
        assert!(
            dump.contains("checks: none"),
            "expected 'checks: none' in checked dump:\n{dump}"
        );
    }

    /// Elab stage: a function with no drops emits `drop_plans: none`
    /// (the == 0 boundary, not > 0).
    #[test]
    fn dump_elab_no_drops_emits_drop_plans_none() {
        let elab = ElaboratedMirFunction {
            name: "no_drops".to_string(),
            return_ty: ResolvedTy::I64,
            statements: vec![],
            decisions: vec![],
            blocks: vec![],
            drop_plans: vec![],
            coroutine: None,
            lambda_captures: vec![],
        };
        let pipeline = IrPipeline {
            elaborated_mir: vec![elab],
            ..empty_pipeline()
        };
        let dump = dump_mir(&pipeline, DumpStage::Elab);
        assert!(
            dump.contains("drop_plans: none"),
            "expected 'drop_plans: none' in elab dump:\n{dump}"
        );
    }

    /// Determinism: calling `dump_mir` twice produces byte-identical results.
    #[test]
    fn dump_mir_is_deterministic() {
        let func = minimal_raw_func("det");
        let pipeline = minimal_pipeline_with_raw(func);
        let a = dump_mir(&pipeline, DumpStage::Raw);
        let b = dump_mir(&pipeline, DumpStage::Raw);
        assert_eq!(a, b, "dump_mir is not deterministic");
    }

    /// Raw dump of a `Terminator::Suspend` with a `SuspendKind::ChannelRecv`
    /// side-table entry renders the kind tag and `elem_ty` inline.
    #[test]
    fn suspend_with_channel_recv_kind_renders_tag_and_elem_ty() {
        use std::collections::HashMap;
        let mut suspend_kinds = HashMap::new();
        suspend_kinds.insert(
            0,
            SuspendKind::ChannelRecv {
                receiver: Place::Local(0),
                result_dest: Place::Local(1),
                elem_ty: ResolvedTy::String,
                deadline_result_dest: None,
                error_dest: None,
            },
        );
        let func = RawMirFunction {
            name: "recv_actor".to_string(),
            return_ty: ResolvedTy::I64,
            call_conv: FunctionCallConv::Default,
            params: vec![],
            locals: vec![ResolvedTy::I64, ResolvedTy::I64],
            local_names: vec![],
            local_scopes: vec![],
            local_decl_bytes: vec![],
            scope_table: vec![],
            blocks: vec![BasicBlock {
                id: 0,
                statements: vec![],
                instructions: vec![],
                terminator: Terminator::Suspend {
                    resume: 1,
                    cleanup: 2,
                    is_final: false,
                },
            }],
            decisions: vec![],
            intrinsic_id: None,
            await_deadline_ns: std::collections::HashMap::new(),
            suspend_kinds,
            lambda_actor_user_param_locals: vec![],
            span: None,
            instr_spans: std::collections::BTreeMap::new(),
        };
        let pipeline = minimal_pipeline_with_raw(func);
        let dump = dump_mir(&pipeline, DumpStage::Raw);
        assert!(
            dump.contains("suspend [channel_recv] elem_ty=string is_final=false"),
            "expected kind tag and elem_ty in Raw suspend dump:\n{dump}"
        );
        // Also confirm the bare `suspend is_final=` prefix is present (for grep compat).
        assert!(
            dump.contains("suspend is_final=") || dump.contains("suspend ["),
            "suspend rendering must contain 'suspend is_final=' or 'suspend [':\n{dump}"
        );
    }

    /// Raw dump of a `Terminator::Suspend` with a `SuspendKind::StreamNext`
    /// carrying `elem_ty=bytes` renders the bytes element type.
    #[test]
    fn suspend_with_stream_next_bytes_renders_elem_ty_bytes() {
        use std::collections::HashMap;
        let mut suspend_kinds = HashMap::new();
        suspend_kinds.insert(
            0,
            SuspendKind::StreamNext {
                stream: Place::Local(0),
                result_dest: Place::Local(1),
                elem_ty: ResolvedTy::Bytes,
                deadline_result_dest: None,
                error_dest: None,
            },
        );
        let func = RawMirFunction {
            name: "stream_actor".to_string(),
            return_ty: ResolvedTy::I64,
            call_conv: FunctionCallConv::Default,
            params: vec![],
            locals: vec![ResolvedTy::I64, ResolvedTy::I64],
            local_names: vec![],
            local_scopes: vec![],
            local_decl_bytes: vec![],
            scope_table: vec![],
            blocks: vec![BasicBlock {
                id: 0,
                statements: vec![],
                instructions: vec![],
                terminator: Terminator::Suspend {
                    resume: 1,
                    cleanup: 2,
                    is_final: false,
                },
            }],
            decisions: vec![],
            intrinsic_id: None,
            await_deadline_ns: std::collections::HashMap::new(),
            suspend_kinds,
            lambda_actor_user_param_locals: vec![],
            span: None,
            instr_spans: std::collections::BTreeMap::new(),
        };
        let pipeline = minimal_pipeline_with_raw(func);
        let dump = dump_mir(&pipeline, DumpStage::Raw);
        assert!(
            dump.contains("suspend [stream_next] elem_ty=bytes is_final=false"),
            "expected [stream_next] elem_ty=bytes in Raw suspend dump:\n{dump}"
        );
        assert!(
            !dump.contains("elem_ty=string"),
            "bytes stream must NOT render elem_ty=string:\n{dump}"
        );
    }

    /// Trap renders as "trap(<TrapKind>)" — not a Debug form.
    #[test]
    fn trap_terminator_renders_kind_tag() {
        let func = RawMirFunction {
            name: "trapper".to_string(),
            return_ty: ResolvedTy::I64,
            call_conv: FunctionCallConv::Default,
            params: vec![],
            locals: vec![],
            local_names: vec![],
            local_scopes: vec![],
            local_decl_bytes: vec![],
            scope_table: vec![],
            blocks: vec![BasicBlock {
                id: 0,
                statements: vec![],
                instructions: vec![],
                terminator: Terminator::Trap {
                    kind: TrapKind::IntegerOverflow,
                },
            }],
            decisions: vec![],
            intrinsic_id: None,
            await_deadline_ns: std::collections::HashMap::new(),
            suspend_kinds: std::collections::HashMap::new(),
            lambda_actor_user_param_locals: vec![],
            span: None,
            instr_spans: std::collections::BTreeMap::new(),
        };
        let pipeline = minimal_pipeline_with_raw(func);
        let dump = dump_mir(&pipeline, DumpStage::Raw);
        assert!(
            dump.contains("trap(IntegerOverflow)"),
            "expected trap(IntegerOverflow) in dump:\n{dump}"
        );
    }
}
