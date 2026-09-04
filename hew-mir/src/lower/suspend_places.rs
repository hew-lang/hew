#[cfg(test)]
use super::*;
#[cfg(not(test))]
use super::{
    base_local, BuiltinType, ClosureEnvFieldOwnership, HirExpr, HirExprKind, HirStmt, HirStmtKind,
    Instr, Place, ResolvedTy, SelectArm, SelectArmKind, SuspendKind, Terminator,
};
use crate::{raw_virtual_operation_class, RawVirtualClass};

/// The *source* (read) operands of an instruction — every `Place` whose
/// value the instruction consumes, excluding the destination(s) it writes.
///
/// W5-011 P3 (fail-closed sole-owner derivation). A heap-owning `string`
/// local whose backing local surfaces here has had its pointer copied or
/// aliased out of its slot (a `Move` src, a call/runtime/aggregate/variant
/// operand, a payload, a field store, …) and is therefore NOT the sole
/// owner of its buffer at scope exit. The match is intentionally
/// exhaustive with no wildcard: a future `Instr` variant cannot be added
/// without classifying its operands here, so a new alias-producing
/// instruction auto-excludes its sources from scope-exit drop (it can
/// never silently re-open a double-free). When a place's role is
/// ambiguous, it is classified as a source (over-exclusion leaks, never
/// double-frees). Mirrors `instr_places` structurally but drops the
/// write-dest from each arm.
#[allow(
    clippy::too_many_lines,
    clippy::match_same_arms,
    reason = "flat exhaustive match over every Instr variant; the line \
              count is the variant count, not nesting — the exhaustiveness \
              is the fail-closed guarantee. Arms with identical bodies are \
              kept separate per-variant so a future variant cannot be folded \
              into an existing source-classification by accident"
)]
#[must_use]
pub fn instr_source_places(instr: &Instr) -> Vec<Place> {
    match instr {
        // No operands at all.
        Instr::Value(operation) => match raw_virtual_operation_class(operation) {
            Some(
                RawVirtualClass::Integer | RawVirtualClass::Bool | RawVirtualClass::Tuple,
            )
            | None => Vec::new(),
        },
        Instr::OwnershipEvent(_)
        | Instr::EnterContext
        | Instr::ExitContext
        | Instr::CheckCancellation
        // Raw value operations are deliberately disjoint from `Place`; the
        // ReturnAbi materialization has a virtual source rather than a
        // place-source, so neither can alias a storage owner out of scope.
        | Instr::MaterializeValue { .. }
        | Instr::ContextField { .. }
        | Instr::ConstI64 { .. }
        | Instr::StringLit { .. }
        | Instr::BytesLit { .. }
        | Instr::ConstGlobalLoad { .. }
        | Instr::FloatLit { .. }
        | Instr::CharLit { .. }
        | Instr::UnitLit { .. }
        | Instr::DurationLit { .. }
        // A field load out of the hidden actor-state pointer reads no
        // operand `Place` — the source is the implicit context arg.
        | Instr::ActorStateFieldLoad { .. }
        // A payload-slot neutralize stores a constant null — no source operand.
        | Instr::NeutralizePayloadSlot { .. }
        | Instr::AggregateProjectionNeutralize { .. } => Vec::new(),
        Instr::InteriorMutationCommit { place } => vec![*place],
        // Binary arithmetic / comparison: both operands are sources, the
        // dest (and any overflow-flag dest) is a write.
        Instr::IntAdd { lhs, rhs, .. }
        | Instr::IntSub { lhs, rhs, .. }
        | Instr::IntMul { lhs, rhs, .. }
        | Instr::IntArithCheckedOption { lhs, rhs, .. }
        | Instr::IntArithSaturating { lhs, rhs, .. }
        | Instr::IntDiv { lhs, rhs, .. }
        | Instr::IntRem { lhs, rhs, .. }
        | Instr::IntBitAnd { lhs, rhs, .. }
        | Instr::IntBitOr { lhs, rhs, .. }
        | Instr::IntBitXor { lhs, rhs, .. }
        | Instr::IntShl { lhs, rhs, .. }
        | Instr::IntShr { lhs, rhs, .. }
        | Instr::IntCmp { lhs, rhs, .. }
        | Instr::FloatCmp { lhs, rhs, .. }
        | Instr::IdentityCompare { lhs, rhs, .. }
        | Instr::IntArithChecked { lhs, rhs, .. }
        | Instr::FloatAdd { lhs, rhs, .. }
        | Instr::FloatSub { lhs, rhs, .. }
        | Instr::FloatMul { lhs, rhs, .. }
        | Instr::FloatDiv { lhs, rhs, .. }
        | Instr::FloatRem { lhs, rhs, .. } => vec![*lhs, *rhs],
        Instr::CancellationTokenIsCancelled { token, .. } => vec![*token],
        Instr::RcIntrinsic {
            op: hew_types::RcIntrinsicOp::New | hew_types::RcIntrinsicOp::Set,
            value,
            ..
        } => {
            value.iter().copied().collect()
        }
        Instr::RcIntrinsic { .. } => Vec::new(),
        // `.next()` borrows the generator handle — it does NOT alias it out.
        // The handle stays the sole owner of its heap companion pointer so its
        // scope-exit drop fires `hew_gen_coro_destroy` exactly once. Excluding
        // `ctx` here (classifying it as a source) would suppress that drop and
        // leak the generator's coro frame + heap companion.
        Instr::GeneratorNext { .. } => vec![],
        // The wire codec only READS its operand (the serialize thunk walks the
        // value; the deserialize thunk reads the bytes) — neither copies the
        // operand's heap pointer out of its slot nor frees it. The caller's
        // binding stays the sole owner, so the operand is excluded from sources
        // and its scope-exit drop is preserved (mirrors `GeneratorNext`'s
        // borrowed `ctx`).
        Instr::WireCodec { .. } => vec![],
        // `RecordCloneInplace` reads `src` (borrows it; does not consume).
        // The original `src` binding stays live after the clone.
        Instr::RecordCloneInplace { src, .. } => vec![*src],
        // `EnumCloneInplace` likewise borrows `src` (non-consuming read).
        Instr::EnumCloneInplace { src, .. } => vec![*src],
        // Snapshot cloning borrows the source and writes a fresh non-aliasing
        // destination; it must not suppress the sender original's drop.
        Instr::ValueSnapshotClone { .. } => vec![],
        Instr::ValueSnapshotDrop { value, guard, .. } => {
            let mut places = vec![*value];
            places.extend(*guard);
            places
        }
        Instr::BoolNot { operand, .. }
        | Instr::FloatNeg { operand, .. }
        | Instr::IntBitNot { operand, .. }
        | Instr::IntNegChecked { operand, .. } => vec![*operand],
        // The src is read into the dest; the dest is a write.
        Instr::Move { src, .. } => vec![*src],
        // A retain reads the triple but transfers no ownership out of the
        // current scope; the matching co-owner mint is classified separately.
        Instr::BytesRetain { .. } | Instr::StringRetain { .. } => Vec::new(),
        Instr::NumericCast { src, .. }
        | Instr::SaturatingWidthCast { src, .. }
        | Instr::TryWidthCast { src, .. } => vec![*src],
        // A Drop reads the place it releases.
        Instr::Drop { place, .. } => vec![*place],
        Instr::AggregateOverwriteRelease {
            old, replacement, ..
        } => vec![*old, *replacement],
        // Witness size/align read no operand (the type is static metadata,
        // not a runtime place); drop-glue reads the place it releases; a
        // witness move reads its source.
        Instr::WitnessSizeOf { .. } | Instr::WitnessAlignOf { .. } => Vec::new(),
        Instr::WitnessDropGlue { place, .. } => vec![*place],
        Instr::WitnessMove { src, .. } => vec![*src],
        Instr::CallRuntimeAbi(call) => call.args().to_vec(),
        Instr::AutoLockAcquire { lock } | Instr::AutoLockRelease { lock } => vec![*lock],
        // Aggregate construction: every field/element value is shared into
        // the new aggregate; the dest is a write.
        Instr::RecordInit { fields, .. } => fields.iter().map(|(_, p)| *p).collect(),
        // Closure env construction carries an ownership manifest. Stack-env
        // fields are borrow-only and must not suppress source drops; heap fields
        // that own a moved capture are the source-transfer reads.
        Instr::ClosureEnvInit { fields, .. } => fields
            .iter()
            .filter(|field| field.ownership == ClosureEnvFieldOwnership::OwnsMoved)
            .map(|field| field.src)
            .collect(),
        Instr::TupleConstruct { elements, .. } => elements.clone(),
        // Field loads: the aggregate is the source; the dest is a write
        // (and, for the interior-aliasing loads, a projection seed — see
        // `projection_alias_dest`).
        Instr::RecordFieldLoad { record, .. } => vec![*record],
        Instr::RecordFieldDrop { record, .. } => vec![*record],
        // FieldDropInPlace reads its base aggregate (GEP + in-place field
        // release); like `RecordFieldDrop` it produces no dest.
        Instr::FieldDropInPlace { base, .. } => vec![*base],
        Instr::TupleFieldLoad { tuple, .. } => vec![*tuple],
        Instr::ClosureEnvFieldLoad { env, .. } => vec![*env],
        // Field stores: both the target aggregate and the stored value are
        // read (the aggregate stays live; the value is shared into it).
        Instr::RecordFieldStore { record, src, .. } => vec![*record, *src],
        Instr::ActorStateFieldStore { src, .. } => vec![*src],
        // Closure-env write-back reads both the env pointer (the aggregate
        // stays live) and the value being stored into it.
        Instr::ClosureEnvFieldStore { env, src, .. } => vec![*env, *src],
        Instr::MakeClosure { env, .. } => vec![*env],
        Instr::CallClosure { callee, args, .. } => {
            let mut places = vec![*callee];
            places.extend(args.iter().copied());
            places
        }
        Instr::SpawnTaskDirect { task, .. } => vec![*task],
        Instr::SpawnTaskClosure { task, env, .. } => vec![*task, *env],
        Instr::SpawnActor {
            state, init_args, ..
        } => {
            let mut places = Vec::new();
            if let Some(state) = state {
                places.push(*state);
            }
            places.extend(init_args.iter().copied());
            places
        }
        Instr::CoerceToDynTrait { value, .. } => vec![*value],
        Instr::CallTraitMethod {
            fat_pointer, args, ..
        } => {
            let mut places = vec![*fat_pointer];
            places.extend(args.iter().copied());
            places
        }
        Instr::MachineEmitPlaceholder { payload, .. } => payload.clone(),
        Instr::EnumTagLoad { src, .. } => vec![*src],
        Instr::MachineStateName { src_local, .. } => vec![Place::Local(*src_local)],
        Instr::MachineEmitTake { event_tag, .. } => vec![*event_tag],
    }
}
/// The *source* (read) operands of a terminator — every `Place` whose
/// value crosses the block edge as a read, excluding the slot the
/// terminator writes (a `Call`'s `dest`, an `Ask`'s `reply_dest`, a
/// `Select` arm's binding). Same fail-closed exhaustiveness contract as
/// [`instr_source_places`]: a string surfacing here as an operand (a
/// returned value moved to `ReturnSlot` earlier, an actor `Send`/`Ask`
/// payload, a `select` arm payload, a `yield` value) is aliased out and
/// excluded from scope-exit drop.
/// True when a terminator is a SUSPEND CARRIER — the structural fact codegen's
/// `is_coroutine` / `has_suspend` read to lower a function as a
/// `presplitcoroutine`. The single authority both the MIR closure-call
/// discriminator and the codegen coroutine boundary derive from, so they can
/// never disagree (`container-abi-ctor-op-agreement`).
#[must_use]
pub fn terminator_is_suspend_carrier(term: &Terminator) -> bool {
    matches!(
        term,
        // The ten pure-{resume,cleanup} carriers all collapse to the bare
        // `Suspend`; `SuspendingScopeDeadline` and `SuspendingSelect` keep their
        // distinct terminators (extra CFG edges) but are still suspend carriers.
        Terminator::Suspend { .. }
            | Terminator::SuspendingScopeDeadline { .. }
            | Terminator::SuspendingSelect { .. }
    )
}
/// Source operands the per-arm payload of a `select{}` reads across its block
/// edge (the same for `Terminator::Select` and `Terminator::SuspendingSelect`,
/// whose `arms` payloads are identical). Each arm's `binding` is the slot the
/// won value is written into — a write, not a source.
#[must_use]
fn select_arm_source_places(arms: &[SelectArm]) -> Vec<Place> {
    let mut places = Vec::new();
    for arm in arms {
        match &arm.kind {
            SelectArmKind::StreamNext { stream } => places.push(*stream),
            SelectArmKind::ActorAsk {
                actor, args, value, ..
            } => {
                places.push(*actor);
                places.extend(args.iter().copied());
                places.push(*value);
            }
            SelectArmKind::TaskAwait { task } => places.push(*task),
            SelectArmKind::ChannelRecv { receiver, .. } => places.push(*receiver),
            SelectArmKind::AfterTimer { duration } => places.push(*duration),
        }
    }
    places
}
/// Source operands a collapsed suspension carrier reads across its block edge,
/// recovered from the [`SuspendKind`] side-table payload. This is the
/// bare-[`Terminator::Suspend`] analogue of the per-carrier arms in
/// [`terminator_source_places`]: each variant returns the SAME places the
/// dedicated `Suspending*` terminator did (the readiness sources / forwarded
/// args; the result/reply/error dests are write slots bound on the resume edge,
/// not sources). The exhaustive match is the fail-closed guarantee that a new
/// `SuspendKind` variant forces a source-classification decision.
#[must_use]
pub fn suspend_kind_source_places(kind: &SuspendKind) -> Vec<Place> {
    match kind {
        // The receiver descriptor and payload are reads. A ChildRef carrier
        // additionally reads the two extracted stable-role words used by
        // owner-scoped submission.
        SuspendKind::ActorSend {
            actor,
            stable_role,
            value,
            ..
        }
        | SuspendKind::Ask {
            actor,
            stable_role,
            value,
            ..
        } => {
            let mut places = vec![*actor, *value];
            if let Some(role) = stable_role {
                places.extend([role.supervisor_token, role.slot_index]);
            }
            places
        }
        // `conn` is the read source; `result_dest` is a resume-edge write.
        SuspendKind::Read { conn, .. } => vec![*conn],
        // `listener` is the accept source; `result_dest` is a resume-edge write.
        SuspendKind::Accept { listener, .. } => vec![*listener],
        // `stream` is the recv source; `result_dest` is a resume-edge write.
        SuspendKind::StreamNext { stream, .. } => vec![*stream],
        // `receiver` is the recv source; `result_dest` is a resume-edge write.
        SuspendKind::ChannelRecv { receiver, .. } => vec![*receiver],
        // `sink` + `value` are the send sources.
        SuspendKind::StreamSend { sink, value } => vec![*sink, *value],
        // The closure pair (`callee`) + forwarded `args` are reads; `result_dest`
        // is a completion-edge write.
        SuspendKind::CallClosure { callee, args, .. } => {
            let mut places = Vec::with_capacity(args.len() + 1);
            places.push(*callee);
            places.extend(args.iter().copied());
            places
        }
        // `actor` + `value` + `timeout_ms` are reads; the dests are writes.
        SuspendKind::RemoteAsk {
            actor,
            value,
            timeout_ms,
            ..
        } => vec![*actor, *value, *timeout_ms],
        // `scope` (scope-scoped observer registration) + `task` (await source)
        // are reads; `result_dest` is a resume-edge write.
        SuspendKind::TaskAwait { scope, task, .. } => vec![*scope, *task],
        // `sup_place` (the supervisor PID) is the restart-observer registration
        // source; `result_dest` is a resume-edge write (re-fetched handle).
        SuspendKind::RestartWait { sup_place, .. } => vec![*sup_place],
        // `duration_ns` is the deadline source (nanoseconds); the resume edge binds nothing.
        SuspendKind::Sleep { duration_ns } => vec![*duration_ns],
        // `instant_ns` is the wakeup time source; the resume edge binds nothing.
        SuspendKind::SleepUntil { instant_ns } => vec![*instant_ns],
    }
}
#[allow(
    clippy::match_same_arms,
    reason = "exhaustive match over every Terminator variant; Send and Ask \
              share an operand shape but are kept as separate arms so a \
              future terminator cannot be folded into an existing \
              source-classification by accident — the exhaustiveness is the \
              fail-closed guarantee"
)]
#[must_use]
pub fn terminator_source_places(
    term: &Terminator,
    suspend_kind: Option<&SuspendKind>,
) -> Vec<Place> {
    match term {
        Terminator::Return
        | Terminator::Unreachable
        | Terminator::Goto { .. }
        | Terminator::Trap { .. } => Vec::new(),
        Terminator::Branch { cond, .. } => vec![*cond],
        Terminator::Call { args, .. } => args.clone(),
        Terminator::Yield { value, .. } => vec![*value],
        // `dest` is the handle slot the generator is written into (a write);
        // `body_fn` is a static symbol. The typed env plan's place is the
        // synthetic RecordInit shell read and consumed by construction.
        Terminator::MakeGenerator { env, .. } => env.iter().map(|plan| plan.place).collect(),
        // Lambda-actor construction: `dest` is written; `body_fn` and
        // `state_drop_fn` are static symbols. The capture env (when
        // present) is READ — codegen heap-boxes its bytes — so it is a
        // source operand.
        Terminator::MakeLambdaActor { env, .. } => env.iter().copied().collect(),
        // A bare `Suspend` reads what its collapsed carrier read: the
        // [`SuspendKind`] side-table payload supplies the readiness sources /
        // forwarded args (`suspend_kind_source_places`). A `Suspend` with NO
        // side-table entry is a generator / synthetic substrate suspend whose
        // value channel is the coro frame out-pointer (not a `Place`), so it
        // reads nothing across the block edge.
        Terminator::Suspend { .. } => {
            suspend_kind.map_or_else(Vec::new, suspend_kind_source_places)
        }
        Terminator::Send {
            actor,
            stable_role,
            value,
            ..
        }
        // `reply_dest` is the slot the reply is written into — a write, not
        // a source.
        | Terminator::Ask {
            actor,
            stable_role,
            value,
            ..
        } => {
            let mut places = vec![*actor, *value];
            if let Some(role) = stable_role {
                places.extend([role.supervisor_token, role.slot_index]);
            }
            places
        }
        // The ten pure-{resume,cleanup} suspension carriers collapsed onto the
        // bare `Suspend` arm above, which recovers their source operands from the
        // `SuspendKind` side-table via `suspend_kind_source_places`.
        Terminator::RemoteAsk {
            actor,
            value,
            timeout_ms,
            ..
        } => vec![*actor, *value, *timeout_ms],
        // `SuspendingScopeDeadline` reads `scope` (the children it joins/cancels)
        // + `duration_ms` (the deadline source); the timeout body block is a CFG
        // edge, not an operand.
        Terminator::SuspendingScopeDeadline {
            scope, duration_ms, ..
        } => vec![*scope, *duration_ms],
        // The suspending select carries the identical `arms` payload, so its
        // per-arm source operands are read the same way as the blocking select.
        Terminator::Select { arms, .. } | Terminator::SuspendingSelect { arms, .. } => {
            select_arm_source_places(arms)
        }
        Terminator::Join { branches, .. } => {
            let mut places = Vec::new();
            for branch in branches {
                places.push(branch.actor);
                places.extend(branch.args.iter().copied());
                places.push(branch.value);
                // `branch.reply_dest` is the slot the reply is written
                // into — a write, not a source.
            }
            places
        }
    }
}
/// True when the scrutinee carries the `VecIter::next` desugar's synthetic
/// `let __hew_iter_value_N = iter.vec.get(iter.idx)` clone-out read.
///
/// This is intentionally keyed on the typed Vec/Get family and the fixed
/// synthetic binding name, not merely on `hew_vec_get_clone`: a source-level
/// `match xs.get(i)` is also a fresh result but is not an iterator frame whose
/// payload follows the per-iteration body/edge lifecycle.
pub(super) fn hir_expr_contains_synthetic_vec_get_clone(expr: &HirExpr) -> bool {
    match &expr.kind {
        HirExprKind::Block(block) => {
            block
                .statements
                .iter()
                .any(hir_stmt_is_synthetic_vec_get_clone)
                || block
                    .tail
                    .as_deref()
                    .is_some_and(hir_expr_contains_synthetic_vec_get_clone)
        }
        HirExprKind::If {
            condition,
            then_expr,
            else_expr,
        } => {
            hir_expr_contains_synthetic_vec_get_clone(condition)
                || hir_expr_contains_synthetic_vec_get_clone(then_expr)
                || else_expr
                    .as_deref()
                    .is_some_and(hir_expr_contains_synthetic_vec_get_clone)
        }
        _ => false,
    }
}
fn hir_stmt_is_synthetic_vec_get_clone(stmt: &HirStmt) -> bool {
    matches!(
        &stmt.kind,
        HirStmtKind::Let(binding, Some(value))
            if binding.name.starts_with("__hew_iter_value_")
                && matches!(
                    value.kind,
                    HirExprKind::ResolvedImplCall {
                        target_family:
                            hew_types::MethodTargetFamily::Vec(hew_types::VecMethod::Get),
                        ref target_symbol,
                        ..
                    } if matches!(target_symbol.as_str(), "hew_vec_get_clone" | "hew_vec_take_owned")
                )
    )
}
pub(super) fn place_refs_local(place: Place, local: u32) -> bool {
    base_local(place) == Some(local)
}
/// True when every reference to `local` inside `args` is a borrow `contract`
/// proves — a borrowing string-argument position (`hew_string_concat`,
/// `print`/`println`, …), the collection/Vec/bytes receiver slot
/// (`args[0]`; a reference anywhere in the by-value tail `args[1..]` still
/// counts as unproven), or the all-args-borrow bytes contract
/// (`hew_bytes_append`: receiver + unpacked source triple are every-position
/// read-only borrows). `true` when `local` does not appear in `args` at all.
///
/// Shared by the `Terminator::Call` arm of
/// [`generator_yield_terminator_escapes`] and the `Instr::CallRuntimeAbi` arm
/// of [`generator_yield_instr_escapes`] — the SAME closed, positive-
/// membership ownership-contract table
/// `binder_read_is_borrow_safe_{instr,terminator}` consults, never a
/// structural "any call is a borrow" rule. A callee outside the list —
/// including a directly-resolved user Hew function — is unproven: it may
/// forward the argument back out as its own return value (an
/// identity/pass-through helper: validators, `.trim()`-style wrappers,
/// decorators), which would let the generator/recv-yield exit-edge ledger
/// (`return`/`break`/`continue`) free the buffer the call just handed to its
/// caller — a silent use-after-free (GitHub issue #2412: `return
/// wrap(v)`). Fail-closed: unproven is NOT borrow-safe, never re-admitted
/// (LESSONS `boundary-fail-closed`). WHEN OBSOLETE: the COW retain-on-share
/// spine (A240) replaces this leak-on-uncertainty posture with an exact
/// retain, and every `Call` argument can be admitted unconditionally.
pub(super) fn call_args_borrow_safe(
    contract: crate::runtime_symbols::CalleeOwnershipContract,
    args: &[Place],
    local: u32,
) -> bool {
    let refs = |p: &Place| place_refs_local(*p, local);
    if !args.iter().any(refs) {
        return true;
    }
    // `hew_bytes_append` borrows the receiver AND the unpacked source triple —
    // every argument position is a read-only borrow, none consumed — so a
    // reference to `local` anywhere in the argument list is borrow-safe (it
    // does not escape via the call). Mirrors the exemption the composite-drop
    // provers `binder_read_is_borrow_safe_terminator`/`_instr` already carry;
    // without it, threading a for-await/generator loop variable into the
    // `hew_bytes_append` source triple made the exit-edge release retract and
    // leaked that value on a `return`/`break`/`continue` exit (#2474).
    if contract.borrows_all_bytes_args() {
        return true;
    }
    let receiver_borrow_safe = (contract.borrows_vec_receiver()
        || contract.borrows_collection_receiver()
        || contract.borrows_bytes_receiver())
        && !args.iter().skip(1).any(refs);
    contract.borrows_string_call_args() || receiver_borrow_safe
}
/// True when an instruction transfers ownership of `local` out of its slot
/// (so a body-end drop of the binding would be unsound). A fresh, solely-owned
/// generator-yielded value escapes only via:
///
///   - a `Move` out of its slot into another local (ownership transfer / rebind),
///   - a store into a surviving aggregate (`RecordInit`, `RecordFieldStore`,
///     `TupleConstruct`, `ActorStateFieldStore`, `MakeClosure`,
///     `CoerceToDynTrait`),
///   - a spawn capture (`SpawnActor`, `SpawnTaskDirect`, `SpawnTaskClosure`),
///   - a re-`Drop` (the binding already has a release scheduled).
///
/// A borrowing read — a `.len()`-style getter call, a runtime-ABI argument, an
/// arithmetic/comparison operand — does NOT transfer ownership, so it does not
/// escape. The match is exhaustive (no wildcard) so a future `Instr` variant
/// forces an explicit escape/borrow classification rather than silently
/// defaulting to "safe to drop" (which could re-open a double-free).
#[allow(
    clippy::match_same_arms,
    clippy::too_many_lines,
    reason = "exhaustive match over every Instr variant; several ownership-transfer \
              shapes (Move/WitnessMove, the aggregate stores) and every borrow shape \
              share a body, but are kept as separate arms so a future Instr cannot be \
              folded into an existing classification by accident — the exhaustiveness \
              is the fail-closed guarantee against re-opening a double-free"
)]
pub(super) fn generator_yield_instr_escapes(instr: &Instr, local: u32) -> bool {
    let refs = |p: Place| place_refs_local(p, local);
    match instr {
        // Ownership-transferring shapes: the binding's pointer ends up in a
        // location that outlives the body (another local / an aggregate / a
        // spawned entity), or it is re-dropped.
        Instr::Move { src, .. } => refs(*src),
        Instr::WitnessMove { src, .. } => refs(*src),
        // A `Drop` of the binding is a RELEASE, not an ownership escape — and a
        // `Drop` carrying a `drop_fn` is one the consuming-body lowering itself
        // emitted: the break/continue-edge yield-value free
        // (`emit_generator_yield_value_drops_for_break_continue`). That edge and
        // the body-end fall-through drop are mutually exclusive in the CFG, so
        // counting our own break/continue free as an "escape" here would wrongly
        // suppress the fall-through body-end drop and leak the non-break
        // iterations (verified regression: j_unbounded 0 -> 100). The
        // null-after-free on every inline drop keeps a structurally-reachable
        // double drop a no-op. A `Drop` with NO `drop_fn` is a move-checker /
        // generic release whose double-fire is not guarded, so it still counts
        // as a re-drop escape (fail-closed).
        Instr::Drop {
            place,
            drop_fn: None,
            ..
        } => refs(*place),
        Instr::Drop {
            drop_fn: Some(_), ..
        } => false,
        Instr::AggregateOverwriteRelease { .. } => false,
        Instr::RecordInit { fields, .. } => fields.iter().any(|(_, p)| refs(*p)),
        Instr::ClosureEnvInit { fields, .. } => fields
            .iter()
            .filter(|field| field.ownership == ClosureEnvFieldOwnership::OwnsMoved)
            .any(|field| refs(field.src)),
        Instr::TupleConstruct { elements, .. } => elements.iter().any(|p| refs(*p)),
        Instr::RecordFieldStore { src, .. } => refs(*src),
        Instr::ActorStateFieldStore { src, .. } => refs(*src),
        // A closure-env write-back stores `src` into the env; like the other
        // field stores it escapes the local iff `src` is the tracked local.
        Instr::ClosureEnvFieldStore { src, .. } => refs(*src),
        Instr::MakeClosure { env, .. } => refs(*env),
        Instr::CoerceToDynTrait { value, .. } => refs(*value),
        Instr::SpawnActor {
            state, init_args, ..
        } => state.is_some_and(&refs) || init_args.iter().any(|p| refs(*p)),
        Instr::SpawnTaskDirect { task, .. } => refs(*task),
        Instr::SpawnTaskClosure { task, env, .. } => refs(*task) || refs(*env),
        // A `CallRuntimeAbi` argument is a borrow only when its callee symbol
        // is on the closed ownership-contract list [`call_args_borrow_safe`]
        // consults — see its doc comment. Any callee outside that list is
        // unproven and counts as an escape (fail-closed).
        Instr::CallRuntimeAbi(call) => !call_args_borrow_safe(
            crate::runtime_symbols::callee_ownership_contract(call.symbol()),
            call.args(),
            local,
        ),
        // A closure or trait-method call is dynamic dispatch — no
        // compile-time symbol to consult an ownership contract for. Any
        // reference to `local` (the callee/receiver pair itself, or an
        // argument) is unproven and counts as an escape (fail-closed; same
        // rationale as the `CallRuntimeAbi` arm above).
        Instr::CallClosure { callee, args, .. } => {
            refs(*callee) || args.iter().any(|p| refs(*p))
        }
        Instr::CallTraitMethod {
            fat_pointer, args, ..
        } => refs(*fat_pointer) || args.iter().any(|p| refs(*p)),
        // Borrowing reads — a context/cancellation query or an arithmetic
        // operand does not retain the yielded value. These do NOT escape it.
        Instr::OwnershipEvent(_)
        | Instr::EnterContext
        | Instr::ExitContext
        | Instr::CheckCancellation
        | Instr::Value(_)
        | Instr::MaterializeValue { .. }
        | Instr::ContextField { .. }
        | Instr::ConstI64 { .. }
        | Instr::IntAdd { .. }
        | Instr::IntSub { .. }
        | Instr::IntMul { .. }
        | Instr::IntDiv { .. }
        | Instr::IntRem { .. }
        | Instr::IntBitAnd { .. }
        | Instr::IntBitOr { .. }
        | Instr::IntBitXor { .. }
        | Instr::BoolNot { .. }
        | Instr::IntNegChecked { .. }
        | Instr::FloatNeg { .. }
        | Instr::IntBitNot { .. }
        | Instr::IntShl { .. }
        | Instr::IntShr { .. }
        | Instr::IntArithChecked { .. }
        | Instr::IntArithCheckedOption { .. }
        | Instr::IntArithSaturating { .. }
        | Instr::IntCmp { .. }
        | Instr::IdentityCompare { .. }
        | Instr::CancellationTokenIsCancelled { .. }
        | Instr::RcIntrinsic { .. }
        | Instr::GeneratorNext { .. }
        | Instr::WireCodec { .. }
        | Instr::BytesRetain { .. }
        | Instr::StringRetain { .. }
        | Instr::NumericCast { .. }
        | Instr::SaturatingWidthCast { .. }
        | Instr::TryWidthCast { .. }
        | Instr::AutoLockAcquire { .. }
        | Instr::AutoLockRelease { .. }
        | Instr::WitnessSizeOf { .. }
        | Instr::WitnessAlignOf { .. }
        | Instr::WitnessDropGlue { .. }
        | Instr::StringLit { .. }
        | Instr::BytesLit { .. }
        | Instr::ConstGlobalLoad { .. }
        | Instr::RecordFieldLoad { .. }
        | Instr::RecordFieldDrop { .. }
        // FieldDropInPlace is an interior in-place field release (uses its
        // base, no dest, no alias) — like `RecordFieldDrop` it moves no
        // ownership out of the frame.
        | Instr::FieldDropInPlace { .. }
        | Instr::TupleFieldLoad { .. }
        | Instr::ClosureEnvFieldLoad { .. }
        | Instr::ActorStateFieldLoad { .. }
        | Instr::FloatLit { .. }
        | Instr::CharLit { .. }
        | Instr::UnitLit { .. }
        | Instr::DurationLit { .. }
        | Instr::FloatAdd { .. }
        | Instr::FloatSub { .. }
        | Instr::FloatMul { .. }
        | Instr::FloatDiv { .. }
        | Instr::FloatRem { .. }
        | Instr::FloatCmp { .. }
        | Instr::MachineEmitPlaceholder { .. }
        | Instr::EnumTagLoad { .. }
        | Instr::MachineStateName { .. }
        | Instr::MachineEmitTake { .. }
        // RecordCloneInplace borrows src (non-consuming read); it does not
        // transfer ownership of any local out of the frame.
        | Instr::RecordCloneInplace { .. }
        // EnumCloneInplace has the same non-consuming-read semantics.
        | Instr::EnumCloneInplace { .. }
        | Instr::ValueSnapshotClone { .. }
        | Instr::ValueSnapshotDrop { .. } => false,
        // Neutralizing a projection normally suppresses a body-end root drop.
        // Minted call carriers apply their narrower shell-safety authority in
        // the caller and may exempt a null-safe projected transfer.
        Instr::NeutralizePayloadSlot { place, .. } => refs(*place),
        Instr::AggregateProjectionNeutralize { root, .. } => refs(*root),
        Instr::InteriorMutationCommit { place } => refs(*place),
    }
}
/// True when a terminator transfers ownership of `local` out of the body: a
/// return moves it to the caller, a re-yield hands it back to a consumer, an
/// actor send/ask/select transfers it into the message. A `Call` argument is a
/// borrow ONLY when the callee is on the closed ownership-contract borrow
/// list — see [`generator_yield_terminator_escapes`]'s `Terminator::Call` arm.
#[allow(
    clippy::match_same_arms,
    reason = "exhaustive match over every Terminator variant; the non-escaping \
              control-flow terminators and `Return` share a `false` body but are kept \
              separate so a future terminator forces an explicit classification — the \
              exhaustiveness is the fail-closed guarantee"
)]
/// Does a collapsed suspension carrier (recovered from the [`SuspendKind`]
/// side-table) transfer a generator-yielded `local` OUT across the suspend?
/// Only the value-moving carriers (`Ask` / `StreamSend` / `RemoteAsk` move their
/// `value` payload into the message / channel / wire) can; the handle-read and
/// result-binding carriers never carry a yielded value. The bare-`Suspend`
/// analogue of the per-carrier arms in [`generator_yield_terminator_escapes`].
fn suspend_kind_yield_escapes(kind: &SuspendKind, local: u32) -> bool {
    match kind {
        SuspendKind::ActorSend { value, .. }
        | SuspendKind::Ask { value, .. }
        | SuspendKind::StreamSend { value, .. }
        | SuspendKind::RemoteAsk { value, .. } => place_refs_local(*value, local),
        // Handle reads + result-binding carriers transfer no yielded value out.
        SuspendKind::Read { .. }
        | SuspendKind::Accept { .. }
        | SuspendKind::StreamNext { .. }
        | SuspendKind::ChannelRecv { .. }
        | SuspendKind::CallClosure { .. }
        | SuspendKind::TaskAwait { .. }
        // `await_restart` reads the supervisor PID and binds the re-fetched
        // handle on resume; it moves no yielded value across the suspend.
        | SuspendKind::RestartWait { .. }
        | SuspendKind::Sleep { .. }
        | SuspendKind::SleepUntil { .. } => false,
    }
}
#[allow(
    clippy::match_same_arms,
    reason = "exhaustive match over every Terminator variant; the value-moving \
              suspension carriers share a body with Send/Ask and the \
              handle-read carriers share the false body, but each is kept a \
              separate arm so a future terminator cannot be folded into an \
              existing escape classification by accident — the exhaustiveness \
              is the fail-closed guarantee"
)]
pub(super) fn generator_yield_terminator_escapes(
    term: &Terminator,
    suspend_kind: Option<&SuspendKind>,
    local: u32,
) -> bool {
    match term {
        // A `Call`'s args are a borrow only when the callee is on the closed
        // ownership-contract list [`call_args_borrow_safe`] consults — see
        // its doc comment. A callee outside that list — including a
        // directly-resolved user Hew function — is unproven and counts as an
        // escape (fail-closed): it may forward the argument back out as its
        // own return value (GitHub issue #2412: `return wrap(v)`).
        Terminator::Call { callee, args, .. } => !call_args_borrow_safe(
            crate::runtime_symbols::callee_ownership_contract(callee),
            args,
            local,
        ),
        Terminator::Goto { .. }
        | Terminator::Branch { .. }
        | Terminator::Unreachable
        | Terminator::Trap { .. }
        | Terminator::MakeGenerator { .. } => false,
        // A bare `Suspend` escapes a yielded `local` exactly when its collapsed
        // carrier did — only the value-moving carriers (Ask/StreamSend/
        // RemoteAsk) do, recovered from the side-table. A `Suspend` with no
        // side-table entry is a generator / synthetic suspend whose value
        // channel is the frame out-pointer, so it escapes nothing.
        Terminator::Suspend { .. } => {
            suspend_kind.is_some_and(|k| suspend_kind_yield_escapes(k, local))
        }
        // The ten pure-{resume,cleanup} suspension carriers collapsed onto the
        // bare `Suspend` arm above (their escape posture is recovered from the
        // `SuspendKind` side-table via `suspend_kind_yield_escapes`).
        // `SuspendingScopeDeadline` carries `scope` + `duration_ms` — neither is
        // a generator-yielded `local`, so it never escapes one.
        Terminator::SuspendingScopeDeadline { .. } => false,
        // Lambda-actor construction: body/state-drop are static symbols,
        // but the capture env (when present) escapes into the actor's
        // heap-boxed state — a yielded value reachable through it must
        // not be body-end dropped.
        Terminator::MakeLambdaActor { env, .. } => env.is_some_and(|p| place_refs_local(p, local)),
        // A bare `Return` moves the function's ReturnSlot (already written by an
        // earlier `Move`, caught by the instr scan); `Return` itself carries no
        // operand. Re-yield / send / ask / select transfer the value out.
        Terminator::Return => false,
        Terminator::Yield { value, .. } => place_refs_local(*value, local),
        Terminator::Send { value, .. } | Terminator::Ask { value, .. } => {
            place_refs_local(*value, local)
        }
        Terminator::RemoteAsk { value, .. } => place_refs_local(*value, local),
        Terminator::Select { arms, .. } | Terminator::SuspendingSelect { arms, .. } => {
            select_arm_source_places(arms)
                .into_iter()
                .any(|p| place_refs_local(p, local))
        }
        Terminator::Join { branches, .. } => branches.iter().any(|branch| {
            place_refs_local(branch.actor, local)
                || branch.args.iter().any(|a| place_refs_local(*a, local))
                || place_refs_local(branch.value, local)
        }),
    }
}
pub(super) fn retained_string_terminator_drop_safe(
    term: &Terminator,
    suspend_kind: Option<&SuspendKind>,
    local: u32,
) -> bool {
    let reads_binding = terminator_source_places(term, suspend_kind)
        .into_iter()
        .any(|place| place_refs_local(place, local));
    if !reads_binding {
        return true;
    }
    // A borrowing string call (`hew_string_length`, `hew_string_concat`, the
    // `.len()` / `.to_uppercase()` getters, copy-in transforms, and print sinks)
    // reads its string argument without retaining it. A payload binder passed
    // there is a transient borrow, not an escape, so the parent enum composite
    // still owns the buffer and keeps its `EnumInPlace` drop. The binder itself
    // is excluded from its own sole-owner drop because it is read as a source
    // operand here, so the composite drop is the single owner.
    if let Terminator::Call { callee, args, .. } = term {
        if crate::runtime_symbols::callee_ownership_contract(callee).borrows_string_call_args()
            && args.iter().any(|arg| place_refs_local(*arg, local))
        {
            return true;
        }
    }
    matches!(
        term,
        Terminator::Call {
            callee,
            args,
            dest: None,
            ..
        } if matches!(callee.as_str(), "print" | "println" | "print_str" | "println_str")
            && matches!(args.as_slice(), [arg] if place_refs_local(*arg, local))
    )
}
/// The payload type of a checker-resolved `Option<T>` — the element type a
/// channel/stream recv binds. The recv flip sites derive their terminator's
/// `elem_ty` from the recv call's declared `Option<T>` return type so the
/// element witness stays checker-authoritative end to end; a non-`Option`
/// return shape (impossible from HIR's recv lowering) falls back to the
/// blocking-call path, whose codegen intercept fails closed on its own.
pub(super) fn option_payload_ty(ty: &ResolvedTy) -> Option<&ResolvedTy> {
    match ty {
        ResolvedTy::Named {
            args,
            builtin: Some(BuiltinType::Option),
            ..
        } if args.len() == 1 => args.first(),
        _ => None,
    }
}
