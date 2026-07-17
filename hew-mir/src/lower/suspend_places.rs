#[cfg(test)]
use super::*;
#[cfg(not(test))]
use super::{
    base_local, is_borrowing_call_abi, is_handle_borrowing_call_abi, ty_is_nonowning_handle_leaf,
    ty_is_owned_handle_leaf, ClosureEnvFieldOwnership, HirExpr, HirExprKind, HirStmt, HirStmtKind,
    Instr, Place, ResolvedTy, SelectArm, SelectArmKind, SuspendKind, Terminator,
};

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
        Instr::EnterContext
        | Instr::ExitContext
        | Instr::CheckCancellation
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
        | Instr::NeutralizePayloadSlot { .. } => Vec::new(),
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
        // `actor` + `value` are the reads; result/reply/error dests are writes.
        SuspendKind::Ask { actor, value, .. } => vec![*actor, *value],
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
        Terminator::Return | Terminator::Goto { .. } | Terminator::Trap { .. } => Vec::new(),
        Terminator::Branch { cond, .. } => vec![*cond],
        Terminator::Call { args, .. } => args.clone(),
        Terminator::Yield { value, .. } => vec![*value],
        // `dest` is the handle slot the generator is written into (a write);
        // `body_fn` is a static symbol, not a Place. `env` (when present) IS
        // read — the ramp flat-copies the capture env into the coro frame —
        // but is deliberately NOT listed as a source: the env place is always
        // the dedicated closure-env aggregate local `ClosureEnvInit` just
        // built (never a user binding or a leaf string/bytes temp), and
        // listing it would make the escape scans treat the env local itself
        // as escaping. Every USER value enters the env through
        // `ClosureEnvInit`, whose reads the dataflow table
        // (`dataflow::instr_reads_writes`) already reports — consumers that
        // need every read (the nested fresh-temp collectors) use that table,
        // not this one. Note the asymmetry with `MakeLambdaActor` below,
        // which does list its env.
        Terminator::MakeGenerator { .. } => Vec::new(),
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
        Terminator::Send { actor, value, .. } => vec![*actor, *value],
        // `reply_dest` is the slot the reply is written into — a write, not
        // a source.
        Terminator::Ask { actor, value, .. } => vec![*actor, *value],
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
pub(super) fn hir_expr_contains_synthetic_vec_string_index(expr: &HirExpr) -> bool {
    match &expr.kind {
        HirExprKind::Block(block) => {
            block
                .statements
                .iter()
                .any(hir_stmt_is_synthetic_vec_string_index)
                || block
                    .tail
                    .as_deref()
                    .is_some_and(hir_expr_contains_synthetic_vec_string_index)
        }
        HirExprKind::If {
            condition,
            then_expr,
            else_expr,
        } => {
            hir_expr_contains_synthetic_vec_string_index(condition)
                || hir_expr_contains_synthetic_vec_string_index(then_expr)
                || else_expr
                    .as_deref()
                    .is_some_and(hir_expr_contains_synthetic_vec_string_index)
        }
        _ => false,
    }
}
fn hir_stmt_is_synthetic_vec_string_index(stmt: &HirStmt) -> bool {
    matches!(
        &stmt.kind,
        HirStmtKind::Let(binding, Some(value))
            if binding.name.starts_with("__hew_iter_value_")
                && matches!(binding.ty, ResolvedTy::String)
                && matches!(value.kind, HirExprKind::Index { .. })
                && matches!(value.ty, ResolvedTy::String)
    )
}
/// Element-type-AGNOSTIC companion to
/// [`hir_expr_contains_synthetic_vec_string_index`]: true when the scrutinee
/// carries the `for x in <vec>` desugar's synthetic `let __hew_iter_value_N =
/// <vec>[i]` binding for ANY element type (owned tuple / record / enum, not
/// just `string`). Used SOLELY by the #2523 provenance-skip decision: every
/// such element is a FRESH, solely-owned per-frame value the iteration handed
/// the body, never a projection of a re-readable aggregate that retains the
/// bits, so a move-out (`let (k, val) = pair`, `return pair`) is a legitimate
/// ownership transfer that must not route through default-deny. The
/// string-specific disposition logic keeps its own narrower detector.
pub(super) fn hir_expr_contains_synthetic_vec_index(expr: &HirExpr) -> bool {
    match &expr.kind {
        HirExprKind::Block(block) => {
            block.statements.iter().any(hir_stmt_is_synthetic_vec_index)
                || block
                    .tail
                    .as_deref()
                    .is_some_and(hir_expr_contains_synthetic_vec_index)
        }
        HirExprKind::If {
            condition,
            then_expr,
            else_expr,
        } => {
            hir_expr_contains_synthetic_vec_index(condition)
                || hir_expr_contains_synthetic_vec_index(then_expr)
                || else_expr
                    .as_deref()
                    .is_some_and(hir_expr_contains_synthetic_vec_index)
        }
        _ => false,
    }
}
fn hir_stmt_is_synthetic_vec_index(stmt: &HirStmt) -> bool {
    matches!(
        &stmt.kind,
        HirStmtKind::Let(binding, Some(value))
            if binding.name.starts_with("__hew_iter_value_")
                && matches!(value.kind, HirExprKind::Index { .. })
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
        Instr::EnterContext
        | Instr::ExitContext
        | Instr::CheckCancellation
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
        // A payload-slot neutralize nulls the scrutinee's transferred slot; it
        // does not hand any local out of the body.
        | Instr::NeutralizePayloadSlot { .. } => false,
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
        SuspendKind::Ask { value, .. }
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
/// The *escape* operands of an instruction — the source reads that alias an
/// owned handle OUT of the [`detect_unproven_aggregate_handle_double_free`]
/// gate's tracked dataflow (into untracked heap storage, a closure env, a
/// spawned execution context, or a `dyn` box). A carrier surfacing here is
/// freed on a path the per-origin drop tally cannot model, so its origins are
/// poisoned and refused. Returns the empty set for the propagation / free /
/// borrow reads the fixpoint and tally already account for — including
/// `CallRuntimeAbi`, whose operands are the *container* being read (`hew_vec_len`
/// / `hew_vec_get_ptr`), never an owned-handle leaf aliased out.
#[allow(
    clippy::match_same_arms,
    reason = "the closure-env-field arms intentionally share the empty-escape \
              result of the `_` arm but are named explicitly so the closure \
              write-back/read path's escape classification is visible on the \
              diff and is forced to be re-examined if the `#1'` BitCopy gate is \
              ever loosened to owned captures (exhaustive-traversal-and-lowering)"
)]
pub(super) fn instr_escape_places(instr: &Instr) -> Vec<Place> {
    match instr {
        // A value stored into a still-live aggregate / actor state is aliased
        // into storage that will drop it independently of its source.
        Instr::RecordFieldStore { src, .. } | Instr::ActorStateFieldStore { src, .. } => {
            vec![*src]
        }
        Instr::ClosureEnvInit { fields, .. } => fields
            .iter()
            .filter(|field| field.ownership == ClosureEnvFieldOwnership::OwnsMoved)
            .map(|field| field.src)
            .collect(),
        // Captured / spawned / erased: the handle moves into a context whose
        // drop this function cannot see.
        Instr::MakeClosure { env, .. } => vec![*env],
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
        // Closure-env field write-back (`#1'`) stores a BitCopy scalar: the
        // store is gated to `ValueClass::BitCopy`, and an owned capture
        // reassignment fails closed with NotYetImplemented. A scalar `src`
        // carries no owned handle, so nothing is aliased out of the tracked
        // dataflow — no escape place. If that gate is ever loosened to admit
        // owned captures, `src` would alias into the env's independently-dropped
        // storage and MUST surface here as `vec![*src]`, mirroring
        // `RecordFieldStore` above. Named explicitly (not left to the `_` arm)
        // so this invariant is visible on the closure path (exhaustive-traversal).
        Instr::ClosureEnvFieldStore { .. } => Vec::new(),
        // Reading a field OUT of the env is a borrow into tracked dataflow, not
        // an alias of an owned handle into untracked storage — no escape place.
        Instr::ClosureEnvFieldLoad { .. } => Vec::new(),
        // Dispatched calls take their arguments (and the receiver) by value.
        Instr::CallClosure { args, .. } => args.clone(),
        Instr::CallTraitMethod {
            fat_pointer, args, ..
        } => {
            let mut places = vec![*fat_pointer];
            places.extend(args.iter().copied());
            places
        }
        _ => Vec::new(),
    }
}
/// The payload type of a checker-resolved `Option<T>` — the element type a
/// channel/stream recv binds. The recv flip sites derive their terminator's
/// `elem_ty` from the recv call's declared `Option<T>` return type so the
/// element witness stays checker-authoritative end to end; a non-`Option`
/// return shape (impossible from HIR's recv lowering) falls back to the
/// blocking-call path, whose codegen intercept fails closed on its own.
pub(super) fn option_payload_ty(ty: &ResolvedTy) -> Option<&ResolvedTy> {
    match ty {
        ResolvedTy::Named { name, args, .. } if name == "Option" && args.len() == 1 => args.first(),
        _ => None,
    }
}
/// The *escape* operands of a terminator — the source reads that alias an owned
/// handle out of the gate's tracked dataflow. A `Call` (a user function OR a
/// runtime collection-push helper like `hew_vec_push_ptr`) takes every argument
/// by value; a `Yield` hands its value to the resumer; an actor `Send`/`Ask`
/// transfers its message *payload* to the receiver. The actor-pid and
/// timeout operands of a send/ask are borrowed, not aliased out, so they are
/// deliberately excluded — poisoning them would over-refuse all actor code.
///
/// Two narrow `Call` exemptions keep this from over-refusing borrows that
/// provably cannot double-free (`local_tys` supplies the per-arg static type):
///
///   1. A by-value arg whose static type is a NON-OWNING, no-`close` actor-pid
///      leaf (`Pid`/`LocalPid`/`RemotePid`) — its drop frees nothing (the
///      actor lifecycle is owned by the runtime scheduler; the pid is a
///      by-value reference snapshot), so passing it by value can never alias a
///      second free. This admits any `fn f(p: LocalPid<_>)` by-value call.
///   2. The known-borrowing runtime ABIs in [`is_borrowing_call_abi`] — the
///      callee snapshots/reads its handle args without retaining or
///      transferring them. The ratified active-mode `conn.attach(handler)`
///      lowers to `hew_tcp_attach_local(conn, handler)`, whose `LocalPid`
///      handler the runtime registers as a non-owning `HewActorRef::Local`
///      by-value snapshot; the borrowed args are exempted per-arg (not
///      blanket) so an owning handle arg is never silently let through.
///
/// Every OTHER arg — owning handle leaves (Generator/Stream/Sink/Duplex/halves/
/// LambdaActorHandle/CancellationToken) and any aggregate that may carry one —
/// stays poisoned: the container-push / aggregate-store / storing cross-call
/// double-free shapes the fixpoint cannot model must keep failing closed.
/// Owned values a collapsed suspension carrier moves OUT across the suspend,
/// recovered from the [`SuspendKind`] side-table — the bare-[`Terminator::Suspend`]
/// analogue of the per-carrier escape arms in [`terminator_escape_places`].
/// MEMORY-SAFETY: the value-moving carriers (`Ask`/`StreamSend` move `value`
/// into the message/channel queue; `RemoteAsk` serialises `value` onto the wire;
/// `CallClosure` forwards `args` by value into the callee coroutine) MUST be
/// poisoned, or a source-side drop double-frees an owned-handle payload the
/// message/channel/callee also releases. The read-back carriers
/// (`Read`/`Accept`/`StreamNext`/`ChannelRecv`/`TaskAwait`/`Sleep`) move no
/// owned value into a sink the fixpoint cannot model, so they escape nothing.
#[must_use]
fn suspend_kind_escape_places(kind: &SuspendKind) -> Vec<Place> {
    match kind {
        SuspendKind::Ask { value, .. }
        | SuspendKind::StreamSend { value, .. }
        | SuspendKind::RemoteAsk { value, .. } => vec![*value],
        SuspendKind::CallClosure { args, .. } => args.clone(),
        SuspendKind::Read { .. }
        | SuspendKind::Accept { .. }
        | SuspendKind::StreamNext { .. }
        | SuspendKind::ChannelRecv { .. }
        | SuspendKind::TaskAwait { .. }
        // `await_restart` reads the borrowed supervisor PID and re-fetches the
        // child handle on resume — no owned value escapes into a sink.
        | SuspendKind::RestartWait { .. }
        | SuspendKind::Sleep { .. }
        | SuspendKind::SleepUntil { .. } => Vec::new(),
    }
}
#[allow(
    clippy::match_same_arms,
    reason = "Yield and the actor Send/Ask/RemoteAsk payloads share an escape \
              shape but are kept as separate arms so the deliberate exclusion \
              of the borrowed actor-pid / timeout operands stays explicit"
)]
pub(super) fn terminator_escape_places(
    term: &Terminator,
    suspend_kind: Option<&SuspendKind>,
    local_tys: &[ResolvedTy],
) -> Vec<Place> {
    let arg_is_borrowed =
        |builtin: Option<hew_types::runtime_call::RuntimeCallFamily>, place: &Place| -> bool {
            let arg_ty = base_local(*place).and_then(|l| local_tys.get(l as usize));
            // A no-close actor-pid leaf is a borrow under ANY callee; the
            // `is_borrowing_call_abi` allowlist additionally borrows the callee's
            // non-handle-leaf args by value (e.g. `hew_tcp_attach_local`'s
            // LocalPid handler — but its `conn` is consumed, so the per-arg
            // owned-handle-leaf guard keeps `conn` poisoned). The
            // `is_handle_borrowing_call_abi` allowlist promotes the stricter
            // shape: the callee borrows EVERY arg including owned-handle leaves
            // (the stream recv / try_recv runtime entries borrow the stream
            // handle to read one item — the handle continues to live in the
            // caller's slot afterwards, identical drop semantics to the
            // suspending `Terminator::SuspendingStreamNext` path).
            //
            // Both allowlists key on the call's carried typed family — a
            // callee that arrives without its family (`None`) is never
            // exempted, which fails CLOSED: the arg stays poisoned and the
            // gate over-refuses rather than ever admitting a phantom borrow.
            arg_ty.is_some_and(ty_is_nonowning_handle_leaf)
                || (is_borrowing_call_abi(builtin)
                    && arg_ty.is_some_and(|t| !ty_is_owned_handle_leaf(t)))
                || is_handle_borrowing_call_abi(builtin)
        };
    match term {
        Terminator::Call {
            callee,
            builtin,
            args,
            ..
        } => {
            // `MonitorRef::recv_down` is an ordinary stdlib method (not a
            // BuiltinNamedType rewrite), but its ABI is a proven borrow: the
            // body reads `ref_id`, calls `hew_node_monitor_recv`, and has no
            // parameter drop plan. Treating the Result-pattern payload binder
            // as escaping here would reject the safe
            // `match monitor(remote) { Ok(m) => m.recv_down(..), .. }` shape.
            let borrows_owned_handle = callee == "MonitorRef::recv_down";
            args.iter()
                .copied()
                .filter(|place| {
                    let borrowed_handle = borrows_owned_handle
                        && base_local(*place).is_some_and(|local| {
                            local_tys
                                .get(local as usize)
                                .is_some_and(ty_is_owned_handle_leaf)
                        });
                    !(borrowed_handle || arg_is_borrowed(*builtin, place))
                })
                .collect()
        }
        Terminator::Yield { value, .. } => vec![*value],
        Terminator::Send { value, .. }
        | Terminator::Ask { value, .. }
        | Terminator::RemoteAsk { value, .. } => vec![*value],
        // The suspending select transfers each ActorAsk arm's `value` payload
        // into its actor message queue exactly as the blocking select does
        // (the readiness waitset still issues the ask); poison each owned
        // payload identically.
        Terminator::Select { arms, .. } | Terminator::SuspendingSelect { arms, .. } => {
            let mut places = Vec::new();
            for arm in arms {
                if let SelectArmKind::ActorAsk { value, .. } = &arm.kind {
                    places.push(*value);
                }
            }
            places
        }
        // A `join { a.m(owned), ... }` transfers each branch's `value`
        // payload into its actor message queue exactly as the `select`
        // ActorAsk arms above do — poison each so the escape gate refuses
        // or poisons an owned-handle payload identically.
        Terminator::Join { branches, .. } => branches.iter().map(|branch| branch.value).collect(),
        // The ten pure-{resume,cleanup} suspension carriers collapsed onto the
        // bare `Suspend` arm below, which recovers their escaping payload from the
        // `SuspendKind` side-table via `suspend_kind_escape_places` (the
        // value-moving Ask/StreamSend/RemoteAsk/CallClosure carriers poison their
        // payload; the read-back carriers poison nothing — the double-free guard).
        // `SuspendingScopeDeadline` carries only a `scope` handle + a duration
        // scalar, moving no owned value into an un-modellable sink.
        Terminator::SuspendingScopeDeadline { .. } => Vec::new(),
        // Control-flow / non-transferring terminators escape nothing. A bare
        // `Suspend` with a side-table entry is a collapsed carrier — poison its
        // moved-out payload; a `Suspend` with no entry is a generator / synthetic
        // suspend whose value channel is the frame out-pointer (escapes nothing).
        Terminator::Suspend { .. } => {
            suspend_kind.map_or_else(Vec::new, suspend_kind_escape_places)
        }
        Terminator::Return
        | Terminator::Goto { .. }
        | Terminator::Branch { .. }
        | Terminator::Trap { .. }
        | Terminator::MakeGenerator { .. } => Vec::new(),
        // Lambda-actor construction: body_fn and state_drop_fn are static
        // symbols and the `dest` handle slot is the WRITE — but the capture
        // env (when present) transfers into the actor's heap-boxed state,
        // so it is poisoned like any other moved-into-sink payload.
        Terminator::MakeLambdaActor { env, .. } => env.iter().copied().collect(),
    }
}
#[cfg(test)]
mod f1_suspending_escape_poison {
    //! F-1: the escape-poison gate (`terminator_escape_places`) must list the
    //! `value` payload of a SUSPENDABLE `await peer.method(owned)` / `await
    //! sink.send(owned)` as escaping, exactly as the blocking
    //! `Ask`/`Send`/`RemoteAsk` already do. Before the fix both suspending
    //! payload-carriers fell into a `_ => Vec::new()` catch-all and were NOT
    //! poisoned, so a suspendable handler whose payload was an owned handle
    //! double-freed (the source body-end drop plus the message consumption both
    //! released it). These tests poke the gate directly with synthetic
    //! terminators because the owned-handle-leaf payload shapes have no
    //! buildable v0.5 source surface that reaches a `SuspendingAsk` /
    //! `SuspendingStreamSend` (owned-handle aggregate extraction is itself
    //! fail-closed), mirroring the `w3053_aggregate_handle_double_free_gate`
    //! synthetic-MIR approach. They FAIL on the pre-fix catch-all tree.
    use super::*;

    fn sink_string_ty() -> ResolvedTy {
        ResolvedTy::named_builtin("Sink", BuiltinType::Sink, vec![ResolvedTy::String])
    }

    // A collapsed suspension carrier is a bare `Suspend` whose payload lives in
    // the side-table; the escape-poison reads it via the `Option<&SuspendKind>`.
    fn suspend() -> Terminator {
        Terminator::Suspend {
            resume: 1,
            cleanup: 2,
            is_final: false,
        }
    }

    #[test]
    fn suspending_ask_poisons_its_owned_payload() {
        let value = Place::Local(3);
        let kind = SuspendKind::Ask {
            actor: Place::Local(1),
            msg_type: 0,
            value,
            result_dest: Place::Local(4),
            reply_dest: Place::Local(5),
            error_dest: Place::Local(6),
        };
        let mut local_tys = vec![ResolvedTy::I64; 7];
        local_tys[3] = sink_string_ty();
        assert!(
            terminator_escape_places(&suspend(), Some(&kind), &local_tys).contains(&value),
            "Ask.value (an owned Sink payload) must escape-poison so it \
             is excluded from the source body-end drop — else it double-frees"
        );
    }

    #[test]
    fn suspending_stream_send_poisons_its_owned_payload() {
        let value = Place::Local(2);
        let kind = SuspendKind::StreamSend {
            sink: Place::Local(1),
            value,
        };
        let mut local_tys = vec![ResolvedTy::I64; 3];
        local_tys[2] = sink_string_ty();
        assert_eq!(
            terminator_escape_places(&suspend(), Some(&kind), &local_tys),
            vec![value],
            "StreamSend.value must escape-poison like the blocking Send"
        );
    }

    #[test]
    fn suspending_ask_matches_blocking_ask_escape_set() {
        // The fix's invariant: the suspendable analogue poisons identically to
        // the blocking form. A divergence is exactly the F-1 double-free gap.
        let value = Place::Local(3);
        let local_tys = vec![ResolvedTy::I64; 7];
        let blocking = Terminator::Ask {
            actor: Place::Local(1),
            msg_type: 0,
            value,
            result_dest: Place::Local(4),
            reply_dest: Place::Local(5),
            error_dest: Place::Local(6),
            next: 1,
        };
        let kind = SuspendKind::Ask {
            actor: Place::Local(1),
            msg_type: 0,
            value,
            result_dest: Place::Local(4),
            reply_dest: Place::Local(5),
            error_dest: Place::Local(6),
        };
        assert_eq!(
            terminator_escape_places(&blocking, None, &local_tys),
            terminator_escape_places(&suspend(), Some(&kind), &local_tys),
            "the suspendable ask must poison the same payload set as the blocking ask"
        );
    }

    #[test]
    fn non_payload_suspending_carriers_escape_nothing() {
        // The read-back carriers transfer no owned value OUT; they must remain
        // empty (no over-poisoning that would refuse legitimate code).
        let read = SuspendKind::Read {
            conn: Place::Local(1),
            result_dest: Place::Local(2),
            deadline_result_dest: None,
            error_dest: None,
            to_string: false,
        };
        let next = SuspendKind::StreamNext {
            stream: Place::Local(1),
            result_dest: Place::Local(2),
            elem_ty: ResolvedTy::Bytes,
            deadline_result_dest: None,
            error_dest: None,
        };
        assert!(terminator_escape_places(&suspend(), Some(&read), &[]).is_empty());
        assert!(terminator_escape_places(&suspend(), Some(&next), &[]).is_empty());
    }

    #[test]
    fn suspending_call_closure_poisons_its_forwarded_args() {
        // `await closure(owned_handle)` forwards `args` by value into the callee
        // coroutine exactly as the non-suspending `Instr::CallClosure` does, so
        // an owned-handle arg must escape-poison identically (else the source
        // body-end drop plus the callee's consumption double-free). The closure
        // pair (`callee`) is a borrowed read and must NOT be poisoned.
        let callee = Place::Local(1);
        let arg0 = Place::Local(2);
        let arg1 = Place::Local(3);
        let kind = SuspendKind::CallClosure {
            callee,
            args: vec![arg0, arg1],
            ret_ty: ResolvedTy::Unit,
            result_dest: None,
        };
        let escaped = terminator_escape_places(&suspend(), Some(&kind), &[]);
        assert_eq!(
            escaped,
            vec![arg0, arg1],
            "SuspendingCallClosure must poison its forwarded args like Instr::CallClosure"
        );
        assert!(
            !escaped.contains(&callee),
            "the borrowed closure pair must NOT be poisoned"
        );
    }

    // ---------- derive_local_collection_drop_allowed: alias-root exclusion ----------

    /// A `HashMap<i64, i64>` handle `ResolvedTy` for synthetic collection
    /// candidates. The escape scan dispatches on the `HashMap` builtin
    /// discriminant; the arg detail is immaterial.
    fn hashmap_i64_i64_ty() -> ResolvedTy {
        ResolvedTy::Named {
            name: "HashMap".to_string(),
            args: vec![ResolvedTy::I64, ResolvedTy::I64],
            builtin: Some(BuiltinType::HashMap),
            is_opaque: false,
        }
    }

    /// Regression for the alias-root admission bug: two collection candidates
    /// where `b` is a whole-value alias of `a` (`alias_of[b] == a`) and `b`
    /// escapes into an aggregate (`RecordInit` — the `spawn Holder(f: b)`
    /// shape). `note_escape` records the escape under the ROOT `a`, so a final
    /// admission that tested the candidate's own local (`excluded_roots
    /// .contains(&b)`) would still admit `b` — a producer-side
    /// `hew_hashmap_free_layout` of the very handle the aggregate now owns: a
    /// double free. Resolving each candidate to its alias root before the
    /// exclusion test excludes the whole alias group. The escape-scan is the
    /// sole authority here (the dataflow `Consumed` net runs separately), so
    /// this pins the function's own contract independent of what the front end
    /// can currently lower.
    #[test]
    fn derive_local_collection_drop_allowed_excludes_aliased_member_when_root_escapes() {
        let map_ty = hashmap_i64_i64_ty();
        let binding_a = BindingId(101);
        let binding_b = BindingId(102);

        // Block: `b = a` (whole-value alias), then `b` is placed into a record
        // (the aggregate-ingress escape), then return.
        let block = BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![
                Instr::Move {
                    dest: Place::Local(2),
                    src: Place::Local(1),
                },
                Instr::RecordInit {
                    ty: ResolvedTy::Named {
                        name: "Holder".to_string(),
                        args: vec![],
                        builtin: None,
                        is_opaque: false,
                    },
                    fields: vec![(FieldOffset(0), Place::Local(2))],
                    dest: Place::Local(3),
                },
            ],
            terminator: Terminator::Return,
        };

        let owned_locals = vec![
            (binding_a, "a".to_string(), map_ty.clone()),
            (binding_b, "b".to_string(), map_ty.clone()),
        ];
        let binding_locals: HashMap<BindingId, Place> =
            [(binding_a, Place::Local(1)), (binding_b, Place::Local(2))]
                .into_iter()
                .collect();

        let allowed = derive_local_collection_drop_allowed(
            &[block],
            &std::collections::HashMap::new(),
            &owned_locals,
            &binding_locals,
            &HashMap::new(),
            ty_is_local_collection_handle,
        );

        assert!(
            !allowed.contains(&binding_b),
            "the aliased ESCAPER `b` must not earn a scope-exit free — the \
             aggregate it was moved into owns the handle now; admitting it \
             double-frees (this is the alias-root bug). allowed: {allowed:?}"
        );
        assert!(
            !allowed.contains(&binding_a),
            "the alias root `a` shares `b`'s handle and must also be excluded; \
             allowed: {allowed:?}"
        );
        assert!(
            allowed.is_empty(),
            "no handle in an escaped alias group may be freed here; allowed: {allowed:?}"
        );
    }

    /// Companion positive control: the same `b = a` whole-value alias with NO
    /// escape keeps both candidates admitted. The alias-root resolution must
    /// only EXCLUDE on a real escape — it must not over-exclude a live,
    /// non-escaping alias group (which would silently reintroduce the leak the
    /// lane fixes). The per-exit liveness filter downstream still drops only the
    /// one live member; admitting both here is correct at this layer.
    #[test]
    fn derive_local_collection_drop_allowed_admits_non_escaping_alias_group() {
        let map_ty = hashmap_i64_i64_ty();
        let binding_a = BindingId(201);
        let binding_b = BindingId(202);

        // `b = a` then a receiver-borrowing op on `b` (interior read) and return:
        // no owning-sink escape anywhere.
        let block = BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![Instr::Move {
                dest: Place::Local(2),
                src: Place::Local(1),
            }],
            terminator: Terminator::Call {
                callee: "hew_hashmap_len_layout".to_string(),
                builtin: Some(hew_types::runtime_call::RuntimeCallFamily::HashMapLenLayout),
                args: vec![Place::Local(2)],
                dest: Some(Place::Local(3)),
                next: 1,
            },
        };

        let owned_locals = vec![
            (binding_a, "a".to_string(), map_ty.clone()),
            (binding_b, "b".to_string(), map_ty.clone()),
        ];
        let binding_locals: HashMap<BindingId, Place> =
            [(binding_a, Place::Local(1)), (binding_b, Place::Local(2))]
                .into_iter()
                .collect();

        let allowed = derive_local_collection_drop_allowed(
            &[block],
            &std::collections::HashMap::new(),
            &owned_locals,
            &binding_locals,
            &HashMap::new(),
            ty_is_local_collection_handle,
        );

        assert!(
            allowed.contains(&binding_a) && allowed.contains(&binding_b),
            "a non-escaping alias group must stay admitted (root resolution must \
             not over-exclude); allowed: {allowed:?}"
        );
    }

    // ---------- derive_local_bytes_drop_allowed: fail-closed admission ----------

    /// Scaffolding: a single-candidate owned-locals fixture for the bytes
    /// prover (`b` at `Place::Local(1)`).
    #[allow(
        clippy::type_complexity,
        reason = "test-only fixture tuple mirroring the prover's three \
                  parameters; a named struct would only add indirection"
    )]
    fn bytes_prover_fixture() -> (
        Vec<(BindingId, String, ResolvedTy)>,
        HashMap<BindingId, Place>,
        BindingId,
        Vec<ResolvedTy>,
    ) {
        let binding = BindingId(301);
        let owned_locals = vec![(binding, "b".to_string(), ResolvedTy::Bytes)];
        let binding_locals: HashMap<BindingId, Place> =
            [(binding, Place::Local(1))].into_iter().collect();
        (
            owned_locals,
            binding_locals,
            binding,
            vec![ResolvedTy::Unit, ResolvedTy::Bytes],
        )
    }

    /// A bytes local that is never read anywhere is the sole owner of its
    /// buffer and must be admitted for the scope-exit `hew_bytes_drop` — the
    /// sender-local leak shape this prover exists to close.
    #[test]
    fn derive_local_bytes_drop_allowed_admits_sole_owner_local() {
        let (owned_locals, binding_locals, binding, local_tys) = bytes_prover_fixture();
        let block = BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Return,
        };
        let allowed = derive_local_bytes_drop_allowed(
            &[block],
            &std::collections::HashMap::new(),
            &owned_locals,
            &binding_locals,
            &local_tys,
            &HashSet::new(),
        )
        .allowed;
        assert!(
            allowed.contains(&binding),
            "a never-read bytes local must earn its scope-exit drop; allowed: {allowed:?}"
        );
    }

    /// A bytes local consumed by an actor `Send` is handed to the mailbox via
    /// `memcpy` — the receive side / actor state owns the buffer from then on
    /// (the probe-pinned ownership trace). The send terminator reads the place,
    /// so the escape scan must exclude the binding even BEFORE the dataflow
    /// `Consumed` filter (the belt-and-suspenders net) runs.
    #[test]
    fn derive_local_bytes_drop_allowed_excludes_send_consumed_local() {
        let (owned_locals, binding_locals, binding, local_tys) = bytes_prover_fixture();
        let block = BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Send {
                actor: Place::Local(9),
                msg_type: 0,
                value: Place::Local(1),
                next: 1,
                alias_mode: crate::model::SendAliasMode::Copy,
            },
        };
        let allowed = derive_local_bytes_drop_allowed(
            &[block],
            &std::collections::HashMap::new(),
            &owned_locals,
            &binding_locals,
            &local_tys,
            &HashSet::new(),
        )
        .allowed;
        assert!(
            !allowed.contains(&binding),
            "a send-consumed bytes local must NOT earn a sender-side drop — the \
             actor owns the buffer now (state_drop is its release); admitting it \
             double-frees; allowed: {allowed:?}"
        );
    }

    /// A receiver-borrowing bytes runtime op (`hew_bytes_len` as
    /// `Instr::CallRuntimeAbi`) reads the triple as arg[0] but only borrows
    /// it; the binding must stay admitted (otherwise every `b.len()` would
    /// silently reintroduce the leak).
    #[test]
    fn derive_local_bytes_drop_allowed_admits_borrow_receiver_read() {
        let (owned_locals, binding_locals, binding, local_tys) = bytes_prover_fixture();
        let call = crate::model::RuntimeCall::new(
            "hew_bytes_len",
            vec![Place::Local(1)],
            Some(Place::Local(2)),
        )
        .expect("hew_bytes_len is an allowlisted runtime symbol");
        let block = BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![Instr::CallRuntimeAbi(call)],
            terminator: Terminator::Return,
        };
        let allowed = derive_local_bytes_drop_allowed(
            &[block],
            &std::collections::HashMap::new(),
            &owned_locals,
            &binding_locals,
            &local_tys,
            &HashSet::new(),
        )
        .allowed;
        assert!(
            allowed.contains(&binding),
            "a borrow-listed receiver read must not exclude the binding; \
             allowed: {allowed:?}"
        );
    }

    /// A runtime call NOT on the bytes receiver-borrow allow-list that reads
    /// the triple is an escape (default-deny): the binding leaks rather than
    /// risking a free against an unverified ownership contract.
    #[test]
    fn derive_local_bytes_drop_allowed_excludes_unlisted_runtime_read() {
        let (owned_locals, binding_locals, binding, local_tys) = bytes_prover_fixture();
        let call =
            crate::model::RuntimeCall::new("hew_task_scope_join_all", vec![Place::Local(1)], None)
                .expect("hew_task_scope_join_all is an allowlisted runtime symbol");
        let block = BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![Instr::CallRuntimeAbi(call)],
            terminator: Terminator::Return,
        };
        let allowed = derive_local_bytes_drop_allowed(
            &[block],
            &std::collections::HashMap::new(),
            &owned_locals,
            &binding_locals,
            &local_tys,
            &HashSet::new(),
        )
        .allowed;
        assert!(
            !allowed.contains(&binding),
            "an unlisted runtime read must default-deny (leak, never \
             double-free); allowed: {allowed:?}"
        );
    }

    /// A bytes triple loaded out of a still-live aggregate becomes a retained
    /// co-owner when the dest is an owned binding: MIR emits one retain marker
    /// and admits the binding's balancing scope-exit drop.
    #[test]
    fn derive_local_bytes_drop_allowed_admits_retained_projection_binding() {
        let (owned_locals, binding_locals, binding, local_tys) = bytes_prover_fixture();
        let block = BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![Instr::RecordFieldLoad {
                record: Place::Local(5),
                field_offset: FieldOffset(0),
                dest: Place::Local(1),
            }],
            terminator: Terminator::Return,
        };
        let derivation = derive_local_bytes_drop_allowed(
            &[block],
            &std::collections::HashMap::new(),
            &owned_locals,
            &binding_locals,
            &local_tys,
            &HashSet::new(),
        );
        assert!(
            derivation.allowed.contains(&binding),
            "a retained field-loaded bytes binding must earn its balancing drop; \
             allowed: {:?}",
            derivation.allowed
        );
        assert_eq!(
            derivation.retain_sites,
            vec![BytesRetainSite {
                block: 0,
                instr_index: 0,
                placement: BytesRetainPlacement::After,
                value: Place::Local(1),
                required_bindings: Vec::new(),
            }]
        );
    }

    #[test]
    fn derive_local_bytes_drop_allowed_skips_borrow_only_field_temp_retain() {
        let local_tys = vec![ResolvedTy::Unit, ResolvedTy::Bytes, ResolvedTy::I64];
        let len_call = crate::model::RuntimeCall::new(
            "hew_bytes_len",
            vec![Place::Local(1)],
            Some(Place::Local(2)),
        )
        .expect("hew_bytes_len is allowlisted");
        let block = BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![
                Instr::RecordFieldLoad {
                    record: Place::Local(5),
                    field_offset: FieldOffset(0),
                    dest: Place::Local(1),
                },
                Instr::CallRuntimeAbi(len_call),
            ],
            terminator: Terminator::Return,
        };
        let derivation = derive_local_bytes_drop_allowed(
            &[block],
            &HashMap::new(),
            &[],
            &HashMap::new(),
            &local_tys,
            &HashSet::new(),
        );
        assert!(
            derivation.retain_sites.is_empty(),
            "borrow-only field receiver temps must not be retained: {:?}",
            derivation.retain_sites
        );
    }

    #[test]
    fn derive_local_bytes_drop_allowed_marks_live_local_coown_move() {
        let a = BindingId(401);
        let b = BindingId(402);
        let owned = vec![
            (a, "a".to_string(), ResolvedTy::Bytes),
            (b, "b".to_string(), ResolvedTy::Bytes),
        ];
        let binding_locals = HashMap::from([(a, Place::Local(1)), (b, Place::Local(2))]);
        let block = BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![Instr::Move {
                dest: Place::Local(2),
                src: Place::Local(1),
            }],
            terminator: Terminator::Return,
        };
        let derivation = derive_local_bytes_drop_allowed(
            &[block],
            &HashMap::new(),
            &owned,
            &binding_locals,
            &[ResolvedTy::Unit, ResolvedTy::Bytes, ResolvedTy::Bytes],
            &HashSet::new(),
        );
        assert_eq!(derivation.allowed, HashSet::from([a, b]));
        assert_eq!(
            derivation.retain_sites,
            vec![BytesRetainSite {
                block: 0,
                instr_index: 0,
                placement: BytesRetainPlacement::Before,
                value: Place::Local(1),
                required_bindings: vec![a, b],
            }]
        );
    }

    #[test]
    fn derive_local_bytes_drop_allowed_marks_live_local_aggregate_coown() {
        let (owned_locals, binding_locals, binding, local_tys) = bytes_prover_fixture();
        let block = BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![Instr::RecordInit {
                ty: ResolvedTy::Named {
                    name: "Holder".to_string(),
                    args: vec![],
                    builtin: None,
                    is_opaque: false,
                },
                fields: vec![(FieldOffset(0), Place::Local(1))],
                dest: Place::Local(3),
            }],
            terminator: Terminator::Return,
        };
        let derivation = derive_local_bytes_drop_allowed(
            &[block],
            &HashMap::new(),
            &owned_locals,
            &binding_locals,
            &local_tys,
            &HashSet::new(),
        );
        assert!(derivation.allowed.contains(&binding));
        assert_eq!(
            derivation.retain_sites,
            vec![BytesRetainSite {
                block: 0,
                instr_index: 0,
                placement: BytesRetainPlacement::Before,
                value: Place::Local(1),
                required_bindings: vec![binding],
            }]
        );
    }

    #[test]
    fn derive_local_bytes_drop_allowed_marks_borrowed_param_return() {
        let block = BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(1),
            }],
            terminator: Terminator::Return,
        };
        let derivation = derive_local_bytes_drop_allowed(
            &[block],
            &HashMap::new(),
            &[],
            &HashMap::new(),
            &[ResolvedTy::Unit, ResolvedTy::Bytes],
            &HashSet::from([1]),
        );
        assert_eq!(
            derivation.retain_sites,
            vec![BytesRetainSite {
                block: 0,
                instr_index: 0,
                placement: BytesRetainPlacement::Before,
                value: Place::Local(1),
                required_bindings: Vec::new(),
            }]
        );
    }

    #[test]
    fn derive_local_bytes_drop_allowed_marks_each_borrowed_param_return_copy() {
        let block = BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![
                Instr::TupleConstruct {
                    elements: vec![Place::Local(1), Place::Local(1)],
                    dest: Place::Local(2),
                },
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(2),
                },
            ],
            terminator: Terminator::Return,
        };
        let derivation = derive_local_bytes_drop_allowed(
            &[block],
            &HashMap::new(),
            &[],
            &HashMap::new(),
            &[
                ResolvedTy::Unit,
                ResolvedTy::Bytes,
                ResolvedTy::Tuple(vec![ResolvedTy::Bytes, ResolvedTy::Bytes]),
            ],
            &HashSet::from([1]),
        );
        assert_eq!(
            derivation.retain_sites,
            vec![
                BytesRetainSite {
                    block: 0,
                    instr_index: 0,
                    placement: BytesRetainPlacement::Before,
                    value: Place::Local(1),
                    required_bindings: Vec::new(),
                },
                BytesRetainSite {
                    block: 0,
                    instr_index: 0,
                    placement: BytesRetainPlacement::Before,
                    value: Place::Local(1),
                    required_bindings: Vec::new(),
                },
            ]
        );
    }

    /// A240 Stage S1 hole #1: `let b = a;` where `a` is a by-value `bytes`
    /// PARAMETER (a borrow — the caller retains ownership and drops the
    /// original). `b` is an owned local admitted to its scope-exit drop, so
    /// dropping `b` unretained would free the caller's live buffer and the
    /// caller's own drop would then double-free it. The move-share scan must
    /// mint a `BytesRetain` for `b` even though the source is not an owned
    /// candidate — matched here purely via `borrowed_alias_of` (the param is
    /// deliberately absent from `binding_locals` to isolate that trigger).
    #[test]
    fn derive_local_bytes_drop_allowed_marks_borrowed_param_local_coown() {
        let b = BindingId(501);
        let owned = vec![(b, "b".to_string(), ResolvedTy::Bytes)];
        // `b` at Local(1); the borrowed param `a` at Local(0) is NOT owned.
        let binding_locals = HashMap::from([(b, Place::Local(1))]);
        let block = BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![Instr::Move {
                dest: Place::Local(1),
                src: Place::Local(0),
            }],
            terminator: Terminator::Return,
        };
        let derivation = derive_local_bytes_drop_allowed(
            &[block],
            &HashMap::new(),
            &owned,
            &binding_locals,
            &[ResolvedTy::Bytes, ResolvedTy::Bytes],
            &HashSet::from([0]),
        );
        assert!(
            derivation.allowed.contains(&b),
            "the co-owning local must still earn its scope-exit drop; allowed: {:?}",
            derivation.allowed
        );
        assert_eq!(
            derivation.retain_sites,
            vec![BytesRetainSite {
                block: 0,
                instr_index: 0,
                placement: BytesRetainPlacement::Before,
                value: Place::Local(0),
                required_bindings: vec![b],
            }],
            "b co-owning a borrowed param buffer must retain, gated on b's own drop"
        );
    }

    /// A240 Stage S1 hole #2: `let a = ..; let b = a; return a;`. Both `a` and
    /// `b` co-own ONE buffer; `a` is RETURNED (moved out — its reference handed
    /// to the caller, so `a` leaves `owned_locals`) while `b` drops locally.
    /// Dropping `b` unretained double-frees the returned buffer. The scan must
    /// mint `b`'s retain even though the source `a` is no longer an owned
    /// candidate — matched via `binding_local_bases` (a named binding slot),
    /// NOT via `borrowed_alias_of`. The return move (`ReturnSlot ← Local(1)`)
    /// mints nothing itself (its dest is not a `Place::Local`).
    #[test]
    fn derive_local_bytes_drop_allowed_marks_owned_partner_escape_coown() {
        let a = BindingId(601);
        let b = BindingId(602);
        // Only `b` is owned at scope exit; `a` was consumed by the return.
        let owned = vec![(b, "b".to_string(), ResolvedTy::Bytes)];
        let binding_locals = HashMap::from([(a, Place::Local(1)), (b, Place::Local(2))]);
        let block = BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![
                Instr::Move {
                    dest: Place::Local(2),
                    src: Place::Local(1),
                },
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(1),
                },
            ],
            terminator: Terminator::Return,
        };
        let derivation = derive_local_bytes_drop_allowed(
            &[block],
            &HashMap::new(),
            &owned,
            &binding_locals,
            &[ResolvedTy::Unit, ResolvedTy::Bytes, ResolvedTy::Bytes],
            &HashSet::new(),
        );
        assert!(
            derivation.allowed.contains(&b),
            "the surviving local co-owner must still earn its scope-exit drop; allowed: {:?}",
            derivation.allowed
        );
        assert_eq!(
            derivation.retain_sites,
            vec![BytesRetainSite {
                block: 0,
                instr_index: 0,
                placement: BytesRetainPlacement::Before,
                value: Place::Local(1),
                required_bindings: vec![b],
            }],
            "b co-owning a returned buffer must retain, gated on b's own drop"
        );
    }

    #[test]
    fn derive_local_bytes_drop_allowed_marks_container_element_read() {
        let (owned_locals, binding_locals, binding, local_tys) = bytes_prover_fixture();
        let get = crate::model::RuntimeCall::new(
            "hew_vec_get_owned",
            vec![Place::Local(5), Place::Local(6)],
            Some(Place::Local(1)),
        )
        .expect("hew_vec_get_owned is allowlisted");
        let block = BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![Instr::CallRuntimeAbi(get)],
            terminator: Terminator::Return,
        };
        let derivation = derive_local_bytes_drop_allowed(
            &[block],
            &HashMap::new(),
            &owned_locals,
            &binding_locals,
            &local_tys,
            &HashSet::new(),
        );
        assert!(derivation.allowed.contains(&binding));
        assert_eq!(
            derivation.retain_sites,
            vec![BytesRetainSite {
                block: 0,
                instr_index: 0,
                placement: BytesRetainPlacement::After,
                value: Place::Local(1),
                required_bindings: Vec::new(),
            }]
        );
    }
}
