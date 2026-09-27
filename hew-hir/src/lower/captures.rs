//! Closure capture collection.

use super::*;

// ── Lambda-actor capture walker ─────────────────────────────────────────────

/// Recursive walk over a lowered actor-lambda body collecting captures.
///
/// A capture is any `HirExprKind::BindingRef { resolved: Binding(id) }`
/// whose `id` is NOT one of the lambda's own parameter bindings. The
/// first occurrence of each capture is recorded with a strength
/// classifier — `Weak` iff the id matches the lambda's let-binding id
/// passed in `self_id` (the forward-bound recursive-self case, §5.9
/// ratification 2), `Strong` otherwise. Subsequent references to the
/// same binding are skipped — codegen wires one runtime capture per
/// binding, not one per use site.
///
/// The walk is exhaustive over `HirExprKind` variants; nested lambdas
/// (an actor lambda inside another actor lambda's body) are NOT
/// descended into — their captures belong to the inner lambda's
/// frame and are reported separately when that lambda's own
/// `lower_spawn_lambda_actor` call ran. The nested lambda's appearance
/// in the outer body's capture set, if any, would come from the
/// outer body referencing a name that the inner lambda also referenced;
/// but `BindingRef` lives only in the outer body's expression tree, so
/// this falls out naturally from not descending into the inner body.
#[expect(
    clippy::too_many_lines,
    reason = "single-pass HIR walker; one arm per HirExprKind variant by design"
)]
pub(super) fn collect_captures_walk(
    expr: &HirExpr,
    param_ids: &std::collections::HashSet<BindingId>,
    seen: &mut std::collections::HashSet<BindingId>,
    captures: &mut Vec<HirLambdaCapture>,
    self_id: Option<BindingId>,
) {
    match &expr.kind {
        HirExprKind::RcIntrinsic {
            receiver, value, ..
        } => {
            for operand in receiver.iter().chain(value.iter()) {
                collect_captures_walk(operand, param_ids, seen, captures, self_id);
            }
        }
        HirExprKind::BindingRef {
            name,
            resolved: ResolvedRef::Binding(id),
        } => {
            // Parameters of the current lambda are intra-frame
            // bindings, never captures from the enclosing scope.
            if param_ids.contains(id) {
                return;
            }
            if !seen.insert(*id) {
                return;
            }
            let kind = if Some(*id) == self_id {
                HirCaptureKind::Weak
            } else {
                HirCaptureKind::Strong
            };
            captures.push(HirLambdaCapture {
                binding: *id,
                name: name.clone(),
                ty: expr.ty.clone(),
                kind,
            });
        }
        // Empty-body terminals: nothing to walk.
        //   - BindingRef without a resolved binding (Item / Unresolved):
        //     does not produce a capture from the enclosing scope.
        //   - Literal: no sub-expressions.
        //   - SpawnLambdaActor (nested): its captures belong to its own
        //     frame and were classified when the inner lambda lowered.
        //   - Unsupported: nothing to walk.
        //   - MachineFieldAccess: implicit `self` receiver, no sub-expressions.
        HirExprKind::BindingRef { .. }
        | HirExprKind::ContextReader { .. }
        | HirExprKind::Literal(_)
        | HirExprKind::RegexLiteralRef { .. }
        | HirExprKind::SpawnLambdaActor { .. }
        | HirExprKind::Closure { .. }
        | HirExprKind::Continue { .. }
        | HirExprKind::ActorSelf
        | HirExprKind::Unsupported(_) => {}
        HirExprKind::Binary { left, right, .. } | HirExprKind::IdentityCompare { left, right } => {
            collect_captures_walk(left, param_ids, seen, captures, self_id);
            collect_captures_walk(right, param_ids, seen, captures, self_id);
        }
        HirExprKind::Unary { operand, .. } | HirExprKind::WireCodec { operand, .. } => {
            collect_captures_walk(operand, param_ids, seen, captures, self_id);
        }
        HirExprKind::ConnAwaitRead { conn, .. } => {
            collect_captures_walk(conn, param_ids, seen, captures, self_id);
        }
        HirExprKind::AwaitRestart { child } | HirExprKind::AwaitTask { operand: child, .. } => {
            collect_captures_walk(child, param_ids, seen, captures, self_id);
        }
        HirExprKind::ListenerAwaitAccept { listener, .. } => {
            collect_captures_walk(listener, param_ids, seen, captures, self_id);
        }
        HirExprKind::ArrayRepeat { value }
        | HirExprKind::NumericCast { value, .. }
        | HirExprKind::SaturatingWidthCast { value, .. }
        | HirExprKind::TryWidthCast { value, .. }
        | HirExprKind::CoerceToDynTrait { value, .. } => {
            collect_captures_walk(value, param_ids, seen, captures, self_id);
        }
        HirExprKind::TupleLiteral { elements } | HirExprKind::ArrayLiteral { elements } => {
            for elem in elements {
                collect_captures_walk(elem, param_ids, seen, captures, self_id);
            }
        }
        HirExprKind::Call { callee, args, .. } => {
            collect_captures_walk(callee, param_ids, seen, captures, self_id);
            for arg in args {
                collect_captures_walk(arg, param_ids, seen, captures, self_id);
            }
        }
        HirExprKind::Spawn { args, .. } => {
            for (_, arg) in args {
                collect_captures_walk(arg, param_ids, seen, captures, self_id);
            }
        }
        HirExprKind::ActorMessage { receiver, args, .. }
        | HirExprKind::ActorDelivery { receiver, args, .. }
        | HirExprKind::ActorAsk { receiver, args, .. }
        | HirExprKind::ActorGenStream { receiver, args, .. }
        | HirExprKind::CallDynMethod { receiver, args, .. }
        | HirExprKind::ResolvedImplCall { receiver, args, .. }
        | HirExprKind::CallTraitMethodStatic { receiver, args, .. }
        | HirExprKind::VarSelfMethodCall { receiver, args, .. } => {
            collect_captures_walk(receiver, param_ids, seen, captures, self_id);
            for arg in args {
                collect_captures_walk(arg, param_ids, seen, captures, self_id);
            }
        }
        HirExprKind::RemoteActorAsk {
            receiver,
            msg,
            timeout_ms,
            ..
        } => {
            collect_captures_walk(receiver, param_ids, seen, captures, self_id);
            collect_captures_walk(msg, param_ids, seen, captures, self_id);
            collect_captures_walk(timeout_ms, param_ids, seen, captures, self_id);
        }
        HirExprKind::RemoteActorSend { receiver, msg } => {
            collect_captures_walk(receiver, param_ids, seen, captures, self_id);
            collect_captures_walk(msg, param_ids, seen, captures, self_id);
        }
        HirExprKind::Block(block)
        | HirExprKind::Scope { body: block }
        | HirExprKind::Race { body: block }
        | HirExprKind::ForkBlock { body: block, .. }
        | HirExprKind::GenBlock { body: block, .. } => {
            collect_captures_walk_block(block, param_ids, seen, captures, self_id);
        }
        HirExprKind::Yield { value, .. }
        | HirExprKind::Break { value, .. }
        | HirExprKind::Return { value } => {
            if let Some(value) = value {
                collect_captures_walk(value, param_ids, seen, captures, self_id);
            }
        }
        HirExprKind::ScopeRecovery {
            scope,
            error,
            handler,
        } => {
            collect_captures_walk(scope, param_ids, seen, captures, self_id);
            let mut handler_locals = param_ids.clone();
            handler_locals.insert(error.id);
            collect_captures_walk(handler, &handler_locals, seen, captures, self_id);
        }
        HirExprKind::ScopeDeadline { duration, body } => {
            collect_captures_walk(duration, param_ids, seen, captures, self_id);
            collect_captures_walk_block(body, param_ids, seen, captures, self_id);
        }
        HirExprKind::If {
            condition,
            then_expr,
            else_expr,
        } => {
            collect_captures_walk(condition, param_ids, seen, captures, self_id);
            collect_captures_walk(then_expr, param_ids, seen, captures, self_id);
            if let Some(else_expr) = else_expr {
                collect_captures_walk(else_expr, param_ids, seen, captures, self_id);
            }
        }
        HirExprKind::StructInit { fields, base, .. } => {
            for (_, field) in fields {
                collect_captures_walk(field, param_ids, seen, captures, self_id);
            }
            if let Some(base) = base {
                collect_captures_walk(base, param_ids, seen, captures, self_id);
            }
        }
        HirExprKind::FieldAccess { object, .. } => {
            collect_captures_walk(object, param_ids, seen, captures, self_id);
        }
        HirExprKind::Select(select) => {
            for arm in &select.arms {
                match &arm.kind {
                    HirSelectArmKind::StreamNext { stream } => {
                        collect_captures_walk(stream, param_ids, seen, captures, self_id);
                    }
                    HirSelectArmKind::ActorAsk { call } => {
                        collect_captures_walk(call, param_ids, seen, captures, self_id);
                    }
                    HirSelectArmKind::TaskAwait { task } => {
                        collect_captures_walk(task, param_ids, seen, captures, self_id);
                    }
                    HirSelectArmKind::AfterTimer { duration } => {
                        collect_captures_walk(duration, param_ids, seen, captures, self_id);
                    }
                }
                collect_captures_walk(&arm.body, param_ids, seen, captures, self_id);
            }
        }
        HirExprKind::TupleIndex { tuple, .. } => {
            collect_captures_walk(tuple, param_ids, seen, captures, self_id);
        }
        HirExprKind::Index { container, index }
        | HirExprKind::BorrowedIndex { container, index } => {
            collect_captures_walk(container, param_ids, seen, captures, self_id);
            collect_captures_walk(index, param_ids, seen, captures, self_id);
        }
        HirExprKind::Slice {
            container,
            start,
            end,
        } => {
            collect_captures_walk(container, param_ids, seen, captures, self_id);
            if let Some(s) = start {
                collect_captures_walk(s, param_ids, seen, captures, self_id);
            }
            if let Some(e) = end {
                collect_captures_walk(e, param_ids, seen, captures, self_id);
            }
        }
        HirExprKind::CancellationTokenIsCancelled { receiver }
        | HirExprKind::GeneratorNext { receiver, .. }
        | HirExprKind::RecordCloneCall { src: receiver, .. }
        | HirExprKind::SubsumedValue {
            source: receiver, ..
        } => {
            collect_captures_walk(receiver, param_ids, seen, captures, self_id);
        }
        HirExprKind::StreamRecvAwait { stream, .. } => {
            collect_captures_walk(stream, param_ids, seen, captures, self_id);
        }
        HirExprKind::MachineVariantCtor { payload, .. } => {
            // Machine state constructors are not expected inside lambda bodies.
            // Walk payload fields defensively for exhaustiveness.
            if let Some(fields) = payload {
                for (_, val) in fields {
                    collect_captures_walk(val, param_ids, seen, captures, self_id);
                }
            }
        }
        HirExprKind::While {
            condition, body, ..
        } => {
            collect_captures_walk(condition, param_ids, seen, captures, self_id);
            collect_captures_walk_block(body, param_ids, seen, captures, self_id);
        }
        HirExprKind::ForRange {
            start,
            end,
            step,
            body,
            ..
        } => {
            collect_captures_walk(start, param_ids, seen, captures, self_id);
            collect_captures_walk(end, param_ids, seen, captures, self_id);
            collect_captures_walk(step, param_ids, seen, captures, self_id);
            collect_captures_walk_block(body, param_ids, seen, captures, self_id);
        }
        HirExprKind::Match { scrutinee, arms } => {
            collect_captures_walk(scrutinee, param_ids, seen, captures, self_id);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    collect_captures_walk(guard, param_ids, seen, captures, self_id);
                }
                collect_captures_walk(&arm.body, param_ids, seen, captures, self_id);
            }
        }
        HirExprKind::Loop { body, .. } => {
            collect_captures_walk_block(body, param_ids, seen, captures, self_id);
        }
    }
}

#[allow(
    clippy::too_many_lines,
    reason = "single exhaustive walker over HirExprKind variants; splitting risks traversal gaps"
)]
pub(super) fn collect_general_closure_captures_walk(
    expr: &HirExpr,
    outer_bindings: &HashMap<BindingId, OuterClosureBinding>,
    seen: &mut HashSet<BindingId>,
    captures: &mut Vec<ClosureCaptureCandidate>,
) {
    match &expr.kind {
        HirExprKind::RcIntrinsic {
            receiver, value, ..
        } => {
            for operand in receiver.iter().chain(value.iter()) {
                collect_general_closure_captures_walk(operand, outer_bindings, seen, captures);
            }
        }
        HirExprKind::BindingRef {
            name,
            resolved: ResolvedRef::Binding(id),
        } if outer_bindings.contains_key(id) => {
            if seen.insert(*id) {
                let span = outer_bindings
                    .get(id)
                    .map(|(_, _, span)| span.clone())
                    .unwrap_or_default();
                captures.push((*id, name.clone(), span));
            }
        }
        HirExprKind::Closure {
            captures: nested, ..
        } => {
            for capture in nested {
                if outer_bindings.contains_key(&capture.binding) && seen.insert(capture.binding) {
                    let span = outer_bindings
                        .get(&capture.binding)
                        .map(|(_, _, span)| span.clone())
                        .unwrap_or_default();
                    captures.push((capture.binding, capture.name.clone(), span));
                }
            }
        }
        HirExprKind::BindingRef { .. }
        | HirExprKind::ContextReader { .. }
        | HirExprKind::Literal(_)
        | HirExprKind::RegexLiteralRef { .. }
        | HirExprKind::SpawnLambdaActor { .. }
        | HirExprKind::Continue { .. }
        | HirExprKind::ActorSelf
        | HirExprKind::Unsupported(_) => {}
        HirExprKind::Binary { left, right, .. } | HirExprKind::IdentityCompare { left, right } => {
            collect_general_closure_captures_walk(left, outer_bindings, seen, captures);
            collect_general_closure_captures_walk(right, outer_bindings, seen, captures);
        }
        HirExprKind::Unary { operand, .. } | HirExprKind::WireCodec { operand, .. } => {
            collect_general_closure_captures_walk(operand, outer_bindings, seen, captures);
        }
        HirExprKind::ConnAwaitRead { conn, .. } => {
            collect_general_closure_captures_walk(conn, outer_bindings, seen, captures);
        }
        HirExprKind::AwaitRestart { child } | HirExprKind::AwaitTask { operand: child, .. } => {
            collect_general_closure_captures_walk(child, outer_bindings, seen, captures);
        }
        HirExprKind::ListenerAwaitAccept { listener, .. } => {
            collect_general_closure_captures_walk(listener, outer_bindings, seen, captures);
        }
        HirExprKind::StreamRecvAwait { stream, .. } => {
            collect_general_closure_captures_walk(stream, outer_bindings, seen, captures);
        }
        HirExprKind::ArrayRepeat { value }
        | HirExprKind::NumericCast { value, .. }
        | HirExprKind::SaturatingWidthCast { value, .. }
        | HirExprKind::TryWidthCast { value, .. }
        | HirExprKind::CoerceToDynTrait { value, .. } => {
            collect_general_closure_captures_walk(value, outer_bindings, seen, captures);
        }
        HirExprKind::TupleLiteral { elements } | HirExprKind::ArrayLiteral { elements } => {
            for elem in elements {
                collect_general_closure_captures_walk(elem, outer_bindings, seen, captures);
            }
        }
        HirExprKind::Call { callee, args, .. } => {
            collect_general_closure_captures_walk(callee, outer_bindings, seen, captures);
            for arg in args {
                collect_general_closure_captures_walk(arg, outer_bindings, seen, captures);
            }
        }
        HirExprKind::Spawn { args, .. } => {
            for (_, arg) in args {
                collect_general_closure_captures_walk(arg, outer_bindings, seen, captures);
            }
        }
        HirExprKind::ActorMessage { receiver, args, .. }
        | HirExprKind::ActorDelivery { receiver, args, .. }
        | HirExprKind::ActorAsk { receiver, args, .. }
        | HirExprKind::ActorGenStream { receiver, args, .. }
        | HirExprKind::CallDynMethod { receiver, args, .. }
        | HirExprKind::ResolvedImplCall { receiver, args, .. }
        | HirExprKind::CallTraitMethodStatic { receiver, args, .. }
        | HirExprKind::VarSelfMethodCall { receiver, args, .. } => {
            collect_general_closure_captures_walk(receiver, outer_bindings, seen, captures);
            for arg in args {
                collect_general_closure_captures_walk(arg, outer_bindings, seen, captures);
            }
        }
        HirExprKind::RemoteActorAsk {
            receiver,
            msg,
            timeout_ms,
            ..
        } => {
            collect_general_closure_captures_walk(receiver, outer_bindings, seen, captures);
            collect_general_closure_captures_walk(msg, outer_bindings, seen, captures);
            collect_general_closure_captures_walk(timeout_ms, outer_bindings, seen, captures);
        }
        HirExprKind::RemoteActorSend { receiver, msg } => {
            collect_general_closure_captures_walk(receiver, outer_bindings, seen, captures);
            collect_general_closure_captures_walk(msg, outer_bindings, seen, captures);
        }
        HirExprKind::Block(block)
        | HirExprKind::Scope { body: block }
        | HirExprKind::Race { body: block }
        | HirExprKind::ForkBlock { body: block, .. }
        | HirExprKind::GenBlock { body: block, .. } => {
            collect_general_closure_captures_walk_block(block, outer_bindings, seen, captures);
        }
        HirExprKind::Yield { value, .. }
        | HirExprKind::Break { value, .. }
        | HirExprKind::Return { value } => {
            if let Some(value) = value {
                collect_general_closure_captures_walk(value, outer_bindings, seen, captures);
            }
        }
        HirExprKind::ScopeRecovery { scope, handler, .. } => {
            collect_general_closure_captures_walk(scope, outer_bindings, seen, captures);
            collect_general_closure_captures_walk(handler, outer_bindings, seen, captures);
        }
        HirExprKind::ScopeDeadline { duration, body } => {
            collect_general_closure_captures_walk(duration, outer_bindings, seen, captures);
            collect_general_closure_captures_walk_block(body, outer_bindings, seen, captures);
        }
        HirExprKind::If {
            condition,
            then_expr,
            else_expr,
        } => {
            collect_general_closure_captures_walk(condition, outer_bindings, seen, captures);
            collect_general_closure_captures_walk(then_expr, outer_bindings, seen, captures);
            if let Some(else_expr) = else_expr {
                collect_general_closure_captures_walk(else_expr, outer_bindings, seen, captures);
            }
        }
        HirExprKind::StructInit { fields, base, .. } => {
            for (_, field) in fields {
                collect_general_closure_captures_walk(field, outer_bindings, seen, captures);
            }
            if let Some(base) = base {
                collect_general_closure_captures_walk(base, outer_bindings, seen, captures);
            }
        }
        HirExprKind::FieldAccess { object, .. } => {
            collect_general_closure_captures_walk(object, outer_bindings, seen, captures);
        }
        HirExprKind::Select(select) => {
            for arm in &select.arms {
                match &arm.kind {
                    HirSelectArmKind::StreamNext { stream } => {
                        collect_general_closure_captures_walk(
                            stream,
                            outer_bindings,
                            seen,
                            captures,
                        );
                    }
                    HirSelectArmKind::ActorAsk { call } => {
                        collect_general_closure_captures_walk(call, outer_bindings, seen, captures);
                    }
                    HirSelectArmKind::TaskAwait { task } => {
                        collect_general_closure_captures_walk(task, outer_bindings, seen, captures);
                    }
                    HirSelectArmKind::AfterTimer { duration } => {
                        collect_general_closure_captures_walk(
                            duration,
                            outer_bindings,
                            seen,
                            captures,
                        );
                    }
                }
                collect_general_closure_captures_walk(&arm.body, outer_bindings, seen, captures);
            }
        }
        HirExprKind::TupleIndex { tuple, .. } => {
            collect_general_closure_captures_walk(tuple, outer_bindings, seen, captures);
        }
        HirExprKind::Index { container, index }
        | HirExprKind::BorrowedIndex { container, index } => {
            collect_general_closure_captures_walk(container, outer_bindings, seen, captures);
            collect_general_closure_captures_walk(index, outer_bindings, seen, captures);
        }
        HirExprKind::Slice {
            container,
            start,
            end,
        } => {
            collect_general_closure_captures_walk(container, outer_bindings, seen, captures);
            if let Some(s) = start {
                collect_general_closure_captures_walk(s, outer_bindings, seen, captures);
            }
            if let Some(e) = end {
                collect_general_closure_captures_walk(e, outer_bindings, seen, captures);
            }
        }
        HirExprKind::CancellationTokenIsCancelled { receiver }
        | HirExprKind::GeneratorNext { receiver, .. }
        | HirExprKind::RecordCloneCall { src: receiver, .. }
        | HirExprKind::SubsumedValue {
            source: receiver, ..
        } => {
            collect_general_closure_captures_walk(receiver, outer_bindings, seen, captures);
        }
        HirExprKind::MachineVariantCtor { payload, .. } => {
            // Machine state constructors cannot appear inside closure bodies.
            // Walk payload fields defensively for exhaustiveness.
            if let Some(fields) = payload {
                for (_, val) in fields {
                    collect_general_closure_captures_walk(val, outer_bindings, seen, captures);
                }
            }
        }
        HirExprKind::While {
            condition, body, ..
        } => {
            collect_general_closure_captures_walk(condition, outer_bindings, seen, captures);
            collect_general_closure_captures_walk_block(body, outer_bindings, seen, captures);
        }
        HirExprKind::ForRange {
            start,
            end,
            step,
            body,
            ..
        } => {
            collect_general_closure_captures_walk(start, outer_bindings, seen, captures);
            collect_general_closure_captures_walk(end, outer_bindings, seen, captures);
            collect_general_closure_captures_walk(step, outer_bindings, seen, captures);
            collect_general_closure_captures_walk_block(body, outer_bindings, seen, captures);
        }
        HirExprKind::Match { scrutinee, arms } => {
            collect_general_closure_captures_walk(scrutinee, outer_bindings, seen, captures);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    collect_general_closure_captures_walk(guard, outer_bindings, seen, captures);
                }
                collect_general_closure_captures_walk(&arm.body, outer_bindings, seen, captures);
            }
        }
        HirExprKind::Loop { body, .. } => {
            collect_general_closure_captures_walk_block(body, outer_bindings, seen, captures);
        }
    }
}

pub(super) fn collect_general_closure_captures_walk_block(
    block: &HirBlock,
    outer_bindings: &HashMap<BindingId, OuterClosureBinding>,
    seen: &mut HashSet<BindingId>,
    captures: &mut Vec<ClosureCaptureCandidate>,
) {
    for stmt in &block.statements {
        match &stmt.kind {
            HirStmtKind::Let(_, Some(value)) | HirStmtKind::Destructure { value, .. } => {
                collect_general_closure_captures_walk(value, outer_bindings, seen, captures);
            }
            HirStmtKind::Expr(expr) | HirStmtKind::Return(Some(expr)) => {
                collect_general_closure_captures_walk(expr, outer_bindings, seen, captures);
            }
            HirStmtKind::Assign { target, value, .. } => {
                collect_general_closure_captures_walk(target, outer_bindings, seen, captures);
                collect_general_closure_captures_walk(value, outer_bindings, seen, captures);
            }
            HirStmtKind::Let(_, None) | HirStmtKind::Return(None) => {}
            HirStmtKind::Defer { body, .. } => {
                collect_general_closure_captures_walk(body, outer_bindings, seen, captures);
            }
        }
    }
    if let Some(tail) = &block.tail {
        collect_general_closure_captures_walk(tail, outer_bindings, seen, captures);
    }
}

pub(super) fn collect_captures_walk_block(
    block: &HirBlock,
    param_ids: &std::collections::HashSet<BindingId>,
    seen: &mut std::collections::HashSet<BindingId>,
    captures: &mut Vec<HirLambdaCapture>,
    self_id: Option<BindingId>,
) {
    // A lambda captures only bindings from an enclosing frame.  Keep a
    // lexical exclusion set for this block: each `let` initializer still sees
    // the outer scope, then its newly declared binding becomes local for the
    // remaining statements and tail.  Nested blocks receive a clone through
    // their recursive call, so shadowing never leaks back out.
    let mut locally_bound = param_ids.clone();
    for stmt in &block.statements {
        match &stmt.kind {
            HirStmtKind::Let(binding, Some(value)) => {
                collect_captures_walk(value, &locally_bound, seen, captures, self_id);
                locally_bound.insert(binding.id);
            }
            HirStmtKind::Destructure { value, fields } => {
                collect_captures_walk(value, &locally_bound, seen, captures, self_id);
                locally_bound.extend(
                    fields
                        .iter()
                        .filter_map(|field| field.binding.as_ref())
                        .map(|binding| binding.id),
                );
            }
            HirStmtKind::Let(binding, None) => {
                locally_bound.insert(binding.id);
            }
            HirStmtKind::Expr(expr) | HirStmtKind::Return(Some(expr)) => {
                collect_captures_walk(expr, &locally_bound, seen, captures, self_id);
            }
            HirStmtKind::Assign { target, value, .. } => {
                collect_captures_walk(target, &locally_bound, seen, captures, self_id);
                collect_captures_walk(value, &locally_bound, seen, captures, self_id);
            }
            HirStmtKind::Return(None) => {}
            HirStmtKind::Defer { body, .. } => {
                collect_captures_walk(body, &locally_bound, seen, captures, self_id);
            }
        }
    }
    if let Some(tail) = &block.tail {
        collect_captures_walk(tail, &locally_bound, seen, captures, self_id);
    }
}
