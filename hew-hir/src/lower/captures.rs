//! Closure capture collection.

use super::*;

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
                    .map(|(_, _, span, _)| span.clone())
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
                        .map(|(_, _, span, _)| span.clone())
                        .unwrap_or_default();
                    captures.push((capture.binding, capture.name.clone(), span));
                }
            }
        }
        HirExprKind::BindingRef { .. }
        | HirExprKind::ContextReader { .. }
        | HirExprKind::Literal(_)
        | HirExprKind::RegexLiteralRef { .. }
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
        HirExprKind::AwaitTask { operand: child, .. } => {
            collect_general_closure_captures_walk(child, outer_bindings, seen, captures);
        }
        HirExprKind::ArrayRepeat { value }
        | HirExprKind::NumericCast { value, .. }
        | HirExprKind::SaturatingWidthCast { value, .. }
        | HirExprKind::TryWidthCast { value, .. }
        | HirExprKind::CoerceToDynTrait { value, .. } => {
            collect_general_closure_captures_walk(value, outer_bindings, seen, captures);
        }
        HirExprKind::TaskRace {
            members: elements, ..
        }
        | HirExprKind::TupleLiteral { elements }
        | HirExprKind::ArrayLiteral { elements } => {
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
        | HirExprKind::VarSelfMethodCall { receiver, args, .. } => {
            collect_general_closure_captures_walk(receiver, outer_bindings, seen, captures);
            for arg in args {
                collect_general_closure_captures_walk(arg, outer_bindings, seen, captures);
            }
        }
        HirExprKind::CallTraitMethodStatic { receiver, args, .. } => {
            for operand in receiver.receiver().into_iter().chain(args) {
                collect_general_closure_captures_walk(operand, outer_bindings, seen, captures);
            }
        }
        HirExprKind::Block(block)
        | HirExprKind::Scope { body: block, .. }
        | HirExprKind::ForkBlock { body: block, .. }
        | HirExprKind::GenBlock { body: block, .. } => {
            collect_general_closure_captures_walk_block(block, outer_bindings, seen, captures);
        }
        HirExprKind::Yield { value, .. }
        | HirExprKind::Break { value, .. }
        | HirExprKind::Return { value, .. } => {
            if let Some(value) = value {
                collect_general_closure_captures_walk(value, outer_bindings, seen, captures);
            }
        }
        HirExprKind::ScopeRecovery { scope, handler, .. } => {
            collect_general_closure_captures_walk(scope, outer_bindings, seen, captures);
            collect_general_closure_captures_walk(handler, outer_bindings, seen, captures);
        }
        HirExprKind::ScopeDeadline { duration, body, .. } => {
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
                    HirSelectArmKind::Call { call } => {
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
        HirExprKind::GeneratorNext { receiver, .. }
        | HirExprKind::RecordCloneCall { src: receiver, .. }
        | HirExprKind::CallableSpecialize {
            source: receiver, ..
        }
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
        HirExprKind::RangeValue { start, end, .. } => {
            collect_general_closure_captures_walk(start, outer_bindings, seen, captures);
            collect_general_closure_captures_walk(end, outer_bindings, seen, captures);
        }
        HirExprKind::RangeAdapt { range, adapter } => {
            collect_general_closure_captures_walk(range, outer_bindings, seen, captures);
            if let crate::HirRangeAdapter::Step(step) = adapter {
                collect_general_closure_captures_walk(step, outer_bindings, seen, captures);
            }
        }
        HirExprKind::ForRange { range, body, .. } => {
            collect_general_closure_captures_walk(range, outer_bindings, seen, captures);
            collect_general_closure_captures_walk_block(body, outer_bindings, seen, captures);
        }
        HirExprKind::Match { scrutinee, arms } => {
            collect_general_closure_captures_walk(scrutinee, outer_bindings, seen, captures);
            for arm in arms {
                for statement in &arm.pattern_bindings {
                    if let HirStmtKind::Destructure { value, .. } = &statement.kind {
                        collect_general_closure_captures_walk(
                            value,
                            outer_bindings,
                            seen,
                            captures,
                        );
                    }
                }
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
