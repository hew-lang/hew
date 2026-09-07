//! Conservative syntactic escape classification for closure bindings.

use hew_parser::ast::{Block, Expr, Spanned, Stmt, StringPart};

use super::types::{ClosureEscapeFact, ClosureEscapeKind, ClosureEscapeRule};

// ── escape classifier ──────────────────────────────────────────────────

#[derive(Default)]
struct EscapeAccumulator {
    /// At least one use seen.
    any_use: bool,
    /// Use is inside a `fork { ... }` body — Forked wins over Escapes.
    forked_use: bool,
    /// Some use is non-direct-call (Escapes unless Forked also fires).
    nonlocal_use: bool,
    /// First non-local rule that determines the published escape fact.
    nonlocal_rule: Option<ClosureEscapeRule>,
}

impl EscapeAccumulator {
    fn record_nonlocal(&mut self, rule: ClosureEscapeRule) {
        self.nonlocal_use = true;
        if self.nonlocal_rule.is_none() {
            self.nonlocal_rule = Some(rule);
        }
    }
}

/// Classify a closure literal whose introducing `let <name> = |...| ...`
/// statement is at index `let_index` inside `block_stmts`. Walks every
/// statement after the let plus the block's trailing expression looking
/// for uses of `binding_name`.
pub(super) fn classify_closure_escape_in_block(
    block_stmts: &[Spanned<Stmt>],
    block_trailing: Option<&Spanned<Expr>>,
    let_index: usize,
    binding_name: &str,
    outer_in_fork: bool,
) -> ClosureEscapeFact {
    let mut acc = EscapeAccumulator::default();
    for (stmt, _) in block_stmts.iter().skip(let_index + 1) {
        esc_visit_stmt(stmt, binding_name, outer_in_fork, &mut acc);
    }
    if let Some(tail) = block_trailing {
        esc_visit_expr(&tail.0, binding_name, outer_in_fork, &mut acc, true);
    }

    if !acc.any_use {
        // No use-sites — conservative default. The closure
        // never runs and never escapes, but the classifier cannot
        // *positively* prove `Local` (the introduction may be a typo
        // or dead branch). Conservative call: `Escapes` /
        // `NoStaticBinding`.
        return ClosureEscapeFact {
            kind: ClosureEscapeKind::Escapes,
            rule: ClosureEscapeRule::NoStaticBinding,
        };
    }
    if acc.forked_use {
        return ClosureEscapeFact {
            kind: ClosureEscapeKind::Forked,
            rule: ClosureEscapeRule::InsideForkBlock,
        };
    }
    if acc.nonlocal_use {
        return ClosureEscapeFact {
            kind: ClosureEscapeKind::Escapes,
            rule: acc
                .nonlocal_rule
                .unwrap_or(ClosureEscapeRule::PassedToHigherOrder),
        };
    }
    ClosureEscapeFact {
        kind: ClosureEscapeKind::Local,
        rule: ClosureEscapeRule::DirectCallOnly,
    }
}

fn esc_visit_stmt(stmt: &Stmt, name: &str, in_fork: bool, acc: &mut EscapeAccumulator) {
    match stmt {
        Stmt::Let { value, .. } | Stmt::Var { value, .. } => {
            if let Some((e, _)) = value {
                // RHS of a binding — any reference to our closure name
                // here is a store into another binding.
                esc_visit_expr(e, name, in_fork, acc, /* is_tail = */ true);
            }
        }
        Stmt::Assign { target, value, .. } => {
            esc_visit_expr(&target.0, name, in_fork, acc, false);
            esc_visit_expr(&value.0, name, in_fork, acc, true);
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            esc_visit_expr(&condition.0, name, in_fork, acc, false);
            esc_visit_block(then_block, name, in_fork, acc);
            if let Some(eb) = else_block {
                if let Some(b) = &eb.block {
                    esc_visit_block(b, name, in_fork, acc);
                }
                if let Some(if_stmt) = &eb.if_stmt {
                    esc_visit_stmt(&if_stmt.0, name, in_fork, acc);
                }
            }
        }
        Stmt::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            esc_visit_expr(&expr.0, name, in_fork, acc, false);
            esc_visit_block(body, name, in_fork, acc);
            if let Some(b) = else_body {
                esc_visit_block(b, name, in_fork, acc);
            }
        }
        Stmt::Match { scrutinee, arms } => {
            esc_visit_expr(&scrutinee.0, name, in_fork, acc, false);
            for arm in arms {
                if let Some((g, _)) = &arm.guard {
                    esc_visit_expr(g, name, in_fork, acc, false);
                }
                esc_visit_expr(&arm.body.0, name, in_fork, acc, false);
            }
        }
        Stmt::Loop { body, .. } => esc_visit_block(body, name, in_fork, acc),
        Stmt::For { iterable, body, .. } => {
            esc_visit_expr(&iterable.0, name, in_fork, acc, false);
            esc_visit_block(body, name, in_fork, acc);
        }
        Stmt::While {
            condition, body, ..
        } => {
            esc_visit_expr(&condition.0, name, in_fork, acc, false);
            esc_visit_block(body, name, in_fork, acc);
        }
        Stmt::WhileLet { expr, body, .. } => {
            esc_visit_expr(&expr.0, name, in_fork, acc, false);
            esc_visit_block(body, name, in_fork, acc);
        }
        Stmt::Break { value, .. } => {
            if let Some((e, _)) = value {
                esc_visit_expr(e, name, in_fork, acc, true);
            }
        }
        Stmt::Continue { .. } => {}
        Stmt::Return(opt) => {
            if let Some((e, _)) = opt {
                // A bare reference to our closure-bound name in a
                // return statement is the textbook `Returned` rule.
                if let Expr::Identifier(n) = e {
                    if n == name {
                        acc.any_use = true;
                        if in_fork {
                            acc.forked_use = true;
                        } else {
                            acc.record_nonlocal(ClosureEscapeRule::Returned);
                        }
                        return;
                    }
                }
                esc_visit_expr(e, name, in_fork, acc, true);
            }
        }
        Stmt::Defer(boxed) => esc_visit_expr(&boxed.0, name, in_fork, acc, false),
        Stmt::Expression((e, _)) => esc_visit_expr(e, name, in_fork, acc, false),
    }
}

fn esc_visit_block(block: &Block, name: &str, in_fork: bool, acc: &mut EscapeAccumulator) {
    for (stmt, _) in &block.stmts {
        esc_visit_stmt(stmt, name, in_fork, acc);
    }
    if let Some(tail) = &block.trailing_expr {
        esc_visit_expr(&tail.0, name, in_fork, acc, true);
    }
}

/// Walk one expression looking for references to the closure-bound
/// `name`. `is_tail` is true when the value flows out of the closure's
/// introducing scope; used to distinguish `EscapesViaBlockValue` from a
/// generic `PassedToHigherOrder`.
#[allow(
    clippy::too_many_lines,
    clippy::match_same_arms,
    reason = "AST visitor over full Expr surface; arm-per-variant is the \
              clearest form even when some arms have identical bodies"
)]
fn esc_visit_expr(
    expr: &Expr,
    name: &str,
    in_fork: bool,
    acc: &mut EscapeAccumulator,
    is_tail: bool,
) {
    match expr {
        Expr::Identifier(n) if n == name => {
            acc.any_use = true;
            if in_fork {
                acc.forked_use = true;
            } else if is_tail {
                acc.record_nonlocal(ClosureEscapeRule::EscapesViaBlockValue);
            } else {
                acc.record_nonlocal(ClosureEscapeRule::StoredOrSent);
            }
        }
        Expr::Identifier(_) => {}
        Expr::Call { function, args, .. } => {
            // Direct call `name(args)` is the only safe shape.
            let direct = matches!(&function.0, Expr::Identifier(n) if n == name);
            if direct {
                acc.any_use = true;
                if in_fork {
                    acc.forked_use = true;
                }
                // Direct call: do NOT mark nonlocal. Args still need
                // to be scanned in case they pass the name elsewhere
                // (would be unusual but possible: `f(f)` style).
                for arg in args {
                    let (e, _) = arg.expr();
                    esc_visit_arg(e, name, in_fork, acc);
                }
                return;
            }
            esc_visit_expr(&function.0, name, in_fork, acc, false);
            for arg in args {
                let (e, _) = arg.expr();
                esc_visit_arg(e, name, in_fork, acc);
            }
        }
        Expr::MethodCall { receiver, args, .. } => {
            esc_visit_expr(&receiver.0, name, in_fork, acc, false);
            for arg in args {
                let (e, _) = arg.expr();
                esc_visit_arg(e, name, in_fork, acc);
            }
        }
        Expr::Binary { left, right, .. } => {
            esc_visit_expr(&left.0, name, in_fork, acc, false);
            esc_visit_expr(&right.0, name, in_fork, acc, false);
        }
        Expr::Coalesce { left, right } => {
            esc_visit_expr(&left.0, name, in_fork, acc, false);
            esc_visit_expr(&right.0, name, in_fork, acc, is_tail);
        }
        Expr::Handle {
            operand,
            error,
            body,
        } => {
            esc_visit_expr(&operand.0, name, in_fork, acc, false);
            if error.0 != name {
                esc_visit_expr(&body.0, name, in_fork, acc, is_tail);
            }
        }
        // `clone <operand>` is read-only and recurses like other unary forms.
        Expr::Unary { operand, .. } | Expr::Send(operand) | Expr::Clone(operand) => {
            esc_visit_expr(&operand.0, name, in_fork, acc, false);
        }
        Expr::ReturnError(value) => esc_visit_arg(&value.0, name, in_fork, acc),
        Expr::Literal(_)
        | Expr::QualifiedAssoc(_)
        | Expr::This
        | Expr::RegexLiteral(_)
        | Expr::ByteStringLiteral(_)
        | Expr::ByteArrayLiteral(_) => {}
        Expr::ContextVariant(context) => {
            if let Some(record) = &context.record {
                for (_, (value, _)) in &record.fields {
                    esc_visit_arg(value, name, in_fork, acc);
                }
                if let Some(base) = &record.base {
                    esc_visit_arg(&base.0, name, in_fork, acc);
                }
            }
        }
        Expr::GenericApplySuffix { target, .. } => {
            esc_visit_expr(&target.0, name, in_fork, acc, is_tail);
        }
        Expr::RecordInitSuffix {
            target,
            fields,
            base,
        } => {
            esc_visit_expr(&target.0, name, in_fork, acc, false);
            for (_, (value, _)) in fields {
                esc_visit_arg(value, name, in_fork, acc);
            }
            if let Some(base) = base {
                esc_visit_arg(&base.0, name, in_fork, acc);
            }
        }
        Expr::Tuple(items) | Expr::Array(items) => {
            for (e, _) in items {
                esc_visit_arg(e, name, in_fork, acc);
            }
        }
        Expr::ArrayRepeat { value, count } => {
            esc_visit_arg(&value.0, name, in_fork, acc);
            esc_visit_expr(&count.0, name, in_fork, acc, false);
        }
        Expr::MapLiteral { entries } => {
            for ((k, _), (v, _)) in entries {
                esc_visit_arg(k, name, in_fork, acc);
                esc_visit_arg(v, name, in_fork, acc);
            }
        }
        Expr::Block(block) => esc_visit_block(block, name, in_fork, acc),
        Expr::If {
            condition,
            then_block,
            else_block,
        } => {
            esc_visit_expr(&condition.0, name, in_fork, acc, false);
            esc_visit_expr(&then_block.0, name, in_fork, acc, is_tail);
            if let Some(eb) = else_block {
                esc_visit_expr(&eb.0, name, in_fork, acc, is_tail);
            }
        }
        Expr::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            esc_visit_expr(&expr.0, name, in_fork, acc, false);
            esc_visit_block(body, name, in_fork, acc);
            if let Some(b) = else_body {
                esc_visit_block(b, name, in_fork, acc);
            }
        }
        Expr::Match { scrutinee, arms } => {
            esc_visit_expr(&scrutinee.0, name, in_fork, acc, false);
            for arm in arms {
                if let Some((g, _)) = &arm.guard {
                    esc_visit_expr(g, name, in_fork, acc, false);
                }
                esc_visit_expr(&arm.body.0, name, in_fork, acc, is_tail);
            }
        }
        // Nested closures: don't descend syntactically. The
        // transitive-escape rule is honored at dispatcher level by
        // observing the inner closure's own classification.
        Expr::Lambda { .. } | Expr::SpawnLambdaActor { .. } => {}
        Expr::Spawn { target, args, .. } => {
            esc_visit_expr(&target.0, name, in_fork, acc, false);
            for (_, (e, _)) in args {
                esc_visit_arg(e, name, in_fork, acc);
            }
        }
        Expr::Scope { body } => esc_visit_block(body, name, /* in_fork = */ false, acc),
        Expr::ForkChild { expr, .. } => {
            esc_visit_expr(&expr.0, name, /* in_fork = */ true, acc, false);
        }
        Expr::ForkBlock { body } => {
            esc_visit_block(body, name, /* in_fork = */ true, acc);
        }
        Expr::ScopeDeadline { duration, body } => {
            esc_visit_expr(&duration.0, name, in_fork, acc, false);
            esc_visit_block(body, name, in_fork, acc);
        }
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let StringPart::Expr((e, _)) | StringPart::StructuralExpr((e, _)) = part {
                    esc_visit_expr(e, name, in_fork, acc, false);
                }
            }
        }
        Expr::StructInit { fields, base, .. } => {
            for (_, (e, _)) in fields {
                esc_visit_arg(e, name, in_fork, acc);
            }
            if let Some(b) = base {
                esc_visit_arg(&b.0, name, in_fork, acc);
            }
        }
        Expr::Select { arms, timeout } => {
            for arm in arms {
                esc_visit_expr(&arm.source.0, name, in_fork, acc, false);
                esc_visit_expr(&arm.body.0, name, in_fork, acc, false);
            }
            if let Some(t) = timeout {
                esc_visit_expr(&t.duration.0, name, in_fork, acc, false);
                esc_visit_expr(&t.body.0, name, in_fork, acc, false);
            }
        }
        Expr::Join(items) | Expr::Race(items) => {
            for (e, _) in items {
                esc_visit_arg(e, name, in_fork || matches!(expr, Expr::Race(_)), acc);
            }
        }
        Expr::Timeout { expr, duration } => {
            esc_visit_expr(&expr.0, name, in_fork, acc, is_tail);
            esc_visit_expr(&duration.0, name, in_fork, acc, false);
        }
        Expr::UnsafeBlock(block) => esc_visit_block(block, name, in_fork, acc),
        Expr::Yield(opt) => {
            if let Some(boxed) = opt {
                esc_visit_arg(&boxed.0, name, in_fork, acc);
            }
        }
        Expr::FieldAccess { object, .. } => esc_visit_expr(&object.0, name, in_fork, acc, false),
        Expr::Index { object, index } => {
            esc_visit_expr(&object.0, name, in_fork, acc, false);
            esc_visit_expr(&index.0, name, in_fork, acc, false);
        }
        Expr::Cast { expr, .. } => esc_visit_expr(&expr.0, name, in_fork, acc, false),
        Expr::PostfixTry(inner) => esc_visit_expr(&inner.0, name, in_fork, acc, is_tail),
        Expr::Range { start, end, .. } => {
            if let Some(s) = start {
                esc_visit_expr(&s.0, name, in_fork, acc, false);
            }
            if let Some(e) = end {
                esc_visit_expr(&e.0, name, in_fork, acc, false);
            }
        }
        Expr::Is { lhs, rhs } => {
            esc_visit_expr(&lhs.0, name, in_fork, acc, false);
            esc_visit_expr(&rhs.0, name, in_fork, acc, false);
        }
        Expr::MachineEmit { fields, .. } => {
            for (_, (e, _)) in fields {
                esc_visit_arg(e, name, in_fork, acc);
            }
        }
        Expr::Await(inner) | Expr::AwaitRestart(inner) => {
            esc_visit_expr(&inner.0, name, in_fork, acc, false);
        }
        Expr::GenBlock { body } => esc_visit_block(body, name, in_fork, acc),
        Expr::Return(opt) => {
            // `return <expr>` carries the operand out of the closure; treat it
            // as an argument-position escape, mirroring `yield <expr>`.
            if let Some(boxed) = opt {
                esc_visit_arg(&boxed.0, name, in_fork, acc);
            }
        }
    }
}

/// Argument-position walker — a bare reference to the closure-bound
/// name as an argument flags `PassedToHigherOrder`.
fn esc_visit_arg(expr: &Expr, name: &str, in_fork: bool, acc: &mut EscapeAccumulator) {
    if let Expr::Identifier(n) = expr {
        if n == name {
            acc.any_use = true;
            if in_fork {
                acc.forked_use = true;
            } else {
                acc.record_nonlocal(ClosureEscapeRule::PassedToHigherOrder);
            }
            return;
        }
    }
    esc_visit_expr(expr, name, in_fork, acc, false);
}
