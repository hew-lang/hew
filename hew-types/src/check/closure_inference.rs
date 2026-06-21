//! Syntactic analyses over a closure body and over the block that
//! introduces a closure binding.
//!
//! Two passes live here:
//!
//! 1. [`scan_lambda_body`] — runs over the lambda body AST to compute
//!    [`LambdaBodyFacts`]: which syntactic names appear in mutating
//!    positions (capture-mode inference picks `BorrowMut` over `Borrow`
//!    for these), and whether the body contains any suspend point form
//!    (gates `NonSyncMutCaptureCrossesSuspend` on this).
//!
//! 2. [`classify_closure_escape_in_block`] — runs over the block that
//!    encloses a lambda's introducing `let f = ...` binding (or any
//!    closure-bound name) to compute the conservative
//!    [`ClosureEscapeKind`]. `Local` requires positive proof from a
//!    direct-call use-site; everything else defaults to `Escapes`.
//!
//! Both passes are purely syntactic; they do not consult the type
//! substitution or the trait registry.

use hew_parser::ast::{Block, CallArg, ElseBlock, Expr, MatchArm, Spanned, Stmt, StringPart};

use super::types::{ClosureEscapeFact, ClosureEscapeKind, ClosureEscapeRule};

/// Facts extracted from one walk of a lambda body.
#[derive(Debug, Default, Clone)]
pub(super) struct LambdaBodyFacts {
    /// Surface names that appear in a mutating position in the body.
    pub mutated_names: std::collections::HashSet<String>,
    /// Whether the body contains any suspend-point form.
    pub has_suspend: bool,
    /// First-seen suspend point kind label (for diagnostic):
    /// `"await"`, `"for await"`, `"channel recv"`, or empty.
    pub suspend_kind: String,
}

/// Method-call receivers reached by these names are treated as mutated.
/// Narrow on purpose — only the runtime's documented mutating-in-place
/// methods are listed. Extending this list is a deliberate decision.
const MUTATING_METHODS: &[&str] = &[
    "push", "pop", "insert", "remove", "clear", "extend", "append", "set", "swap", "sort",
    "reverse", "truncate", "drain", "retain", "send",
];

pub(super) fn scan_lambda_body(body: &Spanned<Expr>) -> LambdaBodyFacts {
    let mut facts = LambdaBodyFacts::default();
    body_visit_expr(&body.0, &mut facts);
    facts
}

fn body_visit_block(block: &Block, out: &mut LambdaBodyFacts) {
    for (stmt, _) in &block.stmts {
        body_visit_stmt(stmt, out);
    }
    if let Some(tail) = &block.trailing_expr {
        body_visit_expr(&tail.0, out);
    }
}

#[allow(
    clippy::match_same_arms,
    reason = "AST visitor: identical arms cluster the relevant shapes and \
              merging across distant variants hurts readability"
)]
fn body_visit_stmt(stmt: &Stmt, out: &mut LambdaBodyFacts) {
    match stmt {
        Stmt::Let { value, .. } | Stmt::Var { value, .. } => {
            if let Some((e, _)) = value {
                body_visit_expr(e, out);
            }
        }
        Stmt::Assign { target, value, .. } => {
            if let Some(name) = root_identifier(&target.0) {
                out.mutated_names.insert(name);
            }
            body_visit_expr(&target.0, out);
            body_visit_expr(&value.0, out);
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            body_visit_expr(&condition.0, out);
            body_visit_block(then_block, out);
            if let Some(eb) = else_block {
                body_visit_else_block(eb, out);
            }
        }
        Stmt::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            body_visit_expr(&expr.0, out);
            body_visit_block(body, out);
            if let Some(b) = else_body {
                body_visit_block(b, out);
            }
        }
        Stmt::Match { scrutinee, arms } => {
            body_visit_expr(&scrutinee.0, out);
            for arm in arms {
                body_visit_arm(arm, out);
            }
        }
        Stmt::Loop { body, .. } => body_visit_block(body, out),
        Stmt::For {
            is_await,
            iterable,
            body,
            ..
        } => {
            if *is_await && !out.has_suspend {
                out.has_suspend = true;
                out.suspend_kind = "for await".to_string();
            }
            body_visit_expr(&iterable.0, out);
            body_visit_block(body, out);
        }
        Stmt::While {
            condition, body, ..
        } => {
            body_visit_expr(&condition.0, out);
            body_visit_block(body, out);
        }
        Stmt::WhileLet { expr, body, .. } => {
            body_visit_expr(&expr.0, out);
            body_visit_block(body, out);
        }
        Stmt::Break { value, .. } => {
            if let Some((e, _)) = value {
                body_visit_expr(e, out);
            }
        }
        Stmt::Continue { .. } => {}
        Stmt::Return(opt) => {
            if let Some((e, _)) = opt {
                body_visit_expr(e, out);
            }
        }
        Stmt::Defer(boxed) => body_visit_expr(&boxed.0, out),
        Stmt::Expression((e, _)) => body_visit_expr(e, out),
    }
}

fn body_visit_else_block(eb: &ElseBlock, out: &mut LambdaBodyFacts) {
    if let Some(b) = &eb.block {
        body_visit_block(b, out);
    }
    if let Some(if_stmt) = &eb.if_stmt {
        body_visit_stmt(&if_stmt.0, out);
    }
}

fn body_visit_arm(arm: &MatchArm, out: &mut LambdaBodyFacts) {
    if let Some((g, _)) = &arm.guard {
        body_visit_expr(g, out);
    }
    body_visit_expr(&arm.body.0, out);
}

fn body_visit_args(args: &[CallArg], out: &mut LambdaBodyFacts) {
    for arg in args {
        let (e, _) = arg.expr();
        body_visit_expr(e, out);
    }
}

#[allow(
    clippy::too_many_lines,
    clippy::match_same_arms,
    reason = "AST visitor over full Expr surface; arm-per-variant is the \
              clearest form even when some arms have identical bodies"
)]
fn body_visit_expr(expr: &Expr, out: &mut LambdaBodyFacts) {
    match expr {
        Expr::Await(inner) => {
            if !out.has_suspend {
                out.has_suspend = true;
                out.suspend_kind = "await".to_string();
            }
            body_visit_expr(&inner.0, out);
        }
        Expr::MethodCall {
            receiver,
            method,
            args,
        } => {
            if MUTATING_METHODS.contains(&method.as_str()) {
                if let Some(name) = root_identifier(&receiver.0) {
                    out.mutated_names.insert(name);
                }
            }
            body_visit_expr(&receiver.0, out);
            body_visit_args(args, out);
        }
        Expr::Binary { left, right, .. } => {
            body_visit_expr(&left.0, out);
            body_visit_expr(&right.0, out);
        }
        Expr::Unary { operand, .. } | Expr::Clone(operand) => body_visit_expr(&operand.0, out),
        Expr::Literal(_)
        | Expr::Identifier(_)
        | Expr::This
        | Expr::RegexLiteral(_)
        | Expr::ByteStringLiteral(_)
        | Expr::ByteArrayLiteral(_) => {}
        Expr::Tuple(items) | Expr::Array(items) => {
            for (e, _) in items {
                body_visit_expr(e, out);
            }
        }
        Expr::ArrayRepeat { value, count } => {
            body_visit_expr(&value.0, out);
            body_visit_expr(&count.0, out);
        }
        Expr::MapLiteral { entries } => {
            for ((k, _), (v, _)) in entries {
                body_visit_expr(k, out);
                body_visit_expr(v, out);
            }
        }
        Expr::Block(block) => body_visit_block(block, out),
        Expr::If {
            condition,
            then_block,
            else_block,
        } => {
            body_visit_expr(&condition.0, out);
            body_visit_expr(&then_block.0, out);
            if let Some(eb) = else_block {
                body_visit_expr(&eb.0, out);
            }
        }
        Expr::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            body_visit_expr(&expr.0, out);
            body_visit_block(body, out);
            if let Some(b) = else_body {
                body_visit_block(b, out);
            }
        }
        Expr::Match { scrutinee, arms } => {
            body_visit_expr(&scrutinee.0, out);
            for arm in arms {
                body_visit_arm(arm, out);
            }
        }
        // Inner lambdas: do NOT descend — the inner lambda owns its
        // own body scan.
        Expr::Lambda { .. } | Expr::SpawnLambdaActor { .. } => {}
        Expr::Spawn { target, args, .. } => {
            body_visit_expr(&target.0, out);
            for (_, (e, _)) in args {
                body_visit_expr(e, out);
            }
        }
        Expr::Scope { body } => body_visit_block(body, out),
        Expr::ForkChild { expr, .. } => body_visit_expr(&expr.0, out),
        Expr::ForkBlock { body } => body_visit_block(body, out),
        Expr::ScopeDeadline { duration, body } => {
            body_visit_expr(&duration.0, out);
            body_visit_block(body, out);
        }
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let StringPart::Expr((e, _)) = part {
                    body_visit_expr(e, out);
                }
            }
        }
        Expr::Call { function, args, .. } => {
            body_visit_expr(&function.0, out);
            body_visit_args(args, out);
        }
        Expr::StructInit { fields, base, .. } => {
            for (_, (e, _)) in fields {
                body_visit_expr(e, out);
            }
            if let Some(b) = base {
                body_visit_expr(&b.0, out);
            }
        }
        Expr::Select { arms, timeout } => {
            for arm in arms {
                // Channel recv (select recv arm) is a suspend point.
                if !out.has_suspend {
                    out.has_suspend = true;
                    out.suspend_kind = "channel recv".to_string();
                }
                body_visit_expr(&arm.source.0, out);
                body_visit_expr(&arm.body.0, out);
            }
            if let Some(t) = timeout {
                body_visit_expr(&t.duration.0, out);
                body_visit_expr(&t.body.0, out);
            }
        }
        Expr::Join(exprs) => {
            for (e, _) in exprs {
                body_visit_expr(e, out);
            }
        }
        Expr::Timeout { expr, duration } => {
            body_visit_expr(&expr.0, out);
            body_visit_expr(&duration.0, out);
        }
        Expr::UnsafeBlock(block) => body_visit_block(block, out),
        Expr::Yield(opt) => {
            // `yield` is a generator suspend point — control leaves the
            // closure body before the resumed continuation observes any
            // captured-binding mutation. Treated as a suspend source
            // for the `NonSyncMutCaptureCrossesSuspend` gate, alongside
            // `await` / `for await` / channel `recv`.
            if !out.has_suspend {
                out.has_suspend = true;
                out.suspend_kind = "yield".to_string();
            }
            if let Some(boxed) = opt {
                body_visit_expr(&boxed.0, out);
            }
        }
        Expr::FieldAccess { object, .. } => body_visit_expr(&object.0, out),
        Expr::Index { object, index } => {
            body_visit_expr(&object.0, out);
            body_visit_expr(&index.0, out);
        }
        Expr::Cast { expr, .. } => body_visit_expr(&expr.0, out),
        Expr::PostfixTry(inner) => body_visit_expr(&inner.0, out),
        Expr::Range { start, end, .. } => {
            if let Some(s) = start {
                body_visit_expr(&s.0, out);
            }
            if let Some(e) = end {
                body_visit_expr(&e.0, out);
            }
        }
        Expr::Is { lhs, rhs } => {
            body_visit_expr(&lhs.0, out);
            body_visit_expr(&rhs.0, out);
        }
        Expr::MachineEmit { fields, .. } => {
            for (_, (e, _)) in fields {
                body_visit_expr(e, out);
            }
        }
        Expr::GenBlock { body } => body_visit_block(body, out),
        Expr::Return(opt) => {
            // `return` is a divergent leaf, not a suspend point; just recurse
            // into the operand so captures referenced by `return <expr>` are
            // observed.
            if let Some(boxed) = opt {
                body_visit_expr(&boxed.0, out);
            }
        }
    }
}

/// Walk an expression's leftmost-anchored path back to its root
/// identifier. Used to convert `x.f`, `x[0]`, `x.method()` to the
/// surface name being mutated.
#[allow(
    clippy::match_same_arms,
    reason = "Each arm names a distinct AST shape; collapsing identical \
              recursion arms obscures the path taken at a use site"
)]
fn root_identifier(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Identifier(name) => Some(name.clone()),
        Expr::FieldAccess { object, .. } => root_identifier(&object.0),
        Expr::Index { object, .. } => root_identifier(&object.0),
        Expr::MethodCall { receiver, .. } => root_identifier(&receiver.0),
        Expr::Cast { expr, .. } => root_identifier(&expr.0),
        Expr::PostfixTry(inner) => root_identifier(&inner.0),
        _ => None,
    }
}

// ── escape classifier ──────────────────────────────────────────────────

#[derive(Default)]
struct EscapeAccumulator {
    /// At least one use seen.
    any_use: bool,
    /// Use is inside a `fork { ... }` body — Forked wins over Escapes.
    forked_use: bool,
    /// Some use is non-direct-call (Escapes unless Forked also fires).
    nonlocal_use: bool,
    /// First non-local rule that fired (for advisory diagnostic).
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
        // `clone <operand>` is read-only and recurses like other unary forms.
        Expr::Unary { operand, .. } | Expr::Clone(operand) => {
            esc_visit_expr(&operand.0, name, in_fork, acc, false);
        }
        Expr::Literal(_)
        | Expr::This
        | Expr::RegexLiteral(_)
        | Expr::ByteStringLiteral(_)
        | Expr::ByteArrayLiteral(_) => {}
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
                if let StringPart::Expr((e, _)) = part {
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
        Expr::Join(items) => {
            for (e, _) in items {
                esc_visit_arg(e, name, in_fork, acc);
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
        Expr::Await(inner) => esc_visit_expr(&inner.0, name, in_fork, acc, false),
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
