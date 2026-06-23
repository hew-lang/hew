//! The `needless_bool` lint.
//!
//! Flags an `if`/`else` whose only purpose is to produce a boolean literal in
//! each branch:
//!
//! - `if c { true } else { false }` → `c`
//! - `if c { false } else { true }` → `!c`
//!
//! ## Precision over recall
//!
//! - Both branches are required; each must be exactly a single boolean literal
//!   (an empty-statement block whose only content is `true` / `false`). Any
//!   extra statement in a branch disqualifies the `if`.
//! - The two branches must be *opposite* literals — `if c { true } else { true }`
//!   is a different smell (a constant) and is left alone.
//! - An `else if` chain never qualifies (its else branch is not a bare boolean
//!   literal).
//!
//! The rewrite yields the same boolean value in any context, so unlike the
//! position-sensitive lints this one fires wherever the shape appears
//! (statement, block tail, `let` initialiser, argument, …).

use hew_parser::ast::{Block, Expr, Literal, Span, Stmt};

use crate::error::TypeError;

use super::{LintCtx, LintId, LintLevels, NodeVisitor};

/// Entry point: walk `body` and flag every literal-returning `if`/`else`.
pub(super) fn check(ctx: &LintCtx, levels: &LintLevels, body: &Block, out: &mut Vec<TypeError>) {
    let mut visitor = NeedlessBool { hits: Vec::new() };
    super::walk_body(body, &mut visitor);
    for (span, positive) in visitor.hits {
        let (message, suggestion) = if positive {
            (
                "this `if`/`else` just yields its condition".to_string(),
                "replace the whole `if` with the condition".to_string(),
            )
        } else {
            (
                "this `if`/`else` just yields the negation of its condition".to_string(),
                "replace the whole `if` with the negated condition (`!…`)".to_string(),
            )
        };
        ctx.emit(
            levels,
            LintId::NeedlessBool,
            &span,
            message,
            suggestion,
            out,
        );
    }
}

struct NeedlessBool {
    /// `(span, positive)` where `positive` is `true` when the then-branch is
    /// `true` (rewrite → `cond`) and `false` when it is `false` (→ `!cond`).
    hits: Vec<(Span, bool)>,
}

impl NeedlessBool {
    fn record(&mut self, span: &Span, then_value: bool, else_value: bool) {
        // Opposite polarities only: equal branches are a constant, not this lint.
        if then_value != else_value {
            self.hits.push((span.clone(), then_value));
        }
    }
}

impl NodeVisitor for NeedlessBool {
    fn visit_stmt(&mut self, stmt: &Stmt, span: &Span) {
        if let Stmt::If {
            then_block,
            else_block: Some(else_block),
            ..
        } = stmt
        {
            // Plain `else { … }` only — an `else if` carries an `if_stmt`.
            if else_block.if_stmt.is_some() {
                return;
            }
            let Some(else_block) = &else_block.block else {
                return;
            };
            if let (Some(then_value), Some(else_value)) =
                (block_bool(then_block), block_bool(else_block))
            {
                self.record(span, then_value, else_value);
            }
        }
    }

    fn visit_expr(&mut self, expr: &Expr, span: &Span) {
        if let Expr::If {
            then_block,
            else_block: Some(else_expr),
            ..
        } = expr
        {
            if let (Some(then_value), Some(else_value)) =
                (branch_bool(&then_block.0), branch_bool(&else_expr.0))
            {
                self.record(span, then_value, else_value);
            }
        }
    }
}

/// The boolean value of a branch expression that is exactly a boolean literal
/// (directly, or as the sole trailing expression of an otherwise-empty block).
fn branch_bool(expr: &Expr) -> Option<bool> {
    match expr {
        Expr::Literal(Literal::Bool(value)) => Some(*value),
        Expr::Block(block) => block_bool(block),
        _ => None,
    }
}

/// The boolean value of a block that contains nothing but a boolean-literal
/// trailing expression (no statements).
fn block_bool(block: &Block) -> Option<bool> {
    if !block.stmts.is_empty() {
        return None;
    }
    branch_bool(&block.trailing_expr.as_ref()?.0)
}
