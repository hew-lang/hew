//! The `redundant_else_after_return` lint.
//!
//! Flags an `if` whose then-branch unconditionally diverges (every control-flow
//! path ends in `return` / `break` / `continue`, or a `!`-typed call such as
//! `panic` / `exit`) yet still carries an `else`. When the then-branch can never
//! fall through, the `else` adds nothing: its body can be dropped one level of
//! indentation and run unconditionally after the `if`.
//!
//! ## Reusing the checker's divergence facts
//!
//! Rather than re-deriving which calls diverge, the lint reuses the type the
//! checker already recorded for each expression: a diverging expression
//! (expression-position `return`, a `panic` / `exit` call, a block/`if`/`match`
//! all of whose arms diverge) is typed [`Ty::Never`]. Terminator *statements*
//! (`return` / `break` / `continue`) are not expressions and carry no recorded
//! type, so those are matched structurally. A block diverges exactly when its
//! tail position — the trailing expression, else the final statement —
//! diverges, with nested `if` / `match` requiring *every* branch to diverge.
//!
//! ## Precision over recall
//!
//! - Only fires when the then-branch diverges on *all* paths (a bare
//!   `if c { return }` with no else, or a nested `if` without a sibling
//!   diverging branch, never qualifies).
//! - Skips `if let` / `while let` (the binding scope complicates the rewrite).
//! - Skips an `else if` chain: flattening `} else if … {` into a bare statement
//!   is not the clean de-indent this lint promises.
//! - Considers the `if` only in statement position (`Stmt::If`) or block-tail
//!   position (a trailing `Expr::If`), where dropping the `else` preserves
//!   meaning. A value-position `if` feeding a `let` / argument is left alone.
//! - Skips when the `else` body introduces a named `let` or `var` binding:
//!   de-indenting would move it to the enclosing scope, where a later
//!   same-name `let` becomes a hard same-scope rebinding error.  Wildcard
//!   patterns (`let _ = …`) are not considered named bindings and do not
//!   suppress the lint.

use hew_parser::ast::{Block, Expr, Pattern, PatternField, Span, Stmt};

use crate::error::TypeError;
use crate::ty::Ty;

use super::{LintCtx, LintId, LintLevels, NodeVisitor};

/// Entry point: walk `body` and flag every redundant-else `if`.
pub(super) fn check(ctx: &LintCtx, levels: &LintLevels, body: &Block, out: &mut Vec<TypeError>) {
    let mut visitor = RedundantElse {
        ctx,
        hits: Vec::new(),
    };
    super::walk_body(body, &mut visitor);
    for span in visitor.hits {
        ctx.emit(
            levels,
            LintId::RedundantElseAfterReturn,
            &span,
            "this `else` is redundant — the `if` branch always diverges".to_string(),
            "drop the `else` and de-indent its body to run after the `if`".to_string(),
            out,
        );
    }
}

struct RedundantElse<'a> {
    ctx: &'a LintCtx<'a>,
    hits: Vec<Span>,
}

impl NodeVisitor for RedundantElse<'_> {
    fn visit_block(&mut self, block: &Block) {
        // Statement-position ifs: the value is discarded, so dropping the else
        // and running its body unconditionally is sound when the then branch
        // diverges.
        for (stmt, span) in &block.stmts {
            if let Stmt::If {
                then_block,
                else_block: Some(else_block),
                ..
            } = stmt
            {
                // `else if` is not a clean de-indent; require a plain `else`.
                if else_block.if_stmt.is_some() || else_block.block.is_none() {
                    continue;
                }
                // Suppress when the else body introduces a named binding.
                // De-indenting would move that binding to the enclosing scope;
                // any later same-name `let` there becomes a same-scope
                // rebinding error.  Wildcard `_` is exempt.
                if else_block
                    .block
                    .as_ref()
                    .is_some_and(block_has_any_named_binding)
                {
                    continue;
                }
                if block_always_diverges(self.ctx, then_block) {
                    self.hits.push(span.clone());
                }
            }
        }

        // Block-tail if: the block's value is this if's value, but because the
        // then branch diverges that value only ever comes from the else, so the
        // de-indent (`else` body becomes the new block tail) preserves meaning.
        if let Some(trailing) = &block.trailing_expr {
            if let Expr::If {
                then_block,
                else_block: Some(else_expr),
                ..
            } = &trailing.0
            {
                // An `else if` chain surfaces as the else expression being
                // itself an `if` / `if let`; skip it.
                if matches!(else_expr.0, Expr::If { .. } | Expr::IfLet { .. }) {
                    return;
                }
                // Suppress when the else body (an Expr::Block) introduces a
                // named binding for the same scope-safety reason as above.
                if expr_block_has_any_named_binding(&else_expr.0) {
                    return;
                }
                if expr_always_diverges(self.ctx, &then_block.0, &then_block.1) {
                    self.hits.push(trailing.1.clone());
                }
            }
        }
    }
}

/// Does executing `block` always leave via divergence (never fall off the end)?
///
/// Only the tail position decides this: a block falls through exactly when its
/// trailing expression (or, lacking one, its final statement) falls through.
fn block_always_diverges(ctx: &LintCtx, block: &Block) -> bool {
    if let Some(trailing) = &block.trailing_expr {
        return expr_always_diverges(ctx, &trailing.0, &trailing.1);
    }
    match block.stmts.last() {
        Some((stmt, span)) => stmt_always_diverges(ctx, stmt, span),
        None => false,
    }
}

fn stmt_always_diverges(ctx: &LintCtx, stmt: &Stmt, _span: &Span) -> bool {
    match stmt {
        Stmt::Return(_) | Stmt::Break { .. } | Stmt::Continue { .. } => true,
        Stmt::Expression((expr, span)) => expr_always_diverges(ctx, expr, span),
        Stmt::If {
            then_block,
            else_block: Some(else_block),
            ..
        } => {
            let else_div = if let Some(if_stmt) = &else_block.if_stmt {
                stmt_always_diverges(ctx, &if_stmt.0, &if_stmt.1)
            } else if let Some(block) = &else_block.block {
                block_always_diverges(ctx, block)
            } else {
                false
            };
            block_always_diverges(ctx, then_block) && else_div
        }
        Stmt::IfLet {
            body,
            else_body: Some(else_body),
            ..
        } => block_always_diverges(ctx, body) && block_always_diverges(ctx, else_body),
        Stmt::Match { arms, .. } => {
            !arms.is_empty()
                && arms
                    .iter()
                    .all(|arm| expr_always_diverges(ctx, &arm.body.0, &arm.body.1))
        }
        _ => false,
    }
}

/// Whether the expression at `span` diverges on every path.
///
/// Reuses the checker's recorded [`Ty::Never`] (the result of its
/// reachability/never-type analysis) for the call cases, descends structurally
/// into a block / expression-position `return` so a then-branch that ends in a
/// terminator *statement* (which carries no `Never` type of its own) is still
/// recognised.
fn expr_always_diverges(ctx: &LintCtx, expr: &Expr, span: &Span) -> bool {
    match expr {
        Expr::Return(_) => true,
        Expr::Block(block) => block_always_diverges(ctx, block),
        _ => ctx
            .resolved_type_at(span)
            .is_some_and(|ty| matches!(ty, Ty::Never)),
    }
}

// ── Binding-scope guards ──────────────────────────────────────────────────────

/// `true` when `block` contains a top-level `let` with a named pattern or a
/// `var` declaration.
///
/// De-indenting a block with such statements moves the binding(s) into the
/// enclosing scope, where a later same-name `let` becomes a hard same-scope
/// rebinding error.  `let _ = …` (wildcard pattern) is exempted: `_` never
/// introduces a named binding and cannot conflict.
fn block_has_any_named_binding(block: &Block) -> bool {
    block.stmts.iter().any(|(stmt, _)| match stmt {
        Stmt::Let { pattern, .. } => pattern_introduces_binding(&pattern.0),
        Stmt::Var { .. } => true,
        _ => false,
    })
}

/// `true` when `expr` is an [`Expr::Block`] that contains a named binding.
fn expr_block_has_any_named_binding(expr: &Expr) -> bool {
    if let Expr::Block(block) = expr {
        block_has_any_named_binding(block)
    } else {
        false
    }
}

/// `true` when `pat` introduces at least one named (non-wildcard) variable
/// binding that would occupy a slot in the enclosing scope after de-indent.
fn pattern_introduces_binding(pat: &Pattern) -> bool {
    match pat {
        // Non-binding leaf patterns.
        Pattern::Wildcard | Pattern::Literal(_) => false,
        // A single named binding.
        Pattern::Identifier(_) => true,
        // Composite patterns: any sub-pattern may bind.
        Pattern::Constructor { patterns, .. } | Pattern::Tuple(patterns) => {
            patterns.iter().any(|(p, _)| pattern_introduces_binding(p))
        }
        Pattern::Struct { fields, .. } | Pattern::RecordShorthand { fields, .. } => {
            fields.iter().any(struct_field_introduces_binding)
        }
        Pattern::Or(a, b) => pattern_introduces_binding(&a.0) || pattern_introduces_binding(&b.0),
        // Regex captures are named bindings.
        Pattern::Regex { captures, .. } => !captures.is_empty(),
    }
}

/// `true` when a struct/record pattern field introduces a named binding.
///
/// A shorthand field (`{ x }` with no explicit sub-pattern) always binds the
/// field name; an explicit sub-pattern (`{ x: pat }`) delegates recursively.
fn struct_field_introduces_binding(field: &PatternField) -> bool {
    match &field.pattern {
        None => true, // shorthand: `{ name }` binds `name`
        Some((sub, _)) => pattern_introduces_binding(sub),
    }
}
