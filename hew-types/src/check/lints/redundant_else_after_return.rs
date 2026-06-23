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

use hew_parser::ast::{Block, Expr, Span, Stmt};

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
