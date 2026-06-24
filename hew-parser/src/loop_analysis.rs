/// AST-level loop analysis utilities.
///
/// Provides break-detection over the raw AST so both the type checker
/// (`hew-types`) and HIR lowering (`hew-hir`) can share the logic without
/// duplicating it.
use crate::ast::{Block, Expr, Stmt};

/// Returns `true` if the `body` of a `loop { … }` contains a `break`
/// statement that would exit THAT loop — i.e., a break that targets the
/// loop identified by `self_label`.
///
/// This is the public entry point; it calls the internal depth-aware helpers.
///
/// # Semantics
/// * An unlabeled `break` at depth 0 (direct body of the loop) exits this loop.
/// * A labeled `break @L` at any depth exits the loop whose label is `L`.
/// * Inner loops (`loop`, `for`, `while`, `while let`) increase the depth so
///   an unlabeled break inside them does NOT count as exiting the outer loop.
/// * Lambda bodies (`|…| …`, `spawn_lambda_actor`) are new scope boundaries;
///   `break` inside them cannot escape to an outer loop.
#[must_use]
pub fn loop_body_has_break(body: &Block, self_label: Option<&str>) -> bool {
    ast_block_has_break(body, self_label, 0)
}

fn ast_block_has_break(block: &Block, self_label: Option<&str>, depth: usize) -> bool {
    for (stmt, _) in &block.stmts {
        if ast_stmt_has_break(stmt, self_label, depth) {
            return true;
        }
    }
    if let Some(trailing) = &block.trailing_expr {
        return ast_expr_has_break(&trailing.0, self_label, depth);
    }
    false
}

fn ast_stmt_has_break(stmt: &Stmt, self_label: Option<&str>, depth: usize) -> bool {
    match stmt {
        Stmt::Break { label, .. } => {
            if depth == 0 {
                // An unlabeled break, or a break with our own label, exits this loop.
                label.is_none() || label.as_deref() == self_label
            } else {
                // Inside an inner loop, only a break that explicitly names OUR
                // label escapes to the outer loop.
                label.as_deref() == self_label && self_label.is_some()
            }
        }
        Stmt::Loop { body, .. } | Stmt::For { body, .. } | Stmt::While { body, .. } => {
            // Inner loop: increase depth so unlabeled inner breaks don't count.
            ast_block_has_break(body, self_label, depth + 1)
        }
        Stmt::WhileLet { body, .. } => ast_block_has_break(body, self_label, depth + 1),
        Stmt::If {
            then_block,
            else_block,
            condition,
        } => {
            let _ = condition; // condition cannot contain a break
            if ast_block_has_break(then_block, self_label, depth) {
                return true;
            }
            if let Some(eb) = else_block {
                if let Some(block) = &eb.block {
                    if ast_block_has_break(block, self_label, depth) {
                        return true;
                    }
                }
                if let Some(if_stmt) = &eb.if_stmt {
                    if ast_stmt_has_break(&if_stmt.0, self_label, depth) {
                        return true;
                    }
                }
            }
            false
        }
        Stmt::IfLet {
            body, else_body, ..
        } => {
            if ast_block_has_break(body, self_label, depth) {
                return true;
            }
            if let Some(eb) = else_body {
                if ast_block_has_break(eb, self_label, depth) {
                    return true;
                }
            }
            false
        }
        Stmt::Match { arms, scrutinee: _ } => arms
            .iter()
            .any(|arm| ast_expr_has_break(&arm.body.0, self_label, depth)),
        Stmt::Let {
            value, else_block, ..
        } => {
            if let Some(val) = value {
                if ast_expr_has_break(&val.0, self_label, depth) {
                    return true;
                }
            }
            if let Some(eb) = else_block {
                if ast_block_has_break(eb, self_label, depth) {
                    return true;
                }
            }
            false
        }
        Stmt::Var {
            value: Some(val), ..
        } => ast_expr_has_break(&val.0, self_label, depth),
        Stmt::Assign { value, .. } => ast_expr_has_break(&value.0, self_label, depth),
        Stmt::Defer(expr) => ast_expr_has_break(&expr.0, self_label, depth),
        Stmt::Expression(expr) => ast_expr_has_break(&expr.0, self_label, depth),
        _ => false,
    }
}

fn ast_expr_has_break(expr: &Expr, self_label: Option<&str>, depth: usize) -> bool {
    match expr {
        Expr::Block(block) => ast_block_has_break(block, self_label, depth),
        Expr::If {
            then_block,
            else_block,
            ..
        } => {
            if ast_expr_has_break(&then_block.0, self_label, depth) {
                return true;
            }
            if let Some(eb) = else_block {
                return ast_expr_has_break(&eb.0, self_label, depth);
            }
            false
        }
        Expr::Match { arms, .. } => arms
            .iter()
            .any(|arm| ast_expr_has_break(&arm.body.0, self_label, depth)),
        Expr::Binary { left, right, .. } => {
            ast_expr_has_break(&left.0, self_label, depth)
                || ast_expr_has_break(&right.0, self_label, depth)
        }
        Expr::Unary { operand, .. } | Expr::Clone(operand) => {
            ast_expr_has_break(&operand.0, self_label, depth)
        }
        // All other expression forms (literals, identifiers, calls, lambdas,
        // spawn-lambdas, etc.) do not directly contain break statements, or
        // (for lambdas and spawn-lambdas) introduce independent scopes where a
        // `break` cannot escape to the outer loop.
        _ => false,
    }
}
