//! Tail call detection pass.
//!
//! Walks the AST and marks `Expr::Call` nodes as tail calls when they appear
//! directly inside a `Stmt::Return`.

use crate::ast::{Block, Expr, Item, Program, Stmt};

/// Marks tail calls in a parsed program.
///
/// A call is considered a tail call when it is the direct return value in a
/// `return call(...)` statement.
pub fn mark_tail_calls(program: &mut Program) {
    for (item, _span) in &mut program.items {
        mark_item(item);
    }
}

fn mark_item(item: &mut Item) {
    match item {
        Item::Function(decl) => mark_block(&mut decl.body),
        Item::Actor(decl) => {
            for handler in &mut decl.receive_fns {
                mark_block(&mut handler.body);
            }
            for method in &mut decl.methods {
                mark_block(&mut method.body);
            }
            if let Some(init) = &mut decl.init {
                mark_block(&mut init.body);
            }
        }
        Item::Impl(decl) => {
            for method in &mut decl.methods {
                mark_block(&mut method.body);
            }
        }
        _ => {}
    }
}

fn mark_block(block: &mut Block) {
    if block_contains_defer(block) {
        return;
    }
    for (stmt, _span) in &mut block.stmts {
        mark_stmt(stmt);
    }
}

/// Returns `true` if any statement in the block (or nested blocks) is a `Defer`.
fn block_contains_defer(block: &Block) -> bool {
    block
        .stmts
        .iter()
        .any(|(stmt, _)| stmt_contains_defer(stmt))
}

fn stmt_contains_defer(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Defer(_) => true,
        Stmt::If {
            then_block,
            else_block,
            ..
        } => {
            block_contains_defer(then_block)
                || else_block.as_ref().is_some_and(|eb| {
                    eb.if_stmt
                        .as_ref()
                        .is_some_and(|boxed| stmt_contains_defer(&boxed.0))
                        || eb.block.as_ref().is_some_and(block_contains_defer)
                })
        }
        Stmt::Loop { body, .. } | Stmt::For { body, .. } | Stmt::While { body, .. } => {
            block_contains_defer(body)
        }
        _ => false,
    }
}

fn mark_stmt(stmt: &mut Stmt) {
    match stmt {
        Stmt::Return(Some((expr, _span))) => mark_tail_expr(expr),
        Stmt::If {
            then_block,
            else_block,
            ..
        } => {
            mark_block(then_block);
            if let Some(eb) = else_block {
                if let Some(if_stmt) = &mut eb.if_stmt {
                    mark_stmt(&mut if_stmt.0);
                }
                if let Some(block) = &mut eb.block {
                    mark_block(block);
                }
            }
        }
        Stmt::Loop { body, .. } | Stmt::For { body, .. } | Stmt::While { body, .. } => {
            mark_block(body);
        }
        _ => {}
    }
}

fn mark_tail_expr(expr: &mut Expr) {
    if let Expr::Call { is_tail_call, .. } = expr {
        *is_tail_call = true;
    }
}
