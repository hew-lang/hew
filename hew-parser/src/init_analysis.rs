//! Syntactic questions about an actor `init` body.

use std::collections::BTreeSet;

use crate::ast::{Block, ElseBlock, Expr, Stmt};

/// Bare names an `init` body assigns with a plain `name = value` or
/// `self.name = value`, in any nested structural block.
///
/// This decides which state fields init owns: a field without a default that
/// appears here is deferred to init (D447). Lambda, generator and fork bodies
/// are not walked: a store from a child body is not the actor's own
/// initialization sequence.
#[must_use]
pub fn assigned_bare_names(body: &Block) -> BTreeSet<String> {
    let mut names = BTreeSet::new();
    block_targets(body, &mut names);
    names
}

fn block_targets(block: &Block, names: &mut BTreeSet<String>) {
    for (stmt, _) in &block.stmts {
        stmt_targets(stmt, names);
    }
    if let Some(trailing) = &block.trailing_expr {
        expr_targets(&trailing.0, names);
    }
}

fn else_targets(else_block: &ElseBlock, names: &mut BTreeSet<String>) {
    if let Some(block) = &else_block.block {
        block_targets(block, names);
    }
    if let Some(if_stmt) = &else_block.if_stmt {
        stmt_targets(&if_stmt.0, names);
    }
}

fn stmt_targets(stmt: &Stmt, names: &mut BTreeSet<String>) {
    match stmt {
        Stmt::Assign {
            target,
            op: None,
            value,
        } => {
            match &target.0 {
                Expr::Identifier(name) => {
                    names.insert(name.clone());
                }
                Expr::FieldAccess { object, field } if matches!(&object.0, Expr::Identifier(receiver) if receiver == "self") =>
                {
                    names.insert(field.clone());
                }
                _ => {}
            }
            expr_targets(&value.0, names);
        }
        Stmt::Assign { value, .. } => expr_targets(&value.0, names),
        Stmt::Let {
            value, else_block, ..
        } => {
            if let Some(value) = value {
                expr_targets(&value.0, names);
            }
            if let Some(else_block) = else_block {
                block_targets(else_block, names);
            }
        }
        Stmt::Var { value, .. } | Stmt::Break { value, .. } | Stmt::Return(value) => {
            if let Some(value) = value {
                expr_targets(&value.0, names);
            }
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            expr_targets(&condition.0, names);
            block_targets(then_block, names);
            if let Some(else_block) = else_block {
                else_targets(else_block, names);
            }
        }
        Stmt::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            expr_targets(&expr.0, names);
            block_targets(body, names);
            if let Some(else_body) = else_body {
                expr_targets(&else_body.0, names);
            }
        }
        Stmt::Match { scrutinee, arms } => {
            expr_targets(&scrutinee.0, names);
            for arm in arms {
                expr_targets(&arm.body.0, names);
            }
        }
        Stmt::Loop { body, .. } => block_targets(body, names),
        Stmt::For { iterable, body, .. } => {
            expr_targets(&iterable.0, names);
            block_targets(body, names);
        }
        Stmt::While {
            condition, body, ..
        } => {
            expr_targets(&condition.0, names);
            block_targets(body, names);
        }
        Stmt::WhileLet { expr, body, .. } => {
            expr_targets(&expr.0, names);
            block_targets(body, names);
        }
        Stmt::Defer(expr) => expr_targets(&expr.0, names),
        Stmt::Expression(expr) => expr_targets(&expr.0, names),
        Stmt::Continue { .. } => {}
    }
}

fn expr_targets(expr: &Expr, names: &mut BTreeSet<String>) {
    match expr {
        Expr::Block(block)
        | Expr::Scope { body: block }
        | Expr::ScopeDeadline { body: block, .. } => {
            block_targets(block, names);
        }
        Expr::UnsafeBlock(block) => block_targets(block, names),
        Expr::If {
            then_block,
            else_block,
            ..
        } => {
            expr_targets(&then_block.0, names);
            if let Some(else_block) = else_block {
                expr_targets(&else_block.0, names);
            }
        }
        Expr::IfLet {
            body, else_body, ..
        } => {
            block_targets(body, names);
            if let Some(else_body) = else_body {
                expr_targets(&else_body.0, names);
            }
        }
        Expr::Match { arms, .. } => {
            for arm in arms {
                expr_targets(&arm.body.0, names);
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::assigned_bare_names;

    fn init_body(source: &str) -> crate::ast::Block {
        let result = crate::parse(source);
        assert!(result.errors.is_empty(), "{:?}", result.errors);
        for item in result.program.items {
            if let crate::ast::Item::Actor(actor) = item.0 {
                return actor.init.expect("init").body;
            }
        }
        panic!("no actor")
    }

    #[test]
    fn collects_bare_and_self_targets_through_nested_blocks() {
        let body = init_body(
            "actor A { var a: i64, var b: i64, var c: i64, var d: i64, init(x: i64) { \
             a = x; if x > 0 { self.b = 1; } else { match x { _ => { c = 2; } } } \
             let f = || { d = 3; }; } }",
        );
        let names = assigned_bare_names(&body);
        assert_eq!(
            names.into_iter().collect::<Vec<_>>(),
            vec!["a".to_string(), "b".to_string(), "c".to_string()]
        );
    }
}
