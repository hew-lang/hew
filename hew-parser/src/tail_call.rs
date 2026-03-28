//! Tail call detection pass.
//!
//! Walks the AST and marks `Expr::Call` nodes as tail calls when they appear
//! directly in tail position or as the direct value of a `return`.

use crate::ast::{Block, Expr, Item, Program, Stmt, StringPart};

/// Marks tail calls in a parsed program.
///
/// A call is considered a tail call when it is the direct return value in a
/// `return call(...)` statement or the direct trailing expression in a tail
/// position.
pub fn mark_tail_calls(program: &mut Program) {
    for (item, _span) in &mut program.items {
        mark_item(item);
    }
}

fn mark_item(item: &mut Item) {
    match item {
        Item::Function(decl) => mark_block(&mut decl.body, true),
        Item::Actor(decl) => {
            for handler in &mut decl.receive_fns {
                mark_block(&mut handler.body, true);
            }
            for method in &mut decl.methods {
                mark_block(&mut method.body, true);
            }
            if let Some(init) = &mut decl.init {
                mark_block(&mut init.body, true);
            }
        }
        Item::Impl(decl) => {
            for method in &mut decl.methods {
                mark_block(&mut method.body, true);
            }
        }
        _ => {}
    }
}

fn mark_block(block: &mut Block, is_tail_position: bool) {
    if block_contains_defer(block) {
        return;
    }

    for (stmt, _span) in &mut block.stmts {
        mark_stmt(stmt);
    }

    if let Some(expr) = &mut block.trailing_expr {
        mark_expr(&mut expr.0, is_tail_position);
    }
}

/// Returns `true` if any statement or tail expression in the block (or nested
/// blocks) is a `Defer`.
fn block_contains_defer(block: &Block) -> bool {
    block
        .stmts
        .iter()
        .any(|(stmt, _)| stmt_contains_defer(stmt))
        || block
            .trailing_expr
            .as_ref()
            .is_some_and(|expr| expr_contains_defer(&expr.0))
}

fn stmt_contains_defer(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Let { value, .. } => value
            .as_ref()
            .is_some_and(|(expr, _)| expr_contains_defer(expr)),
        Stmt::Var { value, .. } => value
            .as_ref()
            .is_some_and(|(expr, _)| expr_contains_defer(expr)),
        Stmt::Assign { target, value, .. } => {
            expr_contains_defer(&target.0) || expr_contains_defer(&value.0)
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            expr_contains_defer(&condition.0)
                || block_contains_defer(then_block)
                || else_block.as_ref().is_some_and(|eb| {
                    eb.if_stmt
                        .as_ref()
                        .is_some_and(|boxed| stmt_contains_defer(&boxed.0))
                        || eb.block.as_ref().is_some_and(block_contains_defer)
                })
        }
        Stmt::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            expr_contains_defer(&expr.0)
                || block_contains_defer(body)
                || else_body.as_ref().is_some_and(block_contains_defer)
        }
        Stmt::Match { scrutinee, arms } => {
            expr_contains_defer(&scrutinee.0)
                || arms.iter().any(|arm| {
                    arm.guard
                        .as_ref()
                        .is_some_and(|(guard, _)| expr_contains_defer(guard))
                        || expr_contains_defer(&arm.body.0)
                })
        }
        Stmt::Loop { body, .. } => block_contains_defer(body),
        Stmt::For { iterable, body, .. } => {
            expr_contains_defer(&iterable.0) || block_contains_defer(body)
        }
        Stmt::While {
            condition, body, ..
        } => expr_contains_defer(&condition.0) || block_contains_defer(body),
        Stmt::WhileLet { expr, body, .. } => {
            expr_contains_defer(&expr.0) || block_contains_defer(body)
        }
        Stmt::Break { value, .. } => value
            .as_ref()
            .is_some_and(|(expr, _)| expr_contains_defer(expr)),
        Stmt::Continue { .. } => false,
        Stmt::Return(value) => value
            .as_ref()
            .is_some_and(|(expr, _)| expr_contains_defer(expr)),
        Stmt::Defer(_) => true,
        Stmt::Expression((expr, _)) => expr_contains_defer(expr),
    }
}

#[expect(
    clippy::too_many_lines,
    reason = "exhaustive defer scan over all expression variants"
)]
fn expr_contains_defer(expr: &Expr) -> bool {
    match expr {
        Expr::Binary { left, right, .. } => {
            expr_contains_defer(&left.0) || expr_contains_defer(&right.0)
        }
        Expr::Unary { operand, .. } | Expr::Await(operand) | Expr::PostfixTry(operand) => {
            expr_contains_defer(&operand.0)
        }
        Expr::Literal(_)
        | Expr::Identifier(_)
        | Expr::This
        | Expr::Cooperate
        | Expr::RegexLiteral(_)
        | Expr::ByteStringLiteral(_)
        | Expr::ByteArrayLiteral(_)
        | Expr::Lambda { .. }
        | Expr::SpawnLambdaActor { .. }
        | Expr::Yield(None)
        | Expr::ScopeCancel
        | Expr::ScopeLaunch(_)
        | Expr::ScopeSpawn(_) => false,
        Expr::Tuple(items) | Expr::Array(items) | Expr::Join(items) => {
            items.iter().any(|(expr, _)| expr_contains_defer(expr))
        }
        Expr::ArrayRepeat { value, count } => {
            expr_contains_defer(&value.0) || expr_contains_defer(&count.0)
        }
        Expr::MapLiteral { entries } => entries
            .iter()
            .any(|(key, value)| expr_contains_defer(&key.0) || expr_contains_defer(&value.0)),
        Expr::Block(block) | Expr::Unsafe(block) | Expr::Scope { body: block, .. } => {
            block_contains_defer(block)
        }
        Expr::If {
            condition,
            then_block,
            else_block,
        } => {
            expr_contains_defer(&condition.0)
                || expr_contains_defer(&then_block.0)
                || else_block
                    .as_ref()
                    .is_some_and(|else_expr| expr_contains_defer(&else_expr.0))
        }
        Expr::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            expr_contains_defer(&expr.0)
                || block_contains_defer(body)
                || else_body.as_ref().is_some_and(block_contains_defer)
        }
        Expr::Match { scrutinee, arms } => {
            expr_contains_defer(&scrutinee.0)
                || arms.iter().any(|arm| {
                    arm.guard
                        .as_ref()
                        .is_some_and(|(guard, _)| expr_contains_defer(guard))
                        || expr_contains_defer(&arm.body.0)
                })
        }
        Expr::Spawn { target, args } => {
            expr_contains_defer(&target.0)
                || args.iter().any(|(_, expr)| expr_contains_defer(&expr.0))
        }
        Expr::InterpolatedString(parts) => parts.iter().any(|part| match part {
            StringPart::Literal(_) => false,
            StringPart::Expr((expr, _)) => expr_contains_defer(expr),
        }),
        Expr::Call { function, args, .. } => {
            expr_contains_defer(&function.0)
                || args.iter().any(|arg| expr_contains_defer(&arg.expr().0))
        }
        Expr::MethodCall { receiver, args, .. } => {
            expr_contains_defer(&receiver.0)
                || args.iter().any(|arg| expr_contains_defer(&arg.expr().0))
        }
        Expr::StructInit { fields, .. } => {
            fields.iter().any(|(_, expr)| expr_contains_defer(&expr.0))
        }
        Expr::Send { target, message } => {
            expr_contains_defer(&target.0) || expr_contains_defer(&message.0)
        }
        Expr::Select { arms, timeout } => {
            arms.iter()
                .any(|arm| expr_contains_defer(&arm.source.0) || expr_contains_defer(&arm.body.0))
                || timeout.as_ref().is_some_and(|timeout| {
                    expr_contains_defer(&timeout.duration.0) || expr_contains_defer(&timeout.body.0)
                })
        }
        Expr::Timeout { expr, duration } => {
            expr_contains_defer(&expr.0) || expr_contains_defer(&duration.0)
        }
        Expr::Yield(Some(expr)) | Expr::Cast { expr, .. } => expr_contains_defer(&expr.0),
        Expr::FieldAccess { object, .. } => expr_contains_defer(&object.0),
        Expr::Index { object, index } => {
            expr_contains_defer(&object.0) || expr_contains_defer(&index.0)
        }
        Expr::Range { start, end, .. } => {
            start
                .as_ref()
                .is_some_and(|expr| expr_contains_defer(&expr.0))
                || end
                    .as_ref()
                    .is_some_and(|expr| expr_contains_defer(&expr.0))
        }
    }
}

fn mark_stmt(stmt: &mut Stmt) {
    match stmt {
        Stmt::Let { value, .. } | Stmt::Var { value, .. } | Stmt::Break { value, .. } => {
            if let Some((expr, _)) = value {
                mark_expr(expr, false);
            }
        }
        Stmt::Assign { target, value, .. } => {
            mark_expr(&mut target.0, false);
            mark_expr(&mut value.0, false);
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            mark_expr(&mut condition.0, false);
            mark_block(then_block, false);
            if let Some(eb) = else_block {
                if let Some(if_stmt) = &mut eb.if_stmt {
                    mark_stmt(&mut if_stmt.0);
                }
                if let Some(block) = &mut eb.block {
                    mark_block(block, false);
                }
            }
        }
        Stmt::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            mark_expr(&mut expr.0, false);
            mark_block(body, false);
            if let Some(block) = else_body {
                mark_block(block, false);
            }
        }
        Stmt::Match { scrutinee, arms } => {
            mark_expr(&mut scrutinee.0, false);
            for arm in arms {
                if let Some((guard, _)) = &mut arm.guard {
                    mark_expr(guard, false);
                }
                mark_expr(&mut arm.body.0, false);
            }
        }
        Stmt::Loop { body, .. } => mark_block(body, false),
        Stmt::For { iterable, body, .. } => {
            mark_expr(&mut iterable.0, false);
            mark_block(body, false);
        }
        Stmt::While {
            condition, body, ..
        } => {
            mark_expr(&mut condition.0, false);
            mark_block(body, false);
        }
        Stmt::WhileLet { expr, body, .. } => {
            mark_expr(&mut expr.0, false);
            mark_block(body, false);
        }
        Stmt::Continue { .. } | Stmt::Return(None) => {}
        Stmt::Return(Some((expr, _span))) => mark_expr(expr, true),
        Stmt::Defer(expr) => mark_expr(&mut expr.0, false),
        Stmt::Expression((expr, _)) => mark_expr(expr, false),
    }
}

#[expect(
    clippy::too_many_lines,
    reason = "exhaustive tail-position scan over all expression variants"
)]
fn mark_expr(expr: &mut Expr, is_tail_position: bool) {
    match expr {
        Expr::Binary { left, right, .. } => {
            mark_expr(&mut left.0, false);
            mark_expr(&mut right.0, false);
        }
        Expr::Unary { operand, .. } | Expr::Await(operand) | Expr::PostfixTry(operand) => {
            mark_expr(&mut operand.0, false);
        }
        Expr::Literal(_)
        | Expr::Identifier(_)
        | Expr::This
        | Expr::Cooperate
        | Expr::RegexLiteral(_)
        | Expr::ByteStringLiteral(_)
        | Expr::ByteArrayLiteral(_)
        | Expr::Lambda { .. }
        | Expr::SpawnLambdaActor { .. }
        | Expr::Yield(None)
        | Expr::ScopeCancel
        | Expr::ScopeLaunch(_)
        | Expr::ScopeSpawn(_) => {}
        Expr::Tuple(items) | Expr::Array(items) | Expr::Join(items) => {
            for (expr, _) in items {
                mark_expr(expr, false);
            }
        }
        Expr::ArrayRepeat { value, count } => {
            mark_expr(&mut value.0, false);
            mark_expr(&mut count.0, false);
        }
        Expr::MapLiteral { entries } => {
            for (key, value) in entries {
                mark_expr(&mut key.0, false);
                mark_expr(&mut value.0, false);
            }
        }
        Expr::Block(block) | Expr::Unsafe(block) | Expr::Scope { body: block, .. } => {
            mark_block(block, is_tail_position);
        }
        Expr::If {
            condition,
            then_block,
            else_block,
        } => {
            mark_expr(&mut condition.0, false);
            mark_expr(&mut then_block.0, is_tail_position);
            if let Some(else_expr) = else_block {
                mark_expr(&mut else_expr.0, is_tail_position);
            }
        }
        Expr::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            mark_expr(&mut expr.0, false);
            mark_block(body, is_tail_position);
            if let Some(block) = else_body {
                mark_block(block, is_tail_position);
            }
        }
        Expr::Match { scrutinee, arms } => {
            mark_expr(&mut scrutinee.0, false);
            for arm in arms {
                if let Some((guard, _)) = &mut arm.guard {
                    mark_expr(guard, false);
                }
                mark_expr(&mut arm.body.0, is_tail_position);
            }
        }
        Expr::Spawn { target, args } => {
            mark_expr(&mut target.0, false);
            for (_, expr) in args {
                mark_expr(&mut expr.0, false);
            }
        }
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let StringPart::Expr((expr, _)) = part {
                    mark_expr(expr, false);
                }
            }
        }
        Expr::Call {
            function,
            args,
            is_tail_call,
            ..
        } => {
            mark_expr(&mut function.0, false);
            for arg in args {
                mark_expr(&mut arg.expr_mut().0, false);
            }
            if is_tail_position {
                *is_tail_call = true;
            }
        }
        Expr::MethodCall { receiver, args, .. } => {
            mark_expr(&mut receiver.0, false);
            for arg in args {
                mark_expr(&mut arg.expr_mut().0, false);
            }
        }
        Expr::StructInit { fields, .. } => {
            for (_, expr) in fields {
                mark_expr(&mut expr.0, false);
            }
        }
        Expr::Send { target, message } => {
            mark_expr(&mut target.0, false);
            mark_expr(&mut message.0, false);
        }
        Expr::Select { arms, timeout } => {
            for arm in arms {
                mark_expr(&mut arm.source.0, false);
                mark_expr(&mut arm.body.0, is_tail_position);
            }
            if let Some(timeout) = timeout {
                mark_expr(&mut timeout.duration.0, false);
                mark_expr(&mut timeout.body.0, is_tail_position);
            }
        }
        Expr::Timeout { expr, duration } => {
            mark_expr(&mut expr.0, false);
            mark_expr(&mut duration.0, false);
        }
        Expr::Yield(Some(expr)) | Expr::Cast { expr, .. } => mark_expr(&mut expr.0, false),
        Expr::FieldAccess { object, .. } => mark_expr(&mut object.0, false),
        Expr::Index { object, index } => {
            mark_expr(&mut object.0, false);
            mark_expr(&mut index.0, false);
        }
        Expr::Range { start, end, .. } => {
            if let Some(expr) = start {
                mark_expr(&mut expr.0, false);
            }
            if let Some(expr) = end {
                mark_expr(&mut expr.0, false);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::mark_tail_calls;
    use crate::{
        ast::{Expr, Item, Stmt},
        parse,
    };

    fn parse_and_mark(source: &str) -> Item {
        let mut result = parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        mark_tail_calls(&mut result.program);
        result
            .program
            .items
            .into_iter()
            .next()
            .expect("function item")
            .0
    }

    fn first_function(source: &str) -> crate::ast::FnDecl {
        match parse_and_mark(source) {
            Item::Function(function) => function,
            item => panic!("expected function item, got {item:?}"),
        }
    }

    #[test]
    fn trailing_match_expression_marks_arm_calls_as_tail() {
        let function = first_function(
            "fn example(cond: bool) -> int { return match cond { true => expensive_call(), false => other_call() }; }",
        );

        let Stmt::Return(Some((Expr::Match { arms, .. }, _))) = &function.body.stmts[0].0 else {
            panic!("expected return match expression");
        };

        for arm in arms {
            let Expr::Call { is_tail_call, .. } = &arm.body.0 else {
                panic!("expected call in match arm");
            };
            assert!(*is_tail_call, "expected match arm call to be marked tail");
        }
    }

    #[test]
    fn match_statement_arm_blocks_mark_nested_return_calls_as_tail() {
        let function = first_function(
            "fn example(cond: bool) -> int { match cond { true => { return expensive_call(); }, false => { return other_call(); } } }",
        );

        let Stmt::Match { arms, .. } = &function.body.stmts[0].0 else {
            panic!("expected match statement");
        };

        for arm in arms {
            let Expr::Block(block) = &arm.body.0 else {
                panic!("expected block arm body");
            };
            let Stmt::Return(Some((Expr::Call { is_tail_call, .. }, _))) = &block.stmts[0].0 else {
                panic!("expected return call in arm block");
            };
            assert!(
                *is_tail_call,
                "expected nested return call to be marked tail"
            );
        }
    }

    #[test]
    fn return_match_block_arms_mark_trailing_calls_as_tail() {
        let function = first_function(
            "fn example(cond: bool) -> int { return match cond { true => { expensive_call() }, false => { other_call() } }; }",
        );

        let Stmt::Return(Some((Expr::Match { arms, .. }, _))) = &function.body.stmts[0].0 else {
            panic!("expected return match expression");
        };

        for arm in arms {
            let Expr::Block(block) = &arm.body.0 else {
                panic!("expected block arm body");
            };
            let Expr::Call { is_tail_call, .. } =
                &block.trailing_expr.as_ref().expect("block trailing expr").0
            else {
                panic!("expected trailing call in arm block");
            };
            assert!(
                *is_tail_call,
                "expected block trailing call to be marked tail"
            );
        }
    }

    #[test]
    fn non_tail_match_expression_in_binding_leaves_arm_calls_unmarked() {
        let function = first_function(
            "fn example(cond: bool) -> int { let value = match cond { true => expensive_call(), false => other_call() }; value }",
        );

        let Stmt::Let {
            value: Some((Expr::Match { arms, .. }, _)),
            ..
        } = &function.body.stmts[0].0
        else {
            panic!("expected let-bound match expression");
        };

        for arm in arms {
            let Expr::Call { is_tail_call, .. } = &arm.body.0 else {
                panic!("expected call in match arm");
            };
            assert!(
                !*is_tail_call,
                "expected non-tail match arm call to remain unmarked"
            );
        }
    }

    #[test]
    fn non_tail_match_block_arms_leave_trailing_calls_unmarked() {
        let function = first_function(
            "fn example(cond: bool) -> int { let value = match cond { true => { expensive_call() }, false => { other_call() } }; value }",
        );

        let Stmt::Let {
            value: Some((Expr::Match { arms, .. }, _)),
            ..
        } = &function.body.stmts[0].0
        else {
            panic!("expected let-bound match expression");
        };

        for arm in arms {
            let Expr::Block(block) = &arm.body.0 else {
                panic!("expected block arm body");
            };
            let Expr::Call { is_tail_call, .. } =
                &block.trailing_expr.as_ref().expect("block trailing expr").0
            else {
                panic!("expected trailing call in arm block");
            };
            assert!(
                !*is_tail_call,
                "expected non-tail block trailing call to remain unmarked"
            );
        }
    }
}
