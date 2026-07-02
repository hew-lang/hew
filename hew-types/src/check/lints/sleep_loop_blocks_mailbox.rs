//! The `sleep_loop_blocks_mailbox` lint.
//!
//! Flags actor receive handlers that loop around `sleep`/`sleep_until` without
//! an in-loop exit path. Actor message handlers run to completion, so another
//! message to the same actor cannot update the loop guard until the current
//! handler returns.
//!
//! ## Precision over recall
//!
//! The lint only considers loops whose continuation condition is obviously
//! mailbox-controlled: `loop`, `while true`, `while flag`, or `while !flag`.
//! Compound conditions and derived progress checks are not candidates. A
//! reachable `break` or an assignment to the bare guard name inside the loop is
//! treated as an in-handler exit path and suppresses the diagnostic.

use hew_parser::{
    ast::{
        Block, CallArg, ElseBlock, Expr, Literal, MatchArm, SelectArm, Span, Stmt, StringPart,
        UnaryOp,
    },
    loop_body_has_break,
};

use crate::error::TypeError;

use super::{LintCtx, LintId, LintLevels};

/// Entry point: discover candidate sleep loops inside one actor receive body.
pub(super) fn check(ctx: &LintCtx, levels: &LintLevels, body: &Block, out: &mut Vec<TypeError>) {
    find_in_block(ctx, levels, body, out);
}

fn find_in_block(ctx: &LintCtx, levels: &LintLevels, block: &Block, out: &mut Vec<TypeError>) {
    for (stmt, span) in &block.stmts {
        find_in_stmt(ctx, levels, stmt, span, out);
    }
    if let Some(trailing) = &block.trailing_expr {
        find_in_expr(ctx, levels, &trailing.0, out);
    }
}

#[allow(
    clippy::too_many_lines,
    reason = "statement visitor enumerates every block-bearing stmt shape"
)]
fn find_in_stmt(
    ctx: &LintCtx,
    levels: &LintLevels,
    stmt: &Stmt,
    span: &Span,
    out: &mut Vec<TypeError>,
) {
    match stmt {
        Stmt::Loop { label, body } => {
            try_flag(
                ctx,
                levels,
                &Candidate::Unconditional,
                label.as_ref(),
                body,
                span,
                out,
            );
            find_in_block(ctx, levels, body, out);
        }
        Stmt::While {
            label,
            condition,
            body,
        } => {
            if let Some(candidate) = candidate_from_condition(&condition.0) {
                try_flag(ctx, levels, &candidate, label.as_ref(), body, span, out);
            }
            find_in_expr(ctx, levels, &condition.0, out);
            find_in_block(ctx, levels, body, out);
        }
        Stmt::For { iterable, body, .. } => {
            find_in_expr(ctx, levels, &iterable.0, out);
            find_in_block(ctx, levels, body, out);
        }
        Stmt::WhileLet { expr, body, .. } => {
            find_in_expr(ctx, levels, &expr.0, out);
            find_in_block(ctx, levels, body, out);
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            find_in_expr(ctx, levels, &condition.0, out);
            find_in_block(ctx, levels, then_block, out);
            if let Some(else_block) = else_block {
                find_in_else(ctx, levels, else_block, out);
            }
        }
        Stmt::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            find_in_expr(ctx, levels, &expr.0, out);
            find_in_block(ctx, levels, body, out);
            if let Some(else_body) = else_body {
                find_in_block(ctx, levels, else_body, out);
            }
        }
        Stmt::Match { scrutinee, arms } => {
            find_in_expr(ctx, levels, &scrutinee.0, out);
            for arm in arms {
                find_in_arm(ctx, levels, arm, out);
            }
        }
        Stmt::Let {
            value, else_block, ..
        } => {
            if let Some(value) = value {
                find_in_expr(ctx, levels, &value.0, out);
            }
            if let Some(else_block) = else_block {
                find_in_block(ctx, levels, else_block, out);
            }
        }
        Stmt::Var { value, .. } | Stmt::Break { value, .. } | Stmt::Return(value) => {
            if let Some(value) = value {
                find_in_expr(ctx, levels, &value.0, out);
            }
        }
        Stmt::Assign { target, value, .. } => {
            find_in_expr(ctx, levels, &target.0, out);
            find_in_expr(ctx, levels, &value.0, out);
        }
        Stmt::Defer(expr) => find_in_expr(ctx, levels, &expr.0, out),
        Stmt::Expression(expr) => find_in_expr(ctx, levels, &expr.0, out),
        Stmt::Continue { .. } => {}
    }
}

fn find_in_else(
    ctx: &LintCtx,
    levels: &LintLevels,
    else_block: &ElseBlock,
    out: &mut Vec<TypeError>,
) {
    if let Some(if_stmt) = &else_block.if_stmt {
        find_in_stmt(ctx, levels, &if_stmt.0, &if_stmt.1, out);
    }
    if let Some(block) = &else_block.block {
        find_in_block(ctx, levels, block, out);
    }
}

fn find_in_arm(ctx: &LintCtx, levels: &LintLevels, arm: &MatchArm, out: &mut Vec<TypeError>) {
    if let Some(guard) = &arm.guard {
        find_in_expr(ctx, levels, &guard.0, out);
    }
    find_in_expr(ctx, levels, &arm.body.0, out);
}

#[allow(
    clippy::too_many_lines,
    reason = "expression visitor enumerates every block-bearing expr shape"
)]
fn find_in_expr(ctx: &LintCtx, levels: &LintLevels, expr: &Expr, out: &mut Vec<TypeError>) {
    match expr {
        Expr::Block(block) | Expr::Scope { body: block } | Expr::ForkBlock { body: block } => {
            find_in_block(ctx, levels, block, out);
        }
        Expr::UnsafeBlock(block) => find_in_block(ctx, levels, block, out),
        Expr::GenBlock { body } => find_in_block(ctx, levels, body, out),
        Expr::If {
            condition,
            then_block,
            else_block,
        } => {
            find_in_expr(ctx, levels, &condition.0, out);
            find_in_expr(ctx, levels, &then_block.0, out);
            if let Some(else_block) = else_block {
                find_in_expr(ctx, levels, &else_block.0, out);
            }
        }
        Expr::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            find_in_expr(ctx, levels, &expr.0, out);
            find_in_block(ctx, levels, body, out);
            if let Some(else_body) = else_body {
                find_in_block(ctx, levels, else_body, out);
            }
        }
        Expr::Match { scrutinee, arms } => {
            find_in_expr(ctx, levels, &scrutinee.0, out);
            for arm in arms {
                find_in_arm(ctx, levels, arm, out);
            }
        }
        Expr::Lambda { body, .. } | Expr::SpawnLambdaActor { body, .. } => {
            find_in_expr(ctx, levels, &body.0, out);
        }
        Expr::Spawn { target, args, .. } => {
            find_in_expr(ctx, levels, &target.0, out);
            for (_, value) in args {
                find_in_expr(ctx, levels, &value.0, out);
            }
        }
        Expr::ScopeDeadline { duration, body } => {
            find_in_expr(ctx, levels, &duration.0, out);
            find_in_block(ctx, levels, body, out);
        }
        Expr::Timeout { expr, duration } => {
            find_in_expr(ctx, levels, &expr.0, out);
            find_in_expr(ctx, levels, &duration.0, out);
        }
        Expr::Call { function, args, .. } => {
            find_in_expr(ctx, levels, &function.0, out);
            for arg in args {
                find_in_expr(ctx, levels, &arg.expr().0, out);
            }
        }
        Expr::MethodCall { receiver, args, .. } => {
            find_in_expr(ctx, levels, &receiver.0, out);
            for arg in args {
                find_in_expr(ctx, levels, &arg.expr().0, out);
            }
        }
        Expr::StructInit { fields, base, .. } => {
            for (_, value) in fields {
                find_in_expr(ctx, levels, &value.0, out);
            }
            if let Some(base) = base {
                find_in_expr(ctx, levels, &base.0, out);
            }
        }
        Expr::Select { arms, timeout } => {
            for arm in arms {
                find_in_expr(ctx, levels, &arm.source.0, out);
                find_in_expr(ctx, levels, &arm.body.0, out);
            }
            if let Some(timeout) = timeout {
                find_in_expr(ctx, levels, &timeout.duration.0, out);
                find_in_expr(ctx, levels, &timeout.body.0, out);
            }
        }
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let StringPart::Expr(expr) = part {
                    find_in_expr(ctx, levels, &expr.0, out);
                }
            }
        }
        Expr::Tuple(items) | Expr::Array(items) | Expr::Join(items) => {
            for item in items {
                find_in_expr(ctx, levels, &item.0, out);
            }
        }
        Expr::ArrayRepeat { value, count } => {
            find_in_expr(ctx, levels, &value.0, out);
            find_in_expr(ctx, levels, &count.0, out);
        }
        Expr::MapLiteral { entries } => {
            for (key, value) in entries {
                find_in_expr(ctx, levels, &key.0, out);
                find_in_expr(ctx, levels, &value.0, out);
            }
        }
        Expr::Binary { left, right, .. } => {
            find_in_expr(ctx, levels, &left.0, out);
            find_in_expr(ctx, levels, &right.0, out);
        }
        Expr::Unary { operand, .. }
        | Expr::Clone(operand)
        | Expr::Await(operand)
        | Expr::AwaitRestart(operand)
        | Expr::PostfixTry(operand)
        | Expr::ForkChild { expr: operand, .. } => {
            find_in_expr(ctx, levels, &operand.0, out);
        }
        Expr::Cast { expr, .. } | Expr::FieldAccess { object: expr, .. } => {
            find_in_expr(ctx, levels, &expr.0, out);
        }
        Expr::Index { object, index } => {
            find_in_expr(ctx, levels, &object.0, out);
            find_in_expr(ctx, levels, &index.0, out);
        }
        Expr::Range { start, end, .. } => {
            if let Some(start) = start {
                find_in_expr(ctx, levels, &start.0, out);
            }
            if let Some(end) = end {
                find_in_expr(ctx, levels, &end.0, out);
            }
        }
        Expr::Is { lhs, rhs } => {
            find_in_expr(ctx, levels, &lhs.0, out);
            find_in_expr(ctx, levels, &rhs.0, out);
        }
        Expr::MachineEmit { fields, .. } => {
            for (_, value) in fields {
                find_in_expr(ctx, levels, &value.0, out);
            }
        }
        Expr::Yield(value) | Expr::Return(value) => {
            if let Some(value) = value {
                find_in_expr(ctx, levels, &value.0, out);
            }
        }
        Expr::Literal(_)
        | Expr::Identifier(_)
        | Expr::This
        | Expr::RegexLiteral(_)
        | Expr::ByteStringLiteral(_)
        | Expr::ByteArrayLiteral(_) => {}
    }
}

#[derive(Debug, Clone)]
enum Candidate {
    Unconditional,
    Guard(String),
}

fn candidate_from_condition(condition: &Expr) -> Option<Candidate> {
    match condition {
        Expr::Literal(Literal::Bool(true)) => Some(Candidate::Unconditional),
        Expr::Identifier(name) => Some(Candidate::Guard(name.clone())),
        Expr::Unary {
            op: UnaryOp::Not,
            operand,
        } => match &operand.0 {
            Expr::Identifier(name) => Some(Candidate::Guard(name.clone())),
            Expr::Binary { .. }
            | Expr::Unary { .. }
            | Expr::Clone(_)
            | Expr::Literal(_)
            | Expr::Tuple(_)
            | Expr::Array(_)
            | Expr::ArrayRepeat { .. }
            | Expr::MapLiteral { .. }
            | Expr::Block(_)
            | Expr::If { .. }
            | Expr::IfLet { .. }
            | Expr::Match { .. }
            | Expr::Lambda { .. }
            | Expr::Spawn { .. }
            | Expr::SpawnLambdaActor { .. }
            | Expr::Scope { .. }
            | Expr::ForkChild { .. }
            | Expr::ForkBlock { .. }
            | Expr::ScopeDeadline { .. }
            | Expr::InterpolatedString(_)
            | Expr::Call { .. }
            | Expr::MethodCall { .. }
            | Expr::StructInit { .. }
            | Expr::Select { .. }
            | Expr::Join(_)
            | Expr::Timeout { .. }
            | Expr::UnsafeBlock(_)
            | Expr::Yield(_)
            | Expr::Return(_)
            | Expr::This
            | Expr::FieldAccess { .. }
            | Expr::Index { .. }
            | Expr::Cast { .. }
            | Expr::PostfixTry(_)
            | Expr::Range { .. }
            | Expr::Await(_)
            | Expr::AwaitRestart(_)
            | Expr::RegexLiteral(_)
            | Expr::ByteStringLiteral(_)
            | Expr::ByteArrayLiteral(_)
            | Expr::Is { .. }
            | Expr::MachineEmit { .. }
            | Expr::GenBlock { .. } => None,
        },
        Expr::Binary { .. }
        | Expr::Unary { .. }
        | Expr::Clone(_)
        | Expr::Literal(_)
        | Expr::Tuple(_)
        | Expr::Array(_)
        | Expr::ArrayRepeat { .. }
        | Expr::MapLiteral { .. }
        | Expr::Block(_)
        | Expr::If { .. }
        | Expr::IfLet { .. }
        | Expr::Match { .. }
        | Expr::Lambda { .. }
        | Expr::Spawn { .. }
        | Expr::SpawnLambdaActor { .. }
        | Expr::Scope { .. }
        | Expr::ForkChild { .. }
        | Expr::ForkBlock { .. }
        | Expr::ScopeDeadline { .. }
        | Expr::InterpolatedString(_)
        | Expr::Call { .. }
        | Expr::MethodCall { .. }
        | Expr::StructInit { .. }
        | Expr::Select { .. }
        | Expr::Join(_)
        | Expr::Timeout { .. }
        | Expr::UnsafeBlock(_)
        | Expr::Yield(_)
        | Expr::Return(_)
        | Expr::This
        | Expr::FieldAccess { .. }
        | Expr::Index { .. }
        | Expr::Cast { .. }
        | Expr::PostfixTry(_)
        | Expr::Range { .. }
        | Expr::Await(_)
        | Expr::AwaitRestart(_)
        | Expr::RegexLiteral(_)
        | Expr::ByteStringLiteral(_)
        | Expr::ByteArrayLiteral(_)
        | Expr::Is { .. }
        | Expr::MachineEmit { .. }
        | Expr::GenBlock { .. } => None,
    }
}

fn try_flag(
    ctx: &LintCtx,
    levels: &LintLevels,
    candidate: &Candidate,
    label: Option<&String>,
    body: &Block,
    span: &Span,
    out: &mut Vec<TypeError>,
) {
    if !bounded_contains_sleep(body) || loop_body_has_break(body, label.map(String::as_str)) {
        return;
    }
    if let Candidate::Guard(name) = candidate {
        if assigns_identifier(body, name) {
            return;
        }
    }

    ctx.emit(
        levels,
        LintId::SleepLoopBlocksMailbox,
        span,
        "this loop suspends on `sleep`/`sleep_until` but the actor's mailbox is never observed until it exits — a sibling message (e.g. a stop handler) cannot be dispatched until this loop returns".to_string(),
        "use a `#[every(duration)]` periodic receive handler with a boolean flag checked each tick instead — each firing is a separate dispatched message, so a stop-style handler can interleave between ticks".to_string(),
        out,
    );
}

fn bounded_contains_sleep(block: &Block) -> bool {
    block
        .stmts
        .iter()
        .any(|(stmt, _)| bounded_stmt_has_sleep(stmt))
        || block
            .trailing_expr
            .as_ref()
            .is_some_and(|expr| bounded_expr_has_sleep(&expr.0))
}

#[allow(
    clippy::too_many_lines,
    reason = "statement visitor enumerates every Stmt shape to preserve boundary decisions"
)]
fn bounded_stmt_has_sleep(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Loop { .. }
        | Stmt::For { .. }
        | Stmt::While { .. }
        | Stmt::WhileLet { .. }
        | Stmt::Continue { .. } => false,
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            bounded_expr_has_sleep(&condition.0)
                || bounded_contains_sleep(then_block)
                || else_block.as_ref().is_some_and(bounded_else_has_sleep)
        }
        Stmt::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            bounded_expr_has_sleep(&expr.0)
                || bounded_contains_sleep(body)
                || else_body.as_ref().is_some_and(bounded_contains_sleep)
        }
        Stmt::Match { scrutinee, arms } => {
            bounded_expr_has_sleep(&scrutinee.0) || arms.iter().any(bounded_arm_has_sleep)
        }
        Stmt::Let {
            value, else_block, ..
        } => {
            value
                .as_ref()
                .is_some_and(|value| bounded_expr_has_sleep(&value.0))
                || else_block.as_ref().is_some_and(bounded_contains_sleep)
        }
        Stmt::Var { value, .. } | Stmt::Break { value, .. } | Stmt::Return(value) => value
            .as_ref()
            .is_some_and(|value| bounded_expr_has_sleep(&value.0)),
        Stmt::Assign { target, value, .. } => {
            bounded_expr_has_sleep(&target.0) || bounded_expr_has_sleep(&value.0)
        }
        Stmt::Defer(expr) => bounded_expr_has_sleep(&expr.0),
        Stmt::Expression(expr) => bounded_expr_has_sleep(&expr.0),
    }
}

fn bounded_else_has_sleep(else_block: &ElseBlock) -> bool {
    else_block
        .if_stmt
        .as_ref()
        .is_some_and(|stmt| bounded_stmt_has_sleep(&stmt.0))
        || else_block
            .block
            .as_ref()
            .is_some_and(bounded_contains_sleep)
}

fn bounded_arm_has_sleep(arm: &MatchArm) -> bool {
    arm.guard
        .as_ref()
        .is_some_and(|guard| bounded_expr_has_sleep(&guard.0))
        || bounded_expr_has_sleep(&arm.body.0)
}

fn bounded_call_args_have_sleep(args: &[CallArg]) -> bool {
    args.iter().any(|arg| bounded_expr_has_sleep(&arg.expr().0))
}

#[allow(
    clippy::too_many_lines,
    reason = "expression visitor enumerates every Expr shape to preserve boundary decisions"
)]
fn bounded_expr_has_sleep(expr: &Expr) -> bool {
    match expr {
        Expr::Call { function, args, .. } => {
            matches!(&function.0, Expr::Identifier(name) if name == "sleep" || name == "sleep_until")
                || bounded_expr_has_sleep(&function.0)
                || bounded_call_args_have_sleep(args)
        }
        Expr::Lambda { .. }
        | Expr::SpawnLambdaActor { .. }
        | Expr::GenBlock { .. }
        | Expr::Literal(_)
        | Expr::Identifier(_)
        | Expr::This
        | Expr::RegexLiteral(_)
        | Expr::ByteStringLiteral(_)
        | Expr::ByteArrayLiteral(_) => false,
        Expr::Block(block) | Expr::Scope { body: block } | Expr::ForkBlock { body: block } => {
            bounded_contains_sleep(block)
        }
        Expr::UnsafeBlock(block) => bounded_contains_sleep(block),
        Expr::If {
            condition,
            then_block,
            else_block,
        } => {
            bounded_expr_has_sleep(&condition.0)
                || bounded_expr_has_sleep(&then_block.0)
                || else_block
                    .as_ref()
                    .is_some_and(|else_block| bounded_expr_has_sleep(&else_block.0))
        }
        Expr::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            bounded_expr_has_sleep(&expr.0)
                || bounded_contains_sleep(body)
                || else_body.as_ref().is_some_and(bounded_contains_sleep)
        }
        Expr::Match { scrutinee, arms } => {
            bounded_expr_has_sleep(&scrutinee.0) || arms.iter().any(bounded_arm_has_sleep)
        }
        Expr::Spawn { target, args, .. } => {
            bounded_expr_has_sleep(&target.0)
                || args
                    .iter()
                    .any(|(_, value)| bounded_expr_has_sleep(&value.0))
        }
        Expr::ScopeDeadline { duration, body } => {
            bounded_expr_has_sleep(&duration.0) || bounded_contains_sleep(body)
        }
        Expr::Timeout { expr, duration } => {
            bounded_expr_has_sleep(&expr.0) || bounded_expr_has_sleep(&duration.0)
        }
        Expr::MethodCall { receiver, args, .. } => {
            bounded_expr_has_sleep(&receiver.0) || bounded_call_args_have_sleep(args)
        }
        Expr::StructInit { fields, base, .. } => {
            fields
                .iter()
                .any(|(_, value)| bounded_expr_has_sleep(&value.0))
                || base
                    .as_ref()
                    .is_some_and(|base| bounded_expr_has_sleep(&base.0))
        }
        Expr::Select { arms, timeout } => {
            arms.iter().any(bounded_select_arm_has_sleep)
                || timeout.as_ref().is_some_and(|timeout| {
                    bounded_expr_has_sleep(&timeout.duration.0)
                        || bounded_expr_has_sleep(&timeout.body.0)
                })
        }
        Expr::InterpolatedString(parts) => parts.iter().any(|part| match part {
            StringPart::Literal(_) => false,
            StringPart::Expr(expr) => bounded_expr_has_sleep(&expr.0),
        }),
        Expr::Tuple(items) | Expr::Array(items) | Expr::Join(items) => {
            items.iter().any(|item| bounded_expr_has_sleep(&item.0))
        }
        Expr::ArrayRepeat { value, count } => {
            bounded_expr_has_sleep(&value.0) || bounded_expr_has_sleep(&count.0)
        }
        Expr::MapLiteral { entries } => entries
            .iter()
            .any(|(key, value)| bounded_expr_has_sleep(&key.0) || bounded_expr_has_sleep(&value.0)),
        Expr::Binary { left, right, .. } => {
            bounded_expr_has_sleep(&left.0) || bounded_expr_has_sleep(&right.0)
        }
        Expr::Unary { operand, .. }
        | Expr::Clone(operand)
        | Expr::Await(operand)
        | Expr::AwaitRestart(operand)
        | Expr::PostfixTry(operand)
        | Expr::ForkChild { expr: operand, .. } => bounded_expr_has_sleep(&operand.0),
        Expr::Cast { expr, .. } | Expr::FieldAccess { object: expr, .. } => {
            bounded_expr_has_sleep(&expr.0)
        }
        Expr::Index { object, index } => {
            bounded_expr_has_sleep(&object.0) || bounded_expr_has_sleep(&index.0)
        }
        Expr::Range { start, end, .. } => {
            start
                .as_ref()
                .is_some_and(|start| bounded_expr_has_sleep(&start.0))
                || end
                    .as_ref()
                    .is_some_and(|end| bounded_expr_has_sleep(&end.0))
        }
        Expr::Is { lhs, rhs } => bounded_expr_has_sleep(&lhs.0) || bounded_expr_has_sleep(&rhs.0),
        Expr::MachineEmit { fields, .. } => fields
            .iter()
            .any(|(_, value)| bounded_expr_has_sleep(&value.0)),
        Expr::Yield(value) | Expr::Return(value) => value
            .as_ref()
            .is_some_and(|value| bounded_expr_has_sleep(&value.0)),
    }
}

fn bounded_select_arm_has_sleep(arm: &SelectArm) -> bool {
    bounded_expr_has_sleep(&arm.source.0) || bounded_expr_has_sleep(&arm.body.0)
}

fn assigns_identifier(block: &Block, name: &str) -> bool {
    block
        .stmts
        .iter()
        .any(|(stmt, _)| stmt_assigns_identifier(stmt, name))
        || block
            .trailing_expr
            .as_ref()
            .is_some_and(|expr| expr_assigns_identifier(&expr.0, name))
}

#[allow(
    clippy::too_many_lines,
    reason = "statement visitor enumerates every Stmt shape so assignment sites are not missed"
)]
fn stmt_assigns_identifier(stmt: &Stmt, name: &str) -> bool {
    match stmt {
        Stmt::Assign { target, value, .. } => {
            matches!(&target.0, Expr::Identifier(target_name) if target_name == name)
                || expr_assigns_identifier(&target.0, name)
                || expr_assigns_identifier(&value.0, name)
        }
        Stmt::Loop { body, .. } => assigns_identifier(body, name),
        Stmt::For { iterable, body, .. } => {
            expr_assigns_identifier(&iterable.0, name) || assigns_identifier(body, name)
        }
        Stmt::While {
            condition, body, ..
        } => expr_assigns_identifier(&condition.0, name) || assigns_identifier(body, name),
        Stmt::WhileLet { expr, body, .. } => {
            expr_assigns_identifier(&expr.0, name) || assigns_identifier(body, name)
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            expr_assigns_identifier(&condition.0, name)
                || assigns_identifier(then_block, name)
                || else_block
                    .as_ref()
                    .is_some_and(|else_block| else_assigns_identifier(else_block, name))
        }
        Stmt::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            expr_assigns_identifier(&expr.0, name)
                || assigns_identifier(body, name)
                || else_body
                    .as_ref()
                    .is_some_and(|else_body| assigns_identifier(else_body, name))
        }
        Stmt::Match { scrutinee, arms } => {
            expr_assigns_identifier(&scrutinee.0, name)
                || arms.iter().any(|arm| arm_assigns_identifier(arm, name))
        }
        Stmt::Let {
            value, else_block, ..
        } => {
            value
                .as_ref()
                .is_some_and(|value| expr_assigns_identifier(&value.0, name))
                || else_block
                    .as_ref()
                    .is_some_and(|else_block| assigns_identifier(else_block, name))
        }
        Stmt::Var { value, .. } | Stmt::Break { value, .. } | Stmt::Return(value) => value
            .as_ref()
            .is_some_and(|value| expr_assigns_identifier(&value.0, name)),
        Stmt::Defer(expr) => expr_assigns_identifier(&expr.0, name),
        Stmt::Expression(expr) => expr_assigns_identifier(&expr.0, name),
        Stmt::Continue { .. } => false,
    }
}

fn else_assigns_identifier(else_block: &ElseBlock, name: &str) -> bool {
    else_block
        .if_stmt
        .as_ref()
        .is_some_and(|stmt| stmt_assigns_identifier(&stmt.0, name))
        || else_block
            .block
            .as_ref()
            .is_some_and(|block| assigns_identifier(block, name))
}

fn arm_assigns_identifier(arm: &MatchArm, name: &str) -> bool {
    arm.guard
        .as_ref()
        .is_some_and(|guard| expr_assigns_identifier(&guard.0, name))
        || expr_assigns_identifier(&arm.body.0, name)
}

fn call_args_assign_identifier(args: &[CallArg], name: &str) -> bool {
    args.iter()
        .any(|arg| expr_assigns_identifier(&arg.expr().0, name))
}

#[allow(
    clippy::too_many_lines,
    reason = "expression visitor enumerates every Expr shape so assignment sites are not missed"
)]
fn expr_assigns_identifier(expr: &Expr, name: &str) -> bool {
    match expr {
        Expr::Block(block)
        | Expr::Scope { body: block }
        | Expr::ForkBlock { body: block }
        | Expr::GenBlock { body: block } => assigns_identifier(block, name),
        Expr::UnsafeBlock(block) => assigns_identifier(block, name),
        Expr::If {
            condition,
            then_block,
            else_block,
        } => {
            expr_assigns_identifier(&condition.0, name)
                || expr_assigns_identifier(&then_block.0, name)
                || else_block
                    .as_ref()
                    .is_some_and(|else_block| expr_assigns_identifier(&else_block.0, name))
        }
        Expr::IfLet {
            expr,
            body,
            else_body,
            ..
        } => {
            expr_assigns_identifier(&expr.0, name)
                || assigns_identifier(body, name)
                || else_body
                    .as_ref()
                    .is_some_and(|else_body| assigns_identifier(else_body, name))
        }
        Expr::Match { scrutinee, arms } => {
            expr_assigns_identifier(&scrutinee.0, name)
                || arms.iter().any(|arm| arm_assigns_identifier(arm, name))
        }
        Expr::Lambda { body, .. } | Expr::SpawnLambdaActor { body, .. } => {
            expr_assigns_identifier(&body.0, name)
        }
        Expr::Spawn { target, args, .. } => {
            expr_assigns_identifier(&target.0, name)
                || args
                    .iter()
                    .any(|(_, value)| expr_assigns_identifier(&value.0, name))
        }
        Expr::ScopeDeadline { duration, body } => {
            expr_assigns_identifier(&duration.0, name) || assigns_identifier(body, name)
        }
        Expr::Timeout { expr, duration } => {
            expr_assigns_identifier(&expr.0, name) || expr_assigns_identifier(&duration.0, name)
        }
        Expr::Call { function, args, .. } => {
            expr_assigns_identifier(&function.0, name) || call_args_assign_identifier(args, name)
        }
        Expr::MethodCall { receiver, args, .. } => {
            expr_assigns_identifier(&receiver.0, name) || call_args_assign_identifier(args, name)
        }
        Expr::StructInit { fields, base, .. } => {
            fields
                .iter()
                .any(|(_, value)| expr_assigns_identifier(&value.0, name))
                || base
                    .as_ref()
                    .is_some_and(|base| expr_assigns_identifier(&base.0, name))
        }
        Expr::Select { arms, timeout } => {
            arms.iter().any(|arm| {
                expr_assigns_identifier(&arm.source.0, name)
                    || expr_assigns_identifier(&arm.body.0, name)
            }) || timeout.as_ref().is_some_and(|timeout| {
                expr_assigns_identifier(&timeout.duration.0, name)
                    || expr_assigns_identifier(&timeout.body.0, name)
            })
        }
        Expr::InterpolatedString(parts) => parts.iter().any(|part| match part {
            StringPart::Literal(_) => false,
            StringPart::Expr(expr) => expr_assigns_identifier(&expr.0, name),
        }),
        Expr::Tuple(items) | Expr::Array(items) | Expr::Join(items) => items
            .iter()
            .any(|item| expr_assigns_identifier(&item.0, name)),
        Expr::ArrayRepeat { value, count } => {
            expr_assigns_identifier(&value.0, name) || expr_assigns_identifier(&count.0, name)
        }
        Expr::MapLiteral { entries } => entries.iter().any(|(key, value)| {
            expr_assigns_identifier(&key.0, name) || expr_assigns_identifier(&value.0, name)
        }),
        Expr::Binary { left, right, .. } => {
            expr_assigns_identifier(&left.0, name) || expr_assigns_identifier(&right.0, name)
        }
        Expr::Unary { operand, .. }
        | Expr::Clone(operand)
        | Expr::Await(operand)
        | Expr::AwaitRestart(operand)
        | Expr::PostfixTry(operand)
        | Expr::ForkChild { expr: operand, .. } => expr_assigns_identifier(&operand.0, name),
        Expr::Cast { expr, .. } | Expr::FieldAccess { object: expr, .. } => {
            expr_assigns_identifier(&expr.0, name)
        }
        Expr::Index { object, index } => {
            expr_assigns_identifier(&object.0, name) || expr_assigns_identifier(&index.0, name)
        }
        Expr::Range { start, end, .. } => {
            start
                .as_ref()
                .is_some_and(|start| expr_assigns_identifier(&start.0, name))
                || end
                    .as_ref()
                    .is_some_and(|end| expr_assigns_identifier(&end.0, name))
        }
        Expr::Is { lhs, rhs } => {
            expr_assigns_identifier(&lhs.0, name) || expr_assigns_identifier(&rhs.0, name)
        }
        Expr::MachineEmit { fields, .. } => fields
            .iter()
            .any(|(_, value)| expr_assigns_identifier(&value.0, name)),
        Expr::Yield(value) | Expr::Return(value) => value
            .as_ref()
            .is_some_and(|value| expr_assigns_identifier(&value.0, name)),
        Expr::Literal(_)
        | Expr::Identifier(_)
        | Expr::This
        | Expr::RegexLiteral(_)
        | Expr::ByteStringLiteral(_)
        | Expr::ByteArrayLiteral(_) => false,
    }
}
