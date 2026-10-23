//! Bounded progress checks for fresh, unescaped semaphore counters.
//!
//! Only straight-line code and untimed nested blocks retain facts. Unknown
//! calls, control flow, forks and cancellation scopes discard them. This is a
//! warning about a proven local wait, not a claim to detect arbitrary deadlock.

use std::collections::HashMap;

use hew_parser::ast::{Block, Expr, Ident, Literal, Pattern, Span, Spanned, Stmt, UnaryOp};

use crate::{error::TypeError, ty::Ty};

use super::{LintCtx, LintId, LintLevels};

#[derive(Default)]
struct Facts {
    aliases: HashMap<Ident, usize>,
    counts: Vec<i128>,
}

pub(super) fn check(ctx: &LintCtx, levels: &LintLevels, body: &Block, out: &mut Vec<TypeError>) {
    block(ctx, levels, body, &mut Facts::default(), out);
}

fn block(
    ctx: &LintCtx,
    levels: &LintLevels,
    body: &Block,
    facts: &mut Facts,
    out: &mut Vec<TypeError>,
) {
    for (stmt, _) in &body.stmts {
        match stmt {
            Stmt::Let {
                pattern: (Pattern::Identifier(name), _),
                value: Some(value),
                else_block: None,
                ..
            } => {
                let alias = expression(ctx, levels, value, facts, out);
                facts.aliases.remove(name);
                if let Some(id) = alias {
                    facts.aliases.insert(*name, id);
                }
            }
            Stmt::Expression(value) => {
                expression(ctx, levels, value, facts, out);
                if matches!(ctx.resolved_type_at(&value.1), Some(Ty::Named { head, .. }) if head.registry_key() == "std.semaphore.Lease")
                {
                    facts.aliases.clear();
                }
            }
            _ => facts.aliases.clear(),
        }
    }
    if let Some(value) = &body.trailing_expr {
        expression(ctx, levels, value, facts, out);
    }
}

fn integer(expr: &Expr) -> Option<i128> {
    match expr {
        Expr::Literal(Literal::Integer { value, .. }) => Some(*value),
        Expr::Unary {
            op: UnaryOp::Negate,
            operand,
        } => integer(&operand.0)?.checked_neg(),
        _ => None,
    }
}

fn semaphore(ctx: &LintCtx, span: &Span) -> bool {
    matches!(ctx.resolved_type_at(span), Some(Ty::Named { head, .. }) if head.registry_key() == "std.semaphore.Semaphore")
}

fn warn(ctx: &LintCtx, levels: &LintLevels, span: &Span, message: &str, out: &mut Vec<TypeError>) {
    ctx.emit(levels, LintId::SemaphoreProgress, span, message.to_owned(),
        "supply capacity before waiting, or arrange a producer; use a deadline for intentional cancellation-only parking".to_owned(), out);
}

fn expression(
    ctx: &LintCtx,
    levels: &LintLevels,
    value: &Spanned<Expr>,
    facts: &mut Facts,
    out: &mut Vec<TypeError>,
) -> Option<usize> {
    let (expr, span) = value;
    let key = super::super::types::SpanKey::in_module(span, ctx.module_idx);
    let constructor = ctx.checker.defs.lookup_path("std.semaphore.new");
    if constructor.is_some() && ctx.checker.selected_callable_declaration_at(&key) == constructor {
        if let Expr::Call { args, .. } | Expr::MethodCall { args, .. } = expr {
            if let Some(count) = args.first().and_then(|arg| integer(&arg.expr().0)) {
                if (0..=2_147_483_647).contains(&count) {
                    let id = facts.counts.len();
                    facts.counts.push(count);
                    return Some(id);
                }
                ctx.emit(
                    levels,
                    LintId::SemaphoreProgress,
                    span,
                    "semaphore capacity is outside 0..=2147483647 and will fail".to_owned(),
                    "choose a capacity in 0..=2147483647".to_owned(),
                    out,
                );
            }
        }
    }
    if let Expr::MethodCall {
        receiver,
        method,
        args,
    } = expr
    {
        if semaphore(ctx, &receiver.1) && args.is_empty() {
            if let Expr::Ident(name) = &receiver.0 {
                if let Some(&id) = facts.aliases.get(name) {
                    match method.0.name.as_str() {
                        "clone" => return Some(id),
                        "count" | "close" => return None,
                        "release" => {
                            facts.counts[id] += 1;
                            return None;
                        }
                        "acquire" | "lease" => {
                            if facts.counts[id] == 0 {
                                warn(ctx, levels, span, "acquisition cannot progress: this fresh semaphore has no remaining capacity and no escaped producer", out);
                            } else {
                                facts.counts[id] -= 1;
                            }
                            // Lease cleanup is not modelled across scope exits; stop
                            // tracking when its ownership leaves this straight line.
                            return None;
                        }
                        _ => {}
                    }
                }
            }
        }
    }
    match expr {
        Expr::Clone(operand) if semaphore(ctx, &operand.1) => {
            if let Expr::Ident(name) = &operand.0 {
                return facts.aliases.get(name).copied();
            }
            facts.aliases.clear();
        }
        Expr::Literal(_) => {}
        Expr::Block(body) | Expr::Scope { body } => {
            // Local shadowing and resource destruction change bindings/counts.
            // Check the nested block with current facts, then forget them.
            block(ctx, levels, body, facts, out);
            facts.aliases.clear();
        }
        _ => facts.aliases.clear(),
    }
    None
}
