//! Analysis module for inlay hints.
//!
//! Produces inlay hints (type annotations for unannotated `let`/`var` bindings
//! and lambda return types) using byte offsets rather than LSP positions.

use hew_parser::ast::{Block, Expr, Item, Span, Stmt, TypeBodyItem};
use hew_parser::ParseResult;
use hew_types::check::SpanKey;
use hew_types::TypeCheckOutput;

use crate::{InlayHint, InlayHintKind};

/// Build inlay hints for the entire document.
pub fn build_inlay_hints(
    source: &str,
    parse_result: &ParseResult,
    tc: &TypeCheckOutput,
) -> Vec<InlayHint> {
    let mut hints = Vec::new();
    for (item, _span) in &parse_result.program.items {
        collect_inlay_hints_from_item(source, item, tc, &mut hints);
    }
    hints
}

fn collect_inlay_hints_from_item(
    source: &str,
    item: &Item,
    tc: &TypeCheckOutput,
    hints: &mut Vec<InlayHint>,
) {
    match item {
        Item::Function(f) => collect_inlay_hints_from_block(source, &f.body, tc, hints),
        Item::Actor(a) => {
            for recv in &a.receive_fns {
                collect_inlay_hints_from_block(source, &recv.body, tc, hints);
            }
            for method in &a.methods {
                collect_inlay_hints_from_block(source, &method.body, tc, hints);
            }
        }
        Item::TypeDecl(td) => {
            for body_item in &td.body {
                if let TypeBodyItem::Method(method) = body_item {
                    collect_inlay_hints_from_block(source, &method.body, tc, hints);
                }
            }
        }
        Item::Impl(i) => {
            for method in &i.methods {
                collect_inlay_hints_from_block(source, &method.body, tc, hints);
            }
        }
        _ => {}
    }
}

fn collect_inlay_hints_from_block(
    source: &str,
    block: &Block,
    tc: &TypeCheckOutput,
    hints: &mut Vec<InlayHint>,
) {
    for (stmt, _span) in &block.stmts {
        collect_inlay_hints_from_stmt(source, stmt, tc, hints);
    }
}

fn collect_inlay_hints_from_stmt(
    source: &str,
    stmt: &Stmt,
    tc: &TypeCheckOutput,
    hints: &mut Vec<InlayHint>,
) {
    match stmt {
        Stmt::Let { pattern, ty, value } => {
            if ty.is_none() {
                if let Some(value_expr) = value {
                    let span_key = SpanKey {
                        start: value_expr.1.start,
                        end: value_expr.1.end,
                    };
                    if let Some(inferred_ty) = tc.expr_types.get(&span_key) {
                        hints.push(InlayHint {
                            offset: pattern.1.end,
                            label: format!(": {inferred_ty}"),
                            kind: InlayHintKind::Type,
                            padding_left: false,
                        });
                    }
                }
            }
            if let Some(value_expr) = value {
                collect_inlay_hints_from_expr(source, &value_expr.0, tc, hints);
            }
        }
        Stmt::Var {
            ty, value, name, ..
        } => {
            if ty.is_none() {
                if let Some(value_expr) = value {
                    let span_key = SpanKey {
                        start: value_expr.1.start,
                        end: value_expr.1.end,
                    };
                    if let Some(inferred_ty) = tc.expr_types.get(&span_key) {
                        let name_end = find_var_name_end(source, &value_expr.1, name);
                        hints.push(InlayHint {
                            offset: name_end,
                            label: format!(": {inferred_ty}"),
                            kind: InlayHintKind::Type,
                            padding_left: false,
                        });
                    }
                }
            }
            if let Some(value_expr) = value {
                collect_inlay_hints_from_expr(source, &value_expr.0, tc, hints);
            }
        }
        Stmt::For { body, .. } | Stmt::Loop { body, .. } | Stmt::While { body, .. } => {
            collect_inlay_hints_from_block(source, body, tc, hints);
        }
        Stmt::If {
            then_block,
            else_block,
            ..
        } => {
            collect_inlay_hints_from_block(source, then_block, tc, hints);
            if let Some(eb) = else_block {
                if let Some(if_stmt) = &eb.if_stmt {
                    collect_inlay_hints_from_stmt(source, &if_stmt.0, tc, hints);
                }
                if let Some(block) = &eb.block {
                    collect_inlay_hints_from_block(source, block, tc, hints);
                }
            }
        }
        Stmt::IfLet {
            body, else_body, ..
        } => {
            collect_inlay_hints_from_block(source, body, tc, hints);
            if let Some(block) = else_body {
                collect_inlay_hints_from_block(source, block, tc, hints);
            }
        }
        Stmt::Match { arms, .. } => {
            for arm in arms {
                collect_inlay_hints_from_expr(source, &arm.body.0, tc, hints);
            }
        }
        Stmt::Expression(expr) => {
            collect_inlay_hints_from_expr(source, &expr.0, tc, hints);
        }
        _ => {}
    }
}

fn collect_inlay_hints_from_expr(
    source: &str,
    expr: &Expr,
    tc: &TypeCheckOutput,
    hints: &mut Vec<InlayHint>,
) {
    match expr {
        Expr::Lambda {
            return_type, body, ..
        } => {
            if return_type.is_none() {
                let span_key = SpanKey {
                    start: body.1.start,
                    end: body.1.end,
                };
                if let Some(body_ty) = tc.expr_types.get(&span_key) {
                    hints.push(InlayHint {
                        offset: body.1.start,
                        label: format!("-> {body_ty} "),
                        kind: InlayHintKind::Type,
                        padding_left: true,
                    });
                }
            }
            collect_inlay_hints_from_expr(source, &body.0, tc, hints);
        }
        Expr::Block(block)
        | Expr::Unsafe(block)
        | Expr::ScopeLaunch(block)
        | Expr::ScopeSpawn(block) => {
            collect_inlay_hints_from_block(source, block, tc, hints);
        }
        Expr::Scope { body, .. } => {
            collect_inlay_hints_from_block(source, body, tc, hints);
        }
        Expr::If {
            then_block,
            else_block,
            ..
        } => {
            collect_inlay_hints_from_expr(source, &then_block.0, tc, hints);
            if let Some(else_expr) = else_block {
                collect_inlay_hints_from_expr(source, &else_expr.0, tc, hints);
            }
        }
        Expr::IfLet {
            body, else_body, ..
        } => {
            collect_inlay_hints_from_block(source, body, tc, hints);
            if let Some(block) = else_body {
                collect_inlay_hints_from_block(source, block, tc, hints);
            }
        }
        Expr::Match { arms, .. } => {
            for arm in arms {
                collect_inlay_hints_from_expr(source, &arm.body.0, tc, hints);
            }
        }
        Expr::Cast { expr: inner, .. } => {
            collect_inlay_hints_from_expr(source, &inner.0, tc, hints);
        }
        _ => {}
    }
}

/// Find the end offset of the variable name in a `var name = ...` statement.
fn find_var_name_end(source: &str, value_span: &Span, name: &str) -> usize {
    let before_eq = &source[..value_span.start];
    if let Some(eq_pos) = before_eq.rfind('=') {
        let trimmed = source[..eq_pos].trim_end();
        if trimmed.ends_with(name) {
            return trimmed.len();
        }
    }
    value_span.start.saturating_sub(3)
}
