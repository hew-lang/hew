//! HIR-shape regression for `break` / `continue` / bare `loop {}` lowering
//! (Stage 1 of the break/continue completeness lane).
//!
//! Before this lane, `Stmt::Break` / `Stmt::Continue` / `Stmt::Loop` fell
//! through the `lower_stmt` `_` catchall to `unsupported(..,"slice-2")`,
//! producing `HirExprKind::Unsupported` plus a `NotYetImplemented`
//! diagnostic. These tests pin that both unlabeled and labeled forms lower to
//! the dedicated `HirExprKind::Break` / `Continue` / loop nodes and emit no
//! `NotYetImplemented`; MIR owns the target selection and defer-flush window.

use hew_hir::{
    lower_program, HirBlock, HirDiagnosticKind, HirExprKind, HirStmtKind, ResolutionCtx,
};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

fn typecheck_and_lower(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "typecheck errors: {:#?}",
        tc_output.errors
    );
    lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    )
}

/// Walk a block (statements + tail) and return `true` if any contained
/// `HirExprKind` satisfies `pred`. Recurses into the loop bodies this lane
/// introduces so nested `break`/`continue` are reachable.
fn block_has_kind<F: Fn(&HirExprKind) -> bool + Copy>(block: &HirBlock, pred: F) -> bool {
    for stmt in &block.statements {
        match &stmt.kind {
            HirStmtKind::Expr(e) | HirStmtKind::Let(_, Some(e)) | HirStmtKind::Return(Some(e)) => {
                if expr_has_kind(e, pred) {
                    return true;
                }
            }
            HirStmtKind::Assign { value, .. } => {
                if expr_has_kind(value, pred) {
                    return true;
                }
            }
            HirStmtKind::Defer { body, .. } => {
                if expr_has_kind(body, pred) {
                    return true;
                }
            }
            HirStmtKind::Let(_, None) | HirStmtKind::Return(None) => {}
        }
    }
    if let Some(tail) = &block.tail {
        if expr_has_kind(tail, pred) {
            return true;
        }
    }
    false
}

fn expr_has_kind<F: Fn(&HirExprKind) -> bool + Copy>(expr: &hew_hir::HirExpr, pred: F) -> bool {
    if pred(&expr.kind) {
        return true;
    }
    match &expr.kind {
        HirExprKind::Loop { body, .. }
        | HirExprKind::While { body, .. }
        | HirExprKind::ForRange { body, .. }
        | HirExprKind::WhileLet { body, .. } => block_has_kind(body, pred),
        HirExprKind::Block(block) => block_has_kind(block, pred),
        HirExprKind::Break {
            value: Some(value), ..
        } => expr_has_kind(value, pred),
        _ => false,
    }
}

/// Locate the lowered body of the top-level `fn main`.
fn main_body(output: &hew_hir::LowerOutput) -> &HirBlock {
    for item in &output.module.items {
        if let hew_hir::HirItem::Function(f) = item {
            if f.name == "main" {
                return &f.body;
            }
        }
    }
    panic!("no `fn main` in lowered module");
}

fn has_not_yet_implemented(output: &hew_hir::LowerOutput) -> bool {
    output
        .diagnostics
        .iter()
        .any(|d| matches!(d.kind, HirDiagnosticKind::NotYetImplemented { .. }))
}

// ── Unlabeled forms lower to the dedicated nodes (no Unsupported) ────────────

#[test]
fn break_in_while_lowers_to_break_node() {
    let output = typecheck_and_lower(
        r"
        fn main() {
            while true {
                break;
            }
        }
        ",
    );
    assert!(
        !has_not_yet_implemented(&output),
        "unlabeled `break;` must not emit NotYetImplemented: {:#?}",
        output.diagnostics
    );
    let body = main_body(&output);
    assert!(
        block_has_kind(body, |k| matches!(k, HirExprKind::Break { .. })),
        "expected a HirExprKind::Break node in the while body"
    );
    assert!(
        !block_has_kind(body, |k| matches!(k, HirExprKind::Unsupported(_))),
        "no Unsupported node may survive for unlabeled break"
    );
}

#[test]
fn continue_in_while_lowers_to_continue_node() {
    let output = typecheck_and_lower(
        r"
        fn main() {
            while true {
                continue;
            }
        }
        ",
    );
    assert!(
        !has_not_yet_implemented(&output),
        "unlabeled `continue;` must not emit NotYetImplemented: {:#?}",
        output.diagnostics
    );
    let body = main_body(&output);
    assert!(
        block_has_kind(body, |k| matches!(k, HirExprKind::Continue { .. })),
        "expected a HirExprKind::Continue node in the while body"
    );
    assert!(
        !block_has_kind(body, |k| matches!(k, HirExprKind::Unsupported(_))),
        "no Unsupported node may survive for unlabeled continue"
    );
}

#[test]
fn bare_loop_with_break_lowers_to_loop_node() {
    let output = typecheck_and_lower(
        r"
        fn main() {
            loop {
                break;
            }
        }
        ",
    );
    assert!(
        !has_not_yet_implemented(&output),
        "bare `loop {{ break; }}` must not emit NotYetImplemented: {:#?}",
        output.diagnostics
    );
    let body = main_body(&output);
    assert!(
        block_has_kind(body, |k| matches!(k, HirExprKind::Loop { .. })),
        "expected a HirExprKind::Loop node in main"
    );
    // The `break;` inside the loop body is reachable through the Loop node.
    assert!(
        block_has_kind(body, |k| matches!(k, HirExprKind::Break { .. })),
        "expected the loop body's break to lower to a Break node"
    );
}

#[test]
fn break_with_value_lowers_and_carries_operand() {
    // `break <value>` — Stage 1 carries the operand on the node so Stage 2
    // can lower it for side-effects before the jump (LESSONS
    // `cleanup-all-exits`). Loop-as-expression value return is out of scope.
    let output = typecheck_and_lower(
        r"
        fn main() {
            loop {
                break 1 + 2;
            }
        }
        ",
    );
    assert!(
        !has_not_yet_implemented(&output),
        "`break <value>;` must not emit NotYetImplemented: {:#?}",
        output.diagnostics
    );
    let body = main_body(&output);
    assert!(
        block_has_kind(body, |k| matches!(
            k,
            HirExprKind::Break { value: Some(_), .. }
        )),
        "expected a HirExprKind::Break carrying a value operand"
    );
    // The operand expression must be lowered (not dropped): the `1 + 2`
    // Binary node is reachable through the Break node's value.
    assert!(
        block_has_kind(body, |k| matches!(k, HirExprKind::Binary { .. })),
        "expected the break operand `1 + 2` to be lowered into a Binary node"
    );
}

// ── Labeled forms carry labels through HIR ───────────────────────────────────

#[test]
fn labeled_break_lowers_with_label() {
    let output = typecheck_and_lower(
        r"
        fn main() {
            @outer: loop {
                break @outer;
            }
        }
        ",
    );
    assert!(
        !has_not_yet_implemented(&output),
        "labeled `break @outer;` must lower without NotYetImplemented: {:#?}",
        output.diagnostics
    );
    let body = main_body(&output);
    assert!(
        block_has_kind(body, |k| matches!(
            k,
            HirExprKind::Loop {
                label: Some(label),
                ..
            } if label == "outer"
        )),
        "expected the loop label to survive HIR lowering"
    );
    assert!(
        block_has_kind(body, |k| matches!(
            k,
            HirExprKind::Break {
                label: Some(label),
                ..
            } if label == "outer"
        )),
        "expected labeled break to carry `outer` into MIR"
    );
}

#[test]
fn labeled_continue_lowers_with_label() {
    let output = typecheck_and_lower(
        r"
        fn main() {
            @outer: while true {
                continue @outer;
            }
        }
        ",
    );
    assert!(
        !has_not_yet_implemented(&output),
        "labeled `continue @outer;` must lower without NotYetImplemented: {:#?}",
        output.diagnostics
    );
    let body = main_body(&output);
    assert!(
        block_has_kind(body, |k| matches!(
            k,
            HirExprKind::While {
                label: Some(label),
                ..
            } if label == "outer"
        )),
        "expected the while label to survive HIR lowering"
    );
    assert!(
        block_has_kind(body, |k| matches!(
            k,
            HirExprKind::Continue {
                label: Some(label)
            } if label == "outer"
        )),
        "expected labeled continue to carry `outer` into MIR"
    );
}
