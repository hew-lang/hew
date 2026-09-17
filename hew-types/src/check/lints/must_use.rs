//! The `must_use` lint.
//!
//! Flags a *discarded* machine step report. A step reports typed outputs
//! and a disposition; dropping it on the floor loses both. A program should
//! read it or discard it explicitly (`let _ = …`).
//!
//! Send and ask outcomes are not a lint tier: discarding one is
//! `E_SEND_RESULT_DROPPED`, a compile error raised by the statement checker
//! (HEW-SPEC-2026 §2.1.1, §5.6).
//!
//! ## Precision over recall
//!
//! - Only fires in **statement position** (a `Stmt::Expression` whose value the
//!   block discards). A trailing expression (the block's value), a `let` /
//!   `var` binding, and a `match` / `if let` scrutinee are all "used" and never
//!   flagged.
//! - The discarded expression's resolved type must be a declaration the
//!   checker registered as a step report, verified through
//!   [`LintCtx::resolved_type_at`]; an unresolved type stays silent.
//! - `// hew:allow(must_use)` (or an explicit `let _ = …`) is the documented
//!   opt-out, mirroring the registry's other suppressible lints.

use hew_parser::ast::{Block, Span, Stmt};

use crate::error::TypeError;
use crate::ty::Ty;

use super::{LintCtx, LintId, LintLevels, NodeVisitor};

/// Entry point: walk `body` and flag every discarded step report.
pub(super) fn check(ctx: &LintCtx, levels: &LintLevels, body: &Block, out: &mut Vec<TypeError>) {
    let mut visitor = MustUse {
        ctx,
        hits: Vec::new(),
    };
    super::walk_body(body, &mut visitor);
    for hit in visitor.hits {
        ctx.emit(
            levels,
            LintId::MustUse,
            &hit.span,
            hit.message,
            hit.suggestion,
            out,
        );
    }
}

struct Hit {
    span: Span,
    message: String,
    suggestion: String,
}

struct MustUse<'a> {
    ctx: &'a LintCtx<'a>,
    hits: Vec<Hit>,
}

impl NodeVisitor for MustUse<'_> {
    fn visit_block(&mut self, block: &Block) {
        // Statement position only: a block's `stmts` are evaluated for effect
        // (value discarded); its `trailing_expr` is the block's value and is
        // therefore used. Bindings (`Stmt::Let`/`Stmt::Var`), matches, and the
        // `?` operator are distinct stmt/expr shapes and never reach here.
        for (stmt, _span) in &block.stmts {
            let Stmt::Expression((_expr, expr_span)) = stmt else {
                continue;
            };
            let Some(Ty::Named { name, .. }) = self.ctx.resolved_type_at(expr_span) else {
                continue;
            };
            if self
                .ctx
                .checker
                .identity
                .declaration_by_path(&name)
                .is_some_and(|declaration| self.ctx.checker.must_use_types.contains(declaration))
            {
                self.hits.push(Hit {
                    span: expr_span.clone(),
                    message: format!("discarded `{name}` machine step report"),
                    suggestion: "handle its typed outputs and disposition, or discard explicitly with `let _ = …`".to_string(),
                });
            }
        }
    }
}
