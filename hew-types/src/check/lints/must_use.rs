//! The `must_use` lint.
//!
//! Flags a *discarded* value whose type carries a write/send failure that must
//! not be ignored — `WriteError` and `SendError`, the error arms returned by
//! `Connection.write` and the pid `tell` family. Dropping such a value on the
//! floor silently fails open: a backpressure or disconnect signal vanishes, an
//! undelivered message is never noticed. A program should handle it (`?`,
//! `match`) or discard it explicitly (`let _ = …`).
//!
//! The value is flagged in two shapes:
//!
//! - the bare error: `WriteError` / `SendError`;
//! - a `Result<_, E>` whose error arm is one of those — the common case, since
//!   `write()` returns `Result<(), WriteError>` and `tell()` returns
//!   `Result<(), SendError>`.
//!
//! ## Precision over recall
//!
//! - Only fires in **statement position** (a `Stmt::Expression` whose value the
//!   block discards). A trailing expression (the block's value), a `let` /
//!   `var` binding, and a `match` / `if let` scrutinee are all "used" and never
//!   flagged. `expr?` lands here typed as the unwrapped ok arm, so a handled
//!   call is silent; only the unhandled discard remains.
//! - The discarded expression's resolved type must be exactly `WriteError` /
//!   `SendError` or `Result<_, WriteError|SendError>`, verified through
//!   [`LintCtx::resolved_type_at`]; an unresolved type stays silent.
//! - `// hew:allow(must_use)` (or an explicit `let _ = …`) is the documented
//!   opt-out, mirroring the registry's other suppressible lints.

use hew_parser::ast::{Block, Span, Stmt};

use crate::error::TypeError;
use crate::ty::Ty;

use super::{LintCtx, LintId, LintLevels, NodeVisitor};

/// Entry point: walk `body` and flag every discarded must-use error value.
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
            format!("discarded `{}` value; an ignored write/send error fails open", hit.error),
            format!("handle it (`?`, `match`) or discard explicitly with `let _ = …` — `{}` reports backpressure, disconnect, or undelivered sends", hit.error),
            out,
        );
    }
}

struct Hit {
    span: Span,
    /// Stable name of the offending error type (`WriteError` / `SendError`),
    /// used to render the message and suggestion.
    error: &'static str,
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
            if let Some(error) = self
                .ctx
                .resolved_type_at(expr_span)
                .as_ref()
                .and_then(must_use_error)
            {
                self.hits.push(Hit {
                    span: expr_span.clone(),
                    error,
                });
            }
        }
    }
}

/// The stable name of the must-use error a discarded value carries, or `None`.
///
/// Matches the bare error type and a `Result<_, E>` whose error arm is one. The
/// match is by canonical name so it covers both the builtin `SendError` and the
/// stdlib `WriteError` enum identically.
fn must_use_error(ty: &Ty) -> Option<&'static str> {
    if let Some((_, err)) = ty.as_result() {
        return error_name(err);
    }
    error_name(ty)
}

/// `WriteError` / `SendError` named-type detection.
fn error_name(ty: &Ty) -> Option<&'static str> {
    match ty {
        Ty::Named { name, .. } if name == "WriteError" => Some("WriteError"),
        Ty::Named { name, .. } if name == "SendError" => Some("SendError"),
        _ => None,
    }
}
