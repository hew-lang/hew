//! The `must_use` lint.
//!
//! Flags a *discarded* value whose type carries a failure that must not be
//! ignored — `WriteError`, the error arm returned by `Connection.write`.
//! Dropping such a value on the floor silently fails open: a backpressure or
//! disconnect signal vanishes. A program should handle it (`?`, `match`) or
//! discard it explicitly (`let _ = …`).
//!
//! Send and ask outcomes are not a lint tier: discarding one is
//! `E_SEND_RESULT_DROPPED`, a compile error raised by the statement checker
//! (HEW-SPEC-2026 §2.1.1, §5.6).
//!
//! The value is flagged in two shapes:
//!
//! - the bare error: `WriteError`;
//! - a `Result<_, E>` whose error arm is one — the common case, since
//!   `write()` returns `Result<(), WriteError>`.
//!
//! ## Precision over recall
//!
//! - Only fires in **statement position** (a `Stmt::Expression` whose value the
//!   block discards). A trailing expression (the block's value), a `let` /
//!   `var` binding, and a `match` / `if let` scrutinee are all "used" and never
//!   flagged. `expr?` lands here typed as the unwrapped ok arm, so a handled
//!   call is silent; only the unhandled discard remains.
//! - The discarded expression's resolved type must be exactly `WriteError` or
//!   a `Result<_, WriteError>`, verified through [`LintCtx::resolved_type_at`];
//!   an unresolved type stays silent.
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
            let ty = self.ctx.resolved_type_at(expr_span);
            if let Some(error) = ty.as_ref().and_then(must_use_error) {
                self.hits.push(Hit {
                    span: expr_span.clone(),
                    message: format!("discarded `{}` value; an ignored {} error fails open", error.name, error.class),
                    suggestion: format!("handle it (`?`, `match`) or discard explicitly with `let _ = …` — `{}` reports {}", error.name, error.reports),
                });
            } else if let Some(Ty::Named { name, .. }) = ty {
                if self
                    .ctx
                    .checker
                    .identity
                    .declaration_by_path(&name)
                    .is_some_and(|declaration| {
                        self.ctx.checker.must_use_types.contains(declaration)
                    })
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
}

/// A must-use error type and the failure it loses when silently discarded.
///
/// `class` keys the primary diagnostic ("an ignored {class} error fails open");
/// `reports` names the concrete failure in the suggestion. Grouping by class
/// keeps the message identical for every error that fails the same way.
#[derive(Clone, Copy)]
struct MustUseError {
    /// Stable name of the offending error type, shown in the diagnostic.
    name: &'static str,
    /// Error-class phrase for the primary message (`write/send`, `ask`).
    class: &'static str,
    /// What the discarded error silently drops, for the suggestion.
    reports: &'static str,
}

impl MustUseError {
    /// `WriteError` — backpressure / disconnect from `Connection.write`.
    const WRITE: Self = Self {
        name: "WriteError",
        class: "write/send",
        reports: "backpressure, disconnect, or undelivered sends",
    };
}

/// The must-use error a discarded value carries, or `None`.
///
/// Matches the bare error type and a `Result<_, E>` whose error arm is one. The
/// match is by canonical name so it covers the builtin `SendError` / `AskError`
/// and the stdlib `WriteError` enum identically.
fn must_use_error(ty: &Ty) -> Option<MustUseError> {
    if let Some((_, err)) = ty.as_result() {
        return error_kind(err);
    }
    error_kind(ty)
}

/// `WriteError` named-type detection.
fn error_kind(ty: &Ty) -> Option<MustUseError> {
    let Ty::Named { name, builtin, .. } = ty else {
        return None;
    };
    (builtin.is_none() && name == crate::stdlib::STD_NET_WRITE_ERROR).then_some(MustUseError::WRITE)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::BuiltinType;

    fn named(name: &str, builtin: Option<BuiltinType>) -> Ty {
        Ty::Named {
            name: name.to_string(),
            args: Vec::new(),
            builtin,
        }
    }

    #[test]
    fn only_the_stdlib_write_error_is_must_use() {
        assert!(error_kind(&named(crate::stdlib::STD_NET_WRITE_ERROR, None)).is_some());
        // Delivery outcomes moved to E_SEND_RESULT_DROPPED and must not be
        // reported a second time as a lint.
        for delivery in [
            named("std.builtins.SendError", Some(BuiltinType::SendError)),
            named("std.builtins.AskError", Some(BuiltinType::AskError)),
        ] {
            assert!(error_kind(&delivery).is_none(), "{delivery:?}");
        }
        assert!(error_kind(&named("WriteError", None)).is_none());
    }
}
